//! The message of a source that does not parse, as CRuby builds it
//! (`pm_parse_process_error` and `pm_parse_errors_format` in
//! `prism_compile.c`): `FILE:LINE: syntax error(s) found`, followed by
//! the source lines around every error prism reported, each error under
//! its line with a `^~~` marker (#2230).
//!
//! ```text
//! t.rb:2: syntax errors found
//!   1 | x =
//! > 2 | y = (1 +
//!     |         ^ expected a matching `)`
//!     |         ^ unexpected end-of-input; expected an expression after the operator
//! ```
//!
//! This is a port of those two functions, kept in their order and with
//! their names so that the two can be read side by side.

/// One diagnostic of the parse: its message and the byte extent it is
/// about.
pub(super) struct Diagnostic {
    pub message: Vec<u8>,
    pub start: usize,
    pub end: usize,
}

/// What the errors of a parse amount to (prism's `pm_error_level_t`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum ErrorLevel {
    /// A SyntaxError.
    Syntax,
    /// An ArgumentError: one error that takes over as the only one.
    Argument,
    /// A LoadError.
    Load,
}

/// The level of the error with *message*. The Rust prism wrapper does
/// not hand out a diagnostic's level, and exactly two messages have
/// another than "syntax" (`templates/src/diagnostic.c.erb`).
fn error_level(message: &[u8]) -> ErrorLevel {
    match message {
        b"unknown or invalid encoding in the magic comment" => ErrorLevel::Argument,
        b"no Ruby script found in input" => ErrorLevel::Load,
        _ => ErrorLevel::Syntax,
    }
}

/// The source the errors are about, with what the formatting reads off
/// prism's parser: where each line starts (`newline_list`), the number
/// of the first line (`start_line`) and the width of the character at a
/// position (`encoding->char_width`, 0 for a byte sequence that is not
/// one).
pub(super) struct Source<'a> {
    bytes: &'a [u8],
    offsets: Vec<usize>,
    start_line: i32,
    char_width: &'a dyn Fn(&[u8]) -> usize,
}

impl<'a> Source<'a> {
    pub(super) fn new(
        bytes: &'a [u8],
        start_line: i32,
        char_width: &'a dyn Fn(&[u8]) -> usize,
    ) -> Self {
        let mut offsets = vec![0];
        offsets.extend(
            bytes
                .iter()
                .enumerate()
                .filter(|(_, b)| **b == b'\n')
                .map(|(i, _)| i + 1),
        );
        Source {
            bytes,
            offsets,
            start_line,
            char_width,
        }
    }

    /// `pm_newline_list_line_column`: the line of the byte at *offset*,
    /// counted from *start_line*, and its column in bytes.
    fn line_column(&self, offset: usize, start_line: i32) -> (i32, usize) {
        let index = self.offsets.partition_point(|&o| o <= offset) - 1;
        (index as i32 + start_line, offset - self.offsets[index])
    }

    fn width_at(&self, pos: usize) -> usize {
        match self.bytes.get(pos..) {
            Some(rest) if !rest.is_empty() => (self.char_width)(rest),
            _ => 0,
        }
    }
}

/// `pm_parse_error_t`: an error as it is going to be shown.
struct ParseError<'a> {
    error: &'a Diagnostic,
    line: i32,
    column_start: usize,
    column_end: usize,
}

const COLOR_BOLD: &[u8] = b"\x1b[1m";
const COLOR_GRAY: &[u8] = b"\x1b[2m";
const COLOR_RED: &[u8] = b"\x1b[1;31m";
const COLOR_RESET: &[u8] = b"\x1b[m";
const ERROR_TRUNCATE: usize = 30;

/// `pm_parse_errors_format_sort`: the errors by line and column. An
/// error is put in front of those already there at its position, so
/// errors at one position come out in the reverse of prism's order.
fn sort<'a>(source: &Source<'_>, errors: &[&'a Diagnostic]) -> Vec<ParseError<'a>> {
    let mut sorted: Vec<ParseError<'a>> = Vec::with_capacity(errors.len());
    for error in errors {
        let (line, column) = source.line_column(error.start, source.start_line);
        let (end_line, end_column) = source.line_column(error.end, source.start_line);
        let index = sorted
            .iter()
            .position(|e| !(e.line < line || (e.line == line && e.column_start < column)))
            .unwrap_or(sorted.len());
        let mut column_end = if line == end_line {
            end_column
        } else {
            let at = (line - source.start_line) as usize;
            source.offsets[at + 1] - source.offsets[at] - 1
        };
        // At least one column of error.
        if column == column_end {
            column_end += 1;
        }
        sorted.insert(
            index,
            ParseError {
                error,
                line,
                column_start: column,
                column_end,
            },
        );
    }
    sorted
}

/// `pm_parse_error_format_t`: how line numbers and the lines without
/// one are prefixed, for the widest line number shown.
struct Format {
    width: usize,
    highlight: u8,
}

impl Format {
    fn number_prefix(&self, line: i32, out: &mut Vec<u8>) {
        if self.highlight > 0 {
            out.extend_from_slice(COLOR_GRAY);
        }
        out.extend_from_slice(format!("{line:>width$} | ", width = self.width).as_bytes());
        if self.highlight > 0 {
            out.extend_from_slice(COLOR_RESET);
        }
    }

    fn blank_prefix(&self, out: &mut Vec<u8>) {
        if self.highlight > 0 {
            out.extend_from_slice(COLOR_GRAY);
        }
        out.extend(std::iter::repeat_n(b' ', self.width + 1));
        out.extend_from_slice(b"| ");
        if self.highlight > 0 {
            out.extend_from_slice(COLOR_RESET);
        }
    }

    fn divider(&self, out: &mut Vec<u8>) {
        if self.highlight > 0 {
            out.extend_from_slice(COLOR_GRAY);
        }
        out.extend_from_slice(b"  ");
        out.extend(std::iter::repeat_n(b'~', (self.width + 4).min(8)));
        if self.highlight > 0 {
            out.extend_from_slice(COLOR_RESET);
        }
        out.push(b'\n');
    }
}

/// `pm_parse_errors_format_line`: line *line* of the source behind its
/// number. A line that goes on for [`ERROR_TRUNCATE`] bytes or more
/// past *column_end* is cut there, and one whose *column_start* is that
/// far in starts at it.
fn format_line(
    source: &Source<'_>,
    format: &Format,
    line: i32,
    column_start: usize,
    column_end: usize,
    out: &mut Vec<u8>,
) {
    let index = (line - source.start_line) as usize;
    let len = source.bytes.len();
    let mut start = source.offsets[index];
    let mut end = if index >= source.offsets.len() - 1 {
        len
    } else {
        source.offsets[index + 1]
    };
    format.number_prefix(line, out);

    let mut truncate_end = false;
    if column_end != 0 && end >= start + column_end + ERROR_TRUNCATE {
        let mut end_candidate = start + column_end + ERROR_TRUNCATE;
        let mut ptr = start;
        while ptr < end_candidate {
            let char_width = source.width_at(ptr);
            // A character that does not decode: cut at the fixed width.
            if char_width == 0 {
                break;
            }
            if ptr + char_width > end_candidate {
                end_candidate = ptr;
                break;
            }
            ptr += char_width;
        }
        end = end_candidate;
        truncate_end = true;
    }

    if column_start >= ERROR_TRUNCATE {
        out.extend_from_slice(b"... ");
        start += column_start;
    }
    if start < end {
        out.extend_from_slice(&source.bytes[start..end]);
    }

    if truncate_end {
        out.extend_from_slice(b" ...\n");
    } else if end == len && source.bytes.last() != Some(&b'\n') {
        out.push(b'\n');
    }
}

/// `pm_parse_errors_format`: the source around every error, each error
/// under its line. *highlight* is 0 for plain text, 1 for bold and 2
/// for colors; *inline_messages* puts an error's message after its
/// marker.
fn format_errors(
    source: &Source<'_>,
    errors: &[&Diagnostic],
    out: &mut Vec<u8>,
    highlight: u8,
    inline_messages: bool,
) {
    let errors = sort(source, errors);
    let widest = |line: i32| if line < 0 { -line * 10 } else { line };
    let max_line_number = widest(errors[0].line).max(widest(errors[errors.len() - 1].line));
    let format = Format {
        width: match max_line_number {
            ..10 => 1,
            10..100 => 2,
            100..1000 => 3,
            1000..10000 => 4,
            _ => 5,
        },
        highlight,
    };

    // The lines before an error are shown for context, but no line is
    // shown twice when two errors are close.
    let mut last_line = source.start_line - 1;
    let mut last_column_start = 0;
    for (index, error) in errors.iter().enumerate() {
        if error.line - last_line > 1 {
            if error.line - last_line > 2 {
                if index != 0 && error.line - last_line > 3 {
                    format.divider(out);
                }
                out.extend_from_slice(b"  ");
                format_line(source, &format, error.line - 2, 0, 0, out);
            }
            out.extend_from_slice(b"  ");
            format_line(source, &format, error.line - 1, 0, 0, out);
        }

        // The line with the error, for the first error on it.
        if index == 0 || error.line != last_line {
            match highlight {
                0 => out.extend_from_slice(b"> "),
                1 => out.extend_from_slice(&[COLOR_BOLD, b"> ", COLOR_RESET].concat()),
                _ => out.extend_from_slice(&[COLOR_RED, b"> ", COLOR_RESET].concat()),
            }
            last_column_start = error.column_start;
            let column_end = errors[index..]
                .iter()
                .take_while(|e| e.line == error.line)
                .map(|e| e.column_end)
                .max()
                .unwrap_or(error.column_end);
            format_line(
                source,
                &format,
                error.line,
                error.column_start,
                column_end,
                out,
            );
        }

        let start = source.offsets[(error.line - source.start_line) as usize];
        if start == source.bytes.len() {
            out.push(b'\n');
        }

        // The marker: blanks up to the error's column, then a caret and
        // a tilde for every further character of it. A character counts
        // one column whatever width a terminal shows it in.
        out.extend_from_slice(b"  ");
        format.blank_prefix(out);
        let mut column = 0;
        if last_column_start >= ERROR_TRUNCATE {
            out.extend_from_slice(b"    ");
            column = last_column_start;
        }
        while column < error.column_start {
            out.push(b' ');
            column += source.width_at(start + column).max(1);
        }
        match highlight {
            0 => {}
            1 => out.extend_from_slice(COLOR_BOLD),
            _ => out.extend_from_slice(COLOR_RED),
        }
        out.push(b'^');
        column += source.width_at(start + column).max(1);
        while column < error.column_end {
            out.push(b'~');
            column += source.width_at(start + column).max(1);
        }
        if highlight > 0 {
            out.extend_from_slice(COLOR_RESET);
        }
        if inline_messages {
            out.push(b' ');
            out.extend_from_slice(&error.error.message);
        }
        out.push(b'\n');

        // Up to two lines after the error, depending on where the next
        // one is.
        last_line = error.line;
        let next_line = match errors.get(index + 1) {
            Some(next) => next.line,
            None => {
                let mut next_line = source.offsets.len() as i32 + source.start_line;
                // No extra line for a file that ends with a newline.
                if source.offsets.last() == Some(&source.bytes.len()) {
                    next_line -= 1;
                }
                next_line
            }
        };
        for _ in 0..2 {
            if next_line - last_line > 1 {
                last_line += 1;
                out.extend_from_slice(b"  ");
                format_line(source, &format, last_line, 0, 0, out);
            }
        }
    }
}

/// `pm_parse_process_error_utf8_p`: whether the lines the error touches
/// are valid UTF-8. A message is assumed to be UTF-8 text, so the
/// source is only quoted when they are.
fn utf8_lines(source: &Source<'_>, error: &Diagnostic) -> bool {
    let (start_line, _) = source.line_column(error.start, 1);
    let (end_line, _) = source.line_column(error.end, 1);
    let start = source.offsets[start_line as usize - 1];
    let end = match source.offsets.get(end_line as usize) {
        Some(end) => *end,
        None => source.bytes.len(),
    };
    std::str::from_utf8(&source.bytes[start..end]).is_ok()
}

/// `pm_parse_process_error`: the class and the message of the error to
/// raise for a parse that reported *errors* (at least one), for the
/// file named *filepath*.
pub(super) fn process_error(
    source: &Source<'_>,
    filepath: &[u8],
    errors: &[Diagnostic],
    highlight: u8,
) -> (ErrorLevel, Vec<u8>) {
    let mut buffer = Vec::new();
    let position = |buffer: &mut Vec<u8>, error: &Diagnostic| {
        let (line, _) = source.line_column(error.start, source.start_line);
        buffer.extend_from_slice(filepath);
        buffer.extend_from_slice(format!(":{line}: ").as_bytes());
    };
    let mut valid_utf8 = true;
    for error in errors {
        match error_level(&error.message) {
            ErrorLevel::Syntax => valid_utf8 = valid_utf8 && utf8_lines(source, error),
            // Takes over as the only error raised.
            ErrorLevel::Argument => {
                position(&mut buffer, error);
                buffer.extend_from_slice(&error.message);
                if utf8_lines(source, error) {
                    buffer.push(b'\n');
                    format_errors(source, &[error], &mut buffer, highlight, false);
                }
                return (ErrorLevel::Argument, buffer);
            }
            // No source in this one.
            ErrorLevel::Load => return (ErrorLevel::Load, error.message.clone()),
        }
    }

    let head = &errors[0];
    position(&mut buffer, head);
    buffer.extend_from_slice(if errors.len() > 1 {
        b"syntax errors found\n"
    } else {
        b"syntax error found\n"
    });
    if valid_utf8 {
        let all: Vec<&Diagnostic> = errors.iter().collect();
        format_errors(source, &all, &mut buffer, highlight, true);
    } else {
        for (i, error) in errors.iter().enumerate() {
            if i != 0 {
                buffer.push(b'\n');
            }
            position(&mut buffer, error);
            buffer.extend_from_slice(&error.message);
        }
    }
    (ErrorLevel::Syntax, buffer)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn utf8_width(bytes: &[u8]) -> usize {
        let len = match bytes[0] {
            0..0x80 => 1,
            0xc2..0xe0 => 2,
            0xe0..0xf0 => 3,
            0xf0..0xf5 => 4,
            _ => return 0,
        };
        match bytes.get(..len) {
            Some(c) if std::str::from_utf8(c).is_ok() => len,
            _ => 0,
        }
    }

    fn diag(message: &str, start: usize, end: usize) -> Diagnostic {
        Diagnostic {
            message: message.as_bytes().to_vec(),
            start,
            end,
        }
    }

    fn report(src: &str, start_line: i32, errors: &[Diagnostic], highlight: u8) -> String {
        let source = Source::new(src.as_bytes(), start_line, &utf8_width);
        let (level, message) = process_error(&source, b"t.rb", errors, highlight);
        assert_eq!(level, ErrorLevel::Syntax);
        String::from_utf8(message).unwrap()
    }

    /// What CRuby 4.0.9 prints for these sources, with the diagnostics
    /// its prism reports for them.
    #[test]
    fn one_error_and_its_context() {
        assert_eq!(
            report(
                "a = 1; )\n",
                1,
                &[diag("unexpected ')', ignoring it", 7, 8)],
                0
            ),
            "t.rb:1: syntax error found\n\
             > 1 | a = 1; )\n    \
             |        ^ unexpected ')', ignoring it\n"
        );
        // Two lines before and after, and the divider between errors
        // that are further apart than that.
        let src = "l1\nl2\nl3\nl4 )\nl5\nl6\nl7\nl8\nl9\nl10\nl11 )\nl12\n";
        assert_eq!(
            report(src, 1, &[diag("first", 12, 13), diag("second", 37, 38)], 0),
            "t.rb:4: syntax errors found\n   \
               2 | l2\n   \
               3 | l3\n\
             >  4 | l4 )\n     \
                  |    ^ first\n   \
               5 | l5\n   \
               6 | l6\n  \
             ~~~~~~\n   \
               9 | l9\n  \
              10 | l10\n\
             > 11 | l11 )\n     \
                  |     ^ second\n  \
              12 | l12\n"
        );
    }

    #[test]
    fn errors_at_one_position_are_shown_last_first() {
        let src = "x = \ny = (1 +\n";
        let errors = [
            diag("fourth", 13, 14),
            diag("third", 13, 14),
            diag("second", 13, 14),
            diag("first", 13, 14),
        ];
        assert_eq!(
            report(src, 1, &errors, 0),
            "t.rb:2: syntax errors found\n  \
               1 | x = \n\
             > 2 | y = (1 +\n    \
                 |         ^ first\n    \
                 |         ^ second\n    \
                 |         ^ third\n    \
                 |         ^ fourth\n"
        );
    }

    #[test]
    fn extent_is_marked_with_tildes() {
        let src = "def m\n  1 +\nend\nfoo(\n";
        let errors = [diag("a", 12, 15), diag("b", 12, 15), diag("c", 20, 20)];
        assert_eq!(
            report(src, 1, &errors, 0),
            "t.rb:3: syntax errors found\n  \
               1 | def m\n  \
               2 |   1 +\n\
             > 3 | end\n    \
                 | ^~~ b\n    \
                 | ^~~ a\n\
             > 4 | foo(\n    \
                 |     ^ c\n"
        );
        // An error that spans lines is marked to the end of its first,
        // and a character counts one column whatever its width.
        let src = "\u{3042}\u{3044} = [\n1\n";
        assert_eq!(
            report(src, 1, &[diag("m", 0, 13)], 0),
            "t.rb:1: syntax error found\n\
             > 1 | \u{3042}\u{3044} = [\n    \
                 | ^~~~~~ m\n  \
               2 | 1\n"
        );
    }

    #[test]
    fn long_lines_are_truncated() {
        let pad = "a".repeat(40);
        let src = format!("{pad})  {pad}\n");
        assert_eq!(
            report(&src, 1, &[diag("m", 40, 41)], 0),
            format!(
                "t.rb:1: syntax error found\n\
                 > 1 | ... )  {} ...\n    \
                     |     ^ m\n",
                "a".repeat(28)
            )
        );
    }

    #[test]
    fn line_numbers_follow_the_first_line() {
        // `eval(src, nil, "t.rb", 99)`, and a source with no newline at
        // its end.
        assert_eq!(
            report("x\n)", 99, &[diag("m", 2, 3)], 0),
            "t.rb:100: syntax error found\n   \
                99 | x\n\
             > 100 | )\n      \
                   | ^ m\n"
        );
        // An error at the very end of a source that ends with a newline
        // is on a line of its own.
        assert_eq!(
            report("x\n", 1, &[diag("m", 2, 2)], 0),
            "t.rb:2: syntax error found\n  \
               1 | x\n\
             > 2 | \n    \
                 | ^ m\n"
        );
    }

    #[test]
    fn highlighted() {
        assert_eq!(
            report(")\n", 1, &[diag("m", 0, 1)], 1),
            "t.rb:1: syntax error found\n\
             \x1b[1m> \x1b[m\x1b[2m1 | \x1b[m)\n  \
             \x1b[2m  | \x1b[m\x1b[1m^\x1b[m m\n"
        );
        assert_eq!(
            report(")\n", 1, &[diag("m", 0, 1)], 2),
            "t.rb:1: syntax error found\n\
             \x1b[1;31m> \x1b[m\x1b[2m1 | \x1b[m)\n  \
             \x1b[2m  | \x1b[m\x1b[1;31m^\x1b[m m\n"
        );
    }

    #[test]
    fn source_that_is_not_utf8_is_not_quoted() {
        let source = Source::new(b"\xff )\n)\n", 1, &utf8_width);
        let errors = [diag("first", 2, 3), diag("second", 4, 5)];
        let (level, message) = process_error(&source, b"t.rb", &errors, 0);
        assert_eq!(level, ErrorLevel::Syntax);
        assert_eq!(
            message,
            b"t.rb:1: syntax errors found\nt.rb:1: first\nt.rb:2: second"
        );
    }

    #[test]
    fn argument_and_load_levels() {
        let source = Source::new(b"# encoding: foo\n)\n", 1, &utf8_width);
        let errors = [
            diag("unknown or invalid encoding in the magic comment", 12, 15),
            diag("other", 16, 17),
        ];
        let (level, message) = process_error(&source, b"t.rb", &errors, 0);
        assert_eq!(level, ErrorLevel::Argument);
        assert_eq!(
            String::from_utf8(message).unwrap(),
            "t.rb:1: unknown or invalid encoding in the magic comment\n\
             > 1 | # encoding: foo\n    \
                 |             ^~~\n  \
               2 | )\n"
        );
        let errors = [diag("no Ruby script found in input", 0, 0)];
        let (level, message) = process_error(&source, b"t.rb", &errors, 0);
        assert_eq!(level, ErrorLevel::Load);
        assert_eq!(message, b"no Ruby script found in input");
    }
}
