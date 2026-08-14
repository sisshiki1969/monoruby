use std::{borrow::Cow, path::PathBuf};

pub type SourceInfoRef = std::rc::Rc<SourceInfo>;

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Loc(pub usize, pub usize);

impl Loc {
    pub fn merge(&self, loc: Loc) -> Self {
        use std::cmp::*;
        Loc(min(self.0, loc.0), max(self.1, loc.1))
    }
}

/// This struct holds infomation of a certain line in the code.
///
/// `end` is a position of the line end ('\n') or the end of the code (`code.len()`).
///
/// `top..end` is guaranteed to be a valid UTF-8 boundary of the code.
#[derive(Debug, Clone, Copy, PartialEq)]
struct Line {
    /// line number. (the first line is 1)
    pub line_no: usize,
    /// byte position of the line top in the code.
    pub top: usize,
    /// byte position of the line end in the code.
    pub end: usize,
}

impl Line {
    fn new(line_no: usize, top: usize, end: usize) -> Self {
        Line { line_no, top, end }
    }
}

/// Byte offset of every line top in the source, ascending, so that a
/// position → line-number answer is a binary search rather than a scan
/// from the top of the file.
///
/// `line_tops[0]` is always 0, and each `'\n'` at byte *p* contributes
/// *p + 1*; index *i* therefore holds the top of line *i + 1*. Built once
/// per `SourceInfo`, which is immutable after construction.
///
/// The scan it replaces was quadratic in aggregate: [`SourceInfo::get_line`]
/// is called once per `def` executed (`Executor::invoke_method_added`), per
/// class body entered, and per constant store, so loading the ~10k lines of
/// `builtins/*.rb` at every interpreter boot re-walked those files ~1200
/// times — about 30% of startup.
#[derive(Clone, PartialEq)]
struct LineTops(Vec<usize>);

impl LineTops {
    fn new(code: &str) -> Self {
        // A `'\n'` byte cannot occur inside a multi-byte UTF-8 sequence
        // (continuation bytes are all >= 0x80), so scanning bytes finds
        // exactly the newlines `char_indices()` would.
        let mut tops = Vec::with_capacity(code.len() / 24 + 1);
        tops.push(0);
        tops.extend(
            code.as_bytes()
                .iter()
                .enumerate()
                .filter(|(_, b)| **b == b'\n')
                .map(|(pos, _)| pos + 1),
        );
        Self(tops)
    }

    /// 1-based number of the line holding byte *pos*. A `'\n'` belongs to
    /// the line it terminates.
    fn line_no(&self, pos: usize) -> usize {
        self.0.partition_point(|&top| top <= pos)
    }

    /// Number of `'\n'` in the source.
    fn newlines(&self) -> usize {
        self.0.len() - 1
    }
}

/// Printed as a summary: the offsets themselves are noise in the debug
/// output of an error or an iseq, and there is one per line of the file.
impl std::fmt::Debug for LineTops {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "LineTops({} lines)", self.0.len())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceInfo {
    /// directory path of the source code.
    pub path: PathBuf,
    /// Load-time canonical (absolute, symlink-resolved) path of the
    /// source file; `None` for eval'd code and other non-file sources.
    /// Captured when the file is parsed, so
    /// `Thread::Backtrace::Location#absolute_path` stays correct after
    /// a later `Dir.chdir` or even removal of the file.
    pub absolute_path: Option<PathBuf>,
    /// source code text.
    pub code: String,
    /// line number offset for eval (0-based: e.g. lineno=1 means offset=0, lineno=42 means offset=41).
    pub line_offset: i64,
    /// Source encoding declared by a magic comment
    /// (`# encoding: NAME` / `# coding: NAME`). `None` means no
    /// magic comment was present, in which case callers should
    /// default to UTF-8 (Ruby's default source encoding since 2.0).
    /// Stored verbatim as written in the source so consumers can
    /// resolve aliases through their own normaliser.
    pub source_encoding: Option<String>,
    /// Line-start index over `code`, built at construction. Private, so
    /// the only way to build a `SourceInfo` stays the constructors below
    /// and the index can never disagree with `code`.
    line_tops: LineTops,
    /// Value of the `# frozen_string_literal:` magic comment, if present
    /// (`None` = not specified → string literals are mutable, Ruby's
    /// default). When `Some(true)`, every string literal in the file is a
    /// shared, frozen, deduplicated object.
    pub frozen_string_literal: Option<bool>,
}

impl Default for SourceInfo {
    fn default() -> Self {
        SourceInfo::new(PathBuf::default(), "")
    }
}

//
// public interface
//
impl SourceInfo {
    pub fn new(path: impl Into<PathBuf>, code: impl Into<String>) -> Self {
        let mut code = code.into();
        if !code.ends_with('\n') {
            code.push('\n');
        }
        SourceInfo {
            path: path.into(),
            absolute_path: None,
            line_tops: LineTops::new(&code),
            code,
            line_offset: 0,
            source_encoding: None,
            frozen_string_literal: None,
        }
    }

    /// Set the load-time canonical path (see `absolute_path`).
    pub fn with_absolute_path(mut self, absolute_path: Option<PathBuf>) -> Self {
        self.absolute_path = absolute_path;
        self
    }

    pub fn new_eval(
        path: impl Into<PathBuf>,
        code: impl Into<String>,
        line_offset: i64,
    ) -> Self {
        let mut code = code.into();
        if !code.ends_with('\n') {
            code.push('\n');
        }
        SourceInfo {
            path: path.into(),
            absolute_path: None,
            line_tops: LineTops::new(&code),
            code,
            line_offset,
            source_encoding: None,
            frozen_string_literal: None,
        }
    }

    /// Return a copy of this `SourceInfo` with `source_encoding` set
    /// to `enc`. Used by parsers to attach the magic-comment-derived
    /// source encoding after the source has already been wrapped.
    pub fn with_source_encoding(mut self, enc: Option<String>) -> Self {
        self.source_encoding = enc;
        self
    }

    /// Builder: attach the `# frozen_string_literal:` magic-comment value.
    pub fn with_frozen_string_literal(mut self, frozen: Option<bool>) -> Self {
        self.frozen_string_literal = frozen;
        self
    }

    pub fn get_line(&self, loc: &Loc) -> i64 {
        // A position past the end of the source reports the *last* line
        // rather than one past it — the shape a `Loc` synthesised at EOF
        // (an unterminated construct) needs.
        if loc.0 >= self.code.len() {
            return self.line_tops.newlines() as i64 + self.line_offset;
        }
        self.line_tops.line_no(loc.0) as i64 + self.line_offset
    }

    /// Get file name.
    pub fn file_name(&self) -> Cow<'_, str> {
        self.path.to_string_lossy()
    }

    /// Get short_file name.
    pub fn short_file_name(&self) -> Cow<'_, str> {
        if let Some(name) = self.path.file_name() {
            name.to_string_lossy()
        } else {
            Cow::Borrowed("<internal>")
        }
    }

    /// Show the location of *loc* in source text.
    pub fn show_loc(&self, loc: &Loc) {
        eprint!("{}", self.get_location(loc));
    }

    /// Return a string represents the location of `loc` in the source code using '^^^'.
    pub fn get_location(&self, loc: &Loc) -> String {
        let code = self.code.clone() + " ";
        let mut p = loc.0;
        while p < loc.1 + 1 {
            p += code[p..].chars().next().unwrap_or('\n').len_utf8();
        }
        let loc = if code.len() <= loc.0 {
            Loc(code.len() - 1, code.len())
        } else {
            Loc(loc.0, p)
        };
        if self.code.is_empty() {
            return "(internal)".to_string();
        }
        let mut res_string = String::new();
        let lines = self.get_lines(&loc);
        let term = console::Term::stdout();
        let term_width = term.size().1 as usize;
        if let Some(line) = lines.first() {
            res_string += &format!("{}:{}\n", self.file_name(), line.line_no as i64 + self.line_offset);
            for line in &lines {
                let start = line.top;
                let end = line.end;
                let mut lead = if loc.0 <= line.top {
                    0
                } else {
                    loc.0 - line.top
                };
                let offset = lead / term_width * term_width;
                let offset = if offset == 0 {
                    0
                } else {
                    let mut len = 0;
                    for c in code[start..loc.0].chars() {
                        len += c.len_utf8();
                        if len >= offset {
                            break;
                        }
                    }
                    len
                };
                lead -= offset;
                let range_start = std::cmp::max(loc.0, line.top);
                let range_end = std::cmp::min(loc.1, line.end);
                let length = if range_start == range_end {
                    1
                } else {
                    range_end - range_start
                };
                res_string += &code[(start + offset)..end];
                res_string += "\n";
                res_string +=
                    &" ".repeat(text_width(&code[(start + offset)..(start + offset + lead)]));
                res_string += &"^".repeat(text_width(
                    &code[(start + offset + lead)..(start + offset + lead + length)],
                ));
                res_string += "\n";
            }
        } else {
            res_string += &format!("NOT FOUND: {:?} {}\n", loc, code.len());
            let line = match lines.last() {
                Some(line) => (line.line_no + 1, line.end + 1, loc.1),
                None => (1, 0, loc.1),
            };
            let lead = text_width(&code[line.1..loc.0]);
            let length = text_width(&code[loc.0..loc.1]);
            let is_cr = loc.1 >= code.len() || self.get_next_char(loc.1) == Some('\n');
            res_string += &format!("{}:{}\n", self.file_name(), line.0);
            res_string += if !is_cr {
                &code[line.1..=loc.1]
            } else {
                &code[line.1..loc.1]
            };
            res_string += &" ".repeat(lead);
            res_string += &"^".repeat(length + 1);
            res_string += "\n";
        }
        res_string
    }
}

fn text_width(s: &str) -> usize {
    console::measure_text_width(s) + s.chars().filter(|c| c == &'\t').count() * 7
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The scan [`SourceInfo::get_line`] used to perform, kept as the
    /// oracle the binary search is checked against.
    fn get_line_by_scan(info: &SourceInfo, loc: &Loc) -> i64 {
        if loc.0 >= info.code.len() {
            return info
                .code
                .char_indices()
                .filter_map(|(pos, ch)| if ch == '\n' { Some(pos) } else { None })
                .count() as i64
                + info.line_offset;
        }
        let mut line_top = 0;
        info.code
            .char_indices()
            .filter_map(|(pos, ch)| if ch == '\n' { Some(pos) } else { None })
            .enumerate()
            .map(|(idx, pos)| {
                let top = line_top;
                line_top = pos + 1;
                Line::new(idx + 1, top, pos)
            })
            .find_map(|line| {
                if line.end >= loc.0 && line.top <= loc.0 {
                    Some(line.line_no as i64 + info.line_offset)
                } else {
                    None
                }
            })
            .unwrap()
    }

    /// Every byte offset of a few awkward sources — an empty file, one
    /// with blank lines, one with multi-byte characters (a `'\n'` byte
    /// never occurs inside a UTF-8 sequence, which is what lets the
    /// index scan bytes), one without a trailing newline, and an eval'd
    /// one carrying a `line_offset` — must answer exactly what the scan
    /// answered, including one position past the end.
    #[test]
    fn get_line_matches_the_scan_it_replaced() {
        for (code, line_offset) in [
            ("", 0),
            ("\n", 0),
            ("\n\n\n", 0),
            ("a", 0),
            ("a\nbb\n\nccc\n", 0),
            ("a\nbb\n\nccc", 0),
            ("日本語\nと\n改行\n", 0),
            ("# コメント\ndef f(x) = x + 1\nf(2)\n", 0),
            ("a\nbb\nccc\n", 41),
            ("a\nbb\nccc\n", -1),
        ] {
            let info = SourceInfo::new_eval("(test)", code, line_offset);
            for pos in 0..=info.code.len() {
                let loc = Loc(pos, pos);
                assert_eq!(
                    get_line_by_scan(&info, &loc),
                    info.get_line(&loc),
                    "code {code:?}, offset {line_offset}, pos {pos}"
                );
            }
        }
    }
}

impl SourceInfo {
    fn get_next_char(&self, pos: usize) -> Option<char> {
        self.code[pos..].chars().next()
    }

    fn get_lines(&self, loc: &Loc) -> Vec<Line> {
        let mut line_top = 0;
        let mut line_max = 1;
        let code_len = self.code.len();
        let mut lines: Vec<_> = self
            .code
            .char_indices()
            .filter_map(|(pos, ch)| if ch == '\n' { Some(pos) } else { None })
            .enumerate()
            .map(|(idx, pos)| {
                let top = line_top;
                line_top = pos + 1;
                line_max = idx + 1;
                Line::new(idx + 1, top, pos)
            })
            .filter(|line| {
                if loc.0 == loc.1 {
                    line.top <= loc.1 && line.end >= loc.0
                } else {
                    line.top < loc.1 && line.end >= loc.0
                }
            })
            .collect();
        if line_top <= code_len && code_len >= loc.0 && line_top < loc.1 {
            lines.push(Line::new(line_max + 1, line_top, code_len));
        }
        lines
    }
}
