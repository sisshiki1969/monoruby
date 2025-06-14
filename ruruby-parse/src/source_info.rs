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

#[derive(Debug, Clone, PartialEq)]
pub struct SourceInfo {
    /// directory path of the source code.
    pub path: PathBuf,
    /// source code text.
    pub code: String,
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
            code,
        }
    }

    pub fn get_line(&self, loc: &Loc) -> usize {
        if loc.0 >= self.code.len() {
            return self
                .code
                .char_indices()
                .filter_map(|(pos, ch)| if ch == '\n' { Some(pos) } else { None })
                .count();
        }
        let mut line_top = 0;
        self.code
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
                    Some(line.line_no)
                } else {
                    None
                }
            })
            .unwrap()
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
            res_string += &format!("{}:{}\n", self.file_name(), line.line_no);
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
                res_string += &" ".repeat(console::measure_text_width(
                    &code[(start + offset)..(start + offset + lead)],
                ));
                res_string += &"^".repeat(console::measure_text_width(
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
            let lead = console::measure_text_width(&code[line.1..loc.0]);
            let length = console::measure_text_width(&code[loc.0..loc.1]);
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
