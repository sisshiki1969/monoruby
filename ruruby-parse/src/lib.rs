#![feature(let_chains)]

mod error;
mod lvar_collector;
mod node;
mod parser;
mod source_info;
mod token;
pub use error::*;
pub use lvar_collector::*;
pub use node::*;
pub use parser::*;
pub use source_info::*;
use token::*;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Annot<T: PartialEq + Default> {
    pub kind: T,
    pub loc: Loc,
}

impl<T: PartialEq + Default> Annot<T> {
    pub fn new(kind: T, loc: Loc) -> Self {
        Annot { kind, loc }
    }

    pub fn loc(&self) -> Loc {
        self.loc
    }
}

#[derive(Debug, Clone, PartialEq)]
enum ParenKind {
    Bracket,
    Paren,
    Brace,
}

#[derive(Debug, Clone, PartialEq)]
enum RubyString {
    Bytes(Vec<u8>),
    Utf8(String),
}

impl std::default::Default for RubyString {
    fn default() -> Self {
        RubyString::Bytes(vec![])
    }
}

impl std::convert::From<String> for RubyString {
    fn from(s: String) -> Self {
        RubyString::Utf8(s)
    }
}

impl std::ops::AddAssign<&str> for RubyString {
    fn add_assign(&mut self, rhs: &str) {
        match self {
            RubyString::Bytes(bytes) => {
                bytes.extend_from_slice(rhs.as_bytes());
            }
            RubyString::Utf8(string) => {
                string.push_str(rhs);
            }
        }
    }
}

impl std::ops::AddAssign<&RubyString> for RubyString {
    fn add_assign(&mut self, rhs: &RubyString) {
        match (std::mem::take(self), rhs) {
            (RubyString::Utf8(string), RubyString::Bytes(rhs)) => {
                let mut bytes = string.into_bytes();
                bytes.extend_from_slice(rhs);
                *self = RubyString::Bytes(bytes);
            }
            (RubyString::Utf8(mut string), RubyString::Utf8(rhs)) => {
                string.push_str(rhs);
                *self = RubyString::Utf8(string);
            }
            (RubyString::Bytes(mut bytes), RubyString::Bytes(rhs)) => {
                bytes.extend_from_slice(rhs);
                *self = RubyString::Bytes(bytes);
            }
            (RubyString::Bytes(mut bytes), RubyString::Utf8(rhs)) => {
                bytes.extend_from_slice(rhs.as_bytes());
                *self = RubyString::Bytes(bytes);
            }
        }
    }
}

impl RubyString {
    fn new() -> Self {
        RubyString::Utf8(String::new())
    }

    fn into_string(self) -> Result<String, LexerErr> {
        match self {
            RubyString::Bytes(bytes) => {
                let s = String::from_utf8(bytes).map_err(|_| {
                    LexerErr(
                        ParseErrKind::SyntaxError("Invalid UTF-8.".to_string()),
                        Loc(0, 0),
                    )
                })?;
                Ok(s)
            }
            RubyString::Utf8(string) => Ok(string),
        }
    }

    fn push_char(&mut self, ch: char) {
        match self {
            RubyString::Bytes(bytes) => {
                bytes.extend_from_slice(ch.to_string().as_bytes());
            }
            RubyString::Utf8(string) => {
                string.push(ch);
            }
        }
    }

    fn push_byte(&mut self, b: u8) {
        let mut bytes = match std::mem::take(self) {
            RubyString::Bytes(bytes) => bytes,
            RubyString::Utf8(string) => string.into_bytes(),
        };
        bytes.push(b);
        *self = RubyString::Bytes(bytes);
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn test() {
        use crate::parser::*;
        let res = Parser::<DummyContext>::parse_program(
            "nil".to_string(),
            std::path::PathBuf::from("path"),
        )
        .unwrap();
        eprintln!("{:?}", res)
    }
}
