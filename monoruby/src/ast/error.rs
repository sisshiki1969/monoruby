use super::*;

#[derive(Debug, Clone, PartialEq)]
pub struct ParseErr {
    pub kind: ParseErrKind,
    pub loc: Loc,
    pub source_info: SourceInfoRef,
}

#[derive(Clone, PartialEq)]
pub enum ParseErrKind {
    UnexpectedEOF,
    SyntaxError(String),
    /// What prism reported for a source that does not parse, as the
    /// error CRuby raises for it (`pm_parse_process_error`): its class,
    /// and its message in the encoding of the source (#2230).
    Diagnostics {
        level: crate::parser::ErrorLevel,
        message: Vec<u8>,
        encoding: crate::value::Encoding,
    },
}

impl std::fmt::Debug for ParseErrKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedEOF => write!(f, "SyntaxError (Unexpected EOF.)"),
            Self::SyntaxError(msg) => write!(f, "SyntaxError ({})", msg),
            Self::Diagnostics { level, message, .. } => {
                write!(f, "{level:?} ({})", String::from_utf8_lossy(message))
            }
        }
    }
}
