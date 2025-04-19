use super::*;

#[derive(Debug, Clone, PartialEq)]
pub struct ParseErr {
    pub kind: ParseErrKind,
    pub loc: Loc,
    pub source_info: SourceInfoRef,
}

impl ParseErr {
    pub(crate) fn from_lexer_err(err: LexerErr, source_info: SourceInfoRef) -> Self {
        Self {
            kind: err.0,
            loc: err.1,
            source_info,
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum ParseErrKind {
    UnexpectedEOF,
    SyntaxError(String),
}

impl std::fmt::Debug for ParseErrKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedEOF => write!(f, "SyntaxError (Unexpected EOF.)"),
            Self::SyntaxError(msg) => write!(f, "SyntaxError ({})", msg),
        }
    }
}
