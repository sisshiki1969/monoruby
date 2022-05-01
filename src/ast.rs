#[derive(Clone, Copy, PartialEq)]
pub enum CmpKind {
    Eq,
    Ne,
    Ge,
    Gt,
    Le,
    Lt,
}

impl std::fmt::Debug for CmpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eq => write!(f, "=="),
            Self::Ne => write!(f, "!="),
            Self::Ge => write!(f, ">="),
            Self::Gt => write!(f, ">"),
            Self::Le => write!(f, "<="),
            Self::Lt => write!(f, "<"),
        }
    }
}
