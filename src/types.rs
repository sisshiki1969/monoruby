#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Integer,
    Float,
    Bool,
    Nil,
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Integer => "i32",
            Self::Float => "f64",
            Self::Bool => "bool",
            Self::Nil => "nil",
        };
        write!(f, "{}", s)
    }
}
