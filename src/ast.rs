use super::*;

///
/// AST for statements.
///
#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Expr(Spanned<Expr>),
    Decl(Spanned<Decl>),
}

impl Default for Stmt {
    fn default() -> Self {
        Stmt::Expr((Expr::Nil, Span::default()))
    }
}

impl Stmt {
    pub fn expr(expr: Spanned<Expr>) -> Spanned<Stmt> {
        let span = expr.1.clone();
        (Self::Expr(expr), span)
    }
}

///
/// AST for expression.
///
#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
    MethodDef(String, Vec<String>, Vec<Spanned<Expr>>),
}

///
/// AST for expression.
///
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    If(Box<Spanned<Expr>>, Vec<Spanned<Expr>>, Vec<Spanned<Expr>>), // (cond, then, else)
    While(Box<Spanned<Expr>>, Vec<Spanned<Expr>>),
    Call(String, Vec<Spanned<Expr>>), // (func_name, arg)

    Mul(Box<Spanned<Expr>>, Box<Spanned<Expr>>), // (lhs, rhs)
    Div(Box<Spanned<Expr>>, Box<Spanned<Expr>>), // (lhs, rhs)
    Add(Box<Spanned<Expr>>, Box<Spanned<Expr>>), // (lhs, rhs)
    Sub(Box<Spanned<Expr>>, Box<Spanned<Expr>>), // (lhs, rhs)
    Neg(Box<Spanned<Expr>>),
    Cmp(CmpKind, Box<Spanned<Expr>>, Box<Spanned<Expr>>),

    Integer(i32),
    Float(f64),
    Nil,
    True,
    False,
    Self_,
    LocalStore(String, Box<Spanned<Expr>>),
    LocalLoad(String),
    Return(Box<Spanned<Stmt>>),
}

impl Default for Expr {
    fn default() -> Self {
        Expr::Nil
    }
}

impl Expr {
    pub fn is_local(&self) -> Option<String> {
        if let Self::LocalLoad(ident) = self {
            Some(ident.to_owned())
        } else {
            None
        }
    }

    pub fn is_smi(&self) -> Option<i16> {
        if let Self::Integer(i) = self {
            if *i == *i as i16 as i32 {
                return Some(*i as i16);
            }
        }
        None
    }

    pub fn ret(stmt: Spanned<Stmt>) -> Self {
        Self::Return(Box::new(stmt))
    }
}

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
