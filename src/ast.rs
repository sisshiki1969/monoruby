use super::*;

///
/// AST for statements.
///
#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Expr(Spanned<Expr>),
    Decl(Spanned<Decl>),
}

///
/// AST for expression.
///
#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
    MethodDef(String, String, Vec<Spanned<Expr>>),
}

///
/// AST for expression.
///
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    If(Box<Spanned<Expr>>, Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Mul(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Div(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Add(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Sub(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Neg(Box<Spanned<Expr>>),
    Cmp(CmpKind, Box<Spanned<Expr>>, Box<Spanned<Expr>>),

    Integer(i32),
    Float(f64),
    LocalStore(String, Box<Spanned<Expr>>),
    LocalLoad(String),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CmpKind {
    Eq,
    Ne,
    Ge,
    Gt,
    Le,
    Lt,
}
