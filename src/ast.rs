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
    If(Box<Spanned<Expr>>, Vec<Spanned<Expr>>, Vec<Spanned<Expr>>), // (cond, then, else)
    While(Box<Spanned<Expr>>, Vec<Spanned<Expr>>),
    Call(String, Box<Spanned<Expr>>), // (func_name, arg)

    Mul(Box<Spanned<Expr>>, Box<Spanned<Expr>>), // (lhs, rhs)
    Div(Box<Spanned<Expr>>, Box<Spanned<Expr>>), // (lhs, rhs)
    Add(Box<Spanned<Expr>>, Box<Spanned<Expr>>), // (lhs, rhs)
    Sub(Box<Spanned<Expr>>, Box<Spanned<Expr>>), // (lhs, rhs)
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
