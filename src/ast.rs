///
/// AST for expression.
///
#[derive(Debug, PartialEq)]
pub enum Expr {
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Integer(i32),
    Float(f64),
}
