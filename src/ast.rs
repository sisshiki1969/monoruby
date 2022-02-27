///
/// AST for statements.
///
#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Decl(Decl),
}

///
/// AST for expression.
///
#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
    MethodDef(String, String, Vec<Expr>),
}

///
/// AST for expression.
///
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Cmp(CmpKind, Box<Expr>, Box<Expr>),

    Integer(i32),
    Float(f64),
    LocalStore(String, Box<Expr>),
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
