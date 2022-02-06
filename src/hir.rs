use super::*;

#[derive(Clone, Debug, PartialEq)]
pub struct HIR {
    ty: Type,
    op: HIROp,
}

impl HIR {
    pub fn ty(&self) -> Type {
        self.ty
    }

    pub fn kind(&self) -> &HIROp {
        &self.op
    }

    fn new_integer(i: i32) -> Self {
        HIR {
            ty: Type::Integer,
            op: HIROp::Integer(i),
        }
    }

    fn new_float(f: f64) -> Self {
        HIR {
            ty: Type::Float,
            op: HIROp::Float(f),
        }
    }

    fn new_ineg(lhs: HIR) -> Self {
        assert_eq!(Type::Integer, lhs.ty);
        HIR {
            ty: Type::Integer,
            op: HIROp::INeg(Box::new(lhs)),
        }
    }

    fn new_fneg(lhs: HIR) -> Self {
        assert_eq!(Type::Float, lhs.ty);
        HIR {
            ty: Type::Float,
            op: HIROp::FNeg(Box::new(lhs)),
        }
    }

    fn new_iadd(lhs: HIR, rhs: HIR) -> Self {
        assert_eq!(Type::Integer, lhs.ty);
        assert_eq!(Type::Integer, rhs.ty);
        HIR {
            ty: Type::Integer,
            op: HIROp::IAdd(Box::new(lhs), Box::new(rhs)),
        }
    }

    fn new_fadd(lhs: HIR, rhs: HIR) -> Self {
        assert_eq!(Type::Float, lhs.ty);
        assert_eq!(Type::Float, rhs.ty);
        HIR {
            ty: Type::Float,
            op: HIROp::FAdd(Box::new(lhs), Box::new(rhs)),
        }
    }

    fn new_isub(lhs: HIR, rhs: HIR) -> Self {
        assert_eq!(Type::Integer, lhs.ty);
        assert_eq!(Type::Integer, rhs.ty);
        HIR {
            ty: Type::Integer,
            op: HIROp::ISub(Box::new(lhs), Box::new(rhs)),
        }
    }

    fn new_fsub(lhs: HIR, rhs: HIR) -> Self {
        assert_eq!(Type::Float, lhs.ty);
        assert_eq!(Type::Float, rhs.ty);
        HIR {
            ty: Type::Float,
            op: HIROp::FSub(Box::new(lhs), Box::new(rhs)),
        }
    }

    fn new_imul(lhs: HIR, rhs: HIR) -> Self {
        assert_eq!(Type::Integer, lhs.ty);
        assert_eq!(Type::Integer, rhs.ty);
        HIR {
            ty: Type::Integer,
            op: HIROp::IMul(Box::new(lhs), Box::new(rhs)),
        }
    }

    fn new_fmul(lhs: HIR, rhs: HIR) -> Self {
        assert_eq!(Type::Float, lhs.ty);
        assert_eq!(Type::Float, rhs.ty);
        HIR {
            ty: Type::Float,
            op: HIROp::FMul(Box::new(lhs), Box::new(rhs)),
        }
    }

    fn new_idiv(lhs: HIR, rhs: HIR) -> Self {
        assert_eq!(Type::Integer, lhs.ty);
        assert_eq!(Type::Integer, rhs.ty);
        HIR {
            ty: Type::Integer,
            op: HIROp::IDiv(Box::new(lhs), Box::new(rhs)),
        }
    }

    fn new_fdiv(lhs: HIR, rhs: HIR) -> Self {
        assert_eq!(Type::Float, lhs.ty);
        assert_eq!(Type::Float, rhs.ty);
        HIR {
            ty: Type::Float,
            op: HIROp::FDiv(Box::new(lhs), Box::new(rhs)),
        }
    }

    fn new_as_float(hir: HIR) -> Self {
        assert_eq!(Type::Integer, hir.ty);
        HIR {
            ty: Type::Float,
            op: match hir.op {
                HIROp::Integer(i) => HIROp::Float(i as f64),
                _ => HIROp::IntAsFloat(Box::new(hir)),
            },
        }
    }
}

impl HIR {
    pub fn from_ast(ast: &Expr) -> Self {
        match ast {
            Expr::Integer(i) => HIR::new_integer(*i),
            Expr::Float(f) => HIR::new_float(*f),
            Expr::Neg(box lhs) => {
                let lhs = HIR::from_ast(lhs);
                match lhs.op {
                    HIROp::Integer(i) => HIR::new_integer(-i),
                    HIROp::Float(f) => HIR::new_float(-f),
                    _ => match lhs.ty {
                        Type::Integer => HIR::new_ineg(lhs),
                        Type::Float => HIR::new_fneg(lhs),
                    },
                }
            }
            Expr::Add(box lhs, box rhs) => {
                let lhs = HIR::from_ast(lhs);
                let rhs = HIR::from_ast(rhs);
                match (lhs.ty, rhs.ty) {
                    (Type::Integer, Type::Integer) => HIR::new_iadd(lhs, rhs),
                    (Type::Integer, Type::Float) => {
                        let lhs = HIR::new_as_float(lhs);
                        HIR::new_fadd(lhs, rhs)
                    }
                    (Type::Float, Type::Integer) => {
                        let rhs = HIR::new_as_float(rhs);
                        HIR::new_fadd(lhs, rhs)
                    }
                    (Type::Float, Type::Float) => HIR::new_fadd(lhs, rhs),
                }
            }
            Expr::Sub(box lhs, box rhs) => {
                let lhs = HIR::from_ast(lhs);
                let rhs = HIR::from_ast(rhs);
                match (lhs.ty, rhs.ty) {
                    (Type::Integer, Type::Integer) => HIR::new_isub(lhs, rhs),
                    (Type::Integer, Type::Float) => {
                        let lhs = HIR::new_as_float(lhs);
                        HIR::new_fsub(lhs, rhs)
                    }
                    (Type::Float, Type::Integer) => {
                        let rhs = HIR::new_as_float(rhs);
                        HIR::new_fsub(lhs, rhs)
                    }
                    (Type::Float, Type::Float) => HIR::new_fsub(lhs, rhs),
                }
            }
            Expr::Mul(box lhs, box rhs) => {
                let lhs = HIR::from_ast(lhs);
                let rhs = HIR::from_ast(rhs);
                match (lhs.ty, rhs.ty) {
                    (Type::Integer, Type::Integer) => HIR::new_imul(lhs, rhs),
                    (Type::Integer, Type::Float) => {
                        let lhs = HIR::new_as_float(lhs);
                        HIR::new_fmul(lhs, rhs)
                    }
                    (Type::Float, Type::Integer) => {
                        let rhs = HIR::new_as_float(rhs);
                        HIR::new_fmul(lhs, rhs)
                    }
                    (Type::Float, Type::Float) => HIR::new_fmul(lhs, rhs),
                }
            }
            Expr::Div(box lhs, box rhs) => {
                let lhs = HIR::from_ast(lhs);
                let rhs = HIR::from_ast(rhs);
                match (lhs.ty, rhs.ty) {
                    (Type::Integer, Type::Integer) => HIR::new_idiv(lhs, rhs),
                    (Type::Integer, Type::Float) => {
                        let lhs = HIR::new_as_float(lhs);
                        HIR::new_fdiv(lhs, rhs)
                    }
                    (Type::Float, Type::Integer) => {
                        let rhs = HIR::new_as_float(rhs);
                        HIR::new_fdiv(lhs, rhs)
                    }
                    (Type::Float, Type::Float) => HIR::new_fdiv(lhs, rhs),
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum HIROp {
    Integer(i32),
    IntAsFloat(Box<HIR>),
    Float(f64),
    INeg(Box<HIR>),
    FNeg(Box<HIR>),
    IAdd(Box<HIR>, Box<HIR>),
    FAdd(Box<HIR>, Box<HIR>),
    ISub(Box<HIR>, Box<HIR>),
    FSub(Box<HIR>, Box<HIR>),
    IMul(Box<HIR>, Box<HIR>),
    FMul(Box<HIR>, Box<HIR>),
    IDiv(Box<HIR>, Box<HIR>),
    FDiv(Box<HIR>, Box<HIR>),
}
