use super::*;

#[derive(Clone, PartialEq)]
pub(super) enum Inst {
    Br(usize),
    CondBr(Reg, usize),
    CondNotBr(Reg, usize),
    Integer(Reg, i32),
    Float(Reg, f64),
    Nil(Reg),
    Neg(Reg, Reg),               // ret, src
    Add(Reg, Reg, Reg),          // ret, lhs, rhs
    Sub(Reg, Reg, Reg),          // ret, lhs, rhs
    Mul(Reg, Reg, Reg),          // ret, lhs, rhs
    Div(Reg, Reg, Reg),          // ret, lhs, rhs
    Cmp(CmpKind, Reg, Reg, Reg), // kind, lhs, rhs
    Ret(Reg),
    Mov(Reg, Reg),                            // dst, offset
    Call(BcFuncId, Option<Reg>, Temp, usize), // (id, ret, args, args_len)
}

impl std::fmt::Debug for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Br(dst) => write!(f, "br\t{}", dst),
            Self::CondBr(reg, dst) => write!(f, "condbr\t{:?}\t{}", reg, dst),
            Self::CondNotBr(reg, dst) => write!(f, "condnbr\t{:?}\t{}", reg, dst),
            Self::Integer(reg, num) => write!(f, "integer\t{:?}\t{}", reg, num),
            Self::Float(reg, num) => write!(f, "integer\t{:?}\t{}", reg, num),
            Self::Nil(reg) => write!(f, "nil\t{:?}", reg),
            Self::Neg(dst, src) => write!(f, "neg\t{:?}\t{:?}", dst, src),
            Self::Add(dst, lhs, rhs) => write!(f, "add\t{:?}\t{:?}\t{:?}", dst, lhs, rhs),
            Self::Sub(dst, lhs, rhs) => write!(f, "sub\t{:?}\t{:?}\t{:?}", dst, lhs, rhs),
            Self::Mul(dst, lhs, rhs) => write!(f, "mul\t{:?}\t{:?}\t{:?}", dst, lhs, rhs),
            Self::Div(dst, lhs, rhs) => write!(f, "div\t{:?}\t{:?}\t{:?}", dst, lhs, rhs),
            Self::Cmp(kind, dst, lhs, rhs) => {
                write!(f, "cmp\t{:?}\t{:?}\t{:?}\t{:?}", kind, dst, lhs, rhs)
            }
            Self::Ret(reg) => write!(f, "ret\t{:?}", reg),
            Self::Mov(dst, src) => write!(f, "mov\t{:?}\t{:?}", dst, src),
            Self::Call(id, ret, arg, len) => {
                write!(f, "call\t{:?}\t{:?}\t{:?}:{}", id, ret, arg, len)
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum Reg {
    Local(Local),
    Temp(Temp),
}

impl std::fmt::Debug for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Local(local) => write!(f, "{:?}", local),
            Self::Temp(temp) => write!(f, "{:?}", temp),
        }
    }
}

impl std::convert::From<Local> for Reg {
    fn from(local: Local) -> Self {
        Reg::Local(local)
    }
}

impl std::convert::From<Temp> for Reg {
    fn from(temp: Temp) -> Self {
        Reg::Temp(temp)
    }
}
