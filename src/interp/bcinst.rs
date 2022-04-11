use super::*;

#[derive(Debug, Clone, PartialEq)]
pub(super) enum Inst {
    Br(usize),
    CondBr(Temp, usize),
    CondNotBr(Temp, usize),
    Integer(Reg, i32),
    Float(Temp, f64),
    Nil(Temp),
    Neg(Temp, Temp),                // ret, src
    Add(Temp, Temp, Temp),          // ret, lhs, rhs
    Sub(Temp, Temp, Temp),          // ret, lhs, rhs
    Mul(Temp, Temp, Temp),          // ret, lhs, rhs
    Div(Temp, Temp, Temp),          // ret, lhs, rhs
    Cmp(CmpKind, Temp, Temp, Temp), // kind, lhs, rhs
    Ret(Temp),
    Mov(Reg, Reg),                             // dst, offset
    Call(BcFuncId, Option<Temp>, Temp, usize), // (id, ret, args, args_len)
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
