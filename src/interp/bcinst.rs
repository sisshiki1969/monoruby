use super::*;

#[derive(Clone, PartialEq)]
pub(super) enum BcIr {
    Br(usize),
    CondBr(BcReg, usize),
    CondNotBr(BcReg, usize),
    Integer(BcReg, i32),
    Float(BcReg, f64),
    Nil(BcReg),
    Neg(BcReg, BcReg),                 // ret, src
    Add(BcReg, BcReg, BcReg),          // ret, lhs, rhs
    Addri(BcReg, BcReg, i16),          // ret, lhs, int
    Sub(BcReg, BcReg, BcReg),          // ret, lhs, rhs
    Subri(BcReg, BcReg, i16),          // ret, lhs, int
    Mul(BcReg, BcReg, BcReg),          // ret, lhs, rhs
    Div(BcReg, BcReg, BcReg),          // ret, lhs, rhs
    Cmp(CmpKind, BcReg, BcReg, BcReg), // kind, lhs, rhs
    Cmpri(CmpKind, BcReg, BcReg, i16), // kind, lhs, rhs
    Ret(BcReg),
    Mov(BcReg, BcReg),                            // dst, offset
    Call(BcFuncId, Option<BcReg>, BcTemp, usize), // (id, ret, args, args_len)
}

impl std::fmt::Debug for BcIr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Br(dst) => write!(f, "br =>{}", dst),
            Self::CondBr(reg, dst) => write!(f, "condbr {:?} =>{}", reg, dst),
            Self::CondNotBr(reg, dst) => write!(f, "condnbr {:?} =>{}", reg, dst),
            Self::Integer(reg, num) => write!(f, "{:?} = {}", reg, num),
            Self::Float(reg, num) => write!(f, "{:?} = {}", reg, num),
            Self::Nil(reg) => write!(f, "{:?} = nil", reg),
            Self::Neg(dst, src) => write!(f, "{:?} = neg {:?}", dst, src),
            Self::Add(dst, lhs, rhs) => write!(f, "{:?} = {:?} + {:?}", dst, lhs, rhs),
            Self::Addri(dst, lhs, rhs) => write!(f, "{:?} = {:?} + {}:i16", dst, lhs, rhs),
            Self::Sub(dst, lhs, rhs) => write!(f, "{:?} = {:?} - {:?}", dst, lhs, rhs),
            Self::Subri(dst, lhs, rhs) => write!(f, "{:?} = {:?} - {}:i16", dst, lhs, rhs),
            Self::Mul(dst, lhs, rhs) => write!(f, "{:?} = {:?} * {:?}", dst, lhs, rhs),
            Self::Div(dst, lhs, rhs) => write!(f, "{:?} = {:?} / {:?}", dst, lhs, rhs),
            Self::Cmp(kind, dst, lhs, rhs) => {
                write!(f, "{:?} = {:?} {:?} {:?}", dst, lhs, kind, rhs)
            }
            Self::Cmpri(kind, dst, lhs, rhs) => {
                write!(f, "{:?} = {:?} {:?} {}:i16", dst, lhs, kind, rhs)
            }
            Self::Ret(reg) => write!(f, "ret {:?}", reg),
            Self::Mov(dst, src) => write!(f, "{:?} = {:?}", dst, src),
            Self::Call(id, ret, arg, len) => match ret {
                Some(ret) => write!(f, "{:?} = call {:?} ({:?}: {})", ret, id, arg, len),
                None => write!(f, "_ = call {:?} ({:?}: {})", id, arg, len),
            },
        }
    }
}
