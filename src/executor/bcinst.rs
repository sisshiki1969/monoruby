use super::*;

///
/// bytecode Ir.
///
#[derive(Clone, PartialEq)]
pub(super) enum BcIr {
    Br(usize),
    CondBr(BcReg, usize),
    CondNotBr(BcReg, usize),
    Integer(BcReg, i32),
    Const(BcReg, u32),
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
    Mov(BcReg, BcReg),                             // dst, offset
    FnCall(IdentId, Option<BcReg>, BcTemp, usize), // (id, ret, args, args_len)
}

impl std::fmt::Debug for BcIr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Br(dst) => write!(f, "br =>{}", dst),
            Self::CondBr(reg, dst) => write!(f, "condbr {:?} =>{}", reg, dst),
            Self::CondNotBr(reg, dst) => write!(f, "condnbr {:?} =>{}", reg, dst),
            Self::Integer(reg, num) => write!(f, "{:?} = {}", reg, num),
            Self::Const(reg, id) => write!(f, "{:?} = const[{}]", reg, id),
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
            Self::FnCall(id, ret, arg, len) => match ret {
                Some(ret) => write!(f, "{:?} = call {:?} ({:?}: {})", id, ret, arg, len),
                None => write!(f, "_ = call {:?} ({:?}: {})", id, arg, len),
            },
        }
    }
}

///
/// Bytecode instructions.
///
#[derive(Clone, PartialEq)]
pub(super) enum BcOp {
    /// branch(dest)
    Br(InstId),
    /// conditional branch(%reg, dest)  : branch when reg was true.
    CondBr(u16, InstId),
    /// conditional branch(%reg, dest)  : branch when reg was false.
    CondNotBr(u16, InstId),
    /// integer(%reg, i32)
    Integer(u16, i32),
    /// constant(%ret, constant_id)
    Const(u16, u32),
    /// nil(%reg)
    Nil(u16),
    /// negate(%ret, %src)
    Neg(u16, u16),
    /// add(%ret, %lhs, %rhs)
    Add(u16, u16, u16),
    /// add with small integer(%ret, %lhs, rhs:i16)
    Addri(u16, u16, i16),
    /// sub(%ret, %lhs, %rhs)
    Sub(u16, u16, u16),
    /// sub with small integer(%ret, %lhs, rhs:i16)
    Subri(u16, u16, i16),
    /// mul(%ret, %lhs, %rhs)
    Mul(u16, u16, u16),
    /// div(%ret, %lhs, %rhs)
    Div(u16, u16, u16),
    /// compare(cmp_kind, %ret, %lhs, %rhs)
    Cmp(CmpKind, u16, u16, u16),
    /// compare to small integer(cmp_kind, %ret, %lhs, rhs:i16)
    Cmpri(CmpKind, u16, u16, i16),
    /// return(%ret)
    Ret(u16),
    /// move(%dst, %src)
    Mov(u16, u16),
    /// func call(func_id, %ret, %args, args_len)
    FnCall(IdentId, u16, u16, u16),
}

impl std::fmt::Debug for BcOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Br(dst) => write!(f, "br =>{:?}", dst),
            Self::CondBr(reg, dst) => write!(f, "condbr {:?} =>{:?}", reg, dst),
            Self::CondNotBr(reg, dst) => write!(f, "condnbr {:?} =>{:?}", reg, dst),
            Self::Integer(reg, num) => write!(f, "{:?} = {}", reg, num),
            Self::Const(reg, id) => write!(f, "{:?} = const[{}]", reg, id),
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
            Self::FnCall(id, ret, arg, len) => {
                if *ret == u16::MAX {
                    write!(f, "_ = call {:?} ({:?}: {})", id, arg, len)
                } else {
                    write!(f, "{:?} = call {:?} ({:?}: {})", id, ret, arg, len)
                }
            }
        }
    }
}
