use super::*;
use ruruby_parse::IdentId;

///
/// bytecode Ir.
///
#[derive(Debug, Clone, PartialEq)]
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

///
/// Bytecode instructions.
///
#[derive(Debug, Clone, PartialEq)]
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
    /// eq(%ret, %lhs, %rhs)
    Eq(u16, u16, u16),
    /// ne(%ret, %lhs, %rhs)
    Ne(u16, u16, u16),
    /// lt(%ret, %lhs, %rhs)
    Lt(u16, u16, u16),
    /// le(%ret, %lhs, %rhs)
    Le(u16, u16, u16),
    /// gt(%ret, %lhs, %rhs)
    Gt(u16, u16, u16),
    /// ge(%ret, %lhs, %rhs)
    Ge(u16, u16, u16),
    /// eqri(%ret, %lhs, rhs: i16)
    Eqri(u16, u16, i16),
    /// neri(%ret, %lhs, rhs: i16)
    Neri(u16, u16, i16),
    /// ltri(%ret, %lhs, rhs: i16)
    Ltri(u16, u16, i16),
    /// leri(%ret, %lhs, rhs: i16)
    Leri(u16, u16, i16),
    /// gtri(%ret, %lhs, rhs: i16)
    Gtri(u16, u16, i16),
    /// geri(%ret, %lhs, rhs: i16)
    Geri(u16, u16, i16),
    /// return(%ret)
    Ret(u16),
    /// move(%dst, %src)
    Mov(u16, u16),
    /// func call(func_id, %ret, %args, args_len)
    FnCall(IdentId, u16, u16, u16),
}
