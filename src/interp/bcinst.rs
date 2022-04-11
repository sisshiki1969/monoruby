use super::*;

#[derive(Debug, Clone, PartialEq)]
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
    LocalStore(Option<Reg>, Local, Reg),     // offset, src
    LocalLoad(Local, Reg),                   // offset, dst
    Call(BcFuncId, Option<Reg>, Reg, usize), // (id, ret, args, args_len)
}
