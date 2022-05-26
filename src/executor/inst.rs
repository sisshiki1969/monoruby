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
    MethodDef(IdentId, FuncId),
}

///
/// Bytecode instructions.
///
#[derive(Debug, Clone, PartialEq)]
pub(super) enum BcOp {
    /// branch(dest)
    Br(i32),
    /// conditional branch(%reg, dest)  : branch when reg was true.
    CondBr(u16, i32),
    /// conditional branch(%reg, dest)  : branch when reg was false.
    CondNotBr(u16, i32),
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
    /// func call(%ret, callsite_id)  if there is no return register, %ret = 0.
    FnCall(u16, CallsiteId),
    /// method definition(method_def_id)
    MethodDef(MethodDefId),
}

fn enc_wl(opcode: u16, op1: u16, op2: u32) -> u64 {
    ((opcode as u64) << 48) + ((op1 as u64) << 32) + (op2 as u64)
}

fn enc_l(opcode: u16, op1: u32) -> u64 {
    enc_wl(opcode, 0, op1)
}

fn enc_www(opcode: u16, op1: u16, op2: u16, op3: u16) -> u64 {
    ((opcode as u64) << 48) + ((op1 as u64) << 32) + ((op2 as u64) << 16) + (op3 as u64)
}

fn enc_ww(opcode: u16, op1: u16, op2: u16) -> u64 {
    enc_www(opcode, op1, op2, 0)
}

fn enc_w(opcode: u16, op1: u16) -> u64 {
    enc_www(opcode, op1, 0, 0)
}

fn enc_wwsw(opcode: u16, op1: u16, op2: u16, op3: i16) -> u64 {
    enc_www(opcode, op1, op2, op3 as u16)
}

fn dec_wl(op: u64) -> (u16, u32) {
    ((op >> 32) as u16, op as u32)
}

fn dec_www(op: u64) -> (u16, u16, u16) {
    ((op >> 32) as u16, (op >> 16) as u16, op as u16)
}

impl BcOp {
    pub fn to_u64(&self) -> u64 {
        use BcOp::*;
        match self {
            FnCall(op1, op2) => enc_wl(1, *op1, op2.0),
            MethodDef(op1) => enc_l(2, op1.0),
            Br(op1) => enc_l(3, *op1 as u32),
            CondBr(op1, op2) => enc_wl(4, *op1, *op2 as u32),
            CondNotBr(op1, op2) => enc_wl(5, *op1, *op2 as u32),
            Integer(op1, op2) => enc_wl(6, *op1, *op2 as u32),
            Const(op1, op2) => enc_wl(7, *op1, *op2),
            Nil(op1) => enc_w(8, *op1),
            Neg(op1, op2) => enc_ww(129, *op1, *op2),
            Add(op1, op2, op3) => enc_www(130, *op1, *op2, *op3),
            Sub(op1, op2, op3) => enc_www(131, *op1, *op2, *op3),
            Mul(op1, op2, op3) => enc_www(132, *op1, *op2, *op3),
            Div(op1, op2, op3) => enc_www(133, *op1, *op2, *op3),
            Eq(op1, op2, op3) => enc_www(134, *op1, *op2, *op3),
            Ne(op1, op2, op3) => enc_www(135, *op1, *op2, *op3),
            Lt(op1, op2, op3) => enc_www(136, *op1, *op2, *op3),
            Le(op1, op2, op3) => enc_www(137, *op1, *op2, *op3),
            Gt(op1, op2, op3) => enc_www(138, *op1, *op2, *op3),
            Ge(op1, op2, op3) => enc_www(139, *op1, *op2, *op3),
            Addri(op1, op2, op3) => enc_wwsw(140, *op1, *op2, *op3),
            Subri(op1, op2, op3) => enc_wwsw(141, *op1, *op2, *op3),
            Eqri(op1, op2, op3) => enc_wwsw(142, *op1, *op2, *op3),
            Neri(op1, op2, op3) => enc_wwsw(143, *op1, *op2, *op3),
            Ltri(op1, op2, op3) => enc_wwsw(144, *op1, *op2, *op3),
            Leri(op1, op2, op3) => enc_wwsw(145, *op1, *op2, *op3),
            Gtri(op1, op2, op3) => enc_wwsw(146, *op1, *op2, *op3),
            Geri(op1, op2, op3) => enc_wwsw(147, *op1, *op2, *op3),
            Ret(op1) => enc_w(148, *op1),
            Mov(op1, op2) => enc_ww(149, *op1, *op2),
        }
    }

    pub fn from_u64(op: u64) -> Self {
        let opcode = (op >> 48) as u16;
        if opcode & 0x80 == 0 {
            let (op1, op2) = dec_wl(op);
            match opcode {
                1 => Self::FnCall(op1, CallsiteId(op2)),
                2 => Self::MethodDef(MethodDefId(op2)),
                3 => Self::Br(op2 as i32),
                4 => Self::CondBr(op1, op2 as i32),
                5 => Self::CondNotBr(op1, op2 as i32),
                6 => Self::Integer(op1, op2 as i32),
                7 => Self::Const(op1, op2),
                8 => Self::Nil(op1),
                _ => unreachable!(),
            }
        } else {
            let (op1, op2, op3) = dec_www(op);
            match opcode {
                129 => Self::Neg(op1, op2),
                130 => Self::Add(op1, op2, op3),
                131 => Self::Sub(op1, op2, op3),
                132 => Self::Mul(op1, op2, op3),
                133 => Self::Div(op1, op2, op3),
                134 => Self::Eq(op1, op2, op3),
                135 => Self::Ne(op1, op2, op3),
                136 => Self::Lt(op1, op2, op3),
                137 => Self::Le(op1, op2, op3),
                138 => Self::Gt(op1, op2, op3),
                139 => Self::Ge(op1, op2, op3),
                140 => Self::Addri(op1, op2, op3 as i16),
                141 => Self::Subri(op1, op2, op3 as i16),
                142 => Self::Eqri(op1, op2, op3 as i16),
                143 => Self::Neri(op1, op2, op3 as i16),
                144 => Self::Ltri(op1, op2, op3 as i16),
                145 => Self::Leri(op1, op2, op3 as i16),
                146 => Self::Gtri(op1, op2, op3 as i16),
                147 => Self::Geri(op1, op2, op3 as i16),
                148 => Self::Ret(op1),
                149 => Self::Mov(op1, op2),
                _ => unreachable!(),
            }
        }
    }
}