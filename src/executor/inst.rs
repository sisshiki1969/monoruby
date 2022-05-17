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
    FnCall(CallsiteId),
    MethodDef(MethodDefId),
}

fn enc_l(opcode: u16, op1: u32) -> u64 {
    ((opcode as u64) << 48) + op1 as u64
}

fn dec_l(op: u64) -> u32 {
    op as u32
}

fn enc_www(opcode: u16, op1: &u16, op2: &u16, op3: &u16) -> u64 {
    ((opcode as u64) << 48) + ((*op1 as u64) << 32) + ((*op2 as u64) << 16) + (*op3 as u64)
}

fn dec_www(op: u64) -> (u16, u16, u16) {
    ((op >> 32) as u16, (op >> 16) as u16, op as u16)
}

fn enc_wl(opcode: u16, op1: &u16, op2: &u32) -> u64 {
    ((opcode as u64) << 48) + ((*op1 as u64) << 32) + (*op2 as u64)
}

fn dec_wl(op: u64) -> (u16, u32) {
    ((op >> 32) as u16, op as u32)
}

fn enc_ww(opcode: u16, op1: &u16, op2: &u16) -> u64 {
    enc_www(opcode, op1, op2, &0)
}

fn enc_w(opcode: u16, op1: &u16) -> u64 {
    enc_www(opcode, op1, &0, &0)
}

fn enc_wwsw(opcode: u16, op1: &u16, op2: &u16, op3: &i16) -> u64 {
    enc_www(opcode, op1, op2, &(*op3 as u16))
}

impl BcOp {
    pub fn to_u64(&self) -> u64 {
        use BcOp::*;
        match self {
            Br(op1) => enc_l(1, op1.0),
            CondBr(op1, op2) => enc_wl(2, op1, &op2.0),
            CondNotBr(op1, op2) => enc_wl(3, op1, &op2.0),
            Integer(op1, op2) => enc_wl(4, op1, &(*op2 as u32)),
            Const(op1, op2) => enc_wl(5, op1, op2),
            Nil(op1) => enc_w(6, op1),
            Neg(op1, op2) => enc_ww(7, op1, op2),
            Add(op1, op2, op3) => enc_www(8, op1, op2, op3),
            Sub(op1, op2, op3) => enc_www(9, op1, op2, op3),
            Mul(op1, op2, op3) => enc_www(11, op1, op2, op3),
            Div(op1, op2, op3) => enc_www(12, op1, op2, op3),
            Eq(op1, op2, op3) => enc_www(13, op1, op2, op3),
            Ne(op1, op2, op3) => enc_www(14, op1, op2, op3),
            Lt(op1, op2, op3) => enc_www(15, op1, op2, op3),
            Le(op1, op2, op3) => enc_www(16, op1, op2, op3),
            Gt(op1, op2, op3) => enc_www(17, op1, op2, op3),
            Ge(op1, op2, op3) => enc_www(18, op1, op2, op3),
            Addri(op1, op2, op3) => enc_wwsw(19, op1, op2, op3),
            Subri(op1, op2, op3) => enc_wwsw(20, op1, op2, op3),
            Eqri(op1, op2, op3) => enc_wwsw(21, op1, op2, op3),
            Neri(op1, op2, op3) => enc_wwsw(22, op1, op2, op3),
            Ltri(op1, op2, op3) => enc_wwsw(23, op1, op2, op3),
            Leri(op1, op2, op3) => enc_wwsw(24, op1, op2, op3),
            Gtri(op1, op2, op3) => enc_wwsw(25, op1, op2, op3),
            Geri(op1, op2, op3) => enc_wwsw(26, op1, op2, op3),
            Ret(op1) => enc_w(27, op1),
            Mov(op1, op2) => enc_ww(28, op1, op2),
            FnCall(op1) => enc_l(29, op1.0),
            MethodDef(op1) => enc_l(30, op1.0),
        }
    }

    pub fn from_u64(op: u64) -> Self {
        match (op >> 48) as u16 {
            1 => {
                let op1 = dec_l(op);
                Self::Br(InstId(op1))
            }
            2 => {
                let (op1, op2) = dec_wl(op);
                Self::CondBr(op1, InstId(op2))
            }
            3 => {
                let (op1, op2) = dec_wl(op);
                Self::CondNotBr(op1, InstId(op2))
            }
            4 => {
                let (op1, op2) = dec_wl(op);
                Self::Integer(op1, op2 as i32)
            }
            5 => {
                let (op1, op2) = dec_wl(op);
                Self::Const(op1, op2)
            }
            6 => {
                let (op1, _, _) = dec_www(op);
                Self::Nil(op1)
            }
            7 => {
                let (op1, op2, _) = dec_www(op);
                Self::Neg(op1, op2)
            }
            8 => {
                let (op1, op2, op3) = dec_www(op);
                Self::Add(op1, op2, op3)
            }
            9 => {
                let (op1, op2, op3) = dec_www(op);
                Self::Sub(op1, op2, op3)
            }
            11 => {
                let (op1, op2, op3) = dec_www(op);
                Self::Mul(op1, op2, op3)
            }
            12 => {
                let (op1, op2, op3) = dec_www(op);
                Self::Div(op1, op2, op3)
            }
            13 => {
                let (op1, op2, op3) = dec_www(op);
                Self::Eq(op1, op2, op3)
            }
            14 => {
                let (op1, op2, op3) = dec_www(op);
                Self::Ne(op1, op2, op3)
            }
            15 => {
                let (op1, op2, op3) = dec_www(op);
                Self::Lt(op1, op2, op3)
            }
            16 => {
                let (op1, op2, op3) = dec_www(op);
                Self::Le(op1, op2, op3)
            }
            17 => {
                let (op1, op2, op3) = dec_www(op);
                Self::Gt(op1, op2, op3)
            }
            18 => {
                let (op1, op2, op3) = dec_www(op);
                Self::Ge(op1, op2, op3)
            }
            19 => {
                let (op1, op2, op3) = dec_www(op);
                Self::Addri(op1, op2, op3 as i16)
            }
            20 => {
                let (op1, op2, op3) = dec_www(op);
                Self::Subri(op1, op2, op3 as i16)
            }
            21 => {
                let (op1, op2, op3) = dec_www(op);
                Self::Eqri(op1, op2, op3 as i16)
            }
            22 => {
                let (op1, op2, op3) = dec_www(op);
                Self::Neri(op1, op2, op3 as i16)
            }
            23 => {
                let (op1, op2, op3) = dec_www(op);
                Self::Ltri(op1, op2, op3 as i16)
            }
            24 => {
                let (op1, op2, op3) = dec_www(op);
                Self::Leri(op1, op2, op3 as i16)
            }
            25 => {
                let (op1, op2, op3) = dec_www(op);
                Self::Gtri(op1, op2, op3 as i16)
            }
            26 => {
                let (op1, op2, op3) = dec_www(op);
                Self::Geri(op1, op2, op3 as i16)
            }
            27 => {
                let (op1, _, _) = dec_www(op);
                Self::Ret(op1)
            }
            28 => {
                let (op1, op2, _) = dec_www(op);
                Self::Mov(op1, op2)
            }
            29 => {
                let op1 = dec_l(op);
                Self::FnCall(CallsiteId(op1))
            }
            30 => {
                let op1 = dec_l(op);
                Self::MethodDef(MethodDefId(op1))
            }
            _ => unreachable!(),
        }
    }
}
