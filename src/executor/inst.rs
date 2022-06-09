use super::*;

///
/// bytecode Ir.
///
#[derive(Debug, Clone, PartialEq)]
pub(super) enum BcIr {
    Br(usize),
    CondBr(BcReg, usize),
    CondNotBr(BcReg, usize),
    Integer(BcReg, i32),
    Symbol(BcReg, IdentId),
    Literal(BcReg, u32),
    Nil(BcReg),
    Neg(BcReg, BcReg),                 // ret, src
    Add(BcReg, BcReg, BcReg),          // ret, lhs, rhs
    Addri(BcReg, BcReg, i16),          // ret, lhs, int
    Sub(BcReg, BcReg, BcReg),          // ret, lhs, rhs
    Subri(BcReg, BcReg, i16),          // ret, lhs, int
    Mul(BcReg, BcReg, BcReg),          // ret, lhs, rhs
    Div(BcReg, BcReg, BcReg),          // ret, lhs, rhs
    BitOr(BcReg, BcReg, BcReg),        // ret, lhs, rhs
    BitAnd(BcReg, BcReg, BcReg),       // ret, lhs, rhs
    BitXor(BcReg, BcReg, BcReg),       // ret, lhs, rhs
    Shr(BcReg, BcReg, BcReg),          // ret, lhs, rhs
    Shl(BcReg, BcReg, BcReg),          // ret, lhs, rhs
    Cmp(CmpKind, BcReg, BcReg, BcReg), // kind, dst, lhs, rhs
    Cmpri(CmpKind, BcReg, BcReg, i16), // kind, dst, lhs, rhs
    Ret(BcReg),
    Mov(BcReg, BcReg),                                        // dst, offset
    MethodCall(BcReg, IdentId, Option<BcReg>, BcTemp, usize), // (recv, id, ret, args, args_len)
    MethodDef(IdentId, FuncId),
    ConcatStr(Option<BcReg>, BcTemp, usize), // (ret, args, args_len)
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
    /// Symbol(%reg, IdentId)
    Symbol(u16, IdentId),
    /// literal(%ret, literal_id)
    Literal(u16, u32),
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
    /// bor(%ret, %lhs, %rhs)
    BitOr(u16, u16, u16),
    /// band(%ret, %lhs, %rhs)
    BitAnd(u16, u16, u16),
    /// bxor(%ret, %lhs, %rhs)
    BitXor(u16, u16, u16),
    /// shr(%ret, %lhs, %rhs)
    Shr(u16, u16, u16),
    /// shl(%ret, %lhs, %rhs)
    Shl(u16, u16, u16),
    /// cmp(%ret, %lhs, %rhs)
    Cmp(CmpKind, u16, u16, u16),
    /// cmpri(%ret, %lhs, rhs: i16)
    Cmpri(CmpKind, u16, u16, i16),
    /// return(%ret)
    Ret(u16),
    /// move(%dst, %src)
    Mov(u16, u16),
    /// func call(%recv, callsite_id)
    MethodCall(u16, CallsiteId),
    /// method definition(method_def_id)
    MethodDef(MethodDefId),
    /// concatenate strings(ret, args, args_len)
    ConcatStr(u16, u16, u16),
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
            MethodCall(op1, op2) => enc_wl(1, *op1, op2.0),
            MethodDef(op1) => enc_l(2, op1.0),
            Br(op1) => enc_l(3, *op1 as u32),
            CondBr(op1, op2) => enc_wl(4, *op1, *op2 as u32),
            CondNotBr(op1, op2) => enc_wl(5, *op1, *op2 as u32),
            Integer(op1, op2) => enc_wl(6, *op1, *op2 as u32),
            Literal(op1, op2) => enc_wl(7, *op1, *op2),
            Nil(op1) => enc_w(8, *op1),
            Symbol(op1, op2) => enc_wl(9, *op1, op2.get()),

            Neg(op1, op2) => enc_ww(129, *op1, *op2),
            Add(op1, op2, op3) => enc_www(130, *op1, *op2, *op3),
            Sub(op1, op2, op3) => enc_www(131, *op1, *op2, *op3),
            Mul(op1, op2, op3) => enc_www(132, *op1, *op2, *op3),
            Div(op1, op2, op3) => enc_www(133, *op1, *op2, *op3),
            Cmp(kind, op1, op2, op3) => enc_www(134 + *kind as u16, *op1, *op2, *op3),
            Addri(op1, op2, op3) => enc_wwsw(140, *op1, *op2, *op3),
            Subri(op1, op2, op3) => enc_wwsw(141, *op1, *op2, *op3),
            Cmpri(kind, op1, op2, op3) => enc_wwsw(142 + *kind as u16, *op1, *op2, *op3),
            Ret(op1) => enc_w(148, *op1),
            Mov(op1, op2) => enc_ww(149, *op1, *op2),
            BitOr(op1, op2, op3) => enc_www(150, *op1, *op2, *op3),
            BitAnd(op1, op2, op3) => enc_www(151, *op1, *op2, *op3),
            BitXor(op1, op2, op3) => enc_www(152, *op1, *op2, *op3),
            Shr(op1, op2, op3) => enc_www(153, *op1, *op2, *op3),
            Shl(op1, op2, op3) => enc_www(154, *op1, *op2, *op3),
            ConcatStr(op1, op2, op3) => enc_www(155, *op1, *op2, *op3),
        }
    }

    pub fn from_u64(op: u64) -> Self {
        let opcode = (op >> 48) as u16;
        if opcode & 0x80 == 0 {
            let (op1, op2) = dec_wl(op);
            match opcode {
                1 => Self::MethodCall(op1, CallsiteId(op2)),
                2 => Self::MethodDef(MethodDefId(op2)),
                3 => Self::Br(op2 as i32),
                4 => Self::CondBr(op1, op2 as i32),
                5 => Self::CondNotBr(op1, op2 as i32),
                6 => Self::Integer(op1, op2 as i32),
                7 => Self::Literal(op1, op2),
                8 => Self::Nil(op1),
                9 => Self::Symbol(op1, IdentId::from(op2)),
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
                134 => Self::Cmp(CmpKind::Eq, op1, op2, op3),
                135 => Self::Cmp(CmpKind::Ne, op1, op2, op3),
                136 => Self::Cmp(CmpKind::Lt, op1, op2, op3),
                137 => Self::Cmp(CmpKind::Le, op1, op2, op3),
                138 => Self::Cmp(CmpKind::Gt, op1, op2, op3),
                139 => Self::Cmp(CmpKind::Ge, op1, op2, op3),
                140 => Self::Addri(op1, op2, op3 as i16),
                141 => Self::Subri(op1, op2, op3 as i16),
                142 => Self::Cmpri(CmpKind::Eq, op1, op2, op3 as i16),
                143 => Self::Cmpri(CmpKind::Ne, op1, op2, op3 as i16),
                144 => Self::Cmpri(CmpKind::Lt, op1, op2, op3 as i16),
                145 => Self::Cmpri(CmpKind::Le, op1, op2, op3 as i16),
                146 => Self::Cmpri(CmpKind::Gt, op1, op2, op3 as i16),
                147 => Self::Cmpri(CmpKind::Ge, op1, op2, op3 as i16),
                148 => Self::Ret(op1),
                149 => Self::Mov(op1, op2),
                150 => Self::BitOr(op1, op2, op3),
                151 => Self::BitAnd(op1, op2, op3),
                152 => Self::BitXor(op1, op2, op3),
                153 => Self::Shr(op1, op2, op3),
                154 => Self::Shl(op1, op2, op3),
                155 => Self::ConcatStr(op1, op2, op3),
                _ => unreachable!(),
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub(super) enum CmpKind {
    Eq = 0,
    Ne = 1,
    Lt = 2,
    Le = 3,
    Gt = 4,
    Ge = 5,
}

impl std::fmt::Debug for CmpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eq => write!(f, "=="),
            Self::Ne => write!(f, "!="),
            Self::Ge => write!(f, ">="),
            Self::Gt => write!(f, ">"),
            Self::Le => write!(f, "<="),
            Self::Lt => write!(f, "<"),
        }
    }
}
