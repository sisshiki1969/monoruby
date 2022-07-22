use super::*;

///
/// kinds of binary operation.
///
#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) enum BinOpK {
    Add = 0,
    Sub = 1,
    Mul = 2,
    Div = 3,
    BitOr = 4,
    BitAnd = 5,
    BitXor = 6,
    Shr = 7,
    Shl = 8,
}

use std::fmt;
impl fmt::Display for BinOpK {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match *self {
            BinOpK::Add => "+",
            BinOpK::Sub => "-",
            BinOpK::Mul => "*",
            BinOpK::Div => "/",
            BinOpK::BitOr => "|",
            BinOpK::BitAnd => "&",
            BinOpK::BitXor => "^",
            BinOpK::Shr => ">>",
            BinOpK::Shl => "<<",
        };
        write!(f, "{}", s)
    }
}

///
/// bytecode Ir.
///
#[derive(Debug, Clone, PartialEq)]
pub(super) enum BcIr {
    Br(usize),
    CondBr(BcReg, usize, bool),
    CondNotBr(BcReg, usize, bool),
    Integer(BcReg, i32),
    Symbol(BcReg, IdentId),
    Literal(BcReg, u32),
    LoadConst(BcReg, IdentId),
    StoreConst(BcReg, IdentId),
    Nil(BcReg),
    Neg(BcReg, BcReg),                       // ret, src
    BinOp(BinOpK, BcReg, BcReg, BcReg),      // kind, ret, lhs, rhs
    BinOpRi(BinOpK, BcReg, BcReg, i16),      // kind, ret, lhs, rhs
    Cmp(CmpKind, BcReg, BcReg, BcReg, bool), // kind, dst, lhs, rhs, optimizable
    Cmpri(CmpKind, BcReg, BcReg, i16, bool), // kind, dst, lhs, rhs, optimizable
    Ret(BcReg),
    Mov(BcReg, BcReg),                  // dst, offset
    MethodCall(Option<BcReg>, IdentId), // (ret, id)
    MethodArgs(BcReg, BcTemp, usize),   // (recv, args, args_len)
    InlineCache,
    MethodDef(IdentId, FuncId),
    ConcatStr(Option<BcReg>, BcTemp, usize), // (ret, args, args_len)
    LoopStart,
    LoopEnd,
}

#[derive(Clone, Copy, PartialEq)]
#[repr(C)]
pub(crate) struct Bc {
    pub(crate) op1: Bc1,
    pub(crate) op2: Bc2,
}

impl Bc {
    pub(crate) fn from(op1: Bc1, op2: Bc2) -> Self {
        Self { op1, op2 }
    }

    pub(crate) fn from_with_class_and_version(op1: u64, class_id: ClassId, version: u32) -> Self {
        Self {
            op1: Bc1::from(op1),
            op2: Bc2::class_and_version(class_id, version),
        }
    }
}

impl std::fmt::Debug for Bc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        fn optstr(opt: bool) -> &'static str {
            if opt {
                "_"
            } else {
                ""
            }
        }
        fn disp_str(disp: i32) -> String {
            if disp >= 0 {
                format!("+{:05}", disp + 1)
            } else {
                format!("{:05}", disp + 1)
            }
        }
        let bcop1 = BcOp1::from_bc(*self);
        match bcop1 {
            BcOp1::Br(disp) => {
                writeln!(f, "br => {}", disp_str(disp))
            }
            BcOp1::CondBr(reg, disp, opt) => {
                writeln!(f, "condbr {}%{} => {}", optstr(opt), reg, disp_str(disp))
            }
            BcOp1::CondNotBr(reg, disp, opt) => {
                writeln!(f, "condnbr {}%{} => {}", optstr(opt), reg, disp_str(disp))
            }
            BcOp1::Integer(reg, num) => writeln!(f, "%{} = {}: i32", reg, num),
            BcOp1::Symbol(reg, id) => {
                writeln!(f, "%{} = {:?}", reg, id)
            }
            BcOp1::Literal(reg, id) => {
                writeln!(f, "%{} = literal[#{}]", reg, id)
            }
            BcOp1::LoadConst(reg, id) => {
                writeln!(f, "%{} = const[{:?}]", reg, id)
            }
            BcOp1::StoreConst(reg, id) => {
                writeln!(f, "const[{:?}] = %{}", id, reg)
            }
            BcOp1::Nil(reg) => writeln!(f, "%{} = nil", reg),
            BcOp1::Neg(dst, src) => writeln!(f, "%{} = neg %{}", dst, src),
            BcOp1::BinOp(kind, dst, lhs, rhs) => {
                let class_id = Bc2::from_bc_classid(*self);
                let op1 = format!("%{} = %{} {} %{}", dst, lhs, kind, rhs);
                writeln!(f, "{:36} {:?}", op1, class_id)
            }
            BcOp1::BinOpRi(kind, dst, lhs, rhs) => {
                let class_id = Bc2::from_bc_classid(*self);
                let op1 = format!("%{} = %{} {} {}: i16", dst, lhs, kind, rhs,);
                writeln!(f, "{:36} {:?}", op1, class_id)
            }
            BcOp1::Cmp(kind, dst, lhs, rhs, opt) => {
                let class_id = Bc2::from_bc_classid(*self);
                let op1 = format!("{}%{} = %{} {:?} %{}", optstr(opt), dst, lhs, kind, rhs,);
                writeln!(f, "{:36} {:?}", op1, class_id)
            }
            BcOp1::Cmpri(kind, dst, lhs, rhs, opt) => {
                let class_id = Bc2::from_bc_classid(*self);
                let op1 = format!("{}%{} = %{} {:?} {}: i16", optstr(opt), dst, lhs, kind, rhs,);
                writeln!(f, "{:36} {:?}", op1, class_id)
            }

            BcOp1::Ret(reg) => writeln!(f, "ret %{}", reg),
            BcOp1::Mov(dst, src) => writeln!(f, "%{} = %{}", dst, src),
            BcOp1::MethodCall(ret, name) => {
                let class_id = Bc2::from_bc_classid(*self);
                let op1 = format!(
                    "{} = call {:?}",
                    match ret {
                        0 => "_".to_string(),
                        ret => format!("%{:?}", ret),
                    },
                    name,
                );
                writeln!(f, "{:36} {:?}", op1, class_id)
            }
            BcOp1::MethodArgs(recv, args, len) => {
                writeln!(f, "%{}.call_args (%{}; {})", recv, args, len)
            }
            BcOp1::MethodDef(id) => {
                writeln!(f, "define {:?}", id)
            }
            BcOp1::ConcatStr(ret, args, len) => match ret {
                0 => writeln!(f, "_ = concat(%{}; {})", args, len),
                ret => writeln!(f, "%{:?} = concat(%{}; {})", ret, args, len),
            },
            BcOp1::LoopStart(count) => writeln!(
                f,
                "loop_start counter={} jit-addr={:016x}",
                count,
                Bc2::from_jit_addr(*self)
            ),
            BcOp1::LoopEnd => writeln!(f, "loop_end"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(transparent)]
pub(crate) struct Bc1(u64);

impl Bc1 {
    pub(crate) fn from(op: u64) -> Self {
        Self(op)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(transparent)]
pub(crate) struct Bc2(u64);

impl Bc2 {
    pub(crate) fn from(op: u64) -> Self {
        Self(op)
    }

    fn class_and_version(class_id: ClassId, version: u32) -> Self {
        let id: u32 = class_id.into();
        Self(((version as u64) << 32) + (id as u64))
    }

    pub(crate) fn from_bc_classid(bcop: Bc) -> ClassId {
        ClassId::new(bcop.op2.0 as u32)
    }

    //#[cfg(feature = "emit-bc")]
    pub(crate) fn from_jit_addr(bcop: Bc) -> u64 {
        bcop.op2.0
    }
}

///
/// Bytecode instructions.
///
#[derive(Debug, Clone, PartialEq)]
pub(super) enum BcOp1 {
    /// branch(dest)
    Br(i32),
    /// conditional branch(%reg, dest, optimizable)  : branch when reg was true.
    CondBr(u16, i32, bool),
    /// conditional branch(%reg, dest, optimizable)  : branch when reg was false.
    CondNotBr(u16, i32, bool),
    /// integer(%reg, i32)
    Integer(u16, i32),
    /// Symbol(%reg, IdentId)
    Symbol(u16, IdentId),
    /// literal(%ret, literal_id)
    Literal(u16, u32),
    LoadConst(u16, ConstSiteId),
    StoreConst(u16, IdentId),
    /// nil(%reg)
    Nil(u16),
    /// negate(%ret, %src)
    Neg(u16, u16),
    /// binop(kind, %ret, %lhs, %rhs)
    BinOp(BinOpK, u16, u16, u16),
    /// binop with small integer(kind, %ret, %lhs, %rhs)
    BinOpRi(BinOpK, u16, u16, i16),
    /// cmp(%ret, %lhs, %rhs, optimizable)
    Cmp(CmpKind, u16, u16, u16, bool),
    /// cmpri(%ret, %lhs, rhs: i16, optimizable)
    Cmpri(CmpKind, u16, u16, i16, bool),
    /// return(%ret)
    Ret(u16),
    /// move(%dst, %src)
    Mov(u16, u16),
    /// func call(%ret, name)
    MethodCall(u16, IdentId),
    /// func call 2nd opecode(%recv, %args, %len)
    MethodArgs(u16, u16, u16),
    /// method definition(method_def_id)
    MethodDef(MethodDefId),
    /// concatenate strings(ret, args, args_len)
    ConcatStr(u16, u16, u16),
    /// loop start marker
    LoopStart(u32),
    LoopEnd,
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

impl BcOp1 {
    pub fn to_bc(&self) -> Bc {
        use BcOp1::*;
        let op = match self {
            MethodCall(op1, op2) => {
                return Bc::from_with_class_and_version(
                    enc_wl(1, *op1, op2.get()),
                    ClassId::new(0),
                    -1i32 as u32,
                )
            }
            MethodArgs(op1, op2, op3) => enc_www(130, *op1, *op2, *op3),
            MethodDef(op1) => enc_l(2, op1.0),
            Br(op1) => enc_l(3, *op1 as u32),
            CondBr(op1, op2, opt) => enc_wl(if *opt { 12 } else { 4 }, *op1, *op2 as u32),
            CondNotBr(op1, op2, opt) => enc_wl(if *opt { 13 } else { 5 }, *op1, *op2 as u32),
            Integer(op1, op2) => enc_wl(6, *op1, *op2 as u32),
            Literal(op1, op2) => enc_wl(7, *op1, *op2),
            Nil(op1) => enc_w(8, *op1),
            Symbol(op1, op2) => enc_wl(9, *op1, op2.get()),
            LoadConst(op1, op2) => enc_wl(10, *op1, op2.get()),
            StoreConst(op1, op2) => enc_wl(11, *op1, op2.get()),
            LoopStart(_) => enc_l(14, 0),
            LoopEnd => enc_l(15, 0),

            Neg(op1, op2) => enc_ww(129, *op1, *op2),
            BinOp(kind, op1, op2, op3) => {
                return Bc::from_with_class_and_version(
                    enc_www(170 + *kind as u16, *op1, *op2, *op3),
                    INTEGER_CLASS,
                    -1i32 as u32,
                )
            }
            Cmp(kind, op1, op2, op3, opt) => {
                let op1 = if *opt {
                    enc_www(156 + *kind as u16, *op1, *op2, *op3)
                } else {
                    enc_www(134 + *kind as u16, *op1, *op2, *op3)
                };
                return Bc::from_with_class_and_version(op1, INTEGER_CLASS, -1i32 as u32);
            }
            BinOpRi(BinOpK::Add, op1, op2, op3) => {
                let op1 = enc_wwsw(140, *op1, *op2, *op3);
                return Bc::from_with_class_and_version(op1, INTEGER_CLASS, -1i32 as u32);
            }
            BinOpRi(BinOpK::Sub, op1, op2, op3) => {
                let op1 = enc_wwsw(141, *op1, *op2, *op3);
                return Bc::from_with_class_and_version(op1, INTEGER_CLASS, -1i32 as u32);
            }
            BinOpRi(..) => unimplemented!(),
            Cmpri(kind, op1, op2, op3, opt) => {
                let op1 = if *opt {
                    enc_wwsw(162 + *kind as u16, *op1, *op2, *op3)
                } else {
                    enc_wwsw(142 + *kind as u16, *op1, *op2, *op3)
                };
                return Bc::from_with_class_and_version(op1, INTEGER_CLASS, -1i32 as u32);
            }
            Ret(op1) => enc_w(148, *op1),
            Mov(op1, op2) => enc_ww(149, *op1, *op2),
            ConcatStr(op1, op2, op3) => enc_www(155, *op1, *op2, *op3),
        };
        Bc::from(Bc1::from(op), Bc2::from(0))
    }

    pub fn from_bc(bcop: Bc) -> Self {
        let op = bcop.op1.0;
        let opcode = (op >> 48) as u16;
        if opcode & 0x80 == 0 {
            let (op1, op2) = dec_wl(op);
            match opcode {
                1 => Self::MethodCall(op1, IdentId::from(op2)),
                2 => Self::MethodDef(MethodDefId(op2)),
                3 => Self::Br(op2 as i32),
                4 => Self::CondBr(op1, op2 as i32, false),
                5 => Self::CondNotBr(op1, op2 as i32, false),
                6 => Self::Integer(op1, op2 as i32),
                7 => Self::Literal(op1, op2),
                8 => Self::Nil(op1),
                9 => Self::Symbol(op1, IdentId::from(op2)),
                10 => Self::LoadConst(op1, ConstSiteId(op2)),
                11 => Self::StoreConst(op1, IdentId::from(op2)),
                12 => Self::CondBr(op1, op2 as i32, true),
                13 => Self::CondNotBr(op1, op2 as i32, true),
                14 => Self::LoopStart(op2),
                15 => Self::LoopEnd,
                _ => unreachable!(),
            }
        } else {
            let (op1, op2, op3) = dec_www(op);
            match opcode {
                129 => Self::Neg(op1, op2),
                130 => Self::MethodArgs(op1, op2, op3),
                134 => Self::Cmp(CmpKind::Eq, op1, op2, op3, false),
                135 => Self::Cmp(CmpKind::Ne, op1, op2, op3, false),
                136 => Self::Cmp(CmpKind::Lt, op1, op2, op3, false),
                137 => Self::Cmp(CmpKind::Le, op1, op2, op3, false),
                138 => Self::Cmp(CmpKind::Gt, op1, op2, op3, false),
                139 => Self::Cmp(CmpKind::Ge, op1, op2, op3, false),
                140 => Self::BinOpRi(BinOpK::Add, op1, op2, op3 as i16),
                141 => Self::BinOpRi(BinOpK::Sub, op1, op2, op3 as i16),
                142 => Self::Cmpri(CmpKind::Eq, op1, op2, op3 as i16, false),
                143 => Self::Cmpri(CmpKind::Ne, op1, op2, op3 as i16, false),
                144 => Self::Cmpri(CmpKind::Lt, op1, op2, op3 as i16, false),
                145 => Self::Cmpri(CmpKind::Le, op1, op2, op3 as i16, false),
                146 => Self::Cmpri(CmpKind::Gt, op1, op2, op3 as i16, false),
                147 => Self::Cmpri(CmpKind::Ge, op1, op2, op3 as i16, false),
                148 => Self::Ret(op1),
                149 => Self::Mov(op1, op2),
                155 => Self::ConcatStr(op1, op2, op3),
                156 => Self::Cmp(CmpKind::Eq, op1, op2, op3, true),
                157 => Self::Cmp(CmpKind::Ne, op1, op2, op3, true),
                158 => Self::Cmp(CmpKind::Lt, op1, op2, op3, true),
                159 => Self::Cmp(CmpKind::Le, op1, op2, op3, true),
                160 => Self::Cmp(CmpKind::Gt, op1, op2, op3, true),
                161 => Self::Cmp(CmpKind::Ge, op1, op2, op3, true),
                162 => Self::Cmpri(CmpKind::Eq, op1, op2, op3 as i16, true),
                163 => Self::Cmpri(CmpKind::Ne, op1, op2, op3 as i16, true),
                164 => Self::Cmpri(CmpKind::Lt, op1, op2, op3 as i16, true),
                165 => Self::Cmpri(CmpKind::Le, op1, op2, op3 as i16, true),
                166 => Self::Cmpri(CmpKind::Gt, op1, op2, op3 as i16, true),
                167 => Self::Cmpri(CmpKind::Ge, op1, op2, op3 as i16, true),
                170 => Self::BinOp(BinOpK::Add, op1, op2, op3),
                171 => Self::BinOp(BinOpK::Sub, op1, op2, op3),
                172 => Self::BinOp(BinOpK::Mul, op1, op2, op3),
                173 => Self::BinOp(BinOpK::Div, op1, op2, op3),
                174 => Self::BinOp(BinOpK::BitOr, op1, op2, op3),
                175 => Self::BinOp(BinOpK::BitAnd, op1, op2, op3),
                176 => Self::BinOp(BinOpK::BitXor, op1, op2, op3),
                177 => Self::BinOp(BinOpK::Shr, op1, op2, op3),
                178 => Self::BinOp(BinOpK::Shl, op1, op2, op3),
                _ => unreachable!(),
            }
        }
    }
}
