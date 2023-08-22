use super::*;
use ruruby_parse::CmpKind;

#[derive(Clone, Copy, PartialEq)]
#[repr(C)]
pub(crate) struct Bc {
    pub op1: u64,
    pub op2: Bc2,
}

impl Bc {
    pub fn is_integer1(&self) -> bool {
        self.classid1() == INTEGER_CLASS
    }

    pub fn is_integer2(&self) -> bool {
        self.classid2() == INTEGER_CLASS
    }

    pub fn is_float1(&self) -> bool {
        self.classid1() == FLOAT_CLASS
    }

    pub fn is_float2(&self) -> bool {
        self.classid2() == FLOAT_CLASS
    }

    pub fn is_integer_binop(&self) -> bool {
        self.classid1() == INTEGER_CLASS && self.classid2() == INTEGER_CLASS
    }

    pub fn is_float_binop(&self) -> bool {
        match (self.classid1(), self.classid2()) {
            (INTEGER_CLASS, INTEGER_CLASS) => false,
            (INTEGER_CLASS | FLOAT_CLASS, INTEGER_CLASS | FLOAT_CLASS) => true,
            _ => false,
        }
    }
    pub fn classid1(&self) -> ClassId {
        ClassId::new(self.op2.0 as u32)
    }

    pub fn classid2(&self) -> ClassId {
        ClassId::new((self.op2.0 >> 32) as u32)
    }

    pub fn class_version(&self) -> (ClassId, u32) {
        let op = self.op2.0;
        (ClassId::new(op as u32), (op >> 32) as u32)
    }

    pub fn value(&self) -> Option<Value> {
        match self.op2.0 {
            0 => None,
            v => Some(Value::from(v)),
        }
    }

    #[cfg(any(feature = "emit-bc", feature = "emit-asm", feature = "log-jit",))]
    pub fn into_jit_addr(self) -> u64 {
        self.op2.0
    }

    pub(super) fn from(op1: u64) -> Self {
        Self {
            op1,
            op2: Bc2::from(0),
        }
    }

    pub(super) fn from_u32(op1: u64, op2: u32) -> Self {
        Self {
            op1,
            op2: Bc2::from(op2 as u64),
        }
    }

    pub(super) fn from_fn_info(op1: u64, fn_info: &FnInitInfo) -> Self {
        let FnInitInfo {
            arg_num,
            block_pos,
            req_num,
            info,
            ..
        } = fn_info;
        Bc::from_with_num(
            op1,
            *req_num as u16,
            *block_pos as u16,
            *info as u16,
            *arg_num as u16,
        )
    }

    pub(super) fn from_with_callid(op1: u64, callid: CallSiteId) -> Self {
        Self {
            op1,
            op2: Bc2::from(callid.get() as u64),
        }
    }

    pub(super) fn from_with_value(op1: u64, val: Value) -> Self {
        Self {
            op1,
            op2: Bc2::from(val.get()),
        }
    }

    pub(super) fn from_with_func_name_id(op1: u64, name: Option<IdentId>, func_id: FuncId) -> Self {
        Self {
            op1,
            op2: Bc2::from(
                ((func_id.get() as u64) << 32)
                    + (if let Some(name) = name {
                        name.get() as u64
                    } else {
                        0
                    }),
            ),
        }
    }

    pub(super) fn from_with_class_and_version(op1: u64, class_id: ClassId, version: u32) -> Self {
        Self {
            op1,
            op2: Bc2::class_and_version(class_id, version),
        }
    }

    pub(super) fn from_with_class2(op1: u64) -> Self {
        Self {
            op1,
            op2: Bc2::class2(ClassId::default(), ClassId::default()),
        }
    }

    fn from_with_num(op1: u64, num0: u16, num1: u16, num2: u16, num3: u16) -> Self {
        Self {
            op1,
            op2: Bc2::from(
                ((num3 as u64) << 48)
                    + ((num2 as u64) << 32)
                    + ((num1 as u64) << 16)
                    + (num0 as u64),
            ),
        }
    }

    pub fn u16(&self, id: usize) -> u16 {
        (self.op2.0 >> (id * 16)) as u16
    }

    pub fn func_data(&self) -> Option<&'static FuncData> {
        let op = self.op2.0;
        if op == 0 {
            None
        } else {
            Some(unsafe { &*(op as *const FuncData) })
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct MethodInfo {
    pub func_data: Option<&'static FuncData>,
}

impl MethodInfo {
    pub fn new(func_data: Option<&'static FuncData>) -> Self {
        MethodInfo { func_data }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum BrKind {
    BrIf = 0,
    BrIfNot = 1,
}

impl BrKind {
    pub fn from(i: u16) -> Self {
        match i {
            0 => Self::BrIf,
            1 => Self::BrIfNot,
            _ => unreachable!(),
        }
    }

    #[cfg(any(feature = "emit-bc", feature = "emit-asm", feature = "log-jit",))]
    pub(crate) fn to_s(self) -> &'static str {
        match self {
            Self::BrIf => "",
            Self::BrIfNot => "not",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) enum BinopMode {
    RR(BcReg, BcReg),
    RI(BcReg, i16),
    IR(i16, BcReg),
}

///
/// bytecode Ir.
///
#[derive(Debug, Clone, PartialEq)]
pub(super) enum BcIr {
    Nil(BcReg),
    Integer(BcReg, i32),
    Symbol(BcReg, IdentId),
    Literal(BcReg, Value),
    Array(BcReg, CallSiteId),
    Hash {
        ret: BcReg,
        args: BcReg,
        len: u16,
    },
    Range {
        ret: BcReg,
        start: BcReg,
        end: BcReg,
        exclude_end: bool,
    },
    Index(BcReg, BcReg, BcReg),      // ret, base, index
    StoreIndex(BcReg, BcReg, BcReg), // src, base, index
    LoadConst {
        ret: BcReg,
        toplevel: bool,
        prefix: Vec<IdentId>,
        name: IdentId,
    },
    StoreConst(BcReg, IdentId),
    LoadGvar {
        ret: BcReg,
        name: IdentId,
    },
    StoreGvar {
        val: BcReg,
        name: IdentId,
    },
    LoadSvar {
        ret: BcReg,
        id: u32,
    },
    BlockArgProxy(BcReg, usize),
    BlockArg(BcReg, usize),
    LoadDynVar {
        /// return register of the current frame.
        ret: BcReg,
        /// source register of the outer frame.
        src: BcReg,
        /// outer frame count.
        outer: usize,
    },
    StoreDynVar {
        /// destination register of the outer frame.
        dst: BcReg,
        /// outer frame count.
        outer: usize,
        /// source register of the current frame.
        src: BcReg,
    },
    LoadIvar(BcReg, IdentId),  // ret, id  - %ret = @id
    StoreIvar(BcReg, IdentId), // src, id  - @id = %src
    BitNot {
        ret: BcReg,
        src: BcReg,
    },
    Pos {
        ret: BcReg,
        src: BcReg,
    },
    Neg {
        ret: BcReg,
        src: BcReg,
    },
    Not {
        ret: BcReg,
        src: BcReg,
    },
    BinOp(BinOpK, BcReg, BinopMode),      // kind, ret, (lhs, rhs)
    Cmp(CmpKind, BcReg, BinopMode, bool), // kind, dst, (lhs, rhs), optimizable
    Mov(BcReg, BcReg),                    // dst, offset
    CheckLocal(BcReg, Label),
    Br(Label),
    CondBr(BcReg, Label, bool, BrKind),
    Ret(BcReg),
    MethodRet(BcReg),
    Break(BcReg),
    Raise(BcReg),
    EnsureEnd,
    MethodCall(Option<BcReg>, CallSiteId, bool), // (ret, id, has_splat)
    MethodCallBlock(Option<BcReg>, CallSiteId, bool), // (ret, id, has_splat)
    Super(Option<BcReg>, CallSiteId),
    MethodArgs(BcReg, BcReg, usize), // (recv, args, args_len)
    Yield {
        ret: Option<BcReg>,
        args: BcReg,
        len: usize,
        callid: CallSiteId,
    },
    InitMethod(FnInitInfo),
    InitBlock(FnInitInfo),
    MethodDef {
        name: IdentId,
        func_id: FuncId,
    },
    ClassDef {
        ret: Option<BcReg>,
        superclass: Option<BcReg>,
        name: IdentId,
        func_id: FuncId,
    },
    SingletonClassDef {
        ret: Option<BcReg>,
        base: BcReg,
        func_id: FuncId,
    },
    ModuleDef {
        ret: Option<BcReg>,
        name: IdentId,
        func_id: FuncId,
    },
    SingletonMethodDef {
        obj: BcReg,
        name: IdentId,
        func_id: FuncId,
    },
    ConcatStr(Option<BcReg>, BcTemp, usize), // (ret, args, args_len)
    ConcatRegexp(Option<BcReg>, BcTemp, usize), // (ret, args, args_len)
    ExpandArray(BcReg, BcReg, u16),          // (src, dst, len)
    AliasMethod {
        new: BcReg,
        old: BcReg,
    },
    DefinedYield {
        ret: BcReg,
    },
    DefinedConst {
        ret: BcReg,
        toplevel: bool,
        prefix: Vec<IdentId>,
        name: IdentId,
    },
    DefinedMethod {
        ret: BcReg,
        recv: BcReg,
        name: IdentId,
    },
    DefinedGvar {
        ret: BcReg,
        name: IdentId,
    },
    DefinedIvar {
        ret: BcReg,
        name: IdentId,
    },
    LoopStart,
    LoopEnd,
}

#[derive(Clone, PartialEq, Default)]
pub struct FnInitInfo {
    pub reg_num: usize,
    pub arg_num: usize,
    pub req_num: usize,
    pub reqopt_num: usize,
    pub block_pos: usize,
    /// bit 0:rest(yes=1 no =0) bit 1:block
    pub info: usize,
    pub stack_offset: usize,
}

impl std::fmt::Debug for FnInitInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FnInitInfo {
            reg_num,
            arg_num,
            req_num,
            reqopt_num,
            block_pos,
            stack_offset,
            ..
        } = *self;
        write!(
            f,
            "reg:{reg_num} arg:{arg_num} req:{req_num} opt:{} rest:{} block:{:?} stack_offset:{stack_offset}",
            reqopt_num - req_num,
            self.has_rest_param(),
            if block_pos == 0 {
                None
            } else {
                Some(block_pos)
            }
        )
    }
}

impl FnInitInfo {
    pub(super) fn new(total_reg_num: usize, info: &ISeqInfo) -> Self {
        let reg_num = total_reg_num - 1;
        let arg_num = info.args.args_names.len();
        let stack_offset = (reg_num * 8 + LBP_ARG0 as usize + 15) >> 4;
        FnInitInfo {
            reg_num,
            arg_num,
            req_num: info.req_num(),
            reqopt_num: info.reqopt_num(),
            block_pos: info.block_pos(),
            info: info.info(),
            stack_offset,
        }
    }

    pub(super) fn has_rest_param(&self) -> bool {
        (self.info & 0b1) != 0
    }

    pub(crate) fn has_block_param(&self) -> bool {
        (self.info & 0b10) != 0
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(transparent)]
pub(crate) struct Bc2(pub u64);

impl Bc2 {
    fn from(op: u64) -> Self {
        Self(op)
    }

    fn class_and_version(class_id: ClassId, version: u32) -> Self {
        let id: u32 = class_id.into();
        Self(((version as u64) << 32) + (id as u64))
    }

    fn class2(class_id1: ClassId, class_id2: ClassId) -> Self {
        let id1: u32 = class_id1.into();
        let id2: u32 = class_id2.into();
        Self(((id2 as u64) << 32) + (id1 as u64))
    }

    pub fn get_value(&self) -> Value {
        Value::from(self.0)
    }
}

#[derive(Clone)]
pub(crate) struct DynVar {
    pub reg: SlotId,
    pub outer: usize,
}

impl std::fmt::Debug for DynVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "dynvar({}, {:?})", self.outer, self.reg)
    }
}
