use super::*;
use ruruby_parse::CmpKind;

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum BrKind {
    BrIf = 0,
    BrIfNot = 1,
}

impl BrKind {
    pub fn from(i: u8) -> Self {
        match i {
            0 => Self::BrIf,
            1 => Self::BrIfNot,
            _ => unreachable!(),
        }
    }

    #[cfg(feature = "dump-bc")]
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

pub(crate) struct BytecodeIr(Vec<(BytecodeInst, Loc)>);

impl BytecodeIr {
    pub(super) fn new(ir: Vec<(BytecodeInst, Loc)>) -> Self {
        Self(ir)
    }

    pub(crate) fn is_loop_start(&self, index: usize) -> bool {
        matches!(self.0[index].0, BytecodeInst::LoopStart)
    }

    pub(crate) fn is_loop_end(&self, index: usize) -> bool {
        matches!(self.0[index].0, BytecodeInst::LoopEnd)
    }

    pub(crate) fn is_terminal(&self, index: usize) -> bool {
        self.0[index].0.is_terminal()
    }
}

///
/// bytecode Ir.
///
#[derive(Debug, Clone)]
pub(super) enum BytecodeInst {
    Nil(BcReg),
    Integer(BcReg, i32),
    Symbol(BcReg, IdentId),
    Literal(BcReg, Value),
    Array(BcReg, Box<CallSite>),
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
    Lambda {
        dst: BcReg,
        func: Box<Functions>,
    },
    Index(BcReg, BcReg, BcReg),      // ret, base, index
    StoreIndex(BcReg, BcReg, BcReg), // src, base, index
    LoadConst {
        dst: BcReg,
        base: Option<BcReg>,
        toplevel: bool,
        prefix: Vec<IdentId>,
        name: IdentId,
    },
    CheckConst {
        dst: BcReg,
        base: Option<BcReg>,
        toplevel: bool,
        prefix: Vec<IdentId>,
        name: IdentId,
    },
    StoreConst {
        src: BcReg,
        toplevel: bool,
        base: Option<BcReg>,
        prefix: Vec<IdentId>,
        name: IdentId,
    },
    LoadGvar {
        dst: BcReg,
        name: IdentId,
    },
    StoreGvar {
        val: BcReg,
        name: IdentId,
    },
    LoadCvar {
        dst: BcReg,
        name: IdentId,
    },
    CheckCvar {
        dst: BcReg,
        name: IdentId,
    },
    StoreCvar {
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
        dst: BcReg,
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
    BinOp(BinOpK, Option<BcReg>, BinopMode), // kind, ret, (lhs, rhs)
    Cmp(CmpKind, Option<BcReg>, BinopMode, bool), // kind, dst, (lhs, rhs), optimizable
    Mov(BcReg, BcReg),                       // dst, offset
    CheckLocal(BcReg, Label),
    Br(Label),
    CondBr(BcReg, Label, bool, BrKind),
    /// when *BcReg* is nil, goto *Label*.
    NilBr(BcReg, Label),
    OptCase {
        // a register for cond and ret.
        reg: BcReg,
        min: u16,
        max: u16,
        table: Vec<(u16, Label)>,
        /// destination labels.
        /// it is guaranteed then labels.len() >= 1 and labels[0] is the label for else clause.
        labels: Vec<Label>,
    },
    Ret(BcReg),
    MethodRet(BcReg),
    BlockBreak(BcReg),
    Raise(BcReg),
    EnsureEnd,
    MethodCall(Box<CallSite>),
    MethodCallBlock(Box<CallSite>),
    Yield(Box<CallSite>),
    InlineCache(Box<CallSite>),
    InitMethod(FnInitInfo),
    MethodDef {
        name: IdentId,
        func: Box<Functions>,
    },
    ClassDef {
        ret: Option<BcReg>,
        base: Option<BcReg>,
        superclass: Option<BcReg>,
        name: IdentId,
        func: Box<Functions>,
    },
    ModuleDef {
        ret: Option<BcReg>,
        base: Option<BcReg>,
        name: IdentId,
        func: Box<Functions>,
    },
    SingletonClassDef {
        ret: Option<BcReg>,
        base: BcReg,
        func: Box<Functions>,
    },
    SingletonMethodDef {
        obj: BcReg,
        name: IdentId,
        func: Box<Functions>,
    },
    ConcatStr(Option<BcReg>, BcTemp, usize), // (ret, args, args_len)
    ConcatRegexp(Option<BcReg>, BcTemp, usize), // (ret, args, args_len)
    ExpandArray(BcReg, BcReg, u16),          // (src, dst, len)
    AliasMethod {
        new: IdentId,
        old: IdentId,
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

impl BytecodeInst {
    pub fn is_terminal(&self) -> bool {
        match self {
            // Br or Ret or MethodRet or Break or Raise or OptCase
            Self::Br(_)
            | Self::Ret(_)
            | Self::MethodRet(_)
            | Self::BlockBreak(_)
            | Self::Raise(_)
            | Self::OptCase { .. } => true,
            _ => false,
        }
    }

    pub fn can_be_inlined(&self) -> bool {
        match self {
            Self::InitMethod(..)
            | Self::Integer(..)
            | Self::InlineCache(..)
            | Self::Literal(..)
            | Self::LoadConst { .. }
            | Self::LoadIvar(..)
            | Self::StoreConst { .. }
            | Self::StoreIvar(..)
            | Self::Ret(..) => true,
            _ => false,
        }
    }
}

#[derive(Clone, PartialEq, Default)]
pub(crate) struct FnInitInfo {
    pub reg_num: usize,
    pub arg_num: usize,
    pub stack_offset: usize,
}

impl std::fmt::Debug for FnInitInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FnInitInfo {
            reg_num,
            arg_num,
            stack_offset,
            ..
        } = *self;
        write!(f, "reg:{reg_num} arg:{arg_num} stack_offset:{stack_offset}",)
    }
}

impl FnInitInfo {
    pub(super) fn new(total_reg_num: usize, info: &ISeqInfo) -> Self {
        let reg_num = total_reg_num - 1;
        let arg_num = info.args.args_names.len();
        let stack_offset = (reg_num * 8 + (RSP_LOCAL_FRAME + LFP_ARG0) as usize + 31) / 16;
        FnInitInfo {
            reg_num,
            arg_num,
            stack_offset,
        }
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
