use super::*;
use ruruby_parse::CmpKind;

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

pub(crate) struct Ir(Vec<(BcIr, Loc)>);

impl Ir {
    pub(super) fn new(ir: Vec<(BcIr, Loc)>) -> Self {
        Self(ir)
    }

    pub(crate) fn is_loop_start(&self, index: usize) -> bool {
        match self.0[index].0 {
            BcIr::LoopStart => true,
            _ => false,
        }
    }

    pub(crate) fn is_loop_end(&self, index: usize) -> bool {
        match self.0[index].0 {
            BcIr::LoopEnd => true,
            _ => false,
        }
    }

    pub(crate) fn is_terminal(&self, index: usize) -> bool {
        self.0[index].0.is_terminal()
    }
}

///
/// bytecode Ir.
///
#[derive(Debug, Clone)]
pub(super) enum BcIr {
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
    Index(BcReg, BcReg, BcReg),      // ret, base, index
    StoreIndex(BcReg, BcReg, BcReg), // src, base, index
    LoadConst {
        dst: BcReg,
        base: Option<BcReg>,
        toplevel: bool,
        prefix: Vec<IdentId>,
        name: IdentId,
    },
    StoreConst(BcReg, IdentId),
    LoadGvar {
        dst: BcReg,
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
    BinOp(BinOpK, Option<BcReg>, BinopMode), // kind, ret, (lhs, rhs)
    Cmp(CmpKind, Option<BcReg>, BinopMode, bool), // kind, dst, (lhs, rhs), optimizable
    Mov(BcReg, BcReg),                       // dst, offset
    CheckLocal(BcReg, Label),
    Br(Label),
    CondBr(BcReg, Label, bool, BrKind),
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
    Break(BcReg),
    Raise(BcReg),
    EnsureEnd,
    MethodCall(Option<BcReg>, Box<CallSite>, bool), // (ret, id, has_splat)
    MethodCallBlock(Option<BcReg>, Box<CallSite>, bool), // (ret, id, has_splat)
    Super(Option<BcReg>, Box<CallSite>),
    MethodArgs(BcReg, BcReg, usize), // (recv, args, args_len)
    Yield {
        ret: Option<BcReg>,
        args: BcReg,
        len: usize,
        callsite: Box<CallSite>,
    },
    InitMethod(FnInitInfo),
    InitBlock(FnInitInfo),
    MethodDef {
        name: IdentId,
        func: Box<Functions>,
    },
    ClassDef {
        ret: Option<BcReg>,
        superclass: Option<BcReg>,
        name: IdentId,
        func: Box<Functions>,
    },
    SingletonClassDef {
        ret: Option<BcReg>,
        base: BcReg,
        func: Box<Functions>,
    },
    ModuleDef {
        ret: Option<BcReg>,
        name: IdentId,
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

impl BcIr {
    pub(crate) fn is_terminal(&self) -> bool {
        match self {
            // Br or Ret or MethodRet or Break or Raise or OptCase
            Self::Br(_)
            | Self::Ret(_)
            | Self::MethodRet(_)
            | Self::Break(_)
            | Self::Raise(_)
            | Self::OptCase { .. } => true,
            _ => false,
        }
    }
}

#[derive(Clone, PartialEq, Default)]
pub(crate) struct FnInitInfo {
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
    pub(crate) fn from(op: u64) -> Self {
        Self(op)
    }

    pub(crate) fn class_and_version(class_id: ClassId, version: u32) -> Self {
        let id: u32 = class_id.into();
        Self(((version as u64) << 32) + (id as u64))
    }

    pub(crate) fn class2(class_id1: ClassId, class_id2: ClassId) -> Self {
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
