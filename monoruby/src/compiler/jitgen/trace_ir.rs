use super::*;
use crate::bytecodegen::{inst::*, BinOpK, UnOpK};

///
/// IR for JIT compiler.
///
#[derive(Debug, Clone)]
pub(crate) enum TraceIr {
    /// branch(dest)
    Br(i32),
    /// conditional branch(%reg, dest, optimizable)  : branch when reg was true.
    CondBr(SlotId, i32, bool, BrKind),
    /// check local var(%reg, dest)  : branch when reg was None.
    OptCase {
        cond: SlotId,
        optid: OptCaseId,
    },
    CheckLocal(SlotId, i32),
    /// integer(%reg, i32)
    Integer(SlotId, i32),
    /// Symbol(%reg, IdentId)
    Symbol(SlotId, IdentId),
    /// literal(%ret, value)
    Literal(SlotId, Value),
    Array {
        dst: SlotId,
        callid: CallSiteId,
    },
    Hash {
        dst: SlotId,
        args: SlotId,
        len: u16,
    },
    Range {
        dst: SlotId,
        start: SlotId,
        end: SlotId,
        exclude_end: bool,
    },
    Index {
        dst: SlotId,
        base: SlotId,
        idx: SlotId,
    },
    IndexAssign {
        src: SlotId,
        base: SlotId,
        idx: SlotId,
    },
    LoadConst(SlotId, ConstSiteId),
    StoreConst(SlotId, IdentId),
    LoadDynVar(SlotId, DynVar),
    StoreDynVar(DynVar, SlotId),
    BlockArgProxy(SlotId, usize),
    BlockArg(SlotId, usize),
    LoadIvar(SlotId, IdentId, Option<ClassId>, IvarId), // ret, id  - %ret = @id
    StoreIvar(SlotId, IdentId, Option<ClassId>, IvarId), // src, id  - @id = %src
    LoadGvar {
        dst: SlotId,
        name: IdentId,
    },
    StoreGvar {
        src: SlotId,
        name: IdentId,
    },
    LoadSvar {
        dst: SlotId,
        id: u32,
    },
    Nil(SlotId),
    BitNot {
        dst: SlotId,
        src: SlotId,
    },
    UnOp {
        kind: UnOpK,
        dst: SlotId,
        src: SlotId,
    },
    Not {
        dst: SlotId,
        src: SlotId,
    },
    BinOp {
        kind: BinOpK,
        dst: Option<SlotId>,
        mode: OpMode,
    },
    IBinOp {
        kind: BinOpK,
        dst: Option<SlotId>,
        mode: OpMode,
    },
    FBinOp {
        kind: BinOpK,
        dst: Option<SlotId>,
        mode: OpMode,
    },
    /// cmp(cmpkind, %ret, opmode, optimizable)
    Cmp(ruruby_parse::CmpKind, Option<SlotId>, OpMode, bool),
    /// return(%src)
    Ret(SlotId),
    /// method_return(%src)
    MethodRet(SlotId),
    /// method_return(%src)
    Break(SlotId),
    /// raise(%src)
    Raise(SlotId),
    /// ensure_end
    EnsureEnd,
    /// move(%dst, %src)
    Mov(SlotId, SlotId),
    /// initialize_method
    InitMethod(FnInitInfo),
    //                0       4       8       12      16
    //                +-------+-------+-------+-------+
    // MethodCall     |   |ret|identid| class |version|
    //                +-------+-------+-------+-------+
    // MethodArgs     |   |rcv|arg|len|    CodePtr    |
    //                +-------+-------+-------+-------+
    //                |      Meta     |      PC       |
    //                +-------+-------+-------+-------+
    /// func call(%ret, name)
    MethodCall {
        callid: CallSiteId,
    },
    MethodCallBlock {
        callid: CallSiteId,
    },
    InlineCall {
        inline_id: crate::executor::inline::InlineMethodId,
        callid: CallSiteId,
    },
    InlineCache,
    Yield {
        callid: CallSiteId,
    },
    /// method definition(method_name, func_id)
    MethodDef {
        name: IdentId,
        func_id: FuncId,
    },
    /// method definition(method_name, func_id)
    SingletonMethodDef {
        obj: SlotId,
        name: IdentId,
        func_id: FuncId,
    },
    /// class definition(method_name, func_id)
    ClassDef {
        dst: Option<SlotId>,
        superclass: SlotId,
        name: IdentId,
        func_id: FuncId,
    },
    ModuleDef {
        dst: Option<SlotId>,
        name: IdentId,
        func_id: FuncId,
    },
    SingletonClassDef {
        dst: Option<SlotId>,
        base: SlotId,
        func_id: FuncId,
    },
    /// concatenate strings(ret, args, args_len)
    ConcatStr(Option<SlotId>, SlotId, u16),
    ConcatRegexp(Option<SlotId>, SlotId, u16),
    ExpandArray(SlotId, SlotId, u16),
    AliasMethod {
        new: SlotId,
        old: SlotId,
    },
    DefinedYield {
        dst: SlotId,
    },
    DefinedConst {
        dst: SlotId,
        siteid: ConstSiteId,
    },
    DefinedMethod {
        dst: SlotId,
        recv: SlotId,
        name: IdentId,
    },
    DefinedGvar {
        dst: SlotId,
        name: IdentId,
    },
    DefinedIvar {
        dst: SlotId,
        name: IdentId,
    },
    /// loop start marker
    LoopStart(u32),
    LoopEnd,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum OpMode {
    RR(SlotId, SlotId),
    RI(SlotId, i16),
    IR(i16, SlotId),
}

impl OpMode {
    pub(crate) fn is_float_op(&self, pc: &Bc) -> bool {
        match self {
            Self::RR(..) => pc.is_float_binop(),
            Self::RI(..) => pc.is_float1(),
            Self::IR(..) => pc.is_float2(),
        }
    }

    pub(crate) fn is_integer_op(&self, pc: &Bc) -> bool {
        match self {
            Self::RR(..) => pc.is_integer_binop(),
            Self::RI(..) => pc.is_integer1(),
            Self::IR(..) => pc.is_integer2(),
        }
    }
}
