use super::*;
use ruruby_parse::CmpKind;

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
    Array(BcReg, BcReg, u16),
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
    LoadConst(BcReg, bool, Vec<IdentId>, IdentId),
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
    CheckLocal(BcReg, usize),
    Br(usize),
    CondBr(BcReg, usize, bool, BrKind),
    Ret(BcReg),
    MethodCall(Option<BcReg>, CallSiteId, bool), // (ret, id, has_splat)
    MethodCallBlock(Option<BcReg>, CallSiteId, bool), // (ret, id, has_splat)
    Yield {
        ret: Option<BcReg>,
        args: BcReg,
        len: usize,
        callid: CallSiteId,
    },
    InitMethod(FnInitInfo),
    InitBlock(FnInitInfo),
    MethodArgs(BcReg, BcReg, usize), // (recv, args, args_len)
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
    ExpandArray(BcReg, BcReg, u16),          // (src, dst, len)
    Splat(BcReg),
    AliasMethod {
        new: BcReg,
        old: BcReg,
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
    pub(super) fn new(info: &ISeqInfo) -> Self {
        let reg_num = info.total_reg_num() - 1;
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

    pub(super) fn has_block_param(&self) -> bool {
        (self.info & 0b10) != 0
    }
}

#[derive(Clone, Copy, PartialEq)]
#[repr(C)]
pub struct Bc {
    pub(crate) op1: u64,
    pub(crate) op2: Bc2,
}

impl Bc {
    pub(crate) fn from(op1: u64) -> Self {
        Self {
            op1,
            op2: Bc2::from(0),
        }
    }

    pub(crate) fn from_with_num(op1: u64, num0: u16, num1: u16, num2: u16, num3: u16) -> Self {
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

    pub(crate) fn from_fn_info(op1: u64, fn_info: &FnInitInfo) -> Self {
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

    pub(crate) fn from_with_callid(op1: u64, callid: CallSiteId) -> Self {
        Self {
            op1,
            op2: Bc2::from(callid.get() as u64),
        }
    }

    pub(crate) fn from_with_value(op1: u64, val: Value) -> Self {
        Self {
            op1,
            op2: Bc2::from(val.get()),
        }
    }

    pub(crate) fn from_with_func_name_id(op1: u64, name: Option<IdentId>, func_id: FuncId) -> Self {
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

    pub(crate) fn from_with_class_and_version(op1: u64, class_id: ClassId, version: u32) -> Self {
        Self {
            op1,
            op2: Bc2::class_and_version(class_id, version),
        }
    }

    pub(crate) fn from_with_class2(op1: u64) -> Self {
        Self {
            op1,
            op2: Bc2::class2(ClassId::default(), ClassId::default()),
        }
    }

    pub(crate) fn classid1(&self) -> ClassId {
        ClassId::new(self.op2.0 as u32)
    }

    pub(crate) fn classid2(&self) -> ClassId {
        ClassId::new((self.op2.0 >> 32) as u32)
    }

    pub(crate) fn class_version(&self) -> (ClassId, u32) {
        let op = self.op2.0;
        (ClassId::new(op as u32), (op >> 32) as u32)
    }

    pub(crate) fn u16(&self, id: usize) -> u16 {
        (self.op2.0 >> (id * 16)) as u16
    }

    fn func_data(&self) -> Option<&'static FuncData> {
        let op = self.op2.0;
        if op == 0 {
            None
        } else {
            Some(unsafe { &*(op as *const FuncData) })
        }
    }

    pub(crate) fn value(&self) -> Option<Value> {
        match self.op2.0 {
            0 => None,
            v => Some(Value::from(v)),
        }
    }

    #[cfg(any(feature = "emit-bc", feature = "emit-asm", feature = "log-jit"))]
    pub(crate) fn into_jit_addr(self) -> u64 {
        self.op2.0
    }

    pub(crate) fn is_integer1(&self) -> bool {
        self.classid1() == INTEGER_CLASS
    }

    pub(crate) fn is_integer2(&self) -> bool {
        self.classid2() == INTEGER_CLASS
    }

    pub(crate) fn is_float1(&self) -> bool {
        self.classid1() == FLOAT_CLASS
    }

    pub(crate) fn is_float2(&self) -> bool {
        self.classid2() == FLOAT_CLASS
    }

    pub(crate) fn is_integer_binop(&self) -> bool {
        self.classid1() == INTEGER_CLASS && self.classid2() == INTEGER_CLASS
    }

    pub(crate) fn is_float_binop(&self) -> bool {
        match (self.classid1(), self.classid2()) {
            (INTEGER_CLASS, INTEGER_CLASS) => false,
            (INTEGER_CLASS | FLOAT_CLASS, INTEGER_CLASS | FLOAT_CLASS) => true,
            _ => false,
        }
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

    fn class2(class_id1: ClassId, class_id2: ClassId) -> Self {
        let id1: u32 = class_id1.into();
        let id2: u32 = class_id2.into();
        Self(((id2 as u64) << 32) + (id1 as u64))
    }

    fn get_value(&self) -> Value {
        Value::from(self.0)
    }
}

///
/// Bytecode instructions.
///
#[derive(Debug, Clone)]
pub(super) enum TraceIr {
    /// branch(dest)
    Br(i32),
    /// conditional branch(%reg, dest, optimizable)  : branch when reg was true.
    CondBr(SlotId, i32, bool, BrKind),
    /// check local var(%reg, dest)  : branch when reg was None.
    CheckLocal(SlotId, i32),
    /// integer(%reg, i32)
    Integer(SlotId, i32),
    /// Symbol(%reg, IdentId)
    Symbol(SlotId, IdentId),
    /// literal(%ret, value)
    Literal(SlotId, Value),
    Array {
        ret: SlotId,
        args: SlotId,
        len: u16,
    },
    Hash {
        ret: SlotId,
        args: SlotId,
        len: u16,
    },
    Range {
        ret: SlotId,
        start: SlotId,
        end: SlotId,
        exclude_end: bool,
    },
    Index {
        ret: SlotId,
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
    LoadIvar(SlotId, IdentId, ClassId, IvarId), // ret, id  - %ret = @id
    StoreIvar(SlotId, IdentId, ClassId, IvarId), // src, id  - @id = %src
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
    /// nil(%reg)
    Nil(SlotId),
    /// negate(%ret, %src)
    Neg {
        ret: SlotId,
        src: SlotId,
    },
    Not {
        ret: SlotId,
        src: SlotId,
    },
    /// binop(kind, %ret, %lhs, %rhs)
    BinOp {
        kind: BinOpK,
        ret: SlotId,
        mode: OpMode,
    },
    IntegerBinOp {
        kind: BinOpK,
        ret: SlotId,
        mode: OpMode,
    },
    FloatBinOp {
        kind: BinOpK,
        ret: SlotId,
        mode: OpMode,
    },
    /// cmp(%ret, %lhs, %rhs, optimizable)
    Cmp(CmpKind, SlotId, OpMode, bool),
    /// return(%ret)
    Ret(SlotId),
    /// move(%dst, %src)
    Mov(SlotId, SlotId),
    /// initialize_method
    InitMethod(FnInitInfo),
    /// initialize_block
    InitBlock(FnInitInfo),
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
        ret: SlotId,
        callid: CallSiteId,
        _class: ClassId,
        has_splat: bool,
        _version: u32,
        info: MethodInfo,
    },
    MethodCallBlock {
        ret: SlotId,
        callid: CallSiteId,
        _class: ClassId,
        has_splat: bool,
        _version: u32,
        info: MethodInfo,
    },
    InlineCall {
        ret: SlotId,
        method: InlineMethod,
        _class: ClassId,
        _version: u32,
        info: MethodInfo,
    },
    Yield {
        ret: SlotId,
        args: SlotId,
        len: u16,
        callid: CallSiteId,
    },
    /// func call 2nd opecode(%recv, %args, len)
    MethodArgs(MethodInfo),
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
        ret: SlotId,
        superclass: SlotId,
        name: IdentId,
        func_id: FuncId,
    },
    ModuleDef {
        ret: SlotId,
        name: IdentId,
        func_id: FuncId,
    },
    SingletonClassDef {
        ret: SlotId,
        base: SlotId,
        func_id: FuncId,
    },
    /// concatenate strings(ret, args, args_len)
    ConcatStr(SlotId, SlotId, u16),
    ExpandArray(SlotId, SlotId, u16),
    Splat(SlotId),
    AliasMethod {
        new: SlotId,
        old: SlotId,
    },
    /// loop start marker
    LoopStart(u32),
    LoopEnd,
}

#[derive(Debug, Clone)]
pub(super) struct MethodInfo {
    pub recv: SlotId,
    pub args: SlotId,
    pub len: u16,
    pub func_data: Option<&'static FuncData>,
}

impl MethodInfo {
    fn new(recv: SlotId, args: SlotId, len: u16, func_data: Option<&'static FuncData>) -> Self {
        MethodInfo {
            recv,
            args,
            len,
            func_data,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) enum BrKind {
    BrIf = 0,
    BrIfNot = 1,
}

impl BrKind {
    pub(super) fn from(i: u16) -> Self {
        match i {
            0 => Self::BrIf,
            1 => Self::BrIfNot,
            _ => unreachable!(),
        }
    }

    #[cfg(any(feature = "emit-bc", feature = "emit-asm", feature = "log-jit"))]
    pub(super) fn to_s(self) -> &'static str {
        match self {
            Self::BrIf => "",
            Self::BrIfNot => "not",
        }
    }
}

fn dec_wl(op: u64) -> (u16, u32) {
    ((op >> 32) as u16, op as u32)
}

fn dec_www(op: u64) -> (u16, u16, u16) {
    ((op >> 32) as u16, (op >> 16) as u16, op as u16)
}

impl TraceIr {
    pub(crate) fn from_bc(pc: BcPc, fnstore: &FnStore) -> Self {
        let op = pc.op1;
        let opcode = (op >> 48) as u16;
        if opcode & 0x80 == 0 {
            let (op1, op2) = dec_wl(op);
            match opcode {
                1 => Self::SingletonMethodDef {
                    obj: SlotId::new(op1),
                    name: IdentId::from((pc.op2.0) as u32),
                    func_id: FuncId::new((pc.op2.0 >> 32) as u32),
                },
                2 => Self::MethodDef {
                    name: IdentId::from((pc.op2.0) as u32),
                    func_id: FuncId::new((pc.op2.0 >> 32) as u32),
                },
                3 => Self::Br(op2 as i32),
                4 => Self::CondBr(SlotId::new(op1), op2 as i32, false, BrKind::BrIf),
                5 => Self::CondBr(SlotId::new(op1), op2 as i32, false, BrKind::BrIfNot),
                6 => Self::Integer(SlotId::new(op1), op2 as i32),
                7 => Self::Literal(SlotId::new(op1), pc.op2.get_value()),
                8 => Self::Nil(SlotId::new(op1)),
                9 => Self::Symbol(SlotId::new(op1), IdentId::from(op2)),
                10 => Self::LoadConst(SlotId::new(op1), ConstSiteId(op2)),
                11 => Self::StoreConst(SlotId::new(op1), IdentId::from(op2)),
                12..=13 => Self::CondBr(
                    SlotId::new(op1),
                    op2 as i32,
                    true,
                    BrKind::from(opcode - 12),
                ),
                14 => Self::LoopStart(op2),
                15 => Self::LoopEnd,
                16 => {
                    let (class, ivar) = pc.class_version();
                    Self::LoadIvar(
                        SlotId::new(op1),
                        IdentId::from(op2),
                        class,
                        IvarId::new(ivar),
                    )
                }
                17 => {
                    let (class, ivar) = pc.class_version();
                    Self::StoreIvar(
                        SlotId::new(op1),
                        IdentId::from(op2),
                        class,
                        IvarId::new(ivar),
                    )
                }
                18 => Self::ClassDef {
                    ret: SlotId::new(op1),
                    superclass: SlotId::new(op2 as u16),
                    name: IdentId::from((pc.op2.0) as u32),
                    func_id: FuncId::new((pc.op2.0 >> 32) as u32),
                },
                19 => Self::ModuleDef {
                    ret: SlotId::new(op1),
                    name: IdentId::from((pc.op2.0) as u32),
                    func_id: FuncId::new((pc.op2.0 >> 32) as u32),
                },
                20 => Self::CheckLocal(SlotId::new(op1), op2 as i32),
                21 => Self::BlockArgProxy(SlotId::new(op1), op2 as usize),
                22 => Self::SingletonClassDef {
                    ret: SlotId::new(op1),
                    base: SlotId::new(op2 as u16),
                    func_id: FuncId::new((pc.op2.0 >> 32) as u32),
                },
                25 => Self::LoadGvar {
                    dst: SlotId::new(op1),
                    name: IdentId::from(op2),
                },
                26 => Self::StoreGvar {
                    src: SlotId::new(op1),
                    name: IdentId::from(op2),
                },
                27 => Self::Splat(SlotId::new(op1)),
                28 => Self::LoadSvar {
                    dst: SlotId::new(op1),
                    id: op2,
                },
                30..=31 => {
                    let (_class, _version) = pc.class_version();
                    let info = match Self::from_bc(pc + 1, fnstore) {
                        Self::MethodArgs(info) => info,
                        _ => unreachable!(),
                    };
                    let has_splat = opcode == 30;

                    if let Some(func_data) = info.func_data {
                        if !has_splat {
                            if let Some(inline_id) = fnstore.get_inline(func_data.meta.func_id()) {
                                return Self::InlineCall {
                                    ret: SlotId::new(op1),
                                    method: *inline_id,
                                    _class,
                                    _version,
                                    info,
                                };
                            }
                        }
                    }
                    Self::MethodCall {
                        ret: SlotId::new(op1),
                        callid: op2.into(),
                        _class,
                        has_splat,
                        _version,
                        info,
                    }
                }
                32..=33 => {
                    let (_class, _version) = pc.class_version();
                    let info = match Self::from_bc(pc + 1, fnstore) {
                        Self::MethodArgs(info) => info,
                        _ => unreachable!(),
                    };
                    Self::MethodCallBlock {
                        ret: SlotId::new(op1),
                        callid: op2.into(),
                        _class,
                        has_splat: opcode == 32,
                        _version,
                        info,
                    }
                }
                _ => unreachable!("{:016x}", op),
            }
        } else {
            let (op1, op2, op3) = dec_www(op);
            match opcode {
                128 => Self::Not {
                    ret: SlotId::new(op1),
                    src: SlotId::new(op2),
                },
                129 => Self::Neg {
                    ret: SlotId::new(op1),
                    src: SlotId::new(op2),
                },
                130 => Self::MethodArgs(MethodInfo::new(
                    SlotId::new(op1),
                    SlotId::new(op2),
                    op3,
                    pc.func_data(),
                )),
                131 => Self::Array {
                    ret: SlotId::new(op1),
                    args: SlotId::new(op2),
                    len: op3,
                },
                132 => Self::Index {
                    ret: SlotId::new(op1),
                    base: SlotId::new(op2),
                    idx: SlotId::new(op3),
                },
                133 => Self::IndexAssign {
                    src: SlotId::new(op1),
                    base: SlotId::new(op2),
                    idx: SlotId::new(op3),
                },
                134..=141 => Self::Cmp(
                    CmpKind::from(opcode - 134),
                    SlotId::new(op1),
                    OpMode::RR(SlotId::new(op2), SlotId::new(op3)),
                    false,
                ),
                142..=149 => Self::Cmp(
                    CmpKind::from(opcode - 142),
                    SlotId::new(op1),
                    OpMode::RI(SlotId::new(op2), op3 as i16),
                    false,
                ),
                150 => Self::LoadDynVar(
                    SlotId::new(op1),
                    DynVar {
                        reg: SlotId::new(op2),
                        outer: op3 as usize,
                    },
                ),
                151 => Self::StoreDynVar(
                    DynVar {
                        reg: SlotId::new(op1),
                        outer: op2 as usize,
                    },
                    SlotId::new(op3),
                ),
                152 => Self::Yield {
                    ret: SlotId::new(op1),
                    args: SlotId::new(op2),
                    len: op3,
                    callid: CallSiteId::from(pc.op2.0 as u32),
                },
                154..=161 => Self::Cmp(
                    CmpKind::from(opcode - 154),
                    SlotId(op1),
                    OpMode::RR(SlotId(op2), SlotId(op3)),
                    true,
                ),
                162..=169 => Self::Cmp(
                    CmpKind::from(opcode - 162),
                    SlotId::new(op1),
                    OpMode::RI(SlotId::new(op2), op3 as i16),
                    true,
                ),
                170 => Self::InitMethod(FnInitInfo {
                    reg_num: op1 as usize,
                    arg_num: pc.u16(3) as usize,
                    block_pos: pc.u16(1) as usize,
                    reqopt_num: op2 as usize,
                    req_num: pc.u16(0) as usize,
                    info: pc.u16(2) as usize,
                    stack_offset: op3 as usize,
                }),
                171 => Self::ExpandArray(SlotId::new(op1), SlotId::new(op2), op3),
                172 => Self::InitBlock(FnInitInfo {
                    reg_num: op1 as usize,
                    arg_num: pc.u16(3) as usize,
                    block_pos: pc.u16(1) as usize,
                    reqopt_num: op2 as usize,
                    req_num: pc.u16(0) as usize,
                    info: pc.u16(2) as usize,
                    stack_offset: op3 as usize,
                }),
                173 => Self::AliasMethod {
                    new: SlotId::new(op2),
                    old: SlotId::new(op3),
                },
                174 => Self::Hash {
                    ret: SlotId::new(op1),
                    args: SlotId::new(op2),
                    len: op3,
                },
                175 => Self::Ret(SlotId::new(op1)),
                176 => Self::Mov(SlotId::new(op1), SlotId::new(op2)),
                177..=178 => Self::Range {
                    ret: SlotId::new(op1),
                    start: SlotId::new(op2),
                    end: SlotId::new(op3),
                    exclude_end: match opcode - 177 {
                        0 => false,
                        1 => true,
                        _ => unreachable!(),
                    },
                },
                179 => Self::ConcatStr(SlotId::new(op1), SlotId::new(op2), op3),
                180..=199 => {
                    let kind = BinOpK::from(opcode - 180);
                    let ret = SlotId::new(op1);
                    let mode = OpMode::IR(op2 as i16, SlotId::new(op3));
                    if pc.is_integer2() {
                        Self::IntegerBinOp { kind, ret, mode }
                    } else if pc.is_float2() {
                        Self::FloatBinOp { kind, ret, mode }
                    } else {
                        Self::BinOp { kind, ret, mode }
                    }
                }
                200..=219 => {
                    let kind = BinOpK::from(opcode - 200);
                    let ret = SlotId::new(op1);
                    let mode = OpMode::RR(SlotId::new(op2), SlotId::new(op3));
                    if pc.is_integer_binop() {
                        Self::IntegerBinOp { kind, ret, mode }
                    } else if pc.is_float_binop() {
                        Self::FloatBinOp { kind, ret, mode }
                    } else {
                        Self::BinOp { kind, ret, mode }
                    }
                }
                220..=239 => {
                    let kind = BinOpK::from(opcode - 220);
                    let ret = SlotId::new(op1);
                    let mode = OpMode::RI(SlotId::new(op2), op3 as i16);
                    if pc.is_integer1() {
                        Self::IntegerBinOp { kind, ret, mode }
                    } else if pc.is_float1() {
                        Self::FloatBinOp { kind, ret, mode }
                    } else {
                        Self::BinOp { kind, ret, mode }
                    }
                }
                _ => unreachable!("{:016x}", op),
            }
        }
    }

    pub(crate) fn is_branch(pc: BcPc) -> Option<i32> {
        let op = pc.op1;
        let opcode = (op >> 48) as u16;
        if opcode & 0x80 == 0 {
            let (_, op2) = dec_wl(op);
            match opcode {
                3 => Some(op2 as i32),       // Br
                4 => Some(op2 as i32),       // CondBr
                5 => Some(op2 as i32),       // CondBr
                12..=13 => Some(op2 as i32), // CondBr
                20 => Some(op2 as i32),      // CheckLocal
                _ => None,
            }
        } else {
            None
        }
    }

    pub(crate) fn is_terminal(pc: BcPc) -> bool {
        let op = pc.op1;
        let opcode = (op >> 48) as u16;
        if opcode & 0x80 == 0 {
            opcode == 3 // Br
        } else {
            opcode == 175 // Ret
        }
    }
}
