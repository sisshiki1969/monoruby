use ruruby_parse::CmpKind;

use crate::{
    bytecodegen::inst::{BrKind, DynVar, FnInitInfo, MethodInfo},
    *,
};

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
    BlockArg(SlotId, usize),
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
    BitNot {
        ret: SlotId,
        src: SlotId,
    },
    /// negate(%ret, %src)
    Neg {
        ret: SlotId,
        src: SlotId,
    },
    Pos {
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
    IBinOp {
        kind: BinOpK,
        ret: SlotId,
        mode: OpMode,
    },
    FBinOp {
        kind: BinOpK,
        ret: SlotId,
        mode: OpMode,
    },
    /// cmp(%ret, %lhs, %rhs, optimizable)
    Cmp(ruruby_parse::CmpKind, SlotId, OpMode, bool),
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
        has_splat: bool,
        info: MethodInfo,
        #[allow(dead_code)]
        class: ClassId,
        #[allow(dead_code)]
        version: u32,
    },
    MethodCallBlock {
        callid: CallSiteId,
        has_splat: bool,
        info: MethodInfo,
        #[allow(dead_code)]
        class: ClassId,
        #[allow(dead_code)]
        version: u32,
    },
    Super {
        callid: CallSiteId,
        info: MethodInfo,
        #[allow(dead_code)]
        class: ClassId,
        #[allow(dead_code)]
        version: u32,
    },
    InlineCall {
        inline_id: crate::executor::inline::InlineMethodId,
        callsite: CallSiteId,
        #[allow(dead_code)]
        class: ClassId,
        #[allow(dead_code)]
        version: u32,
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
    ConcatRegexp(SlotId, SlotId, u16),
    ExpandArray(SlotId, SlotId, u16),
    AliasMethod {
        new: SlotId,
        old: SlotId,
    },
    DefinedYield {
        ret: SlotId,
    },
    DefinedConst {
        ret: SlotId,
        siteid: ConstSiteId,
    },
    DefinedMethod {
        ret: SlotId,
        recv: SlotId,
        name: IdentId,
    },
    DefinedGvar {
        ret: SlotId,
        name: IdentId,
    },
    DefinedIvar {
        ret: SlotId,
        name: IdentId,
    },
    /// loop start marker
    LoopStart(u32),
    LoopEnd,
}

pub(in crate::executor) fn dec_wl(op: u64) -> (u16, u32) {
    ((op >> 32) as u16, op as u32)
}

fn dec_www(op: u64) -> (u16, u16, u16) {
    ((op >> 32) as u16, (op >> 16) as u16, op as u16)
}

impl TraceIr {
    pub(crate) fn from_bc(pc: BcPc) -> Self {
        let op = pc.op1;
        let opcode = pc.opcode();
        if opcode & 0xffc0 == 0 {
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
                23 => Self::BlockArg(SlotId::new(op1), op2 as usize),
                25 => Self::LoadGvar {
                    dst: SlotId::new(op1),
                    name: IdentId::from(op2),
                },
                26 => Self::StoreGvar {
                    src: SlotId::new(op1),
                    name: IdentId::from(op2),
                },
                28 => Self::LoadSvar {
                    dst: SlotId::new(op1),
                    id: op2,
                },
                30..=31 => {
                    let (class, version) = pc.class_version();
                    let info = match Self::from_bc(pc + 1) {
                        Self::MethodArgs(info) => info,
                        _ => unreachable!(),
                    };
                    let has_splat = opcode == 30;

                    if let Some(func_data) = info.func_data {
                        if !has_splat {
                            if let Some(inline_id) =
                                crate::executor::inline::InlineTable::get_inline(
                                    func_data.func_id(),
                                )
                            {
                                return Self::InlineCall {
                                    inline_id,
                                    callsite: op2.into(),
                                    class,
                                    version,
                                };
                            }
                        }
                    }
                    Self::MethodCall {
                        callid: op2.into(),
                        has_splat,
                        info,
                        class,
                        version,
                    }
                }
                32..=33 => {
                    let (class, version) = pc.class_version();
                    let info = match Self::from_bc(pc + 1) {
                        Self::MethodArgs(info) => info,
                        _ => unreachable!(),
                    };
                    Self::MethodCallBlock {
                        callid: op2.into(),
                        has_splat: opcode == 32,
                        info,
                        class,
                        version,
                    }
                }
                34 => {
                    let (class, version) = pc.class_version();
                    let info = match Self::from_bc(pc + 1) {
                        Self::MethodArgs(info) => info,
                        _ => unreachable!(),
                    };
                    Self::Super {
                        callid: op2.into(),
                        info,
                        class,
                        version,
                    }
                }
                _ => unreachable!("{:016x}", op),
            }
        } else {
            let (op1, op2, op3) = dec_www(op);
            match opcode {
                64 => Self::DefinedYield {
                    ret: SlotId::new(op1),
                },
                65 => Self::DefinedConst {
                    ret: SlotId::new(op1),
                    siteid: ConstSiteId(pc.op2.0 as u32),
                },
                66 => Self::DefinedMethod {
                    ret: SlotId::new(op1),
                    recv: SlotId::new(op2),
                    name: IdentId::from(pc.op2.0 as u32),
                },
                67 => Self::DefinedGvar {
                    ret: SlotId::new(op1),
                    name: IdentId::from(pc.op2.0 as u32),
                },
                68 => Self::DefinedIvar {
                    ret: SlotId::new(op1),
                    name: IdentId::from(pc.op2.0 as u32),
                },
                80 => Self::Ret(SlotId::new(op1)),
                81 => Self::MethodRet(SlotId::new(op1)),
                82 => Self::Break(SlotId::new(op1)),
                83 => Self::Raise(SlotId::new(op1)),
                85 => Self::EnsureEnd,
                86 => Self::ConcatRegexp(SlotId::new(op1), SlotId::new(op2), op3),
                126 => Self::Pos {
                    ret: SlotId::new(op1),
                    src: SlotId::new(op2),
                },
                127 => Self::BitNot {
                    ret: SlotId::new(op1),
                    src: SlotId::new(op2),
                },
                128 => Self::Not {
                    ret: SlotId::new(op1),
                    src: SlotId::new(op2),
                },
                129 => Self::Neg {
                    ret: SlotId::new(op1),
                    src: SlotId::new(op2),
                },
                130 => Self::MethodArgs(MethodInfo::new(pc.func_data())),
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
                172 => Self::InitMethod(FnInitInfo {
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
                        Self::IBinOp { kind, ret, mode }
                    } else if pc.is_float2() {
                        Self::FBinOp { kind, ret, mode }
                    } else {
                        Self::BinOp { kind, ret, mode }
                    }
                }
                200..=219 => {
                    let kind = BinOpK::from(opcode - 200);
                    let ret = SlotId::new(op1);
                    let mode = OpMode::RR(SlotId::new(op2), SlotId::new(op3));
                    if pc.is_integer_binop() {
                        Self::IBinOp { kind, ret, mode }
                    } else if pc.is_float_binop() {
                        Self::FBinOp { kind, ret, mode }
                    } else {
                        Self::BinOp { kind, ret, mode }
                    }
                }
                220..=239 => {
                    let kind = BinOpK::from(opcode - 220);
                    let ret = SlotId::new(op1);
                    let mode = OpMode::RI(SlotId::new(op2), op3 as i16);
                    if pc.is_integer1() {
                        Self::IBinOp { kind, ret, mode }
                    } else if pc.is_float1() {
                        Self::FBinOp { kind, ret, mode }
                    } else {
                        Self::BinOp { kind, ret, mode }
                    }
                }
                _ => unreachable!("{:016x}", op),
            }
        }
    }
}
