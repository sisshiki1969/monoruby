use super::*;
use crate::bytecodegen::{BinOpK, UnOpK, inst::*};

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum OpMode {
    RR(SlotId, SlotId),
    RI(SlotId, i16),
    IR(i16, SlotId),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum FOpClass {
    Float,
    Integer,
}

impl Into<ClassId> for FOpClass {
    fn into(self) -> ClassId {
        match self {
            FOpClass::Float => FLOAT_CLASS,
            FOpClass::Integer => INTEGER_CLASS,
        }
    }
}

impl From<ClassId> for FOpClass {
    fn from(class_id: ClassId) -> Self {
        match class_id {
            FLOAT_CLASS => FOpClass::Float,
            INTEGER_CLASS => FOpClass::Integer,
            _ => unreachable!(),
        }
    }
}

///
/// Float binary op info.
///
/// lhs and rhs are always floats or integers.
///
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct FBinOpInfo {
    pub lhs: SlotId,
    pub rhs: SlotId,
    pub lhs_class: FOpClass,
    pub rhs_class: FOpClass,
}

#[derive(Debug, Clone)]
pub(crate) struct MethodCacheEntry {
    pub recv_class: ClassId,
    pub func_id: FuncId,
    pub version: u32,
}

impl std::cmp::PartialEq for MethodCacheEntry {
    fn eq(&self, other: &Self) -> bool {
        self.recv_class == other.recv_class && self.func_id == other.func_id
    }
}

///
/// IR for JIT compiler.
///
#[derive(Debug, Clone)]
pub(crate) enum TraceIr {
    /// branch(disp)
    Br(i32),
    /// conditional branch(%reg, disp, optimizable)  : branch when reg was true.
    CondBr(SlotId, i32, bool, BrKind),
    /// conditional branch(%reg, disp)  : branch when reg is nil.
    NilBr(SlotId, i32),
    /// check local var(%reg, dest)  : branch when reg was None.
    CheckLocal(SlotId, i32),
    OptCase {
        cond: SlotId,
        min: u16,
        max: u16,
        else_disp: i32,
        branch_table: Box<[i32]>,
    },
    CheckKwRest(SlotId),

    /// integer(%reg, i32)
    Immediate(SlotId, Value),

    /// literal(%ret, value)
    Literal(SlotId, Value),
    Array {
        dst: SlotId,
        callid: CallSiteId,
    },
    Lambda {
        dst: SlotId,
        func_id: FuncId,
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

    BlockArgProxy(SlotId, usize),
    BlockArg(SlotId, usize),

    LoadConst(SlotId, ConstSiteId),
    StoreConst(SlotId, ConstSiteId),
    LoadDynVar(SlotId, DynVar),
    StoreDynVar(DynVar, SlotId),
    LoadIvar(SlotId, IdentId, Option<(ClassId, IvarId)>), // ret, id  - %ret = @id
    StoreIvar(SlotId, IdentId, Option<(ClassId, IvarId)>), // src, id  - @id = %src
    LoadGvar {
        dst: SlotId,
        name: IdentId,
    },
    StoreGvar {
        src: SlotId,
        name: IdentId,
    },
    LoadCvar {
        dst: SlotId,
        name: IdentId,
    },
    CheckCvar {
        dst: SlotId,
        name: IdentId,
    },
    StoreCvar {
        src: SlotId,
        name: IdentId,
    },
    LoadSvar {
        dst: SlotId,
        id: u32,
    },
    Not {
        dst: SlotId,
        src: SlotId,
    },
    UnOp {
        kind: UnOpK,
        dst: SlotId,
        src: SlotId,
        ic: Option<ClassId>,
    },

    BinOp {
        kind: BinOpK,
        dst: Option<SlotId>,
        lhs: SlotId,
        rhs: SlotId,
        ic: Option<(ClassId, ClassId)>,
    },
    BinCmp {
        kind: ruruby_parse::CmpKind,
        dst: Option<SlotId>,
        lhs: SlotId,
        rhs: SlotId,
        ic: Option<(ClassId, ClassId)>,
    },
    BinCmpBr {
        kind: ruruby_parse::CmpKind,
        _dst: Option<SlotId>,
        lhs: SlotId,
        rhs: SlotId,
        disp: i32,
        brkind: BrKind,
        ic: Option<(ClassId, ClassId)>,
    },
    ArrayTEq {
        lhs: SlotId,
        rhs: SlotId,
    },
    Index {
        dst: SlotId,
        base: SlotId,
        idx: SlotId,
        class: Option<(ClassId, ClassId)>, // (base_class, idx_class)
    },
    IndexAssign {
        base: SlotId,
        idx: SlotId,
        src: SlotId,
        class: Option<(ClassId, ClassId)>, // (base_class, idx_class)
    },
    MethodCall {
        _polymorphic: bool,
        callid: CallSiteId,
        cache: Option<MethodCacheEntry>,
    },

    /// return(%src)
    Ret(SlotId),
    ///
    /// Return from method.
    ///
    /// method_return(%src)
    MethodRet(SlotId),
    ///
    /// Break from block.
    ///
    /// break(%src)
    BlockBreak(SlotId),
    /// raise(%src)
    Raise(SlotId),
    /// ensure_end
    EnsureEnd,
    /// toa{%src, %dst}
    ToA {
        dst: SlotId,
        src: SlotId,
    },
    /// move(%dst, %src)
    Mov(SlotId, SlotId),
    /// initialize_method
    InitMethod(FnInitInfo),
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
        base: Option<SlotId>,
        superclass: Option<SlotId>,
        name: IdentId,
        func_id: FuncId,
    },
    ModuleDef {
        dst: Option<SlotId>,
        base: Option<SlotId>,
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
    ExpandArray {
        src: SlotId,
        dst: (SlotId, u16, Option<u16>),
    },
    UndefMethod {
        undef: IdentId,
    },
    AliasMethod {
        new: IdentId,
        old: IdentId,
    },
    ///
    /// Check if `yield` is callable.
    ///
    /// Set `dst` to "yield" if callable, `nil` if not.
    ///
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
    ///
    /// Check if `super` is callable.
    ///
    /// Set `dst` to "super" if callable, `nil` if not.
    ///
    DefinedSuper {
        dst: SlotId,
    },
    ///
    /// Check if global var `name` exists.
    ///
    /// Set `dst`` to "global-variable" if exists, `nil` if not.
    ///
    DefinedGvar {
        dst: SlotId,
        name: IdentId,
    },
    DefinedIvar {
        dst: SlotId,
        name: IdentId,
    },
    /// loop start marker
    LoopStart {
        counter: u32,
        jit_addr: *const u8,
    },
    LoopEnd,
}

impl TraceIr {
    pub(crate) fn from_pc(pc: BytecodePtr, store: &Store) -> Self {
        let op1 = pc.op1;
        let op2 = pc.op2;
        let opcode = pc.opcode();
        if opcode & 0xc0 == 0 {
            let (op1_w, op1_l) = dec_wl(op1);
            match opcode {
                1 => TraceIr::SingletonMethodDef {
                    obj: SlotId::new(op1_w),
                    name: IdentId::from(op2.0 as u32),
                    func_id: FuncId::new((op2.0 >> 32) as u32),
                },
                2 => TraceIr::MethodDef {
                    name: IdentId::from((op2.0) as u32),
                    func_id: FuncId::new((op2.0 >> 32) as u32),
                },
                3 => TraceIr::Br(op1_l as i32),
                4 => TraceIr::CondBr(SlotId::new(op1_w), op1_l as i32, false, BrKind::BrIf),
                5 => TraceIr::CondBr(SlotId::new(op1_w), op1_l as i32, false, BrKind::BrIfNot),
                6 => TraceIr::Immediate(SlotId::new(op1_w), op2.get_value()),
                7 => TraceIr::Literal(SlotId::new(op1_w), op2.get_value()),
                10 | 18 => TraceIr::LoadConst(SlotId::new(op1_w), ConstSiteId(op1_l)),
                11 => TraceIr::StoreConst(SlotId::new(op1_w), ConstSiteId(op1_l)),
                12..=13 => TraceIr::CondBr(
                    SlotId::new(op1_w),
                    op1_l as i32,
                    true,
                    BrKind::from(opcode - 12),
                ),
                14 => TraceIr::LoopStart {
                    counter: op1_l,
                    jit_addr: pc.into_jit_addr(),
                },
                15 => TraceIr::LoopEnd,
                16 => TraceIr::LoadIvar(
                    SlotId::new(op1_w),
                    IdentId::from(op1_l),
                    if let Some(class) = pc.cached_class0() {
                        let ivar = pc.cached_ivarid();
                        Some((class, ivar))
                    } else {
                        None
                    },
                ),
                17 => TraceIr::StoreIvar(
                    SlotId::new(op1_w),
                    IdentId::from(op1_l),
                    if let Some(class) = pc.cached_class0() {
                        let ivar = pc.cached_ivarid();
                        Some((class, ivar))
                    } else {
                        None
                    },
                ),
                19 => TraceIr::CheckKwRest(SlotId::new(op1_w)),
                20 => TraceIr::CheckLocal(SlotId::new(op1_w), op1_l as i32),
                21 => TraceIr::BlockArgProxy(SlotId::new(op1_w), op1_l as usize),
                22 => TraceIr::SingletonClassDef {
                    dst: SlotId::from(op1_w),
                    base: SlotId::new(op1_l as u16),
                    func_id: FuncId::new((op2.0 >> 32) as u32),
                },
                23 => TraceIr::BlockArg(SlotId::new(op1_w), op1_l as usize),
                24 => TraceIr::CheckCvar {
                    dst: SlotId::new(op1_w),
                    name: IdentId::from(op1_l),
                },
                25 => TraceIr::LoadGvar {
                    dst: SlotId::new(op1_w),
                    name: IdentId::from(op1_l),
                },
                26 => TraceIr::StoreGvar {
                    src: SlotId::new(op1_w),
                    name: IdentId::from(op1_l),
                },
                27 => TraceIr::LoadCvar {
                    dst: SlotId::new(op1_w),
                    name: IdentId::from(op1_l),
                },
                28 => TraceIr::LoadSvar {
                    dst: SlotId::new(op1_w),
                    id: op1_l,
                },
                29 => TraceIr::StoreCvar {
                    src: SlotId::new(op1_w),
                    name: IdentId::from(op1_l),
                },
                30..=33 => {
                    let callid = op1_l.into();
                    let polymorphic = match pc.opcode_sub() {
                        0 => false,
                        1 => true,
                        _ => unreachable!(),
                    };
                    let cache = if let Some(cache) = pc.method_cache() {
                        Some(cache)
                    } else {
                        None
                    };
                    TraceIr::MethodCall {
                        _polymorphic: polymorphic,
                        callid,
                        cache,
                    }
                }
                34..=35 => TraceIr::Yield {
                    callid: op1_l.into(),
                },
                36 => {
                    let optid = OptCaseId::from(op1_l);
                    let OptCaseInfo {
                        min,
                        max,
                        offsets,
                        branch_table,
                    } = &store[optid];
                    let else_disp = offsets[0] as i32;
                    let branch_table: Box<[_]> =
                        branch_table.iter().map(|ofs| *ofs as i32).collect();
                    TraceIr::OptCase {
                        cond: SlotId::new(op1_w),
                        min: *min,
                        max: *max,
                        else_disp,
                        branch_table,
                    }
                }
                37 => TraceIr::NilBr(SlotId::new(op1_w), op1_l as i32),
                38 => TraceIr::Lambda {
                    dst: SlotId::new(op1_w),
                    func_id: FuncId::new(op1_l),
                },
                39 => TraceIr::Array {
                    dst: SlotId::new(op1_w),
                    callid: CallSiteId::from(op1_l),
                },
                40 => {
                    let (_, op1_w2, op1_w3) = dec_www(op1);
                    TraceIr::ArrayTEq {
                        lhs: SlotId::new(op1_w2),
                        rhs: SlotId::new(op1_w3),
                    }
                }
                _ => unreachable!("{:016x}", op1),
            }
        } else {
            let (op1_w1, op2_w2, op3_w3) = dec_www(op1);
            match opcode {
                64 => TraceIr::DefinedYield {
                    dst: SlotId::new(op1_w1),
                },
                65 => TraceIr::DefinedConst {
                    dst: SlotId::new(op1_w1),
                    siteid: ConstSiteId(op2.0 as u32),
                },
                66 => TraceIr::DefinedMethod {
                    dst: SlotId::new(op1_w1),
                    recv: SlotId::new(op2_w2),
                    name: IdentId::from(op2.0 as u32),
                },
                67 => TraceIr::DefinedGvar {
                    dst: SlotId::new(op1_w1),
                    name: IdentId::from(op2.0 as u32),
                },
                68 => TraceIr::DefinedIvar {
                    dst: SlotId::new(op1_w1),
                    name: IdentId::from(op2.0 as u32),
                },
                69 => TraceIr::DefinedSuper {
                    dst: SlotId::new(op1_w1),
                },
                70 => TraceIr::ClassDef {
                    dst: SlotId::from(op1_w1),
                    base: SlotId::from(op2_w2),
                    superclass: SlotId::from(op3_w3),
                    name: IdentId::from((op2.0) as u32),
                    func_id: FuncId::new((op2.0 >> 32) as u32),
                },
                71 => TraceIr::ModuleDef {
                    dst: SlotId::from(op1_w1),
                    base: SlotId::from(op2_w2),
                    name: IdentId::from((op2.0) as u32),
                    func_id: FuncId::new((op2.0 >> 32) as u32),
                },
                80 => TraceIr::Ret(SlotId::new(op1_w1)),
                81 => TraceIr::MethodRet(SlotId::new(op1_w1)),
                82 => TraceIr::BlockBreak(SlotId::new(op1_w1)),
                83 => TraceIr::Raise(SlotId::new(op1_w1)),
                85 => TraceIr::EnsureEnd,
                86 => TraceIr::ConcatRegexp(SlotId::from(op1_w1), SlotId::new(op2_w2), op3_w3),
                120 => TraceIr::Not {
                    dst: SlotId::new(op1_w1),
                    src: SlotId::new(op2_w2),
                },
                121..=123 => {
                    let kind = UnOpK::from(opcode - 121);
                    let dst = SlotId::new(op1_w1);
                    let src = SlotId::new(op2_w2);
                    TraceIr::UnOp {
                        kind,
                        dst,
                        src,
                        ic: pc.classid1(),
                    }
                }
                130 => TraceIr::InlineCache,
                132 => TraceIr::Index {
                    dst: SlotId::new(op1_w1),
                    base: SlotId::new(op2_w2),
                    idx: SlotId::new(op3_w3),
                    class: if let Some(base_class) = pc.classid1()
                        && let Some(idx_class) = pc.classid2()
                    {
                        Some((base_class, idx_class))
                    } else {
                        None
                    },
                },
                133 => TraceIr::IndexAssign {
                    base: SlotId::new(op2_w2),
                    idx: SlotId::new(op3_w3),
                    src: SlotId::new(op1_w1),
                    class: if let Some(base_class) = pc.classid1()
                        && let Some(idx_class) = pc.classid2()
                    {
                        Some((base_class, idx_class))
                    } else {
                        None
                    },
                },
                140..=146 => {
                    let kind = CmpKind::from(opcode - 140);
                    let dst = SlotId::from(op1_w1);
                    let lhs = SlotId::new(op2_w2);
                    let rhs = SlotId::new(op3_w3);
                    let ic = if let Some(lhs_class) = pc.classid1()
                        && let Some(rhs_class) = pc.classid2()
                    {
                        Some((lhs_class, rhs_class))
                    } else {
                        None
                    };
                    TraceIr::BinCmp {
                        kind,
                        dst,
                        lhs,
                        rhs,
                        ic,
                    }
                }

                148 => TraceIr::LoadDynVar(
                    SlotId::new(op1_w1),
                    DynVar {
                        reg: SlotId::new(op2_w2),
                        outer: op3_w3 as usize,
                    },
                ),
                149 => TraceIr::StoreDynVar(
                    DynVar {
                        reg: SlotId::new(op1_w1),
                        outer: op2_w2 as usize,
                    },
                    SlotId::new(op3_w3),
                ),
                150..=156 => {
                    let kind = CmpKind::from(opcode - 150);
                    let dst = SlotId::from(op1_w1);
                    let lhs = SlotId::new(op2_w2);
                    let rhs = SlotId::new(op3_w3);
                    let ic = if let Some(lhs_class) = pc.classid1()
                        && let Some(rhs_class) = pc.classid2()
                    {
                        Some((lhs_class, rhs_class))
                    } else {
                        None
                    };
                    let (disp, brkind) = match TraceIr::from_pc(pc + 1, store) {
                        TraceIr::CondBr(_, dest, true, brkind) => (dest + 1, brkind),
                        _ => unreachable!(),
                    };
                    TraceIr::BinCmpBr {
                        kind,
                        _dst: dst,
                        lhs,
                        rhs,
                        disp,
                        brkind,
                        ic,
                    }
                }
                160..=168 => {
                    let kind = BinOpK::from(opcode - 160);
                    let dst = SlotId::from(op1_w1);
                    let lhs = SlotId::new(op2_w2);
                    let rhs = SlotId::new(op3_w3);
                    let ic = if let Some(lhs_class) = pc.classid1()
                        && let Some(rhs_class) = pc.classid2()
                    {
                        Some((lhs_class, rhs_class))
                    } else {
                        None
                    };
                    TraceIr::BinOp {
                        kind,
                        dst,
                        lhs,
                        rhs,
                        ic,
                    }
                }

                170 => TraceIr::InitMethod(FnInitInfo {
                    reg_num: op1_w1 as usize,
                    arg_num: op2_w2 as usize,
                    stack_offset: op3_w3 as usize,
                }),
                171 => TraceIr::ExpandArray {
                    src: SlotId::new(op1_w1),
                    dst: (SlotId::new(op2_w2), op3_w3, {
                        let rest = op2.get_u16();
                        if rest == 0 { None } else { Some(rest - 1) }
                    }),
                },
                172 => {
                    let undef = IdentId::from(dec_wl(op1).1);
                    TraceIr::UndefMethod { undef }
                }
                173 => {
                    let (new, old) = op2.get_ident2();
                    TraceIr::AliasMethod { new, old }
                }
                174 => TraceIr::Hash {
                    dst: SlotId::new(op1_w1),
                    args: SlotId::new(op2_w2),
                    len: op3_w3,
                },
                175 => TraceIr::ToA {
                    dst: SlotId::new(op1_w1),
                    src: SlotId::new(op2_w2),
                },
                176 => TraceIr::Mov(SlotId::new(op1_w1), SlotId::new(op2_w2)),
                177..=178 => TraceIr::Range {
                    dst: SlotId::new(op1_w1),
                    start: SlotId::new(op2_w2),
                    end: SlotId::new(op3_w3),
                    exclude_end: match opcode - 177 {
                        0 => false,
                        1 => true,
                        _ => unreachable!(),
                    },
                },
                179 => TraceIr::ConcatStr(SlotId::from(op1_w1), SlotId::new(op2_w2), op3_w3),
                _ => unreachable!("{:016x}", op1),
            }
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
    #[cfg(feature = "dump-traceir")]
    pub(crate) fn format(store: &Store, iseq_id: ISeqId, pc: BytecodePtr) -> Option<String> {
        fn optstr(opt: bool) -> &'static str {
            if opt { "_" } else { "" }
        }

        fn ret_str(slot: Option<SlotId>) -> String {
            match slot {
                None => "_".to_string(),
                Some(ret) => format!("{:?}", ret),
            }
        }

        fn fmt(store: &Store, s: String, class: Option<(ClassId, ClassId)>) -> String {
            format!(
                "{:36} [{}][{}]",
                s,
                store.debug_class_name(if let Some((lhs, _)) = class {
                    Some(lhs)
                } else {
                    None
                }),
                store.debug_class_name(if let Some((_, rhs)) = class {
                    Some(rhs)
                } else {
                    None
                }),
            )
        }

        fn cmp_fmt(
            store: &Store,
            kind: ruruby_parse::CmpKind,
            dst: Option<SlotId>,
            lhs: SlotId,
            rhs: SlotId,
            class: impl Into<Option<(ClassId, ClassId)>>,
            optimizable: bool,
        ) -> String {
            let class: Option<(ClassId, ClassId)> = class.into();
            let s = format!(
                "{}{} = {:?} {:?} {:?}",
                optstr(optimizable),
                ret_str(dst),
                lhs,
                kind,
                rhs,
            );
            fmt(store, s, class)
        }

        fn binop_fmt(
            store: &Store,
            kind: BinOpK,
            dst: Option<SlotId>,
            lhs: SlotId,
            rhs: SlotId,
            class: impl Into<Option<(ClassId, ClassId)>>,
        ) -> String {
            let class: Option<_> = class.into();
            let op1 = format!("{} = {:?} {} {:?}", ret_str(dst), lhs, kind, rhs);
            fmt(store, op1, class)
        }

        let iseq = &store[iseq_id];
        let bc_pos = iseq.get_pc_index(Some(pc));
        let s = match Self::from_pc(pc, store) {
            TraceIr::InitMethod(info) => {
                format!("init_method {info:?}")
            }
            TraceIr::CheckLocal(local, disp) => {
                let dest = iseq.get_bb(bc_pos + 1 + disp);
                format!("check_local({local:?}) =>{dest:?}")
            }
            TraceIr::CheckKwRest(local) => {
                format!("check_kw_rest({:?})", local)
            }
            TraceIr::Br(disp) => {
                let dest = iseq.get_bb(bc_pos + 1 + disp);
                format!("br => {dest:?}")
            }
            TraceIr::CondBr(reg, disp, opt, kind) => {
                let dest = iseq.get_bb(bc_pos + 1 + disp);
                format!("cond{}br {}{reg:?} => {dest:?}", kind.to_s(), optstr(opt),)
            }
            TraceIr::NilBr(reg, disp) => {
                let dest = iseq.get_bb(bc_pos + 1 + disp);
                format!("nilbr {reg:?} => {dest:?}")
            }
            TraceIr::OptCase {
                cond,
                min,
                max,
                else_disp,
                branch_table,
            } => {
                let else_dest = iseq.get_bb(bc_pos + 1 + else_disp);
                let branch_table: Vec<String> = branch_table
                    .iter()
                    .map(|disp| {
                        let dest = iseq.get_bb(bc_pos + 1 + *disp);
                        format!("{dest:?}")
                    })
                    .collect();
                format!(
                    "opt_case {cond:?}: else -> {else_dest:?}  {min}..{max} -> branch_table:{branch_table:?}",
                )
            }
            TraceIr::Immediate(reg, val) => format!("{:?} = {}", reg, val.debug(store)),
            TraceIr::Literal(reg, val) => {
                format!("{:?} = literal[{}]", reg, val.debug(store))
            }
            TraceIr::Range {
                dst,
                start,
                end,
                exclude_end,
            } => format!(
                "{:?} = {:?} {} {:?}",
                dst,
                start,
                if exclude_end { "..." } else { ".." },
                end
            ),
            TraceIr::Array { dst, callid } => {
                let CallSiteInfo {
                    args,
                    pos_num,
                    splat_pos,
                    ..
                } = &store[callid];
                let mut s = format!("{:?} = array[", dst);
                for i in 0..*pos_num {
                    let prefix = if splat_pos.contains(&i) { "*" } else { "" };
                    if i != 0 {
                        s += ",";
                    }
                    s += &format!("{prefix}{:?}", *args + i);
                }
                format!("{s}]")
            }
            TraceIr::Lambda { dst, func_id } => {
                format!("{:?} = lambda[{:?}]", dst, func_id)
            }
            TraceIr::Hash { dst, args, len } => {
                format!("{:?} = hash[{:?}; {}]", dst, args, len)
            }
            TraceIr::Index {
                dst,
                base,
                idx,
                class,
            } => {
                let op1 = format!("{:?} = {:?}.[{:?}]", dst, base, idx);
                fmt(store, op1, class)
            }
            TraceIr::IndexAssign {
                base,
                idx,
                src,
                class,
            } => {
                let op1 = format!("{:?}:.[{:?}:] = {:?}", base, idx, src,);
                fmt(store, op1, class)
            }
            TraceIr::LoadConst(reg, id) => {
                let op = store[id].format();
                let op1 = format!("{:?} = {op}", reg);
                format!(
                    "{:36} [{}]",
                    op1,
                    match &store[id].cache {
                        None => "<INVALID>".to_string(),
                        Some(cache) => cache.value.debug(store),
                    }
                )
            }
            TraceIr::StoreConst(src, id) => {
                let op = store[id].format();
                format!("{op} = {:?}", src)
            }
            TraceIr::BlockArgProxy(dst, outer) => {
                format!("{:?} = block_proxy({outer})", dst)
            }
            TraceIr::BlockArg(dst, outer) => {
                format!("{:?} = block_arg({outer})", dst)
            }
            TraceIr::LoadDynVar(dst, src) => {
                format!("{:?} = {:?}", dst, src)
            }
            TraceIr::StoreDynVar(dst, src) => {
                format!("{:?} = {:?}", dst, src)
            }
            TraceIr::LoadIvar(reg, id, Some((class_id, ivar_id))) => {
                format!(
                    "{:?} = {id}: {}",
                    reg,
                    format!("{}[{:?}]", store.debug_class_name(class_id), ivar_id)
                )
            }
            TraceIr::LoadIvar(reg, id, None) => {
                format!("{:?} = {id}: {}", reg, format!("-[-]"))
            }
            TraceIr::StoreIvar(reg, id, Some((class_id, ivar_id))) => {
                format!(
                    "{id}: {} = {:?}",
                    format!("{}[{:?}]", store.debug_class_name(class_id), ivar_id),
                    reg
                )
            }
            TraceIr::StoreIvar(reg, id, None) => {
                format!("{id}: {} = {:?}", format!("-[-]"), reg)
            }
            TraceIr::LoadGvar { dst: ret, name } => {
                format!("{:?} = {name}", ret)
            }
            TraceIr::StoreGvar { src, name } => {
                format!("{name} = {:?}", src)
            }
            TraceIr::LoadCvar { dst: ret, name } => {
                format!("{:?} = {name}", ret)
            }
            TraceIr::CheckCvar { dst: ret, name } => {
                format!("{:?} = {name}?", ret)
            }
            TraceIr::StoreCvar { src, name } => {
                format!("{name} = {:?}", src)
            }
            TraceIr::LoadSvar { dst: ret, id } => {
                // 0 => $&
                // 1 => $'
                // 100 + n => $n
                format!(
                    "{:?} = ${}",
                    ret,
                    match id {
                        ruruby_parse::SPECIAL_LASTMATCH => "&".to_string(),
                        ruruby_parse::SPECIAL_POSTMATCH => "'".to_string(),
                        ruruby_parse::SPECIAL_LOADPATH => "$LOAD_PATH".to_string(),
                        ruruby_parse::SPECIAL_LOADEDFEATURES => "$LOADED_FEATURES".to_string(),
                        n if n >= 100 => (n - 100).to_string(),
                        _ => unreachable!(),
                    }
                )
            }
            TraceIr::UnOp {
                kind,
                dst,
                src,
                ic: src_class,
            } => {
                let op1 = format!("{:?} = {}{:?}", dst, kind, src);
                format!("{:36} [{}]", op1, store.debug_class_name(src_class),)
            }
            TraceIr::Not { dst, src } => {
                let op1 = format!("{:?} = !{:?}", dst, src);
                format!("{:36}", op1)
            }

            TraceIr::BinOp {
                kind,
                dst,
                lhs,
                rhs,
                ic,
            } => binop_fmt(store, kind, dst, lhs, rhs, ic.clone()),

            TraceIr::BinCmp {
                kind,
                dst,
                lhs,
                rhs,
                ic,
            } => cmp_fmt(store, kind, dst, lhs, rhs, ic.clone(), false),
            TraceIr::BinCmpBr {
                kind,
                _dst: dst,
                lhs,
                rhs,
                ic,
                ..
            } => cmp_fmt(store, kind, dst, lhs, rhs, ic.clone(), true),

            TraceIr::ArrayTEq { lhs, rhs } => {
                format!("{lhs:?} = *{lhs:?} === {rhs:?}")
            }

            TraceIr::Ret(reg) => format!("ret {:?}", reg),
            TraceIr::MethodRet(reg) => format!("method_ret {:?}", reg),
            TraceIr::BlockBreak(reg) => format!("break {:?}", reg),
            TraceIr::Raise(reg) => format!("raise {:?}", reg),
            TraceIr::EnsureEnd => format!("ensure_end"),
            TraceIr::ToA { dst, src } => format!("{dst:?} = {src:?}.to_a"),
            TraceIr::Mov(dst, src) => format!("{dst:?} = {src:?}"),
            TraceIr::MethodCall {
                _polymorphic: polymorphic,
                callid,
                cache,
            } => {
                let callsite = &store[callid];
                let name = if let Some(name) = callsite.name {
                    name.to_string()
                } else {
                    "super".to_string()
                };
                let CallSiteInfo { recv, dst, .. } = callsite;
                let s = callsite.format_args();
                let op1 = format!("{} = {:?}.{name}{s}", ret_str(*dst), recv);
                format!(
                    "{:36} {}[{}] {}",
                    op1,
                    if polymorphic { "POLYMORPHIC " } else { "" },
                    store.debug_class_name(if let Some(entry) = &cache {
                        Some(entry.recv_class)
                    } else {
                        None
                    }),
                    if let Some(entry) = cache {
                        format!("{:?}", entry.func_id)
                    } else {
                        "-".to_string()
                    }
                )
            }
            TraceIr::Yield { callid } => {
                let dst = store[callid].dst;
                let s = store[callid].format_args();
                format!("{} = yield{s}", ret_str(dst))
            }
            TraceIr::InlineCache => return None,
            TraceIr::MethodDef { name, func_id } => {
                format!("method_def {name}: {:?}", func_id)
            }
            TraceIr::SingletonMethodDef { obj, name, func_id } => {
                format!("singleton_method_def {:?}.{name}: {:?}", obj, func_id)
            }
            TraceIr::ClassDef {
                dst,
                base,
                superclass,
                name,
                func_id,
            } => {
                format!(
                    "{} = class_def {}{name}{}: {:?}",
                    ret_str(dst),
                    if let Some(base) = base {
                        format!("{:?}::", base)
                    } else {
                        "".to_string()
                    },
                    if let Some(superclass) = superclass {
                        format!(" < {:?}", superclass)
                    } else {
                        "".to_string()
                    },
                    func_id
                )
            }
            TraceIr::ModuleDef {
                dst,
                base,
                name,
                func_id,
            } => {
                format!(
                    "{} = module_def {}{name}: {:?}",
                    ret_str(dst),
                    if let Some(base) = base {
                        format!("{:?}::", base)
                    } else {
                        "".to_string()
                    },
                    func_id
                )
            }
            TraceIr::SingletonClassDef {
                dst: ret,
                base,
                func_id,
            } => {
                format!(
                    "{} = singleton_class_def << {:?}: {:?}",
                    ret_str(ret),
                    base,
                    func_id
                )
            }
            TraceIr::ConcatStr(ret, args, len) => {
                format!("{} = concat({:?}; {})", ret_str(ret), args, len)
            }
            TraceIr::ConcatRegexp(ret, args, len) => {
                format!("{} = concat_regexp({:?}; {})", ret_str(ret), args, len)
            }
            TraceIr::ExpandArray {
                src,
                dst: (dst, len, rest_pos),
            } => {
                let mut s = String::new();
                for i in 0..len {
                    let prefix = if rest_pos == Some(i) { "*" } else { "" };
                    if i != 0 {
                        s += ",";
                    }
                    s += &format!("{prefix}{:?}", dst + i);
                }
                format!("{s} = expand({src:?})")
            }
            TraceIr::UndefMethod { undef } => {
                format!("undef_method({:?})", undef)
            }
            TraceIr::AliasMethod { new, old } => {
                format!("alias_method({:?}<-{:?})", new, old)
            }
            TraceIr::DefinedYield { dst } => format!("{:?} = defined?(yield)", dst),
            TraceIr::DefinedConst { dst, siteid } => {
                let s = store[siteid].format();
                format!("{:?} = defined?(constant) {s}", dst)
            }
            TraceIr::DefinedMethod { dst, recv, name } => {
                format!("{:?} = defined?(method) {:?}.{}", dst, recv, name)
            }
            TraceIr::DefinedSuper { dst } => {
                format!("{:?} = defined?(super)", dst)
            }
            TraceIr::DefinedGvar { dst, name } => {
                format!("{:?} = defined?(gvar) {}", dst, name)
            }
            TraceIr::DefinedIvar { dst, name } => {
                format!("{:?} = defined?(ivar) {}", dst, name)
            }
            TraceIr::LoopStart { counter, jit_addr } => {
                format!("loop_start counter={counter} jit-addr={:?}", jit_addr)
            }
            TraceIr::LoopEnd => "loop_end".to_string(),
        };
        Some(s)
    }
}
