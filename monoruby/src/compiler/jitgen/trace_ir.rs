use super::*;
use crate::bytecodegen::{inst::*, BinOpK, UnOpK};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum OpMode {
    RR(SlotId, SlotId),
    RI(SlotId, i16),
    IR(i16, SlotId),
}

///
/// IR for JIT compiler.
///
#[derive(Debug, Clone)]
pub(crate) enum TraceIr {
    /// branch(dest)
    Br(BasicBlockId),
    /// conditional branch(%reg, dest, optimizable)  : branch when reg was true.
    CondBr(SlotId, BasicBlockId, bool, BrKind),
    /// conditional branch(%reg, dest)  : branch when reg is nil.
    NilBr(SlotId, BasicBlockId),
    /// check local var(%reg, dest)  : branch when reg was None.
    OptCase {
        cond: SlotId,
        min: u16,
        max: u16,
        dest_bb: Box<[BasicBlockId]>,
        branch_table: Box<[BasicBlockId]>,
    },
    CheckLocal(SlotId, BasicBlockId),
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
    Index {
        dst: SlotId,
        base: SlotId,
        idx: SlotId,
        base_class: Option<ClassId>,
        idx_class: Option<ClassId>,
    },
    IndexAssign {
        src: SlotId,
        base: SlotId,
        idx: SlotId,
        base_class: Option<ClassId>,
        idx_class: Option<ClassId>,
    },
    LoadConst(SlotId, ConstSiteId),
    StoreConst(SlotId, ConstSiteId),
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
    Nil(SlotId),
    BitNot {
        dst: SlotId,
        src: SlotId,
        src_class: Option<ClassId>,
    },
    UnOp {
        kind: UnOpK,
        dst: SlotId,
        src: SlotId,
        src_class: Option<ClassId>,
    },
    IUnOp {
        kind: UnOpK,
        dst: SlotId,
        src: SlotId,
    },
    FUnOp {
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
        lhs_class: Option<ClassId>,
        rhs_class: Option<ClassId>,
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
        lhs_class: ClassId,
        rhs_class: ClassId,
    },

    Cmp {
        kind: ruruby_parse::CmpKind,
        dst: Option<SlotId>,
        mode: OpMode,
        lhs_class: Option<ClassId>,
        rhs_class: Option<ClassId>,
    },
    ICmp {
        kind: ruruby_parse::CmpKind,
        dst: Option<SlotId>,
        mode: OpMode,
    },
    FCmp {
        kind: ruruby_parse::CmpKind,
        dst: Option<SlotId>,
        mode: OpMode,
        lhs_class: ClassId,
        rhs_class: ClassId,
    },

    CmpBr {
        kind: ruruby_parse::CmpKind,
        dst: Option<SlotId>,
        mode: OpMode,
        lhs_class: Option<ClassId>,
        rhs_class: Option<ClassId>,
        dest: BasicBlockId,
        brkind: BrKind,
    },
    ICmpBr {
        kind: ruruby_parse::CmpKind,
        dst: Option<SlotId>,
        mode: OpMode,
        dest: BasicBlockId,
        brkind: BrKind,
    },
    FCmpBr {
        kind: ruruby_parse::CmpKind,
        dst: Option<SlotId>,
        mode: OpMode,
        lhs_class: ClassId,
        rhs_class: ClassId,
        dest: BasicBlockId,
        brkind: BrKind,
    },

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
        recv_class: Option<ClassId>,
        fid: Option<FuncId>,
        version: u32,
    },
    MethodCallWithBlock {
        callid: CallSiteId,
        recv_class: Option<ClassId>,
        fid: Option<FuncId>,
        version: u32,
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
        dst: (SlotId, u16),
    },
    AliasMethod {
        new: IdentId,
        old: IdentId,
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
    #[allow(dead_code)]
    LoopStart {
        counter: u32,
        jit_addr: *const u8,
    },
    LoopEnd,
}

/*enum DefKind<'a> {
    None,
    Reg { slot: &'a SlotId, is_float: bool },
    Range { start: &'a SlotId, len: u16 },
}

impl<'a> DefKind<'a> {
    fn reg(slot: &'a SlotId, is_float: bool) -> Self {
        Self::Reg { slot, is_float }
    }

    fn range(start: &'a SlotId, len: u16) -> Self {
        Self::Range { start, len }
    }

    fn from_slot(slot: &'a Option<SlotId>, is_float: bool) -> Self {
        if let Some(slot) = slot {
            Self::Reg { slot, is_float }
        } else {
            Self::None
        }
    }
}*/

impl TraceIr {
    pub(crate) fn get_exit_type(&self) -> Option<ExitType> {
        match self {
            TraceIr::Ret(..) | TraceIr::MethodRet(..) | TraceIr::Break(..) | TraceIr::Raise(..) => {
                return Some(ExitType::Return);
            }
            TraceIr::Br(_)
            | TraceIr::CondBr(..)
            | TraceIr::NilBr(..)
            | TraceIr::CheckLocal(..)
            | TraceIr::OptCase { .. } => {
                return Some(ExitType::Continue);
            }
            _ => {}
        }
        None
    }

    /*pub(crate) fn get_def(&self, store: &Store) -> DefKind {
        match self {
            TraceIr::DefinedYield { dst }
            | TraceIr::DefinedConst { dst, .. }
            | TraceIr::DefinedGvar { dst, .. }
            | TraceIr::DefinedIvar { dst, .. }
            | TraceIr::Integer(dst, ..)
            | TraceIr::Symbol(dst, ..)
            | TraceIr::Nil(dst)
            | TraceIr::DefinedMethod { dst, .. }
            | TraceIr::Array { dst, .. }
            | TraceIr::Hash { dst, .. }
            | TraceIr::Range { dst, .. }
            | TraceIr::Index { dst, .. }
            | TraceIr::Lambda { dst, .. }
            | TraceIr::BlockArgProxy(dst, _)
            | TraceIr::BlockArg(dst, _)
            | TraceIr::LoadDynVar(dst, ..)
            | TraceIr::LoadIvar(dst, ..)
            | TraceIr::LoadCvar { dst, .. }
            | TraceIr::CheckCvar { dst, .. }
            | TraceIr::LoadGvar { dst, .. }
            | TraceIr::LoadSvar { dst, .. }
            | TraceIr::BitNot { dst, .. }
            | TraceIr::Not { dst, .. } => DefKind::reg(dst, false),
            TraceIr::Literal(dst, val) => {
                let is_float = val.class() == FLOAT_CLASS;
                DefKind::reg(dst, is_float)
            }
            TraceIr::ClassDef { dst, .. }
            | TraceIr::ModuleDef { dst, .. }
            | TraceIr::SingletonClassDef { dst, .. } => DefKind::from_slot(dst, false),
            TraceIr::LoadConst(dst, constsite) => {
                let is_float = if let Some(value) = store[*constsite].cache.2 {
                    value.is_float()
                } else {
                    false
                };
                DefKind::reg(dst, is_float)
            }
            TraceIr::UnOp { kind: _, dst, src } => {
                let is_float = pc.is_float1();
                &[(dst, is_float)]
            }
            TraceIr::FBinOp { dst, mode, .. } => DefKind::from_slot(dst, true),
            TraceIr::IBinOp { dst, .. }
            | TraceIr::BinOp { dst, .. }
            | TraceIr::IBinOp { dst, .. }
            | TraceIr::IBinOp { dst, .. }
            | TraceIr::BinOp { dst, .. }
            | TraceIr::BinOp { dst, .. }
            | TraceIr::Cmp(_, dst, ..) => DefKind::from_slot(dst, false),
            TraceIr::Mov(dst, src) => {
                info.copy(dst, src);
            }
            TraceIr::ConcatStr(dst, ..) | TraceIr::ConcatRegexp(dst, ..) => {
                DefKind::from_slot(dst, false)
            }
            TraceIr::ExpandArray {
                dst: (dst, len), ..
            } => DefKind::range(dst, *len),
            TraceIr::Yield { callid }
            | TraceIr::MethodCall { callid, .. }
            | TraceIr::MethodCallBlock { callid, .. } => {
                let CallSiteInfo { dst, .. } = &store[*callid];
                DefKind::from_slot(dst, false)
            }
            TraceIr::InlineCall { inline_id, callid }
            | TraceIr::InlineObjectSend { inline_id, callid }
            | TraceIr::InlineObjectSendSplat { inline_id, callid } => {
                (store.get_inline_info(inline_id).inline_analysis)(&mut info, &store[callid]);
            }
            _ => DefKind::None,
        }
    }*/
}

impl TraceIr {
    #[cfg(feature = "dump-bc")]
    pub(crate) fn format(&self, store: &Store) -> Option<String> {
        fn optstr(opt: bool) -> &'static str {
            if opt {
                "_"
            } else {
                ""
            }
        }

        fn ret_str(slot: Option<SlotId>) -> String {
            match slot {
                None => "_".to_string(),
                Some(ret) => format!("{:?}", ret),
            }
        }

        fn fmt(
            store: &Store,
            s: String,
            lhs_class: Option<ClassId>,
            rhs_class: Option<ClassId>,
        ) -> String {
            format!(
                "{:36} [{}][{}]",
                s,
                store.debug_class_name(lhs_class),
                store.debug_class_name(rhs_class)
            )
        }

        fn cmp_fmt(
            store: &Store,
            kind: ruruby_parse::CmpKind,
            dst: Option<SlotId>,
            mode: &OpMode,
            lhs_class: Option<ClassId>,
            rhs_class: Option<ClassId>,
            optimizable: bool,
        ) -> String {
            let s = match mode {
                OpMode::RR(lhs, rhs) => {
                    format!(
                        "{}{} = {:?} {:?} {:?}",
                        optstr(optimizable),
                        ret_str(dst),
                        lhs,
                        kind,
                        rhs,
                    )
                }
                OpMode::RI(lhs, rhs) => format!(
                    "{}{} = {:?} {:?} {}: i16",
                    optstr(optimizable),
                    ret_str(dst),
                    lhs,
                    kind,
                    rhs,
                ),
                _ => unreachable!(),
            };
            fmt(store, s, lhs_class, rhs_class)
        }

        fn binop_fmt(
            store: &Store,
            kind: BinOpK,
            dst: Option<SlotId>,
            mode: &OpMode,
            lhs_class: Option<ClassId>,
            rhs_class: Option<ClassId>,
        ) -> String {
            let (op1, lhs_class, rhs_class) = match mode {
                OpMode::RR(lhs, rhs) => (
                    format!("{} = {:?} {} {:?}", ret_str(dst), lhs, kind, rhs),
                    lhs_class,
                    rhs_class,
                ),
                OpMode::RI(lhs, rhs) => (
                    format!("{} = {:?} {} {}: i16", ret_str(dst), lhs, kind, rhs),
                    lhs_class,
                    Some(INTEGER_CLASS),
                ),
                OpMode::IR(lhs, rhs) => (
                    format!("{} = {}: i16 {} {:?}", ret_str(dst), lhs, kind, rhs),
                    Some(INTEGER_CLASS),
                    rhs_class,
                ),
            };
            fmt(store, op1, lhs_class, rhs_class)
        }

        let s = match self {
            TraceIr::InitMethod(info) => {
                format!("init_method {info:?}")
            }
            TraceIr::CheckLocal(local, dest) => {
                format!("check_local({:?}) =>{:?}", local, dest)
            }
            TraceIr::Br(dest) => {
                format!("br => {:?}", dest)
            }
            TraceIr::CondBr(reg, dest, opt, kind) => {
                format!(
                    "cond{}br {}{:?} => {:?}",
                    kind.to_s(),
                    optstr(*opt),
                    reg,
                    dest
                )
            }
            TraceIr::NilBr(reg, dest) => {
                format!("nilbr {:?} => {:?}", reg, dest)
            }
            TraceIr::OptCase {
                cond,
                min,
                max,
                branch_table,
                ..
            } => {
                format!(
                    "opt_case {:?}: {min}..{max} -> branch_table:{:?}",
                    cond, branch_table
                )
            }
            TraceIr::Integer(reg, num) => format!("{:?} = {}: i32", reg, num),
            TraceIr::Symbol(reg, id) => format!("{:?} = :{id}", reg),
            TraceIr::Range {
                dst,
                start,
                end,
                exclude_end,
            } => format!(
                "{:?} = {:?} {} {:?}",
                dst,
                start,
                if *exclude_end { "..." } else { ".." },
                end
            ),
            TraceIr::Literal(reg, val) => {
                format!("{:?} = literal[{}]", reg, val.debug(store))
            }
            TraceIr::Array { dst, callid } => {
                let CallSiteInfo { args, pos_num, .. } = store[*callid];
                format!("{:?} = array[{:?}; {}]", dst, args, pos_num)
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
                base_class,
                idx_class,
            } => {
                let op1 = format!("{:?} = {:?}.[{:?}]", dst, base, idx);
                fmt(store, op1, *base_class, *idx_class)
            }
            TraceIr::IndexAssign {
                src,
                base,
                idx,
                base_class,
                idx_class,
            } => {
                let op1 = format!("{:?}:.[{:?}:] = {:?}", base, idx, src,);
                fmt(store, op1, *base_class, *idx_class)
            }
            TraceIr::LoadConst(reg, id) => {
                let op = store[*id].format();
                let op1 = format!("{:?} = {op}", reg);
                format!(
                    "{:36} [{}]",
                    op1,
                    match store[*id].cache.cached_value {
                        None => "<INVALID>".to_string(),
                        Some(val) => val.debug(store),
                    }
                )
            }
            TraceIr::StoreConst(src, id) => {
                let op = store[*id].format();
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
            TraceIr::LoadIvar(reg, id, class_id, ivar_id) => {
                format!(
                    "{:?} = {id}: {}",
                    reg,
                    if let Some(id) = class_id {
                        format!("{}[{:?}]", store.debug_class_name(*id), ivar_id)
                    } else {
                        format!("-")
                    }
                )
            }
            TraceIr::StoreIvar(reg, id, class_id, ivar_id) => {
                format!(
                    "{id}: {} = {:?}",
                    if let Some(id) = class_id {
                        format!("{}[{:?}]", store.debug_class_name(*id), ivar_id)
                    } else {
                        format!("-")
                    },
                    reg
                )
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
                    match *id {
                        ruruby_parse::SPECIAL_LASTMATCH => "&".to_string(),
                        ruruby_parse::SPECIAL_POSTMATCH => "'".to_string(),
                        ruruby_parse::SPECIAL_LOADPATH => "$LOAD_PATH".to_string(),
                        ruruby_parse::SPECIAL_LOADEDFEATURES => "$LOADED_FEATURES".to_string(),
                        n if n >= 100 => (n - 100).to_string(),
                        _ => unreachable!(),
                    }
                )
            }
            TraceIr::Nil(reg) => format!("{:?} = nil", reg),
            TraceIr::BitNot {
                dst,
                src,
                src_class,
            } => {
                let op1 = format!("{:?} = ~{:?}", dst, src);
                format!("{:36} [{}]", op1, store.debug_class_name(*src_class),)
            }
            TraceIr::UnOp {
                kind,
                dst,
                src,
                src_class,
            } => {
                let op1 = format!("{:?} = {}{:?}", dst, kind, src);
                format!("{:36} [{}]", op1, store.debug_class_name(*src_class),)
            }
            TraceIr::IUnOp { kind, dst, src } => {
                let op1 = format!("{:?} = {}{:?}", dst, kind, src);
                format!("{:36} [{}]", op1, store.debug_class_name(INTEGER_CLASS),)
            }
            TraceIr::FUnOp { kind, dst, src } => {
                let op1 = format!("{:?} = {}{:?}", dst, kind, src);
                format!("{:36} [{}]", op1, store.debug_class_name(FLOAT_CLASS),)
            }
            TraceIr::Not { dst, src } => {
                let op1 = format!("{:?} = !{:?}", dst, src);
                format!("{:36}", op1)
            }

            TraceIr::BinOp {
                kind,
                dst,
                mode,
                lhs_class,
                rhs_class,
            } => binop_fmt(store, *kind, *dst, mode, *lhs_class, *rhs_class),
            TraceIr::FBinOp {
                kind,
                dst,
                mode,
                lhs_class,
                rhs_class,
            } => binop_fmt(store, *kind, *dst, mode, Some(*lhs_class), Some(*rhs_class)),
            TraceIr::IBinOp { kind, dst, mode } => {
                let lhs_class = Some(INTEGER_CLASS);
                let rhs_class = Some(INTEGER_CLASS);
                binop_fmt(store, *kind, *dst, mode, lhs_class, rhs_class)
            }

            TraceIr::Cmp {
                kind,
                dst,
                mode,
                lhs_class,
                rhs_class,
            } => cmp_fmt(store, *kind, *dst, mode, *lhs_class, *rhs_class, false),
            TraceIr::CmpBr {
                kind,
                dst,
                mode,
                lhs_class,
                rhs_class,
                ..
            } => cmp_fmt(store, *kind, *dst, mode, *lhs_class, *rhs_class, true),

            TraceIr::FCmp {
                kind,
                dst,
                mode,
                lhs_class,
                rhs_class,
            } => cmp_fmt(
                store,
                *kind,
                *dst,
                mode,
                Some(*lhs_class),
                Some(*rhs_class),
                false,
            ),
            TraceIr::FCmpBr {
                kind,
                dst,
                mode,
                lhs_class,
                rhs_class,
                ..
            } => cmp_fmt(
                store,
                *kind,
                *dst,
                mode,
                Some(*lhs_class),
                Some(*rhs_class),
                true,
            ),

            TraceIr::ICmp { kind, dst, mode } => {
                let lhs_class = Some(INTEGER_CLASS);
                let rhs_class = Some(INTEGER_CLASS);
                cmp_fmt(store, *kind, *dst, mode, lhs_class, rhs_class, false)
            }
            TraceIr::ICmpBr {
                kind, dst, mode, ..
            } => {
                let lhs_class = Some(INTEGER_CLASS);
                let rhs_class = Some(INTEGER_CLASS);
                cmp_fmt(store, *kind, *dst, mode, lhs_class, rhs_class, true)
            }

            TraceIr::Ret(reg) => format!("ret {:?}", reg),
            TraceIr::MethodRet(reg) => format!("method_ret {:?}", reg),
            TraceIr::Break(reg) => format!("break {:?}", reg),
            TraceIr::Raise(reg) => format!("raise {:?}", reg),
            TraceIr::EnsureEnd => format!("ensure_end"),
            TraceIr::Mov(dst, src) => format!("{:?} = {:?}", dst, src),
            TraceIr::MethodCall {
                callid,
                recv_class,
                fid,
                ..
            }
            | TraceIr::MethodCallWithBlock {
                callid,
                recv_class,
                fid,
                ..
            } => {
                let callsite = &store[*callid];
                if callsite.block_fid.is_none()
                    && let Some(fid) = fid
                    && let Some(inline_info) = store.inline_info.get_inline(*fid)
                    && (*fid == OBJECT_SEND_FUNCID && callsite.object_send_single_splat()
                        || callsite.is_simple())
                {
                    let CallSiteInfo { recv, dst, .. } = *callsite;
                    let name = &inline_info.name;
                    let s = callsite.format_args();
                    let op1 = format!("{} = {:?}.inline {name}{s}", ret_str(dst), recv,);
                    format!("{:36} [{}]", op1, store.debug_class_name(*recv_class))
                } else {
                    let name = if let Some(name) = callsite.name {
                        name.to_string()
                    } else {
                        "super".to_string()
                    };
                    let CallSiteInfo { recv, dst, .. } = callsite;
                    let s = callsite.format_args();
                    let op1 = format!("{} = {:?}.{name}{s}", ret_str(*dst), recv,);
                    format!("{:36} [{}]", op1, store.debug_class_name(*recv_class),)
                }
            }
            TraceIr::Yield { callid } => {
                let dst = store[*callid].dst;
                let s = store[*callid].format_args();
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
                    ret_str(*dst),
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
                    ret_str(*dst),
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
                    ret_str(*ret),
                    base,
                    func_id
                )
            }
            TraceIr::ConcatStr(ret, args, len) => {
                format!("{} = concat({:?}; {})", ret_str(*ret), args, len)
            }
            TraceIr::ConcatRegexp(ret, args, len) => {
                format!("{} = concat_regexp({:?}; {})", ret_str(*ret), args, len)
            }
            TraceIr::ExpandArray {
                src,
                dst: (dst, len),
            } => {
                format!("{:?}; {} = expand({:?})", dst, len, src)
            }
            TraceIr::AliasMethod { new, old } => {
                format!("alias_method({:?}<-{:?})", new, old)
            }
            TraceIr::DefinedYield { dst } => format!("{:?} = defined?(yield)", dst),
            TraceIr::DefinedConst { dst, siteid } => {
                let s = store[*siteid].format();
                format!("{:?} = defined?(constant) {s}", dst)
            }
            TraceIr::DefinedMethod {
                dst: ret,
                recv,
                name,
            } => {
                format!("{:?} = defined?(method) {:?}.{}", ret, recv, name)
            }
            TraceIr::DefinedGvar { dst: ret, name } => {
                format!("{:?} = defined?(gvar) {}", ret, name)
            }
            TraceIr::DefinedIvar { dst: ret, name } => {
                format!("{:?} = defined?(ivar) {}", ret, name)
            }
            TraceIr::LoopStart { counter, jit_addr } => {
                format!("loop_start counter={counter} jit-addr={:?}", jit_addr)
            }
            TraceIr::LoopEnd => "loop_end".to_string(),
        };
        Some(s)
    }
}
