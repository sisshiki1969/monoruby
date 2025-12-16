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
    pub dst: Option<SlotId>,
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
        else_dest: BasicBlockId,
        branch_table: Box<[BasicBlockId]>,
    },
    CheckLocal(SlotId, BasicBlockId),
    CheckKwRest(SlotId),
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
    Nil(SlotId),
    Not {
        dst: SlotId,
        src: SlotId,
    },
    BitNot {
        dst: SlotId,
        src: SlotId,
        ic: Option<ClassId>,
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
        dst: Option<SlotId>,
        lhs: SlotId,
        rhs: SlotId,
        dest_bb: BasicBlockId,
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
        src: SlotId,
        base: SlotId,
        idx: SlotId,
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
    #[allow(dead_code)]
    LoopStart {
        counter: u32,
        jit_addr: *const u8,
    },
    LoopEnd,
}

impl TraceIr {
    pub(crate) fn format(&self, store: &Store) -> Option<String> {
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

        let s = match self {
            TraceIr::InitMethod(info) => {
                format!("init_method {info:?}")
            }
            TraceIr::CheckLocal(local, dest) => {
                format!("check_local({:?}) =>{:?}", local, dest)
            }
            TraceIr::CheckKwRest(local) => {
                format!("check_kw_rest({:?})", local)
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
                else_dest,
                branch_table,
            } => {
                format!(
                    "opt_case {cond:?}: else -> {else_dest:?}  {min}..{max} -> branch_table:{branch_table:?}",
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
                let CallSiteInfo {
                    args,
                    pos_num,
                    splat_pos,
                    ..
                } = &store[*callid];
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
                fmt(store, op1, *class)
            }
            TraceIr::IndexAssign {
                src,
                base,
                idx,
                class,
            } => {
                let op1 = format!("{:?}:.[{:?}:] = {:?}", base, idx, src,);
                fmt(store, op1, *class)
            }
            TraceIr::LoadConst(reg, id) => {
                let op = store[*id].format();
                let op1 = format!("{:?} = {op}", reg);
                format!(
                    "{:36} [{}]",
                    op1,
                    match &store[*id].cache {
                        None => "<INVALID>".to_string(),
                        Some(cache) => cache.value.debug(store),
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
            TraceIr::LoadIvar(reg, id, Some((class_id, ivar_id))) => {
                format!(
                    "{:?} = {id}: {}",
                    reg,
                    format!("{}[{:?}]", store.debug_class_name(*class_id), ivar_id)
                )
            }
            TraceIr::LoadIvar(reg, id, None) => {
                format!("{:?} = {id}: {}", reg, format!("-[-]"))
            }
            TraceIr::StoreIvar(reg, id, Some((class_id, ivar_id))) => {
                format!(
                    "{id}: {} = {:?}",
                    format!("{}[{:?}]", store.debug_class_name(*class_id), ivar_id),
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
                ic: src_class,
            } => {
                let op1 = format!("{:?} = ~{:?}", dst, src);
                format!("{:36} [{}]", op1, store.debug_class_name(*src_class),)
            }
            TraceIr::UnOp {
                kind,
                dst,
                src,
                ic: src_class,
            } => {
                let op1 = format!("{:?} = {}{:?}", dst, kind, src);
                format!("{:36} [{}]", op1, store.debug_class_name(*src_class),)
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
            } => binop_fmt(store, *kind, *dst, *lhs, *rhs, ic.clone()),

            TraceIr::BinCmp {
                kind,
                dst,
                lhs,
                rhs,
                ic,
            } => cmp_fmt(store, *kind, *dst, *lhs, *rhs, ic.clone(), false),
            TraceIr::BinCmpBr {
                kind,
                dst,
                lhs,
                rhs,
                ic,
                ..
            } => cmp_fmt(store, *kind, *dst, *lhs, *rhs, ic.clone(), true),

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
                let callsite = &store[*callid];
                let name = if let Some(name) = callsite.name {
                    name.to_string()
                } else {
                    "super".to_string()
                };
                let CallSiteInfo { recv, dst, .. } = callsite;
                let s = callsite.format_args();
                let op1 = format!("{} = {:?}.{name}{s}", ret_str(*dst), recv,);
                format!(
                    "{:36} {}[{}] {}",
                    op1,
                    if *polymorphic { "POLYMORPHIC " } else { "" },
                    store.debug_class_name(if let Some(entry) = cache {
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
                dst: (dst, len, rest_pos),
            } => {
                let mut s = String::new();
                for i in 0..*len {
                    let prefix = if rest_pos == &Some(i) { "*" } else { "" };
                    if i != 0 {
                        s += ",";
                    }
                    s += &format!("{prefix}{:?}", *dst + i);
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
                let s = store[*siteid].format();
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
