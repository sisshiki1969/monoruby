use inline::InlineTable;
use monoasm::DestLabel;
use ruruby_parse::{LvarCollector, ParseResult};

use crate::compiler::jitgen::trace_ir::MethodCacheEntry;

use super::*;
use std::{cell::RefCell, pin::Pin};

mod class;
mod function;
mod iseq;
pub use class::*;
pub use function::*;
pub(crate) use iseq::*;

thread_local! {
    pub static GLOBAL_METHOD_CACHE: RefCell<GlobalMethodCache> = RefCell::new(GlobalMethodCache::default());
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct MethodTableEntry {
    owner: ClassId,
    func_id: Option<FuncId>,
    visibility: Visibility,
    is_basic_op: bool,
}

impl MethodTableEntry {
    pub fn func_id(&self) -> Option<FuncId> {
        self.func_id
    }

    pub fn visibility(&self) -> Visibility {
        self.visibility
    }

    pub fn is_public(&self) -> bool {
        self.func_id.is_some() && self.visibility == Visibility::Public
    }

    pub fn is_private(&self) -> bool {
        self.func_id.is_some() && self.visibility == Visibility::Private
    }

    pub fn is_public_protected(&self) -> bool {
        self.func_id.is_some()
            && matches!(self.visibility, Visibility::Protected | Visibility::Public)
    }
}

pub struct Store {
    /// function info.
    pub(crate) functions: function::Funcs,
    /// ISEQ info.
    pub(crate) iseqs: Vec<ISeqInfo>,
    /// class table.
    classes: ClassInfoTable,
    /// call site info.
    callsite_info: Vec<CallSiteInfo>,
    /// const access site info.
    constsite_info: Vec<ConstSiteInfo>,
    /// opt case branch info.
    optcase_info: Vec<OptCaseInfo>,
    /// inline info.
    pub(crate) inline_info: InlineTable,
}

impl std::ops::Deref for Store {
    type Target = ClassInfoTable;
    fn deref(&self) -> &Self::Target {
        &self.classes
    }
}

impl std::ops::DerefMut for Store {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.classes
    }
}

impl std::ops::Index<FuncId> for Store {
    type Output = FuncInfo;
    fn index(&self, index: FuncId) -> &FuncInfo {
        &self.functions[index]
    }
}

impl std::ops::IndexMut<FuncId> for Store {
    fn index_mut(&mut self, index: FuncId) -> &mut FuncInfo {
        &mut self.functions[index]
    }
}

impl std::ops::Index<ISeqId> for Store {
    type Output = ISeqInfo;
    fn index(&self, index: ISeqId) -> &ISeqInfo {
        &self.iseqs[index.get()]
    }
}

impl std::ops::IndexMut<ISeqId> for Store {
    fn index_mut(&mut self, index: ISeqId) -> &mut ISeqInfo {
        &mut self.iseqs[index.get()]
    }
}

impl std::ops::Index<ClassId> for Store {
    type Output = ClassInfo;
    fn index(&self, index: ClassId) -> &ClassInfo {
        &self.classes[index]
    }
}

impl std::ops::IndexMut<ClassId> for Store {
    fn index_mut(&mut self, index: ClassId) -> &mut ClassInfo {
        &mut self.classes[index]
    }
}

impl std::ops::Index<ConstSiteId> for Store {
    type Output = ConstSiteInfo;
    fn index(&self, index: ConstSiteId) -> &ConstSiteInfo {
        &self.constsite_info[index.0 as usize]
    }
}

impl std::ops::IndexMut<ConstSiteId> for Store {
    fn index_mut(&mut self, index: ConstSiteId) -> &mut ConstSiteInfo {
        &mut self.constsite_info[index.0 as usize]
    }
}

impl std::ops::Index<CallSiteId> for Store {
    type Output = CallSiteInfo;
    fn index(&self, index: CallSiteId) -> &CallSiteInfo {
        &self.callsite_info[index.0 as usize]
    }
}

impl std::ops::IndexMut<CallSiteId> for Store {
    fn index_mut(&mut self, index: CallSiteId) -> &mut CallSiteInfo {
        &mut self.callsite_info[index.0 as usize]
    }
}

//impl std::ops::IndexMut<CallSiteId> for Store {
//    fn index_mut(&mut self, index: CallSiteId) -> &mut CallSiteInfo {
//        &mut self.callsite_info[index.0 as usize]
//    }
//}

impl std::ops::Index<OptCaseId> for Store {
    type Output = OptCaseInfo;
    fn index(&self, index: OptCaseId) -> &Self::Output {
        &self.optcase_info[index.0 as usize]
    }
}

impl alloc::GC<RValue> for Store {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.functions.mark(alloc);
        self.iseqs.iter().for_each(|info| info.mark(alloc));
        self.constsite_info.iter().for_each(|info| info.mark(alloc));
        self.classes.table.iter().for_each(|info| info.mark(alloc));
    }
}

impl Store {
    pub(super) fn new() -> Self {
        Self {
            functions: function::Funcs::default(),
            iseqs: vec![],
            constsite_info: vec![],
            callsite_info: vec![],
            optcase_info: vec![],
            classes: ClassInfoTable::new(),
            inline_info: InlineTable::default(),
        }
    }

    pub fn iseq(&self, func_id: FuncId) -> &ISeqInfo {
        let iseq = self[func_id].as_iseq();
        &self[iseq]
    }

    pub fn iseq_mut(&mut self, func_id: FuncId) -> &mut ISeqInfo {
        let iseq = self[func_id].as_iseq();
        &mut self[iseq]
    }

    pub fn ancestors(&self, class_id: ClassId) -> Vec<Module> {
        let mut class = self[class_id].get_module();
        let mut v = vec![class];
        while let Some(super_class) = class.superclass() {
            v.push(super_class);
            class = super_class;
        }
        v
    }

    pub fn show_ancestors(&self, class_id: ClassId) {
        let mut class = self[class_id].get_module();
        eprint!(
            "ancestors of {:?}<{}>: ",
            class.id(),
            class.id().get_name(self),
        );
        while let Some(class_) = class.superclass() {
            eprint!("{:?}<{}> ", class_.id(), class_.id().get_name(self),);
            class = class_;
        }
        eprintln!("");
    }
}

impl Store {
    #[cfg(feature = "emit-bc")]
    pub(super) fn functions(&self) -> &[FuncInfo] {
        self.functions.functions()
    }

    pub(crate) fn func_len(&self) -> usize {
        self.functions.function_len()
    }

    /// Get class name of *ClassId*.
    pub(crate) fn get_class_name(&self, class: ClassId) -> String {
        let class_obj = self.classes[class].get_module();
        match self.classes[class].get_name() {
            Some(_) => {
                let v: Vec<_> = self.classes.get_parents(class).into_iter().rev().collect();
                v.join("::")
            }
            None => match class_obj.is_singleton() {
                None => format!("#<Class:{:016x}>", class_obj.as_val().id()),
                Some(base) => format!("#<Class:{}>", base.to_s(self)),
            },
        }
    }

    ///
    /// Get the description of the function *func_id*.
    ///
    /// If the function is a method, the description is "*class_name*#*method_name*".
    /// if the function is a block, the description is "block in *method_name*".
    ///
    pub(crate) fn func_description(&self, func_id: FuncId) -> String {
        let info = &self[func_id];
        if let Some(iseq) = info.is_iseq() {
            let iseq = &self[iseq];
            let mother = iseq.mother().0;
            if mother != func_id {
                format!("block in {}", self.func_description(mother))
            } else {
                match info.owner_class() {
                    Some(owner) => format!("{}#{}", owner.get_name(self), iseq.name()),
                    None => iseq.name().to_string(),
                }
            }
        } else {
            let name = if let Some(name) = info.name() {
                format!("{:?}", name)
            } else {
                String::new()
            };
            match info.owner_class() {
                Some(owner) => format!("{}#{name}", owner.get_name(self)),
                None => name.to_string(),
            }
        }
    }

    pub fn location(&self, func_id: Option<FuncId>, source: &SourceInfoRef, loc: Loc) -> String {
        let source = source.clone();
        if let Some(func_id) = func_id {
            format!(
                "{}:{}:in '{}'",
                source.file_name(),
                source.get_line(&loc),
                self.func_description(func_id)
            )
        } else {
            format!("{}:{}", source.file_name(), source.get_line(&loc))
        }
    }

    pub fn internal_location(&self, func_id: FuncId) -> String {
        format!("<internal>:in '{}'", self.func_description(func_id))
    }

    fn new_iseq(&mut self, info: ISeqInfo) -> ISeqId {
        let id = self.iseqs.len();
        self.iseqs.push(info);
        ISeqId::new(id)
    }

    pub(crate) fn new_main(&mut self, result: ParseResult) -> Result<FuncId> {
        self.new_iseq_method(
            Some(IdentId::get_id("/main")),
            BlockInfo {
                params: vec![],
                body: Box::new(result.node),
                lvar: LvarCollector::new(),
                loc: Loc::default(),
            },
            Loc::default(),
            result.source_info,
            true,
        )
    }

    pub fn new_classdef(
        &mut self,
        name: Option<IdentId>,
        info: BlockInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        let func_id = self.functions.add_classdef(info)?;
        let info = ISeqInfo::new_method(func_id, name, ParamsInfo::default(), loc, sourceinfo);
        let iseq = self.new_iseq(info);
        let info = FuncInfo::new_classdef_iseq(name, func_id, iseq);
        self.functions.info.push(info);
        Ok(func_id)
    }

    pub fn new_iseq_method(
        &mut self,
        name: Option<IdentId>,
        info: BlockInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
        top_level: bool,
    ) -> Result<FuncId> {
        let (func_id, params) = self.functions.add_iseq_method(info)?;
        let info = ISeqInfo::new_method(func_id, name, params.clone(), loc, sourceinfo);
        let iseq = self.new_iseq(info);
        let info = FuncInfo::new_method_iseq(name, func_id, iseq, params, top_level);
        self.functions.info.push(info);
        Ok(func_id)
    }

    pub(crate) fn new_block(
        &mut self,
        mother: (FuncId, usize),
        outer: (FuncId, ExternalContext),
        optional_params: Vec<(usize, bytecodegen::BcLocal, IdentId)>,
        is_block_style: bool,
        info: BlockInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        let (func_id, params) = self.functions.add_block(optional_params, info)?;
        let info = ISeqInfo::new_block(func_id, mother, outer, params.clone(), loc, sourceinfo);
        let iseq = self.new_iseq(info);
        let info = FuncInfo::new_block_iseq(func_id, iseq, params, is_block_style);
        self.functions.info.push(info);
        Ok(func_id)
    }

    pub(crate) fn new_eval(
        &mut self,
        mother: (FuncId, usize),
        result: ParseResult,
        outer: (FuncId, ExternalContext),
        loc: Loc,
    ) -> Result<FuncId> {
        let info = BlockInfo {
            params: vec![],
            body: Box::new(result.node),
            lvar: LvarCollector::new(),
            loc,
        };
        self.new_block(mother, outer, vec![], false, info, loc, result.source_info)
    }

    pub(super) fn new_builtin_func(
        &mut self,
        name: &str,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
        kw: &[&str],
        kw_rest: bool,
    ) -> FuncId {
        self.functions
            .new_native_func(name.to_string(), address, min, max, rest, kw, kw_rest)
    }

    pub(super) fn new_basic_op(
        &mut self,
        name: String,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        self.functions
            .new_native_basic_op(name, address, min, max, rest)
    }

    pub(super) fn new_builtin_func_eval(
        &mut self,
        name: String,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        self.functions
            .new_native_func_eval(name, address, min, max, rest)
    }

    pub(super) fn new_proc_method(&mut self, proc: Proc) -> FuncId {
        self.functions.new_proc_method(proc)
    }

    pub(super) fn new_attr_reader(&mut self, name: IdentId, ivar_name: IdentId) -> FuncId {
        self.functions.new_attr_reader(name, ivar_name)
    }

    pub(super) fn new_attr_writer(&mut self, name: IdentId, ivar_name: IdentId) -> FuncId {
        self.functions.new_attr_writer(name, ivar_name)
    }

    pub(crate) fn new_callsite(
        &mut self,
        name: Option<IdentId>,
        pos_num: usize,
        kw_pos: SlotId,
        kw_args: indexmap::IndexMap<IdentId, usize>,
        splat_pos: Vec<usize>,
        hash_splat_pos: Vec<SlotId>,
        block_fid: Option<FuncId>,
        block_arg: Option<SlotId>,
        args: SlotId,
        recv: SlotId,
        dst: Option<SlotId>,
    ) -> CallSiteId {
        let id = CallSiteId(self.callsite_info.len() as u32);
        self.callsite_info.push(CallSiteInfo {
            id,
            name,
            pos_num,
            kw_pos,
            kw_args,
            splat_pos,
            hash_splat_pos,
            block_fid,
            block_arg,
            args,
            recv,
            dst,
        });
        id
    }

    pub(crate) fn new_constsite(
        &mut self,
        base: Option<SlotId>,
        name: IdentId,
        prefix: Vec<IdentId>,
        toplevel: bool,
    ) -> ConstSiteId {
        let info = ConstSiteInfo {
            name,
            base,
            prefix,
            toplevel,
            cache: None,
        };
        let id = self.constsite_info.len();
        self.constsite_info.push(info);
        ConstSiteId(id as u32)
    }

    pub(crate) fn new_optcase(
        &mut self,
        min: u16,
        max: u16,
        branch_table: Vec<u32>,
        offsets: Vec<u32>,
    ) -> OptCaseId {
        let id = self.optcase_info.len();
        let branch_table = branch_table.into_boxed_slice();
        self.optcase_info.push(OptCaseInfo {
            min,
            max,
            branch_table,
            offsets,
        });
        OptCaseId::from(id as u32)
    }

    pub(crate) fn set_func_data(&mut self, func_id: FuncId) {
        let info = self.iseq(func_id);
        let regs = info.total_reg_num();
        let pc = info.get_top_pc();
        self[func_id].set_pc_regnum(pc, u16::try_from(regs).unwrap());
    }

    ///
    /// Check whether a method *name* of class *class_id* exists.
    ///
    /// This fn does not consider visibility.
    ///
    pub(crate) fn check_method_for_class(
        &self,
        class_id: ClassId,
        name: IdentId,
    ) -> Option<MethodTableEntry> {
        let class_version = Globals::class_version();
        self.check_method_for_class_with_version(class_id, name, class_version)
    }

    pub(crate) fn check_method_for_class_with_version(
        &self,
        class_id: ClassId,
        name: IdentId,
        class_version: u32,
    ) -> Option<MethodTableEntry> {
        GLOBAL_METHOD_CACHE.with(|cache| {
            let mut c = cache.borrow_mut();
            if let Some(entry) = c.get(class_id, name, class_version) {
                return entry.cloned();
            };
            let entry = self.classes.search_method_by_class_id(class_id, name);
            c.insert((name, class_id), class_version, entry.clone());
            entry
        })
    }

    pub(super) fn invalidate_jit_code(&mut self) {
        self.iseqs
            .iter_mut()
            .for_each(|info| info.invalidate_jit_code())
    }
}

impl Store {
    /// Get class name of *ClassId*.
    pub(crate) fn debug_class_name(&self, class: impl Into<Option<ClassId>>) -> String {
        if let Some(class) = class.into() {
            let class_obj = self.classes[class].get_module();
            match self.classes[class].get_name() {
                Some(_) => {
                    let v: Vec<_> = self.classes.get_parents(class).into_iter().rev().collect();
                    v.join("::")
                }
                None => match class_obj.is_singleton() {
                    None => format!("#<Class:{:016x}>", class_obj.as_val().id()),
                    Some(base) => format!("#<Class:{}>", base.debug_tos(self)),
                },
            }
        } else {
            "<INVALID>".to_string()
        }
    }

    //#[cfg(feature = "emit-bc")]
    pub fn dump_iseq(&self, iseq_id: ISeqId) {
        use bytecodegen::BcIndex;

        let iseq = &self[iseq_id];
        eprintln!("------------------------------------");
        let loc = iseq.loc;
        let line = iseq.sourceinfo.get_line(&loc);
        let file_name = iseq.sourceinfo.file_name();
        eprintln!(
            "<{}> {file_name}:{line}",
            self.func_description(iseq.func_id()),
        );
        for (bc_idx, cache_type) in &iseq.cache_map {
            let pc = iseq.get_pc(*bc_idx);
            if let Some(MethodCacheEntry {
                recv_class,
                func_id,
                version,
            }) = pc.method_cache()
            {
                eprintln!(
                    "{:?} {:?} {:08x} {:?}",
                    recv_class, func_id, version, cache_type
                );
            }
        }
        eprintln!(
            "{:?} owner:{:?} local_vars:{} temp:{}",
            self[iseq.func_id()].meta(),
            self[iseq.func_id()].owner_class(),
            iseq.local_num(),
            iseq.temp_num
        );
        eprintln!("{:?}", iseq.args);
        eprintln!("{:?}", iseq.get_exception_map());
        for i in 0..iseq.bytecode_len() {
            let bc_pos = BcIndex::from(i);
            if let Some(bbid) = iseq.bb_info.is_bb_head(bc_pos) {
                eprintln!("  {:?}", bbid);
            };
            let trace_ir = iseq.trace_ir(self, bc_pos);
            if let Some(fmt) = trace_ir.format(self) {
                eprintln!("    {bc_pos} [{:02}] {fmt}", iseq.sp[i].0);
            };
        }
        eprintln!("------------------------------------");
    }

    #[cfg(feature = "profile")]
    pub fn clear_stats(&mut self) {
        GLOBAL_METHOD_CACHE.with(|cache| {
            cache.borrow_mut().clear_stats();
        });
    }

    #[cfg(feature = "profile")]
    pub(crate) fn show_stats(&self) {
        eprintln!("global method cache stats (top 20)");
        eprintln!("{:30} {:30} {:10}", "func name", "class", "count");
        eprintln!("------------------------------------------------------------------------");
        GLOBAL_METHOD_CACHE.with(|cache| {
            let c = cache.borrow();
            let mut v = c.global_method_cache_stats();
            v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
            for ((class_id, name), count) in v.into_iter().take(20) {
                eprintln!(
                    "{:30} {:30} {:10}",
                    name.to_string(),
                    self.debug_class_name(*class_id),
                    count
                );
            }
        });

        eprintln!();
        eprintln!("full method exploration stats (top 20)");
        eprintln!("{:25} {:45} {:10}", "func name", "class", "count");
        eprintln!(
            "----------------------------------------------------------------------------------"
        );
        GLOBAL_METHOD_CACHE.with(|cache| {
            let c = cache.borrow();
            let mut v = c.method_exprolation_stats();
            v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
            for ((class_id, name), count) in v.into_iter().take(20) {
                eprintln!(
                    "{:25} {:45} {:10}",
                    name.to_string(),
                    self.debug_class_name(*class_id),
                    count
                );
            }
        });
    }
}

pub struct ClassInfoTable {
    table: Vec<ClassInfo>,
}

impl std::ops::Index<ClassId> for ClassInfoTable {
    type Output = ClassInfo;
    fn index(&self, index: ClassId) -> &Self::Output {
        &self.table[index.u32() as usize]
    }
}

impl std::ops::IndexMut<ClassId> for ClassInfoTable {
    fn index_mut(&mut self, index: ClassId) -> &mut Self::Output {
        &mut self.table[index.u32() as usize]
    }
}

impl ClassInfoTable {
    fn new() -> Self {
        Self {
            table: vec![ClassInfo::new(); 50],
        }
    }

    fn add_class(&mut self) -> ClassId {
        let id = self.table.len();
        self.table.push(ClassInfo::new());
        ClassId::new(id as u32)
    }

    fn copy_class(&mut self, original_class: ClassId) -> ClassId {
        let id = self.table.len();
        let info = self[original_class].copy();
        self.table.push(info);
        ClassId::new(id as u32)
    }

    fn def_builtin_class(&mut self, class: ClassId) {
        self[class] = ClassInfo::new();
    }

    pub(crate) fn search_method_by_class_id(
        &self,
        class_id: ClassId,
        name: IdentId,
    ) -> Option<MethodTableEntry> {
        let module = self[class_id].get_module();
        self.search_method(module, name)
    }
}

#[derive(Debug, Clone)]
pub struct ConstCache {
    pub version: usize,
    pub base_class: Option<Value>,
    pub value: Value,
}

impl alloc::GC<RValue> for ConstCache {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        if let Some(v) = &self.base_class {
            v.mark(alloc)
        }
        self.value.mark(alloc);
    }
}

#[derive(Debug, Clone)]
pub struct ConstSiteInfo {
    /// Name of constants.
    pub name: IdentId,
    /// The slot of base object.
    pub(crate) base: Option<SlotId>,
    /// Qualifier.
    pub prefix: Vec<IdentId>,
    /// Is toplevel?. (e.g. ::Foo)
    pub toplevel: bool,
    /// Inline constant cache.
    pub cache: Option<ConstCache>, //(version, base_class, value)
}

impl alloc::GC<RValue> for ConstSiteInfo {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        if let Some(cache) = &self.cache {
            cache.mark(alloc);
        }
    }
}

impl ConstSiteInfo {
    pub fn format(&self) -> String {
        let ConstSiteInfo {
            name,
            base,
            prefix,
            toplevel,
            ..
        } = self;
        let mut const_name = if *toplevel { "::" } else { "" }.to_string();
        for c in prefix {
            const_name += &format!("{:?}::", c);
        }
        const_name += &format!("{:?}", name);
        format!(
            "{}{}",
            if let Some(base) = base {
                format!("{:?}::", base)
            } else {
                "".to_string()
            },
            const_name
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct ConstSiteId(pub u32);

/// Infomation for a call site.
#[derive(Debug, Clone)]
pub struct CallSiteInfo {
    /// Call site id.
    pub id: CallSiteId,
    /// Name of method. (None for *super*)
    pub name: Option<IdentId>,
    /// Position of the receiver.
    pub(crate) recv: SlotId,
    /// Position of the first argument.
    pub(crate) args: SlotId,
    /// Number of positional arguments.
    pub pos_num: usize,
    /// Positions of splat arguments.
    pub splat_pos: Vec<usize>,
    /// *FuncId* of passed block.
    pub block_fid: Option<FuncId>,
    /// Position of block argument.
    pub(crate) block_arg: Option<SlotId>,
    /// Postion of keyword arguments.
    pub(crate) kw_pos: SlotId,
    /// Names and positions of keyword arguments.
    pub kw_args: indexmap::IndexMap<IdentId, usize>,
    /// Position of hash splat arguments.
    pub(crate) hash_splat_pos: Vec<SlotId>,
    /// Position where the result is to be stored to.
    pub(crate) dst: Option<SlotId>,
}

impl CallSiteInfo {
    pub fn kw_may_exists(&self) -> bool {
        !self.kw_args.is_empty() || !self.hash_splat_pos.is_empty()
    }

    pub fn has_splat(&self) -> bool {
        !self.splat_pos.is_empty()
    }

    pub fn has_hash_splat(&self) -> bool {
        !self.hash_splat_pos.is_empty()
    }

    pub fn kw_len(&self) -> usize {
        self.kw_args.len() + self.hash_splat_pos.len()
    }

    pub fn is_func_call(&self) -> bool {
        self.recv.is_self()
    }

    pub fn block_handler(&self, lfp: Lfp) -> Option<BlockHandler> {
        if let Some(block_fid) = self.block_fid {
            let bh = BlockHandler::from_caller(block_fid);
            Some(bh)
        } else if let Some(block_arg) = self.block_arg {
            match lfp.register(block_arg) {
                Some(v) => Some(BlockHandler::new(v)),
                None => None,
            }
        } else {
            None
        }
    }

    ///
    /// This call site has no keyword arguments, no splat arguments, no hash splat arguments, and no block argument.
    ///
    pub fn is_simple(&self) -> bool {
        self.kw_len() == 0 && !self.has_splat() && self.block_arg.is_none()
    }

    pub(crate) fn object_send_single_splat(&self) -> bool {
        self.splat_pos.len() == 1 && self.pos_num == 1 && !self.kw_may_exists()
    }

    pub fn format_args(&self) -> String {
        let CallSiteInfo {
            pos_num,
            splat_pos,
            kw_pos,
            kw_args,
            hash_splat_pos,
            block_arg,
            block_fid,
            args,
            ..
        } = self;
        let mut s = String::new();
        if *pos_num > 0 {
            for i in 0..*pos_num {
                if i > 0 {
                    s += ",";
                }
                if splat_pos.contains(&i) {
                    s += "*";
                }
                s += &format!("{:?}", *args + i);
            }
        }
        for (i, (k, v)) in kw_args.iter().enumerate() {
            if i > 0 || *pos_num > 0 {
                s += ",";
            }
            s += &format!("{}:{:?}", k, *kw_pos + *v);
        }
        for pos in hash_splat_pos.iter() {
            if s.len() > 0 {
                s += ",";
            }
            s += &format!("**{:?}", pos);
        }
        if let Some(block_arg) = block_arg {
            if s.len() > 0 {
                s += ",";
            }
            s += &format!("&{:?}", block_arg);
        }
        s = format!("({})", s);
        if let Some(block_fid) = block_fid {
            s += &format!(" {{ {:?} }}", block_fid);
        }
        s
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct CallSiteId(pub u32);

impl std::convert::From<u32> for CallSiteId {
    fn from(id: u32) -> Self {
        Self(id)
    }
}

impl CallSiteId {
    pub fn get(&self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct OptCaseInfo {
    pub min: u16,
    pub max: u16,
    pub branch_table: Box<[u32]>,
    pub offsets: Vec<u32>,
}

impl OptCaseInfo {
    pub(crate) fn find(&self, idx: Value) -> u32 {
        if let Some(idx) = idx.try_fixnum() {
            if let Ok(idx) = u16::try_from(idx) {
                if idx >= self.min && idx <= self.max {
                    return self.branch_table[(idx - self.min) as usize];
                }
            }
        }
        self.offsets[0]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct OptCaseId(pub u32);

impl std::convert::From<u32> for OptCaseId {
    fn from(id: u32) -> Self {
        Self(id)
    }
}

impl OptCaseId {
    pub fn get(&self) -> u32 {
        self.0
    }
}

#[derive(Default)]
struct GlobalMethodCache {
    version: u32,
    cache: HashMap<(IdentId, ClassId), Option<MethodTableEntry>>,
    #[cfg(feature = "profile")]
    /// Stats for global method cache access.
    global_method_cache_stats: HashMap<(ClassId, IdentId), usize>,
    #[cfg(feature = "profile")]
    /// Stats for method cache miss.
    method_exprolation_stats: HashMap<(ClassId, IdentId), usize>,
}

impl GlobalMethodCache {
    fn get(
        &mut self,
        class_id: ClassId,
        name: IdentId,
        class_version: u32,
    ) -> Option<Option<&MethodTableEntry>> {
        #[cfg(feature = "profile")]
        {
            match self.global_method_cache_stats.get_mut(&(class_id, name)) {
                Some(c) => *c += 1,
                None => {
                    self.global_method_cache_stats.insert((class_id, name), 1);
                }
            }
        }
        if self.version != class_version {
            self.cache.clear();
            self.version = class_version;
            return None;
        }
        self.cache.get(&(name, class_id)).map(|e| e.as_ref())
    }

    fn insert(
        &mut self,
        key: (IdentId, ClassId),
        class_version: u32,
        entry: Option<MethodTableEntry>,
    ) {
        #[cfg(feature = "profile")]
        {
            match self.method_exprolation_stats.get_mut(&(key.1, key.0)) {
                Some(c) => *c += 1,
                None => {
                    self.method_exprolation_stats.insert((key.1, key.0), 1);
                }
            }
        }
        self.version = class_version;
        self.cache.insert(key, entry);
    }

    #[cfg(feature = "profile")]
    pub(super) fn clear_stats(&mut self) {
        self.global_method_cache_stats.clear();
        self.method_exprolation_stats.clear();
    }

    #[cfg(feature = "profile")]
    pub(super) fn global_method_cache_stats(&self) -> Vec<(&(ClassId, IdentId), &usize)> {
        self.global_method_cache_stats.iter().collect()
    }

    #[cfg(feature = "profile")]
    pub(super) fn method_exprolation_stats(&self) -> Vec<(&(ClassId, IdentId), &usize)> {
        self.method_exprolation_stats.iter().collect()
    }
}
