use monoasm::DestLabel;
use ruruby_parse::{LvarCollector, ParseResult};

use crate::executor::inline::InlineMethodId;

use super::*;
use std::pin::Pin;

mod class;
mod function;
mod iseq;
pub use class::*;
pub use function::*;
pub(crate) use iseq::*;

pub(crate) struct ClassInfoTable {
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
    pub(crate) fn new() -> Self {
        Self {
            table: vec![ClassInfo::new(); 40],
        }
    }

    pub(super) fn add_class(&mut self) -> ClassId {
        let id = self.table.len();
        self.table.push(ClassInfo::new());
        ClassId::new(id as u32)
    }

    pub(super) fn copy_class(&mut self, original_class: ClassId) -> ClassId {
        let id = self.table.len();
        let info = self[original_class].copy();
        self.table.push(info);
        ClassId::new(id as u32)
    }

    pub(super) fn def_builtin_class(&mut self, class: ClassId) {
        self[class] = ClassInfo::new();
    }
}

pub struct InlineFuncInfo {
    pub inline_gen: Box<InlineGen>,
    pub inline_analysis: InlineAnalysis,
    //#[cfg(feature = "dump-bc")]
    pub name: String,
}

pub struct Store {
    /// function info.
    pub(crate) functions: function::Funcs,
    /// class table.
    pub(crate) classes: ClassInfoTable,
    /// call site info.
    callsite_info: Vec<CallSiteInfo>,
    /// const access site info.
    constsite_info: Vec<ConstSiteInfo>,
    /// opt case branch info.
    optcase_info: Vec<OptCaseInfo>,
    /// inline method info.
    inline_method: Vec<InlineFuncInfo>,
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
        self.constsite_info.iter().for_each(|info| info.mark(alloc));
        self.classes.table.iter().for_each(|info| info.mark(alloc));
    }
}

impl Store {
    pub(super) fn new() -> Self {
        Self {
            functions: function::Funcs::default(),
            constsite_info: vec![],
            callsite_info: vec![],
            optcase_info: vec![],
            classes: ClassInfoTable::new(),
            inline_method: vec![],
        }
    }
}

impl Store {
    pub(super) fn add_inline_info(
        &mut self,
        inline_gen: Box<InlineGen>,
        inline_analysis: InlineAnalysis,
        _name: String,
    ) -> InlineMethodId {
        let id = self.inline_method.len();
        self.inline_method.push(InlineFuncInfo {
            inline_gen,
            inline_analysis,
            //#[cfg(feature = "dump-bc")]
            name: _name,
        });
        InlineMethodId::new(id)
    }

    pub(crate) fn get_inline_info(&self, id: InlineMethodId) -> &InlineFuncInfo {
        let id: usize = id.into();
        &self.inline_method[id]
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

    pub(crate) fn add_main(&mut self, result: ParseResult) -> Result<FuncId> {
        self.functions.add_method(
            Some(IdentId::get_id("/main")),
            BlockInfo {
                params: vec![],
                body: Box::new(result.node),
                lvar: LvarCollector::new(),
                loc: Loc::default(),
            },
            Loc::default(),
            result.source_info,
        )
    }

    pub fn add_classdef(
        &mut self,
        name: Option<IdentId>,
        info: BlockInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        self.functions.add_classdef(name, info, loc, sourceinfo)
    }

    pub(crate) fn add_block(
        &mut self,
        mother: (FuncId, usize),
        outer: (FuncId, ExternalContext),
        optional_params: Vec<(usize, bytecodegen::BcLocal, IdentId)>,
        is_block_style: bool,
        info: BlockInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        self.functions.add_block(
            mother,
            outer,
            optional_params,
            is_block_style,
            info,
            loc,
            sourceinfo,
        )
    }

    pub(crate) fn add_eval(
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
        self.functions
            .add_block(mother, outer, vec![], false, info, loc, result.source_info)
    }

    pub(super) fn add_builtin_func(
        &mut self,
        name: &str,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
        kw: &[&str],
    ) -> FuncId {
        self.functions
            .add_native_func(name.to_string(), address, min, max, rest, kw)
    }

    pub(super) fn add_basic_op(
        &mut self,
        name: String,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        self.functions
            .add_native_basic_op(name, address, min, max, rest)
    }

    pub(super) fn add_builtin_func_eval(
        &mut self,
        name: String,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        self.functions
            .add_native_func_eval(name, address, min, max, rest)
    }

    pub(super) fn add_attr_reader(&mut self, name: IdentId, ivar_name: IdentId) -> FuncId {
        self.functions.add_attr_reader(name, ivar_name)
    }

    pub(super) fn add_attr_writer(&mut self, name: IdentId, ivar_name: IdentId) -> FuncId {
        self.functions.add_attr_writer(name, ivar_name)
    }

    pub(crate) fn add_callsite(
        &mut self,
        name: Option<IdentId>,
        pos_num: usize,
        kw_pos: SlotId,
        kw_args: IndexMap<IdentId, usize>,
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

    pub(crate) fn add_constsite(
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
            cache: ConstCache::default(),
        };
        let id = self.constsite_info.len();
        self.constsite_info.push(info);
        ConstSiteId(id as u32)
    }

    pub(crate) fn add_optcase(
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
        let info = self[func_id].as_ruby_func();
        let regs = info.total_reg_num();
        let pc = info.get_top_pc();
        self[func_id].set_pc_regnum(pc, u16::try_from(regs).unwrap());
    }
}

impl Store {
    /// Get class name of *ClassId*.
    pub(crate) fn get_class_name(&self, class: impl Into<Option<ClassId>>) -> String {
        if let Some(class) = class.into() {
            let class_obj = self.classes[class].get_module();
            match self.classes[class].get_name_id() {
                Some(_) => {
                    let v: Vec<_> = self
                        .classes
                        .get_parents(class)
                        .into_iter()
                        .rev()
                        .map(|name| name.to_string())
                        .collect();
                    v.join("::")
                }
                None => match class_obj.is_singleton() {
                    None => format!("#<Class:{:016x}>", class_obj.as_val().id()),
                    Some(base) => format!("#<Class:{}>", base.debug(self)),
                },
            }
        } else {
            "<INVALID>".to_string()
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConstCache {
    pub cached_version: usize,
    pub cached_base_class: Option<Value>,
    pub cached_value: Option<Value>,
}

impl std::default::Default for ConstCache {
    fn default() -> Self {
        Self {
            cached_version: usize::MAX,
            cached_base_class: None,
            cached_value: None,
        }
    }
}

impl alloc::GC<RValue> for ConstCache {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        if let Some(v) = &self.cached_base_class {
            v.mark(alloc)
        }
        if let Some(v) = &self.cached_value {
            v.mark(alloc)
        }
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
    pub cache: ConstCache, //(version, base_class, value)
}

impl alloc::GC<RValue> for ConstSiteInfo {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.cache.mark(alloc);
    }
}

impl ConstSiteInfo {
    //#[cfg(feature = "dump-bc")]
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
            (*c).append_to(&mut const_name);
            const_name += "::";
        }
        (*name).append_to(&mut const_name);
        format!(
            "{}const[{}]",
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
    pub(crate) block_arg: Option<SlotId>,
    /// Postion of keyword arguments.
    pub(crate) kw_pos: SlotId,
    /// Names and positions of keyword arguments.
    pub kw_args: IndexMap<IdentId, usize>,
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
