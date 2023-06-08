use monoasm::DestLabel;
use ruruby_parse::LvarCollector;

use super::*;
use std::pin::Pin;

mod class;
mod function;
mod iseq;
pub use class::*;
pub use function::*;
pub(crate) use iseq::ISeqInfo;

#[derive(Default)]
pub(in crate::executor) struct Store {
    /// function info.
    functions: function::Funcs,
    /// inline function info.
    //inline: HashMap<FuncId, InlineMethod>,
    /// call site info.
    callsite_info: Vec<CallSiteInfo>,
    /// const access site info.
    constsite_info: Vec<ConstSiteInfo>,
    /// class table.
    classes: Vec<ClassInfo>,
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

impl std::ops::Index<ClassId> for Store {
    type Output = ClassInfo;
    fn index(&self, index: ClassId) -> &Self::Output {
        &self.classes[index.0 as usize]
    }
}

impl std::ops::IndexMut<ClassId> for Store {
    fn index_mut(&mut self, index: ClassId) -> &mut Self::Output {
        &mut self.classes[index.0 as usize]
    }
}

impl alloc::GC<RValue> for Store {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.functions.mark(alloc);
        self.constsite_info.iter().for_each(|info| {
            if let Some(v) = info.cache.1 {
                v.mark(alloc)
            }
        });
        self.classes.iter().for_each(|info| info.mark(alloc));
    }
}

impl Store {
    pub(super) fn new() -> Self {
        Self {
            functions: function::Funcs::default(),
            //inline: HashMap::default(),
            constsite_info: vec![],
            callsite_info: vec![],
            classes: vec![ClassInfo::new(); 20],
        }
    }

    pub(super) fn add_class(&mut self) -> ClassId {
        let id = self.classes.len();
        self.classes.push(ClassInfo::new());
        ClassId(id as u32)
    }

    pub(super) fn copy_class(&mut self, original_class: ClassId) -> ClassId {
        let id = self.classes.len();
        let info = self[original_class].copy();
        self.classes.push(info);
        ClassId(id as u32)
    }

    pub(super) fn def_builtin_class(&mut self, class: ClassId) {
        self[class] = ClassInfo::new();
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

    pub(in crate::executor) fn get_compile_info(&mut self) -> CompileInfo {
        self.functions.get_compile_info()
    }

    pub(crate) fn add_main(&mut self, ast: Node, sourceinfo: SourceInfoRef) -> Result<FuncId> {
        self.functions.add_method(
            Some(IdentId::get_id("/main")),
            BlockInfo {
                params: vec![],
                body: Box::new(ast),
                lvar: LvarCollector::new(),
            },
            sourceinfo,
        )
    }

    pub(in crate::executor) fn add_method(
        &mut self,
        name: Option<IdentId>,
        info: BlockInfo,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        self.functions.add_method(name, info, sourceinfo)
    }

    pub(in crate::executor) fn add_classdef(
        &mut self,
        name: Option<IdentId>,
        info: BlockInfo,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        self.functions.add_classdef(name, info, sourceinfo)
    }

    pub(in crate::executor) fn add_block(
        &mut self,
        mother: FuncId,
        outer: (FuncId, Vec<(HashMap<IdentId, u16>, Option<IdentId>)>),
        optional_params: Vec<(usize, BcLocal, IdentId)>,
        info: BlockInfo,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        self.functions
            .add_block(mother, outer, optional_params, info, sourceinfo)
    }

    pub(super) fn add_builtin_func(
        &mut self,
        name: String,
        address: BuiltinFn,
        arity: i32,
    ) -> FuncId {
        self.functions.add_native_func(name, address, arity)
    }

    pub(super) fn add_attr_reader(&mut self, name: IdentId, ivar_name: IdentId) -> FuncId {
        self.functions.add_attr_reader(name, ivar_name)
    }

    pub(super) fn add_attr_writer(&mut self, name: IdentId, ivar_name: IdentId) -> FuncId {
        self.functions.add_attr_writer(name, ivar_name)
    }

    pub(crate) fn callsite_offset(&self) -> usize {
        self.callsite_info.len()
    }

    pub(crate) fn add_callsite(
        &mut self,
        name: Option<IdentId>,
        pos_num: usize,
        kw_pos: SlotId,
        kw_args: HashMap<IdentId, usize>,
        splat_pos: Vec<usize>,
        hash_splat_pos: Vec<SlotId>,
    ) {
        self.callsite_info.push(CallSiteInfo {
            name,
            pos_num,
            kw_pos,
            kw_args,
            splat_pos,
            hash_splat_pos,
        })
    }

    pub(crate) fn add_constsite(
        &mut self,
        name: IdentId,
        prefix: Vec<IdentId>,
        toplevel: bool,
    ) -> ConstSiteId {
        let info = ConstSiteInfo {
            name,
            prefix,
            toplevel,
            cache: (usize::MAX, None),
        };
        let id = self.constsite_info.len();
        self.constsite_info.push(info);
        ConstSiteId(id as u32)
    }

    pub(crate) fn set_func_data(&mut self, func_id: FuncId) {
        let info = self[func_id].as_ruby_func();
        let regs = info.total_reg_num();
        let pc = info.get_top_pc();
        self[func_id].data.set_pc(pc);
        self[func_id].data.set_reg_num(regs as i64);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::executor) struct ConstSiteInfo {
    /// Name of constants.
    pub name: IdentId,
    /// Qualifier.
    pub prefix: Vec<IdentId>,
    /// Is toplevel?. (e.g. ::Foo)
    pub toplevel: bool,
    /// Inline constant cache.
    pub cache: (usize, Option<Value>), //(version, value)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct ConstSiteId(pub u32);

/// Infomation for a call site.
#[derive(Debug, Clone)]
pub struct CallSiteInfo {
    /// Name of method. (None for *super*)
    pub name: Option<IdentId>,
    /// Number of positional arguments.
    pub pos_num: usize,
    /// Postion of keyword arguments.
    pub(crate) kw_pos: SlotId,
    /// Names and positions of keyword arguments.
    pub kw_args: HashMap<IdentId, usize>,
    /// Positions of splat arguments.
    pub splat_pos: Vec<usize>,
    /// Position of hash splat arguments.
    pub(crate) hash_splat_pos: Vec<SlotId>,
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
