use ruruby_parse::LvarCollector;

use super::*;
use std::pin::Pin;

mod function;

#[derive(Default)]
pub(crate) struct Store {
    /// function info.
    functions: function::Funcs,
    /// inline function info.
    inline: HashMap<FuncId, InlineMethod>,
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
        self.classes.iter().for_each(|info| info.mark(alloc));
    }
}

impl Store {
    pub(super) fn new() -> Self {
        Self {
            functions: function::Funcs::default(),
            inline: HashMap::default(),
            constsite_info: vec![],
            callsite_info: vec![],
            classes: vec![ClassInfo::new(); 20],
        }
    }

    #[cfg(feature = "emit-bc")]
    pub(super) fn functions(&self) -> &[FuncInfo] {
        self.functions.functions()
    }

    pub(crate) fn func_len(&self) -> usize {
        self.functions.function_len()
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
    pub(crate) fn add_method(
        &mut self,
        name: Option<IdentId>,
        info: BlockInfo,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        self.functions.add_method(name, info, sourceinfo)
    }

    pub(crate) fn add_classdef(
        &mut self,
        name: Option<IdentId>,
        info: BlockInfo,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        self.functions.add_classdef(name, info, sourceinfo)
    }

    pub(crate) fn add_block(
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

    pub(crate) fn get_inline(&self, func_id: FuncId) -> Option<&InlineMethod> {
        self.inline.get(&func_id)
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

    pub(super) fn add_inline(&mut self, func_id: FuncId, inline_id: InlineMethod) {
        self.inline.insert(func_id, inline_id);
    }

    pub(crate) fn callsite_offset(&self) -> usize {
        self.callsite_info.len()
    }

    pub(in crate::executor) fn get_compile_info(&mut self) -> CompileInfo {
        self.functions.get_compile_info()
    }

    pub(crate) fn add_callsite(
        &mut self,
        name: Option<IdentId>,
        arg_num: usize,
        kw_pos: SlotId,
        kw_args: HashMap<IdentId, usize>,
        splat_pos: Vec<usize>,
        hash_splat_pos: Vec<SlotId>,
    ) {
        self.callsite_info.push(CallSiteInfo {
            name,
            arg_num,
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
        let pc = info.get_pc(0);
        self[func_id].data.set_pc(pc);
        self[func_id].data.set_reg_num(regs as i64);
    }
}

///
/// ID of function.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct FuncId(std::num::NonZeroU32);

impl From<FuncId> for u32 {
    fn from(id: FuncId) -> u32 {
        id.0.get()
    }
}

impl FuncId {
    pub fn new(id: u32) -> Self {
        Self(std::num::NonZeroU32::new(id).unwrap())
    }

    pub fn get(&self) -> u32 {
        self.0.get()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstSiteInfo {
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
    pub arg_num: usize,
    /// Postion of keyword arguments.
    pub(crate) kw_pos: SlotId,
    /// Names and positions of keyword arguments.
    pub kw_args: HashMap<IdentId, usize>,
    /// Positions of splat arguments.
    pub splat_pos: Vec<usize>,
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

#[derive(Debug, Clone)]
pub(crate) enum FuncKind {
    ISeq(ISeqInfo),
    Builtin { abs_address: u64 },
    AttrReader { ivar_name: IdentId },
    AttrWriter { ivar_name: IdentId },
}

impl alloc::GC<RValue> for FuncKind {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        if let FuncKind::ISeq(info) = self {
            info.mark(alloc)
        }
    }
}

impl std::default::Default for FuncKind {
    fn default() -> Self {
        Self::Builtin { abs_address: 0 }
    }
}

pub const FUNCDATA_OFFSET_CODEPTR: u64 = 0;
pub const FUNCDATA_OFFSET_META: u64 = 8;
pub const FUNCDATA_OFFSET_REGNUM: u64 = 12;
pub const FUNCDATA_OFFSET_PC: u64 = 16;

#[derive(Debug, Clone, Default)]
pub struct FuncInfo {
    /// name of this function.
    name: Option<IdentId>,
    pub(in crate::executor) data: FuncData,
    pub(in crate::executor) kind: FuncKind,
}

impl alloc::GC<RValue> for FuncInfo {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.kind.mark(alloc);
    }
}

impl FuncInfo {
    fn new_method_iseq(
        name: impl Into<Option<IdentId>>,
        func_id: Option<FuncId>,
        args: ParamsInfo,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        let name = name.into();
        let info = ISeqInfo::new_method(func_id, name, args, sourceinfo);
        Self {
            name,
            data: FuncData {
                codeptr: None,
                pc: None,
                meta: Meta::vm_method(func_id, 0),
            },
            kind: FuncKind::ISeq(info),
        }
    }

    fn new_block_iseq(
        func_id: Option<FuncId>,
        mother: FuncId,
        outer: (FuncId, Vec<(HashMap<IdentId, u16>, Option<IdentId>)>),
        args: ParamsInfo,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        let info = ISeqInfo::new_block(func_id, mother, outer, args, sourceinfo);
        Self {
            name: None,
            data: FuncData {
                codeptr: None,
                pc: None,
                meta: Meta::vm_method(func_id, 0),
            },
            kind: FuncKind::ISeq(info),
        }
    }

    fn new_classdef_iseq(
        name: Option<IdentId>,
        func_id: Option<FuncId>,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        let info = ISeqInfo::new_method(func_id, name, ParamsInfo::default(), sourceinfo);
        Self {
            name,
            data: FuncData {
                codeptr: None,
                pc: None,
                meta: Meta::vm_classdef(func_id, 0),
            },
            kind: FuncKind::ISeq(info),
        }
    }

    fn new_native(func_id: FuncId, name: String, address: BuiltinFn, arity: i32) -> Self {
        let reg_num = if arity == -1 { -1 } else { arity as i64 };
        Self {
            name: Some(IdentId::get_id_from_string(name)),
            data: FuncData {
                codeptr: None,
                pc: None,
                meta: Meta::native(func_id, reg_num),
            },
            kind: FuncKind::Builtin {
                abs_address: address as *const u8 as u64,
            },
        }
    }

    fn new_attr_reader(func_id: FuncId, name: IdentId, ivar_name: IdentId) -> Self {
        Self {
            name: Some(name),
            data: FuncData {
                codeptr: None,
                pc: None,
                meta: Meta::native(func_id, 0),
            },
            kind: FuncKind::AttrReader { ivar_name },
        }
    }

    fn new_attr_writer(func_id: FuncId, name: IdentId, ivar_name: IdentId) -> Self {
        Self {
            name: Some(name),
            data: FuncData {
                codeptr: None,
                pc: None,
                meta: Meta::native(func_id, 1),
            },
            kind: FuncKind::AttrWriter { ivar_name },
        }
    }

    pub(crate) fn name(&self) -> Option<IdentId> {
        self.name
    }

    pub(crate) fn as_ruby_func(&self) -> &ISeqInfo {
        match &self.kind {
            FuncKind::ISeq(info) => info,
            _ => unreachable!(),
        }
    }

    pub(crate) fn as_ruby_func_mut(&mut self) -> &mut ISeqInfo {
        match &mut self.kind {
            FuncKind::ISeq(info) => info,
            _ => unreachable!(),
        }
    }
}

impl FuncInfo {
    #[cfg(feature = "emit-bc")]
    pub(crate) fn dump_bc(&self, globals: &Globals) {
        let info = self.as_ruby_func();
        eprintln!("------------------------------------");
        eprintln!(
            "{:?} name:{} bc:{:?} meta:{:?} {:?}",
            info.id(),
            match self.name {
                Some(name) => name.to_string(),
                None => "<ANONYMOUS>".to_string(),
            },
            BcPcBase::new(info),
            self.data.meta,
            self.kind
        );
        eprintln!(
            "{:?}",
            info.exception_map
                .iter()
                .map(|(range, rescue, ensure, err_reg)| {
                    let start = info.get_pc_index(Some(range.start));
                    let end = info.get_pc_index(Some(range.end));
                    let rescue = rescue.map(|pc| info.get_pc_index(Some(pc)));
                    let ensure = ensure.map(|pc| info.get_pc_index(Some(pc)));
                    (start..end, rescue, ensure, err_reg)
                })
                .collect::<Vec<_>>()
        );
        let bb_info = info.get_bb_info();
        for (i, pc) in info.bytecode().iter().enumerate() {
            let pc = BcPc::from(pc);
            if let Some(fmt) = pc.format(globals, i) {
                eprint!("{}:{:05} ", if bb_info[i].is_some() { "+" } else { " " }, i);
                eprintln!("{}", fmt);
            };
        }
        eprintln!("------------------------------------");
    }
}

///
/// Information of instruction sequences.
///
#[derive(Clone, Default)]
pub(crate) struct ISeqInfo {
    /// ID of this function.
    id: Option<FuncId>,
    pub(crate) mother: Option<FuncId>,
    name: Option<IdentId>,
    /// Bytecode.
    pub(super) bytecode: Option<Pin<Box<[Bc]>>>,
    /// Source map.
    pub sourcemap: Vec<Loc>,
    /// Exception handling map.
    exception_map: Vec<(
        std::ops::Range<BcPc>, // range of capturing exception
        Option<BcPc>,          // rescue destination pc
        Option<BcPc>,          // ensure destination pc
        Option<SlotId>,        // a slot where an error object is assigned
    )>,
    /// the name of arguments.
    pub(in crate::executor) args: ParamsInfo,
    /// outer local variables. (dynamic_locals, block_param)
    pub outer_locals: Vec<(HashMap<IdentId, u16>, Option<IdentId>)>,
    /// literal values. (for GC)
    pub literals: Vec<Value>,
    /// The number of non-temporary registers.
    pub non_temp_num: u16,
    /// The number of temporary registers.
    pub temp_num: u16,
    pub lexical_context: Vec<Module>,
    pub sourceinfo: SourceInfoRef,
    pub(crate) is_block_style: bool,
}

impl std::fmt::Debug for ISeqInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "RubyFuncInfo {{ id:{} name:{} method:{:?} args: {} non_temp: {} temp: {}}}",
            self.id().get(),
            self.name(),
            self.mother,
            self.args.args_names.len(),
            self.non_temp_num,
            self.temp_num
        )
    }
}

impl alloc::GC<RValue> for ISeqInfo {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.literals.iter().for_each(|v| v.mark(alloc));
        self.lexical_context.iter().for_each(|m| m.mark(alloc));
    }
}

impl ISeqInfo {
    pub(in crate::executor) fn new(
        id: Option<FuncId>,
        mother: Option<FuncId>,
        outer_locals: Vec<(HashMap<IdentId, u16>, Option<IdentId>)>,
        name: Option<IdentId>,
        args: ParamsInfo,
        sourceinfo: SourceInfoRef,
        is_block: bool,
    ) -> Self {
        ISeqInfo {
            id,
            mother,
            name,
            bytecode: None,
            sourcemap: vec![],
            exception_map: vec![],
            args: args.clone(),
            outer_locals,
            literals: vec![],
            non_temp_num: 0,
            temp_num: 0,
            lexical_context: vec![],
            sourceinfo,
            is_block_style: is_block,
        }
    }

    pub(in crate::executor) fn new_block(
        id: Option<FuncId>,
        mother: FuncId,
        outer: (FuncId, Vec<(HashMap<IdentId, u16>, Option<IdentId>)>),
        args: ParamsInfo,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        Self::new(id, Some(mother), outer.1, None, args, sourceinfo, true)
    }

    pub(in crate::executor) fn new_method(
        id: Option<FuncId>,
        name: Option<IdentId>,
        args: ParamsInfo,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        Self::new(id, id, vec![], name, args, sourceinfo, false)
    }

    pub(crate) fn id(&self) -> FuncId {
        self.id.unwrap()
    }

    ///
    /// Set bytecode to the *ISeqInfo*.
    ///
    pub(in crate::executor) fn set_bytecode(&mut self, bc: Vec<Bc>) {
        self.bytecode = Some(Box::into_pin(bc.into_boxed_slice()));
    }

    ///
    /// Get a number of registers.
    ///
    pub(crate) fn total_reg_num(&self) -> usize {
        1 + (self.non_temp_num + self.temp_num) as usize
    }

    ///
    /// Get a number of non-temp registers.
    ///
    pub(crate) fn local_num(&self) -> usize {
        self.non_temp_num as usize
    }

    ///
    /// Get a number of keyword arguments.
    ///
    pub(crate) fn key_num(&self) -> usize {
        self.args.keyword_names.len()
    }

    ///
    /// Get a position of keyword arguments.
    ///
    pub(crate) fn block_pos(&self) -> usize {
        if self.args.block_param.is_some() {
            self.args.pos_num + self.key_num() + 1
        } else {
            0
        }
    }

    ///
    /// Get a number of required arguments.
    ///
    pub(crate) fn req_num(&self) -> usize {
        self.args.required_num
    }

    ///
    /// get a number of required + optional arguments.
    ///
    pub(crate) fn reqopt_num(&self) -> usize {
        self.args.reqopt_num
    }

    ///
    /// Get a number of required + optional + rest arguments.
    ///
    pub(crate) fn pos_num(&self) -> usize {
        self.args.pos_num
    }

    ///
    /// Get a parameter info.
    ///
    /// bit 0:rest(yes=1 no =0) bit 1:block
    pub(crate) fn info(&self) -> usize {
        (if self.args.block_param.is_some() {
            2
        } else {
            0
        }) + (self.args.pos_num - self.args.reqopt_num)
    }

    ///
    /// Get a block argument name.
    ///
    pub(crate) fn block_param_name(&self) -> Option<IdentId> {
        self.args.block_param
    }

    ///
    /// Get the name of iseq.
    ///
    pub(crate) fn name(&self) -> String {
        match &self.name {
            Some(name) => name.to_string(),
            None => "<unnamed>".to_string(),
        }
    }

    ///
    /// Get a reference of bytecode.
    ///
    pub(in crate::executor) fn bytecode(&self) -> &[Bc] {
        self.bytecode.as_ref().unwrap()
    }

    ///
    /// Get pc(*BcPc*) for instruction index(*idx*).
    ///
    pub(in crate::executor) fn get_pc(&self, idx: usize) -> BcPc {
        BcPc::from(&self.bytecode()[idx])
    }

    ///
    /// Get an instruction index(*usize*) corresponding to pc(*BcPc*).
    ///
    pub(in crate::executor) fn get_pc_index(&self, pc: Option<BcPc>) -> usize {
        if let Some(pos) = pc {
            pos - self.get_pc(0)
        } else {
            0
        }
    }

    ///
    /// Explore exception table for pc(*BcPc*) and return error handler's pc(*BcPc*) and the slot where an error object is to be stored.
    ///
    pub(in crate::executor) fn get_exception_dest(
        &self,
        pc: BcPc,
    ) -> Option<(Option<BcPc>, Option<BcPc>, Option<SlotId>)> {
        self.exception_map
            .iter()
            .filter_map(|(range, rescue, ensure, slot)| {
                if range.contains(&pc) {
                    Some((*rescue, *ensure, *slot))
                } else {
                    None
                }
            })
            .nth(0)
    }

    pub(in crate::executor) fn exception_push(
        &mut self,
        range: std::ops::Range<BcPc>,
        rescue: Option<BcPc>,
        ensure: Option<BcPc>,
        err_reg: Option<SlotId>,
    ) {
        self.exception_map.push((range, rescue, ensure, err_reg));
    }

    ///
    /// Get bytecode length.
    ///
    pub(crate) fn bytecode_len(&self) -> usize {
        self.bytecode().len()
    }

    ///
    /// Get bytecode address.
    ///
    #[cfg(feature = "emit-bc")]
    pub(in crate::executor) fn bytecode_top(&self) -> *const Bc {
        self.bytecode().as_ptr()
    }

    ///
    /// Get basic block information.
    ///
    /// This returns a Vec which represents whether it is a start of a basic block for each bytecode.
    ///
    /// Some((basic_block_id, Vec of source bytecodes)) => a start bytecode of a basic block.
    ///
    pub(crate) fn get_bb_info(&self) -> Vec<Option<(usize, Vec<usize>)>> {
        let mut info = vec![vec![]; self.bytecode_len() + 1];
        for (idx, pc) in self.bytecode().iter().enumerate() {
            let pc = BcPc::from(pc);
            if let Some(disp) = TraceIr::is_branch(pc) {
                info[((idx + 1) as i32 + disp) as usize].push(idx);
            }
        }
        assert_eq!(0, info[self.bytecode_len()].len());
        let mut bb_id = 1;
        let mut bb_info: Vec<_> = info
            .into_iter()
            .map(|e| {
                if !e.is_empty() {
                    let id = bb_id;
                    bb_id += 1;
                    Some((id, e))
                } else {
                    None
                }
            })
            .collect();
        for (idx, pc) in self.bytecode().iter().enumerate() {
            let pc = BcPc::from(pc);
            if !TraceIr::is_terminal(pc) {
                if let Some(ref mut elem) = bb_info[idx + 1] {
                    elem.1.push(idx);
                }
            }
        }
        // a first bytecode is always a start of basic block.
        if bb_info[0].is_none() {
            bb_info[0] = Some((0, vec![]));
        }
        /*for (id, i, v) in bb_info.iter().enumerate().filter_map(|(i, e)| match e {
            None => None,
            Some((id, v)) => Some((id, i, v)),
        }) {
            eprintln!("{} {} {:?}", id, i, v);
        }*/
        bb_info
    }
}
