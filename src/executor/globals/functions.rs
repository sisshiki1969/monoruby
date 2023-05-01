use ruruby_parse::LvarCollector;

use super::*;
use std::pin::Pin;

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

pub(super) struct Funcs {
    info: Vec<FuncInfo>,
    compile_info: Vec<CompileInfo>,
}

impl std::ops::Index<FuncId> for Funcs {
    type Output = FuncInfo;
    fn index(&self, index: FuncId) -> &FuncInfo {
        &self.info[u32::from(index) as usize]
    }
}

impl std::ops::IndexMut<FuncId> for Funcs {
    fn index_mut(&mut self, index: FuncId) -> &mut FuncInfo {
        &mut self.info[u32::from(index) as usize]
    }
}

impl std::default::Default for Funcs {
    fn default() -> Self {
        Self {
            info: vec![FuncInfo::default()],
            compile_info: vec![],
        }
    }
}

impl alloc::GC<RValue> for Funcs {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.info.iter().for_each(|info| info.mark(alloc))
    }
}

impl Funcs {
    fn next_func_id(&self) -> FuncId {
        FuncId::new(self.info.len() as u32)
    }

    pub(crate) fn add_method(
        &mut self,
        name: Option<IdentId>,
        info: BlockInfo,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        let args = self.handle_args(info, vec![], &sourceinfo)?;
        Ok(self.add_method_iseq(name, args, sourceinfo))
    }

    fn add_block(
        &mut self,
        mother: FuncId,
        outer: (FuncId, Vec<(HashMap<IdentId, u16>, Option<IdentId>)>),
        for_params: Vec<(usize, BcLocal, IdentId)>,
        info: BlockInfo,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        let args = self.handle_args(info, for_params, &sourceinfo)?;
        let func_id = self.next_func_id();
        let info = FuncInfo::new_block_iseq(Some(func_id), mother, outer, args, sourceinfo);
        self.info.push(info);
        Ok(func_id)
    }

    fn add_classdef(
        &mut self,
        name: Option<IdentId>,
        info: BlockInfo,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        let _ = self.handle_args(info, vec![], &sourceinfo)?;
        let func_id = self.next_func_id();
        let info = FuncInfo::new_classdef_iseq(name, Some(func_id), sourceinfo);
        self.info.push(info);
        Ok(func_id)
    }

    fn add_method_iseq(
        &mut self,
        name: Option<IdentId>,
        args: ParamsInfo,
        sourceinfo: SourceInfoRef,
    ) -> FuncId {
        let func_id = self.next_func_id();
        let info = FuncInfo::new_method_iseq(name, Some(func_id), args, sourceinfo);
        self.info.push(info);
        func_id
    }

    fn add_native_func(&mut self, name: String, address: BuiltinFn, arity: i32) -> FuncId {
        let id = self.next_func_id();
        self.info
            .push(FuncInfo::new_native(id, name, address, arity));
        id
    }

    fn add_attr_reader(&mut self, name: IdentId, ivar_name: IdentId) -> FuncId {
        let id = self.next_func_id();
        let info = FuncInfo::new_attr_reader(id, name, ivar_name);
        self.info.push(info);
        id
    }

    fn add_attr_writer(&mut self, name: IdentId, ivar_name: IdentId) -> FuncId {
        let id = self.next_func_id();
        let info = FuncInfo::new_attr_writer(id, name, ivar_name);
        self.info.push(info);
        id
    }

    fn handle_args(
        &mut self,
        info: BlockInfo,
        for_params: Vec<(usize, BcLocal, IdentId)>,
        sourceinfo: &SourceInfoRef,
    ) -> Result<ParamsInfo> {
        let BlockInfo {
            params, box body, ..
        } = info;
        let mut args_names = vec![];
        let mut keyword_names = vec![];
        let mut keyword_initializers = vec![];
        let mut destruct_args = vec![];
        let mut expand = vec![];
        let mut optional_info = vec![];
        let mut required_num = 0;
        let mut optional_num = 0;
        let mut rest = 0;
        let mut block_param = None;
        let mut for_param_info = vec![];
        for (dst_outer, dst_reg, _name) in for_params {
            for_param_info.push(ForParamInfo {
                dst_outer,
                dst_reg,
                src_reg: args_names.len(),
            });
            args_names.push(None);
            required_num += 1;
        }
        for param in params {
            match param.kind {
                ParamKind::Param(name) => {
                    args_names.push(Some(IdentId::get_id_from_string(name)));
                    required_num += 1;
                }
                ParamKind::Destruct(names) => {
                    expand.push((args_names.len(), destruct_args.len(), names.len()));
                    args_names.push(None);
                    required_num += 1;
                    names.into_iter().for_each(|(name, _)| {
                        destruct_args.push(Some(IdentId::get_id_from_string(name)));
                    });
                }
                ParamKind::Optional(name, box initializer) => {
                    let local = BcLocal(args_names.len() as u16);
                    args_names.push(Some(IdentId::get_id_from_string(name)));
                    optional_num += 1;
                    optional_info.push(OptionalInfo { local, initializer });
                }
                ParamKind::Rest(name) => {
                    args_names.push(name.map(|n| IdentId::get_id_from_string(n)));
                    assert_eq!(0, rest);
                    rest = 1;
                }
                ParamKind::Keyword(name, init) => {
                    let name = IdentId::get_id_from_string(name);
                    args_names.push(Some(name));
                    keyword_names.push(name);
                    keyword_initializers.push(init);
                }
                ParamKind::Block(name) => {
                    let name = IdentId::get_id_from_string(name.clone());
                    args_names.push(Some(name));
                    block_param = Some(name);
                }
                _ => {
                    return Err(MonorubyErr::unsupported_parameter_kind(
                        param.kind,
                        param.loc,
                        sourceinfo.clone(),
                    ))
                }
            }
        }

        let reqopt_num = required_num + optional_num;
        let expand_info: Vec<_> = expand
            .into_iter()
            .map(|(src, dst, len)| DestructureInfo {
                src,
                dst: args_names.len() + dst,
                len,
            })
            .collect();
        args_names.append(&mut destruct_args);
        self.compile_info.push(CompileInfo::new(
            body,
            keyword_initializers,
            expand_info,
            optional_info,
            for_param_info,
        ));
        Ok(ParamsInfo {
            args_names,
            keyword_names,
            pos_num: reqopt_num + rest,
            reqopt_num,
            required_num,
            block_param,
        })
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

#[derive(Default)]
pub struct FnStore {
    functions: Funcs,
    inline: HashMap<FuncId, InlineMethod>,
    /// call site info.
    callsite_info: Vec<CallSiteInfo>,
    /// const access site info.
    constsite_info: Vec<ConstSiteInfo>,
}

impl std::ops::Index<FuncId> for FnStore {
    type Output = FuncInfo;
    fn index(&self, index: FuncId) -> &FuncInfo {
        &self.functions[index]
    }
}

impl std::ops::IndexMut<FuncId> for FnStore {
    fn index_mut(&mut self, index: FuncId) -> &mut FuncInfo {
        &mut self.functions[index]
    }
}

impl std::ops::Index<ConstSiteId> for FnStore {
    type Output = ConstSiteInfo;
    fn index(&self, index: ConstSiteId) -> &ConstSiteInfo {
        &self.constsite_info[index.0 as usize]
    }
}

impl std::ops::IndexMut<ConstSiteId> for FnStore {
    fn index_mut(&mut self, index: ConstSiteId) -> &mut ConstSiteInfo {
        &mut self.constsite_info[index.0 as usize]
    }
}

impl std::ops::Index<CallSiteId> for FnStore {
    type Output = CallSiteInfo;
    fn index(&self, index: CallSiteId) -> &CallSiteInfo {
        &self.callsite_info[index.0 as usize]
    }
}

impl std::ops::IndexMut<CallSiteId> for FnStore {
    fn index_mut(&mut self, index: CallSiteId) -> &mut CallSiteInfo {
        &mut self.callsite_info[index.0 as usize]
    }
}

impl alloc::GC<RValue> for FnStore {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.functions.mark(alloc);
    }
}

impl FnStore {
    pub(super) fn new() -> Self {
        Self {
            functions: Funcs::default(),
            inline: HashMap::default(),
            constsite_info: vec![],
            callsite_info: vec![],
        }
    }

    #[cfg(feature = "emit-bc")]
    pub(super) fn functions(&self) -> &Vec<FuncInfo> {
        &self.functions.info
    }

    pub(crate) fn len(&self) -> usize {
        self.functions.info.len()
    }

    pub(in crate::executor) fn get_init(&mut self) -> CompileInfo {
        self.functions.compile_info.remove(0)
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
}

impl FnStore {
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

    pub fn callsite_offset(&self) -> usize {
        self.callsite_info.len()
    }

    pub fn functions_offset(&self) -> usize {
        self.functions.info.len()
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
                .map(|(range, dest, err_reg)| {
                    let start = info.get_pc_index(Some(range.start));
                    let end = info.get_pc_index(Some(range.end));
                    let dest = info.get_pc_index(Some(*dest));
                    (start..end, dest, err_reg)
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
        BcPc,                  // destination pc
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

    /// set bytecode.
    pub(in crate::executor) fn set_bytecode(&mut self, bc: Vec<Bc>) {
        self.bytecode = Some(Box::into_pin(bc.into_boxed_slice()));
    }

    /// get bytecode address.
    /*pub(in crate::executor) fn get_bytecode_address(&self, index: usize) -> BcPc {
        BcPcBase::new(self) + index
    }*/

    /// get a number of registers.
    pub(crate) fn total_reg_num(&self) -> usize {
        1 + (self.non_temp_num + self.temp_num) as usize
    }

    /// get a number of non-temp registers.
    pub(crate) fn local_num(&self) -> usize {
        self.non_temp_num as usize
    }

    /// get a number of keyword arguments.
    pub(crate) fn key_num(&self) -> usize {
        self.args.keyword_names.len()
    }

    /// get a position of keyword arguments.
    pub(crate) fn block_pos(&self) -> usize {
        if self.args.block_param.is_some() {
            self.args.pos_num + self.key_num() + 1
        } else {
            0
        }
    }

    /// get a number of required arguments.
    pub(crate) fn req_num(&self) -> usize {
        self.args.required_num
    }

    /// get a number of required + optional arguments.
    pub(crate) fn reqopt_num(&self) -> usize {
        self.args.reqopt_num
    }

    /// get a number of required + optional + rest arguments.
    pub(crate) fn pos_num(&self) -> usize {
        self.args.pos_num
    }

    /// bit 0:rest(yes=1 no =0) bit 1:block
    pub(crate) fn info(&self) -> usize {
        (if self.args.block_param.is_some() {
            2
        } else {
            0
        }) + (self.args.pos_num - self.args.reqopt_num)
    }

    /// get a block argument name.
    pub(crate) fn block_param_name(&self) -> Option<IdentId> {
        self.args.block_param
    }

    /// get name.
    pub(crate) fn name(&self) -> String {
        match &self.name {
            Some(name) => name.to_string(),
            None => "<unnamed>".to_string(),
        }
    }

    /// get bytecode.
    pub(in crate::executor) fn bytecode(&self) -> &[Bc] {
        self.bytecode.as_ref().unwrap()
    }

    pub(in crate::executor) fn get_pc(&self, idx: usize) -> BcPc {
        BcPc::from(&self.bytecode()[idx])
    }

    pub(in crate::executor) fn get_pc_index(&self, pc: Option<BcPc>) -> usize {
        if let Some(pos) = pc {
            pos - self.get_pc(0)
        } else {
            0
        }
    }

    pub(in crate::executor) fn get_exception_dest(
        &self,
        pc: BcPc,
    ) -> Option<(BcPc, Option<SlotId>)> {
        self.exception_map
            .iter()
            .filter_map(|(range, dest, slot)| {
                if range.contains(&pc) {
                    Some((*dest, *slot))
                } else {
                    None
                }
            })
            .nth(0)
    }

    pub(in crate::executor) fn exception_push(
        &mut self,
        range: std::ops::Range<BcPc>,
        dest: BcPc,
        err_reg: Option<SlotId>,
    ) {
        self.exception_map.push((range, dest, err_reg));
    }

    /// get bytecode length.
    pub(crate) fn bytecode_len(&self) -> usize {
        self.bytecode().len()
    }

    /// get bytecode address.
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
