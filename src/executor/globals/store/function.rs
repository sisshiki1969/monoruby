use super::*;

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

pub(crate) struct Funcs {
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
    #[cfg(feature = "emit-bc")]
    pub(super) fn functions(&self) -> &[FuncInfo] {
        &self.info
    }

    pub(super) fn function_len(&self) -> usize {
        self.info.len()
    }

    pub(super) fn get_compile_info(&mut self) -> CompileInfo {
        self.compile_info.remove(0)
    }

    pub(super) fn add_method(
        &mut self,
        name: Option<IdentId>,
        info: BlockInfo,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        let args = self.handle_args(info, vec![], &sourceinfo)?;
        Ok(self.add_method_iseq(name, args, sourceinfo))
    }

    pub(super) fn add_block(
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

    pub(super) fn add_classdef(
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

    pub(super) fn add_native_func(
        &mut self,
        name: String,
        address: BuiltinFn,
        arity: i32,
    ) -> FuncId {
        let id = self.next_func_id();
        self.info
            .push(FuncInfo::new_native(id, name, address, arity));
        id
    }

    pub(super) fn add_attr_reader(&mut self, name: IdentId, ivar_name: IdentId) -> FuncId {
        let id = self.next_func_id();
        let info = FuncInfo::new_attr_reader(id, name, ivar_name);
        self.info.push(info);
        id
    }

    pub(super) fn add_attr_writer(&mut self, name: IdentId, ivar_name: IdentId) -> FuncId {
        let id = self.next_func_id();
        let info = FuncInfo::new_attr_writer(id, name, ivar_name);
        self.info.push(info);
        id
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

    fn next_func_id(&self) -> FuncId {
        FuncId::new(self.info.len() as u32)
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

    pub(crate) fn is_ruby_func(&mut self) -> bool {
        match &mut self.kind {
            FuncKind::ISeq(_) => true,
            _ => false,
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
        eprintln!("{:?}", info.get_exception_map());
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