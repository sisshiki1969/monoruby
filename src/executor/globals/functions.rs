use ruruby_parse::{ArgList, CaseBranch, FormalParam, RescueEntry};

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

#[derive(Clone)]
pub(super) struct Funcs(Vec<FuncInfo>);

impl std::ops::Index<FuncId> for Funcs {
    type Output = FuncInfo;
    fn index(&self, index: FuncId) -> &FuncInfo {
        &self.0[u32::from(index) as usize]
    }
}

impl std::ops::IndexMut<FuncId> for Funcs {
    fn index_mut(&mut self, index: FuncId) -> &mut FuncInfo {
        &mut self.0[u32::from(index) as usize]
    }
}

impl alloc::GC<RValue> for Funcs {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.0.iter().for_each(|info| info.mark(alloc))
    }
}

impl Funcs {
    fn default() -> Self {
        Self(vec![FuncInfo::new_method_iseq(
            None,
            None,
            ArgumentsInfo::default(),
            Node::new_nil(Loc(0, 0)),
            SourceInfoRef::default(),
        )])
    }

    fn next_func_id(&self) -> FuncId {
        FuncId::new(self.0.len() as u32)
    }

    pub(crate) fn add_method(
        &mut self,
        name: Option<String>,
        info: BlockInfo,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        let args = handle_args(vec![], info.params, &sourceinfo)?;
        Ok(self.add_method_iseq(name, args, *info.body, sourceinfo))
    }

    fn add_block(
        &mut self,
        outer: (FuncId, Vec<(HashMap<String, u16>, Option<String>)>),
        for_params: Vec<(usize, BcLocal, String)>,
        info: BlockInfo,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        let args = handle_args(for_params, info.params, &sourceinfo)?;
        let func_id = self.next_func_id();
        self.0.push(FuncInfo::new_block_iseq(
            Some(func_id),
            outer,
            args,
            *info.body,
            sourceinfo,
        ));
        Ok(func_id)
    }

    fn add_classdef(
        &mut self,
        name: Option<String>,
        body: Node,
        sourceinfo: SourceInfoRef,
    ) -> FuncId {
        let func_id = self.next_func_id();
        self.0.push(FuncInfo::new_classdef_iseq(
            name,
            Some(func_id),
            body,
            sourceinfo,
        ));
        func_id
    }

    fn add_method_iseq(
        &mut self,
        name: Option<String>,
        args: ArgumentsInfo,
        body: Node,
        sourceinfo: SourceInfoRef,
    ) -> FuncId {
        let func_id = self.next_func_id();
        self.0.push(FuncInfo::new_method_iseq(
            name,
            Some(func_id),
            args,
            body,
            sourceinfo,
        ));
        func_id
    }

    fn add_native_func(&mut self, name: String, address: BuiltinFn, arity: i32) -> FuncId {
        let id = self.next_func_id();
        self.0.push(FuncInfo::new_native(id, name, address, arity));
        id
    }

    fn add_attr_reader(&mut self, name: String, ivar_name: IdentId) -> FuncId {
        let id = self.next_func_id();
        self.0.push(FuncInfo::new_attr_reader(id, name, ivar_name));
        id
    }

    fn add_attr_writer(&mut self, name: String, ivar_name: IdentId) -> FuncId {
        let id = self.next_func_id();
        self.0.push(FuncInfo::new_attr_writer(id, name, ivar_name));
        id
    }
}

fn handle_args(
    for_params: Vec<(usize, BcLocal, String)>,
    params: Vec<FormalParam>,
    sourceinfo: &SourceInfoRef,
) -> Result<ArgumentsInfo> {
    let mut args_names = vec![];
    let mut keyword_args = vec![];
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
        //args_names.push(Some(name));
        required_num += 1;
    }
    for param in params {
        match param.kind {
            ParamKind::Param(name) => {
                args_names.push(Some(name));
                required_num += 1;
            }
            ParamKind::Destruct(names) => {
                expand.push((args_names.len(), destruct_args.len(), names.len()));
                args_names.push(None);
                required_num += 1;
                names.into_iter().for_each(|(name, _)| {
                    destruct_args.push(Some(name));
                });
            }
            ParamKind::Optional(name, box initializer) => {
                let local = BcLocal(args_names.len() as u16);
                args_names.push(Some(name));
                optional_num += 1;
                optional_info.push(OptionalInfo { local, initializer });
            }
            ParamKind::Rest(name) => {
                args_names.push(Some(name));
                assert_eq!(0, rest);
                rest = 1;
            }
            ParamKind::Keyword(name, init) => {
                args_names.push(Some(name.clone()));
                let name = IdentId::get_ident_id_from_string(name);
                keyword_args.push((name, init));
            }
            ParamKind::Block(name) => {
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
        .map(|(src, dst, len)| ExpandInfo {
            src,
            dst: args_names.len() + dst,
            len,
        })
        .collect();
    args_names.append(&mut destruct_args);
    Ok(ArgumentsInfo {
        args_names,
        keyword_args,
        pos_num: reqopt_num + rest,
        reqopt_num,
        required_num,
        block_param,
        expand_info,
        optional_info,
        for_param_info,
    })
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

#[derive(Clone)]
pub struct FnStore {
    functions: Funcs,
    inline: HashMap<FuncId, InlineMethod>,
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
        }
    }

    #[cfg(feature = "emit-bc")]
    pub(super) fn functions(&self) -> &Vec<FuncInfo> {
        &self.functions.0
    }

    fn len(&self) -> usize {
        self.functions.0.len()
    }
}

impl FnStore {
    pub(crate) fn add_method(
        &mut self,
        name: Option<String>,
        info: BlockInfo,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        self.functions.add_method(name, info, sourceinfo)
    }

    pub(crate) fn add_classdef(
        &mut self,
        name: Option<String>,
        body: Node,
        sourceinfo: SourceInfoRef,
    ) -> FuncId {
        self.functions.add_classdef(name, body, sourceinfo)
    }

    pub(crate) fn add_block(
        &mut self,
        outer: (FuncId, Vec<(HashMap<String, u16>, Option<String>)>),
        optional_params: Vec<(usize, BcLocal, String)>,
        info: BlockInfo,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        self.functions
            .add_block(outer, optional_params, info, sourceinfo)
    }

    pub(crate) fn get_inline(&self, func_id: FuncId) -> Option<&InlineMethod> {
        self.inline.get(&func_id)
    }

    pub(super) fn compile_script(
        &mut self,
        ast: Node,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        let main_fid = self.functions.add_method_iseq(
            Some("/main".to_string()),
            ArgumentsInfo::default(),
            ast,
            sourceinfo,
        );
        let mut fid = main_fid;

        while self.len() > fid.get() as usize {
            self.compile_func(fid)?;
            fid = FuncId::new(fid.get() + 1);
        }

        Ok(main_fid)
    }

    pub(super) fn add_builtin_func(
        &mut self,
        name: String,
        address: BuiltinFn,
        arity: i32,
    ) -> FuncId {
        self.functions.add_native_func(name, address, arity)
    }

    pub(super) fn add_attr_reader(&mut self, name: String, ivar_name: IdentId) -> FuncId {
        self.functions.add_attr_reader(name, ivar_name)
    }

    pub(super) fn add_attr_writer(&mut self, name: String, ivar_name: IdentId) -> FuncId {
        self.functions.add_attr_writer(name, ivar_name)
    }

    pub(super) fn add_inline(&mut self, func_id: FuncId, inline_id: InlineMethod) {
        self.inline.insert(func_id, inline_id);
    }
}

impl FnStore {
    /// Generate bytecode for a function which has *func_id*.
    fn compile_func(&mut self, func_id: FuncId) -> Result<()> {
        let mut info = std::mem::take(self[func_id].as_ruby_func_mut());
        IrContext::compile_func(&mut info, self)?;
        //ir.ir_to_bytecode(&mut info, self);

        let regs = info.total_reg_num();
        std::mem::swap(&mut info, self[func_id].as_ruby_func_mut());
        self[func_id].data.pc = self[func_id].as_ruby_func().get_bytecode_address(0);
        self[func_id].data.set_reg_num(regs as i64);
        Ok(())
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
        match self {
            FuncKind::ISeq(info) => info.mark(alloc),
            _ => {}
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
pub const FUNCDATA_OFFSET_PC: u64 = 16;

#[derive(Debug, Clone, Default)]
pub struct FuncInfo {
    /// name of this function.
    name: Option<String>,
    /// arity of this function.
    /// -1 for variable numbers.
    arity: i32,
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
        name: impl Into<Option<String>>,
        func_id: Option<FuncId>,
        args: ArgumentsInfo,
        body: Node,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        let name = name.into();
        let info = ISeqInfo::new_method(func_id, name.clone(), args, body, sourceinfo);
        Self {
            name,
            arity: info.args.arity(),
            data: FuncData {
                codeptr: None,
                pc: BcPc::default(),
                meta: Meta::vm_method(func_id, 0),
            },
            kind: FuncKind::ISeq(info),
        }
    }

    fn new_block_iseq(
        func_id: Option<FuncId>,
        outer: (FuncId, Vec<(HashMap<String, u16>, Option<String>)>),
        args: ArgumentsInfo,
        body: Node,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        let info = ISeqInfo::new_block(func_id, outer, args, body, sourceinfo);
        Self {
            name: None,
            arity: info.args.arity(),
            data: FuncData {
                codeptr: None,
                pc: BcPc::default(),
                meta: Meta::vm_method(func_id, 0),
            },
            kind: FuncKind::ISeq(info),
        }
    }

    fn new_classdef_iseq(
        name: Option<String>,
        func_id: Option<FuncId>,
        body: Node,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        let info = ISeqInfo::new_method(
            func_id,
            name.clone(),
            ArgumentsInfo::default(),
            body,
            sourceinfo,
        );
        Self {
            name,
            arity: info.args.arity(),
            data: FuncData {
                codeptr: None,
                pc: BcPc::default(),
                meta: Meta::vm_classdef(func_id, 0),
            },
            kind: FuncKind::ISeq(info),
        }
    }

    fn new_native(func_id: FuncId, name: String, address: BuiltinFn, arity: i32) -> Self {
        let reg_num = if arity == -1 { -1 } else { arity as i64 };
        Self {
            name: Some(name),
            arity,
            data: FuncData {
                codeptr: None,
                pc: BcPc::default(),
                meta: Meta::native(func_id, reg_num),
            },
            kind: FuncKind::Builtin {
                abs_address: address as *const u8 as u64,
            },
        }
    }

    fn new_attr_reader(func_id: FuncId, name: String, ivar_name: IdentId) -> Self {
        Self {
            name: Some(name),
            arity: 0,
            data: FuncData {
                codeptr: None,
                pc: BcPc::default(),
                meta: Meta::native(func_id, 0),
            },
            kind: FuncKind::AttrReader { ivar_name },
        }
    }

    fn new_attr_writer(func_id: FuncId, name: String, ivar_name: IdentId) -> Self {
        Self {
            name: Some(name),
            arity: 1,
            data: FuncData {
                codeptr: None,
                pc: BcPc::default(),
                meta: Meta::native(func_id, 1),
            },
            kind: FuncKind::AttrWriter { ivar_name },
        }
    }

    pub(crate) fn name(&self) -> Option<&String> {
        self.name.as_ref()
    }

    pub(crate) fn arity(&self) -> i32 {
        self.arity
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
            info.id,
            match &self.name {
                Some(name) => name,
                None => "<ANONYMOUS>",
            },
            BcPcBase::new(info),
            self.data.meta,
            self.kind
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
    name: Option<String>,
    /// Bytecode.
    pub(super) bytecode: Option<Pin<Box<[Bc]>>>,
    /// Source map.
    pub sourcemap: Vec<Loc>,
    /// the name of arguments.
    pub(in crate::executor) args: ArgumentsInfo,
    /// local variables.
    locals: HashMap<String, u16>,
    /// outer local variables. (dynamic_locals, block_param)
    outer_locals: Vec<(HashMap<String, u16>, Option<String>)>,
    /// literal values. (for GC)
    pub literals: Vec<Value>,
    /// The current register id.
    pub temp: u16,
    /// The number of temporary registers.
    temp_num: u16,
    pub lexical_context: Vec<Module>,
    /// AST.
    pub ast: Option<Node>,
    pub sourceinfo: SourceInfoRef,
    pub(crate) is_lambda: bool,
}

impl std::fmt::Debug for ISeqInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "RubyFuncInfo {{ id: {}, name: {:?}. locals: {:?} }}",
            self.id().get(),
            self.name,
            self.locals
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
        outer_locals: Vec<(HashMap<String, u16>, Option<String>)>,
        name: Option<String>,
        args: ArgumentsInfo,
        body: Node,
        sourceinfo: SourceInfoRef,
        is_block: bool,
    ) -> Self {
        let mut info = ISeqInfo {
            id,
            name,
            bytecode: None,
            sourcemap: vec![],
            args: args.clone(),
            locals: HashMap::default(),
            outer_locals,
            literals: vec![],
            temp: 0,
            temp_num: 0,
            lexical_context: vec![],
            ast: Some(body),
            sourceinfo,
            is_lambda: is_block,
        };
        args.args_names.into_iter().for_each(|name| {
            info.add_local(name);
        });
        if let Some(name) = args.block_param {
            info.add_local(name);
        }
        info
    }

    pub(in crate::executor) fn new_block(
        id: Option<FuncId>,
        outer: (FuncId, Vec<(HashMap<String, u16>, Option<String>)>),
        args: ArgumentsInfo,
        body: Node,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        Self::new(id, outer.1, None, args, body, sourceinfo, true)
    }

    pub(in crate::executor) fn new_method(
        id: Option<FuncId>,
        name: Option<String>,
        args: ArgumentsInfo,
        body: Node,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        Self::new(id, vec![], name, args, body, sourceinfo, false)
    }

    pub(crate) fn id(&self) -> FuncId {
        self.id.unwrap()
    }

    /// set bytecode.
    pub(crate) fn set_bytecode(&mut self, bc: Vec<Bc>) {
        self.bytecode = Some(Box::into_pin(bc.into_boxed_slice()));
    }

    /// get bytecode address.
    pub(in crate::executor) fn get_bytecode_address(&self, index: usize) -> BcPc {
        BcPcBase::new(self) + index
    }

    /// get a number of registers.
    pub(crate) fn total_reg_num(&self) -> usize {
        1 + self.locals.len() + self.temp_num as usize
    }

    /// get a number of local vars.
    pub(crate) fn local_num(&self) -> usize {
        self.locals.len()
    }

    /// get a number of keyword arguments.
    pub(crate) fn key_num(&self) -> usize {
        self.args.keyword_args.len()
    }

    /// get a number of required arguments.
    pub(crate) fn req_num(&self) -> usize {
        self.args.required_num
    }

    /// get a number of required + optional arguments.
    pub(crate) fn reqopt_num(&self) -> usize {
        self.args.reqopt_num
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
    pub(crate) fn block_param_name(&self) -> Option<&String> {
        self.args.block_param.as_ref()
    }

    /// get a outer block argument name.
    pub(crate) fn outer_block_param_name(&self, outer: usize) -> Option<&String> {
        self.outer_locals[outer - 1].1.as_ref()
    }

    /// get name.
    #[cfg(any(feature = "emit-asm", feature = "log-jit", feature = "emit-tir"))]
    pub(crate) fn name(&self) -> String {
        match &self.name {
            Some(name) => name,
            None => "<unnamed>",
        }
        .to_string()
    }

    /// get bytecode.
    pub(crate) fn bytecode(&self) -> &[Bc] {
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

    /// get bytecode length.
    pub(crate) fn bytecode_len(&self) -> usize {
        self.bytecode().len()
    }

    /// get bytecode address.
    pub(crate) fn bytecode_top(&self) -> *const Bc {
        self.bytecode().as_ptr()
    }

    pub(crate) fn get_locals(&self) -> Vec<(HashMap<String, u16>, Option<String>)> {
        let mut locals = vec![(self.locals.clone(), self.block_param_name().cloned())];
        locals.extend_from_slice(&self.outer_locals);
        locals
    }

    /// get the next register id.
    pub(crate) fn next_reg(&self) -> BcTemp {
        BcTemp(self.temp)
    }

    pub(crate) fn push(&mut self) -> BcTemp {
        let reg = BcTemp(self.temp);
        self.temp += 1;
        if self.temp > self.temp_num {
            self.temp_num = self.temp;
        }
        reg
    }

    pub(crate) fn pop(&mut self) -> BcTemp {
        self.temp -= 1;
        BcTemp(self.temp)
    }

    pub(crate) fn popn(&mut self, len: usize) {
        self.temp -= len as u16;
    }

    pub(crate) fn assign_local(&mut self, ident: &str) -> BcLocal {
        match self.locals.get(ident) {
            Some(local) => BcLocal(*local),
            None => self.add_local(ident.to_owned()),
        }
    }

    pub(crate) fn refer_local(&mut self, ident: &str) -> BcLocal {
        match self.locals.get(ident) {
            Some(local) => BcLocal(*local),
            None => panic!("undefined local var `{}`", ident),
        }
    }

    pub(crate) fn refer_dynamic_local(&self, outer: usize, ident: &str) -> BcLocal {
        BcLocal(*self.outer_locals[outer - 1].0.get(ident).expect(&format!(
            "Bytecodegen: dynamic local was not found. {outer} {ident} {:?} {:?}",
            self.outer_locals, self.locals
        )))
    }

    /// Add a variable identifier without checking duplicates.
    fn add_local(&mut self, ident: impl Into<Option<String>>) -> BcLocal {
        let ident = match ident.into() {
            Some(ident) => ident,
            None => "/".to_string(),
        };
        let local = self.locals.len() as u16;
        assert!(self.locals.insert(ident, local).is_none());
        BcLocal(local)
    }

    pub(crate) fn is_assign_local(&mut self, node: &Node) -> Option<BcLocal> {
        if let NodeKind::LocalVar(0, name) = &node.kind {
            Some(self.assign_local(name))
        } else {
            None
        }
    }

    pub(crate) fn is_refer_local(&mut self, node: &Node) -> Option<BcLocal> {
        if let NodeKind::LocalVar(0, name) = &node.kind {
            Some(self.refer_local(name))
        } else {
            None
        }
    }

    pub(crate) fn level_down(&mut self, node: &mut Node, level: usize) {
        match &mut node.kind {
            NodeKind::LocalVar(l, _) => {
                if *l >= level {
                    *l += 1;
                }
            }
            NodeKind::MulAssign(n1, n2) => {
                n1.into_iter().for_each(|n| {
                    if level == 0 {
                        if let NodeKind::LocalVar(0, name) = &n.kind {
                            self.assign_local(&name);
                        }
                    }
                    self.level_down(n, level);
                });
                n2.into_iter().for_each(|n| self.level_down(n, level));
            }
            NodeKind::Lambda(BlockInfo { params, body, .. }) => {
                self.level_down(body, level + 1);
                for p in params {
                    match &mut p.kind {
                        ParamKind::Optional(_, n) => {
                            self.level_down(n, level);
                        }
                        ParamKind::Keyword(_, Some(n)) => {
                            self.level_down(n, level);
                        }
                        _ => {}
                    }
                }
            }
            NodeKind::SelfValue
            | NodeKind::Nil
            | NodeKind::Integer(_)
            | NodeKind::Bignum(_)
            | NodeKind::Float(_)
            | NodeKind::Imaginary(_)
            | NodeKind::Bool(_)
            | NodeKind::String(_)
            | NodeKind::Symbol(_)
            | NodeKind::Ident(_)
            | NodeKind::InstanceVar(_)
            | NodeKind::GlobalVar(_)
            | NodeKind::SpecialVar(_)
            | NodeKind::ClassVar(_)
            | NodeKind::MethodDef(..)
            | NodeKind::SingletonMethodDef(..)
            | NodeKind::ClassDef { .. }
            | NodeKind::SingletonClassDef { .. } => {}
            NodeKind::CompStmt(nodes)
            | NodeKind::InterporatedString(nodes)
            | NodeKind::Array(nodes, ..)
            | NodeKind::RegExp(nodes, ..) => {
                nodes.into_iter().for_each(|n| self.level_down(n, level));
            }
            NodeKind::Command(n)
            | NodeKind::UnOp(_, n)
            | NodeKind::Splat(n)
            | NodeKind::Break(n)
            | NodeKind::Next(n)
            | NodeKind::Return(n)
            | NodeKind::Defined(n) => {
                self.level_down(n, level);
            }
            NodeKind::Const { parent, .. } => {
                if let Some(n) = parent {
                    self.level_down(n, level);
                }
            }
            NodeKind::BinOp(_, box n1, box n2)
            | NodeKind::AssignOp(_, box n1, box n2)
            | NodeKind::Range {
                start: box n1,
                end: box n2,
                ..
            }
            | NodeKind::While {
                cond: box n1,
                body: box n2,
                ..
            }
            | NodeKind::AliasMethod(box n1, box n2) => {
                self.level_down(n1, level);
                self.level_down(n2, level);
            }
            NodeKind::If {
                cond: n1,
                then_: n2,
                else_: n3,
            } => {
                self.level_down(n1, level);
                self.level_down(n2, level);
                self.level_down(n3, level);
            }
            NodeKind::Hash(pairs, ..) => pairs.into_iter().for_each(|(n1, n2)| {
                self.level_down(n1, level);
                self.level_down(n2, level);
            }),
            NodeKind::FuncCall { arglist, .. } | NodeKind::Yield(arglist) => {
                self.level_down_arglist(arglist, level);
            }
            NodeKind::MethodCall {
                receiver, arglist, ..
            } => {
                self.level_down(receiver, level);
                self.level_down_arglist(arglist, level);
            }
            NodeKind::Index { base, index } => {
                self.level_down(base, level);
                index.into_iter().for_each(|n| self.level_down(n, level));
            }
            NodeKind::For { param, iter, body } => {
                for (outer, name) in param {
                    if level == *outer {
                        self.assign_local(name);
                    }
                    if *outer >= level {
                        *outer += 1;
                    }
                }
                self.level_down(iter, level);
                let BlockInfo { params, body, .. } = body;
                self.level_down(body, level);
                for p in params {
                    match &mut p.kind {
                        ParamKind::Optional(_, n) => {
                            self.level_down(n, level);
                        }
                        ParamKind::Keyword(_, Some(n)) => {
                            self.level_down(n, level);
                        }
                        _ => {}
                    }
                }
            }
            NodeKind::Case { cond, when_, else_ } => {
                if let Some(n) = cond {
                    self.level_down(n, level);
                }
                self.level_down(else_, level);
                for CaseBranch { when, body } in when_ {
                    when.into_iter().for_each(|n| self.level_down(n, level));
                    self.level_down(body, level);
                }
            }
            NodeKind::Super(args) => {
                if let Some(arglist) = args {
                    self.level_down_arglist(arglist, level);
                }
            }
            NodeKind::Begin {
                body,
                rescue,
                else_,
                ensure,
            } => {
                self.level_down(body, level);
                for RescueEntry {
                    exception_list,
                    assign,
                    body,
                } in rescue
                {
                    exception_list
                        .into_iter()
                        .for_each(|n| self.level_down(n, level));
                    if let Some(n) = assign {
                        self.level_down(n, level);
                    }
                    self.level_down(body, level);
                }
                if let Some(n) = else_ {
                    self.level_down(n, level);
                }
                if let Some(n) = ensure {
                    self.level_down(n, level);
                }
            }
        }
    }

    fn level_down_arglist(&mut self, arglist: &mut ArgList, level: usize) {
        let ArgList {
            args,
            kw_args,
            hash_splat,
            block,
            ..
        } = arglist;
        args.into_iter().for_each(|n| self.level_down(n, level));
        kw_args
            .into_iter()
            .for_each(|(_, n)| self.level_down(n, level));
        hash_splat
            .into_iter()
            .for_each(|n| self.level_down(n, level));
        if let Some(n) = block {
            self.level_down(n, level);
        }
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

impl ISeqInfo {
    pub(in crate::executor) fn get_index(&self, reg: &BcReg) -> SlotId {
        let id = match reg {
            BcReg::Self_ => 0,
            BcReg::Temp(i) => 1 + self.locals.len() as u16 + i.0,
            BcReg::Local(i) => 1 + i.0,
        };
        SlotId(id)
    }

    pub(crate) fn add_constsite(
        &self,
        store: &mut FnStore,
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
        let id = store.constsite_info.len();
        store.constsite_info.push(info);
        ConstSiteId(id as u32)
    }
}
