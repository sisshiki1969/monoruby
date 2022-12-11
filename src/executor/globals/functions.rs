use ruruby_parse::FormalParam;

use super::*;
use std::pin::Pin;

///
/// ID of function.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(transparent)]
pub struct FuncId(pub u32);

impl From<FuncId> for u32 {
    fn from(id: FuncId) -> u32 {
        id.0
    }
}

#[derive(Clone, Default, PartialEq)]
pub struct ArgumentNames {
    // req + opt + rest
    pub arg_num: usize,
    // req + optional
    pub pos_num: usize,
    pub req_num: usize,
    pub names: Vec<Option<String>>,
}

impl ArgumentNames {
    fn arity(&self) -> i32 {
        if self.pos_num == self.req_num && self.pos_num == self.arg_num {
            self.pos_num as i32
        } else {
            -1
        }
    }
}

#[derive(Clone, Default, PartialEq)]
pub struct ExpandInfo {
    pub src: usize,
    pub dst: usize,
    pub len: usize,
}

#[derive(Clone, Default, PartialEq)]
pub(crate) struct OptionalInfo {
    pub(crate) local: BcLocal,
    pub(crate) initializer: Node,
}

#[derive(Clone, PartialEq)]
pub(crate) struct Funcs(Vec<FuncInfo>);

impl std::ops::Index<FuncId> for Funcs {
    type Output = FuncInfo;
    fn index(&self, index: FuncId) -> &FuncInfo {
        &self.0[index.0 as usize]
    }
}

impl std::ops::IndexMut<FuncId> for Funcs {
    fn index_mut(&mut self, index: FuncId) -> &mut FuncInfo {
        &mut self.0[index.0 as usize]
    }
}

impl Funcs {
    fn default() -> Self {
        Self(vec![FuncInfo::new_iseq(
            None,
            FuncId(0),
            None,
            ArgumentNames::default(),
            vec![],
            vec![],
            Node::new_nil(Loc(0, 0)),
            SourceInfoRef::default(),
            false,
        )])
    }

    fn next_func_id(&self) -> FuncId {
        FuncId(self.0.len() as u32)
    }

    pub(crate) fn add_method(
        &mut self,
        name: Option<String>,
        info: BlockInfo,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        let (args, expand, optional) = handle_args(info.params, &sourceinfo)?;
        Ok(self.add_iseq(
            None, name, args, expand, optional, *info.body, sourceinfo, false,
        ))
    }

    pub(crate) fn add_block(
        &mut self,
        outer: (FuncId, Vec<HashMap<String, u16>>),
        info: BlockInfo,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        let (args, expand, optional) = handle_args(info.params, &sourceinfo)?;
        Ok(self.add_iseq(
            Some(outer),
            None,
            args,
            expand,
            optional,
            *info.body,
            sourceinfo,
            false,
        ))
    }

    pub(crate) fn add_classdef(
        &mut self,
        name: Option<String>,
        body: Node,
        sourceinfo: SourceInfoRef,
    ) -> FuncId {
        self.add_iseq(
            None,
            name,
            ArgumentNames::default(),
            vec![],
            vec![],
            body,
            sourceinfo,
            true,
        )
    }

    fn add_iseq(
        &mut self,
        outer: Option<(FuncId, Vec<HashMap<String, u16>>)>,
        name: Option<String>,
        args: ArgumentNames,
        expand: Vec<ExpandInfo>,
        optional: Vec<OptionalInfo>,
        body: Node,
        sourceinfo: SourceInfoRef,
        is_classdef: bool,
    ) -> FuncId {
        let func_id = self.next_func_id();
        self.0.push(FuncInfo::new_iseq(
            name,
            func_id,
            outer,
            args,
            expand,
            optional,
            body,
            sourceinfo,
            is_classdef,
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
    params: Vec<FormalParam>,
    sourceinfo: &SourceInfoRef,
) -> Result<(ArgumentNames, Vec<ExpandInfo>, Vec<OptionalInfo>)> {
    let mut args = vec![];
    let mut destruct_args = vec![];
    let mut expand = vec![];
    let mut optional = vec![];
    let mut req_num = 0;
    let mut rest = 0;
    for param in params {
        match param.kind {
            ParamKind::Param(name) => {
                args.push(Some(name));
                req_num += 1;
            }
            ParamKind::Optional(name, box initializer) => {
                let local = BcLocal(args.len() as u16);
                args.push(Some(name));
                optional.push(OptionalInfo { local, initializer });
            }
            ParamKind::Rest(name) => {
                args.push(Some(name));
                assert_eq!(0, rest);
                rest = 1;
            }
            ParamKind::Destruct(names) => {
                expand.push((args.len(), destruct_args.len(), names.len()));
                args.push(None);
                req_num += 1;
                names.into_iter().for_each(|(name, _)| {
                    destruct_args.push(Some(name));
                });
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
    let pos_num = args.len() - rest;
    let expand: Vec<_> = expand
        .into_iter()
        .map(|(src, dst, len)| ExpandInfo {
            src,
            dst: args.len() + dst,
            len,
        })
        .collect();
    args.append(&mut destruct_args);
    Ok((
        ArgumentNames {
            names: args,
            arg_num: pos_num + rest,
            pos_num,
            req_num,
        },
        expand,
        optional,
    ))
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

#[derive(Clone, PartialEq)]
pub(crate) struct FnStore {
    pub(crate) functions: Funcs,
    pub(crate) inline: HashMap<FuncId, InlineMethod>,
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
    pub(super) fn compile_script(
        &mut self,
        ast: Node,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        let main_fid = self.functions.add_iseq(
            None,
            Some("/main".to_string()),
            ArgumentNames::default(),
            vec![],
            vec![],
            ast,
            sourceinfo,
            false,
        );
        let mut fid = main_fid;

        while self.len() > fid.0 as usize {
            self.compile_func(fid)?;
            fid = FuncId(fid.0 + 1);
        }

        Ok(main_fid)
    }

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
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum FuncKind {
    ISeq(ISeqInfo),
    Builtin { abs_address: u64 },
    AttrReader { ivar_name: IdentId },
    AttrWriter { ivar_name: IdentId },
}

impl std::default::Default for FuncKind {
    fn default() -> Self {
        Self::Builtin { abs_address: 0 }
    }
}

pub const FUNCDATA_OFFSET_CODEPTR: u64 = 0;
pub const FUNCDATA_OFFSET_PC: u64 = 8;
pub const FUNCDATA_OFFSET_META: u64 = 16;

#[derive(Debug, Clone, PartialEq, Default)]
pub(crate) struct FuncInfo {
    /// name of this function.
    name: Option<String>,
    /// arity of this function.
    /// -1 for variable numbers.
    arity: i32,
    pub(crate) data: FuncData,
    pub(crate) kind: FuncKind,
}

impl FuncInfo {
    fn new_iseq(
        name: Option<String>,
        func_id: FuncId,
        outer: Option<(FuncId, Vec<HashMap<String, u16>>)>,
        args: ArgumentNames,
        expand: Vec<ExpandInfo>,
        optional: Vec<OptionalInfo>,
        body: Node,
        sourceinfo: SourceInfoRef,
        is_classdef: bool,
    ) -> Self {
        let info = if let Some(outer) = outer {
            ISeqInfo::new_block(
                func_id,
                outer,
                name.clone(),
                args,
                expand,
                optional,
                body,
                sourceinfo,
            )
        } else {
            ISeqInfo::new_method(
                func_id,
                name.clone(),
                args,
                expand,
                optional,
                body,
                sourceinfo,
            )
        };
        Self {
            name,
            arity: info.args.arity(),
            data: FuncData {
                codeptr: None,
                pc: BcPc::default(),
                meta: if is_classdef {
                    Meta::vm_classdef(info.id, 0)
                } else {
                    Meta::vm_method(info.id, 0)
                },
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

    fn as_ruby_func_mut(&mut self) -> &mut ISeqInfo {
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
            "{:?} name:{} pos_num:{:?} bc:{:?} meta:{:?}",
            info.id,
            match &self.name {
                Some(name) => name,
                None => "<ANONYMOUS>",
            },
            info.args.pos_num,
            BcPcBase::new(info),
            self.data.meta,
        );
        let mut skip = false;
        let bb_info = info.get_bb_info();
        for (i, pc) in info.bytecode().iter().enumerate() {
            let pc = BcPc::from(pc);
            if skip {
                skip = false;
                continue;
            }
            let bcop1 = pc.op1();
            if let TraceIr::MethodArgs(..) = bcop1 {
                skip = true;
                continue;
            };
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
#[derive(Clone, Default, PartialEq)]
pub(crate) struct ISeqInfo {
    /// ID of this function.
    pub(crate) id: FuncId,
    outer: Option<FuncId>,
    name: Option<String>,
    /// Bytecode.
    pub(super) bytecode: Option<Pin<Box<[Bc]>>>,
    /// Source map.
    pub sourcemap: Vec<Loc>,
    /// the name of arguments.
    args: ArgumentNames,
    /// expand array info. (src_reg, dst_reg, len)
    pub(crate) expand: Vec<ExpandInfo>,
    /// optional parameters initializer
    pub(crate) optional: Vec<OptionalInfo>,
    /// local variables.
    locals: HashMap<String, u16>,
    /// outer local variables.
    outer_locals: Vec<HashMap<String, u16>>,
    /// The current register id.
    pub temp: u16,
    /// The number of temporary registers.
    temp_num: u16,
    /// AST.
    pub ast: Option<Node>,
    pub sourceinfo: SourceInfoRef,
    pub(crate) is_block: bool,
}

impl std::fmt::Debug for ISeqInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "RubyFuncInfo {{ id: {}, name: {:?}. pos_num: {:?} }}",
            self.id.0, self.name, self.args.pos_num
        )
    }
}

impl ISeqInfo {
    pub(crate) fn new(
        id: FuncId,
        outer: Option<FuncId>,
        outer_locals: Vec<HashMap<String, u16>>,
        name: Option<String>,
        args: ArgumentNames,
        expand: Vec<ExpandInfo>,
        optional: Vec<OptionalInfo>,
        body: Node,
        sourceinfo: SourceInfoRef,
        is_block: bool,
    ) -> Self {
        let mut info = ISeqInfo {
            id,
            outer,
            name,
            bytecode: None,
            sourcemap: vec![],
            args: args.clone(),
            expand,
            optional,
            locals: HashMap::default(),
            outer_locals,
            temp: 0,
            temp_num: 0,
            ast: Some(body),
            sourceinfo,
            is_block,
        };
        args.names.into_iter().for_each(|name| {
            info.add_local(name);
        });
        info
    }

    pub(crate) fn new_block(
        id: FuncId,
        outer: (FuncId, Vec<HashMap<String, u16>>),
        name: Option<String>,
        args: ArgumentNames,
        expand: Vec<ExpandInfo>,
        optional: Vec<OptionalInfo>,
        body: Node,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        Self::new(
            id,
            Some(outer.0),
            outer.1,
            name,
            args,
            expand,
            optional,
            body,
            sourceinfo,
            true,
        )
    }

    pub(crate) fn new_method(
        id: FuncId,
        name: Option<String>,
        args: ArgumentNames,
        expand: Vec<ExpandInfo>,
        optional: Vec<OptionalInfo>,
        body: Node,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        Self::new(
            id,
            None,
            vec![],
            name,
            args,
            expand,
            optional,
            body,
            sourceinfo,
            false,
        )
    }

    /// set bytecode.
    pub(crate) fn set_bytecode(&mut self, bc: Vec<Bc>) {
        self.bytecode = Some(Box::into_pin(bc.into_boxed_slice()));
    }

    /// get bytecode address.
    pub(crate) fn get_bytecode_address(&self, index: usize) -> BcPc {
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

    /// get a number of required arguments.
    pub(crate) fn arg_num(&self) -> usize {
        self.args.arg_num
    }

    /// get a number of required arguments.
    pub(crate) fn req_num(&self) -> usize {
        self.args.req_num
    }

    /// get a number of positional arguments.
    pub(crate) fn pos_num(&self) -> usize {
        self.args.pos_num
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

    pub(crate) fn get_pc(&self, idx: usize) -> BcPc {
        BcPc::from(&self.bytecode()[idx])
    }

    pub(crate) fn get_pc_index(&self, pc: Option<BcPc>) -> usize {
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

    pub(crate) fn get_locals(&self) -> Vec<HashMap<String, u16>> {
        let mut locals = vec![self.locals.clone()];
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

    pub(crate) fn refer_dynamic_local(&self, outer: usize, ident: &str) -> u16 {
        *self.outer_locals[outer - 1].get(ident).unwrap()
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
        if let NodeKind::LocalVar(name) = &node.kind {
            Some(self.assign_local(name))
        } else {
            None
        }
    }

    pub(crate) fn is_refer_local(&mut self, node: &Node) -> Option<BcLocal> {
        if let NodeKind::LocalVar(name) = &node.kind {
            Some(self.refer_local(name))
        } else {
            None
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
        let mut skip = false;
        for (idx, pc) in self.bytecode().iter().enumerate() {
            let pc = BcPc::from(pc);
            if skip {
                skip = false;
                continue;
            }
            match pc.op1() {
                TraceIr::MethodArgs(..) => {
                    skip = true;
                }
                TraceIr::Br(disp)
                | TraceIr::CondBr(_, disp, _, _)
                | TraceIr::CheckLocal(_, disp) => {
                    info[((idx + 1) as i32 + disp) as usize].push(idx);
                }
                _ => {}
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
            if skip {
                skip = false;
                continue;
            }
            match pc.op1() {
                TraceIr::MethodArgs(..) => {
                    skip = true;
                    if let Some(ref mut elem) = bb_info[idx + 2] {
                        elem.1.push(idx);
                    }
                }
                TraceIr::Br(_) | TraceIr::Ret(_) => {}
                _ => {
                    if let Some(ref mut elem) = bb_info[idx + 1] {
                        elem.1.push(idx);
                    }
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
    pub(crate) fn get_index(&self, reg: &BcReg) -> SlotId {
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
