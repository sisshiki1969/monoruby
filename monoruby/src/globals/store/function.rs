use super::*;
use crate::bytecodegen::{BcLocal, CompileInfo, DestructureInfo, ForParamInfo, OptionalInfo};

pub(crate) const FUNCDATA_CODEPTR: u64 = std::mem::offset_of!(FuncData, codeptr) as _;
pub(crate) const FUNCDATA_META: u64 = std::mem::offset_of!(FuncData, meta) as _;
pub(crate) const FUNCDATA_REGNUM: u64 = FUNCDATA_META + META_REGNUM;
pub(crate) const FUNCDATA_PC: u64 = std::mem::offset_of!(FuncData, pc) as _;

pub(crate) const META_FUNCID: u64 = std::mem::offset_of!(Meta, func_id) as _;
pub(crate) const META_REGNUM: u64 = std::mem::offset_of!(Meta, reg_num) as _;
//pub(crate) const META_KIND: u64 = std::mem::offset_of!(Meta, kind) as _;

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

#[derive(Debug, Clone, PartialEq, Default)]
pub(crate) struct FuncData {
    /// address of function.
    codeptr: Option<monoasm::CodePtr>,
    /// metadata of this function.
    meta: Meta,
    /// the address of program counter
    pc: Option<BcPc>,
}

impl FuncData {
    pub fn pc(&self) -> Option<BcPc> {
        self.pc
    }

    pub(super) fn set_pc(&mut self, pc: BcPc) {
        self.pc = Some(pc);
    }

    pub(super) fn set_reg_num(&mut self, reg_num: u16) {
        self.meta.set_reg_num(reg_num);
    }

    pub(in crate::globals) fn set_codeptr(&mut self, codeptr: monoasm::CodePtr) {
        self.codeptr = Some(codeptr);
    }

    pub fn meta(&self) -> Meta {
        self.meta
    }

    pub fn codeptr(&self) -> Option<monoasm::CodePtr> {
        self.codeptr
    }
}

///
/// Metadata.
///
/// ~~~text
///   7   6   5   4    3   2    1    0
/// +-------+-------+---------+----+----+
/// |    FuncId     | reg_num |kind|mode|
/// +-------+-------+---------+----+----+
/// ~~~
///
#[derive(Clone, Copy, PartialEq, Eq, Default)]
#[repr(C)]
pub struct Meta {
    func_id: Option<FuncId>,
    reg_num: u16,
    /// bit 7:  0:on_stack 1:on_heap
    /// bit 3:  0:no eval 1:eval(which possibly manipulates stack slots in outer frames)
    /// bit 2:  0:method_style arg 1:block_style arg
    /// bit 1:  0:Ruby 1:native
    /// bit 0:  0:method 1:class_def
    kind: u8,
    mode: u8,
}

impl std::fmt::Debug for Meta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {} {:?} reg_num:{}",
            if self.is_native() { "NATIVE" } else { "Ruby" },
            if self.is_class_def() {
                "class_def"
            } else {
                "method"
            },
            if self.on_stack() { "stack" } else { "heap" },
            self.func_id(),
            self.reg_num()
        )
    }
}

impl Meta {
    pub fn new(
        func_id: Option<FuncId>,
        reg_num: u16,
        is_eval: bool,
        is_native: bool,
        is_class_def: bool,
        is_block_style: bool,
    ) -> Self {
        Self {
            func_id,
            reg_num,
            kind: (is_eval as u8) << 3
                | (is_block_style as u8) << 2
                | (is_native as u8) << 1
                | is_class_def as u8,
            mode: 0,
        }
    }

    pub fn get(&self) -> u64 {
        unsafe { std::mem::transmute(*self) }
    }

    fn vm_method(func_id: impl Into<Option<FuncId>>, reg_num: i64, is_block_style: bool) -> Self {
        let reg_num = reg_num as i16 as u16;
        Self::new(func_id.into(), reg_num, false, false, false, is_block_style)
    }

    fn vm_classdef(func_id: impl Into<Option<FuncId>>, reg_num: i64) -> Self {
        let reg_num = reg_num as i16 as u16;
        Self::new(func_id.into(), reg_num, false, false, true, false)
    }

    fn native(func_id: FuncId) -> Self {
        Self::new(Some(func_id), 1, false, true, false, false)
    }

    fn native_eval(func_id: FuncId) -> Self {
        Self::new(Some(func_id), 1, true, true, false, false)
    }

    pub fn func_id(&self) -> FuncId {
        self.func_id.unwrap()
    }

    pub fn reg_num(&self) -> i64 {
        self.reg_num as i16 as i64
    }

    ///
    /// Returns true if this function possibly manipulates outer local variables.
    ///
    pub fn is_eval(&self) -> bool {
        (self.kind & 0b1000) != 0
    }

    pub fn is_native(&self) -> bool {
        (self.kind & 0b10) != 0
    }

    pub fn on_stack(&self) -> bool {
        self.kind & 0b1000_0000 == 0
    }

    pub fn set_on_heap(&mut self) {
        self.kind |= 0b1000_0000;
    }

    /// method:0 class_def:1
    pub fn is_class_def(&self) -> bool {
        (self.kind & 0b1) == 1
    }

    ///
    /// Set the number of registers in Meta.
    ///
    pub fn set_reg_num(&mut self, reg_num: u16) {
        self.reg_num = reg_num;
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn meta_test() {
        let meta = Meta::vm_method(FuncId::new(12), 42, false);
        assert_eq!(FuncId::new(12), meta.func_id());
        assert_eq!(42, meta.reg_num());
        assert_eq!(false, meta.is_class_def());
        let mut meta = Meta::vm_method(FuncId::new(42), -1, false);
        assert_eq!(FuncId::new(42), meta.func_id());
        assert_eq!(-1, meta.reg_num());
        meta.set_reg_num(12);
        assert_eq!(FuncId::new(42), meta.func_id());
        assert_eq!(12, meta.reg_num());
        let mut meta = Meta::vm_classdef(FuncId::new(12), 42);
        assert_eq!(true, meta.is_class_def());
        meta.set_reg_num(12);
        assert_eq!(true, meta.is_class_def());
        assert_eq!(8, std::mem::size_of::<i64>());
        assert_eq!(8, std::mem::size_of::<Option<monoasm::CodePtr>>());
        assert_eq!(8, std::mem::size_of::<BcPc>());
        assert_eq!(8, std::mem::size_of::<Meta>());
    }
}

pub(crate) struct Funcs {
    pub info: MonoVec<FuncInfo>,
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
        let mut info = MonoVec::with_capacity(256);
        info.push(FuncInfo::default());
        Self {
            info,
            compile_info: vec![],
        }
    }
}

use crate::builtins::Arg;

#[monoruby_builtin]
fn enum_yielder(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let e: Enumerator = lfp.self_val().into();
    let receiver = e.obj;
    let method = e.method;
    let args = &*e.args;
    vm.invoke_method_inner(
        globals,
        method,
        receiver,
        args,
        Some(BlockHandler::from(FuncId::new(2))),
    )
}

#[monoruby_builtin]
fn yielder(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let v = Value::array_from_iter(lfp.iter());
    vm.yield_fiber(globals, v)
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
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        let (args, compile_info) = Self::handle_args(info, vec![], &sourceinfo)?;
        self.compile_info.push(compile_info);
        Ok(self.add_method_iseq(name, args, loc, sourceinfo))
    }

    pub(super) fn add_block(
        &mut self,
        mother: (FuncId, usize),
        outer: (FuncId, ExternalContext),
        for_params: Vec<(usize, BcLocal, IdentId)>,
        info: BlockInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        let (args, compile_info) = Self::handle_args(info, for_params, &sourceinfo)?;
        self.compile_info.push(compile_info);
        let func_id = self.next_func_id();
        let info = FuncInfo::new_block_iseq(func_id, mother, outer, args, loc, sourceinfo);
        self.info.push(info);
        Ok(func_id)
    }

    pub(super) fn add_classdef(
        &mut self,
        name: Option<IdentId>,
        info: BlockInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        let (_, compile_info) = Self::handle_args(info, vec![], &sourceinfo)?;
        self.compile_info.push(compile_info);
        let func_id = self.next_func_id();
        let info = FuncInfo::new_classdef_iseq(name, func_id, loc, sourceinfo);
        self.info.push(info);
        Ok(func_id)
    }

    pub(super) fn add_native_func(&mut self, name: String, address: BuiltinFn) -> FuncId {
        let id = self.next_func_id();
        self.info.push(FuncInfo::new_native(id, name, address));
        id
    }

    pub(super) fn add_native_func_eval(&mut self, name: String, address: BuiltinFn) -> FuncId {
        let id = self.next_func_id();
        self.info.push(FuncInfo::new_native_eval(id, name, address));
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
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> FuncId {
        let func_id = self.next_func_id();
        let info = FuncInfo::new_method_iseq(name, func_id, args, loc, sourceinfo);
        self.info.push(info);
        func_id
    }

    fn next_func_id(&self) -> FuncId {
        FuncId::new(self.info.len() as u32)
    }

    fn handle_args(
        info: BlockInfo,
        for_params: Vec<(usize, BcLocal, IdentId)>,
        sourceinfo: &SourceInfoRef,
    ) -> Result<(ParamsInfo, CompileInfo)> {
        let BlockInfo {
            params,
            box body,
            loc,
            ..
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
        let mut kw_rest_param = None;
        let mut block_param = None;
        let mut for_param_info = vec![];
        for (dst_outer, dst_reg, _name) in for_params {
            for_param_info.push(ForParamInfo::new(dst_outer, dst_reg, args_names.len()));
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
                    optional_info.push(OptionalInfo::new(local, initializer));
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
                ParamKind::KWRest(name) => {
                    let name = IdentId::get_id_from_string(name);
                    assert!(kw_rest_param.is_none());
                    kw_rest_param = Some(SlotId(1 + args_names.len() as u16));
                    args_names.push(Some(name));
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
            .map(|(src, dst, len)| DestructureInfo::new(src, args_names.len() + dst, len))
            .collect();
        args_names.append(&mut destruct_args);
        let compile_info = CompileInfo::new(
            body,
            keyword_initializers,
            expand_info,
            optional_info,
            for_param_info,
            loc,
        );
        let params_info = ParamsInfo::new(
            required_num,
            reqopt_num,
            reqopt_num + rest,
            args_names,
            keyword_names,
            kw_rest_param,
            block_param,
        );
        Ok((params_info, compile_info))
    }
}

#[derive(Debug, Clone)]
pub(crate) enum FuncKind {
    ISeq(Box<ISeqInfo>),
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

pub const FUNCINFO_DATA: usize = std::mem::offset_of!(FuncInfo, data);

#[derive(Debug, Clone, Default)]
pub struct FuncInfo {
    /// name of this function.
    name: Option<IdentId>,
    data: FuncData,
    pub(crate) kind: FuncKind,
    /// JIT code entries for each class of *self*.
    jit_entry: Box<HashMap<ClassId, DestLabel>>,
    _padding: usize,
}

impl alloc::GC<RValue> for FuncInfo {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.kind.mark(alloc);
    }
}

impl FuncInfo {
    fn new(name: impl Into<Option<IdentId>>, kind: FuncKind, meta: Meta) -> Self {
        let name = name.into();
        Self {
            name,
            data: FuncData {
                codeptr: None,
                pc: None,
                meta,
            },
            kind,
            jit_entry: Default::default(),
            _padding: 0,
        }
    }

    fn new_method_iseq(
        name: impl Into<Option<IdentId>>,
        func_id: FuncId,
        args: ParamsInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        let name = name.into();
        let info = ISeqInfo::new_method(func_id, name, args, loc, sourceinfo);
        Self::new(
            name,
            FuncKind::ISeq(Box::new(info)),
            Meta::vm_method(func_id, 0, false),
        )
    }

    fn new_block_iseq(
        func_id: FuncId,
        mother: (FuncId, usize),
        outer: (FuncId, ExternalContext),
        args: ParamsInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        let info = ISeqInfo::new_block(func_id, mother, outer, args, loc, sourceinfo);
        Self::new(
            None,
            FuncKind::ISeq(Box::new(info)),
            Meta::vm_method(func_id, 0, true),
        )
    }

    fn new_classdef_iseq(
        name: Option<IdentId>,
        func_id: FuncId,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        let info = ISeqInfo::new_method(func_id, name, ParamsInfo::default(), loc, sourceinfo);
        Self::new(
            name,
            FuncKind::ISeq(Box::new(info)),
            Meta::vm_classdef(func_id, 0),
        )
    }

    fn new_native(func_id: FuncId, name: String, address: BuiltinFn) -> Self {
        Self::new(
            IdentId::get_id_from_string(name),
            FuncKind::Builtin {
                abs_address: address as *const u8 as u64,
            },
            Meta::native(func_id),
        )
    }

    fn new_native_eval(func_id: FuncId, name: String, address: BuiltinFn) -> Self {
        Self::new(
            IdentId::get_id_from_string(name),
            FuncKind::Builtin {
                abs_address: address as *const u8 as u64,
            },
            Meta::native_eval(func_id),
        )
    }

    fn new_attr_reader(func_id: FuncId, name: IdentId, ivar_name: IdentId) -> Self {
        Self::new(
            name,
            FuncKind::AttrReader { ivar_name },
            Meta::native(func_id),
        )
    }

    fn new_attr_writer(func_id: FuncId, name: IdentId, ivar_name: IdentId) -> Self {
        Self::new(
            name,
            FuncKind::AttrWriter { ivar_name },
            Meta::native(func_id),
        )
    }

    pub(crate) fn name(&self) -> Option<IdentId> {
        self.name
    }

    ///
    /// Get meta data (Meta) of this function.
    ///
    pub(crate) fn meta(&self) -> Meta {
        self.data.meta()
    }

    ///
    /// Get program counter (BcPc) of this function.
    ///
    pub(crate) fn pc(&self) -> BcPc {
        self.data.pc().unwrap()
    }

    ///
    /// Get code pointer (Option<CodePtr>) of this function.
    ///
    pub(crate) fn codeptr(&self) -> Option<monoasm::CodePtr> {
        self.data.codeptr()
    }

    ///
    /// Set a program counter (BcPc) and the number of registers of this function.
    ///
    pub(super) fn set_pc_regnum(&mut self, pc: BcPc, reg_num: u16) {
        self.data.set_pc(pc);
        self.data.set_reg_num(reg_num);
    }

    pub(in crate::globals) fn set_codeptr(&mut self, codeptr: monoasm::CodePtr) {
        self.data.set_codeptr(codeptr)
    }

    pub(crate) fn data_ref(&self) -> &FuncData {
        &self.data
    }

    pub(crate) fn get_data(&self) -> (Meta, monoasm::CodePtr, Option<BcPc>) {
        let meta = self.data.meta();
        let codeptr = self.data.codeptr().unwrap();
        let pc = self.data.pc();
        (meta, codeptr, pc)
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

    pub(crate) fn is_ruby_func(&self) -> Option<&ISeqInfo> {
        match &self.kind {
            FuncKind::ISeq(info) => Some(info),
            _ => None,
        }
    }

    pub(crate) fn add_jit_code(
        &mut self,
        self_class: ClassId,
        entry: DestLabel,
    ) -> Option<DestLabel> {
        self.jit_entry.insert(self_class, entry)
    }

    pub(crate) fn get_jit_code(&self, self_class: ClassId) -> Option<DestLabel> {
        self.jit_entry.get(&self_class).cloned()
    }
}

impl FuncInfo {
    #[cfg(feature = "emit-bc")]
    pub(crate) fn dump_bc(&self, globals: &Globals) {
        let info = self.as_ruby_func();
        eprintln!("------------------------------------");
        let loc = info.loc;
        let line = info.sourceinfo.get_line(&loc);
        let file_name = info.sourceinfo.file_name();
        eprintln!(
            "{} {file_name}:{line}",
            globals.store.func_description(info.id()),
        );
        eprintln!("meta:{:?} {:?}", self.data.meta, self.kind);
        eprintln!("{:?}", info.get_exception_map());
        for (i, pc) in info.bytecode().iter().enumerate() {
            let pc = BcPc::from(pc);
            if let Some(fmt) = globals.format(pc, i) {
                eprint!(
                    "{}:{:05} [{:02}] ",
                    if info.bb_info.is_bb_head(bytecodegen::BcIndex::from(i)) {
                        "+"
                    } else {
                        " "
                    },
                    i,
                    info.sp[i].0
                );
                eprintln!("{}", fmt);
            };
        }
        eprintln!("------------------------------------");
    }
}
