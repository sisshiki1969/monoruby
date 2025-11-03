use super::*;
use crate::bytecodegen::{BcLocal, CompileInfo, DestructureInfo, ForParamInfo, OptionalInfo};

pub(crate) const FUNCDATA_CODEPTR: u64 = std::mem::offset_of!(FuncData, codeptr) as _;
pub(crate) const FUNCDATA_META: u64 = std::mem::offset_of!(FuncData, meta) as _;
//pub(crate) const FUNCDATA_REGNUM: u64 = FUNCDATA_META + META_REGNUM;
pub(crate) const FUNCDATA_PC: u64 = std::mem::offset_of!(FuncData, pc) as _;
pub(crate) const FUNCDATA_OFS: u64 = std::mem::offset_of!(FuncData, ofs) as _;
pub(crate) const FUNCDATA_MIN: u64 = std::mem::offset_of!(FuncData, min) as _;
//pub(crate) const FUNCDATA_MAX: u64 = std::mem::offset_of!(FuncData, max) as _;

//pub(crate) const META_FUNCID: u64 = std::mem::offset_of!(Meta, func_id) as _;
pub(crate) const META_REGNUM: u64 = std::mem::offset_of!(Meta, reg_num) as _;
pub(crate) const META_KIND: u64 = std::mem::offset_of!(Meta, kind) as _;

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
    pub const fn new(id: u32) -> Self {
        Self(std::num::NonZeroU32::new(id).unwrap())
    }

    pub fn get(&self) -> u32 {
        self.0.get()
    }

    pub fn lexical_class(self, store: &Store) -> ClassId {
        if let Some(iseq) = store[self].is_iseq() {
            store[iseq]
                .lexical_context
                .last()
                .cloned()
                .unwrap_or(OBJECT_CLASS)
        } else {
            store[self].owner_class().unwrap_or(OBJECT_CLASS)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
#[repr(C)]
pub(crate) struct FuncData {
    /// address of function.
    codeptr: Option<monoasm::CodePtr>,
    /// metadata of this function.
    meta: Meta,
    /// the address of program counter
    pc: Option<BytecodePtrBase>,
    ofs: u16,
    min: u16,
    max: u16,
    /// class id which this function belongs to.
    owner: Option<ClassId>,
}

impl FuncData {
    pub fn pc(&self) -> Option<BytecodePtrBase> {
        self.pc
    }

    pub(super) fn set_pc(&mut self, pc: BytecodePtrBase) {
        self.pc = Some(pc);
    }

    pub fn set_reg_num(&mut self, reg_num: u16) {
        self.meta.set_reg_num(reg_num);
        self.set_offset(reg_num);
    }

    fn set_offset(&mut self, reg_num: u16) {
        self.ofs =
            ((reg_num as usize * 8 + (RSP_LOCAL_FRAME + LFP_SELF) as usize + 31) / 16) as u16;
    }

    pub(in crate::globals) fn set_codeptr(&mut self, codeptr: monoasm::CodePtr) {
        self.codeptr = Some(codeptr);
    }

    /*pub fn stack_offset(&self) -> u16 {
        self.ofs
    }*/

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
/// |    FuncId     | reg_num |mode|kind|
/// +-------+-------+---------+----+----+
/// ~~~
///
#[derive(Clone, Copy, PartialEq, Eq, Default)]
#[repr(C)]
pub struct Meta {
    func_id: Option<FuncId>,
    reg_num: u16,
    mode: u8,
    /// bit 7:  0:on_stack 1:on_heap
    /// bit 4:  1:simple (no optional, no rest, no keyword, no block)
    /// bit 3:  0:no eval 1:eval(which possibly manipulates stack slots in outer frames)
    /// bit 2:  0:method_style arg 1:block_style arg
    /// bit 1:  0:Ruby 1:native
    /// bit 0:  0:method 1:class_def
    kind: u8,
}

impl std::fmt::Debug for Meta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} {}{}{}{} reg_num:{}",
            self.func_id(),
            if self.is_native() { "NATIVE " } else { "" },
            if self.is_class_def() {
                "class_def "
            } else {
                ""
            },
            if self.is_simple() { "SIMPLE " } else { "" },
            if self.on_stack() { "stack" } else { "heap" },
            self.reg_num()
        )
    }
}

impl Meta {
    pub fn new(
        func_id: Option<FuncId>,
        reg_num: u16,
        is_simple: bool,
        is_eval: bool,
        is_native: bool,
        is_class_def: bool,
        is_block_style: bool,
    ) -> Self {
        Self {
            func_id,
            reg_num,
            mode: 0,
            kind: (is_simple as u8) << 4
                | (is_eval as u8) << 3
                | (is_block_style as u8) << 2
                | (is_native as u8) << 1
                | is_class_def as u8,
        }
    }

    pub fn get(&self) -> u64 {
        unsafe { std::mem::transmute(*self) }
    }

    fn vm_method(
        func_id: impl Into<Option<FuncId>>,
        reg_num: i64,
        is_block_style: bool,
        is_simple: bool,
    ) -> Self {
        let reg_num = reg_num as i16 as u16;
        Self::new(
            func_id.into(),
            reg_num,
            is_simple,
            false,
            false,
            false,
            is_block_style,
        )
    }

    fn vm_classdef(func_id: impl Into<Option<FuncId>>, reg_num: i64) -> Self {
        let reg_num = reg_num as i16 as u16;
        Self::new(func_id.into(), reg_num, true, false, false, true, false)
    }

    fn proc(func_id: FuncId, reg_num: usize, is_simple: bool, is_block_style: bool) -> Self {
        Self::new(
            Some(func_id),
            reg_num as u16,
            is_simple,
            false,
            false,
            false,
            is_block_style,
        )
    }

    fn native(func_id: FuncId, reg_num: usize, is_simple: bool) -> Self {
        Self::new(
            Some(func_id),
            reg_num as u16,
            is_simple,
            false,
            true,
            false,
            false,
        )
    }

    fn native_eval(func_id: FuncId, reg_num: usize, is_simple: bool) -> Self {
        Self::new(
            Some(func_id),
            reg_num as u16,
            is_simple,
            true,
            true,
            false,
            false,
        )
    }

    pub fn func_id(&self) -> FuncId {
        self.func_id.unwrap()
    }

    pub fn reg_num(&self) -> i64 {
        self.reg_num as i16 as i64
    }

    ///
    /// If `self` is "simple", return true.
    ///
    /// "simple" means that the function has no optional, rest, keyword, keyword rest, and block parameters.
    ///
    pub fn is_simple(&self) -> bool {
        (self.kind & 0b1_0000) != 0
    }

    ///
    /// Returns true if this function possibly manipulates outer local variables.
    ///
    pub fn is_eval(&self) -> bool {
        (self.kind & 0b1000) != 0
    }

    pub fn is_block_style(&self) -> bool {
        (self.kind & 0b100) != 0
    }

    pub fn set_method_style(&mut self) {
        self.kind &= !0b100
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
mod tests {
    use super::*;

    #[test]
    fn meta_test() {
        let meta = Meta::vm_method(FuncId::new(12), 42, false, false);
        assert_eq!(FuncId::new(12), meta.func_id());
        assert_eq!(42, meta.reg_num());
        assert_eq!(false, meta.is_class_def());
        let mut meta = Meta::vm_method(FuncId::new(42), -1, false, false);
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
        assert_eq!(8, std::mem::size_of::<BytecodePtr>());
        assert_eq!(8, std::mem::size_of::<Meta>());
    }
}

pub(crate) struct Funcs {
    pub(in crate::globals) info: MonoVec<FuncInfo>,
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
        let mut info = MonoVec::with_capacity(8192);
        info.push(FuncInfo::default());
        Self {
            info,
            compile_info: vec![],
        }
    }
}

impl alloc::GC<RValue> for Funcs {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.info.iter().for_each(|info| info.mark(alloc));
    }
}

#[monoruby_builtin]
fn enum_yielder(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let e = Enumerator::new(lfp.self_val());
    let receiver = e.obj;
    let method = e.method;
    let args = &*e.args;
    vm.invoke_method_inner(
        globals,
        method,
        receiver,
        args,
        Some(BlockHandler::from_current(FuncId::new(2))),
        None,
    )
}

#[monoruby_builtin]
fn yielder(vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    vm.yield_fiber(lfp.arg(0))
}

impl Funcs {
    pub(super) fn functions(&self) -> &[FuncInfo] {
        &self.info
    }

    pub(super) fn function_len(&self) -> usize {
        self.info.len()
    }

    pub fn get_compile_info(&mut self) -> CompileInfo {
        self.compile_info.remove(0)
    }

    pub(super) fn new_proc_method(&mut self, proc: Proc) -> FuncId {
        let func_id = self.next_func_id();
        let is_block_style = self[proc.func_id()].is_block_style();
        let reg_num = self[proc.func_id()].meta().reg_num() as usize;
        let params = self[proc.func_id()].params();
        self.info.push(FuncInfo::new_proc(
            func_id,
            "proc".to_string(),
            proc,
            params,
            reg_num,
            is_block_style,
        ));
        func_id
    }

    pub(super) fn add_iseq_method(&mut self, info: BlockInfo) -> Result<(FuncId, ParamsInfo)> {
        let (params_info, compile_info) = Self::handle_args(info, vec![])?;
        self.compile_info.push(compile_info);
        let func_id = self.next_func_id();
        Ok((func_id, params_info))
    }

    pub(super) fn add_block(
        &mut self,
        for_params: Vec<(usize, BcLocal, IdentId)>,
        info: BlockInfo,
    ) -> Result<(FuncId, ParamsInfo)> {
        let (params_info, compile_info) = Self::handle_args(info, for_params)?;
        self.compile_info.push(compile_info);
        let func_id = self.next_func_id();
        Ok((func_id, params_info))
    }

    pub(super) fn add_classdef(&mut self, info: BlockInfo) -> Result<FuncId> {
        let (_, compile_info) = Self::handle_args(info, vec![])?;
        self.compile_info.push(compile_info);
        let func_id = self.next_func_id();
        Ok(func_id)
    }

    pub(super) fn new_native_func(
        &mut self,
        name: String,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
        kw_names: &[&str],
        kw_rest: bool,
    ) -> FuncId {
        let id = self.next_func_id();
        self.info.push(FuncInfo::new_native(
            id, name, address, min, max, rest, kw_names, kw_rest,
        ));
        id
    }

    pub(super) fn new_native_basic_op(
        &mut self,
        name: String,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        let id = self.next_func_id();
        self.info.push(FuncInfo::new_native_basic_op(
            id, name, address, min, max, rest,
        ));
        id
    }

    pub(super) fn new_native_func_eval(
        &mut self,
        name: String,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        let id = self.next_func_id();
        self.info
            .push(FuncInfo::new_native_eval(id, name, address, min, max, rest));
        id
    }

    pub(super) fn new_attr_reader(&mut self, name: IdentId, ivar_name: IdentId) -> FuncId {
        let id = self.next_func_id();
        let info = FuncInfo::new_attr_reader(id, name, ivar_name);
        self.info.push(info);
        id
    }

    pub(super) fn new_attr_writer(&mut self, name: IdentId, ivar_name: IdentId) -> FuncId {
        let id = self.next_func_id();
        let info = FuncInfo::new_attr_writer(id, name, ivar_name);
        self.info.push(info);
        id
    }

    fn next_func_id(&self) -> FuncId {
        FuncId::new(self.info.len() as u32)
    }

    fn handle_args(
        info: BlockInfo,
        for_params: Vec<(usize, BcLocal, IdentId)>,
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
        let mut post_num = 0;
        let mut rest = None;
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
                    assert_eq!(rest, None);
                    rest = Some(args_names.len());
                    args_names.push(name.map(IdentId::get_id_from_string));
                }
                ParamKind::Post(name) => {
                    args_names.push(Some(IdentId::get_id_from_string(name)));
                    post_num += 1;
                }
                ParamKind::Delegate => {
                    assert_eq!(rest, None);
                    rest = Some(args_names.len());
                    args_names.push(None);
                    assert!(kw_rest_param.is_none());
                    kw_rest_param = Some(SlotId(1 + args_names.len() as u16));
                    args_names.push(None);
                    block_param = Some(IdentId::get_id(""));
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
            }
        }

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
            optional_num,
            rest,
            post_num,
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
    ISeq(ISeqId),
    Proc(Proc),
    Builtin { abs_address: u64 },
    AttrReader { ivar_name: IdentId },
    AttrWriter { ivar_name: IdentId },
}

impl std::default::Default for FuncKind {
    fn default() -> Self {
        Self::Builtin { abs_address: 0 }
    }
}

impl alloc::GC<RValue> for FuncKind {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        match self {
            FuncKind::Proc(proc) => proc.mark(alloc),
            _ => {}
        }
    }
}

pub const FUNCINFO_DATA: usize = std::mem::offset_of!(FuncInfo, data);

#[derive(Debug, Clone, PartialEq, Default)]
enum FuncType {
    /// Top-level.
    TopLevel,
    /// Method.
    #[default]
    Method,
    /// Block.
    Block,
    /// Class Definition.
    ClassDef,
}

#[derive(Debug, Clone, Default)]
struct FuncExt {
    /// name of this function.
    name: Option<IdentId>,
    /// type of this function.
    ty: FuncType,
    /// `DestLabel` of entry site.
    entry: Option<DestLabel>,
    /// parameter information of this function.
    params: ParamsInfo,
    #[cfg(feature = "perf")]
    wrapper: Option<(monoasm::CodePtr, usize, monoasm::CodePtr, usize)>,
}

#[derive(Debug, Clone, Default)]
pub struct FuncInfo {
    /// function data.
    data: FuncData,
    pub(crate) kind: FuncKind,
    ext: Box<FuncExt>,
}

impl alloc::GC<RValue> for FuncInfo {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        if let FuncKind::Proc(proc) = &self.kind {
            proc.mark(alloc);
        }
    }
}

impl FuncInfo {
    fn new(
        name: impl Into<Option<IdentId>>,
        kind: FuncKind,
        ty: FuncType,
        meta: Meta,
        params: ParamsInfo,
    ) -> Self {
        let name = name.into();
        let min = params.req_num() as u16;
        let max = params.max_positional_args() as u16;
        let mut data = FuncData {
            codeptr: None,
            pc: None,
            meta,
            ofs: 0,
            min,
            max,
            owner: None,
        };
        data.set_offset(max);
        Self {
            data,
            kind,
            ext: Box::new(FuncExt {
                name,
                ty,
                entry: None,
                params,
                #[cfg(feature = "perf")]
                wrapper: None,
            }),
        }
    }

    pub(super) fn new_method_iseq(
        name: impl Into<Option<IdentId>>,
        func_id: FuncId,
        iseq: ISeqId,
        params: ParamsInfo,
        top_level: bool,
    ) -> Self {
        let name = name.into();
        Self::new(
            name,
            FuncKind::ISeq(iseq),
            if top_level {
                FuncType::TopLevel
            } else {
                FuncType::Method
            },
            Meta::vm_method(func_id, 0, false, params.is_simple()),
            params,
        )
    }

    pub(super) fn new_block_iseq(
        func_id: FuncId,
        iseq: ISeqId,
        params: ParamsInfo,
        is_block_style: bool,
    ) -> Self {
        Self::new(
            None,
            FuncKind::ISeq(iseq),
            FuncType::Block,
            Meta::vm_method(func_id, 0, is_block_style, params.is_simple()),
            params,
        )
    }

    pub(super) fn new_classdef_iseq(name: Option<IdentId>, func_id: FuncId, iseq: ISeqId) -> Self {
        Self::new(
            name,
            FuncKind::ISeq(iseq),
            FuncType::ClassDef,
            Meta::vm_classdef(func_id, 0),
            ParamsInfo::default(),
        )
    }

    fn new_native(
        func_id: FuncId,
        name: String,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
        kw_names: &[&str],
        kw_rest: bool,
    ) -> Self {
        let kw_names = kw_names.iter().map(|s| IdentId::get_id(s)).collect();
        let params = ParamsInfo::new_native(min, max, rest, kw_names, kw_rest);
        let reg_num = params.total_args() + 1;
        Self::new(
            IdentId::get_id_from_string(name),
            FuncKind::Builtin {
                abs_address: address as *const u8 as u64,
            },
            FuncType::Method,
            Meta::native(func_id, reg_num, params.is_simple()),
            params,
        )
    }

    fn new_proc(
        func_id: FuncId,
        name: String,
        proc: Proc,
        params: ParamsInfo,
        reg_num: usize,
        is_block_style: bool,
    ) -> Self {
        Self::new(
            IdentId::get_id_from_string(name),
            FuncKind::Proc(proc),
            FuncType::Method,
            Meta::proc(func_id, reg_num, params.is_simple(), is_block_style),
            params,
        )
    }

    fn new_native_basic_op(
        func_id: FuncId,
        name: String,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
    ) -> Self {
        let params = ParamsInfo::new_native(min, max, rest, vec![], false);
        let reg_num = params.total_args() + 1;
        Self::new(
            IdentId::get_id_from_string(name),
            FuncKind::Builtin {
                abs_address: address as *const u8 as u64,
            },
            FuncType::Method,
            Meta::native(func_id, reg_num, params.is_simple()),
            params,
        )
    }

    fn new_native_eval(
        func_id: FuncId,
        name: String,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
    ) -> Self {
        let params = ParamsInfo::new_native(min, max, rest, vec![], false);
        let reg_num = params.total_args() + 1;
        Self::new(
            IdentId::get_id_from_string(name),
            FuncKind::Builtin {
                abs_address: address as *const u8 as u64,
            },
            FuncType::Method,
            Meta::native_eval(func_id, reg_num, params.is_simple()),
            params,
        )
    }

    fn new_attr_reader(func_id: FuncId, name: IdentId, ivar_name: IdentId) -> Self {
        let params = ParamsInfo::new_attr_reader();
        let reg_num = params.total_args() + 1;
        Self::new(
            name,
            FuncKind::AttrReader { ivar_name },
            FuncType::Method,
            Meta::native(func_id, reg_num, true),
            params,
        )
    }

    fn new_attr_writer(func_id: FuncId, name: IdentId, ivar_name: IdentId) -> Self {
        let params = ParamsInfo::new_attr_writer();
        let reg_num = params.total_args() + 1;
        Self::new(
            name,
            FuncKind::AttrWriter { ivar_name },
            FuncType::Method,
            Meta::native(func_id, reg_num, true),
            params,
        )
    }

    /*pub(crate) fn stack_offset(&self) -> u16 {
        self.data.stack_offset()
    }*/

    pub(crate) fn name(&self) -> Option<IdentId> {
        self.ext.name
    }

    ///
    /// Whether this function is a method.
    ///
    /// return false for top-level, block, and class definition.
    ///
    pub(crate) fn is_method(&self) -> bool {
        self.ext.ty == FuncType::Method
    }

    ///
    /// Whether this function is a method, a class definition, or a top-level.
    ///
    pub(crate) fn is_method_type(&self) -> bool {
        match self.ext.ty {
            FuncType::Method | FuncType::ClassDef | FuncType::TopLevel => true,
            _ => false,
        }
    }

    pub(crate) fn owner_class(&self) -> Option<ClassId> {
        self.data.owner
    }

    pub(super) fn set_owner_class(&mut self, class: ClassId) {
        self.data.owner = Some(class);
    }

    pub(super) fn entry_label(&self) -> DestLabel {
        self.ext.entry.clone().unwrap()
    }

    ///
    /// Get meta data (Meta) of this function.
    ///
    pub fn meta(&self) -> Meta {
        self.data.meta()
    }

    pub(crate) fn set_method_style(&mut self) {
        self.data.meta.set_method_style();
    }

    ///
    /// Get code pointer (Option<CodePtr>) of this function.
    ///
    pub(crate) fn codeptr(&self) -> Option<monoasm::CodePtr> {
        self.data.codeptr()
    }

    pub(crate) fn params(&self) -> ParamsInfo {
        self.ext.params.clone()
    }

    /// The number of required arguments.
    pub(crate) fn req_num(&self) -> usize {
        self.ext.params.req_num()
    }

    /// The number of optional arguments.
    pub(crate) fn opt_num(&self) -> usize {
        self.ext.params.opt_num()
    }

    /// The number of required + optional arguments.
    pub(crate) fn reqopt_num(&self) -> usize {
        self.ext.params.reqopt_num()
    }

    /// The number of post arguments.
    pub(crate) fn post_num(&self) -> usize {
        self.ext.params.post_num()
    }

    /// The posiiton of keyword arguments.
    pub(crate) fn kw_reg_pos(&self) -> SlotId {
        self.ext.params.kw_reg_pos()
    }

    pub(crate) fn kw_names(&self) -> &[IdentId] {
        &self.ext.params.kw_names
    }

    pub(crate) fn kw_rest(&self) -> Option<SlotId> {
        self.ext.params.kw_rest
    }

    pub(crate) fn no_keyword(&self) -> bool {
        self.kw_names().is_empty() && self.kw_rest().is_none()
    }

    pub(crate) fn is_rest(&self) -> bool {
        self.ext.params.is_rest().is_some()
    }

    pub(crate) fn rest_pos(&self) -> Option<u16> {
        self.ext.params.is_rest()
    }

    pub(crate) fn is_block_style(&self) -> bool {
        self.meta().is_block_style()
    }

    pub(crate) fn total_args(&self) -> usize {
        self.ext.params.total_args()
    }

    pub(crate) fn get_offset(&self) -> usize {
        ((RSP_LOCAL_FRAME + LFP_ARG0) as usize + 8 * self.total_args() + 8) & !0xf
    }

    ///
    /// Get the minimal number of positional arguments (= required + post) of this function.
    ///
    pub(crate) fn min_positional_args(&self) -> usize {
        self.ext.params.min_positional_args()
    }

    ///
    /// Get the max number of positional arguments (= required + optional + post) of this function.
    ///
    pub(crate) fn max_positional_args(&self) -> usize {
        self.ext.params.max_positional_args()
    }

    pub(crate) fn total_positional_args(&self) -> usize {
        self.ext.params.total_positional_args()
    }

    /*pub(crate) fn discard_excess_positional_args(&self) -> bool {
        self.is_block_style() && !self.is_rest()
    }*/

    pub(crate) fn single_arg_expand(&self) -> bool {
        self.is_block_style() && (self.total_positional_args() > 1)
    }

    pub(crate) fn apply_args(&self, pos_num: usize) -> (usize, usize, usize) {
        if pos_num <= self.req_num() {
            (pos_num, 0, 0)
        } else if pos_num <= self.min_positional_args() {
            (self.req_num(), 0, pos_num - self.req_num())
        } else if pos_num > self.max_positional_args() {
            (self.req_num(), self.opt_num(), self.post_num())
        } else {
            (
                self.req_num(),
                pos_num - self.min_positional_args(),
                self.post_num(),
            )
        }
    }

    ///
    /// Set a program counter (BcPc) and the number of registers of this function.
    ///
    pub(super) fn set_pc_regnum(&mut self, pc: BytecodePtrBase, reg_num: u16) {
        self.data.set_pc(pc);
        self.data.set_reg_num(reg_num);
    }

    pub(in crate::globals) fn set_entry(&mut self, entry: DestLabel, codeptr: monoasm::CodePtr) {
        self.ext.entry = Some(entry);
        self.data.set_codeptr(codeptr)
    }

    #[cfg(feature = "perf")]
    pub(in crate::globals) fn set_wrapper_info(
        &mut self,
        info: (monoasm::CodePtr, usize, monoasm::CodePtr, usize),
    ) {
        self.ext.wrapper = Some(info);
    }

    #[cfg(feature = "perf")]
    pub(in crate::globals) fn get_wrapper_info(
        &self,
    ) -> (monoasm::CodePtr, usize, monoasm::CodePtr, usize) {
        self.ext.wrapper.clone().unwrap()
    }

    pub(crate) fn data_ref(&self) -> &FuncData {
        &self.data
    }

    pub(crate) fn get_data(&self) -> (Meta, monoasm::CodePtr, Option<BytecodePtrBase>) {
        let meta = self.data.meta();
        let codeptr = self.data.codeptr().unwrap();
        let pc = self.data.pc();
        (meta, codeptr, pc)
    }

    pub fn as_iseq(&self) -> ISeqId {
        match &self.kind {
            FuncKind::ISeq(iseq) => *iseq,
            _ => unreachable!("{:?}", self),
        }
    }

    pub(crate) fn is_iseq(&self) -> Option<ISeqId> {
        match &self.kind {
            FuncKind::ISeq(iseq) => Some(*iseq),
            _ => None,
        }
    }

    fn positional_within_range(&self, pos_num: usize) -> bool {
        let min = self.min_positional_args();
        let max = self.max_positional_args();
        min <= pos_num && pos_num <= max
    }
}

impl Store {
    ///
    /// Check whether this function call is a *simple* call.
    ///
    /// *simple* call means that:
    /// - no splat arguments
    /// - no hash splat arguments
    /// - no single argument expansion in block call
    /// - no extra positional argument
    /// - no rest param
    /// - if method_call, required + post <= (the number of positional arguments) <= required + optional + post
    ///
    pub(crate) fn is_simple_call(&self, fid: FuncId, callid: CallSiteId) -> bool {
        let callsite = &self[callid];
        let info = &self[fid];
        let pos_num = callsite.pos_num;
        if pos_num == 1 && info.single_arg_expand() {
            return false;
        };
        if info.no_keyword() && callsite.kw_may_exists() {
            return false;
        };
        !callsite.has_splat()
            && !callsite.has_hash_splat()
            && !info.is_rest()
            && (info.is_block_style() || info.positional_within_range(pos_num))
    }
}
