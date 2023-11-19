use super::*;
use crate::{builtins::Arg, bytecodegen::*};

pub mod compiler;
mod frame;
pub mod inline;
pub mod op;
use crate::bytecodegen::inst::*;
use compiler::jitgen::trace_ir::*;
pub use compiler::*;
use fancy_regex::Captures;
pub use frame::*;
use monoasm::*;
pub use op::*;
use ruruby_parse::{Loc, SourceInfoRef};

pub type Result<T> = std::result::Result<T, MonorubyErr>;
pub type BuiltinFn = extern "C" fn(&mut Executor, &mut Globals, LFP, Arg) -> Option<Value>;
pub type BinaryOpFn = extern "C" fn(&mut Executor, &mut Globals, Value, Value) -> Option<Value>;

const BP_PREV_CFP: i64 = 8;
const BP_LFP: i64 = 16;
const LFP_OFFSET: i64 = 24;
const LBP_OUTER: i64 = 0 + LFP_OFFSET;
/// Meta 8bytes
const LBP_META: i64 = 8 + LFP_OFFSET;
/// Meta::Regnum 2bytes
const LBP_META_REGNUM: i64 = LBP_META - META_REGNUM as i64;
/// Meta::FuncId 4bytes
const LBP_META_FUNCID: i64 = LBP_META + META_FUNCID as i64;
const LBP_BLOCK: i64 = 16 + LFP_OFFSET;
const LBP_SELF: i64 = 24 + LFP_OFFSET;
pub const LBP_ARG0: i64 = LBP_SELF + 8;

const EXECUTOR_CFP: i64 = std::mem::offset_of!(Executor, cfp) as _;
const EXECUTOR_RSP_SAVE: i64 = std::mem::offset_of!(Executor, rsp_save) as _;
const EXECUTOR_PARENT_FIBER: i64 = std::mem::offset_of!(Executor, parent_fiber) as _;

///
/// Bytecode interpreter.
///
#[derive(Debug)]
#[repr(C)]
pub struct Executor {
    /// control frame pointer.
    cfp: Option<CFP>,
    /// rsp save area.
    ///
    /// - 0: created
    /// - -1: terminated
    /// - other: suspended
    rsp_save: Option<std::ptr::NonNull<u8>>,
    parent_fiber: Option<std::ptr::NonNull<Executor>>,
    lexical_class: Vec<Vec<Cref>>,
    sp_last_match: Option<Value>,   // $&        : Regexp.last_match(0)
    sp_post_match: Option<Value>,   // $'        : Regexp.post_match
    sp_matches: Vec<Option<Value>>, // $1 ... $n : Regexp.last_match(n)
    temp_stack: Vec<Value>,
    /// error information.
    exception: Option<MonorubyErr>,
}

impl std::default::Default for Executor {
    fn default() -> Self {
        Self {
            cfp: None,
            rsp_save: None,
            parent_fiber: None,
            lexical_class: vec![vec![]],
            sp_last_match: None,
            sp_post_match: None,
            sp_matches: vec![],
            temp_stack: vec![],
            exception: None,
        }
    }
}

impl alloc::GC<RValue> for Executor {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.sp_matches.iter().for_each(|v| {
            if let Some(v) = v {
                v.mark(alloc)
            }
        });
        if let Some(v) = self.sp_last_match {
            v.mark(alloc)
        };
        if let Some(v) = self.sp_post_match {
            v.mark(alloc)
        };
        self.temp_stack.iter().for_each(|v| v.mark(alloc));
        let mut cfp = self.cfp;
        while let Some(inner_cfp) = cfp {
            inner_cfp.lfp().mark(alloc);
            cfp = inner_cfp.prev();
        }
    }
}

impl Executor {
    pub fn init(globals: &mut Globals) -> Self {
        let mut executor = Self::default();
        let path = std::path::Path::new("startup/startup.rb");
        let code = include_str!("../startup/startup.rb").to_string();
        if let Err(err) = executor.exec_script(globals, code, path) {
            err.show_error_message_and_all_loc();
            panic!("error occurred in startup.");
        };
        #[cfg(feature = "emit-bc")]
        {
            globals.startup_flag = true;
        }
        executor
    }

    pub fn cfp(&self) -> CFP {
        self.cfp.unwrap()
    }

    pub unsafe fn register(&self, index: usize) -> Option<Value> {
        self.cfp().lfp().register(index)
    }

    fn method_func_id(&self) -> FuncId {
        self.cfp().method_func_id()
    }

    pub fn temp_len(&self) -> usize {
        self.temp_stack.len()
    }

    pub fn temp_push(&mut self, val: Value) {
        self.temp_stack.push(val);
    }

    pub fn temp_append(&mut self, mut v: Vec<Value>) -> usize {
        let len = self.temp_stack.len();
        self.temp_stack.append(&mut v);
        len
    }

    pub fn temp_extend_form_slice(&mut self, slice: &[Value]) -> usize {
        let len = self.temp_stack.len();
        self.temp_stack.extend_from_slice(slice);
        len
    }

    pub fn temp_clear(&mut self, len: usize) {
        self.temp_stack.truncate(len);
    }

    pub fn temp_tear(&mut self, len: usize) -> Vec<Value> {
        self.temp_stack.drain(len..).collect()
    }

    pub fn parent_fiber(&self) -> Option<NonNull<Executor>> {
        self.parent_fiber
    }
}

impl Executor {
    pub fn exec_script(
        &mut self,
        globals: &mut Globals,
        code: String,
        path: &std::path::Path,
    ) -> Result<Value> {
        let fid = match ruruby_parse::Parser::parse_program(code, path) {
            Ok(res) => bytecodegen::compile_script(globals, res.node, res.source_info),
            Err(err) => Err(MonorubyErr::parse(err)),
        }?;
        self.exec_func(globals, fid)
    }

    ///
    /// Execute top level method.
    ///
    /// *main* object is set to *self*.
    ///
    pub fn exec_func(&mut self, globals: &mut Globals, func_id: FuncId) -> Result<Value> {
        #[cfg(feature = "emit-bc")]
        globals.dump_bc();

        let res = (globals.codegen.entry_point)(self, globals, func_id);
        res.ok_or_else(|| self.take_error())
    }

    pub(crate) fn enter_class_context(&mut self) {
        self.lexical_class.push(vec![]);
    }

    pub(crate) fn exit_class_context(&mut self) {
        self.lexical_class.pop();
    }

    pub(crate) fn push_class_context(&mut self, class_id: ClassId) {
        self.lexical_class
            .last_mut()
            .unwrap()
            .push(Cref::new(class_id, false, Visibility::Public));
    }

    pub(crate) fn pop_class_context(&mut self) -> Option<ClassId> {
        self.lexical_class
            .last_mut()
            .unwrap()
            .pop()
            .map(|x| x.class_id)
    }

    pub(crate) fn set_module_function(&mut self) {
        self.lexical_class
            .last_mut()
            .unwrap()
            .last_mut()
            .unwrap()
            .module_function = true;
    }

    fn get_class_context(&self) -> Cref {
        self.lexical_class
            .last()
            .unwrap()
            .last()
            .cloned()
            .unwrap_or_else(|| Cref::new(OBJECT_CLASS, false, Visibility::Private))
    }

    fn context_class_id(&self) -> ClassId {
        self.lexical_class
            .last()
            .unwrap()
            .last()
            .map(|cref| cref.class_id)
            .unwrap_or(OBJECT_CLASS)
    }

    pub(crate) fn context_visibility(&self) -> Visibility {
        self.lexical_class
            .last()
            .unwrap()
            .last()
            .map(|cref| cref.visibility)
            .unwrap_or(Visibility::Private)
    }

    pub(crate) fn set_context_visibility(&mut self, visi: Visibility) {
        self.lexical_class
            .last_mut()
            .unwrap()
            .last_mut()
            .unwrap()
            .visibility = visi;
    }
}

//
// error handlers
//
impl Executor {
    pub fn set_error(&mut self, err: MonorubyErr) {
        assert_eq!(self.exception, None);
        self.exception = Some(err);
    }

    pub(crate) fn take_error(&mut self) -> MonorubyErr {
        std::mem::take(&mut self.exception).unwrap()
    }

    fn exception(&self) -> Option<&MonorubyErr> {
        self.exception.as_ref()
    }

    fn take_ex_obj(&mut self, globals: &Globals) -> Value {
        let err = self.take_error();
        self.exception_to_val(globals, err)
    }

    fn err_divide_by_zero(&mut self) {
        self.set_error(MonorubyErr::divide_by_zero());
    }

    fn err_wrong_number_of_arg_range(
        &mut self,
        given: usize,
        range: std::ops::RangeInclusive<usize>,
    ) {
        self.set_error(MonorubyErr::wrong_number_of_arg_range(given, range))
    }

    ///
    /// Set FrozenError with message "can't modify frozen Integer: 5".
    ///
    pub(crate) fn err_cant_modify_frozen(&mut self, globals: &Globals, val: Value) {
        self.set_error(MonorubyErr::cant_modify_frozen(globals, val));
    }

    fn push_error_location(&mut self, loc: Loc, sourceinfo: SourceInfoRef) {
        match &mut self.exception {
            Some(err) => {
                err.push_trace(loc, sourceinfo);
            }
            None => unreachable!(),
        };
    }

    fn exception_to_val(&self, globals: &Globals, err: MonorubyErr) -> Value {
        let class_id = globals.get_error_class(&err);
        Value::new_exception_from_err(err, class_id)
    }
}

//
// handling fiber.
//
impl Executor {
    pub fn fiber_state(&self) -> FiberState {
        match self.rsp_save {
            None => FiberState::Created,
            Some(p) if p.as_ptr() as i64 == -1 => FiberState::Terminated,
            _ => FiberState::Suspended,
        }
    }

    pub fn save_rsp(&mut self, rsp: *mut u8) {
        self.rsp_save = Some(std::ptr::NonNull::new(rsp).unwrap());
    }

    pub(crate) fn yield_fiber(&mut self, globals: &Globals, val: Value) -> Result<Value> {
        match (globals.codegen.yield_fiber)(self as _, val) {
            Some(res) => Ok(res),
            None => Err(unsafe { self.parent_fiber.unwrap().as_mut().take_error() }),
        }
    }
}

// Handling constants.

impl Executor {
    ///
    /// Find Constant in current class context.
    ///
    /// This fn returns the value of the constant and the class id of the base object.
    /// It is necessary to check the base class for confirmation of cache consistency.
    ///
    fn find_constant(
        &self,
        globals: &mut Globals,
        site_id: ConstSiteId,
    ) -> Result<(Value, Option<Value>)> {
        let base = globals.store[site_id]
            .base
            .map(|base| unsafe { self.register(base.0 as usize) }.unwrap());
        let current_func = self.method_func_id();
        globals.find_constant(site_id, current_func, base)
    }

    fn set_constant(&self, globals: &mut Globals, name: IdentId, val: Value) {
        let parent = self.context_class_id();
        globals.set_constant(parent, name, val);
    }
}

// Invokation of methods and blocks.

impl Executor {
    ///
    /// Invoke method for *receiver* and *method*.
    ///
    pub(crate) fn invoke_method(
        &mut self,
        globals: &mut Globals,
        method: IdentId,
        receiver: Value,
        args: &[Value],
        block_handler: Option<BlockHandler>,
    ) -> Option<Value> {
        let func_id = match globals.find_method(receiver, method, false) {
            Ok(id) => id,
            Err(err) => {
                self.set_error(err);
                return None;
            }
        };
        (globals.codegen.method_invoker)(
            self,
            globals,
            func_id,
            receiver,
            args.as_ptr(),
            args.len(),
            block_handler,
        )
    }

    ///
    /// Invoke method for *receiver* and *method*.
    ///
    pub(crate) fn invoke_method_inner(
        &mut self,
        globals: &mut Globals,
        method: IdentId,
        receiver: Value,
        args: &[Value],
        block_handler: Option<BlockHandler>,
    ) -> Result<Value> {
        let func_id = globals.find_method(receiver, method, false)?;
        match (globals.codegen.method_invoker)(
            self,
            globals,
            func_id,
            receiver,
            args.as_ptr(),
            args.len(),
            block_handler,
        ) {
            Some(res) => Ok(res),
            None => Err(self.take_error()),
        }
    }

    ///
    /// Invoke method for *receiver* and *method*.
    ///
    pub(crate) fn invoke_method_inner2(
        &mut self,
        globals: &mut Globals,
        method: IdentId,
        receiver: Value,
        args: Arg,
        len: usize,
        block_handler: Option<BlockHandler>,
    ) -> Result<Value> {
        let func_id = globals.find_method(receiver, method, false)?;
        match (globals.codegen.method_invoker2)(
            self,
            globals,
            func_id,
            receiver,
            args,
            len,
            block_handler,
        ) {
            Some(res) => Ok(res),
            None => Err(self.take_error()),
        }
    }

    ///
    /// Invoke block for *block_handler*.
    ///
    /// To get BlockData, use get_block_data().
    ///  
    /// let data = globals.get_block_data(cfp, block);
    ///
    pub(crate) fn invoke_block(
        &mut self,
        globals: &mut Globals,
        data: &ProcInner,
        args: &[Value],
    ) -> Result<Value> {
        match (globals.codegen.block_invoker)(
            self,
            globals,
            data,
            Value::nil(),
            args.as_ptr(),
            args.len(),
        ) {
            Some(val) => Ok(val),
            None => Err(self.take_error()),
        }
    }

    pub(crate) fn invoke_block_with_self(
        &mut self,
        globals: &mut Globals,
        data: &ProcInner,
        self_val: Value,
        args: &[Value],
    ) -> Result<Value> {
        match (globals.codegen.block_invoker_with_self)(
            self,
            globals,
            data as _,
            self_val,
            args.as_ptr(),
            args.len(),
        ) {
            Some(val) => Ok(val),
            None => Err(self.take_error()),
        }
    }

    pub(crate) fn invoke_block_once(
        &mut self,
        globals: &mut Globals,
        bh: BlockHandler,
        args: &[Value],
    ) -> Result<Value> {
        let data = globals.get_block_data(self.cfp(), bh);
        self.invoke_block(globals, &data, args)
    }

    pub(crate) fn invoke_block_iter1(
        &mut self,
        globals: &mut Globals,
        bh: BlockHandler,
        iter: impl Iterator<Item = Value>,
    ) -> Result<()> {
        let data = globals.get_block_data(self.cfp(), bh);
        for val in iter {
            self.invoke_block(globals, &data, &[val])?;
        }
        Ok(())
    }

    pub(crate) fn invoke_block_map1(
        &mut self,
        globals: &mut Globals,
        bh: BlockHandler,
        iter: impl Iterator<Item = Value>,
    ) -> Result<Vec<Value>> {
        let data = globals.get_block_data(self.cfp(), bh);
        let t = self.temp_len();
        for v in iter {
            let res = self.invoke_block(globals, &data, &[v])?;
            self.temp_push(res);
        }
        let vec = self.temp_tear(t);
        Ok(vec)
    }

    pub(crate) fn flat_map(
        &mut self,
        globals: &mut Globals,
        bh: BlockHandler,
        iter: impl Iterator<Item = Value>,
    ) -> Result<Vec<Value>> {
        let mut v = vec![];
        for elem in self.invoke_block_map1(globals, bh, iter)? {
            match elem.is_array() {
                Some(ary) => v.extend(ary.iter()),
                None => v.push(elem),
            }
        }
        Ok(v)
    }

    pub(crate) fn invoke_block_fold1(
        &mut self,
        globals: &mut Globals,
        bh: BlockHandler,
        iter: impl Iterator<Item = Value>,
        mut res: Value,
    ) -> Result<Value> {
        let data = globals.get_block_data(self.cfp(), bh);
        for elem in iter {
            res = self.invoke_block(globals, &data, &[res, elem])?;
        }
        Ok(res)
    }

    ///
    /// Invoke proc.
    ///
    pub(crate) fn invoke_proc(
        &mut self,
        globals: &mut Globals,
        proc: Value,
        args: &[Value],
    ) -> Result<Value> {
        (globals.codegen.block_invoker)(
            self,
            globals,
            proc.as_proc(),
            Value::nil(),
            args.as_ptr(),
            args.len(),
        )
        .ok_or_else(|| self.take_error())
    }

    pub(crate) fn invoke_method_if_exists(
        &mut self,
        globals: &mut Globals,
        method: IdentId,
        receiver: Value,
        args: Arg,
        len: usize,
        bh: Option<BlockHandler>,
    ) -> Result<Value> {
        if let Some(func_id) = globals.check_method(receiver, method) {
            self.invoke_func2(globals, func_id, receiver, args, len, bh)
        } else {
            Ok(Value::nil())
        }
    }

    ///
    /// Invoke func with *args*: Args.
    ///
    pub(crate) fn invoke_func2(
        &mut self,
        globals: &mut Globals,
        func_id: FuncId,
        receiver: Value,
        args: Arg,
        len: usize,
        bh: Option<BlockHandler>,
    ) -> Result<Value> {
        let bh = bh.map(|bh| bh.delegate());
        (globals.codegen.method_invoker2)(self, globals, func_id, receiver, args, len, bh)
            .ok_or_else(|| self.take_error())
    }

    ///
    /// Invoke func with *args*: Args.
    ///
    fn invoke_func(
        &mut self,
        globals: &mut Globals,
        func_id: FuncId,
        receiver: Value,
        args: &[Value],
        bh: Option<BlockHandler>,
    ) -> Option<Value> {
        let bh = bh.map(|bh| bh.delegate());
        (globals.codegen.method_invoker)(
            self,
            globals,
            func_id,
            receiver,
            args.as_ptr(),
            args.len(),
            bh,
        )
    }

    fn define_class(
        &mut self,
        globals: &mut Globals,
        name: IdentId,
        superclass: Option<Value>,
        is_module: u32,
    ) -> Result<Value> {
        let parent = self.context_class_id();
        let self_val = match globals.get_constant(parent, name) {
            Some(val) => {
                val.expect_class_or_module(globals)?;
                if let Some(superclass) = superclass {
                    assert!(is_module != 1);
                    let superclass_id = superclass.expect_class(globals)?;
                    if Some(superclass_id) != val.as_class().superclass_id() {
                        return Err(MonorubyErr::superclass_mismatch(name));
                    }
                }
                val.as_class()
            }
            None => {
                let superclass = match superclass {
                    Some(superclass) => {
                        assert!(is_module != 1);
                        superclass.expect_class(globals)?;
                        superclass.as_class()
                    }
                    None => globals.object_class(),
                };
                globals.define_class(name, Some(superclass), parent, is_module == 1)
            }
        };
        self.push_class_context(self_val.id());
        Ok(self_val.as_val())
    }
}

impl Executor {
    pub fn generate_proc(&mut self, globals: &mut Globals, bh: BlockHandler) -> Result<Proc> {
        if bh.try_proxy().is_some() {
            let outer_lfp = self.cfp().prev().unwrap().lfp();
            outer_lfp.move_frame_to_heap();
            let proc = Proc::new(globals.get_block_data(self.cfp(), bh));
            Ok(proc)
        } else if bh.try_proc().is_some() {
            Ok(bh.0.into())
        } else {
            unimplemented!()
        }
    }

    ///
    /// Generate a proc object for Enumerator.
    ///
    /// this method use current `self` for the `obj` field for the Enumerator.
    ///
    /// ### args
    /// - *method*: the method name to generate a proc.
    ///
    /// ### return
    /// - the generated proc object.
    ///
    pub fn generate_enumerator(
        &mut self,
        globals: &mut Globals,
        method: IdentId,
        obj: Value,
        args: Vec<Value>,
    ) -> Result<Value> {
        let func_data = globals.get_func_data(FuncId::new(1)).clone();
        let outer_lfp = LFP::dummy_heap_frame_with_self(obj);
        let proc = Proc::from(outer_lfp, func_data);
        let e = Value::new_enumerator(obj, method, proc, args);
        Ok(e)
    }
}

// Handling special variables.

impl Executor {
    ///
    /// Save captured strings to special variables.
    ///
    /// - $n (n:0,1,2,3...) <- The string which matched with nth parenthesis in the last successful match.
    ///
    /// - $& <- The string which matched successfully at last.
    ///
    /// - $' <- The string after $&.
    ///
    pub(crate) fn save_captures(&mut self, captures: &Captures, given: &str) {
        //let id1 = IdentId::get_id("$&");
        //let id2 = IdentId::get_id("$'");
        match captures.get(0) {
            Some(m) => {
                self.sp_last_match = Some(Value::string_from_str(&given[m.start()..m.end()]));
                self.sp_post_match = Some(Value::string_from_str(&given[m.end()..]));
            }
            None => {
                self.sp_last_match = None;
                self.sp_post_match = None;
            }
        };

        self.sp_matches.clear();
        for i in 0..captures.len() {
            self.sp_matches.push(
                captures
                    .get(i)
                    .map(|m| Value::string_from_str(&given[m.start()..m.end()])),
            );
        }
    }

    pub(crate) fn get_special_matches(&self, mut nth: i64) -> Value {
        if nth < 0 {
            nth += self.sp_matches.len() as i64
        }
        if nth >= 0 {
            self.sp_matches
                .get(nth as usize)
                .cloned()
                .unwrap_or_default()
                .unwrap_or_default()
        } else {
            Value::nil()
        }
    }

    pub(crate) fn get_last_matchdata(&self) -> Value {
        Value::array_from_iter(self.sp_matches.iter().map(|v| v.unwrap_or_default()))
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct BlockHandler(pub Value);

impl std::fmt::Debug for BlockHandler {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some((fid, outer)) = self.try_proxy() {
            write!(f, "fn:{:?} outer:{outer}", fid)
        } else if let Some(proc) = self.try_proc() {
            write!(f, "proc:{:?}", proc)
        } else {
            write!(f, "unknown handler {:016x}", self.0.id())
        }
    }
}

impl BlockHandler {
    pub fn new(val: Value) -> Self {
        Self(val)
    }

    pub fn from(func_id: FuncId) -> Self {
        let block_handler = ((u32::from(func_id) as i64) << 16) + 1;
        let bh = Value::integer(block_handler);
        Self::new(bh)
    }

    pub fn try_proxy(&self) -> Option<(FuncId, u16)> {
        self.0.try_fixnum().map(|i| {
            let i = i as u64;
            let func_id = FuncId::new(u32::try_from(i >> 16).unwrap());
            let idx = i as u16;
            (func_id, idx)
        })
    }

    pub fn delegate(self) -> Self {
        match self.0.try_fixnum() {
            Some(i) => Self(Value::integer(i + 1)),
            None => self,
        }
    }

    pub fn try_proc(&self) -> Option<&ProcInner> {
        self.0.is_proc()
    }

    pub(crate) fn as_proc(&self) -> &ProcInner {
        self.0.as_proc()
    }
}

#[derive(Debug, Clone)]
struct Cref {
    class_id: ClassId,
    module_function: bool,
    visibility: Visibility,
}

impl Cref {
    fn new(class_id: ClassId, module_function: bool, visibility: Visibility) -> Self {
        Self {
            class_id,
            module_function,
            visibility,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum FiberState {
    Created,
    Suspended,
    Terminated,
}

#[derive(Clone, Copy, PartialEq)]
#[repr(C)]
pub(crate) struct Bc {
    pub op1: u64,
    pub op2: Bc2,
}

impl Bc {
    pub fn is_integer1(&self) -> bool {
        self.classid1() == INTEGER_CLASS
    }

    pub fn is_integer2(&self) -> bool {
        self.classid2() == INTEGER_CLASS
    }

    pub fn is_float1(&self) -> bool {
        self.classid1() == FLOAT_CLASS
    }

    pub fn is_float2(&self) -> bool {
        self.classid2() == FLOAT_CLASS
    }

    pub fn is_integer_binop(&self) -> bool {
        self.classid1() == INTEGER_CLASS && self.classid2() == INTEGER_CLASS
    }

    pub fn is_float_binop(&self) -> bool {
        match (self.classid1(), self.classid2()) {
            (INTEGER_CLASS, INTEGER_CLASS) => false,
            (INTEGER_CLASS | FLOAT_CLASS, INTEGER_CLASS | FLOAT_CLASS) => true,
            _ => false,
        }
    }
    pub fn classid1(&self) -> ClassId {
        ClassId::new(self.op2.0 as u32)
    }

    pub fn classid2(&self) -> ClassId {
        ClassId::new((self.op2.0 >> 32) as u32)
    }

    pub fn cached_version(&self) -> u32 {
        let op = self.op2.0;
        (op >> 32) as u32
    }

    pub fn cached_ivarid(&self) -> IvarId {
        let op = self.op2.0;
        IvarId::new((op >> 32) as u32)
    }

    pub fn cached_class(&self) -> ClassId {
        let op = self.op2.0;
        ClassId::new(op as u32)
    }

    pub fn value(&self) -> Option<Value> {
        match self.op2.0 {
            0 => None,
            v => Some(Value::from(v)),
        }
    }

    #[cfg(feature = "dump-bc")]
    pub fn into_jit_addr(self) -> u64 {
        self.op2.0
    }

    pub(super) fn from(op1: u64) -> Self {
        Self {
            op1,
            op2: Bc2::from(0),
        }
    }

    pub(super) fn from_u32(op1: u64, op2: u32) -> Self {
        Self {
            op1,
            op2: Bc2::from(op2 as u64),
        }
    }

    pub(super) fn from_fn_info(op1: u64, fn_info: &FnInitInfo) -> Self {
        let FnInitInfo {
            arg_num,
            block_pos,
            req_num,
            info,
            ..
        } = fn_info;
        Bc::from_with_num(
            op1,
            *req_num as u16,
            *block_pos as u16,
            *info as u16,
            *arg_num as u16,
        )
    }

    pub(super) fn from_with_callid(op1: u64, callid: CallSiteId) -> Self {
        Self {
            op1,
            op2: Bc2::from(callid.get() as u64),
        }
    }

    pub(super) fn from_with_value(op1: u64, val: Value) -> Self {
        Self {
            op1,
            op2: Bc2::from(val.id()),
        }
    }

    pub(super) fn from_with_func_name_id(op1: u64, name: Option<IdentId>, func_id: FuncId) -> Self {
        Self {
            op1,
            op2: Bc2::from(
                ((func_id.get() as u64) << 32)
                    + (if let Some(name) = name {
                        name.get() as u64
                    } else {
                        0
                    }),
            ),
        }
    }

    pub(super) fn from_with_class_and_version(op1: u64, class_id: ClassId, version: u32) -> Self {
        Self {
            op1,
            op2: Bc2::class_and_version(class_id, version),
        }
    }

    pub(super) fn from_with_class2(op1: u64) -> Self {
        Self {
            op1,
            op2: Bc2::class2(ClassId::default(), ClassId::default()),
        }
    }

    fn from_with_num(op1: u64, num0: u16, num1: u16, num2: u16, num3: u16) -> Self {
        Self {
            op1,
            op2: Bc2::from(
                ((num3 as u64) << 48)
                    + ((num2 as u64) << 32)
                    + ((num1 as u64) << 16)
                    + (num0 as u64),
            ),
        }
    }

    pub fn u16(&self, id: usize) -> u16 {
        (self.op2.0 >> (id * 16)) as u16
    }

    pub fn func_id(&self) -> Option<FuncId> {
        let op = self.op2.0 as u32;
        if op == 0 {
            None
        } else {
            Some(FuncId::new(op))
        }
    }
}

///
/// Program counter base.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
struct BcPcBase(std::ptr::NonNull<Bc>);

impl std::ops::Add<usize> for BcPcBase {
    type Output = BcPc;
    fn add(self, rhs: usize) -> BcPc {
        BcPc::new(unsafe { self.as_ptr().add(rhs) })
    }
}

impl std::ops::Add<BcIndex> for BcPcBase {
    type Output = BcPc;
    fn add(self, rhs: BcIndex) -> BcPc {
        BcPc(unsafe { std::ptr::NonNull::new(self.as_ptr().offset(rhs.0 as isize)).unwrap() })
    }
}

impl BcPcBase {
    fn as_ptr(&self) -> *mut Bc {
        self.0.as_ptr()
    }
}

///
/// Program counter
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub(crate) struct BcPc(std::ptr::NonNull<Bc>);

impl BcPc {
    fn new(ptr: *mut Bc) -> Self {
        Self(std::ptr::NonNull::new(ptr).unwrap())
    }

    fn as_ptr(&self) -> *mut Bc {
        self.0.as_ptr()
    }

    fn is_loop(&self) -> bool {
        matches!(self.get_ir(), TraceIr::LoopStart(_))
    }

    fn opcode(&self) -> u16 {
        (self.op1 >> 48) as u16
    }

    pub fn from(bc: &Bc) -> Self {
        Self(std::ptr::NonNull::from(bc))
    }

    fn get_u64(self) -> u64 {
        self.0.as_ptr() as _
    }

    fn write2(self, data: u64) {
        unsafe { *((self.as_ptr() as *mut u64).add(1)) = data }
    }
}

impl std::ops::Sub<BcPcBase> for BcPc {
    type Output = usize;
    fn sub(self, rhs: BcPcBase) -> usize {
        let offset = unsafe { self.as_ptr().offset_from(rhs.as_ptr()) };
        assert!(offset >= 0, "self:{:?} rhs:{:?}", self, rhs);
        offset as usize
    }
}

impl std::ops::Sub<BcPc> for BcPc {
    type Output = usize;
    fn sub(self, rhs: BcPc) -> usize {
        let offset = unsafe { self.as_ptr().offset_from(rhs.as_ptr()) };
        assert!(offset >= 0, "self:{:?} rhs:{:?}", self, rhs);
        offset as usize
    }
}

impl std::ops::Add<isize> for BcPc {
    type Output = BcPc;
    fn add(self, rhs: isize) -> BcPc {
        BcPc::new(unsafe { self.as_ptr().offset(rhs) })
    }
}

impl std::ops::Sub<isize> for BcPc {
    type Output = BcPc;
    fn sub(self, rhs: isize) -> BcPc {
        BcPc::new(unsafe { self.as_ptr().offset(-rhs) })
    }
}

impl std::ops::AddAssign<i32> for BcPc {
    fn add_assign(&mut self, offset: i32) {
        unsafe {
            *self = BcPc::new(self.as_ptr().offset(offset as isize));
        }
    }
}

impl std::ops::Deref for BcPc {
    type Target = Bc;
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl BcPc {
    pub fn get_ir(&self) -> TraceIr {
        TraceIr::from_bc(*self)
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub(crate) struct SlotId(pub u16);

impl std::iter::Step for SlotId {
    fn steps_between(start: &Self, end: &Self) -> Option<usize> {
        if start.0 <= end.0 {
            Some((end.0 - start.0) as usize)
        } else {
            None
        }
    }

    fn forward_checked(start: Self, count: usize) -> Option<Self> {
        if start.0 + count as u16 <= std::u16::MAX {
            Some(Self(start.0 + count as u16))
        } else {
            None
        }
    }

    fn backward_checked(start: Self, count: usize) -> Option<Self> {
        if start.0 >= count as u16 {
            Some(Self(start.0 - count as u16))
        } else {
            None
        }
    }
}

impl SlotId {
    pub fn new(reg: u16) -> Self {
        Self(reg)
    }

    pub fn from(reg: u16) -> Option<Self> {
        if reg == 0 {
            None
        } else {
            Some(Self(reg))
        }
    }

    pub fn self_() -> Self {
        Self(0)
    }

    pub fn is_zero(&self) -> bool {
        self.0 == 0
    }
}

impl std::fmt::Debug for SlotId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl std::ops::Add<u16> for SlotId {
    type Output = Self;
    fn add(self, rhs: u16) -> Self {
        Self(self.0 + rhs)
    }
}

///
/// kinds of binary operation.
///
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOpK {
    Add = 0,
    Sub = 1,
    Mul = 2,
    Div = 3,
    BitOr = 4,
    BitAnd = 5,
    BitXor = 6,
    Rem = 7,
    Exp = 8,
}

use std::{fmt, ptr::NonNull};
impl fmt::Display for BinOpK {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match *self {
            BinOpK::Add => "+",
            BinOpK::Sub => "-",
            BinOpK::Mul => "*",
            BinOpK::Div => "/",
            BinOpK::BitOr => "|",
            BinOpK::BitAnd => "&",
            BinOpK::BitXor => "^",
            BinOpK::Rem => "%",
            BinOpK::Exp => "**",
        };
        write!(f, "{}", s)
    }
}

impl BinOpK {
    pub fn from(i: u16) -> Self {
        match i {
            0 => BinOpK::Add,
            1 => BinOpK::Sub,
            2 => BinOpK::Mul,
            3 => BinOpK::Div,
            4 => BinOpK::BitOr,
            5 => BinOpK::BitAnd,
            6 => BinOpK::BitXor,
            7 => BinOpK::Rem,
            8 => BinOpK::Exp,
            _ => unreachable!(),
        }
    }

    fn generic_func(&self) -> BinaryOpFn {
        match self {
            BinOpK::Add => add_values,
            BinOpK::Sub => sub_values,
            BinOpK::Mul => mul_values,
            BinOpK::Div => div_values,
            BinOpK::BitOr => bitor_values,
            BinOpK::BitAnd => bitand_values,
            BinOpK::BitXor => bitxor_values,
            BinOpK::Rem => rem_values,
            BinOpK::Exp => pow_values,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Visibility {
    Public = 0,
    Protected = 1,
    Private = 2,
}

/*impl Visibility {
    pub fn is_public(&self) -> bool {
        self == &Self::Public
    }

    pub fn is_private(&self) -> bool {
        self == &Self::Private
    }
}*/

extern "C" fn exec_jit_compile_patch(
    globals: &mut Globals,
    func_id: FuncId,
    self_value: Value,
    entry: monoasm::DestLabel,
) {
    let patch_point = globals.codegen.jit.label();
    let entry_label = globals.codegen.jit.label();
    let guard = globals.codegen.jit.label();
    globals
        .codegen
        .class_guard_stub(self_value.class(), patch_point, entry_label, guard);

    assert!(globals[func_id]
        .add_jit_code(self_value.class(), patch_point)
        .is_none());
    globals.exec_jit_compile_method(func_id, self_value, entry_label);

    globals.codegen.jit.apply_jmp_patch(entry, guard);
}

extern "C" fn exec_jit_recompile_method(globals: &mut Globals, func_id: FuncId, self_value: Value) {
    let entry_label = globals.codegen.jit.label();
    globals.exec_jit_compile_method(func_id, self_value, entry_label);
    let patch_point = globals[func_id].get_jit_code(self_value.class()).unwrap();
    globals
        .codegen
        .jit
        .apply_jmp_patch(patch_point, entry_label);
}

///
/// Compile the loop.
///
extern "C" fn exec_jit_partial_compile(
    globals: &mut Globals,
    func_id: FuncId,
    self_value: Value,
    pc: BcPc,
) {
    let entry_label = globals.codegen.jit.label();
    globals.exec_jit_compile(func_id, self_value, Some(pc), entry_label);
    let codeptr = globals.codegen.jit.get_label_address(entry_label);
    pc.write2(codeptr.as_ptr() as u64);
}

struct Root<'a, 'b> {
    globals: &'a Globals,
    executor: &'b Executor,
}

impl<'a, 'b> alloc::GC<RValue> for Root<'a, 'b> {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.globals.mark(alloc);
        self.executor.mark(alloc);
    }
}

impl<'a, 'b> alloc::GCRoot<RValue> for Root<'a, 'b> {
    fn startup_flag(&self) -> bool {
        true
    }
}

///
/// Execute garbage collection.
///
extern "C" fn execute_gc(globals: &Globals, mut executor: &Executor) {
    // Get root Executor.
    while let Some(parent) = executor.parent_fiber {
        executor = unsafe { parent.as_ref() };
    }
    alloc::ALLOC.with(|alloc| alloc.borrow_mut().gc(&Root { globals, executor }));
}
