use super::*;
use monoasm::*;

pub(crate) mod compiler;
mod frame;
pub mod inline;
pub mod op;
pub use builtins::*;
use bytecodegen::*;
use compiler::jitgen::trace_ir::*;
pub use compiler::Codegen;
pub use compiler::*;
use fancy_regex::Captures;
pub use frame::*;
pub use globals::*;
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
const LBP_META_REGNUM: i64 = LBP_META - 4;
/// Meta::FuncId 4bytes
const LBP_META_FUNCID: i64 = LBP_META;
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

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum OpMode {
    RR(SlotId, SlotId),
    RI(SlotId, i16),
    IR(i16, SlotId),
}

impl OpMode {
    fn is_float_op(&self, pc: &Bc) -> bool {
        match self {
            Self::RR(..) => pc.is_float_binop(),
            Self::RI(..) => pc.is_float1(),
            Self::IR(..) => pc.is_float2(),
        }
    }

    fn is_integer_op(&self, pc: &Bc) -> bool {
        match self {
            Self::RR(..) => pc.is_integer_binop(),
            Self::RI(..) => pc.is_integer1(),
            Self::IR(..) => pc.is_integer2(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Cref {
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

impl Executor {
    pub fn init(globals: &mut Globals) -> Self {
        let mut executor = Self::default();
        let path = std::path::Path::new("startup/startup.rb");
        let code = include_str!("../startup/startup.rb").to_string();
        if let Err(err) = executor.eval_script(globals, code, path) {
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

//
// error handlers
//
impl Executor {
    pub fn set_error(&mut self, err: MonorubyErr) {
        self.exception = Some(err);
    }

    pub(crate) fn exception(&self) -> Option<&MonorubyErr> {
        self.exception.as_ref()
    }

    pub(crate) fn take_error(&mut self) -> MonorubyErr {
        std::mem::take(&mut self.exception).unwrap()
    }

    pub(crate) fn take_ex_obj(&mut self, globals: &Globals) -> Value {
        let err = self.take_error();
        self.exception_to_val(globals, err)
    }

    pub(crate) fn err_divide_by_zero(&mut self) {
        self.set_error(MonorubyErr::divide_by_zero());
    }

    pub(crate) fn err_wrong_number_of_arg_range(
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

    pub(crate) fn push_error_location(&mut self, loc: Loc, sourceinfo: SourceInfoRef) {
        match &mut self.exception {
            Some(err) => {
                err.push_trace(loc, sourceinfo);
            }
            None => unreachable!(),
        };
    }

    pub fn exception_to_val(&self, globals: &Globals, err: MonorubyErr) -> Value {
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

impl Executor {
    ///
    /// Execute top level method.
    ///
    /// *main* object is set to *self*.
    pub fn execute(&mut self, globals: &mut Globals, func_id: FuncId) -> Result<Value> {
        #[cfg(feature = "emit-bc")]
        globals.dump_bc();

        let res = (globals.codegen.entry_point)(self, globals, func_id);
        res.ok_or_else(|| self.take_error())
    }

    pub(crate) fn eval_script(
        &mut self,
        globals: &mut Globals,
        code: String,
        path: &std::path::Path,
    ) -> Result<Value> {
        globals
            .compile_script(code, path)
            .and_then(|fid| self.execute(globals, fid))
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
            .map(|base| unsafe { self.cfp().lfp().register(base.0 as usize) }.unwrap());
        let current_func = self.method_func_id();
        globals.find_constant(site_id, current_func, base)
    }

    fn set_constant(&self, globals: &mut Globals, name: IdentId, val: Value) {
        let parent = self.context_class_id();
        globals.set_constant(parent, name, val);
    }
}

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

    pub(crate) fn is_branch(self) -> Option<i32> {
        let op = self.op1;
        let opcode = self.opcode();
        if opcode & 0x80 == 0 {
            let (_, op2) = compiler::jitgen::trace_ir::dec_wl(op);
            match opcode {
                3 => Some(op2 as i32),       // Br
                4 => Some(op2 as i32),       // CondBr
                5 => Some(op2 as i32),       // CondBr
                12..=13 => Some(op2 as i32), // CondBr
                20 => Some(op2 as i32),      // CheckLocal
                _ => None,
            }
        } else {
            None
        }
    }

    pub(crate) fn is_terminal(self) -> bool {
        let opcode = self.opcode();
        // Br or Ret or MethodRet or Break or Raise
        opcode == 3 || opcode == 80 || opcode == 81 || opcode == 82 || opcode == 83
    }

    pub(crate) fn is_loop_start(self) -> bool {
        let opcode = self.opcode();
        opcode == 14 // LoopStart
    }

    pub(crate) fn is_loop_end(self) -> bool {
        let opcode = self.opcode();
        opcode == 15 // LoopEnd
    }
}

impl BcPc {
    #[cfg(feature = "dump-bc")]
    pub fn format(&self, globals: &Globals, i: usize) -> Option<String> {
        fn optstr(opt: bool) -> &'static str {
            if opt {
                "_"
            } else {
                ""
            }
        }
        let s = match TraceIr::from_bc(*self) {
            TraceIr::InitMethod(info) => {
                format!("init_method {info:?}")
            }
            TraceIr::CheckLocal(local, disp) => {
                format!("check_local({:?}) =>:{:05}", local, i as i32 + 1 + disp)
            }
            TraceIr::Br(disp) => {
                format!("br =>:{:05}", i as i32 + 1 + disp)
            }
            TraceIr::CondBr(reg, disp, opt, kind) => {
                format!(
                    "cond{}br {}{:?} =>:{:05}",
                    kind.to_s(),
                    optstr(opt),
                    reg,
                    i as i32 + 1 + disp
                )
            }
            TraceIr::Integer(reg, num) => format!("{:?} = {}: i32", reg, num),
            TraceIr::Symbol(reg, id) => format!("{:?} = :{id}", reg),
            TraceIr::Range {
                dst,
                start,
                end,
                exclude_end,
            } => format!(
                "{:?} = {:?} {} {:?}",
                dst,
                start,
                if exclude_end { "..." } else { ".." },
                end
            ),
            TraceIr::Literal(reg, val) => {
                format!("{:?} = literal[{}]", reg, globals.inspect(val))
            }
            TraceIr::Array { dst, callid } => {
                let CallSiteInfo { args, pos_num, .. } = globals.store[callid];
                format!("{:?} = array[{:?}; {}]", dst, args, pos_num)
            }
            TraceIr::Hash { dst, args, len } => {
                format!("{:?} = hash[{:?}; {}]", dst, args, len)
            }
            TraceIr::Index { dst, base, idx } => {
                let op1 = format!("{:?} = {:?}.[{:?}]", dst, base, idx);
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    self.classid1().get_name(globals),
                    self.classid2().get_name(globals)
                )
            }
            TraceIr::IndexAssign { src, base, idx } => {
                format!("{:?}:.[{:?}:] = {:?}", base, idx, src,)
            }
            TraceIr::LoadConst(reg, id) => {
                let ConstSiteInfo {
                    name,
                    base,
                    prefix,
                    toplevel,
                    ..
                } = &globals.store[id];
                let mut const_name = if *toplevel { "::" } else { "" }.to_string();
                for c in prefix {
                    c.append_to(&mut const_name);
                    const_name += "::";
                }
                name.append_to(&mut const_name);
                let op1 = format!(
                    "{:?} = {}const[{}]",
                    reg,
                    if let Some(base) = base {
                        format!("{:?}::", base)
                    } else {
                        "".to_string()
                    },
                    const_name
                );
                format!(
                    "{:36} [{}]",
                    op1,
                    match self.value() {
                        None => "<INVALID>".to_string(),
                        Some(val) => globals.inspect(val),
                    }
                )
            }
            TraceIr::StoreConst(src, id) => {
                format!("const[{id}] = {:?}", src)
            }
            TraceIr::BlockArgProxy(dst, outer) => {
                format!("{:?} = block_proxy({outer})", dst)
            }
            TraceIr::BlockArg(dst, outer) => {
                format!("{:?} = block_arg({outer})", dst)
            }
            TraceIr::LoadDynVar(dst, src) => {
                format!("{:?} = {:?}", dst, src)
            }
            TraceIr::StoreDynVar(dst, src) => {
                format!("{:?} = {:?}", dst, src)
            }
            TraceIr::LoadIvar(reg, id, class_id, ivar_id) => {
                format!(
                    "{:?} = {id}: {}",
                    reg,
                    if let Some(id) = class_id {
                        format!("{}[{:?}]", id.get_name(globals), ivar_id)
                    } else {
                        format!("-")
                    }
                )
            }
            TraceIr::StoreIvar(reg, id, class_id, ivar_id) => {
                format!(
                    "{id}: {} = {:?}",
                    if let Some(id) = class_id {
                        format!("{}[{:?}]", id.get_name(globals), ivar_id)
                    } else {
                        format!("-")
                    },
                    reg
                )
            }
            TraceIr::LoadGvar { dst: ret, name } => {
                format!("{:?} = {name}", ret)
            }
            TraceIr::StoreGvar { src, name } => {
                format!("{name} = {:?}", src)
            }
            TraceIr::LoadSvar { dst: ret, id } => {
                // 0 => $&
                // 1 => $'
                // 100 + n => $n
                format!(
                    "{:?} = ${}",
                    ret,
                    match id {
                        0 => "&".to_string(),
                        1 => "'".to_string(),
                        n => (n - 100).to_string(),
                    }
                )
            }
            TraceIr::Nil(reg) => format!("{:?} = nil", reg),
            TraceIr::BitNot { dst, src } => {
                let op1 = format!("{:?} = ~{:?}", dst, src);
                format!("{:36} [{}]", op1, self.classid1().get_name(globals),)
            }
            TraceIr::Pos { dst, src } => {
                let op1 = format!("{:?} = +{:?}", dst, src);
                format!("{:36} [{}]", op1, self.classid1().get_name(globals),)
            }
            TraceIr::Neg { dst, src } => {
                let op1 = format!("{:?} = -{:?}", dst, src);
                format!("{:36} [{}]", op1, self.classid1().get_name(globals),)
            }
            TraceIr::Not { dst, src } => {
                let op1 = format!("{:?} = !{:?}", dst, src);
                format!("{:36} [{}]", op1, self.classid1().get_name(globals),)
            }
            TraceIr::BinOp {
                kind,
                dst,
                mode: OpMode::RR(lhs, rhs),
            }
            | TraceIr::IBinOp {
                kind,
                dst,
                mode: OpMode::RR(lhs, rhs),
            }
            | TraceIr::FBinOp {
                kind,
                dst,
                mode: OpMode::RR(lhs, rhs),
            } => {
                let op1 = format!("{} = {:?} {} {:?}", ret_str(dst), lhs, kind, rhs);
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    self.classid1().get_name(globals),
                    self.classid2().get_name(globals)
                )
            }
            TraceIr::BinOp {
                kind,
                dst,
                mode: OpMode::RI(lhs, rhs),
            }
            | TraceIr::IBinOp {
                kind,
                dst,
                mode: OpMode::RI(lhs, rhs),
            }
            | TraceIr::FBinOp {
                kind,
                dst,
                mode: OpMode::RI(lhs, rhs),
            } => {
                let op1 = format!("{} = {:?} {} {}: i16", ret_str(dst), lhs, kind, rhs,);
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    self.classid1().get_name(globals),
                    self.classid2().get_name(globals)
                )
            }
            TraceIr::BinOp {
                kind,
                dst,
                mode: OpMode::IR(lhs, rhs),
            }
            | TraceIr::IBinOp {
                kind,
                dst,
                mode: OpMode::IR(lhs, rhs),
            }
            | TraceIr::FBinOp {
                kind,
                dst,
                mode: OpMode::IR(lhs, rhs),
            } => {
                let op1 = format!("{} = {}: i16 {} {:?}", ret_str(dst), lhs, kind, rhs,);
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    self.classid1().get_name(globals),
                    self.classid2().get_name(globals)
                )
            }
            TraceIr::Cmp(kind, dst, mode, opt) => {
                let op1 = match mode {
                    OpMode::RR(lhs, rhs) => {
                        format!(
                            "{}{} = {:?} {:?} {:?}",
                            optstr(opt),
                            ret_str(dst),
                            lhs,
                            kind,
                            rhs,
                        )
                    }
                    OpMode::RI(lhs, rhs) => format!(
                        "{}{} = {:?} {:?} {}: i16",
                        optstr(opt),
                        ret_str(dst),
                        lhs,
                        kind,
                        rhs,
                    ),
                    _ => unreachable!(),
                };
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    self.classid1().get_name(globals),
                    self.classid2().get_name(globals)
                )
            }

            TraceIr::Ret(reg) => format!("ret {:?}", reg),
            TraceIr::MethodRet(reg) => format!("method_ret {:?}", reg),
            TraceIr::Break(reg) => format!("break {:?}", reg),
            TraceIr::Raise(reg) => format!("raise {:?}", reg),
            TraceIr::EnsureEnd => format!("ensure_end"),
            TraceIr::Mov(dst, src) => format!("{:?} = {:?}", dst, src),
            TraceIr::MethodCall {
                callid,
                has_splat,
                class,
                ..
            }
            | TraceIr::MethodCallBlock {
                callid,
                has_splat,
                class,
                ..
            } => {
                let callsite = &globals.store[callid];
                let name = callsite.name.unwrap();
                let CallSiteInfo {
                    recv,
                    args,
                    pos_num,
                    ret,
                    block_fid,
                    block_arg,
                    ..
                } = *callsite;
                let kw_len = callsite.kw_args.len();
                let op1 = format!(
                    "{} = {:?}.{name}({}{}{}){}",
                    ret_str(ret),
                    recv,
                    if pos_num == 0 {
                        "".to_string()
                    } else {
                        format!("{:?};{}{}", args, pos_num, if has_splat { "*" } else { "" })
                    },
                    if kw_len == 0 {
                        "".to_string()
                    } else {
                        format!(" kw:{:?};{}", args + pos_num as u16, kw_len)
                    },
                    if let Some(block_arg) = block_arg {
                        format!(" &{:?}", block_arg)
                    } else {
                        "".to_string()
                    },
                    if let Some(block_fid) = block_fid {
                        format!(" {{ {:?} }}", block_fid)
                    } else {
                        "".to_string()
                    },
                );
                format!("{:36} [{}]", op1, class.get_name(globals))
            }
            TraceIr::Super { callid, class, .. } => {
                let callsite = &globals.store[callid];
                let CallSiteInfo {
                    args,
                    pos_num,
                    ret,
                    block_fid,
                    ..
                } = *callsite;
                let kw_len = callsite.kw_args.len();
                let op1 = format!(
                    "{} = super({}{}){}",
                    ret_str(ret),
                    if pos_num == 0 {
                        "".to_string()
                    } else {
                        format!("{:?};{}", args, pos_num)
                    },
                    if kw_len == 0 {
                        "".to_string()
                    } else {
                        format!(" kw:{:?};{}", args + pos_num as u16, kw_len)
                    },
                    if let Some(block_fid) = block_fid {
                        format!(" {{ {:?} }}", block_fid)
                    } else {
                        "".to_string()
                    },
                );
                format!("{:36} [{}]", op1, class.get_name(globals))
            }
            TraceIr::InlineCall {
                inline_id,
                callsite,
                class,
                ..
            } => {
                let CallSiteInfo {
                    recv,
                    args,
                    pos_num,
                    ret,
                    ..
                } = globals.store[callsite];
                let name = &globals.store.get_inline_info(inline_id).2;
                let op1 = if pos_num == 0 {
                    format!("{} = {:?}.inline {name}()", ret_str(ret), recv,)
                } else {
                    format!(
                        "{} = {:?}.inline {name}({:?}; {})",
                        ret_str(ret),
                        recv,
                        args,
                        pos_num,
                    )
                };
                format!("{:36} [{}]", op1, class.get_name(globals))
            }
            TraceIr::Yield {
                ret,
                args,
                len,
                callid: _,
            } => {
                if len == 0 {
                    format!("{} = yield", ret_str(ret))
                } else {
                    format!("{} = yield({:?}; {})", ret_str(ret), args, len)
                }
            }
            TraceIr::MethodArgs => return None,
            TraceIr::MethodDef { name, func_id } => {
                format!("method_def {name}: {:?}", func_id)
            }
            TraceIr::SingletonMethodDef { obj, name, func_id } => {
                format!("singleton_method_def {:?}.{name}: {:?}", obj, func_id)
            }
            TraceIr::ClassDef {
                ret,
                superclass,
                name,
                func_id,
            } => {
                format!(
                    "{} = class_def {name} < {:?}: {:?}",
                    ret_str(ret),
                    superclass,
                    func_id
                )
            }
            TraceIr::ModuleDef { ret, name, func_id } => {
                format!("{} = module_def {name}: {:?}", ret_str(ret), func_id)
            }
            TraceIr::SingletonClassDef { ret, base, func_id } => {
                format!(
                    "{} = singleton_class_def << {:?}: {:?}",
                    ret_str(ret),
                    base,
                    func_id
                )
            }
            TraceIr::ConcatStr(ret, args, len) => {
                format!("{} = concat({:?}; {})", ret_str(ret), args, len)
            }
            TraceIr::ConcatRegexp(ret, args, len) => {
                format!("{} = concat_regexp({:?}; {})", ret_str(ret), args, len)
            }
            TraceIr::ExpandArray(src, dst, len) => {
                format!("{:?}; {} = expand({:?})", dst, len, src)
            }
            TraceIr::AliasMethod { new, old } => {
                format!("alias_method({:?}<-{:?})", new, old)
            }
            TraceIr::DefinedYield { ret } => format!("{:?} = defined?(yield)", ret),
            TraceIr::DefinedConst { ret, siteid } => {
                let ConstSiteInfo {
                    name,
                    prefix,
                    toplevel,
                    ..
                } = &globals.store[siteid];
                let mut const_name = if *toplevel { "::" } else { "" }.to_string();
                for c in prefix {
                    c.append_to(&mut const_name);
                    const_name += "::";
                }
                name.append_to(&mut const_name);
                format!("{:?} = defined?(constant) {const_name}", ret)
            }
            TraceIr::DefinedMethod { ret, recv, name } => {
                format!("{:?} = defined?(method) {:?}.{}", ret, recv, name)
            }
            TraceIr::DefinedGvar { ret, name } => {
                format!("{:?} = defined?(gvar) {}", ret, name)
            }
            TraceIr::DefinedIvar { ret, name } => {
                format!("{:?} = defined?(ivar) {}", ret, name)
            }
            TraceIr::LoopStart(count) => format!(
                "loop_start counter={} jit-addr={:016x}",
                count,
                self.into_jit_addr()
            ),
            TraceIr::LoopEnd => "loop_end".to_string(),
        };
        Some(s)
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

#[cfg(feature = "dump-bc")]
fn ret_str(slot: Option<SlotId>) -> String {
    match slot {
        None => "_".to_string(),
        Some(ret) => format!("{:?}", ret),
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

///
/// an index of bytecode instruction.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default, PartialOrd)]
pub(crate) struct BcIndex(pub u32);

impl std::fmt::Debug for BcIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{:05}", self.0)
    }
}

impl std::fmt::Display for BcIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{:05}", self.0)
    }
}

impl std::ops::Add<usize> for BcIndex {
    type Output = Self;
    fn add(self, rhs: usize) -> Self::Output {
        Self((self.0 as usize + rhs) as u32)
    }
}

impl std::ops::Add<i64> for BcIndex {
    type Output = Self;
    fn add(self, rhs: i64) -> Self::Output {
        Self((self.0 as i64 + rhs) as u32)
    }
}

impl std::ops::Add<i32> for BcIndex {
    type Output = Self;
    fn add(self, rhs: i32) -> Self::Output {
        Self((self.0 as i64 + rhs as i64) as u32)
    }
}

impl std::iter::Step for BcIndex {
    fn steps_between(start: &Self, end: &Self) -> Option<usize> {
        if start > end {
            None
        } else {
            Some((end.0 - start.0) as usize)
        }
    }

    fn forward_checked(start: Self, count: usize) -> Option<Self> {
        Some(start + count)
    }

    fn backward_checked(start: Self, count: usize) -> Option<Self> {
        if (start.0 as usize) < count {
            None
        } else {
            Some(BcIndex((start.0 as usize - count) as _))
        }
    }
}

impl BcIndex {
    pub(crate) fn from(i: usize) -> Self {
        Self(i as u32)
    }

    pub(crate) fn to_usize(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Visibility {
    Public = 0,
    Protected = 1,
    Private = 2,
}

impl Visibility {
    pub fn is_public(&self) -> bool {
        self == &Self::Public
    }

    pub fn is_private(&self) -> bool {
        self == &Self::Private
    }
}

extern "C" fn exec_jit_compile_patch(
    globals: &mut Globals,
    func_id: FuncId,
    self_value: Value,
    entry: DestLabel,
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
