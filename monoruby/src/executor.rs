use super::*;
use crate::bytecodegen::*;

mod constants;
pub mod frame;
pub mod inline;
pub mod op;
use crate::bytecodegen::inst::*;
use crate::compiler::jitgen::trace_ir::*;
pub use compiler::*;
use fancy_regex::Captures;
pub use frame::*;
pub use op::*;
use ruruby_parse::{CmpKind, Loc, SourceInfoRef};

pub type Result<T> = std::result::Result<T, MonorubyErr>;
pub type BuiltinFn = extern "C" fn(&mut Executor, &mut Globals, Lfp) -> Option<Value>;
pub type BinaryOpFn = extern "C" fn(&mut Executor, &mut Globals, Value, Value) -> Option<Value>;
pub type UnaryOpFn = extern "C" fn(&mut Executor, &mut Globals, Value) -> Option<Value>;

pub(crate) const RSP_STACK_LFP: i32 = 40 - LFP_OFFSET;
pub(crate) const RSP_CFP: i32 = 24;
pub(crate) const BP_CFP: i32 = 8;
pub(crate) const CFP_LFP: i32 = 8;

const LFP_OFFSET: i32 = 24;
pub(crate) const LFP_OUTER: i32 = 0 + LFP_OFFSET;
/// Meta 8bytes
pub(crate) const LFP_META: i32 = 8 + LFP_OFFSET;
/// Meta::Regnum 2bytes
pub(crate) const LFP_META_REGNUM: i32 = LFP_META - META_REGNUM as i32;
/// Meta::FuncId 4bytes
//pub(crate) const LBP_META_FUNCID: i64 = LBP_META + META_FUNCID as i64;
pub(crate) const LFP_BLOCK: i32 = 16 + LFP_OFFSET;
pub(crate) const LFP_SELF: i32 = 24 + LFP_OFFSET;
pub const LFP_ARG0: i32 = LFP_SELF + 8;

pub(crate) const EXECUTOR_CFP: i64 = std::mem::offset_of!(Executor, cfp) as _;
pub(crate) const EXECUTOR_RSP_SAVE: i64 = std::mem::offset_of!(Executor, rsp_save) as _;
pub(crate) const EXECUTOR_PARENT_FIBER: i64 = std::mem::offset_of!(Executor, parent_fiber) as _;

///
/// Bytecode interpreter.
///
#[derive(Debug)]
#[repr(C)]
pub struct Executor {
    /// control frame pointer.
    cfp: Option<Cfp>,
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
        let path = dirs::home_dir()
            .unwrap()
            .join(".monoruby")
            .join("startup.rb");
        if let Err(err) = executor.require(globals, &path, false) {
            err.show_error_message_and_all_loc(globals);
            panic!("error occurred in startup.");
        }
        #[cfg(feature = "emit-bc")]
        {
            globals.startup_flag = true;
        }
        executor
    }

    pub fn cfp(&self) -> Cfp {
        self.cfp.unwrap()
    }

    pub(crate) unsafe fn get_slot(&self, index: SlotId) -> Option<Value> {
        self.cfp().lfp().register(index.0 as usize)
    }

    pub(crate) fn method_func_id(&self) -> FuncId {
        self.cfp().method_func_id()
    }

    pub fn temp_len(&self) -> usize {
        self.temp_stack.len()
    }

    pub fn temp_push(&mut self, val: Value) {
        self.temp_stack.push(val);
    }

    pub fn temp_clear(&mut self, len: usize) {
        self.temp_stack.truncate(len);
    }

    pub fn temp_array_new(&mut self, size_hint: impl Into<Option<usize>>) {
        let size_hint = size_hint.into();
        if let Some(size_hint) = size_hint {
            self.temp_stack.push(Value::array_with_capacity(size_hint));
        } else {
            self.temp_stack.push(Value::array_empty());
        }
    }

    pub fn temp_pop(&mut self) -> Value {
        self.temp_stack.pop().unwrap()
    }

    pub fn temp_array_push(&mut self, v: Value) {
        self.temp_stack.last_mut().unwrap().as_array().push(v);
    }

    pub fn temp_array_extend_from_slice(&mut self, slice: &[Value]) {
        self.temp_stack
            .last_mut()
            .unwrap()
            .as_array()
            .extend_from_slice(slice);
    }

    pub fn parent_fiber(&self) -> Option<std::ptr::NonNull<Executor>> {
        self.parent_fiber
    }

    pub fn sp_last_match(&self) -> Value {
        self.sp_last_match.unwrap_or_default()
    }

    pub fn sp_post_match(&self) -> Value {
        self.sp_post_match.unwrap_or_default()
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
            Ok(res) => bytecodegen::compile_script(globals, res),
            Err(err) => Err(MonorubyErr::parse(err)),
        }?;
        self.eval_toplevel(globals, fid)
    }

    ///
    /// Execute top level method.
    ///
    /// *main* object is set to *self*.
    ///
    pub fn eval_toplevel(&mut self, globals: &mut Globals, func_id: FuncId) -> Result<Value> {
        #[cfg(feature = "emit-bc")]
        globals.dump_bc();

        let main_object = globals.main_object;
        let res = (globals.codegen.method_invoker)(
            self,
            globals,
            func_id,
            main_object,
            (&[]).as_ptr(),
            0,
            None,
        );
        res.ok_or_else(|| self.take_error())
    }

    ///
    /// Execute top level method.
    ///
    /// *main* object is set to *self*.
    ///
    pub fn invoke_binding(&mut self, globals: &mut Globals, binding_lfp: Lfp) -> Result<Value> {
        #[cfg(feature = "emit-bc")]
        globals.dump_bc();

        let res = (globals.codegen.binding_invoker)(self, globals, binding_lfp);
        res.ok_or_else(|| self.take_error())
    }

    pub fn require(
        &mut self,
        globals: &mut Globals,
        file_name: &std::path::Path,
        is_relative: bool,
    ) -> Result<bool> {
        if let Some((file_body, path)) = globals.load_lib(&file_name, is_relative)? {
            #[cfg(feature = "require")]
            {
                eprintln!("require: {:?}", &path);
            }
            self.enter_class_context();
            let res = self.exec_script(globals, file_body, &path);
            self.exit_class_context();
            res?;
            Ok(true)
        } else {
            Ok(false)
        }
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

    pub(crate) fn exception(&self) -> Option<&MonorubyErr> {
        self.exception.as_ref()
    }

    pub(crate) fn take_ex_obj(&mut self, globals: &mut Globals) -> Value {
        let err = self.take_error();

        let name = err.get_class_name();
        let class_id = self
            .get_constant(globals, OBJECT_CLASS, IdentId::get_id(name))
            .unwrap()
            .expect(name)
            .as_class_id();

        if let MonorubyErrKind::Load(path) = &err.kind() {
            let path = Value::string_from_str(path.as_os_str().to_str().unwrap());
            let v = Value::new_exception_from_err(globals, err, class_id);
            globals.set_ivar(v, IdentId::get_id("/path"), path).unwrap();
            v
        } else {
            Value::new_exception_from_err(globals, err, class_id)
        }
    }

    pub(crate) fn err_divide_by_zero(&mut self) {
        self.set_error(MonorubyErr::divide_by_zero());
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
    /// It is necessary to check the base class for keeping cache consistency.
    ///
    pub(crate) fn find_constant(
        &mut self,
        globals: &mut Globals,
        site_id: ConstSiteId,
    ) -> Result<(Value, Option<Value>)> {
        let base = globals.store[site_id]
            .base
            .map(|base| unsafe { self.get_slot(base) }.unwrap());
        let current_func = self.method_func_id();
        let ConstSiteInfo {
            name,
            toplevel,
            mut prefix,
            ..
        } = globals.store[site_id].clone();
        let mut parent = if let Some(base) = base {
            base.expect_class_or_module(globals)?.id()
        } else if toplevel {
            OBJECT_CLASS
        } else if prefix.is_empty() {
            let v = self.search_constant_checked(globals, name, current_func)?;
            return Ok((v, None));
        } else {
            let parent = prefix.remove(0);
            self.search_constant_checked(globals, parent, current_func)?
                .expect_class_or_module(globals)?
                .id()
        };
        for constant in prefix {
            parent = self
                .get_constant_checked(globals, parent, constant)?
                .expect_class_or_module(globals)?
                .id();
        }
        let v = self.get_constant_checked(globals, parent, name)?;
        Ok((v, base))
    }

    pub(crate) fn set_constant(
        &mut self,
        globals: &mut Globals,
        site_id: ConstSiteId,
        val: Value,
    ) -> Result<()> {
        let ConstSiteInfo {
            toplevel,
            base,
            mut prefix,
            name,
            ..
        } = globals.store[site_id].clone();
        let mut parent = if let Some(base) = base {
            let base = unsafe { self.get_slot(base) }.unwrap();
            base.expect_class_or_module(globals)?.id()
        } else if toplevel {
            OBJECT_CLASS
        } else if prefix.is_empty() {
            self.context_class_id()
        } else {
            let parent = prefix.remove(0);
            let current_func = self.method_func_id();
            self.search_constant_checked(globals, parent, current_func)?
                .expect_class_or_module(globals)?
                .id()
        };
        for constant in prefix {
            parent = self
                .get_constant_checked(globals, parent, constant)?
                .expect_class_or_module(globals)?
                .id();
        }
        globals.set_constant(parent, name, val);
        Ok(())
    }

    ///
    /// Find and return a class variable with `name`.
    ///
    pub(crate) fn find_class_variable(
        &self,
        globals: &mut Globals,
        name: IdentId,
    ) -> Result<Value> {
        let parent = self.get_parent(globals)?;
        globals.get_class_variable(parent, name).map(|(_, v)| v)
    }

    ///
    /// Set a class variable with `name` to `value`.
    ///
    pub(crate) fn set_class_variable(
        &self,
        globals: &mut Globals,
        name: IdentId,
        val: Value,
    ) -> Result<()> {
        let parent = self.get_parent(globals)?;
        let parent = match globals.search_class_variables_superclass(parent, name) {
            Some((module, _)) => module,
            None => parent,
        };
        globals.set_class_variable(parent.id(), name, val);
        Ok(())
    }

    ///
    /// Get the parent module of the current context.
    ///
    fn get_parent(&self, globals: &Globals) -> Result<Module> {
        let fid = self.cfp().method_func_id();
        let parent = globals.store[fid].as_ruby_func().lexical_context.last();
        match parent {
            Some(parent) => Ok(*parent),
            None => Err(MonorubyErr::runtimeerr(
                "class variable access from toplevel",
            )),
        }
    }
}

impl Executor {
    pub(crate) fn define_method(
        &mut self,
        globals: &mut Globals,
        name: IdentId,
        func: FuncId,
    ) -> Option<Value> {
        let Cref {
            class_id,
            module_function,
            visibility,
        } = self.get_class_context();
        let current_func = self.method_func_id();
        if globals[func].is_ruby_func().is_some() {
            globals[func].as_ruby_func_mut().lexical_context =
                globals[current_func].as_ruby_func().lexical_context.clone();
            globals.add_method(class_id, name, func, visibility);
            if module_function {
                globals.add_singleton_method(class_id, name, func, visibility);
            }
            globals.class_version_inc();
            if globals.codegen.bop_redefine_flags() != 0 {
                self.immediate_eviction(globals);
            }
            Some(Value::nil())
        } else {
            let err = MonorubyErr::internalerr(format!(
                "define func: {:?} {:016x}",
                name,
                (func.get() as u64) + ((name.get() as u64) << 32)
            ));
            runtime::_dump_stacktrace(self, globals);
            self.set_error(err);
            None
        }
    }

    fn immediate_eviction(&mut self, globals: &mut Globals) {
        let mut cfp = self.cfp();
        let mut return_addr = unsafe { cfp.return_addr() };
        while let Some(prev_cfp) = cfp.prev() {
            let ret = return_addr.unwrap();
            if !globals.codegen.check_vm_address(ret) {
                if let Some((patch_point, deopt)) = globals.codegen.get_deopt_with_return_addr(ret)
                {
                    let patch_point = patch_point.unwrap();
                    globals
                        .codegen
                        .jit
                        .apply_jmp_patch_address(patch_point, deopt);
                    unsafe { patch_point.as_ptr().write(0xe9) };
                }
            }
            cfp = prev_cfp;
            return_addr = unsafe { cfp.return_addr() };
        }
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
        bh: Option<BlockHandler>,
    ) -> Option<Value> {
        let func_id = match globals.find_method(receiver, method, false) {
            Ok(id) => id,
            Err(err) => {
                self.set_error(err);
                return None;
            }
        };
        self.invoke_func(globals, func_id, receiver, args, bh)
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
        bh: Option<BlockHandler>,
    ) -> Result<Value> {
        let func_id = globals.find_method(receiver, method, true)?;
        self.invoke_func(globals, func_id, receiver, args, bh)
            .ok_or_else(|| self.take_error())
    }

    pub(crate) fn invoke_tos(&mut self, globals: &mut Globals, receiver: Value) -> Value {
        match receiver.unpack() {
            RV::Object(_) => {}
            _ => return Value::string(receiver.to_s(globals)),
        }
        let func_id = globals.find_method(receiver, IdentId::TO_S, true).unwrap();
        self.invoke_func(globals, func_id, receiver, &[], None)
            .unwrap()
    }

    ///
    /// Invoke block for *block_handler*.
    ///
    /// To get BlockData, use get_block_data().
    ///  
    /// let data = vm.get_block_data(globals, block);
    ///
    pub(crate) fn invoke_block(
        &mut self,
        globals: &mut Globals,
        data: &ProcInner,
        args: &[Value],
    ) -> Result<Value> {
        (globals.codegen.block_invoker)(
            self,
            globals,
            data,
            Value::nil(),
            args.as_ptr(),
            args.len(),
        )
        .ok_or_else(|| self.take_error())
    }

    pub(crate) fn invoke_block_with_self(
        &mut self,
        globals: &mut Globals,
        data: &ProcInner,
        self_val: Value,
        args: &[Value],
    ) -> Result<Value> {
        (globals.codegen.block_invoker_with_self)(
            self,
            globals,
            data as _,
            self_val,
            args.as_ptr(),
            args.len(),
        )
        .ok_or_else(|| self.take_error())
    }

    pub(crate) fn invoke_block_once(
        &mut self,
        globals: &mut Globals,
        bh: BlockHandler,
        args: &[Value],
    ) -> Result<Value> {
        let data = self.get_block_data(globals, bh)?;
        self.invoke_block(globals, &data, args)
    }

    pub(crate) fn invoke_block_iter1(
        &mut self,
        globals: &mut Globals,
        bh: BlockHandler,
        iter: impl Iterator<Item = Value>,
    ) -> Result<()> {
        let data = self.get_block_data(globals, bh)?;
        for val in iter {
            self.invoke_block(globals, &data, &[val])?;
        }
        Ok(())
    }

    pub(crate) fn invoke_block_iter_with_index1(
        &mut self,
        globals: &mut Globals,
        bh: BlockHandler,
        iter: impl Iterator<Item = Value>,
    ) -> Result<()> {
        let data = self.get_block_data(globals, bh)?;
        for (index, val) in iter.enumerate() {
            self.invoke_block(globals, &data, &[val, Value::integer(index as i64)])?;
        }
        Ok(())
    }

    pub(crate) fn invoke_block_map1(
        &mut self,
        globals: &mut Globals,
        bh: BlockHandler,
        iter: impl Iterator<Item = Value>,
        size_hint: impl Into<Option<usize>>,
    ) -> Result<Value> {
        let data = self.get_block_data(globals, bh)?;
        let old = alloc::Allocator::<RValue>::set_enabled(false);
        self.temp_array_new(size_hint);
        for v in iter {
            let res = self.invoke_block(globals, &data, &[v])?;
            self.temp_array_push(res);
        }
        alloc::Allocator::<RValue>::set_enabled(old);
        let v = self.temp_pop();
        Ok(v)
    }

    pub(crate) fn invoke_block_flat_map1(
        &mut self,
        globals: &mut Globals,
        bh: BlockHandler,
        iter: impl Iterator<Item = Value>,
        size_hint: impl Into<Option<usize>>,
    ) -> Result<Value> {
        let data = self.get_block_data(globals, bh)?;
        let old = alloc::Allocator::<RValue>::set_enabled(false);
        self.temp_array_new(size_hint);
        for v in iter {
            let res = self.invoke_block(globals, &data, &[v])?;
            if let Some(ary) = res.try_array_ty() {
                self.temp_array_extend_from_slice(&ary);
            } else {
                self.temp_array_push(res);
            }
        }
        alloc::Allocator::<RValue>::set_enabled(old);
        let v = self.temp_pop();
        Ok(v)
    }

    pub(crate) fn invoke_block_fold1(
        &mut self,
        globals: &mut Globals,
        bh: BlockHandler,
        iter: impl Iterator<Item = Value>,
        mut res: Value,
    ) -> Result<Value> {
        let data = self.get_block_data(globals, bh)?;
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
        proc: Proc,
        args: &[Value],
    ) -> Result<Value> {
        //if globals.store[proc.func_id()].is_block_style() {
        (globals.codegen.block_invoker)(
            self,
            globals,
            &proc,
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
        args: &[Value],
        bh: Option<BlockHandler>,
    ) -> Result<Value> {
        if let Some(func_id) = globals.check_method(receiver, method) {
            self.invoke_func(globals, func_id, receiver, args, bh)
                .ok_or_else(|| self.take_error())
        } else {
            Ok(Value::nil())
        }
    }

    ///
    /// Invoke func with *args*: Args.
    ///
    pub(crate) fn invoke_func(
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

    pub(crate) fn invoke_func_inner(
        &mut self,
        globals: &mut Globals,
        func_id: FuncId,
        receiver: Value,
        args: &[Value],
        bh: Option<BlockHandler>,
    ) -> Result<Value> {
        self.invoke_func(globals, func_id, receiver, args, bh)
            .ok_or_else(|| self.take_error())
    }
}

impl Executor {
    ///
    /// Generate *ProcInner* from 'bh'.
    ///
    pub(crate) fn get_block_data(
        &mut self,
        globals: &mut Globals,
        bh: BlockHandler,
    ) -> Result<ProcInner> {
        let mut cfp = self.cfp();
        if let Some((func_id, idx)) = bh.try_proxy() {
            for _ in 0..idx {
                cfp = cfp.prev().unwrap();
            }
            Ok(ProcInner::from(cfp.lfp(), func_id))
        } else if let Some(proc) = bh.try_proc() {
            Ok(proc.clone())
        } else {
            self.to_proc(globals, bh.get())
        }
    }

    pub fn to_s(&mut self, globals: &mut Globals, receiver: Value) -> Result<String> {
        self.invoke_method_inner(globals, IdentId::TO_S, receiver, &[], None)?
            .expect_string()
    }

    fn to_proc(&mut self, globals: &mut Globals, val: Value) -> Result<ProcInner> {
        match self.invoke_method_inner(globals, IdentId::TO_PROC, val, &[], None) {
            Ok(proc) => {
                if let Some(proc) = proc.is_proc() {
                    return Ok(proc.clone());
                }
            }
            _ => {}
        };
        Err(MonorubyErr::typeerr(
            "",
            TypeErrKind::WrongArgumentType {
                val,
                expected: "Proc",
            },
        ))
    }

    pub(crate) fn define_class(
        &mut self,
        globals: &mut Globals,
        base: Option<Value>,
        name: IdentId,
        superclass: Option<Value>,
        is_module: u32,
    ) -> Result<Value> {
        let parent = match base {
            Some(base) => base.expect_class_or_module(globals)?.id(),
            None => self.context_class_id(),
        };
        let self_val = match self.get_constant(globals, parent, name)? {
            Some(val) => {
                let val = val.expect_class_or_module(globals)?;
                if let Some(superclass) = superclass {
                    assert!(is_module != 1);
                    let superclass_id = superclass.expect_class(globals)?.id();
                    if Some(superclass_id) != val.get_real_superclass().map(|m| m.id()) {
                        return Err(MonorubyErr::superclass_mismatch(name));
                    }
                }
                val
            }
            None => {
                let superclass = match superclass {
                    Some(superclass) => {
                        assert!(is_module != 1);
                        superclass.expect_class(globals)?
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
            let proc = Proc::from(self.get_block_data(globals, bh)?);
            Ok(proc)
        } else if bh.try_proc().is_some() {
            Ok(Proc::new(bh.0))
        } else {
            unimplemented!()
        }
    }

    pub fn generate_lambda(&mut self, func_id: FuncId) -> Proc {
        let outer_lfp = self.cfp().lfp();
        outer_lfp.move_frame_to_heap();
        Proc::from(ProcInner::from(outer_lfp, func_id))
    }

    pub fn generate_binding(&mut self) -> Binding {
        let lfp = self.cfp().prev().unwrap().lfp();
        Binding::from_outer(lfp)
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
        method: IdentId,
        obj: Value,
        args: Vec<Value>,
    ) -> Result<Value> {
        let outer_lfp = Lfp::dummy_heap_frame_with_self(obj);
        let proc = Proc::from_parts(outer_lfp, FuncId::new(1));
        let e = Value::new_enumerator(obj, method, proc, args);
        Ok(e)
    }
}

// Handling special variables.

impl Executor {
    pub(crate) fn clear_capture_special_variables(&mut self) {
        self.sp_last_match = None;
        self.sp_post_match = None;
        self.sp_matches.clear();
    }
    ///
    /// Save captured strings to special variables.
    ///
    /// - $n (n:0,1,2,3...) <- The string which matched with nth parenthesis in the last successful match.
    ///
    /// - $& <- The string which matched successfully at last.
    ///
    /// - $' <- The string after $&.
    ///
    pub(crate) fn save_capture_special_variables(&mut self, captures: &Captures, given: &str) {
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
pub struct BlockHandler(Value);

impl std::fmt::Debug for BlockHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

    pub fn get(self) -> Value {
        self.0
    }

    pub fn func_id(&self) -> FuncId {
        if let Some((fid, _)) = self.try_proxy() {
            fid
        } else if let Some(proc) = self.try_proc() {
            proc.func_id()
        } else {
            unimplemented!()
        }
    }

    pub fn from_caller(func_id: FuncId) -> Self {
        let block_handler = ((u32::from(func_id) as i64) << 16) + 1;
        let bh = Value::integer(block_handler);
        Self::new(bh)
    }

    pub fn from_current(func_id: FuncId) -> Self {
        let block_handler = (u32::from(func_id) as i64) << 16;
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

    pub(crate) fn id(&self) -> u64 {
        self.0.id()
    }
}

#[derive(Debug, Clone)]
struct Cref {
    pub(crate) class_id: ClassId,
    pub(crate) module_function: bool,
    pub(crate) visibility: Visibility,
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
        self.classid1() == Some(INTEGER_CLASS)
    }

    pub fn is_integer2(&self) -> bool {
        self.classid2() == Some(INTEGER_CLASS)
    }

    pub fn is_float1(&self) -> bool {
        self.classid1() == Some(FLOAT_CLASS)
    }

    pub fn is_float2(&self) -> bool {
        self.classid2() == Some(FLOAT_CLASS)
    }

    pub fn is_integer_binop(&self) -> bool {
        self.classid1() == Some(INTEGER_CLASS) && self.classid2() == Some(INTEGER_CLASS)
    }

    pub fn is_float_binop(&self) -> bool {
        match (self.classid1(), self.classid2()) {
            (Some(class1), Some(class2)) => match (class1, class2) {
                (INTEGER_CLASS, INTEGER_CLASS) => false,
                (INTEGER_CLASS | FLOAT_CLASS, INTEGER_CLASS | FLOAT_CLASS) => true,
                _ => false,
            },
            _ => false,
        }
    }
    pub fn classid1(&self) -> Option<ClassId> {
        ClassId::from(self.op2.0 as u32)
    }

    pub fn classid2(&self) -> Option<ClassId> {
        ClassId::from((self.op2.0 >> 32) as u32)
    }

    pub fn cached_version(&self) -> u32 {
        let op = self.op2.0;
        (op >> 32) as u32
    }

    pub fn cached_ivarid(&self) -> IvarId {
        let op = self.op2.0;
        IvarId::new((op >> 32) as u32)
    }

    fn fid(&self) -> Option<FuncId> {
        let op = self.op2.0 as u32;
        if op == 0 {
            None
        } else {
            Some(FuncId::new(op))
        }
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
            req_num,
            info,
            ..
        } = fn_info;
        Bc::from_with_num(op1, *req_num as u16, 0, *info as u16, *arg_num as u16)
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

    pub(super) fn from_with_class_and_version(
        op1: u64,
        class_id: Option<ClassId>,
        version: u32,
    ) -> Self {
        Self {
            op1,
            op2: Bc2::class_and_version(class_id, version),
        }
    }

    pub(super) fn from_with_class2(op1: u64) -> Self {
        Self {
            op1,
            op2: Bc2::class2(None, None),
        }
    }

    pub(super) fn from_with_ident2(op1: u64, id1: IdentId, id2: IdentId) -> Self {
        Self {
            op1,
            op2: Bc2::ident2(id1, id2),
        }
    }

    pub(super) fn from_with_num(op1: u64, num0: u16, num1: u16, num2: u16, num3: u16) -> Self {
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

    pub(crate) fn as_ptr(&self) -> *mut Bc {
        self.0.as_ptr()
    }

    pub(crate) fn opcode(&self) -> u8 {
        (self.op1 >> 48) as u8
    }

    /*pub(crate) fn opcode_sub(&self) -> u8 {
        (self.op1 >> 56) as u8
    }*/

    pub(crate) fn is_loop_start(&self) -> bool {
        self.opcode() == 14 // TraceIr::LoopStart(_))
    }

    pub fn from(bc: &Bc) -> Self {
        Self(std::ptr::NonNull::from(bc))
    }

    pub fn u64(self) -> u64 {
        self.0.as_ptr() as _
    }

    fn write2(self, data: u64) {
        unsafe { *((self.as_ptr() as *mut u64).add(1)) = data }
    }

    pub(crate) fn cached_fid(self) -> Option<FuncId> {
        (*self).fid()
    }

    pub(crate) fn cached_class1(self) -> Option<ClassId> {
        (*(self + 1)).classid1()
    }

    pub(crate) fn cached_class0(self) -> Option<ClassId> {
        self.classid1()
    }
}

impl BcPc {
    pub(crate) fn trace_ir(&self, store: &Store) -> TraceIr {
        let op = self.op1;
        let opcode = self.opcode();
        if opcode & 0xc0 == 0 {
            let (op1, op2) = dec_wl(op);
            match opcode {
                1 => TraceIr::SingletonMethodDef {
                    obj: SlotId::new(op1),
                    name: IdentId::from(self.op2.0 as u32),
                    func_id: FuncId::new((self.op2.0 >> 32) as u32),
                },
                2 => TraceIr::MethodDef {
                    name: IdentId::from((self.op2.0) as u32),
                    func_id: FuncId::new((self.op2.0 >> 32) as u32),
                },
                3 => TraceIr::Br(op2 as i32),
                4 => TraceIr::CondBr(SlotId::new(op1), op2 as i32, false, BrKind::BrIf),
                5 => TraceIr::CondBr(SlotId::new(op1), op2 as i32, false, BrKind::BrIfNot),
                6 => TraceIr::Integer(SlotId::new(op1), op2 as i32),
                7 => TraceIr::Literal(SlotId::new(op1), self.op2.get_value()),
                8 => TraceIr::Nil(SlotId::new(op1)),
                9 => TraceIr::Symbol(SlotId::new(op1), IdentId::from(op2)),
                10 | 18 => TraceIr::LoadConst(SlotId::new(op1), ConstSiteId(op2)),
                11 => TraceIr::StoreConst(SlotId::new(op1), ConstSiteId(op2)),
                12..=13 => TraceIr::CondBr(
                    SlotId::new(op1),
                    op2 as i32,
                    true,
                    BrKind::from(opcode - 12),
                ),
                14 => TraceIr::LoopStart(op2),
                15 => TraceIr::LoopEnd,
                16 => {
                    let class = self.cached_class0();
                    let ivar = self.cached_ivarid();
                    TraceIr::LoadIvar(SlotId::new(op1), IdentId::from(op2), class, ivar)
                }
                17 => {
                    let class = self.cached_class0();
                    let ivar = self.cached_ivarid();
                    TraceIr::StoreIvar(SlotId::new(op1), IdentId::from(op2), class, ivar)
                }
                20 => TraceIr::CheckLocal(SlotId::new(op1), op2 as i32),
                21 => TraceIr::BlockArgProxy(SlotId::new(op1), op2 as usize),
                22 => TraceIr::SingletonClassDef {
                    dst: SlotId::from(op1),
                    base: SlotId::new(op2 as u16),
                    func_id: FuncId::new((self.op2.0 >> 32) as u32),
                },
                23 => TraceIr::BlockArg(SlotId::new(op1), op2 as usize),
                24 => TraceIr::CheckCvar {
                    dst: SlotId::new(op1),
                    name: IdentId::from(op2),
                },
                25 => TraceIr::LoadGvar {
                    dst: SlotId::new(op1),
                    name: IdentId::from(op2),
                },
                26 => TraceIr::StoreGvar {
                    src: SlotId::new(op1),
                    name: IdentId::from(op2),
                },
                27 => TraceIr::LoadCvar {
                    dst: SlotId::new(op1),
                    name: IdentId::from(op2),
                },
                28 => TraceIr::LoadSvar {
                    dst: SlotId::new(op1),
                    id: op2,
                },
                29 => TraceIr::StoreCvar {
                    src: SlotId::new(op1),
                    name: IdentId::from(op2),
                },
                30..=31 => {
                    let cached_fid = self.cached_fid();
                    let is_simple = opcode == 30;
                    let callid: CallSiteId = op2.into();

                    if let Some(fid) = cached_fid
                        && let Some(inline_id) =
                            crate::executor::inline::InlineTable::get_inline(fid)
                    {
                        if fid == OBJECT_SEND_FUNCID {
                            if store[callid].splat_pos.len() == 1
                                && store[callid].pos_num == 1
                                && !store[callid].kw_may_exists()
                            {
                                return TraceIr::InlineObjectSendSplat { inline_id, callid };
                            } else if is_simple {
                                return TraceIr::InlineObjectSend { inline_id, callid };
                            }
                        } else if is_simple {
                            return TraceIr::InlineCall { inline_id, callid };
                        }
                    }
                    TraceIr::MethodCall { callid }
                }
                32..=33 => TraceIr::MethodCallBlock { callid: op2.into() },
                34..=35 => TraceIr::Yield { callid: op2.into() },
                36 => TraceIr::OptCase {
                    cond: SlotId::new(op1),
                    optid: OptCaseId::from(op2),
                },
                37 => TraceIr::NilBr(SlotId::new(op1), op2 as i32),
                38 => TraceIr::Lambda {
                    dst: SlotId::new(op1),
                    func_id: FuncId::new(op2),
                },
                39 => TraceIr::Array {
                    dst: SlotId::new(op1),
                    callid: CallSiteId::from(op2),
                },
                _ => unreachable!("{:016x}", op),
            }
        } else {
            let (op1, op2, op3) = dec_www(op);
            match opcode {
                64 => TraceIr::DefinedYield {
                    dst: SlotId::new(op1),
                },
                65 => TraceIr::DefinedConst {
                    dst: SlotId::new(op1),
                    siteid: ConstSiteId(self.op2.0 as u32),
                },
                66 => TraceIr::DefinedMethod {
                    dst: SlotId::new(op1),
                    recv: SlotId::new(op2),
                    name: IdentId::from(self.op2.0 as u32),
                },
                67 => TraceIr::DefinedGvar {
                    dst: SlotId::new(op1),
                    name: IdentId::from(self.op2.0 as u32),
                },
                68 => TraceIr::DefinedIvar {
                    dst: SlotId::new(op1),
                    name: IdentId::from(self.op2.0 as u32),
                },
                70 => TraceIr::ClassDef {
                    dst: SlotId::from(op1),
                    base: SlotId::from(op2),
                    superclass: SlotId::from(op3),
                    name: IdentId::from((self.op2.0) as u32),
                    func_id: FuncId::new((self.op2.0 >> 32) as u32),
                },
                71 => TraceIr::ModuleDef {
                    dst: SlotId::from(op1),
                    base: SlotId::from(op2),
                    name: IdentId::from((self.op2.0) as u32),
                    func_id: FuncId::new((self.op2.0 >> 32) as u32),
                },
                80 => TraceIr::Ret(SlotId::new(op1)),
                81 => TraceIr::MethodRet(SlotId::new(op1)),
                82 => TraceIr::Break(SlotId::new(op1)),
                83 => TraceIr::Raise(SlotId::new(op1)),
                85 => TraceIr::EnsureEnd,
                86 => TraceIr::ConcatRegexp(SlotId::from(op1), SlotId::new(op2), op3),
                126 => TraceIr::UnOp {
                    kind: UnOpK::Pos,
                    dst: SlotId::new(op1),
                    src: SlotId::new(op2),
                },
                127 => TraceIr::BitNot {
                    dst: SlotId::new(op1),
                    src: SlotId::new(op2),
                },
                128 => TraceIr::Not {
                    dst: SlotId::new(op1),
                    src: SlotId::new(op2),
                },
                129 => TraceIr::UnOp {
                    kind: UnOpK::Neg,
                    dst: SlotId::new(op1),
                    src: SlotId::new(op2),
                },
                130 => TraceIr::InlineCache,
                132 => TraceIr::Index {
                    dst: SlotId::new(op1),
                    base: SlotId::new(op2),
                    idx: SlotId::new(op3),
                },
                133 => TraceIr::IndexAssign {
                    src: SlotId::new(op1),
                    base: SlotId::new(op2),
                    idx: SlotId::new(op3),
                },
                134..=141 => TraceIr::Cmp(
                    CmpKind::from(opcode - 134),
                    SlotId::from(op1),
                    OpMode::RR(SlotId::new(op2), SlotId::new(op3)),
                    false,
                ),
                142..=149 => TraceIr::Cmp(
                    CmpKind::from(opcode - 142),
                    SlotId::from(op1),
                    OpMode::RI(SlotId::new(op2), op3 as i16),
                    false,
                ),
                150 => TraceIr::LoadDynVar(
                    SlotId::new(op1),
                    DynVar {
                        reg: SlotId::new(op2),
                        outer: op3 as usize,
                    },
                ),
                151 => TraceIr::StoreDynVar(
                    DynVar {
                        reg: SlotId::new(op1),
                        outer: op2 as usize,
                    },
                    SlotId::new(op3),
                ),
                154..=161 => TraceIr::Cmp(
                    CmpKind::from(opcode - 154),
                    SlotId::from(op1),
                    OpMode::RR(SlotId(op2), SlotId(op3)),
                    true,
                ),
                162..=169 => TraceIr::Cmp(
                    CmpKind::from(opcode - 162),
                    SlotId::from(op1),
                    OpMode::RI(SlotId::new(op2), op3 as i16),
                    true,
                ),
                170 | 172 => TraceIr::InitMethod(FnInitInfo {
                    reg_num: op1 as usize,
                    arg_num: self.u16(3) as usize,
                    reqopt_num: op2 as usize,
                    req_num: self.u16(0) as usize,
                    info: self.u16(2) as usize,
                    stack_offset: op3 as usize,
                }),
                171 => TraceIr::ExpandArray(SlotId::new(op1), SlotId::new(op2), op3),
                173 => {
                    let (new, old) = self.op2.get_ident2();
                    TraceIr::AliasMethod { new, old }
                }
                174 => TraceIr::Hash {
                    dst: SlotId::new(op1),
                    args: SlotId::new(op2),
                    len: op3,
                },
                176 => TraceIr::Mov(SlotId::new(op1), SlotId::new(op2)),
                177..=178 => TraceIr::Range {
                    dst: SlotId::new(op1),
                    start: SlotId::new(op2),
                    end: SlotId::new(op3),
                    exclude_end: match opcode - 177 {
                        0 => false,
                        1 => true,
                        _ => unreachable!(),
                    },
                },
                179 => TraceIr::ConcatStr(SlotId::from(op1), SlotId::new(op2), op3),
                180..=189 => {
                    let kind = BinOpK::from(opcode - 180);
                    let dst = SlotId::from(op1);
                    let mode = OpMode::IR(op2 as i16, SlotId::new(op3));
                    if self.is_integer2() {
                        TraceIr::IBinOp { kind, dst, mode }
                    } else if self.is_float2() {
                        TraceIr::FBinOp { kind, dst, mode }
                    } else {
                        TraceIr::BinOp { kind, dst, mode }
                    }
                }
                190..=199 => {
                    let kind = BinOpK::from(opcode - 190);
                    let dst = SlotId::from(op1);
                    let mode = OpMode::RI(SlotId::new(op2), op3 as i16);
                    if self.is_integer1() {
                        TraceIr::IBinOp { kind, dst, mode }
                    } else if self.is_float1() {
                        TraceIr::FBinOp { kind, dst, mode }
                    } else {
                        TraceIr::BinOp { kind, dst, mode }
                    }
                }
                200..=209 => {
                    let kind = BinOpK::from(opcode - 200);
                    let dst = SlotId::from(op1);
                    let mode = OpMode::RR(SlotId::new(op2), SlotId::new(op3));
                    if self.is_integer_binop() {
                        TraceIr::IBinOp { kind, dst, mode }
                    } else if self.is_float_binop() {
                        TraceIr::FBinOp { kind, dst, mode }
                    } else {
                        TraceIr::BinOp { kind, dst, mode }
                    }
                }
                _ => unreachable!("{:016x}", op),
            }
        }
    }
}

fn dec_wl(op: u64) -> (u16, u32) {
    ((op >> 32) as u16, op as u32)
}

fn dec_www(op: u64) -> (u16, u16, u16) {
    ((op >> 32) as u16, (op >> 16) as u16, op as u16)
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
        if start.0 as usize + count <= std::u16::MAX as usize {
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

    pub fn is_self(&self) -> bool {
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

impl std::ops::Add<usize> for SlotId {
    type Output = Self;
    fn add(self, rhs: usize) -> Self {
        Self(self.0 + rhs as u16)
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

pub(crate) extern "C" fn exec_jit_compile_patch(
    globals: &mut Globals,
    lfp: Lfp,
    entry_patch_point: monoasm::DestLabel,
) {
    let patch_point = globals.codegen.jit.label();
    let jit_entry = globals.codegen.jit.label();
    let guard = globals.codegen.jit.label();
    let func_id = lfp.meta().func_id();
    let self_value = lfp.self_val();
    let self_class = self_value.class();
    //eprintln!("{:?}", lfp.outermost().block());
    globals
        .codegen
        .class_guard_stub(self_class, patch_point, jit_entry, guard);

    let old_entry = globals[func_id].add_jit_code(self_class, patch_point);
    assert!(old_entry.is_none());
    globals.exec_jit_compile_method(func_id, self_value, jit_entry);

    globals
        .codegen
        .jit
        .apply_jmp_patch(entry_patch_point, guard);
}

pub(crate) extern "C" fn exec_jit_recompile_method(vm: &mut Executor, globals: &mut Globals) {
    let lfp = vm.cfp().lfp();
    let self_value = lfp.self_val();
    let func_id = lfp.meta().func_id();
    let jit_entry = globals.codegen.jit.label();
    globals.exec_jit_compile_method(func_id, self_value, jit_entry);
    // get_jit_code() must not be None.
    // After BOP redefinition occurs, recompilation in invalidated methods cause None.
    if let Some(patch_point) = globals[func_id].get_jit_code(self_value.class()) {
        globals.codegen.jit.apply_jmp_patch(patch_point, jit_entry);
    }
}

///
/// Compile the loop.
///
pub(crate) extern "C" fn exec_jit_partial_compile(
    vm: &mut Executor,
    globals: &mut Globals,
    pc: BcPc,
) {
    let entry_label = globals.codegen.jit.label();
    let self_value = vm.cfp().lfp().self_val();
    let func_id = vm.cfp().lfp().meta().func_id();
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
        unsafe { crate::builtins::YIELDER.unwrap().mark(alloc) };
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
pub(crate) extern "C" fn execute_gc(globals: &Globals, mut executor: &Executor) {
    // Get root Executor.
    while let Some(parent) = executor.parent_fiber {
        executor = unsafe { parent.as_ref() };
    }
    alloc::ALLOC.with(|alloc| alloc.borrow_mut().gc(&Root { globals, executor }));
}
