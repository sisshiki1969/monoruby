use super::*;

pub mod coerce;
mod constants;
pub mod frame;
pub mod inline;
pub mod op;
pub use codegen::*;
pub use frame::*;
pub use op::*;
use ruruby_parse::{Loc, SourceInfoRef};

pub type Result<T> = std::result::Result<T, MonorubyErr>;
pub type BuiltinFn = extern "C" fn(&mut Executor, &mut Globals, Lfp, BytecodePtr) -> Option<Value>;
pub type BinaryOpFn = extern "C" fn(&mut Executor, &mut Globals, Value, Value) -> Option<Value>;
pub type UnaryOpFn = extern "C" fn(&mut Executor, &mut Globals, Value) -> Option<Value>;

pub(crate) const RSP_LOCAL_FRAME: i32 = 40;
pub(crate) const RSP_CFP: i32 = 24;
pub(crate) const BP_CFP: i32 = 8;

// Control frame offsets
pub(crate) const CFP_LFP: i32 = 8;

// Local frame offsets
pub(crate) const LFP_OUTER: i32 = 0;
/// Meta 8bytes
pub(crate) const LFP_META: i32 = 8;
/// Meta::Regnum 2bytes
pub(crate) const LFP_REGNUM: i32 = LFP_META - META_REGNUM as i32;
/// Meta::FuncId 4bytes
pub(crate) const LFP_FUNCID: i32 = LFP_META + META_FUNCID as i32;
pub(crate) const LFP_BLOCK: i32 = 16;
pub(crate) const LFP_SELF: i32 = 24;
pub const LFP_ARG0: i32 = LFP_SELF + 8;

pub(crate) const EXECUTOR_CFP: i64 = std::mem::offset_of!(Executor, cfp) as _;
pub(crate) const EXECUTOR_RSP_SAVE: i64 = std::mem::offset_of!(Executor, rsp_save) as _;
pub(crate) const EXECUTOR_PARENT_FIBER: i64 = std::mem::offset_of!(Executor, parent_fiber) as _;
pub(crate) const EXECUTOR_STACK_LIMIT: i64 = std::mem::offset_of!(Executor, stack_limit) as _;

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
    stack_limit: usize,
    /// lexical class stack.
    lexical_class: Vec<Vec<Cref>>,
    sp_last_match: Option<String>,   // $&        : Regexp.last_match(0)
    sp_post_match: Option<String>,   // $'        : Regexp.post_match
    sp_matches: Vec<Option<String>>, // $&, $1 ... $n : Regexp.last_match(n)
    temp_stack: Vec<Value>,
    require_level: usize,
    /// error information.
    exception: Option<MonorubyErr>,
}

impl std::default::Default for Executor {
    fn default() -> Self {
        Self {
            cfp: None,
            rsp_save: None,
            parent_fiber: None,
            stack_limit: 0,
            lexical_class: vec![vec![]],
            sp_last_match: None,
            sp_post_match: None,
            sp_matches: vec![],
            temp_stack: vec![],
            require_level: 0,
            exception: None,
        }
    }
}

impl alloc::GC<RValue> for Executor {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.temp_stack.iter().for_each(|v| v.mark(alloc));
        let mut cfp = self.cfp;
        while let Some(inner_cfp) = cfp {
            inner_cfp.lfp().mark(alloc);
            cfp = inner_cfp.prev();
        }
    }
}

impl Executor {
    pub fn init(globals: &mut Globals, program_name: &str) -> Result<Self> {
        let program_name = Value::string_from_str(program_name);
        globals.set_gvar(IdentId::get_id("$0"), program_name);
        globals.set_gvar(IdentId::get_id("$PROGRAM_NAME"), program_name);
        let mut executor = Self::default();
        let path = dirs::home_dir()
            .unwrap()
            .join(".monoruby")
            .join("builtins")
            .join("startup.rb");
        executor.require(globals, &path, false)?;
        if !globals.no_gems {
            executor.load_gems(globals);
        }
        // Clear stale $! that may have been set during startup/gem loading.
        globals.set_gvar(IdentId::get_id("$!"), Value::nil());
        CODEGEN.with(|codegen| {
            codegen.borrow_mut().startup_flag = true;
        });
        #[cfg(feature = "profile")]
        globals.clear_stats();
        Ok(executor)
    }

    ///
    /// Set stack limit to (rsp - MAX_STACK_SIZE).
    ///
    pub fn init_stack_limit(&mut self, globals: &Globals) {
        let invoker = globals.invokers.init_stack_limit;
        invoker(self);
    }

    ///
    /// Set stack limit to (*rsp* - MAX_STACK_SIZE).
    ///
    pub fn set_stack_limit(&mut self, rsp: *mut u8) {
        // SAFETY: rsp is a valid stack pointer, and subtracting MAX_STACK_SIZE
        // stays within the allocated stack region.
        let stack_limit = unsafe { rsp.sub(MAX_STACK_SIZE) };
        self.stack_limit = stack_limit as usize;
    }

    fn load_gems(&mut self, globals: &mut Globals) {
        for gem in ["rubygems"] {
            if let Err(err) = self.require(globals, &std::path::PathBuf::from(gem), false) {
                err.show_error_message_and_all_loc(&globals.store);
                panic!("error occured in loading {gem}");
            }
        }
    }

    pub fn cfp(&self) -> Cfp {
        self.cfp.unwrap()
    }

    pub(crate) unsafe fn get_slot(&self, index: SlotId) -> Option<Value> {
        self.cfp().lfp().register(index)
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

    pub fn temp_hash_insert(&mut self, globals: &mut Globals, k: Value, v: Value) -> Result<()> {
        let mut h = self.temp_stack.last_mut().unwrap().as_hash();
        h.insert(k, v, self, globals)
    }

    pub fn temp_array_extend_from_slice(&mut self, slice: &[Value]) {
        self.temp_stack
            .last_mut()
            .unwrap()
            .as_array()
            .extend_from_slice(slice);
    }

    pub fn inc_require_level(&mut self) -> usize {
        let level = self.require_level;
        self.require_level += 1;
        level
    }

    pub fn dec_require_level(&mut self) -> usize {
        let level = self.require_level;
        self.require_level -= 1;
        level
    }

    pub fn parent_fiber(&self) -> Option<std::ptr::NonNull<Executor>> {
        self.parent_fiber
    }

    pub fn sp_last_match(&self) -> Value {
        self.sp_last_match
            .as_ref()
            .map(|s| Value::string_from_str(s))
            .unwrap_or_default()
    }

    pub fn sp_post_match(&self) -> Value {
        self.sp_post_match
            .as_ref()
            .map(|s| Value::string_from_str(s))
            .unwrap_or_default()
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
            Ok(res) => bytecodegen::bytecode_compile_script(globals, res),
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
        let res = (globals.invokers.method)(
            self,
            globals,
            func_id,
            main_object,
            ([]).as_ptr(),
            0,
            None,
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

        let res = (globals.invokers.binding)(self, globals, binding_lfp);
        res.ok_or_else(|| self.take_error())
    }

    pub fn require(
        &mut self,
        globals: &mut Globals,
        file_name: &std::path::Path,
        is_relative: bool,
    ) -> Result<bool> {
        if let Some((file_body, canonicalized_path)) =
            globals.require_lib(file_name, is_relative)?
        {
            match self.load_impl(globals, file_body, &canonicalized_path, None) {
                Ok(()) => {}
                Err(err) => {
                    globals
                        .loaded_canonicalized_files
                        .shift_remove(&canonicalized_path);
                    return Err(err);
                }
            };
            Ok(true)
        } else {
            Ok(false)
        }
    }

    ///
    /// Kernel#load — always (re-)executes the file, never checks loaded_features.
    ///
    /// When `r#priv` is true the file should run inside an anonymous module;
    ///
    pub fn load(
        &mut self,
        globals: &mut Globals,
        file_name: &std::path::Path,
        r#priv: bool,
    ) -> Result<()> {
        let wrap = if r#priv {
            Some(globals.define_toplevel_module(""))
        } else {
            None
        };

        let (file_body, path) = globals.find_for_load(file_name)?;
        self.load_impl(globals, file_body, &path, wrap)?;
        Ok(())
    }

    fn load_impl(
        &mut self,
        globals: &mut Globals,
        file_body: String,
        path: &std::path::Path,
        wrap: Option<Module>,
    ) -> Result<()> {
        let _level = self.inc_require_level();

        #[cfg(feature = "dump-require")]
        eprintln!("{} > {:?}", "  ".repeat(_level), path);

        self.enter_class_context();
        if let Some(wrap) = wrap {
            self.push_class_context(wrap.id());
        }
        let res = self.exec_script(globals, file_body, path);
        self.exit_class_context();

        #[cfg(feature = "dump-require")]
        eprintln!("{} < {:?}", "  ".repeat(_level), path);

        self.dec_require_level();

        res?;
        Ok(())
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

    pub(crate) fn push_instance_eval_context(&mut self, receiver: Value) {
        self.lexical_class
            .last_mut()
            .unwrap()
            .push(Cref::new_instance_eval(receiver, Visibility::Public));
    }

    pub(crate) fn pop_class_context(&mut self) {
        self.lexical_class.last_mut().unwrap().pop();
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

    pub fn context_class_id(&self) -> ClassId {
        self.lexical_class
            .last()
            .unwrap()
            .last()
            .map(|cref| match cref.context {
                DefinitionContext::Class(class_id) => class_id,
                DefinitionContext::Receiver(val) => val.class(),
            })
            .unwrap_or(OBJECT_CLASS)
    }

    /// Returns the lexical class nesting at the current point of execution,
    /// from innermost to outermost. Used by `Module.nesting`.
    pub(crate) fn current_class_nesting(&self) -> Vec<ClassId> {
        let frame = match self.lexical_class.last() {
            Some(f) => f,
            None => return vec![],
        };
        frame
            .iter()
            .rev()
            .filter_map(|cref| match cref.context {
                DefinitionContext::Class(class_id) => Some(class_id),
                DefinitionContext::Receiver(_) => None,
            })
            .collect()
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

    pub(crate) fn discard_error(&mut self) {
        self.exception = None
    }

    pub(crate) fn pop_error_trace(&mut self) {
        self.exception.as_mut().unwrap().trace.pop();
    }

    pub(crate) fn exception(&self) -> Option<&MonorubyErr> {
        self.exception.as_ref()
    }

    pub(crate) fn take_ex_obj(&mut self, globals: &mut Globals) -> Value {
        let err = self.take_error();
        match err.kind() {
            MonorubyErrKind::Load(path) => {
                let path = Value::string_from_str(&path.as_os_str().to_string_lossy());
                let v = Value::new_exception(err);
                globals
                    .store
                    .set_ivar(v, IdentId::get_id("/path"), path)
                    .unwrap();
                v
            }
            MonorubyErrKind::SystemExit(status) => {
                let status = *status;
                let v = Value::new_exception(err);
                globals
                    .store
                    .set_ivar(v, IdentId::get_id("/status"), Value::integer(status as i64))
                    .unwrap();
                v
            }
            MonorubyErrKind::NotMethod(Some(receiver)) => {
                let receiver = *receiver;
                let v = Value::new_exception(err);
                globals
                    .store
                    .set_ivar(v, IdentId::get_id("/receiver"), Value::from_u64(receiver))
                    .unwrap();
                v
            }
            _ => Value::new_exception(err),
        }
    }

    pub(crate) fn err_divide_by_zero(&mut self) {
        self.set_error(MonorubyErr::divide_by_zero());
    }

    ///
    /// Set FrozenError with message "can't modify frozen Integer: 5".
    ///
    pub(crate) fn err_cant_modify_frozen(&mut self, store: &Store, val: Value) {
        self.set_error(MonorubyErr::cant_modify_frozen(store, val));
    }

    pub(crate) fn push_error_location(&mut self, loc: Loc, sourceinfo: SourceInfoRef, fid: FuncId) {
        match &mut self.exception {
            Some(err) => {
                err.push_trace(loc, sourceinfo, fid);
            }
            None => unreachable!(),
        };
    }

    pub(crate) fn push_internal_error_location(&mut self, fid: FuncId) {
        match &mut self.exception {
            Some(err) => {
                err.push_internal_trace(fid);
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

    pub(crate) fn yield_fiber(&mut self, val: Value) -> Result<Value> {
        if self.parent_fiber.is_none() {
            return Err(MonorubyErr::fibererr(
                "can't yield from main fiber".to_string(),
            ));
        }
        let invoker = CODEGEN.with(|codegen| codegen.borrow().yield_fiber);
        match invoker(self as _, val) {
            Some(res) => Ok(res),
            // SAFETY: parent_fiber is guaranteed to be a valid pointer to an Executor
            // when it is set. This pointer remains valid for the lifetime of this fiber.
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
        let ConstSiteInfo {
            name,
            toplevel,
            mut prefix,
            base,
            ..
        } = globals.store[site_id].clone();
        // SAFETY: get_slot is safe to call here because we're accessing a valid slot
        // that was previously set by the bytecode compiler.
        let base = base.map(|base| unsafe { self.get_slot(base) }.unwrap());
        let current_func = self.method_func_id();
        let mut parent = if let Some(base) = base {
            base.expect_class_or_module(&globals.store)?.id()
        } else if toplevel {
            OBJECT_CLASS
        } else if prefix.is_empty() {
            let v = self.search_constant_checked(globals, name, current_func)?;
            return Ok((v, None));
        } else {
            let parent = prefix.remove(0);
            self.search_constant_checked(globals, parent, current_func)?
                .expect_class_or_module(&globals.store)?
                .id()
        };
        for constant in prefix {
            parent = self
                .get_constant_checked(globals, parent, constant)?
                .expect_class_or_module(&globals.store)?
                .id();
        }
        let v =
            self.get_constant_superclass_checked(globals, globals[parent].get_module(), name)?;
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
            source_loc,
            ..
        } = globals.store[site_id].clone();
        let mut parent = if let Some(base) = base {
            // SAFETY: get_slot is safe to call here because we're accessing a valid slot
            // that was previously set by the bytecode compiler.
            let base = unsafe { self.get_slot(base) }.unwrap();
            base.expect_class_or_module(&globals.store)?.id()
        } else if toplevel {
            OBJECT_CLASS
        } else if prefix.is_empty() {
            self.context_class_id()
        } else {
            let parent = prefix.remove(0);
            let current_func = self.method_func_id();
            self.search_constant_checked(globals, parent, current_func)?
                .expect_class_or_module(&globals.store)?
                .id()
        };
        for constant in prefix {
            parent = self
                .get_constant_checked(globals, parent, constant)?
                .expect_class_or_module(&globals.store)?
                .id();
        }
        globals.set_constant(parent, name, val);
        if let Some((file, line)) = source_loc {
            globals.store[parent].record_constant_location(name, file, line);
        }
        let receiver = globals.store[parent].get_module().into();
        self.invoke_method_inner(
            globals,
            IdentId::CONST_ADDED,
            receiver,
            &[Value::symbol(name)],
            None,
            None,
        )?;
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
        let parent = globals.store.iseq(fid).lexical_context.last().cloned();
        match parent {
            Some(parent) => Ok(globals[parent].get_module()),
            None => Err(MonorubyErr::runtimeerr(
                "class variable access from toplevel",
            )),
        }
    }
}

impl Executor {
    /// Invoke method_added or singleton_method_added callback for `class_id`.
    fn invoke_method_added(
        &mut self,
        globals: &mut Globals,
        class_id: ClassId,
        name: IdentId,
    ) -> Result<()> {
        let module = globals.store[class_id].get_module();
        let (hook, receiver) = if let Some(original_obj) = module.is_singleton() {
            (IdentId::SINGLETON_METHOD_ADDED, original_obj)
        } else {
            (IdentId::METHOD_ADDED, module.into())
        };
        self.invoke_method_inner(globals, hook, receiver, &[Value::symbol(name)], None, None)?;
        Ok(())
    }

    /// Register a singleton method and invoke the singleton_method_added hook.
    pub(crate) fn add_singleton_method(
        &mut self,
        globals: &mut Globals,
        class_id: ClassId,
        name: IdentId,
        func_id: FuncId,
        visibility: Visibility,
    ) -> Result<()> {
        let singleton_id = globals.store.get_metaclass(class_id).id();
        self.add_method(globals, singleton_id, name, func_id, visibility)
    }

    /// Register a public method and invoke the method_added hook.
    pub(crate) fn add_public_method(
        &mut self,
        globals: &mut Globals,
        class_id: ClassId,
        name: IdentId,
        func_id: FuncId,
    ) -> Result<()> {
        self.add_method(globals, class_id, name, func_id, Visibility::Public)
    }

    /// Register a private method and invoke the method_added hook.
    pub(crate) fn add_private_method(
        &mut self,
        globals: &mut Globals,
        class_id: ClassId,
        name: IdentId,
        func_id: FuncId,
    ) -> Result<()> {
        self.add_method(globals, class_id, name, func_id, Visibility::Private)
    }

    /// Register a method with the given visibility and invoke the method_added hook.
    pub(crate) fn add_method(
        &mut self,
        globals: &mut Globals,
        class_id: ClassId,
        name: IdentId,
        func_id: FuncId,
        visibility: Visibility,
    ) -> Result<()> {
        globals.add_method(class_id, name, func_id, visibility);
        self.invoke_method_added(globals, class_id, name)
    }

    /// Create an alias and invoke the method_added hook.
    pub(crate) fn alias_method_for_class(
        &mut self,
        globals: &mut Globals,
        class_id: ClassId,
        new_name: IdentId,
        old_name: IdentId,
    ) -> Result<()> {
        globals.alias_method_for_class(class_id, new_name, old_name)?;
        self.invoke_method_added(globals, class_id, new_name)
    }

    /// Define an attr_reader and invoke the method_added hook.
    pub(crate) fn define_attr_reader(
        &mut self,
        globals: &mut Globals,
        class_id: ClassId,
        method_name: IdentId,
        visi: Visibility,
    ) -> Result<IdentId> {
        let name = globals.define_attr_reader(class_id, method_name, visi);
        self.invoke_method_added(globals, class_id, name)?;
        Ok(name)
    }

    /// Define an attr_writer and invoke the method_added hook.
    pub(crate) fn define_attr_writer(
        &mut self,
        globals: &mut Globals,
        class_id: ClassId,
        method_name: IdentId,
        visi: Visibility,
    ) -> Result<IdentId> {
        let name = globals.define_attr_writer(class_id, method_name, visi);
        self.invoke_method_added(globals, class_id, name)?;
        Ok(name)
    }

    /// Invoke method_removed or singleton_method_removed callback for `class_id`.
    pub(crate) fn invoke_method_removed(
        &mut self,
        globals: &mut Globals,
        class_id: ClassId,
        name: IdentId,
    ) -> Result<()> {
        let module = globals.store[class_id].get_module();
        let (hook, receiver) = if let Some(original_obj) = module.is_singleton() {
            (IdentId::SINGLETON_METHOD_REMOVED, original_obj)
        } else {
            (IdentId::METHOD_REMOVED, module.into())
        };
        self.invoke_method_inner(globals, hook, receiver, &[Value::symbol(name)], None, None)?;
        Ok(())
    }

    /// Invoke method_undefined or singleton_method_undefined callback for `class_id`.
    pub(crate) fn invoke_method_undefined(
        &mut self,
        globals: &mut Globals,
        class_id: ClassId,
        name: IdentId,
    ) -> Result<()> {
        let module = globals.store[class_id].get_module();
        let (hook, receiver) = if let Some(original_obj) = module.is_singleton() {
            (IdentId::SINGLETON_METHOD_UNDEFINED, original_obj)
        } else {
            (IdentId::METHOD_UNDEFINED, module.into())
        };
        self.invoke_method_inner(globals, hook, receiver, &[Value::symbol(name)], None, None)?;
        Ok(())
    }

    pub(crate) fn define_method(
        &mut self,
        globals: &mut Globals,
        name: IdentId,
        func: FuncId,
    ) -> Result<Value> {
        let cref = self.get_class_context();
        let current_func = self.method_func_id();
        if let Some(iseq) = globals.store[func].is_iseq() {
            globals.store[iseq].lexical_context =
                globals.store.iseq(current_func).lexical_context.clone();
        } else {
            runtime::_dump_stacktrace(self, globals);
            return Err(MonorubyErr::runtimeerr(format!(
                "define func: {:?} {:016x}",
                name,
                (func.get() as u64) + ((name.get() as u64) << 32)
            )));
        }
        let class_id = match cref.context {
            DefinitionContext::Class(class_id) => class_id,
            DefinitionContext::Receiver(receiver) => globals.store.get_singleton(receiver)?.id(),
        };
        Codegen::check_bop_redefine(self.cfp());
        if cref.module_function {
            self.add_method(globals, class_id, name, func, Visibility::Private)?;
            self.add_singleton_method(globals, class_id, name, func, cref.visibility)?;
        } else {
            self.add_method(globals, class_id, name, func, cref.visibility)?;
        }
        Ok(Value::nil())
    }
}

// Invokation of methods and blocks.

impl Executor {
    ///
    /// Find method *name* for object *obj*.
    ///
    /// If not found, return MethodNotFound error.
    ///
    pub fn find_method(
        &mut self,
        globals: &mut Globals,
        recv: Value,
        func_name: IdentId,
        is_func_call: bool,
    ) -> Result<FuncId> {
        let class_id = recv.class();
        match globals.check_method_for_class(class_id, func_name) {
            Some(entry) => {
                match entry.visibility() {
                    Visibility::Private => {
                        if !is_func_call {
                            return Err(MonorubyErr::private_method_called(
                                globals, func_name, recv,
                            ));
                        }
                    }
                    Visibility::Protected => {
                        let caller_self = self.cfp().lfp().self_val();
                        let recv_class = recv.real_class(&globals.store).id();
                        if !caller_self.is_kind_of(&globals.store, recv_class) {
                            return Err(MonorubyErr::protected_method_called(
                                globals, func_name, recv,
                            ));
                        }
                    }
                    _ => {}
                }
                match entry.func_id() {
                    Some(func_id) => Ok(func_id),
                    None => Err(MonorubyErr::method_not_found(globals, func_name, recv)),
                }
            }
            None => Err(MonorubyErr::method_not_found(globals, func_name, recv)),
        }
    }

    ///
    /// Invoke method for *receiver* and *method*.
    ///
    pub(crate) fn invoke_method(
        &mut self,
        globals: &mut Globals,
        method: IdentId,
        is_func_call: bool,
        receiver: Value,
        args: &[Value],
        bh: Option<BlockHandler>,
        kw_args: Option<Hashmap>,
    ) -> Option<Value> {
        let func_id = match self.find_method(globals, receiver, method, is_func_call) {
            Ok(id) => id,
            Err(original_err) => {
                // Fall back to method_missing, matching CRuby behavior.
                match self.find_method(globals, receiver, IdentId::METHOD_MISSING, true) {
                    Ok(mm_func_id) => {
                        let mut mm_args = Vec::with_capacity(args.len() + 1);
                        mm_args.push(Value::symbol(method));
                        mm_args.extend_from_slice(args);
                        return self
                            .invoke_func(globals, mm_func_id, receiver, &mm_args, bh, kw_args);
                    }
                    Err(_) => {
                        self.set_error(original_err);
                        return None;
                    }
                }
            }
        };
        self.invoke_func(globals, func_id, receiver, args, bh, kw_args)
    }

    ///
    /// Invoke method for *receiver* and *method*.
    ///
    pub(crate) fn invoke_method_simple(
        &mut self,
        globals: &mut Globals,
        method: IdentId,
        receiver: Value,
        args: &[Value],
    ) -> Option<Value> {
        self.invoke_method(globals, method, false, receiver, args, None, None)
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
        kw_args: Option<Hashmap>,
    ) -> Result<Value> {
        match self.find_method(globals, receiver, method, true) {
            Ok(func_id) => self.invoke_func_inner(globals, func_id, receiver, args, bh, kw_args),
            Err(original_err) => {
                // Fall back to method_missing, matching CRuby behavior.
                match self.find_method(globals, receiver, IdentId::METHOD_MISSING, true) {
                    Ok(mm_func_id) => {
                        let mut mm_args = Vec::with_capacity(args.len() + 1);
                        mm_args.push(Value::symbol(method));
                        mm_args.extend_from_slice(args);
                        self.invoke_func_inner(globals, mm_func_id, receiver, &mm_args, bh, kw_args)
                    }
                    Err(_) => Err(original_err),
                }
            }
        }
    }

    pub(crate) fn invoke_eq(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<bool> {
        let func_id = self.find_method(globals, lhs, IdentId::_EQ, true)?;
        let b = self.invoke_func_inner(globals, func_id, lhs, &[rhs], None, None)?;
        Ok(b.as_bool())
    }

    pub(crate) fn invoke_method_missing(
        &mut self,
        globals: &mut Globals,
        receiver: Value,
        lfp: Lfp,
        callsite: CallSiteId,
    ) -> Option<Value> {
        // Extract all needed fields from callsite before we mutably borrow self/globals.
        let cs = &globals.store[callsite];
        let cs_name = cs.name;
        let cs_args = cs.args;
        let cs_pos_num = cs.pos_num;
        let cs_splat_pos = cs.splat_pos.clone();
        let cs_kw_pos = cs.kw_pos;
        let cs_kw_len = cs.kw_len();
        let cs_kw_args = cs.kw_args.clone();
        let cs_hash_splat_pos = cs.hash_splat_pos.clone();
        let bh = cs.block_handler(lfp);

        let method_name = if let Some(name) = cs_name {
            name
        } else {
            let func_id = self.method_func_id();
            globals.store[func_id].name().unwrap()
        };
        // SAFETY: args_to_vec safely accesses the arguments stored in the local frame pointer.
        // The callsite.args and callsite.pos_num are valid and within bounds.
        let mut args = unsafe { lfp.args_to_vec(cs_args, cs_pos_num) };
        // Expand splat arguments: positions listed in splat_pos hold Array values
        // that must be flattened into the argument list.
        if !cs_splat_pos.is_empty() {
            let mut expanded = Vec::new();
            for (i, v) in args.into_iter().enumerate() {
                if cs_splat_pos.contains(&i) {
                    if let Some(ary) = v.try_array_ty() {
                        expanded.extend_from_slice(&ary);
                    } else {
                        expanded.push(v);
                    }
                } else {
                    expanded.push(v);
                }
            }
            args = expanded;
        }
        args.insert(0, Value::symbol(method_name));
        let kw = if cs_kw_len == 0 {
            None
        } else {
            let mut map = HashmapInner::default();
            for (k, offset) in cs_kw_args.into_iter() {
                map.insert(
                    Value::symbol(k),
                    lfp.register(cs_kw_pos + offset).unwrap(),
                    self,
                    globals,
                )
                .unwrap();
            }
            // Merge hash splat arguments into the keyword hash.
            for pos in cs_hash_splat_pos.iter() {
                if let Some(v) = lfp.register(*pos) {
                    if !v.is_nil() {
                        if let Some(hash) = v.try_hash_ty() {
                            for (k, v) in hash.iter() {
                                map.insert(k, v, self, globals).unwrap();
                            }
                        }
                    }
                }
            }
            Some(Value::hash_from_inner(map).as_hash())
        };
        // method_missing should always be callable regardless of visibility.
        // In Ruby, method_missing is conventionally private, but the VM must
        // still dispatch to it when a method is not found.
        //
        // When a proxy block handler is present, we must adjust its depth by -1.
        // invoke_method_missing is called from JIT/VM code without adding a
        // frame to the CFP chain, but invoke_func (called downstream) applies
        // delegate() which increments the proxy depth by 1. Without this
        // pre-adjustment, block_arg in the method_missing body would try to
        // walk too many frames and panic.
        let bh = bh.map(|bh| bh.undelegate());
        let res = self.invoke_method(
            globals,
            IdentId::METHOD_MISSING,
            true,
            receiver,
            &args,
            bh,
            kw,
        );
        if res.is_none() {
            self.pop_error_trace();
        }
        res
    }

    pub(crate) fn invoke_tos(&mut self, globals: &mut Globals, receiver: Value) -> Result<Value> {
        match receiver.unpack() {
            RV::Object(_) => {}
            _ => return Ok(Value::string(receiver.to_s(&globals.store))),
        }
        let func_id = self.find_method(globals, receiver, IdentId::TO_S, true)?;
        self.invoke_func_inner(globals, func_id, receiver, &[], None, None)
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
        data: &ProcData,
        args: &[Value],
    ) -> Result<Value> {
        (globals.invokers.block)(
            self,
            globals,
            data,
            Value::nil(),
            args.as_ptr(),
            args.len(),
            None,
        )
        .ok_or_else(|| self.take_error())
    }

    pub(crate) fn invoke_block_with_self(
        &mut self,
        globals: &mut Globals,
        data: &ProcData,
        self_val: Value,
        args: &[Value],
    ) -> Result<Value> {
        (globals.invokers.block_with_self)(
            self,
            globals,
            data as _,
            self_val,
            args.as_ptr(),
            args.len(),
            None,
        )
        .ok_or_else(|| self.take_error())
    }

    pub(crate) fn module_eval(
        &mut self,
        globals: &mut Globals,
        module: Module,
        bh: BlockHandler,
    ) -> Result<Value> {
        let data = self.get_block_data(globals, bh)?;
        self.push_class_context(module.id());
        let res = self.invoke_block_with_self(globals, &data, module.get(), &[module.get()]);
        self.pop_class_context();
        res
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

    /*pub(crate) fn invoke_block_iter_with_index1(
        &mut self,
        globals: &mut Globals,
        bh: BlockHandler,
        iter: impl Iterator<Item = Value>,
    ) -> Result<()> {
        let data = self.get_block_data(globals, bh)?;
        for (index, val) in iter.enumerate() {
            self.invoke_block(
                globals,
                &data,
                &[Value::array2(val, Value::integer(index as i64))],
            )?;
        }
        Ok(())
    }*/

    pub(crate) fn invoke_block_map1(
        &mut self,
        globals: &mut Globals,
        bh: BlockHandler,
        iter: impl Iterator<Item = Value>,
        size_hint: impl Into<Option<usize>>,
    ) -> Result<Value> {
        fn inner(
            vm: &mut Executor,
            globals: &mut Globals,
            data: &ProcData,
            iter: impl Iterator<Item = Value>,
        ) -> Result<()> {
            for v in iter {
                let res = vm.invoke_block(globals, data, &[v])?;
                vm.temp_array_push(res);
            }
            Ok(())
        }
        let data = self.get_block_data(globals, bh)?;
        self.temp_array_new(size_hint);
        let res = inner(self, globals, &data, iter);
        let v = self.temp_pop();
        res?;
        Ok(v)
    }

    pub(crate) fn invoke_block_flat_map1(
        &mut self,
        globals: &mut Globals,
        bh: BlockHandler,
        iter: impl Iterator<Item = Value>,
        size_hint: impl Into<Option<usize>>,
    ) -> Result<Value> {
        fn inner(
            vm: &mut Executor,
            globals: &mut Globals,
            data: &ProcData,
            iter: impl Iterator<Item = Value>,
        ) -> Result<()> {
            for v in iter {
                let res = vm.invoke_block(globals, &data, &[v])?;
                if let Some(ary) = res.try_array_ty() {
                    vm.temp_array_extend_from_slice(&ary);
                } else {
                    vm.temp_array_push(res);
                }
            }
            Ok(())
        }
        let data = self.get_block_data(globals, bh)?;
        self.temp_array_new(size_hint);
        let res = inner(self, globals, &data, iter);
        let v = self.temp_pop();
        res?;
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
        proc: &ProcInner,
        args: &[Value],
    ) -> Result<Value> {
        let proc = ProcData::from_proc(proc);
        (globals.invokers.block)(
            self,
            globals,
            &proc,
            Value::nil(),
            args.as_ptr(),
            args.len(),
            None,
        )
        .ok_or_else(|| {
            let err = self.take_error();
            // MethodReturn that escapes a Proc#call means the target method
            // has already returned — convert to LocalJumpError.
            if let MonorubyErrKind::MethodReturn(val, _) = err.kind() {
                MonorubyErr::localjumperr_with_val("unexpected return", *val)
            } else {
                err
            }
        })
    }

    pub(crate) fn invoke_method_if_exists(
        &mut self,
        globals: &mut Globals,
        method: IdentId,
        receiver: Value,
        args: &[Value],
        bh: Option<BlockHandler>,
        kw_args: Option<Hashmap>,
    ) -> Result<Option<Value>> {
        Ok(
            if let Some(func_id) = globals.check_method(receiver, method) {
                Some(self.invoke_func_inner(globals, func_id, receiver, args, bh, kw_args)?)
            } else {
                None
            },
        )
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
        kw_args: Option<Hashmap>,
    ) -> Option<Value> {
        let bh = bh.map(|bh| bh.delegate());
        (globals.invokers.method)(
            self,
            globals,
            func_id,
            receiver,
            args.as_ptr(),
            args.len(),
            bh,
            kw_args,
        )
    }

    pub(crate) fn invoke_func_inner(
        &mut self,
        globals: &mut Globals,
        func_id: FuncId,
        receiver: Value,
        args: &[Value],
        bh: Option<BlockHandler>,
        kw_args: Option<Hashmap>,
    ) -> Result<Value> {
        self.invoke_func(globals, func_id, receiver, args, bh, kw_args)
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
    ) -> Result<ProcData> {
        if let Some(proxy) = bh.try_proxy() {
            Ok(ProcData::from_proxy(self, proxy))
        } else if let Some(proc) = bh.0.is_proc() {
            Ok(ProcData::from_proc(&proc))
        } else if let Some(proc) =
            self.invoke_method_if_exists(globals, IdentId::TO_PROC, bh.0, &[], None, None)?
            && let Some(proc) = proc.is_proc()
        {
            Ok(ProcData::from_proc(&proc))
        } else {
            Err(MonorubyErr::wrong_argument_type(globals, bh.0, "Proc"))
        }
    }

    pub fn to_s(&mut self, globals: &mut Globals, receiver: Value) -> Result<String> {
        self.invoke_method_inner(globals, IdentId::TO_S, receiver, &[], None, None)?
            .expect_string(globals)
    }

    pub(crate) fn define_class(
        &mut self,
        globals: &mut Globals,
        base: Option<Value>,
        name: IdentId,
        superclass: Option<Value>,
        is_module: bool,
    ) -> Result<Value> {
        let parent = match base {
            Some(base) => base.expect_class_or_module(&globals.store)?.id(),
            None => self.context_class_id(),
        };
        let (self_val, is_new) = match self.get_constant(globals, parent, name)? {
            Some(val) => {
                let val = val.expect_class_or_module(&globals.store)?;
                if let Some(superclass) = superclass {
                    assert!(!is_module);
                    let superclass_id = superclass.expect_class(globals)?.id();
                    if Some(superclass_id) != val.get_real_superclass().map(|m| m.id()) {
                        return Err(MonorubyErr::superclass_mismatch(name));
                    }
                }
                (val, false)
            }
            None => {
                let superclass = match superclass {
                    Some(superclass) => {
                        assert!(!is_module);
                        superclass.expect_class(globals)?
                    }
                    None => globals.store.object_class(),
                };
                let new_module = if is_module {
                    globals.define_module_with_identid(name, parent)
                } else {
                    let new_class =
                        globals.define_class_with_identid(name, Some(superclass), parent);
                    // CRuby invokes `const_added` BEFORE `inherited` when a
                    // new class is created via the `class` keyword.
                    let parent_val = globals.store[parent].get_module().into();
                    self.invoke_method_inner(
                        globals,
                        IdentId::CONST_ADDED,
                        parent_val,
                        &[Value::symbol(name)],
                        None,
                        None,
                    )?;
                    self.invoke_method_inner(
                        globals,
                        IdentId::INHERITED,
                        superclass.as_val(),
                        &[new_class.as_val()],
                        None,
                        None,
                    )?;
                    return self.finish_class_def(globals, new_class);
                };
                (new_module, true)
            }
        };
        if is_new {
            // Module case: const_added without inherited.
            let parent_val = globals.store[parent].get_module().into();
            self.invoke_method_inner(
                globals,
                IdentId::CONST_ADDED,
                parent_val,
                &[Value::symbol(name)],
                None,
                None,
            )?;
        }
        self.push_class_context(self_val.id());
        Ok(self_val.as_val())
    }

    fn finish_class_def(&mut self, _globals: &mut Globals, new_class: Module) -> Result<Value> {
        self.push_class_context(new_class.id());
        Ok(new_class.as_val())
    }
}

impl Executor {
    pub(crate) fn generate_proc(&self, bh: BlockHandler, pc: BytecodePtr) -> Result<Proc> {
        self.generate_proc_inner(self.cfp(), bh, pc)
    }

    pub(crate) fn generate_proc_inner(
        &self,
        mut cfp: Cfp,
        bh: BlockHandler,
        pc: BytecodePtr,
    ) -> Result<Proc> {
        if let Some((fid, outer)) = bh.try_proxy() {
            // Walk back through the call frame chain to the block's outer scope,
            // using the proxy's depth index.
            let mut vm = self;
            for _ in 0..outer {
                (vm, cfp) = Self::prev_cfp(vm, cfp);
            }
            // Move the correct outer frame (and its lexical chain) to the heap,
            // so the proc can safely reference it after the current scope exits.
            let outer_lfp = cfp.lfp().move_frame_to_heap();
            Ok(Proc::from_parts(outer_lfp, fid, pc))
        } else if let Some(proc) = bh.try_proc() {
            Ok(proc)
        } else {
            Err(MonorubyErr::runtimeerr(format!(
                "not yet implemented: block handler {bh:?}"
            )))
        }
    }

    pub(crate) fn generate_lambda(&mut self, func_id: FuncId, pc: BytecodePtr) -> Proc {
        let outer_lfp = self.cfp().lfp().move_frame_to_heap();
        Proc::from_parts(outer_lfp, func_id, pc)
    }

    pub(crate) fn generate_binding(&mut self, pc: BytecodePtr) -> Binding {
        let lfp = self.cfp().prev().unwrap().lfp();
        Binding::from_outer(lfp, Some(pc))
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
    pub(crate) fn generate_enumerator(
        &mut self,
        method: IdentId,
        obj: Value,
        args: Vec<Value>,
        pc: BytecodePtr,
    ) -> Result<Value> {
        let outer_lfp = Lfp::dummy_heap_frame_with_self(obj);
        let proc = Proc::from_parts(outer_lfp, FuncId::new(1), pc);
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
    pub(crate) fn save_capture_special_variables<'h>(
        &mut self,
        captures: &onigmo_regex::Captures<'h>,
    ) {
        match captures.get(0) {
            Some(m) => {
                self.sp_last_match = Some(m.to_string());
                self.sp_post_match = Some(m.post().to_string());
            }
            None => {
                self.sp_last_match = None;
                self.sp_post_match = None;
            }
        };

        self.sp_matches.clear();
        for m in captures.iter() {
            self.sp_matches.push(m.map(|m| m.to_string()));
        }
    }

    pub(crate) fn get_special_matches(&self, mut nth: i64) -> Value {
        if nth < 0 {
            nth += self.sp_matches.len() as i64
        }
        if nth >= 0 {
            if let Some(Some(s)) = self.sp_matches.get(nth as usize) {
                return Value::string_from_str(s);
            }
        };
        Value::nil()
    }

    pub(crate) fn get_last_matchdata(&self) -> Value {
        if self.sp_matches.is_empty() {
            return Value::nil();
        }
        Value::array_from_iter(self.sp_matches.iter().map(|s| {
            if let Some(s) = s {
                Value::string_from_str(s)
            } else {
                Value::nil()
            }
        }))
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

    /// Reverse of delegate(): decrement the proxy depth by 1.
    /// Non-proxy block handlers (e.g. Proc) are returned unchanged.
    pub fn undelegate(self) -> Self {
        match self.0.try_fixnum() {
            Some(i) => Self(Value::integer(i - 1)),
            None => self,
        }
    }

    pub fn try_proc(self) -> Option<Proc> {
        Proc::try_new(self.0)
    }

    pub(crate) fn id(&self) -> u64 {
        self.0.id()
    }
}

/// The default definee context for `def`.
#[derive(Clone, Copy, Debug)]
enum DefinitionContext {
    /// A normal class/module context. `def` defines a method on this class.
    Class(ClassId),
    /// An `instance_eval`/`instance_exec` context. `def` defines a singleton method
    /// on this receiver. The singleton class is created lazily at `def` time.
    Receiver(Value),
}

#[derive(Clone, Copy, Debug)]
struct Cref {
    pub(crate) context: DefinitionContext,
    pub(crate) module_function: bool,
    pub(crate) visibility: Visibility,
}

impl Cref {
    fn new(class_id: ClassId, module_function: bool, visibility: Visibility) -> Self {
        Self {
            context: DefinitionContext::Class(class_id),
            module_function,
            visibility,
        }
    }

    fn new_instance_eval(receiver: Value, visibility: Visibility) -> Self {
        Self {
            context: DefinitionContext::Receiver(receiver),
            module_function: false,
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

#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Hash, Default)]
#[repr(transparent)]
pub struct SlotId(pub u16);

impl std::iter::Step for SlotId {
    fn steps_between(start: &Self, end: &Self) -> (usize, Option<usize>) {
        if start.0 <= end.0 {
            let d = end.0 as usize - start.0 as usize;
            (d, Some(d))
        } else {
            (0, None)
        }
    }

    fn forward_checked(start: Self, count: usize) -> Option<Self> {
        if start.0 as usize + count <= u16::MAX as usize {
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
        if reg == 0 { None } else { Some(Self(reg)) }
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
    Undefined = 3,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum RecompileReason {
    NotCached = 0,
    MethodNotFound = 1,
    IvarIdNotFound = 2,
    ClassVersionGuardFailed = 3,
}

struct Root<'a, 'b> {
    globals: &'a Globals,
    executor: &'b Executor,
}

impl<'a, 'b> alloc::GC<RValue> for Root<'a, 'b> {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        // SAFETY: YIELDER is a static mutable variable that is properly initialized
        // before GC runs. Access is synchronized through the single-threaded nature
        // of the Ruby VM.
        unsafe { crate::builtins::YIELDER.unwrap().mark(alloc) };
        self.globals.mark(alloc);
        self.executor.mark(alloc);
    }
}

impl<'a, 'b> alloc::GCRoot<RValue> for Root<'a, 'b> {
    #[cfg(feature = "gc-debug")]
    fn startup_flag(&self) -> bool {
        true
    }
}

///
/// Execute garbage collection.
///
pub(crate) extern "C" fn execute_gc(
    mut executor: &mut Executor,
    globals: &mut Globals,
) -> Option<Value> {
    CODEGEN.with(|codegen| {
        let codegen = codegen.borrow_mut();
        if codegen.sigint_flag() {
            executor.set_error(MonorubyErr::runtimeerr("Interrupt"));
            codegen.unset_sigint_flag();
            None
        } else {
            Some(())
        }
    })?;
    // Get root Executor.
    while let Some(mut parent) = executor.parent_fiber {
        // SAFETY: parent_fiber is guaranteed to be a valid pointer to an Executor
        // that outlives this borrow. The parent fiber structure is maintained correctly.
        executor = unsafe { parent.as_mut() };
    }
    alloc::ALLOC.with(|alloc| alloc.borrow_mut().gc(&Root { globals, executor }));
    Some(Value::nil())
}
