use super::*;

mod constants;
pub mod frame;
pub mod inline;
pub mod op;
pub use codegen::*;
pub use frame::*;
pub use op::*;
use ruruby_parse::{Loc, SourceInfoRef};

pub type Result<T> = std::result::Result<T, MonorubyErr>;
pub type BuiltinFn = extern "C" fn(&mut Executor, &mut Globals, Lfp) -> Option<Value>;
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
pub(crate) const LFP_META_REGNUM: i32 = LFP_META - META_REGNUM as i32;
/// Meta::FuncId 4bytes
//pub(crate) const LBP_META_FUNCID: i64 = LBP_META + META_FUNCID as i64;
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
        let path = dirs::home_dir().unwrap().join(".monoruby").join("builtins");
        for file in [
            "startup.rb",
            "comparable.rb",
            "enumerable.rb",
            "builtins.rb",
            "pathname_builtins.rb",
        ] {
            executor.require(globals, &path.clone().join(file), false)?;
        }
        if !globals.no_gems {
            executor.load_gems(globals);
        }
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
        let stack_limit = unsafe { rsp.sub(MAX_STACK_SIZE) };
        self.stack_limit = stack_limit as usize;
    }

    fn load_gems(&mut self, globals: &mut Globals) {
        if let Err(err) = self.require(globals, &std::path::PathBuf::from("rubygems"), false) {
            err.show_error_message_and_all_loc(&globals.store);
            panic!("error occured in loading Gems.");
        }
        if let Err(err) = self.require(globals, &std::path::PathBuf::from("pp"), false) {
            err.show_error_message_and_all_loc(&globals.store);
            panic!("error occurred in loading pp.");
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
        if let Some((file_body, path, canonicalized_path)) =
            globals.load_lib(file_name, is_relative)?
        {
            let _level = self.inc_require_level();

            #[cfg(feature = "dump-require")]
            eprintln!("{} > {:?}", "  ".repeat(_level), canonicalized_path);

            self.enter_class_context();
            let res = self.exec_script(globals, file_body, &path);
            self.exit_class_context();

            #[cfg(feature = "dump-require")]
            eprintln!("{} < {:?}", "  ".repeat(_level), canonicalized_path);

            self.dec_require_level();
            if res.is_err() {
                globals
                    .loaded_canonicalized_files
                    .shift_remove(&canonicalized_path);
            }
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

    pub fn context_class_id(&self) -> ClassId {
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
        if let MonorubyErrKind::Load(path) = &err.kind() {
            let path = Value::string_from_str(path.as_os_str().to_str().unwrap());
            let v = Value::new_exception(err);
            globals
                .store
                .set_ivar(v, IdentId::get_id("/path"), path)
                .unwrap();
            v
        } else {
            Value::new_exception(err)
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
        let invoker = CODEGEN.with(|codegen| codegen.borrow().yield_fiber);
        match invoker(self as _, val) {
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
        let ConstSiteInfo {
            name,
            toplevel,
            mut prefix,
            base,
            ..
        } = globals.store[site_id].clone();
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
            ..
        } = globals.store[site_id].clone();
        let mut parent = if let Some(base) = base {
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
    pub(crate) fn define_method(
        &mut self,
        globals: &mut Globals,
        name: IdentId,
        func: FuncId,
    ) -> Result<Value> {
        let Cref {
            class_id,
            module_function,
            visibility,
        } = self.get_class_context();
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
        globals.add_method(class_id, name, func, visibility);
        if module_function {
            globals.add_singleton_method(class_id, name, func, visibility);
        }
        Codegen::check_bop_redefine(self.cfp());
        self.invoke_method_if_exists(
            globals,
            IdentId::METHOD_ADDED,
            globals.store[class_id].get_module().into(),
            &[Value::symbol(name)],
            None,
            None,
        )?;
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
                        //if !is_func_call {
                        //    self.err_protected_method_called(func_name, obj);
                        //    return None;
                        //}
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
            Err(err) => {
                self.set_error(err);
                return None;
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
        let func_id = self.find_method(globals, receiver, method, true)?;
        self.invoke_func_inner(globals, func_id, receiver, args, bh, kw_args)
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
        let callsite = &globals.store[callsite];
        assert!(callsite.splat_pos.is_empty());
        assert!(!callsite.has_hash_splat());
        let is_func_call = callsite.is_func_call();
        let method_name = if let Some(name) = callsite.name {
            name
        } else {
            let func_id = self.method_func_id();
            globals.store[func_id].name().unwrap()
        };
        let bh = callsite.block_handler(lfp);
        let mut args = unsafe { lfp.args_to_vec(callsite.args, callsite.pos_num) };
        args.insert(0, Value::symbol(method_name));
        let kw_pos = callsite.kw_pos;
        let kw = if callsite.kw_len() == 0 {
            None
        } else {
            let mut map = HashmapInner::default();
            for (k, offset) in callsite.kw_args.clone().into_iter() {
                map.insert(
                    Value::symbol(k),
                    lfp.register(kw_pos + offset).unwrap(),
                    self,
                    globals,
                )
                .unwrap();
            }
            Some(Value::hash_from_inner(map).as_hash())
        };
        let res = self.invoke_method(
            globals,
            IdentId::METHOD_MISSING,
            is_func_call,
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
        let invoker = CODEGEN.with(|codegen| codegen.borrow().block_invoker_with_self);
        invoker(
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
        .ok_or_else(|| self.take_error())
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
        let self_val = match self.get_constant(globals, parent, name)? {
            Some(val) => {
                let val = val.expect_class_or_module(&globals.store)?;
                if let Some(superclass) = superclass {
                    assert!(!is_module);
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
                        assert!(!is_module);
                        superclass.expect_class(globals)?
                    }
                    None => globals.store.object_class(),
                };
                if is_module {
                    globals.define_module_with_identid(name, parent)
                } else {
                    globals.define_class_with_identid(name, Some(superclass), parent)
                }
            }
        };
        self.push_class_context(self_val.id());
        Ok(self_val.as_val())
    }
}

impl Executor {
    pub fn generate_proc(&mut self, bh: BlockHandler) -> Result<Proc> {
        if let Some(proxy) = bh.try_proxy() {
            let outer_lfp = self.cfp().prev().unwrap().lfp();
            outer_lfp.move_frame_to_heap();
            let proc = Proc::from(ProcData::from_proxy(self, proxy).to_proc().unwrap());
            Ok(proc)
        } else if let Some(proc) = bh.try_proc() {
            Ok(proc)
        } else {
            unimplemented!("bh: {bh:?}")
        }
    }

    pub fn generate_lambda(&mut self, func_id: FuncId) -> Proc {
        let outer_lfp = self.cfp().lfp();
        outer_lfp.move_frame_to_heap();
        Proc::from_parts(outer_lfp, func_id)
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

    pub fn try_proc(self) -> Option<Proc> {
        Proc::try_new(self.0)
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

#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Hash, Default)]
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
        executor = unsafe { parent.as_mut() };
    }
    alloc::ALLOC.with(|alloc| alloc.borrow_mut().gc(&Root { globals, executor }));
    Some(Value::nil())
}
