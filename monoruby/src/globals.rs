use ruruby_parse::{BlockInfo, Loc, LvarCollector, Node, ParamKind, Parser, SourceInfoRef};
use std::io::{BufWriter, Stdout, stdout};
use std::io::{Read, Write};
use std::path::PathBuf;
use std::sync::atomic::AtomicU8;

use super::*;

mod dump;
mod error;
mod gvar;
mod method;
mod prng;
mod require;
mod store;
#[cfg(any(feature = "deopt", feature = "profile"))]
pub(crate) use dump::log_deoptimize;
pub use error::*;
pub use gvar::*;
use prng::*;
pub use require::load_file;
pub use store::*;

pub static WARNING: std::sync::LazyLock<AtomicU8> = std::sync::LazyLock::new(|| AtomicU8::new(0u8));

pub(crate) type InlineGen = dyn Fn(
    &mut jitgen::AbstractState,
    &mut jitgen::asmir::AsmIr,
    &crate::jitgen::JitContext,
    &Store,
    CallSiteId,
    ClassId,
    Option<ClassId>,
) -> bool;

pub(crate) const GLOBALS_FUNCINFO: usize =
    std::mem::offset_of!(Globals, store.functions.info) + MONOVEC_PTR;

#[derive(Clone, Debug)]
pub(crate) struct ExternalContext {
    scope: Vec<(
        indexmap::IndexMap<IdentId, bytecodegen::BcLocal>,
        Option<IdentId>,
    )>,
}

impl ruruby_parse::LocalsContext for ExternalContext {
    fn find_lvar(&self, name: &str) -> Option<usize> {
        let id = IdentId::get_id(name);
        for (outer, scope) in self.scope.iter().enumerate() {
            if scope.0.get(&id).is_some() {
                return Some(outer + 1);
            }
        }
        None
    }
}

impl std::ops::Index<usize> for ExternalContext {
    type Output = (
        indexmap::IndexMap<IdentId, bytecodegen::BcLocal>,
        Option<IdentId>,
    );
    fn index(&self, index: usize) -> &Self::Output {
        &self.scope[index]
    }
}

impl ExternalContext {
    pub fn new() -> Self {
        Self { scope: vec![] }
    }
}

pub(crate) struct Invokers {
    pub init_stack_limit: extern "C" fn(&mut Executor) -> *const u8,
    pub method: MethodInvoker,
    pub block: BlockInvoker,
    pub block_with_self: BlockInvoker,
    pub fiber: FiberInvoker,
    pub fiber_with_self: FiberInvoker,
    pub binding: BindingInvoker,
}

///
/// Global state.
///
pub struct Globals {
    /// main object (`self`` of toplevel).
    pub main_object: Value,
    /// function and class info.
    pub store: Store,
    /// global variables and special variables.
    pub(crate) gvars: GvarTable,
    /// suppress jit compilation.
    pub no_jit: bool,
    /// suppress loading gem.
    pub no_gems: bool,
    /// stdout.
    stdout: BufWriter<Stdout>,
    /// library directries.
    load_path: Value,
    /// standard PRNG
    random: Box<Prng>,
    /// loaded libraries (canonical path).
    pub(crate) loaded_canonicalized_files: indexmap::IndexSet<PathBuf>,
    /// cache for Symbol#name (frozen strings keyed by IdentId).
    pub(crate) symbol_names: HashMap<IdentId, Value>,
    /// address of invokers.
    pub(crate) invokers: Invokers,
    /// stats for deoptimization
    #[cfg(feature = "profile")]
    deopt_stats: HashMap<(FuncId, bytecodegen::BcIndex), usize>,
    #[cfg(feature = "profile")]
    jit_class_unmatched_stats: HashMap<(FuncId, ClassId), usize>,
    #[cfg(feature = "profile")]
    jit_recompile_count: HashMap<(FuncId, ClassId, RecompileReason), usize>,
    #[cfg(feature = "emit-bc")]
    dumped_bc: usize,
}

impl std::ops::Deref for Globals {
    type Target = Store;
    fn deref(&self) -> &Self::Target {
        &self.store
    }
}

impl std::ops::DerefMut for Globals {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.store
    }
}

impl alloc::GC<RValue> for Globals {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.main_object.mark(alloc);
        self.load_path.mark(alloc);
        self.store.mark(alloc);
        self.gvars.mark_values(|v| v.mark(alloc));
        self.symbol_names.values().for_each(|v| v.mark(alloc));
    }
}

impl Globals {
    pub fn new(warning: u8, no_jit: bool, no_gems: bool) -> Self {
        assert_eq!(64, std::mem::size_of::<FuncInfo>());

        WARNING.store(warning, std::sync::atomic::Ordering::Relaxed);

        let main_object = Value::object(OBJECT_CLASS);

        let mut loaded_canonicalized_files = indexmap::IndexSet::default();
        ["thread.rb"].iter().for_each(|f| {
            loaded_canonicalized_files.insert(std::path::PathBuf::from(f));
        });

        let invokers = CODEGEN.with(|codegen| {
            let codegen = codegen.borrow();
            Invokers {
                init_stack_limit: codegen.init_stack_limit,
                method: codegen.method_invoker,
                block: codegen.block_invoker,
                block_with_self: codegen.block_invoker_with_self,
                fiber: codegen.fiber_invoker,
                fiber_with_self: codegen.fiber_invoker_with_self,
                binding: codegen.binding_invoker,
            }
        });

        let mut globals = Self {
            main_object,
            store: Store::new(),
            gvars: GvarTable::new(),
            no_jit,
            no_gems,
            stdout: BufWriter::new(stdout()),
            load_path: Value::array_empty(),
            random: Box::new(Prng::new()),
            loaded_canonicalized_files,
            symbol_names: HashMap::default(),
            invokers,
            #[cfg(feature = "profile")]
            deopt_stats: HashMap::default(),
            #[cfg(feature = "profile")]
            jit_class_unmatched_stats: HashMap::default(),
            #[cfg(feature = "profile")]
            jit_recompile_count: HashMap::default(),
            #[cfg(feature = "emit-bc")]
            dumped_bc: 1,
        };

        let mut object_class =
            globals.define_builtin_class("Object", OBJECT_CLASS, None, OBJECT_CLASS, ObjTy::OBJECT);
        let basic_object = globals.define_builtin_class(
            "BasicObject",
            BASIC_OBJECT_CLASS,
            None,
            OBJECT_CLASS,
            ObjTy::OBJECT,
        );
        object_class.set_superclass(Some(basic_object));
        // Seed the root of the alloc_func inheritance chain with the default
        // generic allocator. `generate_class_obj` copies this field from the
        // superclass at class-creation time, so every subsequent class picks
        // it up automatically unless a builtin overrides it.
        globals.store[BASIC_OBJECT_CLASS].set_alloc_func(default_alloc_func);
        globals.store[OBJECT_CLASS].set_alloc_func(default_alloc_func);
        globals.set_constant(
            BASIC_OBJECT_CLASS,
            IdentId::get_id("BasicObject"),
            basic_object.get(),
        );
        assert_eq!(
            ENUM_YIELDER_FUNCID,
            globals.define_builtin_func(OBJECT_CLASS, "", enum_yielder, 0)
        );
        assert_eq!(
            YIELDER_FUNCID,
            globals.define_builtin_func_rest(OBJECT_CLASS, "", yielder)
        );
        assert_eq!(
            SYMBOL_TO_PROC_BODY_FUNCID,
            globals.define_builtin_func_with(OBJECT_CLASS, "", symbol_to_proc_body, 1, 1, true)
        );
        globals.random.init_with_seed(None);
        gvar::init_builtin_gvars(&mut globals);
        crate::builtins::init_builtins(&mut globals);
        globals
            .store
            .set_ivar(main_object, IdentId::_NAME, Value::string_from_str("main"))
            .unwrap();

        // load library path
        let load_path = dirs::home_dir()
            .unwrap()
            .join(".monoruby")
            .join("library_path");
        let path_list = match std::fs::read_to_string(&load_path) {
            Ok(s) => s,
            Err(_) => {
                eprintln!(
                    "Warning: failed to read library path file: {:?}. Ruby may not be installed.",
                    load_path
                );
                String::new()
            }
        };
        // prepend monoruby-specific lib directory so it can override CRuby stdlib files
        let monoruby_lib = dirs::home_dir().unwrap().join(".monoruby").join("lib");
        globals.extend_load_path(std::iter::once(monoruby_lib.to_string_lossy().to_string()));
        let list: Vec<_> = path_list.split('\n').map(|s| s.to_string()).collect();
        globals.extend_load_path(list.iter().cloned());

        // set constants
        let pcg_name = env!("CARGO_PKG_NAME");
        let pcg_version = env!("CARGO_PKG_VERSION");
        let val = Value::string(format!("{pcg_name} {pcg_version} [x86_64-linux]",));
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_DESCRIPTION", val);
        let val = Value::string_from_str("ruby");
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_ENGINE", val);

        let version_path = dirs::home_dir()
            .unwrap()
            .join(".monoruby")
            .join("ruby_version");
        let ruby_version = std::fs::read_to_string(&version_path).unwrap();

        let val = Value::string_from_str(&ruby_version.trim());
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_VERSION", val);
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_ENGINE_VERSION", val);

        globals
    }

    pub fn new_test() -> Self {
        Globals::new(1, false, true)
    }

    pub fn locals_len(&self, func_id: FuncId) -> usize {
        match self.store[func_id].kind {
            FuncKind::ISeq(info) => self.store[info].locals.len(),
            _ => 0,
        }
    }

    pub fn run(&mut self, code: impl Into<String>, path: &std::path::Path) -> Result<Value> {
        let code = code.into();
        let program_name = path.to_string_lossy().to_string();
        let mut executor = Executor::init(self, &program_name)?;
        executor.init_stack_limit(self);
        let res = executor.exec_script(self, code, path);
        let _ = self.flush_stdout();
        #[cfg(any(feature = "profile", feature = "jit-log"))]
        self.show_stats();
        #[cfg(feature = "gc-log")]
        {
            alloc::ALLOC.with(|alloc| {
                eprintln!("garbage collector profile:");
                eprintln!(
                    "total allocated objects: {}",
                    alloc.borrow().total_allocated()
                );
                eprintln!(
                    "total gc executed count: {}",
                    alloc.borrow().total_gc_counter()
                );
            });
        }
        res
    }

    pub(crate) fn compile_script_eval(
        &mut self,
        code: String,
        path: impl Into<PathBuf>,
        caller_cfp: Cfp,
        receiver_class: Option<ClassId>,
        lineno: i64,
    ) -> Result<FuncId> {
        let line_offset = lineno - 1;
        // Walk the CFP chain to find the nearest *iseq* (Ruby) frame.
        // `Module#class_eval` / `instance_eval` / `Kernel#eval` may be
        // invoked indirectly through builtin frames (mspec, helpers
        // written in Rust, …), so the immediate caller often isn't a
        // Ruby method itself. CRuby's `rb_eval_string` likewise looks
        // up the nearest cref/binding rather than failing at the first
        // C frame.
        //
        // Side-effect note: only the *outer scope* (lexical
        // ancestors + visible locals) is taken from this iseq;
        // arguments, self, and the lexical-class override (`class_eval`
        // pushing the receiver as the eval cref) come from the caller
        // we were originally given.
        let outer = {
            let mut frame = Some(caller_cfp);
            loop {
                match frame {
                    Some(cfp) => match self.store[cfp.lfp().func_id()].is_iseq() {
                        Some(iseq) => break Some(iseq),
                        None => frame = cfp.prev(),
                    },
                    None => break None,
                }
            }
        };
        let outer = match outer {
            Some(iseq) => iseq,
            None => {
                return Err(MonorubyErr::runtimeerr(
                    "eval requires a Ruby method context",
                ));
            }
        };
        let external_context = self.store.scoped_locals(outer);

        match Parser::parse_program_eval(code, path.into(), Some(&external_context), line_offset) {
            Ok(result) => {
                let fid =
                    bytecodegen::bytecode_compile_eval(self, result, outer, Loc::default(), None)?;
                if let Some(class_id) = receiver_class {
                    if let Some(info) = self.store.iseq_mut(fid) {
                        info.lexical_context.push(class_id);
                    }
                }
                #[cfg(feature = "emit-bc")]
                self.dump_bc();
                Ok(fid)
            }
            Err(err) => Err(MonorubyErr::parse(err)),
        }
    }

    pub fn compile_script_binding(
        &mut self,
        code: String,
        path: impl Into<PathBuf>,
        binding: Binding,
        lineno: i64,
    ) -> Result<()> {
        let line_offset = lineno - 1;
        let outer_fid = binding.outer_fid();
        let outer = match self.store[outer_fid].is_iseq() {
            Some(iseq) => iseq,
            None => {
                return Err(MonorubyErr::runtimeerr(
                    "eval with binding requires a Ruby method context",
                ));
            }
        };
        let external_context = self.store.scoped_locals(outer);

        let context = if let Some(fid) = binding.func_id() {
            let mut lvar = LvarCollector::new();
            for (name, _) in &self.store.iseq(fid).locals {
                lvar.insert(&name.get_name());
            }
            Some(lvar)
        } else {
            None
        };

        let fid = match Parser::parse_program_binding(
            code,
            path.into(),
            context.clone(),
            Some(&external_context),
            line_offset,
        ) {
            Ok(res) => {
                let res =
                    bytecodegen::bytecode_compile_eval(self, res, outer, Loc::default(), context);
                #[cfg(feature = "emit-bc")]
                self.dump_bc();
                res
            }
            Err(err) => Err(MonorubyErr::parse(err)),
        }?;
        self.new_binding_frame(fid, binding.self_val(), binding);
        Ok(())
    }

    pub(crate) fn get_func_data(&mut self, func_id: FuncId) -> &FuncData {
        let info = &self.store[func_id];
        assert!(info.codeptr().is_some());
        info.data_ref()
    }

    ///
    /// Set GC enable flag.
    ///
    /// ### return
    /// GC enable flag before set.
    ///
    pub fn gc_enable(flag: bool) -> bool {
        alloc::ALLOC.with(|alloc| {
            let old = alloc.borrow().gc_enabled;
            alloc.borrow_mut().gc_enabled = flag;
            old
        })
    }

    pub fn flush_stdout(&mut self) -> Result<()> {
        self.stdout
            .flush()
            .map_err(|e| MonorubyErr::runtimeerr(format!("flush: {}", e)))
    }

    pub fn write_stdout(&mut self, bytes: &[u8]) -> Result<()> {
        self.stdout
            .write_all(bytes)
            .map_err(|e| MonorubyErr::runtimeerr(format!("write: {}", e)))
    }

    pub fn print_value(&mut self, val: Value) -> Result<()> {
        if let Some(s) = val.is_rstring() {
            self.stdout.write_all(&s)
        } else {
            let v = val.to_s(&self.store).into_bytes();
            self.stdout.write_all(&v)
        }
        .map_err(|e| MonorubyErr::runtimeerr(format!("write: {}", e)))
    }

    // Handling global variables.
    //
    // The plain `set_gvar` / `get_gvar` methods skip hooks and directly touch
    // `Simple` entries. They are used for call sites that assign to pre-known
    // plain globals like `$0`, `$*`, `$!`, where hooking is not involved.
    //
    // The hook-aware entry point is [`GvarTable::get`] / [`GvarTable::set`],
    // which takes `&mut Executor` and is used by the bytecode/JIT runtime
    // trampolines for `LoadGvar` / `StoreGvar`.

    pub fn set_gvar(&mut self, name: IdentId, val: Value) {
        self.gvars.set_simple(name, val);
    }

    pub fn get_gvar(&mut self, name: IdentId) -> Option<Value> {
        self.gvars.get_simple(name)
    }

    /// Register a getter / setter pair for a global variable name.
    ///
    /// `setter == None` makes the variable read-only.
    pub fn define_hooked_variable(
        &mut self,
        name: IdentId,
        getter: GvarGetter,
        setter: Option<GvarSetter>,
    ) {
        self.gvars.define_hook(name, getter, setter);
    }

    /// Alias `new_name` to `old_name`. Subsequent reads and writes of
    /// `new_name` are forwarded to `old_name`'s entry.
    pub fn alias_global_variable(&mut self, new_name: IdentId, old_name: IdentId) {
        self.gvars.define_alias(new_name, old_name);
    }

    ///
    /// Create new heap binding frame with *fid* and *self_val*.
    ///
    /// local variables are copied from *binding_lfp* if any.
    ///
    fn new_binding_frame(&mut self, fid: FuncId, self_val: Value, mut binding: Binding) {
        let meta = self.store[fid].meta();
        let mut lfp = Lfp::heap_frame(self_val, meta);
        lfp.set_outer(Some(binding.outer_lfp()));
        if let Some(binding_lfp) = binding.binding() {
            let locals_len = self.locals_len(binding_lfp.func_id());
            for i in SlotId(1)..SlotId(1) + locals_len {
                let v = binding_lfp.register(i);
                // SAFETY: Setting register values during frame initialization.
                // The slot index is within bounds (1..locals_len).
                unsafe { lfp.set_register(i, v) }
            }
        }
        binding.set_inner(lfp);
    }

    ///
    /// Create new heap frame with *fid* and *self_val*.
    ///
    /// local variables are copied from *binding_lfp* if any.
    ///
    pub fn new_heap_frame(
        &mut self,
        fid: FuncId,
        self_val: Value,
        binding_lfp: Option<Lfp>,
    ) -> Lfp {
        let meta = self.store[fid].meta();
        let mut lfp = Lfp::heap_frame(self_val, meta);
        if let Some(binding_lfp) = binding_lfp {
            lfp.set_outer(binding_lfp.outer());
            let locals_len = self.locals_len(binding_lfp.func_id());
            for i in SlotId(1)..SlotId(1) + locals_len {
                let v = binding_lfp.register(i);
                // SAFETY: Setting register values during frame initialization.
                // The slot index is within bounds (1..locals_len).
                unsafe { lfp.set_register(i, v) }
            }
        }
        lfp
    }

    // Handling library load path.

    pub fn get_load_path(&self) -> Value {
        self.load_path
    }

    pub fn extend_load_path(&mut self, iter: impl Iterator<Item = String>) {
        self.load_path.as_array().extend(iter.map(Value::string));
    }

    pub(crate) fn get_loaded_features(&self) -> Value {
        let iter = self
            .loaded_canonicalized_files
            .iter()
            .map(|s| Value::string_from_str(s.to_string_lossy().as_ref()));
        Value::array_from_iter(iter)
    }

    pub(crate) fn current_source_path(&self, executor: &Executor) -> &std::path::Path {
        let source_func_id = executor.cfp().get_source_pos();
        &self.store.iseq(source_func_id).sourceinfo.path
    }

    /// ## ABI of JIT-compiled code.
    ///
    /// ### argument registers:
    ///  - rdi: number pf args
    ///
    /// ### global registers:
    ///  - rbx: &mut Interp
    ///  - r12: &mut Globals
    ///  - r13: pc (dummy for JIT-ed code)
    ///
    /// ## stack layout when just after the code is called
    /// ~~~text
    ///       +-------------+
    /// -0x00 | return addr | <- rsp
    ///       +-------------+
    /// -0x08 |  (old rbp)  |
    ///       +-------------+
    /// -0x10 |    meta     |
    ///       +-------------+
    /// -0x18 |     %0      |
    ///       +-------------+
    /// -0x20 | %1(1st arg) |
    ///       +-------------+
    ///       |             |
    /// ~~~~
    ///
    ///  - meta and arguments is set by caller.
    ///  - (old rbp) is to be set by callee.
    ///
    pub(crate) fn gen_wrapper(&mut self, func_id: FuncId) {
        CODEGEN.with(|codegen| {
            let mut codegen = codegen.borrow_mut();
            #[cfg(feature = "perf")]
            let pair = codegen.get_address_pair();
            let entry = codegen.gen_wrapper(self, func_id);
            let codeptr = codegen.jit.get_label_address(&entry);
            self.store[func_id].set_entry(entry, codeptr);
            #[cfg(feature = "perf")]
            {
                let info = codegen.get_wrapper_info(pair);
                self.store[func_id].set_wrapper_info(info);
            }
        });
    }

    pub(crate) fn class_version_inc() {
        CODEGEN.with(|codegen| codegen.borrow_mut().class_version_inc());
    }

    pub(crate) fn class_version() -> u32 {
        CODEGEN.with(|codegen| codegen.borrow().class_version())
    }

    /// Bump the global *constant* version counter, invalidating every
    /// JIT/VM constant lookup cache. Call this whenever the *resolution*
    /// of an existing constant might change without the constant itself
    /// being assigned — for example after `Module#include` /
    /// `Module#prepend` adds a new iclass to a class chain that earlier
    /// callers have already resolved against.
    pub(crate) fn const_version_inc() {
        CODEGEN.with(|codegen| codegen.borrow_mut().const_version_inc());
    }

    pub fn set_constant(&mut self, class_id: ClassId, name: IdentId, val: Value) {
        CODEGEN.with(|codegen| codegen.borrow_mut().const_version_inc());
        self.store.set_constant(class_id, name, val);
    }

    pub fn remove_constant(&mut self, class_id: ClassId, name: IdentId) -> Option<Value> {
        CODEGEN.with(|codegen| codegen.borrow_mut().const_version_inc());
        self[class_id].remove_constant(name)
    }
}

impl Globals {
    #[cfg(feature = "profile")]
    pub fn clear_stats(&mut self) {
        self.deopt_stats.clear();
        self.jit_class_unmatched_stats.clear();
        self.jit_recompile_count.clear();
        self.store.clear_stats();
    }

    #[cfg(feature = "profile")]
    pub fn countup_recompile(
        &mut self,
        func_id: FuncId,
        class_id: ClassId,
        reason: &RecompileReason,
    ) {
        match self
            .jit_recompile_count
            .get_mut(&(func_id, class_id, *reason))
        {
            Some(c) => *c += 1,
            None => {
                self.jit_recompile_count
                    .insert((func_id, class_id, *reason), 1);
            }
        };
    }
}

impl Globals {
    pub(crate) fn generate_range(
        &mut self,
        start: Value,
        end: Value,
        exclude_end: bool,
    ) -> Result<Value> {
        if start.is_nil()
            || end.is_nil()
            || start.is_linear() && end.is_linear()
            || start.real_class(&self.store).id() == end.real_class(&self.store).id()
        {
            return Ok(Value::range(start, end, exclude_end));
        }
        return Err(MonorubyErr::bad_range(start, end));
    }
}

// Random generator
impl Globals {
    pub(crate) fn random_seed(&self) -> i32 {
        self.random.seed
    }

    pub(crate) fn random_init(&mut self, seed: Option<i64>) {
        self.random.init_with_seed(seed)
    }

    pub(crate) fn random_gen<T>(&mut self) -> T
    where
        rand::distr::StandardUniform: rand::prelude::Distribution<T>,
    {
        self.random.random()
    }
}
