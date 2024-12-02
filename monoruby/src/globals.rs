use ruruby_parse::{BlockInfo, Loc, LvarCollector, Node, ParamKind, Parser, SourceInfoRef};
use std::io::{stdout, BufWriter, Stdout};
use std::io::{Read, Write};
use std::path::PathBuf;
use std::sync::atomic::AtomicU8;

use super::*;

mod dump;
mod error;
mod method;
mod prng;
mod require;
mod store;
#[cfg(any(feature = "deopt", feature = "profile"))]
pub(crate) use dump::log_deoptimize;
pub use error::*;
use prng::*;
pub use store::*;

pub static WARNING: std::sync::LazyLock<AtomicU8> = std::sync::LazyLock::new(|| AtomicU8::new(0u8));

pub(crate) type InlineGen =
    dyn Fn(&mut jitgen::asmir::AsmIr, &Store, &mut jitgen::BBContext, CallSiteId, BytecodePtr);

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct MethodTableEntry {
    owner: ClassId,
    func_id: Option<FuncId>,
    visibility: Visibility,
    is_basic_op: bool,
}

impl MethodTableEntry {
    pub fn func_id(&self) -> Option<FuncId> {
        self.func_id
    }

    pub fn owner(&self) -> ClassId {
        self.owner
    }
}

pub(crate) const GLOBALS_FUNCINFO: usize =
    std::mem::offset_of!(Globals, store.functions.info) + MONOVEC_PTR;
pub(crate) const OBJECT_SEND_FUNCID: FuncId = FuncId::new(3);

#[derive(Clone, Debug)]
pub(crate) struct ExternalContext {
    scope: Vec<(IndexMap<IdentId, bytecodegen::BcLocal>, Option<IdentId>)>,
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
    type Output = (IndexMap<IdentId, bytecodegen::BcLocal>, Option<IdentId>);
    fn index(&self, index: usize) -> &Self::Output {
        &self.scope[index]
    }
}

impl ExternalContext {
    pub fn new() -> Self {
        Self { scope: vec![] }
    }

    pub fn one(locals: IndexMap<IdentId, bytecodegen::BcLocal>, block: Option<IdentId>) -> Self {
        Self {
            scope: vec![(locals, block)],
        }
    }

    pub fn is_empty(&self) -> bool {
        self.scope.is_empty()
    }

    pub fn extend_from_slice(&mut self, other: &Self) {
        self.scope.extend_from_slice(&other.scope);
    }
}

///
/// Global state.
///
pub struct Globals {
    /// main object (`self`` of toplevel).
    pub main_object: Value,
    /// function and class info.
    pub(crate) store: Store,
    /// code generator.
    pub codegen: Codegen,
    /// globals variables.
    global_vars: HashMap<IdentId, Value>,

    /// suppress jit compilation.
    no_jit: bool,
    /// suppress loading gem.
    pub no_gems: bool,
    /// stdout.
    stdout: BufWriter<Stdout>,
    /// library directries.
    load_path: Value,
    /// standard PRNG
    random: Box<Prng>,
    /// loaded libraries (canonical path).
    loaded_canonicalized_files: IndexSet<PathBuf>,
    pub(super) startup_flag: bool,
    /// stats for deoptimization
    #[cfg(feature = "profile")]
    deopt_stats: HashMap<(FuncId, bytecodegen::BcIndex), usize>,
    #[cfg(feature = "profile")]
    jit_class_unmatched_stats: HashMap<(FuncId, ClassId), usize>,
    #[cfg(feature = "emit-bc")]
    dumped_bc: usize,
}

impl std::ops::Index<FuncId> for Globals {
    type Output = FuncInfo;
    fn index(&self, index: FuncId) -> &FuncInfo {
        &self.store[index]
    }
}

impl std::ops::IndexMut<FuncId> for Globals {
    fn index_mut(&mut self, index: FuncId) -> &mut FuncInfo {
        &mut self.store[index]
    }
}

impl alloc::GC<RValue> for Globals {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.main_object.mark(alloc);
        self.load_path.mark(alloc);
        self.store.mark(alloc);
        self.global_vars.values().for_each(|v| v.mark(alloc));
    }
}

impl Globals {
    pub fn new(warning: u8, no_jit: bool, no_gems: bool) -> Self {
        assert_eq!(64, std::mem::size_of::<FuncInfo>());

        WARNING.store(warning, std::sync::atomic::Ordering::Relaxed);

        let main_object = Value::object(OBJECT_CLASS);

        let mut globals = Self {
            main_object,
            codegen: Codegen::new(no_jit),
            store: Store::new(),
            global_vars: HashMap::default(),
            no_jit,
            no_gems,
            stdout: BufWriter::new(stdout()),
            load_path: Value::array_empty(),
            random: Box::new(Prng::new()),
            loaded_canonicalized_files: IndexSet::default(),
            startup_flag: false,
            #[cfg(feature = "profile")]
            deopt_stats: HashMap::default(),
            #[cfg(feature = "profile")]
            jit_class_unmatched_stats: HashMap::default(),
            #[cfg(feature = "emit-bc")]
            dumped_bc: 1,
        };

        let mut object_class =
            globals.define_builtin_class_by_str("Object", OBJECT_CLASS, None, OBJECT_CLASS);
        let basic_object = globals.define_builtin_class_by_str(
            "BasicObject",
            BASIC_OBJECT_CLASS,
            None,
            OBJECT_CLASS,
        );
        object_class.set_superclass(Some(basic_object));
        assert_eq!(
            FuncId::new(1),
            globals.define_builtin_func(OBJECT_CLASS, "", enum_yielder, 0)
        );
        assert_eq!(
            FuncId::new(2),
            globals.define_builtin_func_rest(OBJECT_CLASS, "", yielder)
        );
        assert_eq!(
            OBJECT_SEND_FUNCID,
            globals.define_builtin_inline_funcs_with(
                OBJECT_CLASS,
                "send",
                &["__send__"],
                crate::builtins::send,
                Box::new(crate::builtins::object_send),
                0,
                0,
                true,
            )
        );
        globals.random.init_with_seed(None);
        crate::builtins::init_builtins(&mut globals);
        globals
            .set_ivar(main_object, IdentId::_NAME, Value::string_from_str("main"))
            .unwrap();

        // load library path
        let load_path = dirs::home_dir()
            .unwrap()
            .join(".monoruby")
            .join("library_path");
        let path_list = std::fs::read_to_string(&load_path).unwrap();
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

        let val = Value::string(ruby_version);
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_VERSION", val);
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_ENGINE_VERSION", val);
        globals
    }

    pub fn new_test() -> Self {
        Globals::new(1, false, true)
    }

    pub fn run(&mut self, code: impl Into<String>, path: &std::path::Path) -> Result<Value> {
        let code = code.into();
        let mut executor = Executor::init(self);
        let res = executor.exec_script(self, code, path);
        self.flush_stdout();
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
    ) -> Result<FuncId> {
        let outer_fid = caller_cfp.lfp().meta().func_id();
        let mother = caller_cfp.method_func_id_depth();
        let mut ex_scope = IndexMap::default();
        for (name, idx) in &self[outer_fid].as_ruby_func().locals {
            ex_scope.insert(*name, *idx);
        }
        let mut external_context = ExternalContext::one(ex_scope, None);
        external_context.extend_from_slice(&self[outer_fid].as_ruby_func().outer_locals);

        match Parser::parse_program_eval(code, path.into(), Some(&external_context)) {
            Ok(res) => {
                let res = bytecodegen::bytecode_compile_eval(
                    self,
                    res,
                    mother,
                    (outer_fid, external_context),
                    Loc::default(),
                    None,
                );
                #[cfg(feature = "emit-bc")]
                self.dump_bc();
                res
            }
            Err(err) => Err(MonorubyErr::parse(err)),
        }
    }

    pub fn compile_script_binding(
        &mut self,
        code: String,
        path: impl Into<PathBuf>,
        binding: Binding,
    ) -> Result<()> {
        let outer_fid = binding.outer_lfp().meta().func_id();
        let (lfp, outer) = binding.outer_lfp().outermost_lfp_depth();
        let mother = (lfp.meta().func_id(), outer);
        let mut ex_scope = IndexMap::default();
        for (name, idx) in &self[outer_fid].as_ruby_func().locals {
            ex_scope.insert(*name, *idx);
        }
        let mut external_context = ExternalContext::one(ex_scope, None);
        external_context.extend_from_slice(&self[outer_fid].as_ruby_func().outer_locals);

        let context = if let Some(fid) = binding.func_id() {
            let mut lvar = LvarCollector::new();
            for (name, _) in &self[fid].as_ruby_func().locals {
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
        ) {
            Ok(res) => {
                let res = bytecodegen::bytecode_compile_eval(
                    self,
                    res,
                    mother,
                    (outer_fid, external_context),
                    Loc::default(),
                    context,
                );
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
        let info = &self[func_id];
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

    pub fn flush_stdout(&mut self) {
        self.stdout.flush().unwrap();
    }

    pub fn write_stdout(&mut self, bytes: &[u8]) {
        self.stdout.write_all(bytes).unwrap();
    }

    pub fn print_value(&mut self, val: Value) {
        if let Some(s) = val.is_bytes() {
            self.stdout.write_all(s)
        } else {
            let v = val.to_s(self).into_bytes();
            self.stdout.write_all(&v)
        }
        .unwrap();
    }

    // Handling global variable

    pub fn set_gvar(&mut self, name: IdentId, val: Value) {
        self.global_vars.insert(name, val);
    }

    pub fn get_gvar(&mut self, name: IdentId) -> Option<Value> {
        self.global_vars.get(&name).cloned()
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
            let locals_len = self[binding_lfp.meta().func_id()].locals_len();
            for i in 1..1 + locals_len {
                let v = binding_lfp.register(i);
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
            let locals_len = self[binding_lfp.meta().func_id()].locals_len();
            for i in 1..1 + locals_len {
                let v = binding_lfp.register(i);
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
        &self[source_func_id].as_ruby_func().sourceinfo.path
    }

    pub(crate) fn func_description(&self, func_id: FuncId) -> String {
        let info = &self[func_id];
        if let Some(func) = info.is_ruby_func() {
            let mother = func.mother.0;
            if mother != func_id {
                format!("block in {}", self.func_description(mother))
            } else {
                match info.owner_class() {
                    Some(owner) => format!("{:?}#{}", owner.get_name_id(self), func.name()),
                    None => func.name().to_string(),
                }
            }
        } else {
            let name = if let Some(name) = info.name() {
                format!("{:?}", name)
            } else {
                String::new()
            };
            match info.owner_class() {
                Some(owner) => format!("{:?}#{name}", owner.get_name_id(self)),
                None => name.to_string(),
            }
        }
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
        #[cfg(feature = "perf")]
        let pair = self.codegen.get_address_pair();
        let kind = self[func_id].kind.clone();
        let entry = self.codegen.gen_wrapper(&kind, self.no_jit);
        let codeptr = self.codegen.jit.get_label_address(entry);
        self[func_id].set_entry(entry, codeptr);
        #[cfg(feature = "perf")]
        {
            let info = self.codegen.get_wrapper_info(pair);
            self[func_id].set_wrapper_info(info);
        }
    }

    pub(crate) fn class_version_inc(&mut self) {
        self.codegen.class_version_inc()
    }

    pub(crate) fn class_version(&self) -> u32 {
        self.codegen.class_version()
    }
}

impl Globals {
    #[cfg(feature = "profile")]
    pub fn clear_stats(&mut self) {
        self.deopt_stats.clear();
        self.store.clear_stats();
        self.jit_class_unmatched_stats.clear();
    }
}

impl Globals {
    /// Get class name of *ClassId*.
    pub(crate) fn get_class_name(&self, class: impl Into<Option<ClassId>>) -> String {
        if let Some(class) = class.into() {
            let class_obj = self.store.classes[class].get_module();
            match self.store.classes[class].get_name_id() {
                Some(_) => {
                    let v: Vec<_> = self
                        .store
                        .classes
                        .get_parents(class)
                        .into_iter()
                        .rev()
                        .map(|name| name.to_string())
                        .collect();
                    v.join("::")
                }
                None => match class_obj.is_singleton() {
                    None => format!("#<Class:{:016x}>", class_obj.as_val().id()),
                    Some(base) => format!("#<Class:{}>", base.to_s(self)),
                },
            }
        } else {
            "<INVALID>".to_string()
        }
    }

    pub(crate) fn generate_range(
        &mut self,
        start: Value,
        end: Value,
        exclude_end: bool,
    ) -> Result<Value> {
        if start.real_class(self).id() != end.real_class(self).id() {
            return Err(MonorubyErr::bad_range(start, end));
        }
        Ok(Value::range(start, end, exclude_end))
    }
}

// Random generator
impl Globals {
    pub(crate) fn random_seed(&self) -> &[u8; 4] {
        &self.random.seed
    }

    pub(crate) fn random_init(&mut self, seed: Option<i64>) {
        self.random.init_with_seed(seed)
    }

    pub(crate) fn random_gen<T>(&mut self) -> T
    where
        rand::distributions::Standard: rand::prelude::Distribution<T>,
    {
        self.random.gen()
    }
}
