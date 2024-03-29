use fancy_regex::Regex;
use monoasm::CodePtr;
use ruruby_parse::{BlockInfo, Loc, Node, ParamKind, Parser, SourceInfoRef};
use std::io::{stdout, BufWriter, Stdout};
use std::io::{Read, Write};
use std::path::PathBuf;
use std::rc::Rc;

use super::*;

mod dump;
mod error;
mod method;
mod prng;
mod store;
#[cfg(any(feature = "deopt", feature = "profile"))]
pub(crate) use dump::log_deoptimize;
pub use error::*;
use prng::*;
pub use store::*;

pub(crate) type InlineGen =
    dyn Fn(&mut jitgen::asmir::AsmIr, &Store, &mut jitgen::BBContext, &CallSiteInfo, BcPc);
pub(crate) type InlineAnalysis = fn(&mut analysis::SlotInfo, &CallSiteInfo);

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct MethodTableEntry {
    owner: ClassId,
    func_id: Option<FuncId>,
    visibility: Visibility,
}

impl MethodTableEntry {
    pub fn func_id(&self) -> FuncId {
        self.func_id.unwrap()
    }
}

pub(crate) const GLOBALS_FUNCINFO: usize =
    std::mem::offset_of!(Globals, store.functions.info) + MONOVEC_PTR;
pub(crate) const OBJECT_SEND_FUNCID: FuncId = FuncId::new(3);

#[derive(Clone, Debug)]
pub(crate) struct ExternalContext {
    scope: Vec<(HashMap<IdentId, bytecodegen::BcLocal>, Option<IdentId>)>,
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
    type Output = (HashMap<IdentId, bytecodegen::BcLocal>, Option<IdentId>);
    fn index(&self, index: usize) -> &Self::Output {
        &self.scope[index]
    }
}

impl ExternalContext {
    pub fn new() -> Self {
        Self { scope: vec![] }
    }

    pub fn one(locals: HashMap<IdentId, bytecodegen::BcLocal>, block: Option<IdentId>) -> Self {
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
    /// function and class info.
    pub(crate) store: Store,
    /// code generator.
    pub codegen: Codegen,
    /// globals variables.
    global_vars: HashMap<IdentId, Value>,
    /// global method cache.
    global_method_cache: GlobalMethodCache,
    /// regex cache.
    regexp_cache: HashMap<String, Rc<Regex>>,
    /// warning level.
    pub(super) warning: u8,
    /// suppress jit compilation.
    no_jit: bool,
    /// stdout.
    stdout: BufWriter<Stdout>,
    /// library directries.
    lib_directories: Vec<String>,
    /// standard PRNG
    random: Box<Prng>,
    /// loaded libraries (canonical path).
    loaded_canonicalized_files: IndexSet<PathBuf>,
    /// stats for deoptimization
    #[cfg(feature = "profile")]
    deopt_stats: HashMap<(FuncId, usize), usize>,
    /// stats for inline method cache miss
    #[cfg(feature = "profile")]
    global_method_cache_stats: HashMap<(ClassId, IdentId), usize>,
    /// stats for method cache miss
    #[cfg(feature = "profile")]
    method_exploration_stats: HashMap<(ClassId, IdentId), usize>,
    #[cfg(feature = "profile")]
    jit_class_unmatched_stats: HashMap<(FuncId, ClassId), usize>,
    #[cfg(feature = "emit-bc")]
    dumped_bc: usize,
    #[cfg(feature = "emit-bc")]
    pub(super) startup_flag: bool,
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
        self.store.mark(alloc);
        self.global_vars.values().for_each(|v| v.mark(alloc));
    }
}

impl Globals {
    pub fn new(warning: u8, no_jit: bool) -> Self {
        assert_eq!(64, std::mem::size_of::<FuncInfo>());
        let main_object = Value::object(OBJECT_CLASS);

        let mut globals = Self {
            codegen: Codegen::new(no_jit, main_object),
            store: Store::new(),
            global_vars: HashMap::default(),
            global_method_cache: GlobalMethodCache::default(),
            regexp_cache: HashMap::default(),
            warning,
            no_jit,
            stdout: BufWriter::new(stdout()),
            lib_directories: vec![
                "/home/monochrome/.rbenv/versions/3.3.0/lib/ruby/gems/3.3.0/gems/json-2.7.1/lib/"
                    .to_string(),
            ],
            random: Box::new(Prng::new()),
            loaded_canonicalized_files: IndexSet::default(),
            #[cfg(feature = "profile")]
            deopt_stats: HashMap::default(),
            #[cfg(feature = "profile")]
            global_method_cache_stats: HashMap::default(),
            #[cfg(feature = "profile")]
            method_exploration_stats: HashMap::default(),
            #[cfg(feature = "profile")]
            jit_class_unmatched_stats: HashMap::default(),
            #[cfg(feature = "emit-bc")]
            dumped_bc: 1,
            #[cfg(feature = "emit-bc")]
            startup_flag: false,
        };
        globals.define_builtin_class_by_str("Object", OBJECT_CLASS, None, OBJECT_CLASS);
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
                analysis::v_v_vv,
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
        let load_path = include_str!(concat!(env!("OUT_DIR"), "/libpath.rb"));
        let nodes = Parser::parse_program(load_path.to_string(), PathBuf::new())
            .unwrap()
            .node;

        let lib = Array::new(Value::from_ast2(&nodes));
        globals.extend_load_path(lib.iter().map(|v| v.as_str().to_string()));

        // load gem library path
        let load_path = include_str!(concat!(env!("OUT_DIR"), "/gempath.rb"));
        let nodes = Parser::parse_program(load_path.to_string(), PathBuf::new())
            .unwrap()
            .node;

        let lib = Array::new(Value::from_ast2(&nodes));
        globals.extend_load_path(lib.iter().map(|v| v.as_str().to_string()));

        // set constants
        let pcg_name = env!("CARGO_PKG_NAME");
        let pcg_version = env!("CARGO_PKG_VERSION");
        let description = format!("{pcg_name} {pcg_version} [x86_64-linux]",);
        let val = Value::string_from_str(&description);
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_DESCRIPTION", val);
        let val = Value::string_from_str(pcg_name);
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_ENGINE", val);
        let val = Value::string_from_str("3.3.0");
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_VERSION", val);
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_ENGINE_VERSION", val);
        globals
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
        let mut ex_scope = HashMap::default();
        for (name, idx) in &self[outer_fid].as_ruby_func().locals {
            ex_scope.insert(*name, *idx);
        }
        let mut external_context = ExternalContext::one(ex_scope, None);
        external_context.extend_from_slice(&self[outer_fid].as_ruby_func().outer_locals);

        match Parser::parse_program_eval(code, path.into(), Some(&external_context)) {
            Ok(res) => {
                let res = bytecodegen::compile_eval(
                    self,
                    res.node,
                    mother,
                    (outer_fid, external_context),
                    Loc::default(),
                    res.source_info,
                );
                #[cfg(feature = "emit-bc")]
                self.dump_bc();
                res
            }
            Err(err) => Err(MonorubyErr::parse(err)),
        }
    }

    pub(crate) fn get_func_data(&mut self, func_id: FuncId) -> &FuncData {
        let info = &self[func_id];
        assert!(info.codeptr().is_some());
        info.data_ref()
    }

    pub fn gc_enable(flag: bool) {
        alloc::ALLOC.with(|alloc| alloc.borrow_mut().gc_enabled = flag);
    }

    pub(crate) fn flush_stdout(&mut self) {
        self.stdout.flush().unwrap();
    }

    pub(crate) fn write_stdout(&mut self, bytes: &[u8]) {
        self.stdout.write_all(bytes).unwrap();
    }

    // Handling global variable

    pub(crate) fn set_gvar(&mut self, name: IdentId, val: Value) {
        self.global_vars.insert(name, val);
    }

    pub(crate) fn get_gvar(&mut self, name: IdentId) -> Option<Value> {
        self.global_vars.get(&name).cloned()
    }

    // Handling regex.

    pub(crate) fn set_regex(&mut self, k: String, v: Rc<Regex>) -> Option<Rc<Regex>> {
        self.regexp_cache.insert(k, v)
    }

    pub(crate) fn get_regex(&self, k: &str) -> Option<&Rc<Regex>> {
        self.regexp_cache.get(k)
    }

    // Handling library load path.

    pub fn get_load_path(&self) -> Value {
        let iter = self
            .lib_directories
            .iter()
            .map(|s| Value::string_from_str(s));
        Value::array_from_iter(iter)
    }

    pub fn extend_load_path(&mut self, iter: impl Iterator<Item = String>) {
        self.lib_directories.extend(iter)
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

    ///
    /// Load external library.
    ///
    pub(crate) fn load_lib(
        &mut self,
        file_name: &std::path::Path,
        is_relative: bool,
    ) -> Result<Option<(String, std::path::PathBuf)>> {
        if !is_relative {
            for lib in self.lib_directories.clone() {
                let mut lib = std::path::PathBuf::from(lib);
                lib.push(file_name);
                lib.set_extension("rb");
                if lib.exists() {
                    return self.load_file(lib);
                }
                lib.set_extension("so");
                if lib.exists() {
                    eprintln!("Warning: currently, can not require .so file. {:?}", lib);
                }
            }
            Err(MonorubyErr::cant_load(None, file_name))
        } else {
            if file_name.exists() {
                return self.load_file(file_name.into());
            }
            Err(MonorubyErr::cant_load(None, file_name))
        }
    }

    pub(crate) fn func_description(&self, func_id: FuncId) -> String {
        let info = &self[func_id];
        if let Some(func) = info.is_ruby_func() {
            let mother = func.mother.0;
            if mother != func_id {
                format!("block in {}", self.func_description(mother))
            } else {
                match info.owner_class() {
                    Some(owner) => format!(
                        "{}#{}",
                        match owner.get_name_id(self) {
                            Some(name) => format!("{:?}", name),
                            None => "<unnamed>".to_string(),
                        },
                        func.name()
                    ),
                    None => format!("{}", func.name()),
                }
            }
        } else {
            let name = if let Some(name) = info.name() {
                format!("{:?}", name)
            } else {
                String::new()
            };
            match info.owner_class() {
                Some(owner) => format!(
                    "{}#{name}",
                    match owner.get_name_id(self) {
                        Some(name) => format!("{:?}", name),
                        None => "<unnamed>".to_string(),
                    },
                ),
                None => format!("{name}"),
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
    pub(crate) fn gen_wrapper(&mut self, func_id: FuncId) -> CodePtr {
        #[cfg(feature = "perf")]
        let pair = self.codegen.get_address_pair();
        let kind = self[func_id].kind.clone();
        let codeptr = self.codegen.gen_wrapper(kind, self.no_jit);
        self[func_id].set_codeptr(codeptr);
        #[cfg(feature = "perf")]
        {
            let info = self.codegen.get_wrapper_info(pair);
            self[func_id].set_wrapper_info(info);
        }
        codeptr
    }

    pub(crate) fn class_version_inc(&mut self) {
        self.codegen.class_version_inc()
    }

    pub(crate) fn class_version(&self) -> u32 {
        self.codegen.class_version()
    }

    ///
    /// Load the library if it has never been loaded before.
    ///
    /// If the library was loaded, return the code and canonical path.
    /// Otherwise, returns Ok(None).
    ///
    /// When an error occured in loading, returns Err.
    ///
    fn load_file(
        &mut self,
        path: std::path::PathBuf,
    ) -> Result<Option<(String, std::path::PathBuf)>> {
        fn load(
            globals: &mut Globals,
            path: std::path::PathBuf,
        ) -> std::result::Result<Option<(String, std::path::PathBuf)>, std::io::Error> {
            let mut file_body = String::new();
            let mut file = std::fs::OpenOptions::new().read(true).open(&path)?;
            file.read_to_string(&mut file_body)?;
            globals.loaded_canonicalized_files.insert(path.clone());
            Ok(Some((file_body, path)))
        }
        let path = match path.canonicalize() {
            Ok(path) => path,
            Err(err) => return Err(MonorubyErr::cant_load(Some(err), &path)),
        };
        if self.loaded_canonicalized_files.get(&path).is_some() {
            return Ok(None);
        }
        load(self, path.clone()).map_err(|err| MonorubyErr::cant_load(Some(err), &path))
    }
}

impl Globals {
    /// Get class name of *ClassId*.
    pub(crate) fn get_class_name(&self, class: impl Into<Option<ClassId>>) -> String {
        if let Some(class) = class.into() {
            let class_obj = class.get_module(self);
            match self.store[class].get_name_id() {
                Some(_) => {
                    let v: Vec<_> = class
                        .get_parents(self)
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

    pub(crate) fn inspect2(&self, val: Value) -> String {
        match val.unpack() {
            RV::Object(rvalue) => rvalue.inspect2(self),
            _ => val.inspect(self),
        }
    }

    pub(crate) fn val_to_bytes(&self, val: Value) -> Vec<u8> {
        if let RV::String(s) = val.unpack() {
            return s.to_vec();
        }
        val.to_s(self).into_bytes()
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
