use fancy_regex::Regex;
use monoasm::DestLabel;
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
#[cfg(any(feature = "log-jit", feature = "profile"))]
pub(crate) use dump::log_deoptimize;
pub use error::*;
use prng::*;
pub use store::*;

pub(crate) type InlineGen =
    fn(&mut Codegen, &mut jitgen::BBContext, &CallSiteInfo, BcPc, DestLabel);
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

pub const GLOBALS_FUNCINFO: usize =
    std::mem::offset_of!(Globals, store) + STORE_FUNCTION + FUNCS_INFO;

///
/// Global state.
///
pub struct Globals {
    /// code generator.
    pub codegen: Codegen,
    /// function and class info.
    pub(crate) store: Store,
    /// globals variables.
    global_vars: HashMap<IdentId, Value>,
    /// global method cache.
    global_method_cache: HashMap<(IdentId, ClassId), (u32, Option<MethodTableEntry>)>,
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
    random: Prng,
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
        let main_object = Value::object(OBJECT_CLASS);

        let mut globals = Self {
            codegen: Codegen::new(no_jit, main_object),
            store: Store::new(),
            global_vars: HashMap::default(),
            global_method_cache: HashMap::default(),
            regexp_cache: HashMap::default(),
            warning,
            no_jit,
            stdout: BufWriter::new(stdout()),
            lib_directories: vec![
                "/home/monochrome/.rbenv/versions/3.3.0-dev/lib/ruby/gems/3.3.0+0/gems/json-2.6.3/lib".to_string(),
                "/home/monochrome/.rbenv/versions/3.3.0-dev/lib/ruby/gems/3.3.0+0/extensions/x86_64-linux/3.3.0+0-static/json-2.6.3".to_string()
            ],
            random: Prng::new(),
            loaded_canonicalized_files: IndexSet::default(),
            #[cfg(feature = "profile")]
            deopt_stats: HashMap::default(),
            #[cfg(feature = "profile")]
            global_method_cache_stats: HashMap::default(),
            #[cfg(feature = "profile")]
            method_exploration_stats: HashMap::default(),
            #[cfg(feature = "emit-bc")]
            dumped_bc: 1,
            #[cfg(feature = "emit-bc")]
            startup_flag: false,
        };
        assert_eq!(
            FuncId::new(1),
            globals.define_builtin_func(OBJECT_CLASS, "", enum_yielder)
        );
        assert_eq!(
            FuncId::new(2),
            globals.define_builtin_func(OBJECT_CLASS, "", yielder)
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

        let lib: Array = Value::from_ast2(&nodes).into();
        globals.extend_load_path(lib.iter().map(|v| v.as_str().to_string()));
        // set constants
        let pcg_name = env!("CARGO_PKG_NAME");
        let pcg_version = env!("CARGO_PKG_VERSION");
        let description = format!("{pcg_name} {pcg_version} [x86_64-linux]",);
        let val = Value::string_from_str(&description);
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_DESCRIPTION", val);
        let val = Value::string_from_str(pcg_name);
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_ENGINE", val);
        let val = Value::string_from_str(pcg_version);
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_VERSION", val);
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_ENGINE_VERSION", val);
        globals
    }

    pub fn run(&mut self, code: impl Into<String>, path: &std::path::Path) -> Result<Value> {
        let code = code.into();
        let mut executor = Executor::init(self);
        let res = executor.exec_script(self, code, path);
        self.flush_stdout();
        #[cfg(feature = "profile")]
        self.show_stats();
        res
    }

    pub fn compile_script_with_binding(
        &mut self,
        code: String,
        path: impl Into<PathBuf>,
        context: Option<ruruby_parse::LvarCollector>,
        extern_context: Option<ruruby_parse::DummyFrame>,
    ) -> Result<FuncId> {
        match Parser::parse_program_binding(code, path.into(), context, extern_context) {
            Ok(res) => bytecodegen::compile_script(self, res.node, res.source_info),
            Err(err) => Err(MonorubyErr::parse(err)),
        }
    }

    pub(crate) fn get_func_data(&mut self, func_id: FuncId) -> &FuncData {
        let data = &self[func_id].data;
        assert!(data.codeptr().is_some());
        data
    }

    pub(crate) fn get_yield_data(&mut self, cfp: CFP) -> Option<Proc> {
        cfp.get_block()
            .map(|bh| Proc::new(self.get_block_data(cfp, bh)))
    }

    pub(crate) fn get_block_data(&mut self, mut cfp: CFP, bh: BlockHandler) -> ProcInner {
        if let Some((func_id, idx)) = bh.try_proxy() {
            for _ in 0..idx {
                cfp = cfp.prev().unwrap();
            }
            let func_data = self.get_func_data(func_id).clone();
            ProcInner::from(cfp.lfp(), func_data)
        } else {
            bh.as_proc().clone()
        }
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
        let kind = self[func_id].kind.clone();
        let codeptr = self.codegen.gen_wrapper(kind, self.no_jit);
        self[func_id].data.set_codeptr(codeptr);
    }

    pub(super) fn class_version_inc(&mut self) {
        unsafe { *self.codegen.class_version_addr += 1 }
    }

    pub fn class_version(&self) -> u32 {
        unsafe { *self.codegen.class_version_addr }
    }

    fn load_file(
        &mut self,
        path: std::path::PathBuf,
    ) -> Result<Option<(String, std::path::PathBuf)>> {
        let path = path.canonicalize().unwrap();
        if !self.loaded_canonicalized_files.insert(path.clone()) {
            return Ok(None);
        }
        let mut file_body = String::new();
        let err = match std::fs::OpenOptions::new().read(true).open(&path) {
            Ok(mut file) => match file.read_to_string(&mut file_body) {
                Ok(_) => return Ok(Some((file_body, path))),
                Err(err) => err,
            },
            Err(err) => err,
        };
        Err(MonorubyErr::cant_load(Some(err), &path))
    }
}

impl Globals {
    pub(crate) fn to_s(&self, val: Value) -> String {
        match val.unpack() {
            RV::None => "Undef".to_string(),
            RV::Nil => "".to_string(),
            RV::Bool(b) => format!("{:?}", b),
            RV::Fixnum(n) => format!("{}", n),
            RV::BigInt(n) => format!("{}", n),
            RV::Float(f) => dtoa::Buffer::new().format(f).to_string(),
            RV::Symbol(id) => id.to_string(),
            RV::String(s) => match String::from_utf8(s.to_vec()) {
                Ok(s) => s,
                Err(_) => format!("{:?}", s),
            },
            RV::Object(rvalue) => match rvalue.ty() {
                ObjKind::CLASS | ObjKind::MODULE => rvalue.as_class_id().get_name(self),
                ObjKind::TIME => rvalue.as_time().to_string(),
                ObjKind::ARRAY => rvalue.as_array().to_s(self),
                ObjKind::OBJECT => self.object_tos(val),
                ObjKind::RANGE => self.range_tos(val),
                ObjKind::PROC => Self::proc_tos(val),
                ObjKind::HASH => self.hash_tos(val),
                ObjKind::REGEXP => Self::regexp_tos(val),
                ObjKind::IO => rvalue.as_io().to_string(),
                ObjKind::EXCEPTION => rvalue.as_exception().msg().to_string(),
                ObjKind::METHOD => rvalue.as_method().to_s(self),
                ObjKind::FIBER => self.fiber_tos(val),
                ObjKind::ENUMERATOR => self.enumerator_tos(val),
                ObjKind::GENERATOR => self.object_tos(val),
                _ => format!("{:016x}", val.id()),
            },
        }
    }

    pub(crate) fn val_to_bytes(&self, val: Value) -> Vec<u8> {
        if let RV::String(s) = val.unpack() {
            return s.to_vec();
        }
        self.to_s(val).into_bytes()
    }

    pub fn inspect(&self, val: Value) -> String {
        match val.unpack() {
            RV::None | RV::Bool(_) | RV::Fixnum(_) | RV::BigInt(_) | RV::Float(_) => {}
            RV::Nil => return "nil".to_string(),
            RV::Symbol(id) => return format!(":{id}"),
            RV::String(s) => {
                return match String::from_utf8(s.to_vec()) {
                    Ok(s) => format!("{:?}", s),
                    Err(_) => format!("{:?}", s),
                }
            }
            RV::Object(rvalue) => match rvalue.ty() {
                ObjKind::OBJECT => return self.object_inspect(val),
                ObjKind::EXCEPTION => {
                    let class_name = val.class().get_name(self);
                    let msg = rvalue.as_exception().msg();
                    return format!("#<{class_name}: {msg}>");
                }
                ObjKind::GENERATOR => return self.object_tos(val),
                _ => {}
            },
        }
        self.to_s(val)
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

    fn proc_tos(val: Value) -> String {
        format!("#<Proc:0x{:016x}>", val.rvalue().id())
    }

    fn object_tos(&self, val: Value) -> String {
        if let Some(name) = self.get_ivar(val, IdentId::_NAME) {
            self.to_s(name)
        } else {
            format!(
                "#<{}:0x{:016x}>",
                val.real_class(self).id().get_name(self),
                val.rvalue().id()
            )
        }
    }

    fn object_inspect(&self, val: Value) -> String {
        if let Some(name) = self.get_ivar(val, IdentId::_NAME) {
            self.to_s(name)
        } else {
            let mut s = String::new();
            for (id, v) in self.get_ivars(val).into_iter() {
                s += &format!(" {id}={}", self.inspect(v));
            }
            format!(
                "#<{}:0x{:016x}{s}>",
                val.class().get_name(self),
                val.rvalue().id()
            )
        }
    }

    fn hash_tos(&self, val: Value) -> String {
        let hash = val.as_hash();
        match hash.len() {
            0 => "{}".to_string(),
            _ => {
                let mut result = "".to_string();
                let mut first = true;
                for (k, v) in hash.iter() {
                    let k_inspect = if k == val {
                        "{...}".to_string()
                    } else {
                        self.inspect(k)
                    };
                    let v_inspect = if v == val {
                        "{...}".to_string()
                    } else {
                        self.inspect(v)
                    };
                    result = if first {
                        format!("{k_inspect}=>{v_inspect}")
                    } else {
                        format!("{result}, {k_inspect}=>{v_inspect}")
                    };
                    first = false;
                }
                format! {"{{{}}}", result}
            }
        }
    }

    fn fiber_tos(&self, val: Value) -> String {
        let fiber: Fiber = val.into();
        let state = match fiber.state() {
            FiberState::Created => "created",
            FiberState::Terminated => "terminated",
            FiberState::Suspended => "suspended",
        };
        let func_id = fiber.func_id();
        format!(
            "#<Fiber:0x{:016x} {} ({state})>",
            val.id(),
            self[func_id].as_ruby_func().get_location(),
        )
    }

    fn enumerator_tos(&self, val: Value) -> String {
        let e: Enumerator = val.into();
        format!("#<Enumerator: {} {}>", self.to_s(e.obj), e.method)
    }

    fn regexp_tos(val: Value) -> String {
        let regexp = val.is_regex().unwrap();
        format!("/{}/", regexp.as_str())
    }

    fn range_tos(&self, val: Value) -> String {
        let range = val.as_range();
        format!(
            "{}{}{}",
            self.inspect(range.start),
            if range.exclude_end() { "..." } else { ".." },
            self.inspect(range.end),
        )
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
