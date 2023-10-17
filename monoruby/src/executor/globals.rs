use fancy_regex::Regex;
use ruruby_parse::{BlockInfo, Loc, Node, ParamKind, Parser, SourceInfoRef};
use std::io::{stdout, BufWriter, Stdout};
use std::io::{Read, Write};
use std::path::PathBuf;
use std::rc::Rc;

use super::*;

mod method;
mod prng;
mod store;
pub use compiler::*;
pub use error::*;
use prng::*;
pub use store::*;

pub(in crate::executor) type InlineGen =
    fn(&mut Codegen, &mut jitgen::BBContext, &CallSiteInfo, BcPc, DestLabel);
pub(in crate::executor) type InlineAnalysis = fn(&mut analysis::SlotInfo, &CallSiteInfo);

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
    pub(crate) regexp_cache: HashMap<String, Rc<Regex>>,
    /// warning level.
    pub(super) warning: u8,
    /// suppress jit compilation.
    no_jit: bool,
    /// stdout.
    stdout: BufWriter<Stdout>,
    /// library directries.
    pub lib_directories: Vec<String>,
    /// standard PRNG
    random: Prng,
    /// loaded libraries (canonical path).
    loaded_canonicalized_files: IndexSet<PathBuf>,
    /// stats for deoptimization
    #[cfg(feature = "profile")]
    pub(super) deopt_stats: HashMap<(FuncId, usize), usize>,
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
        let mut lib: Vec<String> = lib.iter().map(|v| v.as_string()).collect();
        globals.lib_directories.append(&mut lib);
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

    pub fn compile_and_run(&mut self, code: &str, path: &std::path::Path) -> Result<Value> {
        let mut executor = Executor::init(self);
        let res = executor.eval_script(self, code.to_string(), path);
        self.flush_stdout();
        #[cfg(feature = "profile")]
        self.show_stats();
        res
    }

    pub fn compile_script(&mut self, code: String, path: impl Into<PathBuf>) -> Result<FuncId> {
        match Parser::parse_program(code, path.into()) {
            Ok(res) => bytecodegen::compile_script(self, res.node, res.source_info),
            Err(err) => Err(MonorubyErr::parse(err)),
        }
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

    pub(crate) fn get_yield_data(&mut self, cfp: CFP) -> Option<Proc> {
        cfp.get_block()
            .map(|bh| Proc::new(self.get_block_data(cfp, bh)))
    }

    pub(crate) fn get_block_data(&mut self, mut cfp: CFP, bh: BlockHandler) -> ProcInner {
        if let Some((func_id, idx)) = bh.try_proxy() {
            for _ in 0..idx {
                cfp = cfp.prev().unwrap();
            }
            let func_data = self.compile_on_demand(func_id).clone();
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

    pub(crate) fn set_gvar(&mut self, name: IdentId, val: Value) {
        self.global_vars.insert(name, val);
    }

    pub(crate) fn get_gvar(&mut self, name: IdentId) -> Option<Value> {
        self.global_vars.get(&name).cloned()
    }

    pub(crate) fn get_load_path(&self) -> Value {
        let iter = self
            .lib_directories
            .iter()
            .map(|s| Value::string_from_str(s));
        Value::array_from_iter(iter)
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

    pub(crate) fn compile_on_demand(&mut self, func_id: FuncId) -> &FuncData {
        if self[func_id].data.codeptr.is_none() {
            let kind = self[func_id].kind.clone();
            let codeptr = self.codegen.jit.get_current_address();
            self.codegen.gen_wrapper(kind, self.no_jit);
            self[func_id].data.codeptr = Some(codeptr);
        }
        &self[func_id].data
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

impl Globals {
    #[cfg(feature = "emit-bc")]
    pub fn dump_bc(&mut self) {
        let dumped_bc = self.dumped_bc;
        if self.startup_flag {
            self.store.functions()[dumped_bc..]
                .iter()
                .for_each(|info| match &info.kind {
                    FuncKind::ISeq(_) => info.dump_bc(self),
                    _ => {}
                });
        }
        self.dumped_bc = self.store.func_len();
    }

    #[cfg(feature = "emit-asm")]
    pub(crate) fn dump_disas(&mut self, sourcemap: Vec<(BcIndex, usize)>, func_id: FuncId) {
        let (start, code_end, end) = self.codegen.jit.code_block.last().unwrap();
        eprintln!(
            "offset:{:?} code: {} bytes  data: {} bytes",
            start,
            *code_end - *start,
            *end - *code_end
        );
        self.codegen.jit.select_page(0);
        let dump = self.codegen.jit.dump_code().unwrap();
        let dump: Vec<(usize, String)> = dump
            .split('\n')
            .filter(|s| s.len() >= 29)
            .map(|x| {
                let i = x.find(':').unwrap();
                (
                    match usize::from_str_radix(&x[0..i].trim(), 16) {
                        Ok(i) => i,
                        _ => {
                            panic!("{}", &x[0..i].trim());
                        }
                    },
                    x[i + 24..].to_string(),
                )
            })
            .collect();
        let func = self[func_id].as_ruby_func();
        for (i, text) in dump {
            sourcemap
                .iter()
                .filter_map(
                    |(bc_pos, code_pos)| {
                        if *code_pos == i {
                            Some(*bc_pos)
                        } else {
                            None
                        }
                    },
                )
                .for_each(|bc_pos| {
                    let pc = BcPc::from(&func.bytecode()[bc_pos.to_usize()]);
                    eprintln!(
                        "{bc_pos} {}",
                        match pc.format(self, bc_pos.to_usize()) {
                            Some(s) => s,
                            None => "".to_string(),
                        }
                    );
                });

            eprintln!("  {:05x}: {}", i, text);
        }
    }

    pub(in crate::executor) unsafe fn dump_frame_info(&mut self, lfp: LFP) {
        let meta = lfp.meta();
        let outer = lfp.outer();
        let func_id = meta.func_id();
        let block = lfp.block();
        eprintln!(
            "    {} block:{} outer:{} {:?}",
            self.store.func_description(func_id),
            match block {
                Some(block) => {
                    match block.try_proxy() {
                        Some((func_id, idx)) => {
                            format!("BlockArgProxy {{ {:?}, {} }}", func_id, idx)
                        }
                        _ => self.inspect(block.0),
                    }
                }
                None => "None".to_string(),
            },
            match outer {
                None => "None".to_string(),
                Some(outer) => format!("{:?}", outer.lfp()),
            },
            meta,
        );
        eprint!("    ");
        for r in 0..meta.reg_num() as usize {
            eprint!(
                "%{}{}:[{}] ",
                r,
                if r == 0 { "(self)" } else { "" },
                if let Some(v) = lfp.register(r) {
                    self.inspect(v)
                } else {
                    "None".to_string()
                }
            );
        }
        eprintln!();
    }

    #[cfg(feature = "profile")]
    pub(crate) fn show_stats(&self) {
        eprintln!();
        eprintln!("deoptimization stats (top 20)");
        eprintln!(
            "{:30} FuncId [{:05}]     {:7}",
            "func name", "index", "count"
        );
        eprintln!("-------------------------------------------------------------------------------------------------------------------");
        let mut v: Vec<_> = self.deopt_stats.iter().collect();
        v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
        for ((func_id, index), count) in v.into_iter().take(20) {
            let bc = BcPc::from(&self.store[*func_id].as_ruby_func().bytecode()[*index]);
            let fmt = if let Some(fmt) = bc.format(self, *index) {
                fmt
            } else {
                "<INVALID>".to_string()
            };
            let name = self.store.func_description(*func_id);
            eprintln!(
                "{:30}  {:5} [{:05}]  {:10}   {fmt}",
                name,
                func_id.get(),
                index,
                count
            );
        }
        eprintln!();
        eprintln!("global method cache stats (top 20)");
        eprintln!("{:30} {:30} {:10}", "func name", "class", "count");
        eprintln!("------------------------------------------------------------------------");
        let mut v: Vec<_> = self.global_method_cache_stats.iter().collect();
        v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
        for ((class_id, name), count) in v.into_iter().take(20) {
            eprintln!(
                "{:30} {:30} {:10}",
                name.to_string(),
                class_id.get_name(self),
                count
            );
        }
        eprintln!();
        eprintln!("full method exploration stats (top 20)");
        eprintln!("{:30} {:30} {:10}", "func name", "class", "count");
        eprintln!("------------------------------------------------------------------------");
        let mut v: Vec<_> = self.method_exploration_stats.iter().collect();
        v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
        for ((class_id, name), count) in v.into_iter().take(20) {
            eprintln!(
                "{:30} {:30} {:10}",
                name.to_string(),
                class_id.get_name(self),
                count
            );
        }
    }
}
