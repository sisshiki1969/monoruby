use fancy_regex::Regex;
use ruruby_parse::{
    BinOp, BlockInfo, Loc, LvarCollector, Node, NodeKind, ParamKind, ParseErr, ParseErrKind,
    Parser, SourceInfoRef,
};
use std::io::{stdout, BufWriter, Stdout};
use std::io::{Read, Write};
use std::path::PathBuf;
use std::rc::Rc;

use super::*;

mod class;
mod error;
mod functions;
mod method;
pub use class::*;
pub use compiler::*;
pub use error::*;
pub use functions::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum InlineMethod {
    IntegerTof,
    MathSqrt,
    MathCos,
    MathSin,
}

struct Root<'a> {
    globals: &'a Globals,
    current_cfp: CFP,
}

impl<'a> GC<RValue> for Root<'a> {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        self.globals.class.mark(alloc);
        self.globals.func.mark(alloc);
        self.globals
            .global_vars
            .values()
            .for_each(|v| v.mark(alloc));
        let mut cfp = self.current_cfp;
        unsafe {
            loop {
                let lfp = cfp.lfp();
                let meta = lfp.meta();
                for r in 0..meta.reg_num() as usize {
                    let v = lfp.register(r);
                    v.mark(alloc);
                }
                lfp.block().map(|v| v.mark(alloc));

                cfp = cfp.prev();
                if cfp.is_null() {
                    break;
                }
            }
        }
    }
}

impl<'a> GCRoot<RValue> for Root<'a> {
    fn startup_flag(&self) -> bool {
        true
    }
}

///
/// Global state.
///
pub struct Globals {
    /// code generator.
    pub codegen: Codegen,
    /// function info.
    pub(crate) func: FnStore,
    /// class table.
    class: ClassStore,
    /// globals variables.
    global_vars: HashMap<IdentId, Value>,
    /// error information.
    error: Option<MonorubyErr>,
    /// global method cache.
    global_method_cache: HashMap<(IdentId, ClassId), (u32, Option<FuncId>)>,
    /// regex cache.
    pub regexp_cache: HashMap<String, Rc<Regex>>,
    /// warning level.
    pub(super) warning: u8,
    /// suppress jit compilation.
    pub(super) no_jit: bool,
    /// stdout.
    stdout: BufWriter<Stdout>,
    /// library directries.
    pub lib_directories: Vec<String>,
    #[cfg(feature = "log-jit")]
    /// stats for deoptimization
    pub(crate) deopt_stats: HashMap<(FuncId, usize), usize>,
    #[cfg(feature = "log-jit")]
    /// stats for method cache miss
    pub(crate) method_cache_stats: HashMap<(ClassId, IdentId), usize>,
}

///
/// Execute garbage collection.
///
pub(in crate::executor) extern "C" fn execute_gc(globals: &Globals, current_cfp: CFP) {
    ALLOC.with(|alloc| {
        alloc.borrow_mut().check_gc(&Root {
            globals,
            current_cfp,
        })
    });
}

impl Globals {
    pub fn new(warning: u8, no_jit: bool) -> Self {
        let main_object = Value::new_object(OBJECT_CLASS);
        let mut globals = Self {
            codegen: Codegen::new(no_jit, main_object),
            func: FnStore::new(),
            class: ClassStore::new(),
            global_vars: HashMap::default(),
            global_method_cache: HashMap::default(),
            regexp_cache: HashMap::default(),
            error: None,
            warning,
            no_jit,
            stdout: BufWriter::new(stdout()),
            lib_directories: vec![],
            #[cfg(feature = "log-jit")]
            deopt_stats: HashMap::default(),
            #[cfg(feature = "log-jit")]
            method_cache_stats: HashMap::default(),
        };
        builtins::init_builtins(&mut globals);
        globals.set_ivar(
            main_object,
            IdentId::_NAME,
            Value::new_string_from_str("main"),
        );
        globals
    }

    pub fn compile_and_run(&mut self, code: &str, path: &std::path::Path) -> Result<Value> {
        let mut executor = Executor::default();
        match executor.eval_script(self, code.to_string(), path) {
            Some(val) => Ok(val),
            None => Err(self.take_error().unwrap()),
        }
    }

    pub fn exec_startup(&mut self) {
        // load library path
        let load_path = include_str!(concat!(env!("OUT_DIR"), "/libpath.rb"));
        let nodes = Parser::parse_program(load_path.to_string(), PathBuf::new())
            .unwrap()
            .node;

        let mut lib: Vec<String> = Value::from_ast2(&nodes)
            .as_array()
            .to_vec()
            .into_iter()
            .map(|v| v.as_string().to_string())
            .collect();
        self.lib_directories.append(&mut lib);
        let path = std::path::Path::new("startup/startup.rb");
        let code = include_str!("../../startup/startup.rb").to_string();
        let mut executor = Executor::default();
        match executor.eval_script(self, code, path) {
            Some(_) => {}
            None => {
                let err = self.take_error().unwrap();
                err.show_error_message_and_all_loc(self);
                panic!("error occurred in startup.");
            }
        };
        let pcg_name = env!("CARGO_PKG_NAME");
        let pcg_version = env!("CARGO_PKG_VERSION");
        let description = format!("{pcg_name} {pcg_version} [x86_64-linux]",);
        let val = Value::new_string_from_str(&description);
        self.set_constant(OBJECT_CLASS, IdentId::get_ident_id("RUBY_DESCRIPTION"), val);
        let val = Value::new_string_from_str(pcg_name);
        self.set_constant(OBJECT_CLASS, IdentId::get_ident_id("RUBY_ENGINE"), val);
        let val = Value::new_string_from_str(pcg_version);
        self.set_constant(OBJECT_CLASS, IdentId::get_ident_id("RUBY_VERSION"), val);
        self.set_constant(
            OBJECT_CLASS,
            IdentId::get_ident_id("RUBY_ENGINE_VERSION"),
            val,
        );
    }

    pub fn compile_script(&mut self, code: String, path: impl Into<PathBuf>) -> Result<FuncId> {
        match Parser::parse_program(code, path.into()) {
            Ok(res) => self.func.compile_script(res.node, res.source_info),
            Err(err) => Err(MonorubyErr::parse(err)),
        }
    }

    pub fn compile_script_with_binding(
        &mut self,
        code: String,
        path: impl Into<PathBuf>,
        context: Option<LvarCollector>,
    ) -> Result<(FuncId, LvarCollector)> {
        match Parser::parse_program_binding(code, path.into(), context, None) {
            Ok(res) => {
                let collector = res.lvar_collector;
                let fid = self.func.compile_script(res.node, res.source_info)?;
                Ok((fid, collector))
            }
            Err(err) => Err(MonorubyErr::parse(err)),
        }
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

    pub(crate) fn get_gvar(&mut self, name: IdentId) -> Value {
        match self.global_vars.get(&name) {
            Some(val) => *val,
            None => Value::nil(),
        }
    }

    pub(crate) fn load_lib(&mut self, path: &std::path::Path) -> Option<(String, PathBuf)> {
        for lib in self.lib_directories.clone() {
            let mut lib = std::path::PathBuf::from(lib);
            lib.push(path);
            //if lib.is_absolute() {
            lib.set_extension("rb");
            if lib.exists() {
                return self.load_file(&lib);
            }
            lib.set_extension("so");
            if lib.exists() {
                eprintln!("Warning: currently, can not require .so file. {:?}", lib);
            }
            //}
        }
        self.err_cant_load(None, path);
        None
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

    pub(super) fn compile_on_demand(&mut self, func_id: FuncId) -> &FuncData {
        //let func = &mut globals.func[func_id];
        if self.func[func_id].data.codeptr.is_none() {
            let kind = self.func[func_id].kind.clone();
            let codeptr = self.codegen.gen_wrapper(kind, self.no_jit);
            self.func[func_id].data.codeptr = Some(codeptr);
        }
        &self.func[func_id].data
    }

    pub(super) fn get_block_data(
        &mut self,
        block_handler: BlockHandler,
        interp: &Executor,
    ) -> BlockData {
        if let Some(bh) = block_handler.try_fixnum() {
            let func_id = FuncId(u32::try_from((bh as u64) >> 16).unwrap());
            let mut cfp = interp.cfp;
            unsafe {
                for _ in 0..bh as i16 as u16 {
                    cfp = cfp.prev();
                }
                let func_data = self.compile_on_demand(func_id) as _;
                BlockData {
                    outer_lfp: cfp.lfp(),
                    func_data,
                }
            }
        } else {
            block_handler.as_proc().clone()
        }
    }

    pub(super) fn execute(
        &mut self,
        executor: &mut Executor,
        func_data: *const FuncData,
    ) -> Option<Value> {
        (self.codegen.entry_point)(executor, self, func_data)
    }

    fn class_version_inc(&mut self) {
        unsafe { *self.codegen.class_version_addr += 1 }
    }

    fn class_version(&self) -> u32 {
        unsafe { *self.codegen.class_version_addr }
    }

    fn load_file(&mut self, path: &std::path::Path) -> Option<(String, PathBuf)> {
        let mut file_body = String::new();
        match std::fs::OpenOptions::new().read(true).open(path) {
            Ok(mut file) => match file.read_to_string(&mut file_body) {
                Ok(_) => {}
                Err(err) => {
                    self.err_cant_load(Some(err), path);
                    return None;
                }
            },
            Err(err) => {
                self.err_cant_load(Some(err), path);
                return None;
            }
        };

        Some((file_body, path.into()))
    }
}

impl Globals {
    pub(crate) fn val_tos(&self, val: Value) -> String {
        match val.unpack() {
            RV::None => "Undef".to_string(),
            RV::Nil => "nil".to_string(),
            RV::Bool(b) => format!("{:?}", b),
            RV::Integer(n) => format!("{}", n),
            RV::BigInt(n) => format!("{}", n),
            RV::Float(f) => dtoa::Buffer::new().format(f).to_string(),
            RV::Symbol(id) => IdentId::get_name(id),
            RV::String(s) => match String::from_utf8(s.to_vec()) {
                Ok(s) => s,
                Err(_) => format!("{:?}", s),
            },
            RV::Object(rvalue) => match rvalue.kind() {
                ObjKind::CLASS => rvalue.as_class().get_name(self),
                ObjKind::TIME => rvalue.as_time().to_string(),
                ObjKind::ARRAY => self.array_tos(rvalue.as_array()),
                ObjKind::OBJECT => self.object_tos(val),
                ObjKind::RANGE => self.range_tos(val),
                ObjKind::PROC => self.proc_tos(val),
                ObjKind::HASH => self.hash_tos(val),
                _ => format!("{:016x}", val.get()),
            },
        }
    }

    pub(crate) fn val_to_bytes(&self, val: Value) -> Vec<u8> {
        if let RV::String(s) = val.unpack() {
            return s.to_vec();
        }
        self.val_tos(val).into_bytes()
    }

    pub(crate) fn val_inspect(&self, val: Value) -> String {
        match val.unpack() {
            RV::None => "Undef".to_string(),
            RV::Nil => "nil".to_string(),
            RV::Bool(b) => format!("{:?}", b),
            RV::Integer(n) => format!("{}", n),
            RV::BigInt(n) => format!("{}", n),
            RV::Float(f) => dtoa::Buffer::new().format(f).to_string(),
            RV::Symbol(id) => format!(":{}", IdentId::get_name(id)),
            RV::String(s) => match String::from_utf8(s.to_vec()) {
                Ok(s) => format!("\"{}\"", escape_string::escape(&s)),
                Err(_) => format!("{:?}", s),
            },
            RV::Object(rvalue) => match rvalue.kind() {
                ObjKind::CLASS => rvalue.as_class().get_name(self),
                ObjKind::TIME => rvalue.as_time().to_string(),
                ObjKind::ARRAY | ObjKind::SPLAT => self.array_tos(rvalue.as_array()),
                ObjKind::OBJECT => self.object_inspect(val),
                ObjKind::RANGE => self.range_tos(val),
                ObjKind::PROC => self.proc_tos(val),
                ObjKind::HASH => self.hash_tos(val),
                ObjKind::REGEXP => self.regexp_tos(val),
                kind => unreachable!("{:016x} {kind}", val.get()),
            },
        }
    }

    pub(crate) fn generate_range(
        &mut self,
        start: Value,
        end: Value,
        exclude_end: bool,
    ) -> Option<Value> {
        if start.get_real_class_id(self) != end.get_real_class_id(self) {
            self.err_bad_range(start, end);
            return None;
        }
        Some(Value::new_range(start, end, exclude_end))
    }

    fn array_tos(&self, v: &ArrayInner) -> String {
        match v.len() {
            0 => "[]".to_string(),
            1 => format!("[{}]", self.val_inspect(v[0])),
            _ => {
                let mut s = format!("[{}", self.val_inspect(v[0]));
                for val in v[1..].iter() {
                    s += &format!(", {}", self.val_inspect(*val));
                }
                s += "]";
                s
            }
        }
    }

    fn object_tos(&self, val: Value) -> String {
        if let Some(name) = self.get_ivar(val, IdentId::_NAME) {
            self.val_tos(name)
        } else {
            format!(
                "#<{}:0x{:016x}>",
                val.class_id().get_name(self),
                val.rvalue().id()
            )
        }
    }

    fn proc_tos(&self, val: Value) -> String {
        format!("#<Proc:0x{:016x}>", val.rvalue().id())
    }

    fn object_inspect(&self, val: Value) -> String {
        if let Some(name) = self.get_ivar(val, IdentId::_NAME) {
            self.val_tos(name)
        } else {
            let mut s = String::new();
            for (id, v) in self.get_ivars(val).into_iter() {
                s += &format!(" {}={}", IdentId::get_name(id), self.val_inspect(v));
            }
            format!(
                "#<{}:0x{:016x}{s}>",
                val.class_id().get_name(self),
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
                        k.inspect(self)
                    };
                    let v_inspect = if v == val {
                        "{...}".to_string()
                    } else {
                        v.inspect(self)
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

    fn regexp_tos(&self, val: Value) -> String {
        let regexp = val.is_regex().unwrap();
        format!("/{}/", regexp.as_str())
    }

    fn range_tos(&self, val: Value) -> String {
        let range = val.as_range();
        format!(
            "{}{}{}",
            self.val_inspect(range.start),
            if range.exclude_end() { "..." } else { ".." },
            self.val_inspect(range.end),
        )
    }

    pub(crate) fn check_arg(&mut self, func_id: FuncId, args_len: usize) -> Option<()> {
        let arity = self.func[func_id].arity();
        if arity != -1 && (arity as usize) != args_len {
            self.error = Some(MonorubyErr::wrong_arguments(arity as usize, args_len));
            return None;
        }
        Some(())
    }
}

impl Globals {
    #[cfg(feature = "emit-bc")]
    pub(crate) fn dump_bc(&self) {
        self.func
            .functions()
            .iter()
            .skip(1)
            .for_each(|info| match &info.kind {
                FuncKind::ISeq(_) => info.dump_bc(self),
                _ => {}
            });
    }

    #[cfg(any(feature = "emit-asm"))]
    pub(crate) fn dump_disas(&mut self, sourcemap: Vec<(usize, usize)>, func_id: FuncId) {
        let (start, code_end, end) = self.codegen.jit.code_block.last().unwrap();
        eprintln!(
            "offset:{:?} code: {} bytes  data: {} bytes",
            start,
            *code_end - *start,
            *end - *code_end
        );
        self.codegen.jit.select_page(0);
        let dump = self.codegen.jit.dump_code().unwrap();
        //eprintln!("{}", dump);
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
                    x[28..].to_string(),
                )
            })
            .collect();
        let func = self.func[func_id].as_ruby_func();
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
                    let pc = BcPc::from(&func.bytecode()[bc_pos]);
                    eprintln!(
                        ":{:05} {}",
                        bc_pos,
                        match pc.format(self, bc_pos) {
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
            "    name:[{}] block:{} outer:{} {:?}",
            self.func[func_id]
                .name()
                .unwrap_or(&"<unnamed>".to_string()),
            match block {
                Some(block) => {
                    match block.unpack() {
                        RV::Integer(i) => {
                            let i = i as u64;
                            let func_id = u32::try_from(i >> 16).unwrap();
                            let idx = i as u64 as u16;
                            format!("BlockArgProxy {{ {:?}, {} }}", FuncId(func_id), idx)
                        }
                        _ => unimplemented!(),
                    }
                }
                None => "None".to_string(),
            },
            if outer.is_null() {
                "None".to_string()
            } else {
                format!("{:?}", outer)
            },
            meta,
        );
        eprint!("    ");
        for r in 0..meta.reg_num() as usize {
            let v = lfp.register(r);
            eprint!(
                "%{}{}:[{}] ",
                r,
                if r == 0 { "(self)" } else { "" },
                self.val_inspect(v)
            );
        }
        eprintln!();
    }
}
