use ruruby_parse::{BlockInfo, Loc, LvarCollector, Node, ParamKind, Parser, SourceInfoRef};
use std::io::{BufWriter, Stdout, stdout};
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
pub use require::load_file;
pub use store::*;

pub static WARNING: std::sync::LazyLock<AtomicU8> = std::sync::LazyLock::new(|| AtomicU8::new(0u8));

pub(crate) type InlineGen = dyn Fn(
    &mut jitgen::AbstractState,
    &mut jitgen::asmir::AsmIr,
    &crate::jitgen::JitContext,
    &Store,
    &CallSiteInfo,
    ClassId,
    BytecodePtr,
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
    /// globals variables.
    global_vars: HashMap<IdentId, Value>,
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
        self.global_vars.values().for_each(|v| v.mark(alloc));
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
                fiber: codegen.fiber_invoker,
                fiber_with_self: codegen.fiber_invoker_with_self,
                binding: codegen.binding_invoker,
            }
        });

        let mut globals = Self {
            main_object,
            store: Store::new(),
            global_vars: HashMap::default(),
            no_jit,
            no_gems,
            stdout: BufWriter::new(stdout()),
            load_path: Value::array_empty(),
            random: Box::new(Prng::new()),
            loaded_canonicalized_files,
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
        assert_eq!(
            FuncId::new(1),
            globals.define_builtin_func(OBJECT_CLASS, "", enum_yielder, 0)
        );
        assert_eq!(
            FuncId::new(2),
            globals.define_builtin_func_rest(OBJECT_CLASS, "", yielder)
        );
        globals.random.init_with_seed(None);
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
        let program_name = match path.file_name() {
            Some(name) => name.to_string_lossy().to_string(),
            None => ".".to_string(),
        };
        let mut executor = Executor::init(self, &program_name)?;
        executor.init_stack_limit(self);
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
        let outer_fid = caller_cfp.lfp().func_id();
        let outer = self.store[outer_fid].as_iseq();
        let external_context = self.store.scoped_locals(outer);

        match Parser::parse_program_eval(code, path.into(), Some(&external_context)) {
            Ok(result) => {
                let res =
                    bytecodegen::bytecode_compile_eval(self, result, outer, Loc::default(), None);
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
        let outer_fid = binding.outer_lfp().func_id();
        let outer = self.store[outer_fid].as_iseq();
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

    pub fn flush_stdout(&mut self) {
        self.stdout.flush().unwrap();
    }

    pub fn write_stdout(&mut self, bytes: &[u8]) {
        self.stdout.write_all(bytes).unwrap();
    }

    pub fn print_value(&mut self, val: Value) {
        if let Some(s) = val.is_rstring() {
            self.stdout.write_all(&s)
        } else {
            let v = val.to_s(&self.store).into_bytes();
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
            let locals_len = self.locals_len(binding_lfp.func_id());
            for i in SlotId(1)..SlotId(1) + locals_len {
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
            let locals_len = self.locals_len(binding_lfp.func_id());
            for i in SlotId(1)..SlotId(1) + locals_len {
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

macro_rules! next_char {
    ($ch:ident, $chars:ident) => {
        $ch = match $chars.next() {
            Some(c) => c,
            None => break,
        };
    };
}

fn expect_char(chars: &mut std::str::Chars) -> Result<char> {
    let ch = match chars.next() {
        Some(ch) => ch,
        None => {
            return Err(MonorubyErr::argumenterr(
                "Invalid termination of format string",
            ));
        }
    };
    Ok(ch)
}

enum Integer {
    Fixnum(i64),
    BigInt(num::BigInt),
}

fn coerce_to_integer(globals: &mut Globals, val: Value) -> Result<Integer> {
    match val.unpack() {
        RV::Fixnum(i) => return Ok(Integer::Fixnum(i)),
        RV::String(s) => {
            let s = s.check_utf8()?;
            if let Ok(i) = s.parse::<i64>() {
                return Ok(Integer::Fixnum(i));
            } else if let Ok(b) = s.parse::<num::BigInt>() {
                return Ok(Integer::BigInt(b));
            }
        }
        _ => {}
    };
    let s = val.to_s(&globals.store);
    Err(MonorubyErr::argumenterr(format!(
        "invalid value for Integer(): {}",
        s
    )))
}

fn coerce_to_float(globals: &mut Globals, val: Value) -> Result<f64> {
    match val.unpack() {
        RV::Fixnum(i) => Ok(i as f64),
        RV::Float(f) => Ok(f),
        _ => {
            let s = val.to_s(&globals.store);
            Err(MonorubyErr::argumenterr(format!(
                "invalid value for Float(): {}",
                s
            )))
        }
    }
}

fn coerce_to_char(val: Value) -> Result<char> {
    match val.unpack() {
        RV::Fixnum(i) => {
            if let Ok(u) = u32::try_from(i) {
                if let Some(c) = char::from_u32(u) {
                    return Ok(c);
                }
            }
            Err(MonorubyErr::argumenterr("invalid character"))
        }
        RV::Float(f) => {
            let f = f.trunc();
            if 0.0 <= f && f <= u32::MAX as f64 {
                if let Some(c) = char::from_u32(f as u32) {
                    return Ok(c);
                }
            }
            Err(MonorubyErr::argumenterr("invalid character"))
        }
        RV::String(s) => {
            let s = s.check_utf8()?;
            if s.chars().count() != 1 {
                Err(MonorubyErr::argumenterr("%c requires a character"))
            } else {
                Ok(s.chars().next().unwrap())
            }
        }
        _ => Err(MonorubyErr::argumenterr("invalid character")),
    }
}

impl Globals {
    pub(crate) fn generate_range(
        &mut self,
        start: Value,
        end: Value,
        exclude_end: bool,
    ) -> Result<Value> {
        if !start.is_nil()
            && !end.is_nil()
            && start.real_class(&self.store).id() != end.real_class(&self.store).id()
        {
            return Err(MonorubyErr::bad_range(start, end));
        }
        Ok(Value::range(start, end, exclude_end))
    }

    pub(crate) fn format_by_args(&mut self, self_str: &str, arguments: &[Value]) -> Result<String> {
        let mut arg_no = 0;
        let mut format_str = String::new();
        let mut chars = self_str.chars();
        let mut ch = match chars.next() {
            Some(ch) => ch,
            None => return Ok(String::new()),
        };
        loop {
            if ch != '%' {
                format_str.push(ch);
                next_char!(ch, chars);
                continue;
            }
            match chars.next() {
                Some('%') => {
                    format_str.push('%');
                    next_char!(ch, chars);
                    continue;
                }
                Some(c) => ch = c,
                None => {
                    return Err(MonorubyErr::argumenterr(
                        "incomplete format specifier; use %% (double %) instead",
                    ));
                }
            };
            let mut zero_flag = false;
            // Zero-fill
            if ch == '0' {
                zero_flag = true;
                ch = expect_char(&mut chars)?;
            }
            // Width
            let mut width = 0usize;
            while ch.is_ascii_digit() {
                width = width * 10 + ch as usize - '0' as usize;
                ch = expect_char(&mut chars)?;
            }
            // Precision
            let mut precision = 0usize;
            if ch == '.' {
                ch = expect_char(&mut chars)?;
                while ch.is_ascii_digit() {
                    precision = precision * 10 + ch as usize - '0' as usize;
                    ch = expect_char(&mut chars)?;
                }
            } else {
                precision = 6;
            };
            if arguments.len() <= arg_no {
                return Err(MonorubyErr::argumenterr("too few arguments"));
            };
            // Specifier
            let val = arguments[arg_no];
            arg_no += 1;
            let format = match ch {
                'c' => {
                    let ch = coerce_to_char(val)?;
                    format!("{}", ch)
                }
                's' => val.to_s(&self.store),
                'd' | 'i' => {
                    let val = coerce_to_integer(self, val)?;
                    if zero_flag {
                        match val {
                            Integer::Fixnum(val) => {
                                format!("{:0w$.p$}", val, w = width, p = precision)
                            }
                            Integer::BigInt(val) => {
                                format!("{:0w$.p$}", val, w = width, p = precision)
                            }
                        }
                    } else {
                        match val {
                            Integer::Fixnum(val) => {
                                format!("{:w$.p$}", val, w = width, p = precision)
                            }
                            Integer::BigInt(val) => {
                                format!("{:w$.p$}", val, w = width, p = precision)
                            }
                        }
                    }
                }
                'b' => {
                    let val = coerce_to_integer(self, val)?;
                    if zero_flag {
                        match val {
                            Integer::Fixnum(val) => format!("{:0w$b}", val, w = width),
                            Integer::BigInt(val) => format!("{:0w$b}", val, w = width),
                        }
                    } else {
                        match val {
                            Integer::Fixnum(val) => format!("{:w$b}", val, w = width),
                            Integer::BigInt(val) => format!("{:w$b}", val, w = width),
                        }
                    }
                }
                'x' => {
                    let val = coerce_to_integer(self, val)?;
                    if zero_flag {
                        match val {
                            Integer::Fixnum(val) => format!("{:0w$x}", val, w = width),
                            Integer::BigInt(val) => format!("{:0w$x}", val, w = width),
                        }
                    } else {
                        match val {
                            Integer::Fixnum(val) => format!("{:w$x}", val, w = width),
                            Integer::BigInt(val) => format!("{:w$x}", val, w = width),
                        }
                    }
                }
                'X' => {
                    let val = coerce_to_integer(self, val)?;
                    if zero_flag {
                        match val {
                            Integer::Fixnum(val) => format!("{:0w$X}", val, w = width),
                            Integer::BigInt(val) => format!("{:0w$X}", val, w = width),
                        }
                    } else {
                        match val {
                            Integer::Fixnum(val) => format!("{:w$X}", val, w = width),
                            Integer::BigInt(val) => format!("{:w$X}", val, w = width),
                        }
                    }
                }
                'f' => {
                    let f = coerce_to_float(self, val)?;
                    if zero_flag {
                        format!("{:0w$.p$}", f, w = width, p = precision)
                    } else {
                        format!("{:w$.p$}", f, w = width, p = precision)
                    }
                }
                'e' => {
                    let f = coerce_to_float(self, val)?;
                    if zero_flag {
                        format!("{:0w$.p$e}", f, w = width, p = precision)
                    } else {
                        format!("{:w$.p$e}", f, w = width, p = precision)
                    }
                }
                'E' => {
                    let f = coerce_to_float(self, val)?;
                    if zero_flag {
                        format!("{:0w$.p$E}", f, w = width, p = precision)
                    } else {
                        format!("{:w$.p$E}", f, w = width, p = precision)
                    }
                }
                _ => {
                    return Err(MonorubyErr::argumenterr(format!(
                        "malformed format string - %{}",
                        ch
                    )));
                }
            };
            format_str += &format;
            next_char!(ch, chars);
        }

        Ok(format_str)
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
