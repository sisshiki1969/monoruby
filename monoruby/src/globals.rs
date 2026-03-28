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
    CallSiteId,
    ClassId,
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
                block_with_self: codegen.block_invoker_with_self,
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
        globals.set_constant(
            BASIC_OBJECT_CLASS,
            IdentId::get_id("BasicObject"),
            basic_object.get(),
        );
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
        let outer_fid = caller_cfp.lfp().func_id();
        let outer = match self.store[outer_fid].is_iseq() {
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
        let outer_fid = binding.outer_lfp().func_id();
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
        RV::Float(f) => {
            let t = f.trunc();
            return if i64::MIN as f64 <= t && t <= i64::MAX as f64 {
                Ok(Integer::Fixnum(t as i64))
            } else {
                use num::FromPrimitive;
                Ok(Integer::BigInt(
                    num::BigInt::from_f64(t).expect("float is not NaN or infinite"),
                ))
            };
        }
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

/// Apply width padding to a string, with left or right alignment.
fn apply_width(s: &str, width: usize, left_align: bool, pad: char) -> String {
    if s.len() >= width {
        return s.to_string();
    }
    let padding: String = std::iter::repeat(pad).take(width - s.len()).collect();
    if left_align {
        format!("{}{}", s, padding)
    } else {
        format!("{}{}", padding, s)
    }
}

/// Format an integer string with sign/space/zero/width flags.
fn format_integer_with_flags(
    s: &str,
    width: usize,
    zero_flag: bool,
    minus_flag: bool,
    plus_flag: bool,
    space_flag: bool,
) -> String {
    let (is_neg, digits) = if let Some(stripped) = s.strip_prefix('-') {
        (true, stripped)
    } else {
        (false, s)
    };
    let sign = if is_neg {
        "-"
    } else if plus_flag {
        "+"
    } else if space_flag {
        " "
    } else {
        ""
    };
    let body = format!("{}{}", sign, digits);
    if zero_flag && width > body.len() {
        let pad = width - sign.len();
        format!("{}{:0>w$}", sign, digits, w = pad)
    } else {
        apply_width(&body, width, minus_flag, ' ')
    }
}

/// Format a float with sign/space/zero/width flags.
/// `s` is the formatted absolute value, `f` is the original float (for sign detection).
fn format_float_with_flags(
    s: &str,
    f: f64,
    width: usize,
    zero_flag: bool,
    minus_flag: bool,
    plus_flag: bool,
    space_flag: bool,
) -> String {
    let sign = if f.is_sign_negative() && !f.is_nan() {
        "-"
    } else if plus_flag {
        "+"
    } else if space_flag {
        " "
    } else {
        ""
    };
    let body = format!("{}{}", sign, s);
    if zero_flag && width > body.len() {
        let pad = width - sign.len();
        format!("{}{:0>w$}", sign, s, w = pad)
    } else {
        apply_width(&body, width, minus_flag, ' ')
    }
}

/// Format a float using %g/%G rules:
/// Use scientific notation if exponent < -4 or >= precision, otherwise fixed.
/// Strip trailing zeros after decimal point (and the point itself if no digits remain).
fn format_g(f: f64, precision: usize, uppercase: bool) -> String {
    if f == 0.0 {
        return "0".to_string();
    }
    if f.is_infinite() {
        return if f > 0.0 {
            "Inf".to_string()
        } else {
            "-Inf".to_string()
        };
    }
    if f.is_nan() {
        return "NaN".to_string();
    }
    let exp = f.log10().floor() as i32;
    if exp < -4 || exp >= precision as i32 {
        // Use scientific notation with (precision - 1) digits after decimal
        let sci_prec = if precision > 1 { precision - 1 } else { 0 };
        let s = if uppercase {
            format!("{:.p$E}", f, p = sci_prec)
        } else {
            format!("{:.p$e}", f, p = sci_prec)
        };
        // Strip trailing zeros in the mantissa part (before 'e'/'E')
        strip_trailing_zeros_scientific(&s)
    } else {
        // Use fixed notation
        // precision means total significant digits
        let fixed_prec = if precision as i32 > exp + 1 {
            (precision as i32 - exp - 1) as usize
        } else {
            0
        };
        let s = format!("{:.p$}", f, p = fixed_prec);
        strip_trailing_zeros_fixed(&s)
    }
}

/// Strip trailing zeros from fixed-point notation (e.g., "1.200" -> "1.2", "1.0" -> "1")
fn strip_trailing_zeros_fixed(s: &str) -> String {
    if !s.contains('.') {
        return s.to_string();
    }
    let trimmed = s.trim_end_matches('0').trim_end_matches('.');
    trimmed.to_string()
}

/// Strip trailing zeros from scientific notation (e.g., "1.200e+03" -> "1.2e+03")
fn strip_trailing_zeros_scientific(s: &str) -> String {
    let (mantissa, exponent) = if let Some(pos) = s.find('e') {
        (&s[..pos], &s[pos..])
    } else if let Some(pos) = s.find('E') {
        (&s[..pos], &s[pos..])
    } else {
        return s.to_string();
    };
    let trimmed = strip_trailing_zeros_fixed(mantissa);
    format!("{}{}", trimmed, exponent)
}

/// Normalize Rust's scientific notation exponent to Ruby format.
/// Rust: "1.23e6" or "1.23e-5" -> Ruby: "1.23e+06" or "1.23e-05"
/// Always includes sign, always at least 2 digits in exponent.
fn normalize_sci_exponent(s: &str) -> String {
    let (prefix, sep, exp_str) = if let Some(pos) = s.find('e') {
        (&s[..pos], "e", &s[pos + 1..])
    } else if let Some(pos) = s.find('E') {
        (&s[..pos], "E", &s[pos + 1..])
    } else {
        return s.to_string();
    };
    let (sign, digits) = if let Some(stripped) = exp_str.strip_prefix('-') {
        ("-", stripped)
    } else if let Some(stripped) = exp_str.strip_prefix('+') {
        ("+", stripped)
    } else {
        ("+", exp_str)
    };
    // Pad exponent to at least 2 digits
    if digits.len() < 2 {
        format!("{}{}{}0{}", prefix, sep, sign, digits)
    } else {
        format!("{}{}{}{}", prefix, sep, sign, digits)
    }
}

/// Format a negative integer in base for Ruby's two's complement representation.
/// Ruby uses `..f01` style for negative hex, `..7401` for negative octal, etc.
///
/// Algorithm: take abs value, format in base, compute (base-1)-complement + 1,
/// strip leading fill digits (keeping one), prefix with `..`.
fn format_neg_twos_complement(val: i64, base: u32, uppercase: bool) -> String {
    let max_digit = base - 1;
    let fill_char = if uppercase {
        char::from_digit(max_digit, base)
            .unwrap()
            .to_ascii_uppercase()
    } else {
        char::from_digit(max_digit, base).unwrap()
    };

    let abs_val = (val as i128).unsigned_abs() as u64;
    // Format absolute value in base
    let abs_digits: Vec<u32> = {
        let s = match base {
            16 => format!("{:x}", abs_val),
            8 => format!("{:o}", abs_val),
            2 => format!("{:b}", abs_val),
            _ => unreachable!(),
        };
        s.chars()
            .map(|c| c.to_digit(base).unwrap())
            .collect()
    };

    // Compute (base-1)-complement: each digit d -> (base-1) - d
    let mut complement: Vec<u32> = abs_digits.iter().map(|&d| max_digit - d).collect();

    // Add 1 (with carry)
    let mut carry = 1u32;
    for d in complement.iter_mut().rev() {
        let sum = *d + carry;
        *d = sum % base;
        carry = sum / base;
    }
    // If there's still carry, we need to prepend, but for Ruby's format
    // it just means more fill digits (which get stripped anyway)

    // Convert digits to chars
    let result: String = complement
        .iter()
        .map(|&d| {
            let c = char::from_digit(d, base).unwrap();
            if uppercase {
                c.to_ascii_uppercase()
            } else {
                c
            }
        })
        .collect();

    // Strip leading fill chars but keep one
    let stripped = result.trim_start_matches(fill_char);
    if stripped.is_empty() {
        format!("..{}", fill_char)
    } else {
        format!("..{}{}", fill_char, stripped)
    }
}

/// Format integer for %b/%B/%o/%x/%X with sign, prefix, and flags.
fn format_int_with_prefix(
    is_neg: bool,
    digits: &str,
    prefix: &str,
    width: usize,
    zero_flag: bool,
    minus_flag: bool,
    plus_flag: bool,
    space_flag: bool,
) -> String {
    let sign = if is_neg {
        "-"
    } else if plus_flag {
        "+"
    } else if space_flag {
        " "
    } else {
        ""
    };
    let body = format!("{}{}{}", sign, prefix, digits);
    if zero_flag && width > body.len() {
        let pad = width - sign.len() - prefix.len();
        format!("{}{}{:0>w$}", sign, prefix, digits, w = pad)
    } else {
        apply_width(&body, width, minus_flag, ' ')
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

    pub(crate) fn format_by_args(
        &mut self,
        self_str: &str,
        arguments: &[Value],
    ) -> Result<String> {
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
            // Parse flags
            let mut zero_flag = false;
            let mut minus_flag = false;
            let mut plus_flag = false;
            let mut space_flag = false;
            let mut hash_flag = false;
            loop {
                match ch {
                    '0' => zero_flag = true,
                    '-' => minus_flag = true,
                    '+' => plus_flag = true,
                    ' ' => space_flag = true,
                    '#' => hash_flag = true,
                    _ => break,
                }
                ch = expect_char(&mut chars)?;
            }
            // Left-align overrides zero-fill
            if minus_flag {
                zero_flag = false;
            }
            // Plus flag overrides space flag
            if plus_flag {
                space_flag = false;
            }
            // Width (may be '*')
            let mut width = 0usize;
            if ch == '*' {
                if arguments.len() <= arg_no {
                    return Err(MonorubyErr::argumenterr("too few arguments"));
                }
                let w = coerce_to_integer(self, arguments[arg_no])?;
                arg_no += 1;
                match w {
                    Integer::Fixnum(v) => {
                        if v < 0 {
                            minus_flag = true;
                            zero_flag = false;
                            width = (-v) as usize;
                        } else {
                            width = v as usize;
                        }
                    }
                    Integer::BigInt(_) => {
                        return Err(MonorubyErr::argumenterr("width too big"));
                    }
                }
                ch = expect_char(&mut chars)?;
            } else {
                while ch.is_ascii_digit() {
                    width = width * 10 + ch as usize - '0' as usize;
                    ch = expect_char(&mut chars)?;
                }
            }
            // Precision
            let mut precision = None;
            if ch == '.' {
                ch = expect_char(&mut chars)?;
                let mut prec = 0usize;
                if ch == '*' {
                    if arguments.len() <= arg_no {
                        return Err(MonorubyErr::argumenterr("too few arguments"));
                    }
                    let p = coerce_to_integer(self, arguments[arg_no])?;
                    arg_no += 1;
                    match p {
                        Integer::Fixnum(v) => {
                            if v >= 0 {
                                prec = v as usize;
                            }
                        }
                        Integer::BigInt(_) => {
                            return Err(MonorubyErr::argumenterr("precision too big"));
                        }
                    }
                    ch = expect_char(&mut chars)?;
                } else {
                    while ch.is_ascii_digit() {
                        prec = prec * 10 + ch as usize - '0' as usize;
                        ch = expect_char(&mut chars)?;
                    }
                }
                precision = Some(prec);
            }
            if arguments.len() <= arg_no {
                return Err(MonorubyErr::argumenterr("too few arguments"));
            };
            // Specifier
            let val = arguments[arg_no];
            arg_no += 1;
            let format = match ch {
                'c' => {
                    let c = coerce_to_char(val)?;
                    let s = format!("{}", c);
                    apply_width(&s, width, minus_flag, ' ')
                }
                's' => {
                    let mut s = val.to_s(&self.store);
                    if let Some(prec) = precision {
                        if s.len() > prec {
                            s.truncate(prec);
                        }
                    }
                    apply_width(&s, width, minus_flag, ' ')
                }
                'p' => {
                    let s = val.inspect(&self.store);
                    apply_width(&s, width, minus_flag, ' ')
                }
                'd' | 'i' => {
                    let ival = coerce_to_integer(self, val)?;
                    let s = match ival {
                        Integer::Fixnum(v) => format!("{}", v),
                        Integer::BigInt(v) => format!("{}", v),
                    };
                    format_integer_with_flags(
                        &s, width, zero_flag, minus_flag, plus_flag, space_flag,
                    )
                }
                'u' => {
                    let ival = coerce_to_integer(self, val)?;
                    let s = match ival {
                        Integer::Fixnum(v) => format!("{}", v),
                        Integer::BigInt(v) => format!("{}", v),
                    };
                    format_integer_with_flags(
                        &s, width, zero_flag, minus_flag, plus_flag, space_flag,
                    )
                }
                'b' | 'B' => {
                    let ival = coerce_to_integer(self, val)?;
                    match ival {
                        Integer::Fixnum(v) if v < 0 => {
                            let tc = format_neg_twos_complement(v, 2, ch == 'B');
                            let prefix = if hash_flag {
                                if ch == 'B' { "0B" } else { "0b" }
                            } else {
                                ""
                            };
                            let body = format!("{}{}", prefix, tc);
                            apply_width(&body, width, minus_flag, ' ')
                        }
                        _ => {
                            let digits = match ival {
                                Integer::Fixnum(v) => format!("{:b}", v),
                                Integer::BigInt(v) => format!("{:b}", v),
                            };
                            let is_zero = digits == "0";
                            let prefix = if hash_flag && !is_zero {
                                if ch == 'B' { "0B" } else { "0b" }
                            } else {
                                ""
                            };
                            format_int_with_prefix(
                                false, &digits, prefix, width, zero_flag, minus_flag,
                                plus_flag, space_flag,
                            )
                        }
                    }
                }
                'o' => {
                    let ival = coerce_to_integer(self, val)?;
                    match ival {
                        Integer::Fixnum(v) if v < 0 => {
                            let tc = format_neg_twos_complement(v, 8, false);
                            apply_width(&tc, width, minus_flag, ' ')
                        }
                        _ => {
                            let digits = match ival {
                                Integer::Fixnum(v) => format!("{:o}", v),
                                Integer::BigInt(v) => format!("{:o}", v),
                            };
                            let prefix = if hash_flag {
                                if digits.starts_with('0') { "" } else { "0" }
                            } else {
                                ""
                            };
                            format_int_with_prefix(
                                false, &digits, prefix, width, zero_flag, minus_flag,
                                plus_flag, space_flag,
                            )
                        }
                    }
                }
                'x' | 'X' => {
                    let ival = coerce_to_integer(self, val)?;
                    match ival {
                        Integer::Fixnum(v) if v < 0 => {
                            let tc = format_neg_twos_complement(v, 16, ch == 'X');
                            let prefix = if hash_flag {
                                if ch == 'X' { "0X" } else { "0x" }
                            } else {
                                ""
                            };
                            let body = format!("{}{}", prefix, tc);
                            apply_width(&body, width, minus_flag, ' ')
                        }
                        _ => {
                            let digits = match ival {
                                Integer::Fixnum(v) => {
                                    if ch == 'X' {
                                        format!("{:X}", v)
                                    } else {
                                        format!("{:x}", v)
                                    }
                                }
                                Integer::BigInt(v) => {
                                    if ch == 'X' {
                                        format!("{:X}", v)
                                    } else {
                                        format!("{:x}", v)
                                    }
                                }
                            };
                            let is_zero = digits == "0";
                            let prefix = if hash_flag && !is_zero {
                                if ch == 'X' { "0X" } else { "0x" }
                            } else {
                                ""
                            };
                            format_int_with_prefix(
                                false, &digits, prefix, width, zero_flag, minus_flag,
                                plus_flag, space_flag,
                            )
                        }
                    }
                }
                'f' => {
                    let f = coerce_to_float(self, val)?;
                    let prec = precision.unwrap_or(6);
                    let s = format!("{:.p$}", f.abs(), p = prec);
                    format_float_with_flags(
                        &s, f, width, zero_flag, minus_flag, plus_flag, space_flag,
                    )
                }
                'e' | 'E' => {
                    let f = coerce_to_float(self, val)?;
                    let prec = precision.unwrap_or(6);
                    let s = if ch == 'E' {
                        normalize_sci_exponent(&format!("{:.p$E}", f.abs(), p = prec))
                    } else {
                        normalize_sci_exponent(&format!("{:.p$e}", f.abs(), p = prec))
                    };
                    format_float_with_flags(
                        &s, f, width, zero_flag, minus_flag, plus_flag, space_flag,
                    )
                }
                'g' | 'G' => {
                    let f = coerce_to_float(self, val)?;
                    let prec = precision.unwrap_or(6);
                    let prec = if prec == 0 { 1 } else { prec };
                    let s = format_g(f.abs(), prec, ch == 'G');
                    let s = normalize_sci_exponent(&s);
                    format_float_with_flags(
                        &s, f, width, zero_flag, minus_flag, plus_flag, space_flag,
                    )
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

    pub(crate) fn format_by_hash(
        &mut self,
        vm: &mut Executor,
        self_str: &str,
        hash: Hashmap,
    ) -> Result<String> {
        let mut format_str = String::new();
        let mut chars = self_str.chars().peekable();
        while let Some(ch) = chars.next() {
            if ch != '%' {
                format_str.push(ch);
                continue;
            }
            match chars.peek() {
                Some('%') => {
                    chars.next();
                    format_str.push('%');
                    continue;
                }
                Some('{') => {
                    chars.next(); // consume '{'
                    let mut key = String::new();
                    loop {
                        match chars.next() {
                            Some('}') => break,
                            Some(c) => key.push(c),
                            None => {
                                return Err(MonorubyErr::argumenterr(
                                    "malformed format string - missing '}'",
                                ));
                            }
                        }
                    }
                    let key_val = Value::symbol_from_str(&key);
                    let val = hash.get(key_val, vm, self)?.unwrap_or(Value::nil());
                    format_str += &val.to_s(&self.store);
                }
                _ => {
                    return Err(MonorubyErr::argumenterr(format!(
                        "malformed format string - %{}",
                        chars.peek().map_or(' ', |c| *c)
                    )));
                }
            }
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
