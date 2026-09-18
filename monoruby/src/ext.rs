//! Dynamically loaded native extensions: the interpreter side of the C ABI
//! in `monoruby-ext-sys` (`doc/native_extension_loading.md` §4).
//!
//! `require "foo.so"` finds `libfoo.so` next to the running binary (or in
//! `<install root>/ext`, or on `MONORUBY_EXT_PATH`), `dlopen`s it and calls
//! its `Init_foo` with a context whose `api` is [`MR_API`] — the one table
//! of function pointers through which the extension reaches the
//! interpreter. Methods the extension defines are ordinary builtins whose
//! `BuiltinFn` is [`ext_method_trampoline`]; it finds the extension's
//! function through the calling frame's `FuncId` and re-marshals the
//! arguments as `argc / argv`, so neither `Lfp` nor `BytecodePtr` is part
//! of the ABI.
//!
//! Two rules keep this sound and are enforced here rather than trusted:
//! errors cross the boundary as a pending error plus `MR_UNDEF` (a Rust
//! panic inside an extension would abort, as in any builtin), and an
//! extension never sees a `Value` it could hold unrooted without having
//! been told so — every table entry that hands one out documents whether
//! it survives a call back into Ruby.

use crate::alloc::GC;
use crate::*;
use monoruby_ext_sys::*;
use std::collections::HashMap;
use std::ffi::{CStr, c_char, c_int, c_void};
use std::path::{Path, PathBuf};

// ---------------------------------------------------------------------
// Per-interpreter state
// ---------------------------------------------------------------------

/// A method an extension defined: found from the trampoline through the
/// frame's `FuncId`.
#[derive(Clone, Copy)]
struct ExtMethod {
    f: MrMethodFn,
    /// `MR_ARGC_VARIADIC` when the builtin was registered with a rest
    /// parameter, whose Array the trampoline flattens into `argv`.
    argc: c_int,
}

#[derive(Default)]
pub(crate) struct ExtState {
    methods: HashMap<FuncId, ExtMethod>,
    /// `gc_pin` counts, keyed by value bits; marked as roots.
    pins: HashMap<u64, u32>,
    /// The `dlopen` handles, kept for the life of the process: an
    /// extension is never unloaded (its method pointers live in the
    /// function table).
    handles: Vec<usize>,
}

impl ExtState {
    pub(crate) fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        for bits in self.pins.keys() {
            Value::from_u64(*bits).mark(alloc);
        }
    }
}

// ---------------------------------------------------------------------
// Native payloads
// ---------------------------------------------------------------------

/// The payload of an object of a class defined with `MR_CLASS_NATIVE`:
/// the extension's pointer plus the callbacks that mark and free it.
/// `data` is null between allocation and `native_set` (or after a `dup`
/// of a kind that cannot be copied).
struct ExtNative {
    data: *mut c_void,
    ops: *const MrNativeOps,
}

impl ExtNative {
    fn empty() -> Self {
        Self {
            data: std::ptr::null_mut(),
            ops: std::ptr::null(),
        }
    }

    fn release(&mut self) {
        if !self.data.is_null()
            && !self.ops.is_null()
            // SAFETY: `ops` is the static table the extension registered
            // the payload with; it outlives every object of the kind.
            && let Some(free) = unsafe { (*self.ops).free }
        {
            // SAFETY: `data` was handed to us by the extension together
            // with `ops`, and is released exactly once — here, after
            // which it is nulled.
            unsafe { free(self.data) };
        }
        self.data = std::ptr::null_mut();
    }
}

impl Drop for ExtNative {
    fn drop(&mut self) {
        self.release();
    }
}

impl NativeData for ExtNative {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        if !self.data.is_null()
            && !self.ops.is_null()
            // SAFETY: as in `release`.
            && let Some(mark) = unsafe { (*self.ops).mark }
        {
            // SAFETY: the marker is only ever passed back to `gc_mark`,
            // which casts it back to the allocator for this collection.
            unsafe { mark(self.data, alloc as *mut _ as *mut MrMarker) };
        }
    }

    fn dup(&self) -> Option<Box<dyn NativeData>> {
        if self.data.is_null() || self.ops.is_null() {
            return None;
        }
        // SAFETY: as in `release`.
        let dup = unsafe { (*self.ops).dup }?;
        // SAFETY: the extension's own copier over its own payload.
        let data = unsafe { dup(self.data) };
        if data.is_null() {
            return None;
        }
        Some(Box::new(ExtNative {
            data,
            ops: self.ops,
        }))
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

/// The allocator of every `MR_CLASS_NATIVE` class: an instance with no
/// payload yet, for the extension's `initialize` to fill with `native_set`.
extern "C" fn ext_alloc_func(class_id: ClassId, _globals: &mut Globals) -> Value {
    Value::new_native(class_id, Box::new(ExtNative::empty()))
}

// ---------------------------------------------------------------------
// The context and the trampoline
// ---------------------------------------------------------------------

fn make_ctx(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Option<Lfp>,
    pc: Option<BytecodePtr>,
) -> MrContext {
    MrContext {
        api: &MR_API,
        vm: vm as *mut Executor as *mut c_void,
        globals: globals as *mut Globals as *mut c_void,
        frame: match lfp {
            Some(lfp) => lfp.as_ptr() as *mut c_void,
            None => std::ptr::null_mut(),
        },
        pc: match pc {
            // SAFETY: `BytecodePtr` is a transparent non-null pointer.
            Some(pc) => unsafe { std::mem::transmute::<BytecodePtr, *mut c_void>(pc) },
            None => std::ptr::null_mut(),
        },
    }
}

/// The interpreter behind a context. Every table entry starts here.
///
/// # Safety
/// `ctx` must be a context this module built, still within the call it
/// was built for.
unsafe fn parts<'a>(ctx: *mut MrContext) -> (&'a mut Executor, &'a mut Globals) {
    // SAFETY: the caller's contract; the two pointers were `&mut`
    // borrows for the duration of the call.
    unsafe {
        (
            &mut *((*ctx).vm as *mut Executor),
            &mut *((*ctx).globals as *mut Globals),
        )
    }
}

/// Leave `err` pending unless one already is: an extension that keeps
/// calling the API after a failure must not trip `set_error`'s
/// double-set assertion, and the first error is the one to report.
fn set_err(vm: &mut Executor, err: MonorubyErr) {
    if !vm.has_error() {
        vm.set_error(err);
    }
}

fn val(bits: MrValue) -> Option<Value> {
    if bits == MR_UNDEF {
        None
    } else {
        Some(Value::from_u64(bits))
    }
}

/// # Safety
/// `name` must be a NUL-terminated string.
unsafe fn cstr<'a>(name: *const c_char) -> &'a str {
    // SAFETY: the caller's contract.
    unsafe { CStr::from_ptr(name) }.to_str().unwrap_or("")
}

/// # Safety
/// `ptr` must point at `len` readable bytes.
unsafe fn slice<'a>(ptr: *const u8, len: usize) -> &'a [u8] {
    if len == 0 {
        return &[];
    }
    // SAFETY: the caller's contract.
    unsafe { std::slice::from_raw_parts(ptr, len) }
}

/// # Safety
/// `argv` must point at `argc` values.
unsafe fn args(argv: *const MrValue, argc: c_int) -> Vec<Value> {
    // SAFETY: the caller's contract; `MrValue` and `Value` share a layout
    // except that `Value` is non-zero, which `val` checks.
    unsafe { slice(argv as *const u8, 0) };
    let n = argc.max(0) as usize;
    (0..n)
        // SAFETY: as above.
        .map(|i| unsafe { *argv.add(i) })
        .filter_map(val)
        .collect()
}

/// The `BuiltinFn` of every extension method.
extern "C" fn ext_method_trampoline(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Option<Value> {
    let fid = lfp.func_id();
    let Some(m) = globals.ext.methods.get(&fid).copied() else {
        vm.set_error(MonorubyErr::runtimeerr(
            "extension method table entry is missing",
        ));
        return None;
    };
    let mut argv: smallvec::SmallVec<[MrValue; 8]> = smallvec::SmallVec::new();
    if m.argc == MR_ARGC_VARIADIC {
        argv.extend(lfp.arg(0).as_array_inner().iter().map(|v| v.id()));
    } else {
        argv.extend((0..m.argc as usize).map(|i| lfp.arg(i).id()));
    }
    let block = lfp.block().map_or(MR_UNDEF, |bh| bh.get().id());
    let temp_base = vm.temp_len();
    let mut ctx = make_ctx(vm, globals, Some(lfp), Some(pc));
    // SAFETY: the extension's own function, with the arguments it declared.
    let res = unsafe {
        (m.f)(
            &mut ctx,
            lfp.self_val().id(),
            argv.len() as c_int,
            argv.as_ptr(),
            block,
        )
    };
    vm.temp_clear(temp_base);
    match val(res) {
        Some(v) if !vm.has_error() => Some(v),
        Some(_) => None,
        None => {
            if !vm.has_error() {
                vm.set_error(MonorubyErr::runtimeerr(format!(
                    "extension method `{}` returned MR_UNDEF without raising",
                    globals.store[fid]
                        .name()
                        .map(|n| n.get_name())
                        .unwrap_or_default()
                )));
            }
            let mut err = vm.take_error();
            crate::builtins::kernel::push_builtin_trace(vm, globals, &mut err, fid);
            vm.set_error(err);
            None
        }
    }
}

// ---------------------------------------------------------------------
// Loading
// ---------------------------------------------------------------------

const DLEXT: &str = if cfg!(target_os = "macos") {
    "dylib"
} else {
    "so"
};

/// The directories searched for `lib<name>.<DLEXT>`, in order: the
/// `MONORUBY_EXT_PATH` entries, the running binary's directory (with its
/// parent when the binary is a test executable in `deps/`, and its `deps/`
/// when it is not — where `cargo build` leaves a workspace member's
/// cdylib), and the install root's `ext/`.
fn search_dirs() -> Vec<PathBuf> {
    let mut dirs = extra_dirs().lock().unwrap().clone();
    if let Some(p) = std::env::var_os("MONORUBY_EXT_PATH") {
        dirs.extend(std::env::split_paths(&p));
    }
    if let Ok(exe) = std::env::current_exe()
        && let Some(dir) = exe.parent()
    {
        dirs.push(dir.to_path_buf());
        if dir.file_name().is_some_and(|n| n == "deps") {
            if let Some(parent) = dir.parent() {
                dirs.push(parent.to_path_buf());
            }
        } else {
            dirs.push(dir.join("deps"));
        }
    }
    dirs.push(crate::globals::install_root().join("ext"));
    dirs
}

/// Directories added at run time, ahead of the rest — the test harness
/// builds extension crates into their own target dir and registers it.
fn extra_dirs() -> &'static std::sync::Mutex<Vec<PathBuf>> {
    static EXTRA: std::sync::OnceLock<std::sync::Mutex<Vec<PathBuf>>> = std::sync::OnceLock::new();
    EXTRA.get_or_init(|| std::sync::Mutex::new(vec![]))
}

pub fn add_search_dir(dir: PathBuf) {
    let dir = dir.canonicalize().unwrap_or(dir);
    let mut dirs = extra_dirs().lock().unwrap();
    if !dirs.contains(&dir) {
        dirs.push(dir);
    }
}

/// The shared library `require "<stem>.so"` loads, if one is installed.
pub(crate) fn find_extension(stem: &str) -> Option<PathBuf> {
    let file = format!("lib{stem}.{DLEXT}");
    let plain = format!("{stem}.{DLEXT}");
    search_dirs()
        .into_iter()
        .flat_map(|d| [d.join(&file), d.join(&plain)])
        .find(|p| p.is_file())
}

/// Whether `path` is a file `find_extension` would have answered — the
/// require resolver's cue to load it natively rather than as Ruby.
pub(crate) fn is_extension_path(path: &Path) -> bool {
    let same = |a: &Path, b: &Path| {
        a == b || matches!((a.canonicalize(), b.canonicalize()), (Ok(a), Ok(b)) if a == b)
    };
    path.extension().is_some_and(|e| e == DLEXT)
        && path
            .parent()
            .is_some_and(|dir| search_dirs().iter().any(|d| same(d, dir)))
}

/// `Init_<name>`, `name` being the file stem without its `lib` prefix,
/// with `-` as `_` (a crate named `foo-bar` builds `libfoo_bar.so`).
fn init_symbol(path: &Path) -> String {
    let stem = path
        .file_stem()
        .map(|s| s.to_string_lossy())
        .unwrap_or_default();
    let stem = stem.strip_prefix("lib").unwrap_or(&stem);
    format!("Init_{}", stem.replace('-', "_"))
}

/// `dlopen` the extension at `path` and run its `Init_`.
pub(crate) fn load_extension(vm: &mut Executor, globals: &mut Globals, path: &Path) -> Result<()> {
    use std::os::unix::ffi::OsStrExt;
    let cpath = std::ffi::CString::new(path.as_os_str().as_bytes())
        .map_err(|_| MonorubyErr::loaderr("extension path contains NUL", path.to_path_buf()))?;
    // SAFETY: plain libc calls with a valid C string.
    let handle = unsafe { libc::dlopen(cpath.as_ptr(), libc::RTLD_NOW | libc::RTLD_LOCAL) };
    if handle.is_null() {
        return Err(MonorubyErr::loaderr(
            format!("{} -- {}", dlerror(), path.display()),
            path.to_path_buf(),
        ));
    }
    let sym = std::ffi::CString::new(init_symbol(path)).unwrap();
    // SAFETY: as above.
    let init = unsafe { libc::dlsym(handle, sym.as_ptr()) };
    if init.is_null() {
        return Err(MonorubyErr::loaderr(
            format!(
                "{} is not a monoruby extension: no {}",
                path.display(),
                sym.to_string_lossy()
            ),
            path.to_path_buf(),
        ));
    }
    // SAFETY: the symbol is the extension's `Init_`, declared with `MrInitFn`'s type.
    let init: MrInitFn = unsafe { std::mem::transmute::<*mut c_void, MrInitFn>(init) };
    let temp_base = vm.temp_len();
    let mut ctx = make_ctx(vm, globals, None, None);
    // SAFETY: the extension's initializer, with the context it expects.
    let rc = unsafe { init(&mut ctx) };
    vm.temp_clear(temp_base);
    if vm.has_error() {
        return Err(vm.take_error());
    }
    if rc != 0 {
        return Err(MonorubyErr::loaderr(
            format!(
                "{} returned {rc} from {}",
                path.display(),
                sym.to_string_lossy()
            ),
            path.to_path_buf(),
        ));
    }
    globals.ext.handles.push(handle as usize);
    Ok(())
}

fn dlerror() -> String {
    // SAFETY: plain libc call; the returned string is valid until the
    // next dl* call, and copied out here.
    unsafe {
        let p = libc::dlerror();
        if p.is_null() {
            "dlopen failed".to_string()
        } else {
            CStr::from_ptr(p).to_string_lossy().into_owned()
        }
    }
}

// ---------------------------------------------------------------------
// The table
// ---------------------------------------------------------------------

pub(crate) static MR_API: MrApi = MrApi {
    abi_version: MR_ABI_VERSION,
    size: std::mem::size_of::<MrApi>() as u32,
    raise: mr_raise,
    raise_kind: mr_raise_kind,
    error_pending: mr_error_pending,
    error_take: mr_error_take,
    raise_exception: mr_raise_exception,
    object_class: mr_object_class,
    define_class: mr_define_class,
    define_module: mr_define_module,
    define_method: mr_define_method,
    const_get: mr_const_get,
    const_set: mr_const_set,
    type_of: mr_type_of,
    class_of: mr_class_of,
    is_kind_of: mr_is_kind_of,
    int_new: mr_int_new,
    int_get: mr_int_get,
    float_new: mr_float_new,
    float_get: mr_float_get,
    str_new: mr_str_new,
    bytes_new: mr_bytes_new,
    str_ptr: mr_str_ptr,
    str_encoding: mr_str_encoding,
    sym_new: mr_sym_new,
    sym_to_str: mr_sym_to_str,
    ary_new: mr_ary_new,
    ary_push: mr_ary_push,
    ary_len: mr_ary_len,
    ary_get: mr_ary_get,
    hash_new: mr_hash_new,
    hash_set: mr_hash_set,
    hash_get: mr_hash_get,
    ivar_get: mr_ivar_get,
    ivar_set: mr_ivar_set,
    inspect: mr_inspect,
    respond_to: mr_respond_to,
    native_new: mr_native_new,
    native_data: mr_native_data,
    native_set: mr_native_set,
    gc_mark: mr_gc_mark,
    temp_push: mr_temp_push,
    temp_len: mr_temp_len,
    temp_truncate: mr_temp_truncate,
    gc_pin: mr_gc_pin,
    gc_unpin: mr_gc_unpin,
    funcall: mr_funcall,
    yield_block: mr_yield_block,
    block_to_proc: mr_block_to_proc,
    proc_call: mr_proc_call,
    call_blocking: mr_call_blocking,
    ruby_version: mr_ruby_version,
};

// ---- errors ----------------------------------------------------------

/// `klass.new(msg)` as a `MonorubyErr`: the exception's own class, so a
/// `rescue Foo::Error` in Ruby matches.
fn exception_of(vm: &mut Executor, globals: &mut Globals, klass: Value, msg: &str) -> MonorubyErr {
    if klass.is_class().is_none() {
        return MonorubyErr::typeerr(format!("{} is not a class", klass.inspect(&globals.store)));
    }
    let m = Value::string_from_str(msg);
    match vm.invoke_method_inner(globals, IdentId::NEW, klass, &[m], None, None) {
        Ok(ex) => match ex.is_exception() {
            Some(inner) => MonorubyErr::new_from_exception(inner).with_original(ex),
            None => MonorubyErr::typeerr("exception class/object expected"),
        },
        Err(e) => e,
    }
}

unsafe extern "C" fn mr_raise(
    ctx: *mut MrContext,
    exc_class: MrValue,
    msg: *const c_char,
    len: usize,
) {
    // SAFETY: the table's contract (a live context, `len` bytes at `msg`).
    let (vm, globals) = unsafe { parts(ctx) };
    let msg = String::from_utf8_lossy(unsafe { slice(msg as *const u8, len) }).into_owned();
    let err = match val(exc_class) {
        Some(klass) => exception_of(vm, globals, klass, &msg),
        None => MonorubyErr::runtimeerr(msg),
    };
    set_err(vm, err);
}

unsafe extern "C" fn mr_raise_kind(
    ctx: *mut MrContext,
    kind: MrErrorKind,
    msg: *const c_char,
    len: usize,
) {
    // SAFETY: the table's contract.
    let (vm, _) = unsafe { parts(ctx) };
    let msg = String::from_utf8_lossy(unsafe { slice(msg as *const u8, len) }).into_owned();
    let err = match kind {
        MrErrorKind::Runtime => MonorubyErr::runtimeerr(msg),
        MrErrorKind::Argument => MonorubyErr::argumenterr(msg),
        MrErrorKind::Type => MonorubyErr::typeerr(msg),
        MrErrorKind::Range => MonorubyErr::rangeerr(msg),
        MrErrorKind::Index => MonorubyErr::indexerr(msg),
        MrErrorKind::IO => MonorubyErr::ioerr(msg),
        MrErrorKind::NotImplemented => MonorubyErr::new(MonorubyErrKind::Unimplemented, msg),
        MrErrorKind::Frozen => MonorubyErr::new(MonorubyErrKind::Frozen(None), msg),
        MrErrorKind::Load => MonorubyErr::loaderr(msg, PathBuf::new()),
    };
    set_err(vm, err);
}

unsafe extern "C" fn mr_error_pending(ctx: *mut MrContext) -> c_int {
    // SAFETY: the table's contract.
    let (vm, _) = unsafe { parts(ctx) };
    vm.has_error() as c_int
}

unsafe extern "C" fn mr_error_take(ctx: *mut MrContext) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, _) = unsafe { parts(ctx) };
    if vm.has_error() {
        Value::new_exception(vm.take_error()).id()
    } else {
        MR_UNDEF
    }
}

unsafe extern "C" fn mr_raise_exception(ctx: *mut MrContext, exc: MrValue) {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let err = match val(exc) {
        Some(ex) => match ex.is_exception() {
            Some(inner) => MonorubyErr::new_from_exception(inner).with_original(ex),
            None => MonorubyErr::typeerr(format!(
                "exception object expected, got {}",
                ex.inspect(&globals.store)
            )),
        },
        None => MonorubyErr::runtimeerr("unknown error"),
    };
    set_err(vm, err);
}

// ---- classes ---------------------------------------------------------

/// `outer` as a class id, `Object` for `MR_UNDEF`.
fn outer_of(vm: &mut Executor, globals: &Globals, outer: MrValue) -> Option<ClassId> {
    match val(outer) {
        None => Some(OBJECT_CLASS),
        Some(v) => match v.is_class_or_module() {
            Some(m) => Some(m.id()),
            None => {
                set_err(
                    vm,
                    MonorubyErr::typeerr(format!(
                        "{} is not a class/module",
                        v.inspect(&globals.store)
                    )),
                );
                None
            }
        },
    }
}

unsafe extern "C" fn mr_object_class(ctx: *mut MrContext) -> MrValue {
    // SAFETY: the table's contract.
    let (_, globals) = unsafe { parts(ctx) };
    globals.store.object_class().as_val().id()
}

unsafe extern "C" fn mr_define_class(
    ctx: *mut MrContext,
    outer: MrValue,
    name: *const c_char,
    superclass: MrValue,
    flags: u32,
) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let name = unsafe { cstr(name) };
    let Some(outer) = outer_of(vm, globals, outer) else {
        return MR_UNDEF;
    };
    let name_id = IdentId::get_id(name);
    if let Some(existing) = globals.store.get_constant_noautoload(outer, name_id) {
        if existing.is_class().is_some() {
            return existing.id();
        }
        set_err(vm, MonorubyErr::typeerr(format!("{name} is not a class")));
        return MR_UNDEF;
    }
    let superclass = match val(superclass) {
        None => globals.store.object_class(),
        Some(v) => match v.is_class() {
            Some(m) => m,
            None => {
                set_err(vm, MonorubyErr::typeerr("superclass must be a Class"));
                return MR_UNDEF;
            }
        },
    };
    let module = if flags & MR_CLASS_NATIVE != 0 {
        let m = globals
            .store
            .define_class_with_instance_ty(name, superclass, outer, ObjTy::NATIVE);
        globals.store[m.id()].set_alloc_func(ext_alloc_func);
        m
    } else {
        globals.store.define_class(name, superclass, outer)
    };
    module.as_val().id()
}

unsafe extern "C" fn mr_define_module(
    ctx: *mut MrContext,
    outer: MrValue,
    name: *const c_char,
) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let name = unsafe { cstr(name) };
    let Some(outer) = outer_of(vm, globals, outer) else {
        return MR_UNDEF;
    };
    let name_id = IdentId::get_id(name);
    if let Some(existing) = globals.store.get_constant_noautoload(outer, name_id) {
        if existing.is_module().is_some() {
            return existing.id();
        }
        set_err(vm, MonorubyErr::typeerr(format!("{name} is not a module")));
        return MR_UNDEF;
    }
    globals
        .store
        .define_module_with_identid(name_id, outer)
        .as_val()
        .id()
}

unsafe extern "C" fn mr_define_method(
    ctx: *mut MrContext,
    klass: MrValue,
    name: *const c_char,
    f: MrMethodFn,
    argc: c_int,
    flags: u32,
) {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let name = unsafe { cstr(name) };
    let Some(class_id) = val(klass)
        .and_then(|v| v.is_class_or_module())
        .map(|m| m.id())
    else {
        set_err(
            vm,
            MonorubyErr::typeerr("define_method: class/module expected"),
        );
        return;
    };
    let t: BuiltinFn = ext_method_trampoline;
    let singleton = flags & MR_METHOD_SINGLETON != 0;
    let private = flags & MR_METHOD_PRIVATE != 0;
    let fid = match (argc == MR_ARGC_VARIADIC, singleton, private) {
        (false, false, false) => globals.define_builtin_func(class_id, name, t, argc as usize),
        (false, false, true) => {
            globals.define_private_builtin_func(class_id, name, t, argc as usize)
        }
        (false, true, _) => globals.define_builtin_class_func(class_id, name, t, argc as usize),
        (true, false, false) => globals.define_builtin_func_rest(class_id, name, t),
        (true, false, true) => globals.define_private_builtin_func_rest(class_id, name, t),
        (true, true, _) => globals.define_builtin_class_func_rest(class_id, name, t),
    };
    globals.ext.methods.insert(fid, ExtMethod { f, argc });
}

unsafe extern "C" fn mr_const_get(
    ctx: *mut MrContext,
    outer: MrValue,
    name: *const c_char,
) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let name = unsafe { cstr(name) };
    let Some(outer) = outer_of(vm, globals, outer) else {
        return MR_UNDEF;
    };
    globals
        .store
        .get_constant_noautoload(outer, IdentId::get_id(name))
        .map_or(MR_UNDEF, |v| v.id())
}

unsafe extern "C" fn mr_const_set(
    ctx: *mut MrContext,
    outer: MrValue,
    name: *const c_char,
    v: MrValue,
) {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let name = unsafe { cstr(name) };
    let Some(outer) = outer_of(vm, globals, outer) else {
        return;
    };
    let Some(v) = val(v) else { return };
    globals.set_constant(outer, IdentId::get_id(name), v);
}

// ---- values ----------------------------------------------------------

unsafe extern "C" fn mr_type_of(_: *mut MrContext, v: MrValue) -> MrType {
    let Some(v) = val(v) else {
        return MrType::Undef;
    };
    if v.is_nil() {
        return MrType::Nil;
    }
    if v == Value::bool(true) {
        return MrType::True;
    }
    if v == Value::bool(false) {
        return MrType::False;
    }
    if v.is_fixnum() {
        return MrType::Integer;
    }
    if v.is_float() {
        return MrType::Float;
    }
    if v.try_symbol().is_some() {
        return MrType::Symbol;
    }
    match v.try_rvalue().map(|rv| rv.ty()) {
        Some(ObjTy::BIGNUM) => MrType::Integer,
        Some(ObjTy::FLOAT) => MrType::Float,
        Some(ObjTy::STRING) => MrType::String,
        Some(ObjTy::ARRAY) => MrType::Array,
        Some(ObjTy::HASH) => MrType::Hash,
        Some(ObjTy::CLASS) => MrType::Class,
        Some(ObjTy::MODULE) => MrType::Module,
        Some(ObjTy::PROC) => MrType::Proc,
        Some(ObjTy::EXCEPTION) => MrType::Exception,
        Some(ObjTy::NATIVE) => MrType::Native,
        _ => MrType::Object,
    }
}

unsafe extern "C" fn mr_class_of(ctx: *mut MrContext, v: MrValue) -> MrValue {
    // SAFETY: the table's contract.
    let (_, globals) = unsafe { parts(ctx) };
    let Some(v) = val(v) else { return MR_UNDEF };
    v.get_class_obj(&globals.store).as_val().id()
}

unsafe extern "C" fn mr_is_kind_of(ctx: *mut MrContext, v: MrValue, klass: MrValue) -> c_int {
    // SAFETY: the table's contract.
    let (_, globals) = unsafe { parts(ctx) };
    let (Some(v), Some(klass)) = (val(v), val(klass).and_then(|k| k.is_class_or_module())) else {
        return 0;
    };
    v.is_kind_of(&globals.store, klass.id()) as c_int
}

unsafe extern "C" fn mr_int_new(i: i64) -> MrValue {
    Value::integer(i).id()
}

unsafe extern "C" fn mr_int_get(ctx: *mut MrContext, v: MrValue, out: *mut i64) -> c_int {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(v) = val(v) else { return 0 };
    match v.coerce_to_int_i64(vm, globals) {
        Ok(i) => {
            // SAFETY: `out` is the caller's slot.
            unsafe { *out = i };
            1
        }
        Err(e) => {
            set_err(vm, e);
            0
        }
    }
}

unsafe extern "C" fn mr_float_new(f: f64) -> MrValue {
    Value::float(f).id()
}

unsafe extern "C" fn mr_float_get(ctx: *mut MrContext, v: MrValue, out: *mut f64) -> c_int {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(v) = val(v) else { return 0 };
    match v.coerce_to_f64(vm, globals) {
        Ok(f) => {
            // SAFETY: `out` is the caller's slot.
            unsafe { *out = f };
            1
        }
        Err(e) => {
            set_err(vm, e);
            0
        }
    }
}

unsafe extern "C" fn mr_str_new(_: *mut MrContext, ptr: *const u8, len: usize) -> MrValue {
    // SAFETY: the table's contract.
    let bytes = unsafe { slice(ptr, len) };
    Value::string_from_vec(bytes.to_vec()).id()
}

unsafe extern "C" fn mr_bytes_new(_: *mut MrContext, ptr: *const u8, len: usize) -> MrValue {
    // SAFETY: the table's contract.
    let bytes = unsafe { slice(ptr, len) };
    Value::bytes_from_slice(bytes).id()
}

unsafe extern "C" fn mr_str_ptr(ctx: *mut MrContext, v: MrValue, len: *mut usize) -> *const u8 {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(v) = val(v) else {
        return std::ptr::null();
    };
    if v.try_rvalue().is_none_or(|rv| rv.ty() != ObjTy::STRING) {
        set_err(
            vm,
            MonorubyErr::no_implicit_conversion(&globals.store, v, STRING_CLASS),
        );
        return std::ptr::null();
    }
    let bytes = v.as_rstring_inner().as_bytes();
    // SAFETY: `len` is the caller's slot.
    unsafe { *len = bytes.len() };
    bytes.as_ptr()
}

unsafe extern "C" fn mr_sym_new(_: *mut MrContext, ptr: *const u8, len: usize) -> MrValue {
    // SAFETY: the table's contract.
    let bytes = unsafe { slice(ptr, len) };
    Value::symbol_from_str(&String::from_utf8_lossy(bytes)).id()
}

unsafe extern "C" fn mr_sym_to_str(ctx: *mut MrContext, v: MrValue) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, _) = unsafe { parts(ctx) };
    match val(v) {
        Some(v) if v.try_symbol().is_some() => Value::string(v.as_symbol().get_name()).id(),
        _ => {
            set_err(vm, MonorubyErr::typeerr("Symbol expected"));
            MR_UNDEF
        }
    }
}

unsafe extern "C" fn mr_ary_new(_: *mut MrContext) -> MrValue {
    Value::array_empty().id()
}

unsafe extern "C" fn mr_ary_push(ctx: *mut MrContext, ary: MrValue, v: MrValue) -> c_int {
    // SAFETY: the table's contract.
    let (vm, _) = unsafe { parts(ctx) };
    let (Some(ary), Some(v)) = (val(ary), val(v)) else {
        return -1;
    };
    match ary.try_array_ty() {
        Some(mut a) => {
            a.push(v);
            0
        }
        None => {
            set_err(vm, MonorubyErr::typeerr("Array expected"));
            -1
        }
    }
}

unsafe extern "C" fn mr_ary_len(_: *mut MrContext, ary: MrValue) -> usize {
    val(ary)
        .and_then(|a| a.try_array_ty())
        .map_or(0, |a| a.len())
}

unsafe extern "C" fn mr_ary_get(_: *mut MrContext, ary: MrValue, idx: usize) -> MrValue {
    val(ary)
        .and_then(|a| a.try_array_ty())
        .and_then(|a| a.get(idx).copied())
        .map_or(MR_UNDEF, |v| v.id())
}

unsafe extern "C" fn mr_hash_new(_: *mut MrContext) -> MrValue {
    Value::hash_from_inner(HashmapInner::default()).id()
}

unsafe extern "C" fn mr_hash_set(
    ctx: *mut MrContext,
    hash: MrValue,
    key: MrValue,
    v: MrValue,
) -> c_int {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let (Some(hash), Some(key), Some(v)) = (val(hash), val(key), val(v)) else {
        return -1;
    };
    let Some(mut h) = hash.try_hash_ty() else {
        set_err(vm, MonorubyErr::typeerr("Hash expected"));
        return -1;
    };
    match h.insert(key, v, vm, globals) {
        Ok(()) => 0,
        Err(e) => {
            set_err(vm, e);
            -1
        }
    }
}

unsafe extern "C" fn mr_hash_get(ctx: *mut MrContext, hash: MrValue, key: MrValue) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let (Some(hash), Some(key)) = (val(hash), val(key)) else {
        return MR_UNDEF;
    };
    let Some(h) = hash.try_hash_ty() else {
        set_err(vm, MonorubyErr::typeerr("Hash expected"));
        return MR_UNDEF;
    };
    match h.get(key, vm, globals) {
        Ok(Some(v)) => v.id(),
        Ok(None) => MR_UNDEF,
        Err(e) => {
            set_err(vm, e);
            MR_UNDEF
        }
    }
}

unsafe extern "C" fn mr_ivar_get(
    ctx: *mut MrContext,
    obj: MrValue,
    name: *const c_char,
) -> MrValue {
    // SAFETY: the table's contract.
    let (_, globals) = unsafe { parts(ctx) };
    let name = unsafe { cstr(name) };
    let Some(obj) = val(obj) else { return MR_UNDEF };
    globals
        .store
        .get_ivar(obj, IdentId::get_id(name))
        .unwrap_or(Value::nil())
        .id()
}

unsafe extern "C" fn mr_ivar_set(
    ctx: *mut MrContext,
    obj: MrValue,
    name: *const c_char,
    v: MrValue,
) -> c_int {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let name = unsafe { cstr(name) };
    let (Some(obj), Some(v)) = (val(obj), val(v)) else {
        return -1;
    };
    match globals.store.set_ivar(obj, IdentId::get_id(name), v) {
        Ok(()) => 0,
        Err(e) => {
            set_err(vm, e);
            -1
        }
    }
}

unsafe extern "C" fn mr_inspect(ctx: *mut MrContext, v: MrValue) -> MrValue {
    // SAFETY: the table's contract.
    let (_, globals) = unsafe { parts(ctx) };
    let Some(v) = val(v) else { return MR_UNDEF };
    Value::string(v.inspect(&globals.store)).id()
}

// ---- native objects --------------------------------------------------

unsafe extern "C" fn mr_native_new(
    ctx: *mut MrContext,
    klass: MrValue,
    data: *mut c_void,
    ops: *const MrNativeOps,
) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, _) = unsafe { parts(ctx) };
    let Some(class_id) = val(klass).and_then(|v| v.is_class()).map(|m| m.id()) else {
        set_err(vm, MonorubyErr::typeerr("native_new: class expected"));
        return MR_UNDEF;
    };
    Value::new_native(class_id, Box::new(ExtNative { data, ops })).id()
}

unsafe extern "C" fn mr_native_data(
    _: *mut MrContext,
    obj: MrValue,
    ops: *const MrNativeOps,
) -> *mut c_void {
    let Some(obj) = val(obj) else {
        return std::ptr::null_mut();
    };
    match obj.try_native::<ExtNative>() {
        Some(n) if ops.is_null() || n.ops == ops => n.data,
        _ => std::ptr::null_mut(),
    }
}

unsafe extern "C" fn mr_native_set(
    ctx: *mut MrContext,
    obj: MrValue,
    data: *mut c_void,
    ops: *const MrNativeOps,
) -> c_int {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(mut obj) = val(obj) else { return -1 };
    let Some(rv) = obj.try_rvalue_mut().filter(|rv| rv.ty() == ObjTy::NATIVE) else {
        set_err(
            vm,
            MonorubyErr::typeerr(format!(
                "{} is not a native object",
                obj.inspect(&globals.store)
            )),
        );
        return -1;
    };
    match rv.as_native_mut().as_any_mut().downcast_mut::<ExtNative>() {
        Some(n) => {
            n.release();
            n.data = data;
            n.ops = ops;
        }
        None => rv.replace_native(Box::new(ExtNative { data, ops })),
    }
    0
}

// ---- GC --------------------------------------------------------------

unsafe extern "C" fn mr_gc_mark(marker: *mut MrMarker, v: MrValue) {
    let Some(v) = val(v) else { return };
    // SAFETY: `marker` is the allocator `ExtNative::mark` passed, cast back.
    let alloc = unsafe { &mut *(marker as *mut alloc::Allocator<RValue>) };
    v.mark(alloc);
}

unsafe extern "C" fn mr_temp_push(ctx: *mut MrContext, v: MrValue) {
    // SAFETY: the table's contract.
    let (vm, _) = unsafe { parts(ctx) };
    if let Some(v) = val(v) {
        vm.temp_push(v);
    }
}

unsafe extern "C" fn mr_temp_len(ctx: *mut MrContext) -> usize {
    // SAFETY: the table's contract.
    let (vm, _) = unsafe { parts(ctx) };
    vm.temp_len()
}

unsafe extern "C" fn mr_temp_truncate(ctx: *mut MrContext, len: usize) {
    // SAFETY: the table's contract.
    let (vm, _) = unsafe { parts(ctx) };
    vm.temp_clear(len);
}

unsafe extern "C" fn mr_gc_pin(ctx: *mut MrContext, v: MrValue) {
    // SAFETY: the table's contract.
    let (_, globals) = unsafe { parts(ctx) };
    if val(v).is_some_and(|v| !v.is_packed_value()) {
        *globals.ext.pins.entry(v).or_insert(0) += 1;
    }
}

unsafe extern "C" fn mr_gc_unpin(ctx: *mut MrContext, v: MrValue) {
    // SAFETY: the table's contract.
    let (_, globals) = unsafe { parts(ctx) };
    if let Some(n) = globals.ext.pins.get_mut(&v) {
        *n -= 1;
        if *n == 0 {
            globals.ext.pins.remove(&v);
        }
    }
}

// ---- calling Ruby ----------------------------------------------------

fn block_handler(block: MrValue) -> Option<BlockHandler> {
    val(block).map(BlockHandler::new)
}

unsafe extern "C" fn mr_funcall(
    ctx: *mut MrContext,
    recv: MrValue,
    name: *const c_char,
    argc: c_int,
    argv: *const MrValue,
    block: MrValue,
) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let name = unsafe { cstr(name) };
    let Some(recv) = val(recv) else {
        return MR_UNDEF;
    };
    let args = unsafe { args(argv, argc) };
    match vm.invoke_method_inner(
        globals,
        IdentId::get_id(name),
        recv,
        &args,
        block_handler(block),
        None,
    ) {
        Ok(v) => v.id(),
        Err(e) => {
            set_err(vm, e);
            MR_UNDEF
        }
    }
}

unsafe extern "C" fn mr_yield_block(
    ctx: *mut MrContext,
    block: MrValue,
    argc: c_int,
    argv: *const MrValue,
) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(bh) = block_handler(block) else {
        set_err(vm, MonorubyErr::localjumperr("no block given (yield)"));
        return MR_UNDEF;
    };
    let args = unsafe { args(argv, argc) };
    match vm.invoke_block_once(globals, bh, &args) {
        Ok(v) => v.id(),
        Err(e) => {
            set_err(vm, e);
            MR_UNDEF
        }
    }
}

unsafe extern "C" fn mr_block_to_proc(ctx: *mut MrContext, block: MrValue) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(bh) = block_handler(block) else {
        return MR_NIL;
    };
    // SAFETY: the context's `pc` is the calling frame's, set by the
    // trampoline; null only inside `Init_`, where there is no block.
    let pc = unsafe { (*ctx).pc };
    if pc.is_null() {
        set_err(
            vm,
            MonorubyErr::runtimeerr("block_to_proc outside a method call"),
        );
        return MR_UNDEF;
    }
    // SAFETY: the reverse of `make_ctx`'s transmute.
    let pc = unsafe { std::mem::transmute::<*mut c_void, BytecodePtr>(pc) };
    match vm.generate_proc(globals, bh, pc) {
        Ok(p) => {
            let v: Value = p.into();
            v.id()
        }
        Err(e) => {
            set_err(vm, e);
            MR_UNDEF
        }
    }
}

unsafe extern "C" fn mr_proc_call(
    ctx: *mut MrContext,
    proc_: MrValue,
    argc: c_int,
    argv: *const MrValue,
) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(p) = val(proc_) else { return MR_UNDEF };
    if p.is_proc().is_none() {
        set_err(
            vm,
            MonorubyErr::typeerr(format!("Proc expected, got {}", p.inspect(&globals.store))),
        );
        return MR_UNDEF;
    }
    let args = unsafe { args(argv, argc) };
    match vm.invoke_proc(globals, p.as_proc_inner(), &args) {
        Ok(v) => v.id(),
        Err(e) => {
            set_err(vm, e);
            MR_UNDEF
        }
    }
}

// ---- threads ---------------------------------------------------------

/// What `call_blocking` hands the native pool: the extension's function
/// and its argument, raw — the worker touches no Ruby state.
pub(crate) struct ExtWorkerCall {
    f: MrBlockingFn,
    arg: usize,
}

impl ExtWorkerCall {
    pub(crate) fn run(&self) -> i64 {
        // SAFETY: the extension's own function with its own argument,
        // declared not to touch the interpreter.
        unsafe { (self.f)(self.arg as *mut c_void) }
    }
}

unsafe extern "C" fn mr_call_blocking(
    ctx: *mut MrContext,
    f: MrBlockingFn,
    arg: *mut c_void,
    out: *mut i64,
) -> c_int {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let call = ExtWorkerCall {
        f,
        arg: arg as usize,
    };
    match crate::native_pool::run_blocking(vm, globals, crate::native_pool::NativeOp::Ext(call)) {
        Ok(c) => {
            // SAFETY: `out` is the caller's slot.
            unsafe { *out = c.ret };
            0
        }
        Err(e) => {
            set_err(vm, e);
            -1
        }
    }
}

// ---- misc ------------------------------------------------------------

unsafe extern "C" fn mr_ruby_version() -> *const c_char {
    concat!(env!("MONORUBY_RUBY_VERSION"), "\0").as_ptr() as *const c_char
}

unsafe extern "C" fn mr_str_encoding(ctx: *mut MrContext, v: MrValue) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(v) = val(v) else { return MR_UNDEF };
    if v.try_rvalue().is_none_or(|rv| rv.ty() != ObjTy::STRING) {
        set_err(
            vm,
            MonorubyErr::no_implicit_conversion(&globals.store, v, STRING_CLASS),
        );
        return MR_UNDEF;
    }
    Value::string_from_str(v.as_rstring_inner().encoding().name()).id()
}

unsafe extern "C" fn mr_respond_to(ctx: *mut MrContext, v: MrValue, name: *const c_char) -> c_int {
    // SAFETY: the table's contract.
    let (_, globals) = unsafe { parts(ctx) };
    let name = unsafe { cstr(name) };
    let Some(v) = val(v) else { return 0 };
    globals
        .store
        .check_method_for_class(v.class(), IdentId::get_id(name))
        .is_some() as c_int
}
