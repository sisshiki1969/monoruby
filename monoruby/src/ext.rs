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

mod dynsym;

// ---------------------------------------------------------------------
// Per-interpreter state
// ---------------------------------------------------------------------

/// The extension's entry point for one method.
#[derive(Clone, Copy)]
enum ExtFn {
    Plain(MrMethodFn),
    /// `define_method_data`: the function and the pointer handed back
    /// to it.
    Data(MrMethodDataFn, *mut c_void),
}

/// A method an extension defined: found from the trampoline through the
/// frame's `FuncId`.
#[derive(Clone, Copy)]
struct ExtMethod {
    f: ExtFn,
    /// `MR_ARGC_VARIADIC` when the builtin was registered with a rest
    /// parameter, whose Array the trampoline flattens into `argv`.
    argc: c_int,
}

#[derive(Default)]
pub(crate) struct ExtState {
    methods: fxhash::FxHashMap<FuncId, ExtMethod>,
    /// `gc_pin` counts, keyed by value bits; marked as roots.
    pins: fxhash::FxHashMap<u64, u32>,
    /// `gc_register_address` slots, read afresh at every collection.
    addresses: Vec<usize>,
    /// The `dlopen` handles, kept for the life of the process: an
    /// extension is never unloaded (its method pointers live in the
    /// function table).
    handles: Vec<usize>,
    /// Set by `Executor::invoke_func` when it enters an extension method
    /// with keywords (from Rust: an Enumerator replaying `each_line(chomp:
    /// true)`, `send`, an extension's `rb_funcallv_kw`), and taken by the
    /// trampoline the invoker reaches next. No Ruby runs in between.
    kw_pending: bool,
    /// The frames (local frame addresses) of the extension methods so
    /// entered and still running: their `rb_keyword_given_p` is yes.
    kw_frames: Vec<usize>,
    /// Names this interpreter's C-API extension mappings; see
    /// `CAPI_INSTANCES`.
    owner: CapiOwner,
}

/// Identifies one interpreter's C-API extension instances.
struct CapiOwner(u64);

impl Default for CapiOwner {
    fn default() -> Self {
        static NEXT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);
        CapiOwner(NEXT.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}

/// One `dlopen`ed mapping of a C-API extension, and the interpreter it
/// was initialized for.
struct CapiInstance {
    handle: usize,
    owner: u64,
}

/// Every C-API extension mapping in the process, by the path it was
/// required from.
///
/// An extension written for CRuby keeps its state in C statics (its
/// classes, cached IDs and VALUEs, and the layer's own binding to the
/// interpreter and its cached method objects), which CRuby, with one VM
/// per process, initializes once. A monoruby process can run several
/// interpreters — the test harness runs one per test thread — and
/// `dlopen` hands every one of them the same mapping, so a second
/// `Init_` would overwrite the first interpreter's statics under it, and
/// statics an `Init_` does not reassign (the layer's lazily filled
/// caches) would point into another interpreter's heap. Each interpreter
/// therefore gets a mapping of its own: the first one the original file,
/// every later one a private copy, which the dynamic loader maps afresh
/// because it is another inode. A mapping is never reused or unloaded —
/// the statics of an interpreter that ended describe a heap that is
/// gone — so a process that keeps starting interpreters keeps one
/// mapping per interpreter that loaded the extension.
static CAPI_INSTANCES: std::sync::Mutex<Option<HashMap<PathBuf, Vec<CapiInstance>>>> =
    std::sync::Mutex::new(None);

/// The handle of the mapping of the C-API extension at `path` that the
/// interpreter `owner` is to initialize; `handle` is the original's,
/// just opened (and that reference dropped again when another mapping
/// is used). See `CAPI_INSTANCES`.
fn claim_capi_instance(path: &Path, handle: *mut c_void, owner: u64) -> Result<*mut c_void> {
    use std::os::unix::ffi::OsStrExt;
    let mut map = CAPI_INSTANCES.lock().unwrap_or_else(|e| e.into_inner());
    let list = map
        .get_or_insert_with(HashMap::default)
        .entry(path.to_path_buf())
        .or_default();
    if let Some(inst) = list.iter().find(|i| i.owner == owner) {
        let h = inst.handle as *mut c_void;
        if h != handle {
            // SAFETY: drops the reference `load_extension` just took; the
            // original stays mapped for the instance that holds it.
            unsafe { libc::dlclose(handle) };
        }
        return Ok(h);
    }
    if list.is_empty() {
        list.push(CapiInstance {
            handle: handle as usize,
            owner,
        });
        return Ok(handle);
    }
    // Another interpreter holds the original: map a private copy. The copy keeps the
    // file name (`Init_` is looked up by it) in a directory of its own,
    // and is removed once mapped.
    static SEQ: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let dir = std::env::temp_dir().join(format!(
        "monoruby-capi-{}-{}",
        std::process::id(),
        SEQ.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
    ));
    let err = |e: std::io::Error| {
        MonorubyErr::loaderr(
            format!("cannot copy {}: {e}", path.display()),
            path.to_path_buf(),
        )
    };
    std::fs::create_dir_all(&dir).map_err(err)?;
    let copy = dir.join(path.file_name().unwrap_or_default());
    let res = std::fs::copy(path, &copy).map_err(err).and_then(|_| {
        let cpath = std::ffi::CString::new(copy.as_os_str().as_bytes()).unwrap();
        // SAFETY: plain libc call with a valid C string.
        let h = unsafe { libc::dlopen(cpath.as_ptr(), libc::RTLD_NOW | libc::RTLD_LOCAL) };
        if h.is_null() {
            Err(MonorubyErr::loaderr(
                format!("{} -- {}", dlerror(), copy.display()),
                path.to_path_buf(),
            ))
        } else {
            Ok(h)
        }
    });
    let _ = std::fs::remove_file(&copy);
    let _ = std::fs::remove_dir(&dir);
    let h = res?;
    // SAFETY: as above; the original stays mapped for its holder.
    unsafe { libc::dlclose(handle) };
    list.push(CapiInstance {
        handle: h as usize,
        owner,
    });
    Ok(h)
}

impl ExtState {
    /// See `kw_pending`.
    pub(crate) fn note_kw_entry(&mut self, func_id: FuncId) -> bool {
        let ext = self.methods.get(&func_id).is_some_and(|m| m.argc == MR_ARGC_VARIADIC);
        self.kw_pending |= ext;
        ext
    }

    pub(crate) fn clear_kw_entry(&mut self) {
        self.kw_pending = false;
    }

    pub(crate) fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        for bits in self.pins.keys() {
            Value::from_u64(*bits).mark(alloc);
        }
        for addr in &self.addresses {
            // SAFETY: the extension registered the address of a static
            // `MrValue` slot and keeps it valid until it unregisters it.
            let bits = unsafe { std::ptr::read_volatile(*addr as *const MrValue) };
            if let Some(v) = val(bits)
                && !v.is_packed_value()
            {
                v.mark(alloc);
            }
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
    let kw_frame = std::mem::take(&mut globals.ext.kw_pending).then(|| lfp.as_ptr() as usize);
    if let Some(frame) = kw_frame {
        globals.ext.kw_frames.push(frame);
    }
    let temp_base = vm.temp_len();
    let mut ctx = make_ctx(vm, globals, Some(lfp), Some(pc));
    // SAFETY: the extension's own function, with the arguments it declared.
    let res = unsafe {
        match m.f {
            ExtFn::Plain(f) => f(
                &mut ctx,
                lfp.self_val().id(),
                argv.len() as c_int,
                argv.as_ptr(),
                block,
            ),
            ExtFn::Data(f, data) => f(
                &mut ctx,
                lfp.self_val().id(),
                argv.len() as c_int,
                argv.as_ptr(),
                block,
                data,
            ),
        }
    };
    vm.temp_clear(temp_base);
    if let Some(frame) = kw_frame
        && let Some(i) = globals.ext.kw_frames.iter().rposition(|f| *f == frame)
    {
        globals.ext.kw_frames.remove(i);
    }
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

pub(crate) const DLEXT: &str = if cfg!(target_os = "macos") {
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
    // SAFETY: as above.
    let capi_init = unsafe { libc::dlsym(handle, c"monoruby_capi_init".as_ptr()) };
    if !capi_init.is_null() {
        let handle = claim_capi_instance(path, handle, globals.ext.owner.0)?;
        // SAFETY: as above, on the claimed instance.
        let capi_init = unsafe { libc::dlsym(handle, c"monoruby_capi_init".as_ptr()) };
        return load_capi_extension(vm, globals, path, handle, capi_init);
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

/// The entry point the CRuby C API layer (`monoruby_capi/src/capi.c`)
/// links into every extension built against its `ruby.h`: binds the
/// layer to the interpreter and runs the extension's `void Init_<name>(void)`
/// under its own exception frame. 0 on success; -1 with an error pending;
/// -2 when the running monoruby's table is older than the layer needs.
type CapiInitFn = unsafe extern "C" fn(ctx: *mut MrContext, init: *mut c_void) -> c_int;

/// Load an extension written against the CRuby C API (it carries
/// `monoruby_capi_init`). Its initializer is CRuby's
/// `Init_<basename>`, the basename taken as it is (a CRuby extension
/// `libfoo.so` defines `Init_libfoo`), with the `lib`-stripped form of
/// `init_symbol` as a fallback for one installed as `lib<name>.so`.
fn load_capi_extension(
    vm: &mut Executor,
    globals: &mut Globals,
    path: &Path,
    handle: *mut c_void,
    capi_init: *mut c_void,
) -> Result<()> {
    let stem = path
        .file_stem()
        .map(|s| s.to_string_lossy().into_owned())
        .unwrap_or_default();
    let mut candidates = vec![format!("Init_{stem}")];
    let stripped = init_symbol(path);
    if !candidates.contains(&stripped) {
        candidates.push(stripped);
    }
    let Some((sym, init)) = candidates.iter().find_map(|name| {
        let c = std::ffi::CString::new(name.as_str()).ok()?;
        // SAFETY: plain libc call with a valid handle and C string.
        let p = unsafe { libc::dlsym(handle, c.as_ptr()) };
        (!p.is_null()).then_some((name.clone(), p))
    }) else {
        return Err(MonorubyErr::loaderr(
            format!(
                "{} is not a Ruby extension: no {}",
                path.display(),
                candidates[0]
            ),
            path.to_path_buf(),
        ));
    };
    // SAFETY: the layer's entry point, declared with `CapiInitFn`'s type.
    let capi_init: CapiInitFn =
        unsafe { std::mem::transmute::<*mut c_void, CapiInitFn>(capi_init) };
    // Values the initializer creates stay rooted until it returns, as
    // during any extension call.
    let temp_base = vm.temp_len();
    let mut ctx = make_ctx(vm, globals, None, None);
    // SAFETY: the layer's entry point with a live context and the
    // extension's `void Init_<name>(void)`.
    let rc = unsafe { capi_init(&mut ctx, init) };
    vm.temp_clear(temp_base);
    if vm.has_error() {
        return Err(vm.take_error());
    }
    match rc {
        0 => {
            globals.ext.handles.push(handle as usize);
            Ok(())
        }
        -2 => Err(MonorubyErr::loaderr(
            format!(
                "{} was built against a newer monoruby C API than this interpreter provides",
                path.display()
            ),
            path.to_path_buf(),
        )),
        rc => Err(MonorubyErr::loaderr(
            format!("{} returned {rc} from {sym}", path.display()),
            path.to_path_buf(),
        )),
    }
}

/// Whether `path` carries a shared-library suffix (`.so`, `.bundle`, or
/// monoruby's own `DLEXT`) — the only files `is_capi_extension` opens.
pub(crate) fn has_native_lib_ext(path: &Path) -> bool {
    path.extension()
        .is_some_and(|e| e == "so" || e == DLEXT || e == "bundle")
}

/// Whether the shared library at `path` was built against monoruby's
/// CRuby-compatible `ruby.h` — the require resolver's cue to load a `.so`
/// found on `$LOAD_PATH` natively instead of looking for a Ruby stand-in.
/// The answer is whether the file exports the layer's entry point,
/// read from its symbol table (`dynsym`): the library is never opened
/// for the question. A `dlopen` would run its constructors and map its
/// dependencies, and a host CRuby's extension linked against a shared
/// `libruby` loads that way without an error, bringing `libruby` and its
/// own copies of the libraries it uses into the process for good
/// (`dlclose` unloads neither). Only a file whose format the reader does
/// not know is still opened, as before.
pub(crate) fn is_capi_extension(path: &Path) -> bool {
    use std::os::unix::ffi::OsStrExt;
    if !has_native_lib_ext(path) {
        return false;
    }
    if let Some(found) = dynsym::exports(path, "monoruby_capi_init") {
        return found;
    }
    let Ok(cpath) = std::ffi::CString::new(path.as_os_str().as_bytes()) else {
        return false;
    };
    // SAFETY: plain libc calls with valid C strings.
    unsafe {
        let handle = libc::dlopen(cpath.as_ptr(), libc::RTLD_NOW | libc::RTLD_LOCAL);
        if handle.is_null() {
            // Drain the error so a later `dlerror` reports its own call.
            libc::dlerror();
            return false;
        }
        let found = !libc::dlsym(handle, c"monoruby_capi_init".as_ptr()).is_null();
        // The handle is reference-counted: `load_extension` opens it again.
        libc::dlclose(handle);
        found
    }
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
    intern: mr_intern,
    funcall_sym: mr_funcall_sym,
    define_method_data: mr_define_method_data,
    str_cptr: mr_str_cptr,
    str_resize: mr_str_resize,
    str_cat: mr_str_cat,
    ary_ptr: mr_ary_ptr,
    gc_register_address: mr_gc_register_address,
    gc_unregister_address: mr_gc_unregister_address,
    class_set_alloc: mr_class_set_alloc,
    native_take: mr_native_take,
    call_super: mr_call_super,
    regexp_prepare: mr_regexp_prepare,
    regexp_ptr: mr_regexp_ptr,
    onig_api: mr_onig_api,
    enc_precise_mbclen: mr_enc_precise_mbclen,
    keyword_given: mr_keyword_given,
    enc_codepoint: mr_enc_codepoint,
    backref_get: mr_backref_get,
    backref_set: mr_backref_set,
    call_blocking_ubf: mr_call_blocking_ubf,
    gvar_hook: mr_gvar_hook,
    lastline_get: mr_lastline_get,
    lastline_set: mr_lastline_set,
    obj_frozen: mr_obj_frozen,
    gvar_get: mr_gvar_get,
    gvar_set: mr_gvar_set,
    funcall_kw: mr_funcall_kw,
    str_rptr: mr_str_rptr,
    str_subseq: mr_str_subseq,
    str_enc_name: mr_str_enc_name,
    str_set_enc: mr_str_set_enc,
    str_coderange: mr_str_coderange,
    str_dup: mr_str_dup,
    str_capacity: mr_str_capacity,
    thread_current: mr_thread_current,
    thread_local_aref: mr_thread_local_aref,
    obj_freeze: mr_obj_freeze,
};

// ---- regexp (ruby/re.h) ----------------------------------------------

/// The `regex_t`s handed to extensions, one per (pattern, options,
/// codec), compiled by `onig_new` exactly as the engine behind the
/// Regexp was and kept for the life of the process: an extension holds
/// the pointer across calls (strscan keeps it with the scanner), and
/// freeing one while a Regexp that produced it is collected would need
/// a count the C side cannot keep. A process has few distinct patterns
/// that reach a C extension.
struct CapiRegex(*mut onigmo_regex::regex_t);
// SAFETY: the pointer is only ever handed to Onigmo, which is thread
// agnostic for a compiled pattern, and to extensions running under the
// GVL.
unsafe impl Send for CapiRegex {}
unsafe impl Sync for CapiRegex {}

static CAPI_REGEX_CACHE: std::sync::Mutex<
    Option<HashMap<(Vec<u8>, u32, onigmo_regex::OnigmoEncoding), CapiRegex>>,
> = std::sync::Mutex::new(None);

/// The cached `regex_t` for `key`, compiled on first use. A thread's
/// recent lookups are answered by the address of the pattern bytes,
/// checked against the bytes themselves (a freed Regexp's buffer can be
/// reused), ahead of the shared table: strscan prepares its pattern on
/// every scan, and hashing the source under the lock dominated a scan.
fn capi_regex(key: (&[u8], u32, onigmo_regex::OnigmoEncoding)) -> Result<*mut c_void> {
    type Recent = fxhash::FxHashMap<(usize, u32, onigmo_regex::OnigmoEncoding), (Box<[u8]>, usize)>;
    thread_local! {
        static RECENT: std::cell::RefCell<Recent> = Default::default();
    }
    let (src, option, enc) = key;
    let addr = (src.as_ptr() as usize, option, enc);
    if let Some(p) = RECENT.with(|r| {
        r.borrow()
            .get(&addr)
            .and_then(|(bytes, p)| (**bytes == *src).then_some(*p))
    }) {
        return Ok(p as *mut c_void);
    }
    let p = capi_regex_shared((src.to_vec(), option, enc))?;
    RECENT.with(|r| {
        let mut r = r.borrow_mut();
        if r.len() >= 256 {
            r.clear();
        }
        r.insert(addr, (src.into(), p as usize));
    });
    Ok(p)
}

fn capi_regex_shared(key: (Vec<u8>, u32, onigmo_regex::OnigmoEncoding)) -> Result<*mut c_void> {
    let mut guard = CAPI_REGEX_CACHE.lock().unwrap_or_else(|e| e.into_inner());
    let cache = guard.get_or_insert_with(HashMap::new);
    if let Some(re) = cache.get(&key) {
        return Ok(re.0 as *mut c_void);
    }
    let (pattern, option, enc) = &key;
    let mut raw: *mut onigmo_regex::regex_t = std::ptr::null_mut();
    let mut einfo = std::mem::MaybeUninit::<onigmo_regex::OnigErrorInfo>::uninit();
    // SAFETY: the pattern bytes outlive the call; `raw` and `einfo` are
    // out-parameters Onigmo fills; the syntax table is a static.
    let r = unsafe {
        onigmo_regex::onig_new(
            &mut raw,
            pattern.as_ptr(),
            pattern.as_ptr().add(pattern.len()),
            *option,
            enc.as_type(),
            &raw const onigmo_regex::OnigSyntaxRuby,
            einfo.as_mut_ptr(),
        )
    };
    if r != onigmo_regex::ONIG_NORMAL as c_int {
        let mut buf = [0u8; onigmo_regex::ONIG_MAX_ERROR_MESSAGE_LEN as usize];
        // SAFETY: `buf` is the length Onigmo documents for the message.
        let len = unsafe {
            onigmo_regex::onig_error_code_to_str(buf.as_mut_ptr(), r as isize, einfo.as_mut_ptr())
        } as usize;
        let msg = String::from_utf8_lossy(&buf[..len.min(buf.len())]).into_owned();
        return Err(MonorubyErr::regexerr(msg));
    }
    cache.insert(key, CapiRegex(raw));
    Ok(raw as *mut c_void)
}

unsafe extern "C" fn mr_regexp_prepare(ctx: *mut MrContext, re: MrValue, str: MrValue) -> *mut c_void {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let (Some(re), Some(str)) = (val(re), val(str)) else {
        return std::ptr::null_mut();
    };
    if re.is_regex().is_none() {
        set_err(vm, MonorubyErr::wrong_argument_type(&globals.store, re, "Regexp"));
        return std::ptr::null_mut();
    }
    if str.try_rvalue().is_none_or(|rv| rv.ty() != ObjTy::STRING) {
        set_err(vm, MonorubyErr::no_implicit_conversion(&globals.store, str, STRING_CLASS));
        return std::ptr::null_mut();
    }
    let key = match re.as_regexp_inner().capi_compile_key(&globals.store, str.as_rstring_inner()) {
        Ok(key) => key,
        Err(err) => {
            set_err(vm, err);
            return std::ptr::null_mut();
        }
    };
    match capi_regex(key) {
        Ok(p) => p,
        Err(err) => {
            set_err(vm, err);
            std::ptr::null_mut()
        }
    }
}

unsafe extern "C" fn mr_regexp_ptr(ctx: *mut MrContext, re: MrValue) -> *mut c_void {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(re) = val(re) else {
        return std::ptr::null_mut();
    };
    if re.is_regex().is_none() {
        set_err(vm, MonorubyErr::wrong_argument_type(&globals.store, re, "Regexp"));
        return std::ptr::null_mut();
    }
    match capi_regex(re.as_regexp_inner().capi_engine_key()) {
        Ok(p) => p,
        Err(err) => {
            set_err(vm, err);
            std::ptr::null_mut()
        }
    }
}

// The Onigmo entry points with their types erased to what the table
// declares. Each is a cast and a call.
/// `rb_enc_precise_mbclen` over monoruby's own encoding walkers, in
/// Onigmo's answer encoding (`ONIGENC_MBCLEN_*`).
unsafe extern "C" fn mr_enc_precise_mbclen(
    _: *mut MrContext,
    name: *const c_char,
    p: *const u8,
    e: *const u8,
) -> c_int {
    use crate::value::PreciseLen;
    if p >= e {
        return -1 - 1;
    }
    // SAFETY: the table's contract: `name` is a C string, `p..e` a
    // readable byte range.
    let bytes = unsafe { std::slice::from_raw_parts(p, e.offset_from(p) as usize) };
    let Some(enc) = enc_of_cname(name) else {
        return 1;
    };
    match crate::value::precise_mbclen(enc, bytes, 0) {
        PreciseLen::Char(n) => n as c_int,
        PreciseLen::NeedMore => -1 - 1,
        PreciseLen::Invalid => -1,
    }
}

/// `rb_enc_codepoint_len`: the code point of the character at `p`, its
/// width through `len`; -1 when none of the named encoding starts there.
unsafe extern "C" fn mr_enc_codepoint(
    _: *mut MrContext,
    name: *const c_char,
    p: *const u8,
    e: *const u8,
    len: *mut c_int,
) -> i64 {
    use crate::value::PreciseLen;
    if p >= e {
        return -1;
    }
    // SAFETY: the table's contract: `name` is a C string, `p..e` a
    // readable byte range, `len` null or writable.
    let (name, bytes) = unsafe {
        (
            CStr::from_ptr(name).to_string_lossy(),
            std::slice::from_raw_parts(p, e.offset_from(p) as usize),
        )
    };
    let enc = Encoding::try_from_str(&name).unwrap_or(Encoding::Ascii8);
    let (n, cp) = match crate::value::precise_mbclen(enc, bytes, 0) {
        PreciseLen::Char(n) => (n, crate::value::enc_codepoint(enc, &bytes[..n])),
        _ => (0, None),
    };
    let Some(cp) = cp else {
        return -1;
    };
    if !len.is_null() {
        // SAFETY: as above.
        unsafe { *len = n as c_int };
    }
    cp as i64
}

/// `rb_backref_get`: `$~` as the Ruby code that called the extension
/// method sees it. A native call pushes no frame of its own for `$~`,
/// so the extension's matching (`rb_reg_match`, `rb_funcall(re, "match")`)
/// lands in the caller's slot, as CRuby's does -- which is why
/// `rb_backref_set` exists: an extension saves it first and puts it
/// back (date's parsers do).
unsafe extern "C" fn mr_backref_get(ctx: *mut MrContext) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, _) = unsafe { parts(ctx) };
    vm.get_last_matchdata().id()
}

/// `rb_backref_set`: see `mr_backref_get`.
unsafe extern "C" fn mr_backref_set(ctx: *mut MrContext, md: MrValue) {
    // SAFETY: the table's contract.
    let (vm, _) = unsafe { parts(ctx) };
    vm.set_backref(Value::from_u64(md));
}

/// `rb_lastline_get`: `$_` of the Ruby code that called the extension
/// method, read past the method's native frame as `$~` is
/// (`mr_backref_get`).
unsafe extern "C" fn mr_lastline_get(ctx: *mut MrContext) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, _) = unsafe { parts(ctx) };
    vm.get_last_read_line().id()
}

/// `rb_lastline_set`: see `mr_lastline_get`.
unsafe extern "C" fn mr_lastline_set(ctx: *mut MrContext, v: MrValue) {
    // SAFETY: the table's contract.
    let (vm, _) = unsafe { parts(ctx) };
    vm.set_last_read_line(Value::from_u64(v));
}

/// `OBJ_FROZEN`: what `Kernel#frozen?` answers, without the call.
unsafe extern "C" fn mr_obj_frozen(_ctx: *mut MrContext, v: MrValue) -> c_int {
    Value::from_u64(v).is_frozen() as c_int
}

/// `rb_gv_get`: the global `name` (with its `$`), through its hook as a
/// Ruby read would be (`$/`, `$;`, `$~`, ...).
unsafe extern "C" fn mr_gvar_get(ctx: *mut MrContext, name: *const c_char) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let name = unsafe { cstr(name) };
    crate::globals::GvarTable::get(vm, globals, IdentId::get_id(name)).id()
}

/// `rb_gv_set`: assign the global `name`, running its setter's checks;
/// -1 with the error pending when it refuses.
unsafe extern "C" fn mr_gvar_set(ctx: *mut MrContext, name: *const c_char, v: MrValue) -> c_int {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let name = unsafe { cstr(name) };
    let Some(v) = val(v) else { return -1 };
    match crate::globals::GvarTable::set(vm, globals, IdentId::get_id(name), v) {
        Ok(()) => 0,
        Err(e) => {
            set_err(vm, e);
            -1
        }
    }
}

/// `rb_define_hooked_variable` and its relatives: hand the global
/// variable `name` to the extension's getter and setter.
unsafe extern "C" fn mr_gvar_hook(
    ctx: *mut MrContext,
    name: MrSym,
    data: *mut c_void,
    getter: Option<MrGvarGetFn>,
    setter: Option<MrGvarSetFn>,
) {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    if name == MR_NO_SYM {
        set_err(vm, MonorubyErr::argumenterr("gvar_hook was given MR_NO_SYM"));
        return;
    }
    globals.define_native_gvar(
        IdentId::from(name),
        NativeGvar {
            data,
            getter,
            setter,
        },
    );
}

/// A read of the global variable `name`, hooked by an extension: its
/// getter's answer. A read has no error path through the VM and the JIT
/// (`get_global_var` returns a bare `Value`), so a getter that raised
/// is reported as a warning and the read is nil.
pub(crate) fn gvar_native_get(
    vm: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
    hook: NativeGvar,
) -> Value {
    let Some(getter) = hook.getter else {
        return Value::nil();
    };
    let temp_base = vm.temp_len();
    let mut ctx = make_ctx(vm, globals, None, None);
    // SAFETY: the extension's own function, with the data it registered.
    let res = unsafe { getter(&mut ctx, hook.data) };
    vm.temp_clear(temp_base);
    if vm.has_error() {
        let err = vm.take_error();
        let msg = format!(
            "warning: the getter of {name} raised, and a global variable read cannot: {}",
            err.get_error_message(&globals.store)
        );
        let _ = vm.ruby_warn(globals, &msg);
        return Value::nil();
    }
    val(res).unwrap_or(Value::nil())
}

/// An assignment to the global variable `name`, hooked by an extension:
/// its setter runs, or NameError when it has none.
pub(crate) fn gvar_native_set(
    vm: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
    hook: NativeGvar,
    val: Value,
) -> Result<()> {
    let Some(setter) = hook.setter else {
        return Err(MonorubyErr::nameerr(format!(
            "{name} is a read-only variable"
        )));
    };
    let temp_base = vm.temp_len();
    let mut ctx = make_ctx(vm, globals, None, None);
    // SAFETY: as in `gvar_native_get`.
    let rc = unsafe { setter(&mut ctx, hook.data, val.id()) };
    vm.temp_clear(temp_base);
    if vm.has_error() {
        return Err(vm.take_error());
    }
    if rc != 0 {
        return Err(MonorubyErr::runtimeerr(format!(
            "the setter of {name} failed without raising"
        )));
    }
    Ok(())
}

/// `rb_keyword_given_p`: the running extension method's frame was
/// entered by a call instruction whose site carries keywords, and the
/// method received them as a trailing Hash. Entered any other way (from
/// `Init_`, an extension's `rb_funcall`), the answer is no.
unsafe extern "C" fn mr_keyword_given(ctx: *mut MrContext) -> c_int {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    // SAFETY: the context's `frame` is the calling method's, set by the
    // trampoline; null only inside `Init_`.
    let frame = unsafe { (*ctx).frame };
    if frame.is_null() {
        return 0;
    }
    // SAFETY: the reverse of `make_ctx`'s `as_ptr`.
    let lfp = unsafe { Lfp::new(frame as *mut u8) };
    let last = globals.ext.methods.get(&lfp.func_id()).and_then(|m| match m.argc {
        MR_ARGC_VARIADIC => lfp.arg(0).as_array_inner().last().copied(),
        0 => None,
        argc => Some(lfp.arg(argc as usize - 1)),
    });
    if !last.is_some_and(|v| v.try_hash_ty().is_some()) {
        return 0;
    }
    if globals.ext.kw_frames.contains(&(frame as usize)) {
        return 1;
    }
    keyword_given(vm, globals).unwrap_or(false) as c_int
}

/// Whether the call site that entered the running frame carries
/// keywords; through a built-in that forwards its arguments (`send`,
/// `public_send`, `Method#call`) to the site that called it, as CRuby's
/// keyword flag travels with the arguments. A Ruby frame whose pc is not
/// a send (a yield), a native caller of any other kind (an extension's
/// `rb_funcall`), or the bottom of the stack ends the walk with `None`.
fn keyword_given(vm: &Executor, globals: &Globals) -> Option<bool> {
    let mut cfp = vm.cfp();
    loop {
        if let Some((_, callid)) = crate::codegen::runtime::entered_by(globals, cfp) {
            return Some(globals.store[callid].kw_may_exists());
        }
        cfp = cfp.prev()?;
        let fid = cfp.lfp().func_id();
        if globals.store[fid].is_iseq().is_some() || globals.ext.methods.contains_key(&fid) {
            return None;
        }
    }
}

mod onig_shim {
    use super::*;
    use onigmo_regex as o;
    type Reg = *mut o::regex_t;
    type Region = *mut o::OnigRegion;

    pub(super) unsafe extern "C" fn search(
        reg: *mut c_void,
        str: *const u8,
        end: *const u8,
        start: *const u8,
        range: *const u8,
        region: *mut c_void,
        option: u32,
    ) -> isize {
        // SAFETY: the extension's contract, as for a direct Onigmo call.
        unsafe { o::onig_search(reg as Reg, str, end, start, range, region as Region, option) }
    }
    pub(super) unsafe extern "C" fn match_at(
        reg: *mut c_void,
        str: *const u8,
        end: *const u8,
        at: *const u8,
        region: *mut c_void,
        option: u32,
    ) -> isize {
        // SAFETY: as above.
        unsafe { o::onig_match(reg as Reg, str, end, at, region as Region, option) }
    }
    pub(super) unsafe extern "C" fn region_new() -> *mut c_void {
        // SAFETY: as above.
        unsafe { o::onig_region_new() as *mut c_void }
    }
    pub(super) unsafe extern "C" fn region_init(region: *mut c_void) {
        // SAFETY: as above.
        unsafe { o::onig_region_init(region as Region) }
    }
    pub(super) unsafe extern "C" fn region_free(region: *mut c_void, free_self: c_int) {
        // SAFETY: as above.
        unsafe { o::onig_region_free(region as Region, free_self) }
    }
    pub(super) unsafe extern "C" fn region_clear(region: *mut c_void) {
        // SAFETY: as above.
        unsafe { o::onig_region_clear(region as Region) }
    }
    pub(super) unsafe extern "C" fn region_copy(to: *mut c_void, from: *const c_void) {
        // SAFETY: as above.
        unsafe { o::onig_region_copy(to as Region, from as *const o::OnigRegion) }
    }
    pub(super) unsafe extern "C" fn region_resize(region: *mut c_void, n: c_int) -> c_int {
        // SAFETY: as above.
        unsafe { o::onig_region_resize(region as Region, n) }
    }
    pub(super) unsafe extern "C" fn region_set(region: *mut c_void, at: c_int, beg: c_int, end: c_int) -> c_int {
        // SAFETY: as above.
        unsafe { o::onig_region_set(region as Region, at, beg, end) }
    }
    pub(super) unsafe extern "C" fn free(reg: *mut c_void) {
        // SAFETY: as above.
        unsafe { o::onig_free(reg as Reg) }
    }
    pub(super) unsafe extern "C" fn name_to_backref_number(
        reg: *mut c_void,
        name: *const u8,
        name_end: *const u8,
        region: *const c_void,
    ) -> c_int {
        // SAFETY: as above.
        unsafe { o::onig_name_to_backref_number(reg as Reg, name, name_end, region as *const o::OnigRegion) }
    }
    pub(super) unsafe extern "C" fn foreach_name(
        reg: *mut c_void,
        func: unsafe extern "C" fn(*const u8, *const u8, c_int, *mut c_int, *mut c_void, *mut c_void) -> c_int,
        arg: *mut c_void,
    ) -> c_int {
        // SAFETY: the callback's fifth argument is the `regex_t *` in
        // both signatures; only the pointee type differs.
        let func: unsafe extern "C" fn(*const u8, *const u8, c_int, *mut c_int, Reg, *mut c_void) -> c_int =
            unsafe { std::mem::transmute(func) };
        // SAFETY: as above.
        unsafe { o::onig_foreach_name(reg as Reg, Some(func), arg) }
    }
    pub(super) unsafe extern "C" fn number_of_names(reg: *const c_void) -> c_int {
        // SAFETY: as above.
        unsafe { o::onig_number_of_names(reg as *const o::regex_t) }
    }
    pub(super) unsafe extern "C" fn number_of_captures(reg: *const c_void) -> c_int {
        // SAFETY: as above.
        unsafe { o::onig_number_of_captures(reg as *const o::regex_t) }
    }
    pub(super) unsafe extern "C" fn number_of_capture_histories(reg: *const c_void) -> c_int {
        // SAFETY: as above.
        unsafe { o::onig_number_of_capture_histories(reg as *const o::regex_t) }
    }
    pub(super) unsafe extern "C" fn error_code_to_str(buf: *mut u8, code: isize) -> c_int {
        // SAFETY: `buf` is ONIG_MAX_ERROR_MESSAGE_LEN bytes by the
        // extension's contract; no `OnigErrorInfo` follows.
        unsafe { o::onig_error_code_to_str(buf, code) }
    }
}

static MR_ONIG_API: MrOnigApi = MrOnigApi {
    search: onig_shim::search,
    match_at: onig_shim::match_at,
    region_new: onig_shim::region_new,
    region_init: onig_shim::region_init,
    region_free: onig_shim::region_free,
    region_clear: onig_shim::region_clear,
    region_copy: onig_shim::region_copy,
    region_resize: onig_shim::region_resize,
    region_set: onig_shim::region_set,
    free: onig_shim::free,
    name_to_backref_number: onig_shim::name_to_backref_number,
    foreach_name: onig_shim::foreach_name,
    number_of_names: onig_shim::number_of_names,
    number_of_captures: onig_shim::number_of_captures,
    number_of_capture_histories: onig_shim::number_of_capture_histories,
    error_code_to_str: onig_shim::error_code_to_str,
};

unsafe extern "C" fn mr_onig_api() -> *const MrOnigApi {
    &MR_ONIG_API
}

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
        let err = vm.take_error();
        match err.kind {
            // A non-local exit (`return` / `break` out of a block, `throw`,
            // ...) leaving through the extension's frames has no class;
            // box it unchanged so `raise_exception` resumes the jump.
            MonorubyErrKind::MethodReturn(..)
            | MonorubyErrKind::BlockBreak(..)
            | MonorubyErrKind::Throw(..)
            | MonorubyErrKind::FiberKill
            | MonorubyErrKind::Retry
            | MonorubyErrKind::Redo => Value::new_exception_boxed(err, EXCEPTION_CLASS).id(),
            _ => Value::new_exception(err).id(),
        }
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
    define_ext_method(globals, class_id, name, ExtFn::Plain(f), argc, flags);
}

unsafe extern "C" fn mr_define_method_data(
    ctx: *mut MrContext,
    klass: MrValue,
    name: *const c_char,
    f: MrMethodDataFn,
    data: *mut c_void,
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
    define_ext_method(globals, class_id, name, ExtFn::Data(f, data), argc, flags);
}

fn define_ext_method(
    globals: &mut Globals,
    class_id: ClassId,
    name: &str,
    f: ExtFn,
    argc: c_int,
    flags: u32,
) {
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
    let (vm, globals) = unsafe { parts(ctx) };
    let (Some(ary), Some(v)) = (val(ary), val(v)) else {
        return -1;
    };
    match ary.try_array_ty() {
        Some(_) if let Err(e) = ary.ensure_not_frozen(&globals.store) => {
            set_err(vm, e);
            -1
        }
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
    if let Err(e) = hash.ensure_not_frozen(&globals.store) {
        set_err(vm, e);
        return -1;
    }
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
    let name = unsafe { cstr(name) };
    // SAFETY: the table's contract, passed straight through.
    unsafe { funcall_id(ctx, recv, IdentId::get_id(name), argc, argv, block) }
}

///
/// Intern a name once, so a loop that calls into Ruby does not pay for
/// it on every call.
///
/// `funcall` takes the name as a C string, which means a `&str` built
/// and validated, then hashed against the interning table under its
/// lock — per call. That is affordable when what it calls is a method
/// body of any size, and it is not when the callee is a few lines: the
/// psych extension hands libyaml's events to a `Psych::Handler` with
/// two of these per event, and interning `"scalar"` and
/// `"event_location"` over and over was most of what the dispatch cost.
///
unsafe extern "C" fn mr_intern(_: *mut MrContext, ptr: *const u8, len: usize) -> MrSym {
    // SAFETY: the table's contract — `len` bytes readable at `ptr`.
    let bytes = unsafe { slice(ptr, len) };
    match std::str::from_utf8(bytes) {
        Ok(s) => IdentId::get_id(s).get(),
        Err(_) => MR_NO_SYM,
    }
}

unsafe extern "C" fn mr_funcall_sym(
    ctx: *mut MrContext,
    recv: MrValue,
    sym: MrSym,
    argc: c_int,
    argv: *const MrValue,
    block: MrValue,
) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, _) = unsafe { parts(ctx) };
    if sym == MR_NO_SYM {
        set_err(
            vm,
            MonorubyErr::argumenterr("funcall_sym was given MR_NO_SYM"),
        );
        return MR_UNDEF;
    }
    let id = IdentId::from(sym);
    // SAFETY: as above.
    unsafe { funcall_id(ctx, recv, id, argc, argv, block) }
}

/// What `funcall` and `funcall_sym` both do, once the name is an
/// `IdentId`.
///
/// # Safety
/// `ctx` is a live context and `argv[0..argc]` is readable, as the
/// table's contract requires.
unsafe fn funcall_id(
    ctx: *mut MrContext,
    recv: MrValue,
    method: IdentId,
    argc: c_int,
    argv: *const MrValue,
    block: MrValue,
) -> MrValue {
    // SAFETY: the caller's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(recv) = val(recv) else {
        return MR_UNDEF;
    };
    let n = argc.max(0) as usize;
    let args: smallvec::SmallVec<[Value; 8]> = (0..n)
        // SAFETY: as above — `argv` points at `argc` values.
        .map(|i| unsafe { *argv.add(i) })
        .filter_map(val)
        .collect();
    match vm.invoke_method_inner(globals, method, recv, &args, block_handler(block), None) {
        Ok(v) => v.id(),
        Err(e) => {
            set_err(vm, e);
            MR_UNDEF
        }
    }
}

/// `rb_funcallv_kw`: `funcall_sym`, with the last of `argv` passed as
/// keywords when `kw_splat` is set and it is a Hash. A callee written in
/// Ruby binds them as keywords; one in an extension sees them as its
/// trailing Hash with `rb_keyword_given_p` true (`ExtState::kw_pending`),
/// as from a Ruby call site that passed keywords.
unsafe extern "C" fn mr_funcall_kw(
    ctx: *mut MrContext,
    recv: MrValue,
    sym: MrSym,
    argc: c_int,
    argv: *const MrValue,
    block: MrValue,
    kw_splat: c_int,
) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    if sym == MR_NO_SYM {
        set_err(vm, MonorubyErr::argumenterr("funcall_kw was given MR_NO_SYM"));
        return MR_UNDEF;
    }
    let Some(recv) = val(recv) else {
        return MR_UNDEF;
    };
    // SAFETY: as above.
    let mut args = unsafe { args(argv, argc) };
    let kw = match args.last() {
        Some(h) if kw_splat != 0 && h.try_hash_ty().is_some() => Some(Hashmap::new(*h)),
        _ => None,
    };
    if kw.is_none() {
        // SAFETY: as above.
        return unsafe { funcall_id(ctx, recv.id(), IdentId::from(sym), argc, argv, block) };
    }
    args.pop();
    // `**{}` passes nothing at all.
    let kw = kw.filter(|h| !h.is_empty());
    match vm.invoke_method_inner(globals, IdentId::from(sym), recv, &args, block_handler(block), kw) {
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

unsafe extern "C" fn mr_call_blocking_ubf(
    ctx: *mut MrContext,
    f: MrBlockingFn,
    arg: *mut c_void,
    ubf: *mut c_void,
    ubf_arg: *mut c_void,
    out: *mut i64,
) -> c_int {
    if ubf.is_null() {
        // SAFETY: the table's contract, forwarded.
        return unsafe { mr_call_blocking(ctx, f, arg, out) };
    }
    let func = if ubf as usize == monoruby_ext_sys::MR_UBF_IO {
        None
    } else {
        // SAFETY: the extension passed its `rb_unblock_function_t`.
        Some(unsafe { std::mem::transmute::<*mut c_void, unsafe extern "C" fn(*mut c_void)>(ubf) })
    };
    let mut thread = {
        // SAFETY: the table's contract.
        let (vm, _) = unsafe { parts(ctx) };
        crate::scheduler::current_thread(vm)
    };
    let saved = thread
        .as_thread_inner_mut()
        .blocking_ubf
        .replace(crate::scheduler::Ubf {
            func,
            arg: ubf_arg as usize,
            requested: false,
        });
    // SAFETY: the table's contract, forwarded.
    let rc = unsafe { mr_call_blocking(ctx, f, arg, out) };
    thread.as_thread_inner_mut().blocking_ubf = saved;
    rc
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

// ---- appended for the CRuby C API layer ------------------------------

/// `v` as a String, or a TypeError pending.
fn string_arg(vm: &mut Executor, globals: &Globals, v: MrValue) -> Option<Value> {
    let v = val(v)?;
    if v.try_rvalue().is_some_and(|rv| rv.ty() == ObjTy::STRING) {
        Some(v)
    } else {
        set_err(
            vm,
            MonorubyErr::no_implicit_conversion(&globals.store, v, STRING_CLASS),
        );
        None
    }
}

unsafe extern "C" fn mr_str_cptr(ctx: *mut MrContext, v: MrValue, len: *mut usize) -> *mut u8 {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(mut s) = string_arg(vm, globals, v) else {
        return std::ptr::null_mut();
    };
    let (ptr, n) = s.as_rstring_inner_mut().c_str_ptr();
    if !len.is_null() {
        // SAFETY: `len` is the caller's slot.
        unsafe { *len = n };
    }
    ptr
}

/// `RSTRING_PTR`: a String's own NUL-terminated buffer (`str_cptr`)
/// unless it is a view into a shared buffer (a slice of a longer string
/// shares it, and so does the string it was taken from), whose bytes are
/// handed out in place: detaching it here would move the bytes on every
/// call and break the pointer arithmetic C code does across calls
/// (stringio's `ptr->pos = e - RSTRING_PTR(str)`). As in CRuby, writing
/// through it needs `rb_str_modify` (`str_cptr`) first, which is also
/// what drops the cached code range: a read keeps it.
unsafe extern "C" fn mr_str_rptr(ctx: *mut MrContext, v: MrValue, len: *mut usize) -> *mut u8 {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(mut s) = string_arg(vm, globals, v) else {
        return std::ptr::null_mut();
    };
    let inner = s.as_rstring_inner_mut();
    let (ptr, n) = if inner.is_shared() {
        let bytes = inner.as_bytes();
        (bytes.as_ptr() as *mut u8, bytes.len())
    } else {
        inner.c_str_ptr_read()
    };
    if !len.is_null() {
        // SAFETY: `len` is the caller's slot.
        unsafe { *len = n };
    }
    ptr
}

/// `rb_str_subseq`: bytes `beg..beg+len` of a String (clamped to it) as
/// a new String of the same encoding, sharing the buffer when it is long
/// enough, as `byteslice` does — without the method call.
unsafe extern "C" fn mr_str_subseq(ctx: *mut MrContext, v: MrValue, beg: i64, len: i64) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(s) = string_arg(vm, globals, v) else {
        return MR_UNDEF;
    };
    let n = s.as_rstring_inner().len();
    let beg = (beg.max(0) as usize).min(n);
    let end = beg.saturating_add(len.max(0) as usize).min(n);
    crate::value::string_substring(s, beg, end).id()
}

/// The name of a String's encoding as a NUL-terminated C string that
/// lives for the process: one pointer per encoding, so the caller can
/// cache by it (`rb_enc_get`).
unsafe extern "C" fn mr_str_enc_name(ctx: *mut MrContext, v: MrValue) -> *const c_char {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(s) = string_arg(vm, globals, v) else {
        return std::ptr::null();
    };
    static_enc_name(s.as_rstring_inner().encoding())
}

fn static_enc_name(enc: Encoding) -> *const c_char {
    use std::sync::{Mutex, OnceLock};
    // The common three without the shared table's lock (always these
    // pointers: the table never sees them).
    match enc {
        Encoding::Utf8(0) => return c"UTF-8".as_ptr(),
        Encoding::Ascii8 => return c"ASCII-8BIT".as_ptr(),
        Encoding::UsAscii => return c"US-ASCII".as_ptr(),
        _ => {}
    }
    static NAMES: OnceLock<Mutex<HashMap<Encoding, &'static CStr>>> = OnceLock::new();
    let mut names = NAMES.get_or_init(Default::default).lock().unwrap();
    let p = names
        .entry(enc)
        .or_insert_with(|| {
            let c = std::ffi::CString::new(enc.name()).unwrap_or_default();
            Box::leak(c.into_boxed_c_str())
        })
        .as_ptr();
    p
}

/// The Encoding a C string names, cached by the pointer: the layer hands
/// the same `rb_encoding` name pointer in again and again. A cached entry
/// is checked against the bytes, so a reused buffer is never misread.
fn enc_of_cname(name: *const c_char) -> Option<Encoding> {
    thread_local! {
        static CACHE: std::cell::RefCell<fxhash::FxHashMap<usize, Encoding>> = Default::default();
    }
    // SAFETY: the caller's contract: `name` is a C string.
    let bytes = unsafe { CStr::from_ptr(name) }.to_bytes();
    let key = name as usize;
    if let Some(enc) = CACHE.with(|c| c.borrow().get(&key).copied())
        && enc.name().as_bytes() == bytes
    {
        return Some(enc);
    }
    let enc = Encoding::try_from_str(std::str::from_utf8(bytes).ok()?).ok()?;
    if enc.name().as_bytes() == bytes {
        CACHE.with(|c| c.borrow_mut().insert(key, enc));
    }
    Some(enc)
}

/// `rb_enc_associate`: `force_encoding(name)` without the method call. 0
/// done; -1 with an error pending (not a String, frozen); -2 when `name`
/// is not one this resolves (the caller falls back to `force_encoding`).
unsafe extern "C" fn mr_str_set_enc(ctx: *mut MrContext, v: MrValue, name: *const c_char) -> c_int {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(mut s) = string_arg(vm, globals, v) else {
        return -1;
    };
    let Some(enc) = enc_of_cname(name) else {
        return -2;
    };
    if let Err(e) = s.ensure_string_mutable(vm, globals) {
        set_err(vm, e);
        return -1;
    }
    s.as_rstring_inner_mut().set_encoding(enc);
    0
}

unsafe extern "C" fn mr_str_coderange(ctx: *mut MrContext, v: MrValue) -> c_int {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(s) = string_arg(vm, globals, v) else {
        return -1;
    };
    match s.as_rstring_inner().code_range() {
        crate::value::CodeRange::SevenBit | crate::value::CodeRange::Unknown => 1,
        crate::value::CodeRange::Valid => 2,
        crate::value::CodeRange::Broken => 3,
    }
}

unsafe extern "C" fn mr_str_capacity(ctx: *mut MrContext, v: MrValue) -> i64 {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(mut s) = string_arg(vm, globals, v) else {
        return -1;
    };
    let inner = s.as_rstring_inner_mut();
    if inner.is_shared() {
        return inner.len() as i64;
    }
    // The buffer `str_rptr` hands out (it makes room for the NUL first).
    inner.c_str_ptr_read();
    inner.owned_capacity().saturating_sub(1) as i64
}

unsafe extern "C" fn mr_thread_current(ctx: *mut MrContext) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, _) = unsafe { parts(ctx) };
    crate::scheduler::current_thread(vm).id()
}

/// `Thread#[]` for a Symbol key, the way builtins/thread.rb keeps the
/// fiber-locals (`@fiber_locals`: fiber, or `:root`, => { key => value })
/// but without dispatching: bigdecimal reads its modes this way on every
/// operation. Anything unexpected falls back to calling `[]`.
unsafe extern "C" fn mr_thread_local_aref(ctx: *mut MrContext, thread: MrValue, sym: MrSym) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(th) = val(thread) else {
        return Value::nil().id();
    };
    let key = Value::symbol(IdentId::from(sym));
    let mut fast = || -> Option<Result<Value>> {
        static FIBER_LOCALS: std::sync::OnceLock<IdentId> = std::sync::OnceLock::new();
        static ROOT: std::sync::OnceLock<IdentId> = std::sync::OnceLock::new();
        let root = Value::symbol(*ROOT.get_or_init(|| IdentId::get_id("root")));
        let ivar = *FIBER_LOCALS.get_or_init(|| IdentId::get_id("@fiber_locals"));
        let all = globals.store.get_ivar(th, ivar);
        let Some(all) = all else {
            return Some(Ok(Value::nil()));
        };
        if all.is_nil() {
            return Some(Ok(Value::nil()));
        }
        let all = all.try_hash_ty()?;
        let fk = if th.id() == crate::scheduler::current_thread(vm).id() {
            vm.current_fiber().unwrap_or(root)
        } else {
            root
        };
        let table = match all.get(fk, vm, globals) {
            Ok(Some(t)) => t,
            Ok(None) => return Some(Ok(Value::nil())),
            Err(e) => return Some(Err(e)),
        };
        let table = table.try_hash_ty()?;
        Some(table.get(key, vm, globals).map(|v| v.unwrap_or_default()))
    };
    match fast() {
        Some(Ok(v)) => v.id(),
        Some(Err(e)) => {
            set_err(vm, e);
            MR_UNDEF
        }
        // SAFETY: one argument, `key`, alive on this frame.
        None => unsafe { funcall_id(ctx, thread, IdentId::get_id("[]"), 1, &key.id(), MR_UNDEF) },
    }
}

unsafe extern "C" fn mr_obj_freeze(ctx: *mut MrContext, v: MrValue) {
    // SAFETY: the table's contract.
    let (_, globals) = unsafe { parts(ctx) };
    let mut v = Value::from_u64(v);
    if !v.is_packed_value() {
        v.set_frozen();
        // As Object#freeze: an existing singleton class freezes too.
        if let Some(singleton) = globals.store.has_singleton(v) {
            singleton.as_val().set_frozen();
        }
    }
}

unsafe extern "C" fn mr_str_dup(ctx: *mut MrContext, v: MrValue) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(s) = string_arg(vm, globals, v) else {
        return MR_UNDEF;
    };
    if s.class() == STRING_CLASS {
        let len = s.as_rstring_inner().len();
        return crate::value::string_substring(s, 0, len).id();
    }
    // SAFETY: no arguments.
    unsafe { funcall_id(ctx, v, IdentId::get_id("dup"), 0, std::ptr::null(), MR_UNDEF) }
}

unsafe extern "C" fn mr_str_resize(ctx: *mut MrContext, v: MrValue, len: usize) -> c_int {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(mut s) = string_arg(vm, globals, v) else {
        return -1;
    };
    if let Err(e) = s.ensure_not_frozen(&globals.store) {
        set_err(vm, e);
        return -1;
    }
    s.as_rstring_inner_mut().resize_keep_capacity(len);
    0
}

unsafe extern "C" fn mr_str_cat(
    ctx: *mut MrContext,
    v: MrValue,
    ptr: *const u8,
    len: usize,
) -> c_int {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(mut s) = string_arg(vm, globals, v) else {
        return -1;
    };
    if let Err(e) = s.ensure_string_mutable(vm, globals) {
        set_err(vm, e);
        return -1;
    }
    // SAFETY: the table's contract — `len` bytes readable at `ptr`.
    let bytes = unsafe { slice(ptr, len) };
    s.as_rstring_inner_mut().extend_from_slice_merge_cr(bytes);
    0
}

unsafe extern "C" fn mr_ary_ptr(
    ctx: *mut MrContext,
    ary: MrValue,
    len: *mut usize,
) -> *const MrValue {
    // SAFETY: the table's contract.
    let (vm, _) = unsafe { parts(ctx) };
    let Some(a) = val(ary).filter(|a| a.try_array_ty().is_some()) else {
        set_err(vm, MonorubyErr::typeerr("Array expected"));
        return std::ptr::null();
    };
    let elems: &[Value] = a.as_array_inner();
    if !len.is_null() {
        // SAFETY: `len` is the caller's slot.
        unsafe { *len = elems.len() };
    }
    // `Value` is a transparent `u64`.
    elems.as_ptr() as *const MrValue
}

unsafe extern "C" fn mr_gc_register_address(ctx: *mut MrContext, addr: *const MrValue) {
    // SAFETY: the table's contract.
    let (_, globals) = unsafe { parts(ctx) };
    if !addr.is_null() {
        globals.ext.addresses.push(addr as usize);
    }
}

unsafe extern "C" fn mr_gc_unregister_address(ctx: *mut MrContext, addr: *const MrValue) {
    // SAFETY: the table's contract.
    let (_, globals) = unsafe { parts(ctx) };
    if let Some(i) = globals.ext.addresses.iter().position(|a| *a == addr as usize) {
        globals.ext.addresses.swap_remove(i);
    }
}

unsafe extern "C" fn mr_class_set_alloc(ctx: *mut MrContext, klass: MrValue, flags: u32) -> c_int {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    let Some(class_id) = val(klass).and_then(|v| v.is_class()).map(|m| m.id()) else {
        set_err(vm, MonorubyErr::typeerr("class_set_alloc: class expected"));
        return -1;
    };
    let info = &mut globals.store[class_id];
    if flags & MR_ALLOC_UNDEF != 0 {
        info.clear_alloc_func();
        return 0;
    }
    if flags & MR_ALLOC_NATIVE != 0 {
        // A class whose instances are plain objects, or already native.
        // One with another built-in layout (a String subclass, ...)
        // cannot carry a payload.
        if !matches!(info.instance_ty(), Some(ObjTy::OBJECT) | Some(ObjTy::NATIVE)) {
            let name = globals.store.get_class_name(class_id);
            set_err(
                vm,
                MonorubyErr::typeerr(format!("instances of {name} cannot carry a native payload")),
            );
            return -1;
        }
        info.set_instance_ty(ObjTy::NATIVE);
        info.set_alloc_func(ext_alloc_func);
    }
    0
}

unsafe extern "C" fn mr_native_take(_: *mut MrContext, obj: MrValue) -> *mut c_void {
    let Some(mut obj) = val(obj) else {
        return std::ptr::null_mut();
    };
    let Some(rv) = obj.try_rvalue_mut().filter(|rv| rv.ty() == ObjTy::NATIVE) else {
        return std::ptr::null_mut();
    };
    match rv.as_native_mut().as_any_mut().downcast_mut::<ExtNative>() {
        Some(n) => {
            let data = n.data;
            n.data = std::ptr::null_mut();
            data
        }
        None => std::ptr::null_mut(),
    }
}

unsafe extern "C" fn mr_call_super(
    ctx: *mut MrContext,
    argc: c_int,
    argv: *const MrValue,
    block: MrValue,
) -> MrValue {
    // SAFETY: the table's contract.
    let (vm, globals) = unsafe { parts(ctx) };
    // SAFETY: the context's `frame` is the calling method's, set by the
    // trampoline; null only inside `Init_`.
    let frame = unsafe { (*ctx).frame };
    if frame.is_null() {
        set_err(vm, MonorubyErr::runtimeerr("super called outside of method"));
        return MR_UNDEF;
    }
    // SAFETY: the reverse of `make_ctx`'s `as_ptr`.
    let lfp = unsafe { Lfp::new(frame as *mut u8) };
    let fid = lfp.func_id();
    let recv = lfp.self_val();
    let Some(name) = globals[fid].name() else {
        set_err(vm, MonorubyErr::runtimeerr("super called outside of method"));
        return MR_UNDEF;
    };
    let Some(super_fid) = globals.store.check_super(recv.class(), fid, name) else {
        set_err(vm, MonorubyErr::super_method_not_found(&globals.store, name, recv));
        return MR_UNDEF;
    };
    // SAFETY: as above.
    let args = unsafe { args(argv, argc) };
    match vm.invoke_func_named(globals, super_fid, name, recv, &args, block_handler(block), None) {
        Ok(v) => v.id(),
        Err(e) => {
            set_err(vm, e);
            MR_UNDEF
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The compile failure of `capi_regex`: unreachable from an
    /// extension (the pattern was compiled by monoruby before it is
    /// re-targeted at a codec `check_match_encoding` allowed), so the
    /// message path is exercised here.
    #[test]
    fn capi_regex_reports_onig_new_failure() {
        let err = capi_regex((b"(".as_slice(), 0, onigmo_regex::OnigmoEncoding::UTF8)).unwrap_err();
        assert_eq!(err.message(), "end pattern with unmatched parenthesis");
        let ok = capi_regex((b"a+".as_slice(), 0, onigmo_regex::OnigmoEncoding::UTF8)).unwrap();
        assert!(!ok.is_null());
        // Cached: the same key answers the same `regex_t`.
        assert_eq!(ok, capi_regex((b"a+".as_slice(), 0, onigmo_regex::OnigmoEncoding::UTF8)).unwrap());
    }

    /// The thread-local front of `capi_regex` empties itself once it
    /// holds 256 patterns, and still answers afterwards.
    #[test]
    fn capi_regex_recent_cache_wraps() {
        let pats: Vec<Vec<u8>> = (0..300).map(|i| format!("p{i}x+").into_bytes()).collect();
        for p in &pats {
            assert!(!capi_regex((p.as_slice(), 0, onigmo_regex::OnigmoEncoding::UTF8)).unwrap().is_null());
        }
        let first = capi_regex((pats[0].as_slice(), 0, onigmo_regex::OnigmoEncoding::UTF8)).unwrap();
        assert_eq!(first, capi_regex((pats[0].as_slice(), 0, onigmo_regex::OnigmoEncoding::UTF8)).unwrap());
    }

    /// One interpreter claiming the same C-API extension twice gets the
    /// same mapping (and the extra `dlopen` reference is dropped); another
    /// interpreter gets a mapping of its own.
    #[test]
    fn capi_instances_per_owner() {
        use std::os::unix::ffi::OsStrExt;
        let Some(path) = find_extension("strscan") else {
            eprintln!("skipped: strscan.so is not installed");
            return;
        };
        let open = || {
            let c = std::ffi::CString::new(path.as_os_str().as_bytes()).unwrap();
            // SAFETY: plain libc call with a valid C string.
            let h = unsafe { libc::dlopen(c.as_ptr(), libc::RTLD_NOW | libc::RTLD_LOCAL) };
            assert!(!h.is_null(), "{}", dlerror());
            h
        };
        let (me, other) = (u64::MAX - 1, u64::MAX - 2);
        let a = claim_capi_instance(&path, open(), me).unwrap();
        let b = claim_capi_instance(&path, open(), me).unwrap();
        assert_eq!(a, b);
        let c = claim_capi_instance(&path, open(), other).unwrap();
        assert_ne!(a, c);
        assert_eq!(c, claim_capi_instance(&path, open(), other).unwrap());
    }

    /// The table's String / call / global entries on the inputs the
    /// layer screens out before calling them (a non-String, an unknown
    /// encoding name, a frozen receiver, `MR_NO_SYM`, keywords that are
    /// not a Hash), and the subclass / shared-buffer cases.
    #[test]
    fn abi_entry_edges() {
        let mut globals = Globals::new_test();
        let res = globals
            .run(
                r#"class CapiStr < String; end
                   $capi_keep = [CapiStr.new("ab"), "x" * 100, :sym, "fz".freeze,
                                 ->(*a, **k) { raise "boom" if k[:boom]; [a, k] }, { boom: true }]"#,
                std::path::Path::new("."),
            )
            .unwrap();
        let mut vm = Executor::init(&mut globals, "abi_entry_edges").unwrap();
        let items: Vec<Value> = res.as_array_inner().iter().copied().collect();
        let (sub, long, sym, frozen, lam, boom) = (items[0], items[1], items[2], items[3], items[4], items[5]);
        let mut ctx = make_ctx(&mut vm, &mut globals, None, None);
        let ctx = &mut ctx as *mut MrContext;
        // SAFETY: a live context over this test's interpreter; every
        // value is rooted by `$capi_keep`.
        unsafe {
            let failed = |ctx: *mut MrContext| {
                let (vm, _) = parts(ctx);
                assert!(vm.has_error());
                vm.take_error();
            };
            assert_eq!(mr_str_coderange(ctx, sym.id()), -1);
            failed(ctx);
            assert_eq!(mr_str_capacity(ctx, sym.id()), -1);
            failed(ctx);
            assert!(mr_str_enc_name(ctx, sym.id()).is_null());
            failed(ctx);
            assert_eq!(mr_str_subseq(ctx, sym.id(), 0, 1), MR_UNDEF);
            failed(ctx);
            assert_eq!(mr_str_dup(ctx, sym.id()), MR_UNDEF);
            failed(ctx);
            assert_eq!(mr_str_set_enc(ctx, sym.id(), c"UTF-8".as_ptr()), -1);
            failed(ctx);
            assert_eq!(mr_str_set_enc(ctx, long.id(), c"NO-SUCH-ENCODING".as_ptr()), -2);
            assert_eq!(mr_str_set_enc(ctx, frozen.id(), c"US-ASCII".as_ptr()), -1);
            failed(ctx);
            // A subclass instance goes through `dup`, keeping its class.
            let d = Value::from_u64(mr_str_dup(ctx, sub.id()));
            assert_eq!(d.class(), sub.class());
            assert_eq!(d.as_rstring_inner().to_str().unwrap(), "ab");
            // A slice viewing the long string's buffer holds its own length.
            let slice = mr_str_subseq(ctx, long.id(), 1, 90);
            assert!(mr_str_capacity(ctx, slice) >= 90);
            assert!(mr_str_capacity(ctx, long.id()) >= 100);
            // funcall_kw
            let call = IdentId::get_id("call").get() as MrSym;
            assert_eq!(mr_funcall_kw(ctx, lam.id(), MR_NO_SYM, 0, std::ptr::null(), MR_UNDEF, 1), MR_UNDEF);
            failed(ctx);
            assert_eq!(mr_funcall_kw(ctx, MR_UNDEF, call, 0, std::ptr::null(), MR_UNDEF, 1), MR_UNDEF);
            // Keywords asked for, but the last argument is not a Hash.
            let one = [Value::i32(1).id()];
            let r = Value::from_u64(mr_funcall_kw(ctx, lam.id(), call, 1, one.as_ptr(), MR_UNDEF, 1));
            assert_eq!(r.as_array_inner().len(), 2);
            let args = [Value::i32(1).id(), boom.id()];
            assert_eq!(mr_funcall_kw(ctx, lam.id(), call, 2, args.as_ptr(), MR_UNDEF, 1), MR_UNDEF);
            failed(ctx);
            // Globals: a plain one, and one whose setter refuses.
            assert_eq!(mr_gvar_set(ctx, c"$capi_abi_gv".as_ptr(), Value::i32(5).id()), 0);
            assert_eq!(Value::from_u64(mr_gvar_get(ctx, c"$capi_abi_gv".as_ptr())).try_fixnum(), Some(5));
            assert_eq!(mr_gvar_set(ctx, c"$$".as_ptr(), Value::i32(5).id()), -1);
            failed(ctx);
            assert_eq!(mr_gvar_set(ctx, c"$capi_abi_gv".as_ptr(), MR_UNDEF), -1);
        }
    }

    #[test]
    fn enc_codepoint_edges() {
        let bytes = "\u{3042}".as_bytes();
        let p = bytes.as_ptr();
        // SAFETY: `p..p+n` is inside `bytes`; the context is unused.
        unsafe {
            let e = p.add(bytes.len());
            let ctx = std::ptr::null_mut();
            let mut len = 0;
            assert_eq!(mr_enc_codepoint(ctx, c"UTF-8".as_ptr(), p, e, &mut len), 0x3042);
            assert_eq!(len, 3);
            assert_eq!(mr_enc_codepoint(ctx, c"UTF-8".as_ptr(), p, p, &mut len), -1);
            assert_eq!(mr_enc_codepoint(ctx, c"UTF-8".as_ptr(), p, p.add(2), &mut len), -1);
            assert_eq!(mr_enc_codepoint(ctx, c"UTF-8".as_ptr(), p.add(1), e, &mut len), -1);
            // A byte per character where monoruby has no decoder.
            assert_eq!(mr_enc_codepoint(ctx, c"NO-SUCH-ENCODING".as_ptr(), p, e, std::ptr::null_mut()), 0xe3);
            assert_eq!(mr_enc_codepoint(ctx, c"ASCII-8BIT".as_ptr(), p.add(1), e, &mut len), 0x81);
            assert_eq!(len, 1);
        }
    }

    #[test]
    fn enc_precise_mbclen_edges() {
        let bytes = "\u{3042}".as_bytes();
        let p = bytes.as_ptr();
        // SAFETY: `p..p+n` is inside `bytes`; the context is unused.
        unsafe {
            let e = p.add(bytes.len());
            let ctx = std::ptr::null_mut();
            assert_eq!(mr_enc_precise_mbclen(ctx, c"UTF-8".as_ptr(), p, e), 3);
            // A truncated character wants more bytes; an empty range too.
            assert_eq!(mr_enc_precise_mbclen(ctx, c"UTF-8".as_ptr(), p, p.add(1)), -2);
            assert_eq!(mr_enc_precise_mbclen(ctx, c"UTF-8".as_ptr(), p, p), -2);
            // A high byte alone is invalid in UTF-8 and one character in BINARY.
            assert_eq!(mr_enc_precise_mbclen(ctx, c"UTF-8".as_ptr(), p.add(1), e), -1);
            assert_eq!(mr_enc_precise_mbclen(ctx, c"ASCII-8BIT".as_ptr(), p.add(1), e), 1);
            // An encoding name monoruby does not know counts bytes.
            assert_eq!(mr_enc_precise_mbclen(ctx, c"NO-SUCH-ENCODING".as_ptr(), p, e), 1);
        }
    }
}
