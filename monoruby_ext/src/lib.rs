//! Safe Rust over `monoruby-ext-sys`, for writing a monoruby extension in
//! Rust: a [`Value`], a [`Ctx`] whose methods are the table's entries with
//! Rust types, [`Error`] as "an error is pending", native payloads as
//! ordinary Rust types, and the [`method!`] / [`native!`] macros that make
//! the `extern "C"` glue.
//!
//! An extension crate is a `cdylib` depending on this crate alone (never
//! on `monoruby`), exporting one `Init_<name>`:
//!
//! ```ignore
//! use monoruby_ext::*;
//!
//! fn add(ctx: &mut Ctx, _self: Value, args: &[Value], _block: Block) -> Result<Value> {
//!     let a = ctx.int(args[0])?;
//!     let b = ctx.int(args[1])?;
//!     Ok(Value::int(a + b))
//! }
//!
//! #[unsafe(no_mangle)]
//! pub unsafe extern "C" fn Init_hello(ctx: *mut MrContext) -> c_int {
//!     init(ctx, |ctx| {
//!         let m = ctx.define_module(Value::UNDEF, "Hello")?;
//!         ctx.define_method(m, "add", method!(add), 2, MR_METHOD_SINGLETON);
//!         Ok(())
//!     })
//! }
//! ```
//!
//! The rules of the ABI (`monoruby_ext_sys`'s crate docs) still apply;
//! this layer makes the first two automatic (no monoruby link, no unwind:
//! a panic in a method becomes a RuntimeError) and the third visible (a
//! `Value` is `Copy` and the collector does not see it: root it with
//! [`Ctx::temp_push`] / [`Ctx::pin`] or a native payload's `mark` across
//! any call back into Ruby).

#![allow(clippy::missing_safety_doc)]

pub use monoruby_ext_sys::*;
use std::ffi::{CString, c_char, c_int, c_void};

// ---------------------------------------------------------------------
// Values
// ---------------------------------------------------------------------

/// A Ruby value. `Copy`, opaque, and *not* a root: see the crate docs.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Value(pub MrValue);

impl Value {
    pub const UNDEF: Value = Value(MR_UNDEF);
    pub const NIL: Value = Value(MR_NIL);
    pub const TRUE: Value = Value(MR_TRUE);
    pub const FALSE: Value = Value(MR_FALSE);

    pub fn nil() -> Value {
        Value::NIL
    }

    pub fn bool(b: bool) -> Value {
        if b { Value::TRUE } else { Value::FALSE }
    }

    pub fn int(i: i64) -> Value {
        Value(api_int_new(i))
    }

    pub fn float(f: f64) -> Value {
        Value(api_float_new(f))
    }

    pub fn is_undef(self) -> bool {
        self.0 == MR_UNDEF
    }

    pub fn is_nil(self) -> bool {
        self.0 == MR_NIL
    }

    /// Ruby truthiness: anything but `nil` and `false`.
    pub fn truthy(self) -> bool {
        self.0 != MR_NIL && self.0 != MR_FALSE
    }

    pub fn raw(self) -> MrValue {
        self.0
    }

    fn some(self) -> Option<Value> {
        if self.is_undef() { None } else { Some(self) }
    }
}

/// `nil`.
impl Default for Value {
    fn default() -> Value {
        Value::NIL
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Value({:#x})", self.0)
    }
}

/// The block handle a method received (`MR_UNDEF` = none). Valid during
/// that call; [`Ctx::block_to_proc`] keeps it beyond.
#[derive(Clone, Copy)]
pub struct Block(pub MrValue);

impl Block {
    pub fn is_given(self) -> bool {
        self.0 != MR_UNDEF
    }
}

/// "An error is pending on the interpreter": the method returns
/// `MR_UNDEF` and the interpreter raises it. Made by [`Ctx::raise`] and
/// friends, or inherited from a Ruby call that raised.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Error;

pub type Result<T> = std::result::Result<T, Error>;

// The two entries that take no context are reached through the table
// the last `Init_` / method call saw. Every extension is loaded into one
// interpreter, whose table never moves, so a single static suffices.
static API: std::sync::atomic::AtomicPtr<MrApi> =
    std::sync::atomic::AtomicPtr::new(std::ptr::null_mut());

fn api() -> &'static MrApi {
    let p = API.load(std::sync::atomic::Ordering::Relaxed);
    assert!(!p.is_null(), "monoruby_ext used before Init_");
    // SAFETY: set from a context the interpreter handed us; the table is
    // a static on its side.
    unsafe { &*p }
}

fn api_int_new(i: i64) -> MrValue {
    // SAFETY: a pure constructor.
    unsafe { (api().int_new)(i) }
}

fn api_float_new(f: f64) -> MrValue {
    // SAFETY: a pure constructor.
    unsafe { (api().float_new)(f) }
}

// ---------------------------------------------------------------------
// The context
// ---------------------------------------------------------------------

/// An interned method name, from [`Ctx::intern`]. Copyable, and valid
/// for the life of the process.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Sym(MrSym);

/// The interpreter, for the duration of one call into the extension.
/// Methods that run Ruby (or park the green thread) take `&mut self`, so
/// a borrow handed out by `&self` — a String's bytes — cannot be held
/// across them.
pub struct Ctx {
    raw: *mut MrContext,
}

impl Ctx {
    /// # Safety
    /// `raw` is the context of the call currently being made into the
    /// extension (or a C callback running inside it).
    pub unsafe fn from_raw(raw: *mut MrContext) -> Ctx {
        // SAFETY: the caller's contract.
        API.store(
            unsafe { (*raw).api } as *mut MrApi,
            std::sync::atomic::Ordering::Relaxed,
        );
        Ctx { raw }
    }

    pub fn raw(&self) -> *mut MrContext {
        self.raw
    }

    /// The identity of the interpreter this context belongs to. One
    /// process can hold several (monoruby's test harness builds one per
    /// test, on its own thread) and the extension's `Init_` runs once in
    /// each, while its `static`s are shared by all — so state that refers
    /// to one interpreter's objects (the classes `Init_` defined) is
    /// keyed by this, not kept in a plain `static`.
    pub fn interpreter_id(&self) -> usize {
        // SAFETY: a live context.
        unsafe { (*self.raw).globals as usize }
    }

    fn api(&self) -> &MrApi {
        // SAFETY: a live context.
        unsafe { &*(*self.raw).api }
    }

    fn ret(&self, v: MrValue) -> Result<Value> {
        Value(v).some().ok_or(Error)
    }

    // ---- errors --------------------------------------------------------

    /// Raise `exc_class.new(msg)`.
    pub fn raise(&mut self, exc_class: Value, msg: impl AsRef<str>) -> Error {
        let m = msg.as_ref();
        // SAFETY: a live context; the message is read before returning.
        unsafe { (self.api().raise)(self.raw, exc_class.0, m.as_ptr() as *const c_char, m.len()) };
        Error
    }

    pub fn raise_kind(&mut self, kind: MrErrorKind, msg: impl AsRef<str>) -> Error {
        let m = msg.as_ref();
        // SAFETY: as `raise`.
        unsafe { (self.api().raise_kind)(self.raw, kind, m.as_ptr() as *const c_char, m.len()) };
        Error
    }

    pub fn runtime_error(&mut self, msg: impl AsRef<str>) -> Error {
        self.raise_kind(MrErrorKind::Runtime, msg)
    }

    pub fn argument_error(&mut self, msg: impl AsRef<str>) -> Error {
        self.raise_kind(MrErrorKind::Argument, msg)
    }

    pub fn type_error(&mut self, msg: impl AsRef<str>) -> Error {
        self.raise_kind(MrErrorKind::Type, msg)
    }

    pub fn error_pending(&self) -> bool {
        // SAFETY: a live context.
        unsafe { (self.api().error_pending)(self.raw) != 0 }
    }

    /// The pending error as an exception object, cleared from the
    /// interpreter — to carry across frames that cannot return `Err`,
    /// then [`Ctx::raise_exception`].
    pub fn error_take(&mut self) -> Option<Value> {
        // SAFETY: a live context.
        Value(unsafe { (self.api().error_take)(self.raw) }).some()
    }

    pub fn raise_exception(&mut self, exc: Value) -> Error {
        // SAFETY: a live context.
        unsafe { (self.api().raise_exception)(self.raw, exc.0) };
        Error
    }

    // ---- classes -------------------------------------------------------

    pub fn object_class(&self) -> Value {
        // SAFETY: a live context.
        Value(unsafe { (self.api().object_class)(self.raw) })
    }

    /// `outer::name` (`Value::UNDEF` outer = top level), defined under
    /// `superclass` (`Value::UNDEF` = Object) or reopened.
    pub fn define_class(
        &mut self,
        outer: Value,
        name: &str,
        superclass: Value,
        flags: u32,
    ) -> Result<Value> {
        let c = cstring(name);
        // SAFETY: a live context and a NUL-terminated name.
        self.ret(unsafe {
            (self.api().define_class)(self.raw, outer.0, c.as_ptr(), superclass.0, flags)
        })
    }

    pub fn define_module(&mut self, outer: Value, name: &str) -> Result<Value> {
        let c = cstring(name);
        // SAFETY: as `define_class`.
        self.ret(unsafe { (self.api().define_module)(self.raw, outer.0, c.as_ptr()) })
    }

    /// Define `name` on `klass` as `f` (from [`method!`]), taking `argc`
    /// positional arguments (`MR_ARGC_VARIADIC` for any number), with
    /// `MR_METHOD_*` flags.
    pub fn define_method(
        &mut self,
        klass: Value,
        name: &str,
        f: MrMethodFn,
        argc: c_int,
        flags: u32,
    ) {
        let c = cstring(name);
        // SAFETY: as `define_class`.
        unsafe { (self.api().define_method)(self.raw, klass.0, c.as_ptr(), f, argc, flags) }
    }

    /// `outer::name`, `None` when undefined.
    pub fn const_get(&self, outer: Value, name: &str) -> Option<Value> {
        let c = cstring(name);
        // SAFETY: as `define_class`.
        Value(unsafe { (self.api().const_get)(self.raw, outer.0, c.as_ptr()) }).some()
    }

    pub fn const_set(&mut self, outer: Value, name: &str, v: Value) {
        let c = cstring(name);
        // SAFETY: as `define_class`.
        unsafe { (self.api().const_set)(self.raw, outer.0, c.as_ptr(), v.0) }
    }

    // ---- values --------------------------------------------------------

    pub fn type_of(&self, v: Value) -> MrType {
        // SAFETY: a live context.
        unsafe { (self.api().type_of)(self.raw, v.0) }
    }

    pub fn class_of(&self, v: Value) -> Value {
        // SAFETY: a live context.
        Value(unsafe { (self.api().class_of)(self.raw, v.0) })
    }

    pub fn is_kind_of(&self, v: Value, klass: Value) -> bool {
        // SAFETY: a live context.
        unsafe { (self.api().is_kind_of)(self.raw, v.0, klass.0) != 0 }
    }

    /// `v` as an integer, converting with `to_int`; a TypeError otherwise.
    pub fn int(&mut self, v: Value) -> Result<i64> {
        let mut out = 0i64;
        // SAFETY: a live context and our own out-slot.
        if unsafe { (self.api().int_get)(self.raw, v.0, &mut out) } != 0 {
            Ok(out)
        } else {
            Err(Error)
        }
    }

    pub fn float(&mut self, v: Value) -> Result<f64> {
        let mut out = 0f64;
        // SAFETY: as `int`.
        if unsafe { (self.api().float_get)(self.raw, v.0, &mut out) } != 0 {
            Ok(out)
        } else {
            Err(Error)
        }
    }

    /// A UTF-8 String.
    pub fn str(&self, s: impl AsRef<[u8]>) -> Value {
        let s = s.as_ref();
        // SAFETY: a live context; the bytes are copied.
        Value(unsafe { (self.api().str_new)(self.raw, s.as_ptr(), s.len()) })
    }

    /// A BINARY String.
    pub fn bytes(&self, s: impl AsRef<[u8]>) -> Value {
        let s = s.as_ref();
        // SAFETY: as `str`.
        Value(unsafe { (self.api().bytes_new)(self.raw, s.as_ptr(), s.len()) })
    }

    /// The bytes of a String, borrowed until the interpreter next runs
    /// (which every `&mut self` method may do). A TypeError otherwise.
    pub fn str_bytes(&self, v: Value) -> Result<&[u8]> {
        let mut len = 0usize;
        // SAFETY: a live context and our own out-slot.
        let p = unsafe { (self.api().str_ptr)(self.raw, v.0, &mut len) };
        if p.is_null() {
            return Err(Error);
        }
        // SAFETY: `len` bytes the interpreter owns, borrowed for `&self`.
        Ok(unsafe { std::slice::from_raw_parts(p, len) })
    }

    /// The bytes of a String, copied.
    pub fn str_vec(&self, v: Value) -> Result<Vec<u8>> {
        self.str_bytes(v).map(|b| b.to_vec())
    }

    /// A String as `String` (lossily).
    pub fn str_string(&self, v: Value) -> Result<String> {
        self.str_bytes(v)
            .map(|b| String::from_utf8_lossy(b).into_owned())
    }

    pub fn is_string(&self, v: Value) -> bool {
        self.type_of(v) == MrType::String
    }

    /// The name of a String's encoding (`"UTF-8"`, `"ASCII-8BIT"`, ...).
    pub fn str_encoding(&mut self, v: Value) -> Result<String> {
        // SAFETY: a live context.
        let s = self.ret(unsafe { (self.api().str_encoding)(self.raw, v.0) })?;
        self.str_string(s)
    }

    /// Whether a String is BINARY (ASCII-8BIT) encoded.
    pub fn str_is_binary(&mut self, v: Value) -> Result<bool> {
        Ok(self.str_encoding(v)? == "ASCII-8BIT")
    }

    pub fn sym(&self, name: &str) -> Value {
        // SAFETY: as `str`.
        Value(unsafe { (self.api().sym_new)(self.raw, name.as_ptr(), name.len()) })
    }

    /// A Symbol's name; a TypeError for anything else.
    pub fn sym_name(&mut self, v: Value) -> Result<String> {
        // SAFETY: a live context.
        let s = self.ret(unsafe { (self.api().sym_to_str)(self.raw, v.0) })?;
        self.str_string(s)
    }

    pub fn ary_new(&self) -> Value {
        // SAFETY: a live context.
        Value(unsafe { (self.api().ary_new)(self.raw) })
    }

    pub fn ary_from(&self, items: &[Value]) -> Value {
        let a = self.ary_new();
        for v in items {
            let _ = self.ary_push(a, *v);
        }
        a
    }

    pub fn ary_push(&self, ary: Value, v: Value) -> Result<()> {
        // SAFETY: a live context.
        if unsafe { (self.api().ary_push)(self.raw, ary.0, v.0) } == 0 {
            Ok(())
        } else {
            Err(Error)
        }
    }

    pub fn ary_len(&self, ary: Value) -> usize {
        // SAFETY: a live context.
        unsafe { (self.api().ary_len)(self.raw, ary.0) }
    }

    pub fn ary_get(&self, ary: Value, idx: usize) -> Option<Value> {
        // SAFETY: a live context.
        Value(unsafe { (self.api().ary_get)(self.raw, ary.0, idx) }).some()
    }

    pub fn hash_new(&self) -> Value {
        // SAFETY: a live context.
        Value(unsafe { (self.api().hash_new)(self.raw) })
    }

    pub fn hash_set(&mut self, hash: Value, key: Value, v: Value) -> Result<()> {
        // SAFETY: a live context.
        if unsafe { (self.api().hash_set)(self.raw, hash.0, key.0, v.0) } == 0 {
            Ok(())
        } else {
            Err(Error)
        }
    }

    pub fn hash_get(&mut self, hash: Value, key: Value) -> Result<Option<Value>> {
        // SAFETY: a live context.
        let v = Value(unsafe { (self.api().hash_get)(self.raw, hash.0, key.0) });
        if self.error_pending() {
            Err(Error)
        } else {
            Ok(v.some())
        }
    }

    /// `name` includes the `@`; `nil` when unset.
    pub fn ivar_get(&self, obj: Value, name: &str) -> Value {
        let c = cstring(name);
        // SAFETY: a live context and a NUL-terminated name.
        Value(unsafe { (self.api().ivar_get)(self.raw, obj.0, c.as_ptr()) })
    }

    pub fn ivar_set(&mut self, obj: Value, name: &str, v: Value) -> Result<()> {
        let c = cstring(name);
        // SAFETY: as `ivar_get`.
        if unsafe { (self.api().ivar_set)(self.raw, obj.0, c.as_ptr(), v.0) } == 0 {
            Ok(())
        } else {
            Err(Error)
        }
    }

    /// `v.inspect`, without running Ruby.
    pub fn inspect(&self, v: Value) -> String {
        // SAFETY: a live context.
        let s = Value(unsafe { (self.api().inspect)(self.raw, v.0) });
        self.str_string(s).unwrap_or_default()
    }

    /// The name of `v`'s class.
    pub fn class_name(&self, v: Value) -> String {
        self.inspect(self.class_of(v))
    }

    /// Whether `v` has a method `name` (private ones included), by lookup
    /// alone — nothing runs.
    pub fn respond_to(&self, v: Value, name: &str) -> bool {
        let c = cstring(name);
        // SAFETY: a live context and a NUL-terminated name.
        unsafe { (self.api().respond_to)(self.raw, v.0, c.as_ptr()) != 0 }
    }

    /// `recv.name(*args)` if `recv` has such a method, `None` otherwise.
    pub fn funcall_if_exists(&mut self, recv: Value, name: &str, args: &[Value]) -> Result<Option<Value>> {
        if self.respond_to(recv, name) { self.funcall(recv, name, args, None).map(Some) } else { Ok(None) }
    }

    // ---- native objects ------------------------------------------------

    /// An instance of `klass` (defined with `MR_CLASS_NATIVE`) owning
    /// `data`.
    pub fn native_new<T: Native>(&mut self, klass: Value, data: T) -> Result<Value> {
        let p = Box::into_raw(Box::new(data)) as *mut c_void;
        // SAFETY: a live context; the box is owned by the object from
        // here, freed through `T::OPS.free`.
        let v = unsafe { (self.api().native_new)(self.raw, klass.0, p, T::ops()) };
        if v == MR_UNDEF {
            // SAFETY: not taken; ours again.
            drop(unsafe { Box::from_raw(p as *mut T) });
            return Err(Error);
        }
        Ok(Value(v))
    }

    /// The payload of `obj` if it is a `T`.
    ///
    /// The borrow is unbounded on purpose: the payload lives as long as
    /// its object, which the caller's frame holds; do not keep it across
    /// a call that may replace the payload (`native_set` on the same
    /// object) or collect the object.
    pub fn native<T: Native>(&self, obj: Value) -> Option<&'static mut T> {
        // SAFETY: a live context; the pointer is one `native_new` /
        // `native_set` stored with `T::OPS`, so it is a `T`.
        let p = unsafe { (self.api().native_data)(self.raw, obj.0, T::ops()) } as *mut T;
        if p.is_null() {
            None
        } else {
            Some(unsafe { &mut *p })
        }
    }

    /// Give `obj` the payload `data` (replacing and freeing any old one).
    pub fn native_set<T: Native>(&mut self, obj: Value, data: T) -> Result<()> {
        let p = Box::into_raw(Box::new(data)) as *mut c_void;
        // SAFETY: as `native_new`.
        if unsafe { (self.api().native_set)(self.raw, obj.0, p, T::ops()) } == 0 {
            Ok(())
        } else {
            // SAFETY: not taken; ours again.
            drop(unsafe { Box::from_raw(p as *mut T) });
            Err(Error)
        }
    }

    // ---- GC ------------------------------------------------------------

    /// Root `v` until the current method call returns.
    pub fn temp_push(&mut self, v: Value) {
        // SAFETY: a live context.
        unsafe { (self.api().temp_push)(self.raw, v.0) }
    }

    pub fn temp_len(&self) -> usize {
        // SAFETY: a live context.
        unsafe { (self.api().temp_len)(self.raw) }
    }

    pub fn temp_truncate(&mut self, len: usize) {
        // SAFETY: a live context.
        unsafe { (self.api().temp_truncate)(self.raw, len) }
    }

    /// Root `v` until [`Ctx::unpin`] (counted).
    pub fn pin(&mut self, v: Value) {
        // SAFETY: a live context.
        unsafe { (self.api().gc_pin)(self.raw, v.0) }
    }

    pub fn unpin(&mut self, v: Value) {
        // SAFETY: a live context.
        unsafe { (self.api().gc_unpin)(self.raw, v.0) }
    }

    // ---- calling Ruby --------------------------------------------------

    /// Intern `name`, giving a [`Sym`] to call it by.
    ///
    /// Interning hashes the name under a lock, so take the symbol once —
    /// before a loop, or in `Init_` — and call with
    /// [`funcall_sym`](Self::funcall_sym) inside it. A `Sym` never goes
    /// stale: monoruby interns per process and never forgets, so one
    /// taken in `Init_` is good for every interpreter afterwards.
    ///
    /// # Panics
    /// If `name` is not valid UTF-8 — which, for a `&str`, it is.
    pub fn intern(&self, name: &str) -> Sym {
        // SAFETY: a live context; `name.len()` bytes readable at its
        // pointer.
        let sym = unsafe { (self.api().intern)(self.raw, name.as_ptr(), name.len()) };
        assert_ne!(MR_NO_SYM, sym, "could not intern {name:?}");
        Sym(sym)
    }

    /// [`funcall`](Self::funcall) with the name already interned. Use it
    /// wherever the same method is called more than a few times: it skips
    /// the C string and the interning, which is most of the cost of
    /// calling a small Ruby method from here.
    pub fn funcall_sym(
        &mut self,
        recv: Value,
        sym: Sym,
        args: &[Value],
        block: Option<Block>,
    ) -> Result<Value> {
        let b = block.map_or(MR_UNDEF, |b| b.0);
        // SAFETY: a live context, a symbol from `intern`, `args.len()`
        // values at `args`.
        self.ret(unsafe {
            (self.api().funcall_sym)(
                self.raw,
                recv.0,
                sym.0,
                args.len() as c_int,
                args.as_ptr() as *const MrValue,
                b,
            )
        })
    }

    /// `recv.name(*args, &block)`, private methods included.
    pub fn funcall(
        &mut self,
        recv: Value,
        name: &str,
        args: &[Value],
        block: Option<Block>,
    ) -> Result<Value> {
        let c = cstring(name);
        let b = block.map_or(MR_UNDEF, |b| b.0);
        // SAFETY: a live context, a NUL-terminated name, `args.len()`
        // values at `args`.
        self.ret(unsafe {
            (self.api().funcall)(
                self.raw,
                recv.0,
                c.as_ptr(),
                args.len() as c_int,
                args.as_ptr() as *const MrValue,
                b,
            )
        })
    }

    pub fn yield_block(&mut self, block: Block, args: &[Value]) -> Result<Value> {
        // SAFETY: as `funcall`.
        self.ret(unsafe {
            (self.api().yield_block)(
                self.raw,
                block.0,
                args.len() as c_int,
                args.as_ptr() as *const MrValue,
            )
        })
    }

    /// The block as a Proc, to keep beyond this call. `nil` when none.
    pub fn block_to_proc(&mut self, block: Block) -> Result<Value> {
        // SAFETY: a live context.
        self.ret(unsafe { (self.api().block_to_proc)(self.raw, block.0) })
    }

    pub fn proc_call(&mut self, proc_: Value, args: &[Value]) -> Result<Value> {
        // SAFETY: as `funcall`.
        self.ret(unsafe {
            (self.api().proc_call)(
                self.raw,
                proc_.0,
                args.len() as c_int,
                args.as_ptr() as *const MrValue,
            )
        })
    }

    // ---- threads -------------------------------------------------------

    /// Run `f` on a worker thread while this green thread parks. `f` must
    /// not touch Ruby; it may use anything it captured (the closure lives
    /// in this frame, which stays put).
    pub fn call_blocking<F: FnOnce() -> i64>(&mut self, f: F) -> Result<i64> {
        unsafe extern "C" fn thunk<F: FnOnce() -> i64>(arg: *mut c_void) -> i64 {
            // SAFETY: `arg` is the `Option<F>` below, taken exactly once.
            let slot = unsafe { &mut *(arg as *mut Option<F>) };
            (slot.take().expect("blocking closure run twice"))()
        }
        let mut slot: Option<F> = Some(f);
        let mut out = 0i64;
        // SAFETY: a live context; `slot` and `out` outlive the call — the
        // green thread parks here until the worker is done.
        let rc = unsafe {
            (self.api().call_blocking)(
                self.raw,
                thunk::<F>,
                &mut slot as *mut Option<F> as *mut c_void,
                &mut out,
            )
        };
        if rc == 0 { Ok(out) } else { Err(Error) }
    }

    // ---- misc ----------------------------------------------------------

    pub fn ruby_version(&self) -> String {
        // SAFETY: a static NUL-terminated string.
        unsafe { std::ffi::CStr::from_ptr((self.api().ruby_version)()) }
            .to_string_lossy()
            .into_owned()
    }
}

fn cstring(s: &str) -> CString {
    CString::new(
        s.as_bytes()
            .iter()
            .copied()
            .take_while(|&b| b != 0)
            .collect::<Vec<u8>>(),
    )
    .unwrap()
}

// ---------------------------------------------------------------------
// Native payloads
// ---------------------------------------------------------------------

/// The collector's cursor, inside [`Native::mark`].
pub struct Marker(*mut MrMarker);

impl From<&Value> for Value {
    fn from(v: &Value) -> Value {
        *v
    }
}

impl Marker {
    pub fn mark(&mut self, v: impl Into<Value>) {
        let v: Value = v.into();
        if !v.is_undef() {
            // SAFETY: a marker the interpreter is passing right now.
            unsafe { (api().gc_mark)(self.0, v.0) }
        }
    }
}

/// A Rust type that can be the payload of a native object. Implemented
/// with [`native!`], which also makes the static `MrNativeOps` the type
/// is known by.
pub trait Native: Sized + 'static {
    /// Report every `Value` the payload holds.
    fn mark(&self, _marker: &mut Marker) {}
    /// A copy for `dup` / `clone`; `None` (the default) leaves the copy
    /// without a payload.
    fn dup(&self) -> Option<Self> {
        None
    }
    fn ops() -> &'static MrNativeOps;
}

pub unsafe extern "C" fn __native_mark<T: Native>(data: *mut c_void, marker: *mut MrMarker) {
    // SAFETY: `data` is a `T` we boxed; the marker is the interpreter's.
    unsafe { (*(data as *const T)).mark(&mut Marker(marker)) }
}

pub unsafe extern "C" fn __native_free<T: Native>(data: *mut c_void) {
    // SAFETY: the box `native_new` / `native_set` leaked, handed back once.
    drop(unsafe { Box::from_raw(data as *mut T) });
}

pub unsafe extern "C" fn __native_dup<T: Native>(data: *mut c_void) -> *mut c_void {
    // SAFETY: `data` is a `T` we boxed.
    match unsafe { (*(data as *const T)).dup() } {
        Some(copy) => Box::into_raw(Box::new(copy)) as *mut c_void,
        None => std::ptr::null_mut(),
    }
}

/// `native!(Type, "Class::Name")`: the `MrNativeOps` static for a type
/// implementing `Native`'s `mark` / `dup` (and its `ops`).
///
/// ```ignore
/// struct Counter { n: i64, tag: Value }
/// native!(Counter, "Hello::Counter", |c, m| m.mark(c.tag));
/// ```
#[macro_export]
macro_rules! native {
    ($ty:ty, $name:literal) => {
        $crate::native!($ty, $name, |_this, _marker| {}, |_this| None);
    };
    ($ty:ty, $name:literal, $mark:expr) => {
        $crate::native!($ty, $name, $mark, |_this| None);
    };
    ($ty:ty, $name:literal, $mark:expr, $dup:expr) => {
        impl $crate::Native for $ty {
            fn mark(&self, marker: &mut $crate::Marker) {
                let f: fn(&$ty, &mut $crate::Marker) = $mark;
                f(self, marker)
            }
            fn dup(&self) -> Option<Self> {
                let f: fn(&$ty) -> Option<$ty> = $dup;
                f(self)
            }
            fn ops() -> &'static $crate::MrNativeOps {
                static OPS: $crate::MrNativeOps = $crate::MrNativeOps {
                    name: concat!($name, "\0").as_ptr() as *const ::std::ffi::c_char,
                    mark: Some($crate::__native_mark::<$ty>),
                    free: Some($crate::__native_free::<$ty>),
                    dup: Some($crate::__native_dup::<$ty>),
                };
                &OPS
            }
        }
    };
}

// ---------------------------------------------------------------------
// Methods and Init
// ---------------------------------------------------------------------

/// The Rust shape of an extension method.
pub type Method = fn(&mut Ctx, Value, &[Value], Block) -> Result<Value>;

/// Called by the `extern "C"` thunk [`method!`] makes: run `f`, turning
/// `Err` into `MR_UNDEF` (the error is already pending) and a panic into
/// a RuntimeError.
pub unsafe fn __call(
    ctx: *mut MrContext,
    self_: MrValue,
    argc: c_int,
    argv: *const MrValue,
    block: MrValue,
    f: Method,
) -> MrValue {
    // SAFETY: the interpreter's contract for a method call.
    let mut cx = unsafe { Ctx::from_raw(ctx) };
    let args: &[Value] = if argc <= 0 {
        &[]
    } else {
        // SAFETY: `argc` values at `argv`; `Value` is transparent over `MrValue`.
        unsafe { std::slice::from_raw_parts(argv as *const Value, argc as usize) }
    };
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        f(&mut cx, Value(self_), args, Block(block))
    })) {
        Ok(Ok(v)) => v.0,
        Ok(Err(Error)) => MR_UNDEF,
        Err(payload) => {
            let msg = payload
                .downcast_ref::<&str>()
                .map(|s| s.to_string())
                .or_else(|| payload.downcast_ref::<String>().cloned())
                .unwrap_or_else(|| "panic".to_string());
            if !cx.error_pending() {
                cx.runtime_error(format!("extension panicked: {msg}"));
            }
            MR_UNDEF
        }
    }
}

/// The `MrMethodFn` for a Rust [`Method`].
#[macro_export]
macro_rules! method {
    ($f:expr) => {{
        unsafe extern "C" fn thunk(
            ctx: *mut $crate::MrContext,
            self_: $crate::MrValue,
            argc: ::std::ffi::c_int,
            argv: *const $crate::MrValue,
            block: $crate::MrValue,
        ) -> $crate::MrValue {
            // SAFETY: the interpreter's contract for a method call.
            unsafe { $crate::__call(ctx, self_, argc, argv, block, $f) }
        }
        thunk as $crate::MrMethodFn
    }};
}

/// The body of `Init_<name>`: check the ABI, run `f`, answer the status
/// the interpreter expects.
pub unsafe fn init(ctx: *mut MrContext, f: impl FnOnce(&mut Ctx) -> Result<()>) -> c_int {
    // SAFETY: the interpreter's contract for `Init_`.
    let mut cx = unsafe { Ctx::from_raw(ctx) };
    if cx.api().abi_version != MR_ABI_VERSION {
        cx.raise_kind(
            MrErrorKind::Load,
            format!(
                "extension built for monoruby ABI {} but this is {}",
                MR_ABI_VERSION,
                cx.api().abi_version
            ),
        );
        return 1;
    }
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f(&mut cx))) {
        Ok(Ok(())) => 0,
        Ok(Err(Error)) => 1,
        Err(_) => {
            if !cx.error_pending() {
                cx.raise_kind(MrErrorKind::Load, "extension panicked in Init_");
            }
            1
        }
    }
}
