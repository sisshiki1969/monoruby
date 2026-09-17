//! The C ABI between monoruby and a dynamically loaded extension.
//!
//! An extension is a shared library (`libfoo.so` / `libfoo.dylib`) exporting
//! `int Init_foo(MrContext *ctx)`. monoruby `dlopen`s it when Ruby code
//! `require`s `foo.so`, calls `Init_foo`, and from then on the extension's
//! methods run like any builtin. Everything the extension needs from the
//! interpreter comes through the [`MrApi`] table the context points at —
//! the extension never links against monoruby, and this crate has no
//! dependencies, so an extension in Rust depends on it alone.
//!
//! This is *not* the CRuby C API: a [`MrValue`] has monoruby's bit layout,
//! errors are returned, never unwound, and the interpreter is reached only
//! through the table. `doc/native_extension_loading.md` §4 has the design;
//! `include/monoruby_ext.h` is this file for C.
//!
//! # Rules an extension must keep
//!
//! 1. Never link the `monoruby` crate — call through `ctx->api`.
//! 2. Never unwind across the boundary: report an error with
//!    [`MrApi::raise`] / [`MrApi::raise_kind`] and return [`MR_UNDEF`].
//!    A Rust extension catches panics itself.
//! 3. Every [`MrValue`] the extension holds across a call back into Ruby
//!    ([`MrApi::funcall`], [`MrApi::yield_block`], [`MrApi::call_blocking`])
//!    is either on the temp stack ([`MrApi::temp_push`]), reachable from a
//!    native object's `mark`, or pinned ([`MrApi::gc_pin`]). Values in C
//!    memory the collector cannot see are otherwise reclaimed under it.
//! 4. A call that may block in the kernel goes through
//!    [`MrApi::call_blocking`], so the other green threads keep running.
//! 5. `Init_foo` is called once, when the library is loaded.
//!
//! # Versioning
//!
//! [`MrApi::abi_version`] is [`MR_ABI_VERSION`]; monoruby refuses an
//! extension whose `Init_` reports a mismatch (it returns non-zero after
//! checking). Entries are only ever appended to the table, and
//! [`MrApi::size`] says how many bytes of it the running monoruby fills.

#![no_std]
#![allow(non_camel_case_types)]

use core::ffi::{c_char, c_int, c_void};

/// The current ABI version. Bumped when an existing entry changes
/// meaning; appending entries does not bump it.
pub const MR_ABI_VERSION: u32 = 1;

/// A Ruby value, with monoruby's tagged 64-bit layout. Opaque to the
/// extension except for the immediates below and `0`.
pub type MrValue = u64;

/// Not a value. Returned by a method to mean "an error is pending"
/// (after `raise`), and by lookups to mean "absent".
pub const MR_UNDEF: MrValue = 0;
/// `nil`.
pub const MR_NIL: MrValue = 0x04;
/// `false`.
pub const MR_FALSE: MrValue = 0x14;
/// `true`.
pub const MR_TRUE: MrValue = 0x1c;

/// The interpreter-side context of one call into the extension. Only
/// `api` is for the extension; the rest is monoruby's, valid for the
/// duration of the call the context was handed to (including from any C
/// callback that runs inside that call).
#[repr(C)]
pub struct MrContext {
    pub api: *const MrApi,
    pub vm: *mut c_void,
    pub globals: *mut c_void,
    pub frame: *mut c_void,
    pub pc: *mut c_void,
}

/// An extension method. `argv[0..argc]` are the positional arguments
/// (the receiver is `self_`); `block` is the block handle (`MR_UNDEF`
/// when none was given), usable with `yield_block` / `block_to_proc`
/// during this call. Returns the result, or `MR_UNDEF` with an error
/// pending.
pub type MrMethodFn = unsafe extern "C" fn(
    ctx: *mut MrContext,
    self_: MrValue,
    argc: c_int,
    argv: *const MrValue,
    block: MrValue,
) -> MrValue;

/// `Init_<name>`: returns 0 on success. Any other value (or a pending
/// error) fails the `require` with LoadError.
pub type MrInitFn = unsafe extern "C" fn(ctx: *mut MrContext) -> c_int;

/// A function `call_blocking` runs on a worker thread. It must not touch
/// Ruby values or the API.
pub type MrBlockingFn = unsafe extern "C" fn(arg: *mut c_void) -> i64;

/// The collector's cursor, handed to a native object's `mark`; only
/// `gc_mark` takes it.
#[repr(C)]
pub struct MrMarker {
    _private: [u8; 0],
}

/// The callbacks of one kind of native object. The address of the
/// `MrNativeOps` is the kind's identity: `native_data(obj, ops)` answers
/// NULL for an object of another kind, so keep one static instance per
/// kind.
#[repr(C)]
pub struct MrNativeOps {
    /// For diagnostics.
    pub name: *const c_char,
    /// Report every `MrValue` the payload holds with `gc_mark`. NULL when
    /// it holds none.
    pub mark: Option<unsafe extern "C" fn(data: *mut c_void, marker: *mut MrMarker)>,
    /// Release the payload; called when the object is collected or its
    /// payload replaced by `native_set`. NULL when nothing needs freeing.
    pub free: Option<unsafe extern "C" fn(data: *mut c_void)>,
    /// A shallow copy for `Object#dup` / `#clone`, or NULL when the kind
    /// cannot be copied — the copy is then a payload-less object, and
    /// `native_data` answers NULL for it until `native_set`.
    pub dup: Option<unsafe extern "C" fn(data: *mut c_void) -> *mut c_void>,
}

/// What `type_of` answers.
#[repr(u32)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum MrType {
    Undef = 0,
    Nil = 1,
    True = 2,
    False = 3,
    Integer = 4,
    Float = 5,
    Symbol = 6,
    String = 7,
    Array = 8,
    Hash = 9,
    Class = 10,
    Module = 11,
    Proc = 12,
    Exception = 13,
    /// An object with a native payload (`native_new`).
    Native = 14,
    /// Anything else.
    Object = 15,
}

/// The built-in exception classes `raise_kind` takes, so the common
/// errors need no constant lookup.
#[repr(u32)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum MrErrorKind {
    Runtime = 0,
    Argument = 1,
    Type = 2,
    Range = 3,
    Index = 4,
    IO = 5,
    NotImplemented = 6,
    Frozen = 7,
    Load = 8,
}

/// `define_method` flags.
pub const MR_METHOD_PRIVATE: u32 = 1;
/// Define on the singleton class (a class method).
pub const MR_METHOD_SINGLETON: u32 = 2;
/// `define_class` flag: instances carry a native payload
/// (`native_new` / `native_set` / `native_data`).
pub const MR_CLASS_NATIVE: u32 = 1;
/// `define_method` argc: any number of arguments; the method sees them
/// all in `argv`.
pub const MR_ARGC_VARIADIC: c_int = -1;

/// The table. `abi_version` and `size` first; the rest in this order,
/// appended to and never reordered.
#[repr(C)]
pub struct MrApi {
    pub abi_version: u32,
    pub size: u32,

    // ---- errors --------------------------------------------------------
    /// Raise `exc_class.new(msg)`; the extension then returns `MR_UNDEF`.
    pub raise: unsafe extern "C" fn(
        ctx: *mut MrContext,
        exc_class: MrValue,
        msg: *const c_char,
        len: usize,
    ),
    /// Raise one of the built-in kinds.
    pub raise_kind: unsafe extern "C" fn(
        ctx: *mut MrContext,
        kind: MrErrorKind,
        msg: *const c_char,
        len: usize,
    ),
    /// Whether an error is pending (a callee's `MR_UNDEF` left one).
    pub error_pending: unsafe extern "C" fn(ctx: *mut MrContext) -> c_int,
    /// Take the pending error as an exception object (`MR_UNDEF` when
    /// none), clearing it — to stash one across C frames that cannot
    /// carry it, then `raise_exception` it afterwards.
    pub error_take: unsafe extern "C" fn(ctx: *mut MrContext) -> MrValue,
    /// Raise an exception object (one from `error_take`, or built with
    /// `funcall`).
    pub raise_exception: unsafe extern "C" fn(ctx: *mut MrContext, exc: MrValue),

    // ---- classes -------------------------------------------------------
    /// `Object`.
    pub object_class: unsafe extern "C" fn(ctx: *mut MrContext) -> MrValue,
    /// `outer::name` (`outer` = `MR_UNDEF` for the top level), defined
    /// with `superclass` (`MR_UNDEF` = `Object`) if it does not exist yet,
    /// reopened if it does. `MR_CLASS_NATIVE` for a class whose instances
    /// hold a payload.
    pub define_class: unsafe extern "C" fn(
        ctx: *mut MrContext,
        outer: MrValue,
        name: *const c_char,
        superclass: MrValue,
        flags: u32,
    ) -> MrValue,
    /// As `define_class`, for a module.
    pub define_module:
        unsafe extern "C" fn(ctx: *mut MrContext, outer: MrValue, name: *const c_char) -> MrValue,
    /// Define `name` on `klass`: `argc` positional arguments
    /// (`MR_ARGC_VARIADIC` for any number), `MR_METHOD_*` flags.
    pub define_method: unsafe extern "C" fn(
        ctx: *mut MrContext,
        klass: MrValue,
        name: *const c_char,
        f: MrMethodFn,
        argc: c_int,
        flags: u32,
    ),
    /// `outer::name`, or `MR_UNDEF` when undefined (no autoload, no error).
    pub const_get:
        unsafe extern "C" fn(ctx: *mut MrContext, outer: MrValue, name: *const c_char) -> MrValue,
    pub const_set: unsafe extern "C" fn(
        ctx: *mut MrContext,
        outer: MrValue,
        name: *const c_char,
        val: MrValue,
    ),

    // ---- values --------------------------------------------------------
    pub type_of: unsafe extern "C" fn(ctx: *mut MrContext, v: MrValue) -> MrType,
    pub class_of: unsafe extern "C" fn(ctx: *mut MrContext, v: MrValue) -> MrValue,
    pub is_kind_of: unsafe extern "C" fn(ctx: *mut MrContext, v: MrValue, klass: MrValue) -> c_int,
    pub int_new: unsafe extern "C" fn(i: i64) -> MrValue,
    /// `v` as an integer, converting with `to_int` as CRuby's `NUM2LL`
    /// does. 1 on success; 0 with a TypeError pending.
    pub int_get: unsafe extern "C" fn(ctx: *mut MrContext, v: MrValue, out: *mut i64) -> c_int,
    pub float_new: unsafe extern "C" fn(f: f64) -> MrValue,
    pub float_get: unsafe extern "C" fn(ctx: *mut MrContext, v: MrValue, out: *mut f64) -> c_int,
    /// A UTF-8 String.
    pub str_new: unsafe extern "C" fn(ctx: *mut MrContext, ptr: *const u8, len: usize) -> MrValue,
    /// A BINARY (ASCII-8BIT) String.
    pub bytes_new: unsafe extern "C" fn(ctx: *mut MrContext, ptr: *const u8, len: usize) -> MrValue,
    /// The bytes of a String, valid until the String is next mutated or
    /// Ruby runs. NULL with a TypeError pending when `v` is not a String.
    pub str_ptr:
        unsafe extern "C" fn(ctx: *mut MrContext, v: MrValue, len: *mut usize) -> *const u8,
    pub sym_new: unsafe extern "C" fn(ctx: *mut MrContext, ptr: *const u8, len: usize) -> MrValue,
    /// A Symbol's name as a String.
    pub sym_to_str: unsafe extern "C" fn(ctx: *mut MrContext, v: MrValue) -> MrValue,
    pub ary_new: unsafe extern "C" fn(ctx: *mut MrContext) -> MrValue,
    pub ary_push: unsafe extern "C" fn(ctx: *mut MrContext, ary: MrValue, v: MrValue) -> c_int,
    pub ary_len: unsafe extern "C" fn(ctx: *mut MrContext, ary: MrValue) -> usize,
    /// `MR_UNDEF` out of range.
    pub ary_get: unsafe extern "C" fn(ctx: *mut MrContext, ary: MrValue, idx: usize) -> MrValue,
    pub hash_new: unsafe extern "C" fn(ctx: *mut MrContext) -> MrValue,
    pub hash_set:
        unsafe extern "C" fn(ctx: *mut MrContext, hash: MrValue, key: MrValue, v: MrValue) -> c_int,
    /// `MR_UNDEF` when absent.
    pub hash_get: unsafe extern "C" fn(ctx: *mut MrContext, hash: MrValue, key: MrValue) -> MrValue,
    /// `nil` when unset. `name` includes the `@`.
    pub ivar_get:
        unsafe extern "C" fn(ctx: *mut MrContext, obj: MrValue, name: *const c_char) -> MrValue,
    pub ivar_set: unsafe extern "C" fn(
        ctx: *mut MrContext,
        obj: MrValue,
        name: *const c_char,
        v: MrValue,
    ) -> c_int,
    /// `v.inspect` (without calling Ruby).
    pub inspect: unsafe extern "C" fn(ctx: *mut MrContext, v: MrValue) -> MrValue,

    // ---- native objects ------------------------------------------------
    /// An instance of `klass` (defined with `MR_CLASS_NATIVE`) owning
    /// `data`, released and marked through `ops`.
    pub native_new: unsafe extern "C" fn(
        ctx: *mut MrContext,
        klass: MrValue,
        data: *mut c_void,
        ops: *const MrNativeOps,
    ) -> MrValue,
    /// The payload of `obj` if it is a native object of kind `ops`
    /// (NULL `ops` = any kind); NULL otherwise, with no error.
    pub native_data: unsafe extern "C" fn(
        ctx: *mut MrContext,
        obj: MrValue,
        ops: *const MrNativeOps,
    ) -> *mut c_void,
    /// Give `obj` (an instance of a native class, e.g. one `new`
    /// allocated before `initialize`) the payload `data`, freeing the
    /// old one. 0 on success; -1 with a TypeError pending.
    pub native_set: unsafe extern "C" fn(
        ctx: *mut MrContext,
        obj: MrValue,
        data: *mut c_void,
        ops: *const MrNativeOps,
    ) -> c_int,

    // ---- GC ------------------------------------------------------------
    /// From a native object's `mark` only.
    pub gc_mark: unsafe extern "C" fn(marker: *mut MrMarker, v: MrValue),
    /// Root `v` for the rest of the current call (the temp stack is
    /// truncated when the call returns).
    pub temp_push: unsafe extern "C" fn(ctx: *mut MrContext, v: MrValue),
    pub temp_len: unsafe extern "C" fn(ctx: *mut MrContext) -> usize,
    pub temp_truncate: unsafe extern "C" fn(ctx: *mut MrContext, len: usize),
    /// Root `v` until `gc_unpin` (counted: pin twice, unpin twice).
    pub gc_pin: unsafe extern "C" fn(ctx: *mut MrContext, v: MrValue),
    pub gc_unpin: unsafe extern "C" fn(ctx: *mut MrContext, v: MrValue),

    // ---- calling Ruby --------------------------------------------------
    /// `recv.name(*argv, &block)`, private methods included. `MR_UNDEF`
    /// with the error pending on a raise.
    pub funcall: unsafe extern "C" fn(
        ctx: *mut MrContext,
        recv: MrValue,
        name: *const c_char,
        argc: c_int,
        argv: *const MrValue,
        block: MrValue,
    ) -> MrValue,
    /// Yield to the block handle a method received.
    pub yield_block: unsafe extern "C" fn(
        ctx: *mut MrContext,
        block: MrValue,
        argc: c_int,
        argv: *const MrValue,
    ) -> MrValue,
    /// The block handle as a Proc object, to keep beyond the call.
    pub block_to_proc: unsafe extern "C" fn(ctx: *mut MrContext, block: MrValue) -> MrValue,
    pub proc_call: unsafe extern "C" fn(
        ctx: *mut MrContext,
        proc_: MrValue,
        argc: c_int,
        argv: *const MrValue,
    ) -> MrValue,

    // ---- threads -------------------------------------------------------
    /// Run `f(arg)` on a worker thread while this green thread parks.
    /// 0 with `f`'s result in `out`; -1 with an error pending when the
    /// wait was interrupted (the call still completes on the worker).
    pub call_blocking: unsafe extern "C" fn(
        ctx: *mut MrContext,
        f: MrBlockingFn,
        arg: *mut c_void,
        out: *mut i64,
    ) -> c_int,

    // ---- misc ----------------------------------------------------------
    /// `RUBY_VERSION`, NUL-terminated, static.
    pub ruby_version: unsafe extern "C" fn() -> *const c_char,
}
