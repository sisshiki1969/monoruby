use std::ffi::c_void;

use super::*;
use jitgen::{AbstractState, JitContext};
use libffi::middle::{Arg, Cif, CodePtr, Type};
use smallvec::SmallVec;
use crate::codegen::jitgen::deopt_log::DeoptCause;

// ---------------------------------------------------------------------------
// Fiddle type codes  (must match stdlib/fiddle.rb and gem/ffi_c.rb)
//
// We use CRuby's Fiddle convention: each canonical type has a small positive
// integer code, and the unsigned variant is its negation. opengl-bindings2
// and other CRuby-targeted gems write `-Fiddle::TYPE_INT` to mean
// "unsigned int", so we need the same convention.
//
// Platform aliases (INTPTR_T / SIZE_T / SSIZE_T / PTRDIFF_T / UINTPTR_T)
// alias to LONG / ULONG on x86-64 Linux; they are not separate Rust
// constants because match arms would collide.
// ---------------------------------------------------------------------------
const TYPE_VOID:       i64 =  0;
const TYPE_VOIDP:      i64 =  1;
const TYPE_CHAR:       i64 =  2;
const TYPE_UCHAR:      i64 = -2;
const TYPE_SHORT:      i64 =  3;
const TYPE_USHORT:     i64 = -3;
const TYPE_INT:        i64 =  4;
const TYPE_UINT:       i64 = -4;
const TYPE_LONG:       i64 =  5;
const TYPE_ULONG:      i64 = -5;
const TYPE_LONG_LONG:  i64 =  6;
const TYPE_ULONG_LONG: i64 = -6;
const TYPE_FLOAT:      i64 =  7;
const TYPE_DOUBLE:     i64 =  8;
// CRuby's Fiddle (1.1) doesn't expose TYPE_BOOL; we keep it for FFI's
// `:bool` arg and treat it like a 32-bit int, matching libffi's `bool` ABI
// on x86-64. Picked an unused value (9) to avoid clashes.
const TYPE_BOOL:       i64 =  9;

// ---------------------------------------------------------------------------
// Argument storage (keeps values alive while libffi call is running)
//
// libffi::middle::Arg is just *mut c_void with no lifetime.
// We must keep CArg values alive (pinned in a Vec) for the duration of the
// call so the raw pointers stored inside Arg remain valid.
// ---------------------------------------------------------------------------

enum CArg {
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    F32(f32),
    F64(f64),
}

impl CArg {
    /// Return a libffi Arg pointing into this CArg.
    /// SAFETY: `self` must not be moved or dropped while the Arg is in use.
    fn as_libffi_arg(&'_ self) -> Arg<'_> {
        match self {
            CArg::I8(v) => Arg::new(v),
            CArg::U8(v) => Arg::new(v),
            CArg::I16(v) => Arg::new(v),
            CArg::U16(v) => Arg::new(v),
            CArg::I32(v) => Arg::new(v),
            CArg::U32(v) => Arg::new(v),
            CArg::I64(v) => Arg::new(v),
            CArg::U64(v) => Arg::new(v),
            CArg::F32(v) => Arg::new(v),
            CArg::F64(v) => Arg::new(v),
        }
    }

    fn to_ffi_type(&self) -> Type {
        match self {
            CArg::I8(_) => Type::i8(),
            CArg::U8(_) => Type::u8(),
            CArg::I16(_) => Type::i16(),
            CArg::U16(_) => Type::u16(),
            CArg::I32(_) => Type::i32(),
            CArg::U32(_) => Type::u32(),
            CArg::I64(_) => Type::i64(),
            CArg::U64(_) => Type::u64(),
            CArg::F32(_) => Type::f32(),
            CArg::F64(_) => Type::f64(),
        }
    }
}

// ---------------------------------------------------------------------------
// Type-code helpers
// ---------------------------------------------------------------------------

fn type_code_to_ret_ffi_type(ty: i64) -> Result<Type> {
    // INTPTR_T / PTRDIFF_T / SSIZE_T alias TYPE_LONG (positive 5);
    // UINTPTR_T / SIZE_T alias TYPE_ULONG (negative -5). Those alias paths
    // hit the LONG/ULONG arms automatically without separate constants.
    match ty {
        TYPE_VOID => Ok(Type::void()),
        TYPE_VOIDP => Ok(Type::pointer()),
        TYPE_CHAR => Ok(Type::i8()),
        TYPE_UCHAR => Ok(Type::u8()),
        TYPE_SHORT => Ok(Type::i16()),
        TYPE_USHORT => Ok(Type::u16()),
        TYPE_INT | TYPE_BOOL => Ok(Type::i32()),
        TYPE_UINT => Ok(Type::u32()),
        TYPE_LONG | TYPE_LONG_LONG => Ok(Type::i64()),
        TYPE_ULONG | TYPE_ULONG_LONG => Ok(Type::u64()),
        TYPE_FLOAT => Ok(Type::f32()),
        TYPE_DOUBLE => Ok(Type::f64()),
        _ => Err(MonorubyErr::runtimeerr(format!(
            "Fiddle: unsupported return type code {}",
            ty
        ))),
    }
}

/// Coerce a Ruby Integer argument to the `i64` a C integer parameter — or a
/// typed `___write` — is built from.
///
/// monoruby's Fixnum is an i63, so an `Integer` anywhere in
/// `[2^62, 2^64)` — `sqlite3_bind_int64(…, 2**62)`, or any `unsigned long`
/// with its top bits set — arrives as a BigInt even though it fits the C
/// type exactly. `expect_integer` rejects those, which made perfectly
/// in-range values fail with "no implicit conversion of Integer into
/// Integer". Accept both representations and let the caller's `as` cast
/// narrow to the declared width, matching C's own conversion rules.
///
/// `___write` needs exactly the same latitude — `ptr.write_uint64(2**63)`
/// stores a word the C type holds exactly — so it converts through here too.
fn integer_arg_to_i64(globals: &Globals, val: Value) -> Result<i64> {
    match val.unpack() {
        RV::Fixnum(i) => Ok(i),
        RV::BigInt(b) => num::ToPrimitive::to_i64(b)
            .or_else(|| num::ToPrimitive::to_u64(b).map(|u| u as i64))
            .ok_or_else(|| {
                MonorubyErr::rangeerr("bignum too big to convert into a C integer")
            }),
        _ => Err(MonorubyErr::no_implicit_conversion(
            globals,
            val,
            INTEGER_CLASS,
        )),
    }
}

/// Convert a Ruby Value and a Fiddle type code into a `CArg`.
///
/// For `TYPE_VOIDP`, a Ruby String value is accepted: a pointer to its
/// actual byte buffer is passed to the C function. We mutate the String
/// to ensure a trailing NUL in spare capacity (so `strlen`-style reads
/// stop at `len`) but the visible content is unchanged. Writes the C
/// function makes within `[0, len)` are visible to subsequent Ruby reads
/// — this is required for callers like `glGenTextures(1, buf)` and
/// `memcpy(buf, src, n)` that fill `buf` in place.
///
/// `mut val` is taken by value (Value is Copy) so we can freely take an
/// `&mut` to its underlying RValue without disturbing the caller.
fn value_to_carg(globals: &mut Globals, mut val: Value, ty: i64) -> Result<CArg> {
    // Same alias note as type_code_to_ret_ffi_type: INTPTR_T family
    // collapses to LONG/ULONG codes in CRuby's convention.
    match ty {
        TYPE_VOID => Ok(CArg::I64(0)), // should not appear as argument
        TYPE_CHAR => Ok(CArg::I8(integer_arg_to_i64(globals, val)? as i8)),
        TYPE_UCHAR => Ok(CArg::U8(integer_arg_to_i64(globals, val)? as u8)),
        TYPE_SHORT => Ok(CArg::I16(integer_arg_to_i64(globals, val)? as i16)),
        TYPE_USHORT => Ok(CArg::U16(integer_arg_to_i64(globals, val)? as u16)),
        TYPE_INT | TYPE_BOOL => Ok(CArg::I32(integer_arg_to_i64(globals, val)? as i32)),
        TYPE_UINT => Ok(CArg::U32(integer_arg_to_i64(globals, val)? as u32)),
        TYPE_LONG | TYPE_LONG_LONG => Ok(CArg::I64(integer_arg_to_i64(globals, val)?)),
        TYPE_ULONG | TYPE_ULONG_LONG => Ok(CArg::U64(integer_arg_to_i64(globals, val)? as u64)),
        TYPE_VOIDP => {
            // Accept: nil → NULL, Integer → address, String → raw bytes ptr
            match val.unpack() {
                RV::Fixnum(i) => Ok(CArg::U64(i as u64)),
                RV::BigInt(b) => Ok(CArg::U64(num::ToPrimitive::to_u64(b).unwrap_or(0))),
                RV::Nil => Ok(CArg::U64(0)),
                RV::String(_) => {
                    // Hand the C function the actual String buffer (so
                    // it can write back into Ruby-visible memory) but
                    // first ensure a trailing NUL in spare capacity
                    // (read-only callers like strlen need it). The
                    // String stays alive via the args slice in
                    // fiddle_call_inner, so the pointer is valid for
                    // the duration of the call.
                    let inner = val.as_rstring_inner_mut();
                    let ptr = inner.nul_terminated_buf_ptr();
                    Ok(CArg::U64(ptr as u64))
                }
                _ => {
                    // Other objects (e.g. FFI::Pointer): coerce via to_i
                    let addr = val.expect_integer(globals)?;
                    Ok(CArg::U64(addr as u64))
                }
            }
        }
        TYPE_FLOAT => {
            let f = val.coerce_to_f64_no_convert(globals)? as f32;
            Ok(CArg::F32(f))
        }
        TYPE_DOUBLE => {
            let f = val.coerce_to_f64_no_convert(globals)?;
            Ok(CArg::F64(f))
        }
        _ => Err(MonorubyErr::runtimeerr(format!(
            "Fiddle: unsupported argument type code {}",
            ty
        ))),
    }
}

// ---------------------------------------------------------------------------
// Core libffi call
// ---------------------------------------------------------------------------

fn fiddle_call_inner(
    globals: &mut Globals,
    ptr: usize,
    args: &[Value],
    arg_type_codes: &[i64],
    ret_type_code: i64,
) -> Result<Value> {
    if args.len() != arg_type_codes.len() {
        return Err(MonorubyErr::argumenterr(format!(
            "Fiddle: args length ({}) != arg_types length ({})",
            args.len(),
            arg_type_codes.len()
        )));
    }

    // Build C-typed argument storage
    let c_args: Vec<CArg> = args
        .iter()
        .zip(arg_type_codes.iter())
        .map(|(&val, &ty)| value_to_carg(globals, val, ty))
        .collect::<Result<_>>()?;

    // libffi argument types derived from c_args (preserves exact widths)
    let ffi_arg_types: Vec<Type> = c_args.iter().map(|ca| ca.to_ffi_type()).collect();

    let ffi_ret_type = type_code_to_ret_ffi_type(ret_type_code)?;
    let cif = Cif::new(ffi_arg_types.into_iter(), ffi_ret_type);

    // Create Arg raw pointers into c_args.
    // SAFETY: c_args is not moved or dropped until after cif.call() returns.
    let ffi_args: Vec<Arg> = c_args.iter().map(|ca| ca.as_libffi_arg()).collect();

    let func = CodePtr(ptr as *mut c_void);

    // SAFETY: `c_args` outlives the call below, and `cif` was built from the
    // same type codes the arguments were marshalled with.
    // `___call` has no descriptor, so no string-return folding.
    let result = unsafe { call_with_cif(&cif, func, &ffi_args, ret_type_code, false)? };

    // Keep c_args alive until here
    drop(ffi_args);
    drop(c_args);

    Ok(result)
}

/// Perform the libffi call and return the raw result, normalised to 64 bits.
///
/// Kept separate from boxing so the same call can happen on a worker thread,
/// which must not touch the Ruby heap: the worker produces these bits, and
/// the interpreter thread turns them into a Value with [`bits_to_value`].
/// Floating-point returns travel as `f64::to_bits`, everything else as the
/// value widened to `i64` exactly the way its arm boxed it before.
///
/// SAFETY: the caller must keep the `CArg` storage that `ffi_args` points into
/// alive until this returns, and `cif` must have been built from the same type
/// codes the arguments were marshalled with.
unsafe fn call_raw(cif: &Cif, func: CodePtr, ffi_args: &[Arg], ret_type_code: i64) -> Result<i64> {
    Ok(match ret_type_code {
        // For void, call with i64 return and discard.
        TYPE_VOID => {
            let _: i64 = unsafe { cif.call(func, &ffi_args) };
            0
        }
        TYPE_CHAR => (unsafe { cif.call::<i8>(func, &ffi_args) }) as i64,
        TYPE_UCHAR => (unsafe { cif.call::<u8>(func, &ffi_args) }) as i64,
        TYPE_SHORT => (unsafe { cif.call::<i16>(func, &ffi_args) }) as i64,
        TYPE_USHORT => (unsafe { cif.call::<u16>(func, &ffi_args) }) as i64,
        TYPE_INT | TYPE_BOOL => (unsafe { cif.call::<i32>(func, &ffi_args) }) as i64,
        TYPE_UINT => (unsafe { cif.call::<u32>(func, &ffi_args) }) as i64,
        TYPE_LONG | TYPE_LONG_LONG => unsafe { cif.call::<i64>(func, &ffi_args) },
        TYPE_VOIDP | TYPE_ULONG | TYPE_ULONG_LONG => {
            unsafe { cif.call::<u64>(func, &ffi_args) as i64 }
        }
        // Widened to f64 here so the bit pattern is the one `Value::float`
        // would have received either way.
        TYPE_FLOAT => ((unsafe { cif.call::<f32>(func, &ffi_args) }) as f64).to_bits() as i64,
        TYPE_DOUBLE => (unsafe { cif.call::<f64>(func, &ffi_args) }).to_bits() as i64,
        _ => {
            return Err(MonorubyErr::runtimeerr(format!(
                "Fiddle: unsupported return type code {}",
                ret_type_code
            )));
        }
    })
}

/// Box a raw result from [`call_raw`] as the Ruby value its type code means.
///
/// `ret_as_string` reads a pointer return back as a String; see the field of
/// the same name on [`PreparedFn`].
///
/// SAFETY: with `ret_as_string`, `bits` must be a pointer that is either NULL
/// or a live NUL-terminated C string.
unsafe fn bits_to_value(bits: i64, ret_type_code: i64, ret_as_string: bool) -> Value {
    if ret_as_string {
        return unsafe { cstr_to_value(bits as u64) };
    }
    match ret_type_code {
        TYPE_VOID => Value::nil(),
        TYPE_FLOAT | TYPE_DOUBLE => Value::float(f64::from_bits(bits as u64)),
        _ => Value::integer(bits),
    }
}

/// Call and box in one step, for the ordinary inline path.
///
/// SAFETY: as [`call_raw`].
unsafe fn call_with_cif(
    cif: &Cif,
    func: CodePtr,
    ffi_args: &[Arg],
    ret_type_code: i64,
    ret_as_string: bool,
) -> Result<Value> {
    let bits = unsafe { call_raw(cif, func, ffi_args, ret_type_code)? };
    Ok(unsafe { bits_to_value(bits, ret_type_code, ret_as_string) })
}

// ---------------------------------------------------------------------------
// Builtin functions exposed to Ruby as Fiddle module-level functions
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Prepared calls
//
// `Cif::new` runs `ffi_prep_cif` *and* a `libc::malloc` for the type array, so
// building the CIF per call dominates the cost of a small foreign call: the
// naive path measures ~108ns fixed plus ~108ns per argument, against a C body
// that is often a single load. A call site's signature never changes, so it is
// prepared once and only the argument marshalling is repeated.
//
// A prepared record is `Box::leak`ed and its address handed to Ruby as an
// Integer, the same way `___dlopen` hands back a raw handle. They are created
// at attach time and live as long as the symbol they wrap, so there is nothing
// to reclaim. Leaking also makes the record immortal and therefore safe to
// share between threads: `ffi_call` only reads the CIF, so concurrent calls
// through one record do not race.
// ---------------------------------------------------------------------------

struct PreparedFn {
    ptr: CodePtr,
    cif: Cif,
    arg_codes: Vec<i64>,
    ret_code: i64,
    /// Return the `char *` result as a Ruby String rather than an address.
    ///
    /// A `:string` return is a pointer as far as libffi is concerned, so
    /// without this the Ruby layer has to follow every call with a second
    /// `___read_string` — two builtin round trips where one would do, on
    /// exactly the calls that are already the most common in a real binding
    /// (`sqlite3_column_text`, `sqlite3_column_name`, `sqlite3_errmsg`).
    ret_as_string: bool,
    /// Run this call on a worker thread instead of inline.
    ///
    /// A C function that blocks in the kernel would otherwise freeze the
    /// whole interpreter: green threads share one OS thread, so nothing else
    /// runs until it returns. Declaring the call blocking sends it to
    /// `native_pool` and parks the calling green thread on the completion
    /// pipe, exactly like `File#flock`.
    blocking: bool,
}

/// Flags accepted as `___prepare`'s fourth argument.
///
/// A bitmask rather than a run of booleans: these are call-site properties a
/// binding knows at attach time, and there will be more of them.
/// Kept in sync with `gem/ffi_c.rb` and `gem/sqlite3/sqlite3_native.rb`.
const PREPARE_RETURN_STRING: i64 = 1;
const PREPARE_BLOCKING: i64 = 2;
const PREPARE_KNOWN_FLAGS: i64 = PREPARE_RETURN_STRING | PREPARE_BLOCKING;

/// A prepared call packaged for a worker thread.
///
/// Holds only C-level data: the address of the leaked descriptor and the
/// already-marshalled arguments. The libffi `Arg` pointer array is built on
/// the worker, pointing into that thread's own copy of `args`.
pub(crate) struct FfiWorkerCall {
    site: *const PreparedFn,
    args: SmallVec<[CArg; PREPARED_INLINE_ARGS]>,
}

// SAFETY: `site` addresses a `Box::leak`ed `PreparedFn`, so it outlives every
// thread, and `ffi_call` only reads the CIF — concurrent calls through one
// descriptor do not race. `args` is plain C data with no Ruby heap
// references. A `TYPE_VOIDP` argument may carry a raw pointer into a Ruby
// String's buffer, which stays valid because the allocator does not move
// objects and the parked caller's frame keeps the String reachable for the
// GC (`Executor::mark` walks the whole cfp chain).
unsafe impl Send for FfiWorkerCall {}

impl FfiWorkerCall {
    /// Perform the call. Runs on a worker thread — must not touch the Ruby
    /// heap or any interpreter thread-local.
    pub(crate) fn run(&self) -> i64 {
        // SAFETY: see the `Send` justification above.
        let pf: &PreparedFn = unsafe { &*self.site };
        let ffi_args: SmallVec<[Arg; PREPARED_INLINE_ARGS]> =
            self.args.iter().map(|ca| ca.as_libffi_arg()).collect();
        // SAFETY: `self.args` outlives `ffi_args`, and the CIF was built from
        // the codes those arguments were marshalled with. The return code was
        // validated by `___prepare`, so `call_raw` cannot take its error arm.
        unsafe { call_raw(&pf.cif, pf.ptr, &ffi_args, pf.ret_code).unwrap_or(0) }
    }
}

/// Build a Ruby String from a C `char *`, mapping NULL to nil.
///
/// SAFETY: `ptr` must be either 0 or a NUL-terminated C string that stays
/// valid for the duration of the copy.
unsafe fn cstr_to_value(ptr: u64) -> Value {
    if ptr == 0 {
        return Value::nil();
    }
    let s = unsafe { std::ffi::CStr::from_ptr(ptr as *const libc::c_char) };
    Value::string_from_vec(s.to_bytes().to_vec())
}

/// Largest argument count marshalled without spilling to the heap.
const PREPARED_INLINE_ARGS: usize = 8;

/// The libffi `Type` a value of type code `ty` is passed as.
///
/// This must agree with `value_to_carg`, which decides the `CArg` width: the
/// CIF is built once from the codes, so a disagreement would hand `ffi_call` a
/// value of a different width than the CIF promises.
fn type_code_to_arg_ffi_type(ty: i64) -> Result<Type> {
    Ok(match ty {
        TYPE_CHAR => Type::i8(),
        TYPE_UCHAR => Type::u8(),
        TYPE_SHORT => Type::i16(),
        TYPE_USHORT => Type::u16(),
        TYPE_INT | TYPE_BOOL => Type::i32(),
        TYPE_UINT => Type::u32(),
        TYPE_LONG | TYPE_LONG_LONG => Type::i64(),
        TYPE_VOIDP | TYPE_ULONG | TYPE_ULONG_LONG => Type::u64(),
        TYPE_FLOAT => Type::f32(),
        TYPE_DOUBLE => Type::f64(),
        _ => {
            return Err(MonorubyErr::runtimeerr(format!(
                "Fiddle: unsupported argument type code {}",
                ty
            )));
        }
    })
}

/// ### Fiddle.___prepare(ptr, arg_types, ret_type) -> Integer
///
/// Build a reusable call descriptor for the function at `ptr` and return an
/// opaque id to pass to `___invoke`. Validates both the argument and the
/// return type codes up front, so `___invoke` never has to.
#[monoruby_builtin]
fn fiddle_prepare(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let ptr = lfp.arg(0).expect_integer(globals)? as usize;
    // There is no legitimate call to address 0, and `___dlsym` hands back 0
    // for a symbol it could not resolve. Catching it here turns a segfault at
    // the first call into an error at attach time, where the symbol name is
    // still known.
    if ptr == 0 {
        return Err(MonorubyErr::argumenterr(
            "Fiddle: cannot prepare a call to a NULL function pointer",
        ));
    }
    let types_ary = lfp.arg(1).expect_array_ty(globals)?;
    let ret_code = lfp.arg(2).expect_integer(globals)?;
    let flags = match lfp.try_arg(3) {
        Some(v) => v.expect_integer(globals)?,
        None => 0,
    };
    if flags & !PREPARE_KNOWN_FLAGS != 0 {
        return Err(MonorubyErr::argumenterr(format!(
            "Fiddle: unknown prepare flags {}",
            flags & !PREPARE_KNOWN_FLAGS
        )));
    }
    let ret_as_string = flags & PREPARE_RETURN_STRING != 0;
    let blocking = flags & PREPARE_BLOCKING != 0;
    // Only a pointer return can be read back as a string, and both facades
    // spell `:string` as VOIDP. Refusing anything else here keeps `___invoke`
    // from having to decide what a "string" of some other width would mean.
    if ret_as_string && ret_code != TYPE_VOIDP {
        return Err(MonorubyErr::argumenterr(
            "Fiddle: a string return requires the VOIDP return type code",
        ));
    }

    let arg_codes: Vec<i64> = types_ary
        .iter()
        .map(|v| v.expect_integer(globals))
        .collect::<Result<_>>()?;

    // `___invoke` passes arguments in fixed frame slots, so a signature wider
    // than that cannot be served. Reject it here rather than at call time: a
    // successful prepare then *guarantees* `___invoke` works, which is what
    // lets the Ruby facades treat a raised prepare as "fall back to ___call".
    if arg_codes.len() > PREPARED_INLINE_ARGS {
        return Err(MonorubyErr::argumenterr(format!(
            "Fiddle: cannot prepare a call with more than {} arguments (got {})",
            PREPARED_INLINE_ARGS,
            arg_codes.len()
        )));
    }

    let arg_types: Vec<Type> = arg_codes
        .iter()
        .map(|&ty| type_code_to_arg_ffi_type(ty))
        .collect::<Result<_>>()?;
    // Rejects an unsupported return code here rather than per call.
    let ret_type = type_code_to_ret_ffi_type(ret_code)?;

    let prepared = Box::new(PreparedFn {
        ptr: CodePtr(ptr as *mut c_void),
        cif: Cif::new(arg_types.into_iter(), ret_type),
        arg_codes,
        ret_code,
        ret_as_string,
        blocking,
    });

    Ok(Value::integer(Box::leak(prepared) as *mut PreparedFn as i64))
}

/// ### Fiddle.___invoke(id, *args) -> Object
///
/// Call a function prepared by `___prepare`. Arguments are read straight out
/// of the frame, so a call allocates nothing on the Ruby side and — up to
/// `PREPARED_INLINE_ARGS` — nothing on the Rust side either.
#[monoruby_builtin]
fn fiddle_invoke(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let id = lfp.arg(0).expect_integer(globals)?;
    // SAFETY: `id` is the address of a leaked PreparedFn handed out by
    // `___prepare`, so the record is live for the rest of the process. The
    // primitives are private to monoruby's own Ruby layer (`___`-prefixed and
    // undocumented), which only ever passes back an id it was given.
    let pf: &PreparedFn = unsafe { &*(id as *const PreparedFn) };

    let n = pf.arg_codes.len();
    // Trailing slots are optional, so an unsupplied one reads back as None
    // rather than shortening the frame.
    if n < PREPARED_INLINE_ARGS && lfp.try_arg(n + 1).is_some() {
        return Err(MonorubyErr::argumenterr(format!(
            "Fiddle: too many arguments (expected {})",
            n
        )));
    }

    let mut c_args: SmallVec<[CArg; PREPARED_INLINE_ARGS]> = SmallVec::with_capacity(n);
    for (i, &ty) in pf.arg_codes.iter().enumerate() {
        let Some(v) = lfp.try_arg(i + 1) else {
            return Err(MonorubyErr::argumenterr(format!(
                "Fiddle: wrong number of arguments (given {}, expected {})",
                i, n
            )));
        };
        c_args.push(value_to_carg(globals, v, ty)?);
    }
    // A call the binding declared blocking runs on a worker thread while this
    // green thread parks, so one slow C call does not freeze every other
    // thread on this interpreter. The round trip costs tens of microseconds
    // (see `native_pool`), which is why it is opt-in per function rather than
    // the default.
    if pf.blocking {
        let bits = native_pool::run_blocking(
            vm,
            globals,
            native_pool::NativeOp::Ffi(FfiWorkerCall {
                site: pf as *const PreparedFn,
                args: c_args,
            }),
        )?
        .ret;
        // SAFETY: the worker has returned, so the pointer a string return
        // yields is whatever the C function produced — the same contract the
        // inline path relies on.
        return Ok(unsafe { bits_to_value(bits, pf.ret_code, pf.ret_as_string) });
    }

    let ffi_args: SmallVec<[Arg; PREPARED_INLINE_ARGS]> =
        c_args.iter().map(|ca| ca.as_libffi_arg()).collect();

    // SAFETY: `c_args` is still alive, and the CIF was built from the same
    // type codes these arguments were marshalled with.
    let result = unsafe { call_with_cif(&pf.cif, pf.ptr, &ffi_args, pf.ret_code, pf.ret_as_string)? };

    // Keep the CArg storage alive until the call has returned.
    drop(ffi_args);
    drop(c_args);

    Ok(result)
}

/// ### Fiddle.___call(ptr, args, arg_types, ret_type) -> Object
///
/// Low-level foreign-function call using libffi.
/// - ptr       : Integer  – function address from dlsym
/// - args      : Array    – Ruby argument values
/// - arg_types : Array    – Fiddle type-code integers for each argument
/// - ret_type  : Integer  – Fiddle type-code for the return value
///
/// Builds the CIF on every call. Prefer `___prepare` + `___invoke` on a hot
/// path; this entry point remains for call sites whose signature is not known
/// until the call itself.
#[monoruby_builtin]
fn fiddle_call(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let ptr = lfp.arg(0).expect_integer(globals)? as usize;
    let args_ary = lfp.arg(1).expect_array_ty(globals)?;
    let types_ary = lfp.arg(2).expect_array_ty(globals)?;
    let ret_type = lfp.arg(3).expect_integer(globals)?;

    let args: Vec<Value> = args_ary.iter().copied().collect();
    let arg_types: Vec<i64> = types_ary
        .iter()
        .map(|v| v.expect_integer(globals))
        .collect::<Result<_>>()?;

    fiddle_call_inner(globals, ptr, &args, &arg_types, ret_type)
}

/// ### Fiddle.___read(ptr, type_code) -> Integer | Float
///
/// Read a single typed value from memory at `ptr`.
#[monoruby_builtin]
fn fiddle_read(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let ptr = integer_arg_to_i64(globals, lfp.arg(0))? as usize;
    let ty = lfp.arg(1).expect_integer(globals)?;
    if ptr == 0 {
        return Err(MonorubyErr::runtimeerr("Fiddle.___read: NULL pointer"));
    }
    let result = unsafe {
        match ty {
            TYPE_CHAR => Value::integer(*(ptr as *const i8) as i64),
            TYPE_UCHAR => Value::integer(*(ptr as *const u8) as i64),
            TYPE_SHORT => Value::integer(*(ptr as *const i16) as i64),
            TYPE_USHORT => Value::integer(*(ptr as *const u16) as i64),
            TYPE_INT | TYPE_BOOL => Value::integer(*(ptr as *const i32) as i64),
            TYPE_UINT => Value::integer(*(ptr as *const u32) as i64),
            TYPE_LONG | TYPE_LONG_LONG => {
                Value::integer(*(ptr as *const i64))
            }
            TYPE_VOIDP | TYPE_ULONG | TYPE_ULONG_LONG => {
                // Reinterpreting as i64 would report every address / unsigned
                // value with bit 63 set as a negative Integer.
                Value::integer_from_u64(*(ptr as *const u64))
            }
            TYPE_FLOAT => Value::float(*(ptr as *const f32) as f64),
            TYPE_DOUBLE => Value::float(*(ptr as *const f64)),
            _ => {
                return Err(MonorubyErr::runtimeerr(format!(
                    "Fiddle.___read: unsupported type code {}",
                    ty
                )));
            }
        }
    };
    Ok(result)
}

/// ### Fiddle.___write(ptr, type_code, value) -> Integer
///
/// Write a single typed value to memory at `ptr`.
/// Returns `ptr` so callers can chain.
#[monoruby_builtin]
fn fiddle_write(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let ptr = integer_arg_to_i64(globals, lfp.arg(0))? as usize;
    let ty = lfp.arg(1).expect_integer(globals)?;
    let val = lfp.arg(2);
    if ptr == 0 {
        return Err(MonorubyErr::runtimeerr("Fiddle.___write: NULL pointer"));
    }
    unsafe {
        match ty {
            TYPE_CHAR => *(ptr as *mut i8) = integer_arg_to_i64(globals, val)? as i8,
            TYPE_UCHAR => *(ptr as *mut u8) = integer_arg_to_i64(globals, val)? as u8,
            TYPE_SHORT => *(ptr as *mut i16) = integer_arg_to_i64(globals, val)? as i16,
            TYPE_USHORT => *(ptr as *mut u16) = integer_arg_to_i64(globals, val)? as u16,
            TYPE_INT | TYPE_BOOL => *(ptr as *mut i32) = integer_arg_to_i64(globals, val)? as i32,
            TYPE_UINT => *(ptr as *mut u32) = integer_arg_to_i64(globals, val)? as u32,
            TYPE_LONG | TYPE_LONG_LONG => {
                *(ptr as *mut i64) = integer_arg_to_i64(globals, val)?;
            }
            TYPE_VOIDP | TYPE_ULONG | TYPE_ULONG_LONG => {
                *(ptr as *mut u64) = integer_arg_to_i64(globals, val)? as u64;
            }
            TYPE_FLOAT => *(ptr as *mut f32) = val.coerce_to_f64_no_convert(globals)? as f32,
            TYPE_DOUBLE => *(ptr as *mut f64) = val.coerce_to_f64_no_convert(globals)?,
            _ => {
                return Err(MonorubyErr::runtimeerr(format!(
                    "Fiddle.___write: unsupported type code {}",
                    ty
                )));
            }
        }
    }
    Ok(lfp.arg(0)) // return ptr
}

// ---------------------------------------------------------------------------
// Inline JIT specializations for ___read / ___write
//
// When the type code is a constant Fixnum at compile time, the JIT can emit a
// direct typed load/store against the memory pointed to by `ptr`, skipping the
// type-code dispatch and the libffi-free Rust path. NULL pointers deopt to the
// interpreter so the regular builtin raises the runtime error.
//
// Widths up to 32 bits always fit monoruby's i63 Fixnum, so those need no
// range check. The 64-bit types (LONG / LONG_LONG / ULONG / ULONG_LONG /
// VOIDP) do not: a value outside `[-2^62, 2^62)` — for unsigned, anything
// `>= 2^62` — has to become a Bignum. Rather than give up on inlining them,
// the emitted code takes the common in-range case and deopts to the builtin
// on the values that need boxing (which the builtin now handles: it accepts
// and produces Bignums for these types).
// ---------------------------------------------------------------------------

#[derive(Clone, Copy)]
enum ReadKind {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    F64,
}

fn fiddle_read_inline(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: Option<ClassId>,
    _: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() || callsite.pos_num != 2 {
        return false;
    }
    let CallSiteInfo { args, dst, .. } = *callsite;
    let Some(dst) = dst else {
        return false;
    };

    let Some(ty_lit) = state.is_fixnum_literal(args + 1usize) else {
        return false;
    };
    let kind = match ty_lit.get() {
        TYPE_CHAR => ReadKind::I8,
        TYPE_UCHAR => ReadKind::U8,
        TYPE_SHORT => ReadKind::I16,
        TYPE_USHORT => ReadKind::U16,
        TYPE_INT | TYPE_BOOL => ReadKind::I32,
        TYPE_UINT => ReadKind::U32,
        TYPE_LONG | TYPE_LONG_LONG => ReadKind::I64,
        TYPE_VOIDP | TYPE_ULONG | TYPE_ULONG_LONG => ReadKind::U64,
        TYPE_DOUBLE => ReadKind::F64,
        _ => return false,
    };

    state.load_fixnum(ir, args, GP::Rdi);
    let deopt = ir.new_deopt(state);

    match kind {
        ReadKind::F64 => {
            let fret = state.def_F(dst);
            ir.inline(move |r#gen, _, labels, base| {
                let d = r#gen.deopt_label(labels, deopt, DeoptCause::Value(GP::Rdi));
                r#gen.emit_fiddle_read_f64(fret, &d, base);
            });
        }
        _ => {
            // (byte width, signed) for the typed integer load.
            let (width, signed) = match kind {
                ReadKind::I8 => (1, true),
                ReadKind::U8 => (1, false),
                ReadKind::I16 => (2, true),
                ReadKind::U16 => (2, false),
                ReadKind::I32 => (4, true),
                ReadKind::U32 => (4, false),
                ReadKind::I64 => (8, true),
                ReadKind::U64 => (8, false),
                ReadKind::F64 => unreachable!(),
            };
            ir.inline(move |r#gen, _, labels, _| {
                let d = r#gen.deopt_label(labels, deopt, DeoptCause::Value(GP::Rdi));
                r#gen.emit_fiddle_read_int(width, signed, &d);
            });
            state.def_reg2acc_fixnum(ir, GP::Rax, dst);
        }
    }
    true
}

#[derive(Clone, Copy)]
enum WriteKind {
    Int8,
    Int16,
    Int32,
    Int64,
    F64,
}

fn fiddle_write_inline(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: Option<ClassId>,
    _: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() || callsite.pos_num != 3 {
        return false;
    }
    let CallSiteInfo { args, dst, .. } = *callsite;

    let Some(ty_lit) = state.is_fixnum_literal(args + 1usize) else {
        return false;
    };
    let kind = match ty_lit.get() {
        TYPE_CHAR | TYPE_UCHAR => WriteKind::Int8,
        TYPE_SHORT | TYPE_USHORT => WriteKind::Int16,
        TYPE_INT | TYPE_UINT | TYPE_BOOL => WriteKind::Int32,
        TYPE_VOIDP | TYPE_LONG | TYPE_ULONG | TYPE_LONG_LONG | TYPE_ULONG_LONG => {
            WriteKind::Int64
        }
        TYPE_DOUBLE => WriteKind::F64,
        _ => return false,
    };

    let val_slot = args + 2usize;
    state.load_fixnum(ir, args, GP::Rdi);

    match kind {
        WriteKind::F64 => {
            let xsrc = state.load_fpr(ir, val_slot);
            let deopt = ir.new_deopt(state);
            ir.inline(move |r#gen, _, labels, base| {
                let d = r#gen.deopt_label(labels, deopt, DeoptCause::Value(GP::Rdi));
                r#gen.emit_fiddle_write_f64(xsrc, &d, base);
            });
        }
        _ => {
            state.load_fixnum(ir, val_slot, GP::Rsi);
            let deopt = ir.new_deopt(state);
            let width = match kind {
                WriteKind::Int8 => 1,
                WriteKind::Int16 => 2,
                WriteKind::Int32 => 4,
                WriteKind::Int64 => 8,
                WriteKind::F64 => unreachable!(),
            };
            ir.inline(move |r#gen, _, labels, _| {
                let d = r#gen.deopt_label(labels, deopt, DeoptCause::Value(GP::Rdi));
                r#gen.emit_fiddle_write_int(width, &d);
            });
        }
    }

    state.def_reg2acc_fixnum(ir, GP::Rax, dst);
    true
}

/// ### Fiddle.___read_string(ptr) -> String | nil
///
/// Read a null-terminated C string from `ptr`.
/// Returns nil if `ptr` is 0 (NULL).
#[monoruby_builtin]
fn fiddle_read_string(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let ptr = lfp.arg(0).expect_integer(globals)? as u64;
    // SAFETY: the caller passes an address it obtained from C and vouches
    // that it points at a live NUL-terminated string.
    Ok(unsafe { cstr_to_value(ptr) })
}

/// ### Fiddle.___read_bytes(ptr, len) -> String
///
/// Read exactly `len` bytes from `ptr` into a binary Ruby String.
#[monoruby_builtin]
fn fiddle_read_bytes(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let ptr = lfp.arg(0).expect_integer(globals)? as *const u8;
    let len = lfp.arg(1).expect_integer(globals)? as usize;
    if ptr.is_null() {
        return Err(MonorubyErr::runtimeerr(
            "Fiddle.___read_bytes: NULL pointer",
        ));
    }
    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
    Ok(Value::bytes_from_slice(slice))
}

/// ### Fiddle.___write_bytes(ptr, bytes) -> Integer
///
/// Copy a Ruby String's raw bytes to `ptr`.  Returns `ptr`.
#[monoruby_builtin]
fn fiddle_write_bytes(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let ptr = lfp.arg(0).expect_integer(globals)? as *mut u8;
    let bytes_val = lfp.arg(1);
    let src = bytes_val.as_rstring_inner();
    if ptr.is_null() {
        return Err(MonorubyErr::runtimeerr(
            "Fiddle.___write_bytes: NULL pointer",
        ));
    }
    unsafe { std::ptr::copy_nonoverlapping(src.as_ptr(), ptr, src.len()) };
    Ok(lfp.arg(0))
}

/// ### Fiddle.___free(ptr)
///
/// Free heap memory allocated by Fiddle.malloc / Kernel.___malloc.
#[monoruby_builtin]
fn fiddle_free(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let ptr = lfp.arg(0).expect_integer(globals)? as *mut libc::c_void;
    if !ptr.is_null() {
        unsafe { libc::free(ptr) };
    }
    Ok(Value::nil())
}

// ---------------------------------------------------------------------------
// Module initialisation
// ---------------------------------------------------------------------------

pub(super) fn init(globals: &mut Globals) {
    let fiddle = globals.define_toplevel_module("Fiddle").id();

    // Low-level primitives used by stdlib/fiddle.rb and gem/ffi_c.rb.
    //
    // This is *the* shared native-call API of monoruby: `Fiddle` owns the
    // primitives, and every higher-level facade (Fiddle's own Ruby classes,
    // FFI in gem/ffi_c.rb, and the FFI-free bridges such as
    // gem/sqlite3/sqlite3_native.rb) is a thin Ruby layer on top of it.
    // Registering them once here also means the JIT inliners below
    // (`___read` / `___write`) benefit every caller rather than just Fiddle.
    globals.define_builtin_module_func(fiddle, "___call", fiddle_call, 4);
    // Prepared calls: build the CIF once, then invoke with the arguments
    // passed positionally (no Array, no per-call ffi_prep_cif).
    globals.define_builtin_module_func_with(fiddle, "___prepare", fiddle_prepare, 3, 4, false);
    globals.define_builtin_module_func_with(
        fiddle,
        "___invoke",
        fiddle_invoke,
        1,
        1 + PREPARED_INLINE_ARGS,
        false,
    );
    globals.define_builtin_module_inline_func(
        fiddle,
        "___read",
        fiddle_read,
        inline_gen2!(fiddle_read_inline),
        2,
    );
    globals.define_builtin_module_inline_func(
        fiddle,
        "___write",
        fiddle_write,
        inline_gen2!(fiddle_write_inline),
        3,
    );
    globals.define_builtin_module_func(fiddle, "___read_string", fiddle_read_string, 1);
    globals.define_builtin_module_func(fiddle, "___read_bytes", fiddle_read_bytes, 2);
    globals.define_builtin_module_func(fiddle, "___write_bytes", fiddle_write_bytes, 2);
    globals.define_builtin_module_func(fiddle, "___free", fiddle_free, 1);

    // dlopen / dlsym / malloc (implementations live in kernel.rs, but they
    // belong to the same primitive set, so expose them here too — a facade
    // then needs no namespace other than `Fiddle`).
    globals.define_builtin_module_func_with(fiddle, "___dlopen", kernel::dlopen, 1, 2, false);
    globals.define_builtin_module_func(fiddle, "___dlsym", kernel::dlsym, 2);
    globals.define_builtin_module_func_with(fiddle, "___malloc", kernel::malloc, 1, 2, false);

    // SIZEOF constants for x86-64 Linux
    for (name, size) in [
        ("SIZEOF_VOIDP", 8i64),
        ("SIZEOF_CHAR", 1),
        ("SIZEOF_SHORT", 2),
        ("SIZEOF_INT", 4),
        ("SIZEOF_LONG", 8),
        ("SIZEOF_LONG_LONG", 8),
        ("SIZEOF_FLOAT", 4),
        ("SIZEOF_DOUBLE", 8),
        ("SIZEOF_BOOL", 4),
        ("SIZEOF_INTPTR_T", 8),
        ("SIZEOF_UINTPTR_T", 8),
        ("SIZEOF_PTRDIFF_T", 8),
        ("SIZEOF_SIZE_T", 8),
        ("SIZEOF_SSIZE_T", 8),
    ] {
        globals.set_constant_by_str(fiddle, name, Value::integer(size));
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    // The harness's reference Ruby runs with `--disable-gem`, so the
    // ffi gem is unavailable to compare against. These tests instead
    // drive the shared low-level primitives registered in `init` above
    // (`Fiddle.___dlopen`/`___dlsym`/`___call`/`___read`/`___write`/…) and
    // embed their own `raise` assertions — a misbehaving primitive
    // surfaces as a monoruby-side exception and fails the test.
    //
    // Type codes match the constants defined at the top of this file
    // (kept in sync with `gem/ffi_c.rb` and `stdlib/fiddle.rb`).
    // CRuby's Fiddle convention: positive=signed, negation=unsigned.
    // SIZE_T is an alias of ULONG (= -LONG = -5) on x86-64.
    // Library names differ by platform: Linux ships glibc as `libc.so.6`
    // and libm as `libm.so.6`; macOS folds both into a single
    // `libSystem.B.dylib` (since 10.4) and has no separate libm. Pick
    // the right names at runtime via `RUBY_PLATFORM` so the same Ruby
    // fixture works on either OS.
    const TYPE_PRELUDE: &str = r#"
        TY_VOIDP  = 1
        TY_INT    = 4
        TY_LLONG  = 6
        TY_DOUBLE = 8
        TY_SIZE_T = -5
        __libc, __libm = case RUBY_PLATFORM
          when /darwin/ then ["/usr/lib/libSystem.B.dylib", "/usr/lib/libSystem.B.dylib"]
          else               ["libc.so.6", "libm.so.6"]
        end
        LIBC = Fiddle.___dlopen(__libc)
        LIBM = Fiddle.___dlopen(__libm) || Fiddle.___dlopen(__libc)
    "#;

    // Regression for https://github.com/sisshiki1969/monoruby/pull/337:
    // Ruby strings must be NUL-terminated before being handed to C via
    // a `TYPE_VOIDP` argument. Without the fix, `strlen` read past the
    // end of the string buffer and returned a length that depended on
    // whatever garbage followed in memory — the cause of the garbled
    // optcarrot window title.
    #[test]
    fn fiddle_string_is_nul_terminated() {
        run_test_no_result_check(&format!(
            r#"{TYPE_PRELUDE}
            strlen = Fiddle.___dlsym(LIBC, "strlen")
            call = ->(s) {{ Fiddle.___call(strlen, [s], [TY_VOIDP], TY_SIZE_T) }}
            raise unless call.call("")               == 0
            raise unless call.call("hello")          == 5
            raise unless call.call("hello_optcarrot") == 15
            # Short strings placed back-to-back used to leak neighbouring
            # bytes through strlen before the fix; loop many times so at
            # least one iteration trips any non-zero byte left over in
            # the small-string inline buffer.
            50.times do |i|
              s = "t_#{{i}}"
              actual = call.call(s)
              raise "strlen=#{{actual}} bytesize=#{{s.bytesize}}" unless actual == s.bytesize
            end
            :ok
            "#
        ));
    }

    // Two `:string` arguments forwarded in one call — exercises the
    // CArg storage path that keeps multiple NUL-terminated buffers
    // alive at once.
    #[test]
    fn fiddle_two_string_args() {
        run_test_no_result_check(&format!(
            r#"{TYPE_PRELUDE}
            strcmp = Fiddle.___dlsym(LIBC, "strcmp")
            call = ->(a, b) {{ Fiddle.___call(strcmp, [a, b], [TY_VOIDP, TY_VOIDP], TY_INT) }}
            raise unless call.call("abc", "abc") == 0
            raise unless call.call("abc", "abd")  < 0
            raise unless call.call("abd", "abc")  > 0
            20.times do |i|
              raise unless call.call("k_#{{i}}", "k_#{{i}}") == 0
            end
            :ok
            "#
        ));
    }

    // Integer args/returns of varying widths.
    #[test]
    fn fiddle_integer_roundtrip() {
        run_test_no_result_check(&format!(
            r#"{TYPE_PRELUDE}
            abs   = Fiddle.___dlsym(LIBC, "abs")
            llabs = Fiddle.___dlsym(LIBC, "llabs")
            raise unless Fiddle.___call(abs,   [-42], [TY_INT],   TY_INT)   == 42
            raise unless Fiddle.___call(abs,   [ 42], [TY_INT],   TY_INT)   == 42
            raise unless Fiddle.___call(llabs, [-1_000_000_000_000], [TY_LLONG], TY_LLONG) == 1_000_000_000_000
            raise unless Fiddle.___call(llabs, [ 1_000_000_000_000], [TY_LLONG], TY_LLONG) == 1_000_000_000_000
            :ok
            "#
        ));
    }

    // Double args/returns via libm's `sqrt` and `floor`.
    #[test]
    fn fiddle_double_args() {
        run_test_no_result_check(&format!(
            r#"{TYPE_PRELUDE}
            sqrt  = Fiddle.___dlsym(LIBM, "sqrt")
            floor = Fiddle.___dlsym(LIBM, "floor")
            raise unless Fiddle.___call(sqrt,  [0.0], [TY_DOUBLE], TY_DOUBLE) == 0.0
            raise unless (Fiddle.___call(sqrt, [2.0], [TY_DOUBLE], TY_DOUBLE) - Math.sqrt(2.0)).abs < 1e-12
            raise unless Fiddle.___call(floor, [3.7],  [TY_DOUBLE], TY_DOUBLE) == 3.0
            raise unless Fiddle.___call(floor, [-0.5], [TY_DOUBLE], TY_DOUBLE) == -1.0
            :ok
            "#
        ));
    }

    // Typed memory read/write at a heap address via ___malloc / ___write
    // / ___read / ___free — exercises the numeric argument path for
    // int and double without involving the ffi gem.
    #[test]
    fn fiddle_read_write_roundtrip() {
        run_test_no_result_check(&format!(
            r#"{TYPE_PRELUDE}
            ptr = Fiddle.___malloc(16)
            raise "malloc returned NULL" if ptr == 0
            begin
              Fiddle.___write(ptr, TY_INT, 0x41424344)
              raise unless Fiddle.___read(ptr, TY_INT) == 0x41424344
              Fiddle.___write(ptr, TY_DOUBLE, 3.14)
              raise unless Fiddle.___read(ptr, TY_DOUBLE) == 3.14
            ensure
              Fiddle.___free(ptr)
            end
            :ok
            "#
        ));
    }

    // `___read_string` stops at the first NUL byte; `___read_bytes`
    // takes an explicit length. Verifying both on the same buffer keeps
    // the string-read path in sync with the NUL-termination behaviour
    // of the write path.
    #[test]
    fn fiddle_read_string_and_bytes() {
        run_test_no_result_check(&format!(
            r#"{TYPE_PRELUDE}
            ptr = Fiddle.___malloc(32)
            raise if ptr == 0
            begin
              Fiddle.___write_bytes(ptr, "hello\0world\0junk")
              raise unless Fiddle.___read_string(ptr)    == "hello"
              raise unless Fiddle.___read_bytes(ptr, 11) == "hello\0world"
            ensure
              Fiddle.___free(ptr)
            end
            :ok
            "#
        ));
    }

    // Hot loop over `Fiddle.___read` / `Fiddle.___write` with
    // compile-time-constant type codes — exercises the inline JIT
    // path that emits a typed load/store instead of dispatching
    // through the Rust builtin.
    #[test]
    fn fiddle_read_write_inline_jit() {
        run_test_no_result_check(&format!(
            r#"{TYPE_PRELUDE}
            TY_CHAR   = 2
            TY_UCHAR  = -2
            TY_SHORT  = 3
            TY_USHORT = -3
            TY_UINT   = -4
            ptr = Fiddle.___malloc(16)
            raise "malloc returned NULL" if ptr == 0
            begin
              200.times do
                Fiddle.___write(ptr, TY_CHAR,   -42)
                raise unless Fiddle.___read(ptr, TY_CHAR)   == -42
                Fiddle.___write(ptr, TY_UCHAR,  200)
                raise unless Fiddle.___read(ptr, TY_UCHAR)  == 200
                Fiddle.___write(ptr, TY_SHORT,  -12345)
                raise unless Fiddle.___read(ptr, TY_SHORT)  == -12345
                Fiddle.___write(ptr, TY_USHORT, 60000)
                raise unless Fiddle.___read(ptr, TY_USHORT) == 60000
                Fiddle.___write(ptr, TY_INT,    0x41424344)
                raise unless Fiddle.___read(ptr, TY_INT)    == 0x41424344
                Fiddle.___write(ptr, TY_UINT,   0xDEADBEEF)
                raise unless Fiddle.___read(ptr, TY_UINT)   == 0xDEADBEEF
                Fiddle.___write(ptr, TY_DOUBLE, 3.14)
                raise unless Fiddle.___read(ptr, TY_DOUBLE) == 3.14
              end
            ensure
              Fiddle.___free(ptr)
            end
            :ok
            "#
        ));
    }

    // Passing `nil` as `TYPE_VOIDP` resolves to a NULL pointer.
    // `memcpy(NULL, NULL, 0)` is well-defined on glibc — the zero
    // length short-circuits before any dereference.
    #[test]
    fn fiddle_null_pointer_arg() {
        run_test_no_result_check(&format!(
            r#"{TYPE_PRELUDE}
            memcpy = Fiddle.___dlsym(LIBC, "memcpy")
            res = Fiddle.___call(memcpy, [nil, nil, 0], [TY_VOIDP, TY_VOIDP, TY_SIZE_T], TY_VOIDP)
            raise unless res == 0
            :ok
            "#
        ));
    }

    // A prepared call must agree with `___call` across every argument and
    // return kind: the CIF is now built from the type codes alone, so a
    // disagreement with `value_to_carg`'s choice of CArg width would hand
    // libffi a value of the wrong size.
    #[test]
    fn fiddle_prepare_matches_call() {
        run_test_no_result_check(&format!(
            r#"{TYPE_PRELUDE}
            abs    = Fiddle.___dlsym(LIBC, "abs")
            llabs  = Fiddle.___dlsym(LIBC, "llabs")
            strlen = Fiddle.___dlsym(LIBC, "strlen")
            memcpy = Fiddle.___dlsym(LIBC, "memcpy")
            pow    = Fiddle.___dlsym(LIBM, "pow")

            p_abs    = Fiddle.___prepare(abs,    [TY_INT],   TY_INT)
            p_llabs  = Fiddle.___prepare(llabs,  [TY_LLONG], TY_LLONG)
            p_strlen = Fiddle.___prepare(strlen, [TY_VOIDP], TY_SIZE_T)
            p_memcpy = Fiddle.___prepare(memcpy, [TY_VOIDP, TY_VOIDP, TY_SIZE_T], TY_VOIDP)
            p_pow    = Fiddle.___prepare(pow,    [TY_DOUBLE, TY_DOUBLE], TY_DOUBLE)

            # int / long long, both signs
            [-42, 0, 42].each do |n|
              raise unless Fiddle.___invoke(p_abs, n) == Fiddle.___call(abs, [n], [TY_INT], TY_INT)
            end
            [-1_000_000_000_000, 1_000_000_000_000].each do |n|
              raise unless Fiddle.___invoke(p_llabs, n) == n.abs
            end

            # String argument keeps the NUL-termination guarantee
            ["", "hello", "hello_optcarrot"].each do |s|
              raise unless Fiddle.___invoke(p_strlen, s) == s.bytesize
            end

            # nil as a NULL pointer, and a pointer return
            raise unless Fiddle.___invoke(p_memcpy, nil, nil, 0) == 0

            # double in, double out
            raise unless Fiddle.___invoke(p_pow, 2.0, 10.0) == 1024.0

            # 0-arg signatures are prepared too
            getpid = Fiddle.___dlsym(LIBC, "getpid")
            p_getpid = Fiddle.___prepare(getpid, [], TY_INT)
            raise unless Fiddle.___invoke(p_getpid) == Fiddle.___call(getpid, [], [], TY_INT)

            # arity is enforced against the prepared signature
            begin
              Fiddle.___invoke(p_abs)
              raise "expected ArgumentError for too few"
            rescue ArgumentError
            end
            begin
              Fiddle.___invoke(p_abs, 1, 2)
              raise "expected ArgumentError for too many"
            rescue ArgumentError
            end

            # a signature ___invoke cannot serve is refused at prepare time,
            # so a successful prepare guarantees the call works
            begin
              Fiddle.___prepare(abs, [TY_INT] * 9, TY_INT)
              raise "expected ArgumentError for over-wide signature"
            rescue ArgumentError
            end
            :ok
            "#
        ));
    }

    // Every argument type code must map to an libffi type. Preparing is pure
    // — it only builds the CIF — so each code can be checked without needing
    // a C function that actually has that signature.
    #[test]
    fn fiddle_prepare_accepts_every_arg_type_code() {
        run_test_no_result_check(&format!(
            r#"{TYPE_PRELUDE}
            TY_UCHAR  = -2
            TY_SHORT  =  3
            TY_USHORT = -3
            TY_UINT   = -4
            TY_LONG   =  5
            TY_ULONG  = -5
            TY_ULLONG = -6
            TY_CHAR   =  2
            TY_FLOAT  =  7
            TY_BOOL   =  9
            abs = Fiddle.___dlsym(LIBC, "abs")

            codes = [TY_CHAR, TY_UCHAR, TY_SHORT, TY_USHORT, TY_INT, TY_UINT,
                     TY_LONG, TY_LLONG, TY_ULONG, TY_ULLONG, TY_VOIDP,
                     TY_FLOAT, TY_DOUBLE, TY_BOOL]
            codes.each do |c|
              raise "prepare failed for #{{c}}" unless Fiddle.___prepare(abs, [c], TY_INT) != 0
            end
            # every return code is validated up front too
            (codes + [0]).each do |c|
              raise "prepare failed for ret #{{c}}" unless Fiddle.___prepare(abs, [], c) != 0
            end

            # TYPE_VOID is not a legal *argument*, and unknown codes are refused
            [0, 99, -99].each do |bad|
              begin
                Fiddle.___prepare(abs, [bad], TY_INT)
                raise "expected an error for arg code #{{bad}}"
              rescue RuntimeError
              end
            end
            [99, -99].each do |bad|
              begin
                Fiddle.___prepare(abs, [], bad)
                raise "expected an error for ret code #{{bad}}"
              rescue RuntimeError
              end
            end
            :ok
            "#
        ));
    }

    // monoruby's Fixnum is an i63, so a C integer argument arrives as either a
    // Fixnum or a BigInt depending only on magnitude. Both must convert, and
    // anything that genuinely does not fit — or is not an Integer at all —
    // must raise rather than pass a garbage value to C.
    #[test]
    fn fiddle_integer_arg_conversion() {
        run_test_no_result_check(&format!(
            r#"{TYPE_PRELUDE}
            abs   = Fiddle.___dlsym(LIBC, "abs")
            llabs = Fiddle.___dlsym(LIBC, "llabs")
            p_abs   = Fiddle.___prepare(abs,   [TY_INT],   TY_INT)
            p_llabs = Fiddle.___prepare(llabs, [TY_LLONG], TY_LLONG)

            # Fixnum, and a BigInt that fits i64 (2**62 is already a BigInt here)
            raise unless Fiddle.___invoke(p_llabs, -5) == 5
            raise unless Fiddle.___invoke(p_llabs, 2**62) == 2**62
            raise unless Fiddle.___invoke(p_llabs, 9223372036854775807) == 9223372036854775807

            # A BigInt in [2**63, 2**64) is a valid unsigned 64-bit value; it
            # reaches C with the same bit pattern, so `int` truncation gives -1.
            raise unless Fiddle.___invoke(p_abs, 18446744073709551615) == 1

            # Genuinely out of range for any C integer
            begin
              Fiddle.___invoke(p_llabs, 2**80)
              raise "expected RangeError"
            rescue RangeError
            end

            # Not an Integer at all
            begin
              Fiddle.___invoke(p_abs, "nope")
              raise "expected TypeError"
            rescue TypeError
            end
            begin
              Fiddle.___call(abs, [nil], [TY_INT], TY_INT)
              raise "expected TypeError"
            rescue TypeError
            end
            :ok
            "#
        ));
    }

    // A `:string` return is folded into the prepared descriptor so that one
    // builtin performs both the call and the char*-to-String copy. What it
    // produces has to be indistinguishable from the two-call form it
    // replaces, NULL included.
    #[test]
    fn fiddle_prepared_string_return() {
        run_test_no_result_check(&format!(
            r#"{TYPE_PRELUDE}
            FLAG_RETURN_STRING = 1
            getenv = Fiddle.___dlsym(LIBC, "getenv")
            two = Fiddle.___prepare(getenv, [TY_VOIDP], TY_VOIDP)
            one = Fiddle.___prepare(getenv, [TY_VOIDP], TY_VOIDP, FLAG_RETURN_STRING)

            # A variable that exists: the same String either way. (Compared
            # against the two-call form rather than a literal, so the test
            # does not depend on what the environment actually holds.)
            a = Fiddle.___read_string(Fiddle.___invoke(two, "PATH"))
            b = Fiddle.___invoke(one, "PATH")
            raise "fused result differs" unless a == b
            raise "not a String" unless b.is_a?(String)
            raise "unexpectedly empty" if b.empty?

            # A variable that does not exist: NULL becomes nil on both paths.
            miss = "MONORUBY_NO_SUCH_ENV_VAR_XYZZY"
            raise "two-call NULL" unless Fiddle.___read_string(Fiddle.___invoke(two, miss)).nil?
            raise "fused NULL" unless Fiddle.___invoke(one, miss).nil?

            # The flag only means something for a pointer return.
            begin
              Fiddle.___prepare(getenv, [TY_VOIDP], TY_INT, FLAG_RETURN_STRING)
              raise "expected an ArgumentError"
            rescue ArgumentError
            end

            # An empty flag word is the three-argument behaviour: the raw
            # address comes back, not a String.
            three = Fiddle.___prepare(getenv, [TY_VOIDP], TY_VOIDP, 0)
            raise "explicit false" unless Fiddle.___invoke(three, "PATH").is_a?(Integer)
            :ok
            "#
        ));
    }

    // The point of the blocking flag: green threads share one OS thread, so a
    // C call that blocks inline freezes every one of them. Offloaded, the
    // others keep running. Measured by how far a busy thread gets while a
    // 250ms C sleep is in flight -- inline it must not advance at all.
    #[test]
    fn fiddle_blocking_yields_to_other_threads() {
        run_test_no_result_check(&format!(
            r#"{TYPE_PRELUDE}
            FLAG_BLOCKING = 2
            usleep = Fiddle.___dlsym(LIBC, "usleep")
            inline   = Fiddle.___prepare(usleep, [TY_INT], TY_INT, 0)
            offload  = Fiddle.___prepare(usleep, [TY_INT], TY_INT, FLAG_BLOCKING)

            def progress_during(id)
              counter = 0
              stop = false
              bg = Thread.new {{ until stop; counter += 1; Thread.pass; end }}
              sleep 0.05                     # let the busy thread get going
              base = counter
              raise "usleep failed" unless Fiddle.___invoke(id, 250_000) == 0
              advanced = counter - base
              stop = true
              bg.join
              advanced
            end

            inline_advanced  = progress_during(inline)
            offload_advanced = progress_during(offload)

            # Inline, nothing else can run at all. The margins are loose on
            # purpose -- what matters is one is zero-ish and the other is not.
            raise "inline call let other threads run (#{{inline_advanced}})" if inline_advanced > 10
            raise "offloaded call starved other threads (#{{offload_advanced}})" if offload_advanced < 100
            :ok
            "#
        ));
    }

    // Everything a call can return has to survive the trip through a worker
    // thread, which hands back raw bits for the interpreter thread to box.
    #[test]
    fn fiddle_blocking_return_types() {
        run_test_no_result_check(&format!(
            r#"{TYPE_PRELUDE}
            TY_FLOAT = 7
            B = 2      # blocking
            S = 1      # return string

            i   = Fiddle.___prepare(Fiddle.___dlsym(LIBC, "abs"),      [TY_INT],   TY_INT,   B)
            l   = Fiddle.___prepare(Fiddle.___dlsym(LIBC, "llabs"),    [TY_LLONG], TY_LLONG, B)
            d   = Fiddle.___prepare(Fiddle.___dlsym(LIBM, "sqrt"),     [TY_DOUBLE], TY_DOUBLE, B)
            f   = Fiddle.___prepare(Fiddle.___dlsym(LIBC, "strtof"),   [TY_VOIDP, TY_VOIDP], TY_FLOAT, B)
            v   = Fiddle.___prepare(Fiddle.___dlsym(LIBC, "free"),     [TY_VOIDP], 0,        B)
            ptr = Fiddle.___prepare(Fiddle.___dlsym(LIBC, "memcpy"),   [TY_VOIDP, TY_VOIDP, TY_LLONG], TY_VOIDP, B)
            str = Fiddle.___prepare(Fiddle.___dlsym(LIBC, "strerror"), [TY_INT],   TY_VOIDP, B | S)
            env = Fiddle.___prepare(Fiddle.___dlsym(LIBC, "getenv"),   [TY_VOIDP], TY_VOIDP, B | S)
            len = Fiddle.___prepare(Fiddle.___dlsym(LIBC, "strlen"),   [TY_VOIDP], TY_LLONG, B)

            raise "int"    unless Fiddle.___invoke(i, -42) == 42
            raise "int64"  unless Fiddle.___invoke(l, -2**40) == 2**40
            raise "bignum" unless Fiddle.___invoke(l, 2**62) == 2**62
            raise "double" unless (Fiddle.___invoke(d, 1024.0) - 32.0).abs < 1e-9
            raise "float"  unless (Fiddle.___invoke(f, "2.5", nil) - 2.5).abs < 1e-6
            raise "void"   unless Fiddle.___invoke(v, Fiddle.___malloc(8)).nil?
            # A String argument hands over its own buffer; the worker holds
            # that raw pointer while this frame keeps the String alive.
            raise "string arg" unless Fiddle.___invoke(len, "hello world") == 11

            src = Fiddle.___malloc(16)
            dst = Fiddle.___malloc(16)
            Fiddle.___write_bytes(src, "abcdefgh")
            raise "pointer return" unless Fiddle.___invoke(ptr, dst, src, 8) == dst
            raise "pointer content" unless Fiddle.___read_bytes(dst, 8) == "abcdefgh"

            raise "string return" unless Fiddle.___invoke(str, 2).is_a?(String)
            raise "NULL to nil" unless Fiddle.___invoke(env, "MONORUBY_NO_SUCH_VAR_XYZZY").nil?
            :ok
            "#
        ));
    }

    // A thread parked on a worker must still be interruptible: the ticket is
    // discarded and the worker's late result dropped, rather than the thread
    // being stuck until the C call happens to return.
    #[test]
    fn fiddle_blocking_is_interruptible() {
        run_test_no_result_check(&format!(
            r#"{TYPE_PRELUDE}
            FLAG_BLOCKING = 2
            slp = Fiddle.___prepare(Fiddle.___dlsym(LIBC, "usleep"), [TY_INT], TY_INT, FLAG_BLOCKING)

            # kill: returns long before the 3s call would have
            t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
            th = Thread.new {{ Fiddle.___invoke(slp, 3_000_000) }}
            sleep 0.1
            th.kill
            th.join
            elapsed = Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0
            raise "kill did not interrupt the parked call" if elapsed > 1.5

            # raise: delivered while parked
            message = nil
            th2 = Thread.new do
              begin
                Fiddle.___invoke(slp, 3_000_000)
                :not_raised
              rescue => e
                message = e.message
                :raised
              end
            end
            sleep 0.1
            th2.raise(RuntimeError, "boom")
            raise "raise was not delivered" unless th2.value == :raised
            raise "wrong exception: #{{message}}" unless message == "boom"

            # and the interpreter is still usable afterwards
            raise "call after interrupt" unless Fiddle.___invoke(slp, 1000) == 0
            :ok
            "#
        ));
    }

    // `___prepare` is the one place that can still reject a bad call site
    // cheaply, so it validates the flag word and refuses a NULL target --
    // `___dlsym` returns 0 for a symbol it could not resolve, and calling
    // that would be a segfault with no clue as to which symbol was missing.
    #[test]
    fn fiddle_prepare_rejects_bad_flags_and_null_target() {
        run_test_no_result_check(&format!(
            r#"{TYPE_PRELUDE}
            abs = Fiddle.___dlsym(LIBC, "abs")
            [64, 8, -1].each do |bad|
              begin
                Fiddle.___prepare(abs, [TY_INT], TY_INT, bad)
                raise "expected an ArgumentError for flags #{{bad}}"
              rescue ArgumentError
              end
            end
            begin
              Fiddle.___prepare(0, [TY_INT], TY_INT, 0)
              raise "expected an ArgumentError for a NULL target"
            rescue ArgumentError
            end
            # the two defined flags, together, are accepted
            raise "valid flags" unless Fiddle.___prepare(abs, [TY_INT], TY_VOIDP, 1 | 2) != 0
            :ok
            "#
        ));
    }

    // The prepared path is what Fiddle::Function and FFI::Function now use,
    // so exercise it through the public facade as well.
    #[test]
    fn fiddle_function_prepared() {
        run_test_no_result_check(
            r#"
            require "fiddle"
            libm = RUBY_PLATFORM =~ /darwin/ ? "/usr/lib/libSystem.B.dylib" : "libm.so.6"
            pow = Fiddle::Function.new(
              Fiddle::Handle.new(libm)["pow"],
              [Fiddle::TYPE_DOUBLE, Fiddle::TYPE_DOUBLE],
              Fiddle::TYPE_DOUBLE
            )
            raise unless pow.call(2.0, 10.0) == 1024.0
            raise unless pow.call(3.0, 2.0) == 9.0
            begin
              pow.call(2.0)
              raise "expected ArgumentError"
            rescue ArgumentError
            end
            :ok
            "#,
        );
    }
}
