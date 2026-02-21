use std::ffi::c_void;

use super::*;
use libffi::middle::{Arg, Cif, CodePtr, Type};

// ---------------------------------------------------------------------------
// Fiddle type codes  (must match startup/fiddle.rb and startup/ffi_c.rb)
// ---------------------------------------------------------------------------
const TYPE_VOID: i64 = 0;
const TYPE_VOIDP: i64 = -1;
const TYPE_CHAR: i64 = -2;
const TYPE_UCHAR: i64 = -3;
const TYPE_SHORT: i64 = -4;
const TYPE_USHORT: i64 = -5;
const TYPE_INT: i64 = -6;
const TYPE_UINT: i64 = -7;
const TYPE_LONG: i64 = -8;
const TYPE_ULONG: i64 = -9;
const TYPE_LONG_LONG: i64 = -10;
const TYPE_ULONG_LONG: i64 = -11;
const TYPE_FLOAT: i64 = -12;
const TYPE_DOUBLE: i64 = -13;
const TYPE_BOOL: i64 = -14;
// Platform-specific aliases (all 8 bytes on x86-64)
const TYPE_INTPTR_T: i64 = -15;
const TYPE_UINTPTR_T: i64 = -16;
const TYPE_PTRDIFF_T: i64 = -17;
const TYPE_SIZE_T: i64 = -18;
const TYPE_SSIZE_T: i64 = -19;

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
    match ty {
        TYPE_VOID => Ok(Type::void()),
        TYPE_VOIDP | TYPE_UINTPTR_T | TYPE_SIZE_T => Ok(Type::pointer()),
        TYPE_CHAR => Ok(Type::i8()),
        TYPE_UCHAR => Ok(Type::u8()),
        TYPE_SHORT => Ok(Type::i16()),
        TYPE_USHORT => Ok(Type::u16()),
        TYPE_INT | TYPE_BOOL => Ok(Type::i32()),
        TYPE_UINT => Ok(Type::u32()),
        TYPE_LONG | TYPE_LONG_LONG | TYPE_INTPTR_T | TYPE_PTRDIFF_T | TYPE_SSIZE_T => {
            Ok(Type::i64())
        }
        TYPE_ULONG | TYPE_ULONG_LONG => Ok(Type::u64()),
        TYPE_FLOAT => Ok(Type::f32()),
        TYPE_DOUBLE => Ok(Type::f64()),
        _ => Err(MonorubyErr::runtimeerr(format!(
            "Fiddle: unsupported return type code {}",
            ty
        ))),
    }
}

/// Convert a Ruby Value and a Fiddle type code into a `CArg`.
///
/// For `TYPE_VOIDP`, a Ruby String value is accepted: its raw byte-buffer
/// pointer is passed to the C function.  An Integer is treated as an address.
fn value_to_carg(globals: &mut Globals, val: Value, ty: i64) -> Result<CArg> {
    match ty {
        TYPE_VOID => Ok(CArg::I64(0)), // should not appear as argument
        TYPE_CHAR => Ok(CArg::I8(val.expect_integer(globals)? as i8)),
        TYPE_UCHAR => Ok(CArg::U8(val.expect_integer(globals)? as u8)),
        TYPE_SHORT => Ok(CArg::I16(val.expect_integer(globals)? as i16)),
        TYPE_USHORT => Ok(CArg::U16(val.expect_integer(globals)? as u16)),
        TYPE_INT | TYPE_BOOL => Ok(CArg::I32(val.expect_integer(globals)? as i32)),
        TYPE_UINT => Ok(CArg::U32(val.expect_integer(globals)? as u32)),
        TYPE_LONG | TYPE_LONG_LONG | TYPE_INTPTR_T | TYPE_PTRDIFF_T | TYPE_SSIZE_T => {
            Ok(CArg::I64(val.expect_integer(globals)?))
        }
        TYPE_ULONG | TYPE_ULONG_LONG | TYPE_UINTPTR_T | TYPE_SIZE_T => {
            Ok(CArg::U64(val.expect_integer(globals)? as u64))
        }
        TYPE_VOIDP => {
            // Accept: nil → NULL, Integer → address, String → raw bytes ptr
            match val.unpack() {
                RV::Fixnum(i) => Ok(CArg::U64(i as u64)),
                RV::BigInt(b) => Ok(CArg::U64(num::ToPrimitive::to_u64(b).unwrap_or(0))),
                RV::Nil => Ok(CArg::U64(0)),
                _ => {
                    // String: pass raw content pointer (GC cannot run here)
                    let ptr = val.as_rstring_inner().as_ptr() as u64;
                    Ok(CArg::U64(ptr))
                }
            }
        }
        TYPE_FLOAT => {
            let f = val.coerce_to_f64(globals)? as f32;
            Ok(CArg::F32(f))
        }
        TYPE_DOUBLE => {
            let f = val.coerce_to_f64(globals)?;
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

    let result = match ret_type_code {
        TYPE_VOID => {
            // For void, call with i64 return and discard.
            let _: i64 = unsafe { cif.call(func, &ffi_args) };
            Value::nil()
        }
        TYPE_CHAR => {
            let v: i8 = unsafe { cif.call(func, &ffi_args) };
            Value::integer(v as i64)
        }
        TYPE_UCHAR => {
            let v: u8 = unsafe { cif.call(func, &ffi_args) };
            Value::integer(v as i64)
        }
        TYPE_SHORT => {
            let v: i16 = unsafe { cif.call(func, &ffi_args) };
            Value::integer(v as i64)
        }
        TYPE_USHORT => {
            let v: u16 = unsafe { cif.call(func, &ffi_args) };
            Value::integer(v as i64)
        }
        TYPE_INT | TYPE_BOOL => {
            let v: i32 = unsafe { cif.call(func, &ffi_args) };
            Value::integer(v as i64)
        }
        TYPE_UINT => {
            let v: u32 = unsafe { cif.call(func, &ffi_args) };
            Value::integer(v as i64)
        }
        TYPE_LONG | TYPE_LONG_LONG | TYPE_INTPTR_T | TYPE_PTRDIFF_T | TYPE_SSIZE_T => {
            let v: i64 = unsafe { cif.call(func, &ffi_args) };
            Value::integer(v)
        }
        TYPE_VOIDP | TYPE_ULONG | TYPE_ULONG_LONG | TYPE_UINTPTR_T | TYPE_SIZE_T => {
            let v: u64 = unsafe { cif.call(func, &ffi_args) };
            Value::integer(v as i64)
        }
        TYPE_FLOAT => {
            let v: f32 = unsafe { cif.call(func, &ffi_args) };
            Value::float(v as f64)
        }
        TYPE_DOUBLE => {
            let v: f64 = unsafe { cif.call(func, &ffi_args) };
            Value::float(v)
        }
        _ => {
            return Err(MonorubyErr::runtimeerr(format!(
                "Fiddle: unsupported return type code {}",
                ret_type_code
            )));
        }
    };

    // Keep c_args alive until here
    drop(ffi_args);
    drop(c_args);

    Ok(result)
}

// ---------------------------------------------------------------------------
// Builtin functions exposed to Ruby as Fiddle module-level functions
// ---------------------------------------------------------------------------

/// ### Fiddle.___call(ptr, args, arg_types, ret_type) -> Object
///
/// Low-level foreign-function call using libffi.
/// - ptr       : Integer  – function address from dlsym
/// - args      : Array    – Ruby argument values
/// - arg_types : Array    – Fiddle type-code integers for each argument
/// - ret_type  : Integer  – Fiddle type-code for the return value
#[monoruby_builtin]
fn fiddle_call(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn fiddle_read(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ptr = lfp.arg(0).expect_integer(globals)? as usize;
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
            TYPE_LONG | TYPE_LONG_LONG | TYPE_INTPTR_T | TYPE_PTRDIFF_T | TYPE_SSIZE_T => {
                Value::integer(*(ptr as *const i64))
            }
            TYPE_VOIDP | TYPE_ULONG | TYPE_ULONG_LONG | TYPE_UINTPTR_T | TYPE_SIZE_T => {
                Value::integer(*(ptr as *const u64) as i64)
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
fn fiddle_write(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ptr = lfp.arg(0).expect_integer(globals)? as usize;
    let ty = lfp.arg(1).expect_integer(globals)?;
    let val = lfp.arg(2);
    if ptr == 0 {
        return Err(MonorubyErr::runtimeerr("Fiddle.___write: NULL pointer"));
    }
    unsafe {
        match ty {
            TYPE_CHAR => *(ptr as *mut i8) = val.expect_integer(globals)? as i8,
            TYPE_UCHAR => *(ptr as *mut u8) = val.expect_integer(globals)? as u8,
            TYPE_SHORT => *(ptr as *mut i16) = val.expect_integer(globals)? as i16,
            TYPE_USHORT => *(ptr as *mut u16) = val.expect_integer(globals)? as u16,
            TYPE_INT | TYPE_BOOL => *(ptr as *mut i32) = val.expect_integer(globals)? as i32,
            TYPE_UINT => *(ptr as *mut u32) = val.expect_integer(globals)? as u32,
            TYPE_LONG | TYPE_LONG_LONG | TYPE_INTPTR_T | TYPE_PTRDIFF_T | TYPE_SSIZE_T => {
                *(ptr as *mut i64) = val.expect_integer(globals)?;
            }
            TYPE_VOIDP | TYPE_ULONG | TYPE_ULONG_LONG | TYPE_UINTPTR_T | TYPE_SIZE_T => {
                *(ptr as *mut u64) = val.expect_integer(globals)? as u64;
            }
            TYPE_FLOAT => *(ptr as *mut f32) = val.coerce_to_f64(globals)? as f32,
            TYPE_DOUBLE => *(ptr as *mut f64) = val.coerce_to_f64(globals)?,
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

/// ### Fiddle.___read_string(ptr) -> String | nil
///
/// Read a null-terminated C string from `ptr`.
/// Returns nil if `ptr` is 0 (NULL).
#[monoruby_builtin]
fn fiddle_read_string(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ptr = lfp.arg(0).expect_integer(globals)? as usize;
    if ptr == 0 {
        return Ok(Value::nil());
    }
    let s = unsafe { std::ffi::CStr::from_ptr(ptr as *const libc::c_char) };
    Ok(Value::string_from_vec(s.to_bytes().to_vec()))
}

/// ### Fiddle.___read_bytes(ptr, len) -> String
///
/// Read exactly `len` bytes from `ptr` into a binary Ruby String.
#[monoruby_builtin]
fn fiddle_read_bytes(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn fiddle_write_bytes(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn fiddle_free(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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

    // Low-level primitives used by startup/fiddle.rb and startup/ffi_c.rb
    globals.define_builtin_module_func(fiddle, "___call", fiddle_call, 4);
    globals.define_builtin_module_func(fiddle, "___read", fiddle_read, 2);
    globals.define_builtin_module_func(fiddle, "___write", fiddle_write, 3);
    globals.define_builtin_module_func(fiddle, "___read_string", fiddle_read_string, 1);
    globals.define_builtin_module_func(fiddle, "___read_bytes", fiddle_read_bytes, 2);
    globals.define_builtin_module_func(fiddle, "___write_bytes", fiddle_write_bytes, 2);
    globals.define_builtin_module_func(fiddle, "___free", fiddle_free, 1);

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
