use std::ffi::c_void;

use super::*;
use jitgen::JitContext;
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
    /// Ruby String passed as a C `char*`. Owns a heap buffer with an
    /// appended NUL terminator (Ruby strings are not NUL-terminated) and
    /// caches its pointer as a u64 so `as_libffi_arg` can reference a
    /// stable location. Moving the `CArg` is safe because the `Vec<u8>`
    /// move leaves the heap allocation in place.
    CStr {
        _buf: Vec<u8>,
        ptr: u64,
    },
}

impl CArg {
    fn from_bytes_nul_terminated(bytes: &[u8]) -> Self {
        let mut buf = Vec::with_capacity(bytes.len() + 1);
        buf.extend_from_slice(bytes);
        buf.push(0);
        let ptr = buf.as_ptr() as u64;
        CArg::CStr { _buf: buf, ptr }
    }

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
            CArg::CStr { ptr, .. } => Arg::new(ptr),
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
            CArg::CStr { .. } => Type::pointer(),
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
                RV::String(_) => {
                    // Ruby strings are not NUL-terminated, so passing
                    // `as_ptr()` directly to a C function expecting a
                    // `char*` (e.g. `SDL_SetWindowTitle`, `strlen`)
                    // causes a read past the end of the string buffer.
                    // Copy into a NUL-terminated buffer owned by the
                    // CArg for the duration of the libffi call.
                    let inner = val.as_rstring_inner();
                    Ok(CArg::from_bytes_nul_terminated(inner.as_bytes()))
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
fn fiddle_write(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
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
// When the type code is a constant Fixnum at compile time and the requested
// width fits in a Fixnum (i63), the JIT can emit a direct typed load/store
// against the memory pointed to by `ptr`, skipping the type-code dispatch
// and libffi-free Rust path. NULL pointers deopt to the interpreter so the
// regular builtin raises the runtime error.
// ---------------------------------------------------------------------------

#[derive(Clone, Copy)]
enum ReadKind {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    F64,
}

fn fiddle_read_inline(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
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
        TYPE_DOUBLE => ReadKind::F64,
        _ => return false,
    };

    state.load_fixnum(ir, args, GP::Rdi);
    let deopt = ir.new_deopt(state);

    match kind {
        ReadKind::F64 => {
            let fret = state.def_F(dst);
            ir.inline(move |r#gen, _, labels, base| {
                let deopt_label = &labels[deopt];
                monoasm! { &mut r#gen.jit,
                    sarq rdi, 1;
                    testq rdi, rdi;
                    jz deopt_label;
                    movq xmm0, [rdi];
                };
                r#gen.store_fpr_into_xmm(fret, base);
            });
        }
        _ => {
            ir.inline(move |r#gen, _, labels, _| {
                let deopt_label = &labels[deopt];
                monoasm! { &mut r#gen.jit,
                    sarq rdi, 1;
                    testq rdi, rdi;
                    jz deopt_label;
                };
                match kind {
                    ReadKind::I8 => monoasm! { &mut r#gen.jit, movsxb rax, [rdi]; },
                    ReadKind::U8 => monoasm! { &mut r#gen.jit, movzxb rax, [rdi]; },
                    ReadKind::I16 => monoasm! { &mut r#gen.jit, movsxw rax, [rdi]; },
                    ReadKind::U16 => monoasm! { &mut r#gen.jit, movzxw rax, [rdi]; },
                    ReadKind::I32 => monoasm! { &mut r#gen.jit, movsxl rax, [rdi]; },
                    ReadKind::U32 => monoasm! { &mut r#gen.jit, movl rax, [rdi]; },
                    ReadKind::F64 => unreachable!(),
                };
                // Tag as Fixnum: rax = (rax << 1) | 1.
                monoasm! { &mut r#gen.jit,
                    addq rax, rax;
                    orq rax, 1;
                };
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
    F64,
}

fn fiddle_write_inline(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
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
        TYPE_DOUBLE => WriteKind::F64,
        _ => return false,
    };

    let val_slot = args + 2usize;
    state.load_fixnum(ir, args, GP::Rdi);

    match kind {
        WriteKind::F64 => {
            let xsrc = state.load_xmm(ir, val_slot);
            let deopt = ir.new_deopt(state);
            ir.inline(move |r#gen, _, labels, base| {
                let deopt_label = &labels[deopt];
                r#gen.load_fpr_into_xmm0(xsrc, base);
                monoasm! { &mut r#gen.jit,
                    movq rax, rdi;
                    sarq rdi, 1;
                    testq rdi, rdi;
                    jz deopt_label;
                    movq [rdi], xmm0;
                };
            });
        }
        _ => {
            state.load_fixnum(ir, val_slot, GP::Rsi);
            let deopt = ir.new_deopt(state);
            ir.inline(move |r#gen, _, labels, _| {
                let deopt_label = &labels[deopt];
                monoasm! { &mut r#gen.jit,
                    movq rax, rdi;
                    sarq rdi, 1;
                    testq rdi, rdi;
                    jz deopt_label;
                    sarq rsi, 1;
                };
                match kind {
                    WriteKind::Int8 => monoasm! { &mut r#gen.jit, movb [rdi], rsi; },
                    WriteKind::Int16 => monoasm! { &mut r#gen.jit, movw [rdi], rsi; },
                    WriteKind::Int32 => monoasm! { &mut r#gen.jit, movl [rdi], rsi; },
                    WriteKind::F64 => unreachable!(),
                };
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

    // Low-level primitives used by startup/fiddle.rb and startup/ffi_c.rb
    globals.define_builtin_module_func(fiddle, "___call", fiddle_call, 4);
    globals.define_builtin_module_inline_func(
        fiddle,
        "___read",
        fiddle_read,
        Box::new(fiddle_read_inline),
        2,
    );
    globals.define_builtin_module_inline_func(
        fiddle,
        "___write",
        fiddle_write,
        Box::new(fiddle_write_inline),
        3,
    );
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

#[cfg(test)]
mod tests {
    use crate::tests::*;

    // The harness's reference Ruby runs with `--disable-gem`, so the
    // ffi gem is unavailable to compare against. These tests instead
    // drive the low-level builtins registered in `ffi.rs`
    // (`FFI.___dlopen`/`___dlsym`/`___call`/`___read`/`___write`/…) and
    // embed their own `raise` assertions — a misbehaving primitive
    // surfaces as a monoruby-side exception and fails the test.
    //
    // Type codes match the constants defined at the top of this file
    // (kept in sync with `startup/ffi_c.rb`).
    const TYPE_PRELUDE: &str = r#"
        TY_VOIDP = -1
        TY_INT   = -6
        TY_LLONG = -10
        TY_DOUBLE = -13
        TY_SIZE_T = -18
        LIBC = FFI.___dlopen("libc.so.6")
        LIBM = FFI.___dlopen("libm.so.6") || FFI.___dlopen("libc.so.6")
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
            strlen = FFI.___dlsym(LIBC, "strlen")
            call = ->(s) {{ FFI.___call(strlen, [s], [TY_VOIDP], TY_SIZE_T) }}
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
            strcmp = FFI.___dlsym(LIBC, "strcmp")
            call = ->(a, b) {{ FFI.___call(strcmp, [a, b], [TY_VOIDP, TY_VOIDP], TY_INT) }}
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
            abs   = FFI.___dlsym(LIBC, "abs")
            llabs = FFI.___dlsym(LIBC, "llabs")
            raise unless FFI.___call(abs,   [-42], [TY_INT],   TY_INT)   == 42
            raise unless FFI.___call(abs,   [ 42], [TY_INT],   TY_INT)   == 42
            raise unless FFI.___call(llabs, [-1_000_000_000_000], [TY_LLONG], TY_LLONG) == 1_000_000_000_000
            raise unless FFI.___call(llabs, [ 1_000_000_000_000], [TY_LLONG], TY_LLONG) == 1_000_000_000_000
            :ok
            "#
        ));
    }

    // Double args/returns via libm's `sqrt` and `floor`.
    #[test]
    fn fiddle_double_args() {
        run_test_no_result_check(&format!(
            r#"{TYPE_PRELUDE}
            sqrt  = FFI.___dlsym(LIBM, "sqrt")
            floor = FFI.___dlsym(LIBM, "floor")
            raise unless FFI.___call(sqrt,  [0.0], [TY_DOUBLE], TY_DOUBLE) == 0.0
            raise unless (FFI.___call(sqrt, [2.0], [TY_DOUBLE], TY_DOUBLE) - Math.sqrt(2.0)).abs < 1e-12
            raise unless FFI.___call(floor, [3.7],  [TY_DOUBLE], TY_DOUBLE) == 3.0
            raise unless FFI.___call(floor, [-0.5], [TY_DOUBLE], TY_DOUBLE) == -1.0
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
            ptr = FFI.___malloc(16)
            raise "malloc returned NULL" if ptr == 0
            begin
              FFI.___write(ptr, TY_INT, 0x41424344)
              raise unless FFI.___read(ptr, TY_INT) == 0x41424344
              FFI.___write(ptr, TY_DOUBLE, 3.14)
              raise unless FFI.___read(ptr, TY_DOUBLE) == 3.14
            ensure
              FFI.___free(ptr)
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
            ptr = FFI.___malloc(32)
            raise if ptr == 0
            begin
              FFI.___write_bytes(ptr, "hello\0world\0junk")
              raise unless FFI.___read_string(ptr)    == "hello"
              raise unless FFI.___read_bytes(ptr, 11) == "hello\0world"
            ensure
              FFI.___free(ptr)
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
            TY_CHAR   = -2
            TY_UCHAR  = -3
            TY_SHORT  = -4
            TY_USHORT = -5
            TY_UINT   = -7
            ptr = FFI.___malloc(16)
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
              FFI.___free(ptr)
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
            memcpy = FFI.___dlsym(LIBC, "memcpy")
            res = FFI.___call(memcpy, [nil, nil, 0], [TY_VOIDP, TY_VOIDP, TY_SIZE_T], TY_VOIDP)
            raise unless res == 0
            :ok
            "#
        ));
    }
}
