use super::*;
use crate::value::rvalue::{
    BUF_EXTERNAL, BUF_INTERNAL, BUF_LOCKED, BUF_MAPPED, BUF_PRIVATE, BUF_READONLY, BUF_SHARED,
    BufStorage, IoBufferInner,
};

//
// IO::Buffer — a fixed-size byte buffer (CRuby 3.1+, io_buffer.c).
//
// Storage lives in `ObjTy::IO_BUFFER` (`IoBufferInner`): a zero page
// (null), an owned heap allocation (INTERNAL / anonymous MAPPED), a view
// into a Ruby String (`.for`), or a view into another buffer (`#slice`).
// Slices share bytes with — but lock independently of — their source.
//

fn page_size() -> usize {
    // SAFETY: sysconf is async-signal-safe and always valid for
    // _SC_PAGESIZE.
    let n = unsafe { libc::sysconf(libc::_SC_PAGESIZE) };
    if n > 0 { n as usize } else { 4096 }
}

pub(super) fn init(globals: &mut Globals) {
    let object_class = globals.object_class();
    let klass = globals.define_builtin_class(
        "Buffer",
        IO_BUFFER_CLASS,
        object_class,
        IO_CLASS,
        ObjTy::IO_BUFFER,
    );
    let _ = klass;
    globals.store[IO_BUFFER_CLASS].set_alloc_func(io_buffer_alloc_func);

    globals.set_constant_by_str(
        IO_BUFFER_CLASS,
        "EXTERNAL",
        Value::integer(BUF_EXTERNAL as i64),
    );
    globals.set_constant_by_str(
        IO_BUFFER_CLASS,
        "INTERNAL",
        Value::integer(BUF_INTERNAL as i64),
    );
    globals.set_constant_by_str(IO_BUFFER_CLASS, "MAPPED", Value::integer(BUF_MAPPED as i64));
    globals.set_constant_by_str(IO_BUFFER_CLASS, "SHARED", Value::integer(BUF_SHARED as i64));
    globals.set_constant_by_str(IO_BUFFER_CLASS, "LOCKED", Value::integer(BUF_LOCKED as i64));
    globals.set_constant_by_str(
        IO_BUFFER_CLASS,
        "PRIVATE",
        Value::integer(BUF_PRIVATE as i64),
    );
    globals.set_constant_by_str(
        IO_BUFFER_CLASS,
        "READONLY",
        Value::integer(BUF_READONLY as i64),
    );
    globals.set_constant_by_str(
        IO_BUFFER_CLASS,
        "PAGE_SIZE",
        Value::integer(page_size() as i64),
    );
    globals.set_constant_by_str(IO_BUFFER_CLASS, "DEFAULT_SIZE", Value::integer(65536));
    // Byte-order probes (CRuby exposes these for get/set_value users).
    globals.set_constant_by_str(
        IO_BUFFER_CLASS,
        "LITTLE_ENDIAN",
        Value::bool(cfg!(target_endian = "little")),
    );
    globals.set_constant_by_str(
        IO_BUFFER_CLASS,
        "BIG_ENDIAN",
        Value::bool(cfg!(target_endian = "big")),
    );
    globals.set_constant_by_str(IO_BUFFER_CLASS, "HOST_ENDIAN", Value::bool(true));

    globals.define_builtin_class_func_with(IO_BUFFER_CLASS, "for", buffer_for, 1, 1, false);
    globals.define_builtin_class_func(IO_BUFFER_CLASS, "string", buffer_string, 1);
    globals.define_private_builtin_func_with(
        IO_BUFFER_CLASS,
        "initialize",
        initialize,
        0,
        2,
        false,
    );
    globals.define_builtin_func(IO_BUFFER_CLASS, "size", size, 0);
    globals.define_builtin_func(IO_BUFFER_CLASS, "valid?", valid_, 0);
    globals.define_builtin_func(IO_BUFFER_CLASS, "null?", null_, 0);
    globals.define_builtin_func(IO_BUFFER_CLASS, "empty?", empty_, 0);
    globals.define_builtin_func(IO_BUFFER_CLASS, "external?", external_, 0);
    globals.define_builtin_func(IO_BUFFER_CLASS, "internal?", internal_, 0);
    globals.define_builtin_func(IO_BUFFER_CLASS, "mapped?", mapped_, 0);
    globals.define_builtin_func(IO_BUFFER_CLASS, "shared?", shared_, 0);
    globals.define_builtin_func(IO_BUFFER_CLASS, "private?", private_, 0);
    globals.define_builtin_func(IO_BUFFER_CLASS, "readonly?", readonly_, 0);
    globals.define_builtin_func(IO_BUFFER_CLASS, "locked?", locked_, 0);
    globals.define_builtin_func(IO_BUFFER_CLASS, "locked", locked_block, 0);
    globals.define_builtin_func_with(IO_BUFFER_CLASS, "get_string", get_string, 0, 3, false);
    globals.define_builtin_func_with(IO_BUFFER_CLASS, "set_string", set_string, 1, 4, false);
    globals.define_builtin_func_with(IO_BUFFER_CLASS, "copy", copy, 1, 4, false);
    globals.define_private_builtin_func(IO_BUFFER_CLASS, "initialize_copy", initialize_copy, 1);
    globals.define_builtin_func(IO_BUFFER_CLASS, "free", free, 0);
    globals.define_builtin_func(IO_BUFFER_CLASS, "transfer", transfer, 0);
    globals.define_builtin_func(IO_BUFFER_CLASS, "resize", resize, 1);
    globals.define_builtin_func_with(IO_BUFFER_CLASS, "slice", slice, 0, 2, false);
    globals.define_builtin_func_with(IO_BUFFER_CLASS, "clear", clear, 0, 3, false);
    globals.define_builtin_func(IO_BUFFER_CLASS, "to_s", to_s, 0);
    globals.define_builtin_func(IO_BUFFER_CLASS, "inspect", inspect, 0);
    globals.define_builtin_func(IO_BUFFER_CLASS, "<=>", cmp, 1);
    globals.define_builtin_func(IO_BUFFER_CLASS, "hexdump", hexdump, 0);
    globals.define_builtin_class_func_with(IO_BUFFER_CLASS, "map", buffer_map, 1, 4, false);
    globals.define_builtin_func(IO_BUFFER_CLASS, "&", bit_and, 1);
    globals.define_builtin_func(IO_BUFFER_CLASS, "|", bit_or, 1);
    globals.define_builtin_func(IO_BUFFER_CLASS, "^", bit_xor, 1);
    globals.define_builtin_func(IO_BUFFER_CLASS, "~", bit_not, 0);
    globals.define_builtin_func(IO_BUFFER_CLASS, "and!", bit_and_inplace, 1);
    globals.define_builtin_func(IO_BUFFER_CLASS, "or!", bit_or_inplace, 1);
    globals.define_builtin_func(IO_BUFFER_CLASS, "xor!", bit_xor_inplace, 1);
    globals.define_builtin_func(IO_BUFFER_CLASS, "not!", bit_not_inplace, 0);
    globals.define_builtin_func(IO_BUFFER_CLASS, "get_value", get_value, 2);
    globals.define_builtin_func(IO_BUFFER_CLASS, "set_value", set_value, 3);
    globals.define_builtin_func_with(IO_BUFFER_CLASS, "each", each, 1, 3, false);
    globals.define_builtin_func_with(IO_BUFFER_CLASS, "values", values, 1, 3, false);
    globals.define_builtin_func_with(IO_BUFFER_CLASS, "each_byte", each_byte, 0, 2, false);
}

/// A get/set_value type symbol: byte width, kind, endianness.
/// Lowercase multi-byte names are little-endian, uppercase big-endian
/// (`:U8`/`:S8` have no endianness).
#[derive(Clone, Copy)]
enum ValKind {
    Unsigned,
    Signed,
    Float,
}

fn parse_value_type(name: &str) -> Option<(usize, ValKind, bool)> {
    // (width, kind, big_endian)
    Some(match name {
        "U8" => (1, ValKind::Unsigned, false),
        "S8" => (1, ValKind::Signed, false),
        "u16" => (2, ValKind::Unsigned, false),
        "U16" => (2, ValKind::Unsigned, true),
        "s16" => (2, ValKind::Signed, false),
        "S16" => (2, ValKind::Signed, true),
        "u32" => (4, ValKind::Unsigned, false),
        "U32" => (4, ValKind::Unsigned, true),
        "s32" => (4, ValKind::Signed, false),
        "S32" => (4, ValKind::Signed, true),
        "u64" => (8, ValKind::Unsigned, false),
        "U64" => (8, ValKind::Unsigned, true),
        "s64" => (8, ValKind::Signed, false),
        "S64" => (8, ValKind::Signed, true),
        "f32" => (4, ValKind::Float, false),
        "F32" => (4, ValKind::Float, true),
        "f64" => (8, ValKind::Float, false),
        "F64" => (8, ValKind::Float, true),
        _ => return None,
    })
}

fn value_type_arg(v: Value) -> Result<(usize, ValKind, bool)> {
    let name = match v.try_symbol_or_string() {
        Some(id) => id.get_name(),
        None => return Err(MonorubyErr::argumenterr("Invalid type name!")),
    };
    parse_value_type(&name).ok_or_else(|| MonorubyErr::argumenterr("Invalid type name!"))
}

fn decode_value(bytes: &[u8], kind: ValKind, big: bool) -> Value {
    let width = bytes.len();
    let mut buf = [0u8; 8];
    buf[..width].copy_from_slice(bytes);
    let raw = if big {
        let mut b = [0u8; 8];
        b[8 - width..].copy_from_slice(bytes);
        u64::from_be_bytes(b)
    } else {
        u64::from_le_bytes(buf)
    };
    match kind {
        ValKind::Unsigned => Value::integer_from_u64(raw & mask(width)),
        ValKind::Signed => {
            let shift = 64 - width as u32 * 8;
            Value::integer(((raw << shift) as i64) >> shift)
        }
        ValKind::Float => match width {
            4 => Value::float(f32::from_bits(raw as u32) as f64),
            _ => Value::float(f64::from_bits(raw)),
        },
    }
}

fn mask(width: usize) -> u64 {
    if width == 8 {
        u64::MAX
    } else {
        (1u64 << (width * 8)) - 1
    }
}

fn encode_value(
    vm: &mut Executor,
    globals: &mut Globals,
    v: Value,
    width: usize,
    kind: ValKind,
    big: bool,
) -> Result<[u8; 8]> {
    let raw: u64 = match kind {
        ValKind::Float => {
            let f = if let Some(f) = v.try_float() {
                f
            } else if matches!(v.unpack(), RV::Fixnum(_) | RV::BigInt(_)) {
                v.coerce_to_int_i64(vm, globals)? as f64
            } else {
                // CRuby's rb_to_float message, with the lowercased
                // class word ("string", "nil", ...).
                let desc = if v.is_nil() {
                    "nil".to_string()
                } else {
                    v.get_real_class_name(globals).to_lowercase()
                };
                return Err(MonorubyErr::typeerr(format!(
                    "no implicit conversion to float from {desc}"
                )));
            };
            if width == 4 {
                (f as f32).to_bits() as u64
            } else {
                f.to_bits()
            }
        }
        _ => (v.coerce_to_int_i64(vm, globals)? as u64) & mask(width),
    };
    let mut out = [0u8; 8];
    if big {
        out[..width].copy_from_slice(&raw.to_be_bytes()[8 - width..]);
    } else {
        out[..width].copy_from_slice(&raw.to_le_bytes()[..width]);
    }
    Ok(out)
}

///
/// ### IO::Buffer#get_value
///
/// - get_value(type, offset) -> Integer | Float
///
#[monoruby_builtin]
fn get_value(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let (width, kind, big) = value_type_arg(lfp.arg(0))?;
    let offset = strict_index_arg(vm, globals, lfp.arg(1), "Offset can't be negative!")?;
    let buf = lfp.self_val().as_iobuffer();
    if offset + width > buf.size {
        return Err(MonorubyErr::argumenterr(format!(
            "Type extends beyond end of buffer! (offset={} > size={})",
            offset, buf.size
        )));
    }
    let bytes = buf.read_bytes()?;
    Ok(decode_value(&bytes[offset..offset + width], kind, big))
}

///
/// ### IO::Buffer#set_value
///
/// - set_value(type, offset, value) -> Integer (bytes written)
///
#[monoruby_builtin]
fn set_value(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut buf = lfp.self_val().as_iobuffer();
    check_writable(globals, &buf)?;
    let (width, kind, big) = value_type_arg(lfp.arg(0))?;
    let offset = strict_index_arg(vm, globals, lfp.arg(1), "Offset can't be negative!")?;
    if offset + width > buf.size {
        return Err(MonorubyErr::argumenterr(format!(
            "Type extends beyond end of buffer! (offset={} > size={})",
            offset, buf.size
        )));
    }
    let encoded = encode_value(vm, globals, lfp.arg(2), width, kind, big)?;
    buf.write_at(offset, &encoded[..width])?;
    // CRuby returns the offset just past the written value.
    Ok(Value::integer((offset + width) as i64))
}

/// Iterate `(absolute_offset, decoded_value)` pairs of `type` starting at
/// `offset` for `count` items (or to the end of the buffer).
fn each_values(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Vec<(usize, Value)>> {
    let buf = lfp.self_val().as_iobuffer();
    let (width, kind, big) = value_type_arg(lfp.arg(0))?;
    let mut offset = match lfp.try_arg(1) {
        Some(v) => strict_index_arg(vm, globals, v, "Offset can't be negative!")?,
        None => 0,
    };
    let count = match lfp.try_arg(2) {
        Some(v) if !v.is_nil() => Some(strict_index_arg(vm, globals, v, "Count can't be negative!")?),
        _ => None,
    };
    let bytes = buf.read_bytes()?;
    if offset > bytes.len() {
        return Err(MonorubyErr::argumenterr(
            "The given offset is bigger than the buffer size!",
        ));
    }
    let mut out = Vec::new();
    match count {
        // An explicit count reads exactly that many items; running past
        // the end is an error (CRuby validates each read), while the
        // default form stops at the last whole item.
        Some(c) => {
            for _ in 0..c {
                if offset + width > bytes.len() {
                    return Err(MonorubyErr::argumenterr(format!(
                        "Type extends beyond end of buffer! (offset={} > size={})",
                        offset,
                        bytes.len()
                    )));
                }
                out.push((
                    offset,
                    decode_value(&bytes[offset..offset + width], kind, big),
                ));
                offset += width;
            }
        }
        None => {
            while offset + width <= bytes.len() {
                out.push((
                    offset,
                    decode_value(&bytes[offset..offset + width], kind, big),
                ));
                offset += width;
            }
        }
    }
    Ok(out)
}

///
/// ### IO::Buffer#each
///
/// - each(type, offset = 0, count = nil) {|offset, value| ... } -> self
/// - each(type, offset = 0, count = nil) -> Enumerator
///
#[monoruby_builtin]
fn each(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let Some(bh) = lfp.block() else {
        let mut args = vec![lfp.arg(0)];
        for i in 1..3 {
            if let Some(v) = lfp.try_arg(i) {
                args.push(v);
            }
        }
        return vm.generate_enumerator(IdentId::get_id("each"), lfp.self_val(), args, pc);
    };
    let pairs = each_values(vm, globals, lfp)?;
    let p = vm.get_block_data(globals, bh)?;
    for (off, v) in pairs {
        vm.invoke_block(globals, &p, &[Value::integer(off as i64), v])?;
    }
    Ok(lfp.self_val())
}

///
/// ### IO::Buffer#values
///
/// - values(type, offset = 0, count = nil) -> Array
///
#[monoruby_builtin]
fn values(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let pairs = each_values(vm, globals, lfp)?;
    Ok(Value::array_from_vec(
        pairs.into_iter().map(|(_, v)| v).collect(),
    ))
}

///
/// ### IO::Buffer#each_byte
///
/// - each_byte(offset = 0, count = nil) {|byte| ... } -> self
/// - each_byte(offset = 0, count = nil) -> Enumerator
///
#[monoruby_builtin]
fn each_byte(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let Some(bh) = lfp.block() else {
        let mut args = vec![];
        for i in 0..2 {
            if let Some(v) = lfp.try_arg(i) {
                args.push(v);
            }
        }
        return vm.generate_enumerator(IdentId::get_id("each_byte"), self_, args, pc);
    };
    let offset = match lfp.try_arg(0) {
        Some(v) => strict_index_arg(vm, globals, v, "Offset can't be negative!")?,
        None => 0,
    };
    let count = match lfp.try_arg(1) {
        Some(v) if !v.is_nil() => Some(strict_index_arg(vm, globals, v, "Count can't be negative!")?),
        _ => None,
    };
    let bytes = self_.as_iobuffer_inner().read_bytes()?;
    if offset > bytes.len() {
        return Err(MonorubyErr::argumenterr(
            "The given offset is bigger than the buffer size!",
        ));
    }
    // An overlong count clamps (unlike #each/#values, which validate).
    let end = count
        .map(|c| offset.saturating_add(c).min(bytes.len()))
        .unwrap_or(bytes.len());
    let p = vm.get_block_data(globals, bh)?;
    for &b in bytes.get(offset..end).unwrap_or(&[]) {
        vm.invoke_block(globals, &p, &[Value::integer(b as i64)])?;
    }
    Ok(self_)
}

/// Allocator: a null buffer; #initialize fills it in.
pub(crate) extern "C" fn io_buffer_alloc_func(class_id: ClassId, _: &mut Globals) -> Value {
    let mut v = Value::new_io_buffer(IoBufferInner::null());
    v.change_class(class_id);
    v
}

fn buffer_err(globals: &mut Globals, name: &str, msg: &str) -> MonorubyErr {
    // IO::Buffer's error classes are defined in startup.rb; raise by
    // constant lookup so `rescue IO::Buffer::LockedError` works.
    let id = IdentId::get_id(name);
    if let Some(class) = globals
        .store
        .get_constant_noautoload(IO_BUFFER_CLASS, id)
        .and_then(|v| v.is_class())
    {
        MonorubyErr::new(MonorubyErrKind::Other(class.id()), msg)
    } else {
        MonorubyErr::runtimeerr(msg)
    }
}

fn locked_err(globals: &mut Globals, msg: &str) -> MonorubyErr {
    buffer_err(globals, "LockedError", msg)
}

fn access_err(globals: &mut Globals, msg: &str) -> MonorubyErr {
    buffer_err(globals, "AccessError", msg)
}

/// A slice whose source no longer covers its span (freed, transferred,
/// or shrunk) raises InvalidatedError on data access.
fn ensure_view_valid(globals: &mut Globals, buf: &IoBufferInner) -> Result<()> {
    if buf.read_bytes().is_err() {
        return Err(buffer_err(
            globals,
            "InvalidatedError",
            "Buffer has been invalidated!",
        ));
    }
    Ok(())
}

fn check_writable(globals: &mut Globals, buf: &IoBufferInner) -> Result<()> {
    if buf.flags & BUF_READONLY != 0 {
        return Err(access_err(globals, "Buffer is not writable!"));
    }
    Ok(())
}

///
/// ### IO::Buffer#initialize
///
/// - new(size = DEFAULT_SIZE, flags = nil) -> IO::Buffer
///
#[monoruby_builtin]
fn initialize(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let size = match lfp.try_arg(0) {
        None => 65536,
        Some(v) => {
            if !matches!(v.unpack(), RV::Fixnum(_) | RV::BigInt(_)) {
                return Err(MonorubyErr::typeerr("not an Integer"));
            }
            let s = v.coerce_to_int_i64(vm, globals)?;
            if s < 0 {
                return Err(MonorubyErr::argumenterr("Size can't be negative!"));
            }
            s as usize
        }
    };
    let flags = match lfp.try_arg(1) {
        Some(v) if !v.is_nil() => {
            if v.try_fixnum().is_none() {
                return Err(MonorubyErr::typeerr("not an Integer"));
            }
            let f = v.coerce_to_int_i64(vm, globals)?;
            if f < 0 {
                return Err(MonorubyErr::argumenterr("Flags can't be negative!"));
            }
            Some(f as u32)
        }
        _ => None,
    };

    let inner = if size == 0 {
        IoBufferInner::null()
    } else {
        let flags = match flags {
            None => {
                if size < page_size() {
                    BUF_INTERNAL
                } else {
                    BUF_MAPPED
                }
            }
            Some(f) => {
                if f & (BUF_INTERNAL | BUF_MAPPED) == 0 {
                    return Err(buffer_err(
                        globals,
                        "AllocationError",
                        "Could not allocate buffer!",
                    ));
                }
                f
            }
        };
        IoBufferInner::owned(vec![0u8; size], flags)
    };
    *lfp.self_val().as_iobuffer_inner_mut() = inner;
    Ok(lfp.self_val())
}

///
/// ### IO::Buffer.for
///
/// - for(string) -> IO::Buffer (readonly view)
/// - for(string) {|buffer| ... } -> block result (mutable view; the
///   string is locked for the duration of the block)
///
#[monoruby_builtin]
fn buffer_for(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let s = lfp.arg(0);
    let Some(rs) = s.is_rstring() else {
        return Err(MonorubyErr::no_implicit_conversion(
            globals,
            s,
            STRING_CLASS,
        ));
    };
    let size = rs.as_bytes().len();
    match lfp.block() {
        None => {
            // Without a block the buffer snapshots the string's bytes
            // (CRuby acquires a frozen copy): later mutation of the
            // original must not show through.
            let snapshot = RString::bytes(rs.as_bytes().to_vec());
            let inner = IoBufferInner::string_backed(snapshot, size, BUF_EXTERNAL | BUF_READONLY);
            Ok(Value::new_io_buffer(inner))
        }
        Some(bh) => {
            // Block form: writable unless the string is frozen. The
            // buffer is invalidated (nulled) when the block exits.
            let mut flags = BUF_EXTERNAL;
            if s.is_frozen() {
                flags |= BUF_READONLY;
            }
            let inner = IoBufferInner::string_backed(rs, size, flags);
            let buf_val = Value::new_io_buffer(inner);
            let result = vm.with_temp_scope(|vm| {
                vm.temp_push(buf_val);
                vm.invoke_block_once(globals, bh, &[buf_val])
            });
            let mut bv = buf_val;
            *bv.as_iobuffer_inner_mut() = IoBufferInner::null();
            result
        }
    }
}

///
/// ### IO::Buffer.string
///
/// - string(length) {|buffer| ... } -> String
///
/// Yields a zero-filled buffer of `length` bytes and returns its final
/// contents as a binary String.
#[monoruby_builtin]
fn buffer_string(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let len = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    if len < 0 {
        return Err(MonorubyErr::argumenterr(
            "negative string size (or size too big)",
        ));
    }
    let bh = lfp
        .block()
        .ok_or_else(|| MonorubyErr::localjumperr("no block given"))?;
    let mut string = RString::bytes(vec![0u8; len as usize]);
    let inner = IoBufferInner::string_backed(string, len as usize, BUF_EXTERNAL);
    let buf_val = Value::new_io_buffer(inner);
    vm.with_temp_scope(|vm| {
        vm.temp_push(string.into());
        vm.temp_push(buf_val);
        vm.invoke_block_once(globals, bh, &[buf_val])
    })?;
    let mut bv = buf_val;
    *bv.as_iobuffer_inner_mut() = IoBufferInner::null();
    string.set_encoding(crate::value::Encoding::Ascii8);
    Ok(string.into())
}

#[monoruby_builtin]
fn size(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::integer(
        lfp.self_val().as_iobuffer_inner().size as i64,
    ))
}

#[monoruby_builtin]
fn valid_(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // A buffer is "valid" when its view is safe to use: null buffers
    // (including freed/transferred ones) are trivially valid; a slice is
    // valid while it still fits its source.
    let self_ = lfp.self_val();
    let buf = self_.as_iobuffer_inner();
    let ok = match &buf.storage {
        BufStorage::Slice { .. } | BufStorage::Str { .. } => buf.read_bytes().is_ok(),
        _ => true,
    };
    Ok(Value::bool(ok))
}

#[monoruby_builtin]
fn null_(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_iobuffer().is_null()))
}

#[monoruby_builtin]
fn empty_(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_iobuffer().size == 0))
}

macro_rules! flag_predicate {
    ($name:ident, $flag:expr) => {
        #[monoruby_builtin]
        fn $name(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
            Ok(Value::bool(lfp.self_val().as_iobuffer().flags & $flag != 0))
        }
    };
}
flag_predicate!(external_, BUF_EXTERNAL);
flag_predicate!(internal_, BUF_INTERNAL);
flag_predicate!(mapped_, BUF_MAPPED);
flag_predicate!(shared_, BUF_SHARED);
flag_predicate!(private_, BUF_PRIVATE);
flag_predicate!(readonly_, BUF_READONLY);

#[monoruby_builtin]
fn locked_(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_iobuffer().locked))
}

///
/// ### IO::Buffer#locked
///
/// - locked {|buffer| ... } -> block result
///
#[monoruby_builtin]
fn locked_block(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut buf = lfp.self_val().as_iobuffer();
    if buf.locked {
        return Err(locked_err(globals, "Buffer already locked!"));
    }
    let bh = lfp
        .block()
        .ok_or_else(|| MonorubyErr::localjumperr("no block given (yield)"))?;
    buf.locked = true;
    let result = vm.invoke_block_once(globals, bh, &[buf.into()]);
    // CRuby only unlocks on a normal return: an exception raised inside
    // the block leaves the buffer locked.
    if result.is_ok() {
        buf.locked = false;
    }
    result
}

/// Resolve (offset, length) arguments against the buffer size with
/// CRuby's error messages.
fn offset_length(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    size: usize,
    off_idx: usize,
    len_idx: usize,
) -> Result<(usize, usize)> {
    let offset = match lfp.try_arg(off_idx) {
        Some(v) if !v.is_nil() => {
            let o = v.coerce_to_int_i64(vm, globals)?;
            if o < 0 {
                return Err(MonorubyErr::argumenterr("Offset can't be negative!"));
            }
            o as usize
        }
        _ => 0,
    };
    if offset > size {
        return Err(MonorubyErr::argumenterr(
            "The given offset is bigger than the buffer size!",
        ));
    }
    let length = match lfp.try_arg(len_idx) {
        Some(v) if !v.is_nil() => {
            let l = v.coerce_to_int_i64(vm, globals)?;
            if l < 0 {
                return Err(MonorubyErr::argumenterr("Length can't be negative!"));
            }
            l as usize
        }
        _ => size - offset,
    };
    if offset + length > size {
        return Err(MonorubyErr::argumenterr(
            "Specified offset+length is bigger than the buffer size!",
        ));
    }
    Ok((offset, length))
}

///
/// ### IO::Buffer#get_string
///
/// - get_string(offset = 0, length = nil, encoding = Encoding::BINARY) -> String
///
#[monoruby_builtin]
fn get_string(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let buf = lfp.self_val().as_iobuffer();
    ensure_view_valid(globals, &buf)?;
    let (offset, length) = offset_length(vm, globals, lfp, buf.size, 0, 1)?;
    let bytes = {
        let all = buf.read_bytes()?;
        all[offset..offset + length].to_vec()
    };
    let mut s = RString::bytes(bytes);
    // An Encoding object or an encoding name String; CRuby silently
    // falls back to BINARY for a name it does not know.
    let enc = match lfp.try_arg(2) {
        Some(v) if !v.is_nil() => match v.is_str() {
            Some(name) => crate::value::Encoding::try_from_str(name).ok(),
            None => super::io::enc_obj_to_enum(globals, v),
        },
        _ => Some(crate::value::Encoding::Ascii8),
    };
    s.set_encoding(enc.unwrap_or(crate::value::Encoding::Ascii8));
    Ok(s.into())
}

///
/// ### IO::Buffer#set_string
///
/// - set_string(string, offset = 0, length = nil, source_offset = 0) -> Integer
///
/// Same argument shape and error set as #copy with the string as the
/// source. CRuby's check order: string conversion and strict index
/// extraction first, then writability, then the source-side range
/// checks, then the destination bounds.
#[monoruby_builtin]
fn set_string(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let src = lfp.arg(0).coerce_to_rstring(vm, globals)?;
    let offset = match lfp.try_arg(1) {
        Some(v) => strict_index_arg(vm, globals, v, "Offset can't be negative!")?,
        None => 0,
    };
    let length = match lfp.try_arg(2) {
        Some(v) if !v.is_nil() => {
            Some(strict_index_arg(vm, globals, v, "Length can't be negative!")?)
        }
        _ => None,
    };
    let source_offset = match lfp.try_arg(3) {
        Some(v) => strict_index_arg(vm, globals, v, "Offset can't be negative!")?,
        None => 0,
    };

    let mut buf = lfp.self_val().as_iobuffer();
    check_writable(globals, &buf)?;
    ensure_view_valid(globals, &buf)?;

    let src_bytes = src.as_bytes();
    let source_size = src_bytes.len();
    if source_offset > source_size {
        return Err(MonorubyErr::argumenterr(
            "The given source offset is bigger than the source itself!",
        ));
    }
    let length = match length {
        Some(l) => {
            if source_offset.checked_add(l).is_none_or(|e| e > source_size) {
                return Err(MonorubyErr::argumenterr(
                    "The computed source range exceeds the size of the source buffer!",
                ));
            }
            l
        }
        None => source_size - source_offset,
    };
    if offset.checked_add(length).is_none_or(|e| e > buf.size) {
        return Err(MonorubyErr::argumenterr(
            "Specified offset+length is bigger than the buffer size!",
        ));
    }
    // The source string can be the buffer's own backing store
    // (`IO::Buffer.for(str) { |b| b.set_string(str, ...) }`); snapshot
    // that aliasing case, like #copy, so `write_at`'s `&mut` never
    // overlaps the borrowed source bytes.
    if src_bytes.as_ptr() as usize == buf.storage_root() {
        let data = src_bytes[source_offset..source_offset + length].to_vec();
        buf.write_at(offset, &data)?;
    } else {
        let data = &src_bytes[source_offset..source_offset + length];
        buf.write_at(offset, data)?;
    }
    Ok(Value::integer(length as i64))
}

/// Strict Integer extraction for offset/length/count arguments
/// (#copy, #set_string, #get_value/#set_value, #each/#values/#each_byte):
/// CRuby's io_buffer_extract_* take Integers only (no nil, no #to_int),
/// and reject negatives with a per-argument message.
fn strict_index_arg(
    vm: &mut Executor,
    globals: &mut Globals,
    v: Value,
    neg_msg: &str,
) -> Result<usize> {
    if !matches!(v.unpack(), RV::Fixnum(_) | RV::BigInt(_)) {
        return Err(MonorubyErr::typeerr("not an Integer"));
    }
    let n = v.coerce_to_int_i64(vm, globals)?;
    if n < 0 {
        return Err(MonorubyErr::argumenterr(neg_msg));
    }
    Ok(n as usize)
}

///
/// ### IO::Buffer#initialize_copy (the #dup / #clone hook)
///
/// CRuby materializes a writable INTERNAL copy of the source's bytes: a
/// mapped, external, readonly, or slice source all dup to a plain
/// internal buffer of the same size; a zero-size source stays null;
/// locked state does not transfer (and a locked source may be duped).
#[monoruby_builtin]
fn initialize_copy(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let src = lfp.arg(0).as_iobuffer();
    let inner = if src.size == 0 {
        IoBufferInner::null()
    } else {
        IoBufferInner::owned(src.read_bytes()?.to_vec(), BUF_INTERNAL)
    };
    *self_.as_iobuffer_inner_mut() = inner;
    Ok(self_)
}

///
/// ### IO::Buffer#copy
///
/// - copy(source, offset = 0, length = nil, source_offset = 0) -> Integer
///
/// Copies `length` bytes (default: the rest of `source` past
/// `source_offset`) out of the `source` buffer into this buffer at
/// `offset`, returning the byte count. The source span is snapshotted
/// before writing, so overlapping self-copies behave like CRuby's
/// memmove. CRuby's check order is kept: source-side extraction and
/// range errors come before the writability check, which comes before
/// the destination bounds check; a locked buffer may still be written.
#[monoruby_builtin]
fn copy(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let source_v = lfp.arg(0);
    let Some(source) = source_v.try_iobuffer_inner() else {
        // CRuby renders nil as "nil", not "NilClass", here.
        let name = if source_v.is_nil() {
            "nil".to_string()
        } else {
            source_v.get_real_class_name(globals)
        };
        return Err(MonorubyErr::typeerr(format!(
            "wrong argument type {name} (expected IO::Buffer)"
        )));
    };
    let offset = match lfp.try_arg(1) {
        Some(v) => strict_index_arg(vm, globals, v, "Offset can't be negative!")?,
        None => 0,
    };
    let length = match lfp.try_arg(2) {
        Some(v) if !v.is_nil() => {
            Some(strict_index_arg(vm, globals, v, "Length can't be negative!")?)
        }
        _ => None,
    };
    let source_offset = match lfp.try_arg(3) {
        Some(v) => strict_index_arg(vm, globals, v, "Offset can't be negative!")?,
        None => 0,
    };

    let source_size = source.size;
    if source_offset > source_size {
        return Err(MonorubyErr::argumenterr(
            "The given source offset is bigger than the source itself!",
        ));
    }
    let length = match length {
        Some(l) => {
            if source_offset.checked_add(l).is_none_or(|e| e > source_size) {
                return Err(MonorubyErr::argumenterr(
                    "The computed source range exceeds the size of the source buffer!",
                ));
            }
            l
        }
        None => source_size - source_offset,
    };

    let mut buf = lfp.self_val().as_iobuffer();
    check_writable(globals, &buf)?;
    if offset.checked_add(length).is_none_or(|e| e > buf.size) {
        return Err(MonorubyErr::argumenterr(
            "Specified offset+length is bigger than the buffer size!",
        ));
    }

    // The common case — distinct buffers — copies straight from the
    // source's span. Only when both spans resolve to the same
    // allocation (a self-copy, slices of one buffer, views of one
    // String) can they overlap; snapshotting just that case keeps
    // memmove's overlap semantics without aliasing `write_at`'s
    // `&mut` with a borrow of the same bytes (which is UB — it
    // miscompiled to an infinite loop).
    if source.storage_root() == buf.storage_root() {
        let data = source.read_bytes()?[source_offset..source_offset + length].to_vec();
        buf.write_at(offset, &data)?;
    } else {
        let data = &source.read_bytes()?[source_offset..source_offset + length];
        buf.write_at(offset, data)?;
    }
    Ok(Value::integer(length as i64))
}

///
/// ### IO::Buffer#free
///
/// - free -> self
///
#[monoruby_builtin]
fn free(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut buf = lfp.self_val().as_iobuffer();
    if buf.locked {
        return Err(locked_err(globals, "Buffer is locked!"));
    }
    *buf = IoBufferInner::null();
    Ok(buf.into())
}

///
/// ### IO::Buffer#transfer
///
/// - transfer -> IO::Buffer
///
/// Moves the storage into a fresh buffer; self becomes null.
#[monoruby_builtin]
fn transfer(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_ = lfp.self_val();
    if self_.as_iobuffer_inner().locked {
        return Err(locked_err(
            globals,
            "Cannot transfer ownership of locked buffer!",
        ));
    }
    let inner = std::mem::replace(self_.as_iobuffer_inner_mut(), IoBufferInner::null());
    // The nulled original keeps its flag bits (CRuby: "+0 NULL INTERNAL");
    // #free, by contrast, clears them.
    self_.as_iobuffer_inner_mut().flags = inner.flags;
    Ok(Value::new_io_buffer(inner))
}

///
/// ### IO::Buffer#resize
///
/// - resize(new_size) -> self
///
#[monoruby_builtin]
fn resize(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut buf = lfp.self_val().as_iobuffer();
    if buf.locked {
        return Err(locked_err(globals, "Cannot resize locked buffer!"));
    }
    if !matches!(lfp.arg(0).unpack(), RV::Fixnum(_) | RV::BigInt(_)) {
        return Err(MonorubyErr::typeerr("not an Integer"));
    }
    let new_size = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    if new_size < 0 {
        return Err(MonorubyErr::argumenterr("Size can't be negative!"));
    }
    let new_size = new_size as usize;
    match &buf.storage {
        BufStorage::Str { .. } | BufStorage::Slice { .. } => {
            return Err(access_err(globals, "Cannot resize external buffer!"));
        }
        // A shared file mapping cannot be resized; a PRIVATE one can
        // (its pages are copy-on-write and never written back anyway).
        BufStorage::FileMap { .. } if buf.flags & BUF_PRIVATE == 0 => {
            return Err(access_err(globals, "Cannot resize external buffer!"));
        }
        _ => {}
    }
    let mut bytes = buf.read_bytes()?.to_vec();
    bytes.resize(new_size, 0);
    let flags = if buf.is_null() {
        if new_size < page_size() {
            BUF_INTERNAL
        } else {
            BUF_MAPPED
        }
    } else {
        buf.flags
    };
    *buf = if new_size == 0 {
        IoBufferInner::null()
    } else {
        IoBufferInner::owned(bytes, flags)
    };
    Ok(buf.into())
}

///
/// ### IO::Buffer#slice
///
/// - slice(offset = 0, length = nil) -> IO::Buffer
///
/// A view sharing this buffer's bytes (writes go through), with an
/// independent lock state.
#[monoruby_builtin]
fn slice(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let buf = lfp.self_val().as_iobuffer();
    let (offset, length) = offset_length(vm, globals, lfp, buf.size, 0, 1)?;
    // A slice inherits only the passthrough flags (readonly), not the
    // storage-kind flags — matching CRuby, where `slice.internal?` is
    // false even for a slice of an internal buffer.
    let flags = buf.flags & BUF_READONLY;
    // A slice of a null buffer is itself null; a 0-length slice of a
    // live buffer is a real (non-null) view.
    if buf.is_null() {
        return Ok(Value::new_io_buffer(IoBufferInner::null()));
    }
    // Slices of string-backed buffers reference the String directly, so
    // they outlive a #free of the source buffer (while the String lives).
    if let BufStorage::Str { s, offset: base } = &buf.storage {
        return Ok(Value::new_io_buffer(IoBufferInner::string_backed_at(
            *s,
            base + offset,
            length,
            flags,
        )));
    }
    Ok(Value::new_io_buffer(IoBufferInner::slice_of(
        buf, offset, length, flags,
    )))
}

///
/// ### IO::Buffer#clear
///
/// - clear(value = 0, offset = 0, length = nil) -> self
///
#[monoruby_builtin]
fn clear(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut buf = lfp.self_val().as_iobuffer();
    check_writable(globals, &buf)?;
    let value = match lfp.try_arg(0) {
        Some(v) if !v.is_nil() => (v.coerce_to_int_i64(vm, globals)? & 0xff) as u8,
        _ => 0,
    };
    let size = buf.size;
    let (offset, length) = offset_length(vm, globals, lfp, size, 1, 2)?;
    buf.write_at(offset, &vec![value; length])?;
    Ok(buf.into())
}

/// Render the CRuby single-line description: address, size, flag names.
fn describe(buf: &IoBufferInner) -> String {
    /// The address shown in #to_s / #inspect is the *storage* base pointer
    /// (CRuby prints the mapped/allocated base): #transfer moves the storage
    /// to a new object, and transfer_spec asserts the description string —
    /// address included — carries over.
    fn buffer_addr(buf: &IoBufferInner) -> u64 {
        match &buf.storage {
            BufStorage::Owned(vec) => {
                if vec.is_empty() {
                    0
                } else {
                    vec.as_ptr() as u64
                }
            }
            BufStorage::FileMap { ptr, .. } => *ptr as u64,
            BufStorage::Str { s, offset } => s.as_bytes().as_ptr() as u64 + *offset as u64,
            BufStorage::Slice { parent, offset } => buffer_addr(parent) + *offset as u64,
        }
    }
    let addr = buffer_addr(buf);
    let mut s = format!("#<IO::Buffer 0x{:016x}+{}", addr, buf.size);
    if buf.is_null() {
        s.push_str(" NULL");
    }
    {
        if buf.flags & BUF_EXTERNAL != 0 {
            s.push_str(" EXTERNAL");
        }
        if buf.flags & BUF_INTERNAL != 0 {
            s.push_str(" INTERNAL");
        }
        if buf.flags & BUF_MAPPED != 0 {
            s.push_str(" MAPPED");
        }
        if matches!(buf.storage, BufStorage::FileMap { .. }) {
            s.push_str(" FILE");
        }
        if buf.flags & BUF_SHARED != 0 {
            s.push_str(" SHARED");
        }
        if buf.locked {
            s.push_str(" LOCKED");
        }
        if buf.flags & BUF_PRIVATE != 0 {
            s.push_str(" PRIVATE");
        }
        if buf.flags & BUF_READONLY != 0 {
            s.push_str(" READONLY");
        }
        if matches!(
            buf.storage,
            BufStorage::Str { .. } | BufStorage::Slice { .. }
        ) {
            s.push_str(" SLICE");
        }
    }
    s.push('>');
    s
}

#[monoruby_builtin]
fn to_s(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let buf = self_.as_iobuffer();
    Ok(Value::string(describe(&buf)))
}

fn hexdump_string(bytes: &[u8]) -> String {
    let mut out = String::new();
    for (i, chunk) in bytes.chunks(16).enumerate() {
        if i > 0 || !out.is_empty() {
            out.push('\n');
        }
        let hex: Vec<String> = chunk.iter().map(|b| format!("{:02x}", b)).collect();
        let ascii: String = chunk
            .iter()
            .map(|&b| {
                if (0x20..0x7f).contains(&b) {
                    b as char
                } else {
                    '.'
                }
            })
            .collect();
        out.push_str(&format!("0x{:08x}  {:<48}{}", i * 16, hex.join(" "), ascii));
    }
    out
}

#[monoruby_builtin]
fn hexdump(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let buf = self_.as_iobuffer_inner();
    let bytes = buf.read_bytes()?;
    Ok(Value::string(hexdump_string(&bytes)))
}

#[monoruby_builtin]
fn inspect(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let buf = self_.as_iobuffer_inner();
    let mut s = describe(buf);
    if !buf.is_null() {
        let bytes = buf.read_bytes()?;
        if !bytes.is_empty() {
            s.push('\n');
            s.push_str(&hexdump_string(&bytes));
        }
    }
    Ok(Value::string(s))
}

#[monoruby_builtin]
fn cmp(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let other = lfp.arg(0);
    if other
        .try_rvalue()
        .is_none_or(|rv| rv.ty() != ObjTy::IO_BUFFER)
    {
        return Err(MonorubyErr::typeerr(format!(
            "wrong argument type {} (expected IO::Buffer)",
            other.get_real_class_name(globals)
        )));
    }
    let a = self_.as_iobuffer_inner().read_bytes()?;
    let b = other.as_iobuffer_inner().read_bytes()?;
    let ord = a.cmp(&b);
    Ok(Value::integer(match ord {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    }))
}

///
/// ### IO::Buffer.map
///
/// - map(file, size = nil, offset = 0, flags = 0) -> IO::Buffer
///
/// Maps `file` via mmap(2): MAP_SHARED (writes reach the file and other
/// processes) unless PRIVATE is requested, PROT_READ-only when READONLY
/// is requested. The mapping is released when the buffer is freed,
/// transferred away, or collected.
#[monoruby_builtin]
fn buffer_map(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let io_v = lfp.arg(0);
    let fd = if io_v.try_rvalue().is_some_and(|rv| rv.ty() == ObjTy::IO) {
        io_v.as_io_inner().fileno()?
    } else if globals
        .check_method(io_v, IdentId::get_id("fileno"))
        .is_some()
    {
        vm.invoke_method_inner(globals, IdentId::get_id("fileno"), io_v, &[], None, None)?
            .coerce_to_int_i64(vm, globals)? as i32
    } else {
        return Err(MonorubyErr::typeerr(format!(
            "expected IO or #fileno, {} given",
            io_v.get_real_class_name(globals)
        )));
    };

    let offset = match lfp.try_arg(2) {
        Some(v) => {
            // An explicit nil (or any non-Integer) offset is a TypeError;
            // only an absent argument defaults to 0.
            let o = v.coerce_to_int_i64(vm, globals)?;
            if o < 0 {
                return Err(MonorubyErr::argumenterr("Offset can't be negative!"));
            }
            o
        }
        None => 0,
    };

    // SAFETY: fstat on a live fd with a valid out-pointer.
    let file_size = unsafe {
        let mut st: libc::stat = std::mem::zeroed();
        if libc::fstat(fd, &mut st) != 0 {
            let err = std::io::Error::last_os_error();
            return Err(MonorubyErr::errno_with_msg(
                &globals.store,
                &err,
                "io_buffer_map_file:fstat",
            ));
        }
        st.st_size
    };
    let size = match lfp.try_arg(1) {
        Some(v) if !v.is_nil() => {
            // Strict Integer (no #to_int / Float coercion), like CRuby.
            if !matches!(v.unpack(), RV::Fixnum(_) | RV::BigInt(_)) {
                return Err(MonorubyErr::typeerr("not an Integer"));
            }
            let s = v.coerce_to_int_i64(vm, globals)?;
            if s < 0 {
                return Err(MonorubyErr::argumenterr("Size can't be negative!"));
            }
            if s == 0 {
                return Err(MonorubyErr::argumenterr("Size can't be zero!"));
            }
            // A mapping reaching past EOF would SIGBUS on access beyond
            // the last file-backed page; CRuby rejects it up front, with
            // the size blamed when the size alone is too big and the
            // offset blamed otherwise.
            if s > file_size {
                return Err(MonorubyErr::argumenterr(
                    "Size can't be larger than file size!",
                ));
            }
            if s + offset > file_size {
                return Err(MonorubyErr::argumenterr("Offset too large!"));
            }
            s as usize
        }
        _ => {
            if file_size <= offset {
                return Err(MonorubyErr::argumenterr(
                    "Invalid negative or zero file size!",
                ));
            }
            (file_size - offset) as usize
        }
    };

    let req_flags = match lfp.try_arg(3) {
        Some(v) if !v.is_nil() => v.coerce_to_int_i64(vm, globals)? as u32,
        _ => 0,
    };
    let readonly = req_flags & BUF_READONLY != 0;
    let private = req_flags & BUF_PRIVATE != 0;

    let prot = if readonly {
        libc::PROT_READ
    } else {
        libc::PROT_READ | libc::PROT_WRITE
    };
    let map_flags = if private {
        libc::MAP_PRIVATE
    } else {
        libc::MAP_SHARED
    };
    // SAFETY: a fresh anonymous address is requested; fd/offset/size come
    // from the checks above. MAP_FAILED is handled.
    let ptr = unsafe {
        libc::mmap(
            std::ptr::null_mut(),
            size,
            prot,
            map_flags,
            fd,
            offset as libc::off_t,
        )
    };
    if ptr == libc::MAP_FAILED {
        let err = std::io::Error::last_os_error();
        return Err(MonorubyErr::errno_with_msg(
            &globals.store,
            &err,
            "io_buffer_map_file:mmap",
        ));
    }

    let mut flags = if private {
        BUF_MAPPED | BUF_PRIVATE
    } else {
        BUF_EXTERNAL | BUF_MAPPED | BUF_SHARED
    };
    if readonly {
        flags |= BUF_READONLY;
    }
    Ok(Value::new_io_buffer(IoBufferInner::file_map(
        ptr as *mut u8,
        size,
        flags,
    )))
}

/// Coerce the bitwise-op argument to a non-empty mask byte vector with
/// CRuby's messages ("nil", not "NilClass", in the TypeError).
fn mask_arg<'a>(globals: &mut Globals, v: &'a Value) -> Result<&'a [u8]> {
    let Some(buf) = v.try_iobuffer_inner() else {
        let name = if v.is_nil() {
            "nil".to_string()
        } else {
            v.get_real_class_name(globals)
        };
        return Err(MonorubyErr::typeerr(format!(
            "wrong argument type {} (expected IO::Buffer)",
            name
        )));
    };
    let mask = buf.read_bytes()?;
    if mask.is_empty() {
        return Err(buffer_err(globals, "MaskError", "Zero-length mask given!"));
    }
    Ok(mask)
}

fn bitwise_bytes(bytes: &[u8], mask: &[u8], op: impl Fn(u8, u8) -> u8) -> Vec<u8> {
    // The mask is applied cyclically: extra mask bytes are ignored, a
    // short mask repeats.
    bytes
        .iter()
        .enumerate()
        .map(|(i, b)| op(*b, mask[i % mask.len()]))
        .collect()
}

macro_rules! bitwise_new {
    ($name:ident, $op:expr) => {
        #[monoruby_builtin]
        fn $name(
            _: &mut Executor,
            globals: &mut Globals,
            lfp: Lfp,
            _: BytecodePtr,
        ) -> Result<Value> {
            let arg0 = lfp.arg(0);
            let mask = mask_arg(globals, &arg0)?;
            let buf = lfp.self_val().as_iobuffer();
            let bytes = buf.read_bytes()?;
            let out = bitwise_bytes(&bytes, &mask, $op);
            Ok(Value::new_io_buffer(IoBufferInner::owned(
                out,
                BUF_INTERNAL,
            )))
        }
    };
}
bitwise_new!(bit_and, |a, b| a & b);
bitwise_new!(bit_or, |a, b| a | b);
bitwise_new!(bit_xor, |a, b| a ^ b);

macro_rules! bitwise_inplace {
    ($name:ident, $op:expr) => {
        #[monoruby_builtin]
        fn $name(
            _: &mut Executor,
            globals: &mut Globals,
            lfp: Lfp,
            _: BytecodePtr,
        ) -> Result<Value> {
            let mut self_ = lfp.self_val();
            let arg0 = lfp.arg(0);
            // CRuby validates the mask (type, then emptiness) before the
            // writability of the receiver.
            let mask = mask_arg(globals, &arg0)?;
            check_writable(globals, self_.as_iobuffer_inner())?;
            let bytes = self_.as_iobuffer_inner().read_bytes()?;
            let out = bitwise_bytes(&bytes, &mask, $op);
            self_.as_iobuffer_inner_mut().write_at(0, &out)?;
            Ok(self_)
        }
    };
}
bitwise_inplace!(bit_and_inplace, |a, b| a & b);
bitwise_inplace!(bit_or_inplace, |a, b| a | b);
bitwise_inplace!(bit_xor_inplace, |a, b| a ^ b);

#[monoruby_builtin]
fn bit_not(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let buf = lfp.self_val().as_iobuffer();
    let bytes = buf.read_bytes()?;
    let out: Vec<u8> = bytes.iter().map(|b| !b).collect();
    Ok(Value::new_io_buffer(IoBufferInner::owned(
        out,
        BUF_INTERNAL,
    )))
}

#[monoruby_builtin]
fn bit_not_inplace(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut buf = lfp.self_val().as_iobuffer();
    check_writable(globals, &buf)?;
    let bytes = buf.read_bytes()?;
    let out: Vec<u8> = bytes.iter().map(|b| !b).collect();
    buf.write_at(0, &out)?;
    Ok(buf.into())
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn io_buffer_map_and_bitwise() {
        // .map: mmap-backed buffers (FILE flag word, write-through to the
        // file, size/offset/flags forms, READONLY/PRIVATE, EACCES for a
        // read-only fd without READONLY, EINVAL for a non-page-aligned
        // offset, ArgumentError for an empty file) and the bitwise family
        // (cyclic mask, TypeError/MaskError/AccessError ordering).
        run_test_once(
            r##"
            require "tempfile"
            r = []
            er = ->(&blk) { begin; blk.call; :no_raise; rescue => e; [e.class, e.message]; end }
            t = Tempfile.new("mrb_map"); File.write(t.path, "abcdefghij")
            f = File.open(t.path, "r+")
            b = IO::Buffer.map(f)
            r << [b.size, b.get_string, b.mapped?, b.external?, b.shared?, b.private?, b.readonly?]
            r << b.to_s.sub(/0x\h+/, "0xA")
            b.set_string("XY")
            r << File.read(t.path)
            b.free
            b2 = IO::Buffer.map(f, 4); r << [b2.size, b2.get_string]; b2.free
            b3 = IO::Buffer.map(f, 4, 0, IO::Buffer::READONLY)
            r << [b3.readonly?, er.call { b3.set_string("z") }]; b3.free
            b4 = IO::Buffer.map(f, nil, 0, IO::Buffer::PRIVATE)
            r << [b4.private?, b4.shared?]
            b4.set_string("QQ"); r << File.read(t.path); b4.free
            f.close
            f2 = File.open(t.path, "r")
            r << (begin; IO::Buffer.map(f2); :no_raise; rescue => e; e.class.ancestors.include?(SystemCallError); end)
            b5 = IO::Buffer.map(f2, nil, 0, IO::Buffer::READONLY); r << b5.get_string(0, 2); b5.free
            f2.close
            t2 = Tempfile.new("mrb_empty"); f3 = File.open(t2.path, "w+")
            r << er.call { IO::Buffer.map(f3) }
            f3.close; t2.close!
            # Page size is platform-dependent (16 KiB on Apple Silicon),
            # so align the offset to the real page size.
            File.write(t.path, "z" * (IO::Buffer::PAGE_SIZE + 904))
            f4 = File.open(t.path, "r+")
            b6 = IO::Buffer.map(f4, nil, IO::Buffer::PAGE_SIZE); r << [b6.size, b6.get_string(0, 3)]; b6.free
            r << (begin; IO::Buffer.map(f4, nil, 100); :no_raise; rescue => e; e.class.ancestors.include?(SystemCallError); end)
            f4.close; t.close!
            r << er.call { IO::Buffer.map("str") }
            # bitwise family
            b = IO::Buffer.for("12345".b)
            m = IO::Buffer.for("\xF8\x8F".b)
            r1 = b & m; r << [r1.get_string, r1.internal?, r1.size]
            r << (b | m).get_string << (b ^ m).get_string
            r4 = ~b; r << [r4.get_string, r4.internal?] << b.get_string
            IO::Buffer.for(+"12345") { |bb| res = bb.and!(m); r << [res.equal?(bb), bb.get_string, bb.external?] }
            IO::Buffer.for(+"12345") { |bb| bb.or!(m); r << bb.get_string }
            IO::Buffer.for(+"12345") { |bb| bb.xor!(m); r << bb.get_string }
            IO::Buffer.for(+"12345") { |bb| bb.not!; r << bb.get_string }
            r << er.call { b & "x" } << er.call { b.and!(nil) }
            e = IO::Buffer.new(0)
            r << er.call { b & e } << er.call { b.and!(e) }
            r << er.call { IO::Buffer.for("abc").and!(m) }
            lk = IO::Buffer.new(2); lk.set_string("ab")
            lk.locked { lk.and!(m); r << lk.get_string }
            r
            "##,
        );
    }

    #[test]
    fn io_buffer_copy() {
        // Argument forms, memmove overlap semantics, slices on either
        // side, empty/freed sources, and CRuby's error classes, messages
        // and check ordering (source range before writability before
        // destination bounds; locked buffers stay writable).
        run_test_once(
            r##"
            r = []
            er = ->(&blk) { begin; blk.call; :no_raise; rescue => e; [e.class, e.message]; end }
            src = IO::Buffer.for("ABCDEFGH")
            b = IO::Buffer.new(16)
            r << b.copy(src) << b.get_string
            r << b.copy(src, 4) << b.get_string
            r << b.copy(src, 2, 3) << b.copy(src, 0, 3, 5) << b.get_string
            r << b.copy(src, 0, 0) << b.copy(src, 0, nil, 5)
            # overlap within the same buffer: memmove semantics
            o = IO::Buffer.new(10)
            o.set_string("0123456789")
            r << o.copy(o, 2, 6, 0) << o.get_string
            # slice as source and as destination (writes through to parent)
            sl = src.slice(2, 4)
            r << b.copy(sl) << b.get_string(0, 4)
            d = IO::Buffer.new(8)
            dsl = d.slice(2, 4)
            r << dsl.copy(src, 0, 3) << d.get_string
            # empty and freed sources copy 0 bytes; a zero-size destination
            # only fails when there is something to copy
            n = IO::Buffer.new(0)
            r << b.copy(n)
            f = IO::Buffer.new(4); f.free
            r << b.copy(f)
            r << er.call { n.copy(src) } << n.copy(src, 0, 0)
            # bounds and type errors
            r << er.call { b.copy(src, 0, 4, 6) }
            r << er.call { b.copy(src, 14, 4) }
            r << er.call { b.copy(src, -1) }
            r << er.call { b.copy(src, 0, -2) }
            r << er.call { b.copy(src, 0, 2, -1) }
            r << er.call { b.copy(src, 0, 2, 9) }
            r << er.call { b.copy(src, 0, nil, 9) }
            r << er.call { b.copy(123) }
            r << er.call { b.copy(nil) }
            r << er.call { b.copy("str") }
            r << er.call { b.copy(src, 1.5) }
            r << er.call { b.copy(src, 0, 2, nil) }
            # readonly: source-side errors win, then AccessError before
            # the destination bounds check
            ro = IO::Buffer.for("frozen")
            r << er.call { ro.copy(src) }
            r << er.call { ro.copy(src, 0, 2, 9) }
            r << er.call { ro.copy(src, -1) }
            # locked buffers may still be copied into and out of
            lk = IO::Buffer.new(8)
            lk.locked { r << lk.copy(src, 0, 2) }
            r << lk.get_string
            lk.locked { r << b.copy(lk, 0, 2) }
            r
            "##,
        );
        run_test_error(r#"IO::Buffer.new(4).copy"#);
        run_test_error(r#"s = IO::Buffer.for("ab"); IO::Buffer.new(4).copy(s, 0, 1, 0, 0)"#);
    }

    #[test]
    fn io_buffer_string_args_and_views() {
        // The CRuby-verified fine print of the string-facing API:
        // get_string's encoding argument (Encoding object, name String,
        // unknown-name fallback to BINARY, nil offset/length), set_string's
        // (offset, length, source_offset) shape with #copy's error set and
        // check order, slice argument forms and invalidation, clear
        // defaults and value masking, hexdump, and each_byte bounds.
        run_test_once(
            r##"
            r = []
            er = ->(&blk) { begin; blk.call; :no_raise; rescue => e; [e.class, e.message]; end }
            g = IO::Buffer.new(8); g.set_string("h\xC3\xA9llo")
            r << [g.get_string(0, 3, Encoding::UTF_8).encoding.name, g.get_string(0, 3).encoding.name]
            r << [g.get_string(0, 3, "UTF-8").encoding.name, g.get_string(0, 2, "BINARY").encoding.name]
            r << [g.get_string(0, 2, "ASCII-8BIT").encoding.name, g.get_string(0, 2, "BOGUS").encoding.name]
            r << [g.get_string(nil).bytesize, g.get_string(0, nil).bytesize, g.get_string(0, 2, Encoding::BINARY).encoding.name]
            # set_string: (string, offset = 0, length = nil, source_offset = 0)
            w = IO::Buffer.new(8)
            r << [w.set_string("abcdef", 1, 2), w.get_string]
            r << [w.set_string("abcdef", 0, 2, 3), w.get_string]
            r << [w.set_string("abc", 0, nil, 2), w.get_string(0, 2)]
            r << [w.set_string(""), w.set_string("", 8)]
            o = Object.new; def o.to_str; "XY"; end
            r << [w.set_string(o, 6), w.get_string(6)]
            r << er.call { w.set_string(123) } << er.call { w.set_string(:ab) }
            r << er.call { w.set_string("abc", 0, 5) }
            r << er.call { w.set_string("abc", 0, 2, 5) }
            r << er.call { w.set_string("abc", -1) } << er.call { w.set_string("abc", nil) }
            r << er.call { w.set_string("abc", 0, -1) } << er.call { w.set_string("abc", 0, 1, -1) }
            r << er.call { w.set_string("aa", 100) } << er.call { w.set_string("", 9) }
            # readonly: conversion and index extraction beat AccessError,
            # AccessError beats the source-range checks
            ro = IO::Buffer.for("frozen")
            r << er.call { ro.set_string("x") } << er.call { ro.set_string("x", -1) }
            r << er.call { ro.set_string(123) } << er.call { ro.set_string("x", 0, 5) }
            # the source string may be the buffer's own backing store
            str = +"abcd"
            IO::Buffer.for(str) { |bb| bb.set_string(str, 1, 3) }
            r << str
            # slice forms, nesting, bounds, invalidation
            p8 = IO::Buffer.new(8); p8.set_string("abcdefgh")
            r << [p8.slice.size, p8.slice(3).get_string, p8.slice(2, 4).slice(1, 2).get_string]
            r << er.call { p8.slice(9) } << er.call { p8.slice(0, 9) }
            r << er.call { p8.slice(-1) } << er.call { p8.slice(0, -1) }
            rsl = IO::Buffer.for("frozen").slice(1, 3)
            r << [rsl.readonly?, er.call { rsl.set_string("x") }]
            big = IO::Buffer.new(8); bsl = big.slice(4, 4); big.resize(2)
            r << er.call { bsl.get_string }
            fb = IO::Buffer.new(8); fsl = fb.slice(0, 4); fb.free
            r << er.call { fsl.get_string }
            # clear: default value/offset, values masked to a byte
            c = IO::Buffer.new(4)
            r << [c.clear(0x41).get_string, c.clear.get_string.bytes, c.clear(0x141).get_string.bytes]
            r << er.call { IO::Buffer.for("ab").clear(1) }
            # hexdump
            h = IO::Buffer.new(20); h.set_string("Hello World! \x01\x02")
            r << h.hexdump
            # each_byte: bounds check the offset. An overlong count is NOT
            # compared: CRuby yields exactly count items, reading past the
            # end of the buffer (garbage bytes); monoruby clamps at size.
            eb = IO::Buffer.for("abcdef")
            r << [eb.each_byte(2).to_a, eb.each_byte(1, 3).to_a]
            r << er.call { eb.each_byte(7) {} } << (eb.each_byte(6) {} && :ok)
            r << er.call { eb.each_byte(-1) {} } << er.call { eb.each_byte(nil) {} }
            r << er.call { eb.each_byte(0, -1) {} }
            # resize: growth zero-fills and keeps content
            gr = IO::Buffer.new(4); gr.set_string("abcd"); gr.resize(8)
            r << [gr.size, gr.get_string.bytes]
            r << er.call { gr.resize("x") } << er.call { gr.resize(-1) }
            r
            "##,
        );
    }

    #[test]
    fn io_buffer_value_bounds_and_dup() {
        // Strict Integer offsets for get/set_value, per-item validation
        // of an explicit each/values count, the float conversion
        // TypeError, #dup's writable-internal-copy semantics, and the
        // Comparable family riding on <=>.
        run_test_once(
            r##"
            r = []
            er = ->(&blk) { begin; blk.call; :no_raise; rescue => e; [e.class, e.message]; end }
            b = IO::Buffer.new(8)
            r << er.call { b.get_value(:U8, -1) } << er.call { b.set_value(:U8, -1, 0) }
            r << er.call { b.get_value(:U8, nil) } << er.call { b.get_value(:U8, 1.5) }
            r << er.call { b.set_value(:U8, nil, 1) }
            r << er.call { IO::Buffer.for("abcd").set_value(:U8, 0, 1) }
            r << er.call { b.set_value(:U8, 0, "x") } << er.call { b.set_value(:f32, 0, "x") }
            # each/values: up-front offset bound, per-item count validation
            e6 = IO::Buffer.for("abcdef")
            r << e6.values(:U8, 6) << er.call { e6.values(:U8, 7) }
            r << e6.values(:u16, 5) << e6.values(:u16, 0, 3).size
            r << er.call { e6.values(:u16, 0, 4) }
            r << er.call { e6.each(:U8, -1) {} } << er.call { e6.values(:U8, nil) }
            r << er.call { e6.values(:U8, 0, -1) } << er.call { e6.each(:U8, 0, -1) {} }
            # dup / clone: a writable INTERNAL copy, whatever the source
            m = IO::Buffer.new(64, IO::Buffer::MAPPED).dup
            r << [m.internal?, m.mapped?, m.readonly?, m.size]
            n0 = IO::Buffer.new(0).dup
            r << [n0.size, n0.null?]
            dsl = IO::Buffer.for("abcdef").slice(1, 3).dup
            r << [dsl.size, dsl.get_string, dsl.internal?, dsl.external?]
            rd = IO::Buffer.for("frozen").dup
            r << [rd.readonly?, rd.set_string("X"), rd.get_string]
            lk = IO::Buffer.new(4)
            lk.locked { r << er.call { lk.dup } << er.call { lk.clone } }
            str = +"xyz"
            IO::Buffer.for(str) { |v| dv = v.dup; dv.set_string("A"); r << [str, dv.get_string] }
            # Comparable rides on <=>; a non-buffer operand is a TypeError
            x = IO::Buffer.for("ab"); y = IO::Buffer.for("ac")
            r << [x < y, x <= y, y > x, x.between?(x, y)]
            r << er.call { x == "ab" }
            # .string argument errors
            r << er.call { IO::Buffer.string(-1) { } } << er.call { IO::Buffer.string("x") { } }
            # a freed buffer still answers size; data access fails
            fr = IO::Buffer.new(4); fr.free
            r << [er.call { fr.get_value(:U8, 0) }, er.call { fr.get_string }, fr.size]
            r
            "##,
        );
    }

    #[test]
    fn io_buffer_construction_and_flags() {
        // Constants, allocation-kind selection, error classes and
        // messages, flag-forced allocation. Addresses are stripped from
        // #to_s so the comparison with CRuby is stable.
        run_test_once(
            r##"
            b = IO::Buffer
            r = []
            r << [b::EXTERNAL, b::INTERNAL, b::MAPPED, b::SHARED, b::LOCKED, b::PRIVATE, b::READONLY]
            r << (b::PAGE_SIZE > 0) << (b::DEFAULT_SIZE == 65536)
            [:AllocationError, :AccessError, :LockedError, :InvalidatedError, :MaskError].each do |c|
              r << [c, b.const_get(c).superclass]
            end
            strip = ->(buf) { buf.to_s.sub(/0x\h+/, "0xA") }
            buf = IO::Buffer.new(8)
            r << strip.(buf) << buf.inspect.sub(/0x\h+\+/, "0xA+")
            r << strip.(IO::Buffer.new(IO::Buffer::PAGE_SIZE))
            z = IO::Buffer.new(0)
            r << z.to_s << [z.null?, z.valid?, z.empty?, z.size]
            buf.free
            r << buf.to_s << [buf.null?, buf.valid?, buf.size]
            m = IO::Buffer.new(64, IO::Buffer::MAPPED)
            r << [m.mapped?, m.internal?, m.external?]
            i = IO::Buffer.new(IO::Buffer::PAGE_SIZE * 2, IO::Buffer::INTERNAL)
            r << [i.internal?, i.mapped?]
            r << IO::Buffer.new(12, IO::Buffer::INTERNAL | IO::Buffer::SHARED).shared?
            er = ->(&blk) { begin; blk.call; :no_raise; rescue => e; [e.class, e.message]; end }
            r << er.call { IO::Buffer.new(-1) }
            r << er.call { IO::Buffer.new(4, 0) }
            r << er.call { IO::Buffer.new("x") }
            r << er.call { IO::Buffer.for(123) }
            r
            "##,
        );
    }

    #[test]
    fn io_buffer_for_string_slice_semantics() {
        // .for (readonly / block-mutable, invalidation at block exit,
        // write-through to the String), .string, slices sharing bytes
        // with independent lock state, get/set_string bounds.
        run_test_once(
            r##"
            r = []
            s = IO::Buffer.for("abc")
            r << [s.external?, s.readonly?, s.valid?]
            begin; s.set_string("x"); rescue => e; r << [e.class, e.message]; end
            str = +"hello"
            inner = nil
            IO::Buffer.for(str) { |x| inner = x; x.set_string("HELLO") }
            r << str << inner.null?
            r << IO::Buffer.for("frozen".freeze).readonly?
            r << (IO::Buffer.string(7) { |bb| bb.set_string("test") })
            b = IO::Buffer.new(8); b.set_string("abcdefgh")
            sl = b.slice(2, 4)
            r << sl.get_string
            sl.set_string("XY")
            r << b.get_string << [sl.external?, sl.internal?]
            b.locked { r << [b.locked?, sl.locked?] }
            gs = IO::Buffer.for("h\xC3\xA9llo".b)
            r << [gs.size, gs.get_string(1, 2), gs.get_string(1)]
            er = ->(&blk) { begin; blk.call; :no_raise; rescue => e; [e.class, e.message]; end }
            r << er.call { gs.get_string(10) }
            r << er.call { gs.get_string(0, 100) }
            w = IO::Buffer.new(4); w.set_string("ab", 1)
            r << w.get_string << w.set_string("zzz")
            r << IO::Buffer.new(5).clear(0x41, 1, 3).get_string
            r
            "##,
        );
    }

    #[test]
    fn io_buffer_typed_values_and_iteration() {
        // get_value/set_value across the full 18-symbol type table
        // (lowercase = little-endian, uppercase = big-endian), the
        // set_value return-offset quirk, bounds/unknown-type errors, and
        // the each/values/each_byte iteration family (blockless forms
        // return Enumerators).
        run_test_once(
            r##"
            r = []
            b = IO::Buffer.for("\x01\x02\x03\x04\x05\x06\x07\x88".b)
            %i[U8 S8 u16 U16 s16 S16 u32 U32 s32 S32 u64 U64 s64 S64 f32 F32 f64 F64].each do |t|
              begin
                r << [t, b.get_value(t, 0)]
              rescue => e
                r << [t, e.class, e.message]
              end
            end
            er = ->(&blk) { begin; blk.call; :no_raise; rescue => e; [e.class, e.message]; end }
            r << er.call { b.get_value(:U8, 100) }
            r << er.call { b.get_value(:bogus, 0) }
            w = IO::Buffer.new(8)
            r << w.set_value(:U16, 0, 0x0102) << w.get_string(0, 2).bytes
            r << w.set_value(:u16, 2, 0x0102) << w.get_string(2, 2).bytes
            r << w.set_value(:S8, 4, -1) << w.get_string(4, 1).bytes
            r << w.set_value(:f32, 4, 1.5) << w.get_value(:f32, 4)
            r << w.set_value(:F64, 0, -2.25) << w.get_value(:F64, 0)
            r << er.call { w.set_value(:U8, 100, 1) }
            r << b.each(:U8).first(3)
            r << b.each(:u16, 2).first(2)
            r << b.each(:U8, 2, 2).to_a
            c = 0
            b.each(:U8) { |o, v| c += 1 }
            r << c
            r << b.values(:U8).first(4)
            r << b.values(:u32, 4)
            r << b.each_byte.first(3)
            cb = []
            b.each_byte { |x| cb << x }
            r << cb.size
            r
            "##,
        );
    }

    #[test]
    fn io_buffer_lock_transfer_resize_compare() {
        run_test_once(
            r##"
            r = []
            er = ->(&blk) { begin; blk.call; :no_raise; rescue => e; [e.class, e.message]; end }
            lb = IO::Buffer.new(4)
            lb.locked { r << lb.locked?; r << er.call { lb.locked {} } }
            # an exception raised inside #locked leaves the buffer locked
            r << lb.locked?
            l2 = IO::Buffer.new(4)
            r << er.call { l2.locked { l2.resize(8) } } << l2.locked?
            l3 = IO::Buffer.new(4)
            r << er.call { l3.locked { l3.free } }
            l4 = IO::Buffer.new(4)
            r << er.call { l4.locked { l4.transfer } }
            r << (IO::Buffer.new(4).locked { :ret })
            b2 = IO::Buffer.new(4)
            t = b2.transfer
            r << [b2.null?, t.valid?, t.size]
            rz = IO::Buffer.new(8); rz.resize(4)
            r << [rz.size, rz.get_string]
            r << er.call { IO::Buffer.for("abc").resize(5) }
            n = IO::Buffer.new(0); n.resize(4)
            r << [n.size, n.internal?]
            x = IO::Buffer.new(2); x.set_string("ab")
            y = IO::Buffer.new(2); y.set_string("ab")
            # CRuby's IO::Buffer#<=> leaks the raw memcmp(3) result, whose
            # magnitude varies with the libc/CPU (e.g. -1 vs -32769 for the
            # same operands on different glibc builds) — compare the sign
            # only, or the differential test is flaky across environments.
            r << [x == y, (x <=> y) <=> 0]
            y.set_string("ac")
            r << [x == y, (x <=> y) <=> 0]
            r << er.call { x <=> "ab" }
            r
            "##,
        );
    }
}
