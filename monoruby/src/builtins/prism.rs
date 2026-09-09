use super::*;
use std::ffi::{c_char, c_int};

//
// Prism: the Ruby-level `Prism` module's native half.
//
// monoruby already links libprism (the `ruby-prism-sys` crate, static) for
// its own parser. The prism gem's Ruby half (`prism.rb`, the node classes,
// `Prism::Serialize`, the `Translation::*` layers) is pure Ruby and ships
// with the vendored stdlib; what it needs from a native backend is one
// call per operation that parses and hands back the *serialized* tree,
// which `Prism::Serialize` turns into node objects. That is exactly what
// the gem's FFI backend (`prism/ffi.rb`) does through the ffi gem; these
// builtins do the same through the library we already carry, and
// `stdlib/prism/prism.rb` (standing in for the gem's C extension
// `prism/prism.so`) builds the public API on them.
//
// The serialization format is tied to the prism version: the crate and
// the vendored gem must agree (both 1.9.0 today; `Prism::VERSION` is
// read from the library so a mismatch shows up as a version rather than
// as garbage). The C signatures are declared here rather than taken from
// the crate's bindings, which allowlist only what the Rust parser uses.
//

/// `pm_buffer_t` (include/prism/util/pm_buffer.h): the growable byte
/// buffer the serializers write into.
#[repr(C)]
struct PmBuffer {
    length: usize,
    capacity: usize,
    value: *mut c_char,
}

unsafe extern "C" {
    fn pm_version() -> *const c_char;
    fn pm_buffer_init(buffer: *mut PmBuffer) -> bool;
    fn pm_buffer_free(buffer: *mut PmBuffer);
    fn pm_serialize_parse(buffer: *mut PmBuffer, source: *const u8, size: usize, data: *const c_char);
    fn pm_serialize_parse_comments(
        buffer: *mut PmBuffer,
        source: *const u8,
        size: usize,
        data: *const c_char,
    );
    fn pm_serialize_lex(buffer: *mut PmBuffer, source: *const u8, size: usize, data: *const c_char);
    fn pm_serialize_parse_lex(
        buffer: *mut PmBuffer,
        source: *const u8,
        size: usize,
        data: *const c_char,
    );
    fn pm_parse_success_p(source: *const u8, size: usize, data: *const c_char) -> bool;
    fn pm_string_query_local(source: *const u8, length: usize, encoding_name: *const c_char) -> c_int;
    fn pm_string_query_constant(source: *const u8, length: usize, encoding_name: *const c_char) -> c_int;
    fn pm_string_query_method_name(
        source: *const u8,
        length: usize,
        encoding_name: *const c_char,
    ) -> c_int;
}

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_toplevel_module("Prism").id();
    globals.define_builtin_module_func(klass, "__serialize", serialize, 3);
    globals.define_builtin_module_func(klass, "__parse_success?", parse_success, 2);
    globals.define_builtin_module_func(klass, "__string_query", string_query, 3);
    globals.define_builtin_module_func(klass, "__version", version, 0);
}

/// The `data` argument of the serializers is the options blob
/// `Prism.dump_options` packs (`pm_options_read` decodes it with the
/// lengths it carries). An empty blob means "no options": NULL, which
/// `pm_options_read` accepts, rather than a short buffer it would read
/// past.
fn options_ptr(options: &[u8]) -> *const c_char {
    if options.is_empty() {
        std::ptr::null()
    } else {
        options.as_ptr() as *const c_char
    }
}

///
/// ### Prism.__serialize(kind, source, options) -> String
///
/// Run the serializer selected by `kind` (0 parse, 1 lex, 2 parse + lex,
/// 3 comments) over `source` with the packed `options`, answering the
/// serialized bytes (ASCII-8BIT).
///
#[monoruby_builtin]
fn serialize(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let kind = lfp.arg(0).expect_integer(&globals.store)?;
    let (source_v, options_v) = (lfp.arg(1), lfp.arg(2));
    let source = source_v.expect_bytes(&globals.store)?;
    let options = options_v.expect_bytes(&globals.store)?;
    let mut buffer = PmBuffer {
        length: 0,
        capacity: 0,
        value: std::ptr::null_mut(),
    };
    // SAFETY: `buffer` is initialized by prism and freed below; `source`
    // and `options` outlive the calls, which only read them.
    let bytes = unsafe {
        if !pm_buffer_init(&mut buffer) {
            return Err(MonorubyErr::runtimeerr("Prism: could not allocate the buffer"));
        }
        let data = options_ptr(options);
        match kind {
            0 => pm_serialize_parse(&mut buffer, source.as_ptr(), source.len(), data),
            1 => pm_serialize_lex(&mut buffer, source.as_ptr(), source.len(), data),
            2 => pm_serialize_parse_lex(&mut buffer, source.as_ptr(), source.len(), data),
            3 => pm_serialize_parse_comments(&mut buffer, source.as_ptr(), source.len(), data),
            _ => {
                pm_buffer_free(&mut buffer);
                return Err(MonorubyErr::argumenterr(format!("unknown serializer {kind}")));
            }
        }
        let bytes = std::slice::from_raw_parts(buffer.value as *const u8, buffer.length).to_vec();
        pm_buffer_free(&mut buffer);
        bytes
    };
    Ok(Value::bytes(bytes))
}

///
/// ### Prism.__parse_success?(source, options) -> bool
///
#[monoruby_builtin]
fn parse_success(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let (source_v, options_v) = (lfp.arg(0), lfp.arg(1));
    let source = source_v.expect_bytes(&globals.store)?;
    let options = options_v.expect_bytes(&globals.store)?;
    // SAFETY: both slices outlive the call, which only reads them.
    let ok = unsafe { pm_parse_success_p(source.as_ptr(), source.len(), options_ptr(options)) };
    Ok(Value::bool(ok))
}

///
/// ### Prism.__string_query(kind, string, encoding_name) -> Integer
///
/// `pm_string_query_{local,constant,method_name}` (kind 0, 1, 2): -1 for
/// an unusable encoding, 0 false, 1 true.
///
#[monoruby_builtin]
fn string_query(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let kind = lfp.arg(0).expect_integer(&globals.store)?;
    let (string_v, encoding_v) = (lfp.arg(1), lfp.arg(2));
    let string = string_v.expect_bytes(&globals.store)?;
    let encoding = std::ffi::CString::new(encoding_v.expect_str(&globals.store)?)
        .map_err(|_| MonorubyErr::argumenterr("encoding name contains a NUL"))?;
    // SAFETY: `string` and the C string outlive the call, which only
    // reads them.
    let res = unsafe {
        match kind {
            0 => pm_string_query_local(string.as_ptr(), string.len(), encoding.as_ptr()),
            1 => pm_string_query_constant(string.as_ptr(), string.len(), encoding.as_ptr()),
            2 => pm_string_query_method_name(string.as_ptr(), string.len(), encoding.as_ptr()),
            _ => return Err(MonorubyErr::argumenterr(format!("unknown query {kind}"))),
        }
    };
    Ok(Value::integer(res as i64))
}

///
/// ### Prism.__version -> String
///
#[monoruby_builtin]
fn version(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // SAFETY: `pm_version` answers a static NUL-terminated string.
    let s = unsafe { std::ffi::CStr::from_ptr(pm_version()) };
    Ok(Value::string_from_str(&s.to_string_lossy()))
}
