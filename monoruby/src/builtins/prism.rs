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
// `gem/prism/prism.rb` (standing in for the gem's C extension
// `prism/prism.so`) builds the public API on them.
//
// The serialization format is tied to the prism version: the crate and
// the vendored gem must agree (both 2.1.0 today; `Prism::VERSION` is
// read from the library so a mismatch shows up as a version rather than
// as garbage). The C signatures are declared here rather than taken from
// the crate's bindings, which allowlist only what the Rust parser uses.
//

/// `pm_buffer_t` (include/prism/buffer.h): opaque since prism 2.0; only
/// ever handled through a pointer from `pm_buffer_new`.
#[repr(C)]
struct PmBuffer {
    _private: [u8; 0],
}

unsafe extern "C" {
    fn pm_version() -> *const c_char;
    fn pm_buffer_new() -> *mut PmBuffer;
    fn pm_buffer_value(buffer: *const PmBuffer) -> *const c_char;
    fn pm_buffer_length(buffer: *const PmBuffer) -> usize;
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
    fn pm_serialize_parse_success_p(source: *const u8, size: usize, data: *const c_char) -> bool;
    fn pm_string_query_local(source: *const u8, length: usize, encoding_name: *const c_char) -> c_int;
    fn pm_string_query_constant(source: *const u8, length: usize, encoding_name: *const c_char) -> c_int;
    fn pm_string_query_method_name(
        source: *const u8,
        length: usize,
        encoding_name: *const c_char,
    ) -> c_int;
}

/// The head of every `pm_*_node_t` (`pm_node_t`, include/prism/ast.h):
/// all that [`node_id_at`] reads of a node.
#[repr(C)]
struct PmNode {
    type_: u16,
    flags: u16,
    node_id: u32,
    start: u32,
    length: u32,
}

/// `pm_arena_t` / `pm_parser_t`: opaque, handled through pointers.
#[repr(C)]
struct PmOpaque {
    _private: [u8; 0],
}

/// `pm_visit_result_t` (include/prism/node.h).
const PM_VISIT_DESCEND: std::ffi::c_uint = 1;
const PM_VISIT_STOP: std::ffi::c_uint = 2;

unsafe extern "C" {
    fn pm_arena_new() -> *mut PmOpaque;
    fn pm_arena_free(arena: *mut PmOpaque);
    fn pm_parser_new(
        arena: *mut PmOpaque,
        source: *const u8,
        size: usize,
        options: *const std::ffi::c_void,
    ) -> *mut PmOpaque;
    fn pm_parser_free(parser: *mut PmOpaque);
    fn pm_parse(parser: *mut PmOpaque) -> *mut PmNode;
    fn pm_visit_node(
        node: *const PmNode,
        visitor: extern "C" fn(*const PmNode, *mut std::ffi::c_void) -> std::ffi::c_uint,
        data: *mut std::ffi::c_void,
    ) -> bool;
}

/// `pm_node_type` of the nodes error_highlight knows how to spot
/// (`ErrorHighlight::Spotter#spot`'s prism half).
const SPOTTED_NODES: [u16; 10] = [
    14, // PM_BLOCK_NODE
    19, // PM_CALL_NODE
    20, // PM_CALL_OPERATOR_WRITE_NODE
    37, // PM_CONSTANT_PATH_NODE
    38, // PM_CONSTANT_PATH_OPERATOR_WRITE_NODE
    42, // PM_CONSTANT_READ_NODE
    45, // PM_DEF_NODE
    74, // PM_INDEX_OPERATOR_WRITE_NODE
    93, // PM_LAMBDA_NODE
    95, // PM_LOCAL_VARIABLE_OPERATOR_WRITE_NODE
];

/// Nodes that only group other nodes, and so share the extent of what
/// they hold without being what an instruction was compiled from.
const GROUPING_NODES: [u16; 4] = [
    5,   // PM_ARGUMENTS_NODE
    117, // PM_PARENTHESES_NODE
    122, // PM_PROGRAM_NODE
    141, // PM_STATEMENTS_NODE
];

const PM_CONSTANT_PATH_NODE: u16 = 37;
const PM_CONSTANT_PATH_OPERATOR_WRITE_NODE: u16 = 38;
const PM_CONSTANT_READ_NODE: u16 = 42;

struct NodeSearch {
    loc: crate::ast::Loc,
    spotted: Option<u32>,
    other: Option<u32>,
    /// The innermost `Foo::Bar += 1` entered so far: where it starts,
    /// and its id.
    op_cdecl: Option<(u32, u32)>,
}

extern "C" fn node_search_visit(
    node: *const PmNode,
    data: *mut std::ffi::c_void,
) -> std::ffi::c_uint {
    // SAFETY: prism hands the callback a node of the tree being walked
    // and the `NodeSearch` `node_id_at` passed to `pm_visit_node`.
    let (node, search) = unsafe { (&*node, &mut *(data as *mut NodeSearch)) };
    let start = node.start as usize;
    // The same rule as `location_to_loc` in parser/prism_backend.rs.
    let last = (start + node.length as usize).saturating_sub(1).max(start);
    if node.type_ == PM_CONSTANT_PATH_OPERATOR_WRITE_NODE {
        search.op_cdecl = Some((node.start, node.node_id));
    }
    if start == search.loc.0 && last == search.loc.1 {
        if SPOTTED_NODES.contains(&node.type_) {
            // The constant path `Foo::Bar += 1` reads, and each scope
            // of it, is compiled by CRuby as part of the assignment:
            // a NameError there carries the assignment's id. Those
            // nodes all start where the assignment does, and nothing
            // else under it or after it starts there.
            let id = match search.op_cdecl {
                Some((at, id))
                    if at == node.start
                        && matches!(node.type_, PM_CONSTANT_PATH_NODE | PM_CONSTANT_READ_NODE) =>
                {
                    id
                }
                _ => node.node_id,
            };
            search.spotted = Some(id);
            return PM_VISIT_STOP;
        }
        if search.other.is_none() && !GROUPING_NODES.contains(&node.type_) {
            search.other = Some(node.node_id);
        }
    }
    PM_VISIT_DESCEND
}

/// The `node_id` of the node of `source` that covers exactly `loc`,
/// with `source` parsed as `Prism.parse_file` parses it (no options), so
/// that the id names the same node in the tree that call builds.
///
/// monoruby keeps, for each instruction, the extent of the node it was
/// compiled from (a `Loc`), where CRuby keeps that node's id; this turns
/// the one into the other for
/// `RubyVM::AbstractSyntaxTree.node_id_for_backtrace_location`. Several
/// nodes can cover one extent (`foo` is a `CallNode` inside a
/// `StatementsNode`): a node error_highlight can spot wins, then any
/// node that is not a mere grouping.
pub(crate) fn node_id_at(source: &[u8], loc: crate::ast::Loc) -> Option<u32> {
    let mut search = NodeSearch {
        loc,
        spotted: None,
        other: None,
        op_cdecl: None,
    };
    // SAFETY: the arena and the parser are created here and freed below;
    // `source` outlives the parser, which only reads it. The tree lives
    // in the arena, so it is walked before either is freed.
    unsafe {
        let arena = pm_arena_new();
        let parser = pm_parser_new(arena, source.as_ptr(), source.len(), std::ptr::null());
        let root = pm_parse(parser);
        if !root.is_null() {
            pm_visit_node(
                root,
                node_search_visit,
                &mut search as *mut NodeSearch as *mut _,
            );
        }
        pm_parser_free(parser);
        pm_arena_free(arena);
    }
    search.spotted.or(search.other)
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
    // SAFETY: `buffer` is initialized by prism and freed below; `source`
    // and `options` outlive the calls, which only read them.
    let bytes = unsafe {
        let buffer = pm_buffer_new();
        if buffer.is_null() {
            return Err(MonorubyErr::runtimeerr("Prism: could not allocate the buffer"));
        }
        let data = options_ptr(options);
        match kind {
            0 => pm_serialize_parse(buffer, source.as_ptr(), source.len(), data),
            1 => pm_serialize_lex(buffer, source.as_ptr(), source.len(), data),
            2 => pm_serialize_parse_lex(buffer, source.as_ptr(), source.len(), data),
            3 => pm_serialize_parse_comments(buffer, source.as_ptr(), source.len(), data),
            _ => {
                pm_buffer_free(buffer);
                return Err(MonorubyErr::argumenterr(format!("unknown serializer {kind}")));
            }
        }
        let bytes = std::slice::from_raw_parts(pm_buffer_value(buffer) as *const u8, pm_buffer_length(buffer)).to_vec();
        pm_buffer_free(buffer);
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
    let ok = unsafe { pm_serialize_parse_success_p(source.as_ptr(), source.len(), options_ptr(options)) };
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
    let bytes = string_v.expect_bytes(&globals.store)?;
    // prism's `pm_slice_type` peeks at the byte *after* the slice (its
    // trailing `!` / `?` / `=` check runs once the cursor reaches `end`),
    // which is harmless for CRuby's NUL-terminated strings but reads
    // whatever follows a bare slice here — `local?("foo")` answered false
    // whenever the next heap byte happened to be one of those. Hand it a
    // NUL-terminated copy so the peek is well-defined, as it is under CRuby.
    let mut string = Vec::with_capacity(bytes.len() + 1);
    string.extend_from_slice(bytes);
    string.push(0);
    let len = bytes.len();
    let encoding = std::ffi::CString::new(encoding_v.expect_str(&globals.store)?)
        .map_err(|_| MonorubyErr::argumenterr("encoding name contains a NUL"))?;
    // SAFETY: `string` (NUL-terminated, `len + 1` bytes) and the C string
    // outlive the call, which only reads them.
    let res = unsafe {
        match kind {
            0 => pm_string_query_local(string.as_ptr(), len, encoding.as_ptr()),
            1 => pm_string_query_constant(string.as_ptr(), len, encoding.as_ptr()),
            2 => pm_string_query_method_name(string.as_ptr(), len, encoding.as_ptr()),
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

#[cfg(test)]
mod tests {
    use super::node_id_at;
    use crate::ast::Loc;

    /// `Loc` is a first and a last byte, both inclusive.
    #[test]
    fn node_id_at_finds_the_node_of_an_extent() {
        //           0         1         2
        //           0123456789012345678901234567890
        let src = b"foo.bar\nFoo::Bar += 1\nFoo::Bar\n";
        let call = node_id_at(src, Loc(0, 6)).unwrap();
        let recv = node_id_at(src, Loc(0, 2)).unwrap();
        assert_ne!(call, recv);
        // No node is exactly `oo.b`, or reaches past the end.
        assert_eq!(node_id_at(src, Loc(1, 4)), None);
        assert_eq!(node_id_at(src, Loc(0, 99)), None);

        // `Foo::Bar += 1`: the path it reads, and the scope of that
        // path, answer the assignment's id.
        let write = node_id_at(src, Loc(8, 20)).unwrap();
        assert_eq!(node_id_at(src, Loc(8, 15)), Some(write));
        assert_eq!(node_id_at(src, Loc(8, 10)), Some(write));
        // The value is a node of its own, not one the gem spots.
        let one = node_id_at(src, Loc(20, 20)).unwrap();
        assert_ne!(one, write);

        // A path that is only read keeps its own ids.
        let path = node_id_at(src, Loc(22, 29)).unwrap();
        let scope = node_id_at(src, Loc(22, 24)).unwrap();
        assert_ne!(path, scope);
        assert_ne!(path, write);
        assert_ne!(scope, write);

        // A statement that fills its line is found under the nodes that
        // only group it; a source that is nothing but grouping has none.
        assert_eq!(
            node_id_at(b"foo\n", Loc(0, 2)),
            node_id_at(b"foo", Loc(0, 2))
        );
        assert!(node_id_at(b"foo", Loc(0, 2)).is_some());
        assert_eq!(node_id_at(b"", Loc(0, 0)), None);
    }
}
