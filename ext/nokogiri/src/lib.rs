//! Nokogiri's native half (`nokogiri_native.so`) as a monoruby extension:
//! what the gem's `ext/nokogiri` (nokogiri.so) provides, written over the
//! bundled libxml2 (`libxml2-src`, 2.13.8 with nokogiri's patches, plus
//! libxslt and gumbo). The gem's Ruby half is vendored unchanged under
//! `gem/nokogiri/`; `gem/nokogiri/nokogiri.rb` stands in for the C
//! extension and requires this library, whose `Init_nokogiri_native`
//! builds the class tree and registers the native methods. See
//! `doc/nokogiri.md`.
//!
//! Objects: a `Document` owns its `xmlDoc` and frees it (with every node
//! that was unlinked from it) when collected; a `Node` / `Namespace` /
//! `NodeSet` / `XPathContext` is a native object holding a libxml2 pointer.
//! Each `xmlNode` maps to at most one Ruby object, remembered in the
//! node's `_private` and kept alive by the document's `node_cache`
//! (nokogiri's scheme, so identity and lifetimes match).
//!
//! This is `src/builtins/nokogiri/` moved out of the interpreter
//! (doc/native_extension_loading.md, step 3): every interpreter service
//! goes through `monoruby_ext`'s `Ctx`, and the C callbacks libxml2 makes
//! (SAX, XPath handlers, XSLT extension functions, IO) find the running
//! call's context through the state their native method registered.

// Every native method has the same `(ctx, this, args, block)` shape for
// the `method!` thunk; most use only some of them.
#![allow(unused_variables)]

pub(crate) use libxml2_src as xml;
pub(crate) use monoruby_ext::*;
use std::cell::RefCell;
use std::collections::HashSet;
use std::ffi::{CStr, CString, c_char, c_int, c_void};

mod document;
mod dtd;
mod html5;
mod misc;
mod node;
mod node_set;
mod reader;
mod sax;
mod schema;
mod xpath;
mod xslt;

// ---------------------------------------------------------------------
// Small conveniences over the extension API
// ---------------------------------------------------------------------

pub(crate) trait CtxExt {
    fn ary_from_vec(&self, v: Vec<Value>) -> Value;
}

impl CtxExt for Ctx {
    fn ary_from_vec(&self, v: Vec<Value>) -> Value {
        self.ary_from(&v)
    }
}

pub(crate) trait BlockExt {
    /// The block, if one was given.
    fn given(self) -> Option<Block>;
}

impl BlockExt for Block {
    fn given(self) -> Option<Block> {
        if self.is_given() { Some(self) } else { None }
    }
}

/// `recv.name(*args)` if `recv` has the method, `None` otherwise.
pub(crate) fn funcall_if_exists(
    ctx: &mut Ctx,
    recv: Value,
    name: &str,
    args: &[Value],
) -> Result<Option<Value>> {
    ctx.funcall_if_exists(recv, name, args)
}

/// A `NameError`.
pub(crate) fn name_error(ctx: &mut Ctx, msg: &str) -> Error {
    match ctx.const_get(Value::UNDEF, "NameError") {
        Some(k) => ctx.raise(k, msg),
        None => ctx.runtime_error(msg),
    }
}

/// The bytes of `v` if it is a String.
/// The String's own buffer as a raw pointer and length: for a C parser
/// whose output keeps pointing into its input. The buffer stays put
/// (monoruby's collector does not move) as long as the String is alive
/// and not mutated; the caller keeps it rooted.
pub(crate) fn str_raw(ctx: &mut Ctx, v: Value) -> Result<(*const u8, usize)> {
    let b = ctx.str_bytes(v)?;
    Ok((b.as_ptr(), b.len()))
}

pub(crate) fn try_bytes(ctx: &Ctx, v: Value) -> Option<Vec<u8>> {
    if ctx.is_string(v) {
        ctx.str_vec(v).ok()
    } else {
        None
    }
}

pub(crate) fn try_fixnum(ctx: &mut Ctx, v: Value) -> Option<i64> {
    if ctx.type_of(v) == MrType::Integer {
        ctx.int(v).ok()
    } else {
        None
    }
}

pub(crate) fn try_symbol(ctx: &mut Ctx, v: Value) -> Option<String> {
    if ctx.type_of(v) == MrType::Symbol {
        ctx.sym_name(v).ok()
    } else {
        None
    }
}

/// `Some(v)` when `v` is a Hash.
pub(crate) fn try_hash(ctx: &Ctx, v: Value) -> Option<Value> {
    if ctx.type_of(v) == MrType::Hash {
        Some(v)
    } else {
        None
    }
}

/// The elements of an Array argument; a TypeError for anything else.
pub(crate) fn ary_vec(ctx: &mut Ctx, v: Value) -> Result<Vec<Value>> {
    if ctx.type_of(v) != MrType::Array {
        return Err(ctx.type_error(format!(
            "wrong argument type {} (expected Array)",
            builtin_type_name(ctx, v)
        )));
    }
    Ok((0..ctx.ary_len(v))
        .filter_map(|i| ctx.ary_get(v, i))
        .collect())
}

/// `wrong number of arguments` for a variadic registration that stands in
/// for an optional-argument arity.
pub(crate) fn check_arity(ctx: &mut Ctx, args: &[Value], min: usize, max: usize) -> Result<()> {
    if args.len() < min || args.len() > max {
        let expected = if min == max {
            min.to_string()
        } else {
            format!("{min}..{max}")
        };
        return Err(ctx.argument_error(format!(
            "wrong number of arguments (given {}, expected {expected})",
            args.len()
        )));
    }
    Ok(())
}

// ---------------------------------------------------------------------
// Classes
// ---------------------------------------------------------------------

/// The classes the native methods live on and wrap nodes into. Per
/// interpreter (see `Ctx::interpreter_id`): this library's statics are
/// shared by every interpreter of the process while `Init_` runs in each.
#[derive(Clone, Copy)]
#[allow(dead_code)]
pub(crate) struct Classes {
    pub nokogiri: Value,
    pub xml: Value,
    pub node: Value,
    pub element: Value,
    pub character_data: Value,
    pub text: Value,
    pub cdata: Value,
    pub comment: Value,
    pub attr: Value,
    pub document: Value,
    pub document_fragment: Value,
    pub pi: Value,
    pub entity_ref: Value,
    pub dtd: Value,
    pub entity_decl: Value,
    pub element_decl: Value,
    pub attribute_decl: Value,
    pub element_content: Value,
    pub namespace: Value,
    pub node_set: Value,
    pub xpath_context: Value,
    pub syntax_error: Value,
    pub xml_syntax_error: Value,
    pub xpath_syntax_error: Value,
    pub html4: Value,
    pub html4_document: Value,
    pub encoding_handler: Value,
    pub entity_lookup: Value,
    pub sax_parser: Value,
    pub sax_parser_context: Value,
    pub sax_push_parser: Value,
    pub html4_sax_parser: Value,
    pub html4_sax_parser_context: Value,
    pub html4_sax_push_parser: Value,
    pub reader: Value,
    pub schema: Value,
    pub relax_ng: Value,
    pub element_description: Value,
    pub gumbo: Value,
    pub html5_document: Value,
    pub xslt: Value,
    pub stylesheet: Value,
}

thread_local! {
    static CLASSES: RefCell<Option<(usize, Classes)>> = const { RefCell::new(None) };
}

pub(crate) fn classes() -> Classes {
    CLASSES.with(|c| c.borrow().as_ref().expect("Nokogiri is not initialized").1)
}

fn module(ctx: &mut Ctx, parent: Value, name: &str) -> Result<Value> {
    ctx.define_module(parent, name)
}

fn class(ctx: &mut Ctx, parent: Value, name: &str, superclass: Value) -> Result<Value> {
    ctx.define_class(parent, name, superclass, 0)
}

/// A class whose instances are native objects: their ivars live in the
/// heap table, never in the inline slots the JIT uses for plain objects.
/// Subclasses (the gem's, and the node kinds) inherit the type.
fn native_class(ctx: &mut Ctx, parent: Value, name: &str, superclass: Value) -> Result<Value> {
    ctx.define_class(parent, name, superclass, MR_CLASS_NATIVE)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn Init_nokogiri_native(ctx: *mut MrContext) -> c_int {
    // SAFETY: the interpreter's contract for `Init_`.
    unsafe { init(ctx, nokogiri_init) }
}

/// Build `Nokogiri` and the classes with native methods (the ones
/// `Init_nokogiri` defines), register the methods, set the version
/// constants. Run once per interpreter, before the gem's Ruby files
/// reopen the classes.
fn nokogiri_init(ctx: &mut Ctx) -> Result<()> {
    let id = ctx.interpreter_id();
    if CLASSES.with(|c| c.borrow().as_ref().is_some_and(|(i, _)| *i == id)) {
        return Ok(());
    }
    // SAFETY: plain library initialization.
    unsafe { xml::xmlInitParser() };
    let object = ctx.object_class();
    let standard_error = ctx
        .const_get(Value::UNDEF, "StandardError")
        .expect("StandardError");
    let nokogiri = module(ctx, Value::UNDEF, "Nokogiri")?;
    let xml_m = module(ctx, nokogiri, "XML")?;
    let xpath_m = module(ctx, xml_m, "XPath")?;
    let xslt_m = module(ctx, nokogiri, "XSLT")?;
    let gumbo = module(ctx, nokogiri, "Gumbo")?;
    let html5 = module(ctx, nokogiri, "HTML5")?;
    let xml_sax = module(ctx, xml_m, "SAX")?;
    let html4 = module(ctx, nokogiri, "HTML4")?;
    let html4_sax = module(ctx, html4, "SAX")?;

    let syntax_error = class(ctx, nokogiri, "SyntaxError", standard_error)?;
    let xml_syntax_error = class(ctx, xml_m, "SyntaxError", syntax_error)?;
    let xpath_syntax_error = class(ctx, xpath_m, "SyntaxError", xml_syntax_error)?;
    let node = native_class(ctx, xml_m, "Node", object)?;
    let element = class(ctx, xml_m, "Element", node)?;
    let character_data = class(ctx, xml_m, "CharacterData", node)?;
    let text = class(ctx, xml_m, "Text", character_data)?;
    let cdata = class(ctx, xml_m, "CDATA", text)?;
    let comment = class(ctx, xml_m, "Comment", character_data)?;
    let attr = class(ctx, xml_m, "Attr", node)?;
    let document = class(ctx, xml_m, "Document", node)?;
    let document_fragment = class(ctx, xml_m, "DocumentFragment", node)?;
    let pi = class(ctx, xml_m, "ProcessingInstruction", node)?;
    let entity_ref = class(ctx, xml_m, "EntityReference", node)?;
    let dtd = class(ctx, xml_m, "DTD", node)?;
    let entity_decl = class(ctx, xml_m, "EntityDecl", node)?;
    let element_decl = class(ctx, xml_m, "ElementDecl", node)?;
    let attribute_decl = class(ctx, xml_m, "AttributeDecl", node)?;
    let element_content = native_class(ctx, xml_m, "ElementContent", object)?;
    let namespace = native_class(ctx, xml_m, "Namespace", object)?;
    let node_set = native_class(ctx, xml_m, "NodeSet", object)?;
    let xpath_context = native_class(ctx, xml_m, "XPathContext", object)?;
    let html4_document = class(ctx, html4, "Document", document)?;
    let html5_document = class(ctx, html5, "Document", html4_document)?;
    let encoding_handler = native_class(ctx, nokogiri, "EncodingHandler", object)?;
    let entity_lookup = class(ctx, html4, "EntityLookup", object)?;
    let element_description = native_class(ctx, html4, "ElementDescription", object)?;
    let sax_parser = native_class(ctx, xml_sax, "Parser", object)?;
    let sax_parser_context = native_class(ctx, xml_sax, "ParserContext", object)?;
    let sax_push_parser = native_class(ctx, xml_sax, "PushParser", object)?;
    let html4_sax_parser = class(ctx, html4_sax, "Parser", sax_parser)?;
    let html4_sax_parser_context = class(ctx, html4_sax, "ParserContext", sax_parser_context)?;
    let html4_sax_push_parser = class(ctx, html4_sax, "PushParser", sax_push_parser)?;
    let reader = native_class(ctx, xml_m, "Reader", object)?;
    let schema = native_class(ctx, xml_m, "Schema", object)?;
    let relax_ng = class(ctx, xml_m, "RelaxNG", schema)?;
    let stylesheet = native_class(ctx, xslt_m, "Stylesheet", object)?;

    let c = Classes {
        nokogiri,
        xml: xml_m,
        node,
        element,
        character_data,
        text,
        cdata,
        comment,
        attr,
        document,
        document_fragment,
        pi,
        entity_ref,
        dtd,
        entity_decl,
        element_decl,
        attribute_decl,
        element_content,
        namespace,
        node_set,
        xpath_context,
        syntax_error,
        xml_syntax_error,
        xpath_syntax_error,
        html4,
        html4_document,
        encoding_handler,
        entity_lookup,
        sax_parser,
        sax_parser_context,
        sax_push_parser,
        html4_sax_parser,
        html4_sax_parser_context,
        html4_sax_push_parser,
        reader,
        schema,
        relax_ng,
        element_description,
        gumbo,
        html5_document,
        xslt: xslt_m,
        stylesheet,
    };
    CLASSES.with(|cell| *cell.borrow_mut() = Some((id, c)));

    misc::init(ctx, &c);
    document::init(ctx, &c);
    dtd::init(ctx, &c);
    html5::init(ctx, &c);
    node::init(ctx, &c);
    node_set::init(ctx, &c);
    reader::init(ctx, &c);
    sax::init(ctx, &c);
    schema::init(ctx, &c);
    xpath::init(ctx, &c);
    xslt::init(ctx, &c);

    // Constants `Init_nokogiri` sets (version/info.rb reads them).
    // SAFETY: a NUL-terminated static string of the library.
    let loaded = unsafe { CStr::from_ptr(xml::xmlParserVersion) }
        .to_bytes()
        .to_vec();
    let v = ctx.str("2.13.8");
    ctx.const_set(nokogiri, "LIBXML_COMPILED_VERSION", v);
    let v = ctx.str(loaded);
    ctx.const_set(nokogiri, "LIBXML_LOADED_VERSION", v);
    ctx.const_set(nokogiri, "LIBXML_ICONV_ENABLED", Value::bool(true));
    ctx.const_set(nokogiri, "LIBXML_ZLIB_ENABLED", Value::bool(false));
    let v = ctx.str("default");
    ctx.const_set(nokogiri, "LIBXML_MEMORY_MANAGEMENT", v);
    ctx.const_set(nokogiri, "PACKAGED_LIBRARIES", Value::bool(true));
    ctx.const_set(nokogiri, "PRECOMPILED_LIBRARIES", Value::bool(false));
    let patches = [
        "0001-Remove-script-macro-support.patch",
        "0002-Update-entities-to-remove-handling-of-ssi.patch",
        "0009-allow-wildcard-namespaces.patch",
        "0010-update-config.guess-and-config.sub-for-libxml2.patch",
        "0011-rip-out-libxml2-s-libc_single_threaded-support.patch",
        "0019-xpath-Use-separate-static-hash-table-for-standard-fu.patch",
        "0020-CVE-2025-6021-tree-Fix-integer-overflow-in-xmlBuildQ.patch",
        "0021-CVE-2025-6170-Fix-potential-buffer-overflows-of-inte.patch",
        "0022-CVE-2025-49795-schematron-Fix-null-pointer-dereferen.patch",
        "0023-CVE-2025-49794-CVE-2025-49796-schematron-Fix-xmlSche.patch",
    ];
    let list: Vec<Value> = patches.iter().map(|p| ctx.str(p)).collect();
    let v = ctx.ary_from(&list);
    ctx.const_set(nokogiri, "LIBXML2_PATCHES", v);
    let v = ctx.str("1.1.43");
    ctx.const_set(nokogiri, "LIBXSLT_COMPILED_VERSION", v);
    // The loaded version is the numeric form (`xsltLibxsltVersion`, "10143").
    // SAFETY: a constant of the library.
    let loaded = unsafe { xml::xsltLibxsltVersion };
    let v = ctx.str(loaded.to_string());
    ctx.const_set(nokogiri, "LIBXSLT_LOADED_VERSION", v);
    // nokogiri's only libxslt patch (build system: config.guess / config.sub).
    let p = ctx.str("0001-update-config.guess-and-config.sub-for-libxslt.patch");
    let v = ctx.ary_from(&[p]);
    ctx.const_set(nokogiri, "LIBXSLT_PATCHES", v);
    ctx.const_set(nokogiri, "LIBXSLT_DATETIME_ENABLED", Value::bool(true));
    Ok(())
}

// ---------------------------------------------------------------------
// Strings
// ---------------------------------------------------------------------

/// A UTF-8 Ruby String from libxml2 bytes (`NOKOGIRI_STR_NEW2`).
pub(crate) fn utf8(ctx: &Ctx, bytes: &[u8]) -> Value {
    ctx.str(bytes)
}

/// A String from a NUL-terminated libxml2 string, `nil` for NULL. The
/// C string is not freed.
pub(crate) unsafe fn xml_str(ctx: &Ctx, p: *const xml::xmlChar) -> Value {
    if p.is_null() {
        return Value::nil();
    }
    // SAFETY: the caller passes a NUL-terminated string.
    let s = unsafe { CStr::from_ptr(p as *const c_char) };
    utf8(ctx, s.to_bytes())
}

/// `xml_str`, then free the C string (which libxml2 handed to us).
pub(crate) unsafe fn xml_str_owned(ctx: &Ctx, p: *mut xml::xmlChar) -> Value {
    // SAFETY: as `xml_str`; the string is ours to free.
    unsafe {
        let v = xml_str(ctx, p);
        if !p.is_null() {
            xml::xml_free()(p as *mut c_void);
        }
        v
    }
}

/// The name `Check_Type` reports for a value of the wrong type: "nil" /
/// "true" / "false" for the immediates, the class name otherwise.
pub(crate) fn builtin_type_name(ctx: &Ctx, v: Value) -> String {
    if v.is_nil() {
        "nil".to_string()
    } else if v == Value::TRUE {
        "true".to_string()
    } else if v == Value::FALSE {
        "false".to_string()
    } else {
        ctx.class_name(v)
    }
}

/// The bytes of a String argument as a C string (`StringValueCStr`).
pub(crate) fn cstr(v: Value, ctx: &mut Ctx) -> Result<CString> {
    let bytes = ctx.str_vec(v)?;
    CString::new(bytes).map_err(|_| ctx.argument_error("string contains null byte"))
}

/// `cstr`, with `nil` allowed (a NULL for libxml2).
pub(crate) fn opt_cstr(v: Value, ctx: &mut Ctx) -> Result<Option<CString>> {
    if v.is_nil() {
        Ok(None)
    } else {
        cstr(v, ctx).map(Some)
    }
}

pub(crate) fn cptr(c: &Option<CString>) -> *const xml::xmlChar {
    match c {
        Some(c) => c.as_ptr() as *const xml::xmlChar,
        None => std::ptr::null(),
    }
}

// ---------------------------------------------------------------------
// Documents
// ---------------------------------------------------------------------

/// The payload of a `Nokogiri::XML::Document`: the `xmlDoc` it owns.
pub(crate) struct XmlDocument {
    pub doc: *mut xml::xmlDoc,
    /// Every Ruby object wrapping a node (or namespace) of this document
    /// (nokogiri's `node_cache`): a node's Ruby identity lives as long as
    /// the document.
    pub node_cache: Vec<Value>,
    /// Nodes unlinked from the tree (nokogiri's `unlinkedNodes`): they
    /// belong to the document and are released with it.
    pub unlinked: HashSet<usize>,
}

native!(XmlDocument, "Nokogiri::XML::Document", |d, m| {
    for v in &d.node_cache {
        m.mark(v);
    }
});

impl Drop for XmlDocument {
    fn drop(&mut self) {
        // SAFETY: the document and its unlinked nodes are ours; nothing
        // else refers to them once the Ruby object is unreachable (every
        // node wrapper marks its document, so they die together).
        unsafe {
            for &p in &self.unlinked {
                let node = p as *mut xml::xmlNode;
                match (*node).type_ {
                    xml::XML_ATTRIBUTE_NODE => xml::xmlFreePropList(node as *mut xml::xmlAttr),
                    xml::XML_NAMESPACE_DECL => xml::xmlFreeNs(node as *mut xml::xmlNs),
                    xml::XML_DTD_NODE => xml::xmlFreeDtd(node as *mut xml::xmlDtd),
                    _ => {
                        // An orphan is handed back to the document so
                        // `xmlFreeDoc` frees it (nokogiri's `dealloc`).
                        if (*node).parent.is_null() {
                            (*node).next = std::ptr::null_mut();
                            (*node).prev = std::ptr::null_mut();
                            xml::xmlAddChild(self.doc as *mut xml::xmlNode, node);
                        }
                    }
                }
            }
            (*self.doc)._private = std::ptr::null_mut();
            xml::xmlFreeDoc(self.doc);
        }
    }
}

/// The Ruby `Document` of `doc`, if it has one (`DOC_RUBY_OBJECT`).
pub(crate) unsafe fn doc_value(doc: *mut xml::xmlDoc) -> Option<Value> {
    if doc.is_null() {
        return None;
    }
    // SAFETY: `doc` is a live document; `_private` holds the Value bits
    // (or NULL).
    let p = unsafe { (*doc)._private };
    if p.is_null() {
        None
    } else {
        Some(Value(p as u64))
    }
}

pub(crate) unsafe fn doc_native<'a>(
    ctx: &Ctx,
    doc: *mut xml::xmlDoc,
) -> Option<&'a mut XmlDocument> {
    // SAFETY: as `Ctx::native`.
    ctx.native::<XmlDocument>(unsafe { doc_value(doc)? })
}

/// Remember `node` as unlinked from its document
/// (`noko_xml_document_pin_node`).
pub(crate) unsafe fn pin_node(ctx: &Ctx, node: *mut xml::xmlNode) {
    // SAFETY: a live node of a wrapped document.
    unsafe {
        if let Some(d) = doc_native(ctx, (*node).doc) {
            d.unlinked.insert(node as usize);
        }
    }
}

pub(crate) unsafe fn pin_namespace(ctx: &Ctx, ns: *mut xml::xmlNs, doc: *mut xml::xmlDoc) {
    // SAFETY: as `pin_node`.
    unsafe {
        if let Some(d) = doc_native(ctx, doc) {
            d.unlinked.insert(ns as usize);
        }
    }
}

pub(crate) fn doc_ptr(ctx: &mut Ctx, v: Value) -> Result<*mut xml::xmlDoc> {
    match ctx.native::<XmlDocument>(v) {
        Some(d) => Ok(d.doc),
        None => Err(ctx.argument_error("expected a Nokogiri::XML::Document")),
    }
}

/// Wrap a freshly created `xmlDoc` into a Ruby `Document` of `class`,
/// calling `initialize(*args)` (`noko_xml_document_wrap_with_init_args`).
pub(crate) fn wrap_document(
    ctx: &mut Ctx,
    class: Value,
    doc: *mut xml::xmlDoc,
    args: &[Value],
) -> Result<Value> {
    let rb = ctx.native_new(
        class,
        XmlDocument {
            doc,
            node_cache: vec![],
            unlinked: HashSet::new(),
        },
    )?;
    // SAFETY: `doc` is a live document we now own.
    unsafe { (*doc)._private = rb.0 as *mut c_void };
    ctx.ivar_set(rb, "@decorators", Value::nil())?;
    ctx.ivar_set(rb, "@errors", Value::nil())?;
    // The new object is reachable only from here until `initialize`
    // returns: root it (the GC does not scan the Rust stack).
    let len = ctx.temp_len();
    ctx.temp_push(rb);
    let r = ctx.funcall(rb, "initialize", args, None);
    ctx.temp_truncate(len);
    r?;
    Ok(rb)
}

// ---------------------------------------------------------------------
// Nodes
// ---------------------------------------------------------------------

/// The payload of a `Nokogiri::XML::Node` (and its subclasses): the node,
/// owned by its document.
pub(crate) struct XmlNode {
    pub node: *mut xml::xmlNode,
}

native!(XmlNode, "Nokogiri::XML::Node", |n, m| {
    // The document keeps the node's memory alive (`_xml_node_mark`).
    // SAFETY: a wrapped node is freed only with its document, which
    // cannot be collected while this wrapper is alive.
    unsafe {
        if let Some(doc) = doc_value((*n.node).doc) {
            m.mark(doc);
        }
    }
});

/// The libxml2 node of a `Node` (a `Document` answers its `xmlDoc`, which
/// starts with the same fields — `Noko_Node_Get_Struct`).
pub(crate) fn node_ptr(ctx: &mut Ctx, v: Value) -> Result<*mut xml::xmlNode> {
    if let Some(n) = ctx.native::<XmlNode>(v) {
        return Ok(n.node);
    }
    if let Some(d) = ctx.native::<XmlDocument>(v) {
        return Ok(d.doc as *mut xml::xmlNode);
    }
    Err(ctx.argument_error("node must be a Nokogiri::XML::Node"))
}

pub(crate) fn is_node(ctx: &Ctx, v: Value) -> bool {
    ctx.native::<XmlNode>(v).is_some() || ctx.native::<XmlDocument>(v).is_some()
}

/// The Ruby object of `node`, made now if it has none
/// (`noko_xml_node_wrap`). `class` overrides the class chosen by the
/// node's type.
pub(crate) fn wrap_node_as(
    ctx: &mut Ctx,
    class: Option<Value>,
    node: *mut xml::xmlNode,
) -> Result<Value> {
    // SAFETY: `node` is a live node of a live (or absent) document.
    unsafe {
        let ty = (*node).type_;
        if ty == xml::XML_DOCUMENT_NODE || ty == xml::XML_HTML_DOCUMENT_NODE {
            return doc_value(node as *mut xml::xmlDoc)
                .ok_or_else(|| ctx.runtime_error("document has no Ruby object"));
        }
        if ty == xml::XML_NAMESPACE_DECL {
            return wrap_namespace(ctx, node as *mut xml::xmlNs, std::ptr::null_mut());
        }
        let doc = (*node).doc;
        let doc_val = doc_value(doc);
        if !(*node)._private.is_null() && doc_val.is_some() {
            return Ok(Value((*node)._private as u64));
        }
        let c = classes();
        let class = class.unwrap_or(match ty {
            xml::XML_ELEMENT_NODE => c.element,
            xml::XML_TEXT_NODE => c.text,
            xml::XML_ATTRIBUTE_NODE => c.attr,
            xml::XML_ENTITY_REF_NODE => c.entity_ref,
            xml::XML_COMMENT_NODE => c.comment,
            xml::XML_DOCUMENT_FRAG_NODE => c.document_fragment,
            xml::XML_PI_NODE => c.pi,
            xml::XML_ENTITY_DECL => c.entity_decl,
            xml::XML_CDATA_SECTION_NODE => c.cdata,
            xml::XML_DTD_NODE => c.dtd,
            xml::XML_ATTRIBUTE_DECL => c.attribute_decl,
            xml::XML_ELEMENT_DECL => c.element_decl,
            _ => c.node,
        });
        let rb = ctx.native_new(class, XmlNode { node })?;
        (*node)._private = rb.0 as *mut c_void;
        if let Some(dv) = doc_val {
            if let Some(d) = doc_native(ctx, doc) {
                d.node_cache.push(rb);
            }
            ctx.funcall(dv, "decorate", &[rb], None)?;
        }
        Ok(rb)
    }
}

pub(crate) fn wrap_node(ctx: &mut Ctx, node: *mut xml::xmlNode) -> Result<Value> {
    wrap_node_as(ctx, None, node)
}

/// `wrap_node`, `nil` for NULL.
pub(crate) fn wrap_node_or_nil(ctx: &mut Ctx, node: *mut xml::xmlNode) -> Result<Value> {
    if node.is_null() {
        Ok(Value::nil())
    } else {
        wrap_node(ctx, node)
    }
}

// ---------------------------------------------------------------------
// Namespaces
// ---------------------------------------------------------------------

/// The payload of a `Nokogiri::XML::Namespace`. A namespace of a document
/// is owned by the document; one yielded by an XPath namespace axis is a
/// copy that this object frees.
pub(crate) struct XmlNamespace {
    pub ns: *mut xml::xmlNs,
    owned: bool,
}

native!(XmlNamespace, "Nokogiri::XML::Namespace");

impl Drop for XmlNamespace {
    fn drop(&mut self) {
        if !self.owned {
            return;
        }
        // SAFETY: an XPath copy (`xmlXPathNodeSetDupNs`) allocated with
        // libxml2's allocator, ours to free (`_xml_namespace_dealloc`).
        unsafe {
            let free = xml::xml_free();
            if !(*self.ns).href.is_null() {
                free((*self.ns).href as *mut c_void);
            }
            if !(*self.ns).prefix.is_null() {
                free((*self.ns).prefix as *mut c_void);
            }
            free(self.ns as *mut c_void);
        }
    }
}

/// `noko_xml_namespace_wrap`: `doc` NULL means an XPath copy the object
/// will own.
pub(crate) fn wrap_namespace(
    ctx: &mut Ctx,
    ns: *mut xml::xmlNs,
    doc: *mut xml::xmlDoc,
) -> Result<Value> {
    // SAFETY: `ns` is live; `_private` holds the Value bits or NULL.
    unsafe {
        if !(*ns)._private.is_null() {
            return Ok(Value((*ns)._private as u64));
        }
        let owned = doc.is_null();
        let rb = ctx.native_new(classes().namespace, XmlNamespace { ns, owned })?;
        if let Some(dv) = doc_value(doc) {
            ctx.ivar_set(rb, "@document", dv)?;
            if let Some(d) = doc_native(ctx, doc) {
                d.node_cache.push(rb);
            }
        }
        (*ns)._private = rb.0 as *mut c_void;
        Ok(rb)
    }
}

pub(crate) fn namespace_ptr(ctx: &mut Ctx, v: Value) -> Result<*mut xml::xmlNs> {
    match ctx.native::<XmlNamespace>(v) {
        Some(n) => Ok(n.ns),
        None => Err(ctx.argument_error("expected a Nokogiri::XML::Namespace")),
    }
}

// ---------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------

/// `XML_FROM_XPATH` of `xmlErrorDomain`.
const XML_FROM_XPATH: c_int = 12;

/// A libxml2 error copied out of the structured error callback, turned
/// into a `SyntaxError` after the library call returns (no Ruby runs
/// inside libxml2).
pub(crate) struct ErrorRecord {
    domain: c_int,
    code: c_int,
    message: Option<Vec<u8>>,
    level: c_int,
    file: Option<Vec<u8>>,
    line: c_int,
    str1: Option<Vec<u8>>,
    str2: Option<Vec<u8>>,
    str3: Option<Vec<u8>>,
    int1: c_int,
    column: c_int,
    path: Option<Vec<u8>>,
}

unsafe fn opt_bytes(p: *const c_char) -> Option<Vec<u8>> {
    if p.is_null() {
        None
    } else {
        // SAFETY: NUL-terminated.
        Some(unsafe { CStr::from_ptr(p) }.to_bytes().to_vec())
    }
}

/// The structured error handler: `data` is a `*mut Vec<ErrorRecord>`.
pub(crate) unsafe extern "C" fn collect_error(data: *mut c_void, error: *const xml::xmlError) {
    // SAFETY: libxml2 passes back the pointer we registered, and a live
    // error record.
    unsafe {
        if data.is_null() || error.is_null() {
            return;
        }
        let list = &mut *(data as *mut Vec<ErrorRecord>);
        let e = &*error;
        let path = if e.node.is_null() {
            None
        } else {
            let p = xml::xmlGetNodePath(e.node as *const xml::xmlNode);
            let path = opt_bytes(p as *const c_char);
            if !p.is_null() {
                xml::xml_free()(p as *mut c_void);
            }
            path
        };
        list.push(ErrorRecord {
            domain: e.domain,
            code: e.code,
            message: opt_bytes(e.message),
            level: e.level,
            file: opt_bytes(e.file),
            line: e.line,
            str1: opt_bytes(e.str1),
            str2: opt_bytes(e.str2),
            str3: opt_bytes(e.str3),
            int1: e.int1,
            column: e.int2,
            path,
        });
    }
}

fn opt_str(ctx: &Ctx, b: &Option<Vec<u8>>) -> Value {
    match b {
        Some(b) => utf8(ctx, b),
        None => Value::nil(),
    }
}

/// `noko_xml_syntax_error__wrap`: a `Nokogiri::XML::SyntaxError` (or
/// `XPath::SyntaxError`) carrying the error's fields.
pub(crate) fn syntax_error_value(ctx: &mut Ctx, e: &ErrorRecord) -> Result<Value> {
    let c = classes();
    let class = if e.domain == XML_FROM_XPATH {
        c.xpath_syntax_error
    } else {
        c.xml_syntax_error
    };
    let msg = opt_str(ctx, &e.message);
    let ex = ctx.funcall(class, "new", &[msg], None)?;
    ctx.temp_push(ex);
    ctx.ivar_set(ex, "@domain", Value::int(e.domain as i64))?;
    ctx.ivar_set(ex, "@code", Value::int(e.code as i64))?;
    ctx.ivar_set(ex, "@level", Value::int(e.level as i64))?;
    let v = opt_str(ctx, &e.file);
    ctx.ivar_set(ex, "@file", v)?;
    ctx.ivar_set(ex, "@line", Value::int(e.line as i64))?;
    let v = opt_str(ctx, &e.path);
    ctx.ivar_set(ex, "@path", v)?;
    let v = opt_str(ctx, &e.str1);
    ctx.ivar_set(ex, "@str1", v)?;
    let v = opt_str(ctx, &e.str2);
    ctx.ivar_set(ex, "@str2", v)?;
    let v = opt_str(ctx, &e.str3);
    ctx.ivar_set(ex, "@str3", v)?;
    ctx.ivar_set(ex, "@int1", Value::int(e.int1 as i64))?;
    ctx.ivar_set(ex, "@column", Value::int(e.column as i64))?;
    Ok(ex)
}

pub(crate) fn errors_to_array(ctx: &mut Ctx, errors: &[ErrorRecord]) -> Result<Value> {
    // Each `SyntaxError.new` runs Ruby: keep the ones made so far rooted
    // (`syntax_error_value` pushes each on the temp stack).
    let len = ctx.temp_len();
    let mut list = Vec::with_capacity(errors.len());
    let r = (|| -> Result<()> {
        for e in errors {
            list.push(syntax_error_value(ctx, e)?);
        }
        Ok(())
    })();
    let ary = ctx.ary_from(&list);
    ctx.temp_truncate(len);
    r?;
    Ok(ary)
}

/// Raise the exception object `ex`.
/// Stash the exception a callback raised: taken off the interpreter (so
/// the library call can wind down) and rooted on the temp stack until the
/// native method re-raises it with `raise` once the library has returned.
/// The trampoline unwinds the temp stack after the method.
pub(crate) fn stash_error(ctx: &mut Ctx) -> Value {
    let e = match ctx.error_take() {
        Some(e) => e,
        None => {
            ctx.runtime_error("callback failed without an exception");
            ctx.error_take().unwrap_or_default()
        }
    };
    ctx.temp_push(e);
    e
}

pub(crate) fn raise(ctx: &mut Ctx, ex: Value) -> Error {
    ctx.raise_exception(ex)
}

// ---------------------------------------------------------------------
// Ruby IO callbacks
// ---------------------------------------------------------------------

/// The context of a libxml2 IO callback that reads from / writes to a
/// Ruby IO. An exception raised by the IO is swallowed and reported to
/// libxml2 as an IO error (nokogiri's `noko_io_read` / `noko_io_write`
/// run the call under `rb_rescue` and answer -1), so a parse sees
/// "Unknown IO error" and a save stops.
pub(crate) struct IoCtx {
    /// The context of the native call the library call runs in,
    /// refreshed by each native method before it hands the IO to libxml2.
    pub ctx: *mut MrContext,
    pub io: Value,
}

impl IoCtx {
    pub fn new(ctx: &mut Ctx, io: Value) -> Self {
        IoCtx { ctx: ctx.raw(), io }
    }
}

/// `noko_io_read`: `io.read(len)` into libxml2's buffer; nil is EOF, an
/// exception or a non-String answer is an IO error.
pub(crate) unsafe extern "C" fn io_read(
    ctx: *mut c_void,
    buffer: *mut c_char,
    len: c_int,
) -> c_int {
    // SAFETY: `ctx` is the `IoCtx` registered by the caller, alive for
    // the whole library call; `buffer` has `len` bytes.
    unsafe {
        let c = &mut *(ctx as *mut IoCtx);
        let mut cx = Ctx::from_raw(c.ctx);
        let Ok(v) = cx.funcall(c.io, "read", &[Value::int(len as i64)], None) else {
            let _ = cx.error_take();
            return -1;
        };
        if v.is_nil() {
            return 0;
        }
        let Some(bytes) = try_bytes(&cx, v) else {
            return -1;
        };
        let n = bytes.len().min(len as usize);
        std::ptr::copy_nonoverlapping(bytes.as_ptr(), buffer as *mut u8, n);
        n as c_int
    }
}

/// `noko_io_write`: `io.write(chunk)`, the chunk tagged with the IO's
/// external encoding (binary when it has none).
pub(crate) unsafe extern "C" fn io_write(
    ctx: *mut c_void,
    buffer: *const c_char,
    len: c_int,
) -> c_int {
    // SAFETY: as `io_read`.
    unsafe {
        let c = &mut *(ctx as *mut IoCtx);
        let mut cx = Ctx::from_raw(c.ctx);
        let bytes = std::slice::from_raw_parts(buffer as *const u8, len as usize);
        // `chunk` is freshly allocated and, until it is handed to a call as
        // an argument, lives only in this Rust local — which the collector
        // does not scan. The `external_encoding` dispatch below re-enters
        // Ruby, and so allocates: without a root, a collection there sweeps
        // the chunk and `force_encoding` is then dispatched on a recycled
        // cell. `force_encoding`'s result needs the same treatment before
        // the `write` dispatch. Root them for the duration; the scope also
        // covers the `-1` early returns.
        let scope = cx.temp_len();
        let r = (|| -> Option<c_int> {
            let mut chunk = cx.bytes(bytes);
            cx.temp_push(chunk);
            let enc = match cx.funcall_if_exists(c.io, "external_encoding", &[]) {
                Ok(Some(enc)) => enc,
                Ok(None) => Value::nil(),
                Err(_) => return None,
            };
            if !enc.is_nil() {
                match cx.funcall(chunk, "force_encoding", &[enc], None) {
                    Ok(v) => {
                        chunk = v;
                        cx.temp_push(v);
                    }
                    Err(_) => return None,
                }
            }
            match cx.funcall(c.io, "write", &[chunk], None) {
                Ok(n) => Some(try_fixnum(&mut cx, n).unwrap_or(len as i64) as c_int),
                Err(_) => None,
            }
        })();
        cx.temp_truncate(scope);
        match r {
            Some(n) => n,
            None => {
                let _ = cx.error_take();
                -1
            }
        }
    }
}

pub(crate) unsafe extern "C" fn io_close(_ctx: *mut c_void) -> c_int {
    0
}
