//! Nokogiri's native half: what `ext/nokogiri` (nokogiri.so) provides,
//! written over the bundled libxml2 (`libxml2-src`, 2.13.8 with nokogiri's
//! patches). The gem's Ruby half is vendored unchanged under
//! `gem/nokogiri/`; `gem/nokogiri/nokogiri.rb` stands in for the C
//! extension and calls `String.__nokogiri_init`, which builds the class
//! tree and registers the native methods. See `doc/nokogiri.md`.
//!
//! Objects: a `Document` owns its `xmlDoc` and frees it (with every node
//! that was unlinked from it) when collected; a `Node` / `Namespace` /
//! `NodeSet` / `XPathContext` is a native object holding a libxml2 pointer.
//! Each `xmlNode` maps to at most one Ruby object, remembered in the
//! node's `_private` and kept alive by the document's `node_cache`
//! (nokogiri's scheme, so identity and lifetimes match).

use super::*;
use crate::alloc::GC;
use libxml2_src as xml;
use std::cell::RefCell;
use std::collections::HashSet;
use std::ffi::{CStr, CString, c_char, c_int, c_void};

mod document;
mod dtd;
mod misc;
mod node;
mod node_set;
mod xpath;

pub(crate) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(STRING_CLASS, "__nokogiri_init", nokogiri_init, 0);
}

/// The classes the native methods live on and wrap nodes into.
#[derive(Clone, Copy)]
#[allow(dead_code)]
pub(super) struct Classes {
    pub nokogiri: ClassId,
    pub xml: ClassId,
    pub node: ClassId,
    pub element: ClassId,
    pub character_data: ClassId,
    pub text: ClassId,
    pub cdata: ClassId,
    pub comment: ClassId,
    pub attr: ClassId,
    pub document: ClassId,
    pub document_fragment: ClassId,
    pub pi: ClassId,
    pub entity_ref: ClassId,
    pub dtd: ClassId,
    pub entity_decl: ClassId,
    pub element_decl: ClassId,
    pub attribute_decl: ClassId,
    pub element_content: ClassId,
    pub namespace: ClassId,
    pub node_set: ClassId,
    pub xpath_context: ClassId,
    pub syntax_error: ClassId,
    pub xml_syntax_error: ClassId,
    pub xpath_syntax_error: ClassId,
    pub html4: ClassId,
    pub html4_document: ClassId,
    pub encoding_handler: ClassId,
    pub entity_lookup: ClassId,
}

thread_local! {
    static CLASSES: RefCell<Option<Classes>> = const { RefCell::new(None) };
}

pub(super) fn classes() -> Classes {
    CLASSES.with(|c| c.borrow().expect("Nokogiri is not initialized"))
}

fn module(globals: &mut Globals, parent: ClassId, name: &str) -> ClassId {
    let id = IdentId::get_id(name);
    if let Some(v) = globals.store.get_constant_noautoload(parent, id) {
        return v.as_class_id();
    }
    globals.store.define_module_with_identid(id, parent).id()
}

fn class(globals: &mut Globals, parent: ClassId, name: &str, superclass: ClassId) -> ClassId {
    let id = IdentId::get_id(name);
    if let Some(v) = globals.store.get_constant_noautoload(parent, id) {
        return v.as_class_id();
    }
    let sup = globals.store.get_module(superclass);
    globals.store.define_class(name, sup, parent).id()
}

/// A class whose instances are native objects (`ObjTy::NATIVE`): their
/// ivars live in the heap table, never in the inline slots the JIT uses
/// for plain objects. Subclasses (the gem's, and the node kinds) inherit
/// the type.
fn native_class(globals: &mut Globals, parent: ClassId, name: &str, superclass: ClassId) -> ClassId {
    let id = IdentId::get_id(name);
    if let Some(v) = globals.store.get_constant_noautoload(parent, id) {
        return v.as_class_id();
    }
    let sup = globals.store.get_module(superclass);
    globals
        .store
        .define_class_with_instance_ty(name, sup, parent, ObjTy::NATIVE)
        .id()
}

/// String.__nokogiri_init -> nil
///
/// Build `Nokogiri` and the classes with native methods (the ones
/// `Init_nokogiri` defines), register the methods, set the version
/// constants. Called once by the extension stand-in before the gem's
/// Ruby files reopen the classes.
#[monoruby_builtin]
fn nokogiri_init(_: &mut Executor, globals: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    if CLASSES.with(|c| c.borrow().is_some()) {
        return Ok(Value::nil());
    }
    // SAFETY: plain library initialization.
    unsafe { xml::xmlInitParser() };
    let standard_error = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("StandardError"))
        .expect("StandardError")
        .as_class_id();
    let nokogiri = module(globals, OBJECT_CLASS, "Nokogiri");
    let xml_m = module(globals, nokogiri, "XML");
    let xpath_m = module(globals, xml_m, "XPath");
    for m in ["Gumbo", "HTML4", "HTML5", "XSLT"] {
        module(globals, nokogiri, m);
    }
    module(globals, xml_m, "SAX");
    let html4 = module(globals, nokogiri, "HTML4");
    module(globals, html4, "SAX");

    let syntax_error = class(globals, nokogiri, "SyntaxError", standard_error);
    let xml_syntax_error = class(globals, xml_m, "SyntaxError", syntax_error);
    let xpath_syntax_error = class(globals, xpath_m, "SyntaxError", xml_syntax_error);
    let node = native_class(globals, xml_m, "Node", OBJECT_CLASS);
    let element = class(globals, xml_m, "Element", node);
    let character_data = class(globals, xml_m, "CharacterData", node);
    let text = class(globals, xml_m, "Text", character_data);
    let cdata = class(globals, xml_m, "CDATA", text);
    let comment = class(globals, xml_m, "Comment", character_data);
    let attr = class(globals, xml_m, "Attr", node);
    let document = class(globals, xml_m, "Document", node);
    let document_fragment = class(globals, xml_m, "DocumentFragment", node);
    let pi = class(globals, xml_m, "ProcessingInstruction", node);
    let entity_ref = class(globals, xml_m, "EntityReference", node);
    let dtd = class(globals, xml_m, "DTD", node);
    let entity_decl = class(globals, xml_m, "EntityDecl", node);
    let element_decl = class(globals, xml_m, "ElementDecl", node);
    let attribute_decl = class(globals, xml_m, "AttributeDecl", node);
    let element_content = native_class(globals, xml_m, "ElementContent", OBJECT_CLASS);
    let namespace = native_class(globals, xml_m, "Namespace", OBJECT_CLASS);
    let node_set = native_class(globals, xml_m, "NodeSet", OBJECT_CLASS);
    let xpath_context = native_class(globals, xml_m, "XPathContext", OBJECT_CLASS);
    let html4_document = class(globals, html4, "Document", document);
    let encoding_handler = native_class(globals, nokogiri, "EncodingHandler", OBJECT_CLASS);
    let entity_lookup = class(globals, html4, "EntityLookup", OBJECT_CLASS);
    class(globals, html4, "ElementDescription", OBJECT_CLASS);

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
    };
    CLASSES.with(|cell| *cell.borrow_mut() = Some(c));

    misc::init(globals, &c);
    document::init(globals, &c);
    dtd::init(globals, &c);
    node::init(globals, &c);
    node_set::init(globals, &c);
    xpath::init(globals, &c);

    // Constants `Init_nokogiri` sets (version/info.rb reads them).
    let set = |globals: &mut Globals, name: &str, v: Value| {
        globals.set_constant(nokogiri, IdentId::get_id(name), v);
    };
    // SAFETY: a NUL-terminated static string of the library.
    let loaded = unsafe { CStr::from_ptr(xml::xmlParserVersion) }
        .to_string_lossy()
        .into_owned();
    set(globals, "LIBXML_COMPILED_VERSION", Value::string_from_str("2.13.8"));
    set(globals, "LIBXML_LOADED_VERSION", Value::string(loaded));
    set(globals, "LIBXML_ICONV_ENABLED", Value::bool(true));
    set(globals, "LIBXML_ZLIB_ENABLED", Value::bool(false));
    set(globals, "LIBXML_MEMORY_MANAGEMENT", Value::string_from_str("default"));
    set(globals, "PACKAGED_LIBRARIES", Value::bool(true));
    set(globals, "PRECOMPILED_LIBRARIES", Value::bool(false));
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
    set(
        globals,
        "LIBXML2_PATCHES",
        Value::array_from_vec(patches.iter().map(|p| Value::string_from_str(p)).collect()),
    );
    // libxslt is not bundled yet (doc/nokogiri.md, stage 6).
    set(globals, "LIBXSLT_COMPILED_VERSION", Value::string_from_str("0.0.0"));
    // The loaded version is the numeric form (`xsltEngineVersion`, "10143").
    set(globals, "LIBXSLT_LOADED_VERSION", Value::string_from_str("00000"));
    set(globals, "LIBXSLT_PATCHES", Value::array_from_vec(vec![]));
    set(globals, "LIBXSLT_DATETIME_ENABLED", Value::bool(false));
    Ok(Value::nil())
}

// ---------------------------------------------------------------------
// Strings
// ---------------------------------------------------------------------

/// A UTF-8 Ruby String from libxml2 bytes (`NOKOGIRI_STR_NEW2`).
pub(super) fn utf8(bytes: &[u8]) -> Value {
    Value::string_from_vec(bytes.to_vec())
}

/// A String from a NUL-terminated libxml2 string, `nil` for NULL. The
/// C string is not freed.
pub(super) unsafe fn xml_str(p: *const xml::xmlChar) -> Value {
    if p.is_null() {
        return Value::nil();
    }
    // SAFETY: the caller passes a NUL-terminated string.
    let s = unsafe { CStr::from_ptr(p as *const c_char) };
    utf8(s.to_bytes())
}

/// `xml_str`, then free the C string (which libxml2 handed to us).
pub(super) unsafe fn xml_str_owned(p: *mut xml::xmlChar) -> Value {
    // SAFETY: as `xml_str`; the string is ours to free.
    unsafe {
        let v = xml_str(p);
        if !p.is_null() {
            xml::xml_free()(p as *mut c_void);
        }
        v
    }
}

/// The bytes of a String argument as a C string (`StringValueCStr`).
pub(super) fn cstr(v: Value, store: &Store) -> Result<CString> {
    let bytes = v.expect_bytes(store)?;
    CString::new(bytes.to_vec()).map_err(|_| MonorubyErr::argumenterr("string contains null byte"))
}

/// `cstr`, with `nil` allowed (a NULL for libxml2).
pub(super) fn opt_cstr(v: Value, store: &Store) -> Result<Option<CString>> {
    if v.is_nil() { Ok(None) } else { cstr(v, store).map(Some) }
}

pub(super) fn cptr(c: &Option<CString>) -> *const xml::xmlChar {
    match c {
        Some(c) => c.as_ptr() as *const xml::xmlChar,
        None => std::ptr::null(),
    }
}

// ---------------------------------------------------------------------
// Documents
// ---------------------------------------------------------------------

/// The payload of a `Nokogiri::XML::Document`: the `xmlDoc` it owns.
pub(super) struct XmlDocument {
    pub doc: *mut xml::xmlDoc,
    /// Every Ruby object wrapping a node (or namespace) of this document
    /// (nokogiri's `node_cache`): a node's Ruby identity lives as long as
    /// the document.
    pub node_cache: Vec<Value>,
    /// Nodes unlinked from the tree (nokogiri's `unlinkedNodes`): they
    /// belong to the document and are released with it.
    pub unlinked: HashSet<usize>,
}

impl NativeData for XmlDocument {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        for v in &self.node_cache {
            v.mark(alloc);
        }
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

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
pub(super) unsafe fn doc_value(doc: *mut xml::xmlDoc) -> Option<Value> {
    if doc.is_null() {
        return None;
    }
    // SAFETY: `doc` is a live document; `_private` holds the Value bits
    // (or NULL).
    let p = unsafe { (*doc)._private };
    if p.is_null() { None } else { Some(Value::from_u64(p as u64)) }
}

/// The native payload of a native object, detached from the borrow of
/// the (Copy) Value handle.
pub(super) unsafe fn native_mut<'a, T: NativeData>(mut v: Value) -> Option<&'a mut T> {
    let rv: *mut RValue = v.try_rvalue_mut()?;
    // SAFETY: the object is alive (the caller holds a Value to it); the
    // reference is used within one native call.
    unsafe {
        if (*rv).ty() != ObjTy::NATIVE {
            return None;
        }
        (*rv).as_native_mut().as_any_mut().downcast_mut::<T>()
    }
}

pub(super) unsafe fn doc_native<'a>(doc: *mut xml::xmlDoc) -> Option<&'a mut XmlDocument> {
    // SAFETY: as `native_mut`.
    unsafe { native_mut::<XmlDocument>(doc_value(doc)?) }
}

/// Remember `node` as unlinked from its document
/// (`noko_xml_document_pin_node`).
pub(super) unsafe fn pin_node(node: *mut xml::xmlNode) {
    // SAFETY: a live node of a wrapped document.
    unsafe {
        if let Some(d) = doc_native((*node).doc) {
            d.unlinked.insert(node as usize);
        }
    }
}

pub(super) unsafe fn pin_namespace(ns: *mut xml::xmlNs, doc: *mut xml::xmlDoc) {
    // SAFETY: as `pin_node`.
    unsafe {
        if let Some(d) = doc_native(doc) {
            d.unlinked.insert(ns as usize);
        }
    }
}

pub(super) fn doc_ptr(v: Value) -> Result<*mut xml::xmlDoc> {
    match v.try_native::<XmlDocument>() {
        Some(d) => Ok(d.doc),
        None => Err(MonorubyErr::argumenterr("expected a Nokogiri::XML::Document")),
    }
}

/// Wrap a freshly created `xmlDoc` into a Ruby `Document` of `class`,
/// calling `initialize(*args)` (`noko_xml_document_wrap_with_init_args`).
pub(super) fn wrap_document(
    vm: &mut Executor,
    globals: &mut Globals,
    class: ClassId,
    doc: *mut xml::xmlDoc,
    args: &[Value],
) -> Result<Value> {
    let rb = Value::new_native(
        class,
        Box::new(XmlDocument {
            doc,
            node_cache: vec![],
            unlinked: HashSet::new(),
        }),
    );
    // SAFETY: `doc` is a live document we now own.
    unsafe { (*doc)._private = rb.id() as *mut c_void };
    globals.store.set_ivar(rb, IdentId::get_id("@decorators"), Value::nil())?;
    globals.store.set_ivar(rb, IdentId::get_id("@errors"), Value::nil())?;
    // The new object is reachable only from here until `initialize`
    // returns: root it (the GC does not scan the Rust stack).
    let len = vm.temp_len();
    vm.temp_push(rb);
    let r = vm.invoke_method_inner(globals, IdentId::INITIALIZE, rb, args, None, None);
    vm.temp_clear(len);
    r?;
    Ok(rb)
}

// ---------------------------------------------------------------------
// Nodes
// ---------------------------------------------------------------------

/// The payload of a `Nokogiri::XML::Node` (and its subclasses): the node,
/// owned by its document.
pub(super) struct XmlNode {
    pub node: *mut xml::xmlNode,
}

impl NativeData for XmlNode {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        // The document keeps the node's memory alive (`_xml_node_mark`).
        // SAFETY: a wrapped node is freed only with its document, which
        // cannot be collected while this wrapper is alive.
        unsafe {
            if let Some(doc) = doc_value((*self.node).doc) {
                doc.mark(alloc);
            }
        }
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

/// The libxml2 node of a `Node` (a `Document` answers its `xmlDoc`, which
/// starts with the same fields — `Noko_Node_Get_Struct`).
pub(super) fn node_ptr(v: Value) -> Result<*mut xml::xmlNode> {
    if let Some(n) = v.try_native::<XmlNode>() {
        return Ok(n.node);
    }
    if let Some(d) = v.try_native::<XmlDocument>() {
        return Ok(d.doc as *mut xml::xmlNode);
    }
    Err(MonorubyErr::argumenterr("node must be a Nokogiri::XML::Node"))
}

pub(super) fn is_node(v: Value) -> bool {
    v.try_native::<XmlNode>().is_some() || v.try_native::<XmlDocument>().is_some()
}

/// The Ruby object of `node`, made now if it has none
/// (`noko_xml_node_wrap`). `class` overrides the class chosen by the
/// node's type.
pub(super) fn wrap_node_as(
    vm: &mut Executor,
    globals: &mut Globals,
    class: Option<ClassId>,
    node: *mut xml::xmlNode,
) -> Result<Value> {
    // SAFETY: `node` is a live node of a live (or absent) document.
    unsafe {
        let ty = (*node).type_;
        if ty == xml::XML_DOCUMENT_NODE || ty == xml::XML_HTML_DOCUMENT_NODE {
            return doc_value(node as *mut xml::xmlDoc)
                .ok_or_else(|| MonorubyErr::runtimeerr("document has no Ruby object"));
        }
        if ty == xml::XML_NAMESPACE_DECL {
            return wrap_namespace(globals, node as *mut xml::xmlNs, std::ptr::null_mut());
        }
        let doc = (*node).doc;
        let doc_val = doc_value(doc);
        if !(*node)._private.is_null() && doc_val.is_some() {
            return Ok(Value::from_u64((*node)._private as u64));
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
        let rb = Value::new_native(class, Box::new(XmlNode { node }));
        (*node)._private = rb.id() as *mut c_void;
        if let Some(dv) = doc_val {
            if let Some(d) = doc_native(doc) {
                d.node_cache.push(rb);
            }
            vm.invoke_method_inner(globals, IdentId::get_id("decorate"), dv, &[rb], None, None)?;
        }
        Ok(rb)
    }
}

pub(super) fn wrap_node(
    vm: &mut Executor,
    globals: &mut Globals,
    node: *mut xml::xmlNode,
) -> Result<Value> {
    wrap_node_as(vm, globals, None, node)
}

/// `wrap_node`, `nil` for NULL.
pub(super) fn wrap_node_or_nil(
    vm: &mut Executor,
    globals: &mut Globals,
    node: *mut xml::xmlNode,
) -> Result<Value> {
    if node.is_null() { Ok(Value::nil()) } else { wrap_node(vm, globals, node) }
}

// ---------------------------------------------------------------------
// Namespaces
// ---------------------------------------------------------------------

/// The payload of a `Nokogiri::XML::Namespace`. A namespace of a document
/// is owned by the document; one yielded by an XPath namespace axis is a
/// copy that this object frees.
pub(super) struct XmlNamespace {
    pub ns: *mut xml::xmlNs,
    owned: bool,
}

impl NativeData for XmlNamespace {
    fn mark(&self, _alloc: &mut alloc::Allocator<RValue>) {}
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

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
pub(super) fn wrap_namespace(
    globals: &mut Globals,
    ns: *mut xml::xmlNs,
    doc: *mut xml::xmlDoc,
) -> Result<Value> {
    // SAFETY: `ns` is live; `_private` holds the Value bits or NULL.
    unsafe {
        if !(*ns)._private.is_null() {
            return Ok(Value::from_u64((*ns)._private as u64));
        }
        let owned = doc.is_null();
        let rb = Value::new_native(classes().namespace, Box::new(XmlNamespace { ns, owned }));
        if let Some(dv) = doc_value(doc) {
            globals.store.set_ivar(rb, IdentId::get_id("@document"), dv)?;
            if let Some(d) = doc_native(doc) {
                d.node_cache.push(rb);
            }
        }
        (*ns)._private = rb.id() as *mut c_void;
        Ok(rb)
    }
}

pub(super) fn namespace_ptr(v: Value) -> Result<*mut xml::xmlNs> {
    match v.try_native::<XmlNamespace>() {
        Some(n) => Ok(n.ns),
        None => Err(MonorubyErr::argumenterr("expected a Nokogiri::XML::Namespace")),
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
pub(super) struct ErrorRecord {
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
pub(super) unsafe extern "C" fn collect_error(data: *mut c_void, error: *const xml::xmlError) {
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

fn opt_str(b: &Option<Vec<u8>>) -> Value {
    match b {
        Some(b) => utf8(b),
        None => Value::nil(),
    }
}

/// `noko_xml_syntax_error__wrap`: a `Nokogiri::XML::SyntaxError` (or
/// `XPath::SyntaxError`) carrying the error's fields.
pub(super) fn syntax_error_value(
    vm: &mut Executor,
    globals: &mut Globals,
    e: &ErrorRecord,
) -> Result<Value> {
    let c = classes();
    let class = if e.domain == XML_FROM_XPATH { c.xpath_syntax_error } else { c.xml_syntax_error };
    let class_val = globals.store.get_module(class).as_val();
    let ex = vm.invoke_method_inner(globals, IdentId::NEW, class_val, &[opt_str(&e.message)], None, None)?;
    let set = |globals: &mut Globals, name: &str, v: Value| -> Result<()> {
        globals.store.set_ivar(ex, IdentId::get_id(name), v)
    };
    set(globals, "@domain", Value::integer(e.domain as i64))?;
    set(globals, "@code", Value::integer(e.code as i64))?;
    set(globals, "@level", Value::integer(e.level as i64))?;
    set(globals, "@file", opt_str(&e.file))?;
    set(globals, "@line", Value::integer(e.line as i64))?;
    set(globals, "@path", opt_str(&e.path))?;
    set(globals, "@str1", opt_str(&e.str1))?;
    set(globals, "@str2", opt_str(&e.str2))?;
    set(globals, "@str3", opt_str(&e.str3))?;
    set(globals, "@int1", Value::integer(e.int1 as i64))?;
    set(globals, "@column", Value::integer(e.column as i64))?;
    Ok(ex)
}

pub(super) fn errors_to_array(
    vm: &mut Executor,
    globals: &mut Globals,
    errors: &[ErrorRecord],
) -> Result<Value> {
    // Each `SyntaxError.new` runs Ruby: keep the ones made so far rooted.
    let len = vm.temp_len();
    let mut build = || -> Result<Vec<Value>> {
        for e in errors {
            let ex = syntax_error_value(vm, globals, e)?;
            vm.temp_push(ex);
        }
        Ok((len..vm.temp_len()).map(|i| vm.temp_at(i)).collect())
    };
    let r = build();
    vm.temp_clear(len);
    Ok(Value::array_from_vec(r?))
}

/// Raise the exception object `ex`.
pub(super) fn raise(ex: Value) -> MonorubyErr {
    match ex.is_exception() {
        Some(inner) => MonorubyErr::new_from_exception(inner).with_original(ex),
        None => MonorubyErr::typeerr("exception class/object expected"),
    }
}

// ---------------------------------------------------------------------
// Ruby IO callbacks
// ---------------------------------------------------------------------

/// The context of a libxml2 IO callback that reads from / writes to a
/// Ruby IO: an exception raised by the IO is kept here and re-raised after
/// the library call returns.
pub(super) struct IoCtx {
    pub vm: *mut Executor,
    pub globals: *mut Globals,
    pub io: Value,
    pub error: Option<MonorubyErr>,
}

impl IoCtx {
    pub fn new(vm: &mut Executor, globals: &mut Globals, io: Value) -> Self {
        IoCtx {
            vm,
            globals,
            io,
            error: None,
        }
    }
}

/// `noko_io_read`: `io.read(len)` into libxml2's buffer.
pub(super) unsafe extern "C" fn io_read(ctx: *mut c_void, buffer: *mut c_char, len: c_int) -> c_int {
    // SAFETY: `ctx` is the `IoCtx` registered by the caller, alive for
    // the whole library call; `buffer` has `len` bytes.
    unsafe {
        let c = &mut *(ctx as *mut IoCtx);
        let vm = &mut *c.vm;
        let globals = &mut *c.globals;
        let read = IdentId::get_id("read");
        match vm.invoke_method_inner(globals, read, c.io, &[Value::integer(len as i64)], None, None) {
            Err(e) => {
                c.error = Some(e);
                -1
            }
            Ok(v) => {
                if v.is_nil() {
                    return 0;
                }
                let Ok(bytes) = v.expect_bytes(&globals.store) else {
                    return -1;
                };
                let n = bytes.len().min(len as usize);
                std::ptr::copy_nonoverlapping(bytes.as_ptr(), buffer as *mut u8, n);
                n as c_int
            }
        }
    }
}

/// `noko_io_write`: `io.write(chunk)`, the chunk tagged with the IO's
/// external encoding (binary when it has none).
pub(super) unsafe extern "C" fn io_write(ctx: *mut c_void, buffer: *const c_char, len: c_int) -> c_int {
    // SAFETY: as `io_read`.
    unsafe {
        let c = &mut *(ctx as *mut IoCtx);
        let vm = &mut *c.vm;
        let globals = &mut *c.globals;
        let bytes = std::slice::from_raw_parts(buffer as *const u8, len as usize);
        let mut chunk = Value::bytes(bytes.to_vec());
        let external_encoding = IdentId::get_id("external_encoding");
        let enc = match vm.invoke_method_if_exists(globals, external_encoding, c.io, &[], None, None) {
            Ok(Some(enc)) => enc,
            Ok(None) => Value::nil(),
            Err(e) => {
                c.error = Some(e);
                return -1;
            }
        };
        if !enc.is_nil() {
            let force = IdentId::get_id("force_encoding");
            match vm.invoke_method_inner(globals, force, chunk, &[enc], None, None) {
                Ok(v) => chunk = v,
                Err(e) => {
                    c.error = Some(e);
                    return -1;
                }
            }
        }
        match vm.invoke_method_inner(globals, IdentId::get_id("write"), c.io, &[chunk], None, None) {
            Ok(n) => n.try_fixnum().unwrap_or(len as i64) as c_int,
            Err(e) => {
                c.error = Some(e);
                -1
            }
        }
    }
}

pub(super) unsafe extern "C" fn io_close(_ctx: *mut c_void) -> c_int {
    0
}
