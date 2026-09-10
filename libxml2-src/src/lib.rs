//! Hand-written FFI for the vendored libxml2 (2.13.8): the structs
//! monoruby's Nokogiri reads (the tree, node sets, XPath objects, errors)
//! and the entry points it calls. Layouts follow `include/libxml/*.h` of
//! that exact version; the C library is built and linked by `build.rs`.
//!
//! Only what is used is declared. Everything here is `unsafe` to call;
//! the owners of the pointers are the Nokogiri objects in monoruby.

#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use core::ffi::{c_char, c_double, c_int, c_long, c_uchar, c_ulong, c_ushort, c_void};

pub type xmlChar = c_uchar;

/// `xmlElementType`.
pub const XML_ELEMENT_NODE: c_int = 1;
pub const XML_ATTRIBUTE_NODE: c_int = 2;
pub const XML_TEXT_NODE: c_int = 3;
pub const XML_CDATA_SECTION_NODE: c_int = 4;
pub const XML_ENTITY_REF_NODE: c_int = 5;
pub const XML_ENTITY_NODE: c_int = 6;
pub const XML_PI_NODE: c_int = 7;
pub const XML_COMMENT_NODE: c_int = 8;
pub const XML_DOCUMENT_NODE: c_int = 9;
pub const XML_DOCUMENT_TYPE_NODE: c_int = 10;
pub const XML_DOCUMENT_FRAG_NODE: c_int = 11;
pub const XML_NOTATION_NODE: c_int = 12;
pub const XML_HTML_DOCUMENT_NODE: c_int = 13;
pub const XML_DTD_NODE: c_int = 14;
pub const XML_ELEMENT_DECL: c_int = 15;
pub const XML_ATTRIBUTE_DECL: c_int = 16;
pub const XML_ENTITY_DECL: c_int = 17;
pub const XML_NAMESPACE_DECL: c_int = 18;
pub const XML_XINCLUDE_START: c_int = 19;
pub const XML_XINCLUDE_END: c_int = 20;

/// `xmlXPathObjectType`.
pub const XPATH_UNDEFINED: c_int = 0;
pub const XPATH_NODESET: c_int = 1;
pub const XPATH_BOOLEAN: c_int = 2;
pub const XPATH_NUMBER: c_int = 3;
pub const XPATH_STRING: c_int = 4;
pub const XPATH_USERS: c_int = 8;
pub const XPATH_XSLT_TREE: c_int = 9;

/// `xmlErrorLevel`.
pub const XML_ERR_NONE: c_int = 0;
pub const XML_ERR_WARNING: c_int = 1;
pub const XML_ERR_ERROR: c_int = 2;
pub const XML_ERR_FATAL: c_int = 3;

/// `xmlParserOption` (the values Nokogiri's `ParseOptions` carries).
pub const XML_PARSE_RECOVER: c_int = 1 << 0;
pub const XML_PARSE_NOENT: c_int = 1 << 1;
pub const XML_PARSE_DTDLOAD: c_int = 1 << 2;
pub const XML_PARSE_DTDATTR: c_int = 1 << 3;
pub const XML_PARSE_DTDVALID: c_int = 1 << 4;
pub const XML_PARSE_NOERROR: c_int = 1 << 5;
pub const XML_PARSE_NOWARNING: c_int = 1 << 6;
pub const XML_PARSE_PEDANTIC: c_int = 1 << 7;
pub const XML_PARSE_NOBLANKS: c_int = 1 << 8;
pub const XML_PARSE_SAX1: c_int = 1 << 9;
pub const XML_PARSE_XINCLUDE: c_int = 1 << 10;
pub const XML_PARSE_NONET: c_int = 1 << 11;
pub const XML_PARSE_NODICT: c_int = 1 << 12;
pub const XML_PARSE_NSCLEAN: c_int = 1 << 13;
pub const XML_PARSE_NOCDATA: c_int = 1 << 14;
pub const XML_PARSE_NOXINCNODE: c_int = 1 << 15;
pub const XML_PARSE_COMPACT: c_int = 1 << 16;
pub const XML_PARSE_OLD10: c_int = 1 << 17;
pub const XML_PARSE_NOBASEFIX: c_int = 1 << 18;
pub const XML_PARSE_HUGE: c_int = 1 << 19;
pub const XML_PARSE_OLDSAX: c_int = 1 << 20;
pub const XML_PARSE_IGNORE_ENC: c_int = 1 << 21;
pub const XML_PARSE_BIG_LINES: c_int = 1 << 22;
pub const XML_PARSE_NO_XXE: c_int = 1 << 23;

/// `xmlSaveOption`.
pub const XML_SAVE_FORMAT: c_int = 1 << 0;
pub const XML_SAVE_NO_DECL: c_int = 1 << 1;
pub const XML_SAVE_NO_EMPTY: c_int = 1 << 2;
pub const XML_SAVE_NO_XHTML: c_int = 1 << 3;
pub const XML_SAVE_XHTML: c_int = 1 << 4;
pub const XML_SAVE_AS_XML: c_int = 1 << 5;
pub const XML_SAVE_AS_HTML: c_int = 1 << 6;
pub const XML_SAVE_WSNONSIG: c_int = 1 << 7;

/// `xmlDocProperties`.
pub const XML_DOC_HTML: c_int = 1 << 7;

#[repr(C)]
pub struct xmlNs {
    pub next: *mut xmlNs,
    pub type_: c_int,
    pub href: *const xmlChar,
    pub prefix: *const xmlChar,
    pub _private: *mut c_void,
    pub context: *mut xmlDoc,
}

#[repr(C)]
pub struct xmlDtd {
    pub _private: *mut c_void,
    pub type_: c_int,
    pub name: *const xmlChar,
    pub children: *mut xmlNode,
    pub last: *mut xmlNode,
    pub parent: *mut xmlDoc,
    pub next: *mut xmlNode,
    pub prev: *mut xmlNode,
    pub doc: *mut xmlDoc,
    pub notations: *mut c_void,
    pub elements: *mut c_void,
    pub attributes: *mut c_void,
    pub entities: *mut c_void,
    pub ExternalID: *const xmlChar,
    pub SystemID: *const xmlChar,
    pub pentities: *mut c_void,
}

#[repr(C)]
pub struct xmlAttr {
    pub _private: *mut c_void,
    pub type_: c_int,
    pub name: *const xmlChar,
    pub children: *mut xmlNode,
    pub last: *mut xmlNode,
    pub parent: *mut xmlNode,
    pub next: *mut xmlAttr,
    pub prev: *mut xmlAttr,
    pub doc: *mut xmlDoc,
    pub ns: *mut xmlNs,
    pub atype: c_int,
    pub psvi: *mut c_void,
    pub id: *mut c_void,
}

/// `xmlEntityType`.
pub const XML_INTERNAL_GENERAL_ENTITY: c_int = 1;
pub const XML_EXTERNAL_GENERAL_PARSED_ENTITY: c_int = 2;
pub const XML_EXTERNAL_GENERAL_UNPARSED_ENTITY: c_int = 3;
pub const XML_INTERNAL_PARAMETER_ENTITY: c_int = 4;
pub const XML_EXTERNAL_PARAMETER_ENTITY: c_int = 5;
pub const XML_INTERNAL_PREDEFINED_ENTITY: c_int = 6;

/// An `XML_ENTITY_DECL` node (`entities.h`).
#[repr(C)]
pub struct xmlEntity {
    pub _private: *mut c_void,
    pub type_: c_int,
    pub name: *const xmlChar,
    pub children: *mut xmlNode,
    pub last: *mut xmlNode,
    pub parent: *mut xmlDtd,
    pub next: *mut xmlNode,
    pub prev: *mut xmlNode,
    pub doc: *mut xmlDoc,
    pub orig: *mut xmlChar,
    pub content: *mut xmlChar,
    pub length: c_int,
    pub etype: c_int,
    pub ExternalID: *const xmlChar,
    pub SystemID: *const xmlChar,
    pub nexte: *mut xmlEntity,
    pub URI: *const xmlChar,
    pub owner: c_int,
    pub flags: c_int,
    pub expandedSize: c_ulong,
}

/// An `XML_ELEMENT_DECL` node.
#[repr(C)]
pub struct xmlElement {
    pub _private: *mut c_void,
    pub type_: c_int,
    pub name: *const xmlChar,
    pub children: *mut xmlNode,
    pub last: *mut xmlNode,
    pub parent: *mut xmlDtd,
    pub next: *mut xmlNode,
    pub prev: *mut xmlNode,
    pub doc: *mut xmlDoc,
    pub etype: c_int,
    pub content: *mut xmlElementContent,
    pub attributes: *mut xmlAttribute,
    pub prefix: *const xmlChar,
    pub contModel: *mut c_void,
}

/// An `XML_ATTRIBUTE_DECL` node.
#[repr(C)]
pub struct xmlAttribute {
    pub _private: *mut c_void,
    pub type_: c_int,
    pub name: *const xmlChar,
    pub children: *mut xmlNode,
    pub last: *mut xmlNode,
    pub parent: *mut xmlDtd,
    pub next: *mut xmlNode,
    pub prev: *mut xmlNode,
    pub doc: *mut xmlDoc,
    pub nexth: *mut xmlAttribute,
    pub atype: c_int,
    pub def: c_int,
    pub defaultValue: *const xmlChar,
    pub tree: *mut xmlEnumeration,
    pub prefix: *const xmlChar,
    pub elem: *const xmlChar,
}

#[repr(C)]
pub struct xmlNotation {
    pub name: *const xmlChar,
    pub PublicID: *const xmlChar,
    pub SystemID: *const xmlChar,
}

#[repr(C)]
pub struct xmlEnumeration {
    pub next: *mut xmlEnumeration,
    pub name: *const xmlChar,
}

/// The content model tree of an element declaration.
#[repr(C)]
pub struct xmlElementContent {
    pub type_: c_int,
    pub ocur: c_int,
    pub name: *const xmlChar,
    pub c1: *mut xmlElementContent,
    pub c2: *mut xmlElementContent,
    pub parent: *mut xmlElementContent,
    pub prefix: *const xmlChar,
}

#[repr(C)]
pub struct xmlNode {
    pub _private: *mut c_void,
    pub type_: c_int,
    pub name: *const xmlChar,
    pub children: *mut xmlNode,
    pub last: *mut xmlNode,
    pub parent: *mut xmlNode,
    pub next: *mut xmlNode,
    pub prev: *mut xmlNode,
    pub doc: *mut xmlDoc,
    pub ns: *mut xmlNs,
    pub content: *mut xmlChar,
    pub properties: *mut xmlAttr,
    pub nsDef: *mut xmlNs,
    pub psvi: *mut c_void,
    pub line: c_ushort,
    pub extra: c_ushort,
}

#[repr(C)]
pub struct xmlDoc {
    pub _private: *mut c_void,
    pub type_: c_int,
    pub name: *mut c_char,
    pub children: *mut xmlNode,
    pub last: *mut xmlNode,
    pub parent: *mut xmlNode,
    pub next: *mut xmlNode,
    pub prev: *mut xmlNode,
    pub doc: *mut xmlDoc,
    pub compression: c_int,
    pub standalone: c_int,
    pub intSubset: *mut xmlDtd,
    pub extSubset: *mut xmlDtd,
    pub oldNs: *mut xmlNs,
    pub version: *const xmlChar,
    pub encoding: *const xmlChar,
    pub ids: *mut c_void,
    pub refs: *mut c_void,
    pub URL: *const xmlChar,
    pub charset: c_int,
    pub dict: *mut c_void,
    pub psvi: *mut c_void,
    pub parseFlags: c_int,
    pub properties: c_int,
}

#[repr(C)]
pub struct xmlNodeSet {
    pub nodeNr: c_int,
    pub nodeMax: c_int,
    pub nodeTab: *mut *mut xmlNode,
}

#[repr(C)]
pub struct xmlXPathObject {
    pub type_: c_int,
    pub nodesetval: *mut xmlNodeSet,
    pub boolval: c_int,
    pub floatval: c_double,
    pub stringval: *mut xmlChar,
    pub user: *mut c_void,
    pub index: c_int,
    pub user2: *mut c_void,
    pub index2: c_int,
}

#[repr(C)]
pub struct xmlError {
    pub domain: c_int,
    pub code: c_int,
    pub message: *mut c_char,
    pub level: c_int,
    pub file: *mut c_char,
    pub line: c_int,
    pub str1: *mut c_char,
    pub str2: *mut c_char,
    pub str3: *mut c_char,
    pub int1: c_int,
    pub int2: c_int,
    pub ctxt: *mut c_void,
    pub node: *mut c_void,
}

/// Opaque handles.
#[repr(C)]
pub struct xmlParserCtxt {
    _opaque: [u8; 0],
}
/// Only the leading fields are declared (the ones read or written); the
/// struct is always allocated by libxml2.
#[repr(C)]
pub struct xmlXPathContext {
    pub doc: *mut xmlDoc,
    pub node: *mut xmlNode,
    _rest: [u8; 0],
}
/// As `xmlXPathContext`: the leading fields only.
#[repr(C)]
pub struct xmlXPathParserContext {
    pub cur: *const xmlChar,
    pub base: *const xmlChar,
    pub error: c_int,
    pub context: *mut xmlXPathContext,
    pub value: *mut xmlXPathObject,
    _rest: [u8; 0],
}
#[repr(C)]
pub struct xmlSaveCtxt {
    _opaque: [u8; 0],
}
/// `xmlCharEncodingHandler`: only the leading `name` is declared.
#[repr(C)]
pub struct xmlCharEncodingHandler {
    pub name: *mut c_char,
    _rest: [u8; 0],
}
#[repr(C)]
pub struct htmlEntityDesc {
    pub value: core::ffi::c_uint,
    pub name: *const c_char,
    pub desc: *const c_char,
}
#[repr(C)]
pub struct xmlDOMWrapCtxt {
    _opaque: [u8; 0],
}
#[repr(C)]
pub struct xmlBuffer {
    _opaque: [u8; 0],
}
#[repr(C)]
pub struct xmlValidCtxt {
    _opaque: [u8; 0],
}

pub type xmlHashScanner =
    Option<unsafe extern "C" fn(payload: *mut c_void, data: *mut c_void, name: *const xmlChar)>;

pub type xmlStructuredErrorFunc =
    Option<unsafe extern "C" fn(userData: *mut c_void, error: *const xmlError)>;
pub type xmlInputReadCallback =
    Option<unsafe extern "C" fn(context: *mut c_void, buffer: *mut c_char, len: c_int) -> c_int>;
pub type xmlInputCloseCallback = Option<unsafe extern "C" fn(context: *mut c_void) -> c_int>;
pub type xmlOutputWriteCallback = Option<
    unsafe extern "C" fn(context: *mut c_void, buffer: *const c_char, len: c_int) -> c_int,
>;
pub type xmlOutputCloseCallback = Option<unsafe extern "C" fn(context: *mut c_void) -> c_int>;
pub type xmlFreeFunc = Option<unsafe extern "C" fn(mem: *mut c_void)>;
pub type xmlMallocFunc = Option<unsafe extern "C" fn(size: usize) -> *mut c_void>;
pub type xmlReallocFunc = Option<unsafe extern "C" fn(mem: *mut c_void, size: usize) -> *mut c_void>;
pub type xmlStrdupFunc = Option<unsafe extern "C" fn(str: *const c_char) -> *mut c_char>;
pub type xmlXPathFunction =
    Option<unsafe extern "C" fn(ctxt: *mut xmlXPathParserContext, nargs: c_int)>;
pub type xmlXPathFuncLookupFunc = Option<
    unsafe extern "C" fn(
        ctxt: *mut c_void,
        name: *const xmlChar,
        ns_uri: *const xmlChar,
    ) -> xmlXPathFunction,
>;

unsafe extern "C" {
    // ---- library ----
    pub fn xmlInitParser();
    pub fn xmlMemGet(
        freeFunc: *mut xmlFreeFunc,
        mallocFunc: *mut xmlMallocFunc,
        reallocFunc: *mut xmlReallocFunc,
        strdupFunc: *mut xmlStrdupFunc,
    ) -> c_int;
    pub static xmlParserVersion: *const c_char;

    // ---- parsing ----
    pub fn xmlNewParserCtxt() -> *mut xmlParserCtxt;
    pub fn xmlFreeParserCtxt(ctxt: *mut xmlParserCtxt);
    pub fn xmlCtxtSetErrorHandler(
        ctxt: *mut xmlParserCtxt,
        handler: xmlStructuredErrorFunc,
        data: *mut c_void,
    );
    pub fn xmlCtxtReadMemory(
        ctxt: *mut xmlParserCtxt,
        buffer: *const c_char,
        size: c_int,
        url: *const c_char,
        encoding: *const c_char,
        options: c_int,
    ) -> *mut xmlDoc;
    pub fn xmlCtxtReadIO(
        ctxt: *mut xmlParserCtxt,
        ioread: xmlInputReadCallback,
        ioclose: xmlInputCloseCallback,
        ioctx: *mut c_void,
        url: *const c_char,
        encoding: *const c_char,
        options: c_int,
    ) -> *mut xmlDoc;

    // ---- documents ----
    pub fn xmlNewDoc(version: *const xmlChar) -> *mut xmlDoc;
    pub fn xmlFreeDoc(cur: *mut xmlDoc);
    pub fn xmlDocGetRootElement(doc: *const xmlDoc) -> *mut xmlNode;
    pub fn xmlDocSetRootElement(doc: *mut xmlDoc, root: *mut xmlNode) -> *mut xmlNode;
    pub fn xmlNewDocFragment(doc: *mut xmlDoc) -> *mut xmlNode;
    pub fn xmlCreateIntSubset(
        doc: *mut xmlDoc,
        name: *const xmlChar,
        ExternalID: *const xmlChar,
        SystemID: *const xmlChar,
    ) -> *mut xmlDtd;

    // ---- nodes ----
    pub fn xmlNewDocNode(
        doc: *mut xmlDoc,
        ns: *mut xmlNs,
        name: *const xmlChar,
        content: *const xmlChar,
    ) -> *mut xmlNode;
    pub fn xmlNewDocText(doc: *const xmlDoc, content: *const xmlChar) -> *mut xmlNode;
    pub fn xmlNewDocTextLen(doc: *mut xmlDoc, content: *const xmlChar, len: c_int) -> *mut xmlNode;
    pub fn xmlNewDocComment(doc: *mut xmlDoc, content: *const xmlChar) -> *mut xmlNode;
    pub fn xmlNewCDataBlock(doc: *mut xmlDoc, content: *const xmlChar, len: c_int) -> *mut xmlNode;
    pub fn xmlNewDocPI(doc: *mut xmlDoc, name: *const xmlChar, content: *const xmlChar)
    -> *mut xmlNode;
    pub fn xmlNewReference(doc: *const xmlDoc, name: *const xmlChar) -> *mut xmlNode;
    pub fn xmlDocCopyNode(node: *mut xmlNode, doc: *mut xmlDoc, recursive: c_int) -> *mut xmlNode;
    pub fn xmlCopyNode(node: *mut xmlNode, recursive: c_int) -> *mut xmlNode;
    pub fn xmlAddChild(parent: *mut xmlNode, cur: *mut xmlNode) -> *mut xmlNode;
    pub fn xmlAddNextSibling(cur: *mut xmlNode, elem: *mut xmlNode) -> *mut xmlNode;
    pub fn xmlAddPrevSibling(cur: *mut xmlNode, elem: *mut xmlNode) -> *mut xmlNode;
    pub fn xmlReplaceNode(old: *mut xmlNode, cur: *mut xmlNode) -> *mut xmlNode;
    pub fn xmlUnlinkNode(cur: *mut xmlNode);
    pub fn xmlFreeNode(cur: *mut xmlNode);
    pub fn xmlFreeNodeList(cur: *mut xmlNode);
    pub fn xmlSetTreeDoc(tree: *mut xmlNode, doc: *mut xmlDoc) -> c_int;
    pub fn xmlNodeGetContent(cur: *const xmlNode) -> *mut xmlChar;
    pub fn xmlNodeSetContent(cur: *mut xmlNode, content: *const xmlChar) -> c_int;
    pub fn xmlNodeSetContentLen(cur: *mut xmlNode, content: *const xmlChar, len: c_int) -> c_int;
    pub fn xmlNodeAddContent(cur: *mut xmlNode, content: *const xmlChar) -> c_int;
    pub fn xmlNodeSetName(cur: *mut xmlNode, name: *const xmlChar);
    pub fn xmlGetNodePath(node: *const xmlNode) -> *mut xmlChar;
    pub fn xmlGetLineNo(node: *const xmlNode) -> c_long;
    pub fn xmlIsBlankNode(node: *const xmlNode) -> c_int;
    pub fn xmlNodeGetLang(cur: *const xmlNode) -> *mut xmlChar;
    pub fn xmlNodeSetLang(cur: *mut xmlNode, lang: *const xmlChar) -> c_int;
    pub fn xmlFirstElementChild(parent: *mut xmlNode) -> *mut xmlNode;
    pub fn xmlLastElementChild(parent: *mut xmlNode) -> *mut xmlNode;
    pub fn xmlNextElementSibling(node: *mut xmlNode) -> *mut xmlNode;
    pub fn xmlPreviousElementSibling(node: *mut xmlNode) -> *mut xmlNode;
    pub fn xmlStringGetNodeList(doc: *const xmlDoc, value: *const xmlChar) -> *mut xmlNode;
    pub fn xmlEncodeSpecialChars(doc: *const xmlDoc, input: *const xmlChar) -> *mut xmlChar;
    pub fn xmlEncodeEntitiesReentrant(doc: *mut xmlDoc, input: *const xmlChar) -> *mut xmlChar;
    pub fn xmlTextMerge(first: *mut xmlNode, second: *mut xmlNode) -> *mut xmlNode;
    pub fn xmlDOMWrapNewCtxt() -> *mut xmlDOMWrapCtxt;
    pub fn xmlDOMWrapFreeCtxt(ctxt: *mut xmlDOMWrapCtxt);
    pub fn xmlDOMWrapAdoptNode(
        ctxt: *mut xmlDOMWrapCtxt,
        sourceDoc: *mut xmlDoc,
        node: *mut xmlNode,
        destDoc: *mut xmlDoc,
        destParent: *mut xmlNode,
        options: c_int,
    ) -> c_int;
    pub fn xmlDOMWrapRemoveNode(
        ctxt: *mut xmlDOMWrapCtxt,
        doc: *mut xmlDoc,
        node: *mut xmlNode,
        options: c_int,
    ) -> c_int;
    pub fn xmlReconciliateNs(doc: *mut xmlDoc, tree: *mut xmlNode) -> c_int;
    pub fn xmlNewNode(ns: *mut xmlNs, name: *const xmlChar) -> *mut xmlNode;
    pub fn xmlFreePropList(cur: *mut xmlAttr);
    pub fn xmlFreeDtd(cur: *mut xmlDtd);
    pub fn xmlGetIntSubset(doc: *const xmlDoc) -> *mut xmlDtd;

    // ---- DTD ----
    /// `hash` is an `xmlHashTablePtr` (the `xmlDtd` tables are typed
    /// `void *`).
    pub fn xmlHashScan(hash: *mut c_void, scan: xmlHashScanner, data: *mut c_void);
    pub fn xmlNewValidCtxt() -> *mut xmlValidCtxt;
    pub fn xmlFreeValidCtxt(ctxt: *mut xmlValidCtxt);
    pub fn xmlValidateDtd(ctxt: *mut xmlValidCtxt, doc: *mut xmlDoc, dtd: *mut xmlDtd) -> c_int;
    pub fn xmlAddDocEntity(
        doc: *mut xmlDoc,
        name: *const xmlChar,
        type_: c_int,
        ExternalID: *const xmlChar,
        SystemID: *const xmlChar,
        content: *const xmlChar,
    ) -> *mut xmlEntity;
    pub fn xmlSplitQName2(name: *const xmlChar, prefix: *mut *mut xmlChar) -> *mut xmlChar;
    pub fn xmlStrEqual(str1: *const xmlChar, str2: *const xmlChar) -> c_int;
    /// The per-thread output indentation globals (`xmlIndentTreeOutput`
    /// / `xmlTreeIndentString` are macros over these accessors).
    pub fn __xmlIndentTreeOutput() -> *mut c_int;
    pub fn __xmlTreeIndentString() -> *mut *const c_char;

    // ---- attributes ----
    pub fn xmlHasProp(node: *const xmlNode, name: *const xmlChar) -> *mut xmlAttr;
    pub fn xmlHasNsProp(node: *const xmlNode, name: *const xmlChar, nameSpace: *const xmlChar)
    -> *mut xmlAttr;
    pub fn xmlGetProp(node: *const xmlNode, name: *const xmlChar) -> *mut xmlChar;
    pub fn xmlGetNoNsProp(node: *const xmlNode, name: *const xmlChar) -> *mut xmlChar;
    pub fn xmlGetNsProp(node: *const xmlNode, name: *const xmlChar, nameSpace: *const xmlChar)
    -> *mut xmlChar;
    pub fn xmlSetProp(node: *mut xmlNode, name: *const xmlChar, value: *const xmlChar) -> *mut xmlAttr;
    pub fn xmlSetNsProp(
        node: *mut xmlNode,
        ns: *mut xmlNs,
        name: *const xmlChar,
        value: *const xmlChar,
    ) -> *mut xmlAttr;
    pub fn xmlUnsetProp(node: *mut xmlNode, name: *const xmlChar) -> c_int;
    pub fn xmlUnsetNsProp(node: *mut xmlNode, ns: *mut xmlNs, name: *const xmlChar) -> c_int;
    pub fn xmlRemoveProp(cur: *mut xmlAttr) -> c_int;
    pub fn xmlNewDocProp(doc: *mut xmlDoc, name: *const xmlChar, value: *const xmlChar) -> *mut xmlAttr;
    pub fn xmlNodeListGetString(doc: *mut xmlDoc, list: *const xmlNode, inLine: c_int) -> *mut xmlChar;

    // ---- namespaces ----
    pub fn xmlNewNs(node: *mut xmlNode, href: *const xmlChar, prefix: *const xmlChar) -> *mut xmlNs;
    pub fn xmlFreeNs(cur: *mut xmlNs);
    pub fn xmlSearchNs(doc: *mut xmlDoc, node: *mut xmlNode, nameSpace: *const xmlChar) -> *mut xmlNs;
    pub fn xmlSearchNsByHref(doc: *mut xmlDoc, node: *mut xmlNode, href: *const xmlChar) -> *mut xmlNs;
    pub fn xmlSetNs(node: *mut xmlNode, ns: *mut xmlNs);
    pub fn xmlGetNsList(doc: *const xmlDoc, node: *const xmlNode) -> *mut *mut xmlNs;

    // ---- strings ----
    pub fn xmlStrdup(cur: *const xmlChar) -> *mut xmlChar;
    pub fn xmlStrlen(str: *const xmlChar) -> c_int;

    // ---- XPath ----
    pub fn xmlXPathNewContext(doc: *mut xmlDoc) -> *mut xmlXPathContext;
    pub fn xmlXPathFreeContext(ctxt: *mut xmlXPathContext);
    pub fn xmlXPathSetContextNode(node: *mut xmlNode, ctx: *mut xmlXPathContext) -> c_int;
    pub fn xmlXPathSetErrorHandler(
        ctxt: *mut xmlXPathContext,
        handler: xmlStructuredErrorFunc,
        context: *mut c_void,
    );
    pub fn xmlXPathRegisterNs(ctxt: *mut xmlXPathContext, prefix: *const xmlChar, ns_uri: *const xmlChar)
    -> c_int;
    pub fn xmlXPathRegisterVariable(
        ctxt: *mut xmlXPathContext,
        name: *const xmlChar,
        value: *mut xmlXPathObject,
    ) -> c_int;
    pub fn xmlXPathRegisterFuncLookup(
        ctxt: *mut xmlXPathContext,
        f: xmlXPathFuncLookupFunc,
        funcCtxt: *mut c_void,
    );
    pub fn xmlXPathEval(str: *const xmlChar, ctx: *mut xmlXPathContext) -> *mut xmlXPathObject;
    pub fn xmlXPathFreeObject(obj: *mut xmlXPathObject);
    pub fn xmlXPathNewCString(val: *const c_char) -> *mut xmlXPathObject;
    pub fn xmlXPathNewString(val: *const xmlChar) -> *mut xmlXPathObject;
    pub fn xmlXPathNewFloat(val: c_double) -> *mut xmlXPathObject;
    pub fn xmlXPathNewBoolean(val: c_int) -> *mut xmlXPathObject;
    pub fn xmlXPathNewNodeSet(val: *mut xmlNode) -> *mut xmlXPathObject;
    pub fn xmlXPathWrapNodeSet(val: *mut xmlNodeSet) -> *mut xmlXPathObject;
    pub fn xmlXPathNodeSetCreate(val: *mut xmlNode) -> *mut xmlNodeSet;
    pub fn xmlXPathFreeNodeSet(obj: *mut xmlNodeSet);
    pub fn xmlXPathNodeSetAdd(cur: *mut xmlNodeSet, val: *mut xmlNode) -> c_int;
    pub fn xmlXPathNodeSetAddUnique(cur: *mut xmlNodeSet, val: *mut xmlNode) -> c_int;
    pub fn xmlXPathNodeSetContains(cur: *mut xmlNodeSet, val: *mut xmlNode) -> c_int;
    pub fn xmlXPathNodeSetDel(cur: *mut xmlNodeSet, val: *mut xmlNode);
    pub fn xmlXPathNodeSetMerge(val1: *mut xmlNodeSet, val2: *mut xmlNodeSet) -> *mut xmlNodeSet;
    pub fn xmlXPathNodeSetSort(set: *mut xmlNodeSet);
    pub fn xmlXPathIntersection(nodes1: *mut xmlNodeSet, nodes2: *mut xmlNodeSet) -> *mut xmlNodeSet;
    pub fn xmlXPathCastNodeSetToString(ns: *mut xmlNodeSet) -> *mut xmlChar;
    pub fn xmlXPathCastToString(val: *mut xmlXPathObject) -> *mut xmlChar;
    pub fn xmlXPathCastToNumber(val: *mut xmlXPathObject) -> c_double;
    pub fn xmlXPathCastToBoolean(val: *mut xmlXPathObject) -> c_int;
    pub fn valuePop(ctxt: *mut xmlXPathParserContext) -> *mut xmlXPathObject;
    pub fn valuePush(ctxt: *mut xmlXPathParserContext, value: *mut xmlXPathObject) -> c_int;
    pub fn xmlXPathRegisterFuncNS(
        ctxt: *mut xmlXPathContext,
        name: *const xmlChar,
        ns_uri: *const xmlChar,
        f: xmlXPathFunction,
    ) -> c_int;
    pub fn xmlXPathStringFunction(ctxt: *mut xmlXPathParserContext, nargs: c_int);
    pub fn xmlXPathErr(ctxt: *mut xmlXPathParserContext, error: c_int);
    pub fn xmlXPathCmpNodes(node1: *mut xmlNode, node2: *mut xmlNode) -> c_int;
    pub fn xmlXPathFreeNodeSetList(obj: *mut xmlXPathObject);

    // ---- HTML ----
    pub fn htmlNewParserCtxt() -> *mut xmlParserCtxt;
    pub fn htmlCtxtReadMemory(
        ctxt: *mut xmlParserCtxt,
        buffer: *const c_char,
        size: c_int,
        url: *const c_char,
        encoding: *const c_char,
        options: c_int,
    ) -> *mut xmlDoc;
    pub fn htmlCtxtReadIO(
        ctxt: *mut xmlParserCtxt,
        ioread: xmlInputReadCallback,
        ioclose: xmlInputCloseCallback,
        ioctx: *mut c_void,
        url: *const c_char,
        encoding: *const c_char,
        options: c_int,
    ) -> *mut xmlDoc;
    pub fn htmlNewDoc(uri: *const xmlChar, external_id: *const xmlChar) -> *mut xmlDoc;
    pub fn htmlNodeDump(buf: *mut xmlBuffer, doc: *mut xmlDoc, cur: *mut xmlNode) -> c_int;
    pub fn xmlBufferCreate() -> *mut xmlBuffer;
    pub fn xmlBufferContent(buf: *const xmlBuffer) -> *const xmlChar;
    pub fn xmlBufferFree(buf: *mut xmlBuffer);
    pub fn xmlParseInNodeContext(
        node: *mut xmlNode,
        data: *const c_char,
        datalen: c_int,
        options: c_int,
        lst: *mut *mut xmlNode,
    ) -> c_int;
    pub fn xmlSetStructuredErrorFunc(ctx: *mut c_void, handler: xmlStructuredErrorFunc);

    // ---- encodings, HTML entities ----
    pub fn xmlFindCharEncodingHandler(name: *const c_char) -> *mut xmlCharEncodingHandler;
    pub fn xmlCharEncCloseFunc(handler: *mut xmlCharEncodingHandler) -> c_int;
    pub fn xmlAddEncodingAlias(name: *const c_char, alias: *const c_char) -> c_int;
    pub fn xmlDelEncodingAlias(alias: *const c_char) -> c_int;
    pub fn xmlCleanupEncodingAliases();
    pub fn htmlEntityLookup(name: *const xmlChar) -> *const htmlEntityDesc;

    // ---- serialization ----
    pub fn xmlSaveToIO(
        iowrite: xmlOutputWriteCallback,
        ioclose: xmlOutputCloseCallback,
        ioctx: *mut c_void,
        encoding: *const c_char,
        options: c_int,
    ) -> *mut xmlSaveCtxt;
    pub fn xmlSaveDoc(ctxt: *mut xmlSaveCtxt, doc: *mut xmlDoc) -> c_long;
    pub fn xmlSaveTree(ctxt: *mut xmlSaveCtxt, node: *mut xmlNode) -> c_long;
    pub fn xmlSaveFlush(ctxt: *mut xmlSaveCtxt) -> c_int;
    pub fn xmlSaveClose(ctxt: *mut xmlSaveCtxt) -> c_int;
}

/// The library's `free` for memory it hands out (`xmlFree`), resolved
/// once through `xmlMemGet` so no assumption is made about how the global
/// is exported.
pub fn xml_free() -> unsafe extern "C" fn(*mut c_void) {
    use std::sync::OnceLock;
    static FREE: OnceLock<usize> = OnceLock::new();
    let f = *FREE.get_or_init(|| {
        let mut free: xmlFreeFunc = None;
        // SAFETY: plain out-pointers to the local option slots.
        unsafe {
            xmlMemGet(
                &mut free,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                std::ptr::null_mut(),
            );
        }
        free.expect("libxml2 exposes no free function") as usize
    });
    // SAFETY: the address was taken from a `xmlFreeFunc` above.
    unsafe { std::mem::transmute::<usize, unsafe extern "C" fn(*mut c_void)>(f) }
}
