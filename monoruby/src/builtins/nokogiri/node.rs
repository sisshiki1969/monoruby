//! `Nokogiri::XML::Node` and the node subclasses with native
//! constructors (`Text`, `Comment`, `CDATA`, `ProcessingInstruction`,
//! `Attr`, `DocumentFragment`), and `Namespace`.

use super::node_set::wrap_node_set;
use super::*;

pub(super) fn init(globals: &mut Globals, c: &Classes) {
    let n = c.node;
    globals.define_builtin_class_func_rest(n, "new", node_new);
    globals.define_builtin_func(n, "add_namespace_definition", add_namespace_definition, 2);
    globals.define_builtin_func(n, "attribute", attribute, 1);
    globals.define_builtin_func(n, "attribute_nodes", attribute_nodes, 0);
    globals.define_builtin_func(n, "attribute_with_ns", attribute_with_ns, 2);
    globals.define_builtin_func(n, "blank?", blank, 0);
    globals.define_builtin_func(n, "child", child, 0);
    globals.define_builtin_func(n, "children", children, 0);
    globals.define_builtin_func(n, "content", content, 0);
    globals.define_builtin_func(n, "create_internal_subset", create_internal_subset, 3);
    globals.define_builtin_func(n, "data_ptr?", data_ptr, 0);
    globals.define_builtin_func(n, "document", document, 0);
    globals.define_builtin_func(n, "element_children", element_children, 0);
    globals.define_builtin_func(n, "encode_special_chars", encode_special_chars, 1);
    globals.define_builtin_func(n, "external_subset", external_subset, 0);
    globals.define_builtin_func(n, "first_element_child", first_element_child, 0);
    globals.define_builtin_func(n, "internal_subset", internal_subset, 0);
    globals.define_builtin_func(n, "key?", key, 1);
    globals.define_builtin_func(n, "lang", lang, 0);
    globals.define_builtin_func(n, "lang=", set_lang, 1);
    globals.define_builtin_func(n, "last_element_child", last_element_child, 0);
    globals.define_builtin_func(n, "line", line, 0);
    globals.define_builtin_func(n, "line=", set_line, 1);
    globals.define_builtin_func(n, "namespace", namespace, 0);
    globals.define_builtin_func(n, "namespace_definitions", namespace_definitions, 0);
    globals.define_builtin_func(n, "namespace_scopes", namespace_scopes, 0);
    globals.define_builtin_func(n, "namespaced_key?", namespaced_key, 2);
    globals.define_builtin_func(n, "native_content=", set_native_content, 1);
    globals.define_builtin_func(n, "next_element", next_element, 0);
    globals.define_builtin_func(n, "next_sibling", next_sibling, 0);
    globals.define_builtin_func(n, "node_name", node_name, 0);
    globals.define_builtin_func(n, "node_name=", set_node_name, 1);
    globals.define_builtin_func(n, "node_type", node_type, 0);
    globals.define_builtin_func(n, "parent", parent, 0);
    globals.define_builtin_func(n, "path", path, 0);
    globals.define_builtin_func(n, "pointer_id", pointer_id, 0);
    globals.define_builtin_func(n, "previous_element", previous_element, 0);
    globals.define_builtin_func(n, "previous_sibling", previous_sibling, 0);
    globals.define_builtin_func(n, "unlink", unlink, 0);
    globals.define_private_builtin_func(n, "add_child_node", add_child_node, 1);
    globals.define_private_builtin_func(n, "add_next_sibling_node", add_next_sibling_node, 1);
    globals.define_private_builtin_func(n, "add_previous_sibling_node", add_previous_sibling_node, 1);
    globals.define_private_builtin_func(n, "replace_node", replace_node, 1);
    globals.define_private_builtin_func(n, "compare", compare, 1);
    globals.define_private_builtin_func(n, "get", get, 1);
    globals.define_private_builtin_func(n, "set", set, 2);
    globals.define_private_builtin_func(n, "set_namespace", set_namespace, 1);
    globals.define_private_builtin_func(n, "native_write_to", native_write_to, 4);
    globals.define_private_builtin_func(n, "in_context", in_context, 2);
    globals.define_private_builtin_func(n, "dump_html", dump_html, 0);

    globals.define_builtin_class_func_rest(c.text, "new", text_new);
    globals.define_builtin_class_func_rest(c.comment, "new", comment_new);
    globals.define_builtin_class_func_rest(c.cdata, "new", cdata_new);
    globals.define_builtin_class_func_rest(c.pi, "new", pi_new);
    globals.define_builtin_class_func_rest(c.attr, "new", attr_new);
    globals.define_builtin_func(c.attr, "value=", attr_set_value, 1);
    globals.define_builtin_class_func(c.document_fragment, "native_new", fragment_native_new, 1);
    globals.define_builtin_func(c.namespace, "prefix", ns_prefix, 0);
    globals.define_builtin_func(c.namespace, "href", ns_href, 0);
}

/// The node of `self`.
fn this(lfp: Lfp) -> Result<*mut xml::xmlNode> {
    node_ptr(lfp.self_val())
}

fn class_name(globals: &Globals, v: Value) -> String {
    globals.store.get_class_name(v.class())
}

/// The positional arguments of a `_rest` builtin.
fn rest_args(lfp: Lfp, min: usize) -> Result<Vec<Value>> {
    let args: Vec<Value> = lfp.arg(0).as_array().iter().cloned().collect();
    if args.len() < min {
        return Err(MonorubyErr::argumenterr(format!(
            "wrong number of arguments (given {}, expected {min}+)",
            args.len()
        )));
    }
    Ok(args)
}

/// The tail of a native constructor: wrap the new node into the receiver
/// class, `initialize(*args)`, yield it to the block.
fn construct(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    class: Option<ClassId>,
    node: *mut xml::xmlNode,
    args: &[Value],
) -> Result<Value> {
    // SAFETY: a fresh node of a live document.
    unsafe { pin_node(node) };
    let rb = wrap_node_as(vm, globals, class, node)?;
    vm.invoke_method_inner(globals, IdentId::INITIALIZE, rb, args, None, None)?;
    if let Some(bh) = lfp.block() {
        vm.invoke_block_once(globals, bh, &[rb])?;
    }
    Ok(rb)
}

/// Node.new(name, document, ...) { |node| } -> Node
#[monoruby_builtin]
fn node_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let args = rest_args(lfp, 2)?;
    let name = cstr(args[0], &globals.store)?;
    if !is_node(args[1]) {
        return Err(MonorubyErr::argumenterr("document must be a Nokogiri::XML::Node"));
    }
    // SAFETY: a live node; the new node is ours until the document takes it.
    let node = unsafe {
        let doc = (*node_ptr(args[1])?).doc;
        let node = xml::xmlNewNode(std::ptr::null_mut(), name.as_ptr() as *const xml::xmlChar);
        if node.is_null() {
            return Err(MonorubyErr::runtimeerr("could not create node"));
        }
        (*node).doc = doc;
        node
    };
    let class = if class == classes().node { None } else { Some(class) };
    construct(vm, globals, lfp, class, node, &args)
}

/// Text.new(string, document, ...) -> Text
#[monoruby_builtin]
fn text_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let args = rest_args(lfp, 2)?;
    if args[0].try_bytes().is_none() {
        return Err(MonorubyErr::typeerr(format!(
            "wrong argument type {} (expected String)",
            class_name(globals, args[0])
        )));
    }
    let content = cstr(args[0], &globals.store)?;
    if !is_node(args[1]) {
        return Err(MonorubyErr::typeerr(format!(
            "expected second parameter to be a Nokogiri::XML::Document, received {}",
            class_name(globals, args[1])
        )));
    }
    // SAFETY: a live document.
    let node = unsafe {
        let doc = (*node_ptr(args[1])?).doc;
        xml::xmlNewDocText(doc, content.as_ptr() as *const xml::xmlChar)
    };
    construct(vm, globals, lfp, Some(class), node, &args)
}

/// Comment.new(document, content, ...) -> Comment
#[monoruby_builtin]
fn comment_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let args = rest_args(lfp, 2)?;
    if args[1].try_bytes().is_none() {
        return Err(MonorubyErr::typeerr(format!(
            "wrong argument type {} (expected String)",
            class_name(globals, args[1])
        )));
    }
    let content = cstr(args[1], &globals.store)?;
    let mut document = args[0];
    if document.try_native::<XmlDocument>().is_none() {
        if !is_node(document) {
            return Err(MonorubyErr::argumenterr("first argument must be a XML::Document or XML::Node"));
        }
        document = vm.invoke_method_inner(globals, IdentId::get_id("document"), document, &[], None, None)?;
    }
    let doc = doc_ptr(document)?;
    // SAFETY: a live document.
    let node = unsafe { xml::xmlNewDocComment(doc, content.as_ptr() as *const xml::xmlChar) };
    construct(vm, globals, lfp, Some(class), node, &args)
}

/// CDATA.new(document, content, ...) -> CDATA
#[monoruby_builtin]
fn cdata_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let args = rest_args(lfp, 2)?;
    if !is_node(args[0]) {
        return Err(MonorubyErr::typeerr(format!(
            "expected first parameter to be a Nokogiri::XML::Document, received {}",
            class_name(globals, args[0])
        )));
    }
    let content = args[1].expect_bytes(&globals.store)?.to_vec();
    // SAFETY: a live document; the content buffer outlives the call.
    let node = unsafe {
        let doc = (*node_ptr(args[0])?).doc;
        xml::xmlNewCDataBlock(doc, content.as_ptr(), content.len() as c_int)
    };
    construct(vm, globals, lfp, Some(class), node, &args)
}

/// ProcessingInstruction.new(document, name, content, ...) -> ProcessingInstruction
#[monoruby_builtin]
fn pi_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let args = rest_args(lfp, 3)?;
    let doc = doc_ptr(args[0])?;
    let name = cstr(args[1], &globals.store)?;
    let content = cstr(args[2], &globals.store)?;
    // SAFETY: a live document.
    let node = unsafe {
        xml::xmlNewDocPI(
            doc,
            name.as_ptr() as *const xml::xmlChar,
            content.as_ptr() as *const xml::xmlChar,
        )
    };
    construct(vm, globals, lfp, Some(class), node, &args)
}

/// Attr.new(document, name, ...) -> Attr
#[monoruby_builtin]
fn attr_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let args = rest_args(lfp, 2)?;
    let doc = doc_ptr(args[0]).map_err(|_| MonorubyErr::argumenterr("parameter must be a Nokogiri::XML::Document"))?;
    let name = cstr(args[1], &globals.store)?;
    // SAFETY: a live document.
    let node = unsafe { xml::xmlNewDocProp(doc, name.as_ptr() as *const xml::xmlChar, std::ptr::null()) };
    construct(vm, globals, lfp, Some(class), node as *mut xml::xmlNode, &args)
}

/// Attr#value=(content)
#[monoruby_builtin]
fn attr_set_value(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let attr = this(lfp)? as *mut xml::xmlAttr;
    let content = lfp.arg(0);
    // SAFETY: a live attribute of a live document.
    unsafe {
        if !(*attr).children.is_null() {
            xml::xmlFreeNodeList((*attr).children);
        }
        (*attr).children = std::ptr::null_mut();
        (*attr).last = std::ptr::null_mut();
        if content.is_nil() {
            return Ok(content);
        }
        let c = cstr(content, &globals.store)?;
        let value = xml::xmlEncodeEntitiesReentrant((*attr).doc, c.as_ptr() as *const xml::xmlChar);
        if xml::xmlStrlen(value) == 0 {
            (*attr).children = xml::xmlNewDocText((*attr).doc, value);
        } else {
            (*attr).children = xml::xmlStringGetNodeList((*attr).doc, value);
        }
        xml::xml_free()(value as *mut c_void);
        let mut cur = (*attr).children;
        while !cur.is_null() {
            (*cur).parent = attr as *mut xml::xmlNode;
            (*cur).doc = (*attr).doc;
            if (*cur).next.is_null() {
                (*attr).last = cur;
            }
            cur = (*cur).next;
        }
    }
    Ok(content)
}

/// DocumentFragment.native_new(document) -> DocumentFragment
#[monoruby_builtin]
fn fragment_native_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let doc = doc_ptr(lfp.arg(0))?;
    // SAFETY: a live document.
    let node = unsafe { xml::xmlNewDocFragment(doc) };
    // SAFETY: a fresh node.
    unsafe { pin_node(node) };
    wrap_node_as(vm, globals, Some(class), node)
}

/// Namespace#prefix -> String | nil
#[monoruby_builtin]
fn ns_prefix(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ns = namespace_ptr(lfp.self_val())?;
    // SAFETY: a live namespace.
    Ok(unsafe { xml_str((*ns).prefix) })
}

/// Namespace#href -> String | nil
#[monoruby_builtin]
fn ns_href(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ns = namespace_ptr(lfp.self_val())?;
    // SAFETY: a live namespace.
    Ok(unsafe { xml_str((*ns).href) })
}

// ---- traversal ----

/// Node#document -> Document
#[monoruby_builtin]
fn document(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node.
    unsafe { doc_value((*node).doc) }.ok_or_else(|| MonorubyErr::runtimeerr("node has no document"))
}

/// Node#parent -> Node | nil
#[monoruby_builtin]
fn parent(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node.
    wrap_node_or_nil(vm, globals, unsafe { (*node).parent })
}

/// Node#child -> Node | nil
#[monoruby_builtin]
fn child(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node.
    wrap_node_or_nil(vm, globals, unsafe { (*node).children })
}

/// Node#next_sibling -> Node | nil
#[monoruby_builtin]
fn next_sibling(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node.
    wrap_node_or_nil(vm, globals, unsafe { (*node).next })
}

/// Node#previous_sibling -> Node | nil
#[monoruby_builtin]
fn previous_sibling(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node.
    wrap_node_or_nil(vm, globals, unsafe { (*node).prev })
}

/// Node#next_element -> Element | nil
#[monoruby_builtin]
fn next_element(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node.
    wrap_node_or_nil(vm, globals, unsafe { xml::xmlNextElementSibling(node) })
}

/// Node#previous_element -> Element | nil
#[monoruby_builtin]
fn previous_element(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node.
    wrap_node_or_nil(vm, globals, unsafe { xml::xmlPreviousElementSibling(node) })
}

/// Node#first_element_child -> Element | nil
#[monoruby_builtin]
fn first_element_child(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node.
    wrap_node_or_nil(vm, globals, unsafe { xml::xmlFirstElementChild(node) })
}

/// Node#last_element_child -> Element | nil
#[monoruby_builtin]
fn last_element_child(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node.
    wrap_node_or_nil(vm, globals, unsafe { xml::xmlLastElementChild(node) })
}

/// Node#children -> NodeSet
#[monoruby_builtin]
fn children(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node; the new set is ours.
    let (set, doc) = unsafe {
        let mut child = (*node).children;
        let set = xml::xmlXPathNodeSetCreate(child);
        if !child.is_null() {
            child = (*child).next;
            while !child.is_null() {
                xml::xmlXPathNodeSetAddUnique(set, child);
                child = (*child).next;
            }
        }
        (set, doc_value((*node).doc).unwrap_or_default())
    };
    wrap_node_set(vm, globals, set, doc)
}

/// Node#element_children -> NodeSet
#[monoruby_builtin]
fn element_children(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node; the new set is ours.
    let (set, doc) = unsafe {
        let mut child = xml::xmlFirstElementChild(node);
        let set = xml::xmlXPathNodeSetCreate(child);
        if !child.is_null() {
            child = xml::xmlNextElementSibling(child);
            while !child.is_null() {
                xml::xmlXPathNodeSetAddUnique(set, child);
                child = xml::xmlNextElementSibling(child);
            }
        }
        (set, doc_value((*node).doc).unwrap_or_default())
    };
    wrap_node_set(vm, globals, set, doc)
}

// ---- properties ----

/// Node#node_name -> String | nil
#[monoruby_builtin]
fn node_name(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node.
    Ok(unsafe { xml_str((*node).name) })
}

/// Node#node_name=(name)
#[monoruby_builtin]
fn set_node_name(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    let name = cstr(lfp.arg(0), &globals.store)?;
    // SAFETY: a live node.
    unsafe { xml::xmlNodeSetName(node, name.as_ptr() as *const xml::xmlChar) };
    Ok(lfp.arg(0))
}

/// Node#node_type -> Integer
#[monoruby_builtin]
fn node_type(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node.
    Ok(Value::integer(unsafe { (*node).type_ } as i64))
}

/// Node#content -> String | nil
#[monoruby_builtin]
fn content(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node; the content is ours to free.
    Ok(unsafe { xml_str_owned(xml::xmlNodeGetContent(node)) })
}

/// Node#native_content=(string)
#[monoruby_builtin]
fn set_native_content(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    let content = cstr(lfp.arg(0), &globals.store)?;
    // SAFETY: a live node; the children it drops stay pinned.
    unsafe {
        let mut child = (*node).children;
        while !child.is_null() {
            let next = (*child).next;
            xml::xmlUnlinkNode(child);
            pin_node(child);
            child = next;
        }
        xml::xmlNodeSetContent(node, content.as_ptr() as *const xml::xmlChar);
    }
    Ok(lfp.arg(0))
}

/// Node#blank? -> bool
#[monoruby_builtin]
fn blank(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node.
    Ok(Value::bool(unsafe { xml::xmlIsBlankNode(node) } == 1))
}

/// Node#path -> String
#[monoruby_builtin]
fn path(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node; the path is ours to free.
    unsafe {
        let p = xml::xmlGetNodePath(node);
        if p.is_null() {
            // What libxml <= 2.9.10 answered (nokogiri #2250).
            Ok(Value::string_from_str("?"))
        } else {
            Ok(xml_str_owned(p))
        }
    }
}

/// Node#line -> Integer
#[monoruby_builtin]
fn line(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node.
    Ok(Value::integer(unsafe { xml::xmlGetLineNo(node) } as i64))
}

/// Node#line=(n)
#[monoruby_builtin]
fn set_line(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    let n = lfp.arg(0).expect_integer(&globals.store)?;
    // SAFETY: a live node; long line numbers of text nodes ride in `psvi`
    // (SAX2.c / tree.c).
    unsafe {
        if n < 65535 {
            (*node).line = n as u16;
        } else {
            (*node).line = 65535;
            if (*node).type_ == xml::XML_TEXT_NODE {
                (*node).psvi = n as isize as *mut c_void;
            }
        }
    }
    Ok(lfp.arg(0))
}

/// Node#pointer_id -> Integer
#[monoruby_builtin]
fn pointer_id(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    Ok(Value::integer(node as usize as i64))
}

/// Node#data_ptr? -> true
#[monoruby_builtin]
fn data_ptr(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    Ok(Value::bool(!node.is_null()))
}

/// Node#lang -> String | nil
#[monoruby_builtin]
fn lang(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node; the string is ours to free.
    Ok(unsafe { xml_str_owned(xml::xmlNodeGetLang(node)) })
}

/// Node#lang=(lang) -> nil
#[monoruby_builtin]
fn set_lang(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    let lang = cstr(lfp.arg(0), &globals.store)?;
    // SAFETY: a live node.
    unsafe { xml::xmlNodeSetLang(node, lang.as_ptr() as *const xml::xmlChar) };
    Ok(Value::nil())
}

/// Node#encode_special_chars(string) -> String
#[monoruby_builtin]
fn encode_special_chars(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    let s = cstr(lfp.arg(0), &globals.store)?;
    // SAFETY: a live node; the result is ours to free.
    Ok(unsafe { xml_str_owned(xml::xmlEncodeSpecialChars((*node).doc, s.as_ptr() as *const xml::xmlChar)) })
}

/// Node#compare(other) -> Integer (document order, `xmlXPathCmpNodes`)
#[monoruby_builtin]
fn compare(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    let other = node_ptr(lfp.arg(0))?;
    // SAFETY: live nodes.
    Ok(Value::integer(unsafe { xml::xmlXPathCmpNodes(other, node) } as i64))
}

// ---- attributes ----

/// Node#attribute(name) -> Attr | nil
#[monoruby_builtin]
fn attribute(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    let name = cstr(lfp.arg(0), &globals.store)?;
    // SAFETY: a live node.
    let prop = unsafe { xml::xmlHasProp(node, name.as_ptr() as *const xml::xmlChar) };
    wrap_node_or_nil(vm, globals, prop as *mut xml::xmlNode)
}

/// Node#attribute_with_ns(name, namespace) -> Attr | nil
#[monoruby_builtin]
fn attribute_with_ns(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    let name = cstr(lfp.arg(0), &globals.store)?;
    let ns = opt_cstr(lfp.arg(1), &globals.store)?;
    // SAFETY: a live node.
    let prop = unsafe { xml::xmlHasNsProp(node, name.as_ptr() as *const xml::xmlChar, cptr(&ns)) };
    wrap_node_or_nil(vm, globals, prop as *mut xml::xmlNode)
}

/// Node#attribute_nodes -> Array of Attr
#[monoruby_builtin]
fn attribute_nodes(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    let mut attrs = vec![];
    // SAFETY: a live node.
    let mut prop = unsafe { (*node).properties };
    while !prop.is_null() {
        attrs.push(wrap_node(vm, globals, prop as *mut xml::xmlNode)?);
        // SAFETY: a live attribute.
        prop = unsafe { (*prop).next };
    }
    Ok(Value::array_from_vec(attrs))
}

/// Node#key?(name) -> bool
#[monoruby_builtin]
fn key(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    let name = cstr(lfp.arg(0), &globals.store)?;
    // SAFETY: a live node.
    Ok(Value::bool(!unsafe { xml::xmlHasProp(node, name.as_ptr() as *const xml::xmlChar) }.is_null()))
}

/// Node#namespaced_key?(name, namespace) -> bool
#[monoruby_builtin]
fn namespaced_key(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    let name = cstr(lfp.arg(0), &globals.store)?;
    let ns = opt_cstr(lfp.arg(1), &globals.store)?;
    // SAFETY: a live node.
    let prop = unsafe { xml::xmlHasNsProp(node, name.as_ptr() as *const xml::xmlChar, cptr(&ns)) };
    Ok(Value::bool(!prop.is_null()))
}

/// Node#get(name) -> String | nil: the attribute's value, `prefix:name`
/// resolved through the namespaces in scope.
#[monoruby_builtin]
fn get(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let arg = lfp.arg(0);
    if arg.is_nil() {
        return Ok(Value::nil());
    }
    let node = this(lfp)?;
    let full = cstr(arg, &globals.store)?;
    let bytes = full.as_bytes();
    // SAFETY: a live node; the results are ours to free.
    let value = unsafe {
        match bytes.iter().position(|&b| b == b':') {
            Some(colon) => {
                let prefix = CString::new(&bytes[..colon]).unwrap();
                let name = CString::new(&bytes[colon + 1..]).unwrap();
                let ns = xml::xmlSearchNs((*node).doc, node, prefix.as_ptr() as *const xml::xmlChar);
                if !ns.is_null() {
                    xml::xmlGetNsProp(node, name.as_ptr() as *const xml::xmlChar, (*ns).href)
                } else {
                    xml::xmlGetProp(node, full.as_ptr() as *const xml::xmlChar)
                }
            }
            None => xml::xmlGetNoNsProp(node, full.as_ptr() as *const xml::xmlChar),
        }
    };
    // SAFETY: a libxml2 string, ours to free.
    Ok(unsafe { xml_str_owned(value) })
}

/// Node#set(name, value) -> value
#[monoruby_builtin]
fn set(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    let value = lfp.arg(1);
    // SAFETY: a live node.
    unsafe {
        if (*node).type_ != xml::XML_ELEMENT_NODE {
            return Ok(Value::nil());
        }
        let name = cstr(lfp.arg(0), &globals.store)?;
        let val = cstr(value, &globals.store)?;
        let prop = xml::xmlHasProp(node, name.as_ptr() as *const xml::xmlChar);
        if !prop.is_null() && !(*prop).children.is_null() {
            // The old value's text nodes may have Ruby objects: keep them.
            let mut cur = (*prop).children;
            while !cur.is_null() {
                let next = (*cur).next;
                if !(*cur)._private.is_null() {
                    pin_node(cur);
                    xml::xmlUnlinkNode(cur);
                }
                cur = next;
            }
        }
        xml::xmlSetProp(node, name.as_ptr() as *const xml::xmlChar, val.as_ptr() as *const xml::xmlChar);
    }
    Ok(value)
}

// ---- namespaces ----

/// Node#namespace -> Namespace | nil
#[monoruby_builtin]
fn namespace(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node.
    unsafe {
        if (*node).ns.is_null() {
            return Ok(Value::nil());
        }
        wrap_namespace(globals, (*node).ns, (*node).doc)
    }
}

/// Node#namespace_definitions -> Array of Namespace
#[monoruby_builtin]
fn namespace_definitions(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    let mut defs = vec![];
    // SAFETY: a live node.
    unsafe {
        let mut ns = (*node).nsDef;
        while !ns.is_null() {
            defs.push(wrap_namespace(globals, ns, (*node).doc)?);
            ns = (*ns).next;
        }
    }
    Ok(Value::array_from_vec(defs))
}

/// Node#namespace_scopes -> Array of Namespace
#[monoruby_builtin]
fn namespace_scopes(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    let mut scopes = vec![];
    // SAFETY: a live node; the list is ours to free.
    unsafe {
        let list = xml::xmlGetNsList((*node).doc, node);
        if !list.is_null() {
            let mut j = 0;
            while !(*list.add(j)).is_null() {
                scopes.push(wrap_namespace(globals, *list.add(j), (*node).doc)?);
                j += 1;
            }
            xml::xml_free()(list as *mut c_void);
        }
    }
    Ok(Value::array_from_vec(scopes))
}

/// Node#add_namespace_definition(prefix, href) -> Namespace | nil
#[monoruby_builtin]
fn add_namespace_definition(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    let prefix = opt_cstr(lfp.arg(0), &globals.store)?;
    let href = cstr(lfp.arg(1), &globals.store)?;
    // SAFETY: a live node.
    unsafe {
        let mut element = node;
        let mut ns = xml::xmlSearchNs((*node).doc, node, cptr(&prefix));
        if ns.is_null() {
            if (*node).type_ != xml::XML_ELEMENT_NODE {
                element = (*node).parent;
            }
            ns = xml::xmlNewNs(element, href.as_ptr() as *const xml::xmlChar, cptr(&prefix));
        }
        if ns.is_null() {
            return Ok(Value::nil());
        }
        if prefix.is_none() || node != element {
            xml::xmlSetNs(node, ns);
        }
        wrap_namespace(globals, ns, (*node).doc)
    }
}

/// Node#set_namespace(namespace) -> self
#[monoruby_builtin]
fn set_namespace(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    let arg = lfp.arg(0);
    let ns = if arg.is_nil() { std::ptr::null_mut() } else { namespace_ptr(arg)? };
    // SAFETY: a live node.
    unsafe { xml::xmlSetNs(node, ns) };
    Ok(lfp.self_val())
}

// ---- DTD ----

/// Node#internal_subset -> DTD | nil
#[monoruby_builtin]
fn internal_subset(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node.
    let dtd = unsafe {
        if (*node).doc.is_null() {
            return Ok(Value::nil());
        }
        xml::xmlGetIntSubset((*node).doc)
    };
    wrap_node_or_nil(vm, globals, dtd as *mut xml::xmlNode)
}

/// Node#external_subset -> DTD | nil
#[monoruby_builtin]
fn external_subset(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node.
    let dtd = unsafe {
        if (*node).doc.is_null() {
            return Ok(Value::nil());
        }
        (*(*node).doc).extSubset
    };
    wrap_node_or_nil(vm, globals, dtd as *mut xml::xmlNode)
}

/// Node#create_internal_subset(name, external_id, system_id) -> DTD | nil
#[monoruby_builtin]
fn create_internal_subset(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    let name = opt_cstr(lfp.arg(0), &globals.store)?;
    let external_id = opt_cstr(lfp.arg(1), &globals.store)?;
    let system_id = opt_cstr(lfp.arg(2), &globals.store)?;
    // SAFETY: a live node of a live document.
    let dtd = unsafe {
        let doc = (*node).doc;
        if !xml::xmlGetIntSubset(doc).is_null() {
            return Err(MonorubyErr::runtimeerr("Document already has an internal subset"));
        }
        xml::xmlCreateIntSubset(doc, cptr(&name), cptr(&external_id), cptr(&system_id))
    };
    wrap_node_or_nil(vm, globals, dtd as *mut xml::xmlNode)
}

// ---- tree editing ----

/// Node#unlink -> self
#[monoruby_builtin]
fn unlink(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node; unlinked nodes stay owned by the document.
    unsafe {
        xml::xmlUnlinkNode(node);
        pin_node(node);
    }
    Ok(lfp.self_val())
}

#[derive(Clone, Copy, PartialEq)]
enum Reparent {
    Child,
    NextSibling,
    PrevSibling,
    Replace,
}

/// `xmlReplaceNode`, merging the replacement with adjacent text
/// (`xmlReplaceNodeWrapper`).
unsafe fn replace_node_wrapper(pivot: *mut xml::xmlNode, new_node: *mut xml::xmlNode) -> *mut xml::xmlNode {
    // SAFETY: live nodes of one document.
    unsafe {
        let mut retval = xml::xmlReplaceNode(pivot, new_node);
        if retval == pivot {
            retval = new_node;
        }
        if !retval.is_null() && (*retval).type_ == xml::XML_TEXT_NODE {
            if !(*retval).prev.is_null() && (*(*retval).prev).type_ == xml::XML_TEXT_NODE {
                retval = xml::xmlTextMerge((*retval).prev, retval);
            }
            if !(*retval).next.is_null() && (*(*retval).next).type_ == xml::XML_TEXT_NODE {
                retval = xml::xmlTextMerge(retval, (*retval).next);
            }
        }
        retval
    }
}

unsafe fn raise_if_ancestor_of_self(node: *mut xml::xmlNode) -> Result<()> {
    // SAFETY: a live node.
    unsafe {
        let mut ancestor = (*node).parent;
        while !ancestor.is_null() {
            if ancestor == node {
                let name = CStr::from_ptr((*node).name as *const c_char).to_string_lossy();
                return Err(MonorubyErr::runtimeerr(format!(
                    "cycle detected: node '{name}' is an ancestor of itself"
                )));
            }
            ancestor = (*ancestor).parent;
        }
    }
    Ok(())
}

/// After a node was moved into a tree: bind `prefix:name` names to the
/// namespaces now in scope, drop namespace definitions the new parent
/// already provides, and do the same for the subtree (`relink_namespace`).
unsafe fn relink_namespace(globals: &Globals, reparented: *mut xml::xmlNode) {
    // SAFETY: a live node of a live document.
    unsafe {
        let ty = (*reparented).type_;
        if ty != xml::XML_ATTRIBUTE_NODE && ty != xml::XML_ELEMENT_NODE {
            return;
        }
        let free = xml::xml_free();
        if (*reparented).ns.is_null() || (*(*reparented).ns).prefix.is_null() {
            let mut prefix: *mut xml::xmlChar = std::ptr::null_mut();
            let name = xml::xmlSplitQName2((*reparented).name, &mut prefix);
            let is_xmlns = !prefix.is_null()
                && CStr::from_ptr(prefix as *const c_char).to_bytes() == b"xmlns";
            if ty == xml::XML_ATTRIBUTE_NODE && (prefix.is_null() || is_xmlns) {
                if !name.is_null() {
                    free(name as *mut c_void);
                }
                if !prefix.is_null() {
                    free(prefix as *mut c_void);
                }
                return;
            }
            let ns = xml::xmlSearchNs((*reparented).doc, reparented, prefix);
            if !ns.is_null() {
                xml::xmlNodeSetName(reparented, name);
                xml::xmlSetNs(reparented, ns);
            }
            if !name.is_null() {
                free(name as *mut c_void);
            }
            if !prefix.is_null() {
                free(prefix as *mut c_void);
            }
        }
        if ty != xml::XML_ELEMENT_NODE || (*reparented).parent.is_null() {
            return;
        }
        let doc = (*reparented).doc;
        if (*reparented).ns.is_null() && doc as *mut xml::xmlNode != (*reparented).parent {
            let inherit = doc_value(doc)
                .and_then(|d| globals.store.get_ivar(d, IdentId::get_id("@namespace_inheritance")))
                .is_some_and(|v| v.id() == TRUE_VALUE);
            if inherit {
                xml::xmlSetNs(reparented, (*(*reparented).parent).ns);
            }
        }
        if !(*reparented).nsDef.is_null() {
            let mut curr = (*reparented).nsDef;
            let mut prev: *mut xml::xmlNs = std::ptr::null_mut();
            while !curr.is_null() {
                let ns = xml::xmlSearchNsByHref(doc, (*reparented).parent, (*curr).href);
                if !ns.is_null() && ns != curr && xml::xmlStrEqual((*ns).prefix, (*curr).prefix) != 0 {
                    if !prev.is_null() {
                        (*prev).next = (*curr).next;
                    } else {
                        (*reparented).nsDef = (*curr).next;
                    }
                    pin_namespace(curr, doc);
                } else {
                    prev = curr;
                }
                curr = (*curr).next;
            }
        }
        if !(*reparented).ns.is_null() {
            let ns = xml::xmlSearchNs(doc, reparented, (*(*reparented).ns).prefix);
            if !ns.is_null()
                && ns != (*reparented).ns
                && xml::xmlStrEqual((*ns).prefix, (*(*reparented).ns).prefix) != 0
                && xml::xmlStrEqual((*ns).href, (*(*reparented).ns).href) != 0
            {
                xml::xmlSetNs(reparented, ns);
            }
        }
        if (*reparented).ns.is_null() {
            return;
        }
        let mut child = (*reparented).children;
        while !child.is_null() {
            relink_namespace(globals, child);
            child = (*child).next;
        }
        if ty == xml::XML_ELEMENT_NODE {
            let mut attr = (*reparented).properties;
            while !attr.is_null() {
                relink_namespace(globals, attr as *mut xml::xmlNode);
                attr = (*attr).next;
            }
        }
    }
}

/// Move `reparentee` next to / under / in place of `pivot`
/// (`reparent_node_with`): a node from another document or a text node
/// is copied first (the original stays owned by its document), and the
/// Ruby object of the reparentee is re-pointed at the node in the tree.
fn reparent(
    vm: &mut Executor,
    globals: &mut Globals,
    pivot_obj: Value,
    reparentee_obj: Value,
    how: Reparent,
) -> Result<Value> {
    if !is_node(reparentee_obj) || reparentee_obj.try_native::<XmlDocument>().is_some() {
        return Err(MonorubyErr::argumenterr("node must be a Nokogiri::XML::Node"));
    }
    let original = node_ptr(reparentee_obj)?;
    let pivot = node_ptr(pivot_obj)?;
    // SAFETY: live nodes; every node dropped from a tree stays pinned.
    let reparented = unsafe {
        let parent = if how == Reparent::Child { pivot } else { (*pivot).parent };
        if !parent.is_null() {
            let rt = (*original).type_;
            let ok = match (*parent).type_ {
                xml::XML_DOCUMENT_NODE | xml::XML_HTML_DOCUMENT_NODE => matches!(
                    rt,
                    xml::XML_ELEMENT_NODE
                        | xml::XML_PI_NODE
                        | xml::XML_COMMENT_NODE
                        | xml::XML_DOCUMENT_TYPE_NODE
                        | xml::XML_TEXT_NODE
                        | xml::XML_CDATA_SECTION_NODE
                        | xml::XML_ENTITY_REF_NODE
                ),
                xml::XML_DOCUMENT_FRAG_NODE | xml::XML_ENTITY_REF_NODE | xml::XML_ELEMENT_NODE => matches!(
                    rt,
                    xml::XML_ELEMENT_NODE
                        | xml::XML_PI_NODE
                        | xml::XML_COMMENT_NODE
                        | xml::XML_TEXT_NODE
                        | xml::XML_CDATA_SECTION_NODE
                        | xml::XML_ENTITY_REF_NODE
                ),
                xml::XML_ATTRIBUTE_NODE => matches!(rt, xml::XML_TEXT_NODE | xml::XML_ENTITY_REF_NODE),
                _ => false,
            };
            if !ok {
                return Err(MonorubyErr::argumenterr(format!(
                    "cannot reparent {} there",
                    class_name(globals, reparentee_obj)
                )));
            }
        }
        let mut reparentee = original;
        if (*reparentee).doc != (*pivot).doc || (*reparentee).type_ == xml::XML_TEXT_NODE {
            if (*reparentee).type_ == xml::XML_TEXT_NODE && !(*reparentee)._private.is_null() {
                (*reparentee)._private = std::ptr::null_mut();
            }
            let default_prefix = !(*reparentee).ns.is_null() && (*(*reparentee).ns).prefix.is_null();
            pin_node(reparentee);
            reparentee = xml::xmlDocCopyNode(reparentee, (*pivot).doc, 1);
            if reparentee.is_null() {
                return Err(MonorubyErr::runtimeerr("Could not reparent node (xmlDocCopyNode)"));
            }
            if default_prefix && !(*reparentee).ns.is_null() && !(*(*reparentee).ns).prefix.is_null() {
                xml::xml_free()((*(*reparentee).ns).prefix as *mut c_void);
                (*(*reparentee).ns).prefix = std::ptr::null();
            }
        }
        xml::xmlUnlinkNode(original);
        if how == Reparent::Replace
            && (*reparentee).type_ == xml::XML_TEXT_NODE
            && !(*pivot).next.is_null()
            && (*(*pivot).next).type_ == xml::XML_TEXT_NODE
        {
            // The text after the pivot would be merged into (and freed
            // with) the replacement: keep a copy in its place instead.
            let next_text = (*pivot).next;
            let new_next_text = xml::xmlDocCopyNode(next_text, (*pivot).doc, 1);
            xml::xmlUnlinkNode(next_text);
            pin_node(next_text);
            xml::xmlAddNextSibling(pivot, new_next_text);
        }
        let reparented = match how {
            Reparent::Child => xml::xmlAddChild(pivot, reparentee),
            Reparent::NextSibling => xml::xmlAddNextSibling(pivot, reparentee),
            Reparent::PrevSibling => xml::xmlAddPrevSibling(pivot, reparentee),
            Reparent::Replace => replace_node_wrapper(pivot, reparentee),
        };
        if reparented.is_null() {
            return Err(MonorubyErr::runtimeerr("Could not reparent node"));
        }
        if let Some(n) = native_mut::<XmlNode>(reparentee_obj) {
            n.node = reparented;
        }
        reparented
    };
    let reparented_obj = wrap_node(vm, globals, reparented)?;
    vm.invoke_method_inner(globals, IdentId::get_id("decorate!"), reparented_obj, &[], None, None)?;
    // SAFETY: a live node in its tree.
    unsafe {
        raise_if_ancestor_of_self(reparented)?;
        relink_namespace(globals, reparented);
    }
    Ok(reparented_obj)
}

/// Node#add_child_node(node) -> node
#[monoruby_builtin]
fn add_child_node(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    reparent(vm, globals, lfp.self_val(), lfp.arg(0), Reparent::Child)
}

/// Node#add_next_sibling_node(node) -> node
#[monoruby_builtin]
fn add_next_sibling_node(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    reparent(vm, globals, lfp.self_val(), lfp.arg(0), Reparent::NextSibling)
}

/// Node#add_previous_sibling_node(node) -> node
#[monoruby_builtin]
fn add_previous_sibling_node(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    reparent(vm, globals, lfp.self_val(), lfp.arg(0), Reparent::PrevSibling)
}

/// Node#replace_node(node) -> node
#[monoruby_builtin]
fn replace_node(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let result = reparent(vm, globals, lfp.self_val(), lfp.arg(0), Reparent::Replace)?;
    let pivot = this(lfp)?;
    // SAFETY: the replaced node, now out of the tree, stays with the document.
    unsafe { pin_node(pivot) };
    Ok(result)
}

// ---- fragment parsing ----

/// Node#in_context(string, options) -> NodeSet: parse `string` as
/// children of this node (`xmlParseInNodeContext`); errors join the
/// document's `errors`.
#[monoruby_builtin]
fn in_context(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    let data = lfp.arg(0).expect_bytes(&globals.store)?.to_vec();
    let options = lfp.arg(1).expect_integer(&globals.store)? as c_int;
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: a live node of a live document; the error list is
    // registered for the duration of the parse only.
    let (set, doc_val) = unsafe {
        let doc = (*node).doc;
        let doc_val = doc_value(doc).ok_or_else(|| MonorubyErr::runtimeerr("node has no document"))?;
        let doc_is_empty = (*doc).children.is_null();
        let node_children = (*node).children;
        let doc_children = (*doc).children;
        let mut list: *mut xml::xmlNode = std::ptr::null_mut();
        xml::xmlSetStructuredErrorFunc(&mut errors as *mut _ as *mut c_void, Some(collect_error));
        let error = xml::xmlParseInNodeContext(
            node,
            data.as_ptr() as *const c_char,
            data.len() as c_int,
            options,
            &mut list,
        );
        xml::xmlSetStructuredErrorFunc(std::ptr::null_mut(), None);
        if error != 0 {
            (*doc).children = doc_children;
            (*node).children = node_children;
        }
        let mut child = (*doc).children;
        while !child.is_null() {
            (*child).parent = doc as *mut xml::xmlNode;
            child = (*child).next;
        }
        if error != 0 && doc_is_empty && !(*doc).children.is_null() {
            let mut top = node;
            while !(*top).parent.is_null() {
                top = (*top).parent;
            }
            if (*top).type_ == xml::XML_DOCUMENT_FRAG_NODE {
                (*doc).children = std::ptr::null_mut();
            }
        }
        // XML_ERR_INTERNAL_ERROR / XML_ERR_NO_MEMORY
        if error == 1 || error == 2 {
            return Err(MonorubyErr::runtimeerr(format!("error parsing fragment ({error})")));
        }
        let set = xml::xmlXPathNodeSetCreate(std::ptr::null_mut());
        while !list.is_null() {
            let tmp = (*list).next;
            (*list).next = std::ptr::null_mut();
            xml::xmlXPathNodeSetAddUnique(set, list);
            pin_node(list);
            list = tmp;
        }
        (set, doc_val)
    };
    if !errors.is_empty() {
        let new_errors = errors_to_array(vm, globals, &errors)?;
        let err_ary = globals
            .store
            .get_ivar(doc_val, IdentId::get_id("@errors"))
            .unwrap_or_default();
        if err_ary.is_nil() {
            globals.store.set_ivar(doc_val, IdentId::get_id("@errors"), new_errors)?;
        } else {
            vm.invoke_method_inner(globals, IdentId::get_id("concat"), err_ary, &[new_errors], None, None)?;
        }
    }
    wrap_node_set(vm, globals, set, doc_val)
}

/// Node#dump_html -> String (`htmlNodeDump`)
#[monoruby_builtin]
fn dump_html(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    // SAFETY: a live node; the buffer is ours.
    unsafe {
        let buf = xml::xmlBufferCreate();
        xml::htmlNodeDump(buf, (*node).doc, node);
        let s = xml_str(xml::xmlBufferContent(buf));
        xml::xmlBufferFree(buf);
        Ok(s)
    }
}

// ---- serialization ----

/// Node#native_write_to(io, encoding, indent_string, options) -> io
#[monoruby_builtin]
fn native_write_to(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = this(lfp)?;
    let io = lfp.arg(0);
    let encoding = lfp.arg(1);
    let enc = if encoding.as_bool() { Some(cstr(encoding, &globals.store)?) } else { None };
    let indent = cstr(lfp.arg(2), &globals.store)?;
    let options = lfp.arg(3).expect_integer(&globals.store)? as c_int;
    let mut ioctx = IoCtx::new(vm, globals, io);
    // SAFETY: the indent string and IO context outlive the save; the
    // indent globals are restored before returning.
    unsafe {
        *xml::__xmlIndentTreeOutput() = 1;
        let before = *xml::__xmlTreeIndentString();
        *xml::__xmlTreeIndentString() = indent.as_ptr();
        let ctx = xml::xmlSaveToIO(
            Some(io_write),
            Some(io_close),
            &mut ioctx as *mut IoCtx as *mut c_void,
            cptr(&enc) as *const c_char,
            options,
        );
        if !ctx.is_null() {
            xml::xmlSaveTree(ctx, node);
            xml::xmlSaveClose(ctx);
        }
        *xml::__xmlTreeIndentString() = before;
    }
    if let Some(err) = ioctx.error.take() {
        return Err(err);
    }
    Ok(io)
}
