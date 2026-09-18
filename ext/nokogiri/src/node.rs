//! `Nokogiri::XML::Node` and the node subclasses with native
//! constructors (`Text`, `Comment`, `CDATA`, `ProcessingInstruction`,
//! `Attr`, `DocumentFragment`), and `Namespace`.

use super::node_set::wrap_node_set;
use crate::*;

pub(crate) fn init(ctx: &mut Ctx, c: &Classes) {
    let n = c.node;
    ctx.define_method(
        n,
        "new",
        method!(node_new),
        MR_ARGC_VARIADIC,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(
        n,
        "add_namespace_definition",
        method!(add_namespace_definition),
        2,
        0,
    );
    ctx.define_method(n, "attribute", method!(attribute), 1, 0);
    ctx.define_method(n, "attribute_nodes", method!(attribute_nodes), 0, 0);
    ctx.define_method(n, "attribute_with_ns", method!(attribute_with_ns), 2, 0);
    ctx.define_method(n, "blank?", method!(blank), 0, 0);
    ctx.define_method(n, "child", method!(child), 0, 0);
    ctx.define_method(n, "children", method!(children), 0, 0);
    ctx.define_method(n, "content", method!(content), 0, 0);
    ctx.define_method(
        n,
        "create_internal_subset",
        method!(create_internal_subset),
        3,
        0,
    );
    ctx.define_method(n, "data_ptr?", method!(data_ptr), 0, 0);
    ctx.define_method(n, "document", method!(document), 0, 0);
    ctx.define_method(n, "element_children", method!(element_children), 0, 0);
    ctx.define_method(
        n,
        "encode_special_chars",
        method!(encode_special_chars),
        1,
        0,
    );
    ctx.define_method(n, "external_subset", method!(external_subset), 0, 0);
    ctx.define_method(n, "first_element_child", method!(first_element_child), 0, 0);
    ctx.define_method(n, "internal_subset", method!(internal_subset), 0, 0);
    ctx.define_method(n, "key?", method!(key), 1, 0);
    ctx.define_method(n, "lang", method!(lang), 0, 0);
    ctx.define_method(n, "lang=", method!(set_lang), 1, 0);
    ctx.define_method(n, "last_element_child", method!(last_element_child), 0, 0);
    ctx.define_method(n, "line", method!(line), 0, 0);
    ctx.define_method(n, "line=", method!(set_line), 1, 0);
    ctx.define_method(n, "namespace", method!(namespace), 0, 0);
    ctx.define_method(
        n,
        "namespace_definitions",
        method!(namespace_definitions),
        0,
        0,
    );
    ctx.define_method(n, "namespace_scopes", method!(namespace_scopes), 0, 0);
    ctx.define_method(n, "namespaced_key?", method!(namespaced_key), 2, 0);
    ctx.define_method(n, "native_content=", method!(set_native_content), 1, 0);
    ctx.define_method(n, "next_element", method!(next_element), 0, 0);
    ctx.define_method(n, "next_sibling", method!(next_sibling), 0, 0);
    ctx.define_method(n, "node_name", method!(node_name), 0, 0);
    ctx.define_method(n, "node_name=", method!(set_node_name), 1, 0);
    ctx.define_method(n, "node_type", method!(node_type), 0, 0);
    ctx.define_method(n, "parent", method!(parent), 0, 0);
    ctx.define_method(n, "path", method!(path), 0, 0);
    ctx.define_method(n, "pointer_id", method!(pointer_id), 0, 0);
    ctx.define_method(n, "previous_element", method!(previous_element), 0, 0);
    ctx.define_method(n, "previous_sibling", method!(previous_sibling), 0, 0);
    ctx.define_method(n, "unlink", method!(unlink), 0, 0);
    ctx.define_method(
        n,
        "initialize_copy_with_args",
        method!(initialize_copy_with_args),
        3,
        0,
    );
    ctx.define_method(
        n,
        "add_child_node",
        method!(add_child_node),
        1,
        MR_METHOD_PRIVATE,
    );
    ctx.define_method(
        n,
        "add_next_sibling_node",
        method!(add_next_sibling_node),
        1,
        MR_METHOD_PRIVATE,
    );
    ctx.define_method(
        n,
        "add_previous_sibling_node",
        method!(add_previous_sibling_node),
        1,
        MR_METHOD_PRIVATE,
    );
    ctx.define_method(
        n,
        "replace_node",
        method!(replace_node),
        1,
        MR_METHOD_PRIVATE,
    );
    ctx.define_method(n, "compare", method!(compare), 1, MR_METHOD_PRIVATE);
    ctx.define_method(n, "get", method!(get), 1, MR_METHOD_PRIVATE);
    ctx.define_method(n, "set", method!(set), 2, MR_METHOD_PRIVATE);
    ctx.define_method(
        n,
        "set_namespace",
        method!(set_namespace),
        1,
        MR_METHOD_PRIVATE,
    );
    ctx.define_method(
        n,
        "native_write_to",
        method!(native_write_to),
        4,
        MR_METHOD_PRIVATE,
    );
    ctx.define_method(n, "in_context", method!(in_context), 2, MR_METHOD_PRIVATE);
    ctx.define_method(n, "dump_html", method!(dump_html), 0, MR_METHOD_PRIVATE);

    ctx.define_method(
        c.text,
        "new",
        method!(text_new),
        MR_ARGC_VARIADIC,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(
        c.comment,
        "new",
        method!(comment_new),
        MR_ARGC_VARIADIC,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(
        c.cdata,
        "new",
        method!(cdata_new),
        MR_ARGC_VARIADIC,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(
        c.pi,
        "new",
        method!(pi_new),
        MR_ARGC_VARIADIC,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(
        c.entity_ref,
        "new",
        method!(entity_ref_new),
        MR_ARGC_VARIADIC,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(
        c.attr,
        "new",
        method!(attr_new),
        MR_ARGC_VARIADIC,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(c.attr, "value=", method!(attr_set_value), 1, 0);
    ctx.define_method(
        c.document_fragment,
        "native_new",
        method!(fragment_native_new),
        1,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(c.namespace, "prefix", method!(ns_prefix), 0, 0);
    ctx.define_method(c.namespace, "href", method!(ns_href), 0, 0);
}

/// The node of `self`.
fn recv(ctx: &mut Ctx, this: Value) -> Result<*mut xml::xmlNode> {
    node_ptr(ctx, this)
}

fn class_name(ctx: &Ctx, v: Value) -> String {
    ctx.class_name(v)
}

/// The positional arguments of a `_rest` builtin.
fn rest_args(ctx: &mut Ctx, args: &[Value], min: usize) -> Result<Vec<Value>> {
    let args: Vec<Value> = args.to_vec();
    if args.len() < min {
        return Err(ctx.argument_error(format!(
            "wrong number of arguments (given {}, expected {min}+)",
            args.len()
        )));
    }
    Ok(args)
}

/// The tail of a native constructor: wrap the new node into the receiver
/// class, `initialize(*args)`, yield it to the block.
fn construct(
    ctx: &mut Ctx,
    block: Block,
    class: Option<Value>,
    node: *mut xml::xmlNode,
    args: &[Value],
) -> Result<Value> {
    // SAFETY: a fresh node of a live document.
    unsafe { pin_node(ctx, node) };
    let rb = wrap_node_as(ctx, class, node)?;
    ctx.funcall(rb, "initialize", args, None)?;
    if let Some(bh) = block.given() {
        ctx.yield_block(bh, &[rb])?;
    }
    Ok(rb)
}

/// Node.new(name, document, ...) { |node| } -> Node
fn node_new(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let args = rest_args(ctx, args, 2)?;
    let name = cstr(args[0], ctx)?;
    if !is_node(ctx, args[1]) {
        return Err(ctx.argument_error("document must be a Nokogiri::XML::Node"));
    }
    // SAFETY: a live node; the new node is ours until the document takes it.
    let node = unsafe {
        let doc = (*node_ptr(ctx, args[1])?).doc;
        let node = xml::xmlNewNode(std::ptr::null_mut(), name.as_ptr() as *const xml::xmlChar);
        if node.is_null() {
            return Err(ctx.runtime_error("could not create node"));
        }
        (*node).doc = doc;
        node
    };
    let class = if class == classes().node {
        None
    } else {
        Some(class)
    };
    construct(ctx, block, class, node, &args)
}

/// Text.new(string, document, ...) -> Text
fn text_new(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let args = rest_args(ctx, args, 2)?;
    if !ctx.is_string(args[0]) {
        return Err(ctx.type_error(format!(
            "wrong argument type {} (expected String)",
            builtin_type_name(ctx, args[0])
        )));
    }
    let content = cstr(args[0], ctx)?;
    if !is_node(ctx, args[1]) {
        return Err(ctx.type_error(format!(
            "expected second parameter to be a Nokogiri::XML::Document, received {}",
            class_name(ctx, args[1])
        )));
    }
    // SAFETY: a live document.
    let node = unsafe {
        let doc = (*node_ptr(ctx, args[1])?).doc;
        xml::xmlNewDocText(doc, content.as_ptr() as *const xml::xmlChar)
    };
    construct(ctx, block, Some(class), node, &args)
}

/// Comment.new(document, content, ...) -> Comment
fn comment_new(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let args = rest_args(ctx, args, 2)?;
    if !ctx.is_string(args[1]) {
        return Err(ctx.type_error(format!(
            "wrong argument type {} (expected String)",
            builtin_type_name(ctx, args[1])
        )));
    }
    let content = cstr(args[1], ctx)?;
    let mut document = args[0];
    if ctx.native::<XmlDocument>(document).is_none() {
        if !is_node(ctx, document) {
            return Err(ctx.argument_error("first argument must be a XML::Document or XML::Node"));
        }
        document = ctx.funcall(document, "document", &[], None)?;
    }
    let doc = doc_ptr(ctx, document)?;
    // SAFETY: a live document.
    let node = unsafe { xml::xmlNewDocComment(doc, content.as_ptr() as *const xml::xmlChar) };
    construct(ctx, block, Some(class), node, &args)
}

/// CDATA.new(document, content, ...) -> CDATA
fn cdata_new(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let args = rest_args(ctx, args, 2)?;
    if !is_node(ctx, args[0]) {
        return Err(ctx.type_error(format!(
            "expected first parameter to be a Nokogiri::XML::Document, received {}",
            class_name(ctx, args[0])
        )));
    }
    let content = ctx.str_vec(args[1])?;
    // SAFETY: a live document; the content buffer outlives the call.
    let node = unsafe {
        let doc = (*node_ptr(ctx, args[0])?).doc;
        xml::xmlNewCDataBlock(doc, content.as_ptr(), content.len() as c_int)
    };
    construct(ctx, block, Some(class), node, &args)
}

/// ProcessingInstruction.new(document, name, content, ...) -> ProcessingInstruction
fn pi_new(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let args = rest_args(ctx, args, 3)?;
    let doc = doc_ptr(ctx, args[0])?;
    let name = cstr(args[1], ctx)?;
    let content = cstr(args[2], ctx)?;
    // SAFETY: a live document.
    let node = unsafe {
        xml::xmlNewDocPI(
            doc,
            name.as_ptr() as *const xml::xmlChar,
            content.as_ptr() as *const xml::xmlChar,
        )
    };
    construct(ctx, block, Some(class), node, &args)
}

/// EntityReference.new(document, name, ...) -> EntityReference
fn entity_ref_new(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let args = rest_args(ctx, args, 2)?;
    let doc = doc_ptr(ctx, args[0])?;
    let name = cstr(args[1], ctx)?;
    // SAFETY: a live document.
    let node = unsafe { xml::xmlNewReference(doc, name.as_ptr() as *const xml::xmlChar) };
    construct(ctx, block, Some(class), node, &args)
}

/// Attr.new(document, name, ...) -> Attr
fn attr_new(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let args = rest_args(ctx, args, 2)?;
    if ctx.native::<XmlDocument>(args[0]).is_none() {
        return Err(ctx.argument_error("parameter must be a Nokogiri::XML::Document"));
    }
    let doc = doc_ptr(ctx, args[0])?;
    let name = cstr(args[1], ctx)?;
    // SAFETY: a live document.
    let node =
        unsafe { xml::xmlNewDocProp(doc, name.as_ptr() as *const xml::xmlChar, std::ptr::null()) };
    construct(ctx, block, Some(class), node as *mut xml::xmlNode, &args)
}

/// Attr#value=(content)
fn attr_set_value(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let attr = recv(ctx, this)? as *mut xml::xmlAttr;
    let content = args[0];
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
        let c = cstr(content, ctx)?;
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
fn fragment_native_new(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let doc = doc_ptr(ctx, args[0])?;
    // SAFETY: a live document.
    let node = unsafe { xml::xmlNewDocFragment(doc) };
    // SAFETY: a fresh node.
    unsafe { pin_node(ctx, node) };
    wrap_node_as(ctx, Some(class), node)
}

/// Namespace#prefix -> String | nil
fn ns_prefix(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let ns = namespace_ptr(ctx, this)?;
    // SAFETY: a live namespace.
    Ok(unsafe { xml_str(ctx, (*ns).prefix) })
}

/// Namespace#href -> String | nil
fn ns_href(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let ns = namespace_ptr(ctx, this)?;
    // SAFETY: a live namespace.
    Ok(unsafe { xml_str(ctx, (*ns).href) })
}

// ---- traversal ----

/// Node#document -> Document
fn document(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node.
    unsafe { doc_value((*node).doc) }.ok_or_else(|| ctx.runtime_error("node has no document"))
}

/// Node#parent -> Node | nil
fn parent(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node.
    wrap_node_or_nil(ctx, unsafe { (*node).parent })
}

/// Node#child -> Node | nil
fn child(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node.
    wrap_node_or_nil(ctx, unsafe { (*node).children })
}

/// Node#next_sibling -> Node | nil
fn next_sibling(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node.
    wrap_node_or_nil(ctx, unsafe { (*node).next })
}

/// Node#previous_sibling -> Node | nil
fn previous_sibling(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node.
    wrap_node_or_nil(ctx, unsafe { (*node).prev })
}

/// Node#next_element -> Element | nil
fn next_element(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node.
    wrap_node_or_nil(ctx, unsafe { xml::xmlNextElementSibling(node) })
}

/// Node#previous_element -> Element | nil
fn previous_element(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node.
    wrap_node_or_nil(ctx, unsafe { xml::xmlPreviousElementSibling(node) })
}

/// Node#first_element_child -> Element | nil
fn first_element_child(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node.
    wrap_node_or_nil(ctx, unsafe { xml::xmlFirstElementChild(node) })
}

/// Node#last_element_child -> Element | nil
fn last_element_child(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node.
    wrap_node_or_nil(ctx, unsafe { xml::xmlLastElementChild(node) })
}

/// Node#children -> NodeSet
fn children(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
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
    wrap_node_set(ctx, set, doc)
}

/// Node#element_children -> NodeSet
fn element_children(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
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
    wrap_node_set(ctx, set, doc)
}

// ---- properties ----

/// Node#node_name -> String | nil
fn node_name(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node.
    Ok(unsafe { xml_str(ctx, (*node).name) })
}

/// Node#node_name=(name)
fn set_node_name(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    let name = cstr(args[0], ctx)?;
    // SAFETY: a live node.
    unsafe { xml::xmlNodeSetName(node, name.as_ptr() as *const xml::xmlChar) };
    Ok(args[0])
}

/// Node#node_type -> Integer
fn node_type(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node.
    Ok(Value::int(unsafe { (*node).type_ } as i64))
}

/// Node#content -> String | nil
fn content(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node; the content is ours to free.
    Ok(unsafe { xml_str_owned(ctx, xml::xmlNodeGetContent(node)) })
}

/// Node#native_content=(string)
fn set_native_content(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    let content = cstr(args[0], ctx)?;
    // SAFETY: a live node; the children it drops stay pinned.
    unsafe {
        let mut child = (*node).children;
        while !child.is_null() {
            let next = (*child).next;
            xml::xmlUnlinkNode(child);
            pin_node(ctx, child);
            child = next;
        }
        xml::xmlNodeSetContent(node, content.as_ptr() as *const xml::xmlChar);
    }
    Ok(args[0])
}

/// Node#blank? -> bool
fn blank(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node.
    Ok(Value::bool(unsafe { xml::xmlIsBlankNode(node) } == 1))
}

/// Node#path -> String
fn path(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node; the path is ours to free.
    unsafe {
        let p = xml::xmlGetNodePath(node);
        if p.is_null() {
            // What libxml <= 2.9.10 answered (nokogiri #2250).
            Ok(ctx.str("?"))
        } else {
            Ok(xml_str_owned(ctx, p))
        }
    }
}

/// Node#line -> Integer
fn line(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node.
    Ok(Value::int(unsafe { xml::xmlGetLineNo(node) } as i64))
}

/// Node#line=(n)
fn set_line(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    let n = ctx.int(args[0])?;
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
    Ok(args[0])
}

/// Node#pointer_id -> Integer
fn pointer_id(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    Ok(Value::int(node as usize as i64))
}

/// Node#data_ptr? -> true
fn data_ptr(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    Ok(Value::bool(!node.is_null()))
}

/// Node#lang -> String | nil
fn lang(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node; the string is ours to free.
    Ok(unsafe { xml_str_owned(ctx, xml::xmlNodeGetLang(node)) })
}

/// Node#lang=(lang) -> nil
fn set_lang(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    let lang = cstr(args[0], ctx)?;
    // SAFETY: a live node.
    unsafe { xml::xmlNodeSetLang(node, lang.as_ptr() as *const xml::xmlChar) };
    Ok(Value::nil())
}

/// Node#encode_special_chars(string) -> String
fn encode_special_chars(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    let s = cstr(args[0], ctx)?;
    // SAFETY: a live node; the result is ours to free.
    Ok(unsafe {
        xml_str_owned(
            ctx,
            xml::xmlEncodeSpecialChars((*node).doc, s.as_ptr() as *const xml::xmlChar),
        )
    })
}

/// Node#compare(other) -> Integer (document order, `xmlXPathCmpNodes`)
fn compare(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    let other = node_ptr(ctx, args[0])?;
    // SAFETY: live nodes.
    Ok(Value::int(
        unsafe { xml::xmlXPathCmpNodes(other, node) } as i64
    ))
}

// ---- attributes ----

/// Node#attribute(name) -> Attr | nil
fn attribute(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    let name = cstr(args[0], ctx)?;
    // SAFETY: a live node.
    let prop = unsafe { xml::xmlHasProp(node, name.as_ptr() as *const xml::xmlChar) };
    wrap_node_or_nil(ctx, prop as *mut xml::xmlNode)
}

/// Node#attribute_with_ns(name, namespace) -> Attr | nil
fn attribute_with_ns(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    let name = cstr(args[0], ctx)?;
    let ns = opt_cstr(args[1], ctx)?;
    // SAFETY: a live node.
    let prop = unsafe { xml::xmlHasNsProp(node, name.as_ptr() as *const xml::xmlChar, cptr(&ns)) };
    wrap_node_or_nil(ctx, prop as *mut xml::xmlNode)
}

/// Node#attribute_nodes -> Array of Attr
fn attribute_nodes(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    let mut attrs = vec![];
    // SAFETY: a live node.
    let mut prop = unsafe { (*node).properties };
    while !prop.is_null() {
        attrs.push(wrap_node(ctx, prop as *mut xml::xmlNode)?);
        // SAFETY: a live attribute.
        prop = unsafe { (*prop).next };
    }
    Ok(ctx.ary_from_vec(attrs))
}

/// Node#key?(name) -> bool
fn key(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    let name = cstr(args[0], ctx)?;
    // SAFETY: a live node.
    Ok(Value::bool(
        !unsafe { xml::xmlHasProp(node, name.as_ptr() as *const xml::xmlChar) }.is_null(),
    ))
}

/// Node#namespaced_key?(name, namespace) -> bool
fn namespaced_key(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    let name = cstr(args[0], ctx)?;
    let ns = opt_cstr(args[1], ctx)?;
    // SAFETY: a live node.
    let prop = unsafe { xml::xmlHasNsProp(node, name.as_ptr() as *const xml::xmlChar, cptr(&ns)) };
    Ok(Value::bool(!prop.is_null()))
}

/// Node#get(name) -> String | nil: the attribute's value, `prefix:name`
/// resolved through the namespaces in scope.
fn get(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let arg = args[0];
    if arg.is_nil() {
        return Ok(Value::nil());
    }
    let node = recv(ctx, this)?;
    let full = cstr(arg, ctx)?;
    let bytes = full.as_bytes();
    // SAFETY: a live node; the results are ours to free.
    let value = unsafe {
        match bytes.iter().position(|&b| b == b':') {
            Some(colon) => {
                let prefix = CString::new(&bytes[..colon]).unwrap();
                let name = CString::new(&bytes[colon + 1..]).unwrap();
                let ns =
                    xml::xmlSearchNs((*node).doc, node, prefix.as_ptr() as *const xml::xmlChar);
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
    Ok(unsafe { xml_str_owned(ctx, value) })
}

/// Node#set(name, value) -> value
fn set(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    let value = args[1];
    // SAFETY: a live node.
    unsafe {
        if (*node).type_ != xml::XML_ELEMENT_NODE {
            return Ok(Value::nil());
        }
        let name = cstr(args[0], ctx)?;
        let val = cstr(value, ctx)?;
        let prop = xml::xmlHasProp(node, name.as_ptr() as *const xml::xmlChar);
        if !prop.is_null() && !(*prop).children.is_null() {
            // The old value's text nodes may have Ruby objects: keep them.
            let mut cur = (*prop).children;
            while !cur.is_null() {
                let next = (*cur).next;
                if !(*cur)._private.is_null() {
                    pin_node(ctx, cur);
                    xml::xmlUnlinkNode(cur);
                }
                cur = next;
            }
        }
        xml::xmlSetProp(
            node,
            name.as_ptr() as *const xml::xmlChar,
            val.as_ptr() as *const xml::xmlChar,
        );
    }
    Ok(value)
}

// ---- namespaces ----

/// Node#namespace -> Namespace | nil
fn namespace(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node.
    unsafe {
        if (*node).ns.is_null() {
            return Ok(Value::nil());
        }
        wrap_namespace(ctx, (*node).ns, (*node).doc)
    }
}

/// Node#namespace_definitions -> Array of Namespace
fn namespace_definitions(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let node = recv(ctx, this)?;
    let mut defs = vec![];
    // SAFETY: a live node.
    unsafe {
        let mut ns = (*node).nsDef;
        while !ns.is_null() {
            defs.push(wrap_namespace(ctx, ns, (*node).doc)?);
            ns = (*ns).next;
        }
    }
    Ok(ctx.ary_from_vec(defs))
}

/// Node#namespace_scopes -> Array of Namespace
fn namespace_scopes(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    let mut scopes = vec![];
    // SAFETY: a live node; the list is ours to free.
    unsafe {
        let list = xml::xmlGetNsList((*node).doc, node);
        if !list.is_null() {
            let mut j = 0;
            while !(*list.add(j)).is_null() {
                scopes.push(wrap_namespace(ctx, *list.add(j), (*node).doc)?);
                j += 1;
            }
            xml::xml_free()(list as *mut c_void);
        }
    }
    Ok(ctx.ary_from_vec(scopes))
}

/// Node#add_namespace_definition(prefix, href) -> Namespace | nil
fn add_namespace_definition(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let node = recv(ctx, this)?;
    let prefix = opt_cstr(args[0], ctx)?;
    let href = cstr(args[1], ctx)?;
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
        wrap_namespace(ctx, ns, (*node).doc)
    }
}

/// Node#set_namespace(namespace) -> self
fn set_namespace(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    let arg = args[0];
    let ns = if arg.is_nil() {
        std::ptr::null_mut()
    } else {
        namespace_ptr(ctx, arg)?
    };
    // SAFETY: a live node.
    unsafe { xml::xmlSetNs(node, ns) };
    Ok(this)
}

// ---- DTD ----

/// Node#internal_subset -> DTD | nil
fn internal_subset(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node.
    let dtd = unsafe {
        if (*node).doc.is_null() {
            return Ok(Value::nil());
        }
        xml::xmlGetIntSubset((*node).doc)
    };
    wrap_node_or_nil(ctx, dtd as *mut xml::xmlNode)
}

/// Node#external_subset -> DTD | nil
fn external_subset(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node.
    let dtd = unsafe {
        if (*node).doc.is_null() {
            return Ok(Value::nil());
        }
        (*(*node).doc).extSubset
    };
    wrap_node_or_nil(ctx, dtd as *mut xml::xmlNode)
}

/// Node#create_internal_subset(name, external_id, system_id) -> DTD | nil
fn create_internal_subset(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let node = recv(ctx, this)?;
    let name = opt_cstr(args[0], ctx)?;
    let external_id = opt_cstr(args[1], ctx)?;
    let system_id = opt_cstr(args[2], ctx)?;
    // SAFETY: a live node of a live document.
    let dtd = unsafe {
        let doc = (*node).doc;
        if !xml::xmlGetIntSubset(doc).is_null() {
            return Err(ctx.runtime_error("Document already has an internal subset"));
        }
        xml::xmlCreateIntSubset(doc, cptr(&name), cptr(&external_id), cptr(&system_id))
    };
    wrap_node_or_nil(ctx, dtd as *mut xml::xmlNode)
}

/// Node#initialize_copy_with_args(other, level, new_parent_doc) -> self:
/// the tail of `Node#dup` / `#clone` — `self` is the payload-less copy
/// `Object#dup` made; it becomes the wrapper of a copy of `other` in
/// `new_parent_doc` (`rb_xml_node_initialize_copy_with_args`).
fn initialize_copy_with_args(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let self_val = this;
    let other = node_ptr(ctx, args[0])?;
    let level = ctx.int(args[1])? as c_int;
    let new_doc_val = args[2];
    let new_doc = doc_ptr(ctx, new_doc_val)?;
    // SAFETY: live nodes; the copy is ours until the document takes it.
    let copy = unsafe { xml::xmlDocCopyNode(other, new_doc, level) };
    if copy.is_null() {
        return Ok(Value::nil());
    }
    ctx.native_set(self_val, XmlNode { node: copy })?;
    // SAFETY: a live copy in a live document.
    unsafe {
        (*copy)._private = self_val.0 as *mut c_void;
        pin_node(ctx, copy);
        if let Some(d) = doc_native(ctx, new_doc) {
            d.node_cache.push(self_val);
        }
    }
    ctx.funcall(new_doc_val, "decorate", &[self_val], None)?;
    Ok(self_val)
}

// ---- tree editing ----

/// Node#unlink -> self
fn unlink(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node; unlinked nodes stay owned by the document.
    unsafe {
        xml::xmlUnlinkNode(node);
        pin_node(ctx, node);
    }
    Ok(this)
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
unsafe fn replace_node_wrapper(
    pivot: *mut xml::xmlNode,
    new_node: *mut xml::xmlNode,
) -> *mut xml::xmlNode {
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

unsafe fn raise_if_ancestor_of_self(ctx: &mut Ctx, node: *mut xml::xmlNode) -> Result<()> {
    // SAFETY: a live node.
    unsafe {
        let mut ancestor = (*node).parent;
        while !ancestor.is_null() {
            if ancestor == node {
                let name = CStr::from_ptr((*node).name as *const c_char).to_string_lossy();
                return Err(ctx.runtime_error(format!(
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
unsafe fn relink_namespace(ctx: &Ctx, reparented: *mut xml::xmlNode) {
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
            let is_xmlns =
                !prefix.is_null() && CStr::from_ptr(prefix as *const c_char).to_bytes() == b"xmlns";
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
                .and_then(|d| Some(ctx.ivar_get(d, "@namespace_inheritance")))
                .is_some_and(|v| v == Value::TRUE);
            if inherit {
                xml::xmlSetNs(reparented, (*(*reparented).parent).ns);
            }
        }
        if !(*reparented).nsDef.is_null() {
            let mut curr = (*reparented).nsDef;
            let mut prev: *mut xml::xmlNs = std::ptr::null_mut();
            while !curr.is_null() {
                let ns = xml::xmlSearchNsByHref(doc, (*reparented).parent, (*curr).href);
                if !ns.is_null()
                    && ns != curr
                    && xml::xmlStrEqual((*ns).prefix, (*curr).prefix) != 0
                {
                    if !prev.is_null() {
                        (*prev).next = (*curr).next;
                    } else {
                        (*reparented).nsDef = (*curr).next;
                    }
                    pin_namespace(ctx, curr, doc);
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
            relink_namespace(ctx, child);
            child = (*child).next;
        }
        if ty == xml::XML_ELEMENT_NODE {
            let mut attr = (*reparented).properties;
            while !attr.is_null() {
                relink_namespace(ctx, attr as *mut xml::xmlNode);
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
    ctx: &mut Ctx,
    pivot_obj: Value,
    reparentee_obj: Value,
    how: Reparent,
) -> Result<Value> {
    if !is_node(ctx, reparentee_obj) || ctx.native::<XmlDocument>(reparentee_obj).is_some() {
        return Err(ctx.argument_error("node must be a Nokogiri::XML::Node"));
    }
    let original = node_ptr(ctx, reparentee_obj)?;
    let pivot = node_ptr(ctx, pivot_obj)?;
    // SAFETY: live nodes; every node dropped from a tree stays pinned.
    let reparented = unsafe {
        let parent = if how == Reparent::Child {
            pivot
        } else {
            (*pivot).parent
        };
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
                xml::XML_DOCUMENT_FRAG_NODE | xml::XML_ENTITY_REF_NODE | xml::XML_ELEMENT_NODE => {
                    matches!(
                        rt,
                        xml::XML_ELEMENT_NODE
                            | xml::XML_PI_NODE
                            | xml::XML_COMMENT_NODE
                            | xml::XML_TEXT_NODE
                            | xml::XML_CDATA_SECTION_NODE
                            | xml::XML_ENTITY_REF_NODE
                    )
                }
                xml::XML_ATTRIBUTE_NODE => {
                    matches!(rt, xml::XML_TEXT_NODE | xml::XML_ENTITY_REF_NODE)
                }
                _ => false,
            };
            if !ok {
                return Err(ctx.argument_error(format!(
                    "cannot reparent {} there",
                    class_name(ctx, reparentee_obj)
                )));
            }
        }
        let mut reparentee = original;
        if (*reparentee).doc != (*pivot).doc || (*reparentee).type_ == xml::XML_TEXT_NODE {
            if (*reparentee).type_ == xml::XML_TEXT_NODE && !(*reparentee)._private.is_null() {
                (*reparentee)._private = std::ptr::null_mut();
            }
            let default_prefix =
                !(*reparentee).ns.is_null() && (*(*reparentee).ns).prefix.is_null();
            pin_node(ctx, reparentee);
            reparentee = xml::xmlDocCopyNode(reparentee, (*pivot).doc, 1);
            if reparentee.is_null() {
                return Err(ctx.runtime_error("Could not reparent node (xmlDocCopyNode)"));
            }
            if default_prefix
                && !(*reparentee).ns.is_null()
                && !(*(*reparentee).ns).prefix.is_null()
            {
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
            pin_node(ctx, next_text);
            xml::xmlAddNextSibling(pivot, new_next_text);
        }
        let reparented = match how {
            Reparent::Child => xml::xmlAddChild(pivot, reparentee),
            Reparent::NextSibling => xml::xmlAddNextSibling(pivot, reparentee),
            Reparent::PrevSibling => xml::xmlAddPrevSibling(pivot, reparentee),
            Reparent::Replace => replace_node_wrapper(pivot, reparentee),
        };
        if reparented.is_null() {
            return Err(ctx.runtime_error("Could not reparent node"));
        }
        if let Some(n) = ctx.native::<XmlNode>(reparentee_obj) {
            n.node = reparented;
        }
        reparented
    };
    let reparented_obj = wrap_node(ctx, reparented)?;
    ctx.funcall(reparented_obj, "decorate!", &[], None)?;
    // SAFETY: a live node in its tree.
    unsafe {
        raise_if_ancestor_of_self(ctx, reparented)?;
        relink_namespace(ctx, reparented);
    }
    Ok(reparented_obj)
}

/// Node#add_child_node(node) -> node
fn add_child_node(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    reparent(ctx, this, args[0], Reparent::Child)
}

/// Node#add_next_sibling_node(node) -> node
fn add_next_sibling_node(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    reparent(ctx, this, args[0], Reparent::NextSibling)
}

/// Node#add_previous_sibling_node(node) -> node
fn add_previous_sibling_node(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    reparent(ctx, this, args[0], Reparent::PrevSibling)
}

/// Node#replace_node(node) -> node
fn replace_node(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let result = reparent(ctx, this, args[0], Reparent::Replace)?;
    let pivot = recv(ctx, this)?;
    // SAFETY: the replaced node, now out of the tree, stays with the document.
    unsafe { pin_node(ctx, pivot) };
    Ok(result)
}

// ---- fragment parsing ----

/// Node#in_context(string, options) -> NodeSet: parse `string` as
/// children of this node (`xmlParseInNodeContext`); errors join the
/// document's `errors`.
fn in_context(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    let data = ctx.str_vec(args[0])?;
    let options = ctx.int(args[1])? as c_int;
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: a live node of a live document; the error list is
    // registered for the duration of the parse only.
    let (set, doc_val) = unsafe {
        let doc = (*node).doc;
        let doc_val = doc_value(doc).ok_or_else(|| ctx.runtime_error("node has no document"))?;
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
            return Err(ctx.runtime_error(format!("error parsing fragment ({error})")));
        }
        let set = xml::xmlXPathNodeSetCreate(std::ptr::null_mut());
        while !list.is_null() {
            let tmp = (*list).next;
            (*list).next = std::ptr::null_mut();
            xml::xmlXPathNodeSetAddUnique(set, list);
            pin_node(ctx, list);
            list = tmp;
        }
        (set, doc_val)
    };
    if !errors.is_empty() {
        let new_errors = errors_to_array(ctx, &errors)?;
        let err_ary = ctx.ivar_get(doc_val, "@errors");
        if err_ary.is_nil() {
            ctx.ivar_set(doc_val, "@errors", new_errors)?;
        } else {
            ctx.funcall(err_ary, "concat", &[new_errors], None)?;
        }
    }
    wrap_node_set(ctx, set, doc_val)
}

/// Node#dump_html -> String (`htmlNodeDump`)
fn dump_html(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    // SAFETY: a live node; the buffer is ours.
    unsafe {
        let buf = xml::xmlBufferCreate();
        xml::htmlNodeDump(buf, (*node).doc, node);
        let s = xml_str(ctx, xml::xmlBufferContent(buf));
        xml::xmlBufferFree(buf);
        Ok(s)
    }
}

// ---- serialization ----

/// Node#native_write_to(io, encoding, indent_string, options) -> io
fn native_write_to(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = recv(ctx, this)?;
    let io = args[0];
    let encoding = args[1];
    let enc = if encoding.truthy() {
        Some(cstr(encoding, ctx)?)
    } else {
        None
    };
    let indent = cstr(args[2], ctx)?;
    let options = ctx.int(args[3])? as c_int;
    let mut ioctx = IoCtx::new(ctx, io);
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
    Ok(io)
}
