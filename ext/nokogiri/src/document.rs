//! `Nokogiri::XML::Document`: parsing (`read_memory`, `read_io`), `new`,
//! the root element, encoding / version / url.

use crate::*;

pub(crate) fn init(ctx: &mut Ctx, c: &Classes) {
    let d = c.document;
    ctx.define_method(
        d,
        "read_memory",
        method!(read_memory),
        4,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(d, "read_io", method!(read_io), 4, MR_METHOD_SINGLETON);
    ctx.define_method(
        d,
        "new",
        method!(new),
        MR_ARGC_VARIADIC,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(d, "root", method!(root), 0, 0);
    ctx.define_method(d, "root=", method!(set_root), 1, 0);
    ctx.define_method(d, "encoding", method!(encoding), 0, 0);
    ctx.define_method(d, "encoding=", method!(set_encoding), 1, 0);
    ctx.define_method(d, "version", method!(version), 0, 0);
    ctx.define_method(d, "url", method!(url), 0, 0);
    ctx.define_method(
        d,
        "initialize_copy_with_args",
        method!(doc_initialize_copy_with_args),
        2,
        0,
    );

    let h = c.html4_document;
    ctx.define_method(
        h,
        "read_memory",
        method!(html_read_memory),
        4,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(h, "read_io", method!(html_read_io), 4, MR_METHOD_SINGLETON);
    ctx.define_method(
        h,
        "new",
        method!(html_new),
        MR_ARGC_VARIADIC,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(h, "type", method!(html_type), 0, 0);
}

/// The document of `self` (a `Document`).
fn recv(ctx: &mut Ctx, this: Value) -> Result<*mut xml::xmlDoc> {
    doc_ptr(ctx, this)
}

/// After a parse: wrap the document into the receiver class with the
/// collected errors in `@errors`, or raise them
/// (`SyntaxError.aggregate`) when nothing was parsed.
fn finish_parse(
    ctx: &mut Ctx,
    class: Value,
    doc: *mut xml::xmlDoc,
    errors: &[ErrorRecord],
) -> Result<Value> {
    let rb_errors = errors_to_array(ctx, errors)?;
    if doc.is_null() {
        let klass = classes().xml_syntax_error;
        let aggregate = "aggregate";
        let ex = ctx.funcall(klass, aggregate, &[rb_errors], None)?;
        return Err(if ex.truthy() {
            raise(ctx, ex)
        } else {
            ctx.runtime_error("Could not parse document")
        });
    }
    // `wrap_document` runs `initialize`: keep the error list rooted.
    let len = ctx.temp_len();
    ctx.temp_push(rb_errors);
    let rb_doc = wrap_document(ctx, class, doc, &[]);
    ctx.temp_truncate(len);
    let rb_doc = rb_doc?;
    ctx.ivar_set(rb_doc, "@errors", rb_errors)?;
    Ok(rb_doc)
}

/// Document.read_memory(string, url, encoding, options) -> Document
fn read_memory(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let input = ctx.str_vec(args[0])?;
    let url = opt_cstr(args[1], ctx)?;
    let enc = opt_cstr(args[2], ctx)?;
    let options = ctx.int(args[3])? as c_int;
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: the buffers outlive the call; the error list is registered
    // as the (thread-local) structured error handler for the call only.
    // The global handler, not the context's: with `NOERROR` /
    // `NOWARNING` libxml2 bypasses the context's handler but still
    // reports through the global one, which is how nokogiri collects
    // `errors` under `DEFAULT_HTML` (and raises under `strict`).
    let doc = unsafe {
        let ctxt = xml::xmlNewParserCtxt();
        xml::xmlSetStructuredErrorFunc(&mut errors as *mut _ as *mut c_void, Some(collect_error));
        let doc = xml::xmlCtxtReadMemory(
            ctxt,
            input.as_ptr() as *const c_char,
            input.len() as c_int,
            cptr(&url) as *const c_char,
            cptr(&enc) as *const c_char,
            options,
        );
        xml::xmlSetStructuredErrorFunc(std::ptr::null_mut(), None);
        xml::xmlFreeParserCtxt(ctxt);
        doc
    };
    finish_parse(ctx, class, doc, &errors)
}

/// Document.read_io(io, url, encoding, options) -> Document
fn read_io(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let io = args[0];
    let url = opt_cstr(args[1], ctx)?;
    let enc = opt_cstr(args[2], ctx)?;
    let options = ctx.int(args[3])? as c_int;
    let mut errors: Vec<ErrorRecord> = vec![];
    let mut ioctx = IoCtx::new(ctx, io);
    // SAFETY: as `read_memory`; the IO context lives across the call.
    let doc = unsafe {
        let ctxt = xml::xmlNewParserCtxt();
        xml::xmlSetStructuredErrorFunc(&mut errors as *mut _ as *mut c_void, Some(collect_error));
        let doc = xml::xmlCtxtReadIO(
            ctxt,
            Some(io_read),
            Some(io_close),
            &mut ioctx as *mut IoCtx as *mut c_void,
            cptr(&url) as *const c_char,
            cptr(&enc) as *const c_char,
            options,
        );
        xml::xmlSetStructuredErrorFunc(std::ptr::null_mut(), None);
        xml::xmlFreeParserCtxt(ctxt);
        doc
    };
    finish_parse(ctx, class, doc, &errors)
}

/// Document.new(version = "1.0", ...) -> Document
fn new(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let args: Vec<Value> = args.to_vec();
    let version = match args.first() {
        Some(v) if !v.is_nil() => cstr(*v, ctx)?,
        _ => CString::new("1.0").unwrap(),
    };
    // SAFETY: a NUL-terminated version string.
    let doc = unsafe { xml::xmlNewDoc(version.as_ptr() as *const xml::xmlChar) };
    if doc.is_null() {
        return Err(ctx.runtime_error("could not create document"));
    }
    wrap_document(ctx, class, doc, &args)
}

/// After an HTML parse (`rb_html_document_s_read_memory`): without
/// `RECOVER`, any error or warning is fatal.
fn finish_html_parse(
    ctx: &mut Ctx,
    class: Value,
    doc: *mut xml::xmlDoc,
    errors: &[ErrorRecord],
    options: c_int,
) -> Result<Value> {
    let rb_errors = errors_to_array(ctx, errors)?;
    if doc.is_null() || (options & xml::XML_PARSE_RECOVER == 0 && !errors.is_empty()) {
        if !doc.is_null() {
            // SAFETY: a document nobody else holds.
            unsafe { xml::xmlFreeDoc(doc) };
        }
        let Some(first) = errors.first() else {
            return Err(ctx.runtime_error("Could not parse document"));
        };
        let ex = syntax_error_value(ctx, first)?;
        let text = ctx.funcall(ex, "to_s", &[], None)?;
        let text = ctx.str_string(text)?;
        let msg = ctx.str(format!(
            "Parser without recover option encountered error or warning: {text}"
        ));
        let klass = classes().xml_syntax_error;
        let ex = ctx.funcall(klass, "new", &[msg], None)?;
        return Err(raise(ctx, ex));
    }
    let len = ctx.temp_len();
    ctx.temp_push(rb_errors);
    let rb_doc = wrap_document(ctx, class, doc, &[]);
    ctx.temp_truncate(len);
    let rb_doc = rb_doc?;
    ctx.ivar_set(rb_doc, "@errors", rb_errors)?;
    Ok(rb_doc)
}

/// HTML4::Document.read_memory(string, url, encoding, options) -> Document
fn html_read_memory(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let input = ctx.str_vec(args[0])?;
    let url = opt_cstr(args[1], ctx)?;
    let enc = opt_cstr(args[2], ctx)?;
    let options = ctx.int(args[3])? as c_int;
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: as `read_memory`, with the HTML parser.
    let doc = unsafe {
        let ctxt = xml::htmlNewParserCtxt();
        xml::xmlSetStructuredErrorFunc(&mut errors as *mut _ as *mut c_void, Some(collect_error));
        let doc = xml::htmlCtxtReadMemory(
            ctxt,
            input.as_ptr() as *const c_char,
            input.len() as c_int,
            cptr(&url) as *const c_char,
            cptr(&enc) as *const c_char,
            options,
        );
        xml::xmlSetStructuredErrorFunc(std::ptr::null_mut(), None);
        xml::xmlFreeParserCtxt(ctxt);
        doc
    };
    finish_html_parse(ctx, class, doc, &errors, options)
}

/// HTML4::Document.read_io(io, url, encoding, options) -> Document
fn html_read_io(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let io = args[0];
    let url = opt_cstr(args[1], ctx)?;
    let enc = opt_cstr(args[2], ctx)?;
    let options = ctx.int(args[3])? as c_int;
    let mut errors: Vec<ErrorRecord> = vec![];
    let mut ioctx = IoCtx::new(ctx, io);
    // SAFETY: as `read_io`, with the HTML parser.
    let doc = unsafe {
        let ctxt = xml::htmlNewParserCtxt();
        xml::xmlSetStructuredErrorFunc(&mut errors as *mut _ as *mut c_void, Some(collect_error));
        let doc = xml::htmlCtxtReadIO(
            ctxt,
            Some(io_read),
            Some(io_close),
            &mut ioctx as *mut IoCtx as *mut c_void,
            cptr(&url) as *const c_char,
            cptr(&enc) as *const c_char,
            options,
        );
        xml::xmlSetStructuredErrorFunc(std::ptr::null_mut(), None);
        xml::xmlFreeParserCtxt(ctxt);
        doc
    };
    // An `EncodingReader` wrapping the IO may have found the document's
    // encoding mid-way and asks for a re-parse (`encoding_found`).
    let encoding_found = "encoding_found";
    if let Some(found) = funcall_if_exists(ctx, io, encoding_found, &[])?
        && !found.is_nil()
    {
        if !doc.is_null() {
            // SAFETY: a document nobody else holds.
            unsafe { xml::xmlFreeDoc(doc) };
        }
        return Err(raise(ctx, found));
    }
    finish_html_parse(ctx, class, doc, &errors, options)
}

/// HTML4::Document.new(uri = nil, external_id = nil) -> Document
fn html_new(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let args: Vec<Value> = args.to_vec();
    let mut opt = |i: usize| -> Result<Option<CString>> {
        match args.get(i) {
            Some(v) if v.truthy() => cstr(*v, ctx).map(Some),
            _ => Ok(None),
        }
    };
    let uri = opt(0)?;
    let external_id = opt(1)?;
    // SAFETY: NUL-terminated strings or NULL.
    let doc = unsafe { xml::htmlNewDoc(cptr(&uri), cptr(&external_id)) };
    if doc.is_null() {
        return Err(ctx.runtime_error("could not create document"));
    }
    wrap_document(ctx, class, doc, &args)
}

/// HTML4::Document#type -> Integer
fn html_type(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let doc = recv(ctx, this)?;
    // SAFETY: a live document.
    Ok(Value::int(unsafe { (*doc).type_ } as i64))
}

/// Document#root -> Element | nil
fn root(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let doc = recv(ctx, this)?;
    // SAFETY: a live document.
    let root = unsafe { xml::xmlDocGetRootElement(doc) };
    wrap_node_or_nil(ctx, root)
}

/// Document#root=(node)
fn set_root(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let doc = recv(ctx, this)?;
    let new_root = args[0];
    // SAFETY: live nodes of live documents.
    unsafe {
        let current = xml::xmlDocGetRootElement(doc);
        if !current.is_null() {
            xml::xmlUnlinkNode(current);
            pin_node(ctx, current);
        }
        let mut c_new_root = std::ptr::null_mut();
        if !new_root.is_nil() {
            if !is_node(ctx, new_root) {
                return Err(ctx.argument_error(format!(
                    "expected Nokogiri::XML::Node but received {}",
                    ctx.class_name(new_root)
                )));
            }
            c_new_root = node_ptr(ctx, new_root)?;
            if (*c_new_root).doc != doc {
                c_new_root = xml::xmlDocCopyNode(c_new_root, doc, 1);
                if c_new_root.is_null() {
                    return Err(ctx.runtime_error("Could not reparent node (xmlDocCopyNode)"));
                }
            }
        }
        xml::xmlDocSetRootElement(doc, c_new_root);
    }
    Ok(new_root)
}

/// Document#encoding -> String | nil
fn encoding(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let doc = recv(ctx, this)?;
    // SAFETY: a live document.
    Ok(unsafe { xml_str(ctx, (*doc).encoding) })
}

/// Document#encoding=(name)
fn set_encoding(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let doc = recv(ctx, this)?;
    let enc = cstr(args[0], ctx)?;
    // SAFETY: a live document; the old name was allocated by libxml2.
    unsafe {
        if !(*doc).encoding.is_null() {
            xml::xml_free()((*doc).encoding as *mut c_void);
        }
        (*doc).encoding = xml::xmlStrdup(enc.as_ptr() as *const xml::xmlChar);
    }
    Ok(args[0])
}

/// Document#version -> String | nil
fn version(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let doc = recv(ctx, this)?;
    // SAFETY: a live document.
    Ok(unsafe { xml_str(ctx, (*doc).version) })
}

/// Document#initialize_copy_with_args(other, level) -> self: the tail of
/// `Document#dup` / `#clone` — `self` is the payload-less copy `Object#dup`
/// made; it becomes the owner of a copy of `other`'s tree
/// (`rb_xml_document_initialize_copy_with_args`).
fn doc_initialize_copy_with_args(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let self_val = this;
    let other = doc_ptr(ctx, args[0])?;
    let level = ctx.int(args[1])? as c_int;
    // SAFETY: a live document; the copy is ours.
    let copy = unsafe { xml::xmlCopyDoc(other, level) };
    if copy.is_null() {
        return Ok(Value::nil());
    }
    // SAFETY: a fresh document.
    unsafe {
        (*copy).type_ = (*other).type_;
        (*copy)._private = self_val.0 as *mut c_void;
    }
    ctx.native_set(
        self_val,
        XmlDocument {
            doc: copy,
            node_cache: vec![],
            unlinked: HashSet::new(),
        },
    )?;
    Ok(self_val)
}

/// Document#url -> String | nil
fn url(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let doc = recv(ctx, this)?;
    // SAFETY: a live document.
    Ok(unsafe { xml_str(ctx, (*doc).URL) })
}
