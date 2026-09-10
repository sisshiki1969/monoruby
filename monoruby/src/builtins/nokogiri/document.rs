//! `Nokogiri::XML::Document`: parsing (`read_memory`, `read_io`), `new`,
//! the root element, encoding / version / url.

use super::*;

pub(super) fn init(globals: &mut Globals, c: &Classes) {
    let d = c.document;
    globals.define_builtin_class_func(d, "read_memory", read_memory, 4);
    globals.define_builtin_class_func(d, "read_io", read_io, 4);
    globals.define_builtin_class_func_rest(d, "new", new);
    globals.define_builtin_func(d, "root", root, 0);
    globals.define_builtin_func(d, "root=", set_root, 1);
    globals.define_builtin_func(d, "encoding", encoding, 0);
    globals.define_builtin_func(d, "encoding=", set_encoding, 1);
    globals.define_builtin_func(d, "version", version, 0);
    globals.define_builtin_func(d, "url", url, 0);

    let h = c.html4_document;
    globals.define_builtin_class_func(h, "read_memory", html_read_memory, 4);
    globals.define_builtin_class_func(h, "read_io", html_read_io, 4);
    globals.define_builtin_class_func_rest(h, "new", html_new);
    globals.define_builtin_func(h, "type", html_type, 0);
}

/// The document of `self` (a `Document`).
fn this(lfp: Lfp) -> Result<*mut xml::xmlDoc> {
    doc_ptr(lfp.self_val())
}

/// After a parse: wrap the document into the receiver class with the
/// collected errors in `@errors`, or raise them
/// (`SyntaxError.aggregate`) when nothing was parsed.
fn finish_parse(
    vm: &mut Executor,
    globals: &mut Globals,
    class: ClassId,
    doc: *mut xml::xmlDoc,
    errors: &[ErrorRecord],
) -> Result<Value> {
    let rb_errors = errors_to_array(vm, globals, errors)?;
    if doc.is_null() {
        let klass = globals.store.get_module(classes().xml_syntax_error).as_val();
        let aggregate = IdentId::get_id("aggregate");
        let ex = vm.invoke_method_inner(globals, aggregate, klass, &[rb_errors], None, None)?;
        return Err(if ex.as_bool() {
            raise(ex)
        } else {
            MonorubyErr::runtimeerr("Could not parse document")
        });
    }
    // `wrap_document` runs `initialize`: keep the error list rooted.
    let len = vm.temp_len();
    vm.temp_push(rb_errors);
    let rb_doc = wrap_document(vm, globals, class, doc, &[]);
    vm.temp_clear(len);
    let rb_doc = rb_doc?;
    globals.store.set_ivar(rb_doc, IdentId::get_id("@errors"), rb_errors)?;
    Ok(rb_doc)
}

/// Document.read_memory(string, url, encoding, options) -> Document
#[monoruby_builtin]
fn read_memory(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let input = lfp.arg(0).expect_bytes(&globals.store)?.to_vec();
    let url = opt_cstr(lfp.arg(1), &globals.store)?;
    let enc = opt_cstr(lfp.arg(2), &globals.store)?;
    let options = lfp.arg(3).expect_integer(&globals.store)? as c_int;
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
    finish_parse(vm, globals, class, doc, &errors)
}

/// Document.read_io(io, url, encoding, options) -> Document
#[monoruby_builtin]
fn read_io(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let io = lfp.arg(0);
    let url = opt_cstr(lfp.arg(1), &globals.store)?;
    let enc = opt_cstr(lfp.arg(2), &globals.store)?;
    let options = lfp.arg(3).expect_integer(&globals.store)? as c_int;
    let mut errors: Vec<ErrorRecord> = vec![];
    let mut ioctx = IoCtx::new(vm, globals, io);
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
    if let Some(err) = ioctx.error.take() {
        if !doc.is_null() {
            // SAFETY: a document nobody else holds.
            unsafe { xml::xmlFreeDoc(doc) };
        }
        return Err(err);
    }
    finish_parse(vm, globals, class, doc, &errors)
}

/// Document.new(version = "1.0", ...) -> Document
#[monoruby_builtin]
fn new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let args: Vec<Value> = lfp.arg(0).as_array().iter().cloned().collect();
    let version = match args.first() {
        Some(v) if !v.is_nil() => cstr(*v, &globals.store)?,
        _ => CString::new("1.0").unwrap(),
    };
    // SAFETY: a NUL-terminated version string.
    let doc = unsafe { xml::xmlNewDoc(version.as_ptr() as *const xml::xmlChar) };
    if doc.is_null() {
        return Err(MonorubyErr::runtimeerr("could not create document"));
    }
    wrap_document(vm, globals, class, doc, &args)
}

/// After an HTML parse (`rb_html_document_s_read_memory`): without
/// `RECOVER`, any error or warning is fatal.
fn finish_html_parse(
    vm: &mut Executor,
    globals: &mut Globals,
    class: ClassId,
    doc: *mut xml::xmlDoc,
    errors: &[ErrorRecord],
    options: c_int,
) -> Result<Value> {
    let rb_errors = errors_to_array(vm, globals, errors)?;
    if doc.is_null() || (options & xml::XML_PARSE_RECOVER == 0 && !errors.is_empty()) {
        if !doc.is_null() {
            // SAFETY: a document nobody else holds.
            unsafe { xml::xmlFreeDoc(doc) };
        }
        let Some(first) = errors.first() else {
            return Err(MonorubyErr::runtimeerr("Could not parse document"));
        };
        let ex = syntax_error_value(vm, globals, first)?;
        let text = vm.invoke_method_inner(globals, IdentId::get_id("to_s"), ex, &[], None, None)?;
        let text = text.expect_string(&globals.store)?;
        let msg = Value::string(format!(
            "Parser without recover option encountered error or warning: {text}"
        ));
        let klass = globals.store.get_module(classes().xml_syntax_error).as_val();
        let ex = vm.invoke_method_inner(globals, IdentId::NEW, klass, &[msg], None, None)?;
        return Err(raise(ex));
    }
    let len = vm.temp_len();
    vm.temp_push(rb_errors);
    let rb_doc = wrap_document(vm, globals, class, doc, &[]);
    vm.temp_clear(len);
    let rb_doc = rb_doc?;
    globals.store.set_ivar(rb_doc, IdentId::get_id("@errors"), rb_errors)?;
    Ok(rb_doc)
}

/// HTML4::Document.read_memory(string, url, encoding, options) -> Document
#[monoruby_builtin]
fn html_read_memory(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let input = lfp.arg(0).expect_bytes(&globals.store)?.to_vec();
    let url = opt_cstr(lfp.arg(1), &globals.store)?;
    let enc = opt_cstr(lfp.arg(2), &globals.store)?;
    let options = lfp.arg(3).expect_integer(&globals.store)? as c_int;
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
    finish_html_parse(vm, globals, class, doc, &errors, options)
}

/// HTML4::Document.read_io(io, url, encoding, options) -> Document
#[monoruby_builtin]
fn html_read_io(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let io = lfp.arg(0);
    let url = opt_cstr(lfp.arg(1), &globals.store)?;
    let enc = opt_cstr(lfp.arg(2), &globals.store)?;
    let options = lfp.arg(3).expect_integer(&globals.store)? as c_int;
    let mut errors: Vec<ErrorRecord> = vec![];
    let mut ioctx = IoCtx::new(vm, globals, io);
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
    if let Some(err) = ioctx.error.take() {
        if !doc.is_null() {
            // SAFETY: a document nobody else holds.
            unsafe { xml::xmlFreeDoc(doc) };
        }
        return Err(err);
    }
    // An `EncodingReader` wrapping the IO may have found the document's
    // encoding mid-way and asks for a re-parse (`encoding_found`).
    let encoding_found = IdentId::get_id("encoding_found");
    if let Some(found) = vm.invoke_method_if_exists(globals, encoding_found, io, &[], None, None)?
        && !found.is_nil()
    {
        if !doc.is_null() {
            // SAFETY: a document nobody else holds.
            unsafe { xml::xmlFreeDoc(doc) };
        }
        return Err(raise(found));
    }
    finish_html_parse(vm, globals, class, doc, &errors, options)
}

/// HTML4::Document.new(uri = nil, external_id = nil) -> Document
#[monoruby_builtin]
fn html_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let args: Vec<Value> = lfp.arg(0).as_array().iter().cloned().collect();
    let opt = |i: usize| -> Result<Option<CString>> {
        match args.get(i) {
            Some(v) if v.as_bool() => cstr(*v, &globals.store).map(Some),
            _ => Ok(None),
        }
    };
    let uri = opt(0)?;
    let external_id = opt(1)?;
    // SAFETY: NUL-terminated strings or NULL.
    let doc = unsafe { xml::htmlNewDoc(cptr(&uri), cptr(&external_id)) };
    if doc.is_null() {
        return Err(MonorubyErr::runtimeerr("could not create document"));
    }
    wrap_document(vm, globals, class, doc, &args)
}

/// HTML4::Document#type -> Integer
#[monoruby_builtin]
fn html_type(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let doc = this(lfp)?;
    // SAFETY: a live document.
    Ok(Value::integer(unsafe { (*doc).type_ } as i64))
}

/// Document#root -> Element | nil
#[monoruby_builtin]
fn root(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let doc = this(lfp)?;
    // SAFETY: a live document.
    let root = unsafe { xml::xmlDocGetRootElement(doc) };
    wrap_node_or_nil(vm, globals, root)
}

/// Document#root=(node)
#[monoruby_builtin]
fn set_root(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let doc = this(lfp)?;
    let new_root = lfp.arg(0);
    // SAFETY: live nodes of live documents.
    unsafe {
        let current = xml::xmlDocGetRootElement(doc);
        if !current.is_null() {
            xml::xmlUnlinkNode(current);
            pin_node(current);
        }
        let mut c_new_root = std::ptr::null_mut();
        if !new_root.is_nil() {
            if !is_node(new_root) {
                return Err(MonorubyErr::argumenterr(format!(
                    "expected Nokogiri::XML::Node but received {}",
                    globals.store.get_class_name(new_root.class())
                )));
            }
            c_new_root = node_ptr(new_root)?;
            if (*c_new_root).doc != doc {
                c_new_root = xml::xmlDocCopyNode(c_new_root, doc, 1);
                if c_new_root.is_null() {
                    return Err(MonorubyErr::runtimeerr("Could not reparent node (xmlDocCopyNode)"));
                }
            }
        }
        xml::xmlDocSetRootElement(doc, c_new_root);
    }
    Ok(new_root)
}

/// Document#encoding -> String | nil
#[monoruby_builtin]
fn encoding(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let doc = this(lfp)?;
    // SAFETY: a live document.
    Ok(unsafe { xml_str((*doc).encoding) })
}

/// Document#encoding=(name)
#[monoruby_builtin]
fn set_encoding(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let doc = this(lfp)?;
    let enc = cstr(lfp.arg(0), &globals.store)?;
    // SAFETY: a live document; the old name was allocated by libxml2.
    unsafe {
        if !(*doc).encoding.is_null() {
            xml::xml_free()((*doc).encoding as *mut c_void);
        }
        (*doc).encoding = xml::xmlStrdup(enc.as_ptr() as *const xml::xmlChar);
    }
    Ok(lfp.arg(0))
}

/// Document#version -> String | nil
#[monoruby_builtin]
fn version(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let doc = this(lfp)?;
    // SAFETY: a live document.
    Ok(unsafe { xml_str((*doc).version) })
}

/// Document#url -> String | nil
#[monoruby_builtin]
fn url(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let doc = this(lfp)?;
    // SAFETY: a live document.
    Ok(unsafe { xml_str((*doc).URL) })
}
