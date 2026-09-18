//! SAX: `Nokogiri::XML::SAX::Parser` (the `xmlSAXHandler` whose callbacks
//! call the Ruby `SAX::Document`), `SAX::ParserContext` (a pull parse over
//! a String / IO / file), `SAX::PushParser` (`xmlParseChunk`), and the
//! `HTML4::SAX` variants over the HTML parser.
//!
//! libxml2 calls the handler from inside `xmlParseDocument` /
//! `xmlParseChunk`; the callbacks find the running call (`SaxCall`: the
//! executor, the parser object) through the context's `_private`. A Ruby
//! exception raised by the document cannot unwind through C, so the
//! callback stores it, stops the parser (`xmlStopParser`) and the native
//! method re-raises it once the library call returns.

use crate::*;

/// The payload of a `SAX::Parser`: the callback table libxml2 is handed.
struct XmlSaxHandler {
    handler: Box<xml::xmlSAXHandler>,
}

native!(XmlSaxHandler, "XmlSaxHandler");

/// The payload of a `SAX::ParserContext` (XML or HTML4): the parser
/// context, the IO it reads (when made by `native_io`) and the parser
/// whose handler it was last given.
struct XmlSaxParserContext {
    ctxt: *mut xml::xmlParserCtxt,
    io: Option<Box<IoCtx>>,
    /// The bytes of a `native_memory` input: the context reads them in
    /// place, so they are kept (never read from Rust) until it is freed.
    _buffer: Vec<u8>,
    sax: Value,
}

native!(XmlSaxParserContext, "XmlSaxParserContext", |this, m| {
    if let Some(io) = &this.io {
        m.mark(io.io);
    }
    m.mark(this.sax);
});

impl Drop for XmlSaxParserContext {
    fn drop(&mut self) {
        // SAFETY: the context is ours; its handler belongs to the parser
        // object (`xml_sax_parser_context_type_free`).
        unsafe { free_parser_ctxt(self.ctxt) };
    }
}

/// The payload of a `SAX::PushParser` (XML or HTML4): NULL until
/// `initialize_native`.
struct XmlSaxPushParser {
    ctxt: *mut xml::xmlParserCtxt,
    sax: Value,
}

native!(XmlSaxPushParser, "XmlSaxPushParser", |this, m| {
    m.mark(this.sax);
});

impl Drop for XmlSaxPushParser {
    fn drop(&mut self) {
        // SAFETY: as `XmlSaxParserContext` (`xml_sax_push_parser_free`).
        unsafe { free_parser_ctxt(self.ctxt) };
    }
}

/// Free a parser context whose `sax` table (if any) is owned by a
/// `SAX::Parser` object, with the document a SAX2 default may have built.
unsafe fn free_parser_ctxt(ctxt: *mut xml::xmlParserCtxt) {
    if ctxt.is_null() {
        return;
    }
    // SAFETY: a live context that nothing else refers to.
    unsafe {
        xml::mrb_xml_ctxt_set_sax(ctxt, std::ptr::null_mut());
        let doc = xml::mrb_xml_ctxt_get_my_doc(ctxt);
        if !doc.is_null() {
            xml::xmlFreeDoc(doc);
        }
        xml::xmlFreeParserCtxt(ctxt);
    }
}

/// The state of one native call into libxml2 that runs SAX callbacks,
/// reachable from the context's `_private` for its duration.
struct SaxCall {
    ctx: *mut MrContext,
    /// The `SAX::Parser` (its `@document` receives the events).
    parser: Value,
    /// The exception a callback raised: the parser was stopped, and it is
    /// re-raised after the library call.
    error: Option<Value>,
}

/// Run `f` for a callback from libxml2: skipped once an exception is
/// pending; an exception it raises is kept and stops the parser.
unsafe fn with_call(ctx: *mut c_void, f: impl FnOnce(&mut Ctx, Value) -> Result<()>) {
    let ctxt = ctx as *mut xml::xmlParserCtxt;
    // SAFETY: `ctx` is the parser context (`userData`), whose `_private`
    // is the `SaxCall` of the native method running the parse.
    unsafe {
        let call = xml::mrb_xml_ctxt_get_private(ctxt) as *mut SaxCall;
        if call.is_null() {
            return;
        }
        let call = &mut *call;
        if call.error.is_some() {
            return;
        }
        let mut cx = Ctx::from_raw(call.ctx);
        let ctx = &mut cx;
        let doc = ctx.ivar_get(call.parser, "@document");
        if f(ctx, doc).is_err() {
            call.error = Some(stash_error(ctx));
            xml::xmlStopParser(ctxt);
        }
    }
}

fn call(ctx: &mut Ctx, doc: Value, method: &str, args: &[Value]) -> Result<()> {
    ctx.funcall(doc, method, args, None)?;
    Ok(())
}

// ---- the callbacks (`noko_xml_sax_parser_*_callback`) ----

unsafe extern "C" fn cb_start_document(ctx: *mut c_void) {
    // SAFETY: libxml2 passes the context registered as `userData`.
    unsafe {
        xml::xmlSAX2StartDocument(ctx);
        let ctxt = ctx as *mut xml::xmlParserCtxt;
        with_call(ctx, |ctx, doc| {
            let standalone = xml::mrb_xml_ctxt_get_standalone(ctxt);
            // -1: no XML declaration.
            if standalone != -1 {
                let encoding = xml_str(ctx, xml::mrb_xml_ctxt_get_encoding(ctxt));
                let version = xml_str(ctx, xml::mrb_xml_ctxt_get_version(ctxt));
                let standalone = match standalone {
                    0 => ctx.str("no"),
                    1 => ctx.str("yes"),
                    _ => Value::nil(),
                };
                call(ctx, doc, "xmldecl", &[version, encoding, standalone])?;
            }
            call(ctx, doc, "start_document", &[])
        });
    }
}

unsafe extern "C" fn cb_html_start_document(ctx: *mut c_void) {
    // SAFETY: as `cb_start_document`.
    unsafe {
        xml::xmlSAX2StartDocument(ctx);
        with_call(ctx, |ctx, doc| call(ctx, doc, "start_document", &[]));
    }
}

unsafe extern "C" fn cb_end_document(ctx: *mut c_void) {
    // SAFETY: as `cb_start_document`.
    unsafe { with_call(ctx, |ctx, doc| call(ctx, doc, "end_document", &[])) }
}

unsafe extern "C" fn cb_start_element(
    ctx: *mut c_void,
    name: *const xml::xmlChar,
    atts: *mut *const xml::xmlChar,
) {
    // SAFETY: as `cb_start_document`; `atts` is a NULL-terminated list of
    // name / value pairs.
    unsafe {
        with_call(ctx, |ctx, doc| {
            let mut attributes = vec![];
            if !atts.is_null() {
                let mut i = 0;
                while !(*atts.add(i)).is_null() {
                    let attr = xml_str(ctx, *atts.add(i));
                    let value = xml_str(ctx, *atts.add(i + 1));
                    attributes.push(ctx.ary_from_vec(vec![attr, value]));
                    i += 2;
                }
            }
            let attributes = ctx.ary_from_vec(attributes);
            call(ctx, doc, "start_element", &[xml_str(ctx, name), attributes])
        })
    }
}

unsafe extern "C" fn cb_end_element(ctx: *mut c_void, name: *const xml::xmlChar) {
    // SAFETY: as `cb_start_document`.
    unsafe {
        with_call(ctx, |ctx, doc| {
            call(ctx, doc, "end_element", &[xml_str(ctx, name)])
        })
    }
}

unsafe extern "C" fn cb_start_element_ns(
    ctx: *mut c_void,
    localname: *const xml::xmlChar,
    prefix: *const xml::xmlChar,
    uri: *const xml::xmlChar,
    nb_namespaces: c_int,
    namespaces: *mut *const xml::xmlChar,
    nb_attributes: c_int,
    _nb_defaulted: c_int,
    attributes: *mut *const xml::xmlChar,
) {
    // SAFETY: as `cb_start_document`; `namespaces` holds `nb_namespaces`
    // prefix / URI pairs, `attributes` `nb_attributes` quintuples
    // (localname, prefix, URI, value start, value end).
    unsafe {
        with_call(ctx, |ctx, doc| {
            let attr_class = ctx
                .const_get(classes().sax_parser, "Attribute")
                .ok_or_else(|| {
                    name_error(
                        ctx,
                        "uninitialized constant Nokogiri::XML::SAX::Parser::Attribute",
                    )
                })?;
            let attr_ary = ctx.ary_from_vec(vec![]);
            // `Attribute.new` runs Ruby: keep the array (and so the
            // attributes made so far) rooted.
            let len = ctx.temp_len();
            ctx.temp_push(attr_ary);
            let mut fill = || -> Result<()> {
                if !attributes.is_null() {
                    for i in (0..nb_attributes as usize * 5).step_by(5) {
                        let start = *attributes.add(i + 3);
                        let end = *attributes.add(i + 4);
                        let value =
                            std::slice::from_raw_parts(start, end.offset_from(start) as usize);
                        let args = [
                            xml_str(ctx, *attributes.add(i)),
                            xml_str(ctx, *attributes.add(i + 1)),
                            xml_str(ctx, *attributes.add(i + 2)),
                            utf8(ctx, value),
                        ];
                        let attr = ctx.funcall(attr_class, "new", &args, None)?;
                        ctx.ary_push(attr_ary, attr)?;
                    }
                }
                Ok(())
            };
            let r = fill();
            ctx.temp_truncate(len);
            r?;
            let mut ns_list = vec![];
            if !namespaces.is_null() {
                for i in (0..nb_namespaces as usize * 2).step_by(2) {
                    ns_list.push(ctx.ary_from_vec(vec![
                        xml_str(ctx, *namespaces.add(i)),
                        xml_str(ctx, *namespaces.add(i + 1)),
                    ]));
                }
            }
            let args = [
                xml_str(ctx, localname),
                attr_ary,
                xml_str(ctx, prefix),
                xml_str(ctx, uri),
                ctx.ary_from_vec(ns_list),
            ];
            call(ctx, doc, "start_element_namespace", &args)
        })
    }
}

unsafe extern "C" fn cb_end_element_ns(
    ctx: *mut c_void,
    localname: *const xml::xmlChar,
    prefix: *const xml::xmlChar,
    uri: *const xml::xmlChar,
) {
    // SAFETY: as `cb_start_document`.
    unsafe {
        with_call(ctx, |ctx, doc| {
            let args = [
                xml_str(ctx, localname),
                xml_str(ctx, prefix),
                xml_str(ctx, uri),
            ];
            call(ctx, doc, "end_element_namespace", &args)
        })
    }
}

unsafe extern "C" fn cb_characters(ctx: *mut c_void, ch: *const xml::xmlChar, len: c_int) {
    // SAFETY: as `cb_start_document`; `ch` has `len` bytes.
    unsafe {
        with_call(ctx, |ctx, doc| {
            let s = utf8(ctx, std::slice::from_raw_parts(ch, len as usize));
            call(ctx, doc, "characters", &[s])
        })
    }
}

unsafe extern "C" fn cb_comment(ctx: *mut c_void, value: *const xml::xmlChar) {
    // SAFETY: as `cb_start_document`.
    unsafe {
        with_call(ctx, |ctx, doc| {
            call(ctx, doc, "comment", &[xml_str(ctx, value)])
        })
    }
}

unsafe extern "C" fn cb_cdata_block(ctx: *mut c_void, value: *const xml::xmlChar, len: c_int) {
    // SAFETY: as `cb_characters`.
    unsafe {
        with_call(ctx, |ctx, doc| {
            let s = utf8(ctx, std::slice::from_raw_parts(value, len as usize));
            call(ctx, doc, "cdata_block", &[s])
        })
    }
}

unsafe extern "C" fn cb_processing_instruction(
    ctx: *mut c_void,
    name: *const xml::xmlChar,
    content: *const xml::xmlChar,
) {
    // SAFETY: as `cb_start_document`.
    unsafe {
        with_call(ctx, |ctx, doc| {
            call(
                ctx,
                doc,
                "processing_instruction",
                &[xml_str(ctx, name), xml_str(ctx, content)],
            )
        })
    }
}

unsafe extern "C" fn cb_reference(ctx: *mut c_void, name: *const xml::xmlChar) {
    // SAFETY: as `cb_start_document`; the entity is the document's.
    unsafe {
        let entity = xml::xmlSAX2GetEntity(ctx, name);
        with_call(ctx, |ctx, doc| {
            let args = if !entity.is_null() && !(*entity).content.is_null() {
                [
                    xml_str(ctx, (*entity).name),
                    xml_str(ctx, (*entity).content),
                ]
            } else {
                [xml_str(ctx, name), Value::nil()]
            };
            call(ctx, doc, "reference", &args)
        })
    }
}

/// The formatted `warning` / `error` text from the glue's variadic
/// callbacks.
unsafe extern "C" fn cb_message(ctx: *mut c_void, is_error: c_int, text: *const c_char) {
    // SAFETY: as `cb_start_document`; `text` is NUL-terminated.
    unsafe {
        let text = CStr::from_ptr(text).to_bytes();
        with_call(ctx, |ctx, doc| {
            let msg = utf8(ctx, text);
            call(
                ctx,
                doc,
                if is_error != 0 { "error" } else { "warning" },
                &[msg],
            )
        })
    }
}

/// Fill the table as `noko_xml_sax_parser__initialize_native` does: the
/// Ruby-facing callbacks, and libxml2's SAX2 defaults for DTDs and
/// entities.
fn fill_handler(h: &mut xml::xmlSAXHandler) {
    h.startDocument = Some(cb_start_document);
    h.endDocument = Some(cb_end_document);
    h.startElement = Some(cb_start_element);
    h.endElement = Some(cb_end_element);
    h.startElementNs = Some(cb_start_element_ns);
    h.endElementNs = Some(cb_end_element_ns);
    h.characters = Some(cb_characters);
    h.comment = Some(cb_comment);
    h.warning = Some(xml::mrb_xml_sax_warning);
    h.error = Some(xml::mrb_xml_sax_error);
    h.cdataBlock = Some(cb_cdata_block);
    h.processingInstruction = Some(cb_processing_instruction);
    h.reference = Some(cb_reference);
    h.getEntity = Some(xml::xmlSAX2GetEntity);
    h.internalSubset = Some(xml::xmlSAX2InternalSubset);
    h.externalSubset = Some(xml::xmlSAX2ExternalSubset);
    h.isStandalone = Some(xml::xmlSAX2IsStandalone);
    h.hasInternalSubset = Some(xml::xmlSAX2HasInternalSubset);
    h.hasExternalSubset = Some(xml::xmlSAX2HasExternalSubset);
    h.resolveEntity = Some(xml::xmlSAX2ResolveEntity);
    h.getParameterEntity = Some(xml::xmlSAX2GetParameterEntity);
    h.entityDecl = Some(xml::xmlSAX2EntityDecl);
    h.unparsedEntityDecl = Some(xml::xmlSAX2UnparsedEntityDecl);
    h.initialized = xml::XML_SAX2_MAGIC;
}

pub(crate) fn init(ctx: &mut Ctx, c: &Classes) {
    // SAFETY: registering the (single, static) message sink.
    unsafe { xml::mrb_xml_sax_set_message_handler(Some(cb_message)) };

    ctx.define_method(
        c.sax_parser,
        "initialize_native",
        method!(initialize_native),
        0,
        MR_METHOD_PRIVATE,
    );
    ctx.define_method(
        c.html4_sax_parser,
        "initialize_native",
        method!(html_initialize_native),
        0,
        MR_METHOD_PRIVATE,
    );

    let x = c.sax_parser_context;
    ctx.define_method(x, "native_io", method!(native_io), 2, MR_METHOD_SINGLETON);
    ctx.define_method(
        x,
        "native_memory",
        method!(native_memory),
        2,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(
        x,
        "native_file",
        method!(native_file),
        2,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(x, "parse_with", method!(parse_with), 1, 0);
    ctx.define_method(
        x,
        "replace_entities=",
        method!(ctx_set_replace_entities),
        1,
        0,
    );
    ctx.define_method(x, "replace_entities", method!(ctx_replace_entities), 0, 0);
    ctx.define_method(x, "recovery=", method!(ctx_set_recovery), 1, 0);
    ctx.define_method(x, "recovery", method!(ctx_recovery), 0, 0);
    ctx.define_method(x, "line", method!(ctx_line), 0, 0);
    ctx.define_method(x, "column", method!(ctx_column), 0, 0);
    let h = c.html4_sax_parser_context;
    ctx.define_method(
        h,
        "native_memory",
        method!(html_native_memory),
        2,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(
        h,
        "native_file",
        method!(html_native_file),
        2,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(h, "parse_with", method!(html_parse_with), 1, 0);

    let p = c.sax_push_parser;
    ctx.define_method(p, "options", method!(push_options), 0, 0);
    ctx.define_method(p, "options=", method!(push_set_options), 1, 0);
    ctx.define_method(p, "replace_entities", method!(push_replace_entities), 0, 0);
    ctx.define_method(
        p,
        "replace_entities=",
        method!(push_set_replace_entities),
        1,
        0,
    );
    ctx.define_method(
        p,
        "initialize_native",
        method!(push_initialize_native),
        2,
        MR_METHOD_PRIVATE,
    );
    ctx.define_method(
        p,
        "native_write",
        method!(push_native_write),
        2,
        MR_METHOD_PRIVATE,
    );
    let hp = c.html4_sax_push_parser;
    ctx.define_method(
        hp,
        "initialize_native",
        method!(html_push_initialize_native),
        3,
        MR_METHOD_PRIVATE,
    );
    ctx.define_method(
        hp,
        "native_write",
        method!(html_push_native_write),
        2,
        MR_METHOD_PRIVATE,
    );
}

// ---- SAX::Parser ----

/// The handler table of a `SAX::Parser`.
/// The handler table of a `SAX::Parser`, made (zeroed) on first use: an
/// instance is allocated by the interpreter with no payload
/// (`xml_sax_parser_allocate`'s job).
fn handler_ptr(ctx: &mut Ctx, v: Value) -> Result<*mut xml::xmlSAXHandler> {
    if ctx.native::<XmlSaxHandler>(v).is_none() {
        if !ctx.is_kind_of(v, classes().sax_parser) {
            return Err(ctx.argument_error("argument must be a Nokogiri::XML::SAX::Parser"));
        }
        // SAFETY: an all-NULL callback table is libxml2's empty handler.
        let handler = Box::new(unsafe { std::mem::zeroed::<xml::xmlSAXHandler>() });
        ctx.native_set(v, XmlSaxHandler { handler })?;
    }
    let h = ctx.native::<XmlSaxHandler>(v).unwrap();
    Ok(&*h.handler as *const xml::xmlSAXHandler as *mut xml::xmlSAXHandler)
}

/// SAX::Parser#initialize_native -> self
fn initialize_native(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let h = handler_ptr(ctx, this)?;
    // SAFETY: the table is owned by `self`.
    unsafe { fill_handler(&mut *h) };
    Ok(this)
}

/// HTML4::SAX::Parser#initialize_native -> self: the XML table with the
/// HTML `start_document` (no XML declaration).
fn html_initialize_native(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let h = handler_ptr(ctx, this)?;
    // SAFETY: as `initialize_native`.
    unsafe {
        fill_handler(&mut *h);
        (*h).startDocument = Some(cb_html_start_document);
    }
    Ok(this)
}

// ---- SAX::ParserContext ----

fn context_ptr(ctx: &mut Ctx, v: Value) -> Result<*mut xml::xmlParserCtxt> {
    match ctx.native::<XmlSaxParserContext>(v) {
        Some(c) => Ok(c.ctxt),
        None => Err(ctx.argument_error("expected a Nokogiri::XML::SAX::ParserContext")),
    }
}

/// `nil` or an `Encoding`, else TypeError (nokogiri's check).
fn check_encoding(ctx: &mut Ctx, enc: Value) -> Result<()> {
    if enc.is_nil() {
        return Ok(());
    }
    let encoding_class = ctx.const_get(Value::UNDEF, "Encoding");
    if encoding_class.is_some_and(|c| ctx.is_kind_of(enc, c)) {
        return Ok(());
    }
    Err(ctx.type_error("argument must be an Encoding object"))
}

/// `noko_xml_sax_parser_context_set_encoding`: switch the context to the
/// `Encoding`'s name; on failure the context is freed and the libxml2
/// errors raised.
fn set_encoding(ctx: &mut Ctx, ctxt: *mut xml::xmlParserCtxt, enc: Value) -> Result<()> {
    if enc.is_nil() {
        return Ok(());
    }
    let name = ctx.funcall(enc, "name", &[], None)?;
    let name = cstr(name, ctx)?;
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: a live context; the handler is registered for the call.
    let result = unsafe {
        xml::xmlSetStructuredErrorFunc(&mut errors as *mut _ as *mut c_void, Some(collect_error));
        let r = xml::xmlSwitchEncodingName(ctxt, name.as_ptr());
        xml::xmlSetStructuredErrorFunc(std::ptr::null_mut(), None);
        r
    };
    if result != 0 {
        // SAFETY: the context is not wrapped yet.
        unsafe { xml::xmlFreeParserCtxt(ctxt) };
        let rb_errors = errors_to_array(ctx, &errors)?;
        let klass = classes().xml_syntax_error;
        let ex = ctx.funcall(klass, "aggregate", &[rb_errors], None)?;
        return Err(if ex.is_nil() {
            ctx.runtime_error("could not set encoding")
        } else {
            raise(ctx, ex)
        });
    }
    Ok(())
}

/// The context's default SAX table is replaced by the parser's at
/// `parse_with`: drop it now (`xmlFree(c_context->sax)`).
unsafe fn drop_default_sax(ctxt: *mut xml::xmlParserCtxt) {
    // SAFETY: a live context whose table libxml2 allocated.
    unsafe {
        let sax = xml::mrb_xml_ctxt_get_sax(ctxt);
        if !sax.is_null() {
            xml::xml_free()(sax as *mut c_void);
            xml::mrb_xml_ctxt_set_sax(ctxt, std::ptr::null_mut());
        }
    }
}

fn wrap_context(
    ctx: &mut Ctx,
    class: Value,
    ctxt: *mut xml::xmlParserCtxt,
    io: Option<Box<IoCtx>>,
    buffer: Vec<u8>,
    input: Value,
) -> Result<Value> {
    let rb = ctx.native_new(
        class,
        XmlSaxParserContext {
            ctxt,
            io,
            _buffer: buffer,
            sax: Value::nil(),
        },
    )?;
    if !input.is_nil() {
        ctx.ivar_set(rb, "@input", input)?;
    }
    Ok(rb)
}

/// SAX::ParserContext.native_io(io, encoding) -> ParserContext
fn native_io(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let io = args[0];
    let enc = args[1];
    if !ctx.respond_to(io, "read") {
        return Err(ctx.type_error("argument expected to respond to :read"));
    }
    check_encoding(ctx, enc)?;
    let mut ioctx = Box::new(IoCtx::new(ctx, io));
    // SAFETY: the IO context lives in the payload for as long as the
    // parser context; its executor pointers are refreshed at each parse.
    let ctxt = unsafe {
        xml::xmlCreateIOParserCtxt(
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            Some(io_read),
            Some(io_close),
            &mut *ioctx as *mut IoCtx as *mut c_void,
            xml::XML_CHAR_ENCODING_NONE,
        )
    };
    if ctxt.is_null() {
        return Err(ctx.runtime_error("failed to create xml sax parser context"));
    }
    set_encoding(ctx, ctxt, enc)?;
    // SAFETY: a live context.
    unsafe { drop_default_sax(ctxt) };
    wrap_context(ctx, class, ctxt, Some(ioctx), vec![], io)
}

/// SAX::ParserContext.native_file(path, encoding) -> ParserContext
fn native_file(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let enc = args[1];
    check_encoding(ctx, enc)?;
    let path = cstr(args[0], ctx)?;
    // SAFETY: a NUL-terminated path.
    let ctxt = unsafe { xml::xmlCreateFileParserCtxt(path.as_ptr()) };
    if ctxt.is_null() {
        return Err(ctx.runtime_error("failed to create xml sax parser context"));
    }
    set_encoding(ctx, ctxt, enc)?;
    // SAFETY: a live context.
    unsafe { drop_default_sax(ctxt) };
    wrap_context(ctx, class, ctxt, None, vec![], Value::nil())
}

/// The bytes of a `native_memory` input (`Check_Type(T_STRING)`, then
/// "input string cannot be empty").
fn memory_input(ctx: &mut Ctx, input: Value) -> Result<Vec<u8>> {
    let Some(bytes) = try_bytes(ctx, input) else {
        return Err(ctx.type_error(format!(
            "wrong argument type {} (expected String)",
            builtin_type_name(ctx, input)
        )));
    };
    if bytes.is_empty() {
        return Err(ctx.runtime_error("input string cannot be empty"));
    }
    Ok(bytes.to_vec())
}

/// SAX::ParserContext.native_memory(string, encoding) -> ParserContext
fn native_memory(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let buffer = memory_input(ctx, args[0])?;
    let enc = args[1];
    check_encoding(ctx, enc)?;
    // SAFETY: the buffer moves into the payload, which outlives the
    // context's reads of it.
    let ctxt = unsafe {
        xml::xmlCreateMemoryParserCtxt(buffer.as_ptr() as *const c_char, buffer.len() as c_int)
    };
    if ctxt.is_null() {
        return Err(ctx.runtime_error("failed to create xml sax parser context"));
    }
    set_encoding(ctx, ctxt, enc)?;
    // SAFETY: a live context.
    unsafe { drop_default_sax(ctxt) };
    wrap_context(ctx, class, ctxt, None, buffer, args[0])
}

/// HTML4::SAX::ParserContext.native_memory(string, encoding) -> ParserContext
fn html_native_memory(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let buffer = memory_input(ctx, args[0])?;
    let enc = args[1];
    check_encoding(ctx, enc)?;
    // SAFETY: as `native_memory`, with the HTML parser.
    let ctxt = unsafe {
        xml::htmlCreateMemoryParserCtxt(buffer.as_ptr() as *const c_char, buffer.len() as c_int)
    };
    if ctxt.is_null() {
        return Err(ctx.runtime_error("failed to create xml sax parser context"));
    }
    set_encoding(ctx, ctxt, enc)?;
    // SAFETY: a live context.
    unsafe { drop_default_sax(ctxt) };
    wrap_context(ctx, class, ctxt, None, buffer, Value::nil())
}

/// HTML4::SAX::ParserContext.native_file(path, encoding) -> ParserContext
fn html_native_file(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let enc = args[1];
    check_encoding(ctx, enc)?;
    let path = cstr(args[0], ctx)?;
    // SAFETY: a NUL-terminated path.
    let ctxt = unsafe { xml::htmlCreateFileParserCtxt(path.as_ptr(), std::ptr::null()) };
    if ctxt.is_null() {
        return Err(ctx.runtime_error("failed to create xml sax parser context"));
    }
    set_encoding(ctx, ctxt, enc)?;
    // SAFETY: a live context.
    unsafe { drop_default_sax(ctxt) };
    wrap_context(ctx, class, ctxt, None, vec![], Value::nil())
}

/// Give the context the parser's handler and run `parse` under a
/// `SaxCall`; a pending exception is re-raised afterwards.
fn parse_context(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    parse: unsafe extern "C" fn(*mut xml::xmlParserCtxt) -> c_int,
) -> Result<Value> {
    let self_val = this;
    let ctxt = context_ptr(ctx, self_val)?;
    let parser = args[0];
    if !ctx.is_kind_of(parser, classes().sax_parser) {
        return Err(ctx.argument_error("argument must be a Nokogiri::XML::SAX::Parser"));
    }
    let sax = handler_ptr(ctx, parser)?;
    // SAFETY: the payload is ours for the call; the IO callbacks may run
    // Ruby, with the pointers refreshed here.
    let raw = ctx.raw();
    {
        let pc = ctx.native::<XmlSaxParserContext>(self_val).unwrap();
        pc.sax = parser;
        if let Some(io) = &mut pc.io {
            io.ctx = raw;
        }
    }
    let mut call = SaxCall {
        ctx: ctx.raw(),
        parser,
        error: None,
    };
    // SAFETY: a live context; the handler belongs to `parser`, which the
    // payload now keeps alive; `call` outlives the parse.
    unsafe {
        xml::mrb_xml_ctxt_set_sax(ctxt, sax);
        xml::mrb_xml_ctxt_set_user_data(ctxt, ctxt as *mut c_void);
        xml::mrb_xml_ctxt_set_private(ctxt, &mut call as *mut SaxCall as *mut c_void);
        xml::xmlSetStructuredErrorFunc(std::ptr::null_mut(), None);
        parse(ctxt);
        xml::mrb_xml_ctxt_set_private(ctxt, std::ptr::null_mut());
    }
    if let Some(e) = call.error {
        return Err(raise(ctx, e));
    }
    Ok(Value::nil())
}

/// SAX::ParserContext#parse_with(parser) -> nil
fn parse_with(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    parse_context(ctx, this, args, xml::xmlParseDocument)
}

/// HTML4::SAX::ParserContext#parse_with(parser) -> nil
fn html_parse_with(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    parse_context(ctx, this, args, xml::htmlParseDocument)
}

/// Set or clear a parse option on a context.
fn set_option(
    ctx: &mut Ctx,
    ctxt: *mut xml::xmlParserCtxt,
    option: c_int,
    on: bool,
    what: &str,
) -> Result<()> {
    // SAFETY: a live context.
    let error = unsafe {
        let options = xml::mrb_xml_ctxt_get_options(ctxt);
        let options = if on {
            options | option
        } else {
            options & !option
        };
        xml::xmlCtxtSetOptions(ctxt, options)
    };
    if error != 0 {
        return Err(ctx.runtime_error(format!("failed to set {what} ({error:x})")));
    }
    Ok(())
}

fn has_option(ctxt: *mut xml::xmlParserCtxt, option: c_int) -> Value {
    // SAFETY: a live context.
    Value::bool(unsafe { xml::mrb_xml_ctxt_get_options(ctxt) } & option != 0)
}

/// SAX::ParserContext#replace_entities=(value)
fn ctx_set_replace_entities(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let ctxt = context_ptr(ctx, this)?;
    set_option(
        ctx,
        ctxt,
        xml::XML_PARSE_NOENT,
        args[0].truthy(),
        "parser context options",
    )?;
    Ok(args[0])
}

/// SAX::ParserContext#replace_entities -> bool
fn ctx_replace_entities(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    Ok(has_option(context_ptr(ctx, this)?, xml::XML_PARSE_NOENT))
}

/// SAX::ParserContext#recovery=(value)
fn ctx_set_recovery(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let ctxt = context_ptr(ctx, this)?;
    set_option(
        ctx,
        ctxt,
        xml::XML_PARSE_RECOVER,
        args[0].truthy(),
        "parser context options",
    )?;
    Ok(args[0])
}

/// SAX::ParserContext#recovery -> bool
fn ctx_recovery(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    Ok(has_option(context_ptr(ctx, this)?, xml::XML_PARSE_RECOVER))
}

/// SAX::ParserContext#line -> Integer | nil
fn ctx_line(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let ctxt = context_ptr(ctx, this)?;
    // SAFETY: a live context.
    let line = unsafe { xml::mrb_xml_ctxt_get_line(ctxt) };
    Ok(if line < 0 {
        Value::nil()
    } else {
        Value::int(line as i64)
    })
}

/// SAX::ParserContext#column -> Integer | nil
fn ctx_column(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let ctxt = context_ptr(ctx, this)?;
    // SAFETY: a live context.
    let col = unsafe { xml::mrb_xml_ctxt_get_column(ctxt) };
    Ok(if col < 0 {
        Value::nil()
    } else {
        Value::int(col as i64)
    })
}

// ---- SAX::PushParser ----

fn push_ptr(ctx: &mut Ctx, v: Value) -> Result<*mut xml::xmlParserCtxt> {
    match ctx.native::<XmlSaxPushParser>(v) {
        Some(p) if !p.ctxt.is_null() => Ok(p.ctxt),
        Some(_) => Err(ctx.runtime_error("push parser is not initialized")),
        None => Err(ctx.argument_error("expected a Nokogiri::XML::SAX::PushParser")),
    }
}

/// Install a freshly created push context into `self`.
fn install_push_ctxt(
    ctx: &mut Ctx,
    self_val: Value,
    ctxt: *mut xml::xmlParserCtxt,
    sax: Value,
) -> Result<Value> {
    if ctxt.is_null() {
        return Err(ctx.runtime_error("Could not create a parser context"));
    }
    if ctx.native::<XmlSaxPushParser>(self_val).is_none() {
        // A fresh instance has no payload yet (`xml_sax_push_parser_allocate`).
        if !ctx.is_kind_of(self_val, classes().sax_push_parser) {
            // SAFETY: a context nobody else holds.
            unsafe { free_parser_ctxt(ctxt) };
            return Err(ctx.argument_error("expected a Nokogiri::XML::SAX::PushParser"));
        }
        ctx.native_set(
            self_val,
            XmlSaxPushParser {
                ctxt: std::ptr::null_mut(),
                sax: Value::nil(),
            },
        )?;
    }
    // SAFETY: the payload is ours; the context is live.
    unsafe {
        xml::mrb_xml_ctxt_set_user_data(ctxt, ctxt as *mut c_void);
        let p = ctx.native::<XmlSaxPushParser>(self_val).unwrap();
        free_parser_ctxt(p.ctxt);
        p.ctxt = ctxt;
        p.sax = sax;
    }
    Ok(self_val)
}

/// SAX::PushParser#initialize_native(sax, filename) -> self
fn push_initialize_native(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let sax = args[0];
    let handler = handler_ptr(ctx, sax)?;
    let filename = opt_cstr(args[1], ctx)?;
    // SAFETY: the handler belongs to `sax`, kept alive by the payload.
    let ctxt = unsafe {
        xml::xmlCreatePushParserCtxt(
            handler,
            std::ptr::null_mut(),
            std::ptr::null(),
            0,
            cptr(&filename) as *const c_char,
        )
    };
    install_push_ctxt(ctx, this, ctxt, sax)
}

/// HTML4::SAX::PushParser#initialize_native(sax, filename, encoding) -> self
fn html_push_initialize_native(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let sax = args[0];
    let handler = handler_ptr(ctx, sax)?;
    let filename = opt_cstr(args[1], ctx)?;
    let mut enc = xml::XML_CHAR_ENCODING_NONE;
    if !args[2].is_nil() {
        let name = cstr(args[2], ctx)?;
        // SAFETY: a NUL-terminated name.
        enc = unsafe { xml::xmlParseCharEncoding(name.as_ptr()) };
        if enc == xml::XML_CHAR_ENCODING_ERROR {
            return Err(ctx.argument_error("Unsupported Encoding"));
        }
    }
    // SAFETY: as `push_initialize_native`.
    let ctxt = unsafe {
        xml::htmlCreatePushParserCtxt(
            handler,
            std::ptr::null_mut(),
            std::ptr::null(),
            0,
            cptr(&filename) as *const c_char,
            enc,
        )
    };
    install_push_ctxt(ctx, this, ctxt, sax)
}

/// Feed a chunk (`native_write`): a callback's exception is re-raised;
/// otherwise a parse failure without `RECOVER` raises the context's last
/// error.
fn push_write(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    parse_chunk: unsafe extern "C" fn(
        *mut xml::xmlParserCtxt,
        *const c_char,
        c_int,
        c_int,
    ) -> c_int,
) -> Result<Value> {
    let self_val = this;
    let ctxt = push_ptr(ctx, self_val)?;
    let chunk = args[0];
    let chunk: Vec<u8> = if chunk.is_nil() {
        vec![]
    } else {
        ctx.str_vec(chunk)?
    };
    let last = args[1] == Value::TRUE;
    let parser = ctx.native::<XmlSaxPushParser>(self_val).unwrap().sax;
    let mut call = SaxCall {
        ctx: ctx.raw(),
        parser,
        error: None,
    };
    // SAFETY: a live context; `call` and `chunk` outlive the call.
    let status = unsafe {
        xml::mrb_xml_ctxt_set_private(ctxt, &mut call as *mut SaxCall as *mut c_void);
        xml::xmlSetStructuredErrorFunc(std::ptr::null_mut(), None);
        let status = parse_chunk(
            ctxt,
            chunk.as_ptr() as *const c_char,
            chunk.len() as c_int,
            last as c_int,
        );
        xml::mrb_xml_ctxt_set_private(ctxt, std::ptr::null_mut());
        status
    };
    if let Some(e) = call.error {
        return Err(raise(ctx, e));
    }
    // SAFETY: a live context.
    if status != 0 && unsafe { xml::mrb_xml_ctxt_get_options(ctxt) } & xml::XML_PARSE_RECOVER == 0 {
        let mut errors: Vec<ErrorRecord> = vec![];
        // SAFETY: the context's last error record (or NULL).
        unsafe {
            let e = xml::xmlCtxtGetLastError(ctxt as *mut c_void);
            collect_error(&mut errors as *mut _ as *mut c_void, e);
        }
        if let Some(e) = errors.first() {
            let ex = syntax_error_value(ctx, e)?;
            return Err(raise(ctx, ex));
        }
        // No error record: the parser was stopped by an earlier
        // callback exception (`XML_ERR_USER_STOP`) and takes no more.
        return Err(ctx.runtime_error(format!("parser is stopped ({status})")));
    }
    Ok(self_val)
}

/// SAX::PushParser#native_write(chunk, last_chunk) -> self
fn push_native_write(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    push_write(ctx, this, args, xml::xmlParseChunk)
}

/// HTML4::SAX::PushParser#native_write(chunk, last_chunk) -> self
fn html_push_native_write(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    push_write(ctx, this, args, xml::htmlParseChunk)
}

/// SAX::PushParser#options -> Integer
fn push_options(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let ctxt = push_ptr(ctx, this)?;
    // SAFETY: a live context.
    Ok(Value::int(
        unsafe { xml::mrb_xml_ctxt_get_options(ctxt) } as i64
    ))
}

/// SAX::PushParser#options=(options) -> nil
fn push_set_options(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let ctxt = push_ptr(ctx, this)?;
    let options = ctx.int(args[0])? as c_int;
    // SAFETY: a live context.
    let error = unsafe { xml::xmlCtxtSetOptions(ctxt, options) };
    if error != 0 {
        return Err(ctx.runtime_error(format!("Cannot set XML parser context options ({error:x})")));
    }
    Ok(Value::nil())
}

/// SAX::PushParser#replace_entities -> bool
fn push_replace_entities(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    Ok(has_option(push_ptr(ctx, this)?, xml::XML_PARSE_NOENT))
}

/// SAX::PushParser#replace_entities=(value)
fn push_set_replace_entities(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let ctxt = push_ptr(ctx, this)?;
    set_option(
        ctx,
        ctxt,
        xml::XML_PARSE_NOENT,
        args[0].truthy(),
        "parser context options",
    )?;
    Ok(args[0])
}
