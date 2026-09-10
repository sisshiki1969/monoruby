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

use super::*;

/// The payload of a `SAX::Parser`: the callback table libxml2 is handed.
struct XmlSaxHandler {
    handler: Box<xml::xmlSAXHandler>,
}

impl NativeData for XmlSaxHandler {
    fn mark(&self, _alloc: &mut alloc::Allocator<RValue>) {}
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

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

impl NativeData for XmlSaxParserContext {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        if let Some(io) = &self.io {
            io.io.mark(alloc);
        }
        self.sax.mark(alloc);
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

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

impl NativeData for XmlSaxPushParser {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.sax.mark(alloc);
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

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
    vm: *mut Executor,
    globals: *mut Globals,
    /// The `SAX::Parser` (its `@document` receives the events).
    parser: Value,
    /// The exception a callback raised: the parser was stopped, and it is
    /// re-raised after the library call.
    error: Option<MonorubyErr>,
}

/// Run `f` for a callback from libxml2: skipped once an exception is
/// pending; an exception it raises is kept and stops the parser.
unsafe fn with_call(ctx: *mut c_void, f: impl FnOnce(&mut Executor, &mut Globals, Value) -> Result<()>) {
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
        let vm = &mut *call.vm;
        let globals = &mut *call.globals;
        let doc = globals
            .store
            .get_ivar(call.parser, IdentId::get_id("@document"))
            .unwrap_or_default();
        if let Err(e) = f(vm, globals, doc) {
            call.error = Some(e);
            xml::xmlStopParser(ctxt);
        }
    }
}

fn call(vm: &mut Executor, globals: &mut Globals, doc: Value, method: &str, args: &[Value]) -> Result<()> {
    vm.invoke_method_inner(globals, IdentId::get_id(method), doc, args, None, None)?;
    Ok(())
}

// ---- the callbacks (`noko_xml_sax_parser_*_callback`) ----

unsafe extern "C" fn cb_start_document(ctx: *mut c_void) {
    // SAFETY: libxml2 passes the context registered as `userData`.
    unsafe {
        xml::xmlSAX2StartDocument(ctx);
        let ctxt = ctx as *mut xml::xmlParserCtxt;
        with_call(ctx, |vm, globals, doc| {
            let standalone = xml::mrb_xml_ctxt_get_standalone(ctxt);
            // -1: no XML declaration.
            if standalone != -1 {
                let encoding = xml_str(xml::mrb_xml_ctxt_get_encoding(ctxt));
                let version = xml_str(xml::mrb_xml_ctxt_get_version(ctxt));
                let standalone = match standalone {
                    0 => Value::string_from_str("no"),
                    1 => Value::string_from_str("yes"),
                    _ => Value::nil(),
                };
                call(vm, globals, doc, "xmldecl", &[version, encoding, standalone])?;
            }
            call(vm, globals, doc, "start_document", &[])
        });
    }
}

unsafe extern "C" fn cb_html_start_document(ctx: *mut c_void) {
    // SAFETY: as `cb_start_document`.
    unsafe {
        xml::xmlSAX2StartDocument(ctx);
        with_call(ctx, |vm, globals, doc| call(vm, globals, doc, "start_document", &[]));
    }
}

unsafe extern "C" fn cb_end_document(ctx: *mut c_void) {
    // SAFETY: as `cb_start_document`.
    unsafe { with_call(ctx, |vm, globals, doc| call(vm, globals, doc, "end_document", &[])) }
}

unsafe extern "C" fn cb_start_element(ctx: *mut c_void, name: *const xml::xmlChar, atts: *mut *const xml::xmlChar) {
    // SAFETY: as `cb_start_document`; `atts` is a NULL-terminated list of
    // name / value pairs.
    unsafe {
        with_call(ctx, |vm, globals, doc| {
            let mut attributes = vec![];
            if !atts.is_null() {
                let mut i = 0;
                while !(*atts.add(i)).is_null() {
                    let attr = xml_str(*atts.add(i));
                    let value = xml_str(*atts.add(i + 1));
                    attributes.push(Value::array_from_vec(vec![attr, value]));
                    i += 2;
                }
            }
            let attributes = Value::array_from_vec(attributes);
            call(vm, globals, doc, "start_element", &[xml_str(name), attributes])
        })
    }
}

unsafe extern "C" fn cb_end_element(ctx: *mut c_void, name: *const xml::xmlChar) {
    // SAFETY: as `cb_start_document`.
    unsafe { with_call(ctx, |vm, globals, doc| call(vm, globals, doc, "end_element", &[xml_str(name)])) }
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
        with_call(ctx, |vm, globals, doc| {
            let attr_class = globals
                .store
                .get_constant_noautoload(classes().sax_parser, IdentId::get_id("Attribute"))
                .ok_or_else(|| MonorubyErr::nameerr("uninitialized constant Nokogiri::XML::SAX::Parser::Attribute"))?;
            let attr_ary = Value::array_from_vec(vec![]);
            // `Attribute.new` runs Ruby: keep the array (and so the
            // attributes made so far) rooted.
            let len = vm.temp_len();
            vm.temp_push(attr_ary);
            let mut fill = || -> Result<()> {
                if !attributes.is_null() {
                    for i in (0..nb_attributes as usize * 5).step_by(5) {
                        let start = *attributes.add(i + 3);
                        let end = *attributes.add(i + 4);
                        let value = std::slice::from_raw_parts(start, end.offset_from(start) as usize);
                        let args = [
                            xml_str(*attributes.add(i)),
                            xml_str(*attributes.add(i + 1)),
                            xml_str(*attributes.add(i + 2)),
                            utf8(value),
                        ];
                        let attr = vm.invoke_method_inner(globals, IdentId::NEW, attr_class, &args, None, None)?;
                        attr_ary.as_array_mut(&globals.store)?.push(attr);
                    }
                }
                Ok(())
            };
            let r = fill();
            vm.temp_clear(len);
            r?;
            let mut ns_list = vec![];
            if !namespaces.is_null() {
                for i in (0..nb_namespaces as usize * 2).step_by(2) {
                    ns_list.push(Value::array_from_vec(vec![
                        xml_str(*namespaces.add(i)),
                        xml_str(*namespaces.add(i + 1)),
                    ]));
                }
            }
            let args = [
                xml_str(localname),
                attr_ary,
                xml_str(prefix),
                xml_str(uri),
                Value::array_from_vec(ns_list),
            ];
            call(vm, globals, doc, "start_element_namespace", &args)
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
        with_call(ctx, |vm, globals, doc| {
            let args = [xml_str(localname), xml_str(prefix), xml_str(uri)];
            call(vm, globals, doc, "end_element_namespace", &args)
        })
    }
}

unsafe extern "C" fn cb_characters(ctx: *mut c_void, ch: *const xml::xmlChar, len: c_int) {
    // SAFETY: as `cb_start_document`; `ch` has `len` bytes.
    unsafe {
        with_call(ctx, |vm, globals, doc| {
            let s = utf8(std::slice::from_raw_parts(ch, len as usize));
            call(vm, globals, doc, "characters", &[s])
        })
    }
}

unsafe extern "C" fn cb_comment(ctx: *mut c_void, value: *const xml::xmlChar) {
    // SAFETY: as `cb_start_document`.
    unsafe { with_call(ctx, |vm, globals, doc| call(vm, globals, doc, "comment", &[xml_str(value)])) }
}

unsafe extern "C" fn cb_cdata_block(ctx: *mut c_void, value: *const xml::xmlChar, len: c_int) {
    // SAFETY: as `cb_characters`.
    unsafe {
        with_call(ctx, |vm, globals, doc| {
            let s = utf8(std::slice::from_raw_parts(value, len as usize));
            call(vm, globals, doc, "cdata_block", &[s])
        })
    }
}

unsafe extern "C" fn cb_processing_instruction(ctx: *mut c_void, name: *const xml::xmlChar, content: *const xml::xmlChar) {
    // SAFETY: as `cb_start_document`.
    unsafe {
        with_call(ctx, |vm, globals, doc| {
            call(vm, globals, doc, "processing_instruction", &[xml_str(name), xml_str(content)])
        })
    }
}

unsafe extern "C" fn cb_reference(ctx: *mut c_void, name: *const xml::xmlChar) {
    // SAFETY: as `cb_start_document`; the entity is the document's.
    unsafe {
        let entity = xml::xmlSAX2GetEntity(ctx, name);
        with_call(ctx, |vm, globals, doc| {
            let args = if !entity.is_null() && !(*entity).content.is_null() {
                [xml_str((*entity).name), xml_str((*entity).content)]
            } else {
                [xml_str(name), Value::nil()]
            };
            call(vm, globals, doc, "reference", &args)
        })
    }
}

/// The formatted `warning` / `error` text from the glue's variadic
/// callbacks.
unsafe extern "C" fn cb_message(ctx: *mut c_void, is_error: c_int, text: *const c_char) {
    // SAFETY: as `cb_start_document`; `text` is NUL-terminated.
    unsafe {
        let msg = utf8(CStr::from_ptr(text).to_bytes());
        with_call(ctx, |vm, globals, doc| {
            call(vm, globals, doc, if is_error != 0 { "error" } else { "warning" }, &[msg])
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

pub(super) fn init(globals: &mut Globals, c: &Classes) {
    // SAFETY: registering the (single, static) message sink.
    unsafe { xml::mrb_xml_sax_set_message_handler(Some(cb_message)) };

    for parser in [c.sax_parser, c.html4_sax_parser] {
        globals.store[parser].set_alloc_func(sax_parser_alloc);
    }
    globals.define_private_builtin_func(c.sax_parser, "initialize_native", initialize_native, 0);
    globals.define_private_builtin_func(c.html4_sax_parser, "initialize_native", html_initialize_native, 0);

    let x = c.sax_parser_context;
    globals.define_builtin_class_func(x, "native_io", native_io, 2);
    globals.define_builtin_class_func(x, "native_memory", native_memory, 2);
    globals.define_builtin_class_func(x, "native_file", native_file, 2);
    globals.define_builtin_func(x, "parse_with", parse_with, 1);
    globals.define_builtin_func(x, "replace_entities=", ctx_set_replace_entities, 1);
    globals.define_builtin_func(x, "replace_entities", ctx_replace_entities, 0);
    globals.define_builtin_func(x, "recovery=", ctx_set_recovery, 1);
    globals.define_builtin_func(x, "recovery", ctx_recovery, 0);
    globals.define_builtin_func(x, "line", ctx_line, 0);
    globals.define_builtin_func(x, "column", ctx_column, 0);
    let h = c.html4_sax_parser_context;
    globals.define_builtin_class_func(h, "native_memory", html_native_memory, 2);
    globals.define_builtin_class_func(h, "native_file", html_native_file, 2);
    globals.define_builtin_func(h, "parse_with", html_parse_with, 1);

    for push in [c.sax_push_parser, c.html4_sax_push_parser] {
        globals.store[push].set_alloc_func(push_parser_alloc);
    }
    let p = c.sax_push_parser;
    globals.define_builtin_func(p, "options", push_options, 0);
    globals.define_builtin_func(p, "options=", push_set_options, 1);
    globals.define_builtin_func(p, "replace_entities", push_replace_entities, 0);
    globals.define_builtin_func(p, "replace_entities=", push_set_replace_entities, 1);
    globals.define_private_builtin_func(p, "initialize_native", push_initialize_native, 2);
    globals.define_private_builtin_func(p, "native_write", push_native_write, 2);
    let hp = c.html4_sax_push_parser;
    globals.define_private_builtin_func(hp, "initialize_native", html_push_initialize_native, 3);
    globals.define_private_builtin_func(hp, "native_write", html_push_native_write, 2);
}

// ---- SAX::Parser ----

extern "C" fn sax_parser_alloc(class_id: ClassId, _globals: &mut Globals) -> Value {
    Value::new_native(
        class_id,
        Box::new(XmlSaxHandler {
            handler: Box::new(xml::xmlSAXHandler::zeroed()),
        }),
    )
}

/// The handler table of a `SAX::Parser`.
fn handler_ptr(v: Value) -> Result<*mut xml::xmlSAXHandler> {
    match v.try_native::<XmlSaxHandler>() {
        Some(h) => Ok(&*h.handler as *const xml::xmlSAXHandler as *mut xml::xmlSAXHandler),
        None => Err(MonorubyErr::argumenterr("argument must be a Nokogiri::XML::SAX::Parser")),
    }
}

/// SAX::Parser#initialize_native -> self
#[monoruby_builtin]
fn initialize_native(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let h = handler_ptr(lfp.self_val())?;
    // SAFETY: the table is owned by `self`.
    unsafe { fill_handler(&mut *h) };
    Ok(lfp.self_val())
}

/// HTML4::SAX::Parser#initialize_native -> self: the XML table with the
/// HTML `start_document` (no XML declaration).
#[monoruby_builtin]
fn html_initialize_native(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let h = handler_ptr(lfp.self_val())?;
    // SAFETY: as `initialize_native`.
    unsafe {
        fill_handler(&mut *h);
        (*h).startDocument = Some(cb_html_start_document);
    }
    Ok(lfp.self_val())
}

// ---- SAX::ParserContext ----

fn context_ptr(v: Value) -> Result<*mut xml::xmlParserCtxt> {
    match v.try_native::<XmlSaxParserContext>() {
        Some(c) => Ok(c.ctxt),
        None => Err(MonorubyErr::argumenterr("expected a Nokogiri::XML::SAX::ParserContext")),
    }
}

/// `nil` or an `Encoding`, else TypeError (nokogiri's check).
fn check_encoding(globals: &Globals, enc: Value) -> Result<()> {
    if enc.is_nil() {
        return Ok(());
    }
    let encoding_class = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::ENCODING)
        .map(|v| v.as_class_id());
    if encoding_class.is_some_and(|c| enc.is_kind_of(&globals.store, c)) {
        return Ok(());
    }
    Err(MonorubyErr::typeerr("argument must be an Encoding object"))
}

/// `noko_xml_sax_parser_context_set_encoding`: switch the context to the
/// `Encoding`'s name; on failure the context is freed and the libxml2
/// errors raised.
fn set_encoding(vm: &mut Executor, globals: &mut Globals, ctxt: *mut xml::xmlParserCtxt, enc: Value) -> Result<()> {
    if enc.is_nil() {
        return Ok(());
    }
    let name = vm.invoke_method_inner(globals, IdentId::get_id("name"), enc, &[], None, None)?;
    let name = cstr(name, &globals.store)?;
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
        let rb_errors = errors_to_array(vm, globals, &errors)?;
        let klass = globals.store.get_module(classes().xml_syntax_error).as_val();
        let ex = vm.invoke_method_inner(globals, IdentId::get_id("aggregate"), klass, &[rb_errors], None, None)?;
        return Err(if ex.is_nil() { MonorubyErr::runtimeerr("could not set encoding") } else { raise(ex) });
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
    globals: &mut Globals,
    class: ClassId,
    ctxt: *mut xml::xmlParserCtxt,
    io: Option<Box<IoCtx>>,
    buffer: Vec<u8>,
    input: Value,
) -> Result<Value> {
    let rb = Value::new_native(
        class,
        Box::new(XmlSaxParserContext {
            ctxt,
            io,
            _buffer: buffer,
            sax: Value::nil(),
        }),
    );
    if !input.is_nil() {
        globals.store.set_ivar(rb, IdentId::get_id("@input"), input)?;
    }
    Ok(rb)
}

/// SAX::ParserContext.native_io(io, encoding) -> ParserContext
#[monoruby_builtin]
fn native_io(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let io = lfp.arg(0);
    let enc = lfp.arg(1);
    if globals.store.check_method_for_class(io.class(), IdentId::get_id("read")).is_none() {
        return Err(MonorubyErr::typeerr("argument expected to respond to :read"));
    }
    check_encoding(globals, enc)?;
    let mut ioctx = Box::new(IoCtx::new(vm, globals, io));
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
        return Err(MonorubyErr::runtimeerr("failed to create xml sax parser context"));
    }
    set_encoding(vm, globals, ctxt, enc)?;
    // SAFETY: a live context.
    unsafe { drop_default_sax(ctxt) };
    wrap_context(globals, class, ctxt, Some(ioctx), vec![], io)
}

/// SAX::ParserContext.native_file(path, encoding) -> ParserContext
#[monoruby_builtin]
fn native_file(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let enc = lfp.arg(1);
    check_encoding(globals, enc)?;
    let path = cstr(lfp.arg(0), &globals.store)?;
    // SAFETY: a NUL-terminated path.
    let ctxt = unsafe { xml::xmlCreateFileParserCtxt(path.as_ptr()) };
    if ctxt.is_null() {
        return Err(MonorubyErr::runtimeerr("failed to create xml sax parser context"));
    }
    set_encoding(vm, globals, ctxt, enc)?;
    // SAFETY: a live context.
    unsafe { drop_default_sax(ctxt) };
    wrap_context(globals, class, ctxt, None, vec![], Value::nil())
}

/// The bytes of a `native_memory` input (`Check_Type(T_STRING)`, then
/// "input string cannot be empty").
fn memory_input(globals: &Globals, input: Value) -> Result<Vec<u8>> {
    let Some(bytes) = input.try_bytes() else {
        return Err(MonorubyErr::typeerr(format!(
            "wrong argument type {} (expected String)",
            builtin_type_name(globals, input)
        )));
    };
    if bytes.is_empty() {
        return Err(MonorubyErr::runtimeerr("input string cannot be empty"));
    }
    Ok(bytes.to_vec())
}

/// SAX::ParserContext.native_memory(string, encoding) -> ParserContext
#[monoruby_builtin]
fn native_memory(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let buffer = memory_input(globals, lfp.arg(0))?;
    let enc = lfp.arg(1);
    check_encoding(globals, enc)?;
    // SAFETY: the buffer moves into the payload, which outlives the
    // context's reads of it.
    let ctxt = unsafe { xml::xmlCreateMemoryParserCtxt(buffer.as_ptr() as *const c_char, buffer.len() as c_int) };
    if ctxt.is_null() {
        return Err(MonorubyErr::runtimeerr("failed to create xml sax parser context"));
    }
    set_encoding(vm, globals, ctxt, enc)?;
    // SAFETY: a live context.
    unsafe { drop_default_sax(ctxt) };
    wrap_context(globals, class, ctxt, None, buffer, lfp.arg(0))
}

/// HTML4::SAX::ParserContext.native_memory(string, encoding) -> ParserContext
#[monoruby_builtin]
fn html_native_memory(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let buffer = memory_input(globals, lfp.arg(0))?;
    let enc = lfp.arg(1);
    check_encoding(globals, enc)?;
    // SAFETY: as `native_memory`, with the HTML parser.
    let ctxt = unsafe { xml::htmlCreateMemoryParserCtxt(buffer.as_ptr() as *const c_char, buffer.len() as c_int) };
    if ctxt.is_null() {
        return Err(MonorubyErr::runtimeerr("failed to create xml sax parser context"));
    }
    set_encoding(vm, globals, ctxt, enc)?;
    // SAFETY: a live context.
    unsafe { drop_default_sax(ctxt) };
    wrap_context(globals, class, ctxt, None, buffer, Value::nil())
}

/// HTML4::SAX::ParserContext.native_file(path, encoding) -> ParserContext
#[monoruby_builtin]
fn html_native_file(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let enc = lfp.arg(1);
    check_encoding(globals, enc)?;
    let path = cstr(lfp.arg(0), &globals.store)?;
    // SAFETY: a NUL-terminated path.
    let ctxt = unsafe { xml::htmlCreateFileParserCtxt(path.as_ptr(), std::ptr::null()) };
    if ctxt.is_null() {
        return Err(MonorubyErr::runtimeerr("failed to create xml sax parser context"));
    }
    set_encoding(vm, globals, ctxt, enc)?;
    // SAFETY: a live context.
    unsafe { drop_default_sax(ctxt) };
    wrap_context(globals, class, ctxt, None, vec![], Value::nil())
}

/// Give the context the parser's handler and run `parse` under a
/// `SaxCall`; a pending exception is re-raised afterwards.
fn parse_context(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    parse: unsafe extern "C" fn(*mut xml::xmlParserCtxt) -> c_int,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let ctxt = context_ptr(self_val)?;
    let parser = lfp.arg(0);
    if !parser.is_kind_of(&globals.store, classes().sax_parser) {
        return Err(MonorubyErr::argumenterr("argument must be a Nokogiri::XML::SAX::Parser"));
    }
    let sax = handler_ptr(parser)?;
    // SAFETY: the payload is ours for the call; the IO callbacks may run
    // Ruby, with the pointers refreshed here.
    unsafe {
        let ctx = native_mut::<XmlSaxParserContext>(self_val).unwrap();
        ctx.sax = parser;
        if let Some(io) = &mut ctx.io {
            io.vm = vm;
            io.globals = globals;
        }
    }
    let mut call = SaxCall {
        vm,
        globals,
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
        return Err(e);
    }
    Ok(Value::nil())
}

/// SAX::ParserContext#parse_with(parser) -> nil
#[monoruby_builtin]
fn parse_with(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    parse_context(vm, globals, lfp, xml::xmlParseDocument)
}

/// HTML4::SAX::ParserContext#parse_with(parser) -> nil
#[monoruby_builtin]
fn html_parse_with(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    parse_context(vm, globals, lfp, xml::htmlParseDocument)
}

/// Set or clear a parse option on a context.
fn set_option(ctxt: *mut xml::xmlParserCtxt, option: c_int, on: bool, what: &str) -> Result<()> {
    // SAFETY: a live context.
    let error = unsafe {
        let options = xml::mrb_xml_ctxt_get_options(ctxt);
        let options = if on { options | option } else { options & !option };
        xml::xmlCtxtSetOptions(ctxt, options)
    };
    if error != 0 {
        return Err(MonorubyErr::runtimeerr(format!("failed to set {what} ({error:x})")));
    }
    Ok(())
}

fn has_option(ctxt: *mut xml::xmlParserCtxt, option: c_int) -> Value {
    // SAFETY: a live context.
    Value::bool(unsafe { xml::mrb_xml_ctxt_get_options(ctxt) } & option != 0)
}

/// SAX::ParserContext#replace_entities=(value)
#[monoruby_builtin]
fn ctx_set_replace_entities(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ctxt = context_ptr(lfp.self_val())?;
    set_option(ctxt, xml::XML_PARSE_NOENT, lfp.arg(0).as_bool(), "parser context options")?;
    Ok(lfp.arg(0))
}

/// SAX::ParserContext#replace_entities -> bool
#[monoruby_builtin]
fn ctx_replace_entities(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(has_option(context_ptr(lfp.self_val())?, xml::XML_PARSE_NOENT))
}

/// SAX::ParserContext#recovery=(value)
#[monoruby_builtin]
fn ctx_set_recovery(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ctxt = context_ptr(lfp.self_val())?;
    set_option(ctxt, xml::XML_PARSE_RECOVER, lfp.arg(0).as_bool(), "parser context options")?;
    Ok(lfp.arg(0))
}

/// SAX::ParserContext#recovery -> bool
#[monoruby_builtin]
fn ctx_recovery(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(has_option(context_ptr(lfp.self_val())?, xml::XML_PARSE_RECOVER))
}

/// SAX::ParserContext#line -> Integer | nil
#[monoruby_builtin]
fn ctx_line(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ctxt = context_ptr(lfp.self_val())?;
    // SAFETY: a live context.
    let line = unsafe { xml::mrb_xml_ctxt_get_line(ctxt) };
    Ok(if line < 0 { Value::nil() } else { Value::integer(line as i64) })
}

/// SAX::ParserContext#column -> Integer | nil
#[monoruby_builtin]
fn ctx_column(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ctxt = context_ptr(lfp.self_val())?;
    // SAFETY: a live context.
    let col = unsafe { xml::mrb_xml_ctxt_get_column(ctxt) };
    Ok(if col < 0 { Value::nil() } else { Value::integer(col as i64) })
}

// ---- SAX::PushParser ----

extern "C" fn push_parser_alloc(class_id: ClassId, _globals: &mut Globals) -> Value {
    Value::new_native(
        class_id,
        Box::new(XmlSaxPushParser {
            ctxt: std::ptr::null_mut(),
            sax: Value::nil(),
        }),
    )
}

fn push_ptr(v: Value) -> Result<*mut xml::xmlParserCtxt> {
    match v.try_native::<XmlSaxPushParser>() {
        Some(p) if !p.ctxt.is_null() => Ok(p.ctxt),
        Some(_) => Err(MonorubyErr::runtimeerr("push parser is not initialized")),
        None => Err(MonorubyErr::argumenterr("expected a Nokogiri::XML::SAX::PushParser")),
    }
}

/// Install a freshly created push context into `self`.
fn install_push_ctxt(self_val: Value, ctxt: *mut xml::xmlParserCtxt, sax: Value) -> Result<Value> {
    if ctxt.is_null() {
        return Err(MonorubyErr::runtimeerr("Could not create a parser context"));
    }
    // SAFETY: the payload is ours; the context is live.
    unsafe {
        xml::mrb_xml_ctxt_set_user_data(ctxt, ctxt as *mut c_void);
        let p = native_mut::<XmlSaxPushParser>(self_val)
            .ok_or_else(|| MonorubyErr::argumenterr("expected a Nokogiri::XML::SAX::PushParser"))?;
        free_parser_ctxt(p.ctxt);
        p.ctxt = ctxt;
        p.sax = sax;
    }
    Ok(self_val)
}

/// SAX::PushParser#initialize_native(sax, filename) -> self
#[monoruby_builtin]
fn push_initialize_native(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let sax = lfp.arg(0);
    let handler = handler_ptr(sax)?;
    let filename = opt_cstr(lfp.arg(1), &globals.store)?;
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
    install_push_ctxt(lfp.self_val(), ctxt, sax)
}

/// HTML4::SAX::PushParser#initialize_native(sax, filename, encoding) -> self
#[monoruby_builtin]
fn html_push_initialize_native(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let sax = lfp.arg(0);
    let handler = handler_ptr(sax)?;
    let filename = opt_cstr(lfp.arg(1), &globals.store)?;
    let mut enc = xml::XML_CHAR_ENCODING_NONE;
    if !lfp.arg(2).is_nil() {
        let name = cstr(lfp.arg(2), &globals.store)?;
        // SAFETY: a NUL-terminated name.
        enc = unsafe { xml::xmlParseCharEncoding(name.as_ptr()) };
        if enc == xml::XML_CHAR_ENCODING_ERROR {
            return Err(MonorubyErr::argumenterr("Unsupported Encoding"));
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
    install_push_ctxt(lfp.self_val(), ctxt, sax)
}

/// Feed a chunk (`native_write`): a callback's exception is re-raised;
/// otherwise a parse failure without `RECOVER` raises the context's last
/// error.
fn push_write(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    parse_chunk: unsafe extern "C" fn(*mut xml::xmlParserCtxt, *const c_char, c_int, c_int) -> c_int,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let ctxt = push_ptr(self_val)?;
    let chunk = lfp.arg(0);
    let chunk: Vec<u8> = if chunk.is_nil() { vec![] } else { chunk.expect_bytes(&globals.store)?.to_vec() };
    let last = lfp.arg(1).id() == TRUE_VALUE;
    let parser = self_val.try_native::<XmlSaxPushParser>().unwrap().sax;
    let mut call = SaxCall {
        vm,
        globals,
        parser,
        error: None,
    };
    // SAFETY: a live context; `call` and `chunk` outlive the call.
    let status = unsafe {
        xml::mrb_xml_ctxt_set_private(ctxt, &mut call as *mut SaxCall as *mut c_void);
        xml::xmlSetStructuredErrorFunc(std::ptr::null_mut(), None);
        let status = parse_chunk(ctxt, chunk.as_ptr() as *const c_char, chunk.len() as c_int, last as c_int);
        xml::mrb_xml_ctxt_set_private(ctxt, std::ptr::null_mut());
        status
    };
    if let Some(e) = call.error {
        return Err(e);
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
            return Err(raise(syntax_error_value(vm, globals, e)?));
        }
        // No error record: the parser was stopped by an earlier
        // callback exception (`XML_ERR_USER_STOP`) and takes no more.
        return Err(MonorubyErr::runtimeerr(format!("parser is stopped ({status})")));
    }
    Ok(self_val)
}

/// SAX::PushParser#native_write(chunk, last_chunk) -> self
#[monoruby_builtin]
fn push_native_write(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    push_write(vm, globals, lfp, xml::xmlParseChunk)
}

/// HTML4::SAX::PushParser#native_write(chunk, last_chunk) -> self
#[monoruby_builtin]
fn html_push_native_write(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    push_write(vm, globals, lfp, xml::htmlParseChunk)
}

/// SAX::PushParser#options -> Integer
#[monoruby_builtin]
fn push_options(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ctxt = push_ptr(lfp.self_val())?;
    // SAFETY: a live context.
    Ok(Value::integer(unsafe { xml::mrb_xml_ctxt_get_options(ctxt) } as i64))
}

/// SAX::PushParser#options=(options) -> nil
#[monoruby_builtin]
fn push_set_options(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ctxt = push_ptr(lfp.self_val())?;
    let options = lfp.arg(0).expect_integer(&globals.store)? as c_int;
    // SAFETY: a live context.
    let error = unsafe { xml::xmlCtxtSetOptions(ctxt, options) };
    if error != 0 {
        return Err(MonorubyErr::runtimeerr(format!("Cannot set XML parser context options ({error:x})")));
    }
    Ok(Value::nil())
}

/// SAX::PushParser#replace_entities -> bool
#[monoruby_builtin]
fn push_replace_entities(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(has_option(push_ptr(lfp.self_val())?, xml::XML_PARSE_NOENT))
}

/// SAX::PushParser#replace_entities=(value)
#[monoruby_builtin]
fn push_set_replace_entities(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ctxt = push_ptr(lfp.self_val())?;
    set_option(ctxt, xml::XML_PARSE_NOENT, lfp.arg(0).as_bool(), "parser context options")?;
    Ok(lfp.arg(0))
}
