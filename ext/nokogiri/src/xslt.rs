//! `Nokogiri::XSLT` over the bundled libxslt (`xslt_stylesheet.c`):
//! `Stylesheet.parse_stylesheet_doc` / `#transform` / `#serialize`, and
//! `XSLT.register`: extension functions in a namespace answered by the
//! instance methods of a Ruby class (one instance per transform).

use super::xpath::{XPATH_INVALID_TYPE, marshal_funcall};
use crate::*;
use std::cell::Cell;

/// The payload of a `Stylesheet`: the compiled stylesheet, and the
/// handler instances of the running transform (`func_instances`), which
/// libxslt holds only as raw module data.
pub(crate) struct XsltStylesheet {
    ss: *mut xml::xsltStylesheet,
    func_instances: Vec<Value>,
}

native!(XsltStylesheet, "XsltStylesheet", |this, m| {
    for v in &this.func_instances {
        m.mark(v);
    }
});

impl Drop for XsltStylesheet {
    fn drop(&mut self) {
        // SAFETY: the stylesheet (and the document copy it parsed) is ours.
        unsafe { xml::xsltFreeStylesheet(self.ss) }
    }
}

pub(crate) fn init(ctx: &mut Ctx, c: &Classes) {
    ctx.define_method(
        c.stylesheet,
        "parse_stylesheet_doc",
        method!(parse_stylesheet_doc),
        1,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(c.stylesheet, "serialize", method!(serialize), 1, 0);
    ctx.define_method(c.stylesheet, "transform", method!(transform), MR_ARGC_VARIADIC, 0) /* arity 1..2 */;
    ctx.define_method(
        c.xslt,
        "register",
        method!(register),
        2,
        MR_METHOD_SINGLETON,
    );
    // `@modules`: the registered handler classes by namespace URI.
    let xslt = c.xslt;
    let modules = ctx.hash_new();
    ctx.ivar_set(xslt, "@modules", modules)
        .expect("Nokogiri::XSLT is a module");
    // SAFETY: plain library initialization (`Init_nokogiri`); libxslt's own
    // extras are registered by its lazy `xsltInit`.
    unsafe { xml::exsltRegisterAll() };
}

fn recv(ctx: &mut Ctx, this: Value) -> Result<*mut xml::xsltStylesheet> {
    match ctx.native::<XsltStylesheet>(this) {
        Some(s) => Ok(s.ss),
        None => Err(ctx.argument_error("expected a Nokogiri::XSLT::Stylesheet")),
    }
}

/// `noko_xml_document_unwrap`: a Document, nothing else. The error names
/// the argument as `rb_check_typeddata` does: a wrapped node by its
/// struct name, anything else by its class.
fn document_ptr(ctx: &mut Ctx, v: Value) -> Result<*mut xml::xmlDoc> {
    ctx.native::<XmlDocument>(v).map(|d| d.doc).ok_or_else(|| {
        let name = if ctx.native::<XmlNode>(v).is_some() {
            "xmlNode".to_string()
        } else {
            builtin_type_name(ctx, v)
        };
        ctx.type_error(format!("wrong argument type {name} (expected xmlDoc)"))
    })
}

/// Run `f` with libxslt's generic errors (and libxml2's too when
/// `with_xml`) collected: its result and the text reported.
unsafe fn capture_errors<T>(with_xml: bool, f: impl FnOnce() -> T) -> (T, String) {
    // SAFETY: the capture is begun and ended around the call; the text is
    // ours to free.
    unsafe {
        let handle = xml::mrb_xslt_error_capture_begin(with_xml as c_int);
        let r = f();
        let mut len = 0usize;
        let text = xml::mrb_xslt_error_capture_end(handle, &mut len);
        let msg = if text.is_null() {
            String::new()
        } else {
            let s = String::from_utf8_lossy(std::slice::from_raw_parts(text as *const u8, len))
                .into_owned();
            xml::mrb_xslt_error_capture_free(text);
            s
        };
        (r, msg)
    }
}

/// Stylesheet.parse_stylesheet_doc(document) -> Stylesheet
///
/// The stylesheet is compiled from a copy of the document (libxslt takes
/// the copy over); a failure raises the errors reported as a RuntimeError.
fn parse_stylesheet_doc(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let doc = document_ptr(ctx, args[0])?;
    // SAFETY: a live document; the copy belongs to the stylesheet on
    // success and is ours to free on failure.
    let (ss, msg) = unsafe {
        let copy = xml::xmlCopyDoc(doc, 1);
        let (ss, msg) = capture_errors(false, || xml::xsltParseStylesheetDoc(copy));
        if ss.is_null() {
            xml::xmlFreeDoc(copy);
        }
        (ss, msg)
    };
    if ss.is_null() {
        return Err(ctx.runtime_error(msg));
    }
    let rb = ctx.native_new(
        class,
        XsltStylesheet {
            ss,
            func_instances: vec![],
        },
    )?;
    // SAFETY: a live stylesheet we own; the extension callbacks find the
    // Ruby object through it.
    unsafe { xml::mrb_xslt_stylesheet_set_private(ss, rb.0 as *mut c_void) };
    Ok(rb)
}

/// Stylesheet#serialize(document) -> String: the document as the
/// stylesheet's output method says.
fn serialize(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let doc = document_ptr(ctx, args[0])?;
    let ss = recv(ctx, this)?;
    let mut ptr: *mut xml::xmlChar = std::ptr::null_mut();
    let mut len: c_int = 0;
    // SAFETY: a live document and stylesheet; the buffer is ours to free.
    unsafe {
        xml::xsltSaveResultToString(&mut ptr, &mut len, doc, ss);
        if ptr.is_null() {
            return Ok(utf8(ctx, &[]));
        }
        let s = utf8(
            ctx,
            std::slice::from_raw_parts(ptr as *const u8, len.max(0) as usize),
        );
        xml::xml_free()(ptr as *mut c_void);
        Ok(s)
    }
}

/// The state of a running `transform`, reachable from the extension
/// callbacks libxslt makes during it.
struct XsltCall {
    ctx: *mut MrContext,
    /// The document being transformed (the node sets an extension
    /// function receives belong to it, unless they are libxslt's own).
    document: Value,
    /// The exception a handler raised (or a bad return type): the
    /// evaluation is aborted and it is re-raised by `transform`.
    error: Option<Value>,
}

thread_local! {
    static XSLT_CALL: Cell<*mut XsltCall> = const { Cell::new(std::ptr::null_mut()) };
}

/// Stylesheet#transform(document, params = []) -> XML::Document
fn transform(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    check_arity(ctx, args, 1, 2)?;
    let rb_document = args[0];
    let mut rb_param = args.get(1).copied().unwrap_or_default();
    if rb_param.is_nil() {
        rb_param = ctx.ary_from_vec(vec![]);
    }
    if ctx.native::<XmlDocument>(rb_document).is_none() {
        return Err(ctx.argument_error("argument must be a Nokogiri::XML::Document"));
    }
    // A Hash of params is flattened to the name / value list.
    if try_hash(ctx, rb_param).is_some() {
        rb_param = ctx.funcall(rb_param, "to_a", &[], None)?;
        rb_param = ctx.funcall(rb_param, "flatten", &[], None)?;
    }
    if ctx.type_of(rb_param) != MrType::Array {
        return Err(ctx.type_error(format!(
            "wrong argument type {} (expected Array)",
            builtin_type_name(ctx, rb_param)
        )));
    }
    let entries: Vec<Value> = ary_vec(ctx, rb_param)?.iter().cloned().collect();
    let mut params: Vec<CString> = Vec::with_capacity(entries.len());
    for e in entries {
        params.push(cstr(e, ctx)?);
    }
    let mut c_params: Vec<*const c_char> = params.iter().map(|p| p.as_ptr()).collect();
    c_params.push(std::ptr::null());
    let ss = recv(ctx, this)?;
    let c_document = doc_ptr(ctx, rb_document)?;
    let mut call = XsltCall {
        ctx: ctx.raw(),
        document: rb_document,
        error: None,
    };
    // SAFETY: a live stylesheet and document; the transform context is
    // ours for the check, the copy (if any) for the call; the call state
    // is registered for this transform only; the result is ours.
    let (result, msg) = unsafe {
        // libxslt strips the blank text nodes of the source when the
        // stylesheet asks for it: not from under their Ruby objects
        // (nokogiri #2800), so such a document is transformed as a copy.
        let tc = xml::xsltNewTransformContext(ss, c_document);
        let defensive = !tc.is_null()
            && xml::xsltNeedElemSpaceHandling(tc) != 0
            && super::schema::has_wrapped_blank_nodes(ctx, c_document);
        if !tc.is_null() {
            xml::xsltFreeTransformContext(tc);
        }
        let c_document = if defensive {
            xml::xmlCopyDoc(c_document, 1)
        } else {
            c_document
        };
        let prev = XSLT_CALL.with(|c| c.replace(&mut call as *mut XsltCall));
        let (result, msg) = capture_errors(true, || {
            xml::xsltApplyStylesheet(ss, c_document, c_params.as_ptr())
        });
        XSLT_CALL.with(|c| c.set(prev));
        if defensive {
            xml::xmlFreeDoc(c_document);
        }
        (result, msg)
    };
    drop(params);
    let discard = |result: *mut xml::xmlDoc| {
        if !result.is_null() {
            // SAFETY: a result nobody else holds.
            unsafe { xml::xmlFreeDoc(result) };
        }
    };
    if let Some(e) = call.error {
        discard(result);
        return Err(raise(ctx, e));
    }
    if !msg.is_empty() {
        discard(result);
        return Err(ctx.runtime_error(msg));
    }
    if result.is_null() {
        return Err(ctx.runtime_error("could not apply the stylesheet"));
    }
    wrap_document(ctx, classes().document, result, &[])
}

/// XSLT.register(uri, klass) -> XSLT: the instance methods of `klass`
/// become the XSLT extension functions of the namespace `uri`
/// (globally, as nokogiri's).
fn register(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let this = this;
    let uri = args[0];
    let obj = args[1];
    let modules = Some(ctx.ivar_get(this, "@modules")).unwrap_or_default();
    if modules.is_nil() {
        return Err(ctx.runtime_error("internal error: @modules not set"));
    }
    ctx.funcall(modules, "[]=", &[uri, obj], None)?;
    let c_uri = cstr(uri, ctx)?;
    // SAFETY: libxslt copies the URI; the callbacks are static.
    unsafe {
        xml::xsltRegisterExtModule(
            c_uri.as_ptr() as *const xml::xmlChar,
            Some(ext_init),
            Some(ext_shutdown),
        )
    };
    Ok(this)
}

/// The Ruby `Stylesheet` of a transform's stylesheet, if it has one.
unsafe fn stylesheet_of<'a>(
    ctx: &Ctx,
    ctxt: *mut xml::xsltTransformContext,
) -> Option<&'a mut XsltStylesheet> {
    // SAFETY: a live transform context; `_private` holds the Value bits
    // set by `parse_stylesheet_doc` (or NULL).
    unsafe {
        let p = xml::mrb_xslt_stylesheet_get_private(xml::mrb_xslt_transform_ctxt_style(ctxt));
        if p.is_null() {
            return None;
        }
        ctx.native::<XsltStylesheet>(Value(p as u64))
    }
}

/// `initFunc`: a transform reaches a registered namespace. Register a
/// function per instance method of its handler class, make the instance
/// (kept by the stylesheet for the transform) and hand it to libxslt as
/// the module's data.
unsafe extern "C" fn ext_init(
    ctxt: *mut xml::xsltTransformContext,
    uri: *const xml::xmlChar,
) -> *mut c_void {
    // SAFETY: libxslt calls this inside `transform`, whose call state is
    // registered; the context and the URI are live.
    unsafe {
        let call_p = XSLT_CALL.with(|c| c.get());
        if call_p.is_null() {
            return std::ptr::null_mut();
        }
        let call = &mut *call_p;
        if call.error.is_some() {
            return std::ptr::null_mut();
        }
        let mut cx = Ctx::from_raw(call.ctx);
        let ctx = &mut cx;
        let r = (|| -> Result<*mut c_void> {
            let xslt = classes().xslt;
            let modules = ctx.ivar_get(xslt, "@modules");
            let key = xml_str(ctx, uri);
            let obj = ctx.funcall(modules, "[]", &[key], None)?;
            let methods = ctx.funcall(obj, "instance_methods", &[Value::bool(false)], None)?;
            let methods: Vec<Value> = ary_vec(ctx, methods)?.iter().cloned().collect();
            for m in methods {
                let name = ctx.funcall(m, "to_s", &[], None)?;
                let name = cstr(name, ctx)?;
                xml::xsltRegisterExtFunction(
                    ctxt,
                    name.as_ptr() as *const xml::xmlChar,
                    uri,
                    Some(ext_call),
                );
            }
            let inst = ctx.funcall(obj, "new", &[], None)?;
            if let Some(w) = stylesheet_of(ctx, ctxt) {
                w.func_instances.push(inst);
            }
            Ok(inst.0 as *mut c_void)
        })();
        match r {
            Ok(p) => p,
            Err(_) => {
                call.error = Some(stash_error(ctx));
                std::ptr::null_mut()
            }
        }
    }
}

/// `shutdownFunc`: the transform is over, the handler instances go.
unsafe extern "C" fn ext_shutdown(
    ctxt: *mut xml::xsltTransformContext,
    _uri: *const xml::xmlChar,
    _data: *mut c_void,
) {
    // SAFETY: a live transform context, inside `transform`, whose call
    // state is registered.
    unsafe {
        let call_p = XSLT_CALL.with(|c| c.get());
        if call_p.is_null() {
            return;
        }
        let ctx = Ctx::from_raw((*call_p).ctx);
        if let Some(w) = stylesheet_of(&ctx, ctxt) {
            w.func_instances.clear();
        }
    }
}

/// `method_caller`: an extension function — call the method of that
/// name on the module's handler instance.
unsafe extern "C" fn ext_call(ctxt: *mut xml::xmlXPathParserContext, nargs: c_int) {
    // SAFETY: libxslt calls this with its live parser context, inside
    // `transform`, whose call state is registered.
    unsafe {
        let call_p = XSLT_CALL.with(|c| c.get());
        if call_p.is_null() {
            xml::xmlXPathErr(ctxt, XPATH_INVALID_TYPE);
            return;
        }
        let call = &mut *call_p;
        if call.error.is_some() {
            xml::xmlXPathErr(ctxt, XPATH_INVALID_TYPE);
            return;
        }
        let xctx = (*ctxt).context;
        let transform = xml::xsltXPathGetTransformContext(ctxt);
        let handler = xml::xsltGetExtData(transform, xml::mrb_xpath_ctx_get_function_uri(xctx));
        if handler.is_null() {
            xml::xmlXPathErr(ctxt, XPATH_INVALID_TYPE);
            return;
        }
        let handler = Value(handler as u64);
        let name = CStr::from_ptr(xml::mrb_xpath_ctx_get_function(xctx) as *const c_char)
            .to_string_lossy()
            .into_owned();
        let document = doc_value((*xctx).doc).unwrap_or(call.document);
        let mut cx = Ctx::from_raw(call.ctx);
        if marshal_funcall(&mut cx, ctxt, nargs, handler, document, &name).is_err() {
            call.error = Some(stash_error(&mut cx));
            xml::xmlXPathErr(ctxt, XPATH_INVALID_TYPE);
        }
    }
}
