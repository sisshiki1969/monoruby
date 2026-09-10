//! `Nokogiri::XSLT` over the bundled libxslt (`xslt_stylesheet.c`):
//! `Stylesheet.parse_stylesheet_doc` / `#transform` / `#serialize`, and
//! `XSLT.register`: extension functions in a namespace answered by the
//! instance methods of a Ruby class (one instance per transform).

use super::xpath::{XPATH_INVALID_TYPE, marshal_funcall};
use super::*;
use std::cell::Cell;

/// The payload of a `Stylesheet`: the compiled stylesheet, and the
/// handler instances of the running transform (`func_instances`), which
/// libxslt holds only as raw module data.
pub(super) struct XsltStylesheet {
    ss: *mut xml::xsltStylesheet,
    func_instances: Vec<Value>,
}

impl NativeData for XsltStylesheet {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        for v in &self.func_instances {
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

impl Drop for XsltStylesheet {
    fn drop(&mut self) {
        // SAFETY: the stylesheet (and the document copy it parsed) is ours.
        unsafe { xml::xsltFreeStylesheet(self.ss) }
    }
}

pub(super) fn init(globals: &mut Globals, c: &Classes) {
    globals.define_builtin_class_func(
        c.stylesheet,
        "parse_stylesheet_doc",
        parse_stylesheet_doc,
        1,
    );
    globals.define_builtin_func(c.stylesheet, "serialize", serialize, 1);
    globals.define_builtin_func_with(c.stylesheet, "transform", transform, 1, 2, false);
    globals.define_builtin_class_func(c.xslt, "register", register, 2);
    // `@modules`: the registered handler classes by namespace URI.
    let xslt = globals.store.get_module(c.xslt).as_val();
    globals
        .store
        .set_ivar(
            xslt,
            IdentId::get_id("@modules"),
            Value::hash(RubyMap::default()),
        )
        .expect("Nokogiri::XSLT is a module");
    // SAFETY: plain library initialization (`Init_nokogiri`); libxslt's own
    // extras are registered by its lazy `xsltInit`.
    unsafe { xml::exsltRegisterAll() };
}

fn this(lfp: Lfp) -> Result<*mut xml::xsltStylesheet> {
    match lfp.self_val().try_native::<XsltStylesheet>() {
        Some(s) => Ok(s.ss),
        None => Err(MonorubyErr::argumenterr(
            "expected a Nokogiri::XSLT::Stylesheet",
        )),
    }
}

/// `noko_xml_document_unwrap`: a Document, nothing else. The error names
/// the argument as `rb_check_typeddata` does: a wrapped node by its
/// struct name, anything else by its class.
fn document_ptr(globals: &Globals, v: Value) -> Result<*mut xml::xmlDoc> {
    v.try_native::<XmlDocument>().map(|d| d.doc).ok_or_else(|| {
        let name = if v.try_native::<XmlNode>().is_some() {
            "xmlNode".to_string()
        } else {
            builtin_type_name(globals, v)
        };
        MonorubyErr::typeerr(format!("wrong argument type {name} (expected xmlDoc)"))
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
#[monoruby_builtin]
fn parse_stylesheet_doc(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let doc = document_ptr(globals, lfp.arg(0))?;
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
        return Err(MonorubyErr::runtimeerr(msg));
    }
    let rb = Value::new_native(
        class,
        Box::new(XsltStylesheet {
            ss,
            func_instances: vec![],
        }),
    );
    // SAFETY: a live stylesheet we own; the extension callbacks find the
    // Ruby object through it.
    unsafe { xml::mrb_xslt_stylesheet_set_private(ss, rb.id() as *mut c_void) };
    Ok(rb)
}

/// Stylesheet#serialize(document) -> String: the document as the
/// stylesheet's output method says.
#[monoruby_builtin]
fn serialize(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let doc = document_ptr(globals, lfp.arg(0))?;
    let ss = this(lfp)?;
    let mut ptr: *mut xml::xmlChar = std::ptr::null_mut();
    let mut len: c_int = 0;
    // SAFETY: a live document and stylesheet; the buffer is ours to free.
    unsafe {
        xml::xsltSaveResultToString(&mut ptr, &mut len, doc, ss);
        if ptr.is_null() {
            return Ok(utf8(&[]));
        }
        let s = utf8(std::slice::from_raw_parts(
            ptr as *const u8,
            len.max(0) as usize,
        ));
        xml::xml_free()(ptr as *mut c_void);
        Ok(s)
    }
}

/// The state of a running `transform`, reachable from the extension
/// callbacks libxslt makes during it.
struct XsltCall {
    vm: *mut Executor,
    globals: *mut Globals,
    /// The document being transformed (the node sets an extension
    /// function receives belong to it, unless they are libxslt's own).
    document: Value,
    /// The exception a handler raised (or a bad return type): the
    /// evaluation is aborted and it is re-raised by `transform`.
    error: Option<MonorubyErr>,
}

thread_local! {
    static XSLT_CALL: Cell<*mut XsltCall> = const { Cell::new(std::ptr::null_mut()) };
}

/// Stylesheet#transform(document, params = []) -> XML::Document
#[monoruby_builtin]
fn transform(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let rb_document = lfp.arg(0);
    let mut rb_param = lfp.try_arg(1).unwrap_or_default();
    if rb_param.is_nil() {
        rb_param = Value::array_from_vec(vec![]);
    }
    if rb_document.try_native::<XmlDocument>().is_none() {
        return Err(MonorubyErr::argumenterr(
            "argument must be a Nokogiri::XML::Document",
        ));
    }
    // A Hash of params is flattened to the name / value list.
    if rb_param.try_hash_ty().is_some() {
        rb_param =
            vm.invoke_method_inner(globals, IdentId::get_id("to_a"), rb_param, &[], None, None)?;
        rb_param = vm.invoke_method_inner(
            globals,
            IdentId::get_id("flatten"),
            rb_param,
            &[],
            None,
            None,
        )?;
    }
    if rb_param.ty() != Some(ObjTy::ARRAY) {
        return Err(MonorubyErr::typeerr(format!(
            "wrong argument type {} (expected Array)",
            builtin_type_name(globals, rb_param)
        )));
    }
    let entries: Vec<Value> = rb_param.as_array().iter().cloned().collect();
    let mut params: Vec<CString> = Vec::with_capacity(entries.len());
    for e in entries {
        params.push(cstr(e, &globals.store)?);
    }
    let mut c_params: Vec<*const c_char> = params.iter().map(|p| p.as_ptr()).collect();
    c_params.push(std::ptr::null());
    let ss = this(lfp)?;
    let c_document = doc_ptr(rb_document)?;
    let mut call = XsltCall {
        vm,
        globals,
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
            && super::schema::has_wrapped_blank_nodes(c_document);
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
        return Err(e);
    }
    if !msg.is_empty() {
        discard(result);
        return Err(MonorubyErr::runtimeerr(msg));
    }
    if result.is_null() {
        return Err(MonorubyErr::runtimeerr("could not apply the stylesheet"));
    }
    wrap_document(vm, globals, classes().document, result, &[])
}

/// XSLT.register(uri, klass) -> XSLT: the instance methods of `klass`
/// become the XSLT extension functions of the namespace `uri`
/// (globally, as nokogiri's).
#[monoruby_builtin]
fn register(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let this = lfp.self_val();
    let uri = lfp.arg(0);
    let obj = lfp.arg(1);
    let modules = globals
        .store
        .get_ivar(this, IdentId::get_id("@modules"))
        .unwrap_or_default();
    if modules.is_nil() {
        return Err(MonorubyErr::runtimeerr("internal error: @modules not set"));
    }
    vm.invoke_method_inner(
        globals,
        IdentId::get_id("[]="),
        modules,
        &[uri, obj],
        None,
        None,
    )?;
    let c_uri = cstr(uri, &globals.store)?;
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
    ctxt: *mut xml::xsltTransformContext,
) -> Option<&'a mut XsltStylesheet> {
    // SAFETY: a live transform context; `_private` holds the Value bits
    // set by `parse_stylesheet_doc` (or NULL).
    unsafe {
        let p = xml::mrb_xslt_stylesheet_get_private(xml::mrb_xslt_transform_ctxt_style(ctxt));
        if p.is_null() {
            return None;
        }
        native_mut::<XsltStylesheet>(Value::from_u64(p as u64))
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
        let vm = &mut *call.vm;
        let globals = &mut *call.globals;
        let r = (|| -> Result<*mut c_void> {
            let xslt = globals.store.get_module(classes().xslt).as_val();
            let modules = globals
                .store
                .get_ivar(xslt, IdentId::get_id("@modules"))
                .unwrap_or_default();
            let key = xml_str(uri);
            let obj = vm.invoke_method_inner(
                globals,
                IdentId::get_id("[]"),
                modules,
                &[key],
                None,
                None,
            )?;
            let methods = vm.invoke_method_inner(
                globals,
                IdentId::get_id("instance_methods"),
                obj,
                &[Value::bool(false)],
                None,
                None,
            )?;
            let methods: Vec<Value> = methods.as_array().iter().cloned().collect();
            for m in methods {
                let name =
                    vm.invoke_method_inner(globals, IdentId::get_id("to_s"), m, &[], None, None)?;
                let name = cstr(name, &globals.store)?;
                xml::xsltRegisterExtFunction(
                    ctxt,
                    name.as_ptr() as *const xml::xmlChar,
                    uri,
                    Some(ext_call),
                );
            }
            let inst = vm.invoke_method_inner(globals, IdentId::NEW, obj, &[], None, None)?;
            if let Some(w) = stylesheet_of(ctxt) {
                w.func_instances.push(inst);
            }
            Ok(inst.id() as *mut c_void)
        })();
        match r {
            Ok(p) => p,
            Err(e) => {
                call.error = Some(e);
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
    // SAFETY: a live transform context.
    unsafe {
        if let Some(w) = stylesheet_of(ctxt) {
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
        let ctx = (*ctxt).context;
        let transform = xml::xsltXPathGetTransformContext(ctxt);
        let handler = xml::xsltGetExtData(transform, xml::mrb_xpath_ctx_get_function_uri(ctx));
        if handler.is_null() {
            xml::xmlXPathErr(ctxt, XPATH_INVALID_TYPE);
            return;
        }
        let handler = Value::from_u64(handler as u64);
        let name = CStr::from_ptr(xml::mrb_xpath_ctx_get_function(ctx) as *const c_char)
            .to_string_lossy()
            .into_owned();
        let document = doc_value((*ctx).doc).unwrap_or(call.document);
        if let Err(e) = marshal_funcall(
            &mut *call.vm,
            &mut *call.globals,
            ctxt,
            nargs,
            handler,
            document,
            &name,
        ) {
            call.error = Some(e);
            xml::xmlXPathErr(ctxt, XPATH_INVALID_TYPE);
        }
    }
}
