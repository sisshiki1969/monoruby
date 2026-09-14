//! `Nokogiri::XML::XPathContext`: an `xmlXPathContext` bound to a node,
//! with nokogiri's two built-in XPath functions (`css-class`,
//! `local-name-is`) that the CSS-to-XPath translation relies on, and the
//! custom-function handler of `evaluate(expr, handler)`: a Ruby object
//! whose methods XPath calls as `nokogiri:name(...)`.

use super::node_set::wrap_node_set;
use super::*;

const NOKOGIRI_PREFIX: &[u8] = b"nokogiri\0";
const NOKOGIRI_URI: &[u8] = b"http://www.nokogiri.org/default_ns/ruby/extensions_functions\0";
const NOKOGIRI_BUILTIN_PREFIX: &[u8] = b"nokogiri-builtin\0";
const NOKOGIRI_BUILTIN_URI: &[u8] = b"https://www.nokogiri.org/default_ns/ruby/builtins\0";

/// `xmlXPathError` codes used by the built-in functions.
const XPATH_INVALID_ARITY: c_int = 12;
pub(super) const XPATH_INVALID_TYPE: c_int = 11;

/// The payload of an `XPathContext`.
pub(super) struct XPathContext {
    ctx: *mut xml::xmlXPathContext,
    /// The document the context evaluates against, kept alive.
    document: Value,
}

impl NativeData for XPathContext {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.document.mark(alloc);
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl Drop for XPathContext {
    fn drop(&mut self) {
        // SAFETY: the context is ours.
        unsafe { xml::xmlXPathFreeContext(self.ctx) };
    }
}

pub(super) fn init(globals: &mut Globals, c: &Classes) {
    let x = c.xpath_context;
    globals.define_builtin_class_func(x, "new", new, 1);
    globals.define_builtin_func_with(x, "evaluate", evaluate, 1, 2, false);
    globals.define_builtin_func(x, "register_ns", register_ns, 2);
    globals.define_builtin_func(x, "register_variable", register_variable, 2);
    globals.define_builtin_func(x, "node=", set_node, 1);
}

fn this(lfp: Lfp) -> Result<*mut xml::xmlXPathContext> {
    match lfp.self_val().try_native::<XPathContext>() {
        Some(c) => Ok(c.ctx),
        None => Err(MonorubyErr::argumenterr("expected a Nokogiri::XML::XPathContext")),
    }
}

/// Whether `val` is one of the whitespace-separated words of `str`
/// (`_noko_xml_xpath_context__css_class`).
fn css_class(hay: &[u8], needle: &[u8]) -> bool {
    if needle.is_empty() {
        return true;
    }
    let blank = |b: u8| matches!(b, b' ' | b'\t' | b'\n' | b'\r');
    let mut s = hay;
    while !s.is_empty() {
        if s.starts_with(needle) && s.get(needle.len()).is_none_or(|&b| blank(b)) {
            return true;
        }
        while !s.is_empty() && !blank(s[0]) {
            s = &s[1..];
        }
        while !s.is_empty() && blank(s[0]) {
            s = &s[1..];
        }
    }
    false
}

unsafe fn c_bytes<'a>(p: *const xml::xmlChar) -> &'a [u8] {
    if p.is_null() {
        &[]
    } else {
        // SAFETY: a NUL-terminated libxml2 string.
        unsafe { CStr::from_ptr(p as *const c_char) }.to_bytes()
    }
}

/// XPath `nokogiri-builtin:css-class(haystack, needle)`.
unsafe extern "C" fn xpath_css_class(ctxt: *mut xml::xmlXPathParserContext, nargs: c_int) {
    // SAFETY: libxml2 calls this with its live parser context and the
    // arguments on the value stack.
    unsafe {
        if nargs != 2 {
            xml::xmlXPathErr(ctxt, XPATH_INVALID_ARITY);
            return;
        }
        xml::xmlXPathStringFunction(ctxt, 1);
        let needle = xml::valuePop(ctxt);
        if needle.is_null() || (*needle).type_ != xml::XPATH_STRING {
            xml::xmlXPathFreeObject(needle);
            xml::xmlXPathErr(ctxt, XPATH_INVALID_TYPE);
            return;
        }
        xml::xmlXPathStringFunction(ctxt, 1);
        let hay = xml::valuePop(ctxt);
        if hay.is_null() || (*hay).type_ != xml::XPATH_STRING {
            xml::xmlXPathFreeObject(hay);
            xml::xmlXPathFreeObject(needle);
            xml::xmlXPathErr(ctxt, XPATH_INVALID_TYPE);
            return;
        }
        let found = css_class(c_bytes((*hay).stringval), c_bytes((*needle).stringval));
        xml::valuePush(ctxt, xml::xmlXPathNewBoolean(found as c_int));
        xml::xmlXPathFreeObject(hay);
        xml::xmlXPathFreeObject(needle);
    }
}

/// XPath `nokogiri-builtin:local-name-is(name)`.
unsafe extern "C" fn xpath_local_name_is(ctxt: *mut xml::xmlXPathParserContext, nargs: c_int) {
    // SAFETY: as `xpath_css_class`.
    unsafe {
        if nargs != 1 {
            xml::xmlXPathErr(ctxt, XPATH_INVALID_ARITY);
            return;
        }
        xml::xmlXPathStringFunction(ctxt, 1);
        let name = xml::valuePop(ctxt);
        if name.is_null() || (*name).type_ != xml::XPATH_STRING {
            xml::xmlXPathFreeObject(name);
            xml::xmlXPathErr(ctxt, XPATH_INVALID_TYPE);
            return;
        }
        let node = (*(*ctxt).context).node;
        let equal = !node.is_null() && xml::xmlStrEqual((*node).name, (*name).stringval) != 0;
        xml::valuePush(ctxt, xml::xmlXPathNewBoolean(equal as c_int));
        xml::xmlXPathFreeObject(name);
    }
}

/// XPathContext.new(node) -> XPathContext
#[monoruby_builtin]
fn new(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let node = node_ptr(lfp.arg(0))?;
    // SAFETY: a live node of a live document; the context is ours.
    let (ctx, document) = unsafe {
        let ctx = xml::xmlXPathNewContext((*node).doc);
        (*ctx).node = node;
        xml::xmlXPathRegisterNs(ctx, NOKOGIRI_PREFIX.as_ptr(), NOKOGIRI_URI.as_ptr());
        xml::xmlXPathRegisterNs(ctx, NOKOGIRI_BUILTIN_PREFIX.as_ptr(), NOKOGIRI_BUILTIN_URI.as_ptr());
        xml::xmlXPathRegisterFuncNS(ctx, b"css-class\0".as_ptr(), NOKOGIRI_BUILTIN_URI.as_ptr(), Some(xpath_css_class));
        xml::xmlXPathRegisterFuncNS(
            ctx,
            b"local-name-is\0".as_ptr(),
            NOKOGIRI_BUILTIN_URI.as_ptr(),
            Some(xpath_local_name_is),
        );
        (ctx, doc_value((*node).doc).unwrap_or_default())
    };
    Ok(Value::new_native(class, Box::new(XPathContext { ctx, document })))
}

/// XPathContext#node=(node)
#[monoruby_builtin]
fn set_node(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ctx = this(lfp)?;
    let node = node_ptr(lfp.arg(0))?;
    // SAFETY: a live context and node.
    unsafe {
        (*ctx).doc = (*node).doc;
        (*ctx).node = node;
    }
    Ok(lfp.arg(0))
}

/// XPathContext#register_ns(prefix, uri) -> self
#[monoruby_builtin]
fn register_ns(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ctx = this(lfp)?;
    let prefix = cstr(lfp.arg(0), &globals.store)?;
    let uri = opt_cstr(lfp.arg(1), &globals.store)?;
    // SAFETY: a live context; libxml2 copies the strings.
    unsafe { xml::xmlXPathRegisterNs(ctx, prefix.as_ptr() as *const xml::xmlChar, cptr(&uri)) };
    Ok(lfp.self_val())
}

/// XPathContext#register_variable(name, value) -> self
#[monoruby_builtin]
fn register_variable(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ctx = this(lfp)?;
    let name = cstr(lfp.arg(0), &globals.store)?;
    let value = cstr(lfp.arg(1), &globals.store)?;
    // SAFETY: a live context; the variable object is owned by it.
    unsafe {
        let obj = xml::xmlXPathNewCString(value.as_ptr());
        xml::xmlXPathRegisterVariable(ctx, name.as_ptr() as *const xml::xmlChar, obj);
    }
    Ok(lfp.self_val())
}

/// The state of one `evaluate` with a handler, reachable from the XPath
/// context's function-lookup data while it runs.
struct XPathCall {
    vm: *mut Executor,
    globals: *mut Globals,
    handler: Value,
    document: Value,
    /// The exception a handler method raised (or a bad return type):
    /// the evaluation is aborted and it is re-raised afterwards.
    error: Option<MonorubyErr>,
}

/// The XPath value `obj` as a Ruby value (`_noko_xml_xpath_context__xpath2ruby`);
/// a node set is taken over by the new `NodeSet`.
unsafe fn xpath_to_ruby(
    vm: &mut Executor,
    globals: &mut Globals,
    obj: *mut xml::xmlXPathObject,
    document: Value,
) -> Result<Value> {
    // SAFETY: a live XPath object.
    unsafe {
        Ok(match (*obj).type_ {
            xml::XPATH_STRING => xml_str((*obj).stringval),
            xml::XPATH_NODESET => {
                let set = (*obj).nodesetval;
                (*obj).nodesetval = std::ptr::null_mut();
                wrap_node_set(vm, globals, set, document)?
            }
            xml::XPATH_NUMBER => Value::float((*obj).floatval),
            xml::XPATH_BOOLEAN => Value::bool((*obj).boolval == 1),
            _ => xml_str_owned(xml::xmlXPathCastToString(obj)),
        })
    }
}

/// The function lookup registered for a handler: any XPath function the
/// handler responds to is `handler_invoke`.
unsafe extern "C" fn handler_lookup(data: *mut c_void, name: *const xml::xmlChar, _ns_uri: *const xml::xmlChar) -> xml::xmlXPathFunction {
    // SAFETY: `data` is the `XPathCall` of the running `evaluate`.
    unsafe {
        let call = &*(data as *const XPathCall);
        let globals = &*call.globals;
        let name = CStr::from_ptr(name as *const c_char).to_string_lossy();
        let method = IdentId::get_id(&name);
        if globals.store.check_method_for_class(call.handler.class(), method).is_some() {
            Some(handler_invoke)
        } else {
            None
        }
    }
}

/// Convert the arguments on the stack, call `handler.name(*args)`, push
/// its result as an XPath value
/// (`Nokogiri_marshal_xpath_funcall_and_return_values`). A handler
/// exception or a bad return type is answered as the error; the caller
/// aborts the evaluation with it. Shared with the XSLT extension
/// functions.
pub(super) unsafe fn marshal_funcall(
    vm: &mut Executor,
    globals: &mut Globals,
    ctxt: *mut xml::xmlXPathParserContext,
    nargs: c_int,
    handler: Value,
    document: Value,
    name: &str,
) -> Result<()> {
    // SAFETY: a live parser context with `nargs` arguments on its stack.
    unsafe {
        // The arguments (popped last first); each conversion may run Ruby,
        // so the ones made so far stay rooted.
        let len = vm.temp_len();
        let mut convert = || -> Result<Vec<Value>> {
            let mut args = vec![Value::nil(); nargs as usize];
            for j in (0..nargs as usize).rev() {
                let obj = xml::valuePop(ctxt);
                let v = xpath_to_ruby(vm, globals, obj, document);
                xml::xmlXPathFreeObject(obj);
                let v = v?;
                vm.temp_push(v);
                args[j] = v;
            }
            Ok(args)
        };
        let args = convert();
        let result = args.and_then(|args| vm.invoke_method_inner(globals, IdentId::get_id(name), handler, &args, None, None));
        vm.temp_clear(len);
        let result = result?;
        // The result as an XPath value (nil pushes nothing).
        if result.is_nil() {
            return Ok(());
        }
        let number = match result.unpack() {
            RV::Float(f) => Some(f),
            RV::Fixnum(i) => Some(i as f64),
            RV::BigInt(b) => Some(b.to_f64().unwrap_or(f64::NAN)),
            _ => None,
        };
        if let Some(f) = number {
            xml::valuePush(ctxt, xml::xmlXPathNewFloat(f));
        } else if result.id() == TRUE_VALUE {
            xml::valuePush(ctxt, xml::xmlXPathNewBoolean(1));
        } else if result.id() == FALSE_VALUE {
            xml::valuePush(ctxt, xml::xmlXPathNewBoolean(0));
        } else if result.try_bytes().is_some() {
            let s = cstr(result, &globals.store)?;
            xml::valuePush(ctxt, xml::xmlXPathNewString(s.as_ptr() as *const xml::xmlChar));
        } else if result.try_native::<super::node_set::XmlNodeSet>().is_some() {
            let set = super::node_set::set_ptr(result)?;
            xml::valuePush(ctxt, xml::xmlXPathWrapNodeSet(xml::xmlXPathNodeSetMerge(std::ptr::null_mut(), set)));
        } else if result.ty() == Some(ObjTy::ARRAY) {
            let klass = globals.store.get_module(classes().node_set).as_val();
            let set = vm.invoke_method_inner(globals, IdentId::NEW, klass, &[document, result], None, None)?;
            let set = super::node_set::set_ptr(set)?;
            xml::valuePush(ctxt, xml::xmlXPathWrapNodeSet(xml::xmlXPathNodeSetMerge(std::ptr::null_mut(), set)));
        } else {
            return Err(MonorubyErr::runtimeerr("Invalid return type"));
        }
        Ok(())
    }
}

/// The handler method named by the context, called with the arguments
/// on the stack; a failure aborts the evaluation.
unsafe extern "C" fn handler_invoke(ctxt: *mut xml::xmlXPathParserContext, nargs: c_int) {
    // SAFETY: libxml2 calls this with its live parser context whose
    // XPath context carries the running `XPathCall`.
    unsafe {
        let ctx = (*ctxt).context;
        let call = &mut *(xml::mrb_xpath_ctx_get_func_lookup_data(ctx) as *mut XPathCall);
        if call.error.is_some() {
            xml::xmlXPathErr(ctxt, XPATH_INVALID_TYPE);
            return;
        }
        let name = CStr::from_ptr(xml::mrb_xpath_ctx_get_function(ctx) as *const c_char).to_string_lossy().into_owned();
        if let Err(e) = marshal_funcall(&mut *call.vm, &mut *call.globals, ctxt, nargs, call.handler, call.document, &name) {
            call.error = Some(e);
            xml::xmlXPathErr(ctxt, XPATH_INVALID_TYPE);
        }
    }
}

/// XPathContext#evaluate(expression, handler = nil) -> NodeSet | String | Float | bool
#[monoruby_builtin]
fn evaluate(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ctx = this(lfp)?;
    let expr = cstr(lfp.arg(0), &globals.store)?;
    let document = lfp.self_val().try_native::<XPathContext>().unwrap().document;
    let handler = lfp.try_arg(1).unwrap_or_default();
    let mut errors: Vec<ErrorRecord> = vec![];
    let mut call = XPathCall {
        vm,
        globals,
        handler,
        document,
        error: None,
    };
    // SAFETY: a live context; the error list, the function lookup and the
    // call state are registered for this evaluation only; the result
    // object is ours.
    let obj = unsafe {
        if !handler.is_nil() {
            xml::xmlXPathRegisterFuncLookup(ctx, Some(handler_lookup), &mut call as *mut XPathCall as *mut c_void);
        }
        xml::xmlXPathSetErrorHandler(ctx, Some(collect_error), &mut errors as *mut _ as *mut c_void);
        let obj = xml::xmlXPathEval(expr.as_ptr() as *const xml::xmlChar, ctx);
        xml::xmlXPathSetErrorHandler(ctx, None, std::ptr::null_mut());
        if !handler.is_nil() {
            xml::xmlXPathRegisterFuncLookup(ctx, None, std::ptr::null_mut());
        }
        obj
    };
    if let Some(e) = call.error {
        if !obj.is_null() {
            // SAFETY: a result nobody else holds.
            unsafe { xml::xmlXPathFreeObject(obj) };
        }
        return Err(e);
    }
    if obj.is_null() {
        return Err(match errors.first() {
            Some(e) => raise(syntax_error_value(vm, globals, e)?),
            None => {
                let klass = globals.store.get_module(classes().xpath_syntax_error).as_val();
                let msg = Value::string(format!("Invalid expression: {}", expr.to_string_lossy()));
                raise(vm.invoke_method_inner(globals, IdentId::NEW, klass, &[msg], None, None)?)
            }
        });
    }
    // SAFETY: a live result object.
    unsafe {
        let result = match (*obj).type_ {
            xml::XPATH_STRING => xml_str((*obj).stringval),
            xml::XPATH_NODESET => {
                let set = (*obj).nodesetval;
                (*obj).nodesetval = std::ptr::null_mut();
                wrap_node_set(vm, globals, set, document)?
            }
            xml::XPATH_NUMBER => Value::float((*obj).floatval),
            xml::XPATH_BOOLEAN => Value::bool((*obj).boolval == 1),
            _ => wrap_node_set(vm, globals, std::ptr::null_mut(), document)?,
        };
        xml::xmlXPathFreeObject(obj);
        Ok(result)
    }
}
