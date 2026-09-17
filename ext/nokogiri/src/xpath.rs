//! `Nokogiri::XML::XPathContext`: an `xmlXPathContext` bound to a node,
//! with nokogiri's two built-in XPath functions (`css-class`,
//! `local-name-is`) that the CSS-to-XPath translation relies on, and the
//! custom-function handler of `evaluate(expr, handler)`: a Ruby object
//! whose methods XPath calls as `nokogiri:name(...)`.

use super::node_set::wrap_node_set;
use crate::*;

const NOKOGIRI_PREFIX: &[u8] = b"nokogiri\0";
const NOKOGIRI_URI: &[u8] = b"http://www.nokogiri.org/default_ns/ruby/extensions_functions\0";
const NOKOGIRI_BUILTIN_PREFIX: &[u8] = b"nokogiri-builtin\0";
const NOKOGIRI_BUILTIN_URI: &[u8] = b"https://www.nokogiri.org/default_ns/ruby/builtins\0";

/// `xmlXPathError` codes used by the built-in functions.
const XPATH_INVALID_ARITY: c_int = 12;
pub(crate) const XPATH_INVALID_TYPE: c_int = 11;

/// The payload of an `XPathContext`.
pub(crate) struct XPathContext {
    ctx: *mut xml::xmlXPathContext,
    /// The document the context evaluates against, kept alive.
    document: Value,
}

native!(XPathContext, "XPathContext", |this, m| {
    m.mark(this.document);
});

impl Drop for XPathContext {
    fn drop(&mut self) {
        // SAFETY: the context is ours.
        unsafe { xml::xmlXPathFreeContext(self.ctx) };
    }
}

pub(crate) fn init(ctx: &mut Ctx, c: &Classes) {
    let x = c.xpath_context;
    ctx.define_method(x, "new", method!(new), 1, MR_METHOD_SINGLETON);
    ctx.define_method(x, "evaluate", method!(evaluate), MR_ARGC_VARIADIC, 0) /* arity 1..2 */;
    ctx.define_method(x, "register_ns", method!(register_ns), 2, 0);
    ctx.define_method(x, "register_variable", method!(register_variable), 2, 0);
    ctx.define_method(x, "node=", method!(set_node), 1, 0);
}

fn recv(ctx: &mut Ctx, this: Value) -> Result<*mut xml::xmlXPathContext> {
    match ctx.native::<XPathContext>(this) {
        Some(c) => Ok(c.ctx),
        None => Err(ctx.argument_error("expected a Nokogiri::XML::XPathContext")),
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
fn new(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let node = node_ptr(ctx, args[0])?;
    // SAFETY: a live node of a live document; the context is ours.
    let (xctx, document) = unsafe {
        let xctx = xml::xmlXPathNewContext((*node).doc);
        (*xctx).node = node;
        xml::xmlXPathRegisterNs(xctx, NOKOGIRI_PREFIX.as_ptr(), NOKOGIRI_URI.as_ptr());
        xml::xmlXPathRegisterNs(
            xctx,
            NOKOGIRI_BUILTIN_PREFIX.as_ptr(),
            NOKOGIRI_BUILTIN_URI.as_ptr(),
        );
        xml::xmlXPathRegisterFuncNS(
            xctx,
            b"css-class\0".as_ptr(),
            NOKOGIRI_BUILTIN_URI.as_ptr(),
            Some(xpath_css_class),
        );
        xml::xmlXPathRegisterFuncNS(
            xctx,
            b"local-name-is\0".as_ptr(),
            NOKOGIRI_BUILTIN_URI.as_ptr(),
            Some(xpath_local_name_is),
        );
        (xctx, doc_value((*node).doc).unwrap_or_default())
    };
    ctx.native_new(
        class,
        XPathContext {
            ctx: xctx,
            document,
        },
    )
}

/// XPathContext#node=(node)
fn set_node(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let xctx = recv(ctx, this)?;
    let node = node_ptr(ctx, args[0])?;
    // SAFETY: a live context and node.
    unsafe {
        (*xctx).doc = (*node).doc;
        (*xctx).node = node;
    }
    Ok(args[0])
}

/// XPathContext#register_ns(prefix, uri) -> self
fn register_ns(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let xctx = recv(ctx, this)?;
    let prefix = cstr(args[0], ctx)?;
    let uri = opt_cstr(args[1], ctx)?;
    // SAFETY: a live context; libxml2 copies the strings.
    unsafe { xml::xmlXPathRegisterNs(xctx, prefix.as_ptr() as *const xml::xmlChar, cptr(&uri)) };
    Ok(this)
}

/// XPathContext#register_variable(name, value) -> self
fn register_variable(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let xctx = recv(ctx, this)?;
    let name = cstr(args[0], ctx)?;
    let value = cstr(args[1], ctx)?;
    // SAFETY: a live context; the variable object is owned by it.
    unsafe {
        let obj = xml::xmlXPathNewCString(value.as_ptr());
        xml::xmlXPathRegisterVariable(xctx, name.as_ptr() as *const xml::xmlChar, obj);
    }
    Ok(this)
}

/// The state of one `evaluate` with a handler, reachable from the XPath
/// context's function-lookup data while it runs.
struct XPathCall {
    ctx: *mut MrContext,
    handler: Value,
    document: Value,
    /// The exception a handler method raised (or a bad return type):
    /// the evaluation is aborted and it is re-raised afterwards.
    error: Option<Value>,
}

/// The XPath value `obj` as a Ruby value (`_noko_xml_xpath_context__xpath2ruby`);
/// a node set is taken over by the new `NodeSet`.
unsafe fn xpath_to_ruby(
    ctx: &mut Ctx,
    obj: *mut xml::xmlXPathObject,
    document: Value,
) -> Result<Value> {
    // SAFETY: a live XPath object.
    unsafe {
        Ok(match (*obj).type_ {
            xml::XPATH_STRING => xml_str(ctx, (*obj).stringval),
            xml::XPATH_NODESET => {
                let set = (*obj).nodesetval;
                (*obj).nodesetval = std::ptr::null_mut();
                wrap_node_set(ctx, set, document)?
            }
            xml::XPATH_NUMBER => Value::float((*obj).floatval),
            xml::XPATH_BOOLEAN => Value::bool((*obj).boolval == 1),
            _ => xml_str_owned(ctx, xml::xmlXPathCastToString(obj)),
        })
    }
}

/// The function lookup registered for a handler: any XPath function the
/// handler responds to is `handler_invoke`.
unsafe extern "C" fn handler_lookup(
    data: *mut c_void,
    name: *const xml::xmlChar,
    _ns_uri: *const xml::xmlChar,
) -> xml::xmlXPathFunction {
    // SAFETY: `data` is the `XPathCall` of the running `evaluate`.
    unsafe {
        let call = &*(data as *const XPathCall);
        let cx = Ctx::from_raw(call.ctx);
        let name = CStr::from_ptr(name as *const c_char).to_string_lossy();
        if cx.respond_to(call.handler, &name) {
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
pub(crate) unsafe fn marshal_funcall(
    ctx: &mut Ctx,
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
        let len = ctx.temp_len();
        let mut convert = || -> Result<Vec<Value>> {
            let mut args = vec![Value::nil(); nargs as usize];
            for j in (0..nargs as usize).rev() {
                let obj = xml::valuePop(ctxt);
                let v = xpath_to_ruby(ctx, obj, document);
                xml::xmlXPathFreeObject(obj);
                let v = v?;
                ctx.temp_push(v);
                args[j] = v;
            }
            Ok(args)
        };
        let args = convert();
        let result = args.and_then(|args| ctx.funcall(handler, name, &args, None));
        ctx.temp_truncate(len);
        let result = result?;
        // The result as an XPath value (nil pushes nothing).
        if result.is_nil() {
            return Ok(());
        }
        let ty = ctx.type_of(result);
        let number = match ty {
            MrType::Float => Some(ctx.float(result)?),
            // (a Bignum is answered as a Float by `float`)
            MrType::Integer => Some(ctx.float(result)?),
            _ => None,
        };
        if let Some(f) = number {
            xml::valuePush(ctxt, xml::xmlXPathNewFloat(f));
        } else if ty == MrType::True {
            xml::valuePush(ctxt, xml::xmlXPathNewBoolean(1));
        } else if ty == MrType::False {
            xml::valuePush(ctxt, xml::xmlXPathNewBoolean(0));
        } else if ty == MrType::String {
            let s = cstr(result, ctx)?;
            xml::valuePush(
                ctxt,
                xml::xmlXPathNewString(s.as_ptr() as *const xml::xmlChar),
            );
        } else if ctx.native::<super::node_set::XmlNodeSet>(result).is_some() {
            let set = super::node_set::set_ptr(ctx, result)?;
            xml::valuePush(
                ctxt,
                xml::xmlXPathWrapNodeSet(xml::xmlXPathNodeSetMerge(std::ptr::null_mut(), set)),
            );
        } else if ty == MrType::Array {
            let klass = classes().node_set;
            let set = ctx.funcall(klass, "new", &[document, result], None)?;
            let set = super::node_set::set_ptr(ctx, set)?;
            xml::valuePush(
                ctxt,
                xml::xmlXPathWrapNodeSet(xml::xmlXPathNodeSetMerge(std::ptr::null_mut(), set)),
            );
        } else {
            return Err(ctx.runtime_error("Invalid return type"));
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
        let xctx = (*ctxt).context;
        let call = &mut *(xml::mrb_xpath_ctx_get_func_lookup_data(xctx) as *mut XPathCall);
        if call.error.is_some() {
            xml::xmlXPathErr(ctxt, XPATH_INVALID_TYPE);
            return;
        }
        let name = CStr::from_ptr(xml::mrb_xpath_ctx_get_function(xctx) as *const c_char)
            .to_string_lossy()
            .into_owned();
        let mut cx = Ctx::from_raw(call.ctx);
        if marshal_funcall(&mut cx, ctxt, nargs, call.handler, call.document, &name).is_err() {
            call.error = Some(stash_error(&mut cx));
            xml::xmlXPathErr(ctxt, XPATH_INVALID_TYPE);
        }
    }
}

/// XPathContext#evaluate(expression, handler = nil) -> NodeSet | String | Float | bool
fn evaluate(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    check_arity(ctx, args, 1, 2)?;
    let xctx = recv(ctx, this)?;
    let expr = cstr(args[0], ctx)?;
    let document = ctx.native::<XPathContext>(this).unwrap().document;
    let handler = args.get(1).copied().unwrap_or_default();
    let mut errors: Vec<ErrorRecord> = vec![];
    let mut call = XPathCall {
        ctx: ctx.raw(),
        handler,
        document,
        error: None,
    };
    // SAFETY: a live context; the error list, the function lookup and the
    // call state are registered for this evaluation only; the result
    // object is ours.
    let obj = unsafe {
        if !handler.is_nil() {
            xml::xmlXPathRegisterFuncLookup(
                xctx,
                Some(handler_lookup),
                &mut call as *mut XPathCall as *mut c_void,
            );
        }
        xml::xmlXPathSetErrorHandler(
            xctx,
            Some(collect_error),
            &mut errors as *mut _ as *mut c_void,
        );
        let obj = xml::xmlXPathEval(expr.as_ptr() as *const xml::xmlChar, xctx);
        xml::xmlXPathSetErrorHandler(xctx, None, std::ptr::null_mut());
        if !handler.is_nil() {
            xml::xmlXPathRegisterFuncLookup(xctx, None, std::ptr::null_mut());
        }
        obj
    };
    if let Some(e) = call.error {
        if !obj.is_null() {
            // SAFETY: a result nobody else holds.
            unsafe { xml::xmlXPathFreeObject(obj) };
        }
        return Err(raise(ctx, e));
    }
    if obj.is_null() {
        let ex = match errors.first() {
            Some(e) => syntax_error_value(ctx, e)?,
            None => {
                let klass = classes().xpath_syntax_error;
                let msg = ctx.str(format!("Invalid expression: {}", expr.to_string_lossy()));
                ctx.funcall(klass, "new", &[msg], None)?
            }
        };
        return Err(raise(ctx, ex));
    }
    // SAFETY: a live result object.
    unsafe {
        let result = match (*obj).type_ {
            xml::XPATH_STRING => xml_str(ctx, (*obj).stringval),
            xml::XPATH_NODESET => {
                let set = (*obj).nodesetval;
                (*obj).nodesetval = std::ptr::null_mut();
                wrap_node_set(ctx, set, document)?
            }
            xml::XPATH_NUMBER => Value::float((*obj).floatval),
            xml::XPATH_BOOLEAN => Value::bool((*obj).boolval == 1),
            _ => wrap_node_set(ctx, std::ptr::null_mut(), document)?,
        };
        xml::xmlXPathFreeObject(obj);
        Ok(result)
    }
}
