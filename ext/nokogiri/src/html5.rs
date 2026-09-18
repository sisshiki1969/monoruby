//! HTML5 over the bundled gumbo-parser (`ext/nokogiri/gumbo.c`):
//! `Nokogiri::Gumbo.parse` / `.fragment` build a libxml2 document from
//! gumbo's tree (the walk itself is C, `glue/monoruby_gumbo.c`), and the
//! HTML5 serializer behind `Node#to_html` on HTML5 documents
//! (`html_standard_serialize`, `prepend_newline?`).

use crate::*;

pub(crate) fn init(ctx: &mut Ctx, c: &Classes) {
    // Three positional arguments and keywords (`max_attributes:`,
    // `max_errors:`, `max_tree_depth:`, `parse_noscript_content_as_text:`);
    // a variadic registration receives the keywords as a trailing Hash.
    ctx.define_method(
        c.gumbo,
        "parse",
        method!(parse),
        MR_ARGC_VARIADIC,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(
        c.gumbo,
        "fragment",
        method!(fragment),
        MR_ARGC_VARIADIC,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(
        c.node,
        "html_standard_serialize",
        method!(html_standard_serialize),
        1,
        MR_METHOD_PRIVATE,
    );
    ctx.define_method(
        c.node,
        "prepend_newline?",
        method!(prepend_newline),
        0,
        MR_METHOD_PRIVATE,
    );
}

/// The keyword arguments of `parse` / `fragment` (`common_options`): the
/// three required limits, the optional noscript flag; anything else is
/// an unknown keyword.
struct Options {
    max_attributes: c_int,
    max_errors: c_int,
    max_tree_depth: c_int,
    noscript_as_text: bool,
}

fn options(ctx: &mut Ctx, args: &[Value]) -> Result<Options> {
    // The three positionals, then the keywords as one Hash (the C
    // extension takes them with `rb_get_kwargs`: every missing required
    // keyword is named at once, an unknown one is an error).
    let kw = match args.get(3).copied() {
        Some(h) if try_hash(ctx, h).is_some() => Some(h),
        _ => None,
    };
    if args.len() > 4 || (args.len() == 4 && kw.is_none()) || args.len() < 3 {
        return Err(ctx.argument_error(format!(
            "wrong number of arguments (given {}, expected 3)",
            args.len()
        )));
    }
    let get = |ctx: &mut Ctx, name: &str| -> Result<Option<Value>> {
        let Some(h) = kw else { return Ok(None) };
        let k = ctx.sym(name);
        Ok(ctx.hash_get(h, k)?.filter(|v| !v.is_nil()))
    };
    let mut missing: Vec<&str> = vec![];
    let mut required = [0 as c_int; 3];
    for (i, name) in ["max_attributes", "max_errors", "max_tree_depth"]
        .iter()
        .enumerate()
    {
        match get(ctx, name)? {
            Some(v) => required[i] = ctx.int(v)? as c_int,
            None => missing.push(name),
        }
    }
    if !missing.is_empty() {
        let names: Vec<String> = missing.iter().map(|n| format!(":{n}")).collect();
        return Err(ctx.argument_error(format!(
            "missing keyword{}: {}",
            if missing.len() > 1 { "s" } else { "" },
            names.join(", ")
        )));
    }
    let noscript_as_text = get(ctx, "parse_noscript_content_as_text")?.is_some_and(|v| v.truthy());
    if let Some(h) = kw {
        let keys = ctx.funcall(h, "keys", &[], None)?;
        for k in ary_vec(ctx, keys)? {
            let name = match try_symbol(ctx, k) {
                Some(n) => n,
                None => ctx.inspect(k),
            };
            if ![
                "max_attributes",
                "max_errors",
                "max_tree_depth",
                "parse_noscript_content_as_text",
            ]
            .contains(&name.as_str())
            {
                return Err(ctx.argument_error(format!("unknown keyword: :{name}")));
            }
        }
    }
    Ok(Options {
        max_attributes: required[0],
        max_errors: required[1],
        max_tree_depth: required[2],
        noscript_as_text,
    })
}

/// A fragment parse's context (`options.fragment_*`).
struct Fragment {
    context: CString,
    namespace: c_int,
    encoding: Option<CString>,
    quirks_mode: c_int,
    has_form_ancestor: bool,
}

/// Run gumbo (`perform_parse`); the output is freed by the caller. The
/// input must be a String (`Check_Type`).
fn perform_parse(
    ctx: &mut Ctx,
    opts: &Options,
    fragment: Option<&Fragment>,
    input: Value,
) -> Result<*mut xml::GumboOutput> {
    if !ctx.is_string(input) {
        return Err(ctx.type_error(format!(
            "wrong argument type {} (expected String)",
            builtin_type_name(ctx, input)
        )));
    }
    // The String's own bytes, not a copy: the errors gumbo records point
    // into the parsed buffer, and `add_errors` renders them against it.
    let (ptr, len) = str_raw(ctx, input)?;
    // SAFETY: the buffer outlives the call; the option strings do too.
    let output = unsafe {
        xml::mrb_gumbo_parse(
            ptr as *const c_char,
            len,
            opts.max_attributes,
            opts.max_errors,
            opts.max_tree_depth,
            opts.noscript_as_text as c_int,
            fragment.is_some() as c_int,
            fragment.map_or(std::ptr::null(), |f| f.context.as_ptr()),
            fragment.map_or(0, |f| f.namespace),
            fragment
                .and_then(|f| f.encoding.as_ref())
                .map_or(std::ptr::null(), |e| e.as_ptr()),
            fragment.map_or(0, |f| f.quirks_mode),
            fragment.is_some_and(|f| f.has_form_ancestor) as c_int,
        )
    };
    if output.is_null() {
        return Err(ctx.runtime_error("could not parse"));
    }
    // SAFETY: a live output.
    let status = unsafe { xml::mrb_gumbo_output_status(output) };
    if status != xml::GUMBO_STATUS_OK {
        // SAFETY: a static message; the output is ours to free.
        let msg = unsafe {
            let s = CStr::from_ptr(xml::mrb_gumbo_status_string(status))
                .to_string_lossy()
                .into_owned();
            xml::mrb_gumbo_destroy_output(output);
            s
        };
        if status == xml::GUMBO_STATUS_OUT_OF_MEMORY {
            let klass = ctx
                .const_get(ctx.object_class(), "NoMemoryError")
                .ok_or_else(|| ctx.runtime_error(msg.clone()))?;
            let ex = ctx.funcall(klass, "new", &[ctx.str(msg)], None)?;
            return Err(raise(ctx, ex));
        }
        return Err(ctx.argument_error(msg));
    }
    Ok(output)
}

/// `add_errors`: the parse errors as `SyntaxError`s in the target's
/// `@errors` (left alone when there are none).
fn add_errors(
    ctx: &mut Ctx,
    output: *const xml::GumboOutput,
    target: Value,
    input: Value,
    url: Value,
) -> Result<()> {
    // SAFETY: a live output.
    let count = unsafe { xml::mrb_gumbo_error_count(output) };
    if count == 0 {
        return Ok(());
    }
    // The errors point into the parsed buffer: the diagnostics must be
    // rendered against that same memory (the String's own bytes, which
    // do not move), not a copy.
    let (ptr, len) = if ctx.is_string(input) {
        str_raw(ctx, input)?
    } else {
        (std::ptr::null(), 0)
    };
    let file = try_bytes(ctx, url);
    let mut records = Vec::with_capacity(count);
    for i in 0..count {
        let mut size = 0usize;
        let mut code: *const c_char = std::ptr::null();
        let (mut line, mut column) = (0usize, 0usize);
        // SAFETY: a live output; the message is malloc'd for us.
        let (message, str1) = unsafe {
            let msg = xml::mrb_gumbo_error(
                output,
                i,
                ptr as *const c_char,
                len,
                &mut size,
                &mut code,
                &mut line,
                &mut column,
            );
            let message = if msg.is_null() {
                vec![]
            } else {
                let m = std::slice::from_raw_parts(msg as *const u8, size).to_vec();
                xml::mrb_gumbo_free(msg as *mut c_void);
                m
            };
            let str1 = if code.is_null() {
                None
            } else {
                Some(CStr::from_ptr(code).to_bytes().to_vec())
            };
            (message, str1)
        };
        records.push(ErrorRecord {
            domain: 1, // XML_FROM_PARSER
            code: 1,   // XML_ERR_INTERNAL_ERROR
            message: Some(message),
            level: 2, // XML_ERR_ERROR
            file: file.clone(),
            line: line as c_int,
            str1,
            str2: None,
            str3: None,
            int1: 0,
            column: column as c_int,
            path: None,
        });
    }
    let errors = errors_to_array(ctx, &records)?;
    ctx.ivar_set(target, "@errors", errors)
}

/// Gumbo.parse(input, url, klass, **options) -> HTML5::Document
fn parse(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let input = args[0];
    let url = args[1];
    let klass = args[2];
    let opts = options(ctx, args)?;
    let output = perform_parse(ctx, &opts, None, input)?;
    // SAFETY: a live output (freed below) and a fresh document that the
    // Ruby object takes over.
    let result = unsafe {
        let doc = if xml::mrb_gumbo_has_doctype(output) != 0 {
            xml::mrb_gumbo_new_html_doc(
                xml::mrb_gumbo_doctype_name(output),
                xml::mrb_gumbo_doctype_system(output),
                xml::mrb_gumbo_doctype_public(output),
            )
        } else {
            xml::mrb_gumbo_new_html_doc(std::ptr::null(), std::ptr::null(), std::ptr::null())
        };
        if doc.is_null() {
            xml::mrb_gumbo_destroy_output(output);
            return Err(ctx.runtime_error("could not create document"));
        }
        xml::mrb_gumbo_build_document(doc, output);
        let quirks = xml::mrb_gumbo_quirks_mode(output);
        wrap_document(ctx, klass, doc, &[]).and_then(|rdoc| {
            // `SyntaxError.new` runs Ruby: keep the document rooted.
            let len = ctx.temp_len();
            ctx.temp_push(rdoc);
            let r = (|| {
                ctx.ivar_set(rdoc, "@url", url)?;
                ctx.ivar_set(rdoc, "@quirks_mode", Value::int(quirks as i64))?;
                add_errors(ctx, output, rdoc, input, url)
            })();
            ctx.temp_truncate(len);
            r.map(|()| rdoc)
        })
    };
    // SAFETY: the output is ours.
    unsafe { xml::mrb_gumbo_destroy_output(output) };
    result
}

/// `lookup_namespace`: the gumbo namespace of a node's namespace href
/// (HTML for none); an unknown one is an error when `require_known`,
/// -1 otherwise.
fn lookup_namespace(ctx: &mut Ctx, node: Value, require_known: bool) -> Result<c_int> {
    let ns = ctx.funcall(node, "namespace", &[], None)?;
    if ns.is_nil() {
        return Ok(xml::GUMBO_NAMESPACE_HTML);
    }
    let href = ctx.funcall(ns, "href", &[], None)?;
    let href = ctx.str_vec(href)?;
    match href.as_slice() {
        b"http://www.w3.org/1999/xhtml" => Ok(xml::GUMBO_NAMESPACE_HTML),
        b"http://www.w3.org/1998/Math/MathML" => Ok(xml::GUMBO_NAMESPACE_MATHML),
        b"http://www.w3.org/2000/svg" => Ok(xml::GUMBO_NAMESPACE_SVG),
        _ if require_known => Err(ctx.argument_error(format!(
            "Unexpected namespace URI \"{}\"",
            String::from_utf8_lossy(&href)
        ))),
        _ => Ok(-1),
    }
}

/// Gumbo.fragment(fragment, tags, context, **options) -> nil: parse
/// `tags` as the children of `fragment` in the given context (nil: body,
/// a "ns:tag" string, or a Node).
fn fragment(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let doc_fragment = args[0];
    let tags = args[1];
    let pctx = args[2];
    let opts = options(ctx, args)?;
    let name_id = "name";

    let mut namespace = xml::GUMBO_NAMESPACE_HTML;
    let mut has_form_ancestor = false;
    let mut encoding: Option<CString> = None;
    let context: CString;
    if pctx.is_nil() {
        context = CString::new("body").unwrap();
    } else if let Some(s) = try_bytes(ctx, pctx) {
        let tag = s;
        let mut local = tag.as_slice();
        if let Some(colon) = tag.iter().position(|&b| b == b':') {
            let prefix = &tag[..colon];
            let lower = prefix.to_ascii_lowercase();
            namespace = match lower.as_slice() {
                b"svg" => xml::GUMBO_NAMESPACE_SVG,
                b"html" => xml::GUMBO_NAMESPACE_HTML,
                b"math" => xml::GUMBO_NAMESPACE_MATHML,
                _ => {
                    // (nokogiri formats the whole string here: its `%*s`
                    // takes the prefix length as a field width)
                    return Err(ctx.argument_error(format!(
                        "Invalid context namespace '{}'",
                        String::from_utf8_lossy(&tag)
                    )));
                }
            };
            local = &tag[colon + 1..];
        } else {
            // For convenience, put 'svg' and 'math' in their namespaces.
            let lower = tag.to_ascii_lowercase();
            if lower == b"svg" {
                namespace = xml::GUMBO_NAMESPACE_SVG;
            } else if lower == b"math" {
                namespace = xml::GUMBO_NAMESPACE_MATHML;
            }
        }
        has_form_ancestor =
            namespace == xml::GUMBO_NAMESPACE_HTML && local.eq_ignore_ascii_case(b"form");
        context =
            CString::new(local).map_err(|_| ctx.argument_error("string contains null byte"))?;
    } else {
        let tag_name = ctx.funcall(pctx, name_id, &[], None)?;
        let tag = ctx.str_vec(tag_name)?;
        namespace = lookup_namespace(ctx, pctx, true)?;
        // A form ancestor, including self.
        let element_p = "element?";
        let parent = "parent";
        let mut node = pctx;
        while !node.is_nil() {
            let is_element = ctx.funcall(node, element_p, &[], None)?;
            if is_element.truthy() {
                let name = ctx.funcall(node, name_id, &[], None)?;
                let is_form = try_bytes(ctx, name).is_some_and(|n| n.eq_ignore_ascii_case(b"form"));
                if is_form && lookup_namespace(ctx, node, false)? == xml::GUMBO_NAMESPACE_HTML {
                    has_form_ancestor = true;
                    break;
                }
            }
            node = match funcall_if_exists(ctx, node, parent, &[])? {
                Some(p) => p,
                None => Value::nil(),
            };
        }
        if namespace == xml::GUMBO_NAMESPACE_MATHML && tag.eq_ignore_ascii_case(b"annotation-xml") {
            let key = ctx.str("encoding");
            let enc = ctx.funcall(pctx, "[]", &[key], None)?;
            if enc.truthy() {
                if try_bytes(ctx, enc).is_none() {
                    return Err(ctx.type_error(format!(
                        "wrong argument type {} (expected String)",
                        builtin_type_name(ctx, enc)
                    )));
                }
                encoding = Some(cstr(enc, ctx)?);
            }
        }
        context = CString::new(tag).map_err(|_| ctx.argument_error("string contains null byte"))?;
    }

    // Quirks mode.
    let doc = ctx.funcall(doc_fragment, "document", &[], None)?;
    let dtd = ctx.funcall(doc, "internal_subset", &[], None)?;
    let doc_quirks = ctx.ivar_get(doc, "@quirks_mode");
    let quirks_mode = if pctx.is_nil() || try_bytes(ctx, pctx).is_some() || doc_quirks.is_nil() {
        xml::GUMBO_DOCTYPE_NO_QUIRKS
    } else if dtd.is_nil() {
        xml::GUMBO_DOCTYPE_QUIRKS
    } else {
        let get = |ctx: &mut Ctx, m: &str| -> Result<Option<CString>> {
            let v = ctx.funcall(dtd, m, &[], None)?;
            opt_cstr(v, ctx)
        };
        let name = get(ctx, "name")?;
        let pubid = get(ctx, "external_id")?;
        let sysid = get(ctx, "system_id")?;
        // SAFETY: NUL-terminated strings or NULL.
        unsafe {
            xml::mrb_gumbo_compute_quirks_mode(
                cptr(&name) as *const c_char,
                cptr(&pubid) as *const c_char,
                cptr(&sysid) as *const c_char,
            )
        }
    };

    let frag = Fragment {
        context,
        namespace,
        encoding,
        quirks_mode,
        has_form_ancestor,
    };
    let xml_doc = doc_ptr(ctx, doc)?;
    let xml_frag = node_ptr(ctx, doc_fragment)?;
    let output = perform_parse(ctx, &opts, Some(&frag), tags)?;
    // SAFETY: a live output (freed below), a live document and fragment.
    let result = unsafe {
        xml::mrb_gumbo_build_fragment(xml_doc, xml_frag, output);
        let quirks = xml::mrb_gumbo_quirks_mode(output);
        ctx.ivar_set(doc_fragment, "@quirks_mode", Value::int(quirks as i64))
            .and_then(|()| add_errors(ctx, output, doc_fragment, tags, ctx.str("#fragment")))
    };
    // SAFETY: the output is ours.
    unsafe { xml::mrb_gumbo_destroy_output(output) };
    result?;
    Ok(Value::nil())
}

// ---- the HTML5 serializer (`html_standard_serialize`) ----

unsafe fn c_str<'a>(p: *const xml::xmlChar) -> &'a [u8] {
    if p.is_null() {
        &[]
    } else {
        // SAFETY: a NUL-terminated libxml2 string.
        unsafe { CStr::from_ptr(p as *const c_char) }.to_bytes()
    }
}

/// `should_prepend_newline`: a pre / textarea / listing whose content
/// starts with a newline (which the HTML syntax would otherwise drop).
unsafe fn should_prepend_newline(node: *mut xml::xmlNode) -> bool {
    // SAFETY: a live node.
    unsafe {
        let name = c_str((*node).name);
        let child = (*node).children;
        if (*node).name.is_null()
            || child.is_null()
            || !matches!(name, b"pre" | b"textarea" | b"listing")
        {
            return false;
        }
        (*child).type_ == xml::XML_TEXT_NODE
            && !(*child).content.is_null()
            && *(*child).content == b'\n'
    }
}

/// Whether an unnamespaced node is one of `tagnames`.
unsafe fn is_one_of(node: *mut xml::xmlNode, tagnames: &[&[u8]]) -> bool {
    // SAFETY: a live node.
    unsafe {
        // Fragments have no name; a namespaced node is foreign content.
        if (*node).name.is_null() || !(*node).ns.is_null() {
            return false;
        }
        let name = c_str((*node).name);
        tagnames.contains(&name)
    }
}

const XHTML_NS: &[u8] = b"http://www.w3.org/1999/xhtml";
const MATHML_NS: &[u8] = b"http://www.w3.org/1998/Math/MathML";
const SVG_NS: &[u8] = b"http://www.w3.org/2000/svg";

/// Elements in the HTML, MathML and SVG namespaces do not use a
/// namespace prefix in the HTML syntax.
unsafe fn output_tagname(out: &mut Vec<u8>, elem: *mut xml::xmlNode) {
    // SAFETY: a live element.
    unsafe {
        let mut name = c_str((*elem).name);
        let ns = (*elem).ns;
        if !ns.is_null() && !(*ns).href.is_null() && !(*ns).prefix.is_null() {
            let href = c_str((*ns).href);
            if href != XHTML_NS && href != MATHML_NS && href != SVG_NS {
                out.extend_from_slice(c_str((*ns).prefix));
                out.push(b':');
                if let Some(colon) = name.iter().position(|&b| b == b':') {
                    name = &name[colon + 1..];
                }
            }
        }
        out.extend_from_slice(name);
    }
}

unsafe fn output_attr_name(out: &mut Vec<u8>, attr: *mut xml::xmlAttr) {
    // SAFETY: a live attribute.
    unsafe {
        let ns = (*attr).ns;
        let mut name = c_str((*attr).name);
        if !ns.is_null() && !(*ns).href.is_null() {
            let uri = c_str((*ns).href);
            let localname = match name.iter().position(|&b| b == b':') {
                Some(colon) => &name[colon + 1..],
                None => name,
            };
            if uri == b"http://www.w3.org/XML/1998/namespace" {
                out.extend_from_slice(b"xml:");
                name = localname;
            } else if uri == b"http://www.w3.org/2000/xmlns/" {
                // xmlns:xmlns -> xmlns, xmlns:foo -> xmlns:foo
                if localname != b"xmlns" {
                    out.extend_from_slice(b"xmlns:");
                }
                name = localname;
            } else if uri == b"http://www.w3.org/1999/xlink" {
                out.extend_from_slice(b"xlink:");
                name = localname;
            } else if !(*ns).prefix.is_null() {
                out.extend_from_slice(c_str((*ns).prefix));
                out.push(b':');
                name = localname;
            }
        }
        out.extend_from_slice(name);
    }
}

fn output_escaped(out: &mut Vec<u8>, s: &[u8], attr: bool) {
    let mut i = 0;
    while i < s.len() {
        let ch = s[i];
        let (replacement, replaced): (&[u8], usize) = if ch == b'&' {
            (b"&amp;", 1)
        } else if ch == 0xC2 && s.get(i + 1) == Some(&0xA0) {
            // U+00A0 NO-BREAK SPACE has the UTF-8 encoding C2 A0.
            (b"&nbsp;", 2)
        } else if attr && ch == b'"' {
            (b"&quot;", 1)
        } else if !attr && ch == b'<' {
            (b"&lt;", 1)
        } else if !attr && ch == b'>' {
            (b"&gt;", 1)
        } else {
            out.push(ch);
            i += 1;
            continue;
        };
        out.extend_from_slice(replacement);
        i += replaced;
    }
}

const VOID_ELEMENTS: &[&[u8]] = &[
    b"area",
    b"base",
    b"basefont",
    b"bgsound",
    b"br",
    b"col",
    b"embed",
    b"frame",
    b"hr",
    b"img",
    b"input",
    b"keygen",
    b"link",
    b"meta",
    b"param",
    b"source",
    b"track",
    b"wbr",
];

const UNESCAPED_TEXT_ELEMENTS: &[&[u8]] = &[
    b"style",
    b"script",
    b"xmp",
    b"iframe",
    b"noembed",
    b"noframes",
    b"plaintext",
    b"noscript",
];

unsafe fn output_node(
    ctx: &mut Ctx,
    out: &mut Vec<u8>,
    node: *mut xml::xmlNode,
    preserve_newline: bool,
) -> Result<()> {
    // SAFETY: a live node of a live document.
    unsafe {
        match (*node).type_ {
            xml::XML_ELEMENT_NODE => {
                out.push(b'<');
                output_tagname(out, node);
                let mut attr = (*node).properties;
                while !attr.is_null() {
                    out.push(b' ');
                    output_node(ctx, out, attr as *mut xml::xmlNode, preserve_newline)?;
                    attr = (*attr).next;
                }
                out.push(b'>');
                if !is_one_of(node, VOID_ELEMENTS) {
                    if preserve_newline && should_prepend_newline(node) {
                        out.push(b'\n');
                    }
                    let mut child = (*node).children;
                    while !child.is_null() {
                        output_node(ctx, out, child, preserve_newline)?;
                        child = (*child).next;
                    }
                    out.extend_from_slice(b"</");
                    output_tagname(out, node);
                    out.push(b'>');
                }
            }
            xml::XML_ATTRIBUTE_NODE => {
                let attr = node as *mut xml::xmlAttr;
                output_attr_name(out, attr);
                if !(*attr).children.is_null() {
                    out.extend_from_slice(b"=\"");
                    let value = xml::xmlNodeListGetString((*attr).doc, (*attr).children, 1);
                    output_escaped(out, c_str(value), true);
                    if !value.is_null() {
                        xml::xml_free()(value as *mut c_void);
                    }
                    out.push(b'"');
                } else {
                    out.extend_from_slice(b"=\"\"");
                }
            }
            xml::XML_TEXT_NODE => {
                let parent = (*node).parent;
                if !parent.is_null() && is_one_of(parent, UNESCAPED_TEXT_ELEMENTS) {
                    out.extend_from_slice(c_str((*node).content));
                } else {
                    output_escaped(out, c_str((*node).content), false);
                }
            }
            xml::XML_CDATA_SECTION_NODE => {
                out.extend_from_slice(b"<![CDATA[");
                out.extend_from_slice(c_str((*node).content));
                out.extend_from_slice(b"]]>");
            }
            xml::XML_COMMENT_NODE => {
                out.extend_from_slice(b"<!--");
                out.extend_from_slice(c_str((*node).content));
                out.extend_from_slice(b"-->");
            }
            xml::XML_PI_NODE => {
                out.extend_from_slice(b"<?");
                out.extend_from_slice(c_str((*node).content));
                out.push(b'>');
            }
            xml::XML_DOCUMENT_TYPE_NODE | xml::XML_DTD_NODE => {
                out.extend_from_slice(b"<!DOCTYPE ");
                out.extend_from_slice(c_str((*node).name));
                out.push(b'>');
            }
            xml::XML_DOCUMENT_NODE | xml::XML_DOCUMENT_FRAG_NODE | xml::XML_HTML_DOCUMENT_NODE => {
                let mut child = (*node).children;
                while !child.is_null() {
                    output_node(ctx, out, child, preserve_newline)?;
                    child = (*child).next;
                }
            }
            ty => {
                return Err(ctx.runtime_error(format!(
                    "Unsupported document node ({ty}); this is a bug in Nokogiri"
                )));
            }
        }
        Ok(())
    }
}

/// Node#html_standard_serialize(preserve_newline) -> String
fn html_standard_serialize(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let node = node_ptr(ctx, this)?;
    let mut out = Vec::with_capacity(4096);
    // SAFETY: a live node.
    unsafe { output_node(ctx, &mut out, node, args[0].truthy())? };
    Ok(utf8(ctx, &out))
}

/// Node#prepend_newline? -> bool
fn prepend_newline(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let node = node_ptr(ctx, this)?;
    // SAFETY: a live node.
    Ok(Value::bool(unsafe { should_prepend_newline(node) }))
}
