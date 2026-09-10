//! HTML5 over the bundled gumbo-parser (`ext/nokogiri/gumbo.c`):
//! `Nokogiri::Gumbo.parse` / `.fragment` build a libxml2 document from
//! gumbo's tree (the walk itself is C, `glue/monoruby_gumbo.c`), and the
//! HTML5 serializer behind `Node#to_html` on HTML5 documents
//! (`html_standard_serialize`, `prepend_newline?`).

use super::*;

pub(super) fn init(globals: &mut Globals, c: &Classes) {
    const KW: &[&str] = &["max_attributes", "max_errors", "max_tree_depth", "parse_noscript_content_as_text"];
    globals.define_builtin_class_func_with_kw(c.gumbo, "parse", parse, 3, 3, false, KW, true);
    globals.define_builtin_class_func_with_kw(c.gumbo, "fragment", fragment, 3, 3, false, KW, true);
    globals.define_private_builtin_func(c.node, "html_standard_serialize", html_standard_serialize, 1);
    globals.define_private_builtin_func(c.node, "prepend_newline?", prepend_newline, 0);
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

fn options(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Options> {
    // `rb_get_kwargs`: every missing required keyword is named at once.
    let missing: Vec<&str> = ["max_attributes", "max_errors", "max_tree_depth"]
        .iter()
        .enumerate()
        .filter(|(i, _)| lfp.try_arg(3 + i).is_none_or(|v| v.is_nil()))
        .map(|(_, name)| *name)
        .collect();
    if !missing.is_empty() {
        let names: Vec<String> = missing.iter().map(|n| format!(":{n}")).collect();
        return Err(MonorubyErr::argumenterr(format!(
            "missing keyword{}: {}",
            if missing.len() > 1 { "s" } else { "" },
            names.join(", ")
        )));
    }
    let required = |i: usize| -> Result<c_int> { Ok(lfp.arg(3 + i).expect_integer(&globals.store)? as c_int) };
    let max_attributes = required(0)?;
    let max_errors = required(1)?;
    let max_tree_depth = required(2)?;
    let noscript_as_text = lfp.try_arg(6).is_some_and(|v| v.as_bool());
    if let Some(rest) = lfp.try_arg(7)
        && rest.try_hash_ty().is_some()
        && let Some((k, _)) = rest.as_hash().iter().next()
    {
        let name = vm.invoke_method_inner(globals, IdentId::get_id("inspect"), k, &[], None, None)?;
        return Err(MonorubyErr::argumenterr(format!("unknown keyword: {}", name.as_str())));
    }
    Ok(Options {
        max_attributes,
        max_errors,
        max_tree_depth,
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
    vm: &mut Executor,
    globals: &mut Globals,
    opts: &Options,
    fragment: Option<&Fragment>,
    input: Value,
) -> Result<*mut xml::GumboOutput> {
    let Some(bytes) = input.try_bytes() else {
        return Err(MonorubyErr::typeerr(format!(
            "wrong argument type {} (expected String)",
            builtin_type_name(globals, input)
        )));
    };
    let bytes = bytes.as_bytes();
    // SAFETY: the buffer outlives the call; the option strings do too.
    let output = unsafe {
        xml::mrb_gumbo_parse(
            bytes.as_ptr() as *const c_char,
            bytes.len(),
            opts.max_attributes,
            opts.max_errors,
            opts.max_tree_depth,
            opts.noscript_as_text as c_int,
            fragment.is_some() as c_int,
            fragment.map_or(std::ptr::null(), |f| f.context.as_ptr()),
            fragment.map_or(0, |f| f.namespace),
            fragment.and_then(|f| f.encoding.as_ref()).map_or(std::ptr::null(), |e| e.as_ptr()),
            fragment.map_or(0, |f| f.quirks_mode),
            fragment.is_some_and(|f| f.has_form_ancestor) as c_int,
        )
    };
    if output.is_null() {
        return Err(MonorubyErr::runtimeerr("could not parse"));
    }
    // SAFETY: a live output.
    let status = unsafe { xml::mrb_gumbo_output_status(output) };
    if status != xml::GUMBO_STATUS_OK {
        // SAFETY: a static message; the output is ours to free.
        let msg = unsafe {
            let s = CStr::from_ptr(xml::mrb_gumbo_status_string(status)).to_string_lossy().into_owned();
            xml::mrb_gumbo_destroy_output(output);
            s
        };
        if status == xml::GUMBO_STATUS_OUT_OF_MEMORY {
            let klass = globals
                .store
                .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("NoMemoryError"))
                .ok_or_else(|| MonorubyErr::runtimeerr(msg.clone()))?;
            let ex = vm.invoke_method_inner(globals, IdentId::NEW, klass, &[Value::string(msg)], None, None)?;
            return Err(raise(ex));
        }
        return Err(MonorubyErr::argumenterr(msg));
    }
    Ok(output)
}

/// `add_errors`: the parse errors as `SyntaxError`s in the target's
/// `@errors` (left alone when there are none).
fn add_errors(
    vm: &mut Executor,
    globals: &mut Globals,
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
    let bytes: &[u8] = match input.try_bytes() {
        Some(b) => b.as_bytes(),
        None => &[],
    };
    let file = url.try_bytes().map(|b| b.as_bytes().to_vec());
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
                bytes.as_ptr() as *const c_char,
                bytes.len(),
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
            let str1 = if code.is_null() { None } else { Some(CStr::from_ptr(code).to_bytes().to_vec()) };
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
    let errors = errors_to_array(vm, globals, &records)?;
    globals.store.set_ivar(target, IdentId::get_id("@errors"), errors)
}

/// Gumbo.parse(input, url, klass, **options) -> HTML5::Document
#[monoruby_builtin]
fn parse(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let input = lfp.arg(0);
    let url = lfp.arg(1);
    let klass = lfp.arg(2).as_class_id();
    let opts = options(vm, globals, lfp)?;
    let output = perform_parse(vm, globals, &opts, None, input)?;
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
            return Err(MonorubyErr::runtimeerr("could not create document"));
        }
        xml::mrb_gumbo_build_document(doc, output);
        let quirks = xml::mrb_gumbo_quirks_mode(output);
        wrap_document(vm, globals, klass, doc, &[]).and_then(|rdoc| {
            // `SyntaxError.new` runs Ruby: keep the document rooted.
            let len = vm.temp_len();
            vm.temp_push(rdoc);
            let r = (|| {
                globals.store.set_ivar(rdoc, IdentId::get_id("@url"), url)?;
                globals.store.set_ivar(rdoc, IdentId::get_id("@quirks_mode"), Value::integer(quirks as i64))?;
                add_errors(vm, globals, output, rdoc, input, url)
            })();
            vm.temp_clear(len);
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
fn lookup_namespace(vm: &mut Executor, globals: &mut Globals, node: Value, require_known: bool) -> Result<c_int> {
    let ns = vm.invoke_method_inner(globals, IdentId::get_id("namespace"), node, &[], None, None)?;
    if ns.is_nil() {
        return Ok(xml::GUMBO_NAMESPACE_HTML);
    }
    let href = vm.invoke_method_inner(globals, IdentId::get_id("href"), ns, &[], None, None)?;
    let href = href.expect_bytes(&globals.store)?.to_vec();
    match href.as_slice() {
        b"http://www.w3.org/1999/xhtml" => Ok(xml::GUMBO_NAMESPACE_HTML),
        b"http://www.w3.org/1998/Math/MathML" => Ok(xml::GUMBO_NAMESPACE_MATHML),
        b"http://www.w3.org/2000/svg" => Ok(xml::GUMBO_NAMESPACE_SVG),
        _ if require_known => Err(MonorubyErr::argumenterr(format!(
            "Unexpected namespace URI \"{}\"",
            String::from_utf8_lossy(&href)
        ))),
        _ => Ok(-1),
    }
}

/// Gumbo.fragment(fragment, tags, context, **options) -> nil: parse
/// `tags` as the children of `fragment` in the given context (nil: body,
/// a "ns:tag" string, or a Node).
#[monoruby_builtin]
fn fragment(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let doc_fragment = lfp.arg(0);
    let tags = lfp.arg(1);
    let ctx = lfp.arg(2);
    let opts = options(vm, globals, lfp)?;
    let name_id = IdentId::get_id("name");

    let mut namespace = xml::GUMBO_NAMESPACE_HTML;
    let mut has_form_ancestor = false;
    let mut encoding: Option<CString> = None;
    let context: CString;
    if ctx.is_nil() {
        context = CString::new("body").unwrap();
    } else if let Some(s) = ctx.try_bytes() {
        let tag = s.as_bytes().to_vec();
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
                    return Err(MonorubyErr::argumenterr(format!(
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
        has_form_ancestor = namespace == xml::GUMBO_NAMESPACE_HTML && local.eq_ignore_ascii_case(b"form");
        context = CString::new(local).map_err(|_| MonorubyErr::argumenterr("string contains null byte"))?;
    } else {
        let tag_name = vm.invoke_method_inner(globals, name_id, ctx, &[], None, None)?;
        let tag = tag_name.expect_bytes(&globals.store)?.to_vec();
        namespace = lookup_namespace(vm, globals, ctx, true)?;
        // A form ancestor, including self.
        let element_p = IdentId::get_id("element?");
        let parent = IdentId::get_id("parent");
        let mut node = ctx;
        while !node.is_nil() {
            let is_element = vm.invoke_method_inner(globals, element_p, node, &[], None, None)?;
            if is_element.as_bool() {
                let name = vm.invoke_method_inner(globals, name_id, node, &[], None, None)?;
                let is_form = name.try_bytes().is_some_and(|n| n.as_bytes().eq_ignore_ascii_case(b"form"));
                if is_form && lookup_namespace(vm, globals, node, false)? == xml::GUMBO_NAMESPACE_HTML {
                    has_form_ancestor = true;
                    break;
                }
            }
            node = match vm.invoke_method_if_exists(globals, parent, node, &[], None, None)? {
                Some(p) => p,
                None => Value::nil(),
            };
        }
        if namespace == xml::GUMBO_NAMESPACE_MATHML && tag.eq_ignore_ascii_case(b"annotation-xml") {
            let key = Value::string_from_str("encoding");
            let enc = vm.invoke_method_inner(globals, IdentId::get_id("[]"), ctx, &[key], None, None)?;
            if enc.as_bool() {
                if enc.try_bytes().is_none() {
                    return Err(MonorubyErr::typeerr(format!(
                        "wrong argument type {} (expected String)",
                        builtin_type_name(globals, enc)
                    )));
                }
                encoding = Some(cstr(enc, &globals.store)?);
            }
        }
        context = CString::new(tag).map_err(|_| MonorubyErr::argumenterr("string contains null byte"))?;
    }

    // Quirks mode.
    let doc = vm.invoke_method_inner(globals, IdentId::get_id("document"), doc_fragment, &[], None, None)?;
    let dtd = vm.invoke_method_inner(globals, IdentId::get_id("internal_subset"), doc, &[], None, None)?;
    let doc_quirks = globals
        .store
        .get_ivar(doc, IdentId::get_id("@quirks_mode"))
        .unwrap_or_default();
    let quirks_mode = if ctx.is_nil() || ctx.try_bytes().is_some() || doc_quirks.is_nil() {
        xml::GUMBO_DOCTYPE_NO_QUIRKS
    } else if dtd.is_nil() {
        xml::GUMBO_DOCTYPE_QUIRKS
    } else {
        let get = |vm: &mut Executor, globals: &mut Globals, m: &str| -> Result<Option<CString>> {
            let v = vm.invoke_method_inner(globals, IdentId::get_id(m), dtd, &[], None, None)?;
            opt_cstr(v, &globals.store)
        };
        let name = get(vm, globals, "name")?;
        let pubid = get(vm, globals, "external_id")?;
        let sysid = get(vm, globals, "system_id")?;
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
    let xml_doc = doc_ptr(doc)?;
    let xml_frag = node_ptr(doc_fragment)?;
    let output = perform_parse(vm, globals, &opts, Some(&frag), tags)?;
    // SAFETY: a live output (freed below), a live document and fragment.
    let result = unsafe {
        xml::mrb_gumbo_build_fragment(xml_doc, xml_frag, output);
        let quirks = xml::mrb_gumbo_quirks_mode(output);
        globals
            .store
            .set_ivar(doc_fragment, IdentId::get_id("@quirks_mode"), Value::integer(quirks as i64))
            .and_then(|()| add_errors(vm, globals, output, doc_fragment, tags, Value::string_from_str("#fragment")))
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
        if (*node).name.is_null() || child.is_null() || !matches!(name, b"pre" | b"textarea" | b"listing") {
            return false;
        }
        (*child).type_ == xml::XML_TEXT_NODE && !(*child).content.is_null() && *(*child).content == b'\n'
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
    b"area", b"base", b"basefont", b"bgsound", b"br", b"col", b"embed", b"frame", b"hr", b"img", b"input", b"keygen",
    b"link", b"meta", b"param", b"source", b"track", b"wbr",
];

const UNESCAPED_TEXT_ELEMENTS: &[&[u8]] =
    &[b"style", b"script", b"xmp", b"iframe", b"noembed", b"noframes", b"plaintext", b"noscript"];

unsafe fn output_node(out: &mut Vec<u8>, node: *mut xml::xmlNode, preserve_newline: bool) -> Result<()> {
    // SAFETY: a live node of a live document.
    unsafe {
        match (*node).type_ {
            xml::XML_ELEMENT_NODE => {
                out.push(b'<');
                output_tagname(out, node);
                let mut attr = (*node).properties;
                while !attr.is_null() {
                    out.push(b' ');
                    output_node(out, attr as *mut xml::xmlNode, preserve_newline)?;
                    attr = (*attr).next;
                }
                out.push(b'>');
                if !is_one_of(node, VOID_ELEMENTS) {
                    if preserve_newline && should_prepend_newline(node) {
                        out.push(b'\n');
                    }
                    let mut child = (*node).children;
                    while !child.is_null() {
                        output_node(out, child, preserve_newline)?;
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
                    output_node(out, child, preserve_newline)?;
                    child = (*child).next;
                }
            }
            ty => {
                return Err(MonorubyErr::runtimeerr(format!(
                    "Unsupported document node ({ty}); this is a bug in Nokogiri"
                )));
            }
        }
        Ok(())
    }
}

/// Node#html_standard_serialize(preserve_newline) -> String
#[monoruby_builtin]
fn html_standard_serialize(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = node_ptr(lfp.self_val())?;
    let mut out = Vec::with_capacity(4096);
    // SAFETY: a live node.
    unsafe { output_node(&mut out, node, lfp.arg(0).as_bool())? };
    Ok(utf8(&out))
}

/// Node#prepend_newline? -> bool
#[monoruby_builtin]
fn prepend_newline(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let node = node_ptr(lfp.self_val())?;
    // SAFETY: a live node.
    Ok(Value::bool(unsafe { should_prepend_newline(node) }))
}
