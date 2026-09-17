//! The DTD family: `Nokogiri::XML::DTD` (the declaration tables, `validate`,
//! the identifiers), `EntityDecl`, `ElementDecl`, `AttributeDecl`,
//! `ElementContent` (a content-model tree node, owned by its DTD) and
//! `Document#create_entity`.

use crate::*;

/// The payload of an `ElementContent`: a `xmlElementContent` owned by the
/// element declaration of a document (the object's `@document` keeps that
/// alive, as nokogiri's `noko_xml_element_content_wrap` does).
struct XmlElementContent {
    content: *mut xml::xmlElementContent,
}

native!(XmlElementContent, "XmlElementContent");

pub(crate) fn init(ctx: &mut Ctx, c: &Classes) {
    let d = c.dtd;
    ctx.define_method(d, "notations", method!(notations), 0, 0);
    ctx.define_method(d, "elements", method!(elements), 0, 0);
    ctx.define_method(d, "entities", method!(entities), 0, 0);
    ctx.define_method(d, "validate", method!(validate), 1, 0);
    ctx.define_method(d, "attributes", method!(attributes), 0, 0);
    ctx.define_method(d, "system_id", method!(dtd_system_id), 0, 0);
    ctx.define_method(d, "external_id", method!(dtd_external_id), 0, 0);

    let e = c.entity_decl;
    ctx.define_method(e, "original_content", method!(original_content), 0, 0);
    ctx.define_method(e, "content", method!(entity_content), 0, 0);
    ctx.define_method(e, "entity_type", method!(entity_type), 0, 0);
    ctx.define_method(e, "external_id", method!(entity_external_id), 0, 0);
    ctx.define_method(e, "system_id", method!(entity_system_id), 0, 0);
    for (name, v) in [
        ("INTERNAL_GENERAL", xml::XML_INTERNAL_GENERAL_ENTITY),
        (
            "EXTERNAL_GENERAL_PARSED",
            xml::XML_EXTERNAL_GENERAL_PARSED_ENTITY,
        ),
        (
            "EXTERNAL_GENERAL_UNPARSED",
            xml::XML_EXTERNAL_GENERAL_UNPARSED_ENTITY,
        ),
        ("INTERNAL_PARAMETER", xml::XML_INTERNAL_PARAMETER_ENTITY),
        ("EXTERNAL_PARAMETER", xml::XML_EXTERNAL_PARAMETER_ENTITY),
        ("INTERNAL_PREDEFINED", xml::XML_INTERNAL_PREDEFINED_ENTITY),
    ] {
        ctx.const_set(e, name, Value::int(v as i64));
    }

    let el = c.element_decl;
    ctx.define_method(el, "element_type", method!(element_type), 0, 0);
    ctx.define_method(el, "content", method!(element_content), 0, 0);
    ctx.define_method(el, "prefix", method!(element_prefix), 0, 0);

    let a = c.attribute_decl;
    ctx.define_method(a, "attribute_type", method!(attribute_type), 0, 0);
    ctx.define_method(a, "default", method!(attribute_default), 0, 0);
    ctx.define_method(a, "enumeration", method!(enumeration), 0, 0);

    let ec = c.element_content;
    ctx.define_method(ec, "name", method!(content_name), 0, 0);
    ctx.define_method(ec, "type", method!(content_type), 0, 0);
    ctx.define_method(ec, "occur", method!(content_occur), 0, 0);
    ctx.define_method(ec, "prefix", method!(content_prefix), 0, 0);
    ctx.define_method(ec, "c1", method!(content_c1), 0, MR_METHOD_PRIVATE);
    ctx.define_method(ec, "c2", method!(content_c2), 0, MR_METHOD_PRIVATE);

    ctx.define_method(
        c.document,
        "create_entity",
        method!(create_entity),
        MR_ARGC_VARIADIC,
        0,
    );
}

fn dtd(ctx: &mut Ctx, this: Value) -> Result<*mut xml::xmlDtd> {
    Ok(node_ptr(ctx, this)? as *mut xml::xmlDtd)
}

/// One entry of a DTD hash table, copied out of the `xmlHashScan`
/// callback; the Ruby objects are made after the scan (no Ruby runs inside
/// libxml2).
struct Entry {
    name: Vec<u8>,
    payload: *mut c_void,
}

unsafe extern "C" fn scan_collect(
    payload: *mut c_void,
    data: *mut c_void,
    name: *const xml::xmlChar,
) {
    // SAFETY: libxml2 passes back the `Vec<Entry>` we registered and a
    // NUL-terminated key.
    unsafe {
        let list = &mut *(data as *mut Vec<Entry>);
        let name = CStr::from_ptr(name as *const c_char).to_bytes().to_vec();
        list.push(Entry { name, payload });
    }
}

unsafe fn scan(table: *mut c_void) -> Vec<Entry> {
    let mut entries: Vec<Entry> = vec![];
    // SAFETY: a live hash table; the callback only appends to `entries`.
    unsafe {
        xml::xmlHashScan(
            table,
            Some(scan_collect),
            &mut entries as *mut _ as *mut c_void,
        )
    };
    entries
}

/// A Hash of name → wrapped node for the nodes of a DTD table
/// (`element_copier`).
fn node_table(ctx: &mut Ctx, table: *mut c_void) -> Result<Value> {
    // SAFETY: a live table of declaration nodes.
    let entries = unsafe { scan(table) };
    let hash = ctx.hash_new();
    // Wrapping runs Ruby (`decorate`): keep the hash rooted meanwhile.
    let len = ctx.temp_len();
    ctx.temp_push(hash);
    let mut fill = || -> Result<()> {
        for e in &entries {
            let node = wrap_node(ctx, e.payload as *mut xml::xmlNode)?;
            ctx.hash_set(hash, utf8(ctx, &e.name), node)?;
        }
        Ok(())
    };
    let r = fill();
    ctx.temp_truncate(len);
    r?;
    Ok(hash)
}

/// DTD#notations -> Hash of name → Notation | nil
fn notations(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let dtd = dtd(ctx, this)?;
    // SAFETY: a live DTD.
    let table = unsafe { (*dtd).notations };
    if table.is_null() {
        return Ok(Value::nil());
    }
    // SAFETY: a live table of `xmlNotation`s.
    let entries = unsafe { scan(table) };
    let klass = ctx
        .const_get(classes().xml, "Notation")
        .ok_or_else(|| name_error(ctx, "uninitialized constant Nokogiri::XML::Notation"))?;
    let hash = ctx.hash_new();
    let len = ctx.temp_len();
    ctx.temp_push(hash);
    let mut fill = || -> Result<()> {
        for e in &entries {
            let n = e.payload as *const xml::xmlNotation;
            // SAFETY: a live notation.
            let args = unsafe {
                [
                    xml_str(ctx, (*n).name),
                    xml_str(ctx, (*n).PublicID),
                    xml_str(ctx, (*n).SystemID),
                ]
            };
            let notation = ctx.funcall(klass, "new", &args, None)?;
            ctx.hash_set(hash, utf8(ctx, &e.name), notation)?;
        }
        Ok(())
    };
    let r = fill();
    ctx.temp_truncate(len);
    r?;
    Ok(hash)
}

/// DTD#elements -> Hash of name → ElementDecl | nil
fn elements(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let dtd = dtd(ctx, this)?;
    // SAFETY: a live DTD.
    let table = unsafe { (*dtd).elements };
    if table.is_null() {
        return Ok(Value::nil());
    }
    node_table(ctx, table)
}

/// DTD#entities -> Hash of name → EntityDecl | nil
fn entities(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let dtd = dtd(ctx, this)?;
    // SAFETY: a live DTD.
    let table = unsafe { (*dtd).entities };
    if table.is_null() {
        return Ok(Value::nil());
    }
    node_table(ctx, table)
}

/// DTD#attributes -> Hash of name → AttributeDecl (empty when none)
fn attributes(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let dtd = dtd(ctx, this)?;
    // SAFETY: a live DTD.
    let table = unsafe { (*dtd).attributes };
    if table.is_null() {
        return Ok(ctx.hash_new());
    }
    node_table(ctx, table)
}

/// DTD#validate(document) -> Array of SyntaxError
fn validate(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let dtd = dtd(ctx, this)?;
    let doc = doc_ptr(ctx, args[0])?;
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: a live DTD and document; the validation errors go through
    // the global structured handler, registered for this call only.
    unsafe {
        let ctxt = xml::xmlNewValidCtxt();
        xml::xmlSetStructuredErrorFunc(&mut errors as *mut _ as *mut c_void, Some(collect_error));
        xml::xmlValidateDtd(ctxt, doc, dtd);
        xml::xmlSetStructuredErrorFunc(std::ptr::null_mut(), None);
        xml::xmlFreeValidCtxt(ctxt);
    }
    errors_to_array(ctx, &errors)
}

/// DTD#system_id -> String | nil
fn dtd_system_id(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let dtd = dtd(ctx, this)?;
    // SAFETY: a live DTD.
    Ok(unsafe { xml_str(ctx, (*dtd).SystemID) })
}

/// DTD#external_id -> String | nil
fn dtd_external_id(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let dtd = dtd(ctx, this)?;
    // SAFETY: a live DTD.
    Ok(unsafe { xml_str(ctx, (*dtd).ExternalID) })
}

// ---- EntityDecl ----

fn entity(ctx: &mut Ctx, this: Value) -> Result<*mut xml::xmlEntity> {
    Ok(node_ptr(ctx, this)? as *mut xml::xmlEntity)
}

/// EntityDecl#original_content -> String | nil
fn original_content(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let e = entity(ctx, this)?;
    // SAFETY: a live entity declaration.
    Ok(unsafe { xml_str(ctx, (*e).orig) })
}

/// EntityDecl#content -> String | nil
fn entity_content(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let e = entity(ctx, this)?;
    // SAFETY: a live entity declaration; `content` has `length` bytes.
    unsafe {
        if (*e).content.is_null() {
            return Ok(Value::nil());
        }
        let bytes = std::slice::from_raw_parts((*e).content, (*e).length as usize);
        Ok(utf8(ctx, bytes))
    }
}

/// EntityDecl#entity_type -> Integer
fn entity_type(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let e = entity(ctx, this)?;
    // SAFETY: a live entity declaration.
    Ok(Value::int(unsafe { (*e).etype } as i64))
}

/// EntityDecl#external_id -> String | nil
fn entity_external_id(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let e = entity(ctx, this)?;
    // SAFETY: a live entity declaration.
    Ok(unsafe { xml_str(ctx, (*e).ExternalID) })
}

/// EntityDecl#system_id -> String | nil
fn entity_system_id(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let e = entity(ctx, this)?;
    // SAFETY: a live entity declaration.
    Ok(unsafe { xml_str(ctx, (*e).SystemID) })
}

// ---- ElementDecl ----

fn element(ctx: &mut Ctx, this: Value) -> Result<*mut xml::xmlElement> {
    Ok(node_ptr(ctx, this)? as *mut xml::xmlElement)
}

/// ElementDecl#element_type -> Integer
fn element_type(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let e = element(ctx, this)?;
    // SAFETY: a live element declaration.
    Ok(Value::int(unsafe { (*e).etype } as i64))
}

/// ElementDecl#content -> ElementContent | nil
fn element_content(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let e = element(ctx, this)?;
    // SAFETY: a live element declaration.
    let content = unsafe { (*e).content };
    if content.is_null() {
        return Ok(Value::nil());
    }
    let document = ctx.funcall(this, "document", &[], None)?;
    wrap_element_content(ctx, document, content)
}

/// ElementDecl#prefix -> String | nil
fn element_prefix(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let e = element(ctx, this)?;
    // SAFETY: a live element declaration.
    Ok(unsafe { xml_str(ctx, (*e).prefix) })
}

// ---- AttributeDecl ----

fn attribute(ctx: &mut Ctx, this: Value) -> Result<*mut xml::xmlAttribute> {
    Ok(node_ptr(ctx, this)? as *mut xml::xmlAttribute)
}

/// AttributeDecl#attribute_type -> Integer
fn attribute_type(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let a = attribute(ctx, this)?;
    // SAFETY: a live attribute declaration.
    Ok(Value::int(unsafe { (*a).atype } as i64))
}

/// AttributeDecl#default -> String | nil
fn attribute_default(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let a = attribute(ctx, this)?;
    // SAFETY: a live attribute declaration.
    Ok(unsafe { xml_str(ctx, (*a).defaultValue) })
}

/// AttributeDecl#enumeration -> Array of String
fn enumeration(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let a = attribute(ctx, this)?;
    let mut list = vec![];
    // SAFETY: a live attribute declaration with a NULL-terminated
    // enumeration list.
    unsafe {
        let mut e = (*a).tree;
        while !e.is_null() {
            list.push(xml_str(ctx, (*e).name));
            e = (*e).next;
        }
    }
    Ok(ctx.ary_from_vec(list))
}

// ---- ElementContent ----

/// `noko_xml_element_content_wrap`: a new `ElementContent` each time,
/// holding its document in `@document`.
fn wrap_element_content(
    ctx: &mut Ctx,
    document: Value,
    content: *mut xml::xmlElementContent,
) -> Result<Value> {
    let rb = ctx.native_new(classes().element_content, XmlElementContent { content })?;
    ctx.ivar_set(rb, "@document", document)?;
    Ok(rb)
}

fn content(ctx: &mut Ctx, this: Value) -> Result<*mut xml::xmlElementContent> {
    match ctx.native::<XmlElementContent>(this) {
        Some(c) => Ok(c.content),
        None => Err(ctx.argument_error("expected a Nokogiri::XML::ElementContent")),
    }
}

/// ElementContent#name -> String | nil
fn content_name(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let c = content(ctx, this)?;
    // SAFETY: a live content node.
    Ok(unsafe { xml_str(ctx, (*c).name) })
}

/// ElementContent#type -> Integer
fn content_type(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let c = content(ctx, this)?;
    // SAFETY: a live content node.
    Ok(Value::int(unsafe { (*c).type_ } as i64))
}

/// ElementContent#occur -> Integer
fn content_occur(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let c = content(ctx, this)?;
    // SAFETY: a live content node.
    Ok(Value::int(unsafe { (*c).ocur } as i64))
}

/// ElementContent#prefix -> String | nil
fn content_prefix(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let c = content(ctx, this)?;
    // SAFETY: a live content node.
    Ok(unsafe { xml_str(ctx, (*c).prefix) })
}

fn content_child(ctx: &mut Ctx, this: Value, _args: &[Value], second: bool) -> Result<Value> {
    let c = content(ctx, this)?;
    // SAFETY: a live content node.
    let child = unsafe { if second { (*c).c2 } else { (*c).c1 } };
    if child.is_null() {
        return Ok(Value::nil());
    }
    let document = Some(ctx.ivar_get(this, "@document")).unwrap_or_default();
    wrap_element_content(ctx, document, child)
}

/// ElementContent#c1 -> ElementContent | nil (private)
fn content_c1(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    content_child(ctx, this, args, false)
}

/// ElementContent#c2 -> ElementContent | nil (private)
fn content_c2(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    content_child(ctx, this, args, true)
}

// ---- Document#create_entity ----

/// Document#create_entity(name, type = INTERNAL_GENERAL, external_id = nil,
/// system_id = nil, content = nil) -> EntityDecl
fn create_entity(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let doc = doc_ptr(ctx, this)?;
    let args: Vec<Value> = args.to_vec();
    if args.is_empty() || args.len() > 5 {
        return Err(ctx.argument_error(format!(
            "wrong number of arguments (given {}, expected 1..5)",
            args.len()
        )));
    }
    let arg = |i: usize| args.get(i).copied().unwrap_or_default();
    let name = opt_cstr(arg(0), ctx)?;
    let ty = if arg(1).is_nil() {
        xml::XML_INTERNAL_GENERAL_ENTITY
    } else {
        ctx.int(arg(1))? as c_int
    };
    let external_id = opt_cstr(arg(2), ctx)?;
    let system_id = opt_cstr(arg(3), ctx)?;
    let content = opt_cstr(arg(4), ctx)?;
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: a live document; NUL-terminated strings or NULL; the
    // structured handler is registered for this call only.
    let entity = unsafe {
        xml::xmlSetStructuredErrorFunc(&mut errors as *mut _ as *mut c_void, Some(collect_error));
        let e = xml::xmlAddDocEntity(
            doc,
            cptr(&name),
            ty,
            cptr(&external_id),
            cptr(&system_id),
            cptr(&content),
        );
        xml::xmlSetStructuredErrorFunc(std::ptr::null_mut(), None);
        e
    };
    if entity.is_null() {
        let rb_errors = errors_to_array(ctx, &errors)?;
        let klass = classes().xml_syntax_error;
        let ex = ctx.funcall(klass, "aggregate", &[rb_errors], None)?;
        return Err(if ex.truthy() {
            raise(ctx, ex)
        } else {
            ctx.runtime_error("Could not create entity")
        });
    }
    wrap_node_as(
        ctx,
        Some(classes().entity_decl),
        entity as *mut xml::xmlNode,
    )
}
