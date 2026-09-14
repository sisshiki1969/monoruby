//! The DTD family: `Nokogiri::XML::DTD` (the declaration tables, `validate`,
//! the identifiers), `EntityDecl`, `ElementDecl`, `AttributeDecl`,
//! `ElementContent` (a content-model tree node, owned by its DTD) and
//! `Document#create_entity`.

use super::*;

/// The payload of an `ElementContent`: a `xmlElementContent` owned by the
/// element declaration of a document (the object's `@document` keeps that
/// alive, as nokogiri's `noko_xml_element_content_wrap` does).
struct XmlElementContent {
    content: *mut xml::xmlElementContent,
}

impl NativeData for XmlElementContent {
    fn mark(&self, _alloc: &mut alloc::Allocator<RValue>) {}
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

pub(super) fn init(globals: &mut Globals, c: &Classes) {
    let d = c.dtd;
    globals.define_builtin_func(d, "notations", notations, 0);
    globals.define_builtin_func(d, "elements", elements, 0);
    globals.define_builtin_func(d, "entities", entities, 0);
    globals.define_builtin_func(d, "validate", validate, 1);
    globals.define_builtin_func(d, "attributes", attributes, 0);
    globals.define_builtin_func(d, "system_id", dtd_system_id, 0);
    globals.define_builtin_func(d, "external_id", dtd_external_id, 0);

    let e = c.entity_decl;
    globals.define_builtin_func(e, "original_content", original_content, 0);
    globals.define_builtin_func(e, "content", entity_content, 0);
    globals.define_builtin_func(e, "entity_type", entity_type, 0);
    globals.define_builtin_func(e, "external_id", entity_external_id, 0);
    globals.define_builtin_func(e, "system_id", entity_system_id, 0);
    for (name, v) in [
        ("INTERNAL_GENERAL", xml::XML_INTERNAL_GENERAL_ENTITY),
        ("EXTERNAL_GENERAL_PARSED", xml::XML_EXTERNAL_GENERAL_PARSED_ENTITY),
        ("EXTERNAL_GENERAL_UNPARSED", xml::XML_EXTERNAL_GENERAL_UNPARSED_ENTITY),
        ("INTERNAL_PARAMETER", xml::XML_INTERNAL_PARAMETER_ENTITY),
        ("EXTERNAL_PARAMETER", xml::XML_EXTERNAL_PARAMETER_ENTITY),
        ("INTERNAL_PREDEFINED", xml::XML_INTERNAL_PREDEFINED_ENTITY),
    ] {
        globals.set_constant(e, IdentId::get_id(name), Value::integer(v as i64));
    }

    let el = c.element_decl;
    globals.define_builtin_func(el, "element_type", element_type, 0);
    globals.define_builtin_func(el, "content", element_content, 0);
    globals.define_builtin_func(el, "prefix", element_prefix, 0);

    let a = c.attribute_decl;
    globals.define_builtin_func(a, "attribute_type", attribute_type, 0);
    globals.define_builtin_func(a, "default", attribute_default, 0);
    globals.define_builtin_func(a, "enumeration", enumeration, 0);

    let ec = c.element_content;
    globals.define_builtin_func(ec, "name", content_name, 0);
    globals.define_builtin_func(ec, "type", content_type, 0);
    globals.define_builtin_func(ec, "occur", content_occur, 0);
    globals.define_builtin_func(ec, "prefix", content_prefix, 0);
    globals.define_private_builtin_func(ec, "c1", content_c1, 0);
    globals.define_private_builtin_func(ec, "c2", content_c2, 0);

    globals.define_builtin_func_rest(c.document, "create_entity", create_entity);
}

fn dtd(lfp: Lfp) -> Result<*mut xml::xmlDtd> {
    Ok(node_ptr(lfp.self_val())? as *mut xml::xmlDtd)
}

/// One entry of a DTD hash table, copied out of the `xmlHashScan`
/// callback; the Ruby objects are made after the scan (no Ruby runs inside
/// libxml2).
struct Entry {
    name: Vec<u8>,
    payload: *mut c_void,
}

unsafe extern "C" fn scan_collect(payload: *mut c_void, data: *mut c_void, name: *const xml::xmlChar) {
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
    unsafe { xml::xmlHashScan(table, Some(scan_collect), &mut entries as *mut _ as *mut c_void) };
    entries
}

/// A Hash of name → wrapped node for the nodes of a DTD table
/// (`element_copier`).
fn node_table(vm: &mut Executor, globals: &mut Globals, table: *mut c_void) -> Result<Value> {
    // SAFETY: a live table of declaration nodes.
    let entries = unsafe { scan(table) };
    let hash = Value::hash_with_capacity(entries.len());
    // Wrapping runs Ruby (`decorate`): keep the hash rooted meanwhile.
    let len = vm.temp_len();
    vm.temp_push(hash);
    let mut fill = || -> Result<()> {
        for e in &entries {
            let node = wrap_node(vm, globals, e.payload as *mut xml::xmlNode)?;
            hash.as_hash_mut(&globals.store)?.insert(utf8(&e.name), node, vm, globals)?;
        }
        Ok(())
    };
    let r = fill();
    vm.temp_clear(len);
    r?;
    Ok(hash)
}

/// DTD#notations -> Hash of name → Notation | nil
#[monoruby_builtin]
fn notations(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let dtd = dtd(lfp)?;
    // SAFETY: a live DTD.
    let table = unsafe { (*dtd).notations };
    if table.is_null() {
        return Ok(Value::nil());
    }
    // SAFETY: a live table of `xmlNotation`s.
    let entries = unsafe { scan(table) };
    let klass = globals
        .store
        .get_constant_noautoload(classes().xml, IdentId::get_id("Notation"))
        .ok_or_else(|| MonorubyErr::nameerr("uninitialized constant Nokogiri::XML::Notation"))?;
    let hash = Value::hash_with_capacity(entries.len());
    let len = vm.temp_len();
    vm.temp_push(hash);
    let mut fill = || -> Result<()> {
        for e in &entries {
            let n = e.payload as *const xml::xmlNotation;
            // SAFETY: a live notation.
            let args = unsafe { [xml_str((*n).name), xml_str((*n).PublicID), xml_str((*n).SystemID)] };
            let notation = vm.invoke_method_inner(globals, IdentId::NEW, klass, &args, None, None)?;
            hash.as_hash_mut(&globals.store)?.insert(utf8(&e.name), notation, vm, globals)?;
        }
        Ok(())
    };
    let r = fill();
    vm.temp_clear(len);
    r?;
    Ok(hash)
}

/// DTD#elements -> Hash of name → ElementDecl | nil
#[monoruby_builtin]
fn elements(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let dtd = dtd(lfp)?;
    // SAFETY: a live DTD.
    let table = unsafe { (*dtd).elements };
    if table.is_null() {
        return Ok(Value::nil());
    }
    node_table(vm, globals, table)
}

/// DTD#entities -> Hash of name → EntityDecl | nil
#[monoruby_builtin]
fn entities(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let dtd = dtd(lfp)?;
    // SAFETY: a live DTD.
    let table = unsafe { (*dtd).entities };
    if table.is_null() {
        return Ok(Value::nil());
    }
    node_table(vm, globals, table)
}

/// DTD#attributes -> Hash of name → AttributeDecl (empty when none)
#[monoruby_builtin]
fn attributes(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let dtd = dtd(lfp)?;
    // SAFETY: a live DTD.
    let table = unsafe { (*dtd).attributes };
    if table.is_null() {
        return Ok(Value::hash_with_capacity(0));
    }
    node_table(vm, globals, table)
}

/// DTD#validate(document) -> Array of SyntaxError
#[monoruby_builtin]
fn validate(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let dtd = dtd(lfp)?;
    let doc = doc_ptr(lfp.arg(0))?;
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
    errors_to_array(vm, globals, &errors)
}

/// DTD#system_id -> String | nil
#[monoruby_builtin]
fn dtd_system_id(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let dtd = dtd(lfp)?;
    // SAFETY: a live DTD.
    Ok(unsafe { xml_str((*dtd).SystemID) })
}

/// DTD#external_id -> String | nil
#[monoruby_builtin]
fn dtd_external_id(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let dtd = dtd(lfp)?;
    // SAFETY: a live DTD.
    Ok(unsafe { xml_str((*dtd).ExternalID) })
}

// ---- EntityDecl ----

fn entity(lfp: Lfp) -> Result<*mut xml::xmlEntity> {
    Ok(node_ptr(lfp.self_val())? as *mut xml::xmlEntity)
}

/// EntityDecl#original_content -> String | nil
#[monoruby_builtin]
fn original_content(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let e = entity(lfp)?;
    // SAFETY: a live entity declaration.
    Ok(unsafe { xml_str((*e).orig) })
}

/// EntityDecl#content -> String | nil
#[monoruby_builtin]
fn entity_content(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let e = entity(lfp)?;
    // SAFETY: a live entity declaration; `content` has `length` bytes.
    unsafe {
        if (*e).content.is_null() {
            return Ok(Value::nil());
        }
        let bytes = std::slice::from_raw_parts((*e).content, (*e).length as usize);
        Ok(utf8(bytes))
    }
}

/// EntityDecl#entity_type -> Integer
#[monoruby_builtin]
fn entity_type(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let e = entity(lfp)?;
    // SAFETY: a live entity declaration.
    Ok(Value::integer(unsafe { (*e).etype } as i64))
}

/// EntityDecl#external_id -> String | nil
#[monoruby_builtin]
fn entity_external_id(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let e = entity(lfp)?;
    // SAFETY: a live entity declaration.
    Ok(unsafe { xml_str((*e).ExternalID) })
}

/// EntityDecl#system_id -> String | nil
#[monoruby_builtin]
fn entity_system_id(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let e = entity(lfp)?;
    // SAFETY: a live entity declaration.
    Ok(unsafe { xml_str((*e).SystemID) })
}

// ---- ElementDecl ----

fn element(lfp: Lfp) -> Result<*mut xml::xmlElement> {
    Ok(node_ptr(lfp.self_val())? as *mut xml::xmlElement)
}

/// ElementDecl#element_type -> Integer
#[monoruby_builtin]
fn element_type(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let e = element(lfp)?;
    // SAFETY: a live element declaration.
    Ok(Value::integer(unsafe { (*e).etype } as i64))
}

/// ElementDecl#content -> ElementContent | nil
#[monoruby_builtin]
fn element_content(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let e = element(lfp)?;
    // SAFETY: a live element declaration.
    let content = unsafe { (*e).content };
    if content.is_null() {
        return Ok(Value::nil());
    }
    let document = vm.invoke_method_inner(globals, IdentId::get_id("document"), lfp.self_val(), &[], None, None)?;
    wrap_element_content(globals, document, content)
}

/// ElementDecl#prefix -> String | nil
#[monoruby_builtin]
fn element_prefix(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let e = element(lfp)?;
    // SAFETY: a live element declaration.
    Ok(unsafe { xml_str((*e).prefix) })
}

// ---- AttributeDecl ----

fn attribute(lfp: Lfp) -> Result<*mut xml::xmlAttribute> {
    Ok(node_ptr(lfp.self_val())? as *mut xml::xmlAttribute)
}

/// AttributeDecl#attribute_type -> Integer
#[monoruby_builtin]
fn attribute_type(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let a = attribute(lfp)?;
    // SAFETY: a live attribute declaration.
    Ok(Value::integer(unsafe { (*a).atype } as i64))
}

/// AttributeDecl#default -> String | nil
#[monoruby_builtin]
fn attribute_default(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let a = attribute(lfp)?;
    // SAFETY: a live attribute declaration.
    Ok(unsafe { xml_str((*a).defaultValue) })
}

/// AttributeDecl#enumeration -> Array of String
#[monoruby_builtin]
fn enumeration(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let a = attribute(lfp)?;
    let mut list = vec![];
    // SAFETY: a live attribute declaration with a NULL-terminated
    // enumeration list.
    unsafe {
        let mut e = (*a).tree;
        while !e.is_null() {
            list.push(xml_str((*e).name));
            e = (*e).next;
        }
    }
    Ok(Value::array_from_vec(list))
}

// ---- ElementContent ----

/// `noko_xml_element_content_wrap`: a new `ElementContent` each time,
/// holding its document in `@document`.
fn wrap_element_content(
    globals: &mut Globals,
    document: Value,
    content: *mut xml::xmlElementContent,
) -> Result<Value> {
    let rb = Value::new_native(classes().element_content, Box::new(XmlElementContent { content }));
    globals.store.set_ivar(rb, IdentId::get_id("@document"), document)?;
    Ok(rb)
}

fn content(lfp: Lfp) -> Result<*mut xml::xmlElementContent> {
    match lfp.self_val().try_native::<XmlElementContent>() {
        Some(c) => Ok(c.content),
        None => Err(MonorubyErr::argumenterr("expected a Nokogiri::XML::ElementContent")),
    }
}

/// ElementContent#name -> String | nil
#[monoruby_builtin]
fn content_name(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let c = content(lfp)?;
    // SAFETY: a live content node.
    Ok(unsafe { xml_str((*c).name) })
}

/// ElementContent#type -> Integer
#[monoruby_builtin]
fn content_type(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let c = content(lfp)?;
    // SAFETY: a live content node.
    Ok(Value::integer(unsafe { (*c).type_ } as i64))
}

/// ElementContent#occur -> Integer
#[monoruby_builtin]
fn content_occur(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let c = content(lfp)?;
    // SAFETY: a live content node.
    Ok(Value::integer(unsafe { (*c).ocur } as i64))
}

/// ElementContent#prefix -> String | nil
#[monoruby_builtin]
fn content_prefix(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let c = content(lfp)?;
    // SAFETY: a live content node.
    Ok(unsafe { xml_str((*c).prefix) })
}

fn content_child(globals: &mut Globals, lfp: Lfp, second: bool) -> Result<Value> {
    let c = content(lfp)?;
    // SAFETY: a live content node.
    let child = unsafe { if second { (*c).c2 } else { (*c).c1 } };
    if child.is_null() {
        return Ok(Value::nil());
    }
    let document = globals
        .store
        .get_ivar(lfp.self_val(), IdentId::get_id("@document"))
        .unwrap_or_default();
    wrap_element_content(globals, document, child)
}

/// ElementContent#c1 -> ElementContent | nil (private)
#[monoruby_builtin]
fn content_c1(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    content_child(globals, lfp, false)
}

/// ElementContent#c2 -> ElementContent | nil (private)
#[monoruby_builtin]
fn content_c2(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    content_child(globals, lfp, true)
}

// ---- Document#create_entity ----

/// Document#create_entity(name, type = INTERNAL_GENERAL, external_id = nil,
/// system_id = nil, content = nil) -> EntityDecl
#[monoruby_builtin]
fn create_entity(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let doc = doc_ptr(lfp.self_val())?;
    let args: Vec<Value> = lfp.arg(0).as_array().iter().cloned().collect();
    if args.is_empty() || args.len() > 5 {
        return Err(MonorubyErr::argumenterr(format!(
            "wrong number of arguments (given {}, expected 1..5)",
            args.len()
        )));
    }
    let arg = |i: usize| args.get(i).copied().unwrap_or_default();
    let name = opt_cstr(arg(0), &globals.store)?;
    let ty = if arg(1).is_nil() {
        xml::XML_INTERNAL_GENERAL_ENTITY
    } else {
        arg(1).expect_integer(&globals.store)? as c_int
    };
    let external_id = opt_cstr(arg(2), &globals.store)?;
    let system_id = opt_cstr(arg(3), &globals.store)?;
    let content = opt_cstr(arg(4), &globals.store)?;
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: a live document; NUL-terminated strings or NULL; the
    // structured handler is registered for this call only.
    let entity = unsafe {
        xml::xmlSetStructuredErrorFunc(&mut errors as *mut _ as *mut c_void, Some(collect_error));
        let e = xml::xmlAddDocEntity(doc, cptr(&name), ty, cptr(&external_id), cptr(&system_id), cptr(&content));
        xml::xmlSetStructuredErrorFunc(std::ptr::null_mut(), None);
        e
    };
    if entity.is_null() {
        let rb_errors = errors_to_array(vm, globals, &errors)?;
        let klass = globals.store.get_module(classes().xml_syntax_error).as_val();
        let ex = vm.invoke_method_inner(globals, IdentId::get_id("aggregate"), klass, &[rb_errors], None, None)?;
        return Err(if ex.as_bool() { raise(ex) } else { MonorubyErr::runtimeerr("Could not create entity") });
    }
    wrap_node_as(vm, globals, Some(classes().entity_decl), entity as *mut xml::xmlNode)
}
