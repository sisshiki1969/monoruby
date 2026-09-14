//! `Nokogiri::XML::Schema` (XML Schema, `xml_schema.c`) and
//! `Nokogiri::XML::RelaxNG` (`xml_relax_ng.c`): a compiled schema owned
//! by the Ruby object, `from_document` to compile one and the private
//! `validate_document` / `validate_file` the gem's `validate` calls,
//! answering an array of `SyntaxError`s.

use super::*;

/// The payload of a `Schema` / `RelaxNG`.
enum XmlSchema {
    Xsd(*mut xml::xmlSchema),
    RelaxNg(*mut xml::xmlRelaxNG),
}

impl NativeData for XmlSchema {
    fn mark(&self, _alloc: &mut alloc::Allocator<RValue>) {}
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl Drop for XmlSchema {
    fn drop(&mut self) {
        // SAFETY: the compiled schema is ours.
        unsafe {
            match *self {
                XmlSchema::Xsd(s) => xml::xmlSchemaFree(s),
                XmlSchema::RelaxNg(s) => xml::xmlRelaxNGFree(s),
            }
        }
    }
}

pub(super) fn init(globals: &mut Globals, c: &Classes) {
    globals.define_builtin_class_func_rest(c.schema, "from_document", schema_from_document);
    globals.define_private_builtin_func(c.schema, "validate_document", schema_validate_document, 1);
    globals.define_private_builtin_func(c.schema, "validate_file", schema_validate_file, 1);
    globals.define_builtin_class_func_rest(c.relax_ng, "from_document", relax_ng_from_document);
    globals.define_private_builtin_func(c.relax_ng, "validate_document", relax_ng_validate_document, 1);
}

fn xsd(v: Value) -> Result<*mut xml::xmlSchema> {
    match v.try_native::<XmlSchema>() {
        Some(XmlSchema::Xsd(s)) => Ok(*s),
        _ => Err(MonorubyErr::argumenterr("expected a Nokogiri::XML::Schema")),
    }
}

fn relax_ng(v: Value) -> Result<*mut xml::xmlRelaxNG> {
    match v.try_native::<XmlSchema>() {
        Some(XmlSchema::RelaxNg(s)) => Ok(*s),
        _ => Err(MonorubyErr::argumenterr("expected a Nokogiri::XML::RelaxNG")),
    }
}

/// `(document, parse_options = DEFAULT_SCHEMA)` of `from_document`: the
/// `xmlDoc` (a Node's document is accepted, as nokogiri does), the
/// options object and its integer value.
fn from_document_args(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<(*mut xml::xmlDoc, Value, c_int)> {
    let args: Vec<Value> = lfp.arg(0).as_array().iter().cloned().collect();
    if args.is_empty() || args.len() > 2 {
        return Err(MonorubyErr::argumenterr(format!(
            "wrong number of arguments (given {}, expected 1..2)",
            args.len()
        )));
    }
    let document = args[0];
    if !is_node(document) {
        return Err(MonorubyErr::typeerr(format!(
            "expected parameter to be a Nokogiri::XML::Document, received {}",
            globals.store.get_class_name(document.class())
        )));
    }
    // SAFETY: a live node or document.
    let doc = unsafe { (*node_ptr(document)?).doc };
    let mut options = args.get(1).copied().unwrap_or_default();
    if options.is_nil() {
        let parse_options = globals
            .store
            .get_constant_noautoload(classes().xml, IdentId::get_id("ParseOptions"))
            .ok_or_else(|| MonorubyErr::nameerr("uninitialized constant Nokogiri::XML::ParseOptions"))?;
        options = globals
            .store
            .get_constant_noautoload(parse_options.as_class_id(), IdentId::get_id("DEFAULT_SCHEMA"))
            .ok_or_else(|| MonorubyErr::nameerr("uninitialized constant Nokogiri::XML::ParseOptions::DEFAULT_SCHEMA"))?;
    }
    let to_i = vm.invoke_method_inner(globals, IdentId::get_id("to_i"), options, &[], None, None)?;
    let c_options = to_i.expect_integer(&globals.store)? as c_int;
    Ok((doc, options, c_options))
}

/// Whether a blank text node of `doc` has a Ruby object: the schema
/// parser would strip it from under the wrapper, so the schema is then
/// compiled from a copy (`noko_xml_document_has_wrapped_blank_nodes_p`,
/// nokogiri #2001).
pub(super) unsafe fn has_wrapped_blank_nodes(doc: *mut xml::xmlDoc) -> bool {
    // SAFETY: a live document; every cached wrapper's node is live.
    unsafe {
        let Some(d) = doc_native(doc) else {
            return false;
        };
        d.node_cache.iter().any(|v| match v.try_native::<XmlNode>() {
            Some(n) => xml::xmlIsBlankNode(n.node) != 0,
            None => false,
        })
    }
}

/// The tail of `from_document`: wrap the compiled schema (`@errors`,
/// `@parse_options`), or raise the collected errors.
fn finish(
    vm: &mut Executor,
    globals: &mut Globals,
    class: ClassId,
    schema: Option<XmlSchema>,
    errors: &[ErrorRecord],
    options: Value,
) -> Result<Value> {
    let rb_errors = errors_to_array(vm, globals, errors)?;
    let Some(schema) = schema else {
        let klass = globals.store.get_module(classes().xml_syntax_error).as_val();
        let ex = vm.invoke_method_inner(globals, IdentId::get_id("aggregate"), klass, &[rb_errors], None, None)?;
        return Err(if ex.as_bool() { raise(ex) } else { MonorubyErr::runtimeerr("Could not parse document") });
    };
    let rb = Value::new_native(class, Box::new(schema));
    globals.store.set_ivar(rb, IdentId::get_id("@errors"), rb_errors)?;
    globals.store.set_ivar(rb, IdentId::get_id("@parse_options"), options)?;
    Ok(rb)
}

/// Schema.from_document(document, parse_options = DEFAULT_SCHEMA) -> Schema
#[monoruby_builtin]
fn schema_from_document(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let (doc, options, c_options) = from_document_args(vm, globals, lfp)?;
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: a live document; the copy (if any), the parser context and
    // the handlers are ours for the call; the entity loader is restored.
    let schema = unsafe {
        let copied = has_wrapped_blank_nodes(doc);
        let doc = if copied { xml::xmlCopyDoc(doc, 1) } else { doc };
        let ctxt = xml::xmlSchemaNewDocParserCtxt(doc);
        let errs = &mut errors as *mut _ as *mut c_void;
        xml::xmlSetStructuredErrorFunc(errs, Some(collect_error));
        xml::xmlSchemaSetParserStructuredErrors(ctxt, Some(collect_error), errs);
        let saved_loader = if c_options & xml::XML_PARSE_NONET != 0 {
            let saved = xml::xmlGetExternalEntityLoader();
            xml::xmlSetExternalEntityLoader(Some(xml::xmlNoNetExternalEntityLoader));
            saved
        } else {
            None
        };
        let schema = xml::xmlSchemaParse(ctxt);
        if saved_loader.is_some() {
            xml::xmlSetExternalEntityLoader(saved_loader);
        }
        xml::xmlSchemaFreeParserCtxt(ctxt);
        xml::xmlSetStructuredErrorFunc(std::ptr::null_mut(), None);
        if copied {
            xml::xmlFreeDoc(doc);
        }
        if schema.is_null() { None } else { Some(XmlSchema::Xsd(schema)) }
    };
    finish(vm, globals, class, schema, &errors, options)
}

/// RelaxNG.from_document(document, parse_options = DEFAULT_SCHEMA) -> RelaxNG
#[monoruby_builtin]
fn relax_ng_from_document(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    // Only a Document (`noko_xml_document_unwrap`'s check).
    if let Some(document) = lfp.arg(0).as_array().first()
        && document.try_native::<XmlDocument>().is_none()
    {
        return Err(MonorubyErr::typeerr(format!(
            "wrong argument type {} (expected xmlDoc)",
            builtin_type_name(globals, *document)
        )));
    }
    let (doc, options, _) = from_document_args(vm, globals, lfp)?;
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: as `schema_from_document`.
    let schema = unsafe {
        let ctxt = xml::xmlRelaxNGNewDocParserCtxt(doc);
        let errs = &mut errors as *mut _ as *mut c_void;
        xml::xmlSetStructuredErrorFunc(errs, Some(collect_error));
        xml::xmlRelaxNGSetParserStructuredErrors(ctxt, Some(collect_error), errs);
        let schema = xml::xmlRelaxNGParse(ctxt);
        xml::xmlRelaxNGFreeParserCtxt(ctxt);
        xml::xmlSetStructuredErrorFunc(std::ptr::null_mut(), None);
        if schema.is_null() { None } else { Some(XmlSchema::RelaxNg(schema)) }
    };
    finish(vm, globals, class, schema, &errors, options)
}

/// The validation errors as an array, with `fallback` when the
/// validation failed without reporting any.
fn validation_result(
    vm: &mut Executor,
    globals: &mut Globals,
    errors: &[ErrorRecord],
    status: c_int,
    fallback: Option<&str>,
) -> Result<Value> {
    let rb_errors = errors_to_array(vm, globals, errors)?;
    if status != 0 && errors.is_empty() {
        if let Some(msg) = fallback {
            rb_errors.as_array_mut(&globals.store)?.push(Value::string_from_str(msg));
        }
    }
    Ok(rb_errors)
}

/// Schema#validate_document(document) -> Array of SyntaxError
#[monoruby_builtin]
fn schema_validate_document(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let schema = xsd(lfp.self_val())?;
    let doc = doc_ptr(lfp.arg(0))?;
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: a live schema and document; the context is ours.
    let status = unsafe {
        let ctxt = xml::xmlSchemaNewValidCtxt(schema);
        if ctxt.is_null() {
            return Err(MonorubyErr::runtimeerr("Could not create a validation context"));
        }
        xml::xmlSchemaSetValidStructuredErrors(ctxt, Some(collect_error), &mut errors as *mut _ as *mut c_void);
        let status = xml::xmlSchemaValidateDoc(ctxt, doc);
        xml::xmlSchemaFreeValidCtxt(ctxt);
        status
    };
    validation_result(vm, globals, &errors, status, Some("Could not validate document"))
}

/// Schema#validate_file(filename) -> Array of SyntaxError
#[monoruby_builtin]
fn schema_validate_file(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let schema = xsd(lfp.self_val())?;
    let filename = cstr(lfp.arg(0), &globals.store)?;
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: a live schema, a NUL-terminated path; the context is ours.
    let status = unsafe {
        let ctxt = xml::xmlSchemaNewValidCtxt(schema);
        if ctxt.is_null() {
            return Err(MonorubyErr::runtimeerr("Could not create a validation context"));
        }
        xml::xmlSchemaSetValidStructuredErrors(ctxt, Some(collect_error), &mut errors as *mut _ as *mut c_void);
        let status = xml::xmlSchemaValidateFile(ctxt, filename.as_ptr(), 0);
        xml::xmlSchemaFreeValidCtxt(ctxt);
        status
    };
    validation_result(vm, globals, &errors, status, Some("Could not validate file."))
}

/// RelaxNG#validate_document(document) -> Array of SyntaxError
#[monoruby_builtin]
fn relax_ng_validate_document(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let schema = relax_ng(lfp.self_val())?;
    let doc = doc_ptr(lfp.arg(0))?;
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: as `schema_validate_document`.
    let status = unsafe {
        let ctxt = xml::xmlRelaxNGNewValidCtxt(schema);
        if ctxt.is_null() {
            return Err(MonorubyErr::runtimeerr("Could not create a validation context"));
        }
        xml::xmlRelaxNGSetValidStructuredErrors(ctxt, Some(collect_error), &mut errors as *mut _ as *mut c_void);
        let status = xml::xmlRelaxNGValidateDoc(ctxt, doc);
        xml::xmlRelaxNGFreeValidCtxt(ctxt);
        status
    };
    validation_result(vm, globals, &errors, status, None)
}
