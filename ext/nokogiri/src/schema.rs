//! `Nokogiri::XML::Schema` (XML Schema, `xml_schema.c`) and
//! `Nokogiri::XML::RelaxNG` (`xml_relax_ng.c`): a compiled schema owned
//! by the Ruby object, `from_document` to compile one and the private
//! `validate_document` / `validate_file` the gem's `validate` calls,
//! answering an array of `SyntaxError`s.

use crate::*;

/// The payload of a `Schema` / `RelaxNG`.
enum XmlSchema {
    Xsd(*mut xml::xmlSchema),
    RelaxNg(*mut xml::xmlRelaxNG),
}

native!(XmlSchema, "XmlSchema");

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

pub(crate) fn init(ctx: &mut Ctx, c: &Classes) {
    ctx.define_method(
        c.schema,
        "from_document",
        method!(schema_from_document),
        MR_ARGC_VARIADIC,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(
        c.schema,
        "validate_document",
        method!(schema_validate_document),
        1,
        MR_METHOD_PRIVATE,
    );
    ctx.define_method(
        c.schema,
        "validate_file",
        method!(schema_validate_file),
        1,
        MR_METHOD_PRIVATE,
    );
    ctx.define_method(
        c.relax_ng,
        "from_document",
        method!(relax_ng_from_document),
        MR_ARGC_VARIADIC,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(
        c.relax_ng,
        "validate_document",
        method!(relax_ng_validate_document),
        1,
        MR_METHOD_PRIVATE,
    );
}

fn xsd(ctx: &mut Ctx, v: Value) -> Result<*mut xml::xmlSchema> {
    match ctx.native::<XmlSchema>(v) {
        Some(XmlSchema::Xsd(s)) => Ok(*s),
        _ => Err(ctx.argument_error("expected a Nokogiri::XML::Schema")),
    }
}

fn relax_ng(ctx: &mut Ctx, v: Value) -> Result<*mut xml::xmlRelaxNG> {
    match ctx.native::<XmlSchema>(v) {
        Some(XmlSchema::RelaxNg(s)) => Ok(*s),
        _ => Err(ctx.argument_error("expected a Nokogiri::XML::RelaxNG")),
    }
}

/// `(document, parse_options = DEFAULT_SCHEMA)` of `from_document`: the
/// `xmlDoc` (a Node's document is accepted, as nokogiri does), the
/// options object and its integer value.
fn from_document_args(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
) -> Result<(*mut xml::xmlDoc, Value, c_int)> {
    let args: Vec<Value> = args.to_vec();
    if args.is_empty() || args.len() > 2 {
        return Err(ctx.argument_error(format!(
            "wrong number of arguments (given {}, expected 1..2)",
            args.len()
        )));
    }
    let document = args[0];
    if !is_node(ctx, document) {
        return Err(ctx.type_error(format!(
            "expected parameter to be a Nokogiri::XML::Document, received {}",
            ctx.class_name(document)
        )));
    }
    // SAFETY: a live node or document.
    let doc = unsafe { (*node_ptr(ctx, document)?).doc };
    let mut options = args.get(1).copied().unwrap_or_default();
    if options.is_nil() {
        let parse_options = ctx
            .const_get(classes().xml, "ParseOptions")
            .ok_or_else(|| name_error(ctx, "uninitialized constant Nokogiri::XML::ParseOptions"))?;
        options = ctx
            .const_get(parse_options, "DEFAULT_SCHEMA")
            .ok_or_else(|| {
                name_error(
                    ctx,
                    "uninitialized constant Nokogiri::XML::ParseOptions::DEFAULT_SCHEMA",
                )
            })?;
    }
    let to_i = ctx.funcall(options, "to_i", &[], None)?;
    let c_options = ctx.int(to_i)? as c_int;
    Ok((doc, options, c_options))
}

/// Whether a blank text node of `doc` has a Ruby object: the schema
/// parser would strip it from under the wrapper, so the schema is then
/// compiled from a copy (`noko_xml_document_has_wrapped_blank_nodes_p`,
/// nokogiri #2001).
pub(crate) unsafe fn has_wrapped_blank_nodes(ctx: &Ctx, doc: *mut xml::xmlDoc) -> bool {
    // SAFETY: a live document; every cached wrapper's node is live.
    unsafe {
        let Some(d) = doc_native(ctx, doc) else {
            return false;
        };
        d.node_cache
            .iter()
            .any(|v| match ctx.native::<XmlNode>(*v) {
                Some(n) => xml::xmlIsBlankNode(n.node) != 0,
                None => false,
            })
    }
}

/// The tail of `from_document`: wrap the compiled schema (`@errors`,
/// `@parse_options`), or raise the collected errors.
fn finish(
    ctx: &mut Ctx,
    class: Value,
    schema: Option<XmlSchema>,
    errors: &[ErrorRecord],
    options: Value,
) -> Result<Value> {
    let rb_errors = errors_to_array(ctx, errors)?;
    let Some(schema) = schema else {
        let klass = classes().xml_syntax_error;
        let ex = ctx.funcall(klass, "aggregate", &[rb_errors], None)?;
        return Err(if ex.truthy() {
            raise(ctx, ex)
        } else {
            ctx.runtime_error("Could not parse document")
        });
    };
    let rb = ctx.native_new(class, schema)?;
    ctx.ivar_set(rb, "@errors", rb_errors)?;
    ctx.ivar_set(rb, "@parse_options", options)?;
    Ok(rb)
}

/// Schema.from_document(document, parse_options = DEFAULT_SCHEMA) -> Schema
fn schema_from_document(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let (doc, options, c_options) = from_document_args(ctx, this, args)?;
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: a live document; the copy (if any), the parser context and
    // the handlers are ours for the call; the entity loader is restored.
    let schema = unsafe {
        let copied = has_wrapped_blank_nodes(ctx, doc);
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
        if schema.is_null() {
            None
        } else {
            Some(XmlSchema::Xsd(schema))
        }
    };
    finish(ctx, class, schema, &errors, options)
}

/// RelaxNG.from_document(document, parse_options = DEFAULT_SCHEMA) -> RelaxNG
fn relax_ng_from_document(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let class = this;
    // Only a Document (`noko_xml_document_unwrap`'s check).
    if let Some(&document) = args.first()
        && ctx.native::<XmlDocument>(document).is_none()
    {
        return Err(ctx.type_error(format!(
            "wrong argument type {} (expected xmlDoc)",
            builtin_type_name(ctx, document)
        )));
    }
    let (doc, options, _) = from_document_args(ctx, this, args)?;
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
        if schema.is_null() {
            None
        } else {
            Some(XmlSchema::RelaxNg(schema))
        }
    };
    finish(ctx, class, schema, &errors, options)
}

/// The validation errors as an array, with `fallback` when the
/// validation failed without reporting any.
fn validation_result(
    ctx: &mut Ctx,
    errors: &[ErrorRecord],
    status: c_int,
    fallback: Option<&str>,
) -> Result<Value> {
    let rb_errors = errors_to_array(ctx, errors)?;
    if status != 0 && errors.is_empty() {
        if let Some(msg) = fallback {
            ctx.ary_push(rb_errors, ctx.str(msg))?;
        }
    }
    Ok(rb_errors)
}

/// Schema#validate_document(document) -> Array of SyntaxError
fn schema_validate_document(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let schema = xsd(ctx, this)?;
    let doc = doc_ptr(ctx, args[0])?;
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: a live schema and document; the context is ours.
    let status = unsafe {
        let ctxt = xml::xmlSchemaNewValidCtxt(schema);
        if ctxt.is_null() {
            return Err(ctx.runtime_error("Could not create a validation context"));
        }
        xml::xmlSchemaSetValidStructuredErrors(
            ctxt,
            Some(collect_error),
            &mut errors as *mut _ as *mut c_void,
        );
        let status = xml::xmlSchemaValidateDoc(ctxt, doc);
        xml::xmlSchemaFreeValidCtxt(ctxt);
        status
    };
    validation_result(ctx, &errors, status, Some("Could not validate document"))
}

/// Schema#validate_file(filename) -> Array of SyntaxError
fn schema_validate_file(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let schema = xsd(ctx, this)?;
    let filename = cstr(args[0], ctx)?;
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: a live schema, a NUL-terminated path; the context is ours.
    let status = unsafe {
        let ctxt = xml::xmlSchemaNewValidCtxt(schema);
        if ctxt.is_null() {
            return Err(ctx.runtime_error("Could not create a validation context"));
        }
        xml::xmlSchemaSetValidStructuredErrors(
            ctxt,
            Some(collect_error),
            &mut errors as *mut _ as *mut c_void,
        );
        let status = xml::xmlSchemaValidateFile(ctxt, filename.as_ptr(), 0);
        xml::xmlSchemaFreeValidCtxt(ctxt);
        status
    };
    validation_result(ctx, &errors, status, Some("Could not validate file."))
}

/// RelaxNG#validate_document(document) -> Array of SyntaxError
fn relax_ng_validate_document(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let schema = relax_ng(ctx, this)?;
    let doc = doc_ptr(ctx, args[0])?;
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: as `schema_validate_document`.
    let status = unsafe {
        let ctxt = xml::xmlRelaxNGNewValidCtxt(schema);
        if ctxt.is_null() {
            return Err(ctx.runtime_error("Could not create a validation context"));
        }
        xml::xmlRelaxNGSetValidStructuredErrors(
            ctxt,
            Some(collect_error),
            &mut errors as *mut _ as *mut c_void,
        );
        let status = xml::xmlRelaxNGValidateDoc(ctxt, doc);
        xml::xmlRelaxNGFreeValidCtxt(ctxt);
        status
    };
    validation_result(ctx, &errors, status, None)
}
