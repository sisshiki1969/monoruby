//! `Nokogiri::EncodingHandler` (libxml2's character encoding handlers
//! and aliases), `Nokogiri::HTML4::EntityLookup` and
//! `Nokogiri::HTML4::ElementDescription` (libxml2's static HTML element
//! table, `html4_element_description.c`).

use crate::*;

/// The payload of an `EncodingHandler`.
struct EncodingHandler {
    handler: *mut xml::xmlCharEncodingHandler,
}

native!(EncodingHandler, "EncodingHandler");

impl Drop for EncodingHandler {
    fn drop(&mut self) {
        // SAFETY: a handler `xmlFindCharEncodingHandler` handed out.
        unsafe { xml::xmlCharEncCloseFunc(self.handler) };
    }
}

pub(crate) fn init(ctx: &mut Ctx, c: &Classes) {
    let e = c.encoding_handler;
    ctx.define_method(
        e,
        "[]",
        method!(encoding_handler_get),
        1,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(
        e,
        "delete",
        method!(encoding_handler_delete),
        1,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(
        e,
        "alias",
        method!(encoding_handler_alias),
        2,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(
        e,
        "clear_aliases!",
        method!(encoding_handler_clear_aliases),
        0,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(e, "name", method!(encoding_handler_name), 0, 0);
    ctx.define_method(c.entity_lookup, "get", method!(entity_lookup_get), 1, 0);

    let d = c.element_description;
    ctx.define_method(
        d,
        "[]",
        method!(element_description_get),
        1,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(d, "name", method!(desc_name), 0, 0);
    ctx.define_method(
        d,
        "implied_start_tag?",
        method!(desc_implied_start_tag),
        0,
        0,
    );
    ctx.define_method(d, "implied_end_tag?", method!(desc_implied_end_tag), 0, 0);
    ctx.define_method(d, "save_end_tag?", method!(desc_save_end_tag), 0, 0);
    ctx.define_method(d, "empty?", method!(desc_empty), 0, 0);
    ctx.define_method(d, "deprecated?", method!(desc_deprecated), 0, 0);
    ctx.define_method(d, "inline?", method!(desc_inline), 0, 0);
    ctx.define_method(d, "description", method!(desc_description), 0, 0);
    ctx.define_method(d, "sub_elements", method!(desc_sub_elements), 0, 0);
    ctx.define_method(
        d,
        "default_sub_element",
        method!(desc_default_sub_element),
        0,
        0,
    );
    ctx.define_method(
        d,
        "optional_attributes",
        method!(desc_optional_attributes),
        0,
        0,
    );
    ctx.define_method(
        d,
        "deprecated_attributes",
        method!(desc_deprecated_attributes),
        0,
        0,
    );
    ctx.define_method(
        d,
        "required_attributes",
        method!(desc_required_attributes),
        0,
        0,
    );
}

/// The payload of an `ElementDescription`: an entry of libxml2's static
/// element table (never freed).
struct ElementDescription {
    desc: *const xml::htmlElemDesc,
}

native!(ElementDescription, "ElementDescription");

fn desc(ctx: &mut Ctx, this: Value) -> Result<*const xml::htmlElemDesc> {
    match ctx.native::<ElementDescription>(this) {
        Some(d) => Ok(d.desc),
        None => Err(ctx.type_error("expected a Nokogiri::HTML4::ElementDescription")),
    }
}

/// An Array of the strings of a NULL-terminated list (empty for NULL).
unsafe fn string_list(ctx: &Ctx, list: *const *const c_char) -> Value {
    let mut out = vec![];
    if !list.is_null() {
        // SAFETY: a NULL-terminated list of NUL-terminated static strings.
        unsafe {
            let mut i = 0;
            while !(*list.add(i)).is_null() {
                out.push(xml_str(ctx, *list.add(i) as *const xml::xmlChar));
                i += 1;
            }
        }
    }
    ctx.ary_from_vec(out)
}

/// ElementDescription[name] -> ElementDescription | nil
fn element_description_get(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let class = this;
    let name = cstr(args[0], ctx)?;
    // SAFETY: a NUL-terminated name; the description is static.
    let desc = unsafe { xml::htmlTagLookup(name.as_ptr() as *const xml::xmlChar) };
    if desc.is_null() {
        return Ok(Value::nil());
    }
    ctx.native_new(class, ElementDescription { desc })
}

/// ElementDescription#name -> String | nil
fn desc_name(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let d = desc(ctx, this)?;
    // SAFETY: a static description.
    Ok(unsafe { xml_str(ctx, (*d).name as *const xml::xmlChar) })
}

/// ElementDescription#description -> String
fn desc_description(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let d = desc(ctx, this)?;
    // SAFETY: a static description.
    Ok(unsafe { xml_str(ctx, (*d).desc as *const xml::xmlChar) })
}

macro_rules! desc_flag {
    ($name:ident, $field:ident) => {
        fn $name(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
            let d = desc(ctx, this)?;
            // SAFETY: a static description.
            Ok(Value::bool(unsafe { (*d).$field } != 0))
        }
    };
}

// ElementDescription#implied_start_tag? / #implied_end_tag? /
// #save_end_tag? / #empty? / #deprecated? / #inline?
desc_flag!(desc_implied_start_tag, startTag);
desc_flag!(desc_implied_end_tag, endTag);
desc_flag!(desc_save_end_tag, saveEndTag);
desc_flag!(desc_empty, empty);
desc_flag!(desc_deprecated, depr);
desc_flag!(desc_inline, isinline);

/// ElementDescription#sub_elements -> Array of String
fn desc_sub_elements(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let d = desc(ctx, this)?;
    // SAFETY: a static description.
    Ok(unsafe { string_list(ctx, (*d).subelts) })
}

/// ElementDescription#default_sub_element -> String | nil
fn desc_default_sub_element(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let d = desc(ctx, this)?;
    // SAFETY: a static description.
    Ok(unsafe { xml_str(ctx, (*d).defaultsubelt as *const xml::xmlChar) })
}

/// ElementDescription#optional_attributes -> Array of String
fn desc_optional_attributes(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let d = desc(ctx, this)?;
    // SAFETY: a static description.
    Ok(unsafe { string_list(ctx, (*d).attrs_opt) })
}

/// ElementDescription#deprecated_attributes -> Array of String
fn desc_deprecated_attributes(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let d = desc(ctx, this)?;
    // SAFETY: a static description.
    Ok(unsafe { string_list(ctx, (*d).attrs_depr) })
}

/// ElementDescription#required_attributes -> Array of String. Nokogiri
/// walks `attrs_req` for as long as `attrs_depr` has entries (a slip in
/// `required_attributes`); reproduced, bounded by both lists, so the
/// answers match.
fn desc_required_attributes(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let d = desc(ctx, this)?;
    let mut out = vec![];
    // SAFETY: a static description with NULL-terminated lists.
    unsafe {
        let req = (*d).attrs_req;
        let depr = (*d).attrs_depr;
        if !req.is_null() && !depr.is_null() {
            let mut i = 0;
            while !(*depr.add(i)).is_null() && !(*req.add(i)).is_null() {
                out.push(xml_str(ctx, *req.add(i) as *const xml::xmlChar));
                i += 1;
            }
        }
    }
    Ok(ctx.ary_from_vec(out))
}

/// EncodingHandler[name] -> EncodingHandler | nil
fn encoding_handler_get(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let class = this;
    let name = cstr(args[0], ctx)?;
    // SAFETY: a NUL-terminated name.
    let handler = unsafe { xml::xmlFindCharEncodingHandler(name.as_ptr()) };
    if handler.is_null() {
        return Ok(Value::nil());
    }
    ctx.native_new(class, EncodingHandler { handler })
}

/// EncodingHandler.delete(name) -> true | nil
fn encoding_handler_delete(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let name = cstr(args[0], ctx)?;
    // SAFETY: a NUL-terminated name.
    if unsafe { xml::xmlDelEncodingAlias(name.as_ptr()) } != 0 {
        return Ok(Value::nil());
    }
    Ok(Value::bool(true))
}

/// EncodingHandler.alias(name, alias) -> alias
fn encoding_handler_alias(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let from = cstr(args[0], ctx)?;
    let to = cstr(args[1], ctx)?;
    // SAFETY: NUL-terminated names; libxml2 copies them.
    unsafe { xml::xmlAddEncodingAlias(from.as_ptr(), to.as_ptr()) };
    Ok(args[1])
}

/// EncodingHandler.clear_aliases! -> self
fn encoding_handler_clear_aliases(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    // SAFETY: plain library call.
    unsafe { xml::xmlCleanupEncodingAliases() };
    Ok(this)
}

/// EncodingHandler#name -> String
fn encoding_handler_name(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    let self_val = this;
    let Some(h) = ctx.native::<EncodingHandler>(self_val) else {
        return Err(ctx.type_error("expected a Nokogiri::EncodingHandler"));
    };
    // SAFETY: a live handler with a NUL-terminated name.
    Ok(unsafe { xml_str(ctx, (*h.handler).name as *const xml::xmlChar) })
}

/// EntityLookup#get(name) -> HTML4::EntityDescription | nil
fn entity_lookup_get(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let name = cstr(args[0], ctx)?;
    // SAFETY: a NUL-terminated name; the description is static.
    let desc = unsafe { xml::htmlEntityLookup(name.as_ptr() as *const xml::xmlChar) };
    if desc.is_null() {
        return Ok(Value::nil());
    }
    // SAFETY: a static entity description.
    let args = unsafe {
        [
            Value::int((*desc).value as i64),
            xml_str(ctx, (*desc).name as *const xml::xmlChar),
            xml_str(ctx, (*desc).desc as *const xml::xmlChar),
        ]
    };
    let klass = ctx
        .const_get(classes().html4, "EntityDescription")
        .ok_or_else(|| {
            name_error(
                ctx,
                "uninitialized constant Nokogiri::HTML4::EntityDescription",
            )
        })?;
    ctx.funcall(klass, "new", &args, None)
}
