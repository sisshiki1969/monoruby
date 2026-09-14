//! `Nokogiri::EncodingHandler` (libxml2's character encoding handlers
//! and aliases), `Nokogiri::HTML4::EntityLookup` and
//! `Nokogiri::HTML4::ElementDescription` (libxml2's static HTML element
//! table, `html4_element_description.c`).

use super::*;

/// The payload of an `EncodingHandler`.
struct EncodingHandler {
    handler: *mut xml::xmlCharEncodingHandler,
}

impl NativeData for EncodingHandler {
    fn mark(&self, _alloc: &mut alloc::Allocator<RValue>) {}
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl Drop for EncodingHandler {
    fn drop(&mut self) {
        // SAFETY: a handler `xmlFindCharEncodingHandler` handed out.
        unsafe { xml::xmlCharEncCloseFunc(self.handler) };
    }
}

pub(super) fn init(globals: &mut Globals, c: &Classes) {
    let e = c.encoding_handler;
    globals.define_builtin_class_func(e, "[]", encoding_handler_get, 1);
    globals.define_builtin_class_func(e, "delete", encoding_handler_delete, 1);
    globals.define_builtin_class_func(e, "alias", encoding_handler_alias, 2);
    globals.define_builtin_class_func(e, "clear_aliases!", encoding_handler_clear_aliases, 0);
    globals.define_builtin_func(e, "name", encoding_handler_name, 0);
    globals.define_builtin_func(c.entity_lookup, "get", entity_lookup_get, 1);

    let d = c.element_description;
    globals.define_builtin_class_func(d, "[]", element_description_get, 1);
    globals.define_builtin_func(d, "name", desc_name, 0);
    globals.define_builtin_func(d, "implied_start_tag?", desc_implied_start_tag, 0);
    globals.define_builtin_func(d, "implied_end_tag?", desc_implied_end_tag, 0);
    globals.define_builtin_func(d, "save_end_tag?", desc_save_end_tag, 0);
    globals.define_builtin_func(d, "empty?", desc_empty, 0);
    globals.define_builtin_func(d, "deprecated?", desc_deprecated, 0);
    globals.define_builtin_func(d, "inline?", desc_inline, 0);
    globals.define_builtin_func(d, "description", desc_description, 0);
    globals.define_builtin_func(d, "sub_elements", desc_sub_elements, 0);
    globals.define_builtin_func(d, "default_sub_element", desc_default_sub_element, 0);
    globals.define_builtin_func(d, "optional_attributes", desc_optional_attributes, 0);
    globals.define_builtin_func(d, "deprecated_attributes", desc_deprecated_attributes, 0);
    globals.define_builtin_func(d, "required_attributes", desc_required_attributes, 0);
}

/// The payload of an `ElementDescription`: an entry of libxml2's static
/// element table (never freed).
struct ElementDescription {
    desc: *const xml::htmlElemDesc,
}

impl NativeData for ElementDescription {
    fn mark(&self, _alloc: &mut alloc::Allocator<RValue>) {}
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

fn desc(lfp: Lfp) -> Result<*const xml::htmlElemDesc> {
    match lfp.self_val().try_native::<ElementDescription>() {
        Some(d) => Ok(d.desc),
        None => Err(MonorubyErr::typeerr("expected a Nokogiri::HTML4::ElementDescription")),
    }
}

/// An Array of the strings of a NULL-terminated list (empty for NULL).
unsafe fn string_list(list: *const *const c_char) -> Value {
    let mut out = vec![];
    if !list.is_null() {
        // SAFETY: a NULL-terminated list of NUL-terminated static strings.
        unsafe {
            let mut i = 0;
            while !(*list.add(i)).is_null() {
                out.push(xml_str(*list.add(i) as *const xml::xmlChar));
                i += 1;
            }
        }
    }
    Value::array_from_vec(out)
}

/// ElementDescription[name] -> ElementDescription | nil
#[monoruby_builtin]
fn element_description_get(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let name = cstr(lfp.arg(0), &globals.store)?;
    // SAFETY: a NUL-terminated name; the description is static.
    let desc = unsafe { xml::htmlTagLookup(name.as_ptr() as *const xml::xmlChar) };
    if desc.is_null() {
        return Ok(Value::nil());
    }
    Ok(Value::new_native(class, Box::new(ElementDescription { desc })))
}

/// ElementDescription#name -> String | nil
#[monoruby_builtin]
fn desc_name(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let d = desc(lfp)?;
    // SAFETY: a static description.
    Ok(unsafe { xml_str((*d).name as *const xml::xmlChar) })
}

/// ElementDescription#description -> String
#[monoruby_builtin]
fn desc_description(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let d = desc(lfp)?;
    // SAFETY: a static description.
    Ok(unsafe { xml_str((*d).desc as *const xml::xmlChar) })
}

macro_rules! desc_flag {
    ($name:ident, $field:ident) => {
        #[monoruby_builtin]
        fn $name(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
            let d = desc(lfp)?;
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
#[monoruby_builtin]
fn desc_sub_elements(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let d = desc(lfp)?;
    // SAFETY: a static description.
    Ok(unsafe { string_list((*d).subelts) })
}

/// ElementDescription#default_sub_element -> String | nil
#[monoruby_builtin]
fn desc_default_sub_element(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let d = desc(lfp)?;
    // SAFETY: a static description.
    Ok(unsafe { xml_str((*d).defaultsubelt as *const xml::xmlChar) })
}

/// ElementDescription#optional_attributes -> Array of String
#[monoruby_builtin]
fn desc_optional_attributes(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let d = desc(lfp)?;
    // SAFETY: a static description.
    Ok(unsafe { string_list((*d).attrs_opt) })
}

/// ElementDescription#deprecated_attributes -> Array of String
#[monoruby_builtin]
fn desc_deprecated_attributes(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let d = desc(lfp)?;
    // SAFETY: a static description.
    Ok(unsafe { string_list((*d).attrs_depr) })
}

/// ElementDescription#required_attributes -> Array of String. Nokogiri
/// walks `attrs_req` for as long as `attrs_depr` has entries (a slip in
/// `required_attributes`); reproduced, bounded by both lists, so the
/// answers match.
#[monoruby_builtin]
fn desc_required_attributes(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let d = desc(lfp)?;
    let mut out = vec![];
    // SAFETY: a static description with NULL-terminated lists.
    unsafe {
        let req = (*d).attrs_req;
        let depr = (*d).attrs_depr;
        if !req.is_null() && !depr.is_null() {
            let mut i = 0;
            while !(*depr.add(i)).is_null() && !(*req.add(i)).is_null() {
                out.push(xml_str(*req.add(i) as *const xml::xmlChar));
                i += 1;
            }
        }
    }
    Ok(Value::array_from_vec(out))
}

/// EncodingHandler[name] -> EncodingHandler | nil
#[monoruby_builtin]
fn encoding_handler_get(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let name = cstr(lfp.arg(0), &globals.store)?;
    // SAFETY: a NUL-terminated name.
    let handler = unsafe { xml::xmlFindCharEncodingHandler(name.as_ptr()) };
    if handler.is_null() {
        return Ok(Value::nil());
    }
    Ok(Value::new_native(class, Box::new(EncodingHandler { handler })))
}

/// EncodingHandler.delete(name) -> true | nil
#[monoruby_builtin]
fn encoding_handler_delete(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let name = cstr(lfp.arg(0), &globals.store)?;
    // SAFETY: a NUL-terminated name.
    if unsafe { xml::xmlDelEncodingAlias(name.as_ptr()) } != 0 {
        return Ok(Value::nil());
    }
    Ok(Value::bool(true))
}

/// EncodingHandler.alias(name, alias) -> alias
#[monoruby_builtin]
fn encoding_handler_alias(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let from = cstr(lfp.arg(0), &globals.store)?;
    let to = cstr(lfp.arg(1), &globals.store)?;
    // SAFETY: NUL-terminated names; libxml2 copies them.
    unsafe { xml::xmlAddEncodingAlias(from.as_ptr(), to.as_ptr()) };
    Ok(lfp.arg(1))
}

/// EncodingHandler.clear_aliases! -> self
#[monoruby_builtin]
fn encoding_handler_clear_aliases(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // SAFETY: plain library call.
    unsafe { xml::xmlCleanupEncodingAliases() };
    Ok(lfp.self_val())
}

/// EncodingHandler#name -> String
#[monoruby_builtin]
fn encoding_handler_name(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let Some(h) = self_val.try_native::<EncodingHandler>() else {
        return Err(MonorubyErr::typeerr("expected a Nokogiri::EncodingHandler"));
    };
    // SAFETY: a live handler with a NUL-terminated name.
    Ok(unsafe { xml_str((*h.handler).name as *const xml::xmlChar) })
}

/// EntityLookup#get(name) -> HTML4::EntityDescription | nil
#[monoruby_builtin]
fn entity_lookup_get(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let name = cstr(lfp.arg(0), &globals.store)?;
    // SAFETY: a NUL-terminated name; the description is static.
    let desc = unsafe { xml::htmlEntityLookup(name.as_ptr() as *const xml::xmlChar) };
    if desc.is_null() {
        return Ok(Value::nil());
    }
    // SAFETY: a static entity description.
    let args = unsafe {
        [
            Value::integer((*desc).value as i64),
            xml_str((*desc).name as *const xml::xmlChar),
            xml_str((*desc).desc as *const xml::xmlChar),
        ]
    };
    let klass = globals
        .store
        .get_constant_noautoload(classes().html4, IdentId::get_id("EntityDescription"))
        .ok_or_else(|| MonorubyErr::nameerr("uninitialized constant Nokogiri::HTML4::EntityDescription"))?;
    vm.invoke_method_inner(globals, IdentId::NEW, klass, &args, None, None)
}
