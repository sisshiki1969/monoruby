//! `Nokogiri::EncodingHandler` (libxml2's character encoding handlers
//! and aliases) and `Nokogiri::HTML4::EntityLookup`.

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
