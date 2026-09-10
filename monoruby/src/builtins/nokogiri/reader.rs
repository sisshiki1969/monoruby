//! `Nokogiri::XML::Reader`: a cursor over `xmlTextReader` (`xml_reader.c`).
//! The reader owns its `xmlTextReader`, the IO it pulls from (when made by
//! `from_io`) and the bytes of a `from_memory` input; parse errors are
//! collected into the Ruby `errors` array as they happen.

use super::*;

/// The payload of a `Reader`.
struct XmlReader {
    reader: *mut xml::xmlTextReader,
    io: Option<Box<IoCtx>>,
    /// The bytes of a `from_memory` input, read in place by the reader.
    _buffer: Vec<u8>,
}

impl NativeData for XmlReader {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        if let Some(io) = &self.io {
            io.io.mark(alloc);
        }
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl Drop for XmlReader {
    fn drop(&mut self) {
        // SAFETY: the reader is ours. Its document is freed separately:
        // `xmlTextReaderCurrentDoc` (called by `read`) marks it preserved,
        // so `xmlFreeTextReader` leaves it (`xml_reader_deallocate`).
        unsafe {
            let doc = xml::xmlTextReaderCurrentDoc(self.reader);
            xml::xmlFreeTextReader(self.reader);
            if !doc.is_null() {
                xml::xmlFreeDoc(doc);
            }
        }
    }
}

pub(super) fn init(globals: &mut Globals, c: &Classes) {
    let r = c.reader;
    globals.define_builtin_class_func_rest(r, "from_memory", from_memory);
    globals.define_builtin_class_func_rest(r, "from_io", from_io);
    globals.define_builtin_func(r, "attribute", attribute, 1);
    globals.define_builtin_func(r, "attribute_at", attribute_at, 1);
    globals.define_builtin_func(r, "attribute_count", attribute_count, 0);
    globals.define_builtin_func(r, "attribute_hash", attribute_hash, 0);
    globals.define_builtin_func(r, "attributes?", has_attributes_p, 0);
    globals.define_builtin_func(r, "base_uri", base_uri, 0);
    globals.define_builtin_func(r, "default?", is_default, 0);
    globals.define_builtin_func(r, "depth", depth, 0);
    globals.define_builtin_func(r, "empty_element?", empty_element, 0);
    globals.define_builtin_func(r, "encoding", encoding, 0);
    globals.define_builtin_func(r, "inner_xml", inner_xml, 0);
    globals.define_builtin_func(r, "lang", lang, 0);
    globals.define_builtin_func(r, "local_name", local_name, 0);
    globals.define_builtin_func(r, "name", name, 0);
    globals.define_builtin_func(r, "namespace_uri", namespace_uri, 0);
    globals.define_builtin_func(r, "namespaces", namespaces, 0);
    globals.define_builtin_func(r, "node_type", node_type, 0);
    globals.define_builtin_func(r, "outer_xml", outer_xml, 0);
    globals.define_builtin_func(r, "prefix", prefix, 0);
    globals.define_builtin_func(r, "read", read, 0);
    globals.define_builtin_func(r, "state", state, 0);
    globals.define_builtin_func(r, "value", value, 0);
    globals.define_builtin_func(r, "value?", has_value, 0);
    globals.define_builtin_func(r, "xml_version", xml_version, 0);
}

/// The reader of `self`, with the IO callbacks' executor pointers made
/// current (any call may pull more input).
fn this(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<*mut xml::xmlTextReader> {
    // SAFETY: the payload is ours for the call.
    let Some(r) = (unsafe { native_mut::<XmlReader>(lfp.self_val()) }) else {
        return Err(MonorubyErr::argumenterr("expected a Nokogiri::XML::Reader"));
    };
    if let Some(io) = &mut r.io {
        io.vm = vm;
        io.globals = globals;
    }
    Ok(r.reader)
}

/// `reader.errors` (the Ruby array the parse errors go to).
fn errors_of(vm: &mut Executor, globals: &mut Globals, reader: Value) -> Result<Value> {
    vm.invoke_method_inner(globals, IdentId::get_id("errors"), reader, &[], None, None)
}

/// Append the errors of a library call to `reader.errors`.
fn push_errors(vm: &mut Executor, globals: &mut Globals, reader: Value, errors: &[ErrorRecord]) -> Result<Value> {
    let rb_errors = errors_of(vm, globals, reader)?;
    if !errors.is_empty() {
        let new_errors = errors_to_array(vm, globals, errors)?;
        vm.invoke_method_inner(globals, IdentId::get_id("concat"), rb_errors, &[new_errors], None, None)?;
    }
    Ok(rb_errors)
}

/// Make a reader over `source` and `initialize(source, url, encoding)`
/// (`from_memory` / `from_io`); `make` builds the `xmlTextReader`.
fn construct(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    what: &str,
    make: impl FnOnce(&mut Globals, Value, Option<CString>, Option<CString>, c_int) -> Result<(*mut xml::xmlTextReader, Option<Box<IoCtx>>, Vec<u8>)>,
) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let args: Vec<Value> = lfp.arg(0).as_array().iter().cloned().collect();
    if args.is_empty() || args.len() > 4 {
        return Err(MonorubyErr::argumenterr(format!(
            "wrong number of arguments (given {}, expected 1..4)",
            args.len()
        )));
    }
    let arg = |i: usize| args.get(i).copied().unwrap_or_default();
    let source = arg(0);
    if !source.as_bool() {
        return Err(MonorubyErr::argumenterr(format!("{what} cannot be nil")));
    }
    let url = if arg(1).as_bool() { Some(cstr(arg(1), &globals.store)?) } else { None };
    let enc = if arg(2).as_bool() { Some(cstr(arg(2), &globals.store)?) } else { None };
    let options = if arg(3).as_bool() { arg(3).expect_integer(&globals.store)? as c_int } else { 0 };
    let (reader, io, buffer) = make(globals, source, url, enc, options)?;
    if reader.is_null() {
        return Err(MonorubyErr::runtimeerr("couldn't create a parser"));
    }
    let rb = Value::new_native(
        class,
        Box::new(XmlReader {
            reader,
            io,
            _buffer: buffer,
        }),
    );
    // `initialize` is private: call it as a function call; root the new
    // object across it.
    let len = vm.temp_len();
    vm.temp_push(rb);
    let r = vm.invoke_method_inner_vis(globals, IdentId::INITIALIZE, rb, &[source, arg(1), arg(2)], None, None, true);
    vm.temp_clear(len);
    r?;
    Ok(rb)
}

/// Reader.from_memory(string, url = nil, encoding = nil, options = 0) -> Reader
#[monoruby_builtin]
fn from_memory(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    construct(vm, globals, lfp, "string", |globals, source, url, enc, options| {
        let buffer = source.expect_bytes(&globals.store)?.to_vec();
        // SAFETY: the buffer moves into the payload, outliving the reader.
        let reader = unsafe {
            xml::xmlReaderForMemory(
                buffer.as_ptr() as *const c_char,
                buffer.len() as c_int,
                cptr(&url) as *const c_char,
                cptr(&enc) as *const c_char,
                options,
            )
        };
        Ok((reader, None, buffer))
    })
}

/// Reader.from_io(io, url = nil, encoding = nil, options = 0) -> Reader
#[monoruby_builtin]
fn from_io(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let vm_ptr: *mut Executor = vm;
    construct(vm, globals, lfp, "io", |globals, source, url, enc, options| {
        // SAFETY: the executor outlives the call; the IO context lives in
        // the payload, its pointers refreshed at each native call.
        let mut io = Box::new(IoCtx::new(unsafe { &mut *vm_ptr }, globals, source));
        let reader = unsafe {
            xml::xmlReaderForIO(
                Some(io_read),
                Some(io_close),
                &mut *io as *mut IoCtx as *mut c_void,
                cptr(&url) as *const c_char,
                cptr(&enc) as *const c_char,
                options,
            )
        };
        Ok((reader, Some(io), vec![]))
    })
}

/// Reader#read -> self | nil: advance; errors go to `errors`, and a
/// failed read raises their aggregate.
#[monoruby_builtin]
fn read(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let reader = this(vm, globals, lfp)?;
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: a live reader; the handler is registered for the call.
    let status = unsafe {
        xml::xmlSetStructuredErrorFunc(&mut errors as *mut _ as *mut c_void, Some(collect_error));
        let status = xml::xmlTextReaderRead(reader);
        xml::xmlSetStructuredErrorFunc(std::ptr::null_mut(), None);
        status
    };
    let rb_errors = push_errors(vm, globals, lfp.self_val(), &errors)?;
    // SAFETY: the document, if any, is live (and now preserved).
    unsafe {
        let doc = xml::xmlTextReaderCurrentDoc(reader);
        if !doc.is_null() && (*doc).encoding.is_null() {
            let encoding_id = IdentId::get_id("@encoding");
            let enc = globals.store.get_ivar(lfp.self_val(), encoding_id).unwrap_or_default();
            let enc = if enc.as_bool() {
                cstr(enc, &globals.store)?
            } else {
                globals.store.set_ivar(lfp.self_val(), encoding_id, Value::string_from_str("UTF-8"))?;
                CString::new("UTF-8").unwrap()
            };
            (*doc).encoding = xml::xmlStrdup(enc.as_ptr() as *const xml::xmlChar);
        }
    }
    match status {
        1 => Ok(lfp.self_val()),
        0 => Ok(Value::nil()),
        _ => {
            let klass = globals.store.get_module(classes().xml_syntax_error).as_val();
            let ex = vm.invoke_method_inner(globals, IdentId::get_id("aggregate"), klass, &[rb_errors], None, None)?;
            Err(if ex.as_bool() { raise(ex) } else { MonorubyErr::runtimeerr(format!("Error pulling: {status}")) })
        }
    }
}

/// `has_attributes`: an element with attributes or namespace definitions.
unsafe fn has_attributes(reader: *mut xml::xmlTextReader) -> bool {
    // SAFETY: a live reader; its current node, if any, is live.
    unsafe {
        let node = xml::xmlTextReaderCurrentNode(reader);
        !node.is_null()
            && (*node).type_ == xml::XML_ELEMENT_NODE
            && (!(*node).properties.is_null() || !(*node).nsDef.is_null())
    }
}

/// Expand the current node for `attribute_hash` / `namespaces`; a failure
/// raises the first collected error.
fn expand(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, reader: *mut xml::xmlTextReader) -> Result<*mut xml::xmlNode> {
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: a live reader.
    let node = unsafe {
        xml::xmlSetStructuredErrorFunc(&mut errors as *mut _ as *mut c_void, Some(collect_error));
        let node = xml::xmlTextReaderExpand(reader);
        xml::xmlSetStructuredErrorFunc(std::ptr::null_mut(), None);
        node
    };
    let rb_errors = push_errors(vm, globals, lfp.self_val(), &errors)?;
    if node.is_null() {
        let first = vm.invoke_method_inner(globals, IdentId::get_id("first"), rb_errors, &[], None, None)?;
        if !first.is_nil() {
            let msg = vm.invoke_method_inner(globals, IdentId::get_id("to_s"), first, &[], None, None)?;
            let klass = globals.store.get_module(classes().xml_syntax_error).as_val();
            let ex = vm.invoke_method_inner(globals, IdentId::NEW, klass, &[msg], None, None)?;
            return Err(raise(ex));
        }
    }
    Ok(node)
}

/// Reader#attribute_hash -> Hash of name => value
#[monoruby_builtin]
fn attribute_hash(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let reader = this(vm, globals, lfp)?;
    let hash = Value::hash_with_capacity(0);
    // SAFETY: a live reader.
    if !unsafe { has_attributes(reader) } {
        return Ok(hash);
    }
    let node = expand(vm, globals, lfp, reader)?;
    if node.is_null() {
        return Ok(Value::nil());
    }
    // SAFETY: a live node and its attributes.
    unsafe {
        let mut prop = (*node).properties;
        while !prop.is_null() {
            let name = xml_str((*prop).name);
            let value = xml_str_owned(xml::xmlNodeGetContent(prop as *mut xml::xmlNode));
            hash.as_hash_mut(&globals.store)?.insert(name, value, vm, globals)?;
            prop = (*prop).next;
        }
    }
    Ok(hash)
}

/// Reader#namespaces -> Hash of "xmlns[:prefix]" => href
#[monoruby_builtin]
fn namespaces(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let reader = this(vm, globals, lfp)?;
    let hash = Value::hash_with_capacity(0);
    // SAFETY: a live reader.
    if !unsafe { has_attributes(reader) } {
        return Ok(hash);
    }
    let node = expand(vm, globals, lfp, reader)?;
    if node.is_null() {
        return Ok(Value::nil());
    }
    // SAFETY: a live element and its namespace definitions.
    unsafe {
        if (*node).type_ != xml::XML_ELEMENT_NODE {
            return Ok(hash);
        }
        let mut ns = (*node).nsDef;
        while !ns.is_null() {
            let mut key = b"xmlns".to_vec();
            if !(*ns).prefix.is_null() {
                key.push(b':');
                key.extend_from_slice(CStr::from_ptr((*ns).prefix as *const c_char).to_bytes());
            }
            hash.as_hash_mut(&globals.store)?.insert(utf8(&key), xml_str((*ns).href), vm, globals)?;
            ns = (*ns).next;
        }
    }
    Ok(hash)
}

/// Reader#attribute(name) -> String | nil
#[monoruby_builtin]
fn attribute(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let reader = this(vm, globals, lfp)?;
    if lfp.arg(0).is_nil() {
        return Ok(Value::nil());
    }
    let name = cstr(lfp.arg(0), &globals.store)?;
    // SAFETY: a live reader; the value is ours to free.
    Ok(unsafe { xml_str_owned(xml::xmlTextReaderGetAttribute(reader, name.as_ptr() as *const xml::xmlChar)) })
}

/// Reader#attribute_at(index) -> String | nil
#[monoruby_builtin]
fn attribute_at(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let reader = this(vm, globals, lfp)?;
    if lfp.arg(0).is_nil() {
        return Ok(Value::nil());
    }
    // `rb_Integer`: `Kernel#Integer` conversion (an ArgumentError for a
    // non-numeric String).
    let index = vm.invoke_method_inner_vis(globals, IdentId::get_id("Integer"), lfp.self_val(), &[lfp.arg(0)], None, None, true)?;
    let index = index.expect_integer(&globals.store)? as c_int;
    // SAFETY: a live reader; the value is ours to free.
    Ok(unsafe { xml_str_owned(xml::xmlTextReaderGetAttributeNo(reader, index)) })
}

/// A count-like answer: -1 is nil.
fn count_or_nil(n: c_int) -> Value {
    if n == -1 { Value::nil() } else { Value::integer(n as i64) }
}

/// A yes / no / unknown answer: 0 false, 1 true, else nil.
fn tri(n: c_int) -> Value {
    match n {
        0 => Value::bool(false),
        1 => Value::bool(true),
        _ => Value::nil(),
    }
}

/// Reader#attribute_count -> Integer | nil
#[monoruby_builtin]
fn attribute_count(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let reader = this(vm, globals, lfp)?;
    // SAFETY: a live reader.
    Ok(count_or_nil(unsafe { xml::xmlTextReaderAttributeCount(reader) }))
}

/// Reader#attributes? -> bool | nil
#[monoruby_builtin]
fn has_attributes_p(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let reader = this(vm, globals, lfp)?;
    // SAFETY: a live reader.
    Ok(Value::bool(unsafe { has_attributes(reader) }))
}

/// Reader#default? -> bool | nil
#[monoruby_builtin]
fn is_default(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let reader = this(vm, globals, lfp)?;
    // SAFETY: a live reader.
    Ok(tri(unsafe { xml::xmlTextReaderIsDefault(reader) }))
}

/// Reader#value? -> bool | nil
#[monoruby_builtin]
fn has_value(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let reader = this(vm, globals, lfp)?;
    // SAFETY: a live reader.
    Ok(tri(unsafe { xml::xmlTextReaderHasValue(reader) }))
}

/// Reader#depth -> Integer | nil
#[monoruby_builtin]
fn depth(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let reader = this(vm, globals, lfp)?;
    // SAFETY: a live reader.
    Ok(count_or_nil(unsafe { xml::xmlTextReaderDepth(reader) }))
}

/// Reader#empty_element? -> bool
#[monoruby_builtin]
fn empty_element(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let reader = this(vm, globals, lfp)?;
    // SAFETY: a live reader.
    Ok(Value::bool(unsafe { xml::xmlTextReaderIsEmptyElement(reader) } != 0))
}

/// Reader#encoding -> String | nil: the parser's, else the constructor's.
#[monoruby_builtin]
fn encoding(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let reader = this(vm, globals, lfp)?;
    // SAFETY: a live reader.
    let enc = unsafe { xml_str(xml::xmlTextReaderConstEncoding(reader)) };
    if !enc.is_nil() {
        return Ok(enc);
    }
    let enc = globals
        .store
        .get_ivar(lfp.self_val(), IdentId::get_id("@encoding"))
        .unwrap_or_default();
    Ok(if enc.as_bool() { enc } else { Value::nil() })
}

/// Reader#inner_xml -> String | nil
#[monoruby_builtin]
fn inner_xml(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let reader = this(vm, globals, lfp)?;
    // SAFETY: a live reader; the string is ours to free.
    Ok(unsafe { xml_str_owned(xml::xmlTextReaderReadInnerXml(reader)) })
}

/// Reader#outer_xml -> String | nil
#[monoruby_builtin]
fn outer_xml(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let reader = this(vm, globals, lfp)?;
    // SAFETY: a live reader; the string is ours to free.
    Ok(unsafe { xml_str_owned(xml::xmlTextReaderReadOuterXml(reader)) })
}

/// Reader#base_uri -> String | nil
#[monoruby_builtin]
fn base_uri(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let reader = this(vm, globals, lfp)?;
    // SAFETY: a live reader; the string is ours to free.
    Ok(unsafe { xml_str_owned(xml::xmlTextReaderBaseUri(reader)) })
}

macro_rules! const_string {
    ($name:ident, $f:ident) => {
        #[monoruby_builtin]
        fn $name(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
            let reader = this(vm, globals, lfp)?;
            // SAFETY: a live reader; the string belongs to it.
            Ok(unsafe { xml_str(xml::$f(reader)) })
        }
    };
}

// Reader#lang / #local_name / #name / #namespace_uri / #prefix / #value /
// #xml_version -> String | nil
const_string!(lang, xmlTextReaderConstXmlLang);
const_string!(local_name, xmlTextReaderConstLocalName);
const_string!(name, xmlTextReaderConstName);
const_string!(namespace_uri, xmlTextReaderConstNamespaceUri);
const_string!(prefix, xmlTextReaderConstPrefix);
const_string!(value, xmlTextReaderConstValue);
const_string!(xml_version, xmlTextReaderConstXmlVersion);

/// Reader#state -> Integer
#[monoruby_builtin]
fn state(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let reader = this(vm, globals, lfp)?;
    // SAFETY: a live reader.
    Ok(Value::integer(unsafe { xml::xmlTextReaderReadState(reader) } as i64))
}

/// Reader#node_type -> Integer
#[monoruby_builtin]
fn node_type(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let reader = this(vm, globals, lfp)?;
    // SAFETY: a live reader.
    Ok(Value::integer(unsafe { xml::xmlTextReaderNodeType(reader) } as i64))
}
