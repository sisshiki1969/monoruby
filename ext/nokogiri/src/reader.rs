//! `Nokogiri::XML::Reader`: a cursor over `xmlTextReader` (`xml_reader.c`).
//! The reader owns its `xmlTextReader`, the IO it pulls from (when made by
//! `from_io`) and the bytes of a `from_memory` input; parse errors are
//! collected into the Ruby `errors` array as they happen.

use crate::*;

/// The payload of a `Reader`.
struct XmlReader {
    reader: *mut xml::xmlTextReader,
    io: Option<Box<IoCtx>>,
    /// The bytes of a `from_memory` input, read in place by the reader.
    _buffer: Vec<u8>,
}

native!(XmlReader, "XmlReader", |this, m| {
    if let Some(io) = &this.io {
        m.mark(io.io);
    }
});

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

pub(crate) fn init(ctx: &mut Ctx, c: &Classes) {
    let r = c.reader;
    ctx.define_method(
        r,
        "from_memory",
        method!(from_memory),
        MR_ARGC_VARIADIC,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(
        r,
        "from_io",
        method!(from_io),
        MR_ARGC_VARIADIC,
        MR_METHOD_SINGLETON,
    );
    ctx.define_method(r, "attribute", method!(attribute), 1, 0);
    ctx.define_method(r, "attribute_at", method!(attribute_at), 1, 0);
    ctx.define_method(r, "attribute_count", method!(attribute_count), 0, 0);
    ctx.define_method(r, "attribute_hash", method!(attribute_hash), 0, 0);
    ctx.define_method(r, "attributes?", method!(has_attributes_p), 0, 0);
    ctx.define_method(r, "base_uri", method!(base_uri), 0, 0);
    ctx.define_method(r, "default?", method!(is_default), 0, 0);
    ctx.define_method(r, "depth", method!(depth), 0, 0);
    ctx.define_method(r, "empty_element?", method!(empty_element), 0, 0);
    ctx.define_method(r, "encoding", method!(encoding), 0, 0);
    ctx.define_method(r, "inner_xml", method!(inner_xml), 0, 0);
    ctx.define_method(r, "lang", method!(lang), 0, 0);
    ctx.define_method(r, "local_name", method!(local_name), 0, 0);
    ctx.define_method(r, "name", method!(name), 0, 0);
    ctx.define_method(r, "namespace_uri", method!(namespace_uri), 0, 0);
    ctx.define_method(r, "namespaces", method!(namespaces), 0, 0);
    ctx.define_method(r, "node_type", method!(node_type), 0, 0);
    ctx.define_method(r, "outer_xml", method!(outer_xml), 0, 0);
    ctx.define_method(r, "prefix", method!(prefix), 0, 0);
    ctx.define_method(r, "read", method!(read), 0, 0);
    ctx.define_method(r, "state", method!(state), 0, 0);
    ctx.define_method(r, "value", method!(value), 0, 0);
    ctx.define_method(r, "value?", method!(has_value), 0, 0);
    ctx.define_method(r, "xml_version", method!(xml_version), 0, 0);
}

/// The reader of `self`, with the IO callbacks' executor pointers made
/// current (any call may pull more input).
fn recv(ctx: &mut Ctx, this: Value) -> Result<*mut xml::xmlTextReader> {
    let Some(r) = ctx.native::<XmlReader>(this) else {
        return Err(ctx.argument_error("expected a Nokogiri::XML::Reader"));
    };
    if let Some(io) = &mut r.io {
        io.ctx = ctx.raw();
    }
    Ok(r.reader)
}

/// `reader.errors` (the Ruby array the parse errors go to).
fn errors_of(ctx: &mut Ctx, reader: Value) -> Result<Value> {
    ctx.funcall(reader, "errors", &[], None)
}

/// Append the errors of a library call to `reader.errors`.
fn push_errors(ctx: &mut Ctx, reader: Value, errors: &[ErrorRecord]) -> Result<Value> {
    let rb_errors = errors_of(ctx, reader)?;
    if !errors.is_empty() {
        let new_errors = errors_to_array(ctx, errors)?;
        ctx.funcall(rb_errors, "concat", &[new_errors], None)?;
    }
    Ok(rb_errors)
}

/// Make a reader over `source` and `initialize(source, url, encoding)`
/// (`from_memory` / `from_io`); `make` builds the `xmlTextReader`.
fn construct(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    what: &str,
    make: impl FnOnce(
        &mut Ctx,
        Value,
        Option<CString>,
        Option<CString>,
        c_int,
    ) -> Result<(*mut xml::xmlTextReader, Option<Box<IoCtx>>, Vec<u8>)>,
) -> Result<Value> {
    let class = this;
    let args: Vec<Value> = args.to_vec();
    if args.is_empty() || args.len() > 4 {
        return Err(ctx.argument_error(format!(
            "wrong number of arguments (given {}, expected 1..4)",
            args.len()
        )));
    }
    let arg = |i: usize| args.get(i).copied().unwrap_or_default();
    let source = arg(0);
    if !source.truthy() {
        return Err(ctx.argument_error(format!("{what} cannot be nil")));
    }
    let url = if arg(1).truthy() {
        Some(cstr(arg(1), ctx)?)
    } else {
        None
    };
    let enc = if arg(2).truthy() {
        Some(cstr(arg(2), ctx)?)
    } else {
        None
    };
    let options = if arg(3).truthy() {
        ctx.int(arg(3))? as c_int
    } else {
        0
    };
    let (reader, io, buffer) = make(ctx, source, url, enc, options)?;
    if reader.is_null() {
        return Err(ctx.runtime_error("couldn't create a parser"));
    }
    let rb = ctx.native_new(
        class,
        XmlReader {
            reader,
            io,
            _buffer: buffer,
        },
    )?;
    // `initialize` is private: call it as a function call; root the new
    // object across it.
    let len = ctx.temp_len();
    ctx.temp_push(rb);
    let r = ctx.funcall(rb, "initialize", &[source, arg(1), arg(2)], None);
    ctx.temp_truncate(len);
    r?;
    Ok(rb)
}

/// Reader.from_memory(string, url = nil, encoding = nil, options = 0) -> Reader
fn from_memory(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    construct(
        ctx,
        this,
        args,
        "string",
        |ctx, source, url, enc, options| {
            let buffer = ctx.str_vec(source)?;
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
        },
    )
}

/// Reader.from_io(io, url = nil, encoding = nil, options = 0) -> Reader
fn from_io(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    construct(ctx, this, args, "io", |ctx, source, url, enc, options| {
        // SAFETY: the executor outlives the call; the IO context lives in
        // the payload, its pointers refreshed at each native call.
        let mut io = Box::new(IoCtx::new(ctx, source));
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
fn read(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let reader = recv(ctx, this)?;
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: a live reader; the handler is registered for the call.
    let status = unsafe {
        xml::xmlSetStructuredErrorFunc(&mut errors as *mut _ as *mut c_void, Some(collect_error));
        let status = xml::xmlTextReaderRead(reader);
        xml::xmlSetStructuredErrorFunc(std::ptr::null_mut(), None);
        status
    };
    let rb_errors = push_errors(ctx, this, &errors)?;
    // SAFETY: the document, if any, is live (and now preserved).
    unsafe {
        let doc = xml::xmlTextReaderCurrentDoc(reader);
        if !doc.is_null() && (*doc).encoding.is_null() {
            let encoding_id = "@encoding";
            let enc = Some(ctx.ivar_get(this, encoding_id)).unwrap_or_default();
            let enc = if enc.truthy() {
                cstr(enc, ctx)?
            } else {
                ctx.ivar_set(this, encoding_id, ctx.str("UTF-8"))?;
                CString::new("UTF-8").unwrap()
            };
            (*doc).encoding = xml::xmlStrdup(enc.as_ptr() as *const xml::xmlChar);
        }
    }
    match status {
        1 => Ok(this),
        0 => Ok(Value::nil()),
        _ => {
            let klass = classes().xml_syntax_error;
            let ex = ctx.funcall(klass, "aggregate", &[rb_errors], None)?;
            Err(if ex.truthy() {
                raise(ctx, ex)
            } else {
                ctx.runtime_error(format!("Error pulling: {status}"))
            })
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
fn expand(
    ctx: &mut Ctx,
    this: Value,
    _args: &[Value],
    reader: *mut xml::xmlTextReader,
) -> Result<*mut xml::xmlNode> {
    let mut errors: Vec<ErrorRecord> = vec![];
    // SAFETY: a live reader.
    let node = unsafe {
        xml::xmlSetStructuredErrorFunc(&mut errors as *mut _ as *mut c_void, Some(collect_error));
        let node = xml::xmlTextReaderExpand(reader);
        xml::xmlSetStructuredErrorFunc(std::ptr::null_mut(), None);
        node
    };
    let rb_errors = push_errors(ctx, this, &errors)?;
    if node.is_null() {
        let first = ctx.funcall(rb_errors, "first", &[], None)?;
        if !first.is_nil() {
            let msg = ctx.funcall(first, "to_s", &[], None)?;
            let klass = classes().xml_syntax_error;
            let ex = ctx.funcall(klass, "new", &[msg], None)?;
            return Err(raise(ctx, ex));
        }
    }
    Ok(node)
}

/// Reader#attribute_hash -> Hash of name => value
fn attribute_hash(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let reader = recv(ctx, this)?;
    let hash = ctx.hash_new();
    // SAFETY: a live reader.
    if !unsafe { has_attributes(reader) } {
        return Ok(hash);
    }
    let node = expand(ctx, this, args, reader)?;
    if node.is_null() {
        return Ok(Value::nil());
    }
    // SAFETY: a live node and its attributes.
    unsafe {
        let mut prop = (*node).properties;
        while !prop.is_null() {
            let name = xml_str(ctx, (*prop).name);
            let value = xml_str_owned(ctx, xml::xmlNodeGetContent(prop as *mut xml::xmlNode));
            ctx.hash_set(hash, name, value)?;
            prop = (*prop).next;
        }
    }
    Ok(hash)
}

/// Reader#namespaces -> Hash of "xmlns[:prefix]" => href
fn namespaces(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let reader = recv(ctx, this)?;
    let hash = ctx.hash_new();
    // SAFETY: a live reader.
    if !unsafe { has_attributes(reader) } {
        return Ok(hash);
    }
    let node = expand(ctx, this, args, reader)?;
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
            ctx.hash_set(hash, utf8(ctx, &key), xml_str(ctx, (*ns).href))?;
            ns = (*ns).next;
        }
    }
    Ok(hash)
}

/// Reader#attribute(name) -> String | nil
fn attribute(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let reader = recv(ctx, this)?;
    if args[0].is_nil() {
        return Ok(Value::nil());
    }
    let name = cstr(args[0], ctx)?;
    // SAFETY: a live reader; the value is ours to free.
    Ok(unsafe {
        xml_str_owned(
            ctx,
            xml::xmlTextReaderGetAttribute(reader, name.as_ptr() as *const xml::xmlChar),
        )
    })
}

/// Reader#attribute_at(index) -> String | nil
fn attribute_at(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let reader = recv(ctx, this)?;
    if args[0].is_nil() {
        return Ok(Value::nil());
    }
    // `rb_Integer`: `Kernel#Integer` conversion (an ArgumentError for a
    // non-numeric String).
    let index = ctx.funcall(this, "Integer", &[args[0]], None)?;
    let index = ctx.int(index)? as c_int;
    // SAFETY: a live reader; the value is ours to free.
    Ok(unsafe { xml_str_owned(ctx, xml::xmlTextReaderGetAttributeNo(reader, index)) })
}

/// A count-like answer: -1 is nil.
fn count_or_nil(n: c_int) -> Value {
    if n == -1 {
        Value::nil()
    } else {
        Value::int(n as i64)
    }
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
fn attribute_count(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let reader = recv(ctx, this)?;
    // SAFETY: a live reader.
    Ok(count_or_nil(unsafe {
        xml::xmlTextReaderAttributeCount(reader)
    }))
}

/// Reader#attributes? -> bool | nil
fn has_attributes_p(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let reader = recv(ctx, this)?;
    // SAFETY: a live reader.
    Ok(Value::bool(unsafe { has_attributes(reader) }))
}

/// Reader#default? -> bool | nil
fn is_default(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let reader = recv(ctx, this)?;
    // SAFETY: a live reader.
    Ok(tri(unsafe { xml::xmlTextReaderIsDefault(reader) }))
}

/// Reader#value? -> bool | nil
fn has_value(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let reader = recv(ctx, this)?;
    // SAFETY: a live reader.
    Ok(tri(unsafe { xml::xmlTextReaderHasValue(reader) }))
}

/// Reader#depth -> Integer | nil
fn depth(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let reader = recv(ctx, this)?;
    // SAFETY: a live reader.
    Ok(count_or_nil(unsafe { xml::xmlTextReaderDepth(reader) }))
}

/// Reader#empty_element? -> bool
fn empty_element(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let reader = recv(ctx, this)?;
    // SAFETY: a live reader.
    Ok(Value::bool(
        unsafe { xml::xmlTextReaderIsEmptyElement(reader) } != 0,
    ))
}

/// Reader#encoding -> String | nil: the parser's, else the constructor's.
fn encoding(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let reader = recv(ctx, this)?;
    // SAFETY: a live reader.
    let enc = unsafe { xml_str(ctx, xml::xmlTextReaderConstEncoding(reader)) };
    if !enc.is_nil() {
        return Ok(enc);
    }
    let enc = Some(ctx.ivar_get(this, "@encoding")).unwrap_or_default();
    Ok(if enc.truthy() { enc } else { Value::nil() })
}

/// Reader#inner_xml -> String | nil
fn inner_xml(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let reader = recv(ctx, this)?;
    // SAFETY: a live reader; the string is ours to free.
    Ok(unsafe { xml_str_owned(ctx, xml::xmlTextReaderReadInnerXml(reader)) })
}

/// Reader#outer_xml -> String | nil
fn outer_xml(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let reader = recv(ctx, this)?;
    // SAFETY: a live reader; the string is ours to free.
    Ok(unsafe { xml_str_owned(ctx, xml::xmlTextReaderReadOuterXml(reader)) })
}

/// Reader#base_uri -> String | nil
fn base_uri(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let reader = recv(ctx, this)?;
    // SAFETY: a live reader; the string is ours to free.
    Ok(unsafe { xml_str_owned(ctx, xml::xmlTextReaderBaseUri(reader)) })
}

macro_rules! const_string {
    ($name:ident, $f:ident) => {
        fn $name(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
            let reader = recv(ctx, this)?;
            // SAFETY: a live reader; the string belongs to it.
            Ok(unsafe { xml_str(ctx, xml::$f(reader)) })
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
fn state(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let reader = recv(ctx, this)?;
    // SAFETY: a live reader.
    Ok(Value::int(
        unsafe { xml::xmlTextReaderReadState(reader) } as i64
    ))
}

/// Reader#node_type -> Integer
fn node_type(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let reader = recv(ctx, this)?;
    // SAFETY: a live reader.
    Ok(Value::int(
        unsafe { xml::xmlTextReaderNodeType(reader) } as i64
    ))
}
