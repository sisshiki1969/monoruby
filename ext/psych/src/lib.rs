//! Psych's native half (`psych_native.so`) as a monoruby extension —
//! `gem/psych/psych.rb` stands in for the gem's C extension, psych.so, and
//! requires this library.
//!
//! The psych gem is a thin Ruby layer (nodes, TreeBuilder, ToRuby,
//! YAMLTree, ...) over libyaml's event API: the extension only drives
//! libyaml's parser, calling `Handler` methods per event, and feeds
//! `Psych::Emitter` events to libyaml's emitter. `libyaml-safer` is a
//! port of libyaml 0.2.5, so parse events and emitted text are the ones
//! CRuby's psych produces.
//!
//! An emitter is a native object (libyaml's is stateful across the
//! events of a stream, and rejects them out of order with "expected
//! STREAM-START" etc.), held in a thread-local table and addressed by an
//! integer handle; the Ruby `Psych::Emitter` frees it at `end_stream` and
//! from a finalizer.
//!
//! This is `src/builtins/yaml.rs` moved out of the interpreter
//! (doc/native_extension_loading.md, step 3).

use libyaml_safer::{
    Emitter, Encoding, Event, EventData, MappingStyle, Parser, ScalarStyle, SequenceStyle,
    TagDirective, VersionDirective,
};
use monoruby_ext::*;
use std::ffi::c_int;

#[unsafe(no_mangle)]
pub unsafe extern "C" fn Init_psych_native(ctx: *mut MrContext) -> c_int {
    // SAFETY: the interpreter's contract for `Init_`.
    unsafe { init(ctx, init_psych) }
}

fn init_psych(ctx: &mut Ctx) -> Result<()> {
    let string = ctx.const_get(Value::UNDEF, "String").ok_or(Error)?;
    let s = MR_METHOD_SINGLETON;
    ctx.define_method(string, "__yaml_parse", method!(yaml_parse), 2, s);
    ctx.define_method(string, "__yaml_emitter_new", method!(emitter_new), 3, s);
    ctx.define_method(string, "__yaml_emit", method!(yaml_emit), 2, s);
    ctx.define_method(string, "__yaml_emitter_free", method!(emitter_free), 1, s);
    ctx.define_method(
        string,
        "__yaml_libyaml_version",
        method!(libyaml_version),
        0,
        s,
    );
    Ok(())
}

fn opt_str(ctx: &mut Ctx, v: Value) -> Result<Option<String>> {
    if v.is_nil() {
        Ok(None)
    } else {
        Ok(Some(ctx.str_string(v)?))
    }
}

fn opt_str_value(ctx: &Ctx, s: &Option<String>) -> Value {
    match s {
        Some(s) => ctx.str(s),
        None => Value::nil(),
    }
}

/// String.__yaml_parse(handler, source) -> nil | [line, column, offset, problem, context]
///
/// Parse `source` (bytes, UTF-8 or UTF-16 with a BOM), calling on
/// `handler` what psych's parser calls: `event_location(start_line,
/// start_column, end_line, end_column)` before each event, then
/// `start_stream(encoding)`, `start_document(version, tag_directives,
/// implicit)`, `end_document(implicit)`, `alias(anchor)`, `scalar(value,
/// anchor, tag, plain, quoted, style)`, `start_sequence(anchor, tag,
/// implicit, style)`, `end_sequence`, `start_mapping(...)`,
/// `end_mapping`, `end_stream`. A syntax error answers its position and
/// texts for the Ruby side to raise as `Psych::SyntaxError`.
fn yaml_parse(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let handler = args[0];
    let src = ctx.str_vec(args[1])?;
    let mut input: &[u8] = &src;
    let mut parser = Parser::new();
    parser.set_input_string(&mut input);

    loop {
        let event = match parser.parse() {
            Ok(ev) => ev,
            Err(e) => {
                // psych reports the *context* mark (where the enclosing
                // construct began), 1-based, and libyaml's problem
                // offset, which is only set for reader errors (not
                // exposed by the port: 0).
                let (line, column) = match e.context_mark().or_else(|| e.problem_mark()) {
                    Some(m) => (m.line as i64 + 1, m.column as i64 + 1),
                    None => (0, 0),
                };
                let offset = 0;
                let context = match e.context() {
                    Some(c) => ctx.str(c),
                    None => Value::nil(),
                };
                let problem = ctx.str(e.problem());
                return Ok(ctx.ary_from(&[
                    Value::int(line),
                    Value::int(column),
                    Value::int(offset),
                    problem,
                    context,
                ]));
            }
        };
        let loc = [
            Value::int(event.start_mark.line as i64),
            Value::int(event.start_mark.column as i64),
            Value::int(event.end_mark.line as i64),
            Value::int(event.end_mark.column as i64),
        ];
        ctx.funcall(handler, "event_location", &loc, None)?;
        // The event's Values are built, then handed to the handler in one
        // call: nothing runs Ruby between the allocations and the call
        // that roots them in its frame.
        let (name, args): (&str, Vec<Value>) = match event.data {
            EventData::StreamStart { encoding } => {
                ("start_stream", vec![Value::int(encoding as i64)])
            }
            EventData::StreamEnd => ("end_stream", vec![]),
            EventData::DocumentStart {
                version_directive,
                tag_directives,
                implicit,
            } => {
                let version = match version_directive {
                    Some(v) => {
                        ctx.ary_from(&[Value::int(v.major as i64), Value::int(v.minor as i64)])
                    }
                    None => ctx.ary_new(),
                };
                let tags = ctx.ary_new();
                for t in tag_directives.iter() {
                    let h = ctx.str(&t.handle);
                    let p = ctx.str(&t.prefix);
                    let pair = ctx.ary_from(&[h, p]);
                    ctx.ary_push(tags, pair)?;
                }
                ("start_document", vec![version, tags, Value::bool(implicit)])
            }
            EventData::DocumentEnd { implicit } => ("end_document", vec![Value::bool(implicit)]),
            EventData::Alias { anchor } => ("alias", vec![ctx.str(anchor)]),
            EventData::Scalar {
                anchor,
                tag,
                value,
                plain_implicit,
                quoted_implicit,
                style,
            } => (
                "scalar",
                vec![
                    ctx.str(value),
                    opt_str_value(ctx, &anchor),
                    opt_str_value(ctx, &tag),
                    Value::bool(plain_implicit),
                    Value::bool(quoted_implicit),
                    Value::int(style as i64),
                ],
            ),
            EventData::SequenceStart {
                anchor,
                tag,
                implicit,
                style,
            } => (
                "start_sequence",
                vec![
                    opt_str_value(ctx, &anchor),
                    opt_str_value(ctx, &tag),
                    Value::bool(implicit),
                    Value::int(style as i64),
                ],
            ),
            EventData::SequenceEnd => ("end_sequence", vec![]),
            EventData::MappingStart {
                anchor,
                tag,
                implicit,
                style,
            } => (
                "start_mapping",
                vec![
                    opt_str_value(ctx, &anchor),
                    opt_str_value(ctx, &tag),
                    Value::bool(implicit),
                    Value::int(style as i64),
                ],
            ),
            EventData::MappingEnd => ("end_mapping", vec![]),
            #[allow(unreachable_patterns)]
            _ => ("empty", vec![]),
        };
        let done = name == "end_stream";
        ctx.funcall(handler, name, &args, None)?;
        if done {
            return Ok(Value::nil());
        }
    }
}

/// `VersionDirective` / `TagDirective` are `#[non_exhaustive]` without a
/// public constructor in libyaml-safer 0.3, so the values come from the
/// library's own parser: a one-line document carrying the directive is
/// parsed and the directive lifted off its DOCUMENT-START event. Only
/// `Psych.dump(..., version:)` and tag directives reach here.
fn directives_of(
    ctx: &mut Ctx,
    src: &str,
) -> Result<(Option<VersionDirective>, Vec<TagDirective>)> {
    let bytes = src.as_bytes();
    let mut input: &[u8] = bytes;
    let mut parser = Parser::new();
    parser.set_input_string(&mut input);
    loop {
        match parser.parse() {
            Ok(Event {
                data:
                    EventData::DocumentStart {
                        version_directive,
                        tag_directives,
                        ..
                    },
                ..
            }) => {
                return Ok((version_directive, tag_directives));
            }
            Ok(Event {
                data: EventData::StreamEnd,
                ..
            }) => return Err(ctx.runtime_error("no document")),
            Ok(_) => {}
            Err(e) => return Err(ctx.runtime_error(e.problem())),
        }
    }
}

fn version_directive(ctx: &mut Ctx, major: i64, minor: i64) -> Result<VersionDirective> {
    let (v, _) = directives_of(ctx, &format!("%YAML {major}.{minor}\n--- a\n"))?;
    v.ok_or_else(|| ctx.runtime_error("invalid version directive"))
}

fn tag_directive(ctx: &mut Ctx, handle: &str, prefix: &str) -> Result<TagDirective> {
    if handle.contains(char::is_whitespace) || prefix.contains(char::is_whitespace) {
        return Err(ctx.runtime_error("invalid tag directive"));
    }
    let (_, mut tags) = directives_of(ctx, &format!("%TAG {handle} {prefix}\n--- a\n"))?;
    if tags.is_empty() {
        return Err(ctx.runtime_error("invalid tag directive"));
    }
    Ok(tags.remove(0))
}

fn scalar_style(n: i64) -> ScalarStyle {
    match n {
        1 => ScalarStyle::Plain,
        2 => ScalarStyle::SingleQuoted,
        3 => ScalarStyle::DoubleQuoted,
        4 => ScalarStyle::Literal,
        5 => ScalarStyle::Folded,
        _ => ScalarStyle::Any,
    }
}

fn sequence_style(n: i64) -> SequenceStyle {
    match n {
        1 => SequenceStyle::Block,
        2 => SequenceStyle::Flow,
        _ => SequenceStyle::Any,
    }
}

fn mapping_style(n: i64) -> MappingStyle {
    match n {
        1 => MappingStyle::Block,
        2 => MappingStyle::Flow,
        _ => MappingStyle::Any,
    }
}

fn encoding_of(n: i64) -> Encoding {
    match n {
        1 => Encoding::Utf8,
        2 => Encoding::Utf16Le,
        3 => Encoding::Utf16Be,
        _ => Encoding::Any,
    }
}

/// The emitter writes into a shared buffer through `SinkWriter`; the
/// entry drains that buffer after each event. The emitter borrows the
/// sink for `'static`, so the sink is a leaked box freed after the
/// emitter is dropped.
struct SinkWriter(std::rc::Rc<std::cell::RefCell<Vec<u8>>>);

impl std::io::Write for SinkWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.borrow_mut().extend_from_slice(buf);
        Ok(buf.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

struct EmitterEntry {
    emitter: std::mem::ManuallyDrop<Emitter<'static>>,
    sink: *mut SinkWriter,
    buf: std::rc::Rc<std::cell::RefCell<Vec<u8>>>,
}

impl Drop for EmitterEntry {
    fn drop(&mut self) {
        // SAFETY: the emitter (the only borrower of the sink) is dropped
        // first; the sink was leaked from a Box in `emitter_new` and is
        // freed exactly once, here.
        unsafe {
            std::mem::ManuallyDrop::drop(&mut self.emitter);
            drop(Box::from_raw(self.sink));
        }
    }
}

thread_local! {
    static EMITTERS: std::cell::RefCell<Vec<Option<EmitterEntry>>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

/// String.__yaml_emitter_new(canonical, indentation, line_width) -> Integer
///
/// A libyaml emitter with psych's settings (unicode output, and the
/// `DumperOptions` given), answering its handle.
fn emitter_new(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let canonical = args[0].truthy();
    let indent = ctx.int(args[1])?;
    let width = ctx.int(args[2])?;
    let buf = std::rc::Rc::new(std::cell::RefCell::new(Vec::new()));
    let sink = Box::into_raw(Box::new(SinkWriter(buf.clone())));
    let mut emitter = Emitter::new();
    // SAFETY: `sink` stays allocated until `EmitterEntry::drop`, which
    // drops the emitter before freeing it.
    emitter.set_output(unsafe { &mut *sink });
    // psych's emitter always asks for unicode output (non-ASCII text is
    // written as is, not escaped).
    emitter.set_unicode(true);
    emitter.set_canonical(canonical);
    emitter.set_indent(indent as i32);
    emitter.set_width(width as i32);
    let entry = EmitterEntry {
        emitter: std::mem::ManuallyDrop::new(emitter),
        sink,
        buf,
    };
    let handle = EMITTERS.with(|t| {
        let mut t = t.borrow_mut();
        if let Some(i) = t.iter().position(|e| e.is_none()) {
            t[i] = Some(entry);
            i
        } else {
            t.push(Some(entry));
            t.len() - 1
        }
    });
    Ok(Value::int(handle as i64))
}

/// String.__yaml_emitter_free(handle) -> nil
fn emitter_free(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let handle = ctx.int(args[0])? as usize;
    EMITTERS.with(|t| {
        let mut t = t.borrow_mut();
        if let Some(slot) = t.get_mut(handle) {
            *slot = None;
        }
    });
    Ok(Value::nil())
}

/// The `i`th element of the event array, `nil` past its end.
fn arg(ctx: &Ctx, ev: Value, i: usize) -> Value {
    ctx.ary_get(ev, i).unwrap_or(Value::nil())
}

/// String.__yaml_emit(handle, event) -> String
///
/// Feed one `Psych::Emitter` event, `[kind, *args]`, to the emitter and
/// answer what it wrote for it (possibly ""). Kinds: 0
/// start_stream(encoding), 1 end_stream, 2 start_document(version,
/// tag_directives, implicit), 3 end_document(implicit), 4 scalar(value,
/// anchor, tag, plain, quoted, style), 5 start_sequence(anchor, tag,
/// implicit, style), 6 end_sequence, 7 start_mapping(anchor, tag,
/// implicit, style), 8 end_mapping, 9 alias(anchor). An emitter error
/// ("expected STREAM-START", ...) is a RuntimeError, as in psych.
fn yaml_emit(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let handle = ctx.int(args[0])? as usize;
    let ev = args[1];
    if ctx.type_of(ev) != MrType::Array {
        return Err(ctx.type_error("event must be an Array"));
    }
    let kind = ctx.int(arg(ctx, ev, 0))?;
    let event = match kind {
        0 => {
            let enc = ctx.int(arg(ctx, ev, 1))?;
            Event::stream_start(encoding_of(enc))
        }
        1 => Event::stream_end(),
        2 => {
            let v = arg(ctx, ev, 1);
            let version = if ctx.type_of(v) == MrType::Array && ctx.ary_len(v) >= 2 {
                let major = ctx.int(arg(ctx, v, 0))?;
                let minor = ctx.int(arg(ctx, v, 1))?;
                Some(version_directive(ctx, major, minor)?)
            } else {
                None
            };
            let mut tags = Vec::new();
            let list = arg(ctx, ev, 2);
            if ctx.type_of(list) == MrType::Array {
                for i in 0..ctx.ary_len(list) {
                    let pair = arg(ctx, list, i);
                    if ctx.type_of(pair) != MrType::Array {
                        return Err(ctx.type_error("tag tuple must be an Array"));
                    }
                    if ctx.ary_len(pair) < 2 {
                        return Err(ctx.runtime_error("tag tuple must be of length 2"));
                    }
                    let handle = ctx.str_string(arg(ctx, pair, 0))?;
                    let prefix = ctx.str_string(arg(ctx, pair, 1))?;
                    tags.push(tag_directive(ctx, &handle, &prefix)?);
                }
            }
            Event::document_start(version, &tags, arg(ctx, ev, 3).truthy())
        }
        3 => Event::document_end(arg(ctx, ev, 1).truthy()),
        4 => {
            let value = ctx.str_string(arg(ctx, ev, 1))?;
            let anchor = opt_str(ctx, arg(ctx, ev, 2))?;
            let tag = opt_str(ctx, arg(ctx, ev, 3))?;
            let style = ctx.int(arg(ctx, ev, 6))?;
            Event::scalar(
                anchor.as_deref(),
                tag.as_deref(),
                &value,
                arg(ctx, ev, 4).truthy(),
                arg(ctx, ev, 5).truthy(),
                scalar_style(style),
            )
        }
        5 => {
            let anchor = opt_str(ctx, arg(ctx, ev, 1))?;
            let tag = opt_str(ctx, arg(ctx, ev, 2))?;
            let style = ctx.int(arg(ctx, ev, 4))?;
            Event::sequence_start(
                anchor.as_deref(),
                tag.as_deref(),
                arg(ctx, ev, 3).truthy(),
                sequence_style(style),
            )
        }
        6 => Event::sequence_end(),
        7 => {
            let anchor = opt_str(ctx, arg(ctx, ev, 1))?;
            let tag = opt_str(ctx, arg(ctx, ev, 2))?;
            let style = ctx.int(arg(ctx, ev, 4))?;
            Event::mapping_start(
                anchor.as_deref(),
                tag.as_deref(),
                arg(ctx, ev, 3).truthy(),
                mapping_style(style),
            )
        }
        8 => Event::mapping_end(),
        9 => {
            let anchor = ctx.str_string(arg(ctx, ev, 1))?;
            Event::alias(&anchor)
        }
        k => return Err(ctx.argument_error(format!("unknown emitter event {k}"))),
    };
    let flush = kind == 1 || kind == 3;
    let res: std::result::Result<Vec<u8>, String> = EMITTERS.with(|t| {
        let mut t = t.borrow_mut();
        let entry = match t.get_mut(handle).and_then(|e| e.as_mut()) {
            Some(e) => e,
            None => return Err("emitter is closed".to_string()),
        };
        if let Err(e) = entry.emitter.emit(event) {
            return Err(e.problem().to_string());
        }
        if flush && let Err(e) = entry.emitter.flush() {
            return Err(e.problem().to_string());
        }
        Ok(std::mem::take(&mut *entry.buf.borrow_mut()))
    });
    match res {
        Ok(out) => Ok(if std::str::from_utf8(&out).is_ok() {
            ctx.str(&out)
        } else {
            ctx.bytes(&out)
        }),
        Err(msg) => Err(ctx.runtime_error(msg)),
    }
}

/// String.__yaml_libyaml_version -> [major, minor, patch]
fn libyaml_version(ctx: &mut Ctx, _: Value, _: &[Value], _: Block) -> Result<Value> {
    // libyaml-safer is a port of libyaml 0.2.5.
    Ok(ctx.ary_from(&[Value::int(0), Value::int(2), Value::int(5)]))
}
