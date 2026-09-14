use super::*;
use libyaml_safer::{
    Emitter, Encoding, Event, EventData, MappingStyle, Parser, ScalarStyle, SequenceStyle,
    TagDirective, VersionDirective,
};

//
// YAML — the native half of Psych (gem/psych/psych.rb stands in for the
// gem's C extension, psych.so).
//
// The psych gem is a thin Ruby layer (nodes, TreeBuilder, ToRuby,
// YAMLTree, ...) over libyaml's event API: the extension only drives
// libyaml's parser, calling `Handler` methods per event, and feeds
// `Psych::Emitter` events to libyaml's emitter. `libyaml-safer` is a
// port of libyaml 0.2.5, so parse events and emitted text are the ones
// CRuby's psych produces.
//
// An emitter is a native object (libyaml's is stateful across the
// events of a stream, and rejects them out of order with "expected
// STREAM-START" etc.), held in a thread-local table and addressed by an
// integer handle; the Ruby `Psych::Emitter` frees it at `end_stream` and
// from a finalizer.
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(STRING_CLASS, "__yaml_parse", yaml_parse, 2);
    globals.define_builtin_class_func(STRING_CLASS, "__yaml_emitter_new", emitter_new, 3);
    globals.define_builtin_class_func(STRING_CLASS, "__yaml_emit", yaml_emit, 2);
    globals.define_builtin_class_func(STRING_CLASS, "__yaml_emitter_free", emitter_free, 1);
    globals.define_builtin_class_func(STRING_CLASS, "__yaml_libyaml_version", libyaml_version, 0);
}

fn opt_str(v: Value, globals: &Globals) -> Result<Option<String>> {
    if v.is_nil() {
        Ok(None)
    } else {
        Ok(Some(v.expect_str(&globals.store)?.to_string()))
    }
}

fn opt_str_value(s: &Option<String>) -> Value {
    match s {
        Some(s) => Value::string_from_str(s),
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
#[monoruby_builtin]
fn yaml_parse(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let handler = lfp.arg(0);
    let src_v = lfp.arg(1);
    let src = src_v.expect_bytes(&globals.store)?.to_vec();
    let mut input: &[u8] = &src;
    let mut parser = Parser::new();
    parser.set_input_string(&mut input);

    let event_location = IdentId::get_id("event_location");
    let ids: [IdentId; 11] = [
        IdentId::get_id("start_stream"),
        IdentId::get_id("end_stream"),
        IdentId::get_id("start_document"),
        IdentId::get_id("end_document"),
        IdentId::get_id("alias"),
        IdentId::get_id("scalar"),
        IdentId::get_id("start_sequence"),
        IdentId::get_id("end_sequence"),
        IdentId::get_id("start_mapping"),
        IdentId::get_id("end_mapping"),
        IdentId::get_id("empty"),
    ];

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
                    Some(c) => Value::string_from_str(c),
                    None => Value::nil(),
                };
                return Ok(Value::array_from_vec(vec![
                    Value::integer(line),
                    Value::integer(column),
                    Value::integer(offset),
                    Value::string_from_str(e.problem()),
                    context,
                ]));
            }
        };
        let loc = [
            Value::integer(event.start_mark.line as i64),
            Value::integer(event.start_mark.column as i64),
            Value::integer(event.end_mark.line as i64),
            Value::integer(event.end_mark.column as i64),
        ];
        vm.invoke_method_inner(globals, event_location, handler, &loc, None, None)?;
        let (id, args): (IdentId, Vec<Value>) = match event.data {
            EventData::StreamStart { encoding } => {
                (ids[0], vec![Value::integer(encoding as i64)])
            }
            EventData::StreamEnd => (ids[1], vec![]),
            EventData::DocumentStart {
                version_directive,
                tag_directives,
                implicit,
            } => {
                let version = match version_directive {
                    Some(v) => Value::array2(
                        Value::integer(v.major as i64),
                        Value::integer(v.minor as i64),
                    ),
                    None => Value::array_empty(),
                };
                let tags = Value::array_from_iter(tag_directives.iter().map(|t| {
                    Value::array2(
                        Value::string_from_str(&t.handle),
                        Value::string_from_str(&t.prefix),
                    )
                }));
                (ids[2], vec![version, tags, Value::bool(implicit)])
            }
            EventData::DocumentEnd { implicit } => (ids[3], vec![Value::bool(implicit)]),
            EventData::Alias { anchor } => (ids[4], vec![Value::string(anchor)]),
            EventData::Scalar {
                anchor,
                tag,
                value,
                plain_implicit,
                quoted_implicit,
                style,
            } => (
                ids[5],
                vec![
                    Value::string(value),
                    opt_str_value(&anchor),
                    opt_str_value(&tag),
                    Value::bool(plain_implicit),
                    Value::bool(quoted_implicit),
                    Value::integer(style as i64),
                ],
            ),
            EventData::SequenceStart {
                anchor,
                tag,
                implicit,
                style,
            } => (
                ids[6],
                vec![
                    opt_str_value(&anchor),
                    opt_str_value(&tag),
                    Value::bool(implicit),
                    Value::integer(style as i64),
                ],
            ),
            EventData::SequenceEnd => (ids[7], vec![]),
            EventData::MappingStart {
                anchor,
                tag,
                implicit,
                style,
            } => (
                ids[8],
                vec![
                    opt_str_value(&anchor),
                    opt_str_value(&tag),
                    Value::bool(implicit),
                    Value::integer(style as i64),
                ],
            ),
            EventData::MappingEnd => (ids[9], vec![]),
            #[allow(unreachable_patterns)]
            _ => (ids[10], vec![]),
        };
        let done = id == ids[1];
        vm.invoke_method_inner(globals, id, handler, &args, None, None)?;
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
fn directives_of(src: &str) -> Result<(Option<VersionDirective>, Vec<TagDirective>)> {
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
            }) => return Ok((version_directive, tag_directives)),
            Ok(Event {
                data: EventData::StreamEnd,
                ..
            }) => return Err(MonorubyErr::runtimeerr("no document")),
            Ok(_) => {}
            Err(e) => return Err(MonorubyErr::runtimeerr(e.problem().to_string())),
        }
    }
}

fn version_directive(major: i64, minor: i64) -> Result<VersionDirective> {
    let (v, _) = directives_of(&format!("%YAML {major}.{minor}\n--- a\n"))?;
    v.ok_or_else(|| MonorubyErr::runtimeerr("invalid version directive"))
}

fn tag_directive(handle: &str, prefix: &str) -> Result<TagDirective> {
    if handle.contains(char::is_whitespace) || prefix.contains(char::is_whitespace) {
        return Err(MonorubyErr::runtimeerr("invalid tag directive"));
    }
    let (_, mut tags) = directives_of(&format!("%TAG {handle} {prefix}\n--- a\n"))?;
    if tags.is_empty() {
        return Err(MonorubyErr::runtimeerr("invalid tag directive"));
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
#[monoruby_builtin]
fn emitter_new(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let canonical = lfp.arg(0).as_bool();
    let indent = lfp.arg(1).expect_integer(&globals.store)?;
    let width = lfp.arg(2).expect_integer(&globals.store)?;
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
    Ok(Value::integer(handle as i64))
}

/// String.__yaml_emitter_free(handle) -> nil
#[monoruby_builtin]
fn emitter_free(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let handle = lfp.arg(0).expect_integer(&globals.store)? as usize;
    EMITTERS.with(|t| {
        let mut t = t.borrow_mut();
        if let Some(slot) = t.get_mut(handle) {
            *slot = None;
        }
    });
    Ok(Value::nil())
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
#[monoruby_builtin]
fn yaml_emit(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let handle = lfp.arg(0).expect_integer(&globals.store)? as usize;
    let ev = lfp.arg(1).expect_array_ty(&globals.store)?;
    let kind = ev
        .first()
        .copied()
        .unwrap_or(Value::nil())
        .expect_integer(&globals.store)?;
    let arg = |i: usize| ev.get(i).copied().unwrap_or(Value::nil());
    let event = match kind {
        0 => Event::stream_start(encoding_of(arg(1).expect_integer(&globals.store)?)),
        1 => Event::stream_end(),
        2 => {
            let version = match arg(1).try_array_ty() {
                Some(v) if v.len() >= 2 => Some(version_directive(
                    v[0].expect_integer(&globals.store)?,
                    v[1].expect_integer(&globals.store)?,
                )?),
                _ => None,
            };
            let mut tags = Vec::new();
            if let Some(list) = arg(2).try_array_ty() {
                for pair in list.iter() {
                    let pair = pair.expect_array_ty(&globals.store)?;
                    if pair.len() < 2 {
                        return Err(MonorubyErr::runtimeerr("tag tuple must be of length 2"));
                    }
                    tags.push(tag_directive(
                        pair[0].expect_str(&globals.store)?,
                        pair[1].expect_str(&globals.store)?,
                    )?);
                }
            }
            Event::document_start(version, &tags, arg(3).as_bool())
        }
        3 => Event::document_end(arg(1).as_bool()),
        4 => {
            let value = arg(1).expect_str(&globals.store)?.to_string();
            let anchor = opt_str(arg(2), globals)?;
            let tag = opt_str(arg(3), globals)?;
            Event::scalar(
                anchor.as_deref(),
                tag.as_deref(),
                &value,
                arg(4).as_bool(),
                arg(5).as_bool(),
                scalar_style(arg(6).expect_integer(&globals.store)?),
            )
        }
        5 => {
            let anchor = opt_str(arg(1), globals)?;
            let tag = opt_str(arg(2), globals)?;
            Event::sequence_start(
                anchor.as_deref(),
                tag.as_deref(),
                arg(3).as_bool(),
                sequence_style(arg(4).expect_integer(&globals.store)?),
            )
        }
        6 => Event::sequence_end(),
        7 => {
            let anchor = opt_str(arg(1), globals)?;
            let tag = opt_str(arg(2), globals)?;
            Event::mapping_start(
                anchor.as_deref(),
                tag.as_deref(),
                arg(3).as_bool(),
                mapping_style(arg(4).expect_integer(&globals.store)?),
            )
        }
        8 => Event::mapping_end(),
        9 => Event::alias(arg(1).expect_str(&globals.store)?),
        k => return Err(MonorubyErr::argumenterr(format!("unknown emitter event {k}"))),
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
        if flush {
            if let Err(e) = entry.emitter.flush() {
                return Err(e.problem().to_string());
            }
        }
        Ok(std::mem::take(&mut *entry.buf.borrow_mut()))
    });
    match res {
        Ok(out) => Ok(match String::from_utf8(out) {
            Ok(s) => Value::string(s),
            Err(e) => Value::bytes(e.into_bytes()),
        }),
        Err(msg) => Err(MonorubyErr::runtimeerr(msg)),
    }
}

/// String.__yaml_libyaml_version -> [major, minor, patch]
#[monoruby_builtin]
fn libyaml_version(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // libyaml-safer is a port of libyaml 0.2.5.
    Ok(Value::array_from_vec(vec![
        Value::integer(0),
        Value::integer(2),
        Value::integer(5),
    ]))
}
