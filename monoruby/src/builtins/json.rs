//!
//! The native half of the json gem (2.18.0, the version CRuby 4.0.6
//! ships as a default gem).
//!
//! The gem's Ruby half is vendored as it is under `gem/json/`; its two
//! C extensions are stood in for by `gem/json/ext/parser.rb` and
//! `gem/json/ext/generator.rb`, which create the classes `parser.so` /
//! `generator.so` define and hand them to `String.__json_setup_parser`
//! / `String.__json_setup_generator` here, which put the methods on
//! them. Everything below is a port of ext/json/ext/parser/parser.c
//! and ext/json/ext/generator/generator.c: the same grammar, the same
//! options, the same messages at the same positions.
//!
//! A `JSON::Ext::ParserConfig` keeps its resolved options in a hidden
//! instance variable, and a `JSON::Ext::Generator::State` keeps each
//! field in one — hidden (`/`-prefixed) so that, as with the C
//! extension's TypedData, `instance_variables` and `State#to_h` see
//! none of them.
//!

use super::*;
use std::cell::Cell;

mod fpconv;

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(STRING_CLASS, "__json_setup_parser", setup_parser, 2);
    globals.define_builtin_class_func(STRING_CLASS, "__json_setup_generator", setup_generator, 2);
}

thread_local! {
    /// `JSON::Ext::Generator::State`, once `json/ext/generator` is loaded.
    static STATE_CLASS: Cell<Option<ClassId>> = const { Cell::new(None) };
    /// `JSON::Fragment`.
    static FRAGMENT_CLASS: Cell<Option<ClassId>> = const { Cell::new(None) };
}

fn expect_module(globals: &Globals, v: Value) -> Result<Module> {
    v.is_class_or_module()
        .ok_or_else(|| MonorubyErr::wrong_argument_type(&globals.store, v, "Module"))
}

/// `String.__json_setup_parser(JSON::Ext::ParserConfig, JSON::Ext::Parser)`
#[monoruby_builtin]
fn setup_parser(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let config = expect_module(globals, lfp.arg(0))?.id();
    let parser = expect_module(globals, lfp.arg(1))?.id();
    globals.define_private_builtin_func(config, "initialize", parser_config_initialize, 1);
    globals.define_builtin_func(config, "parse", parser_config_parse, 1);
    globals.define_builtin_class_func(parser, "parse", parser_m_parse, 2);
    Globals::class_version_inc();
    Ok(Value::nil())
}

/// `String.__json_setup_generator(JSON::Ext::Generator::State,
/// JSON::Ext::Generator::GeneratorMethods)`
#[monoruby_builtin]
fn setup_generator(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let state = expect_module(globals, lfp.arg(0))?.id();
    let methods = expect_module(globals, lfp.arg(1))?.id();
    STATE_CLASS.set(Some(state));
    FRAGMENT_CLASS.set(json_constant(globals, "Fragment").map(|m| m.id()));

    globals.define_builtin_class_func(state, "from_state", state_s_from_state, 1);
    globals.define_builtin_class_func(state, "generate", state_s_generate, 3);
    globals.define_private_builtin_func(state, "_configure", state_configure, 1);
    globals.define_builtin_func_with(state, "generate", state_generate, 1, 2, false);
    globals.define_builtin_func(state, "indent", state_indent, 0);
    globals.define_builtin_func(state, "indent=", state_set_indent, 1);
    globals.define_builtin_func(state, "space", state_space, 0);
    globals.define_builtin_func(state, "space=", state_set_space, 1);
    globals.define_builtin_func(state, "space_before", state_space_before, 0);
    globals.define_builtin_func(state, "space_before=", state_set_space_before, 1);
    globals.define_builtin_func(state, "object_nl", state_object_nl, 0);
    globals.define_builtin_func(state, "object_nl=", state_set_object_nl, 1);
    globals.define_builtin_func(state, "array_nl", state_array_nl, 0);
    globals.define_builtin_func(state, "array_nl=", state_set_array_nl, 1);
    globals.define_builtin_func(state, "as_json", state_as_json, 0);
    globals.define_builtin_func(state, "as_json=", state_set_as_json, 1);
    globals.define_builtin_func(state, "max_nesting", state_max_nesting, 0);
    globals.define_builtin_func(state, "max_nesting=", state_set_max_nesting, 1);
    globals.define_builtin_funcs(
        state,
        "script_safe",
        &["script_safe?", "escape_slash", "escape_slash?"],
        state_script_safe,
        0,
    );
    globals.define_builtin_funcs(
        state,
        "script_safe=",
        &["escape_slash="],
        state_set_script_safe,
        1,
    );
    globals.define_builtin_funcs(state, "strict", &["strict?"], state_strict, 0);
    globals.define_builtin_func(state, "strict=", state_set_strict, 1);
    globals.define_builtin_func(state, "check_circular?", state_check_circular, 0);
    globals.define_builtin_func(state, "allow_nan?", state_allow_nan, 0);
    globals.define_builtin_func(state, "allow_nan=", state_set_allow_nan, 1);
    globals.define_builtin_func(state, "ascii_only?", state_ascii_only, 0);
    globals.define_builtin_func(state, "ascii_only=", state_set_ascii_only, 1);
    globals.define_builtin_func(state, "depth", state_depth, 0);
    globals.define_builtin_func(state, "depth=", state_set_depth, 1);
    globals.define_builtin_func(
        state,
        "buffer_initial_length",
        state_buffer_initial_length,
        0,
    );
    globals.define_builtin_func(
        state,
        "buffer_initial_length=",
        state_set_buffer_initial_length,
        1,
    );
    globals.define_private_builtin_func(
        state,
        "allow_duplicate_key?",
        state_allow_duplicate_key,
        0,
    );

    let to_json: [(&str, BuiltinFn); 9] = [
        ("Object", object_to_json),
        ("Hash", hash_to_json),
        ("Array", array_to_json),
        ("Integer", integer_to_json),
        ("Float", float_to_json),
        ("String", string_to_json),
        ("TrueClass", true_to_json),
        ("FalseClass", false_to_json),
        ("NilClass", nil_to_json),
    ];
    for (name, func) in to_json {
        let module = globals
            .store
            .get_constant_noautoload(methods, IdentId::get_id(name))
            .and_then(|v| v.is_class_or_module())
            .ok_or_else(|| {
                MonorubyErr::runtimeerr(format!(
                    "JSON::Ext::Generator::GeneratorMethods::{name} is missing"
                ))
            })?;
        globals.define_builtin_func_with(module.id(), "to_json", func, 0, 1, false);
    }
    Globals::class_version_inc();
    Ok(Value::nil())
}

// ---------------------------------------------------------------------------
// Shared helpers
// ---------------------------------------------------------------------------

fn json_module(globals: &Globals) -> Option<Value> {
    globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("JSON"))
}

fn json_constant(globals: &Globals, name: &str) -> Option<Module> {
    let json = json_module(globals)?.is_class_or_module()?;
    globals
        .store
        .get_constant_noautoload(json.id(), IdentId::get_id(name))?
        .is_class_or_module()
}

/// The error an exception object raises as.
fn exception_err(globals: &Globals, ex: Value) -> MonorubyErr {
    match ex.is_exception() {
        Some(inner) => MonorubyErr::new_from_exception(inner).with_original(ex),
        None => MonorubyErr::typeerr(format!(
            "exception class/object expected, got {}",
            ex.get_real_class_name(&globals.store)
        )),
    }
}

/// `JSON::<class>.new(*args)` with the instance variables `ivars`, as
/// the error it raises.
fn json_exception(
    vm: &mut Executor,
    globals: &mut Globals,
    class: &str,
    args: &[Value],
    ivars: &[(&str, Value)],
) -> MonorubyErr {
    let Some(class_obj) = json_constant(globals, class) else {
        return MonorubyErr::runtimeerr(format!("JSON::{class} is not defined"));
    };
    let ex =
        match vm.invoke_method_inner(globals, IdentId::NEW, class_obj.as_val(), args, None, None) {
            Ok(ex) => ex,
            Err(err) => return err,
        };
    for (name, val) in ivars {
        if let Err(err) = globals.store.set_ivar(ex, IdentId::get_id(name), *val) {
            return err;
        }
    }
    exception_err(globals, ex)
}

fn utf8_string(bytes: &[u8]) -> Value {
    Value::string_from_inner(RStringInner::from_encoding(bytes, Encoding::UTF8))
}

/// `rb_respond_to`: through `respond_to?` when the object has one.
fn respond_to(vm: &mut Executor, globals: &mut Globals, v: Value, name: IdentId) -> Result<bool> {
    let respond_to_ = IdentId::get_id("respond_to?");
    if globals.check_method(v, respond_to_).is_some() {
        Ok(vm
            .invoke_method_inner(globals, respond_to_, v, &[Value::symbol(name)], None, None)?
            .as_bool())
    } else {
        Ok(globals.check_method(v, name).is_some())
    }
}

/// `rb_proc_call_with_block(proc, args)`.
fn call_proc(
    vm: &mut Executor,
    globals: &mut Globals,
    proc: Value,
    args: &[Value],
) -> Result<Value> {
    match proc.is_proc() {
        Some(p) => vm.invoke_proc(globals, &p, args),
        None => vm.invoke_method_inner(globals, IdentId::get_id("call"), proc, args, None, None),
    }
}

/// `StringValue(source)`: a String, or what `to_str` makes of it.
fn json_source(vm: &mut Executor, globals: &mut Globals, v: Value) -> Result<Value> {
    if v.is_rstring_inner().is_some() {
        return Ok(v);
    }
    if let Some(fid) = globals.check_method(v, IdentId::TO_STR) {
        let converted = vm.invoke_func_inner(globals, fid, v, &[], None, None)?;
        if converted.is_rstring_inner().is_some() {
            return Ok(converted);
        }
        return Err(MonorubyErr::cant_convert_error(
            &globals.store,
            v,
            converted,
            "String",
            IdentId::TO_STR,
        ));
    }
    Err(MonorubyErr::no_implicit_conversion(
        &globals.store,
        v,
        STRING_CLASS,
    ))
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum OnDuplicateKey {
    /// The default: a deprecation warning.
    Deprecated,
    /// `allow_duplicate_key: true`
    Ignore,
    /// `allow_duplicate_key: false`
    Raise,
}

impl OnDuplicateKey {
    fn from_i64(i: i64) -> Self {
        match i {
            1 => Self::Ignore,
            2 => Self::Raise,
            _ => Self::Deprecated,
        }
    }

    fn to_i64(self) -> i64 {
        match self {
            Self::Deprecated => 0,
            Self::Ignore => 1,
            Self::Raise => 2,
        }
    }

    fn from_option(val: Value) -> Self {
        if val.as_bool() {
            Self::Ignore
        } else {
            Self::Raise
        }
    }
}

// ---------------------------------------------------------------------------
// Parser (parser.c)
// ---------------------------------------------------------------------------

#[derive(Clone)]
struct ParserConfig {
    on_load: Option<Value>,
    decimal: Option<(Value, IdentId)>,
    on_duplicate_key: OnDuplicateKey,
    max_nesting: i64,
    allow_nan: bool,
    allow_trailing_comma: bool,
    allow_control_characters: bool,
    symbolize_names: bool,
    freeze: bool,
}

const PARSER_CONFIG_IVAR: &str = "/config";

impl ParserConfig {
    /// A config no option has touched: what an allocated, never
    /// initialized `ParserConfig` parses with — no nesting limit.
    fn zeroed() -> Self {
        Self {
            on_load: None,
            decimal: None,
            on_duplicate_key: OnDuplicateKey::Deprecated,
            max_nesting: 0,
            allow_nan: false,
            allow_trailing_comma: false,
            allow_control_characters: false,
            symbolize_names: false,
            freeze: false,
        }
    }

    /// `parser_config_init`.
    fn new(vm: &mut Executor, globals: &mut Globals, opts: Value) -> Result<Self> {
        let mut config = Self::zeroed();
        config.max_nesting = 100;
        if opts.is_nil() {
            return Ok(config);
        }
        let Some(hash) = opts.try_hash_ty() else {
            return Err(MonorubyErr::wrong_argument_type(
                &globals.store,
                opts,
                "Hash",
            ));
        };
        let pairs: Vec<(Value, Value)> = hash.iter().collect();
        for (key, val) in pairs {
            let Some(key) = key.try_symbol() else {
                continue;
            };
            match key.get_name().as_str() {
                "max_nesting" => config.max_nesting = long_config(val),
                "allow_nan" => config.allow_nan = val.as_bool(),
                "allow_trailing_comma" => config.allow_trailing_comma = val.as_bool(),
                "allow_control_characters" => config.allow_control_characters = val.as_bool(),
                "symbolize_names" => config.symbolize_names = val.as_bool(),
                "freeze" => config.freeze = val.as_bool(),
                "on_load" => config.on_load = val.as_bool().then_some(val),
                "allow_duplicate_key" => config.on_duplicate_key = OnDuplicateKey::from_option(val),
                "decimal_class" if val.as_bool() => {
                    config.decimal = Self::decimal_class(vm, globals, val)?;
                }
                _ => {}
            }
        }
        Ok(config)
    }

    /// `decimal_class:` — the receiver and method a decimal's text is
    /// handed to: `try_convert`, or `new`, or else a class `A::B` is
    /// read as the method `B` of `A` (`BigDecimal` as
    /// `Kernel#BigDecimal`).
    fn decimal_class(
        vm: &mut Executor,
        globals: &mut Globals,
        val: Value,
    ) -> Result<Option<(Value, IdentId)>> {
        let try_convert = IdentId::get_id("try_convert");
        if respond_to(vm, globals, val, try_convert)? {
            return Ok(Some((val, try_convert)));
        }
        if respond_to(vm, globals, val, IdentId::NEW)? {
            return Ok(Some((val, IdentId::NEW)));
        }
        let Some(class) = val.is_class() else {
            return Ok(None);
        };
        let name = globals.store.get_class_name(class.id());
        match name.rfind(':') {
            Some(last_colon) => {
                let mod_path = &name[..last_colon - 1];
                let mut module = globals.store[OBJECT_CLASS].get_module();
                for part in mod_path.split("::") {
                    module = globals
                        .store
                        .get_constant_noautoload(module.id(), IdentId::get_id(part))
                        .and_then(|v| v.is_class_or_module())
                        .ok_or_else(|| {
                            MonorubyErr::argumenterr(format!("undefined class/module {mod_path}"))
                        })?;
                }
                Ok(Some((
                    module.as_val(),
                    IdentId::get_id(&name[last_colon + 1..]),
                )))
            }
            None => Ok(globals
                .store
                .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("Kernel"))
                .map(|kernel| (kernel, IdentId::get_id(&name)))),
        }
    }

    /// The config as the Array a `ParserConfig` keeps.
    fn to_value(&self) -> Value {
        let flags = (self.allow_nan as i64)
            | (self.allow_trailing_comma as i64) << 1
            | (self.allow_control_characters as i64) << 2
            | (self.symbolize_names as i64) << 3
            | (self.freeze as i64) << 4;
        let (decimal, method) = match self.decimal {
            Some((v, m)) => (v, Value::symbol(m)),
            None => (Value::nil(), Value::nil()),
        };
        Value::array_from_vec(vec![
            self.on_load.unwrap_or_default(),
            decimal,
            method,
            Value::integer(self.on_duplicate_key.to_i64()),
            Value::integer(self.max_nesting),
            Value::integer(flags),
        ])
    }

    fn from_value(v: Value) -> Self {
        let a = v.as_array();
        let flags = a[5].try_fixnum().unwrap_or(0);
        Self {
            on_load: (!a[0].is_nil()).then_some(a[0]),
            decimal: a[2].try_symbol().map(|m| (a[1], m)),
            on_duplicate_key: OnDuplicateKey::from_i64(a[3].try_fixnum().unwrap_or(0)),
            max_nesting: a[4].try_fixnum().unwrap_or(0),
            allow_nan: flags & 1 != 0,
            allow_trailing_comma: flags & 2 != 0,
            allow_control_characters: flags & 4 != 0,
            symbolize_names: flags & 8 != 0,
            freeze: flags & 16 != 0,
        }
    }
}

///
/// ### JSON::Ext::ParserConfig#initialize
///
/// - new(opts) -> ParserConfig
///
#[monoruby_builtin]
fn parser_config_initialize(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    check_frozen(globals, self_val)?;
    let config = ParserConfig::new(vm, globals, lfp.arg(0))?;
    globals.store.set_ivar(
        self_val,
        IdentId::get_id(PARSER_CONFIG_IVAR),
        config.to_value(),
    )?;
    Ok(self_val)
}

///
/// ### JSON::Ext::ParserConfig#parse
///
/// - parse(source) -> Object
///
#[monoruby_builtin]
fn parser_config_parse(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let config = match globals
        .store
        .get_ivar(lfp.self_val(), IdentId::get_id(PARSER_CONFIG_IVAR))
    {
        Some(v) => ParserConfig::from_value(v),
        None => ParserConfig::zeroed(),
    };
    let source = convert_encoding(vm, globals, lfp.arg(0))?;
    parse_bytes(vm, globals, &config, source)
}

///
/// ### JSON::Ext::Parser.parse
///
/// - parse(source, opts) -> Object
///
#[monoruby_builtin]
fn parser_m_parse(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let source = convert_encoding(vm, globals, lfp.arg(0))?;
    let config = ParserConfig::new(vm, globals, lfp.arg(1))?;
    parse_bytes(vm, globals, &config, source)
}

/// `convert_encoding(StringValue(source))`: the source's bytes as
/// UTF-8 — a UTF-8 source as it is, ill-formed bytes and all, a BINARY
/// one taken for UTF-8, any other encoding transcoded (its failure the
/// `Encoding::*Error` of that transcoding).
fn convert_encoding(vm: &mut Executor, globals: &mut Globals, source: Value) -> Result<Vec<u8>> {
    let source = json_source(vm, globals, source)?;
    let inner = source.as_rstring_inner();
    Ok(match inner.encoding() {
        Encoding::UTF8 | Encoding::Ascii8 => inner.as_bytes().to_vec(),
        enc => crate::value::transcode_bytes_with_opts(
            inner.as_bytes(),
            enc,
            Encoding::UTF8,
            &crate::value::transcode::TranscodeOpts::default(),
            &globals.store,
        )?,
    })
}

fn parse_bytes(
    vm: &mut Executor,
    globals: &mut Globals,
    config: &ParserConfig,
    src: Vec<u8>,
) -> Result<Value> {
    // Every value lives on the temp stack from the moment it is built
    // until the Array or Hash that holds it is (parser.c's rvalue
    // stack), so an `on_load` proc or a `decimal_class` call that
    // collects cannot take it.
    vm.with_temp_scope(|vm| {
        let mut parser = Parser {
            src: &src,
            cur: 0,
            config,
            nesting: 0,
        };
        let result = parser.parse_any(vm, globals)?;
        parser.ensure_eof(vm, globals)?;
        Ok(result)
    })
}

struct Parser<'a> {
    src: &'a [u8],
    cur: usize,
    config: &'a ParserConfig,
    nesting: i64,
}

const PARSE_ERROR_FRAGMENT_LEN: usize = 32;

impl<'a> Parser<'a> {
    /// The byte at `i`; `0` past the end, where C reads the string's
    /// terminator.
    fn at(&self, i: usize) -> u8 {
        self.src.get(i).copied().unwrap_or(0)
    }

    /// The byte at the cursor (`0` at the end, as for a NUL).
    fn peek(&self) -> u8 {
        self.at(self.cur)
    }

    fn rest(&self) -> usize {
        self.src.len().saturating_sub(self.cur)
    }

    fn eos(&self) -> bool {
        self.cur >= self.src.len()
    }

    /// `cursor_position`: the 1-based line; the column counts the byte
    /// at the cursor itself.
    fn cursor_position(&self, at: usize) -> (i64, i64) {
        let mut i = at as isize;
        let mut column = 0;
        let mut line = 1;
        while i >= 0 {
            let ch = self.at(i as usize);
            i -= 1;
            if ch == b'\n' {
                break;
            }
            column += 1;
        }
        while i >= 0 {
            if self.at(i as usize) == b'\n' {
                line += 1;
            }
            i -= 1;
        }
        (line, column)
    }

    /// What `%s` in a message is: up to 32 bytes from `at` to the next
    /// blank, in quotes, less a trailing multibyte character (cut off
    /// or not); the rest of the input if `at` is a blank; `EOF` past
    /// the end.
    fn fragment(&self, at: usize) -> Vec<u8> {
        if at >= self.src.len() {
            return b"EOF".to_vec();
        }
        let ptr = &self.src[at..];
        let mut len = 0;
        while len < PARSE_ERROR_FRAGMENT_LEN {
            let ch = ptr.get(len).copied().unwrap_or(0);
            if ch == 0 || ch == b'\n' || ch == b' ' || ch == b'\t' || ch == b'\r' {
                break;
            }
            len += 1;
        }
        if len == 0 {
            return ptr.iter().take_while(|b| **b != 0).copied().collect();
        }
        let mut buffer = Vec::with_capacity(len + 2);
        buffer.push(b'\'');
        buffer.extend_from_slice(&ptr[..len]);
        // Is continuation byte
        while (0x80..0xC0).contains(&buffer[len]) {
            len -= 1;
        }
        // multibyte character start
        if buffer[len] >= 0xC0 {
            len -= 1;
        }
        buffer.truncate(len + 1);
        buffer.push(b'\'');
        buffer
    }

    /// `raise_parse_error(format, state)`: a `JSON::ParserError` whose
    /// message ends in the cursor's line and column, which the
    /// exception carries as `line` / `column`.
    fn error(&self, vm: &mut Executor, globals: &mut Globals, format: &str) -> MonorubyErr {
        self.error_at(vm, globals, format, self.cur)
    }

    fn error_at(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
        format: &str,
        at: usize,
    ) -> MonorubyErr {
        let (line, column) = self.cursor_position(at);
        let mut message = Vec::new();
        match format.split_once("%s") {
            Some((pre, post)) => {
                message.extend_from_slice(pre.as_bytes());
                message.extend_from_slice(&self.fragment(at));
                message.extend_from_slice(post.as_bytes());
            }
            None => message.extend_from_slice(format.as_bytes()),
        }
        message.extend_from_slice(format!(" at line {line} column {column}").as_bytes());
        json_exception(
            vm,
            globals,
            "ParserError",
            &[utf8_string(&message)],
            &[
                ("@line", Value::integer(line)),
                ("@column", Value::integer(column)),
            ],
        )
    }

    /// `json_push_value`: the value, after `on_load`, onto the stack.
    fn push(&mut self, vm: &mut Executor, globals: &mut Globals, value: Value) -> Result<Value> {
        let value = match self.config.on_load {
            Some(proc) => call_proc(vm, globals, proc, &[value])?,
            None => value,
        };
        vm.temp_push(value);
        Ok(value)
    }

    /// `//` to the end of the line, `/* */`.
    fn eat_comments(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<()> {
        let start = self.cur;
        self.cur += 1;

        match self.peek() {
            b'/' => {
                self.cur = match self.src[self.cur..].iter().position(|b| *b == b'\n') {
                    Some(pos) => self.cur + pos + 1,
                    None => self.src.len(),
                };
            }
            b'*' => {
                self.cur += 1;
                loop {
                    let from = self.cur.min(self.src.len());
                    let Some(pos) = self.src[from..].iter().position(|b| *b == b'*') else {
                        return Err(self.error_at(
                            vm,
                            globals,
                            "unterminated comment, expected closing '*/'",
                            start,
                        ));
                    };
                    self.cur = from + pos + 1;
                    if self.peek() == b'/' {
                        self.cur += 1;
                        break;
                    }
                }
            }
            _ => return Err(self.error_at(vm, globals, "unexpected token %s", start)),
        }
        Ok(())
    }

    fn eat_whitespace(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<()> {
        loop {
            match self.peek() {
                b' ' | b'\n' | b'\t' | b'\r' => self.cur += 1,
                b'/' => self.eat_comments(vm, globals)?,
                _ => return Ok(()),
            }
        }
    }

    fn ensure_eof(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<()> {
        self.eat_whitespace(vm, globals)?;
        if !self.eos() {
            return Err(self.error(vm, globals, "unexpected token at end of stream %s"));
        }
        Ok(())
    }

    fn literal(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        word: &[u8],
        allowed: bool,
        value: Value,
    ) -> Result<Value> {
        if allowed && self.src[self.cur..].starts_with(word) {
            self.cur += word.len();
            return self.push(vm, globals, value);
        }
        Err(self.error(vm, globals, "unexpected token %s"))
    }

    /// `json_parse_any`.
    fn parse_any(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        self.eat_whitespace(vm, globals)?;

        let allow_nan = self.config.allow_nan;
        match self.peek() {
            b'n' => self.literal(vm, globals, b"null", true, Value::nil()),
            b't' => self.literal(vm, globals, b"true", true, Value::bool(true)),
            b'f' => self.literal(vm, globals, b"false", true, Value::bool(false)),
            b'N' => self.literal(vm, globals, b"NaN", allow_nan, Value::float(f64::NAN)),
            b'I' => self.literal(
                vm,
                globals,
                b"Infinity",
                allow_nan,
                Value::float(f64::INFINITY),
            ),
            b'-' => {
                if self.rest() >= 9 && &self.src[self.cur + 1..self.cur + 9] == b"Infinity" {
                    return self.literal(
                        vm,
                        globals,
                        b"-Infinity",
                        allow_nan,
                        Value::float(f64::NEG_INFINITY),
                    );
                }
                let start = self.cur;
                self.cur += 1;
                let number = self.parse_number(vm, globals, true, start)?;
                self.push(vm, globals, number)
            }
            b'0'..=b'9' => {
                let number = self.parse_number(vm, globals, false, self.cur)?;
                self.push(vm, globals, number)
            }
            b'"' => self.parse_string(vm, globals, false),
            b'[' => self.parse_array(vm, globals),
            b'{' => self.parse_object(vm, globals),
            0 => Err(self.error(vm, globals, "unexpected end of input")),
            _ => Err(self.error(vm, globals, "unexpected character: %s")),
        }
    }

    /// One level deeper, within `max_nesting`.
    fn enter(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<()> {
        self.nesting += 1;
        if self.config.max_nesting != 0 && self.config.max_nesting < self.nesting {
            let message = format!("nesting of {} is too deep", self.nesting);
            return Err(json_exception(
                vm,
                globals,
                "NestingError",
                &[Value::string(message)],
                &[],
            ));
        }
        Ok(())
    }

    fn parse_array(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        self.cur += 1;
        self.eat_whitespace(vm, globals)?;
        let stack_head = vm.temp_len();

        if self.peek() == b']' {
            self.cur += 1;
            let array = self.decode_array(vm, stack_head);
            return self.push(vm, globals, array);
        }
        self.enter(vm, globals)?;
        self.parse_any(vm, globals)?;

        loop {
            self.eat_whitespace(vm, globals)?;

            match self.peek() {
                b',' => {
                    self.cur += 1;
                    if self.config.allow_trailing_comma {
                        self.eat_whitespace(vm, globals)?;
                        if self.peek() == b']' {
                            continue;
                        }
                    }
                    self.parse_any(vm, globals)?;
                }
                b']' => {
                    self.cur += 1;
                    self.nesting -= 1;
                    let array = self.decode_array(vm, stack_head);
                    return self.push(vm, globals, array);
                }
                _ => {
                    return Err(self.error(vm, globals, "expected ',' or ']' after array value"));
                }
            }
        }
    }

    fn decode_array(&self, vm: &mut Executor, stack_head: usize) -> Value {
        let values: Vec<Value> = (stack_head..vm.temp_len()).map(|i| vm.temp_at(i)).collect();
        vm.temp_clear(stack_head);
        let mut array = Value::array_from_vec(values);
        if self.config.freeze {
            array.set_frozen();
        }
        array
    }

    fn parse_object(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        let object_start = self.cur;

        self.cur += 1;
        self.eat_whitespace(vm, globals)?;
        let stack_head = vm.temp_len();

        if self.peek() == b'}' {
            self.cur += 1;
            let object = self.decode_object(vm, globals, stack_head, object_start)?;
            return self.push(vm, globals, object);
        }
        self.enter(vm, globals)?;

        if self.peek() != b'"' {
            return Err(self.error(vm, globals, "expected object key, got %s"));
        }
        self.parse_string(vm, globals, true)?;

        self.eat_whitespace(vm, globals)?;
        if self.peek() != b':' {
            return Err(self.error(vm, globals, "expected ':' after object key"));
        }
        self.cur += 1;

        self.parse_any(vm, globals)?;

        loop {
            self.eat_whitespace(vm, globals)?;

            match self.peek() {
                b'}' => {
                    self.cur += 1;
                    self.nesting -= 1;
                    let object = self.decode_object(vm, globals, stack_head, object_start)?;
                    return self.push(vm, globals, object);
                }
                b',' => {
                    self.cur += 1;
                    self.eat_whitespace(vm, globals)?;

                    if self.config.allow_trailing_comma && self.peek() == b'}' {
                        continue;
                    }

                    if self.peek() != b'"' {
                        return Err(self.error(vm, globals, "expected object key, got: %s"));
                    }
                    self.parse_string(vm, globals, true)?;

                    self.eat_whitespace(vm, globals)?;
                    if self.peek() != b':' {
                        return Err(self.error(
                            vm,
                            globals,
                            "expected ':' after object key, got: %s",
                        ));
                    }
                    self.cur += 1;

                    self.parse_any(vm, globals)?;
                }
                _ => {
                    return Err(self.error(
                        vm,
                        globals,
                        "expected ',' or '}' after object value, got: %s",
                    ));
                }
            }
        }
    }

    /// `json_decode_object`: a duplicated key is reported at the
    /// object's `{`.
    fn decode_object(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
        stack_head: usize,
        object_start: usize,
    ) -> Result<Value> {
        let entries = (vm.temp_len() - stack_head) / 2;
        let mut map = RubyMap::default();
        for i in 0..entries {
            let key = vm.temp_at(stack_head + 2 * i);
            let val = vm.temp_at(stack_head + 2 * i + 1);
            map.insert(key, val, vm, globals)?;
        }

        if map.len() < entries {
            match self.config.on_duplicate_key {
                OnDuplicateKey::Ignore => {}
                OnDuplicateKey::Deprecated => {
                    let key = self.find_duplicated_key(vm, globals, stack_head, entries)?;
                    let (line, column) = self.cursor_position(object_start);
                    let warning = Value::string(format!(
                        "detected duplicate key {} in JSON object. This will raise an error in json 3.0 unless enabled via `allow_duplicate_key: true` at line {line} column {column}",
                        key.inspect(&globals.store)
                    ));
                    if let Some(json) = json_module(globals) {
                        vm.invoke_method_inner(
                            globals,
                            IdentId::get_id("deprecation_warning"),
                            json,
                            &[warning],
                            None,
                            None,
                        )?;
                    }
                }
                OnDuplicateKey::Raise => {
                    let key = self.find_duplicated_key(vm, globals, stack_head, entries)?;
                    let message = format!("duplicate key {}", key.inspect(&globals.store));
                    return Err(self.error_at(vm, globals, &message, object_start));
                }
            }
        }

        vm.temp_clear(stack_head);
        let mut object = Value::hash(map);
        if self.config.freeze {
            object.set_frozen();
        }
        Ok(object)
    }

    /// The first key seen twice, as a String.
    fn find_duplicated_key(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
        stack_head: usize,
        entries: usize,
    ) -> Result<Value> {
        let mut set = RubyMap::default();
        for i in 0..entries {
            let key = vm.temp_at(stack_head + 2 * i);
            let before = set.len();
            set.insert(key, Value::bool(true), vm, globals)?;
            if set.len() == before {
                if let Some(sym) = key.try_symbol() {
                    return Ok(Value::string_from_inner(
                        super::symbol::symbol_string_inner(sym),
                    ));
                }
                return Ok(key);
            }
        }
        Ok(Value::bool(false))
    }

    /// On to the next `"`, `\` or control character.
    fn string_scan(&mut self) -> bool {
        while self.cur < self.src.len() {
            let ch = self.src[self.cur];
            if ch == b'"' || ch == b'\\' || ch < 0x20 {
                return true;
            }
            self.cur += 1;
        }
        false
    }

    fn parse_string(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        is_name: bool,
    ) -> Result<Value> {
        self.cur += 1;
        let start = self.cur;

        if !self.string_scan() {
            return Err(self.error(vm, globals, "unexpected end of input, expected closing \""));
        }

        if self.src[self.cur] == b'"' {
            let src = self.src;
            let string = self.build_string(globals, &src[start..self.cur], is_name)?;
            self.cur += 1;
            return self.push(vm, globals, string);
        }

        loop {
            match self.src[self.cur] {
                b'"' => {
                    let string = self.unescape(vm, globals, start, self.cur, is_name)?;
                    self.cur += 1;
                    return self.push(vm, globals, string);
                }
                // The escaped byte is skipped with it.
                b'\\' => self.cur += 1,
                _ => {
                    if !self.config.allow_control_characters {
                        return Err(self.error(
                            vm,
                            globals,
                            "invalid ASCII control character in string: %s",
                        ));
                    }
                }
            }
            self.cur += 1;
            if !self.string_scan() {
                break;
            }
        }

        Err(self.error(vm, globals, "unexpected end of input, expected closing \""))
    }

    /// A key is interned — the frozen String `-"key"` is — or a
    /// Symbol under `symbolize_names:`; under `freeze:` every string
    /// is interned.
    fn build_string(&self, globals: &mut Globals, bytes: &[u8], is_name: bool) -> Result<Value> {
        if is_name && self.config.symbolize_names {
            let inner = RStringInner::from_encoding(bytes, Encoding::UTF8);
            return Ok(Value::symbol(super::string::intern_string(
                &globals.store,
                &inner,
            )?));
        }
        if is_name || self.config.freeze {
            return Ok(globals.store.intern_frozen_str(bytes, Encoding::UTF8));
        }
        Ok(utf8_string(bytes))
    }

    /// Four hex digits at `p`; a bad one is reported at the `\u`.
    fn unescape_unicode(&self, vm: &mut Executor, globals: &mut Globals, p: usize) -> Result<u32> {
        let mut result = 0;
        for i in 0..4 {
            let digit = match self.at(p + i) {
                ch @ b'0'..=b'9' => ch - b'0',
                ch @ b'a'..=b'f' => ch - b'a' + 10,
                ch @ b'A'..=b'F' => ch - b'A' + 10,
                _ => {
                    return Err(self.error_at(
                        vm,
                        globals,
                        "incomplete unicode character escape sequence at %s",
                        p - 2,
                    ));
                }
            };
            result = (result << 4) | digit as u32;
        }
        Ok(result)
    }

    /// `json_string_unescape`: the string from `string` to
    /// `string_end`, which holds a backslash. The escape errors are
    /// reported where parser.c reports them — a short `\u` and a
    /// broken surrogate pair at the start of the run of plain bytes
    /// before the backslash.
    fn unescape(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
        string: usize,
        string_end: usize,
        is_name: bool,
    ) -> Result<Value> {
        let src = self.src;
        let mut buffer = Vec::with_capacity(string_end - string);
        let mut p = string;
        let mut pe = string;

        while pe < string_end {
            let Some(pos) = src[pe..string_end].iter().position(|b| *b == b'\\') else {
                break;
            };
            pe += pos;
            buffer.extend_from_slice(&src[p..pe]);
            pe += 1;
            match src[pe] {
                // nothing to unescape, just skip the backslash
                b'"' | b'/' => p = pe,
                ch @ (b'\\' | b'n' | b'r' | b't' | b'b' | b'f') => {
                    buffer.push(match ch {
                        b'n' => b'\n',
                        b'r' => b'\r',
                        b't' => b'\t',
                        b'b' => 0x08,
                        b'f' => 0x0c,
                        ch => ch,
                    });
                    pe += 1;
                    p = pe;
                }
                b'u' => {
                    if pe + 5 > string_end {
                        return Err(self.error_at(
                            vm,
                            globals,
                            "incomplete unicode character escape sequence at %s",
                            p,
                        ));
                    }
                    pe += 1;
                    let mut ch = self.unescape_unicode(vm, globals, pe)?;
                    pe += 3;
                    // A high surrogate takes the `\u` low one after it:
                    // the two are one code point above U+FFFF.
                    if (ch & 0xFC00) == 0xD800 {
                        pe += 1;
                        if pe + 6 > string_end {
                            return Err(self.error_at(
                                vm,
                                globals,
                                "incomplete surrogate pair at %s",
                                p,
                            ));
                        }
                        if src[pe] == b'\\' && src[pe + 1] == b'u' {
                            let sur = self.unescape_unicode(vm, globals, pe + 2)?;

                            if (sur & 0xFC00) != 0xDC00 {
                                return Err(self.error_at(
                                    vm,
                                    globals,
                                    "invalid surrogate pair at %s",
                                    p,
                                ));
                            }

                            ch = ((ch & 0x3F) << 10)
                                | ((((ch >> 6) & 0xF) + 1) << 16)
                                | (sur & 0x3FF);
                            pe += 5;
                        } else {
                            return Err(self.error_at(
                                vm,
                                globals,
                                "incomplete surrogate pair at %s",
                                p,
                            ));
                        }
                    }

                    convert_utf32_to_utf8(&mut buffer, ch);
                    pe += 1;
                    p = pe;
                }
                ch if ch < 0x20 => {
                    // Kept, backslash and all, under
                    // `allow_control_characters:`.
                    if !self.config.allow_control_characters {
                        let format = if ch == b'\n' {
                            "Invalid unescaped newline character (\\n) in string: %s"
                        } else {
                            "invalid ASCII control character in string: %s"
                        };
                        return Err(self.error_at(vm, globals, format, pe - 1));
                    }
                }
                _ => {
                    return Err(self.error_at(
                        vm,
                        globals,
                        "invalid escape character in string: %s",
                        pe - 1,
                    ));
                }
            }
        }

        buffer.extend_from_slice(&src[p..string_end]);
        self.build_string(globals, &buffer, is_name)
    }

    fn parse_digits(&mut self) -> usize {
        let start = self.cur;
        while self.peek().is_ascii_digit() {
            self.cur += 1;
        }
        self.cur - start
    }

    /// `json_parse_number`: `start` is at the `-` of a negative
    /// number. An integer of any size is an Integer; a decimal is a
    /// Float, or what `decimal_class` makes of its text.
    fn parse_number(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        negative: bool,
        start: usize,
    ) -> Result<Value> {
        let mut integer = true;
        let first_digit = self.peek();

        let mantissa_digits = self.parse_digits();

        if (first_digit == b'0' && mantissa_digits > 1) || (negative && mantissa_digits == 0) {
            return Err(self.error_at(vm, globals, "invalid number: %s", start));
        }

        if self.peek() == b'.' {
            integer = false;
            self.cur += 1;
            if self.parse_digits() == 0 {
                return Err(self.error_at(vm, globals, "invalid number: %s", start));
            }
        }

        if self.peek().eq_ignore_ascii_case(&b'e') {
            integer = false;
            self.cur += 1;
            if matches!(self.peek(), b'-' | b'+') {
                self.cur += 1;
            }
            if self.parse_digits() == 0 {
                return Err(self.error_at(vm, globals, "invalid number: %s", start));
            }
        }

        let text = &self.src[start..self.cur];
        // The number's bytes are ASCII digits, signs, `.` and `e`.
        let text_str = std::str::from_utf8(text).unwrap();
        if integer {
            return Ok(match text_str.parse::<i64>() {
                Ok(i) => Value::integer(i),
                Err(_) => Value::bigint(num::BigInt::parse_bytes(text, 10).unwrap()),
            });
        }

        if let Some((receiver, method)) = self.config.decimal {
            let text =
                Value::string_from_inner(RStringInner::from_encoding(text, Encoding::Ascii8));
            return vm.invoke_method_inner(globals, method, receiver, &[text], None, None);
        }
        Ok(Value::float(text_str.parse::<f64>().unwrap()))
    }
}

/// `convert_UTF32_to_UTF8`: a lone surrogate is written as the three
/// bytes it would be, as parser.c writes it.
fn convert_utf32_to_utf8(buf: &mut Vec<u8>, ch: u32) {
    if ch <= 0x7F {
        buf.push(ch as u8);
    } else if ch <= 0x07FF {
        buf.push(((ch >> 6) | 0xC0) as u8);
        buf.push(((ch & 0x3F) | 0x80) as u8);
    } else if ch <= 0xFFFF {
        buf.push(((ch >> 12) | 0xE0) as u8);
        buf.push((((ch >> 6) & 0x3F) | 0x80) as u8);
        buf.push(((ch & 0x3F) | 0x80) as u8);
    } else if ch <= 0x1fffff {
        buf.push(((ch >> 18) | 0xF0) as u8);
        buf.push((((ch >> 12) & 0x3F) | 0x80) as u8);
        buf.push((((ch >> 6) & 0x3F) | 0x80) as u8);
        buf.push(((ch & 0x3F) | 0x80) as u8);
    } else {
        buf.push(b'?');
    }
}

// ---------------------------------------------------------------------------
// Generator (generator.c)
// ---------------------------------------------------------------------------

/// A `JSON_Generator_State`.
#[derive(Clone)]
struct GenConfig {
    indent: Option<Value>,
    space: Option<Value>,
    space_before: Option<Value>,
    object_nl: Option<Value>,
    array_nl: Option<Value>,
    as_json: Option<Value>,
    max_nesting: i64,
    depth: i64,
    buffer_initial_length: i64,
    on_duplicate_key: OnDuplicateKey,
    allow_nan: bool,
    ascii_only: bool,
    script_safe: bool,
    strict: bool,
}

const FBUFFER_INITIAL_LENGTH_DEFAULT: i64 = 1024;

impl Default for GenConfig {
    /// `state_init`.
    fn default() -> Self {
        Self {
            indent: None,
            space: None,
            space_before: None,
            object_nl: None,
            array_nl: None,
            as_json: None,
            max_nesting: 100,
            depth: 0,
            buffer_initial_length: FBUFFER_INITIAL_LENGTH_DEFAULT,
            on_duplicate_key: OnDuplicateKey::Deprecated,
            allow_nan: false,
            ascii_only: false,
            script_safe: false,
            strict: false,
        }
    }
}

/// The hidden instance variables a `State` keeps its fields in.
mod ivar {
    pub(super) const INDENT: &str = "/indent";
    pub(super) const SPACE: &str = "/space";
    pub(super) const SPACE_BEFORE: &str = "/space_before";
    pub(super) const OBJECT_NL: &str = "/object_nl";
    pub(super) const ARRAY_NL: &str = "/array_nl";
    pub(super) const AS_JSON: &str = "/as_json";
    pub(super) const MAX_NESTING: &str = "/max_nesting";
    pub(super) const DEPTH: &str = "/depth";
    pub(super) const BUFFER_INITIAL_LENGTH: &str = "/buffer_initial_length";
    pub(super) const ON_DUPLICATE_KEY: &str = "/on_duplicate_key";
    pub(super) const ALLOW_NAN: &str = "/allow_nan";
    pub(super) const ASCII_ONLY: &str = "/ascii_only";
    pub(super) const SCRIPT_SAFE: &str = "/script_safe";
    pub(super) const STRICT: &str = "/strict";
}

fn get_field(store: &Store, state: Value, name: &str) -> Option<Value> {
    store
        .get_ivar(state, IdentId::get_id(name))
        .filter(|v| !v.is_nil())
}

fn set_field(store: &mut Store, state: Value, name: &str, val: Value) -> Result<()> {
    store.set_ivar(state, IdentId::get_id(name), val)
}

impl GenConfig {
    /// The config a `State` holds (`state_init`'s for a field never
    /// set).
    fn read(store: &Store, state: Value) -> Self {
        let object = |name| get_field(store, state, name).filter(|v| v.as_bool());
        let int = |name, default| {
            get_field(store, state, name)
                .and_then(|v| v.try_fixnum())
                .unwrap_or(default)
        };
        let flag = |name| get_field(store, state, name).is_some_and(|v| v.as_bool());
        Self {
            indent: object(ivar::INDENT),
            space: object(ivar::SPACE),
            space_before: object(ivar::SPACE_BEFORE),
            object_nl: object(ivar::OBJECT_NL),
            array_nl: object(ivar::ARRAY_NL),
            as_json: object(ivar::AS_JSON),
            max_nesting: int(ivar::MAX_NESTING, 100),
            depth: int(ivar::DEPTH, 0),
            buffer_initial_length: int(ivar::BUFFER_INITIAL_LENGTH, FBUFFER_INITIAL_LENGTH_DEFAULT),
            on_duplicate_key: OnDuplicateKey::from_i64(int(ivar::ON_DUPLICATE_KEY, 0)),
            allow_nan: flag(ivar::ALLOW_NAN),
            ascii_only: flag(ivar::ASCII_ONLY),
            script_safe: flag(ivar::SCRIPT_SAFE),
            strict: flag(ivar::STRICT),
        }
    }

    /// The config into a `State`.
    fn write(&self, store: &mut Store, state: Value) -> Result<()> {
        let object = |v: Option<Value>| v.unwrap_or(Value::bool(false));
        set_field(store, state, ivar::INDENT, object(self.indent))?;
        set_field(store, state, ivar::SPACE, object(self.space))?;
        set_field(store, state, ivar::SPACE_BEFORE, object(self.space_before))?;
        set_field(store, state, ivar::OBJECT_NL, object(self.object_nl))?;
        set_field(store, state, ivar::ARRAY_NL, object(self.array_nl))?;
        set_field(store, state, ivar::AS_JSON, object(self.as_json))?;
        set_field(
            store,
            state,
            ivar::MAX_NESTING,
            Value::integer(self.max_nesting),
        )?;
        set_field(store, state, ivar::DEPTH, Value::integer(self.depth))?;
        set_field(
            store,
            state,
            ivar::BUFFER_INITIAL_LENGTH,
            Value::integer(self.buffer_initial_length),
        )?;
        set_field(
            store,
            state,
            ivar::ON_DUPLICATE_KEY,
            Value::integer(self.on_duplicate_key.to_i64()),
        )?;
        set_field(store, state, ivar::ALLOW_NAN, Value::bool(self.allow_nan))?;
        set_field(store, state, ivar::ASCII_ONLY, Value::bool(self.ascii_only))?;
        set_field(
            store,
            state,
            ivar::SCRIPT_SAFE,
            Value::bool(self.script_safe),
        )?;
        set_field(store, state, ivar::STRICT, Value::bool(self.strict))?;
        Ok(())
    }

    /// The objects the config holds, which may be copies nothing else
    /// refers to (`string_config`, `to_proc`).
    fn values(&self) -> impl Iterator<Item = Value> {
        [
            self.indent,
            self.space,
            self.space_before,
            self.object_nl,
            self.array_nl,
            self.as_json,
        ]
        .into_iter()
        .flatten()
    }

    /// `configure_state`: the options of `opts` (nil, or a Hash) over
    /// this config; any other key is ignored.
    fn configure(&mut self, vm: &mut Executor, globals: &mut Globals, opts: Value) -> Result<()> {
        if !opts.as_bool() {
            return Ok(());
        }
        let Some(hash) = opts.try_hash_ty() else {
            return Err(MonorubyErr::wrong_argument_type(
                &globals.store,
                opts,
                "Hash",
            ));
        };
        let pairs: Vec<(Value, Value)> = hash.iter().collect();
        for (key, val) in pairs {
            let Some(key) = key.try_symbol() else {
                continue;
            };
            match key.get_name().as_str() {
                "indent" => self.indent = string_config(globals, val)?,
                "space" => self.space = string_config(globals, val)?,
                "space_before" => self.space_before = string_config(globals, val)?,
                "object_nl" => self.object_nl = string_config(globals, val)?,
                "array_nl" => self.array_nl = string_config(globals, val)?,
                "max_nesting" => self.max_nesting = long_config(val),
                "allow_nan" => self.allow_nan = val.as_bool(),
                "ascii_only" => self.ascii_only = val.as_bool(),
                "depth" => self.depth = long_config(val),
                "buffer_initial_length" => {
                    if let Some(len) = buffer_initial_length_config(globals, val)? {
                        self.buffer_initial_length = len;
                    }
                }
                "script_safe" | "escape_slash" => self.script_safe = val.as_bool(),
                "strict" => self.strict = val.as_bool(),
                "allow_duplicate_key" => self.on_duplicate_key = OnDuplicateKey::from_option(val),
                "as_json" => {
                    self.as_json = if val.as_bool() {
                        Some(convert_to_proc(vm, globals, val)?)
                    } else {
                        None
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }
}

/// `string_config`: a non-empty String, kept frozen, or nothing.
fn string_config(globals: &Globals, config: Value) -> Result<Option<Value>> {
    if !config.as_bool() {
        return Ok(None);
    }
    let Some(inner) = config.is_rstring_inner() else {
        return Err(MonorubyErr::wrong_argument_type(
            &globals.store,
            config,
            "String",
        ));
    };
    if inner.is_empty() {
        return Ok(None);
    }
    if config.is_frozen() {
        return Ok(Some(config));
    }
    let mut copy = Value::string_from_inner(inner.clone());
    copy.set_frozen();
    Ok(Some(copy))
}

/// `long_config` (and the parser's `max_nesting`).
fn long_config(num: Value) -> i64 {
    if num.as_bool() {
        num.try_fixnum().unwrap_or(0)
    } else {
        0
    }
}

fn buffer_initial_length_config(globals: &Globals, val: Value) -> Result<Option<i64>> {
    let Some(len) = val.try_fixnum() else {
        return Err(MonorubyErr::wrong_argument_type(
            &globals.store,
            val,
            "Integer",
        ));
    };
    Ok((len > 0).then_some(len))
}

/// `rb_convert_type(val, T_DATA, "Proc", "to_proc")`.
fn convert_to_proc(vm: &mut Executor, globals: &mut Globals, val: Value) -> Result<Value> {
    if val.is_proc().is_some() {
        return Ok(val);
    }
    if !respond_to(vm, globals, val, IdentId::TO_PROC)? {
        return Err(MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into Proc",
            val.builtin_class_name(&globals.store)
        )));
    }
    let proc = vm.invoke_method_inner(globals, IdentId::TO_PROC, val, &[], None, None)?;
    if proc.is_proc().is_none() {
        let cname = val.get_real_class_name(&globals.store);
        return Err(MonorubyErr::typeerr(format!(
            "can't convert {cname} to Proc ({cname}#to_proc gives {})",
            proc.get_real_class_name(&globals.store)
        )));
    }
    Ok(proc)
}

fn state_class() -> Result<ClassId> {
    STATE_CLASS
        .get()
        .ok_or_else(|| MonorubyErr::runtimeerr("JSON::Ext::Generator::State is not loaded"))
}

/// `cState_from_state_s`: a State as it is, a Hash as the options of a
/// new one, anything else a default one.
fn from_state(vm: &mut Executor, globals: &mut Globals, opts: Value) -> Result<Value> {
    let state = state_class()?;
    if opts.is_kind_of(&globals.store, state) {
        return Ok(opts);
    }
    let class = globals.store[state].get_module().as_val();
    if opts.is_kind_of(&globals.store, HASH_CLASS) {
        vm.invoke_method_inner(globals, IdentId::NEW, class, &[opts], None, None)
    } else {
        vm.invoke_method_inner(globals, IdentId::NEW, class, &[], None, None)
    }
}

fn check_frozen(globals: &Globals, v: Value) -> Result<()> {
    if v.is_frozen() {
        Err(MonorubyErr::cant_modify_frozen(&globals.store, v))
    } else {
        Ok(())
    }
}

fn check_string(globals: &Globals, v: Value) -> Result<()> {
    if v.is_rstring_inner().is_some() {
        Ok(())
    } else {
        Err(MonorubyErr::wrong_argument_type(
            &globals.store,
            v,
            "String",
        ))
    }
}

/// `JSON::GeneratorError` for `invalid_object`.
fn generator_error(
    vm: &mut Executor,
    globals: &mut Globals,
    invalid_object: Value,
    message: impl Into<String>,
) -> MonorubyErr {
    generator_error_with(vm, globals, invalid_object, Value::string(message.into()))
}

fn generator_error_with(
    vm: &mut Executor,
    globals: &mut Globals,
    invalid_object: Value,
    message: Value,
) -> MonorubyErr {
    json_exception(
        vm,
        globals,
        "GeneratorError",
        &[message, invalid_object],
        &[],
    )
}

/// What `cState_partial_generate` is given to generate with:
/// `generate_json` for `State#generate`, one type's function for its
/// `to_json`.
#[derive(Clone, Copy)]
enum Func {
    Any,
    Object,
    Array,
    Integer,
    Float,
    String,
}

/// `cState_partial_generate`: `obj` in JSON, as a UTF-8 String — or
/// written to `io`, which is then the result.
fn partial_generate(
    vm: &mut Executor,
    globals: &mut Globals,
    config: GenConfig,
    obj: Value,
    func: Func,
    io: Option<Value>,
) -> Result<Value> {
    let capacity = (config.buffer_initial_length as usize).min(1 << 16);
    // Everything the generation makes and still needs — the config's
    // copies, what `as_json` / `to_s` hand back, transcoded strings —
    // is kept on the temp stack across the Ruby calls it makes.
    vm.with_temp_scope(|vm| {
        for v in config.values() {
            vm.temp_push(v);
        }
        let mut generator = Generator {
            depth: config.depth,
            config,
            buf: Vec::with_capacity(capacity),
            vstate: None,
        };
        match func {
            Func::Any => generator.generate(vm, globals, obj)?,
            Func::Object => generator.object(vm, globals, obj)?,
            Func::Array => generator.array(vm, globals, obj)?,
            Func::Integer => generator.integer(vm, globals, obj)?,
            Func::Float => generator.float(vm, globals, obj)?,
            Func::String => generator.string(vm, globals, obj)?,
        }
        let result = utf8_string(&generator.buf);
        match io {
            Some(io) => {
                vm.invoke_method_inner(
                    globals,
                    IdentId::get_id("write"),
                    io,
                    &[result],
                    None,
                    None,
                )?;
                vm.invoke_method_inner(globals, IdentId::get_id("flush"), io, &[], None, None)?;
                Ok(io)
            }
            None => Ok(result),
        }
    })
}

fn io_arg(io: Option<Value>) -> Option<Value> {
    io.filter(|io| io.as_bool())
}

struct Generator {
    config: GenConfig,
    buf: Vec<u8>,
    depth: i64,
    /// The State a `to_json` this generation calls is handed: made the
    /// first time one is needed (`vstate_spill`), and kept on the temp
    /// stack.
    vstate: Option<Value>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum KeyType {
    String,
    Symbol,
    Other,
}

fn key_type(key: Value) -> KeyType {
    if key.try_symbol().is_some() {
        KeyType::Symbol
    } else if key.is_rstring_inner().is_some() {
        KeyType::String
    } else {
        KeyType::Other
    }
}

fn append_string(buf: &mut Vec<u8>, v: Value) {
    buf.extend_from_slice(v.as_rstring_inner().as_bytes());
}

/// `valid_json_string_p`: ASCII in an ASCII-compatible encoding, or
/// well-formed UTF-8 / US-ASCII.
fn valid_json_string(s: Value) -> bool {
    let inner = s.as_rstring_inner();
    match inner.code_range() {
        CodeRange::SevenBit => true,
        CodeRange::Valid => matches!(inner.encoding(), Encoding::UTF8 | Encoding::UsAscii),
        _ => false,
    }
}

impl Generator {
    fn append_opt(&mut self, v: Option<Value>) {
        if let Some(v) = v {
            append_string(&mut self.buf, v);
        }
    }

    fn append_indent(&mut self, depth: i64) {
        if let Some(indent) = self.config.indent {
            for _ in 0..depth {
                append_string(&mut self.buf, indent);
            }
        }
    }

    /// `vstate_spill`.
    fn vstate(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        if let Some(vstate) = self.vstate {
            return Ok(vstate);
        }
        let vstate = Value::object(state_class()?);
        vm.temp_push(vstate);
        self.config.write(&mut globals.store, vstate)?;
        self.vstate = Some(vstate);
        Ok(vstate)
    }

    /// `json_call_to_json`: `obj.to_json(state)`, the State at this
    /// depth. It is this generation's own State, and the generation
    /// goes on with what the `to_json` made of it.
    fn call_to_json(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        obj: Value,
    ) -> Result<Value> {
        let vstate = self.vstate(vm, globals)?;
        set_field(
            &mut globals.store,
            vstate,
            ivar::DEPTH,
            Value::integer(self.depth),
        )?;
        let result = vm.invoke_method_inner(
            globals,
            IdentId::get_id("to_json"),
            obj,
            &[vstate],
            None,
            None,
        )?;
        self.config = GenConfig::read(&globals.store, vstate);
        Ok(result)
    }

    fn call_as_json(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        obj: Value,
        is_key: bool,
    ) -> Result<Value> {
        let proc = self.config.as_json.unwrap();
        let v = call_proc(vm, globals, proc, &[obj, Value::bool(is_key)])?;
        vm.temp_push(v);
        Ok(v)
    }

    /// `increase_depth`: the message names the depth reached.
    fn increase_depth(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<i64> {
        self.depth += 1;
        let depth = self.depth;
        if self.config.max_nesting != 0 && depth > self.config.max_nesting {
            self.depth -= 1;
            let message = format!(
                "nesting of {} is too deep. Did you try to serialize objects with circular references?",
                self.depth
            );
            return Err(json_exception(
                vm,
                globals,
                "NestingError",
                &[Value::string(message)],
                &[],
            ));
        }
        Ok(depth)
    }

    /// `generate_json`: JSON's own types by exact class, a Symbol and
    /// everything else through its `to_json` (or `to_s`) — or, under
    /// `strict:`, through `as_json` or not at all.
    fn generate(&mut self, vm: &mut Executor, globals: &mut Globals, obj: Value) -> Result<()> {
        let mut obj = obj;
        let mut as_json_called = false;
        loop {
            if obj.is_nil() {
                self.buf.extend_from_slice(b"null");
                return Ok(());
            }
            if obj == Value::bool(false) {
                self.buf.extend_from_slice(b"false");
                return Ok(());
            }
            if obj == Value::bool(true) {
                self.buf.extend_from_slice(b"true");
                return Ok(());
            }
            if obj.try_fixnum().is_some() {
                return self.integer(vm, globals, obj);
            }
            if obj.try_symbol().is_some() {
                return self.symbol(vm, globals, obj);
            }
            if let Some(rv) = obj.try_rvalue() {
                let class = obj.class();
                match rv.ty() {
                    ObjTy::BIGNUM => return self.integer(vm, globals, obj),
                    ObjTy::HASH if class == HASH_CLASS => return self.object(vm, globals, obj),
                    ObjTy::ARRAY if class == ARRAY_CLASS => return self.array(vm, globals, obj),
                    ObjTy::STRING if class == STRING_CLASS => {
                        if valid_json_string(obj) {
                            return self.raw_string(vm, globals, obj);
                        } else if as_json_called {
                            return Err(generator_error(
                                vm,
                                globals,
                                obj,
                                "source sequence is illegal/malformed utf-8",
                            ));
                        } else {
                            obj = self.ensure_valid_encoding(vm, globals, obj, false, false)?;
                            as_json_called = true;
                            continue;
                        }
                    }
                    ObjTy::FLOAT if class == FLOAT_CLASS => return self.float(vm, globals, obj),
                    ObjTy::STRUCT if Some(class) == FRAGMENT_CLASS.get() => {
                        let fragment = obj.as_struct().get(0);
                        check_string(globals, fragment)?;
                        append_string(&mut self.buf, fragment);
                        return Ok(());
                    }
                    _ => {}
                }
            } else if obj.try_float().is_some() {
                return self.float(vm, globals, obj);
            }
            // general
            if self.config.strict {
                if self.config.as_json.is_some() && !as_json_called {
                    obj = self.call_as_json(vm, globals, obj, false)?;
                    as_json_called = true;
                    continue;
                }
                let class = globals.store.get_class_name(obj.class());
                return Err(generator_error(
                    vm,
                    globals,
                    obj,
                    format!("{class} not allowed in JSON"),
                ));
            }
            return self.fallback(vm, globals, obj);
        }
    }

    /// `generate_json_fallback`: `to_json(state)` if the object has
    /// one, its `to_s` as a JSON string if not.
    fn fallback(&mut self, vm: &mut Executor, globals: &mut Globals, obj: Value) -> Result<()> {
        if respond_to(vm, globals, obj, IdentId::get_id("to_json"))? {
            let json = self.call_to_json(vm, globals, obj)?;
            check_string(globals, json)?;
            append_string(&mut self.buf, json);
        } else {
            let s = vm.invoke_method_inner(globals, IdentId::TO_S, obj, &[], None, None)?;
            vm.temp_push(s);
            check_string(globals, s)?;
            self.string(vm, globals, s)?;
        }
        Ok(())
    }

    fn symbol(&mut self, vm: &mut Executor, globals: &mut Globals, obj: Value) -> Result<()> {
        if self.config.strict {
            let name =
                Value::string_from_inner(super::symbol::symbol_string_inner(obj.as_symbol()));
            vm.temp_push(name);
            self.string(vm, globals, name)
        } else {
            self.fallback(vm, globals, obj)
        }
    }

    /// `generate_json_integer`: a Bignum as its `to_s` says.
    fn integer(&mut self, vm: &mut Executor, globals: &mut Globals, obj: Value) -> Result<()> {
        if let Some(i) = obj.try_fixnum() {
            self.buf.extend_from_slice(i.to_string().as_bytes());
            return Ok(());
        }
        let s = vm.invoke_method_inner(globals, IdentId::TO_S, obj, &[], None, None)?;
        if s.is_rstring_inner().is_none() {
            return Err(MonorubyErr::no_implicit_conversion(
                &globals.store,
                s,
                STRING_CLASS,
            ));
        }
        append_string(&mut self.buf, s);
        Ok(())
    }

    /// `generate_json_float`: NaN and the infinities only under
    /// `allow_nan:` (as `Float#to_s` writes them); a finite one as
    /// `fpconv_dtoa` does.
    fn float(&mut self, vm: &mut Executor, globals: &mut Globals, obj: Value) -> Result<()> {
        let value = obj.try_float().unwrap();
        if value.is_finite() {
            fpconv::fpconv_dtoa(value, &mut self.buf);
            return Ok(());
        }
        let text = if value.is_nan() {
            "NaN"
        } else if value > 0.0 {
            "Infinity"
        } else {
            "-Infinity"
        };
        if !self.config.allow_nan {
            if self.config.strict && self.config.as_json.is_some() {
                let casted = self.call_as_json(vm, globals, obj, false)?;
                if casted != obj {
                    self.increase_depth(vm, globals)?;
                    self.generate(vm, globals, casted)?;
                    self.depth -= 1;
                    return Ok(());
                }
            }
            return Err(generator_error(
                vm,
                globals,
                obj,
                format!("{text} not allowed in JSON"),
            ));
        }
        self.buf.extend_from_slice(text.as_bytes());
        Ok(())
    }

    /// `generate_json_string`.
    fn string(&mut self, vm: &mut Executor, globals: &mut Globals, obj: Value) -> Result<()> {
        let obj = self.ensure_valid_encoding(vm, globals, obj, false, false)?;
        if obj.is_rstring_inner().is_some() {
            self.raw_string(vm, globals, obj)
        } else {
            self.generate(vm, globals, obj)
        }
    }

    /// `ensure_valid_encoding`: a string the generator can write — a
    /// UTF-8 (or US-ASCII) one as it is; under `strict:`, what
    /// `as_json` makes of it; a BINARY one holding UTF-8 read as UTF-8
    /// (with a warning unless it is ASCII); anything else transcoded,
    /// its failure the GeneratorError.
    fn ensure_valid_encoding(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        s: Value,
        as_json_called: bool,
        is_key: bool,
    ) -> Result<Value> {
        if valid_json_string(s) {
            return Ok(s);
        }
        vm.temp_push(s);

        if !as_json_called && self.config.strict && self.config.as_json.is_some() {
            let coerced = self.call_as_json(vm, globals, s, false)?;
            if coerced != s {
                if coerced.is_rstring_inner().is_some() {
                    if !valid_json_string(coerced) {
                        return Err(generator_error(
                            vm,
                            globals,
                            s,
                            "source sequence is illegal/malformed utf-8",
                        ));
                    }
                } else if is_key {
                    let class = globals.store.get_class_name(coerced.class());
                    return Err(generator_error(
                        vm,
                        globals,
                        coerced,
                        format!("{class} not allowed as object key in JSON"),
                    ));
                }
                return Ok(coerced);
            }
        }

        let inner = s.as_rstring_inner();
        if inner.encoding() == Encoding::Ascii8 {
            let utf8 = RStringInner::from_encoding(inner.as_bytes(), Encoding::UTF8);
            match utf8.code_range() {
                CodeRange::SevenBit => {
                    let v = Value::string_from_inner(utf8);
                    vm.temp_push(v);
                    return Ok(v);
                }
                CodeRange::Valid => {
                    // For historical reason, a binary string is read
                    // as UTF-8 if it would work; json 3.0 will raise.
                    if Executor::warnings_enabled(globals) {
                        vm.ruby_warn_caller(
                            globals,
                            "warning: JSON.generate: UTF-8 string passed as BINARY, this will raise an encoding error in json 3.0",
                        )?;
                    }
                    let v = Value::string_from_inner(utf8);
                    vm.temp_push(v);
                    return Ok(v);
                }
                _ => {}
            }
        }

        // `str.encode(Encoding::UTF_8)`; its error becomes the
        // GeneratorError's message and cause.
        match vm.invoke_method_inner(
            globals,
            IdentId::get_id("encode"),
            s,
            &[Value::string_from_str("UTF-8")],
            None,
            None,
        ) {
            Ok(v) => {
                vm.temp_push(v);
                Ok(v)
            }
            Err(err) => {
                vm.set_error(err);
                let cause = vm.take_ex_obj(globals);
                vm.temp_push(cause);
                let message = vm.invoke_method_inner(
                    globals,
                    IdentId::get_id("message"),
                    cause,
                    &[],
                    None,
                    None,
                )?;
                let mut err = generator_error_with(vm, globals, s, message);
                err.explicit_cause = Some(cause);
                Err(err)
            }
        }
    }

    /// `raw_generate_json_string`: the string, escaped, in quotes.
    fn raw_string(&mut self, vm: &mut Executor, globals: &mut Globals, obj: Value) -> Result<()> {
        if !matches!(
            obj.as_rstring_inner().code_range(),
            CodeRange::SevenBit | CodeRange::Valid
        ) {
            return Err(generator_error(
                vm,
                globals,
                obj,
                "source sequence is illegal/malformed utf-8",
            ));
        }
        let bytes = obj.as_rstring_inner().as_bytes();
        self.buf.push(b'"');
        if self.config.ascii_only {
            convert_utf8_to_ascii_only_json(&mut self.buf, bytes, self.config.script_safe);
        } else if self.config.script_safe {
            convert_utf8_to_script_safe_json(&mut self.buf, bytes);
        } else {
            convert_utf8_to_json(&mut self.buf, bytes);
        }
        self.buf.push(b'"');
        Ok(())
    }

    /// Once per Hash, at a String or Symbol key after a first key of
    /// another type: `JSON.on_mixed_keys_hash`, which looks for keys
    /// that come out the same.
    fn mixed_keys(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        hash: Value,
        encountered: &mut bool,
    ) -> Result<()> {
        if *encountered {
            return Ok(());
        }
        *encountered = true;
        if self.config.on_duplicate_key != OnDuplicateKey::Ignore
            && let Some(json) = json_module(globals)
        {
            let do_raise = Value::bool(self.config.on_duplicate_key == OnDuplicateKey::Raise);
            vm.invoke_method_inner(
                globals,
                IdentId::get_id("on_mixed_keys_hash"),
                json,
                &[hash, do_raise],
                None,
                None,
            )?;
        }
        Ok(())
    }

    /// `rb_convert_type(key, T_STRING, "String", "to_s")`, and
    /// `convert_string_subclass`.
    fn key_to_s(vm: &mut Executor, globals: &mut Globals, key: Value) -> Result<Value> {
        let s = vm.invoke_method_inner(globals, IdentId::TO_S, key, &[], None, None)?;
        vm.temp_push(s);
        if s.is_rstring_inner().is_none() {
            let cname = key.get_real_class_name(&globals.store);
            return Err(MonorubyErr::typeerr(format!(
                "can't convert {cname} to String ({cname}#to_s gives {})",
                s.get_real_class_name(&globals.store)
            )));
        }
        Ok(s)
    }

    /// `generate_json_object`.
    fn object(&mut self, vm: &mut Executor, globals: &mut Globals, obj: Value) -> Result<()> {
        let depth = self.increase_depth(vm, globals)?;

        let hash = obj.as_hash();
        if hash.len() == 0 {
            self.buf.extend_from_slice(b"{}");
            self.depth -= 1;
            return Ok(());
        }

        self.buf.push(b'{');

        // No new key while the pairs are walked (`rb_hash_foreach`),
        // and the pairs stay reachable if a `to_json` deletes one.
        let _iter_guard = hash.iter_guard();
        let pairs: Vec<(Value, Value)> = hash.iter().collect();
        let roots: Vec<Value> = pairs.iter().flat_map(|(k, v)| [*k, *v]).collect();
        vm.temp_push(Value::array_from_vec(roots));

        let mut first_key_type = None;
        let mut mixed_keys_encountered = false;
        for (key, val) in pairs {
            let mut key = key;
            let mut kt = key_type(key);

            let first_key_type = match first_key_type {
                None => {
                    first_key_type = Some(kt);
                    kt
                }
                Some(first) => {
                    self.buf.push(b',');
                    first
                }
            };

            self.append_opt(self.config.object_nl);
            self.append_indent(depth);

            let mut as_json_called = false;
            let key_to_s = loop {
                match kt {
                    KeyType::String => {
                        if first_key_type != KeyType::String {
                            self.mixed_keys(vm, globals, obj, &mut mixed_keys_encountered)?;
                        }
                        break if key.class() == STRING_CLASS {
                            key
                        } else {
                            Self::key_to_s(vm, globals, key)?
                        };
                    }
                    KeyType::Symbol => {
                        if first_key_type != KeyType::Symbol {
                            self.mixed_keys(vm, globals, obj, &mut mixed_keys_encountered)?;
                        }
                        let name = Value::string_from_inner(super::symbol::symbol_string_inner(
                            key.as_symbol(),
                        ));
                        vm.temp_push(name);
                        break name;
                    }
                    KeyType::Other => {
                        if self.config.strict {
                            if self.config.as_json.is_some() && !as_json_called {
                                key = self.call_as_json(vm, globals, key, true)?;
                                kt = key_type(key);
                                as_json_called = true;
                                continue;
                            }
                            let class = globals.store.get_class_name(key.class());
                            return Err(generator_error(
                                vm,
                                globals,
                                key,
                                format!("{class} not allowed as object key in JSON"),
                            ));
                        }
                        break Self::key_to_s(vm, globals, key)?;
                    }
                }
            };

            let key_to_s =
                self.ensure_valid_encoding(vm, globals, key_to_s, as_json_called, true)?;

            if key_to_s.is_rstring_inner().is_some() && key_to_s.class() == STRING_CLASS {
                self.raw_string(vm, globals, key_to_s)?;
            } else {
                self.generate(vm, globals, key_to_s)?;
            }
            self.append_opt(self.config.space_before);
            self.buf.push(b':');
            self.append_opt(self.config.space);
            self.generate(vm, globals, val)?;
        }

        self.depth -= 1;
        let depth = self.depth;
        if self.config.object_nl.is_some() {
            self.append_opt(self.config.object_nl);
            self.append_indent(depth);
        }
        self.buf.push(b'}');
        Ok(())
    }

    /// `generate_json_array`: the length is read afresh at each
    /// element, as `RARRAY_LEN` is.
    fn array(&mut self, vm: &mut Executor, globals: &mut Globals, obj: Value) -> Result<()> {
        let depth = self.increase_depth(vm, globals)?;

        if obj.as_array().len() == 0 {
            self.buf.extend_from_slice(b"[]");
            self.depth -= 1;
            return Ok(());
        }

        self.buf.push(b'[');
        self.append_opt(self.config.array_nl);
        let mut i = 0;
        while i < obj.as_array().len() {
            if i > 0 {
                self.buf.push(b',');
                self.append_opt(self.config.array_nl);
            }
            self.append_indent(depth);
            let elem = obj.as_array()[i];
            self.generate(vm, globals, elem)?;
            i += 1;
        }
        let depth = depth - 1;
        self.depth = depth;
        if self.config.array_nl.is_some() {
            self.append_opt(self.config.array_nl);
            self.append_indent(depth);
        }
        self.buf.push(b']');
        Ok(())
    }
}

const HEXDIG: &[u8; 16] = b"0123456789abcdef";

/// An ASCII byte escaped: the short forms, `\u00XX` for the other
/// control characters.
fn escape_ascii(buf: &mut Vec<u8>, ch: u8) {
    match ch {
        b'"' => buf.extend_from_slice(b"\\\""),
        b'\\' => buf.extend_from_slice(b"\\\\"),
        b'/' => buf.extend_from_slice(b"\\/"),
        0x08 => buf.extend_from_slice(b"\\b"),
        0x0c => buf.extend_from_slice(b"\\f"),
        b'\n' => buf.extend_from_slice(b"\\n"),
        b'\r' => buf.extend_from_slice(b"\\r"),
        b'\t' => buf.extend_from_slice(b"\\t"),
        _ => buf.extend_from_slice(&[
            b'\\',
            b'u',
            b'0',
            b'0',
            HEXDIG[(ch >> 4) as usize & 0xf],
            HEXDIG[ch as usize & 0xf],
        ]),
    }
}

/// `convert_UTF8_to_JSON`: `"`, `\` and the control characters.
fn convert_utf8_to_json(buf: &mut Vec<u8>, bytes: &[u8]) {
    let mut cursor = 0;
    for (i, &ch) in bytes.iter().enumerate() {
        if ch == b'"' || ch == b'\\' || ch < 0x20 {
            buf.extend_from_slice(&bytes[cursor..i]);
            escape_ascii(buf, ch);
            cursor = i + 1;
        }
    }
    buf.extend_from_slice(&bytes[cursor..]);
}

/// `convert_UTF8_to_script_safe_JSON`: `/`, U+2028 and U+2029 too.
fn convert_utf8_to_script_safe_json(buf: &mut Vec<u8>, bytes: &[u8]) {
    let mut cursor = 0;
    let mut i = 0;
    while i < bytes.len() {
        let ch = bytes[i];
        if ch == b'"' || ch == b'\\' || ch == b'/' || ch < 0x20 {
            buf.extend_from_slice(&bytes[cursor..i]);
            escape_ascii(buf, ch);
            i += 1;
            cursor = i;
        } else if ch == 0xE2
            && bytes.get(i + 1) == Some(&0x80)
            && matches!(bytes.get(i + 2), Some(0xA8 | 0xA9))
        {
            buf.extend_from_slice(&bytes[cursor..i]);
            buf.extend_from_slice(if bytes[i + 2] & 1 != 0 {
                b"\\u2029"
            } else {
                b"\\u2028"
            });
            i += 3;
            cursor = i;
        } else {
            i += 1;
        }
    }
    buf.extend_from_slice(&bytes[cursor..]);
}

/// `convert_UTF8_to_ASCII_only_JSON`: each non-ASCII character as
/// `\uXXXX` (a surrogate pair above U+FFFF).
fn convert_utf8_to_ascii_only_json(buf: &mut Vec<u8>, bytes: &[u8], script_safe: bool) {
    fn hex4(buf: &mut Vec<u8>, u: u32) {
        buf.extend_from_slice(b"\\u");
        for shift in [12, 8, 4, 0] {
            buf.push(HEXDIG[((u >> shift) & 0xf) as usize]);
        }
    }
    let mut cursor = 0;
    let mut i = 0;
    while i < bytes.len() {
        let ch = bytes[i];
        if ch < 0x80 {
            if ch == b'"' || ch == b'\\' || ch < 0x20 || (script_safe && ch == b'/') {
                buf.extend_from_slice(&bytes[cursor..i]);
                escape_ascii(buf, ch);
                cursor = i + 1;
            }
            i += 1;
            continue;
        }
        let (len, mut wchar) = match ch {
            0xC0..=0xDF => (2, (ch & 0x1F) as u32),
            0xE0..=0xEF => (3, (ch & 0x0F) as u32),
            _ => (4, (ch & 0x07) as u32),
        };
        buf.extend_from_slice(&bytes[cursor..i]);
        for k in 1..len {
            wchar = (wchar << 6) | (bytes.get(i + k).copied().unwrap_or(0) & 0x3F) as u32;
        }
        if wchar <= 0xFFFF {
            hex4(buf, wchar);
        } else {
            let wchar = wchar - 0x10000;
            hex4(buf, 0xD800 + (wchar >> 10));
            hex4(buf, 0xDC00 + (wchar & 0x3FF));
        }
        i += len;
        cursor = i;
    }
    buf.extend_from_slice(&bytes[cursor.min(bytes.len())..]);
}

// ---------------------------------------------------------------------------
// JSON::Ext::Generator::State
// ---------------------------------------------------------------------------

///
/// ### JSON::Ext::Generator::State.from_state
///
/// - from_state(opts) -> State
///
#[monoruby_builtin]
fn state_s_from_state(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    from_state(vm, globals, lfp.arg(0))
}

///
/// ### JSON::Ext::Generator::State.generate
///
/// - generate(obj, opts, io) -> String | io
///
#[monoruby_builtin]
fn state_s_generate(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut config = GenConfig::default();
    config.configure(vm, globals, lfp.arg(1))?;
    partial_generate(
        vm,
        globals,
        config,
        lfp.arg(0),
        Func::Any,
        io_arg(lfp.try_arg(2)),
    )
}

///
/// ### JSON::Ext::Generator::State#generate
///
/// - generate(obj) -> String
/// - generate(obj, io) -> io
///
#[monoruby_builtin]
fn state_generate(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let config = GenConfig::read(&globals.store, lfp.self_val());
    partial_generate(
        vm,
        globals,
        config,
        lfp.arg(0),
        Func::Any,
        io_arg(lfp.try_arg(1)),
    )
}

///
/// ### JSON::Ext::Generator::State#_configure
///
/// - _configure(opts) -> self
///
#[monoruby_builtin]
fn state_configure(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    check_frozen(globals, self_val)?;
    let mut config = GenConfig::read(&globals.store, self_val);
    config.configure(vm, globals, lfp.arg(0))?;
    config.write(&mut globals.store, self_val)?;
    Ok(self_val)
}

/// A string field's reader: the frozen String, or a frozen empty one.
fn state_string_field(globals: &Globals, lfp: Lfp, name: &str) -> Value {
    match get_field(&globals.store, lfp.self_val(), name).filter(|v| v.as_bool()) {
        Some(v) => v,
        None => {
            let mut empty = utf8_string(b"");
            empty.set_frozen();
            empty
        }
    }
}

fn state_set_field(globals: &mut Globals, lfp: Lfp, name: &str, val: Value) -> Result<Value> {
    let self_val = lfp.self_val();
    check_frozen(globals, self_val)?;
    set_field(&mut globals.store, self_val, name, val)?;
    Ok(Value::nil())
}

fn state_flag(globals: &Globals, lfp: Lfp, name: &str) -> Value {
    Value::bool(get_field(&globals.store, lfp.self_val(), name).is_some_and(|v| v.as_bool()))
}

macro_rules! state_string_accessor {
    ($get:ident, $set:ident, $ivar:expr) => {
        #[monoruby_builtin]
        fn $get(
            _: &mut Executor,
            globals: &mut Globals,
            lfp: Lfp,
            _: BytecodePtr,
        ) -> Result<Value> {
            Ok(state_string_field(globals, lfp, $ivar))
        }

        #[monoruby_builtin]
        fn $set(
            _: &mut Executor,
            globals: &mut Globals,
            lfp: Lfp,
            _: BytecodePtr,
        ) -> Result<Value> {
            check_frozen(globals, lfp.self_val())?;
            let val = string_config(globals, lfp.arg(0))?.unwrap_or(Value::bool(false));
            state_set_field(globals, lfp, $ivar, val)
        }
    };
}

state_string_accessor!(state_indent, state_set_indent, ivar::INDENT);
state_string_accessor!(state_space, state_set_space, ivar::SPACE);
state_string_accessor!(
    state_space_before,
    state_set_space_before,
    ivar::SPACE_BEFORE
);
state_string_accessor!(state_object_nl, state_set_object_nl, ivar::OBJECT_NL);
state_string_accessor!(state_array_nl, state_set_array_nl, ivar::ARRAY_NL);

macro_rules! state_flag_accessor {
    ($get:ident, $set:ident, $ivar:expr) => {
        #[monoruby_builtin]
        fn $get(
            _: &mut Executor,
            globals: &mut Globals,
            lfp: Lfp,
            _: BytecodePtr,
        ) -> Result<Value> {
            Ok(state_flag(globals, lfp, $ivar))
        }

        #[monoruby_builtin]
        fn $set(
            _: &mut Executor,
            globals: &mut Globals,
            lfp: Lfp,
            _: BytecodePtr,
        ) -> Result<Value> {
            let val = Value::bool(lfp.arg(0).as_bool());
            state_set_field(globals, lfp, $ivar, val)
        }
    };
}

state_flag_accessor!(state_script_safe, state_set_script_safe, ivar::SCRIPT_SAFE);
state_flag_accessor!(state_strict, state_set_strict, ivar::STRICT);
state_flag_accessor!(state_allow_nan, state_set_allow_nan, ivar::ALLOW_NAN);
state_flag_accessor!(state_ascii_only, state_set_ascii_only, ivar::ASCII_ONLY);

///
/// ### JSON::Ext::Generator::State#as_json
///
#[monoruby_builtin]
fn state_as_json(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(get_field(&globals.store, lfp.self_val(), ivar::AS_JSON).unwrap_or(Value::bool(false)))
}

///
/// ### JSON::Ext::Generator::State#as_json=
///
#[monoruby_builtin]
fn state_set_as_json(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    check_frozen(globals, lfp.self_val())?;
    let proc = convert_to_proc(vm, globals, lfp.arg(0))?;
    state_set_field(globals, lfp, ivar::AS_JSON, proc)
}

///
/// ### JSON::Ext::Generator::State#max_nesting
///
#[monoruby_builtin]
fn state_max_nesting(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::integer(
        GenConfig::read(&globals.store, lfp.self_val()).max_nesting,
    ))
}

///
/// ### JSON::Ext::Generator::State#max_nesting=
///
#[monoruby_builtin]
fn state_set_max_nesting(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let val = Value::integer(long_config(lfp.arg(0)));
    state_set_field(globals, lfp, ivar::MAX_NESTING, val)
}

///
/// ### JSON::Ext::Generator::State#check_circular?
///
#[monoruby_builtin]
fn state_check_circular(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::bool(
        GenConfig::read(&globals.store, lfp.self_val()).max_nesting != 0,
    ))
}

///
/// ### JSON::Ext::Generator::State#depth
///
#[monoruby_builtin]
fn state_depth(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::integer(
        GenConfig::read(&globals.store, lfp.self_val()).depth,
    ))
}

///
/// ### JSON::Ext::Generator::State#depth=
///
#[monoruby_builtin]
fn state_set_depth(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let val = Value::integer(long_config(lfp.arg(0)));
    state_set_field(globals, lfp, ivar::DEPTH, val)
}

///
/// ### JSON::Ext::Generator::State#buffer_initial_length
///
#[monoruby_builtin]
fn state_buffer_initial_length(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::integer(
        GenConfig::read(&globals.store, lfp.self_val()).buffer_initial_length,
    ))
}

///
/// ### JSON::Ext::Generator::State#buffer_initial_length=
///
#[monoruby_builtin]
fn state_set_buffer_initial_length(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    check_frozen(globals, lfp.self_val())?;
    if let Some(len) = buffer_initial_length_config(globals, lfp.arg(0))? {
        state_set_field(
            globals,
            lfp,
            ivar::BUFFER_INITIAL_LENGTH,
            Value::integer(len),
        )?;
    }
    Ok(Value::nil())
}

///
/// ### JSON::Ext::Generator::State#allow_duplicate_key?
///
/// true (ignore them), false (raise), nil (the deprecation warning).
///
#[monoruby_builtin]
fn state_allow_duplicate_key(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(
        match GenConfig::read(&globals.store, lfp.self_val()).on_duplicate_key {
            OnDuplicateKey::Ignore => Value::bool(true),
            OnDuplicateKey::Deprecated => Value::nil(),
            OnDuplicateKey::Raise => Value::bool(false),
        },
    )
}

// ---------------------------------------------------------------------------
// JSON::Ext::Generator::GeneratorMethods
// ---------------------------------------------------------------------------

/// `cState_partial_generate(cState_from_state_s(state), obj, func)`.
fn to_json_with(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    obj: Value,
    func: Func,
) -> Result<Value> {
    vm.with_temp_scope(|vm| {
        // `obj` may be a `to_s` nothing else holds, and the State one
        // made here.
        vm.temp_push(obj);
        let state = from_state(vm, globals, lfp.try_arg(0).unwrap_or_default())?;
        vm.temp_push(state);
        let config = GenConfig::read(&globals.store, state);
        partial_generate(vm, globals, config, obj, func, None)
    })
}

///
/// ### JSON::Ext::Generator::GeneratorMethods::Object#to_json
///
/// - to_json(state = nil) -> String
///
/// Its `to_s`, as a JSON string.
///
#[monoruby_builtin]
fn object_to_json(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let s = vm.invoke_method_inner(globals, IdentId::TO_S, lfp.self_val(), &[], None, None)?;
    check_string(globals, s)?;
    to_json_with(vm, globals, lfp, s, Func::String)
}

#[monoruby_builtin]
fn hash_to_json(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    to_json_with(vm, globals, lfp, lfp.self_val(), Func::Object)
}

#[monoruby_builtin]
fn array_to_json(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    to_json_with(vm, globals, lfp, lfp.self_val(), Func::Array)
}

#[monoruby_builtin]
fn integer_to_json(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    to_json_with(vm, globals, lfp, lfp.self_val(), Func::Integer)
}

#[monoruby_builtin]
fn float_to_json(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    to_json_with(vm, globals, lfp, lfp.self_val(), Func::Float)
}

#[monoruby_builtin]
fn string_to_json(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    to_json_with(vm, globals, lfp, lfp.self_val(), Func::String)
}

#[monoruby_builtin]
fn true_to_json(_: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(utf8_string(b"true"))
}

#[monoruby_builtin]
fn false_to_json(_: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(utf8_string(b"false"))
}

#[monoruby_builtin]
fn nil_to_json(_: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(utf8_string(b"null"))
}
