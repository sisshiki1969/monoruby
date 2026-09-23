use super::*;

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(STRING_CLASS, "__json_parse", json_parse, 1);
    globals.define_builtin_class_func(STRING_CLASS, "__json_generate", json_generate, 1);
}

/// String.__json_parse(source) class method
#[monoruby_builtin]
fn json_parse(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let src = json_source(vm, globals, lfp.arg(0))?;
    let inner = src.is_rstring_inner().unwrap();
    // The parser reads UTF-8 (`convert_encoding`): a UTF-8 source is
    // taken as it is, bytes and all, a BINARY one is taken for UTF-8
    // without a look, and any other encoding is transcoded first, so
    // its failure is the `Encoding::*Error` of that transcoding.
    let bytes: std::borrow::Cow<[u8]> = match inner.encoding() {
        Encoding::UTF8 | Encoding::Ascii8 => std::borrow::Cow::Borrowed(inner.as_bytes()),
        enc => std::borrow::Cow::Owned(super::encoding::transcode_bytes_with_opts(
            inner.as_bytes(),
            enc,
            Encoding::UTF8,
            &super::encoding::TranscodeOpts::default(),
            &globals.store,
        )?),
    };
    let mut parser = Parser::new(&bytes);
    parser
        .parse_value(vm, globals)
        .ok_or_else(|| json_error(globals, "ParserError", parser.error_message()))
}

/// The source `JSON.parse` reads: a String, or what `to_str` makes of
/// the argument.
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

/// An error of the `JSON::<name>` class — the class is Ruby's
/// (`stdlib/json.rb`), looked up when needed.
fn json_error(globals: &Globals, name: &str, message: String) -> MonorubyErr {
    match json_error_class(globals, name) {
        Some(cid) => MonorubyErr::new(MonorubyErrKind::Other(cid), message),
        None => MonorubyErr::runtimeerr(message),
    }
}

fn json_error_class(globals: &Globals, name: &str) -> Option<ClassId> {
    let json = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("JSON"))?
        .is_class_or_module()?;
    globals
        .store
        .get_constant_noautoload(json.id(), IdentId::get_id(name))?
        .is_class_or_module()
        .map(|c| c.id())
}

/// Fast native JSON generator: JSON.__generate(obj) -> String
#[monoruby_builtin]
fn json_generate(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut buf = String::new();
    match generate(&mut buf, lfp.arg(0), &globals.store) {
        Ok(()) => Ok(Value::string(buf)),
        // `JSON::GeneratorError.new(message, invalid_object)`, raised as
        // that object so `invalid_object` and `detailed_message` hold.
        Err((message, invalid_object)) => {
            let Some(cid) = json_error_class(globals, "GeneratorError") else {
                return Err(MonorubyErr::runtimeerr(message));
            };
            let class_val = globals.store[cid].get_module().as_val();
            let ex = vm.invoke_method_inner(
                globals,
                IdentId::NEW,
                class_val,
                &[Value::string(message), invalid_object],
                None,
                None,
            )?;
            match ex.is_exception() {
                Some(inner) => Err(MonorubyErr::new_from_exception(inner).with_original(ex)),
                None => Err(MonorubyErr::new(MonorubyErrKind::Other(cid), String::new())),
            }
        }
    }
}

// ---------------------------------------------------------------------------
// JSON Parser
// ---------------------------------------------------------------------------

struct Parser<'a> {
    src: &'a [u8],
    pos: usize,
    err: Option<String>,
}

impl<'a> Parser<'a> {
    fn new(src: &'a [u8]) -> Self {
        Self {
            src,
            pos: 0,
            err: None,
        }
    }

    fn error_message(&self) -> String {
        self.err
            .clone()
            .unwrap_or_else(|| format!("unexpected end of input at position {}", self.pos))
    }

    fn set_error(&mut self, msg: String) {
        if self.err.is_none() {
            self.err = Some(msg);
        }
    }

    fn peek(&self) -> Option<u8> {
        self.src.get(self.pos).copied()
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn skip_ws(&mut self) {
        while self.pos < self.src.len() {
            match self.src[self.pos] {
                b' ' | b'\t' | b'\n' | b'\r' => self.pos += 1,
                _ => break,
            }
        }
    }

    fn expect(&mut self, ch: u8) -> bool {
        self.skip_ws();
        if self.peek() == Some(ch) {
            self.advance();
            true
        } else {
            self.set_error(format!(
                "expected '{}' at position {}",
                ch as char, self.pos
            ));
            false
        }
    }

    fn parse_value(&mut self, vm: &mut Executor, globals: &mut Globals) -> Option<Value> {
        self.skip_ws();
        match self.peek()? {
            b'"' => self.parse_string(),
            b'{' => self.parse_object(vm, globals),
            b'[' => self.parse_array(vm, globals),
            b't' => self.parse_literal(b"true", Value::bool(true)),
            b'f' => self.parse_literal(b"false", Value::bool(false)),
            b'n' => self.parse_literal(b"null", Value::nil()),
            b'-' | b'0'..=b'9' => self.parse_number(),
            ch => {
                self.set_error(format!(
                    "unexpected character '{}' at position {}",
                    ch as char, self.pos
                ));
                None
            }
        }
    }

    /// A string value: UTF-8 by declaration, the source's bytes as
    /// they are — an ill-formed sequence in the source is an
    /// ill-formed sequence in the result, as CRuby's parser leaves it.
    fn parse_string(&mut self) -> Option<Value> {
        let s = self.parse_string_raw()?;
        Some(Value::string_from_inner(RStringInner::from_encoding(&s, Encoding::UTF8)))
    }

    /// An object key: the same string, frozen.
    fn parse_key(&mut self) -> Option<Value> {
        let mut key = self.parse_string()?;
        key.set_frozen();
        Some(key)
    }

    fn parse_string_raw(&mut self) -> Option<Vec<u8>> {
        self.advance(); // skip opening "
        // Scan for closing " to find the raw extent, handling escapes
        // by collecting into a byte buffer.
        let mut s: Vec<u8> = Vec::new();
        let push_char = |s: &mut Vec<u8>, c: char| {
            let mut buf = [0u8; 4];
            s.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
        };
        loop {
            let ch = *self.src.get(self.pos)?;
            self.pos += 1;
            match ch {
                b'"' => return Some(s),
                b'\\' => {
                    let esc_at = self.pos - 1;
                    let esc = *self.src.get(self.pos)?;
                    self.pos += 1;
                    match esc {
                        b'"' => s.push(b'"'),
                        b'\\' => s.push(b'\\'),
                        b'/' => s.push(b'/'),
                        b'b' => s.push(0x08),
                        b'f' => s.push(0x0C),
                        b'n' => s.push(b'\n'),
                        b'r' => s.push(b'\r'),
                        b't' => s.push(b'\t'),
                        b'u' => {
                            let cp = self.parse_unicode_escape()?;
                            if (0xD800..=0xDBFF).contains(&cp) {
                                let low = if self.src.get(self.pos) == Some(&b'\\')
                                    && self.src.get(self.pos + 1) == Some(&b'u')
                                {
                                    self.pos += 2;
                                    Some(self.parse_unicode_escape()?)
                                } else {
                                    None
                                };
                                match low {
                                    Some(low) if (0xDC00..=0xDFFF).contains(&low) => {
                                        let combined =
                                            0x10000 + ((cp - 0xD800) << 10) + (low - 0xDC00);
                                        push_char(&mut s, char::from_u32(combined)?);
                                    }
                                    // A high surrogate without its pair is
                                    // not a character; CRuby refuses it.
                                    _ => {
                                        self.set_error(format!(
                                            "incomplete surrogate pair at '{}'",
                                            String::from_utf8_lossy(
                                                &self.src[esc_at..self.src.len().min(esc_at + 32)]
                                            )
                                        ));
                                        return None;
                                    }
                                }
                            } else {
                                match char::from_u32(cp) {
                                    Some(c) => push_char(&mut s, c),
                                    // A lone low surrogate: spelled as the
                                    // three bytes it would be in UTF-8.
                                    None => {
                                        s.push(0xE0 | (cp >> 12) as u8);
                                        s.push(0x80 | ((cp >> 6) & 0x3F) as u8);
                                        s.push(0x80 | (cp & 0x3F) as u8);
                                    }
                                }
                            }
                        }
                        _ => {
                            s.push(b'\\');
                            s.push(esc);
                        }
                    }
                }
                _ => s.push(ch),
            }
        }
    }

    fn parse_unicode_escape(&mut self) -> Option<u32> {
        if self.pos + 4 > self.src.len() {
            return None;
        }
        let hex = &self.src[self.pos..self.pos + 4];
        self.pos += 4;
        let s = std::str::from_utf8(hex).ok()?;
        u32::from_str_radix(s, 16).ok()
    }

    fn parse_number(&mut self) -> Option<Value> {
        let start = self.pos;
        if self.peek() == Some(b'-') {
            self.advance();
        }
        // Integer part
        match self.peek()? {
            b'0' => self.advance(),
            b'1'..=b'9' => {
                self.advance();
                while let Some(b'0'..=b'9') = self.peek() {
                    self.advance();
                }
            }
            _ => {
                self.set_error(format!("invalid number at position {}", start));
                return None;
            }
        }
        let mut is_float = false;
        // Fractional part
        if self.peek() == Some(b'.') {
            is_float = true;
            self.advance();
            if !matches!(self.peek(), Some(b'0'..=b'9')) {
                self.set_error(format!("invalid number at position {}", start));
                return None;
            }
            while let Some(b'0'..=b'9') = self.peek() {
                self.advance();
            }
        }
        // Exponent
        if matches!(self.peek(), Some(b'e' | b'E')) {
            is_float = true;
            self.advance();
            if matches!(self.peek(), Some(b'+' | b'-')) {
                self.advance();
            }
            if !matches!(self.peek(), Some(b'0'..=b'9')) {
                self.set_error(format!("invalid number at position {}", start));
                return None;
            }
            while let Some(b'0'..=b'9') = self.peek() {
                self.advance();
            }
        }
        let num_str = std::str::from_utf8(&self.src[start..self.pos]).ok()?;
        if is_float {
            let f: f64 = num_str.parse().ok()?;
            Some(Value::float(f))
        } else {
            match num_str.parse::<i64>() {
                Ok(i) => Some(Value::integer(i)),
                Err(_) => {
                    // BigInt fallback
                    let f: f64 = num_str.parse().ok()?;
                    Some(Value::float(f))
                }
            }
        }
    }

    fn parse_object(&mut self, vm: &mut Executor, globals: &mut Globals) -> Option<Value> {
        self.advance(); // skip {
        self.skip_ws();
        let mut map = RubyMap::default();
        if self.peek() == Some(b'}') {
            self.advance();
            return Some(Value::hash(map));
        }
        loop {
            self.skip_ws();
            if self.peek() != Some(b'"') {
                self.set_error(format!("expected string key at position {}", self.pos));
                return None;
            }
            let key = self.parse_key()?;
            if !self.expect(b':') {
                return None;
            }
            let val = self.parse_value(vm, globals)?;
            map.insert(key, val, vm, globals).ok()?;
            self.skip_ws();
            match self.peek() {
                Some(b',') => {
                    self.advance();
                }
                Some(b'}') => {
                    self.advance();
                    return Some(Value::hash(map));
                }
                _ => {
                    self.set_error(format!("expected ',' or '}}' at position {}", self.pos));
                    return None;
                }
            }
        }
    }

    fn parse_array(&mut self, vm: &mut Executor, globals: &mut Globals) -> Option<Value> {
        self.advance(); // skip [
        self.skip_ws();
        let mut arr = vec![];
        if self.peek() == Some(b']') {
            self.advance();
            return Some(Value::array_from_vec(arr));
        }
        loop {
            let val = self.parse_value(vm, globals)?;
            arr.push(val);
            self.skip_ws();
            match self.peek() {
                Some(b',') => {
                    self.advance();
                }
                Some(b']') => {
                    self.advance();
                    return Some(Value::array_from_vec(arr));
                }
                _ => {
                    self.set_error(format!("expected ',' or ']' at position {}", self.pos));
                    return None;
                }
            }
        }
    }

    fn parse_literal(&mut self, expected: &[u8], value: Value) -> Option<Value> {
        if self.src[self.pos..].starts_with(expected) {
            self.pos += expected.len();
            Some(value)
        } else {
            self.set_error(format!("unexpected token at position {}", self.pos));
            None
        }
    }
}

// ---------------------------------------------------------------------------
// JSON Generator
// ---------------------------------------------------------------------------

/// Append `val`'s JSON to `buf`. Every string goes out as UTF-8, and
/// one that cannot is the error: `(message, invalid_object)` — the
/// message CRuby's generator gives, `source sequence is
/// illegal/malformed utf-8` for an ill-formed UTF-8 string and the
/// transcoding's own message for any other encoding.
fn generate(buf: &mut String, val: Value, store: &Store) -> std::result::Result<(), (String, Value)> {
    if val.is_nil() {
        buf.push_str("null");
    } else if val == Value::bool(true) {
        buf.push_str("true");
    } else if val == Value::bool(false) {
        buf.push_str("false");
    } else if let Some(i) = val.try_fixnum() {
        buf.push_str(&i.to_string());
    } else if let Some(f) = val.try_float() {
        if f.is_nan() {
            buf.push_str("NaN");
        } else if f.is_infinite() {
            if f > 0.0 {
                buf.push_str("Infinity");
            } else {
                buf.push_str("-Infinity");
            }
        } else {
            buf.push_str(&dtoa::Buffer::new().format(f));
        }
    } else if let Some(s) = val.is_rstring_inner() {
        let utf8 = json_utf8(s, store).map_err(|msg| (msg, val))?;
        generate_string(buf, &utf8);
    } else if let Some(ary) = val.try_array_ty() {
        buf.push('[');
        let items: Vec<Value> = ary.iter().copied().collect();
        for (i, v) in items.iter().enumerate() {
            if i > 0 {
                buf.push(',');
            }
            generate(buf, *v, store)?;
        }
        buf.push(']');
    } else if let Some(hash) = val.try_hash_ty() {
        buf.push('{');
        let pairs: Vec<(Value, Value)> = hash.iter().collect();
        for (i, (k, v)) in pairs.iter().enumerate() {
            if i > 0 {
                buf.push(',');
            }
            if let Some(s) = k.is_rstring_inner() {
                let utf8 = json_utf8(s, store).map_err(|msg| (msg, *k))?;
                generate_string(buf, &utf8);
            } else {
                generate_string(buf, &k.to_s(store));
            }
            buf.push(':');
            generate(buf, *v, store)?;
        }
        buf.push('}');
    } else {
        generate_string(buf, &val.to_s(store));
    }
    Ok(())
}

/// A string's characters as UTF-8, the way CRuby's generator reads
/// them: a UTF-8 string must be well-formed; a BINARY string whose
/// bytes are well-formed UTF-8 is taken for UTF-8; everything else is
/// transcoded, and the transcoding's failure is the message.
fn json_utf8<'a>(
    s: &'a RStringInner,
    store: &Store,
) -> std::result::Result<std::borrow::Cow<'a, str>, String> {
    const MALFORMED: &str = "source sequence is illegal/malformed utf-8";
    let bytes = s.as_bytes();
    match s.encoding() {
        Encoding::UTF8 => std::str::from_utf8(bytes)
            .map(std::borrow::Cow::Borrowed)
            .map_err(|_| MALFORMED.to_string()),
        enc => {
            if enc == Encoding::Ascii8
                && let Ok(st) = std::str::from_utf8(bytes)
            {
                return Ok(std::borrow::Cow::Borrowed(st));
            }
            let out = super::encoding::transcode_bytes_with_opts(
                bytes,
                enc,
                Encoding::UTF8,
                &super::encoding::TranscodeOpts::default(),
                store,
            )
            .map_err(|e| e.message().to_string())?;
            String::from_utf8(out)
                .map(std::borrow::Cow::Owned)
                .map_err(|_| MALFORMED.to_string())
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn json_generate_writes_every_string_as_utf8() {
        // CRuby's generator: a UTF-8 string must be well-formed, a
        // BINARY string with well-formed UTF-8 bytes is taken for UTF-8,
        // everything else is transcoded and the transcoding's failure
        // is the GeneratorError's message; `invalid_object` names the
        // string, wherever it sat.
        run_test_once(
            r##"
            require "json"
            e = ->(&b) { begin; b.call; rescue JSON::GeneratorError => x; [x.class, x.message, x.invalid_object, x.detailed_message]; end }
            h = ->(s) { s.unpack1("H*") }
            [e.() { JSON.generate(["a\xFFb"]) },
             e.() { JSON.generate(["a\xE3\x81"]) },
             e.() { JSON.generate({"k" => ["a\xFFb"]}) },
             e.() { JSON.generate({"\xFF" => 1}) },
             e.() { JSON.generate("a\xFFb") },
             [h.(JSON.generate(["abc".b])), JSON.generate(["abc".b]).encoding.to_s],
             h.(JSON.generate(["\xE3\x81\x82".b])),
             e.() { JSON.generate(["\xFF".b]) },
             e.() { JSON.generate(["a\x80".force_encoding("US-ASCII")]) },
             h.(JSON.generate(["\u3042".encode("EUC-JP")])),
             h.(JSON.generate(["\u3042".encode("Shift_JIS")])),
             h.(JSON.generate(["\u3042".encode("UTF-16LE")])),
             h.(JSON.generate(["\u3042".encode("ISO-2022-JP")])),
             h.(JSON.generate({"\u3042".encode("EUC-JP") => 1})),
             e.() { JSON.generate(["a\xA4".force_encoding("EUC-JP")]) },
             e.() { JSON.generate(["a".encode("UTF-16LE").byteslice(0, 1)]) }.first(2),
             e.() { JSON.generate(["abc".force_encoding("UTF-7")]) }.first(2),
             e.() { JSON.dump(["a\xFFb"]) },
             JSON.generate(["\u3042"]).bytes]
            "##,
        );
    }

    #[test]
    fn json_parse_reads_the_source_as_utf8() {
        // `convert_encoding`: a UTF-8 source is read as it is, ill-formed
        // bytes and all; a BINARY source is taken for UTF-8; any other
        // encoding is transcoded first, and its failure is an
        // `Encoding::*Error`. Every string in the result is UTF-8, and a
        // key is frozen.
        run_test_once(
            r##"
            require "json"
            e = ->(&b) { begin; b.call; rescue => x; [x.class, x.message]; end }
            d = ->(v) { v.map { |s| [s.unpack1("H*"), s.encoding.to_s, s.valid_encoding?] } }
            [d.(JSON.parse("[\"a\xFFb\"]")),
             d.(JSON.parse("[\"a\xE3\x81\"]")),
             d.(JSON.parse("[\"abc\"]".b)),
             d.(JSON.parse("[\"\xE3\x81\x82\"]".b)),
             d.(JSON.parse("[\"\xFF\"]".b)),
             d.(JSON.parse("[\"\u3042\"]".encode("EUC-JP"))),
             d.(JSON.parse("[\"\u3042\"]".encode("Shift_JIS"))),
             d.(JSON.parse("[\"\u3042\"]".encode("UTF-16LE"))),
             d.(JSON.parse("[\"abc\"]".force_encoding("US-ASCII"))),
             e.() { JSON.parse("[\"a\x80\"]".force_encoding("US-ASCII")) },
             e.() { JSON.parse("[\"\xA4\"]".force_encoding("EUC-JP")) },
             e.() { JSON.parse("[\"a\"]".encode("UTF-16LE").byteslice(0, 7)) },
             e.() { JSON.parse("[1]".force_encoding("UTF-7")) },
             e.() { JSON.parse("[\"\\ud83c\"]") }.first,
             d.(JSON.parse("[\"\\ud83c\\udf63\"]")),
             JSON.parse("{\"a\":1}").keys.map { |k| [k.frozen?, k.encoding.to_s] },
             d.(JSON.parse("{\"\u3042\":1}".encode("EUC-JP")).keys),
             e.() { JSON.parse("x") }.first,
             e.() { JSON.parse(nil) },
             e.() { JSON.parse(Object.new) }.first,
             JSON.parse(Class.new { def to_str = "[2]" }.new)]
            "##,
        );
    }

    #[test]
    fn json_parse_scalars() {
        run_tests(&[
            r#"require "json"; JSON.parse("null")"#,
            r#"require "json"; JSON.parse("true")"#,
            r#"require "json"; JSON.parse("false")"#,
            r#"require "json"; JSON.parse("42")"#,
            r#"require "json"; JSON.parse("-7")"#,
            r#"require "json"; JSON.parse("0")"#,
            r#"require "json"; JSON.parse("3.14")"#,
            r#"require "json"; JSON.parse("-0.5")"#,
            r#"require "json"; JSON.parse("1e10")"#,
            r#"require "json"; JSON.parse("2.5E-3")"#,
            r#"require "json"; JSON.parse("1e+2")"#,
            r#"require "json"; JSON.parse("\"hello\"")"#,
            r#"require "json"; JSON.parse("\"\"")"#,
        ]);
    }

    #[test]
    fn json_parse_string_escapes() {
        run_tests(&[
            r#"require "json"; JSON.parse("\"line1\\nline2\"")"#,
            r#"require "json"; JSON.parse("\"tab\\there\"")"#,
            r#"require "json"; JSON.parse("\"back\\\\slash\"")"#,
            r#"require "json"; JSON.parse("\"quote\\\"here\"")"#,
            r#"require "json"; JSON.parse("\"slash\\/ok\"")"#,
            r#"require "json"; JSON.parse("\"bs\\b\"")"#,
            r#"require "json"; JSON.parse("\"ff\\f\"")"#,
            r#"require "json"; JSON.parse("\"cr\\r\"")"#,
        ]);
    }

    #[test]
    fn json_parse_unicode_escapes() {
        run_tests(&[
            // BMP character
            r#"require "json"; JSON.parse("\"\\u0041\"") == "A""#,
            r#"require "json"; JSON.parse("\"\\u00e9\"") == "é""#,
            // Surrogate pair (U+1F600 = 😀)
            r#"require "json"; JSON.parse("\"\\uD83D\\uDE00\"") == "😀""#,
        ]);
    }

    #[test]
    fn json_parse_utf8_multibyte() {
        run_test(
            r#"
            require "json"
            data = JSON.parse("{\"name\": \"Nokogiri (鋸)\"}")
            data["name"] == "Nokogiri (鋸)"
            "#,
        );
    }

    #[test]
    fn json_parse_objects() {
        run_tests(&[
            r#"require "json"; JSON.parse("{}")"#,
            r#"require "json"; JSON.parse("{\"a\":1}")"#,
            r#"require "json"; JSON.parse("{\"a\":1,\"b\":2}")"#,
            r#"require "json"; JSON.parse("{\"x\":{\"y\":3}}")"#,
        ]);
    }

    #[test]
    fn json_parse_arrays() {
        run_tests(&[
            r#"require "json"; JSON.parse("[]")"#,
            r#"require "json"; JSON.parse("[1]")"#,
            r#"require "json"; JSON.parse("[1,2,3]")"#,
            r#"require "json"; JSON.parse("[[1],[2]]")"#,
        ]);
    }

    #[test]
    fn json_parse_nested() {
        run_test(
            r#"
            require "json"
            data = JSON.parse('{"a":[1,{"b":true}],"c":null}')
            [data["a"][0], data["a"][1]["b"], data["c"]]
            "#,
        );
    }

    #[test]
    fn json_parse_whitespace() {
        run_test(
            r#"
            require "json"
            JSON.parse("  {  \"a\" : 1 ,  \"b\" :  [ 2 , 3 ]  }  ")
            "#,
        );
    }

    #[test]
    fn json_parse_large_integer() {
        run_tests(&[
            r#"require "json"; JSON.parse("9223372036854775807")"#,
            r#"require "json"; JSON.parse("-9223372036854775808")"#,
        ]);
    }

    #[test]
    fn json_parse_errors() {
        run_test_error(r#"require "json"; JSON.parse("")"#);
        run_test_error(r#"require "json"; JSON.parse("{invalid}")"#);
        run_test_error(r#"require "json"; JSON.parse("[1,]")"#);
    }

    #[test]
    fn json_generate_scalars() {
        run_tests(&[
            r#"require "json"; JSON.generate(nil)"#,
            r#"require "json"; JSON.generate(true)"#,
            r#"require "json"; JSON.generate(false)"#,
            r#"require "json"; JSON.generate(42)"#,
            r#"require "json"; JSON.generate(-7)"#,
            r#"require "json"; JSON.generate(3.14)"#,
            r#"require "json"; JSON.generate("hello")"#,
            r#"require "json"; JSON.generate("")"#,
        ]);
    }

    #[test]
    fn json_generate_string_escapes() {
        run_tests(&[
            r#"require "json"; JSON.generate("line\n")"#,
            r#"require "json"; JSON.generate("tab\t")"#,
            r#"require "json"; JSON.generate("q\"q")"#,
            r#"require "json"; JSON.generate("b\\s")"#,
            r#"require "json"; JSON.generate("\b\f\r")"#,
        ]);
    }

    #[test]
    fn json_generate_collections() {
        run_tests(&[
            r#"require "json"; JSON.generate([])"#,
            r#"require "json"; JSON.generate([1, 2, 3])"#,
            r#"require "json"; JSON.generate([[1], [2]])"#,
            r#"require "json"; JSON.generate({})"#,
            r#"require "json"; JSON.generate({"a" => 1})"#,
            r#"require "json"; JSON.generate({"a" => [true, false, nil]})"#,
            r#"require "json"; JSON.generate({"a" => {"b" => 1}})"#,
        ]);
    }

    #[test]
    fn json_generate_special_floats() {
        run_tests(&[
            r#"require "json"; JSON.generate(0.0)"#,
            r#"require "json"; JSON.generate(-0.0)"#,
            r#"require "json"; JSON.generate(1.5)"#,
            r#"require "json"; JSON.generate(-2.5)"#,
        ]);
    }

    #[test]
    fn json_roundtrip() {
        run_test(
            r#"
            require "json"
            original = {"key" => [1, "two", nil, true, false, 3.14]}
            json_str = JSON.generate(original)
            parsed = JSON.parse(json_str)
            parsed == original
            "#,
        );
    }

    #[test]
    fn json_roundtrip_string_escapes() {
        run_test(
            r#"
            require "json"
            s = "tab\there\nnewline\r\nand\\backslash\"quote"
            JSON.parse(JSON.generate(s)) == s
            "#,
        );
    }
}

fn generate_string(buf: &mut String, s: &str) {
    buf.push('"');
    for ch in s.chars() {
        match ch {
            '"' => buf.push_str("\\\""),
            '\\' => buf.push_str("\\\\"),
            '\n' => buf.push_str("\\n"),
            '\r' => buf.push_str("\\r"),
            '\t' => buf.push_str("\\t"),
            '\x08' => buf.push_str("\\b"),
            '\x0C' => buf.push_str("\\f"),
            c if c < '\x20' => {
                buf.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => buf.push(c),
        }
    }
    buf.push('"');
}
