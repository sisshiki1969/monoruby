use super::*;

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(STRING_CLASS, "__json_parse", json_parse, 1);
}

/// String.__json_parse(source) class method
#[monoruby_builtin]
fn json_parse(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let src = lfp.arg(0).expect_string(globals)?;
    let mut parser = Parser::new(src.as_bytes());
    parser
        .parse_value(vm, globals)
        .ok_or_else(|| MonorubyErr::runtimeerr(parser.error_message()))
}

/// Fast native JSON generator: JSON.__generate(obj) -> String

/// Fast native JSON generator: JSON.__generate(obj) -> String
#[monoruby_builtin]
fn json_generate(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut buf = String::new();
    generate(&mut buf, lfp.arg(0), &globals.store);
    Ok(Value::string(buf))
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

    fn parse_string(&mut self) -> Option<Value> {
        let s = self.parse_string_raw()?;
        Some(Value::string(s))
    }

    fn parse_string_raw(&mut self) -> Option<String> {
        self.advance(); // skip opening "
        // Scan for closing " to find the raw extent, handling escapes
        // by collecting into a String.
        let mut s = String::new();
        loop {
            let ch = *self.src.get(self.pos)?;
            self.pos += 1;
            match ch {
                b'"' => return Some(s),
                b'\\' => {
                    let esc = *self.src.get(self.pos)?;
                    self.pos += 1;
                    match esc {
                        b'"' => s.push('"'),
                        b'\\' => s.push('\\'),
                        b'/' => s.push('/'),
                        b'b' => s.push('\x08'),
                        b'f' => s.push('\x0C'),
                        b'n' => s.push('\n'),
                        b'r' => s.push('\r'),
                        b't' => s.push('\t'),
                        b'u' => {
                            let cp = self.parse_unicode_escape()?;
                            if (0xD800..=0xDBFF).contains(&cp) {
                                if self.src.get(self.pos) == Some(&b'\\')
                                    && self.src.get(self.pos + 1) == Some(&b'u')
                                {
                                    self.pos += 2;
                                    let low = self.parse_unicode_escape()?;
                                    if (0xDC00..=0xDFFF).contains(&low) {
                                        let combined =
                                            0x10000 + ((cp - 0xD800) << 10) + (low - 0xDC00);
                                        s.push(char::from_u32(combined)?);
                                    } else {
                                        s.push(char::REPLACEMENT_CHARACTER);
                                    }
                                } else {
                                    s.push(char::REPLACEMENT_CHARACTER);
                                }
                            } else {
                                s.push(char::from_u32(cp)?);
                            }
                        }
                        _ => {
                            s.push('\\');
                            s.push(esc as char);
                        }
                    }
                }
                // Multi-byte UTF-8: decode the full sequence
                b if b >= 0x80 => {
                    self.pos -= 1; // back up to re-read the lead byte
                    let remaining = &self.src[self.pos..];
                    match std::str::from_utf8(remaining) {
                        Ok(rest) => {
                            let c = rest.chars().next()?;
                            s.push(c);
                            self.pos += c.len_utf8();
                        }
                        Err(e) => {
                            // Partial valid prefix
                            let valid_len = e.valid_up_to();
                            if valid_len > 0 {
                                let valid = std::str::from_utf8(&remaining[..valid_len]).ok()?;
                                let c = valid.chars().next()?;
                                s.push(c);
                                self.pos += c.len_utf8();
                            } else {
                                // Skip invalid byte
                                s.push(char::REPLACEMENT_CHARACTER);
                                self.pos += 1;
                            }
                        }
                    }
                }
                _ => s.push(ch as char),
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

    fn parse_object(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Option<Value> {
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
                self.set_error(format!(
                    "expected string key at position {}",
                    self.pos
                ));
                return None;
            }
            let key = self.parse_string()?;
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
                    self.set_error(format!(
                        "expected ',' or '}}' at position {}",
                        self.pos
                    ));
                    return None;
                }
            }
        }
    }

    fn parse_array(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Option<Value> {
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
                    self.set_error(format!(
                        "expected ',' or ']' at position {}",
                        self.pos
                    ));
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

fn generate(buf: &mut String, val: Value, store: &Store) {
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
    } else if let Some(s) = val.is_str() {
        generate_string(buf, s);
    } else if let Some(ary) = val.try_array_ty() {
        buf.push('[');
        let items: Vec<Value> = ary.iter().copied().collect();
        for (i, v) in items.iter().enumerate() {
            if i > 0 {
                buf.push(',');
            }
            generate(buf, *v, store);
        }
        buf.push(']');
    } else if let Some(hash) = val.try_hash_ty() {
        buf.push('{');
        let pairs: Vec<(Value, Value)> = hash.iter().collect();
        for (i, (k, v)) in pairs.iter().enumerate() {
            if i > 0 {
                buf.push(',');
            }
            generate_string(buf, &k.to_s(store));
            buf.push(':');
            generate(buf, *v, store);
        }
        buf.push('}');
    } else {
        generate_string(buf, &val.to_s(store));
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
