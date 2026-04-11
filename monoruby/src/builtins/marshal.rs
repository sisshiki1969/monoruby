use super::*;
use num::BigInt;

//
// Marshal module
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_toplevel_module("Marshal").id();
    globals.define_builtin_module_func(klass, "dump", dump, 1);
    globals.define_builtin_module_func(klass, "load", load, 1);
    globals.define_builtin_module_func(klass, "restore", load, 1);
}

/// ### Marshal.dump
/// - dump(obj) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Marshal/m/dump.html]
#[monoruby_builtin]
fn dump(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let obj = lfp.arg(0);
    let mut buf: Vec<u8> = Vec::new();
    // Marshal version header
    buf.push(0x04);
    buf.push(0x08);
    let mut symbols: Vec<IdentId> = Vec::new();
    marshal_dump_value(&mut buf, obj, globals, &mut symbols)?;
    Ok(Value::bytes(buf))
}

/// ### Marshal.load
/// - load(source) -> Object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Marshal/m/load.html]
#[monoruby_builtin]
fn load(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let source = lfp.arg(0);
    let data = source.expect_bytes(&globals.store)?;
    if data.len() < 2 {
        return Err(MonorubyErr::argumenterr("marshal data too short"));
    }
    if data[0] != 0x04 || data[1] != 0x08 {
        return Err(MonorubyErr::typeerr(format!(
            "incompatible marshal file format (can't be read)\n\
             \tformat version {}.{} required; {}.{} given",
            4, 8, data[0], data[1]
        )));
    }
    let mut cursor = MarshalReader::new(&data[2..]);
    let value = cursor.read_value(vm, globals)?;
    Ok(value)
}

// ============================================================
// Marshal reader (deserializer)
// ============================================================

struct MarshalReader<'a> {
    data: &'a [u8],
    pos: usize,
    /// Symbol table for back-references (';' tag)
    symbols: Vec<IdentId>,
    /// Object table for back-references ('@' tag)
    objects: Vec<Value>,
}

impl<'a> MarshalReader<'a> {
    fn new(data: &'a [u8]) -> Self {
        MarshalReader {
            data,
            pos: 0,
            symbols: Vec::new(),
            objects: Vec::new(),
        }
    }

    fn read_byte(&mut self) -> Result<u8> {
        if self.pos >= self.data.len() {
            return Err(MonorubyErr::argumenterr(
                "marshal data too short",
            ));
        }
        let b = self.data[self.pos];
        self.pos += 1;
        Ok(b)
    }

    fn read_bytes(&mut self, n: usize) -> Result<&'a [u8]> {
        if self.pos + n > self.data.len() {
            return Err(MonorubyErr::argumenterr(
                "marshal data too short",
            ));
        }
        let slice = &self.data[self.pos..self.pos + n];
        self.pos += n;
        Ok(slice)
    }

    /// Read a Marshal-format integer.
    ///
    /// Encoding (inverse of marshal_write_fixnum):
    /// - 0x00 → 0
    /// - 0x06..0x7f (1..122 after subtracting 5) → positive small int
    /// - 0x80..0xfa (-123..-1 after adding 5) → negative small int
    /// - 0x01..0x04 → positive multi-byte (1..4 LE bytes follow)
    /// - 0xfc..0xff → negative multi-byte (1..4 LE bytes follow, count = -n)
    fn read_fixnum(&mut self) -> Result<i32> {
        let b = self.read_byte()? as i8;
        if b == 0 {
            return Ok(0);
        }
        if b > 0 {
            if b <= 4 {
                // 1..4 positive bytes follow
                let nbytes = b as usize;
                let bytes = self.read_bytes(nbytes)?;
                let mut result: u32 = 0;
                for i in 0..nbytes {
                    result |= (bytes[i] as u32) << (i * 8);
                }
                Ok(result as i32)
            } else {
                // small positive: b - 5
                Ok(b as i32 - 5)
            }
        } else {
            // b < 0
            if b >= -4 {
                // -1..-4: negative multi-byte
                let nbytes = (-b) as usize;
                let bytes = self.read_bytes(nbytes)?;
                let mut result: u32 = 0xffffffff;
                for i in 0..nbytes {
                    // Clear byte position and set from data
                    result &= !(0xff << (i * 8));
                    result |= (bytes[i] as u32) << (i * 8);
                }
                Ok(result as i32)
            } else {
                // small negative: b + 5
                Ok(b as i32 + 5)
            }
        }
    }

    /// Read a symbol (type ':') and register it in the symbol table.
    fn read_new_symbol(&mut self) -> Result<IdentId> {
        let len = self.read_fixnum()? as usize;
        let bytes = self.read_bytes(len)?;
        let name = std::str::from_utf8(bytes).map_err(|_| {
            MonorubyErr::argumenterr("invalid symbol encoding in marshal data")
        })?;
        let id = IdentId::get_id(name);
        self.symbols.push(id);
        Ok(id)
    }

    /// Read a symbol, handling both ':' (new symbol) and ';' (symbol reference).
    fn read_symbol(&mut self) -> Result<IdentId> {
        let tag = self.read_byte()?;
        match tag {
            b':' => self.read_new_symbol(),
            b';' => {
                let idx = self.read_fixnum()? as usize;
                self.symbols.get(idx).copied().ok_or_else(|| {
                    MonorubyErr::argumenterr(format!(
                        "bad symbol reference in marshal data: {}",
                        idx
                    ))
                })
            }
            _ => Err(MonorubyErr::argumenterr(format!(
                "expected symbol tag (':' or ';'), got 0x{:02x}",
                tag
            ))),
        }
    }

    /// Read and return the next marshalled value.
    fn read_value(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Value> {
        let tag = self.read_byte()?;
        match tag {
            b'0' => Ok(Value::nil()),
            b'T' => Ok(Value::bool(true)),
            b'F' => Ok(Value::bool(false)),
            b'i' => {
                let n = self.read_fixnum()?;
                Ok(Value::integer(n as i64))
            }
            b'l' => self.read_bignum(),
            b'f' => self.read_float(),
            b':' => {
                let id = self.read_new_symbol()?;
                Ok(Value::symbol(id))
            }
            b';' => {
                let idx = self.read_fixnum()? as usize;
                let id = self.symbols.get(idx).copied().ok_or_else(|| {
                    MonorubyErr::argumenterr(format!(
                        "bad symbol reference in marshal data: {}",
                        idx
                    ))
                })?;
                Ok(Value::symbol(id))
            }
            b'"' => self.read_raw_string(Encoding::Ascii8),
            b'I' => self.read_ivar_wrapped(vm, globals),
            b'[' => self.read_array(vm, globals),
            b'{' => self.read_hash(vm, globals),
            b'@' => {
                // Object reference
                let idx = self.read_fixnum()? as usize;
                self.objects.get(idx).copied().ok_or_else(|| {
                    MonorubyErr::argumenterr(format!(
                        "bad object reference in marshal data: {}",
                        idx
                    ))
                })
            }
            b'o' => self.read_user_object(vm, globals),
            b'u' => self.read_user_marshal(vm, globals),
            _ => Err(MonorubyErr::argumenterr(format!(
                "unsupported marshal type tag: 0x{:02x} ('{}')",
                tag,
                if tag.is_ascii_graphic() {
                    tag as char
                } else {
                    '?'
                }
            ))),
        }
    }

    /// Read a Bignum value.
    /// Format: sign_byte('+'/'-') + marshal_int(word_count) + LE 16-bit words
    fn read_bignum(&mut self) -> Result<Value> {
        let sign_byte = self.read_byte()?;
        let word_count = self.read_fixnum()? as usize;
        let byte_count = word_count * 2;
        let bytes = self.read_bytes(byte_count)?;
        let sign = if sign_byte == b'-' {
            num::bigint::Sign::Minus
        } else {
            num::bigint::Sign::Plus
        };
        let n = BigInt::from_bytes_le(sign, bytes);
        let val = Value::bigint(n);
        self.objects.push(val);
        Ok(val)
    }

    /// Read a Float value.
    /// Format: marshal_int(string_length) + string_bytes
    fn read_float(&mut self) -> Result<Value> {
        let len = self.read_fixnum()? as usize;
        let bytes = self.read_bytes(len)?;
        let s = std::str::from_utf8(bytes).map_err(|_| {
            MonorubyErr::argumenterr("invalid float encoding in marshal data")
        })?;
        let f = match s {
            "inf" => f64::INFINITY,
            "-inf" => f64::NEG_INFINITY,
            "nan" => f64::NAN,
            "-0" => -0.0_f64,
            "0" => 0.0_f64,
            _ => s.parse::<f64>().map_err(|_| {
                MonorubyErr::argumenterr(format!("invalid float value in marshal data: {}", s))
            })?,
        };
        let val = Value::float(f);
        self.objects.push(val);
        Ok(val)
    }

    /// Read a raw string (after the '"' tag has already been consumed).
    /// Format: marshal_int(length) + bytes
    fn read_raw_string(&mut self, encoding: Encoding) -> Result<Value> {
        let len = self.read_fixnum()? as usize;
        let bytes = self.read_bytes(len)?;
        let val = match encoding {
            Encoding::Utf8 | Encoding::UsAscii => Value::string_from_inner(RStringInner::from_encoding(bytes, Encoding::Utf8)),
            Encoding::Ascii8 => Value::bytes_from_slice(bytes),
        };
        self.objects.push(val);
        Ok(val)
    }

    /// Read an instance-variable-wrapped object.
    /// Format: inner_object + marshal_int(ivar_count) + (symbol + value) pairs
    ///
    /// The most common case is a UTF-8 string: I + " + data + 1 + :E + T
    fn read_ivar_wrapped(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Value> {
        let inner_tag = self.read_byte()?;
        match inner_tag {
            b'"' => {
                // String with instance variables (typically encoding)
                let len = self.read_fixnum()? as usize;
                let bytes = self.read_bytes(len)?;
                let ivar_count = self.read_fixnum()? as usize;
                let mut encoding = Encoding::Ascii8;
                for _ in 0..ivar_count {
                    let sym = self.read_symbol()?;
                    let sym_name = sym.get_name();
                    if sym_name == "E" {
                        // Encoding flag
                        let enc_val_tag = self.read_byte()?;
                        match enc_val_tag {
                            b'T' => encoding = Encoding::Utf8,
                            b'F' => encoding = Encoding::Ascii8,
                            _ => {
                                // Could be a string encoding name
                                self.pos -= 1;
                                let enc_val = self.read_value(vm, globals)?;
                                if let Some(s) = enc_val.is_rstring_inner() {
                                    let enc_name = std::str::from_utf8(s.as_bytes()).unwrap_or("");
                                    encoding = Encoding::try_from_str(enc_name).unwrap_or(Encoding::Ascii8);
                                }
                            }
                        }
                    } else {
                        // Skip other instance variables
                        let _val = self.read_value(vm, globals)?;
                    }
                }
                let val = Value::string_from_inner(RStringInner::from_encoding(bytes, encoding));
                self.objects.push(val);
                Ok(val)
            }
            _ => {
                // Other I-wrapped types (e.g. Regexp with encoding)
                self.pos -= 1;
                let val = self.read_value(vm, globals)?;
                // Read and skip instance variables
                let ivar_count = self.read_fixnum()? as usize;
                for _ in 0..ivar_count {
                    let _sym = self.read_symbol()?;
                    let _val = self.read_value(vm, globals)?;
                }
                Ok(val)
            }
        }
    }

    /// Read an Array.
    /// Format: marshal_int(length) + elements
    fn read_array(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Value> {
        let len = self.read_fixnum()? as usize;
        // Reserve an object slot so nested references work correctly
        let obj_idx = self.objects.len();
        self.objects.push(Value::nil()); // placeholder
        let mut elems = Vec::with_capacity(len);
        for _ in 0..len {
            let val = self.read_value(vm, globals)?;
            elems.push(val);
        }
        let arr = Value::array_from_vec(elems);
        self.objects[obj_idx] = arr;
        Ok(arr)
    }

    /// Read a Hash.
    /// Format: marshal_int(length) + (key + value) pairs
    fn read_hash(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Value> {
        let len = self.read_fixnum()? as usize;
        // Reserve an object slot
        let obj_idx = self.objects.len();
        self.objects.push(Value::nil()); // placeholder
        let mut map = RubyMap::default();
        for _ in 0..len {
            let key = self.read_value(vm, globals)?;
            let val = self.read_value(vm, globals)?;
            map.insert(key, val, vm, globals)?;
        }
        let hash = Value::hash(map);
        self.objects[obj_idx] = hash;
        Ok(hash)
    }

    /// Read a user-defined object ('o' tag).
    /// Format: symbol(class_name) + marshal_int(ivar_count) + (symbol + value) pairs
    ///
    /// We create a generic object with instance variables stored as a Hash.
    fn read_user_object(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Value> {
        let class_sym = self.read_symbol()?;
        let ivar_count = self.read_fixnum()? as usize;
        // Reserve an object slot
        let obj_idx = self.objects.len();
        self.objects.push(Value::nil());
        let class_name = class_sym.get_name();

        // Look up the class by checking constants on Object
        let class_name_id = IdentId::get_id(&class_name);
        let obj = match globals.get_constant(OBJECT_CLASS, class_name_id) {
            Some(ConstState::Loaded(class_val)) => {
                if let Some(module) = class_val.is_class_or_module() {
                    Value::object(module.id())
                } else {
                    return Err(MonorubyErr::argumenterr(format!(
                        "undefined class/module {}",
                        class_name
                    )));
                }
            }
            _ => {
                return Err(MonorubyErr::argumenterr(format!(
                    "undefined class/module {}",
                    class_name
                )));
            }
        };
        // Set instance variables
        for _ in 0..ivar_count {
            let ivar_sym = self.read_symbol()?;
            let val = self.read_value(vm, globals)?;
            globals.set_ivar(obj, ivar_sym, val)?;
        }
        self.objects[obj_idx] = obj;
        Ok(obj)
    }

    /// Read a user-marshal object ('u' tag).
    /// Format: symbol(class_name) + raw_data
    ///
    /// Currently, this reads the class name symbol, then reads and returns
    /// the nested value, effectively ignoring the class information.
    fn read_user_marshal(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Value> {
        let _class_sym = self.read_symbol()?;
        let val = self.read_value(vm, globals)?;
        Ok(val)
    }
}

// ============================================================
// Marshal writer helpers
// ============================================================

/// Write a Marshal-format integer (used for lengths and fixnum values).
///
/// Encoding rules:
/// - 0 → 0x00
/// - 1..122 → n + 5
/// - -123..-1 → n - 5
/// - |n| > 122 → sign byte (1..4 or -1..-4) followed by that many LE bytes
fn marshal_write_fixnum(buf: &mut Vec<u8>, n: i32) {
    if n == 0 {
        buf.push(0x00);
    } else if n > 0 && n <= 122 {
        buf.push((n + 5) as u8);
    } else if n < 0 && n >= -123 {
        buf.push((n - 5) as u8);
    } else if n > 0 {
        let bytes = n.to_le_bytes();
        let len = if n <= 0xff {
            1
        } else if n <= 0xffff {
            2
        } else if n <= 0xffffff {
            3
        } else {
            4
        };
        buf.push(len);
        buf.extend_from_slice(&bytes[..len as usize]);
    } else {
        // negative, |n| > 123
        let bytes = n.to_le_bytes();
        let len = if n >= -0x100 {
            1
        } else if n >= -0x10000 {
            2
        } else if n >= -0x1000000 {
            3
        } else {
            4
        };
        buf.push((-len) as u8);
        buf.extend_from_slice(&bytes[..len as usize]);
    }
}

/// Write a Bignum in Marshal format.
///
/// Format: 'l' + sign_byte('+'/'-') + marshal_int(word_count) + LE 16-bit words
fn marshal_write_bignum(buf: &mut Vec<u8>, n: &BigInt) {
    use num::bigint::Sign;
    buf.push(b'l');
    let (sign, le_bytes): (num::bigint::Sign, Vec<u8>) = n.to_bytes_le();
    buf.push(if sign == Sign::Minus { b'-' } else { b'+' });
    // word count in 16-bit (2-byte) units, rounded up
    let word_count = (le_bytes.len() + 1) / 2;
    marshal_write_fixnum(buf, word_count as i32);
    // Write bytes, padding to even length if needed
    buf.extend_from_slice(&le_bytes);
    if le_bytes.len() % 2 != 0 {
        buf.push(0x00);
    }
}

/// Format a float for Marshal in the same way as CRuby's ruby_dtoa (shortest representation).
///
/// CRuby uses David Gay's dtoa mode 0 which produces the shortest decimal string
/// that uniquely identifies the float, formatted like `%g` (switching to exponential
/// when exponent < -4 or when there are trailing zeros in integer part).
///
/// Examples: 1.0 → "1", 10.0 → "1e1", 12.0 → "12", 0.1 → "0.1", 1.23e5 → "1.23e5"
fn float_to_marshal_string(f: f64) -> String {
    let neg = f.is_sign_negative();
    let f_abs = f.abs();

    // Get shortest digits from dtoa
    let s = dtoa::Buffer::new().format(f_abs).to_string();

    // Parse the dtoa output to extract significant digits and decimal exponent.
    // dtoa produces forms like "1.0", "123.0", "0.001", "0.1", "3.14159265358979",
    // or "1.23e100" for very large/small numbers.
    let (digits, exponent) = parse_dtoa_output(&s);

    // Format using ruby_dtoa rules:
    // - If exponent >= significand length and exponent > 1: use exponential (e.g. 10.0 → "1e1")
    // - If exponent <= -5: use exponential (e.g. 1e-5 → "1e-5")
    // - Otherwise: use decimal notation
    let prefix = if neg { "-" } else { "" };
    let ndigits = digits.len() as i32;

    if exponent <= -4
        || (exponent > ndigits && exponent > 1)
        || (exponent >= 1 && has_trailing_zeros(&digits, exponent))
    {
        // Exponential form
        let exp = exponent - 1;
        let sig = if ndigits == 1 {
            digits.clone()
        } else {
            format!("{}.{}", &digits[..1], &digits[1..])
        };
        format!("{prefix}{sig}e{exp}")
    } else if exponent <= 0 {
        // 0.00...0digits form
        let zeros = (-exponent) as usize;
        format!("{prefix}0.{}{}", "0".repeat(zeros), digits)
    } else if exponent >= ndigits {
        // All digits before decimal, no fraction
        format!("{prefix}{}", digits)
    } else {
        // Mixed: some digits before decimal, some after
        let exp = exponent as usize;
        format!("{prefix}{}.{}", &digits[..exp], &digits[exp..])
    }
}

/// Check if the integer representation would have trailing zeros.
/// digits = significant digits, exponent = position of decimal point from left.
/// E.g., digits="1", exponent=2 means "100" → has trailing zeros.
fn has_trailing_zeros(digits: &str, exponent: i32) -> bool {
    exponent > digits.len() as i32
}

/// Parse dtoa output into (significant_digits, exponent).
/// The exponent is the power of 10 such that the value equals 0.{digits} * 10^exponent.
/// Actually, it's the number of digits before the decimal point in the standard form.
///
/// E.g.: "1.0" → ("1", 1), "123.0" → ("123", 3), "0.001" → ("1", -2),
///        "3.14" → ("314", 1), "1.23e100" → ("123", 101)
fn parse_dtoa_output(s: &str) -> (String, i32) {
    if let Some(e_pos) = s.find('e') {
        let mantissa = &s[..e_pos];
        let exp_str = &s[e_pos + 1..];
        let exp_val: i32 = exp_str.parse().unwrap();
        let (digits, dot_exp) = parse_decimal(mantissa);
        (digits, dot_exp + exp_val)
    } else {
        parse_decimal(s)
    }
}

/// Parse a decimal string (no 'e') into (significant_digits, exponent).
/// "1.0" → ("1", 1), "123.0" → ("123", 3), "0.001" → ("1", -2), "0.1" → ("1", 0)
fn parse_decimal(s: &str) -> (String, i32) {
    if let Some(dot_pos) = s.find('.') {
        let int_part = &s[..dot_pos];
        let frac_part = &s[dot_pos + 1..];
        // Combine all significant digits (strip leading zeros from combined)
        let mut all_digits = String::new();
        all_digits.push_str(int_part);
        all_digits.push_str(frac_part);
        // Remove trailing zeros from fraction part contribution
        let trimmed = all_digits.trim_end_matches('0');
        if trimmed.is_empty() || trimmed == "0" {
            return ("0".to_string(), 1);
        }
        // Remove leading zeros
        let trimmed = trimmed.trim_start_matches('0');
        let exponent = if int_part == "0" {
            // 0.XYZ case: exponent is -(number of leading zeros in frac)
            let leading_zeros = frac_part.len() - frac_part.trim_start_matches('0').len();
            -(leading_zeros as i32)
        } else {
            int_part.len() as i32
        };
        (trimmed.to_string(), exponent)
    } else {
        // No decimal point (shouldn't happen with dtoa, but handle it)
        let trimmed = s.trim_end_matches('0');
        if trimmed.is_empty() {
            ("0".to_string(), 1)
        } else {
            (trimmed.to_string(), s.len() as i32)
        }
    }
}

/// Write a float in Marshal format.
///
/// Format: 'f' + marshal_int(string_length) + string_bytes
/// Special cases: 0.0 → "0", -0.0 → "-0", inf → "inf", -inf → "-inf", nan → "nan"
fn marshal_write_float(buf: &mut Vec<u8>, f: f64) {
    buf.push(b'f');
    let s = if f == 0.0 {
        if f.is_sign_negative() {
            "-0".to_string()
        } else {
            "0".to_string()
        }
    } else if f.is_infinite() {
        if f.is_sign_negative() {
            "-inf".to_string()
        } else {
            "inf".to_string()
        }
    } else if f.is_nan() {
        "nan".to_string()
    } else {
        float_to_marshal_string(f)
    };
    marshal_write_fixnum(buf, s.len() as i32);
    buf.extend_from_slice(s.as_bytes());
}

/// Write a Symbol in Marshal format, with symbol table dedup.
///
/// If the symbol has been seen before, write ';' + index.
/// Otherwise, write ':' + marshal_int(length) + bytes and record it.
fn marshal_write_symbol(buf: &mut Vec<u8>, id: IdentId, symbols: &mut Vec<IdentId>) {
    if let Some(idx) = symbols.iter().position(|&s| s == id) {
        buf.push(b';');
        marshal_write_fixnum(buf, idx as i32);
    } else {
        buf.push(b':');
        let name = id.get_name();
        let bytes = name.as_bytes();
        marshal_write_fixnum(buf, bytes.len() as i32);
        buf.extend_from_slice(bytes);
        symbols.push(id);
    }
}

/// Write a String in Marshal format.
///
/// - ASCII-8BIT: '"' + marshal_int(length) + bytes
/// - UTF-8: 'I' + '"' + marshal_int(length) + bytes + ivar_count(1) + :E + true
fn marshal_write_string(buf: &mut Vec<u8>, s: &RStringInner, symbols: &mut Vec<IdentId>) {
    let bytes = s.as_bytes();
    match s.encoding() {
        Encoding::Utf8 => {
            buf.push(b'I');
            buf.push(b'"');
            marshal_write_fixnum(buf, bytes.len() as i32);
            buf.extend_from_slice(bytes);
            // Instance variable: encoding = UTF-8
            marshal_write_fixnum(buf, 1); // 1 ivar
            marshal_write_symbol(buf, IdentId::get_id("E"), symbols);
            buf.push(b'T'); // true
        }
        Encoding::UsAscii | Encoding::Ascii8 => {
            buf.push(b'"');
            marshal_write_fixnum(buf, bytes.len() as i32);
            buf.extend_from_slice(bytes);
        }
    }
}

fn marshal_dump_value(
    buf: &mut Vec<u8>,
    obj: Value,
    globals: &Globals,
    symbols: &mut Vec<IdentId>,
) -> Result<()> {
    match obj.unpack() {
        RV::Nil => {
            buf.push(b'0'); // 0x30
        }
        RV::Bool(true) => {
            buf.push(b'T'); // 0x54
        }
        RV::Bool(false) => {
            buf.push(b'F'); // 0x46
        }
        RV::Fixnum(n) => {
            // Fixnums that fit in 30-bit signed range use 'i' format
            // Larger fixnums must use bignum 'l' format
            if n >= -0x40000000 && n <= 0x3fffffff {
                buf.push(b'i');
                marshal_write_fixnum(buf, n as i32);
            } else {
                // i64 values outside 30-bit range → treat as bignum
                let big = BigInt::from(n);
                marshal_write_bignum(buf, &big);
            }
        }
        RV::BigInt(n) => {
            // Check if it fits in fixnum range
            use num::ToPrimitive;
            if let Some(i) = n.to_i32() {
                if i >= -0x40000000 && i <= 0x3fffffff {
                    buf.push(b'i');
                    marshal_write_fixnum(buf, i);
                    return Ok(());
                }
            }
            marshal_write_bignum(buf, n);
        }
        RV::Float(f) => {
            marshal_write_float(buf, f);
        }
        RV::Symbol(id) => {
            marshal_write_symbol(buf, id, symbols);
        }
        RV::String(s) => {
            marshal_write_string(buf, s, symbols);
        }
        RV::Object(_rv) => {
            // Check for Array and Hash via ty()
            match obj.ty() {
                Some(ObjTy::ARRAY) => {
                    let inner = obj.as_array_inner();
                    buf.push(b'[');
                    marshal_write_fixnum(buf, inner.len() as i32);
                    for elem in inner.iter() {
                        marshal_dump_value(buf, *elem, globals, symbols)?;
                    }
                }
                Some(ObjTy::HASH) => {
                    let inner = obj.as_hashmap_inner();
                    buf.push(b'{');
                    marshal_write_fixnum(buf, inner.len() as i32);
                    for (k, v) in inner.iter() {
                        marshal_dump_value(buf, k, globals, symbols)?;
                        marshal_dump_value(buf, v, globals, symbols)?;
                    }
                }
                Some(ObjTy::OBJECT) => {
                    // User-defined object with instance variables: 'o' tag
                    let class_id = obj.class();
                    let class_name = globals.get_class_name(class_id);
                    let class_name_id = IdentId::get_id(&class_name);
                    let ivars = globals.get_ivars(obj);
                    buf.push(b'o');
                    marshal_write_symbol(buf, class_name_id, symbols);
                    marshal_write_fixnum(buf, ivars.len() as i32);
                    for (name, val) in ivars {
                        marshal_write_symbol(buf, name, symbols);
                        marshal_dump_value(buf, val, globals, symbols)?;
                    }
                }
                _ => {
                    return Err(MonorubyErr::typeerr(format!(
                        "no _dump_data is defined for class {}",
                        globals.get_class_name(obj.class())
                    )));
                }
            }
        }
        _ => {
            return Err(MonorubyErr::typeerr(format!(
                "no _dump_data is defined for class {}",
                globals.get_class_name(obj.class())
            )));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn marshal_dump_nil() {
        run_test("Marshal.dump(nil)");
    }

    #[test]
    fn marshal_dump_true() {
        run_test("Marshal.dump(true)");
    }

    #[test]
    fn marshal_dump_false() {
        run_test("Marshal.dump(false)");
    }

    #[test]
    fn marshal_dump_integers() {
        run_tests(&[
            "Marshal.dump(0)",
            "Marshal.dump(1)",
            "Marshal.dump(-1)",
            "Marshal.dump(5)",
            "Marshal.dump(-5)",
            "Marshal.dump(122)",
            "Marshal.dump(-123)",
            "Marshal.dump(123)",
            "Marshal.dump(-124)",
            "Marshal.dump(255)",
            "Marshal.dump(256)",
            "Marshal.dump(65535)",
            "Marshal.dump(65536)",
            "Marshal.dump(0xffffff)",
            "Marshal.dump(0x1000000)",
            "Marshal.dump(0x3fffffff)",
            "Marshal.dump(-0x40000000)",
        ]);
    }

    #[test]
    fn marshal_dump_bignum() {
        run_test("Marshal.dump(0x40000000)");
        run_test("Marshal.dump(-0x40000001)");
        run_test("Marshal.dump(2**64)");
        run_test("Marshal.dump(-(2**64))");
    }

    #[test]
    fn marshal_dump_float() {
        //run_test("Marshal.dump(-0.0)"); // monoruby doesn't preserve -0.0 literal yet
        run_tests(&[
            "Marshal.dump(0.0)",
            "Marshal.dump(1.0)",
            "Marshal.dump(-1.5)",
            "Marshal.dump(1.0/0)",
            "Marshal.dump(-1.0/0)",
            "Marshal.dump(0.0/0.0)",
            "Marshal.dump(3.14159265358979)",
            "Marshal.dump(100.0)",
            "Marshal.dump(12.0)",
            "Marshal.dump(0.1)",
            "Marshal.dump(0.001)",
            "Marshal.dump(1.23e5)",
            "Marshal.dump(1.23e-5)",
            "Marshal.dump(1.7976931348623157e308)",
            "Marshal.dump(0.0001)",
            "Marshal.dump(0.00001)",
            "Marshal.dump(-100.0)",
            "Marshal.dump(-1.23e-5)",
            "Marshal.dump(2.718281828459045)",
            "Marshal.dump(10.0)",
            "Marshal.dump(1000.0)",
        ]);
    }

    #[test]
    fn marshal_dump_symbol() {
        run_test("Marshal.dump(:hello)");
        run_test("Marshal.dump(:foo)");
        run_test("Marshal.dump(:\"\")");
        run_test("Marshal.dump(:a)");
        run_test("Marshal.dump(:marshal_test_symbol)");
    }

    #[test]
    fn marshal_dump_string() {
        run_test(r#"Marshal.dump("hello")"#);
        run_test(r#"Marshal.dump("")"#);
        run_test(r#"Marshal.dump("a")"#);
        run_test(r#"Marshal.dump("hello world")"#);
        run_test(r#"Marshal.dump("hello".b)"#);
        run_test(r#"Marshal.dump("".b)"#);
    }

    #[test]
    fn marshal_dump_array() {
        run_test("Marshal.dump([])");
        run_test("Marshal.dump([1, 2, 3])");
        run_test(r#"Marshal.dump([1, "hello", :foo, nil, true, false])"#);
        run_test("Marshal.dump([[1, 2], [3, 4]])");
    }

    #[test]
    fn marshal_dump_hash() {
        run_test("Marshal.dump({})");
        run_test(r#"Marshal.dump({a: 1, b: 2})"#);
        run_test(r#"Marshal.dump({"key" => "value"})"#);
    }

    #[test]
    fn marshal_load_nil() {
        run_test("Marshal.load(Marshal.dump(nil))");
    }

    #[test]
    fn marshal_load_bool() {
        run_test("Marshal.load(Marshal.dump(true))");
        run_test("Marshal.load(Marshal.dump(false))");
    }

    #[test]
    fn marshal_load_integers() {
        run_tests(&[
            "Marshal.load(Marshal.dump(0))",
            "Marshal.load(Marshal.dump(1))",
            "Marshal.load(Marshal.dump(-1))",
            "Marshal.load(Marshal.dump(122))",
            "Marshal.load(Marshal.dump(-123))",
            "Marshal.load(Marshal.dump(123))",
            "Marshal.load(Marshal.dump(-124))",
            "Marshal.load(Marshal.dump(255))",
            "Marshal.load(Marshal.dump(256))",
            "Marshal.load(Marshal.dump(65535))",
            "Marshal.load(Marshal.dump(65536))",
            "Marshal.load(Marshal.dump(0xffffff))",
            "Marshal.load(Marshal.dump(0x1000000))",
            "Marshal.load(Marshal.dump(0x3fffffff))",
            "Marshal.load(Marshal.dump(-0x40000000))",
        ]);
    }

    #[test]
    fn marshal_load_bignum() {
        run_test("Marshal.load(Marshal.dump(0x40000000))");
        run_test("Marshal.load(Marshal.dump(-0x40000001))");
        run_test("Marshal.load(Marshal.dump(2**64))");
        run_test("Marshal.load(Marshal.dump(-(2**64)))");
    }

    #[test]
    fn marshal_load_float() {
        run_test("Marshal.load(Marshal.dump(0.0))");
        run_test("Marshal.load(Marshal.dump(1.0))");
        run_test("Marshal.load(Marshal.dump(-1.5))");
        run_test("Marshal.load(Marshal.dump(3.14159265358979))");
        run_test("Marshal.load(Marshal.dump(100.0))");
        run_test("Marshal.load(Marshal.dump(0.1))");
    }

    #[test]
    fn marshal_load_symbol() {
        run_test("Marshal.load(Marshal.dump(:hello))");
        run_test("Marshal.load(Marshal.dump(:foo))");
        run_test("Marshal.load(Marshal.dump(:a))");
    }

    #[test]
    fn marshal_load_string() {
        run_test(r#"Marshal.load(Marshal.dump("hello"))"#);
        run_test(r#"Marshal.load(Marshal.dump(""))"#);
        run_test(r#"Marshal.load(Marshal.dump("hello world"))"#);
        run_test(r#"Marshal.load(Marshal.dump("hello".b))"#);
    }

    #[test]
    fn marshal_load_array() {
        run_test("Marshal.load(Marshal.dump([]))");
        run_test("Marshal.load(Marshal.dump([1, 2, 3]))");
        run_test(r#"Marshal.load(Marshal.dump([1, "hello", :foo, nil, true, false]))"#);
        run_test("Marshal.load(Marshal.dump([[1, 2], [3, 4]]))");
    }

    #[test]
    fn marshal_load_hash() {
        run_test("Marshal.load(Marshal.dump({}))");
        run_test(r#"Marshal.load(Marshal.dump({a: 1, b: 2}))"#);
        run_test(r#"Marshal.load(Marshal.dump({"key" => "value"}))"#);
    }

    #[test]
    fn marshal_roundtrip_complex() {
        run_test(r#"Marshal.load(Marshal.dump({a: [1, 2, 3], b: "hello", c: {d: :e}}))"#);
    }

    #[test]
    fn marshal_dump_user_object() {
        run_test(
            r#"
            class Foo
              def initialize(x, y)
                @x = x
                @y = y
              end
            end
            Marshal.dump(Foo.new(1, 2))
        "#,
        );
        run_test(
            r#"
            class Bar
              def initialize
                @name = "hello"
                @value = 42
                @items = [1, 2, 3]
              end
            end
            Marshal.dump(Bar.new)
        "#,
        );
    }

    #[test]
    fn marshal_roundtrip_user_object() {
        run_test(
            r#"
            class Foo
              attr_accessor :x, :y
              def initialize(x, y)
                @x = x
                @y = y
              end
            end
            obj = Marshal.load(Marshal.dump(Foo.new(1, 2)))
            [obj.class.name, obj.x, obj.y]
        "#,
        );
        run_test(
            r#"
            class Baz
              attr_accessor :name, :items
              def initialize
                @name = "test"
                @items = [1, :foo, nil]
              end
            end
            obj = Marshal.load(Marshal.dump(Baz.new))
            [obj.name, obj.items]
        "#,
        );
    }

    #[test]
    fn marshal_load_errors() {
        // Empty data
        run_test_error(r#"Marshal.load("")"#);
        // Too short (only 1 byte)
        run_test_error(r#"Marshal.load("\x04")"#);
        // Wrong version
        run_test_error(r#"Marshal.load("\x03\x08")"#);
        // Unsupported type tag
        run_test_error(r#"Marshal.load("\x04\x08Q")"#);
    }

    #[test]
    fn marshal_dump_unsupported() {
        // Regexp is not supported
        run_test_error(r#"Marshal.dump(/foo/)"#);
    }

    #[test]
    fn marshal_version_constants() {
        run_test(r##"Marshal::MAJOR_VERSION"##);
        run_test(r##"Marshal::MINOR_VERSION"##);
        run_test(r##"[Marshal::MAJOR_VERSION, Marshal::MINOR_VERSION]"##);
    }
}
