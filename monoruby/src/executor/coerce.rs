use super::*;
use crate::value::IntegerBase;


/// Apply width padding to a string, with left or right alignment.
fn apply_width(s: &str, width: usize, left_align: bool, pad: char) -> String {
    if s.len() >= width {
        return s.to_string();
    }
    let padding: String = std::iter::repeat(pad).take(width - s.len()).collect();
    if left_align {
        format!("{}{}", s, padding)
    } else {
        format!("{}{}", padding, s)
    }
}

fn coerce_to_char(val: Value) -> Result<char> {
    match val.unpack() {
        RV::Fixnum(i) => {
            if let Ok(u) = u32::try_from(i) {
                if let Some(c) = char::from_u32(u) {
                    return Ok(c);
                }
            }
            Err(MonorubyErr::argumenterr("invalid character"))
        }
        RV::Float(f) => {
            let f = f.trunc();
            if 0.0 <= f && f <= u32::MAX as f64 {
                if let Some(c) = char::from_u32(f as u32) {
                    return Ok(c);
                }
            }
            Err(MonorubyErr::argumenterr("invalid character"))
        }
        RV::String(s) => {
            let s = s.check_utf8()?;
            if s.chars().count() != 1 {
                Err(MonorubyErr::argumenterr("%c requires a character"))
            } else {
                Ok(s.chars().next().unwrap())
            }
        }
        _ => Err(MonorubyErr::argumenterr("invalid character")),
    }
}

/// Format an integer string with sign/space/zero/width flags.
fn format_integer_with_flags(
    s: &str,
    width: usize,
    zero_flag: bool,
    minus_flag: bool,
    plus_flag: bool,
    space_flag: bool,
) -> String {
    let (is_neg, digits) = if let Some(stripped) = s.strip_prefix('-') {
        (true, stripped)
    } else {
        (false, s)
    };
    let sign = if is_neg {
        "-"
    } else if plus_flag {
        "+"
    } else if space_flag {
        " "
    } else {
        ""
    };
    let body = format!("{}{}", sign, digits);
    if zero_flag && width > body.len() {
        let pad = width - sign.len();
        format!("{}{:0>w$}", sign, digits, w = pad)
    } else {
        apply_width(&body, width, minus_flag, ' ')
    }
}

/// Format a float with sign/space/zero/width flags.
/// `s` is the formatted absolute value, `f` is the original float (for sign detection).
fn format_float_with_flags(
    s: &str,
    f: f64,
    width: usize,
    zero_flag: bool,
    minus_flag: bool,
    plus_flag: bool,
    space_flag: bool,
) -> String {
    let sign = if f.is_sign_negative() && !f.is_nan() {
        "-"
    } else if plus_flag {
        "+"
    } else if space_flag {
        " "
    } else {
        ""
    };
    let body = format!("{}{}", sign, s);
    if zero_flag && width > body.len() {
        let pad = width - sign.len();
        format!("{}{:0>w$}", sign, s, w = pad)
    } else {
        apply_width(&body, width, minus_flag, ' ')
    }
}

/// Format a float in hexadecimal floating-point notation (%a/%A).
/// The format is: 0x1.<hex-mantissa>p<sign><decimal-exponent>
/// `f` should be the absolute value; sign is handled by the caller.
fn format_hex_float(f: f64, precision: Option<usize>, uppercase: bool) -> String {
    if f.is_nan() {
        return "NaN".to_string();
    }
    if f.is_infinite() {
        return "Inf".to_string();
    }

    let (prefix, p_char) = if uppercase { ("0X", 'P') } else { ("0x", 'p') };

    if f == 0.0 {
        return match precision {
            Some(0) => format!("{}0{}+0", prefix, p_char),
            Some(prec) => format!("{}0.{:0>w$}{}+0", prefix, "", p_char, w = prec),
            None => format!("{}0{}+0", prefix, p_char),
        };
    }

    let bits = f.to_bits();
    let raw_exp = ((bits >> 52) & 0x7ff) as i64;
    let mantissa_bits = bits & 0x000f_ffff_ffff_ffff;

    let (leading, exp, mantissa) = if raw_exp == 0 {
        // Denormalized number
        if mantissa_bits == 0 {
            ('0', 0i64, 0u64)
        } else {
            let shift = mantissa_bits.leading_zeros() as i64 - 12; // 12 = 64 - 52
            let normalized = mantissa_bits << (shift + 1);
            let new_mantissa = normalized & 0x000f_ffff_ffff_ffff;
            let exp = -1022 - shift - 1;
            ('1', exp, new_mantissa)
        }
    } else {
        let exp = raw_exp - 1023;
        ('1', exp, mantissa_bits)
    };

    let exp_sign = if exp >= 0 { "+" } else { "-" };
    let exp_abs = exp.unsigned_abs();

    // Full mantissa hex: 13 hex digits (52 bits / 4 = 13)
    let full_hex = if uppercase {
        format!("{:013X}", mantissa)
    } else {
        format!("{:013x}", mantissa)
    };

    match precision {
        Some(0) => {
            // Round: check if mantissa >= 0x8_0000_0000_0000 (half)
            let rounded_leading = if mantissa >= 0x8_0000_0000_0000 {
                '2'
            } else {
                leading
            };
            format!(
                "{}{}{}{}{}",
                prefix, rounded_leading, p_char, exp_sign, exp_abs
            )
        }
        Some(prec) => {
            if prec >= 13 {
                let padded = format!("{:0<w$}", full_hex, w = prec);
                format!(
                    "{}{}.{}{}{}{}",
                    prefix, leading, padded, p_char, exp_sign, exp_abs
                )
            } else {
                let rounded = round_hex_mantissa(&full_hex, prec, uppercase);
                format!(
                    "{}{}.{}{}{}{}",
                    prefix, leading, rounded, p_char, exp_sign, exp_abs
                )
            }
        }
        None => {
            let trimmed = full_hex.trim_end_matches('0');
            if trimmed.is_empty() {
                format!("{}{}{}{}{}", prefix, leading, p_char, exp_sign, exp_abs)
            } else {
                format!(
                    "{}{}.{}{}{}{}",
                    prefix, leading, trimmed, p_char, exp_sign, exp_abs
                )
            }
        }
    }
}

/// Round a hex mantissa string to the given number of hex digits.
fn round_hex_mantissa(hex: &str, prec: usize, uppercase: bool) -> String {
    let hex_bytes: Vec<u8> = hex.bytes().collect();
    if prec >= hex_bytes.len() {
        return hex.to_string();
    }
    let round_digit = hex_digit_val(hex_bytes[prec]);
    let mut digits: Vec<u8> = hex_bytes[..prec]
        .iter()
        .map(|&b| hex_digit_val(b))
        .collect();
    if round_digit >= 8 {
        let mut carry = true;
        for d in digits.iter_mut().rev() {
            if carry {
                *d += 1;
                if *d >= 16 {
                    *d = 0;
                } else {
                    carry = false;
                }
            }
        }
    }
    digits
        .iter()
        .map(|&d| {
            if d < 10 {
                (b'0' + d) as char
            } else if uppercase {
                (b'A' + d - 10) as char
            } else {
                (b'a' + d - 10) as char
            }
        })
        .collect()
}

fn hex_digit_val(b: u8) -> u8 {
    match b {
        b'0'..=b'9' => b - b'0',
        b'a'..=b'f' => b - b'a' + 10,
        b'A'..=b'F' => b - b'A' + 10,
        _ => 0,
    }
}

/// Format a float using %g/%G rules:
/// Use scientific notation if exponent < -4 or >= precision, otherwise fixed.
/// Strip trailing zeros after decimal point (and the point itself if no digits remain).
fn format_g(f: f64, precision: usize, uppercase: bool) -> String {
    if f == 0.0 {
        return "0".to_string();
    }
    if f.is_infinite() {
        return if f > 0.0 {
            "Inf".to_string()
        } else {
            "-Inf".to_string()
        };
    }
    if f.is_nan() {
        return "NaN".to_string();
    }
    let exp = f.log10().floor() as i32;
    if exp < -4 || exp >= precision as i32 {
        // Use scientific notation with (precision - 1) digits after decimal
        let sci_prec = if precision > 1 { precision - 1 } else { 0 };
        let s = if uppercase {
            format!("{:.p$E}", f, p = sci_prec)
        } else {
            format!("{:.p$e}", f, p = sci_prec)
        };
        // Strip trailing zeros in the mantissa part (before 'e'/'E')
        strip_trailing_zeros_scientific(&s)
    } else {
        // Use fixed notation
        // precision means total significant digits
        let fixed_prec = if precision as i32 > exp + 1 {
            (precision as i32 - exp - 1) as usize
        } else {
            0
        };
        let s = format!("{:.p$}", f, p = fixed_prec);
        strip_trailing_zeros_fixed(&s)
    }
}

/// Strip trailing zeros from fixed-point notation (e.g., "1.200" -> "1.2", "1.0" -> "1")
fn strip_trailing_zeros_fixed(s: &str) -> String {
    if !s.contains('.') {
        return s.to_string();
    }
    let trimmed = s.trim_end_matches('0').trim_end_matches('.');
    trimmed.to_string()
}

/// Strip trailing zeros from scientific notation (e.g., "1.200e+03" -> "1.2e+03")
fn strip_trailing_zeros_scientific(s: &str) -> String {
    let (mantissa, exponent) = if let Some(pos) = s.find('e') {
        (&s[..pos], &s[pos..])
    } else if let Some(pos) = s.find('E') {
        (&s[..pos], &s[pos..])
    } else {
        return s.to_string();
    };
    let trimmed = strip_trailing_zeros_fixed(mantissa);
    format!("{}{}", trimmed, exponent)
}

/// Normalize Rust's scientific notation exponent to Ruby format.
/// Rust: "1.23e6" or "1.23e-5" -> Ruby: "1.23e+06" or "1.23e-05"
/// Always includes sign, always at least 2 digits in exponent.
fn normalize_sci_exponent(s: &str) -> String {
    let (prefix, sep, exp_str) = if let Some(pos) = s.find('e') {
        (&s[..pos], "e", &s[pos + 1..])
    } else if let Some(pos) = s.find('E') {
        (&s[..pos], "E", &s[pos + 1..])
    } else {
        return s.to_string();
    };
    let (sign, digits) = if let Some(stripped) = exp_str.strip_prefix('-') {
        ("-", stripped)
    } else if let Some(stripped) = exp_str.strip_prefix('+') {
        ("+", stripped)
    } else {
        ("+", exp_str)
    };
    // Pad exponent to at least 2 digits
    if digits.len() < 2 {
        format!("{}{}{}0{}", prefix, sep, sign, digits)
    } else {
        format!("{}{}{}{}", prefix, sep, sign, digits)
    }
}

/// Format a negative integer in base for Ruby's two's complement representation.
/// Ruby uses `..f01` style for negative hex, `..7401` for negative octal, etc.
///
/// Algorithm: take abs value, format in base, compute (base-1)-complement + 1,
/// strip leading fill digits (keeping one), prefix with `..`.
fn format_neg_twos_complement(val: i64, base: u32, uppercase: bool) -> String {
    let max_digit = base - 1;
    let fill_char = if uppercase {
        char::from_digit(max_digit, base)
            .unwrap()
            .to_ascii_uppercase()
    } else {
        char::from_digit(max_digit, base).unwrap()
    };

    let abs_val = (val as i128).unsigned_abs() as u64;
    // Format absolute value in base
    let abs_digits: Vec<u32> = {
        let s = match base {
            16 => format!("{:x}", abs_val),
            8 => format!("{:o}", abs_val),
            2 => format!("{:b}", abs_val),
            _ => unreachable!(),
        };
        s.chars().map(|c| c.to_digit(base).unwrap()).collect()
    };

    // Compute (base-1)-complement: each digit d -> (base-1) - d
    let mut complement: Vec<u32> = abs_digits.iter().map(|&d| max_digit - d).collect();

    // Add 1 (with carry)
    let mut carry = 1u32;
    for d in complement.iter_mut().rev() {
        let sum = *d + carry;
        *d = sum % base;
        carry = sum / base;
    }
    // If there's still carry, we need to prepend, but for Ruby's format
    // it just means more fill digits (which get stripped anyway)

    // Convert digits to chars
    let result: String = complement
        .iter()
        .map(|&d| {
            let c = char::from_digit(d, base).unwrap();
            if uppercase { c.to_ascii_uppercase() } else { c }
        })
        .collect();

    // Strip leading fill chars but keep one
    let stripped = result.trim_start_matches(fill_char);
    if stripped.is_empty() {
        format!("..{}", fill_char)
    } else {
        format!("..{}{}", fill_char, stripped)
    }
}

/// Format integer for %b/%B/%o/%x/%X with sign, prefix, and flags.
fn format_int_with_prefix(
    is_neg: bool,
    digits: &str,
    prefix: &str,
    width: usize,
    zero_flag: bool,
    minus_flag: bool,
    plus_flag: bool,
    space_flag: bool,
) -> String {
    let sign = if is_neg {
        "-"
    } else if plus_flag {
        "+"
    } else if space_flag {
        " "
    } else {
        ""
    };
    let body = format!("{}{}{}", sign, prefix, digits);
    if zero_flag && width > body.len() {
        let pad = width - sign.len() - prefix.len();
        format!("{}{}{:0>w$}", sign, prefix, digits, w = pad)
    } else {
        apply_width(&body, width, minus_flag, ' ')
    }
}

impl Executor {
    pub(crate) fn format_by_args(
        &mut self,
        globals: &mut Globals,
        self_str: &str,
        arguments: &[Value],
    ) -> Result<String> {
        let mut arg_no = 0;
        let mut format_str = String::new();
        let fchars: Vec<char> = self_str.chars().collect();
        let flen = fchars.len();
        let mut i = 0;

        // Lazily cached hash from last argument for named references.
        let mut named_hash_cache: Option<Option<Hashmap>> = None;
        let get_named_hash =
            |arguments: &[Value], cache: &mut Option<Option<Hashmap>>| -> Option<Hashmap> {
                if cache.is_none() {
                    let h = arguments.last().and_then(|v| v.try_hash_ty());
                    *cache = Some(h);
                }
                cache.unwrap()
            };

        while i < flen {
            if fchars[i] != '%' {
                format_str.push(fchars[i]);
                i += 1;
                continue;
            }
            i += 1; // skip '%'
            if i >= flen {
                return Err(MonorubyErr::argumenterr(
                    "incomplete format specifier; use %% (double %) instead",
                ));
            }
            // %%
            if fchars[i] == '%' {
                format_str.push('%');
                i += 1;
                continue;
            }

            // Check for %{name} — named reference (to_s only, no format specifier)
            if fchars[i] == '{' {
                i += 1; // skip '{'
                let mut key = String::new();
                while i < flen && fchars[i] != '}' {
                    key.push(fchars[i]);
                    i += 1;
                }
                if i >= flen {
                    return Err(MonorubyErr::argumenterr(
                        "malformed format string - missing '}'",
                    ));
                }
                i += 1; // skip '}'
                let hash = get_named_hash(arguments, &mut named_hash_cache).ok_or_else(|| {
                    MonorubyErr::argumenterr("one hash required")
                })?;
                let key_val = Value::symbol_from_str(&key);
                let val = hash.get(key_val, self, globals)?.unwrap_or(Value::nil());
                format_str += &val.coerce_to_s(self, globals)?;
                continue;
            }

            // Check for %<name>spec — named reference with format specifier
            let named_val = if fchars[i] == '<' {
                i += 1; // skip '<'
                let mut key = String::new();
                while i < flen && fchars[i] != '>' {
                    key.push(fchars[i]);
                    i += 1;
                }
                if i >= flen {
                    return Err(MonorubyErr::argumenterr(
                        "malformed format string - missing '>'",
                    ));
                }
                i += 1; // skip '>'
                let hash = get_named_hash(arguments, &mut named_hash_cache).ok_or_else(|| {
                    MonorubyErr::argumenterr("one hash required")
                })?;
                let key_val = Value::symbol_from_str(&key);
                let val = hash.get(key_val, self, globals)?.unwrap_or(Value::nil());
                Some(val)
            } else {
                None
            };

            // Check for positional argument: non-zero digit(s) followed by '$'
            let positional_arg = if named_val.is_none()
                && i < flen
                && fchars[i].is_ascii_digit()
                && fchars[i] != '0'
            {
                // Look ahead: collect digits then check for '$'
                let mut j = i;
                while j < flen && fchars[j].is_ascii_digit() {
                    j += 1;
                }
                if j < flen && fchars[j] == '$' {
                    // Parse the number
                    let mut num = 0usize;
                    for k in i..j {
                        num = num * 10 + (fchars[k] as usize) - ('0' as usize);
                    }
                    i = j + 1; // skip past '$'
                    if num == 0 || num > arguments.len() {
                        return Err(MonorubyErr::argumenterr("too few arguments"));
                    }
                    Some(arguments[num - 1])
                } else {
                    // Not positional — digits are part of width, don't advance i
                    None
                }
            } else {
                None
            };

            if i >= flen {
                return Err(MonorubyErr::argumenterr(
                    "Invalid termination of format string",
                ));
            }
            let mut ch = fchars[i];

            // Parse flags
            let mut zero_flag = false;
            let mut minus_flag = false;
            let mut plus_flag = false;
            let mut space_flag = false;
            let mut hash_flag = false;
            loop {
                match ch {
                    '0' => zero_flag = true,
                    '-' => minus_flag = true,
                    '+' => plus_flag = true,
                    ' ' => space_flag = true,
                    '#' => hash_flag = true,
                    _ => break,
                }
                i += 1;
                if i >= flen {
                    return Err(MonorubyErr::argumenterr(
                        "Invalid termination of format string",
                    ));
                }
                ch = fchars[i];
            }
            // Left-align overrides zero-fill
            if minus_flag {
                zero_flag = false;
            }
            // Plus flag overrides space flag
            if plus_flag {
                space_flag = false;
            }
            // Width (may be '*')
            let mut width = 0usize;
            if ch == '*' {
                if arguments.len() <= arg_no {
                    return Err(MonorubyErr::argumenterr("too few arguments"));
                }
                let w = arguments[arg_no].coerce_to_integer(self, globals)?;
                arg_no += 1;
                match w {
                    IntegerBase::Fixnum(v) => {
                        if v < 0 {
                            minus_flag = true;
                            zero_flag = false;
                            width = (-v) as usize;
                        } else {
                            width = v as usize;
                        }
                    }
                    IntegerBase::BigInt(_) => {
                        return Err(MonorubyErr::argumenterr("width too big"));
                    }
                }
                i += 1;
                if i >= flen {
                    return Err(MonorubyErr::argumenterr(
                        "Invalid termination of format string",
                    ));
                }
                ch = fchars[i];
            } else {
                while ch.is_ascii_digit() {
                    width = width * 10 + ch as usize - '0' as usize;
                    i += 1;
                    if i >= flen {
                        return Err(MonorubyErr::argumenterr(
                            "Invalid termination of format string",
                        ));
                    }
                    ch = fchars[i];
                }
            }
            // Precision
            let mut precision = None;
            if ch == '.' {
                i += 1;
                if i >= flen {
                    return Err(MonorubyErr::argumenterr(
                        "Invalid termination of format string",
                    ));
                }
                ch = fchars[i];
                let mut prec = 0usize;
                if ch == '*' {
                    if arguments.len() <= arg_no {
                        return Err(MonorubyErr::argumenterr("too few arguments"));
                    }
                    let p = arguments[arg_no].coerce_to_integer(self, globals)?;
                    arg_no += 1;
                    match p {
                        IntegerBase::Fixnum(v) => {
                            if v >= 0 {
                                prec = v as usize;
                            }
                        }
                        IntegerBase::BigInt(_) => {
                            return Err(MonorubyErr::argumenterr("precision too big"));
                        }
                    }
                    i += 1;
                    if i >= flen {
                        return Err(MonorubyErr::argumenterr(
                            "Invalid termination of format string",
                        ));
                    }
                    ch = fchars[i];
                } else {
                    while ch.is_ascii_digit() {
                        prec = prec * 10 + ch as usize - '0' as usize;
                        i += 1;
                        if i >= flen {
                            return Err(MonorubyErr::argumenterr(
                                "Invalid termination of format string",
                            ));
                        }
                        ch = fchars[i];
                    }
                }
                precision = Some(prec);
            }
            // Determine val: positional, named, or sequential
            let val = if let Some(v) = positional_arg {
                v
            } else if let Some(v) = named_val {
                v
            } else {
                if arguments.len() <= arg_no {
                    return Err(MonorubyErr::argumenterr("too few arguments"));
                }
                let v = arguments[arg_no];
                arg_no += 1;
                v
            };
            i += 1; // consume the specifier character
            // Specifier
            let format = match ch {
                'c' => {
                    let c = coerce_to_char(val)?;
                    let s = format!("{}", c);
                    apply_width(&s, width, minus_flag, ' ')
                }
                's' => {
                    let mut s = val.coerce_to_s(self, globals)?;
                    if let Some(prec) = precision {
                        if s.len() > prec {
                            s.truncate(prec);
                        }
                    }
                    apply_width(&s, width, minus_flag, ' ')
                }
                'p' => {
                    let s = val.inspect(&globals.store);
                    apply_width(&s, width, minus_flag, ' ')
                }
                'd' | 'i' => {
                    let ival = val.coerce_to_integer(self, globals)?;
                    let s = match ival {
                        IntegerBase::Fixnum(v) => format!("{}", v),
                        IntegerBase::BigInt(v) => format!("{}", v),
                    };
                    format_integer_with_flags(
                        &s, width, zero_flag, minus_flag, plus_flag, space_flag,
                    )
                }
                'u' => {
                    let ival = val.coerce_to_integer(self, globals)?;
                    let s = match ival {
                        IntegerBase::Fixnum(v) => format!("{}", v),
                        IntegerBase::BigInt(v) => format!("{}", v),
                    };
                    format_integer_with_flags(
                        &s, width, zero_flag, minus_flag, plus_flag, space_flag,
                    )
                }
                'b' | 'B' => {
                    let ival = val.coerce_to_integer(self, globals)?;
                    match ival {
                        IntegerBase::Fixnum(v) if v < 0 => {
                            let tc = format_neg_twos_complement(v, 2, ch == 'B');
                            let prefix = if hash_flag {
                                if ch == 'B' { "0B" } else { "0b" }
                            } else {
                                ""
                            };
                            let body = format!("{}{}", prefix, tc);
                            apply_width(&body, width, minus_flag, ' ')
                        }
                        _ => {
                            let digits = match ival {
                                IntegerBase::Fixnum(v) => format!("{:b}", v),
                                IntegerBase::BigInt(v) => format!("{:b}", v),
                            };
                            let is_zero = digits == "0";
                            let prefix = if hash_flag && !is_zero {
                                if ch == 'B' { "0B" } else { "0b" }
                            } else {
                                ""
                            };
                            format_int_with_prefix(
                                false, &digits, prefix, width, zero_flag, minus_flag, plus_flag,
                                space_flag,
                            )
                        }
                    }
                }
                'o' => {
                    let ival = val.coerce_to_integer(self, globals)?;
                    match ival {
                        IntegerBase::Fixnum(v) if v < 0 => {
                            let tc = format_neg_twos_complement(v, 8, false);
                            apply_width(&tc, width, minus_flag, ' ')
                        }
                        _ => {
                            let digits = match ival {
                                IntegerBase::Fixnum(v) => format!("{:o}", v),
                                IntegerBase::BigInt(v) => format!("{:o}", v),
                            };
                            let prefix = if hash_flag {
                                if digits.starts_with('0') { "" } else { "0" }
                            } else {
                                ""
                            };
                            format_int_with_prefix(
                                false, &digits, prefix, width, zero_flag, minus_flag, plus_flag,
                                space_flag,
                            )
                        }
                    }
                }
                'x' | 'X' => {
                    let ival = val.coerce_to_integer(self, globals)?;
                    match ival {
                        IntegerBase::Fixnum(v) if v < 0 => {
                            let tc = format_neg_twos_complement(v, 16, ch == 'X');
                            let prefix = if hash_flag {
                                if ch == 'X' { "0X" } else { "0x" }
                            } else {
                                ""
                            };
                            let body = format!("{}{}", prefix, tc);
                            apply_width(&body, width, minus_flag, ' ')
                        }
                        _ => {
                            let digits = match ival {
                                IntegerBase::Fixnum(v) => {
                                    if ch == 'X' {
                                        format!("{:X}", v)
                                    } else {
                                        format!("{:x}", v)
                                    }
                                }
                                IntegerBase::BigInt(v) => {
                                    if ch == 'X' {
                                        format!("{:X}", v)
                                    } else {
                                        format!("{:x}", v)
                                    }
                                }
                            };
                            let is_zero = digits == "0";
                            let prefix = if hash_flag && !is_zero {
                                if ch == 'X' { "0X" } else { "0x" }
                            } else {
                                ""
                            };
                            format_int_with_prefix(
                                false, &digits, prefix, width, zero_flag, minus_flag, plus_flag,
                                space_flag,
                            )
                        }
                    }
                }
                'f' => {
                    let f = val.coerce_to_float(self, globals)?;
                    let prec = precision.unwrap_or(6);
                    let s = format!("{:.p$}", f.abs(), p = prec);
                    format_float_with_flags(
                        &s, f, width, zero_flag, minus_flag, plus_flag, space_flag,
                    )
                }
                'e' | 'E' => {
                    let f = val.coerce_to_float(self, globals)?;
                    let prec = precision.unwrap_or(6);
                    let s = if ch == 'E' {
                        normalize_sci_exponent(&format!("{:.p$E}", f.abs(), p = prec))
                    } else {
                        normalize_sci_exponent(&format!("{:.p$e}", f.abs(), p = prec))
                    };
                    format_float_with_flags(
                        &s, f, width, zero_flag, minus_flag, plus_flag, space_flag,
                    )
                }
                'g' | 'G' => {
                    let f = val.coerce_to_float(self, globals)?;
                    let prec = precision.unwrap_or(6);
                    let prec = if prec == 0 { 1 } else { prec };
                    let s = format_g(f.abs(), prec, ch == 'G');
                    let s = normalize_sci_exponent(&s);
                    format_float_with_flags(
                        &s, f, width, zero_flag, minus_flag, plus_flag, space_flag,
                    )
                }
                'a' | 'A' => {
                    let f = val.coerce_to_float(self, globals)?;
                    let s = format_hex_float(f.abs(), precision, ch == 'A');
                    format_float_with_flags(
                        &s, f, width, zero_flag, minus_flag, plus_flag, space_flag,
                    )
                }
                _ => {
                    return Err(MonorubyErr::argumenterr(format!(
                        "malformed format string - %{}",
                        ch
                    )));
                }
            };
            format_str += &format;
        }

        Ok(format_str)
    }

    pub(crate) fn format_by_hash(
        &mut self,
        globals: &mut Globals,
        self_str: &str,
        hash: Hashmap,
    ) -> Result<String> {
        let mut format_str = String::new();
        let mut chars = self_str.chars();
        while let Some(ch) = chars.next() {
            if ch != '%' {
                format_str.push(ch);
                continue;
            }
            match chars.next() {
                Some('%') => {
                    format_str.push('%');
                    continue;
                }
                Some('{') => {
                    let mut key = String::new();
                    loop {
                        match chars.next() {
                            Some('}') => break,
                            Some(c) => key.push(c),
                            None => {
                                return Err(MonorubyErr::argumenterr(
                                    "malformed format string - missing '}'",
                                ));
                            }
                        }
                    }
                    let key_val = Value::symbol_from_str(&key);
                    let val = hash.get(key_val, self, globals)?.unwrap_or(Value::nil());
                    format_str += &val.to_s(&globals.store);
                }
                ch => {
                    return Err(MonorubyErr::argumenterr(format!(
                        "malformed format string - %{}",
                        ch.unwrap_or(' ')
                    )));
                }
            }
        }
        Ok(format_str)
    }
}
