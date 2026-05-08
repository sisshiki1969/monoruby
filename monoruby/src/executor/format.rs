use super::*;
use crate::value::IntegerBase;
use num::BigInt;

/// Apply integer precision: pad digits with leading zeros to at
/// least `prec` digits. Special-case: precision 0 with value 0
/// yields the empty string (matches CRuby `%.0d` % 0 == "").
fn apply_int_precision(s: &str, precision: Option<usize>) -> String {
    let prec = match precision {
        Some(p) => p,
        None => return s.to_string(),
    };
    let (sign, digits) = if let Some(rest) = s.strip_prefix('-') {
        ("-", rest)
    } else {
        ("", s)
    };
    if prec == 0 && digits == "0" {
        return sign.to_string();
    }
    if digits.len() >= prec {
        return s.to_string();
    }
    let pad = prec - digits.len();
    let zeros: String = std::iter::repeat('0').take(pad).collect();
    format!("{}{}{}", sign, zeros, digits)
}

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
    // `0` flag is ignored for Inf / NaN — CRuby pads with spaces in
    // these cases (`%010E` on `Float::INFINITY` → `"       Inf"`).
    if zero_flag && f.is_finite() && width > body.len() {
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
/// When `keep_trailing` is true (`#` flag), trailing zeros and the
/// decimal point are preserved so that the result has `precision`
/// significant digits.
fn format_g(f: f64, precision: usize, uppercase: bool, keep_trailing: bool) -> String {
    if f == 0.0 {
        return if keep_trailing {
            // `%#g % 0` keeps the precision-driven zeros (e.g.
            // `"%#g" % 0 == "0.00000"`).
            let extra = precision.saturating_sub(1);
            if extra == 0 {
                "0.".to_string()
            } else {
                format!("0.{}", "0".repeat(extra))
            }
        } else {
            "0".to_string()
        };
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
        if keep_trailing {
            // Spec: `%#.0e % 100 == "1.e+02"` — empty fractional
            // part still gets the decimal point.
            ensure_decimal_point_scientific(&s)
        } else {
            strip_trailing_zeros_scientific(&s)
        }
    } else {
        // Use fixed notation
        // precision means total significant digits
        let fixed_prec = if precision as i32 > exp + 1 {
            (precision as i32 - exp - 1) as usize
        } else {
            0
        };
        let s = format!("{:.p$}", f, p = fixed_prec);
        if keep_trailing {
            // Spec: `%#g % 123456 == "123456."` (precision 6, no
            // fractional digits — but a `.` is still emitted).
            if s.contains('.') {
                s
            } else {
                format!("{}.", s)
            }
        } else {
            strip_trailing_zeros_fixed(&s)
        }
    }
}

/// Ensure the mantissa of a scientific-notation float string has a
/// `.` (used by the `#` flag with `%e`/`%g`).
fn ensure_decimal_point_scientific(s: &str) -> String {
    let (mantissa, exponent) = if let Some(pos) = s.find('e') {
        (&s[..pos], &s[pos..])
    } else if let Some(pos) = s.find('E') {
        (&s[..pos], &s[pos..])
    } else {
        return s.to_string();
    };
    if mantissa.contains('.') {
        s.to_string()
    } else {
        format!("{}.{}", mantissa, exponent)
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

/// Width padding for `..`-prefixed two's-complement bodies. With the
/// `0` flag, padding is inserted *between* the `..` prefix and the
/// digits using the radix-1 fill char (so `%010b` on -10 becomes
/// `..11110110`, not `   ..10110`). Without `0`, ordinary
/// space-padding is applied to the whole body.
fn apply_twos_complement_width(
    s: &str,
    width: usize,
    zero_flag: bool,
    left_align: bool,
    fill: char,
) -> String {
    if s.len() >= width || !zero_flag || left_align {
        return apply_width(s, width, left_align, ' ');
    }
    let (head, digits) = if let Some(idx) = s.find("..") {
        (&s[..idx + 2], &s[idx + 2..])
    } else {
        ("", s)
    };
    let pad_len = width.saturating_sub(s.len());
    let padding: String = std::iter::repeat(fill).take(pad_len).collect();
    format!("{}{}{}", head, padding, digits)
}

fn apply_twos_complement_precision(s: &str, precision: usize, fill: char) -> String {
    if s.len() >= precision {
        return s.to_string();
    }
    let (head, digits) = if let Some(idx) = s.find("..") {
        (&s[..idx + 2], &s[idx + 2..])
    } else {
        ("", s)
    };
    let pad_len = precision - s.len();
    let padding: String = std::iter::repeat(fill).take(pad_len).collect();
    format!("{}{}{}", head, padding, digits)
}

fn format_neg_twos_complement_bigint(val: &BigInt, base: u32, uppercase: bool) -> String {
    let max_digit = base - 1;
    let fill_char = if uppercase {
        char::from_digit(max_digit, base).unwrap().to_ascii_uppercase()
    } else {
        char::from_digit(max_digit, base).unwrap()
    };
    let abs = val.magnitude();
    let s = match base {
        16 => format!("{:x}", abs),
        8 => format!("{:o}", abs),
        2 => format!("{:b}", abs),
        _ => unreachable!(),
    };
    let abs_digits: Vec<u32> = s.chars().map(|c| c.to_digit(base).unwrap()).collect();
    let mut complement: Vec<u32> = abs_digits.iter().map(|&d| max_digit - d).collect();
    let mut carry = 1u32;
    for d in complement.iter_mut().rev() {
        let sum = *d + carry;
        *d = sum % base;
        carry = sum / base;
    }
    let result: String = complement
        .iter()
        .map(|&d| {
            let c = char::from_digit(d, base).unwrap();
            if uppercase { c.to_ascii_uppercase() } else { c }
        })
        .collect();
    let stripped = result.trim_start_matches(fill_char);
    if stripped.is_empty() {
        format!("..{}", fill_char)
    } else {
        format!("..{}{}", fill_char, stripped)
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

/// If `fchars[*i]` is `<`, parse `<name>` and look the value up in
/// the named-arg hash (cached in `cache`); advances `*i` to one past
/// the closing `>`. Otherwise leaves `*i` untouched and returns
/// `Ok(None)`.
fn try_consume_angle_named(
    vm: &mut Executor,
    globals: &mut Globals,
    arguments: &[Value],
    cache: &mut Option<Option<Hashmap>>,
    fchars: &[char],
    i: &mut usize,
    flen: usize,
) -> Result<Option<Value>> {
    if *i >= flen || fchars[*i] != '<' {
        return Ok(None);
    }
    let mut j = *i + 1;
    let mut key = String::new();
    while j < flen && fchars[j] != '>' {
        key.push(fchars[j]);
        j += 1;
    }
    if j >= flen {
        return Err(MonorubyErr::argumenterr(
            "malformed name - unmatched parenthesis",
        ));
    }
    let hash = get_named_hash_helper(arguments, cache)
        .ok_or_else(|| MonorubyErr::argumenterr("one hash required"))?;
    let key_val = Value::symbol_from_str(&key);
    let val = hash_lookup_or_keyerror(vm, globals, &hash, key_val, key.as_str(), '<')?;
    *i = j + 1;
    Ok(Some(val))
}

/// Snapshot of the closure used for named-hash caching; needed when
/// the inline closure version isn't reachable from a free function.
fn get_named_hash_helper(
    arguments: &[Value],
    cache: &mut Option<Option<Hashmap>>,
) -> Option<Hashmap> {
    if cache.is_none() {
        let h = arguments.last().and_then(|v| v.try_hash_ty());
        *cache = Some(h);
    }
    cache.unwrap()
}

/// Look `key` up in `hash`; honour `Hash#default` /
/// `Hash#default_proc` when the key is missing. Raises CRuby's
/// `KeyError` only when both the explicit lookup and the default
/// produce no value (i.e. `nil`). `bracket` is `'{' | '<'` and
/// selects the matching CRuby message format (`key{name} not found`
/// for `%{name}`, `key<name> not found` for `%<name>spec`).
fn hash_lookup_or_keyerror(
    vm: &mut Executor,
    globals: &mut Globals,
    hash: &Hashmap,
    key_val: Value,
    key_name: &str,
    bracket: char,
) -> Result<Value> {
    if let Some(v) = hash.get(key_val, vm, globals)? {
        return Ok(v);
    }
    // Key absent — let `Hash#[]` apply the default (Value or Proc).
    // CRuby raises `KeyError` only if the default ultimately yields
    // `nil`; spec exercises both `Hash.new(123)` (returns 123) and
    // `Hash.new { nil }` (raises).
    let v = hash.index(vm, globals, key_val)?;
    if !v.is_nil() {
        return Ok(v);
    }
    let (open, close) = match bracket {
        '<' => ('<', '>'),
        _ => ('{', '}'),
    };
    let msg = format!("key{}{}{} not found", open, key_name, close);
    let receiver: Value = (*hash).into();
    Err(MonorubyErr::keyerr_with(msg, receiver, key_val))
}

/// Try to consume an `N$` positional argument reference at
/// `fchars[*i..]`. Recognized only when one or more digits (with a
/// leading non-zero digit) are followed immediately by `$`. On
/// match, advances `*i` past the `$` and returns the 1-based index
/// (or an error if it would be out of range / 0). On no-match,
/// leaves `*i` unchanged and returns `Ok(None)`.
fn try_consume_positional_dollar(
    fchars: &[char],
    i: &mut usize,
    flen: usize,
) -> Result<Option<usize>> {
    if *i >= flen || !fchars[*i].is_ascii_digit() || fchars[*i] == '0' {
        return Ok(None);
    }
    let mut j = *i;
    while j < flen && fchars[j].is_ascii_digit() {
        j += 1;
    }
    if j >= flen || fchars[j] != '$' {
        return Ok(None);
    }
    let mut num = 0usize;
    for k in *i..j {
        num = num
            .checked_mul(10)
            .and_then(|n| n.checked_add(fchars[k] as usize - '0' as usize))
            .ok_or_else(|| MonorubyErr::argumenterr("argument number too big"))?;
    }
    if num == 0 {
        return Err(MonorubyErr::argumenterr(
            "invalid absolute argument number",
        ));
    }
    *i = j + 1;
    Ok(Some(num))
}

/// Resolve a positional-arg reference to its value, validating range.
fn resolve_positional(arguments: &[Value], num: usize) -> Result<Value> {
    if num == 0 || num > arguments.len() {
        return Err(MonorubyErr::argumenterr("too few arguments"));
    }
    Ok(arguments[num - 1])
}

/// Truncate `s` to at most `prec` *characters* (not bytes). Matches
/// CRuby's `%.Ns` semantics, where the precision counts characters.
fn truncate_to_chars(s: &str, prec: usize) -> String {
    let mut count = 0;
    let mut end = s.len();
    for (i, _) in s.char_indices() {
        if count == prec {
            end = i;
            break;
        }
        count += 1;
    }
    s[..end].to_string()
}

/// Tracks whether positional (`%N$x`) and/or sequential (`%x`) args
/// have been consumed so that mixing can be flagged as an error.
#[derive(Default)]
struct ArgTracker {
    used_positional: bool,
    used_sequential: bool,
}

impl ArgTracker {
    fn note_positional(&mut self) -> Result<()> {
        if self.used_sequential {
            return Err(MonorubyErr::argumenterr(
                "numbered(2) after unnumbered(1)",
            ));
        }
        self.used_positional = true;
        Ok(())
    }

    fn note_sequential(&mut self) -> Result<()> {
        if self.used_positional {
            return Err(MonorubyErr::argumenterr(
                "unnumbered(1) mixed with numbered",
            ));
        }
        self.used_sequential = true;
        Ok(())
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
        let mut tracker = ArgTracker::default();

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

            // Positional reference (`%N$...`) immediately after `%`.
            // CRuby also tolerates flags before the `N$`, so we look
            // again after parsing flags below.
            let mut positional_arg = None;
            if let Some(num) = try_consume_positional_dollar(&fchars, &mut i, flen)? {
                tracker.note_positional()?;
                positional_arg = Some(resolve_positional(arguments, num)?);
            }

            // `%<name>spec` — named ref. May appear before flags,
            // between flags and width, between width and precision,
            // and just before the type char.
            let mut named_val = try_consume_angle_named(
                self,
                globals,
                arguments,
                &mut named_hash_cache,
                &fchars,
                &mut i,
                flen,
            )?;

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
                // `<name>` may appear between flag chars.
                if ch == '<' {
                    if let Some(v) = try_consume_angle_named(
                        self,
                        globals,
                        arguments,
                        &mut named_hash_cache,
                        &fchars,
                        &mut i,
                        flen,
                    )? {
                        named_val = Some(v);
                        if i >= flen {
                            return Err(MonorubyErr::argumenterr(
                                "Invalid termination of format string",
                            ));
                        }
                        ch = fchars[i];
                    }
                }
            }
            // Positional reference may also appear after flags (e.g.
            // `%-2$d`).
            if positional_arg.is_none() {
                if let Some(num) = try_consume_positional_dollar(&fchars, &mut i, flen)? {
                    tracker.note_positional()?;
                    positional_arg = Some(resolve_positional(arguments, num)?);
                    if i >= flen {
                        return Err(MonorubyErr::argumenterr(
                            "Invalid termination of format string",
                        ));
                    }
                    ch = fchars[i];
                }
            }
            // Left-align overrides zero-fill
            if minus_flag {
                zero_flag = false;
            }
            // Plus flag overrides space flag
            if plus_flag {
                space_flag = false;
            }
            // Width (may be '*' or '*N$')
            let mut width = 0usize;
            if ch == '*' {
                i += 1;
                if i >= flen {
                    return Err(MonorubyErr::argumenterr(
                        "Invalid termination of format string",
                    ));
                }
                let w_val = if let Some(num) = try_consume_positional_dollar(&fchars, &mut i, flen)?
                {
                    tracker.note_positional()?;
                    resolve_positional(arguments, num)?
                } else {
                    tracker.note_sequential()?;
                    if arguments.len() <= arg_no {
                        return Err(MonorubyErr::argumenterr("too few arguments"));
                    }
                    let v = arguments[arg_no];
                    arg_no += 1;
                    v
                };
                let v = w_val.coerce_to_int_i64(self, globals)?;
                if v < 0 {
                    minus_flag = true;
                    zero_flag = false;
                    width = v.unsigned_abs() as usize;
                } else {
                    width = v as usize;
                }
                if i >= flen {
                    return Err(MonorubyErr::argumenterr(
                        "Invalid termination of format string",
                    ));
                }
                ch = fchars[i];
            } else {
                while ch.is_ascii_digit() {
                    width = width
                        .checked_mul(10)
                        .and_then(|w| w.checked_add(ch as usize - '0' as usize))
                        .ok_or_else(|| MonorubyErr::argumenterr("width too big"))?;
                    i += 1;
                    if i >= flen {
                        return Err(MonorubyErr::argumenterr(
                            "Invalid termination of format string",
                        ));
                    }
                    ch = fchars[i];
                }
            }
            // `<name>` may appear between width and precision.
            if ch == '<' {
                if let Some(v) = try_consume_angle_named(
                    self,
                    globals,
                    arguments,
                    &mut named_hash_cache,
                    &fchars,
                    &mut i,
                    flen,
                )? {
                    named_val = Some(v);
                    if i >= flen {
                        return Err(MonorubyErr::argumenterr(
                            "Invalid termination of format string",
                        ));
                    }
                    ch = fchars[i];
                }
            }
            // Precision (may be '.*' or '.*N$')
            let mut precision = None;
            if ch == '.' {
                i += 1;
                if i >= flen {
                    return Err(MonorubyErr::argumenterr(
                        "Invalid termination of format string",
                    ));
                }
                ch = fchars[i];
                if ch == '*' {
                    i += 1;
                    if i >= flen {
                        return Err(MonorubyErr::argumenterr(
                            "Invalid termination of format string",
                        ));
                    }
                    let p_val =
                        if let Some(num) = try_consume_positional_dollar(&fchars, &mut i, flen)? {
                            tracker.note_positional()?;
                            resolve_positional(arguments, num)?
                        } else {
                            tracker.note_sequential()?;
                            if arguments.len() <= arg_no {
                                return Err(MonorubyErr::argumenterr("too few arguments"));
                            }
                            let v = arguments[arg_no];
                            arg_no += 1;
                            v
                        };
                    let v = p_val.coerce_to_int_i64(self, globals)?;
                    // CRuby silently ignores negative precision.
                    if v >= 0 {
                        precision = Some(v as usize);
                    }
                    if i >= flen {
                        return Err(MonorubyErr::argumenterr(
                            "Invalid termination of format string",
                        ));
                    }
                    ch = fchars[i];
                } else {
                    let mut prec = 0usize;
                    while ch.is_ascii_digit() {
                        prec = prec
                            .checked_mul(10)
                            .and_then(|p| p.checked_add(ch as usize - '0' as usize))
                            .ok_or_else(|| MonorubyErr::argumenterr("precision too big"))?;
                        i += 1;
                        if i >= flen {
                            return Err(MonorubyErr::argumenterr(
                                "Invalid termination of format string",
                            ));
                        }
                        ch = fchars[i];
                    }
                    precision = Some(prec);
                }
            }
            // `<name>` may appear between precision and the type char.
            if ch == '<' {
                if let Some(v) = try_consume_angle_named(
                    self,
                    globals,
                    arguments,
                    &mut named_hash_cache,
                    &fchars,
                    &mut i,
                    flen,
                )? {
                    named_val = Some(v);
                    if i >= flen {
                        return Err(MonorubyErr::argumenterr(
                            "Invalid termination of format string",
                        ));
                    }
                    ch = fchars[i];
                }
            }
            // Positional reference may also appear here, e.g.
            // `%*1$.*2$3$d` puts the `3$` between precision and `d`.
            if positional_arg.is_none() {
                if let Some(num) = try_consume_positional_dollar(&fchars, &mut i, flen)? {
                    tracker.note_positional()?;
                    positional_arg = Some(resolve_positional(arguments, num)?);
                    if i >= flen {
                        return Err(MonorubyErr::argumenterr(
                            "Invalid termination of format string",
                        ));
                    }
                    ch = fchars[i];
                }
            }
            // `{name}` is the to_s-only form. Equivalent to `%s` after
            // `to_s` coercion, but uses the named hash to look the
            // value up. Width and precision (already parsed) still
            // apply.
            if ch == '{' {
                let mut key = String::new();
                let mut j = i + 1;
                while j < flen && fchars[j] != '}' {
                    key.push(fchars[j]);
                    j += 1;
                }
                if j >= flen {
                    return Err(MonorubyErr::argumenterr(
                        "malformed name - unmatched parenthesis",
                    ));
                }
                if named_val.is_some() {
                    return Err(MonorubyErr::argumenterr("named<name> after named{name}"));
                }
                let hash = get_named_hash(arguments, &mut named_hash_cache)
                    .ok_or_else(|| MonorubyErr::argumenterr("one hash required"))?;
                let key_val = Value::symbol_from_str(&key);
                let val =
                    hash_lookup_or_keyerror(self, globals, &hash, key_val, key.as_str(), '{')?;
                let mut s = val.coerce_to_s(self, globals)?;
                if let Some(prec) = precision {
                    if s.chars().count() > prec {
                        s = s.chars().take(prec).collect();
                    }
                }
                format_str += &apply_width(&s, width, minus_flag, ' ');
                i = j + 1;
                continue;
            }
            // Determine val: positional, named, or sequential
            let val = if let Some(v) = positional_arg {
                v
            } else if let Some(v) = named_val {
                v
            } else {
                tracker.note_sequential()?;
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
                    let s = match val.coerce_to_char(self, globals)? {
                        Some(c) => format!("{}", c),
                        None => String::new(),
                    };
                    apply_width(&s, width, minus_flag, ' ')
                }
                's' => {
                    // `%s` always dispatches to `Object#to_s` — it
                    // never tries `to_str`. If the receiver doesn't
                    // respond to `to_s` (e.g. a bare `BasicObject`),
                    // CRuby raises `NoMethodError`. Don't fall back
                    // to the C-level inspect.
                    let mut s = if let Some(string) = val.is_str() {
                        string.to_string()
                    } else if let Some(func_id) = globals.check_method(val, IdentId::TO_S) {
                        let result = self.invoke_func_inner(
                            globals, func_id, val, &[], None, None,
                        )?;
                        if let Some(string) = result.is_str() {
                            string.to_string()
                        } else {
                            result.to_s(&globals.store)
                        }
                    } else {
                        return Err(MonorubyErr::method_not_found(
                            &globals.store,
                            IdentId::TO_S,
                            val,
                        ));
                    };
                    if let Some(prec) = precision {
                        if s.chars().count() > prec {
                            s = truncate_to_chars(&s, prec);
                        }
                    }
                    apply_width(&s, width, minus_flag, ' ')
                }
                'p' => {
                    // `%p` dispatches to `Object#inspect` (Ruby-level)
                    // so user-defined `inspect` overrides are honoured.
                    let mut s = if let Some(func_id) = globals.check_method(val, IdentId::INSPECT) {
                        let result = self.invoke_func_inner(
                            globals, func_id, val, &[], None, None,
                        )?;
                        if let Some(string) = result.is_str() {
                            string.to_string()
                        } else {
                            result.to_s(&globals.store)
                        }
                    } else {
                        val.inspect(&globals.store)
                    };
                    if let Some(prec) = precision {
                        if s.chars().count() > prec {
                            s = truncate_to_chars(&s, prec);
                        }
                    }
                    apply_width(&s, width, minus_flag, ' ')
                }
                'd' | 'i' | 'u' => {
                    let ival = val.coerce_to_integer(self, globals)?;
                    let s = match ival {
                        IntegerBase::Fixnum(v) => format!("{}", v),
                        IntegerBase::BigInt(v) => format!("{}", v),
                    };
                    let s = apply_int_precision(&s, precision);
                    format_integer_with_flags(
                        &s, width, zero_flag, minus_flag, plus_flag, space_flag,
                    )
                }
                'b' | 'B' => {
                    let ival = val.coerce_to_integer(self, globals)?;
                    // `+` / space force the signed form (e.g. `% b`,
                    // `%+b` on -10 → `-1010`). Otherwise CRuby uses
                    // two's-complement notation for negatives, even
                    // with explicit precision (`%.7b % -5` →
                    // `..11011`).
                    let signed_form = plus_flag || space_flag;
                    let neg_tc = if !signed_form {
                        match &ival {
                            IntegerBase::Fixnum(v) if *v < 0 => {
                                Some(format_neg_twos_complement(*v, 2, ch == 'B'))
                            }
                            IntegerBase::BigInt(v)
                                if v.sign() == num::bigint::Sign::Minus =>
                            {
                                Some(format_neg_twos_complement_bigint(v, 2, ch == 'B'))
                            }
                            _ => None,
                        }
                    } else {
                        None
                    };
                    match neg_tc {
                        Some(tc) => {
                            let prefix = if hash_flag {
                                if ch == 'B' { "0B" } else { "0b" }
                            } else {
                                ""
                            };
                            let body = format!("{}{}", prefix, tc);
                            let body = if let Some(p) = precision {
                                apply_twos_complement_precision(&body, p, '1')
                            } else {
                                body
                            };
                            apply_twos_complement_width(
                                &body, width, zero_flag, minus_flag, '1',
                            )
                        }
                        None => {
                            let (is_neg, abs_digits) = match &ival {
                                IntegerBase::Fixnum(v) => {
                                    if *v < 0 {
                                        (true, format!("{:b}", (*v as i128).unsigned_abs()))
                                    } else {
                                        (false, format!("{:b}", *v))
                                    }
                                }
                                IntegerBase::BigInt(v) => (
                                    v.sign() == num::bigint::Sign::Minus,
                                    format!("{:b}", v.magnitude()),
                                ),
                            };
                            let digits = apply_int_precision(&abs_digits, precision);
                            let is_zero = digits == "0";
                            let prefix = if hash_flag && !is_zero {
                                if ch == 'B' { "0B" } else { "0b" }
                            } else {
                                ""
                            };
                            format_int_with_prefix(
                                is_neg, &digits, prefix, width, zero_flag, minus_flag, plus_flag,
                                space_flag,
                            )
                        }
                    }
                }
                'o' => {
                    let ival = val.coerce_to_integer(self, globals)?;
                    let signed_form = plus_flag || space_flag;
                    let neg_tc = if !signed_form {
                        match &ival {
                            IntegerBase::Fixnum(v) if *v < 0 => {
                                Some(format_neg_twos_complement(*v, 8, false))
                            }
                            IntegerBase::BigInt(v)
                                if v.sign() == num::bigint::Sign::Minus =>
                            {
                                Some(format_neg_twos_complement_bigint(v, 8, false))
                            }
                            _ => None,
                        }
                    } else {
                        None
                    };
                    match neg_tc {
                        Some(tc) => {
                            let body = if let Some(p) = precision {
                                apply_twos_complement_precision(&tc, p, '7')
                            } else {
                                tc
                            };
                            apply_twos_complement_width(
                                &body, width, zero_flag, minus_flag, '7',
                            )
                        }
                        None => {
                            let (is_neg, abs_digits) = match &ival {
                                IntegerBase::Fixnum(v) => {
                                    if *v < 0 {
                                        (true, format!("{:o}", (*v as i128).unsigned_abs()))
                                    } else {
                                        (false, format!("{:o}", *v))
                                    }
                                }
                                IntegerBase::BigInt(v) => (
                                    v.sign() == num::bigint::Sign::Minus,
                                    format!("{:o}", v.magnitude()),
                                ),
                            };
                            let digits = apply_int_precision(&abs_digits, precision);
                            let prefix = if hash_flag {
                                if digits.starts_with('0') { "" } else { "0" }
                            } else {
                                ""
                            };
                            format_int_with_prefix(
                                is_neg, &digits, prefix, width, zero_flag, minus_flag, plus_flag,
                                space_flag,
                            )
                        }
                    }
                }
                'x' | 'X' => {
                    let ival = val.coerce_to_integer(self, globals)?;
                    let signed_form = plus_flag || space_flag;
                    let neg_tc = if !signed_form {
                        match &ival {
                            IntegerBase::Fixnum(v) if *v < 0 => {
                                Some(format_neg_twos_complement(*v, 16, ch == 'X'))
                            }
                            IntegerBase::BigInt(v)
                                if v.sign() == num::bigint::Sign::Minus =>
                            {
                                Some(format_neg_twos_complement_bigint(v, 16, ch == 'X'))
                            }
                            _ => None,
                        }
                    } else {
                        None
                    };
                    match neg_tc {
                        Some(tc) => {
                            let prefix = if hash_flag {
                                if ch == 'X' { "0X" } else { "0x" }
                            } else {
                                ""
                            };
                            let body = format!("{}{}", prefix, tc);
                            let fill = if ch == 'X' { 'F' } else { 'f' };
                            let body = if let Some(p) = precision {
                                apply_twos_complement_precision(&body, p, fill)
                            } else {
                                body
                            };
                            apply_twos_complement_width(
                                &body, width, zero_flag, minus_flag, fill,
                            )
                        }
                        None => {
                            let (is_neg, abs_digits) = match &ival {
                                IntegerBase::Fixnum(v) => {
                                    let abs = (*v as i128).unsigned_abs();
                                    if ch == 'X' {
                                        (*v < 0, format!("{:X}", abs))
                                    } else {
                                        (*v < 0, format!("{:x}", abs))
                                    }
                                }
                                IntegerBase::BigInt(v) => {
                                    let abs = v.magnitude();
                                    let s = if ch == 'X' {
                                        format!("{:X}", abs)
                                    } else {
                                        format!("{:x}", abs)
                                    };
                                    (v.sign() == num::bigint::Sign::Minus, s)
                                }
                            };
                            let digits = apply_int_precision(&abs_digits, precision);
                            let is_zero = digits == "0";
                            let prefix = if hash_flag && !is_zero {
                                if ch == 'X' { "0X" } else { "0x" }
                            } else {
                                ""
                            };
                            format_int_with_prefix(
                                is_neg, &digits, prefix, width, zero_flag, minus_flag, plus_flag,
                                space_flag,
                            )
                        }
                    }
                }
                'f' => {
                    let f = val.coerce_to_float(self, globals)?;
                    let prec = precision.unwrap_or(6);
                    let s = if f.is_infinite() {
                        "Inf".to_string()
                    } else if f.is_nan() {
                        "NaN".to_string()
                    } else {
                        let mut s = format!("{:.p$}", f.abs(), p = prec);
                        // `#` flag: keep `.` even at precision 0
                        // (`%#.0f % 123.4 == "123."`).
                        if hash_flag && !s.contains('.') {
                            s.push('.');
                        }
                        s
                    };
                    format_float_with_flags(
                        &s, f, width, zero_flag, minus_flag, plus_flag, space_flag,
                    )
                }
                'e' | 'E' => {
                    let f = val.coerce_to_float(self, globals)?;
                    let prec = precision.unwrap_or(6);
                    let s = if f.is_infinite() {
                        "Inf".to_string()
                    } else if f.is_nan() {
                        "NaN".to_string()
                    } else {
                        let raw = if ch == 'E' {
                            format!("{:.p$E}", f.abs(), p = prec)
                        } else {
                            format!("{:.p$e}", f.abs(), p = prec)
                        };
                        let s = if hash_flag {
                            ensure_decimal_point_scientific(&raw)
                        } else {
                            raw
                        };
                        normalize_sci_exponent(&s)
                    };
                    format_float_with_flags(
                        &s, f, width, zero_flag, minus_flag, plus_flag, space_flag,
                    )
                }
                'g' | 'G' => {
                    let f = val.coerce_to_float(self, globals)?;
                    let prec = precision.unwrap_or(6);
                    let prec = if prec == 0 { 1 } else { prec };
                    let s = format_g(f.abs(), prec, ch == 'G', hash_flag);
                    let s = normalize_sci_exponent(&s);
                    format_float_with_flags(
                        &s, f, width, zero_flag, minus_flag, plus_flag, space_flag,
                    )
                }
                'a' | 'A' => {
                    let f = val.coerce_to_float(self, globals)?;
                    let mut s = format_hex_float(f.abs(), precision, ch == 'A');
                    // `#` flag forces a decimal point even when the
                    // mantissa has no fractional digits (`%#.0a` on
                    // 16.25 → `"0x1.p+4"`).
                    if hash_flag {
                        let p_char = if ch == 'A' { 'P' } else { 'p' };
                        if let Some(p_idx) = s.find(p_char) {
                            let mantissa = &s[..p_idx];
                            if !mantissa.contains('.') {
                                s = format!("{}.{}", mantissa, &s[p_idx..]);
                            }
                        }
                    }
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
}
