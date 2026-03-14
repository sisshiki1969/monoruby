use super::*;
use num::BigInt;

//
// Marshal module
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_toplevel_module("Marshal").id();
    globals.define_builtin_module_func(klass, "dump", dump, 1);
}

/// ### Marshal.dump
/// - dump(obj) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Marshal/m/dump.html]
#[monoruby_builtin]
fn dump(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _pc: BytecodePtr) -> Result<Value> {
    let obj = lfp.arg(0);
    let mut buf: Vec<u8> = Vec::new();
    // Marshal version header
    buf.push(0x04);
    buf.push(0x08);
    marshal_dump_value(&mut buf, obj, globals)?;
    Ok(Value::bytes(buf))
}

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

    if exponent <= -4 || (exponent > ndigits && exponent > 1) || (exponent >= 1 && has_trailing_zeros(&digits, exponent)) {
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

fn marshal_dump_value(buf: &mut Vec<u8>, obj: Value, globals: &Globals) -> Result<()> {
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
        run_test("Marshal.dump(0)");
        run_test("Marshal.dump(1)");
        run_test("Marshal.dump(-1)");
        run_test("Marshal.dump(5)");
        run_test("Marshal.dump(-5)");
        run_test("Marshal.dump(122)");
        run_test("Marshal.dump(-123)");
        run_test("Marshal.dump(123)");
        run_test("Marshal.dump(-124)");
        run_test("Marshal.dump(255)");
        run_test("Marshal.dump(256)");
        run_test("Marshal.dump(65535)");
        run_test("Marshal.dump(65536)");
        run_test("Marshal.dump(0xffffff)");
        run_test("Marshal.dump(0x1000000)");
        run_test("Marshal.dump(0x3fffffff)");
        run_test("Marshal.dump(-0x40000000)");
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
        run_test("Marshal.dump(0.0)");
        //run_test("Marshal.dump(-0.0)"); // monoruby doesn't preserve -0.0 literal yet
        run_test("Marshal.dump(1.0)");
        run_test("Marshal.dump(-1.5)");
        run_test("Marshal.dump(1.0/0)");
        run_test("Marshal.dump(-1.0/0)");
        run_test("Marshal.dump(0.0/0.0)");
        run_test("Marshal.dump(3.14159265358979)");
        run_test("Marshal.dump(100.0)");
        run_test("Marshal.dump(12.0)");
        run_test("Marshal.dump(0.1)");
        run_test("Marshal.dump(0.001)");
        run_test("Marshal.dump(1.23e5)");
        run_test("Marshal.dump(1.23e-5)");
        run_test("Marshal.dump(1.7976931348623157e308)");
        run_test("Marshal.dump(0.0001)");
        run_test("Marshal.dump(0.00001)");
        run_test("Marshal.dump(-100.0)");
        run_test("Marshal.dump(-1.23e-5)");
        run_test("Marshal.dump(2.718281828459045)");
        run_test("Marshal.dump(10.0)");
        run_test("Marshal.dump(1000.0)");
    }
}
