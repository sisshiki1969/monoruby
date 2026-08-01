use super::*;
use crate::value::IntegerBase;
use crate::value::rvalue::{
    Encoding, RStringInner, eucjp_char_width, map_bytes_to_utf8, sjis_char_width,
};
use num::Signed;

///
/// How a `sprintf` run treats text: the encoding the result String is
/// tagged with, and whether the run happens in the byte↔U+00XX
/// surrogate space (see [`RStringInner::regex_view`]).
///
#[derive(Clone, Copy, Debug)]
pub(crate) struct FormatCtx {
    /// Negotiated encoding of the result String.
    enc: Encoding,
    /// Every String taking part is viewed as one `char` per byte
    /// ([`map_bytes_to_utf8`]) and the finished text is decoded back
    /// with [`RStringInner::from_mapped_utf8`]. Needed whenever raw
    /// bytes must survive the `&str`-space formatter: a byte-oriented
    /// result encoding, or a participant whose bytes are not valid
    /// UTF-8 (CRuby formats `"%s" % "\x80"` into a result that simply
    /// reports `valid_encoding? == false`, it does not substitute
    /// U+FFFD).
    byte_space: bool,
}

impl FormatCtx {
    /// A `&str`-space view of `s` for this run.
    fn view<'a>(&self, s: &'a RStringInner) -> Result<std::borrow::Cow<'a, str>> {
        if self.byte_space {
            Ok(std::borrow::Cow::Owned(map_bytes_to_utf8(s.as_bytes())))
        } else {
            s.regex_view()
        }
    }

    /// [`Self::view`] as an owned `String` (the format string itself,
    /// which the formatter iterates by `char`).
    pub(crate) fn view_owned(&self, s: &RStringInner) -> Result<String> {
        self.view(s).map(|v| v.into_owned())
    }

    /// Turn finished format text back into a String value, undoing the
    /// surrogate view when the run used one.
    pub(crate) fn finish(&self, s: &str) -> Value {
        let inner = if self.byte_space && !s.is_ascii() {
            RStringInner::from_mapped_utf8(s, self.enc)
        } else {
            RStringInner::from_encoding_scanned(s.as_bytes(), self.enc)
        };
        Value::string_from_inner(inner)
    }
}

///
/// Negotiate the result encoding for `fmt % arguments`: start from the
/// format String's encoding and fold in each String argument's via
/// `compatible_encoding`, raising `Encoding::CompatibilityError` on an
/// incompatible pair (CRuby semantics). Also decides whether the run
/// needs byte space, for which the values of a trailing named-reference
/// Hash count too — they reach the output just like positional ones.
///
pub(crate) fn negotiate_format(
    store: &Store,
    fmt: &RStringInner,
    arguments: &[Value],
) -> Result<FormatCtx> {
    // A format String that cannot hold ASCII cannot hold the directives
    // either; CRuby rejects it before reading a single specifier.
    if !fmt.encoding().is_ascii_compatible() {
        return Err(MonorubyErr::encoding_compatibility_error_with_store(
            store,
            format!("ASCII incompatible encoding: {}", fmt.encoding().name()),
        ));
    }
    let mut result_inner = fmt.clone();
    let mut broken = !fmt.is_valid_encoding();
    for v in arguments {
        let Some(inner) = v.is_rstring_inner() else {
            continue;
        };
        if !inner.is_valid_encoding() {
            broken = true;
        }
        match result_inner.compatible_encoding(&inner) {
            Some(combined) => {
                if combined != result_inner.encoding() {
                    result_inner.set_encoding(combined);
                }
            }
            None => {
                return Err(MonorubyErr::incompatible_encoding(
                    store,
                    result_inner.encoding(),
                    inner.encoding(),
                ));
            }
        }
    }
    // `%{name}` / `%<name>spec` read their values from a trailing Hash.
    // They only inform the byte-space decision here: folding them into
    // the encoding negotiation as well would be closer to CRuby, but
    // would also start raising `CompatibilityError` for pairs monoruby
    // has always accepted.
    if let Some(hash) = arguments.last().and_then(|v| v.try_hash_ty()) {
        for (_, v) in hash.iter() {
            if let Some(inner) = v.is_rstring_inner()
                && !inner.is_valid_encoding()
            {
                broken = true;
            }
        }
    }
    let enc = result_inner.encoding();
    Ok(FormatCtx {
        enc,
        // US-ASCII joins the byte-oriented encodings because `%c` can
        // put a high byte into a US-ASCII result (onigmo's `%c` code
        // space for it is the full byte range). Everything else that
        // stays US-ASCII is 7-bit, for which the surrogate view is the
        // identity, so this costs nothing.
        byte_space: enc.is_byte_oriented() || enc == Encoding::UsAscii || broken,
    })
}

///
/// CRuby's `rb_enc_uint_chr`: the raw bytes of the character `code`
/// denotes in `enc`. Unicode encodings read `code` as a Unicode scalar;
/// every other encoding reads it as the big-endian byte image of the
/// character itself, so `9415601` (`0x8FABB1`) is the EUC-JP three-byte
/// sequence for `é`.
///
/// The two error kinds follow CRuby: a value that simply overflows a
/// single-byte encoding's code space is a `RangeError`, everything else
/// that is not a character in `enc` — a negative value, a Unicode scalar
/// past `U+10FFFF`, an ill-formed EUC-JP / Shift_JIS byte image — is an
/// `ArgumentError: invalid character`.
///
fn encode_codepoint(code: i64, enc: Encoding) -> Result<Vec<u8>> {
    fn be_bytes(code: u32) -> Vec<u8> {
        if code <= 0xFF {
            vec![code as u8]
        } else if code <= 0xFFFF {
            vec![(code >> 8) as u8, code as u8]
        } else {
            vec![(code >> 16) as u8, (code >> 8) as u8, code as u8]
        }
    }
    /// Onigmo's EUC-JP `code_to_mbclen` plus its trailing-byte check:
    /// ASCII stands alone, and every byte of a multibyte image (SS2
    /// `8E xx`, JIS X 0208 `hi lo`, SS3 `8F hi lo`) must have its high
    /// bit set. Wider than the *decoder*'s notion of a valid character
    /// — `8E FF` has no JIS X 0201 meaning but `%c` still emits it.
    fn valid_euc_jp(b: &[u8]) -> bool {
        match b {
            [c] => *c <= 0x7F,
            [rest @ ..] => rest.iter().all(|c| *c >= 0x80),
        }
    }
    /// Onigmo's Shift_JIS `code_to_mbclen`: a lone byte must not be a
    /// double-byte lead, and a two-byte image is judged by its trail
    /// byte alone (the lead goes unchecked, so `01 40` is accepted).
    fn valid_sjis(b: &[u8]) -> bool {
        match b {
            [c] => !matches!(c, 0x81..=0x9F | 0xE0..=0xFC),
            [_, lo] => matches!(lo, 0x40..=0x7E | 0x80..=0xFC),
            _ => false,
        }
    }
    let invalid = || MonorubyErr::argumenterr("invalid character");
    let Ok(code) = u32::try_from(code) else {
        return Err(invalid());
    };
    match enc {
        // CRuby also emits the CESU-style three-byte form for a lone
        // surrogate (`%c` % 0xD800), producing a broken String; we
        // reject those instead.
        Encoding::Utf8 => {
            let c = char::from_u32(code).ok_or_else(invalid)?;
            let mut buf = [0u8; 4];
            Ok(c.encode_utf8(&mut buf).as_bytes().to_vec())
        }
        Encoding::EucJp => {
            let bytes = be_bytes(code);
            if code > 0xFF_FFFF || !valid_euc_jp(&bytes) {
                return Err(invalid());
            }
            Ok(bytes)
        }
        Encoding::Sjis(_) => {
            let bytes = be_bytes(code);
            if code > 0xFFFF || !valid_sjis(&bytes) {
                return Err(invalid());
            }
            Ok(bytes)
        }
        // Single-byte encodings — including US-ASCII, whose `%c` code
        // space onigmo takes as the full byte range: the character *is*
        // the byte, and anything wider overflows it.
        Encoding::Ascii8
        | Encoding::UsAscii
        | Encoding::Iso8859(_)
        | Encoding::NamedByte(_) => {
            if code > 0xFF {
                return Err(MonorubyErr::rangeerr(format!("{code} out of char range")));
            }
            Ok(vec![code as u8])
        }
        // ASCII-incompatible encodings never reach here: `sprintf` on
        // such a format String raises before any specifier is read.
        _ => Err(invalid()),
    }
}

///
/// `%c`: one character in the result encoding. An Integer goes through
/// [`encode_codepoint`]; anything else contributes its first character
/// (a String directly, other objects via `to_int` / `to_str`).
///
fn format_char(
    vm: &mut Executor,
    globals: &mut Globals,
    val: Value,
    ctx: FormatCtx,
) -> Result<String> {
    if let RV::Fixnum(i) = val.unpack() {
        let bytes = encode_codepoint(i, ctx.enc)?;
        return Ok(if ctx.byte_space {
            map_bytes_to_utf8(&bytes)
        } else {
            // Outside byte space `enc` is UTF-8 or US-ASCII, so the
            // bytes `encode_codepoint` produced are valid UTF-8.
            String::from_utf8_lossy(&bytes).into_owned()
        });
    }
    if let Some(inner) = val.is_rstring_inner() {
        // Take the argument's leading character *in its own encoding*,
        // so a byte-oriented or broken String contributes whole bytes
        // instead of raising on the UTF-8 check.
        let view = ctx.view(&inner)?;
        if ctx.byte_space {
            let width = match inner.encoding() {
                Encoding::EucJp => eucjp_char_width(inner.as_bytes()),
                Encoding::Sjis(_) => sjis_char_width(inner.as_bytes()),
                _ => None,
            }
            .unwrap_or(1);
            return Ok(view.chars().take(width).collect());
        }
        return Ok(match view.chars().next() {
            Some(c) => c.to_string(),
            None => String::new(),
        });
    }
    Ok(match val.coerce_to_char(vm, globals)? {
        Some(c) => c.to_string(),
        None => String::new(),
    })
}

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
    // Non-finite values (`Inf`/`NaN`) are never zero-padded, even with
    // the `0` flag — CRuby pads them with spaces.
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
/// For the `#` flag: ensure the mantissa contains a decimal point
/// (CRuby keeps the point even when no fractional digits follow).
/// Inserts `.` just before the exponent marker (`e`/`E`/`p`/`P`) or
/// at the end if there is none. No-op if a `.` is already present.
fn force_decimal_point(s: &str) -> String {
    if s.contains('.') {
        return s.to_string();
    }
    match s.find(['e', 'E', 'p', 'P']) {
        Some(pos) => format!("{}.{}", &s[..pos], &s[pos..]),
        None => format!("{}.", s),
    }
}

fn format_g(f: f64, precision: usize, uppercase: bool, strip: bool) -> String {
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
        if strip {
            strip_trailing_zeros_scientific(&s)
        } else {
            s
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
        if strip {
            strip_trailing_zeros_fixed(&s)
        } else {
            s
        }
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

/// Minimal digit run (one leading fill digit kept) for Ruby's `..`
/// two's-complement notation, plus the fill character itself, computed
/// from the *absolute value's* base-`base` digit string (lowercase).
fn neg_tc_minimal_from_abs(abs_digits: &str, base: u32, uppercase: bool) -> (char, String) {
    let max_digit = base - 1;
    let fill = {
        let c = char::from_digit(max_digit, base).unwrap();
        if uppercase { c.to_ascii_uppercase() } else { c }
    };
    let abs: Vec<u32> = abs_digits
        .chars()
        .map(|c| c.to_digit(base).unwrap())
        .collect();
    // (base-1)-complement of each digit, then +1 with carry.
    let mut comp: Vec<u32> = abs.iter().map(|&d| max_digit - d).collect();
    let mut carry = 1u32;
    for d in comp.iter_mut().rev() {
        let s = *d + carry;
        *d = s % base;
        carry = s / base;
    }
    let s: String = comp
        .iter()
        .map(|&d| {
            let c = char::from_digit(d, base).unwrap();
            if uppercase { c.to_ascii_uppercase() } else { c }
        })
        .collect();
    let stripped = s.trim_start_matches(fill);
    let digits = if stripped.is_empty() {
        fill.to_string()
    } else {
        format!("{}{}", fill, stripped)
    };
    (fill, digits)
}

/// Assemble a negative `%b/%o/%x` value in Ruby's `..` two's-complement
/// notation, honouring precision, the (`0b`/`0x`) prefix, width, the
/// `0` flag (pads with the fill digit, right after `..`) and `-`.
#[allow(clippy::too_many_arguments)]
fn format_neg_tc(
    abs_digits: &str,
    base: u32,
    uppercase: bool,
    precision: Option<usize>,
    prefix: &str,
    width: usize,
    zero_flag: bool,
    minus_flag: bool,
) -> String {
    let (fill, mut digits) = neg_tc_minimal_from_abs(abs_digits, base, uppercase);
    // Precision P => at least `max(min_len, P - 2)` digits after `..`
    // (the `..` accounts for two of the requested precision digits).
    if let Some(p) = precision {
        let target = std::cmp::max(digits.len(), p.saturating_sub(2));
        while digits.len() < target {
            digits.insert(0, fill);
        }
    }
    let total = |d: &str| prefix.len() + 2 + d.len();
    if zero_flag && !minus_flag && width > total(&digits) {
        while total(&digits) < width {
            digits.insert(0, fill);
        }
        format!("{}..{}", prefix, digits)
    } else {
        let body = format!("{}..{}", prefix, digits);
        apply_width(&body, width, minus_flag, ' ')
    }
}

/// Truncate `s` to at most `n` characters (Ruby string precision counts
/// characters, not bytes).
fn take_chars(s: &str, n: usize) -> String {
    s.chars().take(n).collect()
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

impl Executor {
    pub(crate) fn format_by_args(
        &mut self,
        globals: &mut Globals,
        self_str: &str,
        arguments: &[Value],
        ctx: FormatCtx,
    ) -> Result<String> {
        let mut arg_no = 0;
        // Track whether the format string has used a numbered (`N$`)
        // and/or an unnumbered (sequential `%s` / `*`) argument
        // reference. CRuby forbids mixing the two within one format
        // string.
        let mut used_numbered = false;
        let mut used_unnumbered = false;
        // A named reference (`%<n>` / `%{n}`) may not be mixed with
        // numbered or unnumbered references in one format string.
        let mut used_named = false;
        fn mark_numbered(
            n: usize,
            used_numbered: &mut bool,
            used_unnumbered: bool,
            arg_no: usize,
        ) -> Result<()> {
            if used_unnumbered {
                return Err(MonorubyErr::argumenterr(format!(
                    "numbered({n}) after unnumbered({arg_no})"
                )));
            }
            *used_numbered = true;
            Ok(())
        }
        fn mark_unnumbered(
            used_numbered: bool,
            used_unnumbered: &mut bool,
            arg_no: usize,
        ) -> Result<()> {
            if used_numbered {
                return Err(MonorubyErr::argumenterr(format!(
                    "unnumbered({}) mixed with numbered",
                    arg_no + 1
                )));
            }
            *used_unnumbered = true;
            Ok(())
        }
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

            // `%<name>spec` and `%{name}` — named references. The
            // `<name>` token may appear anywhere within the spec
            // (`%<x>d`, `%+15<x>.5f`, `%<x>+15.5f`, `%-15.5<x>f`),
            // so we accept it before flags, between flags and width,
            // between width and precision, and just before the type
            // char (handled inline below). The `{name}` token is the
            // to_s-only form and may be preceded by flags / width /
            // precision (e.g. `%-20.5{foo}`); we recognize it where
            // the type char would be expected.
            let mut named_val = try_consume_angle_named(
                self,
                globals,
                arguments,
                &mut named_hash_cache,
                &fchars,
                &mut i,
                flen,
            )?;

            // Detect (and consume) a `N$` positional argument reference at
            // the current position. Returns the parsed argument index
            // (1-based) without bounds-checking, advancing `*pos` past the
            // `$`. CRuby allows the `N$` reference both immediately after
            // `%` and after the flag characters (e.g. `%-2$d`).
            fn try_positional(
                fchars: &[char],
                flen: usize,
                pos: &mut usize,
            ) -> Option<usize> {
                let start = *pos;
                if start >= flen || !fchars[start].is_ascii_digit() || fchars[start] == '0'
                {
                    return None;
                }
                let mut j = start;
                while j < flen && fchars[j].is_ascii_digit() {
                    j += 1;
                }
                if j < flen && fchars[j] == '$' {
                    let mut num = 0usize;
                    for &c in &fchars[start..j] {
                        num = num * 10 + (c as usize) - ('0' as usize);
                    }
                    *pos = j + 1;
                    Some(num)
                } else {
                    None
                }
            }

            // Check for positional argument: non-zero digit(s) followed by '$'
            let mut positional_arg = if named_val.is_none() {
                match try_positional(&fchars, flen, &mut i) {
                    Some(num) => {
                        mark_numbered(num, &mut used_numbered, used_unnumbered, arg_no)?;
                        if num == 0 || num > arguments.len() {
                            return Err(MonorubyErr::argumenterr("too few arguments"));
                        }
                        Some(arguments[num - 1])
                    }
                    None => None,
                }
            } else {
                None
            };

            if i >= flen {
                return Err(MonorubyErr::argumenterr(
                    "malformed format string",
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
                        "malformed format string",
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
                                "malformed format string",
                            ));
                        }
                        ch = fchars[i];
                    }
                }
            }
            // A `N$` positional reference may also follow the flag
            // characters (e.g. `%-2$d`, `% 2$d`). Detect it here when it
            // was not already consumed before the flags.
            if positional_arg.is_none() && named_val.is_none() {
                if let Some(num) = try_positional(&fchars, flen, &mut i) {
                    mark_numbered(num, &mut used_numbered, used_unnumbered, arg_no)?;
                    if num == 0 || num > arguments.len() {
                        return Err(MonorubyErr::argumenterr("too few arguments"));
                    }
                    positional_arg = Some(arguments[num - 1]);
                    if i >= flen {
                        return Err(MonorubyErr::argumenterr(
                            "malformed format string",
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
            // Width (may be '*' or '*N$' to take the width from a
            // positional argument, e.g. `%1$*2$d`).
            let mut width = 0usize;
            if ch == '*' {
                i += 1; // skip '*'
                let width_val = if let Some(num) =
                    try_positional(&fchars, flen, &mut i)
                {
                    mark_numbered(num, &mut used_numbered, used_unnumbered, arg_no)?;
                    if num == 0 || num > arguments.len() {
                        return Err(MonorubyErr::argumenterr("too few arguments"));
                    }
                    arguments[num - 1]
                } else {
                    mark_unnumbered(used_numbered, &mut used_unnumbered, arg_no)?;
                    if arguments.len() <= arg_no {
                        return Err(MonorubyErr::argumenterr("too few arguments"));
                    }
                    let v = arguments[arg_no];
                    arg_no += 1;
                    v
                };
                let w = width_val.coerce_to_integer(self, globals)?;
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
                if i >= flen {
                    return Err(MonorubyErr::argumenterr(
                        "malformed format string",
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
                            "malformed format string",
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
                            "malformed format string",
                        ));
                    }
                    ch = fchars[i];
                }
            }
            // Precision: `.N`, `.*` (sequential), or `.*N$`
            // (positional). A negative `.*` precision is ignored
            // (treated as no precision), matching CRuby.
            let mut precision = None;
            if ch == '.' {
                i += 1;
                if i >= flen {
                    return Err(MonorubyErr::argumenterr(
                        "malformed format string",
                    ));
                }
                ch = fchars[i];
                if ch == '*' {
                    i += 1; // skip '*'
                    let prec_val = if let Some(num) =
                        try_positional(&fchars, flen, &mut i)
                    {
                        mark_numbered(num, &mut used_numbered, used_unnumbered, arg_no)?;
                        if num == 0 || num > arguments.len() {
                            return Err(MonorubyErr::argumenterr("too few arguments"));
                        }
                        arguments[num - 1]
                    } else {
                        mark_unnumbered(used_numbered, &mut used_unnumbered, arg_no)?;
                        if arguments.len() <= arg_no {
                            return Err(MonorubyErr::argumenterr("too few arguments"));
                        }
                        let v = arguments[arg_no];
                        arg_no += 1;
                        v
                    };
                    match prec_val.coerce_to_integer(self, globals)? {
                        IntegerBase::Fixnum(v) => {
                            if v >= 0 {
                                precision = Some(v as usize);
                            }
                            // negative precision: ignored (stays None)
                        }
                        IntegerBase::BigInt(_) => {
                            return Err(MonorubyErr::argumenterr("precision too big"));
                        }
                    }
                    if i >= flen {
                        return Err(MonorubyErr::argumenterr(
                            "malformed format string",
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
                                "malformed format string",
                            ));
                        }
                        ch = fchars[i];
                    }
                    precision = Some(prec);
                }
            }
            // A `N$` positional *value* reference may follow the width
            // / precision (e.g. `%*1$.*2$3$d`).
            if positional_arg.is_none() && named_val.is_none() {
                if let Some(num) = try_positional(&fchars, flen, &mut i) {
                    mark_numbered(num, &mut used_numbered, used_unnumbered, arg_no)?;
                    if num == 0 || num > arguments.len() {
                        return Err(MonorubyErr::argumenterr("too few arguments"));
                    }
                    positional_arg = Some(arguments[num - 1]);
                    if i >= flen {
                        return Err(MonorubyErr::argumenterr(
                            "malformed format string",
                        ));
                    }
                    ch = fchars[i];
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
                            "malformed format string",
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
                if used_numbered || used_unnumbered {
                    return Err(MonorubyErr::argumenterr(
                        "named reference is mixed with numbered/unnumbered",
                    ));
                }
                used_named = true;
                let hash = get_named_hash(arguments, &mut named_hash_cache)
                    .ok_or_else(|| MonorubyErr::argumenterr("one hash required"))?;
                let key_val = Value::symbol_from_str(&key);
                let val =
                    hash_lookup_or_keyerror(self, globals, &hash, key_val, key.as_str(), '{')?;
                // A String value goes through the run's view so its raw
                // bytes survive (`%{k}` with a binary or broken value),
                // exactly like `%s` below.
                let mut s = match val.is_rstring_inner() {
                    Some(inner) => match ctx.view(&inner) {
                        Ok(v) => v.into_owned(),
                        // Broken bytes outside byte space: keep the old
                        // lossy render rather than raising.
                        Err(_) => val.coerce_to_s(self, globals)?,
                    },
                    None => val.coerce_to_s(self, globals)?,
                };
                if let Some(prec) = precision {
                    if s.chars().count() > prec {
                        s = s.chars().take(prec).collect();
                    }
                }
                format_str += &apply_width(&s, width, minus_flag, ' ');
                i = j + 1;
                continue;
            }
            // Enforce CRuby's rule that named references cannot be
            // mixed with numbered/unnumbered ones.
            if named_val.is_some() {
                if used_numbered || used_unnumbered {
                    return Err(MonorubyErr::argumenterr(
                        "named reference is mixed with numbered/unnumbered",
                    ));
                }
                used_named = true;
            } else if used_named {
                return Err(MonorubyErr::argumenterr(
                    "numbered/unnumbered reference is mixed with named",
                ));
            }
            // CRuby validates the conversion character *before* demanding
            // an argument for it: `sprintf("%\n")` is malformed even with
            // no arguments left. Unprintable characters get the bare
            // message, printable ones are echoed (`… - %v`).
            if !matches!(
                ch,
                'c' | 's'
                    | 'p'
                    | 'd'
                    | 'i'
                    | 'u'
                    | 'b'
                    | 'B'
                    | 'o'
                    | 'x'
                    | 'X'
                    | 'f'
                    | 'e'
                    | 'E'
                    | 'g'
                    | 'G'
                    | 'a'
                    | 'A'
            ) {
                return Err(MonorubyErr::argumenterr(if ch.is_ascii_graphic() {
                    format!("malformed format string - %{ch}")
                } else {
                    "malformed format string".to_string()
                }));
            }
            // Determine val: positional, named, or sequential
            let val = if let Some(v) = positional_arg {
                v
            } else if let Some(v) = named_val {
                v
            } else {
                mark_unnumbered(used_numbered, &mut used_unnumbered, arg_no)?;
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
                    let s = format_char(self, globals, val, ctx)?;
                    apply_width(&s, width, minus_flag, ' ')
                }
                's' => {
                    // `%s` always dispatches to `Object#to_s` — it
                    // never tries `to_str`. If the receiver doesn't
                    // respond to `to_s` (e.g. a bare `BasicObject`),
                    // CRuby raises `NoMethodError`. Don't fall back
                    // to the C-level inspect.
                    //
                    // String arguments go through the run's view so a
                    // byte-oriented or broken 8-bit string contributes
                    // its bytes as U+00XX surrogates (decoded again by
                    // `FormatCtx::finish`) instead of being lossily
                    // re-rendered with U+FFFD.
                    let mut s = if let Some(inner) = val.is_rstring_inner() {
                        match ctx.view(&inner) {
                            Ok(v) => v.into_owned(),
                            // Broken UTF-8: keep the old lossy render
                            // instead of raising.
                            Err(_) => val.to_s(&globals.store),
                        }
                    } else if let Some(func_id) = globals.check_method(val, IdentId::TO_S) {
                        let result =
                            self.invoke_func_inner(globals, func_id, val, &[], None, None)?;
                        if let Some(inner) = result.is_rstring_inner() {
                            match ctx.view(&inner) {
                                Ok(v) => v.into_owned(),
                                Err(_) => result.to_s(&globals.store),
                            }
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
                            s = take_chars(&s, prec);
                        }
                    }
                    apply_width(&s, width, minus_flag, ' ')
                }
                'p' => {
                    // `%p` dispatches to `Object#inspect` (Ruby-level)
                    // so user-defined `inspect` overrides are honoured.
                    let s = if let Some(func_id) = globals.check_method(val, IdentId::INSPECT) {
                        let result =
                            self.invoke_func_inner(globals, func_id, val, &[], None, None)?;
                        if let Some(string) = result.is_str() {
                            string.to_string()
                        } else {
                            result.to_s(&globals.store)
                        }
                    } else {
                        val.inspect(&globals.store)
                    };
                    let s = match precision {
                        Some(prec) if s.chars().count() > prec => take_chars(&s, prec),
                        _ => s,
                    };
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
                    let (is_neg, abs_digits, pos_digits) = match ival {
                        IntegerBase::Fixnum(v) if v < 0 => {
                            (true, format!("{:b}", v.unsigned_abs()), None)
                        }
                        IntegerBase::Fixnum(v) => (false, String::new(), Some(format!("{:b}", v))),
                        IntegerBase::BigInt(v) if v.is_negative() => {
                            (true, format!("{:b}", -v), None)
                        }
                        IntegerBase::BigInt(v) => {
                            (false, String::new(), Some(format!("{:b}", v)))
                        }
                    };
                    if is_neg {
                        let prefix = if hash_flag {
                            if ch == 'B' { "0B" } else { "0b" }
                        } else {
                            ""
                        };
                        if plus_flag || space_flag {
                            // `+`/space disables two's-complement and
                            // uses sign-magnitude (`-1010`).
                            let digits = apply_int_precision(&abs_digits, precision);
                            format_int_with_prefix(
                                true, &digits, prefix, width, zero_flag, minus_flag,
                                plus_flag, space_flag,
                            )
                        } else {
                            format_neg_tc(
                                &abs_digits, 2, ch == 'B', precision, prefix, width,
                                zero_flag, minus_flag,
                            )
                        }
                    } else {
                        let digits = apply_int_precision(&pos_digits.unwrap(), precision);
                        // `apply_int_precision("0", Some(0))` is "" — so an
                        // empty result is also "zero", and the `#` prefix is
                        // suppressed (`"%#.0b" % 0 == ""`).
                        let is_zero = digits.is_empty() || digits == "0";
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
                'o' => {
                    let ival = val.coerce_to_integer(self, globals)?;
                    let (is_neg, abs_digits, pos_digits) = match ival {
                        IntegerBase::Fixnum(v) if v < 0 => {
                            (true, format!("{:o}", v.unsigned_abs()), None)
                        }
                        IntegerBase::Fixnum(v) => (false, String::new(), Some(format!("{:o}", v))),
                        IntegerBase::BigInt(v) if v.is_negative() => {
                            (true, format!("{:o}", -v), None)
                        }
                        IntegerBase::BigInt(v) => {
                            (false, String::new(), Some(format!("{:o}", v)))
                        }
                    };
                    if is_neg {
                        if plus_flag || space_flag {
                            let digits = apply_int_precision(&abs_digits, precision);
                            let prefix = if hash_flag { "0" } else { "" };
                            format_int_with_prefix(
                                true, &digits, prefix, width, zero_flag, minus_flag,
                                plus_flag, space_flag,
                            )
                        } else {
                            format_neg_tc(
                                &abs_digits, 8, false, precision, "", width, zero_flag,
                                minus_flag,
                            )
                        }
                    } else {
                        let digits = apply_int_precision(&pos_digits.unwrap(), precision);
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
                'x' | 'X' => {
                    let ival = val.coerce_to_integer(self, globals)?;
                    let upper = ch == 'X';
                    let (is_neg, abs_digits, pos_digits) = match ival {
                        IntegerBase::Fixnum(v) if v < 0 => {
                            (true, format!("{:x}", v.unsigned_abs()), None)
                        }
                        IntegerBase::Fixnum(v) => {
                            let d = if upper {
                                format!("{:X}", v)
                            } else {
                                format!("{:x}", v)
                            };
                            (false, String::new(), Some(d))
                        }
                        IntegerBase::BigInt(v) if v.is_negative() => {
                            (true, format!("{:x}", -v), None)
                        }
                        IntegerBase::BigInt(v) => {
                            let d = if upper {
                                format!("{:X}", v)
                            } else {
                                format!("{:x}", v)
                            };
                            (false, String::new(), Some(d))
                        }
                    };
                    if is_neg {
                        let prefix = if hash_flag {
                            if upper { "0X" } else { "0x" }
                        } else {
                            ""
                        };
                        if plus_flag || space_flag {
                            let mag = if upper {
                                abs_digits.to_uppercase()
                            } else {
                                abs_digits.clone()
                            };
                            let digits = apply_int_precision(&mag, precision);
                            format_int_with_prefix(
                                true, &digits, prefix, width, zero_flag, minus_flag,
                                plus_flag, space_flag,
                            )
                        } else {
                            format_neg_tc(
                                &abs_digits, 16, upper, precision, prefix, width,
                                zero_flag, minus_flag,
                            )
                        }
                    } else {
                        let digits = apply_int_precision(&pos_digits.unwrap(), precision);
                        // See the `%b` branch: precision 0 with value 0 gives
                        // "", which is still "zero" and suppresses the prefix
                        // (`"%#.0x" % 0 == ""`).
                        let is_zero = digits.is_empty() || digits == "0";
                        let prefix = if hash_flag && !is_zero {
                            if upper { "0X" } else { "0x" }
                        } else {
                            ""
                        };
                        format_int_with_prefix(
                            false, &digits, prefix, width, zero_flag, minus_flag, plus_flag,
                            space_flag,
                        )
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
                        format!("{:.p$}", f.abs(), p = prec)
                    };
                    // CRuby quirk: `%#f` does NOT force a point for an
                    // Integer argument (unlike `%#e`/`%#g`/`%#a`).
                    let s = if hash_flag && f.is_finite() && !val.is_integer() {
                        force_decimal_point(&s)
                    } else {
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
                    } else if ch == 'E' {
                        normalize_sci_exponent(&format!("{:.p$E}", f.abs(), p = prec))
                    } else {
                        normalize_sci_exponent(&format!("{:.p$e}", f.abs(), p = prec))
                    };
                    let s = if hash_flag && f.is_finite() {
                        force_decimal_point(&s)
                    } else {
                        s
                    };
                    format_float_with_flags(
                        &s, f, width, zero_flag, minus_flag, plus_flag, space_flag,
                    )
                }
                'g' | 'G' => {
                    let f = val.coerce_to_float(self, globals)?;
                    let prec = precision.unwrap_or(6);
                    let prec = if prec == 0 { 1 } else { prec };
                    // The `#` flag keeps trailing zeros and forces a
                    // decimal point.
                    let s = format_g(f.abs(), prec, ch == 'G', !hash_flag);
                    let s = normalize_sci_exponent(&s);
                    let s = if hash_flag && f.is_finite() {
                        force_decimal_point(&s)
                    } else {
                        s
                    };
                    format_float_with_flags(
                        &s, f, width, zero_flag, minus_flag, plus_flag, space_flag,
                    )
                }
                'a' | 'A' => {
                    let f = val.coerce_to_float(self, globals)?;
                    let s = format_hex_float(f.abs(), precision, ch == 'A');
                    let s = if hash_flag && f.is_finite() {
                        force_decimal_point(&s)
                    } else {
                        s
                    };
                    // `%a` zero-padding goes *after* the `0x` prefix
                    // (e.g. `0x00001.8p+7`), not before it.
                    let sign = if f.is_sign_negative() && !f.is_nan() {
                        "-"
                    } else if plus_flag {
                        "+"
                    } else if space_flag {
                        " "
                    } else {
                        ""
                    };
                    let (pfx, rest) = if s.starts_with("0x") || s.starts_with("0X") {
                        s.split_at(2)
                    } else {
                        ("", s.as_str())
                    };
                    if zero_flag && !minus_flag && width > sign.len() + s.len() {
                        let pad = width - sign.len() - pfx.len();
                        format!("{}{}{:0>w$}", sign, pfx, rest, w = pad)
                    } else {
                        apply_width(
                            &format!("{}{}", sign, s),
                            width,
                            minus_flag,
                            ' ',
                        )
                    }
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

        // Sequential (unnumbered) formatting that leaves positional arguments
        // unused: `$DEBUG` raises, and `$VERBOSE` warns (to `$stderr`, so
        // captured by ruby/spec's `complain`). A trailing keyword Hash is the
        // named-reference source, not a positional argument, so exclude it —
        // `format("test", k: 1)` must not warn.
        let positional_len = if arguments
            .last()
            .is_some_and(|v| v.try_hash_ty().is_some())
        {
            arguments.len() - 1
        } else {
            arguments.len()
        };
        if !used_numbered && !used_named && arg_no < positional_len {
            if globals
                .get_gvar(IdentId::get_id("$DEBUG"))
                .is_some_and(|v| v.as_bool())
            {
                return Err(MonorubyErr::argumenterr(
                    "too many arguments for format string",
                ));
            }
            if globals
                .get_gvar(IdentId::get_id("$VERBOSE"))
                .is_some_and(|v| v.as_bool())
            {
                self.ruby_warn(globals, "warning: too many arguments for format string")?;
            }
        }

        Ok(format_str)
    }
}
