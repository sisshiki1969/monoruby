use super::*;
use onigmo_regex::{Captures, FindCaptures, OnigmoEncoding, Regex};
use std::sync::Arc;
use std::sync::{LazyLock, RwLock};

static REGEX_CACHE: LazyLock<RwLock<RegexCache>> = LazyLock::new(|| RwLock::new(RegexCache::new()));

thread_local! {
    /// Compile-time diagnostics Onigmo reported for regexps built
    /// since the last [`RegexpInner::drain_pending_warnings`] call.
    /// Compilation can happen with no `Executor` in reach (bytecodegen
    /// compiles literals), so constructors queue here and the callers
    /// that do own a vm (`Regexp.new`, the `eval` family) drain and
    /// emit through the Ruby warning mechanism.
    static PENDING_REGEXP_WARNINGS: std::cell::RefCell<Vec<String>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

fn queue_regexp_warnings(regex: &Regex) {
    if regex.warnings().is_empty() {
        return;
    }
    PENDING_REGEXP_WARNINGS.with(|w| {
        let mut w = w.borrow_mut();
        for msg in regex.warnings() {
            // Onigmo's onig_syntax_warn already appends the offending
            // pattern (`...: /<source>/`), so the message is complete.
            w.push(format!("warning: {msg}"));
        }
    });
}

#[derive(Debug, Default)]
struct RegexCache(HashMap<(String, u32, OnigmoEncoding), Arc<CachedRegex>>);

/// A compiled pattern as the cache holds it, shared by every
/// `RegexpInner` built from the same (pattern, option, encoding), with
/// what is decided once per compiled pattern alongside it. Derefs to
/// the engine.
#[derive(Debug)]
struct CachedRegex {
    engine: Regex,
    /// The 256-bit byte set of a pattern that is a single-byte class
    /// (see [`RegexpInner::single_byte_class`]); `Some(None)` once
    /// decided not to be one.
    byte_class: std::sync::OnceLock<Option<[u64; 4]>>,
}

impl CachedRegex {
    fn new(engine: Regex) -> Self {
        Self {
            engine,
            byte_class: std::sync::OnceLock::new(),
        }
    }
}

impl std::ops::Deref for CachedRegex {
    type Target = Regex;
    fn deref(&self) -> &Regex {
        &self.engine
    }
}

impl RegexCache {
    fn new() -> Self {
        Self(HashMap::default())
    }
}

#[monoruby_object]
pub struct Regexp(Value);

#[derive(Clone, Debug)]
pub struct RegexpInner {
    regex: Arc<CachedRegex>,
    /// The original regex *source* bytes, exactly as supplied (before
    /// `\u{}` expansion and without any escaping of non-UTF-8 input),
    /// in `declared_encoding`. The matching engine (`regex`) only ever
    /// sees a UTF-8/ASCII view, but `Regexp#source` / `#==` / `#hash` /
    /// `#inspect` / `#to_s` reflect these bytes so non-UTF-8 sources
    /// (Shift_JIS / EUC-JP / binary) round-trip faithfully. `Arc` keeps
    /// `RegexpInner::clone` cheap.
    source: Arc<[u8]>,
    /// The encoding the matching engine runs under
    /// (`UTF8` / `ASCII`). Onigmo only exposes those two; richer
    /// encodings (EUC-JP, Shift_JIS, ISO-8859-*) fall through to
    /// either UTF-8 (ASCII-compatible multi-byte) or ASCII
    /// (BINARY) for the actual scan.
    encoding: OnigmoEncoding,
    /// CRuby-visible source encoding. Tracks the declared encoding
    /// of the regex (set by the source-string encoding plus the
    /// `n`/`u`/`e`/`s` modifiers); rendered by `Regexp#encoding`,
    /// `Regexp#fixed_encoding?`, and used by `Regexp.union`'s
    /// compatibility check.
    declared_encoding: crate::value::Encoding,
    /// True if the encoding was *pinned* by either an explicit
    /// `u`/`e`/`s`/`n` modifier or by non-ASCII content in the
    /// source. Returned by `Regexp#fixed_encoding?`.
    fixed_encoding: bool,
    /// `false` for the empty pattern handed out by
    /// `Regexp.allocate` (never run through `Regexp.new` /
    /// literal construction); `true` once the regex carries a
    /// user-supplied source. CRuby raises `TypeError` when
    /// methods that need the source (`#match`, etc.) are called on
    /// the unallocated form.
    initialized: bool,
    /// Whether the `n` modifier (or the `NOENCODING` option) was given
    /// at construction. CRuby's `ARG_ENCODING_NONE` is a property of
    /// how the regexp was *written*, not of the encoding it ended up
    /// with: `Regexp.new("\xff".b)` is ASCII-8BIT without it, and
    /// `Regexp.new("ab", Regexp::NOENCODING)` carries it while staying
    /// US-ASCII (#1516).
    noencoding: bool,
    /// The source bytes compiled under a native (non-UTF-8) Onigmo codec
    /// (`native_enc`) for byte matching against a subject in that
    /// encoding, cached per regexp so the per-match lookup is a pointer
    /// compare rather than a hash of the source (the global
    /// `NATIVE_CACHE` still dedups across regexps). One slot, filled by
    /// the first native encoding met (a regexp rarely meets two), sized
    /// to keep the payload within the RValue cell.
    native: std::cell::OnceCell<Arc<Regex>>,
    native_enc: std::cell::Cell<OnigmoEncoding>,
    /// Whether the US-ASCII compile of this pattern may stand in for
    /// `regex` on a 7-bit subject (see [`ascii_engine`](Self::ascii_engine)):
    /// 0 = not decided yet, 1 = yes (it lives in `native`), 2 = no.
    ascii_state: std::cell::Cell<u8>,
}

impl PartialEq for RegexpInner {
    fn eq(&self, other: &Self) -> bool {
        if Arc::ptr_eq(&self.regex, &other.regex) {
            return true;
        }
        self.source == other.source
            && self.encoding == other.encoding
            && self.declared_encoding == other.declared_encoding
    }
}

/// The error of a search that Onigmo refused (a start position past the
/// end, an internal limit); the positions computed above keep it from
/// happening, so this is not reachable in-test.
#[coverage(off)]
fn search_failed(err: onigmo_regex::OnigmoError) -> MonorubyErr {
    MonorubyErr::regexerr(format!("Search failed. {:?}", err))
}

/// The `RegexpError` CRuby's `rb_reg_initialize` gives for a pattern
/// that is broken in its own encoding, or `None` if it is not.
///
/// Onigmo's own wording is its internal reading of the offending byte
/// ("too short multibyte code string"), and a source tagged UTF-8 did
/// not even reach it — the `&str` conversion raised a bare
/// `RuntimeError` first (#1522).
pub(crate) fn broken_source_error(
    source: &[u8],
    enc: crate::value::Encoding,
) -> Option<MonorubyErr> {
    if RStringInner::from_encoding_scanned(source, enc).is_valid_encoding() {
        return None;
    }
    Some(MonorubyErr::regexerr(format!(
        "invalid multibyte character: /{}/",
        String::from_utf8_lossy(&crate::builtins::string::regexp_source_desc_bytes(
            source, enc, None,
        ))
    )))
}

impl RegexpInner {
    /// Ruby's Regexp::NOENCODING constant (value 32).
    /// When set in options, the regexp uses ASCII-8BIT (binary) encoding.
    pub const NOENCODING: u32 = 32;

    /// Ruby's Regexp::FIXEDENCODING constant (value 16).
    pub const FIXEDENCODING: u32 = 16;

    // Internal-only flag bits monoruby uses to encode the kcode
    // letter (`u`/`e`/`s`) the user wrote in the literal (`/.../e`)
    // or as a string flag-arg (`Regexp.new("...", "e")`). They sit
    // outside the Onigmo bit range (0x000F), the public Ruby range
    // (NOENCODING|FIXEDENCODING = 0x0030), and the internal
    // ARG_ENCODING_NONE (0x40) so the existing option-mask
    // strip in `regexp_new` still works after we add them.
    pub const KCODE_UTF8: u32 = 1 << 8;
    pub const KCODE_EUCJP: u32 = 1 << 9;
    pub const KCODE_SJIS: u32 = 1 << 10;
    pub const KCODE_MASK: u32 = Self::KCODE_UTF8 | Self::KCODE_EUCJP | Self::KCODE_SJIS;

    /// The onigmo matching pattern (UTF-8/ASCII view, with `\u{}`
    /// expanded). Use [`source_bytes`](Self::source_bytes) for the
    /// CRuby-visible `Regexp#source`.
    pub fn as_str(&self) -> &str {
        self.regex.as_str()
    }

    /// The original source bytes (CRuby `Regexp#source`), in
    /// `declared_encoding`.
    pub fn source_bytes(&self) -> &[u8] {
        &self.source
    }

    /// The source rendered as a `String` for display (`#inspect` /
    /// `#to_s`). Valid-UTF-8 sources pass through unchanged; non-UTF-8
    /// bytes are shown lossily (a rare edge for exotic-encoding regexps).
    pub fn source_string(&self) -> std::borrow::Cow<'_, str> {
        String::from_utf8_lossy(&self.source)
    }

    pub fn encoding(&self) -> OnigmoEncoding {
        self.encoding
    }

    /// Whether `source` embeds a byte >= 0x80 through a `\xHH` escape
    /// (see [`has_non_ascii_hex_escape`]).
    pub fn has_non_ascii_hex_escape(source: &[u8]) -> bool {
        has_non_ascii_hex_escape(source)
    }

    /// CRuby-visible source encoding (set at construction time
    /// from source-string encoding + `n`/`u`/`e`/`s` modifiers).
    pub fn declared_encoding(&self) -> crate::value::Encoding {
        self.declared_encoding
    }

    /// Whether the source encoding was pinned (`u`/`e`/`s`/`n`
    /// modifier set, or non-ASCII content in the source).
    pub fn fixed_encoding(&self) -> bool {
        self.fixed_encoding
    }

    /// True for any `RegexpInner` produced by the normal
    /// construction path; false for the empty placeholder
    /// returned by `Regexp.allocate`.
    pub fn initialized(&self) -> bool {
        self.initialized
    }

    /// Mark the receiver as the "uninitialized" placeholder used
    /// by `Regexp.allocate`. The matching engine still works (so
    /// `#==`/`#hash` don't blow up), but methods that read the
    /// source — `#match`, `#=~`, `#match?` — surface a
    /// `TypeError` to callers.
    pub fn mark_uninitialized(&mut self) {
        self.initialized = false;
    }

    pub fn option(&self) -> u32 {
        let mut opt = self.regex.option();
        if self.noencoding {
            opt |= Self::NOENCODING;
        }
        if self.fixed_encoding {
            // `Regexp#options` exposes FIXEDENCODING for a regexp whose
            // encoding is pinned (the `u`/`e`/`s` modifiers, or a source
            // with non-ASCII bytes). `raw_option`/`#==`/`#hash` are
            // unaffected — they intentionally ignore the encoding flags.
            opt |= Self::FIXEDENCODING;
        }
        opt
    }

    /// Returns the raw onigmo option without Ruby encoding flags.
    pub fn raw_option(&self) -> u32 {
        self.regex.option()
    }

    pub fn option_string(&self) -> String {
        let mut res = String::new();
        let option = self.option();
        if option & onigmo_regex::ONIG_OPTION_MULTILINE != 0 {
            res.push('m');
        }
        if option & onigmo_regex::ONIG_OPTION_IGNORECASE != 0 {
            res.push('i');
        }
        if option & onigmo_regex::ONIG_OPTION_EXTEND != 0 {
            res.push('x');
        }
        // CRuby's `Regexp#inspect` includes the `n` (NOENCODING /
        // ASCII-8BIT) flag but not `u`/`e`/`s` (which are normalised
        // away). Order is m-i-x-n.
        if option & Self::NOENCODING != 0 {
            res.push('n');
        }
        res
    }

    /// Escape `text` so the result can be embedded as a literal in
    /// a regex source. Mirrors CRuby's `Regexp.escape`/`Regexp.quote`,
    /// which extends the standard regex-meta set with `' '` (space)
    /// and `\t`/`\n`/`\r`/`\f`/`\v` whitespace so they survive
    /// unchanged when the result is later compiled with the `x`
    /// modifier. The `regex` crate's `escape` covers the meta set
    /// but skips whitespace, so we layer the whitespace handling on
    /// top.
    pub fn escape(text: &str) -> String {
        // SAFETY: `escape_bytes` only inserts ASCII backslashes
        // before ASCII metacharacters and passes every other byte
        // through unchanged, so a valid-UTF-8 input stays valid.
        String::from_utf8(Self::escape_bytes(text.as_bytes())).unwrap()
    }

    /// Byte-wise `Regexp.escape` / `Regexp.quote`, mirroring CRuby's
    /// `rb_reg_quote`. Escapes the regex metacharacters with a
    /// backslash, rewrites the ASCII whitespace controls to their
    /// `\n` / `\t` … forms, and passes every other byte through
    /// verbatim — including bytes that don't form valid UTF-8, so it
    /// works on "broken" strings.
    pub fn escape_bytes(bytes: &[u8]) -> Vec<u8> {
        let mut out = Vec::with_capacity(bytes.len());
        for &b in bytes {
            match b {
                b'[' | b']' | b'{' | b'}' | b'(' | b')' | b'|' | b'-' | b'*' | b'.'
                | b'\\' | b'?' | b'+' | b'^' | b'$' | b'#' | b' ' => {
                    out.push(b'\\');
                    out.push(b);
                }
                b'\n' => out.extend_from_slice(b"\\n"),
                b'\r' => out.extend_from_slice(b"\\r"),
                b'\x0c' => out.extend_from_slice(b"\\f"),
                b'\x0b' => out.extend_from_slice(b"\\v"),
                b'\t' => out.extend_from_slice(b"\\t"),
                other => out.push(other),
            }
        }
        out
    }
}

/// Expand Ruby's `\u{XXXX}` / `\u{XX YY ZZ}` regex-literal escapes into the
/// forms Onigmo understands (`\uHHHH` for BMP, raw UTF-8 for supplementary).
///
/// Pre-validate the regex source for the escape-shape errors
/// CRuby surfaces with a `: /<source>/` suffix. Returns
/// the input unchanged on success — the caller still needs to
/// run `expand_unicode_braces` for the `\u{...}` rewrite.
///
/// Errors covered:
///   - **trailing backslash** (`\` at end-of-string with no
///     following char) → `"too short escape sequence: /\\/"`.
///   - **`\x` with 0 hex digits** (`\x` followed by nothing or by
///     a non-hex character) → `"invalid hex escape: /\\xY/"`.
fn pre_validate_regex(src: &str) -> Result<()> {
    let bytes = src.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] != b'\\' {
            i += utf8_char_len(bytes[i]);
            continue;
        }
        // `\` at end-of-string. CRuby raises
        // "too short escape sequence: /<src>/".
        if i + 1 >= bytes.len() {
            return Err(MonorubyErr::regexerr(format!(
                "too short escape sequence: /{src}/"
            )));
        }
        let next = bytes[i + 1];
        if next == b'x' {
            // `\x` must be followed by 1-2 hex digits. Onigmo
            // accepts even zero digits silently when the next
            // byte happens to terminate the regex; CRuby always
            // raises `invalid hex escape`. We only flag the
            // zero-digit case here (Onigmo handles 1-2 digit
            // prefixes correctly on its own).
            if i + 2 >= bytes.len() || !bytes[i + 2].is_ascii_hexdigit() {
                return Err(MonorubyErr::regexerr(format!(
                    "invalid hex escape: /{src}/"
                )));
            }
        }
        if (next == b'p' || next == b'P') && bytes.get(i + 2) == Some(&b'{') {
            // `\p{name}` / `\P{name}` (Unicode property) must be closed
            // with `}`. Onigmo accepts an unterminated `\p{` silently;
            // CRuby raises RegexpError. Flag the no-closing-brace case
            // (a valid property always has one).
            if !bytes[i + 3..].contains(&b'}') {
                return Err(MonorubyErr::regexerr(format!(
                    "invalid character property name: /{src}/"
                )));
            }
        }
        // Skip the `\` plus the escaped char.
        i += 2;
    }
    Ok(())
}

/// Onigmo's `\u` handler only accepts exactly four hex digits, so the
/// Ruby-level brace form must be normalized before the source is handed off.
/// Leaves every other escape (including `\\u{...}`) untouched.
fn expand_unicode_braces(src: &str) -> Result<String> {
    if !src.contains("\\u") {
        return Ok(src.to_string());
    }
    let bytes = src.as_bytes();
    let mut out = String::with_capacity(src.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'\\' {
            if i + 2 < bytes.len() && bytes[i + 1] == b'u' && bytes[i + 2] == b'{' {
                let content_start = i + 3;
                if let Some(rel_end) = bytes[content_start..].iter().position(|&b| b == b'}') {
                    let content = std::str::from_utf8(&bytes[content_start..content_start + rel_end])
                        .map_err(|_| MonorubyErr::regexerr("invalid utf-8 in \\u{...}"))?;
                    let raw =
                        std::str::from_utf8(&bytes[i..content_start + rel_end + 1]).unwrap_or("");
                    let mut buf = String::new();
                    let mut empty = true;
                    let mut bad_token: Option<&str> = None;
                    let mut out_of_range: Option<&str> = None;
                    for tok in content.split_ascii_whitespace() {
                        empty = false;
                        if tok.is_empty() || !tok.bytes().all(|b| b.is_ascii_hexdigit()) {
                            bad_token = Some(tok);
                            break;
                        }
                        if tok.len() > 6 {
                            out_of_range = Some(tok);
                            break;
                        }
                        match u32::from_str_radix(tok, 16) {
                            // A surrogate names no character, so CRuby
                            // refuses it the same way it refuses one
                            // past `U+10FFFF` (#1522).
                            Ok(0xd800..=0xdfff) => {
                                out_of_range = Some(tok);
                                break;
                            }
                            Ok(cp) if cp <= 0x10FFFF => {
                                if cp <= 0xFFFF {
                                    use std::fmt::Write;
                                    let _ = write!(buf, "\\u{:04X}", cp);
                                } else if let Some(ch) = char::from_u32(cp) {
                                    buf.push(ch);
                                } else {
                                    out_of_range = Some(tok);
                                    break;
                                }
                            }
                            _ => {
                                out_of_range = Some(tok);
                                break;
                            }
                        }
                    }
                    if bad_token.is_none() && out_of_range.is_none() && !empty {
                        out.push_str(&buf);
                        i = content_start + rel_end + 1;
                        continue;
                    }
                    // Match CRuby's error wording so spec tests
                    // that pattern-match the message pass:
                    //   - empty / non-hex digits → "invalid Unicode list"
                    //   - codepoint > 0x10FFFF or >6 hex digits →
                    //     "invalid Unicode range"
                    let kind = if out_of_range.is_some() {
                        "invalid Unicode range"
                    } else {
                        "invalid Unicode list"
                    };
                    return Err(MonorubyErr::regexerr(format!("{kind}: /{raw}/")));
                }
                return Err(MonorubyErr::regexerr(format!(
                    "invalid Unicode list: /{}/",
                    std::str::from_utf8(&bytes[i..]).unwrap_or("\\u{")
                )));
            }
            // `\uXXXX` (no braces): CRuby requires *exactly* four hex
            // digits and raises `invalid Unicode escape: /\uXYZ/`
            // when fewer are present (or `\u` is followed by a non-
            // hex char). Onigmo would otherwise accept the prefix
            // and silently mis-parse the regex, so we surface the
            // error before handing the pattern over.
            if i + 1 < bytes.len() && bytes[i + 1] == b'u' {
                let after_u = i + 2;
                let hex_end = bytes[after_u..]
                    .iter()
                    .take(4)
                    .take_while(|b| b.is_ascii_hexdigit())
                    .count();
                if hex_end < 4 {
                    let frag_end = (after_u + hex_end).min(bytes.len());
                    let frag = std::str::from_utf8(&bytes[i..frag_end]).unwrap_or("\\u");
                    return Err(MonorubyErr::regexerr(format!(
                        "invalid Unicode escape: /{frag}/"
                    )));
                }
                // Four digits that name a surrogate name no character,
                // as in the braced form above (#1522).
                if let Ok(cp) = u32::from_str_radix(&src[after_u..after_u + 4], 16)
                    && (0xd800..=0xdfff).contains(&cp)
                {
                    return Err(MonorubyErr::regexerr(format!(
                        "invalid Unicode range: /{src}/"
                    )));
                }
                // Fall through: 4 valid hex digits, copy through to
                // Onigmo verbatim.
            }
            // Copy the backslash and the following character (if any) verbatim
            // so standard escapes (e.g. `\\`, `\n`, `\x{...}`) survive the rewrite untouched.
            out.push('\\');
            i += 1;
            if i < bytes.len() {
                let ch_len = utf8_char_len(bytes[i]);
                out.push_str(&src[i..i + ch_len]);
                i += ch_len;
            }
            continue;
        }
        // Copy one UTF-8 scalar verbatim.
        let ch_len = utf8_char_len(bytes[i]);
        out.push_str(&src[i..i + ch_len]);
        i += ch_len;
    }
    Ok(out)
}

/// Resolve the declared (CRuby-visible) encoding for a regex
/// from its source bytes, options, and optional `kcode` flag.
/// Returns `(encoding, fixed_encoding)`.
///
/// CRuby's resolution order:
///   1. `n` modifier or `NOENCODING` flag → BINARY (fixed).
///   2. `u`/`e`/`s` modifier → UTF-8 / EUC-JP / Windows-31J (fixed).
///   3. `FIXEDENCODING` flag → keep the source encoding, pinned.
///   4. Source contains non-ASCII bytes → source's encoding,
///      pinned (CRuby pins because the literal cannot be
///      re-interpreted as US-ASCII).
///   5. Otherwise (pure 7-bit content, no modifier) → `US-ASCII`,
///      *not* pinned. Such regexps freely combine with strings
///      of any ASCII-compatible encoding.
pub(crate) fn resolve_declared_encoding(
    source: &[u8],
    option: u32,
    kcode: Option<u32>,
    source_encoding: Option<crate::value::Encoding>,
) -> (crate::value::Encoding, bool) {
    use crate::value::Encoding;
    // Non-ASCII is decided on the *raw source* bytes (which may be
    // non-UTF-8, e.g. a Shift_JIS pattern), not the escaped UTF-8 view
    // the matching engine sees.
    let has_non_ascii = source.iter().any(|&b| b >= 0x80);
    if option & RegexpInner::NOENCODING != 0 {
        // `/.../n` (NOENCODING): BINARY when the source carries
        // non-ASCII content — either raw bytes >= 0x80 or a `\xHH`
        // escape decoding to one (`/\xc2\xa1/n` is BINARY) — US-ASCII
        // otherwise. CRuby's regex parser only pins to ASCII-8BIT when
        // there's actual binary content; pure-ASCII patterns stay
        // re-tag-free.
        if has_non_ascii || has_non_ascii_hex_escape(source) {
            return (Encoding::Ascii8, true);
        }
        return (Encoding::UsAscii, false);
    }
    if let Some(kc) = kcode {
        let enc = if kc & RegexpInner::KCODE_UTF8 != 0 {
            Encoding::Utf8
        } else if kc & RegexpInner::KCODE_EUCJP != 0 {
            Encoding::EucJp
        } else if kc & RegexpInner::KCODE_SJIS != 0 {
            // CRuby's `/.../s` modifier sets Windows-31J (CP932),
            // not canonical Shift_JIS. Use Sjis(1) so that
            // `/abc/s.encoding.name == "Windows-31J"`.
            Encoding::Sjis(1)
        } else {
            // Bit set but unknown — fall through to source-encoding logic.
            return source_encoding_fallback(source_encoding, has_non_ascii, option);
        };
        return (enc, true);
    }
    // A BINARY source whose only high bytes are `\xHH` escapes is still
    // binary content (CRuby's `unescape_nonascii` pins ASCII-8BIT on
    // such an escape): `Regexp.new("[\xC2-\xDF]".b).encoding` is BINARY.
    if source_encoding == Some(Encoding::Ascii8) && has_non_ascii_hex_escape(source) {
        return (Encoding::Ascii8, true);
    }
    source_encoding_fallback(source_encoding, has_non_ascii, option)
}

fn source_encoding_fallback(
    source_encoding: Option<crate::value::Encoding>,
    has_non_ascii: bool,
    option: u32,
) -> (crate::value::Encoding, bool) {
    use crate::value::Encoding;
    let fixed_flag = option & RegexpInner::FIXEDENCODING != 0;
    match source_encoding {
        Some(src) if has_non_ascii => (src, true),
        // An ASCII-incompatible source encoding (UTF-16/32) is pinned
        // even when the content is all 7-bit: such a string can never be
        // re-interpreted as US-ASCII.
        Some(src) if !src.is_ascii_compatible() => (src, true),
        Some(src) if fixed_flag => (src, true),
        Some(_) => (Encoding::UsAscii, false),
        None if has_non_ascii => (Encoding::Utf8, true),
        None => (Encoding::UsAscii, fixed_flag),
    }
}

/// Scan `source` for a `\xHH` hexadecimal escape whose byte value is
/// non-ASCII (>= 0x80), e.g. `\xc2` or `\xFF`. Used by the `/.../n`
/// (NOENCODING) path to pin the encoding to BINARY when the pattern
/// embeds high bytes via escapes (`/\xc2\xa1/n.encoding == BINARY`).
/// A literal escaped backslash (`\\`) is skipped so `\\xFF` is not
/// mistaken for a high-byte escape.
pub(crate) fn has_non_ascii_hex_escape(source: &[u8]) -> bool {
    let mut i = 0;
    while i + 1 < source.len() {
        if source[i] != b'\\' {
            i += 1;
            continue;
        }
        match source[i + 1] {
            b'x' => {
                let mut j = i + 2;
                let mut val: u32 = 0;
                let mut n = 0;
                while j < source.len() && n < 2 && source[j].is_ascii_hexdigit() {
                    val = val * 16 + (source[j] as char).to_digit(16).unwrap();
                    j += 1;
                    n += 1;
                }
                if n > 0 && val >= 0x80 {
                    return true;
                }
                i = j;
            }
            // Any other escaped char (incl. `\\`) consumes both bytes,
            // so an escaped backslash can't start a spurious `\x`.
            _ => i += 2,
        }
    }
    false
}

/// Scan `src` for a `\u` escape (`\uXXXX` or `\u{...}`) whose
/// codepoint is non-ASCII (>= 0x80). Returns `true` for the first
/// such escape, `false` if none. Used to pin the declared encoding
/// to UTF-8 when the source contains a non-ASCII Unicode escape,
/// matching CRuby's `\u`-fixes-encoding behavior.
/// Whether `src` has a backslash escape whose letter is one of `letters`
/// (`\\` pairs are stepped over, so an escaped backslash before a `p`
/// does not count).
fn has_escape(src: &[u8], letters: &[u8]) -> bool {
    let mut i = 0;
    while i + 1 < src.len() {
        if src[i] == b'\\' {
            if letters.contains(&src[i + 1]) {
                return true;
            }
            i += 2;
        } else {
            i += 1;
        }
    }
    false
}

fn has_non_ascii_unicode_escape(src: &str) -> bool {
    let bytes = src.as_bytes();
    let mut i = 0;
    while i + 1 < bytes.len() {
        if bytes[i] == b'\\' && bytes[i + 1] == b'u' {
            if i + 2 < bytes.len() && bytes[i + 2] == b'{' {
                let content_start = i + 3;
                if let Some(rel_end) = bytes[content_start..].iter().position(|&b| b == b'}') {
                    if let Ok(content) = std::str::from_utf8(
                        &bytes[content_start..content_start + rel_end],
                    ) {
                        for tok in content.split_ascii_whitespace() {
                            if tok.bytes().all(|b| b.is_ascii_hexdigit()) && !tok.is_empty() {
                                if let Ok(cp) = u32::from_str_radix(tok, 16) {
                                    if cp >= 0x80 {
                                        return true;
                                    }
                                }
                            }
                        }
                    }
                    i = content_start + rel_end + 1;
                    continue;
                }
            } else if i + 5 < bytes.len() {
                let hex = &bytes[i + 2..i + 6];
                if hex.iter().all(|b| b.is_ascii_hexdigit()) {
                    if let Ok(s) = std::str::from_utf8(hex) {
                        if let Ok(cp) = u32::from_str_radix(s, 16) {
                            if cp >= 0x80 {
                                return true;
                            }
                        }
                    }
                    i += 6;
                    continue;
                }
            }
        }
        i += 1;
    }
    false
}

fn utf8_char_len(b: u8) -> usize {
    if b < 0x80 {
        1
    } else if b < 0xC0 {
        1 // continuation byte (should not be seen as a leader, but avoid panic)
    } else if b < 0xE0 {
        2
    } else if b < 0xF0 {
        3
    } else {
        4
    }
}

impl RegexpInner {

    /// Create `RegexpInfo` from `escaped_str` escaping all meta characters.
    /// Take (and clear) the compile-time diagnostics queued by regexp
    /// constructions on this thread. Callers with an `Executor` should
    /// forward each entry to `ruby_warn`.
    pub fn drain_pending_warnings() -> Vec<String> {
        PENDING_REGEXP_WARNINGS.with(|w| std::mem::take(&mut *w.borrow_mut()))
    }

    pub fn from_escaped(text: &str) -> Result<Self> {
        RegexpInner::with_option_and_encoding(Self::escape(text), 0, OnigmoEncoding::UTF8)
    }

    /// Create `RegexpInfo` from `reg_str` with `option`. Defaults
    /// to a UTF-8-tagged regex.
    pub fn with_option(reg_str: impl Into<String>, option: u32) -> Result<Self> {
        Self::with_option_and_encoding(reg_str, option, OnigmoEncoding::UTF8)
    }

    /// Create `RegexpInfo` from `reg_str` with `option` and the given
    /// matching engine. The declared (CRuby-visible) encoding is
    /// inferred from the source content alone:
    ///   - `OnigmoEncoding::ASCII` → BINARY (with non-ASCII bytes)
    ///     or US-ASCII (purely 7-bit).
    ///   - `OnigmoEncoding::UTF8` → UTF-8 (with non-ASCII bytes) or
    ///     US-ASCII (purely 7-bit).
    ///
    /// Callers that need the richer "modifier-pinned encoding"
    /// behaviour (`/.../e` → EUC-JP, `/.../s` → Shift_JIS, etc.)
    /// should go through `with_option_kcode` instead.
    pub fn with_option_and_encoding(
        reg_str: impl Into<String>,
        option: u32,
        encoding: OnigmoEncoding,
    ) -> Result<Self> {
        Self::with_option_kcode(reg_str, option, encoding, None, None)
    }

    /// Full-fidelity construction. `kcode` is the
    /// `KCODE_UTF8`/`KCODE_EUCJP`/`KCODE_SJIS` bit set by an
    /// explicit `n`/`u`/`e`/`s` modifier (or a corresponding string-
    /// flag arg to `Regexp.new`); `source_encoding` is the
    /// encoding tag of the source-string the user passed in, used
    /// when no kcode modifier is set and we fall back to the
    /// source's encoding for non-ASCII content.
    pub fn with_option_kcode(
        reg_str: impl Into<String>,
        option: u32,
        encoding: OnigmoEncoding,
        kcode: Option<u32>,
        source_encoding: Option<crate::value::Encoding>,
    ) -> Result<Self> {
        Self::with_option_kcode_source(reg_str, option, encoding, kcode, source_encoding, None)
    }

    /// As [`with_option_kcode`](Self::with_option_kcode) but with an
    /// explicit raw `source` byte sequence (CRuby `Regexp#source`).
    /// When `None`, the source is taken from `reg_str`'s bytes (the
    /// common case — a UTF-8/ASCII pattern that *is* its own source).
    /// `Regexp.new` on a non-UTF-8 String passes the original bytes here
    /// so they survive instead of being escaped away.
    pub fn with_option_kcode_source(
        reg_str: impl Into<String>,
        option: u32,
        encoding: OnigmoEncoding,
        kcode: Option<u32>,
        source_encoding: Option<crate::value::Encoding>,
        source: Option<Vec<u8>>,
    ) -> Result<Self> {
        let reg_str: String = reg_str.into();
        // Capture the source as written (before `\u{}` expansion); the
        // caller may override with the true raw bytes for non-UTF-8 input.
        let source: Arc<[u8]> =
            source.map_or_else(|| Arc::from(reg_str.as_bytes()), Arc::from);
        let (mut declared_encoding, mut fixed_encoding) =
            resolve_declared_encoding(&source, option, kcode, source_encoding);
        // CRuby pins the regex to UTF-8 when the source contains a
        // `\u` escape that decodes to a non-ASCII codepoint, even on
        // an otherwise pure-7-bit pattern (`/\u{1234}/.fixed_encoding?`
        // is `true`), and likewise for a `\p{…}` / `\P{…}` property
        // class (`/\p{Alpha}/.encoding` is UTF-8 and pinned: the class
        // is defined over Unicode, whatever the subject). The `n`/`e`/`s`
        // modifiers and the `NOENCODING` flag override this — they leave
        // the explicit kcode intact.
        if option & Self::NOENCODING == 0
            && kcode.map(|k| k & Self::KCODE_UTF8 != 0).unwrap_or(true)
            && (has_non_ascii_unicode_escape(&reg_str) || has_escape(reg_str.as_bytes(), b"pP"))
        {
            declared_encoding = crate::value::Encoding::Utf8;
            fixed_encoding = true;
        }
        // A source that is broken in its own encoding is refused
        // before Onigmo sees it, with the message CRuby's
        // `rb_reg_initialize` gives: Onigmo's own wording is its
        // internal reading of the byte ("too short multibyte code
        // string"), and monoruby's UTF-8 pre-check raised a bare
        // `RuntimeError` (#1522).
        if let Some(err) = broken_source_error(&source, declared_encoding) {
            return Err(err);
        }
        // Strip Ruby-only bits (`NOENCODING`, `FIXEDENCODING`,
        // `KCODE_*`) before handing the option mask to Onigmo —
        // those bits sit in the same word but Onigmo only understands
        // its own option bits (`IGNORECASE`/`MULTILINE`/`EXTEND`/...).
        let onigmo_option = option
            & !(Self::NOENCODING | Self::FIXEDENCODING | Self::KCODE_MASK);
        let noencoding = option & Self::NOENCODING != 0;
        // Surface the escape-shape errors that CRuby formats with
        // `": /<src>/"` *before* `expand_unicode_braces` rewrites
        // the source — once the `\u{...}` has been replaced with
        // `\uXXXX`, the original source isn't recoverable for the
        // error message.
        pre_validate_regex(&reg_str)?;
        let reg_str = expand_unicode_braces(&reg_str)?;
        match REGEX_CACHE
            .write()
            .unwrap()
            .0
            .entry((reg_str.clone(), onigmo_option, encoding))
        {
            std::collections::hash_map::Entry::Occupied(entry) => {
                // The cached Regex still carries its compile-time
                // diagnostics; CRuby re-warns on every compile of the
                // same pattern, so re-queue on cache hits too.
                queue_regexp_warnings(entry.get());
                Ok(RegexpInner {
                    regex: entry.get().clone(),
                    source,
                    encoding,
                    declared_encoding,
                    fixed_encoding,
                    initialized: true,
                    noencoding,
                    native: Default::default(),
                    native_enc: std::cell::Cell::new(OnigmoEncoding::UTF8),
                    ascii_state: std::cell::Cell::new(0),
                })
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                match Regex::new_with_option_and_encoding(&reg_str, onigmo_option, encoding) {
                    Ok(regexp) => {
                        queue_regexp_warnings(&regexp);
                        let regex = Arc::new(CachedRegex::new(regexp));
                        entry.insert(regex.clone());
                        Ok(RegexpInner {
                            regex,
                            source,
                            encoding,
                            declared_encoding,
                            fixed_encoding,
                            initialized: true,
                            noencoding,
                            native: Default::default(),
                    native_enc: std::cell::Cell::new(OnigmoEncoding::UTF8),
                    ascii_state: std::cell::Cell::new(0),
                        })
                    }
                    Err(err) => {
                        // Onigmo's error message doesn't include
                        // the offending source — CRuby
                        // appends `: /<source>/` so the spec
                        // tests can pattern-match. Match that
                        // format unless the message already
                        // carries a `:` (which means we already
                        // formatted it ourselves in a pre-pass).
                        let raw_msg = err.to_string();
                        let formatted = if raw_msg.contains(':') {
                            raw_msg
                        } else {
                            format!("{raw_msg}: /{reg_str}/")
                        };
                        Err(MonorubyErr::regexerr(formatted))
                    }
                }
            }
        }
    }

    pub fn get_group_members(&self, name: &str) -> Vec<i32> {
        self.regex.get_group_nembers(name)
    }

    pub fn capture_names(&self) -> Result<Vec<String>> {
        self.regex
            .capture_names()
            .map_err(|err| MonorubyErr::regexerr(err.message()))
    }

    pub fn captures<'a>(&self, given: &'a str, vm: &mut Executor) -> Result<Option<Captures<'a>>> {
        self.captures_from_pos(given, 0, vm)
    }

    /// Subjects up to this many bytes are probed for 7-bit content when
    /// the caller has no cached code range; a longer subject takes the
    /// UTF-8 engine unless the caller says otherwise. This bounds the
    /// probe on repeated searches deep into one long string
    /// (`index(re, pos)` in a loop), which would otherwise rescan the
    /// whole string per call.
    const ASCII_PROBE_LIMIT: usize = 2048;

    /// The engine for the UTF-8-view subject `given`: the US-ASCII
    /// compile of the pattern when the subject is 7-bit (`known_ascii`
    /// from the caller's cached code range, else a bounded probe of
    /// `given`), otherwise the UTF-8 one.
    ///
    /// CRuby does the same in `rb_reg_prepare_enc`: a 7-bit pattern
    /// whose encoding is not pinned is a US-ASCII regexp, and a 7-bit
    /// subject is matched with it as is. Under the UTF-8 codec Onigmo
    /// goes through `mbc_enc_len` / `onigenc_mbclen_approximate` at
    /// every position and through Unicode case folding for `/i`; on
    /// 7-bit data the results are identical, only slower
    /// (`/[a-z]/i.match?(s)` took 4.5x CRuby's time). The US-ASCII
    /// compile is the `native` slot's, shared with BINARY subjects
    /// (`onigmo_encoding_for(Ascii8)` is `ASCII` as well).
    fn engine_for(&self, given: &str, known_ascii: Option<bool>) -> &Regex {
        let ascii = match known_ascii {
            Some(ascii) => ascii,
            None => given.len() <= Self::ASCII_PROBE_LIMIT && given.is_ascii(),
        };
        if ascii && let Some(re) = self.ascii_engine() {
            re
        } else {
            &self.regex.engine
        }
    }

    /// The US-ASCII compile of this pattern, when it can stand in for
    /// the UTF-8 one on a 7-bit subject. Eligible: a 7-bit source whose
    /// encoding is not pinned, with no `\p{…}` / `\P{…}` (CRuby pins
    /// those to UTF-8, and Onigmo's US-ASCII codec knows only the POSIX
    /// classes) and no `\u` (the engine pattern carries the expanded
    /// form, not the source). Decided once per regexp; a compile
    /// failure — anything else the US-ASCII codec rejects — settles on
    /// "no" as well.
    fn ascii_engine(&self) -> Option<&Regex> {
        match self.ascii_state.get() {
            1 => {}
            2 => return None,
            _ => {
                let eligible = self.encoding == OnigmoEncoding::UTF8
                    && !self.fixed_encoding
                    && self.source.is_ascii()
                    && !has_escape(&self.source, b"pPu")
                    && self.native_regex(OnigmoEncoding::ASCII).is_ok();
                self.ascii_state.set(if eligible { 1 } else { 2 });
                if !eligible {
                    return None;
                }
            }
        }
        // The slot holds another codec if the regexp met that one first;
        // the UTF-8 engine is still correct, so take it rather than a
        // global-cache lookup per match.
        match self.native.get() {
            Some(re) if self.native_enc.get() == OnigmoEncoding::ASCII => Some(re),
            _ => None,
        }
    }

    pub fn captures_from_pos<'a>(
        &self,
        given: &'a str,
        pos: usize,
        vm: &mut Executor,
    ) -> Result<Option<Captures<'a>>> {
        self.captures_from_pos_with(self.engine_for(given, None), given, pos, vm)
    }

    /// [`captures_from_pos`](Self::captures_from_pos) with `\G` pinned
    /// to `gpos` rather than to `pos` (see
    /// [`find_spans_gpos`](Self::find_spans_gpos)).
    pub fn captures_from_pos_gpos<'a>(
        &self,
        given: &'a str,
        gpos: usize,
        pos: usize,
        vm: &mut Executor,
    ) -> Result<Option<Captures<'a>>> {
        match self
            .engine_for(given, None)
            .captures_from_pos_gpos(given, gpos, pos)
        {
            Ok(res) => {
                if let Some(captures) = &res {
                    vm.save_capture_special_variables(captures, given)
                } else {
                    vm.clear_capture_special_variables();
                }
                Ok(res)
            }
            Err(err) => Err(MonorubyErr::regexerr(format!("Capture failed. {:?}", err))),
        }
    }

    /// [`captures_from_pos`](Self::captures_from_pos) on an engine the
    /// caller picked once (`engine_for`) for a whole walk over `given`.
    fn captures_from_pos_with<'a>(
        &self,
        engine: &Regex,
        given: &'a str,
        pos: usize,
        vm: &mut Executor,
    ) -> Result<Option<Captures<'a>>> {
        match engine.captures_from_pos(given, pos) {
            Ok(res) => {
                if let Some(captures) = &res {
                    vm.save_capture_special_variables(captures, given)
                } else {
                    vm.clear_capture_special_variables();
                }
                Ok(res)
            }
            Err(err) => Err(MonorubyErr::regexerr(format!("Capture failed. {:?}", err))),
        }
    }

    /// The byte set of a pattern that matches exactly one ASCII
    /// character from a fixed set — an alternation of single characters
    /// (`>|<|&`), one bracket class (`[^*\-.0-9A-Z_a-z]`, `['&"<>]`),
    /// or one literal character — as 256 bits, so that an ASCII-only
    /// subject can be scanned with a table lookup per byte instead of
    /// an `onig_search` per match. Decided once per regexp; anything
    /// the classifier does not understand is "no", never a guess (see
    /// [`single_byte_class_of`]).
    pub(crate) fn single_byte_class(&self) -> Option<&[u64; 4]> {
        // Classified from the engine's pattern (the cache key: the
        // source with `\u{}` expanded), so every `RegexpInner` sharing
        // the compiled pattern shares the answer.
        self.regex
            .byte_class
            .get_or_init(|| {
                single_byte_class_of(self.regex.engine.as_str().as_bytes(), self.regex.engine.option())
            })
            .as_ref()
    }

    pub fn captures_iter<'a>(&self, given: &'a str) -> FindCaptures<'_, 'a> {
        // One probe for the whole walk: the iteration is O(len) anyway.
        self.engine_for(given, Some(given.is_ascii())).captures_iter(given)
    }

    /// The `OnigmoEncoding` for a subject of Ruby encoding `enc`, or
    /// `None` when Onigmo has no native codec (fall back to the
    /// UTF-8-view path).
    pub fn onigmo_encoding_for(enc: crate::value::Encoding) -> Option<OnigmoEncoding> {
        use crate::value::Encoding as E;
        Some(match enc {
            // BINARY: one char per byte, matched on the raw bytes. The
            // `\xNN` escapes of a `/n` pattern denote those bytes, and an
            // ASCII-only pattern is byte-transparent; a pattern pinned to
            // another encoding is refused by `check_match_encoding` first.
            E::Ascii8 => OnigmoEncoding::ASCII,
            E::EucJp => OnigmoEncoding::EUC_JP,
            // Ruby treats Shift_JIS / Windows-31J as one codec family;
            // Windows_31J is the superset CRuby actually pins for /s.
            E::Sjis(_) => OnigmoEncoding::Windows_31J,
            E::Iso8859(n) => match n {
                1 => OnigmoEncoding::ISO_8859_1,
                2 => OnigmoEncoding::ISO_8859_2,
                3 => OnigmoEncoding::ISO_8859_3,
                4 => OnigmoEncoding::ISO_8859_4,
                5 => OnigmoEncoding::ISO_8859_5,
                6 => OnigmoEncoding::ISO_8859_6,
                7 => OnigmoEncoding::ISO_8859_7,
                8 => OnigmoEncoding::ISO_8859_8,
                9 => OnigmoEncoding::ISO_8859_9,
                10 => OnigmoEncoding::ISO_8859_10,
                11 => OnigmoEncoding::ISO_8859_11,
                13 => OnigmoEncoding::ISO_8859_13,
                14 => OnigmoEncoding::ISO_8859_14,
                15 => OnigmoEncoding::ISO_8859_15,
                16 => OnigmoEncoding::ISO_8859_16,
                _ => return None,
            },
            // Single-byte NamedByte encodings with an Onigmo codec.
            // Multi-byte ones (Big5, GB18030, EUC-KR/TW, ...) are left
            // out: monoruby's char iteration treats NamedByte subjects
            // as one char per byte, which would disagree with Onigmo's
            // multi-byte char boundaries in MatchData offsets.
            E::NamedByte(_) => match enc.name() {
                "KOI8-R" => OnigmoEncoding::KOI8_R,
                "KOI8-U" => OnigmoEncoding::KOI8_U,
                "Windows-1250" => OnigmoEncoding::Windows_1250,
                "Windows-1251" => OnigmoEncoding::Windows_1251,
                "Windows-1252" => OnigmoEncoding::Windows_1252,
                "Windows-1253" => OnigmoEncoding::Windows_1253,
                "Windows-1254" => OnigmoEncoding::Windows_1254,
                "Windows-1257" => OnigmoEncoding::Windows_1257,
                _ => return None,
            },
            _ => None?,
        })
    }

    /// Compile (with caching) the *source bytes* of this regex under
    /// `enc` for a native-encoding byte match. The regular
    /// [`REGEX_CACHE`] is keyed on the UTF-8-view pattern; native
    /// compiles use the raw source bytes, so they get their own cache.
    fn native_regex(&self, enc: OnigmoEncoding) -> Result<Arc<Regex>> {
        static NATIVE_CACHE: LazyLock<RwLock<HashMap<(Vec<u8>, u32, OnigmoEncoding), Arc<Regex>>>> =
            LazyLock::new(|| RwLock::new(HashMap::default()));
        if let Some(re) = self.native.get()
            && self.native_enc.get() == enc
        {
            return Ok(re.clone());
        }
        let option = self.regex.option();
        let key = (self.source.to_vec(), option, enc);
        let re = if let Some(re) = NATIVE_CACHE.read().unwrap().get(&key) {
            re.clone()
        } else {
            match Regex::new_bytes_with_encoding(&self.source, option, enc) {
                Ok(re) => {
                    let re = Arc::new(re);
                    NATIVE_CACHE.write().unwrap().insert(key, re.clone());
                    re
                }
                Err(err) => return Err(MonorubyErr::regexerr(err.to_string())),
            }
        };
        if self.native.get().is_none() {
            self.native_enc.set(enc);
            let _ = self.native.set(re.clone());
        }
        Ok(re)
    }

    /// Byte-oriented match against a non-UTF-8 subject. `given` must
    /// be `given_value`'s raw bytes (the coordinate system for the
    /// resulting MatchData), and `enc` the Onigmo codec matching the
    /// subject's Ruby encoding. Saves `$~` exactly like
    /// [`captures_from_pos`](Self::captures_from_pos).
    pub fn captures_bytes_from_pos<'a>(
        &self,
        given: &'a [u8],
        given_value: Value,
        enc: OnigmoEncoding,
        pos: usize,
        vm: &mut Executor,
    ) -> Result<Option<onigmo_regex::CapturesBytes<'a>>> {
        let native = self.native_regex(enc)?;
        match native.captures_bytes_from_pos(given, pos) {
            Ok(res) => {
                if let Some(captures) = &res {
                    vm.save_capture_special_variables_bytes(captures, given_value);
                } else {
                    vm.clear_capture_special_variables();
                }
                Ok(res)
            }
            Err(err) => Err(MonorubyErr::regexerr(format!("Capture failed. {:?}", err))),
        }
    }

    /// Find the leftmost-first match for `given`.
    /// Returns `Match`s.
    pub fn find_one<'a>(
        &self,
        vm: &mut Executor,
        given: &'a str,
    ) -> Result<Option<std::ops::Range<usize>>> {
        match self.captures(given, vm)? {
            None => Ok(None),
            Some(captures) => Ok(captures.get(0).map(|m| m.range())),
        }
    }

    pub fn tos(&self) -> String {
        String::from_utf8_lossy(&self.tos_bytes()).into_owned()
    }

    /// CRuby `rb_reg_to_s` as the bytes it writes, in the pattern's own
    /// encoding. Byte-oriented because a pattern that is not UTF-8 has
    /// to come back out as the bytes that went in, which a Rust
    /// `String` cannot carry (#1516).
    pub fn tos_bytes(&self) -> Vec<u8> {
        let option = self.option();
        let mut m = option & onigmo_regex::ONIG_OPTION_MULTILINE != 0;
        let mut i = option & onigmo_regex::ONIG_OPTION_IGNORECASE != 0;
        let mut x = option & onigmo_regex::ONIG_OPTION_EXTEND != 0;
        // CRuby `rb_reg_to_s`: while the whole pattern is a single
        // wrapping option group `(?on-off:body)` (or `(?:body)`), fold
        // its flags into the displayed options and recurse on `body`.
        let mut src: &[u8] = &self.source;
        loop {
            let b = src;
            if b.len() < 4 || b[0] != b'(' || b[1] != b'?' {
                break;
            }
            let mut p = 2;
            let (mut on_m, mut on_i, mut on_x) = (false, false, false);
            while p < b.len() {
                match b[p] {
                    b'm' => on_m = true,
                    b'i' => on_i = true,
                    b'x' => on_x = true,
                    _ => break,
                }
                p += 1;
            }
            let (mut off_m, mut off_i, mut off_x) = (false, false, false);
            if p < b.len() && b[p] == b'-' {
                p += 1;
                while p < b.len() {
                    match b[p] {
                        b'm' => off_m = true,
                        b'i' => off_i = true,
                        b'x' => off_x = true,
                        _ => break,
                    }
                    p += 1;
                }
            }
            // Must be an option group (`...:`), not `(?=`, `(?<`, `(?#`,
            // `(?>`, a named or capturing group, etc.
            if p >= b.len() || b[p] != b':' {
                break;
            }
            // The opening paren must match the final character, i.e. the
            // group spans the entire pattern.
            if regexp_matching_close(b, 0) != Some(b.len() - 1) {
                break;
            }
            // CRuby applies the `on` set then the `off` set.
            if on_m {
                m = true;
            }
            if on_i {
                i = true;
            }
            if on_x {
                x = true;
            }
            if off_m {
                m = false;
            }
            if off_i {
                i = false;
            }
            if off_x {
                x = false;
            }
            src = &src[p + 1..src.len() - 1];
        }
        let mut out = Vec::with_capacity(src.len() + 10);
        out.extend_from_slice(
            format!(
                "(?{}{}{}{}{}{}{}:",
                if m { "m" } else { "" },
                if i { "i" } else { "" },
                if x { "x" } else { "" },
                if m && i && x { "" } else { "-" },
                if !m { "m" } else { "" },
                if !i { "i" } else { "" },
                if !x { "x" } else { "" },
            )
            .as_bytes(),
        );
        out.extend_from_slice(src);
        out.push(b')');
        out
    }

    pub fn inspect(&self) -> String {
        format!(
            "/{}/{}",
            escape_unescaped_slashes(&self.source_string()),
            self.option_string()
        )
    }

    /// CRuby `rb_reg_desc`: `/source/flags`, as the bytes it writes.
    /// `resenc` is the encoding the answer will be read in — what it
    /// cannot show is escaped by value, so a pattern in that same
    /// encoding comes back raw and any other comes back as `\x{…}`
    /// escapes rather than replacement characters (#1516).
    pub fn desc_bytes(&self, resenc: crate::value::Encoding) -> Vec<u8> {
        let mut out = vec![b'/'];
        out.extend_from_slice(&crate::builtins::string::regexp_source_desc_bytes(
            &self.source,
            self.declared_encoding(),
            Some(resenc),
        ));
        out.push(b'/');
        out.extend_from_slice(self.option_string().as_bytes());
        out
    }
}

/// Index of the `)` matching the `(` at `open`, or `None` if
/// unbalanced. Skips backslash escapes and `[...]` character classes
/// (where `(`/`)` are literal).
fn regexp_matching_close(b: &[u8], open: usize) -> Option<usize> {
    if open >= b.len() || b[open] != b'(' {
        return None;
    }
    let mut depth = 0usize;
    let mut k = open;
    let mut in_class = false;
    while k < b.len() {
        match b[k] {
            b'\\' => {
                k += 2;
                continue;
            }
            b'[' if !in_class => in_class = true,
            b']' if in_class => in_class = false,
            b'(' if !in_class => depth += 1,
            b')' if !in_class => {
                depth -= 1;
                if depth == 0 {
                    return Some(k);
                }
            }
            _ => {}
        }
        k += 1;
    }
    None
}

/// Escape forward slashes that aren't already escaped, leaving every
/// other backslash sequence intact. Used by `Regexp#inspect` so that
/// `Regexp.new("/foo/bar").inspect` is `"/\\/foo\\/bar/"` (matching
/// CRuby) without double-escaping `Regexp.new('\\\/')` to
/// `"/\\\\\\\\\\//"` etc.
fn escape_unescaped_slashes(src: &str) -> String {
    let mut out = String::with_capacity(src.len() + 4);
    let mut chars = src.chars();
    while let Some(c) = chars.next() {
        match c {
            '\\' => {
                // Pass through the backslash and whatever it escapes
                // (single char) verbatim, so existing `\/`/`\\`/`\n`/
                // ... aren't double-escaped.
                out.push('\\');
                if let Some(next) = chars.next() {
                    out.push(next);
                }
            }
            '/' => {
                out.push('\\');
                out.push('/');
            }
            other => out.push(other),
        }
    }
    out
}

/// Slice a match range out of a regex haystack view, tolerating
/// ranges that fall inside a UTF-8 character. An `/n` (byte-class)
/// pattern can match a partial character of the surrogate-space
/// view; decode such a range byte-wise instead of panicking.
fn view_slice(given: &str, range: std::ops::Range<usize>) -> std::borrow::Cow<'_, str> {
    match given.get(range.clone()) {
        Some(s) => std::borrow::Cow::Borrowed(s),
        None => std::borrow::Cow::Owned(
            String::from_utf8_lossy(&given.as_bytes()[range]).into_owned(),
        ),
    }
}

/// The group spans of one match: byte offsets into the subject.
pub(crate) type Spans = smallvec::SmallVec<[Option<(usize, usize)>; 4]>;

/// What the iterating and slicing pattern operations (`scan`, `split`,
/// `sub` / `gsub`, `slice`, `index`, …) walk: the bytes the engine sees
/// and the coordinate system every span is in.
///
/// - Text: a UTF-8 `&str` — the subject's own bytes (UTF-8 / US-ASCII /
///   ASCII-only content), or, for a byte-oriented encoding Onigmo has
///   no codec for, `regex_view`'s surrogate image (`mapped`), which the
///   caller decodes back to bytes with `from_mapped_utf8`.
/// - Bytes: the raw bytes of a byte-oriented subject with 8-bit content
///   (BINARY, Shift_JIS, EUC-JP, ISO-8859-x, …), matched by the source
///   compiled under Onigmo's codec for that encoding (`native_regex`),
///   so `/[^a-z]/n` over `"a\xffb".b` matches the one byte 0xFF and a
///   span is a byte range of the subject itself (#1377). The same path
///   `String#match` takes.
pub(crate) struct Subject<'a> {
    bytes: &'a [u8],
    text: Option<&'a str>,
    /// The subject's own encoding — what the chunks cut from it carry.
    enc: crate::value::Encoding,
    /// A text subject that is the surrogate image of a byte-oriented
    /// subject without a codec.
    mapped: bool,
    native: Option<OnigmoEncoding>,
    ascii: bool,
}

impl<'a> Subject<'a> {
    /// A text subject of a string in encoding `enc`; `mapped` when `s`
    /// is its surrogate image rather than its own bytes.
    pub(crate) fn text(s: &'a str, enc: crate::value::Encoding, mapped: bool) -> Self {
        Subject {
            bytes: s.as_bytes(),
            text: Some(s),
            enc,
            mapped,
            native: None,
            ascii: s.is_ascii(),
        }
    }

    /// The raw bytes of `inner`, to be matched under `native`.
    pub(crate) fn bytes(inner: &'a RStringInner, native: OnigmoEncoding) -> Self {
        Subject {
            bytes: inner.as_bytes(),
            text: None,
            enc: inner.encoding(),
            mapped: false,
            native: Some(native),
            ascii: false,
        }
    }

    pub(crate) fn as_bytes(&self) -> &'a [u8] {
        self.bytes
    }

    pub(crate) fn len(&self) -> usize {
        self.bytes.len()
    }

    pub(crate) fn is_ascii(&self) -> bool {
        self.ascii
    }

    /// The text of a text subject.
    pub(crate) fn as_text(&self) -> Option<&'a str> {
        self.text
    }

    /// Surrogate image of a byte-oriented subject without a codec.
    pub(crate) fn mapped(&self) -> bool {
        self.mapped
    }

    /// The encoding of the subject's own bytes.
    pub(crate) fn encoding(&self) -> crate::value::Encoding {
        self.enc
    }

    /// The encoding the walked bytes are in: UTF-8 for any text subject
    /// (a surrogate image included), the subject's own otherwise.
    pub(crate) fn view_encoding(&self) -> crate::value::Encoding {
        if self.text.is_some() {
            crate::value::Encoding::Utf8
        } else {
            self.enc
        }
    }

    /// The byte index just past the character at `pos` (`pos < len`):
    /// how a walk steps over an empty match.
    pub(crate) fn next_char_boundary(&self, pos: usize) -> usize {
        match self.text {
            Some(s) => {
                let mut next = pos + 1;
                while next < s.len() && !s.is_char_boundary(next) {
                    next += 1;
                }
                next
            }
            None => pos + crate::value::rvalue::char_width_at(self.enc, self.bytes, pos),
        }
    }

    /// The byte offset every character starts at.
    pub(crate) fn char_boundaries(&self) -> Vec<usize> {
        match self.text {
            Some(s) => s.char_indices().map(|(b, _)| b).collect(),
            None => {
                let mut v = vec![];
                let mut off = 0;
                while off < self.bytes.len() {
                    v.push(off);
                    off += crate::value::rvalue::char_width_at(self.enc, self.bytes, off);
                }
                v
            }
        }
    }

    /// The number of characters in `bytes[..pos]`.
    pub(crate) fn char_index(&self, pos: usize) -> usize {
        match self.text {
            Some(s) => s[..pos].chars().count(),
            None => crate::value::rvalue::char_count(self.enc, &self.bytes[..pos]),
        }
    }

    /// The byte offset of character index `cp`, clamped to the end.
    pub(crate) fn byte_offset(&self, cp: usize) -> usize {
        match self.text {
            Some(s) => s.char_indices().nth(cp).map_or(s.len(), |(b, _)| b),
            None => {
                let mut off = 0;
                for _ in 0..cp {
                    if off >= self.bytes.len() {
                        break;
                    }
                    off += crate::value::rvalue::char_width_at(self.enc, self.bytes, off);
                }
                off.min(self.bytes.len())
            }
        }
    }

    /// `bytes[range]` as a String of the subject's own encoding: a
    /// shared view of `owner` (the Value whose buffer `bytes` is, when
    /// it is stable) or a copy; a surrogate image is decoded back.
    pub(crate) fn chunk(&self, owner: Option<Value>, range: std::ops::Range<usize>) -> Value {
        if self.mapped() {
            let s = self.text.unwrap();
            return Value::string_from_inner(RStringInner::from_mapped_utf8(
                &view_slice(s, range),
                self.enc,
            ));
        }
        match owner {
            Some(v) => string_substring(v, range.start, range.end),
            None => Value::string_from_inner(RStringInner::from_encoding(
                &self.bytes[range],
                self.enc,
            )),
        }
    }

    /// The walked bytes as a fresh string, in `view_encoding`.
    pub(crate) fn to_inner(&self) -> RStringInner {
        match self.text {
            Some(s) => RStringInner::from_str_scanned(s),
            None => RStringInner::from_encoding_scanned(self.bytes, self.enc),
        }
    }
}

/// The group spans in `region` after a successful search.
pub(crate) fn spans_of(region: &onigmo_regex::Region) -> Spans {
    (0..region.len()).map(|i| region.pos(i)).collect()
}

/// Save `spans` (a match over `subject`) as `$~`; `owner` is the String
/// Value whose bytes a byte subject walks.
pub(crate) fn save_spans(vm: &mut Executor, subject: &Subject, spans: &[Option<(usize, usize)>], owner: Value) {
    match subject.as_text() {
        Some(s) => vm.save_capture_spans(spans, s),
        None => vm.save_capture_spans_bytes(spans, owner),
    }
}

impl RegexpInner {
    /// The byte subject for `inner` when it is a byte-oriented string
    /// with 8-bit content and Onigmo has a codec for its encoding (after
    /// the regexp/subject encoding check); `None` when the text path
    /// (`regex_view`) applies.
    ///
    /// Only a real Regexp takes this path (`regexp_pattern`): a pattern
    /// coerced from a String was escaped from the String's surrogate
    /// view, so its source is not the raw bytes a native compile needs.
    pub(crate) fn native_subject<'a>(
        &self,
        inner: &'a RStringInner,
        store: &Store,
        regexp_pattern: bool,
    ) -> Result<Option<Subject<'a>>> {
        if !regexp_pattern || !inner.needs_byte_mapping() {
            return Ok(None);
        }
        let Some(native) = Self::onigmo_encoding_for(inner.encoding()) else {
            return Ok(None);
        };
        crate::builtins::check_match_encoding(store, self, inner.encoding(), false)?;
        // CRuby refuses a regexp search over a broken string
        // (`rb_reg_prepare_re`: "invalid byte sequence").
        if !inner.is_valid_encoding() {
            return Err(MonorubyErr::argumenterr(format!(
                "invalid byte sequence in {}",
                inner.encoding().name()
            )));
        }
        Ok(Some(Subject::bytes(inner, native)))
    }

    /// Search `subject` from byte offset `pos`, recording the match's
    /// registers into `region`. Never touches `$~`.
    pub(crate) fn find_spans(
        &self,
        subject: &Subject,
        pos: usize,
        region: &mut onigmo_regex::Region,
    ) -> Result<bool> {
        self.find_spans_gpos(subject, pos, pos, region)
    }

    /// [`find_spans`](Self::find_spans) with `\G` pinned to `gpos`
    /// rather than to `pos`.
    ///
    /// A reverse search (`String#rindex`) anchors `\G` at the offset it
    /// was given and then looks for the *last* match starting at or
    /// before it, which it does by probing forward from every candidate
    /// start. Each probe is its own search, so without this every probe
    /// would rebind `\G` to itself; the match may also end past `gpos`
    /// (`/YOU.+\G.+/`), which is why the probes stay forward searches
    /// to the end of the subject rather than one backward search.
    pub(crate) fn find_spans_gpos(
        &self,
        subject: &Subject,
        gpos: usize,
        pos: usize,
        region: &mut onigmo_regex::Region,
    ) -> Result<bool> {
        let r = match (subject.as_text(), subject.native) {
            (Some(s), _) => self
                .engine_for(s, Some(subject.is_ascii()))
                .search_with_region_gpos(s.as_bytes(), gpos, pos, region),
            (None, Some(enc)) => self.native_regex(enc)?.search_bytes_gpos(
                subject.as_bytes(),
                gpos,
                pos,
                subject.len(),
                Some(region),
            ),
            (None, None) => unreachable!("a byte subject always has a codec"),
        };
        r.map(|r| r.is_some()).map_err(search_failed)
    }

    /// The position a walk continues from after the match `start..end`
    /// found from `pos`: past a non-empty match, one character past an
    /// empty one (past the end to terminate).
    fn next_walk_pos(subject: &Subject, start: usize, end: usize) -> usize {
        if end > start {
            end
        } else if start >= subject.len() {
            subject.len() + 1
        } else {
            subject.next_char_boundary(start)
        }
    }
}

// Utility methods

impl RegexpInner {
    /// Resolve `re_val` to a `RegexpInner` and run `f` on it. Accepts:
    ///   - an existing `Regexp` (used in place);
    ///   - a `String` (treated as a regexp source, escaped via
    ///     `from_escaped`);
    ///   - any other object that responds to `to_str` (the returned
    ///     String is then treated as a regexp source).
    /// All `replace_*` entry points share this dispatch — keep it in
    /// one place so the to_str-coercion semantics stay consistent.
    fn with_coerced_regexp<R>(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        f: impl FnOnce(&RegexpInner, &mut Executor, &mut Globals) -> Result<R>,
    ) -> Result<R> {
        if let Some(re) = re_val.is_regex() {
            return f(&re, vm, globals);
        }
        let s_owned;
        let view;
        let s: &str = if let Some(inner) = re_val.is_rstring_inner() {
            // `regex_view` instead of a plain UTF-8 read: a String
            // pattern in a byte-oriented encoding (ISO-8859-1,
            // BINARY, …) is matched through the same byte↔U+00XX
            // surrogate space as the haystack, so its 8-bit bytes
            // line up with the mapped haystack characters.
            view = inner.regex_view()?;
            &view
        } else {
            s_owned = re_val.coerce_to_str(vm, globals)?;
            &s_owned
        };
        let re = Self::from_escaped(s)?;
        f(&re, vm, globals)
    }

    /// Replaces the leftmost-first match with `replace`. `owner` is the
    /// String Value `subject` walks (for `$~`).
    pub(crate) fn replace_one(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        subject: &Subject,
        owner: Value,
        replace: &RStringInner,
    ) -> Result<(RStringInner, bool)> {
        Self::with_coerced_regexp(vm, globals, re_val, |re, vm, globals| {
            re.replace_once(vm, &globals.store, subject, owner, replace)
        })
    }

    /// Replaces the leftmost-first match with what the block answers for
    /// it. `owner` is the String Value `subject` walks (for `$~`); the
    /// matched chunk handed to the block is a copy, since the block may
    /// mutate the receiver.
    /// One replacement driven by a block. `bang` selects `sub!`'s
    /// contract over `sub`'s: CRuby's `sub!` runs `str_mod_check` after
    /// the yield and raises `RuntimeError: "string modified"` when the
    /// block resized the receiver, while `sub` works from a copy and
    /// reports whatever the copy says.
    ///
    /// The splice always runs over bytes copied out before the yield:
    /// `subject` borrows the receiver's live buffer, which a block that
    /// mutates the receiver may reallocate. `sub!` then re-reads the
    /// live bytes once the length check has passed, so an in-place
    /// same-length mutation from the block is visible to it as it is in
    /// CRuby.
    pub(crate) fn replace_one_block(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        subject: &Subject,
        owner: Value,
        bh: BlockHandler,
        bang: bool,
    ) -> Result<(RStringInner, bool)> {
        Self::with_coerced_regexp(vm, globals, re_val, |re, vm, globals| {
            let mut region = onigmo_regex::Region::new();
            if !re.find_spans(subject, 0, &mut region)? {
                vm.clear_capture_special_variables();
                return Ok((subject.to_inner(), false));
            }
            let spans = spans_of(&region);
            let (start, end) = spans[0].unwrap();
            save_spans(vm, subject, &spans, owner);
            let matched = subject.chunk(None, start..end);
            let before = subject.as_bytes().to_vec();
            let (view_enc, is_ascii, mapped) =
                (subject.view_encoding(), subject.is_ascii(), subject.mapped());
            let owner_len = owner.as_rstring_inner().len();
            let result = vm.invoke_block_once(globals, bh, &[matched])?;
            let haystack = if bang {
                crate::value::rvalue::string::check_string_not_modified(owner, owner_len)?;
                // A mapped subject is a surrogate view, not the
                // receiver's bytes, so only the direct walk can re-read.
                if mapped {
                    before
                } else {
                    owner.as_rstring_inner().as_bytes().to_vec()
                }
            } else {
                before
            };
            let rep_inner = block_result_to_inner(vm, globals, result, mapped)?;
            let res = RStringInner::splice_all(
                &globals.store,
                &haystack,
                view_enc,
                is_ascii,
                &[(start..end, rep_inner)],
            )?;
            Ok((res, true))
        })
    }

    /// Replaces all non-overlapping matches in `subject` with `replace`.
    /// `owner` is the String Value `subject` walks (for `$~`).
    pub(crate) fn replace_all(
        vm: &mut Executor,
        globals: &mut Globals,
        regexp: Value,
        subject: &Subject,
        owner: Value,
        replace: &RStringInner,
    ) -> Result<(RStringInner, bool)> {
        Self::with_coerced_regexp(vm, globals, regexp, |re, vm, globals| {
            re.replace_repeat(vm, &globals.store, subject, owner, replace)
        })
    }

    /// Replaces all non-overlapping matches in `recv` with the result of
    /// calling `bh` for each match.
    ///
    /// Matching and splicing run against a frozen snapshot of `recv`, so
    /// they cannot read freed memory if the block reallocates the
    /// receiver (no use-after-free). The live `recv` is length-checked
    /// after each block call: a length change raises
    /// `RuntimeError: "string modified"`, matching CRuby.
    pub(crate) fn replace_all_block(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        recv: Value,
        bh: BlockHandler,
        self_enc: Option<crate::value::Encoding>,
    ) -> Result<(RStringInner, bool)> {
        Self::with_coerced_regexp(vm, globals, re_val, |re, vm, globals| {
            // Probe the live receiver first. Until the first match no
            // block runs, so nothing can reallocate the buffer under the
            // view, and a miss — the common case for a normalizing
            // `gsub(/[^allowed]/) { … }` — then costs one copy of the
            // receiver instead of a frozen snapshot plus a splice.
            vm.clear_capture_special_variables();
            let regexp_pattern = re_val.is_regex().is_some();
            let first = {
                let inner = recv.as_rstring_inner();
                let view;
                let subject = match re.native_subject(inner, &globals.store, regexp_pattern)? {
                    Some(subject) => subject,
                    None => {
                        view = inner.regex_view()?;
                        Subject::text(&view, inner.encoding(), inner.needs_byte_mapping())
                    }
                };
                let mut region = onigmo_regex::Region::new();
                if !re.find_spans(&subject, 0, &mut region)? {
                    // What `splice_all` with no replacements would build.
                    return Ok((subject.to_inner(), false));
                }
                // Nothing matches before this position, so the real
                // walk over the snapshot can start here instead of
                // repeating the scan from the beginning.
                region.pos(0).unwrap().0
            };
            let snapshot = string_snapshot(recv);
            let tmp = vm.temp_len();
            vm.temp_push(snapshot);
            let res = re.replace_all_block_inner(
                vm,
                globals,
                snapshot,
                recv,
                bh,
                self_enc,
                first,
                regexp_pattern,
            );
            vm.temp_clear(tmp);
            res
        })
    }

    fn replace_all_block_inner(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
        snapshot: Value,
        recv: Value,
        bh: BlockHandler,
        self_enc: Option<crate::value::Encoding>,
        first: usize,
        regexp_pattern: bool,
    ) -> Result<(RStringInner, bool)> {
        // `snapshot` is frozen and temp-rooted by the caller, so the
        // subject and the matched views stay valid across `invoke_block`.
        let inner = snapshot.as_rstring_inner();
        let view;
        let subject = match self.native_subject(inner, &globals.store, regexp_pattern)? {
            Some(subject) => subject,
            None => {
                view = inner.regex_view()?;
                Subject::text(&view, inner.encoding(), inner.needs_byte_mapping())
            }
        };
        let recv_len = recv.as_rstring_inner().len();
        let mut range = vec![];
        let data = vm.get_block_data(globals, bh)?;

        vm.clear_capture_special_variables();
        // The non-overlapping walk of `captures_iter`, started at `first`
        // (the caller's probe found nothing before it): an empty match
        // right where the previous match ended is skipped by one
        // character rather than looping forever.
        let mut region = onigmo_regex::Region::new();
        let mut pos = first;
        let mut last_match_end: Option<usize> = None;
        let mut last_spans: Option<Spans> = None;
        while pos <= subject.len() {
            if !self.find_spans(&subject, pos, &mut region)? {
                break;
            }
            let spans = spans_of(&region);
            let (start, end) = spans[0].unwrap();
            if start == end && last_match_end == Some(end) {
                pos = subject.next_char_boundary(pos);
                continue;
            }
            pos = end;
            last_match_end = Some(end);

            let matched = subject.chunk(Some(snapshot), start..end);
            save_spans(vm, &subject, &spans, snapshot);
            last_spans = Some(spans);
            let result = vm.invoke_block(globals, &data, &[matched])?;
            check_string_not_modified(recv, recv_len)?;
            // CRuby raises Encoding::CompatibilityError if the
            // block returned a String whose encoding can't merge
            // with self's. Check before stringifying.
            if let Some(enc) = self_enc {
                if let Some(repl_inner) = result.is_rstring_inner() {
                    let dummy = crate::value::RStringInner::from_encoding(b"", enc);
                    if dummy.compatible_encoding(&repl_inner).is_none() {
                        return Err(MonorubyErr::incompatible_encoding(
                            &globals.store,
                            enc,
                            repl_inner.encoding(),
                        ));
                    }
                }
            }
            let replace = block_result_to_inner(vm, globals, result, subject.mapped())?;

            range.push((start..end, replace));
        }

        // CRuby's `str_gsub` sets `$~` to *its* last match once the walk
        // is over, so a block that matched something of its own does not
        // leave that behind as the caller's backref.
        if let Some(spans) = &last_spans {
            save_spans(vm, &subject, spans, snapshot);
        }

        let is_empty = range.is_empty();
        let res = RStringInner::splice_all(
            &globals.store,
            subject.as_bytes(),
            subject.view_encoding(),
            subject.is_ascii(),
            &range,
        )?;

        Ok((res, !is_empty))
    }

    /// Replaces the first match in `subject` using hash lookup. The
    /// matched text is looked up as a key in the hash via `Hash#[]` so
    /// that any user-defined `default` / `default_proc` fires; values
    /// are coerced via `to_s`. `owner` is the String Value `subject`
    /// walks (for `$~`).
    pub(crate) fn replace_one_hash(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        subject: &Subject,
        owner: Value,
        hash_val: Value,
    ) -> Result<(RStringInner, bool)> {
        Self::with_coerced_regexp(vm, globals, re_val, |re, vm, globals| {
            let mut region = onigmo_regex::Region::new();
            if !re.find_spans(subject, 0, &mut region)? {
                vm.clear_capture_special_variables();
                return Ok((subject.to_inner(), false));
            }
            let spans = spans_of(&region);
            let (start, end) = spans[0].unwrap();
            save_spans(vm, subject, &spans, owner);
            let key = subject.chunk(None, start..end);
            let rep_inner = lookup_hash_replacement(vm, globals, hash_val, key, subject.mapped())?;
            let res = RStringInner::splice_all(
                &globals.store,
                subject.as_bytes(),
                subject.view_encoding(),
                subject.is_ascii(),
                &[(start..end, rep_inner)],
            )?;
            Ok((res, true))
        })
    }

    /// Replaces all non-overlapping matches in `recv` using hash lookup.
    /// Like `replace_all_block`: matches against a frozen snapshot (no
    /// use-after-free if a hash `default_proc` mutates the receiver) and
    /// raises "string modified" on a live length change after each
    /// lookup, matching CRuby.
    pub(crate) fn replace_all_hash(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        recv: Value,
        hash_val: Value,
    ) -> Result<(RStringInner, bool)> {
        Self::with_coerced_regexp(vm, globals, re_val, |re, vm, globals| {
            if let Some(res) = re.replace_all_hash_plain(vm, globals, re_val, recv, hash_val)? {
                return Ok(res);
            }
            let subject = string_snapshot(recv);
            let tmp = vm.temp_len();
            vm.temp_push(subject);
            let res = re.replace_all_hash_inner(vm, globals, re_val, subject, recv, hash_val);
            vm.temp_clear(tmp);
            res
        })
    }

    /// The Regexp `$~` reports after a replace driven by `self`: `re_val`
    /// itself when it is one, else a fresh Regexp for a pattern that was
    /// coerced from a String.
    fn backref_regexp(&self, re_val: Value) -> Value {
        if re_val.is_regex().is_some() {
            re_val
        } else {
            Value::regexp(self.clone())
        }
    }

    /// `gsub(regex, hash)` when nothing between two matches can run
    /// Ruby: the hash's `default` is still the builtin and it has no
    /// default proc (see [`lookup_hash_replacement`] for what a miss
    /// runs), and every replacement it yields is a String or nil (a
    /// miss answers the stored default, nil unless `Hash.new(x)`). Then
    /// the receiver cannot change under the scan, so it is matched in
    /// place with no snapshot and no per-match length check; one onigmo
    /// `Region` serves every match instead of one allocated and freed
    /// per match; and `$~` is built once, for the last match, which is
    /// all a caller can observe. `None` when the shape does not qualify
    /// (the subject is byte-mapped, or a value would need `to_s`):
    /// nothing observable has happened by then, so the generic path
    /// starts over.
    fn replace_all_hash_plain(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        recv: Value,
        hash_val: Value,
    ) -> Result<Option<(RStringInner, bool)>> {
        let hash = Hashmap::new(hash_val);
        if hash.defalut_proc().is_some()
            || !crate::builtins::hash::hash_default_is_builtin(&globals.store, hash_val.class())
        {
            return Ok(None);
        }
        let miss = hash.defalut_value().unwrap_or_default();
        if !miss.is_nil() && miss.is_rstring_inner().is_none() {
            return Ok(None);
        }
        let recv_inner = recv.as_rstring_inner();
        if recv_inner.needs_byte_mapping() {
            return Ok(None);
        }
        let given: &str = recv_inner.check_utf8()?;
        // The replacement a hash value stands for: nil is the empty
        // string, a String is itself, anything else would need `to_s`
        // (`None`: the generic path's business).
        let inner_of = |value: Value| -> Option<RStringInner> {
            if value.is_nil() {
                Some(RStringInner::from_str_scanned(""))
            } else {
                value.is_rstring_inner().cloned()
            }
        };

        let mut replacements: Vec<(std::ops::Range<usize>, RStringInner)> = vec![];
        // The groups of the last match: the region below is cleared by
        // the search that finds nothing more, so they are copied out.
        let mut last: smallvec::SmallVec<[Option<(usize, usize)>; 2]> = smallvec::SmallVec::new();
        vm.clear_capture_special_variables();

        if let Some(set) = self.single_byte_class()
            && recv_inner.is_ascii_only()
        {
            // A single-byte class over an ASCII-only subject: every
            // match is one byte, found by a table lookup; the engine is
            // not entered at all. One key is looked up per distinct
            // byte — the map cannot change under the scan, so the
            // value is the same for every occurrence.
            let bytes = given.as_bytes();
            let mut seen: smallvec::SmallVec<[(u8, RStringInner); 8]> = smallvec::SmallVec::new();
            for (i, &b) in bytes.iter().enumerate() {
                if set[(b >> 6) as usize] & (1u64 << (b & 63)) == 0 {
                    continue;
                }
                let rep = match seen.iter().find(|(sb, _)| *sb == b) {
                    Some((_, rep)) => rep.clone(),
                    None => {
                        let key = string_substring(recv, i, i + 1);
                        let Some(rep) = inner_of(hash.get(key, vm, globals)?.unwrap_or(miss)) else {
                            return Ok(None);
                        };
                        seen.push((b, rep.clone()));
                        rep
                    }
                };
                replacements.push((i..i + 1, rep));
            }
            if let Some((range, _)) = replacements.last() {
                last.push(Some((range.start, range.end)));
            }
            let is_empty = replacements.is_empty();
            let res = RStringInner::splice_all(
                &globals.store,
                given.as_bytes(),
                recv_inner.encoding(),
                recv_inner.is_ascii_only(),
                &replacements,
            )?;
            if !is_empty {
                vm.set_match_regex(self.backref_regexp(re_val));
                vm.save_capture_spans(&last, given);
            }
            return Ok(Some((res, !is_empty)));
        }

        // Same walk as `replace_repeat`: past a non-empty match, and by
        // one Unicode scalar past an empty one (past EOS to terminate),
        // so the zero-width matches CRuby yields are all seen.
        let engine = self.engine_for(given, Some(given.is_ascii()));
        let mut region = onigmo_regex::Region::new();
        let mut pos = 0usize;
        while pos <= given.len() {
            let found = engine
                .search_with_region(given.as_bytes(), pos, &mut region)
                .map_err(|err| MonorubyErr::regexerr(format!("{err}")))?;
            if found.is_none() {
                break;
            }
            let (start, end) = region.pos(0).unwrap();
            // A String key probes vm-free (`HashRef::get`), so no Ruby
            // runs here either. `hash_val` may be a subclass instance or
            // carry a singleton `[]`: the probe is what CRuby's
            // `rb_hash_aref` does regardless.
            let key = string_substring(recv, start, end);
            let Some(rep) = inner_of(hash.get(key, vm, globals)?.unwrap_or(miss)) else {
                return Ok(None);
            };
            replacements.push((start..end, rep));
            last.clear();
            last.extend((0..region.len()).map(|i| region.pos(i)));
            pos = if end > start {
                end
            } else if start >= given.len() {
                given.len() + 1
            } else {
                let mut next = start + 1;
                while next < given.len() && !given.is_char_boundary(next) {
                    next += 1;
                }
                next
            };
        }

        let is_empty = replacements.is_empty();
        let res = RStringInner::splice_all(
                &globals.store,
                given.as_bytes(),
                recv_inner.encoding(),
                recv_inner.is_ascii_only(),
                &replacements,
            )?;
        if !is_empty {
            vm.set_match_regex(self.backref_regexp(re_val));
            vm.save_capture_spans(&last, given);
        }
        Ok(Some((res, !is_empty)))
    }

    fn replace_all_hash_inner(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        snapshot: Value,
        recv: Value,
        hash_val: Value,
    ) -> Result<(RStringInner, bool)> {
        let inner = snapshot.as_rstring_inner();
        let view;
        let regexp_pattern = re_val.is_regex().is_some();
        let subject = match self.native_subject(inner, &globals.store, regexp_pattern)? {
            Some(subject) => subject,
            None => {
                view = inner.regex_view()?;
                Subject::text(&view, inner.encoding(), inner.needs_byte_mapping())
            }
        };
        let recv_len = recv.as_rstring_inner().len();
        let mut range = vec![];

        vm.clear_capture_special_variables();
        let mut region = onigmo_regex::Region::new();
        let mut pos = 0usize;
        let mut last_match_end: Option<usize> = None;
        while pos <= subject.len() {
            if !self.find_spans(&subject, pos, &mut region)? {
                break;
            }
            let spans = spans_of(&region);
            let (start, end) = spans[0].unwrap();
            if start == end && last_match_end == Some(end) {
                pos = subject.next_char_boundary(pos);
                continue;
            }
            pos = end;
            last_match_end = Some(end);

            let key = subject.chunk(Some(snapshot), start..end);
            vm.set_match_regex(self.backref_regexp(re_val));
            save_spans(vm, &subject, &spans, snapshot);
            let replacement = lookup_hash_replacement(vm, globals, hash_val, key, subject.mapped())?;
            check_string_not_modified(recv, recv_len)?;

            range.push((start..end, replacement));
        }

        let is_empty = range.is_empty();
        let res = RStringInner::splice_all(
            &globals.store,
            subject.as_bytes(),
            subject.view_encoding(),
            subject.is_ascii(),
            &range,
        )?;

        Ok((res, !is_empty))
    }

    /// `byte_pos` is a byte offset into `given`, already converted from
    /// the caller's character position and clamped to a char boundary
    /// (see `String#match`, which does the conversion against the
    /// subject's cached code range).
    pub(crate) fn match_one(
        vm: &mut Executor,
        globals: &mut Globals,
        re: Regexp,
        given: &str,
        block: Option<BlockHandler>,
        byte_pos: usize,
    ) -> Result<Value> {
        // Attach the Regexp to the `$~` this match is about to save, so
        // named-capture lookup (`$~[:name]`) works after `String#match`
        // (same stash `Regexp#match` sets on its own path).
        vm.set_match_regex(re.as_val());
        match re.captures_from_pos(given, byte_pos, vm)? {
            None => Ok(Value::nil()),
            Some(captures) => {
                // `captures_from_pos` has just saved this match as `$~`
                // (with the Regexp attached through the stash above), and
                // that object *is* the result — `str.match(re).equal?($~)`
                // holds in CRuby — so hand it back rather than building
                // a second MatchData with a second haystack view. A
                // builtin always runs inside a Ruby frame, so the svar
                // container the save went to exists.
                let _ = captures;
                let match_data = vm
                    .current_match_data()
                    .expect("`$~` was saved by captures_from_pos");
                if let Some(bh) = block {
                    vm.invoke_block_once(globals, bh, &[match_data])
                } else {
                    Ok(match_data)
                }
            }
        }
    }

    /// `StringScanner` primitive: match `sub` (the byte suffix at the scan
    /// position, already a valid engine view) either anchored at its start
    /// (`onig_match`) or as a forward search (`onig_search`), recording the
    /// registers into the caller's reusable `region`. Returns whether it
    /// matched; the caller reads the offsets (relative to `sub`) from the
    /// region. Never touches `$~`. `ascii` is the subject's cached code
    /// range (7-bit or not) — the scanner calls this once per token on
    /// an ever-shorter suffix, so it must not be probed here.
    pub(crate) fn strscan_match(
        &self,
        sub: &str,
        anchored: bool,
        ascii: bool,
        region: &mut onigmo_regex::Region,
    ) -> Result<bool> {
        let engine = self.engine_for(sub, Some(ascii));
        let r = if anchored {
            engine.match_at_with_region(sub.as_bytes(), 0, region)
        } else {
            engine.search_with_region(sub.as_bytes(), 0, region)
        };
        r.map(|r| r.is_some())
            .map_err(|err| MonorubyErr::regexerr(format!("Capture failed. {:?}", err)))
    }

    /// [`strscan_match`](Self::strscan_match) for a non-UTF-8 subject
    /// with a native Onigmo codec (`enc`, from `onigmo_encoding_for`):
    /// `sub` is the raw byte suffix at the scan position, matched by the
    /// source bytes compiled under that codec, so the registers are raw
    /// byte offsets into `sub`.
    pub(crate) fn strscan_match_bytes(
        &self,
        sub: &[u8],
        anchored: bool,
        enc: OnigmoEncoding,
        region: &mut onigmo_regex::Region,
    ) -> Result<bool> {
        let native = self.native_regex(enc)?;
        let r = if anchored {
            native.match_at_with_region(sub, 0, region)
        } else {
            native.search_with_region(sub, 0, region)
        };
        r.map(|r| r.is_some())
            .map_err(|err| MonorubyErr::regexerr(format!("Capture failed. {:?}", err)))
    }

    /// Byte-oriented twin of [`match_pred`](Self::match_pred): whether the
    /// source bytes compiled under `enc` match `bytes` at or after
    /// `byte_pos`. Does NOT set `$~`.
    pub(crate) fn match_pred_bytes(
        &self,
        bytes: &[u8],
        enc: OnigmoEncoding,
        byte_pos: usize,
    ) -> Result<bool> {
        let native = self.native_regex(enc)?;
        // A predicate needs no capture groups: search without a region.
        native
            .search_bytes(bytes, byte_pos, bytes.len(), None)
            .map(|res| res.is_some())
            .map_err(search_failed)
    }

    /// Like `match_one` but returns only a boolean and does NOT set `$~`.
    pub(crate) fn match_pred(
        re: &RegexpInner,
        given: &str,
        char_pos: usize,
    ) -> Result<bool> {
        // `char_pos == given.chars().count()` (the end-of-string
        // anchor position) is a legal starting point in CRuby —
        // `/\Az/.match?("", 0)` finds the zero-width match at byte
        // 0. `nth` returns `None` for that boundary, so handle it
        // explicitly before falling through.
        let byte_pos = if char_pos == 0 {
            0
        } else {
            match given.char_indices().nth(char_pos) {
                Some((pos, _)) => pos,
                None if char_pos == given.chars().count() => given.len(),
                None => return Ok(false),
            }
        };
        // A predicate needs no capture groups: search without a region,
        // which skips the region allocation and the capture bookkeeping.
        re.engine_for(given, None)
            .search(given, byte_pos, given.len(), None)
            .map(|res| res.is_some())
            .map_err(search_failed)
    }

    /// `String#scan` without a block over `subject` (the walk of
    /// `snapshot`, a frozen copy of the receiver): the matches, or the
    /// groups of each. `$~` is left at the last match.
    pub(crate) fn scan(
        &self,
        vm: &mut Executor,
        snapshot: Value,
        subject: &Subject,
    ) -> Result<Vec<Value>> {
        let mut ary = vec![];
        let mut last: Option<Spans> = None;
        vm.clear_capture_special_variables();
        // Walk the haystack manually rather than via `captures_iter`, which
        // drops zero-width matches the CRuby scan is expected to yield — the
        // empty position between two non-empty matches, and the empty match
        // at end-of-string after a non-empty one (e.g.
        // `"foo".scan(/(?~foo)/) == ["fo", "o", ""]`). Same loop as
        // `replace_repeat`: advance past a non-empty match, and by one
        // character past an empty one (past EOS to terminate).
        let mut region = onigmo_regex::Region::new();
        let mut pos = 0usize;
        while pos <= subject.len() {
            if !self.find_spans(subject, pos, &mut region)? {
                break;
            }
            let spans = spans_of(&region);
            let (start, end) = spans[0].unwrap();
            match spans.len() {
                0 => unreachable!(),
                1 => ary.push(subject.chunk(Some(snapshot), start..end)),
                len => {
                    let mut vec = vec![];
                    for &span in &spans[1..len] {
                        match span {
                            Some((s, e)) => vec.push(subject.chunk(Some(snapshot), s..e)),
                            None => vec.push(Value::nil()),
                        }
                    }
                    ary.push(Value::array_from_vec(vec));
                }
            }
            last = Some(spans);
            pos = Self::next_walk_pos(subject, start, end);
        }

        if let Some(spans) = last {
            save_spans(vm, subject, &spans, snapshot);
        }
        Ok(ary)
    }
}

impl RegexpInner {
    /// Replace all matches for `self` in `subject` with `replace`.
    ///
    /// ### return
    /// `(replaced: RStringInner, is_replaced?: bool)`. The result is in
    /// `subject.view_encoding()` unless a replacement's non-ASCII content
    /// settled another (`splice_all`).
    fn replace_repeat(
        &self,
        vm: &mut Executor,
        store: &Store,
        subject: &Subject,
        owner: Value,
        replace: &RStringInner,
    ) -> Result<(RStringInner, bool)> {
        // Walk the haystack manually rather than relying on
        // `captures_iter`, which can skip the zero-width match that
        // sits between two non-empty matches (e.g.
        // `"¿por qué?".gsub(/([a-z\d]*)/, "*")` — the empty position
        // immediately after `"por"` is observable in CRuby but the
        // bundled iterator collapses it). For empty matches we
        // advance by one character so the loop terminates.
        let mut replacements = vec![];
        vm.clear_capture_special_variables();
        let mut last: Option<Spans> = None;
        let mut region = onigmo_regex::Region::new();
        let mut pos = 0usize;
        while pos <= subject.len() {
            if !self.find_spans(subject, pos, &mut region)? {
                break;
            }
            let spans = spans_of(&region);
            let (start, end) = spans[0].unwrap();
            let (rep, mixed) = self.expand_backref(replace.as_bytes(), subject.as_bytes(), &spans);
            replacements.push((
                start..end,
                expansion_inner(store, &rep, replace, mixed, subject.view_encoding())?,
            ));
            last = Some(spans);
            pos = Self::next_walk_pos(subject, start, end);
        }
        let is_empty = replacements.is_empty();
        // Single forward pass instead of N tail-shifting splices.
        let res = RStringInner::splice_all(
            store,
            subject.as_bytes(),
            subject.view_encoding(),
            subject.is_ascii(),
            &replacements,
        )?;

        if let Some(spans) = last {
            // Attach the (possibly coerced-from-String) Regexp to `$~` so
            // `$~.regexp` works after `gsub(String)`.
            vm.set_match_regex(Value::regexp(self.clone()));
            save_spans(vm, subject, &spans, owner);
        }

        Ok((res, !is_empty))
    }

    /// Expand backreference sequences in `replace` (the raw bytes of the
    /// replacement template, in whatever encoding it carries) using
    /// `captures` against `given` (the original haystack). Recognises:
    ///
    /// - `\0`, `\1`-`\9`: numbered captures (`\0` is the full match).
    /// - `\&`: same as `\0` (full match).
    /// - `` \` ``: pre-match (everything before the match).
    /// - `\'`: post-match (everything after the match).
    /// - `\+`: highest-numbered participating capture.
    /// - `\k<name>`: named capture by `<name>`.
    /// - `\\`: literal backslash.
    /// - Trailing `\` is left as a literal backslash.
    /// - Other `\X` sequences are passed through verbatim.
    ///
    /// Works on bytes so a template with non-ASCII bytes in a
    /// byte-oriented encoding (`"\xff".b`) is spliced as those bytes,
    /// never re-encoded; see [`expansion_inner`] for the encoding the
    /// expansion is tagged with.
    ///
    /// The flag is whether any captured text (as opposed to the template
    /// itself) carried non-ASCII bytes — what decides, in
    /// [`expansion_inner`], whether the two can share an encoding.
    fn expand_backref(
        &self,
        replace: &[u8],
        given: &[u8],
        spans: &[Option<(usize, usize)>],
    ) -> (Vec<u8>, bool) {
        let bytes = replace;
        let mut rep: Vec<u8> = Vec::with_capacity(bytes.len());
        let mut captured_non_ascii = false;
        let mut push_captured = |rep: &mut Vec<u8>, s: &[u8]| {
            captured_non_ascii |= !s.is_ascii();
            rep.extend_from_slice(s);
        };
        let group = |i: usize| -> Option<&[u8]> {
            spans.get(i).copied().flatten().map(|(s, e)| &given[s..e])
        };
        let mut i = 0;
        while i < bytes.len() {
            if bytes[i] != b'\\' {
                // Copy the run up to the next backslash verbatim.
                let run = bytes[i..]
                    .iter()
                    .position(|&b| b == b'\\')
                    .unwrap_or(bytes.len() - i);
                rep.extend_from_slice(&bytes[i..i + run]);
                i += run;
                continue;
            }
            if i + 1 >= bytes.len() {
                // Trailing backslash: copy verbatim (CRuby leaves it).
                rep.push(b'\\');
                i += 1;
                continue;
            }
            let next = bytes[i + 1];
            match next {
                b'0'..=b'9' => {
                    let idx = (next - b'0') as usize;
                    if let Some(m) = group(idx) {
                        push_captured(&mut rep, m);
                    }
                    i += 2;
                }
                b'&' => {
                    if let Some(m) = group(0) {
                        push_captured(&mut rep, m);
                    }
                    i += 2;
                }
                b'`' => {
                    if let Some((start, _)) = spans.first().copied().flatten() {
                        push_captured(&mut rep, &given[..start]);
                    }
                    i += 2;
                }
                b'\'' => {
                    if let Some((_, end)) = spans.first().copied().flatten() {
                        push_captured(&mut rep, &given[end..]);
                    }
                    i += 2;
                }
                b'+' => {
                    // Highest-numbered participating capture *group*
                    // (1..). If the regex has no capture groups —
                    // even when there's a full match — `\+` expands
                    // to the empty string, matching CRuby.
                    let mut idx = spans.len();
                    while idx > 1 {
                        idx -= 1;
                        if let Some(m) = group(idx) {
                            push_captured(&mut rep, m);
                            break;
                        }
                    }
                    i += 2;
                }
                b'\\' => {
                    rep.push(b'\\');
                    i += 2;
                }
                b'k' => {
                    // `\k<name>` — named backreference.
                    if i + 2 < bytes.len() && bytes[i + 2] == b'<' {
                        if let Some(end_off) = bytes[i + 3..].iter().position(|&b| b == b'>') {
                            let name_start = i + 3;
                            let name_end = name_start + end_off;
                            let name = String::from_utf8_lossy(&bytes[name_start..name_end]);
                            // Onigmo allows multiple groups to share
                            // a name; pick the highest-numbered one
                            // that participated, matching CRuby.
                            let members = self.get_group_members(&name);
                            let mut chosen: Option<usize> = None;
                            for &m_idx in members.iter() {
                                if group(m_idx as usize).is_some() {
                                    chosen = Some(m_idx as usize);
                                }
                            }
                            if let Some(idx) = chosen {
                                if let Some(m) = group(idx) {
                                    push_captured(&mut rep, m);
                                }
                            }
                            i = name_end + 1;
                            continue;
                        }
                    }
                    // Malformed `\k…`: copy verbatim.
                    rep.extend_from_slice(b"\\k");
                    i += 2;
                }
                _ => {
                    // Unknown `\X`: keep as-is (preserves e.g. `\d`); the
                    // rest of a multibyte X is copied by the next run.
                    rep.push(b'\\');
                    rep.push(next);
                    i += 2;
                }
            }
        }
        (rep, captured_non_ascii)
    }

    /// Replaces the leftmost-first match for `self` in `subject` with
    /// `replace`.
    fn replace_once(
        &self,
        vm: &mut Executor,
        store: &Store,
        subject: &Subject,
        owner: Value,
        replace: &RStringInner,
    ) -> Result<(RStringInner, bool)> {
        let mut region = onigmo_regex::Region::new();
        if !self.find_spans(subject, 0, &mut region)? {
            vm.clear_capture_special_variables();
            return Ok((subject.to_inner(), false));
        }
        let spans = spans_of(&region);
        let (start, end) = spans[0].unwrap();
        let (rep, mixed) = self.expand_backref(replace.as_bytes(), subject.as_bytes(), &spans);
        let rep_inner = expansion_inner(store, &rep, replace, mixed, subject.view_encoding())?;
        let res = RStringInner::splice_all(
            store,
            subject.as_bytes(),
            subject.view_encoding(),
            subject.is_ascii(),
            &[(start..end, rep_inner)],
        )?;
        save_spans(vm, subject, &spans, owner);
        Ok((res, true))
    }
}

/// Coerce the result of a `String#sub`/`#gsub` block to an
/// `RStringInner`, calling user-defined `to_s` so a mock returning
/// a non-String from `to_s` is exercised. Falls back to monoruby's
/// intrinsic `to_s` rendering when the object's `to_s` doesn't
/// return a String. Returns the `RStringInner` directly so callers
/// don't have to round-trip through `String` and re-classify.
///
/// A String (the result itself, or what its `to_s` answered) is taken
/// as it is — its bytes under its own encoding — so a BINARY value with
/// 8-bit content splices as those bytes and `splice_all` settles the
/// result's encoding from it (#1378). Callers (e.g. `replace_all_block`)
/// layer their own `Encoding::CompatibilityError` checks on top.
fn block_result_to_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    v: Value,
    mapped: bool,
) -> Result<RStringInner> {
    // When the surrounding replace runs in surrogate space (`mapped`),
    // a String result must be forward-mapped so its 8-bit bytes splice
    // as U+00XX characters and survive the caller's final decode.
    let as_replacement = |inner: &RStringInner| {
        if mapped {
            RStringInner::from_string_scanned(map_bytes_to_utf8(inner.as_bytes()))
        } else {
            inner.clone()
        }
    };
    if let Some(inner) = v.is_rstring_inner() {
        return Ok(as_replacement(inner));
    }
    let coerced = vm.invoke_method_inner(globals, IdentId::TO_S, v, &[], None, None)?;
    if let Some(inner) = coerced.is_rstring_inner() {
        Ok(as_replacement(inner))
    } else {
        // Intrinsic fallback produces `String`; pre-classify it
        // so the splice that follows lands on a fast path.
        Ok(RStringInner::from_string_scanned(coerced.to_s(&globals.store)))
    }
}

/// The expanded replacement (`expand_backref`'s bytes) as a string: under
/// the template's encoding when the template carries non-ASCII bytes
/// (`"\xff".b` splices as BINARY, and `splice_all` lets the result take
/// that encoding over a 7-bit haystack, as CRuby's `rb_enc_cr_str_buf_cat`
/// does), else the haystack's, so captured text keeps its own. Non-ASCII
/// captured text (`captured_non_ascii`) pasted into a non-ASCII template
/// of another encoding is CRuby's `rb_reg_regsub` failure:
/// `Encoding::CompatibilityError`, the template's encoding first.
fn expansion_inner(
    store: &Store,
    bytes: &[u8],
    template: &RStringInner,
    captured_non_ascii: bool,
    hay_enc: crate::value::Encoding,
) -> Result<RStringInner> {
    let enc = if template.is_ascii_only() {
        hay_enc
    } else {
        let enc = template.encoding();
        if captured_non_ascii && enc != hay_enc {
            return Err(MonorubyErr::incompatible_encoding(store, enc, hay_enc));
        }
        enc
    };
    Ok(RStringInner::from_encoding_scanned(bytes, enc))
}

/// Look up the replacement string for a `String#sub`/`#gsub` match
/// when the second argument is a `Hash`. Calls `Hash#[]` so that any
/// user-defined `default` / `default_proc` fires; missing keys
/// (where `[]` returns `nil` because no default is set) are replaced
/// with the empty string. Values are coerced via `Object#to_s` per
/// CRuby.
/// Classify a regexp source as a single-byte class: `Some(set)` when
/// the pattern matches exactly one ASCII character from `set`, and
/// nothing else, under `option` (Onigmo's bits). Accepted: a top-level
/// alternation of one-character atoms, or a single such atom, where an
/// atom is a literal character, a `\`-escaped punctuation character,
/// a control escape (`\n` `\t` `\r` `\f` `\v` `\a` `\e`), `\xHH` below
/// 0x80, one of `\d` `\w` `\s` `\h` and their negations (ASCII-only in
/// Ruby), or a bracket class of those plus ranges (`a-z`), optionally
/// negated. The `i` option folds ASCII case. Everything else — a
/// quantifier, a group, an anchor, `.`, a POSIX or nested class, `&&`,
/// `x` mode, a non-ASCII source — is `None`: this is a pure
/// recognizer, never a guess, so the caller's scan stays exact.
///
/// The set is meant for ASCII-only subjects: a negated class also
/// matches non-ASCII characters (as whole characters), which the byte
/// table cannot express, so callers must not use it on any other.
fn single_byte_class_of(src: &[u8], option: u32) -> Option<[u64; 4]> {
    if src.is_empty() || !src.is_ascii() || option & onigmo_regex::ONIG_OPTION_EXTEND != 0 {
        return None;
    }
    let mut set = [0u64; 4];
    let mut i = 0;
    loop {
        i = byte_class_atom(src, i, &mut set)?;
        // A quantifier would make the atom match more than one byte.
        if i < src.len() && matches!(src[i], b'*' | b'+' | b'?' | b'{') {
            return None;
        }
        if i == src.len() {
            break;
        }
        if src[i] != b'|' {
            return None;
        }
        i += 1;
        if i == src.len() {
            // A trailing `|` is an empty alternative: zero-width.
            return None;
        }
    }
    if option & onigmo_regex::ONIG_OPTION_IGNORECASE != 0 {
        for c in b'a'..=b'z' {
            let u = c.to_ascii_uppercase();
            if byte_set_has(&set, c) || byte_set_has(&set, u) {
                byte_set_add(&mut set, c);
                byte_set_add(&mut set, u);
            }
        }
    }
    Some(set)
}

fn byte_set_has(set: &[u64; 4], b: u8) -> bool {
    set[(b >> 6) as usize] & (1u64 << (b & 63)) != 0
}

fn byte_set_add(set: &mut [u64; 4], b: u8) {
    set[(b >> 6) as usize] |= 1u64 << (b & 63);
}

/// One character-or-class item of a pattern, as [`single_byte_class_of`]
/// understands it.
enum ByteClassItem {
    /// One byte.
    Byte(u8),
    /// A predefined class (`\d` and friends), already as a set.
    Set([u64; 4]),
}

/// Parse one top-level atom of `src` at `i` into `set`; the index past
/// it, or `None` for anything that is not a one-byte atom.
fn byte_class_atom(src: &[u8], i: usize, set: &mut [u64; 4]) -> Option<usize> {
    match src[i] {
        b'[' => byte_class_bracket(src, i + 1, set),
        // Groups, quantifiers, anchors, any-char, an empty alternative
        // or a stray `]`.
        b'(' | b')' | b'*' | b'+' | b'?' | b'{' | b'}' | b'^' | b'$' | b'.' | b'|' | b']' => None,
        _ => {
            let (item, next) = byte_class_char(src, i)?;
            match item {
                ByteClassItem::Byte(b) => byte_set_add(set, b),
                ByteClassItem::Set(s) => {
                    for (d, x) in set.iter_mut().zip(s) {
                        *d |= x;
                    }
                }
            }
            Some(next)
        }
    }
}

/// Parse a bracket class whose body starts at `i` (just past the `[`)
/// into `set`; the index past the closing `]`.
fn byte_class_bracket(src: &[u8], mut i: usize, set: &mut [u64; 4]) -> Option<usize> {
    let negate = src.get(i) == Some(&b'^');
    if negate {
        i += 1;
    }
    let mut class = [0u64; 4];
    let mut any = false;
    loop {
        match *src.get(i)? {
            b']' if any => break,
            // An empty class, a nested or POSIX class, an intersection.
            b']' | b'[' => return None,
            b'&' if src.get(i + 1) == Some(&b'&') => return None,
            _ => {}
        }
        let (item, next) = byte_class_char(src, i)?;
        any = true;
        i = next;
        match item {
            ByteClassItem::Set(s) => {
                for (d, x) in class.iter_mut().zip(s) {
                    *d |= x;
                }
            }
            // `a-z`: a range, unless the `-` is the last character
            // before `]` (then it is a literal, added on the next turn).
            ByteClassItem::Byte(lo) if src.get(i) == Some(&b'-') && src.get(i + 1) != Some(&b']') => {
                if lo == b'-' {
                    return None;
                }
                let (hi, next) = byte_class_char(src, i + 1)?;
                let ByteClassItem::Byte(hi) = hi else {
                    return None;
                };
                if lo > hi {
                    return None;
                }
                for b in lo..=hi {
                    byte_set_add(&mut class, b);
                }
                i = next;
            }
            ByteClassItem::Byte(b) => byte_set_add(&mut class, b),
        }
    }
    if negate {
        // Within ASCII: the callers only ever scan ASCII-only subjects.
        class[0] = !class[0];
        class[1] = !class[1];
        class[2] = 0;
        class[3] = 0;
    }
    for (d, x) in set.iter_mut().zip(class) {
        *d |= x;
    }
    Some(i + 1)
}

/// Parse one character (literal or escaped) or predefined class at
/// `i`; `None` for any escape the recognizer does not know.
fn byte_class_char(src: &[u8], i: usize) -> Option<(ByteClassItem, usize)> {
    let c = *src.get(i)?;
    if c != b'\\' {
        return Some((ByteClassItem::Byte(c), i + 1));
    }
    let e = *src.get(i + 1)?;
    let byte = |b: u8| Some((ByteClassItem::Byte(b), i + 2));
    let class = |members: &dyn Fn(u8) -> bool, negate: bool| {
        let mut s = [0u64; 4];
        for b in 0u8..128 {
            if members(b) != negate {
                byte_set_add(&mut s, b);
            }
        }
        Some((ByteClassItem::Set(s), i + 2))
    };
    match e {
        b'n' => byte(b'\n'),
        b't' => byte(b'\t'),
        b'r' => byte(b'\r'),
        b'f' => byte(0x0c),
        b'v' => byte(0x0b),
        b'a' => byte(0x07),
        b'e' => byte(0x1b),
        b'd' | b'D' => class(&|b| b.is_ascii_digit(), e == b'D'),
        b'w' | b'W' => class(&|b| b.is_ascii_alphanumeric() || b == b'_', e == b'W'),
        b's' | b'S' => class(&|b| matches!(b, b' ' | b'\t' | b'\n' | 0x0b | 0x0c | b'\r'), e == b'S'),
        b'h' | b'H' => class(&|b| b.is_ascii_hexdigit(), e == b'H'),
        b'x' => {
            let hex = |k: usize| src.get(i + 2 + k).and_then(|d| (*d as char).to_digit(16));
            let d0 = hex(0)?;
            match hex(1) {
                Some(d1) => {
                    let v = d0 * 16 + d1;
                    (v < 0x80).then(|| (ByteClassItem::Byte(v as u8), i + 4))
                }
                None => Some((ByteClassItem::Byte(d0 as u8), i + 3)),
            }
        }
        // `\b` `\A` `\z` `\p{..}` `\k<..>` `\1` `\cX` `\u...` and the rest.
        _ if e.is_ascii_alphanumeric() => None,
        // An escaped punctuation character stands for itself.
        _ => byte(e),
    }
}

#[cfg(test)]
mod byte_class_tests {
    use super::single_byte_class_of;

    fn members(src: &str, option: u32) -> Option<Vec<u8>> {
        single_byte_class_of(src.as_bytes(), option)
            .map(|set| (0u8..=255).filter(|b| super::byte_set_has(&set, *b)).collect())
    }

    #[test]
    fn accepts_single_byte_classes() {
        assert_eq!(members(">|<|&", 0), Some(b"&<>".to_vec()));
        assert_eq!(members("['&\\\"<>]", 0), Some(b"\"&'<>".to_vec()));
        assert_eq!(members("a", 0), Some(b"a".to_vec()));
        assert_eq!(members("\\.", 0), Some(b".".to_vec()));
        assert_eq!(members("[a-c]", 0), Some(b"abc".to_vec()));
        assert_eq!(members("[-a]", 0), Some(b"-a".to_vec()));
        assert_eq!(members("[a-]", 0), Some(b"-a".to_vec()));
        assert_eq!(members("[\\-a]", 0), Some(b"-a".to_vec()));
        assert_eq!(members("\\d", 0), Some(b"0123456789".to_vec()));
        assert_eq!(members("[\\s]", 0), Some(b"\t\n\x0b\x0c\r ".to_vec()));
        assert_eq!(members("\\x41|\\x2", 0), Some(b"\x02A".to_vec()));
        assert_eq!(members("a\\|b", 0), None);
        let uri = members("[^*\\-.0-9A-Z_a-z]", 0).unwrap();
        assert!(uri.contains(&b'/') && uri.contains(&b' ') && uri.contains(&0));
        assert!(!uri.contains(&b'-') && !uri.contains(&b'*') && !uri.contains(&b'Z'));
        assert!(!uri.contains(&0x80) && !uri.contains(&0xff));
        assert_eq!(
            members("[a-c]", onigmo_regex::ONIG_OPTION_IGNORECASE),
            Some(b"ABCabc".to_vec())
        );
        assert_eq!(members("\\D", 0).map(|v| v.len()), Some(128 - 10));
    }

    #[test]
    fn rejects_anything_else() {
        for src in [
            "", "a+", "a*", "a?", "a{1}", "[ab]+", "ab", "a|bc", "(a)", "(?:a)", ".", "^a", "a$",
            "\\A", "\\ba", "\\1", "\\u0041", "\\p{Alpha}", "[[:alpha:]]", "[a[b]]", "[a&&b]",
            "[]", "[^]", "[z-a]", "a|", "|a", "[--a]", "\\x80", "\\cA", "é", "[é]", "\\M-a",
        ] {
            assert_eq!(members(src, 0), None, "{src:?}");
        }
        assert_eq!(members("a", onigmo_regex::ONIG_OPTION_EXTEND), None);
    }
}

/// The replacement `hash_val` yields for the matched text `key`: CRuby's
/// `rb_hash_aref` followed by `rb_obj_as_string`. `rb_hash_aref` reads
/// the map directly — a redefined `Hash#[]`, on a subclass, a singleton
/// or `Hash` itself, is not consulted — and on a miss runs the hash's
/// `default`: the stored value or the default proc while that is still
/// the builtin (`rb_hash_default_value`), the redefinition otherwise.
fn lookup_hash_replacement(
    vm: &mut Executor,
    globals: &mut Globals,
    hash_val: Value,
    key: Value,
    mapped: bool,
) -> Result<RStringInner> {
    let hash = Hashmap::new(hash_val);
    let v = match hash.get(key, vm, globals)? {
        Some(v) => v,
        None => {
            if crate::builtins::hash::hash_default_is_builtin(&globals.store, hash_val.class()) {
                if let Some(proc) = hash.defalut_proc() {
                    vm.invoke_proc(globals, &proc, &[hash_val, key])?
                } else {
                    hash.defalut_value().unwrap_or_default()
                }
            } else {
                vm.invoke_method_inner(globals, IdentId::DEFAULT, hash_val, &[key], None, None)?
            }
        }
    };
    if v.is_nil() {
        Ok(RStringInner::from_str_scanned(""))
    } else if v.is_rstring_inner().is_some() || v.is_str().is_some() {
        Ok(block_result_to_inner(vm, globals, v, mapped)?)
    } else {
        // CRuby coerces non-String hash values via `Object#to_s`.
        let coerced = vm.invoke_method_inner(globals, IdentId::TO_S, v, &[], None, None)?;
        match coerced.is_rstring_inner() {
            Some(_) => Ok(block_result_to_inner(vm, globals, coerced, mapped)?),
            None => Ok(RStringInner::from_string_scanned(coerced.to_s(&globals.store))),
        }
    }
}

#[test]
fn test_regexp() {
    let re = Regex::new(r#"(?:(?m)\A(?:(?m)/)?\z)"#).unwrap();
    assert!(re.find("").unwrap().is_some());
    assert!(re.find("/").unwrap().is_some());
    assert!(!re.find("a").unwrap().is_some());
}

#[cfg(test)]
mod expand_unicode_braces_tests {
    use super::expand_unicode_braces;

    fn ok(input: &str) -> String {
        expand_unicode_braces(input).expect("expected success")
    }

    #[test]
    fn passthrough_when_no_brace() {
        assert_eq!(ok(""), "");
        assert_eq!(ok("abc"), "abc");
        assert_eq!(ok("\\uFFFF"), "\\uFFFF");
        assert_eq!(ok("\\x{20}"), "\\x{20}");
        assert_eq!(ok("\\n\\t"), "\\n\\t");
    }

    #[test]
    fn bmp_codepoint_is_zero_padded() {
        assert_eq!(ok("\\u{20}"), "\\u0020");
        assert_eq!(ok("\\u{7e}"), "\\u007E");
        assert_eq!(ok("\\u{0041}"), "\\u0041");
        assert_eq!(ok("\\u{FFFF}"), "\\uFFFF");
    }

    #[test]
    fn range_with_braces() {
        assert_eq!(ok("[\\u{20}-\\u{7e}]"), "[\\u0020-\\u007E]");
    }

    #[test]
    fn supplementary_plane_emits_raw_utf8() {
        // U+1F600 😀 => 4-byte UTF-8 sequence
        assert_eq!(ok("\\u{1F600}"), "😀");
    }

    #[test]
    fn multi_codepoint_brace() {
        assert_eq!(ok("\\u{20 7e}"), "\\u0020\\u007E");
        assert_eq!(ok("\\u{41 42 43}"), "\\u0041\\u0042\\u0043");
    }

    #[test]
    fn escaped_backslash_is_preserved() {
        // `\\u{20}` => literal backslash followed by `u{20}` characters
        assert_eq!(ok("\\\\u{20}"), "\\\\u{20}");
    }

    #[test]
    fn mixed_with_surrounding_regex_syntax() {
        assert_eq!(ok("^(\\u{41})+$"), "^(\\u0041)+$");
    }

    #[test]
    fn rejects_invalid_hex() {
        assert!(expand_unicode_braces("\\u{xyz}").is_err());
    }

    #[test]
    fn rejects_empty_braces() {
        assert!(expand_unicode_braces("\\u{}").is_err());
    }

    #[test]
    fn rejects_out_of_range() {
        assert!(expand_unicode_braces("\\u{110000}").is_err());
    }

    #[test]
    fn rejects_a_surrogate() {
        // A surrogate is in the BMP range but names no character, so
        // CRuby refuses it as it refuses one past `U+10FFFF`:
        // `Regexp.new("\\u{D800}")` is `RegexpError: invalid Unicode
        // range`, and a *literal* `/\u{D800}/` does not even parse
        // (#1522). Onigmo accepts the four digits, so the check has to
        // be here.
        assert!(expand_unicode_braces("\\u{D800}").is_err());
        assert!(expand_unicode_braces("\\uD800").is_err());
        assert!(expand_unicode_braces("\\u{DFFF}").is_err());
        assert_eq!(ok("\\u{D7FF}"), "\\uD7FF");
        assert_eq!(ok("\\uE000"), "\\uE000");
    }

    #[test]
    fn preserves_non_ascii_after_backslash() {
        assert_eq!(ok("\\あ"), "\\あ");
    }

    #[test]
    fn unterminated_brace_raises_invalid_unicode_list() {
        // CRuby surfaces a `RegexpError("invalid Unicode list:
        // /<src>/")` for an unterminated `\u{` rather than letting
        // Onigmo see the bytes — matching that here keeps the
        // error message under our control (and consistent with
        // the empty / non-hex / out-of-range cases above).
        assert!(expand_unicode_braces("\\u{20").is_err());
    }
}

#[cfg(test)]
mod regex_cache_tests {
    use super::*;
    use onigmo_regex::OnigmoEncoding;

    // Use a distinctive source per test so we don't depend on whether
    // some other test in the suite already populated REGEX_CACHE for
    // this key — REGEX_CACHE is a process-wide static.
    const SRC_HIT: &str = "regex_cache_tests::cache_hit_returns_same_arc";
    const SRC_MISS_A: &str = "regex_cache_tests::cache_miss_a";
    const SRC_MISS_B: &str = "regex_cache_tests::cache_miss_b";

    #[test]
    fn cache_hit_returns_same_arc() {
        // First call lands in the Vacant arm and inserts an Arc<CachedRegex>;
        // second call lands in the Occupied arm and clones the same
        // Arc back out. Both pass through the cache lookup chain
        // (`REGEX_CACHE.write().unwrap().0.entry(...)`), exercising
        // the `.0` field access on the RegexCache tuple struct.
        let r1 = RegexpInner::with_option_and_encoding(SRC_HIT, 0, OnigmoEncoding::UTF8)
            .expect("first compile");
        let r2 = RegexpInner::with_option_and_encoding(SRC_HIT, 0, OnigmoEncoding::UTF8)
            .expect("second compile");
        assert!(
            Arc::ptr_eq(&r1.regex, &r2.regex),
            "second compile should reuse the cached Arc<CachedRegex>",
        );
    }

    #[test]
    fn cache_miss_for_different_source() {
        // Distinct source strings hash to distinct cache keys, so each
        // gets its own Arc<CachedRegex>.
        let r1 = RegexpInner::with_option_and_encoding(SRC_MISS_A, 0, OnigmoEncoding::UTF8)
            .expect("compile A");
        let r2 = RegexpInner::with_option_and_encoding(SRC_MISS_B, 0, OnigmoEncoding::UTF8)
            .expect("compile B");
        assert!(!Arc::ptr_eq(&r1.regex, &r2.regex));
    }

    #[test]
    fn cache_keyed_on_encoding() {
        // Same source + same option but different encoding → distinct
        // cache entries (the Onigmo regex is encoding-bound).
        let src = "regex_cache_tests::cache_keyed_on_encoding";
        let r_utf8 = RegexpInner::with_option_and_encoding(src, 0, OnigmoEncoding::UTF8)
            .expect("UTF-8 compile");
        let r_ascii = RegexpInner::with_option_and_encoding(src, 0, OnigmoEncoding::ASCII)
            .expect("ASCII compile");
        assert!(!Arc::ptr_eq(&r_utf8.regex, &r_ascii.regex));
    }

    #[test]
    fn ruby_only_option_bits_dont_split_cache() {
        // NOENCODING / FIXEDENCODING / KCODE_* are stripped from the
        // option mask before the cache key is built, so toggling any
        // of them shouldn't produce a new entry.
        let src = "regex_cache_tests::ruby_only_option_bits_dont_split_cache";
        let plain =
            RegexpInner::with_option_and_encoding(src, 0, OnigmoEncoding::UTF8)
                .expect("plain compile");
        let with_ruby_bits = RegexpInner::with_option_and_encoding(
            src,
            RegexpInner::NOENCODING | RegexpInner::FIXEDENCODING,
            OnigmoEncoding::UTF8,
        )
        .expect("compile with Ruby-only bits set");
        assert!(
            Arc::ptr_eq(&plain.regex, &with_ruby_bits.regex),
            "Ruby-only option bits must be masked out of the cache key",
        );
    }

    #[test]
    fn invalid_source_returns_regexerr_with_formatted_source() {
        // The Vacant branch's error path: Onigmo rejects the source
        // (unbalanced bracket here), and we re-format the message with
        // a `: /<source>/` suffix matching CRuby spec expectations.
        let err = RegexpInner::with_option_and_encoding("[", 0, OnigmoEncoding::UTF8)
            .expect_err("unbalanced [ should fail to compile");
        let msg = err.message().to_string();
        assert!(msg.contains('['), "expected source in message, got: {msg}");
    }
}
