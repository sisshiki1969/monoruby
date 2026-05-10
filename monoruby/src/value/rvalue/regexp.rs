use super::*;
use onigmo_regex::{Captures, FindCaptures, OnigmoEncoding, Regex};
use std::sync::Arc;
use std::sync::{LazyLock, RwLock};

static REGEX_CACHE: LazyLock<RwLock<RegexCache>> = LazyLock::new(|| RwLock::new(RegexCache::new()));

#[derive(Debug, Default)]
struct RegexCache(HashMap<(String, u32, OnigmoEncoding), Arc<Regex>>);

impl RegexCache {
    fn new() -> Self {
        Self(HashMap::default())
    }
}

#[monoruby_object]
pub struct Regexp(Value);

#[derive(Clone, Debug)]
pub struct RegexpInner {
    regex: Arc<Regex>,
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
}

impl PartialEq for RegexpInner {
    fn eq(&self, other: &Self) -> bool {
        if Arc::ptr_eq(&self.regex, &other.regex) {
            return true;
        }
        self.as_str() == other.as_str()
            && self.encoding == other.encoding
            && self.declared_encoding == other.declared_encoding
    }
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

    pub fn as_str(&self) -> &str {
        self.regex.as_str()
    }

    pub fn encoding(&self) -> OnigmoEncoding {
        self.encoding
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
        if self.encoding == OnigmoEncoding::ASCII {
            opt |= Self::NOENCODING;
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
        let pre = regex::escape(text);
        let mut out = String::with_capacity(pre.len());
        for ch in pre.chars() {
            match ch {
                ' ' => out.push_str("\\ "),
                '\t' => out.push_str("\\t"),
                '\n' => out.push_str("\\n"),
                '\r' => out.push_str("\\r"),
                '\x0c' => out.push_str("\\f"),
                '\x0b' => out.push_str("\\v"),
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
    reg_str: &str,
    option: u32,
    kcode: Option<u32>,
    source_encoding: Option<crate::value::Encoding>,
) -> (crate::value::Encoding, bool) {
    use crate::value::Encoding;
    let has_non_ascii = reg_str.bytes().any(|b| b >= 0x80);
    if option & RegexpInner::NOENCODING != 0 {
        // `/.../n` (NOENCODING): BINARY when source has non-ASCII
        // bytes, US-ASCII otherwise. CRuby's regex parser only
        // pins to ASCII-8BIT when there's actual binary content
        // — pure-ASCII patterns stay re-tag-free.
        if has_non_ascii {
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
        Some(src) if fixed_flag => (src, true),
        Some(_) => (Encoding::UsAscii, false),
        None if has_non_ascii => (Encoding::Utf8, true),
        None => (Encoding::UsAscii, fixed_flag),
    }
}

/// Scan `src` for a `\u` escape (`\uXXXX` or `\u{...}`) whose
/// codepoint is non-ASCII (>= 0x80). Returns `true` for the first
/// such escape, `false` if none. Used to pin the declared encoding
/// to UTF-8 when the source contains a non-ASCII Unicode escape,
/// matching CRuby's `\u`-fixes-encoding behavior.
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
        let reg_str: String = reg_str.into();
        let (mut declared_encoding, mut fixed_encoding) =
            resolve_declared_encoding(&reg_str, option, kcode, source_encoding);
        // CRuby pins the regex to UTF-8 when the source contains a
        // `\u` escape that decodes to a non-ASCII codepoint, even on
        // an otherwise pure-7-bit pattern (`/\u{1234}/.fixed_encoding?`
        // is `true`). The `n`/`e`/`s` modifiers and the `NOENCODING`
        // flag override this — they leave the explicit kcode intact.
        if option & Self::NOENCODING == 0
            && kcode.map(|k| k & Self::KCODE_UTF8 != 0).unwrap_or(true)
            && has_non_ascii_unicode_escape(&reg_str)
        {
            declared_encoding = crate::value::Encoding::Utf8;
            fixed_encoding = true;
        }
        // Strip Ruby-only bits (`NOENCODING`, `FIXEDENCODING`,
        // `KCODE_*`) before handing the option mask to Onigmo —
        // those bits sit in the same word but Onigmo only understands
        // its own option bits (`IGNORECASE`/`MULTILINE`/`EXTEND`/...).
        let onigmo_option = option
            & !(Self::NOENCODING | Self::FIXEDENCODING | Self::KCODE_MASK);
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
            std::collections::hash_map::Entry::Occupied(entry) => Ok(RegexpInner {
                regex: entry.get().clone(),
                encoding,
                declared_encoding,
                fixed_encoding,
                initialized: true,
            }),
            std::collections::hash_map::Entry::Vacant(entry) => {
                match Regex::new_with_option_and_encoding(&reg_str, onigmo_option, encoding) {
                    Ok(regexp) => {
                        let regex = Arc::new(regexp);
                        entry.insert(regex.clone());
                        Ok(RegexpInner {
                            regex,
                            encoding,
                            declared_encoding,
                            fixed_encoding,
                            initialized: true,
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

    pub fn captures_from_pos<'a>(
        &self,
        given: &'a str,
        pos: usize,
        vm: &mut Executor,
    ) -> Result<Option<Captures<'a>>> {
        match self.regex.captures_from_pos(given, pos) {
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

    pub fn captures_iter<'a>(&self, given: &'a str) -> FindCaptures<'_, 'a> {
        self.regex.captures_iter(given)
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
        let option = self.option();
        let m = option & onigmo_regex::ONIG_OPTION_MULTILINE != 0;
        let i = option & onigmo_regex::ONIG_OPTION_IGNORECASE != 0;
        let x = option & onigmo_regex::ONIG_OPTION_EXTEND != 0;
        format!(
            "(?{}{}{}{}{}{}{}:{})",
            if m { "m" } else { "" },
            if i { "i" } else { "" },
            if x { "x" } else { "" },
            if m && i && x { "" } else { "-" },
            if !m { "m" } else { "" },
            if !i { "i" } else { "" },
            if !x { "x" } else { "" },
            self.as_str()
        )
    }

    pub fn inspect(&self) -> String {
        format!(
            "/{}/{}",
            escape_unescaped_slashes(self.as_str()),
            self.option_string()
        )
    }
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
        let s: &str = if let Some(s) = re_val.is_str() {
            s
        } else {
            s_owned = re_val.coerce_to_str(vm, globals)?;
            &s_owned
        };
        let re = Self::from_escaped(s)?;
        f(&re, vm, globals)
    }

    /// Replaces the leftmost-first match with `replace`.
    pub(crate) fn replace_one(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        replace: &str,
    ) -> Result<(RStringInner, bool)> {
        Self::with_coerced_regexp(vm, globals, re_val, |re, vm, globals| {
            re.replace_once(vm, &globals.store, given, replace)
        })
        .map(|(s, c)| (s, c.is_some()))
    }

    pub(crate) fn replace_one_block(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        bh: BlockHandler,
    ) -> Result<(RStringInner, bool)> {
        Self::with_coerced_regexp(vm, globals, re_val, |re, vm, globals| {
            match re.captures(given, vm)? {
                None => Ok((RStringInner::from_str_scanned(given), false)),
                Some(captures) => {
                    let m = captures.get(0).unwrap();
                    let (start, end, matched_str) = (m.start(), m.end(), m.as_str());
                    let mut res = RStringInner::from_str_scanned(given);
                    let matched = Value::string_from_str(matched_str);
                    let result = vm.invoke_block_once(globals, bh, &[matched])?;
                    let rep_inner = block_result_to_inner(vm, globals, result)?;
                    res.bytesplice_with(start, end - start, &rep_inner, &globals.store)?;
                    Ok((res, true))
                }
            }
        })
    }

    /// Replaces all non-overlapping matches in `given` string with `replace`.
    pub(crate) fn replace_all(
        vm: &mut Executor,
        globals: &mut Globals,
        regexp: Value,
        given: &str,
        replace: &str,
    ) -> Result<(RStringInner, bool)> {
        Self::with_coerced_regexp(vm, globals, regexp, |re, vm, globals| {
            re.replace_repeat(vm, &globals.store, given, replace)
        })
    }

    /// Replaces all non-overlapping matches in `given` string with `replace`.
    pub(crate) fn replace_all_block(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        bh: BlockHandler,
        self_enc: Option<crate::value::Encoding>,
    ) -> Result<(RStringInner, bool)> {
        Self::with_coerced_regexp(vm, globals, re_val, |re, vm, globals| {
            let mut range = vec![];
            let data = vm.get_block_data(globals, bh)?;

            vm.clear_capture_special_variables();
            for cap in re.captures_iter(given) {
                let cap = cap.map_err(|err| MonorubyErr::regexerr(format!("{err}")))?;
                let m = cap.get(0).unwrap();

                let matched_str = m.as_str();
                let matched = Value::string_from_str(matched_str);
                vm.save_capture_special_variables(&cap, given);
                let result = vm.invoke_block(globals, &data, &[matched])?;
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
                let replace = block_result_to_inner(vm, globals, result)?;

                range.push((m.range(), replace));
            }

            let mut res = RStringInner::from_str_scanned(given);
            let is_empty = range.is_empty();

            for (r, rep_inner) in range.into_iter().rev() {
                res.bytesplice_with(r.start, r.end - r.start, &rep_inner, &globals.store)?;
            }

            Ok((res, !is_empty))
        })
    }

    /// Replaces the first match in `given` string using hash lookup.
    /// For each match, the matched text is looked up as a key in the
    /// hash via `Hash#[]` so that any user-defined `default` / `default_proc`
    /// fires; values are coerced via `to_s`.
    pub(crate) fn replace_one_hash(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        hash_val: Value,
    ) -> Result<(RStringInner, bool)> {
        Self::with_coerced_regexp(vm, globals, re_val, |re, vm, globals| {
            match re.captures(given, vm)? {
                None => Ok((RStringInner::from_str_scanned(given), false)),
                Some(captures) => {
                    let m = captures.get(0).unwrap();
                    let (start, end, matched_str) = (m.start(), m.end(), m.as_str());
                    let mut res = RStringInner::from_str_scanned(given);
                    let key = Value::string_from_str(matched_str);
                    let rep_inner = lookup_hash_replacement(vm, globals, hash_val, key)?;
                    res.bytesplice_with(start, end - start, &rep_inner, &globals.store)?;
                    Ok((res, true))
                }
            }
        })
    }

    /// Replaces all non-overlapping matches in `given` string using hash lookup.
    pub(crate) fn replace_all_hash(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        hash_val: Value,
    ) -> Result<(RStringInner, bool)> {
        Self::with_coerced_regexp(vm, globals, re_val, |re, vm, globals| {
            let mut range = vec![];

            vm.clear_capture_special_variables();
            for cap in re.captures_iter(given) {
                let cap = cap.map_err(|err| MonorubyErr::regexerr(format!("{err}")))?;
                let m = cap.get(0).unwrap();

                let matched_str = m.as_str();
                let key = Value::string_from_str(matched_str);
                vm.save_capture_special_variables(&cap, given);
                let replacement = lookup_hash_replacement(vm, globals, hash_val, key)?;

                range.push((m.range(), replacement));
            }

            let mut res = RStringInner::from_str_scanned(given);
            let is_empty = range.is_empty();

            for (r, rep_inner) in range.into_iter().rev() {
                res.bytesplice_with(r.start, r.end - r.start, &rep_inner, &globals.store)?;
            }

            Ok((res, !is_empty))
        })
    }

    pub(crate) fn match_one(
        vm: &mut Executor,
        globals: &mut Globals,
        re: Regexp,
        given: &str,
        block: Option<BlockHandler>,
        char_pos: usize,
    ) -> Result<Value> {
        let byte_pos = match given.char_indices().nth(char_pos) {
            Some((pos, _)) => pos,
            None => return Ok(Value::nil()),
        };
        match re.captures_from_pos(given, byte_pos, vm)? {
            None => Ok(Value::nil()),
            Some(captures) => {
                let match_data = Value::new_matchdata(captures, given, re);
                if let Some(bh) = block {
                    vm.invoke_block_once(globals, bh, &[match_data])
                } else {
                    Ok(match_data)
                }
            }
        }
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
        match re.regex.captures_from_pos(given, byte_pos) {
            Ok(res) => Ok(res.is_some()),
            Err(err) => Err(MonorubyErr::regexerr(format!("Capture failed. {:?}", err))),
        }
    }

    pub(crate) fn scan(&self, vm: &mut Executor, given: &str) -> Result<Vec<Value>> {
        let mut ary = vec![];
        let mut last_captures = None;
        vm.clear_capture_special_variables();
        for cap in self.regex.captures_iter(given) {
            let cap = cap.map_err(|err| MonorubyErr::regexerr(format!("{err}")))?;
            match cap.len() {
                0 => unreachable!(),
                1 => {
                    let val = Value::string(cap.get(0).unwrap().to_string());
                    ary.push(val);
                }
                len => {
                    let mut vec = vec![];
                    for i in 1..len {
                        match cap.get(i) {
                            Some(m) => {
                                vec.push(Value::string(m.to_string()));
                            }
                            None => vec.push(Value::nil()),
                        }
                    }
                    let val = Value::array_from_vec(vec);
                    ary.push(val);
                }
            }
            last_captures = Some(cap);
        }

        if let Some(c) = last_captures {
            vm.save_capture_special_variables(&c, given)
        }
        Ok(ary)
    }
}

impl RegexpInner {
    /// Replace all matches for `self` in `given` string with `replace`.
    ///
    /// ### return
    /// `(replaced: RStringInner, is_replaced?: bool)`. The result
    /// inherits the receiver-side encoding implicit in `given`
    /// (always UTF-8 today, since the caller goes through
    /// `expect_str`); cr is propagated through `bytesplice_with`
    /// rather than re-classified after the fact.
    fn replace_repeat(
        &self,
        vm: &mut Executor,
        store: &Store,
        given: &str,
        replace: &str,
    ) -> Result<(RStringInner, bool)> {
        // Walk the haystack manually rather than relying on
        // `captures_iter`, which can skip the zero-width match that
        // sits between two non-empty matches (e.g.
        // `"¿por qué?".gsub(/([a-z\d]*)/, "*")` — the empty position
        // immediately after `"por"` is observable in CRuby but the
        // bundled iterator collapses it). For empty matches we
        // advance by one Unicode scalar so the loop terminates.
        let mut replacements = vec![];
        vm.clear_capture_special_variables();
        let mut last_captures: Option<Captures> = None;
        let mut pos = 0usize;
        while pos <= given.len() {
            let cap = match self.captures_from_pos(given, pos, vm)? {
                Some(c) => c,
                None => break,
            };
            let m = cap.get(0).unwrap();
            let (start, end) = (m.start(), m.end());
            let rep = self.expand_backref(replace, given, &cap);
            replacements.push((start..end, rep));
            last_captures = Some(cap);
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
        let mut res = RStringInner::from_str_scanned(given);
        let is_empty = replacements.is_empty();
        for (r, rep) in replacements.into_iter().rev() {
            // `r` is a UTF-8 byte range produced by Onigmo, so it
            // sits on character boundaries; `rep` is the
            // backref-expanded replacement, which inherits the
            // splice substring's UTF-8 validity.
            let rep_inner = RStringInner::from_string_scanned(rep);
            res.bytesplice_with(r.start, r.end - r.start, &rep_inner, store)?;
        }

        if let Some(c) = last_captures {
            vm.save_capture_special_variables(&c, given)
        }

        Ok((res, !is_empty))
    }

    /// Expand backreference sequences in `replace` using `captures`
    /// against `given` (the original haystack). Recognises:
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
    fn expand_backref(
        &self,
        replace: &str,
        given: &str,
        captures: &Captures,
    ) -> String {
        let bytes = replace.as_bytes();
        let mut rep = String::new();
        let mut i = 0;
        while i < bytes.len() {
            let ch = bytes[i];
            if ch != b'\\' {
                // copy one UTF-8 scalar
                let len = utf8_char_len(ch);
                rep.push_str(&replace[i..i + len]);
                i += len;
                continue;
            }
            // ch == '\\'
            if i + 1 >= bytes.len() {
                // Trailing backslash: copy verbatim (CRuby leaves it).
                rep.push('\\');
                i += 1;
                continue;
            }
            let next = bytes[i + 1];
            match next {
                b'0'..=b'9' => {
                    let idx = (next - b'0') as usize;
                    if let Some(m) = captures.get(idx) {
                        rep.push_str(m.as_str());
                    }
                    i += 2;
                }
                b'&' => {
                    if let Some(m) = captures.get(0) {
                        rep.push_str(m.as_str());
                    }
                    i += 2;
                }
                b'`' => {
                    if let Some(m) = captures.get(0) {
                        rep.push_str(&given[..m.start()]);
                    }
                    i += 2;
                }
                b'\'' => {
                    if let Some(m) = captures.get(0) {
                        rep.push_str(&given[m.end()..]);
                    }
                    i += 2;
                }
                b'+' => {
                    // Highest-numbered participating capture *group*
                    // (1..). If the regex has no capture groups —
                    // even when there's a full match — `\+` expands
                    // to the empty string, matching CRuby.
                    let mut idx = captures.len();
                    while idx > 1 {
                        idx -= 1;
                        if let Some(m) = captures.get(idx) {
                            rep.push_str(m.as_str());
                            break;
                        }
                    }
                    i += 2;
                }
                b'\\' => {
                    rep.push('\\');
                    i += 2;
                }
                b'k' => {
                    // `\k<name>` — named backreference.
                    if i + 2 < bytes.len() && bytes[i + 2] == b'<' {
                        if let Some(end_off) = bytes[i + 3..].iter().position(|&b| b == b'>') {
                            let name_start = i + 3;
                            let name_end = name_start + end_off;
                            let name = &replace[name_start..name_end];
                            // Onigmo stores capture names; look up the
                            // rightmost group with this name (CRuby
                            // chooses the last participating one).
                            // Onigmo allows multiple groups to share
                            // a name; pick the highest-numbered one
                            // that participated, matching CRuby.
                            let members = self.get_group_members(name);
                            let mut chosen: Option<usize> = None;
                            for &m_idx in members.iter() {
                                if let Some(m) = captures.get(m_idx as usize) {
                                    let _ = m;
                                    chosen = Some(m_idx as usize);
                                }
                            }
                            if let Some(idx) = chosen {
                                if let Some(m) = captures.get(idx) {
                                    rep.push_str(m.as_str());
                                }
                            }
                            i = name_end + 1;
                            continue;
                        }
                    }
                    // Malformed `\k…`: copy verbatim.
                    rep.push('\\');
                    rep.push('k');
                    i += 2;
                }
                _ => {
                    // Unknown `\X`: keep as-is (preserves e.g. `\d`).
                    let len = utf8_char_len(next);
                    rep.push('\\');
                    rep.push_str(&replace[i + 1..i + 1 + len]);
                    i += 1 + len;
                }
            }
        }
        rep
    }

    /// Replaces the leftmost-first match for `self` in `given` string with `replace`.
    ///
    /// `(replaced: RStringInner, captures)`. See `replace_repeat`
    /// for why we build the result via `bytesplice_with` rather
    /// than `String::replace_range`.
    fn replace_once<'a>(
        &self,
        vm: &mut Executor,
        store: &Store,
        given: &'a str,
        replace: &str,
    ) -> Result<(RStringInner, Option<Captures<'a>>)> {
        match self.captures(given, vm)? {
            None => Ok((RStringInner::from_str_scanned(given), None)),
            Some(captures) => {
                let mut res = RStringInner::from_str_scanned(given);
                let m = captures.get(0).unwrap();
                let rep = self.expand_backref(replace, given, &captures);
                let rep_inner = RStringInner::from_string_scanned(rep);
                res.bytesplice_with(m.start(), m.end() - m.start(), &rep_inner, store)?;
                Ok((res, Some(captures)))
            }
        }
    }
}

/// Coerce the result of a `String#sub`/`#gsub` block to an
/// `RStringInner`, calling user-defined `to_s` so a mock returning
/// a non-String from `to_s` is exercised. Falls back to monoruby's
/// intrinsic `to_s` rendering when the object's `to_s` doesn't
/// return a String. Returns the `RStringInner` directly so callers
/// don't have to round-trip through `String` and re-classify.
///
/// Result encoding mirrors `block_result_to_string`'s pre-existing
/// behaviour: `is_str()` validates the bytes are UTF-8 and surfaces
/// `ArgumentError: invalid byte sequence in UTF-8` for non-UTF-8
/// receivers. Callers (e.g. `replace_all_block`) layer their own
/// `Encoding::CompatibilityError` checks on top.
fn block_result_to_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    v: Value,
) -> Result<RStringInner> {
    if let Some(s) = v.is_str() {
        return Ok(RStringInner::from_str_scanned(s));
    }
    let coerced = vm.invoke_method_inner(globals, IdentId::TO_S, v, &[], None, None)?;
    if let Some(s) = coerced.is_str() {
        Ok(RStringInner::from_str_scanned(s))
    } else {
        // Intrinsic fallback produces `String`; pre-classify it
        // so the splice that follows lands on a fast path.
        Ok(RStringInner::from_string_scanned(coerced.to_s(&globals.store)))
    }
}

/// Look up the replacement string for a `String#sub`/`#gsub` match
/// when the second argument is a `Hash`. Calls `Hash#[]` so that any
/// user-defined `default` / `default_proc` fires; missing keys
/// (where `[]` returns `nil` because no default is set) are replaced
/// with the empty string. Values are coerced via `Object#to_s` per
/// CRuby.
fn lookup_hash_replacement(
    vm: &mut Executor,
    globals: &mut Globals,
    hash: Value,
    key: Value,
) -> Result<RStringInner> {
    let v =
        vm.invoke_method_inner(globals, IdentId::_INDEX, hash, &[key], None, None)?;
    if v.is_nil() {
        Ok(RStringInner::from_str_scanned(""))
    } else if let Some(s) = v.is_str() {
        Ok(RStringInner::from_str_scanned(s))
    } else {
        // CRuby coerces non-String hash values via `Object#to_s`.
        let coerced = vm.invoke_method_inner(globals, IdentId::TO_S, v, &[], None, None)?;
        match coerced.is_str() {
            Some(s) => Ok(RStringInner::from_str_scanned(s)),
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
    fn bmp_surrogate_passes_through_as_four_digits() {
        // Surrogate values are in the BMP range; we emit them as \uHHHH and
        // leave validation to Onigmo (same behavior as CRuby for regex literals).
        assert_eq!(ok("\\u{D800}"), "\\uD800");
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
        // First call lands in the Vacant arm and inserts an Arc<Regex>;
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
            "second compile should reuse the cached Arc<Regex>",
        );
    }

    #[test]
    fn cache_miss_for_different_source() {
        // Distinct source strings hash to distinct cache keys, so each
        // gets its own Arc<Regex>.
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
