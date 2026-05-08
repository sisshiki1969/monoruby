use super::*;
use smallvec::SmallVec;
use std::cell::Cell;
use std::cmp::Ordering;

pub mod pack;
mod printable;

#[monoruby_object]
pub struct RString(Value);

/// Iterator yielding one character's worth of bytes per call,
/// honouring the declared encoding. For UTF-8, walks valid UTF-8
/// scalars; broken byte sequences advance one byte at a time so the
/// iterator always terminates. Non-UTF-8 encodings use the
/// fixed-code-unit width (1 for Ascii8/UsAscii/Iso8859, 2 for
/// UTF-16, 4 for UTF-32) — multibyte ASCII-compatible families
/// (EUC-JP, Shift_JIS) currently iterate byte-wise.
pub struct CharByteIter<'a> {
    bytes: &'a [u8],
    pos: usize,
    encoding: Encoding,
}

impl<'a> Iterator for CharByteIter<'a> {
    type Item = &'a [u8];

    fn next(&mut self) -> Option<&'a [u8]> {
        if self.pos >= self.bytes.len() {
            return None;
        }
        let width = match self.encoding {
            Encoding::Ascii8
            | Encoding::UsAscii
            | Encoding::Iso8859(_)
            | Encoding::EucJp
            | Encoding::Sjis(_) => 1,
            Encoding::Utf16Le | Encoding::Utf16Be => 2,
            Encoding::Utf32Le | Encoding::Utf32Be => 4,
            Encoding::Utf8 => {
                let b = self.bytes[self.pos];
                if b < 0x80 {
                    1
                } else if b < 0xC0 {
                    1 // continuation byte at scalar boundary => broken; consume one
                } else {
                    let needed = if b < 0xE0 {
                        2
                    } else if b < 0xF0 {
                        3
                    } else {
                        4
                    };
                    let end = (self.pos + needed).min(self.bytes.len());
                    // Validate the candidate scalar; on failure, fall
                    // back to one byte (CRuby's "adds 1 for every
                    // invalid byte in UTF-8" rule).
                    if end - self.pos == needed
                        && std::str::from_utf8(&self.bytes[self.pos..end]).is_ok()
                    {
                        needed
                    } else {
                        1
                    }
                }
            }
        };
        let end = (self.pos + width).min(self.bytes.len());
        let slice = &self.bytes[self.pos..end];
        self.pos = end;
        Some(slice)
    }
}

/// Cached classification of an `RStringInner`'s byte content relative
/// to its declared `encoding`. CRuby's `enum ruby_coderange_type`
/// equivalent — keeps `valid_encoding?` / `ascii_only?` /
/// compatibility checks O(1) after the first walk.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CodeRange {
    /// Not yet computed.
    Unknown,
    /// Every byte is < 0x80; safe under any ASCII-compatible encoding.
    SevenBit,
    /// Encoding-valid (and contains at least one non-ASCII codepoint).
    Valid,
    /// Contains a byte sequence invalid in the declared encoding.
    Broken,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Encoding {
    /// Binary / ASCII-8BIT.
    Ascii8,
    /// UTF-8.
    Utf8,
    /// US-ASCII (a 7-bit subset of UTF-8 for storage purposes).
    UsAscii,
    /// UTF-16 little-endian.
    Utf16Le,
    /// UTF-16 big-endian.
    Utf16Be,
    /// UTF-32 little-endian.
    Utf32Le,
    /// UTF-32 big-endian.
    Utf32Be,
    /// ISO-8859-N for N in 1..=16 (excluding 12). One byte per char,
    /// every byte is "valid" in the encoding.
    Iso8859(u8),
    /// EUC-JP. ASCII-compatible multibyte.
    EucJp,
    /// Shift_JIS / Windows-31J / CP932 (we treat these as one
    /// implementation, but the canonical name is preserved). The
    /// `u8` distinguishes the alias for `name()`/`==`.
    Sjis(u8),
}

impl Encoding {
    /// True if the encoding is a strict superset of US-ASCII for
    /// 7-bit bytes — every byte 0..0x80 represents the same ASCII
    /// character regardless of the encoding. UTF-16 / UTF-32 are
    /// not (their code units are 2 / 4 bytes).
    pub fn is_ascii_compatible(self) -> bool {
        !matches!(
            self,
            Self::Utf16Le | Self::Utf16Be | Self::Utf32Le | Self::Utf32Be
        )
    }

    /// True if the encoding pairs with US-ASCII / UTF-8 such that
    /// existing UTF-8-internal pipelines (`expect_str`,
    /// `String::chars`, regex calls) can run without modification.
    pub fn is_utf8_compatible(self) -> bool {
        matches!(self, Encoding::Utf8 | Encoding::UsAscii)
    }

    /// True if monoruby has no native byte→character decoder for
    /// this encoding and the bytes are stored opaquely. Used to
    /// route operations down the binary-style path.
    pub fn is_dummy(self) -> bool {
        !matches!(self, Encoding::Ascii8 | Encoding::Utf8 | Encoding::UsAscii)
    }

    /// Canonical CRuby-facing name. Matches the constant-name suffix
    /// after `Encoding::`, except `_` is rendered as `-` for the
    /// hyphenated forms users see in `Encoding#to_s`.
    pub fn name(self) -> &'static str {
        match self {
            Encoding::Ascii8 => "ASCII-8BIT",
            Encoding::Utf8 => "UTF-8",
            Encoding::UsAscii => "US-ASCII",
            Encoding::Utf16Le => "UTF-16LE",
            Encoding::Utf16Be => "UTF-16BE",
            Encoding::Utf32Le => "UTF-32LE",
            Encoding::Utf32Be => "UTF-32BE",
            Encoding::Iso8859(1) => "ISO-8859-1",
            Encoding::Iso8859(2) => "ISO-8859-2",
            Encoding::Iso8859(3) => "ISO-8859-3",
            Encoding::Iso8859(4) => "ISO-8859-4",
            Encoding::Iso8859(5) => "ISO-8859-5",
            Encoding::Iso8859(6) => "ISO-8859-6",
            Encoding::Iso8859(7) => "ISO-8859-7",
            Encoding::Iso8859(8) => "ISO-8859-8",
            Encoding::Iso8859(9) => "ISO-8859-9",
            Encoding::Iso8859(10) => "ISO-8859-10",
            Encoding::Iso8859(11) => "ISO-8859-11",
            Encoding::Iso8859(13) => "ISO-8859-13",
            Encoding::Iso8859(14) => "ISO-8859-14",
            Encoding::Iso8859(15) => "ISO-8859-15",
            Encoding::Iso8859(16) => "ISO-8859-16",
            Encoding::Iso8859(_) => "ISO-8859-1",
            Encoding::EucJp => "EUC-JP",
            // 0 = canonical Shift_JIS, 1 = Windows-31J / CP932.
            Encoding::Sjis(0) => "Shift_JIS",
            Encoding::Sjis(_) => "Windows-31J",
        }
    }

    /// Inspect form used by `Encoding#inspect` /
    /// `RStringInner::str_encoding`'s i-var label.
    pub fn inspect_label(self) -> String {
        match self {
            Encoding::Ascii8 => "BINARY (ASCII-8BIT)".to_string(),
            other => other.name().to_string(),
        }
    }

    /// Walk `bytes` and classify them into a `CodeRange` for *this*
    /// encoding. Used to populate the `cr` cache lazily.
    pub fn classify(self, bytes: &[u8]) -> CodeRange {
        if bytes.is_empty() {
            return CodeRange::SevenBit;
        }
        // Quick path: bytes that are all ASCII are SevenBit under any
        // ASCII-compatible encoding (UTF-8/US-ASCII/EUC-JP/SJIS/
        // ISO-8859-*). UTF-16/32 don't qualify even for "abc" (since
        // their code units are 2/4 bytes), so skip the fast path.
        if self.is_ascii_compatible() && bytes.iter().all(|b| *b < 0x80) {
            return CodeRange::SevenBit;
        }
        match self {
            Encoding::UsAscii => {
                // US-ASCII permits *only* 7-bit bytes; the SevenBit
                // fast path above handles the all-ASCII case, so
                // anything reaching here has at least one high byte
                // and is therefore Broken.
                CodeRange::Broken
            }
            Encoding::Utf8 => match std::str::from_utf8(bytes) {
                Ok(_) => CodeRange::Valid,
                Err(_) => CodeRange::Broken,
            },
            Encoding::Ascii8 => CodeRange::Valid, // every byte is "valid"
            Encoding::Iso8859(_) => CodeRange::Valid, // every byte 0..256 represents a glyph
            // For encodings we don't decode natively, treat any
            // sequence as Valid unless its byte count contradicts
            // the code-unit width.
            Encoding::Utf16Le | Encoding::Utf16Be => {
                if bytes.len() % 2 == 0 {
                    CodeRange::Valid
                } else {
                    CodeRange::Broken
                }
            }
            Encoding::Utf32Le | Encoding::Utf32Be => {
                if bytes.len() % 4 == 0 {
                    CodeRange::Valid
                } else {
                    CodeRange::Broken
                }
            }
            Encoding::EucJp | Encoding::Sjis(_) => CodeRange::Valid,
        }
    }

    /// Best-effort encoding-name parser. Recognises every name in
    /// `init_encoding`'s constant table; unknown names raise
    /// `ArgumentError` matching CRuby.
    pub fn try_from_str(s: &str) -> Result<Self> {
        // Normalize: uppercase, replace '-' / '.' with '_'.
        let normalized = s.to_uppercase().replace('-', "_").replace('.', "_");
        match normalized.as_str() {
            "UTF_8" | "UTF8" | "CP65001" => Ok(Encoding::Utf8),
            "ASCII_8BIT" | "BINARY" => Ok(Encoding::Ascii8),
            "US_ASCII" | "ASCII" | "ANSI_X3_4_1968" | "646" => Ok(Encoding::UsAscii),
            "LOCALE" | "EXTERNAL" | "FILESYSTEM" => Ok(Encoding::Utf8),

            "UTF_16" | "UTF_16LE" => Ok(Encoding::Utf16Le),
            "UTF_16BE" => Ok(Encoding::Utf16Be),
            "UTF_32" | "UTF_32LE" => Ok(Encoding::Utf32Le),
            "UTF_32BE" => Ok(Encoding::Utf32Be),

            "ISO_8859_1" | "ISO8859_1" | "LATIN1" => Ok(Encoding::Iso8859(1)),
            "ISO_8859_2" | "ISO8859_2" | "LATIN2" => Ok(Encoding::Iso8859(2)),
            "ISO_8859_3" | "ISO8859_3" | "LATIN3" => Ok(Encoding::Iso8859(3)),
            "ISO_8859_4" | "ISO8859_4" | "LATIN4" => Ok(Encoding::Iso8859(4)),
            "ISO_8859_5" | "ISO8859_5" => Ok(Encoding::Iso8859(5)),
            "ISO_8859_6" | "ISO8859_6" => Ok(Encoding::Iso8859(6)),
            "ISO_8859_7" | "ISO8859_7" => Ok(Encoding::Iso8859(7)),
            "ISO_8859_8" | "ISO8859_8" => Ok(Encoding::Iso8859(8)),
            "ISO_8859_9" | "ISO8859_9" | "LATIN5" => Ok(Encoding::Iso8859(9)),
            "ISO_8859_10" | "ISO8859_10" | "LATIN6" => Ok(Encoding::Iso8859(10)),
            "ISO_8859_11" | "ISO8859_11" => Ok(Encoding::Iso8859(11)),
            "ISO_8859_13" | "ISO8859_13" | "LATIN7" => Ok(Encoding::Iso8859(13)),
            "ISO_8859_14" | "ISO8859_14" | "LATIN8" => Ok(Encoding::Iso8859(14)),
            "ISO_8859_15" | "ISO8859_15" | "LATIN9" => Ok(Encoding::Iso8859(15)),
            "ISO_8859_16" | "ISO8859_16" | "LATIN10" => Ok(Encoding::Iso8859(16)),

            "EUC_JP" | "EUCJP" | "EUCJP_MS" | "EUCJP_WIN" | "CP51932" | "STATELESS_ISO_2022_JP" => {
                Ok(Encoding::EucJp)
            }
            "SHIFT_JIS" | "SJIS" | "MACJAPANESE" | "MACJAPAN" | "ISO_2022_JP" | "ISO2022_JP" => {
                Ok(Encoding::Sjis(0))
            }
            "WINDOWS_31J" | "CP932" | "CSWINDOWS31J" | "WINDOWS31J" => Ok(Encoding::Sjis(1)),

            // Other byte-oriented encodings without native support
            // are stored as ASCII-8BIT but the name isn't preserved
            // (consistent with monoruby's prior behaviour). Includes
            // dummy encodings that Ruby exposes by name for round-
            // trip purposes (UTF-7, Emacs-Mule, CP50220/CP50221).
            "WINDOWS_1250" | "CP1250" | "WINDOWS_1251" | "CP1251" | "WINDOWS_1252" | "CP1252"
            | "WINDOWS_1253" | "CP1253" | "WINDOWS_1254" | "CP1254" | "WINDOWS_1255" | "CP1255"
            | "WINDOWS_1256" | "CP1256" | "WINDOWS_1257" | "CP1257" | "WINDOWS_1258" | "CP1258"
            | "IBM437" | "CP437" | "IBM737" | "CP737" | "IBM775" | "CP775" | "IBM850" | "CP850"
            | "IBM852" | "CP852" | "IBM855" | "CP855" | "IBM857" | "CP857" | "IBM860" | "CP860"
            | "IBM861" | "CP861" | "IBM862" | "CP862" | "IBM863" | "CP863" | "IBM864" | "CP864"
            | "IBM865" | "CP865" | "IBM866" | "CP866" | "IBM869" | "CP869" | "KOI8_R"
            | "KOI8_U" | "GB2312" | "EUC_CN" | "GBK" | "CP936" | "GB18030" | "BIG5"
            | "BIG5_HKSCS" | "BIG5_UAO" | "EUC_KR" | "EUCKR" | "CP949" | "EUC_TW" | "EUCTW"
            | "TIS_620" | "TIS620" | "CESU_8" | "CESU8" | "UTF_7" | "EMACS_MULE" | "CP50220"
            | "CP50221" | "GB12345" | "MACCYRILLIC" | "MACGREEK" | "MACICELAND" | "MACROMAN"
            | "MACROMANIA" | "MACTHAI" | "MACTURKISH" | "MACUKRAINE" => Ok(Encoding::Ascii8),

            _ => Err(MonorubyErr::argumenterr(format!(
                "unknown encoding name - {s}"
            ))),
        }
    }

    /// CRuby's `Encoding.compatible?(a, b)` algorithm for two
    /// strings (ignoring Symbols / nil / Regexp inputs handled by
    /// the caller). Returns the result encoding, or `None` if the
    /// pair is incompatible.
    pub fn compatible(
        a_enc: Encoding,
        a_cr: CodeRange,
        b_enc: Encoding,
        b_cr: CodeRange,
    ) -> Option<Encoding> {
        if a_enc == b_enc {
            return Some(a_enc);
        }
        // CRuby's `rb_enc_compatible` order:
        //
        // 1. Both sides ASCII-compatible AND both 7-bit → the *first*
        //    encoding (left side wins) — `compatible?("abc",
        //    "def".encode("US-ASCII")) == Encoding::UTF_8`.
        // 2. Both ASCII-compatible, exactly one 7-bit → the
        //    non-7-bit side (whoever has actual non-ASCII content
        //    keeps its encoding).
        // 3. Otherwise → incompatible.
        let a_ascii = a_enc.is_ascii_compatible();
        let b_ascii = b_enc.is_ascii_compatible();
        let a_seven = matches!(a_cr, CodeRange::SevenBit);
        let b_seven = matches!(b_cr, CodeRange::SevenBit);
        if a_ascii && b_ascii {
            if a_seven && b_seven {
                return Some(a_enc);
            }
            if a_seven {
                return Some(b_enc);
            }
            if b_seven {
                return Some(a_enc);
            }
        }
        None
    }
}

///
/// Ruby-level String.
///
/// This struct is used to represent a Ruby-level String.
/// `content` is an opaque byte buffer; the declared `ty` (encoding)
/// is informational only — invalid byte sequences for the declared
/// encoding are tolerated (e.g. `"\xff".force_encoding("UTF-8")`).
/// `cr` caches the result of walking `content` against `ty` so that
/// `valid_encoding?` / `ascii_only?` / encoding-compatibility checks
/// don't re-scan on every call.
///
#[derive(Debug, Clone, Eq)]
pub struct RStringInner {
    content: SmallVec<[u8; STRING_INLINE_CAP]>,
    ty: Encoding,
    cr: Cell<CodeRange>,
}

impl std::ops::Deref for RStringInner {
    type Target = [u8];
    fn deref(&self) -> &Self::Target {
        &self.content
    }
}

impl std::cmp::PartialEq for RStringInner {
    fn eq(&self, other: &Self) -> bool {
        self.content == other.content
    }
}

impl std::hash::Hash for RStringInner {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.content.hash(state);
    }
}

impl std::cmp::PartialOrd<Self> for RStringInner {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let lhs = self.as_bytes();
        let rhs = other.as_bytes();
        let ord = if lhs.len() >= rhs.len() {
            for (r, l) in rhs.iter().zip(lhs.iter()) {
                match l.cmp(r) {
                    Ordering::Equal => {}
                    ord => return Some(ord),
                }
            }
            if lhs.len() == rhs.len() {
                Ordering::Equal
            } else {
                Ordering::Greater
            }
        } else {
            for (l, r) in lhs.iter().zip(rhs.iter()) {
                match l.cmp(r) {
                    Ordering::Equal => {}
                    ord => return Some(ord),
                }
            }
            Ordering::Less
        };
        Some(ord)
    }
}

impl std::cmp::Ord for RStringInner {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl RStringInner {
    pub fn to_str(&self) -> Result<std::borrow::Cow<'_, str>> {
        if self.ty.is_utf8_compatible() {
            match std::str::from_utf8(self) {
                Ok(s) => Ok(std::borrow::Cow::Borrowed(s)),
                Err(err) => Err(MonorubyErr::runtimeerr(format!(
                    "invalid byte sequence: {} {}",
                    err,
                    String::from_utf8_lossy(&self.content)
                ))),
            }
        } else {
            // Binary / non-UTF-8 encodings (Ascii8, UTF-16/32,
            // ISO-8859, EUC-JP, Shift_JIS, …): render byte-wise
            // with `\xHH` for high bytes.
            let mut res = String::new();
            for c in self.as_bytes() {
                if c.is_ascii() {
                    res.push(*c as char);
                } else {
                    res += &format!(r#"\x{:0>2X}"#, c);
                }
            }
            Ok(std::borrow::Cow::Owned(res))
        }
    }

    pub fn dump(&self) -> String {
        if self.ty.is_utf8_compatible() {
            let mut res = String::with_capacity(self.len());
            utf8_dump_with_lookahead(&mut res, self.as_bytes());
            res
        } else {
            let mut res = String::with_capacity(self.len());
            let bytes = self.as_bytes();
            for (i, c) in bytes.iter().enumerate() {
                let next = bytes.get(i + 1).copied().unwrap_or(0);
                if *c == b'#' && matches!(next, b'$' | b'@' | b'{') {
                    res.push_str("\\#");
                } else {
                    ascii_escape(&mut res, *c);
                }
            }
            res
        }
    }

    pub fn inspect(&self) -> String {
        match self.ty {
            Encoding::Utf8 => {
                let mut res = String::with_capacity(self.len());
                utf8_inspect_with_lookahead(&mut res, self.as_bytes(), true);
                res
            }
            Encoding::UsAscii => {
                let mut res = String::with_capacity(self.len());
                utf8_inspect_with_lookahead(&mut res, self.as_bytes(), false);
                res
            }
            _ => {
                let mut res = String::with_capacity(self.len());
                for c in self.as_bytes() {
                    ascii_escape(&mut res, *c);
                }
                res
            }
        }
    }

    pub fn valid(&self) -> bool {
        self.is_valid_encoding()
    }
}

/// True iff byte position `pos` falls on a UTF-8 character boundary
/// inside `bytes`. Assumes the surrounding bytes are valid UTF-8 — under
/// that invariant, a non-continuation byte (top two bits != 10) is
/// always the start of a new codepoint.
#[inline]
fn is_utf8_char_boundary(bytes: &[u8], pos: usize) -> bool {
    pos == 0 || pos == bytes.len() || (bytes[pos] & 0xC0) != 0x80
}

fn utf8_escape(s: &mut String, ch: char) {
    let cp = ch as u32;
    if cp < 0x80 {
        // ASCII bytes are escaped per the standard `\\xNN` /
        // named-escape rules. Letting `ascii_escape` handle this
        // keeps `dump` and `inspect` aligned for plain ASCII.
        ascii_escape(s, ch as u8);
    } else if cp <= 0xFFFF {
        // BMP non-ASCII: 4-digit `\uNNNN` form.
        s.push_str(&format!("\\u{:0>4X}", cp));
    } else {
        // Supplementary planes: brace form, variable width.
        s.push_str(&format!("\\u{{{:X}}}", cp));
    }
}

/// Render a single character for `String#inspect`. `next_ch` is the
/// following character (or `'\0'` if there is none); used to escape
/// `#` when followed by `$`, `@`, or `{`. `is_utf8` switches the
/// escape style for ASCII control characters: UTF-8 strings use the
/// `\uNNNN` form, while US-ASCII / binary strings use `\xNN`.
fn utf8_inspect_with_next(s: &mut String, ch: char, next_ch: char, is_utf8: bool) {
    if ch.is_ascii() {
        let b = ch as u8;
        match b {
            b'"' => s.push_str("\\\""),
            b'\\' => s.push_str("\\\\"),
            b'\t' => s.push_str("\\t"),
            b'\n' => s.push_str("\\n"),
            b'\r' => s.push_str("\\r"),
            b'\x0c' => s.push_str("\\f"),
            b'\x08' => s.push_str("\\b"),
            b'\x07' => s.push_str("\\a"),
            b'\x1b' => s.push_str("\\e"),
            b'\x0b' => s.push_str("\\v"),
            b'#' if matches!(next_ch, '$' | '@' | '{') => s.push_str("\\#"),
            c if c.is_ascii_graphic() || c == b' ' => s.push(c as char),
            _ => {
                if is_utf8 {
                    s.push_str(&format!("\\u{:0>4X}", b));
                } else {
                    s.push_str(&format!("\\x{:0>2X}", b));
                }
            }
        }
    } else if printable::is_printable(ch) {
        s.push(ch);
    } else {
        let cp = ch as u32;
        // CRuby prefers the 4-digit `\uNNNN` form for BMP codepoints
        // and only falls back to `\u{N}` once the codepoint exceeds
        // 0xFFFF (`"\u{1F600}".inspect` → `"\\u{1F600}"`).
        if cp <= 0xFFFF {
            s.push_str(&format!("\\u{:0>4X}", cp));
        } else {
            s.push_str(&format!("\\u{{{:X}}}", cp));
        }
    }
}

/// Like `utf8_escape_bytes(..., utf8_inspect)` but threads the next
/// character through so `utf8_inspect_with_next` can decide whether to
/// escape `#` as `\#` (when followed by `$`, `@`, `{`).
fn utf8_inspect_with_lookahead(res: &mut String, bytes: &[u8], is_utf8: bool) {
    let mut i = 0;
    while i < bytes.len() {
        match std::str::from_utf8(&bytes[i..]) {
            Ok(valid) => {
                let chars: Vec<char> = valid.chars().collect();
                for (idx, &c) in chars.iter().enumerate() {
                    let next_ch = chars.get(idx + 1).copied().unwrap_or('\0');
                    utf8_inspect_with_next(res, c, next_ch, is_utf8);
                }
                break;
            }
            Err(e) => {
                let valid_up_to = e.valid_up_to();
                if valid_up_to > 0 {
                    let valid = std::str::from_utf8(&bytes[i..i + valid_up_to]).unwrap_or("");
                    let chars: Vec<char> = valid.chars().collect();
                    for (idx, &c) in chars.iter().enumerate() {
                        let next_ch = chars.get(idx + 1).copied().unwrap_or('\0');
                        utf8_inspect_with_next(res, c, next_ch, is_utf8);
                    }
                    i += valid_up_to;
                }
                let bad_len = e.error_len().unwrap_or(bytes.len() - i);
                for &b in &bytes[i..i + bad_len] {
                    res.push_str(&format!("\\x{:0>2X}", b));
                }
                i += bad_len;
            }
        }
    }
}

fn ascii_escape(s: &mut String, ch: u8) {
    let str = match ch {
        b'"' => "\\\"",
        b'\\' => "\\\\",
        c if c.is_ascii_graphic() => {
            s.push(c as char);
            return;
        }
        b' ' => " ",
        b'\t' => "\\t",
        b'\x0b' => "\\v",
        b'\n' => "\\n",
        b'\r' => "\\r",
        b'\x0c' => "\\f",
        b'\x08' => "\\b",
        b'\x07' => "\\a",
        b'\x1b' => "\\e",
        _ => {
            s.push_str(&format!("\\x{:0>2X}", ch));
            return;
        }
    };
    s.push_str(str);
}

/// `String#dump` with one-character lookahead so `#` can be escaped
/// to `\#` when the next char is `$` / `@` / `{` (CRuby keeps the
/// dumped form parseable as a Ruby literal — those trigrams would
/// otherwise re-trigger interpolation when parsed back).
fn utf8_dump_with_lookahead(res: &mut String, bytes: &[u8]) {
    let mut i = 0;
    while i < bytes.len() {
        match std::str::from_utf8(&bytes[i..]) {
            Ok(valid) => {
                let chars: Vec<char> = valid.chars().collect();
                for (idx, &c) in chars.iter().enumerate() {
                    let next_ch = chars.get(idx + 1).copied().unwrap_or('\0');
                    utf8_dump_one(res, c, next_ch);
                }
                break;
            }
            Err(err) => {
                let valid_up_to = err.valid_up_to();
                if valid_up_to > 0 {
                    let valid_str =
                        unsafe { std::str::from_utf8_unchecked(&bytes[i..i + valid_up_to]) };
                    let chars: Vec<char> = valid_str.chars().collect();
                    for (idx, &c) in chars.iter().enumerate() {
                        let next_ch = chars.get(idx + 1).copied().unwrap_or('\0');
                        utf8_dump_one(res, c, next_ch);
                    }
                }
                let error_len = err.error_len().unwrap_or(bytes.len() - i - valid_up_to);
                for b in &bytes[i + valid_up_to..i + valid_up_to + error_len] {
                    res.push_str(&format!("\\x{:0>2X}", b));
                }
                i += valid_up_to + error_len;
            }
        }
    }
}

fn utf8_dump_one(s: &mut String, ch: char, next_ch: char) {
    if ch == '#' && matches!(next_ch, '$' | '@' | '{') {
        s.push_str("\\#");
        return;
    }
    utf8_escape(s, ch);
}

impl RStringInner {
    /// Low-level constructor. Callers pass the `cr` they want to
    /// record — either `Unknown` (defer classification to the first
    /// query) or a value computed up front (caller has already done
    /// the scan). Keeping `cr` explicit at this layer means every
    /// public constructor states its scanning behaviour in one place.
    fn from(content: SmallVec<[u8; STRING_INLINE_CAP]>, ty: Encoding, cr: CodeRange) -> Self {
        RStringInner {
            content,
            ty,
            cr: Cell::new(cr),
        }
    }

    pub fn encoding(&self) -> Encoding {
        self.ty
    }

    /// Set the declared encoding tag. Resets the cached code range
    /// since the same bytes may now classify differently
    /// (`"abc".force_encoding("UTF-16LE")` was SevenBit under
    /// UTF-8 but has odd byte count under UTF-16LE).
    pub fn set_encoding(&mut self, ty: Encoding) {
        self.ty = ty;
        self.cr.set(CodeRange::Unknown);
    }

    /// Returns the cached code range, computing it on first call.
    pub fn code_range(&self) -> CodeRange {
        match self.cr.get() {
            CodeRange::Unknown => {
                let cr = self.ty.classify(&self.content);
                self.cr.set(cr);
                cr
            }
            other => other,
        }
    }

    pub fn is_ascii_only(&self) -> bool {
        matches!(self.code_range(), CodeRange::SevenBit)
    }

    pub fn is_valid_encoding(&self) -> bool {
        matches!(self.code_range(), CodeRange::SevenBit | CodeRange::Valid)
    }

    /// Number of characters under the declared encoding. Always
    /// succeeds — broken byte sequences fall back to byte counting
    /// (matches CRuby's `String#length` returning byte length on
    /// invalid UTF-8).
    pub fn char_length(&self) -> usize {
        match self.ty {
            // Fixed 1-byte-per-char.
            Encoding::Ascii8 | Encoding::UsAscii | Encoding::Iso8859(_) => self.content.len(),
            // UTF-16 / UTF-32 with one extra unit per broken trailing
            // byte. CRuby's `String#length` for these reports the
            // number of *complete* code units plus one per stray byte
            // ("adds 1 (and not 2) for a incomplete surrogate in
            // UTF-16").
            Encoding::Utf16Le | Encoding::Utf16Be => {
                let len = self.content.len();
                len / 2 + len % 2
            }
            Encoding::Utf32Le | Encoding::Utf32Be => {
                let len = self.content.len();
                len / 4 + (len % 4 > 0) as usize
            }
            // UTF-8: count valid scalars and count each invalid byte
            // individually (CRuby's "adds 1 for every invalid byte
            // in UTF-8" rule). Use the cached cr instead of re-running
            // from_utf8 -- a SevenBit string can answer in O(1).
            Encoding::Utf8 => match self.code_range() {
                CodeRange::SevenBit => self.content.len(),
                CodeRange::Valid => self.content.iter().filter(|&&b| (b & 0xC0) != 0x80).count(),
                CodeRange::Broken => self.iter_char_bytes().count(),
                // code_range() always populates `cr` to a concrete
                // variant before returning; Unknown is unreachable.
                CodeRange::Unknown => unreachable!(),
            },
            // EUC-JP / Shift_JIS: byte count (no native multibyte
            // decoder yet).
            Encoding::EucJp | Encoding::Sjis(_) => self.content.len(),
        }
    }

    /// Iterate the string's characters as byte-slice views into
    /// `self`. The yielded slices reference `self.content`. Used by
    /// `String#chars` / `#each_char` / `#slice` to be encoding-aware
    /// without converting through a `&str`.
    pub fn iter_char_bytes(&self) -> CharByteIter<'_> {
        CharByteIter {
            bytes: &self.content,
            pos: 0,
            encoding: self.ty,
        }
    }

    /// Negotiate the result encoding for an operation that combines
    /// `self` and `other` (string concatenation, replacement, …).
    /// Returns `None` if the two encodings are not compatible —
    /// callers should raise `Encoding::CompatibilityError`.
    pub fn compatible_encoding(&self, other: &Self) -> Option<Encoding> {
        // CRuby's `rb_enc_compatible` empty-side rules:
        //   - both empty → self's encoding (left wins).
        //   - empty self, 7-bit other AND self is ASCII-compatible
        //     → self's encoding (`"".encode("UTF-8") << "hello".force_encoding(BINARY)"`
        //     keeps UTF-8).
        //   - otherwise (one side empty) → the non-empty side's
        //     encoding wins (matters for ASCII-incompatible enc on
        //     the empty side, e.g.
        //     `"".force_encoding("UTF-16LE") + "abc"` → UTF-8).
        if self.content.is_empty() {
            if other.content.is_empty() {
                return Some(self.encoding());
            }
            if matches!(other.code_range(), CodeRange::SevenBit)
                && self.encoding().is_ascii_compatible()
            {
                return Some(self.encoding());
            }
            return Some(other.encoding());
        }
        if other.content.is_empty() {
            if matches!(self.code_range(), CodeRange::SevenBit)
                && other.encoding().is_ascii_compatible()
            {
                return Some(other.encoding());
            }
            return Some(self.encoding());
        }
        Encoding::compatible(
            self.encoding(),
            self.code_range(),
            other.encoding(),
            other.code_range(),
        )
    }

    pub fn check_utf8(&self) -> Result<&str> {
        // Skip the O(n) re-validation when we already know the answer:
        //
        //   - SevenBit: every byte is < 0x80, which is a strict subset
        //     of valid UTF-8, regardless of the declared encoding.
        //   - Utf8 + Valid: classify ran from_utf8 and accepted the
        //     bytes. Other encodings can also classify as Valid (e.g.
        //     EUC-JP / ASCII-8BIT) without being valid UTF-8, so we
        //     still re-check those.
        let cr = self.code_range();
        if matches!(cr, CodeRange::SevenBit)
            || (self.ty == Encoding::Utf8 && matches!(cr, CodeRange::Valid))
        {
            // SAFETY: see above.
            return Ok(unsafe { std::str::from_utf8_unchecked(&self.content) });
        }
        match std::str::from_utf8(self) {
            Ok(s) => Ok(s),
            Err(_) => Err(MonorubyErr::argumenterr("invalid byte sequence in UTF-8")),
        }
    }

    /// O(1): build a UTF-8 string from `&str` without scanning.
    /// `&str` guarantees valid UTF-8 bytes, but the resulting string
    /// starts with `cr = Unknown` — the first operation that needs
    /// the code range (e.g. `is_ascii_only?`, `check_utf8`,
    /// encoding compatibility) will lazy-classify. Use this for
    /// ephemeral strings where the cr may never be queried; pay the
    /// scan only when something needs it.
    pub fn from_str(s: &str) -> Self {
        RStringInner::from(
            SmallVec::from_slice(s.as_bytes()),
            Encoding::Utf8,
            CodeRange::Unknown,
        )
    }

    /// O(N): build a UTF-8 string and pre-scan it so `cr` is set to
    /// `SevenBit` (all bytes < 0x80) or `Valid`. Use for long-lived
    /// strings whose `cr` will be queried often or that are templates
    /// for many `deep_copy` clones — paying the scan once up front
    /// amortises across every later cr-needing operation.
    pub fn from_str_scanned(s: &str) -> Self {
        let cr = if s.is_ascii() {
            CodeRange::SevenBit
        } else {
            CodeRange::Valid
        };
        RStringInner::from(SmallVec::from_slice(s.as_bytes()), Encoding::Utf8, cr)
    }

    /// O(1) variant of `from_str_scanned` that consumes a `String`.
    /// See `from_str` for the distinction between scanning and lazy
    /// variants.
    pub fn from_string(s: String) -> Self {
        RStringInner::from(
            SmallVec::from_vec(s.into_bytes()),
            Encoding::Utf8,
            CodeRange::Unknown,
        )
    }

    /// O(N) variant of `from_string` that pre-scans for SevenBit.
    pub fn from_string_scanned(s: String) -> Self {
        let cr = if s.is_ascii() {
            CodeRange::SevenBit
        } else {
            CodeRange::Valid
        };
        RStringInner::from(SmallVec::from_vec(s.into_bytes()), Encoding::Utf8, cr)
    }

    pub fn bytes(slice: &[u8]) -> Self {
        RStringInner::from(
            SmallVec::from_slice(slice),
            Encoding::Ascii8,
            CodeRange::Unknown,
        )
    }

    pub fn bytes_from_vec(vec: Vec<u8>) -> Self {
        RStringInner::from(
            SmallVec::from_vec(vec),
            Encoding::Ascii8,
            CodeRange::Unknown,
        )
    }

    /// O(1): build a string with the given encoding without
    /// scanning. `cr = Unknown`; lazy-classify on first need.
    pub fn from_encoding(slice: &[u8], encoding: Encoding) -> Self {
        RStringInner::from(SmallVec::from_slice(slice), encoding, CodeRange::Unknown)
    }

    /// O(N): build a string with the given encoding and pre-classify
    /// `cr` via `Encoding::classify`. Useful for source-byte
    /// literals (`"\xFF"`) and other long-lived inputs where the
    /// classification will outlive many clones.
    pub fn from_encoding_scanned(slice: &[u8], encoding: Encoding) -> Self {
        RStringInner::from(
            SmallVec::from_slice(slice),
            encoding,
            encoding.classify(slice),
        )
    }

    /// Build a substring of `parent` covering the byte range
    /// `start..end`, inheriting the parent's encoding and propagating
    /// the cached code range when the parent's classification implies
    /// the child's. Avoids the O(N) re-classification that
    /// `String#match` and friends would otherwise pay on every
    /// byteslice of an already-classified haystack.
    pub fn from_substring(parent: &RStringInner, start: usize, end: usize) -> Self {
        RStringInner::from(
            SmallVec::from_slice(&parent.content[start..end]),
            parent.encoding(),
            Self::propagated_cr(parent, start, end),
        )
    }

    /// Determine the child code range for a `start..end` byte-range
    /// view of `parent`. Returns `Unknown` when the parent's state
    /// doesn't imply the child's (e.g. UTF-8 + Valid where the cut
    /// could land in the middle of a multi-byte sequence, or
    /// encodings without a native decoder).
    fn propagated_cr(parent: &RStringInner, start: usize, end: usize) -> CodeRange {
        // Empty slice: trivially SevenBit (matches `classify` for
        // every encoding).
        if start == end {
            return CodeRange::SevenBit;
        }
        match parent.code_range() {
            // SevenBit: every byte is < 0x80, so any byte-range view
            // is still SevenBit, regardless of encoding or character
            // boundaries.
            CodeRange::SevenBit => CodeRange::SevenBit,
            CodeRange::Valid => match parent.encoding() {
                // Single-byte encodings: every byte position is a
                // character boundary; Valid trivially propagates.
                Encoding::Ascii8 | Encoding::Iso8859(_) => CodeRange::Valid,
                // UTF-8: the cut points must land on character
                // boundaries (a non-continuation byte or one-past-the-
                // end). When both endpoints align, the byte sequence
                // between them is still valid UTF-8.
                Encoding::Utf8 => {
                    if Self::is_utf8_char_boundary(parent, start)
                        && Self::is_utf8_char_boundary(parent, end)
                    {
                        CodeRange::Valid
                    } else {
                        CodeRange::Unknown
                    }
                }
                // UTF-16/UTF-32: code-unit width is fixed at 2/4
                // bytes. A byte slice that starts and ends on a code-
                // unit boundary is still well-formed under the same
                // "byte count parity" rule that `classify` uses.
                Encoding::Utf16Le | Encoding::Utf16Be => {
                    if start % 2 == 0 && end % 2 == 0 {
                        CodeRange::Valid
                    } else {
                        CodeRange::Unknown
                    }
                }
                Encoding::Utf32Le | Encoding::Utf32Be => {
                    if start % 4 == 0 && end % 4 == 0 {
                        CodeRange::Valid
                    } else {
                        CodeRange::Unknown
                    }
                }
                // No native multibyte decoder; defer to lazy
                // re-classify on first use.
                Encoding::UsAscii | Encoding::EucJp | Encoding::Sjis(_) => CodeRange::Unknown,
            },
            // Broken parents are never safe to propagate — a sub-
            // range could be Valid (if the broken bytes are outside
            // it) or still Broken.
            CodeRange::Broken | CodeRange::Unknown => CodeRange::Unknown,
        }
    }

    /// Whether byte index `i` is a UTF-8 character boundary. `0` and
    /// `content.len()` are always boundaries; otherwise the byte at
    /// `i` must not be a continuation byte (top two bits != `10`).
    fn is_utf8_char_boundary(parent: &RStringInner, i: usize) -> bool {
        i == 0 || i == parent.content.len() || (parent.content[i] & 0xC0) != 0x80
    }

    /// O(N): build a string from arbitrary bytes with auto-detected
    /// encoding (UTF-8 if valid, else ASCII-8BIT) and pre-classified
    /// `cr`. Always scans — encoding detection requires walking the
    /// bytes, so we fold the SevenBit/Valid classification into the
    /// same pass.
    pub fn from_vec_scanned(vec: Vec<u8>) -> Self {
        let (enc, cr) = if vec.iter().all(|&b| b < 0x80) {
            (Encoding::Utf8, CodeRange::SevenBit)
        } else if std::str::from_utf8(&vec).is_ok() {
            (Encoding::Utf8, CodeRange::Valid)
        } else {
            // Ascii8 — every byte is "valid" under the binary tag.
            (Encoding::Ascii8, CodeRange::Valid)
        };
        RStringInner::from(SmallVec::from_vec(vec), enc, cr)
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.content
    }

    pub fn set_byte(&mut self, index: usize, byte: u8) {
        self.content[index] = byte;
        // Phase 1 lets a UTF-8-tagged buffer hold invalid bytes,
        // so we no longer downgrade the encoding tag here.
        //
        // Most setbyte calls poke ASCII bytes into already-SevenBit
        // strings; we can preserve the cached classification for that
        // common case to avoid an O(N) re-classify on the next
        // `valid_encoding?` / encoding-compat check. Anything else
        // falls back to lazy re-classify.
        let new_cr = match (self.cr.get(), byte) {
            (CodeRange::SevenBit, b) if b < 0x80 => CodeRange::SevenBit,
            _ => CodeRange::Unknown,
        };
        self.cr.set(new_cr);
    }

    ///
    /// Convert `char_pos` to the true position in char of the string `self`.
    ///
    /// Return None if `i` is out of range.
    ///
    pub fn conv_char_index(&self, char_pos: i64) -> Option<usize> {
        let len = self.char_length();
        if char_pos >= 0 {
            if char_pos <= len as i64 {
                Some(char_pos as usize)
            } else {
                None
            }
        } else {
            match len as i64 + char_pos {
                n if n < 0 => None,
                n => Some(n as usize),
            }
        }
    }

    ///
    /// Convert `char_pos` to the true position in char of the string `self`.
    ///
    /// Return None if `i` is negative.
    ///
    pub fn conv_char_index2(&self, char_pos: i64) -> Option<usize> {
        let len = self.char_length();
        if len == 0 && char_pos == -1 {
            return Some(0);
        }
        if char_pos >= 0 {
            Some(char_pos as usize)
        } else {
            match len as i64 + char_pos {
                n if n < 0 => None,
                n => Some(n as usize),
            }
        }
    }

    pub fn byte_to_char_index(&self, byte_pos: usize) -> Result<usize> {
        for (i, (pos, _)) in self.check_utf8()?.char_indices().enumerate() {
            match pos {
                pos if pos == byte_pos => {
                    return Ok(i);
                }
                pos if pos > byte_pos => {
                    return Err(MonorubyErr::runtimeerr(format!(
                        "invalid byte position: {byte_pos}"
                    )));
                }
                _ => {}
            }
        }
        Err(MonorubyErr::runtimeerr(format!(
            "invalid byte position: {byte_pos}"
        )))
    }

    pub fn get_range(&self, index: usize, len: usize) -> std::ops::Range<usize> {
        // Walk the encoding-aware char iterator, accumulating the
        // byte offsets of chars `index` through `index+len`. Falls
        // back to byte indexing for fixed-1-byte encodings (which
        // is what `iter_char_bytes` would do anyway, but we
        // shortcut it for performance).
        let unit = match self.ty {
            Encoding::Ascii8 | Encoding::UsAscii | Encoding::Iso8859(_) => Some(1),
            Encoding::Utf16Le | Encoding::Utf16Be => Some(2),
            Encoding::Utf32Le | Encoding::Utf32Be => Some(4),
            // EUC-JP / Shift_JIS currently iterate byte-wise too, so
            // a fixed-1-byte shortcut is fine.
            Encoding::EucJp | Encoding::Sjis(_) => Some(1),
            Encoding::Utf8 => None,
        };
        if let Some(u) = unit {
            let total = self.content.len();
            let start_byte = (index * u).min(total);
            let end_byte = ((index + len) * u).min(total);
            return start_byte..end_byte;
        }
        // UTF-8 path: walk per scalar (or per broken byte) so
        // `String#[char_index]` indexes by *characters*, not bytes.
        let mut start_byte: Option<usize> = None;
        let mut end_byte = self.content.len();
        let mut byte_pos = 0usize;
        for (i, c) in self.iter_char_bytes().enumerate() {
            if i == index {
                start_byte = Some(byte_pos);
                if len == 0 {
                    end_byte = byte_pos;
                    break;
                }
            }
            byte_pos += c.len();
            if start_byte.is_some() && i + 1 == index + len {
                end_byte = byte_pos;
                break;
            }
        }
        match start_byte {
            // `index` past the end but exactly equal to char count
            // is the "append point" — empty range at end.
            None if index == self.char_length() => self.content.len()..self.content.len(),
            None => 0..0,
            Some(s) => s..end_byte,
        }
    }

    /// Append `other`'s bytes to `self`, raising
    /// `Encoding::CompatibilityError` (rather than `RuntimeError`)
    /// on incompatible encodings. Used by call sites that have
    /// access to `Store` — preferred for new code; the older
    /// `extend()` is kept for the call sites that don't.
    pub fn extend(&mut self, other: &Self, store: &Store) -> Result<()> {
        if other.is_empty() {
            return Ok(());
        }
        let result_enc = self
            .compatible_encoding(other)
            .ok_or_else(|| MonorubyErr::incompatible_encoding(store, self.ty, other.ty))?;
        // Snapshot (possibly cached) classifications BEFORE extending the
        // buffer and fold them into a combined cr in O(1). Previously every
        // append wiped self.cr to Unknown, which forced the *next*
        // extend's compatible_encoding() call to re-classify the
        // entire growing buffer -- turning N appends from O(N) into O(N²).
        // (See String#<< micro-bench: a 100k-iter `s << "abcde"` loop took
        // ~5.7s before this change vs 5.6ms in CRuby.)
        let new_cr = match (self.cr.get(), other.cr.get()) {
            (CodeRange::SevenBit, CodeRange::SevenBit) => CodeRange::SevenBit,
            (CodeRange::SevenBit | CodeRange::Valid, CodeRange::SevenBit | CodeRange::Valid) => {
                // compatible_encoding already verified the encodings agree,
                // so two well-formed pieces concatenate to a well-formed
                // whole. (Multi-byte boundaries can only collide at the
                // junction if one side was already Broken.)
                CodeRange::Valid
            }
            // Either side is Broken or its cr was never computed --
            // fall back to lazy re-classify on the next access.
            _ => CodeRange::Unknown,
        };
        self.content.extend_from_slice(&other.content);
        self.ty = result_enc;
        self.cr.set(new_cr);
        Ok(())
    }

    pub fn extend_from_slice_checked(&mut self, slice: &[u8]) -> Result<()> {
        if self.ty.is_utf8_compatible() && std::str::from_utf8(slice).is_err() {
            return Err(MonorubyErr::runtimeerr(format!(
                "invalid byte sequence: {:?}",
                slice
            )));
        }
        // We just verified `slice` validates under self.ty when
        // utf8-compatible, so its cr is at worst Valid; if every byte is
        // < 0x80 it's still SevenBit. Combine with self.cr in O(1) so
        // repeated callers (e.g. String#<< with single-char Integer args
        // or per-codepoint encode loops) don't degenerate to O(N²) when
        // self.code_range() is later queried.
        let slice_cr = if slice.iter().all(|&b| b < 0x80) {
            CodeRange::SevenBit
        } else if self.ty.is_utf8_compatible() {
            CodeRange::Valid
        } else {
            CodeRange::Unknown
        };
        let new_cr = match (self.cr.get(), slice_cr) {
            (CodeRange::SevenBit, CodeRange::SevenBit) => CodeRange::SevenBit,
            (CodeRange::SevenBit | CodeRange::Valid, CodeRange::SevenBit | CodeRange::Valid) => {
                CodeRange::Valid
            }
            _ => CodeRange::Unknown,
        };
        self.content.extend_from_slice(slice);
        self.cr.set(new_cr);
        Ok(())
    }

    /// Append raw bytes without any encoding validation. Used by
    /// `String#append_as_bytes` where deliberately producing
    /// "broken" sequences in the receiver's encoding is permitted.
    pub fn extend_from_slice_no_validate(&mut self, slice: &[u8]) {
        self.content.extend_from_slice(slice);
        self.cr.set(CodeRange::Unknown);
    }

    pub fn repeat(&self, len: usize) -> RStringInner {
        // Repetition of a classified buffer trivially preserves cr:
        // SevenBit/Valid stay so under concatenation of identical
        // valid runs, and Broken stays Broken. The empty case
        // (`len == 0`) collapses to SevenBit, matching `classify(b"")`.
        let cr = if len == 0 {
            CodeRange::SevenBit
        } else {
            self.cr.get()
        };
        RStringInner::from(SmallVec::from_vec(self.content.repeat(len)), self.ty, cr)
    }

    /// Mutate `self.content` in place: replace bytes `start..end` with
    /// `replacement`. Encoding tag and `cr` are *not* touched — the
    /// caller is responsible for re-classifying or otherwise updating
    /// them. The shared low-level shuffling used by `bytesplice_with`.
    fn splice_bytes(&mut self, start: usize, end: usize, replacement: &[u8]) {
        debug_assert!(start <= end);
        debug_assert!(end <= self.content.len());
        let len = end - start;
        let new_len = self.content.len() - len + replacement.len();
        if replacement.len() > len {
            // Need to grow: extend first, then shift the tail right.
            let extra = replacement.len() - len;
            self.content.resize(new_len, 0);
            self.content.copy_within(end..new_len - extra, end + extra);
        } else if replacement.len() < len {
            // Shrink: shift the tail left, then truncate.
            let shrink = len - replacement.len();
            let old_len = self.content.len();
            self.content.copy_within(end..old_len, end - shrink);
            self.content.truncate(new_len);
        }
        self.content[start..start + replacement.len()].copy_from_slice(replacement);
    }

    /// Replace byte range `start..start+len` with the bytes of
    /// `replacement`, taking the replacement's encoding and cached
    /// code range into account. Returns
    /// `Encoding::CompatibilityError` (rather than ignoring the
    /// mismatch the way `bytesplice(&[u8])` does) when the receiver
    /// and `replacement` carry incompatible encodings.
    ///
    /// The cached `code_range()` of `replacement` lets us short-cut
    /// the post-mutation classification: when both sides are
    /// SevenBit/Valid and the splice falls on UTF-8 character
    /// boundaries we set the result cr in O(1) without re-walking
    /// the whole buffer. Broken results on UTF-8-compatible
    /// encodings demote to ASCII-8BIT, matching CRuby and
    /// `bytesplice`.
    pub fn bytesplice_with(
        &mut self,
        start: usize,
        len: usize,
        replacement: &RStringInner,
        store: &Store,
    ) -> Result<()> {
        let result_enc = self
            .compatible_encoding(replacement)
            .ok_or_else(|| MonorubyErr::incompatible_encoding(store, self.ty, replacement.ty))?;

        let end = start + len;
        let prev_cr = self.cr.get();
        let prev_ty = self.ty;
        let repl_cr = replacement.code_range();
        let repl_bytes = replacement.as_bytes();

        // Test BEFORE we mutate: if the receiver is valid UTF-8 and
        // both splice endpoints fall on character boundaries, the
        // prefix-free property of UTF-8 lets the receiver-side stay
        // valid as long as the replacement is itself valid (which
        // its cached cr tells us in O(1)).
        let utf8_boundaries_ok = matches!(prev_ty, Encoding::Utf8)
            && matches!(prev_cr, CodeRange::Valid | CodeRange::SevenBit)
            && is_utf8_char_boundary(&self.content, start)
            && is_utf8_char_boundary(&self.content, end);

        self.splice_bytes(start, end, repl_bytes);
        self.ty = result_enc;

        // Fast path 1: SevenBit + ASCII-compatible encoding +
        // ASCII-only replacement (its cr already says SevenBit).
        if matches!(prev_cr, CodeRange::SevenBit)
            && matches!(repl_cr, CodeRange::SevenBit)
            && result_enc.is_ascii_compatible()
        {
            self.cr.set(CodeRange::SevenBit);
            return Ok(());
        }
        // Fast path 2: receiver and replacement are both well-formed
        // UTF-8 (or pure ASCII) and the splice respects character
        // boundaries — the result is well-formed UTF-8 too.
        if utf8_boundaries_ok
            && matches!(repl_cr, CodeRange::SevenBit | CodeRange::Valid)
        {
            self.cr.set(CodeRange::Valid);
            return Ok(());
        }
        // Slow path: re-classify the whole buffer. Caching the
        // result keeps a chain of in-place splices O(N) rather than
        // O(N²).
        let cr = self.ty.classify(&self.content);
        if matches!(cr, CodeRange::Broken) && self.ty.is_utf8_compatible() {
            // CRuby downgrades to ASCII-8BIT when UTF-8-tagged
            // content becomes invalid; under that tag every byte is
            // valid.
            self.ty = Encoding::Ascii8;
            self.cr.set(CodeRange::Valid);
        } else {
            self.cr.set(cr);
        }
        Ok(())
    }

    /// Build an empty `RStringInner` tagged with the given encoding
    /// and pre-allocated for `cap` bytes. Empty content is always
    /// SevenBit (the empty byte sequence is trivially ASCII), so
    /// callers can append into the result without paying for an
    /// initial classification.
    pub fn with_encoding_capacity(encoding: Encoding, cap: usize) -> Self {
        RStringInner::from(
            SmallVec::with_capacity(cap),
            encoding,
            CodeRange::SevenBit,
        )
    }

    pub fn first_code(&self) -> Result<u32> {
        if self.len() == 0 {
            return Err(MonorubyErr::argumenterr("empty string"));
        }
        let ord = if self.ty.is_utf8_compatible() {
            self.check_utf8()?.chars().next().unwrap() as u32
        } else {
            // Binary / non-UTF-8 encodings report the leading byte;
            // matches CRuby for ASCII-8BIT and is the conservative
            // answer for dummy encodings we don't decode.
            self.as_bytes()[0] as u32
        };
        Ok(ord)
    }
}

#[cfg(test)]
mod encoding_tests {
    use super::*;

    #[test]
    fn try_from_str_normalises_separators_and_aliases() {
        assert_eq!(Encoding::try_from_str("UTF-8").unwrap(), Encoding::Utf8);
        assert_eq!(Encoding::try_from_str("utf-8").unwrap(), Encoding::Utf8);
        assert_eq!(Encoding::try_from_str("UTF8").unwrap(), Encoding::Utf8);
        assert_eq!(Encoding::try_from_str("BINARY").unwrap(), Encoding::Ascii8);
        assert_eq!(
            Encoding::try_from_str("ASCII-8BIT").unwrap(),
            Encoding::Ascii8
        );
        assert_eq!(
            Encoding::try_from_str("US-ASCII").unwrap(),
            Encoding::UsAscii
        );
        // ISO-8859-N round-trips through `_` and `-` separators.
        assert_eq!(
            Encoding::try_from_str("ISO-8859-1").unwrap(),
            Encoding::Iso8859(1)
        );
        assert_eq!(
            Encoding::try_from_str("ISO_8859_15").unwrap(),
            Encoding::Iso8859(15)
        );
        assert_eq!(
            Encoding::try_from_str("LATIN1").unwrap(),
            Encoding::Iso8859(1)
        );
        // UTF-16 / UTF-32 family.
        assert_eq!(
            Encoding::try_from_str("UTF-16LE").unwrap(),
            Encoding::Utf16Le
        );
        assert_eq!(
            Encoding::try_from_str("UTF-16BE").unwrap(),
            Encoding::Utf16Be
        );
        assert_eq!(Encoding::try_from_str("UTF-32").unwrap(), Encoding::Utf32Le);
        // Japanese.
        assert_eq!(Encoding::try_from_str("EUC-JP").unwrap(), Encoding::EucJp);
        assert_eq!(
            Encoding::try_from_str("Shift_JIS").unwrap(),
            Encoding::Sjis(0)
        );
        assert_eq!(
            Encoding::try_from_str("Windows-31J").unwrap(),
            Encoding::Sjis(1)
        );
        assert_eq!(Encoding::try_from_str("CP932").unwrap(), Encoding::Sjis(1));
        // Pseudo-encoding names map to UTF-8.
        assert_eq!(Encoding::try_from_str("LOCALE").unwrap(), Encoding::Utf8);
        // Unknown name → ArgumentError.
        assert!(Encoding::try_from_str("Bogus-1").is_err());
    }

    #[test]
    fn name_round_trips_through_try_from_str() {
        for enc in [
            Encoding::Ascii8,
            Encoding::Utf8,
            Encoding::UsAscii,
            Encoding::Utf16Le,
            Encoding::Utf16Be,
            Encoding::Utf32Le,
            Encoding::Utf32Be,
            Encoding::Iso8859(1),
            Encoding::Iso8859(5),
            Encoding::Iso8859(15),
            Encoding::EucJp,
            Encoding::Sjis(0),
            Encoding::Sjis(1),
        ] {
            assert_eq!(Encoding::try_from_str(enc.name()).unwrap(), enc);
        }
    }

    #[test]
    fn ascii_compatible_flags() {
        assert!(Encoding::Utf8.is_ascii_compatible());
        assert!(Encoding::UsAscii.is_ascii_compatible());
        assert!(Encoding::Ascii8.is_ascii_compatible());
        assert!(Encoding::Iso8859(1).is_ascii_compatible());
        assert!(Encoding::EucJp.is_ascii_compatible());
        assert!(Encoding::Sjis(0).is_ascii_compatible());
        assert!(!Encoding::Utf16Le.is_ascii_compatible());
        assert!(!Encoding::Utf16Be.is_ascii_compatible());
        assert!(!Encoding::Utf32Le.is_ascii_compatible());
        assert!(!Encoding::Utf32Be.is_ascii_compatible());
    }

    #[test]
    fn dummy_flags_cover_non_native_decoders() {
        assert!(!Encoding::Utf8.is_dummy());
        assert!(!Encoding::UsAscii.is_dummy());
        assert!(!Encoding::Ascii8.is_dummy());
        assert!(Encoding::Utf16Le.is_dummy());
        assert!(Encoding::Iso8859(1).is_dummy());
        assert!(Encoding::EucJp.is_dummy());
    }

    #[test]
    fn classify_seven_bit_short_circuit() {
        // SevenBit fast path applies to every ASCII-compatible enc
        // when all bytes are < 0x80.
        for enc in [
            Encoding::Utf8,
            Encoding::UsAscii,
            Encoding::Ascii8,
            Encoding::Iso8859(1),
            Encoding::EucJp,
            Encoding::Sjis(0),
        ] {
            assert_eq!(enc.classify(b"abc"), CodeRange::SevenBit, "{:?}", enc);
        }
        // UTF-16/32 don't qualify even for 7-bit-only payloads.
        assert_ne!(Encoding::Utf16Le.classify(b"ab"), CodeRange::SevenBit);
        assert_ne!(Encoding::Utf32Le.classify(b"abcd"), CodeRange::SevenBit);
        // Empty → SevenBit by definition.
        assert_eq!(Encoding::Utf8.classify(b""), CodeRange::SevenBit);
        assert_eq!(Encoding::Utf16Le.classify(b""), CodeRange::SevenBit);
    }

    #[test]
    fn classify_us_ascii_rejects_high_bytes() {
        // High byte under US-ASCII is Broken (Phase 1's stricter
        // semantics — was Valid pre-refactor).
        assert_eq!(Encoding::UsAscii.classify(b"\xff"), CodeRange::Broken);
        assert_eq!(Encoding::UsAscii.classify(b"a\xffz"), CodeRange::Broken);
    }

    #[test]
    fn classify_utf8_validity() {
        assert_eq!(Encoding::Utf8.classify("é".as_bytes()), CodeRange::Valid);
        assert_eq!(Encoding::Utf8.classify(&[0xff]), CodeRange::Broken);
        // Truncated 2-byte scalar.
        assert_eq!(Encoding::Utf8.classify(&[0xC3]), CodeRange::Broken);
    }

    #[test]
    fn classify_utf16_byte_count_parity() {
        // UTF-16 needs even byte count to validate.
        assert_eq!(Encoding::Utf16Le.classify(&[0x61, 0x00]), CodeRange::Valid);
        assert_eq!(
            Encoding::Utf16Le.classify(&[0x61, 0x00, 0x62]),
            CodeRange::Broken
        );
        assert_eq!(Encoding::Utf16Be.classify(&[0x00, 0x61]), CodeRange::Valid);
    }

    #[test]
    fn classify_utf32_byte_count_parity() {
        assert_eq!(
            Encoding::Utf32Le.classify(&[0x61, 0x00, 0x00, 0x00]),
            CodeRange::Valid
        );
        assert_eq!(
            Encoding::Utf32Le.classify(&[0x61, 0x00, 0x00]),
            CodeRange::Broken
        );
    }

    #[test]
    fn cr_cache_is_lazy_and_then_stable() {
        // First read computes; subsequent reads return the same
        // value without re-walking. We can't directly observe
        // "didn't re-walk" in a unit test, but we can pin the
        // computed value.
        let s = RStringInner::from_str("abc");
        assert_eq!(s.code_range(), CodeRange::SevenBit);
        assert_eq!(s.code_range(), CodeRange::SevenBit);
        assert!(s.is_ascii_only());
        assert!(s.is_valid_encoding());

        let bad = RStringInner::from_encoding(b"\xff", Encoding::Utf8);
        assert_eq!(bad.code_range(), CodeRange::Broken);
        assert!(!bad.is_ascii_only());
        assert!(!bad.is_valid_encoding());
    }

    #[test]
    fn set_encoding_invalidates_cache() {
        let mut s = RStringInner::from_str("abc");
        // Prime the cache (SevenBit under UTF-8).
        assert_eq!(s.code_range(), CodeRange::SevenBit);
        // Switch to UTF-16LE — 3 bytes is now a broken code-unit
        // sequence. The cache must clear so the next read returns
        // Broken.
        s.set_encoding(Encoding::Utf16Le);
        assert_eq!(s.code_range(), CodeRange::Broken);
    }

    #[test]
    fn from_str_does_not_scan_until_queried() {
        // The no-scan constructor should leave cr at Unknown so
        // ephemeral strings that never need cr pay nothing. Once
        // queried, lazy classify computes and caches.
        let s = RStringInner::from_str("abc");
        assert_eq!(s.cr.get(), CodeRange::Unknown);
        assert_eq!(s.code_range(), CodeRange::SevenBit);
        assert_eq!(s.cr.get(), CodeRange::SevenBit);

        let s = RStringInner::from_str("あいう");
        assert_eq!(s.cr.get(), CodeRange::Unknown);
        assert_eq!(s.code_range(), CodeRange::Valid);
    }

    #[test]
    fn from_str_scanned_sets_cr_eagerly() {
        // The scanning constructor records cr at construction time
        // so subsequent queries are O(1) and `deep_copy` clones
        // inherit the classification without rescanning.
        let s = RStringInner::from_str_scanned("abc");
        assert_eq!(s.cr.get(), CodeRange::SevenBit);

        let s = RStringInner::from_str_scanned("あいう");
        assert_eq!(s.cr.get(), CodeRange::Valid);

        // Empty -> classify returns SevenBit; eager constructor
        // mirrors that.
        let s = RStringInner::from_str_scanned("");
        assert_eq!(s.cr.get(), CodeRange::SevenBit);
    }

    #[test]
    fn from_string_does_not_scan_until_queried() {
        let s = RStringInner::from_string("abc".to_string());
        assert_eq!(s.cr.get(), CodeRange::Unknown);
        assert_eq!(s.code_range(), CodeRange::SevenBit);
    }

    #[test]
    fn from_string_scanned_sets_cr_eagerly() {
        let s = RStringInner::from_string_scanned("abc".to_string());
        assert_eq!(s.cr.get(), CodeRange::SevenBit);

        let s = RStringInner::from_string_scanned("カナ".to_string());
        assert_eq!(s.cr.get(), CodeRange::Valid);
    }

    #[test]
    fn from_encoding_does_not_scan_until_queried() {
        let s = RStringInner::from_encoding(b"abc", Encoding::Utf8);
        assert_eq!(s.cr.get(), CodeRange::Unknown);
        assert_eq!(s.code_range(), CodeRange::SevenBit);
    }

    #[test]
    fn from_encoding_scanned_sets_cr_eagerly() {
        // Source-byte literal that's all ASCII -> SevenBit.
        let s = RStringInner::from_encoding_scanned(b"abc", Encoding::Utf8);
        assert_eq!(s.cr.get(), CodeRange::SevenBit);

        // Source-byte literal with valid UTF-8 multi-byte content.
        let s = RStringInner::from_encoding_scanned("é".as_bytes(), Encoding::Utf8);
        assert_eq!(s.cr.get(), CodeRange::Valid);

        // Source-byte literal with broken UTF-8 (e.g. `"\xff"`).
        let s = RStringInner::from_encoding_scanned(b"\xff", Encoding::Utf8);
        assert_eq!(s.cr.get(), CodeRange::Broken);

        // Ascii8 + high bytes: trivially Valid (no cut into char
        // sequences).
        let s = RStringInner::from_encoding_scanned(b"\xff\xfe", Encoding::Ascii8);
        assert_eq!(s.cr.get(), CodeRange::Valid);
    }

    #[test]
    fn propagated_cr_empty_range_is_seven_bit() {
        // Any empty byte range matches the way `Encoding::classify`
        // treats `b""` — SevenBit by definition, regardless of the
        // parent's encoding or cr.
        let parent = RStringInner::from_encoding(b"\xff", Encoding::Ascii8);
        assert_eq!(parent.code_range(), CodeRange::Valid);
        assert_eq!(
            RStringInner::propagated_cr(&parent, 0, 0),
            CodeRange::SevenBit
        );
        assert_eq!(
            RStringInner::propagated_cr(&parent, 1, 1),
            CodeRange::SevenBit
        );
    }

    #[test]
    fn propagated_cr_valid_single_byte_encodings_propagate_unconditionally() {
        // ASCII-8BIT: every byte position is a character boundary, so
        // a Valid parent always yields a Valid child for any non-empty
        // sub-range.
        let parent = RStringInner::from_encoding(&[0xff, 0x80, 0x81], Encoding::Ascii8);
        assert_eq!(parent.code_range(), CodeRange::Valid);
        assert_eq!(RStringInner::propagated_cr(&parent, 0, 3), CodeRange::Valid);
        assert_eq!(RStringInner::propagated_cr(&parent, 1, 2), CodeRange::Valid);

        // ISO-8859-1: same — every byte represents a glyph, so any
        // cut respects character boundaries.
        let parent = RStringInner::from_encoding(&[0xc3, 0xa9, 0xfe], Encoding::Iso8859(1));
        assert_eq!(parent.code_range(), CodeRange::Valid);
        assert_eq!(RStringInner::propagated_cr(&parent, 0, 3), CodeRange::Valid);
        assert_eq!(RStringInner::propagated_cr(&parent, 1, 3), CodeRange::Valid);
    }

    #[test]
    fn propagated_cr_valid_utf8_only_propagates_on_char_boundaries() {
        // "abcあdef" — 9 bytes; the あ scalar occupies offsets 3..6
        // (E3 81 82). Cuts that land on offsets 0/3/6/9 are character
        // boundaries; offsets 4/5/7/8 land on continuation bytes and
        // must NOT inherit Valid.
        let parent = RStringInner::from_encoding("abcあdef".as_bytes(), Encoding::Utf8);
        assert_eq!(parent.code_range(), CodeRange::Valid);

        for &(start, end) in &[(0, 3), (0, 6), (3, 6), (6, 9), (3, 9), (0, 9)] {
            assert_eq!(
                RStringInner::propagated_cr(&parent, start, end),
                CodeRange::Valid,
                "{start}..{end} should propagate Valid"
            );
        }

        for &(start, end) in &[(0, 4), (0, 5), (3, 5), (4, 6), (4, 9), (5, 9)] {
            assert_eq!(
                RStringInner::propagated_cr(&parent, start, end),
                CodeRange::Unknown,
                "{start}..{end} crosses a UTF-8 continuation byte"
            );
        }
    }

    #[test]
    fn propagated_cr_valid_utf16_requires_even_byte_endpoints() {
        // "abc" encoded as UTF-16LE: 6 bytes, code-unit width 2.
        let parent =
            RStringInner::from_encoding(&[0x61, 0x00, 0x62, 0x00, 0x63, 0x00], Encoding::Utf16Le);
        assert_eq!(parent.code_range(), CodeRange::Valid);

        for &(start, end) in &[(0, 2), (0, 4), (0, 6), (2, 6), (4, 6)] {
            assert_eq!(
                RStringInner::propagated_cr(&parent, start, end),
                CodeRange::Valid,
                "{start}..{end} should propagate Valid"
            );
        }

        for &(start, end) in &[(0, 1), (0, 3), (0, 5), (1, 4), (3, 6)] {
            assert_eq!(
                RStringInner::propagated_cr(&parent, start, end),
                CodeRange::Unknown,
                "{start}..{end} would split a UTF-16 code unit"
            );
        }

        // BE side: same parity rule.
        let parent = RStringInner::from_encoding(&[0x00, 0x61, 0x00, 0x62], Encoding::Utf16Be);
        assert_eq!(parent.code_range(), CodeRange::Valid);
        assert_eq!(RStringInner::propagated_cr(&parent, 0, 4), CodeRange::Valid);
        assert_eq!(
            RStringInner::propagated_cr(&parent, 1, 4),
            CodeRange::Unknown
        );
    }

    #[test]
    fn propagated_cr_valid_utf32_requires_endpoints_aligned_to_four() {
        // "ab" encoded as UTF-32LE: 8 bytes, code-unit width 4.
        let parent = RStringInner::from_encoding(
            &[0x61, 0x00, 0x00, 0x00, 0x62, 0x00, 0x00, 0x00],
            Encoding::Utf32Le,
        );
        assert_eq!(parent.code_range(), CodeRange::Valid);

        for &(start, end) in &[(0, 4), (0, 8), (4, 8)] {
            assert_eq!(
                RStringInner::propagated_cr(&parent, start, end),
                CodeRange::Valid
            );
        }

        for &(start, end) in &[(0, 2), (0, 5), (1, 4), (2, 8)] {
            assert_eq!(
                RStringInner::propagated_cr(&parent, start, end),
                CodeRange::Unknown
            );
        }
    }

    #[test]
    fn propagated_cr_valid_no_native_decoder_falls_back_to_unknown() {
        // EUC-JP / Shift_JIS: variable-width but monoruby has no
        // native decoder, so we can't tell whether `start`/`end`
        // land on a character boundary. The conservative choice is
        // Unknown so the next operation lazy-classifies.
        let parent = RStringInner::from_encoding(&[0xc6, 0xfc, 0xcb, 0xdc], Encoding::EucJp);
        assert_eq!(parent.code_range(), CodeRange::Valid);
        assert_eq!(
            RStringInner::propagated_cr(&parent, 0, 2),
            CodeRange::Unknown
        );
        assert_eq!(
            RStringInner::propagated_cr(&parent, 0, 4),
            CodeRange::Unknown
        );

        let parent = RStringInner::from_encoding(&[0x82, 0xa0, 0x82, 0xa2], Encoding::Sjis(0));
        assert_eq!(parent.code_range(), CodeRange::Valid);
        assert_eq!(
            RStringInner::propagated_cr(&parent, 0, 1),
            CodeRange::Unknown
        );
        assert_eq!(
            RStringInner::propagated_cr(&parent, 0, 4),
            CodeRange::Unknown
        );
    }

    #[test]
    fn propagated_cr_broken_or_unknown_parent_never_propagates() {
        // A Broken parent could have either Broken or Valid sub-
        // ranges depending on which bytes the slice covers, so we
        // don't propagate.
        let parent = RStringInner::from_encoding(&[0x61, 0xff, 0x62], Encoding::Utf8);
        assert_eq!(parent.code_range(), CodeRange::Broken);
        assert_eq!(
            RStringInner::propagated_cr(&parent, 0, 1),
            CodeRange::Unknown
        );
        assert_eq!(
            RStringInner::propagated_cr(&parent, 0, 3),
            CodeRange::Unknown
        );
        assert_eq!(
            RStringInner::propagated_cr(&parent, 2, 3),
            CodeRange::Unknown
        );
    }

    #[test]
    fn from_vec_scanned_classifies_eagerly() {
        // All-ASCII bytes: tagged UTF-8 and pre-classified as
        // SevenBit, skipping the `from_utf8` rerun the lazy path
        // would do on first use.
        let s = RStringInner::from_vec_scanned(b"abc".to_vec());
        assert_eq!(s.encoding(), Encoding::Utf8);
        assert_eq!(s.code_range(), CodeRange::SevenBit);

        // Non-ASCII but valid UTF-8: tagged UTF-8 and pre-classified
        // Valid (the from_utf8 check landed on Ok).
        let s = RStringInner::from_vec_scanned("あいう".as_bytes().to_vec());
        assert_eq!(s.encoding(), Encoding::Utf8);
        assert_eq!(s.code_range(), CodeRange::Valid);

        // Two-byte UTF-8 scalar: still Valid + UTF-8.
        let s = RStringInner::from_vec_scanned("é".as_bytes().to_vec());
        assert_eq!(s.encoding(), Encoding::Utf8);
        assert_eq!(s.code_range(), CodeRange::Valid);

        // Invalid UTF-8 falls back to ASCII-8BIT, which classifies
        // every non-empty byte sequence as Valid.
        let s = RStringInner::from_vec_scanned(vec![0xff, 0xfe, 0x80]);
        assert_eq!(s.encoding(), Encoding::Ascii8);
        assert_eq!(s.code_range(), CodeRange::Valid);

        // Empty input: SevenBit by definition under any encoding.
        let s = RStringInner::from_vec_scanned(vec![]);
        assert_eq!(s.encoding(), Encoding::Utf8);
        assert_eq!(s.code_range(), CodeRange::SevenBit);
    }

    #[test]
    fn encoding_compatible_same_encoding() {
        // `Encoding::compatible` (the bare classifier used by
        // `Encoding.compatible?`) — same encoding always returns
        // that encoding regardless of CR.
        assert_eq!(
            Encoding::compatible(
                Encoding::Utf8,
                CodeRange::Valid,
                Encoding::Utf8,
                CodeRange::Broken
            ),
            Some(Encoding::Utf8)
        );
    }

    #[test]
    fn encoding_compatible_seven_bit_left_wins() {
        // Both ASCII-compatible AND both 7-bit → left wins.
        assert_eq!(
            Encoding::compatible(
                Encoding::Utf8,
                CodeRange::SevenBit,
                Encoding::UsAscii,
                CodeRange::SevenBit,
            ),
            Some(Encoding::Utf8)
        );
        assert_eq!(
            Encoding::compatible(
                Encoding::UsAscii,
                CodeRange::SevenBit,
                Encoding::Utf8,
                CodeRange::SevenBit,
            ),
            Some(Encoding::UsAscii)
        );
    }

    #[test]
    fn encoding_compatible_seven_bit_defers_to_other() {
        // Exactly one side 7-bit → the non-7-bit side keeps its
        // encoding.
        assert_eq!(
            Encoding::compatible(
                Encoding::Utf8,
                CodeRange::SevenBit,
                Encoding::Iso8859(1),
                CodeRange::Valid,
            ),
            Some(Encoding::Iso8859(1))
        );
        assert_eq!(
            Encoding::compatible(
                Encoding::Iso8859(1),
                CodeRange::Valid,
                Encoding::Utf8,
                CodeRange::SevenBit,
            ),
            Some(Encoding::Iso8859(1))
        );
    }

    #[test]
    fn encoding_compatible_incompatible_pairs() {
        // Two non-7-bit, distinct ASCII-compatible encodings → None.
        assert_eq!(
            Encoding::compatible(
                Encoding::Utf8,
                CodeRange::Valid,
                Encoding::Iso8859(1),
                CodeRange::Valid,
            ),
            None
        );
        // UTF-16 vs anything → None (UTF-16 isn't ASCII-compatible).
        assert_eq!(
            Encoding::compatible(
                Encoding::Utf16Le,
                CodeRange::Valid,
                Encoding::Utf8,
                CodeRange::SevenBit,
            ),
            None
        );
    }

    // ---------- Non-UTF-8 / error-path coverage ----------

    #[test]
    fn to_str_errors_on_invalid_utf8_in_utf8_string() {
        // UTF-8-tagged bytes containing an invalid sequence: `to_str`
        // returns Err with the "invalid byte sequence: ..." prefix.
        let s = RStringInner::from_encoding(b"abc\xFFdef", Encoding::Utf8);
        let err = s.to_str().unwrap_err();
        let msg = err.message();
        assert!(
            msg.starts_with("invalid byte sequence:"),
            "unexpected message: {msg}"
        );
    }

    #[test]
    fn to_str_escapes_high_bytes_for_binary_encoding() {
        // Non-UTF-8 encodings always succeed; high bytes come back as
        // `\xHH` literal in the resulting string.
        let s = RStringInner::from_encoding(b"a\xFFb", Encoding::Ascii8);
        assert_eq!(s.to_str().unwrap().as_ref(), r"a\xFFb");

        let utf16 = RStringInner::from_encoding(&[0x00, 0xD8], Encoding::Utf16Le);
        // 0x00 stays ASCII-printable; 0xD8 escapes.
        assert_eq!(utf16.to_str().unwrap().as_ref(), "\x00\\xD8");
    }

    #[test]
    fn byte_to_char_index_errors_on_invalid_utf8_bytes() {
        // `byte_to_char_index` walks `check_utf8()`, which fails on
        // any byte sequence that isn't valid UTF-8 — here a Shift_JIS
        // string whose high bytes are not valid UTF-8 starters.
        let s = RStringInner::from_encoding(b"\x82\xa0", Encoding::Sjis(0));
        assert!(s.byte_to_char_index(0).is_err());

        // Same for an explicitly-broken UTF-8 string.
        let s = RStringInner::from_encoding(b"abc\xFF", Encoding::Utf8);
        assert!(s.byte_to_char_index(3).is_err());
    }

    #[test]
    fn byte_to_char_index_errors_off_char_boundary() {
        // UTF-8 valid string ("aあb"). `あ` occupies bytes 1..4. A
        // byte position pointing into the middle of `あ` is rejected.
        let s = RStringInner::from_str("aあb");
        assert_eq!(s.byte_to_char_index(0).unwrap(), 0);
        assert_eq!(s.byte_to_char_index(1).unwrap(), 1);
        assert_eq!(s.byte_to_char_index(4).unwrap(), 2);
        assert!(s.byte_to_char_index(2).is_err());
        assert!(s.byte_to_char_index(3).is_err());
        // Out-of-range byte position also errors.
        assert!(s.byte_to_char_index(99).is_err());
    }

    #[test]
    fn dump_non_utf8_compatible_routes_through_ascii_escape() {
        // Non-UTF-8-compatible encodings (UTF-16/32, ISO-8859,
        // EUC-JP, Shift_JIS) take the byte-wise branch in `dump`.
        // Each byte goes through `ascii_escape` (high bytes become
        // `\xHH`).
        let s = RStringInner::from_encoding(b"a\x80\xFFb", Encoding::Ascii8);
        assert_eq!(s.dump(), "a\\x80\\xFFb");

        let s = RStringInner::from_encoding(&[0x00, 0xD8, 0x00, 0x00], Encoding::Utf16Le);
        assert_eq!(s.dump(), "\\x00\\xD8\\x00\\x00");

        // The `#`-trigram lookahead also fires on the byte-wise path.
        let s = RStringInner::from_encoding(b"a#$b", Encoding::Ascii8);
        assert_eq!(s.dump(), "a\\#$b");
        let s = RStringInner::from_encoding(b"#@x", Encoding::Ascii8);
        assert_eq!(s.dump(), "\\#@x");
        let s = RStringInner::from_encoding(b"#{}", Encoding::Ascii8);
        assert_eq!(s.dump(), "\\#{}");
        // Lone `#` (not followed by a trigram char) is left alone.
        let s = RStringInner::from_encoding(b"#abc", Encoding::Ascii8);
        assert_eq!(s.dump(), "#abc");
    }

    #[test]
    fn utf8_dump_with_lookahead_handles_invalid_utf8() {
        // Invalid bytes within a UTF-8-tagged buffer fall into the
        // `Err(_)` arm of `from_utf8` and emit `\xHH` per byte.
        let mut out = String::new();
        utf8_dump_with_lookahead(&mut out, b"a\xFFb");
        assert_eq!(out, "a\\xFFb");

        // Multiple invalid bytes in a row → one `\xHH` each.
        let mut out = String::new();
        utf8_dump_with_lookahead(&mut out, &[0xC0, 0xC1, 0xF5]);
        assert_eq!(out, "\\xC0\\xC1\\xF5");

        // Mixed valid + invalid: walk-and-resume keeps the rest of
        // the input. `#$` inside the valid prefix is escaped.
        let mut out = String::new();
        utf8_dump_with_lookahead(&mut out, b"#$\xFFhi");
        assert_eq!(out, "\\#$\\xFFhi");

        // Truncated multibyte at the end (the `error_len() == None`
        // arm): the leading bytes surface as `\xHH`.
        let mut out = String::new();
        utf8_dump_with_lookahead(&mut out, &[b'a', 0xE3, 0x81]);
        assert_eq!(out, "a\\xE3\\x81");

        // BMP non-ASCII still passes through `utf8_dump_one` →
        // `utf8_escape`, so it lands on `\uNNNN` (confirms the
        // lookahead-driven path doesn't bypass the codepoint-form
        // rule for valid runs).
        let mut out = String::new();
        utf8_dump_with_lookahead(&mut out, "\u{0080}".as_bytes());
        assert_eq!(out, "\\u0080");

        // Supplementary-plane char → brace form.
        let mut out = String::new();
        utf8_dump_with_lookahead(&mut out, "\u{1F600}".as_bytes());
        assert_eq!(out, "\\u{1F600}");
    }

    #[test]
    fn with_encoding_capacity_starts_seven_bit() {
        // The empty byte sequence is trivially ASCII, so a freshly
        // allocated empty buffer should advertise SevenBit
        // regardless of the declared encoding.
        let s = RStringInner::with_encoding_capacity(Encoding::Sjis(0), 16);
        assert_eq!(s.cr.get(), CodeRange::SevenBit);
        assert_eq!(s.encoding(), Encoding::Sjis(0));
        assert_eq!(s.as_bytes().len(), 0);

        let s = RStringInner::with_encoding_capacity(Encoding::Utf16Le, 0);
        assert_eq!(s.cr.get(), CodeRange::SevenBit);
        assert_eq!(s.encoding(), Encoding::Utf16Le);
    }

    #[test]
    fn bytesplice_with_seven_bit_combines_to_seven_bit() {
        let mut globals = Globals::new_test();
        let mut s = RStringInner::from_str_scanned("abcdef");
        let repl = RStringInner::from_str_scanned("XY");
        assert_eq!(s.code_range(), CodeRange::SevenBit);
        assert_eq!(repl.code_range(), CodeRange::SevenBit);

        s.bytesplice_with(2, 2, &repl, &globals.store).unwrap();
        assert_eq!(s.as_bytes(), b"abXYef");
        assert_eq!(s.cr.get(), CodeRange::SevenBit);
        assert_eq!(s.encoding(), Encoding::Utf8);
    }

    #[test]
    fn bytesplice_with_seven_bit_plus_valid_is_valid() {
        let mut globals = Globals::new_test();
        let mut s = RStringInner::from_str_scanned("abcdef");
        let repl = RStringInner::from_str_scanned("あ"); // 3 bytes, Valid
        assert_eq!(s.code_range(), CodeRange::SevenBit);
        assert_eq!(repl.code_range(), CodeRange::Valid);

        s.bytesplice_with(2, 2, &repl, &globals.store).unwrap();
        assert_eq!(s.as_bytes(), "abあef".as_bytes());
        // SevenBit + Valid on aligned UTF-8 boundaries → Valid in O(1).
        assert_eq!(s.cr.get(), CodeRange::Valid);
    }

    #[test]
    fn bytesplice_with_valid_plus_valid_keeps_valid() {
        let mut globals = Globals::new_test();
        let mut s = RStringInner::from_str_scanned("xあy");
        let repl = RStringInner::from_str_scanned("いう");
        assert_eq!(s.code_range(), CodeRange::Valid);
        assert_eq!(repl.code_range(), CodeRange::Valid);

        // Replace the middle 3 bytes ("あ") with the 6-byte replacement.
        s.bytesplice_with(1, 3, &repl, &globals.store).unwrap();
        assert_eq!(s.as_bytes(), "xいうy".as_bytes());
        assert_eq!(s.cr.get(), CodeRange::Valid);
    }

    #[test]
    fn bytesplice_with_us_ascii_and_utf8_combine() {
        // Pure-ASCII Utf8 and UsAscii are compatible; CRuby keeps
        // the receiver's encoding when both sides are SevenBit. The
        // important guarantees here are (a) the call succeeds and
        // (b) cr stays SevenBit, since both sides are pure ASCII
        // and the result encoding is ASCII-compatible.
        let mut globals = Globals::new_test();
        let mut s = RStringInner::from_str_scanned("hi");
        s.set_encoding(Encoding::UsAscii);
        let repl = RStringInner::from_str_scanned("X");
        assert_eq!(s.encoding(), Encoding::UsAscii);
        assert_eq!(repl.encoding(), Encoding::Utf8);

        s.bytesplice_with(0, 1, &repl, &globals.store).unwrap();
        assert_eq!(s.as_bytes(), b"Xi");
        assert!(s.encoding().is_ascii_compatible());
        assert_eq!(s.cr.get(), CodeRange::SevenBit);
    }

    #[test]
    fn bytesplice_with_incompatible_encodings_errors() {
        // Sjis with a non-ASCII byte cannot be combined with a
        // non-ASCII UTF-8 replacement.
        let globals = Globals::new_test();
        let mut s = RStringInner::from_encoding_scanned(b"\x82\xa0", Encoding::Sjis(0)); // "あ" in Sjis
        let repl = RStringInner::from_str_scanned("え");
        assert!(s.bytesplice_with(0, 0, &repl, &globals.store).is_err());
    }

    #[test]
    fn bytesplice_with_breaks_utf8_boundary_demotes_to_ascii8() {
        // Splicing into the middle of a multi-byte UTF-8 character
        // produces broken bytes; under Utf8 tagging CRuby (and our
        // implementation) demotes to ASCII-8BIT.
        let mut globals = Globals::new_test();
        let mut s = RStringInner::from_str_scanned("あ"); // 3 bytes
        let repl = RStringInner::from_str_scanned("X");

        // Replace byte 1 (middle of "あ") — boundary check fails.
        s.bytesplice_with(1, 0, &repl, &globals.store).unwrap();
        assert_eq!(s.encoding(), Encoding::Ascii8);
        assert_eq!(s.cr.get(), CodeRange::Valid);
    }
}
