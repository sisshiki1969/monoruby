use super::*;
use smallvec::SmallVec;
use std::cell::Cell;
use std::cmp::Ordering;

pub mod pack;
mod printable;

#[monoruby_object]
pub struct RString(Value);

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
        let normalized = s
            .to_uppercase()
            .replace('-', "_")
            .replace('.', "_");
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

            "EUC_JP" | "EUCJP" | "EUCJP_MS" | "EUCJP_WIN" | "CP51932"
            | "STATELESS_ISO_2022_JP" => Ok(Encoding::EucJp),
            "SHIFT_JIS" | "SJIS" | "MACJAPANESE" | "MACJAPAN" | "ISO_2022_JP"
            | "ISO2022_JP" => Ok(Encoding::Sjis(0)),
            "WINDOWS_31J" | "CP932" | "CSWINDOWS31J" | "WINDOWS31J" => Ok(Encoding::Sjis(1)),

            // Other byte-oriented encodings without native support
            // are stored as ASCII-8BIT but the name isn't preserved
            // (consistent with monoruby's prior behaviour).
            "WINDOWS_1250" | "CP1250" | "WINDOWS_1251" | "CP1251" | "WINDOWS_1252"
            | "CP1252" | "WINDOWS_1253" | "CP1253" | "WINDOWS_1254" | "CP1254"
            | "WINDOWS_1255" | "CP1255" | "WINDOWS_1256" | "CP1256" | "WINDOWS_1257"
            | "CP1257" | "WINDOWS_1258" | "CP1258" | "IBM437" | "CP437" | "IBM737"
            | "CP737" | "IBM775" | "CP775" | "IBM850" | "CP850" | "IBM852" | "CP852"
            | "IBM855" | "CP855" | "IBM857" | "CP857" | "IBM860" | "CP860" | "IBM861"
            | "CP861" | "IBM862" | "CP862" | "IBM863" | "CP863" | "IBM864" | "CP864"
            | "IBM865" | "CP865" | "IBM866" | "CP866" | "IBM869" | "CP869" | "KOI8_R"
            | "KOI8_U" | "GB2312" | "EUC_CN" | "GBK" | "CP936" | "GB18030" | "BIG5"
            | "BIG5_HKSCS" | "EUC_KR" | "EUCKR" | "CP949" | "EUC_TW" | "EUCTW"
            | "TIS_620" | "TIS620" | "CESU_8" | "CESU8" => Ok(Encoding::Ascii8),

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
            utf8_escape_bytes(&mut res, self.as_bytes(), utf8_escape);
            res
        } else {
            let mut res = String::with_capacity(self.len());
            for c in self.as_bytes() {
                ascii_escape(&mut res, *c);
            }
            res
        }
    }

    pub fn inspect(&self) -> String {
        if self.ty.is_utf8_compatible() {
            let mut res = String::with_capacity(self.len());
            utf8_escape_bytes(&mut res, self.as_bytes(), utf8_inspect);
            res
        } else {
            let mut res = String::with_capacity(self.len());
            for c in self.as_bytes() {
                ascii_escape(&mut res, *c);
            }
            res
        }
    }

    pub fn valid(&self) -> bool {
        self.is_valid_encoding()
    }
}

fn utf8_escape(s: &mut String, ch: char) {
    if ch as u32 <= 0xff {
        ascii_escape(s, ch as u8);
    } else {
        s.push_str(&format!("\\u{:0>4X}", ch as u32));
    }
}

fn utf8_inspect(s: &mut String, ch: char) {
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
            c if c.is_ascii_graphic() || c == b' ' => s.push(c as char),
            // Other ASCII control chars use \uNNNN in UTF-8 strings
            _ => s.push_str(&format!("\\u{:0>4X}", b)),
        }
    } else if printable::is_printable(ch) {
        s.push(ch);
    } else {
        s.push_str(&format!("\\u{{{:0>4X}}}", ch as u32));
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

/// Process a byte slice as UTF-8, applying `escape_fn` to valid characters
/// and escaping invalid bytes as `\xHH`. This avoids panicking on non-UTF-8
/// byte sequences in UTF-8 encoded strings.
fn utf8_escape_bytes(res: &mut String, bytes: &[u8], escape_fn: fn(&mut String, char)) {
    let mut i = 0;
    while i < bytes.len() {
        match std::str::from_utf8(&bytes[i..]) {
            Ok(valid) => {
                for c in valid.chars() {
                    escape_fn(res, c);
                }
                break;
            }
            Err(err) => {
                let valid_up_to = err.valid_up_to();
                // Process valid UTF-8 prefix
                if valid_up_to > 0 {
                    // SAFETY: from_utf8 confirmed these bytes are valid UTF-8.
                    let valid_str = unsafe { std::str::from_utf8_unchecked(&bytes[i..i + valid_up_to]) };
                    for c in valid_str.chars() {
                        escape_fn(res, c);
                    }
                }
                // Escape the invalid byte(s)
                let error_len = err.error_len().unwrap_or(bytes.len() - i - valid_up_to);
                for b in &bytes[i + valid_up_to..i + valid_up_to + error_len] {
                    res.push_str(&format!("\\x{:0>2X}", b));
                }
                i += valid_up_to + error_len;
            }
        }
    }
}

impl RStringInner {
    fn from(content: SmallVec<[u8; STRING_INLINE_CAP]>, ty: Encoding) -> Self {
        RStringInner {
            content,
            ty,
            cr: Cell::new(CodeRange::Unknown),
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

    pub fn check_utf8(&self) -> Result<&str> {
        match std::str::from_utf8(self) {
            Ok(s) => Ok(s),
            Err(_) => Err(MonorubyErr::argumenterr(format!(
                "invalid byte sequence in UTF-8"
            ))),
        }
    }

    pub fn from_str(s: &str) -> Self {
        RStringInner::from(SmallVec::from_slice(s.as_bytes()), Encoding::Utf8)
    }

    pub fn from_string(s: String) -> Self {
        RStringInner::from(SmallVec::from_vec(s.into_bytes()), Encoding::Utf8)
    }

    pub fn bytes(slice: &[u8]) -> Self {
        RStringInner::from(SmallVec::from_slice(slice), Encoding::Ascii8)
    }

    pub fn bytes_from_vec(vec: Vec<u8>) -> Self {
        RStringInner::from(SmallVec::from_vec(vec), Encoding::Ascii8)
    }

    pub fn from_encoding(slice: &[u8], encoding: Encoding) -> Self {
        RStringInner::from(SmallVec::from_slice(slice), encoding)
    }

    pub fn string_from_vec(vec: Vec<u8>) -> Self {
        let enc = if std::str::from_utf8(&vec).is_ok() {
            Encoding::Utf8
        } else {
            Encoding::Ascii8
        };
        RStringInner::from(SmallVec::from_vec(vec), enc)
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.content
    }

    pub fn set_byte(&mut self, index: usize, byte: u8) {
        self.content[index] = byte;
        // Phase 1 lets a UTF-8-tagged buffer hold invalid bytes,
        // so we no longer downgrade the encoding tag here. The
        // cached classification is invalidated and will be
        // re-computed on the next access (`valid_encoding?` will
        // see the byte and report Broken).
        self.cr.set(CodeRange::Unknown);
    }

    ///
    /// Get the length in char of the string `self`.
    ///
    pub fn char_length(&self) -> Result<usize> {
        let len = if self.ty.is_utf8_compatible() {
            self.check_utf8()?.chars().count()
        } else {
            // Non-UTF-8 encodings: report byte length until per-encoding
            // char-iteration is implemented (matches CRuby for
            // ASCII-8BIT and is the conservative answer for the dummy
            // encodings monoruby doesn't decode).
            self.content.len()
        };
        Ok(len)
    }

    ///
    /// Convert `char_pos` to the true position in char of the string `self`.
    ///
    /// Return None if `i` is out of range.
    ///
    pub fn conv_char_index(&self, char_pos: i64) -> Result<Option<usize>> {
        let len = self.char_length()?;
        if char_pos >= 0 {
            if char_pos <= len as i64 {
                Ok(Some(char_pos as usize))
            } else {
                Ok(None)
            }
        } else {
            match len as i64 + char_pos {
                n if n < 0 => Ok(None),
                n => Ok(Some(n as usize)),
            }
        }
    }

    ///
    /// Convert `char_pos` to the true position in char of the string `self`.
    ///
    /// Return None if `i` is negative.
    ///
    pub fn conv_char_index2(&self, char_pos: i64) -> Result<Option<usize>> {
        let len = self.char_length()?;
        if len == 0 && char_pos == -1 {
            return Ok(Some(0));
        }
        if char_pos >= 0 {
            Ok(Some(char_pos as usize))
        } else {
            match len as i64 + char_pos {
                n if n < 0 => Ok(None),
                n => Ok(Some(n as usize)),
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
        if self.ty.is_utf8_compatible() {
            if let Ok(s) = std::str::from_utf8(self) {
                let mut start = 0;
                let mut end = 0;
                for (char_i, (byte_i, _)) in s.char_indices().enumerate() {
                    if char_i == index {
                        start = byte_i;
                        end = s.len();
                    }
                    if char_i == index + len {
                        end = byte_i;
                        break;
                    }
                }
                return start..end;
            }
        }
        // Byte-based indexing fallback (Ascii8 / dummy encodings /
        // broken UTF-8).
        if self.len() <= index {
            0..0
        } else if self.len() <= index + len {
            index..self.len()
        } else {
            index..index + len
        }
    }

    pub fn extend(&mut self, other: &Self) -> Result<()> {
        if other.is_empty() {
            return Ok(());
        }
        let self_ascii_only = self.content.is_ascii();
        let other_ascii_only = other.content.is_ascii();
        match (self.ty, other.ty) {
            (Encoding::Utf8 | Encoding::UsAscii, Encoding::Ascii8) => {
                if other_ascii_only {
                    // ASCII-8BIT with only ASCII bytes is compatible with UTF-8
                } else if self_ascii_only {
                    // self is UTF-8 but ASCII-only, other is binary non-ASCII => downgrade
                    self.ty = Encoding::Ascii8;
                } else {
                    // both have non-ASCII bytes => incompatible
                    return Err(MonorubyErr::runtimeerr(
                        "incompatible character encodings: UTF-8 and ASCII-8BIT",
                    ));
                }
            }
            (Encoding::Ascii8, Encoding::Utf8 | Encoding::UsAscii) => {
                if self_ascii_only && !other_ascii_only {
                    // self is binary but ASCII-only, other has non-ASCII UTF-8 => upgrade
                    self.ty = Encoding::Utf8;
                }
                // otherwise keep ASCII-8BIT
            }
            _ => {}
        }
        self.content.extend_from_slice(other);
        // The cached classification doesn't survive mutation: appending
        // bytes can flip SevenBit → Valid/Broken, or two Broken halves
        // can combine into a Valid character.
        self.cr.set(CodeRange::Unknown);
        Ok(())
    }

    pub fn extend_from_slice_checked(&mut self, slice: &[u8]) -> Result<()> {
        if self.ty.is_utf8_compatible() && std::str::from_utf8(slice).is_err() {
            return Err(MonorubyErr::runtimeerr(format!(
                "invalid byte sequence: {:?}",
                slice
            )));
        }
        self.content.extend_from_slice(slice);
        self.cr.set(CodeRange::Unknown);
        Ok(())
    }

    pub fn repeat(&self, len: usize) -> RStringInner {
        let ty = self.ty;
        let vec = self.content.repeat(len);
        RStringInner::from(SmallVec::from_vec(vec), ty)
    }

    /// Replace byte range `start..start+len` with `replacement` bytes.
    /// After replacement, validates encoding consistency.
    pub fn bytesplice(&mut self, start: usize, len: usize, replacement: &[u8]) {
        let end = start + len;
        let new_len = self.content.len() - len + replacement.len();
        if replacement.len() > len {
            // Need to grow: extend first, then shift
            let extra = replacement.len() - len;
            self.content.resize(new_len, 0);
            // Shift tail right
            self.content.copy_within(end..new_len - extra, end + extra);
        } else if replacement.len() < len {
            // Shrink: shift tail left, then truncate
            let shrink = len - replacement.len();
            let old_len = self.content.len();
            self.content.copy_within(end..old_len, end - shrink);
            self.content.truncate(new_len);
        }
        // Copy replacement in
        self.content[start..start + replacement.len()].copy_from_slice(replacement);
        // Re-check encoding
        if self.ty.is_utf8_compatible() {
            if std::str::from_utf8(&self.content).is_err() {
                self.ty = Encoding::Ascii8;
            }
        } else if self.content.is_ascii() || std::str::from_utf8(&self.content).is_ok() {
            // Keep Ascii8
        }
        self.cr.set(CodeRange::Unknown);
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
        assert_eq!(Encoding::try_from_str("ASCII-8BIT").unwrap(), Encoding::Ascii8);
        assert_eq!(Encoding::try_from_str("US-ASCII").unwrap(), Encoding::UsAscii);
        // ISO-8859-N round-trips through `_` and `-` separators.
        assert_eq!(Encoding::try_from_str("ISO-8859-1").unwrap(), Encoding::Iso8859(1));
        assert_eq!(Encoding::try_from_str("ISO_8859_15").unwrap(), Encoding::Iso8859(15));
        assert_eq!(Encoding::try_from_str("LATIN1").unwrap(), Encoding::Iso8859(1));
        // UTF-16 / UTF-32 family.
        assert_eq!(Encoding::try_from_str("UTF-16LE").unwrap(), Encoding::Utf16Le);
        assert_eq!(Encoding::try_from_str("UTF-16BE").unwrap(), Encoding::Utf16Be);
        assert_eq!(Encoding::try_from_str("UTF-32").unwrap(), Encoding::Utf32Le);
        // Japanese.
        assert_eq!(Encoding::try_from_str("EUC-JP").unwrap(), Encoding::EucJp);
        assert_eq!(Encoding::try_from_str("Shift_JIS").unwrap(), Encoding::Sjis(0));
        assert_eq!(Encoding::try_from_str("Windows-31J").unwrap(), Encoding::Sjis(1));
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
        assert_eq!(Encoding::Utf16Le.classify(&[0x61, 0x00, 0x62]), CodeRange::Broken);
        assert_eq!(Encoding::Utf16Be.classify(&[0x00, 0x61]), CodeRange::Valid);
    }

    #[test]
    fn classify_utf32_byte_count_parity() {
        assert_eq!(
            Encoding::Utf32Le.classify(&[0x61, 0x00, 0x00, 0x00]),
            CodeRange::Valid
        );
        assert_eq!(Encoding::Utf32Le.classify(&[0x61, 0x00, 0x00]), CodeRange::Broken);
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
    fn extend_invalidates_cache_for_new_bytes() {
        let mut a = RStringInner::from_encoding(&[0xC3], Encoding::Utf8);
        // 0xC3 alone is a truncated 2-byte UTF-8 scalar → Broken.
        assert_eq!(a.code_range(), CodeRange::Broken);
        let b = RStringInner::from_encoding(&[0xA9], Encoding::Utf8);
        assert_eq!(b.code_range(), CodeRange::Broken);
        a.extend(&b).unwrap();
        // Two broken halves combine into U+00E9 (é) — Valid.
        assert_eq!(a.code_range(), CodeRange::Valid);
        assert!(a.is_valid_encoding());
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
}
