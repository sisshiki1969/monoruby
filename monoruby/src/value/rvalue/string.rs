use super::*;
use smallvec::SmallVec;
use std::cmp::Ordering;

pub mod pack;
mod printable;

#[monoruby_object]
pub struct RString(Value);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Encoding {
    Ascii8,
    Utf8,
    UsAscii,
}

impl Encoding {
    /// Returns true if the encoding is UTF-8 compatible (UTF-8 or US-ASCII).
    pub fn is_utf8_compatible(self) -> bool {
        matches!(self, Encoding::Utf8 | Encoding::UsAscii)
    }

    pub fn try_from_str(s: &str) -> Result<Self> {
        // Normalize: uppercase, replace '-' with '_'
        let normalized = s.to_uppercase().replace('-', "_");
        match normalized.as_str() {
            // UTF-8
            "UTF_8" | "UTF8" | "CP65001" => Ok(Encoding::Utf8),

            // ASCII-8BIT / BINARY
            "ASCII_8BIT" | "BINARY" => Ok(Encoding::Ascii8),

            // US-ASCII
            "US_ASCII" | "ASCII" | "ANSI_X3.4_1968" | "646" => Ok(Encoding::UsAscii),

            // Special pseudo-encoding names
            "LOCALE" | "EXTERNAL" | "FILESYSTEM" => Ok(Encoding::Utf8),

            // UTF-16/32 variants (accept but map to UTF-8 internally)
            "UTF_16" | "UTF_16BE" | "UTF_16LE" | "UTF_32" | "UTF_32BE" | "UTF_32LE" => {
                Ok(Encoding::Utf8)
            }

            // ISO-8859 family (map to ASCII-8BIT as they are byte-oriented)
            "ISO_8859_1" | "ISO8859_1" | "LATIN1" | "ISO_8859_2" | "ISO8859_2" | "LATIN2"
            | "ISO_8859_3" | "ISO8859_3" | "LATIN3" | "ISO_8859_4" | "ISO8859_4" | "LATIN4"
            | "ISO_8859_5" | "ISO8859_5" | "ISO_8859_6" | "ISO8859_6" | "ISO_8859_7"
            | "ISO8859_7" | "ISO_8859_8" | "ISO8859_8" | "ISO_8859_9" | "ISO8859_9"
            | "LATIN5" | "ISO_8859_10" | "ISO8859_10" | "LATIN6" | "ISO_8859_11"
            | "ISO8859_11" | "ISO_8859_13" | "ISO8859_13" | "LATIN7" | "ISO_8859_14"
            | "ISO8859_14" | "LATIN8" | "ISO_8859_15" | "ISO8859_15" | "LATIN9"
            | "ISO_8859_16" | "ISO8859_16" | "LATIN10" => Ok(Encoding::Ascii8),

            // Japanese encodings
            "EUC_JP" | "EUCJP" | "SHIFT_JIS" | "SJIS" | "ISO_2022_JP" | "ISO2022_JP"
            | "WINDOWS_31J" | "CP932" | "CSWINDOWS31J" | "WINDOWS31J" | "MACJAPANESE"
            | "MACJAPAN" | "EUCJP_MS" | "EUCJP_WIN" | "CP51932"
            | "STATELESS_ISO_2022_JP" => Ok(Encoding::Ascii8),

            // Windows code pages
            "WINDOWS_1250" | "CP1250" | "WINDOWS_1251" | "CP1251" | "WINDOWS_1252" | "CP1252"
            | "WINDOWS_1253" | "CP1253" | "WINDOWS_1254" | "CP1254" | "WINDOWS_1255"
            | "CP1255" | "WINDOWS_1256" | "CP1256" | "WINDOWS_1257" | "CP1257"
            | "WINDOWS_1258" | "CP1258" => Ok(Encoding::Ascii8),

            // IBM code pages
            "IBM437" | "CP437" | "IBM737" | "CP737" | "IBM775" | "CP775" | "IBM850" | "CP850"
            | "IBM852" | "CP852" | "IBM855" | "CP855" | "IBM857" | "CP857" | "IBM860"
            | "CP860" | "IBM861" | "CP861" | "IBM862" | "CP862" | "IBM863" | "CP863"
            | "IBM864" | "CP864" | "IBM865" | "CP865" | "IBM866" | "CP866" | "IBM869"
            | "CP869" => Ok(Encoding::Ascii8),

            // KOI8
            "KOI8_R" | "KOI8_U" => Ok(Encoding::Ascii8),

            // Chinese encodings
            "GB2312" | "EUC_CN" | "GBK" | "CP936" | "GB18030" | "BIG5"
            | "BIG5_HKSCS" => Ok(Encoding::Ascii8),

            // Korean encodings
            "EUC_KR" | "EUCKR" | "CP949" => Ok(Encoding::Ascii8),

            // Other
            "EUC_TW" | "EUCTW" | "TIS_620" | "TIS620" | "CESU_8" | "CESU8" => {
                Ok(Encoding::Ascii8)
            }

            _ => Err(MonorubyErr::argumenterr(format!(
                "unknown encoding name - {s}"
            ))),
        }
    }
}

///
/// Ruby-level String.
///
/// This struct is used to represent a Ruby-level String.
/// if ty is Utf8, content is guaranteed to be a valid utf8 string.
///
#[derive(Debug, Clone, Eq)]
pub struct RStringInner {
    content: SmallVec<[u8; STRING_INLINE_CAP]>,
    ty: Encoding,
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
        match self.ty {
            Encoding::Ascii8 => {
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
            Encoding::Utf8 | Encoding::UsAscii => match std::str::from_utf8(self) {
                Ok(s) => Ok(std::borrow::Cow::Borrowed(s)),
                Err(err) => Err(MonorubyErr::runtimeerr(format!(
                    "invalid byte sequence: {} {}",
                    err,
                    String::from_utf8_lossy(&self.content)
                ))),
            },
        }
    }

    pub fn dump(&self) -> String {
        match self.ty {
            Encoding::Ascii8 => {
                let mut res = String::with_capacity(self.len());
                for c in self.as_bytes() {
                    ascii_escape(&mut res, *c);
                }
                res
            }
            Encoding::Utf8 | Encoding::UsAscii => {
                let mut res = String::with_capacity(self.len());
                utf8_escape_bytes(&mut res, self.as_bytes(), utf8_escape);
                res
            }
        }
    }

    pub fn inspect(&self) -> String {
        match self.ty {
            Encoding::Ascii8 => {
                let mut res = String::with_capacity(self.len());
                for c in self.as_bytes() {
                    ascii_escape(&mut res, *c);
                }
                res
            }
            Encoding::Utf8 | Encoding::UsAscii => {
                let mut res = String::with_capacity(self.len());
                utf8_escape_bytes(&mut res, self.as_bytes(), utf8_inspect);
                res
            }
        }
    }

    pub fn valid(&self) -> bool {
        match self.ty {
            Encoding::Ascii8 => true,
            Encoding::Utf8 | Encoding::UsAscii => std::str::from_utf8(self).is_ok(),
        }
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
        RStringInner { content, ty }
    }

    pub fn encoding(&self) -> Encoding {
        self.ty
    }

    pub fn set_encoding(&mut self, ty: Encoding) {
        self.ty = ty;
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
        self.ty = Encoding::Ascii8;
    }

    ///
    /// Get the length in char of the string `self`.
    ///
    pub fn char_length(&self) -> Result<usize> {
        let len = match self.ty {
            Encoding::Ascii8 => self.content.len(),
            Encoding::Utf8 | Encoding::UsAscii => self.check_utf8()?.chars().count(),
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
        match self.ty {
            Encoding::Ascii8 => {
                if self.len() <= index {
                    0..0
                } else if self.len() <= index + len {
                    index..self.len()
                } else {
                    index..index + len
                }
            }
            Encoding::Utf8 | Encoding::UsAscii => match std::str::from_utf8(self) {
                Ok(s) => {
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
                    start..end
                }
                Err(_) => {
                    // Fall back to byte-based indexing for invalid UTF-8
                    if self.len() <= index {
                        0..0
                    } else if self.len() <= index + len {
                        index..self.len()
                    } else {
                        index..index + len
                    }
                }
            }
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
    }

    pub fn first_code(&self) -> Result<u32> {
        if self.len() == 0 {
            return Err(MonorubyErr::argumenterr("empty string"));
        }
        let ord = match self.ty {
            Encoding::Ascii8 => self.as_bytes()[0] as u32,
            Encoding::Utf8 | Encoding::UsAscii => self.check_utf8()?.chars().next().unwrap() as u32,
        };
        Ok(ord)
    }
}
