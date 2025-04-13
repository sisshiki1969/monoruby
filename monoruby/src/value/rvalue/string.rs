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
}

impl Encoding {
    pub fn try_from_str(s: &str) -> Result<Self> {
        match s.to_uppercase().as_str() {
            "ASCII-8BIT" => Ok(Encoding::Ascii8),
            "UTF-8" => Ok(Encoding::Utf8),
            _ => Err(MonorubyErr::argumenterr(format!(
                "Unknown encoding name - {s}"
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
    pub fn to_str(&self) -> Result<std::borrow::Cow<str>> {
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
            Encoding::Utf8 => match std::str::from_utf8(self) {
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
            Encoding::Utf8 => match std::str::from_utf8(self) {
                Ok(s) => {
                    let mut res = String::with_capacity(self.len());
                    for c in s.chars() {
                        utf8_escape(&mut res, c);
                    }
                    res
                }
                Err(err) => {
                    let s = String::from_utf8_lossy(self).to_string();
                    panic!("invalid byte sequence: {s} {err}");
                    //Err(MonorubyErr::runtimeerr(format!(
                    //    "invalid byte sequence: {s}",
                    //)))
                }
            },
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
            Encoding::Utf8 => match std::str::from_utf8(self) {
                Ok(s) => {
                    let mut res = String::with_capacity(self.len());
                    for c in s.chars() {
                        utf8_inspect(&mut res, c);
                    }
                    res
                }
                Err(err) => {
                    let s = String::from_utf8_lossy(self).to_string();
                    panic!("invalid byte sequence: {s} {err}");
                    //Err(MonorubyErr::runtimeerr(format!(
                    //    "invalid byte sequence: {s}",
                    //)))
                }
            },
        }
    }

    pub fn valid(&self) -> bool {
        match self.ty {
            Encoding::Ascii8 => true,
            Encoding::Utf8 => std::str::from_utf8(self).is_ok(),
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
    if ch as u32 <= 0xff {
        ascii_escape(s, ch as u8);
    } else if printable::is_printable(ch) {
        s.push(ch);
    } else {
        s.push_str(&format!("\\u{:0>4X}", ch as u32));
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
            Err(_) => Err(MonorubyErr::runtimeerr(format!(
                "invalid byte sequence. {:?}",
                self.to_str()
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

    ///
    /// Get the length in char of the string `self`.
    ///
    pub fn char_length(&self) -> Result<usize> {
        let len = match self.ty {
            Encoding::Ascii8 => self.content.len(),
            Encoding::Utf8 => self.check_utf8()?.chars().count(),
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
            Encoding::Utf8 => {
                let s = self.check_utf8().unwrap();
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
        }
    }

    pub fn extend(&mut self, other: &Self) -> Result<()> {
        if self.is_empty() {
            self.content.extend_from_slice(other);
            self.ty = other.ty;
            return Ok(());
        }
        if self.ty == other.ty || other.is_ascii() {
            self.content.extend_from_slice(other);
            Ok(())
        } else {
            Err(MonorubyErr::runtimeerr(format!(
                "incompatible character encodings: {:?} and {:?}",
                self.ty, other.ty
            )))
        }
    }

    pub fn extend_from_slice_checked(&mut self, slice: &[u8]) -> Result<()> {
        if self.ty == Encoding::Utf8 && std::str::from_utf8(slice).is_err() {
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

    pub fn first_code(&self) -> Result<u32> {
        if self.len() == 0 {
            return Err(MonorubyErr::argumenterr("empty string"));
        }
        let ord = match self.ty {
            Encoding::Ascii8 => self.as_bytes()[0] as u32,
            Encoding::Utf8 => self.check_utf8()?.chars().next().unwrap() as u32,
        };
        Ok(ord)
    }
}
