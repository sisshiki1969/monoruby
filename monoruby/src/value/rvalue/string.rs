use super::*;
use smallvec::SmallVec;
use std::cmp::Ordering;

mod printable;

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum Encoding {
    Ascii8,
    Utf8,
}

impl Encoding {
    pub fn from_str(s: &str) -> Result<Self> {
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
#[derive(Debug, Clone)]
pub struct StringInner {
    content: SmallVec<[u8; STRING_INLINE_CAP]>,
    ty: Encoding,
}

impl StringInner {
    pub fn to_string(&self) -> String {
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
                res
            }
            Encoding::Utf8 => match std::str::from_utf8(self) {
                Ok(s) => s.to_string(),
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

impl std::ops::Deref for StringInner {
    type Target = [u8];
    fn deref(&self) -> &Self::Target {
        &self.content
    }
}

impl std::cmp::PartialEq for StringInner {
    fn eq(&self, other: &Self) -> bool {
        self.content == other.content
    }
}

impl std::hash::Hash for StringInner {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.content.hash(state);
    }
}

impl StringInner {
    unsafe fn from(content: SmallVec<[u8; STRING_INLINE_CAP]>, ty: Encoding) -> Self {
        StringInner { content, ty }
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
                self.to_string()
            ))),
        }
    }

    pub fn from_str(s: &str) -> Self {
        unsafe { StringInner::from(SmallVec::from_slice(s.as_bytes()), Encoding::Utf8) }
    }

    pub fn from_string(s: String) -> Self {
        unsafe { StringInner::from(SmallVec::from_vec(s.into_bytes()), Encoding::Utf8) }
    }

    pub fn bytes(slice: &[u8]) -> Self {
        unsafe { StringInner::from(SmallVec::from_slice(slice), Encoding::Ascii8) }
    }

    pub fn bytes_from_vec(vec: Vec<u8>) -> Self {
        unsafe { StringInner::from(SmallVec::from_vec(vec), Encoding::Ascii8) }
    }

    pub fn from_encoding(slice: &[u8], encoding: Encoding) -> Self {
        unsafe { StringInner::from(SmallVec::from_slice(slice), encoding) }
    }

    pub fn string_from_vec(vec: Vec<u8>) -> Self {
        let enc = if std::str::from_utf8(&vec).is_ok() {
            Encoding::Utf8
        } else {
            Encoding::Ascii8
        };
        unsafe { StringInner::from(SmallVec::from_vec(vec), enc) }
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.content
    }

    pub fn length(&self) -> usize {
        match self.ty {
            Encoding::Ascii8 => self.content.len(),
            Encoding::Utf8 => self.check_utf8().unwrap().chars().count(),
        }
    }

    ///
    /// Convert `i` to the position of the char in the string with `len` chars.
    ///
    /// Return None if `i` is out of range.
    ///
    pub fn conv_index(&self, i: i64) -> Option<usize> {
        let len = self.length();
        if i >= 0 {
            if i <= len as i64 {
                Some(i as usize)
            } else {
                None
            }
        } else {
            match len as i64 + i {
                n if n < 0 => None,
                n => Some(n as usize),
            }
        }
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

    pub fn repeat(&self, len: usize) -> StringInner {
        let ty = self.ty;
        let vec = self.content.repeat(len);
        unsafe { StringInner::from(SmallVec::from_vec(vec), ty) }
    }

    pub fn string_cmp(&self, other: &Self) -> Ordering {
        let lhs = self.as_bytes();
        let rhs = other.as_bytes();
        if lhs.len() >= rhs.len() {
            for (r, l) in rhs.iter().zip(lhs.iter()) {
                match l.cmp(r) {
                    Ordering::Equal => {}
                    ord => return ord,
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
                    ord => return ord,
                }
            }
            Ordering::Less
        }
    }

    pub fn ord(&self) -> Result<u32> {
        if self.len() == 0 {
            return Err(MonorubyErr::argumenterr("empty string"));
        }
        let ord = match self.ty {
            Encoding::Ascii8 => self.as_bytes()[0] as u32,
            Encoding::Utf8 => self.check_utf8()?.chars().next().unwrap() as u32,
        };
        Ok(ord as u32)
    }
}
