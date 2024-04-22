use super::*;
use smallvec::SmallVec;
use std::cmp::Ordering;

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum Encoding {
    Ascii8,
    Utf8,
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
    pub fn to_string(&self) -> Result<String> {
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
                Ok(res)
            }
            Encoding::Utf8 => match std::str::from_utf8(self) {
                Ok(s) => Ok(s.to_string()),
                Err(err) => {
                    eprintln!("invalid byte sequence: {:?}", err);
                    let s = String::from_utf8_lossy(self).to_string();
                    //s
                    Err(MonorubyErr::runtimeerr(format!(
                        "invalid byte sequence: {s}",
                    )))
                }
            },
        }
    }
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

    pub fn check(&self) -> Result<&str> {
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

    pub fn bytes_from_vec(vec: Vec<u8>) -> Self {
        unsafe { StringInner::from(SmallVec::from_vec(vec), Encoding::Ascii8) }
    }

    pub fn bytes(slice: &[u8]) -> Self {
        unsafe { StringInner::from(SmallVec::from_slice(slice), Encoding::Ascii8) }
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

    pub fn extend(&mut self, other: &Self) -> Result<()> {
        if self.is_empty() {
            self.content.extend_from_slice(other);
            self.ty = other.ty;
            return Ok(());
        }
        if self.ty == other.ty {
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
            Encoding::Utf8 => self.check()?.chars().next().unwrap() as u32,
        };
        Ok(ord as u32)
    }
}
