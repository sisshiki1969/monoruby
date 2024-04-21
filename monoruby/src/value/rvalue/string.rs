use super::*;
use smallvec::SmallVec;
use std::cmp::Ordering;

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum Encoding {
    Ascii8,
    Utf8,
}

#[derive(Debug, Clone)]
//#[repr(transparent)]
pub struct StringInner {
    content: SmallVec<[u8; STRING_INLINE_CAP]>,
    ty: Encoding,
}

impl std::fmt::Display for StringInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ty {
            Encoding::Ascii8 => {
                for c in self.as_bytes() {
                    if c.is_ascii() {
                        write!(f, "{}", *c as char)?;
                    } else {
                        write!(f, r#"\x{:0>2X}"#, c)?;
                    }
                }
                Ok(())
            }
            Encoding::Utf8 => write!(f, "{}", String::from_utf8_lossy(self.as_bytes())),
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
    fn from(content: SmallVec<[u8; STRING_INLINE_CAP]>, ty: Encoding) -> Self {
        StringInner { content, ty }
    }

    pub fn encoding(&self) -> Encoding {
        self.ty
    }

    pub fn set_encoding(&mut self, ty: Encoding) {
        self.ty = ty;
    }

    pub fn from_str(s: &str) -> Self {
        StringInner::from(SmallVec::from_slice(s.as_bytes()), Encoding::Utf8)
    }

    pub fn from_string(s: String) -> Self {
        StringInner::from(SmallVec::from_vec(s.into_bytes()), Encoding::Utf8)
    }

    pub fn string_from_bytes(slice: &[u8]) -> Self {
        StringInner::from(SmallVec::from_slice(slice), Encoding::Utf8)
    }

    pub fn from_vec(vec: Vec<u8>) -> Self {
        StringInner::from(SmallVec::from_vec(vec), Encoding::Utf8)
    }

    pub fn from_vec_with_encoding(vec: Vec<u8>, ty: Encoding) -> Self {
        StringInner::from(SmallVec::from_vec(vec), ty)
    }

    pub fn bytes(slice: &[u8]) -> Self {
        StringInner::from(SmallVec::from_slice(slice), Encoding::Ascii8)
    }

    pub fn string_from_vec(vec: Vec<u8>) -> Self {
        let enc = if std::str::from_utf8(&vec).is_ok() {
            Encoding::Utf8
        } else {
            Encoding::Ascii8
        };
        StringInner::from(SmallVec::from_vec(vec), enc)
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.content
    }

    pub fn as_str(&self) -> &str {
        std::str::from_utf8(self.as_bytes()).unwrap()
    }

    pub fn extend(&mut self, other: &Self) -> Result<()> {
        if self.is_empty() {
            self.content.extend_from_slice(other);
            self.ty = other.ty;
            return Ok(());
        }
        match (self.ty, other.ty) {
            (Encoding::Utf8, Encoding::Utf8) | (Encoding::Ascii8, Encoding::Ascii8) => {
                self.content.extend_from_slice(other);
                return Ok(());
            }
            (Encoding::Utf8, Encoding::Ascii8) => {
                if other.content.is_ascii() {
                    self.content.extend_from_slice(other);
                    return Ok(());
                }
            }
            (Encoding::Ascii8, Encoding::Utf8) => {
                if other.content.is_ascii() {
                    self.content.extend_from_slice(other);
                    return Ok(());
                }
            }
        }
        return Err(MonorubyErr::runtimeerr(format!(
            "incompatible character encodings: {:?} and {:?}",
            self.ty, other.ty
        )));
    }

    pub fn extend_from_slice(&mut self, slice: &[u8]) {
        self.content.extend_from_slice(slice);
    }

    pub fn repeat(&self, len: usize) -> StringInner {
        let ty = self.ty;
        StringInner::from_vec_with_encoding(self.content.repeat(len), ty)
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
        let ord = match std::str::from_utf8(self.as_bytes()) {
            Ok(s) => s.chars().next().unwrap() as u32,
            Err(_) => self.as_bytes()[0] as u32,
        };
        Ok(ord as u32)
    }
}
