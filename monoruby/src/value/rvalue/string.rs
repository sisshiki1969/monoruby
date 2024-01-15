use std::borrow::Cow;

use super::*;
use smallvec::SmallVec;
use std::cmp::Ordering;

#[derive(Clone, PartialEq, Hash)]
#[repr(transparent)]
pub struct StringInner(SmallVec<[u8; STRING_INLINE_CAP]>);

impl std::fmt::Display for StringInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", String::from_utf8_lossy(self.as_bytes()))
    }
}

impl std::ops::Deref for StringInner {
    type Target = [u8];
    fn deref(&self) -> &Self::Target {
        self.as_bytes()
    }
}

impl StringInner {
    pub fn from_slice(slice: &[u8]) -> Self {
        StringInner(SmallVec::from_slice(slice))
    }

    pub fn from_vec(vec: Vec<u8>) -> Self {
        StringInner(SmallVec::from_vec(vec))
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_ref()
    }

    pub fn as_str(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(self.as_bytes())
    }

    pub fn extend_from_slice(&mut self, slice: &[u8]) {
        self.0.extend_from_slice(slice);
    }

    pub fn repeat(&mut self, len: usize) -> Vec<u8> {
        self.0.repeat(len)
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
