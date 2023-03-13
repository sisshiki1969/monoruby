use std::borrow::Cow;

use crate::*;
use smallvec::SmallVec;

#[derive(Clone)]
#[repr(transparent)]
pub struct StringInner(SmallVec<[u8; STRING_INLINE_CAP]>);

impl std::fmt::Display for StringInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", String::from_utf8_lossy(self.as_bytes()))
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

    /*pub fn to_string(&self) -> String {
        String::from_utf8_lossy(self.as_bytes()).to_string()
    }*/
}
