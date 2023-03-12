use crate::*;
use smallvec::smallvec;
use smallvec::SmallVec;

#[derive(Debug, Clone)]
pub struct ArrayInner(SmallVec<[Value; 4]>);

impl std::ops::Deref for ArrayInner {
    type Target = [Value];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for ArrayInner {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl ArrayInner {
    pub fn new() -> Self {
        ArrayInner(smallvec!())
    }

    pub fn from_vec(v: Vec<Value>) -> Self {
        ArrayInner(SmallVec::from_vec(v))
    }

    pub fn from_iter(iter: impl Iterator<Item = Value>) -> Self {
        ArrayInner(SmallVec::from_iter(iter))
    }

    /*pub fn clear(&mut self) {
        self.0.clear();
    }*/

    pub fn push(&mut self, value: Value) {
        self.0.push(value);
    }

    /*pub fn pop(&mut self) -> Option<Value> {
        self.0.pop()
    }

    pub fn truncate(&mut self, new_len: usize) {
        self.0.truncate(new_len);
    }

    pub fn resize(&mut self, new_len: usize, value: Value) {
        self.0.resize(new_len, value)
    }*/

    pub fn extend(&mut self, iter: impl std::iter::IntoIterator<Item = Value>) {
        self.0.extend(iter);
    }

    pub fn extend_from_slice(&mut self, slice: &[Value]) {
        self.0.extend_from_slice(slice);
    }

    /*pub fn drain(&mut self, range: std::ops::Range<usize>) -> Vec<Value> {
        self.0.drain(range).collect()
    }*/
}

impl ArrayInner {
    pub fn set_index(&mut self, globals: &mut Globals, idx: i64, src: Value) -> Option<Value> {
        if idx >= 0 {
            match self.get_mut(idx as usize) {
                Some(v) => *v = src,
                None => {
                    let idx = idx as usize;
                    self.extend((self.len()..idx).into_iter().map(|_| Value::nil()));
                    self.push(src);
                }
            }
        } else {
            let len = self.len();
            let idx_positive = len as i64 + idx;
            if idx_positive < 0 {
                globals.err_index_too_small(idx, -(len as i64));
                return None;
            } else {
                self[idx_positive as usize] = src;
            }
        };
        Some(src)
    }

    pub fn get_index(&self, idx: i64) -> Option<Value> {
        return Some(if idx >= 0 {
            self.get(idx as usize).cloned().unwrap_or_default()
        } else {
            let idx = self.len() as i64 + idx;
            if idx < 0 {
                Value::nil()
            } else {
                self.get(idx as usize).cloned().unwrap_or_default()
            }
        });
    }

    pub fn get_index_range(&self, globals: &mut Globals, range: &Range) -> Option<Value> {
        let len = self.len() as i64;
        let i_start = match range.start.coerce_to_fixnum(globals)? {
            i if i < 0 => len + i,
            i => i,
        };
        let start = if len < i_start {
            return Some(Value::nil());
        } else if len == i_start {
            return Some(Value::new_empty_array());
        } else {
            i_start as usize
        };
        let i_end = range.end.coerce_to_fixnum(globals)?;
        let end = if i_end >= 0 {
            let end = i_end as usize + if range.exclude_end() { 0 } else { 1 };
            if self.len() < end {
                self.len()
            } else {
                end
            }
        } else {
            (len + i_end + if range.exclude_end() { 0 } else { 1 }) as usize
        };
        if start >= end {
            return Some(Value::new_empty_array());
        }
        Some(Value::new_array_from_iter(self[start..end].iter().cloned()))
    }
}
