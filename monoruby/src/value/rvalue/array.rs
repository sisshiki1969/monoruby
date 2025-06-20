use super::*;
use rand::seq::SliceRandom;
use smallvec::smallvec;
use smallvec::Drain;
use smallvec::SmallVec;

pub const ARRAY_INLINE_CAPA: usize = 5;

#[monoruby_object]
pub struct Array(Value);

impl Default for Array {
    fn default() -> Self {
        Self::new_empty()
    }
}

impl Array {
    pub fn id(self) -> u64 {
        self.0.id()
    }

    pub fn peel(self) -> Value {
        if self.len() == 0 {
            Value::nil()
        } else if self.len() == 1 {
            self[0]
        } else {
            self.into()
        }
    }

    pub fn new_empty() -> Self {
        Self(Value::array_empty())
    }

    pub fn new1(v1: Value) -> Self {
        Self(Value::array1(v1))
    }

    pub fn new2(v1: Value, v2: Value) -> Self {
        Self(Value::array2(v1, v2))
    }

    pub fn new_from_vec(v: Vec<Value>) -> Self {
        Self(Value::array_from_vec(v))
    }

    ///
    /// Uniquify `self`.
    ///
    /// Always returns Ok(removed: bool).
    ///
    /// If some elements were removed, returns Ok(true).
    ///
    pub fn uniq(&mut self) -> Result<bool> {
        let mut h = HashSet::default();
        let mut recursive = false;
        let self_id = self.id();
        self.retain(|x| {
            if self_id == x.id() {
                if !recursive {
                    recursive = true;
                    Ok(true)
                } else {
                    Ok(false)
                }
            } else {
                Ok(h.insert(HashKey(*x)))
            }
        })
        .map(|removed| removed.is_some())
    }
}

#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct ArrayInner(SmallVec<[Value; ARRAY_INLINE_CAPA]>);

impl Default for ArrayInner {
    fn default() -> Self {
        Self::new()
    }
}

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

impl std::iter::FromIterator<Value> for ArrayInner {
    fn from_iter<I: IntoIterator<Item = Value>>(iter: I) -> Self {
        ArrayInner(SmallVec::from_iter(iter))
    }
}

impl ArrayInner {
    pub fn new() -> Self {
        ArrayInner(smallvec!())
    }

    pub fn with_capacity(len: usize) -> Self {
        ArrayInner(SmallVec::with_capacity(len))
    }

    pub fn from(smallvec: SmallVec<[Value; 5]>) -> Self {
        ArrayInner(smallvec)
    }

    pub fn from_vec(v: Vec<Value>) -> Self {
        ArrayInner(SmallVec::from_vec(v))
    }

    /*pub fn from_iter(iter: impl Iterator<Item = Value>) -> Self {
        ArrayInner(SmallVec::from_iter(iter))
    }*/

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn replace(&mut self, v: Vec<Value>) {
        self.0 = SmallVec::from_vec(v);
    }

    pub fn push(&mut self, value: Value) {
        self.0.push(value);
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.0.pop()
    }

    pub fn truncate(&mut self, new_len: usize) {
        self.0.truncate(new_len);
    }

    pub fn resize(&mut self, new_len: usize, value: Value) {
        self.0.resize(new_len, value)
    }

    pub fn fill(&mut self, value: Value) {
        self.0.fill(value);
    }

    pub fn extend(&mut self, iter: impl std::iter::IntoIterator<Item = Value>) {
        self.0.extend(iter);
    }

    pub fn extend_from_slice(&mut self, slice: &[Value]) {
        self.0.extend_from_slice(slice);
    }

    pub fn insert(&mut self, index: usize, element: Value) {
        self.0.insert(index, element)
    }

    pub fn insert_many(&mut self, index: usize, iterable: impl IntoIterator<Item = Value>) {
        self.0.insert_many(index, iterable)
    }

    pub fn drain(
        &mut self,
        range: std::ops::Range<usize>,
    ) -> Drain<'_, [Value; ARRAY_INLINE_CAPA]> {
        self.0.drain(range)
    }

    ///
    /// Retains only elements which f(elem) returns true.
    ///
    /// Returns Some(last_value) when one or some elements were removed.
    pub fn retain<F>(&mut self, mut f: F) -> Result<Option<Value>>
    where
        F: FnMut(&Value) -> Result<bool>,
    {
        let len = self.len();
        let mut del = 0;
        let mut removed = None;
        {
            let v = &mut **self;
            for i in 0..len {
                if !f(&v[i])? {
                    removed = Some(v[i]);
                    del += 1;
                } else if del > 0 {
                    v.swap(i - del, i);
                }
            }
        }
        if del > 0 {
            self.truncate(len - del);
        }
        Ok(removed)
    }

    pub fn remove(&mut self, index: usize) -> Value {
        self.0.remove(index)
    }

    pub fn debug(&self, store: &Store) -> String {
        match self.len() {
            0 => "[]".to_string(),
            1 => format!("[{}]", self[0].debug(store)),
            i => {
                let mut s = format!("[{}", self[0].debug(store));
                for val in self[1..].iter().take(3) {
                    s += &format!(", {}", val.debug(store));
                }
                s += if i > 3 { " .. ]" } else { "]" };
                s
            }
        }
    }

    pub fn to_s(&self, store: &Store) -> String {
        match self.len() {
            0 => "[]".to_string(),
            1 => format!("[{}]", self[0].inspect(store)),
            _ => {
                let mut s = format!("[{}", self[0].inspect(store));
                for val in self[1..].iter() {
                    s += &format!(", {}", val.inspect(store));
                }
                s += "]";
                s
            }
        }
    }
}

impl ArrayInner {
    pub(crate) fn set_index(&mut self, idx: i64, src: Value) -> Result<Value> {
        if idx >= 0 {
            match self.get_mut(idx as usize) {
                Some(v) => *v = src,
                None => {
                    let idx = idx as usize;
                    self.resize(idx, Value::nil());
                    self.push(src);
                }
            }
        } else {
            match self.get_array_index(idx) {
                Some(i) => self[i] = src,
                None => return Err(MonorubyErr::index_too_small(idx, -(self.len() as i64))),
            };
        }
        Ok(src)
    }

    pub(crate) fn set_index2(&mut self, index: usize, length: usize, val: Value) -> Result<Value> {
        let len = self.len();
        match val.try_array_ty() {
            Some(ary) => {
                // if self = ary, something wrong happens..
                let ary_len = ary.len();
                if index >= len || index + length > len {
                    self.resize(index + ary_len, Value::nil());
                } else if ary_len > length {
                    // possibly self == ary
                    self.resize(len + ary_len - length, Value::nil());
                    self.copy_within(index + length..len, index + ary_len);
                } else {
                    // self != ary
                    self.copy_within(index + length..len, index + ary_len);
                    self.resize(len + ary_len - length, Value::nil());
                }
                self[index..index + ary_len].copy_from_slice(&ary[0..ary_len]);
            }
            None => {
                if index >= len {
                    self.resize(index + 1, Value::nil());
                } else if length == 0 {
                    self.push(Value::nil());
                    self.copy_within(index..len, index + 1);
                } else {
                    let end = index + length;
                    if end < len {
                        self.copy_within(end..len, index + 1);
                        self.truncate(len + 1 - length);
                    } else {
                        self.truncate(index + 1);
                    }
                }
                self[index] = val;
            }
        };
        Ok(val)
    }

    /// Calculate array index.
    ///
    /// if `index` is a zero or positeve integer, return `index`.
    /// Else, return `len` + `index.`
    pub(crate) fn get_array_index(&self, index: i64) -> Option<usize> {
        if index < 0 {
            let i = self.len() as i64 + index;
            if i < 0 {
                return None;
            };
            Some(i as usize)
        } else {
            Some(index as usize)
        }
    }

    pub(crate) fn get_array_index_checked(&self, store: &Store, index: Value) -> Result<usize> {
        let index = index.coerce_to_i64(store)?;
        match self.get_array_index(index) {
            Some(i) => Ok(i),
            None => Err(MonorubyErr::index_too_small(index, -(self.len() as i64))),
        }
    }

    pub(crate) fn get_elem2(&self, store: &Store, arg0: Value, arg1: Value) -> Result<Value> {
        let index = arg0.coerce_to_i64(store)?;
        let self_len = self.len();
        let index = self.get_array_index(index).unwrap_or(self_len);
        let len = arg1.coerce_to_i64(store)?;
        let val = if len < 0 || index > self_len {
            Value::nil()
        } else if index == self_len {
            Value::array_empty()
        } else {
            let len = len as usize;
            let start = index;
            let end = std::cmp::min(self_len, start + len);
            Value::array_from_iter(self[start..end].iter().cloned())
        };
        Ok(val)
    }

    pub(crate) fn get_elem1(&self, store: &Store, idx: Value) -> Result<Value> {
        if let Some(range) = idx.is_range() {
            let len = self.len() as i64;
            let i_start = match range.start().coerce_to_i64(store)? {
                i if i < 0 => len + i,
                i => i,
            };
            let start = match len {
                i if i == i_start => return Ok(Value::array_empty()),
                i if i < i_start => return Ok(Value::nil()),
                _ => i_start as usize,
            };

            let i_end = range.end().coerce_to_i64(store)?;
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
                return Ok(Value::array_empty());
            }
            Ok(Value::array_from_iter(self[start..end].iter().cloned()))
        } else {
            let index = idx.coerce_to_i64(store)?;
            let self_len = self.len();
            let index = self.get_array_index(index).unwrap_or(self_len);
            let val = self.get(index).cloned().unwrap_or_default();
            Ok(val)
        }
    }

    pub(crate) fn shuffle<R>(&mut self, rng: &mut R)
    where
        R: rand::Rng + ?Sized,
    {
        self.0.shuffle(rng);
    }
}
