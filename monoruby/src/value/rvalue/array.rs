use crate::*;
use smallvec::smallvec;
use smallvec::Drain;
use smallvec::SmallVec;

pub const ARRAY_INLINE_CAPA: usize = 5;

#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub struct Array(Value);

impl std::ops::Deref for Array {
    type Target = ArrayInner;
    fn deref(&self) -> &Self::Target {
        self.0.as_array()
    }
}

impl std::ops::DerefMut for Array {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.as_array_mut()
    }
}

impl std::convert::From<Value> for Array {
    fn from(v: Value) -> Self {
        assert_eq!(ObjKind::ARRAY, v.rvalue().kind());
        Array(v)
    }
}

impl std::convert::Into<Value> for Array {
    fn into(self) -> Value {
        self.0
    }
}

impl alloc::GC<RValue> for Array {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.0.mark(alloc)
    }
}

impl Array {
    pub fn peel(self) -> Value {
        if self.len() == 0 {
            Value::nil()
        } else if self.len() == 1 {
            self[0]
        } else {
            self.into()
        }
    }
}

#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct ArrayInner(SmallVec<[Value; ARRAY_INLINE_CAPA]>);

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

    pub fn from(smallvec: SmallVec<[Value; 5]>) -> Self {
        ArrayInner(smallvec)
    }

    pub fn from_vec(v: Vec<Value>) -> Self {
        ArrayInner(SmallVec::from_vec(v))
    }

    pub fn from_iter(iter: impl Iterator<Item = Value>) -> Self {
        ArrayInner(SmallVec::from_iter(iter))
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn push(&mut self, value: Value) {
        self.0.push(value);
    }

    /*pub fn pop(&mut self) -> Option<Value> {
        self.0.pop()
    }*/

    pub fn truncate(&mut self, new_len: usize) {
        self.0.truncate(new_len);
    }

    pub fn resize(&mut self, new_len: usize, value: Value) {
        self.0.resize(new_len, value)
    }

    pub fn fill(&mut self, value: Value) {
        self.0.fill(value);
    }

    /*pub fn extend(&mut self, iter: impl std::iter::IntoIterator<Item = Value>) {
        self.0.extend(iter);
    }*/

    pub fn extend_from_slice(&mut self, slice: &[Value]) {
        self.0.extend_from_slice(slice);
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

    pub fn to_s(&self, globals: &Globals) -> String {
        match self.len() {
            0 => "[]".to_string(),
            1 => format!("[{}]", globals.inspect(self[0])),
            _ => {
                let mut s = format!("[{}", globals.inspect(self[0]));
                for val in self[1..].iter() {
                    s += &format!(", {}", globals.inspect(*val));
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
        match val.is_array() {
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

    pub(crate) fn get_index(&self, idx: i64) -> Option<Value> {
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

    /// Calculate array index.
    /// if `index` is a zero or positeve integer, return `index`.
    /// Else, return `len` + `index.`
    fn get_array_index(&self, index: i64) -> Option<usize> {
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

    pub(crate) fn get_elem2(
        &self,
        globals: &mut Globals,
        arg0: Value,
        arg1: Value,
    ) -> Result<Value> {
        let index = arg0.coerce_to_i64(globals)?;
        let self_len = self.len();
        let index = self.get_array_index(index).unwrap_or(self_len);
        let len = arg1.coerce_to_i64(globals)?;
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

    pub(crate) fn get_elem1(&self, globals: &mut Globals, idx: Value) -> Result<Value> {
        if let Some(range) = idx.is_range() {
            let len = self.len() as i64;
            let i_start = match range.start.coerce_to_i64(globals)? {
                i if i < 0 => len + i,
                i => i,
            };
            let start = if len < i_start {
                return Ok(Value::nil());
            } else if len == i_start {
                return Ok(Value::array_empty());
            } else {
                i_start as usize
            };
            let i_end = range.end.coerce_to_i64(globals)?;
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
            let index = idx.coerce_to_i64(globals)?;
            let self_len = self.len();
            let index = self.get_array_index(index).unwrap_or(self_len);
            let val = self.get(index).cloned().unwrap_or_default();
            Ok(val)
        }
    }

    /*pub(crate) fn set_elem(&mut self, args: &[Value]) -> VMResult {
        let val = if args.len() == 3 { args[2] } else { args[1] };
        if args.len() == 2 {
            self.set_elem1(args[0], args[1])
        } else {
            let index = args[0].coerce_to_fixnum("Index")?;
            let index = self.get_array_index(index)?;
            let length = args[1].coerce_to_fixnum("Length")?;
            if length < 0 {
                return Err(RubyError::index(format!("Negative length. {}", length)));
            };
            self.set_elem2(index, length as usize, val)
        }
    }

    pub(crate) fn set_elem1(&mut self, idx: Value, val: Value) -> VMResult {
        if let Some(index) = idx.as_fixnum() {
            if index >= 0 {
                self.set_elem_imm(index as usize, val);
            } else {
                let index = self.get_array_index(index)?;
                self[index] = val;
            }
            Ok(val)
        } else if let Some(range) = idx.as_range() {
            let first = {
                let i = range.start.coerce_to_fixnum("Start of the range")?;
                self.get_array_index(i)?
            };
            let last = {
                let i = range.end.coerce_to_fixnum("End of the range")?;
                self.get_array_index(i)? + if range.exclude { 0 } else { 1 }
            };
            if last < first {
                self.set_elem2(first, 0, val)
            } else {
                let length = last - first;
                self.set_elem2(first, length, val)
            }
        } else {
            Err(VMError::no_implicit_conv(idx, "Integer or Range"))
        }
    }*/
}
