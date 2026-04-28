use super::*;

/// Inner storage for `Struct` subclass instances. Members are kept in a
/// fixed-size [`Vec<Value>`], indexed by member position (matching the
/// order of the class-level `/members` array). Distinct from CRuby's
/// `RStruct` only in that monoruby always heap-allocates the slot vector
/// — the inline-vs-heap fast path is a future optimisation.
///
/// Created at allocation time (`struct_alloc_func`) with all slots set
/// to `nil`. `Struct#initialize` overwrites them positionally.
#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
pub struct StructInner {
    values: Vec<Value>,
}

impl alloc::GC<RValue> for StructInner {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        for v in &self.values {
            v.mark(alloc);
        }
    }
}

impl StructInner {
    pub fn new(len: usize) -> Self {
        Self {
            values: vec![Value::nil(); len],
        }
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn get(&self, index: usize) -> Value {
        self.values[index]
    }

    pub fn try_get(&self, index: usize) -> Option<Value> {
        self.values.get(index).copied()
    }

    pub fn set(&mut self, index: usize, value: Value) {
        self.values[index] = value;
    }

    pub fn iter(&self) -> impl Iterator<Item = &Value> {
        self.values.iter()
    }

    pub fn values(&self) -> &[Value] {
        &self.values
    }
}
