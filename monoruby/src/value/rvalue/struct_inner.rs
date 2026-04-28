use super::*;
use smallvec::{SmallVec, smallvec};

/// Number of slots stored INLINE in the [`StructInner`] (i.e. in the
/// RValue's `kind` union, no heap allocation). Must equal
/// [`ARRAY_INLINE_CAPA`] so the
/// `RVALUE_OFFSET_INLINE` / `RVALUE_OFFSET_ARY_CAPA` / `RVALUE_OFFSET_HEAP_PTR`
/// constants — computed by the vendored `smallvec` crate for
/// `SmallVec<[u64; 5]>` — apply identically here. SmallVec layout
/// depends only on the element-array size, and `Value` is the same
/// size as `u64`, so the constants are interchangeable.
pub const STRUCT_INLINE_SLOTS: usize = ARRAY_INLINE_CAPA;
const _: () = assert!(STRUCT_INLINE_SLOTS == 5);

/// Inner storage for `Struct` subclass instances. Slots live in a
/// fixed-size [`SmallVec`] so up to [`STRUCT_INLINE_SLOTS`] members
/// are stored inline (no heap allocation). Larger structs spill to
/// the heap, with the same layout `ArrayInner` uses for arrays.
///
/// Indexed by member position (matching `/members` order). All slots
/// are initialised to `Value::nil()` by `struct_alloc_func`;
/// `Struct#initialize` overwrites them positionally.
///
/// The struct is `repr(transparent)` over the SmallVec so the JIT can
/// reuse the existing `RVALUE_OFFSET_*` constants.
#[derive(Debug, Clone, PartialEq)]
#[repr(transparent)]
pub struct StructInner(SmallVec<[Value; STRUCT_INLINE_SLOTS]>);

impl alloc::GC<RValue> for StructInner {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        for v in self.0.iter() {
            v.mark(alloc);
        }
    }
}

impl StructInner {
    pub fn new(len: usize) -> Self {
        StructInner(smallvec![Value::nil(); len])
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get(&self, index: usize) -> Value {
        self.0[index]
    }

    pub fn try_get(&self, index: usize) -> Option<Value> {
        self.0.get(index).copied()
    }

    pub fn set(&mut self, index: usize, value: Value) {
        self.0[index] = value;
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Value> {
        self.0.iter()
    }

    pub fn values(&self) -> &[Value] {
        &self.0
    }

    /// True if the slot vector spilled to the heap (member count
    /// exceeded [`STRUCT_INLINE_SLOTS`]). The JIT call-site path uses
    /// this to decide between the inline and heap codegen variants
    /// per receiver class.
    pub fn is_heap(&self) -> bool {
        self.0.spilled()
    }
}
