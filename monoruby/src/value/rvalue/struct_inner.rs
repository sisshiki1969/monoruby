use super::*;

/// Inner storage for `Struct` subclass instances. Members are stored
/// in a heap-allocated array of [`Value`]s, indexed by member position
/// (matching the order of the class-level `/members` array).
///
/// The struct is `repr(C)` with an explicit field order so the JIT can
/// load the slot pointer with a single `mov [rdi + STRUCT_PTR]` after
/// dereferencing the `Box<StructInner>` from the RValue's `kind`.
/// `STRUCT_PTR` is exposed as a const for codegen.
///
/// Created at allocation time (`struct_alloc_func`) with all slots set
/// to `nil`; `Struct#initialize` overwrites them positionally.
#[repr(C)]
pub struct StructInner {
    /// Heap pointer to a `len`-element array of [`Value`]s. ALWAYS the
    /// first field — the JIT relies on offset 0.
    ptr: std::ptr::NonNull<Value>,
    len: usize,
}

/// Byte offset of `StructInner.ptr` within `StructInner`. JIT uses
/// this when emitting struct slot loads/stores.
pub const STRUCT_INNER_PTR_OFFSET: usize = std::mem::offset_of!(StructInner, ptr);

impl std::fmt::Debug for StructInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StructInner")
            .field("len", &self.len)
            .field("values", &self.values())
            .finish()
    }
}

impl Clone for StructInner {
    fn clone(&self) -> Self {
        let mut s = StructInner::new(self.len);
        for (i, v) in self.values().iter().enumerate() {
            s.set(i, *v);
        }
        s
    }
}

impl PartialEq for StructInner {
    fn eq(&self, other: &Self) -> bool {
        self.values() == other.values()
    }
}

impl Drop for StructInner {
    fn drop(&mut self) {
        // SAFETY: We allocated `self.ptr` via `Box::into_raw` (see
        // `StructInner::new`) with the given `self.len`. Reconstruct
        // the `Box<[Value]>` so it gets freed.
        unsafe {
            let slice = std::slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len);
            drop(Box::from_raw(slice as *mut [Value]));
        }
    }
}

impl alloc::GC<RValue> for StructInner {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        for v in self.values() {
            v.mark(alloc);
        }
    }
}

impl StructInner {
    pub fn new(len: usize) -> Self {
        // Allocate a `Box<[Value]>` of `len` nils, then leak it into a
        // raw pointer we'll reclaim in `Drop`.
        let boxed: Box<[Value]> = vec![Value::nil(); len].into_boxed_slice();
        let raw: *mut [Value] = Box::into_raw(boxed);
        // SAFETY: `Box::into_raw` returns a non-null pointer.
        let ptr = unsafe {
            std::ptr::NonNull::new_unchecked((raw as *mut Value).cast::<Value>())
        };
        StructInner { ptr, len }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn get(&self, index: usize) -> Value {
        assert!(index < self.len);
        // SAFETY: bounds-checked above; the heap slice is alive for
        // the lifetime of `self`.
        unsafe { *self.ptr.as_ptr().add(index) }
    }

    pub fn try_get(&self, index: usize) -> Option<Value> {
        if index < self.len {
            Some(self.get(index))
        } else {
            None
        }
    }

    pub fn set(&mut self, index: usize, value: Value) {
        assert!(index < self.len);
        // SAFETY: bounds-checked above.
        unsafe { *self.ptr.as_ptr().add(index) = value };
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Value> {
        self.values().iter()
    }

    pub fn values(&self) -> &[Value] {
        // SAFETY: `ptr` is a valid pointer to `len` initialised
        // [`Value`]s allocated by `StructInner::new`, alive for the
        // lifetime of `self`.
        unsafe { std::slice::from_raw_parts(self.ptr.as_ptr(), self.len) }
    }
}
