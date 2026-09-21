use super::*;

extern crate alloc;

use std::mem;
use std::ops::{Deref, DerefMut};
use std::ptr::{self};
use std::slice;

//type T = Option<Value>;

pub const MONOVEC_PTR: usize = std::mem::offset_of!(MonoVec<Option<Value>>, buf.ptr);
pub const MONOVEC_CAPA: usize = std::mem::offset_of!(MonoVec<Option<Value>>, buf.cap);
pub const MONOVEC_LEN: usize = std::mem::offset_of!(MonoVec<Option<Value>>, len);

///
/// A table of instant variables in the field `ivar_table` of RValue.
///
/// This is almost Vec.
///
#[derive(Clone)]
#[repr(C)]
pub struct MonoVec<T> {
    buf: RawVec<T>,
    len: usize,
}

impl<T> Deref for MonoVec<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        unsafe { slice::from_raw_parts(self.ptr(), self.len) }
    }
}

impl<T> DerefMut for MonoVec<T> {
    fn deref_mut(&mut self) -> &mut [T] {
        unsafe { slice::from_raw_parts_mut(self.ptr(), self.len) }
    }
}

///
/// `RawVec` frees the buffer but knows nothing of what is in it, so
/// without this the elements' destructors never ran. That is free for
/// the `Option<Value>` / `Option<Module>` tables this was written for —
/// they have no drop glue, and this compiles to nothing — but `Funcs`
/// keeps its `FuncInfo`s here, and each of those owns a `Box<FuncExt>`.
/// Every builtin a `Globals` registered therefore leaked its `FuncExt`
/// (and the `DestLabel` its wrapper put in it) when the `Globals` went
/// away: 368 KB per interpreter, which over a test binary that builds
/// one per test came to gigabytes.
///
impl<T> Drop for MonoVec<T> {
    fn drop(&mut self) {
        // SAFETY: the first `len` slots are initialised — `push` writes
        // them, and `extend` zero-fills, which is a valid `None` for the
        // niche-optional types that use it. `RawVec::drop` then frees the
        // buffer they lived in.
        unsafe {
            ptr::drop_in_place(ptr::slice_from_raw_parts_mut(self.ptr(), self.len));
        }
    }
}

impl<T> MonoVec<T> {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            buf: RawVec::with_capacity(capacity),
            len: 0,
        }
    }

    fn ptr(&self) -> *mut T {
        self.buf.ptr.as_ptr()
    }

    pub fn push(&mut self, value: T) {
        // This will panic or abort if we would allocate > isize::MAX bytes
        // or if the length increment would overflow for zero-sized types.
        if self.len == self.buf.capacity() {
            self.buf.grow(self.len + 1);
        }
        unsafe {
            let end = self.as_mut_ptr().add(self.len);
            ptr::write(end, value);
            self.len += 1;
        }
    }

    pub fn reserve(&mut self, additional: usize) {
        self.buf.reserve(self.len, additional);
    }

    pub fn resize(&mut self, new_len: usize) {
        let len = self.len();

        if new_len > len {
            self.extend(new_len - len);
        }
    }

    fn extend(&mut self, n: usize) {
        self.reserve(n);

        unsafe {
            let ptr = self.as_mut_ptr().add(self.len);
            ptr::write_bytes(ptr, 0, n);
        }
        self.len += n;
    }

    /*pub fn into_iter(self) -> IvarTableIntoIter {
        unsafe {
            let iter = RawIter::new(&self);
            let buf = ptr::read(&self.buf);
            mem::forget(self);

            IvarTableIntoIter {
                iter: iter,
                _buf: buf,
            }
        }
    }*/
}

/*pub struct IvarTableIntoIter<T> {
    _buf: RawVec<T>, // we don't actually care about this. Just need it to live.
    iter: RawIter<T>,
}

impl<T> Iterator for IvarTableIntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        self.iter.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<T> Drop for IvarTableIntoIter<T> {
    fn drop(&mut self) {
        for _ in &mut *self {}
    }
}*/

#[repr(C)]
struct RawVec<T> {
    ptr: std::ptr::NonNull<T>,
    cap: usize,
}

impl<T> Clone for RawVec<T> {
    fn clone(&self) -> Self {
        unsafe {
            let ptr = alloc(self.cap);
            ptr::copy_nonoverlapping(self.ptr.as_ptr(), ptr.as_ptr(), self.cap);
            Self { ptr, cap: self.cap }
        }
    }
}

impl<T> Drop for RawVec<T> {
    fn drop(&mut self) {
        let elem_size = mem::size_of::<T>();
        if self.cap != 0 {
            let align = mem::align_of::<T>();
            let num_bytes = elem_size * self.cap;
            unsafe {
                let layout = alloc::alloc::Layout::from_size_align(num_bytes, align).unwrap();
                alloc::alloc::dealloc(self.ptr.as_ptr() as *mut _, layout);
            }
        }
    }
}

impl<T> RawVec<T> {
    /*fn new() -> Self {
        Self::with_capacity(0)
    }*/

    ///
    /// Allocate with capacity.
    ///
    fn with_capacity(capacity: usize) -> Self {
        let (ptr, cap) = if capacity == 0 {
            (std::ptr::NonNull::dangling(), 0)
        } else {
            let cap = capacity.next_power_of_two();
            let ptr = alloc(cap);
            (ptr, cap)
        };
        RawVec { ptr, cap }
    }

    fn capacity(&self) -> usize {
        self.cap
    }

    fn reserve(&mut self, len: usize, additional: usize) {
        let new_len = len + additional;
        if new_len > self.cap {
            self.grow(new_len);
        }
    }

    fn grow(&mut self, min_capacity: usize) {
        let capacity = min_capacity.next_power_of_two();
        let ptr = if self.cap == 0 {
            alloc(capacity)
        } else {
            self.realloc(capacity)
        };

        self.ptr = ptr;
        self.cap = capacity;
    }

    ///
    /// Realloc buffer.
    ///
    fn realloc(&mut self, capacity: usize) -> std::ptr::NonNull<T> {
        assert_ne!(0, capacity);
        let elem_size = mem::size_of::<T>();
        let align = mem::align_of::<T>();
        let layout = alloc::alloc::Layout::from_size_align(self.cap * elem_size, align).unwrap();
        let ptr = unsafe {
            alloc::alloc::realloc(self.ptr.as_ptr() as _, layout, capacity * elem_size) as _
        };
        // If allocate or reallocate fail, we'll get `null` back
        match ptr::NonNull::new(ptr) {
            None => alloc::alloc::handle_alloc_error(layout),
            Some(ptr) => ptr,
        }
    }
}

///
/// Allocate buffer.
///
fn alloc<T>(capacity: usize) -> ptr::NonNull<T> {
    assert_ne!(0, capacity);
    let elem_size = mem::size_of::<T>();
    let align = mem::align_of::<T>();

    let layout = alloc::alloc::Layout::from_size_align(capacity * elem_size, align).unwrap();
    let ptr = unsafe { alloc::alloc::alloc(layout) as *mut T };

    // If allocate or reallocate fail, we'll get `null` back
    match ptr::NonNull::new(ptr) {
        None => alloc::alloc::handle_alloc_error(layout),
        Some(ptr) => ptr,
    }
}
/*
struct RawIter<T> {
    start: *const T,
    end: *const T,
}

/*impl RawIter {
    unsafe fn new(slice: &[T]) -> Self {
        RawIter {
            start: slice.as_ptr(),
            end: if slice.len() == 0 {
                slice.as_ptr()
            } else {
                slice.as_ptr().offset(slice.len() as isize)
            },
        }
    }
}*/

impl<T> Iterator for RawIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        if self.start == self.end {
            None
        } else {
            unsafe {
                let result = ptr::read(self.start);
                self.start = self.start.offset(1);
                Some(result)
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let elem_size = mem::size_of::<T>();
        let len = (self.end as usize - self.start as usize) / elem_size;
        (len, Some(len))
    }
}
*/

#[cfg(test)]
mod monovec_drop_tests {
    use super::MonoVec;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static DROPPED: AtomicUsize = AtomicUsize::new(0);

    /// Stands in for `FuncInfo`: owns something, so forgetting to drop it
    /// leaks.
    struct Owner(#[allow(dead_code)] Box<u64>);

    impl Drop for Owner {
        fn drop(&mut self) {
            DROPPED.fetch_add(1, Ordering::SeqCst);
        }
    }

    /// The buffer is the container's, but so are the elements in it:
    /// `RawVec` frees the former and knows nothing of the latter, so
    /// without `MonoVec`'s own `Drop` everything they own is leaked —
    /// which is what every `Globals` did with its builtins' `FuncExt`s.
    #[test]
    fn dropping_the_vec_drops_what_is_in_it() {
        DROPPED.store(0, Ordering::SeqCst);
        {
            let mut v = MonoVec::with_capacity(2);
            // Past the initial capacity, so the grow path is covered too:
            // the elements must survive the realloc and still be dropped
            // exactly once.
            for i in 0..5u64 {
                v.push(Owner(Box::new(i)));
            }
            assert_eq!(0, DROPPED.load(Ordering::SeqCst));
        }
        assert_eq!(5, DROPPED.load(Ordering::SeqCst));
    }

    /// Only the initialised prefix is dropped — capacity beyond `len`
    /// holds nothing.
    #[test]
    fn spare_capacity_is_not_dropped() {
        DROPPED.store(0, Ordering::SeqCst);
        {
            let mut v: MonoVec<Owner> = MonoVec::with_capacity(64);
            v.push(Owner(Box::new(1)));
        }
        assert_eq!(1, DROPPED.load(Ordering::SeqCst));
    }
}
