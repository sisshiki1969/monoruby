use super::*;

extern crate alloc;

use std::mem;
use std::ops::{Deref, DerefMut};
use std::ptr::{self, Unique};
use std::slice;

type T = Option<Value>;

pub const IVAR_TABLE_PTR: usize = std::mem::offset_of!(IvarTable, buf.ptr);
pub const IVAR_TABLE_CAPA: usize = std::mem::offset_of!(IvarTable, buf.cap);
pub const IVAR_TABLE_LEN: usize = std::mem::offset_of!(IvarTable, len);

///
/// A table of instant variables in the field `ivar_table` of RValue.
///
/// This is almost Vec.
///
#[derive(Clone)]
#[repr(C)]
pub struct IvarTable {
    buf: RawTable,
    len: usize,
}

impl Deref for IvarTable {
    type Target = [T];
    fn deref(&self) -> &[T] {
        unsafe { slice::from_raw_parts(self.ptr(), self.len) }
    }
}

impl DerefMut for IvarTable {
    fn deref_mut(&mut self) -> &mut [T] {
        unsafe { slice::from_raw_parts_mut(self.ptr(), self.len) }
    }
}

impl IvarTable {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            buf: RawTable::with_capacity(capacity),
            len: 0,
        }
    }

    fn ptr(&self) -> *mut T {
        self.buf.ptr.as_ptr()
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

pub struct IvarTableIntoIter {
    _buf: RawTable, // we don't actually care about this. Just need it to live.
    iter: RawIter,
}

impl Iterator for IvarTableIntoIter {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        self.iter.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl Drop for IvarTableIntoIter {
    fn drop(&mut self) {
        for _ in &mut *self {}
    }
}

#[repr(C)]
struct RawTable {
    ptr: Unique<T>,
    cap: usize,
}

impl Clone for RawTable {
    fn clone(&self) -> Self {
        unsafe {
            let ptr = alloc(self.cap);
            ptr::copy_nonoverlapping(self.ptr.as_ptr(), ptr.as_ptr(), self.cap);
            Self {
                ptr: ptr.into(),
                cap: self.cap,
            }
        }
    }
}

impl Drop for RawTable {
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

impl RawTable {
    /*fn new() -> Self {
        Self::with_capacity(0)
    }*/

    ///
    /// Allocate with capacity.
    ///
    fn with_capacity(capacity: usize) -> Self {
        let (ptr, cap) = if capacity == 0 {
            (Unique::dangling(), 0)
        } else {
            let cap = capacity.next_power_of_two();
            let ptr = alloc(cap).into();
            (ptr, cap)
        };
        RawTable { ptr, cap }
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

        self.ptr = ptr.into();
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
fn alloc(capacity: usize) -> ptr::NonNull<T> {
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

struct RawIter {
    start: *const T,
    end: *const T,
}

impl RawIter {
    /*unsafe fn new(slice: &[T]) -> Self {
        RawIter {
            start: slice.as_ptr(),
            end: if slice.len() == 0 {
                slice.as_ptr()
            } else {
                slice.as_ptr().offset(slice.len() as isize)
            },
        }
    }*/
}

impl Iterator for RawIter {
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
