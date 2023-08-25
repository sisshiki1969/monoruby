use super::*;

extern crate alloc;

use std::mem;
use std::ops::{Deref, DerefMut};
use std::ptr::{self, Unique};

type T = Option<Value>;

#[repr(C)]
struct RawTable {
    ptr: Unique<T>,
    cap: usize,
}

impl RawTable {
    fn new() -> Self {
        Self::with_capacity(0)
    }

    fn with_capacity(capacity: usize) -> Self {
        eprintln!("with_capacity() RawTable");
        let ptr = if capacity == 0 {
            Unique::dangling()
        } else {
            unsafe { Unique::new_unchecked(alloc(capacity)) }
        };
        RawTable { ptr, cap: capacity }
    }

    fn grow(&mut self) {
        eprintln!("grow() {}", self.cap);
        unsafe {
            let elem_size = mem::size_of::<T>();
            let align = mem::align_of::<T>();

            let (new_cap, ptr) = if self.cap == 0 {
                //let layout = alloc::alloc::Layout::from_size_align(elem_size, align).unwrap();
                let ptr = alloc(1);
                (1, ptr)
            } else {
                let layout =
                    alloc::alloc::Layout::from_size_align(self.cap * elem_size, align).unwrap();
                let new_cap = 2 * self.cap;
                let ptr =
                    alloc::alloc::realloc(self.ptr.as_ptr() as _, layout, new_cap * elem_size) as _;
                (new_cap, ptr)
            };

            // If allocate or reallocate fail, we'll get `null` back
            if ptr.is_null() {
                oom()
            }
            self.ptr = Unique::new_unchecked(ptr);
            self.cap = new_cap;
        }
    }

    #[inline]
    pub fn reserve(&mut self, len: usize, additional: usize) {
        while len + additional > self.cap {
            self.grow()
        }
    }
}

impl Clone for RawTable {
    fn clone(&self) -> Self {
        let ptr = alloc(self.cap);
        unsafe {
            std::ptr::copy_nonoverlapping(self.ptr.as_ptr(), ptr, self.cap);
        }
        Self {
            ptr: Unique::new(ptr).unwrap(),
            cap: self.cap,
        }
    }
}

impl Drop for RawTable {
    fn drop(&mut self) {
        eprintln!("drop RawTable");
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

fn alloc(capacity: usize) -> *mut T {
    unsafe {
        let elem_size = mem::size_of::<T>();
        let align = mem::align_of::<T>();

        let layout = alloc::alloc::Layout::from_size_align(capacity * elem_size, align).unwrap();
        let ptr = alloc::alloc::alloc(layout) as *mut T;

        // If allocate or reallocate fail, we'll get `null` back
        if ptr.is_null() {
            oom()
        }

        ptr
    }
}

#[repr(C)]
pub struct IvarTable {
    buf: RawTable,
    len: usize,
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

    fn cap(&self) -> usize {
        self.buf.cap
    }

    pub fn new() -> Self {
        IvarTable {
            buf: RawTable::new(),
            len: 0,
        }
    }
    pub fn push(&mut self, elem: T) {
        if self.len == self.cap() {
            self.buf.grow();
        }

        unsafe {
            ptr::write(self.ptr().offset(self.len as isize), elem);
        }

        // Can't fail, we'll OOM first.
        self.len += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            None
        } else {
            self.len -= 1;
            unsafe { Some(ptr::read(self.ptr().offset(self.len as isize))) }
        }
    }

    /*pub fn insert(&mut self, index: usize, elem: T) {
        assert!(index <= self.len, "index out of bounds");
        if self.cap() == self.len {
            self.buf.grow();
        }

        unsafe {
            if index < self.len {
                ptr::copy(
                    self.ptr().offset(index as isize),
                    self.ptr().offset(index as isize + 1),
                    self.len - index,
                );
            }
            ptr::write(self.ptr().offset(index as isize), elem);
            self.len += 1;
        }
    }*/

    /*pub fn remove(&mut self, index: usize) -> T {
        assert!(index < self.len, "index out of bounds");
        unsafe {
            self.len -= 1;
            let result = ptr::read(self.ptr().offset(index as isize));
            ptr::copy(
                self.ptr().offset(index as isize + 1),
                self.ptr().offset(index as isize),
                self.len - index,
            );
            result
        }
    }*/

    pub fn reserve(&mut self, additional: usize) {
        self.buf.reserve(self.len, additional);
    }

    pub fn resize(&mut self, new_len: usize, value: T) {
        let len = self.len();

        if new_len > len {
            self.extend_with(new_len - len, value);
        } else {
            self.truncate(new_len);
        }
    }

    pub fn truncate(&mut self, len: usize) {
        if len < self.len {
            self.len = len;
        }
    }

    fn extend_with(&mut self, n: usize, value: T) {
        self.reserve(n);

        unsafe {
            let mut ptr = self.as_mut_ptr().add(self.len);
            for _ in 0..n {
                eprintln!("{:?}", ptr);
                ptr::write(ptr, value);
                ptr = ptr.add(1);
            }
        }
        self.len += n;
    }

    pub fn into_iter(self) -> IntoIter {
        unsafe {
            let iter = RawIter::new(&self);
            let buf = ptr::read(&self.buf);
            mem::forget(self);

            IntoIter {
                iter: iter,
                _buf: buf,
            }
        }
    }
}

impl Clone for IvarTable {
    fn clone(&self) -> Self {
        Self {
            buf: self.buf.clone(),
            len: self.len,
        }
    }
}

impl Drop for IvarTable {
    fn drop(&mut self) {
        eprintln!("drop IvarTable");
        while let Some(_) = self.pop() {}
        // allocation is handled by RawVec
    }
}

impl Deref for IvarTable {
    type Target = [T];
    fn deref(&self) -> &[T] {
        unsafe { ::std::slice::from_raw_parts(self.ptr(), self.len) }
    }
}

impl DerefMut for IvarTable {
    fn deref_mut(&mut self) -> &mut [T] {
        unsafe { ::std::slice::from_raw_parts_mut(self.ptr(), self.len) }
    }
}

struct RawIter {
    start: *const T,
    end: *const T,
}

impl RawIter {
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

pub struct IntoIter {
    _buf: RawTable, // we don't actually care about this. Just need it to live.
    iter: RawIter,
}

impl Iterator for IntoIter {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        self.iter.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl Drop for IntoIter {
    fn drop(&mut self) {
        for _ in &mut *self {}
    }
}

/// Abort the process, we're out of memory!
///
/// In practice this is probably dead code on most OSes
fn oom() {
    std::process::exit(-9999);
}
