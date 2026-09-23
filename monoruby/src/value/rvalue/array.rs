use super::*;
use smallvec::Drain;
use smallvec::SmallVec;
use smallvec::smallvec;
use std::mem::ManuallyDrop;

pub const ARRAY_INLINE_CAPA: usize = 5;

/// A slice of at least this many elements is handed out as a zero-copy
/// view of a shared root (see [`ArrayContent`]); a shorter one is copied.
/// CRuby's `ARY_DEFAULT_SIZE`: below it the copy is cheaper than the root
/// an unshared parent has to be given first.
pub(crate) const ARRAY_SHARE_MIN: usize = 16;

/// Tag in the `SmallVec` capacity slot marking a shared view. `isize::MAX`
/// for the reason `STRING_SHARED_TAG` gives: a real capacity can never be
/// it, and it must stay positive so the JIT's signed `capa >
/// ARRAY_INLINE_CAPA` select routes a shared array onto its heap path,
/// where the view's `ptr` / `len` overlay the spilled buffer's fields.
pub(crate) const ARRAY_SHARED_TAG: usize = isize::MAX as usize;

#[monoruby_object(write_barrier)]
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
    pub fn uniq(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<bool> {
        let mut h = RubySet::default();
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
                h.insert(*x, vm, globals)
            }
        })
        .map(|removed| removed.is_some())
    }
}

// Write-barrier-protected element stores.
//
// These inherent methods on the `Array` *wrapper* shadow the same-named
// `ArrayInner` methods reached through `Deref`, so existing call sites
// (`ary.push(v)`, `ary.set_index(..)`, …) automatically go through the
// generational write barrier. Only operations that *store* a `Value` need
// a barrier; removers (`pop`, `remove`, `truncate`, `clear`, `drain`) do
// not. See `doc/gc.md`.
impl Array {
    pub fn push(&mut self, value: Value) {
        self.0.as_array_inner_mut().push(value);
        self.0.write_barrier(value);
    }

    pub fn insert(&mut self, index: usize, element: Value) {
        self.0.as_array_inner_mut().insert(index, element);
        self.0.write_barrier(element);
    }

    pub fn fill(&mut self, value: Value) {
        self.0.as_array_inner_mut().fill(value);
        self.0.write_barrier(value);
    }

    pub fn resize(&mut self, new_len: usize, value: Value) {
        self.0.as_array_inner_mut().resize(new_len, value);
        self.0.write_barrier(value);
    }

    pub fn insert_many(&mut self, index: usize, iterable: impl IntoIterator<Item = Value>) {
        self.0.as_array_inner_mut().insert_many(index, iterable);
        self.0.write_barrier_bulk();
    }

    pub fn extend(&mut self, iter: impl std::iter::IntoIterator<Item = Value>) {
        self.0.as_array_inner_mut().extend(iter);
        self.0.write_barrier_bulk();
    }

    pub fn extend_from_slice(&mut self, slice: &[Value]) {
        self.0.as_array_inner_mut().extend_from_slice(slice);
        self.0.write_barrier_bulk();
    }

    pub fn replace(&mut self, v: Vec<Value>) {
        self.0.as_array_inner_mut().replace(v);
        self.0.write_barrier_bulk();
    }

    pub(crate) fn set_index(&mut self, idx: i64, src: Value) -> Result<Value> {
        let r = self.0.as_array_inner_mut().set_index(idx, src)?;
        self.0.write_barrier(src);
        Ok(r)
    }

    pub(crate) fn set_index2(&mut self, index: usize, length: usize, val: Value) -> Result<Value> {
        let r = self.0.as_array_inner_mut().set_index2(index, length, val)?;
        self.0.write_barrier(val);
        Ok(r)
    }
}

pub(crate) type ArrayBuf = SmallVec<[Value; ARRAY_INLINE_CAPA]>;

/// The root and buffer a view of `parent` can be taken from, making one
/// if it has none yet — or `None` when `parent` keeps its elements inline,
/// which cannot be viewed (the buffer lives in the cell and moves with it).
///
/// A view of a view shares the same root. A frozen parent is its own
/// root: frozen, so its buffer stays put. Otherwise the parent's heap
/// buffer is moved into a hidden frozen root and the parent becomes the
/// first view of it — `parent` is left briefly as an empty owned array
/// while the root is allocated, so a collection there sees every object
/// consistent (the buffer is then held by a local, out of the GC's sight).
fn ensure_shared_root(parent: &mut Value) -> Option<(Value, *const Value)> {
    {
        let inner = parent.as_array_inner();
        if inner.is_shared() {
            // SAFETY: tag-discriminated.
            let sa = unsafe { inner.0.shared };
            return Some((sa.root, sa.ptr));
        }
        if !inner.owned_spilled() {
            return None;
        }
        if parent.is_frozen() {
            return Some((*parent, inner.as_ptr()));
        }
    }
    let buf = {
        let inner = parent.as_array_inner_mut();
        // SAFETY: tag-discriminated; `owned` is the live variant.
        std::mem::take(unsafe { &mut *inner.0.owned })
    };
    let mut root = Value::array(ArrayInner::from(buf));
    root.set_frozen();
    let (ptr, len) = {
        let root_inner = root.as_array_inner();
        (root_inner.as_ptr(), root_inner.len())
    };
    // Overwriting the (empty, heap-free) owned content with the view leaks
    // nothing: an un-spilled SmallVec owns no allocation.
    parent.as_array_inner_mut().0 = ArrayContent {
        shared: SharedArray {
            tag: ARRAY_SHARED_TAG,
            ptr,
            len,
            root,
        },
    };
    // `parent` now holds an edge to the freshly allocated (young) root.
    // If `parent` is old, that is an un-barriered old-to-young store: a
    // minor collection would reclaim the root while `parent` still views
    // its buffer. Record the edge, as `ensure_shared_root` does for
    // Strings.
    parent.write_barrier(root);
    Some((root, ptr))
}

/// The view form of an Array's storage: `ptr` / `len` into the buffer of
/// a hidden, frozen `root` Array that owns it. Laid out over the spilled
/// `SmallVec` (capacity / heap ptr / heap len) with `tag` in the capacity
/// slot, so every reader of a heap-buffered array — the JIT's inline
/// `[]`, `length`, block-argument expansion — sees a view exactly as it
/// sees a spilled buffer. The root's buffer never moves: the root is
/// frozen and reachable only through its views, which the GC marks it
/// from.
#[repr(C)]
#[derive(Clone, Copy)]
struct SharedArray {
    tag: usize,
    ptr: *const Value,
    len: usize,
    root: Value,
}

/// An Array's storage: its own `SmallVec`, or a [`SharedArray`] view. The
/// same union overlay as `StringContent`, and the same contract: a write
/// goes through [`ArrayInner::owned_mut`], which copies a view into a
/// buffer of its own first (copy-on-write), so the root is never written.
#[repr(C)]
union ArrayContent {
    owned: ManuallyDrop<ArrayBuf>,
    shared: SharedArray,
}

impl ArrayContent {
    #[inline]
    fn is_shared(&self) -> bool {
        // SAFETY: both variants start with a usize (SmallVec's `capacity`
        // / SharedArray's `tag`), so reading it through either is valid.
        unsafe { self.shared.tag == ARRAY_SHARED_TAG }
    }

    #[inline]
    fn as_slice(&self) -> &[Value] {
        unsafe {
            if self.is_shared() {
                // SAFETY: `ptr` / `len` describe a live sub-range of the
                // root's heap buffer; the root is kept alive by the GC as
                // long as this view is, and its buffer is never reallocated.
                std::slice::from_raw_parts(self.shared.ptr, self.shared.len)
            } else {
                &self.owned
            }
        }
    }

    #[inline]
    fn from_owned(owned: ArrayBuf) -> Self {
        ArrayContent {
            owned: ManuallyDrop::new(owned),
        }
    }
}

#[repr(transparent)]
pub struct ArrayInner(ArrayContent);

impl Drop for ArrayInner {
    fn drop(&mut self) {
        if !self.0.is_shared() {
            // SAFETY: tag-discriminated; `owned` is the live variant, and
            // it is dropped exactly once, here.
            unsafe { ManuallyDrop::drop(&mut self.0.owned) }
        }
    }
}

impl Clone for ArrayInner {
    fn clone(&self) -> Self {
        if self.0.is_shared() {
            // Another view of the same root. The clone is a fresh (young)
            // object, so its edge to the root needs no barrier.
            // SAFETY: tag-discriminated.
            ArrayInner(ArrayContent {
                shared: unsafe { self.0.shared },
            })
        } else {
            // SAFETY: tag-discriminated; `owned` is the live variant.
            ArrayInner::from(unsafe { (*self.0.owned).clone() })
        }
    }
}

impl std::fmt::Debug for ArrayInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.0.as_slice().iter()).finish()
    }
}

impl Default for ArrayInner {
    fn default() -> Self {
        Self::new()
    }
}

impl std::ops::Deref for ArrayInner {
    type Target = [Value];
    fn deref(&self) -> &Self::Target {
        self.0.as_slice()
    }
}

impl std::ops::DerefMut for ArrayInner {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.owned_mut().as_mut_slice()
    }
}

impl std::iter::FromIterator<Value> for ArrayInner {
    fn from_iter<I: IntoIterator<Item = Value>>(iter: I) -> Self {
        ArrayInner::from(SmallVec::from_iter(iter))
    }
}

impl ArrayInner {
    /// Whether this array is a zero-copy view into a shared root's buffer.
    #[inline]
    pub(crate) fn is_shared(&self) -> bool {
        self.0.is_shared()
    }

    /// The hidden root whose buffer this array views, if any. The GC marks
    /// it from here to keep the buffer's owner alive.
    #[inline]
    pub(crate) fn shared_root(&self) -> Option<Value> {
        if self.0.is_shared() {
            // SAFETY: tag-discriminated.
            Some(unsafe { self.0.shared.root })
        } else {
            None
        }
    }

    /// Owned, with the elements on the heap (not in the inline buffer) —
    /// the only buffer that can be moved into a root and viewed.
    fn owned_spilled(&self) -> bool {
        // SAFETY: tag-discriminated; `owned` is the live variant.
        !self.0.is_shared() && unsafe { self.0.owned.spilled() }
    }

    fn from_shared(root: Value, ptr: *const Value, len: usize) -> Self {
        ArrayInner(ArrayContent {
            shared: SharedArray {
                tag: ARRAY_SHARED_TAG,
                ptr,
                len,
                root,
            },
        })
    }

    /// The owned buffer, for writing: a view is first copied into a buffer
    /// of its own, so the root (and every other view of it) is unaffected.
    #[inline]
    fn owned_mut(&mut self) -> &mut ArrayBuf {
        if self.0.is_shared() {
            self.uniquify();
        }
        // SAFETY: `owned` is the live variant (just uniquified if needed).
        unsafe { &mut self.0.owned }
    }

    fn uniquify(&mut self) {
        if self.0.is_shared() {
            let owned = SmallVec::from_slice(self.0.as_slice());
            // Plain assignment: the old value is a view, which owns
            // nothing, so there is nothing to release for it.
            self.0 = ArrayContent::from_owned(owned);
        }
    }
}

impl RubyHash<Executor, Globals, MonorubyErr> for ArrayInner {
    fn ruby_hash<H: std::hash::Hasher>(
        &self,
        state: &mut H,
        e: &mut Executor,
        g: &mut Globals,
    ) -> Result<()> {
        for v in self.iter() {
            v.ruby_hash(state, e, g)?;
        }
        Ok(())
    }
}

impl ArrayInner {
    pub fn new() -> Self {
        ArrayInner::from(smallvec!())
    }

    pub fn with_capacity(len: usize) -> Self {
        ArrayInner::from(SmallVec::with_capacity(len))
    }

    pub fn from(smallvec: SmallVec<[Value; 5]>) -> Self {
        ArrayInner(ArrayContent::from_owned(smallvec))
    }

    pub fn from_vec(v: Vec<Value>) -> Self {
        ArrayInner::from(SmallVec::from_vec(v))
    }

    pub fn from_slice(slice: &[Value]) -> Self {
        ArrayInner::from(SmallVec::from_slice(slice))
    }

    /*pub fn from_iter(iter: impl Iterator<Item = Value>) -> Self {
        ArrayInner(SmallVec::from_iter(iter))
    }*/

    pub fn clear(&mut self) {
        self.owned_mut().clear();
    }

    pub fn replace(&mut self, v: Vec<Value>) {
        // Dropping the old content through `ArrayInner`'s own rule.
        *self = ArrayInner::from(SmallVec::from_vec(v));
    }

    pub fn push(&mut self, value: Value) {
        self.owned_mut().push(value);
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.owned_mut().pop()
    }

    pub fn truncate(&mut self, new_len: usize) {
        self.owned_mut().truncate(new_len);
    }

    pub fn resize(&mut self, new_len: usize, value: Value) {
        self.owned_mut().resize(new_len, value)
    }

    pub fn fill(&mut self, value: Value) {
        self.owned_mut().fill(value);
    }

    pub fn extend(&mut self, iter: impl std::iter::IntoIterator<Item = Value>) {
        self.owned_mut().extend(iter);
    }

    pub fn extend_from_slice(&mut self, slice: &[Value]) {
        self.owned_mut().extend_from_slice(slice);
    }

    pub fn insert(&mut self, index: usize, element: Value) {
        self.owned_mut().insert(index, element)
    }

    pub fn insert_many(&mut self, index: usize, iterable: impl IntoIterator<Item = Value>) {
        self.owned_mut().insert_many(index, iterable)
    }

    pub fn drain(
        &mut self,
        range: std::ops::Range<usize>,
    ) -> Drain<'_, [Value; ARRAY_INLINE_CAPA]> {
        self.owned_mut().drain(range)
    }

    ///
    /// Retains only elements which f(elem) returns true.
    ///
    /// Returns Some(last_value) when one or some elements were removed.
    pub fn retain<F>(&mut self, mut f: F) -> Result<Option<Value>>
    where
        F: FnMut(&Value) -> Result<bool>,
    {
        let mut i = 0;
        let mut del = 0;
        let mut removed = None;
        while i < self.len() {
            let val = self[i];
            if !f(&val)? {
                removed = Some(val);
                del += 1;
            } else if del > 0 {
                self.owned_mut().swap(i - del, i);
            }
            i += 1;
        }
        if del > 0 {
            let len = self.len();
            self.truncate(len - del);
        }
        Ok(removed)
    }

    pub fn remove(&mut self, index: usize) -> Value {
        self.owned_mut().remove(index)
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

    pub fn to_s(&self, store: &Store, self_id: u64) -> String {
        let mut set = HashSet::new();
        set.insert(self_id);
        self.inspect_inner(store, &mut set)
    }

    pub(crate) fn inspect_inner(&self, store: &Store, set: &mut HashSet<u64>) -> String {
        match self.len() {
            0 => "[]".to_string(),
            1 => format!("[{}]", self[0].inspect_inner(store, set)),
            _ => {
                let mut s = format!("[{}", self[0].inspect_inner(store, set));
                for val in self[1..].iter() {
                    s += &format!(", {}", val.inspect_inner(store, set));
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
                let is_self = std::ptr::eq((*ary).as_ptr(), self.as_ptr());
                let ary_data = if is_self {
                    ary.to_vec()
                } else {
                    Vec::new()
                };
                let ary_slice: &[Value] = if is_self {
                    &ary_data
                } else {
                    &ary[..]
                };
                let ary_len = ary_slice.len();
                if index >= len || index + length > len {
                    self.resize(index + ary_len, Value::nil());
                } else if ary_len > length {
                    self.resize(len + ary_len - length, Value::nil());
                    self.copy_within(index + length..len, index + ary_len);
                } else {
                    self.copy_within(index + length..len, index + ary_len);
                    self.resize(len + ary_len - length, Value::nil());
                }
                self[index..index + ary_len].copy_from_slice(&ary_slice[0..ary_len]);
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


}


/// Indexing that may answer with a *view* needs the receiver `Value`, not
/// just its payload: see `slice_value`.
impl Array {
    pub(crate) fn get_elem2(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
        arg0: Value,
        arg1: Value,
    ) -> Result<Value> {
        let index = arg0.coerce_to_int_i64(vm, globals)?;
        let self_len = self.len();
        let index = match self.get_array_index(index) {
            Some(i) => i,
            None => return Ok(Value::nil()), // negative index beyond array size
        };
        let len = arg1.coerce_to_int_i64(vm, globals)?;
        let val = if len < 0 || index > self_len {
            Value::nil()
        } else if index == self_len {
            Value::array_empty()
        } else {
            let len = len as usize;
            let start = index;
            let end = std::cmp::min(self_len, start + len);
            self.slice_value(start, end)
        };
        Ok(val)
    }

    /// `self[start..end]` as a new Array. A long slice of a heap-buffered
    /// array is a view of a shared root — the parent's buffer is moved into
    /// a hidden frozen root the first time, and both the parent and the
    /// slice then read it in place; the copy happens on a write, to
    /// whichever side writes. A short slice, or one of an inline buffer,
    /// is copied as before.
    pub(crate) fn slice_value(&self, start: usize, end: usize) -> Value {
        debug_assert!(start <= end && end <= self.len());
        let len = end - start;
        if len >= ARRAY_SHARE_MIN
            && let Some((root, ptr)) = ensure_shared_root(&mut self.0.clone())
        {
            // SAFETY: `start..end` is within the root's buffer, whose
            // length is the parent's (the parent views all of it).
            let ptr = unsafe { ptr.add(start) };
            Value::array(ArrayInner::from_shared(root, ptr, len))
        } else {
            Value::array_from_slice(&self[start..end])
        }
    }

    pub(crate) fn get_elem1(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
        idx: Value,
    ) -> Result<Value> {
        if let Some(range) = idx.is_range() {
            let len = self.len() as i64;
            // nil begin means 0 (beginless range)
            let i_start = if range.start().is_nil() {
                0
            } else {
                match range.start().coerce_to_int_i64(vm, globals)? {
                    i if i < 0 => len + i,
                    i => i,
                }
            };
            if i_start < 0 {
                return Ok(Value::nil());
            }
            let start = match len {
                i if i == i_start => return Ok(Value::array_empty()),
                i if i < i_start => return Ok(Value::nil()),
                _ => i_start as usize,
            };

            // nil end means array length (endless range)
            let end = if range.end().is_nil() {
                self.len()
            } else {
                let i_end = range.end().coerce_to_int_i64(vm, globals)?;
                if i_end >= 0 {
                    let end = i_end as usize + if range.exclude_end() { 0 } else { 1 };
                    if self.len() < end { self.len() } else { end }
                } else {
                    let e = len + i_end + if range.exclude_end() { 0 } else { 1 };
                    if e < 0 {
                        return Ok(Value::array_empty());
                    }
                    e as usize
                }
            };
            if start >= end {
                return Ok(Value::array_empty());
            }
            Ok(self.slice_value(start, end))
        } else {
            let index = idx.coerce_to_int_i64(vm, globals)?;
            let self_len = self.len();
            let index = self.get_array_index(index).unwrap_or(self_len);
            let val = self.get(index).cloned().unwrap_or_default();
            Ok(val)
        }
    }
}

#[cfg(test)]
mod shared_view_tests {
    use super::*;

    /// The view overlays the spilled `SmallVec`: the JIT reads a heap
    /// array's `ptr` / `len` at fixed offsets and selects the heap path
    /// with a signed `capa > ARRAY_INLINE_CAPA`, so a view must put its
    /// tag, pointer and length exactly there, and the tag must be positive
    /// and larger than any real capacity.
    #[test]
    fn shared_overlay_matches_spilled_smallvec_layout() {
        assert_eq!(
            std::mem::offset_of!(SharedArray, tag),
            smallvec::OFFSET_CAPA
        );
        assert_eq!(
            std::mem::offset_of!(SharedArray, ptr),
            smallvec::OFFSET_HEAP_PTR
        );
        assert_eq!(
            std::mem::offset_of!(SharedArray, len),
            smallvec::OFFSET_HEAP_LEN
        );
        assert_eq!(
            std::mem::size_of::<ArrayContent>(),
            std::mem::size_of::<ArrayBuf>()
        );
        assert_eq!(std::mem::size_of::<ArrayInner>(), std::mem::size_of::<ArrayBuf>());
        assert!(ARRAY_SHARED_TAG > ARRAY_INLINE_CAPA);
        assert!((ARRAY_SHARED_TAG as isize) > 0);

        // A spilled owned buffer read through the `shared` overlay must
        // expose its heap ptr / len on the same offsets.
        let elems: Vec<Value> = (0..100).map(Value::integer).collect();
        let inner = ArrayInner::from_vec(elems);
        assert!(inner.owned_spilled());
        let (ptr, len) = unsafe { (inner.0.shared.ptr, inner.0.shared.len) };
        assert_eq!(ptr, inner.as_ptr());
        assert_eq!(len, inner.len());
        assert!(!inner.is_shared());
    }

    /// A view reads the root's elements, and a write copies it out first:
    /// the root, and every other view, keep what they had.
    #[test]
    fn a_view_reads_the_root_and_copies_on_write() {
        let elems: Vec<Value> = (0..40).map(Value::integer).collect();
        let mut root = Value::array_from_vec(elems);
        root.set_frozen();
        let ptr = root.as_array_inner().as_ptr();
        let mut view = ArrayInner::from_shared(root, unsafe { ptr.add(5) }, 20);
        assert!(view.is_shared());
        assert_eq!(view.shared_root(), Some(root));
        assert_eq!(view.len(), 20);
        assert_eq!(view[0], Value::integer(5));
        let other = view.clone();
        assert!(other.is_shared());

        view.push(Value::integer(99));
        assert!(!view.is_shared());
        assert_eq!(view.len(), 21);
        assert_eq!(view[20], Value::integer(99));
        view[0] = Value::nil();
        assert_eq!(other[0], Value::integer(5));
        assert_eq!(root.as_array_inner()[5], Value::integer(5));
        assert_eq!(root.as_array_inner().len(), 40);
    }
}
