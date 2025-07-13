//! A hash set implemented using [`IndexMap`]

mod iter;
mod slice;

#[cfg(test)]
mod tests;

pub use self::iter::{Drain, IntoIter, Iter};
pub use self::slice::Slice;

use std::collections::hash_map::RandomState;

use crate::util::try_simplify_range;
use alloc::boxed::Box;
use alloc::vec::Vec;
use core::cmp::Ordering;
use core::fmt;
use core::hash::{BuildHasher, Hash};
use core::ops::RangeBounds;

use super::{Entries, Equivalent, RubyEql, RubyMap};

type Bucket<T> = super::Bucket<T, ()>;

/// A hash set where the iteration order of the values is independent of their
/// hash values.
///
/// The interface is closely compatible with the standard
/// [`HashSet`][std::collections::HashSet],
/// but also has additional features.
///
/// # Order
///
/// The values have a consistent order that is determined by the sequence of
/// insertion and removal calls on the set. The order does not depend on the
/// values or the hash function at all. Note that insertion order and value
/// are not affected if a re-insertion is attempted once an element is
/// already present.
///
/// All iterators traverse the set *in order*.  Set operation iterators like
/// [`RubySet::union`] produce a concatenated order, as do their matching "bitwise"
/// operators.  See their documentation for specifics.
///
/// The insertion order is preserved, with **notable exceptions** like the
/// [`.remove()`][Self::remove] or [`.swap_remove()`][Self::swap_remove] methods.
/// Methods such as [`.sort_by()`][Self::sort_by] of
/// course result in a new order, depending on the sorting order.
///
/// # Indices
///
/// The values are indexed in a compact range without holes in the range
/// `0..self.len()`. For example, the method `.get_full` looks up the index for
/// a value, and the method `.get_index` looks up the value by index.
///
/// # Complexity
///
/// Internally, `RubySet<T, S>` just holds an [`IndexMap<T, (), S>`](IndexMap). Thus the complexity
/// of the two are the same for most methods.
///
/// # Examples
///
/// ```
/// use rubymap::RubySet;
///
/// // Collects which letters appear in a sentence.
/// let letters: RubySet<_> = RubySet::from_iter("a short treatise on fungi".chars(), &mut (), &mut ()).unwrap();
///
/// assert!(letters.contains(&'s', &mut (), &mut ()).unwrap());
/// assert!(letters.contains(&'t', &mut (), &mut ()).unwrap());
/// assert!(letters.contains(&'u', &mut (), &mut ()).unwrap());
/// assert!(!letters.contains(&'y', &mut (), &mut ()).unwrap());
/// ```
pub struct RubySet<T, E = (), G = (), R = (), S = RandomState> {
    pub(crate) map: RubyMap<T, (), E, G, R, S>,
}

impl<T, E, G, R> Clone for RubySet<T, E, G, R>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        RubySet {
            map: self.map.clone(),
        }
    }

    fn clone_from(&mut self, other: &Self) {
        self.map.clone_from(&other.map);
    }
}

impl<T: PartialEq + RubyEql<(), (), ()> + Hash> PartialEq for RubySet<T, (), (), ()> {
    fn eq(&self, other: &Self) -> bool {
        self.map == other.map
    }
}

impl<T, E, G, R, S> Entries for RubySet<T, E, G, R, S> {
    type Entry = Bucket<T>;

    #[inline]
    fn into_entries(self) -> Vec<Self::Entry> {
        self.map.into_entries()
    }

    #[inline]
    fn as_entries(&self) -> &[Self::Entry] {
        self.map.as_entries()
    }

    #[inline]
    fn as_entries_mut(&mut self) -> &mut [Self::Entry] {
        self.map.as_entries_mut()
    }

    fn with_entries<F>(&mut self, f: F)
    where
        F: FnOnce(&mut [Self::Entry]),
    {
        self.map.with_entries(f);
    }
}

impl<T, E, G, R> fmt::Debug for RubySet<T, E, G, R>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
}

impl<T, E, G, R> RubySet<T, E, G, R> {
    /// Create a new set. (Does not allocate.)
    pub fn new() -> Self {
        RubySet {
            map: RubyMap::new(),
        }
    }

    /// Create a new set with capacity for `n` elements.
    /// (Does not allocate if `n` is zero.)
    ///
    /// Computes in **O(n)** time.
    pub fn with_capacity(n: usize) -> Self {
        RubySet {
            map: RubyMap::with_capacity(n),
        }
    }
}

impl<T, E, G, R, S> RubySet<T, E, G, R, S> {
    /// Create a new set with capacity for `n` elements.
    /// (Does not allocate if `n` is zero.)
    ///
    /// Computes in **O(n)** time.
    pub fn with_capacity_and_hasher(n: usize, hash_builder: S) -> Self {
        RubySet {
            map: RubyMap::with_capacity_and_hasher(n, hash_builder),
        }
    }

    /// Create a new set with `hash_builder`.
    ///
    /// This function is `const`, so it
    /// can be called in `static` contexts.
    pub const fn with_hasher(hash_builder: S) -> Self {
        RubySet {
            map: RubyMap::with_hasher(hash_builder),
        }
    }

    /// Return the number of elements the set can hold without reallocating.
    ///
    /// This number is a lower bound; the set might be able to hold more,
    /// but is guaranteed to be able to hold at least this many.
    ///
    /// Computes in **O(1)** time.
    pub fn capacity(&self) -> usize {
        self.map.capacity()
    }

    /// Return a reference to the set's `BuildHasher`.
    pub fn hasher(&self) -> &S {
        self.map.hasher()
    }

    /// Return the number of elements in the set.
    ///
    /// Computes in **O(1)** time.
    pub fn len(&self) -> usize {
        self.map.len()
    }

    /// Returns true if the set contains no elements.
    ///
    /// Computes in **O(1)** time.
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    /// Return an iterator over the values of the set, in their order
    pub fn iter(&self) -> Iter<'_, T> {
        Iter::new(self.as_entries())
    }

    /// Remove all elements in the set, while preserving its capacity.
    ///
    /// Computes in **O(n)** time.
    pub fn clear(&mut self) {
        self.map.clear();
    }

    /// Shortens the set, keeping the first `len` elements and dropping the rest.
    ///
    /// If `len` is greater than the set's current length, this has no effect.
    pub fn truncate(&mut self, len: usize, e: &mut E, g: &mut G) -> Result<(), R> {
        self.map.truncate(len, e, g)
    }

    /// Clears the `RubySet` in the given index range, returning those values
    /// as a drain iterator.
    ///
    /// The range may be any type that implements [`RangeBounds<usize>`],
    /// including all of the `std::ops::Range*` types, or even a tuple pair of
    /// `Bound` start and end values. To drain the set entirely, use `RangeFull`
    /// like `set.drain(..)`.
    ///
    /// This shifts down all entries following the drained range to fill the
    /// gap, and keeps the allocated memory for reuse.
    ///
    /// ***Panics*** if the starting point is greater than the end point or if
    /// the end point is greater than the length of the set.
    #[track_caller]
    pub fn drain<Ra>(&mut self, range: Ra, e: &mut E, g: &mut G) -> Result<Drain<'_, T>, R>
    where
        Ra: RangeBounds<usize>,
    {
        Ok(Drain::new(self.map.core.drain(range, e, g)?))
    }

    /// Splits the collection into two at the given index.
    ///
    /// Returns a newly allocated set containing the elements in the range
    /// `[at, len)`. After the call, the original set will be left containing
    /// the elements `[0, at)` with its previous capacity unchanged.
    ///
    /// ***Panics*** if `at > len`.
    #[track_caller]
    pub fn split_off(&mut self, at: usize, e: &mut E, g: &mut G) -> Result<Self, R>
    where
        S: Clone,
    {
        Ok(Self {
            map: self.map.split_off(at, e, g)?,
        })
    }

    /// Reserve capacity for `additional` more values.
    ///
    /// Computes in **O(n)** time.
    pub fn reserve(&mut self, additional: usize) {
        self.map.reserve(additional);
    }

    /// Reserve capacity for `additional` more values, without over-allocating.
    ///
    /// Unlike `reserve`, this does not deliberately over-allocate the entry capacity to avoid
    /// frequent re-allocations. However, the underlying data structures may still have internal
    /// capacity requirements, and the allocator itself may give more space than requested, so this
    /// cannot be relied upon to be precisely minimal.
    ///
    /// Computes in **O(n)** time.
    pub fn reserve_exact(&mut self, additional: usize) {
        self.map.reserve_exact(additional);
    }

    /// Shrink the capacity of the set as much as possible.
    ///
    /// Computes in **O(n)** time.
    pub fn shrink_to_fit(&mut self) {
        self.map.shrink_to_fit();
    }

    /// Shrink the capacity of the set with a lower limit.
    ///
    /// Computes in **O(n)** time.
    pub fn shrink_to(&mut self, min_capacity: usize) {
        self.map.shrink_to(min_capacity);
    }
}

impl<T, E, G, R, S> RubySet<T, E, G, R, S>
where
    T: Hash + RubyEql<E, G, R>,
    S: BuildHasher,
{
    /// Insert the value into the set.
    ///
    /// If an equivalent item already exists in the set, it returns
    /// `false` leaving the original value in the set and without
    /// altering its insertion order. Otherwise, it inserts the new
    /// item and returns `true`.
    ///
    /// Computes in **O(1)** time (amortized average).
    pub fn insert(&mut self, value: T, e: &mut E, g: &mut G) -> Result<bool, R> {
        Ok(self.map.insert(value, (), e, g)?.is_none())
    }

    /// Insert the value into the set, and get its index.
    ///
    /// If an equivalent item already exists in the set, it returns
    /// the index of the existing item and `false`, leaving the
    /// original value in the set and without altering its insertion
    /// order. Otherwise, it inserts the new item and returns the index
    /// of the inserted item and `true`.
    ///
    /// Computes in **O(1)** time (amortized average).
    pub fn insert_full(&mut self, value: T, e: &mut E, g: &mut G) -> Result<(usize, bool), R> {
        let (index, existing) = self.map.insert_full(value, (), e, g)?;
        Ok((index, existing.is_none()))
    }

    /// Insert the value into the set at its ordered position among sorted values.
    ///
    /// This is equivalent to finding the position with
    /// [`binary_search`][Self::binary_search], and if needed calling
    /// [`insert_before`][Self::insert_before] for a new value.
    ///
    /// If the sorted item is found in the set, it returns the index of that
    /// existing item and `false`, without any change. Otherwise, it inserts the
    /// new item and returns its sorted index and `true`.
    ///
    /// If the existing items are **not** already sorted, then the insertion
    /// index is unspecified (like [`slice::binary_search`]), but the value
    /// is moved to or inserted at that position regardless.
    ///
    /// Computes in **O(n)** time (average). Instead of repeating calls to
    /// `insert_sorted`, it may be faster to call batched [`insert`][Self::insert]
    /// or [`extend`][Self::extend] and only call [`sort`][Self::sort] or
    /// [`sort_unstable`][Self::sort_unstable] once.
    pub fn insert_sorted(&mut self, value: T, e: &mut E, g: &mut G) -> Result<(usize, bool), R>
    where
        T: Ord,
    {
        let (index, existing) = self.map.insert_sorted(value, (), e, g)?;
        Ok((index, existing.is_none()))
    }

    /// Insert the value into the set before the value at the given index, or at the end.
    ///
    /// If an equivalent item already exists in the set, it returns `false` leaving the
    /// original value in the set, but moved to the new position. The returned index
    /// will either be the given index or one less, depending on how the value moved.
    /// (See [`shift_insert`](Self::shift_insert) for different behavior here.)
    ///
    /// Otherwise, it inserts the new value exactly at the given index and returns `true`.
    ///
    /// ***Panics*** if `index` is out of bounds.
    /// Valid indices are `0..=set.len()` (inclusive).
    ///
    /// Computes in **O(n)** time (average).
    ///
    /// # Examples
    ///
    /// ```
    /// use rubymap::RubySet;
    /// let mut set: RubySet<char> = RubySet::from_iter('a'..='z', &mut (), &mut ()).unwrap();
    ///
    /// // The new value '*' goes exactly at the given index.
    /// assert_eq!(set.get_index_of(&'*', &mut (), &mut ()).unwrap(), None);
    /// assert_eq!(set.insert_before(10, '*', &mut (), &mut ()).unwrap(), (10, true));
    /// assert_eq!(set.get_index_of(&'*', &mut (), &mut ()).unwrap(), Some(10));
    ///
    /// // Moving the value 'a' up will shift others down, so this moves *before* 10 to index 9.
    /// assert_eq!(set.insert_before(10, 'a', &mut (), &mut ()).unwrap(), (9, false));
    /// assert_eq!(set.get_index_of(&'a', &mut (), &mut ()).unwrap(), Some(9));
    /// assert_eq!(set.get_index_of(&'*', &mut (), &mut ()).unwrap(), Some(10));
    ///
    /// // Moving the value 'z' down will shift others up, so this moves to exactly 10.
    /// assert_eq!(set.insert_before(10, 'z', &mut (), &mut ()).unwrap(), (10, false));
    /// assert_eq!(set.get_index_of(&'z', &mut (), &mut ()).unwrap(), Some(10));
    /// assert_eq!(set.get_index_of(&'*', &mut (), &mut ()).unwrap(), Some(11));
    ///
    /// // Moving or inserting before the endpoint is also valid.
    /// assert_eq!(set.len(), 27);
    /// assert_eq!(set.insert_before(set.len(), '*', &mut (), &mut ()).unwrap(), (26, false));
    /// assert_eq!(set.get_index_of(&'*', &mut (), &mut ()).unwrap(), Some(26));
    /// assert_eq!(set.insert_before(set.len(), '+', &mut (), &mut ()).unwrap(), (27, true));
    /// assert_eq!(set.get_index_of(&'+', &mut (), &mut ()).unwrap(), Some(27));
    /// assert_eq!(set.len(), 28);
    /// ```
    #[track_caller]
    pub fn insert_before(
        &mut self,
        index: usize,
        value: T,
        e: &mut E,
        g: &mut G,
    ) -> Result<(usize, bool), R> {
        let (index, existing) = self.map.insert_before(index, value, (), e, g)?;
        Ok((index, existing.is_none()))
    }

    /// Insert the value into the set at the given index.
    ///
    /// If an equivalent item already exists in the set, it returns `false` leaving
    /// the original value in the set, but moved to the given index.
    /// Note that existing values **cannot** be moved to `index == set.len()`!
    /// (See [`insert_before`](Self::insert_before) for different behavior here.)
    ///
    /// Otherwise, it inserts the new value at the given index and returns `true`.
    ///
    /// ***Panics*** if `index` is out of bounds.
    /// Valid indices are `0..set.len()` (exclusive) when moving an existing value, or
    /// `0..=set.len()` (inclusive) when inserting a new value.
    ///
    /// Computes in **O(n)** time (average).
    ///
    /// # Examples
    ///
    /// ```
    /// use rubymap::RubySet;
    /// let mut set: RubySet<char> = RubySet::from_iter('a'..='z', &mut (), &mut ()).unwrap();
    ///
    /// // The new value '*' goes exactly at the given index.
    /// assert_eq!(set.get_index_of(&'*', &mut (), &mut ()).unwrap(), None);
    /// assert_eq!(set.shift_insert(10, '*', &mut (), &mut ()).unwrap(), true);
    /// assert_eq!(set.get_index_of(&'*', &mut (), &mut ()).unwrap(), Some(10));
    ///
    /// // Moving the value 'a' up to 10 will shift others down, including the '*' that was at 10.
    /// assert_eq!(set.shift_insert(10, 'a', &mut (), &mut ()).unwrap(), false);
    /// assert_eq!(set.get_index_of(&'a', &mut (), &mut ()).unwrap(), Some(10));
    /// assert_eq!(set.get_index_of(&'*', &mut (), &mut ()).unwrap(), Some(9));
    ///
    /// // Moving the value 'z' down to 9 will shift others up, including the '*' that was at 9.
    /// assert_eq!(set.shift_insert(9, 'z', &mut (), &mut ()).unwrap(), false);
    /// assert_eq!(set.get_index_of(&'z', &mut (), &mut ()).unwrap(), Some(9));
    /// assert_eq!(set.get_index_of(&'*', &mut (), &mut ()).unwrap(), Some(10));
    ///
    /// // Existing values can move to len-1 at most, but new values can insert at the endpoint.
    /// assert_eq!(set.len(), 27);
    /// assert_eq!(set.shift_insert(set.len() - 1, '*', &mut (), &mut ()).unwrap(), false);
    /// assert_eq!(set.get_index_of(&'*', &mut (), &mut ()).unwrap(), Some(26));
    /// assert_eq!(set.shift_insert(set.len(), '+', &mut (), &mut ()).unwrap(), true);
    /// assert_eq!(set.get_index_of(&'+', &mut (), &mut ()).unwrap(), Some(27));
    /// assert_eq!(set.len(), 28);
    /// ```
    ///
    /// ```should_panic
    /// use rubymap::RubySet;
    /// let mut set: RubySet<char> = RubySet::from_iter('a'..='z', &mut (), &mut ()).unwrap();
    ///
    /// // This is an invalid index for moving an existing value!
    /// set.shift_insert(set.len(), 'a', &mut (), &mut ()).unwrap();
    /// ```
    #[track_caller]
    pub fn shift_insert(
        &mut self,
        index: usize,
        value: T,
        e: &mut E,
        g: &mut G,
    ) -> Result<bool, R> {
        Ok(self.map.shift_insert(index, value, (), e, g)?.is_none())
    }

    /// Adds a value to the set, replacing the existing value, if any, that is
    /// equal to the given one, without altering its insertion order. Returns
    /// the replaced value.
    ///
    /// Computes in **O(1)** time (average).
    pub fn replace(&mut self, value: T, e: &mut E, g: &mut G) -> Result<Option<T>, R> {
        Ok(self.replace_full(value, e, g)?.1)
    }

    /// Adds a value to the set, replacing the existing value, if any, that is
    /// equal to the given one, without altering its insertion order. Returns
    /// the index of the item and its replaced value.
    ///
    /// Computes in **O(1)** time (average).
    pub fn replace_full(
        &mut self,
        value: T,
        e: &mut E,
        g: &mut G,
    ) -> Result<(usize, Option<T>), R> {
        let hash = self.map.hash(&value);
        Ok(match self.map.core.replace_full(hash, value, (), e, g)? {
            (i, Some((replaced, ()))) => (i, Some(replaced)),
            (i, None) => (i, None),
        })
    }

    /// Moves all values from `other` into `self`, leaving `other` empty.
    ///
    /// This is equivalent to calling [`insert`][Self::insert] for each value
    /// from `other` in order, which means that values that already exist
    /// in `self` are unchanged in their current position.
    ///
    /// See also [`union`][Self::union] to iterate the combined values by
    /// reference, without modifying `self` or `other`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rubymap::RubySet;
    ///
    /// let mut a: RubySet<_, _> = RubySet::from_ary([3, 2, 1], &mut (), &mut ()).unwrap();
    /// let mut b: RubySet<_, _> = RubySet::from_ary([3, 4, 5], &mut (), &mut ()).unwrap();
    /// let old_capacity = b.capacity();
    ///
    /// a.append(&mut b, &mut (), &mut ()).unwrap();
    ///
    /// assert_eq!(a.len(), 5);
    /// assert_eq!(b.len(), 0);
    /// assert_eq!(b.capacity(), old_capacity);
    ///
    /// assert!(a.iter().eq(&[3, 2, 1, 4, 5]));
    /// ```
    pub fn append<S2>(
        &mut self,
        other: &mut RubySet<T, E, G, R, S2>,
        e: &mut E,
        g: &mut G,
    ) -> Result<(), R> {
        self.map.append(&mut other.map, e, g)
    }
}

impl<T, E, G, R, S> RubySet<T, E, G, R, S>
where
    S: BuildHasher,
{
    /// Return `true` if an equivalent to `value` exists in the set.
    ///
    /// Computes in **O(1)** time (average).
    pub fn contains<Q>(&self, value: &Q, e: &mut E, g: &mut G) -> Result<bool, R>
    where
        Q: ?Sized + Hash + Equivalent<T, E, G, R>,
    {
        Ok(self.map.contains_key(value, e, g)?)
    }

    /// Return a reference to the value stored in the set, if it is present,
    /// else `None`.
    ///
    /// Computes in **O(1)** time (average).
    pub fn get<Q>(&self, value: &Q, e: &mut E, g: &mut G) -> Result<Option<&T>, R>
    where
        Q: ?Sized + Hash + Equivalent<T, E, G, R>,
    {
        Ok(self.map.get_key_value(value, e, g)?.map(|(x, &())| x))
    }

    /// Return item index and value
    pub fn get_full<Q>(&self, value: &Q, e: &mut E, g: &mut G) -> Result<Option<(usize, &T)>, R>
    where
        Q: ?Sized + Hash + Equivalent<T, E, G, R>,
    {
        Ok(self.map.get_full(value, e, g)?.map(|(i, x, &())| (i, x)))
    }

    /// Return item index, if it exists in the set
    ///
    /// Computes in **O(1)** time (average).
    pub fn get_index_of<Q>(&self, value: &Q, e: &mut E, g: &mut G) -> Result<Option<usize>, R>
    where
        Q: ?Sized + Hash + Equivalent<T, E, G, R>,
    {
        self.map.get_index_of(value, e, g)
    }

    /// Remove the value from the set, and return `true` if it was present.
    ///
    /// Like [`Vec::swap_remove`], the value is removed by swapping it with the
    /// last element of the set and popping it off. **This perturbs
    /// the position of what used to be the last element!**
    ///
    /// Return `false` if `value` was not in the set.
    ///
    /// Computes in **O(1)** time (average).
    pub fn swap_remove<Q>(&mut self, value: &Q, e: &mut E, g: &mut G) -> Result<bool, R>
    where
        Q: ?Sized + Hash + Equivalent<T, E, G, R>,
    {
        Ok(self.map.swap_remove(value, e, g)?.is_some())
    }

    /// Remove the value from the set, and return `true` if it was present.
    ///
    /// Like [`Vec::remove`], the value is removed by shifting all of the
    /// elements that follow it, preserving their relative order.
    /// **This perturbs the index of all of those elements!**
    ///
    /// Return `false` if `value` was not in the set.
    ///
    /// Computes in **O(n)** time (average).
    pub fn shift_remove<Q>(&mut self, value: &Q, e: &mut E, g: &mut G) -> Result<bool, R>
    where
        Q: ?Sized + Hash + Equivalent<T, E, G, R>,
    {
        Ok(self.map.shift_remove(value, e, g)?.is_some())
    }

    /// Removes and returns the value in the set, if any, that is equal to the
    /// given one.
    ///
    /// Like [`Vec::swap_remove`], the value is removed by swapping it with the
    /// last element of the set and popping it off. **This perturbs
    /// the position of what used to be the last element!**
    ///
    /// Return `None` if `value` was not in the set.
    ///
    /// Computes in **O(1)** time (average).
    pub fn swap_take<Q>(&mut self, value: &Q, e: &mut E, g: &mut G) -> Result<Option<T>, R>
    where
        Q: ?Sized + Hash + Equivalent<T, E, G, R>,
    {
        Ok(self.map.swap_remove_entry(value, e, g)?.map(|(x, ())| x))
    }

    /// Removes and returns the value in the set, if any, that is equal to the
    /// given one.
    ///
    /// Like [`Vec::remove`], the value is removed by shifting all of the
    /// elements that follow it, preserving their relative order.
    /// **This perturbs the index of all of those elements!**
    ///
    /// Return `None` if `value` was not in the set.
    ///
    /// Computes in **O(n)** time (average).
    pub fn shift_take<Q>(&mut self, value: &Q, e: &mut E, g: &mut G) -> Result<Option<T>, R>
    where
        Q: ?Sized + Hash + Equivalent<T, E, G, R>,
    {
        Ok(self.map.shift_remove_entry(value, e, g)?.map(|(x, ())| x))
    }

    /// Remove the value from the set return it and the index it had.
    ///
    /// Like [`Vec::swap_remove`], the value is removed by swapping it with the
    /// last element of the set and popping it off. **This perturbs
    /// the position of what used to be the last element!**
    ///
    /// Return `None` if `value` was not in the set.
    pub fn swap_remove_full<Q>(
        &mut self,
        value: &Q,
        e: &mut E,
        g: &mut G,
    ) -> Result<Option<(usize, T)>, R>
    where
        Q: ?Sized + Hash + Equivalent<T, E, G, R>,
    {
        Ok(self
            .map
            .swap_remove_full(value, e, g)?
            .map(|(i, x, ())| (i, x)))
    }

    /// Remove the value from the set return it and the index it had.
    ///
    /// Like [`Vec::remove`], the value is removed by shifting all of the
    /// elements that follow it, preserving their relative order.
    /// **This perturbs the index of all of those elements!**
    ///
    /// Return `None` if `value` was not in the set.
    pub fn shift_remove_full<Q>(
        &mut self,
        value: &Q,
        e: &mut E,
        g: &mut G,
    ) -> Result<Option<(usize, T)>, R>
    where
        Q: ?Sized + Hash + Equivalent<T, E, G, R>,
    {
        Ok(self
            .map
            .shift_remove_full(value, e, g)?
            .map(|(i, x, ())| (i, x)))
    }
}

impl<T, E, G, R, S> RubySet<T, E, G, R, S> {
    /// Remove the last value
    ///
    /// This preserves the order of the remaining elements.
    ///
    /// Computes in **O(1)** time (average).
    #[doc(alias = "pop_last")] // like `BTreeSet`
    pub fn pop(&mut self, e: &mut E, g: &mut G) -> Result<Option<T>, R> {
        Ok(self.map.pop(e, g)?.map(|(x, ())| x))
    }

    /// Scan through each value in the set and keep those where the
    /// closure `keep` returns `true`.
    ///
    /// The elements are visited in order, and remaining elements keep their
    /// order.
    ///
    /// Computes in **O(n)** time (average).
    pub fn retain<F>(&mut self, mut keep: F)
    where
        F: FnMut(&T) -> bool,
    {
        self.map.retain(move |x, &mut ()| keep(x))
    }

    /// Sort the set’s values by their default ordering.
    ///
    /// This is a stable sort -- but equivalent values should not normally coexist in
    /// a set at all, so [`sort_unstable`][Self::sort_unstable] is preferred
    /// because it is generally faster and doesn't allocate auxiliary memory.
    ///
    /// See [`sort_by`](Self::sort_by) for details.
    pub fn sort(&mut self)
    where
        T: Ord,
    {
        self.map.sort_keys()
    }

    /// Sort the set’s values in place using the comparison function `cmp`.
    ///
    /// Computes in **O(n log n)** time and **O(n)** space. The sort is stable.
    pub fn sort_by<F>(&mut self, mut cmp: F)
    where
        F: FnMut(&T, &T) -> Ordering,
    {
        self.map.sort_by(move |a, _, b, _| cmp(a, b));
    }

    /// Sort the values of the set and return a by-value iterator of
    /// the values with the result.
    ///
    /// The sort is stable.
    pub fn sorted_by<F>(self, mut cmp: F) -> IntoIter<T>
    where
        F: FnMut(&T, &T) -> Ordering,
    {
        let mut entries = self.into_entries();
        entries.sort_by(move |a, b| cmp(&a.key, &b.key));
        IntoIter::new(entries)
    }

    /// Sort the set's values by their default ordering.
    ///
    /// See [`sort_unstable_by`](Self::sort_unstable_by) for details.
    pub fn sort_unstable(&mut self)
    where
        T: Ord,
    {
        self.map.sort_unstable_keys()
    }

    /// Sort the set's values in place using the comparison function `cmp`.
    ///
    /// Computes in **O(n log n)** time. The sort is unstable.
    pub fn sort_unstable_by<F>(&mut self, mut cmp: F)
    where
        F: FnMut(&T, &T) -> Ordering,
    {
        self.map.sort_unstable_by(move |a, _, b, _| cmp(a, b))
    }

    /// Sort the values of the set and return a by-value iterator of
    /// the values with the result.
    pub fn sorted_unstable_by<F>(self, mut cmp: F) -> IntoIter<T>
    where
        F: FnMut(&T, &T) -> Ordering,
    {
        let mut entries = self.into_entries();
        entries.sort_unstable_by(move |a, b| cmp(&a.key, &b.key));
        IntoIter::new(entries)
    }

    /// Sort the set’s values in place using a key extraction function.
    ///
    /// During sorting, the function is called at most once per entry, by using temporary storage
    /// to remember the results of its evaluation. The order of calls to the function is
    /// unspecified and may change between versions of `indexmap` or the standard library.
    ///
    /// Computes in **O(m n + n log n + c)** time () and **O(n)** space, where the function is
    /// **O(m)**, *n* is the length of the map, and *c* the capacity. The sort is stable.
    pub fn sort_by_cached_key<K, F>(&mut self, mut sort_key: F)
    where
        K: Ord,
        F: FnMut(&T) -> K,
    {
        self.with_entries(move |entries| {
            entries.sort_by_cached_key(move |a| sort_key(&a.key));
        });
    }

    /// Search over a sorted set for a value.
    ///
    /// Returns the position where that value is present, or the position where it can be inserted
    /// to maintain the sort. See [`slice::binary_search`] for more details.
    ///
    /// Computes in **O(log(n))** time, which is notably less scalable than looking the value up
    /// using [`get_index_of`][RubySet::get_index_of], but this can also position missing values.
    pub fn binary_search(&self, x: &T) -> Result<usize, usize>
    where
        T: Ord,
    {
        self.as_slice().binary_search(x)
    }

    /// Search over a sorted set with a comparator function.
    ///
    /// Returns the position where that value is present, or the position where it can be inserted
    /// to maintain the sort. See [`slice::binary_search_by`] for more details.
    ///
    /// Computes in **O(log(n))** time.
    #[inline]
    pub fn binary_search_by<'a, F>(&'a self, f: F) -> Result<usize, usize>
    where
        F: FnMut(&'a T) -> Ordering,
    {
        self.as_slice().binary_search_by(f)
    }

    /// Search over a sorted set with an extraction function.
    ///
    /// Returns the position where that value is present, or the position where it can be inserted
    /// to maintain the sort. See [`slice::binary_search_by_key`] for more details.
    ///
    /// Computes in **O(log(n))** time.
    #[inline]
    pub fn binary_search_by_key<'a, B, F>(&'a self, b: &B, f: F) -> Result<usize, usize>
    where
        F: FnMut(&'a T) -> B,
        B: Ord,
    {
        self.as_slice().binary_search_by_key(b, f)
    }

    /// Returns the index of the partition point of a sorted set according to the given predicate
    /// (the index of the first element of the second partition).
    ///
    /// See [`slice::partition_point`] for more details.
    ///
    /// Computes in **O(log(n))** time.
    #[must_use]
    pub fn partition_point<P>(&self, pred: P) -> usize
    where
        P: FnMut(&T) -> bool,
    {
        self.as_slice().partition_point(pred)
    }

    /// Reverses the order of the set’s values in place.
    ///
    /// Computes in **O(n)** time and **O(1)** space.
    pub fn reverse(&mut self) {
        self.map.reverse()
    }

    /// Returns a slice of all the values in the set.
    ///
    /// Computes in **O(1)** time.
    pub fn as_slice(&self) -> &Slice<T> {
        Slice::from_slice(self.as_entries())
    }

    /// Converts into a boxed slice of all the values in the set.
    ///
    /// Note that this will drop the inner hash table and any excess capacity.
    pub fn into_boxed_slice(self) -> Box<Slice<T>> {
        Slice::from_boxed(self.into_entries().into_boxed_slice())
    }

    /// Get a value by index
    ///
    /// Valid indices are `0 <= index < self.len()`.
    ///
    /// Computes in **O(1)** time.
    pub fn get_index(&self, index: usize) -> Option<&T> {
        self.as_entries().get(index).map(Bucket::key_ref)
    }

    /// Returns a slice of values in the given range of indices.
    ///
    /// Valid indices are `0 <= index < self.len()`.
    ///
    /// Computes in **O(1)** time.
    pub fn get_range<Ra: RangeBounds<usize>>(&self, range: Ra) -> Option<&Slice<T>> {
        let entries = self.as_entries();
        let range = try_simplify_range(range, entries.len())?;
        entries.get(range).map(Slice::from_slice)
    }

    /// Get the first value
    ///
    /// Computes in **O(1)** time.
    pub fn first(&self) -> Option<&T> {
        self.as_entries().first().map(Bucket::key_ref)
    }

    /// Get the last value
    ///
    /// Computes in **O(1)** time.
    pub fn last(&self) -> Option<&T> {
        self.as_entries().last().map(Bucket::key_ref)
    }

    /// Remove the value by index
    ///
    /// Valid indices are `0 <= index < self.len()`.
    ///
    /// Like [`Vec::swap_remove`], the value is removed by swapping it with the
    /// last element of the set and popping it off. **This perturbs
    /// the position of what used to be the last element!**
    ///
    /// Computes in **O(1)** time (average).
    pub fn swap_remove_index(
        &mut self,
        index: usize,
        e: &mut E,
        g: &mut G,
    ) -> Result<Option<T>, R> {
        Ok(self.map.swap_remove_index(index, e, g)?.map(|(x, ())| x))
    }

    /// Remove the value by index
    ///
    /// Valid indices are `0 <= index < self.len()`.
    ///
    /// Like [`Vec::remove`], the value is removed by shifting all of the
    /// elements that follow it, preserving their relative order.
    /// **This perturbs the index of all of those elements!**
    ///
    /// Computes in **O(n)** time (average).
    pub fn shift_remove_index(
        &mut self,
        index: usize,
        e: &mut E,
        g: &mut G,
    ) -> Result<Option<T>, R> {
        Ok(self.map.shift_remove_index(index, e, g)?.map(|(x, ())| x))
    }

    /// Moves the position of a value from one index to another
    /// by shifting all other values in-between.
    ///
    /// * If `from < to`, the other values will shift down while the targeted value moves up.
    /// * If `from > to`, the other values will shift up while the targeted value moves down.
    ///
    /// ***Panics*** if `from` or `to` are out of bounds.
    ///
    /// Computes in **O(n)** time (average).
    #[track_caller]
    pub fn move_index(&mut self, from: usize, to: usize, e: &mut E, g: &mut G) -> Result<(), R> {
        self.map.move_index(from, to, e, g)
    }
}

impl<T, E, G, R, S> RubySet<T, E, G, R, S>
where
    T: Hash + RubyEql<E, G, R>,
    S: BuildHasher + Default,
{
    /// Create a new set from an iterator of values.
    pub fn from_iter<I: IntoIterator<Item = T>>(
        iterable: I,
        e: &mut E,
        g: &mut G,
    ) -> Result<Self, R> {
        let iter = iterable.into_iter().map(|x| (x, ()));
        Ok(RubySet {
            map: RubyMap::from_iter(iter, e, g)?,
        })
    }
}

impl<T, E, G, R> RubySet<T, E, G, R, RandomState>
where
    T: RubyEql<E, G, R> + Hash,
{
    /// # Examples
    ///
    /// ```
    /// use rubymap::RubySet;
    ///
    /// # fn main() {
    /// let set1: RubySet<_> = RubySet::from_ary([1, 2, 3, 4], &mut (), &mut ()).unwrap();
    /// let set2: RubySet<_> = RubySet::from_ary([1, 2, 3, 4], &mut (), &mut ()).unwrap();
    /// assert_eq!(set1, set2);
    /// # }
    /// ```
    pub fn from_ary<const N: usize>(arr: [T; N], e: &mut E, g: &mut G) -> Result<Self, R> {
        Self::from_iter(arr, e, g)
    }
}

impl<T, E, G, R, S> RubySet<T, E, G, R, S>
where
    T: Hash + RubyEql<E, G, R>,
    S: BuildHasher,
{
    pub fn extend<I: IntoIterator<Item = T>>(
        &mut self,
        iterable: I,
        e: &mut E,
        g: &mut G,
    ) -> Result<(), R> {
        let iter = iterable.into_iter().map(|x| (x, ()));
        self.map.extend(iter, e, g)
    }
}

impl<T, E, G, R, S> Default for RubySet<T, E, G, R, S>
where
    S: Default,
{
    /// Return an empty [`RubySet`]
    fn default() -> Self {
        RubySet {
            map: RubyMap::default(),
        }
    }
}

impl<T, E, G, R, S> RubyEql<E, G, R> for RubySet<T, E, G, R, S>
where
    T: Hash + RubyEql<E, G, R>,
    S: BuildHasher,
{
    fn eql(&self, other: &RubySet<T, E, G, R, S>, e: &mut E, g: &mut G) -> Result<bool, R> {
        Ok(self.len() == other.len() && self.is_subset(other, e, g)?)
    }
}

impl<T, E, G, R, S> RubySet<T, E, G, R, S>
where
    T: RubyEql<E, G, R> + Hash,
    S: BuildHasher,
{
    /// Returns `true` if `self` has no elements in common with `other`.
    pub fn is_disjoint(
        &self,
        other: &RubySet<T, E, G, R, S>,
        e: &mut E,
        g: &mut G,
    ) -> Result<bool, R> {
        if self.len() <= other.len() {
            for v in self.iter() {
                if other.contains(v, e, g)? {
                    return Ok(false);
                }
            }
        } else {
            for v in other.iter() {
                if self.contains(v, e, g)? {
                    return Ok(false);
                }
            }
        }
        Ok(true)
    }

    /// Returns `true` if all elements of `self` are contained in `other`.
    pub fn is_subset(
        &self,
        other: &RubySet<T, E, G, R, S>,
        e: &mut E,
        g: &mut G,
    ) -> Result<bool, R> {
        if self.len() > other.len() {
            return Ok(false);
        }
        for v in self.iter() {
            if !other.contains(v, e, g)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// Returns `true` if all elements of `other` are contained in `self`.
    pub fn is_superset(
        &self,
        other: &RubySet<T, E, G, R, S>,
        e: &mut E,
        g: &mut G,
    ) -> Result<bool, R> {
        other.is_subset(self, e, g)
    }
}
