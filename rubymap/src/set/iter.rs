use super::{Bucket, Entries, RubySet, Slice};

use alloc::vec::{self, Vec};
use core::fmt;
use core::iter::FusedIterator;
use core::slice::Iter as SliceIter;

impl<'a, T, E, G, R, S> IntoIterator for &'a RubySet<T, E, G, R, S> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<T, E, G, R, S> IntoIterator for RubySet<T, E, G, R, S> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter::new(self.into_entries())
    }
}

/// An iterator over the items of an [`IndexSet`].
///
/// This `struct` is created by the [`IndexSet::iter`] method.
/// See its documentation for more.
pub struct Iter<'a, T> {
    iter: SliceIter<'a, Bucket<T>>,
}

impl<'a, T> Iter<'a, T> {
    pub(super) fn new(entries: &'a [Bucket<T>]) -> Self {
        Self {
            iter: entries.iter(),
        }
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    iterator_methods!(Bucket::key_ref);
}

impl<T> DoubleEndedIterator for Iter<'_, T> {
    double_ended_iterator_methods!(Bucket::key_ref);
}

impl<T> ExactSizeIterator for Iter<'_, T> {
    fn len(&self) -> usize {
        self.iter.len()
    }
}

impl<T> FusedIterator for Iter<'_, T> {}

impl<T> Clone for Iter<'_, T> {
    fn clone(&self) -> Self {
        Iter {
            iter: self.iter.clone(),
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for Iter<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.clone()).finish()
    }
}

impl<T> Default for Iter<'_, T> {
    fn default() -> Self {
        Self { iter: [].iter() }
    }
}

/// An owning iterator over the items of an [`IndexSet`].
///
/// This `struct` is created by the [`IndexSet::into_iter`] method
/// (provided by the [`IntoIterator`] trait). See its documentation for more.
#[derive(Clone)]
pub struct IntoIter<T> {
    iter: vec::IntoIter<Bucket<T>>,
}

impl<T> IntoIter<T> {
    pub(super) fn new(entries: Vec<Bucket<T>>) -> Self {
        Self {
            iter: entries.into_iter(),
        }
    }
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    iterator_methods!(Bucket::key);
}

impl<T> DoubleEndedIterator for IntoIter<T> {
    double_ended_iterator_methods!(Bucket::key);
}

impl<T> ExactSizeIterator for IntoIter<T> {
    fn len(&self) -> usize {
        self.iter.len()
    }
}

impl<T> FusedIterator for IntoIter<T> {}

impl<T: fmt::Debug> fmt::Debug for IntoIter<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let iter = self.iter.as_slice().iter().map(Bucket::key_ref);
        f.debug_list().entries(iter).finish()
    }
}

impl<T> Default for IntoIter<T> {
    fn default() -> Self {
        Self {
            iter: Vec::new().into_iter(),
        }
    }
}

/// A draining iterator over the items of an [`IndexSet`].
///
/// This `struct` is created by the [`IndexSet::drain`] method.
/// See its documentation for more.
pub struct Drain<'a, T> {
    iter: vec::Drain<'a, Bucket<T>>,
}

impl<'a, T> Drain<'a, T> {
    pub(super) fn new(iter: vec::Drain<'a, Bucket<T>>) -> Self {
        Self { iter }
    }

    /// Returns a slice of the remaining entries in the iterator.
    pub fn as_slice(&self) -> &Slice<T> {
        Slice::from_slice(self.iter.as_slice())
    }
}

impl<T> Iterator for Drain<'_, T> {
    type Item = T;

    iterator_methods!(Bucket::key);
}

impl<T> DoubleEndedIterator for Drain<'_, T> {
    double_ended_iterator_methods!(Bucket::key);
}

impl<T> ExactSizeIterator for Drain<'_, T> {
    fn len(&self) -> usize {
        self.iter.len()
    }
}

impl<T> FusedIterator for Drain<'_, T> {}

impl<T: fmt::Debug> fmt::Debug for Drain<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let iter = self.iter.as_slice().iter().map(Bucket::key_ref);
        f.debug_list().entries(iter).finish()
    }
}
