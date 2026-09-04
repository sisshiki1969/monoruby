//! This is the core implementation that doesn't depend on the hasher at all.
//!
//! The methods of `IndexMapCore` don't use any Hash properties of K.
//!
//! It's cleaner to separate them out, then the compiler checks that we are not
//! using Hash at all in these methods.
//!
//! However, we should probably not let this show in the public API or docs.

mod entry;

//pub mod raw_entry_v1;

use hashbrown::hash_table;
use ruby_traits::SymEquivalent;

use crate::vec::{self, Vec};
use core::mem;
use core::ops::RangeBounds;
use std::borrow::Borrow;

use crate::util::simplify_range;
use crate::{Bucket, Equivalent, HashValue, RubyEql};

type Indices<E, G, R> = hash_table::HashTable<usize, E, G, R>;
type Entries<K, V> = Vec<Bucket<K, V>>;

pub use entry::{Entry, IndexedEntry, OccupiedEntry, VacantEntry};

/// Core of the map that does not depend on S
#[derive(Debug)]
pub(crate) struct IndexMapCore<K, V, E, G, R> {
    /// indices mapping from the entry hash to its index.
    indices: Indices<E, G, R>,
    /// CRuby's ar_table idea: while the map stays at or below
    /// [`AR_MAX`] entries, `indices` is left unbuilt (empty, no
    /// allocation) and lookups scan `entries` linearly, comparing the
    /// stored hash first and `eql` only on a hash match — the same
    /// probe/eql pattern as the indexed path, so nothing observable
    /// changes. The first insert taking the map past `AR_MAX` (or an
    /// operation that needs the table, e.g. the `Entry` API) promotes
    /// via [`Self::ensure_indexed`]; promotion is one-way except
    /// `clear`, which resets to linear.
    ///
    /// Invariants: `linear` ⇒ `indices.is_empty()`;
    /// `!linear` ⇒ `indices.len() == entries.len()`.
    linear: bool,
    /// entries is a dense vec maintaining entry order.
    entries: Entries<K, V>,
}

/// Byte offset of the entry vector within [`IndexMapCore`].
///
/// `IndexMapCore` is `repr(Rust)`, so this must come from the compiler
/// rather than from a hand-computed field order — the monoruby JIT bakes
/// it into generated machine code (see [`crate::entries_layout`]).
pub(crate) const fn entries_offset<K, V, E, G, R>() -> usize {
    std::mem::offset_of!(IndexMapCore<K, V, E, G, R>, entries)
}

/// Byte offset of the `linear` flag inside an `IndexMapCore`, for the same
/// consumer as [`entries_offset`]: generated code reads it to decide whether
/// the entries may be scanned directly (no indices table) or the probe must
/// go through the table.
pub(crate) const fn linear_offset<K, V, E, G, R>() -> usize {
    std::mem::offset_of!(IndexMapCore<K, V, E, G, R>, linear)
}

/// Mutable references to the parts of an `IndexMapCore`.
///
/// When using `HashTable::find_entry`, that takes hold of `&mut indices`, so we have to borrow our
/// `&mut entries` separately, and there's no way to go back to a `&mut IndexMapCore`. So this type
/// is used to implement methods on the split references, and `IndexMapCore` can also call those to
/// avoid duplication.
struct RefMut<'a, K, V, E, G, R> {
    indices: &'a mut Indices<E, G, R>,
    entries: &'a mut Entries<K, V>,
}

#[inline(always)]
fn get_hash<K, V>(entries: &[Bucket<K, V>]) -> impl Fn(&usize) -> u64 + '_ {
    move |&i| entries[i].hash.get()
}

#[inline]
fn equivalent<'a, K, V, E, G, R, Q: ?Sized + Equivalent<K, E, G, R>>(
    key: &'a Q,
    entries: &'a [Bucket<K, V>],
) -> impl Fn(&usize, &mut E, &mut G) -> Result<bool, R> + 'a {
    move |&i, e, g| Q::equivalent(key, &entries[i].key, e, g)
}

#[inline]
fn sym_equivalent<'a, K, V, Q: ?Sized + SymEquivalent<K>>(
    key: &'a Q,
    entries: &'a [Bucket<K, V>],
) -> impl Fn(&usize) -> bool + 'a {
    move |&i| Q::equivalent(key, &entries[i].key)
}

#[inline]
fn erase_index<E, G, R>(
    table: &mut Indices<E, G, R>,
    hash: HashValue,
    index: usize,
    e: &mut E,
    g: &mut G,
) -> Result<(), R> {
    if let Ok(entry) = table.find_entry(hash.get(), move |&i, _, _| Ok(i == index), e, g)? {
        entry.remove();
    } else if cfg!(debug_assertions) {
        panic!("index not found");
    }
    Ok(())
}

#[inline]
fn update_index<E, G, R>(
    table: &mut Indices<E, G, R>,
    hash: HashValue,
    old: usize,
    new: usize,
    e: &mut E,
    g: &mut G,
) -> Result<(), R> {
    let index = table
        .find_mut(hash.get(), move |&i, _, _| Ok(i == old), e, g)?
        .expect("index not found");
    *index = new;
    Ok(())
}

/// Inserts many entries into the indices table without reallocating,
/// and without regard for duplication.
///
/// ***Panics*** if there is not sufficient capacity already.
fn insert_bulk_no_grow<K, V, E, G, R>(indices: &mut Indices<E, G, R>, entries: &[Bucket<K, V>]) {
    assert!(indices.capacity() - indices.len() >= entries.len());
    for entry in entries {
        indices.insert_unique(entry.hash.get(), indices.len(), |_| unreachable!());
    }
}

impl<K, V, E, G, R> Clone for IndexMapCore<K, V, E, G, R>
where
    K: Clone,
    V: Clone,
{
    fn clone(&self) -> Self {
        let mut new = Self::new();
        new.clone_from(self);
        new
    }

    fn clone_from(&mut self, other: &Self) {
        self.linear = other.linear;
        self.indices.clone_from(&other.indices);
        if self.entries.capacity() < other.entries.len() {
            // If we must resize, match the indices capacity.
            let additional = other.entries.len() - self.entries.len();
            self.borrow_mut().reserve_entries(additional);
        }
        self.entries.clone_from(&other.entries);
    }
}

impl<K, V, E, G, R> crate::Entries for IndexMapCore<K, V, E, G, R> {
    type Entry = Bucket<K, V>;

    #[inline]
    fn into_entries(self) -> Vec<Self::Entry> {
        self.entries
    }

    #[inline]
    fn as_entries(&self) -> &[Self::Entry] {
        &self.entries
    }

    #[inline]
    fn as_entries_mut(&mut self) -> &mut [Self::Entry] {
        &mut self.entries
    }

    fn with_entries<F>(&mut self, f: F)
    where
        F: FnOnce(&mut [Self::Entry]),
    {
        f(&mut self.entries);
        self.rebuild_hash_table();
    }
}

/// Entry count up to which the indices table is left unbuilt (see the
/// `linear` field). Matches CRuby's `RHASH_AR_TABLE_MAX_SIZE`.
const AR_MAX: usize = 8;

impl<K, V, E, G, R> IndexMapCore<K, V, E, G, R> {
    /// The maximum capacity before the `entries` allocation would exceed `isize::MAX`.
    const MAX_ENTRIES_CAPACITY: usize = (isize::MAX as usize) / mem::size_of::<Bucket<K, V>>();

    #[inline]
    pub(crate) const fn new() -> Self {
        IndexMapCore {
            indices: Indices::new(),
            entries: Vec::new(),
            linear: true,
        }
    }

    #[inline]
    fn borrow_mut(&mut self) -> RefMut<'_, K, V, E, G, R> {
        RefMut::new(&mut self.indices, &mut self.entries)
    }

    #[inline]
    pub(crate) fn with_capacity(n: usize) -> Self {
        // A small-capacity map starts linear: the indices allocation is
        // deferred until (if ever) it outgrows `AR_MAX`.
        if n <= AR_MAX {
            IndexMapCore {
                indices: Indices::new(),
                entries: Vec::with_capacity(n),
                linear: true,
            }
        } else {
            IndexMapCore {
                indices: Indices::with_capacity(n),
                entries: Vec::with_capacity(n),
                linear: false,
            }
        }
    }

    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.entries.len()
    }

    #[inline]
    pub(crate) fn capacity(&self) -> usize {
        if self.linear {
            self.entries.capacity()
        } else {
            Ord::min(self.indices.capacity(), self.entries.capacity())
        }
    }

    /// Build the indices table from `entries` and leave linear mode.
    /// Idempotent; the entry hashes were computed at insertion time, so
    /// no key is re-hashed.
    pub(super) fn ensure_indexed(&mut self) {
        if self.linear {
            self.indices
                .reserve(self.entries.len(), get_hash(&self.entries));
            insert_bulk_no_grow(&mut self.indices, &self.entries);
            self.linear = false;
        }
    }

    /// Linear-mode lookup: stored-hash compare first, `eql` only on a
    /// hash match — the indexed path's exact probe pattern.
    fn linear_find<Q>(
        &self,
        hash: HashValue,
        key: &Q,
        e: &mut E,
        g: &mut G,
    ) -> Result<Option<usize>, R>
    where
        Q: ?Sized + Equivalent<K, E, G, R>,
    {
        for (i, entry) in self.entries.iter().enumerate() {
            if entry.hash == hash && key.equivalent(&entry.key, e, g)? {
                return Ok(Some(i));
            }
        }
        Ok(None)
    }

    pub(crate) fn clear(&mut self) {
        self.indices.clear();
        self.entries.clear();
        self.linear = true;
    }

    pub(crate) fn truncate(&mut self, len: usize, e: &mut E, g: &mut G) -> Result<(), R> {
        if len < self.len() {
            self.erase_indices(len, self.entries.len(), e, g)?;
            self.entries.truncate(len);
        }
        Ok(())
    }

    #[track_caller]
    pub(crate) fn drain<Ra>(
        &mut self,
        range: Ra,
        e: &mut E,
        g: &mut G,
    ) -> Result<vec::Drain<'_, Bucket<K, V>>, R>
    where
        Ra: RangeBounds<usize>,
    {
        let range = simplify_range(range, self.entries.len());
        self.erase_indices(range.start, range.end, e, g)?;
        Ok(self.entries.drain(range))
    }

    #[track_caller]
    pub(crate) fn split_off(&mut self, at: usize, e: &mut E, g: &mut G) -> Result<Self, R> {
        let len = self.entries.len();
        assert!(
            at <= len,
            "index out of bounds: the len is {len} but the index is {at}. Expected index <= len"
        );

        self.erase_indices(at, self.entries.len(), e, g)?;
        let entries = self.entries.split_off(at);

        if self.linear {
            return Ok(Self {
                indices: Indices::new(),
                entries,
                linear: true,
            });
        }
        let mut indices = Indices::with_capacity(entries.len());
        insert_bulk_no_grow(&mut indices, &entries);
        Ok(Self {
            indices,
            entries,
            linear: false,
        })
    }

    /// Reserve capacity for `additional` more key-value pairs.
    pub(crate) fn reserve(&mut self, additional: usize) {
        // A reservation that must outgrow linear mode builds the table
        // up front (so the bulk insert that follows doesn't pay a
        // per-insert promotion check); a small one stays linear.
        if self.linear {
            if self.entries.len() + additional > AR_MAX {
                self.ensure_indexed();
            } else {
                self.entries.reserve(additional);
                return;
            }
        }
        self.indices.reserve(additional, get_hash(&self.entries));
        // Only grow entries if necessary, since we also round up capacity.
        if additional > self.entries.capacity() - self.entries.len() {
            self.borrow_mut().reserve_entries(additional);
        }
    }

    /// Reserve capacity for `additional` more key-value pairs, without over-allocating.
    pub(crate) fn reserve_exact(&mut self, additional: usize) {
        if self.linear {
            if self.entries.len() + additional > AR_MAX {
                self.ensure_indexed();
            } else {
                self.entries.reserve_exact(additional);
                return;
            }
        }
        self.indices.reserve(additional, get_hash(&self.entries));
        self.entries.reserve_exact(additional);
    }

    /// Shrink the capacity of the map with a lower bound
    pub(crate) fn shrink_to(&mut self, min_capacity: usize) {
        if !self.linear {
            self.indices
                .shrink_to(min_capacity, get_hash(&self.entries));
        }
        self.entries.shrink_to(min_capacity);
    }

    /// Remove the last key-value pair
    pub(crate) fn pop(&mut self, e: &mut E, g: &mut G) -> Result<Option<(K, V)>, R> {
        Ok(if let Some(entry) = self.entries.pop() {
            if !self.linear {
                let last = self.entries.len();
                erase_index(&mut self.indices, entry.hash, last, e, g)?;
            }
            Some((entry.key, entry.value))
        } else {
            None
        })
    }

    /// Return the index in `entries` where an equivalent key can be found
    pub(crate) fn get_index_of<Q>(
        &self,
        hash: HashValue,
        key: &Q,
        e: &mut E,
        g: &mut G,
    ) -> Result<Option<usize>, R>
    where
        Q: ?Sized + Equivalent<K, E, G, R>,
    {
        if self.linear {
            return self.linear_find(hash, key, e, g);
        }
        let eq = equivalent(key, &self.entries);
        Ok(self.indices.find(hash.get(), eq, e, g)?.copied())
    }

    /// Append a key-value pair to `entries`,
    /// *without* checking whether it already exists.
    fn push_entry(&mut self, hash: HashValue, key: K, value: V) {
        if self.entries.len() == self.entries.capacity() {
            // Reserve our own capacity synced to the indices,
            // rather than letting `Vec::push` just double it.
            self.borrow_mut().reserve_entries(1);
        }
        self.entries.push(Bucket { hash, key, value });
    }

    pub(crate) fn insert_full(
        &mut self,
        hash: HashValue,
        key: K,
        value: V,
        e: &mut E,
        g: &mut G,
    ) -> Result<(usize, Option<V>), R>
    where
        K: RubyEql<E, G, R>,
    {
        if self.linear {
            if let Some(i) = self.linear_find(hash, &key, e, g)? {
                return Ok((i, Some(mem::replace(&mut self.entries[i].value, value))));
            }
            if self.entries.len() < AR_MAX {
                let i = self.entries.len();
                self.push_entry(hash, key, value);
                return Ok((i, None));
            }
            // Crossing AR_MAX: build the table once, then fall through.
            self.ensure_indexed();
        }
        let eq = equivalent(&key, &self.entries);
        let hasher = get_hash(&self.entries);
        match self.indices.entry(hash.get(), eq, hasher, e, g)? {
            hash_table::Entry::Occupied(entry) => {
                let i = *entry.get();
                Ok((i, Some(mem::replace(&mut self.entries[i].value, value))))
            }
            hash_table::Entry::Vacant(entry) => {
                let i = self.entries.len();
                entry.insert(i);
                self.push_entry(hash, key, value);
                debug_assert_eq!(self.indices.len(), self.entries.len());
                Ok((i, None))
            }
        }
    }

    pub(crate) fn insert_full_sym<I>(
        &mut self,
        hash: HashValue,
        key: I,
        value: V,
    ) -> (usize, Option<V>)
    where
        I: ruby_traits::RubySymEql,
        K: Borrow<I> + From<I>,
    {
        if self.linear {
            for (i, entry) in self.entries.iter().enumerate() {
                if entry.hash == hash && SymEquivalent::equivalent(&key, entry.key.borrow()) {
                    return (i, Some(mem::replace(&mut self.entries[i].value, value)));
                }
            }
            if self.entries.len() < AR_MAX {
                let i = self.entries.len();
                self.push_entry(hash, K::from(key), value);
                return (i, None);
            }
            self.ensure_indexed();
        }
        let eq = sym_equivalent(&key, &self.entries);
        let hasher = get_hash(&self.entries);
        match self.indices.entry_sym(hash.get(), eq, hasher) {
            hash_table::Entry::Occupied(entry) => {
                let i = *entry.get();
                (i, Some(mem::replace(&mut self.entries[i].value, value)))
            }
            hash_table::Entry::Vacant(entry) => {
                let i = self.entries.len();
                entry.insert(i);
                self.push_entry(hash, K::from(key), value);
                debug_assert_eq!(self.indices.len(), self.entries.len());
                (i, None)
            }
        }
    }

    /// [`Self::insert_full`] for a caller-computed digest and plain `==`
    /// key equality: no `E`/`G` threading, no fallible equivalence. Only
    /// sound when `==` agrees with the map's `RubyEql` for the probe key
    /// (bit-comparable keys — packed `Value`s) and `hash` equals what
    /// [`RubyMap::hash`] would produce for `key`.
    pub(crate) fn insert_full_prehashed(
        &mut self,
        hash: HashValue,
        key: K,
        value: V,
    ) -> (usize, Option<V>)
    where
        K: PartialEq,
    {
        if self.linear {
            for (i, entry) in self.entries.iter().enumerate() {
                if entry.hash == hash && entry.key == key {
                    return (i, Some(mem::replace(&mut self.entries[i].value, value)));
                }
            }
            if self.entries.len() < AR_MAX {
                let i = self.entries.len();
                self.push_entry(hash, key, value);
                return (i, None);
            }
            self.ensure_indexed();
        }
        let entries = &self.entries;
        let eq = |&i: &usize| entries[i].key == key;
        let hasher = get_hash(&self.entries);
        match self.indices.entry_sym(hash.get(), eq, hasher) {
            hash_table::Entry::Occupied(entry) => {
                let i = *entry.get();
                (i, Some(mem::replace(&mut self.entries[i].value, value)))
            }
            hash_table::Entry::Vacant(entry) => {
                let i = self.entries.len();
                entry.insert(i);
                self.push_entry(hash, key, value);
                debug_assert_eq!(self.indices.len(), self.entries.len());
                (i, None)
            }
        }
    }

    /// [`Self::get_index_of`] for a caller-computed digest and plain `==`
    /// key equality (see [`Self::insert_full_prehashed`] for the soundness
    /// conditions). The `E`/`G` refs only feed the index table's fallible
    /// probe signature; the equality closure never touches them.
    pub(crate) fn get_index_of_prehashed(
        &self,
        hash: HashValue,
        key: &K,
        e: &mut E,
        g: &mut G,
    ) -> Result<Option<usize>, R>
    where
        K: PartialEq,
    {
        if self.linear {
            for (i, entry) in self.entries.iter().enumerate() {
                if entry.hash == hash && entry.key == *key {
                    return Ok(Some(i));
                }
            }
            return Ok(None);
        }
        let entries = &self.entries;
        let eq = |&i: &usize, _: &mut E, _: &mut G| Ok(entries[i].key == *key);
        Ok(self.indices.find(hash.get(), eq, e, g)?.copied())
    }

    /// [`Self::insert_full_prehashed`] with a caller-supplied equality
    /// predicate in place of `==`, for keys whose `RubyEql` verdict is
    /// computable without the vm but is not bit equality (Ruby `String`s:
    /// content equality across distinct heap objects). The soundness
    /// conditions are the same, with `eq` standing in for `==`: it must
    /// return exactly what the map's `RubyEql` would for the probe key,
    /// and `hash` must equal what [`RubyMap::hash`] would produce for
    /// `key`.
    pub(crate) fn insert_full_prehashed_with(
        &mut self,
        hash: HashValue,
        key: K,
        value: V,
        mut eq: impl FnMut(&K) -> bool,
    ) -> (usize, Option<V>) {
        if self.linear {
            for (i, entry) in self.entries.iter().enumerate() {
                if entry.hash == hash && eq(&entry.key) {
                    return (i, Some(mem::replace(&mut self.entries[i].value, value)));
                }
            }
            if self.entries.len() < AR_MAX {
                let i = self.entries.len();
                self.push_entry(hash, key, value);
                return (i, None);
            }
            self.ensure_indexed();
        }
        let entries = &self.entries;
        let eq = |&i: &usize| eq(&entries[i].key);
        let hasher = get_hash(&self.entries);
        match self.indices.entry_sym(hash.get(), eq, hasher) {
            hash_table::Entry::Occupied(entry) => {
                let i = *entry.get();
                (i, Some(mem::replace(&mut self.entries[i].value, value)))
            }
            hash_table::Entry::Vacant(entry) => {
                let i = self.entries.len();
                entry.insert(i);
                self.push_entry(hash, key, value);
                debug_assert_eq!(self.indices.len(), self.entries.len());
                (i, None)
            }
        }
    }

    /// [`Self::get_index_of_prehashed`] with a caller-supplied equality
    /// predicate in place of `==` (see
    /// [`Self::insert_full_prehashed_with`] for the soundness
    /// conditions). As with the `==` variant, the `E`/`G` refs only feed
    /// the index table's fallible probe signature; the predicate never
    /// touches them.
    pub(crate) fn get_index_of_prehashed_with(
        &self,
        hash: HashValue,
        mut eq: impl FnMut(&K) -> bool,
        e: &mut E,
        g: &mut G,
    ) -> Result<Option<usize>, R> {
        if self.linear {
            for (i, entry) in self.entries.iter().enumerate() {
                if entry.hash == hash && eq(&entry.key) {
                    return Ok(Some(i));
                }
            }
            return Ok(None);
        }
        let entries = &self.entries;
        let eq = |&i: &usize, _: &mut E, _: &mut G| Ok(eq(&entries[i].key));
        Ok(self.indices.find(hash.get(), eq, e, g)?.copied())
    }

    /// Same as `insert_full`, except it also replaces the key
    pub(crate) fn replace_full(
        &mut self,
        hash: HashValue,
        key: K,
        value: V,
        e: &mut E,
        g: &mut G,
    ) -> Result<(usize, Option<(K, V)>), R>
    where
        K: RubyEql<E, G, R>,
    {
        self.ensure_indexed();
        let eq = equivalent(&key, &self.entries);
        let hasher = get_hash(&self.entries);
        match self.indices.entry(hash.get(), eq, hasher, e, g)? {
            hash_table::Entry::Occupied(entry) => {
                let i = *entry.get();
                let entry = &mut self.entries[i];
                let kv = (
                    mem::replace(&mut entry.key, key),
                    mem::replace(&mut entry.value, value),
                );
                Ok((i, Some(kv)))
            }
            hash_table::Entry::Vacant(entry) => {
                let i = self.entries.len();
                entry.insert(i);
                self.push_entry(hash, key, value);
                debug_assert_eq!(self.indices.len(), self.entries.len());
                Ok((i, None))
            }
        }
    }

    /// Remove `key` from the *index table only*, leaving its bucket in
    /// place overwritten with the caller's tombstone key/value. Entry
    /// positions therefore stay stable — nothing shifts — which is what a
    /// traversal that is concurrently walking the entries by index needs.
    ///
    /// The map is promoted to indexed form first: linear lookups scan the
    /// entry vector directly and would otherwise have to know how to skip
    /// dead buckets. Once indexed, lookups can only reach a bucket through
    /// `indices`, which no longer references the dead one, so neither the
    /// tombstone key nor its stale cached hash is ever consulted. The
    /// caller owns the other half of the bargain: while tombstones exist it
    /// must not trigger anything that walks the raw buckets as live entries
    /// (insertion/rehash — barred during Ruby iteration anyway — or the
    /// plain iterators, which the caller is expected to filter).
    ///
    /// Returns the displaced `(index, key, value)`; the caller is expected
    /// to compact with [`Self::compact_tombstones`] when the traversal
    /// window closes.
    pub(crate) fn tombstone_remove_full<Q>(
        &mut self,
        hash: HashValue,
        key: &Q,
        dead_key: K,
        dead_value: V,
        e: &mut E,
        g: &mut G,
    ) -> Result<Option<(usize, K, V)>, R>
    where
        Q: ?Sized + Equivalent<K, E, G, R>,
    {
        self.ensure_indexed();
        let eq = equivalent(key, &self.entries);
        Ok(match self.indices.find_entry(hash.get(), eq, e, g)? {
            Ok(entry) => {
                let (index, _) = entry.remove();
                let bucket = &mut self.entries[index];
                let key = std::mem::replace(&mut bucket.key, dead_key);
                let value = std::mem::replace(&mut bucket.value, dead_value);
                Some((index, key, value))
            }
            Err(_) => None,
        })
    }

    /// [`Self::tombstone_remove_full`] for a caller-chosen entry position
    /// (`Hash#shift` tombstoning the first live entry). Same contract.
    pub(crate) fn tombstone_index(
        &mut self,
        index: usize,
        dead_key: K,
        dead_value: V,
        e: &mut E,
        g: &mut G,
    ) -> Result<Option<(K, V)>, R> {
        if index >= self.entries.len() {
            return Ok(None);
        }
        self.ensure_indexed();
        let hash = self.entries[index].hash;
        erase_index(&mut self.indices, hash, index, e, g)?;
        let bucket = &mut self.entries[index];
        let key = std::mem::replace(&mut bucket.key, dead_key);
        let value = std::mem::replace(&mut bucket.value, dead_value);
        Ok(Some((key, value)))
    }

    /// Rebuild this core with every key passed through `f`, preserving the
    /// stored hashes, the entry order, and the index table (which stores
    /// positions, not keys, so it moves verbatim). Sound only when `f`
    /// preserves hash/eql semantics — e.g. wrapping keys in `Some`.
    pub(crate) fn map_keys<K2>(self, mut f: impl FnMut(K) -> K2) -> IndexMapCore<K2, V, E, G, R> {
        IndexMapCore {
            indices: self.indices,
            linear: self.linear,
            entries: self
                .entries
                .into_iter()
                .map(|b| Bucket {
                    hash: b.hash,
                    key: f(b.key),
                    value: b.value,
                })
                .collect(),
        }
    }

    /// Drop every bucket whose key `is_dead` and rebuild the index table
    /// over the survivors. Closes a tombstone window opened by the
    /// `tombstone_*` methods; positions compact back to dense entry order.
    pub(crate) fn compact_tombstones(&mut self, is_dead: impl Fn(&K) -> bool) {
        self.entries.retain(|b| !is_dead(&b.key));
        if !self.linear {
            self.indices.clear();
            self.indices
                .reserve(self.entries.len(), get_hash(&self.entries));
            insert_bulk_no_grow(&mut self.indices, &self.entries);
        }
    }

    /// Remove an entry by shifting all entries that follow it
    pub(crate) fn shift_remove_full<Q>(
        &mut self,
        hash: HashValue,
        key: &Q,
        e: &mut E,
        g: &mut G,
    ) -> Result<Option<(usize, K, V)>, R>
    where
        Q: ?Sized + Equivalent<K, E, G, R>,
    {
        if self.linear {
            return Ok(match self.linear_find(hash, key, e, g)? {
                Some(index) => {
                    let entry = self.entries.remove(index);
                    Some((index, entry.key, entry.value))
                }
                None => None,
            });
        }
        let eq = equivalent(key, &self.entries);
        Ok(match self.indices.find_entry(hash.get(), eq, e, g)? {
            Ok(entry) => {
                let (index, _) = entry.remove();
                let (key, value) = self.borrow_mut().shift_remove_finish(index, e, g)?;
                Some((index, key, value))
            }
            Err(_) => None,
        })
    }

    /// Remove an entry by shifting all entries that follow it
    #[inline]
    pub(crate) fn shift_remove_index(
        &mut self,
        index: usize,
        e: &mut E,
        g: &mut G,
    ) -> Result<Option<(K, V)>, R> {
        if self.linear {
            return Ok(if index < self.entries.len() {
                let entry = self.entries.remove(index);
                Some((entry.key, entry.value))
            } else {
                None
            });
        }
        self.borrow_mut().shift_remove_index(index, e, g)
    }

    #[inline]
    #[track_caller]
    pub(super) fn move_index(
        &mut self,
        from: usize,
        to: usize,
        e: &mut E,
        g: &mut G,
    ) -> Result<(), R> {
        if self.linear {
            // Pure entry rotation; there are no indices to fix up.
            let _ = &self.entries[from];
            let _ = &self.entries[to];
            if from < to {
                self.entries[from..=to].rotate_left(1);
            } else {
                self.entries[to..=from].rotate_right(1);
            }
            return Ok(());
        }
        self.borrow_mut().move_index(from, to, e, g)
    }

    /// Remove an entry by swapping it with the last
    pub(crate) fn swap_remove_full<Q>(
        &mut self,
        hash: HashValue,
        key: &Q,
        e: &mut E,
        g: &mut G,
    ) -> Result<Option<(usize, K, V)>, R>
    where
        Q: ?Sized + Equivalent<K, E, G, R>,
    {
        if self.linear {
            return Ok(match self.linear_find(hash, key, e, g)? {
                Some(index) => {
                    let entry = self.entries.swap_remove(index);
                    Some((index, entry.key, entry.value))
                }
                None => None,
            });
        }
        let eq = equivalent(key, &self.entries);
        Ok(match self.indices.find_entry(hash.get(), eq, e, g)? {
            Ok(entry) => {
                let (index, _) = entry.remove();
                let (key, value) = self.borrow_mut().swap_remove_finish(index, e, g)?;
                Some((index, key, value))
            }
            Err(_) => None,
        })
    }

    /// Remove an entry by swapping it with the last
    #[inline]
    pub(crate) fn swap_remove_index(
        &mut self,
        index: usize,
        e: &mut E,
        g: &mut G,
    ) -> Result<Option<(K, V)>, R> {
        if self.linear {
            return Ok(if index < self.entries.len() {
                let entry = self.entries.swap_remove(index);
                Some((entry.key, entry.value))
            } else {
                None
            });
        }
        self.borrow_mut().swap_remove_index(index, e, g)
    }

    /// Erase `start..end` from `indices`, and shift `end..` indices down to `start..`
    ///
    /// All of these items should still be at their original location in `entries`.
    /// This is used by `drain`, which will let `Vec::drain` do the work on `entries`.
    fn erase_indices(&mut self, start: usize, end: usize, e: &mut E, g: &mut G) -> Result<(), R> {
        if self.linear {
            // No table to fix up; the caller adjusts `entries` itself.
            return Ok(());
        }
        let (init, shifted_entries) = self.entries.split_at(end);
        let (start_entries, erased_entries) = init.split_at(start);

        let erased = erased_entries.len();
        let shifted = shifted_entries.len();
        let half_capacity = self.indices.capacity() / 2;

        // Use a heuristic between different strategies
        if erased == 0 {
            // Degenerate case, nothing to do
        } else if start + shifted < half_capacity && start < erased {
            // Reinsert everything, as there are few kept indices
            self.indices.clear();

            // Reinsert stable indices, then shifted indices
            insert_bulk_no_grow(&mut self.indices, start_entries);
            insert_bulk_no_grow(&mut self.indices, shifted_entries);
        } else if erased + shifted < half_capacity {
            // Find each affected index, as there are few to adjust

            // Find erased indices
            for (i, entry) in (start..).zip(erased_entries) {
                erase_index(&mut self.indices, entry.hash, i, e, g)?;
            }

            // Find shifted indices
            for ((new, old), entry) in (start..).zip(end..).zip(shifted_entries) {
                update_index(&mut self.indices, entry.hash, old, new, e, g)?;
            }
        } else {
            // Sweep the whole table for adjustments
            let offset = end - start;
            self.indices.retain(move |i| {
                if *i >= end {
                    *i -= offset;
                    true
                } else {
                    *i < start
                }
            });
        }

        debug_assert_eq!(self.indices.len(), start + shifted);
        Ok(())
    }

    pub(crate) fn retain_in_order<F>(&mut self, mut keep: F)
    where
        F: FnMut(&mut K, &mut V) -> bool,
    {
        self.entries
            .retain_mut(|entry| keep(&mut entry.key, &mut entry.value));
        if !self.linear && self.entries.len() < self.indices.len() {
            self.rebuild_hash_table();
        }
    }

    fn rebuild_hash_table(&mut self) {
        if self.linear {
            debug_assert!(self.indices.is_empty());
            return;
        }
        self.indices.clear();
        insert_bulk_no_grow(&mut self.indices, &self.entries);
    }

    pub(crate) fn reverse(&mut self) {
        self.entries.reverse();

        // No need to save hash indices, can easily calculate what they should
        // be, given that this is an in-place reversal.
        let len = self.entries.len();
        for i in &mut self.indices {
            *i = len - *i - 1;
        }
    }
}

/// Reserve entries capacity, rounded up to match the indices (via `try_capacity`).
fn reserve_entries<K, V, E, G, R>(
    entries: &mut Entries<K, V>,
    additional: usize,
    try_capacity: usize,
) {
    // Use a soft-limit on the maximum capacity, but if the caller explicitly
    // requested more, do it and let them have the resulting panic.
    let try_capacity = try_capacity.min(IndexMapCore::<K, V, E, G, R>::MAX_ENTRIES_CAPACITY);
    // In linear mode the indices table has capacity 0, so the sync-up
    // target can sit below the current length; saturate instead of
    // underflowing (try_add == 0 then falls through to reserve_exact).
    let try_add = try_capacity.saturating_sub(entries.len());
    if try_add > additional && entries.try_reserve_exact(try_add).is_ok() {
        return;
    }
    entries.reserve_exact(additional);
}

impl<'a, K, V, E, G, R> RefMut<'a, K, V, E, G, R> {
    #[inline]
    fn new(indices: &'a mut Indices<E, G, R>, entries: &'a mut Entries<K, V>) -> Self {
        Self { indices, entries }
    }

    /// Reserve entries capacity, rounded up to match the indices
    #[inline]
    fn reserve_entries(&mut self, additional: usize) {
        reserve_entries::<K, V, E, G, R>(self.entries, additional, self.indices.capacity());
    }

    /// Insert a key-value pair in `entries`,
    /// *without* checking whether it already exists.
    fn insert_unique(self, hash: HashValue, key: K, value: V) -> OccupiedEntry<'a, K, V, E, G, R> {
        let i = self.indices.len();
        debug_assert_eq!(i, self.entries.len());
        let entry = self
            .indices
            .insert_unique(hash.get(), i, get_hash(self.entries));
        if self.entries.len() == self.entries.capacity() {
            // We can't call `indices.capacity()` while this `entry` has borrowed it, so we'll have
            // to amortize growth on our own. It's still an improvement over the basic `Vec::push`
            // doubling though, since we also consider `MAX_ENTRIES_CAPACITY`.
            reserve_entries::<K, V, E, G, R>(self.entries, 1, 2 * self.entries.capacity());
        }
        self.entries.push(Bucket { hash, key, value });
        OccupiedEntry::new(self.entries, entry)
    }

    /// Insert a key-value pair in `entries` at a particular index,
    /// *without* checking whether it already exists.
    fn shift_insert_unique(
        &mut self,
        index: usize,
        hash: HashValue,
        key: K,
        value: V,
        e: &mut E,
        g: &mut G,
    ) -> Result<(), R> {
        let end = self.indices.len();
        assert!(index <= end);
        // Increment others first so we don't have duplicate indices.
        self.increment_indices(index, end, e, g)?;
        let entries = &*self.entries;
        self.indices.insert_unique(hash.get(), index, move |&i| {
            // Adjust for the incremented indices to find hashes.
            debug_assert_ne!(i, index);
            let i = if i < index { i } else { i - 1 };
            entries[i].hash.get()
        });
        if self.entries.len() == self.entries.capacity() {
            // Reserve our own capacity synced to the indices,
            // rather than letting `Vec::insert` just double it.
            self.reserve_entries(1);
        }
        self.entries.insert(index, Bucket { hash, key, value });
        Ok(())
    }

    /// Remove an entry by shifting all entries that follow it
    fn shift_remove_index(
        &mut self,
        index: usize,
        e: &mut E,
        g: &mut G,
    ) -> Result<Option<(K, V)>, R> {
        Ok(match self.entries.get(index) {
            Some(entry) => {
                erase_index(self.indices, entry.hash, index, e, g)?;
                Some(self.shift_remove_finish(index, e, g)?)
            }
            None => None,
        })
    }

    /// Remove an entry by shifting all entries that follow it
    ///
    /// The index should already be removed from `self.indices`.
    fn shift_remove_finish(&mut self, index: usize, e: &mut E, g: &mut G) -> Result<(K, V), R> {
        // Correct indices that point to the entries that followed the removed entry.
        self.decrement_indices(index + 1, self.entries.len(), e, g)?;

        // Use Vec::remove to actually remove the entry.
        let entry = self.entries.remove(index);
        Ok((entry.key, entry.value))
    }

    /// Remove an entry by swapping it with the last
    fn swap_remove_index(
        &mut self,
        index: usize,
        e: &mut E,
        g: &mut G,
    ) -> Result<Option<(K, V)>, R> {
        Ok(match self.entries.get(index) {
            Some(entry) => {
                erase_index(self.indices, entry.hash, index, e, g)?;
                Some(self.swap_remove_finish(index, e, g)?)
            }
            None => None,
        })
    }

    /// Finish removing an entry by swapping it with the last
    ///
    /// The index should already be removed from `self.indices`.
    fn swap_remove_finish(&mut self, index: usize, e: &mut E, g: &mut G) -> Result<(K, V), R> {
        // use swap_remove, but then we need to update the index that points
        // to the other entry that has to move
        let entry = self.entries.swap_remove(index);

        // correct index that points to the entry that had to swap places
        if let Some(entry) = self.entries.get(index) {
            // was not last element
            // examine new element in `index` and find it in indices
            let last = self.entries.len();
            update_index(self.indices, entry.hash, last, index, e, g)?;
        }

        Ok((entry.key, entry.value))
    }

    /// Decrement all indices in the range `start..end`.
    ///
    /// The index `start - 1` should not exist in `self.indices`.
    /// All entries should still be in their original positions.
    fn decrement_indices(
        &mut self,
        start: usize,
        end: usize,
        e: &mut E,
        g: &mut G,
    ) -> Result<(), R> {
        // Use a heuristic between a full sweep vs. a `find()` for every shifted item.
        let shifted_entries = &self.entries[start..end];
        if shifted_entries.len() > self.indices.capacity() / 2 {
            // Shift all indices in range.
            for i in &mut *self.indices {
                if start <= *i && *i < end {
                    *i -= 1;
                }
            }
        } else {
            // Find each entry in range to shift its index.
            for (i, entry) in (start..end).zip(shifted_entries) {
                update_index(self.indices, entry.hash, i, i - 1, e, g)?;
            }
        }
        Ok(())
    }

    /// Increment all indices in the range `start..end`.
    ///
    /// The index `end` should not exist in `self.indices`.
    /// All entries should still be in their original positions.
    fn increment_indices(
        &mut self,
        start: usize,
        end: usize,
        e: &mut E,
        g: &mut G,
    ) -> Result<(), R> {
        // Use a heuristic between a full sweep vs. a `find()` for every shifted item.
        let shifted_entries = &self.entries[start..end];
        if shifted_entries.len() > self.indices.capacity() / 2 {
            // Shift all indices in range.
            for i in &mut *self.indices {
                if start <= *i && *i < end {
                    *i += 1;
                }
            }
        } else {
            // Find each entry in range to shift its index, updated in reverse so
            // we never have duplicated indices that might have a hash collision.
            for (i, entry) in (start..end).zip(shifted_entries).rev() {
                update_index(self.indices, entry.hash, i, i + 1, e, g)?;
            }
        }
        Ok(())
    }

    #[track_caller]
    fn move_index(&mut self, from: usize, to: usize, e: &mut E, g: &mut G) -> Result<(), R> {
        let from_hash = self.entries[from].hash;
        let _ = self.entries[to]; // explicit bounds check
        if from != to {
            // Use a sentinel index so other indices don't collide.
            update_index(self.indices, from_hash, from, usize::MAX, e, g)?;

            // Update all other indices and rotate the entry positions.
            if from < to {
                self.decrement_indices(from + 1, to + 1, e, g)?;
                self.entries[from..=to].rotate_left(1);
            } else if to < from {
                self.increment_indices(to, from, e, g)?;
                self.entries[to..=from].rotate_right(1);
            }

            // Change the sentinel index to its final position.
            update_index(self.indices, from_hash, usize::MAX, to, e, g)?;
        }
        Ok(())
    }
}
