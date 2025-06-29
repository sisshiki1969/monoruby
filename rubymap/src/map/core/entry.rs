use super::{equivalent, Entries, IndexMapCore, RefMut, RubyEql};
use crate::HashValue;
use core::{fmt, mem};
use hashbrown::hash_table;

impl<K, V, E, G, R> IndexMapCore<K, V, E, G, R> {
    pub(crate) fn entry(
        &mut self,
        hash: HashValue,
        key: K,
        e: &mut E,
        g: &mut G,
    ) -> Result<Entry<'_, K, V, E, G, R>, R>
    where
        K: RubyEql<E, G, R>,
    {
        let entries = &mut self.entries;
        let eq = equivalent(&key, entries);
        Ok(match self.indices.find_entry(hash.get(), eq, e, g)? {
            Ok(index) => Entry::Occupied(OccupiedEntry { entries, index }),
            Err(absent) => Entry::Vacant(VacantEntry {
                map: RefMut::new(absent.into_table(), entries),
                hash,
                key,
            }),
        })
    }
}

/// Entry for an existing key-value pair in an [`IndexMap`][crate::IndexMap]
/// or a vacant location to insert one.
pub enum Entry<'a, K, V, E, G, R> {
    /// Existing slot with equivalent key.
    Occupied(OccupiedEntry<'a, K, V, E, G, R>),
    /// Vacant slot (no equivalent key in the map).
    Vacant(VacantEntry<'a, K, V, E, G, R>),
}

impl<'a, K, V, E, G, R> Entry<'a, K, V, E, G, R> {
    /// Return the index where the key-value pair exists or will be inserted.
    pub fn index(&self) -> usize {
        match *self {
            Entry::Occupied(ref entry) => entry.index(),
            Entry::Vacant(ref entry) => entry.index(),
        }
    }

    /// Sets the value of the entry (after inserting if vacant), and returns an `OccupiedEntry`.
    ///
    /// Computes in **O(1)** time (amortized average).
    pub fn insert_entry(self, value: V) -> OccupiedEntry<'a, K, V, E, G, R> {
        match self {
            Entry::Occupied(mut entry) => {
                entry.insert(value);
                entry
            }
            Entry::Vacant(entry) => entry.insert_entry(value),
        }
    }

    /// Inserts the given default value in the entry if it is vacant and returns a mutable
    /// reference to it. Otherwise a mutable reference to an already existent value is returned.
    ///
    /// Computes in **O(1)** time (amortized average).
    pub fn or_insert(self, default: V) -> &'a mut V {
        match self {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => entry.insert(default),
        }
    }

    /// Inserts the result of the `call` function in the entry if it is vacant and returns a mutable
    /// reference to it. Otherwise a mutable reference to an already existent value is returned.
    ///
    /// Computes in **O(1)** time (amortized average).
    pub fn or_insert_with<F>(self, call: F) -> &'a mut V
    where
        F: FnOnce() -> V,
    {
        match self {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => entry.insert(call()),
        }
    }

    /// Inserts the result of the `call` function with a reference to the entry's key if it is
    /// vacant, and returns a mutable reference to the new value. Otherwise a mutable reference to
    /// an already existent value is returned.
    ///
    /// Computes in **O(1)** time (amortized average).
    pub fn or_insert_with_key<F>(self, call: F) -> &'a mut V
    where
        F: FnOnce(&K) -> V,
    {
        match self {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => {
                let value = call(&entry.key);
                entry.insert(value)
            }
        }
    }

    /// Gets a reference to the entry's key, either within the map if occupied,
    /// or else the new key that was used to find the entry.
    pub fn key(&self) -> &K {
        match *self {
            Entry::Occupied(ref entry) => entry.key(),
            Entry::Vacant(ref entry) => entry.key(),
        }
    }

    /// Modifies the entry if it is occupied.
    pub fn and_modify<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut V),
    {
        if let Entry::Occupied(entry) = &mut self {
            f(entry.get_mut());
        }
        self
    }

    /// Inserts a default-constructed value in the entry if it is vacant and returns a mutable
    /// reference to it. Otherwise a mutable reference to an already existent value is returned.
    ///
    /// Computes in **O(1)** time (amortized average).
    pub fn or_default(self) -> &'a mut V
    where
        V: Default,
    {
        match self {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => entry.insert(V::default()),
        }
    }
}

impl<K: fmt::Debug, V: fmt::Debug, E, G, R> fmt::Debug for Entry<'_, K, V, E, G, R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut tuple = f.debug_tuple("Entry");
        match self {
            Entry::Vacant(v) => tuple.field(v),
            Entry::Occupied(o) => tuple.field(o),
        };
        tuple.finish()
    }
}

/// A view into an occupied entry in an [`IndexMap`][crate::IndexMap].
/// It is part of the [`Entry`] enum.
pub struct OccupiedEntry<'a, K, V, E, G, R> {
    entries: &'a mut Entries<K, V>,
    index: hash_table::OccupiedEntry<'a, usize, E, G, R>,
}

impl<'a, K, V, E, G, R> OccupiedEntry<'a, K, V, E, G, R> {
    pub(crate) fn new(
        entries: &'a mut Entries<K, V>,
        index: hash_table::OccupiedEntry<'a, usize, E, G, R>,
    ) -> Self {
        Self { entries, index }
    }

    /// Return the index of the key-value pair
    #[inline]
    pub fn index(&self) -> usize {
        *self.index.get()
    }

    #[inline]
    fn into_ref_mut(self) -> RefMut<'a, K, V, E, G, R> {
        RefMut::new(self.index.into_table(), self.entries)
    }

    /// Gets a reference to the entry's key in the map.
    ///
    /// Note that this is not the key that was used to find the entry. There may be an observable
    /// difference if the key type has any distinguishing features outside of `Hash` and `Eq`, like
    /// extra fields or the memory address of an allocation.
    pub fn key(&self) -> &K {
        &self.entries[self.index()].key
    }

    /// Gets a reference to the entry's value in the map.
    pub fn get(&self) -> &V {
        &self.entries[self.index()].value
    }

    /// Gets a mutable reference to the entry's value in the map.
    ///
    /// If you need a reference which may outlive the destruction of the
    /// [`Entry`] value, see [`into_mut`][Self::into_mut].
    pub fn get_mut(&mut self) -> &mut V {
        let index = self.index();
        &mut self.entries[index].value
    }

    /// Converts into a mutable reference to the entry's value in the map,
    /// with a lifetime bound to the map itself.
    pub fn into_mut(self) -> &'a mut V {
        let index = self.index();
        &mut self.entries[index].value
    }

    /*pub(super) fn into_muts(self) -> (&'a mut K, &'a mut V) {
        let index = self.index();
        self.entries[index].muts()
    }*/

    /// Sets the value of the entry to `value`, and returns the entry's old value.
    pub fn insert(&mut self, value: V) -> V {
        mem::replace(self.get_mut(), value)
    }

    /// Remove the key, value pair stored in the map for this entry, and return the value.
    ///
    /// **NOTE:** This is equivalent to [`.swap_remove()`][Self::swap_remove], replacing this
    /// entry's position with the last element, and it is deprecated in favor of calling that
    /// explicitly. If you need to preserve the relative order of the keys in the map, use
    /// [`.shift_remove()`][Self::shift_remove] instead.
    #[deprecated(note = "`remove` disrupts the map order -- \
        use `swap_remove` or `shift_remove` for explicit behavior.")]
    pub fn remove(self, e: &mut E, g: &mut G) -> Result<V, R> {
        self.swap_remove(e, g)
    }

    /// Remove the key, value pair stored in the map for this entry, and return the value.
    ///
    /// Like [`Vec::swap_remove`][crate::Vec::swap_remove], the pair is removed by swapping it with
    /// the last element of the map and popping it off.
    /// **This perturbs the position of what used to be the last element!**
    ///
    /// Computes in **O(1)** time (average).
    pub fn swap_remove(self, e: &mut E, g: &mut G) -> Result<V, R> {
        Ok(self.swap_remove_entry(e, g)?.1)
    }

    /// Remove the key, value pair stored in the map for this entry, and return the value.
    ///
    /// Like [`Vec::remove`][crate::Vec::remove], the pair is removed by shifting all of the
    /// elements that follow it, preserving their relative order.
    /// **This perturbs the index of all of those elements!**
    ///
    /// Computes in **O(n)** time (average).
    pub fn shift_remove(self, e: &mut E, g: &mut G) -> Result<V, R> {
        Ok(self.shift_remove_entry(e, g)?.1)
    }

    /// Remove and return the key, value pair stored in the map for this entry
    ///
    /// **NOTE:** This is equivalent to [`.swap_remove_entry()`][Self::swap_remove_entry],
    /// replacing this entry's position with the last element, and it is deprecated in favor of
    /// calling that explicitly. If you need to preserve the relative order of the keys in the map,
    /// use [`.shift_remove_entry()`][Self::shift_remove_entry] instead.
    #[deprecated(note = "`remove_entry` disrupts the map order -- \
        use `swap_remove_entry` or `shift_remove_entry` for explicit behavior.")]
    pub fn remove_entry(self, e: &mut E, g: &mut G) -> Result<(K, V), R> {
        self.swap_remove_entry(e, g)
    }

    /// Remove and return the key, value pair stored in the map for this entry
    ///
    /// Like [`Vec::swap_remove`][crate::Vec::swap_remove], the pair is removed by swapping it with
    /// the last element of the map and popping it off.
    /// **This perturbs the position of what used to be the last element!**
    ///
    /// Computes in **O(1)** time (average).
    pub fn swap_remove_entry(self, e: &mut E, g: &mut G) -> Result<(K, V), R> {
        let (index, entry) = self.index.remove();
        RefMut::new(entry.into_table(), self.entries).swap_remove_finish(index, e, g)
    }

    /// Remove and return the key, value pair stored in the map for this entry
    ///
    /// Like [`Vec::remove`][crate::Vec::remove], the pair is removed by shifting all of the
    /// elements that follow it, preserving their relative order.
    /// **This perturbs the index of all of those elements!**
    ///
    /// Computes in **O(n)** time (average).
    pub fn shift_remove_entry(self, e: &mut E, g: &mut G) -> Result<(K, V), R> {
        let (index, entry) = self.index.remove();
        RefMut::new(entry.into_table(), self.entries).shift_remove_finish(index, e, g)
    }

    /// Moves the position of the entry to a new index
    /// by shifting all other entries in-between.
    ///
    /// This is equivalent to [`IndexMap::move_index`][`crate::IndexMap::move_index`]
    /// coming `from` the current [`.index()`][Self::index].
    ///
    /// * If `self.index() < to`, the other pairs will shift down while the targeted pair moves up.
    /// * If `self.index() > to`, the other pairs will shift up while the targeted pair moves down.
    ///
    /// ***Panics*** if `to` is out of bounds.
    ///
    /// Computes in **O(n)** time (average).
    #[track_caller]
    pub fn move_index(self, to: usize, e: &mut E, g: &mut G) -> Result<(), R> {
        let index = self.index();
        self.into_ref_mut().move_index(index, to, e, g)
    }
}

impl<K: fmt::Debug, V: fmt::Debug, E, G, R> fmt::Debug for OccupiedEntry<'_, K, V, E, G, R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("OccupiedEntry")
            .field("key", self.key())
            .field("value", self.get())
            .finish()
    }
}

impl<'a, K, V, E, G, R> OccupiedEntry<'a, K, V, E, G, R> {
    pub fn from_index(
        other: IndexedEntry<'a, K, V, E, G, R>,
        e: &mut E,
        g: &mut G,
    ) -> Result<Self, R> {
        let IndexedEntry {
            map: RefMut { indices, entries },
            index,
        } = other;
        let hash = entries[index].hash;
        Ok(Self {
            entries,
            index: indices
                .find_entry(hash.get(), move |&i, _, _| Ok(i == index), e, g)?
                .expect("index not found"),
        })
    }
}

/// A view into a vacant entry in an [`IndexMap`][crate::IndexMap].
/// It is part of the [`Entry`] enum.
pub struct VacantEntry<'a, K, V, E, G, R> {
    map: RefMut<'a, K, V, E, G, R>,
    hash: HashValue,
    key: K,
}

impl<'a, K, V, E, G, R> VacantEntry<'a, K, V, E, G, R> {
    /// Return the index where a key-value pair may be inserted.
    pub fn index(&self) -> usize {
        self.map.indices.len()
    }

    /// Gets a reference to the key that was used to find the entry.
    pub fn key(&self) -> &K {
        &self.key
    }

    /// Takes ownership of the key, leaving the entry vacant.
    pub fn into_key(self) -> K {
        self.key
    }

    /// Inserts the entry's key and the given value into the map, and returns a mutable reference
    /// to the value.
    ///
    /// Computes in **O(1)** time (amortized average).
    pub fn insert(self, value: V) -> &'a mut V {
        self.insert_entry(value).into_mut()
    }

    /// Inserts the entry's key and the given value into the map, and returns an `OccupiedEntry`.
    ///
    /// Computes in **O(1)** time (amortized average).
    pub fn insert_entry(self, value: V) -> OccupiedEntry<'a, K, V, E, G, R> {
        let Self { map, hash, key } = self;
        map.insert_unique(hash, key, value)
    }

    /// Inserts the entry's key and the given value into the map at its ordered
    /// position among sorted keys, and returns the new index and a mutable
    /// reference to the value.
    ///
    /// If the existing keys are **not** already sorted, then the insertion
    /// index is unspecified (like [`slice::binary_search`]), but the key-value
    /// pair is inserted at that position regardless.
    ///
    /// Computes in **O(n)** time (average).
    pub fn insert_sorted(self, value: V, e: &mut E, g: &mut G) -> Result<(usize, &'a mut V), R>
    where
        K: Ord,
    {
        let slice = crate::map::Slice::from_slice(self.map.entries);
        let i = slice.binary_search_keys(&self.key).unwrap_err();
        Ok((i, self.shift_insert(i, value, e, g)?))
    }

    /// Inserts the entry's key and the given value into the map at the given index,
    /// shifting others to the right, and returns a mutable reference to the value.
    ///
    /// ***Panics*** if `index` is out of bounds.
    ///
    /// Computes in **O(n)** time (average).
    #[track_caller]
    pub fn shift_insert(
        mut self,
        index: usize,
        value: V,
        e: &mut E,
        g: &mut G,
    ) -> Result<&'a mut V, R> {
        self.map
            .shift_insert_unique(index, self.hash, self.key, value, e, g)?;
        Ok(&mut self.map.entries[index].value)
    }
}

impl<K: fmt::Debug, V, E, G, R> fmt::Debug for VacantEntry<'_, K, V, E, G, R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("VacantEntry").field(self.key()).finish()
    }
}

/// A view into an occupied entry in an [`IndexMap`][crate::IndexMap] obtained by index.
///
/// This `struct` is created from the [`get_index_entry`][crate::IndexMap::get_index_entry] method.
pub struct IndexedEntry<'a, K, V, E, G, R> {
    map: RefMut<'a, K, V, E, G, R>,
    // We have a mutable reference to the map, which keeps the index
    // valid and pointing to the correct entry.
    index: usize,
}

impl<'a, K, V, E, G, R> IndexedEntry<'a, K, V, E, G, R> {
    pub(crate) fn new(map: &'a mut IndexMapCore<K, V, E, G, R>, index: usize) -> Self {
        Self {
            map: map.borrow_mut(),
            index,
        }
    }

    /// Return the index of the key-value pair
    #[inline]
    pub fn index(&self) -> usize {
        self.index
    }

    /// Gets a reference to the entry's key in the map.
    pub fn key(&self) -> &K {
        &self.map.entries[self.index].key
    }

    /// Gets a reference to the entry's value in the map.
    pub fn get(&self) -> &V {
        &self.map.entries[self.index].value
    }

    /// Gets a mutable reference to the entry's value in the map.
    ///
    /// If you need a reference which may outlive the destruction of the
    /// `IndexedEntry` value, see [`into_mut`][Self::into_mut].
    pub fn get_mut(&mut self) -> &mut V {
        &mut self.map.entries[self.index].value
    }

    /// Sets the value of the entry to `value`, and returns the entry's old value.
    pub fn insert(&mut self, value: V) -> V {
        mem::replace(self.get_mut(), value)
    }

    /// Converts into a mutable reference to the entry's value in the map,
    /// with a lifetime bound to the map itself.
    pub fn into_mut(self) -> &'a mut V {
        &mut self.map.entries[self.index].value
    }

    /// Remove and return the key, value pair stored in the map for this entry
    ///
    /// Like [`Vec::swap_remove`][crate::Vec::swap_remove], the pair is removed by swapping it with
    /// the last element of the map and popping it off.
    /// **This perturbs the position of what used to be the last element!**
    ///
    /// Computes in **O(1)** time (average).
    pub fn swap_remove_entry(mut self, e: &mut E, g: &mut G) -> Result<(K, V), R> {
        Ok(self.map.swap_remove_index(self.index, e, g)?.unwrap())
    }

    /// Remove and return the key, value pair stored in the map for this entry
    ///
    /// Like [`Vec::remove`][crate::Vec::remove], the pair is removed by shifting all of the
    /// elements that follow it, preserving their relative order.
    /// **This perturbs the index of all of those elements!**
    ///
    /// Computes in **O(n)** time (average).
    pub fn shift_remove_entry(mut self, e: &mut E, g: &mut G) -> Result<(K, V), R> {
        Ok(self.map.shift_remove_index(self.index, e, g)?.unwrap())
    }

    /// Remove the key, value pair stored in the map for this entry, and return the value.
    ///
    /// Like [`Vec::swap_remove`][crate::Vec::swap_remove], the pair is removed by swapping it with
    /// the last element of the map and popping it off.
    /// **This perturbs the position of what used to be the last element!**
    ///
    /// Computes in **O(1)** time (average).
    pub fn swap_remove(self, e: &mut E, g: &mut G) -> Result<V, R> {
        Ok(self.swap_remove_entry(e, g)?.1)
    }

    /// Remove the key, value pair stored in the map for this entry, and return the value.
    ///
    /// Like [`Vec::remove`][crate::Vec::remove], the pair is removed by shifting all of the
    /// elements that follow it, preserving their relative order.
    /// **This perturbs the index of all of those elements!**
    ///
    /// Computes in **O(n)** time (average).
    pub fn shift_remove(self, e: &mut E, g: &mut G) -> Result<V, R> {
        Ok(self.shift_remove_entry(e, g)?.1)
    }

    /// Moves the position of the entry to a new index
    /// by shifting all other entries in-between.
    ///
    /// This is equivalent to [`IndexMap::move_index`][`crate::IndexMap::move_index`]
    /// coming `from` the current [`.index()`][Self::index].
    ///
    /// * If `self.index() < to`, the other pairs will shift down while the targeted pair moves up.
    /// * If `self.index() > to`, the other pairs will shift up while the targeted pair moves down.
    ///
    /// ***Panics*** if `to` is out of bounds.
    ///
    /// Computes in **O(n)** time (average).
    #[track_caller]
    pub fn move_index(mut self, to: usize, e: &mut E, g: &mut G) -> Result<(), R> {
        self.map.move_index(self.index, to, e, g)
    }
}

impl<K: fmt::Debug, V: fmt::Debug, E, G, R> fmt::Debug for IndexedEntry<'_, K, V, E, G, R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IndexedEntry")
            .field("index", &self.index)
            .field("key", self.key())
            .field("value", self.get())
            .finish()
    }
}

impl<'a, K, V, E, G, R> From<OccupiedEntry<'a, K, V, E, G, R>> for IndexedEntry<'a, K, V, E, G, R> {
    fn from(other: OccupiedEntry<'a, K, V, E, G, R>) -> Self {
        Self {
            index: other.index(),
            map: other.into_ref_mut(),
        }
    }
}
