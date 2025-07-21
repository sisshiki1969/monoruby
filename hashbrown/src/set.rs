use crate::{Equivalent, RubyEql};
use core::hash::{BuildHasher, Hash};
use core::iter::FusedIterator;
use core::{fmt, mem};
use map::make_hash;

use super::map::{self, HashMap, Keys};
use crate::raw::RawExtractIf;
use crate::DefaultHashBuilder;

// Future Optimization (FIXME!)
// =============================
//
// Iteration over zero sized values is a noop. There is no need
// for `bucket.val` in the case of HashSet. I suppose we would need HKT
// to get rid of it properly.

/// A hash set implemented as a `HashMap` where the value is `()`.
///
/// As with the [`HashMap`] type, a `HashSet` requires that the elements
/// implement the [`Eq`] and [`Hash`] traits. This can frequently be achieved by
/// using `#[derive(PartialEq, Eq, Hash)]`. If you implement these yourself,
/// it is important that the following property holds:
///
/// ```text
/// k1 == k2 -> hash(k1) == hash(k2)
/// ```
///
/// In other words, if two keys are equal, their hashes must be equal.
///
///
/// It is a logic error for an item to be modified in such a way that the
/// item's hash, as determined by the [`Hash`] trait, or its equality, as
/// determined by the [`Eq`] trait, changes while it is in the set. This is
/// normally only possible through [`Cell`], [`RefCell`], global state, I/O, or
/// unsafe code.
///
/// It is also a logic error for the [`Hash`] implementation of a key to panic.
/// This is generally only possible if the trait is implemented manually. If a
/// panic does occur then the contents of the `HashSet` may become corrupted and
/// some items may be dropped from the table.
///
/// # Examples
///
/// ```
/// use hashbrown::HashSet;
/// // Type inference lets us omit an explicit type signature (which
/// // would be `HashSet<String>` in this example).
/// let mut books: HashSet<_> = HashSet::new();
///
/// // Add some books.
/// books.insert("A Dance With Dragons".to_string(), &mut (), &mut ()).unwrap();
/// books.insert("To Kill a Mockingbird".to_string(), &mut (), &mut ()).unwrap();
/// books.insert("The Odyssey".to_string(), &mut (), &mut ()).unwrap();
/// books.insert("The Great Gatsby".to_string(), &mut (), &mut ()).unwrap();
///
/// // Check for a specific one.
/// if !books.contains("The Winds of Winter", &mut (), &mut ()).unwrap() {
///     println!("We have {} books, but The Winds of Winter ain't one.",
///              books.len());
/// }
///
/// // Remove a book.
/// books.remove("The Odyssey", &mut (), &mut ()).unwrap();
///
/// // Iterate over everything.
/// for book in &books {
///     println!("{}", book);
/// }
/// ```
///
/// The easiest way to use `HashSet` with a custom type is to derive
/// [`Eq`] and [`Hash`]. We must also derive [`PartialEq`]. This will in the
/// future be implied by [`Eq`].
///
/// ```
/// use hashbrown::HashSet;
/// #[derive(Hash, Eq, PartialEq, Debug)]
/// struct Viking {
///     name: String,
///     power: usize,
/// }
///
/// impl<E, G> equivalent::RubyEql<E, G, ()> for Viking {
///    fn eql(&self, other: &Self, e: &mut E, g: &mut G) -> Result<bool, ()> {
///       Ok(self.name.eql(&other.name, e, g)? && self.power.eql(&other.power, e, g)?)
///     }
/// }
///
/// let mut vikings: HashSet<_> = HashSet::new();
///
/// vikings.insert(Viking { name: "Einar".to_string(), power: 9 }, &mut (), &mut ()).unwrap();
/// vikings.insert(Viking { name: "Einar".to_string(), power: 9 }, &mut (), &mut ()).unwrap();
/// vikings.insert(Viking { name: "Olaf".to_string(), power: 4 }, &mut (), &mut ()).unwrap();
/// vikings.insert(Viking { name: "Harald".to_string(), power: 8 }, &mut (), &mut ()).unwrap();
///
/// // Use derived implementation to print the vikings.
/// for x in &vikings {
///     println!("{:?}", x);
/// }
/// ```
///
/// A `HashSet` with fixed list of elements can be initialized from an array:
///
/// ```
/// use hashbrown::HashSet;
///
/// let viking_names: HashSet<_> =
///     HashSet::from_iter([ "Einar", "Olaf", "Harald" ].into_iter(), &mut (), &mut ()).unwrap();
/// // use the values stored in the set
/// ```
///
/// [`Cell`]: https://doc.rust-lang.org/std/cell/struct.Cell.html
/// [`Eq`]: https://doc.rust-lang.org/std/cmp/trait.Eq.html
/// [`Hash`]: https://doc.rust-lang.org/std/hash/trait.Hash.html
/// [`HashMap`]: struct.HashMap.html
/// [`PartialEq`]: https://doc.rust-lang.org/std/cmp/trait.PartialEq.html
/// [`RefCell`]: https://doc.rust-lang.org/std/cell/struct.RefCell.html
pub struct HashSet<T, E = (), G = (), R = (), S = DefaultHashBuilder> {
    pub(crate) map: HashMap<T, (), E, G, R, S>,
}

impl<T: Clone, E, G, R, S: Clone + Clone> Clone for HashSet<T, E, G, R, S> {
    fn clone(&self) -> Self {
        HashSet {
            map: self.map.clone(),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        self.map.clone_from(&source.map);
    }
}

impl<T: PartialEq + RubyEql<(), (), ()> + Hash> PartialEq for HashSet<T, (), (), ()> {
    fn eq(&self, other: &Self) -> bool {
        self.map == other.map
    }
}

impl<T, E, G, R> HashSet<T, E, G, R, DefaultHashBuilder> {
    /// Creates an empty `HashSet`.
    ///
    /// The hash set is initially created with a capacity of 0, so it will not allocate until it
    /// is first inserted into.
    ///
    /// # HashDoS resistance
    ///
    /// The `hash_builder` normally use a fixed key by default and that does
    /// not allow the `HashSet` to be protected against attacks such as [`HashDoS`].
    /// Users who require HashDoS resistance should explicitly use
    /// [`std::collections::hash_map::RandomState`]
    /// as the hasher when creating a [`HashSet`], for example with
    /// [`with_hasher`](HashSet::with_hasher) method.
    ///
    /// [`HashDoS`]: https://en.wikipedia.org/wiki/Collision_attack
    /// [`std::collections::hash_map::RandomState`]: https://doc.rust-lang.org/std/collections/hash_map/struct.RandomState.html
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    /// let set: HashSet<i32> = HashSet::new();
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    /// Creates an empty `HashSet` with the specified capacity.
    ///
    /// The hash set will be able to hold at least `capacity` elements without
    /// reallocating. If `capacity` is 0, the hash set will not allocate.
    ///
    /// # HashDoS resistance
    ///
    /// The `hash_builder` normally use a fixed key by default and that does
    /// not allow the `HashSet` to be protected against attacks such as [`HashDoS`].
    /// Users who require HashDoS resistance should explicitly use
    /// [`std::collections::hash_map::RandomState`]
    /// as the hasher when creating a [`HashSet`], for example with
    /// [`with_capacity_and_hasher`](HashSet::with_capacity_and_hasher) method.
    ///
    /// [`HashDoS`]: https://en.wikipedia.org/wiki/Collision_attack
    /// [`std::collections::hash_map::RandomState`]: https://doc.rust-lang.org/std/collections/hash_map/struct.RandomState.html
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    /// let set: HashSet<i32> = HashSet::with_capacity(10);
    /// assert!(set.capacity() >= 10);
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            map: HashMap::with_capacity(capacity),
        }
    }
}

impl<T, E, G, R, S> HashSet<T, E, G, R, S> {
    /// Returns the number of elements the set can hold without reallocating.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    /// let set: HashSet<i32> = HashSet::with_capacity(100);
    /// assert!(set.capacity() >= 100);
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn capacity(&self) -> usize {
        self.map.capacity()
    }

    /// An iterator visiting all elements in arbitrary order.
    /// The iterator element type is `&'a T`.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    /// let mut set = HashSet::<_, (), ()>::new();
    /// set.insert("a", &mut (), &mut ()).unwrap();
    /// set.insert("b", &mut (), &mut ()).unwrap();
    ///
    /// // Will print in an arbitrary order.
    /// for x in set.iter() {
    ///     println!("{}", x);
    /// }
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            iter: self.map.keys(),
        }
    }

    /// Returns the number of elements in the set.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    ///
    /// let mut v = HashSet::<_, (), ()>::new();
    /// assert_eq!(v.len(), 0);
    /// v.insert(1, &mut (), &mut ()).unwrap();
    /// assert_eq!(v.len(), 1);
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn len(&self) -> usize {
        self.map.len()
    }

    /// Returns `true` if the set contains no elements.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    ///
    /// let mut v = HashSet::<_, (), ()>::new();
    /// assert!(v.is_empty());
    /// v.insert(1, &mut (), &mut ()).unwrap();
    /// assert!(!v.is_empty());
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    /// Clears the set, returning all elements in an iterator.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    ///
    /// let mut set: HashSet<_, (), ()> = HashSet::from_iter([1, 2, 3].into_iter(), &mut (), &mut ()).unwrap();
    /// assert!(!set.is_empty());
    ///
    /// // print 1, 2, 3 in an arbitrary order
    /// for i in set.drain() {
    ///     println!("{}", i);
    /// }
    ///
    /// assert!(set.is_empty());
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn drain(&mut self) -> Drain<'_, T, E, G, R> {
        Drain {
            iter: self.map.drain(),
        }
    }

    /// Retains only the elements specified by the predicate.
    ///
    /// In other words, remove all elements `e` such that `f(&e)` returns `false`.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    ///
    /// let xs = [1,2,3,4,5,6];
    /// let mut set: HashSet<i32, (), ()> = HashSet::from_iter(xs.into_iter(), &mut (), &mut ()).unwrap();
    /// set.retain(|&k| k % 2 == 0);
    /// assert_eq!(set.len(), 3);
    /// ```
    pub fn retain<F>(&mut self, mut f: F)
    where
        F: FnMut(&T) -> bool,
    {
        self.map.retain(|k, _| f(k));
    }

    /// Drains elements which are true under the given predicate,
    /// and returns an iterator over the removed items.
    ///
    /// In other words, move all elements `e` such that `f(&e)` returns `true` out
    /// into another iterator.
    ///
    /// If the returned `ExtractIf` is not exhausted, e.g. because it is dropped without iterating
    /// or the iteration short-circuits, then the remaining elements will be retained.
    /// Use [`retain()`] with a negated predicate if you do not need the returned iterator.
    ///
    /// [`retain()`]: HashSet::retain
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    /// struct E;
    /// struct G;
    ///
    /// let mut set: HashSet<i32, E, G> = HashSet::from_iter(0..8, &mut E, &mut G).unwrap();
    /// let drained: HashSet<i32, E, G> = HashSet::from_iter(set.extract_if(|v| v % 2 == 0), &mut E, &mut G).unwrap();
    ///
    /// let mut evens = drained.into_iter().collect::<Vec<_>>();
    /// let mut odds = set.into_iter().collect::<Vec<_>>();
    /// evens.sort();
    /// odds.sort();
    ///
    /// assert_eq!(evens, vec![0, 2, 4, 6]);
    /// assert_eq!(odds, vec![1, 3, 5, 7]);
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn extract_if<F>(&mut self, f: F) -> ExtractIf<'_, T, F, E, G, R>
    where
        F: FnMut(&T) -> bool,
    {
        ExtractIf {
            f,
            inner: RawExtractIf {
                iter: unsafe { self.map.table.iter() },
                table: &mut self.map.table,
            },
        }
    }

    /// Clears the set, removing all values.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    /// struct E;
    /// struct G;
    ///
    /// let mut v = HashSet::<_,E,G>::new();
    /// v.insert(1, &mut E, &mut G).unwrap();
    /// v.clear();
    /// assert!(v.is_empty());
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn clear(&mut self) {
        self.map.clear();
    }
}

impl<T, E, G, R, S> HashSet<T, E, G, R, S> {
    /// Creates a new empty hash set which will use the given hasher to hash
    /// keys.
    ///
    /// The hash set is initially created with a capacity of 0, so it will not
    /// allocate until it is first inserted into.
    ///
    /// # HashDoS resistance
    ///
    /// The `hash_builder` normally use a fixed key by default and that does
    /// not allow the `HashSet` to be protected against attacks such as [`HashDoS`].
    /// Users who require HashDoS resistance should explicitly use
    /// [`std::collections::hash_map::RandomState`]
    /// as the hasher when creating a [`HashSet`].
    ///
    /// The `hash_builder` passed should implement the [`BuildHasher`] trait for
    /// the `HashSet` to be useful, see its documentation for details.
    ///
    /// [`HashDoS`]: https://en.wikipedia.org/wiki/Collision_attack
    /// [`std::collections::hash_map::RandomState`]: https://doc.rust-lang.org/std/collections/hash_map/struct.RandomState.html
    /// [`BuildHasher`]: https://doc.rust-lang.org/std/hash/trait.BuildHasher.html
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    /// use hashbrown::DefaultHashBuilder;
    /// struct E;
    /// struct G;
    ///
    /// let s = DefaultHashBuilder::default();
    /// let mut set = HashSet::<_,E,G>::with_hasher(s);
    /// set.insert(2, &mut E, &mut G).unwrap();
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub const fn with_hasher(hasher: S) -> Self {
        Self {
            map: HashMap::with_hasher(hasher),
        }
    }

    /// Creates an empty `HashSet` with the specified capacity, using
    /// `hasher` to hash the keys.
    ///
    /// The hash set will be able to hold at least `capacity` elements without
    /// reallocating. If `capacity` is 0, the hash set will not allocate.
    ///
    /// # HashDoS resistance
    ///
    /// The `hash_builder` normally use a fixed key by default and that does
    /// not allow the `HashSet` to be protected against attacks such as [`HashDoS`].
    /// Users who require HashDoS resistance should explicitly use
    /// [`std::collections::hash_map::RandomState`]
    /// as the hasher when creating a [`HashSet`].
    ///
    /// The `hash_builder` passed should implement the [`BuildHasher`] trait for
    /// the `HashSet` to be useful, see its documentation for details.
    ///
    /// [`HashDoS`]: https://en.wikipedia.org/wiki/Collision_attack
    /// [`std::collections::hash_map::RandomState`]: https://doc.rust-lang.org/std/collections/hash_map/struct.RandomState.html
    /// [`BuildHasher`]: https://doc.rust-lang.org/std/hash/trait.BuildHasher.html
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    /// use hashbrown::DefaultHashBuilder;
    ///
    /// let s = DefaultHashBuilder::default();
    /// let mut set = HashSet::<_, (), ()>::with_capacity_and_hasher(10, s);
    /// set.insert(1, &mut (), &mut ()).unwrap();
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn with_capacity_and_hasher(capacity: usize, hasher: S) -> Self {
        Self {
            map: HashMap::with_capacity_and_hasher(capacity, hasher),
        }
    }
}

impl<T, E, G, R, S> HashSet<T, E, G, R, S> {
    /// Returns a reference to the set's [`BuildHasher`].
    ///
    /// [`BuildHasher`]: https://doc.rust-lang.org/std/hash/trait.BuildHasher.html
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    /// use hashbrown::DefaultHashBuilder;
    ///
    /// let hasher = DefaultHashBuilder::default();
    /// let set: HashSet<i32, (), ()> = HashSet::with_hasher(hasher);
    /// let hasher: &DefaultHashBuilder = set.hasher();
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn hasher(&self) -> &S {
        self.map.hasher()
    }
}

impl<T, E, G, R, S> HashSet<T, E, G, R, S>
where
    T: RubyEql<E, G, R> + Hash,
    S: BuildHasher,
{
    /// Reserves capacity for at least `additional` more elements to be inserted
    /// in the `HashSet`. The collection may reserve more space to avoid
    /// frequent reallocations.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity exceeds [`isize::MAX`] bytes and [`abort`] the program
    /// in case of allocation error.
    ///
    /// [`isize::MAX`]: https://doc.rust-lang.org/std/primitive.isize.html
    /// [`abort`]: https://doc.rust-lang.org/alloc/alloc/fn.handle_alloc_error.html
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    /// let mut set: HashSet<i32> = HashSet::new();
    /// set.reserve(10);
    /// assert!(set.capacity() >= 10);
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn reserve(&mut self, additional: usize) {
        self.map.reserve(additional);
    }

    /// Shrinks the capacity of the set as much as possible. It will drop
    /// down as much as possible while maintaining the internal rules
    /// and possibly leaving some space in accordance with the resize policy.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    /// struct E;
    /// struct G;
    ///
    /// let mut set = HashSet::<_, _, _, ()>::with_capacity(100);
    /// set.insert(1, &mut E, &mut G).unwrap();
    /// set.insert(2, &mut E, &mut G).unwrap();
    /// assert!(set.capacity() >= 100);
    /// set.shrink_to_fit();
    /// assert!(set.capacity() >= 2);
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn shrink_to_fit(&mut self) {
        self.map.shrink_to_fit();
    }

    /// Shrinks the capacity of the set with a lower limit. It will drop
    /// down no lower than the supplied limit while maintaining the internal rules
    /// and possibly leaving some space in accordance with the resize policy.
    ///
    /// Panics if the current capacity is smaller than the supplied
    /// minimum capacity.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    ///
    /// let mut set = HashSet::<_, (), ()>::with_capacity(100);
    /// set.insert(1, &mut (), &mut ()).unwrap();
    /// set.insert(2, &mut (), &mut ()).unwrap();
    /// assert!(set.capacity() >= 100);
    /// set.shrink_to(10);
    /// assert!(set.capacity() >= 10);
    /// set.shrink_to(0);
    /// assert!(set.capacity() >= 2);
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn shrink_to(&mut self, min_capacity: usize) {
        self.map.shrink_to(min_capacity);
    }

    /// Returns `true` if the set contains a value.
    ///
    /// The value may be any borrowed form of the set's value type, but
    /// [`Hash`] and [`Eq`] on the borrowed form *must* match those for
    /// the value type.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    ///
    /// let set: HashSet<_> = HashSet::from_iter([1, 2, 3].into_iter(), &mut (), &mut ()).unwrap();
    /// assert_eq!(set.contains(&1, &mut (), &mut ()).unwrap(), true);
    /// assert_eq!(set.contains(&4, &mut (), &mut ()).unwrap(), false);
    /// ```
    ///
    /// [`Eq`]: https://doc.rust-lang.org/std/cmp/trait.Eq.html
    /// [`Hash`]: https://doc.rust-lang.org/std/hash/trait.Hash.html
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn contains<Q>(&self, value: &Q, e: &mut E, g: &mut G) -> Result<bool, R>
    where
        Q: Hash + Equivalent<T, E, G, R> + ?Sized,
    {
        self.map.contains_key(value, e, g)
    }

    /// Returns a reference to the value in the set, if any, that is equal to the given value.
    ///
    /// The value may be any borrowed form of the set's value type, but
    /// [`Hash`] and [`Eq`] on the borrowed form *must* match those for
    /// the value type.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    ///
    /// let set: HashSet<_, (), ()> = HashSet::from_iter([1, 2, 3].into_iter(), &mut (), &mut ()).unwrap();
    /// assert_eq!(set.get(&2, &mut (), &mut ()).unwrap(), Some(&2));
    /// assert_eq!(set.get(&4, &mut (), &mut ()).unwrap(), None);
    /// ```
    ///
    /// [`Eq`]: https://doc.rust-lang.org/std/cmp/trait.Eq.html
    /// [`Hash`]: https://doc.rust-lang.org/std/hash/trait.Hash.html
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn get<Q>(&self, value: &Q, e: &mut E, g: &mut G) -> Result<Option<&T>, R>
    where
        Q: Hash + Equivalent<T, E, G, R> + ?Sized,
    {
        // Avoid `Option::map` because it bloats LLVM IR.
        Ok(match self.map.get_key_value(value, e, g)? {
            Some((k, _)) => Some(k),
            None => None,
        })
    }

    /// Inserts the given `value` into the set if it is not present, then
    /// returns a reference to the value in the set.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    ///
    /// let mut set: HashSet<_> = HashSet::from_iter([1, 2, 3].into_iter(), &mut (), &mut ()).unwrap();
    /// assert_eq!(set.len(), 3);
    /// assert_eq!(set.get_or_insert(2, &mut (), &mut ()).unwrap(), &2);
    /// assert_eq!(set.get_or_insert(100, &mut (), &mut ()).unwrap(), &100);
    /// assert_eq!(set.len(), 4); // 100 was inserted
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn get_or_insert(&mut self, value: T, e: &mut E, g: &mut G) -> Result<&T, R> {
        let hash = make_hash(&self.map.hash_builder, &value);
        let bucket = match self.map.find_or_find_insert_slot(hash, &value, e, g)? {
            Ok(bucket) => bucket,
            Err(slot) => unsafe { self.map.table.insert_in_slot(hash, slot, (value, ())) },
        };
        unsafe { Ok(&bucket.as_ref().0) }
    }

    /// Inserts a value computed from `f` into the set if the given `value` is
    /// not present, then returns a reference to the value in the set.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    ///
    /// let mut set: HashSet<String> = HashSet::from_iter(["cat", "dog", "horse"]
    ///     .iter().map(|&pet| pet.to_owned()), &mut (), &mut ()).unwrap();
    ///
    /// assert_eq!(set.len(), 3);
    /// for &pet in &["cat", "dog", "fish"] {
    ///     let value = set.get_or_insert_with(pet, str::to_owned, &mut (), &mut ()).unwrap();
    ///     assert_eq!(value, pet);
    /// }
    /// assert_eq!(set.len(), 4); // a new "fish" was inserted
    /// ```
    ///
    /// The following example will panic because the new value doesn't match.
    ///
    /// ```should_panic
    /// let mut set: hashbrown::HashSet<_> = hashbrown::HashSet::new();
    /// set.get_or_insert_with("rust", |_| String::new(), &mut (), &mut ()).unwrap();
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn get_or_insert_with<Q, F>(
        &mut self,
        value: &Q,
        f: F,
        e: &mut E,
        g: &mut G,
    ) -> Result<&T, R>
    where
        Q: Hash + Equivalent<T, E, G, R> + ?Sized,
        F: FnOnce(&Q) -> T,
    {
        let hash = make_hash(&self.map.hash_builder, value);
        let bucket = match self.map.find_or_find_insert_slot(hash, value, e, g)? {
            Ok(bucket) => bucket,
            Err(slot) => {
                let new = f(value);
                assert!(
                    value.equivalent(&new, e, g).unwrap_or(false), // TO_BE_FIXED
                    "new value is not equivalent"
                );
                unsafe { self.map.table.insert_in_slot(hash, slot, (new, ())) }
            }
        };
        unsafe { Ok(&bucket.as_ref().0) }
    }

    /// Gets the given value's corresponding entry in the set for in-place manipulation.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    /// use hashbrown::hash_set::Entry::*;
    ///
    /// let mut singles: HashSet<_> = HashSet::new();
    /// let mut dupes: HashSet<_> = HashSet::new();
    ///
    /// for ch in "a short treatise on fungi".chars() {
    ///     if let Vacant(dupe_entry) = dupes.entry(ch, &mut (), &mut ()).unwrap() {
    ///         // We haven't already seen a duplicate, so
    ///         // check if we've at least seen it once.
    ///         match singles.entry(ch, &mut (), &mut ()).unwrap() {
    ///             Vacant(single_entry) => {
    ///                 // We found a new character for the first time.
    ///                 single_entry.insert();
    ///             }
    ///             Occupied(single_entry) => {
    ///                 // We've already seen this once, "move" it to dupes.
    ///                 single_entry.remove();
    ///                 dupe_entry.insert();
    ///             }
    ///         }
    ///     }
    /// }
    ///
    /// assert!(!singles.contains(&'t', &mut (), &mut ()).unwrap() && dupes.contains(&'t', &mut (), &mut ()).unwrap());
    /// assert!(singles.contains(&'u', &mut (), &mut ()).unwrap() && !dupes.contains(&'u', &mut (), &mut ()).unwrap());
    /// assert!(!singles.contains(&'v', &mut (), &mut ()).unwrap() && !dupes.contains(&'v', &mut (), &mut ()).unwrap());
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn entry(&mut self, value: T, e: &mut E, g: &mut G) -> Result<Entry<'_, T, E, G, R, S>, R> {
        Ok(match self.map.entry(value, e, g)? {
            map::Entry::Occupied(entry) => Entry::Occupied(OccupiedEntry { inner: entry }),
            map::Entry::Vacant(entry) => Entry::Vacant(VacantEntry { inner: entry }),
        })
    }

    /// Returns `true` if the set is a subset of another,
    /// i.e., `other` contains at least all the values in `self`.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    ///
    /// let sup: HashSet<_> = HashSet::from_iter([1, 2, 3].into_iter(), &mut (), &mut ()).unwrap();
    /// let mut set = HashSet::new();
    ///
    /// assert_eq!(set.is_subset(&sup, &mut (), &mut ()).unwrap(), true);
    /// set.insert(2, &mut (), &mut ()).unwrap();
    /// assert_eq!(set.is_subset(&sup, &mut (), &mut ()).unwrap(), true);
    /// set.insert(4, &mut (), &mut ()).unwrap();
    /// assert_eq!(set.is_subset(&sup, &mut (), &mut ()).unwrap(), false);
    /// ```
    pub fn is_subset(&self, other: &Self, e: &mut E, g: &mut G) -> Result<bool, R> {
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

    /// Returns `true` if the set is a superset of another,
    /// i.e., `self` contains at least all the values in `other`.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    ///
    /// let sub: HashSet<_> = HashSet::from_iter([1, 2].into_iter(), &mut (), &mut ()).unwrap();
    /// let mut set = HashSet::new();
    ///
    /// assert_eq!(set.is_superset(&sub, &mut (), &mut ()).unwrap(), false);
    ///
    /// set.insert(0, &mut (), &mut ()).unwrap();
    /// set.insert(1, &mut (), &mut ()).unwrap();
    /// assert_eq!(set.is_superset(&sub, &mut (), &mut ()).unwrap(), false);
    ///
    /// set.insert(2, &mut (), &mut ()).unwrap();
    /// assert_eq!(set.is_superset(&sub, &mut (), &mut ()).unwrap(), true);
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn is_superset(&self, other: &Self, e: &mut E, g: &mut G) -> Result<bool, R> {
        other.is_subset(self, e, g)
    }

    /// Adds a value to the set.
    ///
    /// If the set did not have this value present, `true` is returned.
    ///
    /// If the set did have this value present, `false` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    ///
    /// let mut set: HashSet<_> = HashSet::new();
    ///
    /// assert_eq!(set.insert(2, &mut (), &mut ()).unwrap(), true);
    /// assert_eq!(set.insert(2, &mut (), &mut ()).unwrap(), false);
    /// assert_eq!(set.len(), 1);
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn insert(&mut self, value: T, e: &mut E, g: &mut G) -> Result<bool, R> {
        Ok(self.map.insert(value, (), e, g)?.is_none())
    }

    /// Insert a value the set without checking if the value already exists in the set.
    ///
    /// This operation is faster than regular insert, because it does not perform
    /// lookup before insertion.
    ///
    /// This operation is useful during initial population of the set.
    /// For example, when constructing a set from another set, we know
    /// that values are unique.
    ///
    /// # Safety
    ///
    /// This operation is safe if a value does not exist in the set.
    ///
    /// However, if a value exists in the set already, the behavior is unspecified:
    /// this operation may panic, loop forever, or any following operation with the set
    /// may panic, loop forever or return arbitrary result.
    ///
    /// That said, this operation (and following operations) are guaranteed to
    /// not violate memory safety.
    ///
    /// However this operation is still unsafe because the resulting `HashSet`
    /// may be passed to unsafe code which does expect the set to behave
    /// correctly, and would cause unsoundness as a result.
    #[cfg_attr(feature = "inline-more", inline)]
    pub unsafe fn insert_unique_unchecked(&mut self, value: T) -> &T {
        self.map.insert_unique_unchecked(value, ()).0
    }

    /// Adds a value to the set, replacing the existing value, if any, that is equal to the given
    /// one. Returns the replaced value.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    /// struct E;
    /// struct G;
    ///
    /// let mut set = HashSet::<_>::new();
    /// set.insert(Vec::<i32>::new(), &mut (), &mut ()).unwrap();
    ///
    /// assert_eq!(set.get(&[][..], &mut (), &mut ()).unwrap().unwrap().capacity(), 0);
    /// set.replace(Vec::with_capacity(10), &mut (), &mut ()).unwrap();
    /// assert_eq!(set.get(&[][..], &mut (), &mut ()).unwrap().unwrap().capacity(), 10);
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn replace(&mut self, value: T, e: &mut E, g: &mut G) -> Result<Option<T>, R> {
        let hash = make_hash(&self.map.hash_builder, &value);
        Ok(
            match self.map.find_or_find_insert_slot(hash, &value, e, g)? {
                Ok(bucket) => Some(mem::replace(unsafe { &mut bucket.as_mut().0 }, value)),
                Err(slot) => {
                    unsafe {
                        self.map.table.insert_in_slot(hash, slot, (value, ()));
                    }
                    None
                }
            },
        )
    }

    /// Removes a value from the set. Returns whether the value was
    /// present in the set.
    ///
    /// The value may be any borrowed form of the set's value type, but
    /// [`Hash`] and [`Eq`] on the borrowed form *must* match those for
    /// the value type.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    ///
    /// let mut set: HashSet<_> = HashSet::new();
    ///
    /// set.insert(2, &mut (), &mut ()).unwrap();
    /// assert_eq!(set.remove(&2, &mut (), &mut ()).unwrap(), true);
    /// assert_eq!(set.remove(&2, &mut (), &mut ()).unwrap(), false);
    /// ```
    ///
    /// [`Eq`]: https://doc.rust-lang.org/std/cmp/trait.Eq.html
    /// [`Hash`]: https://doc.rust-lang.org/std/hash/trait.Hash.html
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn remove<Q>(&mut self, value: &Q, e: &mut E, g: &mut G) -> Result<bool, R>
    where
        Q: Hash + Equivalent<T, E, G, R> + ?Sized,
    {
        Ok(self.map.remove(value, e, g)?.is_some())
    }

    /// Removes and returns the value in the set, if any, that is equal to the given one.
    ///
    /// The value may be any borrowed form of the set's value type, but
    /// [`Hash`] and [`Eq`] on the borrowed form *must* match those for
    /// the value type.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    ///
    /// let mut set: HashSet<_> = HashSet::from_iter([1, 2, 3].into_iter(), &mut (), &mut ()).unwrap();
    /// assert_eq!(set.take(&2, &mut (), &mut ()).unwrap(), Some(2));
    /// assert_eq!(set.take(&2, &mut (), &mut ()).unwrap(), None);
    /// ```
    ///
    /// [`Eq`]: https://doc.rust-lang.org/std/cmp/trait.Eq.html
    /// [`Hash`]: https://doc.rust-lang.org/std/hash/trait.Hash.html
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn take<Q>(&mut self, value: &Q, e: &mut E, g: &mut G) -> Result<Option<T>, R>
    where
        Q: Hash + Equivalent<T, E, G, R> + ?Sized,
    {
        // Avoid `Option::map` because it bloats LLVM IR.
        Ok(match self.map.remove_entry(value, e, g)? {
            Some((k, _)) => Some(k),
            None => None,
        })
    }

    /// Returns the total amount of memory allocated internally by the hash
    /// set, in bytes.
    ///
    /// The returned number is informational only. It is intended to be
    /// primarily used for memory profiling.
    #[inline]
    pub fn allocation_size(&self) -> usize {
        self.map.allocation_size()
    }
}

impl<T, E, G, R, S> RubyEql<E, G, R> for HashSet<T, E, G, R, S>
where
    T: RubyEql<E, G, R> + Hash,
    S: BuildHasher,
{
    fn eql(&self, other: &Self, e: &mut E, g: &mut G) -> Result<bool, R> {
        if self.len() != other.len() {
            return Ok(false);
        }

        for key in self.iter() {
            if !other.contains(key, e, g)? {
                return Ok(false);
            }
        }
        Ok(true)
    }
}

impl<T, E, G, R, S> fmt::Debug for HashSet<T, E, G, R, S>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
}

impl<T, E, G, R, S> From<HashMap<T, (), E, G, R, S>> for HashSet<T, E, G, R, S> {
    fn from(map: HashMap<T, (), E, G, R, S>) -> Self {
        Self { map }
    }
}

impl<T, E, G, R, S> HashSet<T, E, G, R, S>
where
    T: RubyEql<E, G, R> + Hash,
    S: BuildHasher + Default,
{
    /// Creates a new HashSet from an iterator.
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn from_iter<I: IntoIterator<Item = T>>(iter: I, e: &mut E, g: &mut G) -> Result<Self, R> {
        let mut set = Self::with_hasher(Default::default());
        set.extend(iter, e, g)?;
        Ok(set)
    }
}

// The default hasher is used to match the std implementation signature
impl<T, E, G, R> HashSet<T, E, G, R, DefaultHashBuilder>
where
    T: RubyEql<E, G, R> + Hash,
{
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    ///
    /// let set1 = HashSet::from_iter([1, 2, 3, 4], &mut (), &mut ()).unwrap();
    /// let set2: HashSet<_> = HashSet::from_iter([1, 2, 3, 4], &mut (), &mut ()).unwrap();
    /// assert_eq!(set1, set2);
    /// ```
    pub fn from_ary<const N: usize>(arr: [T; N], e: &mut E, g: &mut G) -> Result<Self, R> {
        Self::from_iter(arr.into_iter(), e, g)
    }
}

impl<T, E, G, R, S> HashSet<T, E, G, R, S>
where
    T: RubyEql<E, G, R> + Hash,
    S: BuildHasher,
{
    /// Extend the set with the given iterator.
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn extend<I: IntoIterator<Item = T>>(
        &mut self,
        iter: I,
        e: &mut E,
        g: &mut G,
    ) -> Result<(), R> {
        self.map.extend(iter.into_iter().map(|k| (k, ())), e, g)
    }
}

impl<T, E, G, R, S> Default for HashSet<T, E, G, R, S>
where
    S: Default,
{
    /// Creates an empty `HashSet<T, E, G, R, S>` with the `Default` value for the hasher.
    #[cfg_attr(feature = "inline-more", inline)]
    fn default() -> Self {
        Self {
            map: HashMap::default(),
        }
    }
}

/// An iterator over the items of a `HashSet`.
///
/// This `struct` is created by the [`iter`] method on [`HashSet`].
/// See its documentation for more.
///
/// [`HashSet`]: struct.HashSet.html
/// [`iter`]: struct.HashSet.html#method.iter
pub struct Iter<'a, K> {
    iter: Keys<'a, K, ()>,
}

/// An owning iterator over the items of a `HashSet`.
///
/// This `struct` is created by the [`into_iter`] method on [`HashSet`]
/// (provided by the `IntoIterator` trait). See its documentation for more.
///
/// [`HashSet`]: struct.HashSet.html
/// [`into_iter`]: struct.HashSet.html#method.into_iter
pub struct IntoIter<K> {
    iter: map::IntoIter<K, ()>,
}

/// A draining iterator over the items of a `HashSet`.
///
/// This `struct` is created by the [`drain`] method on [`HashSet`].
/// See its documentation for more.
///
/// [`HashSet`]: struct.HashSet.html
/// [`drain`]: struct.HashSet.html#method.drain
pub struct Drain<'a, K, E, G, R> {
    iter: map::Drain<'a, K, (), E, G, R>,
}

/// A draining iterator over entries of a `HashSet` which don't satisfy the predicate `f`.
///
/// This `struct` is created by the [`extract_if`] method on [`HashSet`]. See its
/// documentation for more.
///
/// [`extract_if`]: struct.HashSet.html#method.extract_if
/// [`HashSet`]: struct.HashSet.html
#[must_use = "Iterators are lazy unless consumed"]
pub struct ExtractIf<'a, K, F, E, G, R> {
    f: F,
    inner: RawExtractIf<'a, (K, ()), E, G, R>,
}

impl<'a, T, E, G, R, S> IntoIterator for &'a HashSet<T, E, G, R, S> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    #[cfg_attr(feature = "inline-more", inline)]
    fn into_iter(self) -> Iter<'a, T> {
        self.iter()
    }
}

impl<T, E, G, R, S> IntoIterator for HashSet<T, E, G, R, S> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    /// Creates a consuming iterator, that is, one that moves each value out
    /// of the set in arbitrary order. The set cannot be used after calling
    /// this.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    /// let mut set: HashSet<_> = HashSet::new();
    /// set.insert("a".to_string(), &mut (), &mut ()).unwrap();
    /// set.insert("b".to_string(), &mut (), &mut ()).unwrap();
    ///
    /// // Not possible to collect to a Vec<String> with a regular `.iter()`.
    /// let v: Vec<String> = set.into_iter().collect();
    ///
    /// // Will print in an arbitrary order.
    /// for x in &v {
    ///     println!("{}", x);
    /// }
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    fn into_iter(self) -> IntoIter<T> {
        IntoIter {
            iter: self.map.into_iter(),
        }
    }
}

impl<K> Clone for Iter<'_, K> {
    #[cfg_attr(feature = "inline-more", inline)]
    fn clone(&self) -> Self {
        Iter {
            iter: self.iter.clone(),
        }
    }
}
impl<K> Default for Iter<'_, K> {
    #[cfg_attr(feature = "inline-more", inline)]
    fn default() -> Self {
        Iter {
            iter: Default::default(),
        }
    }
}
impl<'a, K> Iterator for Iter<'a, K> {
    type Item = &'a K;

    #[cfg_attr(feature = "inline-more", inline)]
    fn next(&mut self) -> Option<&'a K> {
        self.iter.next()
    }
    #[cfg_attr(feature = "inline-more", inline)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
    #[cfg_attr(feature = "inline-more", inline)]
    fn fold<B, F>(self, init: B, f: F) -> B
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> B,
    {
        self.iter.fold(init, f)
    }
}
impl<K> ExactSizeIterator for Iter<'_, K> {
    #[cfg_attr(feature = "inline-more", inline)]
    fn len(&self) -> usize {
        self.iter.len()
    }
}
impl<K> FusedIterator for Iter<'_, K> {}

impl<K: fmt::Debug> fmt::Debug for Iter<'_, K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.clone()).finish()
    }
}

impl<K> Default for IntoIter<K> {
    #[cfg_attr(feature = "inline-more", inline)]
    fn default() -> Self {
        IntoIter {
            iter: Default::default(),
        }
    }
}
impl<K> Iterator for IntoIter<K> {
    type Item = K;

    #[cfg_attr(feature = "inline-more", inline)]
    fn next(&mut self) -> Option<K> {
        // Avoid `Option::map` because it bloats LLVM IR.
        match self.iter.next() {
            Some((k, _)) => Some(k),
            None => None,
        }
    }
    #[cfg_attr(feature = "inline-more", inline)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
    #[cfg_attr(feature = "inline-more", inline)]
    fn fold<B, F>(self, init: B, mut f: F) -> B
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> B,
    {
        self.iter.fold(init, |acc, (k, ())| f(acc, k))
    }
}
impl<K> ExactSizeIterator for IntoIter<K> {
    #[cfg_attr(feature = "inline-more", inline)]
    fn len(&self) -> usize {
        self.iter.len()
    }
}
impl<K> FusedIterator for IntoIter<K> {}

impl<K: fmt::Debug> fmt::Debug for IntoIter<K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let entries_iter = self.iter.iter().map(|(k, _)| k);
        f.debug_list().entries(entries_iter).finish()
    }
}

impl<K, E, G, R> Iterator for Drain<'_, K, E, G, R> {
    type Item = K;

    #[cfg_attr(feature = "inline-more", inline)]
    fn next(&mut self) -> Option<K> {
        // Avoid `Option::map` because it bloats LLVM IR.
        match self.iter.next() {
            Some((k, _)) => Some(k),
            None => None,
        }
    }
    #[cfg_attr(feature = "inline-more", inline)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
    #[cfg_attr(feature = "inline-more", inline)]
    fn fold<B, F>(self, init: B, mut f: F) -> B
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> B,
    {
        self.iter.fold(init, |acc, (k, ())| f(acc, k))
    }
}
impl<K, E, G, R> ExactSizeIterator for Drain<'_, K, E, G, R> {
    #[cfg_attr(feature = "inline-more", inline)]
    fn len(&self) -> usize {
        self.iter.len()
    }
}
impl<K, E, G, R> FusedIterator for Drain<'_, K, E, G, R> {}

impl<K: fmt::Debug, E, G, R> fmt::Debug for Drain<'_, K, E, G, R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let entries_iter = self.iter.iter().map(|(k, _)| k);
        f.debug_list().entries(entries_iter).finish()
    }
}

impl<K, F, E, G, R> Iterator for ExtractIf<'_, K, F, E, G, R>
where
    F: FnMut(&K) -> bool,
{
    type Item = K;

    #[cfg_attr(feature = "inline-more", inline)]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next(|&mut (ref k, ())| (self.f)(k))
            .map(|(k, ())| k)
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, self.inner.iter.size_hint().1)
    }
}

impl<K, F, E, G, R> FusedIterator for ExtractIf<'_, K, F, E, G, R> where F: FnMut(&K) -> bool {}

/// A view into a single entry in a set, which may either be vacant or occupied.
///
/// This `enum` is constructed from the [`entry`] method on [`HashSet`].
///
/// [`HashSet`]: struct.HashSet.html
/// [`entry`]: struct.HashSet.html#method.entry
///
/// # Examples
///
/// ```
/// use hashbrown::hash_set::{Entry, HashSet, OccupiedEntry};
///
/// # fn main () {
/// let mut set: HashSet<&str, (), (), ()> = HashSet::from_ary(["a", "b", "c"], &mut (), &mut ()).unwrap();
/// assert_eq!(set.len(), 3);
///
/// // Existing value (insert)
/// let entry: Entry<_, _, _, _, _> = set.entry("a", &mut (), &mut ()).unwrap();
/// let _raw_o: OccupiedEntry<_, _, _, _, _> = entry.insert();
/// assert_eq!(set.len(), 3);
/// // Nonexistent value (insert)
/// set.entry("d", &mut (), &mut ()).unwrap().insert();
///
/// // Existing value (or_insert)
/// set.entry("b", &mut (), &mut ()).unwrap().or_insert();
/// // Nonexistent value (or_insert)
/// set.entry("e", &mut (), &mut ()).unwrap().or_insert();
///
/// println!("Our HashSet: {:?}", set);
///
/// let mut vec: Vec<_> = set.iter().copied().collect();
/// // The `Iter` iterator produces items in arbitrary order, so the
/// // items must be sorted to test them against a sorted array.
/// vec.sort_unstable();
/// assert_eq!(vec, ["a", "b", "c", "d", "e"]);
/// # }
/// ```
pub enum Entry<'a, T, E, G, R, S> {
    /// An occupied entry.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::hash_set::{Entry, HashSet};
    /// let mut set: HashSet<_> = HashSet::from_ary(["a", "b"], &mut (), &mut ()).unwrap();
    ///
    /// match set.entry("a", &mut (), &mut ()).unwrap() {
    ///     Entry::Vacant(_) => unreachable!(),
    ///     Entry::Occupied(_) => { }
    /// }
    /// ```
    Occupied(OccupiedEntry<'a, T, E, G, R, S>),

    /// A vacant entry.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::hash_set::{Entry, HashSet};
    /// let mut set: HashSet<&str> = HashSet::new();
    ///
    /// match set.entry("a", &mut (), &mut ()).unwrap() {
    ///     Entry::Occupied(_) => unreachable!(),
    ///     Entry::Vacant(_) => { }
    /// }
    /// ```
    Vacant(VacantEntry<'a, T, E, G, R, S>),
}

impl<T: fmt::Debug, E, G, R, S> fmt::Debug for Entry<'_, T, E, G, R, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Entry::Vacant(ref v) => f.debug_tuple("Entry").field(v).finish(),
            Entry::Occupied(ref o) => f.debug_tuple("Entry").field(o).finish(),
        }
    }
}

/// A view into an occupied entry in a `HashSet`.
/// It is part of the [`Entry`] enum.
///
/// [`Entry`]: enum.Entry.html
///
/// # Examples
///
/// ```
/// use hashbrown::hash_set::{Entry, HashSet, OccupiedEntry};
/// let mut set = HashSet::new();
/// set.extend(["a", "b", "c"], &mut (), &mut ()).unwrap();
///
/// let _entry_o: OccupiedEntry<_, (), (), (), _> = set.entry("a", &mut (), &mut ()).unwrap().insert();
/// assert_eq!(set.len(), 3);
///
/// // Existing key
/// match set.entry("a", &mut (), &mut ()).unwrap() {
///     Entry::Vacant(_) => unreachable!(),
///     Entry::Occupied(view) => {
///         assert_eq!(view.get(), &"a");
///     }
/// }
///
/// assert_eq!(set.len(), 3);
///
/// // Existing key (take)
/// match set.entry("c", &mut (), &mut ()).unwrap() {
///     Entry::Vacant(_) => unreachable!(),
///     Entry::Occupied(view) => {
///         assert_eq!(view.remove(), "c");
///     }
/// }
/// assert_eq!(set.get(&"c", &mut (), &mut ()).unwrap(), None);
/// assert_eq!(set.len(), 2);
/// ```
pub struct OccupiedEntry<'a, T, E, G, R, S> {
    inner: map::OccupiedEntry<'a, T, (), E, G, R, S>,
}

impl<T: fmt::Debug, E, G, R, S> fmt::Debug for OccupiedEntry<'_, T, E, G, R, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("OccupiedEntry")
            .field("value", self.get())
            .finish()
    }
}

/// A view into a vacant entry in a `HashSet`.
/// It is part of the [`Entry`] enum.
///
/// [`Entry`]: enum.Entry.html
///
/// # Examples
///
/// ```
/// use hashbrown::hash_set::{Entry, HashSet, VacantEntry};
///
/// let mut set: HashSet<_> = HashSet::new();
///
/// let entry_v: VacantEntry<_, (), (), (), _> = match set.entry("a", &mut (), &mut ()).unwrap() {
///     Entry::Vacant(view) => view,
///     Entry::Occupied(_) => unreachable!(),
/// };
/// entry_v.insert();
/// assert!(set.contains("a", &mut (), &mut ()).unwrap() && set.len() == 1);
///
/// // Nonexistent key (insert)
/// match set.entry("b", &mut (), &mut ()).unwrap() {
///     Entry::Vacant(view) => { view.insert(); },
///     Entry::Occupied(_) => unreachable!(),
/// }
/// assert!(set.contains("b", &mut (), &mut ()).unwrap() && set.len() == 2);
/// ```
pub struct VacantEntry<'a, T, E, G, R, S> {
    inner: map::VacantEntry<'a, T, (), E, G, R, S>,
}

impl<T: fmt::Debug, E, G, R, S> fmt::Debug for VacantEntry<'_, T, E, G, R, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("VacantEntry").field(self.get()).finish()
    }
}

impl<'a, T, E, G, R, S> Entry<'a, T, E, G, R, S> {
    /// Sets the value of the entry, and returns an `OccupiedEntry`.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    ///
    /// let mut set: HashSet<&str> = HashSet::new();
    /// let entry = set.entry("horseyland", &mut (), &mut ()).unwrap().insert();
    ///
    /// assert_eq!(entry.get(), &"horseyland");
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn insert(self) -> OccupiedEntry<'a, T, E, G, R, S>
    where
        T: Hash,
        S: BuildHasher,
    {
        match self {
            Entry::Occupied(entry) => entry,
            Entry::Vacant(entry) => entry.insert(),
        }
    }

    /// Ensures a value is in the entry by inserting if it was vacant.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    ///
    /// let mut set: HashSet<&str> = HashSet::new();
    ///
    /// // nonexistent key
    /// set.entry("poneyland", &mut (), &mut ()).unwrap().or_insert();
    /// assert!(set.contains("poneyland", &mut (), &mut ()).unwrap());
    ///
    /// // existing key
    /// set.entry("poneyland", &mut (), &mut ()).unwrap().or_insert();
    /// assert!(set.contains("poneyland", &mut (), &mut ()).unwrap());
    /// assert_eq!(set.len(), 1);
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn or_insert(self)
    where
        T: Hash,
        S: BuildHasher,
    {
        if let Entry::Vacant(entry) = self {
            entry.insert();
        }
    }

    /// Returns a reference to this entry's value.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    ///
    /// let mut set: HashSet<&str> = HashSet::new();
    /// set.entry("poneyland", &mut (), &mut ()).unwrap().or_insert();
    /// // existing key
    /// assert_eq!(set.entry("poneyland", &mut (), &mut ()).unwrap().get(), &"poneyland");
    /// // nonexistent key
    /// assert_eq!(set.entry("horseland", &mut (), &mut ()).unwrap().get(
    /// ), &"horseland");
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn get(&self) -> &T {
        match *self {
            Entry::Occupied(ref entry) => entry.get(),
            Entry::Vacant(ref entry) => entry.get(),
        }
    }
}

impl<T, E, G, R, S> OccupiedEntry<'_, T, E, G, R, S> {
    /// Gets a reference to the value in the entry.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::hash_set::{Entry, HashSet};
    ///
    /// let mut set: HashSet<&str> = HashSet::new();
    /// set.entry("poneyland", &mut (), &mut ()).unwrap().or_insert();
    ///
    /// match set.entry("poneyland", &mut (), &mut ()).unwrap() {
    ///     Entry::Vacant(_) => panic!(),
    ///     Entry::Occupied(entry) => assert_eq!(entry.get(), &"poneyland"),
    /// }
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn get(&self) -> &T {
        self.inner.key()
    }

    /// Takes the value out of the entry, and returns it.
    /// Keeps the allocated memory for reuse.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    /// use hashbrown::hash_set::Entry;
    ///
    /// let mut set: HashSet<&str> = HashSet::new();
    /// // The set is empty
    /// assert!(set.is_empty() && set.capacity() == 0);
    ///
    /// set.entry("poneyland", &mut (), &mut ()).unwrap().or_insert();
    /// let capacity_before_remove = set.capacity();
    ///
    /// if let Entry::Occupied(o) = set.entry("poneyland", &mut (), &mut ()).unwrap() {
    ///     assert_eq!(o.remove(), "poneyland");
    /// }
    ///
    /// assert_eq!(set.contains("poneyland", &mut (), &mut ()).unwrap(), false);
    /// // Now set hold none elements but capacity is equal to the old one
    /// assert!(set.len() == 0 && set.capacity() == capacity_before_remove);
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn remove(self) -> T {
        self.inner.remove_entry().0
    }
}

impl<'a, T, E, G, R, S> VacantEntry<'a, T, E, G, R, S> {
    /// Gets a reference to the value that would be used when inserting
    /// through the `VacantEntry`.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    ///
    /// let mut set: HashSet<&str> = HashSet::new();
    /// assert_eq!(set.entry("poneyland", &mut (), &mut ()).unwrap().get(), &"poneyland");
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn get(&self) -> &T {
        self.inner.key()
    }

    /// Take ownership of the value.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::hash_set::{Entry, HashSet};
    /// struct E;
    /// struct G;
    ///
    /// let mut set: HashSet<&str, E, G, ()> = HashSet::new();
    ///
    /// match set.entry("poneyland", &mut E, &mut G).unwrap() {
    ///     Entry::Occupied(_) => panic!(),
    ///     Entry::Vacant(v) => assert_eq!(v.into_value(), "poneyland"),
    /// }
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn into_value(self) -> T {
        self.inner.into_key()
    }

    /// Sets the value of the entry with the `VacantEntry`'s value.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbrown::HashSet;
    /// use hashbrown::hash_set::Entry;
    /// struct E;
    /// struct G;
    ///
    /// let mut set: HashSet<&str, _, _, ()> = HashSet::new();
    ///
    /// if let Entry::Vacant(o) = set.entry("poneyland", &mut E, &mut G).unwrap() {
    ///     o.insert();
    /// }
    /// assert!(set.contains("poneyland", &mut E, &mut G).unwrap());
    /// ```
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn insert(self) -> OccupiedEntry<'a, T, E, G, R, S>
    where
        T: Hash,
        S: BuildHasher,
    {
        OccupiedEntry {
            inner: self.inner.insert_entry(()),
        }
    }
}

struct E;
struct G;

#[allow(dead_code)]
fn assert_covariance() {
    fn set<'new, E, G>(v: HashSet<&'static str, E, G, ()>) -> HashSet<&'new str, E, G, ()> {
        v
    }
    fn iter<'a, 'new>(v: Iter<'a, &'static str>) -> Iter<'a, &'new str> {
        v
    }
    fn into_iter<'new>(v: IntoIter<&'static str>) -> IntoIter<&'new str> {
        v
    }
    fn drain<'new>(d: Drain<'static, &'static str, E, G, ()>) -> Drain<'new, &'new str, E, G, ()> {
        d
    }
}

#[cfg(test)]
mod test_set {
    use equivalent::RubyEql;

    use super::{make_hash, Equivalent, HashSet};
    use crate::DefaultHashBuilder;
    use std::vec::Vec;
    struct E;
    struct G;

    #[test]
    fn test_zero_capacities() {
        let mut e = E;
        let mut g = G;
        type HS = HashSet<i32, E, G, ()>;

        let s = HS::new();
        assert_eq!(s.capacity(), 0);

        let s = HS::default();
        assert_eq!(s.capacity(), 0);

        let s = HS::with_hasher(DefaultHashBuilder::default());
        assert_eq!(s.capacity(), 0);

        let s = HS::with_capacity(0);
        assert_eq!(s.capacity(), 0);

        let s = HS::with_capacity_and_hasher(0, DefaultHashBuilder::default());
        assert_eq!(s.capacity(), 0);

        let mut s = HS::new();
        s.insert(1, &mut e, &mut g).unwrap();
        s.insert(2, &mut e, &mut g).unwrap();
        s.remove(&1, &mut e, &mut g).unwrap();
        s.remove(&2, &mut e, &mut g).unwrap();
        s.shrink_to_fit();
        assert_eq!(s.capacity(), 0);

        let mut s = HS::new();
        s.reserve(0);
        assert_eq!(s.capacity(), 0);
    }

    #[test]
    fn test_subset_and_superset() {
        let mut e = E;
        let mut g = G;
        let mut a = HashSet::<_, E, G, ()>::new();
        assert!(a.insert(0, &mut e, &mut g).unwrap());
        assert!(a.insert(5, &mut e, &mut g).unwrap());
        assert!(a.insert(11, &mut e, &mut g).unwrap());
        assert!(a.insert(7, &mut e, &mut g).unwrap());

        let mut b = HashSet::new();
        assert!(b.insert(0, &mut e, &mut g).unwrap());
        assert!(b.insert(7, &mut e, &mut g).unwrap());
        assert!(b.insert(19, &mut e, &mut g).unwrap());
        assert!(b.insert(250, &mut e, &mut g).unwrap());
        assert!(b.insert(11, &mut e, &mut g).unwrap());
        assert!(b.insert(200, &mut e, &mut g).unwrap());

        assert!(!a.is_subset(&b, &mut e, &mut g).unwrap());
        assert!(!a.is_superset(&b, &mut e, &mut g).unwrap());
        assert!(!b.is_subset(&a, &mut e, &mut g).unwrap());
        assert!(!b.is_superset(&a, &mut e, &mut g).unwrap());

        assert!(b.insert(5, &mut e, &mut g).unwrap());

        assert!(a.is_subset(&b, &mut e, &mut g).unwrap());
        assert!(!a.is_superset(&b, &mut e, &mut g).unwrap());
        assert!(!b.is_subset(&a, &mut e, &mut g).unwrap());
        assert!(b.is_superset(&a, &mut e, &mut g).unwrap());
    }

    #[test]
    fn test_iterate() {
        let mut e = E;
        let mut g = G;
        let mut a = HashSet::<_, E, G, ()>::new();
        for i in 0..32 {
            assert!(a.insert(i, &mut e, &mut g).unwrap());
        }
        let mut observed: u32 = 0;
        for k in &a {
            observed |= 1 << *k;
        }
        assert_eq!(observed, 0xFFFF_FFFF);
    }

    #[test]
    fn test_from_map() {
        let mut e = E;
        let mut g = G;
        let mut a = crate::HashMap::new();
        a.insert(1, (), &mut e, &mut g).unwrap();
        a.insert(2, (), &mut e, &mut g).unwrap();
        a.insert(3, (), &mut e, &mut g).unwrap();
        a.insert(4, (), &mut e, &mut g).unwrap();

        let a: HashSet<_, E, G, ()> = a.into();

        assert_eq!(a.len(), 4);
        assert!(a.contains(&1, &mut e, &mut g).unwrap());
        assert!(a.contains(&2, &mut e, &mut g).unwrap());
        assert!(a.contains(&3, &mut e, &mut g).unwrap());
        assert!(a.contains(&4, &mut e, &mut g).unwrap());
    }

    #[test]
    fn test_from_iter() {
        let mut e = E;
        let mut g = G;
        let xs = [1, 2, 2, 3, 4, 5, 6, 7, 8, 9];

        let set: HashSet<_, E, G, ()> =
            HashSet::from_iter(xs.iter().copied(), &mut e, &mut g).unwrap();

        for x in &xs {
            assert!(set.contains(x, &mut e, &mut g).unwrap());
        }

        assert_eq!(set.iter().len(), xs.len() - 1);
    }

    #[test]
    fn test_move_iter() {
        let hs = {
            let mut e = E;
            let mut g = G;
            let mut hs = HashSet::<_, E, G, ()>::new();

            hs.insert('a', &mut e, &mut g).unwrap();
            hs.insert('b', &mut e, &mut g).unwrap();

            hs
        };

        let v = hs.into_iter().collect::<Vec<char>>();
        assert!(v == ['a', 'b'] || v == ['b', 'a']);
    }

    #[test]
    fn test_eq() {
        // These constants once happened to expose a bug in insert().
        // I'm keeping them around to prevent a regression.
        let mut e = E;
        let mut g = G;
        let mut s1 = HashSet::<_, E, G, ()>::new();

        s1.insert(1, &mut e, &mut g).unwrap();
        s1.insert(2, &mut e, &mut g).unwrap();
        s1.insert(3, &mut e, &mut g).unwrap();

        let mut s2 = HashSet::<_, E, G, ()>::new();

        s2.insert(1, &mut e, &mut g).unwrap();
        s2.insert(2, &mut e, &mut g).unwrap();

        assert!(!s1.eql(&s2, &mut e, &mut g).unwrap());

        s2.insert(3, &mut e, &mut g).unwrap();

        assert!(s1.eql(&s2, &mut e, &mut g).unwrap());
    }

    #[test]
    fn test_show() {
        let mut e = E;
        let mut g = G;
        let mut set = HashSet::<i32, E, G, ()>::new();
        let empty = HashSet::<i32, E, G, ()>::new();

        set.insert(1, &mut e, &mut g).unwrap();
        set.insert(2, &mut e, &mut g).unwrap();

        let set_str = format!("{set:?}");

        assert!(set_str == "{1, 2}" || set_str == "{2, 1}");
        assert_eq!(format!("{empty:?}"), "{}");
    }

    #[test]
    fn test_trivial_drain() {
        let mut s = HashSet::<i32, E, G, ()>::new();
        for _ in s.drain() {}
        assert!(s.is_empty());
        drop(s);

        let mut s = HashSet::<i32, E, G, ()>::new();
        drop(s.drain());
        assert!(s.is_empty());
    }

    #[test]
    fn test_drain() {
        let mut e = E;
        let mut g = G;
        let mut s: HashSet<_, E, G, ()> = HashSet::from_iter(1..100, &mut e, &mut g).unwrap();

        // try this a bunch of times to make sure we don't screw up internal state.
        for _ in 0..20 {
            assert_eq!(s.len(), 99);

            {
                let mut last_i = 0;
                let mut d = s.drain();
                for (i, x) in d.by_ref().take(50).enumerate() {
                    last_i = i;
                    assert!(x != 0);
                }
                assert_eq!(last_i, 49);
            }

            if !s.is_empty() {
                panic!("s should be empty!");
            }

            // reset to try again.
            s.extend(1..100, &mut e, &mut g).unwrap();
        }
    }

    #[test]
    fn test_replace() {
        use core::hash;

        #[derive(Debug)]
        #[allow(dead_code)]
        struct Foo(&'static str, i32);

        impl PartialEq for Foo {
            fn eq(&self, other: &Self) -> bool {
                self.0 == other.0
            }
        }

        impl equivalent::RubyEql<E, G, ()> for Foo {
            fn eql(&self, other: &Self, _: &mut E, _: &mut G) -> Result<bool, ()> {
                Ok(self == other)
            }
        }

        impl hash::Hash for Foo {
            fn hash<H: hash::Hasher>(&self, h: &mut H) {
                self.0.hash(h);
            }
        }

        let mut e = E;
        let mut g = G;
        let mut s = HashSet::<_, E, G, ()>::new();
        assert_eq!(s.replace(Foo("a", 1), &mut e, &mut g).unwrap(), None);
        assert_eq!(s.len(), 1);
        assert_eq!(
            s.replace(Foo("a", 2), &mut e, &mut g).unwrap(),
            Some(Foo("a", 1))
        );
        assert_eq!(s.len(), 1);

        let mut it = s.iter();
        assert_eq!(it.next(), Some(&Foo("a", 2)));
        assert_eq!(it.next(), None);
    }

    #[test]
    #[allow(clippy::needless_borrow)]
    fn test_extend_ref() {
        let mut e = E;
        let mut g = G;
        let mut a = HashSet::<i32, E, G, ()>::new();
        a.insert(1, &mut e, &mut g).unwrap();

        a.extend([2, 3, 4], &mut e, &mut g).unwrap();

        assert_eq!(a.len(), 4);
        assert!(a.contains(&1, &mut e, &mut g).unwrap());
        assert!(a.contains(&2, &mut e, &mut g).unwrap());
        assert!(a.contains(&3, &mut e, &mut g).unwrap());
        assert!(a.contains(&4, &mut e, &mut g).unwrap());

        let mut b = HashSet::<i32, E, G, ()>::new();
        b.insert(5, &mut e, &mut g).unwrap();
        b.insert(6, &mut e, &mut g).unwrap();

        a.extend(b.iter().cloned(), &mut e, &mut g).unwrap();

        assert_eq!(a.len(), 6);
        assert!(a.contains(&1, &mut e, &mut g).unwrap());
        assert!(a.contains(&2, &mut e, &mut g).unwrap());
        assert!(a.contains(&3, &mut e, &mut g).unwrap());
        assert!(a.contains(&4, &mut e, &mut g).unwrap());
        assert!(a.contains(&5, &mut e, &mut g).unwrap());
        assert!(a.contains(&6, &mut e, &mut g).unwrap());
    }

    #[test]
    fn test_retain() {
        let xs = [1, 2, 3, 4, 5, 6];
        let mut e = E;
        let mut g = G;
        let mut set: HashSet<i32, E, G, ()> =
            HashSet::from_iter(xs.iter().copied(), &mut e, &mut g).unwrap();
        set.retain(|&k| k % 2 == 0);
        assert_eq!(set.len(), 3);
        assert!(set.contains(&2, &mut e, &mut g).unwrap());
        assert!(set.contains(&4, &mut e, &mut g).unwrap());
        assert!(set.contains(&6, &mut e, &mut g).unwrap());
    }

    #[test]
    fn test_extract_if() {
        let mut e = E;
        let mut g = G;
        {
            let mut set: HashSet<i32, E, G, ()> = HashSet::from_iter(0..8, &mut e, &mut g).unwrap();
            let drained = set.extract_if(|&k| k % 2 == 0);
            let mut out = drained.collect::<Vec<_>>();
            out.sort_unstable();
            assert_eq!(vec![0, 2, 4, 6], out);
            assert_eq!(set.len(), 4);
        }
        {
            let mut set: HashSet<i32, E, G, ()> = HashSet::from_iter(0..8, &mut e, &mut g).unwrap();
            set.extract_if(|&k| k % 2 == 0).for_each(drop);
            assert_eq!(set.len(), 4, "Removes non-matching items on drop");
        }
    }

    #[test]
    fn test_const_with_hasher() {
        use core::hash::BuildHasher;
        use std::collections::hash_map::DefaultHasher;

        #[derive(Clone)]
        struct MyHasher;
        impl BuildHasher for MyHasher {
            type Hasher = DefaultHasher;

            fn build_hasher(&self) -> DefaultHasher {
                DefaultHasher::new()
            }
        }

        const EMPTY_SET: HashSet<u32, E, G, (), MyHasher> = HashSet::with_hasher(MyHasher);

        let mut e = E;
        let mut g = G;
        let mut set = EMPTY_SET;
        set.insert(19, &mut e, &mut g).unwrap();
        assert!(set.contains(&19, &mut e, &mut g).unwrap());
    }

    #[test]
    fn rehash_in_place() {
        let mut e = E;
        let mut g = G;
        let mut set = HashSet::<i32, E, G, ()>::new();

        for i in 0..224 {
            set.insert(i, &mut e, &mut g).unwrap();
        }

        assert_eq!(
            set.capacity(),
            224,
            "The set must be at or close to capacity to trigger a re hashing"
        );

        for i in 100..1400 {
            set.remove(&(i - 100), &mut e, &mut g).unwrap();
            set.insert(i, &mut e, &mut g).unwrap();
        }
    }

    #[test]
    fn collect() {
        // At the time of writing, this hits the ZST case in from_base_index
        // (and without the `map`, it does not).
        let mut e = E;
        let mut g = G;
        let mut _set: HashSet<_, E, G, ()> =
            HashSet::from_iter((0..3).map(|_| ()), &mut e, &mut g).unwrap();
    }

    #[test]
    fn test_allocation_info() {
        assert_eq!(HashSet::<(), E, G, ()>::new().allocation_size(), 0);
        assert_eq!(HashSet::<u32, E, G, ()>::new().allocation_size(), 0);
        assert!(
            HashSet::<u32, E, G, ()>::with_capacity(1).allocation_size()
                > core::mem::size_of::<u32>()
        );
    }

    #[test]
    fn duplicate_insert() {
        let mut e = E;
        let mut g = G;
        let mut set = HashSet::<_, E, G, ()>::new();
        set.insert(1, &mut e, &mut g).unwrap();
        set.get_or_insert_with(&1, |_| 1, &mut e, &mut g).unwrap();
        set.get_or_insert_with(&1, |_| 1, &mut e, &mut g).unwrap();
        assert!([1].iter().eq(set.iter()));
    }

    #[test]
    #[should_panic]
    fn some_invalid_equivalent() {
        use core::hash::{Hash, Hasher};
        struct Invalid {
            count: u32,
            other: u32,
        }

        struct InvalidRef {
            count: u32,
            other: u32,
        }

        impl PartialEq for Invalid {
            fn eq(&self, other: &Self) -> bool {
                self.count == other.count && self.other == other.other
            }
        }
        impl RubyEql<E, G, ()> for Invalid {
            fn eql(&self, other: &Self, _: &mut E, _: &mut G) -> Result<bool, ()> {
                Ok(self == other)
            }
        }

        impl Equivalent<Invalid, E, G, ()> for InvalidRef {
            fn equivalent(&self, key: &Invalid, _: &mut E, _: &mut G) -> Result<bool, ()> {
                Ok(self.count == key.count && self.other == key.other)
            }
        }
        impl Hash for Invalid {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.count.hash(state);
            }
        }
        impl Hash for InvalidRef {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.count.hash(state);
            }
        }
        let mut e = E;
        let mut g = G;
        let mut set: HashSet<Invalid, E, G, ()> = HashSet::new();
        let key = InvalidRef { count: 1, other: 1 };
        let value = Invalid { count: 1, other: 2 };
        if make_hash(set.hasher(), &key) == make_hash(set.hasher(), &value) {
            set.get_or_insert_with(&key, |_| value, &mut e, &mut g)
                .unwrap();
        }
    }
}
