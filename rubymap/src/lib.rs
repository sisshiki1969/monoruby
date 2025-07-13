// We *mostly* avoid unsafe code, but `Slice` allows it for DST casting.
#![deny(unsafe_code)]
#![warn(rust_2018_idioms)]

//! [`IndexMap`] is a hash table where the iteration order of the key-value
//! pairs is independent of the hash values of the keys.
//!
//! [`RubySet`] is a corresponding hash set using the same implementation and
//! with similar properties.
//!
//! ### Highlights
//!
//! [`IndexMap`] and [`RubySet`] are drop-in compatible with the std `HashMap`
//! and `HashSet`, but they also have some features of note:
//!
//! - The ordering semantics (see their documentation for details)
//! - Sorting methods and the [`.pop()`][IndexMap::pop] methods.
//! - The [`Equivalent`] trait, which offers more flexible equality definitions
//!   between borrowed and owned versions of keys.
//! - The [`MutableKeys`][map::MutableKeys] trait, which gives opt-in mutable
//!   access to map keys, and [`MutableValues`][set::MutableValues] for sets.
//!
//! ### Feature Flags
//!
//! To reduce the amount of compiled code in the crate by default, certain
//! features are gated behind [feature flags]. These allow you to opt in to (or
//! out of) functionality. Below is a list of the features available in this
//! crate.
//!
//! * `std`: Enables features which require the Rust standard library. For more
//!   information see the section on [`no_std`].
//! * `rayon`: Enables parallel iteration and other parallel methods.
//! * `serde`: Adds implementations for [`Serialize`] and [`Deserialize`]
//!   to [`IndexMap`] and [`RubySet`]. Alternative implementations for
//!   (de)serializing [`IndexMap`] as an ordered sequence are available in the
//!   [`map::serde_seq`] module.
//! * `arbitrary`: Adds implementations for the [`arbitrary::Arbitrary`] trait
//!   to [`IndexMap`] and [`RubySet`].
//! * `quickcheck`: Adds implementations for the [`quickcheck::Arbitrary`] trait
//!   to [`IndexMap`] and [`RubySet`].
//! * `borsh` (**deprecated**): Adds implementations for [`BorshSerialize`] and
//!   [`BorshDeserialize`] to [`IndexMap`] and [`RubySet`]. Due to a cyclic
//!   dependency that arose between [`borsh`] and `indexmap`, `borsh v1.5.6`
//!   added an `indexmap` feature that should be used instead of enabling the
//!   feature here.
//!
//! _Note: only the `std` feature is enabled by default._
//!
//! [feature flags]: https://doc.rust-lang.org/cargo/reference/manifest.html#the-features-section
//! [`no_std`]: #no-standard-library-targets
//! [`Serialize`]: `::serde::Serialize`
//! [`Deserialize`]: `::serde::Deserialize`
//! [`BorshSerialize`]: `::borsh::BorshSerialize`
//! [`BorshDeserialize`]: `::borsh::BorshDeserialize`
//! [`borsh`]: `::borsh`
//! [`arbitrary::Arbitrary`]: `::arbitrary::Arbitrary`
//! [`quickcheck::Arbitrary`]: `::quickcheck::Arbitrary`
//!
//! ### Alternate Hashers
//!
//! [`IndexMap`] and [`RubySet`] have a default hasher type
//! [`S = RandomState`][std::collections::hash_map::RandomState],
//! just like the standard `HashMap` and `HashSet`, which is resistant to
//! HashDoS attacks but not the most performant. Type aliases can make it easier
//! to use alternate hashers:
//!
//! ### Rust Version
//!
//! This version of indexmap requires Rust 1.63 or later.
//!
//! The indexmap 2.x release series will use a carefully considered version
//! upgrade policy, where in a later 2.x version, we will raise the minimum
//! required Rust version.
//!
//! ## No Standard Library Targets
//!
//! This crate supports being built without `std`, requiring `alloc` instead.
//! This is chosen by disabling the default "std" cargo feature, by adding
//! `default-features = false` to your dependency specification.
//!
//! - Creating maps and sets using [`new`][IndexMap::new] and
//!   [`with_capacity`][IndexMap::with_capacity] is unavailable without `std`.
//!   Use methods [`IndexMap::default`], [`with_hasher`][IndexMap::with_hasher],
//!   [`with_capacity_and_hasher`][IndexMap::with_capacity_and_hasher] instead.
//!   A no-std compatible hasher will be needed as well, for example
//!   from the crate `twox-hash`.
//! - Macros [`indexmap!`] and [`indexset!`] are unavailable without `std`. Use
//!   the macros [`indexmap_with_default!`] and [`indexset_with_default!`] instead.

#![cfg_attr(docsrs, feature(doc_cfg))]

extern crate alloc;

#[macro_use]
extern crate std;

use alloc::vec::{self, Vec};

#[macro_use]
mod macros;
mod util;

pub mod map;
pub mod set;

pub use crate::map::RubyMap;
pub use crate::set::RubySet;
pub use equivalent::{Equivalent, RubyEql};

// shared private items

/// Hash value newtype. Not larger than usize, since anything larger
/// isn't used for selecting position anyway.
#[derive(Clone, Copy, Debug, PartialEq)]
struct HashValue(usize);

impl HashValue {
    #[inline(always)]
    fn get(self) -> u64 {
        self.0 as u64
    }
}

#[derive(Copy, Debug)]
struct Bucket<K, V> {
    hash: HashValue,
    key: K,
    value: V,
}

impl<K, V> Clone for Bucket<K, V>
where
    K: Clone,
    V: Clone,
{
    fn clone(&self) -> Self {
        Bucket {
            hash: self.hash,
            key: self.key.clone(),
            value: self.value.clone(),
        }
    }

    fn clone_from(&mut self, other: &Self) {
        self.hash = other.hash;
        self.key.clone_from(&other.key);
        self.value.clone_from(&other.value);
    }
}

impl<K, V> Bucket<K, V> {
    // field accessors -- used for `f` instead of closures in `.map(f)`
    fn key_ref(&self) -> &K {
        &self.key
    }
    fn value_ref(&self) -> &V {
        &self.value
    }
    fn value_mut(&mut self) -> &mut V {
        &mut self.value
    }
    fn key(self) -> K {
        self.key
    }
    fn value(self) -> V {
        self.value
    }
    fn key_value(self) -> (K, V) {
        (self.key, self.value)
    }
    fn refs(&self) -> (&K, &V) {
        (&self.key, &self.value)
    }
    fn ref_mut(&mut self) -> (&K, &mut V) {
        (&self.key, &mut self.value)
    }
    /*fn muts(&mut self) -> (&mut K, &mut V) {
        (&mut self.key, &mut self.value)
    }*/
}

trait Entries {
    type Entry;
    fn into_entries(self) -> Vec<Self::Entry>;
    fn as_entries(&self) -> &[Self::Entry];
    fn as_entries_mut(&mut self) -> &mut [Self::Entry];
    fn with_entries<F>(&mut self, f: F)
    where
        F: FnOnce(&mut [Self::Entry]);
}

// NOTE: This is copied from the slice module in the std lib.
/// The error type returned by [`get_disjoint_indices_mut`][`IndexMap::get_disjoint_indices_mut`].
///
/// It indicates one of two possible errors:
/// - An index is out-of-bounds.
/// - The same index appeared multiple times in the array.
//    (or different but overlapping indices when ranges are provided)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GetDisjointMutError {
    /// An index provided was out-of-bounds for the slice.
    IndexOutOfBounds,
    /// Two indices provided were overlapping.
    OverlappingIndices,
}

impl core::fmt::Display for GetDisjointMutError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let msg = match self {
            GetDisjointMutError::IndexOutOfBounds => "an index is out of bounds",
            GetDisjointMutError::OverlappingIndices => "there were overlapping indices",
        };

        core::fmt::Display::fmt(msg, f)
    }
}

impl std::error::Error for GetDisjointMutError {}
