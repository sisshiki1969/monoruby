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
//! [`S = RubyRandomState`][crate::RubyRandomState]: a seeded,
//! non-cryptographic mixer rather than the standard library's SipHash-1-3
//! (see `hasher.rs` for why, and what the seed does and does not buy).
//! Type aliases can make it easier to use alternate hashers:
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
mod hasher;
mod util;

pub mod map;
pub mod set;

pub use crate::hasher::{RubyHasher, RubyRandomState};
pub use crate::map::RubyMap;
pub use crate::set::RubySet;
pub use ruby_traits::{Equivalent, RubyEql, RubyHash, RubySymEql, RubySymHash};

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


///
/// The in-memory layout needed to read a [`RubyMap`]'s entries directly
/// from generated machine code.
///
/// Every offset here is either derived from the compiler (`offset_of!`)
/// or probed at run time — none of them is assumed. Two are genuinely
/// counter-intuitive, and guessing either would be silent memory
/// corruption rather than a compile error:
///
/// * `Bucket<K, V>` is `repr(Rust)`, and its field order depends on the
///   niches of `K`/`V`. `Bucket<u64, u64>` lays out as hash/key/value,
///   but `Bucket<NonZeroU64, NonZeroU64>` — the shape monoruby's `Value`
///   actually has — lays out as key/value/hash.
/// * `Vec`'s three words are documented as a (pointer, capacity, length)
///   triplet "in an unspecified order". On the pinned toolchain that
///   order is capacity, pointer, length: the data pointer is *not* first.
///
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct EntriesLayout {
    /// Offset from `&RubyMap` to the entry vector's data pointer.
    pub ptr_offset: usize,
    /// Offset from `&RubyMap` to the entry vector's length.
    pub len_offset: usize,
    /// Stride between consecutive entries.
    pub bucket_size: usize,
    /// Offset of the key within one entry.
    pub key_offset: usize,
    /// Offset of the value within one entry.
    pub value_offset: usize,
}

///
/// Resolve the [`EntriesLayout`] for one `RubyMap` instantiation, or
/// `None` if the `Vec` probe cannot identify all three words
/// unambiguously.
///
/// A `None` result is not a failure: the caller is expected to keep its
/// existing out-of-line path, so an unrecognised future `Vec` layout
/// costs performance rather than correctness.
///
#[allow(unsafe_code)]
pub fn entries_layout<K, V, E, G, R, S>() -> Option<EntriesLayout> {
    // Probe a real vector rather than trusting a field order. Capacity 8
    // and length 0 are distinctive: no live allocation is at address 8,
    // and a capacity of 8 cannot be mistaken for the length.
    const PROBE_CAPA: usize = 8;
    let v: Vec<Bucket<K, V>> = Vec::with_capacity(PROBE_CAPA);
    let ptr = v.as_ptr() as usize;
    if v.capacity() != PROBE_CAPA || ptr == PROBE_CAPA || ptr == 0 {
        return None;
    }
    debug_assert_eq!(std::mem::size_of::<Vec<Bucket<K, V>>>(), 3 * WORD);
    // SAFETY: `Vec` is a three-word value (pointer, capacity, length) and
    // all three words are initialized; we only read them as integers.
    let words: [usize; 3] = unsafe { std::ptr::read(&v as *const _ as *const [usize; 3]) };
    let word_at = |want: usize| -> Option<usize> {
        let mut found = None;
        let mut i = 0;
        while i < words.len() {
            if words[i] == want {
                // An ambiguous match means the probe cannot be trusted.
                if found.is_some() {
                    return None;
                }
                found = Some(i * WORD);
            }
            i += 1;
        }
        found
    };
    let vec_ptr = word_at(ptr)?;
    let vec_len = word_at(0)?;
    let vec_capa = word_at(PROBE_CAPA)?;
    // The three must be distinct words; anything else is unrecognised.
    if vec_ptr == vec_len || vec_ptr == vec_capa || vec_len == vec_capa {
        return None;
    }
    let entries = std::mem::offset_of!(RubyMap<K, V, E, G, R, S>, core)
        + crate::map::core::entries_offset::<K, V, E, G, R>();
    Some(EntriesLayout {
        ptr_offset: entries + vec_ptr,
        len_offset: entries + vec_len,
        bucket_size: std::mem::size_of::<Bucket<K, V>>(),
        key_offset: std::mem::offset_of!(Bucket<K, V>, key),
        value_offset: std::mem::offset_of!(Bucket<K, V>, value),
    })
}

const WORD: usize = std::mem::size_of::<usize>();

#[cfg(test)]
mod entries_layout_tests {
    use super::*;
    use std::collections::hash_map::RandomState;
    use std::num::NonZeroU64;

    /// Read entry `i` of `map` the way generated machine code would:
    /// through the probed offsets only, never through the Rust API.
    #[allow(unsafe_code)]
    unsafe fn raw_entry<K: Copy, V: Copy>(map: *const u8, lay: &EntriesLayout, i: usize) -> (K, V) {
        unsafe {
            let base = map.add(lay.ptr_offset).cast::<*const u8>().read();
            let entry = base.add(i * lay.bucket_size);
            (
                entry.add(lay.key_offset).cast::<K>().read(),
                entry.add(lay.value_offset).cast::<V>().read(),
            )
        }
    }

    #[allow(unsafe_code)]
    unsafe fn raw_len(map: *const u8, lay: &EntriesLayout) -> usize {
        unsafe { map.add(lay.len_offset).cast::<usize>().read() }
    }

    /// The offsets must agree with the safe API for every entry — this is
    /// what keeps a toolchain-driven layout change from silently becoming
    /// wrong loads in JIT-generated code.
    #[test]
    #[allow(unsafe_code)]
    fn layout_matches_the_safe_api() {
        let lay = entries_layout::<u64, u64, (), (), (), RandomState>().unwrap();
        let mut map: RubyMap<u64, u64> = RubyMap::new();
        // Enough entries to take the map past the linear `ar_table` form.
        for i in 0..64u64 {
            map.insert(i, i * 7 + 1, &mut (), &mut ()).unwrap();
        }
        let p = &map as *const _ as *const u8;
        assert_eq!(unsafe { raw_len(p, &lay) }, map.len());
        for i in 0..map.len() {
            let (k, v) = unsafe { raw_entry::<u64, u64>(p, &lay, i) };
            let (ek, ev) = map.get_index(i).unwrap();
            assert_eq!((k, v), (*ek, *ev), "entry {i}");
        }
    }

    /// A key type carrying a niche reorders `Bucket`'s fields, so the
    /// offsets must be read per instantiation. Hard-coding the
    /// plain-integer layout — the intuitive hash/key/value order — would
    /// make a `Value`-keyed map read its key out of the hash slot.
    ///
    /// The round-trip for the niche case is exercised against real
    /// `Value`s in monoruby (`hash::tests::entries_layout_*`); this crate
    /// cannot build such a map, because `NonZeroU64` does not implement
    /// `RubyHash`/`RubyEql`.
    #[test]
    fn niche_carrying_keys_reorder_the_bucket() {
        let plain = entries_layout::<u64, u64, (), (), (), RandomState>().unwrap();
        let niche = entries_layout::<NonZeroU64, NonZeroU64, (), (), (), RandomState>().unwrap();
        assert_eq!(plain.bucket_size, niche.bucket_size);
        assert_ne!(
            (plain.key_offset, plain.value_offset),
            (niche.key_offset, niche.value_offset),
            "expected the niche layout to differ from the plain one; if these \
             ever agree it is the assumption behind this test that changed, \
             and `entries_layout` is still the only safe source of offsets"
        );
    }
}
