//! This crate is a Rust port of Google's high-performance [SwissTable] hash
//! map, adapted to make it a drop-in replacement for Rust's standard `HashMap`
//! and `HashSet` types.
//!
//! The original C++ version of [SwissTable] can be found [here], and this
//! [CppCon talk] gives an overview of how the algorithm works.
//!
//! [SwissTable]: https://abseil.io/blog/20180927-swisstables
//! [here]: https://github.com/abseil/abseil-cpp/blob/master/absl/container/internal/raw_hash_set.h
//! [CppCon talk]: https://www.youtube.com/watch?v=ncHmEUmJZf4

#![no_std]
#![allow(
    clippy::doc_markdown,
    clippy::module_name_repetitions,
    clippy::must_use_candidate,
    clippy::option_if_let_else,
    clippy::redundant_else,
    clippy::manual_map,
    clippy::missing_safety_doc,
    clippy::missing_errors_doc
)]
#![warn(missing_docs)]
#![warn(rust_2018_idioms)]

/// Default hasher for [`HashMap`] and [`HashSet`].
#[cfg(feature = "default-hasher")]
pub type DefaultHashBuilder = foldhash::fast::RandomState;

/// Dummy default hasher for [`HashMap`] and [`HashSet`].
#[cfg(not(feature = "default-hasher"))]
pub enum DefaultHashBuilder {}

#[cfg(test)]
#[macro_use]
extern crate std;

#[cfg_attr(test, macro_use)]
extern crate alloc;

#[macro_use]
mod macros;

mod control;
mod raw;
mod util;

mod map;
mod scopeguard;
mod set;
mod table;

pub mod hash_map {
    //! A hash map implemented with quadratic probing and SIMD lookup.
    pub use crate::map::*;
}
pub mod hash_set {
    //! A hash set implemented as a `HashMap` where the value is `()`.
    pub use crate::set::*;
}
pub mod hash_table {
    //! A hash table implemented with quadratic probing and SIMD lookup.
    pub use crate::table::*;
}

pub use crate::map::HashMap;
pub use crate::set::HashSet;
pub use crate::table::HashTable;

#[cfg(feature = "equivalent")]
pub use equivalent::Equivalent;

// This is only used as a fallback when building as part of `std`.
#[cfg(not(feature = "equivalent"))]
/// Key equivalence trait.
///
/// This trait defines the function used to compare the input value with the
/// map keys (or set values) during a lookup operation such as [`HashMap::get`]
/// or [`HashSet::contains`].
/// It is provided with a blanket implementation based on the
/// [`Borrow`](core::borrow::Borrow) trait.
///
/// # Correctness
///
/// Equivalent values must hash to the same value.
pub trait Equivalent<K: ?Sized> {
    /// Checks if this value is equivalent to the given key.
    ///
    /// Returns `true` if both values are equivalent, and `false` otherwise.
    ///
    /// # Correctness
    ///
    /// When this function returns `true`, both `self` and `key` must hash to
    /// the same value.
    fn equivalent(&self, key: &K) -> bool;
}

#[cfg(not(feature = "equivalent"))]
impl<Q: ?Sized, K: ?Sized> Equivalent<K> for Q
where
    Q: Eq,
    K: core::borrow::Borrow<Q>,
{
    fn equivalent(&self, key: &K) -> bool {
        self == key.borrow()
    }
}

/// The error type for `try_reserve` methods.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TryReserveError {
    /// Error due to the computed capacity exceeding the collection's maximum
    /// (usually `isize::MAX` bytes).
    CapacityOverflow,

    /// The memory allocator returned an error
    AllocError {
        /// The layout of the allocation request that failed.
        layout: alloc::alloc::Layout,
    },
}
