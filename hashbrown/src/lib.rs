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

use foldhash;

/// Default hasher for [`HashMap`] and [`HashSet`].
pub type DefaultHashBuilder = foldhash::fast::RandomState;

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

pub use equivalent::{Equivalent, RubyEql};
