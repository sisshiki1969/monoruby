#![feature(box_patterns)]
#![feature(int_roundings)]
#![feature(const_option)]
#![feature(lazy_cell)]
#![allow(clippy::too_many_arguments)]
#![feature(iter_next_chunk)]
#![feature(result_flattening)]
#![feature(let_chains)]
mod alloc;
mod executor;
mod id_table;
#[cfg(test)]
mod tests;
mod value;

// use alloc::*;
pub use executor::*;
use fxhash::FxHashMap as HashMap;
use fxhash::FxHashSet as HashSet;
pub type IndexMap<K, V> = indexmap::IndexMap<K, V, fxhash::FxBuildHasher>;
use id_table::*;
use value::*;

pub use value::rvalue::RegexpInner;

pub const STRING_INLINE_CAP: usize = 39;
