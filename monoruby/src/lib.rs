#![feature(box_patterns)]
#![feature(int_roundings)]
#![feature(const_option)]
#![feature(lazy_cell)]
#![allow(clippy::too_many_arguments)]
#![feature(iter_next_chunk)]
#![feature(result_flattening)]
#![feature(let_chains)]
#![feature(step_trait)]
#![feature(naked_functions)]
#![feature(offset_of)]
mod alloc;
pub mod bytecodegen;
pub mod executor;
mod id_table;
#[cfg(test)]
mod tests;
mod value;

pub use executor::*;
use fxhash::FxHashMap as HashMap;
use fxhash::FxHashSet as HashSet;
pub use monoruby_attr::*;
pub type IndexMap<K, V> = indexmap::IndexMap<K, V, fxhash::FxBuildHasher>;
pub type IndexSet<T> = indexmap::IndexSet<T>;
use id_table::*;
pub use value::*;

pub use value::rvalue::RegexpInner;

pub const STRING_INLINE_CAP: usize = 39;
