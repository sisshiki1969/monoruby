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
mod inline;
#[cfg(test)]
mod tests;
mod value;

// use alloc::*;
pub use executor::*;
use fxhash::FxHashMap as HashMap;
pub use monoruby_attr::monoruby_builtin;
pub type IndexMap<K, V> = indexmap::IndexMap<K, V, fxhash::FxBuildHasher>;
pub type IndexSet<T> = indexmap::IndexSet<T>;
pub use executor::bytecodegen::BytecodeGen;
use id_table::*;
use value::*;

pub use value::rvalue::RegexpInner;

pub const STRING_INLINE_CAP: usize = 39;
