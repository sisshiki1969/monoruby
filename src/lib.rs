#![feature(box_patterns)]
#![feature(int_roundings)]
#![feature(const_option)]
#![feature(once_cell)]
#![allow(clippy::too_many_arguments)]
#![feature(iter_next_chunk)]
mod alloc;
mod executor;
mod id_table;
#[cfg(test)]
mod tests;
mod value;

use alloc::*;
pub use executor::*;
use fxhash::FxHashMap as HashMap;
use id_table::*;
pub type IndexMap<K, V> = indexmap::IndexMap<K, V, fxhash::FxBuildHasher>;
use monoasm::CodePtr;
use value::*;

pub use value::rvalue::regexp::RegexpInfo;

pub const STRING_INLINE_CAP: usize = 39;
