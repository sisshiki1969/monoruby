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
#![feature(ptr_internals)]
mod alloc;
mod builtins;
mod bytecodegen;
mod executor;
mod globals;
mod id_table;
#[cfg(test)]
mod tests;
mod value;

pub use bytecodegen::compile_script;
pub use executor::Executor;
pub use globals::OBJECT_CLASS;
pub use globals::{Globals, MonorubyErr};
pub use value::Value;

use executor::jitgen::analysis;
use executor::*;
use fxhash::FxHashMap as HashMap;
use fxhash::FxHashSet as HashSet;
use globals::*;
use id_table::*;
use monoruby_attr::*;
use value::rvalue::RegexpInner;
use value::*;

const STRING_INLINE_CAP: usize = 39;

type IndexMap<K, V> = indexmap::IndexMap<K, V, fxhash::FxBuildHasher>;
type IndexSet<T> = indexmap::IndexSet<T>;
