#![feature(box_patterns)]
#![feature(int_roundings)]
#![feature(const_option)]
#![allow(clippy::too_many_arguments)]
#![feature(iter_next_chunk)]
#![feature(result_flattening)]
#![feature(let_chains)]
#![feature(step_trait)]
#![feature(naked_functions)]
#![feature(iter_array_chunks)]
#![feature(offset_of_nested)]

mod alloc;
mod builtins;
mod bytecodegen;
mod compiler;
mod executor;
mod globals;
mod id_table;
#[cfg(test)]
mod tests;
mod value;

pub use bytecodegen::bytecode_compile_script;
pub use executor::Executor;
pub use globals::OBJECT_CLASS;
pub use globals::{Globals, MonorubyErr};
pub use value::*;

use builtins::Arg;
use compiler::jitgen::analysis;
//use compiler::jitgen::trace_ir::TraceIr;
use executor::*;
pub use frame::Lfp;
use fxhash::FxHashMap as HashMap;
use fxhash::FxHashSet as HashSet;
use globals::*;
pub use id_table::IdentId;
use monoruby_attr::*;

const STRING_INLINE_CAP: usize = 32;

type IndexMap<K, V> = indexmap::IndexMap<K, V, fxhash::FxBuildHasher>;
type IndexSet<T> = indexmap::IndexSet<T>;
