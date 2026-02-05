#![feature(box_patterns)]
#![feature(int_roundings)]
#![allow(clippy::too_many_arguments)]
#![feature(iter_next_chunk)]
#![feature(step_trait)]
#![feature(iter_array_chunks)]

mod alloc;
mod builtins;
mod bytecode;
mod bytecodegen;
mod codegen;
mod executor;
mod globals;
mod id_table;
pub mod tests;
mod value;

pub(crate) use crate::codegen::jitgen::trace_ir::TraceIr;
pub(crate) use crate::codegen::runtime::ProcData;
pub(crate) use bytecode::*;
pub use bytecodegen::bytecode_compile_script;
pub(crate) use codegen::jitgen::JitContext;
pub use executor::Executor;
pub use globals::OBJECT_CLASS;
pub use globals::load_file;
pub use globals::{Globals, MonorubyErr};
pub use value::*;

use builtins::Arg;
use executor::*;
pub use frame::Lfp;
use fxhash::FxHashMap as HashMap;
use fxhash::FxHashSet as HashSet;
use globals::*;
pub use id_table::IdentId;
use monoruby_attr::*;

const STRING_INLINE_CAP: usize = 32;
const MAX_STACK_SIZE: usize = 64 * 1024; // 256 KiB

type RubyMap<K, V> = rubymap::RubyMap<K, V, Executor, Globals, MonorubyErr>;
type RubySet<T> = rubymap::RubySet<T, Executor, Globals, MonorubyErr, fxhash::FxBuildHasher>;
pub use rubymap::{RubyEql, RubyHash, RubySymHash, RubySymEql};
