#![feature(box_patterns)]
#![allow(clippy::too_many_arguments)]
#![feature(iter_next_chunk)]
#![feature(step_trait)]
#![feature(coverage_attribute)]
// VM-only builds (aarch64 / `--features no-jit`, i.e. `not(jit)`) still carry
// the JIT subsystem's pure-Rust support code (threshold constants, FP-register
// allocation types, JIT runtime arg-handling helpers, etc.). Its asm-emitting
// parts are `#[cfg(jit)]`-excluded, leaving these helpers unreferenced. Allow
// that here rather than `#[cfg(jit)]`-gating each one individually.
#![cfg_attr(not(jit_emit), allow(dead_code, unused_mut))]

/// Wrap a JIT inline-method generator for the `define_builtin_inline_*`
/// registrars. On JIT builds it boxes the generator; on VM-only builds
/// (`not(jit)`: aarch64 / `--features no-jit`) it expands to `()`, dropping
/// the (JIT-only, `#[cfg(jit)]`) generator fn/closure so it is never named or
/// compiled. The matching `#[cfg(not(jit))]` registrar twins take `()`.
#[cfg(jit_emit)]
macro_rules! inline_gen {
    ($f:expr) => {
        Box::new($f)
    };
}
#[cfg(not(jit_emit))]
macro_rules! inline_gen {
    ($f:expr) => {
        ()
    };
}

mod alloc;
pub mod ast;
mod basic_block;
mod builtins;
mod bytecode;
mod bytecodegen;
mod codegen;
mod executor;
mod globals;
mod id_table;
pub mod parser;
mod ruby_probe;
pub mod tests;
mod value;
mod watchdog;

pub(crate) use crate::codegen::runtime::ProcData;
pub(crate) use bytecode::*;
pub use bytecodegen::bytecode_compile_script;
#[cfg(jit_emit)]
pub(crate) use codegen::jitgen::JitContext;
pub use executor::Executor;
pub use executor::frame_leak_stats;
pub use executor::install_panic_hook;
pub use globals::OBJECT_CLASS;
pub use globals::load_file;
pub use globals::{Globals, MonorubyErr, MonorubyErrKind};
pub use value::*;

use builtins::Arg;
use executor::*;
pub use frame::Lfp;
use fxhash::FxHashMap as HashMap;
use fxhash::FxHashSet as HashSet;
use globals::*;
pub use id_table::{IdentId, IdentName};
use monoruby_attr::*;

const STRING_INLINE_CAP: usize = 32;
const MAX_STACK_SIZE: usize = 64 * 1024; // 256 KiB
const CONTINUATION_FRAME_SIZE: usize = 16;

type RubyMap<K, V> = rubymap::RubyMap<K, V, Executor, Globals, MonorubyErr>;
type RubySet<T> = rubymap::RubySet<T, Executor, Globals, MonorubyErr, fxhash::FxBuildHasher>;
pub use rubymap::{RubyEql, RubyHash, RubySymEql, RubySymHash};
