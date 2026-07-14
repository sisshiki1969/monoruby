#![feature(box_patterns)]
#![allow(clippy::too_many_arguments)]
#![feature(iter_next_chunk)]
#![feature(step_trait)]
#![feature(coverage_attribute)]
// The aarch64 backend carries the JIT subsystem's pure-Rust support code
// (threshold constants, FP-register allocation types, JIT runtime arg-handling
// helpers, etc.) but not all of the x86-only asm-emitting parts, leaving some
// of these helpers unreferenced there. Allow that here rather than
// arch-gating each one individually.
#![cfg_attr(target_arch = "aarch64", allow(dead_code, unused_mut))]

/// Wrap a JIT inline-method generator for the `define_builtin_inline_*`
/// registrars. On x86-64 it boxes the generator. The inline asm of these
/// generators has not been ported to aarch64, so there it registers the
/// universal `noinline_gen` instead — it declines to inline, falling back to a
/// normal method call, without ever naming `$f`.
#[cfg(target_arch = "x86_64")]
macro_rules! inline_gen {
    ($f:expr) => {
        Box::new($f)
    };
}
#[cfg(target_arch = "aarch64")]
macro_rules! inline_gen {
    ($f:expr) => {
        Box::new(crate::globals::noinline_gen)
    };
}

/// Like [`inline_gen!`], but for generators whose inline asm has been ported to
/// every arch (`$f` exists and is valid on both x86 and aarch64). Use this when
/// registering a builtin with arch-switched asm; `inline_gen!` stays for the
/// x86-only ones.
macro_rules! inline_gen2 {
    ($f:expr) => {
        Box::new($f)
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
#[cfg(target_arch = "x86_64")]
pub(crate) use codegen::jitgen::JitContext;
pub use executor::Executor;
pub use executor::frame_leak_stats;
pub use executor::install_panic_hook;
pub use executor::terminate_with_signal;
pub use globals::OBJECT_CLASS;
pub use globals::set_cli_flag_gvars;
pub use globals::{load_file, read_source_file};
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
