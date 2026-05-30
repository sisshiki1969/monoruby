//! x86-64 VM-tier backend.
//!
//! Provides the x86-64 implementations of the VM construction / dispatch
//! core (`vmgen`), the method/block/fiber invokers (`invoker`), the native
//! function wrappers (`wrapper`), the `JitModule` frame/GC/signal asm
//! helpers (`jit_module`), and the small `Codegen` asm helpers shared with
//! the JIT (`codegen`). Mirrors `arch/aarch64`.
use super::*;

mod codegen;
mod invoker;
mod jit_module;
mod vmgen;
mod wrapper;
