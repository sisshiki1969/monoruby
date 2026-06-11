//! Architecture switch for the VM-tier backend.
//!
//! monoruby's bytecode VM, method/block/fiber invokers, native-function
//! wrappers, and the small asm helpers used by both the VM and the JIT are
//! written in raw machine code via `monoasm`. Each supported architecture
//! provides its own implementation of these as inherent `impl Codegen` /
//! `impl JitModule` methods; the `Codegen` / `JitModule` types themselves
//! (and everything else in `crate::codegen`) are arch-neutral.
//!
//! The two backends have a mirrored file layout:
//!
//! ```text
//! arch/
//! ├── x86_64/   {codegen, jit_module, invoker, wrapper, vmgen}
//! └── aarch64/  {codegen, jit_module, invoker, wrapper, vmgen}
//! ```
//!
//! Exactly one is compiled, chosen by `target_arch`. Both share the
//! arch-neutral JIT front-end (`crate::codegen::jitgen`): x86-64 emits full
//! machine code, while aarch64 uses its own AsmIR→A64 lowering that deopts to
//! the VM on not-yet-ported instructions.
use super::*;

#[cfg(target_arch = "x86_64")]
mod x86_64;

#[cfg(target_arch = "aarch64")]
mod aarch64;
