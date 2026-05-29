//! aarch64 VM backend (in progress).
//!
//! This is the in-crate counterpart of the x86-64 VM tier
//! (`vmgen`/`invoker`/`wrapper` + the asm methods in `jit_module.rs` and
//! `codegen.rs`), which is gated to `target_arch = "x86_64"`. It transcribes
//! the x86 VM-tier shapes (dispatch core, slot access, runtime calls,
//! conditional branches, guarded tagged-fixnum arithmetic, and the
//! method-call frame model) into monoruby's `JitModule`/`Codegen` structure.
//!
//! Global-register mapping (x86-64 → aarch64), mirroring `CLAUDE.md`:
//!
//! | role               | x86 | aarch64 |
//! |--------------------|-----|---------|
//! | `&mut Executor`    | rbx | x19     |
//! | `&mut Globals`     | r12 | x20     |
//! | program counter    | r13 | x21     |
//! | local frame ptr    | r14 | x22     |
//! | accumulator        | r15 | x23     |
//!
//! `x19..x23` are callee-saved under AAPCS64 and are saved/restored in the VM
//! entry prologue/epilogue (like the x86 `pushq rbp` / `leave`).
//!
//! See `doc/aarch64-vmgen-plan.md` for the porting plan and milestones.
//!
//! The backend is split into a mirror of `arch/x86_64`: `jit_module`
//! (dispatch/frame primitives, stubs, `new`, signal stub), `invoker`
//! (method/block/fiber invokers, `get_class`), `codegen` (entry/GC/stack
//! asm helpers), `vmgen` (`construct_vm` + every bytecode opcode handler +
//! BOP de-optimization), and `wrapper` (`gen_wrapper` + accessor/native
//! wrappers).

use super::*;

// ---- Global registers (mirror the x86 assignment) ----
pub(in crate::codegen) const EXEC: GReg = X19; // &mut Executor   (x86 rbx)
pub(in crate::codegen) const GLOBALS: GReg = X20; // &mut Globals  (x86 r12)
pub(in crate::codegen) const PC: GReg = X21; // program counter   (x86 r13)
pub(in crate::codegen) const LFP: GReg = X22; // local frame ptr  (x86 r14)
pub(in crate::codegen) const ACC: GReg = X23; // accumulator      (x86 r15)
// Caller-saved scratch (x9..x15).
pub(in crate::codegen) const TBL: GReg = X9;
pub(in crate::codegen) const OP: GReg = X10;
pub(in crate::codegen) const TGT: GReg = X11;
pub(in crate::codegen) const TMP: GReg = X12;
pub(in crate::codegen) const TMP2: GReg = X13;

// A scratch register used only as 16-byte-alignment padding in the prologue
// pair stores (x24 is callee-saved but unused by the VM globals).
pub(in crate::codegen) const X19_PAD: GReg = X24;

mod codegen;
mod invoker;
mod jit_module;
mod vmgen;
mod wrapper;
