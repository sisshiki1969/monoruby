//! aarch64 VM backend (in progress).
//!
//! This is the in-crate counterpart of the x86-64 VM tier
//! (`vmgen`/`invoker`/`wrapper` + the asm methods in `jit_module.rs` and
//! `codegen.rs`), which is gated to `target_arch = "x86_64"`. The encoding
//! patterns used here are validated under qemu by the standalone
//! `aarch64-proto` crate (dispatch core, slot access, runtime calls,
//! conditional branches, guarded tagged-fixnum arithmetic, and the
//! method-call frame model); this module transcribes those known-good shapes
//! into monoruby's `JitModule`/`Codegen` structure.
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
//! Status: foundational helpers are transcribed below. The remaining VM-tier
//! surface — `JitModule::new`/`init`, `construct_vm` + the ~150 opcode
//! handlers, the method/block/fiber invokers, and `gen_wrapper` — is still to
//! be ported, so the aarch64 build does not yet link. See
//! `doc/aarch64-vmgen-plan.md`.
#![allow(dead_code)]

use super::*;
use monoasm::*;

// ---- Global registers (mirror the x86 assignment) ----
pub(super) const EXEC: GReg = X19; // &mut Executor   (x86 rbx)
pub(super) const GLOBALS: GReg = X20; // &mut Globals  (x86 r12)
pub(super) const PC: GReg = X21; // program counter   (x86 r13)
pub(super) const LFP: GReg = X22; // local frame ptr  (x86 r14)
pub(super) const ACC: GReg = X23; // accumulator      (x86 r15)
// Caller-saved scratch (x9..x15).
pub(super) const TBL: GReg = X9;
pub(super) const OP: GReg = X10;
pub(super) const TGT: GReg = X11;
pub(super) const TMP: GReg = X12;
pub(super) const TMP2: GReg = X13;

impl JitModule {
    /// Fetch the opcode and dispatch through the 256-entry table.
    ///
    /// x86: `movq r15,(table); movzxb rax,[r13+OPECODE]; addq r13,16;
    /// jmp [r15+rax*8]`. On aarch64 the pc is *not* pre-advanced (A64 has no
    /// LDUR-style negative-displacement scaled load in the builder yet), so
    /// handlers read operands at positive offsets and advance the pc
    /// themselves before re-dispatching. Validated as `dispatch_*` in
    /// `aarch64-proto`.
    ///
    /// ### in
    /// - x21 (PC): bytecode pointer
    /// ### destroy
    /// - x9 (TBL), x10 (OP), x11 (TGT)
    pub(super) fn a64_fetch_and_dispatch(&mut self) {
        let table = self.dispatch.as_ptr() as u64;
        self.jit.mov_imm(TBL, table); // table base
        self.jit.ldrb(OP, PC, OPECODE as u32); // opcode <- [pc + OPECODE]
        self.jit.ldr_reg(TGT, TBL, OP, true); // handler <- table[opcode] (lsl #3)
        self.jit.br(TGT);
    }

    /// Address of the local slot whose index is in `reg`:
    /// `[r14 + reg*8 - LFP_SELF]`. x86: `negq R(r); lea R(r),[r14+R(r)*8-SELF]`.
    /// aarch64: `neg; add Xr,x_lfp,Xr,lsl #3; sub #LFP_SELF`. Validated as
    /// `slot_access_idiom` in `aarch64-proto`.
    ///
    /// ### in / out
    /// - `dst`: slot index in → slot *address* out
    pub(super) fn a64_slot_addr(&mut self, dst: GReg) {
        self.jit.neg(dst, dst);
        self.jit.add_lsl(dst, LFP, dst, 3);
        self.jit.sub_imm(dst, dst, LFP_SELF as u32, 0);
    }

    /// Value of the local slot whose index is in `reg` (slot index → value).
    pub(super) fn a64_slot_value(&mut self, dst: GReg) {
        self.a64_slot_addr(dst);
        self.jit.ldr(dst, dst, 0);
    }

    /// VM entry prologue: save the callee-saved global registers + fp/lr.
    /// x86 analogue: `pushq rbp; movq rbp, rsp`.
    pub(super) fn a64_prologue(&mut self) {
        self.jit.stp_pre(X29, X30, SP, -16); // fp, lr
        self.jit.stp_pre(EXEC, GLOBALS, SP, -16);
        self.jit.stp_pre(PC, LFP, SP, -16);
        self.jit.stp_pre(ACC, X19_PAD, SP, -16); // ACC + 8-byte pad (16-align)
    }

    /// VM exit epilogue: restore the callee-saved registers. x86: `leave; ret`.
    pub(super) fn a64_epilogue(&mut self) {
        self.jit.ldp_post(ACC, X19_PAD, SP, 16);
        self.jit.ldp_post(PC, LFP, SP, 16);
        self.jit.ldp_post(EXEC, GLOBALS, SP, 16);
        self.jit.ldp_post(X29, X30, SP, 16);
        self.jit.ret();
    }
}

// A scratch register used only as 16-byte-alignment padding in the prologue
// pair stores (x24 is callee-saved but unused by the VM globals).
const X19_PAD: GReg = X24;
