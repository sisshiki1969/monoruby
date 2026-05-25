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

// ===========================================================================
// Stub-to-link scaffolding.
//
// To make the aarch64 VM-only build *link*, every VM-tier method the shared
// code calls must exist. The methods below are placeholders that emit a `brk`
// trap (or are no-ops) so the crate compiles and `Codegen::new` constructs;
// executing Ruby will trap until the real handlers/invokers/wrappers are
// ported. Each is marked TODO(aarch64). The encoding patterns to fill them in
// are validated in the `aarch64-proto` crate. See doc/aarch64-vmgen-plan.md.
// ===========================================================================

impl JitModule {
    /// Bind `label` at the current position and emit a trap.
    fn a64_brk_stub(&mut self, label: &DestLabel) {
        self.jit.bind_label(label.clone());
        self.jit.brk(0);
    }

    /// Emit a `brk` trampoline and return its address as a typed fn pointer.
    /// TODO(aarch64): replace each caller with the real invoker.
    fn a64_stub_fn<T>(&mut self) -> T {
        let p = self.jit.get_current_address();
        self.jit.brk(0);
        // SAFETY: T is always a pointer-sized `extern "C"` fn pointer; the
        // trampoline traps if ever called (M0 in progress).
        unsafe { std::mem::transmute_copy::<*mut u8, T>(&p.as_ptr()) }
    }

    pub(super) fn new() -> Self {
        let mut jit = JitMemory::new();
        let class_version = jit.data_i32(1);
        let bop_redefined_flags = jit.data_i32(0);
        let const_version = jit.data_i64(1);
        let alloc_flag = jit.data_i32(if cfg!(feature = "gc-stress") { 1 } else { 0 });
        let pending_signals = jit.data_i32(0);
        let entry_raise = jit.label();
        let entry_panic = jit.label();
        let exec_gc = jit.label();
        let f64_to_val = jit.label();
        let stack_overflow = jit.label();

        // TODO(aarch64): emit the real entry stubs (raise/fetch_and_dispatch/
        // panic/f64_to_val/gc). For now trap so the module links + constructs.
        let entry_unimpl = jit.get_current_address();
        jit.brk(0);
        let dispatch = vec![entry_unimpl; 256];
        let mut j = Self {
            jit,
            class_version,
            const_version,
            alloc_flag,
            pending_signals,
            entry_raise,
            exec_gc,
            f64_to_val,
            vm_stack_overflow: stack_overflow,
            entry_panic,
            dispatch: dispatch.into_boxed_slice().try_into().unwrap(),
            bop_redefined_flags,
        };
        let raise = j.entry_raise.clone();
        let panic = j.entry_panic.clone();
        let gc = j.exec_gc.clone();
        let f64v = j.f64_to_val.clone();
        let ovf = j.vm_stack_overflow.clone();
        j.a64_brk_stub(&raise);
        j.a64_brk_stub(&panic);
        j.a64_brk_stub(&gc);
        j.a64_brk_stub(&f64v);
        j.a64_brk_stub(&ovf);
        j.jit.finalize();
        j
    }

    // --- invokers / entry points (stubs) ---
    pub(super) fn method_invoker(&mut self) -> MethodInvoker {
        self.a64_stub_fn()
    }
    pub(super) fn method_invoker2(&mut self) -> MethodInvoker2 {
        self.a64_stub_fn()
    }
    pub(super) fn block_invoker(&mut self) -> BlockInvoker {
        self.a64_stub_fn()
    }
    pub(super) fn block_invoker_with_self(&mut self) -> BlockInvoker {
        self.a64_stub_fn()
    }
    pub(super) fn binding_invoker(&mut self) -> BindingInvoker {
        self.a64_stub_fn()
    }
    pub(super) fn fiber_invoker(&mut self) -> FiberInvoker {
        self.a64_stub_fn()
    }
    pub(super) fn fiber_invoker_with_self(&mut self) -> FiberInvoker {
        self.a64_stub_fn()
    }
    pub(super) fn resume_fiber(
        &mut self,
    ) -> extern "C" fn(*mut Executor, &mut Executor, Value) -> Option<Value> {
        self.a64_stub_fn()
    }
    pub(super) fn yield_fiber(&mut self) -> extern "C" fn(*mut Executor, Value) -> Option<Value> {
        self.a64_stub_fn()
    }
    pub(super) fn init_stack_limit(&mut self) -> extern "C" fn(&mut Executor) -> *const u8 {
        self.a64_stub_fn()
    }
    pub(super) fn get_class(&mut self) -> DestLabel {
        let l = self.jit.label();
        self.a64_brk_stub(&l);
        l
    }

    pub(crate) fn signal_handler_for(
        &mut self,
        _alloc_flag: DestLabel,
        _pending_signals: DestLabel,
        _signo: i32,
    ) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.brk(0);
        p
    }
}

impl Codegen {
    /// VM dispatch loop + opcode handlers. Real handlers so far: `immediate`
    /// (op 6, integer/immediate literal → slot) and `ret` (op 80). All other
    /// opcodes fall through to the `entry_unimpl` trap until ported. Uses the
    /// qemu-validated patterns from `aarch64-proto`.
    pub(super) fn construct_vm(&mut self) {
        let vm_entry = self.jit.label();
        let entry_fetch = self.jit.label();
        // vm_entry: establish the frame pointer (x86: `pushq rbp; movq rbp,rsp`).
        self.jit.bind_label(vm_entry.clone());
        self.jit.stp_pre(X29, X30, SP, -16);
        self.jit.mov_sp(X29, SP);
        self.jit.bind_label(entry_fetch.clone());
        self.a64_fetch_and_dispatch();
        self.vm_fetch = entry_fetch;
        self.vm_entry = vm_entry;

        let immediate = self.a64_op_immediate();
        let ret = self.a64_op_ret();
        self.dispatch[6] = immediate;
        self.dispatch[80] = ret;
    }

    /// op 6 `immediate`: slot[`[pc+4]`] <- the immediate Value at `[pc+8]`.
    /// (x86 `vm_immediate`: `fetch_r15; movq rax,[r13-8]; vm_store_r15`.)
    fn a64_op_immediate(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        self.jit.ldrh(X10, PC, 4); // dst slot index
        self.jit.ldr(X11, PC, 8); // immediate value
        self.jit.cbz_label(X10, &skip); // slot 0 => discard
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X12, LFP, X10, 3);
        self.jit.sub_imm(X12, X12, LFP_SELF as u32, 0);
        self.jit.str(X11, X12, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 80 `ret`: return slot[`[pc+4]`]'s value (x86 `fetch_addr_r15;
    /// movq rax,[r15]; epilogue`).
    fn a64_op_ret(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.ldrh(X10, PC, 4); // slot index
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X11, LFP, X10, 3);
        self.jit.sub_imm(X11, X11, LFP_SELF as u32, 0);
        self.jit.ldr(X0, X11, 0); // return value
        // epilogue (x86 `leave; ret`): restore the frame pointer and return.
        self.jit.mov_sp(SP, X29);
        self.jit.ldp_post(X29, X30, SP, 16);
        self.jit.ret();
        p
    }

    /// TODO(aarch64): port the real per-method wrapper (see wrapper.rs).
    pub(crate) fn gen_wrapper(&mut self, _globals: &Globals, _fid: FuncId) -> DestLabel {
        let entry = self.jit.label();
        self.a64_brk_stub(&entry);
        entry
    }

    /// TODO(aarch64): the VM BOP fast-path optimization is not emitted yet, so
    /// there is nothing to remove.
    pub(super) fn remove_vm_bop_optimization(&mut self) {}
}
