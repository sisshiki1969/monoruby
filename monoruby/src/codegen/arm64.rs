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

    /// Like `a64_brk_stub` but reports a diagnostic id before trapping.
    fn a64_brk_stub_diag(&mut self, label: &DestLabel, id: u64) {
        self.jit.bind_label(label.clone());
        self.jit.mov_imm(X0, id);
        self.jit
            .mov_imm(X9, crate::codegen::runtime::report_unimpl_op as u64);
        self.jit.blr(X9);
        self.jit.brk(0);
    }

    /// Emit a `brk` trampoline and return its address as a typed fn pointer.
    /// TODO(aarch64): replace each caller with the real invoker.
    fn a64_stub_fn<T>(&mut self, id: u64) -> T {
        let p = self.jit.get_current_address();
        self.jit.mov_imm(X0, 0x51410000 + id); // DIAG: stub invoker hit
        self.jit
            .mov_imm(X9, crate::codegen::runtime::report_unimpl_op as u64);
        self.jit.blr(X9);
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
        jit.mov(X0, OP); // OP (X10) holds the opcode at dispatch time
        jit.mov_imm(X9, crate::codegen::runtime::report_unimpl_op as u64);
        jit.blr(X9);
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
        // entry_raise is emitted later in construct_vm (it needs
        // fetch_and_dispatch); the rest stay stubs for now.
        let _ = &raise;
        j.a64_brk_stub_diag(&panic, 0xffff02); // entry_panic
        j.a64_brk_stub_diag(&gc, 0xffff03); // exec_gc
        j.a64_brk_stub_diag(&f64v, 0xffff04); // f64_to_val
        j.a64_brk_stub_diag(&ovf, 0xffff05); // vm_stack_overflow
        j.jit.finalize();
        j
    }

    // --- invokers / entry points ---
    /// Real `method_invoker` for the simple case (0 args, no kw): set up the
    /// Ruby frame and call into `vm_entry`, then return the result to Rust.
    /// AAPCS64 in: x0 exec, x1 globals, x2 funcid, x3 self, x4 args, x5 len,
    /// x6 block, x7 hashmap. The frame offsets match x86 because aarch64's
    /// `stp fp,lr` (16B at vm_entry) equals x86's `call`(8B)+`push rbp`(8B).
    /// TODO(aarch64): argument copying (assumes 0 args) + stack-overflow check.
    /// Invoker prologue: save callee-saved globals + fp/lr + X25/X26, then set
    /// EXEC=x0, GLOBALS=x1.
    fn a64_invoker_prologue(&mut self) {
        self.jit.stp_pre(X29, X30, SP, -16);
        self.jit.stp_pre(EXEC, GLOBALS, SP, -16);
        self.jit.stp_pre(PC, LFP, SP, -16);
        self.jit.stp_pre(ACC, X24, SP, -16);
        self.jit.stp_pre(X25, X26, SP, -16);
        self.jit.mov(EXEC, X0);
        self.jit.mov(GLOBALS, X1);
    }

    /// fdata(X9) = funcinfo_base + (X2 = funcid)*64 + FUNCINFO_DATA.
    fn a64_get_func_data_x2(&mut self) {
        self.jit.lsl_imm(X10, X2, 32); // zero-extend funcid
        self.jit.lsr_imm(X10, X10, 32);
        self.jit.lsl_imm(X10, X10, 6); // * size_of::<FuncInfo>() (64)
        self.jit.mov_imm(X11, GLOBALS_FUNCINFO as u64);
        self.jit.add(X11, GLOBALS, X11);
        self.jit.ldr(X11, X11, 0);
        self.jit.add(X10, X10, X11);
        self.jit.add_imm(X9, X10, FUNCINFO_DATA as u32, 0);
    }

    /// Shared invoker tail (after the frame self/block/outer/meta are set):
    /// argument setup (simple copy or runtime arg massager), call into the
    /// callee, then the epilogue. Assumes fdata in X9, meta in X14, x4 = args
    /// ptr, x5 = len, x7 = kw (Option<Hashmap>).
    fn a64_invoker_args_and_call(&mut self) {
        let fdata = X9;
        let generic = self.jit.label();
        let simple_done = self.jit.label();
        let argloop = self.jit.label();
        let aftargs = self.jit.label();
        let error_exit = self.jit.label();
        // Simple iff callee is simple (Meta kind bit 4) AND len == min AND
        // no keyword args; otherwise fall back to the runtime arg massager.
        self.jit.lsr_imm(X10, X14, 56); // kind byte
        self.jit.tbz_label(X10, 4, &generic);
        self.jit.ldrh(X10, fdata, FUNCDATA_MIN as u32);
        self.jit.cmp(X5, X10);
        self.jit.bcond_label(Cond::Ne, &generic);
        self.jit.cbnz_label(X7, &generic); // kw present
        // simple copy (upward): args[0..len] -> callee slots 1..=len
        self.jit.cbz_label(X5, &simple_done);
        self.jit.mov(X10, X5); // down counter = len
        self.jit.neg(X11, X5); // up counter = -len
        self.jit.sub_imm(X12, X4, 8, 0); // args - 8
        self.jit.bind_label(argloop.clone());
        self.jit.add_lsl(X13, X12, X10, 3); // &args[down-1]
        self.jit.ldr(X14, X13, 0);
        self.jit.sub_imm(X15, SP, (RSP_LOCAL_FRAME + LFP_SELF) as u32, 0);
        self.jit.add_lsl(X15, X15, X11, 3);
        self.jit.str(X14, X15, 0);
        self.jit.sub_imm(X10, X10, 1, 0);
        self.jit.add_imm(X11, X11, 1, 0);
        self.jit.cbnz_label(X11, &argloop);
        self.jit.bind_label(simple_done);
        self.jit.b_label(&aftargs);
        // generic: handle_invoker_arguments(exec, globals, callee_lfp,
        // arg_num, args, kw). Reserve scratch below the callee frame and
        // preserve SP (X25) + funcdata (X26, caller-saved) across the call.
        self.jit.bind_label(generic);
        self.jit.mov_sp(X25, SP);
        self.jit.mov(X26, fdata);
        self.jit.ldrh(X10, fdata, FUNCDATA_OFS as u32);
        self.jit.lsl_imm(X10, X10, 4);
        self.jit.add_imm(X10, X10, 16, 0);
        self.jit.sub(X11, X25, X10);
        self.jit.mov_sp(SP, X11);
        self.jit.sub_imm(X2, X25, RSP_LOCAL_FRAME as u32, 0); // callee_lfp
        self.jit.mov(X3, X5); // arg_num = len
        self.jit.mov(X5, X7); // kw
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        // x4 = args (unchanged); upward path => handle_invoker_arguments
        self.jit.mov_imm(X9, runtime::handle_invoker_arguments as u64);
        self.jit.blr(X9);
        self.jit.mov(X9, X26); // restore fdata
        self.jit.mov_sp(SP, X25); // restore SP
        self.jit.cbz_label(X0, &error_exit);
        self.jit.bind_label(aftargs);
        // call_invoker: push_frame
        self.jit.ldr(X10, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(X11, SP, RSP_CFP as u32, 0);
        self.jit.str(X10, X11, 0); // [new_cfp] = prev cfp
        self.jit.str(X11, EXEC, EXECUTOR_CFP as u32); // exec.cfp = new cfp
        // set_lfp
        self.jit.sub_imm(LFP, SP, RSP_LOCAL_FRAME as u32, 0);
        self.jit.sub_imm(X10, SP, (RSP_CFP + CFP_LFP) as u32, 0);
        self.jit.str(LFP, X10, 0);
        // pc = funcdata.pc; call funcdata.codeptr
        self.jit.ldr(PC, fdata, FUNCDATA_PC as u32);
        self.jit.ldr(X10, fdata, FUNCDATA_CODEPTR as u32);
        self.jit.blr(X10);
        // restore exec.cfp = [sp - RSP_CFP] (the prev cfp saved above)
        self.jit.sub_imm(X11, SP, RSP_CFP as u32, 0);
        self.jit.ldr(X10, X11, 0);
        self.jit.str(X10, EXEC, EXECUTOR_CFP as u32);
        // Converge with the result (or 0 on error) in x0; the caller emits
        // its own epilogue (standard restore for method/block invokers, or a
        // fiber stack-switch-back for the fiber invoker).
        self.jit.bind_label(error_exit);
    }

    /// Standard invoker epilogue: restore the callee-saved registers saved by
    /// a64_invoker_prologue and return (result in x0).
    fn a64_invoker_epilogue(&mut self) {
        self.jit.ldp_post(X25, X26, SP, 16);
        self.jit.ldp_post(ACC, X24, SP, 16);
        self.jit.ldp_post(PC, LFP, SP, 16);
        self.jit.ldp_post(EXEC, GLOBALS, SP, 16);
        self.jit.ldp_post(X29, X30, SP, 16);
        self.jit.ret();
    }

    /// Save all callee-saved GPRs (x19-x28) + fp/lr for a fiber context switch.
    fn a64_push_callee_save(&mut self) {
        self.jit.stp_pre(X29, X30, SP, -16);
        self.jit.stp_pre(X27, X28, SP, -16);
        self.jit.stp_pre(X25, X26, SP, -16);
        self.jit.stp_pre(X23, X24, SP, -16);
        self.jit.stp_pre(X21, X22, SP, -16);
        self.jit.stp_pre(X19, X20, SP, -16);
    }

    /// Restore what a64_push_callee_save saved (reverse order).
    fn a64_pop_callee_save(&mut self) {
        self.jit.ldp_post(X19, X20, SP, 16);
        self.jit.ldp_post(X21, X22, SP, 16);
        self.jit.ldp_post(X23, X24, SP, 16);
        self.jit.ldp_post(X25, X26, SP, 16);
        self.jit.ldp_post(X27, X28, SP, 16);
        self.jit.ldp_post(X29, X30, SP, 16);
    }

    pub(super) fn method_invoker(&mut self) -> MethodInvoker {
        let codeptr = self.jit.get_current_address();
        let fdata = X9;
        // AAPCS64 in: x0 exec, x1 globals, x2 funcid, x3 self, x4 args,
        // x5 len, x6 block, x7 kw (Option<Hashmap>).
        self.a64_invoker_prologue();
        self.a64_get_func_data_x2();
        // frame setup (method): FB = sp - (RSP_LOCAL_FRAME + LFP_SELF).
        let fb = X12;
        self.jit.sub_imm(fb, SP, (RSP_LOCAL_FRAME + LFP_SELF) as u32, 0);
        self.jit.mov_imm(X13, 0);
        self.jit.str(X3, fb, 0); // LFP_SELF  = self
        self.jit.str(X6, fb, 8); // LFP_BLOCK = block
        self.jit.str(X13, fb, 16); // LFP_CME   = 0
        self.jit.str(X13, fb, 24); // LFP_SVAR  = 0
        self.jit.ldr(X14, fdata, FUNCDATA_META as u32);
        self.jit.str(X14, fb, 32); // LFP_META  = funcdata.meta
        self.jit.str(X13, fb, 40); // LFP_OUTER = 0
        self.a64_invoker_args_and_call();
        self.a64_invoker_epilogue();
        // SAFETY: codeptr points at an extern "C" fn with the MethodInvoker ABI.
        unsafe { std::mem::transmute_copy::<*mut u8, MethodInvoker>(&codeptr.as_ptr()) }
    }

    pub(super) fn method_invoker2(&mut self) -> MethodInvoker2 {
        self.a64_stub_fn(0x2)
    }

    /// Block invoker. AAPCS64 in: x0 exec, x1 globals, x2 &ProcData,
    /// x3 self (dummy unless `with_self`), x4 args, x5 len, x6 kw.
    /// `self` for a plain block comes from the captured outer frame.
    /// TODO(aarch64): resolve_invalidated_outer (heap-promoted outer frames).
    /// Set up the callee block frame from a `&ProcData` in x2 (self in x3 when
    /// `with_self`). Produces fdata in X9 and meta in X14, and writes self /
    /// outer / block / cme / svar / meta into the new frame. Shared by the
    /// block and fiber invokers.
    /// TODO(aarch64): resolve_invalidated_outer (heap-promoted outer frames).
    fn a64_block_frame_setup(&mut self, with_self: bool) {
        let fdata = X9;
        if with_self {
            // store the provided self now (frame slot survives get_func_data)
            self.jit.sub_imm(X10, SP, (RSP_LOCAL_FRAME + LFP_SELF) as u32, 0);
            self.jit.str(X3, X10, 0);
        }
        // outer = [&ProcData + PROCDATA_OUTER]; func_id = [+ PROCDATA_FUNCID]
        self.jit.ldr(X3, X2, runtime::PROCDATA_OUTER as u32); // X3 = outer lfp
        self.jit.ldr32(X2, X2, runtime::PROCDATA_FUNCID as u32); // X2 = func_id
        self.a64_get_func_data_x2(); // fdata = X9 (clobbers X10, X11)
        let fb = X12;
        self.jit.sub_imm(fb, SP, (RSP_LOCAL_FRAME + LFP_SELF) as u32, 0);
        self.jit.mov_imm(X13, 0);
        if !with_self {
            // self = outer.self = [outer - LFP_SELF]
            self.jit.sub_imm(X10, X3, LFP_SELF as u32, 0);
            self.jit.ldr(X10, X10, 0);
            self.jit.str(X10, fb, 0); // LFP_SELF
        }
        self.jit.str(X13, fb, 8); // LFP_BLOCK = 0
        self.jit.str(X13, fb, 16); // LFP_CME = 0
        self.jit.str(X13, fb, 24); // LFP_SVAR = 0
        self.jit.ldr(X14, fdata, FUNCDATA_META as u32);
        self.jit.str(X14, fb, 32); // LFP_META
        self.jit.str(X3, fb, 40); // LFP_OUTER = outer
    }

    fn a64_block_invoker(&mut self, with_self: bool) -> BlockInvoker {
        let codeptr = self.jit.get_current_address();
        self.a64_invoker_prologue();
        self.a64_block_frame_setup(with_self);
        self.jit.mov(X7, X6); // shared arg setup expects kw in x7
        self.a64_invoker_args_and_call();
        self.a64_invoker_epilogue();
        // SAFETY: codeptr points at an extern "C" fn with the BlockInvoker ABI.
        unsafe { std::mem::transmute_copy::<*mut u8, BlockInvoker>(&codeptr.as_ptr()) }
    }

    pub(super) fn block_invoker(&mut self) -> BlockInvoker {
        self.a64_block_invoker(false)
    }
    pub(super) fn block_invoker_with_self(&mut self) -> BlockInvoker {
        self.a64_block_invoker(true)
    }

    /// Fiber invoker (first resume). AAPCS64 in: x0 vm, x1 globals,
    /// x2 &ProcData, x3 self, x4 args, x5 len, x6 child_vm (&mut Executor).
    /// Switch onto the fiber's pre-allocated stack, run its block body, then
    /// switch back to the parent on completion. (Mirrors x86 fiber_invoker.)
    fn a64_fiber_invoker(&mut self, with_self: bool) -> FiberInvoker {
        let codeptr = self.jit.get_current_address();
        // switch in: save parent regs + SP, jump to the fiber's stack.
        self.a64_push_callee_save();
        self.jit.mov_sp(X10, SP);
        self.jit.str(X10, X0, EXECUTOR_RSP_SAVE as u32); // parent.rsp_save = SP
        self.jit.ldr(X10, X6, EXECUTOR_RSP_SAVE as u32);
        self.jit.mov_sp(SP, X10); // SP = child.rsp_save (fresh stack top)
        self.jit.str(X0, X6, EXECUTOR_PARENT_FIBER as u32); // child.parent_fiber = parent
        self.jit.mov(EXEC, X6); // EXEC = child_vm
        self.jit.mov(GLOBALS, X1);
        self.a64_block_frame_setup(with_self);
        self.jit.mov_imm(X7, 0); // fibers carry no keyword args
        self.a64_invoker_args_and_call();
        // body completed: mark this fiber terminated, switch back to parent.
        self.jit.mov_imm(X10, u64::MAX); // -1 = terminated
        self.jit.str(X10, EXEC, EXECUTOR_RSP_SAVE as u32);
        self.jit.ldr(EXEC, EXEC, EXECUTOR_PARENT_FIBER as u32);
        self.jit.ldr(X10, EXEC, EXECUTOR_RSP_SAVE as u32);
        self.jit.mov_sp(SP, X10);
        self.a64_pop_callee_save();
        self.jit.ret();
        // SAFETY: codeptr is an extern "C" fn with the FiberInvoker ABI.
        unsafe { std::mem::transmute_copy::<*mut u8, FiberInvoker>(&codeptr.as_ptr()) }
    }
    /// Binding invoker: run a func body reusing a captured frame as its LFP
    /// (no new local frame, no args). AAPCS64 in: x0 exec, x1 globals, x2 lfp.
    pub(super) fn binding_invoker(&mut self) -> BindingInvoker {
        let codeptr = self.jit.get_current_address();
        self.a64_invoker_prologue();
        self.jit.mov(LFP, X2); // reuse the binding's frame as LFP
        self.jit.sub_imm(X2, LFP, LFP_FUNCID as u32, 0);
        self.jit.ldr32(X2, X2, 0); // funcid = [lfp - LFP_FUNCID]
        self.a64_get_func_data_x2(); // X9 = fdata
        // push_frame; cfp.lfp = LFP (the captured frame)
        self.jit.ldr(X10, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(X11, SP, RSP_CFP as u32, 0);
        self.jit.str(X10, X11, 0);
        self.jit.str(X11, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(X10, SP, (RSP_CFP + CFP_LFP) as u32, 0);
        self.jit.str(LFP, X10, 0);
        self.jit.ldr(PC, X9, FUNCDATA_PC as u32);
        self.jit.ldr(X10, X9, FUNCDATA_CODEPTR as u32);
        self.jit.blr(X10);
        self.jit.sub_imm(X11, SP, RSP_CFP as u32, 0);
        self.jit.ldr(X10, X11, 0);
        self.jit.str(X10, EXEC, EXECUTOR_CFP as u32);
        // epilogue
        self.jit.ldp_post(X25, X26, SP, 16);
        self.jit.ldp_post(ACC, X24, SP, 16);
        self.jit.ldp_post(PC, LFP, SP, 16);
        self.jit.ldp_post(EXEC, GLOBALS, SP, 16);
        self.jit.ldp_post(X29, X30, SP, 16);
        self.jit.ret();
        // SAFETY: codeptr is an extern "C" fn with the BindingInvoker ABI.
        unsafe { std::mem::transmute_copy::<*mut u8, BindingInvoker>(&codeptr.as_ptr()) }
    }
    pub(super) fn fiber_invoker(&mut self) -> FiberInvoker {
        self.a64_fiber_invoker(false)
    }
    pub(super) fn fiber_invoker_with_self(&mut self) -> FiberInvoker {
        self.a64_fiber_invoker(true)
    }

    /// Resume a suspended fiber. In: x0 parent_vm, x1 child_vm, x2 value.
    /// Save the parent context, switch onto the child's saved stack, and
    /// return (the child resumes where it last yielded). x86 resume_fiber.
    pub(super) fn resume_fiber(
        &mut self,
    ) -> extern "C" fn(*mut Executor, &mut Executor, Value) -> Option<Value> {
        let codeptr = self.jit.get_current_address();
        self.a64_push_callee_save();
        self.jit.mov_sp(X10, SP);
        self.jit.str(X10, X0, EXECUTOR_RSP_SAVE as u32); // parent.rsp_save = SP
        self.jit.ldr(X10, X1, EXECUTOR_RSP_SAVE as u32);
        self.jit.mov_sp(SP, X10); // SP = child.rsp_save
        self.jit.str(X0, X1, EXECUTOR_PARENT_FIBER as u32); // child.parent_fiber = parent
        self.a64_pop_callee_save();
        self.jit.mov(X0, X2); // resume value
        self.jit.ret();
        // SAFETY: codeptr matches the resume_fiber ABI.
        unsafe { std::mem::transmute_copy(&codeptr.as_ptr()) }
    }

    /// Yield from a fiber. In: x0 vm (child), x1 value. Save the child
    /// context and switch back to the parent. x86 yield_fiber.
    pub(super) fn yield_fiber(&mut self) -> extern "C" fn(*mut Executor, Value) -> Option<Value> {
        let codeptr = self.jit.get_current_address();
        self.a64_push_callee_save();
        self.jit.mov_sp(X10, SP);
        self.jit.str(X10, X0, EXECUTOR_RSP_SAVE as u32); // child.rsp_save = SP
        self.jit.ldr(X0, X0, EXECUTOR_PARENT_FIBER as u32); // vm = parent
        self.jit.ldr(X10, X0, EXECUTOR_RSP_SAVE as u32);
        self.jit.mov_sp(SP, X10); // SP = parent.rsp_save
        self.a64_pop_callee_save();
        self.jit.mov(X0, X1); // yielded value
        self.jit.ret();
        // SAFETY: codeptr matches the yield_fiber ABI.
        unsafe { std::mem::transmute_copy(&codeptr.as_ptr()) }
    }
    pub(super) fn init_stack_limit(&mut self) -> extern "C" fn(&mut Executor) -> *const u8 {
        // executor.stack_limit = sp - MAX_STACK_SIZE (= 65536 = 16 << 12).
        // x0 = &mut Executor (AAPCS64 arg0).
        let codeptr = self.jit.get_current_address();
        self.jit.mov_sp(X10, SP);
        self.jit.sub_imm(X10, X10, 16, 1); // 16 << 12 = 65536
        self.jit.str(X10, X0, EXECUTOR_STACK_LIMIT as u32);
        self.jit.ret();
        // SAFETY: codeptr is an `extern "C" fn(&mut Executor) -> *const u8`.
        unsafe { std::mem::transmute_copy(&codeptr.as_ptr()) }
    }
    /// `get_class`: x0 = Value in → x0 = ClassId (u32) out. Mirrors the x86
    /// `get_class` tag dispatch. (Invalid receivers trap rather than calling
    /// the x86-gated `illegal_classid`.)
    pub(super) fn get_class(&mut self) -> DestLabel {
        let label = self.jit.label();
        let l1 = self.jit.label();
        let err = self.jit.label();
        let fixnum = self.jit.label();
        let flonum = self.jit.label();
        let symbol = self.jit.label();
        let nil = self.jit.label();
        let bool_ = self.jit.label();
        self.jit.bind_label(label.clone());
        self.jit.tbnz_label(X0, 0, &fixnum); // bit0: fixnum
        self.jit.tbnz_label(X0, 1, &flonum); // bit1: flonum
        self.jit.tbnz_label(X0, 2, &l1); // bit2: other immediate
        self.jit.cbz_label(X0, &err); // 0 is invalid
        self.jit.ldr32(X0, X0, RVALUE_OFFSET_CLASS as u32); // heap: RValue.class
        self.jit.ret();
        self.jit.bind_label(l1);
        self.jit.mov_imm(X1, 0xff);
        self.jit.and_(X2, X0, X1);
        self.jit.cmp_imm(X2, TAG_SYMBOL as u32, 0);
        self.jit.bcond_label(Cond::Eq, &symbol);
        self.jit.cmp_imm(X0, NIL_VALUE as u32, 0);
        self.jit.bcond_label(Cond::Eq, &nil);
        self.jit.mov_imm(X1, 8);
        self.jit.orr(X1, X0, X1);
        self.jit.cmp_imm(X1, TRUE_VALUE as u32, 0);
        self.jit.bcond_label(Cond::Eq, &bool_);
        self.jit.bind_label(err);
        self.jit.mov_imm(X0, 0xc1a5); // DIAG: illegal_classid (get_class)
        self.jit
            .mov_imm(X9, crate::codegen::runtime::report_unimpl_op as u64);
        self.jit.blr(X9);
        self.jit.brk(0); // TODO(aarch64): illegal_classid
        self.jit.bind_label(fixnum);
        self.jit.mov_imm(X0, INTEGER_CLASS.u32() as u64);
        self.jit.ret();
        self.jit.bind_label(flonum);
        self.jit.mov_imm(X0, FLOAT_CLASS.u32() as u64);
        self.jit.ret();
        self.jit.bind_label(symbol);
        self.jit.mov_imm(X0, SYMBOL_CLASS.u32() as u64);
        self.jit.ret();
        self.jit.bind_label(nil);
        self.jit.mov_imm(X0, NIL_CLASS.u32() as u64);
        self.jit.ret();
        self.jit.bind_label(bool_);
        self.jit.mov_imm(X0, BOOL_CLASS.u32() as u64);
        self.jit.ret();
        label
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
    /// entry_raise: error/exception dispatch. Calls `handle_error`, which
    /// returns (value, dest): if `dest` is Some, resume execution there (a
    /// rescue/ensure/retry target); otherwise unwind this VM frame and
    /// return the error (None) to the caller (x86 `init`'s `raise:` block).
    fn a64_gen_entry_raise(&mut self) {
        let raise = self.entry_raise.clone();
        let goto = self.jit.label();
        self.jit.bind_label(raise);
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.sub_imm(X2, LFP, LFP_META as u32, 0);
        self.jit.ldr(X2, X2, 0); // meta = [LFP - LFP_META]
        self.jit.mov(X3, PC); // pc = current instruction
        self.jit
            .mov_imm(X9, crate::codegen::jit_module::handle_error as u64);
        self.jit.blr(X9);
        // x0 = value (Option<Value>), x1 = dest (Option<BytecodePtr>)
        self.jit.cbnz_label(X1, &goto);
        // no handler: unwind this frame and return the error (x0 = None)
        self.jit.mov_sp(SP, X29);
        self.jit.ldp_post(X29, X30, SP, 16);
        self.jit.ret();
        self.jit.bind_label(goto);
        self.jit.mov(PC, X1); // resume at the handler pc
        self.a64_fetch_and_dispatch();
    }

    pub(super) fn construct_vm(&mut self) {
        self.a64_gen_entry_raise();
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

        let init_method = self.a64_op_init_method();
        let immediate = self.a64_op_immediate();
        let literal = self.a64_op_literal();
        let mov = self.a64_op_mov();
        let ret = self.a64_op_ret();
        let add_rr = self.a64_op_iadd(false);
        let sub_rr = self.a64_op_iadd(true);
        let mul_rr = self.a64_op_muldiv(mul_values);
        let div_rr = self.a64_op_muldiv(div_values);
        self.dispatch[6] = immediate;
        self.dispatch[7] = literal;
        self.dispatch[178] = mov;
        self.dispatch[80] = ret;
        self.dispatch[172] = init_method;
        self.dispatch[160] = add_rr;
        self.dispatch[161] = sub_rr;
        self.dispatch[162] = mul_rr;
        self.dispatch[163] = div_rr;

        // loop_start (14) / loop_end (15): in the VM-only build these just
        // advance to the next instruction. TODO(aarch64): the GC poll in
        // loop_start (vm_execute_gc) — currently skipped (works with --no-gc).
        let loop_op = self.a64_op_loop();
        self.dispatch[14] = loop_op;
        self.dispatch[15] = loop_op;

        // branches (the shared `branch` target lives inside `br_inst`).
        let (br_inst, branch) = self.a64_op_br();
        let condbr = self.a64_op_condbr(&branch, false);
        let condnotbr = self.a64_op_condbr(&branch, true);
        self.dispatch[3] = br_inst;
        self.dispatch[4] = condbr;
        self.dispatch[5] = condnotbr;
        self.dispatch[12] = condbr;
        self.dispatch[13] = condnotbr;
        let check_local = self.a64_op_check_local(&branch);
        self.dispatch[20] = check_local;
        let nilbr = self.a64_op_nilbr(&branch);
        self.dispatch[37] = nilbr;
        let optcase = self.a64_op_optcase(&branch);
        self.dispatch[36] = optcase;
        let lambda = self.a64_op_lambda();
        self.dispatch[38] = lambda;

        // integer comparisons (fixnum fast path; generic runtime fallback)
        let eq = self.a64_op_cmp(Cond::Eq, cmp_eq_values as u64);
        let ne = self.a64_op_cmp(Cond::Ne, cmp_ne_values as u64);
        let lt = self.a64_op_cmp(Cond::Lt, cmp_lt_values as u64);
        let le = self.a64_op_cmp(Cond::Le, cmp_le_values as u64);
        let gt = self.a64_op_cmp(Cond::Gt, cmp_gt_values as u64);
        let ge = self.a64_op_cmp(Cond::Ge, cmp_ge_values as u64);
        let teq = self.a64_op_cmp(Cond::Eq, cmp_teq_values as u64);
        self.dispatch[140] = eq;
        self.dispatch[141] = ne;
        self.dispatch[142] = lt;
        self.dispatch[143] = le;
        self.dispatch[144] = gt;
        self.dispatch[145] = ge;
        self.dispatch[146] = teq; // teq
        // 150-156: same comparisons, emitted when the result feeds a branch.
        let method_def = self.a64_op_method_def();
        self.dispatch[2] = method_def;
        let send_simple = self.a64_op_send(true);
        let send = self.a64_op_send(false);
        self.dispatch[30] = send_simple;
        self.dispatch[31] = send;
        self.dispatch[32] = send_simple;
        self.dispatch[33] = send;

        let yield_op = self.a64_op_yield();
        self.dispatch[34] = yield_op;
        self.dispatch[35] = yield_op;

        // break / raise / retry / redo / ensure-end: set an error and route
        // through entry_raise, which handle_error turns into the right control
        // flow (break value / re-raise / retry / redo).
        let method_ret = self.a64_op_err1(runtime::err_method_return as u64, true);
        let block_break = self.a64_op_err1(runtime::err_block_break as u64, true);
        let raise_err = self.a64_op_err_raise();
        let retry_op = self.a64_op_err1(runtime::err_retry as u64, false);
        let redo_op = self.a64_op_err1(runtime::err_redo as u64, false);
        let ensure_end = self.a64_op_ensure_end();
        self.dispatch[81] = method_ret;
        self.dispatch[82] = block_break;
        self.dispatch[83] = raise_err;
        self.dispatch[84] = retry_op;
        self.dispatch[85] = ensure_end;
        self.dispatch[87] = redo_op;

        self.dispatch[150] = eq;
        self.dispatch[151] = ne;
        self.dispatch[152] = lt;
        self.dispatch[153] = le;
        self.dispatch[154] = gt;
        self.dispatch[155] = ge;
        self.dispatch[156] = teq; // teq

        let class_def = self.a64_op_class_def(false);
        let module_def = self.a64_op_class_def(true);
        let singleton_class_def = self.a64_op_singleton_class_def();
        self.dispatch[70] = class_def;
        self.dispatch[71] = module_def;
        self.dispatch[22] = singleton_class_def;

        let load_const = self.a64_op_load_const(runtime::vm_get_constant as u64);
        let check_const = self.a64_op_load_const(runtime::vm_check_constant as u64);
        let store_const = self.a64_op_store_const();
        self.dispatch[10] = load_const;
        self.dispatch[18] = check_const;
        self.dispatch[11] = store_const;

        let load_ivar = self.a64_op_load_ivar();
        let store_ivar = self.a64_op_store_ivar();
        self.dispatch[16] = load_ivar;
        self.dispatch[17] = store_ivar;

        // `defined?` family (ops 64-69): each computes a truthy/nil result.
        // const/method/ivar write through a *mut Value (dst address);
        // yield/super return the Value and we store it.
        let defined_yield = self.a64_op_defined_to_dst(runtime::defined_yield as u64);
        let defined_super = self.a64_op_defined_to_dst(runtime::defined_super as u64);
        let defined_const = self.a64_op_defined_const();
        let defined_method = self.a64_op_defined_method();
        let defined_gvar = self.a64_op_defined_gvar();
        let defined_ivar = self.a64_op_defined_ivar();
        self.dispatch[64] = defined_yield;
        self.dispatch[65] = defined_const;
        self.dispatch[66] = defined_method;
        self.dispatch[67] = defined_gvar;
        self.dispatch[68] = defined_ivar;
        self.dispatch[69] = defined_super;

        // literal constructors / aggregate ops
        let array = self.a64_op_array();
        let hash = self.a64_op_hash();
        let concat = self.a64_op_concat(runtime::concatenate_string as u64);
        let concat_regexp = self.a64_op_concat(runtime::concatenate_regexp as u64);
        let range_incl = self.a64_op_range(false);
        let range_excl = self.a64_op_range(true);
        let expand_array = self.a64_op_expand_array();
        self.dispatch[39] = array;
        self.dispatch[176] = hash;
        self.dispatch[181] = concat;
        self.dispatch[86] = concat_regexp;
        self.dispatch[179] = range_incl;
        self.dispatch[180] = range_excl;
        self.dispatch[173] = expand_array;

        let index = self.a64_op_index();
        let index_assign = self.a64_op_index_assign();
        self.dispatch[132] = index;
        self.dispatch[133] = index_assign;

        let singleton_method_def = self.a64_op_singleton_method_def();
        self.dispatch[1] = singleton_method_def;

        let alias_method = self.a64_op_alias_method();
        let undef_method = self.a64_op_undef_method();
        self.dispatch[175] = alias_method;
        self.dispatch[174] = undef_method;

        let load_gvar = self.a64_op_load_gvar();
        let store_gvar = self.a64_op_store_var(runtime::set_global_var as u64);
        let load_cvar = self.a64_op_load_cvar();
        let store_cvar = self.a64_op_store_var(runtime::set_class_var as u64);
        let alias_gvar = self.a64_op_alias_gvar();
        self.dispatch[25] = load_gvar;
        self.dispatch[26] = store_gvar;
        self.dispatch[27] = load_cvar;
        self.dispatch[28] = alias_gvar;
        self.dispatch[29] = store_cvar;

        let block_arg = self.a64_op_block_arg();
        let check_cvar = self.a64_op_check_cvar();
        let check_kw_rest = self.a64_op_check_kw_rest();
        self.dispatch[23] = block_arg;
        self.dispatch[24] = check_cvar;
        self.dispatch[19] = check_kw_rest;

        let load_dvar = self.a64_op_load_dvar();
        let store_dvar = self.a64_op_store_dvar();
        self.dispatch[148] = load_dvar;
        self.dispatch[149] = store_dvar;

        let block_arg_proxy = self.a64_op_block_arg_proxy();
        self.dispatch[21] = block_arg_proxy;

        let to_a = self.a64_op_to_a();
        self.dispatch[177] = to_a;

        // remaining binary operators (ops 164-170): bitor/bitand/bitxor/
        // rem/pow/shl/shr -- no fixnum fast path, straight to the runtime op.
        let bitor = self.a64_op_binop(bitor_values);
        let bitand = self.a64_op_binop(bitand_values);
        let bitxor = self.a64_op_binop(bitxor_values);
        let rem = self.a64_op_binop(rem_values);
        let pow = self.a64_op_binop(pow_values);
        let shl = self.a64_op_binop(shl_values);
        let shr = self.a64_op_binop(shr_values);
        self.dispatch[164] = bitor;
        self.dispatch[165] = bitand;
        self.dispatch[166] = bitxor;
        self.dispatch[167] = rem;
        self.dispatch[168] = pow;
        self.dispatch[169] = shl;
        self.dispatch[170] = shr;

        // unary operators (ops 121-124): pos, neg, bitnot, not
        let pos = self.a64_op_unop(pos_value as u64);
        let neg = self.a64_op_unop(neg_value as u64);
        let bitnot = self.a64_op_unop(bitnot_value as u64);
        let not = self.a64_op_unop(not_value as u64);
        self.dispatch[121] = pos;
        self.dispatch[122] = neg;
        self.dispatch[123] = bitnot;
        self.dispatch[124] = not;
    }

    /// ops 121-124 `UnOp` (pos/neg/bitnot/not): fn(vm, globals, src `[pc+2]`)
    /// -> dst `[pc+4]`.
    fn a64_op_unop(&mut self, abs: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 2);
        self.a64_slot_value(X2); // src
        self.jit.mov_imm(X9, abs);
        self.jit.blr(X9);
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 20 `CheckLocal`: branch by disp `[pc+0]` if local `[pc+4]` is set
    /// (non-zero); otherwise fall through (used for optional-param defaults).
    fn a64_op_check_local(&mut self, branch: &DestLabel) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.ldrsw(X10, PC, 0); // disp (for the shared branch target)
        self.jit.ldrh(X12, PC, 4); // local slot
        self.a64_slot_value(X12);
        self.jit.cbnz_label(X12, branch);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 36 `OptCase`: dense `case`/`when` jump table. opt_case returns the
    /// branch displacement for cond slot `[pc+4]` against OptCaseId `[pc+0]`,
    /// which feeds the shared branch target.
    fn a64_op_optcase(&mut self, branch: &DestLabel) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.ldr32(X2, PC, 0); // OptCaseId
        self.jit.ldrh(X10, PC, 4); // cond slot
        self.a64_slot_value(X10);
        self.jit.mov(X3, X10); // cond value
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, runtime::opt_case as u64);
        self.jit.blr(X9);
        self.jit.lsl_imm(X10, X0, 32); // zero-extend u32 disp into X10
        self.jit.lsr_imm(X10, X10, 32);
        self.jit.b_label(branch);
        p
    }

    /// op 38 `Lambda`: dst `[pc+4]` <- a lambda Proc for func_id `[pc+0]`.
    /// gen_lambda may promote the current frame to the heap, so LFP is
    /// reloaded from the cfp afterward.
    fn a64_op_lambda(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 0); // func_id
        self.jit.mov(X3, PC); // call-site pc
        self.jit.mov_imm(X9, runtime::gen_lambda as u64);
        self.jit.blr(X9);
        self.jit.sub_imm(X10, X29, (BP_CFP + CFP_LFP) as u32, 0);
        self.jit.ldr(LFP, X10, 0); // restore (possibly heap-promoted) LFP
        self.jit.ldrh(X10, PC, 4); // dst slot
        self.jit.cbz_label(X10, &skip);
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X11, LFP, X10, 3);
        self.jit.sub_imm(X11, X11, LFP_SELF as u32, 0);
        self.jit.str(X0, X11, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 37 `NilBr`: branch by disp `[pc+0]` if cond slot `[pc+4]` is nil.
    fn a64_op_nilbr(&mut self, branch: &DestLabel) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.ldrsw(X10, PC, 0); // disp (for shared branch target)
        self.jit.ldrh(X11, PC, 4); // cond slot
        self.a64_slot_value(X11);
        self.jit.cmp_imm(X11, NIL_VALUE as u32, 0);
        self.jit.bcond_label(Cond::Eq, branch);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 23 `BlockArg`: block_arg(vm, globals, lfp, pc) -> dst `[pc+4]`.
    fn a64_op_block_arg(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov(X2, LFP);
        self.jit.mov(X3, PC); // BytecodePtr (instruction start)
        self.jit.mov_imm(X9, runtime::block_arg as u64);
        self.jit.blr(X9);
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 177 `ToA`: dst `[pc+4]` <- `to_a(src `[pc+2]`)` (splat coercion).
    fn a64_op_to_a(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 2);
        self.a64_slot_value(X2); // src
        self.jit.mov_imm(X9, runtime::to_a as u64);
        self.jit.blr(X9);
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 21 `BlockArgProxy`: dst `[pc+4]` <- the block handler of the frame
    /// `[pc+0]` levels up, re-encoding a proxy handler's depth. (x86
    /// `vm_block_arg_proxy`.)
    fn a64_op_block_arg_proxy(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let loop_ = self.jit.label();
        let loop_exit = self.jit.label();
        let notzero = self.jit.label();
        let exit = self.jit.label();
        let skip = self.jit.label();
        self.jit.mov(X10, LFP);
        self.jit.ldr32(X11, PC, 0); // outer level
        self.jit.cbz_label(X11, &loop_exit);
        self.jit.bind_label(loop_.clone());
        self.jit.ldr(X10, X10, 0); // walk outer chain
        self.jit.subs_imm(X11, X11, 1, 0);
        self.jit.bcond_label(Cond::Ne, &loop_);
        self.jit.bind_label(loop_exit);
        // block handler = [outer - LFP_BLOCK]
        self.jit.sub_imm(X12, X10, LFP_BLOCK as u32, 0);
        self.jit.ldr(X10, X12, 0);
        self.jit.cbnz_label(X10, &notzero);
        self.jit.mov_imm(X10, NIL_VALUE); // no block -> nil
        self.jit.bind_label(notzero);
        // if bit0 == 0 (Proc/nil), keep as-is; else re-encode proxy depth.
        self.jit.tbz_label(X10, 0, &exit);
        self.jit.ldrsw(X12, PC, 0); // outer (signed)
        self.jit.lsl_imm(X12, X12, 2);
        self.jit.add(X10, X10, X12);
        self.jit.add_imm(X10, X10, 2, 0);
        self.jit.bind_label(exit);
        // store X10 to dst [pc+4]
        self.jit.ldrh(X11, PC, 4);
        self.jit.cbz_label(X11, &skip);
        self.jit.neg(X11, X11);
        self.jit.add_lsl(X12, LFP, X11, 3);
        self.jit.sub_imm(X12, X12, LFP_SELF as u32, 0);
        self.jit.str(X10, X12, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 148 `LoadDynVar`: dst `[pc+4]` <- the slot `[pc+2]` of the outer
    /// frame `[pc+0]` levels up the captured outer chain.
    fn a64_op_load_dvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        let loop_ = self.jit.label();
        let exit = self.jit.label();
        let skip = self.jit.label();
        self.jit.ldr(X10, LFP, 0); // X10 = level-1 outer ([LFP] = LFP_OUTER)
        self.jit.ldrh(X11, PC, 0); // outer level
        self.jit.bind_label(loop_.clone());
        self.jit.subs_imm(X11, X11, 1, 0);
        self.jit.bcond_label(Cond::Eq, &exit);
        self.jit.ldr(X10, X10, 0); // walk up
        self.jit.b_label(&loop_);
        self.jit.bind_label(exit);
        self.jit.cbz_label(X10, &raise);
        self.jit.ldrh(X12, PC, 2); // src slot in outer frame
        self.jit.neg(X12, X12);
        self.jit.add_lsl(X13, X10, X12, 3);
        self.jit.sub_imm(X13, X13, LFP_SELF as u32, 0);
        self.jit.ldr(X14, X13, 0); // value
        // store to dst [pc+4]
        self.jit.ldrh(X12, PC, 4);
        self.jit.cbz_label(X12, &skip);
        self.jit.neg(X12, X12);
        self.jit.add_lsl(X13, LFP, X12, 3);
        self.jit.sub_imm(X13, X13, LFP_SELF as u32, 0);
        self.jit.str(X14, X13, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 149 `StoreDynVar`: the slot `[pc+4]` of the outer frame `[pc+2]`
    /// levels up <- src slot `[pc+0]` of the current frame.
    fn a64_op_store_dvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let loop_ = self.jit.label();
        let exit = self.jit.label();
        self.jit.ldr(X10, LFP, 0); // level-1 outer
        self.jit.ldrh(X11, PC, 2); // outer level
        self.jit.bind_label(loop_.clone());
        self.jit.subs_imm(X11, X11, 1, 0);
        self.jit.bcond_label(Cond::Eq, &exit);
        self.jit.ldr(X10, X10, 0);
        self.jit.b_label(&loop_);
        self.jit.bind_label(exit);
        // src value from the current frame (slot [pc+0])
        self.jit.ldrh(X12, PC, 0);
        self.jit.neg(X12, X12);
        self.jit.add_lsl(X13, LFP, X12, 3);
        self.jit.sub_imm(X13, X13, LFP_SELF as u32, 0);
        self.jit.ldr(X14, X13, 0);
        // store to dst slot [pc+4] in the outer frame
        self.jit.ldrh(X12, PC, 4);
        self.jit.neg(X12, X12);
        self.jit.add_lsl(X13, X10, X12, 3);
        self.jit.sub_imm(X13, X13, LFP_SELF as u32, 0);
        self.jit.str(X14, X13, 0);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 19 `CheckKwRest`: if the kw-rest slot `[pc+4]` is nil, replace it
    /// with a fresh empty hash.
    fn a64_op_check_kw_rest(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let exit = self.jit.label();
        self.jit.ldrh(X10, PC, 4);
        self.a64_slot_addr(X10); // &slot
        self.jit.ldr(X11, X10, 0);
        self.jit.cmp_imm(X11, NIL_VALUE as u32, 0);
        self.jit.bcond_label(Cond::Ne, &exit);
        self.jit.mov_imm(X9, runtime::empty_hash as u64);
        self.jit.blr(X9);
        self.jit.ldrh(X10, PC, 4);
        self.a64_slot_addr(X10); // re-compute (clobbered by call)
        self.jit.str(X0, X10, 0);
        self.jit.bind_label(exit);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 24 `CheckCvar`: check_class_var(vm, globals, name `[pc+0]`) -> dst.
    fn a64_op_check_cvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 0); // name
        self.jit.mov_imm(X9, runtime::check_class_var as u64);
        self.jit.blr(X9);
        self.a64_store_dst_and_next(&skip);
        p
    }

    /// op 25 `LoadGvar`: get_global_var(vm, globals, name `[pc+0]`) -> Value.
    fn a64_op_load_gvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 0); // name
        self.jit.mov_imm(X9, runtime::get_global_var as u64);
        self.jit.blr(X9);
        self.a64_store_dst_and_next(&skip);
        p
    }

    /// op 27 `LoadCvar`: get_class_var(vm, globals, name `[pc+0]`) -> Option.
    fn a64_op_load_cvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 0); // name
        self.jit.mov_imm(X9, runtime::get_class_var as u64);
        self.jit.blr(X9);
        self.a64_checked_store_next(&raise);
        p
    }

    /// ops 26/29 `StoreGvar`/`StoreCvar`: set_*_var(vm, globals, name `[pc+0]`,
    /// val `[pc+4]`) -> Option (error-only; no result slot).
    fn a64_op_store_var(&mut self, set_fn: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 0); // name
        self.jit.ldrh(X3, PC, 4);
        self.a64_slot_value(X3); // val
        self.jit.mov_imm(X9, set_fn);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 28 `AliasGvar`: alias_global_var(globals, new `[pc+0]`, old `[pc+8]`).
    fn a64_op_alias_gvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.mov(X0, GLOBALS);
        self.jit.ldr32(X1, PC, 0); // new
        self.jit.ldr32(X2, PC, 8); // old
        self.jit.mov_imm(X9, runtime::alias_global_var as u64);
        self.jit.blr(X9);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 175 `AliasMethod`: alias_method(vm, globals, old `[pc+2]`,
    /// new `[pc+4]`).
    fn a64_op_alias_method(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 2);
        self.a64_slot_value(X2); // old
        self.jit.ldrh(X3, PC, 4);
        self.a64_slot_value(X3); // new
        self.jit.mov_imm(X9, runtime::alias_method as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 174 `UndefMethod`: undef_method(vm, globals, name `[pc+0]`).
    fn a64_op_undef_method(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 0); // name
        self.jit.mov_imm(X9, runtime::undef_method as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 1 `SingletonMethodDef`: `def obj.name` -- singleton_define_method(
    /// vm, globals, name `[pc+8]`, func_id `[pc+12]`, obj slot `[pc+4]`).
    fn a64_op_singleton_method_def(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 8); // name
        self.jit.ldr32(X3, PC, 12); // func_id
        self.jit.ldrh(X4, PC, 4); // obj slot
        self.a64_slot_value(X4); // obj
        self.jit.mov_imm(X9, runtime::singleton_define_method as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 132 `Index`: dst[`[pc+4]`] <- base[`[pc+2]`][idx[`[pc+0]`]], with an
    /// inline ClassId cache at `[pc+8]`.
    fn a64_op_index(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 2);
        self.a64_slot_value(X2); // base
        self.jit.ldrh(X3, PC, 0);
        self.a64_slot_value(X3); // idx
        self.jit.add_imm(X4, PC, 8, 0); // &cache
        self.jit.mov_imm(X9, runtime::get_index as u64);
        self.jit.blr(X9);
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 133 `IndexAssign`: base[`[pc+2]`][idx[`[pc+0]`]] <- src[`[pc+4]`],
    /// with an inline ClassId cache at `[pc+8]`.
    fn a64_op_index_assign(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 2);
        self.a64_slot_value(X2); // base
        self.jit.ldrh(X3, PC, 0);
        self.a64_slot_value(X3); // idx
        self.jit.ldrh(X4, PC, 4);
        self.a64_slot_value(X4); // src
        self.jit.add_imm(X5, PC, 8, 0); // &cache
        self.jit.mov_imm(X9, runtime::set_index as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// Store the Option<Value> result in X0 to the dst slot `[pc+4]`: branch
    /// to `raise` if it is 0 (error), else store, advance PC, and dispatch.
    fn a64_checked_store_next(&mut self, raise: &DestLabel) {
        let skip = self.jit.label();
        self.jit.cbz_label(X0, raise);
        self.jit.ldrh(X10, PC, 4);
        self.jit.cbz_label(X10, &skip);
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X11, LFP, X10, 3);
        self.jit.sub_imm(X11, X11, LFP_SELF as u32, 0);
        self.jit.str(X0, X11, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
    }

    /// op 39 `Array`: gen_array(vm, globals, callid `[pc+0]`, &self).
    fn a64_op_array(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 0); // callid
        self.jit.sub_imm(X3, LFP, LFP_SELF as u32, 0); // &self
        self.jit.mov_imm(X9, runtime::gen_array as u64);
        self.jit.blr(X9);
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 176 `Hash`: gen_hash(vm, globals, src `[pc+2]`, len `[pc+0]`).
    fn a64_op_hash(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 2);
        self.a64_slot_addr(X2); // src
        self.jit.ldrh(X3, PC, 0); // len
        self.jit.mov_imm(X9, runtime::gen_hash as u64);
        self.jit.blr(X9);
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 181 `ConcatStr` / op 86 `ConcatRegexp`: fn(vm, globals,
    /// args `[pc+2]`, len `[pc+0]`).
    fn a64_op_concat(&mut self, abs: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 2);
        self.a64_slot_addr(X2); // args
        self.jit.ldrh(X3, PC, 0); // len
        self.jit.mov_imm(X9, abs);
        self.jit.blr(X9);
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 179/180 `Range`: gen_range(start `[pc+2]`, end `[pc+0]`, vm,
    /// globals, exclude_end).
    fn a64_op_range(&mut self, exclude_end: bool) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.ldrh(X0, PC, 2);
        self.a64_slot_value(X0); // start
        self.jit.ldrh(X1, PC, 0);
        self.a64_slot_value(X1); // end
        self.jit.mov(X2, EXEC);
        self.jit.mov(X3, GLOBALS);
        self.jit.mov_imm(X4, if exclude_end { 1 } else { 0 });
        self.jit.mov_imm(X9, runtime::gen_range as u64);
        self.jit.blr(X9);
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 173 `ExpandArray`: expand_array(src `[pc+4]`, &dst `[pc+2]`,
    /// len `[pc+0]`, rest `[pc+8]`). No result.
    fn a64_op_expand_array(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.ldrh(X0, PC, 4);
        self.a64_slot_value(X0); // src (an Array Value)
        self.jit.ldrh(X1, PC, 2);
        self.a64_slot_addr(X1); // &dst
        self.jit.ldrh(X2, PC, 0); // len
        self.jit.ldrh(X3, PC, 8); // rest
        self.jit.mov_imm(X9, runtime::expand_array as u64);
        self.jit.blr(X9);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// Store `X0` (a Value) into the dst slot at `[pc+4]`, advance PC, dispatch.
    fn a64_store_dst_and_next(&mut self, skip: &DestLabel) {
        self.jit.ldrh(X10, PC, 4);
        self.jit.cbz_label(X10, skip);
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X11, LFP, X10, 3);
        self.jit.sub_imm(X11, X11, LFP_SELF as u32, 0);
        self.jit.str(X0, X11, 0);
        self.jit.bind_label(skip.clone());
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
    }

    /// `defined?` ops 64/69 (yield/super): fn(vm, globals) -> Value, stored to
    /// the dst slot `[pc+4]`.
    fn a64_op_defined_to_dst(&mut self, abs: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, abs);
        self.jit.blr(X9);
        self.a64_store_dst_and_next(&skip);
        p
    }

    /// op 65 `DefinedConst`: defined_const(vm, globals, &dst, site_id `[pc+8]`).
    fn a64_op_defined_const(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 4);
        self.a64_slot_addr(X2); // &dst
        self.jit.ldr32(X3, PC, 8); // site_id
        self.jit.mov_imm(X9, runtime::defined_const as u64);
        self.jit.blr(X9);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 66 `DefinedMethod`: defined_method(vm, globals, &dst, recv `[pc+2]`,
    /// name `[pc+8]`).
    fn a64_op_defined_method(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 4);
        self.a64_slot_addr(X2); // &dst
        self.jit.ldrh(X3, PC, 2);
        self.a64_slot_value(X3); // recv
        self.jit.ldr32(X4, PC, 8); // name
        self.jit.mov_imm(X9, runtime::defined_method as u64);
        self.jit.blr(X9);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 67 `DefinedGvar`: defined_gvar(vm, globals, name `[pc+8]`) -> Value.
    fn a64_op_defined_gvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 8); // name
        self.jit.mov_imm(X9, runtime::defined_gvar as u64);
        self.jit.blr(X9);
        self.a64_store_dst_and_next(&skip);
        p
    }

    /// op 68 `DefinedIvar`: defined_ivar(vm, globals, &dst, name `[pc+8]`).
    fn a64_op_defined_ivar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 4);
        self.a64_slot_addr(X2); // &dst
        self.jit.ldr32(X3, PC, 8); // name
        self.jit.mov_imm(X9, runtime::defined_ivar as u64);
        self.jit.blr(X9);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 16 `LoadIvar`: slot[`[pc+4]`] <- `self.@name` (name `[pc+0]`),
    /// with an inline (ClassId, IvarId) cache at `[pc+8]`.
    fn a64_op_load_ivar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        self.jit.sub_imm(X0, LFP, LFP_SELF as u32, 0);
        self.jit.ldr(X0, X0, 0); // base = self
        self.jit.ldr32(X1, PC, 0); // name
        self.jit.mov(X2, GLOBALS);
        self.jit.add_imm(X3, PC, 8, 0); // &cache
        self.jit
            .mov_imm(X9, get_instance_var_with_cache as u64);
        self.jit.blr(X9);
        self.jit.ldrh(X10, PC, 4); // dst slot
        self.jit.cbz_label(X10, &skip);
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X11, LFP, X10, 3);
        self.jit.sub_imm(X11, X11, LFP_SELF as u32, 0);
        self.jit.str(X0, X11, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 17 `StoreIvar`: `self.@name` (name `[pc+0]`) <- slot[`[pc+4]`],
    /// with an inline (ClassId, IvarId) cache at `[pc+8]`.
    fn a64_op_store_ivar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.sub_imm(X2, LFP, LFP_SELF as u32, 0);
        self.jit.ldr(X2, X2, 0); // base = self
        self.jit.ldr32(X3, PC, 0); // name
        self.jit.ldrh(X10, PC, 4); // src slot
        self.a64_slot_value(X10);
        self.jit.mov(X4, X10); // val
        self.jit.add_imm(X5, PC, 8, 0); // &cache
        self.jit
            .mov_imm(X9, set_instance_var_with_cache as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 10 `LoadConst`: slot[`[pc+4]`] <- constant at ConstSiteId `[pc+0]`.
    /// (x86 `vm_load_const`; the JIT inline-cache slot at `[pc+8]` is not
    /// written — the VM relies on the ConstSite cache + const_version.)
    fn a64_op_load_const(&mut self, get_fn: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        let skip = self.jit.label();
        let cv_addr = self.jit.get_label_address(&self.const_version_label()).as_ptr() as u64;
        self.jit.ldr32(X2, PC, 0); // ConstSiteId
        self.jit.mov_imm(X11, cv_addr);
        self.jit.ldr(X3, X11, 0); // const_version
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, get_fn);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.ldrh(X10, PC, 4); // dst slot
        self.jit.cbz_label(X10, &skip);
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X11, LFP, X10, 3);
        self.jit.sub_imm(X11, X11, LFP_SELF as u32, 0);
        self.jit.str(X0, X11, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 11 `StoreConst`: define constant ConstSiteId `[pc+0]` <- slot
    /// `[pc+4]`, bumping const_version. (x86 `vm_store_const`.)
    fn a64_op_store_const(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        let cv_addr = self.jit.get_label_address(&self.const_version_label()).as_ptr() as u64;
        self.jit.ldr32(X2, PC, 0); // ConstSiteId
        self.jit.ldrh(X10, PC, 4); // src slot
        self.a64_slot_value(X10);
        self.jit.mov(X3, X10); // val
        // const_version += 1
        self.jit.mov_imm(X11, cv_addr);
        self.jit.ldr(X12, X11, 0);
        self.jit.add_imm(X12, X12, 1, 0);
        self.jit.str(X12, X11, 0);
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, runtime::set_constant as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 70 `ClassDef` / op 71 `ModuleDef`: define the class/module, then
    /// run its body as a method with the class as `self`. Bytecode (16B):
    /// `+0` superclass slot (0 = none), `+2` base slot (0 = none),
    /// `+4` dst, `+8` name (IdentId), `+12` func_id (class body).
    fn a64_op_class_def(&mut self, is_module: bool) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        let sup_zero = self.jit.label();
        let sup_done = self.jit.label();
        let base_zero = self.jit.label();
        let base_done = self.jit.label();
        // define_class(vm, globals, name, superclass, is_module, base)
        // superclass (x3): slot[+0] value, or 0 (None) if slot index is 0.
        self.jit.ldrh(X10, PC, 0);
        self.jit.cbz_label(X10, &sup_zero);
        self.a64_slot_value(X10);
        self.jit.mov(X3, X10);
        self.jit.b_label(&sup_done);
        self.jit.bind_label(sup_zero);
        self.jit.mov_imm(X3, 0);
        self.jit.bind_label(sup_done);
        // base (x5): slot[+2] value, or 0 (None).
        self.jit.ldrh(X10, PC, 2);
        self.jit.cbz_label(X10, &base_zero);
        self.a64_slot_value(X10);
        self.jit.mov(X5, X10);
        self.jit.b_label(&base_done);
        self.jit.bind_label(base_zero);
        self.jit.mov_imm(X5, 0);
        self.jit.bind_label(base_done);
        self.jit.ldr32(X2, PC, 8); // name
        self.jit.mov_imm(X4, if is_module { 1 } else { 0 });
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, runtime::define_class as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.mov(X25, X0); // X25 = self (the class), callee-saved
        self.a64_class_def_run();
        p
    }

    /// op 22 `SingletonClassDef`: `class << base`. base = slot `[pc+0]`.
    fn a64_op_singleton_class_def(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        // define_singleton_class(vm, globals, base) -> self
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 0);
        self.a64_slot_value(X2); // base
        self.jit.mov_imm(X9, runtime::define_singleton_class as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.mov(X25, X0); // self = singleton class
        self.a64_class_def_run();
        p
    }

    /// Shared tail of class/module/singleton-class definition: with `X25` set
    /// to the (singleton) class, run the class body (`enter_classdef` ->
    /// call_funcdata -> `exit_classdef`) and store the result to dst `[pc+4]`.
    /// func_id is read from `[pc+12]`.
    fn a64_class_def_run(&mut self) {
        let skip = self.jit.label();
        // enter_classdef(vm, globals, func_id, self) -> &FuncData
        self.jit.ldr32(X2, PC, 12); // func_id
        self.jit.mov(X3, X25);
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, runtime::enter_classdef as u64);
        self.jit.blr(X9);
        self.jit.mov(X26, X0); // X26 = &FuncData, callee-saved
        // cont frame: save caller PC + ACC (the body clobbers them).
        self.jit.sub_imm(SP, SP, 16, 0);
        self.jit.str(PC, SP, 0);
        self.jit.str(ACC, SP, 8);
        // frame setup: zero outer/svar/cme/block; self = class; meta.
        self.jit.mov_imm(X12, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_OUTER) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_SVAR) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_CME) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_BLOCK) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_SELF) as u32, 0);
        self.jit.str(X25, X11, 0); // self = class
        self.jit.ldr(X10, X26, FUNCDATA_META as u32);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_META) as u32, 0);
        self.jit.str(X10, X11, 0);
        // call_funcdata: push frame, set lfp, pc, blr codeptr, restore cfp
        self.jit.ldr(X10, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(X11, SP, RSP_CFP as u32, 0);
        self.jit.str(X10, X11, 0);
        self.jit.str(X11, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(LFP, SP, RSP_LOCAL_FRAME as u32, 0);
        self.jit.sub_imm(X10, SP, (RSP_CFP + CFP_LFP) as u32, 0);
        self.jit.str(LFP, X10, 0);
        self.jit.ldr(PC, X26, FUNCDATA_PC as u32);
        self.jit.ldr(X10, X26, FUNCDATA_CODEPTR as u32);
        self.jit.blr(X10); // x0 = class body result
        self.jit.sub_imm(X11, SP, RSP_CFP as u32, 0);
        self.jit.ldr(X10, X11, 0);
        self.jit.str(X10, EXEC, EXECUTOR_CFP as u32);
        // restore caller LFP from its own frame (x29-relative)
        self.jit.sub_imm(X10, X29, (BP_CFP + CFP_LFP) as u32, 0);
        self.jit.ldr(LFP, X10, 0);
        self.jit.mov(X25, X0); // save result across exit_classdef
        // exit_classdef(vm, globals)
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, runtime::exit_classdef as u64);
        self.jit.blr(X9);
        self.jit.mov(X0, X25); // restore result
        // pop cont frame: restore PC + ACC
        self.jit.ldr(PC, SP, 0);
        self.jit.ldr(ACC, SP, 8);
        self.jit.add_imm(SP, SP, 16, 0);
        // store result to dst [PC+4]
        self.jit.ldrh(X10, PC, 4);
        self.jit.cbz_label(X10, &skip);
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X11, LFP, X10, 3);
        self.jit.sub_imm(X11, X11, LFP_SELF as u32, 0);
        self.jit.str(X0, X11, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
    }

    /// op 2 `method_def`: `define_method(vm, globals, name, func_id)`.
    /// Bytecode: `+8` name, `+12` func_id.
    fn a64_op_method_def(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.ldr32(X2, PC, 8); // name
        self.jit.ldr32(X3, PC, 12); // func_id
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, runtime::define_method as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 30-33 `send`/`send_simple`: method call (slow-path only — no inline
    /// cache; `find_method` does the lookup every time). Handles the simple
    /// case (no kw/block/splat). Bytecode (32 bytes): `+0` callid, `+4` ret
    /// slot, `+8` pos_num, `+10` arg slot, `+12` recv slot.
    fn a64_op_send(&mut self, is_simple: bool) -> CodePtr {
        let p = self.jit.get_current_address();
        let mm = self.jit.label();
        let argloop = self.jit.label();
        let argdone = self.jit.label();
        let generic = self.jit.label();
        let docall = self.jit.label();
        let skip = self.jit.label();
        let after_call = self.jit.label();
        let raise = self.entry_raise.clone();
        // push_cont_frame: save caller PC (sp -= 16; [sp] = PC)
        self.jit.sub_imm(SP, SP, 16, 0);
        self.jit.str(PC, SP, 0);
        // receiver
        self.jit.ldrh(X10, PC, 12);
        self.a64_load_slot(X10, X4, X11); // X4 = recv
        // callee self slot
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_SELF) as u32, 0);
        self.jit.str(X4, X11, 0);
        // find_method(vm, globals, callid, recv) -> funcid
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 0); // callid
        self.jit.mov(X3, X4); // recv
        self.jit.mov_imm(X9, runtime::find_method as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &mm);
        // get_func_data: X15 = funcinfo_base + funcid*64 + FUNCINFO_DATA
        self.jit.lsl_imm(X10, X0, 6);
        self.jit.mov_imm(X11, GLOBALS_FUNCINFO as u64);
        self.jit.add(X11, GLOBALS, X11);
        self.jit.ldr(X11, X11, 0);
        self.jit.add(X10, X10, X11);
        self.jit.add_imm(X15, X10, FUNCINFO_DATA as u32, 0);
        // set_method_outer: zero outer/svar/cme; set meta (kept in X14).
        self.jit.mov_imm(X12, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_OUTER) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_SVAR) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_CME) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.ldr(X14, X15, FUNCDATA_META as u32);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_META) as u32, 0);
        self.jit.str(X14, X11, 0);
        // Simple-send opcodes (no block/splat/kw at the call site) may take
        // the fast positional-copy path when the callee is also simple and
        // arity matches. The full-send opcodes always go generic so that
        // set_frame_block / splat / keyword handling runs.
        self.jit.ldrh(X9, PC, 8); // pos_num
        if is_simple {
            self.jit.lsr_imm(X16, X14, 56); // kind byte
            self.jit.tbz_label(X16, 4, &generic);
            self.jit.ldrh(X16, X15, FUNCDATA_MIN as u32);
            self.jit.cmp(X9, X16);
            self.jit.bcond_label(Cond::Ne, &generic);
        } else {
            self.jit.b_label(&generic);
        }
        // --- simple path: zero block + copy positional args directly ---
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_BLOCK) as u32, 0);
        self.jit.str(X12, X11, 0); // block = 0
        self.jit.ldrh(X10, PC, 10); // arg slot
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X10, LFP, X10, 3);
        self.jit.sub_imm(X10, X10, LFP_SELF as u32, 0); // args base (caller)
        self.jit.cbz_label(X9, &argdone);
        self.jit.neg(X9, X9);
        self.jit.bind_label(argloop.clone());
        self.jit.add_lsl(X11, X10, X9, 3);
        self.jit.ldr(X12, X11, 8); // src = [base + i*8 + 8]
        self.jit.sub_imm(X13, SP, (RSP_LOCAL_FRAME + LFP_SELF) as u32, 0);
        self.jit.add_lsl(X13, X13, X9, 3);
        self.jit.str(X12, X13, 0); // dst = callee self slot + i*8
        self.jit.add_imm(X9, X9, 1, 0);
        self.jit.cbnz_label(X9, &argloop);
        self.jit.bind_label(argdone);
        self.jit.b_label(&docall);
        // --- generic path: vm_handle_arguments(exec, globals, caller_lfp,
        // callee_lfp, callid). Handles rest/optional/keyword/splat + block. ---
        self.jit.bind_label(generic);
        self.jit.sub_imm(X3, SP, RSP_LOCAL_FRAME as u32, 0); // callee lfp
        // Reserve scratch below the callee frame (= ofs*16 + 16, 16-aligned)
        // so the C call's frame can't trample the callee frame being built.
        // Save the pre-reservation SP (X25) and funcdata ptr (X26) in
        // callee-saved registers (AAPCS64 preserves x19-x28); X15 is
        // caller-saved so it would otherwise be lost. Restore SP directly
        // from X25 afterwards.
        self.jit.mov_sp(X25, SP); // X25 = SP before reservation
        self.jit.mov(X26, X15);
        self.jit.ldrh(X10, X15, FUNCDATA_OFS as u32);
        self.jit.lsl_imm(X10, X10, 4);
        self.jit.add_imm(X10, X10, 16, 0);
        self.jit.sub(X11, X25, X10);
        self.jit.mov_sp(SP, X11);
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov(X2, LFP); // caller lfp
        self.jit.ldr32(X4, PC, 0); // callid
        self.jit.mov_imm(X9, runtime::vm_handle_arguments as u64);
        self.jit.blr(X9);
        self.jit.mov(X15, X26); // restore funcdata ptr
        self.jit.mov_sp(SP, X25); // restore SP directly
        self.jit.cbz_label(X0, &raise);
        self.jit.bind_label(docall);
        // call_funcdata: push_frame + set_lfp + pc + blr codeptr + restore cfp
        self.jit.ldr(X10, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(X11, SP, RSP_CFP as u32, 0);
        self.jit.str(X10, X11, 0);
        self.jit.str(X11, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(LFP, SP, RSP_LOCAL_FRAME as u32, 0);
        self.jit.sub_imm(X10, SP, (RSP_CFP + CFP_LFP) as u32, 0);
        self.jit.str(LFP, X10, 0);
        // 4th arg (X3) = call-site BytecodePtr, for with-pc builtins (x86
        // sets `rcx = r13 - 16`); aarch64 PC is already the call site.
        self.jit.mov(X3, PC);
        self.jit.ldr(PC, X15, FUNCDATA_PC as u32);
        self.jit.ldr(X10, X15, FUNCDATA_CODEPTR as u32);
        self.jit.blr(X10);
        self.jit.sub_imm(X11, SP, RSP_CFP as u32, 0);
        self.jit.ldr(X10, X11, 0);
        self.jit.str(X10, EXEC, EXECUTOR_CFP as u32);
        // restore caller LFP from its own frame (x86 `restore_lfp`):
        // LFP = [x29 - (BP_CFP + CFP_LFP)]. The callee clobbers LFP, so we
        // reload it from the caller's stable frame pointer (x29 == x86 rbp).
        self.jit.sub_imm(X10, X29, (BP_CFP + CFP_LFP) as u32, 0);
        self.jit.ldr(LFP, X10, 0);
        // pop_cont_frame: restore PC, advance past the 32-byte send
        self.jit.bind_label(after_call.clone());
        self.jit.ldr(PC, SP, 0);
        self.jit.add_imm(SP, SP, 16, 0);
        self.jit.cbz_label(X0, &raise); // result 0 => error
        self.jit.ldrh(X10, PC, 4); // ret slot
        self.jit.add_imm(PC, PC, 32, 0);
        self.jit.cbz_label(X10, &skip);
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X11, LFP, X10, 3);
        self.jit.sub_imm(X11, X11, LFP_SELF as u32, 0);
        self.jit.str(X0, X11, 0);
        self.jit.bind_label(skip);
        self.a64_fetch_and_dispatch();
        // method_missing: invoke_method_missing(vm, globals, recv, lfp,
        // callid) -> Option, then join the result path. The receiver register
        // was clobbered by find_method, so reload it from the recv slot.
        // invoke_method_missing manages its own frames and preserves PC/LFP
        // (callee-saved), so no cfp/LFP restore is needed here.
        self.jit.bind_label(mm);
        self.jit.ldrh(X10, PC, 12); // recv slot
        self.a64_slot_value(X10);
        self.jit.mov(X2, X10); // receiver
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov(X3, LFP);
        self.jit.ldr32(X4, PC, 0); // callid
        self.jit
            .mov_imm(X9, crate::codegen::runtime::invoke_method_missing as u64);
        self.jit.blr(X9);
        self.jit.b_label(&after_call);
        p
    }

    /// ops 82/84/87 `BlockBreak`/`Retry`/`Redo`: call `f(vm[, globals, val])`
    /// to set the control-flow error, then enter entry_raise. `with_val`
    /// passes slot `[pc+4]`'s value as a 3rd argument (BlockBreak).
    fn a64_op_err1(&mut self, abs: u64, with_val: bool) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        if with_val {
            self.jit.mov(X1, GLOBALS);
            self.jit.ldrh(X2, PC, 4);
            self.a64_slot_value(X2); // val
        }
        self.jit.mov_imm(X9, abs);
        self.jit.blr(X9);
        self.jit.b_label(&raise);
        p
    }

    /// op 83 `Raise`: raise_err(vm, exc `[pc+4]`), then enter entry_raise.
    fn a64_op_err_raise(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.ldrh(X1, PC, 4);
        self.a64_slot_value(X1); // exception value
        self.jit.mov_imm(X9, runtime::raise_err as u64);
        self.jit.blr(X9);
        self.jit.b_label(&raise);
        p
    }

    /// op 85 `EnsureEnd`: if an error is still pending after an ensure block,
    /// re-enter entry_raise; otherwise continue.
    fn a64_op_ensure_end(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov_imm(X9, runtime::check_err as u64);
        self.jit.blr(X9);
        self.jit.cbnz_label(X0, &raise);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 34/35 `Yield`: invoke the current block. Bytecode (32 bytes):
    /// `+0` callid, `+4` ret slot. The block's func/outer come from
    /// `get_yield_data`; self is the block's captured self. Args are set up
    /// via the runtime arg massager (callsite-driven).
    fn a64_op_yield(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        let skip = self.jit.label();
        // push_cont_frame: save caller PC
        self.jit.sub_imm(SP, SP, 16, 0);
        self.jit.str(PC, SP, 0);
        // get_yield_data(vm, globals) -> x0 = outer (Lfp), x1 = func_id
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit
            .mov_imm(X9, runtime::get_yield_data as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X1, &raise); // no block -> error set
        self.jit.mov(X25, X0); // X25 = outer (callee-saved across later calls)
        // get_func_data from func_id (X1) -> X15
        self.jit.lsl_imm(X10, X1, 32);
        self.jit.lsr_imm(X10, X10, 32);
        self.jit.lsl_imm(X10, X10, 6);
        self.jit.mov_imm(X11, GLOBALS_FUNCINFO as u64);
        self.jit.add(X11, GLOBALS, X11);
        self.jit.ldr(X11, X11, 0);
        self.jit.add(X10, X10, X11);
        self.jit.add_imm(X15, X10, FUNCINFO_DATA as u32, 0);
        // block frame setup: outer = X25, self = outer.self, svar/cme/block 0.
        self.jit.mov_imm(X12, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_OUTER) as u32, 0);
        self.jit.str(X25, X11, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_SVAR) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_CME) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_BLOCK) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.sub_imm(X10, X25, LFP_SELF as u32, 0);
        self.jit.ldr(X10, X10, 0); // self = outer.self
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_SELF) as u32, 0);
        self.jit.str(X10, X11, 0);
        self.jit.ldr(X14, X15, FUNCDATA_META as u32);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_META) as u32, 0);
        self.jit.str(X14, X11, 0);
        // generic arg setup: vm_handle_arguments(vm, globals, caller_lfp,
        // callee_lfp, callid). Reserve scratch; preserve SP/funcdata.
        self.jit.sub_imm(X3, SP, RSP_LOCAL_FRAME as u32, 0); // callee_lfp
        self.jit.mov_sp(X25, SP);
        self.jit.mov(X26, X15);
        self.jit.ldrh(X10, X15, FUNCDATA_OFS as u32);
        self.jit.lsl_imm(X10, X10, 4);
        self.jit.add_imm(X10, X10, 16, 0);
        self.jit.sub(X11, X25, X10);
        self.jit.mov_sp(SP, X11);
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov(X2, LFP); // caller lfp
        self.jit.ldr32(X4, PC, 0); // callid
        self.jit
            .mov_imm(X9, runtime::vm_handle_arguments as u64);
        self.jit.blr(X9);
        self.jit.mov(X15, X26);
        self.jit.mov_sp(SP, X25);
        self.jit.cbz_label(X0, &raise);
        // call_funcdata
        self.jit.ldr(X10, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(X11, SP, RSP_CFP as u32, 0);
        self.jit.str(X10, X11, 0);
        self.jit.str(X11, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(LFP, SP, RSP_LOCAL_FRAME as u32, 0);
        self.jit.sub_imm(X10, SP, (RSP_CFP + CFP_LFP) as u32, 0);
        self.jit.str(LFP, X10, 0);
        self.jit.mov(X3, PC); // call-site pc for with-pc builtins
        self.jit.ldr(PC, X15, FUNCDATA_PC as u32);
        self.jit.ldr(X10, X15, FUNCDATA_CODEPTR as u32);
        self.jit.blr(X10);
        self.jit.sub_imm(X11, SP, RSP_CFP as u32, 0);
        self.jit.ldr(X10, X11, 0);
        self.jit.str(X10, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(X10, X29, (BP_CFP + CFP_LFP) as u32, 0);
        self.jit.ldr(LFP, X10, 0); // restore caller LFP
        // pop_cont_frame + store result to ret slot [pc+4]
        self.jit.ldr(PC, SP, 0);
        self.jit.add_imm(SP, SP, 16, 0);
        self.jit.cbz_label(X0, &raise);
        self.jit.ldrh(X10, PC, 4); // ret slot
        self.jit.add_imm(PC, PC, 32, 0);
        self.jit.cbz_label(X10, &skip);
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X11, LFP, X10, 3);
        self.jit.sub_imm(X11, X11, LFP_SELF as u32, 0);
        self.jit.str(X0, X11, 0);
        self.jit.bind_label(skip);
        self.a64_fetch_and_dispatch();
        p
    }

    /// loop_start / loop_end (ops 14/15): advance + dispatch (VM-only).
    fn a64_op_loop(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// Unconditional branch (op 3) + the shared `branch` target used by the
    /// conditional branches. `pc += disp*16 + 16` (x86 `br_inst`/`branch:`).
    fn a64_op_br(&mut self) -> (CodePtr, DestLabel) {
        let p = self.jit.get_current_address();
        let branch = self.jit.label();
        self.jit.ldrsw(X10, PC, 0); // disp (signed, instruction-relative)
        self.jit.bind_label(branch.clone());
        self.jit.lsl_imm(X10, X10, 4);
        self.jit.add(PC, PC, X10);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        (p, branch)
    }

    /// Conditional branch (op 4/12 `condbr`, op 5/13 `condnotbr`). Bytecode:
    /// `+0` disp (i32), `+4` cond slot. Truthiness: `(v | 0x10) != FALSE_VALUE`
    /// (both nil and false collapse to FALSE_VALUE). `not` = branch-if-falsy.
    fn a64_op_condbr(&mut self, branch: &DestLabel, not: bool) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.ldrsw(X10, PC, 0); // disp (kept in X10 for `branch`)
        self.jit.ldrh(X11, PC, 4); // cond slot
        self.a64_load_slot(X11, X12, X13); // cond value
        self.jit.mov_imm(X13, 0x10);
        self.jit.orr(X12, X12, X13);
        self.jit.cmp_imm(X12, FALSE_VALUE as u32, 0);
        let cond = if not { Cond::Eq } else { Cond::Ne };
        self.jit.bcond_label(cond, branch);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// Integer comparison (ops 140-146): `%dst = (%lhs <cond> %rhs)` as a Ruby
    /// boolean. Bytecode: `+0` rhs, `+2` lhs, `+4` dst. Non-fixnum traps
    /// (generic runtime fallback TODO).
    fn a64_op_cmp(&mut self, cond: Cond, cmp_fn: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let generic = self.jit.label();
        let skip = self.jit.label();
        let raise = self.entry_raise.clone();
        self.jit.ldrh(X10, PC, 0); // rhs slot
        self.jit.ldrh(X11, PC, 2); // lhs slot
        self.jit.ldrh(X12, PC, 4); // dst slot
        self.a64_load_slot(X11, X13, X14); // lhs
        self.a64_load_slot(X10, X14, X15); // rhs
        self.jit.tbz_label(X13, 0, &generic);
        self.jit.tbz_label(X14, 0, &generic);
        self.jit.cmp(X13, X14);
        self.jit.cset(X13, cond);
        self.jit.lsl_imm(X13, X13, 3);
        self.jit.mov_imm(X14, FALSE_VALUE);
        self.jit.orr(X13, X13, X14); // FALSE_VALUE | (result << 3)
        self.jit.cbz_label(X12, &skip);
        self.jit.neg(X12, X12);
        self.jit.add_lsl(X10, LFP, X12, 3);
        self.jit.sub_imm(X10, X10, LFP_SELF as u32, 0);
        self.jit.str(X13, X10, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        self.jit.bind_label(generic);
        // cmp_*_values(vm, globals, lhs=X13, rhs=X14) -> Option<Value>
        self.jit.mov(X2, X13); // lhs
        self.jit.mov(X3, X14); // rhs
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, cmp_fn);
        self.jit.blr(X9);
        self.a64_checked_store_next(&raise);
        p
    }

    /// Load the value of the slot whose (positive) index is in `idx`, into
    /// `dst`. `idx` is clobbered. (the `[r14+reg*8-LFP_SELF]` idiom.)
    fn a64_load_slot(&mut self, idx: GReg, dst: GReg, scratch: GReg) {
        self.jit.neg(idx, idx);
        self.jit.add_lsl(scratch, LFP, idx, 3);
        self.jit.sub_imm(scratch, scratch, LFP_SELF as u32, 0);
        self.jit.ldr(dst, scratch, 0);
    }

    /// Generic binary-op fallback: call the runtime `func(vm, globals, lhs,
    /// rhs)` and store the result. Expects lhs in X13, rhs in X14, dst slot in
    /// X12 (all intact). VM globals are callee-saved so no register save is
    /// needed. On a Ruby error (result 0) jumps to entry_raise.
    /// ops 164-170: a binary operator with no fixnum fast path. Loads
    /// lhs `[pc+2]`, rhs `[pc+0]`, dst `[pc+4]` and calls the runtime op.
    fn a64_op_binop(&mut self, func: BinaryOpFn) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.ldrh(X10, PC, 0); // rhs slot
        self.jit.ldrh(X11, PC, 2); // lhs slot
        self.jit.ldrh(X12, PC, 4); // dst slot
        self.a64_load_slot(X11, X13, X14); // X13 = lhs
        self.a64_load_slot(X10, X14, X15); // X14 = rhs
        self.a64_generic_binop(func);
        p
    }

    fn a64_generic_binop(&mut self, func: BinaryOpFn) {
        let raise = self.entry_raise.clone();
        self.jit.mov(X2, X13); // lhs
        self.jit.mov(X3, X14); // rhs
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, func as u64);
        self.jit.blr(X9);
        // The dst slot is re-read from PC (callee-saved): `func` may re-enter
        // the VM (e.g. string `+` dispatches a method), clobbering the
        // caller-saved dst register held before the call.
        self.a64_checked_store_next(&raise);
    }

    /// op 162/163 `mul_rr`/`div_rr`: no fixnum fast path — straight to the
    /// runtime fallback (matches x86 `vm_binops`). Bytecode: `+0` rhs, `+2`
    /// lhs, `+4` dst.
    fn a64_op_muldiv(&mut self, func: BinaryOpFn) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.ldrh(X10, PC, 0);
        self.jit.ldrh(X11, PC, 2);
        self.jit.ldrh(X12, PC, 4);
        self.a64_load_slot(X11, X13, X14); // lhs
        self.a64_load_slot(X10, X14, X15); // rhs
        self.a64_generic_binop(func);
        p
    }

    /// op 160/161 `add_rr`/`sub_rr`: fixnum fast path (`%dst = %lhs ± %rhs`)
    /// with a runtime fallback on non-fixnum/overflow. Operands are kept in
    /// X13/X14 so the fallback can use them; the result is computed in X9.
    fn a64_op_iadd(&mut self, is_sub: bool) -> CodePtr {
        let p = self.jit.get_current_address();
        let generic = self.jit.label();
        let skip = self.jit.label();
        self.jit.ldrh(X10, PC, 0); // rhs slot
        self.jit.ldrh(X11, PC, 2); // lhs slot
        self.jit.ldrh(X12, PC, 4); // dst slot
        self.a64_load_slot(X11, X13, X14); // X13 = lhs
        self.a64_load_slot(X10, X14, X15); // X14 = rhs
        self.jit.tbz_label(X13, 0, &generic);
        self.jit.tbz_label(X14, 0, &generic);
        if is_sub {
            self.jit.subs(X9, X13, X14);
            self.jit.bcond_label(Cond::Vs, &generic);
            self.jit.add_imm(X9, X9, 1, 0); // re-tag
        } else {
            self.jit.sub_imm(X9, X13, 1, 0); // untag one
            self.jit.adds(X9, X9, X14);
            self.jit.bcond_label(Cond::Vs, &generic);
        }
        self.jit.cbz_label(X12, &skip);
        self.jit.neg(X12, X12);
        self.jit.add_lsl(X10, LFP, X12, 3);
        self.jit.sub_imm(X10, X10, LFP_SELF as u32, 0);
        self.jit.str(X9, X10, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        self.jit.bind_label(generic);
        self.a64_generic_binop(if is_sub {
            sub_values
        } else {
            add_values
        });
        p
    }

    /// op 172 `init_method`: allocate the method's stack frame and nil-fill the
    /// uninitialized local slots. Bytecode (relative to instruction start):
    /// `+0` stack-offset, `+2` arg_num, `+4` reg_num. (x86 `vm_init`.)
    /// TODO(aarch64): the captured-frame guard (`branch_if_captured`) — not
    /// needed for non-block top-level methods.
    fn a64_op_init_method(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        let loop_ = self.jit.label();
        // allocate stack: sp -= stack_offset * 16
        self.jit.ldrh(X10, PC, 0);
        self.jit.lsl_imm(X10, X10, 4);
        self.jit.mov_sp(X13, SP); // sp -= X10 (A64 sub can't take SP as a
        self.jit.sub(X13, X13, X10); // shifted-reg operand, so via a GPR)
        self.jit.mov_sp(SP, X13);
        // count = reg_num - arg_num
        self.jit.ldrh(X15, PC, 4); // reg_num
        self.jit.ldrh(X11, PC, 2); // arg_num
        self.jit.sub(X12, X15, X11);
        self.jit.cbz_label(X12, &skip);
        // base = lfp - reg_num*8 - LFP_ARG0 ; fill [base + count*8] downward
        self.jit.neg(X15, X15);
        self.jit.add_lsl(X15, LFP, X15, 3);
        self.jit.sub_imm(X15, X15, LFP_ARG0 as u32, 0);
        self.jit.mov_imm(X14, NIL_VALUE);
        self.jit.bind_label(loop_.clone());
        self.jit.add_lsl(X10, X15, X12, 3);
        self.jit.str(X14, X10, 0);
        self.jit.sub_imm(X12, X12, 1, 0);
        self.jit.cbnz_label(X12, &loop_);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
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

    /// op 7 `literal`: slot[`[pc+4]`] <- a deep copy of the literal Value at
    /// `[pc+8]`. Each evaluation yields a fresh object (mutable literals like
    /// strings/arrays). x86 `vm_literal`: `movq rdi,[r13-8]; value_deep_copy`.
    fn a64_op_literal(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        self.jit.ldr(X0, PC, 8); // literal Value
        self.jit.mov_imm(X9, Value::value_deep_copy as u64);
        self.jit.blr(X9); // x0 = deep copy (PC/LFP are callee-saved)
        self.jit.ldrh(X10, PC, 4); // dst slot index
        self.jit.cbz_label(X10, &skip); // slot 0 => discard
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X12, LFP, X10, 3);
        self.jit.sub_imm(X12, X12, LFP_SELF as u32, 0);
        self.jit.str(X0, X12, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 178 `Mov`: slot[`[pc+4]`] <- slot[`[pc+2]`]. (x86 `fetch3` +
    /// slot copy.)
    fn a64_op_mov(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        self.jit.ldrh(X10, PC, 2); // src slot
        self.a64_slot_value(X10); // X10 = slot[src]
        self.jit.ldrh(X11, PC, 4); // dst slot
        self.jit.cbz_label(X11, &skip);
        self.jit.neg(X11, X11);
        self.jit.add_lsl(X12, LFP, X11, 3);
        self.jit.sub_imm(X12, X12, LFP_SELF as u32, 0);
        self.jit.str(X10, X12, 0);
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

    /// Per-method entry wrapper. For ISeq methods (VM-only build) this is the
    /// vm-stub `b vm_entry` (x86 `gen_vm_stub`). TODO(aarch64): other
    /// FuncKinds (Builtin/AttrReader/ConstReturn/…).
    pub(crate) fn gen_wrapper(&mut self, globals: &Globals, fid: FuncId) -> DestLabel {
        let entry = self.jit.label();
        self.jit.bind_label(entry.clone());
        match &globals.store[fid].kind {
            FuncKind::ISeq(_) => {
                let vm_entry = self.vm_entry();
                self.jit.b_label(&vm_entry);
            }
            FuncKind::Builtin { abs_address } => {
                let abs = *abs_address;
                self.a64_gen_native_func_wrapper(abs);
            }
            FuncKind::AttrReader { ivar_name } => {
                let name = *ivar_name;
                self.a64_gen_attr_reader(name);
            }
            FuncKind::AttrWriter { ivar_name } => {
                let name = *ivar_name;
                self.a64_gen_attr_writer(name);
            }
            FuncKind::StructReader { slot_index, inline } => {
                let (slot_index, inline) = (*slot_index, *inline);
                self.a64_gen_struct_reader(slot_index, inline);
            }
            FuncKind::StructWriter { slot_index, .. } => {
                let slot_index = *slot_index;
                self.a64_gen_struct_writer(slot_index);
            }
            _ => {
                // TODO(aarch64): Proc wrapper.
                self.jit.mov_imm(X0, 0xbad0);
                self.jit
                    .mov_imm(X9, crate::codegen::runtime::report_unimpl_op as u64);
                self.jit.blr(X9);
                self.jit.brk(0);
            }
        }
        self.jit.finalize();
        entry
    }

    /// attr_reader: return `self.@ivar_name` via the cached ivar accessor.
    fn a64_gen_attr_reader(&mut self, ivar_name: IdentId) {
        // Per-method inline cache (InstanceVarCache, 8 bytes, init -1 = miss).
        // Leaked on the heap so its address is known at emit time; aarch64 ADR
        // can't reach a JIT data label from the lazily-generated wrapper.
        let cache_addr = Box::into_raw(Box::new(-1i64)) as u64;
        self.jit.stp_pre(X29, X30, SP, -16);
        self.jit.mov_sp(X29, SP);
        self.jit.sub_imm(X0, LFP, LFP_SELF as u32, 0);
        self.jit.ldr(X0, X0, 0); // self
        self.jit.mov_imm(X1, ivar_name.get() as u64); // name
        self.jit.mov(X2, GLOBALS);
        self.jit.mov_imm(X3, cache_addr); // &cache
        self.jit
            .mov_imm(X9, get_instance_var_with_cache as u64);
        self.jit.blr(X9);
        self.jit.mov_sp(SP, X29);
        self.jit.ldp_post(X29, X30, SP, 16);
        self.jit.ret();
    }

    /// attr_writer: `self.@ivar_name = arg0` via the cached ivar setter.
    fn a64_gen_attr_writer(&mut self, ivar_name: IdentId) {
        let cache_addr = Box::into_raw(Box::new(-1i64)) as u64;
        self.jit.stp_pre(X29, X30, SP, -16);
        self.jit.mov_sp(X29, SP);
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.sub_imm(X2, LFP, LFP_SELF as u32, 0);
        self.jit.ldr(X2, X2, 0); // self
        self.jit.mov_imm(X3, ivar_name.get() as u64); // name
        self.jit.sub_imm(X4, LFP, LFP_ARG0 as u32, 0);
        self.jit.ldr(X4, X4, 0); // val
        self.jit.mov_imm(X5, cache_addr); // &cache
        self.jit
            .mov_imm(X9, set_instance_var_with_cache as u64);
        self.jit.blr(X9);
        self.jit.mov_sp(SP, X29);
        self.jit.ldp_post(X29, X30, SP, 16);
        self.jit.ret();
    }

    /// Struct member reader: read inline slot or heap slot directly (no call).
    fn a64_gen_struct_reader(&mut self, slot_index: u16, inline: bool) {
        self.jit.sub_imm(X0, LFP, LFP_SELF as u32, 0);
        self.jit.ldr(X0, X0, 0); // self
        if inline {
            self.jit
                .ldr(X0, X0, (slot_index as u32) * 8 + RVALUE_OFFSET_INLINE as u32);
        } else {
            self.jit.ldr(X0, X0, RVALUE_OFFSET_HEAP_PTR as u32);
            self.jit.ldr(X0, X0, (slot_index as u32) * 8);
        }
        self.jit.ret();
    }

    /// Struct member writer: route through set_struct_slot_with_check.
    fn a64_gen_struct_writer(&mut self, slot_index: u16) {
        self.jit.stp_pre(X29, X30, SP, -16);
        self.jit.mov_sp(X29, SP);
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.sub_imm(X2, LFP, LFP_SELF as u32, 0);
        self.jit.ldr(X2, X2, 0); // self
        self.jit.sub_imm(X3, LFP, LFP_ARG0 as u32, 0);
        self.jit.ldr(X3, X3, 0); // val
        self.jit.mov_imm(X4, slot_index as u64);
        self.jit.mov_imm(
            X9,
            crate::builtins::struct_class::set_struct_slot_with_check as u64,
        );
        self.jit.blr(X9);
        self.jit.mov_sp(SP, X29);
        self.jit.ldp_post(X29, X30, SP, 16);
        self.jit.ret();
    }

    /// Native (builtin) method wrapper: allocate the arg stack region and call
    /// the Rust builtin `fn(vm, globals, lfp)`. Mirrors x86
    /// `gen_native_func_wrapper`.
    fn a64_gen_native_func_wrapper(&mut self, abs_address: u64) {
        self.jit.stp_pre(X29, X30, SP, -16);
        self.jit.mov_sp(X29, SP);
        // stack offset = ((reg_num + (RSP_LOCAL_FRAME+LFP_ARG0)/8 + 1) & !1) * 8
        self.jit.sub_imm(X10, LFP, LFP_REGNUM as u32, 0);
        self.jit.ldrh(X10, X10, 0); // reg_num
        self.jit
            .add_imm(X10, X10, ((RSP_LOCAL_FRAME + LFP_ARG0) / 8 + 1) as u32, 0);
        self.jit.mov_imm(X11, !1u64);
        self.jit.and_(X10, X10, X11);
        self.jit.lsl_imm(X10, X10, 3);
        self.jit.mov_sp(X11, SP);
        self.jit.sub(X11, X11, X10);
        self.jit.mov_sp(SP, X11);
        // builtin(vm, globals, lfp)
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov(X2, LFP);
        self.jit.mov_imm(X9, abs_address);
        self.jit.blr(X9);
        self.jit.mov_sp(SP, X29);
        self.jit.ldp_post(X29, X30, SP, 16);
        self.jit.ret();
    }

    /// TODO(aarch64): the VM BOP fast-path optimization is not emitted yet, so
    /// there is nothing to remove.
    pub(super) fn remove_vm_bop_optimization(&mut self) {}
}
