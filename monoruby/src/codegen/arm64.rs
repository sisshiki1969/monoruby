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

    // --- invokers / entry points ---
    /// Real `method_invoker` for the simple case (0 args, no kw): set up the
    /// Ruby frame and call into `vm_entry`, then return the result to Rust.
    /// AAPCS64 in: x0 exec, x1 globals, x2 funcid, x3 self, x4 args, x5 len,
    /// x6 block, x7 hashmap. The frame offsets match x86 because aarch64's
    /// `stp fp,lr` (16B at vm_entry) equals x86's `call`(8B)+`push rbp`(8B).
    /// TODO(aarch64): argument copying (assumes 0 args) + stack-overflow check.
    pub(super) fn method_invoker(&mut self) -> MethodInvoker {
        let codeptr = self.jit.get_current_address();
        let fdata = X9;
        // prologue: save callee-saved globals + fp/lr, set globals
        self.jit.stp_pre(X29, X30, SP, -16);
        self.jit.stp_pre(EXEC, GLOBALS, SP, -16);
        self.jit.stp_pre(PC, LFP, SP, -16);
        self.jit.stp_pre(ACC, X24, SP, -16);
        self.jit.mov(EXEC, X0);
        self.jit.mov(GLOBALS, X1);
        // get_func_data: fdata = funcinfo_base + funcid*64 + FUNCINFO_DATA
        self.jit.lsl_imm(X10, X2, 32); // zero-extend funcid
        self.jit.lsr_imm(X10, X10, 32);
        self.jit.lsl_imm(X10, X10, 6); // * size_of::<FuncInfo>() (64)
        self.jit.mov_imm(X11, GLOBALS_FUNCINFO as u64);
        self.jit.add(X11, GLOBALS, X11);
        self.jit.ldr(X11, X11, 0); // funcinfo base ptr
        self.jit.add(X10, X10, X11);
        self.jit.add_imm(fdata, X10, FUNCINFO_DATA as u32, 0);
        // frame setup (not block): FB = sp - (RSP_LOCAL_FRAME + LFP_SELF);
        // slot at lfp-LFP_X sits at FB + (LFP_SELF - LFP_X).
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
        // epilogue (result Value in x0)
        self.jit.ldp_post(ACC, X24, SP, 16);
        self.jit.ldp_post(PC, LFP, SP, 16);
        self.jit.ldp_post(EXEC, GLOBALS, SP, 16);
        self.jit.ldp_post(X29, X30, SP, 16);
        self.jit.ret();
        // SAFETY: codeptr points at an extern "C" fn with the MethodInvoker ABI.
        unsafe { std::mem::transmute_copy::<*mut u8, MethodInvoker>(&codeptr.as_ptr()) }
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

        let init_method = self.a64_op_init_method();
        let immediate = self.a64_op_immediate();
        let ret = self.a64_op_ret();
        let add_rr = self.a64_op_iadd(false);
        let sub_rr = self.a64_op_iadd(true);
        self.dispatch[6] = immediate;
        self.dispatch[80] = ret;
        self.dispatch[172] = init_method;
        self.dispatch[160] = add_rr;
        self.dispatch[161] = sub_rr;

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

        // integer comparisons (fixnum fast path)
        let eq = self.a64_op_cmp(Cond::Eq);
        let ne = self.a64_op_cmp(Cond::Ne);
        let lt = self.a64_op_cmp(Cond::Lt);
        let le = self.a64_op_cmp(Cond::Le);
        let gt = self.a64_op_cmp(Cond::Gt);
        let ge = self.a64_op_cmp(Cond::Ge);
        self.dispatch[140] = eq;
        self.dispatch[141] = ne;
        self.dispatch[142] = lt;
        self.dispatch[143] = le;
        self.dispatch[144] = gt;
        self.dispatch[145] = ge;
        self.dispatch[146] = eq; // teq
        // 150-156: same comparisons, emitted when the result feeds a branch.
        self.dispatch[150] = eq;
        self.dispatch[151] = ne;
        self.dispatch[152] = lt;
        self.dispatch[153] = le;
        self.dispatch[154] = gt;
        self.dispatch[155] = ge;
        self.dispatch[156] = eq; // teq
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
    fn a64_op_cmp(&mut self, cond: Cond) -> CodePtr {
        let p = self.jit.get_current_address();
        let generic = self.jit.label();
        let skip = self.jit.label();
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
        self.jit.brk(0); // TODO(aarch64): generic comparison runtime fallback
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

    /// op 160/161 `add_rr`/`sub_rr`: fixnum fast path (`%dst = %lhs ± %rhs`).
    /// Bytecode: `+0` rhs slot, `+2` lhs slot, `+4` dst slot. Non-fixnum or
    /// overflow falls to `generic` (a trap for now — TODO: runtime fallback to
    /// add_values/sub_values).
    fn a64_op_iadd(&mut self, is_sub: bool) -> CodePtr {
        let p = self.jit.get_current_address();
        let generic = self.jit.label();
        let skip = self.jit.label();
        self.jit.ldrh(X10, PC, 0); // rhs slot
        self.jit.ldrh(X11, PC, 2); // lhs slot
        self.jit.ldrh(X12, PC, 4); // dst slot
        self.a64_load_slot(X11, X13, X14); // X13 = lhs
        self.a64_load_slot(X10, X14, X15); // X14 = rhs
        self.jit.tbz_label(X13, 0, &generic); // lhs not fixnum
        self.jit.tbz_label(X14, 0, &generic); // rhs not fixnum
        if is_sub {
            self.jit.subs(X13, X13, X14);
            self.jit.bcond_label(Cond::Vs, &generic);
            self.jit.add_imm(X13, X13, 1, 0); // re-tag
        } else {
            self.jit.sub_imm(X13, X13, 1, 0); // untag one
            self.jit.adds(X13, X13, X14);
            self.jit.bcond_label(Cond::Vs, &generic);
        }
        // store result (X13) to slot[dst] (X12), skip if dst == 0
        self.jit.cbz_label(X12, &skip);
        self.jit.neg(X12, X12);
        self.jit.add_lsl(X10, LFP, X12, 3);
        self.jit.sub_imm(X10, X10, LFP_SELF as u32, 0);
        self.jit.str(X13, X10, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        self.jit.bind_label(generic);
        self.jit.brk(0); // TODO(aarch64): generic binop runtime fallback
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
            _ => {
                self.jit.brk(0);
            }
        }
        self.jit.finalize();
        entry
    }

    /// TODO(aarch64): the VM BOP fast-path optimization is not emitted yet, so
    /// there is nothing to remove.
    pub(super) fn remove_vm_bop_optimization(&mut self) {}
}
