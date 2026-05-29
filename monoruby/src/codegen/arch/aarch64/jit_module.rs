//! aarch64 `JitModule` asm methods: bytecode dispatch + frame
//! primitives, `brk` stubs, `JitModule::new`, and the per-signal stub.
//!
//! Counterpart of `arch/x86_64/jit_module.rs`.

use super::*;

impl JitModule {
    /// Fetch the opcode and dispatch through the 256-entry table.
    ///
    /// x86: `movq r15,(table); movzxb rax,[r13+OPECODE]; addq r13,16;
    /// jmp [r15+rax*8]`. On aarch64 the pc is *not* pre-advanced (A64 has no
    /// LDUR-style negative-displacement scaled load in the builder yet), so
    /// handlers read operands at positive offsets and advance the pc
    /// themselves before re-dispatching.
    ///
    /// ### in
    /// - x21 (PC): bytecode pointer
    /// ### destroy
    /// - x9 (TBL), x10 (OP), x11 (TGT)
    pub(in crate::codegen) fn a64_fetch_and_dispatch(&mut self) {
        let table = self.dispatch.as_ptr() as u64;
        self.jit.mov_imm(TBL, table); // table base
        self.jit.ldrb(OP, PC, OPECODE as u32); // opcode <- [pc + OPECODE]
        self.jit.ldr_reg(TGT, TBL, OP, true); // handler <- table[opcode] (lsl #3)
        self.jit.br(TGT);
    }

    /// Address of the local slot whose index is in `reg`:
    /// `[r14 + reg*8 - LFP_SELF]`. x86: `negq R(r); lea R(r),[r14+R(r)*8-SELF]`.
    /// aarch64: `neg; add Xr,x_lfp,Xr,lsl #3; sub #LFP_SELF`.
    ///
    /// ### in / out
    /// - `dst`: slot index in → slot *address* out
    pub(in crate::codegen) fn a64_slot_addr(&mut self, dst: GReg) {
        self.jit.neg(dst, dst);
        self.jit.add_lsl(dst, LFP, dst, 3);
        self.jit.sub_imm(dst, dst, LFP_SELF as u32, 0);
    }

    /// Value of the local slot whose index is in `reg` (slot index → value).
    pub(in crate::codegen) fn a64_slot_value(&mut self, dst: GReg) {
        self.a64_slot_addr(dst);
        self.jit.ldr(dst, dst, 0);
    }

    /// VM entry prologue: save the callee-saved global registers + fp/lr.
    /// x86 analogue: `pushq rbp; movq rbp, rsp`.
    pub(in crate::codegen) fn a64_prologue(&mut self) {
        self.jit.stp_pre(X29, X30, SP, -16); // fp, lr
        self.jit.stp_pre(EXEC, GLOBALS, SP, -16);
        self.jit.stp_pre(PC, LFP, SP, -16);
        self.jit.stp_pre(ACC, X19_PAD, SP, -16); // ACC + 8-byte pad (16-align)
    }

    /// VM exit epilogue: restore the callee-saved registers. x86: `leave; ret`.
    pub(in crate::codegen) fn a64_epilogue(&mut self) {
        self.jit.ldp_post(ACC, X19_PAD, SP, 16);
        self.jit.ldp_post(PC, LFP, SP, 16);
        self.jit.ldp_post(EXEC, GLOBALS, SP, 16);
        self.jit.ldp_post(X29, X30, SP, 16);
        self.jit.ret();
    }
    /// Bind `label` at the current position and emit a trap.
    pub(in crate::codegen) fn a64_brk_stub(&mut self, label: &DestLabel) {
        self.jit.bind_label(label.clone());
        self.jit.brk(0);
    }

    /// Like `a64_brk_stub` but reports a diagnostic id before trapping.
    pub(in crate::codegen) fn a64_brk_stub_diag(&mut self, label: &DestLabel, id: u64) {
        self.jit.bind_label(label.clone());
        self.jit.mov_imm(X0, id);
        self.jit
            .mov_imm(X9, crate::codegen::runtime::report_unimpl_op as *const () as u64);
        self.jit.blr(X9);
        self.jit.brk(0);
    }

    /// Emit a `brk` trampoline and return its address as a typed fn pointer.
    /// TODO(aarch64): replace each caller with the real invoker.
    pub(in crate::codegen) fn a64_stub_fn<T>(&mut self, id: u64) -> T {
        let p = self.jit.get_current_address();
        self.jit.mov_imm(X0, 0x51410000 + id); // DIAG: stub invoker hit
        self.jit
            .mov_imm(X9, crate::codegen::runtime::report_unimpl_op as *const () as u64);
        self.jit.blr(X9);
        self.jit.brk(0);
        // SAFETY: T is always a pointer-sized `extern "C"` fn pointer; the
        // trampoline traps if ever called (M0 in progress).
        unsafe { std::mem::transmute_copy::<*mut u8, T>(&p.as_ptr()) }
    }

    pub(in crate::codegen) fn new() -> Self {
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
        jit.mov_imm(X9, crate::codegen::runtime::report_unimpl_op as *const () as u64);
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
        // exec_gc is bound by `construct_vm` (real handler defers to
        // entry_raise on a poll-time error).
        let _ = gc;
        j.a64_brk_stub_diag(&f64v, 0xffff04); // f64_to_val
        // vm_stack_overflow is bound by `construct_vm` (the real handler
        // needs entry_raise, which isn't emitted until then).
        let _ = ovf;
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
    /// Per-signal async-signal-safe stub. Mirrors the x86 version
    /// (`addl [alloc_flag], 10; orl [pending_signals], (bit); ret`). The
    /// handler nudges `alloc_flag` (so the next GC poll notices the signal)
    /// and OR-sets the signal bit into `pending_signals`. `signo` is folded
    /// into the immediate bit at codegen time, so the running handler does
    /// only memory ops and returns.
    ///
    /// The RMW isn't atomic (no ldxr/stxr — these would be illegal at this
    /// level inside async-signal context too), so a nested signal during the
    /// sequence may lose an increment / a bit. The x86 version has the same
    /// property (no LOCK prefix); the lossy case is rare and harmless — the
    /// next allocation poll picks it up.
    pub(crate) fn signal_handler_for(
        &mut self,
        alloc_flag: DestLabel,
        pending_signals: DestLabel,
        signo: i32,
    ) -> CodePtr {
        debug_assert!((1..=32).contains(&signo));
        let bit: u64 = 1u64 << (signo - 1);
        let af_addr = self.jit.get_label_address(&alloc_flag).as_ptr() as u64;
        let ps_addr = self.jit.get_label_address(&pending_signals).as_ptr() as u64;
        let p = self.jit.get_current_address();
        // alloc_flag += 10  (32-bit RMW)
        self.jit.mov_imm(X0, af_addr);
        self.jit.ldr32(X1, X0, 0);
        self.jit.add_imm(X1, X1, 10, 0);
        self.jit.str32(X1, X0, 0);
        // pending_signals |= bit  (32-bit RMW)
        self.jit.mov_imm(X0, ps_addr);
        self.jit.ldr32(X1, X0, 0);
        self.jit.mov_imm(X2, bit);
        self.jit.orr(X1, X1, X2);
        self.jit.str32(X1, X0, 0);
        self.jit.ret();
        p
    }
}
