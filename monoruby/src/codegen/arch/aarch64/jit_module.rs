//! aarch64 `JitModule` asm methods: bytecode dispatch + frame
//! primitives, `brk` stubs, `JitModule::new`, and the per-signal stub.
//!
//! Counterpart of `arch/x86_64/jit_module.rs`. Instruction sequences are
//! written with the `monoasm_arm64!` A64 assembly DSL (the aarch64 analogue
//! of x86's `monoasm!`); the role registers (`EXEC`/`PC`/`TBL`/…) stay
//! `GReg` constants and are spelled `x(NAME.0)` in the DSL. Non-instruction
//! builder calls (label allocation/binding, `get_current_address`,
//! `finalize`, …) remain plain method calls and interleave with the macro,
//! exactly as in the x86 backend.

use super::*;
use monoasm_macro::monoasm_arm64;

impl JitModule {
    /// Fetch the opcode and dispatch through the 256-entry table.
    ///
    /// x86: `movq r15,(table); movzxb rax,[r13+OPECODE]; addq r13,16;
    /// jmp [r15+rax*8]`. On aarch64 the pc is *not* pre-advanced, so handlers
    /// read operands at positive offsets and advance the pc themselves before
    /// re-dispatching.
    ///
    /// ### in
    /// - x21 (PC): bytecode pointer
    /// ### destroy
    /// - x9 (TBL), x10 (OP), x11 (TGT)
    pub(in crate::codegen) fn a64_fetch_and_dispatch(&mut self) {
        let table = self.dispatch.as_ptr() as u64;
        monoasm_arm64!(&mut self.jit,
            mov  x(TBL.0), (table);                       // table base
            ldrb x(OP.0), [x(PC.0), #(OPECODE)];          // opcode <- [pc + OPECODE]
            ldr  x(TGT.0), [x(TBL.0), x(OP.0), lsl #3];   // handler <- table[opcode]
            br   x(TGT.0);
        );
    }

    /// Address of the local slot whose index is in `reg`:
    /// `[r14 + reg*8 - LFP_SELF]`. x86: `negq R(r); lea R(r),[r14+R(r)*8-SELF]`.
    /// aarch64: `neg; add Xr,x_lfp,Xr,lsl #3; sub #LFP_SELF`.
    ///
    /// ### in / out
    /// - `dst`: slot index in → slot *address* out
    pub(in crate::codegen) fn a64_slot_addr(&mut self, dst: GReg) {
        monoasm_arm64!(&mut self.jit,
            neg x(dst.0), x(dst.0);
            add x(dst.0), x(LFP.0), x(dst.0), lsl #3;
            sub x(dst.0), x(dst.0), #(LFP_SELF);
        );
    }

    /// Value of the local slot whose index is in `reg` (slot index → value).
    pub(in crate::codegen) fn a64_slot_value(&mut self, dst: GReg) {
        self.a64_slot_addr(dst);
        monoasm_arm64!(&mut self.jit,
            ldr x(dst.0), [x(dst.0)];
        );
    }

    /// VM entry prologue: save the callee-saved global registers + fp/lr.
    /// x86 analogue: `pushq rbp; movq rbp, rsp`.
    pub(in crate::codegen) fn a64_prologue(&mut self) {
        monoasm_arm64!(&mut self.jit,
            stp x29, x30, [sp, #-16]!;                  // fp, lr
            stp x(EXEC.0), x(GLOBALS.0), [sp, #-16]!;
            stp x(PC.0), x(LFP.0), [sp, #-16]!;
            stp x(ACC.0), x(X19_PAD.0), [sp, #-16]!;    // ACC + 8-byte pad (16-align)
        );
    }

    /// VM exit epilogue: restore the callee-saved registers. x86: `leave; ret`.
    pub(in crate::codegen) fn a64_epilogue(&mut self) {
        monoasm_arm64!(&mut self.jit,
            ldp x(ACC.0), x(X19_PAD.0), [sp], #16;
            ldp x(PC.0), x(LFP.0), [sp], #16;
            ldp x(EXEC.0), x(GLOBALS.0), [sp], #16;
            ldp x29, x30, [sp], #16;
            ret;
        );
    }
    /// Bind `label` at the current position and emit a trap.
    pub(in crate::codegen) fn a64_brk_stub(&mut self, label: &DestLabel) {
        monoasm_arm64!(&mut self.jit,
        label:
            brk #0;
        );
    }

    /// Like `a64_brk_stub` but reports a diagnostic id before trapping.
    pub(in crate::codegen) fn a64_brk_stub_diag(&mut self, label: &DestLabel, id: u64) {
        monoasm_arm64!(&mut self.jit,
        label:
            mov x0, (id);
            mov x9, (crate::codegen::runtime::report_unimpl_op as *const () as u64);
            blr x9;
            brk #0;
        );
    }

    /// Emit a `brk` trampoline and return its address as a typed fn pointer.
    /// TODO(aarch64): replace each caller with the real invoker.
    pub(in crate::codegen) fn a64_stub_fn<T>(&mut self, id: u64) -> T {
        let p = self.jit.get_current_address();
        monoasm_arm64!(&mut self.jit,
            mov x0, (0x51410000 + id);                   // DIAG: stub invoker hit
            mov x9, (crate::codegen::runtime::report_unimpl_op as *const () as u64);
            blr x9;
            brk #0;
        );
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
        monoasm_arm64!(&mut jit,
            mov x0, x(OP.0);                             // OP (X10) holds the opcode at dispatch time
            mov x9, (crate::codegen::runtime::report_unimpl_op as *const () as u64);
            blr x9;
            brk #0;
        );
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
        // f64_to_val is bound later in `construct_vm` (a Codegen context):
        // `a64_gen_f64_to_val` is an `impl Codegen` helper and nothing in `new`
        // references the label yet, so leaving it unbound here is fine.
        let _ = &f64v;
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
        monoasm_arm64!(&mut self.jit,
            // alloc_flag += 10  (32-bit RMW)
            mov x0, (af_addr);
            ldr w1, [x0];
            add x1, x1, #10;
            str w1, [x0];
            // pending_signals |= bit  (32-bit RMW)
            mov x0, (ps_addr);
            ldr w1, [x0];
            mov x2, (bit);
            orr x1, x1, x2;
            str w1, [x0];
            ret;
        );
        p
    }
}
