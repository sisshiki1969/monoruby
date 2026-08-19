//! aarch64 `Codegen` asm helpers: entry-raise / stack-overflow /
//! stack-check / GC-poll code generation.
//!
//! Counterpart of `arch/x86_64/codegen.rs`.

use super::*;
use monoasm_macro::monoasm_arm64;

impl Codegen {
    /// entry_raise: error/exception dispatch. Calls `handle_error`, which
    /// returns (value, dest): if `dest` is Some, resume execution there (a
    /// rescue/ensure/retry target); otherwise unwind this VM frame and
    /// return the error (None) to the caller (x86 `init`'s `raise:` block).
    pub(in crate::codegen) fn a64_gen_entry_raise(&mut self) {
        let raise = self.entry_raise.clone();
        let goto = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            raise:
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldur x2, [x(LFP.0), #(-(LFP_META as i32))];  // meta = [LFP - LFP_META]
            mov x3, x(PC.0);  // pc = current instruction
            mov x9, (crate::codegen::jit_module::handle_error as *const () as u64);
            blr x9;
        // x0 = value (Option<Value>), x1 = dest (Option<BytecodePtr>)
            cbnz x1, goto;
        // no handler: unwind this frame and return the error (x0 = None)
            mov sp, x29;
            ldp x29, x30, [sp], #(16);
            ret;
            goto:
            mov x(PC.0), x1;  // resume at the handler pc
        );
        self.a64_fetch_and_dispatch();
    }

    /// Bind `vm_stack_overflow`: call `stack_overflow(EXEC)` to set the
    /// SystemStackError, then branch into `entry_raise` which unwinds the VM
    /// frame and returns the error to the Rust caller.
    pub(in crate::codegen) fn a64_gen_stack_overflow(&mut self) {
        let ovf = self.vm_stack_overflow.clone();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            ovf:
            mov x0, x(EXEC.0);
            mov x9, (super::stack_overflow as *const () as u64);
            blr x9;
            b raise;
        );
    }

    /// Compare SP to `executor.stack_limit`; if SP <= limit, branch to
    /// `vm_stack_overflow`. Uses X10 and X11 as scratch. EXEC must be valid.
    pub(in crate::codegen) fn a64_check_stack(&mut self) {
        let ovf = self.vm_stack_overflow.clone();
        monoasm_arm64!(&mut self.jit,
            mov x10, sp;
            ldr x11, [x(EXEC.0), #(EXECUTOR_STACK_LIMIT as u32)];
            cmp x10, x11;
        );
        self.jit.bcond_label(Cond::Le, &ovf);
    }

    /// Bind `exec_gc`: call `executor::execute_gc(vm, globals)` (which also
    /// drains pending signals). On error (returns None / X0 == 0) branch to
    /// `entry_raise`; otherwise return to the caller. VM globals X19-X24 are
    /// callee-saved across the Rust call so we only need to preserve fp/lr.
    pub(in crate::codegen) fn a64_gen_exec_gc(&mut self) {
        let gc = self.exec_gc.clone();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            gc:
            stp x29, x30, [sp, #(-16)]!;
            mov x29, sp;
            // Red-zone cushion: the poll can fire while frame data still
            // lives just below sp (a staged callee frame); drop sp past it
            // so the Rust callee's stack frames cannot overwrite it.
            // Mirrors the x86-64 exec_gc stub's `subq rsp, 1032`.
            sub sp, sp, #(1024);
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (crate::executor::execute_gc as *const () as u64);
            blr x9;
            mov sp, x29;
            ldp x29, x30, [sp], #(16);
        // Option<Value>: None (0) → raise; else continue.
        );
        let ok = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            cbnz x0, ok;
            b raise;
            ok:
            ret;
        );
    }

    /// Bind `write_barrier`: the shared out-of-line half of the generational
    /// GC write barrier (see the field doc in `codegen.rs`). Parent in x9;
    /// saves the abstract scratch GPs (x0-x8), the caller-saved FP regs
    /// (d0-d7) and the `bl`-written return address, calls
    /// `jit_module::jit_write_barrier(parent)`, restores everything, and
    /// returns. The *site* preserves its own live x30 around the `bl`, so
    /// the stub only has to keep the return address alive across its inner
    /// `blr`.
    pub(in crate::codegen) fn a64_gen_write_barrier(&mut self) {
        let wb = self.write_barrier.clone();
        let f = crate::codegen::jit_module::jit_write_barrier as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            wb:
            sub sp, sp, #(144);
            str x0, [sp, #(0)];
            str x1, [sp, #(8)];
            str x2, [sp, #(16)];
            str x3, [sp, #(24)];
            str x4, [sp, #(32)];
            str x5, [sp, #(40)];
            str x6, [sp, #(48)];
            str x7, [sp, #(56)];
            str x8, [sp, #(64)];
            str d0, [sp, #(72)];
            str d1, [sp, #(80)];
            str d2, [sp, #(88)];
            str d3, [sp, #(96)];
            str d4, [sp, #(104)];
            str d5, [sp, #(112)];
            str d6, [sp, #(120)];
            str d7, [sp, #(128)];
            str x30, [sp, #(136)];
            mov x0, x9;                // parent -> arg0
            mov x9, (f);
            blr x9;
            ldr x30, [sp, #(136)];
            ldr d7, [sp, #(128)];
            ldr d6, [sp, #(120)];
            ldr d5, [sp, #(112)];
            ldr d4, [sp, #(104)];
            ldr d3, [sp, #(96)];
            ldr d2, [sp, #(88)];
            ldr d1, [sp, #(80)];
            ldr d0, [sp, #(72)];
            ldr x8, [sp, #(64)];
            ldr x7, [sp, #(56)];
            ldr x6, [sp, #(48)];
            ldr x5, [sp, #(40)];
            ldr x4, [sp, #(32)];
            ldr x3, [sp, #(24)];
            ldr x2, [sp, #(16)];
            ldr x1, [sp, #(8)];
            ldr x0, [sp, #(0)];
            add sp, sp, #(144);
            ret;
        );
    }

    /// `f64_to_val`: convert the f64 in `D0` to a boxed `Value` in `X0` —
    /// flonum-encode when the exponent is in range, else heap-allocate a
    /// `Float`. Mirrors x86 `gen_f64_to_val`. Called via `bl` from `FprToStack`.
    ///
    /// `and`/`orr` have no immediate form and there is no `ror` in the
    /// `monoasm_arm64!` macro, so the bit-twiddling uses shift + register-logical
    /// ops (rotate = `lsl`/`lsr`/`orr`; clear-low-2 = `lsr #2; lsl #2`).
    pub(in crate::codegen) fn a64_gen_f64_to_val(&mut self, label: &DestLabel) {
        let normal = self.jit.label();
        let heap = self.jit.label();
        self.jit.bind_label(label.clone());
        monoasm_arm64!(&mut self.jit,
            fcmp d0, #0.0;           // compare D0 with zero
        );
        self.jit.bcond_label(Cond::Ne, &normal); // != 0.0 (or NaN) -> normal
        monoasm_arm64!(&mut self.jit,
            mov x0, (FLOAT_ZERO);
            ret;
            normal:
            fmov x0, d0;             // x0 = bits(d0)
            lsr x1, x0, #(60);
            add x1, x1, #(1);
            mov x9, (6);
            and x1, x1, x9;
            cmp x1, #(4);
        );
        self.jit.bcond_label(Cond::Ne, &heap); // exponent out of flonum range -> heap
        monoasm_arm64!(&mut self.jit,
            // flonum-encode: rol 3, clear low 2 bits, set bit1 (0b10).
            rol x0, x0, #(3);
            lsr x0, x0, #(2);
            lsl x0, x0, #(2);        // clear low 2 bits (and -4)
            mov x9, (2);
            orr x0, x0, x9;          // set 0b10
            ret;
            heap:
            // Heap-allocate. Save the caller-saved FP pool (D2-D7); D8-D15 are
            // AAPCS64 callee-saved and preserved by float_heap. D0 still holds
            // the f64 argument that float_heap reads.
            str x30, [sp, #(-16)]!;
            sub sp, sp, #(48);
            str d2, [sp];
            str d3, [sp, #(8)];
            str d4, [sp, #(16)];
            str d5, [sp, #(24)];
            str d6, [sp, #(32)];
            str d7, [sp, #(40)];
            mov x9, (Value::float_heap as *const () as u64);
            blr x9;                  // x0 = Value::float_heap(d0)
            ldr d2, [sp];
            ldr d3, [sp, #(8)];
            ldr d4, [sp, #(16)];
            ldr d5, [sp, #(24)];
            ldr d6, [sp, #(32)];
            ldr d7, [sp, #(40)];
            add sp, sp, #(48);
            ldr x30, [sp], #(16);
            ret;
        );
    }

    /// VM-side safepoint poll. If any lane of the poll word is set (GC
    /// request, preempt tick, pending signal — see poll_flag.rs), call
    /// `exec_gc`. The hot path is a load and a fall-through `cbz`.
    pub(in crate::codegen) fn a64_vm_execute_gc(&mut self) {
        let gc = self.exec_gc.clone();
        let skip = self.jit.label();
        let pf_addr = self.jit.get_label_address(&self.poll_flag).as_ptr() as u64;
        monoasm_arm64!(&mut self.jit,
            mov x10, (pf_addr);
            ldr w11, [x10];       // zero-extends into x11
            cbz x11, skip;
            bl gc;
            skip:
        );
    }
}
