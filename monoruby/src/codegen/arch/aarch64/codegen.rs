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
            sub x2, x(LFP.0), #(LFP_META as u32);
            ldr x2, [x2];  // meta = [LFP - LFP_META]
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

    /// VM-side GC/signal poll. If `alloc_flag >= 8` (the signal handler nudges
    /// it by 10), call `exec_gc` which drains pending signals + runs GC. The
    /// hot path is two loads, a compare, and a fall-through branch.
    pub(in crate::codegen) fn a64_vm_execute_gc(&mut self) {
        let gc = self.exec_gc.clone();
        let skip = self.jit.label();
        let af_addr = self.jit.get_label_address(&self.alloc_flag).as_ptr() as u64;
        monoasm_arm64!(&mut self.jit,
            mov x10, (af_addr);
            ldr w11, [x10];
            cmp x11, #(8);
        );
        self.jit.bcond_label(Cond::Lt, &skip);
        monoasm_arm64!(&mut self.jit,
            bl gc;
            skip:
        );
    }
}
