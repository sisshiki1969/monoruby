//! aarch64 `Codegen` asm helpers: entry-raise / stack-overflow /
//! stack-check / GC-poll code generation.
//!
//! Counterpart of `arch/x86_64/codegen.rs`.

use super::*;

impl Codegen {
    /// entry_raise: error/exception dispatch. Calls `handle_error`, which
    /// returns (value, dest): if `dest` is Some, resume execution there (a
    /// rescue/ensure/retry target); otherwise unwind this VM frame and
    /// return the error (None) to the caller (x86 `init`'s `raise:` block).
    pub(in crate::codegen) fn a64_gen_entry_raise(&mut self) {
        let raise = self.entry_raise.clone();
        let goto = self.jit.label();
        self.jit.bind_label(raise);
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.sub_imm(X2, LFP, LFP_META as u32, 0);
        self.jit.ldr(X2, X2, 0); // meta = [LFP - LFP_META]
        self.jit.mov(X3, PC); // pc = current instruction
        self.jit
            .mov_imm(X9, crate::codegen::jit_module::handle_error as *const () as u64);
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

    /// Bind `vm_stack_overflow`: call `stack_overflow(EXEC)` to set the
    /// SystemStackError, then branch into `entry_raise` which unwinds the VM
    /// frame and returns the error to the Rust caller.
    pub(in crate::codegen) fn a64_gen_stack_overflow(&mut self) {
        let ovf = self.vm_stack_overflow.clone();
        let raise = self.entry_raise.clone();
        self.jit.bind_label(ovf);
        self.jit.mov(X0, EXEC);
        self.jit
            .mov_imm(X9, super::stack_overflow as *const () as u64);
        self.jit.blr(X9);
        self.jit.b_label(&raise);
    }

    /// Compare SP to `executor.stack_limit`; if SP <= limit, branch to
    /// `vm_stack_overflow`. Uses X10 and X11 as scratch. EXEC must be valid.
    pub(in crate::codegen) fn a64_check_stack(&mut self) {
        let ovf = self.vm_stack_overflow.clone();
        self.jit.mov_sp(X10, SP);
        self.jit.ldr(X11, EXEC, EXECUTOR_STACK_LIMIT as u32);
        self.jit.cmp(X10, X11);
        self.jit.bcond_label(Cond::Le, &ovf);
    }

    /// Bind `exec_gc`: call `executor::execute_gc(vm, globals)` (which also
    /// drains pending signals). On error (returns None / X0 == 0) branch to
    /// `entry_raise`; otherwise return to the caller. VM globals X19-X24 are
    /// callee-saved across the Rust call so we only need to preserve fp/lr.
    pub(in crate::codegen) fn a64_gen_exec_gc(&mut self) {
        let gc = self.exec_gc.clone();
        let raise = self.entry_raise.clone();
        self.jit.bind_label(gc);
        self.jit.stp_pre(X29, X30, SP, -16);
        self.jit.mov_sp(X29, SP);
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit
            .mov_imm(X9, crate::executor::execute_gc as *const () as u64);
        self.jit.blr(X9);
        self.jit.mov_sp(SP, X29);
        self.jit.ldp_post(X29, X30, SP, 16);
        // Option<Value>: None (0) → raise; else continue.
        let ok = self.jit.label();
        self.jit.cbnz_label(X0, &ok);
        self.jit.b_label(&raise);
        self.jit.bind_label(ok);
        self.jit.ret();
    }

    /// VM-side GC/signal poll. If `alloc_flag >= 8` (the signal handler nudges
    /// it by 10), call `exec_gc` which drains pending signals + runs GC. The
    /// hot path is two loads, a compare, and a fall-through branch.
    pub(in crate::codegen) fn a64_vm_execute_gc(&mut self) {
        let gc = self.exec_gc.clone();
        let skip = self.jit.label();
        let af_addr = self.jit.get_label_address(&self.alloc_flag).as_ptr() as u64;
        self.jit.mov_imm(X10, af_addr);
        self.jit.ldr32(X11, X10, 0);
        self.jit.cmp_imm(X11, 8, 0);
        self.jit.bcond_label(Cond::Lt, &skip);
        self.jit.bl_label(&gc);
        self.jit.bind_label(skip);
    }
}
