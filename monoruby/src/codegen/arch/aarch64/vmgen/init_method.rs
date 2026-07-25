use super::*;
use monoasm_macro::monoasm_arm64;

impl Codegen {

    /// op 172 `init_method`: allocate the method's stack frame and nil-fill the
    /// uninitialized local slots. Bytecode (relative to instruction start):
    /// `+0` stack-offset, `+2` arg_num, `+4` reg_num. (x86 `vm_init`.)
    pub(in crate::codegen) fn a64_op_init_method(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        let loop_ = self.jit.label();
        // allocate stack: sp -= stack_offset * 16
        monoasm_arm64!(&mut self.jit,
            ldrh x10, [x(PC.0)];
            lsl x10, x10, #(4);
            mov x13, sp;  // sp -= X10 (A64 sub can't take SP as a
            sub x13, x13, x10;  // shifted-reg operand, so via a GPR)
            mov sp, x13;
        // Skip the nil-fill for a captured (on-heap / invalidated) frame.
        // Its locals live on the heap and may already hold values written in
        // by `new_binding_frame` — e.g. a binding-eval frame that introduced a
        // brand-new local in a previous eval; nil-filling would wipe it.
        // Mirrors x86 `fill_nil`'s leading `branch_if_captured`. The `kind`
        // byte sits at `[LFP - (LFP_META - META_KIND)]`; bit 7 = on_heap,
        // bit 3 = invalidated.
            sub x10, x(LFP.0), #((LFP_META - META_KIND as i32) as u32);
            ldrb x13, [x10];
            tbnz x13, #(7), skip;  // on_heap
            tbnz x13, #(3), skip;  // invalidated
        // count = reg_num - arg_num
            ldrh x15, [x(PC.0), #(4)];  // reg_num
            ldrh x11, [x(PC.0), #(2)];  // arg_num
            sub x12, x15, x11;
            cbz x12, skip;
        // base = lfp - reg_num*8 - LFP_ARG0 ; fill [base + count*8] downward
            neg x15, x15;
            add x15, x(LFP.0), x15, lsl #(3);
            sub x15, x15, #(LFP_ARG0 as u32);
            mov x14, (NIL_VALUE);
            loop_:
            add x10, x15, x12, lsl #(3);
            str x14, [x10];
            sub x12, x12, #(1);
            cbnz x12, loop_;
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        // Callee-entry GC/preempt poll, mirroring x86 `vm_init`: frame
        // linked, sp adjusted, locals nil-filled — a fully
        // frame-consistent poll point on every entry path (including
        // the poll-free Rust invokers).
        self.a64_vm_execute_gc();
        self.a64_fetch_and_dispatch();
        p
    }
}
