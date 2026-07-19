use super::*;

const INIT_METHOD_OFS: i32 = -16;
const INIT_METHOD_ARG: i32 = -14;
const INIT_METHOD_REG: i32 = -12;

impl Codegen {
    /// Initialize method frame
    ///
    /// ### bytecode
    /// ~~~text
    /// -16 -14 -12 -10  -8  -6  -4  -2
    /// +---+---+---+---++---+---+---+---+
    /// |ofs|arg|reg| op||   |   |   |   |
    /// +---+---+---+---++---+---+---+---+
    ///  rsi rdi r15
    /// ~~~
    ///
    /// - +reg: a number of registers
    /// - +arg: a number of arguments.
    /// - +ofs: stack pointer offset
    ///
    pub(super) fn vm_init(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.vm_init_func();
        self.fill_nil();
        // Callee-entry GC/preempt poll. This is the one point every call
        // path funnels through — including the Rust invokers
        // (`invoke_method` / `invoke_block`), which have no call-site
        // poll — and the safest possible poll position: the frame is
        // fully linked, rsp is below it (no staging red zone), args are
        // in their slots and the remaining registers were just
        // nil-filled, so the GC root scan sees a completely consistent
        // frame. Closes the "JIT-compiled block body with no polls,
        // called from a native loop" preemption gap uniformly.
        self.vm_execute_gc();
        self.fetch_and_dispatch();
        label
    }

    ///
    /// ### in
    /// - r13: pc
    /// - r14: LFP
    ///
    /// ### out
    /// - rax: reg_num - arg_num
    /// - r15: reg_num
    ///
    /// ###  destroy
    /// - rax, rdi
    ///
    fn vm_init_func(&mut self) {
        monoasm! { &mut self.jit,
            // setup stack pointer
            movsxw rax, [r13 + (INIT_METHOD_OFS)];
            shlq rax, 4;
            subq rsp, rax;
            movzxw r15, [r13 + (INIT_METHOD_REG)];
            movq rax, r15;        // r15: reg_num
            subw rax, [r13 + (INIT_METHOD_ARG)];   // rax: reg_num - arg_num
        };
    }

    ///
    /// Fill NIL_VALUE to the slots from *r15* .. *r15* + *rax*
    ///
    /// ### in
    /// - rax: reg_num - arg_num
    /// - r15: reg_num
    ///
    fn fill_nil(&mut self) {
        let l0 = self.jit.label();
        let l1 = self.jit.label();
        self.jit.branch_if_captured(&l1);
        monoasm! { &mut self.jit,
            testq rax, rax;
            jz   l1;
            negq r15;
            lea  r15, [r14 + r15 * 8 - (LFP_ARG0)];
        l0:
            movq [r15 + rax * 8], (NIL_VALUE);
            subq rax, 1;
            jne  l0;
        l1:
        };
    }
}
