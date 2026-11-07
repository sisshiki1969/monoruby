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
        self.fill_destruct();
        self.clear_block_param();
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
    /// The run is `[r15 + 8, r15 + 8 + rax * 8)` once *r15* is rebased
    /// below the last register. An odd count stores its top slot alone;
    /// the rest goes two slots per 16-byte `movups` of the shared
    /// `Codegen::nil_pair` constant (a rip-relative load: nothing is read
    /// back from the frame being written).
    ///
    /// ### destroy
    /// - rax, r15, xmm0
    ///
    fn fill_nil(&mut self) {
        let l0 = self.jit.label();
        let l1 = self.jit.label();
        let even = self.jit.label();
        let nil_pair = self.nil_pair.clone();
        self.jit.branch_if_captured(&l1);
        monoasm! { &mut self.jit,
            testq rax, rax;
            jz   l1;
            negq r15;
            lea  r15, [r14 + r15 * 8 - (LFP_ARG0)];
            testq rax, 1;
            jz   even;
            movq [r15 + rax * 8], (NIL_VALUE);
            subq rax, 1;
            jz   l1;
        even:
            movups xmm0, [rip + nil_pair];
        l0:
            // slots `rax - 1` and `rax` (counted from r15).
            movups [r15 + rax * 8 - 8], xmm0;
            subq rax, 2;
            jne  l0;
        l1:
        };
    }

    ///
    /// Clear the named `&block` parameter's slot (`[r13 - 4]`, 0: none)
    /// to 0 (`None`): "not assigned, the frame's block handler is the
    /// value" for `BlockArg` / `BlockArgProxy`.
    ///
    /// ### destroy
    /// - rax
    ///
    fn clear_block_param(&mut self) {
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            movzxw rax, [r13 - 4];
            testq rax, rax;
            jz   exit;
            negq rax;
            movq [r14 + rax * 8 - (LFP_SELF)], 0;
        exit:
        };
    }

    ///
    /// Fill NIL_VALUE to the destructured-parameter slots (`|(a, b)|`).
    ///
    /// They live inside the argument area (so `fill_nil` misses them),
    /// and no caller writes them — the `expand` instructions after entry
    /// do — so without this the callee-entry GC poll marks stack garbage.
    /// The slot range rides in the instruction's second word:
    /// `destruct_start` at `[r13 - 8]`, `destruct_len` at `[r13 - 6]`.
    ///
    /// ### destroy
    /// - rax, rdi
    ///
    fn fill_destruct(&mut self) {
        let l0 = self.jit.label();
        let l1 = self.jit.label();
        self.jit.branch_if_captured(&l1);
        monoasm! { &mut self.jit,
            movzxw rdi, [r13 - 6];  // destruct_len
            testq rdi, rdi;
            jz   l1;
            movzxw rax, [r13 - 8];  // destruct_start
            negq rax;
            lea  rax, [r14 + rax * 8 - (LFP_ARG0)];  // &slot[destruct_start]
        l0:
            movq [rax], (NIL_VALUE);
            subq rax, 8;
            subq rdi, 1;
            jne  l0;
        l1:
        };
    }
}
