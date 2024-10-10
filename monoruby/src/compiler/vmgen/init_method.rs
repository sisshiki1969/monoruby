use super::*;

const INIT_METHOD_OFS: i32 = -16;
const INIT_METHOD_REG: i32 = -12;
const INIT_METHOD_ARG: i32 = -2;

impl Codegen {
    /// Initialize method frame
    ///
    /// ### bytecode
    /// ~~~text
    /// -16 -14 -12 -10  -8  -6  -4  -2
    /// +---+---+---+---++---+---+---+---+
    /// |ofs|rop|reg| op||req|   |   |arg|
    /// +---+---+---+---++---+---+---+---+
    ///  rsi rdi r15
    /// ~~~
    ///
    /// - +reg: a number of registers
    /// - +arg: a number of arguments.
    /// - +ofs: stack pointer offset
    /// - req: a number of required arguments
    /// - rop: req + optional arguments
    ///
    pub(super) fn vm_init(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.vm_init_func();
        self.fill(NIL_VALUE);
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
    /// Fill *val* to the slots from *r15* .. *r15* + *rax*
    ///
    /// ### in
    /// - rax: reg_num - arg_num
    /// - r15: reg_num
    ///
    fn fill(&mut self, val: u64) {
        let l0 = self.jit.label();
        let l1 = self.jit.label();
        self.test_heap_frame();
        monoasm! { &mut self.jit,
            jnz l1;
            testq rax, rax;
            jz   l1;
            negq r15;
            lea  r15, [r14 + r15 * 8 - (LFP_ARG0)];
        l0:
            movq [r15 + rax * 8], (val);
            subq rax, 1;
            jne  l0;
        l1:
        };
    }
}
