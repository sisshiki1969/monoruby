use super::*;

const OFS: i32 = -16;
//const ROP: i32 = -14;
const REG: i32 = -12;
//const OP: i32 = -10;
//const REQ: i32 = -8;
const BLK: i32 = -6;
//const INF: i32 = -4;
const ARG: i32 = -2;

impl Codegen {
    /// Initialize method frame
    ///
    /// ### bytecode
    /// ~~~text
    /// -16 -14 -12 -10  -8  -6  -4  -2
    /// +---+---+---+---++---+---+---+---+
    /// |ofs|rop|reg| op||req|blk|   |arg|
    /// +---+---+---+---++---+---+---+---+
    ///  rsi rdi r15
    /// ~~~
    ///
    /// - +reg: a number of registers
    /// - +arg: a number of arguments.
    /// - +ofs: stack pointer offset
    /// - req: a number of required arguments
    /// - rop: req + optional arguments
    /// - +blk: a position of block argument (if not exists, 0.)
    ///
    pub(super) fn vm_init(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.stack_setup();
        self.vm_init_func();
        self.fill(NIL_VALUE);
        self.fetch_and_dispatch();
        label
    }

    ///
    /// Setup stack pointer.
    ///
    /// ###  destroy
    /// - rax
    ///
    fn stack_setup(&mut self) {
        monoasm! { &mut self.jit,
            // setup stack pointer
            movsxw rax, [r13 + (OFS)];
            shlq rax, 4;
            subq rsp, rax;
        };
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
        let set_block = self.jit.label();
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            // set block parameter
            movzxw rax, [r13 + (BLK)];  // blk
            testq rax, rax;
            jz exit;
            movq rdi, [r14 - (LBP_BLOCK)];
            testq rdi, rdi;
            jnz set_block;
            movq rdi, (NIL_VALUE);
        set_block:
            negq rax;
            movq [r14 + rax * 8 - (LBP_SELF)], rdi;
        exit:
            movzxw r15, [r13 + (REG)];
            movq rax, r15;        // r15: reg_num
            subw rax, [r13 + (ARG)];   // rax: reg_num - arg_num
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
        monoasm! { &mut self.jit,
            testq rax, rax;
            jz   l1;
            negq r15;
            lea  r15, [r14 + r15 * 8 - (LBP_ARG0)];
        l0:
            movq [r15 + rax * 8], (val);
            subq rax, 1;
            jne  l0;
        l1:
        };
    }
}
