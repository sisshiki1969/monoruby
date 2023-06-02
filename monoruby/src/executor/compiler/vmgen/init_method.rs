use super::*;

impl Codegen {
    /// Initialize method frame
    ///
    /// ### bytecode
    /// ~~~text
    /// +6  +4  +2  +0   +14 +12 +10 +8
    /// +---+---+---+---++---+---+---+---+
    /// | op|reg|rop|ofs||arg|inf|blk|req|
    /// +---+---+---+---++---+---+---+---+
    /// ~~~
    ///
    /// - reg: a number of resisters
    /// - arg: a number of arguments.
    /// - ofs: stack pointer offset
    /// - req: a number of required arguments
    /// - reqopt: req + optional arguments
    /// - blk: a position of block argument (if not exists, 0.)
    /// - inf:
    ///
    /// ### registers
    ///
    /// - r15 <- reg
    /// - rdi <- pos
    /// - rsi <- ofs
    ///
    pub(super) fn vm_init(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch3();
        self.stack_setup();
        self.vm_init_func();
        self.fetch_and_dispatch();
        label
    }

    /// setup stack pointer.
    ///
    /// in
    /// rsi: stack_offset
    fn stack_setup(&mut self) {
        monoasm! { &mut self.jit,
            // setup stack pointer
            shlq rsi, 4;
            subq rsp, rsi;
        };
    }

    fn vm_init_func(&mut self) {
        let set_block = self.jit.label();
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
        // set block parameter
            movzxw rax, [r13 - 6];
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
            movzxw rdi, [r13 - 12]; // reg_num
            movq  rax, rdi;         // reg_num
            subw  rax, [r13 - 2];   // reg_num - arg_num
        };
        self.fill(NIL_VALUE);
    }

    /// fill *val* to the slots from *rdi* .. *rdi* + *rax*
    fn fill(&mut self, val: u64) {
        let l0 = self.jit.label();
        let l1 = self.jit.label();
        monoasm! { &mut self.jit,
            testq rax, rax;
            jz   l1;
            negq rdi;
            lea  rdi, [r14 + rdi * 8 - (LBP_ARG0)];
        l0:
            movq [rdi + rax * 8], (val);
            subq rax, 1;
            jne  l0;
        l1:
        };
    }
}
