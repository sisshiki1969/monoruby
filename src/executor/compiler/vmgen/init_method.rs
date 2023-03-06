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
    /// - rdx <- passed args
    ///
    pub(super) fn vm_init_method(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.stack_setup();
        self.vm_init_func(false);
        self.fetch_and_dispatch();
        label
    }

    pub(super) fn vm_init_block(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.stack_setup();
        self.vm_init_func(true);
        self.fetch_and_dispatch();
        label
    }

    /// setup stack pointer.
    ///
    /// in
    /// rsi: stack_offset
    fn stack_setup(&mut self) {
        monoasm! { self.jit,
            // setup stack pointer
            shlq rsi, 4;
            subq rsp, rsi;
        };
    }

    fn vm_init_func(&mut self, is_block: bool) {
        let l1 = self.jit.label();
        let err = self.wrong_argument;
        if !is_block {
            monoasm! { self.jit,
            // in
            // r15: reg_num
            // rdi: reqopt_num
            // rdx: number of args passed from caller
            // [R13 - 14]: reqopt_num
            // [R13 - 12]: reg_num
            // [R13 -  8]: req_num
            // [R13 -  6]: block_pos
            // [R13 -  4]: info      bit 0:rest(yes=1 no =0) bit 1:block
            // [R13 -  2]: arg_num
            // destroy
            // r15, caller-save registers
                // if passed < req, go err.
                cmpw rdx, [r13 - 8];
                jeq  l1;
                jlt  err;
                cmpw rdx, rdi;
                // if passed <= reqopt, pass.
                jle l1;
                // in the case of passed > reqopt
                // if rest does not exists, go err.
                movzxw rax, [r13 - 4];
                testq rax, 0b1;
                jz err;
            l1:
            }
        }
        let set_block = self.jit.label();
        let exit = self.jit.label();
        monoasm! { self.jit,
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
        monoasm! { self.jit,
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

    /*
    /// Expand arg0 if the number of args is 1 and arg0 is Array and pos_num > 1.
    ///
    /// in
    /// rdi: pos_num
    /// rdx: number of args passed from caller
    /// out
    /// rdx: number of args
    /// destroy
    /// rax
    fn expand_arg0(&mut self) {
        let l1 = self.jit.label();
        monoasm! { self.jit,
            // if passed_arg == 1 && arg0 isArray && pos_num >= 2 then expand arg0.
            cmpl rdx, 1;
            jne  l1;
            cmpl rdi, 2;
            jlt  l1;
            movq rax, [r14 - (LBP_ARG0)];
            testq rax, 0b111;
            jnz  l1;
            cmpl [rax + 4], (ARRAY_CLASS.0);
            jne  l1;
            pushq rdi;
            pushq rsi;
            movzxw rdx, [r13 - 8];
            movq rdi, rax;
            lea  rsi, [r14 - (LBP_ARG0)];
            movq rax, (block_expand_array);
            call rax;
            movq rdx, rax;
            popq rsi;
            popq rdi;
        l1:
        }
    }*/
}
