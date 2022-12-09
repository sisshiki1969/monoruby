use super::*;

impl Codegen {
    /// Initialize method frame
    ///
    /// ~~~text
    /// +---+---+---+---++---+---+---+---+
    /// | op|reg|arg|ofs||       |       |
    /// +---+---+---+---++---+---+---+---+
    ///
    /// reg: a number of resisters
    /// arg: a number of arguments
    /// ofs: stack pointer offset
    ///  /// ~~~
    pub(super) fn vm_init_method(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.stack_setup();
        self.init_method();
        self.fetch_and_dispatch();
        label
    }

    pub(super) fn vm_init_block(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.stack_setup();
        self.expand_arg0();
        self.init_method();
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

    fn init_method(&mut self) {
        let l1 = self.jit.label();
        let l2 = self.jit.label();
        let l3 = self.jit.label();
        // in
        // r15: reg_num (except *self*)
        // rdi: arg_num
        // rdx: number of args passed from caller
        // destroy
        // rax, rdx, r15
        monoasm! { self.jit,
            subl r15, 1;
            // if passed_args >= arg_num then goto l1
            cmpw rdx, rdi;
            jge  l1;
            cmpw rdx, [r13 - 8];
            jge  l2;
            movzxw rcx, [r13 - 8];
            movl rax, rcx;
            subl rax, rdx;
        }
        self.fill(1 /* rcx */, NIL_VALUE);
        monoasm! { self.jit,
            movzxw rdx, [r13 - 8];
        l2:
        // rax = arg_num - passed_args
            movl rax, rdi;
            subl rax, rdx;
        // fill zero to residual locals.
            movl rdx, rdi;
        }
        self.fill(2 /* rdx */, 0);
        monoasm! { self.jit,
        l1:
        // fill nil to temporary registers.
        // rax = reg_num - 1 - arg_num
            movq rax, r15;
            subq rax, rdi;
            jz   l3;
        }
        self.fill(15 /* r15 */, NIL_VALUE);
        monoasm! { self.jit,
        l3:
        };
    }

    /// fill *val* to the slots from *ptr* to *ptr* + rax - 1
    fn fill(&mut self, ptr: u64, val: u64) {
        let l0 = self.jit.label();
        monoasm! { self.jit,
            negq R(ptr);
            lea  R(ptr), [rbp + R(ptr) * 8 - (OFFSET_ARG0)];
        l0:
            movq [R(ptr) + rax * 8], (val);
            subq rax, 1;
            jne  l0;
        };
    }

    /// Expand arg0 if the number of args is 1 and arg0 is Array and arg_num > 1.
    ///
    /// in
    /// rdi: arg_num
    /// rdx: number of args passed from caller
    /// out
    /// rdx: number of args
    /// destroy
    /// rax
    fn expand_arg0(&mut self) {
        let l1 = self.jit.label();
        monoasm! { self.jit,
            // if passed_arg == 1 && arg0 isArray && arg_num >= 2 then expand arg0.
            cmpl rdx, 1;
            jne  l1;
            cmpl rdi, 2;
            jlt  l1;
            movq rax, [rbp - (OFFSET_ARG0)];
            testq rax, 0b111;
            jnz  l1;
            cmpl [rax + 4], (ARRAY_CLASS.0);
            jne  l1;
            pushq rdi;
            pushq rsi;
            movq rdx, rdi;
            movq rdi, rax;
            lea  rsi, [rbp - (OFFSET_ARG0)];
            movq rax, (expand_array);
            call rax;
            movq rdx, rax;
            popq rsi;
            popq rdi;
        l1:
        }
    }
}
