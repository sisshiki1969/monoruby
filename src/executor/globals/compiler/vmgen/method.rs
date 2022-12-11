use super::*;

impl Codegen {
    /// Initialize method frame
    ///
    /// ~~~text
    /// +---+---+---+---++---+---+---+---+
    /// | op|reg|arg|ofs||req|   |       |
    /// +---+---+---+---++---+---+---+---+
    ///
    /// reg: a number of resisters
    /// arg: a number of arguments
    /// ofs: stack pointer offset
    ///  /// ~~~
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
        self.expand_arg0();
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
        let l2 = self.jit.label();
        let l3 = self.jit.label();
        let err = self.wrong_argument;
        // in
        // r15: reg_num (except *self*)
        // rdi: pos_num
        // [R13 - 8]: req_num
        // rdx: number of args passed from caller
        // destroy
        // rax, rdx, r15
        monoasm! { self.jit,
          subl r15, 1;
          cmpw rdx, rdi;
        }
        if is_block {
            // if passed_args >= pos_num then goto l1
            monoasm! { self.jit,
              jge  l1;
            }
        } else {
            // if passed_args == pos_num then goto l1
            // if passed_args > pos_num then goto err
            monoasm! { self.jit,
              jeq  l1;
              jgt  err;
            }
        }
        monoasm! { self.jit,
          cmpw rdx, [r13 - 8];
        }
        if is_block {
            // fill zero to residual required arguments.
            monoasm! { self.jit,
            // if passed_args >= req_num then goto l2
                jge  l2;
                movzxw rcx, [r13 - 8];
                movl rax, rcx;
                subl rax, rdx;
            }
            self.fill(1 /* rcx */, NIL_VALUE);
            monoasm! { self.jit,
                movzxw rdx, [r13 - 8];
            }
        } else {
            // if passed_args < req_num then raise error.
            monoasm! { self.jit,
                jlt  err;
            }
        }
        monoasm! { self.jit,
        l2:
        // fill zero to residual locals.
        // rax = pos_num - max(passed_args, req_num)
            movl rax, rdi;
            subl rax, rdx;
            movl rdx, rdi;
        }
        self.fill(2 /* rdx */, 0);
        monoasm! { self.jit,
        l1:
        // fill nil to temporary registers.
        // rax = reg_num - 1 - pos_num
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
        let l1 = self.jit.label();
        monoasm! { self.jit,
            testq rax, rax;
            jz   l1;
            negq R(ptr);
            lea  R(ptr), [rbp + R(ptr) * 8 - (OFFSET_ARG0)];
        l0:
            movq [R(ptr) + rax * 8], (val);
            subq rax, 1;
            jne  l0;
        l1:
        };
    }

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
