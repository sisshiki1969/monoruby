use super::*;

impl Codegen {
    /// Initialize method frame
    ///
    /// ### bytecode
    /// ~~~text
    /// +6  +4  +2  +0   +14 +12 +10 +8
    /// +---+---+---+---++---+---+---+---+
    /// | op|reg|rop|ofs||   |blk|pos|req|
    /// +---+---+---+---++---+---+---+---+
    /// ~~~
    ///
    /// - reg: a number of resisters
    /// - ofs: stack pointer offset
    /// - req: a number of required arguments
    /// - reqopt: a number of positional arguments (req + opt)
    /// - pos: a number of arguments (req + opt + rest)
    /// - blk: position of block parameter
    ///
    /// ### registers
    ///
    /// - r15 <- reg
    /// - rdi <- pos
    /// - rsi <- ofs
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
        let fill_req = self.jit.label();
        let fill_opt = self.jit.label();
        let set_rest_empty = self.jit.label();
        let fill_temp = self.jit.label();
        let err = self.wrong_argument;
        monoasm! { self.jit,
        // in
        // r15: reg_num
        // rdi: reqopt_num
        // [R13 - 14]: reqopt_num
        // [R13 - 12]: reg_num
        // [R13 - 8]: req_num
        // [R13 - 6]: pos_num
        // [R13 - 4]: block_pos
        // rdx: number of args passed from caller
        // destroy
        // r15, caller-save registers
            movzxw r15, [r13 - 14];
            negq r15;
            lea  r15, [r14 + r15 * 8 - (LBP_ARG0)];
            cmpw rdx, rdi;
            // if passed == reqopt, required and optional param was done. goto rest param handling.
            jeq  set_rest_empty;
            // if passed < reqopt, we must fill nil to residual required/optional params.
            jlt  fill_req;
            // in the case of passed > reqopt
            // does rest param exists?
            movzxw rax, [r13 - 6];
            cmpw rax, [r13 - 14];
        }
        if is_block {
            monoasm! { self.jit, jeq  fill_temp; }
        } else {
            // if passed_args > pos_num && no rest parameter then goto err
            monoasm! { self.jit, jeq  err; }
        }
        monoasm! { self.jit,
        // set rest parameter.
          movl rsi, rdx;
          subl rsi, rdi;
          movq rdi, r15;
          // This is necessary because *make_rest_array* may destroy values under the sp
          // when the number of arguments passed > the number of registers.
          // TODO: this workaround may cause an error if the number of excess arguments passed exceeds 128.
          subq rsp, 1024;
          movq rax, (make_rest_array);
          call rax;
          addq rsp, 1024;
          subq r15, 8;
          jmp  fill_temp;
        }
        monoasm! { self.jit,
        fill_req:
          cmpw rdx, [r13 - 8];
        }
        if is_block {
            // fill nil to residual required arguments.
            monoasm! { self.jit,
            // if passed_args >= req then goto fill_opt
                jge  fill_opt;
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
        fill_opt:
        // fill zero to residual locals.
        // rax = reqopt - max(passed_args, req_num)
            movl rax, rdi;
            subl rax, rdx;
            movl rdx, rdi;
        }
        self.fill(2 /* rdx */, 0);
        monoasm! { self.jit,
        set_rest_empty:
        // set rest parameter to empty Array.
            movzxw rax, [r13 - 6];
            cmpw rax, [r13 - 14];
            jeq  fill_temp;
            movq rdi, r15;
            xorq rsi, rsi;
            movq rax, (make_rest_array);
            call rax;
            subq r15, 8;
        fill_temp:
        }
        let set_block = self.jit.label();
        let exit = self.jit.label();
        let l0 = self.jit.label();
        let l1 = self.jit.label();
        monoasm! { self.jit,
            cmpw [r13 - 4], 0;
            jeq  exit;
            movq rdi, [r14 - (LBP_BLOCK)];
            testq rdi, rdi;
            jnz set_block;
            movq rdi, (NIL_VALUE);
        set_block:
            movq [r15], rdi;
            subq r15, 8;
        exit:
            // fill nil to temporary registers.
            // rax = [reg_num]
            movzxw rax, [r13 - 12];
            negq rax;
            lea  rax, [r14 + rax * 8 - (LBP_ARG0)];
        l0:
            cmpq rax, r15;
            je  l1;
            movq [r15], (NIL_VALUE);
            subq r15, 8;
            jmp  l0;
        l1:
        };
        //self.fill(2 /* rdx */, NIL_VALUE);
    }

    /// fill *val* to the slots from *ptr* to *ptr* + rax - 1
    fn fill(&mut self, ptr: u64, val: u64) {
        let l0 = self.jit.label();
        let l1 = self.jit.label();
        monoasm! { self.jit,
            testq rax, rax;
            jz   l1;
            negq R(ptr);
            lea  R(ptr), [r14 + R(ptr) * 8 - (LBP_ARG0)];
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
    }
}
