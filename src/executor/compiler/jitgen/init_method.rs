use super::*;

impl Codegen {
    pub(super) fn prologue(&mut self, pc: BcPc, fnstore: &FnStore) {
        monoasm!(self.jit,
            pushq rbp;
            movq rbp, rsp;
        );
        match pc.get_ir(fnstore) {
            TraceIr::InitMethod(fn_info) => {
                self.setup_stack(fn_info.stack_offset);
                self.init_func(&fn_info, pc, false);
            }
            TraceIr::InitBlock(fn_info) => {
                self.setup_stack(fn_info.stack_offset);
                self.init_func(&fn_info, pc, true);
            }
            _ => unreachable!(),
        }
    }

    fn setup_stack(&mut self, stack_offset: usize) {
        monoasm!(self.jit,
            subq rsp, (stack_offset * 16);
        );
    }

    fn init_func(&mut self, fn_info: &FnInitInfo, pc: BcPc, is_block: bool) {
        let FnInitInfo {
            reg_num,
            arg_num,
            reqopt_num,
            req_num,
            block_pos,
            ..
        } = *fn_info;
        if !is_block {
            let err = self.jit.label();
            self.jit.select_page(1);
            let argument_err = self.wrong_argument;
            monoasm! { self.jit,
            err:
                movq r13, ((pc+1).get_u64());
                jmp  argument_err;
            }
            self.jit.select_page(0);

            // rdx: number of args passed from caller
            let has_rest_param = fn_info.has_rest_param();

            let l1 = self.jit.label();
            monoasm! { self.jit,
                // if passed < req, go err.
                cmpw rdx, (req_num);
                jeq  l1;
                jlt  err;
            }
            if !has_rest_param {
                monoasm! { self.jit,
                    // in the case of passed > reqopt
                    // if rest does not exists, go err.
                    cmpw rdx, (reqopt_num);
                    jgt err;
                };
            }
            monoasm! { self.jit,
            l1:
            };
        }

        if fn_info.has_block_param() {
            monoasm! { self.jit,
                movq rax, [r14 - (LBP_BLOCK)];
                movq [r14 - (block_pos as i32 * 8 + LBP_ARG0)], rax;
            }
        }

        // fill nil to temporary registers.
        let clear_len = reg_num - arg_num;
        if clear_len > 2 {
            monoasm!(self.jit,
                movq rax, (NIL_VALUE);
            );
            for i in 0..clear_len {
                monoasm!(self.jit,
                    movq [r14 - ((arg_num + i) as i32 * 8 + LBP_ARG0)], rax;
                );
            }
        } else {
            for i in 0..clear_len {
                monoasm!(self.jit,
                    movq [r14 - ((arg_num + i) as i32 * 8 + (LBP_ARG0))], (NIL_VALUE);
                );
            }
        }
    }

    /*fn jit_expand_arg0(&mut self, req_num: usize) {
        let l1 = self.jit.label();
        monoasm! { self.jit,
            cmpl rdx, 1;
            jne  l1;
            movq rdi, [r14 - (LBP_ARG0)];
            testq rdi, 0b111;
            jnz  l1;
            cmpl [rdi + 4], (ARRAY_CLASS.0);
            jne  l1;
            movq rdx, (req_num);
            lea  rsi, [r14 - (LBP_ARG0)];
            movq rax, (block_expand_array);
            call rax;
            movq rdx, rax;
        l1:
        };
    }*/

    /// fill *val* to the slots [*end* - rax + 1 .. *end*]
    fn jit_fill(&mut self, ends: usize, val: u64) {
        let l0 = self.jit.label();
        let l1 = self.jit.label();
        monoasm! { self.jit,
            testq rax, rax;
            jz   l1;
            lea  rdi, [r14 - (LBP_ARG0 as i32 + ends as i32 * 8)];
        l0:
            movq [rdi + rax * 8], (val);
            subq rax, 1;
            jne  l0;
        l1:
        };
    }
}
