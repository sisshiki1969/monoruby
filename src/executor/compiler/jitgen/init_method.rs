use super::*;

impl Codegen {
    pub(super) fn prologue(&mut self, pc: BcPc, fnstore: &FnStore) {
        monoasm!(self.jit,
            pushq rbp;
            movq rbp, rsp;
            // save len in rdx.
            movq rdx, rdi;
        );
        match pc.get_ir(fnstore) {
            TraceIr::InitMethod {
                reg_num,
                pos_num,
                reqopt_num,
                req_num,
                block_pos,
                stack_offset,
            } => {
                self.setup_stack(stack_offset);
                self.init_func(reg_num, pos_num, reqopt_num, req_num, block_pos, pc, false);
            }
            TraceIr::InitBlock {
                reg_num,
                pos_num,
                reqopt_num,
                req_num,
                block_pos,
                stack_offset,
            } => {
                self.setup_stack(stack_offset);
                if reqopt_num >= 2 {
                    self.jit_expand_arg0(req_num);
                }
                self.init_func(reg_num, pos_num, reqopt_num, req_num, block_pos, pc, true);
            }
            _ => unreachable!(),
        }
    }

    fn setup_stack(&mut self, stack_offset: usize) {
        monoasm!(self.jit,
            subq rsp, (stack_offset * 16);
        );
    }

    fn init_func(
        &mut self,
        reg_num: usize,
        pos_num: usize,
        reqopt_num: usize,
        req_num: usize,
        _block_pos: usize,
        pc: BcPc,
        is_block: bool,
    ) {
        let err_label = self.jit.label();
        self.jit.select_page(1);
        let err = self.wrong_argument;
        monoasm! { self.jit,
        err_label:
            movq r13, ((pc+1).get_u64());
            jmp  err;
        }
        self.jit.select_page(0);

        // rdx: number of args passed from caller
        let has_rest_param = reqopt_num != pos_num;

        if reqopt_num > 0 {
            if reqopt_num == req_num && !has_rest_param {
                monoasm! { self.jit,
                    cmpl rdx, (reqopt_num);
                }
                if is_block {
                    let fill_temp = self.jit.label();
                    monoasm! { self.jit,
                        jge  fill_temp;
                        movl rax, (req_num);
                        subl rax, rdx;
                    }
                    self.jit_fill(req_num, NIL_VALUE);
                    monoasm! { self.jit,
                    fill_temp:
                    }
                } else {
                    monoasm! { self.jit,
                        jne  err_label;
                    }
                }
            } else {
                let set_rest_empty = self.jit.label();
                let fill_req = self.jit.label();
                let fill_opt = self.jit.label();
                let fill_temp = self.jit.label();
                monoasm! { self.jit,
                    // if passed_args >= reqopt_num then goto l1
                    cmpl rdx, (reqopt_num);
                    jeq  set_rest_empty;
                    jlt  fill_req;
                }
                if has_rest_param {
                    monoasm! { self.jit,
                    lea  rdi, [r14 - (reqopt_num as i32 * 8 + LBP_ARG0)];
                    movl rsi, rdx;
                    subl rsi, (reqopt_num);
                    // This is necessary because *make_rest_array* may destroy values under sp
                    // when the number of arguments passed > the number of registers in this function.
                    // TODO: this workaround may cause an error if the number of excess arguments passed exceeds 128.
                    subq rsp, 1024;
                    movq rax, (make_rest_array);
                    call rax;
                    addq rsp, 1024;
                    jmp  fill_temp;
                        };
                } else if is_block {
                    monoasm! { self.jit, jmp  fill_temp; }
                } else {
                    monoasm! { self.jit, jmp  err_label; }
                }

                monoasm! { self.jit,
                fill_req:
                }
                if req_num > 0 {
                    if reqopt_num != req_num {
                        monoasm! { self.jit,
                            // if passed_args >= req_num then goto l2
                            cmpl rdx, (req_num);
                            jge  fill_opt;
                        }
                    }
                    if is_block {
                        monoasm! { self.jit,
                            movl rax, (req_num);
                            subl rax, rdx;
                        }
                        self.jit_fill(req_num, NIL_VALUE);
                        monoasm! { self.jit,
                            movl rdx, (req_num);
                        }
                    } else {
                        // in method, raise error if passed_args < req_num.
                        monoasm! { self.jit,
                            jmp  err_label;
                        }
                    }
                }
                monoasm! { self.jit,
                fill_opt:
                // rax = pos_num - max(passed_args, req_num)
                    movl rax, (reqopt_num);
                    subl rax, rdx;
                // fill zero to residual locals.
                }
                self.jit_fill(reqopt_num, 0);
                monoasm! { self.jit,
                set_rest_empty:
                };
                if has_rest_param {
                    monoasm! { self.jit,
                        lea  rdi, [r14 - (reqopt_num as i32 * 8 + LBP_ARG0)];
                        xorq rsi, rsi;
                        movq rax, (make_rest_array);
                        call rax;
                    };
                }
                monoasm! { self.jit,
                fill_temp:
                }
            }
        } else if has_rest_param {
            monoasm! { self.jit,
                lea  rdi, [r14 - (LBP_ARG0)];
                movl rsi, rdx;
                subq rsp, 1024;
                movq rax, (make_rest_array);
                call rax;
                addq rsp, 1024;
                //jmp  fill_temp;
            };
        } else if !is_block {
            monoasm! { self.jit,
                cmpl rdx, (0);
                jne  err_label;
            }
        }

        // fill nil to temporary registers.
        let clear_len = reg_num - pos_num;
        if clear_len > 2 {
            monoasm!(self.jit,
                movq rax, (NIL_VALUE);
            );
            for i in 0..clear_len {
                monoasm!(self.jit,
                    movq [r14 - ((pos_num + i) as i32 * 8 + LBP_ARG0)], rax;
                );
            }
        } else {
            for i in 0..clear_len {
                monoasm!(self.jit,
                    movq [r14 - ((pos_num + i) as i32 * 8 + (LBP_ARG0))], (NIL_VALUE);
                );
            }
        }
    }

    fn jit_expand_arg0(&mut self, req_num: usize) {
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
    }

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
