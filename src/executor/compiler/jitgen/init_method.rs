use super::*;

impl Codegen {
    pub(super) fn prologue(&mut self, pc: BcPc, fnstore: &Store) {
        monoasm!(self.jit,
            pushq rbp;
            movq rbp, rsp;
        );
        match pc.get_ir(fnstore) {
            TraceIr::InitMethod(fn_info) => {
                self.setup_stack(fn_info.stack_offset);
                self.init_func(&fn_info);
            }
            _ => unreachable!(),
        }
    }

    fn setup_stack(&mut self, stack_offset: usize) {
        monoasm!(self.jit,
            subq rsp, (stack_offset * 16);
        );
    }

    fn init_func(&mut self, fn_info: &FnInitInfo) {
        let FnInitInfo {
            reg_num,
            arg_num,
            block_pos,
            ..
        } = *fn_info;

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
}
