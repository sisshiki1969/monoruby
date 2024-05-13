use super::*;

impl Codegen {
    pub(super) fn prologue(&mut self, store: &Store, pc: BcPc) {
        monoasm!( &mut self.jit,
            pushq rbp;
            movq rbp, rsp;
        );
        match pc.trace_ir(store) {
            TraceIr::InitMethod(fn_info) => {
                self.setup_stack(fn_info.stack_offset);
                self.init_func(&fn_info);
            }
            _ => unreachable!(),
        }
    }

    fn setup_stack(&mut self, stack_offset: usize) {
        monoasm!( &mut self.jit,
            subq rsp, (stack_offset * 16);
        );
    }

    fn init_func(&mut self, fn_info: &FnInitInfo) {
        // fill block parameter.
        let FnInitInfo {
            reg_num, arg_num, ..
        } = *fn_info;

        let l1 = self.jit.label();
        self.test_heap_frame();
        monoasm! { &mut self.jit,
            jnz l1;
        }
        // fill nil to temporary registers.
        let clear_len = reg_num - arg_num;
        if clear_len > 2 {
            monoasm!( &mut self.jit,
                movq rax, (NIL_VALUE);
            );
            for i in 0..clear_len {
                monoasm!( &mut self.jit,
                    movq [r14 - ((arg_num + i) as i32 * 8 + LBP_ARG0)], rax;
                );
            }
        } else {
            for i in 0..clear_len {
                monoasm!( &mut self.jit,
                    movq [r14 - ((arg_num + i) as i32 * 8 + LBP_ARG0)], (NIL_VALUE);
                );
            }
        }
        self.jit.bind_label(l1);
    }
}
