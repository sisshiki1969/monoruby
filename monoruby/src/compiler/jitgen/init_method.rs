use super::*;

impl Codegen {
    pub(super) fn init_func(&mut self, fn_info: &FnInitInfo) {
        let FnInitInfo {
            reg_num,
            arg_num,
            stack_offset,
            ..
        } = *fn_info;

        monoasm!( &mut self.jit,
            pushq rbp;
            movq rbp, rsp;
            subq rsp, (stack_offset * 16);
        );

        let l1 = self.jit.label();
        self.branch_if_heap_frame(l1);
        // fill nil to temporary registers.
        let clear_len = reg_num - arg_num;
        if clear_len > 2 {
            monoasm!( &mut self.jit,
                movq rax, (NIL_VALUE);
            );
            for i in 0..clear_len {
                monoasm!( &mut self.jit,
                    movq [r14 - ((arg_num + i) as i32 * 8 + LFP_ARG0)], rax;
                );
            }
        } else {
            for i in 0..clear_len {
                monoasm!( &mut self.jit,
                    movq [r14 - ((arg_num + i) as i32 * 8 + LFP_ARG0)], (NIL_VALUE);
                );
            }
        }
        self.jit.bind_label(l1);
    }
}
