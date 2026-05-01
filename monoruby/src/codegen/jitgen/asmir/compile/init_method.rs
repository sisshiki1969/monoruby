use super::*;

impl Codegen {
    ///
    /// Initialize function stack frame.
    ///
    /// `prologue_bytes` is the resolved stack-pointer adjustment in
    /// bytes — derived by [`super::super::context::JitContext::resolve_dyn_var_offsets`]
    /// from the frame's recorded `stack_offset`. `fn_info.stack_offset`
    /// is the static bytecodegen-time hint (in 16-byte units) and is
    /// no longer consulted here; future spill slots that grow the
    /// frame size feed through `prologue_bytes` automatically.
    ///
    pub(super) fn init_func(&mut self, fn_info: &FnInitInfo, prologue_bytes: usize) {
        let FnInitInfo {
            reg_num, arg_num, ..
        } = *fn_info;

        monoasm!( &mut self.jit,
            pushq rbp;
            movq rbp, rsp;
            subq rsp, (prologue_bytes as i32);
        );

        let l1 = self.jit.label();
        // fill nil to non-argument locals and temporary registers.
        let clear_len = reg_num - arg_num;
        if clear_len > 2 {
            monoasm!( &mut self.jit,
                movq rax, (NIL_VALUE);
            );
            for i in 0..clear_len {
                monoasm!( &mut self.jit,
                    movq [rbp - (RBP_LOCAL_FRAME + (arg_num + i) as i32 * 8 + LFP_ARG0)], rax;
                );
            }
        } else {
            for i in 0..clear_len {
                monoasm!( &mut self.jit,
                    movq [rbp - (RBP_LOCAL_FRAME + (arg_num + i) as i32 * 8 + LFP_ARG0)], (NIL_VALUE);
                );
            }
        }
        self.jit.bind_label(l1);
    }
}
