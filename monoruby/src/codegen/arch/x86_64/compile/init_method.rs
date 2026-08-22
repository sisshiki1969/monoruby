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
        // Lazy frame initialization (doc/lazy_frame_init.md): non-argument
        // locals and temps are `C(nil)` / `V` in the abstract state, every
        // point that can observe them (GC safepoint write-back, the
        // suspended-frame fixup, deopt, block-site `locals_to_S`)
        // materializes them, so nothing should ever read this fill. It is
        // POISON — not nil — during the validation soak: a GC marking it
        // aborts naming the frame and slot, turning any coverage hole into
        // a loud, located failure instead of marked stack garbage. The fill
        // (and the constant) disappear entirely once the soak is clean.
        let clear_len = reg_num - arg_num;
        if clear_len > 0 {
            monoasm!( &mut self.jit,
                movq rax, (POISON_VALUE);
            );
            for i in 0..clear_len {
                monoasm!( &mut self.jit,
                    movq [rbp - (RBP_LOCAL_FRAME + (arg_num + i) as i32 * 8 + LFP_ARG0)], rax;
                );
            }
        }
        // Destructured block params (`|(a, b)|`): their slots are inside
        // the argument area, so the loop above misses them, and no caller
        // writes them — the `expand`s after entry do. These stay genuinely
        // nil-filled (not lazy): their abstract state is `S`, so no
        // write-back ever materializes them.
        for i in 0..fn_info.destruct_len {
            monoasm!( &mut self.jit,
                movq [rbp - (RBP_LOCAL_FRAME + (fn_info.destruct_start + i) as i32 * 8 + LFP_ARG0)], (NIL_VALUE);
            );
        }
        self.jit.bind_label(l1);
    }
}
