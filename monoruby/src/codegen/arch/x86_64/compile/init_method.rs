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
    pub(super) fn init_func(
        &mut self,
        fn_info: &FnInitInfo,
        prologue_bytes: usize,
        nil_block_arg: Option<u16>,
    ) {
        monoasm!( &mut self.jit,
            pushq rbp;
            movq rbp, rsp;
            subq rsp, (prologue_bytes as i32);
        );

        let l1 = self.jit.label();
        // Lazy frame initialization (doc/lazy_frame_init.md): non-argument
        // locals and temps are `C(nil)` / `V` in the abstract state, and
        // every point that can observe them materializes them first — the
        // `get_using_fpr` chokepoint before control leaves the compilation
        // unit, the safepoint write-back, deopt / error exits, block-site
        // `locals_to_S`. So the prologue writes nothing at all here: this is
        // the whole point of the optimization, worth ~0.28ns per slot per
        // call (a 64-local frame's call cost drops 24.2ns -> 6.2ns).
        //
        // `frame-poison` puts the fill back as a POISON pattern — never nil,
        // so a slot that reaches the GC unmaterialized aborts naming its
        // frame and slot instead of marking stack garbage. That is how the
        // coverage was validated in the first place; keep it available for
        // re-validating after changes to the write-back machinery.
        #[cfg(feature = "frame-poison")]
        {
            let FnInitInfo {
                reg_num, arg_num, ..
            } = *fn_info;
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
        }
        // A block-parameter slot inside the fill range (`(...)` forwarding):
        // no caller path writes it and no write-back models it — genuine nil,
        // as the old prologue provided (see `AsmInst::Init::nil_block_arg`).
        if let Some(b) = nil_block_arg {
            monoasm!( &mut self.jit,
                movq [rbp - (RBP_LOCAL_FRAME + b as i32 * 8 + LFP_ARG0)], (NIL_VALUE);
            );
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
