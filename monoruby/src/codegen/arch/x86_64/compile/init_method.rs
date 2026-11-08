use super::*;

/// Slot count from which the prologue's nil fill switches to 16-byte
/// `movups` stores. From three slots on the vector form takes fewer
/// instructions (pattern load + one `movups` per pair, vs. the scalar
/// form's `movq rax, nil` + one store per slot) for no more code bytes;
/// at two they tie and the scalar form keeps xmm0 untouched.
const SIMD_FILL_MIN: usize = 3;

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
        // Frame offset of the `i`-th slot of the cleared run. The run
        // descends — slot `i + 1` sits 8 bytes *below* slot `i` — so a
        // 16-byte store addressed at slot `i + 1` covers the pair
        // (`i + 1`, `i`).
        let ofs = |i: usize| -> i32 { RBP_LOCAL_FRAME + (arg_num + i) as i32 * 8 + LFP_ARG0 };
        if clear_len >= SIMD_FILL_MIN {
            // Two slots per store. The pattern is a shared rip-relative
            // constant (`Codegen::nil_pair`), so nothing is read back from
            // the frame we are about to write — a 16-byte load of two
            // just-stored 8-byte words would stall on store forwarding and
            // give most of the saving back.
            let nil_pair = self.nil_pair.clone();
            monoasm!( &mut self.jit,
                movups xmm0, [rip + nil_pair];
            );
            let mut i = 0;
            while i + 1 < clear_len {
                monoasm!( &mut self.jit,
                    movups [rbp - (ofs(i + 1))], xmm0;
                );
                i += 2;
            }
            // Odd tail.
            if i < clear_len {
                monoasm!( &mut self.jit,
                    movq [rbp - (ofs(i))], (NIL_VALUE);
                );
            }
        } else {
            for i in 0..clear_len {
                monoasm!( &mut self.jit,
                    movq [rbp - (ofs(i))], (NIL_VALUE);
                );
            }
        }
        // Destructured block params (`|(a, b)|`): their slots are inside
        // the argument area, so the loop above misses them, and no caller
        // writes them — the `expand`s after entry do. Nil-fill them so
        // the callee-entry GC poll never marks stack garbage.
        for i in 0..fn_info.destruct_len {
            monoasm!( &mut self.jit,
                movq [rbp - (RBP_LOCAL_FRAME + (fn_info.destruct_start + i) as i32 * 8 + LFP_ARG0)], (NIL_VALUE);
            );
        }
        // The named `&block` parameter's slot starts cleared to 0: "not
        // assigned, the frame's block handler is the value" for `BlockArg`
        // / `BlockArgProxy` (VM `clear_block_param`).
        if fn_info.block_param_slot != 0 {
            let off = RBP_LOCAL_FRAME + fn_info.block_param_slot as i32 * 8 + LFP_SELF;
            monoasm!( &mut self.jit,
                movq [rbp - (off)], 0;
            );
        }
        self.jit.bind_label(l1);
    }
}
