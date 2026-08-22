use super::*;
use monoasm_macro::monoasm_arm64;

impl Codegen {

    /// Method prologue: establish fp/lr, reserve the local frame, and nil-fill
    /// the non-argument locals/temps. Mirrors x86 `init_func` (rbp == lfp +
    /// RBP_LOCAL_FRAME, so slots are lfp-relative here). Bails (`false`) if the
    /// frame exceeds the 12-bit `sub sp, sp, #imm` immediate.
    pub(in crate::codegen::jitgen) fn emit_init(
        &mut self,
        info: FnInitInfo,
        prologue_offset: PrologueOffset,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let prologue_bytes = prologue_offset.unwrap_concrete();
        monoasm_arm64!(&mut self.jit,
            stp x29, x30, [sp, #-16]!;
            mov x29, sp;
        );
        self.a64_sp_sub(prologue_bytes as u32);
        let clear_len = info.reg_num - info.arg_num;
        // Lazy frame initialization (doc/lazy_frame_init.md): POISON, not
        // nil — every observer of these slots materializes them from the
        // abstract state, and a GC marking the poison aborts naming the
        // frame and slot. Mirrors x86 `init_func`; the fill disappears once
        // the soak is clean.
        if clear_len > 0 {
            monoasm_arm64!(&mut self.jit, mov x9, (POISON_VALUE););
            for i in 0..clear_len {
                let off = (info.arg_num + i) as u32 * 8 + LFP_ARG0 as u32;
                self.a64_frame_store(9, lfp, off);
            }
        }
        // Destructured block params (`|(a, b)|`): inside the argument
        // area (missed by the loop above) and written only by the
        // `expand`s after entry. These stay genuinely nil-filled (not
        // lazy): their abstract state is `S`, so no write-back ever
        // materializes them. Mirrors x86 `init_func`.
        if info.destruct_len > 0 {
            monoasm_arm64!(&mut self.jit, mov x9, (NIL_VALUE););
            for i in 0..info.destruct_len {
                let off = (info.destruct_start + i) as u32 * 8 + LFP_ARG0 as u32;
                self.a64_frame_store(9, lfp, off);
            }
        }
        true
    }

    /// Per-method ivar-cache prep: when the method accesses heap ivars, ensure
    /// self's var-table is large enough (so the later `Load/StoreIVarHeap` fast
    /// paths can write straight to a slot); grow it via `extend_ivar` otherwise.
    /// A no-op when no heap ivar is accessed or self is always-frozen. Mirrors
    /// x86 `emit_preparation` (inline cold path instead of a page-1 split).
    pub(in crate::codegen::jitgen) fn emit_preparation(&mut self, heap_len: Option<usize>) {
        let Some(heap_len) = heap_len else {
            return;
        };
        let lfp = GP::R14.a64().0; // x22
        let f = extend_ivar as *const () as u64;
        let extend = self.jit.label();
        let exit = self.jit.label();
        // x0 = self (&RValue) and x1 = heap_len are also the `extend_ivar` args,
        // so they are set up *before* the var-table checks (which may branch to
        // `extend` straight away on a None table).
        monoasm_arm64!(&mut self.jit,
            ldur x0, [x(lfp), #(-(LFP_SELF as i32))];   // self
            mov x1, (heap_len as u64);                  // heap_len
            ldr x9, [x0, #(RVALUE_OFFSET_VAR as u32)];  // var_table ptr
            cbz x9, extend;                             // None -> grow
            ldr x10, [x9, #(MONOVEC_CAPA as u32)];
            cbz x10, extend;                            // capa 0 -> grow
            ldr x10, [x9, #(MONOVEC_LEN as u32)];
            cmp x10, x1;                                // len vs heap_len
        );
        self.jit.bcond_label(monoasm::Cond::Lt, &extend); // len < heap_len -> grow
        monoasm_arm64!(&mut self.jit, b exit;);
        // cold: extend_ivar(self, heap_len). At method prologue, so no live FP
        // pool register to preserve (matches x86, which also omits xmm save).
        monoasm_arm64!(&mut self.jit,
            extend:
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
            exit:
        );
    }
}
