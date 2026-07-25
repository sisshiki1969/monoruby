use super::*;
use monoasm_macro::monoasm_arm64;

impl Codegen {


    /// `Kernel#block_given?`: the block slot at [LFP - LFP_BLOCK] is 0 or NIL
    /// when no block was passed. Result Value in Rax (x0).

    /// Length of the array whose pointer is in Rdi (x4) → Rax (x0), untagged.
    /// Arrays store a short length inline (the `capa` field) and switch to a
    /// heap buffer past `ARRAY_INLINE_CAPA`, in which case the real length is
    /// `heap_len`. aarch64 twin of x86 `get_array_length` (`cmovgt` → `csel`).
    pub(crate) fn get_array_length(&mut self) {
        let rdi = GP::Rdi.a64().0; // x4
        let rax = GP::Rax.a64().0; // x0
        monoasm_arm64!(&mut self.jit,
            ldr x(rax), [x(rdi), #(RVALUE_OFFSET_ARY_CAPA as u32)];
            ldr x9, [x(rdi), #(RVALUE_OFFSET_HEAP_LEN as u32)];
            cmp x(rax), #(ARRAY_INLINE_CAPA as u32);
            csel x(rax), x9, x(rax), gt;   // capa > inline cap -> use heap_len
        );
    }

    ///
    /// Array index read with a non-negative i64 index. aarch64 twin of x86
    /// `array_index`.
    ///
    /// ### in
    /// - Rdi (x4): base Array
    /// - Rsi (x3): index, non-negative i64
    ///
    /// ### out
    /// - Rax (x0): result Value (NIL when out of range)
    ///
    pub(crate) fn array_index(&mut self, out_range: &DestLabel) {
        // Unlike x86, the cold (heap / out-of-range) blocks are laid out inline
        // on the same page: aarch64 b/b.cond can't reach monoasm's second page
        // (it is mapped far past the ±128 MB branch range).
        let exit = self.jit.label();
        let heap = self.jit.label();
        let out_range = out_range.clone();
        monoasm_arm64! { &mut self.jit,
            ldr x0, [x4, #(RVALUE_OFFSET_ARY_CAPA as u32)];
            cmp x0, #(ARRAY_INLINE_CAPA as u32);
            b.gt heap;
            // inline: x3 (index) is a non-negative integer.
            cmp x0, x3;                              // capa vs index
            b.le out_range;                          // index >= capa -> out of range
            add x9, x4, x3, lsl #3;
            ldr x0, [x9, #(RVALUE_OFFSET_INLINE as u32)];
            b exit;
        }
        self.jit.bind_label(heap);
        monoasm_arm64! { &mut self.jit,
            ldr x0, [x4, #(RVALUE_OFFSET_HEAP_LEN as u32)];
            cmp x0, x3;
            b.le out_range;
            ldr x4, [x4, #(RVALUE_OFFSET_HEAP_PTR as u32)];
            ldr x0, [x4, x3, lsl #3];
            b exit;
        }
        self.jit.bind_label(out_range);
        monoasm_arm64! { &mut self.jit,
            mov x0, #(NIL_VALUE as u32);
        }
        self.jit.bind_label(exit); // out_range falls through to exit
    }

    ///
    /// Array index assign with a non-negative i64 index. aarch64 twin of x86
    /// `array_index_assign`.
    ///
    /// ### in
    /// - Rdi (x4): base Array
    /// - Rsi (x3): index, non-negative i64
    /// - Rdx (x2): source Value
    ///
    /// ### destroy
    /// - caller-save registers except the FP pool
    ///
    pub(crate) fn array_index_assign(
        &mut self,
        using_fpr: UsingFpr,
        generic: &DestLabel,
        error: &DestLabel,
    ) {
        // Cold (heap / generic-C-call) blocks laid out inline; see array_index
        // for why select_page can't be used on aarch64.
        let exit = self.jit.label();
        let heap = self.jit.label();
        let generic = generic.clone();
        monoasm_arm64! { &mut self.jit,
            ldr x0, [x4, #(RVALUE_OFFSET_ARY_CAPA as u32)];
            cmp x0, #(ARRAY_INLINE_CAPA as u32);
            b.gt heap;
            // inline: x3 (index) is a non-negative integer.
            cmp x0, x3;
            b.le generic;                            // index >= capa -> generic
            add x9, x4, x3, lsl #3;
            str x2, [x9, #(RVALUE_OFFSET_INLINE as u32)];  // src (Rdx) -> slot
        }
        // Write barrier: x4 (Rdi) = the array (parent), x2 (Rdx) = stored value.
        self.emit_write_barrier(GP::Rdi, GP::Rdx);
        monoasm_arm64! { &mut self.jit, b exit; }
        self.jit.bind_label(heap);
        monoasm_arm64! { &mut self.jit,
            ldr x0, [x4, #(RVALUE_OFFSET_HEAP_LEN as u32)];
            cmp x0, x3;
            b.le generic;
        }
        // Write barrier before `x4` (Rdi) is repointed at the heap buffer:
        // x4 = the array (parent), x2 (Rdx) = stored value.
        self.emit_write_barrier(GP::Rdi, GP::Rdx);
        monoasm_arm64! { &mut self.jit,
            ldr x4, [x4, #(RVALUE_OFFSET_HEAP_PTR as u32)];
            add x9, x4, x3, lsl #3;
            str x2, [x9];
            b exit;
        }
        self.jit.bind_label(generic);
        self.emit_fpr_save(using_fpr, false);
        // set_array_integer_index(base, index, vm, globals, src). Source regs at
        // entry: base=x4, index=x3, src=x2. Reorder into the C ABI args
        // (x0..x4) without clobbering a still-needed source.
        let f = set_array_integer_index as *const () as u64;
        monoasm_arm64! { &mut self.jit,
            mov x0, x4;            // base -> arg0
            mov x4, x2;            // src  -> arg4   (x2 free after this)
            mov x1, x3;            // index -> arg1
            mov x3, x20;           // globals -> arg3
            mov x2, x19;           // vm -> arg2
            mov x9, (f);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
        }
        self.emit_fpr_restore(using_fpr, false);
        self.emit_handle_error(error);
        self.jit.bind_label(exit); // generic C-call path falls through to exit
    }

    ///
    /// §20 (B): emit an array integer-index **read** for `AsmInst::ArrayIndex`
    /// (aarch64). The index-register setup + `array_index` call that used to live
    /// in the `ir.inline(|gen| …)` closure, driven by the typed `ArrayIndexKind`.
    ///
    pub(crate) fn gen_array_index(&mut self, kind: ArrayIndexKind) {
        match kind {
            ArrayIndexKind::U16(idx) => {
                let out_range = self.jit.label();
                monoasm_arm64! { &mut self.jit,
                    mov x3, (idx as u64);   // index (already non-negative)
                }
                self.array_index(&out_range);
            }
            ArrayIndexKind::Fixnum => {
                // Single-page layout (no select_page — see Codegen::array_index):
                // the negative-index normalization is laid out inline; a
                // non-negative index branches straight to `checked`.
                let generic = self.jit.label();
                let checked = self.jit.label();
                monoasm_arm64! { &mut self.jit,
                    asr x3, x3, #1;         // untag index
                    cmp x3, #0;
                    b.pl checked;           // non-negative -> use as-is
                }
                self.get_array_length();    // x0 <- len, x4 (base) preserved
                monoasm_arm64! { &mut self.jit,
                    adds x3, x3, x0;        // index += len, set flags
                    b.pl checked;           // normalized non-negative -> recheck
                    b generic;              // past the start -> out of range
                }
                self.jit.bind_label(checked.clone());
                self.array_index(&generic);
            }
        }
    }

    ///
    /// §20 (B): emit an array integer-index **assign** for
    /// `AsmInst::ArrayIndexAssign` (aarch64).
    ///
    pub(crate) fn gen_array_index_assign(
        &mut self,
        kind: ArrayIndexKind,
        using_fpr: UsingFpr,
        error: &DestLabel,
    ) {
        match kind {
            ArrayIndexKind::U16(idx) => {
                let generic = self.jit.label();
                monoasm_arm64! { &mut self.jit,
                    mov x3, (idx as u64);   // index (already non-negative)
                }
                self.array_index_assign(using_fpr, &generic, error);
            }
            ArrayIndexKind::Fixnum => {
                // Single-page layout (no select_page — see array_index_assign).
                let generic = self.jit.label();
                let checked = self.jit.label();
                monoasm_arm64! { &mut self.jit,
                    asr x3, x3, #1;         // untag index
                    cmp x3, #0;
                    b.pl checked;           // non-negative -> use as-is
                }
                self.get_array_length();    // x0 <- len, x4 (base) preserved
                monoasm_arm64! { &mut self.jit,
                    adds x3, x3, x0;        // index += len, set flags
                    b.pl checked;           // normalized non-negative -> recheck
                    b generic;              // past the start -> out of range
                }
                self.jit.bind_label(checked.clone());
                self.array_index_assign(using_fpr, &generic, error);
            }
        }
    }
}
