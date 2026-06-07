use super::*;
#[cfg(all(jit, not(jit_x86)))]
use monoasm_macro::monoasm_arm64;

impl<'a> JitContext<'a> {
    pub(super) fn index(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        base: SlotId,
        idx: SlotId,
        ic: Option<(ClassId, ClassId)>,
        bc_pos: BcIndex,
    ) -> JitResult<CompileResult> {
        let (base_class, idx_class) = state.binary_class(base, idx, ic);
        if let Some(lhs_class) = base_class {
            return self.call_binary_method(
                state,
                ir,
                base,
                idx,
                lhs_class,
                idx_class,
                IdentId::_INDEX,
                bc_pos,
                false,
            );
        }
        return Ok(CompileResult::Recompile(RecompileReason::NotCached));
    }

    pub(super) fn index_assign(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        base: SlotId,
        idx: SlotId,
        src: SlotId,
        ic: Option<(ClassId, ClassId)>,
        bc_pos: BcIndex,
    ) -> JitResult<CompileResult> {
        let (base_class, idx_class) = state.binary_class(base, idx, ic);
        if let Some(recv_class) = base_class {
            return self.call_ternary_method(
                state,
                ir,
                base,
                idx,
                src,
                recv_class,
                idx_class,
                IdentId::_INDEX_ASSIGN,
                bc_pos,
            );
        }
        return Ok(CompileResult::Recompile(RecompileReason::NotCached));
    }
}

impl AbstractState {
#[cfg(jit_x86)]
    pub(crate) fn array_integer_index(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        dst: SlotId,
        base: SlotId,
        idx: SlotId,
    ) {
        self.load_array_ty(ir, store, base, GP::Rdi);
        if let Some(idx) = self.is_u16(idx) {
            ir.inline(move |r#gen, _, _, _| {
                let out_range = r#gen.jit.label();
                monoasm! { &mut r#gen.jit,
                    movl rsi, (idx);
                }
                r#gen.array_index(&out_range);
            });
        } else {
            self.load_fixnum(ir, idx, GP::Rsi);
            ir.inline(move |r#gen, _, _, _| {
                //r#gen.gen_array_index();

                let generic = r#gen.jit.label();
                let checked = r#gen.jit.label();
                let negative = r#gen.jit.label();
                monoasm! { &mut r#gen.jit,
                    sarq  rsi, 1;
                    testq rsi, rsi;
                    js  negative;
                checked:
                }
                r#gen.array_index(&generic);

                r#gen.jit.select_page(1);
                r#gen.jit.bind_label(negative);
                r#gen.get_array_length();
                monoasm! { &mut r#gen.jit,
                    addq rsi, rax;
                    jns  checked;
                    jmp  generic;
                }
                r#gen.jit.select_page(0);
            });
        }
        self.def_rax2acc(ir, dst);
    }

    /// aarch64 twin of `array_integer_index`. The hot-path asm lives in
    /// `Codegen::array_index` (compile_stub.rs); here we set up the index
    /// register (Rsi/x3) and the negative-index normalization.
    #[cfg(all(jit, not(jit_x86)))]
    pub(crate) fn array_integer_index(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        dst: SlotId,
        base: SlotId,
        idx: SlotId,
    ) {
        self.load_array_ty(ir, store, base, GP::Rdi);
        if let Some(idx) = self.is_u16(idx) {
            ir.inline(move |r#gen, _, _, _| {
                let out_range = r#gen.jit.label();
                monoasm_arm64! { &mut r#gen.jit,
                    mov x3, (idx as u64);   // Rsi <- index (already non-negative)
                }
                r#gen.array_index(&out_range);
            });
        } else {
            self.load_fixnum(ir, idx, GP::Rsi);
            ir.inline(move |r#gen, _, _, _| {
                // Single-page layout (no select_page — see Codegen::array_index):
                // the negative-index normalization is laid out inline; a
                // non-negative index branches straight to `checked`.
                let generic = r#gen.jit.label();
                let checked = r#gen.jit.label();
                monoasm_arm64! { &mut r#gen.jit,
                    asr x3, x3, #1;         // untag index (Rsi)
                    cmp x3, #0;
                    b.pl checked;           // non-negative -> use as-is
                }
                r#gen.get_array_length();   // Rax <- len, Rdi (base) preserved
                monoasm_arm64! { &mut r#gen.jit,
                    adds x3, x3, x0;        // index += len, set flags
                    b.pl checked;           // normalized non-negative -> recheck
                    b generic;              // past the start -> out of range
                }
                r#gen.jit.bind_label(checked.clone());
                r#gen.array_index(&generic);
            });
        }
        self.def_rax2acc(ir, dst);
    }

    ///
    /// Aray index assign operation.
    ///
    /// ### in
    /// - rsi: index Fixnum
    /// - rdx: result Value
    ///
    /// ### destroy
    /// - caller save registers except xmm's
    ///
#[cfg(jit_x86)]
    pub(crate) fn array_integer_index_assign(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        src: SlotId,
        base: SlotId,
        idx: SlotId,
    ) {
        self.load_array_ty(ir, store, base, GP::Rdi);
        if let Some(idx) = self.is_u16(idx) {
            self.load(ir, src, GP::Rdx);
            let using_xmm = self.get_using_xmm();
            let error = ir.new_error(self);
            ir.inline(move |r#gen, _, labels, _| {
                let generic = r#gen.jit.label();
                monoasm! { &mut r#gen.jit,
                    movl rsi, (idx);
                }
                r#gen.array_index_assign(using_xmm, &generic, &labels[error]);
            });
        } else {
            self.load_fixnum(ir, idx, GP::Rsi);
            self.load(ir, src, GP::Rdx);
            let using_xmm = self.get_using_xmm();
            let error = ir.new_error(self);
            ir.inline(move |r#gen, _, labels, _| {
                let generic = r#gen.jit.label();
                let checked = r#gen.jit.label();
                let negative = r#gen.jit.label();
                monoasm! { &mut r#gen.jit,
                    sarq  rsi, 1;
                    testq rsi, rsi;
                    js   negative;
                checked:
                };
                r#gen.array_index_assign(using_xmm, &generic, &labels[error]);

                r#gen.jit.select_page(1);
                r#gen.jit.bind_label(negative);
                r#gen.get_array_length();
                monoasm! { &mut r#gen.jit,
                    addq rsi, rax;
                    jns  checked;
                    jmp  generic;
                }
                r#gen.jit.select_page(0);
            });
        }
    }

    /// aarch64 twin of `array_integer_index_assign`. The hot-path + generic
    /// C-call asm lives in `Codegen::array_index_assign` (compile_stub.rs);
    /// here we set up the index (Rsi/x3) + source (Rdx/x2) and normalize a
    /// negative index.
    #[cfg(all(jit, not(jit_x86)))]
    pub(crate) fn array_integer_index_assign(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        src: SlotId,
        base: SlotId,
        idx: SlotId,
    ) {
        self.load_array_ty(ir, store, base, GP::Rdi);
        if let Some(idx) = self.is_u16(idx) {
            self.load(ir, src, GP::Rdx);
            let using_xmm = self.get_using_xmm();
            let error = ir.new_error(self);
            ir.inline(move |r#gen, _, labels, _| {
                let generic = r#gen.jit.label();
                monoasm_arm64! { &mut r#gen.jit,
                    mov x3, (idx as u64);   // Rsi <- index (already non-negative)
                }
                r#gen.array_index_assign(using_xmm, &generic, &labels[error]);
            });
        } else {
            self.load_fixnum(ir, idx, GP::Rsi);
            self.load(ir, src, GP::Rdx);
            let using_xmm = self.get_using_xmm();
            let error = ir.new_error(self);
            ir.inline(move |r#gen, _, labels, _| {
                // Single-page layout (no select_page — see array_index_assign).
                let generic = r#gen.jit.label();
                let checked = r#gen.jit.label();
                monoasm_arm64! { &mut r#gen.jit,
                    asr x3, x3, #1;         // untag index (Rsi)
                    cmp x3, #0;
                    b.pl checked;           // non-negative -> use as-is
                }
                r#gen.get_array_length();   // Rax <- len, Rdi (base) preserved
                monoasm_arm64! { &mut r#gen.jit,
                    adds x3, x3, x0;        // index += len, set flags
                    b.pl checked;           // normalized non-negative -> recheck
                    b generic;              // past the start -> out of range
                }
                r#gen.jit.bind_label(checked.clone());
                r#gen.array_index_assign(using_xmm, &generic, &labels[error]);
            });
        }
    }
}
