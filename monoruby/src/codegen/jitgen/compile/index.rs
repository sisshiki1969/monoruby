use super::*;
use crate::codegen::jitgen::asmir::ArrayIndexKind;

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
            // The `Index` fast-path calls `#[]` directly, bypassing the
            // `is_func_call = false` visibility gate the VM's `get_index`
            // applies. Bail to the interpreter for a non-public `#[]` so its
            // `NoMethodError` still fires (a literal `self[i]` reaches a
            // private `#[]` through the ordinary call site, not here).
            if !self.jit_index_method_is_public(lhs_class, IdentId::_INDEX) {
                return Ok(CompileResult::Deopt);
            }
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
            // The `IndexAssign` fast-path calls `#[]=` directly, bypassing the
            // `is_func_call = false` visibility gate the VM's `set_index`
            // applies. Bail to the interpreter for a non-public `#[]=` so its
            // `NoMethodError` still fires (a literal `self[i] = v` reaches a
            // private `#[]=` through the ordinary call site, not here).
            if !self.jit_index_method_is_public(recv_class, IdentId::_INDEX_ASSIGN) {
                return Ok(CompileResult::Deopt);
            }
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
#[cfg(target_arch = "x86_64")]
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
            ir.push(AsmInst::ArrayIndex {
                kind: ArrayIndexKind::U16(idx),
            });
        } else {
            self.load_fixnum(ir, idx, GP::Rsi);
            ir.push(AsmInst::ArrayIndex {
                kind: ArrayIndexKind::Fixnum,
            });
        }
        self.def_rax2acc(ir, dst);
    }

    /// aarch64 twin of `array_integer_index`. The hot-path asm lives in
    /// `Codegen::array_index` (arch/aarch64/compile.rs); here we set up the index
    /// register (Rsi/x3) and the negative-index normalization.
    #[cfg(target_arch = "aarch64")]
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
            ir.push(AsmInst::ArrayIndex {
                kind: ArrayIndexKind::U16(idx),
            });
        } else {
            self.load_fixnum(ir, idx, GP::Rsi);
            ir.push(AsmInst::ArrayIndex {
                kind: ArrayIndexKind::Fixnum,
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
    /// - caller save registers except fpr's
    ///
#[cfg(target_arch = "x86_64")]
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
            let using_fpr = self.get_using_fpr(ir);
            let error = ir.new_error(self);
            ir.push(AsmInst::ArrayIndexAssign {
                kind: ArrayIndexKind::U16(idx),
                using_fpr,
                error,
            });
        } else {
            self.load_fixnum(ir, idx, GP::Rsi);
            self.load(ir, src, GP::Rdx);
            let using_fpr = self.get_using_fpr(ir);
            let error = ir.new_error(self);
            ir.push(AsmInst::ArrayIndexAssign {
                kind: ArrayIndexKind::Fixnum,
                using_fpr,
                error,
            });
        }
    }

    /// aarch64 twin of `array_integer_index_assign`. The hot-path + generic
    /// C-call asm lives in `Codegen::array_index_assign` (arch/aarch64/compile.rs);
    /// here we set up the index (Rsi/x3) + source (Rdx/x2) and normalize a
    /// negative index.
    #[cfg(target_arch = "aarch64")]
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
            let using_fpr = self.get_using_fpr(ir);
            let error = ir.new_error(self);
            ir.push(AsmInst::ArrayIndexAssign {
                kind: ArrayIndexKind::U16(idx),
                using_fpr,
                error,
            });
        } else {
            self.load_fixnum(ir, idx, GP::Rsi);
            self.load(ir, src, GP::Rdx);
            let using_fpr = self.get_using_fpr(ir);
            let error = ir.new_error(self);
            ir.push(AsmInst::ArrayIndexAssign {
                kind: ArrayIndexKind::Fixnum,
                using_fpr,
                error,
            });
        }
    }
}
