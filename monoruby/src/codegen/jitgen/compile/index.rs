use super::*;

impl<'a> JitContext<'a> {
    pub(super) fn index(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        dst: SlotId,
        base: SlotId,
        idx: SlotId,
        ic: Option<(ClassId, ClassId)>,
        bc_pos: BcIndex,
    ) -> JitResult<CompileResult> {
        let (base_class, idx_class) = state.binary_class(base, idx, ic);
        if let (Some(base_class), Some(INTEGER_CLASS)) = (base_class, idx_class) {
            if self.store[base_class].is_array_ty_instance() {
                state.array_integer_index(ir, &self.store, dst, base, idx);
                return Ok(CompileResult::Continue);
            }
        }
        if let Some(lhs_class) = base_class {
            return self.call_binary_method(
                state,
                ir,
                base,
                idx,
                lhs_class,
                IdentId::_INDEX,
                bc_pos,
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
        if let (Some(base_class), Some(INTEGER_CLASS)) = (base_class, idx_class) {
            if self.store[base_class].is_array_ty_instance() {
                state.array_integer_index_assign(ir, self.store, src, base, idx);
                return Ok(CompileResult::Continue);
            }
        }
        if let Some(recv_class) = base_class {
            return self.call_ternary_method(
                state,
                ir,
                base,
                idx,
                recv_class,
                IdentId::_INDEX_ASSIGN,
                bc_pos,
            );
        }
        return Ok(CompileResult::Recompile(RecompileReason::NotCached));
    }
}

impl AbstractState {
    fn array_integer_index(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        dst: SlotId,
        base: SlotId,
        idx: SlotId,
    ) {
        self.load_array_ty(ir, store, base, GP::Rdi);
        if let Some(idx) = self.is_u16(idx) {
            ir.array_u16_index(idx);
        } else {
            self.load_fixnum(ir, idx, GP::Rsi);
            ir.array_index();
        }
        self.def_rax2acc(ir, dst);
    }

    fn array_integer_index_assign(
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
            ir.array_u16_index_assign(self, idx);
        } else {
            self.load_fixnum(ir, idx, GP::Rsi);
            self.load(ir, src, GP::Rdx);
            ir.array_index_assign(self);
        }
    }
}
