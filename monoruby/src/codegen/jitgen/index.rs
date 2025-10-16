use super::*;

impl BBContext {
    pub(super) fn array_integer_index(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        dst: SlotId,
        base: SlotId,
        idx: SlotId,
    ) {
        let deopt = ir.new_deopt(self);
        self.fetch_array_ty(ir, store, base, GP::Rdi, deopt);
        if let Some(idx) = self.is_u16_literal(idx) {
            ir.array_u16_index(idx);
        } else {
            self.fetch_fixnum(ir, idx, GP::Rsi, deopt);
            ir.array_index();
        }
        self.def_rax2acc(ir, dst);
    }

    pub(super) fn array_integer_index_assign(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        src: SlotId,
        base: SlotId,
        idx: SlotId,
    ) {
        let deopt = ir.new_deopt(self);
        self.fetch_array_ty(ir, store, base, GP::Rdi, deopt);
        if let Some(idx) = self.is_u16_literal(idx) {
            self.fetch(ir, src, GP::Rdx);
            ir.array_u16_index_assign(self, idx);
        } else {
            self.fetch_fixnum(ir, idx, GP::Rsi, deopt);
            self.fetch(ir, src, GP::Rdx);
            ir.array_index_assign(self);
        }
    }
}
