use super::*;

impl BBContext {
    pub(super) fn array_integer_index(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        dst: SlotId,
        base: SlotId,
        idx: SlotId,
        pc: BytecodePtr,
    ) {
        self.load_array_ty(ir, store, base, GP::Rdi, pc);
        if let Some(idx) = self.is_u16(idx) {
            ir.array_u16_index(idx);
        } else {
            self.load_fixnum(ir, idx, GP::Rsi, pc);
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
        pc: BytecodePtr,
    ) {
        self.load_array_ty(ir, store, base, GP::Rdi, pc);
        if let Some(idx) = self.is_u16(idx) {
            self.load(ir, src, GP::Rdx);
            ir.array_u16_index_assign(self, idx, pc);
        } else {
            self.load_fixnum(ir, idx, GP::Rsi, pc);
            self.load(ir, src, GP::Rdx);
            ir.array_index_assign(self, pc);
        }
    }
}
