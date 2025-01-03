use super::*;

impl BBContext {
    pub(super) fn index(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        dst: SlotId,
        base: SlotId,
        idx: SlotId,
        base_class: ClassId,
        idx_class: ClassId,
        pc: BytecodePtr,
    ) {
        if store[base_class].is_array_ty_instance() && idx_class == INTEGER_CLASS {
            let deopt = ir.new_deopt(self, pc);
            self.fetch_array_ty(ir, store, base, GP::Rdi, deopt);
            if let Some(idx) = self.is_u16_literal(idx) {
                self.unlink(dst);
                ir.array_u16_index(idx);
            } else {
                self.fetch_fixnum(ir, idx, GP::Rsi, deopt);
                ir.array_index();
            }
        } else {
            self.write_back_slots(ir, &[base, idx]);
            self.unlink(dst);
            ir.generic_index(self, base, idx, pc);
        }
        self.rax2acc(ir, dst);
    }

    pub(super) fn index_assign(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        src: SlotId,
        base: SlotId,
        idx: SlotId,
        base_class: ClassId,
        idx_class: ClassId,
        pc: BytecodePtr,
    ) {
        if store[base_class].is_array_ty_instance() && idx_class == INTEGER_CLASS {
            let deopt = ir.new_deopt(self, pc);
            self.fetch_array_ty(ir, store, base, GP::Rdi, deopt);
            if let Some(idx) = self.is_u16_literal(idx) {
                self.fetch_for_gpr(ir, src, GP::R15);
                ir.array_u16_index_assign(self, idx, pc);
            } else {
                self.fetch_fixnum(ir, idx, GP::Rsi, deopt);
                self.fetch_for_gpr(ir, src, GP::R15);
                ir.array_index_assign(self, pc);
            }
        } else {
            self.write_back_slots(ir, &[base, idx, src]);
            ir.generic_index_assign(self, pc, base, idx, src);
        }
    }
}
