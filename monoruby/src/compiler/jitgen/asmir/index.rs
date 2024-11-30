use super::*;

impl AsmIr {
    pub(in crate::compiler::jitgen) fn index(
        &mut self,
        bb: &mut BBContext,
        dst: SlotId,
        base: SlotId,
        idx: SlotId,
        base_class: Option<ClassId>,
        idx_class: Option<ClassId>,
        pc: BytecodePtr,
    ) {
        if base_class == Some(ARRAY_CLASS) && idx_class == Some(INTEGER_CLASS) {
            let deopt = self.new_deopt(bb, pc);
            bb.fetch_guard_array(self, base, GP::Rdi, deopt);
            if let Some(idx) = bb.is_u16_literal(idx) {
                bb.unlink(self, dst);
                self.array_u16_index(idx);
            } else {
                bb.fetch_guard_fixnum(self, idx, GP::Rsi, deopt);
                self.array_index();
            }
        } else {
            self.write_back_slots(bb, &[base, idx]);
            bb.unlink(self, dst);
            self.generic_index(bb, base, idx, pc);
        }
        bb.rax2acc(self, dst);
    }

    pub(in crate::compiler::jitgen) fn index_assign(
        &mut self,
        bb: &mut BBContext,
        src: SlotId,
        base: SlotId,
        idx: SlotId,
        base_class: Option<ClassId>,
        idx_class: Option<ClassId>,
        pc: BytecodePtr,
    ) {
        if base_class == Some(ARRAY_CLASS) && idx_class == Some(INTEGER_CLASS) {
            let deopt = self.new_deopt(bb, pc);
            bb.fetch_guard_array(self, base, GP::Rdi, deopt);
            if let Some(idx) = bb.is_u16_literal(idx) {
                bb.fetch_for_gpr(self, src, GP::R15);
                self.array_u16_index_assign(bb, idx, pc);
            } else {
                bb.fetch_guard_fixnum(self, idx, GP::Rsi, deopt);
                bb.fetch_for_gpr(self, src, GP::R15);
                self.array_index_assign(bb, pc);
            }
        } else {
            self.write_back_slots(bb, &[base, idx, src]);
            self.generic_index_assign(bb, pc, base, idx, src);
        }
    }
}
