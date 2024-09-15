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
            self.fetch_guard_array(bb, base, GP::Rdi, deopt);
            if let Some(idx) = bb.is_u16_literal(idx) {
                self.unlink(bb, dst);
                self.array_u16_index(idx);
            } else {
                self.fetch_guard_fixnum(bb, idx, GP::Rsi, deopt);
                self.array_index();
            }
        } else {
            self.write_back_slots(bb, &[base, idx]);
            self.unlink(bb, dst);
            self.generic_index(bb, base, idx, pc);
        }
        self.rax2acc(bb, dst);
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
            self.fetch_guard_array(bb, base, GP::Rdi, deopt);
            if let Some(idx) = bb.is_u16_literal(idx) {
                self.fetch_to_reg(bb, src, GP::R15);
                self.array_u16_index_assign(bb, idx, pc);
            } else {
                self.fetch_guard_fixnum(bb, idx, GP::Rsi, deopt);
                self.fetch_to_reg(bb, src, GP::R15);
                self.array_index_assign(bb, pc);
            }
        } else {
            self.write_back_slots(bb, &[base, idx, src]);
            self.generic_index_assign(bb, pc, base, idx, src);
        }
    }
}
