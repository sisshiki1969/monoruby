use super::*;

impl BBContext {
    ///
    /// fetch *slot* and store in *dst*.
    ///
    /// ### destroy
    /// - rax, rcx
    ///
    pub(crate) fn fetch_for_gpr(&mut self, ir: &mut AsmIr, slot: SlotId, dst: GP) {
        if slot >= self.sp {
            unreachable!("{:?} >= {:?} in fetch_for_gpr()", slot, self.sp);
        };
        self.fetch_gpr(ir, slot, dst);
    }

    ///
    /// fetch *slot* and store in callee stack with `offset`.
    ///
    /// ### destroy
    /// - rax, rcx
    ///
    pub(crate) fn fetch_for_callee(&mut self, ir: &mut AsmIr, slot: SlotId, offset: i32) {
        if slot >= self.sp {
            unreachable!("{:?} >= {:?} in fetch_for_callee()", slot, self.sp);
        };
        self.use_as_value(slot);
        match self.mode(slot) {
            LinkMode::Accumulator => {
                ir.reg2rsp_offset(GP::R15, offset);
            }
            _ => {
                self.fetch_for_gpr(ir, slot, GP::Rax);
                ir.reg2rsp_offset(GP::Rax, offset);
            }
        }
    }

    pub(crate) fn fetch_rhs_for_callee(&mut self, ir: &mut AsmIr, mode: OpMode, offset: i32) {
        match mode {
            OpMode::IR(_, slot) | OpMode::RR(_, slot) => {
                if slot >= self.sp {
                    unreachable!("{:?} >= {:?} in fetch_for_callee()", slot, self.sp);
                };
                self.fetch_for_callee_stack(ir, slot, offset);
            }
            OpMode::RI(_, i) => ir.i32torsp_offset(i as i32, offset),
        }
    }

    pub(crate) fn fetch_array_ty(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        slot: SlotId,
        dst: GP,
        deopt: AsmDeopt,
    ) {
        let is_array = self.is_array_ty(store, slot);
        self.fetch_for_gpr(ir, slot, dst);
        if !is_array {
            ir.guard_array_ty(dst, deopt);
        }
    }

    pub(crate) fn fetch_fixnum(&mut self, ir: &mut AsmIr, slot: SlotId, dst: GP, deopt: AsmDeopt) {
        self.fetch_for_gpr(ir, slot, dst);
        self.guard_class(ir, slot, dst, INTEGER_CLASS, deopt);
    }
}
