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
        self[slot].use_as_value();
        match self.slot(slot) {
            LinkMode::Xmm(xmm) => {
                if dst == GP::R15 {
                    self.writeback_acc(ir);
                }
                ir.xmm2stack(xmm, slot);
                ir.reg_move(GP::Rax, dst);
                self.set_both_float(slot, xmm);
            }
            LinkMode::ConcreteValue(v) => {
                if dst == GP::R15 {
                    self.writeback_acc(ir);
                }
                ir.lit2reg(v, dst);
            }
            LinkMode::Both(_) | LinkMode::Stack => {
                if dst == GP::R15 {
                    self.writeback_acc(ir);
                }
                ir.stack2reg(slot, dst);
            }
            LinkMode::Accumulator => {
                ir.reg_move(GP::R15, dst);
            }
        }
    }

    pub(crate) fn fetch_for_callee(&mut self, ir: &mut AsmIr, slot: SlotId, offset: i32) {
        match self.slot(slot) {
            LinkMode::Accumulator => {
                if slot >= self.sp {
                    unreachable!("{:?} >= {:?} in fetch_for_callee()", slot, self.sp);
                };
                self[slot].use_as_value();
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
            OpMode::IR(_, slot) | OpMode::RR(_, slot) => match self.slot(slot) {
                LinkMode::Accumulator => {
                    if slot >= self.sp {
                        unreachable!("{:?} >= {:?} in fetch_for_callee()", slot, self.sp);
                    };
                    self[slot].use_as_value();
                    ir.reg2rsp_offset(GP::R15, offset);
                }
                _ => {
                    self.fetch_for_gpr(ir, slot, GP::Rax);
                    ir.reg2rsp_offset(GP::Rax, offset);
                }
            },
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
        let is_fixnum = self.is_fixnum(slot);
        self.fetch_for_gpr(ir, slot, dst);
        if !is_fixnum {
            ir.guard_fixnum(dst, deopt);
            self.set_guard_fixnum(slot);
        }
    }

    ///
    /// Read from a slot *reg* as f64, and store in xmm register.
    ///
    /// ### destroy
    /// - rdi
    ///
    pub(super) fn fetch_integer_for_xmm(
        &mut self,
        ir: &mut AsmIr,
        slot: SlotId,
        deopt: AsmDeopt,
    ) -> Xmm {
        fn int_to_both(ir: &mut AsmIr, bb: &mut BBContext, slot: SlotId, deopt: AsmDeopt) -> Xmm {
            let x = bb.store_new_both_integer(slot);
            ir.stack2reg(slot, GP::Rdi);
            ir.int2xmm(GP::Rdi, x, deopt);
            x
        }
        self[slot].use_as_float();
        match self.slot(slot) {
            LinkMode::Both(x) | LinkMode::Xmm(x) => x,
            LinkMode::Stack => int_to_both(ir, self, slot, deopt),
            LinkMode::Accumulator => {
                // -> Both
                let x = self.store_new_both_integer(slot);
                ir.reg2stack(GP::R15, slot);
                ir.int2xmm(GP::R15, x, deopt);
                x
            }
            LinkMode::ConcreteValue(v) => self.fetch_float_concrete_value_for_xmm(ir, slot, v),
        }
    }

    ///
    /// Fetch *reg* as f64, and store in xmm register.
    ///
    /// ### destroy
    /// - rdi, rax
    ///
    ///
    pub(crate) fn fetch_float_for_xmm(
        &mut self,
        ir: &mut AsmIr,
        slot: SlotId,
        deopt: AsmDeopt,
    ) -> Xmm {
        fn float_to_both(ir: &mut AsmIr, bb: &mut BBContext, slot: SlotId, deopt: AsmDeopt) -> Xmm {
            let x = bb.store_new_both_float(slot);
            ir.stack2reg(slot, GP::Rdi);
            ir.float2xmm(GP::Rdi, x, deopt);
            x
        }
        self[slot].use_as_float();
        match self.slot(slot) {
            LinkMode::Both(x) | LinkMode::Xmm(x) => x,
            LinkMode::Stack => float_to_both(ir, self, slot, deopt),
            LinkMode::Accumulator => {
                // -> Both
                let x = self.store_new_both_float(slot);
                ir.reg2stack(GP::R15, slot);
                ir.float2xmm(GP::R15, x, deopt);
                x
            }
            LinkMode::ConcreteValue(v) => self.fetch_float_concrete_value_for_xmm(ir, slot, v),
        }
    }

    fn fetch_float_concrete_value_for_xmm(
        &mut self,
        ir: &mut AsmIr,
        slot: SlotId,
        v: Value,
    ) -> Xmm {
        if let Some(f) = v.try_float() {
            // -> Xmm
            let x = self.store_new_xmm(slot);
            ir.f64toxmm(f, x);
            x
        } else if let Some(i) = v.try_fixnum() {
            // -> Both
            let x = self.store_new_both_integer(slot);
            ir.i64toboth(i, slot, x);
            x
        } else {
            unreachable!()
        }
    }
}
