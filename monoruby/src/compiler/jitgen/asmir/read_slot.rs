use super::*;

impl AsmIr {
    ///
    /// fetch *reg* and store in *dst*.
    ///
    /// ### destroy
    /// - rax, rcx
    ///
    pub(crate) fn fetch_to_reg(&mut self, bb: &mut BBContext, slot: SlotId, dst: GP) {
        if slot >= bb.sp {
            unreachable!("{:?} >= {:?} in fetch_to_reg()", slot, bb.sp);
        };
        bb[slot].use_as_non_float();
        match bb.slot(slot) {
            LinkMode::Xmm(xmm) => {
                if dst == GP::R15 {
                    self.writeback_acc(bb);
                }
                self.xmm2stack(xmm, slot);
                self.reg_move(GP::Rax, dst);
                bb.set_both_float(slot, xmm);
            }
            LinkMode::ConcreteValue(v) => {
                if dst == GP::R15 {
                    self.writeback_acc(bb);
                }
                self.inst.push(AsmInst::LitToReg(v, dst));
            }
            LinkMode::Both(_) | LinkMode::Stack => {
                if dst == GP::R15 {
                    self.writeback_acc(bb);
                }
                self.stack2reg(slot, dst);
            }
            LinkMode::Alias(origin) => {
                if dst == GP::R15 {
                    self.writeback_acc(bb);
                }
                self.stack2reg(origin, dst);
            }
            LinkMode::Accumulator => {
                self.reg_move(GP::R15, dst);
            }
        }
    }

    pub(crate) fn fetch_to_callee_stack(&mut self, bb: &mut BBContext, reg: SlotId, offset: i32) {
        match bb.slot(reg) {
            LinkMode::Accumulator => {
                self.reg2rsp_offset(GP::R15, offset);
            }
            _ => {
                self.fetch_to_reg(bb, reg, GP::Rax);
                self.reg2rsp_offset(GP::Rax, offset);
            }
        }
    }

    pub(crate) fn fetch_guard_array(
        &mut self,
        bb: &mut BBContext,
        slot: SlotId,
        dst: GP,
        deopt: AsmDeopt,
    ) {
        let is_array = bb.is_array_ty(slot);
        self.fetch_to_reg(bb, slot, dst);
        if !is_array {
            self.guard_array_ty(dst, deopt);
            bb.set_guard_array_ty(slot);
        }
    }

    pub(crate) fn fetch_guard_fixnum(
        &mut self,
        bb: &mut BBContext,
        slot: SlotId,
        dst: GP,
        deopt: AsmDeopt,
    ) {
        let is_fixnum = bb.is_fixnum(slot);
        self.fetch_to_reg(bb, slot, dst);
        if !is_fixnum {
            self.guard_fixnum(dst, deopt);
            bb.set_guard_fixnum(slot);
        }
    }

    pub(crate) fn writeback_acc(&mut self, bb: &mut BBContext) {
        if let Some(slot) = self.clear_r15(bb)
            && slot < bb.sp
        {
            self.acc2stack(slot);
        }
    }

    ///
    /// Read from a slot *reg* as f64, and store in xmm register.
    ///
    /// ### destroy
    /// - rdi
    ///
    pub(super) fn fetch_float_assume_integer(
        &mut self,
        bb: &mut BBContext,
        slot: SlotId,
        deopt: AsmDeopt,
    ) -> Xmm {
        bb[slot].use_as_non_float();
        match bb.slot(slot) {
            LinkMode::Both(x) | LinkMode::Xmm(x) => x,
            LinkMode::Stack => {
                // -> Both
                let x = self.store_new_both_integer(bb, slot);
                self.stack2reg(slot, GP::Rdi);
                self.int2xmm(GP::Rdi, x, deopt);
                x
            }
            LinkMode::Accumulator => {
                // -> Both
                let x = self.store_new_both_integer(bb, slot);
                self.reg2stack(GP::R15, slot);
                self.int2xmm(GP::R15, x, deopt);
                x
            }
            LinkMode::Alias(origin) => {
                // -> Both
                let x = self.store_new_both_integer(bb, origin);
                self.stack2reg(origin, GP::Rdi);
                self.int2xmm(GP::Rdi, x, deopt);
                x
            }
            LinkMode::ConcreteValue(v) => {
                if let Some(f) = v.try_float() {
                    // -> Xmm
                    let x = self.store_new_xmm(bb, slot);
                    self.f64toxmm(f, x);
                    x
                } else if let Some(i) = v.try_fixnum() {
                    // -> Both
                    let x = self.store_new_both_integer(bb, slot);
                    self.i64toboth(i, slot, x);
                    x
                } else {
                    unreachable!()
                }
            }
        }
    }

    ///
    /// Fetch *reg* as f64, and store in xmm register.
    ///
    /// ### destroy
    /// - rdi, rax
    ///
    ///
    pub(crate) fn fetch_float_assume_float(
        &mut self,
        bb: &mut BBContext,
        slot: SlotId,
        deopt: AsmDeopt,
    ) -> Xmm {
        bb[slot].use_as_float();
        match bb.slot(slot) {
            LinkMode::Both(x) | LinkMode::Xmm(x) => x,
            LinkMode::Stack => {
                // -> Both
                let x = self.store_new_both_float(bb, slot);
                self.stack2reg(slot, GP::Rdi);
                self.float2xmm(GP::Rdi, x, deopt);
                x
            }
            LinkMode::Accumulator => {
                // -> Both
                let x = self.store_new_both_float(bb, slot);
                self.reg2stack(GP::R15, slot);
                self.float2xmm(GP::R15, x, deopt);
                x
            }
            LinkMode::Alias(origin) => {
                // -> Both
                let x = self.store_new_both_float(bb, origin);
                self.stack2reg(origin, GP::Rdi);
                self.float2xmm(GP::Rdi, x, deopt);
                x
            }
            LinkMode::ConcreteValue(v) => {
                if let Some(f) = v.try_float() {
                    // -> Xmm
                    let x = self.store_new_xmm(bb, slot);
                    self.f64toxmm(f, x);
                    x
                } else if let Some(i) = v.try_fixnum() {
                    // -> Both
                    let x = self.store_new_both_float(bb, slot);
                    self.i64toboth(i, slot, x);
                    x
                } else {
                    unreachable!()
                }
            }
        }
    }
}
