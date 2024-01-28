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
            eprintln!("warning: {:?} >= {:?} in fetch_to_reg()", slot, bb.sp);
            panic!();
        };
        match bb.slot(slot) {
            LinkMode::Xmm(xmm) => {
                if dst == GP::R15 {
                    self.writeback_acc(bb);
                }
                self.xmm2stack(xmm, vec![slot]);
                self.reg_move(GP::Rax, dst);
                bb.set_both_float(slot, xmm);
            }
            LinkMode::Literal(v) => {
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
            LinkMode::R15 => {
                self.reg_move(GP::R15, dst);
            }
        }
    }

    pub(crate) fn fetch_to_rsp_offset(&mut self, bb: &mut BBContext, reg: SlotId, offset: i32) {
        match bb.slot(reg) {
            LinkMode::R15 => {
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

    pub(in crate::compiler::jitgen) fn writeback_acc(&mut self, bb: &mut BBContext) {
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
    fn fetch_float_assume_integer(
        &mut self,
        bb: &mut BBContext,
        reg: SlotId,
        deopt: AsmDeopt,
    ) -> Xmm {
        match bb.slot(reg) {
            LinkMode::Both(x) | LinkMode::Xmm(x) => x,
            LinkMode::Stack => {
                // -> Both
                let x = self.store_new_both(bb, reg, Guarded::Fixnum);
                self.stack2reg(reg, GP::Rdi);
                self.int2xmm(GP::Rdi, x, deopt);
                x
            }
            LinkMode::R15 => {
                // -> Both
                let x = self.store_new_both(bb, reg, Guarded::Fixnum);
                self.reg2stack(GP::R15, reg);
                self.int2xmm(GP::R15, x, deopt);
                x
            }
            LinkMode::Alias(origin) => {
                // -> Both
                let x = self.store_new_both(bb, origin, Guarded::Fixnum);
                self.stack2reg(origin, GP::Rdi);
                self.int2xmm(GP::Rdi, x, deopt);
                x
            }
            LinkMode::Literal(v) => {
                if let Some(f) = v.try_float() {
                    // -> Xmm
                    let x = self.store_new_xmm(bb, reg);
                    self.f64toxmm(f, x);
                    x
                } else if let Some(i) = v.try_fixnum() {
                    // -> Both
                    let x = self.store_new_both(bb, reg, Guarded::Fixnum);
                    self.i64toboth(i, reg, x);
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
        match bb.slot(slot) {
            LinkMode::Both(x) | LinkMode::Xmm(x) => x,
            LinkMode::Stack => {
                // -> Both
                let x = self.store_new_both(bb, slot, Guarded::Float);
                self.stack2reg(slot, GP::Rdi);
                self.float2xmm(GP::Rdi, x, deopt);
                x
            }
            LinkMode::R15 => {
                // -> Both
                let x = self.store_new_both(bb, slot, Guarded::Float);
                self.reg2stack(GP::R15, slot);
                self.float2xmm(GP::R15, x, deopt);
                x
            }
            LinkMode::Alias(origin) => {
                // -> Both
                let x = self.store_new_both(bb, origin, Guarded::Float);
                self.stack2reg(origin, GP::Rdi);
                self.float2xmm(GP::Rdi, x, deopt);
                x
            }
            LinkMode::Literal(v) => {
                if let Some(f) = v.try_float() {
                    // -> Xmm
                    let x = self.store_new_xmm(bb, slot);
                    self.f64toxmm(f, x);
                    x
                } else if let Some(i) = v.try_fixnum() {
                    // -> Both
                    let x = self.store_new_both(bb, slot, Guarded::Float);
                    self.i64toboth(i, slot, x);
                    x
                } else {
                    unreachable!()
                }
            }
        }
    }

    ///
    /// Read from a slot *reg* as f64, and store in xmm register.
    ///
    /// ### destroy
    /// - rdi, rax
    ///
    fn fetch_float_assume(
        &mut self,
        bb: &mut BBContext,
        rhs: SlotId,
        class: Option<ClassId>,
        deopt: AsmDeopt,
    ) -> Xmm {
        match class {
            Some(INTEGER_CLASS) => self.fetch_float_assume_integer(bb, rhs, deopt),
            Some(FLOAT_CLASS) => self.fetch_float_assume_float(bb, rhs, deopt),
            _ => unreachable!(),
        }
    }

    pub(super) fn fetch_float_binary(
        &mut self,
        bb: &mut BBContext,
        lhs: SlotId,
        rhs: SlotId,
        pc: BcPc,
        deopt: AsmDeopt,
    ) -> (Xmm, Xmm) {
        if lhs != rhs {
            (
                self.fetch_float_assume(bb, lhs, pc.classid1(), deopt),
                self.fetch_float_assume(bb, rhs, pc.classid2(), deopt),
            )
        } else {
            let lhs = self.fetch_float_assume(bb, lhs, pc.classid1(), deopt);
            (lhs, lhs)
        }
    }
}
