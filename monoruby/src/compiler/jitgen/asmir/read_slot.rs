use super::*;

impl AsmIr {
    ///
    /// Fetch *reg* and store in a corresponding stack slot.
    ///
    /// ### destroy
    /// - rax, rcx
    pub(crate) fn write_back_slot(&mut self, bb: &mut BBContext, reg: SlotId) {
        if reg >= bb.sp {
            eprintln!("warning: {:?} >= {:?} in fetch_slot()", reg, bb.sp);
            panic!();
        };
        match bb[reg] {
            LinkMode::Xmm(freg) => {
                bb[reg] = LinkMode::Both(freg);
                self.xmm2stack(freg, vec![reg]);
            }
            LinkMode::Literal(v) => {
                bb[reg] = LinkMode::Stack;
                self.lit2stack(v, reg);
            }
            LinkMode::R15 => {
                self.clear_link(bb, reg);
                self.acc2stack(reg);
            }
            LinkMode::Alias(origin) => {
                self.clear_link(bb, reg);
                self.stack2reg(origin, GP::Rax);
                self.reg2stack(GP::Rax, reg);
            }
            LinkMode::Both(_) | LinkMode::Stack => {}
        }
    }

    pub(crate) fn write_back_slots(&mut self, bb: &mut BBContext, reg: &[SlotId]) {
        reg.iter().for_each(|r| self.write_back_slot(bb, *r));
    }

    ///
    /// Fetch from *args* to *args* + *len* - 1 and store in corresponding stack slots.
    ///
    pub(crate) fn write_back_range(&mut self, bb: &mut BBContext, args: SlotId, len: u16) {
        for reg in args.0..args.0 + len {
            self.write_back_slot(bb, SlotId::new(reg))
        }
    }

    pub(crate) fn write_back_callargs(&mut self, bb: &mut BBContext, callsite: &CallSiteInfo) {
        let CallSiteInfo {
            recv, args, len, ..
        } = callsite;
        self.write_back_slot(bb, *recv);
        self.write_back_range(bb, *args, *len as u16);
    }

    pub(crate) fn write_back_args(&mut self, bb: &mut BBContext, callsite: &CallSiteInfo) {
        let CallSiteInfo { args, len, .. } = callsite;
        self.write_back_range(bb, *args, *len as u16);
    }

    ///
    /// fetch *reg* and store in *dst*.
    ///
    /// ### destroy
    /// - rax, rcx
    ///
    pub(crate) fn fetch_to_reg(&mut self, bb: &mut BBContext, reg: SlotId, dst: GP) {
        if reg >= bb.sp {
            eprintln!("warning: {:?} >= {:?} in fetch_to_reg()", reg, bb.sp);
            panic!();
        };
        match bb[reg] {
            LinkMode::Xmm(x) => {
                if dst == GP::R15 {
                    self.writeback_acc(bb);
                }
                self.xmm2stack(x, vec![reg]);
                self.reg_move(GP::Rax, dst);
                bb[reg] = LinkMode::Both(x);
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
                self.stack2reg(reg, dst);
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
        if reg >= bb.sp {
            eprintln!("warning: {:?} >= {:?} in fetch_to_reg()", reg, bb.sp);
            panic!();
        };
        match bb[reg] {
            LinkMode::Xmm(x) => {
                self.xmm2stack(x, vec![reg]);
                self.reg2rsp_offset(GP::Rax, offset);
                bb[reg] = LinkMode::Both(x);
            }
            LinkMode::Literal(v) => {
                self.inst.push(AsmInst::LitToReg(v, GP::Rax));
                self.reg2rsp_offset(GP::Rax, offset);
            }
            LinkMode::Both(_) | LinkMode::Stack => {
                self.stack2reg(reg, GP::Rax);
                self.reg2rsp_offset(GP::Rax, offset);
            }
            LinkMode::Alias(origin) => {
                self.stack2reg(origin, GP::Rax);
                self.reg2rsp_offset(GP::Rax, offset);
            }
            LinkMode::R15 => {
                self.reg2rsp_offset(GP::R15, offset);
            }
        }
    }

    fn fetch_no_float(&mut self, bb: &mut BBContext, slot: SlotId, r: GP, deopt: AsmDeopt) {
        if slot >= bb.sp {
            eprintln!("warning: {:?} >= {:?} in fetch_to_reg()", slot, bb.sp);
            panic!();
        };
        match bb[slot] {
            LinkMode::Xmm(_) => {
                self.inst.push(AsmInst::Deopt(deopt));
            }
            LinkMode::Literal(v) => {
                if r == GP::R15 {
                    self.writeback_acc(bb);
                }
                self.inst.push(AsmInst::LitToReg(v, r));
            }
            LinkMode::Both(_) | LinkMode::Stack => {
                if r == GP::R15 {
                    self.writeback_acc(bb);
                }
                self.stack2reg(slot, r);
            }
            LinkMode::Alias(origin) => {
                if r == GP::R15 {
                    self.writeback_acc(bb);
                }
                self.stack2reg(origin, r);
            }
            LinkMode::R15 => {
                self.reg_move(GP::R15, r);
            }
        }
    }

    pub(crate) fn fetch_guard_array(
        &mut self,
        bb: &mut BBContext,
        slot: SlotId,
        r: GP,
        deopt: AsmDeopt,
    ) {
        let is_array = bb.is_array_ty(slot);
        self.fetch_no_float(bb, slot, r, deopt);
        if !is_array {
            self.guard_array_ty(r, deopt)
        }
    }

    pub(crate) fn fetch_guard_fixnum(
        &mut self,
        bb: &mut BBContext,
        slot: SlotId,
        r: GP,
        deopt: AsmDeopt,
    ) {
        let is_fixnum = bb.is_fixnum(slot);
        self.fetch_no_float(bb, slot, r, deopt);
        if !is_fixnum {
            self.guard_fixnum(r, deopt)
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
        match bb[reg] {
            LinkMode::Both(x) | LinkMode::Xmm(x) => x,
            LinkMode::Stack => {
                // -> Both
                let x = self.link_new_both(bb, reg);
                self.stack2reg(reg, GP::Rdi);
                self.int2xmm(GP::Rdi, x, deopt);
                x
            }
            LinkMode::R15 => {
                // -> Both
                let x = self.link_new_both(bb, reg);
                self.reg2stack(GP::R15, reg);
                self.int2xmm(GP::R15, x, deopt);
                x
            }
            LinkMode::Alias(origin) => {
                // -> Both
                let x = self.link_new_both(bb, origin);
                self.stack2reg(origin, GP::Rdi);
                self.int2xmm(GP::Rdi, x, deopt);
                x
            }
            LinkMode::Literal(v) => {
                if let Some(f) = v.try_float() {
                    // -> Xmm
                    let x = self.link_new_xmm(bb, reg);
                    self.f64toxmm(f, x);
                    x
                } else if let Some(i) = v.try_fixnum() {
                    // -> Both
                    let x = self.link_new_both(bb, reg);
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
        reg: SlotId,
        deopt: AsmDeopt,
    ) -> Xmm {
        match bb[reg] {
            LinkMode::Both(x) | LinkMode::Xmm(x) => x,
            LinkMode::Stack => {
                // -> Both
                let x = self.link_new_both(bb, reg);
                self.stack2reg(reg, GP::Rdi);
                self.float2xmm(GP::Rdi, x, deopt);
                x
            }
            LinkMode::R15 => {
                // -> Both
                let x = self.link_new_both(bb, reg);
                self.reg2stack(GP::R15, reg);
                self.float2xmm(GP::R15, x, deopt);
                x
            }
            LinkMode::Alias(origin) => {
                // -> Both
                let x = self.link_new_both(bb, origin);
                self.stack2reg(origin, GP::Rdi);
                self.float2xmm(GP::Rdi, x, deopt);
                x
            }
            LinkMode::Literal(v) => {
                if let Some(f) = v.try_float() {
                    // -> Xmm
                    let x = self.link_new_xmm(bb, reg);
                    self.f64toxmm(f, x);
                    x
                } else if let Some(i) = v.try_fixnum() {
                    // -> Both
                    let x = self.link_new_both(bb, reg);
                    self.i64toboth(i, reg, x);
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
