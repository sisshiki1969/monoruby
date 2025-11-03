use super::*;

impl BBContext {
    ///
    /// fetch *slot* and store in *r*.
    ///
    /// ### destroy
    /// - rax, rcx
    ///
    pub(crate) fn fetch(&mut self, ir: &mut AsmIr, slot: SlotId, r: GP) {
        //assert_ne!(r, GP::R15);
        let sp = self.sp;
        if slot >= sp {
            unreachable!("{:?} >= {:?} in fetch_for_gpr()", slot, self.sp);
        };
        self.fetch_gpr(ir, slot, r);
    }

    pub(crate) fn fetch_or_reg(&mut self, ir: &mut AsmIr, slot: SlotId, opt: GP) -> GP {
        if let Some(r) = self.on_reg(slot) {
            r
        } else {
            self.fetch(ir, slot, opt);
            opt
        }
    }

    ///
    /// fetch *slot* and store in callee stack with `offset`.
    ///
    /// ### destroy
    /// - rax, rcx
    ///
    pub(in crate::codegen::jitgen) fn fetch_for_callee(
        &mut self,
        ir: &mut AsmIr,
        slot: SlotId,
        ofs: i32,
    ) {
        if slot >= self.sp {
            unreachable!("{:?} >= {:?} in fetch_for_callee()", slot, self.sp);
        };
        self.use_as_value(slot);
        match self.mode(slot) {
            LinkMode::G => {
                ir.reg2rsp_offset(GP::R15, ofs);
            }
            _ => {
                self.fetch(ir, slot, GP::Rax);
                ir.reg2rsp_offset(GP::Rax, ofs);
            }
        }
    }

    pub(in crate::codegen::jitgen) fn fetch_rhs_for_callee(
        &mut self,
        ir: &mut AsmIr,
        mode: OpMode,
        offset: i32,
    ) {
        match mode {
            OpMode::IR(_, slot) | OpMode::RR(_, slot) => {
                if slot >= self.sp {
                    unreachable!("{:?} >= {:?} in fetch_for_callee()", slot, self.sp);
                };
                self.fetch_for_callee_stack(ir, slot, offset);
            }
            OpMode::RI(_, i) => ir.u64torsp_offset(Value::i32(i as i32).id(), offset),
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
        self.fetch(ir, slot, dst);
        if !is_array {
            ir.guard_array_ty(dst, deopt);
        }
    }

    pub(crate) fn fetch_fixnum(&mut self, ir: &mut AsmIr, slot: SlotId, r: GP, deopt: AsmDeopt) {
        self.fetch(ir, slot, r);
        self.guard_fixnum(ir, slot, r, deopt);
    }
}

impl SlotContext {
    ///
    /// fetch *slot* and store in *dst*.
    ///
    /// ### destroy
    /// - rax, rcx
    ///
    fn fetch_gpr(&mut self, ir: &mut AsmIr, slot: SlotId, dst: GP) {
        self.use_as_value(slot);
        match self.mode(slot) {
            LinkMode::F(xmm) => {
                if dst == GP::R15 {
                    assert!(self.r15.is_none());
                }
                // Xmm(x) -> Both(x)
                ir.xmm2stack(xmm, slot);
                ir.reg_move(GP::Rax, dst);
                self.set_Sf_float(slot, xmm);
            }
            LinkMode::C(v) => {
                if dst == GP::R15 {
                    assert!(self.r15.is_none());
                }
                ir.lit2reg(v, dst);
            }
            LinkMode::Sf(_) | LinkMode::S => {
                if dst == GP::R15 {
                    assert!(self.r15.is_none());
                }
                ir.stack2reg(slot, dst);
            }
            LinkMode::G => {
                ir.reg_move(GP::R15, dst);
            }
            LinkMode::MaybeNone => {
                ir.stack2reg(slot, dst);
            }
            LinkMode::V | LinkMode::None => {
                unreachable!("fetch_gpr() {:?}", self.mode(slot));
            }
        }
    }

    ///
    /// fetch *slot* and store in callee stack with `offset`.
    ///
    /// ### destroy
    /// - rax, rcx
    ///
    fn fetch_for_callee_stack(&mut self, ir: &mut AsmIr, slot: SlotId, offset: i32) {
        match self.mode(slot) {
            LinkMode::G => {
                self.use_as_value(slot);
                ir.reg2rsp_offset(GP::R15, offset);
            }
            _ => {
                self.fetch_gpr(ir, slot, GP::Rax);
                ir.reg2rsp_offset(GP::Rax, offset);
            }
        }
    }

    ///
    /// Read from a *slot* as f64, and store in xmm register.
    ///
    /// ### destroy
    /// - rdi
    ///
    pub(crate) fn fetch_integer_for_xmm(
        &mut self,
        ir: &mut AsmIr,
        slot: SlotId,
        deopt: AsmDeopt,
    ) -> Xmm {
        self.use_as_float(slot);
        match self.mode(slot) {
            LinkMode::Sf(x) | LinkMode::F(x) => x,
            LinkMode::S => {
                // -> Sf
                ir.stack2reg(slot, GP::Rdi);
                self.guard_fixnum(ir, slot, GP::Rdi, deopt);
                let x = self.set_new_Sf(slot, Guarded::Fixnum);
                ir.fixnum2xmm(GP::Rdi, x);
                x
            }
            LinkMode::G => {
                // -> Sf
                ir.reg2stack(GP::R15, slot);
                self.guard_fixnum(ir, slot, GP::R15, deopt);
                let x = self.set_new_Sf(slot, Guarded::Fixnum);
                ir.reg_move(GP::R15, GP::Rdi);
                ir.fixnum2xmm(GP::Rdi, x);
                x
            }
            LinkMode::C(v) => self.fetch_float_concrete_value_for_xmm(ir, slot, v),
            LinkMode::V | LinkMode::MaybeNone | LinkMode::None => {
                unreachable!("fetch_integer_for_xmm() {:?}", self.mode(slot));
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
    pub(crate) fn fetch_float_for_xmm(
        &mut self,
        ir: &mut AsmIr,
        slot: SlotId,
        deopt: AsmDeopt,
    ) -> Xmm {
        self.use_as_float(slot);
        match self.mode(slot) {
            LinkMode::Sf(x) | LinkMode::F(x) => x,
            LinkMode::S => {
                // -> Sf
                let x = self.set_new_Sf(slot, Guarded::Float);
                ir.stack2reg(slot, GP::Rdi);
                ir.float_to_xmm(GP::Rdi, x, deopt);
                x
            }
            LinkMode::G => {
                // -> Sf
                let x = self.set_new_Sf(slot, Guarded::Float);
                ir.reg2stack(GP::R15, slot);
                ir.reg_move(GP::R15, GP::Rdi);
                ir.float_to_xmm(GP::Rdi, x, deopt);
                x
            }
            LinkMode::C(v) => self.fetch_float_concrete_value_for_xmm(ir, slot, v),
            LinkMode::V | LinkMode::MaybeNone | LinkMode::None => {
                unreachable!("fetch_float_for_xmm() {:?}", self.mode(slot));
            }
        }
    }

    fn fetch_float_concrete_value_for_xmm(
        &mut self,
        ir: &mut AsmIr,
        slot: SlotId,
        v: Value,
    ) -> Xmm {
        if let Some(f) = v.try_float() {
            // -> F
            let x = self.set_new_F(slot);
            ir.f64_to_xmm(f, x);
            x
        } else if let Some(i) = v.try_fixnum() {
            // -> Sf
            let x = self.set_new_Sf(slot, Guarded::Fixnum);
            ir.i64_to_stack_and_xmm(i, slot, x);
            x
        } else {
            unreachable!()
        }
    }
}
