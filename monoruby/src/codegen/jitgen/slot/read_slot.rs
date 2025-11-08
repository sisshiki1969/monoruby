use super::*;

impl BBContext {
    ///
    /// load *slot* into *r*.
    ///
    /// ### destroy
    /// - rax, rcx
    ///
    /// ### panic
    /// - if *slot* is V or None.
    ///
    pub(crate) fn load(&mut self, ir: &mut AsmIr, slot: SlotId, dst: GP) {
        self.use_as_value(slot);
        match self.mode(slot) {
            LinkMode::F(xmm) => {
                if dst == GP::R15 {
                    assert!(self.r15.is_none());
                }
                // F -> Sf
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
                unreachable!("load() {:?}", self.mode(slot));
            }
        }
    }

    ///
    /// load *slot* into *opt* if not on register, and return the register.
    ///
    /// ### panic
    /// - if *slot* is V or None.
    ///
    pub(crate) fn load_or_reg(&mut self, ir: &mut AsmIr, slot: SlotId, opt: GP) -> GP {
        if let Some(r) = self.on_reg(slot) {
            r
        } else {
            self.load(ir, slot, opt);
            opt
        }
    }

    pub(crate) fn load_array_ty(&mut self, ir: &mut AsmIr, store: &Store, slot: SlotId, dst: GP) {
        self.load(ir, slot, dst);
        if !self.is_array_ty(store, slot) {
            let deopt = ir.new_deopt(self);
            ir.guard_array_ty(dst, deopt);
        }
    }

    pub(crate) fn load_fixnum(&mut self, ir: &mut AsmIr, slot: SlotId, r: GP) {
        self.load(ir, slot, r);
        self.guard_fixnum(ir, slot, r);
    }
}

impl BBContext {
    ///
    /// load *slot* as f64 into xmm register.
    ///
    /// ### destroy
    /// - rdi
    ///
    pub(crate) fn load_xmm_fixnum(&mut self, ir: &mut AsmIr, slot: SlotId) -> Xmm {
        self.use_as_float(slot);
        match self.mode(slot) {
            LinkMode::Sf(x) | LinkMode::F(x) => x,
            LinkMode::S => {
                // S -> Sf
                ir.stack2reg(slot, GP::Rdi);
                self.guard_fixnum(ir, slot, GP::Rdi);
                let x = self.set_new_Sf(slot, Guarded::Fixnum);
                ir.fixnum2xmm(GP::Rdi, x);
                x
            }
            LinkMode::G => {
                // G -> Sf
                ir.reg2stack(GP::R15, slot);
                self.guard_fixnum(ir, slot, GP::R15);
                let x = self.set_new_Sf(slot, Guarded::Fixnum);
                ir.fixnum2xmm(GP::R15, x);
                x
            }
            LinkMode::C(v) => self.load_xmm_from_C(ir, slot, v),
            LinkMode::V | LinkMode::MaybeNone | LinkMode::None => {
                unreachable!("load_xmm_fixnum() {:?}", self.mode(slot));
            }
        }
    }

    ///
    /// load *reg* as f64 into xmm register.
    ///
    /// ### destroy
    /// - rdi, rax
    ///
    ///
    pub(crate) fn load_xmm(&mut self, ir: &mut AsmIr, slot: SlotId) -> Xmm {
        let deopt = ir.new_deopt(self);
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
                ir.float_to_xmm(GP::R15, x, deopt);
                x
            }
            LinkMode::C(v) => self.load_xmm_from_C(ir, slot, v),
            LinkMode::V | LinkMode::MaybeNone | LinkMode::None => {
                unreachable!("load_xmm() {:?}", self.mode(slot));
            }
        }
    }

    #[allow(non_snake_case)]
    fn load_xmm_from_C(&mut self, ir: &mut AsmIr, slot: SlotId, v: Value) -> Xmm {
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

impl BBContext {
    ///
    /// fetch *slot* and store in callee stack with `offset`.
    ///
    /// ### destroy
    /// - rax, rcx
    ///
    /// ### panic
    /// - if *slot* is V or None.
    ///
    pub(in crate::codegen::jitgen) fn fetch_for_callee(
        &mut self,
        ir: &mut AsmIr,
        slot: SlotId,
        ofs: i32,
    ) {
        match self.mode(slot) {
            LinkMode::G => {
                self.use_as_value(slot);
                ir.reg2rsp_offset(GP::R15, ofs);
            }
            _ => {
                self.load(ir, slot, GP::Rax);
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
                self.fetch_for_callee(ir, slot, offset);
            }
            OpMode::RI(_, i) => ir.u64torsp_offset(Value::i32(i as i32).id(), offset),
        }
    }
}
