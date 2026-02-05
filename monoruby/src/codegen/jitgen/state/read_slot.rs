use super::*;

impl AbstractFrame {
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
                    assert!(self.no_r15());
                }
                // F -> Sf
                ir.xmm2stack(xmm, slot);
                ir.reg_move(GP::Rax, dst);
                self.set_Sf_float(slot, xmm);
            }
            LinkMode::C(v) => {
                if dst == GP::R15 {
                    assert!(self.no_r15());
                }
                ir.lit2reg(v, dst);
            }
            LinkMode::Sf(_, _) | LinkMode::S(_) => {
                if dst == GP::R15 {
                    assert!(self.no_r15());
                }
                ir.stack2reg(slot, dst);
            }
            LinkMode::G(_) => {
                ir.reg_move(GP::R15, dst);
            }
            LinkMode::MaybeNone => {
                ir.stack2reg(slot, dst);
            }
            LinkMode::V | LinkMode::None => {
                unreachable!("load() {:?} {:?}: {:?}", slot, self.mode(slot), self);
            }
        }
    }

    ///
    /// load *slot* into *opt* if not on register, and return the register.
    ///
    /// ### panic
    /// - if *slot* is V or None.
    ///
    pub(in crate::codegen::jitgen) fn load_or_reg(
        &mut self,
        ir: &mut AsmIr,
        slot: SlotId,
        opt: GP,
    ) -> GP {
        if let Some(r) = self.on_reg(slot) {
            r
        } else {
            self.load(ir, slot, opt);
            opt
        }
    }

    pub(in crate::codegen::jitgen) fn load_array_ty(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        slot: SlotId,
        dst: GP,
    ) {
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

impl AbstractFrame {
    ///
    /// load *slot* as f64 into xmm register.
    ///
    /// ### destroy
    /// - rdi
    ///
    pub(crate) fn load_xmm_fixnum(&mut self, ir: &mut AsmIr, slot: SlotId) -> Xmm {
        self.use_as_value(slot);
        match self.mode(slot) {
            LinkMode::Sf(x, _) | LinkMode::F(x) => x,
            LinkMode::S(_) => {
                // S -> Sf
                ir.stack2reg(slot, GP::Rdi);
                self.guard_fixnum(ir, slot, GP::Rdi);
                let x = self.set_new_Sf(slot, SfGuarded::Fixnum);
                ir.fixnum2xmm(GP::Rdi, x);
                x
            }
            LinkMode::G(_) => {
                // G -> Sf
                ir.reg2stack(GP::R15, slot);
                self.guard_fixnum(ir, slot, GP::R15);
                let x = self.set_new_Sf(slot, SfGuarded::Fixnum);
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
            LinkMode::Sf(x, _) | LinkMode::F(x) => x,
            LinkMode::S(_) => {
                // -> Sf
                let x = self.set_new_Sf(slot, SfGuarded::Float);
                ir.stack2reg(slot, GP::Rdi);
                ir.float_to_xmm(GP::Rdi, x, deopt);
                x
            }
            LinkMode::G(_) => {
                // -> Sf
                let x = self.set_new_Sf(slot, SfGuarded::Float);
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
            self.load_xmm_from_f64(ir, slot, f)
        } else if let Some(i) = v.try_fixnum() {
            // -> Sf
            let x = self.set_new_Sf(slot, SfGuarded::Fixnum);
            ir.i64_to_stack_and_xmm(i, slot, x);
            x
        } else {
            unreachable!()
        }
    }

    fn load_xmm_from_f64(&mut self, ir: &mut AsmIr, slot: SlotId, f: f64) -> Xmm {
        let x = self.set_new_F(slot);
        ir.f64_to_xmm(f, x);
        x
    }
}

impl AbstractFrame {
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
            LinkMode::G(_) => {
                self.use_as_value(slot);
                ir.reg2rsp_offset(GP::R15, ofs);
            }
            _ => {
                self.load(ir, slot, GP::Rax);
                ir.reg2rsp_offset(GP::Rax, ofs);
            }
        }
    }

    pub(in crate::codegen::jitgen) fn fetch_rest_for_callee(
        &mut self,
        ir: &mut AsmIr,
        src: SlotId,
        len: usize,
        ofs: i32,
    ) {
        self.write_back_range(ir, src, len as u16);
        ir.create_array(self, src, len);
        ir.reg2rsp_offset(GP::Rax, ofs);
    }
}
