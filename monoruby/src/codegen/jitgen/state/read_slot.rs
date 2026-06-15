use super::*;

///
/// A deopt-carrying unbox load produced by `load_xmm` (item ②, step 2): the
/// conversion to emit when materializing a slot's value into an FP register.
///
/// The `FromStack` / `FromAcc` variants need a guard (their `float_to_fpr`
/// deopts if the boxed value is not actually a `Float`); the deopt is supplied
/// by the codegen wrapper, **not** frozen in the record — the analysis half only
/// decided the conversion, while the codegen side creates the side-exit from the
/// placement state at this point (see doc/regalloc_separation.md §9). The
/// numeric-literal variants are guard-free.
///
#[derive(Debug, Clone, Copy, PartialEq)]
enum XmmLoad {
    /// already in an xmm (`Sf` / `F`) — nothing to emit
    None,
    /// `stack2reg(slot, Rdi); float_to_fpr(Rdi, x, deopt)`
    FromStack(FPReg),
    /// `reg2stack(R15, slot); float_to_fpr(R15, x, deopt)`
    FromAcc(FPReg),
    /// `f64_to_fpr(f, x)` (guard-free)
    FromF64(f64, FPReg),
    /// `i64_to_stack_and_fpr(i, slot, x)` (guard-free)
    FromFixnum(i64, FPReg),
}

impl XmmLoad {
    /// `deopt` is required by the guarded (`FromStack` / `FromAcc`) variants and
    /// ignored by the guard-free ones; the guard-free `load_xmm_from_C_state`
    /// path passes `None`.
    fn emit(self, ir: &mut AsmIr, slot: SlotId, deopt: Option<AsmDeopt>) {
        match self {
            XmmLoad::None => {}
            XmmLoad::FromStack(x) => {
                ir.stack2reg(slot, GP::Rdi);
                ir.float_to_fpr(GP::Rdi, x, deopt.unwrap());
            }
            XmmLoad::FromAcc(x) => {
                ir.reg2stack(GP::R15, slot);
                ir.float_to_fpr(GP::R15, x, deopt.unwrap());
            }
            XmmLoad::FromF64(f, x) => ir.f64_to_fpr(f, x),
            XmmLoad::FromFixnum(i, x) => ir.i64_to_stack_and_fpr(i, slot, x),
        }
    }
}

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
            LinkMode::F(fpr) => {
                if dst == GP::R15 {
                    assert!(self.no_r15());
                }
                // F -> Sf
                ir.fpr2stack(fpr, slot);
                ir.reg_move(GP::Rax, dst);
                self.set_Sf_float(slot, fpr);
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
    pub(crate) fn load_xmm_fixnum(&mut self, ir: &mut AsmIr, slot: SlotId) -> FPReg {
        self.use_as_value(slot);
        match self.mode(slot) {
            LinkMode::Sf(x, _) | LinkMode::F(x) => x,
            LinkMode::S(_) => {
                // S -> Sf
                ir.stack2reg(slot, GP::Rdi);
                self.guard_fixnum(ir, slot, GP::Rdi);
                let x = self.set_new_Sf(slot, SfGuarded::Fixnum);
                ir.fixnum2fpr(GP::Rdi, x);
                x
            }
            LinkMode::G(_) => {
                // G -> Sf
                ir.reg2stack(GP::R15, slot);
                self.guard_fixnum(ir, slot, GP::R15);
                let x = self.set_new_Sf(slot, SfGuarded::Fixnum);
                ir.fixnum2fpr(GP::R15, x);
                x
            }
            LinkMode::C(v) => {
                // Guard-free (numeric literal): no deopt needed.
                let (x, load) = self.load_xmm_from_C_state(slot, v);
                load.emit(ir, slot, None);
                x
            }
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
    pub(crate) fn load_xmm(&mut self, ir: &mut AsmIr, slot: SlotId) -> FPReg {
        // The deopt is created *before* the state transition, so its write-back
        // snapshot (`get_write_back`) is the pre-load placement — see the
        // deopt-carrying-transfer note in doc/regalloc_separation.md §9. The
        // record carries only the conversion; the deopt is supplied here (the
        // codegen side), not frozen into the record.
        let deopt = ir.new_deopt(self);
        let (x, load) = self.load_xmm_state(slot);
        load.emit(ir, slot, Some(deopt));
        x
    }

    ///
    /// Analysis half of [`Self::load_xmm`] (item ②, step 2): perform the
    /// abstract-state transition (allocate the xmm, bind the slot) and return
    /// the allocated register plus the conversion to emit. Pure state.
    ///
    fn load_xmm_state(&mut self, slot: SlotId) -> (FPReg, XmmLoad) {
        self.use_as_float(slot);
        match self.mode(slot) {
            LinkMode::Sf(x, _) | LinkMode::F(x) => (x, XmmLoad::None),
            LinkMode::S(_) => {
                // -> Sf
                let x = self.set_new_Sf(slot, SfGuarded::Float);
                (x, XmmLoad::FromStack(x))
            }
            LinkMode::G(_) => {
                // -> Sf
                let x = self.set_new_Sf(slot, SfGuarded::Float);
                (x, XmmLoad::FromAcc(x))
            }
            LinkMode::C(v) => self.load_xmm_from_C_state(slot, v),
            LinkMode::V | LinkMode::MaybeNone | LinkMode::None => {
                unreachable!("load_xmm() {:?}", self.mode(slot));
            }
        }
    }

    #[allow(non_snake_case)]
    fn load_xmm_from_C_state(&mut self, slot: SlotId, v: Value) -> (FPReg, XmmLoad) {
        // `LinkMode::C` may hold any Value; xmm loads only ever come from
        // numeric literals (fixnum / float / heap Float), so anything else
        // is a bug at the call site.
        match v.unpack() {
            RV::Float(f) => {
                // -> F
                let x = self.set_new_F(slot);
                (x, XmmLoad::FromF64(f, x))
            }
            RV::Fixnum(i) => {
                // -> Sf
                let x = self.set_new_Sf(slot, SfGuarded::Fixnum);
                (x, XmmLoad::FromFixnum(i, x))
            }
            _ => {
                unreachable!("load_xmm_from_C() {:?}", v);
            }
        }
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
        for i in src..src + len {
            self.use_as_value(i);
        }
        self.write_back_range(ir, src, len as u16);
        ir.create_array(src, len);
        ir.reg2rsp_offset(GP::Rax, ofs);
    }

    pub(in crate::codegen::jitgen) fn fetch_kwrest_for_callee(
        &mut self,
        ir: &mut AsmIr,
        rest_kw: Vec<(SlotId, IdentId)>,
        ofs: i32,
    ) {
        for (slot, _) in &rest_kw {
            self.use_as_value(*slot);
            self.write_back_slot(ir, *slot);
        }
        if rest_kw.is_empty() {
            ir.lit2reg(Value::nil(), GP::Rax);
        } else {
            ir.kw_rest(rest_kw);
        }
        ir.reg2rsp_offset(GP::Rax, ofs);
    }
}
