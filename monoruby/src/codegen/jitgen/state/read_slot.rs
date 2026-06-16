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

///
/// A GP-register load produced by `load` (item ②, step 2): how to materialize a
/// slot's boxed `Value` into a general-purpose register. Like the other typed-IR
/// records, the *what* is decided by the analysis half (`load_state`) and
/// emitted by the codegen half via [`GpLoad::emit`].
///
#[derive(Debug, Clone, Copy, PartialEq)]
enum GpLoad {
    /// `F` slot: box the xmm to its stack home (leaving the boxed value in rax),
    /// then move rax into `dst`. `fpr2stack(fpr, slot); reg_move(rax, dst)`.
    FprBox(FPReg, SlotId, GP),
    /// `lit2reg(value, dst)`
    Lit(Value, GP),
    /// `stack2reg(slot, dst)`
    Stack(SlotId, GP),
    /// `reg_move(r15, dst)`
    Acc(GP),
}

impl GpLoad {
    fn emit(self, ir: &mut AsmIr) {
        match self {
            GpLoad::FprBox(fpr, slot, dst) => {
                ir.fpr2stack(fpr, slot);
                ir.reg_move(GP::Rax, dst);
            }
            GpLoad::Lit(v, dst) => ir.lit2reg(v, dst),
            GpLoad::Stack(slot, dst) => ir.stack2reg(slot, dst),
            GpLoad::Acc(dst) => ir.reg_move(GP::R15, dst),
        }
    }
}

///
/// A deopt-carrying *fixnum* unbox load produced by `load_xmm_fixnum` (item ②,
/// step 2). Mirrors [`XmmLoad`] but the boxed value is known to be an `Integer`,
/// so the conversion is `fixnum2fpr` (no float reinterpret) preceded by an
/// explicit `Integer` class guard.
///
/// This is the case doc §9 once flagged as "needs the sequence model": its
/// `S`/`G` arms interleave a load (`stack2reg`), a `new_deopt`, a guard and the
/// conversion. The interleaving dissolves because `stack2reg`/`reg2stack` are
/// *pure emits* on `ir` that never touch the frame's placement state, so
/// `new_deopt` **commutes** with them — the deopt can be created up front (as in
/// `load_xmm`) and the load deferred into this record's emit half. The guard is
/// folded in as a bool (`guard_class_state`'s verdict); when set, the codegen
/// half pushes `GuardClass` with the wrapper-supplied deopt.
///
#[derive(Debug, Clone, Copy, PartialEq)]
enum XmmFixnumLoad {
    /// already in an xmm (`Sf` / `F`) — nothing to emit
    None,
    /// `stack2reg(slot, Rdi); [GuardClass(Rdi)]; fixnum2fpr(Rdi, x)`
    FromStack(FPReg, bool),
    /// `reg2stack(R15, slot); [GuardClass(R15)]; fixnum2fpr(R15, x)`
    FromAcc(FPReg, bool),
    /// guard-free numeric literal (`LinkMode::C`) — reuses the [`XmmLoad`] record
    Numeric(XmmLoad),
}

impl XmmFixnumLoad {
    /// `deopt` is required by the guarded (`FromStack` / `FromAcc` with the bool
    /// set) variants and `None` for the guard-free ones.
    fn emit(self, ir: &mut AsmIr, slot: SlotId, deopt: Option<AsmDeopt>) {
        match self {
            XmmFixnumLoad::None => {}
            XmmFixnumLoad::FromStack(x, guard) => {
                ir.stack2reg(slot, GP::Rdi);
                if guard {
                    ir.push(AsmInst::GuardClass(GP::Rdi, INTEGER_CLASS, deopt.unwrap()));
                }
                ir.fixnum2fpr(GP::Rdi, x);
            }
            XmmFixnumLoad::FromAcc(x, guard) => {
                ir.reg2stack(GP::R15, slot);
                if guard {
                    ir.push(AsmInst::GuardClass(GP::R15, INTEGER_CLASS, deopt.unwrap()));
                }
                ir.fixnum2fpr(GP::R15, x);
            }
            XmmFixnumLoad::Numeric(load) => load.emit(ir, slot, None),
        }
    }
}

impl AbstractFrame {
    ///
    /// load *slot* into *dst*.
    ///
    /// ### destroy
    /// - rax, rcx
    ///
    /// ### panic
    /// - if *slot* is V or None.
    ///
    pub(crate) fn load(&mut self, ir: &mut AsmIr, slot: SlotId, dst: GP) {
        self.load_state(slot, dst).emit(ir);
    }

    ///
    /// Analysis half of [`Self::load`] (item ②, step 2): the abstract-state
    /// transition (only the `F` arm changes state, to `Sf`) plus the GP load to
    /// emit. Pure state.
    ///
    fn load_state(&mut self, slot: SlotId, dst: GP) -> GpLoad {
        self.use_as_value(slot);
        match self.mode(slot) {
            LinkMode::F(fpr) => {
                if dst == GP::R15 {
                    assert!(self.no_r15());
                }
                // F -> Sf
                self.set_Sf_float(slot, fpr);
                GpLoad::FprBox(fpr, slot, dst)
            }
            LinkMode::C(v) => {
                if dst == GP::R15 {
                    assert!(self.no_r15());
                }
                GpLoad::Lit(v, dst)
            }
            LinkMode::Sf(_, _) | LinkMode::S(_) => {
                if dst == GP::R15 {
                    assert!(self.no_r15());
                }
                GpLoad::Stack(slot, dst)
            }
            LinkMode::G(_) => GpLoad::Acc(dst),
            LinkMode::MaybeNone => GpLoad::Stack(slot, dst),
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
        // Only the `S`/`G` arms guard, so a deopt is created only for them — and
        // *before* the state transition, so its write-back snapshot is the
        // pre-load placement (cf. `load_xmm`). `use_as_value` (below, in the
        // state half) only touches liveness, so this peek is stable; and the
        // later `stack2reg`/`reg2stack` are pure emits that don't perturb the
        // snapshot. The guard-free `Sf`/`F`/`C` arms create no deopt, exactly as
        // before.
        let deopt = matches!(self.mode(slot), LinkMode::S(_) | LinkMode::G(_))
            .then(|| ir.new_deopt(self));
        let (x, load) = self.load_xmm_fixnum_state(slot);
        load.emit(ir, slot, deopt);
        x
    }

    ///
    /// Analysis half of [`Self::load_xmm_fixnum`] (item ②, step 2): refine the
    /// slot to `Integer`, allocate the xmm and bind the slot, returning the
    /// register plus the conversion (and whether a runtime guard is needed) to
    /// emit. Pure state.
    ///
    fn load_xmm_fixnum_state(&mut self, slot: SlotId) -> (FPReg, XmmFixnumLoad) {
        self.use_as_value(slot);
        match self.mode(slot) {
            LinkMode::Sf(x, _) | LinkMode::F(x) => (x, XmmFixnumLoad::None),
            LinkMode::S(_) => {
                // S -> Sf. Refine the type first (its guard verdict) then take
                // the placement; `set_new_Sf` overwrites the refined `S` guarded
                // with `Sf(Fixnum)`, exactly as `guard_fixnum` + `set_new_Sf` did.
                let guard = self.guard_class_state(slot, INTEGER_CLASS);
                let x = self.set_new_Sf(slot, SfGuarded::Fixnum);
                (x, XmmFixnumLoad::FromStack(x, guard))
            }
            LinkMode::G(_) => {
                // G -> Sf
                let guard = self.guard_class_state(slot, INTEGER_CLASS);
                let x = self.set_new_Sf(slot, SfGuarded::Fixnum);
                (x, XmmFixnumLoad::FromAcc(x, guard))
            }
            LinkMode::C(v) => {
                let (x, load) = self.load_xmm_from_C_state(slot, v);
                (x, XmmFixnumLoad::Numeric(load))
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
