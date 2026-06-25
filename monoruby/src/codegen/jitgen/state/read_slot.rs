use super::*;
use crate::bytecodegen::BinOpK;

///
/// The **deopt program point** a guarded transfer record carries (doc §9): the
/// bytecode `pc` and the write-back placement snapshot needed to rebuild the
/// side-exit if the float/Integer guard fails.
///
/// This is the codegen-*independent* replacement for a frozen `AsmDeopt` (an
/// index into the codegen pass's `side_exit` table). The analysis half records
/// the point (pure values it already tracks); the emit/codegen half materializes
/// the actual side-exit from it via [`AsmIr::deopt_from_point`]. With this the
/// `TransferIR` stream no longer embeds any codegen-pass state — the prerequisite
/// for replaying it from a standalone lowering pass.
///
#[derive(Debug, Clone, PartialEq)]
pub(in crate::codegen::jitgen) struct DeoptPoint {
    pc: BytecodePtr,
    write_back: WriteBack,
}

impl DeoptPoint {
    pub(super) fn new(pc: BytecodePtr, write_back: WriteBack) -> Self {
        Self { pc, write_back }
    }

    pub(in crate::codegen::jitgen) fn pc(&self) -> BytecodePtr {
        self.pc
    }

    pub(in crate::codegen::jitgen) fn write_back(&self) -> &WriteBack {
        &self.write_back
    }
}

///
/// A deopt-carrying unbox load produced by `load_fpr` (item ②, step 2): the
/// conversion to emit when materializing a slot's value into an FP register.
///
/// The `FromStack` / `FromAcc` variants need a guard (their `float_to_fpr`
/// deopts if the boxed value is not actually a `Float`); the emit half
/// materializes the side-exit from the record's [`DeoptPoint`] program point,
/// **not** a frozen `AsmDeopt` — the analysis half only decided the conversion
/// and recorded the point (see doc/regalloc_separation.md §9). The
/// numeric-literal variants are guard-free.
///
#[derive(Debug, Clone, Copy, PartialEq)]
pub(in crate::codegen::jitgen) enum FprLoad {
    /// already in an fpr (`Sf` / `F`) — nothing to emit
    None,
    /// `<load Rdi>; float_to_fpr(Rdi, x, deopt)`. When the slot has a live GP
    /// resident the value is flushed home (`reg2stack`, to keep the `Sf`
    /// invariant) and read into Rdi from the register (`reg_move`); otherwise it
    /// is read from the stack home (`stack2reg`).
    FromStack(FPReg, Option<GP>),
    /// `f64_to_fpr(f, x)` (guard-free)
    FromF64(f64, FPReg),
    /// `i64_to_stack_and_fpr(i, slot, x)` (guard-free)
    FromFixnum(i64, FPReg),
}

impl FprLoad {
    /// `deopt` (the program point) is required by the guarded (`FromStack` /
    /// `FromAcc`) variants and ignored by the guard-free ones; the guard-free
    /// `load_fpr_from_C_state` path passes `None`. The side-exit is materialized
    /// here (codegen side) via [`AsmIr::deopt_from_point`], matching the old
    /// "create the deopt first" order so the `side_exit` table is identical.
    fn emit(self, ir: &mut AsmIr, slot: SlotId, deopt: Option<&DeoptPoint>) {
        match self {
            FprLoad::None => {}
            FprLoad::FromStack(x, gp) => {
                let deopt = ir.deopt_from_point(deopt.unwrap());
                match gp {
                    // The slot transitions `S -> Sf`, whose contract is that its
                    // stack home holds the canonical boxed value (a later
                    // fpr-pressure demote `Sf -> S` emits no spill — see
                    // `try_alloc_fpr_ctx` phase 1). A GP resident's value lives
                    // only in `reg`, so flush it home here before reading it into
                    // Rdi for the conversion; otherwise the home stays stale and
                    // the value is lost on demote.
                    Some(reg) => {
                        ir.reg2stack(reg, slot);
                        ir.reg_move(reg, GP::Rdi);
                    }
                    None => ir.stack2reg(slot, GP::Rdi),
                }
                ir.float_to_fpr(GP::Rdi, x, deopt);
            }
            FprLoad::FromF64(f, x) => ir.f64_to_fpr(f, x),
            FprLoad::FromFixnum(i, x) => ir.i64_to_stack_and_fpr(i, slot, x),
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
pub(in crate::codegen::jitgen) enum GpLoad {
    /// `F` slot: box the fpr to its stack home (leaving the boxed value in rax),
    /// then move rax into `dst`. `fpr2stack(fpr, slot); reg_move(rax, dst)`.
    FprBox(FPReg, SlotId, GP),
    /// `lit2reg(value, dst)`
    Lit(Value, GP),
    /// `stack2reg(slot, dst)`
    Stack(SlotId, GP),
    /// The slot is a live GP resident (an integer binop result still in a
    /// register): move it from the resident register instead of reading its
    /// possibly-stale stack home. `reg_move(src, dst)`.
    FromGp(GP, GP),
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
            GpLoad::FromGp(src, dst) => ir.reg_move(src, dst),
        }
    }
}

///
/// A deopt-carrying *fixnum* unbox load produced by `load_fpr_fixnum` (item ②,
/// step 2). Mirrors [`FprLoad`] but the boxed value is known to be an `Integer`,
/// so the conversion is `fixnum2fpr` (no float reinterpret) preceded by an
/// explicit `Integer` class guard.
///
/// This is the case doc §9 once flagged as "needs the sequence model": its
/// `S`/`G` arms interleave a load (`stack2reg`), a `new_deopt`, a guard and the
/// conversion. The interleaving dissolves because `stack2reg`/`reg2stack` are
/// *pure emits* on `ir` that never touch the frame's placement state, so
/// `new_deopt` **commutes** with them — the deopt can be created up front (as in
/// `load_fpr`) and the load deferred into this record's emit half. The guard is
/// folded in as a bool (`guard_class_state`'s verdict); when set, the codegen
/// half pushes `GuardClass` with the wrapper-supplied deopt.
///
#[derive(Debug, Clone, Copy, PartialEq)]
pub(in crate::codegen::jitgen) enum FprFixnumLoad {
    /// already in an fpr (`Sf` / `F`) — nothing to emit
    None,
    /// `<load Rdi>; [GuardClass(Rdi)]; fixnum2fpr(Rdi, x)`. When the slot has a
    /// live GP resident — the integer side of a mixed `Integer op Float` is
    /// typically still in a register from a prior integer op — the value is
    /// flushed home (`reg2stack`, to keep the `Sf` invariant) and read into Rdi
    /// from the register (`reg_move`); else it is read from its stack home
    /// (`stack2reg`).
    FromStack(FPReg, bool, Option<GP>),
    /// guard-free numeric literal (`LinkMode::C`) — reuses the [`FprLoad`] record
    Numeric(FprLoad),
}

impl FprFixnumLoad {
    /// `deopt` (the program point) is `Some` for the guarded `S` arm and
    /// `None` for the guard-free ones. The side-exit is materialized for the
    /// `S` arm whenever the point is `Some` — even if `guard` is false, so
    /// the `side_exit` table stays identical to the pre-split wrapper, which
    /// created the deopt unconditionally for those arms.
    fn emit(self, ir: &mut AsmIr, slot: SlotId, deopt: Option<&DeoptPoint>) {
        match self {
            FprFixnumLoad::None => {}
            FprFixnumLoad::FromStack(x, guard, gp) => {
                let deopt = deopt.map(|p| ir.deopt_from_point(p));
                match gp {
                    // See `FprLoad::FromStack`: the slot becomes `Sf`, so its
                    // stack home must hold the canonical boxed value. Flush the
                    // resident home before reading it into Rdi.
                    Some(reg) => {
                        ir.reg2stack(reg, slot);
                        ir.reg_move(reg, GP::Rdi);
                    }
                    None => ir.stack2reg(slot, GP::Rdi),
                }
                if guard {
                    ir.push(AsmInst::GuardClass(GP::Rdi, INTEGER_CLASS, deopt.unwrap()));
                }
                ir.fixnum2fpr(GP::Rdi, x);
            }
            FprFixnumLoad::Numeric(load) => load.emit(ir, slot, None),
        }
    }
}

///
/// The unified **typed-IR stream element** (item ②, step 2 — record collection).
/// Every transfer/eviction primitive's analysis half produces one of these; the
/// codegen pass funnels them through [`AsmIr::transfer`], which both *collects*
/// the record (building the typed-IR stream that record-driven lowering will
/// replay) and emits it via [`TransferIR::emit`].
///
/// The guarded variants (`FprLoad` / `FprFixnumLoad`) carry the deopt as a
/// [`DeoptPoint`] program point, **not** a frozen `AsmDeopt` index — so the
/// stream is now fully codegen-independent (the emit half materializes the
/// side-exit). `TransferIR` is therefore `Clone` but not `Copy` (a `DeoptPoint`
/// owns a `WriteBack`).
///
#[derive(Debug, Clone, PartialEq)]
pub(in crate::codegen::jitgen) enum TransferIR {
    Spill(Spill),
    FpXfer(FpXfer),
    GpLoad(GpLoad),
    /// `load_fpr`: deopt-carrying unbox-to-float (always guarded).
    FprLoad {
        load: FprLoad,
        slot: SlotId,
        deopt: DeoptPoint,
    },
    /// `load_fpr_fixnum`: unbox Integer to float; `deopt` present only for the
    /// guarded `S`/`G` arms.
    FprFixnumLoad {
        load: FprFixnumLoad,
        slot: SlotId,
        deopt: Option<DeoptPoint>,
    },
    /// §19 (B): a float binary **operation** — `lhs op rhs -> dst`, all operands
    /// already placed in fpr. The first *operation* (vs value-movement) routed
    /// through the record stream, so the stream is no longer transfers-only — the
    /// step toward a single ordered, replayable codegen record (doc §18.3/§19).
    /// Pure data (no closure, no abstract-state read), so it is `Clone` and
    /// shadow-checkable like the transfer records.
    FloatBinOp {
        kind: BinOpK,
        lhs: FPReg,
        rhs: FPReg,
        dst: FPReg,
    },
    /// §19 (B): a float **comparison** — `lhs <cmp> rhs`, result left in `rax`
    /// (the following `def_rax2acc` is a separate transfer). Pure data, like
    /// `FloatBinOp`.
    FloatCmp {
        kind: CmpKind,
        lhs: FPReg,
        rhs: FPReg,
    },
}

impl TransferIR {
    pub(in crate::codegen::jitgen) fn emit(self, ir: &mut AsmIr) {
        match self {
            TransferIR::Spill(s) => s.emit(ir),
            TransferIR::FpXfer(f) => f.emit(ir),
            TransferIR::GpLoad(g) => g.emit(ir),
            TransferIR::FprLoad { load, slot, deopt } => load.emit(ir, slot, Some(&deopt)),
            TransferIR::FprFixnumLoad { load, slot, deopt } => load.emit(ir, slot, deopt.as_ref()),
            TransferIR::FloatBinOp {
                kind,
                lhs,
                rhs,
                dst,
            } => ir.fpr_binop(kind, lhs, rhs, dst),
            TransferIR::FloatCmp { kind, lhs, rhs } => {
                ir.push(AsmInst::FloatCmp { kind, lhs, rhs })
            }
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
        let g = self.load_state(slot, dst);
        ir.transfer(TransferIR::GpLoad(g));
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
                // F -> Sf
                self.set_Sf_float(slot, fpr);
                GpLoad::FprBox(fpr, slot, dst)
            }
            LinkMode::C(v) => GpLoad::Lit(v, dst),
            LinkMode::Sf(_, _) | LinkMode::S(_) => {
                // A live GP resident holds the slot's (possibly dirty) value in a
                // register; move it from there rather than reading a stale home.
                if let Some(src) = self.gp_regfile.reg_of(slot) {
                    GpLoad::FromGp(src, dst)
                } else {
                    GpLoad::Stack(slot, dst)
                }
            }
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
    /// load *slot* as f64 into fpr register.
    ///
    /// ### destroy
    /// - rdi
    ///
    pub(crate) fn load_fpr_fixnum(&mut self, ir: &mut AsmIr, slot: SlotId) -> FPReg {
        // Only the `S`/`G` arms guard, so a deopt point is recorded only for
        // them — and *before* the state transition, so its write-back snapshot
        // is the pre-load placement (cf. `load_fpr`). `use_as_value` (below, in
        // the state half) only touches liveness, so this peek is stable; and the
        // later `stack2reg`/`reg2stack` are pure emits that don't perturb the
        // snapshot. The guard-free `Sf`/`F`/`C` arms record no deopt, exactly as
        // before. The side-exit itself is materialized in the emit half.
        let deopt =
            matches!(self.mode(slot), LinkMode::S(_)).then(|| self.deopt_point());
        let (x, load) = self.load_fpr_fixnum_state(slot);
        ir.transfer(TransferIR::FprFixnumLoad { load, slot, deopt });
        x
    }

    ///
    /// Analysis half of [`Self::load_fpr_fixnum`] (item ②, step 2): refine the
    /// slot to `Integer`, allocate the fpr and bind the slot, returning the
    /// register plus the conversion (and whether a runtime guard is needed) to
    /// emit. Pure state.
    ///
    fn load_fpr_fixnum_state(&mut self, slot: SlotId) -> (FPReg, FprFixnumLoad) {
        self.use_as_value(slot);
        match self.mode(slot) {
            LinkMode::Sf(x, _) | LinkMode::F(x) => (x, FprFixnumLoad::None),
            LinkMode::S(_) => {
                // S -> Sf. Refine the type first (its guard verdict) then take
                // the placement; `set_new_Sf` overwrites the refined `S` guarded
                // with `Sf(Fixnum)`, exactly as `guard_fixnum` + `set_new_Sf` did.
                // Capture any live GP resident *before* `set_new_Sf` clears it, so
                // the emit reads the value from the register instead of its
                // possibly-stale stack home (the conversion copies to Rdi first, so
                // the resident register survives intact).
                let gp = self.gp_regfile.reg_of(slot);
                let guard = self.guard_class_state(slot, INTEGER_CLASS);
                let x = self.set_new_Sf(slot, SfGuarded::Fixnum);
                (x, FprFixnumLoad::FromStack(x, guard, gp))
            }
            LinkMode::C(v) => {
                let (x, load) = self.load_fpr_from_C_state(slot, v);
                (x, FprFixnumLoad::Numeric(load))
            }
            LinkMode::V | LinkMode::MaybeNone | LinkMode::None => {
                unreachable!("load_fpr_fixnum() {:?}", self.mode(slot));
            }
        }
    }

    ///
    /// load *reg* as f64 into fpr register.
    ///
    /// ### destroy
    /// - rdi, rax
    ///
    ///
    pub(crate) fn load_fpr(&mut self, ir: &mut AsmIr, slot: SlotId) -> FPReg {
        // The deopt point is recorded *before* the state transition, so its
        // write-back snapshot (`get_write_back`) is the pre-load placement — see
        // doc/regalloc_separation.md §9. The record carries the program point
        // (pc + write-back), not a frozen `AsmDeopt`; the emit half materializes
        // the side-exit from it.
        let deopt = self.deopt_point();
        let (x, load) = self.load_fpr_state(slot);
        ir.transfer(TransferIR::FprLoad { load, slot, deopt });
        x
    }

    ///
    /// Capture the deopt **program point** at the current placement state (item
    /// ②, step 2 — deopt program-point-ification, doc §9): the bytecode `pc`
    /// plus the write-back snapshot `get_write_back()` — *which* values are
    /// unboxed/in-acc and must be restored to the stack if a guard fails. This is
    /// pure analysis state; the codegen half turns it into an `AsmDeopt`
    /// side-exit via [`AsmIr::deopt_from_point`]. Mirrors `AsmIr::new_deopt`'s
    /// snapshot, minus the `side_exit` push.
    ///
    pub(in crate::codegen::jitgen) fn deopt_point(&self) -> DeoptPoint {
        DeoptPoint::new(self.pc(), self.get_write_back())
    }

    ///
    /// Analysis half of [`Self::load_fpr`] (item ②, step 2): perform the
    /// abstract-state transition (allocate the fpr, bind the slot) and return
    /// the allocated register plus the conversion to emit. Pure state.
    ///
    fn load_fpr_state(&mut self, slot: SlotId) -> (FPReg, FprLoad) {
        self.use_as_float(slot);
        match self.mode(slot) {
            LinkMode::Sf(x, _) | LinkMode::F(x) => (x, FprLoad::None),
            LinkMode::S(_) => {
                // -> Sf. Capture any live GP resident before `set_new_Sf` clears
                // it so the emit reads from the register rather than the stack
                // home (see `load_fpr_fixnum_state`).
                let gp = self.gp_regfile.reg_of(slot);
                let x = self.set_new_Sf(slot, SfGuarded::Float);
                (x, FprLoad::FromStack(x, gp))
            }
            LinkMode::C(v) => self.load_fpr_from_C_state(slot, v),
            LinkMode::V | LinkMode::MaybeNone | LinkMode::None => {
                unreachable!("load_fpr() {:?}", self.mode(slot));
            }
        }
    }

    #[allow(non_snake_case)]
    fn load_fpr_from_C_state(&mut self, slot: SlotId, v: Value) -> (FPReg, FprLoad) {
        // `LinkMode::C` may hold any Value; fpr loads only ever come from
        // numeric literals (fixnum / float / heap Float), so anything else
        // is a bug at the call site.
        match v.unpack() {
            RV::Float(f) => {
                // -> F
                let x = self.set_new_F(slot);
                (x, FprLoad::FromF64(f, x))
            }
            RV::Fixnum(i) => {
                // -> Sf
                let x = self.set_new_Sf(slot, SfGuarded::Fixnum);
                (x, FprLoad::FromFixnum(i, x))
            }
            _ => {
                unreachable!("load_fpr_from_C() {:?}", v);
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
        self.load(ir, slot, GP::Rax);
        ir.reg2rsp_offset(GP::Rax, ofs);
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
