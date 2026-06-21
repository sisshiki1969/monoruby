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
    /// `stack2reg(slot, Rdi); float_to_fpr(Rdi, x, deopt)`
    FromStack(FPReg),
    /// `G` slot in a GP register: `reg2stack(src, slot); float_to_fpr(src, x,
    /// deopt)`. `src` is `r15` for the accumulator or a pool register once placed.
    FromReg(GP, FPReg),
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
            FprLoad::FromStack(x) => {
                let deopt = ir.deopt_from_point(deopt.unwrap());
                ir.stack2reg(slot, GP::Rdi);
                ir.float_to_fpr(GP::Rdi, x, deopt);
            }
            FprLoad::FromReg(src, x) => {
                let deopt = ir.deopt_from_point(deopt.unwrap());
                ir.reg2stack(src, slot);
                ir.float_to_fpr(src, x, deopt);
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
    /// `G` slot resident in a GP register: `reg_move(src, dst)`. `src` is the
    /// slot's register (`r15` for the accumulator, a pool register `r8`–`r11`
    /// once 9d places it).
    Reg(GP, GP),
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
            GpLoad::Reg(src, dst) => ir.reg_move(src, dst),
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
    /// `stack2reg(slot, Rdi); [GuardClass(Rdi)]; fixnum2fpr(Rdi, x)`
    FromStack(FPReg, bool),
    /// `G` slot in a GP register: `reg2stack(src, slot); [GuardClass(src)];
    /// fixnum2fpr(src, x)`. `src` is `r15` or a pool register once placed.
    FromReg(GP, FPReg, bool),
    /// guard-free numeric literal (`LinkMode::C`) — reuses the [`FprLoad`] record
    Numeric(FprLoad),
}

impl FprFixnumLoad {
    /// `deopt` (the program point) is `Some` for the guarded `S`/`G` arms and
    /// `None` for the guard-free ones. The side-exit is materialized for the
    /// `S`/`G` arms whenever the point is `Some` — even if `guard` is false, so
    /// the `side_exit` table stays identical to the pre-split wrapper, which
    /// created the deopt unconditionally for those arms.
    fn emit(self, ir: &mut AsmIr, slot: SlotId, deopt: Option<&DeoptPoint>) {
        match self {
            FprFixnumLoad::None => {}
            FprFixnumLoad::FromStack(x, guard) => {
                let deopt = deopt.map(|p| ir.deopt_from_point(p));
                ir.stack2reg(slot, GP::Rdi);
                if guard {
                    ir.push(AsmInst::GuardClass(GP::Rdi, INTEGER_CLASS, deopt.unwrap()));
                }
                ir.fixnum2fpr(GP::Rdi, x);
            }
            FprFixnumLoad::FromReg(src, x, guard) => {
                let deopt = deopt.map(|p| ir.deopt_from_point(p));
                ir.reg2stack(src, slot);
                if guard {
                    ir.push(AsmInst::GuardClass(src, INTEGER_CLASS, deopt.unwrap()));
                }
                ir.fixnum2fpr(src, x);
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
    /// §19 (B): an integer **comparison** (`fetch_fixnum_*_nodeopt` already
    /// emitted the fixnum guards), result in `rax`. Deopt-free, pure data.
    IntegerCmp {
        mode: OpMode,
        kind: CmpKind,
        lhs: GP,
        rhs: GP,
    },
    /// §19 (B): an integer binary **operation** with an overflow/`bignum` guard.
    /// Like the guarded loads (`FprLoad`), it carries its deopt as a
    /// [`DeoptPoint`] program point (not a frozen `AsmDeopt`); the emit half
    /// materializes the side-exit via `deopt_from_point`, keeping the record
    /// codegen-independent.
    IntegerBinOp {
        kind: BinOpK,
        lhs: GP,
        rhs: GP,
        mode: OpMode,
        deopt: DeoptPoint,
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
            TransferIR::IntegerCmp {
                mode,
                kind,
                lhs,
                rhs,
            } => ir.integer_cmp(mode, kind, lhs, rhs),
            TransferIR::IntegerBinOp {
                kind,
                lhs,
                rhs,
                mode,
                deopt,
            } => {
                let deopt = ir.deopt_from_point(&deopt);
                ir.integer_binop(kind, lhs, rhs, mode, deopt);
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
            LinkMode::G(_, vreg) => GpLoad::Reg(vreg.phys(), dst),
            LinkMode::MaybeNone => GpLoad::Stack(slot, dst),
            LinkMode::V | LinkMode::None => {
                unreachable!("load() {:?} {:?}: {:?}", slot, self.mode(slot), self);
            }
        }
    }

    /// Bring `lhs` into the register where an in-place fixnum binop computes its
    /// result, and return that register. No `Rdi` copy, no relocate `mov`.
    ///
    /// Register choice, in priority order:
    /// 1. **`lhs` is already in a GP register** (R15 or a pool register) →
    ///    compute in place there, reusing it for the result (zero operand
    ///    copies — the chained-arithmetic / loop-carried fast path). `lhs` is
    ///    spilled to its stack home first (`write_back_slot`), which preserves
    ///    it for any later use *and* keeps the overflow-deopt snapshot
    ///    consistent (the in-place op clobbers the register, but the snapshot
    ///    reads the spilled home).
    /// 2. **gp-alloc, a vacant pool register** → load `lhs` into it and compute
    ///    there, so the result stays resident in `r8`–`r11` and later reads
    ///    avoid a stack reload (this is what makes the pool pay off).
    /// 3. otherwise → the R15 accumulator (freed first).
    pub(in crate::codegen::jitgen) fn fetch_lhs_to_inplace_reg(
        &mut self,
        ir: &mut AsmIr,
        lhs: SlotId,
    ) -> GP {
        if let Some(reg) = self.on_reg(lhs) {
            self.write_back_slot(ir, lhs);
            self.guard_fixnum(ir, lhs, reg);
            return reg;
        }
        #[cfg(feature = "gp-alloc")]
        if let Some(id) = self.try_vacant_pool_id() {
            let reg = crate::codegen::GP_ALLOC_POOL[id];
            self.load(ir, lhs, reg);
            self.guard_fixnum(ir, lhs, reg);
            return reg;
        }
        self.free_acc(ir);
        self.load(ir, lhs, GP::R15);
        self.guard_fixnum(ir, lhs, GP::R15);
        GP::R15
    }

    /// Choose the in-place result register for a fixnum binop whose lhs is an
    /// immediate (the `IR`-Sub case, where the encoder materializes the
    /// immediate into the result register): a vacant pool register under
    /// gp-alloc, else the freed R15 accumulator.
    pub(in crate::codegen::jitgen) fn alloc_inplace_reg(&mut self, ir: &mut AsmIr) -> GP {
        #[cfg(feature = "gp-alloc")]
        if let Some(id) = self.try_vacant_pool_id() {
            return crate::codegen::GP_ALLOC_POOL[id];
        }
        self.free_acc(ir);
        GP::R15
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
            matches!(self.mode(slot), LinkMode::S(_) | LinkMode::G(_, _)).then(|| self.deopt_point());
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
                let guard = self.guard_class_state(slot, INTEGER_CLASS);
                let x = self.set_new_Sf(slot, SfGuarded::Fixnum);
                (x, FprFixnumLoad::FromStack(x, guard))
            }
            LinkMode::G(_, vreg) => {
                // G -> Sf
                let src = vreg.phys();
                let guard = self.guard_class_state(slot, INTEGER_CLASS);
                let x = self.set_new_Sf(slot, SfGuarded::Fixnum);
                (x, FprFixnumLoad::FromReg(src, x, guard))
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
                // -> Sf
                let x = self.set_new_Sf(slot, SfGuarded::Float);
                (x, FprLoad::FromStack(x))
            }
            LinkMode::G(_, vreg) => {
                // -> Sf
                let src = vreg.phys();
                let x = self.set_new_Sf(slot, SfGuarded::Float);
                (x, FprLoad::FromReg(src, x))
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
        match self.mode(slot) {
            LinkMode::G(_, vreg) => {
                self.use_as_value(slot);
                ir.reg2rsp_offset(vreg.phys(), ofs);
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
