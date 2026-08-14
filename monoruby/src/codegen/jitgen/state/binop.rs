//!
//! The numeric binop / comparison **emission primitives** — the
//! register-allocated fixnum paths (`binop_integer*`, `gen_cmp*_integer*`),
//! the xmm float paths (`binop_float`, `gen_cmp*_float`), their constant
//! folds, and the operand-load discipline (`gp_ensure`,
//! `load_binary_(ret_)fpr`). Moved here from `compile/binary_op.rs` so the
//! per-method inline generators in `builtins/numeric/` can drive them while
//! the register-allocator internals stay private to `state`.
//!

use num::Zero;

use crate::bytecodegen::BinOpK;

use super::*;

///
/// §18 handler separation: the allocation-free **decision** a float binary op
/// reduces to (the Layer-① result of `plan_binop_float`). `binop_float` then
/// executes it (Layer-② allocation + emission). Holding the decision as a value —
/// rather than branching straight into `def_C_float` / `load_binary_ret_fpr` —
/// is what makes the type/representation choice separable from the placement.
///
enum FloatBinOpPlan {
    /// Both operands are constant floats and the folded result is a flonum
    /// immediate: a pure constant, no fpr. Carries the folded `f64`.
    Fold(f64),
    /// The fpr path: load operands into fpr, allocate the destination, emit.
    FprOp,
}

fn cmp<T>(kind: CmpKind, lhs: T, rhs: T) -> bool
where
    T: PartialEq + PartialOrd,
{
    match kind {
        CmpKind::Eq | CmpKind::TEq => lhs == rhs,
        CmpKind::Ne => lhs != rhs,
        CmpKind::Lt => lhs < rhs,
        CmpKind::Le => lhs <= rhs,
        CmpKind::Gt => lhs > rhs,
        CmpKind::Ge => lhs >= rhs,
    }
}

impl AbstractFrame {
    fn fold_constant_cmp<T>(&mut self, kind: CmpKind, lhs: T, rhs: T, dst: Option<SlotId>)
    where
        T: PartialEq + PartialOrd,
    {
        let b = cmp(kind, lhs, rhs);
        self.def_C(dst, Immediate::bool(b));
    }

    pub(crate) fn check_concrete_i64(&self, lhs: SlotId, rhs: SlotId) -> Option<(i64, i64)> {
        let lhs = self.is_fixnum_literal(lhs)?.get();
        let rhs = self.is_fixnum_literal(rhs)?.get();
        Some((lhs, rhs))
    }

    #[allow(non_snake_case)]
    pub(crate) fn check_binary_C_f64(&self, lhs: SlotId, rhs: SlotId) -> Option<(f64, f64)> {
        let lhs = self.coerce_C_f64(lhs)?;
        let rhs = self.coerce_C_f64(rhs)?;
        Some((lhs, rhs))
    }



    fn binop_integer_folded(&mut self, kind: BinOpK, lhs: i64, rhs: i64) -> Option<Immediate> {
        match kind {
            BinOpK::Add => {
                if let Some(result) = lhs.checked_add(rhs) {
                    return Immediate::check_fixnum(result);
                }
            }
            BinOpK::Sub => {
                if let Some(result) = lhs.checked_sub(rhs) {
                    return Immediate::check_fixnum(result);
                }
            }
            BinOpK::Mul => {
                if let Some(result) = lhs.checked_mul(rhs) {
                    return Immediate::check_fixnum(result);
                }
            }
            BinOpK::Div => {
                if rhs.is_zero() {
                    return None;
                }
                return Immediate::check_fixnum(lhs.ruby_div(&rhs));
            }
            // Bitwise ops on two i63 fixnums always yield an i63 fixnum.
            BinOpK::BitOr => return Immediate::check_fixnum(lhs | rhs),
            BinOpK::BitAnd => return Immediate::check_fixnum(lhs & rhs),
            BinOpK::BitXor => return Immediate::check_fixnum(lhs ^ rhs),
            BinOpK::Rem | BinOpK::Exp | BinOpK::Shl | BinOpK::Shr => unreachable!(),
        }
        None
    }

    ///
    /// Integer binary operations
    ///
    /// ### in
    /// - rdi: lhs
    /// - rsi: rhs
    ///
    /// ### out
    /// - r15: dst
    ///
    pub(crate) fn binop_integer(
        &mut self,
        ir: &mut AsmIr,
        kind: BinOpK,
        dst: Option<SlotId>,
        lhs: SlotId,
        rhs: SlotId,
    ) {
        if let Some((lhs, rhs)) = self.check_concrete_i64(lhs, rhs)
            && let Some(result) = self.binop_integer_folded(kind, lhs, rhs)
        {
            // The fold redefines `dst` as a constant; drop any stale GP cache of
            // it so a later op does not reuse the register's old value. Other
            // residents stay live (the fold emits nothing and reads no slot).
            if let Some(dst) = dst {
                self.gp_regfile.invalidate(dst);
            }
            self.def_C(dst, result);
            return;
        };

        // All four fixnum ops keep their operands and result in GP registers
        // (the local allocator), reusing residents across consecutive ops.
        self.binop_integer_gp(ir, kind, dst, lhs, rhs);
    }

    /// The register-allocated fixnum binop. Operands are brought into GP
    /// registers (reusing a resident copy, else materialized to the stack home
    /// and loaded + fixnum-guarded), the result is allocated a register (evicting
    /// the oldest resident if full), and the op runs in registers (overflow ->
    /// deopt). The result stays resident — its stack home is written only when
    /// the file is flushed (before a non-binop or at the block boundary) or
    /// re-homed by a deopt write-back.
    ///
    /// `Mul` and `Div` destroy the `rhs` register (Mul untags it in place; Div's
    /// `idiv` sequence sarq's it), and `Div` produces its quotient in `rax`. So
    /// for those two the `rhs` resident is written back to its home (if dirty and
    /// still live) before the op and dropped from the file afterwards, and `Div`
    /// pins both operands so the result lands in a distinct register.
    fn binop_integer_gp(
        &mut self,
        ir: &mut AsmIr,
        kind: BinOpK,
        dst: Option<SlotId>,
        lhs: SlotId,
        rhs: SlotId,
    ) {
        // Immediate form: `Add`/`Sub` with a compile-time fixnum constant on
        // one side (either side for the commutative `Add`) folds the constant
        // into the instruction's immediate operand — no register
        // materialization, no untag adjustment (`(2a+1) ± 2k = 2(a±k)+1`),
        // overflow detection preserved. Constant-constant was already folded
        // by `binop_integer`.
        if matches!(kind, BinOpK::Add | BinOpK::Sub) {
            let var_imm = if let Some(k) = self.is_fixnum_literal(rhs) {
                Some((lhs, k.get()))
            } else if kind == BinOpK::Add
                && let Some(k) = self.is_fixnum_literal(lhs)
            {
                Some((rhs, k.get()))
            } else {
                None
            };
            if let Some((var, k)) = var_imm
                && let Some(imm) = k.checked_mul(2).and_then(|v| i32::try_from(v).ok())
            {
                self.binop_integer_imm(ir, kind, dst, var, imm);
                return;
            }
        }
        // `Mul` and `Div` destroy the `rhs` register before their overflow /
        // divide-by-zero side-exit (Mul `sarq`s it; Div's idiv sequence too).
        let rhs_clobbered = matches!(kind, BinOpK::Mul | BinOpK::Div);
        // 1. Load the operands into registers (reusing a resident copy).
        let (lhs_gp, lhs_guard) = self.gp_ensure(ir, lhs, &[]);
        let (rhs_gp, rhs_guard) = self.gp_ensure(ir, rhs, &[lhs_gp]);
        // Same slot on both sides (`x + x`): one guard proves both operands.
        let rhs_guard = rhs_guard && lhs != rhs;
        // 1b. For `Mul`/`Div`: if `rhs` is a dirty resident, write it to its home
        //     and mark it clean *before* the deopt snapshot. The op clobbers
        //     `rhs_gp` before the side-exit, so the snapshot must re-home `rhs`
        //     from its (now-current) stack home, not from the dead register — a
        //     `dirty_residents()` entry would otherwise store garbage to the slot
        //     on deopt (e.g. the untagged divisor, an invalid `Value`). A fresh /
        //     constant operand is already recoverable (its home is current, or it
        //     re-materializes from `LinkMode::C`).
        if rhs_clobbered && let Some(reg) = self.gp_regfile.dirty_reg_of(rhs) {
            ir.reg2stack(reg, rhs);
            self.gp_regfile.sync(rhs);
        }
        // Whether `lhs`'s snapshot recovery depends on `lhs_gp` surviving the
        // op: a dirty resident is re-homed *from the register* by the deopt
        // write-back, so an in-place Add/Sub (which clobbers the register
        // before the overflow side-exit) must not be chosen for it — see
        // `binop_dst_reg`. Captured before the snapshot.
        let lhs_dirty = self.gp_regfile.dirty_reg_of(lhs).is_some();
        // 2. Snapshot the deopt write-back *before* clearing: it must re-home the
        //    dirty residents that are live at this op's PC — which includes a
        //    dirty operand that is itself a dead-after temporary (a prior binop
        //    result consumed here). The guards and the overflow check below all
        //    side-exit to this point, where the interpreter re-reads the operands
        //    from their stack homes, so they have to be recoverable.
        let deopt = ir.new_deopt(self);
        // 3. `dst` is about to be redefined: drop any stale GP cache of it (done
        //    after the snapshot, so a `dst` that aliases a dirty operand is still
        //    re-homed for the re-execution).
        if let Some(dst) = dst {
            self.gp_regfile.invalidate(dst);
        }
        // 4. Clear `next_sp`: every temporary the stack pointer has popped is now
        //    dead. `next_sp` is the sp *after* this op, so the operands — already
        //    read into registers in step 1 — are freed here too. This must run
        //    after the load (which still reads them) and before the result
        //    allocation (so the result can reuse a freed operand's register).
        let next_sp = self.next_sp();
        self.gp_regfile.free_above_sp(next_sp);
        // 5. Choose the result register and run the op. `Add`/`Sub` compute in
        //    the dst position; `binop_dst_reg` prefers `lhs_gp` itself (in
        //    place, or a binding transfer from a clean live resident — no
        //    move), falling back to a distinct register when `lhs` is dirty
        //    (its register must survive to the side exit for the deopt
        //    write-back). `Mul`/`Div` clobber `rhs` and (Div) produce in
        //    `rax`, so pin both operands and take a distinct register.
        //    `x + x` (both operands in one register) uses the tagged-order
        //    doubling sequence, which reads the shared operand before any
        //    untag — so it may compute in place in the shared register too.
        let double = kind == BinOpK::Add && lhs_gp == rhs_gp;
        let dst_gp = if rhs_clobbered {
            let (gp, spill) = self.gp_regfile.alloc_reg(&[lhs_gp, rhs_gp]);
            if let Some((reg, slot)) = spill {
                ir.reg2stack(reg, slot);
            }
            gp
        } else if double {
            self.binop_dst_reg(ir, lhs, lhs_gp, lhs_dirty, &[])
        } else {
            self.binop_dst_reg(ir, lhs, lhs_gp, lhs_dirty, &[rhs_gp])
        };
        // An operand not yet proven a fixnum is only speculatively an integer,
        // so guard it; the guard then *proves* it a fixnum, so refine its
        // abstract type in place (keeping the resident) — a later integer op on
        // the same slot consumes it guard-free via `is_fixnum`, resident or not.
        if lhs_guard {
            ir.push(AsmInst::GuardClass(lhs_gp, INTEGER_CLASS, deopt));
            self.refine_S_fixnum(lhs);
        }
        if rhs_guard {
            ir.push(AsmInst::GuardClass(rhs_gp, INTEGER_CLASS, deopt));
            self.refine_S_fixnum(rhs);
        }
        if double {
            ir.push(AsmInst::IntegerDouble {
                dst: dst_gp,
                lhs: lhs_gp,
                deopt,
            });
        } else {
            ir.integer_binop_reg(kind, dst_gp, lhs_gp, rhs_gp, deopt);
        }
        // The op left garbage in `rhs_gp`: forget that it cached `rhs`.
        if rhs_clobbered {
            self.gp_regfile.invalidate(rhs);
        }
        if let Some(dst) = dst {
            // Define first (this clears any stale resident of `dst` via `clear`),
            // then bind the result register.
            self.def_S_guarded(dst, Guarded::Fixnum);
            self.gp_regfile.bind(dst_gp, dst, /* dirty */ true);
        }
    }

    /// Immediate-form fixnum `Add`/`Sub`: `dst = lhs <kind> k` with the
    /// constant folded into the instruction (see `AsmInst::IntegerBinOpImm`).
    /// `imm` is the doubled untagged constant `2k` (i32-gated by the caller).
    fn binop_integer_imm(
        &mut self,
        ir: &mut AsmIr,
        kind: BinOpK,
        dst: Option<SlotId>,
        lhs: SlotId,
        imm: i32,
    ) {
        let (lhs_gp, lhs_guard) = self.gp_ensure(ir, lhs, &[]);
        let lhs_dirty = self.gp_regfile.dirty_reg_of(lhs).is_some();
        let deopt = ir.new_deopt(self);
        if let Some(dst) = dst {
            self.gp_regfile.invalidate(dst);
        }
        let next_sp = self.next_sp();
        self.gp_regfile.free_above_sp(next_sp);
        let dst_gp = self.binop_dst_reg(ir, lhs, lhs_gp, lhs_dirty, &[]);
        if lhs_guard {
            ir.push(AsmInst::GuardClass(lhs_gp, INTEGER_CLASS, deopt));
            self.refine_S_fixnum(lhs);
        }
        ir.push(AsmInst::IntegerBinOpImm {
            kind,
            dst: dst_gp,
            lhs: lhs_gp,
            imm,
            deopt,
        });
        if let Some(dst) = dst {
            self.def_S_guarded(dst, Guarded::Fixnum);
            self.gp_regfile.bind(dst_gp, dst, /* dirty */ true);
        }
    }

    /// Choose the result register for an `Add`/`Sub`-family op that computes
    /// in place in the dst position.
    ///
    /// * `lhs` **clean** (or already unbound — a dead-after temp freed by
    ///   `free_above_sp`, a dst-aliasing operand dropped by `invalidate`, or
    ///   a constant's load register): compute in place in `lhs_gp`. A live
    ///   clean resident transfers its register to the result with no data
    ///   move (its stack home is current, so nothing is lost); the slot
    ///   simply drops to `S`.
    /// * `lhs` **dirty**: the deopt write-back re-homes `lhs` *from
    ///   `lhs_gp`*, and the op would clobber that register before the
    ///   overflow side-exit — the interpreter would then re-execute the op
    ///   with a corrupted operand. Compute in a distinct register instead
    ///   (the lowering's `mov dst, lhs` preserves the operand register for
    ///   the side exit).
    fn binop_dst_reg(
        &mut self,
        ir: &mut AsmIr,
        lhs: SlotId,
        lhs_gp: GP,
        lhs_dirty: bool,
        extra_pinned: &[GP],
    ) -> GP {
        // Never compute in place in a register that also carries another live
        // operand (`x + x`: lhs and rhs share one register — clobbering it
        // in place would corrupt the rhs read).
        if !lhs_dirty && !extra_pinned.contains(&lhs_gp) {
            if self.gp_regfile.is_free(lhs_gp) {
                return lhs_gp;
            }
            if self.gp_regfile.reg_of(lhs) == Some(lhs_gp) {
                self.gp_regfile.invalidate(lhs);
                return lhs_gp;
            }
        }
        let mut pinned = vec![lhs_gp];
        pinned.extend_from_slice(extra_pinned);
        let (gp, spill) = self.gp_regfile.alloc_reg(&pinned);
        if let Some((reg, slot)) = spill {
            ir.reg2stack(reg, slot);
        }
        gp
    }

    /// Bring `slot` into a GP register, reusing its resident copy when present
    /// (no reload) and otherwise materializing it to its stack home and loading
    /// it. The returned bool asks the caller to emit a fixnum guard; it is
    /// decided by the slot's abstract type (`is_fixnum`), never by residency —
    /// a reloaded slot that an earlier guard/def already proved a fixnum (e.g.
    /// an evicted binop result) is consumed guard-free, exactly like a resident
    /// one.
    fn gp_ensure(&mut self, ir: &mut AsmIr, slot: SlotId, pinned: &[GP]) -> (GP, bool) {
        if let Some(gp) = self.gp_regfile.reg_of(slot) {
            // Reuse the resident copy. Whether it needs a fixnum class guard is
            // the slot's abstract type, not a per-resident flag: a resident
            // produced by an integer op is `Guarded::Fixnum` (no guard), while one
            // produced by a `call`/`yield` result or a frozen literal is a general
            // `Value` / heap class (`is_fixnum == false`) and must be guarded
            // before integer use.
            return (gp, !self.is_fixnum(slot));
        }
        // Compile-time fixnum constant: load the tagged immediate straight into a
        // register, skipping the stack-home round-trip (`%1 = %2 + 1` loads `1`
        // as `movabs gp, 0x3` rather than materializing it to a slot and reading
        // it back). The value is a known fixnum, so it needs no guard.
        if let Some(v) = self.fixnum_literal_value(slot) {
            let (gp, spill) = self.gp_regfile.alloc_reg(pinned);
            if let Some((reg, s)) = spill {
                ir.reg2stack(reg, s);
            }
            ir.lit2reg(v, gp);
            self.gp_regfile.bind(gp, slot, /* dirty */ false);
            return (gp, false);
        }
        // Not resident and not a constant: put the value at its canonical stack
        // home (a no-op for an `S` slot; materializes a boxed float), then load it.
        self.write_back_slot(ir, slot);
        let (gp, spill) = self.gp_regfile.alloc_reg(pinned);
        if let Some((reg, s)) = spill {
            ir.reg2stack(reg, s);
        }
        ir.stack2reg(slot, gp);
        self.gp_regfile.bind(gp, slot, /* dirty */ false);
        // Guard iff the slot is not already a proven fixnum — the same criterion
        // as the resident path above. An `S(Fixnum)` slot's stack home always
        // holds the proven-fixnum value, so reloading it (after an eviction or
        // a flush) needs no re-guard.
        (gp, !self.is_fixnum(slot))
    }

    /// prepare the GP register file for a float op that keeps
    /// the residents alive (it computes in xmm without clobbering them). The op
    /// reads its operands from their stack homes, so a GP-resident operand — the
    /// integer side of a mixed `Integer <op> Float` — is written back to its home
    /// and marked clean (it stays cached for a following integer op). Finally any
    /// stale GP cache of the result slot is dropped.
    fn binop_float_folded(&self, kind: BinOpK, lhs: f64, rhs: f64) -> Option<f64> {
        Some(match kind {
            BinOpK::Add => lhs + rhs,
            BinOpK::Sub => lhs - rhs,
            BinOpK::Mul => lhs * rhs,
            BinOpK::Div => lhs.ruby_div(&rhs),
            _ => return None,
        })
    }

    ///
    /// §18 handler separation: the **decision** half of `binop_float`, a pure
    /// (`&self`, allocation-free) function of the operand state — the Layer-①
    /// part. It chooses between a constant fold (both operands constant floats and
    /// the result is a flonum immediate) and the fpr path, *without* allocating an
    /// fpr or emitting. `binop_float` then *executes* the chosen plan (the Layer-②
    /// allocation + emission). Separating the two is the template for un-welding
    /// each float-op handler's type/representation decision from its
    /// allocation+emission.
    ///
    fn plan_binop_float(&self, kind: BinOpK, info: FBinOpInfo) -> FloatBinOpPlan {
        if let Some((lhs, rhs)) = self.check_binary_C_f64(info.lhs, info.rhs)
            && let Some(result) = self.binop_float_folded(kind, lhs, rhs)
            && Immediate::flonum(result).is_some()
        {
            FloatBinOpPlan::Fold(result)
        } else {
            FloatBinOpPlan::FprOp
        }
    }

    pub(crate) fn binop_float(&mut self, ir: &mut AsmIr, kind: BinOpK, dst: Option<SlotId>, info: FBinOpInfo) {
        match self.plan_binop_float(kind, info) {
            FloatBinOpPlan::Fold(result) => {
                // `plan_binop_float` already verified `Immediate::flonum`, so this
                // always succeeds — a pure Layer-① constant, no fpr.
                let folded = self.def_C_float(dst, result);
                debug_assert!(folded);
            }
            FloatBinOpPlan::FprOp => {
                let (lhs, rhs, dst) = self.load_binary_ret_fpr(ir, dst, info);
                if let Some(dst) = dst {
                    ir.fpr_binop(kind, lhs, rhs, dst);
                }
            }
        }
    }

    pub(crate) fn gen_cmp_integer(
        &mut self,
        ir: &mut AsmIr,
        kind: CmpKind,
        dst: Option<SlotId>,
        lhs: SlotId,
        rhs: SlotId,
    ) {
        if let Some((lhs, rhs)) = self.check_concrete_i64(lhs, rhs) {
            // The fold redefines `dst`; drop any stale GP cache of it.
            if let Some(dst) = dst {
                self.gp_regfile.invalidate(dst);
            }
            self.fold_constant_cmp(kind, lhs, rhs, dst);
            return;
        };
        self.gen_cmp_integer_gp(ir, kind, dst, lhs, rhs);
    }

    /// the register-allocated fixnum comparison. Operands are
    /// brought into GP registers (reusing residents from a prior binop), guarded,
    /// and compared in registers; the boolean result is stored to `dst`'s stack
    /// home (a bool is never a GP resident, so the file shrinks by the operands
    /// the stack pointer has popped and gains nothing).
    fn gen_cmp_integer_gp(
        &mut self,
        ir: &mut AsmIr,
        kind: CmpKind,
        dst: Option<SlotId>,
        lhs: SlotId,
        rhs: SlotId,
    ) {
        // Immediate form: a compile-time fixnum constant rhs is folded into
        // the compare as its tagged value `2k+1` (tagged fixnums compare in
        // the same order as their untagged values) — no register
        // materialization. Constant-constant was folded by the caller.
        if let Some(k) = self.is_fixnum_literal(rhs)
            && let Some(imm) = k
                .get()
                .checked_mul(2)
                .and_then(|v| v.checked_add(1))
                .and_then(|v| i32::try_from(v).ok())
        {
            let (lhs_gp, lhs_guard) = self.gp_ensure(ir, lhs, &[]);
            let deopt = ir.new_deopt(self);
            if let Some(dst) = dst {
                self.gp_regfile.invalidate(dst);
            }
            let next_sp = self.next_sp();
            self.gp_regfile.free_above_sp(next_sp);
            if lhs_guard {
                ir.push(AsmInst::GuardClass(lhs_gp, INTEGER_CLASS, deopt));
                self.refine_S_fixnum(lhs);
            }
            ir.push(AsmInst::IntegerCmpImm {
                kind,
                dst,
                lhs: lhs_gp,
                imm,
            });
            if let Some(dst) = dst {
                self.def_S(dst);
            }
            return;
        }
        let (lhs_gp, lhs_guard) = self.gp_ensure(ir, lhs, &[]);
        let (rhs_gp, rhs_guard) = self.gp_ensure(ir, rhs, &[lhs_gp]);
        // Snapshot the deopt write-back before clearing (the guards side-exit to
        // a point where the interpreter re-reads the operands from their homes).
        let deopt = ir.new_deopt(self);
        // The result `dst` is redefined as a bool: drop any stale GP cache of it.
        if let Some(dst) = dst {
            self.gp_regfile.invalidate(dst);
        }
        // Clear the popped operand temporaries so a following binop can reuse
        // their registers.
        let next_sp = self.next_sp();
        self.gp_regfile.free_above_sp(next_sp);
        // Guard-then-refine, as in `binop_integer_gp`: the guard proves the
        // operand a fixnum, so record that on the slot and later integer ops
        // consume it guard-free.
        if lhs_guard {
            ir.push(AsmInst::GuardClass(lhs_gp, INTEGER_CLASS, deopt));
            self.refine_S_fixnum(lhs);
        }
        if rhs_guard {
            ir.push(AsmInst::GuardClass(rhs_gp, INTEGER_CLASS, deopt));
            self.refine_S_fixnum(rhs);
        }
        ir.integer_cmp_reg(kind, dst, lhs_gp, rhs_gp);
        if let Some(dst) = dst {
            self.def_S(dst);
        }
    }

    pub(crate) fn gen_cmp_float(
        &mut self,
        ir: &mut AsmIr,
        dst: Option<SlotId>,
        info: FBinOpInfo,
        kind: CmpKind,
    ) {
        if let Some((lhs, rhs)) = self.check_binary_C_f64(info.lhs, info.rhs) {
            self.fold_constant_cmp(kind, lhs, rhs, dst);
            return;
        };
        let binary_fpr = self.load_binary_fpr(ir, info);
        ir.push(AsmInst::FloatCmp {
            kind,
            lhs: binary_fpr.0,
            rhs: binary_fpr.1,
        });
        self.def_rax2acc(ir, dst);
    }

    /// The register-allocated fixnum compare + branch. Like `gen_cmp_integer_gp`,
    /// the operands are brought into GP registers (reusing residents from a prior
    /// binop) and fixnum-guarded. Because this terminates the basic block, the
    /// register file is then **flushed** — every dirty resident spilled to its
    /// stack home — *before* the conditional branch, so both successor blocks
    /// (taken and fall-through) observe slots in their canonical homes and start
    /// with an empty file. The operands stay in their registers for the compare.
    pub(crate) fn gen_cmpbr_integer(
        &mut self,
        ir: &mut AsmIr,
        kind: CmpKind,
        lhs: SlotId,
        rhs: SlotId,
        brkind: BrKind,
        branch_dest: JitLabel,
    ) {
        // Immediate form, as in `gen_cmp_integer_gp`: fold a constant rhs
        // into the compare as its tagged value `2k+1`.
        if let Some(k) = self.is_fixnum_literal(rhs)
            && let Some(imm) = k
                .get()
                .checked_mul(2)
                .and_then(|v| v.checked_add(1))
                .and_then(|v| i32::try_from(v).ok())
        {
            let (lhs_gp, lhs_guard) = self.gp_ensure(ir, lhs, &[]);
            let deopt = ir.new_deopt(self);
            if lhs_guard {
                ir.push(AsmInst::GuardClass(lhs_gp, INTEGER_CLASS, deopt));
                self.refine_S_fixnum(lhs);
            }
            self.flush_gp(ir);
            ir.push(AsmInst::IntegerCmpBrImm {
                kind,
                brkind,
                branch_dest,
                lhs: lhs_gp,
                imm,
            });
            return;
        }
        let (lhs_gp, lhs_guard) = self.gp_ensure(ir, lhs, &[]);
        let (rhs_gp, rhs_guard) = self.gp_ensure(ir, rhs, &[lhs_gp]);
        // Snapshot the deopt write-back before the flush/branch (the guards
        // side-exit to a point where the interpreter re-reads the operands).
        let deopt = ir.new_deopt(self);
        // Guard-then-refine, as in `binop_integer_gp`. The refinement is
        // recorded on the slot (not the register file), so it survives the
        // block-terminator flush below and propagates to the successor blocks.
        if lhs_guard {
            ir.push(AsmInst::GuardClass(lhs_gp, INTEGER_CLASS, deopt));
            self.refine_S_fixnum(lhs);
        }
        if rhs_guard {
            ir.push(AsmInst::GuardClass(rhs_gp, INTEGER_CLASS, deopt));
            self.refine_S_fixnum(rhs);
        }
        // Block terminator: spill the dirty residents to their stack homes before
        // the branch (the operands' registers still hold their values for the
        // compare), leaving the file empty for both successors.
        self.flush_gp(ir);
        ir.integer_cmpbr_reg(kind, brkind, branch_dest, lhs_gp, rhs_gp);
    }

    pub(in crate::codegen::jitgen) fn load_binary_fpr(&mut self, ir: &mut AsmIr, info: FBinOpInfo) -> (FPReg, FPReg) {
        let FBinOpInfo {
            lhs,
            rhs,
            lhs_class,
            rhs_class,
            ..
        } = info;
        if lhs != rhs {
            // Loading lhs may set an `Sf` mode on its fpr. Without pinning,
            // the next allocator call (when loading rhs) can demote that
            // same fpr via Phase-1 of `try_alloc_fpr_demote` and hand it
            // back as the rhs fpr — so the consumer would compare /
            // arithmetic the value with itself. Pin lhs across the rhs
            // load to force the allocator to pick a different physical
            // register.
            let lhs_fpr = self.fetch_float_assume(ir, lhs, lhs_class);
            self.pin_fpr(lhs_fpr);
            let rhs_fpr = self.fetch_float_assume(ir, rhs, rhs_class);
            self.unpin_fpr(lhs_fpr);
            (lhs_fpr, rhs_fpr)
        } else {
            let lhs = self.fetch_float_assume(ir, lhs, lhs_class);
            (lhs, lhs)
        }
    }

    fn load_binary_ret_fpr(
        &mut self,
        ir: &mut AsmIr,
        dst: Option<SlotId>,
        info: FBinOpInfo,
    ) -> (FPReg, FPReg, Option<FPReg>) {
        let (lhs, rhs) = self.load_binary_fpr(ir, info);
        // Pin both operands while allocating the destination — `def_F` calls
        // `alloc_fpr`, which can otherwise pick `lhs` or `rhs` as the spill
        // victim and alias dst onto an operand the consumer still needs.
        self.pin_fpr(lhs);
        self.pin_fpr(rhs);
        let dst = dst.map(|dst| {
            if dst == info.lhs {
                self.def_F_with_fpr(dst, lhs);
                lhs
            } else {
                self.def_F(dst)
            }
        });
        self.unpin_fpr(rhs);
        self.unpin_fpr(lhs);
        (lhs, rhs, dst)
    }


    /// The compile-time comparison fold shared by the primitives and the
    /// binary inline generators' `CmpBr`-mode constant resolution.
    pub(crate) fn fold_cmp<T>(kind: CmpKind, lhs: T, rhs: T) -> bool
    where
        T: PartialEq + PartialOrd,
    {
        cmp(kind, lhs, rhs)
    }

    /// The fused float compare + branch: the xmm mirror of
    /// [`gen_cmpbr_integer`](Self::gen_cmpbr_integer). As a block terminator
    /// it flushes the GP residents to their homes before the branch (this
    /// also makes a mixed integer operand's home current for the compare's
    /// stack read), then compares in xmm and branches.
    pub(crate) fn gen_cmpbr_float(
        &mut self,
        ir: &mut AsmIr,
        info: FBinOpInfo,
        kind: CmpKind,
        brkind: BrKind,
        branch_dest: JitLabel,
    ) {
        self.flush_gp(ir);
        let mode = self.load_binary_fpr(ir, info);
        ir.float_cmp_br(mode, kind, brkind, branch_dest);
    }

    fn fetch_float_assume(&mut self, ir: &mut AsmIr, rhs: SlotId, class: FOpClass) -> FPReg {
        match class {
            FOpClass::Integer => self.load_fpr_fixnum(ir, rhs),
            FOpClass::Float => self.load_fpr(ir, rhs),
        }
    }
}
