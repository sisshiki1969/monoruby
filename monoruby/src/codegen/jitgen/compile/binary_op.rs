use num::Zero;

use crate::bytecodegen::BinOpK;
use crate::codegen::jitgen::state::Guarded;

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

impl<'a> JitContext<'a> {
    ///
    /// Outcome of a binary op whose operand class is unknown *and* whose
    /// inline cache is empty (`Other(None, _)` / the Shl-group `None`).
    ///
    /// - **codegen pass**: deopt + recompile this single instruction. Only
    ///   this path bails at runtime; the rest of the loop stays JIT-compiled,
    ///   and once the inline cache warms the method recompiles cleanly.
    /// - **loop-analysis pass**: do *not* abort the block. Aborting drops the
    ///   loop's back-edge fix-point, which leaves the loop head un-widened —
    ///   a constant `C` that cannot be reconciled with the runtime `G` value
    ///   the real back-edge delivers, hitting the `(G, C)` `unreachable!` in
    ///   `bridge`. Widen the result to a typeless stack value (`S`) and
    ///   continue so the fix-point converges; the loop head is then widened
    ///   consistently and the real (codegen) pass emits the per-instruction
    ///   deopt above.
    ///
    fn binop_uncached(&self, state: &mut AbstractState, dst: Option<SlotId>) -> CompileResult {
        if self.codegen_mode() {
            CompileResult::Recompile(RecompileReason::NotCached)
        } else {
            if let Some(dst) = dst {
                state.def_S(dst);
            }
            CompileResult::Continue
        }
    }

    pub(super) fn binary_op(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        kind: BinOpK,
        dst: Option<SlotId>,
        lhs: SlotId,
        rhs: SlotId,
        ic: Option<(ClassId, ClassId)>,
        bc_pos: BcIndex,
    ) -> JitResult<CompileResult> {
        match kind {
            // These ops are always compiled as method calls.
            // The inline function registered on Integer#<< / Integer#>> /
            // Integer#| / Integer#& / Integer#^ / Integer#** / Integer#% /
            // Float#** / Float#% handles code generation using both-side class
            // info from the BinOp inline cache.
            BinOpK::Shl | BinOpK::Shr | BinOpK::Exp | BinOpK::Rem => {
                // Dispatched through `call_binary_method`. No flush here: a
                // register-only inline (e.g. `Integer#<<`) reads its operands
                // GP-resident-aware and keeps the residents live, while any C-ABI
                // call (a clobbering inline like `Array#<<`, or the cached
                // method-call path) flushes them at its `get_using_fpr` chokepoint.
                let (lhs_class, rhs_class) = state.binary_class(lhs, rhs, ic);
                match lhs_class {
                    None => Ok(self.binop_uncached(state, dst)),
                    Some(lhs_class) => self.call_binary_method(
                        state, ir, lhs, rhs, lhs_class, rhs_class, kind, bc_pos, false,
                    ),
                }
            }
            _ => match state.binop_type(lhs, rhs, ic) {
                BinaryOpType::Integer(l, r) => {
                    // A constant-fold bakes the result (e.g. `100 * 100` -> 10000)
                    // assuming the builtin operator, with no runtime trace of the
                    // op. Guard that assumption: emit `CheckBOP` *before* the fold
                    // (so the deopt write-back still sees the operand literals
                    // live) with the deopt PC at this op, so a later BOP
                    // redefinition deopts and the interpreter re-runs the op
                    // through the de-optimized VM handler. x86 recovers via the
                    // class-version-guard recompile path, so gate this to the
                    // aarch64 (no-recompile) build. Limited to the fold case: the
                    // register fast-path keeps its operands at runtime, so it is
                    // not worth a guard on every arithmetic op.
                    #[cfg(target_arch = "aarch64")]
                    if state.check_concrete_i64(l, r).is_some() {
                        ir.check_bop(state);
                    }
                    state.binop_integer(ir, kind, dst, l, r);
                    Ok(CompileResult::Continue)
                }
                BinaryOpType::Float(info) => {
                    // The fpr path computes in xmm: it never touches the GP
                    // allocatable registers and never allocates (a flonum result
                    // stays in an FPReg; boxing is deferred to a later write-back,
                    // which flushes GP itself), and its deopt write-back already
                    // re-homes the GP residents. So the residents survive a flush.
                    // But it reads its operands from their stack homes, so a
                    // GP-resident operand (the integer side of a mixed op) must be
                    // synced to its home first; then drop a stale cache of `dst`.
                    state.gp_sync_float_operands(ir, info.lhs, info.rhs, dst);
                    state.binop_float(ir, kind, dst, info);
                    Ok(CompileResult::Continue)
                }
                BinaryOpType::Other(None, _) => {
                    // Recompiles (deopts) — its write-back re-homes the residents.
                    Ok(self.binop_uncached(state, dst))
                }
                BinaryOpType::Other(Some(lhs_class), rhs_class) => {
                    // Any C-ABI call flushes at its `get_using_fpr` chokepoint.
                    self.call_binary_method(
                        state, ir, lhs, rhs, lhs_class, rhs_class, kind, bc_pos, false,
                    )
                }
            },
        }
    }

    pub(super) fn binary_cmp(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        kind: CmpKind,
        dst: Option<SlotId>,
        lhs: SlotId,
        rhs: SlotId,
        ic: Option<(ClassId, ClassId)>,
        polymorphic: bool,
        bc_pos: BcIndex,
    ) -> JitResult<CompileResult> {
        match state.binop_type(lhs, rhs, ic) {
            BinaryOpType::Integer(l, r) => {
                state.gen_cmp_integer(ir, kind, dst, l, r);
                Ok(CompileResult::Continue)
            }
            BinaryOpType::Float(info) => {
                // The float comparison computes in xmm and stores a bool: it
                // never touches the GP allocatable registers and never allocates,
                // and its deopt write-back re-homes the GP residents. The
                // residents survive, but the op reads its operands from their
                // stack homes — so sync a GP-resident operand (the integer side
                // of a mixed compare) first, then drop a stale cache of `dst`.
                state.gp_sync_float_operands(ir, info.lhs, info.rhs, dst);
                state.gen_cmp_float(ir, dst, info, kind);
                Ok(CompileResult::Continue)
            }
            BinaryOpType::Other(None, _) => {
                Ok(CompileResult::Recompile(RecompileReason::NotCached))
            }
            BinaryOpType::Other(Some(lhs_class), rhs_class) => {
                state.flush_gp(ir);
                if polymorphic {
                    self.emit_generic_cmp(state, ir, kind, lhs, rhs);
                    state.def_rax2acc(ir, dst);
                    Ok(CompileResult::Continue)
                } else {
                    // Monomorphic compile (POLY not yet set). Make the
                    // recv-class guard recompile-on-miss so the site
                    // flips to the generic path once the VM observes
                    // class variance (Part B).
                    self.call_binary_method(
                        state, ir, lhs, rhs, lhs_class, rhs_class, kind, bc_pos, true,
                    )
                }
            }
        }
    }

    ///
    /// Emit a non-deopting polymorphic comparison: a generic
    /// `cmp_*_values` C-call with **no receiver-class guard**, so the
    /// site never side-exits on receiver class variance (the rubykon
    /// `== nil` vs `== Symbol` pattern). The class-version guard is
    /// kept (it tracks the global class-version counter, not the
    /// receiver class, so it only fires on a real `==`/`<=>`
    /// redefinition — never on class variance, which preserves the
    /// Part B monotone-recompile invariant). Result `Option<Value>`
    /// is left in rax.
    ///
    fn emit_generic_cmp(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        kind: CmpKind,
        lhs: SlotId,
        rhs: SlotId,
    ) {
        state.write_back_slots(ir, &[lhs, rhs]);
        // §9 9d-B: the generic comparison emits a C-ABI call; flush any
        // caller-saved GP-pool resident first (no-op when the pool is empty).
        state.flush_pool(ir);
        self.guard_class_version(state, ir, true);
        let error = ir.new_error(state);
        // Part C: `==`/`!=` get an inline immediate fast path with a
        // generic C-call fallback; other cmp kinds use the plain
        // generic C-call (Part 3-B).
        match kind {
            CmpKind::Eq | CmpKind::Ne => {
                ir.opt_eq_cmp(state, lhs, rhs, kind, cmp_generic_fn(kind))
            }
            _ => ir.generic_binop(state, lhs, rhs, cmp_generic_fn(kind)),
        }
        ir.handle_error(error);
        // The C helper can run arbitrary Ruby (user-defined `==`,
        // `coerce`); invalidate cached guards so subsequent
        // instructions re-establish them.
        state.unset_class_version_guard();
        state.unset_const_version_guard();
        state.unset_side_effect_guard();
    }

    pub(super) fn binary_cmp_br(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        kind: CmpKind,
        lhs: SlotId,
        rhs: SlotId,
        dest_bb: BasicBlockId,
        brkind: BrKind,
        ic: Option<(ClassId, ClassId)>,
        polymorphic: bool,
        bc_pos: BcIndex,
    ) -> JitResult<CompileResult> {
        match state.binop_type(lhs, rhs, ic) {
            BinaryOpType::Integer(l, r) => {
                if let Some(result) = state.check_concrete_i64_cmpbr(l, r, kind, brkind, dest_bb) {
                    return Ok(result);
                }
                let src_idx = bc_pos + 1;
                let dest = self.label();
                state.gen_cmpbr_integer(ir, kind, l, r, brkind, dest);
                self.new_side_branch(src_idx, dest_bb, state.clone(), dest);
                Ok(CompileResult::Continue)
            }
            BinaryOpType::Float(info) => {
                if let Some(result) =
                    state.check_concrete_f64_cmpbr(lhs, rhs, kind, brkind, dest_bb)
                {
                    return Ok(result);
                }
                // Block terminator: spill the GP residents to their homes before
                // the branch. This also makes a mixed integer operand's home
                // current for the float compare's stack read.
                state.flush_gp(ir);
                let src_idx = bc_pos + 1;
                let dest = self.label();
                let mode = state.load_binary_fpr(ir, info);
                ir.float_cmp_br(mode, kind, brkind, dest);
                self.new_side_branch(src_idx, dest_bb, state.clone(), dest);
                Ok(CompileResult::Continue)
            }
            BinaryOpType::Other(None, _) => {
                state.flush_gp(ir);
                Ok(CompileResult::Recompile(RecompileReason::NotCached))
            }
            BinaryOpType::Other(Some(lhs_class), rhs_class) => {
                state.flush_gp(ir);
                if polymorphic {
                    self.emit_generic_cmp(state, ir, kind, lhs, rhs);
                    let src_idx = bc_pos + 1;
                    self.gen_cond_br(state, ir, src_idx, dest_bb, brkind);
                    return Ok(CompileResult::Continue);
                }
                // Monomorphic compile (POLY not yet set): recompile
                // on recv-class-guard miss so the site flips to the
                // generic path once class variance is observed.
                let res = self.call_binary_method(
                    state, ir, lhs, rhs, lhs_class, rhs_class, kind, bc_pos, true,
                )?;
                if let CompileResult::Continue = res {
                    state.unset_class_version_guard();
                    state.unset_const_version_guard();
                    // An inline gen may have resolved the comparison to a
                    // state-known constant (e.g. `String == nil` folds to
                    // `false` under the gen's class guards, LinkMode::C on
                    // the callsite dst). The trailing branch must then be
                    // resolved statically, exactly like `TraceIr::CondBr`
                    // does — emitting a dynamic CondBr here would read a
                    // result from rax that no code ever produced.
                    let callid = self.store.get_callsite_id(self.iseq_id(), bc_pos).unwrap();
                    let dst = self.store[callid].dst;
                    if let Some(dst) = dst
                        && state.is_truthy(dst)
                    {
                        if brkind == BrKind::BrIf {
                            return Ok(CompileResult::Branch(dest_bb));
                        }
                        // BrIfNot on a truthy value: branch statically dead.
                    } else if let Some(dst) = dst
                        && state.is_falsy(dst)
                    {
                        if brkind == BrKind::BrIfNot {
                            return Ok(CompileResult::Branch(dest_bb));
                        }
                        // BrIf on a falsy value: branch statically dead.
                    } else {
                        let src_idx = bc_pos + 1;
                        self.gen_cond_br(state, ir, src_idx, dest_bb, brkind);
                    }
                }
                Ok(res)
            }
        }
    }
}

///
/// The generic `Option<Value>`-returning C comparison helper for
/// *kind* — the same `cmp_*_values` functions the VM's generic
/// binop path calls. These dispatch the fixnum/float fast paths
/// internally and fall back to live method lookup for heap objects,
/// so they never require a receiver-class guard.
///
fn cmp_generic_fn(kind: CmpKind) -> crate::executor::BinaryOpFn {
    use crate::executor::op;
    match kind {
        CmpKind::Eq => op::cmp_eq_values,
        CmpKind::Ne => op::cmp_ne_values,
        CmpKind::Lt => op::cmp_lt_values,
        CmpKind::Le => op::cmp_le_values,
        CmpKind::Gt => op::cmp_gt_values,
        CmpKind::Ge => op::cmp_ge_values,
        CmpKind::TEq => op::cmp_teq_values,
    }
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

    fn check_concrete_i64(&self, lhs: SlotId, rhs: SlotId) -> Option<(i64, i64)> {
        let lhs = self.is_fixnum_literal(lhs)?.get();
        let rhs = self.is_fixnum_literal(rhs)?.get();
        Some((lhs, rhs))
    }

    #[allow(non_snake_case)]
    pub(super) fn check_binary_C_f64(&self, lhs: SlotId, rhs: SlotId) -> Option<(f64, f64)> {
        let lhs = self.coerce_C_f64(lhs)?;
        let rhs = self.coerce_C_f64(rhs)?;
        Some((lhs, rhs))
    }

    pub(super) fn check_concrete_i64_cmpbr(
        &mut self,
        lhs: SlotId,
        rhs: SlotId,
        kind: CmpKind,
        brkind: BrKind,
        dest_bb: BasicBlockId,
    ) -> Option<CompileResult> {
        if let Some((lhs, rhs)) = self.check_concrete_i64(lhs, rhs) {
            let b = cmp(kind, lhs, rhs) ^ (brkind == BrKind::BrIfNot);
            return Some(if b {
                CompileResult::Branch(dest_bb)
            } else {
                CompileResult::Continue
            });
        }
        None
    }

    pub(super) fn check_concrete_f64_cmpbr(
        &mut self,
        lhs: SlotId,
        rhs: SlotId,
        kind: CmpKind,
        brkind: BrKind,
        dest_bb: BasicBlockId,
    ) -> Option<CompileResult> {
        if let Some((lhs, rhs)) = self.check_binary_C_f64(lhs, rhs) {
            let b = cmp(kind, lhs, rhs) ^ (brkind == BrKind::BrIfNot);
            return Some(if b {
                CompileResult::Branch(dest_bb)
            } else {
                CompileResult::Continue
            });
        }
        None
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
    fn binop_integer(
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
        // `Mul` and `Div` destroy the `rhs` register before their overflow /
        // divide-by-zero side-exit (Mul `sarq`s it; Div's idiv sequence too).
        let rhs_clobbered = matches!(kind, BinOpK::Mul | BinOpK::Div);
        // 1. Load the operands into registers (reusing a resident copy).
        let (lhs_gp, lhs_fresh) = self.gp_ensure(ir, lhs, &[]);
        let (rhs_gp, rhs_fresh) = self.gp_ensure(ir, rhs, &[lhs_gp]);
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
        // 5. Allocate the result register and run the op. `Add`/`Sub` compute in
        //    the lhs position, so pin only `rhs_gp` and let the result reuse
        //    `lhs_gp` in place (no move). `Mul`/`Div` clobber `rhs` and (Div)
        //    produce in `rax`, so pin both operands — `lhs_gp` must survive the op
        //    intact for the deopt write-back — and take a distinct register.
        let pinned: &[GP] = if rhs_clobbered {
            &[lhs_gp, rhs_gp]
        } else {
            &[rhs_gp]
        };
        let (dst_gp, spill) = self.gp_regfile.alloc_reg(pinned);
        if let Some((reg, slot)) = spill {
            ir.reg2stack(reg, slot);
        }
        if lhs_fresh {
            ir.push(AsmInst::GuardClass(lhs_gp, INTEGER_CLASS, deopt));
        }
        if rhs_fresh {
            ir.push(AsmInst::GuardClass(rhs_gp, INTEGER_CLASS, deopt));
        }
        ir.integer_binop_reg(kind, dst_gp, lhs_gp, rhs_gp, deopt);
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

    /// Bring `slot` into a GP register: reuse its resident copy (returning
    /// `fresh = false`, no reload/guard) or materialize it to its stack home,
    /// load it, and report `fresh = true` so the caller emits the fixnum guard.
    fn gp_ensure(&mut self, ir: &mut AsmIr, slot: SlotId, pinned: &[GP]) -> (GP, bool) {
        if let Some(gp) = self.gp_regfile.reg_of(slot) {
            return (gp, false);
        }
        // Compile-time fixnum constant: load the tagged immediate straight into a
        // register, skipping the stack-home round-trip (`%1 = %2 + 1` loads `1`
        // as `movabs gp, 0x3` rather than materializing it to a slot and reading
        // it back). The value is a known fixnum, so it needs no guard — report
        // `fresh = false`.
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
        (gp, true)
    }

    /// prepare the GP register file for a float op that keeps
    /// the residents alive (it computes in xmm without clobbering them). The op
    /// reads its operands from their stack homes, so a GP-resident operand — the
    /// integer side of a mixed `Integer <op> Float` — is written back to its home
    /// and marked clean (it stays cached for a following integer op). Finally any
    /// stale GP cache of the result slot is dropped.
    fn gp_sync_float_operands(
        &mut self,
        ir: &mut AsmIr,
        lhs: SlotId,
        rhs: SlotId,
        dst: Option<SlotId>,
    ) {
        for slot in [lhs, rhs] {
            if let Some(reg) = self.gp_regfile.sync(slot) {
                ir.reg2stack(reg, slot);
            }
        }
        if let Some(dst) = dst {
            self.gp_regfile.invalidate(dst);
        }
    }

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

    fn binop_float(&mut self, ir: &mut AsmIr, kind: BinOpK, dst: Option<SlotId>, info: FBinOpInfo) {
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
                    // §19 (B): route the arithmetic through the record stream
                    // (collect + inline-emit + shadow-check) instead of a direct
                    // `fpr_binop`, so the operation joins the ordered codegen
                    // record alongside its operand loads.
                    ir.transfer(TransferIR::FloatBinOp { kind, lhs, rhs, dst });
                }
            }
        }
    }

    pub(super) fn gen_cmp_integer(
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
        let (lhs_gp, lhs_fresh) = self.gp_ensure(ir, lhs, &[]);
        let (rhs_gp, rhs_fresh) = self.gp_ensure(ir, rhs, &[lhs_gp]);
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
        if lhs_fresh {
            ir.push(AsmInst::GuardClass(lhs_gp, INTEGER_CLASS, deopt));
        }
        if rhs_fresh {
            ir.push(AsmInst::GuardClass(rhs_gp, INTEGER_CLASS, deopt));
        }
        ir.integer_cmp_reg(kind, dst, lhs_gp, rhs_gp);
        if let Some(dst) = dst {
            self.def_S(dst);
        }
    }

    pub(super) fn gen_cmp_float(
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
        // §19 (B): route the comparison through the record stream.
        ir.transfer(TransferIR::FloatCmp {
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
    pub(super) fn gen_cmpbr_integer(
        &mut self,
        ir: &mut AsmIr,
        kind: CmpKind,
        lhs: SlotId,
        rhs: SlotId,
        brkind: BrKind,
        branch_dest: JitLabel,
    ) {
        let (lhs_gp, lhs_fresh) = self.gp_ensure(ir, lhs, &[]);
        let (rhs_gp, rhs_fresh) = self.gp_ensure(ir, rhs, &[lhs_gp]);
        // Snapshot the deopt write-back before the flush/branch (the guards
        // side-exit to a point where the interpreter re-reads the operands).
        let deopt = ir.new_deopt(self);
        if lhs_fresh {
            ir.push(AsmInst::GuardClass(lhs_gp, INTEGER_CLASS, deopt));
        }
        if rhs_fresh {
            ir.push(AsmInst::GuardClass(rhs_gp, INTEGER_CLASS, deopt));
        }
        // Block terminator: spill the dirty residents to their stack homes before
        // the branch (the operands' registers still hold their values for the
        // compare), leaving the file empty for both successors.
        self.flush_gp(ir);
        ir.integer_cmpbr_reg(kind, brkind, branch_dest, lhs_gp, rhs_gp);
    }

    pub(super) fn load_binary_fpr(&mut self, ir: &mut AsmIr, info: FBinOpInfo) -> (FPReg, FPReg) {
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

    pub(super) fn load_binary_ret_fpr(
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

    fn fetch_float_assume(&mut self, ir: &mut AsmIr, rhs: SlotId, class: FOpClass) -> FPReg {
        match class {
            FOpClass::Integer => self.load_fpr_fixnum(ir, rhs),
            FOpClass::Float => self.load_fpr(ir, rhs),
        }
    }
}
