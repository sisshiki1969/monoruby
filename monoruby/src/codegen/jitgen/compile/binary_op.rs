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
            BinOpK::Shl
            | BinOpK::Shr
            | BinOpK::BitOr
            | BinOpK::BitAnd
            | BinOpK::BitXor
            | BinOpK::Exp
            | BinOpK::Rem => {
                let (lhs_class, rhs_class) = state.binary_class(lhs, rhs, ic);
                match lhs_class {
                    None => Ok(self.binop_uncached(state, dst)),
                    Some(lhs_class) => self.call_binary_method(
                        state, ir, lhs, rhs, lhs_class, rhs_class, kind, bc_pos, false,
                    ),
                }
            }
            _ => match state.binop_type(lhs, rhs, ic) {
                BinaryOpType::Integer(mode) => {
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
                    if state.check_concrete_i64(mode).is_some() {
                        ir.check_bop(state);
                    }
                    state.binop_integer(ir, kind, dst, mode);
                    Ok(CompileResult::Continue)
                }
                BinaryOpType::Float(info) => {
                    state.binop_float(ir, kind, dst, info);
                    Ok(CompileResult::Continue)
                }
                BinaryOpType::Other(None, _) => Ok(self.binop_uncached(state, dst)),
                BinaryOpType::Other(Some(lhs_class), rhs_class) => self.call_binary_method(
                    state, ir, lhs, rhs, lhs_class, rhs_class, kind, bc_pos, false,
                ),
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
            BinaryOpType::Integer(mode) => {
                state.gen_cmp_integer(ir, kind, dst, mode);
                Ok(CompileResult::Continue)
            }
            BinaryOpType::Float(info) => {
                state.gen_cmp_float(ir, dst, info, kind);
                Ok(CompileResult::Continue)
            }
            BinaryOpType::Other(None, _) => {
                Ok(CompileResult::Recompile(RecompileReason::NotCached))
            }
            BinaryOpType::Other(Some(lhs_class), rhs_class) => {
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
            BinaryOpType::Integer(mode) => {
                if let Some(result) = state.check_concrete_i64_cmpbr(mode, kind, brkind, dest_bb) {
                    return Ok(result);
                }
                let src_idx = bc_pos + 1;
                let dest = self.label();
                state.gen_cmpbr_integer(ir, kind, mode, brkind, dest);
                self.new_side_branch(src_idx, dest_bb, state.clone(), dest);
                Ok(CompileResult::Continue)
            }
            BinaryOpType::Float(info) => {
                if let Some(result) =
                    state.check_concrete_f64_cmpbr(lhs, rhs, kind, brkind, dest_bb)
                {
                    return Ok(result);
                }
                let src_idx = bc_pos + 1;
                let dest = self.label();
                let mode = state.load_binary_fpr(ir, info);
                ir.float_cmp_br(mode, kind, brkind, dest);
                self.new_side_branch(src_idx, dest_bb, state.clone(), dest);
                Ok(CompileResult::Continue)
            }
            BinaryOpType::Other(None, _) => {
                Ok(CompileResult::Recompile(RecompileReason::NotCached))
            }
            BinaryOpType::Other(Some(lhs_class), rhs_class) => {
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

    fn check_concrete_i64(&self, mode: OpMode) -> Option<(i64, i64)> {
        match mode {
            OpMode::RR(lhs, rhs) => {
                let lhs = self.is_fixnum_literal(lhs)?.get();
                let rhs = self.is_fixnum_literal(rhs)?.get();
                Some((lhs, rhs))
            }
            OpMode::RI(lhs, rhs) => {
                let lhs = self.is_fixnum_literal(lhs)?.get();
                Some((lhs, rhs as i64))
            }
            OpMode::IR(lhs, rhs) => {
                let rhs = self.is_fixnum_literal(rhs)?.get();
                Some((lhs as i64, rhs))
            }
        }
    }

    #[allow(non_snake_case)]
    pub(super) fn check_binary_C_f64(&self, lhs: SlotId, rhs: SlotId) -> Option<(f64, f64)> {
        let lhs = self.coerce_C_f64(lhs)?;
        let rhs = self.coerce_C_f64(rhs)?;
        Some((lhs, rhs))
    }

    pub(super) fn check_concrete_i64_cmpbr(
        &mut self,
        mode: OpMode,
        kind: CmpKind,
        brkind: BrKind,
        dest_bb: BasicBlockId,
    ) -> Option<CompileResult> {
        if let Some((lhs, rhs)) = self.check_concrete_i64(mode) {
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
            BinOpK::Rem
            | BinOpK::Exp
            | BinOpK::BitOr
            | BinOpK::BitAnd
            | BinOpK::BitXor
            | BinOpK::Shl
            | BinOpK::Shr => unreachable!(),
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
    fn binop_integer(&mut self, ir: &mut AsmIr, kind: BinOpK, dst: Option<SlotId>, mode: OpMode) {
        if let Some((lhs, rhs)) = self.check_concrete_i64(mode)
            && let Some(result) = self.binop_integer_folded(kind, lhs, rhs)
        {
            self.def_C(dst, result);
            return;
        };

        // §slot-IR (incremental, RR case): keep the AsmIR layer free of physical
        // registers for the reg-reg fixnum binop. Write the operand slots back to
        // their stack homes and emit a slot-based op; the LIR lowering loads them
        // into scratch regs, guards, computes, and stores the result to `dst`.
        // The result lives on the stack (`S`) — the universal interchange with
        // the still-register-based ops, so the two coexist. The immediate (RI/IR)
        // and discarded-result cases stay on the register-based path below.
        if let (BinOpK::Add | BinOpK::Sub | BinOpK::Mul, Some(dst), OpMode::RR(l, r)) =
            (kind, dst, mode)
        {
            self.write_back_slots(ir, &[l, r]);
            let deopt = self.deopt_point();
            ir.transfer(TransferIR::IntegerBinOpSlot {
                kind,
                dst,
                lhs: l,
                rhs: r,
                deopt,
            });
            // The result is a fixnum on the stack (the lowering stored it there).
            self.def_S_guarded(dst, Guarded::Fixnum);
            return;
        }

        match kind {
            BinOpK::Add | BinOpK::Sub | BinOpK::Mul => {
                let rhs = GP::Rsi;
                if let Some(dst) = dst {
                    // Compute the result *in place* in `dst`'s register — the
                    // value is born in its destination, so there is no `Rdi`-copy
                    // + relocate `mov` (the old `def_reg2acc_fixnum` store). The
                    // lhs operand is brought into that register (R15, or a pool
                    // register under gp-alloc so the result stays resident); when
                    // it is already there (`(a+b)+c`) not even a load is emitted.
                    let dst_reg = self.fetch_fixnum_inplace(ir, kind, rhs, mode);
                    // §19 (B): route the guarded integer op through the record
                    // stream, carrying its deopt as a program point.
                    let deopt = self.deopt_point();
                    ir.transfer(TransferIR::IntegerBinOp {
                        kind,
                        lhs: dst_reg,
                        rhs,
                        mode,
                        deopt,
                    });
                    self.def_inplace_fixnum(ir, dst, dst_reg);
                } else {
                    // Result discarded: run the op in the Rdi scratch purely for
                    // the overflow side-exit; no destination store.
                    let lhs = GP::Rdi;
                    match kind {
                        BinOpK::Sub => self.fetch_fixnum_mode(ir, lhs, rhs, mode),
                        _ => self.fetch_fixnum_comm(ir, lhs, rhs, mode),
                    }
                    let deopt = self.deopt_point();
                    ir.transfer(TransferIR::IntegerBinOp { kind, lhs, rhs, mode, deopt });
                }
            }
            BinOpK::Div => {
                let lhs = GP::Rdi;
                let rhs = GP::Rsi;
                self.fetch_fixnum_binary(ir, lhs, rhs, mode);
                let deopt = self.deopt_point();
                ir.transfer(TransferIR::IntegerBinOp { kind, lhs, rhs, mode, deopt });
                self.def_reg2acc_fixnum(ir, GP::Rax, dst);
            }
            BinOpK::Rem
            | BinOpK::Exp
            | BinOpK::BitOr
            | BinOpK::BitAnd
            | BinOpK::BitXor
            | BinOpK::Shl
            | BinOpK::Shr => unreachable!(),
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

    fn cmp_regs(&self, mode: OpMode) -> (GP, GP) {
        match mode {
            OpMode::RR(lhs, rhs) => (self.on_reg_or(lhs, GP::Rdi), self.on_reg_or(rhs, GP::Rsi)),
            OpMode::RI(lhs, _) => (self.on_reg_or(lhs, GP::Rdi), GP::Rsi),
            OpMode::IR(_, rhs) => (GP::Rdi, self.on_reg_or(rhs, GP::Rsi)),
        }
    }

    pub(super) fn gen_cmp_integer(
        &mut self,
        ir: &mut AsmIr,
        kind: CmpKind,
        dst: Option<SlotId>,
        mode: OpMode,
    ) {
        if let Some((lhs, rhs)) = self.check_concrete_i64(mode) {
            self.fold_constant_cmp(kind, lhs, rhs, dst);
            return;
        };
        let (lhs, rhs) = self.fetch_fixnum_mode_nodeopt(ir, mode);
        // §19 (B): route the comparison through the record stream.
        ir.transfer(TransferIR::IntegerCmp {
            mode,
            kind,
            lhs,
            rhs,
        });
        self.def_rax2acc(ir, dst);
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

    pub(super) fn gen_cmpbr_integer(
        &mut self,
        ir: &mut AsmIr,
        kind: CmpKind,
        mode: OpMode,
        brkind: BrKind,
        branch_dest: JitLabel,
    ) {
        let (lhs, rhs) = self.fetch_fixnum_mode_nodeopt(ir, mode);
        ir.integer_cmp_br(mode, kind, lhs, rhs, brkind, branch_dest);
    }

    /// Operand fetch for an *in-place* fixnum binop. Brings the lhs reg operand
    /// into the register where the encoder's in-place sequence leaves the result
    /// (via `fetch_lhs_to_inplace_reg`: R15 or a pool register), and returns that
    /// register so the caller can emit the op and define `dst` there with no
    /// relocate move. `rhs` is the scratch register for the rhs reg operand
    /// (`Rsi`).
    fn fetch_fixnum_inplace(
        &mut self,
        ir: &mut AsmIr,
        kind: BinOpK,
        rhs: GP,
        mode: OpMode,
    ) -> GP {
        match mode {
            OpMode::RR(l, r) => {
                self.load(ir, r, rhs);
                self.guard_fixnum(ir, r, rhs);
                self.fetch_lhs_to_inplace_reg(ir, l)
            }
            OpMode::RI(l, _) => {
                // rhs is an immediate folded by the encoder; lhs reg -> dst reg.
                self.fetch_lhs_to_inplace_reg(ir, l)
            }
            OpMode::IR(_, r) => match kind {
                // Add/Mul are commutative, so the reg operand becomes the
                // in-place accumulator (≡ RI) and the immediate is folded.
                BinOpK::Add | BinOpK::Mul => self.fetch_lhs_to_inplace_reg(ir, r),
                // Sub: the encoder materializes the immediate (lhs) into the
                // result register then subtracts the reg operand, so the result
                // register must be free and the reg operand in the `rhs` scratch.
                _ => {
                    self.load(ir, r, rhs);
                    self.guard_fixnum(ir, r, rhs);
                    self.alloc_inplace_reg(ir)
                }
            },
        }
    }

    fn fetch_fixnum_comm(&mut self, ir: &mut AsmIr, lhs: GP, rhs: GP, mode: OpMode) {
        match mode {
            OpMode::RR(l, r) => self.fetch_fixnum_rr(ir, l, r, lhs, rhs),
            OpMode::RI(slot, _) | OpMode::IR(_, slot) => self.fetch_fixnum_r(ir, slot, lhs),
        }
    }

    fn fetch_fixnum_mode(&mut self, ir: &mut AsmIr, lhs: GP, rhs: GP, mode: OpMode) {
        match mode {
            OpMode::RR(l, r) => self.fetch_fixnum_rr(ir, l, r, lhs, rhs),
            OpMode::RI(l, _) => self.fetch_fixnum_r(ir, l, lhs),
            OpMode::IR(_, r) => self.fetch_fixnum_r(ir, r, rhs),
        }
    }

    fn fetch_fixnum_mode_nodeopt(&mut self, ir: &mut AsmIr, mode: OpMode) -> (GP, GP) {
        let (lhs, rhs) = self.cmp_regs(mode);
        match mode {
            OpMode::RR(l, r) => self.fetch_fixnum_rr_nodeopt(ir, l, r, lhs, rhs),
            OpMode::RI(l, _) => self.fetch_fixnum_r_nodeopt(ir, l, lhs),
            OpMode::IR(_, r) => self.fetch_fixnum_r_nodeopt(ir, r, rhs),
        }
        (lhs, rhs)
    }

    fn fetch_fixnum_binary(&mut self, ir: &mut AsmIr, lhs: GP, rhs: GP, mode: OpMode) {
        match mode {
            OpMode::RR(l, r) => self.fetch_fixnum_rr(ir, l, r, lhs, rhs),
            OpMode::RI(l, r) => {
                ir.lit2reg(Value::i32(r as i32), rhs);
                self.fetch_fixnum_r(ir, l, lhs);
            }
            OpMode::IR(l, r) => {
                ir.lit2reg(Value::i32(l as i32), lhs);
                self.fetch_fixnum_r(ir, r, rhs);
            }
        }
    }

    fn fetch_fixnum_rr(&mut self, ir: &mut AsmIr, l: SlotId, r: SlotId, lhs: GP, rhs: GP) {
        self.load(ir, l, lhs);
        self.load(ir, r, rhs);
        self.guard_fixnum(ir, l, lhs);
        self.guard_fixnum(ir, r, rhs);
    }

    fn fetch_fixnum_rr_nodeopt(&mut self, ir: &mut AsmIr, l: SlotId, r: SlotId, lhs: GP, rhs: GP) {
        self.load(ir, l, lhs);
        self.load(ir, r, rhs);
        if self.is_fixnum_literal(l).is_some() && self.is_fixnum_literal(r).is_some() {
            return;
        }
        self.guard_fixnum(ir, l, lhs);
        self.guard_fixnum(ir, r, rhs);
    }

    fn fetch_fixnum_r(&mut self, ir: &mut AsmIr, slot: SlotId, r: GP) {
        self.load(ir, slot, r);
        if self.is_fixnum_literal(slot).is_some() {
        } else {
            self.guard_fixnum(ir, slot, r);
        }
    }

    fn fetch_fixnum_r_nodeopt(&mut self, ir: &mut AsmIr, slot: SlotId, r: GP) {
        self.load(ir, slot, r);
        if self.is_fixnum_literal(slot).is_some() {
        } else {
            self.guard_fixnum(ir, slot, r);
        }
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
