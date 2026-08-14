use num::Zero;

use crate::bytecodegen::BinOpK;
use crate::codegen::jitgen::state::Guarded;

use super::*;

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

    ///
    /// May the guard-free inline implementation of `class#op` be emitted?
    ///
    /// It may while `class#op` is still the builtin. Answering `true` also
    /// *records* the assumption, because the emitted code has no runtime
    /// check of its own: `set_bop_redefine` reads the recorded set back to
    /// find exactly the compiled bodies a later redefinition invalidates.
    /// Answering `false` sends the operation down the ordinary method-call
    /// path, which the class-version guard already protects — so a body
    /// recompiled after a redefinition keeps inlining every *other* operator.
    ///
    pub(super) fn assume_basic_op(&mut self, class: ClassId, op: IdentId) -> bool {
        if !self.basic_op_assumable(class, op) {
            return false;
        }
        self.record_bop_dep(class, op);
        true
    }

    /// The pure half of [`assume_basic_op`](Self::assume_basic_op): may the
    /// guard-free inline implementation of `class#op` be emitted? Records
    /// nothing — the direct-fire dispatch checks this *before* running a
    /// generator and calls [`record_bop_dep`](Self::record_bop_dep) only when
    /// the generator actually emitted (or folded) code, so a declined
    /// generator leaves no spurious dependency.
    pub(super) fn basic_op_assumable(&self, class: ClassId, op: IdentId) -> bool {
        // An ordinary redefinition binds everywhere.
        if self.store.basic_op_globally_redefined_for(class, op) {
            return false;
        }
        // A refinement binds lexically, so only a body compiled under a set
        // that actually resolves the pair differently has to give up its
        // inline path. Every other scope — including every scope in a program
        // that refines some *other* operator — keeps it.
        if self
            .store
            .basic_op_refined_in_scope(class, op, self.refinements())
        {
            return false;
        }
        true
    }

    /// Record the compiled body's dependence on the builtin `class#op`:
    /// `set_bop_redefine` reads the recorded set back to find exactly the
    /// bodies a later redefinition invalidates.
    pub(super) fn record_bop_dep(&mut self, class: ClassId, op: IdentId) {
        if !self.bop_deps.contains(&(class, op)) {
            self.bop_deps.push((class, op));
        }
    }

    ///
    /// [`AbstractState::binop_type`], demoted to the method-call
    /// classification when the operator its fast path would inline has been
    /// replaced.
    ///
    /// Both fast paths compute with the *receiver's* operator semantics — the
    /// integer path for `Integer op Integer`, the fpr path for `Float op
    /// Float` and for the mixed pairs (`1 + 2.0` is `Integer#+` with a Float
    /// argument, and computes in xmm without ever consulting `Float#+`). So
    /// the invariant to check, and to record, is the one belonging to `lhs`.
    ///
    fn binop_type_checked(
        &mut self,
        state: &AbstractState,
        op: IdentId,
        lhs: SlotId,
        rhs: SlotId,
        ic: Option<(ClassId, ClassId)>,
    ) -> BinaryOpType {
        let ty = state.binop_type(lhs, rhs, ic);
        if let BinaryOpType::Other(..) = ty {
            return ty;
        }
        let (lhs_class, rhs_class) = state.binary_class(lhs, rhs, ic);
        match lhs_class {
            Some(class) if !self.assume_basic_op(class, op) => {
                // Redefined: take the ordinary call instead, which the
                // class-version guard protects. Every other operator in this
                // body keeps its inline path.
                BinaryOpType::Other(lhs_class, rhs_class)
            }
            _ => ty,
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
            _ => match self.binop_type_checked(state, kind.into(), lhs, rhs, ic) {
                BinaryOpType::Integer(l, r) => {
                    // Both the constant fold (`100 * 100` -> 10000) and the
                    // register fast-path's inline arithmetic assume the builtin
                    // operator with no per-op runtime guard. A basic-op
                    // redefinition is instead handled method-wide by
                    // `set_bop_redefine`, identically on both arches: compiled
                    // method entries are reverted (x86 `apply_jmp_patch_address`
                    // to `vm_entry`; aarch64 dispatch-slot zeroing in
                    // `invalidate_jit_code`), the VM's `loop_start` handler is
                    // swapped for the no-opt one so stale OSR loop bodies are
                    // never re-entered, and on-stack frames deopt on return via
                    // `immediate_eviction`'s return-address patching (both
                    // arches; see `emit_call`). A `def` executed *inside* JIT
                    // code is caught by the `check_bop` after
                    // `MethodDef`/`SingletonMethodDef`.
                    state.binop_integer(ir, kind, dst, l, r);
                    Ok(CompileResult::Continue)
                }
                BinaryOpType::Float(info) => {
                    // The fpr path computes in xmm: it never touches the GP
                    // allocatable registers and never allocates (a flonum result
                    // stays in an FPReg; boxing is deferred to a later write-back,
                    // which flushes GP itself), and its deopt write-back already
                    // re-homes the GP residents. So the residents survive a flush.
                    // A GP-resident operand (the integer side of a mixed op) is
                    // read straight from its register by the fpr load (see
                    // `load_fpr_fixnum`); `dst`'s stale resident is dropped by the
                    // result `def`.
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
        match self.binop_type_checked(state, kind.into(), lhs, rhs, ic) {
            BinaryOpType::Integer(l, r) => {
                state.gen_cmp_integer(ir, kind, dst, l, r);
                Ok(CompileResult::Continue)
            }
            BinaryOpType::Float(info) => {
                // The float comparison computes in xmm and stores a bool: it
                // never touches the GP allocatable registers and never allocates,
                // and its deopt write-back re-homes the GP residents. The
                // residents survive; a GP-resident operand (the integer side of a
                // mixed compare) is read straight from its register by the fpr
                // load (see `load_fpr_fixnum`), and `dst`'s stale resident is
                // dropped by the result `def`.
                state.gen_cmp_float(ir, dst, info, kind);
                Ok(CompileResult::Continue)
            }
            BinaryOpType::Other(None, _) => {
                Ok(CompileResult::Recompile(RecompileReason::NotCached))
            }
            BinaryOpType::Other(Some(lhs_class), rhs_class) => {
                state.flush_gp(ir);
                if polymorphic {
                    let is_func_call = self
                        .store
                        .get_callsite_id(self.iseq_id(), bc_pos)
                        .is_some_and(|c| self.store[c].is_func_call());
                    self.emit_generic_cmp(state, ir, kind, lhs, rhs, false, is_func_call);
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
        // `BinCmpBr` (the optimizable opcode: case/when and rescue
        // matching) dispatches `===` with funcall semantics; a plain
        // `BinCmp` (`a === b`) is a public-only call.
        case_semantics: bool,
        // The call site's func-call flag: for `==`/`!=`/`<=>`/plain `===`
        // a private operator is callable only from `self OP x`.
        is_func_call: bool,
    ) {
        state.write_back_slots(ir, &[lhs, rhs]);
        // §9 9d-B: the generic comparison emits a C-ABI call; flush any
        // caller-saved GP-pool resident first (no-op when the pool is empty).
        self.guard_class_version(state, ir, true);
        let error = ir.new_error(state);
        // Part C: `==`/`!=` get an inline immediate fast path with a
        // generic C-call fallback; other cmp kinds use the plain
        // generic C-call (Part 3-B).
        match kind {
            CmpKind::Eq | CmpKind::Ne => {
                ir.opt_eq_cmp(state, lhs, rhs, kind, cmp_generic_fn(kind), is_func_call)
            }
            CmpKind::TEq if case_semantics => {
                // case/when `===` is funcall regardless (the helper forces it).
                ir.generic_binop(state, lhs, rhs, crate::executor::op::cmp_teq_case_values, true)
            }
            _ => ir.generic_binop(state, lhs, rhs, cmp_generic_fn(kind), is_func_call),
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
                    let is_func_call = self
                        .store
                        .get_callsite_id(self.iseq_id(), bc_pos)
                        .is_some_and(|c| self.store[c].is_func_call());
                    self.emit_generic_cmp(state, ir, kind, lhs, rhs, true, is_func_call);
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

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn binop_overflow_deopt_dirty_operand() {
        // Regression: an Add whose lhs is a *dirty* GP resident (a prior
        // binop result) must not compute in place — the overflow side-exit
        // re-homes the operand from its register, and an in-place op would
        // have clobbered it, making the interpreter re-execute the op with
        // a corrupted operand (silently wrong results).
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            def drive
              res = []
              a = 3
              b = 1537228672809129300
              c = 4611686018427387000
              j = 0
              while j < 30
                res << (a * b) + c + j
                res << (a * b) - c - j
                j = j + 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn binop_imm_overflow_boundaries() {
        // Immediate-form Add/Sub at the fixnum limits: the folded `add reg, 2k`
        // must still overflow-deopt exactly where tagged arithmetic does.
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            def drive
              res = []
              i = 4611686018427387800
              j = 0
              while j < 30
                res << i + 200            # crosses FIXNUM MAX -> bignum via deopt
                res << i + 103
                res << (-i) - 300         # crosses FIXNUM MIN
                res << i - 1
                i = i + 1
                j = j + 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn binop_imm_forms() {
        // Immediate forms and their fallbacks: small consts (folded),
        // i32-boundary consts (2k just fits / just overflows i32 -> register
        // fallback), commutative const-lhs Add, and const rhs Sub.
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            def drive
              res = []
              x = 1000
              j = 0
              while j < 30
                res << x + 1
                res << x - 7
                res << 1 + x
                res << x + 1073741823    # 2k == i32::MAX - 1: folded
                res << x + 1073741824    # 2k overflows i32: register form
                res << x - 1073741824
                res << x + (-5)
                j = j + 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn cmp_imm_forms() {
        // Immediate-form comparisons: bool results and fused compare+branch,
        // against small and i32-boundary constants (tagged 2k+1 gate).
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            def drive
              res = []
              x = 500
              j = 0
              while j < 40
                res << (j < 20) << (j <= 20) << (j > 20) << (j >= 20)
                res << (j == 7) << (j != 7)
                res << (x < 1073741823) << (x < 1073741824)
                if j < 25
                  res << :lo
                else
                  res << :hi
                end
                if x == 500
                  res << :eq
                end
                j = j + 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn binop_shared_operand_register() {
        // `x + x` (and friends): lhs and rhs share one register, so the
        // result must NOT compute in place there (regression: the in-place
        // clobber corrupted the rhs read, yielding an untagged even value).
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            def drive
              res = []
              j = 0
              while j < 30
                x = j + 3
                res << x + x << x * x << x - x << (x + x) + x
                res << (x == x) << (x < x)
                big = 4611686018427387000 + j
                res << big + big
                j = j + 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn binop_resident_transfer() {
        // Binding transfer: `y = x + k` where `x` is a clean live resident
        // hands x's register to y (no copy); `x` must still read correctly
        // from its home afterwards, including across an overflow deopt.
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            def drive
              res = []
              j = 0
              while j < 30
                x = j * 3
                y = x + 1
                z = x + y
                w = z - j
                res << x << y << z << w
                big = 4611686018427387900 + j
                p = big + 2
                q = big + 5
                res << p << q << big
                j = j + 1
              end
              res
            end
        "##,
        );
    }
}
