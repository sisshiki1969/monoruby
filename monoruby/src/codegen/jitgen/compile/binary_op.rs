use crate::bytecodegen::BinOpK;
use crate::executor::inline::InlineFuncInfo;

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
    /// It may while `class#op` is still the builtin. Answering `false` sends
    /// the operation down the ordinary method-call path, which the
    /// class-version guard already protects — so a body recompiled after a
    /// redefinition keeps inlining every *other* operator.
    ///
    /// This is a pure check: the emitted code has no runtime check of its
    /// own, so the assumption must also be *recorded* — but only once a
    /// generator has actually emitted (or folded) code, via
    /// [`record_bop_dep`](Self::record_bop_dep). A generator that declines
    /// leaves no spurious dependency behind.
    pub(super) fn basic_op_assumable(&self, class: ClassId, op: IdentId) -> bool {
        // Only a pair the redefinition machinery tracks may be assumed: an
        // untracked pair is never marked redefined, so a guard-free inline of
        // it would outlive its own redefinition. Every pair the numeric
        // generators use is in the table; the index paths (`Hash#[]=`, which
        // deliberately has no entry) rely on this check to stay on the
        // class-version-guarded call path.
        if !self.store.is_basic_op_pair(class, op) {
            return false;
        }
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
    /// Attempt guard-free inline emission of `lhs_class#op` at this binop /
    /// comparison site through the registered binary inline generator.
    ///
    /// Returns `None` when the method doesn't resolve, has no binary
    /// generator, the basic-op license is gone (redefinition / refinement in
    /// scope), or the generator declined — the caller falls back to the
    /// ordinary path. **No class-version guard and no receiver-slot guard**
    /// are emitted: soundness comes from the recorded bop_dep (redefinition
    /// evicts every dependent body via `set_bop_redefine`), and the operand
    /// guards are emitted by the generator itself, only for operands not
    /// already proven (`gp_ensure` / the float-load discipline). The dep is
    /// recorded only when the generator emitted (`Done`) or folded
    /// (`Folded` — a fold bakes in the builtin's semantics just the same),
    /// so a declined generator leaves no spurious dependency.
    ///
    /// Visibility is deliberately not consulted, matching the ad-hoc fast
    /// paths this replaces (the interpreter's fast paths don't consult it
    /// either).
    ///
    #[allow(clippy::too_many_arguments)]
    fn fire_binary_inline(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        op: IdentId,
        lhs: SlotId,
        rhs: SlotId,
        lhs_class: ClassId,
        rhs_class: Option<ClassId>,
        bc_pos: BcIndex,
        mode: BinaryInlineMode,
    ) -> Option<BinaryInlineOutcome> {
        let (fid, _visibility) = self.jit_check_method(lhs_class, op)?;
        let InlineFuncInfo::InlineGenBinary(f) = self.store.inline_info.get_inline(fid)? else {
            return None;
        };
        if !self.basic_op_assumable(lhs_class, op) {
            return None;
        }
        // Every binop / cmp instruction carries a callsite (only RescueTEq
        // doesn't, and that opcode never reaches these dispatchers).
        let callid = self.store.get_callsite_id(self.iseq_id(), bc_pos).unwrap();
        debug_assert_eq!(self.store[callid].recv, lhs);
        debug_assert_eq!(self.store[callid].args, rhs);
        debug_assert!(self.store[callid].block_fid.is_none());
        match self.inline_asm_binary(state, ir, f, callid, lhs_class, rhs_class, mode) {
            BinaryInlineOutcome::Declined => None,
            outcome => {
                self.record_bop_dep(lhs_class, op);
                Some(outcome)
            }
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
            // The InlineGen registered on Integer#<< / Integer#>> /
            // Integer#** / Integer#% / Float#** / Float#% handles code
            // generation using both-side class info from the BinOp inline
            // cache (Integer#| / #& / #^ moved to the binary-generator
            // direct-fire arm below).
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
            _ => {
                let (lhs_class, rhs_class) = state.binary_class(lhs, rhs, ic);
                let Some(lhs_class) = lhs_class else {
                    // Recompiles (deopts) — its write-back re-homes the residents.
                    return Ok(self.binop_uncached(state, dst));
                };
                // The binary inline generator (`Integer#+`'s fixnum path,
                // `Float#+`'s xmm path, the mixed pairs) emits the constant
                // fold or the register fast path with no per-op runtime
                // guard. A basic-op redefinition is instead handled
                // method-wide by `set_bop_redefine`, identically on both
                // arches: compiled method entries are reverted (x86
                // `apply_jmp_patch_address` to `vm_entry`; aarch64
                // dispatch-slot zeroing in `invalidate_jit_code`), the VM's
                // `loop_start` handler is swapped for the no-opt one so
                // stale OSR loop bodies are never re-entered, and on-stack
                // frames deopt on return via `immediate_eviction`'s
                // return-address patching (both arches; see `emit_call`). A
                // `def` executed *inside* JIT code is caught by the
                // `check_bop` after `MethodDef`/`SingletonMethodDef`.
                if let Some(BinaryInlineOutcome::Done) = self.fire_binary_inline(
                    state,
                    ir,
                    kind.into(),
                    lhs,
                    rhs,
                    lhs_class,
                    rhs_class,
                    bc_pos,
                    BinaryInlineMode::Value,
                ) {
                    return Ok(CompileResult::Continue);
                }
                // Any C-ABI call flushes at its `get_using_fpr` chokepoint.
                self.call_binary_method(
                    state, ir, lhs, rhs, lhs_class, rhs_class, kind, bc_pos, false,
                )
            }
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
        let (lhs_class, rhs_class) = state.binary_class(lhs, rhs, ic);
        let Some(lhs_class) = lhs_class else {
            return Ok(CompileResult::Recompile(RecompileReason::NotCached));
        };
        // The comparison generators compute in GP / xmm without touching the
        // C ABI: the GP residents survive (a mixed compare's integer operand
        // is read straight from its register by the fpr load), and `dst`'s
        // stale resident is dropped by the result `def`. `TEq` resolves
        // `===` — on Integer an alias of `==`, on Float a distinct FuncId
        // with the Eq generator — so `case`-style compares inline too.
        if let Some(BinaryInlineOutcome::Done) = self.fire_binary_inline(
            state,
            ir,
            kind.into(),
            lhs,
            rhs,
            lhs_class,
            rhs_class,
            bc_pos,
            BinaryInlineMode::Value,
        ) {
            return Ok(CompileResult::Continue);
        }
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
            self.call_binary_method(state, ir, lhs, rhs, lhs_class, rhs_class, kind, bc_pos, true)
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
        let (lhs_class, rhs_class) = state.binary_class(lhs, rhs, ic);
        let Some(lhs_class) = lhs_class else {
            state.flush_gp(ir);
            return Ok(CompileResult::Recompile(RecompileReason::NotCached));
        };
        // Fused compare-and-branch through the binary inline generator. The
        // fixnum form guards + compares + branches in GP registers; the
        // float form flushes and compares in xmm (both block terminators
        // spill dirty residents before the branch). A both-operands-constant
        // compare comes back as `Folded` and the branch resolves statically
        // — no code, and an orphaned `dest` label is never resolved. This
        // path now checks the basic-op license and records the bop_dep
        // (via `fire_binary_inline`), which the old raw-`binop_type` arm
        // skipped: a fused `while i < n` loop compiled before a later
        // `Integer#<` redefinition is now evicted like every other form.
        let dest = self.label();
        match self.fire_binary_inline(
            state,
            ir,
            kind.into(),
            lhs,
            rhs,
            lhs_class,
            rhs_class,
            bc_pos,
            BinaryInlineMode::CmpBr { brkind, dest },
        ) {
            Some(BinaryInlineOutcome::Done) => {
                // Side-branch bookkeeping stays with the caller; the state is
                // cloned *after* emission so both successors see the operand
                // refinements (`refine_S_fixnum`) the guards established.
                let src_idx = bc_pos + 1;
                self.new_side_branch(src_idx, dest_bb, state.clone(), dest);
                return Ok(CompileResult::Continue);
            }
            Some(BinaryInlineOutcome::Folded(b)) => {
                return Ok(if b ^ (brkind == BrKind::BrIfNot) {
                    CompileResult::Branch(dest_bb)
                } else {
                    CompileResult::Continue
                });
            }
            Some(BinaryInlineOutcome::Declined) | None => {}
        }
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
        let res =
            self.call_binary_method(state, ir, lhs, rhs, lhs_class, rhs_class, kind, bc_pos, true)?;
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

    #[test]
    fn fused_cmpbr_bop_redefinition_evicts() {
        // Regression for the license hole the generator dispatch closed: the
        // fused compare-and-branch (`while i < n`) used to inline `Integer#<`
        // WITHOUT recording a bop_dep, so a post-warmup redefinition left the
        // compiled loop computing the builtin compare. Now the fused form
        // records the dep like every other spelling and the body is evicted.
        run_test_once(
            r##"
            def count(n)
              i = 0
              c = 0
              while i < n
                c += 1
                i += 1
              end
              c
            end
            a = []
            40.times { a << count(20) }
            class Integer
              def <(other) = false
            end
            a << count(20)
            a
        "##,
        );
    }

    #[test]
    fn explicit_send_operator_parity() {
        // The explicit-send spelling fires the same binary generators through
        // compile_method_call (class-version + receiver guards emitted there).
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            def drive
              res = []
              j = 0
              while j < 30
                res << 1.+(2)
                res << 5.-(2)
                res << 3.|(5)
                res << 4.&(6)
                res << 7.^(3)
                res << 2.0./(4)
                res << 1.==(1)
                res << 2.0.<(1)
                res << 2.!=(2)
                res << 3.>=(3.0)
                res << 1.===(1)
                res << 3.7.===(3.7)
                j = j + 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn mixed_numeric_pairs() {
        // Integer-op-Float and Float-op-Integer through the generators: the
        // arithmetic kinds compute in xmm (the old binop_type mixed collapse,
        // now explicit per method), comparisons in both value and fused form.
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            def m(a, b)
              [a + b, a - b, a * b, a / b, a < b, a <= b, a == b, a != b, (a > b ? 1 : 0)]
            end
            def drive
              res = []
              j = 0
              while j < 30
                res << m(1, 2.0)
                res << m(2.0, 1)
                res << m(3, 4)
                res << m(2.5, 1.5)
                j = j + 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn float_rhs_bitop_is_method_call() {
        // `1.0 | 2` used to classify as a Float binop (the binop_type mixed
        // collapse) and reach a per-arch `unreachable!` in the FloatBinOp
        // lowering. The bitwise generators decline a Float rhs, so the site
        // takes the ordinary method call and raises NoMethodError like CRuby.
        run_test_once(
            r##"
            r = 0
            20.times do
              begin
                r = 1 | 2
                1.0 | 2
              rescue NoMethodError
                r += 1
              end
            end
            r
        "##,
        );
    }

    #[test]
    fn case_when_numeric_teq_inline() {
        // case/when dispatches `===` on the `when` literal (the optimizable
        // TEq form): Integer#=== is an alias of `==`, Float#=== a distinct
        // FuncId with the Eq generator — both inline through the fused path.
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            def c(x)
              case x
              when 1 then :a
              when 2.5 then :b
              when 3 then :c
              else :d
              end
            end
            def drive
              res = []
              j = 0
              while j < 30
                res << c(1)
                res << c(2.5)
                res << c(3)
                res << c(:sym)
                j = j + 1
              end
              res
            end
        "##,
        );
    }
}
