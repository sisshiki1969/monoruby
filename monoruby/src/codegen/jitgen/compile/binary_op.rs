use crate::bytecodegen::BinOpK;
use crate::executor::BinaryOpFn;
use crate::executor::inline::InlineFuncInfo;

use super::{method_call::PMC_SET_SHARE_DIVISOR, *};

///
/// The operator at a binary site, as the dispatcher needs to see it.
///
/// `BinOpK` and `CmpKind` describe the same shape of site — one receiver, one
/// argument, an inline cache, a PMC and a call site — and every step of
/// lowering one is a step of lowering the other. The two things that genuinely
/// differ are both reachable from here: the name to resolve, and the
/// class-independent C helper the non-inlined path calls.
///
#[derive(Clone, Copy, PartialEq, Debug)]
pub(super) enum BinaryOp {
    Arith(BinOpK),
    Cmp(CmpKind),
}

impl From<BinOpK> for BinaryOp {
    fn from(kind: BinOpK) -> Self {
        BinaryOp::Arith(kind)
    }
}

impl From<CmpKind> for BinaryOp {
    fn from(kind: CmpKind) -> Self {
        BinaryOp::Cmp(kind)
    }
}

impl From<BinaryOp> for IdentId {
    fn from(op: BinaryOp) -> Self {
        match op {
            BinaryOp::Arith(kind) => kind.into(),
            BinaryOp::Cmp(kind) => kind.into(),
        }
    }
}

impl BinaryOp {
    ///
    /// The class-independent C implementation of this operator — the same
    /// `*_values` helpers the VM's generic path calls.
    ///
    /// Every one is a **guarded entry**: it consults the basic-op flag for
    /// the receiver's class and falls back to `invoke_method`, so it is
    /// correct for *any* operand pair and needs no receiver-class guard of
    /// its own. That is the property every non-deopting path here rests on —
    /// the residual arm of a two-arm dispatch and the polymorphic fallback
    /// both end in one of these, which is why an off-class operand takes a C
    /// call instead of a side exit.
    ///
    fn generic_fn(self) -> BinaryOpFn {
        use crate::executor::op;
        match self {
            BinaryOp::Arith(kind) => match kind {
                BinOpK::Add => op::add_values,
                BinOpK::Sub => op::sub_values,
                BinOpK::Mul => op::mul_values,
                BinOpK::Div => op::div_values,
                BinOpK::Rem => op::rem_values,
                BinOpK::Exp => op::pow_values,
                BinOpK::BitOr => op::bitor_values,
                BinOpK::BitAnd => op::bitand_values,
                BinOpK::BitXor => op::bitxor_values,
                BinOpK::Shl => op::shl_values,
                BinOpK::Shr => op::shr_values,
            },
            BinaryOp::Cmp(kind) => match kind {
                CmpKind::Eq => op::cmp_eq_values,
                CmpKind::Ne => op::cmp_ne_values,
                CmpKind::Lt => op::cmp_lt_values,
                CmpKind::Le => op::cmp_le_values,
                CmpKind::Gt => op::cmp_gt_values,
                CmpKind::Ge => op::cmp_ge_values,
                CmpKind::TEq => op::cmp_teq_values,
            },
        }
    }

    fn cmp_kind(self) -> Option<CmpKind> {
        match self {
            BinaryOp::Cmp(kind) => Some(kind),
            BinaryOp::Arith(_) => None,
        }
    }
}

///
/// How far [`JitContext::compile_binary`] got, and what the caller must still
/// do to sink the result.
///
/// The sink is the one thing the three opcodes genuinely disagree about — a
/// slot for `BinOp`/`BinCmp`, a fused branch for `BinCmpBr` — so it is the
/// one thing handed back rather than decided in the skeleton.
///
enum BinaryLowering {
    /// Inline code was emitted — a guarded generator, or the merge of a
    /// two-arm dispatch. In `CmpBr` mode the fused branch is emitted too and
    /// the caller records the side branch; in `Value` mode `dst` is defined.
    Emitted,
    /// `CmpBr` mode only: both operands were compile-time constants, so the
    /// comparison folded and *no code was emitted*. Carries the raw result,
    /// before `brkind` is applied.
    Folded(bool),
    /// The class-independent C helper was emitted and its error handled;
    /// `Option<Value>` is in rax for the caller to sink.
    Generic,
    /// The ordinary guarded method-call path ran.
    Called(CompileResult),
    /// Nothing was emitted for this instruction; hand the result straight
    /// back to the compile loop.
    Ceased(CompileResult),
}

///
/// The classes [`JitContext::opt_eq_assumable`] has to clear: every class
/// whose values `opt_eq_cmp`'s inline path can decide, i.e. every immediate
/// that is neither a heap pointer nor a flonum (both of which it sends to
/// the generic helper).
///
const OPT_EQ_IMMEDIATE_CLASSES: &[ClassId] = &[
    INTEGER_CLASS,
    NIL_CLASS,
    TRUE_CLASS,
    FALSE_CLASS,
    SYMBOL_CLASS,
];

impl<'a> JitContext<'a> {
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
    /// The shape is the one every operator dispatcher now shares: resolve
    /// `lhs_class#op`, **guard the receiver**, run the generator (which
    /// dispatches on the argument class — `Integer#+` emits the fixnum path
    /// for an Integer rhs and the xmm path for a Float one), and let anything
    /// it declines fall back to the ordinary method call.
    ///
    /// Returns `None` when the method doesn't resolve, has no generator, the
    /// basic-op license is gone (redefinition / refinement in scope, or a
    /// pair the eviction machinery doesn't track), or the generator declined.
    /// **No class-version guard** is emitted: soundness comes from the
    /// recorded bop_dep (a redefinition evicts every dependent body via
    /// `set_bop_redefine`). The dep is recorded only when the generator
    /// emitted (`Done`) or folded (`Folded` — a fold bakes in the builtin's
    /// semantics just the same), so a declined generator leaves no spurious
    /// dependency.
    ///
    /// The receiver guard is emitted here rather than inside each generator
    /// so that a generator may simply assume its receiver class — which is
    /// what lets the plain [`InlineFuncInfo::InlineGen`] operators
    /// (`Integer#% ** << >>`, whose code reads the receiver unguarded) fire
    /// from here too.
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
        // `Some`: the receiver guard exits through a counter-gated,
        // POLY-byte-checked recompile of *target* instead of a plain deopt
        // (see `compile_binary` step 4). `None` inside a dispatch arm, where
        // the state already knows the class and no guard is emitted at all.
        heal: Option<RecompileTarget>,
    ) -> Option<BinaryInlineOutcome> {
        // The Bignum tag resolves to Integer's methods, whose inline
        // generators compute raw fixnum arithmetic — firing one behind a
        // Bignum-profiled receiver would operate on a heap pointer. A
        // Bignum receiver has no inline form; the caller's residual
        // (generic helper / dispatch slow arm) is its fast path.
        if lhs_class == BIGNUM_CLASS {
            return None;
        }
        let (fid, _visibility) = self.jit_check_method(lhs_class, op)?;
        let inline = self.store.inline_info.get_inline(fid)?;
        if !matches!(
            inline,
            InlineFuncInfo::InlineGenBinary(_) | InlineFuncInfo::InlineGen(_)
        ) {
            // `Float#%` / `Float#**` are `CFunc_FF_F`, which the ordinary
            // call path already lowers to an inline xmm C call.
            return None;
        }
        if !self.basic_op_assumable(lhs_class, op) {
            return None;
        }
        // Every binop / cmp instruction carries a callsite (only RescueTEq
        // doesn't, and that opcode never reaches these dispatchers).
        let callid = self.store.get_callsite_id(self.iseq_id(), bc_pos).unwrap();
        debug_assert_eq!(self.store[callid].recv, lhs);
        debug_assert_eq!(self.store[callid].args, rhs);
        debug_assert!(self.store[callid].block_fid.is_none());
        // The receiver guard and the generator emit as one unit: a generator
        // that declines after the guard was emitted must leave no trace.
        let state_save = state.clone();
        let ir_save = ir.save();
        state.guard_recv_class(ir, lhs, lhs_class, heal);
        let outcome = match self.store.inline_info.get_inline(fid).unwrap() {
            InlineFuncInfo::InlineGenBinary(f) => {
                self.inline_asm_binary(state, ir, f, callid, lhs_class, rhs_class, mode)
            }
            // The plain generators (`Integer#% ** << >>`) have no fused
            // compare-and-branch form, so they only serve `Value` mode.
            InlineFuncInfo::InlineGen(f) if matches!(mode, BinaryInlineMode::Value) => {
                if self.inline_asm(state, ir, f, callid, Some(lhs_class), rhs_class) {
                    BinaryInlineOutcome::Done
                } else {
                    BinaryInlineOutcome::Declined
                }
            }
            _ => BinaryInlineOutcome::Declined,
        };
        match outcome {
            BinaryInlineOutcome::Declined => {
                *state = state_save;
                ir.restore(ir_save);
                None
            }
            outcome => {
                self.record_bop_dep(lhs_class, op);
                Some(outcome)
            }
        }
    }

    ///
    /// May [`AsmIr::opt_eq_cmp`]'s inline fast path be emitted for *kind*?
    ///
    /// That fast path answers **bit equality** whenever neither operand is a
    /// heap object or a flonum — so it decides `==` / `!=` for every one of
    /// `Integer` (fixnum), `nil`, `true`, `false` and `Symbol` without a
    /// method lookup and, until this check existed, without consulting the
    /// basic-op flag either. A `class Integer; def ==(other) = :redefined`
    /// left a compiled `a == b` still answering `true`, and no side exit
    /// could repair it: the class-version guard fires, the body recompiles,
    /// and the recompiled body emits the same unconditional fast path. Only
    /// `==`/`!=` were affected — `<`, `<=`, `>`, `>=` go straight to the
    /// generic helper, which does consult the flag.
    ///
    /// So take the fast path only under the same licence the guard-free
    /// inline generators take, for every class it decides, and record the
    /// dependency so `set_bop_redefine` evicts this body.
    ///
    fn opt_eq_assumable(&mut self, kind: CmpKind) -> bool {
        // `!=` is a tracked pair only for a few classes (`BASIC_OP_DEFS`);
        // where it is not, the runtime helper cannot see its redefinition
        // either, so demanding it here would only cost the fast path
        // without closing anything. `==` is tracked for all five.
        let eq = IdentId::_EQ;
        let op: IdentId = kind.into();
        let mut deps = Vec::with_capacity(OPT_EQ_IMMEDIATE_CLASSES.len() + 1);
        for &class in OPT_EQ_IMMEDIATE_CLASSES {
            if !self.basic_op_assumable(class, eq) {
                return false;
            }
            deps.push((class, eq));
            if op != eq && self.store.is_basic_op_pair(class, op) {
                if !self.basic_op_assumable(class, op) {
                    return false;
                }
                deps.push((class, op));
            }
        }
        for (class, op) in deps {
            self.record_bop_dep(class, op);
        }
        true
    }

    ///
    /// The receiver class a polymorphic comparison site should give its
    /// *typed* arm: the one the VM observed here that actually has a working
    /// generator, most observed first.
    ///
    /// Deliberately **not** the inline cache's class, for the same reason
    /// [`index_inline_class`](Self::index_inline_class) is not: the cache
    /// holds whichever class happened to arrive last, which at an alternating
    /// site is a coin flip. rubykon's `group_id_of(id) == captured.identifier`
    /// is the motivating shape — the lhs is an `Array` element, so `Integer`
    /// on a hit and `NilClass` on a miss — and inlining the `NilClass` side
    /// would leave every real comparison on the C call.
    ///
    /// `None` when no observed class can carry the arm, in which case the
    /// caller keeps the ordinary (guarded, deopting) path.
    ///
    /// *nil_arm_eligible*: the caller will give a nil share its own raw-bit
    /// arm. That changes the economics of the share gate: a nil runner-up
    /// below 1/8 still deopts on every appearance under the monomorphic
    /// guard (dewasm DOOM's `_f262` paid 16k exits at a ~1/20 nil share),
    /// while the nil arm costs the hot class one untaken compare — so a
    /// site with any observed nil dispatches regardless of the runner-up's
    /// share. Non-nil stragglers keep the 1/8 rule: their arm is the
    /// generic *call*, which really does want a fair share of the traffic.
    fn dispatch_inline_class(
        &mut self,
        callid: CallSiteId,
        op: IdentId,
        nil_arm_eligible: bool,
    ) -> Option<ClassId> {
        if !self.pmc_really_alternates(callid)
            && !(nil_arm_eligible && self.store[callid].pmc.entries().len() >= 2)
        {
            return None;
        }
        let pmc = &self.store[callid].pmc;
        let mut classes: Vec<(ClassId, u32)> =
            pmc.entries().iter().map(|e| (e.recv, e.count)).collect();
        classes.sort_unstable_by_key(|(_, count)| std::cmp::Reverse(*count));
        classes.into_iter().map(|(class, _)| class).find(|&class| {
            // The Bignum tag resolves to Integer's methods and would look
            // inlinable, but the arm guard proves the *fixnum*
            // representation — a Bignum can never enter it. Its share is
            // served by the residual arm (where `BrClassNe(INTEGER)`'s tag
            // test already routes every heap Integer).
            class != BIGNUM_CLASS
                && matches!(
                    self.jit_check_method(class, op)
                        .and_then(|(fid, _)| self.store.inline_info.get_inline(fid)),
                    Some(InlineFuncInfo::InlineGenBinary(_))
                )
                // Same licence the guarded direct-fire path needs: the arm
                // runs without a class-version guard, so a redefinition has
                // to reach it through the recorded bop dependency.
                && self.basic_op_assumable(class, op)
        })
    }

    ///
    /// Does the PMC show this site's *receiver* genuinely alternating
    /// between classes?
    ///
    /// The POLY bit only says the site was *ever* seen with a second
    /// class; it never clears. A site that is one hot class plus a
    /// handful of stragglers is monomorphic in every way that matters,
    /// and treating it polymorphically is a pure loss — the hot class
    /// pays an extra branch (or the generic call) on every execution. So
    /// require the runner-up to be a real share of the traffic, the same
    /// 1/8 the class-set guard demands of its members. (etanni measured
    /// ~2% slower before this test existed on the two-arm dispatch.)
    ///
    /// Did the VM ever observe *class* as this site's receiver?
    fn pmc_recv_contains(&self, callid: CallSiteId, class: ClassId) -> bool {
        self.store[callid]
            .pmc
            .entries()
            .iter()
            .any(|e| e.recv == class)
    }

    fn pmc_really_alternates(&self, callid: CallSiteId) -> bool {
        let pmc = &self.store[callid].pmc;
        if pmc.entries().len() < 2 {
            return false;
        }
        let observations = pmc.observations();
        let mut counts: Vec<u32> = pmc.entries().iter().map(|e| e.count).collect();
        counts.sort_unstable_by_key(|count| std::cmp::Reverse(*count));
        counts[1].saturating_mul(PMC_SET_SHARE_DIVISOR) >= observations
    }

    ///
    /// Did the VM only ever see one argument class at this site?
    ///
    /// The two-arm dispatch resolves the receiver; the argument is still
    /// handled by a guard inside the arm. So a varying argument is a deopt
    /// the dispatch cannot remove, and the site is better left alone.
    ///
    /// `None` (an argument whose class the VM did not record) counts as
    /// variation: it is exactly the case there is no evidence for.
    ///
    fn pmc_arg_is_stable(&self, callid: CallSiteId) -> bool {
        let mut args = self.store[callid]
            .pmc
            .entries()
            .iter()
            .map(|e| e.arg);
        let Some(Some(first)) = args.next() else {
            return false;
        };
        args.all(|a| a == Some(first))
    }

    ///
    /// Would every arm of this site answer the same *kind* of value?
    ///
    /// A numeric operator answers a float when either operand is one, and the
    /// arms differ only in the receiver. So they agree when the argument is a
    /// float — every arm answers a float — or when no receiver the VM observed
    /// is one, in which case every arm answers an integer. A site that mixes
    /// them can only merge as `S(Value)`, boxing the float arm's result for
    /// the next instruction to decode again.
    ///
    fn pmc_arms_agree_on_float(&self, callid: CallSiteId, rhs_class: Option<ClassId>) -> bool {
        rhs_class == Some(FLOAT_CLASS)
            || !self.store[callid]
                .pmc
                .entries()
                .iter()
                .any(|e| e.recv == FLOAT_CLASS)
    }

    ///
    /// Answer a polymorphic comparison site with a two-arm dispatch:
    ///
    /// ```text
    ///         br_class_ne rdi, C -> slow
    ///         <C#op inlined>
    ///         br merge
    ///   slow: <cmp_*_values>           (correct for *any* operand pair)
    ///   merge:
    /// ```
    ///
    /// The point is the missing third option: there is no deopt.
    ///
    /// Both halves of this already existed and the dispatcher had to pick
    /// one. `fire_binary_inline` won, because it is tried first — so a
    /// polymorphic site got a single-class guard and side-exited on every
    /// off-class operand, which is rubykon's single largest deopt source
    /// (1.8M side exits on one `==`). Taking `emit_generic_cmp` instead
    /// would remove the deopt but put *every* comparison, including the hot
    /// class's, on a C call. Neither is necessary: guard the hot class into
    /// its inline arm and let everything else take the C call.
    ///
    /// Note that the arm guard is stricter than "the class matches" for
    /// `Integer`: `guard_class` tests the fixnum tag, so a `Bignum` — class
    /// `Integer` all the same — falls to the residual arm rather than
    /// deopting, which is where an overflowed operand wants to go anyway.
    ///
    /// Returns `false` (leaving *state* and *ir* untouched) when the site
    /// does not qualify, so the caller takes the ordinary path.
    ///
    #[allow(clippy::too_many_arguments)]
    fn binary_dispatch(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        binop: BinaryOp,
        dst: Option<SlotId>,
        lhs: SlotId,
        rhs: SlotId,
        rhs_class: Option<ClassId>,
        case_semantics: bool,
        bc_pos: BcIndex,
    ) -> JitResult<bool> {
        let Some(callid) = self.store.get_callsite_id(self.iseq_id(), bc_pos) else {
            return Ok(false);
        };
        let op: IdentId = binop.into();
        // See the `nil_arm` comment below; computed up front because it also
        // loosens the dispatch gate (`dispatch_inline_class`).
        let nil_arm_eligible = matches!(binop, BinaryOp::Cmp(CmpKind::Eq | CmpKind::TEq))
            && self.pmc_recv_contains(callid, NIL_CLASS)
            && self.basic_op_assumable(NIL_CLASS, op);
        let Some(inline_class) = self.dispatch_inline_class(callid, op, nil_arm_eligible) else {
            return Ok(false);
        };
        // The dispatch guards the *receiver*. An arithmetic arm still guards
        // its own argument internally — `Integer#+` takes a different path for
        // an Integer rhs than for a Float one — so at a site whose argument
        // class varies, that inner guard keeps failing however the receiver is
        // resolved. Dispatching such a site buys nothing and costs the extra
        // branch plus a `dst` boxed at the merge; measured at +10% on a hot
        // `Float + (Float | Integer)` site whose deopt count did not move.
        //
        // The inline cache cannot see this: it holds the last pair only, so a
        // site with an alternating argument and one whose argument is always
        // the same both report `rhs_class: Float`. The PMC keeps the whole
        // distribution, so ask it.
        //
        // Comparisons are exempt. Their generators answer any operand pair
        // without an inner guard, and their result is a flag that costs
        // nothing to merge.
        // And the arms have to agree on the *kind* of result. Numeric `op`
        // answers a float when either operand is one, and the arms differ only
        // in the receiver — so they agree when the argument is a float (both
        // answer floats) or when no observed receiver is (both answer
        // integers). When they disagree, `dst` is a float on one side and a
        // fixnum on the other, the merge can only be `S(Value)`, and the float
        // arm pays a box the next float instruction has to undo. That measured
        // +62%.
        if matches!(binop, BinaryOp::Arith(_))
            && (!self.pmc_arg_is_stable(callid)
                || !self.pmc_arms_agree_on_float(callid, rhs_class))
        {
            return Ok(false);
        }
        let is_func_call = self.store[callid].is_func_call();

        let state_save = state.clone();
        let ir_save = ir.save();
        let (entry, merge) = self.declare_merge(state, ir, &[lhs, rhs], dst);
        let slow = self.label();

        // A `nil` receiver needs no method call at all for `==` / `===`:
        // `NilClass#==` is identity, and nil's encoding is unique, so the
        // answer is one raw bit-compare of the *argument* against
        // `NIL_VALUE`. Give it its own arm (behind the hot class's) instead
        // of sending the nil share through the generic helper — the
        // rubykon/DOOM alternating shapes spend half their traffic there.
        // Licence: the arm bakes in the builtin, so it is gated on and
        // recorded against the (NIL_CLASS, op) basic-op pair — `!=` has no
        // such pair and is excluded by the gate itself.
        let nil_arm = nil_arm_eligible && inline_class != NIL_CLASS;
        let nil_chk = self.label();
        let arm1_miss = if nil_arm { nil_chk.clone() } else { slow.clone() };

        // ---- arm 1: the class that can be compared inline.
        let mut fast = entry.clone();
        fast.load(ir, lhs, GP::Rdi);
        ir.push(AsmInst::BrClassNe(GP::Rdi, inline_class, arm1_miss));
        // Reaching the arm *is* the proof, so `fire_binary_inline`'s own
        // receiver guard sees a state that already knows the class and emits
        // nothing.
        fast.guard_class_state(lhs, inline_class);
        if !matches!(
            self.fire_binary_inline(
                &mut fast,
                ir,
                op,
                lhs,
                rhs,
                inline_class,
                rhs_class,
                bc_pos,
                BinaryInlineMode::Value,
                None,
            ),
            Some(BinaryInlineOutcome::Done)
        ) {
            // A generator that folded or declined has no arm to be; back the
            // whole dispatch out and let the caller emit the ordinary form.
            ir.restore(ir_save);
            *state = state_save;
            return Ok(false);
        }
        self.end_arm(fast, ir, &merge, true);

        // ---- nil arm: `nil == x` ⇔ `x` is nil, decided by the raw bits.
        if nil_arm {
            ir.push(AsmInst::Label(nil_chk));
            // Rdi still holds the receiver: the only way here is arm 1's
            // class branch, which sits after the load.
            let mut narm = entry.clone();
            ir.push(AsmInst::BrClassNe(GP::Rdi, NIL_CLASS, slow));
            narm.load(ir, rhs, GP::Rsi);
            // `IntegerCmpImm` is a raw `cmp` + flag-to-bool; `NIL_VALUE` is
            // not a tagged fixnum, but for `Eq` bit-equality is exactly the
            // question (no other value shares nil's encoding). `===` on nil
            // is the same identity test.
            ir.push(AsmInst::IntegerCmpImm {
                kind: CmpKind::Eq,
                dst: None,
                lhs: GP::Rsi,
                imm: crate::value::NIL_VALUE as i32,
            });
            narm.def_rax2acc(ir, dst);
            self.end_arm(narm, ir, &merge, true);
            self.record_bop_dep(NIL_CLASS, op);
        }

        // ---- residual arm: every other operand pair, through the generic
        // helper.
        ir.push(AsmInst::Label(slow));
        let mut rest = entry.clone();
        self.emit_generic_binary(&mut rest, ir, binop, lhs, rhs, case_semantics, is_func_call);
        rest.def_rax2acc(ir, dst);
        self.end_arm(rest, ir, &merge, false);

        self.bind_merge(state, ir, merge);
        Ok(true)
    }

    ///
    /// Answer a polymorphic site whose receiver the VM only ever saw as one
    /// (non-numeric) class with a two-arm dispatch around a direct call:
    ///
    /// ```text
    ///         br_class_ne rdi, C -> slow
    ///         <call C#op>                    (class-version guarded, no deopt)
    ///         br merge
    ///   slow: <generic helper>               (correct for *any* operand pair)
    ///   merge:
    /// ```
    ///
    /// The site is polymorphic in the inline cache's terms only: the pair
    /// key changed because the *argument* did. The receiver-keyed dispatch
    /// gives the one receiver class the call it would have had at a
    /// monomorphic site, with the helper — not a deopt — behind it for a
    /// receiver the VM never saw. Numeric receivers are left to the inline
    /// arms above, which already handle their argument variance.
    ///
    /// Returns `false` (leaving *state* and *ir* untouched) when the site
    /// does not qualify.
    ///
    #[allow(clippy::too_many_arguments)]
    fn binary_recv_dispatch(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        binop: BinaryOp,
        dst: Option<SlotId>,
        lhs: SlotId,
        rhs: SlotId,
        case_semantics: bool,
        bc_pos: BcIndex,
    ) -> JitResult<bool> {
        let Some(callid) = self.store.get_callsite_id(self.iseq_id(), bc_pos) else {
            return Ok(false);
        };
        let op: IdentId = binop.into();
        let mut recvs = self.store[callid].pmc.entries().iter().map(|e| e.recv);
        let Some(recv_class) = recvs.next() else {
            return Ok(false);
        };
        if recv_class == INTEGER_CLASS
            || recv_class == FLOAT_CLASS
            // The Bignum tag is numeric too (and `BrClassNe` cannot test a
            // representation): its sites belong to the generic helper.
            || recv_class == BIGNUM_CLASS
            || !recvs.all(|c| c == recv_class)
        {
            return Ok(false);
        }
        let Some((fid, visibility)) = self.jit_check_method(recv_class, op) else {
            return Ok(false);
        };
        // The ways out of `compile_method_call` that would leave the arm
        // unfinished, hoisted (as `pic_groups` does) so the arm always lands
        // on the merge.
        if self.jit_visibility_blocks(callid, visibility)
            || self.store[fid].possibly_capture_without_block()
            || self.store[fid]
                .is_iseq()
                .is_some_and(|iseq| self.store[iseq].has_block_arg())
        {
            return Ok(false);
        }
        let is_func_call = self.store[callid].is_func_call();

        let (entry, merge) = self.declare_merge(state, ir, &[lhs, rhs], dst);
        let slow = self.label();

        // ---- arm 1: the receiver class the VM saw, called directly.
        let mut fast = entry.clone();
        fast.load(ir, lhs, GP::Rdi);
        ir.push(AsmInst::BrClassNe(GP::Rdi, recv_class, slow));
        // Reaching the arm is the proof: `compile_method_call` sees a state
        // that already knows the class and emits no receiver guard.
        fast.guard_class_state(lhs, recv_class);
        let outcome = self.with_arm(false, |this| {
            this.compile_method_call(
                &mut fast,
                ir,
                recv_class,
                None,
                fid,
                visibility,
                callid,
                RecvMissMode::Plain,
            )
        })?;
        debug_assert!(matches!(outcome, CompileResult::Continue));
        self.end_arm(fast, ir, &merge, true);

        // ---- arm 2: every other operand pair, through the generic helper.
        ir.push(AsmInst::Label(slow));
        let mut rest = entry.clone();
        self.emit_generic_binary(&mut rest, ir, binop, lhs, rhs, case_semantics, is_func_call);
        rest.def_rax2acc(ir, dst);
        self.end_arm(rest, ir, &merge, false);

        self.bind_merge(state, ir, merge);
        Ok(true)
    }

    ///
    /// Lower a binary site: the shared skeleton behind `BinOp`, `BinCmp` and
    /// `BinCmpBr`.
    ///
    /// All three are the same instruction shape — one receiver, one argument,
    /// an inline cache, a PMC, a call site — and lower through the same five
    /// steps:
    ///
    /// 1. read the operand classes off the abstract state / inline cache;
    /// 2. try the two-arm dispatch, for a site the VM saw taking several
    ///    receiver classes;
    /// 3. give up (deopt, or widen under loop analysis) when the receiver
    ///    class is unknown *and* uncached;
    /// 4. fire the registered inline generator for `lhs_class#op`;
    /// 5. fall back: the guard-free generic helper once the VM has marked
    ///    the site polymorphic, a guarded call while it is still monomorphic.
    ///
    /// What is genuinely per-opcode is the *sink* — a slot, or a fused
    /// branch — and that is what the caller gets back in [`BinaryLowering`]
    /// to finish. Everything above it is shared, so a fix to the
    /// license checking, the bop dependency or the deopt policy lands on all
    /// three at once instead of on whichever tail happened to be edited.
    ///
    #[allow(clippy::too_many_arguments)]
    fn compile_binary(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        binop: BinaryOp,
        dst: Option<SlotId>,
        lhs: SlotId,
        rhs: SlotId,
        ic: Option<(ClassId, ClassId)>,
        polymorphic: bool,
        bc_pos: BcIndex,
        mode: BinaryInlineMode,
    ) -> JitResult<BinaryLowering> {
        // `BinCmpBr` is the case/when and rescue-matching opcode, and
        // dispatches `===` with funcall semantics; every other form is a
        // public-only call. The mode identifies the opcode, so the two need
        // not be threaded separately.
        let case_semantics = matches!(mode, BinaryInlineMode::CmpBr { .. });
        let (lhs_class, rhs_class) = state.binary_class(lhs, rhs, ic);

        // ---- 2. A site the VM saw comparing more than one lhs class, whose
        // lhs the abstract state cannot pin down: dispatch instead of
        // guarding. (A proven lhs class is monomorphic by construction,
        // whatever the VM saw at other times.)
        //
        // Not attempted in `CmpBr` mode: `CondBr` consumes the accumulator,
        // which a merge does not preserve — the arms would have to land the
        // flag in a slot and reload it — and the fused form is the reason
        // that opcode exists. Its polymorphic residual already never deopts,
        // and the fused sites are a rounding error in practice: ~24 side
        // exits across the whole benchmark set, against 1.8M for the plain
        // `BinCmp` the dispatch does take.
        if polymorphic
            && !case_semantics
            && state.class(lhs).is_none()
            && self.binary_dispatch(state, ir, binop, dst, lhs, rhs, rhs_class, false, bc_pos)?
        {
            return Ok(BinaryLowering::Emitted);
        }

        // ---- 3. Neither the state nor the cache knows the receiver class
        // (`Other(None, _)`, or the Shl-group `None`). There is nothing to
        // compile: deopt and recompile, so the block ends here.
        //
        // Both passes end it. In the codegen pass this is an *unconditional*
        // deopt — `recompile_and_deopt` emits `RecompileDeopt`, which always
        // exits to the interpreter — so the block never reaches whatever
        // follows, including a back-edge. The loop-analysis pass has to model
        // exactly that, and modelling it any other way is what makes the two
        // disagree: an analysis that continued past this point would record a
        // back-edge the codegen pass will not emit, or (worse) widen the loop
        // head for a body that never runs.
        //
        // Ending the *analysis* mid-block leaves the loop with no proven
        // back-edge, which `analyse_backedge_fixpoint` must not confuse with
        // "this loop always exits" — see the give-up handling there. A loop
        // whose back-edge is unreachable for want of a compilable instruction
        // is simply not a loop this JIT can compile.
        let Some(lhs_class) = lhs_class else {
            return Ok(BinaryLowering::Ceased(CompileResult::Recompile(
                RecompileReason::NotCached,
            )));
        };

        // ---- 3b. A comparison with exactly one bignum-constant operand
        // folds to its sign-decided answer here, BEFORE the inline
        // dispatch: the receiver guard the dispatch would emit is a
        // fixnum-representation test, which a bignum constant receiver
        // (`M64 >= x`) can never pass, so the fold has to intercept — the
        // constant side needs no runtime check at all, only the variable
        // side keeps its fixnum guard. Same discipline as
        // `fire_binary_inline`: the answer bakes in the builtin
        // comparison, so it is gated on bop assumability and records the
        // dependency.
        if let BinaryOp::Cmp(kind) = binop
            && lhs_class == INTEGER_CLASS
            && !polymorphic
        {
            let rhs_big = state.is_bigint_literal_sign(rhs).is_some();
            let lhs_big = state.is_bigint_literal_sign(lhs).is_some();
            // Exactly one constant side, and the variable side profiled
            // Integer (that's where the fold's fixnum guard lands).
            if rhs_big != lhs_big
                && (rhs_big || rhs_class == Some(INTEGER_CLASS))
                && self.basic_op_assumable(INTEGER_CLASS, binop.into())
            {
                match mode {
                    BinaryInlineMode::Value => {
                        if state.fold_bigint_const_cmp(ir, kind, dst, lhs, rhs) {
                            self.record_bop_dep(INTEGER_CLASS, binop.into());
                            return Ok(BinaryLowering::Emitted);
                        }
                    }
                    BinaryInlineMode::CmpBr { .. } => {
                        if let Some(b) = state.fold_bigint_const_cmpbr(ir, kind, lhs, rhs) {
                            self.record_bop_dep(INTEGER_CLASS, binop.into());
                            return Ok(BinaryLowering::Folded(b));
                        }
                    }
                }
            }
        }

        // ---- 3c. A *fused* comparison (`BinCmpBr`) at a site the VM marked
        // polymorphic and whose receiver genuinely alternates. The two-arm
        // *value* dispatch (step 2) cannot serve the fused form — its arms
        // would have to materialize the very flag the fusion exists to
        // avoid — so these sites used to fall through to the single-class
        // guard and side-exit on every off-class operand, forever (dewasm
        // DOOM: thousands of `Integer == nil` exits per tick, the "~24
        // fused side exits" note on step 2 notwithstanding). The 1/8-share
        // gate keeps the hot-class fast path for one-hot-class sites,
        // exactly as it does for the value dispatch.
        let fused_poly_callid = (polymorphic && case_semantics && state.class(lhs).is_none())
            .then(|| self.store.get_callsite_id(self.iseq_id(), bc_pos))
            .flatten();

        // The nil/Integer alternation gets real arms, not the C helper: the
        // arms of a *fused* dispatch need no value merge, because each one
        // branches for itself — the nil arm is one raw compare of the
        // argument against `NIL_VALUE` fused straight into the branch, and
        // the Integer arm is the ordinary fused inline (whose receiver
        // guard now only ever sees the leftover classes). Both arms jump to
        // the same branch label with every slot at home (`declare_merge`
        // flushed the operands, the inline's own flush covers its spills),
        // so the caller's single side-branch bookkeeping — registered
        // against the conservative merge state — covers them both.
        // Licence as everywhere: the nil arm bakes in `NilClass#==`/`===`,
        // so it is gated on and recorded against the basic-op pair.
        if let Some(callid) = fused_poly_callid
            && let BinaryInlineMode::CmpBr { brkind, dest } = mode
            && matches!(binop, BinaryOp::Cmp(CmpKind::Eq | CmpKind::TEq))
            && self.pmc_recv_contains(callid, NIL_CLASS)
            && self.pmc_recv_contains(callid, INTEGER_CLASS)
            // The peel has no generic arm: a receiver outside {nil,
            // Integer} lands on the Integer arm's guard, a plain deopt. So
            // it only serves sites whose whole observed profile is the
            // nil/Integer pair — anything richer keeps the guard-free
            // residual routing below. No share gate: the nil arm costs the
            // hot class one untaken compare, while an under-1/8 nil share
            // still deopts on every appearance under a monomorphic guard.
            && self.store[callid]
                .pmc
                .entries()
                .iter()
                .all(|e| e.recv == NIL_CLASS || e.recv == INTEGER_CLASS)
            && self.basic_op_assumable(NIL_CLASS, binop.into())
        {
            let state_save = state.clone();
            let ir_save = ir.save();
            let (entry, merge) = self.declare_merge(state, ir, &[lhs, rhs], None);
            let not_nil = self.label();

            // nil arm: `nil == x` ⇔ `x` is nil — raw bit compare, fused.
            let mut narm = entry.clone();
            narm.load(ir, lhs, GP::Rdi);
            ir.push(AsmInst::BrClassNe(GP::Rdi, NIL_CLASS, not_nil));
            narm.load(ir, rhs, GP::Rsi);
            // Raw `cmp` + fused branch; `NIL_VALUE` is not a tagged fixnum,
            // but for `Eq` bit-equality is exactly the question (no other
            // value shares nil's encoding; `===` on nil is the same test).
            ir.push(AsmInst::IntegerCmpBrImm {
                kind: CmpKind::Eq,
                brkind,
                branch_dest: dest,
                lhs: GP::Rsi,
                imm: crate::value::NIL_VALUE as i32,
            });
            self.end_arm(narm, ir, &merge, true);

            // Integer arm: the fused inline the mono path would have
            // emitted, now behind the nil peel — its fixnum guard still
            // deopts for a class the site never showed (the profile-subset
            // gate above keeps this to genuinely unobserved classes).
            ir.push(AsmInst::Label(not_nil));
            let mut iarm = entry.clone();
            match self.fire_binary_inline(
                &mut iarm,
                ir,
                binop.into(),
                lhs,
                rhs,
                INTEGER_CLASS,
                rhs_class,
                bc_pos,
                mode,
                None,
            ) {
                Some(BinaryInlineOutcome::Done) => {
                    self.record_bop_dep(NIL_CLASS, binop.into());
                    self.end_arm(iarm, ir, &merge, false);
                    self.bind_merge(state, ir, merge);
                    return Ok(BinaryLowering::Emitted);
                }
                // The generator declined (or, impossibly for an unknown
                // receiver, folded): back the peel out and take the
                // residual routing below.
                _ => {
                    ir.restore(ir_save);
                    *state = state_save;
                }
            }
        }

        // Residual routing for the remaining fused polymorphic shapes: the
        // generic helper, which for `==`/`!=` is `opt_eq_cmp`'s inline
        // bit-equality fast path — guard-free either way. Unlike the peel
        // (whose nil arm costs the hot class one compare, so any observed
        // nil justifies it), sending *everything* to the helper taxes the
        // hot class on each execution, so the 1/8-share gate stands here.
        let fused_poly_residual =
            fused_poly_callid.is_some_and(|callid| self.pmc_really_alternates(callid));

        // ---- 4. One path for every operator: guard the receiver, run the
        // generator registered on `lhs_class#op`, and fall back for whatever
        // it declines. The generator picks its emission from the argument
        // class — `Integer#+` computes in GP registers for an Integer rhs and
        // in xmm for a Float one — so the Integer/Float pairs are not a case
        // the dispatcher knows about. `TEq` resolves `===` — on Integer an
        // alias of `==`, on Float a distinct FuncId with the Eq generator —
        // so `case`-style compares inline too.
        //
        // The emitted code carries no per-op check that the operator is still
        // the builtin: a basic-op redefinition is handled method-wide by
        // `set_bop_redefine`, identically on both arches. Compiled method
        // entries are reverted (x86 `apply_jmp_patch_address` to `vm_entry`;
        // aarch64 dispatch-slot zeroing in `invalidate_jit_code`), the VM's
        // `loop_start` handler is swapped for the no-opt one so stale OSR
        // loop bodies are never re-entered, and on-stack frames deopt on
        // return via the eviction walk's return-address patching (see
        // `emit_call`). A `def` executed *inside* JIT code is caught by the
        // `check_bop` after `MethodDef`/`SingletonMethodDef`.
        if !fused_poly_residual {
            // A site still monomorphic in the VM's eyes gets the plain
            // single-class guard — but that guard must not deopt forever
            // once the program *does* start feeding it a second class
            // (compile-before-variance is pure timing: the DOOM shape is a
            // fused `prev == key` whose `prev` starts feeding `nil` only
            // after the site compiled). Hand the guard a counter-gated
            // recompiling exit instead: after a few misses the site
            // recompiles and, now marked polymorphic, takes the dispatch /
            // residual treatment above. The exit only actually recompiles
            // once the VM has stamped the site's POLY byte (see the
            // `BecamePolymorphic` gate in the side-exit lowering), so a
            // miss the profile cannot describe as a class change can never
            // recompile-livelock (the activerecord `out_of_range?` storm
            // shape). A Bignum miss *is* a class change here — the binop
            // ICs record heap Integers under `BIGNUM_CLASS` — so a
            // fixnum-compiled site that starts seeing Bignums heals into
            // the dispatch, whose `Integer` arm is the fixnum tag test and
            // whose residual arm is exactly where the Bignum share wants
            // to run. At a site already compiled polymorphic, reaching
            // here means the polymorphic treatments were tried at *this*
            // compile and declined; a recompile would reproduce this very
            // body, so the guard deopts plainly.
            let heal = (!polymorphic)
                .then(|| self.recv_miss_recompile_target())
                .flatten();
            match self.fire_binary_inline(
                state,
                ir,
                binop.into(),
                lhs,
                rhs,
                lhs_class,
                rhs_class,
                bc_pos,
                mode,
                heal,
            ) {
                Some(BinaryInlineOutcome::Done) => {
                    // ④-b: the Integer/Float inline lowerings are pure
                    // arithmetic — overflow promotes via a Rust helper, guards
                    // and errors exit the trace, nothing dispatches Ruby code —
                    // so the unfrozen-slot proofs survive them. Other classes'
                    // inline generators are not audited for that; drop the
                    // proofs there.
                    if (lhs_class == INTEGER_CLASS || lhs_class == FLOAT_CLASS)
                        && (rhs_class == Some(INTEGER_CLASS) || rhs_class == Some(FLOAT_CLASS))
                    {
                        self.restore_unfrozen(dst);
                    }
                    return Ok(BinaryLowering::Emitted);
                }
                Some(BinaryInlineOutcome::Folded(b)) => return Ok(BinaryLowering::Folded(b)),
                Some(BinaryInlineOutcome::Declined) | None => {}
            }
        }

        // ---- 5. The residual.
        if polymorphic {
            // A site whose *argument* class varies while the receiver stays
            // one class — `children << node` over a dozen node classes — is
            // polymorphic only because the inline cache is keyed on the
            // pair. Its receiver's operator has no guard-free inline arm
            // (`Array#<<` is a builtin the bop table does not track), so it
            // used to fall straight to the generic helper: `invoke_method`
            // and a global-method-cache probe on every execution, twenty
            // times a monomorphic call. Branch on the receiver into a direct
            // call of the one method the VM saw instead, and let anything
            // else take the helper.
            if !matches!(mode, BinaryInlineMode::CmpBr { .. })
                && self.binary_recv_dispatch(state, ir, binop, dst, lhs, rhs, case_semantics, bc_pos)?
            {
                return Ok(BinaryLowering::Emitted);
            }
            // Any C-ABI call flushes at its `get_using_fpr` chokepoint; the
            // GP pool has to be spilled here because the helper clobbers it.
            state.flush_gp(ir);
            let is_func_call = self
                .store
                .get_callsite_id(self.iseq_id(), bc_pos)
                .is_some_and(|c| self.store[c].is_func_call());
            self.emit_generic_binary(state, ir, binop, lhs, rhs, case_semantics, is_func_call);
            return Ok(BinaryLowering::Generic);
        }
        // A BOOL_CLASS receiver whose operator has no unified resolution
        // (TrueClass and FalseClass diverge on it — `|` after boolean.rb's
        // alias, or a user redefinition on one class) has nothing to guard:
        // the profile tags every boolean BOOL_CLASS, so a recompile re-asks
        // the same unanswerable question and the site deopts forever.
        // Dispatch through the generic helper instead.
        if lhs_class == BOOL_CLASS
            && self
                .jit_check_method(lhs_class, IdentId::from(binop))
                .is_none()
        {
            state.flush_gp(ir);
            let is_func_call = self
                .store
                .get_callsite_id(self.iseq_id(), bc_pos)
                .is_some_and(|c| self.store[c].is_func_call());
            self.emit_generic_binary(state, ir, binop, lhs, rhs, case_semantics, is_func_call);
            return Ok(BinaryLowering::Generic);
        }
        // A Bignum-profiled receiver has no inline form and nothing worth
        // guarding: a direct call would need a compound representation
        // guard, and the generic helper already dispatches a heap Integer
        // correctly. Same treatment as the unresolvable BOOL case above —
        // and a site that later turns fixnum flips polymorphic (the tag
        // change stamps POLY) and takes the dispatch, whose Integer arm
        // serves the fixnum share inline.
        if lhs_class == BIGNUM_CLASS {
            state.flush_gp(ir);
            let is_func_call = self
                .store
                .get_callsite_id(self.iseq_id(), bc_pos)
                .is_some_and(|c| self.store[c].is_func_call());
            self.emit_generic_binary(state, ir, binop, lhs, rhs, case_semantics, is_func_call);
            return Ok(BinaryLowering::Generic);
        }
        // Still monomorphic: make the recv-class guard recompile on a miss, so
        // the site flips to the treatment above the moment the VM observes
        // class variance rather than side-exiting forever.
        state.flush_gp(ir);
        Ok(BinaryLowering::Called(self.call_binary_method(
            state,
            ir,
            lhs,
            rhs,
            lhs_class,
            rhs_class,
            IdentId::from(binop),
            bc_pos,
            RecvMissMode::PartB,
        )?))
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
        polymorphic: bool,
        bc_pos: BcIndex,
    ) -> JitResult<CompileResult> {
        match self.compile_binary(
            state,
            ir,
            kind.into(),
            dst,
            lhs,
            rhs,
            ic,
            polymorphic,
            bc_pos,
            BinaryInlineMode::Value,
        )? {
            BinaryLowering::Emitted => Ok(CompileResult::Continue),
            BinaryLowering::Generic => {
                state.def_rax2acc(ir, dst);
                Ok(CompileResult::Continue)
            }
            BinaryLowering::Called(res) | BinaryLowering::Ceased(res) => Ok(res),
            // `Folded` is a `CmpBr`-mode outcome: in `Value` mode a fold is
            // reported as `Done` with the constant already in `dst`.
            BinaryLowering::Folded(_) => unreachable!(),
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
        match self.compile_binary(
            state,
            ir,
            kind.into(),
            dst,
            lhs,
            rhs,
            ic,
            polymorphic,
            bc_pos,
            BinaryInlineMode::Value,
        )? {
            BinaryLowering::Emitted => Ok(CompileResult::Continue),
            BinaryLowering::Generic => {
                state.def_rax2acc(ir, dst);
                Ok(CompileResult::Continue)
            }
            BinaryLowering::Called(res) | BinaryLowering::Ceased(res) => Ok(res),
            BinaryLowering::Folded(_) => unreachable!(),
        }
    }

    #[allow(clippy::too_many_arguments)]
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
        let dest = self.label();
        let src_idx = bc_pos + 1;
        match self.compile_binary(
            state,
            ir,
            kind.into(),
            None,
            lhs,
            rhs,
            ic,
            polymorphic,
            bc_pos,
            BinaryInlineMode::CmpBr { brkind, dest },
        )? {
            // The fused form guards + compares + branches without ever
            // materializing the flag. Side-branch bookkeeping stays here; the
            // state is cloned *after* emission so both successors see the
            // operand refinements (`refine_S_fixnum`) the guards established.
            BinaryLowering::Emitted => {
                self.new_side_branch(src_idx, dest_bb, state.clone(), dest);
                Ok(CompileResult::Continue)
            }
            // A both-operands-constant compare emits no code, and the
            // orphaned `dest` label is never resolved.
            BinaryLowering::Folded(b) => Ok(if b ^ (brkind == BrKind::BrIfNot) {
                CompileResult::Branch(dest_bb)
            } else {
                CompileResult::Continue
            }),
            BinaryLowering::Generic => {
                self.gen_cond_br(state, ir, src_idx, dest_bb, brkind);
                Ok(CompileResult::Continue)
            }
            BinaryLowering::Called(res) => {
                if let CompileResult::Continue = res {
                    state.unset_class_version_guard();
                    state.unset_const_version_guard();
                    // An inline gen may have resolved the comparison to a
                    // state-known constant (e.g. `String == nil` folds to
                    // `false` under the gen's class guards, LinkMode::C on the
                    // callsite dst). The trailing branch must then be resolved
                    // statically, exactly like `TraceIr::CondBr` does —
                    // emitting a dynamic CondBr here would read a result from
                    // rax that no code ever produced.
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
                        self.gen_cond_br(state, ir, src_idx, dest_bb, brkind);
                    }
                }
                Ok(res)
            }
            BinaryLowering::Ceased(res) => Ok(res),
        }
    }

    ///
    /// Emit the class-independent C implementation of *binop*: no
    /// receiver-class guard, so the site never side-exits on receiver class
    /// variance (the rubykon `== nil` vs `== Symbol` pattern). The
    /// class-version guard is kept — it tracks the global class-version
    /// counter, not the receiver class, so it only fires on a real
    /// redefinition, never on class variance, which preserves the monotone
    /// recompile invariant. The result `Option<Value>` is left in rax.
    ///
    fn emit_generic_binary(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        binop: BinaryOp,
        lhs: SlotId,
        rhs: SlotId,
        // `BinCmpBr` (case/when and rescue matching) dispatches `===` with
        // funcall semantics; a plain `BinCmp` (`a === b`) is a public-only
        // call.
        case_semantics: bool,
        // The call site's func-call flag: for `==`/`!=`/`<=>`/plain `===` a
        // private operator is callable only from `self OP x`.
        is_func_call: bool,
    ) {
        state.write_back_slots(ir, &[lhs, rhs]);
        self.guard_class_version(state, ir, true);
        let error = ir.new_error(state);
        match binop.cmp_kind() {
            // `==`/`!=` get an inline immediate fast path with the generic
            // C-call as its fallback, under the same basic-op license the
            // guard-free inline generators take.
            Some(kind @ (CmpKind::Eq | CmpKind::Ne)) if self.opt_eq_assumable(kind) => {
                ir.opt_eq_cmp(state, lhs, rhs, kind, binop.generic_fn(), is_func_call)
            }
            // case/when `===` is funcall regardless (the helper forces it).
            Some(CmpKind::TEq) if case_semantics => {
                ir.generic_binop(state, lhs, rhs, crate::executor::op::cmp_teq_case_values, true)
            }
            _ => ir.generic_binop(state, lhs, rhs, binop.generic_fn(), is_func_call),
        }
        ir.handle_error(error);
        // The C helper can run arbitrary Ruby (a user-defined `==`, `coerce`);
        // invalidate cached guards so subsequent instructions re-establish
        // them.
        state.unset_class_version_guard();
        state.unset_const_version_guard();
        state.unset_side_effect_guard();
    }
}

#[cfg(test)]
mod tests {
    use super::{BinaryOp, INTEGER_CLASS, IdentId};
    use crate::bytecodegen::BinOpK;
    use crate::tests::*;
    use crate::{Globals, Value, ast::CmpKind, executor::Executor};

    /// The contract of [`BinaryOp`]: the C helper it hands out and the name it
    /// resolves must be the *same* operator.
    ///
    /// Both halves are hand-written 11- and 7-arm tables, which is exactly the
    /// shape where a copy-paste slip (`Sub => add_values`) is silent, survives
    /// every type check, and miscompiles. Comparing the helper against a real
    /// `invoke_method` of the name closes that: a table that is wrong but
    /// self-consistent still fails here.
    ///
    /// This also covers the arithmetic arms, which have no caller yet —
    /// arithmetic reaches the generic path only at a polymorphic site whose
    /// arm is not float-valued, so the table is checked in full here rather
    /// than only where a benchmark happens to exercise it.
    #[test]
    fn generic_fn_implements_the_operator_it_names() {
        let mut globals = Globals::new_test();
        let mut vm = Executor::init(&mut globals, "generic_fn_test").unwrap();
        let ops: Vec<BinaryOp> = [
            BinOpK::Add,
            BinOpK::Sub,
            BinOpK::Mul,
            BinOpK::Div,
            BinOpK::Rem,
            BinOpK::Exp,
            BinOpK::BitOr,
            BinOpK::BitAnd,
            BinOpK::BitXor,
            BinOpK::Shl,
            BinOpK::Shr,
        ]
        .into_iter()
        .map(BinaryOp::Arith)
        .chain(
            [
                CmpKind::Eq,
                CmpKind::Ne,
                CmpKind::Lt,
                CmpKind::Le,
                CmpKind::Gt,
                CmpKind::Ge,
                CmpKind::TEq,
            ]
            .into_iter()
            .map(BinaryOp::Cmp),
        )
        .collect();
        // All three orderings are needed to pin the comparison table down: a
        // greater-than pair alone cannot tell `<` from `<=` (both false), and
        // an equal pair alone cannot tell `<` from `>`. Fixnums every operator
        // here accepts, and none of them zero, so `/` and `%` are defined.
        for (l, r) in [(6, 3), (3, 3), (3, 6)] {
            let (lhs, rhs) = (Value::integer(l), Value::integer(r));
            for &binop in &ops {
                let name = IdentId::from(binop);
                let via_helper = (binop.generic_fn())(&mut vm, &mut globals, lhs, rhs, false)
                    .unwrap_or_else(|| panic!("{binop:?} on ({l}, {r}): helper raised"));
                let via_send = vm
                    .invoke_method(&mut globals, name, false, lhs, &[rhs], None, None)
                    .unwrap_or_else(|| panic!("{binop:?} on ({l}, {r}): send raised"));
                assert!(
                    Value::test_eq(&globals.store, via_helper, via_send),
                    "{binop:?} on ({l}, {r}): generic_fn gave {}, but sending {} gave {}",
                    via_helper.inspect(&globals.store),
                    name.get_name(),
                    via_send.inspect(&globals.store),
                );
            }
        }

        // Integers alone cannot pin `TEq` down: on Integer `===` *is* an alias
        // of `==`, so `TEq => cmp_eq_values` would pass everything above. A
        // receiver where the two genuinely differ is what closes it — `Integer
        // === 3` is true where `Integer == 3` is false.
        let klass = globals.store.get_module(INTEGER_CLASS).as_val();
        let three = Value::integer(3);
        let teq = BinaryOp::Cmp(CmpKind::TEq);
        let via_helper = (teq.generic_fn())(&mut vm, &mut globals, klass, three, false).unwrap();
        assert!(
            via_helper.as_bool(),
            "TEq's helper answered `Integer === 3` with {}",
            via_helper.inspect(&globals.store),
        );
    }

    /// The shape `cmp_dispatch` exists for: an lhs that alternates between a
    /// class with a generator and one without, which the old code answered
    /// with a single-class guard that side-exited on every other execution
    /// (rubykon's `group_id_of(id) == captured.identifier`, 1.8M deopts).
    /// The inlined arm must be the *Integer* one even though the inline
    /// cache holds whichever class arrived last.
    #[test]
    fn cmp_dispatch_alternating_nil_and_integer() {
        run_test(
            r#"
            class T
              def initialize = (@a = [1, 2, nil, 4])
              def get(i) = @a[i]
            end
            def probe(t, i, x) = t.get(i) == x
            t = T.new
            res = []
            600.times { |i| res << probe(t, i % 4, (i % 4) + 1) }
            [res.tally.sort_by { |k, _| k.to_s }, probe(t, 2, nil), probe(t, 0, 1)]
            "#,
        );
    }

    /// Every comparison kind through the dispatch, not just `==`/`!=` (which
    /// take the immediate fast path in the residual arm while the others go
    /// straight to the generic C call).
    #[test]
    fn cmp_dispatch_all_kinds() {
        run_test(
            r#"
            vals = [1, 2.5, 3, 4.5]
            def lt(a, b) = a < b
            def le(a, b) = a <= b
            def gt(a, b) = a > b
            def ge(a, b) = a >= b
            def ne(a, b) = a != b
            res = []
            600.times do |i|
              a = vals[i % 4]
              res << [lt(a, 3), le(a, 3), gt(a, 3), ge(a, 3), ne(a, 3)]
            end
            res.uniq.sort_by(&:to_s)
            "#,
        );
    }

    /// The residual arm must run *any* receiver, including a user class with
    /// its own `==`, and must raise from it exactly as the interpreter does.
    #[test]
    fn cmp_dispatch_residual_runs_user_code() {
        run_test_once(
            r#"
            class Weird
              def ==(other) = (other == 7 ? (raise ArgumentError, "no") : :weird)
            end
            def probe(a, b) = a == b
            vals = [1, Weird.new]
            res = []
            600.times { |i| res << probe(vals[i % 2], 3) }
            begin
              probe(Weird.new, 7)
            rescue ArgumentError => e
              res << e.message
            end
            res.tally.sort_by { |k, _| k.to_s }
            "#,
        );
    }

    /// The inlined arm is emitted guard-free under the basic-op licence, so a
    /// later `Integer#==` redefinition has to evict the compiled body through
    /// the recorded bop dependency.
    #[test]
    fn cmp_dispatch_bop_redefinition_evicts() {
        run_test_once(
            r#"
            def probe(a, b) = a == b
            vals = [1, nil]
            res = []
            600.times { |i| res << probe(vals[i % 2], 1) }
            class Integer
              def ==(other) = :redefined
            end
            [res.tally.sort_by { |k, _| k.to_s }, probe(1, 1), probe(nil, 1)]
            "#,
        );
    }

    /// `opt_eq_cmp`'s inline path answers bit-equality for every immediate,
    /// so a redefinition of any of *their* `==` has to evict the body. Before
    /// the licence check this returned the builtin's answer forever: no side
    /// exit could repair it, because the recompiled body emitted the same
    /// unconditional fast path. One class per run — a redefinition is global
    /// and would mask the others.
    #[test]
    fn opt_eq_respects_redefinition() {
        for (klass, recv) in [
            ("Integer", "1"),
            ("NilClass", "nil"),
            ("TrueClass", "true"),
            ("FalseClass", "false"),
            ("Symbol", ":s"),
        ] {
            run_test_once(&format!(
                r#"
                def probe(a, b) = a == b
                def probe_ne(a, b) = a != b
                vals = [{recv}, 1.5]
                res = []
                600.times {{ |i| res << probe(vals[i % 2], {recv}) }}
                600.times {{ |i| res << probe_ne(vals[i % 2], {recv}) }}
                class {klass}
                  def ==(other) = :redefined
                end
                [res.tally.size, probe({recv}, {recv}), probe_ne({recv}, {recv})]
                "#
            ));
        }
    }

    /// A site the VM marked polymorphic that is one hot class plus a rare
    /// straggler stays on the monomorphic guard — the POLY bit never clears,
    /// so without a share test every such site would pay the dispatch
    /// forever. Correctness is identical either way; this pins the shape.
    #[test]
    fn cmp_dispatch_declines_effectively_monomorphic() {
        run_test(
            r#"
            def probe(a, b) = a == b
            res = []
            probe(nil, 1)
            2000.times { |i| res << probe(i % 3, 1) }
            [res.tally.sort_by { |k, _| k.to_s }, probe(nil, 1), probe(1, 1)]
            "#,
        );
    }

    /// An arithmetic site the VM marks polymorphic. `TraceIr::BinOp` now
    /// carries that bit, and a site whose arm is not float-valued now
    /// dispatches on it. Every operand pair here — Integer/Integer,
    /// Float/Integer, Integer/Float, Float/Float, and a Bignum that overflows
    /// the fixnum tag — must come out the same whichever way the site is
    /// lowered, dispatched or guarded.
    #[test]
    fn binop_polymorphic_site_operand_matrix() {
        run_test(
            r#"
            class T
              def initialize = (@a = [1, 2.5, 3, 4.5, 1 << 70])
              def get(i) = @a[i]
            end
            def probe(t, i, j) = t.get(i) + t.get(j)
            t = T.new
            res = []
            600.times { |n| res << probe(t, n % 5, (n + 1) % 5) }
            [res.uniq.sort_by(&:to_s), probe(t, 4, 4), probe(t, 1, 3), probe(t, 0, 4)]
            "#,
        );
    }

    /// The same shape for a comparison, which *does* dispatch: the residual
    /// arm has to handle the Bignum (`guard_class` tests the fixnum tag, so a
    /// Bignum never reaches the inline arm) and the Float/Integer mixes.
    #[test]
    fn cmp_dispatch_operand_matrix() {
        run_test(
            r#"
            class T
              def initialize = (@a = [1, 2.5, 3, 4.5, 1 << 70])
              def get(i) = @a[i]
            end
            def probe(t, i, j) = t.get(i) < t.get(j)
            t = T.new
            res = []
            600.times { |n| res << probe(t, n % 5, (n + 1) % 5) }
            [res.tally.sort_by { |k, _| k.to_s }, probe(t, 4, 0), probe(t, 1, 4)]
            "#,
        );
    }

    /// A *fused* comparison (`if a == b`) at a genuinely alternating
    /// nil/Integer site. The two-arm dispatch cannot serve the fused form,
    /// so `compile_binary` step 3c routes it to the guard-free generic
    /// residual (`opt_eq_cmp`'s bit-equality fast path) instead of the
    /// single-class guard that used to side-exit on every nil (the dewasm
    /// DOOM `prev == key` shape). Every operand mix must answer exactly as
    /// the interpreter does.
    #[test]
    fn fused_cmp_poly_residual() {
        run_test(
            r#"
            def probe(prev, key)
              if prev == key
                :hit
              else
                :miss
              end
            end
            res = []
            300.times { |n| res << probe(n % 3 == 0 ? nil : n, 7) }
            [res.tally.sort_by { |k, _| k.to_s },
             probe(nil, nil), probe(nil, 7), probe(7, 7), probe(7.0, 7), probe(1 << 70, 7)]
            "#,
        );
    }

    /// The compile-before-variance half of the same story: the site compiles
    /// while still monomorphic (plain fused Integer compare, class guard),
    /// and only *then* starts seeing nil. The guard's `BecamePolymorphic`
    /// recompiling exit must flip the body to the polymorphic residual after
    /// a few misses — and, healed or not, every answer must match the
    /// interpreter through the transition.
    #[test]
    fn fused_cmp_mono_heals_to_poly() {
        run_test(
            r#"
            def probe(prev, key)
              if prev == key
                :hit
              else
                :miss
              end
            end
            res = []
            300.times { |n| res << probe(n, 7) }
            300.times { |n| res << probe(n % 3 == 0 ? nil : n, 7) }
            res.tally.sort_by { |k, _| k.to_s }
            "#,
        );
    }

    /// A fixnum-compiled site that starts seeing Bignums: the binop ICs
    /// record a heap Integer under the `BIGNUM_CLASS` tag, so the first
    /// Bignum operand is a class change — it stamps the POLY byte, feeds
    /// the PMC, and the guard's `BecamePolymorphic` exit heals the site
    /// into the polymorphic treatment (fused: the generic residual;
    /// value-mode: the two-arm dispatch, whose `Integer` arm is the fixnum
    /// tag test and whose residual arm serves the Bignum share) instead of
    /// side-exiting on every heap operand forever. Pins the semantics
    /// through the transition.
    #[test]
    fn mono_cmp_heals_on_bignum_misses() {
        run_test(
            r#"
            def probe(a, b)
              if a < b
                :lt
              else
                :ge
              end
            end
            res = []
            300.times { |n| res << probe(n, 100) }
            big = 1 << 62
            300.times { |n| res << probe(big + n, 100) }
            res.tally.sort_by { |k, _| k.to_s }
            "#,
        );
    }

    /// The fused nil-peel: an alternating nil/Integer receiver at a fused
    /// compare gets a raw-bit nil arm plus the ordinary fused Integer
    /// inline, under both branch polarities (`if` and `unless`), with the
    /// nil == nil, `===`, and Float-argument corners pinned against the
    /// interpreter.
    #[test]
    fn fused_nil_peel_operand_matrix() {
        run_test(
            r#"
            def hit(prev, key)
              if prev == key then :hit else :miss end
            end
            def inv(prev, key)
              unless prev == key then :ne else :eq end
            end
            def teq(prev, key)
              if prev === key then :hit else :miss end
            end
            res = []
            300.times do |n|
              v = n % 3 == 0 ? nil : n
              res << hit(v, 7) << inv(v, 7) << teq(v, 7)
            end
            [res.tally.sort_by { |k, _| k.to_s },
             hit(nil, nil), inv(nil, nil), teq(nil, nil),
             hit(nil, 7.0), hit(7, 7.0), hit(1 << 70, 7)]
            "#,
        );
    }

    /// The value-mode nil arm: the two-arm dispatch grows a raw-bit nil arm
    /// when the PMC shows a nil share, so `res = (a == b)` at an
    /// alternating site answers nil receivers without the generic helper.
    /// Semantics pinned across the operand mixes.
    #[test]
    fn value_cmp_nil_arm_operand_matrix() {
        run_test(
            r#"
            def probe(a, b) = (a == b)
            res = []
            300.times do |n|
              v = n % 3 == 0 ? nil : n
              res << probe(v, 7) << probe(v, nil)
            end
            [res.tally.sort_by { |k, _| k.to_s }, probe(nil, nil), probe(7, 7.0)]
            "#,
        );
    }

    /// A site that has only ever seen Bignum receivers compiles to the
    /// generic helper — no inline form exists for a heap Integer, and a
    /// class guard for the representation tag would deopt on every
    /// execution. Both the pure-Bignum and the value-mode mixed shape must
    /// answer exactly as the interpreter.
    #[test]
    fn bignum_mono_cmp_takes_generic() {
        run_test(
            r#"
            big = 1 << 62
            def probe(a, b) = a < b ? 1 : 0
            def eq(a, b) = (a == b)
            r = 0
            300.times { |n| r += probe(big + n, big) }
            ok = []
            300.times { |n| ok << eq(big + (n % 2), big) }
            [r, ok.tally.sort_by { |k, _| k.to_s }]
            "#,
        );
    }

    /// The polymorphic residual's `==` fast path answers bit equality for
    /// `nil` under the basic-op licence, so a `NilClass#==` redefinition must
    /// evict it and the site must dispatch the redefined method from then on
    /// — same contract the guard-free inline generators honor.
    #[test]
    fn poly_cmp_respects_nil_eq_redefinition() {
        // `run_test_once`: the 25× rerun harness would carry the NilClass
        // redefinition into the warmup of every later iteration. Test-mode
        // JIT thresholds make the in-script loops warm enough on their own.
        run_test_once(
            r#"
            def probe(prev, key)
              if prev == key
                :hit
              else
                :miss
              end
            end
            res = []
            300.times { |n| res << probe(n % 3 == 0 ? nil : n, 7) }
            class NilClass
              def ==(other) = true
            end
            60.times { |n| res << probe(n % 3 == 0 ? nil : n, 7) }
            res.tally.sort_by { |k, _| k.to_s }
            "#,
        );
    }

    /// A loop the compiler only ever sees pass straight through, because the
    /// one instruction in its body is uncached — `Mutex#lock`'s
    /// `until try_lock` is the shape in the wild, and it is the reason both
    /// passes must end the block here rather than one of them widening on.
    ///
    /// With both ending it, the analysis records no back-edge, so the loop
    /// head keeps its precise pre-header types and the body compiles to a
    /// deopt: the "loop" becomes the straight-line code it has actually been
    /// observed to be. What this pins is the other half — that the back-edge
    /// still works when it finally *is* taken. The deopt recompiles the
    /// method against a now-warm cache, and the real loop has to produce the
    /// same answers as the interpreter.
    #[test]
    fn uncached_body_loop_compiles_straight_then_loops() {
        run_test(
            r#"
            def probe(a, obj)
              i = 0
              until a[i]
                i = obj + i
              end
              i
            end
            # Warm it with the body never entered: `obj + i` stays uncached
            # and `probe` compiles with that block as a deopt.
            res = []
            300.times { res << probe([true], nil) }
            # Now take the back-edge that was never compiled.
            [res.uniq,
             probe([false, true], 1),
             probe([false, false, true], 1),
             probe([false, false, false, true], 1),
             probe([true], nil)]
            "#,
        );
    }

    /// The arm the arithmetic dispatch newly makes live: a site whose
    /// receiver alternates between `Integer` and a class with its own `+`.
    /// The hot class inlines, and everything else takes `add_values`, which
    /// re-dispatches the name — so the user method has to run, and raise
    /// where it raises.
    ///
    /// This is the shape the dispatch is *for*. rubykon's arithmetic deopts
    /// look similar in the profile but are not: its `Integer` guard failures
    /// are a Bignum failing the fixnum-tag test, and the JIT does not compile
    /// Bignum arithmetic — deopting there is the intended handling, and the
    /// site is monomorphic anyway.
    ///
    /// A Bignum reaching a site that *does* dispatch is handled rather than
    /// exited: it fails the arm's fixnum-tag guard and lands in the residual,
    /// where `add_values` does the BigInt in C. Hence the `1 << 70` below.
    #[test]
    fn binop_dispatch_residual_runs_user_plus() {
        run_test(
            r#"
            class W
              def initialize(v) = (@v = v)
              def +(o) = @v + o * 10
            end
            def probe(x) = x + 1
            vals = [1, W.new(2), 3, W.new(4)]
            res = []
            600.times { |i| res << probe(vals[i % 4]) }
            [res.uniq.sort_by(&:to_s), probe(1 << 70), probe(2.5)]
            "#,
        );
    }

    /// A site polymorphic in the inline cache's terms only — one receiver
    /// class, an argument that varies — takes the receiver-keyed dispatch
    /// (`binary_recv_dispatch`): the observed class calls its method
    /// directly, a receiver the VM never saw lands in the generic helper,
    /// and a redefinition after warmup is caught by the class-version
    /// guard.
    #[test]
    fn binop_arg_varying_site_dispatches_on_receiver() {
        run_test(
            r#"
            def push(a, x) = a << x
            a = []
            200.times { |i| push(a, i.odd? ? "s" : :sym) }
            s = +"str"
            push(s, "x"); push(s, 33)
            def plus(a, b) = a + b
            r = []
            100.times { |i| r << plus("a", i.odd? ? "b" : "c") }
            def eq(a, b) = a == b
            q = []
            100.times { |i| q << eq([1], i.odd? ? [1] : "no") }
            [a.size, a[0..3], s, push(1, 3), r.uniq, plus(1, 2), q.uniq]
            "#,
        );
        // A redefinition is global, so this half runs once (the repeated
        // form would see the redefined operator from its second pass on).
        run_test_once(
            r#"
            def push(a, x) = a << x
            a = []
            100.times { |i| push(a, i.odd? ? "s" : :sym) }
            class Array
              alias push_orig push
              def <<(x) = (push_orig(x, :tagged); self)
            end
            b = []
            50.times { |i| push(b, i) }
            [a.size, b.size, b.last(2)]
            "#,
        );
    }

    /// Two shapes the arithmetic dispatch declines, both of which still have
    /// to answer correctly: a site whose arms disagree on the kind of result
    /// (`Float` receiver, `Integer` argument — one arm answers a float, the
    /// other a fixnum, so the merge could only box), and one whose argument
    /// class varies (the arm's own argument guard would keep deopting however
    /// the receiver is resolved).
    #[test]
    fn binop_float_site_keeps_its_guard() {
        run_test(
            r#"
            def disagree(x) = x + 1
            def unstable(x, y) = x + y
            vals = [1, 2.5, 3, 4.5]
            a = []
            b = []
            600.times do |i|
              a << disagree(vals[i % 4])
              b << unstable(vals[i % 4], vals[(i + 1) % 4])
            end
            [a.uniq.sort_by(&:to_s), b.uniq.sort_by(&:to_s),
             disagree(1 << 70), unstable(2.5, 1 << 70)]
            "#,
        );
    }

    /// The shape the dispatch keeps: every arm answers a float, because the
    /// argument is one, and the argument never varies. This is the case the
    /// earlier "exclude anything involving Float" rule gave up on — it is
    /// worth ~8% — and the receiver alternating is exactly what the dispatch
    /// resolves.
    #[test]
    fn binop_float_arg_site_dispatches() {
        run_test(
            r#"
            def probe(x) = x + 1.5
            vals = [1, 2.5, 3, 4.5]
            res = []
            600.times { |i| res << probe(vals[i % 4]) }
            [res.uniq.sort_by(&:to_s), probe(1 << 70), probe(-0.0)]
            "#,
        );
    }

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
