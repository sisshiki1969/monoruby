use super::*;

impl AbstractState {
    ///
    /// Join abstract states.
    ///
    pub(in crate::codegen::jitgen) fn join(&mut self, other: &AbstractState) {
        let innermost = self.frames.len() - 1;
        for (level, (lhs, rhs)) in self.frames.iter_mut().zip(other.frames.iter()).enumerate() {
            // Identity fast path: a frame neither path has touched since
            // they forked is the *same* `Rc` on both sides — the meet is
            // itself, no slot walk needed. In a deep specialization tower
            // this is every suspended outer frame of every merge.
            if FrameRef::ptr_eq(lhs, rhs) {
                continue;
            }
            // The two sides of a merge carry the same elided objects
            // (`doc/escape_analysis.md` §3): the meet has no way to
            // reconcile a difference (`V` against anything keeps `V`, and a
            // `Discard` would silently drop the object), so the entries of
            // a merge are settled first — an object every path brings
            // identically stays, any other is materialized on its path's
            // bridge (`JitContext::settle_entries`) — and a suspended outer
            // frame's parameter is untouched by the callee being compiled.
            debug_assert!(
                lhs.virtuals() == rhs.virtuals(),
                "elided objects differ at a merge at level {level}: {:?} / {:?}",
                lhs.virtuals(),
                rhs.virtuals()
            );
            let _ = innermost;
            // Only the innermost frame owns fp registers today: a call that
            // hands out a block spills every live one, so an outer frame
            // holds nothing unboxed and the merge must not hand it one
            // either — the two frames' fpr allocators are separate, and an
            // outer bridge has no way to load a slot into an fpr. Lifting
            // this is what carrying `F`/`Sf` across the boundary will need.
            FrameRef::make_mut(lhs).join_with(rhs, level == innermost);
        }
    }
}

impl Liveness {
    pub(in crate::codegen::jitgen) fn join(&mut self, state: &AbstractState) {
        for (i, is_used) in self.enumerate() {
            is_used.join(state.is_used(SlotId(i as u16)));
        }
        let n = state.slot_state().slots_len();
        for i in 0..n {
            let slot = SlotId(i as u16);
            self.join_subtree_float_read(slot, state.subtree_float_read(slot));
        }
    }
}

impl AbstractFrame {
    ///
    /// Join abstract states for the scope.
    ///
    /// ~~~text
    ///                              other
    ///       
    ///                  F      Sf      f64     i63      C
    ///              +-------+-------+-------+-------+-------+
    ///         F    |   F   |  Sf   |   F   |   S   |   S   |  F/Sf/S
    ///              +-------+-------+-------+-------+-------+
    ///         Sf   |   Sf  |  Sf   |   Sf  |  Sf   |   S   |  Sf/S
    ///  self        +-------+-------+-------+-------+-------|
    ///         f64  |   F   |  Sf   |  F*1  |   S   |   S   |  F/Sf/S/C
    ///              +-------+-------+-------+-------+-------|
    ///         i63  |   S   |  Sf   |   S   |   S   |   S   |  Sf/S
    ///              +-------+-------+-------+-------+-------|
    ///         C    |   S   |   S   |   S   |   S   |  S*2  |  S/C
    ///              +-------+-------+-------+-------+-------+
    ///
    ///  *1: if self == other, f64.
    ///  *2: if self == other, Const.
    ///
    /// ~~~
    ///
    /// `allow_fpr` is false for a frame that must stay free of fp registers
    /// (every frame but the innermost — see [`AbstractState::join`]). The
    /// decision stream is unchanged; each action that would place a value
    /// in an fpr is demoted to the `S` it already falls back to when no
    /// physical register is free, so the resulting *types* — and hence the
    /// type-meet shadow check — are the same either way.
    ///
    /// Allocation-free framewise join for resume chains (B2b).
    pub(in crate::codegen::jitgen) fn join_no_alloc(&mut self, other: &AbstractFrame) {
        self.join_with(other, false);
    }

    fn join_with(&mut self, other: &AbstractFrame, allow_fpr: bool) {
        // §5 allocator de-fusion, stage 1: record the per-slot `JoinAction`
        // stream as we merge, then (debug) replay it from the pre-merge frame and
        // assert it reproduces the identical placement. This locks the property
        // the separated allocator pass relies on — the (allocation-free) decision
        // stream plus `apply_join` is a *complete* record of the meet's placement
        // work — and is the regression harness future allocator changes shadow
        // against. See doc/regalloc_separation.md §12.
        #[cfg(debug_assertions)]
        let pre = self.clone();
        self.invariants.join(&other.invariants);
        #[cfg(debug_assertions)]
        let mut actions = Vec::new();
        for i in self.all_regs() {
            self.is_used_mut(i).join(other.is_used(i));
            // De-fuse the meet (§5): `decide_join` is a pure read-only function
            // of the two input `LinkMode`s (the merge *decision*); `apply_join`
            // performs the placement mutation — and is the *only* place the meet
            // allocates an fpr (`try_set_new_F` / `try_set_new_Sf`). This is the
            // seam the allocator pass will own: it will consume the `JoinAction`
            // stream and assign registers + emit edge moves, instead of
            // `apply_join` allocating inline. Behaviour is identical to the old
            // fused per-slot match.
            let mut action = self.decide_join(other, i);
            if allow_fpr {
                // Stage 1'', the loop-boundary meet: when one side holds a
                // *spill-home* `F` and the other a Float view in a
                // different register, prefer the spill home. The bridge on
                // the non-F side is then a register move into the home
                // (once, on that edge); the generic meet would instead
                // pick the register view and make the F side box at every
                // arrival — for a loop back edge, a `f64_to_val` per
                // iteration, the exact cost the deferral removes.
                action = match (self.mode(i), other.mode(i)) {
                    (LinkMode::F(x), LinkMode::Sf(y, SfGuarded::Float))
                    | (LinkMode::Sf(y, SfGuarded::Float), LinkMode::F(x))
                    | (LinkMode::F(x), LinkMode::F(y))
                        if x != y && x.0 >= PHYS_FPR_POOL =>
                    {
                        JoinAction::SetF(x)
                    }
                    (LinkMode::F(y), LinkMode::F(x))
                        if x != y && y.0 < PHYS_FPR_POOL && x.0 >= PHYS_FPR_POOL =>
                    {
                        JoinAction::SetF(x)
                    }
                    _ => action,
                };
            } else {
                // `SetF` (the stage-1'' spill-home meet) passes through:
                // it binds no new register, exactly like `SetSf`.
                action = action.without_fpr();
            }
            #[cfg(debug_assertions)]
            actions.push((i, action));
            self.apply_join(i, action);
        }
        #[cfg(debug_assertions)]
        self.verify_join_replay(other, pre, &actions);
        // Stage-A use propagation: merge the correctness-neutral hint
        // fields. Provenance survives a merge only when both paths agree;
        // a queued float-read report on either path is still evidence, so
        // the pending queues concatenate.
        self.join_subtree_read_meta(other);
    }

    ///
    /// Stage-1 + stage-2 shadow checks (debug-only), given the pre-merge frame
    /// `pre` (consumed as the replay target) and the recorded action stream:
    ///
    /// **Stage 2 — type-meet separability.** Assert the fused meet's *type*
    /// result (`self.guarded(i)`) equals the standalone `join_ty` pass —
    /// `join_ty` computes `Guarded`s with **no allocation**. Proven arm-by-arm:
    /// every non-sentinel arm's result type is `join_ty(self, other)` (the
    /// `SfGuarded → Guarded` map is a join homomorphism, so the `Sf` arms agree
    /// too). This is the type/placement split at the merge — a standalone
    /// type+liveness analysis pass computes identical types, with allocation
    /// peeled off into `apply_join` (doc §10 item 1, doc §12).
    ///
    /// **Stage 1 — placement record completeness.** Replay the recorded
    /// `JoinAction` stream from `pre` and assert it reproduces the merged
    /// placement (every slot's `LinkMode`), including the `try_alloc_fpr` phase-1
    /// cross-slot demotions.
    ///
    #[cfg(debug_assertions)]
    fn verify_join_replay(
        &self,
        other: &AbstractFrame,
        mut pre: AbstractFrame,
        actions: &[(SlotId, JoinAction)],
    ) {
        // Stage 2: the fused meet's type result == the allocation-free `join_ty`
        // pass, for every non-sentinel slot (`guarded()` is undefined on the
        // None/MaybeNone/V sentinels, and the meet leaves sentinel-involved slots
        // untyped).
        let is_sentinel =
            |m| matches!(m, LinkMode::None | LinkMode::MaybeNone | LinkMode::V);
        let expected_ty = pre.slot_state().join_ty(other.slot_state());
        for i in self.all_regs() {
            if is_sentinel(pre.mode(i)) || is_sentinel(other.mode(i)) || is_sentinel(self.mode(i))
            {
                continue;
            }
            debug_assert_eq!(
                self.guarded(i),
                expected_ty[i.0 as usize],
                "type-meet separability broken at {i:?}",
            );
        }

        // Stage 1: replay the action stream from `pre` and check placement.
        pre.invariants.join(&other.invariants);
        for &(i, action) in actions {
            pre.is_used_mut(i).join(other.is_used(i));
            pre.apply_join(i, action);
        }
        for i in self.all_regs() {
            debug_assert_eq!(
                pre.mode(i),
                self.mode(i),
                "JoinAction replay mismatch at {i:?}",
            );
        }
    }
}

///
/// The per-slot merge decision (§5 de-fusion): a pure function of the two
/// predecessors' `LinkMode`s, computed by [`AbstractFrame::decide_join`] and
/// executed by [`AbstractFrame::apply_join`]. Reifying the decision separates
/// the *meet* (analysis) from the *placement mutation + fpr allocation*
/// (codegen/allocation) — the prerequisite for moving allocation into its own
/// pass that lowers these actions to register assignments + edge moves.
///
#[derive(Debug, Clone, Copy, PartialEq)]
enum JoinAction {
    /// keep `self`'s binding unchanged
    Nop,
    /// `_ -> MaybeNone`
    SetMaybeNone,
    /// `_ -> V`
    Discard,
    /// registers disagree across branches: try to rebind to a fresh fpr so each
    /// bridge is a single move; keep the current `F` binding if no phys fpr is
    /// free (the bridge then swaps). [`F`/`F` arm]
    TryFreshFKeep,
    /// try fresh-fpr `F`; fall back to `S(Float)` if no phys fpr is free.
    /// [`C`/`F` and `C`/`C`-both-float arms]
    TryFreshFElseS,
    /// rebind to `Sf(x, guarded)` with the current fpr `x` (registers agree, or
    /// the `Sf`/`C` arm folding a literal into the guard).
    SetSf(FPReg, SfGuarded),
    /// try fresh-fpr `Sf(guarded)`; keep `Sf(x, guarded)` if no phys fpr is free.
    /// [`F`|`Sf` / `Sf`|`F` arm, registers disagree]
    TryFreshSfElseKeep(FPReg, SfGuarded),
    /// try fresh-fpr `Sf(guarded)`; fall back to `S(guarded)` if no phys fpr.
    /// [`C` / `Sf` arm]
    TryFreshSfElseS(SfGuarded),
    /// `_ -> S(guarded)`
    SetS(Guarded),
    /// rebind to `F(x)` with the current fpr `x` — the suspended-frame
    /// meet of a deferred-boxing claim (stage 1''): `F(h)` against
    /// `Sf(h, Float)` or `F(h)` weakens the slot-current half and keeps
    /// the home, emitting nothing (the home is current on both sides).
    SetF(FPReg),
}

impl JoinAction {
    ///
    /// The same decision for a frame that may not hold an fp register:
    /// every placement that would take one becomes the `S` it already
    /// degrades to under register pressure.
    ///
    fn without_fpr(self) -> Self {
        match self {
            JoinAction::TryFreshFKeep | JoinAction::TryFreshFElseS => {
                JoinAction::SetS(Guarded::Float)
            }
            // `SetSf` is the arm where both predecessors already agree on
            // the register, so it binds no new one — the only fpr-shaped
            // decision an outer frame can act on without a register
            // operation (see `AbstractFrame::bridge_at`).
            JoinAction::SetSf(x, guarded) => JoinAction::SetSf(x, guarded),
            JoinAction::TryFreshSfElseKeep(_, guarded)
            | JoinAction::TryFreshSfElseS(guarded) => JoinAction::SetS(guarded.into()),
            other => other,
        }
    }
}

impl AbstractFrame {
    ///
    /// Decide the merge action for slot *i* from the two predecessors' modes
    /// (the meet table in [`Self::join`]). Pure: reads `self`/`other` only.
    ///
    fn decide_join(&self, other: &AbstractFrame, i: SlotId) -> JoinAction {
        use JoinAction::*;
        match (self.mode(i), other.mode(i)) {
            (LinkMode::None, LinkMode::None) => Nop,
            (LinkMode::MaybeNone, _) => Nop,
            (_, LinkMode::MaybeNone) => SetMaybeNone,
            (LinkMode::V, _) => Nop,
            (_, LinkMode::V) => Discard,
            (LinkMode::F(l), LinkMode::F(r)) => {
                if l != r {
                    TryFreshFKeep
                } else {
                    Nop
                }
            }
            (LinkMode::F(_), LinkMode::C(r)) if r.is_float() => Nop,
            // Stage 1'' deferred boxing: `F(h)` against the same *spill*
            // home's `Sf` meets to `F(h)` — the home (the spill slot) is
            // the canonical copy on both sides, and only the slot-current
            // half is dropped. Emitting nothing on either edge. The
            // generic arm below would meet to `Sf`, forcing the F side to
            // box at every loop boundary — the cost the deferral exists
            // to remove.
            (LinkMode::F(x), LinkMode::Sf(y, SfGuarded::Float))
            | (LinkMode::Sf(x, SfGuarded::Float), LinkMode::F(y))
                if x == y && x.0 >= PHYS_FPR_POOL =>
            {
                SetF(x)
            }
            (LinkMode::F(x), LinkMode::Sf(_, _))
            | (LinkMode::Sf(x, _), LinkMode::Sf(_, _) | LinkMode::F(_)) => {
                let mut guarded = match self.mode(i) {
                    LinkMode::F(_) => SfGuarded::Float,
                    LinkMode::Sf(_, guarded) => guarded,
                    _ => unreachable!(),
                };
                let (other_fpr, other_g) = match other.mode(i) {
                    LinkMode::F(y) => (y, SfGuarded::Float),
                    LinkMode::Sf(y, guarded) => (y, guarded),
                    _ => unreachable!(),
                };
                #[cfg(feature = "profile")]
                join_profile::record_frame_sf(guarded, false, other_g, false);
                guarded.join(other_g);
                if x == other_fpr {
                    SetSf(x, guarded)
                } else {
                    TryFreshSfElseKeep(x, guarded)
                }
            }
            (LinkMode::Sf(x, mut guarded), LinkMode::C(r)) if r.is_float() || r.is_fixnum() => {
                #[cfg(feature = "profile")]
                join_profile::record_frame_sf(
                    guarded,
                    false,
                    SfGuarded::from_concrete_value(r),
                    true,
                );
                guarded.join(SfGuarded::from_concrete_value(r));
                SetSf(x, guarded)
            }
            (LinkMode::C(v), LinkMode::F(_)) if v.is_float() => TryFreshFElseS,
            (LinkMode::C(v), LinkMode::Sf(_, r)) if v.is_float() || v.is_fixnum() => {
                let mut guarded = SfGuarded::from_concrete_value(v);
                #[cfg(feature = "profile")]
                join_profile::record_frame_sf(guarded, true, r, false);
                guarded.join(r);
                TryFreshSfElseS(guarded)
            }
            (LinkMode::C(l), LinkMode::C(r)) if l == r => Nop,
            (LinkMode::C(l), LinkMode::C(r)) if l.is_float() && r.is_float() => TryFreshFElseS,
            _ => {
                #[cfg(feature = "profile")]
                join_profile::record_frame_generic(
                    &self.guarded(i),
                    matches!(self.mode(i), LinkMode::C(_)),
                    &other.guarded(i),
                    matches!(other.mode(i), LinkMode::C(_)),
                );
                SetS(self.guarded(i).join(&other.guarded(i)))
            }
        }
    }

    ///
    /// Apply a merge action to slot *i*. The **only** place [`Self::join`]
    /// mutates placement or allocates an fpr.
    ///
    fn apply_join(&mut self, i: SlotId, action: JoinAction) {
        match action {
            JoinAction::Nop => {}
            JoinAction::SetMaybeNone => self.set_MaybeNone(i),
            JoinAction::Discard => self.discard(i),
            JoinAction::TryFreshFKeep => {
                // No AsmIr here — if a Phase-2 spill would be needed,
                // fall back to keeping `F(l)` and let the bridge swap.
                let _ = self.try_set_new_F(i);
            }
            JoinAction::TryFreshFElseS => {
                if self.try_set_new_F(i).is_none() {
                    // Fall back to S — bridge materialises from the concrete
                    // literal on the C side and from fpr on the F side.
                    self.set_S_with_guard(i, Guarded::Float);
                }
            }
            JoinAction::SetSf(x, guarded) => self.set_Sf(i, x, guarded),
            JoinAction::TryFreshSfElseKeep(x, guarded) => {
                if self.try_set_new_Sf(i, guarded).is_none() {
                    self.set_Sf(i, x, guarded);
                }
            }
            JoinAction::TryFreshSfElseS(guarded) => {
                if self.try_set_new_Sf(i, guarded).is_none() {
                    self.set_S_with_guard(i, guarded.into());
                }
            }
            JoinAction::SetS(guarded) => self.set_S_with_guard(i, guarded),
            JoinAction::SetF(x) => {
                // The fpr may be bound only on the *other* side (a spill
                // id this branch never allocated) — grow the file so the
                // rebind resolves; the reverse map stays exact.
                self.grow_fpr_to(x.0 + 1);
                self.set_F(i, x);
            }
        }
    }
}

impl SfGuarded {
    fn join(&mut self, other: SfGuarded) {
        *self = match (*self, other) {
            (SfGuarded::Fixnum, SfGuarded::Fixnum) => SfGuarded::Fixnum,
            (SfGuarded::Float, SfGuarded::Float) => SfGuarded::Float,
            _ => SfGuarded::FixnumOrFloat,
        }
    }

    fn from_concrete_value(v: Value) -> Self {
        if v.is_fixnum() {
            SfGuarded::Fixnum
        } else if v.is_float() {
            SfGuarded::Float
        } else {
            panic!("SfGuarded::from_concrete_value(): not fixnum/float {:?}", v);
        }
    }
}

impl IsUsed {
    fn join(&mut self, other: &Self) {
        *self = match (&self, other) {
            (IsUsed::Used(l), IsUsed::Used(r)) => IsUsed::Used(l.join(r)),
            (IsUsed::Used(x), _) | (_, IsUsed::Used(x)) => IsUsed::Used(*x),
            (IsUsed::Killed, IsUsed::Killed) => IsUsed::Killed,
            _ => IsUsed::ND,
        };
    }
}

///
/// The merge at the granularity the back-edge fixpoint's convergence test
/// sees it (`doc/fixpoint_engine_plan.md` step E): the type lattice has its
/// laws (`guarded_join_lattice_laws`), and these are the same laws one
/// level up, over `LinkMode` — the fused join with its placement, judged
/// by `LinkMode::equiv`, which ignores placement.
///
#[cfg(test)]
mod link_mode_lattice {
    use super::*;

    /// The population: constants, `S` of every `Guarded` shape, `F` and
    /// `Sf` in two registers (a merge of different registers takes other
    /// arms than one of the same register), and the sentinels.
    fn population() -> Vec<LinkMode> {
        let foo = ClassId::new(100);
        let mut modes = vec![
            LinkMode::C(Value::i32(1)),
            LinkMode::C(Value::float(1.0)),
            LinkMode::C(Value::nil()),
            LinkMode::S(Guarded::Value),
            LinkMode::S(Guarded::Fixnum),
            LinkMode::S(Guarded::Float),
            LinkMode::S(Guarded::Class(NIL_CLASS)),
            LinkMode::S(Guarded::Class(foo)),
            LinkMode::S(Guarded::NilOr(NonNil::Fixnum)),
            LinkMode::S(Guarded::NilOr(NonNil::Float)),
            LinkMode::S(Guarded::NilOr(NonNil::Class(foo))),
        ];
        for fpr in [FPReg(0), FPReg(1)] {
            modes.push(LinkMode::F(fpr));
            modes.push(LinkMode::Sf(fpr, SfGuarded::Fixnum));
            modes.push(LinkMode::Sf(fpr, SfGuarded::Float));
            modes.push(LinkMode::Sf(fpr, SfGuarded::FixnumOrFloat));
        }
        modes
    }

    fn sentinels() -> [LinkMode; 3] {
        [LinkMode::None, LinkMode::MaybeNone, LinkMode::V]
    }

    /// A frame with `self` in slot 0 and *mode* in slot 1.
    fn frame(mode: LinkMode) -> AbstractFrame {
        AbstractFrame::for_test(&[LinkMode::S(Guarded::Value), mode])
    }

    /// `a ⊔ b` as the innermost frame of a merge does it, or `None` when
    /// the merge has no arm for the pair (it panics).
    fn join(a: LinkMode, b: LinkMode) -> Option<LinkMode> {
        let hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(|_| {}));
        let res = std::panic::catch_unwind(|| {
            let mut lhs = frame(a);
            lhs.join_with(&frame(b), true);
            lhs.mode(SlotId(1))
        });
        std::panic::set_hook(hook);
        res.ok()
    }

    fn equiv(a: LinkMode, b: LinkMode) -> bool {
        frame(a).equiv(&frame(b))
    }

    /// Every pair the merge has no arm for. Each one pairs `None` (an
    /// optional parameter's slot before it is known whether a value was
    /// passed) with a slot that has a type: no merge meets them, because
    /// both sides of a merge come from the same parameter binding — a
    /// slot is `None` on every path or on none, and a path that may or
    /// may not have bound it is `MaybeNone` (which meets anything).
    #[test]
    fn unjoinable_pairs() {
        let all: Vec<LinkMode> = population().into_iter().chain(sentinels()).collect();
        let mut panics = vec![];
        for &a in &all {
            for &b in &all {
                if join(a, b).is_none() {
                    panics.push((a, b));
                }
            }
        }
        for &(a, b) in &panics {
            assert!(
                (a == LinkMode::None) != (b == LinkMode::None)
                    && !matches!(a, LinkMode::MaybeNone | LinkMode::V)
                    && !matches!(b, LinkMode::MaybeNone | LinkMode::V),
                "unexpected unjoinable pair {a:?} / {b:?}"
            );
        }
        let typed = population().len();
        assert_eq!(panics.len(), 2 * typed, "{panics:?}");
    }

    /// The laws, over every pair (and triple) the merge has an arm for.
    #[test]
    fn join_laws_up_to_equiv() {
        let all: Vec<LinkMode> = population().into_iter().chain(sentinels()).collect();
        let j = |a, b| join(a, b);
        for &a in &all {
            // 1. idempotent
            if let Some(aa) = j(a, a) {
                assert!(equiv(aa, a), "idempotence: {a:?} -> {aa:?}");
            }
            for &b in &all {
                let (Some(ab), Some(ba)) = (j(a, b), j(b, a)) else {
                    continue;
                };
                // 2. commutative (placement may differ)
                assert!(equiv(ab, ba), "commutativity: {a:?} {b:?}: {ab:?} / {ba:?}");
                // 3. an upper bound of both
                for x in [a, b] {
                    if let Some(abx) = j(ab, x) {
                        assert!(
                            equiv(abx, ab),
                            "upper bound: {a:?} {b:?} / {x:?}: {ab:?} -> {abx:?}"
                        );
                    }
                }
                // 4. equiv is a congruence for the join
                if equiv(a, b) {
                    for &c in &all {
                        if let (Some(ac), Some(bc)) = (j(a, c), j(b, c)) {
                            assert!(
                                equiv(ac, bc),
                                "congruence: {a:?} ~ {b:?}, with {c:?}: {ac:?} / {bc:?}"
                            );
                        }
                    }
                }
            }
        }
    }
}
