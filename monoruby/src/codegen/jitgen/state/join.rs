use super::*;

impl AbstractState {
    ///
    /// Join abstract states.
    ///
    pub(in crate::codegen::jitgen) fn join(&mut self, other: &AbstractState) {
        let innermost = self.frames.len() - 1;
        for (level, (lhs, rhs)) in self.frames.iter_mut().zip(other.frames.iter()).enumerate() {
            // Only the innermost frame owns fp registers today: a call that
            // hands out a block spills every live one, so an outer frame
            // holds nothing unboxed and the merge must not hand it one
            // either — the two frames' fpr allocators are separate, and an
            // outer bridge has no way to load a slot into an fpr. Lifting
            // this is what carrying `F`/`Sf` across the boundary will need.
            lhs.join_with(rhs, level == innermost);
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
            if !allow_fpr {
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
                guarded.join(other_g);
                if x == other_fpr {
                    SetSf(x, guarded)
                } else {
                    TryFreshSfElseKeep(x, guarded)
                }
            }
            (LinkMode::Sf(x, mut guarded), LinkMode::C(r)) if r.is_float() || r.is_fixnum() => {
                guarded.join(SfGuarded::from_concrete_value(r));
                SetSf(x, guarded)
            }
            (LinkMode::C(v), LinkMode::F(_)) if v.is_float() => TryFreshFElseS,
            (LinkMode::C(v), LinkMode::Sf(_, r)) if v.is_float() || v.is_fixnum() => {
                let mut guarded = SfGuarded::from_concrete_value(v);
                guarded.join(r);
                TryFreshSfElseS(guarded)
            }
            (LinkMode::C(l), LinkMode::C(r)) if l == r => Nop,
            (LinkMode::C(l), LinkMode::C(r)) if l.is_float() && r.is_float() => TryFreshFElseS,
            _ => SetS(self.guarded(i).join(&other.guarded(i))),
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
