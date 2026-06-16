use super::*;

impl AbstractState {
    ///
    /// Join abstract states.
    ///
    pub(in crate::codegen::jitgen) fn join(&mut self, other: &AbstractState) {
        for (lhs, rhs) in self.frames.iter_mut().zip(other.frames.iter()) {
            lhs.join(rhs);
        }
    }
}

impl Liveness {
    pub(in crate::codegen::jitgen) fn join(&mut self, state: &AbstractState) {
        for (i, is_used) in self.enumerate() {
            is_used.join(state.is_used(SlotId(i as u16)));
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
    fn join(&mut self, other: &AbstractFrame) {
        self.invariants.join(&other.invariants);
        for i in self.all_regs() {
            self.is_used_mut(i).join(other.is_used(i));
            // De-fuse the meet (§5): `decide_join` is a pure read-only function
            // of the two input `LinkMode`s (the merge *decision*); `apply_join`
            // performs the placement mutation — and is the *only* place the meet
            // allocates an xmm (`try_set_new_F` / `try_set_new_Sf`). This is the
            // seam the allocator pass will own: it will consume the `JoinAction`
            // stream and assign registers + emit edge moves, instead of
            // `apply_join` allocating inline. Behaviour is identical to the old
            // fused per-slot match.
            let action = self.decide_join(other, i);
            self.apply_join(i, action);
        }
    }
}

///
/// The per-slot merge decision (§5 de-fusion): a pure function of the two
/// predecessors' `LinkMode`s, computed by [`AbstractFrame::decide_join`] and
/// executed by [`AbstractFrame::apply_join`]. Reifying the decision separates
/// the *meet* (analysis) from the *placement mutation + xmm allocation*
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
    /// registers disagree across branches: try to rebind to a fresh xmm so each
    /// bridge is a single move; keep the current `F` binding if no phys xmm is
    /// free (the bridge then swaps). [`F`/`F` arm]
    TryFreshFKeep,
    /// try fresh-xmm `F`; fall back to `S(Float)` if no phys xmm is free.
    /// [`C`/`F` and `C`/`C`-both-float arms]
    TryFreshFElseS,
    /// rebind to `Sf(x, guarded)` with the current xmm `x` (registers agree, or
    /// the `Sf`/`C` arm folding a literal into the guard).
    SetSf(FPReg, SfGuarded),
    /// try fresh-xmm `Sf(guarded)`; keep `Sf(x, guarded)` if no phys xmm is free.
    /// [`F`|`Sf` / `Sf`|`F` arm, registers disagree]
    TryFreshSfElseKeep(FPReg, SfGuarded),
    /// try fresh-xmm `Sf(guarded)`; fall back to `S(guarded)` if no phys xmm.
    /// [`C` / `Sf` arm]
    TryFreshSfElseS(SfGuarded),
    /// `_ -> S(guarded)`
    SetS(Guarded),
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
                let (other_xmm, other_g) = match other.mode(i) {
                    LinkMode::F(y) => (y, SfGuarded::Float),
                    LinkMode::Sf(y, guarded) => (y, guarded),
                    _ => unreachable!(),
                };
                guarded.join(other_g);
                if x == other_xmm {
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
    /// mutates placement or allocates an xmm.
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
                    // literal on the C side and from xmm on the F side.
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
