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
            match (self.mode(i), other.mode(i)) {
                (LinkMode::None, LinkMode::None) => {}
                (LinkMode::MaybeNone, _) => {}
                (_, LinkMode::MaybeNone) => {
                    self.set_MaybeNone(i);
                }
                (LinkMode::V, _) => {}
                (_, LinkMode::V) => {
                    self.discard(i);
                }
                (LinkMode::F(_), LinkMode::F(_)) => {}
                (LinkMode::F(_), LinkMode::C(r)) if r.is_float() => {}
                (LinkMode::F(x), LinkMode::Sf(_, _))
                | (LinkMode::Sf(x, _), LinkMode::Sf(_, _) | LinkMode::F(_)) => {
                    let mut guarded = match self.mode(i) {
                        LinkMode::F(_) => SfGuarded::Float,
                        LinkMode::Sf(_, guarded) => guarded,
                        _ => unreachable!(),
                    };
                    let other = match other.mode(i) {
                        LinkMode::F(_) => SfGuarded::Float,
                        LinkMode::Sf(_, guarded) => guarded,
                        _ => unreachable!(),
                    };
                    guarded.join(other);
                    self.set_Sf(i, x, guarded);
                }
                (LinkMode::Sf(x, mut guarded), LinkMode::C(r)) if r.is_float() || r.is_fixnum() => {
                    guarded.join(SfGuarded::from_concrete_value(r));
                    self.set_Sf(i, x, guarded)
                }
                (LinkMode::C(v), LinkMode::F(_)) if v.is_float() => {
                    self.set_new_F(i);
                }
                (LinkMode::C(v), LinkMode::Sf(_, r)) if v.is_float() || v.is_fixnum() => {
                    let mut guarded = SfGuarded::from_concrete_value(v);
                    guarded.join(r);
                    self.set_new_Sf(i, guarded);
                }
                (LinkMode::C(l), LinkMode::C(r)) if l == r => {}
                (LinkMode::C(l), LinkMode::C(r)) if l.is_float() && r.is_float() => {
                    self.set_new_F(i);
                }
                _ => {
                    let guarded = self.guarded(i).join(&other.guarded(i));
                    self.set_S_with_guard(i, guarded);
                }
            };
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

impl Guarded {
    fn join(&self, other: &Self) -> Self {
        if self == other { *self } else { Guarded::Value }
    }
}
