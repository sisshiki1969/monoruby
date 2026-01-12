use super::*;

#[derive(Clone, Default)]
pub(in crate::codegen::jitgen) struct Liveness(Vec<IsUsed>);

impl std::fmt::Debug for Liveness {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Liveness {{ ")?;
        for (i, is_used) in self.0.iter().enumerate() {
            match is_used {
                IsUsed::Used(UsedAs { ty, .. }) => {
                    write!(f, "[{:?}: UsedAs {:?}] ", SlotId(i as u16), ty)?
                }
                IsUsed::Killed => write!(f, "[{:?}: Killed] ", SlotId(i as u16))?,
                IsUsed::ND => {}
            }
        }
        write!(f, "}}")
    }
}

impl Liveness {
    pub(in crate::codegen::jitgen) fn new(total_reg_num: usize) -> Self {
        Self(vec![IsUsed::default(); total_reg_num])
    }

    ///
    /// Collect killed (and not used) slots.
    ///
    pub(super) fn killed(&self) -> impl Iterator<Item = SlotId> {
        self.0.iter().enumerate().filter_map(|(i, is_used)| {
            if is_used == &IsUsed::Killed {
                Some(SlotId(i as u16))
            } else {
                None
            }
        })
    }

    ///
    /// Extract a set of registers which will be used as Float in this loop,
    /// *and* xmm-linked on the back-edge.
    ///
    pub(super) fn loop_used_as_float(&self) -> impl Iterator<Item = (SlotId, bool)> {
        self.0.iter().enumerate().flat_map(|(i, b)| match b {
            IsUsed::Used(used) => match used.ty {
                UseTy::Float => Some((SlotId(i as u16), true)),
                UseTy::Both => Some((SlotId(i as u16), false)),
                _ => None,
            },
            _ => None,
        })
    }

    pub(super) fn enumerate(&mut self) -> impl Iterator<Item = (usize, &mut IsUsed)> {
        self.0.iter_mut().enumerate()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub(super) enum IsUsed {
    ///
    /// Not be used nor be killed.
    ///
    #[default]
    ND,
    ///
    /// Used in any of paths.
    ///
    Used(UsedAs),
    ///
    /// Guaranteed not to be used (= killed) in all paths.
    ///
    Killed,
}

impl IsUsed {
    ///
    /// used as f64 with no conversion
    ///
    pub(super) fn use_as_float(&mut self) {
        match self {
            IsUsed::Killed => {}
            IsUsed::Used(used) => used.use_as_float(),
            IsUsed::ND => *self = IsUsed::Used(UsedAs::float()),
        }
    }

    pub(super) fn use_as_non_float(&mut self) {
        match self {
            IsUsed::Killed => {}
            IsUsed::Used(used) => used.use_as_non_float(),
            IsUsed::ND => *self = IsUsed::Used(UsedAs::non_float()),
        }
    }

    pub(super) fn kill(&mut self) {
        match self {
            IsUsed::Killed => {}
            IsUsed::Used(used) => used.kill(),
            IsUsed::ND => *self = IsUsed::Killed,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) struct UsedAs {
    ty: UseTy,
    killed: bool,
}

impl UsedAs {
    pub(super) fn join(&self, other: &Self) -> Self {
        Self {
            ty: self.ty.join(&other.ty),
            killed: self.killed && other.killed,
        }
    }

    fn float() -> Self {
        UsedAs {
            ty: UseTy::Float,
            killed: false,
        }
    }

    fn non_float() -> Self {
        UsedAs {
            ty: UseTy::NonFloat,
            killed: false,
        }
    }

    /// used as f64 with no conversion
    fn use_as_float(&mut self) {
        self.use_as(UseTy::Float);
    }

    fn use_as_non_float(&mut self) {
        self.use_as(UseTy::NonFloat);
    }

    fn use_as(&mut self, other: UseTy) {
        if self.killed {
            return;
        } else {
            self.ty = self.ty.join(&other);
        }
    }

    fn kill(&mut self) {
        self.killed = true;
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum UseTy {
    /// The slot is used as f64 with no conversion.
    Float,
    NonFloat,
    Both,
}

impl UseTy {
    fn join(&self, other: &Self) -> Self {
        match (self, other) {
            (UseTy::Float, UseTy::Float) => UseTy::Float,
            (UseTy::NonFloat, UseTy::NonFloat) => UseTy::NonFloat,
            (_, _) => UseTy::Both,
        }
    }
}
