use num::BigInt;

///
/// The payload of an `ObjTy::BIGNUM` object: an Integer outside the fixnum
/// range.
///
/// Almost every Bignum a program makes is a 64-bit quantity — a mask, a
/// hash, a bitboard, an xorshift state — so the value is kept **inline as
/// an `i128`** while it fits one, and a `num::BigInt` (sign + a heap `Vec`
/// of digits) is only built for a value that does not. The arithmetic,
/// comparison and bit operators compute on the `i128` (`Value::as_i128`)
/// and never ask for a `BigInt`. A consumer that does ask for one
/// (`RValue::as_bignum`: `to_s`, `hash`, `Marshal`, …) converts the object
/// to the `Big` form in place, once; such an object still answers
/// `as_i128` (through `i128::try_from`), so it keeps the fast paths, and
/// the `BigInt` paths are correct for any value in any case.
///
/// The fixnum range is excluded by construction (`Value::from_i128` and
/// `Value::bigint` normalize), so a `Big` never denotes an `i128`-sized
/// value *unless* it was converted from `Inline`; the inline halves are
/// split into two 8-byte words so the payload keeps the cell's 8-byte
/// alignment (an `i128` would force 16).
///
/// The layout is fixed (`repr(C, u64)`) because JIT code reads an inline
/// value straight out of the cell: the discriminant word at
/// `RVALUE_OFFSET_KIND` (`Inline` = [`BignumInner::INLINE_TAG`]), then
/// `lo` at [`BignumInner::LO_OFFSET`] and `hi` at [`BignumInner::HI_OFFSET`]
/// from the payload (see `AsmInst::U64Unbox`).
///
#[derive(Clone)]
#[repr(C, u64)]
pub(crate) enum BignumInner {
    Inline { lo: u64, hi: i64 } = 0,
    Big(BigInt) = 1,
}

const _: () = assert!(std::mem::size_of::<BignumInner>() <= 48);
const _: () = assert!(std::mem::align_of::<BignumInner>() <= 8);

impl BignumInner {
    /// The discriminant word of an `Inline` payload.
    pub(crate) const INLINE_TAG: u64 = 0;
    /// Byte offset of `Inline::lo` from the start of the payload.
    pub(crate) const LO_OFFSET: usize = 8;
    /// Byte offset of `Inline::hi` from the start of the payload.
    pub(crate) const HI_OFFSET: usize = 16;
}

impl BignumInner {
    /// *i* must be outside the fixnum range (the caller normalizes).
    pub(crate) fn from_i128(i: i128) -> Self {
        debug_assert!(!crate::Value::is_i63_i128(i));
        Self::Inline {
            lo: i as u64,
            hi: (i >> 64) as i64,
        }
    }

    /// *b* must be outside the fixnum range (the caller normalizes).
    pub(crate) fn from_bigint(b: BigInt) -> Self {
        match i128::try_from(&b) {
            Ok(i) => Self::from_i128(i),
            Err(_) => Self::Big(b),
        }
    }

    /// The value as an `i128`, when it fits one. A `Big` that fits is one
    /// a `BigInt` consumer converted (see `materialize`); it still takes
    /// the `i128` paths, at the cost of the two-digit check here.
    #[inline]
    pub(crate) fn as_i128(&self) -> Option<i128> {
        match self {
            Self::Inline { lo, hi } => Some(((*hi as i128) << 64) | (*lo as u64 as i128)),
            Self::Big(b) => i128::try_from(b).ok(),
        }
    }

    /// The value as a `BigInt`, converting an inline value in place.
    pub(crate) fn materialize(&mut self) -> &BigInt {
        if let Self::Inline { .. } = self {
            *self = Self::Big(BigInt::from(self.as_i128().unwrap()));
        }
        match self {
            Self::Big(b) => b,
            Self::Inline { .. } => unreachable!(),
        }
    }
}

impl std::fmt::Debug for BignumInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Inline { .. } => write!(f, "{}", self.as_i128().unwrap()),
            Self::Big(b) => write!(f, "{}", b),
        }
    }
}

impl PartialEq for BignumInner {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Inline { .. }, Self::Inline { .. }) => self.as_i128() == other.as_i128(),
            (Self::Big(a), Self::Big(b)) => a == b,
            (Self::Inline { .. }, Self::Big(b)) => i128::try_from(b).ok() == self.as_i128(),
            (Self::Big(a), Self::Inline { .. }) => i128::try_from(a).ok() == other.as_i128(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The JIT's `U64Unbox` reads the three words at fixed offsets.
    #[test]
    fn inline_layout() {
        let v = BignumInner::Inline {
            lo: 0x1122_3344_5566_7788,
            hi: -2,
        };
        let p = &v as *const BignumInner as *const u8;
        // SAFETY: the reads stay inside the enum (size ≥ 24 is asserted
        // by the offsets below being within `size_of`).
        unsafe {
            assert!(BignumInner::HI_OFFSET + 8 <= std::mem::size_of::<BignumInner>());
            assert_eq!(p.cast::<u64>().read(), BignumInner::INLINE_TAG);
            assert_eq!(p.add(BignumInner::LO_OFFSET).cast::<u64>().read(), 0x1122_3344_5566_7788);
            assert_eq!(p.add(BignumInner::HI_OFFSET).cast::<i64>().read(), -2);
        }
        let b = BignumInner::Big(BigInt::from(1) << 200);
        let p = &b as *const BignumInner as *const u64;
        // SAFETY: the discriminant word is the first word of the payload.
        unsafe { assert_ne!(p.read(), BignumInner::INLINE_TAG) };
    }
}
