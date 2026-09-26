use num::bigint::BigInt;
use num::traits::{One, Zero};
use num::{Signed, ToPrimitive};

use super::*;

///
/// A Ruby Integer held by a Rational: `Small` for anything that fits in an
/// `i64`, `Big` (boxed) otherwise.
///
/// The form is canonical — a value that fits in an `i64` is always `Small` —
/// so the derived equality and hash are value equality. `Small` is what keeps
/// an ordinary Rational (`Rational(3, 4)`, a `Time#subsec`) free of heap
/// allocations: only the RValue cell is allocated, not two `BigInt` digit
/// buffers. It is a Rust value rather than a Ruby `Value` (CRuby's choice)
/// because arithmetic has to unpack a `Value` into Rust integers anyway, and a
/// canonical Rust value gets equality and hashing by `derive` and needs no
/// marking; a `RationalInner` that is a temporary inside a builtin also carries
/// no unrooted bignum across a call back into Ruby.
///
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IntegerRepr {
    Small(i64),
    Big(Box<BigInt>),
}

const _: () = assert!(std::mem::size_of::<IntegerRepr>() == 16);

impl IntegerRepr {
    pub fn from_bigint(b: BigInt) -> Self {
        match i64::try_from(&b) {
            Ok(i) => IntegerRepr::Small(i),
            Err(_) => IntegerRepr::Big(Box::new(b)),
        }
    }

    fn from_i128(i: i128) -> Self {
        match i64::try_from(i) {
            Ok(i) => IntegerRepr::Small(i),
            Err(_) => IntegerRepr::Big(Box::new(BigInt::from(i))),
        }
    }

    pub fn to_bigint(&self) -> BigInt {
        match self {
            IntegerRepr::Small(i) => BigInt::from(*i),
            IntegerRepr::Big(b) => (**b).clone(),
        }
    }

    /// The value as an `i64`. `None` means it does not fit (the form is
    /// canonical, so a `Big` never does).
    pub fn to_i64(&self) -> Option<i64> {
        match self {
            IntegerRepr::Small(i) => Some(*i),
            IntegerRepr::Big(_) => None,
        }
    }

    pub fn to_value(&self) -> Value {
        match self {
            IntegerRepr::Small(i) => Value::integer(*i),
            IntegerRepr::Big(b) => Value::bigint((**b).clone()),
        }
    }

    pub fn is_zero(&self) -> bool {
        matches!(self, IntegerRepr::Small(0))
    }

    pub fn is_one(&self) -> bool {
        matches!(self, IntegerRepr::Small(1))
    }

    pub fn is_negative(&self) -> bool {
        match self {
            IntegerRepr::Small(i) => *i < 0,
            IntegerRepr::Big(b) => b.is_negative(),
        }
    }

    /// Whether this equals *b*, which need not be canonical (a bignum
    /// `Value` can hold an `i64` outside the fixnum range).
    pub fn eq_bigint(&self, b: &BigInt) -> bool {
        match self {
            IntegerRepr::Small(i) => i64::try_from(b) == Ok(*i),
            IntegerRepr::Big(x) => **x == *b,
        }
    }

    fn neg(&self) -> Self {
        match self {
            IntegerRepr::Small(i) => match i.checked_neg() {
                Some(n) => IntegerRepr::Small(n),
                None => IntegerRepr::Big(Box::new(-BigInt::from(*i))),
            },
            IntegerRepr::Big(b) => IntegerRepr::from_bigint(-(**b).clone()),
        }
    }
}

impl std::fmt::Display for IntegerRepr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntegerRepr::Small(i) => write!(f, "{i}"),
            IntegerRepr::Big(b) => write!(f, "{b}"),
        }
    }
}

impl From<i64> for IntegerRepr {
    fn from(i: i64) -> Self {
        IntegerRepr::Small(i)
    }
}

impl From<i32> for IntegerRepr {
    fn from(i: i32) -> Self {
        IntegerRepr::Small(i as i64)
    }
}

impl From<u32> for IntegerRepr {
    fn from(i: u32) -> Self {
        IntegerRepr::Small(i as i64)
    }
}

impl From<u64> for IntegerRepr {
    fn from(i: u64) -> Self {
        IntegerRepr::from_i128(i as i128)
    }
}

impl From<i128> for IntegerRepr {
    fn from(i: i128) -> Self {
        IntegerRepr::from_i128(i)
    }
}

impl From<BigInt> for IntegerRepr {
    fn from(b: BigInt) -> Self {
        IntegerRepr::from_bigint(b)
    }
}

impl From<&BigInt> for IntegerRepr {
    fn from(b: &BigInt) -> Self {
        match i64::try_from(b) {
            Ok(i) => IntegerRepr::Small(i),
            Err(_) => IntegerRepr::Big(Box::new(b.clone())),
        }
    }
}

/// Greatest common divisor (binary / Stein's algorithm).
pub(crate) fn gcd_u128(mut a: u128, mut b: u128) -> u128 {
    if a == 0 {
        return b;
    }
    if b == 0 {
        return a;
    }
    let shift = (a | b).trailing_zeros();
    a >>= a.trailing_zeros();
    loop {
        b >>= b.trailing_zeros();
        if a > b {
            std::mem::swap(&mut a, &mut b);
        }
        b -= a;
        if b == 0 {
            return a << shift;
        }
    }
}

pub enum RationalFloorResult {
    Integer(IntegerRepr),
    Rational(RationalInner),
}

///
/// A normalized Rational: `den > 0` and `gcd(num, den) == 1`.
///
/// Arithmetic takes an `i128` fast path whenever every operand is `Small`:
/// the product of two `i64`s and the sum of two such products both fit in an
/// `i128`, so nothing there can overflow, and the result is reduced with an
/// integer gcd before it is narrowed back.
///
#[derive(Clone, Debug, PartialEq)]
pub struct RationalInner {
    num: IntegerRepr,
    den: IntegerRepr,
}

// Stored inline in the RValue cell (`ObjKind::rational`), not boxed.
const _: () = assert!(std::mem::size_of::<RationalInner>() == 32);

impl GC<RValue> for RationalInner {
    fn mark(&self, _alloc: &mut Allocator<RValue>) {}
}

impl RationalInner {
    pub fn new(num: impl Into<IntegerRepr>, den: impl Into<IntegerRepr>) -> Self {
        match (num.into(), den.into()) {
            (IntegerRepr::Small(n), IntegerRepr::Small(d)) => Self::from_i128(n as i128, d as i128),
            (n, d) => Self::from_bigint(n.to_bigint(), d.to_bigint()),
        }
    }

    /// Normalize `n / d` computed on the `i128` fast path. Both operands come
    /// from sums and products of `i64`s, so neither is `i128::MIN` and the
    /// sign flip below cannot overflow.
    fn from_i128(mut n: i128, mut d: i128) -> Self {
        if d == 0 {
            panic!("divided by 0");
        }
        if d < 0 {
            n = -n;
            d = -d;
        }
        let g = gcd_u128(n.unsigned_abs(), d as u128);
        if g > 1 {
            n /= g as i128;
            d /= g as i128;
        }
        Self {
            num: IntegerRepr::from_i128(n),
            den: IntegerRepr::from_i128(d),
        }
    }

    fn from_bigint(mut num: BigInt, mut den: BigInt) -> Self {
        use num::integer::Integer;
        if den.is_zero() {
            panic!("divided by 0");
        }
        if den < BigInt::ZERO {
            num = -num;
            den = -den;
        }
        let g = num.gcd(&den);
        if !g.is_one() {
            num = &num / &g;
            den = &den / &g;
        }
        Self {
            num: IntegerRepr::from_bigint(num),
            den: IntegerRepr::from_bigint(den),
        }
    }

    /// All four components as `i64`s, when every one of them is `Small`.
    fn small_pair(&self, other: &Self) -> Option<(i128, i128, i128, i128)> {
        Some((
            self.num.to_i64()? as i128,
            self.den.to_i64()? as i128,
            other.num.to_i64()? as i128,
            other.den.to_i64()? as i128,
        ))
    }

    pub fn num(&self) -> &IntegerRepr {
        &self.num
    }

    pub fn den(&self) -> &IntegerRepr {
        &self.den
    }

    pub fn num_as_value(&self) -> Value {
        self.num.to_value()
    }

    pub fn den_as_value(&self) -> Value {
        self.den.to_value()
    }

    pub fn to_f(&self) -> f64 {
        // Both exactly representable: one correctly rounded division.
        const EXACT: u64 = 1 << 53;
        if let (Some(n), Some(d)) = (self.num.to_i64(), self.den.to_i64())
            && n.unsigned_abs() <= EXACT
            && (d as u64) <= EXACT
        {
            return n as f64 / d as f64;
        }
        Self::bigint_to_f(&self.num.to_bigint(), &self.den.to_bigint())
    }

    fn bigint_to_f(num: &BigInt, den: &BigInt) -> f64 {
        if num.is_zero() {
            return 0.0;
        }
        // Direct conversion when both fit safely in f64.
        let num_bits = num.bits() as i32;
        let den_bits = den.bits() as i32;
        if num_bits < 1020 && den_bits < 1020 {
            let n = num.to_f64().unwrap_or(f64::INFINITY);
            let d = den.to_f64().unwrap_or(f64::INFINITY);
            if n.is_finite() && d.is_finite() && d != 0.0 {
                return n / d;
            }
        }
        // Otherwise, keep the top ~60 bits of each to preserve precision,
        // then scale the quotient by 2^(num_shift - den_shift).
        let keep: i32 = 60;
        let num_shift = (num_bits - keep).max(0);
        let den_shift = (den_bits - keep).max(0);
        let num_reduced = num.clone() >> num_shift as usize;
        let den_reduced = den.clone() >> den_shift as usize;
        let sign = if num.is_negative() { -1.0 } else { 1.0 };
        let n = num_reduced.to_f64().unwrap_or(0.0).abs();
        let d = den_reduced.to_f64().unwrap_or(1.0);
        let exp_diff = num_shift - den_shift;
        sign * (n / d) * 2.0f64.powi(exp_diff)
    }

    /// Truncating division, as `Rational#to_i`.
    pub fn to_i(&self) -> IntegerRepr {
        match (self.num.to_i64(), self.den.to_i64()) {
            // `den > 0`, so `i64::MIN / -1` cannot occur.
            (Some(n), Some(d)) => IntegerRepr::Small(n / d),
            _ => IntegerRepr::from_bigint(self.num.to_bigint() / self.den.to_bigint()),
        }
    }

    pub fn is_zero(&self) -> bool {
        self.num.is_zero()
    }

    pub fn is_negative(&self) -> bool {
        self.num.is_negative()
    }

    pub fn neg(&self) -> Self {
        Self {
            num: self.num.neg(),
            den: self.den.clone(),
        }
    }

    pub fn abs(&self) -> Self {
        if self.num.is_negative() {
            self.neg()
        } else {
            self.clone()
        }
    }

    pub fn add(&self, other: &Self) -> Self {
        if let Some((a, b, c, d)) = self.small_pair(other) {
            return Self::from_i128(a * d + c * b, b * d);
        }
        let (a, b, c, d) = self.big_pair(other);
        Self::from_bigint(&a * &d + &c * &b, &b * &d)
    }

    pub fn sub(&self, other: &Self) -> Self {
        if let Some((a, b, c, d)) = self.small_pair(other) {
            return Self::from_i128(a * d - c * b, b * d);
        }
        let (a, b, c, d) = self.big_pair(other);
        Self::from_bigint(&a * &d - &c * &b, &b * &d)
    }

    pub fn mul(&self, other: &Self) -> Self {
        if let Some((a, b, c, d)) = self.small_pair(other) {
            return Self::from_i128(a * c, b * d);
        }
        let (a, b, c, d) = self.big_pair(other);
        Self::from_bigint(&a * &c, &b * &d)
    }

    pub fn div(&self, other: &Self) -> Result<Self> {
        if other.num.is_zero() {
            return Err(MonorubyErr::divide_by_zero());
        }
        if let Some((a, b, c, d)) = self.small_pair(other) {
            return Ok(Self::from_i128(a * d, b * c));
        }
        let (a, b, c, d) = self.big_pair(other);
        Ok(Self::from_bigint(&a * &d, &b * &c))
    }

    pub fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if let Some((a, b, c, d)) = self.small_pair(other) {
            return (a * d).cmp(&(c * b));
        }
        let (a, b, c, d) = self.big_pair(other);
        (&a * &d).cmp(&(&c * &b))
    }

    pub fn eq(&self, other: &Self) -> bool {
        self.num == other.num && self.den == other.den
    }

    fn big_pair(&self, other: &Self) -> (BigInt, BigInt, BigInt, BigInt) {
        (
            self.num.to_bigint(),
            self.den.to_bigint(),
            other.num.to_bigint(),
            other.den.to_bigint(),
        )
    }

    /// Check if ndigits is sufficient to represent this Rational exactly in decimal.
    fn ndigits_sufficient(&self, ndigits: i64) -> bool {
        if self.den.is_one() {
            return true;
        }
        let den = self.den.to_bigint();
        let mut t = den.clone();
        while (&t % 2u32).is_zero() {
            t /= 2u32;
        }
        while (&t % 5u32).is_zero() {
            t /= 5u32;
        }
        if !t.is_one() {
            return false;
        }
        // Terminating decimal: count max of factors of 2 and 5
        let (mut d2, mut d5) = (0i64, 0i64);
        let mut t = den;
        while (&t % 2u32).is_zero() {
            t /= 2u32;
            d2 += 1;
        }
        while (&t % 5u32).is_zero() {
            t /= 5u32;
            d5 += 1;
        }
        ndigits >= d2.max(d5)
    }

    /// The components as `i128`s, when both are `Small`.
    fn small(&self) -> Option<(i128, i128)> {
        Some((self.num.to_i64()? as i128, self.den.to_i64()? as i128))
    }

    /// Rational#floor(ndigits)
    /// ndigits == 0 => Integer, ndigits > 0 => Rational, ndigits < 0 => Integer
    pub fn rational_floor(&self, ndigits: i64) -> RationalFloorResult {
        if ndigits == 0
            && let Some((n, d)) = self.small()
        {
            // `d > 0`, so the Euclidean quotient is the floor.
            return RationalFloorResult::Integer(IntegerRepr::from_i128(n.div_euclid(d)));
        }
        let (num, den) = (self.num.to_bigint(), self.den.to_bigint());
        if ndigits == 0 {
            // Integer division rounded toward negative infinity
            RationalFloorResult::Integer(IntegerRepr::from_bigint(
                num::integer::Integer::div_floor(&num, &den),
            ))
        } else if ndigits > 0 {
            if self.ndigits_sufficient(ndigits) {
                return RationalFloorResult::Rational(self.clone());
            }
            let d = BigInt::from(10u32).pow(ndigits as u32);
            let scaled_num = &num * &d;
            let floored = num::integer::Integer::div_floor(&scaled_num, &den);
            RationalFloorResult::Rational(Self::new(floored, d))
        } else {
            let d = BigInt::from(10u32).pow((-ndigits) as u32);
            let i = &num / &den;
            RationalFloorResult::Integer(IntegerRepr::from_bigint(
                num::integer::Integer::div_floor(&i, &d) * &d,
            ))
        }
    }

    /// Rational#ceil(ndigits)
    pub fn rational_ceil(&self, ndigits: i64) -> RationalFloorResult {
        if ndigits == 0
            && let Some((n, d)) = self.small()
        {
            // ceil = -floor(-self); `-n` cannot overflow an i128.
            return RationalFloorResult::Integer(IntegerRepr::from_i128(-(-n).div_euclid(d)));
        }
        let (num, den) = (self.num.to_bigint(), self.den.to_bigint());
        if ndigits == 0 {
            // ceil = -floor(-self)
            let neg_num = -num;
            let floored = num::integer::Integer::div_floor(&neg_num, &den);
            RationalFloorResult::Integer(IntegerRepr::from_bigint(-floored))
        } else if ndigits > 0 {
            if self.ndigits_sufficient(ndigits) {
                return RationalFloorResult::Rational(self.clone());
            }
            let d = BigInt::from(10u32).pow(ndigits as u32);
            let scaled_num = &num * &d;
            let neg_scaled = -&scaled_num;
            let ceiled = -num::integer::Integer::div_floor(&neg_scaled, &den);
            RationalFloorResult::Rational(Self::new(ceiled, d))
        } else {
            let d = BigInt::from(10u32).pow((-ndigits) as u32);
            let i = &num / &den;
            // ceil = -floor(-i, d)
            let neg_i = -&i;
            let floored = num::integer::Integer::div_floor(&neg_i, &d);
            RationalFloorResult::Integer(IntegerRepr::from_bigint(-floored * &d))
        }
    }

    /// Rational#truncate(ndigits)
    pub fn rational_truncate(&self, ndigits: i64) -> RationalFloorResult {
        if ndigits == 0 {
            return RationalFloorResult::Integer(self.to_i());
        }
        let (num, den) = (self.num.to_bigint(), self.den.to_bigint());
        if ndigits > 0 {
            if self.ndigits_sufficient(ndigits) {
                return RationalFloorResult::Rational(self.clone());
            }
            let d = BigInt::from(10u32).pow(ndigits as u32);
            let scaled_num = &num * &d;
            let truncated = &scaled_num / &den;
            RationalFloorResult::Rational(Self::new(truncated, d))
        } else {
            let d = BigInt::from(10u32).pow((-ndigits) as u32);
            let i = &num / &den;
            RationalFloorResult::Integer(IntegerRepr::from_bigint((&i / &d) * &d))
        }
    }

    /// `Rational#round` to an integer on the `i128` fast path: the same
    /// half-way rules as the BigInt path below.
    fn round_small(n: i128, d: i128, half: Option<crate::value::RoundHalf>) -> i128 {
        if d == 1 {
            return n;
        }
        // Truncating quotient and remainder, as BigInt's `div_rem`.
        let (q, r) = (n / d, n % d);
        let abs_doubled = (r * 2).abs();
        if abs_doubled > d {
            if n >= 0 { q + 1 } else { q - 1 }
        } else if abs_doubled < d {
            q
        } else {
            match half {
                Some(crate::value::RoundHalf::Down) => q,
                Some(crate::value::RoundHalf::Even) => {
                    if q % 2 == 0 {
                        q
                    } else if n >= 0 {
                        q + 1
                    } else {
                        q - 1
                    }
                }
                _ => {
                    if n >= 0 {
                        q + 1
                    } else {
                        q - 1
                    }
                }
            }
        }
    }

    /// Rational#round(ndigits, half:)
    /// half: None => :up (default), Some("up"), Some("down"), Some("even")
    pub fn rational_round(
        &self,
        ndigits: i64,
        half: Option<crate::value::RoundHalf>,
    ) -> RationalFloorResult {
        if ndigits == 0
            && let Some((n, d)) = self.small()
        {
            return RationalFloorResult::Integer(IntegerRepr::from_i128(Self::round_small(
                n, d, half,
            )));
        }
        let (num, den) = (self.num.to_bigint(), self.den.to_bigint());
        if ndigits == 0 {
            if den.is_one() {
                return RationalFloorResult::Integer(self.num.clone());
            }
            let (q, r) = num::integer::Integer::div_rem(&num, &den);
            let doubled = &r * BigInt::from(2);
            let abs_doubled = doubled.abs();
            let abs_den = den.abs();
            let result = if abs_doubled > abs_den {
                if num >= BigInt::ZERO { q + 1 } else { q - 1 }
            } else if abs_doubled < abs_den {
                q
            } else {
                // Exactly half
                match half {
                    Some(crate::value::RoundHalf::Down) => q,
                    Some(crate::value::RoundHalf::Even) => {
                        if (&q % 2u32).is_zero() {
                            q
                        } else if num >= BigInt::ZERO {
                            q + 1
                        } else {
                            q - 1
                        }
                    }
                    _ => {
                        // :up or default
                        if num >= BigInt::ZERO { q + 1 } else { q - 1 }
                    }
                }
            };
            RationalFloorResult::Integer(IntegerRepr::from_bigint(result))
        } else if ndigits > 0 {
            if self.ndigits_sufficient(ndigits) {
                return RationalFloorResult::Rational(self.clone());
            }
            let d = BigInt::from(10u32).pow(ndigits as u32);
            let scaled = Self::new(&num * &d, den);
            if let RationalFloorResult::Integer(rounded) = scaled.rational_round(0, half) {
                RationalFloorResult::Rational(Self::new(rounded, d))
            } else {
                unreachable!()
            }
        } else {
            let d = BigInt::from(10u32).pow((-ndigits) as u32);
            let i = &num / &den;
            // Round the integer at the given digit position
            let (q, r) = num::integer::Integer::div_rem(&i, &d);
            let doubled = &r * BigInt::from(2);
            let abs_doubled = doubled.abs();
            let result = if abs_doubled > d.abs() {
                if i >= BigInt::ZERO {
                    (q + 1) * &d
                } else {
                    (q - 1) * &d
                }
            } else if abs_doubled < d.abs() {
                q * &d
            } else {
                match half {
                    Some(crate::value::RoundHalf::Down) => q * &d,
                    Some(crate::value::RoundHalf::Even) => {
                        if (&q % 2u32).is_zero() {
                            q * &d
                        } else if i >= BigInt::ZERO {
                            (q + 1) * &d
                        } else {
                            (q - 1) * &d
                        }
                    }
                    _ => {
                        if i >= BigInt::ZERO {
                            (q + 1) * &d
                        } else {
                            (q - 1) * &d
                        }
                    }
                }
            };
            RationalFloorResult::Integer(IntegerRepr::from_bigint(result))
        }
    }

    /// Convert f64 to exact Rational representation.
    ///
    /// Uses repeated doubling to find the exact binary fraction.
    /// Panics on NaN or Infinity — caller must check before calling.
    pub fn from_f64(f: f64) -> Self {
        debug_assert!(!f.is_nan() && !f.is_infinite());
        if f == 0.0 {
            return Self::new(0, 1);
        }
        let negative = f < 0.0;
        let mut x = if negative { -f } else { f };
        let mut n: u32 = 0;
        while x != x.floor() {
            x *= 2.0;
            n += 1;
            if n > 1074 {
                break;
            }
        }
        let mut num = BigInt::from_f64(x).unwrap_or_else(|| BigInt::from(x as i64));
        let den = BigInt::from(1u64) << n;
        if negative {
            num = -num;
        }
        Self::new(num, den)
    }

    /// Stern-Brocot search: find the simplest rational within [value - eps, value + eps].
    ///
    /// Both `value` and `eps` are RationalInner. `eps` must be non-negative.
    pub fn find_simplest(value: &Self, eps: &Self) -> Self {
        let eps_abs = eps.abs();
        // Handle negative: negate, search, negate back
        if value.is_negative() {
            let pos = value.neg();
            let result = Self::find_simplest(&pos, &eps_abs);
            return result.neg();
        }
        let lo = value.sub(&eps_abs);
        let hi = value.add(&eps_abs);
        let zero = Self::new(0, 1);
        // If range includes zero or is entirely negative, return 0
        if hi.cmp(&zero) == std::cmp::Ordering::Less {
            return zero;
        }
        let lo = if lo.cmp(&zero) == std::cmp::Ordering::Less {
            zero
        } else {
            lo
        };
        let (lo_num, lo_den) = (lo.num.to_bigint(), lo.den.to_bigint());
        let (hi_num, hi_den) = (hi.num.to_bigint(), hi.den.to_bigint());
        let mut p0 = BigInt::from(0);
        let mut q0 = BigInt::from(1);
        let mut p1 = BigInt::from(1);
        let mut q1 = BigInt::from(0);
        loop {
            let pm = &p0 + &p1;
            let qm = &q0 + &q1;
            let med = Self::new(pm.clone(), qm.clone());
            match med.cmp(&lo) {
                std::cmp::Ordering::Less => {
                    // med < lo: advance left bound
                    let top = &lo_num * &qm - &pm * &lo_den;
                    let bot = &p1 * &lo_den - &lo_num * &q1;
                    let k = div_ceil_bigint(&top, &bot);
                    p0 = &p0 + &k * &p1;
                    q0 = &q0 + &k * &q1;
                }
                std::cmp::Ordering::Greater if med.cmp(&hi) == std::cmp::Ordering::Greater => {
                    // med > hi: advance right bound
                    let top = &pm * &hi_den - &hi_num * &qm;
                    let bot = &hi_num * &q0 - &p0 * &hi_den;
                    let k = div_ceil_bigint(&top, &bot);
                    p1 = &p1 + &k * &p0;
                    q1 = &q1 + &k * &q0;
                }
                _ => {
                    // lo <= med <= hi: found it
                    return Self::new(pm, qm);
                }
            }
        }
    }

    pub fn to_s(&self) -> String {
        format!("{}/{}", self.num, self.den)
    }

    pub fn inspect(&self) -> String {
        format!("({}/{})", self.num, self.den)
    }
}

/// Ceiling division for BigInt: ceil(a / b) assuming b > 0.
fn div_ceil_bigint(a: &BigInt, b: &BigInt) -> BigInt {
    use num::integer::Integer;
    if b.is_zero() {
        return BigInt::from(1);
    }
    let (q, r) = a.div_rem(b);
    if r.is_zero() || (r.is_negative() && b.is_negative()) || (r.is_positive() && b.is_positive()) {
        if r.is_zero() { q } else { q + 1 }
    } else {
        q
    }
}
