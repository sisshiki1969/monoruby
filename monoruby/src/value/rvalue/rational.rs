use num::bigint::BigInt;
use num::traits::{One, Zero};
use num::{Signed, ToPrimitive};

use super::*;

pub enum RationalFloorResult {
    Integer(BigInt),
    Rational(RationalInner),
}

#[derive(Clone, Debug, PartialEq)]
pub struct RationalInner {
    num: BigInt,
    den: BigInt,
}

impl GC<RValue> for RationalInner {
    fn mark(&self, _alloc: &mut Allocator<RValue>) {}
}

impl RationalInner {
    pub fn new(num: i64, den: i64) -> Self {
        let mut n = BigInt::from(num);
        let mut d = BigInt::from(den);
        Self::normalize(&mut n, &mut d);
        Self { num: n, den: d }
    }

    pub fn new_bigint(num: BigInt, den: BigInt) -> Self {
        let mut n = num;
        let mut d = den;
        Self::normalize(&mut n, &mut d);
        Self { num: n, den: d }
    }

    fn normalize(num: &mut BigInt, den: &mut BigInt) {
        use num::integer::Integer;
        if den.is_zero() {
            panic!("divided by 0");
        }
        if *den < BigInt::ZERO {
            *num = -num.clone();
            *den = -den.clone();
        }
        let g = num.gcd(den);
        if !g.is_one() {
            *num = &*num / &g;
            *den = &*den / &g;
        }
    }

    pub fn num(&self) -> &BigInt {
        &self.num
    }

    pub fn den(&self) -> &BigInt {
        &self.den
    }

    pub fn num_as_value(&self) -> Value {
        if let Ok(n) = i64::try_from(&self.num) {
            Value::integer(n)
        } else {
            Value::bigint(self.num.clone())
        }
    }

    pub fn den_as_value(&self) -> Value {
        if let Ok(d) = i64::try_from(&self.den) {
            Value::integer(d)
        } else {
            Value::bigint(self.den.clone())
        }
    }

    pub fn to_f(&self) -> f64 {
        let n = self.num.to_f64().unwrap_or(if self.num.is_positive() {
            f64::INFINITY
        } else {
            f64::NEG_INFINITY
        });
        let d = self.den.to_f64().unwrap_or(if self.den.is_positive() {
            f64::INFINITY
        } else {
            f64::NEG_INFINITY
        });
        n / d
    }

    pub fn to_i(&self) -> BigInt {
        &self.num / &self.den
    }

    pub fn is_zero(&self) -> bool {
        self.num.is_zero()
    }

    pub fn is_negative(&self) -> bool {
        self.num < BigInt::ZERO
    }

    pub fn neg(&self) -> Self {
        Self {
            num: -self.num.clone(),
            den: self.den.clone(),
        }
    }

    pub fn abs(&self) -> Self {
        Self {
            num: if self.num < BigInt::ZERO {
                -self.num.clone()
            } else {
                self.num.clone()
            },
            den: self.den.clone(),
        }
    }

    pub fn add(&self, other: &Self) -> Self {
        let n = &self.num * &other.den + &other.num * &self.den;
        let d = &self.den * &other.den;
        Self::new_bigint(n, d)
    }

    pub fn sub(&self, other: &Self) -> Self {
        let n = &self.num * &other.den - &other.num * &self.den;
        let d = &self.den * &other.den;
        Self::new_bigint(n, d)
    }

    pub fn mul(&self, other: &Self) -> Self {
        let n = &self.num * &other.num;
        let d = &self.den * &other.den;
        Self::new_bigint(n, d)
    }

    pub fn div(&self, other: &Self) -> Result<Self> {
        if other.num.is_zero() {
            return Err(MonorubyErr::divide_by_zero());
        }
        let n = &self.num * &other.den;
        let d = &self.den * &other.num;
        Ok(Self::new_bigint(n, d))
    }

    pub fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (&self.num * &other.den).cmp(&(&other.num * &self.den))
    }

    pub fn eq(&self, other: &Self) -> bool {
        self.num == other.num && self.den == other.den
    }

    /// Check if ndigits is sufficient to represent this Rational exactly in decimal.
    fn ndigits_sufficient(&self, ndigits: i64) -> bool {
        if self.den.is_one() {
            return true;
        }
        let mut t = self.den.clone();
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
        let mut t = self.den.clone();
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

    /// Rational#floor(ndigits)
    /// ndigits == 0 => Integer, ndigits > 0 => Rational, ndigits < 0 => Integer
    pub fn rational_floor(&self, ndigits: i64) -> RationalFloorResult {
        if ndigits == 0 {
            // Integer division rounded toward negative infinity
            RationalFloorResult::Integer(num::integer::Integer::div_floor(&self.num, &self.den))
        } else if ndigits > 0 {
            if self.ndigits_sufficient(ndigits) {
                return RationalFloorResult::Rational(self.clone());
            }
            let d = BigInt::from(10u32).pow(ndigits as u32);
            let scaled_num = &self.num * &d;
            let scaled_den = &self.den;
            let floored = num::integer::Integer::div_floor(&scaled_num, scaled_den);
            RationalFloorResult::Rational(Self::new_bigint(floored, d))
        } else {
            let d = BigInt::from(10u32).pow((-ndigits) as u32);
            let i = &self.num / &self.den;
            RationalFloorResult::Integer(num::integer::Integer::div_floor(&i, &d) * &d)
        }
    }

    /// Rational#ceil(ndigits)
    pub fn rational_ceil(&self, ndigits: i64) -> RationalFloorResult {
        if ndigits == 0 {
            // ceil = -floor(-self)
            let neg_num = -self.num.clone();
            let floored = num::integer::Integer::div_floor(&neg_num, &self.den);
            RationalFloorResult::Integer(-floored)
        } else if ndigits > 0 {
            if self.ndigits_sufficient(ndigits) {
                return RationalFloorResult::Rational(self.clone());
            }
            let d = BigInt::from(10u32).pow(ndigits as u32);
            let scaled_num = &self.num * &d;
            let scaled_den = &self.den;
            let neg_scaled = -&scaled_num;
            let ceiled = -num::integer::Integer::div_floor(&neg_scaled, scaled_den);
            RationalFloorResult::Rational(Self::new_bigint(ceiled, d))
        } else {
            let d = BigInt::from(10u32).pow((-ndigits) as u32);
            let i = &self.num / &self.den;
            // ceil = -floor(-i, d)
            let neg_i = -&i;
            let floored = num::integer::Integer::div_floor(&neg_i, &d);
            RationalFloorResult::Integer(-floored * &d)
        }
    }

    /// Rational#truncate(ndigits)
    pub fn rational_truncate(&self, ndigits: i64) -> RationalFloorResult {
        if ndigits == 0 {
            RationalFloorResult::Integer(&self.num / &self.den)
        } else if ndigits > 0 {
            if self.ndigits_sufficient(ndigits) {
                return RationalFloorResult::Rational(self.clone());
            }
            let d = BigInt::from(10u32).pow(ndigits as u32);
            let scaled_num = &self.num * &d;
            let truncated = &scaled_num / &self.den;
            RationalFloorResult::Rational(Self::new_bigint(truncated, d))
        } else {
            let d = BigInt::from(10u32).pow((-ndigits) as u32);
            let i = &self.num / &self.den;
            RationalFloorResult::Integer((&i / &d) * &d)
        }
    }

    /// Rational#round(ndigits, half:)
    /// half: None => :up (default), Some("up"), Some("down"), Some("even")
    pub fn rational_round(&self, ndigits: i64, half: Option<&str>) -> RationalFloorResult {
        if ndigits == 0 {
            if self.den.is_one() {
                return RationalFloorResult::Integer(self.num.clone());
            }
            let (q, r) = num::integer::Integer::div_rem(&self.num, &self.den);
            let doubled = &r * BigInt::from(2);
            let abs_doubled = doubled.abs();
            let abs_den = self.den.abs();
            let result = if abs_doubled > abs_den {
                if self.num >= BigInt::ZERO {
                    q + 1
                } else {
                    q
                }
            } else if abs_doubled < abs_den {
                if self.num >= BigInt::ZERO {
                    q
                } else {
                    q
                }
            } else {
                // Exactly half
                match half {
                    Some("down") => q,
                    Some("even") => {
                        if (&q % 2u32).is_zero() {
                            q
                        } else if self.num >= BigInt::ZERO {
                            q + 1
                        } else {
                            q - 1
                        }
                    }
                    _ => {
                        // :up or default
                        if self.num >= BigInt::ZERO {
                            q + 1
                        } else {
                            q - 1
                        }
                    }
                }
            };
            RationalFloorResult::Integer(result)
        } else if ndigits > 0 {
            if self.ndigits_sufficient(ndigits) {
                return RationalFloorResult::Rational(self.clone());
            }
            let d = BigInt::from(10u32).pow(ndigits as u32);
            let scaled = Self::new_bigint(&self.num * &d, self.den.clone());
            if let RationalFloorResult::Integer(rounded) = scaled.rational_round(0, half) {
                RationalFloorResult::Rational(Self::new_bigint(rounded, d))
            } else {
                unreachable!()
            }
        } else {
            let d = BigInt::from(10u32).pow((-ndigits) as u32);
            let i = &self.num / &self.den;
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
                    Some("down") => q * &d,
                    Some("even") => {
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
            RationalFloorResult::Integer(result)
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
        Self::new_bigint(num, den)
    }

    /// Stern-Brocot search: find the simplest rational within [value - eps, value + eps].
    ///
    /// Both `value` and `eps` are RationalInner. `eps` must be non-negative.
    pub fn find_simplest(value: &Self, eps: &Self) -> Self {
        let eps_abs = eps.abs();
        // Handle negative: negate, search, negate back
        if value.is_negative() {
            let pos = Self::new_bigint(-value.num.clone(), value.den.clone());
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
        let mut p0 = BigInt::from(0);
        let mut q0 = BigInt::from(1);
        let mut p1 = BigInt::from(1);
        let mut q1 = BigInt::from(0);
        loop {
            let pm = &p0 + &p1;
            let qm = &q0 + &q1;
            let med = Self::new_bigint(pm.clone(), qm.clone());
            match med.cmp(&lo) {
                std::cmp::Ordering::Less => {
                    // med < lo: advance left bound
                    // k = ceil((lo * qm - pm) / (p1 - lo * q1))
                    let numer = Self::new_bigint(
                        &lo.num * &qm * &lo.den.clone() - &pm * &lo.den * &lo.den,
                        lo.den.clone() * &lo.den,
                    );
                    let _ = numer; // discard; use direct BigInt math
                    // lo.num/lo.den * qm - pm = (lo.num * qm - pm * lo.den) / lo.den
                    let top = &lo.num * &qm - &pm * &lo.den;
                    // p1 - lo.num/lo.den * q1 = (p1 * lo.den - lo.num * q1) / lo.den
                    let bot = &p1 * &lo.den - &lo.num * &q1;
                    let k = div_ceil_bigint(&top, &bot);
                    p0 = &p0 + &k * &p1;
                    q0 = &q0 + &k * &q1;
                }
                std::cmp::Ordering::Greater if med.cmp(&hi) == std::cmp::Ordering::Greater => {
                    // med > hi: advance right bound
                    let top = &pm * &hi.den - &hi.num * &qm;
                    let bot = &hi.num * &q0 - &p0 * &hi.den;
                    let k = div_ceil_bigint(&top, &bot);
                    p1 = &p1 + &k * &p0;
                    q1 = &q1 + &k * &q0;
                }
                _ => {
                    // lo <= med <= hi: found it
                    return Self::new_bigint(pm, qm);
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
    if r.is_zero() || (r.is_negative() && b.is_negative()) || (r.is_positive() && b.is_positive())
    {
        if r.is_zero() {
            q
        } else {
            q + 1
        }
    } else {
        q
    }
}
