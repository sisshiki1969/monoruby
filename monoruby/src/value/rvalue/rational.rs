use num::bigint::BigInt;
use num::traits::{One, Zero};
use num::ToPrimitive;

use super::*;

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
        let n = self.num.to_f64().unwrap_or(f64::INFINITY);
        let d = self.den.to_f64().unwrap_or(f64::INFINITY);
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
            num: if self.num < BigInt::ZERO { -self.num.clone() } else { self.num.clone() },
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

    pub fn to_s(&self) -> String {
        format!("{}/{}", self.num, self.den)
    }

    pub fn inspect(&self) -> String {
        format!("({}/{})", self.num, self.den)
    }
}

