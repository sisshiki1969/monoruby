use num::ToPrimitive;
use ruruby_parse::NReal;

use super::*;

#[derive(Clone, PartialEq, Hash, Debug)]
pub struct ComplexInner {
    re: Value,
    im: Value,
}

impl ComplexInner {
    pub fn new(re: Value, im: Value) -> Self {
        Self { re, im }
    }

    pub fn re(&self) -> Value {
        self.re
    }

    pub fn im(&self) -> Value {
        self.im
    }

    pub fn eql(&self, other: &Self) -> bool {
        self.re.eql(&other.re) && self.im.eql(&other.im)
    }

    pub fn dup(&self) -> Self {
        Self::new(self.re.dup(), self.im.dup())
    }
}

pub(crate) enum Real {
    Integer(i64),
    BigInt(BigInt),
    Float(f64),
}

impl std::ops::Add for Real {
    type Output = Real;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Real::Integer(lhs), Real::Integer(rhs)) => Real::Integer(lhs + rhs),
            (Real::Integer(lhs), Real::BigInt(rhs)) => Real::BigInt(BigInt::from(lhs) + rhs),
            (Real::Integer(lhs), Real::Float(rhs)) => Real::Float(lhs as f64 + rhs),
            (Real::BigInt(lhs), Real::Integer(rhs)) => Real::BigInt(lhs + BigInt::from(rhs)),
            (Real::BigInt(lhs), Real::BigInt(rhs)) => Real::BigInt(lhs + rhs),
            (Real::BigInt(lhs), Real::Float(rhs)) => Real::Float(lhs.to_f64().unwrap() + rhs),
            (Real::Float(lhs), Real::Integer(rhs)) => Real::Float(lhs + rhs as f64),
            (Real::Float(lhs), Real::BigInt(rhs)) => Real::Float(lhs + rhs.to_f64().unwrap()),
            (Real::Float(lhs), Real::Float(rhs)) => Real::Float(lhs + rhs),
        }
    }
}

impl std::ops::Sub for Real {
    type Output = Real;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Real::Integer(lhs), Real::Integer(rhs)) => Real::Integer(lhs - rhs),
            (Real::Integer(lhs), Real::BigInt(rhs)) => Real::BigInt(BigInt::from(lhs) - rhs),
            (Real::Integer(lhs), Real::Float(rhs)) => Real::Float(lhs as f64 - rhs),
            (Real::BigInt(lhs), Real::Integer(rhs)) => Real::BigInt(lhs - BigInt::from(rhs)),
            (Real::BigInt(lhs), Real::BigInt(rhs)) => Real::BigInt(lhs - rhs),
            (Real::BigInt(lhs), Real::Float(rhs)) => Real::Float(lhs.to_f64().unwrap() - rhs),
            (Real::Float(lhs), Real::Integer(rhs)) => Real::Float(lhs - rhs as f64),
            (Real::Float(lhs), Real::BigInt(rhs)) => Real::Float(lhs - rhs.to_f64().unwrap()),
            (Real::Float(lhs), Real::Float(rhs)) => Real::Float(lhs - rhs),
        }
    }
}

impl std::ops::Mul for Real {
    type Output = Real;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Real::Integer(lhs), Real::Integer(rhs)) => Real::Integer(lhs * rhs),
            (Real::Integer(lhs), Real::BigInt(rhs)) => Real::BigInt(BigInt::from(lhs) * rhs),
            (Real::Integer(lhs), Real::Float(rhs)) => Real::Float(lhs as f64 * rhs),
            (Real::BigInt(lhs), Real::Integer(rhs)) => Real::BigInt(lhs * BigInt::from(rhs)),
            (Real::BigInt(lhs), Real::BigInt(rhs)) => Real::BigInt(lhs * rhs),
            (Real::BigInt(lhs), Real::Float(rhs)) => Real::Float(lhs.to_f64().unwrap() * rhs),
            (Real::Float(lhs), Real::Integer(rhs)) => Real::Float(lhs * rhs as f64),
            (Real::Float(lhs), Real::BigInt(rhs)) => Real::Float(lhs * rhs.to_f64().unwrap()),
            (Real::Float(lhs), Real::Float(rhs)) => Real::Float(lhs * rhs),
        }
    }
}

impl std::ops::Neg for Real {
    type Output = Real;
    fn neg(self) -> Self::Output {
        match self {
            Real::Integer(i) => Real::Integer(-i),
            Real::BigInt(b) => Real::BigInt(-b),
            Real::Float(f) => Real::Float(-f),
        }
    }
}

impl std::convert::From<Real> for Value {
    fn from(real: Real) -> Self {
        match real {
            Real::Integer(i) => Value::integer(i),
            Real::BigInt(b) => Value::bigint(b),
            Real::Float(f) => Value::float(f),
        }
    }
}

impl std::convert::From<NReal> for Real {
    fn from(r: NReal) -> Self {
        match r {
            NReal::Integer(i) => Real::Integer(i),
            NReal::Bignum(b) => Real::BigInt(b),
            NReal::Float(f) => Real::Float(f),
        }
    }
}
