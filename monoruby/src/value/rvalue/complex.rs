use num::{Signed, ToPrimitive};
use paste::paste;
use ruruby_parse::NReal;

use super::*;

#[derive(Clone, Debug, PartialEq, Hash)]
#[repr(transparent)]
pub struct ComplexInner(num::complex::Complex<Real>);

impl std::ops::Deref for ComplexInner {
    type Target = num::complex::Complex<Real>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for ComplexInner {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl std::convert::From<i64> for ComplexInner {
    fn from(i: i64) -> Self {
        ComplexInner(Real::from(i).into())
    }
}

impl std::convert::From<BigInt> for ComplexInner {
    fn from(b: BigInt) -> Self {
        ComplexInner(Real::from(b).into())
    }
}

impl std::convert::From<f64> for ComplexInner {
    fn from(f: f64) -> Self {
        ComplexInner(Real::from(f).into())
    }
}

impl std::convert::From<Real> for ComplexInner {
    fn from(re: Real) -> Self {
        ComplexInner(num::complex::Complex::new(re, 0.into()))
    }
}

impl std::convert::From<num::complex::Complex<Real>> for ComplexInner {
    fn from(complex: num::complex::Complex<Real>) -> Self {
        ComplexInner(complex)
    }
}

impl ComplexInner {
    pub fn new(re: Real, im: Real) -> Self {
        Self(num::complex::Complex { re, im })
    }

    pub fn re(&self) -> Real {
        self.0.re
    }

    pub fn im(&self) -> Real {
        self.0.im
    }

    pub fn eql(&self, other: &Self) -> bool {
        self.0.re.0.eql(&other.0.re.0) && self.0.im.0.eql(&other.0.im.0)
    }

    pub fn dup(&self) -> Self {
        Self::new(self.0.re.dup(), self.0.im.dup())
    }
}

#[derive(Clone, Copy, Debug, Hash)]
#[repr(transparent)]
pub struct Real(Value);

impl std::cmp::PartialEq for Real {
    fn eq(&self, other: &Self) -> bool {
        RealKind::from(*self) == RealKind::from(*other)
    }
}

impl std::cmp::PartialOrd for Real {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        RealKind::from(*self).partial_cmp(&RealKind::from(*other))
    }
}

impl num::Num for Real {
    type FromStrRadixErr = std::num::ParseIntError;

    fn from_str_radix(str: &str, radix: u32) -> std::result::Result<Self, Self::FromStrRadixErr> {
        Ok(i64::from_str_radix(str, radix)?.into())
    }
}

impl std::convert::From<i64> for Real {
    fn from(i: i64) -> Self {
        Real(Value::integer(i))
    }
}

impl std::convert::From<BigInt> for Real {
    fn from(b: BigInt) -> Self {
        Real(Value::bigint(b))
    }
}

impl std::convert::From<f64> for Real {
    fn from(f: f64) -> Self {
        Real(Value::float(f))
    }
}

impl std::convert::From<NReal> for Real {
    fn from(r: NReal) -> Self {
        match r {
            NReal::Integer(i) => Real(Value::integer(i)),
            NReal::Bignum(b) => Real(Value::bigint(b)),
            NReal::Float(f) => Real(Value::float(f)),
        }
    }
}

impl std::convert::From<RealKind> for Real {
    fn from(real: RealKind) -> Self {
        match real {
            RealKind::Integer(i) => Real(Value::integer(i)),
            RealKind::BigInt(b) => Real(Value::bigint(b)),
            RealKind::Float(f) => Real(Value::float(f)),
        }
    }
}

impl std::convert::TryFrom<Value> for Real {
    type Error = MonorubyErr;
    fn try_from(value: Value) -> Result<Self> {
        match value.unpack() {
            RV::Fixnum(i) => Ok(Real(Value::integer(i))),
            RV::BigInt(b) => Ok(Real(Value::bigint(b.clone()))),
            RV::Float(f) => Ok(Real(Value::float(f))),
            _ => Err(MonorubyErr::cant_convert_into_float(value)),
        }
    }
}

impl std::ops::Neg for Real {
    type Output = Real;
    fn neg(self) -> Self::Output {
        RealKind::from(self).neg().into()
    }
}

impl std::ops::Add<Real> for Real {
    type Output = Real;
    fn add(self, other: Real) -> Self::Output {
        (RealKind::from(self) + (RealKind::from(other))).into()
    }
}

impl std::ops::Sub<Real> for Real {
    type Output = Real;
    fn sub(self, other: Real) -> Self::Output {
        (RealKind::from(self) - (RealKind::from(other))).into()
    }
}

impl std::ops::Mul<Real> for Real {
    type Output = Real;
    fn mul(self, other: Real) -> Self::Output {
        (RealKind::from(self) * (RealKind::from(other))).into()
    }
}

impl std::ops::Div<Real> for Real {
    type Output = Real;
    fn div(self, other: Real) -> Self::Output {
        (RealKind::from(self) / (RealKind::from(other))).into()
    }
}

impl std::ops::Rem<Real> for Real {
    type Output = Real;
    fn rem(self, other: Real) -> Self::Output {
        (RealKind::from(self) % (RealKind::from(other))).into()
    }
}

impl num::Zero for Real {
    fn zero() -> Self {
        0i64.into()
    }

    fn is_zero(&self) -> bool {
        match RealKind::from(*self) {
            RealKind::Integer(i) => i.is_zero(),
            RealKind::BigInt(b) => b.is_zero(),
            RealKind::Float(f) => f.is_zero(),
        }
    }
}

impl num::One for Real {
    fn one() -> Self {
        1i64.into()
    }
}

impl Real {
    pub fn get(self) -> Value {
        self.0
    }

    pub fn dup(&self) -> Self {
        Real(self.0.dup())
    }

    pub fn deep_copy(&self) -> Self {
        Real(self.0.deep_copy())
    }

    pub fn is_positive(&self) -> bool {
        RealKind::from(*self).is_positive()
    }
}

pub enum RealKind {
    Integer(i64),
    BigInt(BigInt),
    Float(f64),
}

impl std::cmp::PartialEq for RealKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RealKind::Integer(a), RealKind::Integer(b)) => a == b,
            (RealKind::BigInt(a), RealKind::BigInt(b)) => a == b,
            (RealKind::Float(a), RealKind::Float(b)) => a == b,
            _ => false,
        }
    }
}

impl std::cmp::PartialOrd for RealKind {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (RealKind::Integer(a), RealKind::Integer(b)) => a.partial_cmp(b),
            (RealKind::Integer(a), RealKind::BigInt(b)) => BigInt::from(*a).partial_cmp(b),
            (RealKind::Integer(a), RealKind::Float(b)) => (a.to_f64().unwrap()).partial_cmp(b),
            (RealKind::BigInt(a), RealKind::Integer(b)) => a.partial_cmp(&BigInt::from(*b)),
            (RealKind::BigInt(a), RealKind::BigInt(b)) => a.partial_cmp(b),
            (RealKind::BigInt(a), RealKind::Float(b)) => a.to_f64().unwrap().partial_cmp(b),
            (RealKind::Float(a), RealKind::Integer(b)) => a.partial_cmp(&(*b as f64)),
            (RealKind::Float(a), RealKind::BigInt(b)) => a.partial_cmp(&b.to_f64().unwrap()),
            (RealKind::Float(a), RealKind::Float(b)) => a.partial_cmp(b),
        }
    }
}

impl std::convert::From<RealKind> for Value {
    fn from(real: RealKind) -> Self {
        match real {
            RealKind::Integer(i) => Value::integer(i),
            RealKind::BigInt(b) => Value::bigint(b),
            RealKind::Float(f) => Value::float(f),
        }
    }
}

impl std::convert::From<NReal> for RealKind {
    fn from(r: NReal) -> Self {
        match r {
            NReal::Integer(i) => RealKind::Integer(i),
            NReal::Bignum(b) => RealKind::BigInt(b),
            NReal::Float(f) => RealKind::Float(f),
        }
    }
}

impl std::convert::From<Real> for RealKind {
    fn from(r: Real) -> Self {
        match r.0.unpack() {
            RV::Fixnum(i) => RealKind::Integer(i),
            RV::Float(f) => RealKind::Float(f),
            RV::BigInt(b) => RealKind::BigInt(b.clone()),
            _ => unreachable!(),
        }
    }
}

impl std::convert::TryFrom<Value> for RealKind {
    type Error = MonorubyErr;
    fn try_from(value: Value) -> Result<Self> {
        match value.unpack() {
            RV::Fixnum(i) => Ok(RealKind::Integer(i)),
            RV::BigInt(b) => Ok(RealKind::BigInt(b.clone())),
            RV::Float(f) => Ok(RealKind::Float(f)),
            _ => Err(MonorubyErr::cant_convert_into_float(value)),
        }
    }
}

macro_rules! binops {
    (($op1:ident, $op2:ident)) => {
        paste! {
            impl std::ops::$op1<RealKind> for RealKind {
                type Output = RealKind;
                fn $op2(self, other: RealKind) -> Self::Output {
                    match (self, other) {
                        (RealKind::Integer(a), RealKind::Integer(b)) => {
                            if let Some(c) = a.[<checked_ $op2>](b) {
                                RealKind::Integer(c)
                            } else {
                                RealKind::BigInt(BigInt::from(a).$op2(BigInt::from(b)))
                            }
                        }
                        (RealKind::Integer(a), RealKind::BigInt(b)) => RealKind::BigInt(BigInt::from(a).$op2(b)),
                        (RealKind::Integer(a), RealKind::Float(b)) => RealKind::Float((a as f64).$op2(b)),
                        (RealKind::BigInt(a), RealKind::Integer(b)) => RealKind::BigInt(a.$op2(BigInt::from(b))),
                        (RealKind::BigInt(a), RealKind::BigInt(b)) => RealKind::BigInt(a.$op2(b)),
                        (RealKind::BigInt(a), RealKind::Float(b)) => {
                            RealKind::Float(num::ToPrimitive::to_f64(&a).unwrap().$op2(b))
                        }
                        (RealKind::Float(a), RealKind::Integer(b)) => RealKind::Float(a.$op2(b as f64)),
                        (RealKind::Float(a), RealKind::BigInt(b)) => {
                            RealKind::Float(a.$op2(num::ToPrimitive::to_f64(&b).unwrap()))
                        }
                        (RealKind::Float(a), RealKind::Float(b)) => RealKind::Float(a.$op2(b)),
                    }
                }
            }
        }
    };
    (($op1:ident, $op2:ident), $(($op1_:ident, $op2_:ident)),+) => {
        binops!(($op1, $op2));
        binops!($(($op1_, $op2_)),+);
    };
}

binops!((Add, add), (Sub, sub), (Mul, mul), (Div, div), (Rem, rem));

impl std::ops::Neg for RealKind {
    type Output = RealKind;
    fn neg(self) -> Self::Output {
        match self {
            RealKind::Integer(i) => match i.checked_neg() {
                Some(i) => RealKind::Integer(i),
                None => RealKind::BigInt(BigInt::from(i).neg()),
            },
            RealKind::BigInt(b) => RealKind::BigInt(-b),
            RealKind::Float(f) => RealKind::Float(-f),
        }
    }
}

impl RealKind {
    fn is_positive(&self) -> bool {
        match self {
            RealKind::Integer(i) => i.is_positive(),
            RealKind::BigInt(b) => b.is_positive(),
            RealKind::Float(f) => f.is_sign_positive(),
        }
    }

    pub fn to_f64(&self) -> f64 {
        match self {
            RealKind::Integer(i) => *i as f64,
            RealKind::BigInt(b) => num::ToPrimitive::to_f64(b).unwrap(),
            RealKind::Float(f) => *f,
        }
    }
}
