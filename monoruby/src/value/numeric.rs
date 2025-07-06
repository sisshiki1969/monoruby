use num::{Signed, ToPrimitive, Zero};
use paste::paste;
use rubymap::RubyEql;
use ruruby_parse::NReal;

use super::*;

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct Real(Value);

impl GC<RValue> for Real {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        self.0.mark(alloc)
    }
}

impl std::fmt::Display for Real {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match RealKind::from(*self) {
            RealKind::Integer(i) => write!(fmt, "{}", i),
            RealKind::BigInt(b) => write!(fmt, "{}", b),
            RealKind::Float(f) => write!(fmt, "{}", f),
        }
    }
}

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

impl std::hash::Hash for Real {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match RealKind::from(*self) {
            RealKind::Integer(i) => i.hash(state),
            RealKind::BigInt(b) => b.hash(state),
            RealKind::Float(f) => f.to_bits().hash(state),
        }
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

impl Real {
    pub fn try_from(store: &Store, value: Value) -> Result<Self> {
        match value.unpack() {
            RV::Fixnum(i) => Ok(Real(Value::integer(i))),
            RV::BigInt(b) => Ok(Real(Value::bigint(b.clone()))),
            RV::Float(f) => Ok(Real(Value::float(f))),
            _ => Err(MonorubyErr::cant_convert_into_float(store, value)),
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

impl RubyEql<Executor, Globals, MonorubyErr> for Real {
    fn eql(&self, other: &Self, vm: &mut Executor, globals: &mut Globals) -> Result<bool> {
        self.0.eql(&other.0, vm, globals)
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

    pub fn is_negative(&self) -> bool {
        RealKind::from(*self).is_negative()
    }

    pub fn is_zero(&self) -> bool {
        RealKind::from(*self).is_zero()
    }

    pub fn to_f64(&self) -> f64 {
        RealKind::from(*self).to_f64()
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
            (RealKind::Integer(a), RealKind::Float(b)) => a.to_f64() == Some(*b),
            (RealKind::BigInt(a), RealKind::BigInt(b)) => a == b,
            (RealKind::BigInt(a), RealKind::Float(b)) => a.to_f64() == Some(*b),
            (RealKind::Float(a), RealKind::Integer(b)) => Some(*a) == b.to_f64(),
            (RealKind::Float(a), RealKind::Float(b)) => a == b,
            (RealKind::Float(a), RealKind::BigInt(b)) => Some(*a) == b.to_f64(),
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

impl RealKind {
    pub fn try_from(store: &Store, value: Value) -> Result<Self> {
        match value.unpack() {
            RV::Fixnum(i) => Ok(RealKind::Integer(i)),
            RV::BigInt(b) => Ok(RealKind::BigInt(b.clone())),
            RV::Float(f) => Ok(RealKind::Float(f)),
            _ => Err(MonorubyErr::cant_convert_into_float(store, value)),
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

    fn is_negative(&self) -> bool {
        match self {
            RealKind::Integer(i) => i.is_negative(),
            RealKind::BigInt(b) => b.is_negative(),
            RealKind::Float(f) => f.is_sign_negative(),
        }
    }

    fn is_zero(&self) -> bool {
        match self {
            RealKind::Integer(i) => i.is_zero(),
            RealKind::BigInt(b) => b.is_zero(),
            RealKind::Float(f) => f.is_zero(),
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
