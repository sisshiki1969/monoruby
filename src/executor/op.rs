use super::*;

use num::{BigInt, Integer, ToPrimitive};
use paste::paste;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Sub};

//
// Generic operations.
//

macro_rules! binop_values {
    ($op:ident) => {
        paste! {
            pub(super) extern "C" fn [<$op _values>](lhs: Value, rhs: Value) -> Value {
                match (lhs.unpack(), rhs.unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => match lhs.[<checked_ $op>](rhs){
                        Some(res) => Value::integer(res),
                        None => Value::bigint(BigInt::from(lhs).$op(BigInt::from(rhs))),
                    }
                    (RV::BigInt(lhs), RV::Integer(rhs)) => Value::bigint(lhs.$op(BigInt::from(rhs))),
                    (RV::Integer(lhs), RV::BigInt(rhs)) => Value::bigint(BigInt::from(lhs).$op(rhs)),
                    (RV::BigInt(lhs), RV::BigInt(rhs)) => Value::bigint(lhs.$op(rhs)),
                    (RV::Integer(lhs), RV::Float(rhs)) => Value::float((lhs as f64).$op(&rhs)),
                    (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs.$op(&(rhs as f64))),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs.$op(&rhs)),
                    _ => unreachable!(),
                }
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        binop_values!($op1);
        binop_values!($($op2),+);
    };
}

binop_values!(add, sub, mul);

pub(super) extern "C" fn div_values(lhs: Value, rhs: Value) -> Value {
    match (lhs.unpack(), rhs.unpack()) {
        (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs.div_floor(&rhs)),
        (RV::Integer(lhs), RV::BigInt(rhs)) => Value::bigint(BigInt::from(lhs).div_floor(rhs)),
        (RV::Integer(lhs), RV::Float(rhs)) => Value::float((lhs as f64).div(&rhs)),
        (RV::BigInt(lhs), RV::Integer(rhs)) => Value::bigint(lhs.div_floor(&BigInt::from(rhs))),
        (RV::BigInt(lhs), RV::BigInt(rhs)) => Value::bigint(lhs.div_floor(rhs)),
        (RV::BigInt(lhs), RV::Float(rhs)) => Value::float((lhs.to_f64().unwrap()).div(&rhs)),
        (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs.div(&(rhs as f64))),
        (RV::Float(lhs), RV::BigInt(rhs)) => Value::float(lhs.div(&rhs.to_f64().unwrap())),
        (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs.div(&rhs)),
        _ => unreachable!(),
    }
}

macro_rules! int_binop_values {
    ($op:ident) => {
        paste! {
            pub(super) extern "C" fn [<$op _values>](lhs: Value, rhs: Value) -> Value {
                match (lhs.unpack(), rhs.unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs.$op(&rhs)),
                    (RV::Integer(lhs), RV::BigInt(rhs)) => Value::bigint(BigInt::from(lhs).$op(rhs)),
                    (RV::BigInt(lhs), RV::Integer(rhs)) => Value::bigint(lhs.$op(BigInt::from(rhs))),
                    (RV::BigInt(lhs), RV::BigInt(rhs)) => Value::bigint(lhs.$op(rhs)),
                    _ => unreachable!(),
                }
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        int_binop_values!($op1);
        int_binop_values!($($op2),+);
    };
}

int_binop_values!(bitor, bitand, bitxor);

pub(super) extern "C" fn shr_values(lhs: Value, rhs: Value) -> Value {
    match (lhs.unpack(), rhs.unpack()) {
        (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(if rhs >= 0 {
            lhs.checked_shr(rhs as u64 as u32).unwrap_or(0)
        } else {
            lhs.checked_shl(-rhs as u64 as u32).unwrap_or(0)
        }),
        _ => unreachable!(),
    }
}

pub(super) extern "C" fn shl_values(lhs: Value, rhs: Value) -> Value {
    match (lhs.unpack(), rhs.unpack()) {
        (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(if rhs >= 0 {
            lhs.checked_shl(rhs as u64 as u32).unwrap_or(0)
        } else {
            lhs.checked_shr(-rhs as u64 as u32).unwrap_or(0)
        }),
        _ => unreachable!(),
    }
}

macro_rules! cmp_values {
    ($op:ident) => {
        paste! {
            pub(super) extern "C" fn [<cmp_ $op _values>](lhs: Value, rhs: Value) -> Value {
                let b = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => lhs.$op(&rhs),
                    (RV::Integer(lhs), RV::BigInt(rhs)) => BigInt::from(lhs).$op(&rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => (lhs as f64).$op(&rhs),
                    (RV::BigInt(lhs), RV::Integer(rhs)) => lhs.$op(&BigInt::from(rhs)),
                    (RV::BigInt(lhs), RV::BigInt(rhs)) => lhs.$op(&rhs),
                    (RV::BigInt(lhs), RV::Float(rhs)) => lhs.to_f64().unwrap().$op(&rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => lhs.$op(&(rhs as f64)),
                    (RV::Float(lhs), RV::BigInt(rhs)) => lhs.$op(&(rhs.to_f64().unwrap())),
                    (RV::Float(lhs), RV::Float(rhs)) => lhs.$op(&rhs),
                    _ => unreachable!(),
                };
                Value::bool(b)
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        cmp_values!($op1);
        cmp_values!($($op2),+);
    };
}

cmp_values!(ge, gt, le, lt);

macro_rules! eq_values {
    ($op:ident) => {
        paste! {
            pub(super) extern "C" fn [<cmp_ $op _values>](lhs: Value, rhs: Value) -> Value {
                let b = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => lhs.$op(&rhs),
                    (RV::Integer(lhs), RV::BigInt(rhs)) => BigInt::from(lhs).$op(&rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => (lhs as f64).$op(&rhs),
                    (RV::BigInt(lhs), RV::Integer(rhs)) => lhs.$op(&BigInt::from(rhs)),
                    (RV::BigInt(lhs), RV::BigInt(rhs)) => lhs.$op(&rhs),
                    (RV::BigInt(lhs), RV::Float(rhs)) => lhs.to_f64().unwrap().$op(&rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => lhs.$op(&(rhs as f64)),
                    (RV::Float(lhs), RV::BigInt(rhs)) => lhs.$op(&(rhs.to_f64().unwrap())),
                    (RV::Float(lhs), RV::Float(rhs)) => lhs.$op(&rhs),
                    (RV::Bool(lhs), RV::Bool(rhs)) => lhs.$op(&rhs),
                    _ => unreachable!(),
                };
                Value::bool(b)
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        eq_values!($op1);
        eq_values!($($op2),+);
    };
}

eq_values!(eq, ne);

macro_rules! cmp_ri_values {
    ($op:ident) => {
        paste! {
            pub(super) extern "C" fn [<cmp_ $op _ri_values>](lhs: Value, rhs: i64) -> Value {
                let b = match lhs.unpack() {
                    RV::Integer(lhs) => lhs.$op(&rhs),
                    RV::BigInt(lhs) => lhs.$op(&BigInt::from(rhs)),
                    RV::Float(lhs) => lhs.$op(&(rhs as f64)),
                    _ => unreachable!(),
                };
                Value::bool(b)
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        cmp_ri_values!($op1);
        cmp_ri_values!($($op2),+);
    };
}

cmp_ri_values!(eq, ne, ge, gt, le, lt);

pub(super) extern "C" fn neg_value(lhs: Value) -> Value {
    match lhs.unpack() {
        RV::Integer(lhs) => match lhs.checked_neg() {
            Some(lhs) => Value::integer(lhs),
            None => Value::bigint(-BigInt::from(lhs)),
        },
        RV::Float(lhs) => Value::float(-lhs),
        RV::BigInt(lhs) => Value::bigint(-lhs),
        _ => unreachable!(),
    }
}
