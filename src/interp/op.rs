use super::*;

use paste::paste;
use std::ops::{Add, Div, Mul, Sub};

macro_rules! binop_values {
    ($op:ident) => {
        paste! {
            pub(super) extern "C" fn [<$op _values>](lhs: Value, rhs: Value) -> Value {
                if lhs.is_fnum() && rhs.is_fnum() {
                    Value::fixnum(lhs.as_fnum().$op(&rhs.as_fnum()))
                } else {
                    match (lhs.unpack(), rhs.unpack()) {
                        (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs.$op(&rhs)),
                        (RV::Integer(lhs), RV::Float(rhs)) => Value::float((lhs as f64).$op(&rhs)),
                        (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs.$op(&(rhs as f64))),
                        (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs.$op(&rhs)),
                        _ => unreachable!(),
                    }
                }
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        binop_values!($op1);
        binop_values!($($op2),+);
    };
}

binop_values!(add, sub, mul, div);

macro_rules! binop_ri_values {
    ($op:ident) => {
        paste! {
            pub(super) extern "C" fn [<$op _ri_values>](lhs: Value, rhs: i64) -> Value {
                if lhs.is_fnum() {
                    Value::fixnum(lhs.as_fnum().$op(&rhs))
                } else {
                    match lhs.unpack() {
                        RV::Integer(lhs) => Value::integer(lhs.$op(&(rhs as i32))),
                        RV::Float(lhs) => Value::float(lhs.$op(&(rhs as f64))),
                        _ => unreachable!(),
                    }
                }
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        binop_ri_values!($op1);
        binop_ri_values!($($op2),+);
    };
}

binop_ri_values!(add, sub);

macro_rules! cmp_values {
    ($op:ident) => {
        paste! {
          pub(super) extern "C" fn [<cmp_ $op _values>](lhs: Value, rhs: Value) -> Value {
              let b = if lhs.is_fnum() && rhs.is_fnum() {
                  lhs.as_fnum().$op(&rhs.as_fnum())
              } else {
                  match (lhs.unpack(), rhs.unpack()) {
                      (RV::Integer(lhs), RV::Integer(rhs)) => lhs.$op(&rhs),
                      (RV::Integer(lhs), RV::Float(rhs)) => (lhs as f64).$op(&rhs),
                      (RV::Float(lhs), RV::Integer(rhs)) => lhs.$op(&(rhs as f64)),
                      (RV::Float(lhs), RV::Float(rhs)) => lhs.$op(&rhs),
                      _ => unreachable!(),
                  }
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

cmp_values!(eq, ne, ge, gt, le, lt);

macro_rules! cmp_ri_values {
    ($op:ident) => {
        paste! {
          pub(super) extern "C" fn [<cmp_ $op _ri_values>](lhs: Value, rhs: i64) -> Value {
              let b = if lhs.is_fnum()  {
                  lhs.as_fnum().$op(&rhs)
              } else {
                  match lhs.unpack() {
                      RV::Integer(lhs) => lhs.$op(&(rhs as i32)),
                      RV::Float(lhs) => lhs.$op(&(rhs as f64)),
                      _ => unreachable!(),
                  }
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
    if lhs.is_fnum() {
        Value::fixnum(-lhs.as_fnum())
    } else {
        match lhs.unpack() {
            RV::Integer(lhs) => Value::integer(-lhs),
            RV::Float(lhs) => Value::float(-lhs),
            _ => unreachable!(),
        }
    }
}
