use super::*;

use num::{BigInt, Integer, ToPrimitive};
use paste::paste;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Shl, Shr, Sub};

//
// Generic operations.
//

macro_rules! binop_values {
    (($op:ident, $op_str:expr)) => {
        paste! {
            pub(super) extern "C" fn [<$op _values>](
                interp: &mut Interp,
                globals: &mut Globals,
                lhs: Value,
                rhs: Value
            ) -> Option<Value> {
                let v = match (lhs.unpack(), rhs.unpack()) {
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
                    _ => {
                        interp.error = Some(MonorubyErr::method_not_found(
                            globals.get_ident_id($op_str),
                            Loc::default(),
                            interp.sourceinfo.clone()
                        ));
                        return None;
                    }
                };
                Some(v)
            }
        }
    };
    (($op1:ident, $op_str1:expr), $(($op2:ident, $op_str2:expr)),+) => {
        binop_values!(($op1, $op_str1));
        binop_values!($(($op2, $op_str2)),+);
    };
}

binop_values!((add, "+"), (sub, "-"), (mul, "*"));

pub(super) extern "C" fn div_values(
    interp: &mut Interp,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    let v = match (lhs.unpack(), rhs.unpack()) {
        (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs.div_floor(rhs)),
        (RV::Integer(lhs), RV::BigInt(rhs)) => Value::bigint(BigInt::from(lhs).div_floor(rhs)),
        (RV::Integer(lhs), RV::Float(rhs)) => Value::float((lhs as f64).div(&rhs)),
        (RV::BigInt(lhs), RV::Integer(rhs)) => Value::bigint(lhs.div_floor(&BigInt::from(rhs))),
        (RV::BigInt(lhs), RV::BigInt(rhs)) => Value::bigint(lhs.div_floor(rhs)),
        (RV::BigInt(lhs), RV::Float(rhs)) => Value::float((lhs.to_f64().unwrap()).div(&rhs)),
        (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs.div(&(rhs as f64))),
        (RV::Float(lhs), RV::BigInt(rhs)) => Value::float(lhs.div(&rhs.to_f64().unwrap())),
        (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs.div(&rhs)),
        _ => {
            interp.error = Some(MonorubyErr::method_not_found(
                globals.get_ident_id("/"),
                Loc::default(),
                interp.sourceinfo.clone(),
            ));
            return None;
        }
    };
    Some(v)
}

macro_rules! int_binop_values {
    (($op:ident, $op_str:expr)) => {
        paste! {
            pub(super) extern "C" fn [<$op _values>](
                interp: &mut Interp,
                globals: &mut Globals,
                lhs: Value,
                rhs: Value
            ) -> Option<Value> {
                let v = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs.$op(&rhs)),
                    (RV::Integer(lhs), RV::BigInt(rhs)) => Value::bigint(BigInt::from(lhs).$op(rhs)),
                    (RV::BigInt(lhs), RV::Integer(rhs)) => Value::bigint(lhs.$op(BigInt::from(rhs))),
                    (RV::BigInt(lhs), RV::BigInt(rhs)) => Value::bigint(lhs.$op(rhs)),
                    _ => {
                        interp.error = Some(MonorubyErr::method_not_found(
                            globals.get_ident_id($op_str),
                            Loc::default(),
                            interp.sourceinfo.clone()
                        ));
                        return None;
                    }
                };
                Some(v)
            }
        }
    };
    (($op1:ident, $op_str1:expr), $(($op2:ident, $op_str2:expr)),+) => {
        int_binop_values!(($op1, $op_str1));
        int_binop_values!($(($op2, $op_str2)),+);
    };
}

int_binop_values!((bitor, "|"), (bitand, "&"), (bitxor, "^"));

pub(super) extern "C" fn shr_values(
    interp: &mut Interp,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    let v = match (lhs.unpack(), rhs.unpack()) {
        (RV::Integer(lhs), RV::Integer(rhs)) => {
            if rhs >= 0 {
                int_shr(lhs, rhs as u64 as u32)
            } else {
                int_shl(lhs, -rhs as u64 as u32)
            }
        }
        (RV::BigInt(lhs), RV::Integer(rhs)) => {
            if rhs >= 0 {
                bigint_shr(lhs, rhs as u64 as u32)
            } else {
                bigint_shl(lhs, -rhs as u64 as u32)
            }
        }
        (_lhs, _rhs) => {
            interp.error = Some(MonorubyErr::method_not_found(
                globals.get_ident_id(">>"),
                Loc::default(),
                interp.sourceinfo.clone(),
            ));
            return None;
        }
    };
    Some(v)
}

pub(super) extern "C" fn shl_values(
    interp: &mut Interp,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    let v = match (lhs.unpack(), rhs.unpack()) {
        (RV::Integer(lhs), RV::Integer(rhs)) => {
            if rhs >= 0 {
                int_shl(lhs, rhs as u64 as u32)
            } else {
                int_shr(lhs, -rhs as u64 as u32)
            }
        }
        (RV::BigInt(lhs), RV::Integer(rhs)) => {
            if rhs >= 0 {
                bigint_shl(lhs, rhs as u64 as u32)
            } else {
                bigint_shr(lhs, -rhs as u64 as u32)
            }
        }
        (_lhs, _rhs) => {
            interp.error = Some(MonorubyErr::method_not_found(
                globals.get_ident_id("<<"),
                Loc::default(),
                interp.sourceinfo.clone(),
            ));
            return None;
        }
    };
    Some(v)
}

fn int_shr(lhs: i64, rhs: u32) -> Value {
    Value::integer(
        lhs.checked_shr(rhs)
            .unwrap_or(if lhs >= 0 { 0 } else { -1 }),
    )
}

fn int_shl(lhs: i64, rhs: u32) -> Value {
    match lhs.checked_shl(rhs) {
        Some(res) => Value::integer(res),
        None => bigint_shl(&BigInt::from(lhs), rhs),
    }
}

fn bigint_shr(lhs: &BigInt, rhs: u32) -> Value {
    Value::bigint(lhs.shr(rhs))
}

fn bigint_shl(lhs: &BigInt, rhs: u32) -> Value {
    Value::bigint(lhs.shl(rhs))
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
                    (lhs, rhs) => unreachable!("{:?} cmp {:?}", lhs, rhs),
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

pub(super) extern "C" fn neg_value(
    interp: &mut Interp,
    globals: &mut Globals,
    lhs: Value,
) -> Option<Value> {
    let v = match lhs.unpack() {
        RV::Integer(lhs) => match lhs.checked_neg() {
            Some(lhs) => Value::integer(lhs),
            None => Value::bigint(-BigInt::from(lhs)),
        },
        RV::Float(lhs) => Value::float(-lhs),
        RV::BigInt(lhs) => Value::bigint(-lhs),
        _ => {
            interp.error = Some(MonorubyErr::method_not_found(
                globals.get_ident_id("@-"),
                Loc::default(),
                interp.sourceinfo.clone(),
            ));
            return None;
        }
    };
    Some(v)
}

pub extern "C" fn concatenate_string(arg: *mut Value, len: usize) -> Value {
    let mut res = vec![];
    for i in 0..len {
        let v = unsafe { *arg.sub(i) };
        match v.unpack() {
            RV::Nil => res.extend("nil".bytes()),
            RV::Bool(b) => res.extend(format!("{}", b).bytes()),
            RV::Integer(i) => res.extend(format!("{}", i).bytes()),
            RV::BigInt(b) => res.extend(format!("{}", b).bytes()),
            RV::String(b) => res.extend(b),
            _ => unimplemented!(),
        };
    }
    Value::string(res)
}
