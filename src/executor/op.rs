use super::*;

use num::{BigInt, Integer, ToPrimitive, Zero};
use paste::paste;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Shl, Shr, Sub};

//
// Generic operations.
//

macro_rules! binop_values {
    (($op:ident, $op_str:expr)) => {
        paste! {
            pub(super) extern "C" fn [<$op _values>](
                _interp: &mut Interp,
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
                        globals.error = Some(MonorubyErr::method_not_found($op_str));
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

binop_values!(
    (add, IdentId::_ADD),
    //(sub, IdentId::_SUB),
    (mul, IdentId::_MUL)
);

pub(super) extern "C" fn sub_values(
    _interp: &mut Interp,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    let v = match (lhs.unpack(), rhs.unpack()) {
        (RV::Integer(lhs), RV::Integer(rhs)) => match lhs.checked_sub(rhs) {
            Some(res) => Value::integer(res),
            None => Value::bigint(BigInt::from(lhs).sub(BigInt::from(rhs))),
        },
        (RV::BigInt(lhs), RV::Integer(rhs)) => Value::bigint(lhs.sub(BigInt::from(rhs))),
        (RV::Integer(lhs), RV::BigInt(rhs)) => Value::bigint(BigInt::from(lhs).sub(rhs)),
        (RV::BigInt(lhs), RV::BigInt(rhs)) => Value::bigint(lhs.sub(rhs)),
        (RV::Integer(lhs), RV::Float(rhs)) => Value::float((lhs as f64).sub(&rhs)),
        (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs.sub(&(rhs as f64))),
        (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs.sub(&rhs)),
        (RV::Object(lhs), RV::Object(rhs)) => match (&lhs.kind, &rhs.kind) {
            (ObjKind::Time(lhs), ObjKind::Time(rhs)) => Value::float((*lhs - *rhs).as_secs_f64()),
            _ => {
                globals.error = Some(MonorubyErr::method_not_found(IdentId::_SUB));
                return None;
            }
        },
        _ => {
            globals.error = Some(MonorubyErr::method_not_found(IdentId::_SUB));
            return None;
        }
    };
    Some(v)
}

pub(super) extern "C" fn div_values(
    _interp: &mut Interp,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    let v = match (lhs.unpack(), rhs.unpack()) {
        (RV::Integer(lhs), RV::Integer(rhs)) => {
            if rhs.is_zero() {
                globals.error = Some(MonorubyErr::divide_by_zero());
                return None;
            }
            Value::integer(lhs.div_floor(rhs))
        }
        (RV::Integer(lhs), RV::BigInt(rhs)) => {
            if rhs.is_zero() {
                globals.error = Some(MonorubyErr::divide_by_zero());
                return None;
            }
            Value::bigint(BigInt::from(lhs).div_floor(rhs))
        }
        (RV::Integer(lhs), RV::Float(rhs)) => {
            if rhs.is_zero() {
                globals.error = Some(MonorubyErr::divide_by_zero());
                return None;
            }
            Value::float((lhs as f64).div(&rhs))
        }
        (RV::BigInt(lhs), RV::Integer(rhs)) => {
            if rhs.is_zero() {
                globals.error = Some(MonorubyErr::divide_by_zero());
                return None;
            }
            Value::bigint(lhs.div_floor(&BigInt::from(rhs)))
        }
        (RV::BigInt(lhs), RV::BigInt(rhs)) => {
            if rhs.is_zero() {
                globals.error = Some(MonorubyErr::divide_by_zero());
                return None;
            }
            Value::bigint(lhs.div_floor(rhs))
        }
        (RV::BigInt(lhs), RV::Float(rhs)) => {
            if rhs.is_zero() {
                globals.error = Some(MonorubyErr::divide_by_zero());
                return None;
            }
            Value::float((lhs.to_f64().unwrap()).div(&rhs))
        }
        (RV::Float(lhs), RV::Integer(rhs)) => {
            if rhs.is_zero() {
                globals.error = Some(MonorubyErr::divide_by_zero());
                return None;
            }
            Value::float(lhs.div(&(rhs as f64)))
        }
        (RV::Float(lhs), RV::BigInt(rhs)) => {
            if rhs.is_zero() {
                globals.error = Some(MonorubyErr::divide_by_zero());
                return None;
            }
            Value::float(lhs.div(&rhs.to_f64().unwrap()))
        }
        (RV::Float(lhs), RV::Float(rhs)) => {
            if rhs.is_zero() {
                globals.error = Some(MonorubyErr::divide_by_zero());
                return None;
            }
            Value::float(lhs.div(&rhs))
        }
        _ => {
            globals.error = Some(MonorubyErr::method_not_found(IdentId::_DIV));
            return None;
        }
    };
    Some(v)
}

macro_rules! int_binop_values {
    (($op:ident, $op_str:expr)) => {
        paste! {
            pub(super) extern "C" fn [<$op _values>](
                _interp: &mut Interp,
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
                        globals.error = Some(MonorubyErr::method_not_found($op_str));
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

int_binop_values!(
    (bitor, IdentId::_BOR),
    (bitand, IdentId::_BAND),
    (bitxor, IdentId::_BXOR)
);

pub(super) extern "C" fn shr_values(
    _interp: &mut Interp,
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
            globals.error = Some(MonorubyErr::method_not_found(IdentId::_SHR));
            return None;
        }
    };
    Some(v)
}

pub(super) extern "C" fn shl_values(
    _interp: &mut Interp,
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
            globals.error = Some(MonorubyErr::method_not_found(IdentId::_SHL));
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
    _interp: &mut Interp,
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
            globals.error = Some(MonorubyErr::method_not_found(IdentId::_UMINUS));
            return None;
        }
    };
    Some(v)
}

use dtoa;

pub extern "C" fn concatenate_string(globals: &Globals, arg: *mut Value, len: usize) -> Value {
    let mut res = vec![];
    for i in 0..len {
        let v = unsafe { *arg.sub(i) };
        match v.unpack() {
            RV::Nil => res.extend("nil".bytes()),
            RV::Bool(b) => res.extend(format!("{}", b).bytes()),
            RV::Integer(i) => res.extend(format!("{}", i).bytes()),
            RV::Float(f) => res.extend(format!("{}", dtoa::Buffer::new().format(f)).bytes()),
            RV::BigInt(b) => res.extend(format!("{}", b).bytes()),
            RV::Symbol(sym) => res.extend(globals.get_ident_name(sym).bytes()),
            RV::String(b) => res.extend(b),
            _ => unimplemented!(),
        };
    }
    Value::string(res)
}

pub extern "C" fn vm_get_constant(
    _interp: &mut Interp,
    globals: &mut Globals,
    site_id: ConstSiteId,
    const_version: usize,
) -> Option<Value> {
    let ConstSiteInfo {
        name,
        prefix,
        toplevel,
        cache: (cached_version, val),
    } = &globals.func[site_id];
    assert_eq!(0, prefix.len());
    assert!(!toplevel);
    if *cached_version == const_version {
        return *val;
    };
    let res = match globals.get_constant(*name) {
        Some(v) => Some(v),
        None => {
            globals.error = Some(MonorubyErr::uninitialized_constant(*name));
            None
        }
    };
    globals.func[site_id].cache = (const_version, res);
    res
}

pub extern "C" fn get_constant(
    _interp: &mut Interp,
    globals: &mut Globals,
    site_id: ConstSiteId,
) -> Option<Value> {
    let ConstSiteInfo { name, .. } = &globals.func[site_id];
    let res = match globals.get_constant(*name) {
        Some(v) => Some(v),
        None => {
            globals.error = Some(MonorubyErr::uninitialized_constant(*name));
            None
        }
    };
    res
}

pub extern "C" fn set_constant(
    _interp: &mut Interp,
    globals: &mut Globals,
    name: IdentId,
    val: Value,
    const_version: &mut usize,
) {
    *const_version += 1;
    if globals.set_constant(name, val).is_some() && globals.warning >= 1 {
        eprintln!(
            "warning: already initialized constant {}",
            globals.get_ident_name(name)
        )
    }
}
