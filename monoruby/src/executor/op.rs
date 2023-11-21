use super::*;

mod sort;

use num::{BigInt, Integer, ToPrimitive, Zero};
use paste::paste;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Shl, Shr, Sub};

//
// Generic operations.
//

pub extern "C" fn i64_to_value(i: i64) -> Value {
    Value::integer(i)
}

macro_rules! binop_values {
    (($op:ident, $op_str:expr)) => {
        paste! {
            pub(crate) extern "C" fn [<$op _values>](
                vm: &mut Executor,
                globals: &mut Globals,
                lhs: Value,
                rhs: Value
            ) -> Option<Value> {
                let v = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Fixnum(lhs), RV::Fixnum(rhs)) => match lhs.[<checked_ $op>](rhs){
                        Some(res) => Value::integer(res),
                        None => Value::bigint(BigInt::from(lhs).$op(BigInt::from(rhs))),
                    }
                    (RV::BigInt(lhs), RV::Fixnum(rhs)) => Value::bigint(lhs.$op(BigInt::from(rhs))),
                    (RV::Float(lhs), RV::Fixnum(rhs)) => Value::float(lhs.$op(&(rhs as f64))),

                    (RV::Fixnum(lhs), RV::BigInt(rhs)) => Value::bigint(BigInt::from(lhs).$op(rhs)),
                    (RV::BigInt(lhs), RV::BigInt(rhs)) => Value::bigint(lhs.$op(rhs)),
                    (RV::Float(lhs), RV::BigInt(rhs)) => Value::float(lhs.$op(rhs.to_f64().unwrap())),

                    (RV::Fixnum(lhs), RV::Float(rhs)) => Value::float((lhs as f64).$op(&rhs)),
                    (RV::BigInt(lhs), RV::Float(rhs)) => Value::float(lhs.to_f64().unwrap().$op(&rhs)),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs.$op(&rhs)),
                    (RV::Fixnum(_), _) | (RV::BigInt(_), _) | (RV::Float(_), _) => {
                        let err = MonorubyErr::no_implicit_conversion(&globals, rhs, INTEGER_CLASS);
                        vm.set_error(err);
                        return None;
                    }
                    _ => {
                        return vm.invoke_method(globals, $op_str, lhs, &[rhs], None);
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

pub(crate) extern "C" fn pow_ii(lhs: i64, rhs: i64) -> Value {
    if let Ok(rhs) = i32::try_from(rhs) {
        if rhs < 0 {
            unimplemented!("a**b: b<0 in not supported yet.")
        }
        let rhs = rhs as u32;
        match lhs.checked_pow(rhs) {
            Some(res) => Value::integer(res),
            None => Value::bigint(BigInt::from(lhs).pow(rhs)),
        }
    } else {
        Value::float(f64::INFINITY)
    }
}

fn pow_ff(lhs: f64, rhs: f64) -> Value {
    Value::float(lhs.powf(rhs))
}

pub(crate) extern "C" fn pow_ff_f(lhs: f64, rhs: f64) -> f64 {
    lhs.powf(rhs)
}

// TODO: support rhs < 0.
pub(crate) extern "C" fn pow_values(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    let v = match (lhs.unpack(), rhs.unpack()) {
        (RV::Fixnum(lhs), RV::Fixnum(rhs)) => pow_ii(lhs, rhs),
        (RV::Fixnum(lhs), RV::BigInt(rhs)) => {
            if let Ok(rhs) = rhs.try_into() {
                Value::bigint(BigInt::from(lhs).pow(rhs))
            } else {
                Value::float(f64::INFINITY)
            }
        }
        (RV::Fixnum(lhs), RV::Float(rhs)) => pow_ff(lhs as f64, rhs),
        (RV::BigInt(lhs), RV::Fixnum(rhs)) => {
            if let Ok(rhs) = i32::try_from(rhs) {
                let rhs = rhs as u32;
                Value::bigint(lhs.pow(rhs))
            } else {
                Value::float(f64::INFINITY)
            }
        }
        (RV::BigInt(lhs), RV::BigInt(rhs)) => {
            if let Ok(rhs) = rhs.try_into() {
                Value::bigint(lhs.pow(rhs))
            } else {
                Value::float(f64::INFINITY)
            }
        }
        (RV::BigInt(lhs), RV::Float(rhs)) => pow_ff(lhs.to_f64().unwrap(), rhs),
        (RV::Float(lhs), RV::Fixnum(rhs)) => {
            //if let Ok(rhs) = i32::try_from(rhs) {
            pow_ff(lhs, rhs as f64)
            //} else {
            //    Value::new_float(lhs.powf(rhs as f64))
            //}
        }
        (RV::Float(lhs), RV::BigInt(rhs)) => pow_ff(lhs, rhs.to_f64().unwrap()),
        (RV::Float(lhs), RV::Float(rhs)) => pow_ff(lhs, rhs),
        _ => {
            return vm.invoke_method(globals, IdentId::_POW, lhs, &[rhs], None);
        }
    };
    Some(v)
}

binop_values!(
    (add, IdentId::_ADD),
    (sub, IdentId::_SUB),
    (mul, IdentId::_MUL),
    (rem, IdentId::_REM)
);

pub(crate) extern "C" fn div_values(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    let v = match (lhs.unpack(), rhs.unpack()) {
        (RV::Fixnum(lhs), RV::Fixnum(rhs)) => {
            if rhs.is_zero() {
                vm.err_divide_by_zero();
                return None;
            }
            Value::integer(lhs.div_floor(rhs))
        }
        (RV::Fixnum(lhs), RV::BigInt(rhs)) => {
            if rhs.is_zero() {
                vm.err_divide_by_zero();
                return None;
            }
            Value::bigint(BigInt::from(lhs).div_floor(rhs))
        }
        (RV::Fixnum(lhs), RV::Float(rhs)) => Value::float((lhs as f64).div(&rhs)),
        (RV::BigInt(lhs), RV::Fixnum(rhs)) => {
            if rhs.is_zero() {
                vm.err_divide_by_zero();
                return None;
            }
            Value::bigint(lhs.div_floor(&BigInt::from(rhs)))
        }
        (RV::BigInt(lhs), RV::BigInt(rhs)) => {
            if rhs.is_zero() {
                vm.err_divide_by_zero();
                return None;
            }
            Value::bigint(lhs.div_floor(rhs))
        }
        (RV::BigInt(lhs), RV::Float(rhs)) => Value::float((lhs.to_f64().unwrap()).div(&rhs)),
        (RV::Float(lhs), RV::Fixnum(rhs)) => Value::float(lhs.div(&(rhs as f64))),
        (RV::Float(lhs), RV::BigInt(rhs)) => Value::float(lhs.div(&rhs.to_f64().unwrap())),
        (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs.div(&rhs)),
        _ => {
            return vm.invoke_method(globals, IdentId::_DIV, lhs, &[rhs], None);
        }
    };
    Some(v)
}

macro_rules! int_binop_values {
    (($op:ident, $op_str:expr)) => {
        paste! {
            pub(crate) extern "C" fn [<$op _values>](
                vm: &mut Executor,
                globals: &mut Globals,
                lhs: Value,
                rhs: Value
            ) -> Option<Value> {
                let v = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Fixnum(lhs), RV::Fixnum(rhs)) => Value::integer(lhs.$op(&rhs)),
                    (RV::Fixnum(lhs), RV::BigInt(rhs)) => Value::bigint(BigInt::from(lhs).$op(rhs)),
                    (RV::BigInt(lhs), RV::Fixnum(rhs)) => Value::bigint(lhs.$op(BigInt::from(rhs))),
                    (RV::BigInt(lhs), RV::BigInt(rhs)) => Value::bigint(lhs.$op(rhs)),
                    _ => {
                        return vm.invoke_method(globals, $op_str, lhs, &[rhs], None);
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

pub(crate) extern "C" fn shr_values(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    let v = match (lhs.unpack(), rhs.unpack()) {
        (RV::Fixnum(lhs), RV::Fixnum(rhs)) => {
            if rhs >= 0 {
                int_shr(lhs, rhs as u64 as u32)
            } else {
                int_shl(lhs, -rhs as u64 as u32)
            }
        }
        (RV::BigInt(lhs), RV::Fixnum(rhs)) => {
            if rhs >= 0 {
                bigint_shr(lhs, rhs as u64 as u32)
            } else {
                bigint_shl(lhs, -rhs as u64 as u32)
            }
        }
        _ => {
            return vm.invoke_method(globals, IdentId::_SHR, lhs, &[rhs], None);
        }
    };
    Some(v)
}

pub(crate) extern "C" fn shl_values(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    let v = match (lhs.unpack(), rhs.unpack()) {
        (RV::Fixnum(lhs), RV::Fixnum(rhs)) => {
            if rhs >= 0 {
                int_shl(lhs, rhs as u64 as u32)
            } else {
                int_shr(lhs, -rhs as u64 as u32)
            }
        }
        (RV::BigInt(lhs), RV::Fixnum(rhs)) => {
            if rhs >= 0 {
                bigint_shl(lhs, rhs as u64 as u32)
            } else {
                bigint_shr(lhs, -rhs as u64 as u32)
            }
        }
        _ => {
            return vm.invoke_method(globals, IdentId::_SHL, lhs, &[rhs], None);
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
    (($op:ident, $op_str:expr)) => {
        paste! {
            pub(crate) extern "C" fn [<cmp_ $op _values>](
                vm: &mut Executor,
                globals: &mut Globals,
                lhs: Value,
                rhs: Value
            ) -> Option<Value> {
                let b = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Fixnum(lhs), RV::Fixnum(rhs)) => lhs.$op(&rhs),
                    (RV::Fixnum(lhs), RV::BigInt(rhs)) => BigInt::from(lhs).$op(&rhs),
                    (RV::Fixnum(lhs), RV::Float(rhs)) => (lhs as f64).$op(&rhs),
                    (RV::BigInt(lhs), RV::Fixnum(rhs)) => lhs.$op(&BigInt::from(rhs)),
                    (RV::BigInt(lhs), RV::BigInt(rhs)) => lhs.$op(&rhs),
                    (RV::BigInt(lhs), RV::Float(rhs)) => lhs.to_f64().unwrap().$op(&rhs),
                    (RV::Float(lhs), RV::Fixnum(rhs)) => lhs.$op(&(rhs as f64)),
                    (RV::Float(lhs), RV::BigInt(rhs)) => lhs.$op(&(rhs.to_f64().unwrap())),
                    (RV::Float(lhs), RV::Float(rhs)) => lhs.$op(&rhs),
                    _ => {
                        return vm.invoke_method(globals, $op_str, lhs, &[rhs], None);
                    }
                };
                Some(Value::bool(b))
            }
        }
    };
    (($op1:ident, $op_str1:expr), $(($op2:ident, $op_str2:expr)),+) => {
        cmp_values!(($op1, $op_str1));
        cmp_values!($(($op2, $op_str2)),+);
    };
}

cmp_values!(
    (ge, IdentId::_GE),
    (gt, IdentId::_GT),
    (le, IdentId::_LE),
    (lt, IdentId::_LT)
);

impl Executor {
    pub(crate) fn eq_values_bool(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<bool> {
        let b = match (lhs.unpack(), rhs.unpack()) {
            (RV::Nil, RV::Nil) => true,
            (RV::Nil, _) => false,
            (RV::Fixnum(lhs), RV::Fixnum(rhs)) => lhs.eq(&rhs),
            (RV::Fixnum(lhs), RV::BigInt(rhs)) => BigInt::from(lhs).eq(&rhs),
            (RV::Fixnum(lhs), RV::Float(rhs)) => (lhs as f64).eq(&rhs),
            (RV::Fixnum(_), _) => false,
            (RV::BigInt(lhs), RV::Fixnum(rhs)) => lhs.eq(&BigInt::from(rhs)),
            (RV::BigInt(lhs), RV::BigInt(rhs)) => lhs.eq(&rhs),
            (RV::BigInt(lhs), RV::Float(rhs)) => lhs.to_f64().unwrap().eq(&rhs),
            (RV::BigInt(_), _) => false,
            (RV::Float(lhs), RV::Fixnum(rhs)) => lhs.eq(&(rhs as f64)),
            (RV::Float(lhs), RV::BigInt(rhs)) => lhs.eq(&(rhs.to_f64().unwrap())),
            (RV::Float(lhs), RV::Float(rhs)) => lhs.eq(&rhs),
            (RV::Float(_), _) => false,
            (RV::Bool(lhs), RV::Bool(rhs)) => lhs.eq(&rhs),
            (RV::Bool(_), _) => false,
            (RV::Symbol(lhs), RV::Symbol(rhs)) => lhs.eq(&rhs),
            (RV::Symbol(_), _) => false,
            (RV::String(lhs), RV::String(rhs)) => lhs.eq(rhs),
            (RV::String(_), _) => false,
            _ => self
                .invoke_method_inner(globals, IdentId::_EQ, lhs, &[rhs], None)?
                .as_bool(),
        };
        Ok(b)
    }

    pub(crate) fn ne_values_bool(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<bool> {
        Ok(!self.eq_values_bool(globals, lhs, rhs)?)
    }
}

macro_rules! eq_values {
    ($op:ident) => {
        paste! {
            pub(crate) extern "C" fn [<cmp_ $op _values>](
                vm: &mut Executor,
                globals: &mut Globals,
                lhs: Value,
                rhs: Value
            ) -> Option<Value> {
                match vm.[<$op _values_bool>](globals, lhs, rhs) {
                    Ok(b) => Some(Value::bool(b)),
                    Err(err) => {
                        vm.set_error(err);
                        None
                    }
                }
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        eq_values!($op1);
        eq_values!($($op2),+);
    };
}

eq_values!(eq, ne);

#[test]
fn cmp_values() {
    let mut globals = Globals::new(0, false);
    let mut vm = Executor::default();
    let pairs = [
        (Value::nil(), Value::nil(), true),
        (Value::nil(), Value::integer(100), false),
        (Value::nil(), Value::float(100.0), false),
        (Value::bool(true), Value::bool(true), true),
        (Value::bool(true), Value::bool(false), false),
        (Value::bool(true), Value::nil(), false),
        (Value::integer(100), Value::integer(100), true),
        (Value::integer(100), Value::integer(200), false),
        (Value::integer(100), Value::float(100.0), true),
        (Value::integer(100), Value::float(200.0), false),
        (Value::integer(100), Value::nil(), false),
        (Value::integer(100), Value::bool(true), false),
        (Value::integer(100), Value::bool(false), false),
        (Value::integer(100), Value::symbol(IdentId::TO_S), false),
        (Value::integer(100), Value::string_from_str("100"), false),
        (
            Value::symbol(IdentId::TO_S),
            Value::symbol(IdentId::TO_S),
            true,
        ),
        (
            Value::symbol(IdentId::TO_S),
            Value::symbol(IdentId::NAME),
            false,
        ),
    ];
    for (lhs, rhs, ans) in pairs {
        assert_eq!(
            ans,
            Executor::eq_values_bool(&mut vm, &mut globals, lhs, rhs).unwrap()
        );
        assert_eq!(
            ans,
            !Executor::ne_values_bool(&mut vm, &mut globals, lhs, rhs).unwrap()
        );
    }
}

pub(crate) extern "C" fn cmp_teq_values(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    let b = match (lhs.unpack(), rhs.unpack()) {
        (RV::Nil, RV::Nil) => true,
        (RV::Nil, _) => false,
        (RV::Symbol(lhs), RV::Symbol(rhs)) => lhs == rhs,
        (RV::Symbol(_), _) => false,
        (RV::Bool(lhs), RV::Bool(rhs)) => lhs == rhs,
        (RV::Bool(_), _) => false,
        (RV::Fixnum(lhs), RV::Fixnum(rhs)) => lhs.eq(&rhs),
        (RV::Fixnum(lhs), RV::BigInt(rhs)) => BigInt::from(lhs).eq(rhs),
        (RV::Fixnum(lhs), RV::Float(rhs)) => (lhs as f64).eq(&rhs),
        (RV::BigInt(lhs), RV::Fixnum(rhs)) => lhs.eq(&BigInt::from(rhs)),
        (RV::BigInt(lhs), RV::BigInt(rhs)) => lhs.eq(rhs),
        (RV::BigInt(lhs), RV::Float(rhs)) => lhs.to_f64().unwrap().eq(&rhs),
        (RV::Float(lhs), RV::Fixnum(rhs)) => lhs.eq(&(rhs as f64)),
        (RV::Float(lhs), RV::BigInt(rhs)) => lhs.eq(&(rhs.to_f64().unwrap())),
        (RV::Float(lhs), RV::Float(rhs)) => lhs.eq(&rhs),
        _ => {
            return vm.invoke_method(globals, IdentId::_TEQ, lhs, &[rhs], None);
        }
    };
    Some(Value::bool(b))
}

pub(crate) fn cmp_teq_values_bool(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Result<bool> {
    let b = match (lhs.unpack(), rhs.unpack()) {
        (RV::Nil, RV::Nil) => true,
        (RV::Nil, _) => false,
        (RV::Symbol(lhs), RV::Symbol(rhs)) => lhs == rhs,
        (RV::Symbol(_), _) => false,
        (RV::Bool(lhs), RV::Bool(rhs)) => lhs == rhs,
        (RV::Bool(_), _) => false,
        (RV::Fixnum(lhs), RV::Fixnum(rhs)) => lhs.eq(&rhs),
        (RV::Fixnum(lhs), RV::BigInt(rhs)) => BigInt::from(lhs).eq(rhs),
        (RV::Fixnum(lhs), RV::Float(rhs)) => (lhs as f64).eq(&rhs),
        (RV::BigInt(lhs), RV::Fixnum(rhs)) => lhs.eq(&BigInt::from(rhs)),
        (RV::BigInt(lhs), RV::BigInt(rhs)) => lhs.eq(rhs),
        (RV::BigInt(lhs), RV::Float(rhs)) => lhs.to_f64().unwrap().eq(&rhs),
        (RV::Float(lhs), RV::Fixnum(rhs)) => lhs.eq(&(rhs as f64)),
        (RV::Float(lhs), RV::BigInt(rhs)) => lhs.eq(&(rhs.to_f64().unwrap())),
        (RV::Float(lhs), RV::Float(rhs)) => lhs.eq(&rhs),
        _ => {
            return vm
                .invoke_method_inner(globals, IdentId::_TEQ, lhs, &[rhs], None)
                .map(|v| v.as_bool());
        }
    };
    Ok(b)
}

pub(crate) extern "C" fn cmp_cmp_values(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    match vm.cmp_cmp_values_inner(globals, lhs, rhs) {
        Ok(val) => Some(val),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

impl Executor {
    pub(crate) fn cmp_cmp_values_inner(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<Value> {
        let ord = self.compare_values(globals, lhs, rhs)?;
        Ok(Value::from_ord(ord))
    }

    pub(crate) fn compare_values(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<std::cmp::Ordering> {
        use std::cmp::Ordering;
        let res = match (lhs.unpack(), rhs.unpack()) {
            (RV::Nil, RV::Nil) => Some(Ordering::Equal),
            (RV::Nil, _) => None,
            (RV::Symbol(lhs), RV::Symbol(rhs)) => Some(lhs.compare(&rhs)),
            (RV::Symbol(_), _) => None,
            (RV::Bool(lhs), RV::Bool(rhs)) if lhs == rhs => Some(Ordering::Equal),
            (RV::Bool(_), _) => None,
            (RV::Fixnum(lhs), RV::Fixnum(rhs)) => Some(lhs.cmp(&rhs)),
            (RV::Fixnum(lhs), RV::BigInt(rhs)) => Some(BigInt::from(lhs).cmp(rhs)),
            (RV::Fixnum(lhs), RV::Float(rhs)) => (lhs as f64).partial_cmp(&rhs),
            (RV::BigInt(lhs), RV::Fixnum(rhs)) => lhs.partial_cmp(&BigInt::from(rhs)),
            (RV::BigInt(lhs), RV::BigInt(rhs)) => Some(lhs.cmp(&rhs)),
            (RV::BigInt(lhs), RV::Float(rhs)) => lhs.to_f64().unwrap().partial_cmp(&rhs),
            (RV::Float(lhs), RV::Fixnum(rhs)) => lhs.partial_cmp(&(rhs as f64)),
            (RV::Float(lhs), RV::BigInt(rhs)) => lhs.partial_cmp(&(rhs.to_f64().unwrap())),
            (RV::Float(lhs), RV::Float(rhs)) => lhs.partial_cmp(&rhs),
            _ => {
                if let Some(i) = self
                    .invoke_method_inner(globals, IdentId::_CMP, lhs, &[rhs], None)?
                    .try_fixnum()
                {
                    match i {
                        -1 => Some(std::cmp::Ordering::Less),
                        0 => Some(std::cmp::Ordering::Equal),
                        1 => Some(std::cmp::Ordering::Greater),
                        _ => None,
                    }
                } else {
                    None
                }
            }
        }
        .ok_or_else(|| {
            let lhs = lhs.get_real_class_name(globals);
            let rhs = globals.to_s(rhs);
            MonorubyErr::argumenterr(format!("comparison of {lhs} with {rhs} failed"))
        })?;
        Ok(res)
    }
}

pub(crate) extern "C" fn neg_value(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
) -> Option<Value> {
    let v = match lhs.unpack() {
        RV::Fixnum(lhs) => match lhs.checked_neg() {
            Some(lhs) => Value::integer(lhs),
            None => Value::bigint(-BigInt::from(lhs)),
        },
        RV::Float(lhs) => Value::float(-lhs),
        RV::BigInt(lhs) => Value::bigint(-lhs),
        _ => {
            return vm.invoke_method(globals, IdentId::_UMINUS, lhs, &[], None);
        }
    };
    Some(v)
}

pub(crate) extern "C" fn pos_value(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
) -> Option<Value> {
    let v = match lhs.unpack() {
        RV::Fixnum(lhs) => Value::integer(lhs),
        RV::Float(lhs) => Value::float(lhs),
        RV::BigInt(lhs) => Value::bigint(lhs.clone()),
        _ => {
            return vm.invoke_method(globals, IdentId::get_id("@+"), lhs, &[], None);
        }
    };
    Some(v)
}

pub(crate) extern "C" fn bitnot_value(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
) -> Option<Value> {
    let v = match lhs.unpack() {
        RV::Fixnum(lhs) => Value::integer(!lhs),
        RV::BigInt(lhs) => Value::bigint(!lhs),
        _ => {
            return vm.invoke_method(globals, IdentId::get_id("~"), lhs, &[], None);
        }
    };
    Some(v)
}

pub(crate) fn integer_index1(globals: &Globals, base: Value, index: Value) -> Result<Value> {
    // we must support Integer#[Range].
    match (base.unpack(), index.unpack()) {
        (RV::Fixnum(base), RV::Fixnum(index)) => {
            let val = if index < 0 {
                0
            } else if index > 63 {
                base.is_negative().into()
            } else {
                (base >> index) & 1
            };
            Ok(Value::integer(val))
        }
        (RV::Fixnum(_), RV::BigInt(_)) => Ok(Value::integer(0)),
        (RV::Fixnum(_), _) => Err(MonorubyErr::no_implicit_conversion(
            globals,
            index,
            INTEGER_CLASS,
        )),
        (RV::BigInt(base), RV::Fixnum(index)) => {
            if index < 0 {
                Ok(Value::integer(0))
            } else {
                let i = (base >> index) & num::BigInt::from(1);
                Ok(Value::bigint(i))
            }
        }
        (RV::BigInt(_), RV::BigInt(_)) => Ok(Value::integer(0)),
        (RV::BigInt(_), _) => Err(MonorubyErr::no_implicit_conversion(
            globals,
            index,
            INTEGER_CLASS,
        )),
        _ => unreachable!(),
    }
}

pub extern "C" fn expand_splat(src: Value, dst: *mut Value) -> usize {
    expand_splat_inner(src, dst)
}

pub extern "C" fn vm_expand_splat(
    src: *const Value,
    mut dst: *mut Value,
    len: usize,
    globals: &Globals,
    callid: CallSiteId,
) -> usize {
    let mut dst_len = 0;
    unsafe {
        let splat_pos = &globals.store[callid].splat_pos;
        for i in 0..len {
            let v = *src.sub(i);
            if splat_pos.contains(&i) {
                let ofs = expand_splat_inner(v, dst);
                dst_len += ofs;
                dst = dst.sub(ofs);
            } else {
                *dst = v;
                dst = dst.sub(1);
                dst_len += 1;
            }
        }
    }
    dst_len
}

fn expand_splat_inner(src: Value, dst: *mut Value) -> usize {
    if let Some(ary) = src.is_array() {
        let len = ary.len();
        for i in 0..len {
            unsafe { *dst.sub(i) = ary[i] };
        }
        len
    } else if let Some(_range) = src.is_range() {
        unimplemented!()
    } else if let Some(_hash) = src.is_hash() {
        unimplemented!()
    } else {
        unsafe { *dst = src };
        1
    }
}

pub extern "C" fn block_expand_array(src: Value, dst: *mut Value, min_len: usize) -> usize {
    let ary: Array = src.into();
    let len = ary.len();
    if min_len <= len {
        for i in 0..len {
            unsafe { *dst.sub(i) = ary[i] }
        }
        len
    } else {
        for i in 0..len {
            unsafe { *dst.sub(i) = ary[i] }
        }
        for i in len..min_len {
            unsafe { *dst.sub(i) = Value::nil() }
        }
        min_len
    }
}
