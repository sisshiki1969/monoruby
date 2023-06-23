use super::*;

use num::{BigInt, Integer, ToPrimitive, Zero};
use paste::paste;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Shl, Shr, Sub};

//
// Generic operations.
//

macro_rules! binop_values {
    (($op:ident, $op_str:expr)) => {
        paste! {
            pub(super) extern "C" fn [<$op _values>](
                vm: &mut Executor,
                globals: &mut Globals,
                lhs: Value,
                rhs: Value
            ) -> Option<Value> {
                let v = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => match lhs.[<checked_ $op>](rhs){
                        Some(res) => Value::new_integer(res),
                        None => Value::new_bigint(BigInt::from(lhs).$op(BigInt::from(rhs))),
                    }
                    (RV::BigInt(lhs), RV::Integer(rhs)) => Value::new_bigint(lhs.$op(BigInt::from(rhs))),
                    (RV::Float(lhs), RV::Integer(rhs)) => Value::new_float(lhs.$op(&(rhs as f64))),

                    (RV::Integer(lhs), RV::BigInt(rhs)) => Value::new_bigint(BigInt::from(lhs).$op(rhs)),
                    (RV::BigInt(lhs), RV::BigInt(rhs)) => Value::new_bigint(lhs.$op(rhs)),
                    (RV::Float(lhs), RV::BigInt(rhs)) => Value::new_float(lhs.$op(rhs.to_f64().unwrap())),

                    (RV::Integer(lhs), RV::Float(rhs)) => Value::new_float((lhs as f64).$op(&rhs)),
                    (RV::BigInt(lhs), RV::Float(rhs)) => Value::new_float(lhs.to_f64().unwrap().$op(&rhs)),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::new_float(lhs.$op(&rhs)),
                    (RV::Integer(_), _) | (RV::BigInt(_), _) | (RV::Float(_), _) => {
                        let err = MonorubyErr::no_implicit_conversion(&globals, rhs, INTEGER_CLASS);
                        vm.set_error(err);
                        return None;
                    }
                    _ => {
                        return vm.invoke_method(globals, $op_str, lhs, &[rhs]);
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

pub(super) extern "C" fn pow_ii(lhs: i64, rhs: i64) -> Value {
    if let Ok(rhs) = i32::try_from(rhs) {
        if rhs < 0 {
            unimplemented!("a**b: b<0 in not supported yet.")
        }
        let rhs = rhs as u32;
        match lhs.checked_pow(rhs) {
            Some(res) => Value::new_integer(res),
            None => Value::new_bigint(BigInt::from(lhs).pow(rhs)),
        }
    } else {
        Value::new_float(f64::INFINITY)
    }
}

fn pow_ff(lhs: f64, rhs: f64) -> Value {
    Value::new_float(lhs.powf(rhs))
}

pub(super) extern "C" fn pow_ff_f(lhs: f64, rhs: f64) -> f64 {
    lhs.powf(rhs)
}

// TODO: support rhs < 0.
pub(super) extern "C" fn pow_values(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    let v = match (lhs.unpack(), rhs.unpack()) {
        (RV::Integer(lhs), RV::Integer(rhs)) => pow_ii(lhs, rhs),
        (RV::Integer(lhs), RV::BigInt(rhs)) => {
            if let Ok(rhs) = rhs.try_into() {
                Value::new_bigint(BigInt::from(lhs).pow(rhs))
            } else {
                Value::new_float(f64::INFINITY)
            }
        }
        (RV::Integer(lhs), RV::Float(rhs)) => pow_ff(lhs as f64, rhs),
        (RV::BigInt(lhs), RV::Integer(rhs)) => {
            if let Ok(rhs) = i32::try_from(rhs) {
                let rhs = rhs as u32;
                Value::new_bigint(lhs.pow(rhs))
            } else {
                Value::new_float(f64::INFINITY)
            }
        }
        (RV::BigInt(lhs), RV::BigInt(rhs)) => {
            if let Ok(rhs) = rhs.try_into() {
                Value::new_bigint(lhs.pow(rhs))
            } else {
                Value::new_float(f64::INFINITY)
            }
        }
        (RV::BigInt(lhs), RV::Float(rhs)) => pow_ff(lhs.to_f64().unwrap(), rhs),
        (RV::Float(lhs), RV::Integer(rhs)) => {
            //if let Ok(rhs) = i32::try_from(rhs) {
            pow_ff(lhs, rhs as f64)
            //} else {
            //    Value::new_float(lhs.powf(rhs as f64))
            //}
        }
        (RV::Float(lhs), RV::BigInt(rhs)) => pow_ff(lhs, rhs.to_f64().unwrap()),
        (RV::Float(lhs), RV::Float(rhs)) => pow_ff(lhs, rhs),
        _ => {
            return vm.invoke_method(globals, IdentId::_POW, lhs, &[rhs]);
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

pub(super) extern "C" fn div_values(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    let v = match (lhs.unpack(), rhs.unpack()) {
        (RV::Integer(lhs), RV::Integer(rhs)) => {
            if rhs.is_zero() {
                vm.err_divide_by_zero();
                return None;
            }
            Value::new_integer(lhs.div_floor(rhs))
        }
        (RV::Integer(lhs), RV::BigInt(rhs)) => {
            if rhs.is_zero() {
                vm.err_divide_by_zero();
                return None;
            }
            Value::new_bigint(BigInt::from(lhs).div_floor(rhs))
        }
        (RV::Integer(lhs), RV::Float(rhs)) => Value::new_float((lhs as f64).div(&rhs)),
        (RV::BigInt(lhs), RV::Integer(rhs)) => {
            if rhs.is_zero() {
                vm.err_divide_by_zero();
                return None;
            }
            Value::new_bigint(lhs.div_floor(&BigInt::from(rhs)))
        }
        (RV::BigInt(lhs), RV::BigInt(rhs)) => {
            if rhs.is_zero() {
                vm.err_divide_by_zero();
                return None;
            }
            Value::new_bigint(lhs.div_floor(rhs))
        }
        (RV::BigInt(lhs), RV::Float(rhs)) => Value::new_float((lhs.to_f64().unwrap()).div(&rhs)),
        (RV::Float(lhs), RV::Integer(rhs)) => Value::new_float(lhs.div(&(rhs as f64))),
        (RV::Float(lhs), RV::BigInt(rhs)) => Value::new_float(lhs.div(&rhs.to_f64().unwrap())),
        (RV::Float(lhs), RV::Float(rhs)) => Value::new_float(lhs.div(&rhs)),
        _ => {
            return vm.invoke_method(globals, IdentId::_DIV, lhs, &[rhs]);
        }
    };
    Some(v)
}

macro_rules! int_binop_values {
    (($op:ident, $op_str:expr)) => {
        paste! {
            pub(super) extern "C" fn [<$op _values>](
                vm: &mut Executor,
                globals: &mut Globals,
                lhs: Value,
                rhs: Value
            ) -> Option<Value> {
                let v = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::new_integer(lhs.$op(&rhs)),
                    (RV::Integer(lhs), RV::BigInt(rhs)) => Value::new_bigint(BigInt::from(lhs).$op(rhs)),
                    (RV::BigInt(lhs), RV::Integer(rhs)) => Value::new_bigint(lhs.$op(BigInt::from(rhs))),
                    (RV::BigInt(lhs), RV::BigInt(rhs)) => Value::new_bigint(lhs.$op(rhs)),
                    _ => {
                        return vm.invoke_method(globals, $op_str, lhs, &[rhs]);
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
    vm: &mut Executor,
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
        _ => {
            return vm.invoke_method(globals, IdentId::_SHR, lhs, &[rhs]);
        }
    };
    Some(v)
}

pub(super) extern "C" fn shl_values(
    vm: &mut Executor,
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
        _ => {
            return vm.invoke_method(globals, IdentId::_SHL, lhs, &[rhs]);
        }
    };
    Some(v)
}

fn int_shr(lhs: i64, rhs: u32) -> Value {
    Value::new_integer(
        lhs.checked_shr(rhs)
            .unwrap_or(if lhs >= 0 { 0 } else { -1 }),
    )
}

fn int_shl(lhs: i64, rhs: u32) -> Value {
    match lhs.checked_shl(rhs) {
        Some(res) => Value::new_integer(res),
        None => bigint_shl(&BigInt::from(lhs), rhs),
    }
}

fn bigint_shr(lhs: &BigInt, rhs: u32) -> Value {
    Value::new_bigint(lhs.shr(rhs))
}

fn bigint_shl(lhs: &BigInt, rhs: u32) -> Value {
    Value::new_bigint(lhs.shl(rhs))
}

macro_rules! cmp_values {
    (($op:ident, $op_str:expr)) => {
        paste! {
            pub(super) extern "C" fn [<cmp_ $op _values>](
                vm: &mut Executor,
                globals: &mut Globals,
                lhs: Value,
                rhs: Value
            ) -> Option<Value> {
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
                    _ => {
                        return vm.invoke_method(globals, $op_str, lhs, &[rhs]);
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

macro_rules! eq_values {
    (($op:ident, $op_str:expr)) => {
        paste! {
            pub(super) extern "C" fn [<cmp_ $op _values>](
                vm: &mut Executor,
                globals: &mut Globals,
                lhs: Value,
                rhs: Value
            ) -> Option<Value> {
                match vm.[<cmp_ $op _values_bool>](globals, lhs, rhs) {
                    Ok(b) => Some(Value::bool(b)),
                    Err(err) => {
                        vm.set_error(err);
                        None
                    }
                }
            }

            impl Executor {
                #[allow(unused)]
                pub(super) fn [<cmp_ $op _values_bool>](
                    &mut self,
                    globals: &mut Globals,
                    lhs: Value,
                    rhs: Value
                ) -> Result<bool> {
                    let b = match (lhs.unpack(), rhs.unpack()) {
                        (RV::Nil, RV::Nil) => true.$op(&true),
                        (RV::Nil, _) => false.$op(&true),
                        (RV::Integer(lhs), RV::Integer(rhs)) => lhs.$op(&rhs),
                        (RV::Integer(lhs), RV::BigInt(rhs)) => BigInt::from(lhs).$op(&rhs),
                        (RV::Integer(lhs), RV::Float(rhs)) => (lhs as f64).$op(&rhs),
                        (RV::Integer(lhs), _) => false.$op(&true),
                        (RV::BigInt(lhs), RV::Integer(rhs)) => lhs.$op(&BigInt::from(rhs)),
                        (RV::BigInt(lhs), RV::BigInt(rhs)) => lhs.$op(&rhs),
                        (RV::BigInt(lhs), RV::Float(rhs)) => lhs.to_f64().unwrap().$op(&rhs),
                        (RV::BigInt(lhs), _) => false.$op(&true),
                        (RV::Float(lhs), RV::Integer(rhs)) => lhs.$op(&(rhs as f64)),
                        (RV::Float(lhs), RV::BigInt(rhs)) => lhs.$op(&(rhs.to_f64().unwrap())),
                        (RV::Float(lhs), RV::Float(rhs)) => lhs.$op(&rhs),
                        (RV::Float(lhs), _) => false.$op(&true),
                        (RV::Bool(lhs), RV::Bool(rhs)) => lhs.$op(&rhs),
                        (RV::Bool(lhs), _) => false.$op(&true),
                        (RV::Symbol(lhs), RV::Symbol(rhs)) => lhs.$op(&rhs),
                        (RV::Symbol(lhs), _) => false.$op(&true),
                        (RV::String(lhs), RV::String(rhs)) => lhs.$op(rhs),
                        (RV::String(lhs), _) => false.$op(&true),
                        _ => {
                            self.invoke_method_inner(globals, $op_str, lhs, &[rhs])?.as_bool()
                        }
                    };
                    Ok(b)
                }
            }
        }
    };
    (($op1:ident, $op_str1:expr), $(($op2:ident, $op_str2:expr)),+) => {
        eq_values!(($op1, $op_str1));
        eq_values!($(($op2, $op_str2)),+);
    };
}

eq_values!((eq, IdentId::_EQ), (ne, IdentId::_NEQ));

#[test]
fn cmp_values() {
    let mut globals = Globals::new(0, false);
    let mut vm = Executor::default();
    let pairs = [
        (Value::nil(), Value::nil(), true),
        (Value::nil(), Value::new_integer(100), false),
        (Value::nil(), Value::new_float(100.0), false),
        (Value::bool(true), Value::bool(true), true),
        (Value::bool(true), Value::bool(false), false),
        (Value::bool(true), Value::nil(), false),
        (Value::new_integer(100), Value::new_integer(100), true),
        (Value::new_integer(100), Value::new_integer(200), false),
        (Value::new_integer(100), Value::new_float(100.0), true),
        (Value::new_integer(100), Value::new_float(200.0), false),
        (Value::new_integer(100), Value::nil(), false),
        (Value::new_integer(100), Value::bool(true), false),
        (Value::new_integer(100), Value::bool(false), false),
        (
            Value::new_integer(100),
            Value::new_symbol(IdentId::TO_S),
            false,
        ),
        (
            Value::new_integer(100),
            Value::new_string_from_str("100"),
            false,
        ),
        (
            Value::new_symbol(IdentId::TO_S),
            Value::new_symbol(IdentId::TO_S),
            true,
        ),
        (
            Value::new_symbol(IdentId::TO_S),
            Value::new_symbol(IdentId::NAME),
            false,
        ),
    ];
    for (lhs, rhs, ans) in pairs {
        assert_eq!(
            ans,
            Executor::cmp_eq_values_bool(&mut vm, &mut globals, lhs, rhs).unwrap()
        );
        assert_eq!(
            ans,
            !Executor::cmp_ne_values_bool(&mut vm, &mut globals, lhs, rhs).unwrap()
        );
    }
}

pub(super) extern "C" fn cmp_teq_values(
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
        (RV::Integer(lhs), RV::Integer(rhs)) => lhs.eq(&rhs),
        (RV::Integer(lhs), RV::BigInt(rhs)) => BigInt::from(lhs).eq(rhs),
        (RV::Integer(lhs), RV::Float(rhs)) => (lhs as f64).eq(&rhs),
        (RV::BigInt(lhs), RV::Integer(rhs)) => lhs.eq(&BigInt::from(rhs)),
        (RV::BigInt(lhs), RV::BigInt(rhs)) => lhs.eq(rhs),
        (RV::BigInt(lhs), RV::Float(rhs)) => lhs.to_f64().unwrap().eq(&rhs),
        (RV::Float(lhs), RV::Integer(rhs)) => lhs.eq(&(rhs as f64)),
        (RV::Float(lhs), RV::BigInt(rhs)) => lhs.eq(&(rhs.to_f64().unwrap())),
        (RV::Float(lhs), RV::Float(rhs)) => lhs.eq(&rhs),
        _ => {
            return vm.invoke_method(globals, IdentId::_TEQ, lhs, &[rhs]);
        }
    };
    Some(Value::bool(b))
}

pub(super) extern "C" fn neg_value(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
) -> Option<Value> {
    let v = match lhs.unpack() {
        RV::Integer(lhs) => match lhs.checked_neg() {
            Some(lhs) => Value::new_integer(lhs),
            None => Value::new_bigint(-BigInt::from(lhs)),
        },
        RV::Float(lhs) => Value::new_float(-lhs),
        RV::BigInt(lhs) => Value::new_bigint(-lhs),
        _ => {
            return vm.invoke_method(globals, IdentId::_UMINUS, lhs, &[]);
        }
    };
    Some(v)
}

pub(super) extern "C" fn pos_value(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
) -> Option<Value> {
    let v = match lhs.unpack() {
        RV::Integer(lhs) => Value::new_integer(lhs),
        RV::Float(lhs) => Value::new_float(lhs),
        RV::BigInt(lhs) => Value::new_bigint(lhs.clone()),
        _ => {
            return vm.invoke_method(globals, IdentId::get_id("@+"), lhs, &[]);
        }
    };
    Some(v)
}

pub(super) extern "C" fn bitnot_value(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
) -> Option<Value> {
    let v = match lhs.unpack() {
        RV::Integer(lhs) => Value::new_integer(!lhs),
        RV::BigInt(lhs) => Value::new_bigint(!lhs),
        _ => {
            return vm.invoke_method(globals, IdentId::get_id("~"), lhs, &[]);
        }
    };
    Some(v)
}

pub(super) fn integer_index1(globals: &Globals, base: Value, index: Value) -> Result<Value> {
    // we must support Integer#[Range].
    match (base.unpack(), index.unpack()) {
        (RV::Integer(base), RV::Integer(index)) => {
            let val = if index < 0 {
                0
            } else if index > 63 {
                base.is_negative().into()
            } else {
                (base >> index) & 1
            };
            Ok(Value::new_integer(val))
        }
        (RV::Integer(_), RV::BigInt(_)) => Ok(Value::new_integer(0)),
        (RV::Integer(_), _) => Err(MonorubyErr::no_implicit_conversion(
            globals,
            index,
            INTEGER_CLASS,
        )),
        (RV::BigInt(base), RV::Integer(index)) => {
            if index < 0 {
                Ok(Value::new_integer(0))
            } else {
                let i = (base >> index) & num::BigInt::from(1);
                Ok(Value::new_bigint(i))
            }
        }
        (RV::BigInt(_), RV::BigInt(_)) => Ok(Value::new_integer(0)),
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
    let ary = src.as_array();
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

pub extern "C" fn vm_get_constant(
    vm: &mut Executor,
    globals: &mut Globals,
    site_id: ConstSiteId,
    const_version: usize,
) -> Option<Value> {
    let (cached_version, val) = &globals.store[site_id].cache;
    if *cached_version == const_version {
        return *val;
    };
    match vm.find_constant(globals, site_id) {
        Ok(val) => {
            globals.store[site_id].cache = (const_version, Some(val));
            Some(val)
        }
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}
