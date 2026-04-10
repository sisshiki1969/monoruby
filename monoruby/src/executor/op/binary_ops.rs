//
// Binary operations.
//

use super::*;

use num::{BigInt, Signed, ToPrimitive, Zero};
use paste::paste;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Shl, Shr, Sub};

/// Try the `coerce` protocol: call `rhs.coerce(lhs)` and if it returns
/// a 2-element array, call `ary[0].op(ary[1])`.  Returns `None` if
/// `coerce` is not defined on `rhs`.
pub(crate) fn try_coerce_and_apply(
    vm: &mut Executor,
    globals: &mut Globals,
    op: IdentId,
    lhs: Value,
    rhs: Value,
    type_name: &'static str,
) -> Option<Value> {
    let coerce_id = IdentId::get_id("coerce");
    match vm.invoke_method_if_exists(globals, coerce_id, rhs, &[lhs], None, None) {
        Ok(Some(result)) => {
            if let Some(ary) = result.try_array_ty() {
                if ary.len() == 2 {
                    return vm.invoke_method_simple(globals, op, ary[0], &[ary[1]]);
                }
            }
            let err = MonorubyErr::typeerr("coerce must return [x, y]".to_string());
            vm.set_error(err);
            None
        }
        Ok(None) => {
            // coerce method not defined
            let err = MonorubyErr::cant_coerced_into(globals, op, rhs, type_name);
            vm.set_error(err);
            None
        }
        Err(err) => {
            // coerce raised an error — propagate it (CRuby behavior)
            vm.set_error(err);
            None
        }
    }
}

/// CRuby-compatible coerce for bitwise operations (Integer#&, |, ^).
///
/// CRuby's rb_num_coerce_bit:
/// 1. Call rhs.coerce(lhs) → [x, y]
/// 2. Re-dispatch x.op(y)
/// 3. If coerced x is not Integer, the re-dispatch will call Integer#op again,
///    which calls coerce again → CRuby detects this and raises TypeError.
///
/// We simplify: after coerce, check that x is Integer. If not, raise TypeError.
pub(crate) fn try_coerce_and_apply_bit(
    vm: &mut Executor,
    globals: &mut Globals,
    op: IdentId,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    let coerce_id = IdentId::get_id("coerce");
    match vm.invoke_method_if_exists(globals, coerce_id, rhs, &[lhs], None, None) {
        Ok(Some(result)) => {
            if let Some(ary) = result.try_array_ty() {
                if ary.len() == 2 {
                    let x = ary[0];
                    // Check that coerced lhs is Integer — if not, TypeError
                    match x.unpack() {
                        RV::Fixnum(_) | RV::BigInt(_) => {
                            return vm.invoke_method_simple(globals, op, x, &[ary[1]]);
                        }
                        _ => {
                            let err = MonorubyErr::typeerr(format!(
                                "{} can't be coerced into Integer",
                                rhs.get_real_class_name(&globals.store),
                            ));
                            vm.set_error(err);
                            return None;
                        }
                    }
                }
            }
            let err = MonorubyErr::typeerr("coerce must return [x, y]".to_string());
            vm.set_error(err);
            None
        }
        Ok(None) => {
            let err = MonorubyErr::cant_coerced_into(globals, op, rhs, "Integer");
            vm.set_error(err);
            None
        }
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
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
                match (RealKind::try_from(lhs), RealKind::try_from(rhs)) {
                    (Some(lhs), Some(rhs)) => return Some((lhs.$op(rhs)).into()),
                    _ => {}
                }
                let v = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Fixnum(lhs), RV::Complex(rhs)) => {
                        let lhs = num::complex::Complex::from(Real::from(lhs));
                        Value::complex_from(lhs.$op(rhs))
                    }
                    (RV::BigInt(lhs), RV::Complex(rhs)) => {
                        let lhs = num::complex::Complex::from(Real::from(lhs.clone()));
                        Value::complex_from(lhs.$op(rhs))
                    }
                    (RV::Fixnum(_) | RV::BigInt(_), _) => {
                        return try_coerce_and_apply(vm, globals, $op_str, lhs, rhs, "Integer");
                    }

                    (RV::Float(lhs), RV::Complex(rhs)) => {
                        let lhs = num::complex::Complex::from(Real::from(lhs));
                        Value::complex_from(lhs.$op(rhs))
                    }
                    (RV::Float(_), _) => {
                        return try_coerce_and_apply(vm, globals, $op_str, lhs, rhs, "Float");
                    }
                    (RV::Complex(lhs), RV::Fixnum(rhs)) => {
                        let rhs = num::complex::Complex::from(Real::from(rhs));
                        Value::complex_from(lhs.$op(rhs))
                    }
                    (RV::Complex(lhs), RV::BigInt(rhs)) => {
                        let rhs = num::complex::Complex::from(Real::from(rhs.clone()));
                        Value::complex_from(lhs.$op(rhs))
                    }
                    (RV::Complex(lhs), RV::Float(rhs)) => {
                        let rhs = num::complex::Complex::from(Real::from(rhs));
                        Value::complex_from(lhs.$op(rhs))
                    }
                    (RV::Complex(lhs), RV::Complex(rhs)) => {
                        Value::complex_from(lhs.$op(rhs))
                    }
                    (RV::Complex(_), _) => {
                        return try_coerce_and_apply(vm, globals, $op_str, lhs, rhs, "Complex");
                    }
                    _ => {
                        return vm.invoke_method_simple(globals, $op_str, lhs, &[rhs]);
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

macro_rules! binop_values_no_opt {
    (($op:ident, $op_str:expr)) => {
        paste! {
            pub(crate) extern "C" fn [<$op _values_no_opt>](
                vm: &mut Executor,
                globals: &mut Globals,
                lhs: Value,
                rhs: Value
            ) -> Option<Value> {
                vm.invoke_method_simple(globals, $op_str, lhs, &[rhs])
            }
        }
    };
    (($op1:ident, $op_str1:expr), $(($op2:ident, $op_str2:expr)),+) => {
        binop_values_no_opt!(($op1, $op_str1));
        binop_values_no_opt!($(($op2, $op_str2)),+);
    };
}

binop_values!(
    (add, IdentId::_ADD),
    (sub, IdentId::_SUB),
    (mul, IdentId::_MUL)
);

pub(crate) extern "C" fn div_values(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    match (RealKind::try_from(lhs), RealKind::try_from(rhs)) {
        (Some(lhs), Some(rhs)) => {
            if rhs.check_zero_div() && !lhs.is_float() {
                vm.err_divide_by_zero();
                return None;
            } else {
                return Some((lhs.ruby_div(&rhs)).into());
            }
        }
        _ => {}
    }
    let v = match (lhs.unpack(), rhs.unpack()) {
        (RV::Fixnum(lhs), RV::Complex(rhs)) => {
            if rhs.is_zero() {
                vm.err_divide_by_zero();
                return None;
            }
            let lhs = num::complex::Complex::from(Real::from(lhs));
            Value::complex_from(lhs.div(rhs))
        }
        (RV::BigInt(lhs), RV::Complex(rhs)) => {
            if rhs.is_zero() {
                vm.err_divide_by_zero();
                return None;
            }
            let lhs = num::complex::Complex::from(Real::from(lhs.clone()));
            Value::complex_from(lhs.div(rhs))
        }
        (RV::Float(lhs), RV::Complex(rhs)) => {
            if rhs.is_zero() {
                vm.err_divide_by_zero();
                return None;
            }
            let lhs = num::complex::Complex::from(Real::from(lhs));
            Value::complex_from(lhs.div(rhs))
        }
        (RV::Float(_), _) => {
            return try_coerce_and_apply(vm, globals, IdentId::_DIV, lhs, rhs, "Float");
        }
        (RV::Complex(lhs), RV::Fixnum(rhs)) => {
            if rhs.is_zero() {
                vm.err_divide_by_zero();
                return None;
            }
            let rhs = num::complex::Complex::from(Real::from(rhs));
            Value::complex_from((*lhs).div(rhs))
        }
        (RV::Complex(lhs), RV::BigInt(rhs)) => {
            if rhs.is_zero() {
                vm.err_divide_by_zero();
                return None;
            }
            let rhs = num::complex::Complex::from(Real::from(rhs.clone()));
            Value::complex_from(lhs.div(rhs))
        }
        (RV::Complex(lhs), RV::Float(rhs)) => {
            let rhs = num::complex::Complex::from(Real::from(rhs));
            Value::complex_from(lhs.div(rhs))
        }
        (RV::Complex(lhs), RV::Complex(rhs)) => {
            if rhs.is_zero() {
                vm.err_divide_by_zero();
                return None;
            }
            Value::complex_from(lhs.div(rhs))
        }
        (RV::Complex(_), _) => {
            return try_coerce_and_apply(vm, globals, IdentId::_DIV, lhs, rhs, "Complex");
        }
        (RV::Fixnum(_) | RV::BigInt(_), _) => {
            return try_coerce_and_apply(vm, globals, IdentId::_DIV, lhs, rhs, "Integer");
        }
        _ => {
            return vm.invoke_method_simple(globals, IdentId::_DIV, lhs, &[rhs]);
        }
    };
    Some(v)
}

pub(crate) extern "C" fn rem_values(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    match (RealKind::try_from(lhs), RealKind::try_from(rhs)) {
        (Some(lhs), Some(rhs)) => {
            // For modulo, both integer zero and float zero raise ZeroDivisionError
            if rhs.check_zero_div() || rhs.is_float_zero() {
                vm.err_divide_by_zero();
                return None;
            } else {
                return Some((lhs.ruby_mod(&rhs)).into());
            }
        }
        _ => {}
    }
    let v = match (lhs.unpack(), rhs.unpack()) {
        (RV::BigInt(lhs), RV::Complex(rhs)) => {
            let lhs = num::complex::Complex::from(Real::from(lhs.clone()));
            Value::complex_from(lhs.rem(rhs))
        }
        (RV::Fixnum(_) | RV::BigInt(_), _) => {
            return try_coerce_and_apply(vm, globals, IdentId::_REM, lhs, rhs, "Integer");
        }

        (RV::Float(lhs), RV::Complex(rhs)) => {
            let lhs = num::complex::Complex::from(Real::from(lhs));
            Value::complex_from(lhs.rem(rhs))
        }
        (RV::Float(_), _) => {
            return try_coerce_and_apply(vm, globals, IdentId::_REM, lhs, rhs, "Float");
        }
        (RV::Complex(lhs), RV::Fixnum(rhs)) => {
            let rhs = num::complex::Complex::from(Real::from(rhs));
            Value::complex_from(lhs.rem(rhs))
        }
        (RV::Complex(lhs), RV::BigInt(rhs)) => {
            let rhs = num::complex::Complex::from(Real::from(rhs.clone()));
            Value::complex_from(lhs.rem(rhs))
        }
        (RV::Complex(lhs), RV::Float(rhs)) => {
            let rhs = num::complex::Complex::from(Real::from(rhs));
            Value::complex_from(lhs.rem(rhs))
        }
        (RV::Complex(lhs), RV::Complex(rhs)) => Value::complex_from(lhs.rem(rhs)),
        (RV::Complex(_), _) => {
            return try_coerce_and_apply(vm, globals, IdentId::_REM, lhs, rhs, "Complex");
        }
        _ => {
            return vm.invoke_method_simple(globals, IdentId::_REM, lhs, &[rhs]);
        }
    };
    Some(v)
}

binop_values_no_opt!(
    (add, IdentId::_ADD),
    (sub, IdentId::_SUB),
    (mul, IdentId::_MUL),
    (div, IdentId::_DIV),
    (rem, IdentId::_REM),
    (pow, IdentId::_POW),
    (bitor, IdentId::_BOR),
    (bitand, IdentId::_BAND),
    (bitxor, IdentId::_BXOR),
    (shl, IdentId::_SHL),
    (shr, IdentId::_SHR)
);

/// Maximum result size in bits for integer exponentiation (16 GB on 64-bit).
/// Matches CRuby's BIGLEN_LIMIT in bignum.c.
const BIGLEN_LIMIT: u64 = 1u64 << 34;

/// Check if the result of base ** exp would exceed the size limit.
/// Returns true if the exponentiation is safe to perform.
fn check_pow_limit(base_bits: u64, exp: u64) -> bool {
    base_bits <= BIGLEN_LIMIT
        && exp <= BIGLEN_LIMIT
        && base_bits
            .checked_mul(exp)
            .map_or(false, |v| v <= BIGLEN_LIMIT)
}

pub(crate) extern "C" fn pow_ii(lhs: i64, rhs: i64, vm: &mut Executor) -> Option<Value> {
    if let Ok(rhs) = i32::try_from(rhs) {
        if rhs < 0 {
            if lhs == 0 {
                vm.set_error(MonorubyErr::divide_by_zero());
                return None;
            }
            if lhs == 1 {
                return Some(Value::integer(1));
            }
            if lhs == -1 {
                return Some(Value::integer(if (-rhs) & 1 == 1 { -1 } else { 1 }));
            }
            // a ** -n = Rational(1, a**n) for |a| > 1
            let neg_rhs = (-rhs) as u32;
            let denom = BigInt::from(lhs).pow(neg_rhs);
            return Some(Value::rational_from_bigint(BigInt::from(1), denom));
        }
        let rhs = rhs as u32;
        match lhs.checked_pow(rhs) {
            Some(res) => Some(Value::integer(res)),
            None => {
                let base_bits = 64 - lhs.unsigned_abs().leading_zeros() as u64;
                if !check_pow_limit(base_bits, rhs as u64) {
                    vm.set_error(MonorubyErr::exponent_is_too_large());
                    return None;
                }
                Some(Value::bigint(BigInt::from(lhs).pow(rhs)))
            }
        }
    } else {
        vm.set_error(MonorubyErr::exponent_is_too_large());
        return None;
    }
}

fn pow_ff(lhs: f64, rhs: f64) -> Value {
    let result = lhs.powf(rhs);
    if result.is_nan() && lhs < 0.0 {
        let abs_result = (-lhs).powf(rhs);
        let theta = rhs * std::f64::consts::PI;
        let re = abs_result * theta.cos();
        let im = abs_result * theta.sin();
        Value::complex(re, im)
    } else {
        Value::float(result)
    }
}

// TODO: support rhs < 0.
pub(crate) extern "C" fn pow_values(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    let v = match (lhs.unpack(), rhs.unpack()) {
        (RV::Fixnum(lhs), RV::Fixnum(rhs)) => pow_ii(lhs, rhs, vm)?,
        (RV::Fixnum(lhs), RV::BigInt(rhs)) => {
            // Special cases: 1**n == 1, (-1)**n == 1 or -1
            if lhs == 1 {
                Value::integer(1)
            } else if lhs == -1 {
                Value::integer(if rhs.bit(0) { -1 } else { 1 })
            } else if lhs == 0 {
                if rhs.is_positive() {
                    Value::integer(0)
                } else {
                    vm.set_error(MonorubyErr::divide_by_zero());
                    return None;
                }
            } else {
                vm.set_error(MonorubyErr::exponent_is_too_large());
                return None;
            }
        }
        (RV::Fixnum(lhs), RV::Float(rhs)) => pow_ff(lhs as f64, rhs),
        (RV::BigInt(lhs), RV::Fixnum(rhs)) => {
            if rhs < 0 {
                if lhs.is_zero() {
                    vm.set_error(MonorubyErr::divide_by_zero());
                    return None;
                }
                if *lhs == BigInt::from(1) {
                    Value::integer(1)
                } else if *lhs == BigInt::from(-1) {
                    Value::integer(if (-rhs) & 1 == 1 { -1 } else { 1 })
                } else {
                    let neg_rhs = (-rhs) as u32;
                    let denom = lhs.pow(neg_rhs);
                    Value::rational_from_bigint(BigInt::from(1), denom)
                }
            } else {
                let base_bits = lhs.bits();
                if !check_pow_limit(base_bits, rhs as u64) {
                    vm.set_error(MonorubyErr::exponent_is_too_large());
                    return None;
                }
                if let Ok(rhs) = u32::try_from(rhs) {
                    Value::bigint(lhs.pow(rhs))
                } else {
                    vm.set_error(MonorubyErr::exponent_is_too_large());
                    return None;
                }
            }
        }
        (RV::BigInt(lhs), RV::BigInt(rhs)) => {
            if *lhs == BigInt::from(1) {
                Value::integer(1)
            } else if *lhs == BigInt::from(-1) {
                Value::integer(if rhs.bit(0) { -1 } else { 1 })
            } else if lhs.is_zero() {
                if rhs.is_positive() {
                    Value::integer(0)
                } else {
                    vm.set_error(MonorubyErr::divide_by_zero());
                    return None;
                }
            } else {
                vm.set_error(MonorubyErr::exponent_is_too_large());
                return None;
            }
        }
        (RV::BigInt(lhs), RV::Float(rhs)) => pow_ff(lhs.to_f64().unwrap(), rhs),
        (RV::Fixnum(_) | RV::BigInt(_), _) => {
            return try_coerce_and_apply(vm, globals, IdentId::_POW, lhs, rhs, "Integer");
        }

        (RV::Float(lhs), RV::Fixnum(rhs)) => {
            //if let Ok(rhs) = i32::try_from(rhs) {
            pow_ff(lhs, rhs as f64)
            //} else {
            //    Value::new_float(lhs.powf(rhs as f64))
            //}
        }
        (RV::Float(lhs), RV::BigInt(rhs)) => pow_ff(lhs, rhs.to_f64().unwrap()),
        (RV::Float(lhs), RV::Float(rhs)) => pow_ff(lhs, rhs),
        (RV::Float(_), _) => {
            return try_coerce_and_apply(vm, globals, IdentId::_POW, lhs, rhs, "Float");
        }
        _ => {
            return vm.invoke_method_simple(globals, IdentId::_POW, lhs, &[rhs]);
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
                    (RV::Fixnum(_) | RV::BigInt(_), _) => {
                        // CRuby-compatible: call coerce, then re-dispatch.
                        // If coerced lhs is not Integer, raise TypeError.
                        return try_coerce_and_apply_bit(vm, globals, $op_str, lhs, rhs);
                    }
                    _ => {
                        return vm.invoke_method_simple(globals, $op_str, lhs, &[rhs]);
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
                safe_int_shr(lhs, rhs as u64)
            } else {
                safe_int_shl(lhs, (-rhs) as u64, vm)?
            }
        }
        (RV::BigInt(lhs), RV::Fixnum(rhs)) => {
            if rhs >= 0 {
                safe_bigint_shr(lhs, rhs as u64)
            } else {
                safe_bigint_shl(lhs, (-rhs) as u64, vm)?
            }
        }
        // n >> bignum: if bignum > 0, shift right by huge amount => 0 or -1;
        // if bignum < 0, shift left by huge amount => RangeError (too large)
        (RV::Fixnum(lhs), RV::BigInt(rhs)) => {
            if rhs.is_positive() {
                Value::integer(if lhs >= 0 { 0 } else { -1 })
            } else if lhs == 0 {
                Value::integer(0)
            } else {
                vm.set_error(MonorubyErr::rangeerr("shift width too big"));
                return None;
            }
        }
        (RV::BigInt(lhs), RV::BigInt(rhs)) => {
            if rhs.is_positive() {
                Value::integer(if lhs.is_negative() { -1 } else { 0 })
            } else if lhs.is_zero() {
                Value::integer(0)
            } else {
                vm.set_error(MonorubyErr::rangeerr("shift width too big"));
                return None;
            }
        }
        (RV::Fixnum(_) | RV::BigInt(_), _) => {
            // >> requires to_int conversion (not coerce), supports BigInt shift amounts
            match rhs.coerce_to_int(vm, globals) {
                Ok(rhs_int) => return shr_values(vm, globals, lhs, rhs_int).into(),
                Err(_) => {
                    vm.set_error(MonorubyErr::typeerr(format!(
                        "{} can't be coerced into Integer",
                        rhs.get_real_class_name(&globals.store),
                    )));
                    return None;
                }
            }
        }
        _ => {
            return vm.invoke_method_simple(globals, IdentId::_SHR, lhs, &[rhs]);
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
                safe_int_shl(lhs, rhs as u64, vm)?
            } else {
                safe_int_shr(lhs, (-rhs) as u64)
            }
        }
        (RV::BigInt(lhs), RV::Fixnum(rhs)) => {
            if rhs >= 0 {
                safe_bigint_shl(lhs, rhs as u64, vm)?
            } else {
                safe_bigint_shr(lhs, (-rhs) as u64)
            }
        }
        // n << bignum: if bignum > 0, shift left by huge amount => RangeError (or 0 if n==0);
        // if bignum < 0, shift right by huge amount => 0 or -1
        (RV::Fixnum(lhs), RV::BigInt(rhs)) => {
            if rhs.is_positive() {
                if lhs == 0 {
                    Value::integer(0)
                } else {
                    vm.set_error(MonorubyErr::rangeerr("shift width too big"));
                    return None;
                }
            } else {
                Value::integer(if lhs >= 0 { 0 } else { -1 })
            }
        }
        (RV::BigInt(lhs), RV::BigInt(rhs)) => {
            if rhs.is_positive() {
                if lhs.is_zero() {
                    Value::integer(0)
                } else {
                    vm.set_error(MonorubyErr::rangeerr("shift width too big"));
                    return None;
                }
            } else {
                Value::integer(if lhs.is_negative() { -1 } else { 0 })
            }
        }
        (RV::Fixnum(_) | RV::BigInt(_), _) => {
            // << requires to_int conversion (not coerce), supports BigInt shift amounts
            match rhs.coerce_to_int(vm, globals) {
                Ok(rhs_int) => return shl_values(vm, globals, lhs, rhs_int).into(),
                Err(_) => {
                    vm.set_error(MonorubyErr::typeerr(format!(
                        "{} can't be coerced into Integer",
                        rhs.get_real_class_name(&globals.store),
                    )));
                    return None;
                }
            }
        }
        _ => {
            return vm.invoke_method_simple(globals, IdentId::_SHL, lhs, &[rhs]);
        }
    };
    Some(v)
}

/// Safe shift helpers that handle shift amounts > 32 bits without truncation.
fn safe_int_shr(lhs: i64, rhs: u64) -> Value {
    if rhs >= 64 {
        Value::integer(if lhs >= 0 { 0 } else { -1 })
    } else {
        Value::integer(
            lhs.checked_shr(rhs as u32)
                .unwrap_or(if lhs >= 0 { 0 } else { -1 }),
        )
    }
}

fn safe_int_shl(lhs: i64, rhs: u64, vm: &mut Executor) -> Option<Value> {
    if rhs > u32::MAX as u64 {
        if lhs == 0 {
            Some(Value::integer(0))
        } else {
            vm.set_error(MonorubyErr::rangeerr("shift width too big"));
            None
        }
    } else {
        let rhs = rhs as u32;
        Some(match lhs.checked_shl(rhs) {
            Some(res) if lhs.is_positive() == res.is_positive() => Value::integer(res),
            _ => bigint_shl(&BigInt::from(lhs), rhs),
        })
    }
}

fn safe_bigint_shr(lhs: &BigInt, rhs: u64) -> Value {
    if rhs > u32::MAX as u64 {
        Value::integer(if lhs.is_negative() { -1 } else { 0 })
    } else {
        Value::bigint(lhs.shr(rhs as u32))
    }
}

fn safe_bigint_shl(lhs: &BigInt, rhs: u64, vm: &mut Executor) -> Option<Value> {
    if rhs > u32::MAX as u64 {
        if lhs.is_zero() {
            Some(Value::integer(0))
        } else {
            vm.set_error(MonorubyErr::rangeerr("shift width too big"));
            None
        }
    } else {
        Some(Value::bigint(lhs.shl(rhs as u32)))
    }
}

fn bigint_shl(lhs: &BigInt, rhs: u32) -> Value {
    Value::bigint(lhs.shl(rhs))
}
