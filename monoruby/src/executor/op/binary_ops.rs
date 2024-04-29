//
// Binary operations.
//

use super::*;

use num::{BigInt, Integer, ToPrimitive, Zero};
use paste::paste;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Shl, Shr, Sub};

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
                    (RV::Fixnum(lhs), RV::BigInt(rhs)) => Value::bigint(BigInt::from(lhs).$op(rhs)),
                    (RV::Fixnum(lhs), RV::Float(rhs)) => Value::float((lhs as f64).$op(&rhs)),
                    (RV::Fixnum(lhs), RV::Complex(rhs)) => {
                        let lhs = num::complex::Complex::from(Real::from(lhs));
                        Value::complex_from(lhs.$op(rhs))
                    }
                    (RV::BigInt(lhs), RV::Fixnum(rhs)) => Value::bigint(lhs.$op(BigInt::from(rhs))),
                    (RV::BigInt(lhs), RV::BigInt(rhs)) => Value::bigint(lhs.$op(rhs)),
                    (RV::BigInt(lhs), RV::Float(rhs)) => Value::float(lhs.to_f64().unwrap().$op(&rhs)),
                    (RV::BigInt(lhs), RV::Complex(rhs)) => {
                        let lhs = num::complex::Complex::from(Real::from(lhs.clone()));
                        Value::complex_from(lhs.$op(rhs))
                    }
                    (RV::Fixnum(_) | RV::BigInt(_), _) => {
                        let err = MonorubyErr::cant_coerced_into($op_str, rhs, "Integer");
                        vm.set_error(err);
                        return None;
                    }

                    (RV::Float(lhs), RV::Fixnum(rhs)) => Value::float(lhs.$op(&(rhs as f64))),
                    (RV::Float(lhs), RV::BigInt(rhs)) => Value::float(lhs.$op(rhs.to_f64().unwrap())),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs.$op(&rhs)),
                    (RV::Float(lhs), RV::Complex(rhs)) => {
                        let lhs = num::complex::Complex::from(Real::from(lhs));
                        Value::complex_from(lhs.$op(rhs))
                    }
                    (RV::Float(_), _) => {
                        let err = MonorubyErr::cant_coerced_into($op_str, rhs, "Float");
                        vm.set_error(err);
                        return None;
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
                        let err = MonorubyErr::cant_coerced_into($op_str, rhs, "Complex");
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

macro_rules! binop_values_no_opt {
    (($op:ident, $op_str:expr)) => {
        paste! {
            pub(crate) extern "C" fn [<$op _values_no_opt>](
                vm: &mut Executor,
                globals: &mut Globals,
                lhs: Value,
                rhs: Value
            ) -> Option<Value> {
                vm.invoke_method(globals, $op_str, lhs, &[rhs], None)
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
    (mul, IdentId::_MUL),
    (rem, IdentId::_REM)
);

binop_values_no_opt!(
    (add, IdentId::_ADD),
    (sub, IdentId::_SUB),
    (mul, IdentId::_MUL),
    (div, IdentId::_DIV),
    (rem, IdentId::_REM),
    (pow, IdentId::_POW),
    (bitor, IdentId::_BOR),
    (bitand, IdentId::_BAND),
    (bitxor, IdentId::_BXOR)
);

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
        (RV::Fixnum(_) | RV::BigInt(_), _) => {
            let err = MonorubyErr::cant_coerced_into(IdentId::_POW, rhs, "Integer");
            vm.set_error(err);
            return None;
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
            let err = MonorubyErr::cant_coerced_into(IdentId::_POW, rhs, "Float");
            vm.set_error(err);
            return None;
        }
        _ => {
            return vm.invoke_method(globals, IdentId::_POW, lhs, &[rhs], None);
        }
    };
    Some(v)
}

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
        (RV::Fixnum(lhs), RV::Complex(rhs)) => {
            let lhs = num::complex::Complex::from(Real::from(lhs));
            Value::complex_from(lhs.div(rhs))
        }
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
        (RV::BigInt(lhs), RV::Complex(rhs)) => {
            let lhs = num::complex::Complex::from(Real::from(lhs.clone()));
            Value::complex_from(lhs.div(rhs))
        }
        (RV::Fixnum(_) | RV::BigInt(_), _) => {
            let err = MonorubyErr::cant_coerced_into(IdentId::_DIV, rhs, "Integer");
            vm.set_error(err);
            return None;
        }

        (RV::Float(lhs), RV::Fixnum(rhs)) => Value::float(lhs.div(&(rhs as f64))),
        (RV::Float(lhs), RV::BigInt(rhs)) => Value::float(lhs.div(&rhs.to_f64().unwrap())),
        (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs.div(&rhs)),
        (RV::Float(lhs), RV::Complex(rhs)) => {
            let lhs = num::complex::Complex::from(Real::from(lhs));
            Value::complex_from(lhs.div(rhs))
        }
        (RV::Float(_), _) => {
            let err = MonorubyErr::cant_coerced_into(IdentId::_DIV, rhs, "Float");
            vm.set_error(err);
            return None;
        }
        (RV::Complex(lhs), RV::Fixnum(rhs)) => {
            let rhs = num::complex::Complex::from(Real::from(rhs));
            Value::complex_from(lhs.clone().div(rhs))
        }
        (RV::Complex(lhs), RV::BigInt(rhs)) => {
            let rhs = num::complex::Complex::from(Real::from(rhs.clone()));
            Value::complex_from(lhs.div(rhs))
        }
        (RV::Complex(lhs), RV::Float(rhs)) => {
            let rhs = num::complex::Complex::from(Real::from(rhs));
            Value::complex_from(lhs.div(rhs))
        }
        (RV::Complex(lhs), RV::Complex(rhs)) => Value::complex_from(lhs.div(rhs)),
        (RV::Complex(_), _) => {
            let err = MonorubyErr::cant_coerced_into(IdentId::_DIV, rhs, "Complex");
            vm.set_error(err);
            return None;
        }
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
                    (RV::Fixnum(_) | RV::BigInt(_), _) => {
                        let err = MonorubyErr::cant_coerced_into($op_str, rhs, "Integer");
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
        (RV::Fixnum(_) | RV::BigInt(_), _) => {
            let err = MonorubyErr::cant_coerced_into(IdentId::_SHR, rhs, "Integer");
            vm.set_error(err);
            return None;
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
        (RV::Fixnum(_) | RV::BigInt(_), _) => {
            let err = MonorubyErr::cant_coerced_into(IdentId::_SHL, rhs, "Integer");
            vm.set_error(err);
            return None;
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
        // Work around
        Some(res) if lhs.is_positive() == res.is_positive() => Value::integer(res),
        _ => bigint_shl(&BigInt::from(lhs), rhs),
    }
}

fn bigint_shr(lhs: &BigInt, rhs: u32) -> Value {
    Value::bigint(lhs.shr(rhs))
}

fn bigint_shl(lhs: &BigInt, rhs: u32) -> Value {
    Value::bigint(lhs.shl(rhs))
}
