use super::*;

mod binary_ops;
mod sort;

pub(crate) use binary_ops::*;
use num::{BigInt, ToPrimitive};
use paste::paste;
pub(crate) use sort::*;

//
// Generic operations.
//

pub extern "C" fn i64_to_value(i: i64) -> Value {
    Value::integer(i)
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
                    (RV::Fixnum(_)| RV::BigInt(_) , _) => {
                        // Try coerce protocol for comparison
                        let coerce_id = IdentId::get_id("coerce");
                        if let Ok(Some(result)) = vm.invoke_method_if_exists(globals, coerce_id, rhs, &[lhs], None, None) {
                            if let Some(ary) = result.try_array_ty() {
                                if ary.len() == 2 {
                                    return vm.invoke_method_simple(globals, $op_str, ary[0], &[ary[1]]);
                                }
                            }
                        }
                        let err = MonorubyErr::argumenterr(format!(
                            "comparison of {} with {} failed",
                            lhs.get_real_class_name(globals),
                            rhs.get_real_class_name(globals),
                        ));
                        vm.set_error(err);
                        return None;
                    }

                    (RV::Float(lhs), RV::Fixnum(rhs)) => lhs.$op(&(rhs as f64)),
                    (RV::Float(lhs), RV::BigInt(rhs)) => lhs.$op(&(rhs.to_f64().unwrap())),
                    (RV::Float(lhs), RV::Float(rhs)) => lhs.$op(&rhs),
                    (RV::Float(_) , _) => {
                        // Try coerce protocol for comparison
                        let coerce_id = IdentId::get_id("coerce");
                        if let Ok(Some(result)) = vm.invoke_method_if_exists(globals, coerce_id, rhs, &[lhs], None, None) {
                            if let Some(ary) = result.try_array_ty() {
                                if ary.len() == 2 {
                                    return vm.invoke_method_simple(globals, $op_str, ary[0], &[ary[1]]);
                                }
                            }
                        }
                        let err = MonorubyErr::argumenterr(format!(
                            "comparison of {} with {} failed",
                            lhs.get_real_class_name(globals),
                            rhs.get_real_class_name(globals),
                        ));
                        vm.set_error(err);
                        return None;
                    }
                    _ => {
                        return vm.invoke_method_simple(globals, $op_str, lhs, &[rhs]);
                    }
                };
                Some(Value::bool(b))
            }
        }

        paste! {
            pub(crate) extern "C" fn [<cmp_ $op _values_no_opt>](
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
            (RV::Fixnum(lhs), RV::BigInt(rhs)) => BigInt::from(lhs).eq(rhs),
            (RV::Fixnum(lhs), RV::Float(rhs)) => (lhs as f64).eq(&rhs),
            (RV::Fixnum(_), _) => {
                // Reverse dispatch: try rhs == lhs
                return self.invoke_eq(globals, rhs, lhs);
            }
            (RV::BigInt(lhs), RV::Fixnum(rhs)) => lhs.eq(&BigInt::from(rhs)),
            (RV::BigInt(lhs), RV::BigInt(rhs)) => lhs.eq(rhs),
            (RV::BigInt(lhs), RV::Float(rhs)) => lhs.to_f64().unwrap().eq(&rhs),
            (RV::BigInt(_), _) => {
                // Reverse dispatch: try rhs == lhs
                return self.invoke_eq(globals, rhs, lhs);
            }
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
            _ => self.invoke_eq(globals, lhs, rhs)?,
        };
        Ok(b)
    }

    pub(crate) fn eq_values_bool_no_opt(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<bool> {
        self.invoke_eq(globals, lhs, rhs)
    }

    pub(crate) fn ne_values_bool(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<bool> {
        Ok(!self.eq_values_bool(globals, lhs, rhs)?)
    }

    pub(crate) fn ne_values_bool_no_opt(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<bool> {
        Ok(!self.invoke_eq(globals, lhs, rhs)?)
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

        paste! {
            pub(crate) extern "C" fn [<cmp_ $op _values_no_opt>](
                vm: &mut Executor,
                globals: &mut Globals,
                lhs: Value,
                rhs: Value
            ) -> Option<Value> {
                match vm.[<$op _values_bool_no_opt>](globals, lhs, rhs) {
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
    let mut globals = Globals::new(0, false, true);
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
        (RV::Fixnum(_), _) => {
            // Reverse dispatch for Integer === non-numeric
            return vm.invoke_method_simple(globals, IdentId::_EQ, rhs, &[lhs]);
        }
        (RV::BigInt(lhs), RV::Fixnum(rhs)) => lhs.eq(&BigInt::from(rhs)),
        (RV::BigInt(lhs), RV::BigInt(rhs)) => lhs.eq(rhs),
        (RV::BigInt(lhs), RV::Float(rhs)) => lhs.to_f64().unwrap().eq(&rhs),
        (RV::BigInt(_), _) => {
            // Reverse dispatch for Integer === non-numeric
            return vm.invoke_method_simple(globals, IdentId::_EQ, rhs, &[lhs]);
        }
        (RV::Float(lhs), RV::Fixnum(rhs)) => lhs.eq(&(rhs as f64)),
        (RV::Float(lhs), RV::BigInt(rhs)) => lhs.eq(&(rhs.to_f64().unwrap())),
        (RV::Float(lhs), RV::Float(rhs)) => lhs.eq(&rhs),
        (RV::Float(_), _) => false,
        _ => {
            return vm.invoke_method_simple(globals, IdentId::_TEQ, lhs, &[rhs]);
        }
    };
    Some(Value::bool(b))
}

pub(crate) extern "C" fn cmp_teq_values_no_opt(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    vm.invoke_method_simple(globals, IdentId::_TEQ, lhs, &[rhs])
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
        (RV::Fixnum(_), _) => false,
        (RV::BigInt(lhs), RV::Fixnum(rhs)) => lhs.eq(&BigInt::from(rhs)),
        (RV::BigInt(lhs), RV::BigInt(rhs)) => lhs.eq(rhs),
        (RV::BigInt(lhs), RV::Float(rhs)) => lhs.to_f64().unwrap().eq(&rhs),
        (RV::BigInt(_), _) => false,
        (RV::Float(lhs), RV::Fixnum(rhs)) => lhs.eq(&(rhs as f64)),
        (RV::Float(lhs), RV::BigInt(rhs)) => lhs.eq(&(rhs.to_f64().unwrap())),
        (RV::Float(lhs), RV::Float(rhs)) => lhs.eq(&rhs),
        (RV::Float(_), _) => false,
        _ => {
            return vm
                .invoke_method_inner(globals, IdentId::_TEQ, lhs, &[rhs], None, None)
                .map(|v| v.as_bool());
        }
    };
    Ok(b)
}

#[allow(dead_code)]
pub(crate) fn cmp_teq_values_bool_no_opt(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Result<bool> {
    vm.invoke_method_inner(globals, IdentId::_TEQ, lhs, &[rhs], None, None)
        .map(|v| v.as_bool())
}

impl Executor {
    pub(crate) fn compare_values(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<std::cmp::Ordering> {
        self.compare_values_inner(globals, lhs, rhs)?
            .ok_or_else(|| {
                let lhs = lhs.get_real_class_name(&globals.store);
                let rhs = rhs.to_s(&globals.store);
                MonorubyErr::argumenterr(format!("comparison of {lhs} with {rhs} failed"))
            })
    }

    pub(crate) fn compare_values_inner(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<Option<std::cmp::Ordering>> {
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
            (RV::Fixnum(_), _) => None,
            (RV::BigInt(lhs), RV::Fixnum(rhs)) => lhs.partial_cmp(&BigInt::from(rhs)),
            (RV::BigInt(lhs), RV::BigInt(rhs)) => Some(lhs.cmp(rhs)),
            (RV::BigInt(lhs), RV::Float(rhs)) => lhs.to_f64().unwrap().partial_cmp(&rhs),
            (RV::BigInt(_), _) => None,
            (RV::Float(lhs), RV::Fixnum(rhs)) => lhs.partial_cmp(&(rhs as f64)),
            (RV::Float(lhs), RV::BigInt(rhs)) => lhs.partial_cmp(&(rhs.to_f64().unwrap())),
            (RV::Float(lhs), RV::Float(rhs)) => lhs.partial_cmp(&rhs),
            (RV::Float(_), _) => None,
            _ => {
                if let Some(i) = self
                    .invoke_method_inner(globals, IdentId::_CMP, lhs, &[rhs], None, None)?
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
        };
        Ok(res)
    }
}

macro_rules! unop_value_no_opt {
    (($op:ident, $op_str:expr)) => {
        paste! {
            pub(crate) extern "C" fn [<$op _value_no_opt>](
                vm: &mut Executor,
                globals: &mut Globals,
                lhs: Value,
            ) -> Option<Value> {
                vm.invoke_method_simple(globals, $op_str, lhs, &[])
            }
        }
    };
    (($op1:ident, $op_str1:expr), $(($op2:ident, $op_str2:expr)),+) => {
        unop_value_no_opt!(($op1, $op_str1));
        unop_value_no_opt!($(($op2, $op_str2)),+);
    };
}

unop_value_no_opt!(
    (bitnot, IdentId::_BNOT),
    (pos, IdentId::_UPLUS),
    (neg, IdentId::_UMINUS),
    (not, IdentId::_NOT)
);

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
        RV::BigInt(lhs) => Value::bigint(-lhs),
        RV::Float(lhs) => Value::float(-lhs),
        RV::Complex(lhs) => Value::complex(-lhs.re, -lhs.im),
        _ => {
            return vm.invoke_method_simple(globals, IdentId::_UMINUS, lhs, &[]);
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
        RV::BigInt(lhs) => Value::bigint(lhs.clone()),
        RV::Float(lhs) => Value::float(lhs),
        _ => {
            return vm.invoke_method_simple(globals, IdentId::_UPLUS, lhs, &[]);
        }
    };
    Some(v)
}

pub(crate) extern "C" fn not_value(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
) -> Option<Value> {
    let v = match lhs.unpack() {
        RV::Fixnum(_)
        | RV::BigInt(_)
        | RV::Float(_)
        | RV::String(_)
        | RV::Symbol(_)
        | RV::Complex(_)
        | RV::Bool(true) => Value::bool(false),
        RV::Bool(false) | RV::Nil => Value::bool(true),
        _ => {
            return vm.invoke_method_simple(globals, IdentId::_NOT, lhs, &[]);
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
            return vm.invoke_method_simple(globals, IdentId::_BNOT, lhs, &[]);
        }
    };
    Some(v)
}

pub(crate) fn integer_index1(
    vm: &mut Executor,
    globals: &mut Globals,
    base: Value,
    index: Value,
) -> Result<Value> {
    // Handle Integer#[Range]
    if let Some(range) = index.is_range() {
        let start_val = range.start();
        let end_val = range.end();
        let exclude_end = range.exclude_end();

        // Check for Float::INFINITY in range boundaries
        fn check_float_domain(val: Value, globals: &Globals) -> Result<()> {
            if let Some(f) = val.try_float() {
                if f.is_infinite() || f.is_nan() {
                    let msg = if f.is_nan() {
                        "NaN".to_string()
                    } else if f > 0.0 {
                        "Infinity".to_string()
                    } else {
                        "-Infinity".to_string()
                    };
                    let class_id = globals
                        .store
                        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("FloatDomainError"))
                        .map(|v| v.as_class_id());
                    return Err(match class_id {
                        Some(cid) => MonorubyErr::new(MonorubyErrKind::Other(cid), msg),
                        None => MonorubyErr::rangeerr(msg),
                    });
                }
            }
            Ok(())
        }
        check_float_domain(start_val, globals)?;
        check_float_domain(end_val, globals)?;

        let is_beginless = start_val.is_nil();
        let is_endless = end_val.is_nil();

        // Handle beginless range (..i)
        if is_beginless {
            let end = end_val.coerce_to_int_i64(vm, globals)?;
            let end = if exclude_end { end } else { end + 1 };
            // Extract bits 0..end from base
            let extracted = match base.unpack() {
                RV::Fixnum(b) => {
                    if end <= 0 {
                        0i64
                    } else if end >= 64 {
                        b
                    } else {
                        b & ((1i64 << end) - 1)
                    }
                }
                RV::BigInt(b) => {
                    if end <= 0 {
                        0i64
                    } else {
                        let mask = (BigInt::from(1) << end as usize) - 1;
                        let result: BigInt = b & mask;
                        // Convert to i64 if possible
                        use num::ToPrimitive;
                        result.to_i64().unwrap_or(1) // non-zero means raise
                    }
                }
                _ => unreachable!(),
            };
            if extracted != 0 {
                return Err(MonorubyErr::argumenterr(
                    "The beginless range for Integer#[] results in infinity",
                ));
            }
            return Ok(Value::integer(0));
        }

        // Handle endless range (i..)
        if is_endless {
            let start = start_val.coerce_to_int_i64(vm, globals)?;
            return match base.unpack() {
                RV::Fixnum(b) => {
                    if start >= 0 {
                        if start >= 64 {
                            Ok(Value::integer(if b < 0 { -1 } else { 0 }))
                        } else {
                            Ok(Value::integer(b >> start))
                        }
                    } else {
                        // Negative start in endless range: shift left
                        Ok(Value::bigint(BigInt::from(b) << (-start) as usize))
                    }
                }
                RV::BigInt(b) => {
                    if start >= 0 {
                        Ok(Value::bigint(b >> start as usize))
                    } else {
                        Ok(Value::bigint(b << (-start) as usize))
                    }
                }
                _ => unreachable!(),
            };
        }

        let start = start_val.coerce_to_int_i64(vm, globals)?;
        let end_raw = end_val.coerce_to_int_i64(vm, globals)?;
        let end = if exclude_end { end_raw } else { end_raw + 1 };
        let width = end - start;
        // Check if range is "reversed": for inclusive a..b where b < a, or
        // exclusive a...b where b < a (but NOT a...a which is 0-width)
        let is_reversed = if exclude_end { end_raw < start } else { end_raw < start };
        let shifted = |base: &BigInt| -> BigInt {
            if start >= 0 {
                base >> start
            } else {
                base << (-start) as usize
            }
        };
        // When reversed, extract all bits from start (no masking)
        if is_reversed {
            return match base.unpack() {
                RV::Fixnum(base) => {
                    if start >= 0 {
                        if start >= 64 {
                            Ok(Value::integer(if base < 0 { -1 } else { 0 }))
                        } else {
                            Ok(Value::integer(base >> start))
                        }
                    } else {
                        Ok(Value::bigint(BigInt::from(base) << (-start) as usize))
                    }
                }
                RV::BigInt(base) => Ok(Value::bigint(shifted(base))),
                _ => unreachable!(),
            };
        }
        // Use BigInt path when width >= 64 since mask won't fit in i64
        let base_bigint = match base.unpack() {
            RV::Fixnum(base) => {
                if width < 64 {
                    let mask = (1i64 << width) - 1;
                    return Ok(Value::integer(
                        (if start >= 0 {
                            base >> start
                        } else {
                            base << -start
                        }) & mask,
                    ));
                }
                BigInt::from(base)
            }
            RV::BigInt(base) => base.clone(),
            _ => unreachable!(),
        };
        let shifted_val = shifted(&base_bigint);
        let bits = shifted_val.bits() as i64;
        let val = if width > bits + 1 && shifted_val >= BigInt::ZERO {
            shifted_val
        } else {
            let mask = (BigInt::from(1) << width as usize) - 1;
            shifted_val & mask
        };
        return Ok(Value::bigint(val));
    }
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
