use super::*;

mod binary_ops;
mod sort;

pub(crate) use binary_ops::*;
use num::{BigInt, ToPrimitive};
use paste::paste;

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
                        let err = MonorubyErr::cant_coerced_into($op_str, rhs, "Integer");
                        vm.set_error(err);
                        return None;
                    }

                    (RV::Float(lhs), RV::Fixnum(rhs)) => lhs.$op(&(rhs as f64)),
                    (RV::Float(lhs), RV::BigInt(rhs)) => lhs.$op(&(rhs.to_f64().unwrap())),
                    (RV::Float(lhs), RV::Float(rhs)) => lhs.$op(&rhs),
                    (RV::Float(_) , _) => {
                        let err = MonorubyErr::cant_coerced_into($op_str, rhs, "Float");
                        vm.set_error(err);
                        return None;
                    }
                    _ => {
                        return vm.invoke_method(globals, $op_str, lhs, &[rhs], None);
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
                vm.invoke_method(globals, $op_str, lhs, &[rhs], None)
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
            (RV::Fixnum(_), _) => false,
            (RV::BigInt(lhs), RV::Fixnum(rhs)) => lhs.eq(&BigInt::from(rhs)),
            (RV::BigInt(lhs), RV::BigInt(rhs)) => lhs.eq(rhs),
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

    pub(crate) fn eq_values_bool_no_opt(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<bool> {
        let b = self
            .invoke_method_inner(globals, IdentId::_EQ, lhs, &[rhs], None)?
            .as_bool();
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

    pub(crate) fn ne_values_bool_no_opt(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<bool> {
        Ok(!self.eq_values_bool_no_opt(globals, lhs, rhs)?)
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
            return vm.invoke_method(globals, IdentId::_TEQ, lhs, &[rhs], None);
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
    vm.invoke_method(globals, IdentId::_TEQ, lhs, &[rhs], None)
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
                .invoke_method_inner(globals, IdentId::_TEQ, lhs, &[rhs], None)
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
    vm.invoke_method_inner(globals, IdentId::_TEQ, lhs, &[rhs], None)
        .map(|v| v.as_bool())
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

pub(crate) extern "C" fn cmp_cmp_values_no_opt(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    match vm.cmp_cmp_values_no_opt_inner(globals, lhs, rhs) {
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
        let res = self
            .compare_values_inner(globals, lhs, rhs)?
            .map_or(Value::nil(), Value::from_ord);
        Ok(res)
    }

    pub(crate) fn cmp_cmp_values_no_opt_inner(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<Value> {
        let res = self
            .compare_values_no_opt_inner(globals, lhs, rhs)?
            .map_or(Value::nil(), Value::from_ord);
        Ok(res)
    }

    pub(crate) fn compare_values(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<std::cmp::Ordering> {
        self.compare_values_inner(globals, lhs, rhs)?
            .ok_or_else(|| {
                let lhs = lhs.get_real_class_name(globals);
                let rhs = rhs.to_s(globals);
                MonorubyErr::argumenterr(format!("comparison of {lhs} with {rhs} failed"))
            })
    }

    #[allow(dead_code)]
    pub(crate) fn compare_values_no_opt(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<std::cmp::Ordering> {
        self.compare_values_no_opt_inner(globals, lhs, rhs)?
            .ok_or_else(|| {
                let lhs = lhs.get_real_class_name(globals);
                let rhs = rhs.to_s(globals);
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
        };
        Ok(res)
    }

    pub(crate) fn compare_values_no_opt_inner(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<Option<std::cmp::Ordering>> {
        let res = if let Some(i) = self
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
                vm.invoke_method(globals, $op_str, lhs, &[], None)
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
    (neg, IdentId::_UMINUS)
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
        RV::BigInt(lhs) => Value::bigint(lhs.clone()),
        RV::Float(lhs) => Value::float(lhs),
        _ => {
            return vm.invoke_method(globals, IdentId::_UPLUS, lhs, &[], None);
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
            return vm.invoke_method(globals, IdentId::_BNOT, lhs, &[], None);
        }
    };
    Some(v)
}

pub(crate) fn integer_index1(base: Value, index: Value) -> Result<Value> {
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
        (RV::Fixnum(_), _) => Err(MonorubyErr::no_implicit_conversion(index, INTEGER_CLASS)),
        (RV::BigInt(base), RV::Fixnum(index)) => {
            if index < 0 {
                Ok(Value::integer(0))
            } else {
                let i = (base >> index) & num::BigInt::from(1);
                Ok(Value::bigint(i))
            }
        }
        (RV::BigInt(_), RV::BigInt(_)) => Ok(Value::integer(0)),
        (RV::BigInt(_), _) => Err(MonorubyErr::no_implicit_conversion(index, INTEGER_CLASS)),
        _ => unreachable!(),
    }
}
