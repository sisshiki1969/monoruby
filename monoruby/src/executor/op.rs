use super::*;

pub(crate) mod binary_ops;
mod sort;

pub(crate) use binary_ops::*;
use num::{BigInt, FromPrimitive};
use paste::paste;
pub(crate) use sort::*;

/// Compare BigInt with f64 precisely without losing precision.
/// Returns Ordering (Less, Equal, Greater) of bigint relative to float.
/// Returns None if the float is NaN.
pub(crate) fn bigint_cmp_float(bigint: &BigInt, f: f64) -> Option<std::cmp::Ordering> {
    use std::cmp::Ordering;
    if f.is_nan() {
        return None;
    }
    if f.is_infinite() {
        return if f > 0.0 {
            Some(Ordering::Less)
        } else {
            Some(Ordering::Greater)
        };
    }
    // Convert float to its exact BigInt truncation
    let f_trunc = f.trunc();
    let f_bigint = match BigInt::from_f64(f_trunc) {
        Some(v) => v,
        None => return Some(Ordering::Less), // shouldn't happen for finite f
    };
    match bigint.cmp(&f_bigint) {
        Ordering::Greater => Some(Ordering::Greater),
        Ordering::Less => Some(Ordering::Less),
        Ordering::Equal => {
            // BigInt == trunc(f), so compare with fractional part
            let frac = f - f_trunc;
            if frac > 0.0 {
                Some(Ordering::Less) // bigint < bigint + positive_frac
            } else if frac < 0.0 {
                Some(Ordering::Greater) // bigint > bigint + negative_frac
            } else {
                Some(Ordering::Equal)
            }
        }
    }
}

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
                rhs: Value,
                is_func_call: bool,
            ) -> Option<Value> {
                let b = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Fixnum(lhs), RV::Fixnum(rhs)) => lhs.$op(&rhs),
                    (RV::Fixnum(lhs), RV::BigInt(rhs)) => BigInt::from(lhs).$op(&rhs),
                    (RV::Fixnum(lhs), RV::Float(rhs)) => (lhs as f64).$op(&rhs),
                    (RV::BigInt(lhs), RV::Fixnum(rhs)) => lhs.$op(&BigInt::from(rhs)),
                    (RV::BigInt(lhs), RV::BigInt(rhs)) => lhs.$op(&rhs),
                    (RV::BigInt(lhs), RV::Float(rhs)) => {
                        match bigint_cmp_float(&lhs, rhs) {
                            Some(ord) => ord.$op(&std::cmp::Ordering::Equal),
                            None => false, // NaN comparisons are always false
                        }
                    }
                    (RV::Fixnum(_)| RV::BigInt(_) , _) => {
                        // Try coerce protocol for comparison, propagate exceptions
                        let coerce_id = IdentId::get_id("coerce");
                        match vm.invoke_method_if_exists(globals, coerce_id, rhs, &[lhs], None, None) {
                            Ok(Some(result)) => {
                                if let Some(ary) = result.try_array_ty() {
                                    if ary.len() == 2 {
                                        return vm.invoke_method_simple(globals, $op_str, ary[0], &[ary[1]]);
                                    }
                                }
                            }
                            Ok(None) => {}
                            Err(e) => {
                                // Propagate exception from coerce
                                vm.set_error(e);
                                return None;
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
                    (RV::Float(lhs), RV::BigInt(rhs)) => {
                        match bigint_cmp_float(&rhs, lhs) {
                            Some(ord) => ord.reverse().$op(&std::cmp::Ordering::Equal),
                            None => false, // NaN comparisons are always false
                        }
                    }
                    (RV::Float(lhs), RV::Float(rhs)) => lhs.$op(&rhs),
                    (RV::Float(_) , _) => {
                        // Try coerce protocol for comparison, propagate exceptions
                        let coerce_id = IdentId::get_id("coerce");
                        match vm.invoke_method_if_exists(globals, coerce_id, rhs, &[lhs], None, None) {
                            Ok(Some(result)) => {
                                if let Some(ary) = result.try_array_ty() {
                                    if ary.len() == 2 {
                                        return vm.invoke_method_simple(globals, $op_str, ary[0], &[ary[1]]);
                                    }
                                }
                            }
                            Ok(None) => {}
                            Err(e) => {
                                // Propagate exception from coerce
                                vm.set_error(e);
                                return None;
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
                        // Original receiver `lhs`: honor the call site's
                        // func-call flag so a private relational operator is
                        // callable only from `self OP x` (matches CRuby).
                        return vm.invoke_method(globals, $op_str, is_func_call, lhs, &[rhs], None, None);
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
                rhs: Value,
                is_func_call: bool,
            ) -> Option<Value> {
                vm.invoke_method(globals, $op_str, is_func_call, lhs, &[rhs], None, None)
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
    ///
    /// `==` with CRuby-faithful result-value semantics: the fast paths
    /// produce booleans, and a *reverse* coercion dispatch (`1 == obj`
    /// re-asking `obj == 1`) coerces the answer to a boolean exactly like
    /// CRuby's `rb_equal`, but a direct dispatch of the receiver's own
    /// `==` returns the method's value untouched (`obj == 1` evaluates to
    /// whatever `obj.==` returned).
    ///
    pub(crate) fn eq_values(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<Value> {
        // Internal callers (`Array#==`, `Hash#==`, …) compare with funcall
        // semantics, matching CRuby's `rb_equal`.
        self.eq_values_vis(globals, lhs, rhs, true)
    }

    /// `==` dispatch honoring the call site's func-call flag for a
    /// user-defined `==` (a private `==` is callable only from a func-call
    /// site). The numeric/String *reverse* dispatches keep funcall semantics
    /// (`rb_equal`), so only the direct receiver dispatch consults the flag.
    pub(crate) fn eq_values_vis(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
        is_func_call: bool,
    ) -> Result<Value> {
        let b = match (lhs.unpack(), rhs.unpack()) {
            (RV::Nil, RV::Nil) => true,
            (RV::Nil, _) => false,
            (RV::Fixnum(lhs), RV::Fixnum(rhs)) => lhs.eq(&rhs),
            (RV::Fixnum(lhs), RV::BigInt(rhs)) => BigInt::from(lhs).eq(rhs),
            (RV::Fixnum(lhs), RV::Float(rhs)) => (lhs as f64).eq(&rhs),
            (RV::Fixnum(_), _) => {
                // Reverse dispatch: try rhs == lhs (boolean-coerced, like
                // CRuby's num_equal -> rb_equal)
                return Ok(Value::bool(self.invoke_eq(globals, rhs, lhs)?));
            }
            (RV::BigInt(lhs), RV::Fixnum(rhs)) => lhs.eq(&BigInt::from(rhs)),
            (RV::BigInt(lhs), RV::BigInt(rhs)) => lhs.eq(rhs),
            (RV::BigInt(lhs), RV::Float(rhs)) => {
                bigint_cmp_float(lhs, rhs) == Some(std::cmp::Ordering::Equal)
            }
            (RV::BigInt(_), _) => {
                // Reverse dispatch: try rhs == lhs (boolean-coerced)
                return Ok(Value::bool(self.invoke_eq(globals, rhs, lhs)?));
            }
            (RV::Float(lhs), RV::Fixnum(rhs)) => lhs.eq(&(rhs as f64)),
            (RV::Float(lhs), RV::BigInt(rhs)) => {
                bigint_cmp_float(rhs, lhs) == Some(std::cmp::Ordering::Equal)
            }
            (RV::Float(lhs), RV::Float(rhs)) => lhs.eq(&rhs),
            (RV::Float(_), _) => {
                // Reverse dispatch: try rhs == lhs (boolean-coerced)
                return Ok(Value::bool(self.invoke_eq(globals, rhs, lhs)?));
            }
            (RV::Bool(lhs), RV::Bool(rhs)) => lhs.eq(&rhs),
            (RV::Bool(_), _) => false,
            (RV::Symbol(lhs), RV::Symbol(rhs)) => lhs.eq(&rhs),
            (RV::Symbol(_), _) => false,
            (RV::String(lhs), RV::String(rhs)) => {
                // CRuby `String#==` is content-equal AND encoding-
                // compatible: same bytes with incompatible encodings
                // (e.g. UTF-8 vs UTF-32LE on non-ASCII content)
                // compare *unequal*. The empty-string special case
                // in `compatible_encoding` keeps `"".UTF-16BE ==
                // "".UTF-8` true.
                lhs.eq(rhs) && lhs.compatible_encoding(rhs).is_some()
            }
            (RV::String(_), _) => {
                // CRuby's `String#==` dispatches to `rhs == self` when
                // the rhs *responds to* `to_str` (it does NOT actually
                // call `to_str`). Without this branch, the fast path
                // would short-circuit to `false` and never reach the
                // builtin, masking custom `==` defined alongside
                // `to_str` on a mock. `no_to_str` is a version-stamped
                // memo, so the common `str == nil` case costs a load
                // instead of a method-table probe.
                if globals.store.no_to_str(rhs.class(), Globals::class_version()) {
                    false
                } else {
                    return Ok(Value::bool(self.invoke_eq(globals, rhs, lhs)?));
                }
            }
            _ => return self.invoke_eq_raw_vis(globals, lhs, rhs, is_func_call),
        };
        Ok(Value::bool(b))
    }

    pub(crate) fn eq_values_bool(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<bool> {
        Ok(self.eq_values(globals, lhs, rhs)?.as_bool())
    }

    pub(crate) fn eq_values_no_opt(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
        is_func_call: bool,
    ) -> Result<Value> {
        self.invoke_eq_raw_vis(globals, lhs, rhs, is_func_call)
    }

    ///
    /// `!=` with CRuby-faithful result-value semantics: while `!=`
    /// resolves to a basic op / the default `BasicObject#!=`, the result
    /// is the boolean negation of `==`; a custom `!=` returns the
    /// method's value untouched.
    ///
    pub(crate) fn ne_values(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<Value> {
        // Internal callers use funcall semantics, matching `rb_equal`-style `!=`.
        self.ne_values_vis(globals, lhs, rhs, true)
    }

    /// `!=` dispatch honoring the call site's func-call flag for a redefined
    /// `!=` (or the private-`==` it negates).
    pub(crate) fn ne_values_vis(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
        is_func_call: bool,
    ) -> Result<Value> {
        // Check if the receiver has a custom != method (not a basic op /
        // the default BasicObject#!=). If so, dispatch to it (honoring its
        // visibility) instead of negating ==. `custom_neq` is memoized per
        // class_version, so the common unredefined case costs a load
        // instead of a method-table probe per comparison.
        let class_id = lhs.class();
        if globals
            .store
            .custom_neq(class_id, Globals::class_version())
            .is_some()
        {
            let func_id = self.find_method(globals, lhs, IdentId::_NEQ, is_func_call)?;
            return self.invoke_func_inner(globals, func_id, lhs, &[rhs], None, None);
        }
        Ok(Value::bool(!self.eq_values_vis(globals, lhs, rhs, is_func_call)?.as_bool()))
    }

    pub(crate) fn ne_values_bool(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
    ) -> Result<bool> {
        Ok(self.ne_values(globals, lhs, rhs)?.as_bool())
    }

    pub(crate) fn ne_values_no_opt(
        &mut self,
        globals: &mut Globals,
        lhs: Value,
        rhs: Value,
        is_func_call: bool,
    ) -> Result<Value> {
        let func_id = self.find_method(globals, lhs, IdentId::_NEQ, is_func_call)?;
        self.invoke_func_inner(globals, func_id, lhs, &[rhs], None, None)
    }
}

// `==` / `!=` wrappers return the dispatched method's value as-is (the
// fast paths inside `eq_values` / `ne_values` produce booleans).
macro_rules! eq_values {
    ($op:ident) => {
        paste! {
            pub(crate) extern "C" fn [<cmp_ $op _values>](
                vm: &mut Executor,
                globals: &mut Globals,
                lhs: Value,
                rhs: Value,
                is_func_call: bool,
            ) -> Option<Value> {
                match vm.[<$op _values_vis>](globals, lhs, rhs, is_func_call) {
                    Ok(v) => Some(v),
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
                rhs: Value,
                is_func_call: bool,
            ) -> Option<Value> {
                match vm.[<$op _values_no_opt>](globals, lhs, rhs, is_func_call) {
                    Ok(v) => Some(v),
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

/// Plain `a === b` (the non-optimizable TEq opcode): a public-only call,
/// except when the receiver is the literal `self` — the call site's
/// func-call flag carries that, so `self === b` may reach a private `===`.
pub(crate) extern "C" fn cmp_teq_values(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
    is_func_call: bool,
) -> Option<Value> {
    cmp_teq_values_impl(vm, globals, lhs, rhs, is_func_call)
}

/// `===` dispatch for the *optimizable* TEq opcode, which bytecodegen
/// only emits for case/when and rescue matching: CRuby dispatches
/// those `===` calls with funcall semantics, so a private `===` is
/// allowed regardless of the receiver (`is_func_call` is unused).
pub(crate) extern "C" fn cmp_teq_case_values(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
    _is_func_call: bool,
) -> Option<Value> {
    cmp_teq_values_impl(vm, globals, lhs, rhs, true)
}

/// `===` dispatch for rescue-clause matching (the RescueTEq opcode):
/// the clause must be a Class or Module — CRuby raises TypeError
/// before any `===` dispatch — and the call itself has funcall
/// semantics like case/when (`is_func_call` is unused).
pub(crate) extern "C" fn cmp_teq_rescue_values(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
    _is_func_call: bool,
) -> Option<Value> {
    if lhs.is_class_or_module().is_none() {
        vm.set_error(MonorubyErr::typeerr(
            "class or module required for rescue clause",
        ));
        return None;
    }
    cmp_teq_values_impl(vm, globals, lhs, rhs, true)
}

fn cmp_teq_values_impl(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
    private_ok: bool,
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
        (RV::BigInt(lhs), RV::Float(rhs)) => {
            bigint_cmp_float(lhs, rhs) == Some(std::cmp::Ordering::Equal)
        }
        (RV::BigInt(_), _) => {
            // Reverse dispatch for Integer === non-numeric
            return vm.invoke_method_simple(globals, IdentId::_EQ, rhs, &[lhs]);
        }
        (RV::Float(lhs), RV::Fixnum(rhs)) => lhs.eq(&(rhs as f64)),
        (RV::Float(lhs), RV::BigInt(rhs)) => {
            bigint_cmp_float(rhs, lhs) == Some(std::cmp::Ordering::Equal)
        }
        (RV::Float(lhs), RV::Float(rhs)) => lhs.eq(&rhs),
        (RV::Float(_), _) => {
            // Reverse dispatch for Float === non-numeric
            return vm.invoke_method_simple(globals, IdentId::_EQ, rhs, &[lhs]);
        }
        _ => {
            return vm.invoke_method(globals, IdentId::_TEQ, private_ok, lhs, &[rhs], None, None);
        }
    };
    Some(Value::bool(b))
}

pub(crate) extern "C" fn cmp_teq_values_no_opt(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
    is_func_call: bool,
) -> Option<Value> {
    vm.invoke_method(globals, IdentId::_TEQ, is_func_call, lhs, &[rhs], None, None)
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
        (RV::Fixnum(_), _) => {
            // Reverse dispatch: try rhs == lhs
            return vm.invoke_eq(globals, rhs, lhs);
        }
        (RV::BigInt(lhs), RV::Fixnum(rhs)) => lhs.eq(&BigInt::from(rhs)),
        (RV::BigInt(lhs), RV::BigInt(rhs)) => lhs.eq(rhs),
        (RV::BigInt(lhs), RV::Float(rhs)) => {
            bigint_cmp_float(lhs, rhs) == Some(std::cmp::Ordering::Equal)
        }
        (RV::BigInt(_), _) => {
            // Reverse dispatch: try rhs == lhs
            return vm.invoke_eq(globals, rhs, lhs);
        }
        (RV::Float(lhs), RV::Fixnum(rhs)) => lhs.eq(&(rhs as f64)),
        (RV::Float(lhs), RV::BigInt(rhs)) => {
            bigint_cmp_float(rhs, lhs) == Some(std::cmp::Ordering::Equal)
        }
        (RV::Float(lhs), RV::Float(rhs)) => lhs.eq(&rhs),
        (RV::Float(_), _) => {
            // Reverse dispatch: try rhs == lhs
            return vm.invoke_eq(globals, rhs, lhs);
        }
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
    /// Run `Kernel#at_exit` handlers (LIFO) and `ObjectSpace` finalizers
    /// at program termination. Invoked once from `Globals::run` after the
    /// script body finishes, regardless of how it finished.
    ///
    /// Each handler is dispatched via `#call`, matching CRuby: at_exit
    /// blocks are Procs, finalizers may be any callable (Proc, Method, or
    /// an object with `call`). Finalizers receive the object's id.
    ///
    /// Errors raised inside a handler do not abort the remaining ones,
    /// but they do affect the process exit status, which is what the
    /// returned value reports (`None` = leave the script's own status
    /// alone):
    ///
    /// - `SystemExit` (`exit` called from within a handler) makes its
    ///   status the process exit status, overriding the main script's.
    /// - Any other exception is printed like an uncaught exception, is
    ///   exposed as `$!` to the handlers that run after it, and makes
    ///   the process fail with status 1 (unless a `SystemExit` chose a
    ///   status explicitly).
    pub(crate) fn run_exit_handlers(&mut self, globals: &mut Globals) -> Option<i32> {
        if globals.at_exit_handlers.is_empty() && globals.finalizers.is_empty() {
            return None;
        }
        let call = IdentId::get_id("call");
        let mut status_override = None;
        let mut handler_failed = false;
        // at_exit handlers run in reverse registration order.
        while let Some(handler) = globals.at_exit_handlers.pop() {
            if let Err(err) = self.invoke_method_inner(globals, call, handler, &[], None, None) {
                if let MonorubyErrKind::SystemExit(status) = err.kind() {
                    status_override = Some(*status as i32);
                    continue;
                }
                err.show_error_message_and_all_loc(&globals.store);
                // Expose the exception as `$!` to the remaining
                // handlers (mirrors CRuby, and is what the
                // `$!.message` at_exit specs rely on).
                self.set_error(err);
                let err_val = self.take_ex_obj(globals);
                self.set_errinfo(err_val);
                handler_failed = true;
            }
        }
        // Finalizers run last. Draining the vector (rather than iterating a
        // snapshot) lets a finalizer define further finalizers that then
        // run too, matching CRuby.
        while let Some((id, callable)) = globals.finalizers.pop() {
            let arg = Value::integer(id as i64);
            if let Err(err) = self.invoke_method_inner(globals, call, callable, &[arg], None, None) {
                self.report_finalizer_error(globals, err);
            }
        }
        status_override.or(if handler_failed { Some(1) } else { None })
    }

    /// Report (or swallow) an error escaping an `ObjectSpace` finalizer.
    fn report_finalizer_error(&mut self, globals: &mut Globals, err: MonorubyErr) {
        // `exit` from within a finalizer simply stops that finalizer;
        // the remaining ones still run.
        if matches!(err.kind(), MonorubyErrKind::SystemExit(_)) {
            return;
        }
        // CRuby only warns about a finalizer exception when `$VERBOSE`
        // is not `nil`.
        let verbose = globals
            .get_gvar(IdentId::get_id("$VERBOSE"))
            .is_some_and(|v| !v.is_nil());
        if verbose {
            let msg = format!("warning: Exception in finalizer: {}", err.message());
            let _ = self.ruby_warn(globals, &msg);
        }
    }

    /// Emit a Ruby warning by writing to `$stderr`.
    pub(crate) fn ruby_warn(&mut self, globals: &mut Globals, msg: &str) -> Result<()> {
        let stderr_id = IdentId::get_id("$stderr");
        let stderr = globals.get_gvar(stderr_id).unwrap_or(Value::nil());
        let write_id = IdentId::get_id("write");
        let msg_val = Value::string(format!("{}\n", msg));
        self.invoke_method_inner(globals, write_id, stderr, &[msg_val], None, None)?;
        Ok(())
    }

    ///
    /// The `"file:line"` of the nearest Ruby (iseq) caller of the current
    /// native builtin frame.
    ///
    /// Used for CRuby-style warnings that prefix the caller's location
    /// (e.g. `Array#inject`'s "given block not used", emitted with an
    /// implicit `uplevel: 1`). Returns `None` when no iseq caller is found.
    ///
    pub(crate) fn nearest_caller_location(&self, store: &Store) -> Option<String> {
        // Walk callers via each inner frame's saved call-site pc, exactly
        // like `Kernel#caller` / `complete_backtrace_for_rescue`, skipping
        // native frames until the first Ruby (iseq) caller.
        let mut inner_cfp = self.cfp();
        let mut cfp = inner_cfp.prev()?;
        loop {
            let func_id = cfp.lfp().func_id();
            if let Some(iseq_id) = store[func_id].is_iseq() {
                let info = &store[iseq_id];
                let loc = {
                    let slot = inner_cfp.caller_pc_slot();
                    if slot != 0
                        && slot % 8 == 0
                        && let Some(pc) =
                            unsafe { crate::bytecode::BytecodePtr::from_raw(slot as *mut _) }
                        && info.contains_pc(pc)
                    {
                        info.sourcemap[info.get_pc_index(Some(pc)).to_usize()]
                    } else {
                        info.loc
                    }
                };
                return Some(format!(
                    "{}:{}",
                    info.sourceinfo.file_name(),
                    info.sourceinfo.get_line(&loc)
                ));
            }
            inner_cfp = cfp;
            cfp = cfp.prev()?;
        }
    }

    ///
    /// Like [`Self::ruby_warn`], but prefixes the message with the nearest
    /// Ruby caller's `"file:line: "` (CRuby's `rb_warn` / `warn(uplevel: 1)`
    /// behaviour). Falls back to the bare message when no caller is found.
    ///
    pub(crate) fn ruby_warn_caller(&mut self, globals: &mut Globals, msg: &str) -> Result<()> {
        match self.nearest_caller_location(&globals.store) {
            Some(loc) => self.ruby_warn(globals, &format!("{loc}: {msg}")),
            None => self.ruby_warn(globals, msg),
        }
    }

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
            (RV::BigInt(lhs), RV::Float(rhs)) => bigint_cmp_float(lhs, rhs),
            (RV::BigInt(_), _) => None,
            (RV::Float(lhs), RV::Fixnum(rhs)) => lhs.partial_cmp(&(rhs as f64)),
            (RV::Float(lhs), RV::BigInt(rhs)) => {
                bigint_cmp_float(rhs, lhs).map(|ord| ord.reverse())
            }
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
                is_func_call: bool,
            ) -> Option<Value> {
                vm.invoke_method(globals, $op_str, is_func_call, lhs, &[], None, None)
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
    is_func_call: bool,
) -> Option<Value> {
    let v = match lhs.unpack() {
        RV::Fixnum(lhs) => match lhs.checked_neg() {
            Some(lhs) => Value::integer(lhs),
            None => Value::bigint(-BigInt::from(lhs)),
        },
        RV::BigInt(lhs) => Value::bigint(-lhs),
        RV::Float(lhs) => Value::float(-lhs),
        RV::Complex(c) => {
            let (re, im) = (c.re, c.im);
            let is_builtin_real =
                |v: Value| matches!(v.unpack(), RV::Fixnum(_) | RV::BigInt(_) | RV::Float(_));
            if is_builtin_real(re.get()) && is_builtin_real(im.get()) {
                Value::complex(-re, -im)
            } else {
                // Custom Numeric components: dispatch `-@` to each part via
                // `Complex#-@` (`neg_op`, not `neg_value`), so no recursion.
                return vm.invoke_method(globals, IdentId::_UMINUS, is_func_call, lhs, &[], None, None);
            }
        }
        _ => {
            return vm.invoke_method(globals, IdentId::_UMINUS, is_func_call, lhs, &[], None, None);
        }
    };
    Some(v)
}

pub(crate) extern "C" fn pos_value(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    is_func_call: bool,
) -> Option<Value> {
    let v = match lhs.unpack() {
        RV::Fixnum(lhs) => Value::integer(lhs),
        RV::BigInt(lhs) => Value::bigint(lhs.clone()),
        RV::Float(lhs) => Value::float(lhs),
        _ => {
            return vm.invoke_method(globals, IdentId::_UPLUS, is_func_call, lhs, &[], None, None);
        }
    };
    Some(v)
}

pub(crate) extern "C" fn not_value(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    is_func_call: bool,
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
            return vm.invoke_method(globals, IdentId::_NOT, is_func_call, lhs, &[], None, None);
        }
    };
    Some(v)
}

pub(crate) extern "C" fn bitnot_value(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    is_func_call: bool,
) -> Option<Value> {
    let v = match lhs.unpack() {
        RV::Fixnum(lhs) => Value::integer(!lhs),
        RV::BigInt(lhs) => Value::bigint(!lhs),
        _ => {
            return vm.invoke_method(globals, IdentId::_BNOT, is_func_call, lhs, &[], None, None);
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
