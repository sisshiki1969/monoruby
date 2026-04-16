use num::BigInt;

use super::*;

//
// Rational class
//

pub(super) fn init(globals: &mut Globals, numeric: Module) {
    globals.define_builtin_class(
        "Rational",
        RATIONAL_CLASS,
        numeric,
        OBJECT_CLASS,
        ObjTy::RATIONAL,
    );
    globals.define_builtin_func(RATIONAL_CLASS, "numerator", numerator, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "denominator", denominator, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "to_f", to_f, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "to_i", to_i, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "to_s", to_s, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "inspect", inspect, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "==", eq, 1);
    globals.define_builtin_func(RATIONAL_CLASS, "!=", ne, 1);
    globals.define_builtin_func(RATIONAL_CLASS, "<=>", cmp, 1);
    globals.define_builtin_func(RATIONAL_CLASS, "+", add, 1);
    globals.define_builtin_func(RATIONAL_CLASS, "-", sub, 1);
    globals.define_builtin_func(RATIONAL_CLASS, "*", mul, 1);
    globals.define_builtin_funcs(RATIONAL_CLASS, "/", &["quo"], div, 1);
    globals.define_builtin_func(RATIONAL_CLASS, "**", pow, 1);
    globals.define_builtin_func(RATIONAL_CLASS, "-@", neg, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "abs", abs, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "zero?", zero_, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "positive?", positive_, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "negative?", negative_, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "hash", hash, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "eql?", eql_, 1);
    globals.define_builtin_func_with(RATIONAL_CLASS, "floor", rat_floor, 0, 1, false);
    globals.define_builtin_func_with(RATIONAL_CLASS, "ceil", rat_ceil, 0, 1, false);
    globals.define_builtin_func_with(RATIONAL_CLASS, "truncate", rat_truncate, 0, 1, false);
    globals.define_builtin_func_with_kw(
        RATIONAL_CLASS, "round", rat_round, 0, 1, false, &["half"], false,
    );
    globals.define_builtin_class_func(RATIONAL_CLASS, "__allocate", allocate, 2);
    globals.store[RATIONAL_CLASS].clear_alloc_func();
}

/// Rational.__allocate(num, den) — internal constructor from Ruby
#[monoruby_builtin]
fn allocate(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let num_val = lfp.arg(0);
    let den_val = lfp.arg(1);
    let num = match num_val.unpack() {
        RV::Fixnum(i) => BigInt::from(i),
        RV::BigInt(b) => b.clone(),
        _ => return Err(MonorubyErr::typeerr("not an integer")),
    };
    let den = match den_val.unpack() {
        RV::Fixnum(i) => BigInt::from(i),
        RV::BigInt(b) => b.clone(),
        _ => return Err(MonorubyErr::typeerr("not an integer")),
    };
    Ok(Value::rational_from_bigint(num, den))
}

/// Helper: extract RationalInner from self
fn self_rat<'a>(lfp: Lfp) -> &'a RationalInner {
    // SAFETY: The Lfp holds a reference to the stack frame which outlives this call.
    unsafe { &*(lfp.self_val().as_rational() as *const RationalInner) }
}

/// Helper: convert Value to RationalInner for arithmetic
fn val_to_rat(_: &mut Executor, globals: &mut Globals, v: Value) -> Result<RationalInner> {
    if let Some(r) = v.try_rational() {
        return Ok(r.clone());
    }
    match v.unpack() {
        RV::Fixnum(i) => Ok(RationalInner::new(i, 1)),
        RV::BigInt(b) => Ok(RationalInner::new_bigint(b.clone(), BigInt::from(1))),
        _ => Err(MonorubyErr::cant_convert_into_float(globals, v)),
    }
}

#[monoruby_builtin]
fn numerator(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(self_rat(lfp).num_as_value())
}

#[monoruby_builtin]
fn denominator(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(self_rat(lfp).den_as_value())
}

#[monoruby_builtin]
fn to_f(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::float(self_rat(lfp).to_f()))
}

#[monoruby_builtin]
fn to_i(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let i = self_rat(lfp).to_i();
    Ok(Value::bigint(i))
}

#[monoruby_builtin]
fn to_s(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::string(self_rat(lfp).to_s()))
}

#[monoruby_builtin]
fn inspect(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::string(self_rat(lfp).inspect()))
}

#[monoruby_builtin]
fn eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = self_rat(lfp);
    let rhs = lfp.arg(0);
    if let Some(r) = rhs.try_rational() {
        return Ok(Value::bool(lhs.eq(r)));
    }
    match rhs.unpack() {
        RV::Fixnum(i) => Ok(Value::bool(
            lhs.den() == &BigInt::from(1) && lhs.num() == &BigInt::from(i),
        )),
        RV::BigInt(b) => Ok(Value::bool(lhs.den() == &BigInt::from(1) && lhs.num() == b)),
        RV::Float(f) => Ok(Value::bool(lhs.to_f() == f)),
        _ => {
            // Reverse dispatch
            let eq_id = IdentId::get_id("==");
            let result =
                vm.invoke_method_inner(globals, eq_id, rhs, &[lfp.self_val()], None, None)?;
            Ok(Value::bool(result.as_bool()))
        }
    }
}

#[monoruby_builtin]
fn ne(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = self_rat(lfp);
    let rhs = lfp.arg(0);
    if let Some(r) = rhs.try_rational() {
        return Ok(Value::bool(!lhs.eq(r)));
    }
    match rhs.unpack() {
        RV::Fixnum(i) => Ok(Value::bool(
            !(lhs.den() == &BigInt::from(1) && lhs.num() == &BigInt::from(i)),
        )),
        RV::Float(f) => Ok(Value::bool(lhs.to_f() != f)),
        _ => Ok(Value::bool(true)),
    }
}

#[monoruby_builtin]
fn cmp(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = self_rat(lfp);
    let rhs = lfp.arg(0);
    if let Some(r) = rhs.try_rational() {
        return Ok(Value::integer(match lhs.cmp(r) {
            std::cmp::Ordering::Less => -1,
            std::cmp::Ordering::Equal => 0,
            std::cmp::Ordering::Greater => 1,
        }));
    }
    match rhs.unpack() {
        RV::Fixnum(i) => {
            let other = RationalInner::new(i, 1);
            Ok(Value::integer(match lhs.cmp(&other) {
                std::cmp::Ordering::Less => -1,
                std::cmp::Ordering::Equal => 0,
                std::cmp::Ordering::Greater => 1,
            }))
        }
        RV::BigInt(b) => {
            let other = RationalInner::new_bigint(b.clone(), BigInt::from(1));
            Ok(Value::integer(match lhs.cmp(&other) {
                std::cmp::Ordering::Less => -1,
                std::cmp::Ordering::Equal => 0,
                std::cmp::Ordering::Greater => 1,
            }))
        }
        RV::Float(f) => match lhs.to_f().partial_cmp(&f) {
            Some(ord) => Ok(Value::integer(match ord {
                std::cmp::Ordering::Less => -1,
                std::cmp::Ordering::Equal => 0,
                std::cmp::Ordering::Greater => 1,
            })),
            None => Ok(Value::nil()),
        },
        _ => {
            // Try coerce protocol
            let coerce_id = IdentId::get_id("coerce");
            match vm.invoke_method_if_exists(globals, coerce_id, rhs, &[lfp.self_val()], None, None) {
                Ok(Some(result)) => {
                    if let Some(ary) = result.try_array_ty() {
                        if ary.len() == 2 {
                            let cmp_id = IdentId::get_id("<=>");
                            return vm.invoke_method_inner(globals, cmp_id, ary[0], &[ary[1]], None, None);
                        }
                    }
                    Ok(Value::nil())
                }
                Ok(None) => Ok(Value::nil()),
                Err(e) => Err(e),
            }
        }
    }
}

#[monoruby_builtin]
fn add(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = self_rat(lfp);
    let rhs = lfp.arg(0);
    if let Some(r) = rhs.try_rational() {
        return Ok(Value::rational_from_inner(lhs.add(r)));
    }
    match rhs.unpack() {
        RV::Fixnum(_) | RV::BigInt(_) => {
            let r = val_to_rat(vm, globals, rhs)?;
            Ok(Value::rational_from_inner(lhs.add(&r)))
        }
        RV::Float(f) => Ok(Value::float(lhs.to_f() + f)),
        _ => {
            let coerce_id = IdentId::get_id("coerce");
            let result =
                vm.invoke_method_inner(globals, coerce_id, rhs, &[lfp.self_val()], None, None)?;
            if let Some(ary) = result.try_array_ty() {
                if ary.len() == 2 {
                    let add_id = IdentId::get_id("+");
                    return vm.invoke_method_inner(globals, add_id, ary[0], &[ary[1]], None, None);
                }
            }
            Err(MonorubyErr::typeerr(format!(
                "{} can't be coerced into Rational",
                rhs.get_real_class_name(&globals.store)
            )))
        }
    }
}

#[monoruby_builtin]
fn sub(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = self_rat(lfp);
    let rhs = lfp.arg(0);
    if let Some(r) = rhs.try_rational() {
        return Ok(Value::rational_from_inner(lhs.sub(r)));
    }
    match rhs.unpack() {
        RV::Fixnum(_) | RV::BigInt(_) => {
            let r = val_to_rat(vm, globals, rhs)?;
            Ok(Value::rational_from_inner(lhs.sub(&r)))
        }
        RV::Float(f) => Ok(Value::float(lhs.to_f() - f)),
        _ => {
            let coerce_id = IdentId::get_id("coerce");
            let result =
                vm.invoke_method_inner(globals, coerce_id, rhs, &[lfp.self_val()], None, None)?;
            if let Some(ary) = result.try_array_ty() {
                if ary.len() == 2 {
                    let sub_id = IdentId::get_id("-");
                    return vm.invoke_method_inner(globals, sub_id, ary[0], &[ary[1]], None, None);
                }
            }
            Err(MonorubyErr::typeerr(format!(
                "{} can't be coerced into Rational",
                rhs.get_real_class_name(&globals.store)
            )))
        }
    }
}

#[monoruby_builtin]
fn mul(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = self_rat(lfp);
    let rhs = lfp.arg(0);
    if let Some(r) = rhs.try_rational() {
        return Ok(Value::rational_from_inner(lhs.mul(r)));
    }
    match rhs.unpack() {
        RV::Fixnum(_) | RV::BigInt(_) => {
            let r = val_to_rat(vm, globals, rhs)?;
            Ok(Value::rational_from_inner(lhs.mul(&r)))
        }
        RV::Float(f) => Ok(Value::float(lhs.to_f() * f)),
        _ => {
            let coerce_id = IdentId::get_id("coerce");
            let result =
                vm.invoke_method_inner(globals, coerce_id, rhs, &[lfp.self_val()], None, None)?;
            if let Some(ary) = result.try_array_ty() {
                if ary.len() == 2 {
                    let mul_id = IdentId::get_id("*");
                    return vm.invoke_method_inner(globals, mul_id, ary[0], &[ary[1]], None, None);
                }
            }
            Err(MonorubyErr::typeerr(format!(
                "{} can't be coerced into Rational",
                rhs.get_real_class_name(&globals.store)
            )))
        }
    }
}

#[monoruby_builtin]
fn div(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = self_rat(lfp);
    let rhs = lfp.arg(0);
    if let Some(r) = rhs.try_rational() {
        return Ok(Value::rational_from_inner(lhs.div(r)?));
    }
    match rhs.unpack() {
        RV::Fixnum(_) | RV::BigInt(_) => {
            let r = val_to_rat(vm, globals, rhs)?;
            Ok(Value::rational_from_inner(lhs.div(&r)?))
        }
        RV::Float(f) => {
            Ok(Value::float(lhs.to_f() / f))
        }
        _ => {
            let coerce_id = IdentId::get_id("coerce");
            let result =
                vm.invoke_method_inner(globals, coerce_id, rhs, &[lfp.self_val()], None, None)?;
            if let Some(ary) = result.try_array_ty() {
                if ary.len() == 2 {
                    let div_id = IdentId::get_id("/");
                    return vm.invoke_method_inner(globals, div_id, ary[0], &[ary[1]], None, None);
                }
            }
            Err(MonorubyErr::typeerr(format!(
                "{} can't be coerced into Rational",
                rhs.get_real_class_name(&globals.store)
            )))
        }
    }
}

#[monoruby_builtin]
fn pow(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = self_rat(lfp).clone();
    let rhs = lfp.arg(0);
    match rhs.unpack() {
        RV::Fixnum(exp) => pow_by_integer(&lhs, &BigInt::from(exp)),
        RV::BigInt(exp) => pow_by_integer(&lhs, exp),
        RV::Float(f) => {
            let base = lhs.to_f();
            let result = base.powf(f);
            if result.is_nan() && base < 0.0 {
                // Negative base with fractional exponent: return Complex
                let abs_result = (-base).powf(f);
                let theta = f * std::f64::consts::PI;
                Ok(Value::complex(abs_result * theta.cos(), abs_result * theta.sin()))
            } else {
                Ok(Value::float(result))
            }
        }
        _ => {
            if let Some(r) = rhs.try_rational() {
                if lhs.is_zero() && r.is_negative() {
                    return Err(MonorubyErr::divide_by_zero());
                }
                if *r.den() == BigInt::from(1) {
                    return pow_by_integer(&lhs, r.num());
                }
                let base = lhs.to_f();
                let exp = r.to_f();
                let result = base.powf(exp);
                if result.is_nan() && base < 0.0 {
                    let abs_result = (-base).powf(exp);
                    let theta = exp * std::f64::consts::PI;
                    return Ok(Value::complex(abs_result * theta.cos(), abs_result * theta.sin()));
                }
                return Ok(Value::float(result));
            }
            // Fallback: use coerce protocol when the argument is not a Numeric.
            let coerce_id = IdentId::get_id("coerce");
            if globals.check_method(rhs, coerce_id).is_none() {
                return Err(MonorubyErr::typeerr(format!(
                    "{} can't be coerced into Rational",
                    rhs.get_real_class_name(&globals.store),
                )));
            }
            let result = vm.invoke_method_inner(globals, coerce_id, rhs, &[lfp.self_val()], None, None)?;
            if let Some(ary) = result.try_array_ty() {
                if ary.len() == 2 {
                    let pow_id = IdentId::get_id("**");
                    return vm.invoke_method_inner(globals, pow_id, ary[0], &[ary[1]], None, None);
                }
            }
            Err(MonorubyErr::typeerr(format!(
                "{} can't be coerced into Rational",
                rhs.get_real_class_name(&globals.store),
            )))
        }
    }
}

/// Raise a Rational to an integer (BigInt) power.
///
/// Handles the Ruby 3.4+ behaviour of raising `ArgumentError` for very large
/// exponents (|lhs| > 1), zero-base special cases, and pure integer exponents.
fn pow_by_integer(lhs: &RationalInner, exp: &BigInt) -> Result<Value> {
    use num::ToPrimitive;
    use num::Zero;
    use num::bigint::Sign;
    if exp.is_zero() {
        return Ok(Value::rational_from_bigint(BigInt::from(1), BigInt::from(1)));
    }
    if lhs.is_zero() {
        if exp.sign() == Sign::Minus {
            return Err(MonorubyErr::divide_by_zero());
        }
        return Ok(Value::rational_from_bigint(BigInt::from(0), BigInt::from(1)));
    }
    // |lhs| == 1 shortcut: Rational(1) stays 1; Rational(-1) alternates.
    let is_one = lhs.num() == &BigInt::from(1) && lhs.den() == &BigInt::from(1);
    let is_neg_one = lhs.num() == &BigInt::from(-1) && lhs.den() == &BigInt::from(1);
    if is_one {
        return Ok(Value::rational_from_bigint(BigInt::from(1), BigInt::from(1)));
    }
    if is_neg_one {
        // (-1)^exp is 1 if exp even, -1 if odd.
        let is_even = (exp % 2u32).is_zero();
        let result = if is_even { 1 } else { -1 };
        return Ok(Value::rational_from_bigint(BigInt::from(result), BigInt::from(1)));
    }
    // Exponent must fit in i64 for finite computation.
    let exp_i64 = match exp.to_i64() {
        Some(n) => n,
        None => {
            return Err(MonorubyErr::argumenterr("exponent is too large"));
        }
    };
    if exp_i64 >= 0 {
        let e = exp_i64 as u32;
        let n = lhs.num().pow(e);
        let d = lhs.den().pow(e);
        Ok(Value::rational_from_bigint(n, d))
    } else {
        let e = (-exp_i64) as u32;
        let n = lhs.den().pow(e);
        let d = lhs.num().pow(e);
        Ok(Value::rational_from_bigint(n, d))
    }
}

/// Coerce ndigits argument to i64 (supports to_int coercion).
fn coerce_ndigits(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<i64> {
    match lfp.try_arg(0) {
        Some(v) => v.coerce_to_int_i64(vm, globals),
        None => Ok(0),
    }
}

/// Convert RationalFloorResult to Value.
fn floor_result_to_value(r: rvalue::RationalFloorResult) -> Value {
    match r {
        rvalue::RationalFloorResult::Integer(i) => Value::bigint(i),
        rvalue::RationalFloorResult::Rational(r) => Value::rational_from_inner(r),
    }
}

///
/// ### Rational#floor
///
/// - floor(ndigits = 0) -> Integer | Rational
///
/// [https://docs.ruby-lang.org/ja/latest/method/Rational/i/floor.html]
#[monoruby_builtin]
fn rat_floor(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let r = self_rat(lfp);
    let ndigits = coerce_ndigits(vm, globals, lfp)?;
    Ok(floor_result_to_value(r.rational_floor(ndigits)))
}

///
/// ### Rational#ceil
///
/// - ceil(ndigits = 0) -> Integer | Rational
///
/// [https://docs.ruby-lang.org/ja/latest/method/Rational/i/ceil.html]
#[monoruby_builtin]
fn rat_ceil(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let r = self_rat(lfp);
    let ndigits = coerce_ndigits(vm, globals, lfp)?;
    Ok(floor_result_to_value(r.rational_ceil(ndigits)))
}

///
/// ### Rational#truncate
///
/// - truncate(ndigits = 0) -> Integer | Rational
///
/// [https://docs.ruby-lang.org/ja/latest/method/Rational/i/truncate.html]
#[monoruby_builtin]
fn rat_truncate(
    _: &mut Executor,
    _: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let r = self_rat(lfp);
    // Rational#truncate is strict: it does not invoke to_int.
    let ndigits = match lfp.try_arg(0) {
        Some(v) => match v.unpack() {
            RV::Fixnum(i) => i,
            RV::BigInt(b) => {
                use num::ToPrimitive;
                b.to_i64().unwrap_or(if b.sign() == num::bigint::Sign::Minus {
                    i64::MIN
                } else {
                    i64::MAX
                })
            }
            _ => return Err(MonorubyErr::typeerr("not an integer")),
        },
        None => 0,
    };
    Ok(floor_result_to_value(r.rational_truncate(ndigits)))
}

///
/// ### Rational#round
///
/// - round(ndigits = 0, half: :up) -> Integer | Rational
///
/// [https://docs.ruby-lang.org/ja/latest/method/Rational/i/round.html]
#[monoruby_builtin]
fn rat_round(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let r = self_rat(lfp);
    let ndigits = coerce_ndigits(vm, globals, lfp)?;
    let half = if let Some(kw_val) = lfp.try_arg(1) {
        super::float::parse_half_mode(kw_val)?
    } else {
        None
    };
    let half_str = half.map(|h| match h {
        super::float::RoundHalf::Up => "up",
        super::float::RoundHalf::Down => "down",
        super::float::RoundHalf::Even => "even",
    });
    Ok(floor_result_to_value(r.rational_round(ndigits, half_str)))
}

#[monoruby_builtin]
fn neg(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::rational_from_inner(self_rat(lfp).neg()))
}

#[monoruby_builtin]
fn abs(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::rational_from_inner(self_rat(lfp).abs()))
}

#[monoruby_builtin]
fn zero_(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(self_rat(lfp).is_zero()))
}

#[monoruby_builtin]
fn positive_(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(
        !self_rat(lfp).is_negative() && !self_rat(lfp).is_zero(),
    ))
}

#[monoruby_builtin]
fn negative_(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(self_rat(lfp).is_negative()))
}

#[monoruby_builtin]
fn hash(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::hash::{Hash, Hasher};
    let r = self_rat(lfp);
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    r.num().hash(&mut hasher);
    r.den().hash(&mut hasher);
    Ok(Value::integer(hasher.finish() as i64))
}

#[monoruby_builtin]
fn eql_(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = self_rat(lfp);
    let rhs = lfp.arg(0);
    if let Some(r) = rhs.try_rational() {
        Ok(Value::bool(lhs.eq(r)))
    } else {
        Ok(Value::bool(false))
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn rational_basic() {
        run_test("Rational(3, 4).numerator");
        run_test("Rational(3, 4).denominator");
        run_test("Rational(6, 4).numerator");
    }

    #[test]
    fn rational_arithmetic() {
        run_test("(Rational(1, 2) + Rational(1, 3)).to_s");
        run_test("(Rational(1, 2) - Rational(1, 3)).to_s");
        run_test("(Rational(2, 3) * Rational(3, 4)).to_s");
        run_test("(Rational(2, 3) / Rational(3, 4)).to_s");
    }

    #[test]
    fn rational_comparison() {
        run_test("Rational(1, 2) == Rational(2, 4)");
        run_test("Rational(1, 2) == 0.5");
        run_test("(Rational(1, 2) <=> Rational(1, 3))");
    }

    #[test]
    fn rational_conversion() {
        run_test("Rational(3, 2).to_f");
        run_test("Rational(7, 2).to_i");
        run_test("Rational(3, 4).to_s");
        run_test("Rational(3, 4).inspect");
    }

    #[test]
    fn rational_predicates() {
        run_test("Rational(0, 1).zero?");
        run_test("Rational(1, 2).positive?");
        run_test("Rational(-1, 2).negative?");
        run_test("Rational(1, 2).integer?");
    }

    #[test]
    fn rational_unary() {
        run_test("(-Rational(3, 4)).to_s");
        run_test("Rational(-3, 4).abs.to_s");
    }

    #[test]
    fn rational_power() {
        run_test("(Rational(2, 3) ** 2).to_s");
        run_test("(Rational(2, 3) ** -1).to_s");
    }

    #[test]
    fn rational_power_extended() {
        // Rational ** Rational (integer denominator)
        run_test("(Rational(2, 3) ** Rational(2, 1)).to_s");
        run_test("(Rational(2, 3) ** Rational(-1, 1)).to_s");
        // Rational ** Float
        run_test("(Rational(4, 1) ** 0.5).class");
        // Zero base with negative exponent
        run_test_error("Rational(0, 1) ** -1");
        run_test_error("Rational(0, 1) ** Rational(-1, 1)");
        // Type error for unsupported type
        run_test_error("Rational(1, 2) ** :sym");
    }

    // Fix 3: Negative Rational ** fractional exponent returns Complex
    #[test]
    fn rational_pow_negative_base_complex() {
        // Rational(-8,1) ** Rational(1,3) should return Complex
        run_test("(Rational(-8, 1) ** Rational(1, 3)).class");
        // Rational(-8,1) ** 0.333... should return Complex
        run_test("(Rational(-8, 1) ** (1.0/3)).class");
        // Integer ** Rational also goes through coerce to Rational **
        run_test("((-8) ** Rational(1, 3)).class");
    }

    #[test]
    fn rational_allocate_disabled() {
        run_test_error("Rational.new(1)");
        run_test_error("Rational.allocate");
    }

    #[test]
    fn rational_floor_ceil_round_ndigits() {
        run_tests(&[
            "Rational(7, 3).floor.to_s",
            "Rational(7, 3).ceil.to_s",
            "Rational(7, 3).truncate.to_s",
            "Rational(7, 3).round.to_s",
            // ndigits > 0 returns Rational
            "Rational(7, 3).floor(1).class",
            "Rational(7, 3).ceil(1).class",
            "Rational(7, 3).round(1).class",
        ]);
    }

    #[test]
    fn rational_round_large_ndigits() {
        run_tests(&[
            // Absurdly large ndigits should not hang
            "Rational(3, 2).round(2_097_171)",
            "Rational(3, 2).floor(2_097_171)",
            "Rational(3, 2).ceil(2_097_171)",
            "Rational(3, 2).truncate(2_097_171)",
            // Denominator=1 (integer rational)
            "Rational(6, 1).round(1000000)",
        ]);
    }

    #[test]
    fn rational_cmp_coerce() {
        run_test("Rational(1, 2) <=> 0.5");
        run_test("Rational(1, 2) <=> 1");
        run_test("Rational(1, 2) <=> 'a'");
    }

    #[test]
    fn rational_div_float_zero() {
        run_test("(Rational(1, 2) / 0.0).to_s");
        run_test("(Rational(-1, 2) / 0.0).to_s");
    }

    #[test]
    fn rational_ne() {
        run_tests(&[
            "Rational(1, 2) != Rational(1, 2)",
            "Rational(1, 2) != Rational(1, 3)",
            "Rational(1, 2) != 0.5",
            "Rational(1, 2) != 0.3",
            "Rational(2, 1) != 2",
            "Rational(2, 1) != 3",
            "Rational(1, 2) != :foo",
        ]);
    }

    #[test]
    fn rational_eq_extended() {
        run_tests(&[
            // BigInt comparison
            "Rational(10**20, 1) == 10**20",
            "Rational(10**20, 1) == 10**19",
            // Coerce path (unknown type)
            "Rational(1, 2) == :foo",
        ]);
    }

    #[test]
    fn rational_cmp_extended() {
        run_tests(&[
            // BigInt comparison
            "(Rational(10**20, 1) <=> 10**20)",
            "(Rational(1, 2) <=> 10**20)",
            // nil return for non-comparable
            "(Rational(1, 2) <=> :foo)",
        ]);
    }

    #[test]
    fn rational_floor() {
        run_tests(&[
            // ndigits == 0 (returns Integer)
            "Rational(7, 3).floor",
            "Rational(-7, 3).floor",
            "Rational(6, 3).floor",
            // ndigits > 0 (returns Rational)
            "Rational(7, 3).floor(1).to_s",
            "Rational(-7, 3).floor(1).to_s",
            "Rational(1, 4).floor(1).to_s",
            // ndigits < 0 (rounds toward negative infinity at 10s place)
            "Rational(123, 1).floor(-1)",
            "Rational(-123, 1).floor(-1)",
            // ndigits_sufficient: 1/4 is exact at 2 decimal places
            "Rational(1, 4).floor(2).to_s",
        ]);
    }

    #[test]
    fn rational_ceil() {
        run_tests(&[
            "Rational(7, 3).ceil",
            "Rational(-7, 3).ceil",
            "Rational(6, 3).ceil",
            "Rational(7, 3).ceil(1).to_s",
            "Rational(-7, 3).ceil(1).to_s",
            "Rational(123, 1).ceil(-1)",
            "Rational(-123, 1).ceil(-1)",
        ]);
    }

    #[test]
    fn rational_truncate() {
        run_tests(&[
            "Rational(7, 3).truncate",
            "Rational(-7, 3).truncate",
            "Rational(6, 3).truncate",
            "Rational(7, 3).truncate(1).to_s",
            "Rational(-7, 3).truncate(1).to_s",
            "Rational(123, 1).truncate(-1)",
        ]);
    }

    #[test]
    fn rational_round() {
        run_tests(&[
            // Basic rounding
            "Rational(7, 3).round",
            "Rational(-7, 3).round",
            "Rational(5, 2).round",
            "Rational(-5, 2).round",
            // half: modes
            "Rational(5, 2).round(0, half: :up)",
            "Rational(5, 2).round(0, half: :down)",
            "Rational(5, 2).round(0, half: :even)",
            "Rational(7, 2).round(0, half: :even)",
            // ndigits > 0
            "Rational(7, 3).round(1).to_s",
            "Rational(7, 3).round(2).to_s",
            // ndigits < 0
            "Rational(155, 1).round(-1)",
            "Rational(145, 1).round(-1)",
        ]);
        // invalid half mode
        run_test_error("Rational(1, 2).round(0, half: :foo)");
    }

    #[test]
    fn rational_arithmetic_with_int_and_float() {
        run_tests(&[
            // Rational + Integer
            "(Rational(1, 2) + 1).to_s",
            // Rational + Float
            "(Rational(1, 2) + 0.5).class",
            // Rational - Integer
            "(Rational(1, 2) - 1).to_s",
            // Rational - Float
            "(Rational(1, 2) - 0.5).class",
            // Rational * Integer
            "(Rational(1, 2) * 3).to_s",
            // Rational * Float
            "(Rational(1, 2) * 0.5).class",
            // Rational / Integer
            "(Rational(1, 2) / 3).to_s",
            // Rational / Float
            "(Rational(1, 2) / 0.5).class",
        ]);
    }

    #[test]
    fn rational_new_raises_no_method_error() {
        // Rational.new is undefined; calling it must raise NoMethodError.
        run_test(
            r#"
            begin
              Rational.new(1)
            rescue NoMethodError => e
              :nm
            rescue => e
              e.class
            end
            "#,
        );
    }

    #[test]
    fn rational_div_divmod_mod_float_zero() {
        // Rational#div(0.0) must raise ZeroDivisionError (not FloatDomainError).
        run_tests(&[
            r#"begin; Rational(3, 4).div(0.0); rescue ZeroDivisionError; :zd; end"#,
            r#"begin; Rational(3, 4).divmod(0.0); rescue ZeroDivisionError; :zd; end"#,
            r#"begin; Rational(3, 4) % 0.0; rescue ZeroDivisionError; :zd; end"#,
            // Integer 0 also raises ZeroDivisionError.
            r#"begin; Rational(3, 4).div(0); rescue ZeroDivisionError; :zd; end"#,
            // Non-numeric argument must raise TypeError (not NoMethodError).
            r#"begin; Rational(3, 4).div([]); rescue TypeError; :te; end"#,
        ]);
    }

    #[test]
    fn rational_pow_bigint_and_special_bases() {
        // Rational(1) ** BigInt stays Rational(1).
        run_test("(Rational(1) ** (10 ** 20)).to_s");
        // Rational(-1) ** even/odd BigInt.
        run_test("(Rational(-1) ** (10 ** 20)).to_s");
        run_test("(Rational(-1) ** ((10 ** 20) + 1)).to_s");
        // Rational(0) ** negative Float returns Infinity (no error).
        run_test("(Rational(0, 1) ** -1.0).infinite?");
        // Rational(2) ** BigInt raises ArgumentError ("exponent is too large").
        run_test(
            r#"begin; Rational(2) ** (10 ** 20); rescue ArgumentError; :ae; end"#,
        );
    }

    #[test]
    fn rational_to_f_large_numerator_denominator() {
        // Both numerator and denominator exceed f64 range; ratio is finite.
        // Matches CRuby's Rational#to_f for ~10^308 magnitude inputs.
        run_test(
            r#"
            num = 10 ** 310
            den = 2 * 10 ** 310
            Rational(num, den).to_f
            "#,
        );
        // Extremely large magnitudes with exact 500:1 ratio.
        run_test(
            r#"
            num = 5 * 10 ** 600
            den = 10 ** 598
            Rational(num, den).to_f
            "#,
        );
    }

    #[test]
    fn rational_rationalize_negative() {
        // Stern-Brocot on negative self must negate and restore sign.
        run_tests(&[
            "Rational(-5404319552844595, 18014398509481984).rationalize(Rational(1, 10)).to_s",
            "Rational(-5404319552844595, 18014398509481984).rationalize(0.05).to_s",
            "Rational(-5404319552844595, 18014398509481984).rationalize(0.001).to_s",
            // Positive case still works.
            "Rational(5404319552844595, 18014398509481984).rationalize(Rational(1, 10)).to_s",
            // No argument returns self.
            "Rational(3, 4).rationalize.to_s",
        ]);
    }

    #[test]
    fn rational_truncate_strict_precision() {
        // truncate rejects non-Integer precision without calling to_int.
        run_tests(&[
            r#"begin; Rational(7, 3).truncate(nil); rescue TypeError => e; e.message; end"#,
            r#"begin; Rational(7, 3).truncate(1.0); rescue TypeError => e; e.message; end"#,
            r#"begin; Rational(7, 3).truncate(""); rescue TypeError => e; e.message; end"#,
            // Integer precision still works.
            "Rational(7, 3).truncate(2).to_s",
        ]);
    }

    #[test]
    fn rational_marshal_dump_private() {
        // marshal_dump is a private instance method returning [num, den].
        run_tests(&[
            "Rational.private_instance_methods(false).include?(:marshal_dump)",
            "Rational(3, 5).send(:marshal_dump)",
        ]);
    }
}
