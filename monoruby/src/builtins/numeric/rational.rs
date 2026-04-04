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
    globals.define_builtin_func(RATIONAL_CLASS, "to_r", to_r, 0);
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
    globals.define_builtin_func(RATIONAL_CLASS, "+@", pos, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "abs", abs, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "zero?", zero_, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "positive?", positive_, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "negative?", negative_, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "integer?", integer_, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "hash", hash, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "eql?", eql_, 1);
    globals.define_builtin_func(RATIONAL_CLASS, "freeze", freeze, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "frozen?", frozen_, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "finite?", finite_, 0);
    globals.define_builtin_func(RATIONAL_CLASS, "infinite?", infinite_, 0);
    globals.define_builtin_class_func(RATIONAL_CLASS, "__allocate", allocate, 2);
    globals.define_builtin_class_func(RATIONAL_CLASS, "allocate", super::super::class::undef_allocate, 0);
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
fn to_r(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp.self_val())
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
fn pow(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = self_rat(lfp);
    let rhs = lfp.arg(0);
    match rhs.unpack() {
        RV::Fixnum(exp) => {
            if exp >= 0 {
                let exp = exp as u32;
                let n = lhs.num().pow(exp);
                let d = lhs.den().pow(exp);
                Ok(Value::rational_from_bigint(n, d))
            } else {
                if lhs.is_zero() {
                    return Err(MonorubyErr::divide_by_zero());
                }
                let exp = (-exp) as u32;
                let n = lhs.den().pow(exp);
                let d = lhs.num().pow(exp);
                Ok(Value::rational_from_bigint(n, d))
            }
        }
        RV::Float(f) => {
            if lhs.is_zero() && f < 0.0 {
                return Err(MonorubyErr::divide_by_zero());
            }
            Ok(Value::float(lhs.to_f().powf(f)))
        }
        _ => {
            if let Some(r) = rhs.try_rational() {
                if lhs.is_zero() && r.is_negative() {
                    return Err(MonorubyErr::divide_by_zero());
                }
                if *r.den() == BigInt::from(1) {
                    use num::ToPrimitive;
                    if let Some(exp) = r.num().to_i64() {
                        if exp >= 0 {
                            let exp = exp as u32;
                            let n = lhs.num().pow(exp);
                            let d = lhs.den().pow(exp);
                            return Ok(Value::rational_from_bigint(n, d));
                        } else {
                            if lhs.is_zero() {
                                return Err(MonorubyErr::divide_by_zero());
                            }
                            let exp = (-exp) as u32;
                            let n = lhs.den().pow(exp);
                            let d = lhs.num().pow(exp);
                            return Ok(Value::rational_from_bigint(n, d));
                        }
                    }
                }
                return Ok(Value::float(lhs.to_f().powf(r.to_f())));
            }
            Err(MonorubyErr::typeerr(format!(
                "{} can't be coerced into Rational",
                rhs.get_real_class_name(&globals.store),
            )))
        }
    }
}

#[monoruby_builtin]
fn neg(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::rational_from_inner(self_rat(lfp).neg()))
}

#[monoruby_builtin]
fn pos(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp.self_val())
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
fn integer_(_: &mut Executor, _: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(false))
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

#[monoruby_builtin]
fn freeze(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp.self_val())
}

#[monoruby_builtin]
fn frozen_(_: &mut Executor, _: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(true))
}

#[monoruby_builtin]
fn finite_(_: &mut Executor, _: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(true))
}

#[monoruby_builtin]
fn infinite_(_: &mut Executor, _: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::nil())
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn rational_basic() {
        run_test_once("Rational(3, 4).numerator");
        run_test_once("Rational(3, 4).denominator");
        run_test_once("Rational(6, 4).numerator");
    }

    #[test]
    fn rational_arithmetic() {
        run_test_once("(Rational(1, 2) + Rational(1, 3)).to_s");
        run_test_once("(Rational(1, 2) - Rational(1, 3)).to_s");
        run_test_once("(Rational(2, 3) * Rational(3, 4)).to_s");
        run_test_once("(Rational(2, 3) / Rational(3, 4)).to_s");
    }

    #[test]
    fn rational_comparison() {
        run_test_once("Rational(1, 2) == Rational(2, 4)");
        run_test_once("Rational(1, 2) == 0.5");
        run_test_once("(Rational(1, 2) <=> Rational(1, 3))");
    }

    #[test]
    fn rational_conversion() {
        run_test_once("Rational(3, 2).to_f");
        run_test_once("Rational(7, 2).to_i");
        run_test_once("Rational(3, 4).to_s");
        run_test_once("Rational(3, 4).inspect");
    }

    #[test]
    fn rational_predicates() {
        run_test_once("Rational(0, 1).zero?");
        run_test_once("Rational(1, 2).positive?");
        run_test_once("Rational(-1, 2).negative?");
        run_test_once("Rational(1, 2).integer?");
    }

    #[test]
    fn rational_unary() {
        run_test_once("(-Rational(3, 4)).to_s");
        run_test_once("Rational(-3, 4).abs.to_s");
    }

    #[test]
    fn rational_power() {
        run_test_once("(Rational(2, 3) ** 2).to_s");
        run_test_once("(Rational(2, 3) ** -1).to_s");
    }

    #[test]
    fn rational_power_extended() {
        // Rational ** Rational (integer denominator)
        run_test_once("(Rational(2, 3) ** Rational(2, 1)).to_s");
        run_test_once("(Rational(2, 3) ** Rational(-1, 1)).to_s");
        // Rational ** Float
        run_test_once("(Rational(4, 1) ** 0.5).class");
        // Zero base with negative exponent
        run_test_error("Rational(0, 1) ** -1");
        run_test_error("Rational(0, 1) ** Rational(-1, 1)");
        // Type error for unsupported type
        run_test_error("Rational(1, 2) ** :sym");
    }

    #[test]
    fn rational_allocate_disabled() {
        run_test_error("Rational.new(1)");
        run_test_error("Rational.allocate");
    }

    #[test]
    fn rational_floor_ceil_round_ndigits() {
        run_test_once("Rational(7, 3).floor.to_s");
        run_test_once("Rational(7, 3).ceil.to_s");
        run_test_once("Rational(7, 3).truncate.to_s");
        run_test_once("Rational(7, 3).round.to_s");
        // ndigits > 0 returns Rational
        run_test_once("Rational(7, 3).floor(1).class");
        run_test_once("Rational(7, 3).ceil(1).class");
        run_test_once("Rational(7, 3).round(1).class");
    }

    #[test]
    fn rational_cmp_coerce() {
        run_test_once("Rational(1, 2) <=> 0.5");
        run_test_once("Rational(1, 2) <=> 1");
        run_test_once("Rational(1, 2) <=> 'a'");
    }

    #[test]
    fn rational_div_float_zero() {
        run_test_once("(Rational(1, 2) / 0.0).to_s");
        run_test_once("(Rational(-1, 2) / 0.0).to_s");
    }
}
