use num::ToPrimitive;

use super::*;

//
// Float class
//

pub(super) fn init(globals: &mut Globals, numeric: Module) {
    globals.define_builtin_class("Float", FLOAT_CLASS, numeric, OBJECT_CLASS, None);
    globals.set_constant_by_str(FLOAT_CLASS, "NAN", Value::float(f64::NAN));
    globals.set_constant_by_str(FLOAT_CLASS, "INFINITY", Value::float(f64::INFINITY));
    globals.set_constant_by_str(FLOAT_CLASS, "MAX", Value::float(f64::MAX));
    globals.set_constant_by_str(FLOAT_CLASS, "MIN", Value::float(f64::MIN));
    globals.set_constant_by_str(FLOAT_CLASS, "MAX_10_EXP", Value::i32(f64::MAX_10_EXP));
    globals.set_constant_by_str(FLOAT_CLASS, "MAX_EXP", Value::i32(f64::MAX_EXP));
    globals.set_constant_by_str(FLOAT_CLASS, "EPSILON", Value::float(f64::EPSILON));
    globals.define_builtin_funcs(FLOAT_CLASS, "to_i", &["to_int"], toi, 0);
    globals.define_builtin_func(FLOAT_CLASS, "to_f", tof, 0);

    globals.define_basic_op(FLOAT_CLASS, "+", add, 1);
    globals.define_basic_op(FLOAT_CLASS, "-", sub, 1);
    globals.define_basic_op(FLOAT_CLASS, "*", mul, 1);
    globals.define_builtin_cfunc_ff_f(FLOAT_CLASS, "/", div, div_ff_f, 1);
    globals.define_builtin_cfunc_ff_f(FLOAT_CLASS, "%", rem, rem_ff_f, 1);
    globals.define_builtin_cfunc_ff_f(FLOAT_CLASS, "**", pow, pow_ff_f, 1);
    globals.define_builtin_func(FLOAT_CLASS, "div", div_floor, 1);
    globals.define_builtin_func(FLOAT_CLASS, "modulo", rem, 1);
    globals.define_builtin_func(FLOAT_CLASS, "divmod", divmod, 1);
    globals.define_builtin_func(FLOAT_CLASS, "==", eq, 1);
    globals.define_builtin_func(FLOAT_CLASS, "===", eq, 1);
    globals.define_builtin_func(FLOAT_CLASS, ">=", ge, 1);
    globals.define_builtin_func(FLOAT_CLASS, ">", gt, 1);
    globals.define_builtin_func(FLOAT_CLASS, "<=", le, 1);
    globals.define_builtin_func(FLOAT_CLASS, "<", lt, 1);
    globals.define_builtin_func(FLOAT_CLASS, "!=", ne, 1);
    globals.define_builtin_func(FLOAT_CLASS, "<=>", cmp, 1);
    globals.define_builtin_func_with(FLOAT_CLASS, "floor", floor, 0, 1, false);
    globals.define_builtin_func_with(FLOAT_CLASS, "ceil", ceil, 0, 1, false);
    globals.define_builtin_func_with(FLOAT_CLASS, "truncate", truncate, 0, 1, false);
    globals.define_builtin_func_with(FLOAT_CLASS, "round", round, 0, 2, false);
    globals.define_builtin_func(FLOAT_CLASS, "finite?", finite, 0);
    globals.define_builtin_func(FLOAT_CLASS, "infinite?", infinite, 0);
    globals.define_builtin_func(FLOAT_CLASS, "nan?", nan, 0);
    globals.define_builtin_funcs(FLOAT_CLASS, "abs", &["magnitude"], abs, 0);
    globals.define_builtin_funcs(FLOAT_CLASS, "angle", &["arg", "phase"], angle, 0);
    globals.define_builtin_func(FLOAT_CLASS, "next_float", next_float, 0);
    globals.define_builtin_func(FLOAT_CLASS, "prev_float", prev_float, 0);
}

extern "C" fn pow_ff_f(lhs: f64, rhs: f64) -> f64 {
    lhs.powf(rhs)
}

extern "C" fn div_ff_f(lhs: f64, rhs: f64) -> f64 {
    lhs.ruby_div(&rhs)
}

extern "C" fn rem_ff_f(lhs: f64, rhs: f64) -> f64 {
    lhs.ruby_mod(&rhs)
}

///
/// ### Float#to_f
///
/// - to_f -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/to_f.html]
#[monoruby_builtin]
fn tof(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp.self_val())
}

///
/// ### Float#to_i
///
/// - to_i -> Integer
/// - [NOT SUPPORTED]truncate(ndigits = 0) -> Integer | Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/to_i.html]
#[monoruby_builtin]
fn toi(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    match lfp.self_val().unpack() {
        RV::Float(f) => Ok(Value::integer(f.trunc() as i64)),
        _ => unreachable!(),
    }
}

///
/// ### Float#div
///
/// - div(other) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Numeric/i/div.html]
#[monoruby_builtin]
fn div_floor(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val().try_float().unwrap();
    let rhs = RealKind::expect(globals, lfp.arg(0))?.to_f64();
    let div_floor = (lhs / rhs).floor();
    Value::coerce_f64_to_int(div_floor)
}

///
/// ### Integer#==
///
/// - self == other -> bool
/// - self === other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3d=3d.html]
#[monoruby_builtin]
fn eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let b = vm.eq_values_bool(globals, lfp.self_val(), lfp.arg(0))?;
    Ok(Value::bool(b))
}

///
/// ### Integer#!=
///
/// - self != other -> bool
///
#[monoruby_builtin]
fn ne(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let b = vm.ne_values_bool(globals, lfp.self_val(), lfp.arg(0))?;
    Ok(Value::bool(b))
}

///
/// ### Float#<=>
///
/// - self <=> other -> -1 | 0 | 1 | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/=3c=3d=3e.html]
#[monoruby_builtin]
fn cmp(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    let ord = match (lhs.try_float(), rhs.unpack()) {
        (None, _) => unreachable!(),
        (Some(lhs), RV::Fixnum(rhs)) => lhs.partial_cmp(&(rhs as f64)),
        (Some(lhs), RV::BigInt(rhs)) => lhs.partial_cmp(&rhs.to_f64().unwrap()),
        (Some(lhs), RV::Float(rhs)) => lhs.partial_cmp(&rhs),
        _ => {
            // Try coerce protocol for non-numeric types
            let coerce_id = IdentId::get_id("coerce");
            if let Some(result) =
                vm.invoke_method_if_exists(globals, coerce_id, rhs, &[lhs], None, None)?
            {
                if let Some(ary) = result.try_array_ty() {
                    if ary.len() == 2 {
                        let cmp_id = IdentId::get_id("<=>");
                        let res = vm.invoke_method_inner(
                            globals, cmp_id, ary[0], &[ary[1]], None, None,
                        )?;
                        return Ok(res);
                    }
                }
            }
            None
        }
    };
    Ok(match ord {
        Some(ord) => Value::from_ord(ord),
        None => Value::nil(),
    })
}

///
/// ### Integer#>=
///
/// - self >= other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3e=3d.html]
#[monoruby_builtin]
fn ge(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    crate::executor::op::cmp_ge_values(vm, globals, lfp.self_val(), lfp.arg(0))
        .ok_or_else(|| vm.take_error())
}

///
/// ### Integer#>
///
/// - self > other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3e.html]
#[monoruby_builtin]
fn gt(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    crate::executor::op::cmp_gt_values(vm, globals, lfp.self_val(), lfp.arg(0))
        .ok_or_else(|| vm.take_error())
}

///
/// ### Integer#<=
///
/// - self <= other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3c=3d.html]
#[monoruby_builtin]
fn le(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    crate::executor::op::cmp_le_values(vm, globals, lfp.self_val(), lfp.arg(0))
        .ok_or_else(|| vm.take_error())
}

///
/// ### Integer#<
///
/// - self < other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3c.html]
#[monoruby_builtin]
fn lt(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    crate::executor::op::cmp_lt_values(vm, globals, lfp.self_val(), lfp.arg(0))
        .ok_or_else(|| vm.take_error())
}

///
/// ### Float#floor
///
/// - floor([NOT SUPPORTED]ndigits = 0) -> Integer | Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/floor.html]
#[monoruby_builtin]
fn floor(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ndigits = if let Some(d) = lfp.try_arg(0) {
        d.coerce_to_int_i64(vm, globals)?
    } else {
        0
    };
    let f = lfp.self_val().try_float().unwrap();
    if ndigits == 0 {
        return Ok(Value::integer(f.floor() as i64));
    }
    if ndigits > 0 {
        if let Ok(ndigits) = u32::try_from(ndigits) {
            let mul = 10f64.powi(ndigits as i32);
            let f = (f * mul).floor() / mul;
            Ok(Value::float(f))
        } else {
            Err(MonorubyErr::rangeerr("too big to convert to u32"))
        }
    } else {
        if let Ok(neg_ndigits) = u32::try_from(-ndigits) {
            let mul = 10f64.powi(neg_ndigits as i32);
            let f = (f / mul).floor() * mul;
            if let Some(v) = Value::integer_from_f64(f) {
                Ok(v)
            } else {
                Err(MonorubyErr::rangeerr(format!(
                    "[unreachable] invalid f64: {f}"
                )))
            }
        } else {
            Err(MonorubyErr::rangeerr("too small to convert to u32"))
        }
    }
}

///
/// ### Float#ceil
///
/// - ceil(ndigits = 0) -> Integer | Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/ceil.html]
#[monoruby_builtin]
fn ceil(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ndigits = if let Some(d) = lfp.try_arg(0) {
        d.coerce_to_int_i64(vm, globals)?
    } else {
        0
    };
    let f = lfp.self_val().try_float().unwrap();
    if ndigits == 0 {
        return Ok(Value::integer(f.ceil() as i64));
    }
    if ndigits > 0 {
        if let Ok(ndigits) = u32::try_from(ndigits) {
            let mul = 10f64.powi(ndigits as i32);
            let f = (f * mul).ceil() / mul;
            Ok(Value::float(f))
        } else {
            Err(MonorubyErr::rangeerr("too big to convert to u32"))
        }
    } else {
        if let Ok(neg_ndigits) = u32::try_from(-ndigits) {
            let mul = 10f64.powi(neg_ndigits as i32);
            let f = (f / mul).ceil() * mul;
            if let Some(v) = Value::integer_from_f64(f) {
                return Ok(v);
            } else {
                return Err(MonorubyErr::rangeerr(format!(
                    "[unreachable] invalid f64: {f}"
                )));
            }
        }
        Err(MonorubyErr::rangeerr("too small to convert to u32"))
    }
}

///
/// ### Float#truncate
///
/// - truncate(ndigits = 0) -> Integer | Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/truncate.html]
#[monoruby_builtin]
fn truncate(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ndigits = if let Some(d) = lfp.try_arg(0) {
        d.coerce_to_int_i64(vm, globals)?
    } else {
        0
    };
    let f = lfp.self_val().try_float().unwrap();
    if ndigits == 0 {
        return Ok(Value::integer(f.trunc() as i64));
    }
    if ndigits > 0 {
        if let Ok(ndigits) = u32::try_from(ndigits) {
            let mul = 10f64.powi(ndigits as i32);
            let f = (f * mul).trunc() / mul;
            Ok(Value::float(f))
        } else {
            Err(MonorubyErr::rangeerr("too big to convert to u32"))
        }
    } else {
        if let Ok(neg_ndigits) = u32::try_from(-ndigits) {
            let mul = 10f64.powi(neg_ndigits as i32);
            let f = (f / mul).trunc() * mul;
            if let Some(v) = Value::integer_from_f64(f) {
                return Ok(v);
            } else {
                return Err(MonorubyErr::rangeerr(format!(
                    "[unreachable] invalid f64: {f}"
                )));
            }
        }
        Err(MonorubyErr::rangeerr("too small to convert to u32"))
    }
}

///
/// ### Float#round
///
/// - round(ndigits = 0) -> Integer | Float
/// - round(ndigits = 0, [NOT SUPPORTED]half: :up) -> Integer | Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/round.html]
#[monoruby_builtin]
fn round(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ndigits = if let Some(d) = lfp.try_arg(0) {
        d.coerce_to_int_i64(vm, globals)?
    } else {
        0
    };
    let f = lfp.self_val().try_float().unwrap();
    if ndigits == 0 {
        return Ok(Value::integer(f.round() as i64));
    }
    if ndigits > 0 {
        if let Ok(ndigits) = u32::try_from(ndigits) {
            let mul = 10f64.powi(ndigits as i32);
            let f = (f * mul).round() / mul;
            Ok(Value::float(f))
        } else {
            Err(MonorubyErr::rangeerr("too big to convert to u32"))
        }
    } else {
        if let Ok(neg_ndigits) = u32::try_from(-ndigits) {
            let mul = 10f64.powi(neg_ndigits as i32);
            let f = (f / mul).round() * mul;
            if let Some(v) = Value::integer_from_f64(f) {
                return Ok(v);
            } else {
                return Err(MonorubyErr::rangeerr(format!(
                    "[unreachable] invalid f64: {f}"
                )));
            }
        }
        Err(MonorubyErr::rangeerr("too small to convert to u32"))
    }
}

///
/// ### Float#finite?
///
/// - finite? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/finite=3f.html]
#[monoruby_builtin]
fn finite(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().try_float().unwrap().is_finite()))
}

///
/// ### Float#finite?
///
/// - infinite? -> 1 | -1 | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/infinite=3f.html]
#[monoruby_builtin]
fn infinite(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = lfp.self_val().try_float().unwrap();
    if f.is_infinite() {
        Ok(Value::integer(num::signum(f) as i64))
    } else {
        Ok(Value::nil())
    }
}

///
/// ### Float#nan?
///
/// - nan? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/nan=3f.html]
#[monoruby_builtin]
fn nan(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().try_float().unwrap().is_nan()))
}

///
/// ### Float#abs
///
/// - abs -> Float
/// - magnitude -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/abs.html]
#[monoruby_builtin]
fn abs(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = lfp.self_val().try_float().unwrap();
    Ok(Value::float(f.abs()))
}

///
/// ### Float#angle
///
/// - angle -> 0 | Float
/// - arg -> 0 | Float
/// - phase -> 0 | Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/angle.html]
#[monoruby_builtin]
fn angle(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = lfp.self_val().try_float().unwrap();
    if f.is_nan() {
        Ok(Value::float(f64::NAN))
    } else if f.is_sign_negative() {
        Ok(Value::float(std::f64::consts::PI))
    } else {
        Ok(Value::integer(0))
    }
}

/// ### Float#next_float
#[monoruby_builtin]
fn next_float(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = lfp.self_val().try_float().unwrap();
    if f.is_nan() || (f.is_infinite() && f.is_sign_positive()) {
        return Ok(Value::float(f));
    }
    let bits = f.to_bits();
    let next_bits = if f == 0.0 { 1u64 } else if f > 0.0 { bits + 1 } else { bits - 1 };
    Ok(Value::float(f64::from_bits(next_bits)))
}

/// ### Float#prev_float
#[monoruby_builtin]
fn prev_float(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = lfp.self_val().try_float().unwrap();
    if f.is_nan() || (f.is_infinite() && f.is_sign_negative()) {
        return Ok(Value::float(f));
    }
    let bits = f.to_bits();
    let prev_bits = if f == 0.0 { (1u64 << 63) | 1u64 } else if f > 0.0 { bits - 1 } else { bits + 1 };
    Ok(Value::float(f64::from_bits(prev_bits)))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn angle() {
        run_test("1.0.angle");
        run_test("(-1.0).angle");
        run_test("0.0.angle");
        run_test("(1.0/0).angle");
        run_test("(-1.0/0).angle");
        run_test_once("Float::NAN.angle.nan?");
        run_test("1.0.arg");
        run_test("(-1.0).arg");
        run_test("1.0.phase");
        run_test("(-1.0).phase");
        run_test("1.angle");
        run_test("(-1).angle");
        run_test("0.angle");
    }

    #[test]
    fn float() {
        run_test("4.87.to_i");
        run_test("-2.18.to_i");
        run_test("4.7777.to_f");
        run_test("-725.11.to_f");
        run_test("3.0.div(2)");
        run_test("3.0.div(-2)");
        run_test("(-3.0).div(2)");
        run_test("(-3.0).div(-2)");
        run_test("(-37.044).abs");
        run_test("37.044.magnitude");
    }

    #[test]
    fn round() {
        run_test("1.2.floor");
        run_test("(-1.2).floor");
        run_test("1.2.floor(1)");
        run_test("(-1.2).floor(1)");
        run_test("1.567.floor(2)");
        run_test("1234.567.floor(-2)");
        run_test("11.6.round");
        run_test("11.5.round");
        run_test("11.4.round");
        run_test("(-11.4).round");
        run_test("(-11.5).round");
        run_test("(-11.6).round");
        run_test("(1000 * Math::PI).round(3)");
        run_test("(1000 * Math::PI).round(0)");
        run_test("(1000 * Math::PI).round(-3)");
    }

    #[test]
    fn ceil_truncate_round_large_ndigits() {
        // ndigits > 9 would overflow 10i32.pow() before the fix
        run_test("1.123456789.ceil(15)");
        run_test("1.123456789.floor(15)");
        run_test("1.123456789.truncate(15)");
        run_test("1.123456789.round(15)");
        // Large negative ndigits
        run_test("123456789.0.ceil(-15)");
        run_test("123456789.0.floor(-15)");
        run_test("123456789.0.truncate(-15)");
        run_test("123456789.0.round(-15)");
    }

    #[test]
    fn finite() {
        run_test("(1/0.0).finite?");
        run_test("(-1/0.0).finite?");
        run_test("(Float::NAN).finite?");
        run_test("(5.5).finite?");
        run_test("(1/0.0).infinite?");
        run_test("(-1/0.0).infinite?");
        run_test("(Float::NAN).infinite?");
        run_test("(5.5).infinite?");
        run_test("(1/0.0).nan?");
        run_test("(-1/0.0).nan?");
        run_test("(Float::NAN).nan?");
        run_test("(5.5).nan?");
    }

    #[test]
    fn float_division_by_zero() {
        run_test("(0.0/0).nan?");
        run_test("(0.0/0.0).nan?");
        run_test("1.0/0 == Float::INFINITY");
        run_test("-1.0/0 == -Float::INFINITY");
        run_test("1.0/0.0 == Float::INFINITY");
    }

    #[test]
    fn next_float() {
        run_test("1.0.next_float");
        run_test("0.0.next_float");
        run_test("(-1.0).next_float");
    }

    #[test]
    fn prev_float() {
        run_test("1.0.prev_float");
        run_test("0.0.prev_float");
        run_test("(-1.0).prev_float");
    }

    #[test]
    fn zero_positive_negative() {
        run_test("0.0.zero?");
        run_test("1.0.zero?");
        run_test("(-1.0).zero?");
        run_test("1.0.positive?");
        run_test("(-1.0).positive?");
        run_test("0.0.positive?");
        run_test("1.0.negative?");
        run_test("(-1.0).negative?");
        run_test("0.0.negative?");
    }

    #[test]
    fn float_to_r() {
        run_test_once("1.5.respond_to?(:to_r)");
        run_test_once("1.5.respond_to?(:rationalize)");
    }

    #[test]
    fn float_inspect_to_s() {
        // Special values
        run_test("(1.0/0.0).inspect");
        run_test("(-1.0/0.0).inspect");
        run_test("(0.0/0.0).inspect");
        run_test("(1.0/0.0).to_s");
        run_test("(-1.0/0.0).to_s");
        run_test("(0.0/0.0).to_s");
        // Negative zero
        run_test("(-0.0).inspect");
        run_test("(-0.0).to_s");
        run_test("0.0.inspect");
        // Simple values
        run_test("1.0.inspect");
        run_test("100.0.inspect");
        run_test("(-1.0).inspect");
        // Fixed notation boundaries
        run_test("0.0001.inspect");
        run_test("0.001.inspect");
        run_test("100000000000000.0.inspect");
        // Scientific notation
        run_test("0.00001.inspect");
        run_test("(-0.00001).inspect");
        run_test("1e15.inspect");
        run_test("(-1e15).inspect");
        run_test("1e16.inspect");
        run_test("1e20.inspect");
        // Shortest round-trip representation
        run_test("(1.0/7).inspect");
        run_test("0.21611564636388508.inspect");
        // String interpolation uses to_s
        run_test("\"#{1.0/0.0}\"");
        run_test("\"#{-0.0}\"");
    }

    #[test]
    fn float_cmp_coerce() {
        // Object with coerce method (use run_test_once to avoid JIT deopt issues)
        run_test_once(
            r#"
            class MyNum
              def initialize(n); @n = n; end
              def coerce(other); [other, @n.to_f]; end
            end
            obj = MyNum.new(2)
            1.5 <=> obj
            "#,
        );
        // Object without coerce -> nil
        run_test("1.5 <=> 'a'");
    }
}
