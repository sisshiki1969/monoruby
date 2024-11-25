use num::ToPrimitive;

use super::*;

//
// Float class
//

pub(super) fn init(globals: &mut Globals, numeric: Module) {
    globals.define_builtin_class_by_str("Float", FLOAT_CLASS, numeric, OBJECT_CLASS);
    globals.set_constant_by_str(FLOAT_CLASS, "NAN", Value::float(f64::NAN));
    globals.set_constant_by_str(FLOAT_CLASS, "INFINITY", Value::float(f64::INFINITY));
    globals.set_constant_by_str(FLOAT_CLASS, "MAX", Value::float(f64::MAX));
    globals.set_constant_by_str(FLOAT_CLASS, "MIN", Value::float(f64::MIN));
    globals.set_constant_by_str(FLOAT_CLASS, "MAX_10_EXP", Value::i32(f64::MAX_10_EXP));
    globals.set_constant_by_str(FLOAT_CLASS, "MAX_EXP", Value::i32(f64::MAX_EXP));
    globals.set_constant_by_str(FLOAT_CLASS, "EPSILON", Value::float(f64::EPSILON));
    globals.define_builtin_func(FLOAT_CLASS, "to_i", toi, 0);
    globals.define_builtin_func(FLOAT_CLASS, "to_f", tof, 0);

    globals.define_basic_op(FLOAT_CLASS, "+", add, 1);
    globals.define_basic_op(FLOAT_CLASS, "-", sub, 1);
    globals.define_basic_op(FLOAT_CLASS, "*", mul, 1);
    globals.define_basic_op(FLOAT_CLASS, "/", div, 1);
    globals.define_basic_op(FLOAT_CLASS, "%", rem, 1);
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
    globals.define_builtin_func(FLOAT_CLASS, "floor", floor, 0);
    globals.define_builtin_func(FLOAT_CLASS, "finite?", finite, 0);
    globals.define_builtin_func(FLOAT_CLASS, "infinite?", infinite, 0);
    globals.define_builtin_func(FLOAT_CLASS, "nan?", nan, 0);
    globals.define_builtin_funcs(FLOAT_CLASS, "abs", &["magnitude"], abs, 0);
}

///
/// ### Float#to_f
///
/// - to_f -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/to_f.html]
#[monoruby_builtin]
fn tof(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn toi(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn div_floor(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let lhs = lfp.self_val().try_float().unwrap();
    let rhs = RealKind::try_from(lfp.arg(0))?.to_f64();
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
fn eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let b = vm.eq_values_bool(globals, lfp.self_val(), lfp.arg(0))?;
    Ok(Value::bool(b))
}

///
/// ### Integer#!=
///
/// - self != other -> bool
///
#[monoruby_builtin]
fn ne(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn cmp(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    let ord = match (lhs.try_float(), rhs.unpack()) {
        (None, _) => unreachable!(),
        (Some(lhs), RV::Fixnum(rhs)) => lhs.partial_cmp(&(rhs as f64)),
        (Some(lhs), RV::BigInt(rhs)) => lhs.partial_cmp(&rhs.to_f64().unwrap()),
        (Some(lhs), RV::Float(rhs)) => lhs.partial_cmp(&rhs),
        _ => None,
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
fn ge(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn gt(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn le(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn lt(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn floor(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    match lfp.self_val().unpack() {
        RV::Float(f) => Ok(Value::integer(f.floor() as i64)),
        _ => unreachable!(),
    }
}

///
/// ### Float#finite?
///
/// - finite? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/finite=3f.html]
#[monoruby_builtin]
fn finite(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().try_float().unwrap().is_finite()))
}

///
/// ### Float#finite?
///
/// - infinite? -> 1 | -1 | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/infinite=3f.html]
#[monoruby_builtin]
fn infinite(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let f = lfp.self_val().try_float().unwrap();
    if f.is_infinite() {
        Ok(Value::fixnum(num::signum(f) as i64))
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
fn nan(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn abs(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let f = lfp.self_val().try_float().unwrap();
    Ok(Value::float(f.abs()))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn float() {
        run_test("4.87.to_i");
        run_test("-2.18.to_i");
        run_test("4.7777.to_f");
        run_test("-725.11.to_f");
        run_test("1.2.floor");
        run_test("(-1.2).floor");
        run_test("3.0.div(2)");
        run_test("3.0.div(-2)");
        run_test("(-3.0).div(2)");
        run_test("(-3.0).div(-2)");
        run_test("(-37.044).abs");
        run_test("37.044.magnitude");
    }

    #[test]
    fn divmod() {
        run_test("(11.5).divmod(3.5)");
        run_test("(11.5).divmod(-3.5)");
        run_test("(11.5).divmod(-3.5)");
        run_test("(-11.5).divmod(3.5)");
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
}
