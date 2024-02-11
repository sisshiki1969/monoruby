use num::ToPrimitive;

use super::*;

//
// Float class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Float", FLOAT_CLASS);
    globals.set_constant_by_str(FLOAT_CLASS, "NAN", Value::float(f64::NAN));
    globals.set_constant_by_str(FLOAT_CLASS, "INFINITY", Value::float(f64::INFINITY));
    globals.set_constant_by_str(FLOAT_CLASS, "MAX", Value::float(f64::MAX));
    globals.set_constant_by_str(FLOAT_CLASS, "MIN", Value::float(f64::MIN));
    globals.set_constant_by_str(FLOAT_CLASS, "MAX_10_EXP", Value::i32(f64::MAX_10_EXP));
    globals.set_constant_by_str(FLOAT_CLASS, "MAX_EXP", Value::i32(f64::MAX_EXP));
    globals.set_constant_by_str(FLOAT_CLASS, "EPSILON", Value::float(f64::EPSILON));
    globals.define_builtin_func(FLOAT_CLASS, "to_i", toi, 0);
    globals.define_builtin_func(FLOAT_CLASS, "to_f", tof, 0);
    globals.define_builtin_func(FLOAT_CLASS, "+", add, 1);
    globals.define_builtin_func(FLOAT_CLASS, "-", sub, 1);
    globals.define_builtin_func(FLOAT_CLASS, "*", mul, 1);
    globals.define_builtin_func(FLOAT_CLASS, "/", div, 1);
    globals.define_builtin_func(FLOAT_CLASS, "%", rem, 1);
    globals.define_builtin_func(FLOAT_CLASS, "modulo", rem, 1);
    globals.define_builtin_func(FLOAT_CLASS, "==", eq, 1);
    globals.define_builtin_func(FLOAT_CLASS, "===", eq, 1);
    globals.define_builtin_func(FLOAT_CLASS, ">=", ge, 1);
    globals.define_builtin_func(FLOAT_CLASS, ">", gt, 1);
    globals.define_builtin_func(FLOAT_CLASS, "<=", le, 1);
    globals.define_builtin_func(FLOAT_CLASS, "<", lt, 1);
    globals.define_builtin_func(FLOAT_CLASS, "!=", ne, 1);
    globals.define_builtin_func(FLOAT_CLASS, "<=>", cmp, 1);
    globals.define_builtin_func(FLOAT_CLASS, "floor", floor, 0);
}

///
/// ### Float#to_f
///
/// - to_f -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/to_f.html]
#[monoruby_builtin]
fn tof(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
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
fn toi(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    match lfp.self_val().unpack() {
        RV::Float(f) => Ok(Value::integer(f.trunc() as i64)),
        _ => unreachable!(),
    }
}

///
/// ### Float#+
///
/// - self + other -> Numeric
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/=2b.html]
#[monoruby_builtin]
fn add(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    match super::op::add_values(vm, globals, lfp.self_val(), lfp.arg(0)) {
        Some(val) => Ok(val),
        None => {
            let err = vm.take_error();
            Err(err)
        }
    }
}

///
/// ### Float#-
///
/// - self - other -> Numeric
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/=2d.html]
#[monoruby_builtin]
fn sub(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    match super::op::sub_values(vm, globals, lfp.self_val(), lfp.arg(0)) {
        Some(val) => Ok(val),
        None => {
            let err = vm.take_error();
            Err(err)
        }
    }
}

///
/// ### Float#*
///
/// - self * other -> Numeric
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/=2a.html]
#[monoruby_builtin]
fn mul(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    match super::op::mul_values(vm, globals, lfp.self_val(), lfp.arg(0)) {
        Some(val) => Ok(val),
        None => {
            let err = vm.take_error();
            Err(err)
        }
    }
}

///
/// ### Float#/
///
/// - self / other -> Numeric
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/=2f.html]
#[monoruby_builtin]
fn div(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    match super::op::div_values(vm, globals, lfp.self_val(), lfp.arg(0)) {
        Some(val) => Ok(val),
        None => {
            let err = vm.take_error();
            Err(err)
        }
    }
}

///
/// ### Float#%
///
/// - self % other -> Numeric
/// - modulo(other) -> Numeric
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/=25.html]
#[monoruby_builtin]
fn rem(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    match super::op::rem_values(vm, globals, lfp.self_val(), lfp.arg(0)) {
        Some(val) => Ok(val),
        None => {
            let err = vm.take_error();
            Err(err)
        }
    }
}

///
/// ### Integer#==
///
/// - self == other -> bool
/// - self === other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3d=3d.html]
#[monoruby_builtin]
fn eq(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let b = vm.eq_values_bool(globals, lfp.self_val(), lfp.arg(0))?;
    Ok(Value::bool(b))
}

///
/// ### Integer#!=
///
/// - self != other -> bool
///
#[monoruby_builtin]
fn ne(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
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
fn cmp(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
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
fn ge(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    match crate::executor::op::cmp_ge_values(vm, globals, lfp.self_val(), lfp.arg(0)) {
        Some(res) => Ok(res),
        None => {
            let err = vm.take_error();
            Err(err)
        }
    }
}

///
/// ### Integer#>
///
/// - self > other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3e.html]
#[monoruby_builtin]
fn gt(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    match crate::executor::op::cmp_gt_values(vm, globals, lfp.self_val(), lfp.arg(0)) {
        Some(res) => Ok(res),
        None => {
            let err = vm.take_error();
            Err(err)
        }
    }
}

///
/// ### Integer#<=
///
/// - self <= other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3c=3d.html]
#[monoruby_builtin]
fn le(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    match crate::executor::op::cmp_le_values(vm, globals, lfp.self_val(), lfp.arg(0)) {
        Some(res) => Ok(res),
        None => {
            let err = vm.take_error();
            Err(err)
        }
    }
}

///
/// ### Integer#<
///
/// - self < other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3c.html]
#[monoruby_builtin]
fn lt(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    match crate::executor::op::cmp_lt_values(vm, globals, lfp.self_val(), lfp.arg(0)) {
        Some(res) => Ok(res),
        None => {
            let err = vm.take_error();
            Err(err)
        }
    }
}

///
/// ### Float#floor
///
/// - floor([NOT SUPPORTED]ndigits = 0) -> Integer | Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/floor.html]
#[monoruby_builtin]
fn floor(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    match lfp.self_val().unpack() {
        RV::Float(f) => Ok(Value::integer(f.floor() as i64)),
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn float() {
        run_test("4.87.to_i");
        run_test("-2.18.to_i");
        run_test("4.7777.to_f");
        run_test("-725.11.to_f");
        run_test("1.2.floor");
        run_test("(-1.2).floor");
    }
}
