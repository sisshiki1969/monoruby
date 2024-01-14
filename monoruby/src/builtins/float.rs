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
    globals.define_builtin_func(FLOAT_CLASS, "to_i", toi);
    globals.define_builtin_func(FLOAT_CLASS, "to_f", tof);
    globals.define_builtin_func(FLOAT_CLASS, "floor", floor);
}

///
/// ### Float#to_f
///
/// - to_f -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/to_f.html]
#[monoruby_builtin]
fn tof(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    lfp.check_number_of_arguments(0)?;
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
    lfp.check_number_of_arguments(0)?;
    match lfp.self_val().unpack() {
        RV::Float(f) => Ok(Value::integer(f.trunc() as i64)),
        _ => unreachable!(),
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
    lfp.check_number_of_arguments(0)?;
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
