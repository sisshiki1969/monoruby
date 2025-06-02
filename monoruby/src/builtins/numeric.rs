use super::*;
use paste::paste;

mod complex;
mod float;
mod integer;

//
// Numeric class
//

pub(super) fn init(globals: &mut Globals) {
    let numeric = globals.define_builtin_class_under_obj("Numeric", NUMERIC_CLASS, None);
    integer::init(globals, numeric);
    float::init(globals, numeric);
    complex::init(globals, numeric);
    globals.define_builtin_func(NUMERIC_CLASS, "+", add, 1);
    globals.define_builtin_func(NUMERIC_CLASS, "-", sub, 1);
    globals.define_builtin_func(NUMERIC_CLASS, "*", mul, 1);
    globals.define_builtin_func(NUMERIC_CLASS, "/", div, 1);
    globals.define_builtin_funcs(NUMERIC_CLASS, "%", &["module"], rem, 1);
    globals.define_builtin_func(NUMERIC_CLASS, "**", pow, 1);
    globals.define_builtin_func(NUMERIC_CLASS, "-@", neg, 0);
    globals.define_builtin_func(NUMERIC_CLASS, "+@", pos, 0);
    globals.define_builtin_func(NUMERIC_CLASS, "~", bitnot, 0);
}

macro_rules! binop {
    ($op:ident) => {
        paste! {
            #[monoruby_builtin]
            fn $op(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
                match super::op::[<$op _values>](vm, globals, lfp.self_val(), lfp.arg(0)) {
                    Some(val) => Ok(val),
                    None => {
                        let err = vm.take_error();
                        Err(err)
                    }
                }
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        binop!($op1);
        binop!($($op2),+);
    };
}

binop!(add, sub, mul, div, rem, pow);

///
/// ### Float#divmod
///
/// - divmod(other) -> [Numeric]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/divmod.html]
#[monoruby_builtin]
fn divmod(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let (div, modulo) = match (lfp.self_val().unpack(), lfp.arg(0).unpack()) {
        (RV::Fixnum(lhs), RV::Fixnum(rhs)) => {
            let div = if rhs.is_negative() {
                (-lhs).div_euclid(-rhs)
            } else {
                lhs.div_euclid(rhs)
            };
            let modulo = lhs - rhs * div;
            (Value::integer(div), Value::integer(modulo))
        }
        (RV::Fixnum(lhs), RV::Float(rhs)) => {
            let lhs = lhs as f64;
            let div = if rhs.is_sign_negative() {
                (-lhs).div_euclid(-rhs)
            } else {
                lhs.div_euclid(rhs)
            };
            let modulo = lhs - rhs * div;
            (Value::integer(div as i64), Value::float(modulo))
        }
        (RV::Float(lhs), RV::Fixnum(rhs)) => {
            let rhs = rhs as f64;
            let div = if rhs.is_sign_negative() {
                (-lhs).div_euclid(-rhs)
            } else {
                lhs.div_euclid(rhs)
            };
            let modulo = lhs - rhs * div;
            (Value::integer(div as i64), Value::float(modulo))
        }
        (RV::Float(lhs), RV::Float(rhs)) => {
            let div = if rhs.is_sign_negative() {
                (-lhs).div_euclid(-rhs)
            } else {
                lhs.div_euclid(rhs)
            };
            let modulo = lhs - rhs * div;
            (Value::integer(div as i64), Value::float(modulo))
        }
        _ => return Err(MonorubyErr::cant_convert_into_float(globals, lfp.arg(0))),
    };
    Ok(Value::array2(div, modulo))
}

macro_rules! unop {
    ($op:ident) => {
        paste! {
            #[monoruby_builtin]
            fn $op(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
                match super::op::[<$op _value>](vm, globals, lfp.self_val()) {
                    Some(val) => Ok(val),
                    None => {
                        let err = vm.take_error();
                        Err(err)
                    }
                }
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        unop!($op1);
        unop!($($op2),+);
    };
}

unop!(neg, bitnot);

///
/// ### Integer#+@
///
/// - + self -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Numeric/i/=2b=40.html]
#[monoruby_builtin]
fn pos(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(lfp.self_val())
}

#[cfg(test)]
mod tests {
    use crate::tests::run_test;

    #[test]
    fn divmod() {
        run_test("(11).divmod(3)");
        run_test("(11).divmod(-3)");
        run_test("(11).divmod(-3)");
        run_test("(-11).divmod(3)");

        run_test("(11).divmod(3.5)");
        run_test("(11).divmod(-3.5)");
        run_test("(11).divmod(-3.5)");
        run_test("(-11).divmod(3.5)");

        run_test("(11.5).divmod(3)");
        run_test("(11.5).divmod(-3)");
        run_test("(11.5).divmod(-3)");
        run_test("(-11.5).divmod(3)");

        run_test("(11.5).divmod(3.5)");
        run_test("(11.5).divmod(-3.5)");
        run_test("(11.5).divmod(-3.5)");
        run_test("(-11.5).divmod(3.5)");
    }

    #[test]
    fn bitnot() {
        run_test("~1");
        run_test("~0");
        run_test("~(-1)");
        run_test("~(-2)");
        run_test("~(0x12345678)");
        run_test("~(0x123456789abcdef0)");
    }

    #[test]
    fn neg() {
        run_test("-1");
        run_test("-0");
        run_test("-(0x12345678)");
        run_test("-(0x123456789abcdef0)");
    }

    #[test]
    fn pos() {
        run_test("+1");
        run_test("+0");
        run_test("+(0x12345678)");
        run_test("+(0x123456789abcdef0)");
    }
}
