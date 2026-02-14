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
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);

    let (div, modulo) = match (RealKind::try_from(lhs), RealKind::try_from(rhs)) {
        (Some(lhs), Some(rhs)) => {
            if rhs.check_zero_div() {
                return Err(MonorubyErr::divide_by_zero());
            }
            let (div, modulo) = lhs.ruby_div_mod(&rhs);
            (div.into(), modulo.into())
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
    use crate::tests::*;

    #[test]
    fn binop_integer() {
        let lhs_integer = [
            "0",
            "5375",
            "690426",
            "24829482958347598570210950349530597028472983429873",
        ];
        let rhs_integer = [
            "17",
            "3454",
            "25084",
            "234234645",
            "2352354645657876868978696835652452546462456245646",
        ];
        run_binop_tests(
            &lhs_integer,
            &["+", "-", "*", "/", "&", "|", "^", "%"],
            &rhs_integer,
        );
        run_test_error("3 + :aaa");
        run_test_error("3 - :aaa");
        run_test_error("3 * :aaa");
        run_test_error("3 / :aaa");
        run_test_error("3 % :aaa");
        run_test_error("3 & :aaa");
        run_test_error("3 | :aaa");
        run_test_error("3 ^ :aaa");
    }

    #[test]
    fn binop_numeric() {
        let lhs = [
            "0",
            "690426",
            "24829482958347598570210950349530597028472983429873",
            "0.0",
            "1.2e-13",
            "1.5e21",
            "690426.0",
            "12.3+45.6i",
            "123+45.6i",
            "12.3+456i",
            "12.3+347782374687589587409204209428795359385387874687i",
            "345765834750980975394579384583756387493850539458394+45.6i",
            "123+456i",
            "123+347782374687589587409204209428795359385387874687i",
            "345765834750980975394579384583756387493850539458394+456i",
            "345765834750980975394579384583756387493850539458394+347782374687589587409204209428795359385387874687i",
        ];
        let rhs = [
            "25084",
            "234234645",
            "2352354645657876868978696835652452546462456245646",
            "1.2e-10",
            "1.9e24",
            "54.679+678.234i",
            "54679+54336i",
            "54679+678.234i",
            "54679+12988593475873426874687983435698234234882342i",
            "54.679+54336i",
            "54.679+12988593475873426874687983435698234234882342i",
            "84623499894518399348254679127328445692349+54336i",
            "84623499894518399348254679127328445692349+678.234i",
            "84623499894518399348254679127328445692349+12988593475873426874687983435698234234882342i",
        ];
        run_binop_tests(&lhs, &["+", "-", "*"], &rhs);
        run_binop_tests(&lhs[0..7], &["/", "%"], &rhs[0..5]);
        run_test_error("3.1 + :aaa");
        run_test_error("3.1 - :aaa");
        run_test_error("3.1 * :aaa");
        run_test_error("3.1 / :aaa");
        run_test_error("3.1 % :aaa");
        run_test_error("3.1 & :aaa");
        run_test_error("3.1 | :aaa");
        run_test_error("3.1 ^ :aaa");
    }

    #[test]
    fn cmp_numeric() {
        let lhs = [
            "0",
            "0.0",
            "17",
            "5375",
            "25084",
            "234234645",
            "24829482958347598570210950349530597028472983429873",
            "2352354645657876868978696835652452546462456245646",
            "234.2345",
            "690426.0",
            "234234645.0",
            "0.34e-17",
            "0.34e-18",
            "Float::NAN",
        ];
        let rhs = [":aaa", "nil", "false"];
        run_binop_tests(
            &lhs,
            &["==", "!=", "<", "<=", ">", ">=", "===", "<=>"],
            &lhs,
        );
        run_binop_tests2(&lhs, &["==", "!=", "===", "<=>"], &rhs);
        run_test_error("3 < :aaa");
        run_test_error("3 <= :aaa");
        run_test_error("3 > :aaa");
        run_test_error("3 >= :aaa");
    }

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
