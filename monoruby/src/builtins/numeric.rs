use super::*;
use paste::paste;

mod complex;
mod float;
mod integer;
pub(super) mod rational;

//
// Numeric class
//

pub(super) fn init(globals: &mut Globals) {
    let numeric = globals.define_builtin_class_under_obj("Numeric", NUMERIC_CLASS, None);
    integer::init(globals, numeric);
    float::init(globals, numeric);
    complex::init(globals, numeric);
    rational::init(globals, numeric);
    globals.define_builtin_func(NUMERIC_CLASS, "+", add, 1);
    globals.define_builtin_func(NUMERIC_CLASS, "-", sub, 1);
    globals.define_builtin_func(NUMERIC_CLASS, "*", mul, 1);
    globals.define_builtin_func(NUMERIC_CLASS, "/", div, 1);
    globals.define_builtin_funcs(NUMERIC_CLASS, "%", &["module"], rem, 1);
    globals.define_builtin_func(NUMERIC_CLASS, "**", pow, 1);
    globals.define_builtin_func(NUMERIC_CLASS, "-@", neg, 0);
    globals.define_builtin_func(NUMERIC_CLASS, "~", bitnot, 0);
    globals.define_builtin_funcs(NUMERIC_CLASS, "angle", &["arg", "phase"], angle, 0);
}

macro_rules! binop {
    ($op:ident) => {
        paste! {
            #[monoruby_builtin]
            fn $op(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn divmod(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);

    // Helper for FloatDomainError
    let float_domain_err = |msg: &str| -> MonorubyErr {
        let class_id = globals
            .store
            .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("FloatDomainError"))
            .map(|v| v.as_class_id());
        match class_id {
            Some(cid) => MonorubyErr::new(MonorubyErrKind::Other(cid), msg),
            None => MonorubyErr::rangeerr(msg),
        }
    };
    let (div, modulo) = match (RealKind::try_from(lhs), RealKind::try_from(rhs)) {
        (Some(lhs), Some(rhs)) => {
            // NaN raises FloatDomainError
            if let RealKind::Float(f) = rhs {
                if f.is_nan() {
                    return Err(float_domain_err("NaN"));
                }
            }
            if let RealKind::Float(f) = lhs {
                if f.is_nan() {
                    return Err(float_domain_err("NaN"));
                }
                if f.is_infinite() {
                    return Err(float_domain_err(if f > 0.0 { "Infinity" } else { "-Infinity" }));
                }
            }
            // For divmod, both integer zero and float zero raise ZeroDivisionError
            if rhs.check_zero_div() || rhs.is_float_zero() {
                return Err(MonorubyErr::divide_by_zero());
            }
            let (div, modulo) = lhs.ruby_div_mod(&rhs);
            // Ruby's divmod always returns an Integer quotient
            let div = match div {
                RealKind::Float(f) => Value::coerce_f64_to_int(f)?,
                _ => div.into(),
            };
            (div, modulo.into())
        }
        _ => return Err(MonorubyErr::cant_convert_into_float(globals, lfp.arg(0))),
    };
    Ok(Value::array2(div, modulo))
}

macro_rules! unop {
    ($op:ident) => {
        paste! {
            #[monoruby_builtin]
            fn $op(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
/// ### Numeric#angle
///
/// - angle -> 0 | Float
/// - arg -> 0 | Float
/// - phase -> 0 | Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Numeric/i/angle.html]
#[monoruby_builtin]
fn angle(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let is_negative = match super::op::cmp_lt_values(vm, globals, self_val, Value::integer(0)) {
        Some(v) => v == Value::bool(true),
        None => return Err(vm.take_error()),
    };
    if is_negative {
        Ok(Value::float(std::f64::consts::PI))
    } else {
        Ok(Value::integer(0))
    }
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

    #[test]
    fn comparable_clamp() {
        run_test_once(
            r#"
            class MyNum
              include Comparable
              attr_reader :val
              def initialize(v); @val = v; end
              def <=>(other); @val <=> other.val; end
            end
            a = MyNum.new(1)
            b = MyNum.new(3)
            c = MyNum.new(5)
            d = MyNum.new(4)
            [a.clamp(b, c).val, c.clamp(b, c).val, d.clamp(b, c).val]
        "#,
        );
        run_test_error(
            r#"
            class MyNum
              include Comparable
              attr_reader :val
              def initialize(v); @val = v; end
              def <=>(other); @val <=> other.val; end
            end
            MyNum.new(1).clamp(MyNum.new(0)...MyNum.new(2))
        "#,
        );
    }

    #[test]
    fn comparable_between() {
        run_test_once(
            r#"
            class MyNum
              include Comparable
              attr_reader :val
              def initialize(v); @val = v; end
              def <=>(other); @val <=> other.val; end
            end
            a = MyNum.new(3)
            lo = MyNum.new(1)
            hi = MyNum.new(5)
            [a.between?(lo, hi), MyNum.new(0).between?(lo, hi), MyNum.new(6).between?(lo, hi)]
        "#,
        );
    }

    #[test]
    fn float_truncate() {
        run_test("1.5.truncate");
        run_test("(-1.5).truncate");
        run_test("1.567.truncate(2)");
        run_test("(-1.567).truncate(2)");
    }

    #[test]
    fn float_ceil() {
        run_test("1.1.ceil");
        run_test("(-1.1).ceil");
        run_test("1.0.ceil");
        run_test("1.123.ceil(2)");
    }

    #[test]
    fn float_positive_negative() {
        run_test("1.0.positive?");
        run_test("(-1.0).positive?");
        run_test("0.0.positive?");
        run_test("1.0.negative?");
        run_test("(-1.0).negative?");
        run_test("0.0.negative?");
    }

    #[test]
    fn float_integer() {
        run_test("1.0.integer?");
        run_test("1.5.integer?");
    }

    #[test]
    fn float_coerce() {
        run_test("1.0.coerce(2)");
        run_test("1.0.coerce(2.5)");
    }

    #[test]
    fn float_remainder() {
        run_test("5.0.remainder(3.0)");
        run_test("(-5.0).remainder(3.0)");
        run_test("5.0.remainder(-3.0)");
    }

    #[test]
    fn float_fdiv() {
        run_test("1.0.fdiv(2)");
        run_test("1.0.fdiv(2.0)");
    }

    #[test]
    fn numeric_abs() {
        run_test("1.0.abs");
        run_test("(-1.0).abs");
        run_test("0.0.abs");
        run_test("1.0.magnitude");
    }

    #[test]
    fn numeric_step() {
        run_test("res = []; 1.step(10, 2) {|i| res << i}; res");
        run_test("res = []; 1.step(to: 5, by: 2) {|i| res << i}; res");
        run_test("res = []; 1.0.step(2.0, 0.5) {|i| res << i}; res");
        run_test("res = []; 5.step(1, -1) {|i| res << i}; res");
        run_test("res = []; 1.step(5) {|i| res << i}; res");
    }
}
