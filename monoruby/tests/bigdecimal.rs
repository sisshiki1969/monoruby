extern crate monoruby;
use monoruby::tests::*;

#[test]
fn bigdecimal_require() {
    run_test_once(r#"require "bigdecimal"; BigDecimal("3.14").to_s"#);
}

#[test]
fn bigdecimal_construct() {
    run_test_once(
        r#"
        require "bigdecimal"
        res = []
        res << BigDecimal("3.14").to_s
        res << BigDecimal("123.456").to_s
        res << BigDecimal("0").to_s
        res << BigDecimal("-0").to_s
        res << BigDecimal("1e10").to_s
        res << BigDecimal("1.5e-3").to_s
        res << BigDecimal("NaN").to_s
        res << BigDecimal("Infinity").to_s
        res << BigDecimal("-Infinity").to_s
        res << BigDecimal(42).to_s
        res << BigDecimal(-42).to_s
        res << BigDecimal(0).to_s
        res
        "#,
    );
}

#[test]
fn bigdecimal_predicates() {
    run_test_once(
        r#"
        require "bigdecimal"
        res = []
        res << BigDecimal("NaN").nan?
        res << BigDecimal("3.14").nan?
        res << BigDecimal("Infinity").infinite?
        res << BigDecimal("-Infinity").infinite?
        res << BigDecimal("3.14").infinite?
        res << BigDecimal("0").zero?
        res << BigDecimal("3.14").zero?
        res << BigDecimal("0").finite?
        res << BigDecimal("Infinity").finite?
        res << BigDecimal("NaN").finite?
        res << BigDecimal("3.14").positive?
        res << BigDecimal("-3.14").positive?
        res << BigDecimal("3.14").negative?
        res << BigDecimal("-3.14").negative?
        res
        "#,
    );
}

#[test]
fn bigdecimal_sign_exponent() {
    run_test_once(
        r#"
        require "bigdecimal"
        res = []
        res << BigDecimal("3.14").sign
        res << BigDecimal("-3.14").sign
        res << BigDecimal("0").sign
        res << BigDecimal("-0").sign
        res << BigDecimal("NaN").sign
        res << BigDecimal("Infinity").sign
        res << BigDecimal("-Infinity").sign
        res << BigDecimal("3.14").exponent
        res << BigDecimal("123.456").exponent
        res << BigDecimal("0.00123").exponent
        res << BigDecimal("3.14").n_significant_digits
        res << BigDecimal("123.456").n_significant_digits
        res << BigDecimal.double_fig
        res << BigDecimal.limit
        res
        "#,
    );
}

#[test]
fn bigdecimal_arithmetic() {
    run_test_once(
        r#"
        require "bigdecimal"
        a = BigDecimal("3.14")
        b = BigDecimal("2.0")
        res = []
        res << (a + b).to_s
        res << (a - b).to_s
        res << (a * b).to_s
        res << (BigDecimal("1.5") + 3).to_s
        res << (BigDecimal("10") - BigDecimal("3")).to_s
        res << (BigDecimal("0.1") + BigDecimal("0.2")).to_s
        res
        "#,
    );
}

#[test]
fn bigdecimal_div() {
    run_test_once(
        r#"
        require "bigdecimal"
        res = []
        res << BigDecimal("10").div(BigDecimal("3"), 5).to_s
        res << BigDecimal("10").div(BigDecimal("3"), 10).to_s
        res << BigDecimal("100").div(BigDecimal("4"), 5).to_s
        res
        "#,
    );
}

#[test]
fn bigdecimal_mult_add_sub_with_prec() {
    run_test_once(
        r#"
        require "bigdecimal"
        res = []
        res << BigDecimal("3.14159").mult(BigDecimal("2.71828"), 5).to_s
        res << BigDecimal("3.14").add(BigDecimal("2.71"), 3).to_s
        res << BigDecimal("3.14").sub(BigDecimal("2.71"), 3).to_s
        res
        "#,
    );
}

#[test]
fn bigdecimal_comparison() {
    run_test_once(
        r#"
        require "bigdecimal"
        res = []
        res << (BigDecimal("1.0") == BigDecimal("1.0"))
        res << (BigDecimal("1.0") == BigDecimal("2.0"))
        res << (BigDecimal("1.0") < BigDecimal("2.0"))
        res << (BigDecimal("2.0") > BigDecimal("1.0"))
        res << (BigDecimal("1.0") >= BigDecimal("1.0"))
        res << (BigDecimal("1.0") <= BigDecimal("1.0"))
        res << (BigDecimal("1.0") <=> BigDecimal("2.0"))
        res << (BigDecimal("2.0") <=> BigDecimal("2.0"))
        res << (BigDecimal("3.0") <=> BigDecimal("2.0"))
        res << (BigDecimal("0") == BigDecimal("-0"))
        res
        "#,
    );
}

#[test]
fn bigdecimal_fix_frac() {
    run_test_once(
        r#"
        require "bigdecimal"
        res = []
        res << BigDecimal("123.456").fix.to_s
        res << BigDecimal("123.456").frac.to_s
        res << BigDecimal("0.456").fix.to_s
        res << BigDecimal("0.456").frac.to_s
        res << BigDecimal("100").fix.to_s
        res << BigDecimal("100").frac.to_s
        res
        "#,
    );
}

#[test]
fn bigdecimal_round() {
    run_test_once(
        r#"
        require "bigdecimal"
        res = []
        res << BigDecimal("1.23456789").round(3).to_s
        res << BigDecimal("-1.23456789").round(3).to_s
        res << BigDecimal("1.5").round(0).to_s
        res << BigDecimal("2.5").round(0).to_s
        res
        "#,
    );
}

#[test]
fn bigdecimal_power() {
    run_test_once(
        r#"
        require "bigdecimal"
        res = []
        res << (BigDecimal("2") ** 0).to_s
        res << (BigDecimal("2") ** 1).to_s
        res << (BigDecimal("2") ** 10).to_s
        res << (BigDecimal("3.14") ** 2).to_s
        res
        "#,
    );
}

#[test]
fn bigdecimal_decimal_shift() {
    run_test_once(
        r#"
        require "bigdecimal"
        res = []
        res << BigDecimal("1.5")._decimal_shift(2).to_s
        res << BigDecimal("1.5")._decimal_shift(-2).to_s
        res << BigDecimal("123")._decimal_shift(0).to_s
        res
        "#,
    );
}

#[test]
fn bigdecimal_to_f_to_i() {
    run_test_once(
        r#"
        require "bigdecimal"
        res = []
        res << BigDecimal("3.14").to_f
        res << BigDecimal("3.14").to_i
        res << BigDecimal("0").to_f
        res << BigDecimal("0").to_i
        res << BigDecimal("-3.14").to_f
        res << BigDecimal("-3.14").to_i
        res
        "#,
    );
}

#[test]
fn bigdecimal_special_arithmetic() {
    run_test_once(
        r#"
        require "bigdecimal"
        res = []
        res << (BigDecimal("Infinity") + BigDecimal("1")).to_s
        res << (BigDecimal("Infinity") + BigDecimal("-Infinity")).to_s
        res << (BigDecimal("Infinity") * BigDecimal("2")).to_s
        res << (BigDecimal("Infinity") * BigDecimal("-1")).to_s
        res << BigDecimal("0").zero?
        res << (-BigDecimal("3.14")).to_s
        res << BigDecimal("3.14").abs.to_s
        res << BigDecimal("-3.14").abs.to_s
        res
        "#,
    );
}

#[test]
fn bigdecimal_constants() {
    run_test_once(
        r#"
        require "bigdecimal"
        res = []
        res << BigDecimal::SIGN_NaN
        res << BigDecimal::SIGN_POSITIVE_ZERO
        res << BigDecimal::SIGN_NEGATIVE_ZERO
        res << BigDecimal::SIGN_POSITIVE_FINITE
        res << BigDecimal::SIGN_NEGATIVE_FINITE
        res << BigDecimal::SIGN_POSITIVE_INFINITE
        res << BigDecimal::SIGN_NEGATIVE_INFINITE
        res << BigDecimal::ROUND_UP
        res << BigDecimal::ROUND_DOWN
        res << BigDecimal::ROUND_HALF_UP
        res << BigDecimal::ROUND_HALF_DOWN
        res << BigDecimal::ROUND_CEILING
        res << BigDecimal::ROUND_FLOOR
        res << BigDecimal::ROUND_HALF_EVEN
        res << BigDecimal::INFINITY.to_s
        res << BigDecimal::NAN.to_s
        res
        "#,
    );
}

#[test]
fn bigdecimal_limit_and_mode() {
    run_test_once(
        r#"
        require "bigdecimal"
        res = []
        res << BigDecimal.limit
        old = BigDecimal.limit(10)
        res << old
        res << BigDecimal.limit
        BigDecimal.limit(0)
        res << BigDecimal.limit
        res
        "#,
    );
}

#[test]
fn bigdecimal_def_power_operator() {
    run_test(
        r#"
        class Foo
          def **(other)
            other * 10
          end
        end
        Foo.new ** 42
        "#,
    );
}
