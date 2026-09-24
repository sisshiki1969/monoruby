extern crate monoruby;
use monoruby::tests::*;

#[test]
fn bigdecimal_require() {
    run_test_once(
        r#"
        require "rubygems"
        require "bigdecimal"
        BigDecimal("3.14").to_s
        "#,
    );
}

#[test]
fn bigdecimal_construct() {
    run_test_once(
        r#"
        require "rubygems"
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
        require "rubygems"
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
        require "rubygems"
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
        require "rubygems"
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
        require "rubygems"
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
        require "rubygems"
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
        require "rubygems"
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
        require "rubygems"
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
        require "rubygems"
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
        require "rubygems"
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
        require "rubygems"
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
        require "rubygems"
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
        require "rubygems"
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
        require "rubygems"
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
        require "rubygems"
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

#[test]
fn bigdecimal_dump_and_load() {
    // `_dump` / `_load` (#1648): "<digits the value's base-10**9 words
    // hold>:<to_s>", a BINARY String; `_load` skips the digit count and
    // reads the rest as `BigDecimal()` does. Marshal writes a BigDecimal
    // through them, and json/add/bigdecimal round-trips on them.
    run_test_once(
        r#"
        require "rubygems"
        require "bigdecimal"
        require "json/add/bigdecimal"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        vals = [BigDecimal("3.141", 23), BigDecimal("0"), BigDecimal("-0"), BigDecimal("1"), BigDecimal("123456789"),
                BigDecimal("1234567890"), BigDecimal("0.1"), BigDecimal("0.000000001"), BigDecimal("0.0000000001"),
                BigDecimal("1e100"), BigDecimal("12345678901234567890.123456789"), BigDecimal("NaN"), BigDecimal("Infinity"),
                BigDecimal("-Infinity"), BigDecimal("-3.141", 5), BigDecimal(1.5, 10), BigDecimal("999999999"),
                BigDecimal("1000000000"), BigDecimal("0.999999999"), BigDecimal("0.9999999999"), BigDecimal("123456789.123456789")]
        res = vals.map { |v| [v._dump, v._dump(1).encoding.to_s] }
        res << ["18:0.3141e1", "9:NaN", "100:-Infinity", "1:0.5e1", ":0.25", "0.5", "x", "", "123", "12a:1", "9:abc", "9:1.5 "].map { |s| t.() { BigDecimal._load(s).to_s } }
        res << t.() { BigDecimal._load(1) }
        res << vals.map { |v| x = Marshal.load(Marshal.dump(v)); [x.class, x.to_s] }
        res << Marshal.dump(BigDecimal("3.141", 23)).bytes
        res << t.() { Marshal.load("\x04\bu:\x0FBigDecimal\x1018:0.3141e1".b).to_s }
        res << t.() { JSON(JSON(BigDecimal("3.141", 23)), create_additions: true).to_s }
        res
        "#,
    );
}

#[test]
fn bigdecimal_reads_a_string_strictly() {
    // `BigDecimal()` refuses a String that is not wholly a number, as
    // bigdecimal's strict `VpAlloc` does; `String#to_d` stays lenient.
    run_test_once(
        r#"
        require "rubygems"
        require "bigdecimal"
        require "bigdecimal/util"
        strs = ["", " ", "abc", "1x", " 1 ", "\t1\n", "1_000", "1__0", "_1", "1_", "1_.5", "1._5", "1.5_5", "0x10", "1e", "1e+",
                "1e5", "1E5", "1d5", "1D5", "1e5_0", "1e_5", "1e-5", ".5", "5.", ".", "+.5", "-.5", "--5", "Infinity", "+Infinity",
                "-Infinity", "infinity", "NaN", "nan", "-NaN", " NaN ", "Inf", "1.2.3", "1 2", "1,000", "0b1", "1e1.5", "00012",
                "-0", "5.e3", ".e3", "e3", "1_0e1_0", "1_e5", "1.5_", "-1_", "1\0", "a\"b"]
        [strs.map { |s| begin; BigDecimal(s).to_s; rescue => e; [e.class, e.message]; end },
         BigDecimal("1x", exception: false), "1x".to_d.to_s, "abc".to_d.to_s]
        "#,
    );
}
