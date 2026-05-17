extern crate monoruby;
use monoruby::tests::*;

#[test]
fn rational_creation() {
    run_tests(&[
        "Rational(3, 4).numerator",
        "Rational(3, 4).denominator",
        "Rational(6, 4).numerator",
        "Rational(6, 4).denominator",
        "Rational(3).numerator",
        "Rational(3).denominator",
        "Rational(-3, 4).numerator",
        "Rational(-3, 4).denominator",
        "Rational(3, -4).numerator",
        "Rational(3, -4).denominator",
        "Rational(-3, -4).numerator",
        "Rational(-3, -4).denominator",
        "Rational(3, 4).to_s",
        "Rational(6, 4).to_s",
        "Rational(5, 1).to_s",
        "Rational(-3, 4).to_s",
        "Rational(3, -4).to_s",
        "Rational(3, 4).inspect",
        "Rational(5, 1).inspect",
        "(Rational(1, 2) + Rational(1, 3)).to_s",
        "(Rational(3, 4) + Rational(1, 4)).numerator",
        "(Rational(3, 4) + Rational(1, 4)).denominator",
        "(Rational(3, 4) - Rational(1, 4)).to_s",
        "(Rational(3, 4) * Rational(2, 3)).to_s",
        "(Rational(3, 4) / Rational(2, 3)).to_s",
        "(Rational(3, 4) + 1).to_s",
        "(Rational(3, 4) * 2).to_s",
        "(Rational(3, 4) - 1).to_s",
        "(Rational(3, 4) / 2).to_s",
    ]);
}

#[test]
fn rational_arithmetic_float() {
    run_test_once("Rational(3, 4) + 0.5");
    run_test_once("Rational(3, 4) - 0.5");
    run_test_once("Rational(3, 4) * 0.5");
    run_test_once("Rational(3, 4) / 0.5");
}

#[test]
fn rational_comparison() {
    run_tests(&[
        "Rational(3, 4) == Rational(6, 8)",
        "Rational(3, 4) == Rational(1, 2)",
        "Rational(4, 1) == 4",
    ]);
    run_test_once("Rational(3, 4) == 0.75");
    run_tests(&[
        "(Rational(3, 4) <=> Rational(1, 2))",
        "(Rational(1, 2) <=> Rational(3, 4))",
        "(Rational(1, 2) <=> Rational(1, 2))",
        "Rational(3, 4) > Rational(1, 2)",
        "Rational(1, 4) < Rational(1, 2)",
        "Rational(1, 2) >= Rational(1, 2)",
        "Rational(1, 2) <= Rational(1, 2)",
    ]);
}

#[test]
fn rational_conversion() {
    run_test_once("Rational(3, 4).to_f");
    run_tests(&[
        "Rational(7, 2).to_i",
        "Rational(3, 4).to_r.to_s",
    ]);
}

#[test]
fn rational_predicates() {
    run_tests(&[
        "Rational(3, 4).zero?",
        "Rational(0, 1).zero?",
        "Rational(3, 4).positive?",
        "Rational(-3, 4).positive?",
        "Rational(-3, 4).negative?",
        "Rational(3, 4).negative?",
        "(-Rational(3, 4)).to_s",
        "(+Rational(3, 4)).to_s",
        "Rational(-3, 4).abs.to_s",
        "(Rational(2, 3) ** 2).to_s",
        "(Rational(2, 3) ** 0).to_s",
        "(Rational(2, 3) ** -1).to_s",
        "Rational(3, 4).class.to_s",
        "Rational(3, 4).is_a?(Rational)",
        "Rational(3, 4).is_a?(Numeric)",
    ]);
}

#[test]
fn rational_float_to_r() {
    run_test_once("0.5.to_r.to_s");
    run_test_once("0.75.to_r.to_s");
    run_test_once("0.5.to_r.numerator");
    run_test_once("0.5.to_r.denominator");
}

#[test]
fn rational_integer_to_r() {
    run_tests(&[
        "3.to_r.to_s",
        "3.to_r.numerator",
        "3.to_r.denominator",
    ]);
}

#[test]
fn rational_from_float() {
    run_test_once("Rational(0.5).to_s");
    run_test_once("Rational(0.75).to_s");
    run_test_once("Rational(0.5).numerator");
    run_test_once("Rational(0.5).denominator");
}

#[test]
fn rational_zero_division() {
    run_test_error("Rational(1, 0)");
    run_test_error("Rational(3, 4) / 0");
    run_test_error("Rational(3, 4) / Rational(0, 1)");
}

#[test]
fn rational_eql() {
    run_tests(&[
        "Rational(3, 4).eql?(Rational(3, 4))",
        "Rational(3, 4).eql?(Rational(6, 8))",
        "Rational(3, 4).eql?(0.75)",
        "Rational(3, 4).integer?",
        "Rational(3, 4).finite?",
        "Rational(3, 4).infinite?",
        "Rational(3, 4).hash == Rational(3, 4).hash",
    ]);
}
