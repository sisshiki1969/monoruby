extern crate monoruby;
use monoruby::tests::*;

#[test]
fn rational_creation() {
    run_test("Rational(3, 4).numerator");
    run_test("Rational(3, 4).denominator");
    run_test("Rational(6, 4).numerator");
    run_test("Rational(6, 4).denominator");
    run_test("Rational(3).numerator");
    run_test("Rational(3).denominator");
    run_test("Rational(-3, 4).numerator");
    run_test("Rational(-3, 4).denominator");
    run_test("Rational(3, -4).numerator");
    run_test("Rational(3, -4).denominator");
    run_test("Rational(-3, -4).numerator");
    run_test("Rational(-3, -4).denominator");
}

#[test]
fn rational_to_s() {
    run_test("Rational(3, 4).to_s");
    run_test("Rational(6, 4).to_s");
    run_test("Rational(5, 1).to_s");
    run_test("Rational(-3, 4).to_s");
    run_test("Rational(3, -4).to_s");
}

#[test]
fn rational_inspect() {
    run_test("Rational(3, 4).inspect");
    run_test("Rational(5, 1).inspect");
}

#[test]
fn rational_arithmetic() {
    run_test("(Rational(1, 2) + Rational(1, 3)).to_s");
    run_test("(Rational(3, 4) + Rational(1, 4)).numerator");
    run_test("(Rational(3, 4) + Rational(1, 4)).denominator");
    run_test("(Rational(3, 4) - Rational(1, 4)).to_s");
    run_test("(Rational(3, 4) * Rational(2, 3)).to_s");
    run_test("(Rational(3, 4) / Rational(2, 3)).to_s");
    run_test("(Rational(3, 4) + 1).to_s");
    run_test("(Rational(3, 4) * 2).to_s");
    run_test("(Rational(3, 4) - 1).to_s");
    run_test("(Rational(3, 4) / 2).to_s");
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
    run_test("Rational(3, 4) == Rational(6, 8)");
    run_test("Rational(3, 4) == Rational(1, 2)");
    run_test("Rational(4, 1) == 4");
    run_test_once("Rational(3, 4) == 0.75");
    run_test("(Rational(3, 4) <=> Rational(1, 2))");
    run_test("(Rational(1, 2) <=> Rational(3, 4))");
    run_test("(Rational(1, 2) <=> Rational(1, 2))");
    run_test("Rational(3, 4) > Rational(1, 2)");
    run_test("Rational(1, 4) < Rational(1, 2)");
    run_test("Rational(1, 2) >= Rational(1, 2)");
    run_test("Rational(1, 2) <= Rational(1, 2)");
}

#[test]
fn rational_conversion() {
    run_test_once("Rational(3, 4).to_f");
    run_test("Rational(7, 2).to_i");
    run_test("Rational(3, 4).to_r.to_s");
}

#[test]
fn rational_predicates() {
    run_test("Rational(3, 4).zero?");
    run_test("Rational(0, 1).zero?");
    run_test("Rational(3, 4).positive?");
    run_test("Rational(-3, 4).positive?");
    run_test("Rational(-3, 4).negative?");
    run_test("Rational(3, 4).negative?");
}

#[test]
fn rational_unary() {
    run_test("(-Rational(3, 4)).to_s");
    run_test("(+Rational(3, 4)).to_s");
    run_test("Rational(-3, 4).abs.to_s");
}

#[test]
fn rational_power() {
    run_test("(Rational(2, 3) ** 2).to_s");
    run_test("(Rational(2, 3) ** 0).to_s");
    run_test("(Rational(2, 3) ** -1).to_s");
}

#[test]
fn rational_class() {
    run_test("Rational(3, 4).class.to_s");
    run_test("Rational(3, 4).is_a?(Rational)");
    run_test("Rational(3, 4).is_a?(Numeric)");
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
    run_test("3.to_r.to_s");
    run_test("3.to_r.numerator");
    run_test("3.to_r.denominator");
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
    run_test("Rational(3, 4).eql?(Rational(3, 4))");
    run_test("Rational(3, 4).eql?(Rational(6, 8))");
    run_test("Rational(3, 4).eql?(0.75)");
}

#[test]
fn rational_integer_predicates() {
    run_test("Rational(3, 4).integer?");
    run_test("Rational(3, 4).finite?");
    run_test("Rational(3, 4).infinite?");
}

#[test]
fn rational_hash_eq() {
    run_test("Rational(3, 4).hash == Rational(3, 4).hash");
}
