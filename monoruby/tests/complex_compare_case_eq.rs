//! `Complex` asks its parts rather than assuming what they are, and `===`
//! is a method entry of its own (#2011).
//!
//! A `Complex` may hold any real `Numeric`, so `==`, `<=>`, `to_r`,
//! `rationalize` and friends go through the part's own `==` / `<=>` /
//! `coerce` (`nucomp_eqeq_p`, `nucomp_cmp`, `f_zero_p`, `k_exact_zero_p`
//! in complex.c). `Integer#===`, `Float#===`, `String#===` are separate
//! entries over the same function as `==` — `1.method(:===)` is not an
//! alias display and a backtrace says `Integer#===` — while Array, Hash
//! and Symbol have no `===` at all and take `Kernel#===`.

use monoruby::tests::*;

const HELPER: &str = r#"
  t = ->(&b) { begin; b.call; rescue Exception => e; [e.class.to_s, e.message]; end }
  $log = []
  class ImNum < Numeric
    def initialize(v) = @v = v
    def ==(o) = ($log << [:eq, @v, o.class]; @v == o)
    def real? = true
    def coerce(o) = [o, @v]
    def to_s = "im#{@v}"
    def inspect = "im#{@v}"
    def negative? = false
    def <(o) = @v < o
    def <=>(o) = @v <=> o
  end
  c0 = Complex(1, ImNum.new(0))
  c1 = Complex(1, ImNum.new(2))
  class Boom; def ==(o) = raise("boom"); end
  bt = ->(&b) { begin; b.call; rescue => e; e.backtrace.map { |x| x[/in '(.*)'/, 1] }; end }
"#;

fn probe(exprs: &[&str]) {
    let list = exprs
        .iter()
        .map(|s| format!("t.() {{ {s} }}"))
        .collect::<Vec<_>>()
        .join(",\n");
    run_test_once(&format!("{HELPER}\n[{list}]"));
}

/// The issue's examples: a part that is a user-defined Numeric.
#[test]
fn complex_with_numeric_parts() {
    probe(&[
        "c1 == 1",
        "c0 == 1",
        "c0 == 2",
        "c0.rationalize",
        "c1.rationalize",
        "c0.rationalize(Rational(1, 100))",
        "c0 == c0",
        "c0 == c1",
        "c0 == Complex(1, 0)",
        "Complex(1, 0) == c0",
        "c1 == Complex(1, 2)",
        "c0 <=> 1",
        "c1 <=> 1",
        "c0 <=> Complex(1, 0)",
        "Complex(1, 0) <=> c0",
        "Complex(1, 0) <=> c1",
        "c0 <=> Rational(1, 2)",
        "c0.to_i",
        "c0.to_f",
        "c0.to_r",
        "c1.to_r",
        "c1.to_i",
        "c1.to_f",
        "$log",
    ]);
}

/// Builtin parts keep their answers.
#[test]
fn complex_with_builtin_parts() {
    probe(&[
        "Complex(1, 0) <=> Rational(1, 2)",
        "Complex(1, 0) <=> ImNum.new(3)",
        "Complex(1, 0) <=> \"a\"",
        "Complex(1, 0) <=> nil",
        "Complex(1, 2) <=> 1",
        "Complex(1, 0) <=> Complex(2, 0)",
        "Complex(1, 0) <=> Complex(2, 1)",
        "Complex(1, 0.0).rationalize",
        "Complex(1, 0.0) == 1",
        "Complex(1, 0.0) == ImNum.new(1)",
        "Complex(1, 2) == ImNum.new(1)",
        "Complex(1, 2) == Complex(1, 2)",
        "Complex(1, 2) == Complex(1.0, 2.0)",
        "Complex(1, 2) == Complex(1, 3)",
        "Complex(1, 0) == 1",
        "Complex(1, 0) == 1.0",
        "Complex(1, 0) == Rational(1)",
        "Complex(1, 1) == 1",
        "Complex(1, 0) == \"a\"",
        "Complex(1, 0) == nil",
        "Complex(3, 0).to_i",
        "Complex(3, 0).to_r",
        "Complex(3, 1).to_r",
        "Complex(3, 0.0).to_r",
        "Complex(3, 0.0).to_i",
        "Complex(3.5, 0).rationalize(Rational(1, 10))",
        "Complex(3, 1).rationalize",
        "Complex(2**70, 0) == 2**70",
        "Complex(2**70, 0) <=> 2**70",
    ]);
}

/// `==` on a part may be anything, including one that coerces.
#[test]
fn complex_cmp_through_coerce() {
    probe(&[
        r#"
        k = Class.new(Numeric) do
          def coerce(o) = [o.real.to_f, 2.5]
        end
        [Complex(1, 0) <=> k.new, Complex(3, 0) <=> k.new]
        "#,
        r#"
        k = Class.new { def coerce(o) = [1, 2] }
        Complex(1, 0) <=> k.new
        "#,
        r#"
        k = Class.new { def coerce(o) = :no }
        Complex(1, 0) <=> k.new
        "#,
        r#"
        k = Class.new { def coerce(o) = raise("no") }
        Complex(1, 0) <=> k.new
        "#,
    ]);
}

/// `===` is its own method where CRuby defines one, and Kernel's
/// elsewhere.
#[test]
fn case_eq_method_entries() {
    probe(&[
        "[1.method(:===).inspect, 1.0.method(:===).inspect, 1.method(:==).inspect]",
        "[1.method(:===).name, 1.method(:===).original_name, 1.method(:===).owner]",
        "1.method(:===).unbind.inspect",
        "\"a\".method(:===).inspect",
        "[\"a\".method(:===).name, \"a\".method(:==).name, \"a\".method(:eql?).name]",
        "[[].method(:===).owner, {}.method(:===).owner, :a.method(:===).owner]",
        "[1.0.method(:===).owner, (2**70).method(:===).owner, \"a\".method(:===).owner]",
        "[Integer, Float, String, Array, Hash, Symbol].map { _1.instance_methods(false).include?(:===) }",
        "1.method(:===) == 1.method(:==)",
        "Integer.instance_method(:===) == Integer.instance_method(:==)",
        "1.0.method(:===) == 1.0.method(:==)",
        "\"a\".method(:===) == \"a\".method(:==)",
        "1.method(:==) == 1.method(:==)",
        "1.method(:+) == 1.method(:-)",
        "[1 === 1, 1 === 1.0, 1 === 2, 1.0 === 1, \"a\" === \"a\", \"a\" === :a, :a === :a]",
        "[[1] === [1], [1] === [2], {a: 1} === {a: 1}, :a === \"a\", (2**70) === 2**70]",
        "case 1.0 when 1 then :int else :none end",
        "case \"a\" when \"a\" then :str else :none end",
        "case [1] when [1] then :ary else :none end",
        "case :a when :a then :sym else :none end",
    ]);
}

/// The frame a `===` leaves in a backtrace.
#[test]
fn case_eq_backtrace() {
    probe(&[
        "bt.() { 1 === Boom.new }",
        "bt.() { 1.0 === Boom.new }",
        "bt.() { 1.send(:===, Boom.new) }",
        "bt.() { 1.0.send(:===, Boom.new) }",
        "bt.() { (2**70) === Boom.new }",
        "bt.() { 1 == Boom.new }",
        "bt.() { 1.0 == Boom.new }",
        "bt.() { case Boom.new; when 1 then 1; end }",
        "bt.() { case Boom.new; when 1.0 then 1; end }",
        "bt.() { [Boom.new] === [1] }",
        "bt.() { [1] === [Boom.new] }",
        "bt.() { {a: 1} === {a: Boom.new} }",
    ]);
}

/// Compiled `case` / `===` on the builtin classes.
#[test]
fn case_eq_compiled() {
    run_test(
        r#"
        def f(x)
          case x
          when 1 then :one
          when 2.0 then :two
          when "s" then :str
          when :sym then :sym
          when [1] then :ary
          else :other
          end
        end
        def g(a, b) = a === b
        res = []
        30.times do
          res << f(1) << f(2) << f(2.0) << f("s") << f(:sym) << f([1]) << f(nil)
          res << g(1, 1) << g(1, 1.0) << g(1.5, 1.5) << g("a", "a") << g([1], [1]) << g(:a, :b)
        end
        res.uniq
        "#,
    );
}

/// `<=>` with something that is no real number goes through its `coerce`
/// (`rb_num_coerce_cmp`), and an imaginary part is asked whether it is
/// zero whatever its class (`f_zero_p`, `k_exact_zero_p`).
#[test]
fn complex_cmp_coerce_results_and_zero_parts() {
    probe(&[
        "Complex(1, 0) <=> Object.new",
        "k = Class.new { def coerce(o) = nil }; Complex(1, 0) <=> k.new",
        "k = Class.new { def coerce(o) = [2, 3] }; Complex(1, 0) <=> k.new",
        "k = Class.new { def coerce(o) = 5 }; Complex(1, 0) <=> k.new",
        "Complex(1, 0r) <=> 1",
        "Complex(1, 0r) <=> Complex(2, 0r)",
        "Complex(1, 2**70) <=> 1",
        "Complex(1, 1r) <=> 1",
        "c0 <=> 1",
        "c1 <=> 1",
        "c0.to_i",
        "c1.to_i",
        "c1.to_r",
        "Complex(1, 0r).to_i",
        "Complex(1, 2**70).to_i",
    ]);
}
