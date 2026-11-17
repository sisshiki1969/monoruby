//! The Numeric methods Complex does not have (#2024).
//!
//! CRuby undefines them on Complex (`rb_undef_method(rb_cComplex, …)`):
//! `%`, `div`, `divmod`, `floor`, `ceil`, `modulo`, `remainder`,
//! `round`, `step`, `truncate`, `i`, `positive?`, `negative?`, and the
//! comparisons Comparable gives Numeric. `Complex.new` / `.allocate` are
//! undefined too.

use monoruby::tests::*;

const HELPER: &str = r####"
def t; yield.inspect; rescue Exception => e; "#{e.class}: #{e.message}"; end
c = Complex(1, 2)
L = [:%, :div, :divmod, :floor, :ceil, :modulo, :remainder, :round, :step, :truncate, :i, :positive?, :negative?, :<, :<=, :>, :>=, :between?, :clamp]
"####;

fn probe(exprs: &[&str]) {
    let list = exprs
        .iter()
        .map(|s| format!("t {{ {s} }}"))
        .collect::<Vec<_>>()
        .join(",\n");
    run_test_once(&format!("{HELPER}\n[{list}]"));
}

/// None of them responds, and each call is a NoMethodError.
#[test]
fn undefined_on_complex() {
    probe(&[
        r####"L.select { |m| c.respond_to?(m) }"####,
        r####"c % 2"####,
        r####"c.remainder(2)"####,
        r####"c.divmod(2)"####,
        r####"L.map { |m| t { c.send(m, 1) } }"####,
    ]);
}

/// What `instance_methods`, `method_defined?` and
/// `undefined_instance_methods` say.
#[test]
fn method_tables() {
    probe(&[
        r####"Complex.instance_methods(false).sort"####,
        r####"Complex.instance_methods.sort - Object.instance_methods"####,
        r####"L.map { |m| Complex.method_defined?(m) } .uniq"####,
        r####"L.map { |m| (Complex.instance_method(m) rescue $!.message) }.uniq.size"####,
        r####"Complex.instance_method(:%)"####,
        r####"[Complex.ancestors, Complex.private_instance_methods(false).sort]"####,
        r####"[Complex.undefined_instance_methods.sort]"####,
        r####"[Complex.singleton_methods.sort, (Complex.new rescue $!.message), (Complex.allocate rescue $!.message)]"####,
    ]);
}

/// The rest of Complex is as it was, and a Complex as the other
/// operand.
#[test]
fn what_complex_keeps() {
    probe(&[
        r####"[c.respond_to?(:abs), c.respond_to?(:real?), c.zero?, c.nonzero?, c.integer?, c.finite?, c.infinite?, c.real?, (c.coerce(1)), c.quo(2), c.fdiv(2), c.abs2, c.arg, c.conj, -c, +c, c <=> 1, c == 1]"####,
        r####"[1 < c]"####,
        r####"[1.0.clamp(c, c)]"####,
        r####"[Complex(1,0) <=> 2, Complex(1, 0).to_i, Complex(1,0).to_r, Complex(1, 0).to_f]"####,
        r####"[c.eql?(c), c.hash == Complex(1,2).hash, c.dup.equal?(c), c.to_c.equal?(c), (c.i rescue $!.class)]"####,
        r####"1.step(c, 1).to_a"####,
        r####"[c.rect, c.polar.size, c.abs, c ** 2, c.numerator, c.denominator, (c.rationalize rescue $!.class)]"####,
    ]);
}
