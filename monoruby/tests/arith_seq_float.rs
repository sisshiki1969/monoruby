extern crate monoruby;
use monoruby::tests::*;

// `Enumerator::ArithmeticSequence` follows CRuby's `arith_seq_*`
// (enumerator.c): the sequence is a Float one as soon as *any* of begin,
// end and step is a Float (`ruby_float_step`), not only when begin or
// step is (#1990) — `(1..3.0).step(1).to_a` is `[1.0, 2.0, 3.0]`. A Float
// sequence has `n = floor((end - beg) / unit + err)` terms, each computed
// as `i * unit + beg` and clamped to the end, so it neither accumulates
// error nor overshoots.

const HELPER: &str = r#"
  t = ->(&b) { begin; b.call; rescue Exception => e; [e.class.to_s, e.message]; end }
  probe = ->(s) {
    inf = s.end.nil? || (s.end.is_a?(Float) && s.end.infinite?)
    [
      t.() { s.size }, t.() { s.first }, t.() { s.first(2) }, t.() { s.first(5) },
      t.() { s.last }, t.() { s.last(2) }, t.() { s.last(9) },
      t.() { inf ? s.take(3) : s.to_a },
      t.() { a = []; s.each { |x| a << x; break if a.size > 6 }; a },
      t.() { inf ? :skip : s.map { _1 } },
      t.() { inf ? :skip : [s.min, s.max, s.sum] },
      t.() { inf ? :skip : [s.include?(2), s.include?(2.0)] },
      t.() { s.lazy.map { _1 * 2 }.first(2) },
      t.() { s.each_slice(2).first(1) },
      t.() { s.each.equal?(s) },
      t.() { e = s.each; [e.next, e.next] rescue [$!.class] },
      t.() { [s === s.first, s == s] },
    ]
  }
"#;

fn probe(seqs: &[&str]) {
    let list = seqs
        .iter()
        .map(|s| format!("probe.({s})"))
        .collect::<Vec<_>>()
        .join(",\n");
    run_test_once(&format!("{HELPER}\n[{list}]"));
}

/// The issue's examples: an Integer begin with a Float end.
#[test]
fn integer_begin_float_end() {
    run_test(
        r#"
        [
          (1..3.0).step.to_a, (1..3.0).step.first(2), (1..3.0).step.map { _1 },
          1.step(3.0).to_a, ((1..3.0) % 1).to_a, (1..3.0).step(1).first,
          (1..3.0).step(1).last, (1..3.0).step(1).last(2), (1..3.0).step(1).size,
          (1..3.0).step(1).each_slice(2).to_a, (1..3.0).step(1).with_index.to_a,
        ]
        "#,
    );
    probe(&[
        "(1..3.0).step",
        "(1..3.0).step(1)",
        "(1...3.0).step(1)",
        "1.step(3.0)",
        "1.step(3.0, 1)",
        "(1..3.0) % 1",
        "(3..1.0).step(-1)",
        "(3...1.0).step(-1)",
        "(1..5.5).step(2)",
        "(1...5.0).step(2)",
        "(1...5.0).step(2.0)",
        "(5...1.0).step(-2)",
        "1.step(by: 2, to: 6.0)",
        "1.step(by: -2.0, to: -4)",
        "(1..1.0e20).step(1.0e19)",
    ]);
}

/// A Float begin or step: the term count carries CRuby's epsilon error
/// bound, and the last term is the end itself.
#[test]
fn float_begin_or_step() {
    probe(&[
        "(1.0..3).step(1)",
        "(1..3).step(1.0)",
        "(1..3).step(0.5)",
        "1.0.step(1.3, 0.1)",
        "(1.0..1.3).step(0.1)",
        "(1.0...1.3).step(0.1)",
        "(1.0..2.0).step(0.1)",
        "(1.0...2.0).step(0.1)",
        "(0.0..1.0).step(0.3)",
        "(0.1..0.9).step(0.2)",
    ]);
}

#[test]
fn infinite_and_endless() {
    probe(&[
        "(1.0..3.0).step(Float::INFINITY)",
        "(3.0..1.0).step(Float::INFINITY)",
        "(1.0..Float::INFINITY).step(2)",
        "(1..Float::INFINITY).step(2)",
        "(1..).step(2)",
        "(1.0..).step(2)",
        "1.step",
        "1.0.step(by: -1)",
        "1.step(by: -1.5)",
    ]);
}

/// The Integer sequences are untouched, bignums included.
#[test]
fn integer_sequences() {
    probe(&[
        "(3..1).step(1)",
        "(1..3).step(-1)",
        "(1..10).step(3)",
        "(1...10).step(3)",
        "(10..1).step(-3)",
        "(10...1).step(-3)",
        "(1..2**70).step(2**68)",
        "(2**70..2**70+5).step(2)",
        "(1..2r).step(1)",
    ]);
}

/// A hot Float `each`, long enough to be compiled.
#[test]
fn float_each_in_a_loop() {
    run_test(
        r#"
        sum = 0
        (1..200.0).step(0.5).each { |x| sum += x }
        n = 0
        (1..300).step(3) { |x| n += x }
        down = []
        (5..1.0).step(-1.5) { |x| down << x }
        [sum, n, down]
        "#,
    );
}

/// The Enumerator half of the interface: external iteration, `with_index`
/// / `with_object`, `+`, `rewind` — and no state visible to reflection.
#[test]
fn enumerator_interface() {
    run_test_once(
        r#"
        e = (1..3).step(1)
        [
          (1..3.0).step(1).each.class,
          1.step(10, 3).each_entry.to_a,
          (1..10).step(3).reverse_each.to_a,
          (1..10.0).step(3).reverse_each.to_a,
          (1..10).step(3).each_cons(2).to_a,
          (1..3).step(1).rewind.class,
          [e.next, e.peek, e.next, e.next, (e.next rescue $!.class)],
          (1..3).step(1).with_index(1).to_a,
          (1..3).step(1).with_object([]).to_a.size,
          ((1..3).step(1) + [9]).to_a,
          (1..3.0).step(1).with_index.to_a,
          [(1..3).step(1).frozen?, 1.step(3).frozen?],
          (1..3).step(1).instance_variables,
          1.step(3).instance_variables,
        ]
        "#,
    );
}

/// An Integer or a Float against a Rational (or anything else that
/// coerces) is for the number's own `<=>` to decide: `(1..2r)` is a valid
/// Range and a mixed Array sorts.
#[test]
fn integers_compare_with_rationals() {
    run_test(
        r#"
        o = Object.new
        def o.coerce(n) = [n, 2]
        [
          (1..2r).to_a, (1.0..2r).include?(1.5), (1..2r).step(1).to_a,
          [3, 2r, 1].sort, [3.5, 2r, 1].sort, [1, 2r].minmax, [2**70, 1r].max,
          1.clamp(0r, 3r), (1 <=> o), (3.0 <=> o), (o <=> 1),
          (begin; [1, nil].sort; rescue ArgumentError => e; e.class; end),
          (begin; (1.."a"); rescue ArgumentError => e; e.message; end),
          (begin; [1, :a].max; rescue ArgumentError => e; e.class; end),
        ]
        "#,
    );
}
