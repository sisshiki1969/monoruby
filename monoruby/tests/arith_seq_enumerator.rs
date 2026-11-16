//! `Enumerator::ArithmeticSequence` is a subclass of `Enumerator` (#2004).
//!
//! What asks the class rather than the object — `Enumerator === seq`,
//! `case seq when Enumerator`, `Enumerator.instance_method(:x).bind(seq)`
//! — sees an Enumerator, and the methods the sequence does not define
//! itself are Enumerator's own (`next`, `with_index`, `rewind`, `+`, …)
//! rather than forwarders. As in CRuby they iterate by calling the
//! method the sequence came from on its receiver, which is why a
//! beginless sequence fails with `Range#step`'s own complaint.

use monoruby::tests::*;

const HELPER: &str = r#"
  t = ->(&b) { begin; b.call; rescue Exception => e; [e.class.to_s, e.message]; end }
  as = Enumerator::ArithmeticSequence
  em = ->(name) { Enumerator.instance_method(name).bind(1.step(3)) }
"#;

fn probe(exprs: &[&str]) {
    let list = exprs
        .iter()
        .map(|s| format!("t.() {{ {s} }}"))
        .collect::<Vec<_>>()
        .join(",\n");
    run_test_once(&format!("{HELPER}\n[{list}]"));
}

/// The issue's table.
#[test]
fn class_hierarchy() {
    probe(&[
        "as.ancestors.take(3)",
        "as.superclass",
        "(..3).step(1).each_with_index.first(1)",
        "1.step.first(3)",
        "[Enumerator === 1.step(3), 1.step(3).is_a?(Enumerator), 1.step(3).kind_of?(Enumerable)]",
        "[as === 1.step(3), 1.step(3).instance_of?(as), 1.step(3).class]",
        "case 1.step(3) when Enumerator then :enum else :other end",
        "case (1..3).step(1) when Enumerator then :enum else :other end",
        "case ((1..3) % 1) when Enumerator then :enum else :other end",
        "as.new",
        "as.allocate",
        "[as.instance_methods(false).sort - [:to_a], as < Enumerator, as <= Enumerable]",
        "1.step(3).respond_to?(:lazy)",
        "1.step(3).frozen?",
        "Marshal.dump(1.step(3))",
    ]);
}

/// Enumerator's own methods, bound to a sequence.
#[test]
fn enumerator_methods_bind() {
    probe(&[
        "em.(:next).call",
        "em.(:size).call",
        "em.(:inspect).call",
        "em.(:each).call.class",
        "r = []; em.(:each).call { |x| r << x }; r",
        "em.(:with_index).call.to_a",
        "em.(:with_object).call([]) { |x, m| m << x }",
        "em.(:peek).call",
        "em.(:rewind).call.class",
        "em.(:feed).call(1)",
        "em.(:next_values).call",
        "em.(:peek_values).call",
        "em.(:+).call([9]).to_a",
        "em.(:each_with_index).call.to_a",
        "em.(:each_with_object).call([]) { |x, m| m << x }",
    ]);
}

/// Who owns what.
#[test]
fn method_owners() {
    probe(&[
        "%i[next peek next_values peek_values feed rewind with_index with_object each_with_index each_with_object +].map { 1.step(3).method(_1).owner }",
        "%i[map select each_slice each_entry lazy zip include? min sum entries].map { 1.step(3).method(_1).owner }",
        "%i[each first last size begin end step exclude_end? == === eql? hash inspect].map { 1.step(3).method(_1).owner }",
        "%i[is_a? kind_of? to_s length].map { 1.step(3).method(_1).owner rescue $!.class }",
        "1.step(3).to_s.sub(/0x\\h+/, \"XX\")",
        "1.step(3).length",
        "1.step(3)[0]",
        "[1].each.length",
        "[1.step(3) === 1.step(3), 1.step(3) === (1..3).step(1), 1.step(3) === 2]",
    ]);
}

/// External iteration.
#[test]
fn external_iteration() {
    probe(&[
        "e = 1.step(3); [e.next, e.peek, e.next, e.next, (e.next rescue $!.class), e.rewind.equal?(e), e.next]",
        "e = (1..3).step(1); [e.next, e.next, e.next, (e.next rescue $!.class), (e.peek rescue $!.class)]",
        "e = 1.0.step(2.0, 0.5); [e.next, e.next_values, e.peek_values, e.next, (e.next rescue $!.message)]",
        "e = 1.step; [e.next, e.next, e.rewind.class, e.next]",
        "e = 1.step(3); e.next; e2 = e.dup; [(e2.next rescue $!.class), e.next]",
        "e = 3.step(1, -1); [e.next, e.next, e.next, (e.next rescue $!.class)]",
        "e = 1.step(0); [(e.next rescue $!.class), (e.peek rescue $!.class), e.rewind.equal?(e)]",
        "e = 1.step(3); a = e.next; e.rewind; [a, e.next, e.next]",
    ]);
}

/// `with_index` / `with_object` / `each_with_*`, with and without a block.
#[test]
fn with_index_and_object() {
    probe(&[
        "1.step(3).with_index.to_a",
        "1.step(3).with_index(2).to_a",
        "1.step(3).with_index(2) { |x, i| }",
        "(1..3).step(1).with_index(2) { |x, i| }",
        "((1..3) % 1).with_index(2) { |x, i| }",
        "1.step(3).each_with_index.to_a",
        "1.step(3).each_with_index { |x, i| }",
        "1.step(3).each_with_index(1) { |x, i| }",
        "1.step(3).each_with_object([]) { |x, m| m << x }",
        "1.step(3).with_object([]) { |x, m| m << x }",
        "1.step(3).with_object([]).inspect",
        "1.step(3).with_index.inspect",
        "1.step(3).with_index(1).inspect",
        "1.step(3).each_with_index.inspect",
        "1.step(3).each_with_object([]).inspect",
        "1.step(by: 2, to: 7).with_index.to_a",
        "1.step(by: 2, to: 7).with_index(1) { }",
        "1.step(by: 2, to: 7).with_index(1).to_a",
        "1.step(by: 2, to: 7).each_with_index.inspect",
        "1.0.step(2.0, 0.5).with_index.to_a",
        "1.step(3).each_with_index.with_index(1).to_a",
        "e = 1.step(3).with_index; [e.size, e.next, e.inspect]",
        "e = 1.step(3).each_with_index; [e.size, e.next, e.inspect]",
        "e = 1.step(3).with_object(:a); [e.size, e.next, e.inspect]",
        "a = []; 1.step(7, 3).with_index(10) { |x, i| a << [x, i] }; a",
        "a = []; (1..7).step(3).with_object(:o) { |x, o| a << [x, o] }; a",
    ]);
}

/// The rest of Enumerable and Enumerator, through the sequence.
#[test]
fn enumerable_through_the_sequence() {
    probe(&[
        "1.step(7, 2).each_slice(2).to_a",
        "1.step(7, 2).each_with_index.map { |x, i| x * i }",
        "1.step(7, 2).lazy.map { |x| x * 2 }.first(2)",
        "1.step(Float::INFINITY, 2).lazy.map { |x| x * 2 }.first(2)",
        "(1.step(3) + [9]).to_a",
        "(1.step(3) + [9]).inspect",
        "1.step(3).dup.to_a",
        "1.step(3).clone.to_a",
        "1.step(3).size",
        "1.step(3).each.inspect",
        "1.step(3).each.equal?(1.step(3))",
        "e = 1.step(3); e.each.equal?(e)",
        "1.step(3).each(1)",
        "1.step(3).each_entry.to_a",
        "1.step(3).entries",
        "1.step(3).zip(4.step(6))",
        "[1.step(3).sum, 1.step(3).min, 1.step(3).max, 1.step(3).include?(2), 1.step(3).count]",
        "1.step(3).map.with_index(1).to_a",
        "1.step(3).select.with_index { |x, i| i > 0 }",
        "1.step(10, 3).each_cons(2).to_a",
        "1.step(10, 3).inject(:+)",
        "1.step(10, 3).reverse_each.to_a",
        "[1.step(by: 2, to: 7), 1.step(by: 2), 1.step(to: 7), 1.step(7, by: 2), 1.0.step(by: 0.5, to: 2)].map(&:inspect)",
        "[1.step(by: 2, to: 7).to_a, 1.step(by: 2).first(2), 1.step(to: 3).to_a, 1.step(7, by: 2).to_a]",
    ]);
}

/// A beginless sequence: every route ends in `Range#step`'s error.
#[test]
fn beginless_sequence() {
    probe(&[
        "(..3).step(1).with_index.first(1)",
        "(..3).step(1).each { break }",
        "(..3).step(1).first(1)",
        "(..3).step(1).next",
        "(..3).step(1).size",
        "(..3).step(1).with_index { }",
        "(..3).step(1).with_object(1) { }",
        "(..3).step(1).each_slice(2).first(1)",
        "(..3).step(1).each_entry.first(1)",
        "(..3).step(1).to_a",
        "(..3).step(1).take(1)",
        "(..3).step(1).lazy.first(1)",
        "(..3).step(1).inspect",
        "[(..3).step(1).begin, (..3).step(1).end, (..3).step(1).step, (..3).step(1).last]",
    ]);
}

/// Iterating a sequence in compiled code is unchanged.
#[test]
fn compiled_iteration() {
    run_test(
        r#"
        def f(n)
          s = 0
          1.step(n, 2) { |x| s += x }
          1.step(n, 2).each { |x| s += x }
          (1..n).step(3).each { |x| s += x }
          1.step(n, 2).with_index { |x, i| s += x * i }
          1.step(n, 2).each_with_index { |x, i| s += x * i }
          s + 1.step(n, 2).map { _1 * 2 }.sum + 1.step(n).to_a.size
        end
        r = []
        30.times { |i| r << f(i + 5) }
        r
        "#,
    );
}

/// `Array#[]` with a sequence still slices by its terms.
#[test]
fn array_slice_by_sequence() {
    run_test(
        r#"
        a = [10, 20, 30, 40, 50]
        [a[(0..).step(2)], a[(1..3).step(1)], a[(4..0).step(-1)], a.slice((0..3).step(2)),
         a[(0..-1) % 2], (a[(0..9).step(2)] rescue $!.message), a.values_at(0, 2)]
        "#,
    );
}
