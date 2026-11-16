//! Which operand `<=>` is sent to, and in which order the elements are
//! compared (#2007).
//!
//! `Array#min` / `#max` keep a running answer and make it the receiver
//! (`ary_max_generic`: `optimized_cmp(result, v)`), so `a = [1, nil];
//! a.max` asks `1 <=> nil` and reports "comparison of Integer with nil
//! failed". An array literal with a non-constant element is different:
//! the compiler turns `[x, y].max` into `opt_newarray_send`, whose loop
//! is `optimized_cmp(v, result)` — as is `Enumerable#max`'s. `sort` hands
//! its comparator `(a, b)` in the array's order, `sort_by` / `min_by` /
//! `max_by` compare keys the same way, and a block that answers `nil`
//! fails in `rb_cmpint` with the two *elements* in the message.

use monoruby::tests::*;

const HELPER: &str = r#"
  t = ->(&b) { begin; b.call; rescue Exception => e; [e.class.to_s, e.message]; end }
  $log = []
  class Rec
    attr_reader :n
    def initialize(n) = @n = n
    def <=>(o) = ($log << [n, o.n]; n <=> o.n)
    def inspect = "R#{n}"
  end
  r = ->(*a) { a.map { Rec.new(_1) } }
  l = ->(&b) { $log.clear; [b.call.inspect, $log.dup] }
"#;

fn probe(exprs: &[&str]) {
    let list = exprs
        .iter()
        .map(|s| format!("t.() {{ {s} }}"))
        .collect::<Vec<_>>()
        .join(",\n");
    run_test_once(&format!("{HELPER}\n[{list}]"));
}

/// The issue's examples and their neighbours: the failure message names
/// the receiver of the `<=>` that answered nil.
#[test]
fn comparison_failure_names_the_receiver() {
    probe(&[
        "[1, nil].max",
        "[nil, 1].max",
        "[1, \"a\"].sort",
        "[1.0, nil].min",
        "[1, \"a\"].min",
        "[\"a\", 1].min",
        "[1, nil].minmax",
        "[nil, 1].minmax",
        "[1, 2, nil].max",
        "[1, 2, nil].min",
        "[1, 2, nil].sort",
        "[nil, 2, 1].sort",
        "[3, 1, \"a\", 2].sort",
        "[1, :a].sort!",
        "[:a, 1].sort!",
        "[[1, nil], [1, 2]].sort",
        "[[1, nil], [1, 2]].max",
        "1.clamp(nil, \"a\")",
    ]);
}

/// `[x, y].max` with a non-constant element is `opt_newarray_send`: the
/// new element is the receiver. A literal of constants is a shared array
/// and takes `Array#max`, where the running answer is.
#[test]
fn array_literal_min_max() {
    probe(&[
        "x = 1; y = nil; [x, y].max",
        "x = 1; y = nil; [y, x].max",
        "x = 1; y = nil; [x, y].min",
        "x = 1; y = nil; [y, x].min",
        "x = 1; [x, \"a\"].min",
        "x = 1; [\"a\", x].max",
        "x = 1; a = [x, nil]; a.max",
        "x = 1; a = [x, nil]; a.min",
        "x = 2; [x, 1, 3].max",
        "x = 2; [x, 1, 3].min",
        "x = 2.5; [x, 1, 3].max",
        "x = \"b\"; [x, \"a\", \"c\"].max",
        "x = \"b\"; [x, \"a\", \"c\"].min",
        "l.() { a, b, c = r.(2, 1, 3); [a, b, c].max }",
        "l.() { a, b, c = r.(2, 1, 3); [a, b, c].min }",
        "l.() { a, b, c = r.(2, 1, 3); x = [a, b, c]; x.max }",
        "l.() { a, b, c = r.(2, 1, 3); x = [a, b, c]; x.min }",
    ]);
}

/// The same, compiled: the loop makes the JIT lower `[a, b].max`.
#[test]
fn array_literal_min_max_compiled() {
    run_test(
        r#"
        def mx(a, b) = [a, b].max
        def mn(a, b) = [a, b].min
        def fmx(a, b) = [a, b, 1.5].max
        res = []
        30.times do |i|
          res << mx(i, 3) << mn(i, 3) << fmx(i, 2.5) << mx(i.to_f, 3) << mn("a", "b")
        end
        res << (mx(1, nil) rescue $!.message) << (mx(nil, 1) rescue $!.message)
        res << (mn(1, "a") rescue $!.message) << (mn("a", 1) rescue $!.message)
        res
        "#,
    );
}

/// Through `Enumerable` (an Enumerator, a Range, a Hash, a Set).
#[test]
fn enumerable_comparison_failure() {
    probe(&[
        "[1, nil].each.max",
        "[1, nil].each.min",
        "[1, nil].each.minmax",
        "[1, nil].each.sort",
        "[1, nil].sort_by { |x| x }",
        "[1, nil].min_by { |x| x }",
        "[1, nil].max_by { |x| x }",
        "[1, nil].each.min_by { |x| x }",
        "[1, nil].each.max_by { |x| x }",
        "[1, nil].minmax_by { |x| x }",
        "{a: 1, b: nil}.max_by { |k, v| v }",
        "{a: 1, b: nil}.sort_by { |k, v| v }",
        "{a: 1, \"b\" => 2}.sort",
        "{a: 1, \"b\" => 2}.min",
        "Set[1, nil].max",
        "Set[1, nil].sort",
    ]);
}

/// A comparison block that answers nil: `rb_cmpint` reports the two
/// elements it was comparing.
#[test]
fn block_returning_nil() {
    probe(&[
        "(1..3).max { |a, b| nil }",
        "[1, 2].max { |a, b| nil }",
        "[1, 2].min { |a, b| nil }",
        "[1, 2].sort { |a, b| nil }",
        "[1, 2].minmax { |a, b| nil }",
        "[1, 2].each.max { |a, b| nil }",
        "[1, 2].each.sort { |a, b| nil }",
        "{a: 1, b: 2}.max { |a, b| nil }",
    ]);
}

/// The order of the `<=>` calls themselves.
#[test]
fn comparison_sequence() {
    probe(&[
        "l.() { r.(1, 2).max }",
        "l.() { r.(1, 2).min }",
        "l.() { r.(1, 2).minmax }",
        "l.() { r.(1, 2).sort }",
        "l.() { r.(2, 1, 3).max }",
        "l.() { r.(2, 1, 3).min }",
        "l.() { r.(2, 1, 3).minmax }",
        "l.() { r.(2, 1, 3).sort }",
        "l.() { r.(2, 1, 3).each.max }",
        "l.() { r.(2, 1, 3).each.min }",
        "l.() { r.(2, 1, 3).each.minmax }",
        "l.() { r.(2, 1, 3).each.sort }",
        "l.() { r.(2, 1, 3).sort_by { _1 } }",
        "l.() { r.(2, 1, 3).min_by { _1 } }",
        "l.() { r.(2, 1, 3).max_by { _1 } }",
        "l.() { r.(2, 1, 3).minmax_by { _1 } }",
        "a = []; [[3, 1, 2].sort { |x, y| a << [x, y]; x <=> y }, a]",
        "a = []; [[3, 1, 2].max { |x, y| a << [x, y]; x <=> y }, a]",
        "a = []; [[3, 1, 2].min { |x, y| a << [x, y]; x <=> y }, a]",
        "a = []; [[3, 1, 2].minmax { |x, y| a << [x, y]; x <=> y }, a]",
        "a = []; [[3, 1, 2].each.max { |x, y| a << [x, y]; x <=> y }, a]",
        "a = []; [(1..3).min { |x, y| a << [x, y]; x <=> y }, a]",
    ]);
}

/// What the reordering must not change: the answers.
#[test]
fn results_unchanged() {
    run_test(
        r#"
        a = [5, 3, 9, 1, 7, 3]
        f = [2.5, -1.0, 8.25, 0.0]
        s = %w[pear apple fig]
        h = {b: 2, a: 1, c: 3}
        [
          a.min, a.max, a.minmax, a.sort, a.sort { |x, y| y <=> x },
          a.min(2), a.max(2), a.min { |x, y| y <=> x }, a.max { |x, y| y <=> x },
          a.sort_by { -_1 }, a.min_by { -_1 }, a.max_by { -_1 }, a.minmax_by { -_1 },
          f.min, f.max, f.minmax, f.sort, s.min, s.max, s.minmax, s.sort,
          h.sort, h.min, h.max, h.sort_by { |k, v| -v }, h.min_by { |k, v| v },
          (1..5).min, (1..5).max, (1..5).minmax, (1..5).sort { |x, y| y <=> x },
          (1..5).min { |x, y| y <=> x }, (1..5).max(2), a.each.sort, a.each.min,
          a.each.max, a.each.minmax, a.each_slice(2).min, a.each_slice(2).sort,
          [[2, "b"], [1, "z"], [2, "a"]].sort, [[2, "b"], [1, "z"], [2, "a"]].max,
          [1, 2.0, 3r].max, [1, 2.0, 3r].min, [1, 2.0, 3r].sort, [].max, [].min, [].minmax,
        ]
        "#,
    );
}
