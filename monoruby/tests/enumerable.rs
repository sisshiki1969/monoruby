use monoruby::tests::*;

#[test]
fn inject() {
    run_test(r##"[2, 3, 4, 5].inject {|result, item| result + item }"##);
    run_test(r##"(1..5).inject {|result, item| result + item }"##);
    run_test(r##"[2, 3, 4, 5].inject(0) {|result, item| result + item ** 2 }"##);
    run_test(r##"(1..5).inject(0) {|result, item| result + item ** 2 }"##);
}

#[test]
fn filter() {
    run_test(r##"[1, 2, 3, 4, 5, 6, 7].filter { |x| x.odd? }"##);
    run_test(r##"(1..7).filter { |x| x.odd? }"##);
    run_test(r##"(1..7).filter(&:odd?)"##);
}

#[test]
fn filter_map() {
    run_test(r##"(1..10).filter_map { |i| i * 2 if i.even? }"##);
    run_test(r##"[*(1..10)].filter_map { |i| i * 2 if i.even? }"##);
}

#[test]
fn take_while() {
    run_test(r##"(1..10).take_while { |i| i < 5 }"##);
    run_test(r##"[*(1..10)].take_while { |i| i < 5 }"##);
}

#[test]
fn blockless_returns_an_enumerator() {
    // `flat_map` / `collect_concat` / `partition` on Array, and
    // `map` / `collect` / `flat_map` / `collect_concat` on Range, went
    // straight to `expect_block` and raised LocalJumpError instead of
    // returning an Enumerator (#1496).
    run_tests(&[
        r##"
        [
          [1, 2, 3].flat_map, [1, 2, 3].collect_concat, [1, 2, 3].partition,
          (1..3).map, (1..3).collect, (1..3).flat_map, (1..3).collect_concat,
        ].map { |e| [e.class, e.size] }
        "##,
        // The Enumerator each one builds really replays the method.
        r##"
        [
          [1, 2, 3].flat_map.each { |x| [x, x] },
          [1, 2, 3].collect_concat.each { |x| [x] },
          [1, 2, 3].partition.each(&:odd?),
          (1..3).map.each { |x| x * 2 },
          (1..3).flat_map.each { |x| [x, -x] },
        ]
        "##,
        // …and `with_index` / `to_a` over it behave like CRuby's.
        r##"
        [(1..3).map.with_index { |x, i| [x, i] }, [1, 2].partition.to_a]
        "##,
    ]);
}

#[test]
fn range_map_beyond_integer_endpoints() {
    // `Range#map` / `#flat_map` had an Integer-only fast path and raised
    // `RuntimeError: not supported` for every other shape (#1496); they
    // now fall through to `Enumerable`, which walks `Range#each`.
    run_tests(&[
        r##"[("a".."e").map { |s| s * 2 }, ("a".."c").flat_map { |s| [s, s] }]"##,
        r##"[(:a..:c).map(&:to_s), (:a..:c).collect_concat { |s| [s] }]"##,
        // Endless, and a `break` out of it.
        r##"(1..).map { |x| break x if x > 3 }"##,
        // Empty / reversed / excluded-end shapes.
        r##"[(3..1).map { |x| x }, (1...1).map { |x| x }, ("a"..."c").map { |s| s }]"##,
        // The endpoints CRuby refuses to iterate from.
        r##"begin; (1.0..3.0).map { |x| x }; rescue TypeError => e; e.message; end"##,
        r##"begin; (..3).map { |x| x }; rescue TypeError => e; e.message; end"##,
        // A type with only `succ` and `<=>` still walks.
        r##"
        class Tick
          include Comparable
          attr_reader :n
          def initialize(n) = @n = n
          def succ = Tick.new(@n + 1)
          def <=>(o) = n <=> o.n
        end
        (Tick.new(1)..Tick.new(4)).map(&:n)
        "##,
        // The Integer fast path is still the one that runs, and agrees.
        r##"[(1..4).map { |x| x * x }, (1...4).map { |x| x * x }, (1..4).flat_map { |x| [x] }]"##,
    ]);
}

#[test]
fn enumerable_find_records_only_the_given_argument() {
    // `Enumerable#find(ifnone = nil)` replayed `to_enum(:find, ifnone)`
    // unconditionally, so an omitted `ifnone` showed up as an explicit
    // `nil` in the Enumerator (#1496).
    run_tests(&[
        r##"[(1..3).find.inspect, (1..3).find(nil).inspect, {a: 1}.find.inspect]"##,
        // The argument still does its job.
        r##"[(1..3).find { |x| x > 2 }, (1..3).find(proc { :none }) { |x| x > 9 }, (1..3).find { |x| x > 9 }]"##,
        // Arity is still checked.
        r##"begin; (1..3).find(1, 2) { |x| x }; rescue ArgumentError => e; e.message; end"##,
    ]);
}
