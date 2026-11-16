extern crate monoruby;
use monoruby::tests::*;

// Issue #1996: `Range#===` and `Range#include?` on a linear range are
// CRuby's `r_cover_p` — the value-only test. Only `Range#cover?` has the
// range-in-range rule (`r_cover_range_p`), so `(1..10) === (1..5)` is
// false while `(1..10).cover?(1..5)` is true. A range of objects without
// `succ` raises from `include?` ("can't iterate from"), one whose
// endpoints answer `to_int` is tested like a numeric one.
#[test]
fn range_eqq_and_include_test_a_value() {
    run_test2(
        r##"
        # The rows are collected instead of printed: the harness compares
        # the value of the whole script.
        $o = []
        def row(*a) = a.each { |x| $o << x.to_s }
        def rowp(*a) = a.each { |x| $o << x.inspect }
        def t; yield; rescue Exception => e; "#{e.class}: #{e.message.gsub(/0x\h+/, "0x")}"; end
        rowp (1..10).include?(1..5)
        rowp (1..10).member?(2..3)
        rowp (1..10).===(1..5)
        rowp ((1..10) === (1..5))
        rowp (1.0..10.0).include?(1..5)
        rowp (1..10).include?(1...5)
        rowp ('a'..'z') === ('b'..'c')
        rowp (1..).include?(1..2)
        rowp (case 1..5 when 1..10 then :hit else :miss end)
        class R2 < Range; def cover?(v) = :mine; end
        r = R2.new(1, 10)
        rowp r === "x", r.include?("x")
        class NS; include Comparable; attr_reader :v; def initialize(v) = @v = v; def <=>(o) = o.is_a?(NS) ? v <=> o.v : nil; end
        ns = (NS.new(1)..NS.new(5))
        rowp t { ns.include?(NS.new(3)) }
        rowp t { ns === NS.new(3) }
        rowp t { ns.cover?(NS.new(3)) }
        class TI; include Comparable; attr_reader :v; def initialize(v) = @v = v; def <=>(o) = o.is_a?(TI) ? v <=> o.v : nil; def to_int = v; end
        ti = (TI.new(1)..TI.new(5))
        rowp t { ti.include?(TI.new(3)) }
        rowp t { (Time.at(0)..Time.at(10)).include?(Time.at(0)..Time.at(1)) }
        rowp t { (1..10).cover?(1..5) }
        rowp t { ((1..3)..(1..5)) rescue :bad }
        rowp t { (nil..nil) === (1..2) }
        rowp t { (nil..nil).include?(1..2) }
        $o
        "##,
    );
}

// More endpoint kinds: `to_int` (also through `method_missing`), nil, Time.
#[test]
fn range_include_endpoints() {
    run_test2(
        r##"
        # The rows are collected instead of printed: the harness compares
        # the value of the whole script.
        $o = []
        def row(*a) = a.each { |x| $o << x.to_s }
        def rowp(*a) = a.each { |x| $o << x.inspect }
        def t; r = yield; r.inspect; rescue Exception => e; "#{e.class}: #{e.message}"; end
        class TI; include Comparable; attr_reader :n; def initialize(n) = @n = n; def to_int = @n; def <=>(o) = o.is_a?(TI) ? @n <=> o.n : nil; end
        class MI; include Comparable; attr_reader :n; def initialize(n) = @n = n; def method_missing(m, *a) = m == :to_int ? @n : super; def <=>(o) = o.is_a?(MI) ? @n <=> o.n : nil; end
        class NS; include Comparable; attr_reader :n; def initialize(n) = @n = n; def <=>(o) = o.is_a?(NS) ? @n <=> o.n : nil; end
        rowp t { (nil..nil).include?(1) }
        rowp t { (nil..nil).include?("a") }
        rowp t { (nil..nil) === 1 }
        rowp t { (nil.."z").include?("a") }
        rowp t { ("a"..nil).include?("b") }
        rowp t { (TI.new(1)..TI.new(5)).include?(TI.new(3)) }
        rowp t { (TI.new(1)..TI.new(5)).include?(3) }
        rowp t { (TI.new(1)..nil).include?(TI.new(3)) }
        rowp t { (nil..TI.new(5)).include?(TI.new(3)) }
        rowp t { (MI.new(1)..MI.new(5)).include?(MI.new(3)) }
        rowp t { (MI.new(1)..MI.new(5)).include?(MI.new(7)) }
        rowp t { (NS.new(1)..NS.new(5)).include?(NS.new(3)) }
        rowp t { (NS.new(1)..NS.new(5)) === NS.new(3) }
        rowp t { (NS.new(1)..NS.new(5)).cover?(NS.new(3)) }
        rowp t { (NS.new(1)..NS.new(5)).member?(NS.new(3)) }
        rowp t { (1..5) === (2..3) }
        rowp t { (1..5).include?(2..3) }
        rowp t { (1..5).cover?(2..3) }
        rowp t { ("a".."z") === ("b".."c") }
        rowp t { ("a".."z").cover?("b".."c") }
        rowp t { (Time.at(0)..Time.at(10)) === Time.at(5) }
        rowp t { (Time.at(0)..Time.at(10)).include?(Time.at(5)) }
        rowp t { (Time.at(0)..Time.at(10)) === (Time.at(1)..Time.at(2)) }
        rowp t { (1.0..2.0) === (1.2..1.5) }
        rowp t { ((1..2)..(3..4)) === (2..3) }
        rowp t { (1..) === (2..) }
        rowp t { (..5) === (..3) }
        rowp t { case 2..3 when 1..5 then :in else :out end }
        $o
        "##,
    );
}

// The same through the JIT: `===` in a `case`, with a value and with a
// Range on the left.
#[test]
fn range_eqq_and_include_jit() {
    run_test(
        "r = (1..10); [r === 5, r === (2..3), r === 11, r === 2.5, (case 3 when r then 1 else 2 end)]",
    );
    run_test(
        "r = (1..10); [r.include?(5), r.include?(2..3), r.member?(11), r.cover?(2..3), r.cover?(0..3)]",
    );
    run_test(
        r#"r = ("a".."z"); [r === "c", r === ("b".."c"), r.include?("cc"), r.cover?("cc"), r.cover?("b".."c")]"#,
    );
    run_test(
        "r = (1.0..2.0); [r === 1.5, r === (1.2..1.5), r.include?(1.2..1.5), r.cover?(1.2..1.5)]",
    );
}
