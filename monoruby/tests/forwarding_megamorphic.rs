extern crate monoruby;
use monoruby::tests::*;

// A `recv.m(...)` forwarding site whose receiver the VM saw with more
// classes than a dispatch covers used to guard the one class it was
// compiled for and deoptimize on every other receiver (#1948). It now
// ends in a generic send, like any other megamorphic site, with the
// forwarded rest / `**kwrest` / block bound by the VM's own binder. The
// cases here drive such a site with enough receiver classes and every
// argument shape the binder has a path for, from compiled callers that
// hand the rest over elided as well as from the interpreter.
//
// The targets are `def` methods on purpose: a `define_method` target with
// the wrong number of arguments is #1961.

/// Receiver classes: more than the call site's polymorphic cache holds.
const CLASSES: usize = 12;

#[test]
fn megamorphic_forward_to_new() {
    run_test_once(&format!(
        r#"
        class FmBase
          attr_reader :got
        end
        classes = {CLASSES}.times.map do |i|
          Class.new(FmBase) do
            define_method(:i) {{ i }}
            case i % 4
            when 0 then def initialize(a = :d, b = :e) = @got = [i, a, b]
            when 1 then def initialize(*r, **k) = @got = [i, r, k]
            when 2 then def initialize(a, k: 1, &blk) = @got = [i, a, k, blk&.call(a)]
            else        def initialize(a, *r, k: 2, **o) = @got = [i, a, r, k, o]
            end
          end
        end
        def fm_new(k, ...) = k.new(...)
        r = []
        40.times do |n|
          classes.each do |k|
            r << fm_new(k, n).got
            r << (fm_new(k, n, 2).got rescue :arity)
            r << (fm_new(k, n, k: 3).got rescue :kw)
            r << (fm_new(k, n) {{ |x| x + 1 }}.got)
            r << (fm_new(k).got rescue :none)
          end
        end
        r.uniq.size
        "#
    ));
}

#[test]
fn megamorphic_forward_to_methods() {
    run_test_once(&format!(
        r#"
        classes = {CLASSES}.times.map do |i|
          Class.new do
            define_method(:i) {{ i }}
            case i % 4
            when 0 then def m(a, b = :b) = [i, a, b]
            when 1 then def m(*r) = [i, r]
            when 2 then def m(a, *r, k: 0, &blk) = [i, a, r, k, blk ? blk.call(a) : nil]
            else        def m(a, **kw) = [i, a, kw]
            end
          end
        end
        objs = classes.map(&:new)
        def fm_call(o, ...) = o.m(...)
        def fm_lead(o, ...) = o.m(:lead, ...)
        def fm_two(o, ...) = [o.m(...), o.m(...)]
        r = []
        40.times do |n|
          objs.each do |o|
            r << fm_call(o, n)
            r << (fm_call(o, n, 2) rescue :arity)
            r << (fm_call(o, n, k: 5) rescue :kw)
            r << fm_call(o, n) {{ |x| x * 2 }}
            r << (fm_lead(o) rescue :lead0)
            r << (fm_lead(o, n) rescue :lead1)
            r << (fm_lead(o, n, k: 7) rescue :leadkw)
            r << (fm_two(o, n) rescue :two)
          end
        end
        r.uniq.size
        "#
    ));
}

#[test]
fn megamorphic_forward_inside_an_inlined_trampoline() {
    // The callers pass plain arguments and literal keywords, so the
    // trampoline is specialized into them with its rest (and keywords)
    // taken over from the caller's window rather than built.
    run_test_once(&format!(
        r#"
        classes = {CLASSES}.times.map do |i|
          Class.new do
            define_method(:i) {{ i }}
            if i.even?
              def m(a, b = 0, k: 1) = [i, a, b, k]
            else
              def m(*r, **kw) = [i, r, kw]
            end
          end
        end
        objs = classes.map(&:new)
        def fm_tramp(o, ...) = o.m(...)
        def fm_pos(o, a) = fm_tramp(o, a, a + 1)
        def fm_kw(o, a) = fm_tramp(o, a, k: a * 2)
        def fm_multi(o, a, f)
          x = f ? fm_tramp(o, a) : fm_tramp(o, a, a)
          [x, fm_tramp(o, a, k: 3)]
        end
        r = []
        60.times do |n|
          objs.each do |o|
            r << fm_pos(o, n) << fm_kw(o, n) << fm_multi(o, n, n.odd?)
          end
        end
        r.uniq.size
        "#
    ));
}

#[test]
fn megamorphic_forward_through_two_trampolines() {
    // The outer trampoline's own frame holds the lazy forwarding marker
    // when it is entered from the interpreter.
    run_test_once(&format!(
        r#"
        classes = {CLASSES}.times.map do |i|
          Class.new do
            define_method(:i) {{ i }}
            def m(a = :none, *r, k: i, &blk) = [i, a, r, k, blk&.call]
          end
        end
        objs = classes.map(&:new)
        def fm_inner(o, ...) = o.m(...)
        def fm_outer(...) = fm_inner(...)
        r = []
        40.times do |n|
          objs.each do |o|
            r << fm_outer(o) << fm_outer(o, n) << fm_outer(o, n, n, k: n)
            r << fm_outer(o, n) {{ :blk }}
            r << o.then {{ |x| fm_outer(x, *[n, n]) }}
            r << send(:fm_outer, o, n, k: 1)
          end
        end
        r.uniq.size
        "#
    ));
}

#[test]
fn megamorphic_forward_to_unresolvable_targets() {
    // A receiver the send cannot resolve (`method_missing`, a private
    // method) is the interpreter's business: the site deoptimizes for it
    // and still answers.
    run_test_once(&format!(
        r#"
        classes = {CLASSES}.times.map do |i|
          Class.new do
            define_method(:i) {{ i }}
            case i % 3
            when 0 then def m(*r, **k) = [i, r, k]
            when 1 then def method_missing(name, *r, **k, &b) = [:mm, i, name, r, k, b&.call]
            else
              def m(*r) = [i, r]
              private :m
            end
          end
        end
        objs = classes.map(&:new)
        def fm_any(o, ...) = o.m(...)
        r = []
        40.times do |n|
          objs.each do |o|
            r << (fm_any(o, n) rescue [:err, $!.class])
            r << (fm_any(o, n, k: 1) {{ :b }} rescue [:err, $!.class])
          end
        end
        r.uniq.size
        "#
    ));
}

#[test]
fn megamorphic_forward_keeps_the_forwarded_block_and_binding() {
    run_test_once(&format!(
        r#"
        classes = {CLASSES}.times.map do |i|
          Class.new do
            define_method(:i) {{ i }}
            if i.even?
              def each_twice(a, &blk) = [blk.call(a), blk.call(a + i)]
            else
              def each_twice(a) = [yield(a), yield(a * 2)]
            end
          end
        end
        objs = classes.map(&:new)
        def fm_each(o, ...) = o.each_twice(...)
        total = 0
        r = []
        40.times do |n|
          objs.each do |o|
            r << fm_each(o, n) {{ |x| total += x; x + 1 }}
          end
        end
        [total, r.uniq.size, r.last]
        "#
    ));
}

#[test]
fn forward_to_method_missing_from_an_inlined_trampoline() {
    // #1962: the trampoline's rest is handed over elided, and the
    // `method_missing` lowering has no argument fill to build it.
    run_test(
        r#"
        class FmMissing
          def method_missing(name, *r, **k, &b) = [name, r, k, b&.call]
          def respond_to_missing?(*) = true
        end
        def fm_mm(o, ...) = o.anything(...)
        def fm_mm_pos(o, n) = fm_mm(o, n, n + 1)
        def fm_mm_kw(o, n) = fm_mm(o, n, k: n)
        def fm_mm_blk(o, n) = fm_mm(o, n) { n * 2 }
        o = FmMissing.new
        r = []
        30.times { |n| r << fm_mm_pos(o, n) << fm_mm_kw(o, n) << fm_mm_blk(o, n) }
        r
        "#,
    );
}
