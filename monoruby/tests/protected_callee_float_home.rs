//! A third batch of small CRuby incompatibilities.
//!
//! - **A method made `protected` after its call sites were compiled**
//!   (#2183) raises NoMethodError from JIT code too: the compiler decides
//!   a protected call by the class the unit was compiled for, as the
//!   interpreter decides it by the caller's `self`.
//! - **`__callee__` in a method reached through `&:alias`** (#2181) is the
//!   alias, for a Symbol block, its `to_proc` and a `yield` to either; and
//!   the call site a frame was entered from is the instruction that made
//!   the call, not its one-word neighbour.
//! - **A Float local updated in a block compiled into its method** (#2184)
//!   keeps its updates when something under the same call runs a block
//!   outside the unit: a `yield` to the method's own block, a callee
//!   handing a block of its own on.
//!
//! ARGF's write family (#2175) is in `command_line.rs`, since it needs a
//! command line.

use monoruby::tests::*;

#[test]
fn protected_after_the_call_site_was_compiled() {
    run_test_once(
        r##"
        class PtI2183
          def v = 1
        end
        def go_i2183(o) = o.v
        o = PtI2183.new
        50.times { go_i2183(o) }
        class PtI2183; protected :v; end
        r = 3.times.map { (go_i2183(o) rescue $!.class) }

        class BaseI2183; def w = 5; end
        class AI2183 < BaseI2183; def get(o) = o.w; end
        class BI2183 < BaseI2183; end
        x = AI2183.new; y = BI2183.new
        60.times { x.get(y) }
        class BaseI2183; protected :w; end
        r << 3.times.map { x.get(y) }
        r << 3.times.map { [1].map { |e| y.w } rescue :err }
        r
        "##,
    );
}

#[test]
fn protected_calls_between_instances_stay_compiled() {
    run_test(
        r##"
        class QI2183
          def initialize(x) = @x = x
          def ==(o) = x == o.x
          def cmp(o) = [1, 2].map { |e| o.x + e }
          def idx(o) = o[0] + 1
          protected
          def x = @x
          def [](i) = @x + i
        end
        class SubI2183 < QI2183; end
        class OtherI2183; def peek(o) = o.x; end
        a = QI2183.new(1); b = QI2183.new(1)
        r = nil
        60.times { r = [a == b, a.cmp(b), a.idx(b), SubI2183.new(3) == a, a == SubI2183.new(1)] }
        r << 20.times.map { (OtherI2183.new.peek(a) rescue :err) }.uniq
        r
        "##,
    );
}

#[test]
fn callee_through_a_symbol_block() {
    run_test(
        r##"
        class CI2181
          def who = [__method__, __callee__]
          alias who2 who
          def +(o) = 1
        end
        def y1_i2181 = yield(CI2181.new)
        def t_i2181(c, a, b)
          x = a + b
          c.who2
        end
        c = CI2181.new
        pr = :who2.to_proc
        r = nil
        30.times do
          r = [
            [c].map(&:who2), :who2.to_proc.call(c), c.who2, [c].map(&:who),
            y1_i2181(&:who2), y1_i2181(&pr), pr.(c), pr[c], pr.yield(c),
            [c].each_slice(1).map { |a| a.map(&:who2) },
            [c].flat_map(&:who2), [c].select(&:who2).size, [c].sort_by(&:who2).size,
            [c].each_entry.map(&:who2), [c].each_with_index.map { |e, i| e }.map(&:who2),
            t_i2181(c, c, c),
          ]
        end
        r
        "##,
    );
}

#[test]
fn enumerator_names_survive_a_neighbouring_operator() {
    run_test(
        r##"
        [[1, 2].each_slice(1).inspect, [1, 2].each_cons(1).inspect, [3].collect.inspect,
         [3].filter.inspect, [3].find_all.inspect]
        "##,
    );
}

#[test]
fn float_local_updated_around_a_yield_to_the_given_block() {
    run_test(
        r##"
        def m1_i2184(a)
          s = 0.0
          a.each { |e| s += yield(e) }
          s
        end
        def m2_i2184(a)
          s = 0.0
          n = 0
          a.each { |e| [1].each { |f| s += yield(e) * f; n += 1 } }
          [s, n]
        end
        def m3_i2184(a, &blk)
          s = 0.5
          a.each { |e| s += blk.call(e) }
          s
        end
        def m4_i2184(a)
          s = 1.0
          i = 0
          while i < a.size
            a.each { |e| s += yield(e) }
            s += yield(a[i])
            i += 1
          end
          s
        end
        def g_i2184(e) = yield(e)
        def m5_i2184(a, &blk)
          s = 0.0
          a.each { |e| s += g_i2184(e, &blk) }
          s
        end
        def w1_i2184(a, &b) = m1_i2184(a, &b)
        def w2_i2184(a) = m1_i2184(a) { |x| yield(x) }
        def w3_i2184(a)
          t = 1.0
          r = m1_i2184(a) { |x| t += x; yield(x) }
          [r, t]
        end
        pr = proc { |x| x.to_f * 2 }
        r = nil
        40.times do
          r = [
            m1_i2184([1, 2], &pr), m1_i2184([1, 2], &:to_f), m1_i2184([1, 2]) { |x| x * 1.5 },
            m2_i2184([1, 2], &pr), m2_i2184([1, 2], &:to_f),
            m3_i2184([3, 4], &pr), m3_i2184([3, 4], &:to_f),
            m4_i2184([1, 2], &pr), m4_i2184([1, 2], &:to_f),
            m5_i2184([1, 2], &pr),
            w1_i2184([1, 2], &pr), w1_i2184([1, 2], &:to_f),
            w2_i2184([1, 2], &pr), w2_i2184([1, 2], &:to_f),
            w3_i2184([1, 2], &pr), w3_i2184([1, 2], &:to_f),
          ]
        end
        r
        "##,
    );
}

#[test]
fn block_handed_on_still_reaches_the_callers_locals() {
    // The other side of #2184: a block of this unit that does run outside
    // it — handed on as `&b`, or yielded to from a callee's own block —
    // stores where the caller reads.
    run_test(
        r##"
        def g_i2184b; yield; end
        def h2_i2184b(&b) = g_i2184b(&b)
        def h3_i2184b(&b); yield; g_i2184b(&b); end
        def m2_i2184b
          s = 0.0
          h2_i2184b { s += 1.0 }
          h2_i2184b { s += 1.5 }
          s
        end
        def m3_i2184b
          s = 0.5
          h3_i2184b { s += 1.0 }
          s
        end
        class NodeI2184
          def initialize(k) = @kids = k
          def other; @kids.each { |k| yield k }; end
          def traverse
            other { |e| yield e }
            yield self
          end
          def trav2(&b)
            @kids.each { |k| k.trav2(&b) }
            yield self
          end
        end
        def cnt_i2184(m)
          n = 0
          x = 0.0
          m.traverse { |e| n += 1; x += 0.5 }
          m.trav2 { |e| n += 1; x += 0.25 }
          [n, x]
        end
        m = NodeI2184.new([NodeI2184.new([]), NodeI2184.new([NodeI2184.new([])])])
        r = nil
        40.times { r = [m2_i2184b, m3_i2184b, cnt_i2184(m)] }
        r
        "##,
    );
}

#[test]
fn float_local_survives_a_block_handed_on_under_the_call() {
    // The block that leaves the unit is not the one that stores the
    // local: the method's own block passed on as `&blk`, a callee's own
    // literal block, the same with the method inlined into its caller.
    run_test(
        r##"
        def g_i2184c; yield; end
        def g2_i2184c(e) = yield(e)
        def g3_i2184c(e, &b) = g2_i2184c(e, &b)
        def h3_i2184c(&b) = g_i2184c(&b)
        def m8_i2184c(a, &blk)
          s = 0.0
          a.each { |e| s += g2_i2184c(e, &blk) }
          s
        end
        def m9_i2184c(a, &blk)
          s = 0.0
          a.each { |e| s += g3_i2184c(e, &blk) }
          s
        end
        def m11_i2184c(a)
          s = 0.0
          a.each { |e| s += g2_i2184c(e) { |x| yield(x) } }
          s
        end
        def m0_i2184c
          yield 1.0
          h3_i2184c { }
          yield 2.0
        end
        def r3_i2184c
          t = 0.0
          m0_i2184c { |v| t += v }
          t
        end
        def m1_i2184c
          u = 0.0
          yield 1.0
          [1.0].each { |e| u += e; h3_i2184c { } }
          yield u
        end
        def r4_i2184c
          t = 0.0
          m1_i2184c { |v| t += v }
          t
        end
        def r5_i2184c(a)
          t = 0.0
          a.each { |v| t += v; a.each { |w| t += w; h3_i2184c { t += 0.5 } } }
          t
        end
        def run_i2184c(pr)
          [m8_i2184c([1, 2], &pr), m8_i2184c([1, 2], &:to_f), m8_i2184c([1, 2]) { |x| x * 1.5 },
           m9_i2184c([1, 2], &pr), m9_i2184c([1, 2], &:to_f), m9_i2184c([1, 2]) { |x| x * 1.5 },
           m11_i2184c([1, 2], &pr), m11_i2184c([1, 2], &:to_f), m11_i2184c([1, 2]) { |x| x * 2.5 },
           r3_i2184c, r4_i2184c, r5_i2184c([1.0, 2.0])]
        end
        pr = proc { |x| x.to_f * 2 }
        r = nil
        40.times { r = run_i2184c(pr) }
        r
        "##,
    );
}
