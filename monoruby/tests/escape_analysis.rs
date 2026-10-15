//! Allocation elision for Array literals used as a receiver
//! (`doc/escape_analysis.md`).
//!
//! The JIT does not allocate `[a, b]` in `[a, b].max`: the literal is an
//! elided object in the abstract state until a use either consumes it
//! (the callee's receiver escape summary says how) or could observe it,
//! at which point it is allocated after all. These tests pin both halves:
//! the consuming uses compute what the builtins compute, and every way the
//! object can escape — a use the summary does not cover, an element slot
//! overwritten before the use, a block boundary, a side exit, a
//! redefinition of the callee — still sees a real, fresh Array.
extern crate monoruby;
use monoruby::tests::*;

#[test]
fn element_ops_on_slot_literals() {
    run_test(
        r#"
        def f(a, b)
          [[a, b].max, [a, b].min, [b, a].max, [a, b].size, [a, b].length,
           [a, b].empty?, [].empty?, [].size, [].max, [a, b].first, [a, b].last,
           [].first, [].last, [a].first, [a, b, a + b].last]
        end
        r = nil
        20.times { |i| r = f(i, 2.5) }
        r + f(1, "x".size)
        "#,
    );
}

#[test]
fn element_ops_result_unused_and_in_conditions() {
    run_test(
        r#"
        def f(a, b)
          [a, b].max
          [a, b].first
          n = 0
          n += 1 if [a, b].empty?
          n += 10 unless [a, b].empty?
          n += 100 if [a, b].first == a
          n += [a, b].max if a > 1
          n
        end
        r = []
        20.times { |i| r << f(i, 3) }
        r
        "#,
    );
}

#[test]
fn transparent_callees_on_literals_of_immediates() {
    run_test(
        r#"
        def f(x)
          [[:a, :b].include?(x), [1, 2, 3].include?(x), [1, 2, 3].sum,
           [1, 2, 3].sum { |v| v * 2 }, [1, 2, 3].count { |v| v > 1 },
           [3, 1, 2].sort, [1, 2, 3].join("-"), [1, 2, 3].minmax,
           [1, 2].take(5), [1, 2, 3].drop(1), [1, 2] + [3], [1, 2, 2] - [2],
           [1, 2, nil].compact, [1, 1, 2].uniq, [1, 2].reverse,
           [1, 2] & [2], [1] | [2], [1, 2].hash == [1, 2].hash,
           [1, nil].any?, [1, nil].all?, [1.5, :s, nil, true].inspect,
           [1, 2, 3].pack("C*"), [1, 2].max, [1, 2].first, [].size]
        end
        r = nil
        20.times { |i| r = f(i % 4) }
        r + f(:a)
        "#,
    );
}

#[test]
fn transparent_results_do_not_share_the_template() {
    // Every transparent callee that returns an Array returns a new one:
    // mutating it must not change what the literal evaluates to next time.
    run_test(
        r#"
        def f
          a = [3, 1, 2].sort; a << 9
          b = [1, 2].take(5); b << 9
          c = [1, 2] + [3]; c << 9
          d = [1, 2, 2].uniq; d << 9
          e = [1, nil].compact; e << 9
          [[3, 1, 2].sort, [1, 2].take(5), [1, 2] + [3], [1, 2, 2].uniq, [1, nil].compact, a, b]
        end
        r = nil
        20.times { r = f }
        r
        "#,
    );
}

#[test]
fn escaping_uses_see_a_real_array() {
    run_test(
        r#"
        def g(a) = a
        def f(a, b)
          x = [a, b]
          y = [a, b].map { |v| v * 2 }
          z = [a, b].tap { |t| t << 3 }
          w = g([a, b])
          v = [a, b].include?(a + 1)
          u = [a, b][1]
          t = [a, b].frozen?
          s = [[a, b].max, [a, b]]
          q = [1, 2].each { }
          q << 5
          [x, y, z, w, v, u, t, s, q, [1, 2]]
        end
        r = nil
        20.times { |i| r = f(i, 7) }
        r
        "#,
    );
}

#[test]
fn literal_returned_and_across_blocks() {
    run_test(
        r#"
        def mk(a, b) = [a, b]
        def mk2 = [1, 2]
        def f(a, b, c)
          x = c ? [a, b] : [b, a]
          y = ([a, b] if c)
          x << 0
          [x, y, mk(a, b), mk2.push(3), mk2]
        end
        r = []
        20.times { |i| r << f(i, 1, i.even?) }
        r.last(3)
        "#,
    );
}

#[test]
fn redefinition_after_compile_allocates_again() {
    // The consuming lowerings are guarded by the class version: once
    // `Array#max` / `#include?` are redefined, the call must see a real,
    // fresh Array each time — which the redefinitions here return and
    // mutate.
    run_test_once(
        r#"
        def f(i)
          v = [1, 2].include?(1)
          v << 3 if v.is_a?(Array)
          w = [i, 2].max
          w << 4 if w.is_a?(Array)
          [v, w]
        end
        res = []
        40.times do |i|
          if i == 30
            class Array
              def include?(x) = self
              def max = self
            end
          end
          res << f(i)
        end
        res.last(12) + [[1, 2].include?(1), [5, 6].max]
        "#,
    );
}

#[test]
fn side_exit_with_an_elided_literal_pending() {
    // A type guard between the literal and its use deopts with the literal
    // still elided; the side exit has to allocate it.
    run_test_once(
        r#"
        def f(a, x)
          [a, 1].max + [:p, :q].include?(x).to_s.size
        end
        res = []
        30.times { |i| res << f(i, :p) }
        res << f(2.5, :q)
        res << f("z".size, nil)
        res
        "#,
    );
}

#[test]
fn incomparable_elements_raise() {
    run_test_error(
        r#"
        def f(a, b) = [a, b].max
        20.times { f(1, 2) }
        f(1, "a")
        "#,
    );
}

#[test]
fn refined_callee_is_not_elided() {
    // A refinement in scope resolves the call elsewhere; the receiver
    // escape summary of the builtin no longer applies.
    run_test_once(
        r#"
        module R
          refine Array do
            def max = self
            def include?(x) = self
          end
        end
        using R
        def f(a, b)
          x = [a, b].max
          x << 0
          y = [1, 2].include?(1)
          y << 3
          [x, y, [1, 2]]
        end
        r = nil
        30.times { |i| r = f(i, 1) }
        r
        "#,
    );
}
