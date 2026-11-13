//! Loops whose counter starts as an Integer literal: `i = 0; while ...;
//! i += 1; end`.
//!
//! The back-edge fixpoint analyses such a loop from a head where the
//! counter is already an `Integer` rather than the literal (see
//! `doc/fixpoint_engine_plan.md` §5.8), and analyses it again from the
//! literal when the counter does not come round as one. Each shape below
//! is one way a "counter" can turn out not to be what the seed assumed.

extern crate monoruby;
use monoruby::tests::*;

#[test]
fn counters_stepped_at_the_back_edge() {
    run_test(
        r##"
        def up(n); i = 0; s = 0; while i < n; s += i; i += 1; end; [i, s]; end
        def down(n); i = n; j = 10; while j > 0; i += j; j -= 1; end; [i, j]; end
        def by(n, k); i = 0; c = 0; while i < n; c += 1; i += k; end; [i, c]; end
        def rev(n); i = 0; while i < n; i = 2 + i; end; i; end
        def two(n); i = 0; j = 100; while i < j; i += 1; j -= 1; end; [i, j, n]; end
        def until_(n); i = 0; until i >= n; i += 3; end; i; end
        def post(n); i = 0; begin; i += 1; end while i < n; i; end
        r = nil
        40.times { r = [up(30), down(5), by(30, 7), rev(31), two(0), until_(20), post(20), post(0)] }
        r
        "##,
    );
}

#[test]
fn nested_loops_and_next() {
    run_test(
        r##"
        def grid(n, m)
          s = 0
          i = 0
          while i < n
            j = 0
            while j < m
              s += i * j
              j += 1
            end
            i += 1
          end
          [i, s]
        end
        def skip(n)
          i = 0
          s = 0
          while i < n
            if i.odd?
              i += 1
              next
            end
            s += i
            i += 1
          end
          [i, s]
        end
        def tri(n)
          c = 0
          i = 0
          while i < n
            j = i
            while j < n
              k = 0
              while k < 2
                c += 1
                k += 1
              end
              j += 1
            end
            i += 1
          end
          c
        end
        r = nil
        40.times { r = [grid(12, 9), skip(31), tri(9), grid(0, 5), grid(5, 0)] }
        r
        "##,
    );
}

/// The step is there, but the counter is a constant at the head all the
/// same, or comes round as something other than an Integer.
#[test]
fn counters_that_do_not_come_round_as_integers() {
    run_test(
        r##"
        # Stepped, then reset: a constant at the back edge.
        def reset(n); i = 0; c = 0; while c < n; i += 1; c += i; i = 0; end; [i, c]; end
        # Stepped only on the way out.
        def leave(n); i = 0; c = 0; while true; c += 1; if c > n; i += 1; break; end; end; [i, c]; end
        # A zero step.
        def zero(n); i = 0; c = 0; while c < n; c += 1; i += 0; end; [i, c]; end
        # A step the constant itself rules out.
        def never(n); i = 0; c = 0; while c < n; c += 1; next if i == 0; i += 1; end; [i, c]; end
        # Stepped as an Integer, then overwritten by something else.
        def other(n); i = 0; c = 0; while c < n; c += 1; i += 1; i = i.to_s; i = i.size; end; [i, c]; end
        def strs(n); i = 0; c = 0; a = nil; while c < n; c += 1; i += 1; a = i; i = "x" * i; i = i.size; end; [i, a, c]; end
        # The loop never comes round.
        def once(n); i = 0; while i < n; i += 1; return i; end; -1; end
        r = nil
        40.times { r = [reset(20), leave(20), zero(20), never(20), other(20), strs(9), once(3), once(0)] }
        r
        "##,
    );
}

/// The counter leaves the Integer range, or the step stops being an
/// Integer after the loop was compiled.
#[test]
fn counters_that_change_class() {
    run_test(
        r##"
        def grow(n); i = 1; c = 0; while c < n; i += i; c += 1; end; i; end
        def step(n, k); i = 0; c = 0; while c < n; i += k; c += 1; end; i; end
        r = []
        40.times { r = [grow(20), step(20, 3)] }
        r << grow(70) << grow(130)
        r << step(20, 0.5) << step(20, 2r) << step(3, 2**70) << step(20, 3)
        r
        "##,
    );
}

/// A counter that is a constant on entry to an inner loop only on the
/// first trip, and a loop entered with the counter already typed.
#[test]
fn counters_set_before_and_inside_enclosing_loops() {
    run_test(
        r##"
        def carry(n)
          s = 0
          j = 0
          i = 0
          while i < n
            while j < i
              s += j
              j += 1
            end
            i += 1
          end
          [i, j, s]
        end
        def from(a, n); i = a; s = 0; while i < n; s += i; i += 1; end; s; end
        def blocks(n)
          s = 0
          n.times do |x|
            i = 0
            while i < x
              s += i
              i += 1
            end
          end
          s
        end
        r = nil
        40.times { r = [carry(15), from(3, 30), from(40, 30), blocks(12)] }
        r
        "##,
    );
}
