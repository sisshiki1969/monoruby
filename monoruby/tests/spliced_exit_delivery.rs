//! A stage-2 splice (#1363) parks a `break` / non-local `return` on the
//! intermediate frame whose `ensure` it crosses, tears down to that frame's
//! call site, runs the body compiled, and its `EnsureEnd` is meant to
//! deliver the exit through the specialized teardown arm.
//!
//! It never did. `finish_ensure_spliced` validated the parked exit against
//! the host frame's own `outer()` / `outermost()` — the relation the
//! *exiting block's* frame satisfies (the stage-1 same-frame splice), and
//! one a method host never does, having no `outer` — so every stage-2
//! delivery took the re-raise: `handle_error`, the chain-deopt walk, the
//! VM, an OSR re-entry. Instrumented over the whole suite, the arm's codes
//! (2 / 3) had never once been returned.
//!
//! The check now happens at the exit, before anything is torn down: the
//! JIT hands `defer_*_at` the LFP of the frame it laid the target out as
//! (the defining frame for a `break`, the home method for a `return`), and
//! the runtime — which resolved the target from the frame's *current*
//! style — refuses the splice unless they agree. A refusal is the generic
//! raise from the exit's own pc, with the frames between exit and host
//! still intact, which matters exactly when the runtime target is one of
//! them (a `define_method` body the static walk passed through). The
//! `EnsureEnd` dispatch then classifies by kind alone.
//!
//! These pin the semantics on both sides of that check. The first two are
//! the shapes that now take the arm; the others are the ones that must
//! refuse, and produce CRuby's answer either way.
extern crate monoruby;
use monoruby::tests::*;

/// The `break` crosses `inner`'s `ensure`: parked on `inner`, delivered
/// into `find`'s call site by the arm. The body runs once per exit and
/// the value arrives intact.
#[test]
fn a_spliced_break_is_delivered_by_the_arm() {
    run_test(
        r#"
        $n = 0
        def inner(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $n += 1
          end
        end
        def outer(arr); inner(arr) { |x| yield x }; end
        def find(arr, t); outer(arr) { |x| break x * 10 if x == t }; end
        r = []
        40.times { |i| r << find([1, 2, 3], i % 3 + 1) }
        [r.uniq.sort, $n]
    "#,
    );
}

/// The non-local `return` twin: parked on `inner`, returns from `find`.
#[test]
fn a_spliced_return_is_delivered_by_the_arm() {
    run_test(
        r#"
        $n = 0
        def inner(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $n += 1
          end
        end
        def outer(arr); inner(arr) { |x| yield x }; end
        def find(arr, t)
          outer(arr) { |x| return x * 10 if x == t }
          :not_found
        end
        r = []
        40.times { |i| r << find([1, 2, 3], i % 4 + 1) }
        [r.uniq.sort_by(&:to_s), $n]
    "#,
    );
}

/// The exit value is a Float held unboxed at the exit: the arm returns it
/// boxed through a frame whose normal returns would have used the raw-f64
/// convention.
#[test]
fn a_spliced_break_delivers_a_float() {
    run_test(
        r#"
        $n = 0
        def inner(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $n += 1
          end
        end
        def outer(arr); inner(arr) { |x| yield x }; end
        def find(arr, t); outer(arr) { |x| break x * 1.5 if x == t }; end
        s = 0.0
        40.times { |i| s += find([1, 2, 3], i % 3 + 1) }
        [s, $n]
    "#,
    );
}

/// A lambda's `break` is a local return from the lambda, so the iteration
/// carries on: the runtime resolves it as a `MethodReturn`, the kind the
/// exit did not splice, and refuses.
#[test]
fn a_lambda_break_through_an_intermediate_ensure_is_local() {
    run_test(
        r#"
        $n = 0
        def inner(arr, &b)
          begin
            arr.each(&b)
          ensure
            $n += 1
          end
          :done
        end
        l = lambda { |x| break x if x == 2; $seen << x }
        $seen = []
        r = []
        40.times { $seen.clear; r << inner([1, 2, 3], &l) }
        [r.uniq, $seen, $n]
    "#,
    );
}

/// A `return` written in a block inside a `define_method` body returns
/// from that body — the proc-method boundary the runtime's target walk
/// stops at. Whether the JIT's static walk agrees or the exit refuses,
/// the answer is CRuby's.
#[test]
fn a_return_inside_a_define_method_body_through_an_intermediate_ensure() {
    run_test(
        r#"
        $n = 0
        def inner(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $n += 1
          end
        end
        class C
          define_method(:find) do |arr, t|
            inner(arr) { |x| return x * 10 if x == t }
            :not_found
          end
          def drive(arr, t)
            [find(arr, t), :after]
          end
        end
        c = C.new
        r = []
        40.times { |i| r << c.drive([1, 2, 3], i % 4 + 1) }
        [r.uniq.sort_by(&:to_s), $n]
    "#,
    );
}

/// Two exits of different kinds spliced into the same region: the arm
/// dispatches on the kind of the exit actually parked.
#[test]
fn break_and_return_spliced_into_the_same_region() {
    run_test(
        r#"
        $n = 0
        def inner(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $n += 1
          end
        end
        def outer(arr); inner(arr) { |x| yield x }; end
        def find(arr, t)
          v = outer(arr) { |x|
            return [:ret, x] if x == t && t.odd?
            break [:brk, x] if x == t
          }
          [:fell, v]
        end
        r = []
        40.times { |i| r << find([1, 2, 3], i % 3 + 1) }
        [r.uniq.sort_by(&:to_s), $n]
    "#,
    );
}
