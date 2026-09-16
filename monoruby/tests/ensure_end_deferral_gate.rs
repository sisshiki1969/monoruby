//! The compiled `EnsureEnd` asks one question — "is a deferred unwind
//! parked for this frame?" — and on every path that reaches *compiled*
//! code the answer is no, so the question is now behind a one-word gate
//! instead of a runtime call (the same mirror `emit_ret` tests, #1186).
//!
//! Why the answer is always no. `Executor::defer_unwind` has five call
//! sites. Three are in `handle_error`, each immediately followed by
//! `ErrorReturn::goto(ensure)`, which resumes the **VM** — so the body and
//! its `EnsureEnd` run interpreted from there. The other two are the
//! stage-1/stage-2 splice helpers (#1185), which pair with
//! `ensure_end_spliced`, a different call this gate does not cover. And an
//! exception cannot arrive either: bytecodegen compiles a *separate* copy
//! of the body for the exception edge, ending in `raise`, so
//!
//! ```text
//! [(:00002..:00004, rescue=:00005, ensure=:00008, err_slot=%4)]
//!   BB1  :00005 $n = 1 / :00007 raise %4     <- the exception edge's copy
//!   BB2  :00008 $n = 1 / :00010 ensure_end   <- the normal edge's copy
//! ```
//!
//! an exception goes to BB1 and never reaches BB2's `EnsureEnd` at all.
//!
//! The gate is conservative rather than an elision: when the mirror does
//! name this frame it runs exactly the old sequence. These tests pin the
//! semantics that would break if it were wrong — every kind of unwind that
//! parks a deferral must still run the body exactly once — including the
//! one shape where compiled code demonstrably *does* run with a deferral
//! parked: a loop inside an `ensure` body entered by an unwind, hot enough
//! to be re-entered by OSR (the scenario `emit_ret`'s own gate exists for).
extern crate monoruby;
use monoruby::tests::*;

/// A non-local `return` crossing an `ensure`: `handle_error` parks a
/// deferral and hands the body to the VM.
#[test]
fn a_return_through_an_ensure_still_runs_it_once() {
    run_test(
        r#"
        $log = []
        def mid
          begin
            yield
            :not_reached
          ensure
            $log << :ran
          end
        end
        def home(t)
          mid { return t * 2 }
          :not_reached
        end
        r = []
        40.times { |i| r << home(i) }
        [r.last, $log.size]
        "#,
    );
}

/// The `break` twin.
#[test]
fn a_break_through_an_ensure_still_runs_it_once() {
    run_test(
        r#"
        $log = []
        def mid(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $log << :ran
          end
          :fell_through
        end
        r = []
        40.times { r << (mid([1, 2, 3]) { |x| break x if x == 2 }) }
        [r.uniq, $log.size]
        "#,
    );
}

/// A real exception: it takes the *other* copy of the body (the one
/// ending in `raise`), so the compiled `EnsureEnd` is not on its path at
/// all — and the body must still run.
#[test]
fn an_exception_through_an_ensure_still_runs_it_once() {
    run_test(
        r#"
        $log = []
        def mid
          begin
            raise "boom"
          ensure
            $log << :ran
          end
        end
        r = []
        40.times do
          begin
            mid
          rescue => e
            r << e.message
          end
        end
        [r.uniq, $log.size]
        "#,
    );
}

/// `throw`/`catch` is a third unwind kind with its own `defer_unwind` arm.
#[test]
fn a_throw_through_an_ensure_still_runs_it_once() {
    run_test(
        r#"
        $log = []
        def mid
          begin
            throw :done, 7
          ensure
            $log << :ran
          end
        end
        r = []
        40.times { r << catch(:done) { mid } }
        [r.uniq, $log.size]
        "#,
    );
}

/// The shape the gate's taken path exists for: the unwind hands the
/// `ensure` body to the VM with a deferral parked, and the body contains a
/// loop hot enough to be re-entered by OSR — so compiled code runs while
/// the deferral is live. The deferred `return` must still be delivered
/// after the body finishes.
#[test]
fn a_hot_loop_inside_an_ensure_entered_by_an_unwind() {
    run_test(
        r#"
        $acc = 0
        def mid
          begin
            yield
            :not_reached
          ensure
            i = 0
            while i < 200
              $acc += 1
              i += 1
            end
          end
        end
        def home(t)
          mid { return t * 2 }
          :not_reached
        end
        r = []
        30.times { |i| r << home(i) }
        [r.last, $acc]
        "#,
    );
}

/// `retry` and `redo` reach `handle_error`'s goto arm too; an `ensure`
/// around them must not be double-run or skipped.
#[test]
fn retry_inside_a_region_with_an_ensure() {
    run_test(
        r#"
        $log = []
        def mid
          n = 0
          begin
            begin
              n += 1
              raise "again" if n < 3
              n
            rescue
              retry
            end
          ensure
            $log << n
          end
        end
        r = []
        40.times { $log.clear; r << mid }
        [r.uniq, $log]
        "#,
    );
}

/// An `ensure` whose body itself returns locally overrides the deferred
/// unwind (CRuby), and the parked deferral must be discarded rather than
/// misfire on whatever later reuses the frame's stack address (#1186).
#[test]
fn a_return_inside_the_ensure_overrides_the_deferred_one() {
    run_test(
        r#"
        def mid
          begin
            yield
            :not_reached
          ensure
            return :from_ensure
          end
        end
        def home
          mid { return :from_block }
          :not_reached
        end
        r = []
        40.times { r << home }
        r.uniq
        "#,
    );
}
