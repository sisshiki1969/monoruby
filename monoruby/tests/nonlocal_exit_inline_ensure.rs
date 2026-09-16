//! A `break` out of a block and a non-local `return` now replay their own
//! frame's open `ensure` bodies inline, ahead of the exit, the way a local
//! `return` always has (`emit_ret` → `gen_all_pending_ensures`).
//!
//! Before, those two exits were the only ones that left the job to
//! `handle_error`: the unwinder found the covering region, deferred the
//! exit, ran the body interpreted, and re-delivered it from `EnsureEnd`.
//! That is what #1185 was spending its effort compiling around — and with
//! the bodies emitted inline there is nothing left to compile around: the
//! exit crosses no region, so the JIT lowers it to the plain specialized
//! teardown and the whole splice/deferral machinery is bypassed.
//!
//! The exception table still covers the exit's pc — the regions are real,
//! and an *exception* raised there still uses them — so bytecodegen
//! extends the replay spans (#1368) over the exit instruction itself.
//! Every lookup that asks "which regions are in force here?" then answers
//! for the exit as it does inside a replayed body: `covering_ensure` finds
//! no ensure to run, `errinfo_restore_slots` no `$!` to restore, and
//! `try_splice_exit` nothing to splice. Without that the bodies run
//! **twice**, which is exactly what the counts below pin down.
extern crate monoruby;
use monoruby::tests::*;

/// The shape the whole thing is for: a `break` inside its own block's
/// `begin`..`ensure`. The body must run once per iteration — the breaking
/// one included, and not twice.
#[test]
fn a_break_runs_its_own_ensure_exactly_once() {
    run_test(
        r#"
        $n = 0
        def f(arr, t)
          arr.each do |x|
            begin
              break x if x == t
            ensure
              $n += 1
            end
          end
        end
        r = []
        40.times { r << f([1, 2, 3], 2) }
        [r.uniq, $n]
        "#,
    );
}

/// The non-local `return` twin.
#[test]
fn a_method_return_runs_its_own_ensure_exactly_once() {
    run_test(
        r#"
        $n = 0
        def g(arr, t)
          arr.each do |x|
            begin
              return x * 10 if x == t
            ensure
              $n += 1
            end
          end
          :fell_through
        end
        r = []
        40.times { r << g([1, 2, 3], 2) }
        [r.uniq, $n]
        "#,
    );
}

/// Nested regions run innermost first, each exactly once.
#[test]
fn nested_regions_run_innermost_first() {
    run_test(
        r#"
        $log = []
        def f(arr, t)
          arr.each do |x|
            begin
              begin
                break x if x == t
              ensure
                $log << :inner
              end
            ensure
              $log << :outer
            end
          end
        end
        r = []
        40.times { $log.clear; r << f([1, 2], 2) }
        [r.uniq, $log]
        "#,
    );
}

/// Exiting from inside a `rescue` clause restores `$!` to the region's
/// entry value — the restore the unwinder used to do, now emitted inline
/// and therefore skipped by `handle_error`. Doing both would restore
/// twice; doing neither would leave the rescued exception in `$!`.
#[test]
fn a_break_out_of_a_rescue_clause_restores_errinfo() {
    run_test(
        r#"
        $seen = []
        def f(arr)
          arr.each do |x|
            begin
              raise "inner#{x}"
            rescue
              break x
            end
          end
        end
        r = []
        40.times do
          begin
            raise "outer"
          rescue
            r << f([7, 8])
            $seen << $!.message
          end
        end
        [r.uniq, $seen.uniq]
        "#,
    );
}

/// An `ensure` body containing its own `break` overrides the exit being
/// replayed (CRuby), and must not replay itself — the truncation protocol
/// in `gen_all_pending_ensures`.
#[test]
fn a_break_inside_the_replayed_ensure_overrides_it() {
    run_test(
        r#"
        $n = 0
        def f(arr)
          arr.each do |x|
            begin
              break :from_body
            ensure
              $n += 1
              break :from_ensure
            end
          end
        end
        r = []
        40.times { r << f([1, 2, 3]) }
        [r.uniq, $n]
        "#,
    );
}

/// An `ensure` body that raises replaces the exit, and the outer region's
/// body still runs.
#[test]
fn a_raising_ensure_replaces_the_exit() {
    run_test(
        r#"
        $log = []
        def f(arr)
          arr.each do |x|
            begin
              begin
                break :never
              ensure
                $log << :inner
                raise "from ensure"
              end
            ensure
              $log << :outer
            end
          end
        end
        r = []
        40.times do
          $log.clear
          begin
            r << f([1, 2])
          rescue => e
            r << e.message
          end
        end
        [r.uniq, $log]
        "#,
    );
}

/// A region with both a `rescue` and an `ensure`, where the exit is taken
/// from the body rather than the clause: the exception edge's own copy of
/// the body is in play too, and only one of the two must run.
#[test]
fn a_region_with_both_rescue_and_ensure() {
    run_test(
        r#"
        $n = 0
        def f(arr, t)
          arr.each do |x|
            begin
              raise "boom" if x == 99
              break x if x == t
            rescue
              :rescued
            ensure
              $n += 1
            end
          end
        end
        r = []
        40.times { r << f([1, 99, 2, 3], 2) }
        [r.uniq, $n]
        "#,
    );
}

/// The exit's value is computed by an expression, so it lives in a temp
/// that the replayed bodies must not take — and the bodies are free to use
/// temps of their own.
#[test]
fn the_exit_value_survives_the_replayed_bodies() {
    run_test(
        r#"
        def f(arr, t)
          arr.each do |x|
            begin
              break [x * 3, x + 1].map { |v| v * 2 } if x == t
            ensure
              [1, 2, 3].map { |v| v + 1 }.sum
            end
          end
        end
        r = []
        40.times { r << f([1, 2, 3], 2) }
        r.uniq
        "#,
    );
}

/// A `break` with no enclosing region at all keeps the plain lowering —
/// nothing is recorded, nothing changes.
#[test]
fn a_break_with_no_region_is_unchanged() {
    run_test(
        r#"
        def f(arr, t)
          arr.each { |x| break x * 5 if x == t }
        end
        r = []
        40.times { r << f([1, 2, 3], 2) }
        [r.uniq, f([1, 2, 3], 9)]
        "#,
    );
}
