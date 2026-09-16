//! An exit that leaves a `begin..ensure` region replays the body inline
//! ahead of itself (`gen_all_pending_ensures` /
//! `gen_loop_pending_ensures`), and that copy sits lexically *inside* the
//! very region it is replaying. So when the copy itself raised, the
//! exception table handed the raise straight back to the region whose body
//! was running, and the body ran a second time:
//!
//! ```ruby
//! begin
//!   begin
//!     return
//!   ensure
//!     $log << :inner   # logged twice, CRuby logs it once
//!     raise "E"
//!   end
//! ensure
//!   $log << :outer
//! end
//! ```
//!
//! Every exit that replays is affected — local `return`, loop `break` /
//! `next`, block `break` — and the second run is observable well beyond a
//! duplicated log line: it re-runs whatever the body does, and a body that
//! `return`s or assigns would do it twice.
//!
//! The fix tags each region with an id, records each replayed copy as a
//! span naming the regions it runs outside of, and has every "which
//! regions are in force at this pc" lookup skip them
//! (`ISeqInfo::active_entries`). A region written *inside* an `ensure`
//! body is a region of its own, so it keeps catching normally — the last
//! test here is the one that fails if the spans are tracked by nesting
//! depth instead of by region.
extern crate monoruby;
use monoruby::tests::*;

/// The shape above: the innermost body must run once, then the raise
/// propagates outward through the enclosing `ensure`.
#[test]
fn a_raising_ensure_replayed_for_a_local_return_runs_once() {
    run_test(
        r#"
        $log = []
        def f
          begin
            begin
              return :never
            ensure
              $log << :inner
              raise "E"
            end
          ensure
            $log << :outer
          end
        end
        begin
          f
        rescue => e
          [e.message, $log]
        end
    "#,
    );
}

/// Same, for a `break` out of a `while` loop — `gen_loop_pending_ensures`
/// replays the body just as `emit_ret` does.
#[test]
fn a_raising_ensure_replayed_for_a_loop_break_runs_once() {
    run_test(
        r#"
        $log = []
        def f
          while true
            begin
              begin
                break :never
              ensure
                $log << :inner
                raise "E"
              end
            ensure
              $log << :outer
            end
          end
        end
        begin
          f
        rescue => e
          [e.message, $log]
        end
    "#,
    );
}

/// `next` replays the same way, and does it once per iteration.
#[test]
fn a_raising_ensure_replayed_for_a_loop_next_runs_once() {
    run_test(
        r#"
        $log = []
        def f
          i = 0
          while i < 3
            i += 1
            begin
              begin
                next
              ensure
                $log << [:inner, i]
                raise "E" if i == 2
              end
            ensure
              $log << [:outer, i]
            end
          end
          :done
        end
        begin
          f
        rescue => e
          [e.message, $log]
        end
    "#,
    );
}

/// A block `break` leaves the block frame the same way.
#[test]
fn a_raising_ensure_replayed_for_a_block_break_runs_once() {
    run_test(
        r#"
        $log = []
        def f
          [1, 2, 3].each do |x|
            begin
              begin
                break :never
              ensure
                $log << :inner
                raise "E"
              end
            ensure
              $log << :outer
            end
          end
        end
        begin
          f
        rescue => e
          [e.message, $log]
        end
    "#,
    );
}

/// Three levels: only the regions the exit has already left are switched
/// off, so the raise still climbs one level at a time.
#[test]
fn three_nested_regions_each_run_their_body_once() {
    run_test(
        r#"
        $log = []
        def f
          begin
            begin
              begin
                return :never
              ensure
                $log << :i
                raise "E"
              end
            ensure
              $log << :m
            end
          ensure
            $log << :o
          end
        end
        begin
          f
        rescue => e
          [e.message, $log]
        end
    "#,
    );
}

/// The raise is caught by a `rescue` of an enclosing region in the same
/// frame — the body it came from must not run again on the way there.
#[test]
fn a_raising_ensure_caught_by_an_enclosing_rescue_runs_once() {
    run_test(
        r#"
        $log = []
        def f
          begin
            begin
              return :never
            ensure
              $log << :inner
              raise "E"
            end
          rescue => e
            $log << :rescued
            "caught:#{e.message}"
          end
        end
        [f, $log]
    "#,
    );
}

/// The exit leaves a `rescue` clause, so the replay also carries the `$!`
/// restore — the body still runs once.
#[test]
fn a_raising_ensure_replayed_out_of_a_rescue_clause_runs_once() {
    run_test(
        r#"
        $log = []
        def f
          begin
            begin
              raise "first"
            rescue
              return :never
            ensure
              $log << :inner
              raise "E"
            end
          ensure
            $log << :outer
          end
        end
        begin
          f
        rescue => e
          [e.message, $log]
        end
    "#,
    );
}

/// Both bodies raise: the outer exception wins and each body ran once.
#[test]
fn two_raising_ensures_each_run_once() {
    run_test(
        r#"
        $log = []
        def f
          begin
            begin
              return :never
            ensure
              $log << :inner
              raise "E1"
            end
          ensure
            $log << :outer
            raise "E2"
          end
        end
        begin
          f
        rescue => e
          [e.message, $log]
        end
    "#,
    );
}

/// A `begin..rescue` written *inside* an `ensure` body is a region of its
/// own: the replayed copy runs outside the region it belongs to, but that
/// nested region still catches, and the `return` completes.
#[test]
fn a_region_inside_a_replayed_body_still_catches() {
    run_test(
        r#"
        $log = []
        def f
          begin
            begin
              return :done
            ensure
              $log << :inner
              begin
                raise "E"
              rescue => e
                $log << "caught:#{e.message}"
              end
            end
          ensure
            $log << :outer
          end
        end
        [f, $log]
    "#,
    );
}

/// The same nested region, but inside a body replayed by a loop `break`.
#[test]
fn a_region_inside_a_body_replayed_by_break_still_catches() {
    run_test(
        r#"
        $log = []
        def f
          while true
            begin
              break :done
            ensure
              $log << :inner
              begin
                raise "E"
              rescue => e
                $log << "caught:#{e.message}"
              end
            end
          end
        end
        [f, $log]
    "#,
    );
}
