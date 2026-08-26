//! A deferred unwind must not outlive its frame (issue #1186).
//!
//! When a non-local exit passes through an `ensure`, `handle_error` parks
//! the error in `Executor::deferred_unwind`, keyed by the frame's `Lfp`,
//! and the region's `EnsureEnd` re-raises it. But a *local* exit written
//! inside the `ensure` handler — a block `next` (a plain `Ret`), a
//! `return`-in-`ensure` — overrides the deferred unwind (CRuby semantics)
//! and returns the frame normally, never reaching `EnsureEnd`. Nothing
//! popped the entry: it stayed parked under a stack address that the next
//! frame of the right depth recycles, whose `EnsureEnd` then delivered a
//! *stale* unwind — a spurious `LocalJumpError` at best, and at worst a
//! silently wrong value (the `probe` below returned the leaked method's
//! `9`).
//!
//! The fix ties the deferral to its frame: the `Ret` paths discard a
//! parked deferral owned by the returning frame — a one-load gate against
//! the `Executor::deferred_top_lfp` mirror in the VM's `Ret` opcode, and
//! the same gate in the JIT epilogue of handler-carrying iseqs (reachable
//! compiled via an OSR'd loop inside an `ensure` handler).

extern crate monoruby;
use monoruby::tests::*;

/// The filed shape: `next` inside an `ensure` entered for a deferred
/// `break` overrides the break (a3 == 23 on both), and the deferral must
/// not leak into the same-shaped frame that runs right after (a4 raised
/// `break from proc-closure` before the fix).
#[test]
fn a_next_inside_an_ensure_must_not_leak_the_deferred_break() {
    run_test(
        r#"
        def a3
          s = 0
          [1, 2].each do |i|
            begin
              s += i
              break if i == 2
            ensure
              s += 10
              next
            end
          end
          s
        end
        def a4
          v = [1, 2, 3].each do |i|
            begin
              break i * 100 if i == 2
            ensure
              $keep = i
            end
          end
          [v, $keep]
        end
        r = []
        300.times { r[0] = a3; r[1] = a4 }
        r
        "#,
    );
}

/// The `return`-in-`ensure` twin, with a same-depth probe so the recycled
/// address is exact: before the fix `probe2` returned the leaked `9`
/// instead of `3` — a silently wrong value, not even an exception.
#[test]
fn a_return_in_an_ensure_must_not_leak_the_deferred_return() {
    run_test(
        r#"
        def leaky
          [1].each { return 9 }
        ensure
          return 7
        end
        def probe2
          r = 0
          begin
            r = 1
          ensure
            r += 1
          end
          r + 1
        end
        res = []
        300.times { res[0] = leaky; res[1] = probe2 }
        res
        "#,
    );
}

/// The compiled-`Ret` gate: a hot `while` inside the `ensure` handler
/// OSRs, and the local `return` executes as compiled code while the
/// deferral is parked — the JIT epilogue of a handler-carrying iseq
/// carries the same discard gate as the VM's `Ret`.
#[test]
fn a_compiled_return_inside_an_osr_loop_in_an_ensure() {
    run_test(
        r#"
        $flag = true
        def osr
          [1].each { return 5 }
        ensure
          i = 0; s = 0
          while i < 300
            s += i
            return s if $flag && i == 250
            i += 1
          end
          s
        end
        def probe
          v = [1, 2, 3].each do |i|
            begin
              break i * 100 if i == 2
            ensure
              $keep = i
            end
          end
          [v, $keep]
        end
        r = []
        30.times { r[0] = osr; r[1] = probe }
        r
        "#,
    );
}
