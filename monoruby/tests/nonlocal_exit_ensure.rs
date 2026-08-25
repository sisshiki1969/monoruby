//! A non-local exit written inside a protected region must run the
//! `ensure` bodies it escapes through.
//!
//! `break` out of a block and non-local `return` normally unwind through
//! `handle_error`, which walks the frames and runs each `ensure` on the
//! way out. But when the whole chain is specialized-inlined into one JIT
//! unit, the exits lower to `BlockBreakSpecialized` /
//! `MethodRetSpecialized` — a pure machine-level frame teardown
//! (`lea rbp += Σ; leave; ret`) that never enters `handle_error`.
//!
//! The chain check (`check_exception_handler`) covered only the
//! *suspended* frames, each at its in-progress call site. The frame being
//! compiled — the block whose own `begin`..`ensure` contains the exiting
//! instruction — was not in that range, so the specialized teardown was
//! chosen and silently skipped the block's own `ensure` (issue #1179):
//! the break shape below lost exactly one `c += 0.5` (`22975.5` /
//! `151.5` / `299` against CRuby's `23025.0` / `152.0` / `300`), and the
//! return shape lost every post-warmup increment (`$m == 38`, not `300`).
//! `JitContext::in_protected_region` is the missing half of the check.
extern crate monoruby;
use monoruby::tests::*;

/// Issue #1179's shape: a `break` unwinding through an `ensure` that
/// writes an outer local of the enclosing loop-JIT frame. The `ensure`
/// must run exactly once per iteration — the breaking one included.
#[test]
fn a_break_through_an_ensure_writing_an_outer_local() {
    run_test(
        r#"
        $n = 0
        def g
          a = 0.0; c = 2.0
          i = 0
          while i < 300
            [1].each do |x|
              begin
                a += c
                break if i == 200
              ensure
                c += 0.5
                $n += 1
              end
            end
            i += 1
          end
          [a, c, $n]
        end
        g
        "#,
    );
}

/// The ordering hazard behind the generic path: an `ensure` covering the
/// exit *in the raising frame itself* is interpreted by the VM straight
/// from `entry_raise` — before any error side exit has run, i.e. before
/// the chain-deopt walk has materialized anything into slots. The
/// raising frame's own locals must therefore be slot-true at the exit,
/// exactly as `TraceIr::Raise` already ensures via `locals_to_S`. Here
/// `f` lives only in an fpr at the `break`; without the homing the
/// VM-run `ensure` read the frame's nil-filled slot instead
/// (`NilClass can't be coerced into Float`).
#[test]
fn an_ensure_reading_the_raising_frames_own_float() {
    run_test(
        r#"
        def m2
          out = 0.0
          [1].each do |x|
            f = 1.5 * x
            begin
              break if x == 1
            ensure
              out += f
            end
          end
          out
        end
        r = 0
        300.times { r = m2 }
        r
        "#,
    );
}

/// An `ensure` in an *intermediate* frame is safe without that homing:
/// it only runs after that frame's own error side exit has written back
/// and escalated (the chain-deopt walk converts every suspended frame —
/// constant claims included — before the VM interprets the handler).
/// Pinned here so the ordering contract stays observable: `d` is a
/// constant-claimed outer local, and the `return` raises in the inner
/// block while the `ensure` reading `d` sits one frame out.
#[test]
fn an_intermediate_ensure_reading_an_outer_constant() {
    run_test(
        r#"
        $log = []
        def m
          d = 7
          [1].each do |x|
            begin
              [2].each do |y|
                return 5 if y == 2
              end
            ensure
              $log << d
            end
          end
          99
        end
        300.times { m }
        $log.uniq
        "#,
    );
}

/// The `MethodRet` twin: a non-local `return` out of a block, written
/// inside the block's `begin`..`ensure`. Hot enough that the whole
/// chain (method, `each`, block) inlines into one unit, where the
/// specialized teardown used to skip the `ensure` on every post-warmup
/// call.
#[test]
fn a_return_through_an_ensure_in_a_block() {
    run_test(
        r#"
        $m = 0
        def h
          [1].each do |x|
            begin
              return 42 if x == 1
            ensure
              $m += 1
            end
          end
          99
        end
        res = 0
        300.times { res = h }
        [res, $m]
        "#,
    );
}
