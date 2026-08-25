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
