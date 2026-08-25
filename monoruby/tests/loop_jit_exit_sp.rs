//! The two `entry_raise` exits that used to skip the loop-JIT sp undo.
//!
//! Every aarch64 exit that leaves a loop-JIT body for the VM must release
//! the spill region the entry pinned `sp` below (`a64_undo_loop_rsp_bump`)
//! — the aarch64 VM builds callee frames sp-relative, so the leftover
//! region stays between the VM and its own local area otherwise. `Raise` /
//! `Retry` / `Redo` / `EnsureEnd` always did; `MethodRet` and `BlockBreak`
//! did not even take the byte count.
//!
//! Reaching those two lowerings takes several conditions at once, which
//! these shapes were built to meet (verified by probe):
//!
//! - the exit sits in a **loop-JIT (OSR) unit** — the `while` loops trip
//!   the loop threshold inside a single call;
//! - the **non-specialized** `MethodRet` / `BlockBreak` is lowered — a
//!   `return` inside a block is a non-local method return whose home
//!   frame is outside the OSR unit (`method_caller_specialized_ids` has
//!   no chain), and an exception handler between the frames does the
//!   same for `break` (`iter_caller_specialized_ids` refuses a chain
//!   that crosses one);
//! - for the `MethodRet` shape, the unit also has a non-empty **spill
//!   region** under `stress-spill-pool` (the floats are locals of the
//!   frame the loop belongs to, so they live in FPRs and spill at pool
//!   2), and the `ensure` makes `handle_error` find a handler, so
//!   `entry_raise` takes its `goto` path — the one that resumes the VM
//!   without restoring `sp` from `x29`.
//!
//! Measured on aarch64 before the fix: the VM resumed with `sp` still at
//! the pinned depth — 32 bytes below its own local area on the `MethodRet`
//! shape — and with it after. That is the *safe* direction (deeper, so
//! nothing is overwritten) and no misbehavior was ever observed; the undo
//! closes the inconsistency and these tests pin the reaching shapes down.
extern crate monoruby;
use monoruby::tests::*;

/// Non-specialized `MethodRet` out of a spilling loop-JIT unit: the
/// `return` is inside a block, so it is a non-local method return, and the
/// OSR unit rooted at the block has no chain to the home method frame.
/// The `ensure` gives the unwind a handler to land in.
#[test]
fn method_ret_out_of_a_spilling_loop_jit_unit() {
    run_test(
        r#"
        def f
          [1].each do |x|
            a = 0.5; b = 1.5; c = 2.5; d = 3.5
            i = 0
            begin
              while i < 300
                a += b * c * d
                b += 0.25
                c += 0.125
                return a + b + c + d if i == 250
                i += 1
              end
            ensure
              a += 0.0625
            end
          end
          0.0
        end
        f
        "#,
    );
}

/// Non-specialized `BlockBreak` inside a loop-JIT unit: the block is
/// inlined into the OSR unit of `g`'s `while`, and the `begin`/`ensure`
/// inside it forces the non-specialized lowering.
///
/// The `ensure` deliberately writes only a *block-local* (`t`): an
/// `ensure` that writes an outer local (`c += 0.5`) trips a separate,
/// pre-existing JIT divergence — see
/// [`break_running_an_ensure_that_writes_an_outer_local`].
#[test]
fn block_break_out_of_a_loop_jit_unit() {
    run_test(
        r#"
        def g
          a = 0.0; b = 1.0; c = 2.0; d = 3.0
          i = 0
          while i < 300
            [1, 2, 3].each do |x|
              t = 0.0
              begin
                a += b * c * d * x
                break if i == 200
              ensure
                t += 0.5
              end
            end
            i += 1
          end
          a
        end
        g
        "#,
    );
}

/// Issue #1179's original shape. The first analysis ("the ensure's write
/// happens in the VM but the loop unit reads a stale view") was wrong:
/// counting the `ensure` executions showed the ensure **never ran** for a
/// post-OSR `break`. The break site sat inside a begin/ensure, but
/// `iter_caller_specialized_ids` only checked the *suspended* frames of
/// the chain for handler regions — never the frame the `break` itself is
/// in — so the loop unit compiled the exit as `BlockBreakSpecialized`, a
/// static teardown that consults no exception table. Every `break` taken
/// from the compiled loop skipped its `ensure` (measured on a
/// break-every-iteration variant: 99 of 300 ensures ran — exactly the
/// pre-OSR interpreted ones). Fixed by `pc_in_handler_region`: a
/// non-local exit inside a handler region refuses the specialized
/// lowering and goes through `err_block_break` -> `handle_error`, which
/// runs the ensure.
#[test]
fn break_running_an_ensure_that_writes_an_outer_local() {
    run_test(
        r#"
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
              end
            end
            i += 1
          end
          a
        end
        g
        "#,
    );
}

/// The sharpest form of issue #1179: `break` on *every* iteration, with
/// the `ensure` counting its own executions through a global. Before the
/// fix only the pre-OSR (interpreted) iterations ran their ensure — 99 of
/// 300 — because every `break` compiled into the loop unit took the
/// specialized static teardown past the exception table.
#[test]
fn every_break_runs_its_ensure() {
    run_test(
        r#"
        $n = 0
        def g
          i = 0
          while i < 300
            [1].each do |x|
              begin
                break if true
              ensure
                $n += 1
              end
            end
            i += 1
          end
          $n
        end
        g
        "#,
    );
}

/// The integer variant: rules out any float/register-file involvement —
/// integer locals are always slot-resident (the GP pool is empty), and
/// the deficit was identical, which is what disproved the stale-view
/// analysis and pointed at the ensure never running.
#[test]
fn break_ensure_writing_an_outer_integer() {
    run_test(
        r#"
        def g
          a = 0; c = 2
          i = 0
          while i < 300
            [1].each do |x|
              begin
                a += c
                break if i == 200
              ensure
                c += 1
              end
            end
            i += 1
          end
          [a, c]
        end
        g
        "#,
    );
}
