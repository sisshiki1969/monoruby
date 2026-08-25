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

/// The JIT divergence found while building the shapes above, filed as
/// issue #1179 and since fixed — this is its regression test (the
/// `#[ignore]` this carried while open is dropped). **Not** the sp-undo
/// defect: it reproduced identically before and after that fix, at both
/// FPR pool sizes, and `--no-jit` agreed with CRuby.
///
/// The record above attributed the lost `c += 0.5` to a stale view (the
/// #1172/#1173 family); probes said otherwise — the breaking iteration's
/// `ensure` never ran at all. The `break` inside the block's own
/// `begin`..`ensure` still lowered to `BlockBreakSpecialized`, a pure
/// machine-level teardown that never enters `handle_error`, because the
/// specialization check covered only the chain's *suspended* frames, not
/// the exiting instruction's own coverage. See
/// `JitContext::in_protected_region` and `tests/nonlocal_exit_ensure.rs`.
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
