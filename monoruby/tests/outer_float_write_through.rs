//! Write-through keep: an outer frame's `Sf` float view survives a block
//! that *writes* it (outer-F roadmap, stage 1').
//!
//! At a block-passing call every unboxed local is boxed into its slot and
//! lands on `Sf` — slot current, register view kept (see
//! `tests/float_across_block_call.rs`). A store *into* that slot from the
//! inlined block used to widen the binding to `S`, so the owner re-read
//! and re-unboxed the float after every call. Now such a store refreshes
//! the view's raw-f64 home too — the owner's in-progress call-site FP
//! save slot for a pool-resident fpr, the owner's own spill slot for a
//! spilled one (`AsmInst::StoreOuterFprHomeF`, chain-addressed, resolved
//! against the recorded save layout) — and keeps the binding: the owner's
//! `fpr_restore_cont` reloads the current value and its continuation uses
//! the float with no guard and no unbox.
//!
//! The boxed slot store stays authoritative on every path, which is what
//! makes the keep path-insensitive; a deopt anywhere under the call
//! abandons the owner's compiled continuation (unconditional side-exit
//! escalation), so only fully-compiled paths ever consume the view. The
//! floats below accumulate over hundreds of iterations, so any staleness
//! or a wrong home displacement diverges from the CRuby oracle loudly.
extern crate monoruby;
use monoruby::tests::*;

/// The motivating shape: a loop-carried float written by the block every
/// iteration. The owner's `s += a` consumes the kept view right after
/// each call.
#[test]
fn a_block_writing_a_kept_float_every_iteration() {
    run_test(
        r#"
        def call_block
          yield
        end
        def f(n)
          a = n * 1.5
          s = 0.0
          i = 0
          while i < 300
            call_block { a *= 1.001 }
            s += a
            i += 1
          end
          [s, a]
        end
        f(3)
        "#,
    );
}

/// A type flip on one path: the block writes an `Integer` at one
/// iteration, which must widen the binding (only Float-typed stores
/// refresh the raw home).
#[test]
fn a_type_flip_widens_the_kept_view() {
    run_test(
        r#"
        def t1
          a = 1.5
          s = 0.0
          i = 0
          while i < 300
            [1].each { |x| if i == 150 then a = 7 else a = a.to_f * 1.001 end }
            s += a.to_f
            i += 1
          end
          [s.round(6), a]
        end
        t1
        "#,
    );
}

/// Two levels of nesting: the inner block writes a local two frames out.
/// The chain part of the home displacement crosses both frames.
#[test]
fn a_write_through_two_outer_levels() {
    run_test(
        r#"
        def t2
          a = 2.0
          s = 0.0
          i = 0
          while i < 300
            [1].each { [2].each { a *= 1.001 } }
            s += a
            i += 1
          end
          [s.round(6), a.round(6)]
        end
        t2
        "#,
    );
}

/// Interaction with the spliced non-local exits (#1185): the `ensure`
/// writes the kept slot and a `break` unwinds through it mid-loop.
#[test]
fn a_kept_float_across_a_spliced_break() {
    run_test(
        r#"
        def t4
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
          [a, c]
        end
        t4
        "#,
    );
}

/// Reads mixed with writes inside the block: the slot store stays
/// authoritative, so the block's own re-reads see its writes.
#[test]
fn reads_after_writes_inside_the_block() {
    run_test(
        r#"
        def t3
          a = 1.0
          [5].each { a += 2.5; a *= 2.0 }
          a + 1.0
        end
        r = 0
        300.times { r = t3 }
        r
        "#,
    );
}
