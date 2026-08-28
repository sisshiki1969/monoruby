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

/// From here down: stage 2 — `S -> Sf` *promotion*. A float store from
/// the inlined block into an outer slot the owner never held as a float
/// allocates a fresh spill home in the owner's file and promotes the
/// binding, when the store dominates the whole call subtree (the store
/// in its frame's entry block, every intermediate frame suspended in its
/// own entry block — `JitContext::try_promote_outer_sf`).
///
/// First touch through a direct-yield chain: the owner consumes the
/// promoted view with no guard and no unbox.
#[test]
fn a_first_touch_promotion_through_a_direct_yield() {
    run_test(
        r#"
        def call_block
          yield
        end
        def p1(n)
          x = nil
          call_block { x = n * 1.5 }
          s = 0.0
          300.times { s += x }
          [s, x]
        end
        p1(3)
        "#,
    );
}

/// Zero-trip refusal: `[].each` never runs the block, and `Array#each`'s
/// `yield` sits inside its `while` — not the entry block — so the
/// promotion is refused and the nil flows through untouched.
#[test]
fn a_zero_trip_block_refuses_promotion() {
    run_test(
        r#"
        def p2
          x = nil
          [].each { x = 9.9 }
          x.nil?
        end
        r = nil
        300.times { r = p2 }
        r
        "#,
    );
}

/// Promotion then keep: the first store promotes, the loop's stores hit
/// the stage-1' refresh path on the same home.
#[test]
fn a_promotion_followed_by_kept_refreshes() {
    run_test(
        r#"
        def call_block
          yield
        end
        def p4(n)
          x = nil
          call_block { x = n * 1.5 }
          i = 0; s = 0.0
          while i < 300
            call_block { x *= 1.001 }
            s += x
            i += 1
          end
          [s.round(6), x.round(6)]
        end
        p4(2)
        "#,
    );
}

/// Two dominating levels, with the stored value const-folded (`n * 0.5`
/// under a monomorphic `n` compiles to `C`), exercising the
/// immediate-form home store.
#[test]
fn a_two_level_promotion_of_a_folded_constant() {
    run_test(
        r#"
        def hop
          yield
        end
        def p5(n)
          x = nil
          hop { hop { x = n * 0.5 } }
          x * 4.0
        end
        r = 0
        300.times { r = p5(7) }
        r
        "#,
    );
}

/// A store guarded by a condition inside the block lands in a later
/// basic block — refused, and the both-paths shape stays correct.
#[test]
fn a_conditional_store_refuses_promotion() {
    run_test(
        r#"
        def call_block
          yield
        end
        def p6(n)
          x = nil
          call_block { x = n * 2.5 if n > 1 }
          x.nil? ? -1.0 : x + 1.0
        end
        r = []
        300.times { r = [p6(3), p6(0)] }
        r
        "#,
    );
}

/// From here down: stage 3a — home-directed *reads*. A block's read of
/// an outer Float-guarded `Sf` slot loads the raw f64 straight from its
/// home (the owner's call-site save slot or spill slot) into a fresh `F`
/// of the reading frame (`AsmInst::LoadOuterFprHomeF`): no slot load, no
/// Float guard, no unbox. The read relies only on the binding at its own
/// program point, so it records nothing and needs no drain.
///
/// The full round trip: the block reads and writes the kept float every
/// iteration; with home reads and write-through keeps the loop's floats
/// stay unboxed except the one authoritative slot store per write.
#[test]
fn a_block_reading_and_writing_the_kept_float() {
    run_test(
        r#"
        def call_block
          yield
        end
        def q1(n)
          a = n * 1.5
          s = 0.0
          i = 0
          while i < 300
            call_block { a = a * 1.001 + 0.5 }
            s += a
            i += 1
          end
          [s.round(6), a.round(6)]
        end
        q1(3)
        "#,
    );
}

/// A later block home-reads a view the *promotion* (stage 2) created —
/// the owner never unboxed x itself, yet both blocks work on raw f64s.
#[test]
fn a_home_read_of_a_promoted_view() {
    run_test(
        r#"
        def call_block
          yield
        end
        def q2(n)
          x = nil
          call_block { x = n * 2.5 }
          t = 0.0
          call_block { t = x + 1.0 }
          [x, t]
        end
        r = nil
        300.times { r = q2(4) }
        r
        "#,
    );
}

/// The block consumes the float as a *Value* (`to_s`): the home read
/// defines an `F` in the block's frame, and the ordinary F-boxing
/// machinery takes it from there.
#[test]
fn a_home_read_consumed_as_a_value() {
    run_test(
        r#"
        def call_block
          yield
        end
        def q3(n)
          a = n * 1.5
          s = ""
          call_block { s = a.to_s }
          [a, s]
        end
        r = nil
        300.times { r = q3(2) }
        r
        "#,
    );
}

/// After a type flip widens the binding, reads degrade to the slot path
/// and stay correct.
#[test]
fn a_home_read_degrades_after_a_type_flip() {
    run_test(
        r#"
        def call_block
          yield
        end
        def q4
          a = 1.5
          v = nil
          i = 0
          while i < 300
            call_block { a = 7 if i == 150; v = a.to_f + 0.25 }
            i += 1
          end
          [a, v]
        end
        q4
        "#,
    );
}
