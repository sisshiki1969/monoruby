//! A block handed out of the unit from inside a loop of a specialized
//! callee, writing a local of the suspended caller.
//!
//! The loop's back-edge fixpoint runs on a throwaway clone of the
//! context. A hand-out in the loop body (`unbox_to_S_for_outgoing_block`)
//! widens the caller's slot in two places: the live chain, and the parked
//! copy the caller resumes from after the specialized call. The chain's
//! widening survived into the back-edge join the real compile starts the
//! loop from, so by the time the real compile reached the hand-out the
//! slot was already `S` and nothing was reported again — while the
//! parked copy, widened only on the clone, still said the caller's
//! register held the value. The caller resumed on that register, and
//! every store the block made was lost: `a *= 1.001` three hundred times
//! left `a` at 2.0. The fixpoint now replays each walk's widen log on the
//! real context's parked frames.
//!
//! The hand-out needs an inner call the unit cannot specialize: a receiver
//! whose class is not proven, a `send`, or (under the recursion backstop)
//! a method that is already a frame of the unit. The floats accumulate
//! over hundreds of iterations, so a lost store diverges from the CRuby
//! oracle loudly.
extern crate monoruby;
use monoruby::tests::*;

/// A receiver of two classes: the inner call is generic and its literal
/// block leaves the unit.
#[test]
fn a_handed_out_block_under_an_unproven_receiver() {
    run_test(
        r#"
        def te
          a = 2.0
          s = 0.0
          i = 0
          while i < 300
            [1].each { (i.even? ? [2] : (2..2)).each { a *= 1.001 } }
            s += a
            i += 1
          end
          [s.round(6), a.round(6)]
        end
        te
        "#,
    );
}

/// `send` with a block literal: never specialized, always handed out.
#[test]
fn a_handed_out_block_through_send() {
    run_test(
        r#"
        def tf
          a = 2.0
          s = 0.0
          i = 0
          while i < 300
            [1].each { [2].send(:each) { a *= 1.001 } }
            s += a
            i += 1
          end
          [s.round(6), a.round(6)]
        end
        tf
        "#,
    );
}

/// The caller's local is an Integer rather than a float: the parked
/// claim is a constant, not a register view, and must go the same way.
#[test]
fn a_handed_out_block_writing_a_constant_integer() {
    run_test(
        r#"
        def tg
          n = 0
          s = 0
          i = 0
          while i < 300
            [1].each { [2].send(:each) { n += 1 } }
            s += n
            i += 1
          end
          [s, n]
        end
        tg
        "#,
    );
}

/// The same method nested under itself: the recursion backstop calls the
/// inner `each` instead of specializing it, so its block leaves the unit.
#[test]
fn a_handed_out_block_under_the_recursion_backstop() {
    run_test(
        r#"
        def th
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
        th
        "#,
    );
}
