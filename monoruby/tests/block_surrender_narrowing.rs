//! Handing a block out of a JIT unit surrenders only the block's home
//! chain, not every frame of the state chain.
//!
//! `unbox_to_S_for_outgoing_block` (né `all_frames_unbox_to_S`) used to
//! demote every frame's float views and constants when any block left
//! the unit; a block can only reach its home frame and that frame's
//! lexical ancestors, so a suspended *method* caller elsewhere in the
//! specialization chain keeps its views now. These shapes pin the
//! semantics of exactly the narrowed paths, with float state alive in
//! the frames that are (and are not) reachable — the stress-spill-pool
//! feature exercises the same shapes with a shrunken FPR pool.
extern crate monoruby;
use monoruby::tests::*;

#[test]
fn suspended_caller_floats_survive_block_handout() {
    // outer_m's float local is live across a specialized call to inner,
    // which hands a block literal to a builtin (non-inlinable block →
    // the surrender path). The block's home is inner's frame; outer_m's
    // frame must keep computing correctly without being surrendered.
    run_test(
        r##"
        def inner(k)
          arr = [k + 2, k, k + 1]
          arr.sort! { |a, b| b <=> a }
          arr[0] + 0.25
        end
        def outer_m(x)
          f = x * 1.5
          g = inner(3)
          f + g
        end
        r = 0.0
        60.times { |i| r += outer_m(i) }
        r
        "##,
    );
}

#[test]
fn generic_yield_with_outside_home() {
    // yield in a root unit: the block's home is the root's caller,
    // outside the unit — no in-chain frame is reachable, and the root's
    // own float locals stay live across the yield.
    run_test(
        r##"
        def leaf(i)
          q = 1.25 * i
          t = yield 3
          q + t
        end
        r = 0.0
        60.times { |i| r += leaf(i) { |v| v + i } }
        r
        "##,
    );
}

#[test]
fn block_writes_into_its_home_chain() {
    // The handed block writes its home's local: the home chain must
    // still be surrendered (stale views dropped) so the write is
    // observed after the call.
    run_test(
        r##"
        def collect(n)
          acc = 0.0
          sink = [3, 1, 2]
          sink.sort! { |a, b| acc += 0.5; a <=> b }
          acc + sink[0]
        end
        r = 0.0
        60.times { r += collect(3) }
        r
        "##,
    );
}
