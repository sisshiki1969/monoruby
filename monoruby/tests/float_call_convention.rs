//! What a specialized call does with a float that crosses it.
//!
//! The value travels in a register in both directions where both sides
//! know it is a raw f64: the call site stages an argument in a pool
//! register the callee's entry binds, and the callee's return segments
//! leave the result in the float-return register. Each case below is a
//! shape where one half of that decision has a branch of its own.
extern crate monoruby;
use monoruby::tests::*;

/// A return edge that is a float *literal* rather than an fpr: the
/// constant is materialized straight into the return register, with no
/// fpr to read it out of. Two edges, so the join has to accept the pair.
///
/// This is aobench's shape — an early `return 0.0` guarding a computed
/// float — and the reason the convention accepts a `C` edge at all.
#[test]
fn a_float_constant_return_edge() {
    run_test(
        r#"
        def clamped(d)
          return 0.0 if d < 1.0
          160.0 / d
        end
        s = 0.0
        i = 0
        while i < 200_000
          s += clamped((i % 7).to_f)
          i += 1
        end
        s
        "#,
    );
}

/// A float argument at a call site that also passes a block. The block
/// argument has to be written back before the frame is built, and that
/// write-back can call out and take the register pool with it, so the
/// hand-off is declined here and the argument goes through its slot.
#[test]
fn a_float_argument_at_a_block_passing_call() {
    run_test(
        r#"
        def scaled(x, &blk)
          blk.call(x * 2.0)
        end
        s = 0.0
        i = 0
        while i < 200_000
          s += scaled(i.to_f * 0.5) { |v| v + 1.0 }
          i += 1
        end
        s
        "#,
    );
}
