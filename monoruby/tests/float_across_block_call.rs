//! What survives of a frame's *float* registers across the call that hands
//! out its block.
//!
//! At such a call every unboxed local is boxed into its slot, because the
//! block can read the slot behind the compiler's back. Boxing an `F` — a
//! slot whose only copy is an fpr — leaves both copies live, so the binding
//! lands on `Sf` and the frame keeps reading the float out of the register
//! after the call instead of decoding it out of the box again.
//!
//! That is only correct while the register and the slot still agree, and
//! the block is exactly the thing that can make them disagree. Each case
//! below reports a stale float if the view is kept where it should have
//! been dropped, or reads an unwritten slot if the box is not stored.
extern crate monoruby;
use monoruby::tests::*;

const PRELUDE: &str = r#"
    def call_block
      yield
    end
    def call_block_twice
      yield
      yield
    end
"#;

/// The block reads the outer float: the box has to be in the slot, since
/// a `LoadDynVar` reads the slot and never the caller's register.
#[test]
fn the_block_reads_the_float() {
    run_test_with_prelude(
        r#"
        def f(n)
          a = n * 1.5
          b = call_block { a + 1.0 }
          [a, b, a + 0.25]
        end
        f(3)
        "#,
        PRELUDE,
    );
}

/// The block writes it, so the register the caller kept is stale and the
/// binding has to go.
#[test]
fn the_block_writes_the_float() {
    run_test_with_prelude(
        r#"
        def f(n)
          a = n * 1.5
          call_block { a = a + 2.0 }
          a
        end
        def g(n)
          a = n * 1.5
          call_block_twice { a += 2.0 }
          a * 2.0
        end
        [f(3), g(3)]
        "#,
        PRELUDE,
    );
}

/// The block replaces the float with something that is not a float, so
/// what has to be dropped is not only the register but the `Float` type
/// the `Sf` binding asserts about the slot.
#[test]
fn the_block_changes_the_type() {
    run_test_with_prelude(
        r#"
        def f(n)
          a = n * 1.5
          call_block { a = "str" }
          a
        end
        def g(n)
          a = n * 1.5
          call_block { a = 7 }
          a + 1
        end
        [f(3), g(3)]
        "#,
        PRELUDE,
    );
}

/// The callee is a builtin, so the block is a unit of its own and its
/// stores never reach the compiler: nothing about the float may be kept.
#[test]
fn a_builtin_callee() {
    run_test_with_prelude(
        r#"
        def f(n)
          a = n * 1.5
          3.times { a += 1.0 }
          a
        end
        def g(n)
          a = n * 1.5
          [1, 2].each { a *= 2.0 }
          a
        end
        [f(3), g(3)]
        "#,
        PRELUDE,
    );
}

/// The store is two frames down, and reaches past its immediate outer
/// frame to a float the frame above that was holding in a register.
#[test]
fn a_store_through_two_outer_levels() {
    run_test_with_prelude(
        r#"
        def f(n)
          a = n * 1.5
          b = n * 0.25
          call_block { call_block { a += b } }
          [a, b, a - b]
        end
        f(3)
        "#,
        PRELUDE,
    );
}

/// A loop whose body hands a block out: the merge at the loop head sees
/// one path with the register view still held and one without.
#[test]
fn a_block_call_inside_a_loop() {
    run_test_with_prelude(
        r#"
        def g(n)
          total = 0.0
          5.times do |i|
            a = n * 1.5 + i
            call_block { total += a }
          end
          total
        end
        g(3)
        "#,
        PRELUDE,
    );
}

/// The same with a `while` loop, which used to abort the compiler with
///
/// ```text
/// unreachable code: %1 S(Value)->C(3)
/// ```
///
/// `%1` is `n`. It reached the loop entry as `C(3)`, folded from the call
/// site, and reached the back edge as `S(Value)`, given up by the
/// `forget_constants` on the path through the block-passing call — and
/// `bridge` has no `S -> C`, because nothing can prove a slot holds the
/// constant the target claims.
///
/// The disagreement was between the two passes, not between the paths:
/// the loop-entry target comes from the back-edge fixpoint, which runs in
/// analysis mode, where a guarded unbox emits nothing and so never set
/// `had_deopt` — the very flag `specialized_iseq` reads to decide whether
/// the call may keep the frame's constants. The analysis predicted a kept
/// `C(3)`; codegen gave it up. See `AsmIr::note_analysis_deopt`.
#[test]
fn a_while_loop_around_a_block_passing_call() {
    run_test_with_prelude(
        r#"
        def f(n)
          total = 0.0
          i = 0
          while i < 5
            a = n * 1.5
            call_block { a *= 2.0 }
            total += a
            i += 1
          end
          total
        end
        f(3)
        "#,
        PRELUDE,
    );
}

/// The float is live across the call but the block never touches it, so
/// the register is the only thing that has to still be right — this is
/// the case the change exists for.
#[test]
fn a_float_untouched_by_the_block() {
    run_test_with_prelude(
        r#"
        def f(n)
          a = n * 1.5
          b = n * 2.5
          c = call_block { 1 }
          [a + 1.0, b * 2.0, a * b, c]
        end
        f(3)
        "#,
        PRELUDE,
    );
}
