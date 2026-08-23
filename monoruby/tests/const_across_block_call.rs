//! What the JIT may still believe about a frame after it hands out a block.
//!
//! A block is the one thing at a call site that can store into the
//! caller's frame behind its back, so at a block-passing call every local
//! the compiler was merely *holding* is written to its slot. Whether the
//! *claim* survives is another matter, and it survives only when the
//! compiler can see every store the block makes — which needs all of:
//!
//!  * the callee to be an iseq with an iseq block literal, so the block
//!    is a candidate for compilation into this same unit;
//!  * its compile to have inlined every `yield` and to contain no
//!    deopt-able side exit, since either lets the block run somewhere the
//!    compiler is not looking (confirmed after the fact, not predicted);
//!  * the frame to be this compilation's own. An outer frame gives its
//!    constants up on the way in: its claim describes one moment, and the
//!    compilation may be entered from it many times over.
//!
//! Each case below returns a wrong answer if one of those is skipped.
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

/// The block only reads the caller's local.
#[test]
fn a_read_only_block() {
    run_test_with_prelude(
        r#"
        def f
          x = 10
          y = call_block { x + 1 }
          [x, y, x + y]
        end
        f
        "#,
        PRELUDE,
    );
}

/// The block writes it — believing the constant reports the old value.
#[test]
fn a_store_from_the_block() {
    run_test_with_prelude(
        r#"
        def f
          x = 10
          call_block { x = "str" }
          x
        end
        def g
          x = 10
          call_block_twice { x += 1 }
          x
        end
        def h
          x = 1.0
          call_block { x += 0.5 }
          x
        end
        [f, g, h]
        "#,
        PRELUDE,
    );
}

/// The callee is a builtin, so the block is compiled as a unit of its own
/// and its stores never reach the compiler at all.
#[test]
fn a_builtin_callee() {
    run_test_with_prelude(
        r#"
        def f
          x = 10
          [1, 2].each { x += 1 }
          x
        end
        def g
          x = 3
          1.times { x += 0.5 }
          x
        end
        [f, g]
        "#,
        PRELUDE,
    );
}

/// The store is two frames down, inside a block handed to a builtin from
/// inside a block that was inlined here.
#[test]
fn a_builtin_block_nested_in_an_inlined_block() {
    run_test_with_prelude(
        r#"
        def f
          a = 3
          1.times { call_block { a += 0.5 } }
          a
        end
        def g
          a = 3
          call_block { 1.times { a += 0.5 } }
          a
        end
        def h
          a = 3
          b = 100
          call_block { [1, 2].each { a += 1; b -= 1 } }
          [a, b]
        end
        [f, g, h]
        "#,
        PRELUDE,
    );
}

/// Two levels of inlined block, with the store at the bottom reaching
/// past its immediate outer frame.
#[test]
fn a_store_through_two_outer_levels() {
    run_test_with_prelude(
        r#"
        def f
          a = 3
          b = 4
          call_block { call_block { a += 1; b = "s" } }
          [a, b]
        end
        f
        "#,
        PRELUDE,
    );
}

/// The block reads the slot, not the compiler's claim, so the value has
/// to be there whether or not the claim survives.
#[test]
fn the_block_reads_the_slot() {
    run_test_with_prelude(
        r#"
        def f
          x = 42
          y = 1.5
          z = call_block { [x, y] }
          [x, y, z]
        end
        f
        "#,
        PRELUDE,
    );
}

/// A loop whose body hands a block out: the merge at the loop head sees
/// one path with the constant still held and one without.
#[test]
fn a_block_call_inside_a_loop() {
    run_test_with_prelude(
        r#"
        def f
          total = 0
          10.times do |i|
            x = 5
            [1, 2].each { x += i }
            total += x
          end
          total
        end
        def g
          a = 0
          n = 0
          while n < 10
            b = 7
            call_block { [1].each { a += b } }
            n += 1
          end
          [a, n]
        end
        [f, g]
        "#,
        PRELUDE,
    );
}

/// #1140's shape, reduced: a constant-initialised accumulator written by
/// a block that a Ruby-level `each` yields to. The `each` compile stops
/// at a side exit before it ever reaches the `yield`, so nothing in it
/// tells the caller its `acc` moved — while at runtime the block runs
/// every iteration and accumulates into it.
#[test]
fn an_accumulator_through_a_ruby_level_each() {
    run_test_once(
        r#"
        class SpecAcc
          K = 1.25
          def calc(n, use_k)
            acc = 0.0
            (0...n).each do |x|
              acc += x
              acc += K if use_k
            end
            acc
          end
        end
        r = SpecAcc.new
        s = 0.0
        60.times { s += r.calc(30, true) }
        Object.const_set(:BUMP_SPEC_ACC, 1)
        40.times { s += r.calc(30, false) }
        120.times { s += r.calc(30, true) }
        s.round(6)
        "#,
    );
}

/// The block is entered many times with a different value each time, and
/// nothing in its own chain says so: `Integer#times` is a method, so it
/// is nobody's lexical outer and its loop is not in the block's chain at
/// all. Believing the caller's constant, the block's own `if` merge
/// bridged it back into the caller's slot on every iteration but one —
/// `[2.0, 2.0, 2.0, 3.0, 2.0, 2.0]` for `[2.0, 4.0, 6.0, 3.0, 5.0, 7.0]`.
#[test]
fn a_block_entered_repeatedly_from_a_callee_loop() {
    run_test(
        r#"
        def kill_int(n)
          acc = 0.0
          hits = []
          n.times do |i|
            acc = 1 if i == 3
            acc += 2.0
            hits << acc
          end
          hits
        end
        r = nil
        10.times { r = kill_int(6) }
        r
        "#,
    );
}

/// The same without the branch inside the block, so the only merge that
/// can reintroduce the stale claim is the callee's own loop head.
#[test]
fn a_block_accumulating_across_a_callee_loop() {
    run_test(
        r#"
        def acc_int(n)
          acc = 0.0
          n.times { acc += 2.0 }
          acc
        end
        def acc_range(n)
          acc = 0.0
          (0...n).each { |i| acc += i }
          acc
        end
        r = nil
        10.times { r = [acc_int(6), acc_range(6)] }
        r
        "#,
    );
}
