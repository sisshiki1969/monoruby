//! What the JIT may still believe about a frame after it hands out a block.
//!
//! A block is the one thing at a call site that can store into the
//! caller's frame behind its back, so at a block-passing call every local
//! the compiler was merely *holding* — a `LinkMode::C` constant, an
//! unboxed float in an xmm — is written to its slot and the claim given
//! up. Only the slot's `Guarded` type survives.
//!
//! Keeping the constant instead is tempting, since the compiler *does*
//! see a `StoreDynVar` from a block compiled into the same unit. It is
//! not sound: whether a callee's `yield`s inline is decided inside the
//! callee's own compile, not at the call site, and a call site that looks
//! like it will inline the block does not always get one (see #1140's
//! `Range#each`). These cases are the ones that caught it — each returns
//! a wrong answer if a constant is believed across the call.
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
/// a block that a Ruby-level `each` yields to. The `each` compile can
/// finish without ever inlining that block, so nothing tells the caller
/// its `acc` moved.
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
