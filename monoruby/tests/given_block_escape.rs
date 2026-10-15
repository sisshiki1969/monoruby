//! A method's block leaving a specialized frame for code the unit cannot
//! see.
//!
//! When `m.traverse { |e| n += 1 }` specializes `traverse` into the caller
//! and inlines the block at `traverse`'s own `yield` sites, the compiler
//! keeps claims about the caller's locals (`n` is the constant 0) on the
//! bet that it sees every store the block makes. That bet is off as soon
//! as `traverse` hands its block to code outside the unit: a generic call
//! passing a literal that `yield`s (`other { |e| yield e }`), an `&b`
//! forward of it, or a `super`. The block then runs as a unit of its own,
//! stores into the caller's frame behind the compiler's back, and an
//! inlined `yield` compiled after that point computes with the stale
//! claim — `n` came out as 1 where three yields had run. Each escape now
//! drops the home chain's claims on the spot and marks the frame as having
//! yielded generically, exactly as a non-inlined `yield` does.
//!
//! The shape that found it is a recursive tree walk
//! (`children.each { |c| c.traverse { |e| yield e } }; yield self`,
//! FileUtils' `Entry_#postorder_traverse`), whose recursive block-carrying
//! call also exposed that the block-literal exemption from the
//! specialization budget had no recursion backstop: specializing
//! `traverse` compiled the block, which specialized `traverse`, without
//! end, until the compiler overflowed its native stack. A block-carrying
//! call at a site that is already being specialized further up the stack
//! is now called, not specialized — the site, not the callee, since
//! `[1].each { [2].each { } }` nests the same method through two sites and
//! must keep inlining both. (A call *without* a block to a method that is
//! already a frame of the unit is never specialized: inlining one level
//! of a recursion buys nothing.)
extern crate monoruby;
use monoruby::tests::*;

#[test]
fn recursive_tree_walk_with_a_block_literal() {
    run_test(
        r#"
        class Node
          def initialize(d) = @d = d
          def children = @d > 0 ? [Node.new(@d - 1)] : []
          def traverse
            children.each { |c| c.traverse { |e| yield e } }
            yield self
          end
        end
        r = []
        60.times { n = 0; Node.new(2).traverse { |e| n += 1 }; r << n }
        r
        "#,
    );
}

#[test]
fn recursive_tree_walk_forwarding_the_block_param() {
    run_test(
        r#"
        class Node
          def initialize(d) = @d = d
          def children = @d > 0 ? [Node.new(@d - 1)] : []
          def traverse(&b)
            children.each { |c| c.traverse(&b) }
            yield self
          end
        end
        def run
          n = 0
          Node.new(2).traverse { |e| n += 1 }
          n
        end
        r = []
        60.times { r << run }
        r
        "#,
    );
}

#[test]
fn generic_call_passing_a_yielding_literal_then_an_inlined_yield() {
    run_test(
        r#"
        def helper
          yield 1
          yield 2
        end
        class Node
          def traverse(m)
            __send__(m) { |e| yield e }
            yield self
          end
        end
        def run(m)
          n = 0
          Node.new.traverse(m) { |e| n += 1 }
          n
        end
        r = []
        60.times { r << run(:helper) }
        r
        "#,
    );
}

#[test]
fn generic_call_forwarding_the_block_param_then_an_inlined_yield() {
    run_test(
        r#"
        def helper
          yield 1
          yield 2
        end
        class Node
          def traverse(m, &b)
            __send__(m, &b)
            yield self
          end
        end
        def run(m)
          n = 0
          Node.new.traverse(m) { |e| n += 1 }
          n
        end
        r = []
        60.times { r << run(:helper) }
        r
        "#,
    );
}

#[test]
fn super_passing_the_block_on_then_an_inlined_yield() {
    run_test(
        r#"
        class Base
          def traverse(m)
            yield 1
            yield 2
          end
        end
        class Node < Base
          def traverse(m)
            super
            yield self
          end
        end
        def run(m)
          n = 0
          Node.new.traverse(m) { |e| n += 1 }
          n
        end
        r = []
        60.times { r << run(:x) }
        r
        "#,
    );
}

#[test]
fn literal_nested_two_levels_deep_reaching_the_given_block() {
    run_test(
        r#"
        def helper
          yield 1
        end
        class Node
          def traverse(m)
            [1, 2].each { |i| __send__(m) { |e| yield e + i } }
            yield 10
          end
        end
        def run(m)
          n = 0
          Node.new.traverse(m) { |e| n += e }
          n
        end
        r = []
        60.times { r << run(:helper) }
        r
        "#,
    );
}

/// Recursion through a block literal the callee never yields to: no block
/// frame ever lands on the specialization stack, only the call site
/// repeats.
#[test]
fn recursion_with_a_block_literal_that_is_never_yielded_to() {
    run_test(
        r#"
        def f(n)
          return 0 if n == 0
          1 + f(n - 1) { :unused }
        end
        r = []
        60.times { r << f(4) }
        r
        "#,
    );
}

/// The same method nested under itself through two different sites, with a
/// float stored two outer levels up — the shape the recursion backstop
/// must keep inlining, the inner `each` being a frame of the unit already.
#[test]
fn the_same_method_nested_through_two_sites_keeps_inlining() {
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
