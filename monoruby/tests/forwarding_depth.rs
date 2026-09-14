extern crate monoruby;
use monoruby::tests::*;

// A forwarding hop (`def f(...)`, and the `__builtin_initialize__(...)`
// forward inside the Ruby `Class#new`) is exempt from the specialization
// depth budget: not inlining a trampoline costs the caller the rest
// `Array` its `(...)` binds, because only a specialized frame can carry
// D1's elision. What bounds it instead is a recursion cap, and these
// tests are mostly about that cap holding — a forwarding chain that
// recurses must terminate at compile time and still compute the right
// answer.

const LOOP: usize = if cfg!(feature = "gc-stress") { 20 } else { 300 };

#[test]
fn forwarding_chain_deeper_than_the_budget() {
    // Five forwarding hops, then real work. Past the depth budget (3)
    // every one of these used to become a generic call that built the
    // rest Array; the answer must be the same either way.
    run_test(&format!(
        r#"
        def fd_e(a, b) = [a, b, a + b]
        def fd_d(...) = fd_e(...)
        def fd_c(...) = fd_d(...)
        def fd_b(...) = fd_c(...)
        def fd_a(...) = fd_b(...)
        r = nil
        {LOOP}.times {{ r = fd_a(3, 4) }}
        r
        "#
    ));
}

#[test]
fn forwarding_self_recursion_terminates() {
    // `def f(...) = f(...)` is the shape the recursion cap exists for:
    // the compile must stop descending, and the program must still run.
    run_test(&format!(
        r#"
        def fd_rec(n, ...) = n <= 0 ? 0 : fd_rec(n - 1, ...)
        def fd_mut_a(...) = fd_mut_b(...)
        def fd_mut_b(n, ...) = n <= 0 ? :done : fd_mut_a(n - 1, ...)
        r = []
        {LOOP}.times {{ r = [fd_rec(20), fd_mut_a(20)] }}
        r
        "#
    ));
}

#[test]
fn construction_below_the_budget() {
    // `Class#new` is the forward that matters. These sites sit deeper
    // than the budget reaches — inside a block, inside `Array#initialize`,
    // inside another block — which is exactly where the elision used to
    // be lost.
    run_test(&format!(
        r#"
        class FDPoint
          def initialize(x, y); @x = x; @y = y; end
          def to_a = [@x, @y]
        end
        class FDWrap
          def initialize(...); @v = FDPoint.new(...); end
          def to_a = @v.to_a
        end
        def fd_build(n)
          acc = []
          n.times {{ |i| acc << Array.new(2) {{ |j| FDWrap.new(i, j).to_a }} }}
          acc.last
        end
        r = nil
        {LOOP}.times {{ r = fd_build(4) }}
        [r, String.new("x"), Hash.new(0)[:k]]
        "#
    ));
}

#[test]
fn forwarding_with_keywords_and_blocks() {
    // Shapes the forward must still decline or handle correctly: literal
    // keywords through `(...)`, a block through `(...)`, and a callee
    // whose own parameters are keywords.
    run_test(&format!(
        r#"
        def fd_kw_target(a, b: 1, c: 2) = [a, b, c]
        def fd_kw(...) = fd_kw_target(...)
        def fd_blk_target(a) = yield(a)
        def fd_blk(...) = fd_blk_target(...)
        class FDKwInit
          def initialize(a, b: 5); @v = [a, b]; end
          def to_a = @v
        end
        r = nil
        {LOOP}.times do
          r = [fd_kw(1), fd_kw(1, b: 9), fd_kw(1, c: 8),
               fd_blk(3) {{ |x| x * 3 }},
               FDKwInit.new(1).to_a, FDKwInit.new(1, b: 2).to_a]
        end
        r
        "#
    ));
}

#[test]
fn forwarding_arity_errors_still_raise() {
    // The exemption must not swallow an ArgumentError that the forward
    // would raise at the far end.
    run_test(&format!(
        r#"
        def fd_two(a, b) = [a, b]
        def fd_fwd(...) = fd_two(...)
        r = []
        {LOOP}.times do
          r = []
          begin; fd_fwd(1); rescue ArgumentError => e; r << e.class; end
          begin; fd_fwd(1, 2, 3); rescue ArgumentError => e; r << e.class; end
          r << fd_fwd(1, 2)
        end
        r
        "#
    ));
}
