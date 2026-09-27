extern crate monoruby;
use monoruby::tests::*;

// `&block` forwarded from inside a nested block (`BlockArgProxy` with
// `outer > 0`) passes the method's block handler on as a proxy whose frame
// depth is counted at run time (`Executor::forward_block_param`), instead of
// materializing it into a Proc and promoting the frames to the heap.

#[test]
fn forward_from_nested_block() {
    run_test(
        r#"
        def each_pair_value(h, &block)
          h.each_value { |inner| inner.each_value(&block) }
        end
        res = []
        each_pair_value({ a: { x: 1, y: 2 }, b: { z: 3 } }) { |v| res << v * 10 }
        res
        "#,
    );
}

#[test]
fn forward_two_levels_out() {
    run_test(
        r#"
        def inner; yield 5; end
        def two(&b)
          [1].map { [2].map { inner(&b) } }
        end
        two { |x| x + 1 }
        "#,
    );
}

#[test]
fn forward_break_and_return() {
    run_test(
        r#"
        def inner; yield; :not_broken; end
        def relay(&b); [1, 2].each { |_| inner(&b) }; :done; end
        r1 = relay { break :broken }
        def ret_from_block
          relay { return :returned }
          :fell_through
        end
        [r1, ret_from_block, relay { 1 }]
        "#,
    );
}

#[test]
fn forward_through_reyielding_blocks() {
    // The frame count between the forwarding block and its method is not
    // a static function of the lexical depth (issue #982).
    run_test(
        r#"
        def base; yield :s; end
        def relay; base { |s| yield(s) }; end
        def inner; yield 1; end
        def outer(&b); relay { |_s| [0].map { inner(&b) } }; end
        res = []
        res << outer { |x| x + 100 }
        res << (outer { break :b })
        res
        "#,
    );
}

#[test]
fn forward_handler_kinds() {
    // A Proc, a Symbol, no block, and an assigned parameter.
    run_test(
        r#"
        def inner(*a, &c); c ? c.call(*a) : :none; end
        def fwd(&b); [3].map { |x| inner(x, &b) }; end
        pr = proc { |x| x * 2 }
        def reassigned(&b)
          b = proc { |x| x - 1 }
          [3].map { |x| inner(x, &b) }
        end
        [fwd(&pr), fwd(&:succ), fwd, fwd { |x| x + 1 }, reassigned { |x| x }]
        "#,
    );
}

#[test]
fn forward_block_given_and_identity() {
    run_test(
        r#"
        def given?; block_given?; end
        def get(&c) = c
        def m(&b)
          r = [1, 2].map { get(&b) }
          [[1].map { given?(&b) }, r[0].equal?(r[1]), r[0]&.call]
        end
        [m { :x }, m]
        "#,
    );
}

#[test]
fn forward_after_escape() {
    // The forwarding block outlives its method (a Proc), and the method's
    // frame is on the heap by the time it runs.
    run_test(
        r#"
        def inner; yield 7; end
        def make(&b); proc { inner(&b) }; end
        def make_lambda(&b); -> { [1].map { inner(&b) } }; end
        [make { |x| x * 3 }.call, make_lambda { |x| x + 1 }.call]
        "#,
    );
}

#[test]
fn forward_across_fiber() {
    run_test(
        r#"
        def inner; yield 2; end
        def in_fiber(&b)
          Fiber.new { [1].map { inner(&b) } }.resume
        end
        in_fiber { |x| x * 21 }
        "#,
    );
}
