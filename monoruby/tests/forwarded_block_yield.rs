extern crate monoruby;
use monoruby::tests::*;

// A block written at a `Klass.new { .. }` site reaches `initialize`
// through the Ruby `Class#new` trampoline's `(...)` forward. The JIT
// resolves that chain (`resolve_given_block`) and specializes the
// `yield` inside `initialize` against the literal block — these tests
// pin the semantics that specialization must preserve.

#[test]
fn forwarded_block_yield_semantics() {
    // Closure capture, self, and yield values through the chain.
    run_test(
        r#"
        class FwdA
          attr_reader :a
          def initialize(n)
            @a = []
            i = 0
            while i < n
              @a << yield(i)
              i += 1
            end
          end
        end
        x = 100
        f = FwdA.new(4) { |i| x += i; i * 2 }
        [f.a, x]
        "#,
    );
}

#[test]
fn forwarded_block_break_and_partial_contents() {
    // `break` exits the `new` invocation with the break value; the
    // incremental fill keeps already-produced elements in the receiver
    // (observable via a direct `send(:initialize)`).
    run_test(
        r#"
        r1 = Array.new(4) { |i| break :broke if i == 2; i }
        a = [9]
        r2 = a.send(:initialize, 5) { |i| break :b if i == 3; i.to_s }
        [r1, r2, a]
        "#,
    );
}

#[test]
fn forwarded_block_nonlocal_return() {
    // `return` inside the block returns from the enclosing method,
    // unwinding initialize and Class#new.
    run_test(
        r#"
        class FwdB
          def initialize(n)
            @v = []
            i = 0
            while i < n
              @v << yield(i)
              i += 1
            end
          end
        end
        def probe
          FwdB.new(5) { |i| return [:early, i] if i == 3; i }
          :finished
        end
        probe
        "#,
    );
}

#[test]
fn forwarded_block_given_folds() {
    // `block_given?` inside initialize sees through the forward — both
    // the with-block and the no-block construction, from the same
    // (JIT-compiled) call shapes.
    run_test(
        r#"
        class FwdC
          attr_reader :tag
          def initialize
            @tag = block_given? ? yield : :none
          end
        end
        res = []
        10.times do
          res = [FwdC.new { :blk }.tag, FwdC.new.tag]
        end
        res
        "#,
    );
}

#[test]
fn forwarded_block_proc_argument() {
    // An explicit `&proc` at the `new` site is a dynamic block — the
    // chain resolution must stay conservative and still run it right.
    run_test(
        r#"
        class FwdD
          attr_reader :a
          def initialize(n)
            @a = []
            i = 0
            while i < n
              @a << yield(i)
              i += 1
            end
          end
        end
        pr = ->(i) { i + 10 }
        FwdD.new(3, &pr).a
        "#,
    );
}

#[test]
fn forwarded_block_double_hop() {
    // Two forwarding hops: user -> wrap(...) -> Class#new(...) ->
    // initialize. The walk follows every `(...)` link to the literal.
    run_test(
        r#"
        class FwdE
          attr_reader :a
          def initialize(n)
            @a = []
            i = 0
            while i < n
              @a << yield(i)
              i += 1
            end
          end
        end
        def wrap(...)
          FwdE.new(...)
        end
        y = 5
        w = wrap(3) { |i| i * y }
        r1 = w.a
        r2 = wrap(4) { |i| break :hop_broke if i == 2; i }
        [r1, r2]
        "#,
    );
}
