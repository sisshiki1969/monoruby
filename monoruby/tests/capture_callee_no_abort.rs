extern crate monoruby;
use monoruby::tests::*;

// A callee that can capture the caller's frame used to abort the whole
// compilation unit: a method whose body forwards `&block` into a Proc
// (`BlockArg`), or a builtin that captures without a block (`eval`,
// `binding`, `class_eval` with a string). The caller is compiled now; the
// capturable frames are written back before the call and a capture guard
// after it sends a promoted frame to the VM, which reads the heap copy.
// Every test below runs its body through the JIT and checks against CRuby.

#[test]
fn block_forwarding_callee_not_inlined() {
    // `BodyProxy#initialize(body, &block)` is `Rack::BodyProxy`'s shape:
    // the block escapes as a Proc and is run after `new` returned, so the
    // caller's `x` must be read from the heap frame.
    run_test(
        r#"
        class BodyProxy
          def initialize(body, &block)
            @body = body
            @block = block
          end
          def close = @block.call
        end
        def wrap(a)
          x = a + 1
          f = a.to_f * 1.5
          bp = BodyProxy.new(x) { x += 10; f += 0.25 }
          bp.close
          [x, f, bp.close, x, f]
        end
        res = []
        30.times { |i| res << wrap(i) }
        res
        "#,
    );
}

#[test]
fn block_forwarding_callee_via_small_immediate_call() {
    // An immediate argument makes the call site `specializable`; the
    // callee still must not be inlined.
    run_test(
        r#"
        class Keeper
          def keep(tag, &b)
            @b = b
            tag
          end
          def fire = @b.call
        end
        K = Keeper.new
        def run
          n = 3
          t = K.keep(7) { n *= 2 }
          K.fire
          [t, n, K.fire, n]
        end
        res = []
        30.times { res << run }
        res
        "#,
    );
}

#[test]
fn eval_callee_writes_caller_local() {
    run_test(
        r#"
        def bump(a)
          x = a * 2
          f = a.to_f / 4
          eval("x = x + 1; f = f * 2.0")
          [x, f]
        end
        res = []
        30.times { |i| res << bump(i) }
        res
        "#,
    );
}

#[test]
fn binding_callee_writes_caller_local() {
    run_test(
        r#"
        def via_binding(a)
          x = a
          y = 1.5
          b = binding
          b.local_variable_set(:x, a + 100)
          b.local_variable_set(:y, y * 3)
          [x, y, b.local_variable_get(:x)]
        end
        res = []
        30.times { |i| res << via_binding(i) }
        res
        "#,
    );
}

#[test]
fn eval_callee_inside_inlined_callee() {
    // The capturing call sits in a callee that is itself inlined into the
    // caller: the caller's constant folds must not survive across it.
    run_test(
        r#"
        def inner(v)
          w = v + 1
          eval("w += 5")
          w
        end
        def outer(a)
          k = 10
          r = inner(k)
          [r, k, inner(a)]
        end
        res = []
        30.times { |i| res << outer(i) }
        res
        "#,
    );
}

#[test]
fn class_eval_string_callee() {
    run_test(
        r#"
        class Box; end
        def define_and_call(i)
          n = i
          Box.class_eval("def v#{i % 3} = #{i}")
          n + Box.new.send(:"v#{i % 3}")
        end
        res = []
        30.times { |i| res << define_and_call(i) }
        res
        "#,
    );
}
