extern crate monoruby;
use monoruby::tests::*;

// A call that passes a block literal is followed by a capture guard: if the
// callee kept the block as a Proc, the frame is on the heap now and the rest
// of the method must not read the stack copy. When all that follows is a
// `ret` of the call's result (or of `self`), a captured frame reloads that
// one slot from the heap copy and returns from compiled code instead of
// deopting (`AbstractState::immediate_evict_capturing`).

#[test]
fn capture_ret_super_default_proc() {
    // `InheritableOptions#initialize`: the block becomes the Hash's default
    // proc, so every call promotes the frame. `initialize` is inlined into
    // `new` at its call site.
    run_test(
        r#"
        class Opts < Hash
          def initialize(parent)
            @parent = parent
            super() { |h, k| @parent[k] }
          end
        end
        res = []
        30.times { |i| o = Opts.new({ a: i }); res << o[:a] << o.class }
        res
        "#,
    );
}

#[test]
fn capture_ret_dst_and_self() {
    run_test(
        r#"
        class Keep
          def keep(&b) = (@b = b; self)
          def call = @b.call
        end
        def ret_dst(k)
          x = 10
          k.keep { x += 1 }
        end
        def ret_self(k)
          k.keep { 7 }
          self
        end
        def not_ret(k)
          x = 1
          k.keep { x += 1 }.call
          x
        end
        k = Keep.new
        res = []
        30.times do
          res << ret_dst(k).call << k.call
          res << ret_self(k).class << k.call
          res << not_ret(k)
        end
        res
        "#,
    );
}

#[test]
fn capture_ret_through_binding() {
    // The callee reaches this frame through a Binding and rewrites a local
    // on the heap copy; the returned value is still the call's result.
    run_test(
        r#"
        class Keep
          def keep(&b)
            b.binding.local_variable_set(:x, 99)
            [b.call]
          end
        end
        def m(k)
          x = 1
          k.keep { x }
        end
        def n(k)
          x = 1
          k.keep { x }
          x
        end
        k = Keep.new
        res = []
        30.times { res << m(k) << n(k) }
        res
        "#,
    );
}

#[test]
fn capture_ret_in_block() {
    // A block's capture promotes its outer frame too; the block's own `ret`
    // returns to the yielder, and the method goes on in its own code.
    run_test(
        r#"
        class Keep
          def keep(&b) = (@b = b; @b)
        end
        def m(k)
          y = 5
          r = [1, 2].map { |i| k.keep { i + y } }
          y = 6
          r.map(&:call)
        end
        k = Keep.new
        res = []
        30.times { res << m(k) }
        res
        "#,
    );
}
