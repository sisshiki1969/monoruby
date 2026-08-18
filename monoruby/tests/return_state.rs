extern crate monoruby;
use monoruby::tests::*;

// Precise return states for specialized callees that contain deopt-able
// side exits (the former `frame_had_deopt` widening is gone): the caller's
// compiled continuation may fold the callee's speculative return state
// because any deopt under the call escalates through the chain walk and
// converts the caller — the continuation never runs on a deopt path
// (doc/chain_deopt.md §8.6). These pin exactly the shapes the old widening
// protected against.

#[test]
fn callee_deopt_path_returns_different_class() {
    // `get`'s compiled body covers only the @flag-truthy path (the else BB
    // is unvisited at compile time -> deopt exit) and returns Const 1.5;
    // the caller may fold `y + 1.0` to 2.5. After the flip, the deopt path
    // returns Integer 2 — the conversion must have killed the folding
    // continuation, so the VM computes 3.0 (a stale fold would keep
    // producing 2.5).
    run_test_once(
        r#"
        class RS1
          def initialize = (@flag = true)
          attr_writer :flag
          def get = @flag ? 1.5 : 2
          def use
            y = get
            y + 1.0
          end
        end
        r = RS1.new
        a = []
        60.times { a << r.use }
        r.flag = false
        10.times { a << r.use }
        a.uniq
        "#,
    );
}

#[test]
fn float_chain_through_callee_with_guard_kill() {
    // `half` proves its argument Float (guard) and returns F, so the
    // caller can keep the `s +=` chain unboxed across the call. The
    // Integer tail kills the guard mid-run; the converted frames must
    // produce the interpreter's exact sums.
    run_test_once(
        r#"
        class RS3
          def half(v) = v * 0.5
          def sum(arr)
            s = 0.0
            i = 0
            while i < arr.size
              s += half(arr[i])
              i += 1
            end
            s
          end
        end
        r = RS3.new
        x = r.sum([1.5] * 60)
        y = r.sum([1.5] * 30 + [3] * 5)
        [x.round(6), y.round(6)]
        "#,
    );
}

#[test]
fn nonlocal_return_after_guard_kill_in_block() {
    // PR #505's family: the block's `return` fires after a receiver-class
    // guard fails inside the specialized subtree. The interpreter finishes
    // the iteration and the non-local return's value must reach the
    // (converted) caller unchanged.
    run_test_once(
        r#"
        class RS2
          def initialize = (@objs = [1.5, 2.5, 3.5])
          attr_writer :objs
          def find_gt(x)
            @objs.each do |o|
              return o * 2 if o > x
            end
            :none
          end
        end
        r = RS2.new
        res = []
        50.times { res << r.find_gt(2.0) }
        r.objs = [1, 2.5]
        20.times { res << r.find_gt(2.0) }
        res.uniq
        "#,
    );
}
