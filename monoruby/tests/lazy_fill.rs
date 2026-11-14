//! The deferred prologue fill (`FnInitInfo::lazy_fill`): a loop-free framed
//! JIT body leaves its locals and temporaries unfilled until the frame
//! first becomes visible (a call, a side exit, a poll). Every case below
//! makes the frame visible at a different point — a collection from a
//! callee, a deopt back to the VM, a `binding`, a rescued exception — with
//! some locals not assigned yet, and checks they read as `nil`.

extern crate monoruby;
use monoruby::tests::*;

#[test]
fn gc_in_a_callee_after_a_leaf_path() {
    // The even path returns without a call (no fill at all); the odd path
    // collects from a callee with `b`..`e` still unassigned.
    run_test(
        r##"
        def __gc(x) = (GC.start if x % 5 == 1; x)
        def __f(x)
          a = x * 2
          return a if x.even?
          b = __gc(x)
          c = [a, b]
          d = c.map { |y| y + 1 }
          e = d.sum
          [a, b, c, d, e]
        end
        r = []
        30.times { |i| r << __f(i) }
        r
        "##,
    );
}

#[test]
fn deopt_before_any_call_reads_unassigned_locals() {
    // `x + 1` deopts on the Float before anything was called; the VM
    // resumes in a frame whose `a` and `b` were never assigned.
    run_test_once(
        r##"
        def __f(x)
          a = 7 if x == 1000
          y = x + 1
          b = y if x == 2000
          GC.start if y.is_a?(Float)
          [a, b, y]
        end
        r = []
        40.times { |i| r << __f(i) }
        r << __f(1.5)
        r << __f(1000)
        r
        "##,
    );
}

#[test]
fn binding_sees_nil_for_unassigned_locals() {
    run_test(
        r##"
        def __f(x)
          a = 1 if x.nil?
          b = binding
          [b.local_variable_get(:a), b.local_variable_defined?(:c), (c = 3)]
        end
        r = []
        30.times { |i| r << __f(i) }
        r
        "##,
    );
}

#[test]
fn rescued_exception_leaves_later_locals_nil() {
    run_test(
        r##"
        def __raise(x) = (raise "odd" if x.odd?; x)
        def __gc(x) = (GC.start if x % 5 == 1; x)
        def __f(x)
          begin
            v = __raise(x)
            w = v + 1
          rescue => e
            m = e.message
          end
          __gc(x)
          [v, w, m]
        end
        r = []
        30.times { |i| r << __f(i) }
        r
        "##,
    );
}

#[test]
fn exposure_on_one_arm_then_after_the_merge() {
    // One arm fills (the call), the other does not; the merge is unfilled
    // where either side was, and the collection after it sees every home
    // filled.
    run_test(
        r##"
        def __id(x) = x
        def __f(x)
          if x.odd?
            a = __id(x)
          else
            b = x
          end
          GC.start if x % 7 == 0
          c = [a, b]
          [a, b, c]
        end
        r = []
        30.times { |i| r << __f(i) }
        r
        "##,
    );
}

#[test]
fn many_locals_filled_in_pairs_and_singles() {
    // Enough unassigned locals for the paired (`movups`) fill, with gaps
    // left by the ones assigned before the first call.
    run_test(
        r##"
        def __gc(x) = (GC.start if x % 9 == 4; x)
        def __f(x)
          l1 = x
          l4 = x + 4
          __gc(x)
          l2 = l1 + 1; l3 = l2 + 1; l5 = l4 + 1; l6 = l5 + 1
          l7 = l6 + 1; l8 = l7 + 1; l9 = l8 + 1; l10 = l9 + 1
          [l1, l2, l3, l4, l5, l6, l7, l8, l9, l10]
        end
        r = []
        30.times { |i| r << __f(i) }
        r
        "##,
    );
}

#[test]
fn argument_forwarding_method() {
    // In `def m(...)` the block slot `Iseq::block_param_slot` names is an
    // ordinary local, not the slot the prologue clears.
    run_test(
        r##"
        def __g(*a, **k, &b) = [a, k, b&.call]
        def __f(...)
          GC.start
          __g(...)
        end
        r = []
        30.times { |i| r << __f(i, k: i) { i * 2 } }
        r
        "##,
    );
}
