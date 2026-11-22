extern crate monoruby;
use monoruby::tests::*;

// A JIT call site that proved its receiver's class calls the callee's
// compiled body for that class directly. A callee is usually compiled
// *after* its first caller, though, and such a site used to go through
// the callee's wrapper for good. It is now remembered and pointed at the
// body once that is compiled (`ISeqInfo::add_late_direct_call`). The
// cases here make the caller compile first and then check that the
// repointed call reaches the right body, and stops reaching it when it
// goes stale.

#[test]
fn late_direct_call_reaches_the_callee() {
    run_test_once(
        r#"
        class LdcA
          def initialize(x) = @x = x
          def callee(y) = @x + y
        end
        def ldc_caller(o, y) = o.callee(y)
        o = LdcA.new(10)
        r = []
        100.times { |i| r << ldc_caller(o, i) }
        r
        "#,
    );
}

#[test]
fn late_direct_call_per_receiver_class() {
    // Two sites prove two subclasses that inherit one method but lay their
    // instance variables out differently; each must get its own class's
    // body once that is compiled.
    run_test_once(
        r#"
        class LdcBase
          def get = [@a, @b]
        end
        class LdcS1 < LdcBase
          def initialize = (@a = 1; @b = 2)
        end
        class LdcS2 < LdcBase
          def initialize = (@b = 3; @z = 0; @a = 4)
        end
        def ldc_s1(o) = o.get
        def ldc_s2(o) = o.get
        s1 = LdcS1.new
        s2 = LdcS2.new
        r = []
        50.times { r << ldc_s1(s1) }
        50.times { r << ldc_s2(s2) }
        50.times { r << ldc_s1(s1) << ldc_s2(s2) }
        r.uniq
        "#,
    );
}

#[test]
fn late_direct_call_callee_redefined() {
    run_test_once(
        r#"
        class LdcR
          def callee = 1
        end
        def ldc_r(o) = o.callee
        o = LdcR.new
        r = []
        60.times { r << ldc_r(o) }
        class LdcR
          def callee = 2
        end
        60.times { r << ldc_r(o) }
        r.tally
        "#,
    );
}

#[test]
fn late_direct_call_recursion() {
    run_test_once(
        r#"
        class LdcFib
          def fib(n) = n < 2 ? n : fib(n - 1) + fib(n - 2)
        end
        f = LdcFib.new
        [f.fib(20), f.fib(22)]
        "#,
    );
}

#[test]
fn late_direct_call_bop_redefined_in_callee() {
    // The callee inlined `Integer#-`; redefining it evicts the callee's
    // body, and the repointed caller must see the new operator.
    run_test_once(
        r#"
        class LdcB
          def callee(x) = x - 1
        end
        def ldc_b(o, x) = o.callee(x)
        o = LdcB.new
        r = []
        60.times { |i| r << ldc_b(o, i) }
        class Integer
          def -(other) = 100
        end
        r << ldc_b(o, 5) << ldc_b(o, 6)
        r.last(3)
        "#,
    );
}
