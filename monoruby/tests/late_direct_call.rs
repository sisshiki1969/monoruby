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
//
// A callee with a loop is not inlined into its caller (a frameless body
// would be, and then there is no call to repoint), so the callees here
// carry one.

#[test]
fn late_direct_call_reaches_the_callee() {
    run_test_once(
        r#"
        class LdcA
          def initialize(x) = @x = x
          def callee(y)
            i = 0
            while i < 1; i += 1; end
            @x + y
          end
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
          def get
            i = 0
            while i < 1; i += 1; end
            [@a, @b]
          end
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
          def callee
            i = 0
            while i < 1; i += 1; end
            1
          end
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

/// The caller of a body that inlined `Integer#-` did not inline it, so
/// redefining the operator evicts the callee and not the caller. The
/// caller's call, repointed at the body (`caller_first`) or direct from
/// the start (`callee_first`), must not reach the evicted body: on x86 it
/// went on returning the old operator's results for good.
#[test]
fn late_direct_call_bop_redefined_in_called_body() {
    for callee_first in ["false", "true"] {
        run_test_once(
            &r#"
            class LdcK
              def initialize(n) = @n = n
              def step(x)
                i = 0
                s = 0
                while i < 3
                  s += x - i
                  i += 1
                end
                s + @n
              end
            end
            o = LdcK.new(1)
            300.times { |i| o.step(i) } if CALLEE_FIRST
            def ldc_k(o, x) = o.step(x) * 2
            r = []
            300.times { |i| r << ldc_k(o, i) }
            class Integer
              alias_method :__ldc_orig_minus, :-
              def -(o) = __ldc_orig_minus(o) + 1000
            end
            r << ldc_k(o, 10) << ldc_k(o, 11)
            class Integer
              alias_method :-, :__ldc_orig_minus
            end
            r.last(3)
            "#
            .replace("CALLEE_FIRST", callee_first),
        );
    }
}

/// A callee whose compile for one receiver class bails is given up for
/// good, and the bodies it has for other classes go with it — out of the
/// index that a later redefinition poisons. A caller's call repointed at
/// (or compiled straight into) one of those bodies must not keep running
/// it: on x86 it did, and missed `helper` being redefined.
#[test]
fn late_direct_call_after_callee_invalidated() {
    for callee_first in ["false", "true"] {
        run_test_once(
            &r#"
            class LdcP
              def flag = false
              def m(x)
                i = 0
                while i < 1; i += 1; end
                return (-> { x }).call if flag
                helper(x)
              end
              def helper(x) = x + 1
            end
            class LdcPA < LdcP; end
            class LdcPB < LdcP; def flag = true; end
            def ldc_g(o, x) = o.m(x)
            a = LdcPA.new
            b = LdcPB.new
            1000.times { |i| a.m(i) } if CALLEE_FIRST
            1000.times { |i| ldc_g(a, i) }
            1000.times { |i| b.m(i) }
            class LdcP; def helper(x) = x + 100; end
            [ldc_g(a, 1), a.m(1)]
            "#
            .replace("CALLEE_FIRST", callee_first),
        );
    }
}
