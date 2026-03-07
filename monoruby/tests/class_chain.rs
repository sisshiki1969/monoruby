extern crate monoruby;
use monoruby::tests::*;

#[test]
fn class_chain1() {
    run_test(
        r##"
# Test 1: include + include (via module chain) where M appears twice in the chain
#
# Child includes M directly, then Parent includes N (which includes M).
# This creates two iclasses of M in Child's ancestor chain.
# When M#foo calls super from the second iclass, it should find GrandParent#foo.

    module M
      def foo
        super
      end
    end

    module N
      include M
    end

    class GrandParent
      def foo
        42
      end
    end

    class Parent < GrandParent
    end

    class Child < Parent
      include M
    end

    class Parent
      include N
    end

    Child.new.foo
    "##,
    );
}

#[test]
fn class_chain2() {
    run_test(
        r##"
# Test 2: include + prepend where M appears twice in the chain
#
# B2 includes M2, then A2 prepends M2.
# This creates two iclasses of M2 in B2's ancestor chain.
# When M2#bar calls super from the second iclass, it should find A2#bar.

    module M2
      def bar
        super
      end
    end

    class A2
      def bar
        42
      end
    end

    class B2 < A2
      include M2
    end

    class A2
      prepend M2
    end

    B2.new.bar
    "##,
    );
}
