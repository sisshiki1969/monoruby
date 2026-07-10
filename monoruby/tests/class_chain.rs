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

#[test]
fn classdef_with_live_float() {
    // Regression: a ClassDef reached while a float accumulator is live in the
    // JIT's FP pool must preserve the float (the pool is saved once across the
    // define/enter/body/exit sequence and reloaded into the pool registers
    // before each HandleError). The harness runs this 25x, so the top-level
    // method gets JIT-compiled and exercises the ClassDef emit.
    run_test(
        r##"
        acc = 0.0
        i = 0
        while i < 30
          acc += i.to_f * 0.5
          class Foo
            def bar; 2; end
          end
          acc += Foo.new.bar
          i += 1
        end
        acc
        "##,
    );
}

#[test]
fn singleton_classdef_with_live_float() {
    // Regression: same as classdef_with_live_float for `class << obj`.
    run_test(
        r##"
        obj = Object.new
        acc = 0.0
        i = 0
        while i < 30
          acc += i.to_f * 0.25
          class << obj
            def greet; 7; end
          end
          acc += obj.greet
          i += 1
        end
        acc
        "##,
    );
}


#[test]
fn superclass_must_be_a_class() {
    run_test_once(
        r#"
        res = []
        [42, "s", :sym, Module.new, Object.new.singleton_class].each do |sup|
          begin
            Class.new(sup)
            res << "no error"
          rescue TypeError => e
            res << e.message
          end
          begin
            eval("class SuperclassCheckSpec < sup; end")
            res << "no error"
          rescue TypeError => e
            res << e.message
          end
        end
        res
        "#,
    );
}

#[test]
fn metaclass_is_classed_by_class_metaclass() {
    // MRI's eigenclass model (one level deep): the class of a class's
    // metaclass is Class's metaclass, a module's metaclass is classed
    // by Module's metaclass, and Class's own metaclass is its own
    // class. `.class` still reports Class everywhere (it skips
    // singletons), and plain objects' singleton classes are untouched.
    run_test(
        r#"
        class MCA; end
        class MCH < MCA; end
        module MCM; end
        [
          MCA.singleton_class.is_a?(Class.singleton_class),
          MCM.singleton_class.is_a?(Module.singleton_class),
          MCH.singleton_class.superclass == MCA.singleton_class,
          MCA.singleton_class.class.to_s,
          MCM.singleton_class.class.to_s,
          Object.new.singleton_class.class.to_s,
          Class.singleton_class.class == Class.singleton_class,
          MCA.singleton_class.is_a?(Class),
          MCA.new.singleton_class.is_a?(Class),
          # Metaclass of a metaclass: keeps the one-level class
          # pointer and must neither loop nor recreate on each call.
          MCA.singleton_class.singleton_class.to_s,
          MCA.singleton_class.singleton_class.equal?(MCA.singleton_class.singleton_class),
        ]
        "#,
    );
}
