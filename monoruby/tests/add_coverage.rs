extern crate monoruby;
use monoruby::tests::*;

//
// Tests for method visibility (exercises MethodTableEntry::is_public/is_private/is_public_protected)
//

#[test]
fn visibility_respond_to() {
    run_test(
        r#"
        class C
          def pub_method; end
          private
          def priv_method; end
          protected
          def prot_method; end
        end
        c = C.new
        [
          c.respond_to?(:pub_method),
          c.respond_to?(:priv_method),
          c.respond_to?(:priv_method, true),
          c.respond_to?(:prot_method),
          c.respond_to?(:prot_method, true),
        ]
        "#,
    );
}

#[test]
fn visibility_method_defined() {
    run_test(
        r#"
        class A
          def method1; end
          protected
          def protected1; end
          private
          def private1; end
        end
        class B < A
          def method2; end
        end
        [
          B.method_defined?(:method1),
          B.method_defined?(:method2),
          B.method_defined?(:method2, false),
          B.method_defined?(:method1, false),
          B.public_method_defined?(:method1),
          B.private_method_defined?(:private1),
          B.protected_method_defined?(:protected1),
          B.public_method_defined?(:private1),
          B.public_method_defined?(:protected1),
          B.private_method_defined?(:method1),
          B.protected_method_defined?(:method1),
        ]
        "#,
    );
}

#[test]
fn visibility_private_call() {
    run_test(
        r#"
        class C
          def call_priv
            priv_method
          end
          private
          def priv_method
            42
          end
        end
        C.new.call_priv
        "#,
    );
    run_test_error(
        r#"
        class C
          private
          def priv_method; 42; end
        end
        C.new.priv_method
        "#,
    );
}

#[test]
fn visibility_protected_call() {
    run_test(
        r#"
        class C
          def call_prot(other)
            other.prot_method
          end
          protected
          def prot_method
            99
          end
        end
        c1 = C.new
        c2 = C.new
        c1.call_prot(c2)
        "#,
    );
    // protected methods called from outside should error,
    // but monoruby does not enforce this yet.
}

#[test]
fn visibility_send_private() {
    run_test(
        r#"
        class C
          private
          def secret; 777; end
        end
        C.new.send(:secret)
        "#,
    );
}

//
// Tests for ancestors (exercises Store::ancestors)
//

#[test]
fn ancestors_basic() {
    run_test(
        r#"
        module M1; end
        module M2; end
        class A; end
        class B < A
          include M1
          include M2
        end
        (B.ancestors - Object.ancestors).map(&:to_s)
        "#,
    );
}

#[test]
fn ancestors_with_include_chain() {
    run_test(
        r#"
        module M1; end
        module M2; end
        class A
          include M1
        end
        class B < A
          include M2
        end
        (B.ancestors - Object.ancestors).map(&:to_s)
        "#,
    );
}

#[test]
fn ancestors_deep_chain() {
    run_test(
        r#"
        module M1; end
        module M2
          include M1
        end
        module M3
          include M2
        end
        class A
          include M3
        end
        class B < A; end
        class C < B; end
        (C.ancestors - Object.ancestors).map(&:to_s)
        "#,
    );
}

//
// Tests for constant lookup (exercises ConstSiteInfo and const access paths)
//

#[test]
fn const_nested_access() {
    run_test_with_prelude(
        r#"
        A::B::C::VAL
        "#,
        r#"
        module A
          module B
            module C
              VAL = 42
            end
          end
        end
        "#,
    );
}

#[test]
fn const_toplevel_access() {
    run_test_with_prelude(
        r#"
        [C.get_local, C.get_toplevel]
        "#,
        r#"
        TOP_CONST = 999
        class C
          TOP_CONST = 111
          def self.get_local; TOP_CONST; end
          def self.get_toplevel; ::TOP_CONST; end
        end
        "#,
    );
}

#[test]
fn const_dynamic_access() {
    run_test_with_prelude(
        r#"
        [A, B].map { |c| c::X }
        "#,
        r#"
        class A
          X = 10
        end
        class B
          X = 20
        end
        "#,
    );
}

#[test]
fn const_path_assign_dynamic_base() {
    // Constant assignment whose path prefix is a runtime expression rather
    // than a static constant chain. Single assignment goes through the
    // constant-path write path; a multiple-assignment target goes through
    // `lower_assign_target` (the branch this test guards).
    run_test(
        r##"
        res = []
        m = Module.new
        (m)::X = 7
        res << m::X
        # Multiple-assignment const-path targets with a non-constant base.
        (m)::A, (m)::B = 1, 2
        res << [m::A, m::B]
        x, (m)::C = 10, 20
        res << [x, m::C]
        (m::D, m::E), c = [:d, :e], nil
        res << [m::D, m::E, c]
        # ...and with a static constant-chain base.
        Fo = Module.new
        Fo::P, Fo::Q = 3, 4
        res << [Fo::P, Fo::Q]
        (Fo::R, Fo::S), d = [5, 6], 7
        res << [Fo::R, Fo::S, d]
        # ...and a nested path already rooted on a non-constant base.
        m::N = Module.new
        (m)::N::Deep, e = 8, 9
        res << [m::N::Deep, e]
        res
        "##,
    );
}

#[test]
fn const_inherited() {
    run_test_with_prelude(
        r#"
        Child::INHERITED
        "#,
        r#"
        class Parent
          INHERITED = 42
        end
        class Child < Parent
        end
        "#,
    );
}

//
// Tests for case/when optimization (exercises OptCaseInfo::find)
//

#[test]
fn case_opt() {
    run_tests(&[
        r#"
        res = []
        [-1, 0, 1, 2, 3, 100].each do |x|
          res << case x
          when 0 then "zero"
          when 1 then "one"
          when 2 then "two"
          when 3 then "three"
          else "other"
          end
        end
        res
        "#,
        r#"
        res = []
        [0, 5, 10, 15, 20, 25, -1, 100].each do |x|
          res << case x
          when 0 then "a"
          when 5 then "b"
          when 10 then "c"
          when 15 then "d"
          when 20 then "e"
          else "f"
          end
        end
        res
        "#,
        r#"
        res = []
        ["a", 1, :sym, nil, true].each do |x|
          res << case x
          when 1 then "one"
          when "a" then "str_a"
          when :sym then "symbol"
          when nil then "nil"
          when true then "true"
          else "other"
          end
        end
        res
        "#,
    ]);
}

// A `break` out of a block lowers to the `BlockBreak` AsmInst (a non-local
// exit through `err_block_break`). Each method's block is exercised enough to
// JIT, so the break path runs through native code on both the x86-64 `amd64`
// job and the native arm64 `darwin` job. Covers conditional and unconditional
// breaks, breaks carrying a value, and `each`/`times`/range iterators.
#[test]
fn block_break_jit() {
    run_test(
        r#"
        def first_even(a); a.each { |x| break x if x % 2 == 0 }; end
        def take_first(a); a.each { |x| break x * 10 }; end
        def upto_three(n); (0...n).each { |i| break i if i == 3 }; end
        def sum_until(n); r = 0; n.times { |i| break if i == 5; r += i }; r; end
        [
          first_even([1, 3, 8, 9]),
          first_even([1, 3, 5]),
          take_first([5, 6, 7]),
          upto_three(10),
          sum_until(10),
        ]
        "#,
    );
}

// A dense (>= 8 integer `when`) `case` lowers to the `OptCase` jump-table
// bytecode rather than an `===` chain. Wrapping it in a method exercised many
// times forces the JIT to compile the `OptCase` AsmInst (covered on both the
// x86-64 `amd64` job and the native arm64 `darwin` job). Covers below-min,
// in-range, boundaries, above-max -> else, a non-zero `min`, and several
// `when` values mapping to one body.
#[test]
fn case_opt_jit() {
    run_test(
        r#"
        def classify(n)
          case n
          when 0 then "z0"
          when 1 then "z1"
          when 2 then "z2"
          when 3 then "z3"
          when 4 then "z4"
          when 5 then "z5"
          when 6 then "z6"
          when 7 then "z7"
          when 8 then "z8"
          when 9 then "z9"
          else "other"
          end
        end
        (-3..13).map { |n| classify(n) }
        "#,
    );
    run_test(
        r#"
        def bucket(n)
          case n
          when 10, 12, 14 then :low
          when 11, 13, 15 then :mid
          when 16, 17, 18, 19 then :high
          else :none
          end
        end
        (8..21).map { |n| bucket(n) }
        "#,
    );
}

//
// Tests for call site patterns (exercises CallSiteInfo methods)
//

#[test]
fn callsite_func_call() {
    run_test(
        r#"
        def f(x)
          x * 2
        end
        f(21)
        "#,
    );
}

#[test]
fn callsite_splat_with_kw() {
    run_test_with_prelude(
        r#"
        args = [1, 2, 3]
        f(*args, a: 10, b: 20)
        "#,
        r#"
        def f(*pos, a:, b:)
          [pos, a, b]
        end
        "#,
    );
}

#[test]
fn callsite_hash_splat_with_kw() {
    run_test_with_prelude(
        r#"
        kw = {a: 1, b: 2}
        f(10, **kw, c: 3)
        "#,
        r#"
        def f(x, a:, b:, c:)
          [x, a, b, c]
        end
        "#,
    );
}

#[test]
fn callsite_block_and_block_arg() {
    run_test_with_prelude(
        r#"
        $res = []
        f { 100 }
        p = Proc.new { 200 }
        f(&p)
        $res
        "#,
        r#"
        def f(&blk)
          $res << blk.call
        end
        "#,
    );
}

#[test]
fn callsite_send_with_splat() {
    run_test_with_prelude(
        r#"
        args = [1, 2, 3]
        C.new.send(:foo, *args)
        "#,
        r#"
        class C
          def foo(*x)
            x
          end
        end
        "#,
    );
}

#[test]
fn callsite_no_args() {
    run_test_with_prelude(
        r#"
        f
        "#,
        r#"
        def f
          42
        end
        "#,
    );
}

#[test]
fn callsite_kw_only() {
    run_test_with_prelude(
        r#"
        f(a: 10, b: 20)
        "#,
        r#"
        def f(a:, b:)
          a + b
        end
        "#,
    );
}

//
// Tests for method cache invalidation (exercises GlobalMethodCache)
//

#[test]
fn cache_invalidation_reopen_class() {
    run_test_once(
        r#"
        class C
          def foo; 1; end
        end
        res = []
        c = C.new
        10.times { res << c.foo }
        class C
          def foo; 2; end
        end
        10.times { res << c.foo }
        res
        "#,
    );
}

#[test]
fn cache_invalidation_include_module() {
    run_test_once(
        r#"
        module M
          def bar; 100; end
        end
        class C
          def bar; 1; end
        end
        c = C.new
        res = []
        10.times { res << c.bar }
        class C
          prepend M
        end
        10.times { res << c.bar }
        res
        "#,
    );
}

#[test]
fn cache_invalidation_define_method() {
    run_test_once(
        r#"
        class C
          def foo; 10; end
        end
        c = C.new
        res = []
        5.times { res << c.foo }
        class C
          define_method(:foo) { 20 }
        end
        5.times { res << c.foo }
        res
        "#,
    );
}

//
// Tests for super with various patterns
//

#[test]
fn super_no_superclass_method() {
    // A `super` with no superclass method raises a `super`-specific
    // NoMethodError, not the ordinary "undefined method" message.
    run_test_with_prelude(
        r##"
        begin
          B.new.foo
        rescue NoMethodError => e
          # Only the `super` prefix is portable; the receiver rendering
          # differs from CRuby, so don't compare the whole message.
          e.message.start_with?("super: no superclass method")
        end
        "##,
        r##"
        class A; end
        class B < A
          def foo; super; end
        end
        "##,
    );
}

#[test]
fn class_module_reopen_type_error() {
    // Reopening a constant with the wrong keyword names the *constant*
    // (`X is not a class` / `is not a module`) and rejects class<->module.
    run_test(
        r##"
        Nc = 1
        module RM1; end
        RM2 = Class.new
        msgs = []
        # CRuby appends a "previous definition" line; compare only the prefix.
        begin; class Nc; end;             rescue TypeError => e; msgs << e.message.start_with?("Nc is not a class"); end
        begin; eval("class RM1; end");    rescue TypeError => e; msgs << e.message.start_with?("RM1 is not a class"); end
        begin; eval("module RM2; end");   rescue TypeError => e; msgs << e.message.start_with?("RM2 is not a module"); end
        # same-kind reopen is fine
        class GoodC; end; class GoodC; X = 1; end
        module GoodM; end; module GoodM; Y = 2; end
        msgs << [GoodC::X, GoodM::Y]
        msgs
        "##,
    );
}

#[test]
fn singleton_class_not_instantiable() {
    // A singleton class (metaclass) cannot be instantiated: both `new`
    // and `allocate` raise TypeError, even though it inherits an
    // allocator through its attached object's class.
    run_test(
        r##"
        o = Object.new
        msgs = []
        begin; o.singleton_class.new;      rescue TypeError => e; msgs << e.message; end
        begin; o.singleton_class.allocate; rescue TypeError => e; msgs << e.message; end
        # Ordinary classes still allocate normally.
        msgs << String.new("ok")
        msgs << Class.new.new.is_a?(Object)
        msgs
        "##,
    );
}

#[test]
fn singleton_class_frozen_propagation() {
    // The singleton class of a frozen object is frozen; freezing an object
    // freezes its already-created singleton class.
    run_test(
        r##"
        res = []
        o = Object.new
        o.freeze
        res << o.singleton_class.frozen?          # created from a frozen object
        o2 = Object.new
        sc = o2.singleton_class
        res << sc.frozen?                          # not yet frozen
        o2.freeze
        res << sc.frozen?                          # freezing the object freezes it
        # clone(freeze: false) yields an unfrozen singleton class
        a = Object.new
        a.freeze
        res << a.clone(freeze: false).singleton_class.frozen?
        res
        "##,
    );
}

#[test]
fn super_with_block() {
    run_test_with_prelude(
        r##"
        $res = []
        D.new.f(1,2) { |x| x * 10 }
        $res
        "##,
        r##"
        class C
          def f(a,b,&blk)
            $res << [a, b, blk.call(a)]
          end
        end
        class D < C
          def f(a,b,&blk)
            super
          end
        end
        "##,
    );
}

#[test]
fn super_no_args() {
    run_test_with_prelude(
        r##"
        $res = []
        D.new.f(1, 2, 3)
        $res
        "##,
        r##"
        class C
          def f(a, b, c)
            $res << [a, b, c]
          end
        end
        class D < C
          def f(a, b, c)
            a = a * 10
            super
          end
        end
        "##,
    );
}

#[test]
fn super_in_module() {
    run_test(
        r#"
        module M
          def greet
            "M:" + super
          end
        end
        class Base
          def greet
            "Base"
          end
        end
        class Child < Base
          include M
        end
        Child.new.greet
        "#,
    );
}

#[test]
fn super_with_explicit_args() {
    run_test_with_prelude(
        r##"
        D.new.f(10, 20)
        "##,
        r##"
        class C
          def f(a, b)
            a + b
          end
        end
        class D < C
          def f(a, b)
            super(a * 2, b * 3)
          end
        end
        "##,
    );
}

#[test]
fn super_with_splat_args() {
    run_test_with_prelude(
        r##"
        D.new.f(1, 2, 3, 4, 5)
        "##,
        r##"
        class C
          def f(*args)
            args
          end
        end
        class D < C
          def f(*args)
            super(*args)
          end
        end
        "##,
    );
}

#[test]
fn super_with_kw_args() {
    run_test_with_prelude(
        r##"
        $res = []
        D.new.f(a: 10, b: 20)
        $res
        "##,
        r##"
        class C
          def f(a:, b:)
            $res << [a, b]
          end
        end
        class D < C
          def f(a:, b:)
            super(a: a * 2, b: b * 3)
          end
        end
        "##,
    );
}

#[test]
fn super_with_kw_rest() {
    run_test_with_prelude(
        r##"
        $res = []
        D.new.f(a: 1, b: 2, c: 3)
        $res
        "##,
        r##"
        class C
          def f(**kw)
            $res << kw.sort
          end
        end
        class D < C
          def f(**kw)
            super
          end
        end
        "##,
    );
}

#[test]
fn super_no_args_modified_locals() {
    run_test_with_prelude(
        r##"
        $res = []
        D.new.f(1, 2)
        $res
        "##,
        r##"
        class C
          def f(a, b)
            $res << [a, b]
          end
        end
        class D < C
          def f(a, b)
            a = a * 100
            b = b * 200
            super
          end
        end
        "##,
    );
}

#[test]
fn super_in_nested_block() {
    run_test_with_prelude(
        r##"
        $res = []
        D.new.f(42)
        $res
        "##,
        r##"
        class C
          def f(x)
            $res << x
          end
        end
        class D < C
          def f(x)
            3.times do
              2.times do
                super
              end
            end
          end
        end
        "##,
    );
}

#[test]
fn super_chain_three_levels() {
    run_test(
        r#"
        class A
          def foo(x)
            x + 1
          end
        end
        class B < A
          def foo(x)
            super(x * 2)
          end
        end
        class C < B
          def foo(x)
            super(x + 10)
          end
        end
        C.new.foo(5)
        "#,
    );
}

#[test]
fn super_with_optional_args() {
    run_test_with_prelude(
        r##"
        $res = []
        D.new.f(1)
        D.new.f(1, 2)
        D.new.f(1, 2, 3)
        $res
        "##,
        r##"
        class C
          def f(a, b=10, c=20)
            $res << [a, b, c]
          end
        end
        class D < C
          def f(a, b=100, c=200)
            super
          end
        end
        "##,
    );
}

#[test]
fn super_with_rest_and_kw() {
    run_test_with_prelude(
        r##"
        $res = []
        D.new.f(1, 2, 3, x: 10, y: 20)
        $res
        "##,
        r##"
        class C
          def f(*args, **kw)
            $res << [args, kw.sort]
          end
        end
        class D < C
          def f(*args, **kw)
            super
          end
        end
        "##,
    );
}

#[test]
fn super_with_hash_splat() {
    run_test_with_prelude(
        r##"
        $res = []
        h = {a: 1, b: 2}
        D.new.f(**h)
        $res
        "##,
        r##"
        class C
          def f(a:, b:)
            $res << [a, b]
          end
        end
        class D < C
          def f(**kw)
            super(**kw)
          end
        end
        "##,
    );
}

#[test]
fn super_defined() {
    run_test(
        r#"
        class A
          def foo; end
        end
        class B < A
          def foo
            defined?(super)
          end
        end
        class C
          def bar
            defined?(super)
          end
        end
        [B.new.foo, C.new.bar]
        "#,
    );
}

#[test]
fn super_in_initialize() {
    run_test(
        r#"
        class A
          def initialize(x)
            @x = x
          end
          def get_x; @x; end
        end
        class B < A
          def initialize(x, y)
            super(x)
            @y = y
          end
          def get_y; @y; end
        end
        b = B.new(10, 20)
        [b.get_x, b.get_y]
        "#,
    );
}

#[test]
fn super_method_visibility_inherited() {
    run_test(
        r#"
        class A
          private
          def secret
            42
          end
        end
        class B < A
          public
          def secret
            super
          end
        end
        B.new.secret
        "#,
    );
}

#[test]
fn super_with_multiple_modules() {
    run_test(
        r#"
        module M1
          def foo
            "M1:" + super
          end
        end
        module M2
          def foo
            "M2:" + super
          end
        end
        class Base
          def foo
            "Base"
          end
        end
        class Child < Base
          include M1
          include M2
        end
        Child.new.foo
        "#,
    );
}

#[test]
fn super_with_prepend() {
    run_test(
        r#"
        module M
          def foo
            "M:" + super
          end
        end
        class C
          prepend M
          def foo
            "C"
          end
        end
        C.new.foo
        "#,
    );
}

#[test]
fn super_return_value() {
    run_test(
        r#"
        class A
          def calc(x)
            x * 2
          end
        end
        class B < A
          def calc(x)
            result = super(x + 1)
            result + 100
          end
        end
        B.new.calc(5)
        "#,
    );
}

#[test]
fn super_noarg_with_block_param() {
    run_test_with_prelude(
        r##"
        $res = []
        D.new.f(1, 2) { |x| x * 5 }
        $res
        "##,
        r##"
        class C
          def f(a, b, &blk)
            $res << [a, b, blk.call(a + b)]
          end
        end
        class D < C
          def f(a, b, &blk)
            a = a * 10
            super
          end
        end
        "##,
    );
}

#[test]
fn super_error_no_superclass() {
    run_test_error(
        r#"
        class C
          def foo
            super
          end
        end
        C.new.foo
        "#,
    );
}

//
// Tests for block in various contexts (exercises func_description, block iseq creation)
//

#[test]
fn nested_blocks() {
    run_test(
        r#"
        res = []
        3.times do |i|
          2.times do |j|
            1.times do |k|
              res << [i, j, k]
            end
          end
        end
        res
        "#,
    );
}

#[test]
fn block_closure() {
    run_test(
        r#"
        def make_counter
          count = 0
          inc = Proc.new { count += 1; count }
          get = Proc.new { count }
          [inc, get]
        end
        inc, get = make_counter
        5.times { inc.call }
        get.call
        "#,
    );
}

#[test]
fn block_as_lambda() {
    run_test(
        r#"
        f = lambda { |x, y| x + y }
        f.call(3, 4)
        "#,
    );
}

//
// Tests for eval (exercises Store::new_eval)
//

#[test]
fn eval_basic() {
    run_test(
        r#"
        eval("1 + 2 + 3")
        "#,
    );
}

#[test]
fn eval_with_binding() {
    run_test(
        r#"
        x = 10
        eval("x * 5")
        "#,
    );
}

//
// Tests for class method / singleton method (exercises get_class_name, func_description)
//

#[test]
fn singleton_method() {
    run_test(
        r#"
        class C
          def self.class_method
            42
          end
        end
        C.class_method
        "#,
    );
}

#[test]
fn nested_class_name() {
    run_test(
        r#"
        module Outer
          class Inner
            def self.name_test
              self.to_s
            end
          end
        end
        Outer::Inner.name_test
        "#,
    );
}

#[test]
fn multiple_inheritance_method_lookup() {
    run_test(
        r#"
        module M1
          def who; "M1"; end
        end
        module M2
          def who; "M2"; end
        end
        class A
          include M1
          include M2
        end
        A.new.who
        "#,
    );
}

//
// Tests for attr_reader/attr_writer (exercises new_attr_reader, new_attr_writer)
//

#[test]
fn attr_writer_only() {
    run_test(
        r#"
        class C
          attr_writer :x
          def get_x; @x; end
        end
        c = C.new
        c.x = 42
        c.get_x
        "#,
    );
}

//
// Tests for proc method (exercises new_proc_method)
//

#[test]
fn proc_to_method() {
    run_test(
        r#"
        class C
          define_method(:add) { |a, b| a + b }
        end
        C.new.add(3, 4)
        "#,
    );
}

//
// Tests for error handling paths
//

#[test]
fn method_missing_error() {
    run_test_error(
        r#"
        class C; end
        C.new.nonexistent_method
        "#,
    );
}

#[test]
fn wrong_arity_builtin() {
    run_test_error(
        r#"
        1.send
        "#,
    );
}

#[test]
fn const_not_found() {
    run_test_error(
        r#"
        NONEXISTENT_CONSTANT
        "#,
    );
}

//
// Tests for complex call patterns
//

#[test]
fn mixed_args_complex() {
    run_test_with_prelude(
        r#"
        f(1, 2, 3, x: 10, y: 20) { |v| v * 2 }
        "#,
        r#"
        def f(a, *rest, x:, y:, &blk)
          [a, rest, x, y, blk.call(a)]
        end
        "#,
    );
}

#[test]
fn method_with_all_param_types() {
    run_test_with_prelude(
        r#"
        f(1, 2, 3, 4, 5, 6, a: 10, b: 20, z: 99) { 42 }
        "#,
        r#"
        def f(req1, req2, opt1=0, opt2=0, *rest, post1, post2, a:, b:, **kw, &blk)
          [req1, req2, opt1, opt2, rest, post1, post2, a, b, kw, blk.call]
        end
        "#,
    );
}

#[test]
fn caller_reports_suspended_lines() {
    // Each backtrace entry shows the line the frame is currently
    // executing (its pc saved in the callee's cont-frame slot), not
    // the method's first line — including inside rescue/ensure.
    run_test_once(
        r##"
        def crsl_foo
          begin
            raise "oops"
          rescue
            return caller(0, 2).map { |s| s[/:(\d+):/, 1] }
          end
        end
        def crsl_bar
          crsl_foo
        end
        crsl_bar
        "##,
    );
}

#[test]
fn caller_lines_through_specialized_jit_calls() {
    // A specialized JIT call skips the eager cont-frame pc store; the
    // pc recorded in the deopt table is materialized lazily when a
    // reader (Kernel#caller here) observes the frame. run_test warms
    // the JIT, so the hot caller is specialized when observed.
    run_test(
        r##"
        def cljc_leaf
          ls = caller(0, 3).map { |s| s[/:(\d+):/, 1].to_i }
          ls.map { |l| l - ls[0] }
        end
        def cljc_mid
          cljc_leaf
        end
        $r = []
        20.times do
          $r << cljc_mid
        end
        $r.uniq
        "##,
    );
}

#[test]
fn caller_lines_survive_specialized_frame_eviction() {
    // Redefining a basic op from inside the callee evicts the
    // suspended specialized caller frames (immediate_eviction): the
    // deopt path must lazily write the recorded call-site pc into the
    // cont-frame slot, so caller() lines stay correct afterwards.
    run_test(
        r##"
        class CLSE_Probe; end
        def clse_leaf(i)
          if i == 15
            Float.class_eval do
              def %(o)
                42
              end
            end
          end
          ls = caller(0, 3).map { |s| s[/:(\d+):/, 1].to_i }
          ls.map { |l| l - ls[0] }
        end
        def clse_mid(i)
          clse_leaf(i)
        end
        $r = []
        20.times do |i|
          $r << clse_mid(i)
        end
        $r.uniq
        "##,
    );
}

#[test]
fn super_uses_called_name_for_multi_name_define_method() {
    // One block body installed under two names via define_method:
    // `super()` must dispatch under the name the method was *called*
    // with, recovered from the caller's callsite via the cont-frame pc
    // (the body's stamped name is whichever define_method ran first).
    // run_test warms the JIT: such super sites must stay uncached /
    // deopt to the VM, or one target would be reused for both names.
    run_test(
        r##"
        sup = Class.new do
          def a; "a"; end
          def b; "b"; end
        end
        sub = Class.new(sup) do
          [:a, :b].each do |name|
            define_method name do
              super()
            end
          end
        end
        r = []
        20.times do
          r << sub.new.a
          r << sub.new.b
        end
        r.uniq
        "##,
    );
}

#[test]
fn super_chains_through_duplicate_method_bodies() {
    // Two anonymous modules built by re-running the same `def` bytecode
    // share one FuncId, so the body occupies two ancestor-chain
    // positions. Each frame's occurrence index (counted over the
    // consecutive super-linked frames via the cont-frame pc) selects
    // the position to continue from — super must visit *both* modules
    // before the base class, on every (JIT-warmed) call.
    // run_test re-executes the snippet in one interpreter; guard the
    // definitions so the chain doesn't grow two modules per repeat.
    run_test(
        r##"
        unless defined?(SCDMB_Twice)
          class SCDMB_Base
            def self.whatever
              mod = Module.new do
                def a(array)
                  array << "anon"
                  super
                end
              end
              include mod
            end
            def a(array)
              array << "non-anon"
            end
          end
          class SCDMB_Twice < SCDMB_Base
            whatever
            whatever
          end
        end
        r = []
        20.times do
          r << SCDMB_Twice.new.a([])
        end
        r.uniq
        "##,
    );
}

#[test]
fn super_from_alias_resolves_from_original_position() {
    // `alias_method` re-registers the body in the aliasing class, but
    // super from the aliased method must resolve from the *original*
    // definition's chain position (the called name maps through the
    // alias entry's original_name) — not re-enter the same body.
    run_test(
        r##"
        class SFAR_A
          def name
            [:alias1]
          end
        end
        class SFAR_B < SFAR_A
          def name
            [:alias2] + super
          end
        end
        class SFAR_C < SFAR_B
          alias_method :name3, :name
        end
        r = []
        20.times do
          r << SFAR_C.new.name3
        end
        r.uniq
        "##,
    );
}
