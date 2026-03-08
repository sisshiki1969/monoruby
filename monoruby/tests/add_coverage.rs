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
    run_test(
        r#"
        module A
          module B
            module C
              VAL = 42
            end
          end
        end
        A::B::C::VAL
        "#,
    );
}

#[test]
fn const_toplevel_access() {
    run_test(
        r#"
        TOP_CONST = 999
        class C
          TOP_CONST = 111
          def self.get_local; TOP_CONST; end
          def self.get_toplevel; ::TOP_CONST; end
        end
        [C.get_local, C.get_toplevel]
        "#,
    );
}

#[test]
fn const_dynamic_access() {
    run_test(
        r#"
        class A
          X = 10
        end
        class B
          X = 20
        end
        [A, B].map { |c| c::X }
        "#,
    );
}

#[test]
fn const_inherited() {
    run_test(
        r#"
        class Parent
          INHERITED = 42
        end
        class Child < Parent
        end
        Child::INHERITED
        "#,
    );
}

//
// Tests for case/when optimization (exercises OptCaseInfo::find)
//

#[test]
fn case_opt_negative() {
    run_test(
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
    );
}

#[test]
fn case_opt_large_range() {
    run_test(
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
    );
}

#[test]
fn case_opt_non_integer() {
    run_test(
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
