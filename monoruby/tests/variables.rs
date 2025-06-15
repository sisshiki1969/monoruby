extern crate monoruby;
use monoruby::tests::*;

#[test]
fn instance_var() {
    run_test("@a=42; @a");
    run_test("@a=42; @a = @a * 2; @a");
    run_test("@a=42; b = @a * 2; b");
    run_test("@a=42; c = b = @a * 2; c");
    run_test("@a = 10; @a += 15; @a");
    run_test_with_prelude(
        r###"
        x = C.new
        x.ivar
        "###,
        r###"
        class C
          def initialize
            @a = 1
            @b = 2
            @c = 3
            @d = 4
            @e = 5
            @f = 6
            @g = 7
            @h = 8
          end
          def ivar
            [@a, @b, @c, @d, @e, @f, @g, @h]
          end
        end
        "###,
    );
    run_test_with_prelude(
        r###"
        x = C.new
        x.ivar
        "###,
        r###"
        class C < Array
          def initialize
            @a = 1
            @b = 2
            @c = 3
            @d = 4
            @e = 5
            @f = 6
            @g = 7
            @h = 8
          end
          def ivar
            [@a, @b, @c, @d, @e, @f, @g, @h]
          end
        end
        "###,
    );
}

#[test]
fn class_variables1() {
    run_test_once(
        r#"
            class Foo
              @@foo = 1
              def foo
                @@foo
              end
            end

            class Bar < Foo
              @@foo += 1          # => 2
            end

            class Baz < Bar
              def baz
                @@foo += 1
              end
            end

            $res = []
            20.times do
              $res << Baz.new.baz
              $res << Baz.new.foo
            end
            $res
            "#,
    );
}

#[test]
fn class_variables2() {
    run_test_once(
        r#"
            module Foo
              @@foo = 1
              def foo
                @@foo
              end
            end

            class Bar
              include Foo
              @@foo += 1          # => 2
            end

            class Baz
              include Foo
              def baz
                @@foo += 1
              end
            end

            $res = []
            20.times do
              $res << Baz.new.baz
              $res << Baz.new.foo
            end
            $res
            "#,
    );
}

#[test]
fn class_variables3() {
    run_test_error(
        r#"
            class Foo
            end

            class Bar < Foo
              @@v = :bar
            end

            class Foo
              @@v = :foo
            end

            class Bar
              p @@v       #=> RuntimeError (class variable @@v of Bar is overtaken by Foo)
            end
        "#,
    );
    run_test_error(
        r#"
            @@x = 1
        "#,
    );
    run_test_error(
        r#"
            class C
              @@x
            end
        "#,
    );
    run_test_error(
        r#"
            class C
              def foo
                @@x
              end
            end
            C.new.foo
        "#,
    );
}

#[test]
fn class_variables4() {
    run_test(
        r##"
            class C
              @@x ||= 100
              @@x
            end
        "##,
    );
}

#[test]
fn constant1() {
    run_test_with_prelude(
        r#"
          C::D::E = 100
          C::D.e
          "#,
        r#"
          class C
            class D
              E = 1
              def self.e
                E
              end
            end
          end
      "#,
    );
    run_test(
        r#"
          ::E = 100
          Object::E
            "#,
    );
    run_test(
        r##"
            class C
              D ||= 100
              D
            end
        "##,
    );
}

#[test]
fn constant2() {
    run_test(
        r#"
            Const=4
            Const+=100
            a = Const
            Const
        "#,
    );
}

#[test]
fn constant3() {
    run_test2(
        r#"
            CONST = 5.7
            sum = 0
            for i in 0..19 do
                sum += CONST
                CONST = 1000 if i == 12
            end
            sum
        "#,
    );
}

#[test]
fn constant4() {
    run_test_with_prelude(
        r#"
            $res = []
            A.new # => 100
            B.new # => 200
            $res
            "#,
        r#"
            module M
              def initialize
                $res << self.class::C
              end
            end

            class A
              include M
              C = 100
            end

            class B
              include M
              C = 200
            end
        "#,
    );
}

#[test]
fn global_var() {
    run_test(
        r#"
        a = []
        a << $std
        $std = 42
        a << $std
        $std = nil
        a
        "#,
    )
}
