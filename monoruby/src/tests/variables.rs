#[cfg(test)]
mod test {
    use crate::tests::*;

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
}
