#[cfg(test)]
mod test {
    use crate::tests::*;

    #[test]
    fn class_variables1() {
        run_test_with_prelude(
            r#"
            Baz.new.foo      # => 3
            "#,
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
              @@foo += 1          # => 3
            end
        "#,
        );
    }

    #[test]
    fn class_variables2() {
        run_test_with_prelude(
            r#"
            Baz.new.foo      # => 3
            "#,
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
              @@foo += 1          # => 3
            end
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
    }
}
