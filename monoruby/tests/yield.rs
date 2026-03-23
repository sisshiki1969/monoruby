extern crate monoruby;
use monoruby::tests::*;

#[test]
fn test_yield_in_block() {
    run_test(
        "
        class C
          def f
            x = 0
            7.times { x += yield }
            x
          end
        end

        a = 42
        c = C.new

        c.f { a }
        ",
    );
    run_test(
        "
        class C
          def f
            x = 0
            10.times { x += yield }
            x
          end
        end

        @a = 42
        c = C.new

        c.f { @a }
        ",
    );
}

#[test]
fn test_yield_in_loop() {
    run_test_with_prelude(
        r#"
        m{}
        "#,
        r#"
        def m
            i = 0
            while i<30
              i += 1
              yield
            end
        end
        "#,
    );
}

#[test]
fn test_yield_over_fiber() {
    run_test_with_prelude(
        r#"
        method_with_block { |x| puts x }
        "#,
        r#"
        def method_with_block
          Fiber.new {
            [1,2].each { |x| yield x }  # yield が Fiber 境界を越える
          }.resume
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        foo { puts "hello" }
        "#,
        r#"
        def foo(&blk)
            Fiber.new { blk.call }
            .resume
        end
        "#,
    );
}

#[test]
fn test_yield_over_nested_fibers() {
    run_test_with_prelude(
        r#"
        nested_yield { |x| puts x }
        "#,
        r#"
        def nested_yield
          Fiber.new {
            Fiber.new {
              yield 42
            }.resume
          }.resume
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        foo { |x| puts x }
        "#,
        r#"
        def foo(&blk)
          Fiber.new {
            Fiber.new {
              blk.call("deep")
            }.resume
          }.resume
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        deep_yield { |x| puts x }
        "#,
        r#"
        def deep_yield
          Fiber.new {
            Fiber.new {
              Fiber.new {
                yield 99
              }.resume
            }.resume
          }.resume
        end
        "#,
    );
}
