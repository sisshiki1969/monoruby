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
fn yield_in_detached_context() {
    run_test_with_prelude(
        r#"
        foo { |x| x * 2 }.call(5)
        "#,
        r#"
        def foo(&b)
          b
        end
        "#,
    );

    run_test_with_prelude(
        r#"
      a = []
      foo { |x| a.push(x) }
      a
    "#,
        r#"
    	def foo(&block)
        ["a","b"].each { |e| block.call(e) }
	    end
    "#,
    );
}

#[test]
fn block_arg_with_live_float() {
    // Regression: materializing the block parameter into a Proc (`p = blk`,
    // a BlockArg) while a float accumulator is live in the JIT's FP pool must
    // preserve the float across the runtime call (the pool is saved/restored
    // around it). Before that, the method bailed out of JIT compilation.
    run_test(
        r#"
        def with_block(&blk)
          acc = 0.0
          j = 0
          while j < 200
            acc += j.to_f * 1.25
            p = blk
            acc += 2.0 if p.nil?
            j += 1
          end
          acc
        end
        with_block
        "#,
    );
}

#[test]
fn opt_eq_cmp_with_live_float() {
    // Regression: a polymorphic `==` (receiver class varies -> the inline
    // opt_eq_cmp with a generic C-call slow path) must preserve a live FP pool
    // across the slow-path call.
    run_test(
        r#"
        def calc(n, vals)
          a = 0.0; b = 1.0; c = 2.0; d = 3.0
          i = 0
          while i < n
            a += i.to_f * 0.5
            b += a * 0.25
            c += b * 0.125
            d += c * 0.0625
            v = vals[i % 5]
            a -= 1.0 if (v == nil)
            i += 1
          end
          a + b + c + d
        end
        calc(200, [1, nil, :s, "x", 2.0])
        "#,
    );
}
