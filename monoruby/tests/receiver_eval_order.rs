extern crate monoruby;
use monoruby::tests::*;

// A local read as a receiver / left operand keeps the value it had before
// the later operands ran, even when they reassign it (#1724). The lowerer
// copies such a local into a temp (`Lowerer::lower_operand_before`).

#[test]
fn receiver_reassigned_by_operand() {
    run_test(
        r#"
        x = 1; a = x + (x = 2.5)
        x = 1; b = x.+(x = 2)
        x = 10; c = x.to_s(x = 2)
        arr = [1, 2]; d = arr[(arr = [7]).size]
        s = "a"; e = s.concat((s = "b"))
        x = 1; f = x < (x = 5)
        x = 1; g = (x..(x = 5))
        x = 1; h = x + (x += 3)
        x = 1; i = x + ((x, y = 7, 8); x)
        x = 1; j = x&.+(x = 4)
        [a, b, c, d, e, f, g, h, i, j]
        "#,
    );
}

#[test]
fn receiver_reassigned_by_closure() {
    run_test(
        r#"
        def yield_it = yield
        x = 1; a = x + yield_it { x = 10 }
        x = 1; pr = proc { x = 20 }; b = x + pr.call
        x = 1; pr = proc { x = 30 }; c = x.to_s(pr.call)
        x = 1; d = x + binding.local_variable_set(:x, 40)
        x = 1; e = x + eval("x = 50")
        [a, b, c, d, e]
        "#,
    );
}

#[test]
fn receiver_reassigned_in_method() {
    run_test(
        r#"
        def m1(v)
          r = []
          3.times { |i| r << (v + (v = i)) }
          r
        end
        def m2(x)
          pr = proc { x = 100 }
          y = x + pr.call
          [x, y]
        end
        def m3(x) = x + (x = 2)
        def m4(x)
          r = 0
          i = 0
          while i < 3
            r += x * (x = i + 1)
            i += 1
          end
          r
        end
        [m1(10), m2(1), m3(5), m4(7)]
        "#,
    );
}

// A Float op computing into its own lhs must not do so in place while a
// copy's source still shares the lhs register (#1726), and a local unboxed
// through its copy keeps its value.
#[test]
fn float_op_on_copy_keeps_source() {
    run_test(
        r#"
        def f(n)
          x = 1.5
          r = 0.0
          i = 0
          while i < n
            x = x + 0.25
            y = x
            y = y * 2.0
            r += x + y
            i += 1
          end
          [x, r]
        end
        def g(a, b, n)
          aik = a[0]
          s = 0.0
          j = 0
          while j < n
            s += aik * b[j]
            aik = aik + 1.0 if j == 2
            j += 1
          end
          [aik, s]
        end
        [f(50), g([1.5], [0.5, 1.0, 2.0, 4.0], 4)]
        "#,
    );
}
