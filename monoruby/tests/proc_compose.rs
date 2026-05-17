extern crate monoruby;
use monoruby::tests::*;

#[test]
fn proc_compose_forward() {
    run_test(r#"succ = proc { |s| s.succ }; upcase = proc { |s| s.upcase }; (succ >> upcase).call("Ruby")"#);
    run_test("f = proc { |x| x * x }; g = proc { |x| x + x }; [(f >> g).call(2), (g >> f).call(2)]");
    run_test("mul = proc { |n, m| n * m }; inc = proc { |n| n + 1 }; (mul >> inc).call(2, 3)");
}

#[test]
fn proc_compose_backward() {
    run_test(r#"succ = proc { |s| s.succ }; upcase = proc { |s| s.upcase }; (succ << upcase).call("Ruby")"#);
    run_test("f = proc { |x| x * x }; g = proc { |x| x + x }; [(f << g).call(2), (g << f).call(2)]");
    run_test("inc = proc { |n| n + 1 }; mul = proc { |n, m| n * m }; (inc << mul).call(2, 3)");
}

#[test]
fn proc_compose_lambda_ness() {
    // `>>` follows self; `<<` follows the argument (first invoked).
    run_test(
        r#"
        f = -> x { x * x }
        g = proc { |x| x + x }
        l = -> x { x }
        [
          (g >> g).lambda?, (g >> f).lambda?, (f >> g).lambda?,
          (f << g).lambda?, (f << l).lambda?, (g << f).lambda?
        ]
        "#,
    );
}

#[test]
fn proc_compose_accepts_callable_object() {
    run_test(
        r#"
        inc = proc { |n| n + 1 }
        d = Object.new
        def d.call(n); n * 2; end
        [(inc >> d).call(3), (inc << d).call(3)]
        "#,
    );
}

#[test]
fn proc_compose_type_error() {
    run_test_error("proc { |x| x }.>>(Object.new)");
    run_test_error("proc { |x| x }.<<(42)");
}

#[test]
fn proc_compose_passes_block_to_first() {
    run_test(
        r#"
        rec = []
        one = proc { |&arg| arg.call(:one) if arg }
        two = proc { |&arg| arg.call(:two) if arg }
        (one >> two).call { |x| rec << x }
        (one << two).call { |x| rec << x }
        rec
        "#,
    );
}
