extern crate monoruby;
use monoruby::tests::*;

#[test]
fn imaginary() {
    run_test(r#"5i"#);
    run_test(r#"4+5i"#);
}

#[test]
fn lambda() {
    run_test(
        r#"
        j = 42
        f = ->(x){x + j}
        f.call(3)
        "#,
    );
    run_test_with_prelude(
        r#"
            f.call(17)
            "#,
        r#"
            f = ->(x){x + 42}
            "#,
    );
    run_test_once(
        r#"
        j = 42
        a = []
        f = ->(x){x + j}
        for i in 0..30
            a << f.call(3)
        end
        a
        "#,
    );
    run_test_error(
        r#"
            f = ->(a, b){}
            f.call(1)
            "#,
    );
}
