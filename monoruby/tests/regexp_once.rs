extern crate monoruby;
use monoruby::tests::*;

// `/#{...}/o` interpolates and compiles once per site.

#[test]
fn regexp_once_in_block() {
    run_test(
        r#"
        r = []
        3.times { |i| r << /#{i}/o }
        r.map(&:source)
        "#,
    );
}

#[test]
fn regexp_once_in_method() {
    run_test_with_prelude(
        r#"
        [f(1), f(2), g(3), g(4)].map(&:source)
        "#,
        r#"
        def f(x) = /a#{x}/o
        def g(x) = /b#{x}/
        "#,
    );
}

#[test]
fn regexp_once_evaluates_interpolation_once() {
    run_test_once(
        r#"
        $calls = 0
        o = Object.new
        def o.to_s
          $calls += 1
          "x"
        end
        5.times { /#{o}/o }
        [$calls, /#{o}/o.source]
        "#,
    );
}

// CRuby ignores `o` on a regexp literal used as a bare conditional
// (an implicit `=~ $_`): it is interpolated on every evaluation.
#[test]
fn regexp_once_match_last_line() {
    run_test_once(
        r#"
        r = []
        $_ = "a1"
        3.times { |i| r << i if /a#{i + 1}/o }
        r
        "#,
    );
}

#[test]
fn regexp_once_first_evaluation_wins() {
    run_test_once(
        r#"
        r = []
        5.times { |i| r << /#{i}/o if i >= 2 }
        r.map(&:source)
        "#,
    );
}
