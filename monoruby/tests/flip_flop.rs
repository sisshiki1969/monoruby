extern crate monoruby;
use monoruby::tests::*;

#[test]
fn flip_flop_inclusive() {
    run_test(
        r#"
        r = []
        10.times { |i| r << i if (i == 4)..(i == 5) }
        r
        "#,
    );
}

#[test]
fn flip_flop_exclusive() {
    run_test(
        r#"
        r = []
        10.times { |i| r << i if (i == 4)...(i == 5) }
        r
        "#,
    );
}

#[test]
fn flip_flop_single_element_inclusive() {
    run_test(
        r#"
        r = []
        10.times { |i| r << i if (i == 4)..(i == 4) }
        r
        "#,
    );
}

#[test]
fn flip_flop_single_element_exclusive() {
    // `...` does not evaluate the end condition on the turn-on
    // evaluation, so `(i == 4)...(i == 4)` stays on for the rest.
    // The site ends this snippet still on, and that state persists
    // into any re-execution of the same site, so run it only once.
    run_test_once(
        r#"
        r = []
        10.times { |i| r << i if (i == 4)...(i == 4) }
        r
        "#,
    );
}

#[test]
fn flip_flop_combined() {
    run_test(
        r#"
        r = []
        10.times { |i| r << i if (i == 4)..(i == 5) or (i == 7)...(i == 8) }
        r
        "#,
    );
}

#[test]
fn flip_flop_state_shared_across_invocations() {
    run_test(
        r#"
        r = []
        store_me = proc { |i| r << i if (i == 4)..(i == 7) }
        store_me[1]
        store_me[4]
        proc { store_me[1] }.call
        store_me[7]
        store_me[5]
        r
        "#,
    );
}

#[test]
fn flip_flop_lazy_end_condition() {
    run_test(
        r#"
        r = []
        calls = 0
        10.times { |i| r << i if (calls += 1; i == 4)..(i == 6) }
        [r, calls]
        "#,
    );
}

#[test]
fn flip_flop_integer_literal_compares_with_last_line_number() {
    // A bare Integer literal operand compares against `$.`, not its
    // own truthiness ($. is nil here, so this can never turn on).
    run_test(
        r#"
        r = []
        10.times { |i| r << i if 4..5 }
        r
        "#,
    );
}

#[test]
fn flip_flop_as_ternary_condition() {
    run_test(
        r#"
        from = 1
        to = 2
        [(from..to ? 3 : 4), (from...to ? 3 : 4)]
        "#,
    );
}
