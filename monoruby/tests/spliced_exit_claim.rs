//! A spliced non-local exit (issue #1185) carries what the exit's own
//! state claimed about its value — its class, or the constant it is — to
//! the target's continuation, exactly as a plain specialized `break` /
//! `return` does through `push_return_context`.
//!
//! The value the delivering `EnsureEnd` hands over is the very one the
//! exit left with (it rides the deferral unchanged), so the claim the
//! exit's state made holds at delivery; only the invariants are taken
//! from the delivering host, since the `ensure` bodies run in between. It
//! used to be `as_return_any` — no claim at all — which made every
//! spliced exit's join with the normal return path collapse to `Value`.
//!
//! These shapes make the normal return and the spliced exit agree, so the
//! join keeps the claim and the continuation acts on it; a wrong claim —
//! a constant folded that the runtime value does not equal, a class the
//! value is not — would show as a wrong answer against CRuby.
extern crate monoruby;
use monoruby::tests::*;

/// Both paths return the same literal: the target sees a compile-time
/// constant and folds it, with no machine store of the delivered value.
#[test]
fn a_constant_break_joins_with_a_constant_normal_return() {
    run_test(
        r#"
        $n = 0
        def inner(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $n += 1
          end
        end
        def outer(arr)
          inner(arr) { |x| yield x }
          7
        end
        def find(arr, t)
          v = outer(arr) { |x| break 7 if x == t }
          v + 1
        end
        r = []
        40.times { |i| r << find([1, 2, 3], i % 4 + 1) }
        [r.uniq, $n]
    "#,
    );
}

/// Both paths return an Integer: the target learns the class and adds
/// without a class guard.
#[test]
fn an_integer_break_joins_with_an_integer_normal_return() {
    run_test(
        r#"
        $n = 0
        def inner(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $n += 1
          end
        end
        def outer(arr)
          inner(arr) { |x| yield x }
          0
        end
        def find(arr, t)
          outer(arr) { |x| break x * 10 if x == t } + 1
        end
        r = []
        40.times { |i| r << find([1, 2, 3], i % 4 + 1) }
        [r.uniq.sort, $n]
    "#,
    );
}

/// Both paths return a Float: the class arrives, the value boxed.
#[test]
fn a_float_break_joins_with_a_float_normal_return() {
    run_test(
        r#"
        $n = 0
        def inner(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $n += 1
          end
        end
        def outer(arr)
          inner(arr) { |x| yield x }
          0.5
        end
        def find(arr, t)
          outer(arr) { |x| break x * 1.5 if x == t } + 0.25
        end
        s = 0.0
        40.times { |i| s += find([1, 2, 3], i % 4 + 1) }
        [s, $n]
    "#,
    );
}

/// The paths disagree (Integer vs Float): the join must fall to `Value`,
/// and the continuation must still handle both.
#[test]
fn a_disagreeing_break_falls_to_value() {
    run_test(
        r#"
        $n = 0
        def inner(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $n += 1
          end
        end
        def outer(arr)
          inner(arr) { |x| yield x }
          0
        end
        def find(arr, t)
          outer(arr) { |x| break x * 1.5 if x == t } + 1
        end
        r = []
        40.times { |i| r << find([1, 2, 3], i % 4 + 1) }
        [r.uniq.sort, $n]
    "#,
    );
}

/// A non-local `return`: the home method's own `return` and the spliced
/// one agree on Integer.
#[test]
fn an_integer_return_joins_with_the_home_methods_return() {
    run_test(
        r#"
        $n = 0
        def inner(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $n += 1
          end
        end
        def outer(arr); inner(arr) { |x| yield x }; end
        def find(arr, t)
          outer(arr) { |x| return x * 10 if x == t }
          -1
        end
        r = []
        40.times { |i| r << find([1, 2, 3], i % 4 + 1) + 1 }
        [r.uniq.sort, $n]
    "#,
    );
}

/// Two exits of the same kind through the same host with different
/// claims: the route's claim is their join.
#[test]
fn two_breaks_through_one_host_join_their_claims() {
    run_test(
        r#"
        $n = 0
        def inner(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $n += 1
          end
        end
        def outer(arr)
          inner(arr) { |x| yield x }
          0
        end
        def find(arr, t)
          outer(arr) { |x|
            break 100 if x == t && t.odd?
            break x * 10 if x == t
          } + 1
        end
        r = []
        40.times { |i| r << find([1, 2, 3], i % 4 + 1) }
        [r.uniq.sort, $n]
    "#,
    );
}

/// The claim survives a two-host chain to the delivering host.
#[test]
fn the_claim_survives_a_chain_of_hosts() {
    run_test(
        r#"
        $n = 0
        def inner(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $n += 1
          end
        end
        def mid(arr)
          begin
            inner(arr) { |x| yield x }
          ensure
            $n += 1
          end
        end
        def outer(arr)
          mid(arr) { |x| yield x }
          0
        end
        def find(arr, t)
          outer(arr) { |x| break x * 10 if x == t } + 1
        end
        r = []
        40.times { |i| r << find([1, 2, 3], i % 4 + 1) }
        [r.uniq.sort, $n]
    "#,
    );
}
