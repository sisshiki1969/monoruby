//! Coverage for the `rb_equal` scan behind `Array#include?` / `#index` /
//! `#count` / `#delete` (and the Ruby-level `#rindex` / `#assoc` /
//! `#rassoc`): the identity step, the operand order, the needle classes it
//! answers without a dispatch, and every reason it declines.

extern crate monoruby;
use monoruby::tests::*;

/// The identity step of `rb_equal`: an element that *is* the argument is
/// found however its `==` answers, and a `NaN` needle finds itself for the
/// same reason even though `NaN == NaN` is false.
#[test]
fn an_element_that_is_the_argument_is_found() {
    run_test(
        r##"
        class Never
          def ==(o) = false
        end
        n = Never.new
        nan = Float::NAN
        # Results are reduced to identity/shape, never inspected: an
        # object's `inspect` carries its address.
        [[n, 1].include?(n), [n, 1].index(n), [n, n].count(n),
         [n, 1].dup.delete(n).equal?(n), [n, 1].rindex(n),
         [[n, 1]].assoc(n)&.last, [[1, n]].rassoc(n)&.first,
         [nan].include?(nan), [nan].index(nan), [nan].count(nan),
         [nan].rindex(nan), [nan].dup.delete(nan)]
        "##,
    );
}

/// The *element* is the receiver of `==`, as in CRuby — `index` used to
/// ask the argument instead, which answers differently for an asymmetric
/// `==`.
#[test]
fn the_element_is_the_receiver() {
    run_test(
        r##"
        class Always
          def ==(o) = true
        end
        class Only1
          def ==(o) = o == 1
        end
        a = Always.new
        [[1, 2].index(a), [1, 2].include?(a), [1, 2].count(a),
         [a, 2].index(1), [a, 2].include?(1), [a, 2].count(1),
         [Only1.new, 2].index(1), [1, 2].index(Only1.new),
         [2, 3].index(Only1.new)]
        "##,
    );
}

/// Each needle class the scan answers from the value itself, matching and
/// missing, over every method that scans.
#[test]
fn decided_needle_classes() {
    run_test(
        r##"
        ints = [1, 2, 3]
        syms = [:a, :b]
        flags = [nil, true, false]
        strs = ["a", "b", "a"]
        [ints.include?(3), ints.include?(9), ints.index(2), ints.index(9),
         ints.count(2), ints.dup.delete(2), ints.rindex(2),
         syms.include?(:b), syms.include?(:c), syms.index(:b), syms.count(:c),
         flags.index(nil), flags.index(false), flags.index(true),
         flags.count(nil), flags.include?(0),
         strs.include?("b"), strs.include?("c"), strs.index("a"),
         strs.rindex("a"), strs.count("a"), strs.dup.delete("a"),
         [].include?(1), [].index(1), [].count(1)]
        "##,
    );
}

/// A String needle is answered by content *and* encoding compatibility,
/// exactly as `String#==` would — including the empty-string case, where
/// incompatible encodings still compare equal.
#[test]
fn string_needle_matches_string_semantics() {
    run_test(
        r##"
        [["abc".b].include?("abc"),
         ["あ".encode("UTF-8")].include?("あ".encode("UTF-16BE")),
         ["".encode("UTF-16BE")].include?(""),
         ["a".freeze].index("a"),
         [:a].include?("a"), ["a"].include?(:a),
         ["a"].include?(1), [1].include?("a")]
        "##,
    );
}

/// Every reason the value-side answer does not apply, each of which must
/// still give CRuby's result through the dispatch: a needle whose class is
/// not one of them, a numeric pair that is equal across classes, a Bignum,
/// and a String *subclass* element, whose own `==` CRuby runs.
#[test]
fn undecided_shapes_fall_back_to_dispatch() {
    run_test(
        r##"
        class SubStr < String
          def ==(o) = true
        end
        [[1.0, 2].count(1), [1, 2].count(1.0), [1, 2].index(1.0),
         [2**70, 1].index(2**70), [1].include?(2**70),
         [2**70].include?(2**70 + 0),
         [SubStr.new("zzz")].include?("q"),
         ["q"].include?(SubStr.new("zzz")),
         [[1, 2], [3]].index([3]), [[1, 2]].include?([1, 2]),
         [{a: 1}].include?({a: 1})]
        "##,
    );
}

/// The element side is dispatched through a cache keyed by class, so an
/// array of several classes must still ask each one's own `==`.
#[test]
fn polymorphic_elements_each_use_their_own_eq() {
    run_test(
        r##"
        class Yes
          def ==(o) = true
        end
        class No
          def ==(o) = false
        end
        class Plain
        end
        mixed = [No.new, Plain.new, Yes.new, No.new]
        [mixed.index(:anything), mixed.count(:anything),
         mixed.include?(:anything), mixed.rindex(:anything),
         [Plain.new, Plain.new].index(:anything)]
        "##,
    );
}

/// A redefined `==` on the needle's class takes the scan off the
/// value-side answer, for each class that has one.
#[test]
fn redefined_eq_reaches_the_scan() {
    run_test_once(
        r#"
        class Integer; def ==(o) = true; end
        [[1, 2].index(9), [1, 2].include?(9), [1, 2].count(9)]
        "#,
    );
    run_test_once(
        r#"
        class Symbol; def ==(o) = true; end
        [[:a, :b].index(:z), [:a, :b].include?(:z)]
        "#,
    );
    run_test_once(
        r#"
        class String; def ==(o) = true; end
        [["a", "b"].index("z"), ["a", "b"].include?("z")]
        "#,
    );
    run_test_once(
        r#"
        class NilClass; def ==(o) = true; end
        [[nil, 1].index(false), [nil].include?(false)]
        "#,
    );
}

/// `delete` scans twice — once to find a match, then to filter — and both
/// must agree; the block form runs only when nothing matched.
#[test]
fn delete_scans_agree() {
    run_test(
        r##"
        class Never
          def ==(o) = false
        end
        n = Never.new
        a = [1, 2, 1, 3]
        deleted = a.delete(1)
        b = [n, 1, n]
        b.delete(n)
        [deleted, a, b.size, [1, 2].delete(9), [1, 2].delete(9) { :none },
         ["x", "y"].tap { |s| s.delete("x") }]
        "##,
    );
    run_test_error("[1, 2].freeze.delete(1)");
}
