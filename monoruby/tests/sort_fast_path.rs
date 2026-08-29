//! Coverage for the homogeneous-array sort specialization
//! (`Executor::sort` / `sort_indices_by_homogeneous_keys`): each arm it
//! takes, each reason it declines, and the comparator paths behind it.

extern crate monoruby;
use monoruby::tests::*;

/// The three specialized element types, sorted and sorted in place.
#[test]
fn homogeneous_arrays_sort_by_type() {
    run_test(
        r#"
        ints = [3, 1, 2, 1, -5, 0]
        floats = [3.5, -1.25, 2.0, 3.5, 0.0]
        strings = ["pear", "apple", "fig", "apple"]
        a = ints.dup; a.sort!
        b = floats.dup; b.sort!
        c = strings.dup; c.sort!
        [ints.sort, floats.sort, strings.sort, a, b, c]
        "#,
    );
}

/// Every reason the specialization declines, each of which must still
/// produce CRuby's answer through the general comparator: a mixed array,
/// a `NaN` (which raises rather than sorting), Bignums, Symbols, and
/// arrays too short to sort.
#[test]
fn non_specializable_arrays_fall_back() {
    run_test(
        r#"
        mixed = [2, 1.5, 3]
        bigs = [2**70, 1, 2**80]
        syms = [:c, :a, :b]
        nan = begin
          [Float::NAN, 1.0].sort
        rescue => e
          e.class
        end
        cross = begin
          [1, "a"].sort
        rescue => e
          e.class
        end
        [mixed.sort, bigs.sort, syms.sort, nan, cross, [].sort, [7].sort]
        "#,
    );
}

/// `sort_by` / `sort_by!` order their *keys*, so the same specialization
/// applies to the key vector — for each type, and declining on mixed keys.
#[test]
fn sort_by_specializes_on_its_keys() {
    run_test(
        r#"
        words = ["bbb", "a", "cc", "dddd"]
        by_int = words.sort_by { |s| s.size }
        by_str = words.sort_by { |s| s }
        by_float = words.sort_by { |s| s.size * 1.5 }
        by_neg = [3, 1, 2].sort_by { |x| -x }
        mixed_keys = begin
          [1, 2].sort_by { |x| x.even? ? 1 : "a" }
        rescue => e
          e.class
        end
        w = words.dup; w.sort_by! { |s| s.size }
        f = [1.5, 0.5].dup; f.sort_by! { |x| x }
        [by_int, by_str, by_float, by_neg, mixed_keys, w, f, [].sort_by { |x| x }]
        "#,
    );
}

/// `Hash#sort` orders `[key, value]` pairs — Arrays, so the general
/// comparator handles them, but the keys inside go through the same
/// `<=>` answers.
#[test]
fn hash_and_nested_arrays_sort() {
    run_test(
        r#"
        h = { 3 => :c, 1 => :a, 2 => :b }
        [h.sort, [[2, 1], [1, 9], [1, 2]].sort, [["b", 1], ["a", 2]].sort]
        "#,
    );
}

/// A user comparator is read by sign, as CRuby's `rb_cmpint` does: a
/// `<=>` returning ±5 orders exactly as one returning ±1, and one
/// returning `nil` reports the pair as incomparable.
#[test]
fn user_comparator_is_read_by_sign() {
    run_test(
        r#"
        class Diff
          attr_reader :v
          def initialize(v) = @v = v
          def <=>(o) = v - o.v
        end
        class Nope
          def <=>(o) = nil
        end
        by_diff = [Diff.new(2), Diff.new(1), Diff.new(3), Diff.new(1)].sort.map(&:v)
        incomparable = begin
          [Nope.new, Nope.new].sort
        rescue => e
          e.class
        end
        [by_diff, incomparable, (Diff.new(1) <=> Diff.new(3)), (Nope.new <=> 1)]
        "#,
    );
}

/// The `sort_by` family declines the same way `sort` does, and must then
/// report through the general comparator — including in the destructive
/// form, whose fallback writes the receiver back.
#[test]
fn sort_by_bang_falls_back_on_mixed_keys() {
    run_test(
        r#"
        a = [1, 2, 3]
        raised = begin
          a.sort_by! { |x| x == 2 ? "two" : x }
        rescue => e
          e.class
        end
        b = [[2, :b], [1, :a]]
        b.sort_by! { |pair| pair }
        [raised, b, [9].sort_by { |x| x }, [].sort_by! { |x| x }]
        "#,
    );
}

/// A redefined `<=>` reaches the key sort too, for each specialized key
/// type: the sort must run the redefinition (and report its non-Integer
/// answer) rather than ordering the keys directly.
#[test]
fn sort_by_honors_redefined_cmp() {
    run_test_once(
        r#"
        class Integer; def <=>(o); :OVERRIDDEN; end; end
        begin
          ["bbb", "a"].sort_by { |s| s.size }
        rescue => e
          e.class
        end
        "#,
    );
    run_test_once(
        r#"
        class String; def <=>(o); :OVERRIDDEN; end; end
        begin
          ["bbb", "a"].sort_by { |s| s }
        rescue => e
          e.class
        end
        "#,
    );
    run_test_once(
        r#"
        class Float; def <=>(o); :OVERRIDDEN; end; end
        begin
          [2.5, 1.5].sort_by { |x| x }
        rescue => e
          e.class
        end
        "#,
    );
}

/// Redefining `Float#<=>` has to reach the specialization, which is why
/// the `(class, "<=>")` pairs are tracked in `BASIC_OP_DEFS` (the Integer
/// and String cases live with the rest of that table's coverage, in
/// `globals::store::basic_op`).
#[test]
fn float_cmp_redefinition_reaches_the_sort() {
    run_test_once(
        r#"
        class Float; def <=>(o); :OVERRIDDEN; end; end
        begin
          [2.0, 1.0].sort
        rescue => e
          e.class
        end
        "#,
    );
}
