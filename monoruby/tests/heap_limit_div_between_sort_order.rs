//! A bignum's `div` by a Float, `Comparable#between?` reading `<=>`
//! answers as `rb_cmpint`, and the order in which `sort` / `min(n)` /
//! `max(n)` call the comparator (#2045, #2046, #2014).
//!
//! - **`div`** (#2045). A bignum receiver over a Float is
//!   `rb_big_divide`'s `rb_dbl2big(x / y)`, truncated toward zero; a
//!   fixnum receiver floors. The division matrix in
//!   `num_coerce_cmpint_float_ties_set_marshal.rs` covers the rows.
//! - **`between?`** (#2046). Each `<=>` answer is read as `rb_cmpint`
//!   reads it: an Integer by its sign, nil as the comparison failure,
//!   anything else by `> 0` and then `< 0`, so a NaN answer is 0.
//! - **Comparator order** (#2014). `sort` is CRuby's `ruby_qsort`, which
//!   on Linux is glibc's `qsort_r`, a top-down merge sort; `min(n)` /
//!   `max(n)` / `min_by(n)` / `max_by(n)` are `rb_nmin_run`. A block or a
//!   user-defined `<=>` sees the same pairs in the same order.

use monoruby::tests::*;

const HELPER: &str = r##"
  t = ->(&b) { begin; b.call.inspect.gsub(/0x\h+/, "0x"); rescue Exception => e; "#{e.class}: #{e.message}".gsub(/0x\h+/, "0x"); end }
"##;

#[test]
fn between_reads_answers_as_cmpint() {
    run_test_once(&format!(
        r#"
        {HELPER}
        class A; include Comparable; def initialize(r) = @r = r; def <=>(o) = @r; end
        class W; def >(o) = true; def <(o) = false; end
        class W2; def >(o) = false; def <(o) = true; end
        [Float::NAN, Object.new, true, W.new, W2.new, nil, "a", 1.5, -1.5, 2**70, -(2**70),
         1r, -1r, BasicObject.new, 0, -3, 7].map do |r|
          t.() {{ A.new(r).between?(A.new(r), A.new(r)) }}
        end
        "#
    ));
}

/// The usual answers keep their meaning once `between?` is compiled.
#[test]
fn between_in_compiled_code() {
    run_test(
        r#"
        r = []
        r << 5.between?(1, 10) << 5.between?(6, 10) << 5.between?(1, 4)
        r << 1.5.between?(1, 2) << "b".between?("a", "c") << (2**70).between?(1, 2**71)
        r << (begin; 5.between?(nil, 3); rescue ArgumentError => e; e.message; end)
        r << (begin; 3.between?(1, nil); rescue ArgumentError => e; e.message; end)
        r
        "#,
    );
}

/// Arrays of every small length and a few larger ones, with repeated
/// values, from a fixed sequence.
const ARRAYS: &str = r##"
  seed = 12345
  arrays = [*0..20, 33, 64, 101].map do |len|
    Array.new(len) { seed = (seed * 1103515245 + 12345) % 2**31; seed % (len / 3 + 1) }
  end
"##;

/// `sort` with a block and with a user-defined `<=>`, and `sort_by` with
/// keys that compare through Ruby, call the comparator with the pairs of
/// glibc's merge sort, which is CRuby's `ruby_qsort` on Linux (#2014).
#[test]
fn sort_calls_the_comparator_in_crubys_order() {
    run_test_once(&format!(
        r#"
        {ARRAYS}
        K = Struct.new(:v, :log) do
          include Comparable
          def <=>(o) = (log << [v, o.v]; v <=> o.v)
        end
        arrays.map do |a|
          l1 = []; r1 = a.sort {{ |x, y| l1 << [x, y]; x <=> y }}
          l2 = []; r2 = a.map {{ K.new(_1, l2) }}.sort.map(&:v)
          l3 = []; r3 = a.each_with_index.sort_by {{ |x, i| K.new(x, l3) }}.map(&:last)
          [r1, l1, r2, l2, r3, l3]
        end
        "#
    ));
}

/// `min(n)` / `max(n)` and `min_by(n)` / `max_by(n)` are `rb_nmin_run`:
/// a buffer of `4 * n` cut down by a quickselect, then sorted (#2014).
#[test]
fn nmin_calls_the_comparator_in_crubys_order() {
    run_test_once(&format!(
        r#"
        {ARRAYS}
        arrays.flat_map do |a|
          [1, 2, 3, 7].flat_map do |n|
            l = []
            [
              a.min(n) {{ |x, y| l << [x, y]; x <=> y }}, a.max(n) {{ |x, y| l << [x, y]; x <=> y }},
              a.each.min(n) {{ |x, y| l << [x, y]; x <=> y }}, a.each.max(n) {{ |x, y| l << [x, y]; x <=> y }},
              a.each_with_index.min_by(n) {{ |x, i| l << i; x }}, a.each_with_index.max_by(n) {{ |x, i| l << i; x }},
              (1..a.size).min(n) {{ |x, y| l << [x, y]; a[x - 1] <=> a[y - 1] }},
              a.min(n), a.max(n), l,
            ]
          end
        end
        "#
    ));
}

/// Which pair a failing comparison names follows from the order.
#[test]
fn sort_and_nmin_errors_name_crubys_pair() {
    run_test_once(&format!(
        r#"
        {HELPER}
        [
          t.() {{ [1, nil].max(1) }}, t.() {{ [1, nil].min(1) }}, t.() {{ [1, nil, 2].sort }},
          t.() {{ [3, "a", 1, nil].sort }}, t.() {{ [2, 1, "a"].each.min(2) }},
          t.() {{ [3, 1, 2].min(2) {{ nil }} }}, t.() {{ [3, 1, 2].max(2) {{ 1.5 }} }},
          t.() {{ [1, nil].sort_by {{ _1 }} }}, t.() {{ [3, 1, 2].sort {{ |a, b| raise "x" if a == 1; a <=> b }} }},
        ]
        "#
    ));
}
