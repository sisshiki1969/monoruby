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
