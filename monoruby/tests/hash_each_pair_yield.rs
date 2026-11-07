extern crate monoruby;
use monoruby::tests::*;

// Hash#each yields `key, value` directly (no [k, v] Array) only when the
// block takes two or more required parameters, as CRuby's
// rb_block_pair_yield_optimizable decides. Every other block shape must
// still see a single [k, v] pair.

#[test]
fn hash_each_block_shapes() {
    run_test_with_prelude(
        r##"
        t({a: 1, b: 2})
        "##,
        r##"
        def m(h, &b) = h.each(&b)
        def t(h)
          r = []
          h.each { |k, v| r << [k, v] }
          h.each { |kv| r << kv }
          h.each { |k, v = 9| r << [k, v] }
          h.each { |k, *v| r << [k, v] }
          h.each { |*a| r << a }
          h.each { |k, v, w| r << [k, v, w] }
          h.each { |(a, b), c| r << [a, b, c] }
          h.each_pair { |k, v| r << k }
          pr = proc { |k, v| r << [v, k] }; h.each(&pr)
          pr2 = proc { |k, *v| r << [v, k] }; h.each(&pr2)
          l1 = ->(kv) { r << kv }; h.each(&l1)
          begin; h.each(&->(k, v) { r << [k, v] }); rescue => e; r << e.class; end
          m(h) { |k, v| r << [:m, k, v] }
          r << h.each { |k, v| break [k, v] }
          h2 = h.dup
          h2.each { |k, v| h2.delete(:b); r << [k, v] }
          r << h.map { |k, v| "#{k}=#{v}" }
          r
        end
        "##,
    );
}
