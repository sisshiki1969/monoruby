extern crate monoruby;
use monoruby::tests::*;

// An Array may carry `ARRAY_ELEMS_FIXNUM` ("every element is a Fixnum").
// Debug builds re-check it for every live array at each collection, so
// these tests store non-Fixnums through each JIT fast path into arrays
// that start out all-Fixnum, then collect.

#[test]
fn jit_index_assign_clears_elems_kind() {
    run_test(
        r#"
        def st(a, i, v) = a[i] = v
        r = []
        [1, "s", 2.5, nil, :sym, 3].each do |v|
          a = []
          8.times { |i| a << i }
          b = []
          3.times { |i| b << i }
          st(a, 6, v); st(b, 1, v); st(a, -1, v)
          GC.start
          r << a << b
        end
        r
        "#,
    );
}

#[test]
fn jit_shl_and_slice_assign_clear_elems_kind() {
    run_test(
        r#"
        r = []
        [1, "s", nil, [1]].each do |v|
          a = []
          4.times { |i| a << i }
          a << v
          b = []
          9.times { |i| b << i }
          b << v
          c = []
          6.times { |i| c << i }
          c[1, 2] = [v, v]
          GC.start
          r << a << b << c
        end
        r
        "#,
    );
}

#[test]
fn builtins_clear_elems_kind() {
    run_test(
        r#"
        a = []
        10.times { |i| a << i }
        a.map! { |x| x.to_s }
        b = []
        5.times { |i| b.push(i) }
        b.unshift(:x)
        c = []
        5.times { |i| c << i }
        c.concat(["y"])
        d = Array.new(4, 0)
        d.fill("z")
        e = []
        3.times { |i| e << i }
        e[6] = 1
        GC.start
        [a, b, c, d, e]
        "#,
    );
}
