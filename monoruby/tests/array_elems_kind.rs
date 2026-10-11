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

// The JIT speculates at a loop head that an Array holds only Fixnums
// (`Guarded::FixnumArray`) and types its element reads as Fixnum. These
// break the speculation in each way the body or the entry can.

#[test]
fn jit_elems_kind_entry_miss() {
    run_test(
        r#"
        def sum(a)
          s = 0
          i = 0
          while i < a.size
            s += a[i]
            i += 1
          end
          s
        end
        r = []
        30.times { |j| r << sum([1, 2, 3, j]) }
        r << sum([1, 2.5, 3])
        r << (sum([1, "x"]) rescue $!.class)
        r << sum((1..50).to_a)
        r
        "#,
    );
}

#[test]
fn jit_elems_kind_aliased_store() {
    run_test(
        r#"
        def f(a, b, n)
          s = []
          i = 0
          while i < n
            b[i % b.size] = "x" if i == n - 3
            s << a[i % a.size] + a[0].to_s.size
            i += 1
          end
          s
        end
        r = []
        20.times do
          a = [1, 2, 3, 4]
          r << (f(a, a, 30) rescue [$!.class, a])
        end
        r
        "#,
    );
}

#[test]
fn jit_elems_kind_call_mutates() {
    run_test(
        r#"
        def g(a, i)
          a.map! { |x| x.to_s } if i == 25
        end
        def f(a)
          s = 0
          i = 0
          while i < 40
            g(a, i)
            s += a[i % 4].size
            i += 1
          end
          s
        end
        r = []
        8.times { r << f([1, 2, 3, 4]) }
        r
        "#,
    );
}

#[test]
fn jit_elems_kind_out_of_range() {
    run_test(
        r#"
        def f(a)
          s = 0
          i = 0
          while (x = a[i])
            s += x
            i += 1
          end
          j = -1
          while j > -10
            s += (a[j] || 100)
            j -= 1
          end
          s
        end
        r = []
        30.times { |k| r << f([1, 2, 3, k]) }
        r
        "#,
    );
}

#[test]
fn jit_elems_kind_store_past_end() {
    run_test(
        r#"
        def f(a, n)
          i = 0
          while i < n
            a[i * 2] = (a[i] || 0) + 1
            i += 1
          end
          a
        end
        r = []
        20.times { r << f([1, 2, 3], 5) }
        r << f([1, 2, 3], 2)
        GC.start
        r
        "#,
    );
}
