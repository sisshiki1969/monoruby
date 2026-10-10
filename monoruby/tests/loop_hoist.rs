extern crate monoruby;
use monoruby::tests::*;

// Loop-invariant guard hoisting (`codegen/jitgen/merge.rs`): a slot the
// forward entries of a loop bring in untyped but the back edge carries
// typed is guarded once on the entries, so the head keeps the type. These
// pin the shapes it applies to (parameters, values read by an enclosing
// loop, operands reached through a `Mov` copy) and the ones where the
// hoisted guard misses, against CRuby.

#[test]
fn loop_hoist_invariant_params() {
    run_test(
        r#"
        def f(a, s, n)
          i = 0
          r = 0
          while i < n
            r += a[i % a.size] + s.size
            i += 1
          end
          r
        end
        [f([1, 2, 3], "ab", 10), f([4], "", 3), f([1, 2], "xyz", 0)]
        "#,
    );
}

#[test]
fn loop_hoist_nested_invariant_rows() {
    run_test(
        r#"
        def g(rows, idx, out)
          k = 0
          while k < idx.size
            row = rows[idx[k]]
            j = 0
            while j < row.size
              out[row[j]] += 1
              j += 1
            end
            k += 1
          end
          out
        end
        g([[0, 1], [2, 3, 1], [0]], [0, 1, 2, 1], [0, 0, 0, 0])
        "#,
    );
}

#[test]
fn loop_hoist_guard_miss() {
    run_test_once(
        r#"
        def h(x, n)
          i = 0
          r = 0
          while i < n
            r += x
            i += 1
          end
          r
        end
        res = []
        40.times { res << h(2, 5) }
        res << h(1.5, 4) << h(3, 3)
        40.times { |k| res << h(k.even? ? 2.5 : 7, 3) }
        res << h(2 ** 70, 2) << h(-1, 2)
        res.uniq
        "#,
    );
}

#[test]
fn loop_hoist_nil_on_entry() {
    run_test_once(
        r#"
        def m(a, n)
          i = 0
          last = nil
          while i < n
            last = a[i]
            i += 1
          end
          last
        end
        res = []
        40.times { |k| res << m([k, k + 1], 2) }
        res << m([], 0) << m([nil, 3], 1) << m(["s"], 1)
        res
        "#,
    );
}

#[test]
fn loop_hoist_copy_then_redefine() {
    run_test(
        r#"
        def two(a, b) = [a, b]
        def c(x, n)
          i = 0
          r = []
          while i < n
            # The first argument is a copy of `x`, taken before `x` is
            # redefined; nothing proved about the copy may stick to the
            # new `x`.
            r << two(x, (x = 2.5))
            r << x + 1
            x = i
            i += 1
          end
          r
        end
        [c(1, 3), c(10, 1)]
        "#,
    );
}

#[test]
fn loop_hoist_block_loops() {
    run_test(
        r#"
        def b(a, s)
          t = 0
          3.times do |i|
            a.each { |x| t += x * i + s.size }
          end
          t
        end
        [b([1, 2, 3], "ab"), b([], "x")]
        "#,
    );
}
