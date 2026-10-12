extern crate monoruby;
use monoruby::tests::*;

// A loop head seeds the locals whose elements the loop reads as `Array`
// when the forward entries bring them in untyped (`analyse_loop_head`),
// so the loop-invariant hoisting guards them once on entry. These enter
// such loops with non-Arrays, mixed elements, aliased stores, mutating
// calls, out-of-range and past-the-end accesses.

// break the speculation in each way the body or the entry can.

#[test]
fn loop_array_seed_entry_miss() {
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
fn loop_array_seed_aliased_store() {
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
fn loop_array_seed_call_mutates() {
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
fn loop_array_seed_out_of_range() {
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
fn loop_array_seed_store_past_end() {
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

// The candidate scan (`loop_reads_elements`) walks the loop's bytecode
// linearly, and a `rescue` inside the loop puts its handler, whose
// clause-match opcodes the JIT never visits, inside that range. The walk
// has to step over them rather than decode them.
#[test]
fn loop_array_seed_over_rescue_handler() {
    run_test(
        r#"
        def f(a, n)
          i = 0
          s = 0
          while i < n
            begin
              s += a[i & 3]
              raise "x" if i % 7 == 0
            rescue RuntimeError
              s += 1
            end
            i += 1
          end
          s
        end
        r = []
        30.times { |j| r << f([1, 2, 3, j], 60) }
        r << f([1, 2.5, 3, 4], 60)
        r << (f([1, "x", 3, 4], 60) rescue $!.class)
        r << f(0.step(10).to_a, 200)
        r
        "#,
    );
}

#[test]
fn loop_array_seed_over_rescue_splat_handler() {
    run_test(
        r#"
        def g(a, n, errs)
          i = 0
          s = 0
          while i < n
            begin
              s += a[i % a.size]
              raise ArgumentError if i % 5 == 0
            rescue *errs
              s += 2
            end
            i += 1
          end
          s
        end
        r = []
        errs = [ArgumentError, TypeError]
        30.times { |j| r << g([1, 2, 3, j], 60, errs) }
        r << g([1.5, 2, 3], 60, errs)
        r << (g([1, 2, 3], 60, [TypeError]) rescue $!.class)
        r
        "#,
    );
}
