extern crate monoruby;
use monoruby::tests::*;

// Loop-entry type seeding (`codegen/jitgen/compile/loop_entry.rs`): a loop
// JIT guards the slot types the method's code before the loop predicts.
// These pin the predictions that hold, the ones that miss (the entry guard
// resumes the interpreter, and the recompile drops the slot), and the
// shapes the prefix walk has to get through, against CRuby.

#[test]
fn loop_entry_seed_typed_locals() {
    run_test_once(
        r#"
        def sum(n)
          s = 0
          i = 0
          f = 1.5
          a = [1, 2, 3]
          h = { a: 1 }
          str = "x"
          while i < n
            s += a[i % 3] + i + h[:a]
            f = f * 1.0000001
            i += 1
          end
          [s, f.round(6), str]
        end
        [sum(10), sum(1000), sum(100000), sum(0)]
        "#,
    );
}

#[test]
fn loop_entry_seed_guard_miss() {
    run_test_once(
        r#"
        def g = "s"
        def h(x) = x * 3
        def f(flag)
          # Only the `true` arm has run when the loop is compiled: the
          # prediction says Integer, a later `false` call brings a String.
          x = flag ? 1 : g
          y = 2.0
          i = 0
          r = nil
          while i < 200
            r = [x, y, h(x)]
            i += 1
          end
          r
        end
        res = [f(true)]
        30.times { |k| res << f(k.even?) }
        res << f(false) << f(true)
        res.uniq
        "#,
    );
}

#[test]
fn loop_entry_seed_bignum_entry() {
    run_test_once(
        r#"
        def big(m)
          x = m * m
          i = 0
          t = 0
          while i < 100
            t += x
            i += 1
          end
          t
        end
        r = []
        r << big(3)
        r << big(2**40)
        20.times { |k| r << big(k.even? ? 2**40 : k) }
        r.sum
        "#,
    );
}

#[test]
fn loop_entry_seed_nested_and_block() {
    run_test_once(
        r#"
        def nested(n)
          out = []
          j = 0
          while j < n
            x = j.even? ? j : j.to_f
            i = 0
            acc = 0
            while i < 50
              acc += x
              i += 1
            end
            out << acc
            j += 1
          end
          out
        end
        def in_block(n)
          r = []
          [1, 2.5, :s].each do |e|
            i = 0
            v = 0
            while i < n
              v = e
              i += 1
            end
            r << v
          end
          r
        end
        [nested(40).last(4), in_block(300), in_block(5)]
        "#,
    );
}

#[test]
fn loop_entry_seed_optional_and_keyword_args() {
    run_test_once(
        r#"
        def opt(a, b = 2, *rest, c: 3.0, **kw, &blk)
          i = 0
          s = 0
          while i < 100
            s += a + b + c + rest.size + kw.size
            s += blk.call(i) if blk
            i += 1
          end
          s
        end
        r = []
        r << opt(1)
        r << opt(1, 5)
        r << opt(1, 2, 3, c: 1, d: 4) { |x| x }
        r << opt(1.5, 2.5, c: 0)
        r
        "#,
    );
}

#[test]
fn loop_entry_seed_top_level() {
    run_test_once(
        r#"
        a = 0
        f = 0.5
        s = ""
        i = 0
        while i < 300
          a += i
          f += 0.25
          s = s + "." if i % 100 == 0
          i += 1
        end
        x = nil
        k = 0
        while k < 200
          x = k
          k += 1
        end
        [a, f, s, x]
        "#,
    );
}
