//! Loop pinning (`gp_alloc::LoopPin`): an innermost loop keeps its
//! most-read integer local in a fixed register across its blocks.

use monoruby::tests::*;

#[test]
fn pinned_index_and_counter() {
    // The pinned local is the counter: read by the compare and the
    // element accesses in several blocks, written by `+=` into the pin
    // register on two paths.
    run_test(
        r#"
        def f(a, n)
          s = 0
          i = 0
          while i < n
            if a[i] > 3
              s += a[i]
              i += 1
            else
              s -= 1
              i += 2
            end
          end
          [s, i]
        end
        a = (0..9).to_a * 4
        res = []
        30.times { |k| res << f(a, 40 - k % 7) }
        res
        "#,
    );
}

#[test]
fn pinned_local_overflows_and_deopts() {
    // `i += d` on the pin register overflows into a Bignum: the side exit
    // must resume with the old value from the home.
    run_test(
        r#"
        def f(start, d, n)
          i = start
          c = 0
          while c < n
            i += d
            c += 1
          end
          i
        end
        res = []
        30.times { |k| res << f(k, 3, 20) }
        res << f(2**62 - 10, 3, 20)
        res << f(-(2**62) + 10, -3, 20)
        30.times { |k| res << f(k, 3, 20) }
        res
        "#,
    );
}

#[test]
fn pinned_local_copied_multiplied_and_divided() {
    // A copy of the pinned local takes a register of its own; `*` and `/`
    // clobber their rhs, which may be the pin register.
    run_test(
        r#"
        def f(n)
          i = 1
          acc = 0
          while i < n
            j = i
            acc += 1000 / i + j * i - (acc / (i + 1))
            j += 1
            acc -= j
            i += 1
          end
          acc
        end
        res = []
        30.times { |k| res << f(10 + k) }
        res
        "#,
    );
}

#[test]
fn pinned_local_across_a_call_and_nested_loops() {
    // A call clobbers the pin register (loops that call are not pinned,
    // but an inner loop is, inside an outer one that calls); the inner
    // loop's pin is reloaded on entry.
    run_test(
        r#"
        def g(x) = x * 2
        def f(n)
          s = 0
          k = 0
          while k < n
            i = 0
            while i < k
              s += i & 3
              i += 1
            end
            s += g(k)
            k += 1
          end
          s
        end
        res = []
        30.times { |k| res << f(5 + k % 4) }
        res
        "#,
    );
}

#[test]
fn pinned_local_seen_by_a_thread() {
    // A local the loop pins but a block captures (the frame may be
    // captured): another thread writes it while this one waits at the
    // loop's poll, and the loop must see the write.
    run_test_once(
        r#"
        def f
          i = 0
          t = Thread.new { sleep 0.01; i = 1_000_000 }
          n = 0
          while i < 1_000_000
            i += 1 if i % 2 == 0 || i < 10
            n += 1
          end
          t.join
          i >= 1_000_000
        end
        f
        "#,
    );
}
