extern crate monoruby;
use monoruby::tests::*;

/// A JIT-compiled `a[i] = v` raises `FrozenError` on a frozen receiver
/// (inline and heap buffers, method and loop JIT) instead of storing (#1727).
#[test]
fn jit_index_assign_rejects_frozen() {
    run_test_once(
        r#"
        def f(a, i, v) = a[i] = v
        small = [1, 2, 3]
        big = (0...20).to_a
        r = []
        100.times { f(small, 0, 7); f(big, 10, 7) }
        [[1, 2, 3].freeze, (0...20).to_a.freeze].each do |a|
          begin
            f(a, 1, 9)
          rescue FrozenError => e
            r << e.class
          end
          r << a
        end
        d = [1, 2, 3].freeze
        i = 0
        begin
          while i < 200
            (i == 150 ? d : small)[1] = i
            i += 1
          end
        rescue FrozenError => e
          r << e.class
        end
        r << i << d << small
        r
        "#,
    );
}
