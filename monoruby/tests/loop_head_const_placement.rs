//! A constant that cannot meet the float placement the loop head assigned
//! to its slot.
//!
//! At a loop head, `liveness_analysis` (`use_float`) speculatively gives an
//! `S` slot an unboxed `Sf` placement when the loop reads it as a Float on
//! some path and does not kill it on *every* path. A back edge that reaches
//! the head directly (`next`) with that slot holding a constant the JIT
//! folded — a Bignum literal, `nil` — then bridges `C(v)` into `Sf` / `F`,
//! which used to be `unreachable!` for anything but a Float or Fixnum
//! constant: the compile panicked (caught by `catch_unwind`, so the loop
//! silently ran in the interpreter for good). The bridge now side-exits the
//! placement miss to the interpreter at that instruction instead.
extern crate monoruby;
use monoruby::tests::*;

/// dewasm's `sqlite3_query` shape, reduced: `s0` is Float on most paths,
/// but the `next` arm stores a 64-bit literal (a small Bignum, kept as a
/// folded constant) and jumps straight back to the loop head.
#[test]
fn a_bignum_constant_on_a_next_back_edge_into_a_float_placement() {
    run_test(
        r#"
        def f(n)
          s0 = n * 0.5
          i = 0
          acc = 0.0
          while i < n
            if i % 50 == 49
              s0 = 18446744073709551615
              i += 1
              next
            end
            if i % 2 == 0
              acc += s0 * 2.0
            else
              acc += s0 * 0.5
              s0 = acc * 0.25
            end
            i += 1
          end
          acc
        end
        r = []
        5.times { r << f(1000) }
        r
        "#,
    );
}

/// The same with `nil`: the constant is not a number at all, so the
/// iteration after the `next` must raise `NoMethodError` from the
/// interpreter rather than be computed on a stale unboxed Float.
#[test]
fn a_nil_constant_on_a_next_back_edge_into_a_float_placement() {
    run_test(
        r#"
        def g(n)
          s0 = n * 0.5
          i = 0
          acc = 0.0
          while i < n
            if i % 50 == 49
              s0 = nil
              i += 1
              next
            end
            if i % 2 == 0
              acc += s0 * 2.0
            else
              acc += s0 * 0.5
              s0 = acc * 0.25
            end
            i += 1
          end
          acc
        end
        r = []
        5.times { r << (g(1000) rescue $!.class) }
        r
        "#,
    );
}
