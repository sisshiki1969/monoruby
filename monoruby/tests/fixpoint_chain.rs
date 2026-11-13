//! The back-edge fixpoint (`analyse_backedge_fixpoint`) on loop bodies
//! that move a type one local further per walk: a chain of copies
//! (`a0 = a1; a1 = a2; ...`) needs as many walks as it has locals. The
//! walk bound used to be ten per loop head, whatever the body, and a
//! ten-local chain in a method (whose locals start as constants, so each
//! one has a step to take) panicked with "not fixed" — which aborts the
//! process, since the compile runs under an `extern "C"` frame. See
//! `doc/fixpoint_engine_plan.md` §2.3.

extern crate monoruby;
use monoruby::tests::*;

#[test]
fn copy_chain_below_the_old_bound() {
    run_test(
        r##"
        def f(n)
          a0 = 0; a1 = 0; a2 = 0; a3 = 0; a4 = 0; a5 = 0; a6 = 0; a7 = 0; a8 = 0
          i = 0
          while i < n
            a0 = a1; a1 = a2; a2 = a3; a3 = a4; a4 = a5; a5 = a6; a6 = a7; a7 = a8
            a8 = a8 + 1.5
            i += 1
          end
          a0
        end
        r = nil
        40.times { r = f(40) }
        r
        "##,
    );
}

#[test]
fn copy_chain_at_the_old_bound() {
    run_test(
        r##"
        def f(n)
          a0 = 0; a1 = 0; a2 = 0; a3 = 0; a4 = 0; a5 = 0; a6 = 0; a7 = 0; a8 = 0; a9 = 0
          i = 0
          while i < n
            a0 = a1; a1 = a2; a2 = a3; a3 = a4; a4 = a5; a5 = a6; a6 = a7; a7 = a8; a8 = a9
            a9 = a9 + 1.5
            i += 1
          end
          a0
        end
        r = nil
        40.times { r = f(40) }
        r
        "##,
    );
}

#[test]
fn copy_chain_long() {
    run_test(
        r##"
        def f(n)
          a0 = 0; a1 = 0; a2 = 0; a3 = 0; a4 = 0; a5 = 0; a6 = 0; a7 = 0; a8 = 0; a9 = 0; a10 = 0; a11 = 0; a12 = 0; a13 = 0; a14 = 0; a15 = 0; a16 = 0; a17 = 0; a18 = 0; a19 = 0; a20 = 0; a21 = 0; a22 = 0; a23 = 0
          i = 0
          while i < n
            a0 = a1; a1 = a2; a2 = a3; a3 = a4; a4 = a5; a5 = a6; a6 = a7; a7 = a8; a8 = a9; a9 = a10; a10 = a11; a11 = a12; a12 = a13; a13 = a14; a14 = a15; a15 = a16; a16 = a17; a17 = a18; a18 = a19; a19 = a20; a20 = a21; a21 = a22; a22 = a23
            a23 = a23 + 1.5
            i += 1
          end
          a0
        end
        r = nil
        40.times { r = f(40) }
        r
        "##,
    );
}

/// `C(0.0)` and `F` are both Float, yet not `equiv`: a chain of float
/// locals still settles one local per walk.
#[test]
fn copy_chain_float_init() {
    run_test(
        r##"
        def f(n)
          a0 = 0.0; a1 = 0.0; a2 = 0.0; a3 = 0.0; a4 = 0.0; a5 = 0.0; a6 = 0.0; a7 = 0.0; a8 = 0.0; a9 = 0.0; a10 = 0.0; a11 = 0.0
          i = 0
          while i < n
            a0 = a1; a1 = a2; a2 = a3; a3 = a4; a4 = a5; a5 = a6; a6 = a7; a7 = a8; a8 = a9; a9 = a10; a10 = a11
            a11 = a11 * 0.5 + 1.0
            i += 1
          end
          a0
        end
        r = nil
        40.times { r = f(40) }
        r
        "##,
    );
}

/// A twelve-tap delay line written with a multiple assignment.
#[test]
fn delay_line() {
    run_test(
        r##"
        def fir(n)
          d0 = d1 = d2 = d3 = d4 = d5 = d6 = d7 = d8 = d9 = d10 = d11 = 0.0
          acc = 0.0
          i = 0
          while i < n
            d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10 = d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11
            d11 = i * 0.5
            acc += (d0 + d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11) / 12.0
            i += 1
          end
          acc
        end
        r = nil
        40.times { r = fir(100) }
        r
        "##,
    );
}

/// The chain inside a nested loop: one walk covers the nest, so the
/// inner body's chain is what sets the number of walks.
#[test]
fn copy_chain_nested() {
    run_test(
        r##"
        def g(n)
          a0 = 0; a1 = 0; a2 = 0; a3 = 0; a4 = 0; a5 = 0; a6 = 0; a7 = 0; a8 = 0; a9 = 0
          a10 = 0; a11 = 0; a12 = 0; a13 = 0; a14 = 0; a15 = 0; a16 = 0; a17 = 0; a18 = 0; a19 = 0
          a20 = 0
          i = 0
          while i < n
            j = 0
            while j < 2
              a0 = a1; a1 = a2; a2 = a3; a3 = a4; a4 = a5; a5 = a6; a6 = a7; a7 = a8; a8 = a9; a9 = a10
              a10 = a11; a11 = a12; a12 = a13; a13 = a14; a14 = a15; a15 = a16; a16 = a17; a17 = a18
              a18 = a19; a19 = a20
              a20 = a20 + 1.5
              j += 1
            end
            i += 1
          end
          a0
        end
        r = nil
        40.times { r = g(40) }
        r
        "##,
    );
}
