//! A `break` or a non-local `return` lands in the continuation of a call,
//! the same code a plain return of that call resumes (#2201).
//!
//! The JIT resumed the calling frame from the join of the callee's plain
//! return paths alone. What an exit path had done to an outer local on
//! its way out was missing from that join, for the frames above the one
//! that resumes:
//!
//! - a store on the exit path (`t = 7; break`) was not seen by the code
//!   after the call, which went on reading `t` as the Float, or the
//!   literal, the return paths still claimed;
//! - a call that only ever leaves by an exit has no return path at all,
//!   and its frame's write-back for a deoptimization under the call took
//!   every slot for current, a Float local deferred to its raw-f64 home
//!   included: the local read as `nil`.

use monoruby::tests::*;

#[test]
fn store_on_a_break_path_is_seen_after_the_call() {
    // `c6` returns normally as far as the compiler can see (`each` may
    // run no iteration), with `t` untouched; the path that does run
    // stores `t` and leaves by `break`.
    run_test_once(
        r##"
        def c0_i2201(x) = yield(x)
        def c6_i2201(x)
          [x].each { |e| yield e }
        end
        def a_i2201(a0)
          t = a0 * 2.0
          c0_i2201(a0) { |v| c6_i2201(v) { |w| t = 7; break }; t.to_s }
        end
        def b_i2201(a0)
          t = a0 * 2.0; u = 0.5
          c0_i2201(a0) { |v| c6_i2201(v) { |w| t = 7; break }; u = t + 1.0 }
          [t, u]
        end
        def c_i2201(a0)
          t = 5; u = 0
          c0_i2201(a0) { |v| c6_i2201(v) { |w| if w > 50 then t = 7; break; else u = 3; end }; u = [t, u] }
          [t, u]
        end
        r = []
        200.times { |i| a = i % 7 == 0 ? 98.0 : 1.5; r << [a_i2201(a), b_i2201(a), c_i2201(a)] }
        r.uniq
        "##,
    );
}

#[test]
fn store_on_a_return_path_is_seen_after_the_call() {
    // The same through a `return` out of a block of the callee: it lands
    // where `m6` was called.
    run_test_once(
        r##"
        def c0_i2201(x) = yield(x)
        def m6_i2201(x)
          [x].each { |e| yield e; return 1 }
          0
        end
        def d_i2201(a0)
          t = a0 * 2.0
          c0_i2201(a0) { |v| m6_i2201(v) { |w| t = 7 }; t.to_s }
        end
        def e_i2201(a0)
          t = a0 * 2.0; u = 0.5
          c0_i2201(a0) { |v| m6_i2201(v) { |w| t = 7 }; u = t + 1.0 }
          [t, u]
        end
        def f_i2201(a0)
          t = 5; u = 0
          c0_i2201(a0) { |v| m6_i2201(v) { |w| t = 7 if w > 50 }; u = [t, u] }
          [t, u]
        end
        r = []
        200.times { |i| a = i % 7 == 0 ? 98.0 : 1.5; r << [d_i2201(a), e_i2201(a), f_i2201(a)] }
        r.uniq
        "##,
    );
}

#[test]
fn call_left_only_by_a_return_keeps_its_float_home() {
    // Every compiled path out of the block-running call is the `return`,
    // so the calling frame never resumes. `k.to_s` deoptimizes for an
    // Integer after the Float stores went to the home.
    run_test_once(
        r##"
        def c0_i2201(x) = yield(x)
        def c4_i2201(x) = c0_i2201(x) { |y| yield y }
        def c9_i2201(x); 2.times { yield x }; x; end
        def g_i2201(a0, k)
          t = :a; u = 1.0; t = 2.5 if a0 < 99
          x = c4_i2201(a0) { |v| c9_i2201(v) { |q| s = k.to_s; t += 4.0; u += t }; return [:ret, t, u] if t > 5.0; t += 1.0; v }
          [t, u, x]
        end
        def h_i2201(a0, k)
          t = 1.0
          c0_i2201(a0) { |v| s = k.to_s; t += v; return t }
        end
        60.times { g_i2201(1.5, 2.5); h_i2201(1.5, 2.5) }
        [g_i2201(1.5, 3), g_i2201(1.5, 2.5), h_i2201(1.5, 3), h_i2201(1.5, 2.5)]
        "##,
    );
}

#[test]
fn loop_accumulators_through_a_break() {
    // `loop` comes back only through the `break`: the accumulators stay
    // in their homes across it, with a deoptimization (`k.to_s`) in the
    // middle and a store on the `break` path.
    run_test_once(
        r##"
        def l_i2201(lim, k)
          t = 0.0; u = 1.0; n = 0
          r = loop { t += 1.5; n += 1; s = k.to_s; u = u * 1.5 if n.odd?; break n * 2 if n > lim }
          [t, u, n, r]
        end
        def m_i2201(lim)
          t = 0.0; n = 0
          loop { t += 1.5; n += 1; if n > lim then t = n; break end }
          t
        end
        60.times { l_i2201(8, 2.5); m_i2201(8) }
        [l_i2201(8, 2.5), l_i2201(8, 2), l_i2201(0, 2.5), m_i2201(8), m_i2201(0)]
        "##,
    );
}
