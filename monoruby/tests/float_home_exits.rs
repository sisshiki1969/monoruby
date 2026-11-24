//! A Float local stored by a block compiled into its method, and every way
//! out of the call the block runs under (#2188 and the family around it).
//!
//! The JIT keeps such a local as a raw f64 in a home of the method's frame
//! and may leave the boxed slot stale while the call lasts. The frame has
//! one write-back for every exit taken under the call, so the local has
//! to be either current in its slot the whole time, or current in a home
//! the frame already held when the call began, from then until the call
//! is over:
//!
//! - a home first issued under the call is not yet filled on an exit
//!   taken before the store that issued it (#2188: `t` read as 0.0 after
//!   a block had left the unit and two stores followed);
//! - a deferral that ends under the call (`proc { }`, `binding`, a join,
//!   a `break` nothing else comes back from) leaves the slot stale on an
//!   exit taken before it ended: the local read as `nil`;
//! - two locals copied from one float share its register, and a store
//!   into one changed the other.
//!
//! The last test is a neighbour found by the same generated programs: a
//! write owed on a return path of a frameless block, sent to the wrong
//! frame.

use monoruby::tests::*;

#[test]
fn float_stores_after_a_block_left_the_unit() {
    run_test_once(
        r##"
        def c0_i2188(x) = yield(x)
        def c2_i2188(x, &b) = b.call(x)
        def a_i2188(a)
          t = 1.0
          c0_i2188(a) { |v| c2_i2188(v) { t += 10.0 }; t += 100.0; t += 1000.0 }
          t
        end
        def b_i2188(a)
          t = 1.0
          c0_i2188(a) { |v| pr = proc { t += 10.0 }; pr.call; t += 100.0; t += 1000.0 }
          t
        end
        def c_i2188(a)
          t = 1.0
          c0_i2188(a) { |v| pr = proc { 5 }; t += 100.0; t += 1000.0 }
          t
        end
        r = nil
        60.times { r = [a_i2188(1.5), b_i2188(1.5), c_i2188(1.5)] }
        r
        "##,
    );
}

#[test]
fn exit_before_a_block_leaves_the_unit() {
    // `v.to_s` deoptimizes for an Integer, before the `proc { }` /
    // `binding` that ends the deferral of `t`, and before or after the
    // store.
    run_test_once(
        r##"
        def c1_i2188(x) = yield(x)
        def d1_i2188(a)
          t = 1.5
          c1_i2188(a) { |v| s = v.to_s; t += 1.0; pr = proc { 5 } }
          t
        end
        def d2_i2188(a)
          t = 1.5
          c1_i2188(a) { |v| t += 1.0; s = v.to_s; pr = proc { 5 } }
          t
        end
        def d3_i2188(a)
          t = 1.5
          c1_i2188(a) { |v| s = v.to_s; t += 1.0; bb = binding }
          t
        end
        def d4_i2188(a)
          t = 1.5
          c1_i2188(a) { |v| t += 1.0; s = v.to_s; pr = proc { 5 } if v > 50.0 }
          t
        end
        def d5_i2188(a)
          t = 0.0
          c1_i2188(a) { |v| c1_i2188(v) { |q| t += q }; bb = binding; t += 100.0; t += 1000.0 }
          t
        end
        60.times do |i|
          d1_i2188(1.5); d2_i2188(1.5); d3_i2188(1.5); d5_i2188(1.5)
          d4_i2188(i.odd? ? 1.5 : 99.0)
        end
        [d1_i2188(2), d2_i2188(2), d3_i2188(2), d4_i2188(2), d5_i2188(2)]
        "##,
    );
}

#[test]
fn exit_before_a_break() {
    // `loop` comes back only through the `break`, and so does the block
    // of `once`; `each` also returns normally and keeps its deferral.
    run_test_once(
        r##"
        def once_i2188 = yield
        def e1_i2188(k)
          t = 0.0; n = 0
          loop { t += 1.5; n += 1; s = k.to_s; break if n > 5 }
          t
        end
        def e2_i2188(a, k)
          y = 0.5
          once_i2188 { y += a; s = k.to_s; break }
          y
        end
        def e3_i2188(a, lim, k)
          s = 0.0
          a.each { |x| s += x; t = k.to_s; break if s > lim }
          s
        end
        a = [1.0, 2.0, 3.0, 4.0]
        60.times { e1_i2188(2.5); e2_i2188(1.5, 2.5); e3_i2188(a, 5.5, 2.5) }
        [e1_i2188(2), e2_i2188(1.5, 2), e3_i2188(a, 5.5, 2), e3_i2188(a, 100.0, 2), e3_i2188(a, 0.5, 3)]
        "##,
    );
}

#[test]
fn locals_copied_from_one_float() {
    run_test_once(
        r##"
        def c0_i2188(x) = yield(x)
        def g_i2188(a0)
          t = a0; u = a0
          c0_i2188(a0) { |v| t -= 0.5; u += t; if v > 50 then t += 1.0 else u += 1.0 end }
          [t, u]
        end
        def h_i2188(a0)
          t = a0
          c0_i2188(1.0) { |v| t += v }
          [a0, t]
        end
        r = []
        200.times { |i| r << g_i2188(i % 7 == 0 ? 98.0 : 1.5) << h_i2188(i % 7 == 0 ? 98.0 : 1.5) }
        r.uniq
        "##,
    );
}

#[test]
fn literal_surrendered_on_a_return_of_a_frameless_block() {
    // The inner block is compiled without a frame of its own, inside
    // `c5_i2188`. Only one arm of its `if` stores `t`, so the other
    // return path owes the write of the literal `t` still was: it went
    // to an address past the frame of `k_i2188`, into a temporary of the
    // caller (an element of the Array being built).
    run_test_once(
        r##"
        def c1_i2188(x); yield x; end
        def c5_i2188(x); yield x; end
        def k_i2188(a0)
          t = 1.0; u = 2.0
          c1_i2188(a0) { |v| c5_i2188(v) { |w| if w > 50 then t += 1.0 else u += 1.0 end }; t = v; u += t }
          [t, u]
        end
        60.times { k_i2188(1.5) }
        [[10, 20, 30, 40, 50, 60, k_i2188(1.5)], [10, 20, 30, 40, 50, k_i2188(1.5)], k_i2188(99.0)]
        "##,
    );
}
