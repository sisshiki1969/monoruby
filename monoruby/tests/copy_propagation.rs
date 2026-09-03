//! Local-variable copies in the JIT: `%dst = %src` shares `src`'s GP register
//! instead of storing through the stack home (`GpRegFile` holds a set of
//! slots per register). Every case below mixes copies with the things that
//! must still see the right value: in-place integer ops on a shared
//! register, `Mul`/`Div` clobbering an operand register, overflow / type
//! deopts with copies outstanding, calls and allocations (flushes), loop
//! back-edges, and non-fixnum values riding in the register.

extern crate monoruby;
use monoruby::tests::*;

/// Wrap `body` in a method called enough times for the method JIT, and
/// return the last result.
fn jit(body: &str) -> String {
    format!(
        r##"
        def __f(x)
          {body}
        end
        r = nil
        40.times {{ |i| r = __f(i) }}
        r
        "##
    )
}

#[test]
fn copy_chains_of_fixnums() {
    // The 30k_variables shape: chains of copies between integer ops.
    run_test(&jit(
        r##"
        v0 = x
        v1 = (v0 + 8) & 4095
        v2 = v1
        v3 = v2
        v4 = v3
        v5 = v4 ^ v1
        v6 = v5
        v7 = v6
        v8 = (v7 + v3) & 4095
        v9 = v8
        v10 = v9
        [v10, v9, v6, v2, v0]
        "##,
    ));
}

#[test]
fn in_place_op_on_a_shared_register() {
    // `a += 1` must not clobber `b`, which shares `a`'s register.
    run_test(&jit(
        r##"
        a = x
        b = a
        a += 1
        c = b
        b -= 3
        [a, b, c]
        "##,
    ));
    run_test(&jit(
        r##"
        a = x + 1
        b = a
        a = a + a
        [a, b]
        "##,
    ));
}

#[test]
fn mul_and_div_clobber_a_shared_operand() {
    run_test(&jit(
        r##"
        a = x + 3
        b = a
        c = 7 * b
        d = 1000 / b
        e = b % 5
        [a, b, c, d, e]
        "##,
    ));
}

#[test]
fn deopt_with_copies_outstanding() {
    // Overflow to Bignum after the copies: the side exit must re-home the
    // copies from the shared register.
    run_test(&jit(
        r##"
        a = x + 4611686018427387900
        b = a
        c = b
        d = c + 10
        [a, b, c, d]
        "##,
    ));
    // Type deopt: the copied value is not a fixnum after all.
    run_test(&jit(
        r##"
        a = x > 30 ? 1.5 : x
        b = a
        c = b
        d = c + 1
        [a, b, c, d]
        "##,
    ));
}

#[test]
fn eviction_under_pressure_before_an_overflow_deopt() {
    // plb2/bedcov's `splitmix32`: with the copy `z = x` keeping `x` resident
    // (dirty) for the whole body, the second multiply — which overflows to a
    // Bignum — needs a result register and must evict `x`'s. That eviction
    // has to be spilled *before* the deopt snapshot; a snapshot still naming
    // the evicted register would store the multiply's garbage into `x`.
    run_test(
        r##"
        def splitmix32(x)
          x = (x + 0x9e3779b9) & 0xffffffff
          z = x
          z = (z ^ (z >> 16)) * 0x21f0aaad & 0xffffffff
          z = (z ^ (z >> 15)) * 0x735a2d97 & 0xffffffff
          return z ^ (z >> 15), x
        end
        x = 11
        i = 0
        r = nil
        while i < 200
          r, x = splitmix32(x)
          i += 1
        end
        [r, x]
        "##,
    );
    // The same pressure without a copy: three live results plus an operand
    // fill the four-register file, so the overflowing op evicts a live local.
    run_test(&jit(
        r##"
        a = x * 3 + 1
        b = a * 5 + 2
        c = b * 7 + 3
        d = c * 0x3fffffffffffff + 4
        [a, b, c, d]
        "##,
    ));
}

#[test]
fn copies_across_calls_and_allocations() {
    run_test(&jit(
        r##"
        a = x
        b = a
        s = "z" * 3
        arr = [b, a]
        c = b
        arr << c.to_s
        [a, b, c, s, arr]
        "##,
    ));
}

#[test]
fn copies_of_non_fixnum_values() {
    run_test(&jit(
        r##"
        a = x.odd? ? nil : "str"
        b = a
        c = b
        f = 2.5
        g = f
        h = g + 1.0
        s = :sym
        t = s
        [a, b, c, g, h, t]
        "##,
    ));
}

#[test]
fn copies_in_loops_and_branches() {
    run_test(&jit(
        r##"
        acc = 0
        i = 0
        while i < 20
          a = i
          b = a
          c = b
          if i.even?
            a = c + 1
          else
            b = a * 2
          end
          acc += a + b + c
          i += 1
        end
        [acc, a, b, c]
        "##,
    ));
    run_test(
        r##"
        acc = 0
        i = 0
        while i < 100
          v = i
          w = v
          u = w
          acc += u ^ (v + 1)
          i = u + 1
        end
        acc
        "##,
    );
}

#[test]
fn copy_of_a_call_result_and_self_return() {
    run_test(&jit(
        r##"
        a = x.succ
        b = a
        c = b + 1
        s = "q"
        d = s.freeze
        e = d
        [a, b, c, e]
        "##,
    ));
}
