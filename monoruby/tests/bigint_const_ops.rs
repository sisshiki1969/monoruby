//! Comparisons and arithmetic against bignum *constants* in JIT-compiled
//! code.
//!
//! dewasm-generated wasm runtimes lean on `x <= 0xffff_ffff_ffff_ffff`
//! masks and `x >= 0x8000_0000_0000_0000` sign tests. The bignum sits in
//! a constant (or literal) slot, so the integer lowering's fixnum guard
//! used to land on the constant itself and fail on every execution — a
//! deopt per call, forever (and for `&`-style arithmetic, a recompile
//! storm). Now:
//!
//! - a comparison with a bignum-constant operand folds to its
//!   sign-decided answer under a fixnum guard on the *other* operand
//!   (value and fused compare-branch forms both);
//! - integer arithmetic with a bignum-constant operand declines to the
//!   direct builtin call;
//! - a compile-time constant receiver skips the runtime class guard
//!   (its class is a static fact), in both the binop dispatcher and the
//!   method-call path.
//!
//! These tests pin the *semantics* across the JIT tier — every shape
//! runs hot enough to compile, with bignum-valued arguments mixed in so
//! the fold's fixnum guard deopts (and must stay correct) too.
extern crate monoruby;
use monoruby::tests::*;

#[test]
fn cmp_against_bignum_const_folds() {
    run_test(
        r##"
        BIGP = 0xffff_ffff_ffff_ffff
        BIGN = -(2**63)
        def t(x)
          [x < BIGP, x <= BIGP, x > BIGP, x >= BIGP, x == BIGP, x != BIGP,
           BIGP < x, BIGP <= x, BIGP > x, BIGP >= x, BIGP == x, BIGP != x,
           x < BIGN, x >= BIGN, x == BIGN, BIGN >= x]
        end
        r = []
        60.times { |i| r = t(i - 30) }
        # Bignum arguments take the fold's fixnum-guard deopt and must
        # still answer correctly; the fixnum-window boundaries must not
        # be confused with bignums.
        [r, t(2**64), t(-2**64), t(2**62), t(-2**62), t(2**62 - 1)]
        "##,
    );
}

#[test]
fn fused_cmpbr_against_bignum_literal_folds() {
    // The `x >= 0x8000_...` ? : shape compiles to the fused
    // compare-and-branch; the bignum is a *literal* (LinkMode::C via
    // FrozenLiteral), not a constant-cache entry.
    run_test(
        r##"
        def s64(x) = x >= 0x8000_0000_0000_0000 ? x - 0x1_0000_0000_0000_0000 : x
        def m64(x) = (x >= 0 && x <= 0xffff_ffff_ffff_ffff) ? x : (x & 0xffff_ffff_ffff_ffff)
        r = 0
        60.times { |i| r ^= s64(i) ^ m64(i * 3) }
        [r, s64(2**64 - 1), s64(2**63), s64(2**63 - 1), s64(5),
         m64(-1), m64(2**64 - 1), m64(2**70), m64(-2**70)]
        "##,
    );
}

#[test]
fn arith_with_bignum_const_takes_direct_call() {
    run_test(
        r##"
        BIGP = 0xffff_ffff_ffff_ffff
        BIGN = -(2**63)
        def t(x)
          [x & BIGP, x | 0x1_0000_0000_0000_0000, x ^ BIGP,
           x + BIGP, BIGP - x, x * BIGN, BIGP >> 4, x - BIGN]
        end
        r = []
        60.times { |i| r = t(i - 30) }
        [r, t(2**64), t(-2**63)]
        "##,
    );
}

#[test]
fn bignum_const_receiver_declines_raw_inlines() {
    // Integer#[] / #>> / #succ load the receiver raw on the strength of
    // the caller's fixnum guard; a bignum-constant receiver skips that
    // guard (its class is static) and must therefore take the ordinary
    // builtin call, not the raw-bits inline (`999…9[0]` once shifted the
    // bignum's heap pointer).
    run_test(
        r##"
        B = 999999999999999999999999999999999
        def t = [B[0], B[47], B[48], B[86], B[-1], B >> 4, B << 1, B.succ, B % 7]
        r = nil
        30.times { r = t }
        r
        "##,
    );
}

#[test]
fn bignum_const_cmp_respects_redefinition() {
    // The fold rides the basic-op machinery: redefining Integer#<=
    // must evict the folded body and dispatch the new method.
    run_test_once(
        r##"
        M = 0xffff_ffff_ffff_ffff
        def t(x) = x <= M
        r = []
        30.times { r << t(1) }
        class Integer
          def <=(other) = :redefined
        end
        30.times { r << t(1) }
        r.uniq
        "##,
    );
}
