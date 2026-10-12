//! The JIT's raw-u64 form (`LinkMode::U`): Integer sites that work in
//! [0, 2^64) compute on unboxed u64s. Each case warms the JIT (the test
//! thresholds are 5 calls / 15 loop iterations) and compares with CRuby.

extern crate monoruby;
use monoruby::tests::*;

const PRELUDE: &str = r##"
M = 0xffff_ffff_ffff_ffff
"##;

/// A masked xorshift: `<<` over the u64 width leaves through the checked
/// shift, stamps the site and recompiles it into the boxed forms, while
/// `&`, `^` and `>>` keep the raw form.
#[test]
fn masked_xorshift() {
    run_test_with_prelude(
        r##"
        x = 0x9E3779B97F4A7C15
        r = []
        40.times do
          x = (x ^ (x << 13)) & M
          x = x ^ (x >> 7)
          x = (x ^ (x << 17)) & M
          r << x if r.size < 4
        end
        [x, r]
        "##,
        PRELUDE,
    );
}

/// Bit-board shapes: `1 << sq` with `sq` reaching 63, masks with bit 63,
/// `b &= b - 1` popcount.
#[test]
fn bitboard() {
    run_test_with_prelude(
        r##"
        NOT_A = 0xfefefefefefefefe
        NOT_H = 0x7f7f7f7f7f7f7f7f
        def knight(b)
          ((b << 17) & NOT_A & M) | ((b << 15) & NOT_H & M) | ((b >> 17) & NOT_H) | ((b >> 15) & NOT_A)
        end
        def popcount(b)
          c = 0
          while b != 0
            b &= b - 1
            c += 1
          end
          c
        end
        total = 0
        boards = []
        64.times do |sq|
          b = 1 << sq
          k = knight(b)
          boards << k if sq % 9 == 0
          total += popcount(k)
        end
        [total, boards]
        "##,
        PRELUDE,
    );
}

/// Unsigned comparisons between values above 2^63, in both the boolean
/// and the fused (`if`) forms.
#[test]
fn unsigned_compare() {
    run_test_with_prelude(
        r##"
        a = (2**63) + 5
        b = 2**62
        c = M
        r = []
        20.times do |i|
          a = (a + i) & M
          r << [a > b, a < b, a >= b, a <= b, a == b, a != b, a > c, a <= c, a == a]
          r << (a > b ? 1 : 0) << (a < b ? 1 : 0) << (a >= c ? 1 : 0) << (a <= c ? 1 : 0)
          r << (if a == c then :eq else :ne end)
        end
        r
        "##,
        PRELUDE,
    );
}

/// A raw operand compared with a negative constant: the unbox guard misses
/// on the constant, the site is stamped and recompiled.
#[test]
fn compare_with_negative() {
    run_test_with_prelude(
        r##"
        a = 2**63
        r = []
        20.times do |i|
          a = (a ^ i) & M
          r << (a > -1) << (a < -5) << (-3 < a)
        end
        [a, r]
        "##,
        PRELUDE,
    );
}

/// Checked arithmetic: an `Add` / `Mul` leaving [0, 2^64) and a `Sub`
/// going negative each leave through the exit and get the Bignum answer.
#[test]
fn checked_add_sub_mul() {
    run_test_with_prelude(
        r##"
        a = 2**63
        r = []
        20.times do |i|
          r << (a + a) << (a + i) << (a - 1) << (i - a) << (a * 2) << (a * 1) << ((2**32 + 1) * (2**32 + i))
          a = (a + 7) & M
        end
        [a, r]
        "##,
        PRELUDE,
    );
}

/// A raw value consumed as a Value (a method call, interpolation, a Float
/// operation) is boxed back; the register stays as a cache.
#[test]
fn boxed_consumers() {
    run_test_with_prelude(
        r##"
        a = (2**63) | 5
        r = []
        20.times do |i|
          a = (a ^ (i * 3)) & M
          r << a.to_s(16) << "#{a}" << (a * 1.5) << a.even? << [a].first << (a <=> 1)
        end
        [a, r]
        "##,
        PRELUDE,
    );
}

/// A loop-carried hash (FNV-1a): the raw value crosses the loop back
/// edge and the `*` leaves through its exit (the product wraps).
#[test]
fn fnv1a_loop_carried() {
    run_test_with_prelude(
        r##"
        def fnv(s)
          h = 0xcbf29ce484222325
          s.each_byte do |c|
            h = ((h ^ c) * 0x100000001b3) & M
          end
          h
        end
        r = []
        20.times { |i| r << fnv("hello world #{i}") }
        r
        "##,
        PRELUDE,
    );
}

/// Operands outside the form: a value at or above 2^64, a negative value,
/// a wide Bignum. Each misses the unbox guard and the site falls back.
#[test]
fn out_of_range_operands() {
    run_test_with_prelude(
        r##"
        big = 2**64 + 3
        neg = -(2**63)
        huge = 2**200 + 1
        r = []
        20.times do |i|
          r << (big & 0xff) << (big ^ i) << (neg & M) << (neg >> 1) << (huge & M) << (huge >> 100) << (big > M) << (neg < M)
        end
        r
        "##,
        PRELUDE,
    );
}

/// Shift counts in a register, reaching and passing 64, in both
/// directions.
#[test]
fn register_shift_counts() {
    run_test_with_prelude(
        r##"
        a = 2**63 | 1
        r = []
        20.times do |i|
          k = i * 8
          r << (a >> k) << ((a << k) & M) << (1 << k) << ((M >> k) & 0xff)
        end
        r
        "##,
        PRELUDE,
    );
}

/// The same raw local on both sides and a copied raw local (`y = x`).
#[test]
fn shared_and_copied_operands() {
    run_test_with_prelude(
        r##"
        x = 2**63 + 9
        r = []
        20.times do |i|
          x = (x ^ (i + 1)) & M
          y = x
          z = y
          r << (x ^ x) << (x & x) << (x + x) << (y | 1) << (z >> 3) << (x == y) << (y < z)
          x = y
        end
        [x, r]
        "##,
        PRELUDE,
    );
}

/// A raw value live across a GC safepoint and a block call.
#[test]
fn across_gc_and_calls() {
    run_test_with_prelude(
        r##"
        a = 2**63 | 3
        r = []
        20.times do |i|
          a = (a ^ (i * 5)) & M
          GC.start if i % 7 == 0
          [1, 2].each { |e| r << e if i == 3 }
          r << a if i % 6 == 0
        end
        [a, r]
        "##,
        PRELUDE,
    );
}

/// Memory-address style arithmetic: a base above 2^62 plus small offsets.
#[test]
fn address_arithmetic() {
    run_test_with_prelude(
        r##"
        base = 0x7fff_0000_0000_0000
        r = []
        20.times do |i|
          p1 = base + i * 8
          p2 = p1 - 8
          r << (p1 - p2) << (p1 & 0xfff) << (p1 >> 12) << (p2 < p1)
        end
        r
        "##,
        PRELUDE,
    );
}

/// `&`-masked results through a method that returns them (the raw value
/// is boxed on return).
#[test]
fn returned_raw_values() {
    run_test_with_prelude(
        r##"
        def mix(h, c)
          ((h ^ c) * 0x100000001b3) & M
        end
        def rot(x, k)
          ((x << k) | (x >> (64 - k))) & M
        end
        h = 0xcbf29ce484222325
        r = []
        20.times do |i|
          h = mix(h, i)
          h = rot(h, 13)
          r << h if i % 5 == 0
        end
        [h, r]
        "##,
        PRELUDE,
    );
}

/// A raw temp passed straight as an argument: the box call must not land
/// on the argument window already stored below the stack pointer (a
/// `1 << 62` argument to `div` crashed this way).
#[test]
fn raw_temp_as_argument() {
    run_test_with_prelude(
        r##"
        def div(a, b)
          i = 0
          r = nil
          while i < 30
            r = a / b
            i += 1
          end
          r
        end
        def add3(a, b, c) = a + b + c
        res = []
        25.times do |n|
          [0, 3, 1 << 40].each { |d| res << (div(1 << 62, d) rescue :zero) }
          res << add3(1 << 63, (n ^ 0x8000_0000_0000_0001) & M, n)
        end
        res.uniq
        "##,
        PRELUDE,
    );
}
