//! 64-bit Integer arithmetic around the fixnum / Bignum boundary: masks,
//! hashes, bit boards and shifts whose values live in [0, 2^64). Written
//! for the JIT's former raw-u64 form, kept as result checks for the
//! two-arm dispatch and the inline Bignums. Each case warms the JIT (the
//! test thresholds are 5 calls / 15 loop iterations) and compares with
//! CRuby.

extern crate monoruby;
use monoruby::tests::*;

const PRELUDE: &str = r##"
M = 0xffff_ffff_ffff_ffff
"##;

/// A masked xorshift: `<<` overflows the fixnum range every iteration.
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

/// A value above 2^63 compared with a negative constant.
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

/// Arithmetic leaving [0, 2^64): an `Add` / `Mul` past 2^64 and a `Sub`
/// going negative each get the exact Bignum answer.
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

/// A 64-bit value consumed as a Value (a method call, interpolation, a
/// Float operation).
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

/// A loop-carried hash (FNV-1a): the `*` overflows and is masked.
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

/// Operands outside [0, 2^64): a value at or above 2^64, a negative
/// value, a wide Bignum.
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

/// The same local on both sides and a copied local (`y = x`).
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

/// A 64-bit value live across a GC safepoint and a block call.
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

/// `&`-masked results through a method that returns them.
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

/// A 64-bit temp passed straight as an argument (`1 << 62` to `div`).
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
        out = []
        25.times do |n|
          [0, 3, 1 << 40].each { |d| out << (div(1 << 62, d) rescue :zero) }
          out << add3(1 << 63, (n ^ 0x8000_0000_0000_0001) & M, n)
        end
        out.uniq
        "##,
        PRELUDE,
    );
}

/// An operand still in a GP register when it reaches an overflowing
/// site (`c` read back as nil after the `*` overflowed).
#[test]
fn gp_resident_operand_before_exit() {
    run_test_with_prelude(
        r##"
        def f(x)
          a = x * 3 + 1
          b = a * 5 + 2
          c = b * 7 + 3
          d = c * 0x3fffffffffffff + 4
          e = (c ^ 0x8000_0000_0000_0000) + x
          [a, b, c, d, e]
        end
        out = []
        40.times { |i| out << f(i) if i % 13 == 0 }
        out
        "##,
        PRELUDE,
    );
}

/// Results at the boundaries of [0, 2^64), masked with `& c`: they must
/// agree with the exact Bignum result masked.
#[test]
fn wrapping_boundaries() {
    run_test_with_prelude(
        r##"
        def w(a, b, k)
          [(a + b) & M, (a - b) & M, (b - a) & M, (a * b) & M, (a << k) & M, (a << k) & 0xff,
           (a << 1) & M, (a << 63) & M, (a << 64) & M, (a << 100) & M, (b + 1) & M, (b * 3) & 0xffff,
           b & (b - 1), (a + b) & 0x8000_0000_0000_0000]
        end
        out = []
        vals = [M, M - 1, 2**63, 2**63 - 1, 2**63 + 1, 2**62, 1, 0]
        ks = [0, 1, 7, 31, 63, 64, 65, 100]
        40.times do |i|
          a = vals[i % vals.size]
          b = vals[(i * 3 + 1) % vals.size]
          out << w(a, b, ks[i % ks.size])
        end
        out
        "##,
        PRELUDE,
    );
}

/// Chains of operators between the arithmetic and the mask.
#[test]
fn wrapping_chains() {
    run_test_with_prelude(
        r##"
        def xs(x)
          x = (x ^ (x << 13)) & M
          x ^= x >> 7
          (x ^ (x << 17)) & M
        end
        def mix(a, b, c)
          [((a + b) * c) & M, ((a << 3) | 1) & M, (((a * b) ^ c) + 7) & M, ((a - b) ^ (b - a)) & M, ((a << 5) & 0xff0) & M]
        end
        x = 0x9E3779B97F4A7C15
        out = []
        30.times do |i|
          x = xs(x)
          out << x if i % 7 == 0
          out << mix(x, (x >> 3) | 1, 0x100000001b3)
        end
        out
        "##,
        PRELUDE,
    );
}

/// Chains without a usable mask: no mask, a negative mask, a mask at or
/// above 2^64, a Float operand further down the chain, a local as the
/// intermediate.
#[test]
fn no_fusion_for_other_consumers() {
    run_test_with_prelude(
        r##"
        def f(a, b, fl)
          t = a * b
          [(a << 3) | b, (a + b) & -1, (a * b) & (2**64 + 0xff), (a + b) * 1.5, ((a + b) + fl), t & M,
           (a * b) + 1 & M, ((a << 10) >> 2) & M, (a - b) & M, (b - a)]
        end
        out = []
        20.times do |i|
          a = 2**63 + i
          b = 2**62 + 3 * i
          out << f(a, b, 0.5)
        end
        out
        "##,
        PRELUDE,
    );
}

/// A chain operand that turns negative or reaches 2^64 on some calls.
#[test]
fn exit_inside_a_fused_chain() {
    run_test_with_prelude(
        r##"
        def g(a, b, y)
          z = y & M
          [((a * b) ^ y) & M, ((a << 9) + y) & M, z]
        end
        out = []
        ys = [5, 7, 11, -5, 2**64 + 1, 9]
        30.times do |i|
          out << (g(2**63 + i, 0x100000001b3, ys[i % ys.size]) rescue :err)
        end
        out
        "##,
        PRELUDE,
    );
}

/// A loop-carried value in a `while` loop inside a method, entered with
/// a fixnum, a Bignum below 2^64 and a constant.
#[test]
fn loop_carried_in_a_method() {
    run_test_with_prelude(
        r##"
        def xs(x, n)
          i = 0
          while i < n
            x ^= (x << 13) & M
            x ^= x >> 7
            x ^= (x << 17) & M
            i += 1
          end
          x
        end
        def fnv(s)
          h = 0xcbf29ce484222325
          i = 0
          while i < s.bytesize
            h = ((h ^ s.getbyte(i)) * 0x100000001b3) & M
            i += 1
          end
          h
        end
        out = []
        40.times do |i|
          out << xs(88172645463325252, 20 + i)
          out << xs(0x9E3779B97F4A7C15 + i, 25)
          out << fnv("hello world #{i}")
        end
        out
        "##,
        PRELUDE,
    );
}

/// A top-level `while` loop (a loop JIT entered from the interpreter)
/// carrying a 64-bit value.
#[test]
fn loop_carried_at_top_level() {
    run_test_with_prelude(
        r##"
        x = 0x9E3779B97F4A7C15
        acc = 0
        i = 0
        while i < 200
          x ^= (x << 13) & M
          x ^= x >> 7
          x ^= (x << 17) & M
          acc = (acc + (x >> 60)) & M
          i += 1
        end
        [x, acc]
        "##,
        PRELUDE,
    );
}

/// The carried value enters the loop outside [0, 2^64) on some calls: a
/// negative seed, 2^64 or more.
#[test]
fn loop_carried_entry_out_of_range() {
    run_test_with_prelude(
        r##"
        def mix(x, n)
          i = 0
          while i < n
            x = (x * 6364136223846793005 + 1442695040888963407) & M
            x ^= x >> 29
            i += 1
          end
          x
        end
        seeds = [1, 2**63 + 5, 7, -3, 2**64 + 9, 11, -(2**70), 13]
        out = []
        40.times do |i|
          out << mix(seeds[i % seeds.size], 16 + i % 5)
        end
        out
        "##,
        PRELUDE,
    );
}

/// The carried value is also consumed boxed each iteration (pushed into
/// an Array, passed to a method).
#[test]
fn loop_carried_with_boxed_consumers() {
    run_test_with_prelude(
        r##"
        def note(v, a)
          a << v if a.size < 3
          v
        end
        def walk(x, n)
          a = []
          i = 0
          while i < n
            x = (x ^ (x << 7)) & M
            x = (x ^ (x >> 9)) & M
            note(x, a)
            i += 1
          end
          [x, a]
        end
        out = []
        40.times do |i|
          out << walk(0xdeadbeefcafebabe + i, 24)
        end
        out
        "##,
        PRELUDE,
    );
}

/// Nested loops, each carrying a 64-bit value.
#[test]
fn loop_carried_nested() {
    run_test_with_prelude(
        r##"
        def grid(seed, n, m)
          x = seed & M
          acc = 0
          i = 0
          while i < n
            y = x
            j = 0
            while j < m
              y = (y ^ (y << 11)) & M
              y ^= y >> 5
              j += 1
            end
            x = (x + y * 3) & M
            acc = (acc ^ (x >> 7)) & M
            i += 1
          end
          [x, acc]
        end
        out = []
        40.times do |i|
          out << grid(0x1234567887654321 + i, 20, 18)
        end
        out
        "##,
        PRELUDE,
    );
}
