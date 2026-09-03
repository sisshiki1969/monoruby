//! The default hasher for [`RubyMap`](crate::RubyMap) / [`RubySet`](crate::RubySet).
//!
//! A Ruby `Hash` probes its table on every `[]`, `[]=`, `key?` and
//! `fetch`, so the digest is on the hottest path a hash has. The
//! standard library's [`RandomState`](std::collections::hash_map::RandomState)
//! runs SipHash-1-3, which measured about a third of the cost of a whole
//! lookup (`perf` on a `h["name"]` loop: `string_digest` 18.7 % +
//! `SipHasher13::write` 13.9 %). CRuby does not pay that: its `st_hash`
//! is a seeded, non-cryptographic mixer.
//!
//! This is the same trade: a wyhash-style multiply-fold seeded from a
//! process-wide random value. Keeping the seed secret is what bounds
//! hash-flooding — an attacker who cannot predict it cannot pick
//! colliding keys ahead of time — while the mixing itself is a couple of
//! multiplies rather than a cryptographic permutation.

use std::hash::{BuildHasher, Hasher};

/// Multiply-fold: the low and high halves of a 128-bit product, xored.
/// One of these diffuses every input bit across the whole output.
#[inline(always)]
fn fold(a: u64, b: u64) -> u64 {
    let r = (a as u128).wrapping_mul(b as u128);
    (r as u64) ^ ((r >> 64) as u64)
}

/// Odd, high-entropy multipliers (wyhash's).
const P0: u64 = 0xa0761d6478bd642f;
const P1: u64 = 0xe7037ed1a0b428db;
const P2: u64 = 0x8ebc6af09c88c6e3;

#[inline(always)]
fn read8(bytes: &[u8]) -> u64 {
    u64::from_le_bytes(bytes[..8].try_into().unwrap())
}

#[inline(always)]
fn read4(bytes: &[u8]) -> u64 {
    u32::from_le_bytes(bytes[..4].try_into().unwrap()) as u64
}

/// The `BuildHasher` half: just the process seed.
#[derive(Clone, Copy, Debug)]
pub struct RubyRandomState {
    seed: u64,
}

impl RubyRandomState {
    /// A state on the process-wide random seed — the same one for every
    /// map, as [`RandomState`](std::collections::hash_map::RandomState)
    /// keys per process rather than per instance.
    pub fn new() -> Self {
        Self { seed: seed() }
    }
}

impl Default for RubyRandomState {
    fn default() -> Self {
        Self::new()
    }
}

/// The process seed, taken once from the standard library's own random
/// source (`RandomState` is seeded by the OS) so this crate needs no
/// randomness dependency of its own.
fn seed() -> u64 {
    use std::sync::OnceLock;
    static SEED: OnceLock<u64> = OnceLock::new();
    *SEED.get_or_init(|| {
        let s = std::collections::hash_map::RandomState::new();
        s.hash_one(0x5ee_du64) | 1
    })
}

impl BuildHasher for RubyRandomState {
    type Hasher = RubyHasher;

    #[inline]
    fn build_hasher(&self) -> RubyHasher {
        RubyHasher { h: self.seed }
    }
}

/// The running digest. Every `write_*` folds its input into `h`; the
/// final [`finish`](Hasher::finish) folds once more so the result
/// avalanches even for a single small write (a Symbol key is one
/// `write_u64`).
#[derive(Clone, Copy, Debug)]
pub struct RubyHasher {
    h: u64,
}

impl RubyHasher {
    #[inline(always)]
    fn add(&mut self, v: u64) {
        self.h = fold(self.h ^ v, P0);
    }
}

impl Hasher for RubyHasher {
    #[inline]
    fn finish(&self) -> u64 {
        fold(self.h, P2)
    }

    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        // Length first: it separates "ab" from "a" + "b" across two
        // writes, which `Hash` implementations that stream fields rely on.
        self.add(bytes.len() as u64);
        let mut b = bytes;
        while b.len() >= 16 {
            let lo = read8(b) ^ P1;
            let hi = read8(&b[8..]) ^ P2;
            self.add(fold(lo, hi));
            b = &b[16..];
        }
        match b.len() {
            0 => {}
            1..=3 => {
                let mut v = 0u64;
                for (i, &c) in b.iter().enumerate() {
                    v |= (c as u64) << (8 * i);
                }
                self.add(v);
            }
            4..=7 => {
                // Overlapping 4-byte reads cover 4..=7 without a branch
                // per length.
                self.add(read4(b) | (read4(&b[b.len() - 4..]) << 32));
            }
            _ => {
                // 8..=15, likewise with overlapping 8-byte reads.
                self.add(read8(b));
                self.add(read8(&b[b.len() - 8..]));
            }
        }
    }

    #[inline]
    fn write_u8(&mut self, n: u8) {
        self.add(n as u64);
    }

    #[inline]
    fn write_u16(&mut self, n: u16) {
        self.add(n as u64);
    }

    #[inline]
    fn write_u32(&mut self, n: u32) {
        self.add(n as u64);
    }

    #[inline]
    fn write_u64(&mut self, n: u64) {
        self.add(n);
    }

    #[inline]
    fn write_usize(&mut self, n: usize) {
        self.add(n as u64);
    }

    #[inline]
    fn write_u128(&mut self, n: u128) {
        self.add(n as u64);
        self.add((n >> 64) as u64);
    }

    #[inline]
    fn write_i8(&mut self, n: i8) {
        self.add(n as u64);
    }

    #[inline]
    fn write_i16(&mut self, n: i16) {
        self.add(n as u64);
    }

    #[inline]
    fn write_i32(&mut self, n: i32) {
        self.add(n as u64);
    }

    #[inline]
    fn write_i64(&mut self, n: i64) {
        self.add(n as u64);
    }

    #[inline]
    fn write_isize(&mut self, n: isize) {
        self.add(n as u64);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn digest(f: impl FnOnce(&mut RubyHasher)) -> u64 {
        let mut h = RubyRandomState::default().build_hasher();
        f(&mut h);
        h.finish()
    }

    fn of(bytes: &[u8]) -> u64 {
        digest(|h| h.write(bytes))
    }

    /// Every length branch of `write`: empty, the 1..=3 byte-at-a-time
    /// arm, the overlapping 4-byte and 8-byte arms, and the 16-byte loop
    /// with each possible tail.
    #[test]
    fn write_covers_every_length_branch() {
        let buf: Vec<u8> = (0u8..=63).collect();
        let mut seen = std::collections::HashSet::new();
        for len in 0..buf.len() {
            // Distinct lengths of distinct content must not collide; a
            // branch that dropped its bytes would show up here.
            assert!(seen.insert(of(&buf[..len])), "collision at len {len}");
        }
    }

    /// The length is mixed in, so a split write is not the same as the
    /// concatenation — `Hash` implementations that stream several fields
    /// depend on that.
    #[test]
    fn length_separates_split_writes() {
        let concat = of(b"ab");
        let split = digest(|h| {
            h.write(b"a");
            h.write(b"b");
        });
        assert_ne!(concat, split);
        assert_ne!(of(b"ab"), of(b"ab\0"));
        assert_ne!(of(b""), of(b"\0"));
    }

    /// One flipped bit anywhere in the input changes the digest, and
    /// changes it in the high bits too — hashbrown takes its control byte
    /// from the top 7, so a mixer that only diffuses downward would
    /// cluster.
    #[test]
    fn one_bit_changes_the_whole_digest() {
        let base = [0x11u8; 24];
        let h0 = of(&base);
        for byte in 0..base.len() {
            for bit in 0..8 {
                let mut probe = base;
                probe[byte] ^= 1 << bit;
                let h1 = of(&probe);
                assert_ne!(h0, h1, "byte {byte} bit {bit}");
                assert_ne!(h0 >> 57, h1 >> 57, "control bits, byte {byte} bit {bit}");
            }
        }
    }

    /// Sequential and strided integers are the classic way a weak mixer
    /// falls over: the low bits pick the bucket, so a mixer that leaves
    /// them alone piles every stride into one.
    #[test]
    fn integer_keys_spread_over_buckets() {
        for stride in [1u64, 8, 4096, 1 << 20] {
            let mut buckets = [0usize; 64];
            for i in 0..4096u64 {
                let h = digest(|h| h.write_u64(i.wrapping_mul(stride)));
                buckets[(h % 64) as usize] += 1;
            }
            // 4096 keys over 64 buckets is 64 each; allow a wide margin
            // and still catch clustering.
            let (min, max) = (
                *buckets.iter().min().unwrap(),
                *buckets.iter().max().unwrap(),
            );
            assert!(min >= 20 && max <= 160, "stride {stride}: {min}..{max}");
        }
    }

    /// Every `write_*` the `Hash` derive can reach, so a key type that
    /// streams anything other than `u64` still digests.
    #[test]
    fn every_write_method_digests() {
        let mut seen = std::collections::HashSet::new();
        assert!(seen.insert(digest(|h| h.write_u8(1))));
        assert!(seen.insert(digest(|h| h.write_u16(2))));
        assert!(seen.insert(digest(|h| h.write_u32(3))));
        assert!(seen.insert(digest(|h| h.write_u64(4))));
        assert!(seen.insert(digest(|h| h.write_u128(5))));
        assert!(seen.insert(digest(|h| h.write_usize(6))));
        assert!(seen.insert(digest(|h| h.write_i8(-1))));
        assert!(seen.insert(digest(|h| h.write_i16(-2))));
        assert!(seen.insert(digest(|h| h.write_i32(-3))));
        assert!(seen.insert(digest(|h| h.write_i64(-4))));
        assert!(seen.insert(digest(|h| h.write_isize(-5))));
        // The 128-bit write folds both halves in, so the high half is
        // not dropped.
        assert_ne!(
            digest(|h| h.write_u128(1)),
            digest(|h| h.write_u128(1 | (1 << 64)))
        );
    }

    /// One seed per process: two states digest alike, and a state
    /// survives being cloned into a map.
    #[test]
    fn state_is_stable_within_the_process() {
        let a = RubyRandomState::new();
        let b = RubyRandomState::default();
        assert_eq!(a.hash_one(0x1234u64), b.hash_one(0x1234u64));
        let c = a;
        assert_eq!(a.hash_one("key"), c.hash_one("key"));
    }
}
