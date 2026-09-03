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
