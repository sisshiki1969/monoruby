use crate::*;

//
// Reference MT19937 (`mt19937ar`), the exact algorithm CRuby's
// `random.c` uses, so every stream drawn here — the seeded global PRNG
// behind `Kernel#srand` / `#rand` / `Array#shuffle` as well as `Random`
// instances (`builtins/random.rs` reuses these helpers) — is
// bit-identical to CRuby's.
//

#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct Mt {
    mt: [u32; 624],
    mti: usize,
}

const MT_N: usize = 624;
const MT_M: usize = 397;
const MT_MATRIX_A: u32 = 0x9908_b0df;
const MT_UPPER: u32 = 0x8000_0000;
const MT_LOWER: u32 = 0x7fff_ffff;

impl Mt {
    pub(crate) fn init_genrand(seed: u32) -> Self {
        let mut mt = [0u32; MT_N];
        mt[0] = seed;
        for i in 1..MT_N {
            mt[i] = 1_812_433_253u32
                .wrapping_mul(mt[i - 1] ^ (mt[i - 1] >> 30))
                .wrapping_add(i as u32);
        }
        Self { mt, mti: MT_N }
    }

    pub(crate) fn new_with_key(key: &[u32]) -> Self {
        let mut s = Self::init_genrand(19_650_218);
        let mt = &mut s.mt;
        let (mut i, mut j) = (1usize, 0usize);
        let mut k = MT_N.max(key.len());
        while k != 0 {
            mt[i] = (mt[i] ^ (mt[i - 1] ^ (mt[i - 1] >> 30)).wrapping_mul(1_664_525))
                .wrapping_add(key[j])
                .wrapping_add(j as u32);
            i += 1;
            j += 1;
            if i >= MT_N {
                mt[0] = mt[MT_N - 1];
                i = 1;
            }
            if j >= key.len() {
                j = 0;
            }
            k -= 1;
        }
        k = MT_N - 1;
        while k != 0 {
            mt[i] = (mt[i] ^ (mt[i - 1] ^ (mt[i - 1] >> 30)).wrapping_mul(1_566_083_941))
                .wrapping_sub(i as u32);
            i += 1;
            if i >= MT_N {
                mt[0] = mt[MT_N - 1];
                i = 1;
            }
            k -= 1;
        }
        mt[0] = 0x8000_0000;
        s
    }

    pub(crate) fn next_u32(&mut self) -> u32 {
        if self.mti >= MT_N {
            let mt = &mut self.mt;
            for kk in 0..MT_N - MT_M {
                let y = (mt[kk] & MT_UPPER) | (mt[kk + 1] & MT_LOWER);
                mt[kk] = mt[kk + MT_M] ^ (y >> 1) ^ if y & 1 != 0 { MT_MATRIX_A } else { 0 };
            }
            for kk in MT_N - MT_M..MT_N - 1 {
                let y = (mt[kk] & MT_UPPER) | (mt[kk + 1] & MT_LOWER);
                mt[kk] =
                    mt[kk + MT_M - MT_N] ^ (y >> 1) ^ if y & 1 != 0 { MT_MATRIX_A } else { 0 };
            }
            let y = (mt[MT_N - 1] & MT_UPPER) | (mt[0] & MT_LOWER);
            mt[MT_N - 1] = mt[MT_M - 1] ^ (y >> 1) ^ if y & 1 != 0 { MT_MATRIX_A } else { 0 };
            self.mti = 0;
        }
        let mut y = self.mt[self.mti];
        self.mti += 1;
        y ^= y >> 11;
        y ^= (y << 7) & 0x9d2c_5680;
        y ^= (y << 15) & 0xefc6_0000;
        y ^= y >> 18;
        y
    }

    /// CRuby `rb_rand_bytes`: little-endian 32-bit chunks; a trailing
    /// partial word still consumes a full draw.
    pub(crate) fn fill_bytes(&mut self, dest: &mut [u8]) {
        let mut chunks = dest.chunks_exact_mut(4);
        for c in &mut chunks {
            c.copy_from_slice(&self.next_u32().to_le_bytes());
        }
        let rem = chunks.into_remainder();
        if !rem.is_empty() {
            let b = self.next_u32().to_le_bytes();
            rem.copy_from_slice(&b[..rem.len()]);
        }
    }
}

/// Little-endian `u32` words of `|seed|` (`init_by_array` key). Zero -> `[0]`.
pub(crate) fn seed_words(seed: Value) -> Vec<u32> {
    use num::Signed;
    let big = match seed.unpack() {
        RV::Fixnum(i) => num::BigInt::from(i),
        RV::BigInt(b) => b.clone(),
        _ => num::BigInt::from(0),
    };
    let (_, bytes) = big.abs().to_bytes_le();
    let mut words: Vec<u32> = bytes
        .chunks(4)
        .map(|c| {
            let mut w = [0u8; 4];
            w[..c.len()].copy_from_slice(c);
            u32::from_le_bytes(w)
        })
        .collect();
    if words.is_empty() {
        words.push(0);
    }
    words
}

/// CRuby `rand_init`: a single-word seed uses `init_genrand`;
/// multi-word seeds use `init_by_array`.
pub(crate) fn build_mt(seed: Value) -> Mt {
    let words = seed_words(seed);
    if words.len() <= 1 {
        Mt::init_genrand(words[0])
    } else {
        Mt::new_with_key(&words)
    }
}

/// CRuby `genrand_real` (53-bit, two draws).
pub(crate) fn next_real(mt: &mut Mt, cnt: &mut u64) -> f64 {
    let a = (mt.next_u32() >> 5) as f64;
    let b = (mt.next_u32() >> 6) as f64;
    *cnt += 2;
    (a * 67108864.0 + b) * (1.0 / 9007199254740992.0)
}

fn make_mask(mut x: u32) -> u32 {
    x |= x >> 1;
    x |= x >> 2;
    x |= x >> 4;
    x |= x >> 8;
    x |= x >> 16;
    x
}

/// CRuby `limited_big_rand`: uniform integer in `[0, limit]` where
/// `limit` is given as little-endian `u32` digits, written into `digits`
/// (which must be as long as `limit`).
///
/// The buffer belongs to the caller so that the fixed-width callers can
/// hand over a stack array. They draw once per shuffled or sampled
/// element, and a `Vec` per draw put `malloc`/`free` at a third of
/// `Array#shuffle`'s profile — several times the cost of the MT draw the
/// allocation was wrapping.
///
/// A retry abandons a partially written pass, but the pass that returns
/// assigns every index, so no stale digit can survive into the result and
/// the buffer needs no clearing in between.
pub(crate) fn limited_rand_into(mt: &mut Mt, cnt: &mut u64, limit: &[u32], digits: &mut [u32]) {
    let len = limit.len();
    debug_assert_eq!(len, digits.len());
    loop {
        let mut mask = 0u32;
        let mut boundary = true;
        let mut retry = false;
        for i in (0..len).rev() {
            let lim = limit[i];
            mask = if mask != 0 { 0xffff_ffff } else { make_mask(lim) };
            let rnd = if mask != 0 {
                let r = mt.next_u32() & mask;
                *cnt += 1;
                if boundary {
                    if lim < r {
                        retry = true;
                        break;
                    }
                    if r < lim {
                        boundary = false;
                    }
                }
                r
            } else {
                0
            };
            digits[i] = rnd;
        }
        if !retry {
            return;
        }
    }
}

/// [`limited_rand_into`] for the variable-width (Bignum) callers, which
/// cannot size a buffer at compile time.
pub(crate) fn limited_rand(mt: &mut Mt, cnt: &mut u64, limit: &[u32]) -> Vec<u32> {
    let mut digits = vec![0u32; limit.len()];
    limited_rand_into(mt, cnt, limit, &mut digits);
    digits
}

/// CRuby `rb_random_ulong_limited`: uniform integer in `[0, max]`, drawn
/// from `mt` without allocating. This is the draw behind every
/// `Array#shuffle` / `#sample` index.
pub(crate) fn ulong_limited(mt: &mut Mt, cnt: &mut u64, max: u64) -> u64 {
    if max == 0 {
        return 0;
    }
    let limit = [(max & 0xffff_ffff) as u32, (max >> 32) as u32];
    let len = if limit[1] == 0 { 1 } else { 2 };
    let mut digits = [0u32; 2];
    limited_rand_into(mt, cnt, &limit[..len], &mut digits[..len]);
    digits[0] as u64 | ((digits[1] as u64) << 32)
}

/// Little-endian `u32` digits of a non-negative integer.
pub(crate) fn to_le_digits(big: &num::BigInt) -> Vec<u32> {
    let (_, bytes) = big.to_bytes_le();
    let mut d: Vec<u32> = bytes
        .chunks(4)
        .map(|c| {
            let mut w = [0u8; 4];
            w[..c.len()].copy_from_slice(c);
            u32::from_le_bytes(w)
        })
        .collect();
    if d.is_empty() {
        d.push(0);
    }
    d
}

pub(crate) fn digits_to_value(digits: &[u32]) -> Value {
    let mut bytes = Vec::with_capacity(digits.len() * 4);
    for w in digits {
        bytes.extend_from_slice(&w.to_le_bytes());
    }
    let big = num::BigInt::from_bytes_le(num::bigint::Sign::Plus, &bytes);
    Value::bigint(big)
}

/// `rand(max)` for an integer `max` (> 0): uniform in `[0, max)`.
pub(crate) fn rand_int(mt: &mut Mt, cnt: &mut u64, max: &num::BigInt) -> Value {
    let limit = max - 1u32;
    let digits = limited_rand(mt, cnt, &to_le_digits(&limit));
    digits_to_value(&digits)
}

///
/// The seeded global PRNG behind `Kernel#srand` / `#rand`,
/// `Random.rand`, `Random.bytes` and the default paths of
/// `Array#shuffle` / `#sample`. CRuby models this as `Random::DEFAULT`;
/// keeping the stream and its consumption identical to CRuby makes
/// `srand(n)`-seeded sequences (and specs pinning them) reproducible.
///
pub struct Prng {
    mt: Mt,
}

impl Prng {
    pub fn new() -> Self {
        Self {
            mt: Mt::init_genrand(0),
        }
    }

    /// Seed with the full integer Value (CRuby `rand_init` feeds every
    /// word of the seed, not just the low bits).
    pub(crate) fn seed_value(&mut self, seed: Value) {
        self.mt = build_mt(seed);
    }

    /// Draw a fresh nonnegative seed from OS entropy, seed with it, and
    /// return it (CRuby reports the system-initialized seed).
    pub(crate) fn seed_entropy(&mut self) -> i64 {
        let mut buf = [0u8; 4];
        if let Err(err) = getrandom::fill(&mut buf) {
            panic!("from_entropy failed: {}", err);
        }
        let seed = (u32::from_ne_bytes(buf) & 0x7fff_ffff) as i64;
        self.seed_value(Value::integer(seed));
        seed
    }

    /// CRuby `genrand_real`: uniform Float in `[0, 1)`.
    pub(crate) fn next_real(&mut self) -> f64 {
        let mut cnt = 0;
        next_real(&mut self.mt, &mut cnt)
    }

    /// CRuby `int_pair_to_real_inclusive`: uniform Float in `[0, 1]`
    /// (both ends included), two draws — the mapping an *inclusive*
    /// float-range `rand` uses.
    pub(crate) fn next_real_inclusive(&mut self) -> f64 {
        let a = self.mt.next_u32() as u128;
        let b = self.mt.next_u32() as u128;
        let x = (a << 32) | b;
        let m = (1u128 << 53) | 1;
        let r = ((x * m) >> 64) as u64;
        (r as f64) * (1.0 / 9007199254740992.0)
    }

    /// CRuby `rb_random_ulong_limited`: uniform integer in `[0, max]`.
    pub(crate) fn ulong_limited(&mut self, max: u64) -> u64 {
        // This generator owns a live `Mt`, so it has no draw count to
        // keep — unlike a `Random` instance, which replays from its seed.
        let mut cnt = 0;
        ulong_limited(&mut self.mt, &mut cnt, max)
    }

    /// `rand(max)` for a positive integer `max`: uniform in `[0, max)`.
    pub(crate) fn rand_int(&mut self, max: &num::BigInt) -> Value {
        let mut cnt = 0;
        rand_int(&mut self.mt, &mut cnt, max)
    }

    pub(crate) fn fill_bytes(&mut self, dest: &mut [u8]) {
        self.mt.fill_bytes(dest)
    }
}
