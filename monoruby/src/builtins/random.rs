use super::*;
use num::{Signed, Zero};

/// Reference MT19937 (`mt19937ar`), the exact algorithm CRuby's
/// `random.c` uses, so the generated stream is bit-identical to CRuby.
#[derive(Clone, PartialEq, Eq, Hash)]
struct Mt {
    mt: [u32; 624],
    mti: usize,
}

const MT_N: usize = 624;
const MT_M: usize = 397;
const MT_MATRIX_A: u32 = 0x9908_b0df;
const MT_UPPER: u32 = 0x8000_0000;
const MT_LOWER: u32 = 0x7fff_ffff;

impl Mt {
    fn init_genrand(seed: u32) -> Self {
        let mut mt = [0u32; MT_N];
        mt[0] = seed;
        for i in 1..MT_N {
            mt[i] = 1_812_433_253u32
                .wrapping_mul(mt[i - 1] ^ (mt[i - 1] >> 30))
                .wrapping_add(i as u32);
        }
        Self { mt, mti: MT_N }
    }

    fn new_with_key(key: &[u32]) -> Self {
        let mut s = Self::init_genrand(19_650_218);
        let mt = &mut s.mt;
        let (mut i, mut j) = (1usize, 0usize);
        let mut k = MT_N.max(key.len());
        while k != 0 {
            mt[i] = (mt[i]
                ^ (mt[i - 1] ^ (mt[i - 1] >> 30)).wrapping_mul(1_664_525))
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
            mt[i] = (mt[i]
                ^ (mt[i - 1] ^ (mt[i - 1] >> 30)).wrapping_mul(1_566_083_941))
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

    fn next_u32(&mut self) -> u32 {
        if self.mti >= MT_N {
            let mt = &mut self.mt;
            for kk in 0..MT_N - MT_M {
                let y = (mt[kk] & MT_UPPER) | (mt[kk + 1] & MT_LOWER);
                mt[kk] = mt[kk + MT_M] ^ (y >> 1) ^ if y & 1 != 0 { MT_MATRIX_A } else { 0 };
            }
            for kk in MT_N - MT_M..MT_N - 1 {
                let y = (mt[kk] & MT_UPPER) | (mt[kk + 1] & MT_LOWER);
                mt[kk] = mt[kk + MT_M - MT_N]
                    ^ (y >> 1)
                    ^ if y & 1 != 0 { MT_MATRIX_A } else { 0 };
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
    fn fill_bytes(&mut self, dest: &mut [u8]) {
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

//
// Random class
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_class_under_obj("Random").id();
    globals.define_builtin_class_func_with(klass, "srand", random_srand, 0, 1, false);
    globals.define_builtin_class_func_with(klass, "rand", random_rand, 0, 1, false);
    globals.define_builtin_class_func_with(klass, "random_number", random_rand, 0, 1, false);
    globals.define_builtin_class_func(klass, "urandom", urandom, 1);
    globals.define_builtin_class_func(klass, "bytes", random_bytes, 1);
    globals.define_builtin_class_func(klass, "new_seed", new_seed, 0);
    globals.define_private_builtin_func_with(klass, "initialize", random_init_m, 0, 1, false);
    globals.define_builtin_func_with(klass, "rand", rand, 0, 1, false);
    globals.define_builtin_func(klass, "bytes", instance_bytes, 1);
    globals.define_builtin_func(klass, "seed", inst_seed, 0);
    globals.define_private_builtin_func(klass, "state", inst_state, 0);
    globals.define_builtin_func(klass, "==", inst_eq, 1);
}

// --- CRuby-compatible Mersenne-Twister helpers ---------------------------
//
// A per-instance `Random` stores its original seed and the number of
// 32-bit words drawn so far in two internal ivars. Each operation
// rebuilds the MT generator from the seed (CRuby's `init_by_array`,
// which `rand_mt::Mt::new_with_key` implements), skips the already-drawn
// words, performs the operation, and persists the new draw count. This
// reproduces CRuby's exact stream without serialising MT internal state.

fn ivar_seed() -> IdentId {
    IdentId::get_id("/random_seed")
}
fn ivar_cnt() -> IdentId {
    IdentId::get_id("/random_cnt")
}

/// Little-endian `u32` words of `|seed|` (`init_by_array` key). Zero -> `[0]`.
fn seed_words(seed: Value) -> Vec<u32> {
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

fn build_mt(seed: Value) -> Mt {
    // CRuby `rand_init`: a single-word seed uses `init_genrand`;
    // multi-word seeds use `init_by_array`.
    let words = seed_words(seed);
    if words.len() <= 1 {
        Mt::init_genrand(words[0])
    } else {
        Mt::new_with_key(&words)
    }
}

fn load_state(globals: &Globals, self_: Value) -> (Value, u64) {
    let seed = globals
        .store
        .get_ivar(self_, ivar_seed())
        .unwrap_or(Value::integer(0));
    let cnt = globals
        .store
        .get_ivar(self_, ivar_cnt())
        .and_then(|v| v.try_fixnum())
        .unwrap_or(0) as u64;
    (seed, cnt)
}

fn mt_at(seed: Value, cnt: u64) -> Mt {
    let mut mt = build_mt(seed);
    for _ in 0..cnt {
        mt.next_u32();
    }
    mt
}

/// CRuby `genrand_real` (53-bit, two draws).
fn next_real(mt: &mut Mt, cnt: &mut u64) -> f64 {
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
/// `limit` is given as little-endian `u32` digits. Returns the same.
fn limited_rand(mt: &mut Mt, cnt: &mut u64, limit: &[u32]) -> Vec<u32> {
    let len = limit.len();
    loop {
        let mut mask = 0u32;
        let mut boundary = true;
        let mut digits = vec![0u32; len];
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
            return digits;
        }
    }
}

/// Little-endian `u32` digits of a non-negative integer Value.
fn to_le_digits(big: &num::BigInt) -> Vec<u32> {
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

fn digits_to_value(digits: &[u32]) -> Value {
    let mut bytes = Vec::with_capacity(digits.len() * 4);
    for w in digits {
        bytes.extend_from_slice(&w.to_le_bytes());
    }
    let big = num::BigInt::from_bytes_le(num::bigint::Sign::Plus, &bytes);
    Value::bigint(big)
}

/// `rand(max)` for an integer `max` (> 0): uniform in `[0, max)`.
fn rand_int(mt: &mut Mt, cnt: &mut u64, max: &num::BigInt) -> Value {
    let limit = max - 1u32;
    let digits = limited_rand(mt, cnt, &to_le_digits(&limit));
    digits_to_value(&digits)
}

fn random_seed_value() -> Value {
    let mut buf = [0u8; 16];
    let _ = getrandom::fill(&mut buf);
    Value::bigint(num::BigInt::from_bytes_le(num::bigint::Sign::Plus, &buf))
}

/// CRuby `rb_to_int`-style seed coercion (`to_int`); Integers are kept
/// exactly (so huge Bignum seeds survive).
fn coerce_seed(vm: &mut Executor, globals: &mut Globals, arg: Value) -> Result<Value> {
    match arg.unpack() {
        RV::Fixnum(_) | RV::BigInt(_) => Ok(arg),
        _ => {
            let v = vm.invoke_method_inner(
                globals,
                IdentId::get_id("to_int"),
                arg,
                &[],
                None,
                None,
            )?;
            Ok(v)
        }
    }
}

fn to_bigint(v: Value) -> num::BigInt {
    match v.unpack() {
        RV::Fixnum(i) => num::BigInt::from(i),
        RV::BigInt(b) => b.clone(),
        RV::Float(f) => num::BigInt::from(f as i64),
        _ => num::BigInt::from(0),
    }
}

///
/// ### Random#initialize
///
/// - new(seed = Random.new_seed) -> Random
///
/// [https://docs.ruby-lang.org/ja/latest/method/Random/s/new.html]
#[monoruby_builtin]
fn random_init_m(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let seed = match lfp.try_arg(0) {
        None => random_seed_value(),
        Some(v) => coerce_seed(vm, globals, v)?,
    };
    globals.store.set_ivar(self_, ivar_seed(), seed)?;
    globals
        .store
        .set_ivar(self_, ivar_cnt(), Value::integer(0))?;
    Ok(Value::nil())
}

///
/// ### Random#seed
///
/// [https://docs.ruby-lang.org/ja/latest/method/Random/i/seed.html]
#[monoruby_builtin]
fn inst_seed(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let (seed, _) = load_state(globals, lfp.self_val());
    Ok(seed)
}

/// Private `state` accessor used by ruby/spec to compare two PRNGs.
/// Returns an Integer digest of the current MT state.
#[monoruby_builtin]
fn inst_state(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::hash::{Hash, Hasher};
    let (seed, cnt) = load_state(globals, lfp.self_val());
    let mt = mt_at(seed, cnt);
    let mut h = std::collections::hash_map::DefaultHasher::new();
    mt.hash(&mut h);
    Ok(Value::integer(h.finish() as i64))
}

///
/// ### Random#==
///
/// Two `Random`s are equal iff they would produce the same subsequent
/// sequence (same seed and same number of values drawn).
#[monoruby_builtin]
fn inst_eq(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let other = lfp.arg(0);
    if self_.class() != other.class() {
        return Ok(Value::bool(false));
    }
    let (sa, ca) = load_state(globals, self_);
    let (sb, cb) = load_state(globals, other);
    let eq = mt_at(sa, ca) == mt_at(sb, cb);
    Ok(Value::bool(eq))
}

///
/// ### Random.srand
///
/// - srand -> Integer
/// - srand(number) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Random/s/srand.html]
#[monoruby_builtin]
fn random_srand(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let old_seed = globals.random_seed();
    let new_seed = if lfp.try_arg(0).is_none() {
        None
    } else {
        match lfp.arg(0).unpack() {
            RV::Fixnum(i) => Some(i),
            RV::BigInt(b) => Some(b.to_i64().unwrap_or_else(|| {
                // Truncate BigInt to i64 by taking its low 64 bits.
                use num::bigint::Sign;
                let (sign, digits) = b.to_u64_digits();
                let low = digits.first().copied().unwrap_or(0) as i64;
                if sign == Sign::Minus { low.wrapping_neg() } else { low }
            })),
            RV::Float(f) => Some(f.trunc() as i64),
            _ => {
                return Err(MonorubyErr::no_implicit_conversion(
                    &globals.store,
                    lfp.arg(0),
                    INTEGER_CLASS,
                ));
            }
        }
    };
    globals.random_init(new_seed);
    Ok(Value::integer(old_seed as i64))
}

///
/// ### Random.rand
///
/// - rand -> Float
/// - rand(max) -> Integer | Float
/// - [NOT SUPPORTED] rand(range) -> Integer | Float
///
/// Also bound as `Random.random_number`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Random/s/rand.html]
#[monoruby_builtin]
fn random_rand(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let arg = match lfp.try_arg(0) {
        Some(v)
            if v.try_fixnum().is_none()
                && !v.is_float()
                && v.is_range().is_none()
                && !matches!(v.unpack(), RV::BigInt(_)) =>
        {
            // A non-numeric, non-Range maximum is coerced with #to_int.
            Some(vm.invoke_method_inner(
                globals,
                IdentId::get_id("to_int"),
                v,
                &[],
                None,
                None,
            )?)
        }
        other => other,
    };
    rand_with_arg(globals, arg)
}

///
/// ### Random#rand
///
/// - rand -> Float
/// - rand(max) -> Integer
/// - [NOT SUPPORTED]rand(range) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Random/i/rand.html]
#[monoruby_builtin]
fn rand(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let (seed, cnt) = load_state(globals, self_);
    let mut mt = mt_at(seed, cnt);
    let mut c = cnt;
    let result = inst_rand_op(vm, globals, &mut mt, &mut c, lfp.try_arg(0))?;
    globals
        .store
        .set_ivar(self_, ivar_cnt(), Value::integer(c as i64))?;
    Ok(result)
}

/// Core `Random#rand` dispatch operating on a reconstructed MT.
fn inst_rand_op(
    vm: &mut Executor,
    globals: &mut Globals,
    mt: &mut Mt,
    c: &mut u64,
    arg: Option<Value>,
) -> Result<Value> {
    let arg = match arg {
        Some(v) => v,
        None => return Ok(Value::float(next_real(mt, c))),
    };
    // Range: `rand(a..b)` / `rand(a...b)`.
    if let Some(r) = arg.is_range() {
        let start = r.start();
        let end = r.end();
        let excl = r.exclude_end();
        let any_float = start.is_float() || end.is_float();
        if !any_float
            && let (Some(s), Some(e)) = (start.try_fixnum(), end.try_fixnum())
        {
            let span = e - s + if excl { 0 } else { 1 };
            if span <= 0 {
                return Ok(Value::nil());
            }
            let v = rand_int(mt, c, &num::BigInt::from(span));
            return Ok(Value::integer(s + v.try_fixnum().unwrap_or(0)));
        }
        // If either end point is a Float, both are treated as Floats.
        let to_f = |v: Value| -> Option<f64> {
            v.try_float().or_else(|| v.try_fixnum().map(|i| i as f64))
        };
        if let (Some(s), Some(e)) = (to_f(start), to_f(end)) {
            let f = next_real(mt, c);
            return Ok(Value::float(s + f * (e - s)));
        }
        return Err(MonorubyErr::argumenterr("bad value for range"));
    }
    // Float maximum.
    if arg.is_float() {
        let max = arg.try_float().unwrap();
        if max < 0.0 {
            return Err(MonorubyErr::argumenterr(format!("invalid argument - {max}")));
        }
        let f = next_real(mt, c);
        return Ok(Value::float(if max == 0.0 { f } else { f * max }));
    }
    // Integer maximum (Fixnum or Bignum), or anything #to_int-coercible.
    let max = match arg.unpack() {
        RV::Fixnum(_) | RV::BigInt(_) => arg,
        _ => vm.invoke_method_inner(
            globals,
            IdentId::get_id("to_int"),
            arg,
            &[],
            None,
            None,
        )?,
    };
    let big = to_bigint(max);
    if big.is_negative() || big.is_zero() {
        return Err(MonorubyErr::argumenterr(format!(
            "invalid argument - {big}"
        )));
    }
    Ok(rand_int(mt, c, &big))
}

/// Shared implementation for `Random.rand`, `Random.random_number`,
/// and `Random#rand`. monoruby's per-instance state is not yet
/// distinct from the global PRNG, so all three currently share it.
fn rand_with_arg(globals: &mut Globals, arg: Option<Value>) -> Result<Value> {
    let arg = match arg {
        Some(v) => v,
        None => return Ok(Value::float(globals.random_gen())),
    };
    if let Some(max) = arg.try_fixnum() {
        if max <= 0 {
            return Err(MonorubyErr::argumenterr(format!(
                "invalid argument - {}",
                max
            )));
        }
        let f: f64 = globals.random_gen();
        Ok(Value::integer((f * max as f64) as i64))
    } else if let Some(max) = arg.try_float() {
        if max < 0.0 {
            return Err(MonorubyErr::argumenterr(format!(
                "invalid argument - {}",
                max
            )));
        }
        let f: f64 = globals.random_gen();
        if max == 0.0 {
            Ok(Value::float(f))
        } else {
            Ok(Value::float(f * max))
        }
    } else if arg.is_range().is_some() {
        Err(MonorubyErr::runtimeerr(
            "Range argument is not supported in Random#rand",
        ))
    } else {
        Err(MonorubyErr::runtimeerr(
            "the argument is not supported in Random#rand",
        ))
    }
}

///
/// ### Random.bytes
///
/// - bytes(size) -> String
///
/// Returns a binary string of `size` random bytes drawn from the
/// global PRNG. Equivalent to monoruby's `Random.urandom` but uses
/// the seeded global PRNG (for reproducibility under `Random.srand`)
/// rather than the OS entropy source.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Random/s/bytes.html]
#[monoruby_builtin]
fn random_bytes(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    random_bytes_inner(vm, globals, lfp.arg(0))
}

///
/// ### Random#bytes
///
/// - bytes(size) -> String
///
/// Draws `size` bytes from this instance's stream. CRuby fills the
/// buffer in little-endian 32-bit chunks (`rand_mt::Mt::fill_bytes`
/// implements the same), so the byte sequence is CRuby-identical.
#[monoruby_builtin]
fn instance_bytes(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let size = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    if size < 0 {
        return Err(MonorubyErr::argumenterr("negative string size".to_string()));
    }
    let size = size as usize;
    let (seed, cnt) = load_state(globals, self_);
    let mut mt = mt_at(seed, cnt);
    // ceil(size/4) 32-bit words are consumed (a trailing partial word
    // still draws a full u32, matching CRuby).
    let words = size.div_ceil(4) as u64;
    let mut buf = vec![0u8; size];
    mt.fill_bytes(&mut buf);
    globals
        .store
        .set_ivar(self_, ivar_cnt(), Value::integer((cnt + words) as i64))?;
    Ok(Value::bytes(buf))
}

///
/// ### Random.new_seed
///
/// - new_seed -> Integer
///
/// Returns a fresh, non-deterministic integer suitable as a seed for
/// `Random.new`. Drawn from OS entropy so it is unaffected by
/// `Kernel#srand` / `Random.srand`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Random/s/new_seed.html]
#[monoruby_builtin]
fn new_seed(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut buf = [0u8; 16];
    if let Err(e) = getrandom::fill(&mut buf) {
        return Err(MonorubyErr::runtimeerr(format!(
            "Random.new_seed: {}",
            e
        )));
    }
    let bytes = num::BigInt::from_bytes_le(num::bigint::Sign::Plus, &buf);
    Ok(Value::bigint(bytes))
}

///
/// ### Random.urandom
///
/// - urandom(size) -> String
#[monoruby_builtin]
fn urandom(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    random_bytes_inner(vm, globals, lfp.arg(0))
}

fn random_bytes_inner(vm: &mut Executor, globals: &mut Globals, size_arg: Value) -> Result<Value> {
    let size = if let Some(size) = size_arg.try_fixnum() {
        size
    } else if let Some(size) = size_arg.try_float() {
        size.round() as i64
    } else {
        size_arg.coerce_to_int_i64(vm, globals)?
    };
    if size == 0 {
        return Ok(Value::bytes(vec![]));
    }
    let size = if size < 0 {
        return Err(MonorubyErr::argumenterr("negative string size".to_string()));
    } else {
        size as usize
    };
    let pack_size = (size + 7) & !7;
    let mut v = vec![0; pack_size];
    for chunk in v.as_chunks_mut().0 {
        *chunk = globals.random_gen::<[u8; 8]>();
    }
    v.truncate(size);
    Ok(Value::bytes(v))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn random() {
        run_test(
            r#"
        Random.srand(1234)
        Random.srand(4567)
        "#,
        );
    }

    #[test]
    fn instance_rand() {
        run_test(
            r#"
        rng = Random.new(42)
        a = rng.rand
        a.is_a?(Float) && a >= 0.0 && a < 1.0
        "#,
        );
        run_test(
            r#"
        rng = Random.new(42)
        a = rng.rand(100)
        a.is_a?(Integer) && a >= 0 && a < 100
        "#,
        );
        run_test(
            r#"
        rng = Random.new(42)
        a = rng.rand(1.5)
        a.is_a?(Float) && a >= 0.0 && a < 1.5
        "#,
        );
        run_test(
            r#"
        rng = Random.new(42)
        a = rng.rand(0.0)
        a.is_a?(Float) && a >= 0.0 && a < 1.0
        "#,
        );
        run_test_error("Random.new(42).rand(0)");
        run_test_error("Random.new(42).rand(-1)");
        run_test_error("Random.new(42).rand(-100)");
        run_test_error("Random.new(42).rand(-1.5)");
        run_test_error("Random.new(42).rand(-0.1)");
    }

    #[test]
    fn random_rand() {
        run_test(
            r#"
        Random.srand(42)
        a = Random.rand
        a.is_a?(Float) && a >= 0.0 && a < 1.0
        "#,
        );
        run_test(
            r#"
        Random.srand(42)
        results = 10.times.map { Random.rand }
        results.all? { |x| x.is_a?(Float) && x >= 0.0 && x < 1.0 } && results.uniq.size == results.size
        "#,
        );
        // Class-form `Random.rand(n)` honors the int max — used to be ignored.
        run_test(
            r#"
        Random.srand(42)
        a = Random.rand(100)
        a.is_a?(Integer) && a >= 0 && a < 100
        "#,
        );
    }

    #[test]
    fn random_random_number() {
        run_test(
            r#"
        Random.srand(42)
        a = Random.random_number
        b = Random.random_number(50)
        a.is_a?(Float) && a >= 0.0 && a < 1.0 && b.is_a?(Integer) && b >= 0 && b < 50
        "#,
        );
    }

    #[test]
    fn random_class_bytes() {
        run_test(
            r#"
        s = Random.bytes(8)
        [s.is_a?(String), s.bytesize, s.encoding == Encoding::BINARY]
        "#,
        );
        run_test_error("Random.bytes(-1)");
    }

    #[test]
    fn random_instance_bytes() {
        run_test(
            r#"
        s = Random.new.bytes(15)
        [s.is_a?(String), s.bytesize]
        "#,
        );
    }

    #[test]
    fn random_new_seed() {
        // `Random.new_seed` returns an Integer (Bignum) and varies
        // between calls.
        run_test(
            r#"
        a = Random.new_seed
        b = Random.new_seed
        [a.is_a?(Integer), b.is_a?(Integer), a != b]
        "#,
        );
    }

    #[test]
    fn urandom() {
        run_test(
            r##"
        Random.urandom(0).size
        Random.urandom(1).size
        Random.urandom(7).size
        Random.urandom(7.9).size
        Random.urandom(16).size
        Random.urandom(20).size
        "##,
        );
        run_test_error("Random.urandom(-1)");
        run_test_error("Random.urandom('woo')");
    }

    #[test]
    fn random_cruby_parity() {
        // Bit-identical to CRuby (run_tests compares against `ruby`):
        // seed accessor, deterministic stream, bytes, ranges, ==, eql,
        // seed coercion.
        run_tests(&[
            r#"Random.new(36788).seed"#,
            r#"Random.new(3.4).seed"#,
            r#"Random.new(Rational(20,2)).seed"#,
            r#"Random.new(Complex(20)).seed"#,
            r#"Random.new(33).bytes(8).bytes"#,
            r#"(r=Random.new(33); r.bytes(1000); r.bytes(4).bytes)"#,
            r#"Random.new(2**(63*4)).bytes(4).bytes"#,
            r#"Random.new(33).rand(2**32)"#,
            r#"(20.times.map { Random.new(33).rand(90) }).uniq.size >= 1"#,
            r#"(p=Random.new(33); a=20.times.map{p.rand(90)}; q=Random.new(33); b=20.times.map{q.rand(90)}; a==b)"#,
            r#"(p=Random.new(7); 10.times.map { p.rand(1000000000000) })"#,
            r#"Random.new(42) == Random.new(42)"#,
            r#"Random.new(42) == Random.new(42.5)"#,
            r#"(a=Random.new(1); b=Random.new(1); a.rand; a == b)"#,
            r#"Random.new(1) == Object.new"#,
            r#"Random.new(42).rand(0.0..1).is_a?(Float)"#,
            r#"Random.new(42).rand(0..1.0).is_a?(Float)"#,
            r#"(r=Random.new(99); r.rand(1..6); r.rand(1...10))"#,
            r#"begin; Random.new(Complex(20,2)); :no; rescue RangeError; :range; end"#,
            r#"(o=Object.new; def o.to_int; 99; end; Random.rand(o).is_a?(Integer))"#,
        ]);
    }
}
