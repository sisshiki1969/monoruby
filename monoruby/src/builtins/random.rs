use super::*;
use crate::globals::prng::{Mt, build_mt, next_real, rand_int, ulong_limited};
use num::{Signed, Zero};

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
    globals.define_private_builtin_func(klass, "initialize_copy", random_init_copy, 1);
    globals.define_builtin_func_with(klass, "rand", rand, 0, 1, false);
    // CRuby defines `Random::Formatter#random_number` in C (random.c) and the
    // stdlib `random/formatter` builds its helpers on it; binding it here on
    // the instance side (the class side is bound above) keeps the vendored
    // formatter working for `Random.new.alphanumeric` etc. The Formatter
    // module wiring itself lives in `builtins/random.rb`.
    globals.define_builtin_func_with(klass, "random_number", rand, 0, 1, false);
    globals.define_builtin_func(klass, "bytes", instance_bytes, 1);
    globals.define_builtin_func(klass, "seed", inst_seed, 0);
    globals.define_private_builtin_func(klass, "state", inst_state, 0);
    globals.define_builtin_func(klass, "==", inst_eq, 1);
}

// --- CRuby-compatible Mersenne-Twister helpers ---------------------------
//
// A per-instance `Random` keeps three internal ivars: its original seed
// (`Random#seed`), the number of 32-bit words drawn so far, and the
// generator's *live* state — the 624-word MT table plus position,
// serialized into a binary String (`Mt::to_bytes`). Each operation
// deserializes the state, draws, and writes the advanced state back, so
// a draw costs O(1) regardless of how many preceded it.
//
// It used to persist only `(seed, count)` and rebuild the generator from
// the seed on every call, replaying every previously drawn word — O(n)
// per draw, quadratic over a generator's lifetime. yjit-bench `splay`
// draws from one `Random.new(42)` forever: each iteration was slower
// than the last (1.0 s → 1.9 s and climbing), the harness never
// converged, and the run hit the 400 s cap. The rebuild path survives
// as `mt_at`, the fallback for an instance whose state ivar is missing
// (e.g. an allocated-but-uninitialized object).

fn ivar_seed() -> IdentId {
    IdentId::get_id("/random_seed")
}
fn ivar_cnt() -> IdentId {
    IdentId::get_id("/random_cnt")
}
fn ivar_state() -> IdentId {
    IdentId::get_id("/random_state")
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

/// The instance's live generator and draw count. Deserialized from the
/// state ivar; rebuilt from `(seed, count)` only when that ivar is
/// absent or malformed.
fn load_mt(globals: &Globals, self_: Value) -> (Mt, u64) {
    let (seed, cnt) = load_state(globals, self_);
    let mt = globals
        .store
        .get_ivar(self_, ivar_state())
        .and_then(|v| v.is_rstring_inner().and_then(|s| Mt::from_bytes(s.as_bytes())))
        .unwrap_or_else(|| mt_at(seed, cnt));
    (mt, cnt)
}

/// Persist the generator and draw count. The state String is rewritten
/// in place when present (it is private to this instance — `dup`
/// clones it in `initialize_copy`), else created.
fn store_mt(globals: &mut Globals, self_: Value, mt: &Mt, cnt: u64) -> Result<()> {
    let bytes = mt.to_bytes();
    match globals.store.get_ivar(self_, ivar_state()) {
        Some(mut v) if v.is_rstring_inner().is_some() => {
            v.replace_with_inner(RStringInner::bytes(&bytes));
        }
        _ => {
            globals
                .store
                .set_ivar(self_, ivar_state(), Value::bytes(bytes))?;
        }
    }
    globals
        .store
        .set_ivar(self_, ivar_cnt(), Value::integer(cnt as i64))?;
    Ok(())
}

///
/// A multi-draw session over one `Random` instance's generator.
///
/// The generator is loaded once, advanced across every draw — one per
/// element for `Array#shuffle` — and written back by `finish`.
///
pub(super) struct RandomDraw {
    receiver: Value,
    mt: Mt,
    cnt: u64,
}

impl RandomDraw {
    pub(super) fn new(globals: &Globals, receiver: Value) -> Self {
        let (mt, cnt) = load_mt(globals, receiver);
        Self { receiver, mt, cnt }
    }

    /// Uniform integer in `[0, max]` — CRuby's `rb_random_ulong_limited`
    /// drawn from this instance's generator.
    pub(super) fn ulong_limited(&mut self, max: u64) -> u64 {
        ulong_limited(&mut self.mt, &mut self.cnt, max)
    }

    /// Persist the advanced generator back onto the receiver.
    pub(super) fn finish(self, globals: &mut Globals) -> Result<()> {
        store_mt(globals, self.receiver, &self.mt, self.cnt)
    }
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
    let mt = build_mt(seed);
    // A fresh state String: never share one between instances.
    globals
        .store
        .set_ivar(self_, ivar_state(), Value::bytes(mt.to_bytes()))?;
    globals
        .store
        .set_ivar(self_, ivar_cnt(), Value::integer(0))?;
    Ok(Value::nil())
}

///
/// ### Random#initialize_copy (the #dup / #clone hook)
///
/// The shallow copy shares the state String with the original, and the
/// state is advanced in place — give the copy its own so the two
/// generators continue independently (CRuby: a dup'd Random replays the
/// same sequence as its source from that point on).
#[monoruby_builtin]
fn random_init_copy(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let src = lfp.arg(0);
    let (mt, cnt) = load_mt(globals, src);
    globals
        .store
        .set_ivar(self_, ivar_state(), Value::bytes(mt.to_bytes()))?;
    globals
        .store
        .set_ivar(self_, ivar_cnt(), Value::integer(cnt as i64))?;
    Ok(self_)
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
    let (mt, _) = load_mt(globals, lfp.self_val());
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
    let (ma, _) = load_mt(globals, self_);
    let (mb, _) = load_mt(globals, other);
    Ok(Value::bool(ma == mb))
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
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    // The previous seed is echoed back *exactly* as given (a Bignum seed
    // returns that Bignum), so the reported value is kept as a Value.
    let old_seed = globals.random_seed_value();
    let arg = match lfp.try_arg(0) {
        None => {
            globals.random_init(None);
            return Ok(old_seed);
        }
        Some(v) => {
            // Coerce non-Integers through #to_int (CRuby).
            if v.try_fixnum().is_some() || matches!(v.unpack(), RV::BigInt(_)) {
                v
            } else if let Some(fid) = globals.check_method(v, IdentId::TO_INT) {
                let res = vm.invoke_func_inner(globals, fid, v, &[], None, None)?;
                if res.try_fixnum().is_none() && !matches!(res.unpack(), RV::BigInt(_)) {
                    return Err(MonorubyErr::no_implicit_conversion(
                        &globals.store,
                        v,
                        INTEGER_CLASS,
                    ));
                }
                res
            } else {
                return Err(MonorubyErr::no_implicit_conversion(
                    &globals.store,
                    v,
                    INTEGER_CLASS,
                ));
            }
        }
    };
    // Every word of the seed feeds the Mersenne Twister (CRuby
    // rand_init); the exact Integer is reported back by the next srand.
    globals.random_init_with(arg);
    Ok(old_seed)
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
    let (mut mt, mut c) = load_mt(globals, self_);
    let result = inst_rand_op(vm, globals, &mut mt, &mut c, lfp.try_arg(0))?;
    store_mt(globals, self_, &mt, c)?;
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
        None => return Ok(Value::float(globals.random_float())),
    };
    if let Some(max) = arg.try_fixnum() {
        if max <= 0 {
            return Err(MonorubyErr::argumenterr(format!(
                "invalid argument - {}",
                max
            )));
        }
        Ok(globals.random_rand_int(&num::BigInt::from(max)))
    } else if let Some(max) = arg.try_float() {
        if max < 0.0 {
            return Err(MonorubyErr::argumenterr(format!(
                "invalid argument - {}",
                max
            )));
        }
        let f = globals.random_float();
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
    let (mut mt, cnt) = load_mt(globals, self_);
    // ceil(size/4) 32-bit words are consumed (a trailing partial word
    // still draws a full u32, matching CRuby).
    let words = size.div_ceil(4) as u64;
    let mut buf = vec![0u8; size];
    mt.fill_bytes(&mut buf);
    store_mt(globals, self_, &mt, cnt + words)?;
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
    // CRuby `rb_rand_bytes`: little-endian 32-bit words off the default
    // PRNG, a trailing partial word consuming a full draw.
    let mut v = vec![0; size];
    globals.random_fill_bytes(&mut v);
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
    fn random_live_state() {
        // dup / clone get their own generator (initialize_copy): the copy
        // continues the source's stream independently, and stays == to
        // its source only while both have drawn the same number of words.
        run_tests(&[
            r#"(a=Random.new(7); a.rand; b=a.dup; [a.rand(100), b.rand(100), a == b])"#,
            r#"(a=Random.new(7); b=a.clone; x=b.rand; [x == Random.new(7).rand, a == b, a.rand == x])"#,
            r#"(a=Random.new(9); b=a.dup; 3.times { b.rand }; [a.rand(1000), b.rand(1000)])"#,
            r#"(a=Random.new(5); b=a.dup; a.bytes(7); b.bytes(7); a == b)"#,
            // Bytes and shuffle/sample draw from — and advance — the same
            // live state as rand.
            r#"(r=Random.new(11); [r.bytes(5).bytes, r.rand(100), (1..10).to_a.shuffle(random: r), r.rand(100)])"#,
            r#"(r=Random.new(13); [(1..20).to_a.sample(3, random: r), r.rand])"#,
            // The private state digest tracks the generator.
            r#"(r=Random.new(3); s0=r.send(:state); r.rand; [s0 == r.send(:state), r.send(:state) == Random.new(3).tap { |q| q.rand }.send(:state)])"#,
            r#"(r=Random.new(2**70); r.rand(2**40); r.rand(2**40))"#,
        ]);
        // An allocated-but-uninitialized instance has no state ivar: the
        // rebuild-from-(seed, count) fallback serves it.
        run_test(
            r#"
        r = Random.allocate
        a = r.rand
        b = r.rand(10)
        [a.is_a?(Float) && a >= 0.0 && a < 1.0, b.is_a?(Integer) && b >= 0 && b < 10, r.seed.is_a?(Integer)]
        "#,
        );
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
