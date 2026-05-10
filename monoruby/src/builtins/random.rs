use super::*;

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
    globals.define_builtin_func_with(klass, "rand", rand, 0, 1, false);
    globals.define_builtin_func(klass, "bytes", instance_bytes, 1);
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
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    rand_with_arg(globals, lfp.try_arg(0))
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
fn rand(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    rand_with_arg(globals, lfp.try_arg(0))
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
/// Per-instance state is not yet distinct from `Random.bytes`; both
/// currently draw from the global PRNG.
#[monoruby_builtin]
fn instance_bytes(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    random_bytes_inner(vm, globals, lfp.arg(0))
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
}
