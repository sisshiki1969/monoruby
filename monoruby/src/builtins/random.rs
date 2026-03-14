use super::*;

//
// Random class
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_class_under_obj("Random").id();
    globals.define_builtin_class_func_with(klass, "srand", random_srand, 0, 1, false);
    globals.define_builtin_class_func(klass, "rand", random_rand, 0);
    globals.define_builtin_class_func(klass, "urandom", urandom, 1);
    globals.define_builtin_func_with(klass, "rand", rand, 0, 1, false);
}

///
/// ### Random.srand
///
/// - srand -> Integer
/// - srand(number) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Random/s/srand.html]
#[monoruby_builtin]
fn srand(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let old_seed = globals.random_seed();
    let new_seed = if lfp.try_arg(0).is_none() {
        None
    } else {
        match lfp.arg(0).unpack() {
            RV::Fixnum(i) => Some(i),
            _ => unimplemented!(),
        }
    };
    globals.random_init(new_seed);
    Ok(Value::integer(old_seed as i64))
}

///
/// ### Random.rand
///
/// - rand -> Float
/// - [NOT SUPPORTED] rand(max) -> Integer | Float
/// - [NOT SUPPORTED] rand(range) -> Integer | Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Random/s/rand.html]
#[monoruby_builtin]
fn rand(_vm: &mut Executor, globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = globals.random_gen();
    Ok(Value::float(f))
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
    if let Some(arg) = lfp.try_arg(0) {
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
        } else if let Some(_) = arg.is_range() {
            return Err(MonorubyErr::runtimeerr(
                "Range argument is not supported in Random#rand",
            ));
        } else {
            return Err(MonorubyErr::runtimeerr(
                "the argument is not supported in Random#rand",
            ));
        }
    } else {
        let f: f64 = globals.random_gen();
        Ok(Value::float(f))
    }
}

///
/// ### Random.urandom
///
/// - urandom(size) -> String
#[monoruby_builtin]
fn urandom(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let size = if let Some(size) = lfp.arg(0).try_fixnum() {
        size
    } else if let Some(size) = lfp.arg(0).try_float() {
        size.round() as i64
    } else {
        return Err(MonorubyErr::no_implicit_conversion(
            globals,
            lfp.arg(0),
            INTEGER_CLASS,
        ));
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
