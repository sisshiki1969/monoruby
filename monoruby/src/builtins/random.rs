use super::*;
use num::BigInt;

//
// Random class
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_class_under_obj("Random").id();
    globals.define_builtin_class_func_with(klass, "srand", srand, 0, 1, false);
    globals.define_builtin_class_func(klass, "rand", rand, 0);
    globals.define_builtin_class_func(klass, "urandom", urandom, 1);
}

/// ### Random.srand
/// - srand -> Integer
/// - srand(number) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Random/s/srand.html]
#[monoruby_builtin]
fn srand(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let old_seed = BigInt::from_bytes_le(num::bigint::Sign::Plus, globals.random_seed());
    let new_seed = if lfp.try_arg(0).is_none() {
        None
    } else {
        match lfp.arg(0).unpack() {
            RV::Fixnum(i) => Some(i),
            _ => unimplemented!(),
        }
    };
    globals.random_init(new_seed);
    Ok(Value::bigint(old_seed))
}

// ### Random.rand
/// - rand -> Float
/// - [NOT SUPPORTED] rand(max) -> Integer | Float
/// - [NOT SUPPORTED] rand(range) -> Integer | Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Random/s/rand.html]
#[monoruby_builtin]
fn rand(_vm: &mut Executor, globals: &mut Globals, _lfp: Lfp) -> Result<Value> {
    let f = globals.random_gen();
    Ok(Value::float(f))
}

///
/// ### Random.urandom
///
/// - urandom(size) -> String
#[monoruby_builtin]
fn urandom(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let size = if let Some(size) = lfp.arg(0).try_fixnum() {
        size
    } else if let Some(size) = lfp.arg(0).try_float() {
        size.round() as i64
    } else {
        return Err(MonorubyErr::no_implicit_conversion(
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
    for chunk in v.array_chunks_mut() {
        *chunk = globals.random_gen::<[u8; 8]>();
    }
    v.truncate(size);
    Ok(Value::bytes(v))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

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
