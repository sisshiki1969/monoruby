use super::*;
use num::BigInt;

//
// Random class
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_class_under_obj("Random").id();
    globals.define_builtin_class_func_with(klass, "srand", srand, 0, 1, false);
    globals.define_builtin_class_func(klass, "rand", rand, 0);
}

/// ### Random.srand
/// - srand -> Integer
/// - srand(number) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Random/s/srand.html]
#[monoruby_builtin]
fn srand(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let len = lfp.arg_len();
    let old_seed = BigInt::from_bytes_le(num::bigint::Sign::Plus, globals.random_seed());
    let new_seed = if len == 0 {
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
