use crate::*;
use num::BigInt;

//
// Random class
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_class_under_obj("Random").id();
    globals.define_builtin_class_func(klass, "srand", srand);
    globals.define_builtin_class_func(klass, "rand", rand);
}

/// ### Random.srand
/// - srand -> Integer
/// - srand(number) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Random/s/srand.html]
#[monoruby_builtin]
fn srand(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    MonorubyErr::check_number_of_arguments_range(len, 0..=1)?;
    let old_seed = BigInt::from_bytes_le(num::bigint::Sign::Plus, globals.random_seed());
    let new_seed = if len == 0 {
        None
    } else {
        match arg[0].unpack() {
            RV::Fixnum(i) => Some(i),
            _ => unimplemented!(),
        }
    };
    globals.random_init(new_seed);
    Ok(Value::bigint(old_seed))
}

// ### Random.rand
/// - rand -> Float
/// - rand(max) -> Integer | Float
/// - rand(range) -> Integer | Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Random/s/rand.html]
#[monoruby_builtin]
fn rand(_vm: &mut Executor, globals: &mut Globals, _lfp: LFP, _arg: Arg) -> Result<Value> {
    let f = globals.random_gen();
    Ok(Value::float(f))
}
