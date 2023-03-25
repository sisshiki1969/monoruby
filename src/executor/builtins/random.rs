use crate::*;
use num::BigInt;

//
// Random class
//

pub(super) fn init(globals: &mut Globals, class: ClassId) {
    globals.define_builtin_class_func(class, "srand", srand, -1);
    globals.define_builtin_class_func(class, "rand", rand, 0);
}

/// ### Random.srand
/// - srand -> Integer
/// - srand(number) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Random/s/srand.html]
extern "C" fn srand(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    len: usize,
) -> Option<Value> {
    globals.check_number_of_arguments(len, 0..=1)?;
    let old_seed = BigInt::from_bytes_le(num::bigint::Sign::Plus, &globals.random.seed);
    let new_seed = if len == 0 {
        None
    } else {
        match arg[0].unpack() {
            RV::Integer(i) => Some(i),
            _ => unimplemented!(),
        }
    };
    globals.random.init_with_seed(new_seed);
    Some(Value::new_bigint(old_seed))
}

// ### Random.rand
/// - rand -> Float
/// - rand(max) -> Integer | Float
/// - rand(range) -> Integer | Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Random/s/rand.html]
extern "C" fn rand(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Option<Value> {
    let f = globals.random.gen();
    Some(Value::new_float(f))
}
