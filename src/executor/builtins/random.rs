use crate::*;
use num::BigInt;
use rand::{Rng, SeedableRng};

//
// Random class
//

pub(super) fn init(globals: &mut Globals, class: ClassId) {
    globals.define_builtin_singleton_func(class, "srand", srand, -1);
    globals.define_builtin_singleton_func(class, "rand", rand, 0);
}

/// ### Random.srand
/// - srand -> Integer
/// - srand(number) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Random/s/srand.html]
extern "C" fn srand(
    _vm: &mut Executor,
    globals: &mut Globals,
    _self_val: Value,
    arg: Arg,
    len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    globals.check_number_of_arguments(len, 0..=1)?;
    let old_seed = BigInt::from_bytes_le(num::bigint::Sign::Plus, &globals.random_seed);
    let mut new_seed = <sfmt::SFMT as SeedableRng>::Seed::default();
    if len == 0 {
        if let Err(err) = getrandom::getrandom(&mut new_seed) {
            panic!("from_entropy failed: {}", err);
        }
    } else {
        match arg[0].unpack() {
            RV::Integer(i) => {
                for (i, byte) in (i as i32).to_ne_bytes().iter().enumerate() {
                    new_seed[i] = *byte;
                }
            }
            _ => unimplemented!(),
        };
    }
    globals.random_seed = new_seed;
    globals.random = sfmt::SFMT::from_seed(new_seed);
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
    _self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let f = globals.random.gen();
    Some(Value::new_float(f))
}
