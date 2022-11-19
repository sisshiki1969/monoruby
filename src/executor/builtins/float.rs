use crate::*;

//
// Float class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(FLOAT_CLASS, "to_i", toi, 0);
    globals.define_builtin_func(FLOAT_CLASS, "to_f", tof, 0);
}

extern "C" fn tof(
    _vm: &mut Executor,
    _globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<Value>,
) -> Option<Value> {
    Some(self_val)
}

extern "C" fn toi(
    _vm: &mut Executor,
    _globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<Value>,
) -> Option<Value> {
    match self_val.unpack() {
        RV::Float(f) => Some(Value::new_integer(f.trunc() as i64)),
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn times() {
        run_test("4.87.to_i");
        run_test("-2.18.to_i");
        run_test("4.7777.to_f");
        run_test("-725.11.to_f");
    }
}
