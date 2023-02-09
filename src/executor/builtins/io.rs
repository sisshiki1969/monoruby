use crate::*;

//
// IO class
//

pub(super) fn init(globals: &mut Globals) {
    //globals.define_builtin_singleton_func(IO_CLASS, "new", now, 0);
    globals.define_builtin_func(IO_CLASS, "sync", sync, 0);
    globals.define_builtin_func(IO_CLASS, "sync=", assign_sync, 1);
}

extern "C" fn sync(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    Some(Value::bool(false))
}

extern "C" fn assign_sync(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    Some(arg[0])
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn test() {
        run_test_no_result_check(
            r#"
            $stdin.sync
            $stdin.sync = true
        "#,
        );
    }
}
