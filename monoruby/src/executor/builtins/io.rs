use crate::*;

//
// IO class
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_builtin_class_under_obj("IO", IO_CLASS).id();
    //globals.define_builtin_singleton_func(IO_CLASS, "new", now, 0);
    globals.define_builtin_func(klass, "sync", sync, 0);
    globals.define_builtin_func(klass, "sync=", assign_sync, 1);
}

#[monoruby_builtin]
fn sync(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    Ok(Value::bool(false))
}

#[monoruby_builtin]
fn assign_sync(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    Ok(arg[0])
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
