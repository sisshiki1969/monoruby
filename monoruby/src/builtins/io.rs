use super::*;

//
// IO class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("IO", IO_CLASS);
    //globals.define_builtin_singleton_func(IO_CLASS, "new", now, 0);
    globals.define_builtin_func(IO_CLASS, "sync", sync);
    globals.define_builtin_func(IO_CLASS, "sync=", assign_sync);

    let stdin = Value::new_io_stdin();
    globals.set_constant_by_str(OBJECT_CLASS, "STDIN", stdin);
    globals.set_gvar(IdentId::get_id("$stdin"), stdin);

    let stdout = Value::new_io_stdout();
    globals.set_constant_by_str(OBJECT_CLASS, "STDOUT", stdout);
    globals.set_gvar(IdentId::get_id("$stdout"), stdout);
    globals.set_gvar(IdentId::get_id("$>"), stdout);

    let stderr = Value::new_io_stderr();
    globals.set_constant_by_str(OBJECT_CLASS, "STDERR", stderr);
    globals.set_gvar(IdentId::get_id("$stderr"), stderr);
}

#[monoruby_builtin]
fn sync(_vm: &mut Executor, _globals: &mut Globals, _lfp: LFP, _arg: Arg) -> Result<Value> {
    Ok(Value::bool(false))
}

#[monoruby_builtin]
fn assign_sync(_vm: &mut Executor, _globals: &mut Globals, _lfp: LFP, arg: Arg) -> Result<Value> {
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
