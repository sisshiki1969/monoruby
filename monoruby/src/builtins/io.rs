use super::*;

//
// IO class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("IO", IO_CLASS);
    //globals.define_builtin_singleton_func(IO_CLASS, "new", now, 0);
    globals.define_builtin_func(IO_CLASS, "<<", shl, 1);
    globals.define_builtin_funcs(IO_CLASS, "isatty", &["tty?"], isatty, 0);
    globals.define_builtin_func(IO_CLASS, "sync", sync, 0);
    globals.define_builtin_func(IO_CLASS, "sync=", assign_sync, 1);

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

///
/// ### IO#<<
///
/// - self << object -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/=3c=3c.html]
#[monoruby_builtin]
fn shl(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    if let Some(b) = lfp.arg(0).try_bytes() {
        lfp.self_val().as_io_mut().write(b.as_bytes())?;
    } else {
        let s = vm.to_s(globals, lfp.arg(0))?;
        lfp.self_val().as_io_mut().write(s.as_bytes())?;
    };
    globals.flush_stdout();
    Ok(lfp.self_val())
}

///
/// ### IO#isatty
///
/// - isatty -> bool
/// - tty? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/isatty.html]
#[monoruby_builtin]
fn isatty(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_io_mut().isatty()))
}

#[monoruby_builtin]
fn sync(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp) -> Result<Value> {
    Ok(Value::bool(false))
}

#[monoruby_builtin]
fn assign_sync(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(lfp.arg(0))
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn test() {
        run_test_no_result_check(
            r#"
            $stdout << "a"
            $stdout << 5
            $stdin.sync
            $stdin.sync = true
        "#,
        );
        run_test(
            r#"
            [$stdin.isatty, $stdout.isatty, $stderr.isatty]
        "#,
        )
    }
}
