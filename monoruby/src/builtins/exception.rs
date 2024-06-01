use super::*;

//
// Exception class
//

pub(super) fn init(globals: &mut Globals) {
    let exception_class = globals.define_builtin_class_under_obj("Exception", EXCEPTION_CLASS);
    let standarderr = globals.define_class_by_str("StandardError", exception_class, OBJECT_CLASS);

    let system_exit_id = globals
        .define_class_by_str("SystemExit", exception_class, OBJECT_CLASS)
        .id();
    globals.define_builtin_class_func_with(system_exit_id, "new", system_exit_new, 0, 2, false);
    globals.define_attr_reader(
        system_exit_id,
        IdentId::get_id("status"),
        Visibility::Public,
    );

    globals.define_class_by_str("NoMemoryError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("SecurityError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("SignalException", standarderr, OBJECT_CLASS);

    let scripterr = globals.define_class_by_str("ScriptError", standarderr, OBJECT_CLASS);
    let loaderr = globals.define_class_by_str("LoadError", scripterr, OBJECT_CLASS);
    globals.define_builtin_func(loaderr.id(), "path", loaderror_path, 0);

    globals.define_class_by_str("ArgumentError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("EncodingError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("FiberError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("IOError", standarderr, OBJECT_CLASS);
    let indexerr = globals.define_class_by_str("IndexError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("KeyError", indexerr, OBJECT_CLASS);
    globals.define_class_by_str("StopIteration", indexerr, OBJECT_CLASS);

    globals.define_class_by_str("LocalJumpError", standarderr, OBJECT_CLASS);

    let nameerr = globals.define_class_by_str("NameError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("NoMethodError", nameerr, OBJECT_CLASS);

    globals.define_class_by_str("RangeError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("RegexpError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("RuntimeError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("SystemCallError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("TypeError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("ZeroDivisionError", standarderr, OBJECT_CLASS);

    globals.define_builtin_class_funcs_with(
        EXCEPTION_CLASS,
        "new",
        &["exception"],
        exception_new,
        0,
        1,
        false,
    );
}

///
/// ### Exception.exception
///
/// - new(error_message = nil) -> Exception
/// - exception(error_message = nil) -> Exception
///
/// [https://docs.ruby-lang.org/ja/latest/method/Exception/s/exception.html]
#[monoruby_builtin]
fn exception_new(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class_id = lfp.self_val().expect_class(globals)?;
    let msg = if let Some(msg) = lfp.try_arg(0) {
        msg.expect_string()?
    } else {
        globals.get_class_name(class_id)
    };
    let kind = class_id.get_name_id(globals);
    Ok(Value::new_exception(kind, msg, vec![], class_id))
}

///
/// ### LoadError#path
///
/// - path -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/LoadError/i/path.html]
#[monoruby_builtin]
fn loaderror_path(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    Ok(globals
        .get_ivar(self_, IdentId::get_id("/path"))
        .unwrap_or_default())
}

///
/// ### SystemExit.new
///
/// - new(status = 0, error_message = "") -> SystemExit
///
/// [https://docs.ruby-lang.org/ja/latest/method/SystemExit/s/new.html]
#[monoruby_builtin]
fn system_exit_new(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class_id = lfp.self_val().expect_class(globals)?;
    let name = class_id.get_name_id(globals);
    let (status, msg) = if let Some(arg0) = lfp.try_arg(0) {
        let status = arg0.expect_integer()?;
        if let Some(arg1) = lfp.try_arg(1) {
            (status, arg1.expect_string()?)
        } else {
            (status, format!("{}", name))
        }
    } else {
        (0, format!("{}", name))
    };
    let mut ex = Value::new_exception(name, msg, vec![], class_id);
    ex.set_instance_var(globals, "@status", Value::integer(status))?;

    Ok(ex)
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn exception() {
        run_test_error(
            r##"
            raise StopIteration
        "##,
        );
        run_test_error(
            r##"
            raise StopIteration.new
        "##,
        );
    }

    #[test]
    fn system_exit() {
        run_test(
            r#"
         SystemExit.new(15, "woo").status
        "#,
        );
    }
}
