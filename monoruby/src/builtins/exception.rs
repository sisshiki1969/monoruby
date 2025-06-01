use super::*;

//
// Exception class
//

pub(super) fn init(globals: &mut Globals) {
    let exception_class =
        globals.define_builtin_class_under_obj("Exception", EXCEPTION_CLASS, ObjTy::EXCEPTION);
    let standarderr = globals.define_class("StandardError", exception_class, OBJECT_CLASS);

    let system_exit_id = globals
        .define_class("SystemExit", exception_class, OBJECT_CLASS)
        .id();
    globals.define_builtin_class_func_with(system_exit_id, "new", system_exit_new, 0, 2, false);
    globals.define_attr_reader(
        system_exit_id,
        IdentId::get_id("status"),
        Visibility::Public,
    );

    globals.define_class("NoMemoryError", standarderr, OBJECT_CLASS);
    globals.define_class("SecurityError", standarderr, OBJECT_CLASS);
    globals.define_class("SignalException", standarderr, OBJECT_CLASS);

    let scripterr = globals.define_class("ScriptError", standarderr, OBJECT_CLASS);
    let loaderr = globals.define_class("LoadError", scripterr, OBJECT_CLASS);
    globals.define_builtin_func(loaderr.id(), "path", loaderror_path, 0);
    globals.define_class("SyntaxError", scripterr, OBJECT_CLASS);
    globals.define_class("NotImplementedError", scripterr, OBJECT_CLASS);

    globals.define_class("ArgumentError", standarderr, OBJECT_CLASS);
    globals.define_class("EncodingError", standarderr, OBJECT_CLASS);
    globals.define_class("FiberError", standarderr, OBJECT_CLASS);
    let ioerr = globals.define_class("IOError", standarderr, OBJECT_CLASS);
    globals.define_class("EOFError", ioerr, OBJECT_CLASS);

    let indexerr = globals.define_class("IndexError", standarderr, OBJECT_CLASS);
    globals.define_class("KeyError", indexerr, OBJECT_CLASS);
    globals.define_class("StopIteration", indexerr, OBJECT_CLASS);

    globals.define_class("LocalJumpError", standarderr, OBJECT_CLASS);

    let nameerr = globals.define_class("NameError", standarderr, OBJECT_CLASS);
    globals.define_class("NoMethodError", nameerr, OBJECT_CLASS);

    let runtimeerr = globals.define_class("RuntimeError", standarderr, OBJECT_CLASS);
    globals.define_class("FrozenError", runtimeerr, OBJECT_CLASS);

    globals.define_class("RangeError", standarderr, OBJECT_CLASS);
    globals.define_class("RegexpError", standarderr, OBJECT_CLASS);
    globals.define_class("SystemCallError", standarderr, OBJECT_CLASS);
    globals.define_class("TypeError", standarderr, OBJECT_CLASS);
    globals.define_class("ZeroDivisionError", standarderr, OBJECT_CLASS);

    let fid = globals.define_builtin_class_func_rest(EXCEPTION_CLASS, "new", exception_new);
    globals.add_method(
        EXCEPTION_CLASS,
        IdentId::get_id("exception"),
        fid,
        Visibility::Public,
    );

    globals.define_builtin_func_with(EXCEPTION_CLASS, "initialize", initialize, 0, 1, false);
    globals.define_builtin_func(EXCEPTION_CLASS, "message", message, 0);
    globals.define_builtin_func(EXCEPTION_CLASS, "backtrace", backtrace, 0);
}

///
/// ### Exception.exception
///
/// - new(error_message = nil) -> Exception
/// - exception(error_message = nil) -> Exception
///
/// [https://docs.ruby-lang.org/ja/latest/method/Exception/s/exception.html]
#[monoruby_builtin]
fn exception_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class_id = lfp.self_val().expect_class(globals)?.id();
    let obj = Value::new_exception("".to_string(), "".to_string(), vec![], class_id);

    vm.invoke_method_if_exists(
        globals,
        IdentId::INITIALIZE,
        obj,
        &lfp.arg(0).as_array(),
        lfp.block(),
    )?;

    Ok(obj)
}

///
/// ### Exception#initialize
///
/// - new(error_message = nil) -> Exception
///
/// [https://docs.ruby-lang.org/ja/latest/method/Exception/s/exception.html]
#[monoruby_builtin]
fn initialize(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let class_id = self_.real_class(&globals.store).id();
    let msg = if let Some(msg) = lfp.try_arg(0) {
        msg.expect_string(globals)?
    } else {
        globals.store.get_class_name(class_id)
    };
    let class_name = class_id.get_name(&globals.store);
    self_.is_exception_mut().unwrap().set_message(msg);
    self_.is_exception_mut().unwrap().set_class_name(class_name);
    Ok(Value::nil())
}

///
/// ### Exception#message
///
/// - message -> String
/// - to_s -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Exception/i/message.html]
#[monoruby_builtin]
fn message(_vm: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let ex = self_.is_exception().unwrap();
    Ok(Value::string(ex.get_error_message()))
}

///
/// ### Exception#backtrace
///
/// - backtrace -> [String]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Exception/i/backtrace.html]
#[monoruby_builtin]
fn backtrace(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let v = self_
        .is_exception()
        .unwrap()
        .trace_location(globals)
        .into_iter()
        .map(|s| Value::string(s))
        .collect();
    Ok(Value::array_from_vec(v))
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
        .store
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
    let class_id = lfp.self_val().expect_class(globals)?.id();
    let name = class_id.get_name(&globals.store);
    let (status, msg) = if let Some(arg0) = lfp.try_arg(0) {
        let status = arg0.expect_integer(globals)?;
        if let Some(arg1) = lfp.try_arg(1) {
            (status, arg1.expect_string(globals)?)
        } else {
            (status, name.clone())
        }
    } else {
        (0, name.clone())
    };
    let mut ex = Value::new_exception(name, msg, vec![], class_id);
    ex.set_instance_var(&mut globals.store, "@status", Value::integer(status))?;

    Ok(ex)
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

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
