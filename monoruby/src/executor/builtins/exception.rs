use crate::*;

//
// Exception class
//

pub(super) fn init(globals: &mut Globals) {
    let exception_class = globals.define_builtin_class_under_obj("Exception", EXCEPTION_CLASS);
    let standarderr = globals.define_class_by_str("StandardError", exception_class, OBJECT_CLASS);
    globals.define_class_by_str("NoMemoryError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("SecurityError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("SignalException", standarderr, OBJECT_CLASS);

    let scripterr = globals.define_class_by_str("ScriptError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("LoadError", scripterr, OBJECT_CLASS);

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

    globals.define_builtin_class_func(EXCEPTION_CLASS, "new", exception_new);
    globals.define_builtin_class_func(EXCEPTION_CLASS, "exception", exception_new);
}

///
/// ### Exception.exception
///
/// - new(error_message = nil) -> Exception
/// - exception(error_message = nil) -> Exception
///
/// [https://docs.ruby-lang.org/ja/latest/method/Exception/s/exception.html]
#[monoruby_builtin]
fn exception_new(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    MonorubyErr::check_number_of_arguments_range(len, 0..=1)?;
    let class_id = lfp.self_val().expect_class(globals)?;
    let msg = if len == 0 {
        class_id.get_name(globals)
    } else {
        arg[0].expect_string(globals)?
    };
    let kind = class_id.get_name_id(globals).unwrap();
    Ok(Value::new_exception(kind, msg, vec![], class_id))
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
}
