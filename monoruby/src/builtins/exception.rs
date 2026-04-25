use super::*;

//
// Exception class
//

pub(super) fn init(globals: &mut Globals) {
    let exception_class =
        globals.define_builtin_class_under_obj("Exception", EXCEPTION_CLASS, ObjTy::EXCEPTION);
    globals.define_builtin_class_funcs_with(
        EXCEPTION_CLASS,
        "new",
        &["exception"],
        exception_new,
        0,
        0,
        true,
    );

    globals.store[EXCEPTION_CLASS].set_alloc_func(exception_alloc_func);

    let standarderr = globals.define_class("StandardError", exception_class, OBJECT_CLASS);

    // FatalError — raised when a Rust panic is caught at an `extern "C"`
    // boundary. Inherits directly from Exception (not StandardError) so a
    // bare `rescue` clause does not catch it; the runtime additionally
    // prevents `rescue Exception` / `rescue => e` from catching it.
    globals.define_builtin_exception_class("FatalError", FATAL_ERROR_CLASS, exception_class);

    let system_exit = globals.define_builtin_exception_class(
        "SystemExit",
        SYSTEM_EXIT_ERROR_CLASS,
        exception_class,
    );
    let system_exit_id = system_exit.id();
    globals.define_builtin_class_func_with(system_exit_id, "new", system_exit_new, 0, 2, false);
    globals.define_builtin_func(system_exit_id, "status", system_exit_status, 0);

    globals.define_class("NoMemoryError", standarderr, OBJECT_CLASS);
    globals.define_class("SecurityError", standarderr, OBJECT_CLASS);
    globals.define_class("SignalException", standarderr, OBJECT_CLASS);

    let scripterr = globals.define_class("ScriptError", standarderr, OBJECT_CLASS);
    let loaderr = globals.define_builtin_exception_class("LoadError", LOAD_ERROR_CLASS, scripterr);
    globals.define_builtin_func(loaderr.id(), "path", loaderror_path, 0);
    globals.define_builtin_exception_class("SyntaxError", SYNTAX_ERROR_CLASS, scripterr);
    globals.define_builtin_exception_class(
        "NotImplementedError",
        UNIMPLEMENTED_ERROR_CLASS,
        scripterr,
    );

    globals.define_builtin_exception_class("ArgumentError", ARGUMENTS_ERROR_CLASS, standarderr);
    globals.define_class("EncodingError", standarderr, OBJECT_CLASS);
    globals.define_builtin_exception_class("FiberError", FIBER_ERROR_CLASS, standarderr);
    let ioerr = globals.define_builtin_exception_class("IOError", IO_ERROR_CLASS, standarderr);
    globals.define_class("EOFError", ioerr, OBJECT_CLASS);

    let indexerr =
        globals.define_builtin_exception_class("IndexError", INDEX_ERROR_CLASS, standarderr);
    let keyerr = globals.define_builtin_exception_class("KeyError", KEY_ERROR_CLASS, indexerr);
    globals.define_builtin_func(keyerr.id(), "receiver", keyerror_receiver, 0);
    globals.define_builtin_func(keyerr.id(), "key", keyerror_key, 0);
    globals.define_builtin_exception_class("StopIteration", STOP_ITERATION_CLASS, indexerr);

    globals.define_builtin_exception_class("LocalJumpError", LOCAL_JUMP_ERROR_CLASS, standarderr);

    let nameerr =
        globals.define_builtin_exception_class("NameError", NAME_ERROR_CLASS, standarderr);
    globals.define_builtin_exception_class("NoMethodError", NO_METHOD_ERROR_CLASS, nameerr);
    globals.define_builtin_func(NO_METHOD_ERROR_CLASS, "receiver", nomethoderr_receiver, 0);

    let runtimeerr =
        globals.define_builtin_exception_class("RuntimeError", RUNTIME_ERROR_CLASS, standarderr);
    globals.define_builtin_exception_class("FrozenError", FROZEN_ERROR_CLASS, runtimeerr);

    globals.define_builtin_exception_class("RangeError", RANGE_ERROR_CLASS, standarderr);
    globals.define_builtin_exception_class("RegexpError", REGEX_ERROR_CLASS, standarderr);
    globals.define_class("SystemCallError", standarderr, OBJECT_CLASS);
    globals.define_builtin_exception_class("TypeError", TYPE_ERROR_CLASS, standarderr);
    globals.define_builtin_exception_class(
        "ZeroDivisionError",
        ZERO_DIVISION_ERROR_CLASS,
        standarderr,
    );

    globals.define_builtin_func_with(EXCEPTION_CLASS, "initialize", initialize, 0, 2, false);
    globals.define_builtin_func(EXCEPTION_CLASS, "message", message, 0);
    globals.define_builtin_func(EXCEPTION_CLASS, "backtrace", backtrace, 0);
    globals.define_builtin_func(EXCEPTION_CLASS, "set_backtrace", set_backtrace, 1);
    globals.define_builtin_func(EXCEPTION_CLASS, "cause", cause, 0);
    globals.define_builtin_func(EXCEPTION_CLASS, "==", exception_eq, 1);
    globals.define_builtin_func_with(
        EXCEPTION_CLASS,
        "exception",
        exception_method,
        0,
        1,
        false,
    );
}

/// ### NoMethodError#receiver
#[monoruby_builtin]
fn nomethoderr_receiver(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let v = globals
        .store
        .get_ivar(self_val, IdentId::get_id("/receiver"))
        .unwrap_or_default();
    Ok(v)
}

/// Allocator for `Exception` and its subclasses. The exception is created
/// with its message set to the class name (matching CRuby:
/// `Exception.allocate.message # => "Exception"`).
pub(crate) extern "C" fn exception_alloc_func(class_id: ClassId, globals: &mut Globals) -> Value {
    let name = class_id.get_name(globals);
    Value::new_exception_from_with_class(name, class_id, class_id)
}

///
/// ### Exception.exception
///
/// - new(error_message = nil) -> Exception
/// - exception(error_message = nil) -> Exception
///
/// [https://docs.ruby-lang.org/ja/latest/method/Exception/s/exception.html]
#[monoruby_builtin]
fn exception_new(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().expect_class(globals)?.id();
    let obj = Value::new_exception_from("".to_string(), class_id);

    vm.invoke_method_inner(
        globals,
        IdentId::INITIALIZE,
        obj,
        &lfp.arg(0).as_array(),
        lfp.block(),
        None,
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
fn initialize(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let class_id = self_.real_class(&globals.store).id();
    let message = if let Some(msg) = lfp.try_arg(0) {
        if msg.try_hash_ty().is_some() {
            // Keyword arguments hash passed as positional arg; use default message.
            store_exception_kwargs(vm, globals, self_, msg)?;
            globals.store.get_class_name(class_id)
        } else {
            // Check if there's a keyword hash as second arg
            if let Some(kwargs) = lfp.try_arg(1) {
                if kwargs.try_hash_ty().is_some() {
                    store_exception_kwargs(vm, globals, self_, kwargs)?;
                }
            }
            msg.coerce_to_string(vm, globals)?
        }
    } else {
        globals.store.get_class_name(class_id)
    };
    self_.is_exception_mut().unwrap().set_message(message);
    Ok(Value::nil())
}

/// Store keyword arguments (receiver:, key:) from a Hash into exception ivars.
fn store_exception_kwargs(
    vm: &mut Executor,
    globals: &mut Globals,
    self_: Value,
    kwargs: Value,
) -> Result<()> {
    let hash = kwargs.as_hash();
    let recv_key = Value::symbol_from_str("receiver");
    if let Ok(Some(v)) = hash.get(recv_key, vm, globals) {
        globals
            .store
            .set_ivar(self_, IdentId::get_id("/receiver"), v)?;
    }
    let key_key = Value::symbol_from_str("key");
    if let Ok(Some(v)) = hash.get(key_key, vm, globals) {
        globals.store.set_ivar(self_, IdentId::get_id("/key"), v)?;
    }
    Ok(())
}

///
/// ### Exception#message
///
/// - message -> String
/// - to_s -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Exception/i/message.html]
#[monoruby_builtin]
fn message(_vm: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let ex = self_.is_exception().unwrap();
    Ok(Value::string(ex.message().to_string()))
}

///
/// ### Exception#backtrace
///
/// - backtrace -> [String]
///
/// Returns the backtrace explicitly set via `#set_backtrace` if any;
/// otherwise the trace captured when the exception was raised.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Exception/i/backtrace.html]
#[monoruby_builtin]
fn backtrace(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    if let Some(bt) = globals.store.get_ivar(self_, IdentId::get_id("/backtrace")) {
        return Ok(bt);
    }
    let traces: Vec<String> = self_.is_exception().unwrap().trace_location(globals);
    if traces.is_empty() {
        // Exception was constructed (e.g. `Exception.new`) but never
        // raised, so there is no backtrace to report.
        return Ok(Value::nil());
    }
    Ok(Value::array_from_iter(
        traces.into_iter().map(|s| Value::string(s)),
    ))
}

/// ### Exception#set_backtrace
/// - set_backtrace(backtrace) -> Array
///
/// Stores `backtrace` so subsequent `#backtrace` calls return it. Per
/// CRuby: a String is wrapped in a single-element array, nil clears
/// the stored backtrace, an Array of Strings is used as-is.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Exception/i/set_backtrace.html]
#[monoruby_builtin]
fn set_backtrace(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let arg = lfp.arg(0);
    let stored = if arg.is_nil() {
        Value::nil()
    } else if arg.is_str().is_some() {
        Value::array1(arg)
    } else if let Some(ary) = arg.try_array_ty() {
        for elem in ary.iter() {
            if elem.is_str().is_none() {
                return Err(MonorubyErr::typeerr(
                    "backtrace must be an Array of String",
                ));
            }
        }
        arg
    } else {
        return Err(MonorubyErr::typeerr(
            "backtrace must be an Array of String or a String",
        ));
    };
    globals
        .store
        .set_ivar(lfp.self_val(), IdentId::get_id("/backtrace"), stored)?;
    Ok(stored)
}

/// ### Exception#exception
///
/// - exception -> self
/// - exception(message) -> Exception
///
/// With no arguments (or with `self` as the argument), returns `self`.
/// Otherwise returns a copy of `self` with `message` as the new
/// `#message`. The copy is created via `#dup` so that subclass-defined
/// instance variables are preserved (e.g. `CustomArgumentError#val`).
#[monoruby_builtin]
fn exception_method(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_v = lfp.self_val();
    let Some(arg) = lfp.try_arg(0) else {
        return Ok(self_v);
    };
    if arg.id() == self_v.id() {
        return Ok(self_v);
    }
    let mut new_exc = self_v.dup();
    let msg = arg.coerce_to_string(vm, globals)?;
    new_exc.is_exception_mut().unwrap().set_message(msg);
    Ok(new_exc)
}

/// ### Exception#==
///
/// - self == other -> bool
///
/// Returns true when `other` is an Exception with the same class,
/// the same `#message`, and an equal `#backtrace` (per CRuby).
#[monoruby_builtin]
fn exception_eq(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_v = lfp.self_val();
    let other = lfp.arg(0);
    if self_v.id() == other.id() {
        return Ok(Value::bool(true));
    }
    let self_ex = match self_v.is_exception() {
        Some(e) => e,
        None => return Ok(Value::bool(false)),
    };
    let other_ex = match other.is_exception() {
        Some(e) => e,
        None => return Ok(Value::bool(false)),
    };
    if self_v.class() != other.class() {
        return Ok(Value::bool(false));
    }
    if self_ex.message() != other_ex.message() {
        return Ok(Value::bool(false));
    }
    // Compare backtraces using #backtrace (so we go through the same
    // path users see). Both nil or both equal arrays count as equal.
    let bt_id = IdentId::get_id("backtrace");
    let self_bt = vm
        .invoke_method_if_exists(globals, bt_id, self_v, &[], None, None)?
        .unwrap_or_default();
    let other_bt = vm
        .invoke_method_if_exists(globals, bt_id, other, &[], None, None)?
        .unwrap_or_default();
    Ok(Value::bool(vm.eq_values_bool(globals, self_bt, other_bt)?))
}

/// ### KeyError#receiver
#[monoruby_builtin]
fn keyerror_receiver(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let v = globals
        .store
        .get_ivar(self_val, IdentId::get_id("/receiver"))
        .unwrap_or_default();
    Ok(v)
}

/// ### KeyError#key
#[monoruby_builtin]
fn keyerror_key(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let v = globals
        .store
        .get_ivar(self_val, IdentId::get_id("/key"))
        .unwrap_or_default();
    Ok(v)
}

///
/// ### LoadError#path
///
/// - path -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/LoadError/i/path.html]
#[monoruby_builtin]
fn loaderror_path(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    Ok(globals
        .store
        .get_ivar(self_, IdentId::get_id("/path"))
        .unwrap_or_default())
}

///
/// ### SystemExit#status
///
/// - status -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/SystemExit/i/status.html]
#[monoruby_builtin]
fn system_exit_status(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    Ok(globals
        .store
        .get_ivar(self_, IdentId::get_id("/status"))
        .unwrap_or_default())
}

///
/// ### SystemExit.new
///
/// - new(status = 0, error_message = "") -> SystemExit
///
/// [https://docs.ruby-lang.org/ja/latest/method/SystemExit/s/new.html]
#[monoruby_builtin]
fn system_exit_new(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().expect_class(globals)?.id();
    let name = class_id.get_name(&globals.store);
    let (status, msg) = if let Some(arg0) = lfp.try_arg(0) {
        let status = arg0.coerce_to_int_i64(vm, globals)?;
        if let Some(arg1) = lfp.try_arg(1) {
            (status, arg1.coerce_to_str(vm, globals)?)
        } else {
            (status, name.clone())
        }
    } else {
        (0, name.clone())
    };
    let ex = Value::new_exception_from(msg, class_id);
    globals
        .store
        .set_ivar(ex, IdentId::get_id("/status"), Value::integer(status))
        .unwrap();

    Ok(ex)
}

/// ### Exception#cause
#[monoruby_builtin]
fn cause(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    Ok(globals
        .store
        .get_ivar(self_, IdentId::get_id("/cause"))
        .unwrap_or_default())
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
            raise StopIteration "woo"
        "##,
        );
        run_test_error(
            r##"
            raise StopIteration.new
        "##,
        );
        run_test_error(
            r##"
            raise StopIteration.new "woo"
        "##,
        );
    }

    #[test]
    fn set_backtrace() {
        run_test(
            r#"
            e = RuntimeError.new("test")
            e.set_backtrace(["foo.rb:1:in `bar'", "baz.rb:2:in `qux'"])
        "#,
        );
        run_test(
            r#"
            e = RuntimeError.new("test")
            e.set_backtrace("foo.rb:1:in `bar'")
        "#,
        );
        run_test(
            r#"
            e = RuntimeError.new("test")
            e.set_backtrace(nil)
        "#,
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

    #[test]
    fn cause() {
        run_test(
            r#"
            begin
              raise "original"
            rescue => e
              e.cause
            end
            "#,
        );
    }

    #[test]
    fn nomethoderror_receiver() {
        run_test(
            r#"
            begin
              nil.foo
            rescue NoMethodError => e
              e.receiver
            end
            "#,
        );
    }

    #[test]
    fn nomethoderror_class() {
        run_test(
            r#"
            begin
              nil.foo
            rescue => e
              e.class
            end
            "#,
        );
    }

    #[test]
    fn full_message() {
        run_test(
            r#"
            begin
              raise "test error"
            rescue => e
              e.full_message(order: :top).include?("test error")
            end
            "#,
        );
        run_test(
            r#"
            begin
              raise "test error"
            rescue => e
              e.full_message(order: :top).include?("RuntimeError")
            end
            "#,
        );
    }
}
