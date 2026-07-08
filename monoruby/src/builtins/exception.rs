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
    globals.define_builtin_func(system_exit_id, "success?", system_exit_success_p, 0);

    // These all inherit directly from `Exception`, not `StandardError`, so a
    // bare `rescue` (== `rescue StandardError`) does not catch them — matching
    // CRuby's hierarchy.
    globals.define_class("NoMemoryError", exception_class, OBJECT_CLASS);
    globals.define_class("SecurityError", exception_class, OBJECT_CLASS);
    // `SystemStackError` is raised on stack overflow.
    globals.define_class("SystemStackError", exception_class, OBJECT_CLASS);
    let signal_exception = globals.define_builtin_exception_class(
        "SignalException",
        SIGNAL_EXCEPTION_CLASS,
        exception_class,
    );
    // `Interrupt` is the Ruby class raised on SIGINT (Ctrl-C). It is a
    // subclass of SignalException — `rescue SignalException` catches it
    // but a bare `rescue` (StandardError) does not.
    globals.define_builtin_exception_class("Interrupt", INTERRUPT_CLASS, signal_exception);

    // `ScriptError` (and its `LoadError` / `SyntaxError` /
    // `NotImplementedError` subclasses) inherits directly from `Exception`.
    let scripterr = globals.define_class("ScriptError", exception_class, OBJECT_CLASS);
    let loaderr = globals.define_builtin_exception_class("LoadError", LOAD_ERROR_CLASS, scripterr);
    globals.define_builtin_func(loaderr.id(), "path", loaderror_path, 0);
    globals.define_builtin_exception_class("SyntaxError", SYNTAX_ERROR_CLASS, scripterr);
    globals.define_builtin_exception_class(
        "NotImplementedError",
        UNIMPLEMENTED_ERROR_CLASS,
        scripterr,
    );

    let argerr =
        globals.define_builtin_exception_class("ArgumentError", ARGUMENTS_ERROR_CLASS, standarderr);
    globals.define_class("UncaughtThrowError", argerr, OBJECT_CLASS);
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
    globals.define_builtin_func(NAME_ERROR_CLASS, "name", nameerr_name, 0);
    // `NameError#receiver` (inherited by NoMethodError).
    globals.define_builtin_func(NAME_ERROR_CLASS, "receiver", exc_receiver, 0);
    globals.define_builtin_func_with(
        NAME_ERROR_CLASS,
        "initialize",
        nameerr_initialize,
        0,
        2,
        false,
    );
    globals.define_builtin_exception_class("NoMethodError", NO_METHOD_ERROR_CLASS, nameerr);

    let runtimeerr =
        globals.define_builtin_exception_class("RuntimeError", RUNTIME_ERROR_CLASS, standarderr);
    globals.define_builtin_exception_class("FrozenError", FROZEN_ERROR_CLASS, runtimeerr);
    // `FrozenError#receiver` — the frozen object that was modified.
    globals.define_builtin_func(FROZEN_ERROR_CLASS, "receiver", exc_receiver, 0);

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

/// Shared reader for the `/receiver` ivar — `NameError#receiver` /
/// `NoMethodError#receiver` / `FrozenError#receiver`.
#[monoruby_builtin]
fn exc_receiver(
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

/// `NameError#name` — returns the missing-name Symbol that was passed
/// when the exception was raised (`nil` if the raiser didn't set one).
/// Set via `MonorubyErr::nameerr_with_name`, surfaced as the `/name`
/// ivar in `Executor::take_ex_obj`.
#[monoruby_builtin]
fn nameerr_name(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let v = globals
        .store
        .get_ivar(self_val, IdentId::_NAME)
        .unwrap_or_default();
    Ok(v)
}

///
/// `NameError.new(msg = nil, name = nil)`
///
/// Adds the second positional `name` argument on top of `Exception#initialize`,
/// stored as the `/name` ivar so `NameError#name` returns it.
///
#[monoruby_builtin]
fn nameerr_initialize(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let class_id = self_.real_class(&globals.store).id();
    let message = if let Some(msg) = lfp.try_arg(0) {
        if msg.is_nil() {
            globals.store.get_class_name(class_id)
        } else {
            msg.coerce_to_string(vm, globals)?
        }
    } else {
        globals.store.get_class_name(class_id)
    };
    self_.is_exception_mut().unwrap().set_message(message);
    if let Some(name) = lfp.try_arg(1) {
        if !name.is_nil() {
            // Store the name verbatim — typically a Symbol — so that
            // `NameError#name` can return it as-is.
            globals
                .store
                .set_ivar(self_, IdentId::_NAME, name)?;
        }
    }
    Ok(Value::nil())
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
fn message(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // CRuby's `Exception#message` is `self.to_s`, so a subclass that
    // overrides `#to_s` changes the reported message too.
    vm.invoke_method_inner(globals, IdentId::TO_S, lfp.self_val(), &[], None, None)
}

///
/// ### Exception#backtrace
///
/// - backtrace -> [String] | nil
///
/// Returns the backtrace explicitly stored via `#set_backtrace` if
/// any; otherwise the trace captured when the exception was raised.
/// Returns `nil` for an exception that has neither been raised nor had
/// a backtrace assigned.
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

///
/// ### Exception#set_backtrace
///
/// - set_backtrace(errinfo) -> Array
///
/// Stores *errinfo* so subsequent `#backtrace` calls return it.
/// A String is wrapped in a single-element Array, `nil` clears the
/// stored backtrace, and an Array of Strings is used as-is. Anything
/// else raises `TypeError`.
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

///
/// ### Exception#exception
///
/// - exception -> self
/// - exception(error_message) -> Exception
///
/// With no arguments (or with `self` as the argument) returns `self`.
/// Otherwise returns a copy of `self` (via `#dup`, so subclass-defined
/// instance variables such as `CustomArgumentError#val` are preserved)
/// with *error_message* coerced through `#to_str` as the new `#message`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Exception/i/exception.html]
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

///
/// ### Exception#==
///
/// - self == other -> bool
///
/// Returns `true` when *other* is an Exception of the same class with
/// the same `#message` and an equal `#backtrace`; otherwise `false`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Exception/i/=3d=3d.html]
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
/// `status` may be an Integer (used as exit status), `true` (status 0),
/// `false` (status 1), or `nil` (status 0, default message). Any other
/// value is treated as the message and the status defaults to 0.
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
    let default_msg = class_id.get_name(&globals.store);
    // Determine `status` from arg0 and whether arg0 should also act as the
    // message (only when it's not Integer / Bool / nil).
    let (status, arg0_as_msg) = if let Some(arg0) = lfp.try_arg(0) {
        match arg0.unpack() {
            RV::Bool(true) => (0, None),
            RV::Bool(false) => (1, None),
            RV::Nil => (0, None),
            RV::Fixnum(_) | RV::BigInt(_) => (arg0.coerce_to_int_i64(vm, globals)?, None),
            _ => (0, Some(arg0)),
        }
    } else {
        (0, None)
    };
    // Resolve the message: arg1 takes precedence; otherwise arg0 (if it
    // wasn't an Integer/Bool); otherwise the class name.
    let msg = if let Some(arg1) = lfp.try_arg(1) {
        if arg1.is_nil() {
            default_msg.clone()
        } else {
            arg1.coerce_to_str(vm, globals)?
        }
    } else if let Some(arg0) = arg0_as_msg {
        arg0.coerce_to_str(vm, globals)?
    } else {
        default_msg.clone()
    };
    let ex = Value::new_exception_from(msg, class_id);
    globals
        .store
        .set_ivar(ex, IdentId::get_id("/status"), Value::integer(status))
        .unwrap();

    Ok(ex)
}

///
/// ### SystemExit#success?
///
/// - success? -> bool
///
/// Returns `true` if the exit status is 0.
///
/// [https://docs.ruby-lang.org/ja/latest/method/SystemExit/i/success=3f.html]
#[monoruby_builtin]
fn system_exit_success_p(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let status = globals
        .store
        .get_ivar(self_, IdentId::get_id("/status"))
        .and_then(|v| v.try_fixnum())
        .unwrap_or(0);
    Ok(Value::bool(status == 0))
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
    fn error_receiver_attributes() {
        // `NameError#receiver` / `#name` for a private-constant access
        // expose the *defining* class and the constant symbol;
        // `FrozenError#receiver` is the frozen object; `NoMethodError`
        // inherits `#receiver`.
        run_test(
            r#"
            module RcvV
              class Base; PRIV = 1; private_constant :PRIV; end
              class Child < Base; end
            end
            begin
              RcvV::Child::PRIV
            rescue NameError => e
              [e.receiver.equal?(RcvV::Base), e.name]
            end
            "#,
        );
        run_test(
            r#"
            o = Object.new
            o.freeze
            begin
              def o.x; end
            rescue FrozenError => e
              [e.receiver.equal?(o), e.message.include?("can't modify frozen Object")]
            end
            "#,
        );
        run_test(
            r#"
            obj = Object.new
            begin; obj.nope_method; rescue NoMethodError => e; e.receiver.equal?(obj); end
            "#,
        );
    }

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
        run_tests(&[
            r#"
            e = RuntimeError.new("test")
            e.set_backtrace(["foo.rb:1:in `bar'", "baz.rb:2:in `qux'"])
        "#,
            r#"
            e = RuntimeError.new("test")
            e.set_backtrace("foo.rb:1:in `bar'")
        "#,
            r#"
            e = RuntimeError.new("test")
            e.set_backtrace(nil)
        "#,
        ]);
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
    fn system_exit_initialize_dispatch() {
        run_test(
            r#"
            [
              [SystemExit.new.status, SystemExit.new.message],
              [SystemExit.new(0).status, SystemExit.new(0).message],
              [SystemExit.new(7).status, SystemExit.new(7).message],
              [SystemExit.new("msg").status, SystemExit.new("msg").message],
              [SystemExit.new(true).status, SystemExit.new(true).message],
              [SystemExit.new(false).status, SystemExit.new(false).message],
              [SystemExit.new(nil).status, SystemExit.new(nil).message],
              [SystemExit.new(3, "x").status, SystemExit.new(3, "x").message],
              [SystemExit.new(true, "y").status, SystemExit.new(true, "y").message],
              [SystemExit.new(2, nil).status, SystemExit.new(2, nil).message],
            ]
            "#,
        );
    }

    #[test]
    fn system_exit_success_p() {
        run_test(
            r#"
            [
              SystemExit.new.success?,
              SystemExit.new(0).success?,
              SystemExit.new(1).success?,
              SystemExit.new(true).success?,
              SystemExit.new(false).success?,
            ]
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
    fn reraise_preserves_identity() {
        // `raise exc` re-raises the same object: identity and instance
        // variables survive being caught.
        run_test(
            r#"
            class MyErr < StandardError
              attr_accessor :data
            end
            e = MyErr.new("boom")
            e.data = 42
            begin
              raise e
            rescue MyErr => x
              [x.equal?(e), x.data, x.message]
            end
            "#,
        );
        // `raise ClassWithCustomState` keeps the freshly-built instance.
        run_test(
            r#"
            class Built < StandardError
              def initialize(n); super("n=#{n}"); @n = n; end
              attr_reader :n
            end
            begin
              raise Built.new(7)
            rescue => e
              [e.n, e.message]
            end
            "#,
        );
        // Re-raise does not overwrite an existing backtrace.
        run_test(
            r#"
            first = nil
            begin; raise "a"; rescue => first; end
            bt = first.backtrace
            begin; raise first; rescue => second; end
            [second.equal?(first), second.backtrace == bt]
            "#,
        );
        // A re-raised object is its successor's implicit cause by identity.
        run_test(
            r#"
            cause = RuntimeError.new("cause")
            begin
              raise cause
            rescue
              begin
                1 / 0
              rescue ZeroDivisionError => z
                z.cause.equal?(cause)
              end
            end
            "#,
        );
        // Re-raising the active exception itself records no self-cause.
        run_test(
            r#"
            begin
              raise RuntimeError, "x"
            rescue RuntimeError => e
              begin; raise e; rescue => f; [f.equal?(e), f.cause.inspect]; end
            end
            "#,
        );
        // Bare `raise` re-raises `$!` by identity, with no self-cause.
        run_test(
            r#"
            begin
              raise "x"
            rescue => e
              begin; raise; rescue => f; [f.equal?(e), f.cause.inspect]; end
            end
            "#,
        );
    }

    #[test]
    fn explicit_cause() {
        run_tests(&[
            // `cause:` sets the cause explicitly.
            r#"
            c = StandardError.new("orig")
            begin; raise "new", cause: c; rescue => e; e.cause.equal?(c); end
            "#,
            // `cause: nil` on a fresh exception ⇒ nil cause.
            r#"begin; raise "x", cause: nil; rescue => e; e.cause.inspect; end"#,
            // `cause:` overrides the implicit `$!` cause.
            r#"
            custom = StandardError.new("custom")
            begin
              raise "first"
            rescue
              begin; raise "second", cause: custom; rescue => e; e.cause.equal?(custom); end
            end
            "#,
            // `cause: nil` inside a rescue suppresses implicit chaining.
            r#"
            begin
              raise "first"
            rescue
              begin; raise "second", cause: nil; rescue => e; e.cause.inspect; end
            end
            "#,
            // Exception class with message and explicit cause.
            r#"
            cm = StandardError.new("cm")
            begin; raise ArgumentError, "am", cause: cm; rescue => e; [e.class.name, e.message, e.cause.equal?(cm)]; end
            "#,
            // A cause equal to the raised exception is ignored.
            r#"
            ce = StandardError.new("cause")
            begin; raise ce, cause: ce; rescue => e; [e.equal?(ce), e.cause.inspect]; end
            "#,
            // `cause: nil` does not overwrite an existing cause.
            r#"
            e1 = nil; e2 = nil
            begin
              begin; raise "1"; rescue => e1; raise "2"; end
            rescue => e2
            end
            begin; raise e2, cause: nil; rescue => e; e.cause.equal?(e1); end
            "#,
        ]);
        // Non-exception cause ⇒ TypeError.
        run_test(
            r#"
            begin; raise "m", cause: Object.new; rescue TypeError => e; e.message; end
            "#,
        );
        // A circular cause ⇒ ArgumentError "circular causes".
        run_test(
            r#"
            begin
              begin
                raise "Error 1"
              rescue => e1
                begin
                  raise "Error 2"
                rescue => e2
                  begin
                    raise "Error 3"
                  rescue => e3
                    raise(e1, cause: e3)
                  end
                end
              end
            rescue ArgumentError => e
              e.message
            end
            "#,
        );
        // Only `cause:` with no positional argument ⇒ ArgumentError.
        run_test(
            r#"begin; raise(cause: nil); rescue ArgumentError => e; e.message; end"#,
        );
    }

    #[test]
    fn message_calls_to_s() {
        run_tests(&[
            // Default: message == stored string.
            r#"RuntimeError.new("hi").message"#,
            // No-arg: message == class name.
            r#"RuntimeError.new.message"#,
            // A subclass overriding #to_s changes #message (CRuby:
            // `Exception#message` is `self.to_s`).
            r#"
            class MsgToS < StandardError
              def to_s; "from to_s"; end
            end
            MsgToS.new("orig").message
            "#,
        ]);
    }

    #[test]
    fn implicit_cause_chaining() {
        // An exception raised while handling another records that
        // exception as its #cause.
        run_test(
            r#"
            begin
              begin
                raise "the cause"
              rescue
                raise "the consequence"
              end
            rescue => e
              [e.message, e.cause&.message]
            end
            "#,
        );
        // VM-raised error (1/0) during a rescue is chained too.
        run_test(
            r#"
            begin
              raise "user"
            rescue
              begin
                1 / 0
              rescue ZeroDivisionError => z
                [z.class.name, z.cause&.message]
              end
            end
            "#,
        );
        // No spurious chaining once a rescue has completed.
        run_test(
            r#"
            begin; raise "a"; rescue; end
            begin; raise "b"; rescue => e; e.cause.inspect; end
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
    fn nomethoderror_name() {
        // `NoMethodError#name` (inherited from NameError) returns the missing
        // method's name — for an ordinary missing method, an operator, a
        // private/protected call, and a failed `super`.
        run_tests(&[
            r#"begin; nil.foobar; rescue NoMethodError => e; e.name; end"#,
            r#"begin; nil + 3; rescue NoMethodError => e; e.name; end"#,
            r#"
            class C; private def sec; end; end
            begin; C.new.sec; rescue NoMethodError => e; e.name; end
            "#,
            r#"
            class A; end
            class B < A; def m; super; end; end
            begin; B.new.m; rescue NoMethodError => e; e.name; end
            "#,
            // `name` and `receiver` are both populated.
            r#"
            o = Object.new
            begin; o.nope; rescue NoMethodError => e; [e.name, e.receiver.equal?(o)]; end
            "#,
        ]);
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
        run_tests(&[
            r#"
            begin
              raise "test error"
            rescue => e
              e.full_message(order: :top).include?("test error")
            end
            "#,
            r#"
            begin
              raise "test error"
            rescue => e
              e.full_message(order: :top).include?("RuntimeError")
            end
            "#,
        ]);
    }

    #[test]
    fn detailed_message() {
        run_test(
            r#"
            [RuntimeError.new("boom").detailed_message,
             RuntimeError.new("").detailed_message,
             ArgumentError.new("").detailed_message,
             RuntimeError.new("x").detailed_message(highlight: true),
             ArgumentError.new("bad").detailed_message(highlight: true),
             RuntimeError.new("a\nb").detailed_message,
             Class.new(StandardError).new("m").detailed_message,
             RuntimeError.new("").detailed_message(highlight: true),
             ArgumentError.new("z").detailed_message(foo: 1)]
            "#,
        );
    }

    #[test]
    fn warning_module() {
        run_tests(&[
            r#"Warning.categories.sort"#,
            r#"Warning[:strict_unused_block]"#,
            r#"Warning.singleton_class.ancestors.include?(Warning)"#,
        ]);
        run_test_error(r#"Warning[42]"#);
        run_test_error(r#"Warning["noop"]"#);
        run_test_error(r#"Warning[:nope]"#);
        run_test_error(r#"Warning[:deprecated] = "x"; Warning[1] = false"#);
    }

    // -- Exception#== ------------------------------------------------------
    // -- Exception#exception ----------------------------------------------

    #[test]
    fn exception_eq_and_method_merged() {
        run_tests(&[
            // Identity-equal exceptions are equal.
            r#"
            e = ArgumentError.new
            e == e
            "#,
            // Exceptions of the same class with the same nil message and no
            // backtrace are equal.
            r#"RuntimeError.new == RuntimeError.new"#,
            // Exceptions of the same class with the same explicit message are
            // equal.
            r#"TypeError.new("message") == TypeError.new("message")"#,
            // Different classes are not equal even with the same message.
            r#"RuntimeError.new("m") == TypeError.new("m")"#,
            // Different messages, same class — not equal.
            r#"RuntimeError.new("a") == RuntimeError.new("b")"#,
            // Identical user-set backtraces produce equal exceptions.
            r#"
            one = TypeError.new("m"); one.set_backtrace(["here.rb:1"])
            two = TypeError.new("m"); two.set_backtrace(["here.rb:1"])
            one == two
            "#,
            // Differing user-set backtraces make exceptions unequal.
            r#"
            one = RuntimeError.new("m"); one.set_backtrace(["a.rb:1"])
            two = RuntimeError.new("m"); two.set_backtrace(["b.rb:2"])
            one == two
            "#,
            // Comparing an exception against a non-Exception returns false
            // without raising.
            r#"RuntimeError.new("m") == "m""#,
            // `e.exception` with no argument returns self.
            r#"
            e = RuntimeError.new("m")
            e.exception.equal?(e)
            "#,
            // `e.exception(self)` returns self.
            r#"
            e = RuntimeError.new("m")
            e.exception(e).equal?(e)
            "#,
            // `e.exception("new")` returns a fresh same-class exception with
            // the new message.
            r#"
            e = RuntimeError.new("m")
            e2 = e.exception("new")
            [e2.class, e2.message, e2.equal?(e)]
            "#,
        ]);
    }

    /// `e.exception("new")` is created via #dup so subclass-defined
    /// instance variables (here `@val`) are preserved without
    /// re-running #initialize.
    #[test]
    fn exception_method_preserves_subclass_ivars() {
        run_test(
            r#"
            class MonoRubyExceptionTestErr < StandardError
              attr_reader :val
              def initialize(val)
                @val = val
              end
            end
            e  = MonoRubyExceptionTestErr.new(:boom)
            e2 = e.exception("new message")
            [e2.class, e2.message, e2.val]
            "#,
        );
    }

    // -- Exception#backtrace / set_backtrace -------------------------------

    #[test]
    fn backtrace_set_backtrace_merged() {
        run_tests(&[
            // A fresh, never-raised exception has nil backtrace.
            r#"Exception.new.backtrace"#,
            // `set_backtrace(nil)` clears any stored backtrace; subsequent
            // `backtrace` returns nil.
            r#"
            e = RuntimeError.new("m")
            e.set_backtrace(["x.rb:1"])
            e.set_backtrace(nil)
            e.backtrace
            "#,
            // `set_backtrace(String)` stores it as a single-element array.
            r#"
            e = RuntimeError.new("m")
            e.set_backtrace("here.rb:1")
            e.backtrace
            "#,
            // An Array of Strings is stored as-is and reflected in #backtrace.
            r#"
            e = RuntimeError.new("m")
            e.set_backtrace(["a.rb:1", "b.rb:2"])
            e.backtrace
            "#,
        ]);
    }

    /// Bad inputs to set_backtrace raise TypeError.
    #[test]
    fn set_backtrace_typeerror() {
        run_test_error(r#"RuntimeError.new.set_backtrace(:not_a_string)"#);
        run_test_error(r#"RuntimeError.new.set_backtrace([1, 2])"#);
        run_test_error(r#"RuntimeError.new.set_backtrace(["ok", nil])"#);
        run_test_error(r#"RuntimeError.new.set_backtrace([["nested"]])"#);
    }

    /// `Exception#backtrace_locations` returns nil when backtrace is
    /// nil (regression: previously raised NoMethodError on nil#map).
    #[test]
    fn backtrace_locations_nil_when_unset() {
        run_test(r#"Exception.new.backtrace_locations"#);
    }

    /// `Interrupt < SignalException` (A4 in doc/signal_handling.md): the
    /// SIGINT path raises a real `Interrupt`, caught by
    /// `rescue SignalException` and `rescue Exception`, with the
    /// conventional default message. Only assertions that agree with
    /// CRuby are checked here (SignalException's own superclass differs).
    #[test]
    fn interrupt_class_hierarchy() {
        run_test(
            r##"
            [
              Interrupt.superclass.name,
              Interrupt < SignalException,
              Interrupt < Exception,
              Interrupt.new("boom").message,
              Interrupt.new.message,
              (begin; raise Interrupt, "interrupted"; rescue SignalException => e; "#{e.class}: #{e.message}"; end),
              (begin; raise Interrupt; rescue SignalException => e; e.class.name; end),
              (begin; raise Interrupt; rescue Exception => e; e.class.name; end),
            ]
            "##,
        );
    }

    #[test]
    fn non_standard_error_hierarchy() {
        // These classes inherit directly from `Exception`, not
        // `StandardError`, so a bare `rescue` must not catch them.
        run_test(
            r#"
            [
              NoMemoryError, ScriptError, SecurityError, SignalException,
              SystemStackError, LoadError, SyntaxError, NotImplementedError
            ].map { |k| [k.name, k < StandardError, k < Exception] }
            "#,
        );
    }

    #[test]
    fn bare_rescue_only_catches_standard_error() {
        // A bare `rescue` (== `rescue StandardError`) catches StandardError
        // and its subclasses, but a non-StandardError propagates.
        run_test(
            r#"
            def caught?(k)
              begin
                begin
                  raise k
                rescue
                  :caught
                end
              rescue Exception
                :propagated
              end
            end
            [
              caught?(StandardError), caught?(RuntimeError), caught?(ArgumentError),
              caught?(Exception), caught?(NoMemoryError), caught?(ScriptError),
              caught?(SecurityError), caught?(SystemStackError), caught?(NotImplementedError)
            ]
            "#,
        );
        // Inline (modifier) `rescue` also only rescues StandardError.
        run_test(r#"raise(StandardError) rescue 1"#);
        run_test(
            r#"
            begin
              raise(Exception) rescue 1
              :not_reached
            rescue Exception
              :reraised
            end
            "#,
        );
        // A bare `rescue` with a non-local capture target (regression: the
        // capture target's scratch temps must not corrupt the match stack).
        run_test(
            r#"
            class C
              def initialize; @h = {}; end
              def []=(k, v); @h[k] = v; end
              def [](k); @h[k]; end
              def capture(msg)
                raise msg
              rescue => self[:error]
                :caught
              end
            end
            c = C.new
            [c.capture("boom"), c[:error].message]
            "#,
        );
    }
}
