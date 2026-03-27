use num::ToPrimitive;

use super::*;

use self::clock_gettime::TimeSpec;

mod clock_gettime;
mod rusage;

//
// Process module
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_toplevel_module("Process").id();
    let object_class = globals.store.object_class();
    globals.define_class("Tms", object_class, klass);
    globals.define_builtin_module_func(klass, "pid", pid, 0);
    globals.define_builtin_module_func(klass, "fork", process_fork, 0);
    globals.define_builtin_module_func(klass, "times", times, 0);
    globals.define_builtin_module_func_with(klass, "clock_gettime", clock_gettime, 1, 2, false);
    globals.define_builtin_module_func_with(klass, "exit!", process_exit_bang, 0, 1, false);
    globals.define_builtin_module_func(klass, "euid", euid, 0);
    globals.define_builtin_module_func(klass, "last_status", last_status, 0);

    // Process::Status class — methods defined in Ruby (startup.rb)
    globals.define_class("Status", object_class, klass);

    // Signal module
    let signal = globals.define_toplevel_module("Signal").id();
    globals.define_builtin_module_func(signal, "list", signal_list, 0);
}

///
/// ### Process.#times
///
/// - times -> Process::Tms
///
/// [https://docs.ruby-lang.org/ja/latest/method/Process/m/times.html]
#[monoruby_builtin]
fn times(vm: &mut Executor, globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let tms_class = vm
        .get_qualified_constant(globals, OBJECT_CLASS, &["Process", "Tms"])?
        .as_class();
    let mut val = Value::object(tms_class.id());
    let mut self_rusage = rusage::Rusage::default();
    rusage::getrusage(rusage::RusageWho::Self_, &mut self_rusage);
    val.set_instance_var(
        &mut globals.store,
        "@utime",
        Value::float(self_rusage.ru_utime.get_f64()),
    )?;
    val.set_instance_var(
        &mut globals.store,
        "@stime",
        Value::float(self_rusage.ru_stime.get_f64()),
    )?;
    let mut child_rusage = rusage::Rusage::default();
    rusage::getrusage(rusage::RusageWho::Children, &mut child_rusage);
    val.set_instance_var(
        &mut globals.store,
        "@cutime",
        Value::float(child_rusage.ru_utime.get_f64()),
    )?;
    val.set_instance_var(
        &mut globals.store,
        "@cstime",
        Value::float(child_rusage.ru_stime.get_f64()),
    )?;
    Ok(val)
}

///
/// ### Process.#pid
///
/// - pid -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Process/m/pid.html]
#[monoruby_builtin]
fn pid(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::integer(std::process::id() as i64))
}

///
/// ### Process.#fork
///
/// - fork -> Integer | nil
/// - fork { ... } -> Integer | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Process/m/fork.html]
#[monoruby_builtin]
fn process_fork(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    // SAFETY: fork() is a POSIX system call. We call it in a single-threaded context.
    let pid = unsafe { libc::fork() };
    if pid < 0 {
        return Err(MonorubyErr::runtimeerr("fork failed"));
    }
    if pid == 0 {
        // Child process
        if let Some(bh) = lfp.block() {
            let data = vm.get_block_data(globals, bh)?;
            match vm.invoke_block(globals, &data, &[]) {
                Ok(_) => std::process::exit(0),
                Err(_) => std::process::exit(1),
            }
        }
        Ok(Value::nil())
    } else {
        // Parent process
        Ok(Value::integer(pid as i64))
    }
}

///
/// ### Process.#exit!
///
/// - exit!(status = false) -> ()
///
/// Exits the process immediately without running at_exit handlers.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Process/m/exit=21.html]
#[monoruby_builtin]
fn process_exit_bang(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let status = if let Some(arg0) = lfp.try_arg(0) {
        if let Some(i) = arg0.try_fixnum() {
            i as i32
        } else {
            match arg0.as_bool() {
                true => 0,
                false => 1,
            }
        }
    } else {
        1
    };
    std::process::exit(status);
}

///
/// ### Process.#clock_gettime
///
/// - clock_gettime(clock_id, unit=:float_second) -> Float | Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Process/m/clock_gettime.html]
#[monoruby_builtin]
fn clock_gettime(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let unit = if let Some(arg1) = lfp.try_arg(1) {
        match arg1.try_symbol() {
            Some(id) => id,
            None => {
                return Err(MonorubyErr::argumenterr(format!(
                    "unexpected unit: {}",
                    arg1.to_s(&globals.store)
                )));
            }
        }
    } else {
        IdentId::FLOAT_SECOND
    };
    let mut tp = TimeSpec::default();
    let clk_id = lfp.arg(0).coerce_to_int(vm, globals)? as i32;
    if let Err(errno) = clock_gettime::clock_gettime(clk_id, &mut tp) {
        return Err(MonorubyErr::runtimeerr(format!(
            "clock_gettime failed: errno {}",
            errno
        )));
    }
    Ok(match unit {
        IdentId::FLOAT_SECOND => Value::float(tp.nanosec().to_f64().unwrap() / 1_000_000_000.0),
        IdentId::FLOAT_MILLISECOND => Value::float(tp.nanosec().to_f64().unwrap() / 1_000_000.0),
        IdentId::FLOAT_MICROSECOND => Value::float(tp.nanosec().to_f64().unwrap() / 1000.0),
        IdentId::SECOND => Value::integer(tp.sec()),
        IdentId::MILLISECOND => Value::integer(tp.millisec()),
        IdentId::MICROSECOND => Value::integer(tp.microsec()),
        IdentId::NANOSECOND => Value::integer(tp.nanosec()),
        _ => {
            return Err(MonorubyErr::argumenterr(format!(
                "unexpected unit: {}",
                lfp.arg(1).to_s(&globals.store)
            )));
        }
    })
}

///
/// ### Process.euid
///
/// - euid -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Process/m/euid.html]
#[monoruby_builtin]
fn euid(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // SAFETY: geteuid is a POSIX system call that is safe to call.
    let uid = unsafe { libc::geteuid() };
    Ok(Value::integer(uid as i64))
}

///
/// ### Signal.list
///
/// ### Process.last_status
///
/// - last_status -> Process::Status | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Process/m/last_status.html]
#[monoruby_builtin]
fn last_status(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let val = globals.get_gvar(IdentId::get_id("$?"));
    Ok(val.unwrap_or_default())
}

/// - list -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/Signal/m/list.html]
#[monoruby_builtin]
fn signal_list(
    vm: &mut Executor,
    globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let signals: &[(&str, i64)] = &[
        ("EXIT", 0),
        ("HUP", 1),
        ("INT", 2),
        ("QUIT", 3),
        ("ILL", 4),
        ("TRAP", 5),
        ("ABRT", 6),
        ("IOT", 6),
        ("BUS", 7),
        ("FPE", 8),
        ("KILL", 9),
        ("USR1", 10),
        ("SEGV", 11),
        ("USR2", 12),
        ("PIPE", 13),
        ("ALRM", 14),
        ("TERM", 15),
        ("STKFLT", 16),
        ("CLD", 17),
        ("CHLD", 17),
        ("CONT", 18),
        ("STOP", 19),
        ("TSTP", 20),
        ("TTIN", 21),
        ("TTOU", 22),
        ("URG", 23),
        ("XCPU", 24),
        ("XFSZ", 25),
        ("VTALRM", 26),
        ("PROF", 27),
        ("WINCH", 28),
        ("IO", 29),
        ("POLL", 29),
        ("PWR", 30),
        ("SYS", 31),
    ];
    let mut map = RubyMap::default();
    for (name, num) in signals {
        map.insert(
            Value::string_from_str(name),
            Value::integer(*num),
            vm,
            globals,
        )?;
    }
    Ok(Value::hash(map))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn process() {
        run_test_no_result_check("Process.clock_gettime Process::CLOCK_MONOTONIC, :nanosecond");
        run_test_no_result_check("Process.clock_gettime Process::CLOCK_MONOTONIC");
        run_test_no_result_check("Process.times");
    }

    #[test]
    fn clock_gettime_invalid() {
        run_test_error("Process.clock_gettime(-1)");
    }

    #[test]
    fn process_fork() {
        run_test(
            r#"
            pid = Process.fork { exit 0 }
            pid.is_a?(Integer)
            "#,
        );
    }

    #[test]
    fn process_euid() {
        run_test_once("Process.euid");
    }

    #[test]
    fn signal_list() {
        run_test_once(
            r#"
            h = Signal.list
            [h.is_a?(Hash), h["INT"], h["KILL"], h["EXIT"]]
            "#,
        );
    }

    #[test]
    fn process_exit_bang() {
        run_test("Process.respond_to?(:exit!)");
    }

    #[test]
    fn process_last_status() {
        run_test_no_result_check("Process.last_status");
        run_test_once(
            r#"
            IO.popen("true") { |io| io.read }
            $?.exitstatus
            "#,
        );
        run_test_once(
            r#"
            IO.popen("false") { |io| io.read }
            $?.exitstatus
            "#,
        );
    }
}
