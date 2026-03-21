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
/// ### Process.#clock_gettime
///
/// - clock_gettime(clock_id, unit=:float_second) -> Float | Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Process/m/clock_gettime.html]
#[monoruby_builtin]
fn clock_gettime(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let unit = if let Some(arg1) = lfp.try_arg(1) {
        match arg1.try_symbol() {
            Some(id) => id,
            None => {
                return Err(MonorubyErr::argumenterr(format!(
                    "unexpected unit: {}",
                    arg1.to_s(&globals.store)
                )))
            }
        }
    } else {
        IdentId::FLOAT_SECOND
    };
    let mut tp = TimeSpec::default();
    let clk_id = lfp.arg(0).coerce_to_i64(globals)? as i32;
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
            )))
        }
    })
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
}
