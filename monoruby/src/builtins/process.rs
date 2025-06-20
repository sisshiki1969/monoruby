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
fn times(vm: &mut Executor, globals: &mut Globals, _lfp: Lfp) -> Result<Value> {
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
fn pid(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp) -> Result<Value> {
    Ok(Value::integer(std::process::id() as i64))
}

///
/// ### Process.#clock_gettime
///
/// - clock_gettime(clock_id, unit=:float_second) -> Float | Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Process/m/clock_gettime.html]
#[monoruby_builtin]
fn clock_gettime(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
    clock_gettime::clock_gettime(clk_id, &mut tp);
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
}
