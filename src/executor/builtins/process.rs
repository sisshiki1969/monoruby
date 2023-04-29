use crate::*;

use self::clock_gettime::TimeSpec;

mod clock_gettime;
mod rusage;

//
// Process module
//

pub(super) fn init(globals: &mut Globals, class_id: ClassId) {
    globals.define_class_by_str("Tms", OBJECT_CLASS.get_obj(globals), class_id);
    globals.define_builtin_module_func(class_id, "pid", pid, 0);
    globals.define_builtin_module_func(class_id, "times", times, 0);
    globals.define_builtin_module_func(class_id, "clock_gettime", clock_gettime, 1);
}

///
/// ### Process.#times
///
/// - times -> Process::Tms
///
/// [https://docs.ruby-lang.org/ja/latest/method/Process/m/times.html]
fn times(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let tms_class = globals
        .get_qualified_constant(&["Process", "Tms"])?
        .as_class();
    let mut val = Value::new_object(tms_class.id());
    let mut self_rusage = rusage::Rusage::default();
    rusage::getrusage(rusage::RusageWho::Self_, &mut self_rusage);
    val.set_instance_var(
        globals,
        "@utime",
        Value::new_float(self_rusage.ru_utime.get_f64()),
    )?;
    val.set_instance_var(
        globals,
        "@stime",
        Value::new_float(self_rusage.ru_stime.get_f64()),
    )?;
    let mut child_rusage = rusage::Rusage::default();
    rusage::getrusage(rusage::RusageWho::Children, &mut child_rusage);
    val.set_instance_var(
        globals,
        "@cutime",
        Value::new_float(child_rusage.ru_utime.get_f64()),
    )?;
    val.set_instance_var(
        globals,
        "@cstime",
        Value::new_float(child_rusage.ru_stime.get_f64()),
    )?;
    Ok(val)
}

///
/// ### Process.#pid
///
/// - pid -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Process/m/pid.html]
fn pid(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    Ok(Value::new_integer(std::process::id() as i64))
}

///
/// ### Process.#clock_gettime
///
/// - clock_gettime(clock_id, unit=:float_second) -> Float | Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Process/m/clock_gettime.html]
fn clock_gettime(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let mut tp = TimeSpec::default();
    let clk_id = arg[0].coerce_to_fixnum(globals)? as i32;
    clock_gettime::clock_gettime(clk_id, &mut tp);
    Ok(Value::new_float(tp.to_f64()))
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn process() {
        run_test_no_result_check("Process.clock_gettime Process::CLOCK_MONOTONIC");
        run_test_no_result_check("Process.times");
    }
}
