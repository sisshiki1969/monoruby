use crate::*;

//
// Process class
//

pub(super) fn init(globals: &mut Globals, class_id: ClassId) {
    globals.define_builtin_module_func(class_id, "times", times, 0);
}

///
/// ### Process.#times
/// - times -> Process::Tms
///
/// [https://docs.ruby-lang.org/ja/latest/method/Process/m/times.html]
extern "C" fn times(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _: Value,
    _arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    Some(Value::nil())
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn process() {}
}
