use crate::*;

//
// IO class
//

pub(super) fn init(_globals: &mut Globals) {
    //globals.define_builtin_singleton_func(IO_CLASS, "new", now, 0);
    //globals.define_builtin_func(IO_CLASS, "-", sub, 1);
}

/*
/// ### Time.new
/// - new -> Time
/// - now -> Time
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/s/new.html]
extern "C" fn now(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    Some(Value::nil())
}
*/

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn test() {
        run_test("");
    }
}
