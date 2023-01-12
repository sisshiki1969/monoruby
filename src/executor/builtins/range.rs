use crate::*;

//
// Range class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_singleton_func(RANGE_CLASS, "new", range_new, -1);
    globals.define_builtin_func(RANGE_CLASS, "begin", begin, 0);
    globals.define_builtin_func(RANGE_CLASS, "end", end, 0);
    globals.define_builtin_func(RANGE_CLASS, "exclude_end?", exclude_end, 0);
}

/// ### Range.new
/// - new(first, last, exclude_end = false) -> Range
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/s/new.html]
extern "C" fn range_new(
    _vm: &mut Executor,
    globals: &mut Globals,
    _: Value,
    arg: Arg,
    len: usize,
    _: Option<Value>,
) -> Option<Value> {
    globals.check_number_of_arguments(len, 2..=3)?;
    globals.generate_range(arg[0], arg[1], false)
}

/// ### Range#begin
/// - begin -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/begin.html]
extern "C" fn begin(
    _vm: &mut Executor,
    _globals: &mut Globals,
    self_val: Value,
    _: Arg,
    _: usize,
    _: Option<Value>,
) -> Option<Value> {
    Some(self_val.as_range().start)
}

/// Range#end
/// - end -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/end.html]
extern "C" fn end(
    _vm: &mut Executor,
    _globals: &mut Globals,
    self_val: Value,
    _: Arg,
    _: usize,
    _: Option<Value>,
) -> Option<Value> {
    Some(self_val.as_range().end)
}

/// Range#exclude_end?
/// - exclude_end? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/exclude_end=3f.html]
extern "C" fn exclude_end(
    _vm: &mut Executor,
    _globals: &mut Globals,
    self_val: Value,
    _: Arg,
    _: usize,
    _: Option<Value>,
) -> Option<Value> {
    Some(Value::bool(self_val.as_range().exclude_end()))
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn range() {
        run_test(
            r##"
          r = Range.new(3,10)
          [r.begin, r.end, r.exclude_end?]
        "##,
        );
        run_test("(1..5).exclude_end?");
        run_test("(1...5).exclude_end?");
    }
}
