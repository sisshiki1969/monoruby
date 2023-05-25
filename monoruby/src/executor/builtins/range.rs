use crate::*;

//
// Range class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(RANGE_CLASS, "new", range_new, -1);
    globals.define_builtin_func(RANGE_CLASS, "begin", begin, 0);
    globals.define_builtin_func(RANGE_CLASS, "end", end, 0);
    globals.define_builtin_func(RANGE_CLASS, "exclude_end?", exclude_end, 0);
    globals.define_builtin_func(RANGE_CLASS, "each", each, 0);
}

/// ### Range.new
/// - new(first, last, exclude_end = false) -> Range
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/s/new.html]
#[monoruby_builtin]
fn range_new(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    Executor::check_number_of_arguments(len, 2..=3)?;
    globals.generate_range(arg[0], arg[1], false)
}

/// ### Range#begin
/// - begin -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/begin.html]
#[monoruby_builtin]
fn begin(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg, _: usize) -> Result<Value> {
    Ok(lfp.self_val().as_range().start)
}

/// Range#end
/// - end -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/end.html]
#[monoruby_builtin]
fn end(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg, _: usize) -> Result<Value> {
    Ok(lfp.self_val().as_range().end)
}

/// Range#exclude_end?
/// - exclude_end? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/exclude_end=3f.html]
#[monoruby_builtin]
fn exclude_end(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _: Arg,
    _: usize,
) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_range().exclude_end()))
}

///
/// ### Range#each
///
/// - each {|item| .... } -> self
/// - [NOT SUPPORTED] each -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/each.html]
#[monoruby_builtin]
fn each(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let block_handler = if let Some(block) = lfp.block() {
        block
    } else {
        return Err(MonorubyErr::no_block_given());
    };
    let self_ = lfp.self_val();
    let range = self_.as_range();
    if range.start.is_fixnum() && range.end.is_fixnum() {
        let start = range.start.as_fixnum();
        let mut end = range.end.as_fixnum();
        if !range.exclude_end() {
            end += 1
        }

        let iter = (start..end).map(|i| Value::fixnum(i));
        vm.invoke_block_iter1(globals, block_handler, iter)?;
    } else {
        unimplemented!()
    }
    Ok(self_)
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
        run_test(
            r#"
        a = 0
        (1...5).each do |x|
            a += x
        end
        a
        "#,
        );
        run_test(
            r#"
        a = 0
        (1..5).each do |x|
            a += x
        end
        a
        "#,
        );
    }
}
