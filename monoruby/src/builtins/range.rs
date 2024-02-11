use super::*;

//
// Range class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Range", RANGE_CLASS);
    globals.define_builtin_class_func_with(RANGE_CLASS, "new", range_new, 2, 3, false);
    globals.define_builtin_func(RANGE_CLASS, "begin", begin, 0);
    globals.define_builtin_func(RANGE_CLASS, "end", end, 0);
    globals.define_builtin_func(RANGE_CLASS, "exclude_end?", exclude_end, 0);
    globals.define_builtin_func(RANGE_CLASS, "each", each, 0);
    globals.define_builtin_func(RANGE_CLASS, "all?", all_, 0);
    globals.define_builtin_func(RANGE_CLASS, "collect", map, 0);
    globals.define_builtin_func(RANGE_CLASS, "map", map, 0);
    globals.define_builtin_func(RANGE_CLASS, "collect_concat", flat_map, 0);
    globals.define_builtin_func(RANGE_CLASS, "flat_map", flat_map, 0);
    globals.define_builtin_func(RANGE_CLASS, "entries", toa, 0);
    globals.define_builtin_func(RANGE_CLASS, "to_a", toa, 0);
}

///
/// ### Range.new
///
/// - new(first, last, exclude_end = false) -> Range
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/s/new.html]
#[monoruby_builtin]
fn range_new(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    lfp.check_number_of_arguments_range(2..=3)?;
    globals.generate_range(lfp.arg(0), lfp.arg(1), false)
}

///
/// ### Range#begin
///
/// - begin -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/begin.html]
#[monoruby_builtin]
fn begin(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    Ok(lfp.self_val().as_range().start)
}

///
/// Range#end
///
/// - end -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/end.html]
#[monoruby_builtin]
fn end(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    Ok(lfp.self_val().as_range().end)
}

/// Range#exclude_end?
/// - exclude_end? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/exclude_end=3f.html]
#[monoruby_builtin]
fn exclude_end(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
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
fn each(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let self_ = lfp.self_val();
    let range = self_.as_range();
    if range.start.is_fixnum() && range.end.is_fixnum() {
        let start = range.start.as_fixnum();
        let mut end = range.end.as_fixnum();
        if !range.exclude_end() {
            end += 1
        }

        let iter = (start..end).map(Value::fixnum);
        vm.invoke_block_iter1(globals, bh, iter)?;
        Ok(self_)
    } else {
        Err(MonorubyErr::runtimeerr("not supported"))
    }
}

///
/// ### Range#all
///
/// - all? -> bool
/// - all? {|item| ... } -> bool
/// - all?(pattern) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/all=3f.html]
#[monoruby_builtin]
fn all_(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        let self_ = lfp.self_val();
        let range = self_.as_range();
        if range.start.is_fixnum() && range.end.is_fixnum() {
            let start = range.start.as_fixnum();
            let mut end = range.end.as_fixnum();
            if !range.exclude_end() {
                end += 1
            }

            let iter = (start..end).map(Value::fixnum);
            let data = globals.get_block_data(vm.cfp(), bh);
            for val in iter {
                if !vm.invoke_block(globals, &data, &[val])?.as_bool() {
                    return Ok(Value::bool(false));
                };
            }
            Ok(Value::bool(true))
        } else {
            Err(MonorubyErr::runtimeerr("not supported"))
        }
    } else {
        Ok(Value::bool(true))
    }
}

///
/// ### Enumerable#map
///
/// - [NOT SUPPORTED]collect -> Enumerator
/// - [NOT SUPPORTED]map -> Enumerator
/// - collect {|item| ... } -> [object]
/// - map {|item| ... } -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/collect.html]
#[monoruby_builtin]
fn map(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let self_ = lfp.self_val();
    let range = self_.as_range();
    if range.start.is_fixnum() && range.end.is_fixnum() {
        let start = range.start.as_fixnum();
        let mut end = range.end.as_fixnum();
        if !range.exclude_end() {
            end += 1
        }

        let iter = (start..end).map(Value::fixnum);
        let vec = vm.invoke_block_map1(globals, bh, iter)?;
        Ok(Value::array_from_vec(vec))
    } else {
        Err(MonorubyErr::runtimeerr("not supported"))
    }
}

/// ### Enumerable#collect_concat
///
/// - flat_map {| obj | block } -> Array
/// - collect_concat {| obj | block } -> Array
/// - [NOT SUPPORTED] flat_map -> Enumerator
/// - [NOT SUPPORTED] collect_concat -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/collect_concat.html]
#[monoruby_builtin]
fn flat_map(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let self_ = lfp.self_val();
    let range = self_.as_range();
    if range.start.is_fixnum() && range.end.is_fixnum() {
        let start = range.start.as_fixnum();
        let mut end = range.end.as_fixnum();
        if !range.exclude_end() {
            end += 1
        }
        let iter = (start..end).map(Value::fixnum);

        let v = vm.flat_map(globals, bh, iter)?;
        Ok(Value::array_from_vec(v))
    } else {
        Err(MonorubyErr::runtimeerr("not supported"))
    }
}

///
/// ### Range#entries
///
/// - to_a -> Array
/// - entries -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/entries.html]
#[monoruby_builtin]
fn toa(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let self_ = lfp.self_val();
    let range = self_.as_range();
    if range.start.is_fixnum() && range.end.is_fixnum() {
        let start = range.start.as_fixnum();
        let mut end = range.end.as_fixnum();
        if !range.exclude_end() {
            end += 1
        }

        let vec = (start..end).map(Value::fixnum).collect();
        Ok(Value::array_from_vec(vec))
    } else {
        Err(MonorubyErr::runtimeerr("not supported"))
    }
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
        run_test("(1...5).to_a");
    }

    #[test]
    fn each() {
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

    #[test]
    fn map() {
        run_test(
            r#"
        (1...5).map do |x|
            x * 100
        end
        "#,
        );
        run_test(
            r#"
        (1..5).map do |x|
            x + 100
        end
        "#,
        );
    }

    #[test]
    fn flat_map() {
        run_test(
            r#"
        (1...5).flat_map do |x|
            [x] * x
        end
        "#,
        );
        run_test(
            r#"
        (1..5).flat_map do |x|
            [x] * x
        end
        "#,
        );
    }

    #[test]
    fn all() {
        run_test(
            r#"
        (1...5).all? do |x|
            x > 0
        end
        "#,
        );
        run_test(
            r#"
        (1...5).all? do |x|
            x != 5
        end
        "#,
        );
    }
}
