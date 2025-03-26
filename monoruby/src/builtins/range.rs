use super::*;

//
// Range class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Range", RANGE_CLASS, ObjTy::RANGE);
    globals.define_builtin_class_func_with(RANGE_CLASS, "new", range_new, 2, 2, false);
    globals.define_builtin_func(RANGE_CLASS, "begin", begin, 0);
    globals.define_builtin_func(RANGE_CLASS, "end", end, 0);
    globals.define_builtin_func(RANGE_CLASS, "exclude_end?", exclude_end, 0);
    globals.define_builtin_func(RANGE_CLASS, "each", each, 0);
    //globals.define_builtin_func(RANGE_CLASS, "reject", reject, 0);
    globals.define_builtin_func(RANGE_CLASS, "all?", all_, 0);
    globals.define_builtin_funcs(RANGE_CLASS, "collect", &["map"], map, 0);
    globals.define_builtin_funcs(RANGE_CLASS, "collect_concat", &["flat_map"], flat_map, 0);
    globals.define_builtin_funcs(RANGE_CLASS, "entries", &["to_a"], toa, 0);
}

///
/// ### Range.new
///
/// - new(first, last, exclude_end = false) -> Range
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/s/new.html]
#[monoruby_builtin]
fn range_new(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    globals.generate_range(lfp.arg(0), lfp.arg(1), false)
}

///
/// ### Range#begin
///
/// - begin -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/begin.html]
#[monoruby_builtin]
fn begin(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(lfp.self_val().as_range().start)
}

///
/// Range#end
///
/// - end -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/end.html]
#[monoruby_builtin]
fn end(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(lfp.self_val().as_range().end)
}

/// Range#exclude_end?
/// - exclude_end? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/exclude_end=3f.html]
#[monoruby_builtin]
fn exclude_end(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn each(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let self_ = lfp.self_val();
    let range = self_.as_range();
    if range.start.is_fixnum() && range.end.is_fixnum() {
        let start = range.start.expect_integer()?;
        let mut end = range.end.expect_integer()?;
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

/*
///
/// ### Enumerable#reject
///
/// - reject {|item| ... } -> [object]
/// - reject -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/reject.html]
#[monoruby_builtin]
fn reject(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let range = self_.as_range();
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let mut res = vec![];
        let mut elem = range.start;
        let end = range.end;
        while !vm
            .invoke_method_inner(globals, IdentId::_EQ, elem, &[end], None)?
            .as_bool()
        {
            if !vm.invoke_block(globals, &data, &[elem])?.as_bool() {
                res.push(elem);
            };
            elem = vm.invoke_method_inner(globals, IdentId::get_id("succ"), elem, &[], None)?;
        }
        Ok(Value::array_from_vec(res))
    } else {
        vm.generate_enumerator(IdentId::get_id("reject"), lfp.self_val(), vec![])
    }
}*/

///
/// ### Range#all
///
/// - all? -> bool
/// - all? {|item| ... } -> bool
/// - all?(pattern) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/all=3f.html]
#[monoruby_builtin]
fn all_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        let self_ = lfp.self_val();
        let range = self_.as_range();
        if range.start.is_fixnum() && range.end.is_fixnum() {
            let start = range.start.expect_integer()?;
            let mut end = range.end.expect_integer()?;
            if !range.exclude_end() {
                end += 1
            }

            let iter = (start..end).map(Value::fixnum);
            let data = vm.get_block_data(globals, bh)?;
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
fn map(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let self_ = lfp.self_val();
    let range = self_.as_range();
    if range.start.is_fixnum() && range.end.is_fixnum() {
        let start = range.start.expect_integer()?;
        let mut end = range.end.expect_integer()?;
        if !range.exclude_end() {
            end += 1
        }

        if end <= start {
            return Ok(Value::array_from_vec(vec![]));
        }

        let iter = (start..end).map(Value::fixnum);
        vm.invoke_block_map1(globals, bh, iter, (end - start).unsigned_abs() as usize)
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
fn flat_map(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let self_ = lfp.self_val();
    let range = self_.as_range();
    if range.start.is_fixnum() && range.end.is_fixnum() {
        let start = range.start.expect_integer()?;
        let mut end = range.end.expect_integer()?;
        if !range.exclude_end() {
            end += 1
        }

        if end <= start {
            return Ok(Value::array_from_vec(vec![]));
        }

        let iter = (start..end).map(Value::fixnum);
        vm.invoke_block_flat_map1(globals, bh, iter, (end - start).unsigned_abs() as usize)
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
fn toa(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let range = self_.as_range();
    if let Some(start) = range.start.try_fixnum()
        && let Some(mut end) = range.end.try_fixnum()
    {
        if !range.exclude_end() {
            end += 1
        }

        let vec = (start..end).map(Value::fixnum).collect();
        Ok(Value::array_from_vec(vec))
    } else if let Some(start) = range.start.is_str()
        && let Some(end) = range.end.is_str()
    {
        let mut start = start.to_string();
        let mut v = vec![];
        while start != end {
            v.push(Value::string_from_str(&start));
            start = builtins::string::str_next(&start);
        }
        if !range.exclude_end() {
            v.push(Value::string_from_str(&start));
        }
        Ok(Value::array_from_vec(v))
    } else {
        Err(MonorubyErr::runtimeerr("not supported"))
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

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
        run_test(
            r#"
        (1..1).map do |x|
            x + 100
        end
        "#,
        );
        run_test(
            r#"
        (5..1).map do |x|
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
