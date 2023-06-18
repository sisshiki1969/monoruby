use crate::*;

//
// Array class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(ARRAY_CLASS, "new", new, -1);
    globals.define_builtin_func(ARRAY_CLASS, "size", size, 0);
    globals.define_builtin_func(ARRAY_CLASS, "length", size, 0);
    globals.define_builtin_func(ARRAY_CLASS, "+", add, 1);
    globals.define_builtin_func(ARRAY_CLASS, "<<", shl, 1);
    globals.define_builtin_func(ARRAY_CLASS, "[]", index, -1);
    globals.define_builtin_func(ARRAY_CLASS, "[]=", index_assign, -1);
    globals.define_builtin_func(ARRAY_CLASS, "inject", inject, -1);
    globals.define_builtin_func(ARRAY_CLASS, "reduce", inject, -1);
    globals.define_builtin_func(ARRAY_CLASS, "join", join, -1);
    globals.define_builtin_func(ARRAY_CLASS, "sum", sum, -1);
    globals.define_builtin_func(ARRAY_CLASS, "each", each, 0);
    globals.define_builtin_func(ARRAY_CLASS, "map", map, 0);
    globals.define_builtin_func(ARRAY_CLASS, "detect", detect, 0);
    globals.define_builtin_func(ARRAY_CLASS, "find", detect, 0);
    globals.define_builtin_func(ARRAY_CLASS, "include?", include_, 1);
    globals.define_builtin_func(ARRAY_CLASS, "reverse", reverse, 0);
    globals.define_builtin_func(ARRAY_CLASS, "reverse!", reverse_, 0);
}

///
/// ### Array.new
///
/// - new(size = 0, val = nil) -> Array
/// - new(ary) -> Array
/// - new(size) {|index| ... } -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/s/new.html]
///
/// TODO: Support arguments.
#[monoruby_builtin]
fn new(vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg, len: usize) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let obj = Value::new_array_with_class(vec![], class);
    vm.invoke_method2_if_exists(globals, IdentId::INITIALIZE, obj, arg, len)?;
    Ok(obj)
}

///
/// ### Array#length
///
/// - length -> Integer
/// - size -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/length.html]
#[monoruby_builtin]
fn size(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let len = lfp.self_val().as_array().len();
    Ok(Value::new_integer(len as i64))
}

///
/// ### Array#+
///
/// - self + other -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=2b.html]
#[monoruby_builtin]
fn add(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let mut lhs = lfp.self_val().as_array().clone();
    let rhs = match arg[0].is_array() {
        Some(v) => v,
        None => {
            return Err(MonorubyErr::no_implicit_conversion(
                globals,
                arg[0],
                ARRAY_CLASS,
            ));
        }
    };
    lhs.extend_from_slice(rhs);
    Ok(Value::new_array(lhs))
}

///
/// ### Array#<<
///
/// - self << obj -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=3c=3c.html]
#[monoruby_builtin]
fn shl(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    self_.as_array_mut().push(arg[0]);
    Ok(self_)
}

///
/// ### Array#[]
///
/// - self[nth] -> object | nil
/// - self[range] -> Array | nil
/// - self[start, length] -> Array | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=5b=5d.html]
#[monoruby_builtin]
fn index(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    Executor::check_number_of_arguments(len, 1..=2)?;
    if len == 1 {
        let idx = arg[0];
        lfp.self_val().as_array().get_elem1(globals, idx)
    } else {
        lfp.self_val().as_array().get_elem2(globals, arg[0], arg[1])
    }
}

///
/// ### Array#[]=
///
/// - self[nth] = val
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=5b=5d=3d.html]
#[monoruby_builtin]
fn index_assign(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    if len == 2 {
        let i = arg[0];
        let val = arg[1];
        if let Some(idx) = i.try_fixnum() {
            return lfp.self_val().as_array_mut().set_index(idx, val);
        } else {
            unimplemented!()
        }
    } else if len == 3 {
        let i = arg[0].coerce_to_fixnum(globals)?;
        let l = arg[1].coerce_to_fixnum(globals)?;
        if l < 0 {
            return Err(MonorubyErr::indexerr(format!("negative length ({})", l)));
        }
        if i < 0 {
            return Err(MonorubyErr::index_too_small(i, 0));
        }
        let val = arg[2];
        return lfp
            .self_val()
            .as_array_mut()
            .set_index2(i as usize, l as usize, val);
    } else {
        Err(MonorubyErr::wrong_number_of_arg_range(len, 2..=3))
    }
}

///
/// ### Array#inject
///
/// - inject(init = self.first) {|result, item| ... } -> object
/// - reduce(init = self.first) {|result, item| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/inject.html]
#[monoruby_builtin]
fn inject(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    Executor::check_number_of_arguments(len, 0..=1)?;
    let block_handler = lfp.expect_block()?;
    let self_ = lfp.self_val();
    let mut iter = self_.as_array().iter().cloned();
    let res = if len == 0 {
        iter.next().unwrap_or_default()
    } else {
        arg[0]
    };
    vm.invoke_block_fold1(globals, block_handler, iter, res)
}

///
/// ### Array#join
///
/// - join(sep = $,) -> String
/// TODO: support recursive join for Array class arguments.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/join.html]
#[monoruby_builtin]
fn join(_: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg, len: usize) -> Result<Value> {
    Executor::check_number_of_arguments(len, 0..=1)?;
    let sep = if len == 0 {
        "".to_string()
    } else {
        arg[0].expect_string(globals)?
    };
    let self_ = lfp.self_val();
    let mut res = String::new();
    array_join(globals, &mut res, self_.as_array(), &sep);
    Ok(Value::new_string(res))
}

fn array_join(globals: &Globals, res: &mut String, aref: &ArrayInner, sep: &str) {
    for elem in &**aref {
        let s = globals.tos(*elem);
        if res.is_empty() {
            *res = s;
        } else {
            *res += sep;
            *res += &s;
        }
    }
}

///
/// ### Array#sum
///
/// - sum(init=0) -> object
/// - sum(init=0) {|e| expr } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/sum.html]
#[monoruby_builtin]
fn sum(vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg, len: usize) -> Result<Value> {
    Executor::check_number_of_arguments(len, 0..=1)?;
    let mut sum = if len == 0 { Value::int32(0) } else { arg[0] };
    let self_ = lfp.self_val();
    let iter = self_.as_array().iter().cloned();
    match lfp.block() {
        None => {
            for v in iter {
                sum =
                    executor::op::add_values(vm, globals, sum, v).ok_or_else(|| vm.take_error())?;
            }
        }
        Some(b) => {
            let data = vm.get_block_data(globals, b);
            for v in iter {
                let rhs = vm.invoke_block(globals, data.clone(), &[v])?;
                sum = executor::op::add_values(vm, globals, sum, rhs)
                    .ok_or_else(|| vm.take_error())?;
            }
        }
    }
    Ok(sum)
}

///
/// ### Array#each
///
/// - each {|item| .... } -> self
/// - [NOT SUPPORTED] each -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/each.html]
#[monoruby_builtin]
fn each(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let ary = lfp.self_val();
    let block_handler = lfp.expect_block()?;
    vm.invoke_block_iter1(globals, block_handler, ary.as_array().iter().cloned())?;
    Ok(lfp.self_val())
}

/// ### Array#map
///
/// - map {|item| ... } -> [object]
/// - [NOT SUPPORTED] map -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/collect.html]
#[monoruby_builtin]
fn map(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let ary = lfp.self_val();
    let iter = ary.as_array().iter().cloned();
    let block_handler = lfp.expect_block()?;
    let vec = vm.invoke_block_map1(globals, block_handler, iter)?;
    let res = Value::new_array_from_vec(vec);
    Ok(res)
}

///
/// #### Enumerable#detect
///
/// - find([NOT SUPPORTED]ifnone = nil) {|item| ... } -> object
/// - detect([NOT SUPPORTED]ifnone = nil) {|item| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/detect.html]
#[monoruby_builtin]
fn detect(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let ary = lfp.self_val();
    let bh = lfp.expect_block()?;
    let data = vm.get_block_data(globals, bh);
    for elem in ary.as_array().iter() {
        if vm.invoke_block(globals, data.clone(), &[*elem])?.as_bool() {
            return Ok(*elem);
        };
    }
    Ok(Value::nil())
}

///
/// #### Array#include?
///
/// - include?(val) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/include=3f.html]
#[monoruby_builtin]
fn include_(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    Executor::check_number_of_arguments(len, 1..=1)?;
    let ary = lfp.self_val();
    let rhs = arg[0];
    for lhs in ary.as_array().iter().cloned() {
        if vm.cmp_eq_values_bool(globals, lhs, rhs)? {
            return Ok(Value::bool(true));
        };
    }
    Ok(Value::bool(false))
}

///
/// #### Array#reverse
///
/// - reverse -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/reverse.html]
#[monoruby_builtin]
fn reverse(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    len: usize,
) -> Result<Value> {
    Executor::check_number_of_arguments(len, 0..=0)?;
    let ary: Vec<_> = lfp.self_val().as_array().iter().rev().cloned().collect();
    Ok(Value::new_array_from_vec(ary))
}

///
/// #### Array#reverse!
///
/// - reverse! -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/reverse.html]
#[monoruby_builtin]
fn reverse_(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    len: usize,
) -> Result<Value> {
    Executor::check_number_of_arguments(len, 0..=0)?;
    let mut self_val = lfp.self_val();
    self_val.as_array_mut().reverse();
    Ok(self_val)
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn array_new() {
        run_test_with_prelude(
            r##"
        a = A.new
        a << 4
        a[2] = 5
        a
        "##,
            r##"
        class A < Array
        end
        "##,
        );
    }

    #[test]
    fn size() {
        run_test2(r##"[].size"##);
        run_test2(r##"[].length"##);
        run_test2(r##"[1,2,3].size"##);
        run_test_with_prelude(
            r##"
        a = A.new
        a << 100
        a << 42
        a << 2
        a.size
        "##,
            r##"
        class A < Array
        end
        "##,
        );
    }

    #[test]
    fn add() {
        run_test(r##"[1,2,3] + [4]"##);
        run_test(r##"a = [1,2,3]; b = [4]; a + b; a"##);
        run_test(r##"a = [1,2,3]; b = [4]; a + b; b"##);
        run_test(r##"a = [1,2,3]; b = [4]; a.+(b); b"##);
    }

    #[test]
    fn shl() {
        run_test(r##"a = [1,2,3]; a << 10; a"##);
        run_test(r##"a = [1,2,3]; a.<<(10); a"##);
    }

    #[test]
    fn index() {
        run_test(
            r##"
        a = [1,2,3];
        a[2] = 42;
        a[4] = 99;
        a[-1] = 14;
        a
        "##,
        );
        run_test(
            r##"
        ary = [0, 1, 2, 3]
        ary[1, 2] = ["a", "b", "c", "d"]
        ary
        "##,
        );
        run_test(
            r##"
        ary = [0, 1, 2]
        ary[5, 1] = "Z"
        ary
        "##,
        );
        run_test(
            r##"
        ary = [0, 1, 2, 3]
        ary[0, 10] = ["a"]
        ary
        "##,
        );
        run_test_error(
            r##"
        ary = [0, 1, 2, 3]
        ary[-1, 10] = ["a"]
        ary
        "##,
        );
        run_test_error(
            r##"
        ary = [0, 1, 2, 3]
        ary[0, -1] = ["a"]
        ary
        "##,
        );
        run_test(
            r##"
        a = [1,2,3];
        a.[]=(2, 42);
        a.[]=(4,99);
        a.[]=(-2, 14);
        a
        "##,
        );
        run_test(
            r##"
        a = ["a","b","c","d","e"];
        [a[0..1], a[0...1], a[0..-1], a[-2..-1], a[-2..4], a[0..10], a[10..11], a[2..1], a[-1..-2], a[5..10]]
        "##,
        );
        run_test(
            r##"
        a = [ "a", "b", "c", "d", "e" ];
        [a.[](0), a.[](1), a.[](-1), a.[](-2), a.[](10)]
        "##,
        );
        run_test(
            r##"
        a = [ "a", "b", "c", "d", "e" ];
        [a.[](0..1), a.[](0...1), a.[](0..-1), a.[](-2..-1), a.[](-2..4), a.[](0..10), a.[](10..11), a.[](2..1), a.[](-1..-2), a.[](5..10)]
        "##,
        );
        run_test(
            r##"
        a = [ "a", "b", "c", "d", "e" ];
        [a[0, 1], a[-1, 1], a[0, 10], a[0, 0], a[0, -1], a[10, 1], a[5], a[5, 1], a[5..10]]
        "##,
        );
    }

    #[test]
    fn inject() {
        run_test(r##"[2, 3, 4, 5].inject(0) {|result, item| result + item }"##);
        run_test(r##"[2, 3, 4, 5].inject {|result, item| result + item }"##);
        run_test(r##"[2, 3, 4, 5].inject(5) {|result, item| result + item**2 }"##);
    }

    #[test]
    fn join() {
        run_test(r##"[2, 3, 4, 5].join"##);
        run_test(r##"[2, 3, 4, 5].join("-")"##);
    }

    #[test]
    fn sum() {
        run_test(
            r##"[[].sum, [].sum(0.0), [1, 2, 3].sum, [3, 5.5].sum, [2.5, 3.0].sum(0.0) {|e| e * e }, ["a", "b", "c"].sum("")]"##,
        );
        run_test_error("[Object.new].sum");
    }

    #[test]
    fn each() {
        run_test(
            r##"
        x = 100
        [2, 3, 4, 5].each do |y|
          x += y
        end
        x
        "##,
        );
    }

    #[test]
    fn map() {
        run_test(
            r##"
        x = 10
        [2, 3, 4, 5, 6, 7, 8].map do |y|
          x + y
        end
        "##,
        );
    }

    #[test]
    fn detect() {
        run_test(r#"[1, 2, 3, 4, 5].find {|i| i % 3 == 0 }"#);
        run_test(r#"[2, 2, 2, 2, 2].find {|i| i % 3 == 0 }"#);
    }

    #[test]
    fn include() {
        run_test_with_prelude(
            r#"
            [a.include?("b"), a.include?("z")]
        "#,
            r#"
            a = ["a","b","c"]
        "#,
        );
    }

    #[test]
    fn reverse() {
        run_test(
            r#"
            a = [1, 2, 3, 4, 5]
            [a.reverse, a]
        "#,
        );
        run_test(
            r#"
            a = [1, 2, 3, 4, 5]
            a.reverse!
            a
        "#,
        );
    }
}
