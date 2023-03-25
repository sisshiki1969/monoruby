use crate::{executor::op::add_values, *};

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
    globals.define_builtin_func(ARRAY_CLASS, "[]=", index_assign, 2);
    globals.define_builtin_func(ARRAY_CLASS, "inject", inject, -1);
    globals.define_builtin_func(ARRAY_CLASS, "reduce", inject, -1);
    globals.define_builtin_func(ARRAY_CLASS, "join", join, -1);
    globals.define_builtin_func(ARRAY_CLASS, "sum", sum, -1);
    globals.define_builtin_func(ARRAY_CLASS, "each", each, 0);
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
extern "C" fn new(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Option<Value> {
    let class = lfp.self_val().as_class().class_id();
    let obj = Value::new_array_with_class(vec![], class);
    vm.invoke_method2_if_exists(globals, IdentId::INITIALIZE, obj, arg, len)?;
    Some(obj)
}

///
/// ### Array#length
///
/// - length -> Integer
/// - size -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/length.html]
extern "C" fn size(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Option<Value> {
    let len = lfp.self_val().as_array().len();
    Some(Value::new_integer(len as i64))
}

///
/// ### Array#+
///
/// - self + other -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=2b.html]
extern "C" fn add(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let mut lhs = lfp.self_val().as_array().clone();
    let rhs = match arg[0].is_array() {
        Some(v) => v,
        None => {
            globals.err_no_implicit_conversion(arg[0], ARRAY_CLASS);
            return None;
        }
    };
    lhs.extend_from_slice(rhs);
    Some(Value::new_array(lhs))
}

///
/// ### Array#<<
///
/// - self << obj -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=3c=3c.html]
extern "C" fn shl(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let mut self_ = lfp.self_val();
    self_.as_array_mut().push(arg[0]);
    Some(self_)
}

///
/// ### Array#[]
///
/// - self[nth] -> object | nil
/// - self[range] -> Array | nil
/// - self[start, length] -> Array | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=5b=5d.html]
extern "C" fn index(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Option<Value> {
    globals.check_number_of_arguments(len, 1..=2)?;
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
extern "C" fn index_assign(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let i = arg[0];
    let val = arg[1];
    if let Some(idx) = i.try_fixnum() {
        return lfp.self_val().as_array_mut().set_index(globals, idx, val);
    } else {
        unimplemented!()
    }
}

///
/// ### Array#inject
///
/// - inject(init = self.first) {|result, item| ... } -> object
/// - reduce(init = self.first) {|result, item| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/inject.html]
extern "C" fn inject(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Option<Value> {
    let bh = match lfp.block() {
        Some(bh) => bh,
        None => {
            globals.err_no_block_given();
            return None;
        }
    };
    globals.check_number_of_arguments(len, 0..=1)?;
    let self_ = lfp.self_val();
    let mut iter = self_.as_array().iter();
    let mut res = if len == 0 {
        iter.next().cloned().unwrap_or_default()
    } else {
        arg[0]
    };
    let data = vm.get_block_data(globals, bh);
    for elem in iter {
        res = vm.invoke_block(globals, data.clone(), &[res, *elem])?;
    }
    Some(res)
}

///
/// ### Array#join
///
/// - join(sep = $,) -> String
/// TODO: support recursive join for Array class arguments.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/join.html]
extern "C" fn join(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Option<Value> {
    globals.check_number_of_arguments(len, 0..=1)?;
    let sep = if len == 0 {
        "".to_string()
    } else {
        arg[0].expect_string(globals)?
    };
    let self_ = lfp.self_val();
    let mut res = String::new();
    array_join(globals, &mut res, self_.as_array(), &sep);
    Some(Value::new_string(res))
}

fn array_join(globals: &Globals, res: &mut String, aref: &ArrayInner, sep: &str) {
    for elem in &**aref {
        let s = elem.to_s(globals);
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
extern "C" fn sum(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Option<Value> {
    globals.check_number_of_arguments(len, 0..=1)?;
    let mut sum = if len == 0 { Value::int32(0) } else { arg[0] };
    let self_ = lfp.self_val();
    let aref = self_.as_array();
    match lfp.block() {
        None => {
            for v in &**aref {
                sum = add_values(vm, globals, sum, *v)?;
            }
        }
        Some(bh) => {
            let data = vm.get_block_data(globals, bh);
            for v in &**aref {
                let rhs = vm.invoke_block(globals, data.clone(), &[*v])?;
                sum = add_values(vm, globals, sum, rhs)?;
            }
        }
    }
    Some(sum)
}

///
/// ### Array#each
///
/// - each {|item| .... } -> self
/// - [NOT SUPPORTED] each -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/each.html]
extern "C" fn each(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Option<Value> {
    let block_handler = if let Some(block) = lfp.block() {
        block
    } else {
        globals.err_no_block_given();
        return None;
    };
    let data = vm.get_block_data(globals, block_handler);
    for i in &**lfp.self_val().as_array() {
        vm.invoke_block(globals, data.clone(), &[*i])?;
    }
    Some(lfp.self_val())
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn test_array_new() {
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
    fn test_array_size() {
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
    fn test_array_add() {
        run_test(r##"[1,2,3] + [4]"##);
        run_test(r##"a = [1,2,3]; b = [4]; a + b; a"##);
        run_test(r##"a = [1,2,3]; b = [4]; a + b; b"##);
        run_test(r##"a = [1,2,3]; b = [4]; a.+(b); b"##);
    }

    #[test]
    fn test_array_shl() {
        run_test(r##"a = [1,2,3]; a << 10; a"##);
        run_test(r##"a = [1,2,3]; a.<<(10); a"##);
    }

    #[test]
    fn test_array_index() {
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
        a = [1,2,3];
        a.[]=(2, 42);
        a.[]=(4,99);
        a.[]=(-2, 14);
        a
        "##,
        );
        run_test(
            r##"
        a = [ "a", "b", "c", "d", "e" ];
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
}
