use crate::*;

//
// Array class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_singleton_func(ARRAY_CLASS, "new", new, -1);
    globals.define_builtin_func(ARRAY_CLASS, "size", size, 0);
    globals.define_builtin_func(ARRAY_CLASS, "length", size, 0);
    globals.define_builtin_func(ARRAY_CLASS, "+", add, 1);
    globals.define_builtin_func(ARRAY_CLASS, "<<", shl, 1);
    globals.define_builtin_func(ARRAY_CLASS, "[]=", index_assign, 2);
}

/// ### Array.new
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
    self_val: Value,
    arg: Arg,
    len: usize,
    _block: Option<Value>,
) -> Option<Value> {
    let class = self_val.as_class();
    let obj = Value::new_array_with_class(vec![], class);
    vm.invoke_method2_if_exists(globals, IdentId::INITIALIZE, obj, arg, len)?;
    Some(obj)
}

/// ### Array#length
/// - length -> Integer
/// - size -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/length.html]
extern "C" fn size(
    _vm: &mut Executor,
    _globals: &mut Globals,
    mut self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<Value>,
) -> Option<Value> {
    let len = self_val.as_array_mut().len();
    Some(Value::new_integer(len as i64))
}

/// ### Array#+
/// - self + other -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=2b.html]
extern "C" fn add(
    _vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<Value>,
) -> Option<Value> {
    let mut lhs = self_val.as_array().clone();
    let rhs = match arg[0].is_array() {
        Some(v) => v,
        None => {
            globals.err_no_implict_conv(arg[0], ARRAY_CLASS);
            return None;
        }
    };
    lhs.extend_from_slice(rhs);
    Some(Value::new_array(lhs))
}

/// ### Array#<<
/// - self << obj -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=3c=3c.html]
extern "C" fn shl(
    _vm: &mut Executor,
    _globals: &mut Globals,
    mut self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<Value>,
) -> Option<Value> {
    self_val.as_array_mut().push(arg[0]);
    Some(self_val)
}

/// ### Array#[]=
/// - self[nth] = val
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=5b=5d=3d.html]
extern "C" fn index_assign(
    _vm: &mut Executor,
    globals: &mut Globals,
    mut self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<Value>,
) -> Option<Value> {
    let i = arg[0];
    let val = arg[1];
    if let Some(idx) = i.try_fixnum() {
        return self_val.as_array_mut().set_index(globals, idx, val);
    } else {
        unimplemented!()
    }
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
    }
}
