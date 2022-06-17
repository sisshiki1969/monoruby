use crate::*;

//
// Object class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(OBJECT_CLASS, "puts", puts, -1);
    globals.define_builtin_func(OBJECT_CLASS, "print", print, -1);
    globals.define_builtin_func(OBJECT_CLASS, "assert", assert, 2);
    globals.define_builtin_func(OBJECT_CLASS, "respond_to?", respond_to, 1);
    globals.define_builtin_func(OBJECT_CLASS, "inspect", inspect, 0);
    globals.define_builtin_func(OBJECT_CLASS, "class", class, 0);
    globals.define_builtin_func(OBJECT_CLASS, "singleton_class", singleton_class, 0);
}

/// Kernel#puts
/// - puts(*arg) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/class/Kernel.html#M_PUTS]
extern "C" fn puts(_vm: &mut Interp, globals: &mut Globals, arg: Arg, len: usize) -> Option<Value> {
    for offset in 0..len {
        globals
            .stdout
            .write(&arg[offset].to_s(globals).into_bytes())
            .unwrap();
        globals.stdout.write(b"\n").unwrap();
    }
    Some(Value::nil())
}

/// Kernel#print
/// - print(*arg) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/class/Kernel.html#M_PRINT]
extern "C" fn print(
    _vm: &mut Interp,
    globals: &mut Globals,
    arg: Arg,
    len: usize,
) -> Option<Value> {
    for offset in 0..len {
        globals
            .stdout
            .write(&arg[offset].to_s(globals).into_bytes())
            .unwrap();
    }
    Some(Value::nil())
}

extern "C" fn assert(
    _vm: &mut Interp,
    _globals: &mut Globals,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let expected = arg[0];
    let actual = arg[1];
    assert_eq!(expected, actual);
    Some(Value::nil())
}

/// Object#respond_to?
/// - respond_to?(name, include_all = false) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/class/Object.html#I_RESPOND_TO--3F]
extern "C" fn respond_to(
    _vm: &mut Interp,
    globals: &mut Globals,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let class_id = arg.self_value().class_id();
    let name = match arg[0].unpack() {
        RV::Symbol(id) => id,
        RV::String(b) => globals.get_ident_id(String::from_utf8_lossy(b).as_ref()),
        _ => unimplemented!(),
    };
    Some(Value::bool(
        globals.get_method_inner(class_id, name).is_some(),
    ))
}

/// Object#inspect
/// - inspect -> String
///
/// [https://docs.ruby-lang.org/ja/latest/class/Object.html#I_INSPECT]
extern "C" fn inspect(
    _vm: &mut Interp,
    globals: &mut Globals,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let s = arg.self_value().inspect(globals);
    Some(Value::new_string(s.into_bytes()))
}

extern "C" fn class(
    _vm: &mut Interp,
    globals: &mut Globals,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    Some(arg.self_value().get_real_class_obj(globals))
}

extern "C" fn singleton_class(
    _vm: &mut Interp,
    globals: &mut Globals,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    Some(arg.self_value().get_singleton(globals))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_builtin() {
        run_test(":sym.class");
        run_test("5.class");
        run_test("5.7.class");
        run_test("'windows'.class");
        run_test("puts 100");
        run_test("print '100'");
        run_test("nil.respond_to?(:foo)");
        run_test("nil.inspect");
        run_test("puts Time.singleton_class");
        run_test(r#"File.write("/tmp/foo", "woo")"#);
    }
}
