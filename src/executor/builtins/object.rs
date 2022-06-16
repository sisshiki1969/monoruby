use crate::*;

//
// Object class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(OBJECT_CLASS, "puts", puts, 1);
    globals.define_builtin_func(OBJECT_CLASS, "print", print, 1);
    globals.define_builtin_func(OBJECT_CLASS, "assert", assert, 2);
    globals.define_builtin_func(OBJECT_CLASS, "respond_to?", respond_to, 1);
    globals.define_builtin_func(OBJECT_CLASS, "inspect", inspect, 0);
    globals.define_builtin_func(OBJECT_CLASS, "write", write, 2);
    globals.define_builtin_func(OBJECT_CLASS, "class", class, 0);
    globals.define_builtin_func(OBJECT_CLASS, "singleton_class", singleton_class, 0);
}

extern "C" fn puts(_vm: &mut Interp, globals: &mut Globals, arg: Arg, len: usize) -> Value {
    for offset in 0..len {
        println!("{}", globals.val_tos(arg[offset]));
    }
    Value::nil()
}

extern "C" fn print(_vm: &mut Interp, globals: &mut Globals, arg: Arg, len: usize) -> Value {
    for offset in 0..len {
        match arg[offset].unpack() {
            RV::String(bytes) => {
                globals.stdout.write(bytes).unwrap();
            }
            _ => {
                globals
                    .stdout
                    .write(&globals.val_tos(arg[offset]).into_bytes())
                    .unwrap();
            }
        };
    }
    Value::nil()
}

extern "C" fn assert(_vm: &mut Interp, _globals: &mut Globals, arg: Arg, _len: usize) -> Value {
    let expected = arg[0];
    let actual = arg[1];
    assert_eq!(expected, actual);
    Value::nil()
}

extern "C" fn respond_to(_vm: &mut Interp, globals: &mut Globals, arg: Arg, _len: usize) -> Value {
    let class_id = arg.self_value().class_id();
    let name = match arg[0].unpack() {
        RV::Symbol(id) => id,
        RV::String(b) => globals.get_ident_id(String::from_utf8_lossy(b).as_ref()),
        _ => unimplemented!(),
    };
    Value::bool(globals.get_method_inner(class_id, name).is_some())
}

extern "C" fn inspect(_vm: &mut Interp, globals: &mut Globals, arg: Arg, _len: usize) -> Value {
    let s = globals.val_tos(arg.self_value());
    Value::new_string(s.into_bytes())
}

extern "C" fn write(_vm: &mut Interp, globals: &mut Globals, arg: Arg, _len: usize) -> Value {
    let name = match arg[0].unpack() {
        RV::String(bytes) => String::from_utf8(bytes.clone()).unwrap(),
        _ => unimplemented!("{}", globals.val_tos(arg[0])),
    };
    let mut file = File::create(name).unwrap();
    let bytes = globals.val_tos(arg[1]).into_bytes();
    file.write_all(&bytes).unwrap();
    Value::new_integer(bytes.len() as i64)
}

extern "C" fn class(_vm: &mut Interp, globals: &mut Globals, arg: Arg, _len: usize) -> Value {
    globals.get_real_class_obj(arg.self_value())
}

extern "C" fn singleton_class(
    _vm: &mut Interp,
    globals: &mut Globals,
    arg: Arg,
    _len: usize,
) -> Value {
    globals.get_singleton(arg.self_value())
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
