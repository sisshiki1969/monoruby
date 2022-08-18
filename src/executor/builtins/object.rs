use crate::*;

//
// Object class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(OBJECT_CLASS, "puts", puts, -1);
    globals.define_builtin_func(OBJECT_CLASS, "print", print, -1);
    globals.define_builtin_func(OBJECT_CLASS, "__assert", assert, 2);
    globals.define_builtin_func(OBJECT_CLASS, "__dump", dump, 0);
    globals.define_builtin_func(OBJECT_CLASS, "respond_to?", respond_to, 1);
    globals.define_builtin_func(OBJECT_CLASS, "inspect", inspect, 0);
    globals.define_builtin_func(OBJECT_CLASS, "class", class, 0);
    globals.define_builtin_func(OBJECT_CLASS, "singleton_class", singleton_class, 0);
}

/// ### Kernel#puts
/// - puts(*arg) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/puts.html]
extern "C" fn puts(_vm: &mut Interp, globals: &mut Globals, arg: Arg, len: usize) -> Option<Value> {
    for offset in 0..len {
        let mut bytes = arg[offset].to_bytes(globals);
        bytes.extend(b"\n");
        globals.write_stdout(&bytes);
    }
    globals.flush_stdout();
    Some(Value::nil())
}

/// ### Kernel#print
/// - print(*arg) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/print.html]
extern "C" fn print(
    _vm: &mut Interp,
    globals: &mut Globals,
    arg: Arg,
    len: usize,
) -> Option<Value> {
    for offset in 0..len {
        globals.write_stdout(&arg[offset].to_bytes(globals));
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
    assert!(Value::eq(expected, actual));
    Some(Value::nil())
}

use std::arch::asm;

extern "C" fn dump(
    vm: &mut Interp,
    globals: &mut Globals,
    _arg: Arg,
    _len: usize,
) -> Option<Value> {
    let mut bp: u64;
    unsafe {
        asm!(
            "mov {bp}, rbp",
            bp = out(reg) bp,
        );
    }
    super::op::_dump_stacktrace(vm, globals, bp as *const u64);
    Some(Value::nil())
}

/// ### Object#respond_to?
/// - respond_to?(name, include_all = false) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/respond_to=3f.html]
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

/// ### Object#inspect
/// - inspect -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/inspect.html]
extern "C" fn inspect(
    _vm: &mut Interp,
    globals: &mut Globals,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let s = arg.self_value().inspect(globals);
    Some(Value::new_string(s.into_bytes()))
}

/// ### Object#class
/// - class -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/class.html]
extern "C" fn class(
    _vm: &mut Interp,
    globals: &mut Globals,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    Some(arg.self_value().get_real_class_obj(globals))
}

/// ### Object#singleton_class
/// - singleton_class -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/singleton_class.html]
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
        run_test(":sym.class.to_s");
        run_test("5.class.to_s");
        run_test("5.7.class.to_s");
        run_test("'windows'.class.to_s");
        run_test("puts 100");
        run_test("print '100'");
        run_test("nil.respond_to?(:foo)");
        run_test("nil.inspect");
        run_test("Time.singleton_class.to_s");
        run_test(r#"File.write("/tmp/foo", "woo")"#);
    }
}
