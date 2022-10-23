use crate::*;

//
// Object class
//

pub(super) fn init(globals: &mut Globals) {
    //globals.define_builtin_singleton_func(OBJECT_CLASS, "new", new, 0);
    globals.define_builtin_func(OBJECT_CLASS, "puts", puts, -1);
    globals.define_builtin_func(OBJECT_CLASS, "print", print, -1);
    globals.define_builtin_func(OBJECT_CLASS, "__assert", assert, 2);
    globals.define_builtin_func(OBJECT_CLASS, "__dump", dump, 0);
    globals.define_builtin_func(OBJECT_CLASS, "respond_to?", respond_to, 1);
    globals.define_builtin_func(OBJECT_CLASS, "inspect", inspect, 0);
    globals.define_builtin_func(OBJECT_CLASS, "class", class, 0);
    globals.define_builtin_func(OBJECT_CLASS, "singleton_class", singleton_class, 0);
    globals.define_builtin_func(
        OBJECT_CLASS,
        "instance_variable_defined?",
        instance_variable_defined,
        1,
    );
    globals.define_builtin_func(
        OBJECT_CLASS,
        "instance_variable_set",
        instance_variable_set,
        2,
    );
    globals.define_builtin_func(
        OBJECT_CLASS,
        "instance_variable_get",
        instance_variable_get,
        1,
    );
}

/// ### Object.new
/// - new -> Object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/s/new.html]
/*extern "C" fn new(
    vm: &mut Interp,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
) -> Option<Value> {
    let class = self_val.as_class();
    let obj = Value::new_object(class);
    if let Some(func_id) = globals.find_method(obj, IdentId::INITIALIZE) {
        globals.check_arg(func_id, len)?;
        vm.invoke_func2(globals, func_id, obj, arg, len)?;
    };
    Some(obj)
}*/

/// ### Kernel#puts
/// - puts(*arg) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/puts.html]
extern "C" fn puts(
    _vm: &mut Interp,
    globals: &mut Globals,
    _: Value,
    arg: Arg,
    len: usize,
) -> Option<Value> {
    fn decompose(collector: &mut Vec<Value>, val: Value) {
        match val.is_array() {
            Some(ary) => {
                ary.iter().for_each(|v| decompose(collector, *v));
            }
            None => collector.push(val),
        }
    }
    let mut collector = Vec::new();
    for i in 0..len {
        decompose(&mut collector, arg[i]);
    }

    for v in collector {
        let mut bytes = v.to_bytes(globals);
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
    _: Value,
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
    _: Value,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let expected = arg[0];
    let actual = arg[1];
    eprintln!("assert {:?} {:?}", expected, actual);
    assert!(Value::eq(expected, actual));
    Some(Value::nil())
}

use std::arch::asm;

extern "C" fn dump(
    vm: &mut Interp,
    globals: &mut Globals,
    _: Value,
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
    self_val: Value,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let name = match arg[0].unpack() {
        RV::Symbol(id) => id,
        RV::String(b) => IdentId::get_ident_id_from_string(String::from_utf8_lossy(b).into_owned()),
        _ => unimplemented!(),
    };
    Some(Value::bool(globals.find_method(self_val, name).is_some()))
}

/// ### Object#inspect
/// - inspect -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/inspect.html]
extern "C" fn inspect(
    _vm: &mut Interp,
    globals: &mut Globals,
    self_val: Value,
    _: Arg,
    _len: usize,
) -> Option<Value> {
    let s = self_val.inspect(globals);
    Some(Value::new_string(s.into_bytes()))
}

/// ### Object#class
/// - class -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/class.html]
extern "C" fn class(
    _vm: &mut Interp,
    globals: &mut Globals,
    self_val: Value,
    _: Arg,
    _len: usize,
) -> Option<Value> {
    Some(self_val.get_real_class_id(globals).get_obj(globals))
}

/// ### Object#singleton_class
/// - singleton_class -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/singleton_class.html]
extern "C" fn singleton_class(
    _vm: &mut Interp,
    globals: &mut Globals,
    self_val: Value,
    _: Arg,
    _len: usize,
) -> Option<Value> {
    Some(self_val.get_singleton(globals))
}

/// ### Object#instance_variable_defined?
/// - instance_variable_defined?(var) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_variable_defined=3f.html]
extern "C" fn instance_variable_defined(
    _vm: &mut Interp,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let id = match arg[0].unpack() {
        RV::Symbol(sym) => sym,
        RV::String(s) => IdentId::get_ident_id_from_string(String::from_utf8_lossy(s).into_owned()),
        _ => return None,
    };
    let b = globals.get_ivar(self_val, id).is_some();
    Some(Value::bool(b))
}

/// ### Object#instance_variable_set
/// - instance_variable_set(var, value) -> Object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_variable_set.html]
extern "C" fn instance_variable_set(
    _vm: &mut Interp,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let id = arg[0].expect_symbol_or_string(globals)?;
    let val = arg[1];
    globals.set_ivar(self_val, id, val)?;
    Some(val)
}

/// ### Object#instance_variable_get
/// - instance_variable_get(var) -> Object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_variable_get.html]
extern "C" fn instance_variable_get(
    _vm: &mut Interp,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let id = arg[0].expect_symbol_or_string(globals)?;
    let v = globals.get_ivar(self_val, id).unwrap_or_default();
    Some(v)
}

#[cfg(test)]
mod test {
    use super::tests::*;

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

    #[test]
    fn test_object() {
        run_test(r#"a=Object.new; a.instance_variable_set("@i", 42)"#);
        run_test(r#"a=Object.new; a.instance_variable_get(:@i)"#);
        run_test(
            r#"a=Object.new; a.instance_variable_set("@i", 42); a.instance_variable_defined?(:@i)"#,
        );
        run_test(
            r#"a=Object.new; a.instance_variable_set("@i", 42); a.instance_variable_get(:@i)"#,
        );
    }
}
