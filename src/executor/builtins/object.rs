use std::io::Write;

use num::Zero;

use crate::*;

//
// Object class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_singleton_func(OBJECT_CLASS, "new", object_new, -1);
    globals.define_builtin_func(OBJECT_CLASS, "inspect", inspect, 0);
    globals.define_builtin_func(OBJECT_CLASS, "p", p, -1);
    globals.define_builtin_func(OBJECT_CLASS, "class", class, 0);
    globals.define_builtin_func(OBJECT_CLASS, "singleton_class", singleton_class, 0);
    globals.define_builtin_func(OBJECT_CLASS, "respond_to?", respond_to, 1);
    globals.define_builtin_func(OBJECT_CLASS, "instance_of?", instance_of, 1);
    globals.define_builtin_func(OBJECT_CLASS, "is_a?", is_a, 1);
    globals.define_builtin_func(OBJECT_CLASS, "kind_of?", is_a, 1);
    globals.define_builtin_func(OBJECT_CLASS, "dup", dup, 0);
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
    globals.define_builtin_func(OBJECT_CLASS, "puts", puts, -1);
    globals.define_builtin_func(OBJECT_CLASS, "print", print, -1);
    globals.define_builtin_func(OBJECT_CLASS, "to_s", to_s, 0);
    globals.define_builtin_func(OBJECT_CLASS, "rand", rand, -1);
    globals.define_builtin_func(OBJECT_CLASS, "Integer", kernel_integer, 1);
    globals.define_builtin_func(OBJECT_CLASS, "require", require, 1);
    globals.define_builtin_func(OBJECT_CLASS, "require_relative", require_relative, 1);
    globals.define_builtin_func(OBJECT_CLASS, "system", system, 1);
    globals.define_builtin_func(OBJECT_CLASS, "`", command, 1);
    globals.define_builtin_func(OBJECT_CLASS, "__assert", assert, 2);
    globals.define_builtin_func(OBJECT_CLASS, "__dump", dump, 0);
}

///
/// ### Object.new
///
/// - new -> Object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/s/new.html]
extern "C" fn object_new(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let class = self_val.as_class();
    let obj = Value::new_object(class.class_id());
    if let Some(func_id) = globals.check_method(obj, IdentId::INITIALIZE) {
        vm.invoke_func2(globals, func_id, obj, arg, len)?;
    };
    Some(obj)
}

///
/// ### Kernel.#puts
///
/// - puts(*arg) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/puts.html]
extern "C" fn is_a(
    _vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let class = arg[0].expect_class_or_module(globals)?;
    Some(Value::bool(self_val.is_kind_of(globals, class)))
}

///
/// ### Object#dup
///
/// - dup -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/clone.html]
extern "C" fn dup(
    _vm: &mut Executor,
    _globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    Some(self_val.dup())
}

///
/// ### Kernel.#puts
///
/// - puts(*arg) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/puts.html]
extern "C" fn puts(
    _vm: &mut Executor,
    globals: &mut Globals,
    _: Value,
    arg: Arg,
    len: usize,
    _: Option<BlockHandler>,
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

///
/// ### Kernel.#print
///
/// - print(*arg) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/print.html]
extern "C" fn print(
    _vm: &mut Executor,
    globals: &mut Globals,
    _: Value,
    arg: Arg,
    len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    for i in 0..len {
        globals.write_stdout(&arg[i].to_bytes(globals));
    }
    Some(Value::nil())
}

///
/// ### Object#to_s
///
/// - to_s -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/to_s.html]
extern "C" fn to_s(
    _vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    _: Arg,
    _: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let s = self_val.to_s(globals);
    Some(Value::new_string(s))
}

///
/// ### Kernel.#p
///
/// - p(*arg) -> object | Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/p.html]
extern "C" fn p(
    _vm: &mut Executor,
    globals: &mut Globals,
    _: Value,
    arg: Arg,
    len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let mut buf = String::new();
    for i in 0..len {
        buf += &arg[i].inspect(globals);
        buf += "\n";
    }
    globals.write_stdout(buf.as_bytes());
    Some(match len {
        0 => Value::nil(),
        1 => arg[0],
        _ => {
            let mut ary = vec![];
            for i in 0..len {
                ary.push(arg[i]);
            }
            Value::new_array_from_vec(ary)
        }
    })
}

extern "C" fn assert(
    _vm: &mut Executor,
    globals: &mut Globals,
    _: Value,
    arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let expected = arg[0];
    let actual = arg[1];
    eprintln!(
        "expected:{} actual:{}",
        expected.inspect(globals),
        actual.inspect(globals)
    );
    eprintln!("expected:{:?} actual:{:?}", expected, actual);
    assert!(Value::eq(expected, actual));
    Some(Value::nil())
}

extern "C" fn dump(
    vm: &mut Executor,
    globals: &mut Globals,
    _: Value,
    _arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    unsafe {
        crate::executor::compiler::runtime::_dump_stacktrace(vm, globals);
    }
    Some(Value::nil())
}

///
/// ### Object#respond_to?
///
/// - respond_to?(name, include_all = false) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/respond_to=3f.html]
extern "C" fn respond_to(
    _vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let name = match arg[0].unpack() {
        RV::Symbol(id) => id,
        RV::String(b) => IdentId::get_ident_id_from_string(String::from_utf8_lossy(b).into_owned()),
        _ => unimplemented!(),
    };
    Some(Value::bool(globals.check_method(self_val, name).is_some()))
}

///
/// ### Object#inspect
///
/// - inspect -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/inspect.html]
extern "C" fn inspect(
    _vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    _: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let s = self_val.inspect(globals);
    Some(Value::new_string(s))
}

///
/// ### Object#class
///
/// - class -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/class.html]
extern "C" fn class(
    _vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    _: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    Some(self_val.real_class(globals).as_val())
}

///
/// ### Object#instance_of?
///
/// - instance_of?(klass) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_of=3f.html]
extern "C" fn instance_of(
    _vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let b = self_val.real_class(globals).class_id() == arg[0].expect_class_or_module(globals)?;
    Some(Value::bool(b))
}

///
/// ### Kernel.#rand
///
/// - rand(max = 0) -> Integer | Float
/// - rand(range) -> Integer | Float | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/rand.html]
extern "C" fn rand(
    _vm: &mut Executor,
    globals: &mut Globals,
    _: Value,
    arg: Arg,
    len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    globals.check_number_of_arguments(len, 0..=1)?;
    let i = match len {
        0 => 0i64,
        1 => arg[0].coerce_to_fixnum(globals)?,
        _ => unreachable!(),
    };
    if !i.is_zero() {
        Some(Value::new_integer(
            (rand::random::<f64>() * (i.abs() as f64)) as i64,
        ))
    } else {
        Some(Value::new_float(rand::random()))
    }
}

///
/// ### Object#singleton_class
///
/// - singleton_class -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/singleton_class.html]
extern "C" fn singleton_class(
    _vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    _: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    Some(self_val.get_singleton(globals))
}

///
/// ### Object#instance_variable_defined?
///
/// - instance_variable_defined?(var) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_variable_defined=3f.html]
extern "C" fn instance_variable_defined(
    _vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let id = match arg[0].unpack() {
        RV::Symbol(sym) => sym,
        RV::String(s) => IdentId::get_ident_id_from_string(String::from_utf8_lossy(s).into_owned()),
        _ => return None,
    };
    let b = globals.get_ivar(self_val, id).is_some();
    Some(Value::bool(b))
}

///
/// ### Object#instance_variable_set
///
/// - instance_variable_set(var, value) -> Object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_variable_set.html]
extern "C" fn instance_variable_set(
    _vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let id = arg[0].expect_symbol_or_string(globals)?;
    let val = arg[1];
    globals.set_ivar(self_val, id, val)?;
    Some(val)
}

///
/// ### Object#instance_variable_get
///
/// - instance_variable_get(var) -> Object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_variable_get.html]
extern "C" fn instance_variable_get(
    _vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let id = arg[0].expect_symbol_or_string(globals)?;
    let v = globals.get_ivar(self_val, id).unwrap_or_default();
    Some(v)
}

///
/// ### Kernel.#Integer
///
/// - Integer(arg, base = 0, exception: true) -> Integer | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/Integer.html]
extern "C" fn kernel_integer(
    _vm: &mut Executor,
    globals: &mut Globals,
    _: Value,
    arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let arg0 = arg[0];
    match arg0.unpack() {
        RV::Integer(num) => return Some(Value::new_integer(num)),
        RV::BigInt(num) => return Some(Value::new_bigint(num.clone())),
        RV::Float(num) => return Some(Value::new_integer(num.trunc() as i64)),
        RV::String(b) => {
            if let Ok(s) = String::from_utf8(b.to_vec()) {
                match s.parse::<i64>() {
                    Ok(num) => return Some(Value::new_integer(num)),
                    Err(_) => {
                        let s = arg0.to_s(globals);
                        globals.err_argument(&format!("invalid value for Integer(): {}", s));
                        return None;
                    }
                }
            }
        }
        _ => {}
    };
    globals.err_no_implict_conv(arg0, INTEGER_CLASS);
    None
}

///
/// ### Kernel.#require
///
/// - require(feature) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/require.html]
extern "C" fn require(
    executor: &mut Executor,
    globals: &mut Globals,
    _: Value,
    arg: Arg,
    _: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let feature = arg[0].expect_string(globals)?;
    let path = std::path::Path::new(&feature);
    let (file_body, path) = globals.load_lib(path)?;
    executor.eval_script(globals, file_body, &path)?;
    Some(Value::bool(true))
}

///
/// ### Kernel.#require_relative
///
/// - require_relative(relative_feature) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/require_relative.html]
extern "C" fn require_relative(
    executor: &mut Executor,
    globals: &mut Globals,
    _: Value,
    arg: Arg,
    _: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let mut path = globals.current_source_path(executor);
    path.pop();
    let feature = std::path::PathBuf::from(arg[0].expect_string(globals)?);
    path.extend(&feature);
    let (file_body, path) = globals.load_lib(&path)?;
    executor.eval_script(globals, file_body, &path)?;
    Some(Value::bool(true))
}

fn prepare_command_arg(input: String) -> (String, Vec<String>) {
    let mut args = vec![];
    let include_meta = input.contains(&[
        '*', '?', '{', '}', '[', ']', '<', '>', '(', ')', '~', '&', '|', '\\', '$', ';', '\'',
        '\"', '`', '\n',
    ]);
    let program = if include_meta {
        args.push(if cfg!(windows) { "/C" } else { "-c" }.to_string());
        args.push(input);
        if cfg!(windows) {
            "cmd"
        } else {
            "sh"
        }
    } else {
        let input: Vec<&str> = input.split(' ').collect();
        let arg = input[1..].concat();
        if !arg.is_empty() {
            args.push(arg)
        };
        input[0]
    }
    .to_string();
    (program, args)
}

///
/// ### Kernel.#system
///
/// - system(command, options={}) -> bool | nil
/// - [NOT SUPPORTED] system(env, command, options={}) -> bool | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/system.html]
extern "C" fn system(
    _executor: &mut Executor,
    _globals: &mut Globals,
    _: Value,
    arg: Arg,
    _: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    use std::process::Command;
    let input = arg[0].as_string();
    let (program, args) = prepare_command_arg(input);
    Some(match Command::new(program).args(&args).status() {
        Ok(status) => Value::bool(status.success()),
        Err(_) => Value::nil(),
    })
}

///
/// Kernel.#`
///
/// - `command` -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/=60.html]
extern "C" fn command(
    _executor: &mut Executor,
    globals: &mut Globals,
    _: Value,
    arg: Arg,
    _: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    use std::process::Command;
    let input = arg[0].as_string();
    let (program, args) = prepare_command_arg(input);
    match Command::new(program).args(&args).output() {
        Ok(output) => {
            std::io::stderr().write_all(&output.stderr).unwrap();
            Some(Value::new_string_from_slice(&output.stdout))
        }
        Err(err) => {
            globals.err_runtime(format!("{}", err));
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn test_builtin() {
        run_test2(":sym.class.to_s");
        run_test2("5.class.to_s");
        run_test2("5.7.class.to_s");
        run_test2("'windows'.class.to_s");
        run_test2("puts 100");
        run_test2("print '100'");
        run_test2("p");
        run_test2("p 1");
        run_test2("p 1,2");
        run_test2("p 1,2,3");
        run_test2("nil.respond_to?(:foo)");
        run_test2("nil.inspect");
        run_test2("Time.singleton_class.to_s");
        run_test2(r#"File.write("/tmp/foo", "woo")"#);
    }

    #[test]
    fn test_object() {
        run_test2(r#"a=Object.new; a.instance_variable_set("@i", 42)"#);
        run_test2(r#"a=Object.new; a.instance_variable_get(:@i)"#);
        run_test2(
            r#"a=Object.new; a.instance_variable_set("@i", 42); a.instance_variable_defined?(:@i)"#,
        );
        run_test2(
            r#"a=Object.new; a.instance_variable_set("@i", 42); a.instance_variable_get(:@i)"#,
        );
        run_test2(
            r#"
            20000.times do
                a=Object.new;
                a.instance_variable_set("@a", 0);
                a.instance_variable_set("@b", 1);
                a.instance_variable_set("@c", 2);
                a.instance_variable_set("@d", 3);
                a.instance_variable_set("@e", 4);
                a.instance_variable_set("@f", 5);
                a.instance_variable_set("@g", 6);
                a.instance_variable_set("@h", 7);
                a.instance_variable_set("@i", 8);
                a.instance_variable_set("@j", 9);
                a.instance_variable_set("@k", 10);
                a.instance_variable_set("@l", 11);
                a.inspect;
            end
            "#,
        );
    }

    #[test]
    fn kernel_integer() {
        run_test2(r#"Integer(-2435)"#);
        run_test2(r#"Integer("2435")"#);
        run_test_error(r#"Integer("2435.78")"#);
        run_test_error(r#"Integer([4])"#);
        run_test2(r#"Integer(-2435766756886769978978435)"#);
        run_test2(r#"Integer(2435.4556787)"#);
    }

    #[test]
    fn object_instance_of() {
        run_test2(r#"5.instance_of?(Integer)"#);
        run_test2(r#"5.instance_of?(Float)"#);
        run_test2(r#":ruby.instance_of?(Symbol)"#);
        run_test2(r#":ruby.instance_of?(Object)"#);
        run_test2(
            r#"
        class C < Object
        end
        class S < C
        end

        obj = S.new
        [obj.instance_of?(S), obj.instance_of?(C)]
        "#,
        );
        run_test_error(r#"5.instance_of?(7)"#);
    }

    #[test]
    fn object_isa() {
        run_test("4.is_a? Integer");
        run_test("4.5.is_a? Integer");
        run_test("'Ruby'.is_a? Integer");
        run_test("4.5.is_a? Float");
        run_test("'Ruby'.is_a? Float");
        run_test(
            r#"
        class C
        end
        C.new.is_a? C"#,
        );
        run_test(
            r#"
        class S
        end
        class C < S
        end
        c = C.new
        [c.is_a?(S), c.is_a?(C)]"#,
        );
    }

    #[test]
    fn kernel_system() {
        run_test(r#"system "ls""#);
        run_test(r#"system "jkjkjk""#);
        run_test(r#"system "*""#);
        run_test(r#"`pwd`"#);
        run_test(r#"`*`"#);
        run_test_error(r#"``"#);
    }
}
