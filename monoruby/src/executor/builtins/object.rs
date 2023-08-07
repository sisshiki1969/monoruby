use super::*;
use std::io::Write;

//
// Object class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_by_str("Object", OBJECT_CLASS, None, OBJECT_CLASS);
    //globals.define_builtin_class_func(OBJECT_CLASS, "new", object_new, -1);
    globals.define_builtin_func(OBJECT_CLASS, "inspect", inspect, 0);
    globals.define_builtin_func(OBJECT_CLASS, "class", class, 0);
    globals.define_builtin_func(OBJECT_CLASS, "singleton_class", singleton_class, 0);
    globals.define_builtin_func(OBJECT_CLASS, "respond_to?", respond_to, 1);
    globals.define_builtin_func(OBJECT_CLASS, "instance_of?", instance_of, 1);
    globals.define_builtin_func(OBJECT_CLASS, "is_a?", is_a, 1);
    globals.define_builtin_func(OBJECT_CLASS, "kind_of?", is_a, 1);
    globals.define_builtin_func(OBJECT_CLASS, "to_enum", to_enum, -1);
    globals.define_builtin_func(OBJECT_CLASS, "enum_for", to_enum, -1);

    globals.define_builtin_func(OBJECT_CLASS, "dup", dup, 0);
    globals.define_builtin_func(OBJECT_CLASS, "instance_variable_defined?", iv_defined, 1);
    globals.define_builtin_func(OBJECT_CLASS, "instance_variable_set", iv_set, 2);
    globals.define_builtin_func(OBJECT_CLASS, "instance_variable_get", iv_get, 1);
    globals.define_builtin_func(OBJECT_CLASS, "to_s", to_s, 0);
    globals.define_builtin_func(OBJECT_CLASS, "method", method, 1);
    globals.define_builtin_func(OBJECT_CLASS, "system", system, -1);
    globals.define_builtin_func(OBJECT_CLASS, "`", command, 1);
}

///
/// ### Object.new
///
/// - new -> Object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/s/new.html]
#[monoruby_builtin]
fn object_new(vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    let class = lfp.self_val().as_class_id();
    let obj = Value::object(class);
    vm.invoke_method_if_exists(globals, IdentId::INITIALIZE, obj, arg, len, lfp.block())?;
    Ok(obj)
}

///
/// ### Object#is_a?
///
/// - is_a?(mod) -> bool
/// - kind_of?(mod) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/is_a=3f.html]
#[monoruby_builtin]
fn is_a(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let class = arg[0].expect_class_or_module(globals)?;
    Ok(Value::bool(lfp.self_val().is_kind_of(globals, class)))
}

///
/// ### Object#enum_for
///
/// - to_enum(method = :each, *args) -> Enumerator
/// - enum_for(method = :each, *args) -> Enumerator
/// - to_enum(method = :each, *args) {|*args| ... } -> Enumerator
/// - enum_for(method = :each, *args) {|*args| ... } -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/enum_for.html]
#[monoruby_builtin]
fn to_enum(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let proc = vm.generate_enumerator_proc(globals, IdentId::EACH);
    Ok(Value::new_enumerator(lfp.self_val(), IdentId::EACH, proc))
}

///
/// ### Object#dup
///
/// - dup -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/clone.html]
#[monoruby_builtin]
fn dup(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    Ok(lfp.self_val().dup())
}

///
/// ### Kernel.#puts
///
/// - puts(*arg) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/puts.html]
#[monoruby_builtin]
fn puts(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    fn decompose(collector: &mut Vec<Value>, val: Value) {
        match val.is_array() {
            Some(ary) => {
                ary.iter().for_each(|v| decompose(collector, *v));
            }
            None => collector.push(val),
        }
    }
    let mut collector = Vec::new();
    for v in lfp.iter() {
        decompose(&mut collector, v);
    }

    for v in collector {
        let mut bytes = v.to_bytes(globals);
        bytes.extend(b"\n");
        globals.write_stdout(&bytes);
    }
    globals.flush_stdout();
    Ok(Value::nil())
}

///
/// ### Kernel.#print
///
/// - print(*arg) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/print.html]
#[monoruby_builtin]
fn print(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    for v in lfp.iter() {
        globals.write_stdout(&v.to_bytes(globals));
    }
    Ok(Value::nil())
}

///
/// ### Kernel.#fail
///
/// - [NOT SUPPORTED] raise -> ()
/// - [NOT SUPPORTED] fail -> ()
/// - raise(error_type, message = nil, backtrace = caller(0), cause: $!) -> ()
/// - fail(error_type, message = nil, backtrace = caller(0), cause: $!) -> ()
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/fail.html]
#[monoruby_builtin]
fn raise(vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    MonorubyErr::check_number_of_arguments_range(len, 1..=2)?;
    if let Some(ex) = arg[0].is_exception() {
        let mut err = MonorubyErr::new_from_exception(ex);
        if len == 2 {
            err.set_msg(arg[1].expect_string(globals)?);
        }
        return Err(err);
    } else if let Some(klass) = arg[0].is_class() {
        if klass.get_module(globals).is_exception() {
            if let Some(ex) =
                vm.invoke_method(globals, IdentId::NEW, klass.get_obj(globals), &[], None)
            {
                let mut err = MonorubyErr::new_from_exception(ex.is_exception().unwrap());
                if len == 2 {
                    err.set_msg(arg[1].expect_string(globals)?);
                }
                return Err(err);
            } else {
                return Err(vm.take_error());
            };
        }
    }
    Err(MonorubyErr::typeerr("exception class/object expected"))
}

///
/// ### Kernel.#block_given?
///
/// - block_given? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/block_given=3f.html]
#[monoruby_builtin]
fn block_given(vm: &mut Executor, _globals: &mut Globals, _lfp: LFP, _arg: Arg) -> Result<Value> {
    Ok(Value::bool(vm.cfp().prev().unwrap().block_given()))
}

///
/// ### Object#to_s
///
/// - to_s -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/to_s.html]
#[monoruby_builtin]
fn to_s(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let s = globals.to_s(lfp.self_val());
    Ok(Value::string(s))
}

///
/// ### Object#respond_to?
///
/// - respond_to?(name, include_all = false) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/respond_to=3f.html]
#[monoruby_builtin]
fn respond_to(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let name = match arg[0].unpack() {
        RV::Symbol(id) => id,
        RV::String(b) => IdentId::get_id(String::from_utf8_lossy(b).as_ref()),
        _ => unimplemented!(),
    };
    Ok(Value::bool(
        globals.check_method(lfp.self_val(), name).is_some(),
    ))
}

///
/// ### Object#inspect
///
/// - inspect -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/inspect.html]
#[monoruby_builtin]
fn inspect(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let s = globals.inspect(lfp.self_val());
    Ok(Value::string(s))
}

///
/// ### Object#class
///
/// - class -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/class.html]
#[monoruby_builtin]
fn class(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    Ok(lfp.self_val().real_class(globals).as_val())
}

///
/// ### Object#instance_of?
///
/// - instance_of?(klass) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_of=3f.html]
#[monoruby_builtin]
fn instance_of(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let b = lfp.self_val().real_class(globals).id() == arg[0].expect_class_or_module(globals)?;
    Ok(Value::bool(b))
}

///
/// ### Object#method
///
/// - method(name) -> Method
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/method.html]
#[monoruby_builtin]
fn method(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    MonorubyErr::check_number_of_arguments(len, 1)?;
    let receiver = lfp.self_val();
    let method_name = arg[0].expect_symbol_or_string(globals)?;
    let func_id = globals.find_method(receiver, method_name, false)?;
    Ok(Value::new_method(receiver, func_id))
}

///
/// ### Object#singleton_class
///
/// - singleton_class -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/singleton_class.html]
#[monoruby_builtin]
fn singleton_class(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    Ok(lfp.self_val().get_singleton(globals))
}

///
/// ### Object#instance_variable_defined?
///
/// - instance_variable_defined?(var) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_variable_defined=3f.html]
#[monoruby_builtin]
fn iv_defined(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let id = match arg[0].unpack() {
        RV::Symbol(sym) => sym,
        RV::String(s) => IdentId::get_id(String::from_utf8_lossy(s).as_ref()),
        _ => return Err(MonorubyErr::is_not_symbol_nor_string(globals, arg[0])),
    };
    let b = globals.get_ivar(lfp.self_val(), id).is_some();
    Ok(Value::bool(b))
}

///
/// ### Object#instance_variable_set
///
/// - instance_variable_set(var, value) -> Object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_variable_set.html]
#[monoruby_builtin]
fn iv_set(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let id = arg[0].expect_symbol_or_string(globals)?;
    let val = arg[1];
    globals.set_ivar(lfp.self_val(), id, val)?;
    Ok(val)
}

///
/// ### Object#instance_variable_get
///
/// - instance_variable_get(var) -> Object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_variable_get.html]
#[monoruby_builtin]
fn iv_get(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let id = arg[0].expect_symbol_or_string(globals)?;
    let v = globals.get_ivar(lfp.self_val(), id).unwrap_or_default();
    Ok(v)
}

fn prepare_command_arg(input: String) -> (String, Vec<String>) {
    let mut args = vec![];
    let include_meta = input.contains([
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
/// - system(program, *args, options={}) -> bool | nil
/// - [NOT SUPPORTED] system(env, command, options={}) -> bool | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/system.html]
#[monoruby_builtin]
fn system(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    use std::process::Command;
    let len = lfp.arg_len();
    MonorubyErr::check_min_number_of_arguments(len, 1)?;
    let input = arg[0].as_string();
    let (program, mut args) = prepare_command_arg(input);
    if len > 1 {
        let iter = lfp.iter();
        //iter.take(1);
        for v in iter.take(1) {
            args.push(v.expect_string(globals)?);
        }
    }
    Ok(match Command::new(program).args(&args).status() {
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
#[monoruby_builtin]
fn command(_vm: &mut Executor, _globals: &mut Globals, _lfp: LFP, arg: Arg) -> Result<Value> {
    use std::process::Command;
    let input = arg[0].as_string();
    let (program, args) = prepare_command_arg(input);
    match Command::new(program).args(&args).output() {
        Ok(output) => {
            std::io::stderr().write_all(&output.stderr).unwrap();
            Ok(Value::string_from_vec(output.stdout))
        }
        Err(err) => Err(MonorubyErr::runtimeerr(err)),
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
    fn dup() {
        run_test("1.dup");
        run_test("1.5.dup");
        run_test("'Ruby'.dup");
        run_test(":Ruby.dup");
        run_test("[1,2,3].dup");
        run_test("{a:1,b:2}.dup");
        run_test("(1..3).dup");
    }

    #[test]
    fn block_given() {
        run_test_with_prelude(
            r##"
            [check, check {}]
        "##,
            r##"
        def check
            if block_given?
                "Block is given."
            else
                "Block isn't given."
            end
        end
        "##,
        );
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
    fn object_nil() {
        run_test("4.nil?");
        run_test("4.5.nil?");
        run_test("nil.nil?");
        run_test("true.nil?");
        run_test("false.nil?");
        run_test("[].nil?");
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

    #[test]
    fn to_enum() {
        run_test(
            r#"
        e = [1,2,3,4,5].to_enum
        e.next
        "#,
        );
    }
}
