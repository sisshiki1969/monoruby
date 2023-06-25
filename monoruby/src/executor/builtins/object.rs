use super::*;
use num::Zero;
use std::io::Write;

//
// Object class
//

pub(super) fn init(globals: &mut Globals) {
    //globals.define_builtin_class_func(OBJECT_CLASS, "new", object_new, -1);
    globals.define_builtin_func(OBJECT_CLASS, "inspect", inspect, 0);
    globals.define_builtin_func(OBJECT_CLASS, "p", p, -1);
    globals.define_builtin_func(OBJECT_CLASS, "class", class, 0);
    globals.define_builtin_func(OBJECT_CLASS, "singleton_class", singleton_class, 0);
    globals.define_builtin_func(OBJECT_CLASS, "respond_to?", respond_to, 1);
    globals.define_builtin_func(OBJECT_CLASS, "instance_of?", instance_of, 1);
    globals.define_builtin_func(OBJECT_CLASS, "is_a?", is_a, 1);
    globals.define_builtin_func(OBJECT_CLASS, "kind_of?", is_a, 1);
    globals.define_builtin_func_inlinable(
        OBJECT_CLASS,
        "nil?",
        nil,
        0,
        object_nil,
        analysis_object_nil,
    );
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
    globals.define_builtin_func(OBJECT_CLASS, "block_given?", block_given, 0);
    globals.define_builtin_func(OBJECT_CLASS, "to_s", to_s, 0);
    globals.define_builtin_func(OBJECT_CLASS, "rand", rand, -1);
    globals.define_builtin_func(OBJECT_CLASS, "Integer", kernel_integer, 1);
    globals.define_builtin_func(OBJECT_CLASS, "require", require, 1);
    globals.define_builtin_func(OBJECT_CLASS, "require_relative", require_relative, 1);
    globals.define_builtin_func(OBJECT_CLASS, "system", system, -1);
    globals.define_builtin_func(OBJECT_CLASS, "`", command, 1);
    globals.define_builtin_func(OBJECT_CLASS, "abort", abort, -1);
    globals.define_builtin_func(OBJECT_CLASS, "__dir__", dir_, 0);
    globals.define_builtin_func(OBJECT_CLASS, "__assert", assert, 2);
    globals.define_builtin_func(OBJECT_CLASS, "__dump", dump, 0);
}

///
/// ### Object.new
///
/// - new -> Object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/s/new.html]
#[monoruby_builtin]
fn object_new(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let obj = Value::new_object(class);
    if let Some(func_id) = globals.check_method(obj, IdentId::INITIALIZE) {
        vm.invoke_func2(globals, func_id, obj, arg, len)?;
    };
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
fn is_a(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let class = arg[0].expect_class_or_module(globals)?;
    Ok(Value::bool(lfp.self_val().is_kind_of(globals, class)))
}

///
/// ### Kernel#nil?
///
/// - nil? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/nil=3f.html]
#[monoruby_builtin]
fn nil(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg, _: usize) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().is_nil()))
}

///
/// ### Object#dup
///
/// - dup -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/clone.html]
#[monoruby_builtin]
fn dup(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    Ok(lfp.self_val().dup())
}

///
/// ### Kernel.#puts
///
/// - puts(*arg) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/puts.html]
#[monoruby_builtin]
fn puts(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    fn decompose(collector: &mut Vec<Value>, val: Value) {
        match val.is_array() {
            Some(ary) => {
                ary.iter().for_each(|v| decompose(collector, *v));
            }
            None => collector.push(val),
        }
    }
    let mut collector = Vec::new();
    for v in arg.iter(len) {
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
fn print(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    for v in arg.iter(len) {
        globals.write_stdout(&v.to_bytes(globals));
    }
    Ok(Value::nil())
}

///
/// ### Kernel.#block_given?
///
/// - block_given? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/block_given=3f.html]
#[monoruby_builtin]
fn block_given(
    vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    Ok(Value::bool(vm.cfp().prev().unwrap().block_given()))
}

///
/// ### Object#to_s
///
/// - to_s -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/to_s.html]
#[monoruby_builtin]
fn to_s(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg, _: usize) -> Result<Value> {
    let s = globals.tos(lfp.self_val());
    Ok(Value::new_string(s))
}

///
/// ### Kernel.#p
///
/// - p(*arg) -> object | Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/p.html]
#[monoruby_builtin]
fn p(_vm: &mut Executor, globals: &mut Globals, _lfp: LFP, arg: Arg, len: usize) -> Result<Value> {
    let mut buf = String::new();
    for v in arg.iter(len) {
        buf += &globals.inspect(v);
        buf += "\n";
    }
    globals.write_stdout(buf.as_bytes());
    Ok(match len {
        0 => Value::nil(),
        1 => arg[0],
        _ => {
            let ary = ArrayInner::from_iter(arg.iter(len));
            Value::new_array(ary)
        }
    })
}

#[monoruby_builtin]
fn assert(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let expected = arg[0];
    let actual = arg[1];
    eprintln!(
        "expected:{} actual:{}",
        globals.inspect(expected),
        globals.inspect(actual)
    );
    assert!(Value::eq(expected, actual));
    Ok(Value::nil())
}

#[monoruby_builtin]
fn dump(
    vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    crate::executor::compiler::runtime::_dump_stacktrace(vm, globals);
    Ok(Value::nil())
}

///
/// ### Object#respond_to?
///
/// - respond_to?(name, include_all = false) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/respond_to=3f.html]
#[monoruby_builtin]
fn respond_to(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
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
fn inspect(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _: Arg,
    _len: usize,
) -> Result<Value> {
    let s = globals.inspect(lfp.self_val());
    Ok(Value::new_string(s))
}

///
/// ### Object#class
///
/// - class -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/class.html]
#[monoruby_builtin]
fn class(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _: Arg,
    _len: usize,
) -> Result<Value> {
    Ok(lfp.self_val().real_class(globals).as_val())
}

///
/// ### Object#instance_of?
///
/// - instance_of?(klass) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_of=3f.html]
#[monoruby_builtin]
fn instance_of(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let b = lfp.self_val().real_class(globals).id() == arg[0].expect_class_or_module(globals)?;
    Ok(Value::bool(b))
}

///
/// ### Kernel.#rand
///
/// - rand(max = 0) -> Integer | Float
/// - rand(range) -> Integer | Float | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/rand.html]
#[monoruby_builtin]
fn rand(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments_range(len, 0..=1)?;
    let i = match len {
        0 => 0i64,
        1 => arg[0].coerce_to_fixnum(globals)?,
        _ => unreachable!(),
    };
    if !i.is_zero() {
        Ok(Value::integer(
            (rand::random::<f64>() * (i.abs() as f64)) as i64,
        ))
    } else {
        Ok(Value::float(rand::random()))
    }
}

///
/// ### Object#singleton_class
///
/// - singleton_class -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/singleton_class.html]
#[monoruby_builtin]
fn singleton_class(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _: Arg,
    _len: usize,
) -> Result<Value> {
    Ok(lfp.self_val().get_singleton(globals))
}

///
/// ### Object#instance_variable_defined?
///
/// - instance_variable_defined?(var) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_variable_defined=3f.html]
#[monoruby_builtin]
fn instance_variable_defined(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
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
fn instance_variable_set(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
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
fn instance_variable_get(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let id = arg[0].expect_symbol_or_string(globals)?;
    let v = globals.get_ivar(lfp.self_val(), id).unwrap_or_default();
    Ok(v)
}

///
/// ### Kernel.#Integer
///
/// - Integer(arg, base = 0, exception: true) -> Integer | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/Integer.html]
#[monoruby_builtin]
fn kernel_integer(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let arg0 = arg[0];
    match arg0.unpack() {
        RV::Integer(num) => return Ok(Value::integer(num)),
        RV::BigInt(num) => return Ok(Value::bigint(num.clone())),
        RV::Float(num) => return Ok(Value::integer(num.trunc() as i64)),
        RV::String(b) => {
            if let Ok(s) = String::from_utf8(b.to_vec()) {
                match s.parse::<i64>() {
                    Ok(num) => return Ok(Value::integer(num)),
                    Err(_) => {
                        let s = globals.tos(arg0);
                        return Err(MonorubyErr::argumenterr(format!(
                            "invalid value for Integer(): {}",
                            s
                        )));
                    }
                }
            }
        }
        _ => {}
    };
    Err(MonorubyErr::no_implicit_conversion(
        globals,
        arg0,
        INTEGER_CLASS,
    ))
}

fn load(
    vm: &mut Executor,
    globals: &mut Globals,
    file_name: std::path::PathBuf,
    is_relative: bool,
) -> Result<Value> {
    if let Some((file_body, path)) = globals.load_lib(&file_name, is_relative)? {
        vm.enter_class_context();
        let res = vm.eval_script(globals, file_body, &path);
        vm.exit_class_context();
        res?;
        Ok(Value::bool(true))
    } else {
        Ok(Value::bool(false))
    }
}

///
/// ### Kernel.#require
///
/// - require(feature) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/require.html]
#[monoruby_builtin]
fn require(
    vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    _: usize,
) -> Result<Value> {
    let feature = arg[0].expect_string(globals)?;
    let file_name = std::path::PathBuf::from(feature);
    load(vm, globals, file_name, false)
}

///
/// ### Kernel.#require_relative
///
/// - require_relative(relative_feature) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/require_relative.html]
#[monoruby_builtin]
fn require_relative(
    vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    _: usize,
) -> Result<Value> {
    let mut file_name: std::path::PathBuf = globals.current_source_path(vm).into();
    file_name.pop();
    let feature = std::path::PathBuf::from(arg[0].expect_string(globals)?);
    file_name.extend(&feature);
    file_name.set_extension("rb");
    load(vm, globals, file_name, true)
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
fn system(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    use std::process::Command;
    MonorubyErr::check_min_number_of_arguments(len, 1)?;
    let input = arg[0].as_string();
    let (program, mut args) = prepare_command_arg(input);
    if len > 1 {
        let iter = arg.iter(len);
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
fn command(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    _: usize,
) -> Result<Value> {
    use std::process::Command;
    let input = arg[0].as_string();
    let (program, args) = prepare_command_arg(input);
    match Command::new(program).args(&args).output() {
        Ok(output) => {
            std::io::stderr().write_all(&output.stderr).unwrap();
            Ok(Value::new_string_from_vec(output.stdout))
        }
        Err(err) => Err(MonorubyErr::runtimeerr(format!("{}", err))),
    }
}

///
/// Kernel.#abort
///
/// - abort -> ()
/// - abort(message) -> ()
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/abort.htmll]
#[monoruby_builtin]
fn abort(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments_range(len, 0..=1)?;
    if len == 1 {
        match arg[0].is_string() {
            Some(s) => eprintln!("{}", s),
            None => {
                return Err(MonorubyErr::no_implicit_conversion(
                    globals,
                    arg[0],
                    STRING_CLASS,
                ));
            }
        }
    }
    std::process::exit(1);
}

///
/// Kernel.#__dir__
///
/// - __dir__ -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/__dir__.html]
#[monoruby_builtin]
fn dir_(
    vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    _arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    let path = globals.current_source_path(vm).parent().unwrap();
    Ok(Value::new_string(path.to_string_lossy().to_string()))
}

fn object_nil(
    gen: &mut Codegen,
    ctx: &mut BBContext,
    method_info: &MethodInfo,
    ret: SlotId,
    _pc: BcPc,
    _deopt: DestLabel,
) {
    let MethodInfo { recv, .. } = method_info;
    gen.load_rdi(*recv);
    ctx.dealloc_xmm(ret);
    let l1 = gen.jit.label();
    monoasm!( &mut gen.jit,
        movq rax, (FALSE_VALUE);
        cmpq rdi, (NIL_VALUE);
        jne  l1;
        movq rax, (TRUE_VALUE);
    l1:
    );
    gen.store_rax(ret);
}

fn analysis_object_nil(info: &mut SlotInfo, method_info: &MethodInfo, ret: SlotId) {
    info.use_non_float(method_info.recv);
    info.def_as(ret, false);
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
}
