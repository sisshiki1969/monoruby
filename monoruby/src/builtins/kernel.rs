use super::*;
use num::Zero;
use std::io::Write;

//
// Kernel module
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_module("Kernel");
    let kernel_class = klass.id();
    globals.include_module(OBJECT_CLASS.get_module(globals), klass);
    globals.define_builtin_func(kernel_class, "inspect", inspect);
    globals.define_builtin_func(kernel_class, "class", class);
    globals.define_builtin_func(kernel_class, "singleton_class", singleton_class);
    globals.define_builtin_func(kernel_class, "respond_to?", respond_to);
    globals.define_builtin_func(kernel_class, "instance_of?", instance_of);
    globals.define_builtin_inline_func(kernel_class, "nil?", nil, object_nil, analysis::v_v);
    globals.define_builtin_func(kernel_class, "instance_variable_defined?", iv_defined);
    globals.define_builtin_func(kernel_class, "instance_variable_set", iv_set);
    globals.define_builtin_func(kernel_class, "instance_variable_get", iv_get);
    globals.define_builtin_module_func(kernel_class, "__dir__", dir_);
    globals.define_builtin_module_func(kernel_class, "`", command);
    globals.define_builtin_module_func(kernel_class, "Integer", kernel_integer);
    globals.define_builtin_module_func(kernel_class, "abort", abort);
    globals.define_builtin_module_func(kernel_class, "block_given?", block_given);
    globals.define_builtin_module_func(kernel_class, "eval", eval);
    globals.define_builtin_module_func(kernel_class, "fail", raise);
    globals.define_builtin_module_func(kernel_class, "loop", loop_);
    globals.define_builtin_module_func(kernel_class, "p", p);
    globals.define_builtin_module_func(kernel_class, "print", print);
    globals.define_builtin_module_func(kernel_class, "puts", puts);
    globals.define_builtin_module_func(kernel_class, "raise", raise);
    globals.define_builtin_module_func(kernel_class, "rand", rand);
    globals.define_builtin_module_func(kernel_class, "require", require);
    globals.define_builtin_module_func(kernel_class, "require_relative", require_relative);
    globals.define_builtin_module_func(kernel_class, "system", system);
    globals.define_builtin_func(kernel_class, "method", method);
    globals.define_builtin_func(kernel_class, "__assert", assert);
    globals.define_builtin_func(kernel_class, "__dump", dump);
    globals.define_builtin_func(
        kernel_class,
        "__enum_yield",
        super::enumerator::yielder_yield,
    );
}

///
/// ### Kernel#nil?
///
/// - nil? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/nil=3f.html]
#[monoruby_builtin]
fn nil(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().is_nil()))
}

fn object_nil(
    gen: &mut Codegen,
    ctx: &mut BBContext,
    callsite: &CallSiteInfo,
    _pc: BcPc,
    _deopt: DestLabel,
) {
    let CallSiteInfo { recv, ret, .. } = *callsite;
    gen.fetch_slots(ctx, &[recv]);
    gen.load_rdi(recv);
    ctx.release(ret);
    monoasm!( &mut gen.jit,
        movq rax, (FALSE_VALUE);
        movq rsi, (TRUE_VALUE);
        cmpq rdi, (NIL_VALUE);
        cmoveqq rax, rsi;
    );
    if let Some(ret) = ret {
        gen.store_rax(ret);
    }
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
/// ### Kernel.#loop
///
/// - loop { ... } -> object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/loop.html]
#[monoruby_builtin]
fn loop_(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    MonorubyErr::check_number_of_arguments(len, 0)?;
    let bh = lfp.expect_block()?;
    let data = globals.get_block_data(vm.cfp(), bh);
    loop {
        if let Err(err) = vm.invoke_block(globals, &data, &[]) {
            return if err.kind() == &MonorubyErrKind::StopIteration {
                Ok(Value::nil())
            } else {
                Err(err)
            };
        }
    }
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
            let ex =
                vm.invoke_method_inner(globals, IdentId::NEW, klass.get_obj(globals), &[], None)?;
            let mut err = MonorubyErr::new_from_exception(ex.is_exception().unwrap());
            if len == 2 {
                err.set_msg(arg[1].expect_string(globals)?);
            }
            return Err(err);
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
/// ### Kernel.#p
///
/// - p(*arg) -> object | Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/p.html]
#[monoruby_builtin]
fn p(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    let mut buf = String::new();
    for v in lfp.iter() {
        buf += &globals.inspect(v);
        buf += "\n";
    }
    globals.write_stdout(buf.as_bytes());
    Ok(match len {
        0 => Value::nil(),
        1 => arg[0],
        _ => Value::array_from_iter(lfp.iter()),
    })
}

#[monoruby_builtin]
fn assert(_vm: &mut Executor, globals: &mut Globals, _lfp: LFP, arg: Arg) -> Result<Value> {
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
fn dump(vm: &mut Executor, globals: &mut Globals, _lfp: LFP, _arg: Arg) -> Result<Value> {
    crate::runtime::_dump_stacktrace(vm, globals);
    Ok(Value::nil())
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
/// ### Kernel.#rand
///
/// - rand(max = 0) -> Integer | Float
/// - rand(range) -> Integer | Float | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/rand.html]
#[monoruby_builtin]
fn rand(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    MonorubyErr::check_number_of_arguments_range(len, 0..=1)?;
    let i = match len {
        0 => 0i64,
        1 => arg[0].coerce_to_i64(globals)?,
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

///
/// ### Kernel.#Integer
///
/// - Integer(arg, base = 0, exception: true) -> Integer | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/Integer.html]
#[monoruby_builtin]
fn kernel_integer(_vm: &mut Executor, globals: &mut Globals, _lfp: LFP, arg: Arg) -> Result<Value> {
    let arg0 = arg[0];
    match arg0.unpack() {
        RV::Fixnum(num) => return Ok(Value::integer(num)),
        RV::BigInt(num) => return Ok(Value::bigint(num.clone())),
        RV::Float(num) => return Ok(Value::integer(num.trunc() as i64)),
        RV::String(b) => {
            if let Ok(s) = String::from_utf8(b.to_vec()) {
                match s.parse::<i64>() {
                    Ok(num) => return Ok(Value::integer(num)),
                    Err(_) => {
                        let s = globals.to_s(arg0);
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
fn require(vm: &mut Executor, globals: &mut Globals, _lfp: LFP, arg: Arg) -> Result<Value> {
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
) -> Result<Value> {
    let mut file_name: std::path::PathBuf = globals.current_source_path(vm).into();
    file_name.pop();
    let feature = std::path::PathBuf::from(arg[0].expect_string(globals)?);
    file_name.extend(&feature);
    file_name.set_extension("rb");
    load(vm, globals, file_name, true)
}

///
/// ### Kernel.#eval
///
/// - eval(expr) -> object
/// - eval(expr, bind, fname = "(eval)", lineno = 1) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/eval.html]
#[monoruby_builtin]
fn eval(vm: &mut Executor, globals: &mut Globals, _lfp: LFP, arg: Arg) -> Result<Value> {
    let expr = arg[0].expect_string(globals)?;
    let path = globals.store[vm.cfp().get_source_pos()]
        .as_ruby_func()
        .sourceinfo
        .path
        .clone();
    let fid = globals.compile_script_with_binding(expr, path, None, None)?;
    vm.eval(globals, fid)
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
fn abort(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
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
fn dir_(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    MonorubyErr::check_number_of_arguments(len, 0)?;
    let path = globals.current_source_path(vm).parent().unwrap();
    Ok(Value::string(path.to_string_lossy().to_string()))
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn torigonometric() {
        run_test("Math.cos 149");
        run_test("Math.cos -14.97522");
        run_test("Math.sin 149");
        run_test("Math.sin -14.97522");
    }

    #[test]
    fn eval() {
        run_test(r##"eval "1+2+3""##);
        run_test_error(r##"eval "1/0""##);
    }
}
