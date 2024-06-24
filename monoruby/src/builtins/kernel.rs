use super::*;
use num::ToPrimitive;
use num::Zero;
use std::{io::Write, mem::transmute};

//
// Kernel module
//

pub(super) fn init(globals: &mut Globals) -> Module {
    let klass = globals.define_module("Kernel");
    let kernel_class = klass.id();
    globals.define_builtin_inline_func(
        kernel_class,
        "nil?",
        nil,
        Box::new(object_nil),
        analysis::v_v,
        0,
    );
    globals.define_builtin_module_func_rest(kernel_class, "puts", puts);
    globals.define_builtin_module_func_rest(kernel_class, "print", print);
    globals.define_builtin_module_func(kernel_class, "proc", proc, 0);
    globals.define_builtin_module_func(kernel_class, "lambda", lambda, 0);
    globals.define_builtin_module_func(kernel_class, "binding", binding, 0);
    globals.define_builtin_module_func(kernel_class, "loop", loop_, 0);
    globals.define_builtin_module_func_with(kernel_class, "raise", raise, 1, 2, false);
    globals.define_builtin_module_func_with(kernel_class, "fail", raise, 1, 2, false);
    globals.define_builtin_module_func(kernel_class, "block_given?", block_given, 0);
    globals.define_builtin_module_func_rest(kernel_class, "p", p);
    globals.define_builtin_module_func_with(kernel_class, "rand", rand, 0, 1, false);
    globals.define_builtin_module_func(kernel_class, "Integer", kernel_integer, 1);
    globals.define_builtin_module_func(kernel_class, "Float", kernel_float, 1);
    globals.define_builtin_module_func_with(kernel_class, "Complex", kernel_complex, 1, 2, false);
    globals.define_builtin_module_func_with(kernel_class, "Array", kernel_array, 1, 1, false);
    globals.define_builtin_module_func(kernel_class, "require", require, 1);
    globals.define_builtin_module_func(kernel_class, "require_relative", require_relative, 1);
    globals.define_builtin_module_func_eval_with(kernel_class, "eval", eval, 1, 4, false);
    globals.define_builtin_module_func_with(kernel_class, "system", system, 1, 1, true);
    globals.define_builtin_module_func(kernel_class, "`", command, 1);
    globals.define_builtin_module_func_with(kernel_class, "sleep", sleep, 0, 1, false);
    globals.define_builtin_module_func_with(kernel_class, "abort", abort, 0, 1, false);
    globals.define_builtin_module_func_with(kernel_class, "exit", exit, 0, 1, false);
    globals.define_builtin_module_func_with_kw(
        kernel_class,
        "warn",
        warn,
        0,
        0,
        true,
        &["uplevel", "category"],
    );
    globals.define_builtin_module_func(kernel_class, "__dir__", dir_, 0);
    globals.define_builtin_func(kernel_class, "__assert", assert, 2);
    globals.define_builtin_func(kernel_class, "__dump", dump, 0);
    globals.define_builtin_func(
        kernel_class,
        "__enum_yield",
        super::enumerator::yielder_yield,
        1,
    );

    globals.define_builtin_module_func(kernel_class, "___dlopen", dlopen, 1);
    globals.define_builtin_module_func(kernel_class, "___dlsym", dlsym, 2);
    globals.define_builtin_module_func(kernel_class, "___call", dlcall, 4);
    klass
}

///
/// ### Kernel#nil?
///
/// - nil? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/nil=3f.html]
#[monoruby_builtin]
fn nil(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().is_nil()))
}

fn object_nil(ir: &mut AsmIr, store: &Store, bb: &mut BBContext, callid: CallSiteId, _pc: BcPc) {
    //bb.link_stack(dst);
    ir.inline(|gen, _| {
        monoasm! { &mut gen.jit,
            movq rax, (FALSE_VALUE);
            movq rsi, (TRUE_VALUE);
            cmpq rdi, (NIL_VALUE);
            cmoveqq rax, rsi;
        }
    });
    ir.rax2acc(bb, store[callid].dst);
}

///
/// ### Kernel.#puts
///
/// - puts(*arg) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/puts.html]
#[monoruby_builtin]
fn puts(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    fn decompose(collector: &mut Vec<Value>, val: Value) {
        match val.try_array_ty() {
            Some(ary) => {
                ary.iter().for_each(|v| decompose(collector, *v));
            }
            None => collector.push(val),
        }
    }
    let mut collector = Vec::new();
    for v in lfp.arg(0).as_array().iter().cloned() {
        decompose(&mut collector, v);
    }

    for v in collector {
        globals.print_value(v);
        globals.write_stdout(b"\n");
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
fn print(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    for v in lfp.arg(0).as_array().iter().cloned() {
        globals.print_value(v);
    }
    Ok(Value::nil())
}

///
/// ### Kernel.#lambda
///
/// - proc { ... } -> Proc
/// - lambda { ... } -> Proc
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/lambda.html]
#[monoruby_builtin]
fn proc(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        let p = vm.generate_proc(globals, bh)?;
        Ok(p.into())
    } else {
        Err(MonorubyErr::create_proc_no_block())
    }
}

#[monoruby_builtin]
fn lambda(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        let func_id = bh.func_id();
        globals.store[func_id].set_method_style();
        let p = vm.generate_proc(globals, bh)?;
        Ok(p.into())
    } else {
        Err(MonorubyErr::create_proc_no_block())
    }
}

///
/// ### Kernel.#binding
///
/// - binding -> Binding
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/binding.html]
#[monoruby_builtin]
fn binding(vm: &mut Executor, _: &mut Globals, _: Lfp) -> Result<Value> {
    Ok(vm.generate_binding().as_val())
}

///
/// ### Kernel.#loop
///
/// - loop { ... } -> object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/loop.html]
#[monoruby_builtin]
fn loop_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let data = vm.get_block_data(globals, bh)?;
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
/// - raise(error_type, message = nil, [NOT SUPPORTED] backtrace = caller(0), [NOT SUPPORTED] cause: $!) -> ()
/// - fail(error_type, message = nil, [NOT SUPPORTED] backtrace = caller(0), [NOT SUPPORTED] cause: $!) -> ()
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/fail.html]
#[monoruby_builtin]
fn raise(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    if let Some(ex) = lfp.arg(0).is_exception() {
        let mut err = MonorubyErr::new_from_exception(ex);
        if let Some(arg1) = lfp.try_arg(1) {
            err.set_msg(arg1.expect_string()?);
        }
        return Err(err);
    } else if let Some(klass) = lfp.arg(0).is_class() {
        if klass.get_module(globals).is_exception() {
            let ex =
                vm.invoke_method_inner(globals, IdentId::NEW, klass.get_obj(globals), &[], None)?;
            let mut err = MonorubyErr::new_from_exception(ex.is_exception().unwrap());
            if let Some(arg1) = lfp.try_arg(1) {
                err.set_msg(arg1.expect_string()?);
            }
            return Err(err);
        }
    }
    Err(MonorubyErr::typeerr(
        "exception class/object expected",
        TypeErrKind::Other,
    ))
}

///
/// ### Kernel.#block_given?
///
/// - block_given? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/block_given=3f.html]
#[monoruby_builtin]
fn block_given(vm: &mut Executor, _globals: &mut Globals, _: Lfp) -> Result<Value> {
    Ok(Value::bool(vm.cfp().prev().unwrap().block_given()))
}

///
/// ### Kernel.#p
///
/// - p(*arg) -> object | Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/p.html]
#[monoruby_builtin]
fn p(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let len = lfp.arg(0).as_array().len();
    let mut buf = String::new();
    for v in lfp.arg(0).as_array().iter().cloned() {
        buf += &v.inspect(globals);
        buf += "\n";
    }
    globals.write_stdout(buf.as_bytes());
    Ok(match len {
        0 => Value::nil(),
        1 => lfp.arg(0).as_array()[0],
        _ => lfp.arg(0),
    })
}

#[monoruby_builtin]
fn assert(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let expected = lfp.arg(0);
    let actual = lfp.arg(1);
    eprintln!(
        "expected:{} actual:{}",
        expected.inspect(globals),
        actual.inspect(globals)
    );
    assert!(Value::eq(expected, actual));
    Ok(Value::nil())
}

#[monoruby_builtin]
fn dump(vm: &mut Executor, globals: &mut Globals, _lfp: Lfp) -> Result<Value> {
    crate::runtime::_dump_stacktrace(vm, globals);
    Ok(Value::nil())
}

///
/// ### Kernel.#rand
///
/// - rand(max = 0) -> Integer | Float
/// - rand(range) -> Integer | Float | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/rand.html]
#[monoruby_builtin]
fn rand(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let i = if let Some(arg0) = lfp.try_arg(0) {
        arg0.coerce_to_i64()?
    } else {
        0i64
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
/// ### Kernel.#Integer
///
/// - Integer(arg, [NOT SUPPORTED] base = 0, [NOT SUPPORTED] exception: true) -> Integer | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/Integer.html]
#[monoruby_builtin]
fn kernel_integer(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg0 = lfp.arg(0);
    match arg0.unpack() {
        RV::Fixnum(num) => return Ok(Value::integer(num)),
        RV::BigInt(num) => return Ok(Value::bigint(num.clone())),
        RV::Float(num) => return Ok(Value::integer(num.trunc() as i64)),
        RV::String(b) => match b.check_utf8()?.parse::<i64>() {
            Ok(num) => return Ok(Value::integer(num)),
            Err(_) => {
                let s = arg0.to_s(globals);
                return Err(MonorubyErr::argumenterr(format!(
                    "invalid value for Integer(): {}",
                    s
                )));
            }
        },
        _ => {}
    };
    Err(MonorubyErr::no_implicit_conversion(arg0, INTEGER_CLASS))
}

///
/// ### Kernel.#Float
///
/// - Float(arg, [NOT SUPPORTED] exception: true) -> Float | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/Float.html]
#[monoruby_builtin]
fn kernel_float(_vm: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg0 = lfp.arg(0);
    match arg0.unpack() {
        RV::Fixnum(num) => return Ok(Value::float(num as f64)),
        RV::BigInt(num) => return Ok(Value::float(num.to_f64().unwrap())),
        RV::Float(num) => return Ok(Value::float(num)),
        RV::String(b) => {
            let s = b.to_str()?;
            let (f, err) = parse_f64(&s);
            if err {
                return Err(MonorubyErr::argumenterr(format!(
                    "invalid value for Float(): {s}"
                )));
            }
            return Ok(Value::float(f));
        }
        _ => {}
    };
    Err(MonorubyErr::no_implicit_conversion(arg0, FLOAT_CLASS))
}

///
/// ### Kernel.#Complex
///
/// - Complex(r, i = 0, [NOT SUPPORTED] exception: true) -> Complex | nil
/// - [NOT SUPPORTED] Complex(s, exception: true) -> Complex | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/Complex.html]
#[monoruby_builtin]
fn kernel_complex(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let r = Real::try_from(lfp.arg(0))?;
    let i = if let Some(i) = lfp.try_arg(1) {
        Real::try_from(i)?
    } else {
        Real::zero()
    };
    Ok(Value::complex(r, i))
}

///
/// ### Kernel.#Array
///
/// - Array(arg) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/Array.html]
#[monoruby_builtin]
fn kernel_array(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg = lfp.arg(0);
    if arg.is_array_ty() {
        return Ok(arg);
    }
    if let Some(func_id) = globals.check_method(arg, IdentId::TO_ARY) {
        return vm
            .invoke_func(globals, func_id, arg, &[], None)
            .ok_or_else(|| vm.take_error());
    } else if let Some(func_id) = globals.check_method(arg, IdentId::TO_A) {
        return vm
            .invoke_func(globals, func_id, arg, &[], None)
            .ok_or_else(|| vm.take_error());
    };
    Ok(Value::array1(arg))
}

///
/// ### Kernel.#require
///
/// - require(feature) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/require.html]
#[monoruby_builtin]
fn require(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let feature = lfp.arg(0).expect_string()?;
    let file_name = std::path::PathBuf::from(feature);
    let b = vm.require(globals, &file_name, false)?;
    Ok(Value::bool(b))
}

///
/// ### Kernel.#require_relative
///
/// - require_relative(relative_feature) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/require_relative.html]
#[monoruby_builtin]
fn require_relative(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut file_name: std::path::PathBuf = globals.current_source_path(vm).into();
    file_name.pop();
    let feature = std::path::PathBuf::from(lfp.arg(0).expect_string()?);
    file_name.extend(&feature);
    file_name.set_extension("rb");
    let b = vm.require(globals, &file_name, true)?;
    Ok(Value::bool(b))
}

///
/// ### Kernel.#eval
///
/// - eval(expr) -> object
/// - eval(expr, bind, [NOT SUPPORTED] fname = "(eval)", [NOT SUPPORTED] lineno = 1) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/eval.html]
#[monoruby_builtin]
fn eval(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let expr = lfp.arg(0).expect_string()?;
    let cfp = vm.cfp();
    if let Some(bind) = lfp.try_arg(1)
        && !bind.is_nil()
    {
        let binding = if let Some(b) = Binding::try_new(bind) {
            b
        } else {
            return Err(MonorubyErr::typeerr("Binding expected", TypeErrKind::Other));
        };
        globals.compile_script_binding(expr, "(eval)", binding)?;
        vm.invoke_binding(globals, binding.binding().unwrap())
    } else {
        let caller_cfp = cfp.prev().unwrap();
        let fid = globals.compile_script_eval(expr, "(eval)", caller_cfp)?;
        let proc = ProcInner::from(caller_cfp.lfp(), fid);
        vm.invoke_block(globals, &proc, &[])
    }
}

fn prepare_command_arg(input: &str) -> (String, Vec<String>) {
    let mut args = vec![];
    let include_meta = input.contains([
        '*', '?', '{', '}', '[', ']', '<', '>', '(', ')', '~', '&', '|', '\\', '$', ';', '\'',
        '\"', '`', '\n',
    ]);
    let program = if include_meta {
        args.push(if cfg!(windows) { "/C" } else { "-c" }.to_string());
        args.push(input.to_string());
        if cfg!(windows) {
            "cmd"
        } else {
            "sh"
        }
    } else {
        let input: Vec<&str> = input.split(' ').collect();
        input[1..].iter().for_each(|s| args.push(s.to_string()));
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
fn system(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    use std::process::Command;
    let arg0 = lfp.arg(0);
    let (program, mut args) = prepare_command_arg(&arg0.as_str());
    if let Some(arg1) = lfp.try_arg(1) {
        for v in arg1.as_array().iter() {
            args.push(v.expect_string()?);
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
fn command(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg0 = lfp.arg(0);
    let (program, args) = prepare_command_arg(&arg0.as_str());
    match std::process::Command::new(program).args(&args).output() {
        Ok(output) => {
            std::io::stderr().write_all(&output.stderr).unwrap();
            Ok(Value::string_from_vec(output.stdout))
        }
        Err(err) => Err(MonorubyErr::runtimeerr(format!("{}", err))),
    }
}

///
/// Kernel.#sleep
///
/// - sleep -> Integer
/// - sleep(sec) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/abort.htmll]
#[monoruby_builtin]
fn sleep(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let now = std::time::Instant::now();
    if let Some(sec) = lfp.try_arg(0) {
        let sec = sec.coerce_to_f64()?;
        std::thread::sleep(std::time::Duration::from_secs_f64(sec));
    } else {
        loop {
            std::thread::sleep(std::time::Duration::from_secs(100));
        }
    }
    let elapsed = now.elapsed().as_secs();
    Ok(Value::integer(elapsed as i64))
}

///
/// Kernel.#abort
///
/// - abort -> ()
/// - abort(message) -> ()
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/abort.htmll]
#[monoruby_builtin]
fn abort(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    if let Some(arg0) = lfp.try_arg(0) {
        match arg0.is_str() {
            Some(s) => eprintln!("{}", s),
            None => {
                return Err(MonorubyErr::no_implicit_conversion(arg0, STRING_CLASS));
            }
        }
    }
    std::process::exit(1);
}

///
/// Kernel.#exit
///
/// - exit(status = true) -> ()
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/exit.html]
#[monoruby_builtin]
fn exit(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    if let Some(arg0) = lfp.try_arg(0) {
        if let Some(i) = arg0.try_fixnum() {
            std::process::exit(i as i32);
        } else {
            match arg0.as_bool() {
                true => std::process::exit(0),
                false => std::process::exit(1),
            }
        }
    } else {
        std::process::exit(0);
    }
}

///
/// Kernel.#warn
///
/// - warn(*message, [NOT SUPPORTED] uplevel: nil, [NOT SUPPORTED] category: nil) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/warn.html]
#[monoruby_builtin]
fn warn(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let message = lfp.arg(0);
    if lfp.try_arg(1).is_some() {
        return Err(MonorubyErr::runtimeerr("uplevel is not supported"));
    }
    if lfp.try_arg(2).is_some() {
        return Err(MonorubyErr::runtimeerr("category is not supported"));
    }
    if let Some(ary) = message.try_array_ty() {
        for m in ary.iter() {
            if let Some(s) = m.is_str() {
                eprintln!("{}", s);
            } else {
                eprintln!("{}", m.to_s(globals));
            }
        }
    } else {
        if let Some(s) = message.is_str() {
            eprintln!("{}", s);
        } else {
            eprintln!("{}", message.to_s(globals));
        }
    }
    Ok(Value::nil())
}

///
/// Kernel.#__dir__
///
/// - __dir__ -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/__dir__.html]
#[monoruby_builtin]
fn dir_(vm: &mut Executor, globals: &mut Globals, _lfp: Lfp) -> Result<Value> {
    let path = globals.current_source_path(vm).parent().unwrap();
    Ok(Value::string(path.to_string_lossy().to_string()))
}

///
/// Kernel.#___dlopen
///
/// - dlopen(lib) -> Fiddle::Handle
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiddle/m/dlopen.html]
#[monoruby_builtin]
fn dlopen(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    // see: https://github.com/ruby/fiddle/blob/2b3747e919df5d044c835cbbb27ebf9e27df74f9/ext/fiddle/handle.c#L136
    let arg0 = lfp.arg(0);
    let lib = if let Some(s) = arg0.try_bytes() {
        s.as_bytes().to_vec()
    } else if arg0.is_nil() {
        let handle = unsafe { libc::dlopen(0 as _, libc::RTLD_LAZY) };
        return Ok(Value::integer(handle as usize as i64));
    } else {
        vm.invoke_method_inner(globals, IdentId::get_id("to_str"), arg0, &[], None)?
            .expect_string()?
            .into_bytes()
    };
    let lib = std::ffi::CString::new(lib).unwrap();
    let handle = unsafe { libc::dlopen(lib.as_ptr(), libc::RTLD_LAZY) };
    Ok(Value::integer(handle as usize as i64))
}

///
/// Kernel.#___dlsym
///
/// - dlsym(handle, name) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiddle=3a=3aHandle/s/=5b=5d.html]
#[monoruby_builtin]
fn dlsym(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    // see: https://github.com/ruby/fiddle/blob/2b3747e919df5d044c835cbbb27ebf9e27df74f9/ext/fiddle/handle.c#L136
    let arg0 = if lfp.arg(0).is_nil() {
        0
    } else {
        lfp.arg(0).expect_integer()?
    };
    let arg1 = lfp.arg(1).expect_string()?;
    let name = std::ffi::CString::new(arg1).unwrap();
    let handle = unsafe { libc::dlsym(arg0 as _, name.as_ptr()) };
    Ok(Value::integer(handle as usize as i64))
}

///
/// Kernel.#___call
///
/// - call(ptr, args) -> Integer
///
#[monoruby_builtin]
fn dlcall(_vm: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    fn conv(arg: Value, ty: u32) -> Result<u64> {
        // VOID = 0
        // VOIDP = 1
        // CHAR = 2
        // UCHAR = -2
        // INT = 4
        // UINT = -4
        match ty {
            0 => Ok(0u64),
            1 => Ok(arg.as_bytes().as_ptr() as u64),
            2 => Ok(arg.expect_integer()? as i8 as u8 as u64),
            4 => Ok(arg.expect_integer()? as i32 as u32 as u64),
            _ => Err(MonorubyErr::runtimeerr("not supported")),
        }
    }
    let ptr = lfp.arg(0).expect_integer()? as usize;
    let f: extern "C" fn(u64, u64) -> u64 = unsafe { transmute(ptr) };
    let args = lfp.arg(1).expect_array()?;
    let args_type = lfp.arg(2).expect_array()?;
    let ret_type = lfp.arg(3).expect_integer()?;
    let a1 = conv(args[0], args_type[0].expect_integer()? as u32)?;
    let a2 = conv(args[1], args_type[1].expect_integer()? as u32)?;
    let res = f(a1, a2);
    let res = match ret_type {
        // VOID = 0
        // VOIDP = 1
        // CHAR = 2
        // UCHAR = -2
        // INT = 4
        // UINT = -4
        0 => Value::nil(),
        1 => Value::integer(res as i64),
        2 => Value::integer(res as i64),
        4 => Value::integer(res as i64),
        _ => return Err(MonorubyErr::runtimeerr("not supported")),
    };
    Ok(res)
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn eval() {
        run_test(r##"eval "1+2+3""##);
        run_test(
            r##"
        res = []
        5.times do |a|
            5.times do |b|
                eval "res << a; res << b"
            end
        end
        res
        "##,
        );
        run_test_error(r##"eval "1/0""##);
        run_test_error(r##"eval "jk""##);
    }

    #[test]
    fn eval_binding() {
        run_test(
            r##"
        $res = []
        def f(x)
          binding
        end
        b = f(10)
        x = 20
        $res << eval("x")
        $res << eval("x", b)
        eval("a=42", b)
        $res << eval("x + a", b)
        $res
        "##,
        );
    }

    #[test]
    fn float() {
        run_test(r#"Float(0.0)"#);
        run_test(r#"Float(1)"#);
        run_test(r#"Float(1000000000000000000000000000000000000000000000000000000000000)"#);
        run_test(r#"Float('10.0')"#);
        run_test(r#"Float('   10.0')"#);
        run_test(r#"Float(' -0.7e-10')"#);
        run_test_error(r#"Float(' -0.7 5')"#);
        run_test_error(r#"Float(' -0.7e-10z')"#);
    }

    #[test]
    fn array() {
        run_test(r#"Array([100])"#);
        run_test(r#"Array(100)"#);
        run_test(r#"Array("100")"#);
        run_test_with_prelude(
            r#"
            Array(C.new(3))
            "#,
            r#"
            class C
              def initialize(x)
                @x=x
              end
              def to_a
                [@x,@x]
              end
              def to_ary
                [@x,@x,@x]
              end
            end
            "#,
        );
    }

    #[test]
    fn kernel() {
        run_test_no_result_check("sleep 1");
        run_test_error("abort 1");
        run_test_no_result_check("exit");
        run_test_no_result_check("exit 0");
        run_test_no_result_check("__dir__");
    }

    #[test]
    fn lambda() {
        run_test("proc {|x| x * 2}.call(1)");
        run_test("proc {|x, y| x * y}.call(7,2)");
        run_test("proc {|x, y| [x, y]}.call(7)");
        run_test("proc {|x, y| [x, y]}.call(7, 5)");
        run_test("proc {|x, y| [x, y]}.call(7, 5, 15)");
        run_test("lambda {|x| x * 2}.call(1)");
        run_test("lambda {|x, y| x * y}.call(7,2)");
        run_test_error("lambda {|x| puts x}.call(1,2)");
    }

    #[test]
    fn warn() {
        run_test(r#"warn("woo")"#);
        run_test(r#"warn("woo", :boo, 100)"#);
        run_test_error(r#"warn(uplevel:1)"#);
        run_test_error(r#"warn(category:100)"#);
    }
}
