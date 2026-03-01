use super::*;
use jitgen::JitContext;
use num::ToPrimitive;
use num::Zero;
use std::{io::Write, mem::transmute};

//
// Kernel module
//

pub(super) fn init(globals: &mut Globals) -> Module {
    let klass = globals.define_toplevel_module("Kernel");
    let kernel_class = klass.id();
    globals.define_builtin_inline_func(kernel_class, "nil?", nil, Box::new(kernel_nil), 0);
    globals.define_builtin_module_func_rest(kernel_class, "puts", puts);
    globals.define_builtin_module_func(kernel_class, "gets", gets, 0);
    globals.define_builtin_module_func_rest(kernel_class, "print", print);
    globals.define_builtin_module_func_with_effect(
        kernel_class,
        "proc",
        proc,
        0,
        0,
        false,
        Effect::CAPTURE,
    );
    globals.define_builtin_module_func_with_effect(
        kernel_class,
        "lambda",
        lambda,
        0,
        0,
        false,
        Effect::CAPTURE,
    );
    globals.define_builtin_module_func_with_effect(
        kernel_class,
        "binding",
        binding,
        0,
        0,
        false,
        Effect::CAPTURE | Effect::BINDING,
    );
    globals.define_builtin_module_func(kernel_class, "loop", loop_, 0);
    globals.define_builtin_module_func_with(kernel_class, "raise", raise, 0, 2, false);
    globals.define_builtin_module_func_with(kernel_class, "fail", raise, 0, 2, false);
    globals.define_builtin_module_inline_func(
        kernel_class,
        "block_given?",
        block_given,
        Box::new(kernel_block_given),
        0,
    );
    globals.define_builtin_module_func_rest(kernel_class, "p", p);
    globals.define_builtin_module_func_with(kernel_class, "rand", rand, 0, 1, false);
    globals.define_builtin_module_func(kernel_class, "Integer", kernel_integer, 1);
    globals.define_builtin_module_func(kernel_class, "Float", kernel_float, 1);
    globals.define_builtin_module_func_with(kernel_class, "Complex", kernel_complex, 1, 2, false);
    globals.define_builtin_module_func_with(kernel_class, "Array", kernel_array, 1, 1, false);
    globals.define_builtin_module_func(kernel_class, "require", require, 1);
    globals.define_builtin_module_func(kernel_class, "require_relative", require_relative, 1);
    globals.define_builtin_module_func_with(kernel_class, "load", load_, 1, 2, false);
    globals.define_builtin_module_func(kernel_class, "autoload", autoload, 2);
    globals.define_builtin_module_func_with_effect(
        kernel_class,
        "eval",
        eval,
        1,
        4,
        false,
        Effect::EVAL,
    );
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
        false,
    );
    globals.define_builtin_module_func(kernel_class, "__dir__", dir_, 0);
    globals.define_builtin_module_func(kernel_class, "__method__", method_, 0);
    globals.define_builtin_func(kernel_class, "__assert", assert, 2);
    globals.define_builtin_func_with(kernel_class, "caller", caller, 0, 1, false);
    globals.define_builtin_func(kernel_class, "__dump", dump, 0);
    globals.define_builtin_func(kernel_class, "__check_stack", check_stack, 0);
    globals.define_builtin_func(kernel_class, "__instance_ty", instance_ty, 0);
    globals.define_builtin_func(
        kernel_class,
        "__enum_yield",
        super::enumerator::yielder_yield,
        1,
    );

    globals.define_builtin_module_func_with(kernel_class, "___dlopen", dlopen, 1, 2, false);
    globals.define_builtin_module_func(kernel_class, "___dlsym", dlsym, 2);
    globals.define_builtin_module_func(kernel_class, "___call", dlcall, 4);
    globals.define_builtin_module_func_with(kernel_class, "___malloc", malloc, 1, 2, false);
    globals.define_builtin_module_func(kernel_class, "___memcpyv", memcpyv, 3);
    globals.define_builtin_module_func(kernel_class, "___read_memory", read_memory, 2);

    //globals.define_builtin_inline_func(kernel_class, "____max", max, Box::new(inline_max), 2);
    //globals.define_builtin_inline_func(kernel_class, "____min", min, Box::new(inline_min), 2);

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

fn kernel_nil(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo { recv, dst, .. } = *callsite;
    if state.is_nil(recv) {
        if let Some(dst) = dst {
            state.def_C(dst, Value::bool(true));
        }
    } else if state.is_not_nil(recv) {
        if let Some(dst) = dst {
            state.def_C(dst, Value::bool(false));
        }
    } else {
        state.load(ir, recv, GP::Rdi);
        ir.inline(|r#gen, _, _| {
            monoasm! { &mut r#gen.jit,
                movq rax, (FALSE_VALUE);
                movq rsi, (TRUE_VALUE);
                cmpq rdi, (NIL_VALUE);
                cmoveqq rax, rsi;
            }
        });
        state.def_rax2acc(ir, dst);
    }
    true
}

fn kernel_block_given(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    jitctx: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let dst = callsite.dst;
    if let Some(callid) = jitctx.method_caller_callsite()
        && let Some(b) = store[callid].block_given()
    {
        if let Some(dst) = dst {
            state.def_C(dst, Value::bool(b));
        }
    } else {
        ir.inline(|r#gen, _, _| {
            let exit = r#gen.jit.label();
            monoasm! { &mut r#gen.jit,
                movq rax, (FALSE_VALUE);
                movq rdi, [r14 - (LFP_BLOCK)];
                testq rdi, rdi;
                jz exit;
                cmpq rdi, (NIL_VALUE);
                jeq exit;
                movq rax, (TRUE_VALUE);
            exit:
            }
        });
        state.def_rax2acc(ir, dst);
    }

    true
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
/// ### Kernel.#gets
///
/// - gets([NOT SUPPORTED]rs = $/) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/gets.html]
#[monoruby_builtin]
fn gets(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp) -> Result<Value> {
    let mut buffer = String::new();
    match std::io::stdin().read_line(&mut buffer) {
        Ok(_) => {}
        Err(_) => return Ok(Value::nil()),
    }
    Ok(Value::string(buffer))
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
fn proc(vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        let p = vm.generate_proc(bh)?;
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
        let p = vm.generate_proc(bh)?;
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
/// - raise(message, [NOT SUPPORTED]cause: $!) -> ()
/// - fail(message, [NOT SUPPORTED]cause: $!) -> ()
/// - raise(error_type, message = nil, [NOT SUPPORTED] backtrace = caller(0), [NOT SUPPORTED] cause: $!) -> ()
/// - fail(error_type, message = nil, [NOT SUPPORTED] backtrace = caller(0), [NOT SUPPORTED] cause: $!) -> ()
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/fail.html]
#[monoruby_builtin]
fn raise(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    if lfp.try_arg(0).is_none() {
        let ex = globals.get_gvar(IdentId::get_id("$!")).unwrap_or_default();
        return Err(MonorubyErr::new_from_exception(ex.is_exception().unwrap()));
    }
    if let Some(ex) = lfp.arg(0).is_exception() {
        let mut err = MonorubyErr::new_from_exception(ex);
        if let Some(arg1) = lfp.try_arg(1) {
            err.set_msg(arg1.expect_string(globals)?);
        }
        return Err(err);
    } else if let Some(klass) = lfp.arg(0).is_class() {
        if klass.id() == STOP_ITERATION_CLASS {
            return Err(MonorubyErr::stopiterationerr("".to_string()));
        } else if klass.is_exception() {
            let ex =
                vm.invoke_method_inner(globals, IdentId::NEW, klass.as_val(), &[], None, None)?;
            let mut err = MonorubyErr::new_from_exception(ex.is_exception().unwrap());
            if let Some(arg1) = lfp.try_arg(1) {
                err.set_msg(arg1.expect_string(globals)?);
            }
            return Err(err);
        }
    } else if let Some(message) = lfp.arg(0).is_rstring() {
        return Err(MonorubyErr::runtimeerr(message.to_str()?));
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
    for v in lfp.arg(0).as_array().iter() {
        buf += &v.inspect(&globals.store);
        buf += "\n";
    }
    globals.write_stdout(buf.as_bytes());
    globals.flush_stdout();
    Ok(match len {
        0 => Value::nil(),
        1 => lfp.arg(0).as_array()[0],
        _ => lfp.arg(0),
    })
}

///
/// ### Kernel.#caller
///
/// - caller(start = 1) -> [String] | nil
/// - [NOT SUPPORTED] caller(start, length) -> [String] | nil
/// - [NOT SUPPORTED] caller(range) -> [String] | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/caller.html]
#[monoruby_builtin]
fn caller(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut cfp = vm.cfp();
    let mut v = Vec::new();
    let level = if let Some(arg0) = lfp.try_arg(0) {
        arg0.coerce_to_i64(globals)? as usize + 1
    } else {
        2
    };
    for i in 0..16 {
        let prev_cfp = cfp.prev();
        if i >= level {
            let func_id = cfp.lfp().func_id();
            if let Some(iseq) = globals.store[func_id].is_iseq() {
                let loc = globals.store[iseq].get_location();
                let desc = globals.store.func_description(func_id);
                v.push(Value::string_from_vec(
                    format!("{loc}:in `{desc}`").into_bytes(),
                ));
            }
        }
        if let Some(prev_cfp) = prev_cfp {
            cfp = prev_cfp;
        } else {
            break;
        }
    }
    Ok(Value::array_from_vec(v))
}

#[monoruby_builtin]
fn assert(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let expected = lfp.arg(0);
    let actual = lfp.arg(1);
    eprintln!(
        "expected:{}\nactual  :{}",
        expected.inspect(&globals.store),
        actual.inspect(&globals.store)
    );
    Value::assert_eq(globals, expected, actual);
    Ok(Value::nil())
}

#[monoruby_builtin]
fn dump(vm: &mut Executor, globals: &mut Globals, _lfp: Lfp) -> Result<Value> {
    crate::runtime::_dump_stacktrace(vm, globals);
    Ok(Value::nil())
}

#[monoruby_builtin]
fn check_stack(vm: &mut Executor, globals: &mut Globals, _lfp: Lfp) -> Result<Value> {
    if crate::runtime::_check_stack(vm, globals) {
        crate::runtime::_dump_stacktrace(vm, globals);
    }
    Ok(Value::nil())
}

#[monoruby_builtin]
fn instance_ty(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class_id = lfp.self_val().class();
    let i = if let Some(ty) = globals.store[class_id].instance_ty() {
        ty.get()
    } else {
        0
    };
    Ok(Value::integer(i as _))
}

///
/// ### Kernel.#rand
///
/// - rand(max = 0) -> Integer | Float
/// - rand(range) -> Integer | Float | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/rand.html]
#[monoruby_builtin]
fn rand(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let i = if let Some(arg0) = lfp.try_arg(0) {
        if let Some(range) = arg0.is_range() {
            let start = range.start();
            let end = range.end();
            let start = start.expect_integer(globals)?;
            let end = end.expect_integer(globals)?;
            if start > end {
                return Ok(Value::nil());
            }
            return Ok(Value::integer(
                (rand::random::<f64>() * (end - start) as f64 + start as f64) as i64,
            ));
        }
        arg0.coerce_to_i64(globals)?
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
                let s = arg0.to_s(&globals.store);
                return Err(MonorubyErr::argumenterr(format!(
                    "invalid value for Integer(): {}",
                    s
                )));
            }
        },
        _ => {}
    };
    Err(MonorubyErr::no_implicit_conversion(
        globals,
        arg0,
        INTEGER_CLASS,
    ))
}

///
/// ### Kernel.#Float
///
/// - Float(arg, [NOT SUPPORTED] exception: true) -> Float | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/Float.html]
#[monoruby_builtin]
fn kernel_float(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
    Err(MonorubyErr::no_implicit_conversion(
        globals,
        arg0,
        FLOAT_CLASS,
    ))
}

///
/// ### Kernel.#Complex
///
/// - Complex(r, i = 0, [NOT SUPPORTED] exception: true) -> Complex | nil
/// - [NOT SUPPORTED] Complex(s, exception: true) -> Complex | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/Complex.html]
#[monoruby_builtin]
fn kernel_complex(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let r = Real::try_from(globals, lfp.arg(0))?;
    let i = if let Some(i) = lfp.try_arg(1) {
        Real::try_from(globals, i)?
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
        return vm.invoke_func_inner(globals, func_id, arg, &[], None, None);
    } else if let Some(func_id) = globals.check_method(arg, IdentId::TO_A) {
        return vm.invoke_func_inner(globals, func_id, arg, &[], None, None);
    };
    if arg.is_nil() {
        Ok(Value::array_empty())
    } else {
        Ok(Value::array1(arg))
    }
}

///
/// ### Kernel.#require
///
/// - require(feature) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/require.html]
#[monoruby_builtin]
fn require(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let feature = lfp.arg(0).expect_string(globals)?;
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
    let feature = std::path::PathBuf::from(lfp.arg(0).expect_string(globals)?);
    file_name.extend(&feature);
    file_name.set_extension("rb");
    let b = vm.require(globals, &file_name, true)?;
    Ok(Value::bool(b))
}

///
/// ### Kernel.#load
///
/// - load(filename, wrap=false) -> true
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/load.html]
#[monoruby_builtin]
fn load_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let file_name = std::path::PathBuf::from(lfp.arg(0).expect_string(globals)?);
    let wrap = lfp
        .try_arg(1)
        .map(|v| v.as_bool())
        .unwrap_or(false);
    vm.load(globals, &file_name, wrap)?;
    Ok(Value::bool(true))
}

///
/// ### Kernel.#autoload
///
/// - autoload(const_name, feature) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/autoload.html]
#[monoruby_builtin]
fn autoload(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let const_name = lfp.arg(0).expect_symbol_or_string(globals)?;
    let feature = lfp.arg(1).coerce_to_string(vm, globals)?;
    globals
        .store
        .set_constant_autoload(vm.context_class_id(), const_name, feature);
    Ok(Value::nil())
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
    let expr = lfp.arg(0).expect_string(globals)?;
    let cfp = vm.cfp();
    if let Some(bind) = lfp.try_arg(1)
        && !bind.is_nil()
    {
        let binding = if let Some(b) = Binding::try_new(bind) {
            b
        } else {
            return Err(MonorubyErr::typeerr("Binding expected"));
        };
        globals.compile_script_binding(expr, "(eval)", binding)?;
        vm.invoke_binding(globals, binding.binding().unwrap())
    } else {
        let caller_cfp = cfp.prev().unwrap();
        let fid = globals.compile_script_eval(expr, "(eval)", caller_cfp)?;
        let proc = ProcData::new(caller_cfp.lfp(), fid);
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
        if cfg!(windows) { "cmd" } else { "sh" }
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
fn system(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    use std::process::Command;
    let arg0 = lfp.arg(0);
    let (program, mut args) = prepare_command_arg(arg0.as_str());
    if let Some(arg1) = lfp.try_arg(1) {
        for v in arg1.as_array().iter() {
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
fn command(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg0 = lfp.arg(0);
    let (program, args) = prepare_command_arg(arg0.as_str());
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
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/sleep.html]
#[monoruby_builtin]
fn sleep(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let now = std::time::Instant::now();
    if let Some(sec) = lfp.try_arg(0) {
        let sec = sec.coerce_to_f64(globals)?;
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
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/abort.html]
#[monoruby_builtin]
fn abort(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    if let Some(arg0) = lfp.try_arg(0) {
        match arg0.is_str() {
            Some(s) => eprintln!("{}", s),
            None => {
                return Err(MonorubyErr::no_implicit_conversion(
                    globals,
                    arg0,
                    STRING_CLASS,
                ));
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
                eprintln!("{}", m.to_s(&globals.store));
            }
        }
    } else if let Some(s) = message.is_str() {
        eprintln!("{}", s);
    } else {
        eprintln!("{}", message.to_s(&globals.store));
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
/// Kernel.#__method__
///
/// - __method__ -> Symbol | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/__method__.html]
#[monoruby_builtin]
fn method_(vm: &mut Executor, globals: &mut Globals, _lfp: Lfp) -> Result<Value> {
    let fid = vm.cfp().prev().unwrap().method_func_id();
    if !globals.store[fid].is_method() {
        return Ok(Value::nil());
    }
    globals.store[fid]
        .name()
        .map_or(Ok(Value::nil()), |sym| Ok(Value::symbol(sym)))
}

///
/// Kernel.#___dlopen
///
/// - dlopen(lib, flag = RTLD_LAZY) -> Fiddle::Handle
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiddle/m/dlopen.html]
#[monoruby_builtin]
fn dlopen(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    // see: https://github.com/ruby/fiddle/blob/2b3747e919df5d044c835cbbb27ebf9e27df74f9/ext/fiddle/handle.c#L136
    let arg0 = lfp.arg(0);
    let flags = if let Some(arg1) = lfp.try_arg(1) {
        match i32::try_from(arg1.expect_integer(globals)?) {
            Ok(f) => f,
            Err(_) => return Err(MonorubyErr::argumenterr("Illegale flag value.")),
        }
    } else {
        libc::RTLD_LAZY
    };
    let lib = if arg0.is_nil() {
        None
    } else {
        Some(std::ffi::CString::new(arg0.coerce_to_string(vm, globals)?).unwrap())
    };
    let handle = unsafe {
        libc::dlopen(
            match &lib {
                Some(lib) => lib.as_ptr(),
                None => 0 as _,
            },
            flags,
        )
    };
    if handle.is_null() {
        let err = unsafe { libc::dlerror() };
        if err.is_null() {
            return Err(MonorubyErr::argumenterr(format!(
                "both of dlopen() and dlerror() returned NULL. {:?}",
                lib
            )));
        }
        let message = { unsafe { std::ffi::CStr::from_ptr(err).to_str().unwrap().to_string() } };
        let path = match lib {
            Some(lib) => lib.to_string_lossy().to_string(),
            None => "".to_string(),
        };
        return Err(MonorubyErr::loaderr(
            message,
            std::path::PathBuf::from(path),
        ));
    }
    Ok(Value::integer(handle as usize as i64))
}

///
/// Kernel.#___dlsym
///
/// - dlsym(handle, name) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiddle=3a=3aHandle/s/=5b=5d.html]
#[monoruby_builtin]
fn dlsym(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    // see: https://github.com/ruby/fiddle/blob/2b3747e919df5d044c835cbbb27ebf9e27df74f9/ext/fiddle/handle.c#L136
    let arg0 = if lfp.arg(0).is_nil() {
        0
    } else {
        lfp.arg(0).expect_integer(globals)?
    };
    let arg1 = lfp.arg(1).expect_string(globals)?;
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
fn dlcall(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    fn conv(store: &Store, arg: Value, ty: u32) -> Result<u64> {
        // VOID = 0
        // VOIDP = 1
        // CHAR = 2
        // UCHAR = -2
        // INT = 4
        // UINT = -4
        match ty {
            0 => Ok(0u64),
            1 => Ok(arg.as_rstring_inner().as_ptr() as u64),
            2 => Ok(arg.expect_integer(store)? as i8 as u8 as u64),
            4 => Ok(arg.expect_integer(store)? as i32 as u32 as u64),
            _ => Err(MonorubyErr::runtimeerr("not supported")),
        }
    }
    let ptr = lfp.arg(0).expect_integer(globals)? as usize;
    let args = lfp.arg(1).expect_array_ty(globals)?;
    let args_type = lfp.arg(2).expect_array_ty(globals)?;
    let ret_type = lfp.arg(3).expect_integer(globals)?;
    let res = match args.len() {
        0 => {
            let f: extern "C" fn() -> u64 = unsafe { transmute(ptr) };
            f()
        }
        1 => {
            let f: extern "C" fn(u64) -> u64 = unsafe { transmute(ptr) };
            let a0 = conv(
                globals,
                args[0],
                args_type[0].expect_integer(globals)? as u32,
            )?;
            f(a0)
        }
        2 => {
            let f: extern "C" fn(u64, u64) -> u64 = unsafe { transmute(ptr) };
            let a0 = conv(
                globals,
                args[0],
                args_type[0].expect_integer(globals)? as u32,
            )?;
            let a1 = conv(
                globals,
                args[1],
                args_type[1].expect_integer(globals)? as u32,
            )?;
            f(a0, a1)
        }
        3 => {
            let f: extern "C" fn(u64, u64, u64) -> u64 = unsafe { transmute(ptr) };
            let a0 = conv(
                globals,
                args[0],
                args_type[0].expect_integer(globals)? as u32,
            )?;
            let a1 = conv(
                globals,
                args[1],
                args_type[1].expect_integer(globals)? as u32,
            )?;
            let a2 = conv(
                globals,
                args[2],
                args_type[2].expect_integer(globals)? as u32,
            )?;
            f(a0, a1, a2)
        }
        4 => {
            let f: extern "C" fn(u64, u64, u64, u64) -> u64 = unsafe { transmute(ptr) };
            let a0 = conv(
                globals,
                args[0],
                args_type[0].expect_integer(globals)? as u32,
            )?;
            let a1 = conv(
                globals,
                args[1],
                args_type[1].expect_integer(globals)? as u32,
            )?;
            let a2 = conv(
                globals,
                args[2],
                args_type[2].expect_integer(globals)? as u32,
            )?;
            let a3 = conv(
                globals,
                args[3],
                args_type[3].expect_integer(globals)? as u32,
            )?;
            f(a0, a1, a2, a3)
        }
        5 => {
            let f: extern "C" fn(u64, u64, u64, u64, u64) -> u64 = unsafe { transmute(ptr) };
            let a0 = conv(
                globals,
                args[0],
                args_type[0].expect_integer(globals)? as u32,
            )?;
            let a1 = conv(
                globals,
                args[1],
                args_type[1].expect_integer(globals)? as u32,
            )?;
            let a2 = conv(
                globals,
                args[2],
                args_type[2].expect_integer(globals)? as u32,
            )?;
            let a3 = conv(
                globals,
                args[3],
                args_type[3].expect_integer(globals)? as u32,
            )?;
            let a4 = conv(
                globals,
                args[4],
                args_type[4].expect_integer(globals)? as u32,
            )?;
            f(a0, a1, a2, a3, a4)
        }
        x => {
            return Err(MonorubyErr::argumenterr(format!(
                "arguments too many: {}",
                x
            )));
        }
    };
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

///
/// Kernel.#___malloc
///
/// - malloc(size, clear) -> Integer
///
#[monoruby_builtin]
fn malloc(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let size = lfp.arg(0).expect_integer(globals)? as usize;
    let clear = if let Some(clear) = lfp.try_arg(1) {
        clear.as_bool()
    } else {
        false
    };
    let handle = unsafe {
        if clear {
            libc::calloc(1, size)
        } else {
            libc::malloc(size)
        }
    };
    if handle.is_null() {
        return Err(MonorubyErr::runtimeerr(
            "failed to allocate memory size = {size}",
        ));
    }
    Ok(Value::integer(handle as usize as i64))
}

///
/// Kernel.#___memcpy
///
/// - memcpyv(dst, value, size)
///
#[monoruby_builtin]
fn memcpyv(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let dest = lfp.arg(0).expect_integer(globals)? as *mut u8;
    let value = lfp.arg(1).expect_integer(globals)?;
    let n = lfp.arg(2).expect_integer(globals)? as usize;
    unsafe { libc::memcpy(dest as _, &value as *const i64 as _, n) };
    Ok(lfp.arg(0))
}

///
/// Kernel.#___read_memory
///
/// - read_memory(ptr, length)
///
#[monoruby_builtin]
fn read_memory(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ptr = lfp.arg(0).expect_integer(globals)? as *mut u8;
    let len = lfp.arg(1).expect_integer(globals)? as usize;
    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
    let ary = Value::bytes_from_slice(slice);
    Ok(ary)
}

/*///
/// - ____max(f1:Float, f2:Float) -> Float
///
#[monoruby_builtin]
fn max(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let f1 = lfp.arg(0).coerce_to_f64(globals)?;
    let f2 = lfp.arg(1).coerce_to_f64(globals)?;
    Ok(Value::float(f64::max(f1, f2)))
}

///
/// - ____min(f1:Float, f2:Float) -> Float
///
#[monoruby_builtin]
fn min(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let f1 = lfp.arg(0).coerce_to_f64(globals)?;
    let f2 = lfp.arg(1).coerce_to_f64(globals)?;
    Ok(Value::float(f64::min(f1, f2)))
}

fn inline_max(
    bb: &mut BBContext,
    ir: &mut AsmIr,
    _: &JitContext,
    _: &Store,
    callsite: &CallSiteInfo,
    _: ClassId,
) -> bool {
    let CallSiteInfo {
        args, dst, pos_num, ..
    } = *callsite;
    if !callsite.is_simple() || pos_num != 2 {
        return false;
    }
    let deopt = ir.new_deopt(bb);
    let f1 = bb.fetch_float_for_xmm(ir, args, deopt).enc();
    let f2 = bb.fetch_float_for_xmm(ir, args + 1usize, deopt).enc();
    if let Some(dst) = dst {
        let fret = bb.xmm_write_enc(dst);
        ir.inline(move |gen, _, _| {
            monoasm!( &mut gen.jit,
                movq xmm(fret), xmm(f1);
                maxsd xmm(fret), xmm(f2);
            );
        });
    }
    true
}

fn inline_min(
    bb: &mut BBContext,
    ir: &mut AsmIr,
    _: &JitContext,
    _: &Store,
    callsite: &CallSiteInfo,
    _: ClassId,
) -> bool {
    let CallSiteInfo {
        args, dst, pos_num, ..
    } = *callsite;
    if !callsite.is_simple() || pos_num != 2 {
        return false;
    }
    let deopt = ir.new_deopt(bb);
    let f1 = bb.fetch_float_for_xmm(ir, args, deopt).enc();
    let f2 = bb.fetch_float_for_xmm(ir, args + 1usize, deopt).enc();
    if let Some(dst) = dst {
        let fret = bb.xmm_write_enc(dst);
        ir.inline(move |gen, _, _| {
            monoasm!( &mut gen.jit,
                movq xmm(fret), xmm(f1);
                minsd xmm(fret), xmm(f2);
            );
        });
    }
    true
}*/

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn nil() {
        run_test(r##"'woo'.nil?"##);
        run_test(r##"3.nil?"##);
        run_test(r##":x.nil?"##);
        run_test(r##"nil.nil?"##);
        run_test(r##"false.nil?"##);
    }

    #[test]
    fn block_given() {
        run_test_with_prelude(
            r##"
        $x = []
        p = Proc.new {}
        f {}
        f(&p)
        f
        $x
            "##,
            r##"
        def f
          $x << block_given?
        end
        "##,
        );
    }

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
        run_test_error(r#"Float(:Ruby)"#);
    }

    #[test]
    fn complex() {
        run_test(r#"Complex(30)"#);
        run_test(r#"Complex(30, 4.5)"#);
    }

    #[test]
    fn array() {
        run_test(r#"Array([100])"#);
        run_test(r#"Array(100)"#);
        run_test(r#"Array(nil)"#);
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
        run_test("system 'ls'");
        run_test("system 'ls', '-a'");
        run_test_error("abort 1");
        run_test_no_result_check("exit");
        run_test_no_result_check("exit 0");
        run_test_no_result_check("__dir__");
        run_test_no_result_check("caller(1)");
        run_test_no_result_check("caller()");
        run_test_no_result_check("rand");
        run_test_no_result_check("rand(0)");
        run_test_no_result_check("rand(100)");
        run_test_no_result_check("rand(0..100)");
        run_test_no_result_check("rand(100..0)");
    }

    #[test]
    fn kernel_method__() {
        run_test(
            r##"
        $res = []
        def foo
            __method__
        end
        alias :bar :foo
        $res << foo #=> :foo
        $res << bar #=> :foo
        $res << __method__ #=> nil
        $res
        "##,
        );
        run_test(
            r##"
        $res = []
        def foo
            3.times do
                $res << __method__
            end
        end
        foo
        $res
        "##,
        );
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
        run_test_error(r#"raise "Woo""#);
    }

    #[test]
    fn mem() {
        run_test_no_result_check(
            r##"
            ptr = ___malloc(32, true)
            ___memcpyv(ptr + 8, 0x12345678, 4)
            __assert(___read_memory(ptr, 32), "\x00\x00\x00\x00\x00\x00\x00\x00xV4\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00")
        "##,
        );
    }
}
