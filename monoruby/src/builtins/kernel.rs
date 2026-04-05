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
    globals.define_builtin_func(kernel_class, "!~", not_match, 1);
    //globals.define_builtin_module_func_rest(kernel_class, "puts", puts);
    globals.define_builtin_module_func(kernel_class, "gets", gets, 0);
    //globals.define_builtin_module_func_rest(kernel_class, "print", print);
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
    globals.define_builtin_module_func_with(kernel_class, "raise", raise, 0, 3, false);
    globals.define_builtin_module_func_with(kernel_class, "fail", raise, 0, 2, false);
    globals.define_builtin_module_inline_func(
        kernel_class,
        "block_given?",
        block_given,
        Box::new(kernel_block_given),
        0,
    );
    //globals.define_builtin_module_func_rest(kernel_class, "p", p);
    globals.define_builtin_module_func_rest(kernel_class, "format", format);
    globals.define_builtin_module_func_rest(kernel_class, "sprintf", format);
    globals.define_builtin_module_func_with(kernel_class, "rand", rand, 0, 1, false);
    globals.define_builtin_module_func_with(kernel_class, "Integer", kernel_integer, 1, 2, false);
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
    globals.define_builtin_module_func_rest(kernel_class, "exec", exec);
    globals.define_builtin_module_func(kernel_class, "`", command, 1);
    globals.define_builtin_module_func(kernel_class, "fork", fork, 0);
    globals.define_builtin_module_func_with(kernel_class, "sleep", sleep, 0, 1, false);
    globals.define_builtin_module_func_with(kernel_class, "abort", abort, 0, 1, false);
    globals.define_builtin_module_func_with(kernel_class, "exit", exit, 0, 1, false);
    globals.define_builtin_module_func_with(kernel_class, "exit!", exit_bang, 0, 1, false);
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

    // Kernel methods (matching CRuby: these are defined on Kernel, not Object)
    globals.define_builtin_func(kernel_class, "class", class, 0);
    globals.define_builtin_func(kernel_class, "hash", hash, 0);
    globals.define_builtin_func(kernel_class, "eql?", eql_, 1);
    globals.define_builtin_func(kernel_class, "dup", dup, 0);
    globals.define_builtin_func_with(kernel_class, "clone", clone_val, 0, 1, false);
    globals.define_private_builtin_func(kernel_class, "initialize_copy", initialize_copy, 1);
    globals.define_private_builtin_func(kernel_class, "initialize_clone", initialize_clone, 1);
    globals.define_private_builtin_func(kernel_class, "initialize_dup", initialize_clone, 1);
    globals.define_builtin_funcs_rest(kernel_class, "enum_for", &["to_enum"], to_enum);
    globals.define_builtin_func_rest(kernel_class, "extend", extend);
    globals.define_builtin_func(kernel_class, "kind_of?", is_a, 1);
    globals.define_builtin_inline_func(
        kernel_class,
        "object_id",
        super::object::object_id,
        Box::new(super::object::object_object_id),
        0,
    );
    globals.define_builtin_inline_func_with(
        kernel_class,
        "respond_to?",
        respond_to,
        Box::new(object_respond_to),
        1,
        2,
        false,
    );
    globals.define_builtin_func(kernel_class, "singleton_class", singleton_class, 0);
    globals.define_builtin_func(kernel_class, "to_s", to_s, 0);
    globals.define_builtin_func(kernel_class, "inspect", inspect, 0);
    globals.define_builtin_func(kernel_class, "instance_of?", instance_of, 1);
    globals.define_builtin_func(kernel_class, "instance_variable_defined?", iv_defined, 1);
    globals.define_builtin_func(kernel_class, "instance_variable_set", iv_set, 2);
    globals.define_builtin_func(kernel_class, "instance_variable_get", iv_get, 1);
    globals.define_builtin_func(kernel_class, "instance_variables", iv, 0);
    globals.define_builtin_func(kernel_class, "remove_instance_variable", iv_remove, 1);
    globals.define_builtin_func(kernel_class, "is_a?", is_a, 1);
    globals.define_builtin_inline_funcs_with_kw(
        kernel_class,
        "send",
        &["__send__", "public_send"],
        crate::builtins::send,
        Box::new(crate::builtins::object_send),
        0,
        0,
        true,
        &[],
        true,
    );
    globals.define_builtin_func(kernel_class, "method", method, 1);
    globals.define_builtin_func_with(
        kernel_class,
        "define_singleton_method",
        define_singleton_method,
        1,
        2,
        false,
    );
    globals.define_builtin_func_with(kernel_class, "methods", methods, 0, 1, false);
    globals.define_builtin_func_with(
        kernel_class,
        "singleton_methods",
        singleton_methods,
        0,
        1,
        false,
    );

    klass
}

///
/// ### Kernel#nil?
///
/// - nil? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/nil=3f.html]
#[monoruby_builtin]
fn nil(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().is_nil()))
}

fn kernel_nil(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    _: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo { recv, dst, .. } = *callsite;
    if state.is_nil(recv) {
        if let Some(dst) = dst {
            state.def_C(dst, Immediate::bool(true));
        }
    } else if state.is_not_nil(recv) {
        if let Some(dst) = dst {
            state.def_C(dst, Immediate::bool(false));
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

///
/// ### Kernel#!~
///
/// - self !~ (other) -> bool
///
#[monoruby_builtin]
fn not_match(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    let res = vm.invoke_method_inner(globals, IdentId::_MATCH, lhs, &[rhs], None, None)?;
    Ok(Value::bool(!res.as_bool()))
}

fn kernel_block_given(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    jitctx: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    _: Option<ClassId>,
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
            state.def_C(dst, Immediate::bool(b));
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
fn puts(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
        globals.print_value(v)?;
        globals.write_stdout(b"\n")?;
    }
    globals.flush_stdout()?;
    Ok(Value::nil())
}

///
/// ### Kernel.#gets
///
/// - gets([NOT SUPPORTED]rs = $/) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/gets.html]
#[monoruby_builtin]
fn gets(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn print(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    for v in lfp.arg(0).as_array().iter().cloned() {
        globals.print_value(v)?;
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
fn proc(vm: &mut Executor, _: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        let p = vm.generate_proc(bh, pc)?;
        Ok(p.into())
    } else {
        Err(MonorubyErr::create_proc_no_block())
    }
}

#[monoruby_builtin]
fn lambda(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        let func_id = bh.func_id();
        globals.store[func_id].set_method_style();
        let p = vm.generate_proc(bh, pc)?;
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
fn binding(vm: &mut Executor, _: &mut Globals, _: Lfp, pc: BytecodePtr) -> Result<Value> {
    Ok(vm.generate_binding(pc).as_val())
}

///
/// ### Kernel.#loop
///
/// - loop { ... } -> object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/loop.html]
#[monoruby_builtin]
fn loop_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn raise(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    if lfp.try_arg(0).is_none() {
        let ex = globals.get_gvar(IdentId::get_id("$!")).unwrap_or_default();
        if let Some(ex) = ex.is_exception() {
            return Err(MonorubyErr::new_from_exception(ex));
        } else {
            return Err(MonorubyErr::runtimeerr(""));
        }
    }
    if let Some(ex) = lfp.arg(0).is_exception() {
        let mut err = MonorubyErr::new_from_exception(ex);
        if let Some(arg1) = lfp.try_arg(1) {
            if arg1.try_hash_ty().is_none() {
                err.set_msg(arg1.coerce_to_str(vm, globals)?);
            }
        }
        return Err(err);
    } else if let Some(klass) = lfp.arg(0).is_class() {
        if klass.id() == STOP_ITERATION_CLASS {
            return Err(MonorubyErr::stopiterationerr("".to_string()));
        } else if klass.is_exception() {
            let mut args = vec![];
            if let Some(arg1) = lfp.try_arg(1) {
                if arg1.try_hash_ty().is_none() {
                    args.push(arg1);
                }
            }
            let ex =
                vm.invoke_method_inner(globals, IdentId::NEW, klass.as_val(), &args, None, None)?;
            let err = MonorubyErr::new_from_exception(ex.is_exception().unwrap());
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
fn block_given(vm: &mut Executor, _globals: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(vm.cfp().prev().unwrap().block_given()))
}

///
/// ### Kernel.#p
///
/// - p(*arg) -> object | Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/p.html]
#[monoruby_builtin]
fn p(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let len = lfp.arg(0).as_array().len();
    let mut buf = String::new();
    for v in lfp.arg(0).as_array().iter() {
        let inspected = vm.invoke_method_inner(globals, IdentId::INSPECT, *v, &[], None, None)?;
        buf += &inspected.to_s(&globals.store);
        buf += "\n";
    }
    globals.write_stdout(buf.as_bytes())?;
    globals.flush_stdout()?;
    Ok(match len {
        0 => Value::nil(),
        1 => lfp.arg(0).as_array()[0],
        _ => lfp.arg(0),
    })
}

///
/// ### Kernel.#format
///
/// - format(format_string, *args) -> String
/// - sprintf(format_string, *args) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/format.html]
#[monoruby_builtin]
fn format(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    if args.is_empty() {
        return Err(MonorubyErr::wrong_number_of_arg_min(0, 1));
    }
    let fmt = args[0].coerce_to_str(vm, globals)?;
    let arguments = &args[1..];
    let result = vm.format_by_args(globals, &fmt, arguments)?;
    Ok(Value::string(result))
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
fn caller(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut cfp = vm.cfp();
    let mut v = Vec::new();
    let level = if let Some(arg0) = lfp.try_arg(0) {
        arg0.coerce_to_int_i64(vm, globals)? as usize + 1
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
fn assert(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn dump(vm: &mut Executor, globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    crate::runtime::_dump_stacktrace(vm, globals);
    Ok(Value::nil())
}

#[monoruby_builtin]
fn check_stack(
    vm: &mut Executor,
    globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    if crate::runtime::_check_stack(vm, globals) {
        crate::runtime::_dump_stacktrace(vm, globals);
    }
    Ok(Value::nil())
}

#[monoruby_builtin]
fn instance_ty(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
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
fn rand(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let i = if let Some(arg0) = lfp.try_arg(0) {
        if let Some(range) = arg0.is_range() {
            let start = range.start();
            let end = range.end();
            let start = start.coerce_to_int_i64(vm, globals)?;
            let end = end.coerce_to_int_i64(vm, globals)?;
            if start > end {
                return Ok(Value::nil());
            }
            return Ok(Value::integer(
                (rand::random::<f64>() * (end - start) as f64 + start as f64) as i64,
            ));
        }
        arg0.coerce_to_int_i64(vm, globals)?
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
fn kernel_integer(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
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
    // Try to_int coercion
    if let Some(func_id) = globals.check_method(arg0, IdentId::TO_INT) {
        let result = vm.invoke_func_inner(globals, func_id, arg0, &[], None, None)?;
        match result.unpack() {
            RV::Fixnum(i) => return Ok(Value::integer(i)),
            RV::BigInt(b) => return Ok(Value::bigint(b.clone())),
            _ => {
                return Err(MonorubyErr::typeerr(format!(
                    "can't convert {} into Integer ({}#to_int gives {})",
                    arg0.get_real_class_name(globals),
                    arg0.get_real_class_name(globals),
                    result.get_real_class_name(globals),
                )));
            }
        }
    }
    // Try to_i coercion as a fallback
    let to_i_id = IdentId::get_id("to_i");
    if let Some(func_id) = globals.check_method(arg0, to_i_id) {
        let result = vm.invoke_func_inner(globals, func_id, arg0, &[], None, None)?;
        match result.unpack() {
            RV::Fixnum(i) => return Ok(Value::integer(i)),
            RV::BigInt(b) => return Ok(Value::bigint(b.clone())),
            _ => {
                return Err(MonorubyErr::typeerr(format!(
                    "can't convert {} into Integer ({}#to_i gives {})",
                    arg0.get_real_class_name(globals),
                    arg0.get_real_class_name(globals),
                    result.get_real_class_name(globals),
                )));
            }
        }
    }
    Err(MonorubyErr::typeerr(format!(
        "can't convert {} into Integer",
        arg0.get_real_class_name(globals),
    )))
}

///
/// ### Kernel.#Float
///
/// - Float(arg, [NOT SUPPORTED] exception: true) -> Float | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/Float.html]
#[monoruby_builtin]
fn kernel_float(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
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
    // Try to_f coercion
    if let Some(func_id) = globals.check_method(arg0, IdentId::TO_F) {
        let result = vm.invoke_func_inner(globals, func_id, arg0, &[], None, None)?;
        match result.unpack() {
            RV::Float(f) => return Ok(Value::float(f)),
            RV::Fixnum(i) => return Ok(Value::float(i as f64)),
            _ => {
                return Err(MonorubyErr::typeerr(format!(
                    "can't convert {} into Float ({}#to_f gives {})",
                    arg0.get_real_class_name(globals),
                    arg0.get_real_class_name(globals),
                    result.get_real_class_name(globals),
                )));
            }
        }
    }
    Err(MonorubyErr::typeerr(format!(
        "can't convert {} into Float",
        arg0.get_real_class_name(globals),
    )))
}

///
/// ### Kernel.#Complex
///
/// - Complex(r, i = 0, [NOT SUPPORTED] exception: true) -> Complex | nil
/// - [NOT SUPPORTED] Complex(s, exception: true) -> Complex | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/Complex.html]
#[monoruby_builtin]
fn kernel_complex(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
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
fn kernel_array(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let arg = lfp.arg(0);
    if arg.is_array_ty() {
        return Ok(arg);
    }
    if let Some(func_id) = globals.check_method(arg, IdentId::TO_ARY) {
        let result = vm.invoke_func_inner(globals, func_id, arg, &[], None, None)?;
        if result.is_array_ty() {
            return Ok(result);
        }
        return Err(MonorubyErr::typeerr(format!(
            "can't convert {} into Array ({}#to_ary gives {})",
            arg.get_real_class_name(globals),
            arg.get_real_class_name(globals),
            result.get_real_class_name(globals),
        )));
    } else if let Some(func_id) = globals.check_method(arg, IdentId::TO_A) {
        let result = vm.invoke_func_inner(globals, func_id, arg, &[], None, None)?;
        if result.is_array_ty() {
            return Ok(result);
        }
        return Err(MonorubyErr::typeerr(format!(
            "can't convert {} into Array ({}#to_a gives {})",
            arg.get_real_class_name(globals),
            arg.get_real_class_name(globals),
            result.get_real_class_name(globals),
        )));
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
fn require(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let feature = lfp.arg(0).coerce_to_string(vm, globals)?;
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
fn require_relative(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut file_name: std::path::PathBuf = globals.current_source_path(vm).into();
    file_name.pop();
    let feature = std::path::PathBuf::from(lfp.arg(0).coerce_to_string(vm, globals)?);
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
fn load_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let file_name = std::path::PathBuf::from(lfp.arg(0).coerce_to_string(vm, globals)?);
    let wrap = lfp.try_arg(1).map(|v| v.as_bool()).unwrap_or(false);
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
fn autoload(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
/// - eval(expr, bind, fname = "(eval)", lineno = 1) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/eval.html]
#[monoruby_builtin]
fn eval(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let expr = lfp.arg(0).coerce_to_string(vm, globals)?;
    let cfp = vm.cfp();
    let caller_cfp = cfp.prev().unwrap();
    let fname = if let Some(f) = lfp.try_arg(2) {
        f.coerce_to_str(vm, globals)?
    } else {
        let caller_loc = globals.store.get_caller_loc(caller_cfp, Some(pc));
        format!("(eval at {})", caller_loc)
    };
    let lineno: i64 = if let Some(l) = lfp.try_arg(3) {
        l.coerce_to_int_i64(vm, globals)?
    } else {
        1
    };
    if let Some(bind) = lfp.try_arg(1)
        && !bind.is_nil()
    {
        let binding = if let Some(b) = Binding::try_new(bind) {
            b
        } else {
            return Err(MonorubyErr::typeerr("Binding expected"));
        };
        globals.compile_script_binding(expr, fname, binding, lineno)?;
        vm.invoke_binding(globals, binding.binding().unwrap())
    } else {
        let fid = globals.compile_script_eval(expr, fname, caller_cfp, None, lineno)?;
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
fn system(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::process::Command;
    let arg0 = lfp.arg(0);
    let (program, mut args) = prepare_command_arg(arg0.as_str());
    if let Some(arg1) = lfp.try_arg(1) {
        for v in arg1.as_array().iter() {
            args.push(v.coerce_to_string(vm, globals)?);
        }
    }
    Ok(match Command::new(program).args(&args).status() {
        Ok(status) => Value::bool(status.success()),
        Err(_) => Value::nil(),
    })
}

///
/// ### Kernel.#exec
///
/// - exec(command, *args) -> ()
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/exec.html]
#[monoruby_builtin]
fn exec(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::ffi::CString;
    let args = lfp.arg(0).as_array();
    // Filter out trailing Hash arguments (keyword args like close_others:)
    let str_args: Vec<String> = args
        .iter()
        .filter(|v| v.try_hash_ty().is_none())
        .map(|v| v.coerce_to_string(vm, globals))
        .collect::<Result<Vec<_>>>()?;
    if str_args.is_empty() {
        return Err(MonorubyErr::argumenterr(
            "wrong number of arguments (given 0, expected 1+)",
        ));
    }
    if str_args.len() == 1 {
        // Single string: use shell
        let (program, shell_args) = prepare_command_arg(&str_args[0]);
        let mut all_args = vec![CString::new(program.clone()).unwrap()];
        for a in &shell_args {
            all_args.push(CString::new(a.as_str()).unwrap());
        }
        let c_program = CString::new(program).unwrap();
        // SAFETY: execvp replaces the process. Only fails if the program is not found.
        unsafe {
            libc::execvp(
                c_program.as_ptr(),
                all_args
                    .iter()
                    .map(|a| a.as_ptr())
                    .chain(std::iter::once(std::ptr::null()))
                    .collect::<Vec<_>>()
                    .as_ptr(),
            )
        };
        Err(MonorubyErr::runtimeerr(format!(
            "exec failed: {}",
            std::io::Error::last_os_error()
        )))
    } else {
        // Multiple args: first is program, rest are argv
        let c_program = CString::new(str_args[0].as_str()).unwrap();
        let c_args: Vec<CString> = str_args
            .iter()
            .map(|s| CString::new(s.as_str()).unwrap())
            .collect();
        // SAFETY: execvp replaces the process. Only fails if the program is not found.
        unsafe {
            libc::execvp(
                c_program.as_ptr(),
                c_args
                    .iter()
                    .map(|a| a.as_ptr())
                    .chain(std::iter::once(std::ptr::null()))
                    .collect::<Vec<_>>()
                    .as_ptr(),
            )
        };
        Err(MonorubyErr::runtimeerr(format!(
            "exec failed: {}",
            std::io::Error::last_os_error()
        )))
    }
}

///
/// ### Kernel.#fork
///
/// - fork -> Integer | nil
/// - fork { ... } -> Integer | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/fork.html]
#[monoruby_builtin]
fn fork(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // SAFETY: fork() is a POSIX system call. We call it in a single-threaded context.
    let pid = unsafe { libc::fork() };
    if pid < 0 {
        return Err(MonorubyErr::runtimeerr("fork failed"));
    }
    if pid == 0 {
        // Child process
        if let Some(bh) = lfp.block() {
            let data = vm.get_block_data(globals, bh)?;
            match vm.invoke_block(globals, &data, &[]) {
                Ok(_) => std::process::exit(0),
                Err(_) => std::process::exit(1),
            }
        }
        Ok(Value::nil())
    } else {
        // Parent process
        Ok(Value::integer(pid as i64))
    }
}

///
/// Kernel.#`
///
/// - `command` -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/=60.html]
#[monoruby_builtin]
fn command(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn sleep(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let now = std::time::Instant::now();
    if let Some(sec) = lfp.try_arg(0) {
        let sec = sec.coerce_to_f64(vm, globals)?;
        if sec.is_nan() || sec < 0.0 {
            return Err(MonorubyErr::argumenterr(
                "time interval must not be negative or NaN",
            ));
        }
        std::thread::sleep(std::time::Duration::from_secs_f64(sec));
    } else {
        // monoruby is single-threaded; sleep without argument would block
        // forever with no way to be interrupted. Return immediately.
        return Ok(Value::integer(0));
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
fn abort(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let msg = if let Some(arg0) = lfp.try_arg(0) {
        let s = arg0.coerce_to_str(vm, globals)?;
        eprintln!("{}", s);
        s
    } else {
        "abort".to_string()
    };
    Err(MonorubyErr::new(MonorubyErrKind::SystemExit(1), msg))
}

///
/// Kernel.#exit
///
/// - exit(status = true) -> ()
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/exit.html]
#[monoruby_builtin]
fn exit(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let status = if let Some(arg0) = lfp.try_arg(0) {
        if let Some(i) = arg0.try_fixnum() {
            i as u8
        } else {
            match arg0.as_bool() {
                true => 0,
                false => 1,
            }
        }
    } else {
        0
    };
    Err(MonorubyErr::new(
        MonorubyErrKind::SystemExit(status),
        "exit",
    ))
}

///
/// Kernel.#exit!
///
/// - exit!(status = false) -> ()
///
/// Exits the process immediately. No at_exit handlers are run.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/exit=21.html]
#[monoruby_builtin]
fn exit_bang(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let status = if let Some(arg0) = lfp.try_arg(0) {
        if let Some(i) = arg0.try_fixnum() {
            i as i32
        } else {
            match arg0.as_bool() {
                true => 0,
                false => 1,
            }
        }
    } else {
        1
    };
    std::process::exit(status);
}

///
/// Kernel.#warn
///
/// - warn(*message, [NOT SUPPORTED] uplevel: nil, [NOT SUPPORTED] category: nil) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/warn.html]
#[monoruby_builtin]
fn warn(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let message = lfp.arg(0);
    // uplevel and category keyword arguments are accepted but ignored for now.
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
fn dir_(vm: &mut Executor, globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn method_(vm: &mut Executor, globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn dlopen(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // see: https://github.com/ruby/fiddle/blob/2b3747e919df5d044c835cbbb27ebf9e27df74f9/ext/fiddle/handle.c#L136
    let arg0 = lfp.arg(0);
    let flags = if let Some(arg1) = lfp.try_arg(1) {
        match i32::try_from(arg1.coerce_to_int_i64(vm, globals)?) {
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
        let message = { unsafe { std::ffi::CStr::from_ptr(err).to_string_lossy().into_owned() } };
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
fn dlsym(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn dlcall(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn malloc(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn memcpyv(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn read_memory(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn max(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f1 = lfp.arg(0).coerce_to_f64(vm, globals)?;
    let f2 = lfp.arg(1).coerce_to_f64(vm, globals)?;
    Ok(Value::float(f64::max(f1, f2)))
}

///
/// - ____min(f1:Float, f2:Float) -> Float
///
#[monoruby_builtin]
fn min(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f1 = lfp.arg(0).coerce_to_f64(vm, globals)?;
    let f2 = lfp.arg(1).coerce_to_f64(vm, globals)?;
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

///
/// Object#send
///
/// - send(name, *args) -> object
/// - send(name, *args) { .... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/send.html]
#[monoruby_builtin]
pub(crate) fn send(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let ary = lfp.arg(0).as_array();
    if ary.len() < 1 {
        return Err(MonorubyErr::wrong_number_of_arg_min(ary.len(), 1));
    }
    let method = ary[0].expect_symbol_or_string(globals)?;
    vm.invoke_method_inner(
        globals,
        method,
        lfp.self_val(),
        &ary[1..],
        lfp.block(),
        if let Some(kw) = lfp.try_arg(1)
            && let Some(kw) = kw.try_hash_ty()
            && !kw.is_empty()
        {
            Some(kw)
        } else {
            None
        },
    )
}

pub fn object_send(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    _: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    let no_splat = !callsite.object_send_single_splat();
    if !callsite.is_simple() && no_splat {
        return false;
    }

    state.write_back_recv_and_callargs(ir, callsite);
    state.writeback_acc(ir);
    let using_xmm = state.get_using_xmm();
    let error = ir.new_error(state);
    let callid = callsite.id;
    ir.inline(move |r#gen, store, labels| {
        let error = &labels[error];
        r#gen.object_send_inline(callid, store, using_xmm, &error, no_splat);
    });
    state.def_reg2acc(ir, GP::Rax, callsite.dst);
    true
}

///
/// ### Kernel#extend
/// - extend(*modules) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/extend.html]
#[monoruby_builtin]
fn extend(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    if args.len() == 0 {
        return Err(MonorubyErr::wrong_number_of_arg_min(0, 1));
    }
    let self_val = lfp.self_val();
    for v in args.iter().cloned().rev() {
        vm.invoke_method_inner(globals, IdentId::EXTEND_OBJECT, v, &[self_val], None, None)?;
        vm.invoke_method_inner(globals, IdentId::EXTENDED, v, &[self_val], None, None)?;
    }
    Ok(lfp.self_val())
}

///
/// ### Kernel#is_a?
///
/// - is_a?(mod) -> bool
/// - kind_of?(mod) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/is_a=3f.html]
#[monoruby_builtin]
fn is_a(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.arg(0).expect_class_or_module(&globals.store)?.id();
    Ok(Value::bool(
        lfp.self_val().is_kind_of(&globals.store, class),
    ))
}

///
/// ### Kernel#enum_for
///
/// - to_enum(method = :each, *args) -> Enumerator
/// - enum_for(method = :each, *args) -> Enumerator
/// - [NOT SUPPORTED] to_enum(method = :each,  *args) {|*args| ... } -> Enumerator
/// - [NOT SUPPORTED] enum_for(method = :each, *args) {|*args| ... } -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/enum_for.html]
#[monoruby_builtin]
fn to_enum(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    lfp.expect_no_block()?;
    let args = lfp.arg(0).as_array();
    let (method, args) = if args.is_empty() {
        (IdentId::EACH, vec![])
    } else {
        (
            args[0].expect_symbol_or_string(globals)?,
            args[1..].to_vec(),
        )
    };
    vm.generate_enumerator(method, lfp.self_val(), args, pc)
}

///
/// ### Kernel#dup
///
/// - dup -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/clone.html]
#[monoruby_builtin]
fn dup(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp.self_val().dup())
}

///
/// ### Kernel#clone
///
/// - clone -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/clone.html]
#[monoruby_builtin]
fn clone_val(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(lfp.self_val().clone_value())
}

/// ### Kernel#define_singleton_method
///
/// - define_singleton_method(name, method) -> Symbol
/// - define_singleton_method(name) { ... } -> Symbol
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/define_singleton_method.html]
#[monoruby_builtin]
fn define_singleton_method(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let class_id = globals.store.get_singleton(self_val)?.id();
    let name = lfp.arg(0).expect_symbol_or_string(globals)?;
    let func_id = if let Some(method) = lfp.try_arg(1) {
        if let Some(proc) = method.is_proc() {
            globals.define_proc_method(proc)
        } else if let Some(method) = method.is_method() {
            method.func_id()
        } else if let Some(method) = method.is_umethod() {
            method.func_id()
        } else {
            return Err(MonorubyErr::wrong_argument_type(
                globals,
                method,
                "Proc/Method/UnboundMethod",
            ));
        }
    } else if let Some(bh) = lfp.block() {
        let proc = vm.generate_proc(bh, pc)?;
        globals.define_proc_method(proc)
    } else {
        return Err(MonorubyErr::wrong_number_of_arg(2, 1));
    };
    vm.add_public_method(globals, class_id, name, func_id)?;
    Ok(Value::symbol(name))
}

/// ### Kernel#initialize_copy
///
/// - initialize_copy(obj) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/initialize_copy.html]
#[monoruby_builtin]
fn initialize_copy(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let orig = lfp.arg(0);
    let self_val = lfp.self_val();
    if orig.id() == self_val.id() {
        return Ok(self_val);
    }
    if self_val.real_class(globals).id() != orig.real_class(globals).id()
        || self_val.ty() != orig.ty()
    {
        return Err(MonorubyErr::typeerr(format!(
            "initialize_copy should take same class object self:{} original:{}",
            self_val.class().get_name(globals),
            orig.class().get_name(globals)
        )));
    }
    Ok(self_val)
}

/// ### Kernel#initialize_clone
///
/// - initialize_clone(obj) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/initialize_clone.html]
#[monoruby_builtin]
fn initialize_clone(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let orig = lfp.arg(0);
    let self_val = lfp.self_val();
    vm.invoke_method_inner(
        globals,
        IdentId::get_id("initialize_copy"),
        self_val,
        &[orig],
        None,
        None,
    )
}

///
/// ### Kernel#to_s
///
/// - to_s -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/to_s.html]
#[monoruby_builtin]
fn to_s(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let s = lfp.self_val().to_s(&globals.store);
    Ok(Value::string(s))
}

///
/// ### Kernel#respond_to?
///
/// - respond_to?(name, include_all = false) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/respond_to=3f.html]
#[monoruby_builtin]
fn respond_to(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let name = lfp.arg(0).expect_symbol_or_string(globals)?;
    let include_all = if let Some(arg1) = lfp.try_arg(1) {
        arg1.as_bool()
    } else {
        false
    };
    let found = if include_all {
        globals.check_method(lfp.self_val(), name).is_some()
    } else {
        globals.check_public_method(lfp.self_val(), name).is_some()
    };
    if found {
        return Ok(Value::bool(true));
    }
    // Call respond_to_missing?(name, include_all) as CRuby does.
    let respond_to_missing = IdentId::get_id("respond_to_missing?");
    if let Some(fid) = globals.check_method(lfp.self_val(), respond_to_missing) {
        let result = vm.invoke_func_inner(
            globals,
            fid,
            lfp.self_val(),
            &[Value::symbol(name), Value::bool(include_all)],
            None,
            None,
        )?;
        return Ok(Value::bool(result.as_bool()));
    }
    Ok(Value::bool(false))
}

fn object_respond_to(
    state: &mut AbstractState,
    _: &mut AsmIr,
    ctx: &JitContext,
    store: &Store,
    callid: CallSiteId,
    recv_class: ClassId,
    _: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo {
        dst, args, pos_num, ..
    } = *callsite;
    let dst = if let Some(dst) = dst {
        dst
    } else {
        return false;
    };
    let include_all = if pos_num != 1 {
        if state.is_truthy(args + 1usize) {
            true
        } else if state.is_falsy(args + 1usize) {
            false
        } else {
            return false;
        }
    } else {
        false
    };
    let method_name = if let Some(name) = state.is_symbol_literal(args) {
        name
    } else {
        return false;
    };
    if let Some(entry) =
        store.check_method_for_class_with_version(recv_class, method_name, ctx.class_version())
    {
        if include_all || entry.is_public() {
            state.def_C(dst, Immediate::bool(true));
            return true;
        }
    }
    // Method not found directly. Cannot JIT-inline because respond_to_missing?
    // may be overridden and needs to be called at runtime.
    false
}

///
/// ### Kernel#inspect
///
/// - inspect -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/inspect.html]
#[monoruby_builtin]
fn inspect(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let s = lfp.self_val().inspect(&globals.store);
    Ok(Value::string(s))
}

///
/// ### Kernel#class
///
/// - class -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/class.html]
#[monoruby_builtin]
fn class(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp.self_val().real_class(&globals.store).as_val())
}

///
/// ### Kernel#hash
///
/// - hash -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/hash.html]
#[monoruby_builtin]
fn hash(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::integer(lfp.self_val().id() as _))
}

///
/// ### Kernel#eql?
///
/// - eql?(other) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/eql=3f.html]
#[monoruby_builtin]
fn eql_(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().id() == lfp.arg(0).id()))
}

///
/// ### Kernel#instance_of?
///
/// - instance_of?(klass) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_of=3f.html]
#[monoruby_builtin]
fn instance_of(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let b = lfp.self_val().real_class(&globals.store).id()
        == lfp.arg(0).expect_class_or_module(&globals.store)?.id();
    Ok(Value::bool(b))
}

///
/// ### Kernel#method
///
/// - method(name) -> Method
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/method.html]
#[monoruby_builtin]
fn method(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let receiver = lfp.self_val();
    let method_name = lfp.arg(0).expect_symbol_or_string(globals)?;
    let (func_id, _, owner) = globals.find_method_for_object(receiver, method_name)?;
    Ok(Value::new_method(receiver, func_id, owner))
}

///
/// ### Kernel#singleton_class
///
/// - singleton_class -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/singleton_class.html]
#[monoruby_builtin]
fn singleton_class(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.self_val().get_singleton(&mut globals.store)
}

///
/// ### Kernel#methods
///
/// - methods(include_inherited = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/methods.html]
#[monoruby_builtin]
fn methods(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let inherited_too = lfp.try_arg(0).is_none() || lfp.arg(0).as_bool();
    Ok(Value::array_from_vec(if !inherited_too {
        let class_id = match globals.store.has_singleton(lfp.self_val()) {
            Some(module) => module.id(),
            None => return Ok(Value::array_empty()),
        };
        globals.store.get_method_names(class_id)
    } else {
        let class_id = lfp.self_val().class();
        globals.store.get_method_names_inherit(class_id, true)
    }))
}

///
/// ### Kernel#singleton_methods
///
/// - singleton_methods(inherited_too = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/singleton_methods.html]
#[monoruby_builtin]
fn singleton_methods(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = match globals.store.has_singleton(lfp.self_val()) {
        Some(module) => module.id(),
        None => return Ok(Value::array_empty()),
    };
    let inherited_too = lfp.try_arg(0).is_none() || lfp.arg(0).as_bool();
    Ok(Value::array_from_vec(if !inherited_too {
        globals.store.get_method_names(class_id)
    } else {
        globals.store.get_method_names_inherit(class_id, true)
    }))
}

///
/// ### Kernel#instance_variable_defined?
///
/// - instance_variable_defined?(var) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_variable_defined=3f.html]
#[monoruby_builtin]
fn iv_defined(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let id = match lfp.arg(0).unpack() {
        RV::Symbol(sym) => sym,
        RV::String(s) => IdentId::get_id(s.check_utf8()?),
        _ => return Err(MonorubyErr::is_not_symbol_nor_string(globals, lfp.arg(0))),
    };
    let b = globals.store.get_ivar(lfp.self_val(), id).is_some();
    Ok(Value::bool(b))
}

///
/// ### Kernel#instance_variable_set
///
/// - instance_variable_set(var, value) -> Object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_variable_set.html]
#[monoruby_builtin]
fn iv_set(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let id = lfp.arg(0).expect_symbol_or_string(globals)?;
    let val = lfp.arg(1);
    globals.store.set_ivar(lfp.self_val(), id, val)?;
    Ok(val)
}

///
/// ### Kernel#instance_variable_get
///
/// - instance_variable_get(var) -> Object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_variable_get.html]
#[monoruby_builtin]
fn iv_get(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let id = lfp.arg(0).expect_symbol_or_string(globals)?;
    let v = globals
        .store
        .get_ivar(lfp.self_val(), id)
        .unwrap_or_default();
    Ok(v)
}

///
/// ### Kernel#instance_variables
///
/// - instance_variables -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_variables.html]
#[monoruby_builtin]
fn iv(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let iter = globals
        .store
        .get_ivars(lfp.self_val())
        .into_iter()
        .map(|(id, _)| Value::symbol(id));
    Ok(Value::array_from_iter(iter))
}

///
/// ### Kernel#remove_instance_variable
///
/// - remove_instance_variable(name) -> Object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/remove_instance_variable.html]
#[monoruby_builtin]
fn iv_remove(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let id = lfp.arg(0).expect_symbol_or_string(globals)?;
    match globals.store.remove_ivar(lfp.self_val(), id) {
        Some(val) => Ok(val),
        None => Err(MonorubyErr::nameerr(format!(
            "instance variable {id} not defined"
        ))),
    }
}

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
    fn eval_lineno() {
        run_test(r#"eval("__LINE__", nil, "test.rb", 42)"#);
        run_test(r#"eval("__FILE__", nil, "test.rb", 42)"#);
        run_test(r#"eval("__LINE__\n__LINE__", nil, "test.rb", 10)"#);
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
        run_test(r#"warn(100, uplevel:1)"#);
        run_test(r#"warn(100, category: :experimental)"#);
        run_test_error(r#"raise "Woo""#);
    }

    #[test]
    fn exec_in_fork() {
        // fork test
        run_test(
            r##"
        fork { puts 42 }
        42
        "##,
        );
    }

    #[test]
    fn exec() {
        // exec with multiple args (no shell): verify via system() calling a
        // tiny script that execs /bin/echo and captures output in the shell.
        run_test(
            r##"
        `sh -c '/bin/echo hello world'`.chomp
        "##,
        );
        // exec replaces the process: code after exec is not reached.
        // Verified by checking only "replaced" appears in output, not "NOT_REACHED".
        run_test_no_result_check(
            r##"
        pid = fork do
          exec "/bin/echo", "replaced"
          STDERR.puts "NOT_REACHED"
        end
        pid
        "##,
        );
        // exec with single string (uses shell)
        run_test_no_result_check(
            r##"
        pid = fork do
          exec "exit 0"
        end
        pid
        "##,
        );
        // exec with keyword argument (should not error)
        run_test_no_result_check(
            r##"
        pid = fork do
          exec "/bin/echo", "kw", close_others: false
        end
        pid
        "##,
        );
    }

    #[test]
    fn exit_raises_system_exit() {
        run_test_once(
            r##"
        begin
          exit
        rescue SystemExit => e
          e.status
        end
        "##,
        );
    }

    #[test]
    fn exit_with_status() {
        run_test_once(
            r##"
        begin
          exit(42)
        rescue SystemExit => e
          e.status
        end
        "##,
        );
    }

    #[test]
    fn abort_raises_system_exit() {
        run_test_once(
            r##"
        begin
          abort("test")
        rescue SystemExit => e
          e.status
        end
        "##,
        );
    }

    #[test]
    fn sleep_nan_error() {
        run_test_no_result_check(
            r##"
        begin
          sleep(Float::NAN)
          false
        rescue ArgumentError
          true
        end
        "##,
        );
    }

    #[test]
    fn sleep_negative_error() {
        run_test_no_result_check(
            r##"
        begin
          sleep(-1)
          false
        rescue ArgumentError
          true
        end
        "##,
        );
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

    #[test]
    fn p_user_defined_inspect() {
        // p calls inspect on each argument via Ruby method dispatch
        run_test_no_result_check(
            r##"
        class PFoo
          def inspect
            "pfoo_inspect"
          end
        end
        p PFoo.new
        "##,
        );
    }

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
    fn object_id() {
        run_test_with_prelude(
            r##"
            [id == a.object_id, a.__id__ == a.object_id]
            "##,
            r##"
            a = [1,2,3]
            id = a.object_id
            "##,
        );
        run_test_with_prelude(
            r##"
            [id == a.object_id, a.__id__ == a.object_id]
            "##,
            r##"
            a = 1356
            id = a.object_id
            "##,
        );
        run_test_with_prelude(
            r##"
            [id == a.object_id, a.__id__ == a.object_id]
            "##,
            r##"
            a = -49.52
            id = a.object_id
            "##,
        );
    }

    #[test]
    fn remove_instance_variable() {
        run_test2(
            r#"
            class Foo
              def initialize
                @a = 1
                @b = 2
              end
            end
            f = Foo.new
            f.remove_instance_variable(:@a)
            "#,
        );
        run_test2(
            r#"
            class Foo
              def initialize
                @a = 1
                @b = 2
              end
            end
            f = Foo.new
            f.remove_instance_variable(:@a)
            f.instance_variables
            "#,
        );
        run_test2(
            r#"
            class Foo
              def initialize
                @a = 1
                @b = 2
              end
            end
            f = Foo.new
            f.remove_instance_variable("@a")
            f.instance_variables
            "#,
        );
        run_test2(
            r#"
            module M
              @x = 42
              remove_instance_variable(:@x)
            end
            "#,
        );
        run_test_error(
            r#"
            obj = Object.new
            obj.remove_instance_variable(:@nonexistent)
            "#,
        );
    }

    #[test]
    fn extend() {
        run_test_with_prelude(
            r#"
        res = []
        obj = Object.new
        obj.extend Foo, Bar
        res << obj.a #=> "ok Foo"
        res << obj.b #=> "ok Bar"
        res << Klass.new.a #=> "ok Foo"
        res << Klass.b     #=> "ok Bar"
        res
        "#,
            r#"
        module Foo
          def a
            'ok Foo'
          end
        end

        module Bar
          def b
            'ok Bar'
          end
        end

        class Klass
          include Foo
          extend Bar
        end
        "#,
        );
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
    fn block_given_kernel() {
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
            r#"a=Object.new; a.instance_variable_set("@i", 42); a.instance_variable_defined?(:@j)"#,
        );
        run_test2(
            r#"a=Object.new; a.instance_variable_set("@i", 42); a.instance_variable_get(:@i)"#,
        );
        // TDDO: Object#instace_variables must return Array of Symbols,
        // ordered by the time of definition of the instance variables.
        run_test2(
            r#"
            a=Object.new;
            a.instance_variable_set("@b", 1);
            a.instance_variable_set("@e", 4);
            a.instance_variable_set("@c", 2);
            a.instance_variable_set("@j", 9);
            a.instance_variable_set("@d", 3);
            a.instance_variable_set("@a", 0);
            a.instance_variable_set("@g", 6);
            a.instance_variable_set("@l", 11);
            a.instance_variable_set("@h", 7);
            a.instance_variable_set("@i", 8);
            a.instance_variable_set("@f", 5);
            a.instance_variable_set("@k", 10);
            a.instance_variables.sort
            "#,
        );
    }

    #[test]
    fn respond_to() {
        run_test_with_prelude(
            r#"
        list = [F.new, D.new]
        res = []
        list.each{|it| res << it.hello if it.respond_to?(:hello)}
        list.each{|it| res << it.hello if it.respond_to?(:hello, false)}
        list.each{|it| it.instance_eval("res << hello if it.respond_to?(:hello, true)")}
        res
        "#,
            r#"
        class F
          def hello
            "Bonjour"
          end
        end

        class D
          private
          def hello
            "Guten Tag"
          end
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
    fn kernel_integer_to_int_coercion() {
        // Integer() should call to_int on non-numeric arguments
        run_test_with_prelude(
            r#"Integer(o)"#,
            r#"class C; def to_int; 42; end; end; o = C.new"#,
        );
    }

    #[test]
    fn kernel_float_to_f_coercion() {
        // Float() should call to_f on non-numeric arguments
        run_test_with_prelude(
            r#"Float(o)"#,
            r#"class C; def to_f; 3.14; end; end; o = C.new"#,
        );
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

    #[test]
    fn send() {
        run_test(
            r#"
        "ruby".send(:sub, /./, "R")
        "#,
        );
        run_test(
            r#"
        class Foo
            def foo() "foo" end
            def bar() "bar" end
            def baz() "baz" end
        end
        methods = {1 => :foo, 2 => :bar, 3 => :baz}
        f = Foo.new
        [f.send(methods[1]), f.send(methods[2]), f.send(methods[3])]
        "#,
        );
        run_test_with_prelude(
            r#"
            o.send(:f){ 3 }
            "#,
            r#"
            class C
                def f
                    yield
                end
            end
            o = C.new
            "#,
        );
    }

    #[test]
    fn object_send() {
        run_test_with_prelude(
            r##"
        o = C.new
        [o.send(:foo), o.send("foo"), o.send(:bar, 20), o.send(*[:foo]), o.send(*["bar", 100])]
        "##,
            r##"
        class C
            def foo
                1
            end
            def bar(x)
                x
            end
        end
        "##,
        );
        run_test_error(
            r##"
        class C
            def foo
                1
            end
        end
        C.new.send
        "##,
        );
        run_test_error(
            r##"
        class C
            def foo
                1
            end
        end
        C.new.send(200, 100)
        "##,
        );
        run_test_error(
            r##"
        class C
            def foo
                1
            end
        end
        C.new.send(:foo, 100)
        "##,
        );
        run_test_error(
            r##"
        class C
            def foo
                1
            end
        end
        C.new.send(*[])
        "##,
        );
        run_test_error(
            r##"
          class C
            def b(x, y, z)
              puts "x=#{[x, y, z]}"
            end
          end

          o = C.new
          30.times do |x|
            if x == 28 then
              eval('class C; def b(x); puts "x=#{x}" ;end; end')
            end
            o.send(:b, x, 2, 3)
          end
            "##,
        );
    }

    #[test]
    fn public_send() {
        run_test(
            r##"
        class C
          def foo; "foo"; end
        end
        C.new.public_send(:foo)
        "##,
        );
        run_test(
            r##"
        class C
          def bar(x); x * 2; end
        end
        C.new.public_send(:bar, 21)
        "##,
        );
        run_test(
            r##"
        class C
          def baz; yield + 1; end
        end
        C.new.public_send(:baz) { 41 }
        "##,
        );
    }

    #[test]
    fn methods() {
        run_test_with_prelude(
            r##"
        [
          obj.singleton_methods(false).sort,
          (obj.singleton_methods(true) - obj2.singleton_methods(true)).sort,
          obj.methods(false).sort,
          (obj.methods(true) - obj2.methods(true)).sort,
          Foo.singleton_methods(false).sort,
          (Foo.singleton_methods(true) - Foo2.singleton_methods(true)).sort
        ]
        "##,
            r##"
        Parent = Class.new

        class <<Parent
          private;   def private_class_parent() end
          protected; def protected_class_parent() end
          public;    def public_class_parent() end
        end

        Foo = Class.new(Parent)
        Foo2 = Class.new()

        class <<Foo
          private;   def private_class_foo() end
          protected; def protected_class_foo() end
          public;    def public_class_foo() end
        end

        module Bar
          private;   def private_bar()   end
          protected; def protected_bar() end
          public;    def public_bar()    end
        end

        obj = Foo.new
        obj2 = Foo.new

        class << obj
          include Bar
          private;   def private_self()   end
          protected; def protected_self() end
          public;    def public_self()    end
        end
        "##,
        );
    }

    #[test]
    fn define_singleton_method() {
        // with block
        run_test(
            r#"
            obj = Object.new
            obj.define_singleton_method(:greet) { "hello" }
            obj.greet
            "#,
        );
        // with block taking arguments
        run_test(
            r#"
            obj = Object.new
            obj.define_singleton_method(:add) { |a, b| a + b }
            obj.add(3, 4)
            "#,
        );
        // returns symbol
        run_test(
            r#"
            obj = Object.new
            obj.define_singleton_method(:foo) { 42 }
            "#,
        );
        // with string name
        run_test(
            r#"
            obj = Object.new
            obj.define_singleton_method("bar") { "baz" }
            obj.bar
            "#,
        );
        // with Proc
        run_test(
            r#"
            obj = Object.new
            pr = Proc.new { |x| x * 2 }
            obj.define_singleton_method(:double, pr)
            obj.double(5)
            "#,
        );
        // with Method
        run_test(
            r#"
            class Foo
              def hello
                "hello from Foo"
              end
            end
            obj = Foo.new
            obj.define_singleton_method(:greet, obj.method(:hello))
            obj.greet
            "#,
        );
        // with UnboundMethod
        run_test(
            r#"
            class Foo
              def hi
                "hi"
              end
            end
            obj = Foo.new
            obj.define_singleton_method(:greet, Foo.instance_method(:hi))
            obj.greet
            "#,
        );
        // does not affect other instances
        run_test(
            r#"
            a = Object.new
            b = Object.new
            a.define_singleton_method(:only_a) { "only a" }
            [a.only_a, b.respond_to?(:only_a)]
            "#,
        );
        // error: no block and no method given
        run_test_error(
            r#"
            obj = Object.new
            obj.define_singleton_method(:foo)
            "#,
        );
        // error: wrong argument type
        run_test_error(
            r#"
            obj = Object.new
            obj.define_singleton_method(:foo, 42)
            "#,
        );
    }

    #[test]
    fn initialize_copy() {
        // initialize_copy with same object returns self
        run_test(
            r#"
            class Foo
              def initialize
                @a = 1
              end
            end
            obj = Foo.new
            obj.send(:initialize_copy, obj).equal?(obj)
            "#,
        );
        // initialize_copy with same class succeeds
        run_test(
            r#"
            class Foo
              def initialize
                @a = 1
              end
            end
            a = Foo.new
            b = Foo.new
            b.send(:initialize_copy, a).equal?(b)
            "#,
        );
        // initialize_copy with different class raises TypeError
        run_test_error(
            r#"
            class Foo; end
            class Bar; end
            a = Foo.new
            b = Bar.new
            a.send(:initialize_copy, b)
            "#,
        );
    }

    #[test]
    fn initialize_clone() {
        // initialize_clone delegates to initialize_copy
        run_test(
            r#"
            class Foo
              def initialize
                @a = 1
              end
            end
            a = Foo.new
            b = Foo.new
            b.send(:initialize_clone, a).equal?(b)
            "#,
        );
        // initialize_clone with different class raises TypeError via initialize_copy
        run_test_error(
            r#"
            class Foo; end
            class Bar; end
            a = Foo.new
            b = Bar.new
            a.send(:initialize_clone, b)
            "#,
        );
        // overridden initialize_clone is called
        run_test(
            r#"
            $called = []
            class Foo
              def initialize_clone(orig)
                $called << :initialize_clone
                super
              end
              def initialize_copy(orig)
                $called << :initialize_copy
                super
              end
            end
            a = Foo.new
            b = Foo.new
            b.send(:initialize_clone, a)
            $called
            "#,
        );
    }

    #[test]
    fn initialize_dup() {
        // initialize_dup delegates to initialize_copy
        run_test(
            r#"
            class Foo
              def initialize
                @a = 1
              end
            end
            a = Foo.new
            b = Foo.new
            b.send(:initialize_dup, a).equal?(b)
            "#,
        );
        // initialize_dup with different class raises TypeError via initialize_copy
        run_test_error(
            r#"
            class Foo; end
            class Bar; end
            a = Foo.new
            b = Bar.new
            a.send(:initialize_dup, b)
            "#,
        );
        // overridden initialize_dup is called
        run_test(
            r#"
            $called = []
            class Foo
              def initialize_dup(orig)
                $called << :initialize_dup
                super
              end
              def initialize_copy(orig)
                $called << :initialize_copy
                super
              end
            end
            a = Foo.new
            b = Foo.new
            b.send(:initialize_dup, a)
            $called
            "#,
        );
    }

    #[test]
    fn exit_bang() {
        // exit! is defined and callable (we test via respond_to? since actually
        // calling it would terminate the process immediately)
        run_test("respond_to?(:exit!)");
        run_test("Process.respond_to?(:exit!)");
    }

    #[test]
    fn puts_delegates_to_stdout() {
        // Kernel#puts delegates to $stdout.puts
        run_test_no_result_check(
            r#"
            require "stringio"
            old = $stdout
            $stdout = StringIO.new
            puts "hello"
            result = $stdout.string
            $stdout = old
            result
            "#,
        );
    }

    #[test]
    fn printf_delegates_to_stdout() {
        run_test_no_result_check(
            r#"
            require "stringio"
            old = $stdout
            $stdout = StringIO.new
            printf("%d", 42)
            result = $stdout.string
            $stdout = old
            result
            "#,
        );
    }

    #[test]
    fn format_to_str() {
        run_test_once(
            r#"
            class MyFmt
              def to_str; "num: %d"; end
            end
            format(MyFmt.new, 42)
            "#,
        );
    }

    #[test]
    fn sprintf_coercion() {
        // %d calls to_int (preferred) then to_i as fallback
        run_test(
            r#"
            class HasToInt; def to_int; 42; end; end
            sprintf("%d", HasToInt.new)
            "#,
        );
        run_test(
            r#"
            class HasToI; def to_i; 99; end; end
            sprintf("%d", HasToI.new)
            "#,
        );
        run_test(
            r#"
            class HasBoth; def to_int; 10; end; def to_i; 20; end; end
            sprintf("%d", HasBoth.new)
            "#,
        );
        // %x, %o, %b also call to_int/to_i
        run_test(
            r#"
            class HasToInt; def to_int; 255; end; end
            sprintf("%x", HasToInt.new)
            "#,
        );
        run_test(
            r#"
            class HasToInt; def to_int; 255; end; end
            sprintf("%o", HasToInt.new)
            "#,
        );
        run_test(
            r#"
            class HasToInt; def to_int; 10; end; end
            sprintf("%b", HasToInt.new)
            "#,
        );
        // %f calls to_f
        run_test(
            r#"
            class HasToF; def to_f; 3.14; end; end
            sprintf("%f", HasToF.new)
            "#,
        );
        // %e calls to_f
        run_test(
            r#"
            class HasToF; def to_f; 2.5; end; end
            sprintf("%.1e", HasToF.new)
            "#,
        );
        // %s calls to_s (not to_str)
        run_test(
            r#"
            class HasToS; def to_s; "hello"; end; end
            sprintf("%s", HasToS.new)
            "#,
        );
        // TypeError when no conversion method
        run_test(
            r#"
            begin
              sprintf("%d", Object.new)
            rescue TypeError => e
              e.message
            end
            "#,
        );
        run_test(
            r#"
            begin
              sprintf("%f", Object.new)
            rescue TypeError => e
              e.message
            end
            "#,
        );
    }

    #[test]
    fn sprintf_positional() {
        run_test(r#"sprintf("%1$d %2$d %1$d", 10, 20)"#);
        run_test(r#"sprintf("%1$s %2$s %1$s", "a", "b")"#);
        run_test(r#"sprintf("%2$d", 10, 20)"#);
        run_test(r#"sprintf("%1$05d", 42)"#);
        run_test(r#"sprintf("%1$x", 255)"#);
        run_test(r#"sprintf("%1$o", 8)"#);
        run_test(r#"sprintf("%1$f", 3.14)"#);
    }

    #[test]
    fn sprintf_named() {
        run_test(r#"sprintf("%{foo}", foo: "hello")"#);
        run_test(r#"sprintf("%{foo} %{bar}", foo: 1, bar: 2)"#);
        run_test(r#"sprintf("%{foo} %{foo}", foo: "x")"#);
    }

    #[test]
    fn sprintf_named_format() {
        run_test(r#"sprintf("%<foo>d", foo: 42)"#);
        run_test(r#"sprintf("%<foo>05d", foo: 42)"#);
        run_test(r#"sprintf("%<foo>10d", foo: 42)"#);
        run_test(r#"sprintf("%<foo>x", foo: 255)"#);
        run_test(r#"sprintf("%<foo>f", foo: 3.14)"#);
        run_test(r#"sprintf("%<foo>s", foo: "hello")"#);
    }
}
