use super::*;
#[cfg(target_arch = "x86_64")]
use jitgen::JitContext;
#[cfg(target_arch = "aarch64")]
use jitgen::{AbstractState, JitContext};
use num::ToPrimitive;
use num::Zero;
use std::{io::Write, mem::transmute};

//
// Kernel module
//

pub(super) fn init(globals: &mut Globals) -> Module {
    let klass = globals.define_toplevel_module("Kernel");
    let kernel_class = klass.id();
    globals.define_builtin_inline_func(kernel_class, "nil?", nil, inline_gen2!(kernel_nil), 0);
    globals.define_builtin_inline_func(
        kernel_class,
        "!~",
        not_match,
        inline_gen2!(kernel_not_match),
        1,
    );
    //globals.define_builtin_module_func_rest(kernel_class, "puts", puts);
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
    globals.define_builtin_module_func_with_effect(
        kernel_class,
        "local_variables",
        local_variables,
        0,
        0,
        false,
        Effect::CAPTURE,
    );
    globals.define_builtin_module_func(kernel_class, "loop", loop_, 0);
    globals.define_builtin_module_func_with_kw(
        kernel_class,
        "raise",
        raise,
        0,
        3,
        false,
        &["cause"],
        false,
    );
    // CRuby aliases `fail` to the same `rb_f_raise` callback (variadic).
    // Use the identical signature so `cause:` ends up at the same
    // `lfp.try_arg(3)` slot in the shared trampoline.
    globals.define_builtin_module_func_with_kw(
        kernel_class,
        "fail",
        raise,
        0,
        3,
        false,
        &["cause"],
        false,
    );
    globals.define_builtin_module_inline_func(
        kernel_class,
        "block_given?",
        block_given,
        inline_gen2!(kernel_block_given),
        0,
    );
    //globals.define_builtin_module_func_rest(kernel_class, "p", p);
    globals.define_builtin_module_func_rest(kernel_class, "format", format);
    globals.define_builtin_module_func_rest(kernel_class, "sprintf", format);
    globals.define_builtin_module_func_with(kernel_class, "rand", rand, 0, 1, false);
    globals.define_builtin_module_func_with_kw(
        kernel_class,
        "Integer",
        kernel_integer,
        1,
        2,
        false,
        &["exception"],
        false,
    );
    globals.define_builtin_module_func_with_kw(
        kernel_class,
        "Float",
        kernel_float,
        1,
        1,
        false,
        &["exception"],
        false,
    );
    globals.define_builtin_module_func_with_kw(
        kernel_class,
        "Complex",
        kernel_complex,
        1,
        2,
        false,
        &["exception"],
        false,
    );
    globals.define_builtin_module_func_with_kw(
        kernel_class,
        "Rational",
        kernel_rational,
        1,
        2,
        false,
        &["exception"],
        false,
    );
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
    globals.define_builtin_module_func_rest(kernel_class, "spawn", spawn);
    globals.define_builtin_module_func(kernel_class, "`", command, 1);
    globals.define_builtin_module_func(kernel_class, "fork", fork, 0);
    globals.define_builtin_module_func_with(
        kernel_class,
        "trap",
        crate::builtins::process::signal_trap,
        1,
        2,
        false,
    );
    globals.define_builtin_module_func_with(kernel_class, "sleep", sleep, 0, 1, false);
    globals.define_builtin_module_func_with(kernel_class, "abort", abort, 0, 1, false);
    globals.define_builtin_module_func_with(kernel_class, "exit", exit, 0, 1, false);
    globals.define_builtin_module_func_with(kernel_class, "exit!", exit_bang, 0, 1, false);
    globals.define_builtin_module_func(kernel_class, "at_exit", at_exit, 0);
    // Registry primitives backing `ObjectSpace.define_finalizer` /
    // `undefine_finalizer` (the public methods live in startup.rb, which
    // handles argument validation and the return value).
    globals.define_private_builtin_func(kernel_class, "__register_finalizer", register_finalizer, 2);
    globals.define_private_builtin_func(
        kernel_class,
        "__unregister_finalizer",
        unregister_finalizer,
        1,
    );
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
    // `__callee__` differs from `__method__` only for aliased methods
    // (it reports the called alias). monoruby does not track the
    // call-site alias, so it shares `__method__`'s resolution.
    globals.define_builtin_module_func(kernel_class, "__callee__", method_, 0);
    globals.define_builtin_module_func_with(kernel_class, "catch", catch_, 0, 1, false);
    globals.define_builtin_module_func_with(kernel_class, "throw", throw_, 1, 2, false);
    globals.define_builtin_func(kernel_class, "__assert", assert, 2);
    globals.define_builtin_func_with(kernel_class, "caller", caller, 0, 2, false);
    globals.define_builtin_func(kernel_class, "__dump", dump, 0);
    globals.define_builtin_func(kernel_class, "__check_stack", check_stack, 0);
    globals.define_builtin_func(kernel_class, "__instance_ty", instance_ty, 0);
    globals.define_builtin_func(
        kernel_class,
        "__set_lastline_in_caller",
        set_lastline_in_caller,
        1,
    );
    globals.define_builtin_func(kernel_class, "__backtrace_limit", backtrace_limit, 0);
    globals.define_builtin_module_func_with(kernel_class, "chomp", chomp, 0, 1, false);
    globals.define_builtin_module_func(kernel_class, "chop", chop, 0);
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

    //globals.define_builtin_inline_func(kernel_class, "____max", max, inline_gen!(inline_max), 2);
    //globals.define_builtin_inline_func(kernel_class, "____min", min, inline_gen!(inline_min), 2);

    // Kernel methods (matching CRuby: these are defined on Kernel, not Object)
    globals.define_builtin_func(kernel_class, "class", class, 0);
    globals.define_builtin_func(kernel_class, "hash", hash, 0);
    globals.define_builtin_func(kernel_class, "eql?", eql_, 1);
    globals.define_builtin_func(kernel_class, "dup", dup, 0);
    globals.define_builtin_func_with_kw(kernel_class, "clone", clone_val, 0, 0, false, &["freeze"], false);
    let init_copy_fid =
        globals.define_private_builtin_func(kernel_class, "initialize_copy", initialize_copy, 1);
    let init_clone_fid =
        globals.define_private_builtin_func(kernel_class, "initialize_clone", initialize_clone, 1);
    let init_dup_fid =
        globals.define_private_builtin_func(kernel_class, "initialize_dup", initialize_clone, 1);
    globals
        .store
        .set_default_copy_hooks(init_copy_fid, init_dup_fid, init_clone_fid);
    globals.define_builtin_funcs_rest(kernel_class, "enum_for", &["to_enum"], to_enum);
    globals.define_builtin_func_rest(kernel_class, "extend", extend);
    globals.define_builtin_inline_func(
        kernel_class,
        "object_id",
        super::object::object_id,
        inline_gen2!(super::object::object_object_id),
        0,
    );
    globals.define_builtin_inline_func_with(
        kernel_class,
        "respond_to?",
        respond_to,
        inline_gen2!(object_respond_to),
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
    globals.define_builtin_inline_funcs(
        kernel_class,
        "is_a?",
        &["kind_of?"],
        is_a,
        inline_gen2!(kernel_is_a),
        1,
    );
    globals.define_builtin_inline_funcs_with_kw(
        kernel_class,
        "send",
        &["__send__"],
        crate::builtins::send,
        inline_gen!(crate::builtins::object_send),
        0,
        0,
        true,
        &[],
        true,
    );
    globals.define_builtin_funcs_with_kw(
        kernel_class,
        "public_send",
        &[],
        public_send,
        0,
        0,
        true,
        &[],
        true,
    );
    globals.define_builtin_func(kernel_class, "method", method, 1);
    globals.define_builtin_func(kernel_class, "singleton_method", singleton_method, 1);
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
        "private_methods",
        private_methods,
        0,
        1,
        false,
    );
    globals.define_builtin_func_with(
        kernel_class,
        "protected_methods",
        protected_methods,
        0,
        1,
        false,
    );
    globals.define_builtin_func_with(kernel_class, "public_methods", public_methods, 0, 1, false);
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
        // Pure-LIR predicate (no arch-specific closure): Rax = (Rdi == nil).
        ir.is_nil_to_bool(GP::Rax, GP::Rdi);
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
    not_match_inner(vm, globals, lfp.self_val(), lfp.arg(0))
}

/// `!(lhs =~ rhs)`. The `=~` resolution is memoized per class_version
/// (`Store::match_method`), so the common case costs a load instead of a
/// global-method-cache probe per call. Receivers without `=~` keep the
/// uncached dispatch for its NoMethodError / method_missing semantics.
fn not_match_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Result<Value> {
    let res = if let Some(fid) = globals
        .store
        .match_method(lhs.class(), Globals::class_version())
    {
        vm.invoke_func_inner(globals, fid, lhs, &[rhs], None, None)?
    } else {
        vm.invoke_method_inner(globals, IdentId::_MATCH, lhs, &[rhs], None, None)?
    };
    Ok(Value::bool(!res.as_bool()))
}

/// Runtime arm of the inlined `Kernel#!~` (BinaryOpFn-shaped so the JIT
/// can emit it via the generic binop call, skipping the `Kernel#!~`
/// wrapper frame entirely).
pub(crate) extern "C" fn not_match_values(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    match not_match_inner(vm, globals, lhs, rhs) {
        Ok(v) => Some(v),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

/// JIT inliner for `Kernel#!~`: when the receiver class (guarded by the
/// inline dispatch) resolves `=~`, emit a direct `not_match_values` call
/// — no `Kernel#!~` frame, and the inner `=~` resolution is the
/// memoized load. Receivers without `=~` (NoMethodError path) and
/// non-simple callsites take the generic call.
fn kernel_not_match(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    ctx: &JitContext,
    store: &Store,
    callid: CallSiteId,
    recv_class: ClassId,
    _: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() || callsite.pos_num != 1 {
        return false;
    }
    // BOOL_CLASS is the IC pseudo-class for bool receivers; it has no
    // method table of its own to resolve `=~` against.
    if recv_class == BOOL_CLASS
        || store.match_method(recv_class, ctx.class_version()).is_none()
    {
        return false;
    }
    let CallSiteInfo {
        recv, args, dst, ..
    } = *callsite;
    state.write_back_recv_and_callargs(ir, callsite);
    let error = ir.new_error(state);
    ir.generic_binop(state, recv, args, not_match_values);
    ir.handle_error(error);
    // The dispatched `=~` can run arbitrary Ruby; invalidate cached
    // guards so subsequent instructions re-establish them.
    state.unset_class_version_guard();
    state.unset_const_version_guard();
    state.def_rax2acc(ir, dst);
    true
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
        // Pure-LIR Kernel#block_given? (no arch-specific closure).
        ir.block_given(GP::Rax);
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
fn puts(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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

    let to_s_id = IdentId::get_id("to_s");
    for v in collector {
        // Non-String values stringify through their (possibly
        // user- or Ruby-defined) `to_s` method, matching CRuby's
        // `rb_obj_as_string`; the Rust-side fallback only handles
        // objects whose `to_s` returns a non-String.
        if v.is_rstring().is_some() {
            globals.print_value(v)?;
        } else {
            let s = vm.invoke_method_inner(globals, to_s_id, v, &[], None, None)?;
            if s.is_rstring().is_some() {
                globals.print_value(s)?;
            } else {
                globals.print_value(v)?;
            }
        }
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
/// Where `Kernel#gets` currently reads from. CRuby's `gets` is
/// `ARGF.gets`: when `ARGV` holds file names they are consumed (shifted
/// off `ARGV`) and read in order; only an initially-empty `ARGV` reads
/// stdin. `Kernel#gets` is only ever called from the single Ruby
/// thread, so a process-global is fine.
enum ArgfSource {
    /// Not yet decided — first `gets` call picks stdin or the ARGV files.
    Uninit,
    Stdin,
    File(std::io::BufReader<std::fs::File>),
    /// All ARGV files exhausted.
    Done,
}

static ARGF_SOURCE: std::sync::Mutex<ArgfSource> = std::sync::Mutex::new(ArgfSource::Uninit);

/// Shift the next file name off `ARGV`, if any.
fn shift_argv(globals: &mut Globals) -> Option<String> {
    let argv = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("ARGV"))?;
    let mut ary = argv.try_array_ty()?;
    if ary.is_empty() {
        return None;
    }
    let first = ary[0];
    let name = first
        .is_rstring_inner()
        .map(|s| String::from_utf8_lossy(s.as_bytes()).into_owned())?;
    ary.remove(0);
    Some(name)
}

/// Read one record (up to and including the separator) from `reader`
/// into `buffer`. `sep` is the raw `$/` value: `None` slurps the whole
/// input, `Some(b"")` is paragraph mode.
fn read_record(
    reader: &mut dyn std::io::BufRead,
    sep: &Option<Vec<u8>>,
    buffer: &mut Vec<u8>,
) -> std::io::Result<usize> {
    match sep {
        None => reader.read_to_end(buffer),
        Some(sep) if sep.is_empty() => {
            // Paragraph mode: skip leading blank lines, then read
            // up to (and including) an empty line.
            let mut line: Vec<u8> = Vec::new();
            loop {
                line.clear();
                if reader.read_until(b'\n', &mut line)? == 0 {
                    return Ok(buffer.len());
                }
                if line != b"\n" {
                    buffer.extend_from_slice(&line);
                    break;
                }
            }
            loop {
                line.clear();
                if reader.read_until(b'\n', &mut line)? == 0 {
                    return Ok(buffer.len());
                }
                buffer.extend_from_slice(&line);
                if line == b"\n" {
                    return Ok(buffer.len());
                }
            }
        }
        Some(sep) => {
            let last = *sep.last().unwrap();
            loop {
                if reader.read_until(last, buffer)? == 0 {
                    return Ok(buffer.len());
                }
                if buffer.ends_with(sep) {
                    return Ok(buffer.len());
                }
            }
        }
    }
}

#[monoruby_builtin]
fn gets(vm: &mut Executor, globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // Honour the input record separator `$/` (set by the `-0`
    // command-line switch or by assignment): nil slurps the whole
    // input, "" is paragraph mode, any other string reads up to and
    // including that byte sequence.
    let sep: Option<Vec<u8>> = match globals.get_gvar(IdentId::get_id("$/")) {
        Some(v) if v.is_nil() => None,
        Some(v) => match v.is_rstring_inner() {
            Some(s) => Some(s.as_bytes().to_vec()),
            None => Some(b"\n".to_vec()),
        },
        None => Some(b"\n".to_vec()),
    };
    let mut buffer: Vec<u8> = Vec::new();
    let mut source = ARGF_SOURCE.lock().unwrap();
    let n = loop {
        match &mut *source {
            ArgfSource::Uninit => match shift_argv(globals) {
                Some(name) if name != "-" => match std::fs::File::open(&name) {
                    Ok(f) => *source = ArgfSource::File(std::io::BufReader::new(f)),
                    Err(_) => {
                        return Err(MonorubyErr::runtimeerr(format!(
                            "No such file or directory @ rb_sysopen - {name}"
                        )));
                    }
                },
                Some(_) | None => *source = ArgfSource::Stdin,
            },
            ArgfSource::Stdin => {
                let stdin = std::io::stdin();
                let mut lock = stdin.lock();
                break read_record(&mut lock, &sep, &mut buffer).unwrap_or(0);
            }
            ArgfSource::File(reader) => {
                let n = read_record(reader, &sep, &mut buffer).unwrap_or(0);
                if n > 0 {
                    break n;
                }
                // Current file exhausted: move on to the next ARGV
                // entry, or report end-of-input when none remain.
                match shift_argv(globals) {
                    Some(name) if name != "-" => match std::fs::File::open(&name) {
                        Ok(f) => *source = ArgfSource::File(std::io::BufReader::new(f)),
                        Err(_) => {
                            return Err(MonorubyErr::runtimeerr(format!(
                                "No such file or directory @ rb_sysopen - {name}"
                            )));
                        }
                    },
                    Some(_) => *source = ArgfSource::Stdin,
                    None => *source = ArgfSource::Done,
                }
            }
            ArgfSource::Done => break 0,
        }
    };
    drop(source);
    // Zero bytes read means end-of-input. CRuby's `gets` then returns
    // nil (and `while gets` terminates); returning the empty buffer
    // string here would loop forever since "" is truthy.
    if n == 0 {
        vm.set_last_read_line(Value::nil());
        return Ok(Value::nil());
    }
    // Bump `$.` (input line number) and set `$_` (frame-local last
    // read line), matching CRuby.
    let lineno = globals
        .get_gvar(IdentId::get_id("$."))
        .and_then(|v| v.try_fixnum())
        .unwrap_or(0)
        + 1;
    globals.set_gvar(IdentId::get_id("$."), Value::integer(lineno));
    let s = Value::string_from_vec(buffer);
    vm.set_last_read_line(s);
    Ok(s)
}

///
/// ### Kernel.#chomp / Kernel.#chop
///
/// - chomp(rs = $/) -> String
/// - chop -> String
///
/// `$_ = $_.chomp(rs)` / `$_ = $_.chop` in the calling scope — the
/// `-n`/`-p` loop companions. `$_` must hold a String (TypeError
/// otherwise, like CRuby).
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/chomp.html]
#[monoruby_builtin]
fn chomp(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let line = vm.get_last_read_line();
    if line.is_rstring().is_none() {
        return Err(MonorubyErr::typeerr(format!(
            "$_ value need to be String ({} given)",
            line.get_real_class_name(&globals.store)
        )));
    }
    let rs = match lfp.try_arg(0) {
        Some(rs) => rs,
        None => globals
            .get_gvar(IdentId::get_id("$/"))
            .unwrap_or_else(|| Value::string_from_str("\n")),
    };
    let res = vm.invoke_method_inner(globals, IdentId::get_id("chomp"), line, &[rs], None, None)?;
    vm.set_last_read_line(res);
    Ok(res)
}

/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/chop.html]
#[monoruby_builtin]
fn chop(vm: &mut Executor, globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let line = vm.get_last_read_line();
    if line.is_rstring().is_none() {
        return Err(MonorubyErr::typeerr(format!(
            "$_ value need to be String ({} given)",
            line.get_real_class_name(&globals.store)
        )));
    }
    let res = vm.invoke_method_inner(globals, IdentId::get_id("chop"), line, &[], None, None)?;
    vm.set_last_read_line(res);
    Ok(res)
}

///
/// ### Kernel.#__backtrace_limit (monoruby intrinsic)
///
/// The `--backtrace-limit=N` command-line value, or nil. Used by
/// `Exception#full_message` (startup.rb) to truncate caller frames the
/// way the top-level uncaught-error report does.
///
#[monoruby_builtin]
fn backtrace_limit(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(match crate::globals::backtrace_limit() {
        Some(n) => Value::integer(n as i64),
        None => Value::nil(),
    })
}

///
/// ### Kernel.#__set_lastline_in_caller (monoruby intrinsic)
///
/// Sets `$_` in the method scope of the frame that called the invoking
/// Ruby method. Used by Ruby-implemented IO-like stubs (`stringio.rb`)
/// whose `gets`/`readline` must publish `$_` to their caller the way
/// CRuby's C readers do via `rb_lastline_set`. Returns its argument.
///
#[monoruby_builtin]
fn set_lastline_in_caller(
    vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let val = lfp.arg(0);
    vm.set_last_read_line_in_caller(val);
    Ok(val)
}

///
/// ### Kernel.#print
///
/// - print(*arg) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/print.html]
#[monoruby_builtin]
fn print(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    // Delegate to `$stdout.print` so output-stream reassignment is
    // honoured (and so the `IO#print` write path stays the single
    // source of truth), reading `$stdout` dynamically each call.
    let stdout = globals
        .get_gvar(IdentId::get_id("$stdout"))
        .unwrap_or_default();
    let print_id = IdentId::get_id("print");
    if args.len() == 0 {
        // `print` with no arguments writes `$_` (the caller's
        // frame-local last-read line). A `nil` `$_` prints nothing.
        // `vm.get_last_read_line` resolves the LEP past this native
        // frame, so it sees the Ruby caller's `$_`, not print's own.
        let lastline = vm.get_last_read_line();
        if lastline.is_nil() {
            return Ok(Value::nil());
        }
        vm.invoke_method_inner(globals, print_id, stdout, &[lastline], None, None)?;
    } else {
        let argvec: Vec<Value> = args.iter().cloned().collect();
        vm.invoke_method_inner(globals, print_id, stdout, &argvec, None, None)?;
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
fn proc(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        let p = vm.generate_proc(globals, bh, pc)?;
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
        let p = vm.generate_proc(globals, bh, pc)?;
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
/// ### Kernel.#local_variables
///
/// - local_variables -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/local_variables.html]
#[monoruby_builtin]
fn local_variables(vm: &mut Executor, globals: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    // Report the caller's local variables (the frame that invoked us).
    let caller_cfp = vm.cfp().prev().unwrap();
    let fid = caller_cfp.lfp().func_id();
    let v = match globals.store[fid].is_iseq() {
        Some(iseq) => globals.store.local_variables(iseq),
        None => vec![],
    };
    Ok(Value::array_from_vec(v))
}

///
/// ### Kernel.#loop
///
/// - loop { ... } -> object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/loop.html]
#[monoruby_builtin]
fn loop_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let Some(bh) = lfp.block() else {
        // No block: return an infinite Enumerator (size == Float::INFINITY).
        let method = IdentId::get_id("loop");
        let size = Value::float(f64::INFINITY);
        return vm.generate_enumerator_with_size(method, lfp.self_val(), vec![], pc, Some(size));
    };
    let data = vm.get_block_data(globals, bh)?;
    loop {
        if let Err(err) = vm.invoke_block(globals, &data, &[]) {
            // `loop` swallows StopIteration and any user subclass of it, and
            // re-raises everything else (including control-flow signals).
            return if err.is_stop_iteration(&globals.store) {
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
/// A `SystemExit` (or subclass) exception object carries its exit code
/// in the `/status` ivar, but `MonorubyErrKind::SystemExit` is baked to
/// 0 at construction (`from_class_id`). When such an object is raised,
/// sync the kind's status from the ivar so the process exits with the
/// requested code (`raise SystemExit.new(7)` → exit 7).
fn fix_system_exit_status(globals: &mut Globals, err: &mut MonorubyErr, ex: Value) {
    // Both `SystemExit` itself (kind already `SystemExit`) and any user
    // subclass (kind `Other`, but a SystemExit descendant) exit the
    // process silently with the requested code.
    let is_system_exit = matches!(err.kind, MonorubyErrKind::SystemExit(_)) || {
        let se = globals.store[SYSTEM_EXIT_ERROR_CLASS].get_module();
        se.is_ancestor_of(globals.store[ex.class()].get_module())
    };
    if is_system_exit {
        let status = globals
            .store
            .get_ivar(ex, IdentId::get_id("/status"))
            .and_then(|v| v.try_fixnum())
            .unwrap_or(0);
        err.kind = MonorubyErrKind::SystemExit(status as u8);
    }
}

#[monoruby_builtin]
fn raise(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // `cause:` kwarg is at slot 3 (positional_max=3 for the shared
    // raise/fail trampoline). CRuby: passing `cause:` with no
    // positional arguments raises `ArgumentError "only cause is given
    // with no arguments"`.
    let cause_kwarg = lfp.try_arg(3);
    if lfp.try_arg(0).is_none() {
        if cause_kwarg.is_some() {
            return Err(MonorubyErr::argumenterr(
                "only cause is given with no arguments",
            ));
        }
        let ex = vm.errinfo();
        if let Some(inner) = ex.is_exception() {
            let mut err = MonorubyErr::new_from_exception(inner);
            fix_system_exit_status(globals, &mut err, ex);
            return Err(err.with_original(ex));
        } else {
            return Err(MonorubyErr::runtimeerr(""));
        }
    }
    if let Some(ex) = lfp.arg(0).is_exception() {
        let mut err = MonorubyErr::new_from_exception(ex);
        fix_system_exit_status(globals, &mut err, lfp.arg(0));
        if let Some(arg1) = lfp.try_arg(1) {
            if arg1.try_hash_ty().is_none() {
                // A message override makes CRuby return a *new* exception
                // (`exc.exception(msg)`), so identity is not preserved.
                err.set_msg(arg1.coerce_to_str(vm, globals)?);
                return Err(apply_cause(globals, err, None, cause_kwarg)?);
            }
        }
        let raised = lfp.arg(0);
        return Err(apply_cause(globals, err.with_original(raised), Some(raised), cause_kwarg)?);
    } else if let Some(klass) = lfp.arg(0).is_class() {
        if klass.id() == STOP_ITERATION_CLASS {
            let err = MonorubyErr::stopiterationerr("".to_string());
            return Err(apply_cause(globals, err, None, cause_kwarg)?);
        } else if klass.is_exception() {
            let mut args = vec![];
            if let Some(arg1) = lfp.try_arg(1) {
                if arg1.try_hash_ty().is_none() {
                    args.push(arg1);
                }
            }
            let ex =
                vm.invoke_method_inner(globals, IdentId::NEW, klass.as_val(), &args, None, None)?;
            let mut err = MonorubyErr::new_from_exception(ex.is_exception().unwrap());
            fix_system_exit_status(globals, &mut err, ex);
            let err = err.with_original(ex);
            return Err(apply_cause(globals, err, Some(ex), cause_kwarg)?);
        }
    } else if let Some(message) = lfp.arg(0).is_rstring() {
        let err = MonorubyErr::runtimeerr(message.to_str()?);
        return Err(apply_cause(globals, err, None, cause_kwarg)?);
    }
    // Duck-typed exception. CRuby drives `arg0.exception(msg)` (or
    // `arg0.exception` if no message was given); the result must be
    // an Exception instance — anything else is `TypeError "exception
    // object expected"` (note: distinct from the
    // "exception class/object expected" emitted when arg0 doesn't
    // even respond to `#exception`).
    let exception_id = IdentId::get_id("exception");
    if globals.check_method(lfp.arg(0), exception_id).is_some() {
        let mut args = vec![];
        if let Some(arg1) = lfp.try_arg(1) {
            if arg1.try_hash_ty().is_none() {
                args.push(arg1);
            }
        }
        let result =
            vm.invoke_method_inner(globals, exception_id, lfp.arg(0), &args, None, None)?;
        match result.is_exception() {
            Some(ex) => {
                let err = MonorubyErr::new_from_exception(ex).with_original(result);
                return Err(apply_cause(globals, err, Some(result), cause_kwarg)?);
            }
            None => return Err(MonorubyErr::typeerr("exception object expected")),
        }
    }
    Err(MonorubyErr::typeerr("exception class/object expected"))
}

/// Apply an explicit `cause:` keyword to a to-be-raised error. `raised`
/// is the exception object being raised, when known (for identity and
/// cycle checks). Returns the error to raise — either the original error
/// tagged with the cause, or a validation error (`TypeError` for a
/// non-exception cause, `ArgumentError` for a circular cause).
fn apply_cause(
    globals: &Globals,
    mut err: MonorubyErr,
    raised: Option<Value>,
    cause_kwarg: Option<Value>,
) -> Result<MonorubyErr> {
    let Some(cause) = cause_kwarg else {
        return Ok(err);
    };
    if !cause.is_nil() && cause.is_exception().is_none() {
        return Err(MonorubyErr::typeerr("exception object expected"));
    }
    let cause = match raised {
        // A cause equal to the raised exception itself is ignored.
        Some(raised) if !cause.is_nil() && cause.id() == raised.id() => Value::nil(),
        // Setting a cause whose chain already reaches the raised exception
        // would create a cycle.
        Some(raised)
            if !cause.is_nil() && cause_chain_contains(globals, cause, raised) =>
        {
            return Err(MonorubyErr::argumenterr("circular causes"));
        }
        _ => cause,
    };
    err.explicit_cause = Some(cause);
    Ok(err)
}

/// Whether `target` appears in the `#cause` chain reachable from `start`.
fn cause_chain_contains(globals: &Globals, start: Value, target: Value) -> bool {
    let cause_id = IdentId::get_id("/cause");
    let mut cur = start;
    loop {
        if cur.id() == target.id() {
            return true;
        }
        match globals.store.get_ivar(cur, cause_id) {
            Some(next) if !next.is_nil() => cur = next,
            _ => return false,
        }
    }
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
    let fmt_val = args[0];
    let fmt = fmt_val.coerce_to_str(vm, globals)?;
    let arguments = &args[1..];
    let result = vm.format_by_args(globals, &fmt, arguments)?;
    // Negotiate the result encoding across the format String and every String
    // argument (matching String#%): the compatible union wins (so the format's
    // encoding is preserved, or widened to an argument's), and two
    // ASCII-incompatible non-ASCII encodings raise Encoding::CompatibilityError.
    if let Some(fmt_inner) = fmt_val.is_rstring_inner() {
        let mut result_inner = fmt_inner.clone();
        for v in arguments {
            if let Some(arg_inner) = v.is_rstring_inner() {
                match result_inner.compatible_encoding(arg_inner) {
                    Some(combined) if combined != result_inner.encoding() => {
                        result_inner.set_encoding(combined);
                    }
                    Some(_) => {}
                    None => {
                        return Err(MonorubyErr::incompatible_encoding(
                            &globals.store,
                            result_inner.encoding(),
                            arg_inner.encoding(),
                        ));
                    }
                }
            }
        }
        Ok(Value::string_from_inner(RStringInner::from_encoding_scanned(
            result.as_bytes(),
            result_inner.encoding(),
        )))
    } else {
        Ok(Value::string(result))
    }
}

///
/// ### Kernel.#caller
///
/// - caller(start = 1) -> [String] | nil
/// - caller(start, length) -> [String] | nil
/// - caller(range) -> [String] | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/caller.html]
#[monoruby_builtin]
fn caller(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut cfp = vm.cfp();
    let mut v = Vec::new();
    // Resolve the arguments into the first frame to report (`level` — the
    // number of cfp frames to skip, which includes this builtin's own
    // frame, hence the `+ 1`) and an optional cap on how many frames to
    // return (`length`):
    //   caller            -> start 1, no cap
    //   caller(start)     -> start, no cap
    //   caller(start, n)  -> start, at most n frames
    //   caller(range)     -> a (possibly beginless / endless) range of frames
    let (level, length): (usize, Option<usize>) = match lfp.try_arg(0) {
        None => (2, None),
        Some(arg0) => {
            if let Some(range) = arg0.is_range() {
                // Beginless range starts at the immediate caller (0).
                let start = range.start().try_fixnum().unwrap_or(0).max(0) as usize;
                // Endless range has no cap; otherwise the count is
                // end - start (one more when the end is inclusive).
                let length = range.end().try_fixnum().map(|end| {
                    let end = if range.exclude_end() { end } else { end + 1 };
                    (end - start as i64).max(0) as usize
                });
                (start + 1, length)
            } else {
                let start = arg0.coerce_to_int_i64(vm, globals)?.max(0) as usize;
                let length = match lfp.try_arg(1) {
                    Some(arg1) => Some(arg1.coerce_to_int_i64(vm, globals)?.max(0) as usize),
                    None => None,
                };
                (start + 1, length)
            }
        }
    };
    // The frame iterated in the previous step (this frame's callee),
    // whose cont-frame slot holds this frame's suspended pc.
    let mut inner_cfp: Option<crate::executor::Cfp> = None;
    for i in 0..16 {
        let prev_cfp = cfp.prev();
        if i >= level {
            // Stop once `length` frames have been collected.
            if let Some(len) = length
                && v.len() >= len
            {
                break;
            }
            let func_id = cfp.lfp().func_id();
            if let Some(iseq) = globals.store[func_id].is_iseq() {
                let info = &globals.store[iseq];
                // The frame's *current* line: the pc it saved into its
                // callee's cont-frame slot when it suspended (one past
                // the call instruction). Falls back to the method's
                // start location when the slot is absent (invoker
                // boundary, unaudited dispatch path) or out of range.
                let loc = inner_cfp
                    .and_then(|inner| {
                        let slot = inner.caller_pc_slot();
                        if slot == 0 || slot % 8 != 0 {
                            return None;
                        }
                        // SAFETY: validated against the bytecode span below.
                        let pc = unsafe {
                            crate::bytecode::BytecodePtr::from_raw(slot as *mut _)?
                        };
                        // Both backends store the *call-site* pc.
                        if !info.contains_pc(pc) {
                            if std::env::var_os("MONORUBY_DEBUG_CALLERPC").is_some() {
                                eprintln!(
                                    "CALLERPC REJECT slot={slot:#x} fid={func_id:?} iseq_top={:#x}+{}",
                                    info.get_top_pc().as_ptr() as usize,
                                    info.sourcemap.len() * 16,
                                );
                            }
                            return None;
                        }
                        let idx = info.get_pc_index(Some(pc)).to_usize();
                        let loc = info.sourcemap[idx];
                        // CRuby prints the path as the file was loaded
                        // (absolute for absolute requires) — match it.
                        Some(format!(
                            "{}:{}",
                            info.sourceinfo.file_name(),
                            info.sourceinfo.get_line(&loc)
                        ))
                    })
                    .unwrap_or_else(|| info.get_location());
                let desc = globals.store.func_description(func_id);
                // CRuby 3.4+ delimits the label with single quotes
                // ("…:in '<main>'") instead of the legacy backticks;
                // monoruby targets Ruby 4.0 so we follow suit. Keeps
                // `caller_locations` (which regex-parses these
                // strings) able to extract the label.
                v.push(Value::string_from_vec(
                    format!("{loc}:in '{desc}'").into_bytes(),
                ));
            }
        }
        if let Some(prev_cfp) = prev_cfp {
            inner_cfp = Some(cfp);
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
    // `exception:` keyword is stored after the positional max (2).
    let exception = lfp.try_arg(2).map_or(true, |v| v.as_bool());
    match kernel_integer_inner(vm, globals, lfp) {
        Ok(v) => Ok(v),
        Err(e) => {
            if exception {
                Err(e)
            } else {
                Ok(Value::nil())
            }
        }
    }
}

/// Build a `FloatDomainError` (falls back to `RangeError` if the constant
/// is unavailable).
fn float_domain_error(globals: &Globals, msg: &str) -> MonorubyErr {
    match globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("FloatDomainError"))
        .map(|v| v.as_class_id())
    {
        Some(cid) => MonorubyErr::new(MonorubyErrKind::Other(cid), msg),
        None => MonorubyErr::rangeerr(msg),
    }
}

fn kernel_integer_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
) -> Result<Value> {
    let arg0 = lfp.arg(0);
    // nil -> TypeError ("can't convert nil into Integer"), always; under
    // `exception: false` the outer wrapper turns this into nil.
    if arg0.is_nil() {
        return Err(MonorubyErr::typeerr("can't convert nil into Integer"));
    }
    // Optional second argument: explicit `base` (0 = autodetect from
    // the string's prefix; 2..=36 = forced base, with the prefix
    // optional — `Integer("0x42", 16)` is ok but `("0x42", 10)` errors).
    let base = if let Some(b) = lfp.try_arg(1) {
        let b = b.coerce_to_int_i64(vm, globals)?;
        if b < 0 || b > 36 || b == 1 {
            return Err(MonorubyErr::argumenterr(format!("invalid radix {}", b)));
        }
        Some(b as u32)
    } else {
        None
    };
    match arg0.unpack() {
        RV::Fixnum(num) => return Ok(Value::integer(num)),
        RV::BigInt(num) => return Ok(Value::bigint(num.clone())),
        RV::Float(num) => {
            if num.is_nan() {
                return Err(float_domain_error(globals, "NaN"));
            }
            if num.is_infinite() {
                return Err(float_domain_error(
                    globals,
                    if num < 0.0 { "-Infinity" } else { "Infinity" },
                ));
            }
            return Ok(Value::integer(num.trunc() as i64));
        }
        RV::String(b) => {
            let s = b.check_utf8()?;
            return parse_kernel_integer(s, base.unwrap_or(0));
        }
        _ => {}
    };
    // Try to_int coercion. If it returns a non-Integer (or nil), fall
    // through to `to_i` instead of erroring immediately (CRuby semantics).
    if let Some(func_id) = globals.check_method(arg0, IdentId::TO_INT) {
        let result = vm.invoke_func_inner(globals, func_id, arg0, &[], None, None)?;
        match result.unpack() {
            RV::Fixnum(i) => return Ok(Value::integer(i)),
            RV::BigInt(b) => return Ok(Value::bigint(b.clone())),
            _ => {}
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

/// Parse `s` as an integer literal the way `Kernel#Integer(s, base)`
/// does. `base` may be 0 (auto-detect from a `0x`/`0b`/`0o`/`0d`
/// prefix or a leading `0`) or a value in `2..=36`. Returns the same
/// `ArgumentError` shape CRuby produces (the message embeds the
/// original input, quoted).
pub(crate) fn parse_kernel_integer(s: &str, given_base: u32) -> Result<Value> {
    let raw = s;
    let invalid = || MonorubyErr::argumenterr(format!("invalid value for Integer(): \"{}\"", raw));
    // CRuby trims leading/trailing whitespace.
    let trimmed = s.trim();
    if trimmed.is_empty() {
        return Err(invalid());
    }
    let bytes = trimmed.as_bytes();
    let mut i = 0;
    // Sign.
    let neg = match bytes[i] {
        b'+' => {
            i += 1;
            false
        }
        b'-' => {
            i += 1;
            true
        }
        _ => false,
    };
    if i >= bytes.len() {
        return Err(invalid());
    }
    // Prefix detection. When `given_base == 0`, infer from the prefix
    // (or a leading `0` ⇒ octal). When `given_base != 0`, accept a
    // matching prefix and reject a conflicting one.
    let (effective_base, mut digits_start) = detect_base(&bytes[i..], given_base, &invalid)?;
    digits_start += i;
    let digits = &bytes[digits_start..];
    if digits.is_empty() {
        return Err(invalid());
    }
    // Walk digits, allowing underscore as a digit-separator (must
    // appear strictly between two digit characters).
    let mut acc_str = String::with_capacity(digits.len());
    let mut prev_was_digit = false;
    for (idx, &b) in digits.iter().enumerate() {
        if b == b'_' {
            // `_` is illegal at the start, at the end, or right after
            // another `_`.
            if !prev_was_digit || idx + 1 == digits.len() {
                return Err(invalid());
            }
            prev_was_digit = false;
            continue;
        }
        if !is_digit_for_base(b, effective_base) {
            return Err(invalid());
        }
        acc_str.push(b as char);
        prev_was_digit = true;
    }
    if acc_str.is_empty() {
        return Err(invalid());
    }
    // Try i64 first; fall back to BigInt for values that overflow.
    if let Ok(n) = i64::from_str_radix(&acc_str, effective_base) {
        let signed = if neg { -n } else { n };
        return Ok(Value::integer(signed));
    }
    let big = num::BigInt::parse_bytes(acc_str.as_bytes(), effective_base).ok_or_else(invalid)?;
    let signed = if neg { -big } else { big };
    Ok(Value::bigint(signed))
}

fn detect_base(
    bytes: &[u8],
    given_base: u32,
    invalid: &dyn Fn() -> MonorubyErr,
) -> Result<(u32, usize)> {
    // Look at the leading two bytes for `0X`-style prefixes.
    let prefix_base = if bytes.len() >= 2 && bytes[0] == b'0' {
        match bytes[1] {
            b'x' | b'X' => Some((16, 2)),
            b'b' | b'B' => Some((2, 2)),
            b'o' | b'O' => Some((8, 2)),
            b'd' | b'D' => Some((10, 2)),
            _ => None,
        }
    } else {
        None
    };
    match (given_base, prefix_base) {
        (0, Some((b, off))) => Ok((b, off)),
        // Leading `0` followed by digit(s) → octal under autodetect.
        (0, None) if bytes.len() >= 2 && bytes[0] == b'0' && bytes[1].is_ascii_digit() => {
            Ok((8, 1))
        }
        (0, None) => Ok((10, 0)),
        (b, Some((pb, off))) if b == pb => Ok((b, off)),
        (_, Some(_)) => Err(invalid()),
        (b, None) => Ok((b, 0)),
    }
}

fn is_digit_for_base(b: u8, base: u32) -> bool {
    let d = match b {
        b'0'..=b'9' => (b - b'0') as u32,
        b'a'..=b'z' => (b - b'a' + 10) as u32,
        b'A'..=b'Z' => (b - b'A' + 10) as u32,
        _ => return false,
    };
    d < base
}

/// Parse `s` as `Kernel#Float(s)` does. Handles:
///   - Optional leading/trailing whitespace, sign.
///   - Optional `0x`/`0X` prefix → hex float
///     (`0xH.HHHpEXP`, with `p`/`P` mandatory if there's a fraction).
///   - Decimal float `D.D[eE][+-]?D` with `_` allowed only between
///     two digit characters.
///   - Trailing characters / underscores at boundaries / consecutive
///     underscores / `nan` / `inf` are rejected (CRuby behavior).
///
/// On any structural problem returns `ArgumentError("invalid value
/// for Float(): \"...\"")` with the original input quoted.
pub(crate) fn parse_kernel_float(s: &str) -> Result<Value> {
    let raw = s;
    let invalid = || MonorubyErr::argumenterr(format!("invalid value for Float(): \"{}\"", raw));
    let trimmed = s.trim();
    if trimmed.is_empty() {
        return Err(invalid());
    }
    let bytes = trimmed.as_bytes();
    let mut i = 0;
    let neg = match bytes[i] {
        b'+' => {
            i += 1;
            false
        }
        b'-' => {
            i += 1;
            true
        }
        _ => false,
    };
    if i >= bytes.len() {
        return Err(invalid());
    }
    // Hex prefix branch.
    if bytes.len() >= i + 2 && bytes[i] == b'0' && (bytes[i + 1] == b'x' || bytes[i + 1] == b'X') {
        let payload = strip_underscores(&bytes[i + 2..], true, &invalid)?;
        let f = parse_hex_float(&payload).ok_or_else(invalid)?;
        return Ok(Value::float(if neg { -f } else { f }));
    }
    // Decimal branch.
    let payload = strip_underscores(&bytes[i..], false, &invalid)?;
    let s = std::str::from_utf8(&payload).map_err(|_| invalid())?;
    // Reject CRuby-rejected forms that Rust's parser would accept.
    if s.is_empty()
        || s == "."
        || s.eq_ignore_ascii_case("nan")
        || s.eq_ignore_ascii_case("inf")
        || s.eq_ignore_ascii_case("infinity")
    {
        return Err(invalid());
    }
    // CRuby allows a trailing `.` (e.g. `"10."`) but Rust's `f64::from_str`
    // does too, so no special handling needed.
    let f: f64 = s.parse().map_err(|_| invalid())?;
    // Reaching here, the literal `nan`/`inf`/`infinity` spellings were
    // already rejected, so a non-finite result is numeric overflow of a
    // valid literal (e.g. `"2e1000"`) which CRuby maps to Infinity.
    if f.is_nan() {
        return Err(invalid());
    }
    Ok(Value::float(if neg { -f } else { f }))
}

/// Strip underscores from a digit run, validating that each `_` sits
/// strictly between two digit characters. For a decimal payload only
/// `0-9` count as digits (so `2e_100`, `2_e100`, `0x1p_3` are rejected);
/// for a hex-float mantissa `0-9a-fA-F` count.
fn strip_underscores(
    bytes: &[u8],
    hex: bool,
    invalid: &dyn Fn() -> MonorubyErr,
) -> Result<Vec<u8>> {
    let mut out = Vec::with_capacity(bytes.len());
    let is_digit = |b: u8| {
        if hex {
            b.is_ascii_hexdigit()
        } else {
            b.is_ascii_digit()
        }
    };
    for (i, &b) in bytes.iter().enumerate() {
        if b == b'_' {
            let prev_ok = i > 0 && is_digit(bytes[i - 1]);
            let next_ok = i + 1 < bytes.len() && is_digit(bytes[i + 1]);
            if !prev_ok || !next_ok {
                return Err(invalid());
            }
            continue;
        }
        out.push(b);
    }
    Ok(out)
}

/// Parse a hex float of the form `H.HHH[pE]` (e.g. `0xA`, `0xA.B`,
/// `0x1.8p+3`). The leading `0x` has already been consumed.
fn parse_hex_float(bytes: &[u8]) -> Option<f64> {
    if bytes.is_empty() {
        return None;
    }
    let s = std::str::from_utf8(bytes).ok()?;
    let (mantissa_part, exp_part) = match s.find(|c: char| c == 'p' || c == 'P') {
        Some(idx) => (&s[..idx], Some(&s[idx + 1..])),
        None => (s, None),
    };
    if mantissa_part.is_empty() {
        return None;
    }
    let (int_part, frac_part) = match mantissa_part.find('.') {
        Some(idx) => (&mantissa_part[..idx], Some(&mantissa_part[idx + 1..])),
        None => (mantissa_part, None),
    };
    if int_part.is_empty() && frac_part.map(|f| f.is_empty()).unwrap_or(true) {
        return None;
    }
    if !int_part.is_empty() && !int_part.bytes().all(|b| b.is_ascii_hexdigit()) {
        return None;
    }
    if let Some(f) = frac_part {
        if !f.is_empty() && !f.bytes().all(|b| b.is_ascii_hexdigit()) {
            return None;
        }
    }
    let mut value: f64 = 0.0;
    for b in int_part.bytes() {
        value = value * 16.0 + hex_digit_value(b)? as f64;
    }
    if let Some(frac) = frac_part {
        let mut scale = 1.0 / 16.0;
        for b in frac.bytes() {
            value += hex_digit_value(b)? as f64 * scale;
            scale /= 16.0;
        }
    }
    if let Some(e) = exp_part {
        let (esign, edigits) = match e.as_bytes().first() {
            Some(&b'+') => (1.0, &e[1..]),
            Some(&b'-') => (-1.0, &e[1..]),
            _ => (1.0, e),
        };
        if edigits.is_empty() || !edigits.bytes().all(|b| b.is_ascii_digit()) {
            return None;
        }
        let exp: i32 = edigits.parse().ok()?;
        value *= 2f64.powi(exp * esign as i32);
    }
    Some(value)
}

fn hex_digit_value(b: u8) -> Option<u32> {
    match b {
        b'0'..=b'9' => Some((b - b'0') as u32),
        b'a'..=b'f' => Some((b - b'a' + 10) as u32),
        b'A'..=b'F' => Some((b - b'A' + 10) as u32),
        _ => None,
    }
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
    // `exception:` keyword is stored after the positional max (1).
    let exception = lfp.try_arg(1).map_or(true, |v| v.as_bool());
    match kernel_float_inner(vm, globals, lfp) {
        Ok(v) => Ok(v),
        Err(e) => {
            if exception {
                Err(e)
            } else {
                Ok(Value::nil())
            }
        }
    }
}

fn kernel_float_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
) -> Result<Value> {
    let arg0 = lfp.arg(0);
    if arg0.is_nil() {
        return Err(MonorubyErr::typeerr("can't convert nil into Float"));
    }
    match arg0.unpack() {
        RV::Fixnum(num) => return Ok(Value::float(num as f64)),
        RV::BigInt(num) => return Ok(Value::float(num.to_f64().unwrap())),
        RV::Float(num) => return Ok(Value::float(num)),
        RV::String(b) => {
            let s = b.to_str()?;
            return parse_kernel_float(&s);
        }
        _ => {}
    };
    // Try to_f coercion. CRuby requires `#to_f` to return a Float; an
    // Integer (or anything else) is a TypeError.
    if let Some(func_id) = globals.check_method(arg0, IdentId::TO_F) {
        let result = vm.invoke_func_inner(globals, func_id, arg0, &[], None, None)?;
        match result.unpack() {
            RV::Float(f) => return Ok(Value::float(f)),
            _ => {
                return Err(MonorubyErr::cant_convert_error_f(globals, arg0, result));
            }
        }
    }
    Err(MonorubyErr::cant_convert_into_float(globals, arg0))
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
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    // `exception:` keyword is stored after the positional max (2).
    let exception = lfp.try_arg(2).map_or(true, |v| v.as_bool());
    let arg0 = lfp.arg(0);
    let arg1 = lfp.try_arg(1);

    // nil argument -> TypeError, always raised (even with exception: false).
    if arg0.is_nil() || arg1.map_or(false, |v| v.is_nil()) {
        if exception {
            return Err(MonorubyErr::typeerr("can't convert nil into Complex"));
        }
        return Ok(Value::nil());
    }

    // Single String argument: parse it. Parse errors are ArgumentError /
    // Encoding::CompatibilityError. With `exception: false`, the
    // Encoding::CompatibilityError is still raised but ArgumentError is
    // swallowed (-> nil).
    if arg1.is_none() {
        if let RV::String(b) = arg0.unpack() {
            return match parse_complex_string(globals, b) {
                Ok(v) => Ok(v),
                Err(e) => {
                    if exception || e.kind() != &MonorubyErrKind::Arguments {
                        Err(e)
                    } else {
                        Ok(Value::nil())
                    }
                }
            };
        }
    }

    // When a second argument is present, evaluate it first: a non-Numeric
    // second argument is always swallowable with `exception: false`
    // (`Complex(0, :sym, exception: false)` -> nil), whereas a non-Numeric
    // first argument paired with a *Numeric* second argument always raises
    // (`Complex(:sym, 0, exception: false)` -> TypeError "not a real").
    if let Some(i) = arg1 {
        let im = match real_for_complex(vm, globals, i)? {
            Some(i) => i,
            None => {
                if exception {
                    return Err(MonorubyErr::typeerr("not a real"));
                }
                return Ok(Value::nil());
            }
        };
        let re = match real_for_complex(vm, globals, arg0)? {
            Some(r) => r,
            None => return Err(MonorubyErr::typeerr("not a real")),
        };
        return Ok(Value::complex(re, im));
    }

    let r = match real_for_complex(vm, globals, arg0)? {
        Some(r) => r,
        None => {
            // Single non-Numeric argument: try #to_c coercion.
            let to_c_id = IdentId::get_id("to_c");
            if let Some(func_id) = globals.check_method(arg0, to_c_id) {
                let result =
                    vm.invoke_func_inner(globals, func_id, arg0, &[], None, None)?;
                if result.try_complex().is_some() {
                    return Ok(result);
                }
            }
            // Single non-Numeric, no usable #to_c: swallowable.
            if exception {
                return Err(MonorubyErr::typeerr(format!(
                    "can't convert {} into Complex",
                    arg0.get_real_class_name(globals)
                )));
            }
            return Ok(Value::nil());
        }
    };
    Ok(Value::complex(r, Real::zero()))
}

/// Convert `v` into a `Real` suitable for a Complex component, returning
/// `Ok(None)` when `v` is not a numeric (so the caller can decide between
/// `#to_c` coercion and a `TypeError`).
fn real_for_complex(
    _vm: &mut Executor,
    globals: &mut Globals,
    v: Value,
) -> Result<Option<Real>> {
    match v.unpack() {
        RV::Fixnum(i) => Ok(Some(Real::from(i))),
        RV::BigInt(b) => Ok(Some(Real::from(b.clone()))),
        RV::Float(f) => Ok(Some(Real::from(f))),
        RV::String(b) => {
            // A numeric string component is accepted; a non-numeric string
            // is reported as `None` so the caller raises the right error.
            match parse_complex_string(globals, b) {
                Ok(parsed) => Ok(Some(Real::try_from(globals, parsed)?)),
                Err(e) => {
                    // Encoding::CompatibilityError must still propagate.
                    if e.kind() == &MonorubyErrKind::Arguments {
                        Ok(None)
                    } else {
                        Err(e)
                    }
                }
            }
        }
        _ => {
            if v.try_rational().is_some() {
                return Ok(Some(Real::try_from(globals, v)?));
            }
            let numeric_id = IdentId::get_id("Numeric");
            if let Some(numeric) = globals
                .store
                .get_constant_noautoload(OBJECT_CLASS, numeric_id)
                .map(|c| c.as_class_id())
            {
                if v.is_kind_of(&globals.store, numeric) {
                    return Ok(Some(Real::try_from(globals, v)?));
                }
            }
            Ok(None)
        }
    }
}

/// Parse a Ruby `String` value as a Complex, the way `Kernel#Complex(str)`
/// does. Raises `ArgumentError`/`TypeError`/`Encoding::CompatibilityError`
/// on invalid input.
fn parse_complex_string(globals: &mut Globals, b: &RStringInner) -> Result<Value> {
    // ASCII-incompatible encodings raise Encoding::CompatibilityError.
    if !b.encoding().is_ascii_compatible() {
        return Err(MonorubyErr::encoding_compatibility_error_with_store(
            &globals.store,
            format!("ASCII incompatible encoding: {}", b.encoding().name()),
        ));
    }
    let s = b.check_utf8()?;
    let original = s;
    if s.bytes().any(|c| c == 0) {
        return Err(MonorubyErr::argumenterr("string contains null byte"));
    }
    match parse_complex_literal(s) {
        Some((re, im)) => {
            let re = Real::try_from(globals, re)?;
            let im = Real::try_from(globals, im)?;
            Ok(Value::complex(re, im))
        }
        None => Err(MonorubyErr::argumenterr(format!(
            "invalid value for convert(): {:?}",
            original
        ))),
    }
}

/// Parse a complex literal string into `(real, imaginary)` `Value`s.
/// Returns `None` for any unrecognised / garbage input.
fn parse_complex_literal(s: &str) -> Option<(Value, Value)> {
    let s = s.trim_matches(|c: char| c == ' ' || c == '\t' || c == '\n' || c == '\r');
    if s.is_empty() {
        return Some((Value::integer(0), Value::integer(0)));
    }
    // Reject sequences of consecutive underscores and leading/trailing `_`.
    let bytes = s.as_bytes();
    for (idx, &c) in bytes.iter().enumerate() {
        if c == b'_' {
            // `_` must be surrounded by digits.
            let prev_digit = idx > 0 && bytes[idx - 1].is_ascii_digit();
            let next_digit =
                idx + 1 < bytes.len() && bytes[idx + 1].is_ascii_digit();
            if !prev_digit || !next_digit {
                return None;
            }
        }
    }
    let s: String = s.chars().filter(|&c| c != '_').collect();
    let s = s.as_str();

    // Polar form: `m@a`.
    if let Some(at) = s.find('@') {
        let m = parse_complex_component(&s[..at])?;
        let a = parse_complex_component(&s[at + 1..])?;
        let m = value_to_f64(m);
        let a = value_to_f64(a);
        let re = m * a.cos();
        let im = m * a.sin();
        return Some((complex_real_value(re), complex_real_value(im)));
    }

    // Normalise imaginary unit characters.
    let has_imag = s.ends_with(['i', 'I', 'j', 'J']);
    if has_imag {
        let body = &s[..s.len() - 1];
        if body.is_empty() {
            return Some((Value::integer(0), Value::integer(1)));
        }
        if body == "+" {
            return Some((Value::integer(0), Value::integer(1)));
        }
        if body == "-" {
            return Some((Value::integer(0), Value::integer(-1)));
        }
        // Split into `real` + `imag` on a top-level sign that is not part
        // of an exponent.
        if let Some(pos) = find_complex_sign_split(body) {
            let real_str = &body[..pos];
            let imag_str = &body[pos..];
            let re = parse_complex_component(real_str)?;
            let im = if imag_str == "+" {
                Value::integer(1)
            } else if imag_str == "-" {
                Value::integer(-1)
            } else {
                parse_complex_component(imag_str)?
            };
            return Some((re, im));
        }
        // Pure imaginary.
        let im = parse_complex_component(body)?;
        return Some((Value::integer(0), im));
    }

    // Pure real.
    let re = parse_complex_component(s)?;
    Some((re, Value::integer(0)))
}

/// Find the index of a `+`/`-` separating the real and imaginary parts,
/// skipping a leading sign and exponent signs.
fn find_complex_sign_split(s: &str) -> Option<usize> {
    let bytes = s.as_bytes();
    let mut i = bytes.len();
    while i > 0 {
        i -= 1;
        if (bytes[i] == b'+' || bytes[i] == b'-') && i > 0 {
            if bytes[i - 1] == b'e' || bytes[i - 1] == b'E' {
                continue;
            }
            return Some(i);
        }
    }
    None
}

/// Parse a single real component: an integer, float, or `a/b` rational.
/// Returns `None` for garbage (including `Infinity`/`NaN`).
fn parse_complex_component(s: &str) -> Option<Value> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }
    // Rational `a/b`.
    if let Some(slash) = s.find('/') {
        let num = s[..slash].trim();
        let den = s[slash + 1..].trim();
        let n: i64 = parse_strict_int(num)?;
        let d: i64 = parse_strict_int(den)?;
        if d == 0 {
            return None;
        }
        return Some(Value::rational_from_inner(RationalInner::new(n, d)));
    }
    // Float (contains `.`, `e`, or `E`).
    if s.contains('.') || s.contains('e') || s.contains('E') {
        let f: f64 = s.parse().ok()?;
        if !f.is_finite() {
            return None;
        }
        return Some(Value::float(f));
    }
    // Integer.
    let n: i64 = parse_strict_int(s)?;
    Some(Value::integer(n))
}

/// Parse a strict optionally-signed decimal integer. Rejects empty and
/// any non-digit characters.
fn parse_strict_int(s: &str) -> Option<i64> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }
    let (neg, digits) = match s.strip_prefix('-') {
        Some(rest) => (true, rest),
        None => (false, s.strip_prefix('+').unwrap_or(s)),
    };
    if digits.is_empty() || !digits.bytes().all(|c| c.is_ascii_digit()) {
        return None;
    }
    let v: i64 = digits.parse().ok()?;
    Some(if neg { -v } else { v })
}

fn value_to_f64(v: Value) -> f64 {
    match v.unpack() {
        RV::Fixnum(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap_or(0.0),
        RV::Float(f) => f,
        _ => v.try_rational().map_or(0.0, |r| r.to_f()),
    }
}

fn complex_real_value(f: f64) -> Value {
    if f == (f as i64) as f64 && f.is_finite() {
        Value::integer(f as i64)
    } else {
        Value::float(f)
    }
}

///
/// ### Kernel.#Rational
///
/// - Rational(a, b = 1, exception: true) -> Rational | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/Rational.html]
#[monoruby_builtin]
fn kernel_rational(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    // `exception:` keyword is stored after the positional max (2).
    let exception = lfp.try_arg(2).map_or(true, |v| v.as_bool());
    match kernel_rational_inner(vm, globals, lfp) {
        Ok(v) => Ok(v),
        Err(e) => {
            if exception {
                Err(e)
            } else {
                Ok(Value::nil())
            }
        }
    }
}

fn kernel_rational_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
) -> Result<Value> {
    let a = lfp.arg(0);
    if let Some(b) = lfp.try_arg(1) {
        // Two-argument form: Rational(a, b)
        let ar = val_to_rational(globals, a)?;
        let br = val_to_rational(globals, b)?;
        Ok(Value::rational_from_inner(ar.div(&br)?))
    } else {
        // One-argument form: Rational(a)
        if let Some(r) = a.try_rational() {
            return Ok(Value::rational_from_inner(r.clone()));
        }
        match a.unpack() {
            RV::Fixnum(i) => Ok(Value::rational_from_inner(RationalInner::new(i, 1))),
            RV::BigInt(b) => Ok(Value::rational_from_inner(RationalInner::new_bigint(
                b.clone(),
                num::BigInt::from(1),
            ))),
            RV::Float(f) => {
                if f.is_nan() {
                    return Err(MonorubyErr::rangeerr("can't convert NaN into Rational"));
                }
                if f.is_infinite() {
                    return Err(MonorubyErr::rangeerr(
                        "can't convert Infinity into Rational",
                    ));
                }
                Ok(Value::rational_from_inner(RationalInner::from_f64(f)))
            }
            _ => {
                // Try to_r coercion
                let to_r_id = IdentId::get_id("to_r");
                if let Some(func_id) = globals.check_method(a, to_r_id) {
                    let result = vm.invoke_func_inner(globals, func_id, a, &[], None, None)?;
                    if result.try_rational().is_some() {
                        return Ok(result);
                    }
                }
                Err(MonorubyErr::typeerr(format!(
                    "can't convert {} into Rational",
                    a.get_real_class_name(&globals.store)
                )))
            }
        }
    }
}

/// Convert a Value to RationalInner for Kernel#Rational two-arg form.
fn val_to_rational(globals: &mut Globals, v: Value) -> Result<RationalInner> {
    if let Some(r) = v.try_rational() {
        return Ok(r.clone());
    }
    match v.unpack() {
        RV::Fixnum(i) => Ok(RationalInner::new(i, 1)),
        RV::BigInt(b) => Ok(RationalInner::new_bigint(b.clone(), num::BigInt::from(1))),
        RV::Float(f) => {
            if f.is_nan() || f.is_infinite() {
                return Err(MonorubyErr::rangeerr("can't convert Float into Rational"));
            }
            Ok(RationalInner::from_f64(f))
        }
        _ => Err(MonorubyErr::typeerr(format!(
            "can't convert {} into Rational",
            v.get_real_class_name(&globals.store)
        ))),
    }
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
        return Err(MonorubyErr::cant_convert_error_ary(globals, arg, result));
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
    // NOTE: `require` is intentionally left on `coerce_to_string`.
    // It is intercepted by CRuby's rubygems `kernel_require.rb`
    // shim, and routing the arg through `#to_path` here perturbs
    // `$LOADED_FEATURES` bookkeeping in that shim. `require_relative`
    // and `load` use the direct builtin path and do get the
    // CRuby-accurate `#to_path`/`#to_str` coercion.
    let feature = lfp.arg(0).coerce_to_string(vm, globals)?;
    let file_name = std::path::PathBuf::from(feature);
    let b = vm.require(globals, &file_name, false)?;
    Ok(Value::bool(b))
}

/// CRuby's `rb_get_path`: convert a path argument to a String via
/// `#to_path` (if present) and then `#to_str` (if the `#to_path`
/// result is not already a String). A bare `#to_str` object is also
/// accepted. Anything else is a TypeError.
fn path_arg_to_string(
    vm: &mut Executor,
    globals: &mut Globals,
    arg: Value,
) -> Result<String> {
    if let Some(s) = arg.is_str() {
        return Ok(s.to_string());
    }
    let v = if let Some(fid) = globals.check_method(arg, IdentId::TO_PATH) {
        vm.invoke_func_inner(globals, fid, arg, &[], None, None)?
    } else {
        arg
    };
    if let Some(s) = v.is_str() {
        return Ok(s.to_string());
    }
    if let Some(fid) = globals.check_method(v, IdentId::TO_STR) {
        let r = vm.invoke_func_inner(globals, fid, v, &[], None, None)?;
        if let Some(s) = r.is_str() {
            return Ok(s.to_string());
        }
    }
    Err(MonorubyErr::no_implicit_conversion(
        &globals.store,
        arg,
        STRING_CLASS,
    ))
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
    let feature = std::path::PathBuf::from(path_arg_to_string(vm, globals, lfp.arg(0))?);
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
    let file_name = std::path::PathBuf::from(path_arg_to_string(vm, globals, lfp.arg(0))?);
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
///
/// A FatalError from compiling the eval'd source (e.g. the prism
/// lowerer hitting a node monoruby does not yet support) would
/// otherwise propagate uncatchably and abort the process. Inside
/// `eval`, downgrade it to a catchable SyntaxError — the closest
/// CRuby analogue for "this source cannot be run here" — so callers
/// can `rescue` it.
fn downgrade_eval_fatal(err: MonorubyErr) -> MonorubyErr {
    if err.is_fatal() {
        MonorubyErr::new(MonorubyErrKind::Syntax, err.message().to_string())
    } else {
        err
    }
}

#[monoruby_builtin]
fn eval(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let src_encoding = crate::builtins::eval_src_encoding(lfp.arg(0));
    let expr = crate::builtins::eval_source_bytes(vm, globals, lfp.arg(0))?;
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
        globals
            .compile_script_binding(expr, fname, binding, lineno, src_encoding)
            .map_err(downgrade_eval_fatal)?;
        vm.flush_compile_warnings(globals);
        vm.invoke_binding(globals, binding.binding().unwrap())
    } else {
        let fid = globals
            .compile_script_eval(expr, fname, caller_cfp, None, lineno, src_encoding)
            .map_err(downgrade_eval_fatal)?;
        vm.flush_compile_warnings(globals);
        let proc = ProcData::new(caller_cfp.lfp(), fid);
        // Isolate the eval's cref so toggles like `module_function`,
        // `private`, … set inside the eval'd source don't leak to
        // the surrounding class/module body. Mirrors CRuby's
        // `rb_vm_cref_dup_without_refinements`.
        let pushed = vm.push_eval_cref();
        let res = vm.invoke_block(globals, &proc, &[]);
        vm.pop_eval_cref(pushed);
        res
    }
}

/// Shell commands that must be run *by* the shell even though the command
/// string contains no metacharacters (they are builtins/reserved words, not
/// executables). Mirrors CRuby's `posix_sh_cmds` in proc.c — e.g.
/// `IO.popen("exit 99")` must yield exit status 99, not ENOENT.
const POSIX_SH_CMDS: &[&str] = &[
    "!", ".", ":", "break", "case", "continue", "do", "done", "elif", "else", "esac", "eval",
    "exec", "exit", "export", "fi", "for", "if", "in", "read", "readonly", "return", "set",
    "shift", "then", "times", "trap", "umask", "unset", "until", "wait", "while",
];

pub(super) fn prepare_command_arg(input: &str) -> (String, Vec<String>) {
    let mut args = vec![];
    let include_meta = input.contains([
        '*', '?', '{', '}', '[', ']', '<', '>', '(', ')', '~', '&', '|', '\\', '$', ';', '\'',
        '\"', '`', '\n',
    ]) || POSIX_SH_CMDS.contains(&input.split_whitespace().next().unwrap_or(""));
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
    let (program, mut args) = prepare_command_arg(&arg0.coerce_to_str(vm, globals)?);
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
pub(super) fn exec(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
        // A NUL byte anywhere in the argv would silently turn this
        // into an abort via the next `CString::new(...).unwrap()`.
        // CRuby raises `ArgumentError: string contains null byte` —
        // mirror that. Same below for the multi-arg execvp form.
        let mut all_args = vec![
            CString::new(program.clone()).map_err(|_| {
                MonorubyErr::argumenterr("string contains null byte")
            })?,
        ];
        for a in &shell_args {
            all_args.push(CString::new(a.as_str()).map_err(|_| {
                MonorubyErr::argumenterr("string contains null byte")
            })?);
        }
        let c_program = CString::new(program).map_err(|_| {
            MonorubyErr::argumenterr("string contains null byte")
        })?;
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
        // Multiple args: first is program, rest are argv. NUL bytes
        // in any of them ⇒ `ArgumentError: string contains null byte`
        // (CRuby behaviour) instead of the prior abort.
        let c_program = CString::new(str_args[0].as_str()).map_err(|_| {
            MonorubyErr::argumenterr("string contains null byte")
        })?;
        let c_args: Vec<CString> = str_args
            .iter()
            .map(|s| {
                CString::new(s.as_str()).map_err(|_| {
                    MonorubyErr::argumenterr("string contains null byte")
                })
            })
            .collect::<Result<Vec<_>>>()?;
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
                Err(err) => {
                    if let MonorubyErrKind::SystemExit(status) = &err.kind {
                        std::process::exit(*status as i32)
                    }
                    // An uncaught SignalException must kill the child *as
                    // that signal* so the parent's `$?.signaled?`/`termsig`
                    // see a signal death (CRuby semantics; see the same
                    // logic in main.rs handle_error). Interrupt reports,
                    // plain SignalException dies silently.
                    if let Some((signo, is_interrupt)) =
                        err.signal_exception_signo(&globals.store)
                    {
                        if is_interrupt {
                            err.show_error_message_and_all_loc(&globals.store);
                        }
                        crate::executor::terminate_with_signal(signo);
                    }
                    err.show_error_message_and_all_loc(&globals.store);
                    std::process::exit(1)
                }
            }
        }
        Ok(Value::nil())
    } else {
        // Parent process
        Ok(Value::integer(pid as i64))
    }
}

///
/// ### Kernel.#spawn
///
/// - spawn(command... [, options]) -> Integer
///
/// Launches an external command in a new child process and returns its PID
/// immediately (unlike `system`, it does not wait). Honours the `:in` /
/// `:out` / `:err` redirect options (each an IO or fd Integer) — the form
/// `Open3` uses to wire up pipes, which is what lets bundler fetch git-source
/// gems via `Open3.capture3("git", ...)`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/spawn.html]
#[monoruby_builtin]
pub(super) fn spawn(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::ffi::CString;
    let null_byte = || MonorubyErr::argumenterr("string contains null byte");

    let mut items: Vec<Value> = lfp.arg(0).as_array().iter().copied().collect();

    // Trailing options Hash: honour :in / :out / :err fd redirections,
    // recorded as (child_fd, source_fd) pairs.
    let mut redirects: Vec<(i32, i32)> = vec![];
    if let Some(last) = items.last() {
        if let Some(h) = last.try_hash_ty() {
            for (name, child_fd) in [("in", 0i32), ("out", 1), ("err", 2)] {
                if let Some(v) = h.get(Value::symbol(IdentId::get_id(name)), vm, globals)? {
                    let src = if let Some(i) = v.try_fixnum() {
                        i as i32
                    } else if v.ty() == Some(ObjTy::IO) {
                        v.as_io_inner().fileno()?
                    } else {
                        return Err(MonorubyErr::argumenterr(format!(
                            "spawn: unsupported redirect value for :{name}"
                        )));
                    };
                    redirects.push((child_fd, src));
                }
            }
            items.pop();
        }
    }

    let str_args: Vec<String> = items
        .iter()
        .map(|v| v.coerce_to_string(vm, globals))
        .collect::<Result<Vec<_>>>()?;
    if str_args.is_empty() {
        return Err(MonorubyErr::argumenterr(
            "wrong number of arguments (given 0, expected 1+)",
        ));
    }

    // Build argv as CStrings up front so the child needs no allocation.
    let argv: Vec<CString> = if str_args.len() == 1 {
        // Single string ⇒ shell-style split (mirrors `system` / `exec`).
        let (program, sh_args) = prepare_command_arg(&str_args[0]);
        let mut v = vec![CString::new(program).map_err(|_| null_byte())?];
        for a in sh_args {
            v.push(CString::new(a).map_err(|_| null_byte())?);
        }
        v
    } else {
        str_args
            .iter()
            .map(|s| CString::new(s.as_str()).map_err(|_| null_byte()))
            .collect::<Result<Vec<_>>>()?
    };
    let mut argv_ptrs: Vec<*const libc::c_char> = argv.iter().map(|a| a.as_ptr()).collect();
    argv_ptrs.push(std::ptr::null());
    let max_fd = match unsafe { libc::sysconf(libc::_SC_OPEN_MAX) } {
        n if n > 0 => n as i32,
        _ => 1024,
    };

    // SAFETY: monoruby has no real OS threads (Thread is cooperative), so
    // fork() leaves the child single-threaded and consistent. The child only
    // performs async-signal-safe libc calls (dup2/close/execvp) before
    // replacing itself; every allocation above happened in the parent.
    let pid = unsafe { libc::fork() };
    if pid < 0 {
        return Err(MonorubyErr::runtimeerr(format!(
            "spawn failed: {}",
            std::io::Error::last_os_error()
        )));
    }
    if pid == 0 {
        // ===== child =====
        unsafe {
            // Restore default signal dispositions before exec. monoruby
            // ignores SIGPIPE (and installs other handlers); a freshly
            // exec'd program expects the defaults, e.g. so a command in a
            // pipeline dies on SIGPIPE instead of seeing EPIPE and printing
            // a "Broken pipe" error. Mirrors CRuby's spawn.
            for sig in 1..32 {
                if sig != libc::SIGKILL && sig != libc::SIGSTOP {
                    libc::signal(sig, libc::SIG_DFL);
                }
            }
            for &(child_fd, src_fd) in &redirects {
                if src_fd != child_fd {
                    libc::dup2(src_fd, child_fd);
                }
            }
            // IO.pipe fds are not close-on-exec, so close every other
            // descriptor (>= 3); otherwise a leaked write end keeps the pipe
            // open and the reader never sees EOF when the child exits.
            for fd in 3..max_fd {
                libc::close(fd);
            }
            libc::execvp(argv[0].as_ptr(), argv_ptrs.as_ptr());
            libc::_exit(127);
        }
    }
    // ===== parent =====
    Ok(Value::integer(pid as i64))
}

///
/// Kernel.#`
///
/// - `command` -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/=60.html]
#[monoruby_builtin]
fn command(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::os::unix::process::ExitStatusExt;
    let arg0 = lfp.arg(0);
    let (program, args) = prepare_command_arg(&arg0.coerce_to_string(vm, globals)?);
    let child = std::process::Command::new(program)
        .args(&args)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .map_err(|err| MonorubyErr::runtimeerr(format!("{}", err)))?;
    let pid = child.id();
    match child.wait_with_output() {
        Ok(output) => {
            std::io::stderr().write_all(&output.stderr).unwrap();
            // Set `$?` / Process.last_status from the raw wait(2) status so
            // a signal-terminated child reports `signaled?` / `termsig`
            // (CRuby sets $? after backticks; we previously left it nil).
            let raw = output.status.into_raw();
            if let Ok(status_class) =
                vm.get_qualified_constant(globals, OBJECT_CLASS, &["Process", "Status"])
            {
                if let Ok(status_obj) = vm.invoke_method_inner(
                    globals,
                    IdentId::NEW,
                    status_class,
                    &[Value::integer(raw as i64), Value::integer(pid as i64)],
                    None,
                    None,
                ) {
                    globals.set_gvar(IdentId::get_id("$?"), status_obj);
                }
            }
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
        // Sleep interruptibly, like CRuby: a delivered signal (whose async
        // handler set a pending bit) breaks nanosleep with EINTR; drain it
        // at this safepoint so a SIGTERM etc. raises its SignalException
        // (or runs its trap handler) *immediately*, not after the full
        // duration. `std::thread::sleep` must not be used here — it
        // transparently retries on EINTR, so a signaled sleeper would
        // otherwise doze the whole interval and only convert the signal at
        // some later poll point (racy: the surrounding block may end
        // first, and e.g. a fork child would then exit 0 instead of dying
        // signal-terminated).
        let deadline = now + std::time::Duration::from_secs_f64(sec);
        loop {
            // Drain signals that arrived *before* (re-)entering nanosleep —
            // e.g. delivered between fork return and this call. Such a
            // signal's bit is already set, so it will never EINTR the
            // upcoming nanosleep; without this check the sleeper dozes the
            // full interval and the surrounding block can end without ever
            // reaching a poll (a fork child would then exit 0 instead of
            // dying signal-terminated).
            if crate::codegen::signal_table::PENDING_SIGNALS
                .load(std::sync::atomic::Ordering::Relaxed)
                != 0
                && crate::executor::execute_gc(vm, globals).is_none()
            {
                return Err(vm.take_error());
            }
            let remaining = match deadline.checked_duration_since(std::time::Instant::now()) {
                Some(d) if !d.is_zero() => d,
                _ => break,
            };
            let ts = libc::timespec {
                tv_sec: remaining.as_secs() as libc::time_t,
                tv_nsec: remaining.subsec_nanos() as libc::c_long,
            };
            // SAFETY: plain POSIX nanosleep on a valid timespec.
            let r = unsafe { libc::nanosleep(&ts, std::ptr::null_mut()) };
            if r == 0 {
                break;
            }
            if std::io::Error::last_os_error().raw_os_error() != Some(libc::EINTR) {
                break;
            }
            // A signal interrupted the sleep: run the poll point. An error
            // (converted SignalException) aborts the sleep; a trap handler
            // runs and the sleep resumes for the remainder (CRuby+`trap`
            // semantics on Linux SA_RESTART-less nanosleep).
            if crate::executor::execute_gc(vm, globals).is_none() {
                return Err(vm.take_error());
            }
        }
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
pub(super) fn abort(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let msg = if let Some(arg0) = lfp.try_arg(0) {
        let s = arg0.coerce_to_str(vm, globals)?;
        // Write to the Ruby `$stderr` (which may be reassigned /
        // mocked), not the OS stderr, matching CRuby.
        let stderr = globals
            .get_gvar(IdentId::get_id("$stderr"))
            .unwrap_or(Value::nil());
        if !stderr.is_nil() {
            let line = Value::string(format!("{}\n", s));
            vm.invoke_method_inner(
                globals,
                IdentId::get_id("write"),
                stderr,
                &[line],
                None,
                None,
            )?;
        }
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
pub(super) fn exit(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let status = if let Some(arg0) = lfp.try_arg(0) {
        match arg0.unpack() {
            RV::Bool(true) => 0,
            RV::Bool(false) => 1,
            _ => arg0.coerce_to_int_i64(vm, globals)?,
        }
    } else {
        0
    };
    Err(MonorubyErr::new(
        MonorubyErrKind::SystemExit(status as u8),
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
fn warn(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let message = lfp.arg(0);
    // uplevel and category keyword arguments are accepted but ignored for now.
    let stderr_id = IdentId::get_id("$stderr");
    let stderr = globals.get_gvar(stderr_id).unwrap_or(Value::nil());
    let write_id = IdentId::get_id("write");

    let mut messages = vec![];
    if let Some(ary) = message.try_array_ty() {
        for m in ary.iter() {
            if let Some(s) = m.is_str() {
                messages.push(format!("{}\n", s));
            } else {
                messages.push(format!("{}\n", m.to_s(&globals.store)));
            }
        }
    } else if let Some(s) = message.is_str() {
        messages.push(format!("{}\n", s));
    } else {
        messages.push(format!("{}\n", message.to_s(&globals.store)));
    }

    for msg in messages {
        let msg_val = Value::string(msg);
        vm.invoke_method_inner(globals, write_id, stderr, &[msg_val], None, None)?;
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
    let path = globals.current_source_path(vm);
    let s = path.to_string_lossy();
    // `eval` with a binding (and no explicit filename) yields a
    // synthetic "(eval at ...)" source; there is no directory.
    if s.starts_with("(eval") || s.starts_with("(irb") {
        return Ok(Value::nil());
    }
    // File.dirname semantics: a bare filename has directory ".".
    let dir = match path.parent() {
        Some(p) if !p.as_os_str().is_empty() => p.to_string_lossy().to_string(),
        _ => ".".to_string(),
    };
    Ok(Value::string(dir))
}

///
/// Kernel.#__method__
///
/// - __method__ -> Symbol | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/__method__.html]
#[monoruby_builtin]
fn method_(vm: &mut Executor, globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // Skip native (builtin) caller frames such as `send`/`__send__`/
    // `public_send` so that `send("__method__")` reports the Ruby method that
    // invoked `send`, not `send` itself.
    let mut cfp = vm.cfp().prev();
    while let Some(f) = cfp {
        if !f.lfp().meta().is_native() {
            break;
        }
        cfp = f.prev();
    }
    let Some(caller) = cfp else {
        return Ok(Value::nil());
    };
    let outer = caller.outermost_lfp();
    let fid = outer.func_id();
    // `__method__` yields nil outside of a method body. A `define_method`
    // body is a Proc-typed function (not `FuncType::Method`) but runs in a
    // frame tagged `proc_method` and carries the installed method name, so
    // accept those too.
    if !globals.store[fid].is_method() && !outer.meta().is_proc_method() {
        return Ok(Value::nil());
    }
    globals.store[fid]
        .name()
        // The main script's body carries the internal `/main` label (and,
        // running inside TOPLEVEL_BINDING, a proc_method-tagged frame) —
        // but the top level is not a method: report nil, as CRuby does.
        .filter(|sym| *sym != IdentId::_MAIN)
        .map_or(Ok(Value::nil()), |sym| Ok(Value::symbol(sym)))
}

///
/// Kernel.#catch
///
/// - catch(tag = Object.new) { |tag| ... } -> object
///
#[monoruby_builtin]
fn catch_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let tag = match lfp.try_arg(0) {
        Some(v) => v,
        None => Value::object(OBJECT_CLASS),
    };
    let bh = lfp.expect_block()?;
    let saved_errinfo = vm.errinfo();
    vm.push_catch_tag(tag);
    let res = vm.invoke_block_once(globals, bh, &[tag]);
    vm.pop_catch_tag();
    match res {
        Ok(val) => Ok(val),
        Err(err) => {
            if let MonorubyErrKind::Throw(throw_tag, throw_val) = err.kind() {
                if tag.id() == throw_tag.id() {
                    // A caught throw restores `$!` to its value at
                    // catch entry: throwing from inside a rescue must
                    // not leak the in-flight exception.
                    vm.set_errinfo(saved_errinfo);
                    return Ok(*throw_val);
                }
            }
            Err(err)
        }
    }
}

///
/// Kernel.#throw
///
/// - throw(tag, value = nil) -> void
///
#[monoruby_builtin]
fn throw_(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let tag = lfp.arg(0);
    let value = lfp.try_arg(1).unwrap_or(Value::nil());
    // No active `catch` for this tag: raise a rescuable
    // `UncaughtThrowError` (< ArgumentError) rather than letting the
    // control-flow `Throw` escape uncatchably.
    if !vm.is_catch_tag_active(tag) {
        let msg = format!("uncaught throw {}", tag.inspect(&globals.store));
        if let Some(klass) = globals
            .store
            .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("UncaughtThrowError"))
        {
            let mut err = MonorubyErr::new(MonorubyErrKind::Other(klass.as_class_id()), msg);
            // Surfaced as `UncaughtThrowError#tag`.
            err.payload = Some((tag.id(), "tag"));
            return Err(err);
        }
        return Err(MonorubyErr::argumenterr(msg));
    }
    Err(MonorubyErr::throw(tag, value))
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
        // Drain dlerror() so subsequent dlopen() calls see a fresh
        // error state. The Ruby wrappers
        // (`Fiddle::Handle#initialize`, `FFI::DynamicLibrary#initialize`)
        // check for a nil return and raise their own
        // `Fiddle::DLError` / `LoadError` with a message they
        // construct themselves, so returning nil is what they expect.
        // Raising directly from here breaks the ffi gem's
        // per-candidate fallback loop in
        // `FFI::DynamicLibrary.try_load`: each rescued LoadError
        // would leave `Executor::exception` populated and trip the
        // `assert_eq!(self.exception, None)` double-set guard in
        // `Executor::set_error` on the next dlopen attempt.
        unsafe { libc::dlerror() };
        return Ok(Value::nil());
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

///
/// Object#send
///
/// - send(name, *args) -> object
/// - send(name, *args) { .... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/send.html]
#[monoruby_builtin]
fn public_send(
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
    let receiver = lfp.self_val();
    // public_send only allows public methods.
    // Both private and protected are rejected unconditionally.
    let class_id = receiver.class();
    if let Some(entry) = globals.check_method_for_class(class_id, method) {
        match entry.visibility() {
            Visibility::Private => {
                return Err(MonorubyErr::private_method_called(
                    globals, method, receiver,
                ));
            }
            Visibility::Protected => {
                return Err(MonorubyErr::protected_method_called(
                    globals, method, receiver,
                ));
            }
            _ => {}
        }
    }
    vm.invoke_method_inner(
        globals,
        method,
        receiver,
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

#[cfg(target_arch = "x86_64")]

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
    let using_fpr = state.get_using_fpr(ir);
    let error = ir.new_error(state);
    let callid = callsite.id;
    ir.inline(move |r#gen, store, labels, _| {
        let error = &labels[error];
        r#gen.object_send_inline(callid, store, using_fpr, &error, no_splat);
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

fn kernel_is_a(
    state: &mut AbstractState,
    _ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    recv_class: ClassId,
    _: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo { args, dst, .. } = *callsite;
    // Only fold when the argument is a concrete class/module value.
    // Anything else (including a literal that isn't a class/module — which
    // would raise TypeError) falls back to the regular dispatch.
    let Some(target) = state.is_class_or_module_literal(args) else {
        return false;
    };
    let target_id = target.id();
    // Synthetic classes like BOOL_CLASS have no backing Module object —
    // their superclass chain isn't meaningful for is_a? folding. Fall
    // back to the regular dispatch in that case.
    let Some(recv_module) = store[recv_class].try_get_module() else {
        return false;
    };
    // The receiver-class guard + class_version guard upstream ensure
    // recv_class is exact at this point, so the inheritance chain is
    // fixed: walk recv_module's superclasses to compute the result at
    // compile time.
    let mut cur = Some(recv_module);
    let mut result = false;
    while let Some(m) = cur {
        if m.id() == target_id {
            result = true;
            break;
        }
        cur = m.superclass();
    }
    if let Some(dst) = dst {
        state.def_C(dst, Immediate::bool(result));
    }
    true
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
    // When a block is supplied, it becomes the size proc:
    //   `to_enum(:step, limit, step) { step_size }`
    // CRuby evaluates this lazily when `.size` is called on the
    // returned Enumerator.
    let size = if let Some(bh) = lfp.block() {
        Some(vm.generate_proc(globals, bh, pc)?.as_val())
    } else {
        None
    };
    let args = lfp.arg(0).as_array();
    let (method, args) = if args.is_empty() {
        (IdentId::EACH, vec![])
    } else {
        (
            args[0].expect_symbol_or_string(globals)?,
            args[1..].to_vec(),
        )
    };
    vm.generate_enumerator_with_size(method, lfp.self_val(), args, pc, size)
}

///
/// ### Kernel#dup
///
/// - dup -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/clone.html]
#[monoruby_builtin]
fn dup(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    if let Some(module) = self_val.is_class_or_module() {
        if module.id() == BASIC_OBJECT_CLASS {
            return Err(MonorubyErr::typeerr("can't copy the root class"));
        }
        // Module/Class duplicates must own their own method/constant tables,
        // otherwise `undef_method`, `define_method`, etc. on the copy would
        // mutate the original. `delegate.rb` relies on this by duping Kernel,
        // stripping a few methods, then `include`-ing the dup.
        let dup_module = globals.store.duplicate_module(module.id());
        return Ok(dup_module.as_val());
    }
    let mut copy = self_val.dup();
    // `dup` yields an instance of the receiver's *real* class: the
    // singleton class and its contents (methods, constants) are not
    // carried over — only `clone` copies them (CRuby semantics).
    let real = self_val.real_class(&globals.store).id();
    if copy.class() != real {
        copy.change_class(real);
    }
    copy_finalizers(globals, self_val.id(), copy.id());
    // When the class uses the default (no-op) copy hooks, skip the hook
    // dispatch entirely and return the raw shallow copy — `dup` always
    // produces a same-class copy, so the default `initialize_copy`
    // validation would always pass. This is the hot path for
    // `String#dup`, `Object#dup`, … (CRuby skips it the same way).
    let version = Globals::class_version();
    if globals
        .store
        .uses_default_copy_hooks(copy.class(), version)
    {
        return Ok(copy);
    }
    // Run the `initialize_dup` hook (default validates; a subclass may
    // override it). Root `copy` across the dispatch (it allocates).
    let temp = vm.temp_len();
    vm.temp_push(copy);
    vm.invoke_method_inner(
        globals,
        IdentId::INITIALIZE_DUP,
        copy,
        &[self_val],
        None,
        None,
    )?;
    vm.temp_clear(temp);
    Ok(copy)
}

/// Re-register every finalizer attached to `from` (an object id) onto
/// `to`, so that `dup`/`clone` copies of an object inherit its
/// finalizers, matching CRuby.
fn copy_finalizers(globals: &mut Globals, from: u64, to: u64) {
    let copied: Vec<(u64, Value)> = globals
        .finalizers
        .iter()
        .filter(|(k, _)| *k == from)
        .map(|(_, callable)| (to, *callable))
        .collect();
    globals.finalizers.extend(copied);
}

///
/// ### Kernel#clone
///
/// - clone -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/clone.html]
#[monoruby_builtin]
fn clone_val(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    // `freeze:` keyword (slot 0): `None` = not passed, `Some(v)` = passed
    // (including `Some(nil)`). Only nil / true / false are accepted.
    let freeze_arg = lfp.try_arg(0);
    if let Some(v) = freeze_arg {
        if !(v.is_nil() || v == Value::bool(true) || v == Value::bool(false)) {
            return Err(MonorubyErr::argumenterr(format!(
                "unexpected value for freeze: {}",
                v.get_real_class_name(&globals.store)
            )));
        }
    }
    // `freeze: true`  -> copy is frozen; `freeze: false` -> copy is not frozen;
    // `freeze: nil` / omitted -> copy inherits the original's frozen state
    // (which `clone_value` already carried over).
    let target_frozen = match freeze_arg {
        Some(v) if v == Value::bool(true) => true,
        Some(v) if v == Value::bool(false) => false,
        _ => self_val.is_frozen(),
    };

    let mut copy = self_val.clone_value();
    copy_finalizers(globals, self_val.id(), copy.id());
    if self_val.is_class_or_module().is_some() {
        return Ok(copy);
    }
    // Keep the copy mutable while the copy hook runs (CRuby freezes only
    // afterwards), so a hook may still initialise a to-be-frozen copy.
    copy.clear_frozen();

    // Skip the no-op copy-hook dispatch when the class uses the default hooks
    // (see `dup` / `uses_default_copy_hooks`).
    let version = Globals::class_version();
    if !globals.store.uses_default_copy_hooks(copy.class(), version) {
        // Run the `initialize_clone` hook. When `freeze:` was passed explicitly,
        // forward it as a keyword argument (matching CRuby), so a hook that does
        // not accept keywords raises ArgumentError. Root `copy` across the
        // dispatch (it allocates).
        let temp = vm.temp_len();
        vm.temp_push(copy);
        let kw = if let Some(v) = freeze_arg {
            let mut map = RubyMap::default();
            map.insert(Value::symbol(IdentId::get_id("freeze")), v, vm, globals)?;
            Some(Hashmap::new(Value::hash(map)))
        } else {
            None
        };
        vm.invoke_method_inner(globals, IdentId::INITIALIZE_CLONE, copy, &[self_val], None, kw)?;
        vm.temp_clear(temp);
    }
    // Packed immediates (Integer/Symbol/true/…) are always frozen and have no
    // mutable header, so only real heap values need the frozen flag applied.
    if target_frozen && !copy.is_packed_value() {
        copy.set_frozen();
    }
    Ok(copy)
}

///
/// ### Kernel#at_exit
///
/// - at_exit { ... } -> Proc
///
/// [https://docs.ruby-lang.org/ja/latest/method/Kernel/m/at_exit.html]
#[monoruby_builtin]
fn at_exit(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let Some(bh) = lfp.block() else {
        return Err(MonorubyErr::argumenterr("called without a block"));
    };
    let proc = vm.generate_proc(globals, bh, pc)?;
    let proc = proc.as_val();
    globals.at_exit_handlers.push(proc);
    Ok(proc)
}

///
/// `ObjectSpace.define_finalizer` registry primitive (private).
///
/// Validation of the callable (`respond_to?(:call)`) happens in the Ruby
/// wrapper; here we reject non-reference objects, warn about a
/// self-referencing finalizer, deduplicate, and record the `(object id,
/// callable)` pair (run at program termination). Returns the effective
/// callable — the originally-registered one when a duplicate is given.
#[monoruby_builtin]
fn register_finalizer(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let obj = lfp.arg(0);
    let callable = lfp.arg(1);
    // Immediates (Integer, Symbol, Float-flonum, nil, true, false) have
    // no stable per-object identity, so a finalizer cannot be attached.
    if obj.is_packed_value() {
        return Err(MonorubyErr::argumenterr(format!(
            "cannot define finalizer for {}",
            obj.get_real_class_name(&globals.store)
        )));
    }
    // CRuby warns when the finalizer closes over (or is bound to) the very
    // object being finalized, since that keeps it alive until exit.
    let references_self = if let Some(p) = callable.is_proc() {
        p.self_val().id() == obj.id()
    } else if let Some(m) = callable.is_method() {
        m.receiver().id() == obj.id()
    } else {
        false
    };
    if references_self {
        vm.ruby_warn(globals, "warning: finalizer references object to be finalized")?;
    }
    // A finalizer `==` to one already registered for this object is
    // recorded only once; the originally-registered callable is returned.
    let id = obj.id();
    let existing: Vec<Value> = globals
        .finalizers
        .iter()
        .filter(|(k, _)| *k == id)
        .map(|(_, c)| *c)
        .collect();
    for c in existing {
        if vm.invoke_eq(globals, c, callable)? {
            return Ok(c);
        }
    }
    globals.finalizers.push((id, callable));
    Ok(callable)
}

///
/// `ObjectSpace.undefine_finalizer` registry primitive (private).
///
#[monoruby_builtin]
fn unregister_finalizer(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let obj = lfp.arg(0);
    // CRuby refuses to mutate the finalizer set of a frozen object.
    if obj.is_frozen() {
        return Err(MonorubyErr::cant_modify_frozen(&globals.store, obj));
    }
    let id = obj.id();
    globals.finalizers.retain(|(k, _)| *k != id);
    Ok(obj)
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
    if self_val.is_frozen() {
        return Err(MonorubyErr::cant_modify_frozen(&globals.store, self_val));
    }
    let name = lfp.arg(0).expect_symbol_or_string(globals)?;
    // For a proc/block body, the runtime LFP carries the *block*'s
    // FuncId. `super` resolution (`find_super`) reads that FuncId's
    // `name`, so it must be set or it panics. Mirror the decoration
    // that `Module#define_method` performs.
    let block_fid_to_decorate: Option<FuncId>;
    let func_id = if let Some(method) = lfp.try_arg(1) {
        if let Some(proc) = method.is_proc() {
            let fid = globals.define_proc_method(proc);
            globals.store[fid].set_method_style();
            globals.store[fid].set_name(name);
            block_fid_to_decorate = Some(proc.func_id());
            fid
        } else if let Some(m) = method.is_method() {
            super::module::validate_bind_target(globals, m.owner(), class_id)?;
            block_fid_to_decorate = None;
            m.func_id()
        } else if let Some(m) = method.is_umethod() {
            super::module::validate_bind_target(globals, m.owner(), class_id)?;
            block_fid_to_decorate = None;
            m.func_id()
        } else {
            return Err(MonorubyErr::wrong_argument_type(
                globals,
                method,
                "Proc/Method/UnboundMethod",
            ));
        }
    } else if let Some(bh) = lfp.block() {
        let proc = vm.generate_proc(globals, bh, pc)?;
        let fid = globals.define_proc_method(proc);
        globals.store[fid].set_method_style();
        globals.store[fid].set_name(name);
        block_fid_to_decorate = Some(proc.func_id());
        fid
    } else {
        return Err(MonorubyErr::wrong_number_of_arg(2, 1));
    };
    vm.add_public_method(globals, class_id, name, func_id)?;
    if let Some(block_fid) = block_fid_to_decorate {
        if globals.store[block_fid].name().is_none() {
            globals.store[block_fid].set_name(name);
        }
        if !globals.store[block_fid].owner_class().contains(&class_id) {
            globals.store[block_fid].set_owner_class(class_id);
        }
        globals.store[block_fid].set_proc_method();
        // The body's cref is the scope the block was *written* in (its
        // lexical class), not the receiver's singleton: a nested `def`
        // inside it lands there. See `ISeqInfo::nested_definee`.
        if let Some(iseq) = globals.store[block_fid].is_iseq() {
            // Block iseqs carry no lexical_context of their own; the
            // write-site scope is the mother iseq's (main, a class
            // body, or an enclosing method).
            let mother = globals.store[iseq].mother().0;
            let cref_class = globals.store[mother]
                .lexical_context
                .last()
                .copied()
                .unwrap_or(OBJECT_CLASS);
            globals.store[iseq].nested_definee = Some(cref_class);
        }
    }
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
        IdentId::INITIALIZE_COPY,
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
    let recv = lfp.self_val();
    // This is the default `Object#to_s`. It is normally shadowed by a
    // more specific `to_s` (e.g. `TrueClass#to_s`, `Integer#to_s`); it
    // is reached on a bare object or via `super` from a redefined
    // `to_s`. CRuby's `Object#to_s` returns `"#<ClassName:0x...>"` for
    // *every* receiver. For heap objects that is what `Value::to_s`
    // already produces, but for immediates (true/false/nil/Integer/
    // Symbol) it returns the value literal ("true", "3", …) — that
    // literal is the general-purpose stringification used elsewhere, not
    // the default object representation, so build the `#<...>` form here.
    let s = if recv.try_rvalue().is_some() {
        recv.to_s(&globals.store)
    } else {
        format!(
            "#<{}:0x{:016x}>",
            recv.get_real_class_name(&globals.store),
            recv.id()
        )
    };
    Ok(Value::string(s))
}

///
/// ### Kernel#respond_to?
///
/// - respond_to?(name, include_all = false) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/respond_to=3f.html]
#[monoruby_builtin]
fn respond_to(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
    let respond_to_missing = IdentId::RESPOND_TO_MISSING_;
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
    //let dst = if let Some(dst) = dst {
    //    dst
    //} else {
    //    return false;
    //};
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
            if let Some(dst) = dst {
                state.def_C(dst, Immediate::bool(true));
            }
            return true;
        }
        // Found but private and !include_all: CRuby still consults
        // respond_to_missing?, so fall through.
    }
    // Method not visible. CRuby calls `recv.respond_to_missing?(name, include_all)`
    // and coerces the result to bool. If `respond_to_missing?` resolves to
    // an ISeq whose hint says it returns a constant (typical for the
    // default `Object#respond_to_missing?` which is `def ...; false; end`
    // and any user override that just returns a literal), fold the call
    // to that value. The class_version guard upstream catches any later
    // override that would change the resolution.
    let resolved = store
        .check_method_for_class_with_version(
            recv_class,
            IdentId::RESPOND_TO_MISSING_,
            ctx.class_version(),
        )
        .and_then(|e| e.func_id());
    if let Some(fid) = resolved
        && let Some(iseq) = store[fid].is_iseq()
        && let ISeqHint::ConstReturn(v) = store[iseq].hint
    {
        if let Some(dst) = dst {
            state.def_C(dst, Immediate::bool(v.as_bool()));
        }
        return true;
    }
    false
}

///
/// ### Kernel#inspect
///
/// - inspect -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/inspect.html]
#[monoruby_builtin]
fn inspect(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    // Only plain objects participate in the ivar / hook formatting;
    // every other kind (immediates, internally `/name`-tagged objects,
    // and types with their own `inspect`) keeps the default rendering.
    if self_val.ty() != Some(ObjTy::OBJECT)
        || globals
            .store
            .get_ivar(self_val, IdentId::_NAME)
            .is_some()
    {
        let s = self_val.inspect(&globals.store);
        return Ok(Value::string(s));
    }

    // CRuby consults the (private) `instance_variables_to_inspect`
    // hook: an Array selects/orders the ivars to show, `nil` shows
    // all, anything else is a TypeError.
    let hook = IdentId::get_id("instance_variables_to_inspect");
    let selection = vm.invoke_method_if_exists(globals, hook, self_val, &[], None, None)?;

    let all_ivars = globals.store.get_ivars(self_val);
    let chosen: Vec<(IdentId, Value)> = match selection {
        None => all_ivars,
        Some(v) if v.is_nil() => all_ivars,
        Some(v) => match v.try_array_ty() {
            Some(arr) => arr
                .iter()
                .filter_map(|e| e.try_symbol())
                .filter_map(|nm| {
                    all_ivars
                        .iter()
                        .find(|(n, _)| *n == nm)
                        .map(|(n, val)| (*n, *val))
                })
                .collect(),
            None => {
                let cls = globals
                    .store
                    .get_class_name(v.real_class(&globals.store).id());
                return Err(MonorubyErr::typeerr(format!(
                    "Expected #instance_variables_to_inspect to return an Array or nil, but it returned {cls}"
                )));
            }
        },
    };

    let class_name = globals
        .store
        .get_class_name(self_val.real_class(&globals.store).id());
    let mut s = format!("#<{}:0x{:016x}", class_name, self_val.id());
    for (i, (name, val)) in chosen.iter().enumerate() {
        s += if i == 0 { " " } else { ", " };
        s += &format!("{name}={}", val.inspect(&globals.store));
    }
    s += ">";
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
fn method(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let receiver = lfp.self_val();
    let method_name = lfp.arg(0).expect_symbol_or_string(globals)?;
    match globals.find_method_for_object(receiver, method_name) {
        Ok((func_id, _, owner)) => {
            let original_name = globals
                .store
                .original_name_by_class_id(receiver.class(), method_name);
            Ok(Value::new_method_named(
                receiver,
                func_id,
                owner,
                method_name,
                original_name,
            ))
        }
        Err(err) => {
            // CRuby: if `receiver.respond_to_missing?(name, true)` is truthy,
            // return a Method that proxies to `method_missing`.
            if let Some(rtm_fid) =
                globals.check_method(receiver, IdentId::RESPOND_TO_MISSING_)
            {
                let responds = vm.invoke_func_inner(
                    globals,
                    rtm_fid,
                    receiver,
                    &[Value::symbol(method_name), Value::bool(true)],
                    None,
                    None,
                )?;
                if responds.as_bool()
                    && let Some(mm_fid) =
                        globals.check_method(receiver, IdentId::METHOD_MISSING)
                {
                    return Ok(Value::new_method_missing_proxy(
                        receiver,
                        mm_fid,
                        method_name,
                        receiver.class(),
                    ));
                }
            }
            Err(err)
        }
    }
}

///
/// ### Kernel#singleton_method
///
/// - singleton_method(name) -> Method
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/singleton_method.html]
#[monoruby_builtin]
fn singleton_method(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let receiver = lfp.self_val();
    let method_name = lfp.arg(0).expect_symbol_or_string(globals)?;
    let class_id = match globals.store.has_singleton(receiver) {
        Some(module) => module.id(),
        None => {
            return Err(MonorubyErr::nameerr_with_name(
                format!(
                    "undefined singleton method `{}' for `{}'",
                    method_name,
                    receiver.inspect(&globals.store)
                ),
                method_name,
            ));
        }
    };
    let (func_id, _, owner) = globals.store.find_method_for_class(class_id, method_name)?;
    let original_name = globals
        .store
        .original_name_by_class_id(class_id, method_name);
    Ok(Value::new_method_named(
        receiver,
        func_id,
        owner,
        method_name,
        original_name,
    ))
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
    let s = lfp.self_val().get_singleton(&mut globals.store)?;
    // MRI's `rb_singleton_class`: for a Class receiver, the exposed
    // eigenclass is made to belong to its *own* eigenclass — building
    // S(S(C)) re-links S(C)'s class pointer, so class methods defined
    // on the meta-metaclass (of this class or an ancestor) dispatch on
    // it (`D.singleton_class.ham`).
    if lfp.self_val().is_class_or_module().is_some() {
        let sid = s.as_class_id();
        globals.store.get_metaclass(sid);
    }
    Ok(s)
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
        globals.store.get_method_names_inherit(class_id, false)
    }))
}

///
/// ### Kernel#private_methods
///
/// - private_methods(include_inherited = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/private_methods.html]
#[monoruby_builtin]
fn private_methods(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let inherited_too = lfp.try_arg(0).is_none() || lfp.arg(0).as_bool();
    let class_id = lfp.self_val().class();
    Ok(Value::array_from_vec(if !inherited_too {
        globals.store.get_private_method_names_direct(class_id)
    } else {
        globals
            .store
            .get_private_method_names_inherit_incl_object(class_id)
    }))
}

///
/// ### Kernel#protected_methods
///
/// - protected_methods(include_inherited = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/protected_methods.html]
#[monoruby_builtin]
fn protected_methods(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let inherited_too = lfp.try_arg(0).is_none() || lfp.arg(0).as_bool();
    let class_id = lfp.self_val().class();
    Ok(Value::array_from_vec(if !inherited_too {
        globals.store.get_protected_method_names_direct(class_id)
    } else {
        globals
            .store
            .get_protected_method_names_inherit_incl_object(class_id)
    }))
}

///
/// ### Kernel#public_methods
///
/// - public_methods(include_inherited = true) -> [Symbol]
///
/// monoruby's `methods` already returns the public/protected union; we
/// implement `public_methods` as the same set, matching CRuby for the
/// non-protected cases.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/public_methods.html]
#[monoruby_builtin]
fn public_methods(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let inherited_too = lfp.try_arg(0).is_none() || lfp.arg(0).as_bool();
    let class_id = lfp.self_val().class();
    Ok(Value::array_from_vec(if !inherited_too {
        globals.store.get_public_method_names_direct(class_id)
    } else {
        globals.store.get_method_names_inherit(class_id, false)
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

/// True for a syntactically valid instance-variable name: `@`
/// followed by an identifier whose first char is not a digit
/// (and not another `@`, which would be a class variable).
fn is_valid_ivar_name(s: &str) -> bool {
    let mut it = s.chars();
    if it.next() != Some('@') {
        return false;
    }
    match it.next() {
        Some(c) if c == '_' || c.is_alphabetic() || !c.is_ascii() => {}
        _ => return false,
    }
    it.all(|c| c == '_' || c.is_alphanumeric() || !c.is_ascii())
}

/// Resolve an `instance_variable_*` name argument with CRuby
/// semantics: Symbol/String are used directly, anything else is
/// coerced via `#to_str` (TypeError otherwise), and the resulting
/// name must be a valid instance-variable name (NameError otherwise).
/// Build a `NameError` for the `instance_variable_*` / `class_variable_*`
/// reflection methods, carrying `name_value` verbatim as `#name` (CRuby
/// preserves the exact argument object, which the spec checks with
/// `equal?`) and `receiver` as `#receiver`. The exception object is
/// pre-materialized and attached as the error's `original` so those
/// ivars survive being caught.
pub(crate) fn name_error_reflection(
    globals: &mut Globals,
    msg: String,
    name_value: Value,
    receiver: Value,
) -> MonorubyErr {
    let exc = Value::new_exception_from(msg.clone(), NAME_ERROR_CLASS);
    let _ = globals.store.set_ivar(exc, IdentId::_NAME, name_value);
    let _ = globals
        .store
        .set_ivar(exc, IdentId::get_id("/receiver"), receiver);
    MonorubyErr::nameerr(msg).with_original(exc)
}

fn ivar_name_id(
    vm: &mut Executor,
    globals: &mut Globals,
    receiver: Value,
    arg: Value,
) -> Result<IdentId> {
    let name = if let Some(sym) = arg.try_symbol() {
        sym.get_name().to_string()
    } else if let Some(s) = arg.is_str() {
        s.to_string()
    } else if let Some(func_id) = globals.check_method(arg, IdentId::TO_STR) {
        let r = vm.invoke_func_inner(globals, func_id, arg, &[], None, None)?;
        match r.is_str() {
            Some(s) => s.to_string(),
            None => return Err(MonorubyErr::is_not_symbol_nor_string(&globals.store, arg)),
        }
    } else {
        return Err(MonorubyErr::is_not_symbol_nor_string(&globals.store, arg));
    };
    if !is_valid_ivar_name(&name) {
        // CRuby surfaces the given name and the receiver on this
        // NameError (`#name` / `#receiver`); `#name` is the *exact*
        // argument object (a String/Symbol), which the spec checks with
        // `equal?`, so carry the original `arg` value.
        return Err(name_error_reflection(
            globals,
            format!("'{name}' is not allowed as an instance variable name"),
            arg,
            receiver,
        ));
    }
    Ok(IdentId::get_id(&name))
}

///
/// ### Kernel#instance_variable_defined?
///
/// - instance_variable_defined?(var) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_variable_defined=3f.html]
#[monoruby_builtin]
fn iv_defined(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let id = ivar_name_id(vm, globals, lfp.self_val(), lfp.arg(0))?;
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
fn iv_set(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let id = ivar_name_id(vm, globals, lfp.self_val(), lfp.arg(0))?;
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
fn iv_get(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let id = ivar_name_id(vm, globals, lfp.self_val(), lfp.arg(0))?;
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
fn iv_remove(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let id = ivar_name_id(vm, globals, lfp.self_val(), lfp.arg(0))?;
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
    fn clone_freeze_coverage() {
        // Kernel#clone `freeze:` keyword: forced frozen state, ArgumentError on
        // a bad value, and `freeze: nil` inheriting the original's state.
        run_test_once(
            r##"(o=Object.new; a=o.clone(freeze: true).frozen?; b=(x="s".freeze; x.clone(freeze: false).frozen?); c=(begin; o.clone(freeze: 1); rescue => e; e.class; end); d=o.clone(freeze: nil).frozen?; e2=("f".freeze.clone(freeze: nil).frozen?); [a,b,c,d,e2])"##,
        );
        // `freeze:` is forwarded to #initialize_clone as a keyword argument.
        run_test_once(
            r##"(class KC; def initialize_clone(orig, **kw); $rec=kw; super(orig); end; end; obj=KC.new; obj.clone(freeze: true); r1=($rec == {freeze: true}); class KC2; def initialize_clone(orig); super; end; end; r2=(begin; KC2.new.clone(freeze: true); rescue => e; e.class; end); [r1, r2])"##,
        );
    }

    #[test]
    fn method_callee_coverage() {
        // __method__/__callee__ inside a define_method body, through send, and
        // in a normal method; nil outside any method.
        run_test_once(
            r##"(class MC; define_method(:dm){ [__method__, __callee__] }; def from_send; send "__method__"; end; def normal; __method__; end; end; o=MC.new; [o.dm, o.from_send, o.normal, (__method__)])"##,
        );
    }

    #[test]
    fn loop_coverage() {
        // No-block Enumerator (infinite size) and rescue of StopIteration
        // subclasses / a finished iterator.
        run_test_once(
            r##"(a=loop.instance_of?(Enumerator); b=(loop.size == Float::INFINITY); sub=Class.new(StopIteration); c=(loop{ raise sub }; :done); e=[1,2].each; d=(loop{ e.next }; :finished); [a,b,c,d])"##,
        );
    }

    #[test]
    fn format_alias_coverage() {
        // format/sprintf alias identity (instance + module method) and the
        // private default respond_to_missing?.
        run_test_once(
            r##"(a=Kernel.instance_method(:format)==Kernel.instance_method(:sprintf); b=Kernel.method(:format)==Kernel.method(:sprintf); c=Kernel.private_method_defined?(:respond_to_missing?); e2=format("%03d", 7); [a,b,c,e2])"##,
        );
    }

    #[test]
    fn caller_start_length_and_range() {
        // `caller(start, length)` and `caller(range)`. Assert via sizes and
        // booleans so the result is independent of file paths / the toplevel
        // frame label (which differ between the monoruby and CRuby harnesses).
        run_test(
            r#"
            def lvl3
              [
                caller(1, 1).size,    # immediate caller only
                caller(1, 0).size,    # capped to zero
                caller(2, 1).size,    # one frame, two up
                caller(1..1).size,    # inclusive range, one frame
                caller(1...3).size,   # exclusive range, two frames
                caller(1, 1) == caller(1..1),
              ]
            end
            def lvl2; lvl3; end
            def lvl1; lvl2; end
            lvl1
            "#,
        );
    }

    #[test]
    fn super_from_redefined_builtin_716() {
        // #716: `super` from a method that redefines a builtin must walk
        // to the real ancestor, not re-enter the replaced builtin.
        //
        // `to_s`: super reaches the default `Object#to_s`, which returns
        // "#<ClassName:0x...>" for *every* receiver, including immediates
        // (previously returned the value literal like "true"/"3").
        //
        // Arithmetic operators are not defined on `Numeric` (CRuby
        // semantics), so `super` from a redefined `Integer#+` / `Float#*`
        // finds no super method and raises `NoMethodError`.
        //
        // NOTE: `run_test_once` (single interpreter run) is used
        // deliberately. Re-running these `super`-from-a-redefined-
        // immediate-`to_s` snippets enough times to JIT-compile them
        // trips a *separate, pre-existing* JIT codegen bug (the compiled
        // `super` corrupts a surrounding register); that is tracked
        // separately and is independent of the method-resolution fix
        // verified here.
        run_test_once(
            r##"class TrueClass; def to_s; "T:" + super; end; end
                true.to_s.sub(/0x[0-9a-f]+/, "0xADDR")"##,
        );
        run_test_once(
            r##"class NilClass; def to_s; "N:" + super; end; end
                nil.to_s.sub(/0x[0-9a-f]+/, "0xADDR")"##,
        );
        run_test_once(
            r##"class Integer; def to_s; "I:" + super; end; end
                5.to_s.sub(/0x[0-9a-f]+/, "0xADDR")"##,
        );
        run_test_once(
            r##"class Symbol; def to_s; "S:" + super; end; end
                :foo.to_s.sub(/0x[0-9a-f]+/, "0xADDR")"##,
        );
        // a bare object's default to_s is unchanged
        run_test_once(r##"Object.new.to_s.sub(/0x[0-9a-f]+/, "0xADDR")"##);
        // immediates' own (non-super) to_s is unaffected
        run_test(r##"[true.to_s, 5.to_s, nil.to_s, :foo.to_s]"##);
        // arithmetic super → NoMethodError (Numeric has no operators)
        run_test_once(
            r##"class Integer; def +(o); super(o) * 10; end; end
                begin; 1 + 2; rescue NoMethodError; :nme; end"##,
        );
        run_test_once(
            r##"class Float; def *(o); super(o); end; end
                begin; 2.0 * 3; rescue NoMethodError; :nme; end"##,
        );
    }

    #[test]
    fn not_match_memoized_dispatch() {
        // Kernel#!~ resolves `=~` through a class_version-stamped memo
        // (and the JIT inlines the whole call); a later `=~`
        // (re)definition must be picked up, $~ must still be set, and a
        // receiver without `=~` keeps NoMethodError.
        run_test_once(
            r##"
        res = []
        30.times { res << ("abc" !~ /b/) << ("abc" !~ /z/) << (/z/ !~ "ab") }
        "xyz" !~ /y(z)/
        res << $1
        class MatchM713; def =~(o); o == 1; end; end
        m = MatchM713.new
        30.times { res << (m !~ 1) << (m !~ 2) }
        class String; def =~(o); nil; end; end
        res << ("abc" !~ /b/)
        res << begin; Object.new !~ /x/; rescue NoMethodError; :nme; end
        res.uniq
        "##,
        );
    }

    #[test]
    fn define_singleton_method_super() {
        run_test(
            r##"
        cls = Class.new do
          def bar; ['a']; end
        end
        object = cls.new
        object.define_singleton_method(:bar) { ['b', *super()] }
        a = object.bar
        object.define_singleton_method(:baz, proc { |x| ['z', x] })
        a + object.baz(1)
        "##,
        );
    }

    #[test]
    fn define_singleton_method_errors() {
        run_test_error(
            r##"
        o = Object.new
        o.freeze
        o.define_singleton_method(:foo) { 1 }
        "##,
        );
        run_test_error(
            r##"
        class P1; end
        other = P1.new
        p1 = P1.new
        class << p1
          def sm; :s; end
        end
        um = p1.method(:sm).unbind
        other.send(:define_singleton_method, :osm, um)
        "##,
        );
    }

    #[test]
    fn instance_variable_name_validation() {
        run_test(
            r##"
        o = Object.new
        o.instance_variable_set(:@a, 1)
        s = Object.new
        def s.to_str; "@a"; end
        [o.instance_variable_get(s), o.instance_variable_get("@a"),
         o.instance_variable_defined?(:@a), o.instance_variable_defined?(:@z)]
        "##,
        );
        run_test_error(r##"Object.new.instance_variable_get(:foo)"##);
        run_test_error(r##"Object.new.instance_variable_get(:"@")"##);
        run_test_error(r##"Object.new.instance_variable_get("@1x")"##);
        run_test_error(r##"Object.new.instance_variable_set("@@c", 1)"##);
        run_test_error(r##"Object.new.instance_variable_get(42)"##);
        run_test_error(r##"Object.new.instance_variable_defined?(:bad)"##);
        run_test_error(r##"Object.new.instance_variable_get(Object.new)"##);
        // #to_str returning a non-String is a TypeError.
        run_test_error(
            r##"o=Object.new; def o.to_str; 5; end; Object.new.instance_variable_get(o)"##,
        );
    }

    #[test]
    fn path_arg_to_path_to_str_coercion() {
        // Exercises `path_arg_to_string` (#to_path then #to_str, and
        // the TypeError arm) through `load`.
        run_test_once(
            r##"
        pth = "/tmp/mono_cov_#{Process.pid}_#{rand(1 << 30)}.rb"
        File.write(pth, "$cov = (defined?($cov) && $cov ? $cov : 0) + 1\n")
        $cov = 0
        $pth = pth
        o1 = Object.new
        def o1.to_path; $pth; end
        load(o1)
        inner = Object.new
        def inner.to_str; $pth; end
        $inner = inner
        o2 = Object.new
        def o2.to_path; $inner; end
        load(o2)
        err = begin
          load(Object.new)
          :no
        rescue TypeError
          :te
        end
        File.delete(pth)
        [$cov, err]
        "##,
        );
    }

    #[test]
    fn kernel_putc() {
        run_test(r##"putc(65); putc("Hi"); o=Object.new; def o.to_int; 66; end; putc(o); putc(67)"##);
        run_test_error(r##"putc(nil)"##);
        run_test_error(r##"putc(true)"##);
        run_test_error(r##"putc(false)"##);
        run_test_error(r##"putc(Object.new)"##);
    }

    #[test]
    fn nil() {
        run_tests(&[
            r##"'woo'.nil?"##,
            r##"3.nil?"##,
            r##":x.nil?"##,
            r##"nil.nil?"##,
            r##"false.nil?"##,
        ]);
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
        run_tests(&[
            r##"eval "1+2+3""##,
            r##"
        res = []
        5.times do |a|
            5.times do |b|
                eval "res << a; res << b"
            end
        end
        res
        "##,
        ]);
        run_test_error(r##"eval "1/0""##);
        run_test_error(r##"eval "jk""##);
        // Unsupported eval'd syntax is a catchable SyntaxError, not a
        // process-aborting FatalError.
        run_test(
            r##"
        begin
          eval("x=1; y=2; x..y if x")
          :no_raise
        rescue SyntaxError
          :syntax
        end
        "##,
        );
    }

    #[test]
    fn eval_begin_block() {
        // `BEGIN { ... }` (PreExecutionNode), incl. `return`.
        run_test(r##"def m(n); eval("BEGIN {return n*3}"); end; m(4)"##);
    }

    #[test]
    fn catch_throw_uncaught() {
        run_tests(&[
            r##"catch(:x) { throw :x, 42 }"##,
            r##"catch(:a) { catch(:b) { throw :a, 7 } }"##,
            r##"
        begin
          throw :nope
        rescue UncaughtThrowError => e
          [e.is_a?(ArgumentError), e.message]
        end
        "##,
        ]);
        run_test_error(r##"throw :nope"##);
    }

    #[test]
    fn ensure_defers_nonlocal_unwind() {
        // A non-local `return` / `throw` in flight is suspended across the
        // `ensure` body so the body can itself raise / return / throw
        // without tripping the empty-exception guard in `set_error`.
        // Regression for the `assert_eq!(self.exception, None)` panic.
        run_tests(&[
            // return from block honored when the ensure body is clean
            r##"
            def m
              begin
                [1].each { return :returned }
              ensure
                :ignored
              end
              :fell_through
            end
            m
            "##,
            // a raise in the ensure body overrides the pending return
            r##"
            def m
              begin
                [1].each { return :returned }
              ensure
                raise "boom"
              end
            rescue => e
              "overridden:#{e.message}"
            end
            m
            "##,
            // a builtin-originated error (NoMethodError) in the ensure body
            r##"
            def m
              begin
                [1].each { return :returned }
              ensure
                nil.no_such_method
              end
            rescue NoMethodError
              :rescued_nomethod
            end
            m
            "##,
            // a return in the ensure body overrides the body's return
            r##"
            def m
              begin
                [1].each { return :body }
              ensure
                [1].each { return :ensure_ret }
              end
            end
            m
            "##,
            // throw honored across a clean ensure
            r##"
            def m
              catch(:t) do
                begin
                  throw :t, :thrown
                ensure
                  :clean
                end
              end
            end
            m
            "##,
            // throw overridden by a raise in the ensure body
            r##"
            def m
              catch(:t) do
                begin
                  throw :t, :thrown
                ensure
                  raise "boom"
                end
              end
            rescue => e
              "overridden:#{e.message}"
            end
            m
            "##,
            // nested method deferrals: foo's ensure calls baz, which also
            // does return-from-block + ensure. Both must restore correctly.
            r##"
            def baz
              begin
                [1].each { return :baz_ret }
              ensure
                :baz_clean
              end
            end
            def foo
              begin
                [1].each { return :foo_ret }
              ensure
                $baz_result = baz
              end
            end
            [foo, $baz_result]
            "##,
            // a callee's own clean ensure must not steal the caller's
            // pending deferred return
            r##"
            def inner
              begin
                :ran
              ensure
                :ensured
              end
            end
            def outer
              begin
                [1].each { return :outer_ret }
              ensure
                inner
              end
            end
            outer
            "##,
            // nested ensures in the same method both run; return honored.
            // ($log is reset each run since run_test executes 25x in one
            // process while the CRuby reference runs once.)
            r##"
            def m
              begin
                begin
                  [1].each { return :v }
                ensure
                  $log << :inner
                end
              ensure
                $log << :outer
              end
            end
            $log = []
            [m, $log]
            "##,
        ]);
    }

    #[test]
    fn block_shadow_locals() {
        // Block-local shadow vars (`|x; a|`).
        run_test(
            r##"
        x = 10
        [1, 2, 3].each { |i; x| x = i * 100 }
        a = nil
        f = ->(n; a) { a = n + 1; a }
        [x, f.call(2), a.inspect, (proc { |; z| z = 9; z }.call)]
        "##,
        );
    }

    #[test]
    fn array_implicit_hash_element() {
        run_tests(&[
            r##"[args: [1, 2], kw: {a: "b"}]"##,
            r##"[1, foo: 2, bar: 3]"##,
            r##"[0, {a: 1}, b: 2]"##,
        ]);
    }

    #[test]
    fn eval_lineno() {
        run_tests(&[
            r#"eval("__LINE__", nil, "test.rb", 42)"#,
            r#"eval("__FILE__", nil, "test.rb", 42)"#,
            r#"eval("__LINE__\n__LINE__", nil, "test.rb", 10)"#,
        ]);
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
        run_tests(&[
            r#"Float(0.0)"#,
            r#"Float(1)"#,
            r#"Float(1000000000000000000000000000000000000000000000000000000000000)"#,
            r#"Float('10.0')"#,
            r#"Float('   10.0')"#,
            r#"Float(' -0.7e-10')"#,
        ]);
        run_test_error(r#"Float(' -0.7 5')"#);
        run_test_error(r#"Float(' -0.7e-10z')"#);
        run_test_error(r#"Float(:Ruby)"#);
    }

    #[test]
    fn complex() {
        run_tests(&[r#"Complex(30)"#, r#"Complex(30, 4.5)"#]);
    }

    #[test]
    fn array() {
        run_tests(&[
            r#"Array([100])"#,
            r#"Array(100)"#,
            r#"Array(nil)"#,
            r#"Array("100")"#,
        ]);
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
        run_test_no_result_check("sleep 0.05");
        run_tests(&["system 'ls'", "system 'ls', '-a'"]);
        run_test_error("abort 1");
        run_test_no_result_check("exit");
        run_test_no_result_check("exit 0");
        run_test_no_result_check("__dir__");
        run_tests(&[
            r#"eval("__dir__", nil, "foo.rb")"#,
            r#"eval("__dir__", nil, "foo/bar.rb")"#,
            r#"eval("__dir__", binding).inspect"#,
        ]);
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
        run_tests(&[
            "proc {|x| x * 2}.call(1)",
            "proc {|x, y| x * y}.call(7,2)",
            "proc {|x, y| [x, y]}.call(7)",
            "proc {|x, y| [x, y]}.call(7, 5)",
            "proc {|x, y| [x, y]}.call(7, 5, 15)",
            "lambda {|x| x * 2}.call(1)",
            "lambda {|x, y| x * y}.call(7,2)",
        ]);
        run_test_error("lambda {|x| puts x}.call(1,2)");
    }

    #[test]
    fn warn() {
        run_tests(&[
            r#"warn("woo")"#,
            r#"warn("woo", :boo, 100)"#,
            r#"warn(100, uplevel:1)"#,
            r#"warn(100, category: :experimental)"#,
        ]);
        run_test_error(r#"raise "Woo""#);
        // warn writes to $stderr (not directly to OS stderr)
        run_test(
            r##"
            class MyIO; def initialize; @s = ""; end; def write(s); @s += s.to_s; end; def to_s; @s; end; end
            old = $stderr; io = MyIO.new; $stderr = io
            warn "hello"
            $stderr = old
            io.to_s
            "##,
        );
    }

    // Disabled: forking from a multi-threaded cargo test runner can leave
    // child processes deadlocked in futex_wait_queue, hanging the test run.
    //#[test]
    //fn exec_in_fork() {
    //    // fork test
    //    run_test(
    //        r##"
    //    fork { puts 42 }
    //    42
    //    "##,
    //    );
    //}

    //#[test]
    //fn exec() {
    //    // exec with multiple args (no shell): verify via system() calling a
    //    // tiny script that execs /bin/echo and captures output in the shell.
    //    run_test(
    //        r##"
    //    `sh -c '/bin/echo hello world'`.chomp
    //    "##,
    //    );
    //    // exec replaces the process: code after exec is not reached.
    //    // Verified by checking only "replaced" appears in output, not "NOT_REACHED".
    //    run_test_no_result_check(
    //        r##"
    //    pid = fork do
    //      exec "/bin/echo", "replaced"
    //      STDERR.puts "NOT_REACHED"
    //    end
    //    pid
    //    "##,
    //    );
    //    // exec with single string (uses shell)
    //    run_test_no_result_check(
    //        r##"
    //    pid = fork do
    //      exec "exit 0"
    //    end
    //    pid
    //    "##,
    //    );
    //    // exec with keyword argument (should not error)
    //    run_test_no_result_check(
    //        r##"
    //    pid = fork do
    //      exec "/bin/echo", "kw", close_others: false
    //    end
    //    pid
    //    "##,
    //    );
    //}

    #[test]
    fn exit_raises_system_exit() {
        run_test(
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
        run_test(
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
        run_test(
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
    fn abort_writes_to_dollar_stderr() {
        run_test(
            r##"
        class MyIO; def initialize; @s=+""; end; def write(*a); a.each{|x| @s<<x.to_s}; end; attr_reader :s; end
        io = MyIO.new
        old = $stderr
        $stderr = io
        begin
          begin; abort("a message"); rescue SystemExit; end
        ensure
          $stderr = old
        end
        io.s
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
        run_tests2(&[
            ":sym.class.to_s",
            "5.class.to_s",
            "5.7.class.to_s",
            "'windows'.class.to_s",
            "puts 100",
            "print '100'",
            "p",
            "p 1",
            "p 1,2",
            "p 1,2,3",
            "nil.respond_to?(:foo)",
            "nil.inspect",
        ]);
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
        run_tests(&[
            "1.dup",
            "1.5.dup",
            "'Ruby'.dup",
            ":Ruby.dup",
            "[1,2,3].dup",
            "{a:1,b:2}.dup",
            "(1..3).dup",
        ]);
    }

    #[test]
    fn dup_module() {
        run_test(
            r#"
            m = Module.new
            m.define_method(:foo) { 42 }
            d = m.dup
            d.instance_methods.include?(:foo)
            "#,
        );
        // dup should not share method table
        run_test_once(
            r#"
            m = Module.new
            m.define_method(:foo) { 42 }
            d = m.dup
            d.undef_method(:foo)
            m.instance_methods.include?(:foo)
            "#,
        );
        run_test_error("BasicObject.dup");
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
        run_tests2(&[r#"Integer(-2435)"#, r#"Integer("2435")"#]);
        run_test_error(r#"Integer("2435.78")"#);
        run_test_error(r#"Integer([4])"#);
        run_tests2(&[
            r#"Integer(-2435766756886769978978435)"#,
            r#"Integer(2435.4556787)"#,
        ]);
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
    fn kernel_integer_prefixed_strings() {
        // Hex / oct / bin / decimal prefixes via autodetect (base 0).
        run_tests(&[
            r#"Integer("0x42")"#,
            r#"Integer("0X42")"#,
            r#"Integer("0b101")"#,
            r#"Integer("0B101")"#,
            r#"Integer("0o17")"#,
            r#"Integer("0O17")"#,
            r#"Integer("0d42")"#,
            r#"Integer("042")"#, // leading 0 → octal
            r#"Integer("42")"#,
            r#"Integer("-0x42")"#,
            r#"Integer(" +0x42 ")"#, // whitespace tolerated
            r#"Integer("1_000_000")"#,
            // Explicit base.
            r#"Integer("42", 16)"#,
            r#"Integer("42", 8)"#,
            r#"Integer("0x42", 16)"#, // matching prefix is OK
            // Invalid forms surface ArgumentError; capture the message
            // to verify the quoted-input wording.
            r#"begin; Integer("0x"); rescue ArgumentError => e; e.message; end"#,
            r#"begin; Integer("12abc"); rescue ArgumentError => e; e.message; end"#,
            r#"begin; Integer("0xZZ"); rescue ArgumentError => e; e.message; end"#,
            r#"begin; Integer("__1"); rescue ArgumentError => e; e.message; end"#,
            r#"begin; Integer("1__1"); rescue ArgumentError => e; e.message; end"#,
            r#"begin; Integer("0x42", 10); rescue ArgumentError => e; e.message; end"#,
        ]);
    }

    #[test]
    fn kernel_float_hex_and_underscore() {
        // Hex floats.
        run_tests(&[
            r#"Float("0xA")"#,
            r#"Float("0X1f")"#,
            r#"Float("0xA.B")"#,
            r#"Float("0xA.Bp3")"#,
            r#"Float("0x1.8p+3")"#,
            // Decimal with underscores between digits.
            r#"Float("10_1_0.5_5_5")"#,
            r#"Float("1.5e1_0")"#,
            // Whitespace and sign.
            r#"Float(" -1.5 ")"#,
        ]);
        // Forms CRuby rejects.
        run_tests(&[
            r#"begin; Float(""); rescue ArgumentError => e; e.message; end"#,
            r#"begin; Float("nan"); rescue ArgumentError => e; e.message; end"#,
            r#"begin; Float("inf"); rescue ArgumentError => e; e.message; end"#,
            r#"begin; Float("10__10"); rescue ArgumentError => e; e.message; end"#,
            r#"begin; Float("_1.0"); rescue ArgumentError => e; e.message; end"#,
            r#"begin; Float("1.0_"); rescue ArgumentError => e; e.message; end"#,
            r#"begin; Float("0b1"); rescue ArgumentError => e; e.message; end"#,
            r#"begin; Float("10e10.5"); rescue ArgumentError => e; e.message; end"#,
        ]);
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
    fn kernel_rational() {
        run_tests(&[
            // Integer arguments
            "Rational(3, 4).to_s",
            "Rational(6, 4).to_s",
            // Single integer
            "Rational(5).to_s",
            // Float argument
            "Rational(1.5).to_s",
            "Rational(0.0).to_s",
            // Two floats
            "Rational(1.5, 2.0).to_s",
            // Mixed float/int
            "Rational(1.5, 3).to_s",
            // Rational passthrough
            "Rational(Rational(3, 4)).to_s",
            // Rational / Integer
            "Rational(Rational(3, 4), 2).to_s",
        ]);
        // Division by zero
        run_test_error("Rational(1, 0)");
        // Type error
        run_test_error("Rational(:foo)");
    }

    /// `Rational(*, exception: false)` swallows conversion errors and
    /// returns nil instead of raising — same shape as
    /// `Float(*, exception: …)` and `Integer(*, exception: …)`.
    #[test]
    fn kernel_rational_exception_kw() {
        run_tests(&[
            // Symbol can't coerce; with exception: false ⇒ nil.
            r#"Rational(:foo, exception: false).inspect"#,
            // Two-arg form also honors the kwarg.
            r#"Rational(:foo, 2, exception: false).inspect"#,
            // exception: true is the default — still returns the result.
            r#"Rational(3, 2, exception: true).to_s"#,
        ]);
        // exception: true preserves the error (`Rational(:foo)`).
        run_test_error(r#"Rational(:foo, exception: true)"#);
    }

    #[test]
    fn kernel_integer_exception_and_coercion() {
        // `exception: false` swallows conversion errors -> nil.
        run_tests(&[
            r#"Integer("abc", exception: false).inspect"#,
            r#"Integer(nil, exception: false).inspect"#,
            r#"Integer("---1", exception: false).inspect"#,
            r#"Integer("42", exception: false)"#,
        ]);
        // NaN / Infinity raise FloatDomainError.
        run_test_error("Integer(0.0 / 0.0)");
        run_test_error("Integer(1.0 / 0.0)");
        // to_int returning a non-Integer falls back to to_i.
        run_test_with_prelude(
            r#"Integer(o)"#,
            r#"class C; def to_int; "1"; end; def to_i; 42; end; end; o = C.new"#,
        );
        run_test_with_prelude(
            r#"Integer(o)"#,
            r#"class C; def to_int; nil; end; def to_i; 7; end; end; o = C.new"#,
        );
    }

    #[test]
    fn kernel_float_exception_and_overflow() {
        // Overflow of a valid literal -> Infinity (not ArgumentError).
        run_tests(&[
            r#"Float("2e1000").infinite?"#,
            r#"Float("2E1000") == Float::INFINITY"#,
            r#"Float("abc", exception: false).inspect"#,
            r#"Float(nil, exception: false).inspect"#,
        ]);
        // nil -> TypeError.
        run_test_error("Float(nil)");
        // `_` adjacent to the exponent marker is rejected.
        run_test_error(r#"Float("2e_100")"#);
        run_test_error(r#"Float("2_e100")"#);
        run_test_error(r#"Float("20e100_")"#);
        // #to_f must return a Float, not an Integer.
        run_test_error(
            r#"o = Object.new; def o.to_f; 123; end; Float(o)"#,
        );
    }

    #[test]
    fn kernel_complex_string_parsing() {
        run_tests(&[
            r#"Complex("9").to_s"#,
            r#"Complex("-3").to_s"#,
            r#"Complex("2/3").to_s"#,
            r#"Complex("4+2/3i").to_s"#,
            r#"Complex("2.3").to_s"#,
            r#"Complex("4+2.3i").to_s"#,
            r#"Complex("35i").to_s"#,
            r#"Complex("i").to_s"#,
            r#"Complex("-i").to_s"#,
            r#"Complex("79+4i").to_s"#,
            r#"Complex("79-i").to_s"#,
            r#"Complex("79+4J").to_s"#,
            r#"Complex("2e3+4i").to_s"#,
            r#"Complex("  79+4i  ").to_s"#,
            r#"Complex("7_9+4_0i").to_s"#,
            // exception: false swallows parse errors.
            r#"Complex("ruby", exception: false).inspect"#,
            r#"Complex("79+4iruby", exception: false).inspect"#,
            r#"Complex("NaN", exception: false).inspect"#,
            r#"Complex("7__9+4__0i", exception: false).inspect"#,
        ]);
        // Invalid strings raise ArgumentError; nil raises TypeError.
        run_test_error(r#"Complex("ruby")"#);
        run_test_error(r#"Complex("Infinity")"#);
        run_test_error("Complex(nil)");
        run_test_error("Complex(0, nil)");
        // #to_c coercion for a single non-Numeric argument.
        run_test_with_prelude(
            r#"Complex(o).to_s"#,
            r#"class C; def to_c; Complex(0, 1); end; end; o = C.new"#,
        );
    }

    #[test]
    fn kernel_conversion_branch_coverage() {
        // Integer: bare nil -> TypeError; -Infinity branch; valid
        // float truncation; exception:false on a float-domain error.
        run_test_error("Integer(nil)");
        run_test_error("Integer(-1.0 / 0.0)");
        run_tests(&[
            "Integer(3.99)",
            "Integer(-3.99)",
            r#"Integer(0.0 / 0.0, exception: false).inspect"#,
            r#"Integer(1.0 / 0.0, exception: false).inspect"#,
            r#"Integer("0b1010")"#,
            r#"Integer("0o17")"#,
            r#"Integer("0xff")"#,
            r#"Integer("  -10  ")"#,
        ]);
        run_test_with_prelude(
            r#"Integer(o)"#,
            r#"class C; def to_int; 99; end; end; o = C.new"#,
        );
        // Float: successful #to_f coercion; exception:false -> nil;
        // hex float literal string.
        run_test_with_prelude(
            r#"Float(o)"#,
            r#"class C; def to_f; 1.5; end; end; o = C.new"#,
        );
        run_tests(&[
            r#"Float(nil, exception: false).inspect"#,
            r#"Float("1.0e3")"#,
            r#"Float("0x1.8p3")"#,
            r#"Float(:sym, exception: false).inspect"#,
        ]);
        run_test_error("Float(:sym)");
        // Complex: exception:false -> nil; symbol real part; numeric
        // pairs; invalid polar.
        run_tests(&[
            r#"Complex(nil, exception: false).inspect"#,
            r#"Complex(1, 2).to_s"#,
            r#"Complex(1.5, -2).to_s"#,
            r#"Complex("ruby", exception: false).inspect"#,
        ]);
        // A non-real (Symbol) part is a TypeError even with
        // `exception: false` (CRuby raises regardless).
        run_test_error("Complex(:x, 0)");
        run_test_error("Complex(:x, 0, exception: false)");
        run_test_error(r#"Complex("1@")"#);
    }

    #[test]
    fn object_instance_of() {
        run_tests2(&[
            r#"5.instance_of?(Integer)"#,
            r#"5.instance_of?(Float)"#,
            r#":ruby.instance_of?(Symbol)"#,
            r#":ruby.instance_of?(Object)"#,
        ]);
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
        run_tests(&[
            "4.is_a? Integer",
            "4.5.is_a? Integer",
            "'Ruby'.is_a? Integer",
            "4.5.is_a? Float",
            "'Ruby'.is_a? Float",
        ]);
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
        // Walks include / superclass chain so JIT must consult ancestors.
        // kind_of? alias resolves to the same inline.
        run_tests(&[
            r#"
            [5.is_a?(Integer), 5.is_a?(Numeric), 5.is_a?(Comparable),
             5.is_a?(Object), 5.is_a?(Kernel), 5.is_a?(BasicObject),
             5.is_a?(Float), 5.is_a?(String)]
            "#,
            r#"[5.kind_of?(Integer), 5.kind_of?(Float), "x".kind_of?(String)]"#,
        ]);
        // Make is_a? called from a hot method so the JIT compiles and
        // fires the inline (run_test runs the snippet 25 times).
        run_test(
            r#"
            def check(o); o.is_a?(Integer); end
            r = []
            1000.times { r << check(1) }
            r.uniq
            "#,
        );
    }

    #[test]
    fn object_nil() {
        run_tests(&[
            "4.nil?",
            "4.5.nil?",
            "nil.nil?",
            "true.nil?",
            "false.nil?",
            "[].nil?",
        ]);
    }

    #[test]
    fn kernel_system() {
        run_tests(&[
            r#"system "ls""#,
            r#"system "jkjkjk""#,
            r#"system "*""#,
            r#"`pwd`"#,
            r#"`*`"#,
        ]);
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
        // private method => NoMethodError
        run_test_error(
            r##"
        class C
          private def pri; "private"; end
        end
        C.new.public_send(:pri)
        "##,
        );
        // protected method => NoMethodError even from same class hierarchy
        run_test_error(
            r##"
        class C
          protected def pro; "protected"; end
          def call_pro(other); other.public_send(:pro); end
        end
        C.new.call_pro(C.new)
        "##,
        );
        // protected method from outside => NoMethodError
        run_test_error(
            r##"
        class C
          protected def pro; "protected"; end
        end
        C.new.public_send(:pro)
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
        run_tests(&["respond_to?(:exit!)", "Process.respond_to?(:exit!)"]);
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
        run_test_with_prelude(
            "format(MyFmt.new, 42)",
            r#"
            class MyFmt
              def to_str; "num: %d"; end
            end
            "#,
        );
    }

    #[test]
    fn sprintf_result_encoding() {
        // The result keeps the format String's encoding, widening to an
        // argument's when the format encoding is more restrictive.
        run_test_once(
            r##"(a=format('%s'.encode(Encoding::US_ASCII), 'foobar').encoding.name; b=format('%s'.encode(Encoding::UTF_8), 'x').encoding.name; arg=[98,195,188,114].pack("C*").force_encoding(Encoding::UTF_8); c=("foo %s".dup.force_encoding(Encoding::US_ASCII) % arg).encoding.name; [a,b,c])"##,
        );
    }

    #[test]
    fn sprintf_encoding_error_and_debug() {
        // Incompatible non-ASCII encodings raise Encoding::CompatibilityError;
        // a non-String format operand is coerced via #to_str; excess positional
        // args raise ArgumentError under $DEBUG.
        run_test_once(
            r##"[ (begin; format("hello %s".encode("utf-8"), "world".encode("UTF-16LE")); rescue => e; e.class; end), (o=Object.new; def o.to_str; "%d!"; end; format(o, 5)), (old=$DEBUG; $DEBUG=true; r=(begin; format("test", 1); rescue => e; e.class; end); $DEBUG=old; r) ]"##,
        );
    }

    #[test]
    fn sprintf_hash_flag_zero_precision() {
        // `#` on b/B/x/X does nothing for a zero argument with zero precision.
        run_test(
            r##"[sprintf('%#.0b',0), sprintf('%#.0B',0), sprintf('%#.0x',0), sprintf('%#.0X',0), sprintf('%#b',0), sprintf('%#b',5), sprintf('%#x',255)]"##,
        );
    }

    #[test]
    fn sprintf_verbose_too_many_arguments() {
        // With $VERBOSE, excess positional args warn to $stderr.
        run_test_once(
            r##"(require "stringio"; old=$VERBOSE; $VERBOSE=true; s1=StringIO.new; $stderr=s1; format("test",1); w=s1.string; $stderr=STDERR; $VERBOSE=old; w.include?("too many arguments for format string"))"##,
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
        run_tests(&[
            r#"
            begin
              sprintf("%d", Object.new)
            rescue TypeError => e
              e.message
            end
            "#,
            r#"
            begin
              sprintf("%f", Object.new)
            rescue TypeError => e
              e.message
            end
            "#,
        ]);
    }

    #[test]
    fn sprintf_positional_and_named() {
        run_tests(&[
            r#"sprintf("%1$d %2$d %1$d", 10, 20)"#,
            r#"sprintf("%1$s %2$s %1$s", "a", "b")"#,
            r#"sprintf("%2$d", 10, 20)"#,
            r#"sprintf("%1$05d", 42)"#,
            r#"sprintf("%1$x", 255)"#,
            r#"sprintf("%1$o", 8)"#,
            r#"sprintf("%1$f", 3.14)"#,
            r#"sprintf("%{foo}", foo: "hello")"#,
            r#"sprintf("%{foo} %{bar}", foo: 1, bar: 2)"#,
            r#"sprintf("%{foo} %{foo}", foo: "x")"#,
            r#"sprintf("%<foo>d", foo: 42)"#,
            r#"sprintf("%<foo>05d", foo: 42)"#,
            r#"sprintf("%<foo>10d", foo: 42)"#,
            r#"sprintf("%<foo>x", foo: 255)"#,
            r#"sprintf("%<foo>f", foo: 3.14)"#,
            r#"sprintf("%<foo>s", foo: "hello")"#,
            // Positional `*N$` width / `.*N$` precision stars, mixed
            // with a positional value reference.
            r#""%*1$.*2$3$d" % [10, 5, 1]"#,
            r#""%*1$.*2$3$d" % [-10, 5, 1]"#,
            r#""%-*1$.*2$3$d" % [10, 5, 1]"#,
            r#""%-*1$.*2$3$d" % [-10, 5, 1]"#,
            r#""%*1$.*2$3$d" % [10, -5, 1]"#,
            r#""%2$*1$d" % [5, 42]"#,
            r#""%1$*2$d" % [42, 5]"#,
            // Negative `.*` precision is ignored (sequential form).
            r#""%.*s" % [-1, "abc"]"#,
            r#""%10.*d" % [-3, 7]"#,
        ]);
    }

    #[test]
    fn kernel_clone_and_hash() {
        run_tests(&[
            "[1,2,3].clone",
            r#""hello".clone"#,
            "{a: 1}.clone",
            "1.hash.is_a?(Integer)",
            r#""hello".hash.is_a?(Integer)"#,
            ":foo.hash.is_a?(Integer)",
        ]);
    }

    // --- tests for module_functions added in PR #328 ---

    #[test]
    #[allow(non_snake_case)]
    fn kernel_String_fn() {
        run_tests(&[
            r#"String(1)"#,
            r#"String(:foo)"#,
            r#"String("hello")"#,
            r#"String(nil)"#,
            r#"String(3.14)"#,
            // to_str takes precedence over to_s
            r#"
              class C
                def to_str; "str"; end
                def to_s;   "should_not_see"; end
              end
              String(C.new)
            "#,
        ]);
        // A String(...) of an object without to_s should raise
        // TypeError. Use a BasicObject subclass to avoid Object#to_s.
        run_test_error(
            r#"
              class X < BasicObject; end
              String(X.new)
            "#,
        );
    }

    #[test]
    #[allow(non_snake_case)]
    fn kernel_Hash_fn() {
        run_tests(&[
            r#"Hash(nil)"#,
            r#"Hash([])"#,
            r#"Hash({a: 1})"#,
            r#"
              class HLike
                def to_hash; {b: 2}; end
              end
              Hash(HLike.new)
            "#,
        ]);
        run_test_error(r#"Hash(1)"#);
        run_test_error(r#"Hash("str")"#);
    }

    #[test]
    fn kernel_srand_fn() {
        // srand returns the previous seed and seeds the global PRNG.
        // We can't compare PRNG output against CRuby's, so just assert
        // return type and stability of a seed round-trip.
        run_test(
            r#"
              old = srand(42)
              old.is_a?(Integer) && srand(old).is_a?(Integer)
            "#,
        );
    }

    #[test]
    fn kernel_test_fn() {
        run_tests(&[
            // `test(?e, path)` - exist?
            r#"test(?e, "/etc/hosts") == true"#,
            // `test(?f, path)` - regular file
            r#"test(?f, "/etc/hosts") == true"#,
            // `test(?d, path)` - directory
            r#"test(?d, "/etc") == true"#,
            // integer form accepted too
            r#"test(?e.ord, "/etc/hosts") == true"#,
        ]);
    }

    #[test]
    fn kernel_putc_fn() {
        // `putc` returns the argument unchanged. Writes to stdout as a
        // side effect -- the test framework captures that.
        run_tests(&[r#"putc(65)"#, r#"putc("Z")"#]);
    }

    // --- tests for ARGF stub added in PR #328 ---
    //
    // Monoruby's ARGF is a compatibility stub (plain `ARGFClass`) whose
    // internals are not identical to CRuby's C-level ARGF, so we don't
    // try to compare against CRuby's output here -- instead each test
    // raises if the assertion fails, which surfaces as a panic in the
    // Rust test harness.

    #[test]
    fn argf_basic_shape() {
        run_test_no_result_check(
            r#"
              raise "ARGF should include Enumerable" unless ARGF.is_a?(Enumerable)
              raise "ARGF.class should be a Class"   unless ARGF.class.is_a?(Class)
              raise "ARGF.filename should be '-'"    unless ARGF.filename == '-'
              raise "ARGF.argv should be an Array"   unless ARGF.argv.is_a?(Array)
              # mspec's `argf` helper constructs via ARGF.class.new(*argv)
              dup = ARGF.class.new('a.txt', 'b.txt')
              raise "new(*argv) argv wrong"          unless dup.argv == ['a.txt', 'b.txt']
              raise "new(a).filename wrong"          unless ARGF.class.new('a.txt').filename == 'a.txt'
              :ok
            "#,
        );
    }

    #[test]
    fn argf_enumerable_aliases() {
        run_test_no_result_check(
            r#"
              %i[path file each_line lines eof eof? readlines read to_a].each do |m|
                raise "ARGF missing ##{m}" unless ARGF.respond_to?(m)
              end
              :ok
            "#,
        );
    }

    // --- tests for FatalError class added in PR #328 ---

    #[test]
    fn fatal_error_class_shape() {
        // FatalError must be defined and sit directly under Exception
        // (so a bare `rescue` / `rescue StandardError` cannot see it).
        // CRuby doesn't define a `FatalError` constant, so compare-mode
        // tests would fail; use monoruby-only checks.
        run_test_no_result_check(
            r#"
              raise "FatalError not defined"                  unless defined?(FatalError) == 'constant'
              raise "FatalError must inherit Exception"       unless FatalError.ancestors.include?(Exception)
              raise "FatalError must NOT be a StandardError"  if     FatalError.ancestors.include?(StandardError)
              :ok
            "#,
        );
    }

    #[test]
    fn kernel_eql_and_loop() {
        run_tests(&[
            "1.eql?(1)",
            "1.eql?(1.0)",
            ":foo.eql?(:foo)",
            ":foo.eql?(:bar)",
            r#"
            x = 0
            loop do
              x += 1
              raise StopIteration if x == 5
            end
            x
            "#,
        ]);
    }

    #[test]
    fn kernel_caller() {
        run_test(
            r#"
            def foo
              caller
            end
            foo.is_a?(Array)
            "#,
        );
    }

    #[test]
    fn kernel_proc_lambda() {
        run_tests(&[
            "proc { 1 }.call",
            "lambda { 1 }.call",
            "proc { |x| x }.call(42)",
            "lambda { |x| x }.call(42)",
        ]);
    }

    #[test]
    fn kernel_raise_class() {
        run_test_error("raise ArgumentError");
        run_test_error(r#"raise TypeError, "custom""#);
    }

    /// `raise(cause:)` with no positional args ⇒ ArgumentError,
    /// `raise(obj)` where `obj.exception` returns an Exception ⇒
    /// raise that, and `obj.exception` returning a non-Exception ⇒
    /// `TypeError "exception object expected"`.
    #[test]
    fn kernel_raise_cause_and_duck() {
        run_test(
            r##"
            results = []
            # cause: only ⇒ ArgumentError "only cause is given with no arguments"
            begin
              raise(cause: nil)
            rescue ArgumentError => e
              results << [e.class.name, e.message]
            end
            # obj.exception(msg) returning an Exception
            obj = Object.new
            def obj.exception(msg); StandardError.new(msg); end
            begin
              raise obj, "hi"
            rescue => e
              results << [e.class.name, e.message]
            end
            # obj.exception returning non-Exception ⇒ TypeError "exception object expected"
            bad = Object.new
            def bad.exception; Array; end
            begin
              raise bad
            rescue TypeError => e
              results << [e.class.name, e.message]
            end
            # Plain object with no #exception ⇒ TypeError "exception class/object expected"
            begin
              raise 42
            rescue TypeError => e
              results << [e.class.name, e.message]
            end
            results
            "##,
        );
        // `fail` aliases `raise` — the cause-only path applies too.
        run_test_error(r#"fail(cause: nil)"#);
        // Bare `raise` with no current exception ⇒ RuntimeError
        // (the `$!`-reraise fallthrough).
        run_test_error(r#"raise"#);
    }

    #[test]
    fn kernel_format_error() {
        run_test_error("sprintf()");
    }

    // --- tests for Kernel#catch / Kernel#throw added in PR #440 ---

    #[test]
    fn kernel_catch() {
        run_tests(&[
            // Matching tag → catch returns the throw value.
            "catch(:done) { throw :done, 42 }",
            // No throw → catch returns the block's value.
            "catch(:done) { 99 }",
            // throw with no value defaults to nil.
            "catch(:done) { throw :done }.inspect",
            // The block receives the tag as its argument.
            "catch(:t) { |x| x }",
            // catch with no argument allocates a fresh Object as the tag and
            // yields it; throwing that exact object lets catch return the value.
            "catch { |t| throw t, :ok }",
        ]);
    }

    #[test]
    fn kernel_throw_across_method() {
        // throw unwinds across method boundaries up to the matching catch.
        run_test(
            r#"
            def emit(tag); throw tag, :from_emit; end
            catch(:x) { emit(:x) }
            "#,
        );
    }

    #[test]
    fn kernel_catch_2() {
        run_tests(&[
            // `ensure` runs on the throw path; `rescue` does NOT intercept it.
            r#"
            log = []
            result = catch(:done) do
              begin
                throw :done, 42
              rescue => e
                log << "rescued: #{e.class}"
              ensure
                log << "ensure"
              end
            end
            [result, log]
            "#,
            // Inner catch's tag does not match → the throw skips it and is
            // intercepted by the outer catch.
            r#"
            catch(:outer) do
              catch(:inner) do
                throw :outer, "to_outer"
              end
              "inner_returned"
            end
            "#,
            // Same symbolic tag at both levels → the innermost matching catch
            // wins; the outer catch then completes normally with the value of
            // the expression that follows it.
            r#"
            catch(:t) do
              catch(:t) { throw :t, "innermost" }
              "outer_after_inner"
            end
            "#,
        ]);
    }

    #[test]
    fn kernel_uncaught_throw() {
        // Throwing a tag with no matching catch raises an error.
        run_test_error("throw :nope");
    }

    #[test]
    fn kernel_inspect_ivars_and_hook() {
        // Comma-separated ivars; the `instance_variables_to_inspect`
        // hook selects (Array) / shows-all (nil); addresses normalized.
        // Classic `def...end` only (the ruruby parser used by `bin/test`
        // has no endless-method support).
        run_test(
            r#"
            o = Object.new
            o.instance_eval { @host="h"; @user="u"; @password="p" }
            a = o.inspect.sub(/0x[0-9a-f]+/, '0xX')
            o2 = Object.new
            o2.instance_eval { @host="h"; @user="u"; @password="p" }
            o2.singleton_class.class_eval do
              def instance_variables_to_inspect; [:@host, :@user, :@nope]; end
              private :instance_variables_to_inspect
            end
            b = o2.inspect.sub(/0x[0-9a-f]+/, '0xX')
            o3 = Object.new
            o3.instance_eval { @a=1 }
            o3.singleton_class.class_eval do
              def instance_variables_to_inspect; []; end
              private :instance_variables_to_inspect
            end
            c = o3.inspect.sub(/0x[0-9a-f]+/, '0xX')
            o4 = Object.new
            o4.instance_eval { @x=1; @y=2 }
            o4.singleton_class.class_eval do
              def instance_variables_to_inspect; nil; end
              private :instance_variables_to_inspect
            end
            d = o4.inspect.sub(/0x[0-9a-f]+/, '0xX')
            [a, b, c, d]
            "#,
        );
    }

    #[test]
    fn kernel_inspect_hook_type_error() {
        run_test_error(
            r#"
            o = Object.new
            o.instance_eval { @a=1 }
            o.singleton_class.class_eval do
              def instance_variables_to_inspect; {}; end
              private :instance_variables_to_inspect
            end
            o.inspect
            "#,
        );
    }

    #[test]
    fn objectspace_define_finalizer() {
        // Returns `[0, callable]`; `[0]` is comparable across runtimes.
        run_test(r#"ObjectSpace.define_finalizer(Object.new, ->(id){ id })[0]"#);
        run_test(r#"ObjectSpace.define_finalizer(Object.new){ |id| id }[0]"#);
        // A bound method is an acceptable callable.
        run_test(
            r#"
            h = Object.new
            def h.finalize(id); end
            ObjectSpace.define_finalizer(Object.new, h.method(:finalize))[0]
            "#,
        );
        // undefine_finalizer returns its argument.
        run_test(r#"o = Object.new; ObjectSpace.undefine_finalizer(o).equal?(o)"#);
    }

    #[test]
    fn objectspace_define_finalizer_errors() {
        // callable must respond to #call
        run_test_error(r#"ObjectSpace.define_finalizer(Object.new, Object.new)"#);
        // finalizer cannot be attached to a non-reference (immediate)
        run_test_error(r#"ObjectSpace.define_finalizer(:sym){ 1 }"#);
        // undefine on a frozen object raises FrozenError
        run_test_error(r#"o = Object.new; o.freeze; ObjectSpace.undefine_finalizer(o)"#);
    }

    #[test]
    fn kernel_local_variables() {
        // Method-wrapped so the test harness's own locals don't leak in.
        run_test(r#"def m; a = 1; b = 2; local_variables.sort; end; m"#);
        run_test(r#"def m(x, y); z = 3; local_variables.sort; end; m(1, 2)"#);
        run_test(r#"def m; local_variables; end; m"#);
        run_test(r#"def m; [1].each { |i| j = i * 2; return local_variables.sort }; end; m"#);
        // Anonymous / reserved parameter slots and the hidden `for`-loop
        // index must not appear.
        run_test(r#"def m(*args, **kw, &blk); local_variables.sort; end; m(1)"#);
        run_test(r#"def m; for a, *b in [[1, 2, 3]]; end; local_variables.sort; end; m"#);
        // `Binding#local_variables` shares the same filtering.
        run_test(r#"def m; a = 1; b = 2; binding.local_variables.sort; end; m"#);
        // The implicit `it` block parameter and numbered parameters
        // (`_1`..`_9`) are not reported as local variables...
        run_test(r#"def m; [1].map { it; local_variables }; end; m"#);
        run_test(r#"def m; [[1, 2]].map { _1 + _2; local_variables }; end; m"#);
        run_test(r#"def m; [1].map { it; binding.local_variables }; end; m"#);
        // ...but an explicit `it` local, or an explicit `|it|` block
        // parameter, still is.
        run_test(r#"def m; it = 5; local_variables; end; m"#);
        run_test(r#"def m; [1].map { |it| j = it; local_variables.sort }; end; m"#);
    }

    #[test]
    fn kernel_at_exit() {
        // `at_exit` returns the registered Proc.
        run_test(r#"at_exit { }.is_a?(Proc)"#);
        run_test(r#"at_exit { }.lambda?"#);
        // `at_exit` without a block raises ArgumentError.
        run_test_error(r#"at_exit"#);
    }

    #[test]
    fn dup_clone_copy_hooks() {
        // Default-hook classes copy correctly (the fast path that skips
        // the no-op `initialize_copy`/`dup`/`clone` dispatch).
        run_test(r#"s = +"abc"; t = s.dup; t << "d"; [s, t]"#);
        run_test(r#"["x".freeze.dup.frozen?, "x".freeze.clone.frozen?]"#);
        // A class with custom copy hooks still has them invoked.
        run_test(
            r#"class CH
                 attr_accessor :tag
                 def initialize_dup(o); super; @tag = :dup; end
                 def initialize_clone(o, **); super; @tag = :clone; end
               end
               c = CH.new
               [c.dup.tag, c.clone.tag]"#,
        );
        // A custom `initialize_copy` (reached via both dup and clone) runs.
        run_test(
            r#"class CC
                 attr_accessor :n
                 def initialize_copy(o); super; @n = (o.n || 0) + 1; end
               end
               c = CC.new; c.n = 10
               [c.dup.n, c.clone.n]"#,
        );
        // Array's builtin `initialize_copy` (alias of replace) keeps dup a
        // shallow, independent copy.
        run_test(r#"a = [1, 2, 3]; b = a.dup; b << 4; [a, b]"#);
    }
}
