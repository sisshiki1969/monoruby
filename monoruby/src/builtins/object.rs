use super::*;
use crate::ast::CmpKind;
#[cfg(target_arch = "x86_64")]
use jitgen::{BinaryInlineOutcome, JitContext};
#[cfg(target_arch = "aarch64")]
use jitgen::{AbstractState, BinaryInlineOutcome, JitContext};

//
// BasicObject class and Object class
//

/// The methods of this file that belong to `Kernel` rather than `Object`
/// (`Kernel.instance_method(:freeze)` finds them, and a module's method
/// search does not reach `Object`). Called from `kernel::init`, since
/// `Kernel` does not exist yet when [`init`] runs.
pub(super) fn init_kernel(globals: &mut Globals, kernel_class: ClassId) {
    // CRuby defines `===` on Kernel, NOT on BasicObject. Keeping it on
    // BasicObject would trap `===` for BasicObject subclasses (like
    // mspec's `SpecPositiveOperatorMatcher`) instead of routing through
    // `method_missing`, breaking the `actual.should === expected` idiom.
    globals.define_builtin_func(kernel_class, "===", case_eq, 1);
    // `nil`, `true` and `false` have a `===` of their own in CRuby — the
    // same `case_equal` (`rb_equal`: identity, then `==`), defined on
    // NilClass, TrueClass and FalseClass as well as on Kernel — so
    // `nil.method(:===).owner` is NilClass (#2017).
    for class_id in [NIL_CLASS, TRUE_CLASS, FALSE_CLASS] {
        globals.define_builtin_func(class_id, "===", case_eq, 1);
    }
    globals.define_builtin_func(kernel_class, "freeze", freeze, 0);
    globals.define_builtin_inline_func(
        kernel_class,
        "frozen?",
        frozen,
        inline_gen2!(object_frozen),
        0,
    );
}

pub(super) fn init(globals: &mut Globals) {
    // BasicObject methods

    // Bootstrap-only default constructor (overridden by startup.rb with the
    // Ruby `def initialize; end`, whose `ISeqHint::ConstReturn` lets the JIT
    // fold the call away). Needed before basic_object.rb loads.
    globals.define_private_builtin_func(BASIC_OBJECT_CLASS, "initialize", bo_initialize, 0);
    globals.define_builtin_inline_func(
        BASIC_OBJECT_CLASS,
        "__id__",
        object_id,
        inline_gen2!(object_object_id),
        0,
    );
    // `equal?` is an alias of `==` on BasicObject (they share one method
    // entry/FuncId), so `BasicObject.instance_method(:equal?) ==
    // BasicObject.instance_method(:==)` holds — see ruby/spec
    // core/basicobject/equal_spec.rb.
    globals.define_builtin_funcs(BASIC_OBJECT_CLASS, "==", &["equal?"], eq, 1);
    globals.define_builtin_inline_func(BASIC_OBJECT_CLASS, "!", not_, inline_gen2!(object_not), 0);
    let neq_fid = globals.define_builtin_inline_func(
        BASIC_OBJECT_CLASS,
        "!=",
        ne,
        inline_gen2!(object_ne),
        1,
    );
    globals.store.set_default_neq(neq_fid);
    globals.define_builtin_funcs_with_effect(
        BASIC_OBJECT_CLASS,
        "instance_eval",
        &[],
        instance_eval,
        0,
        0,
        true,
        Effect::EVAL,
    );
    let instance_exec_fid = globals.define_builtin_funcs_with_effect(
        BASIC_OBJECT_CLASS,
        "instance_exec",
        &[],
        instance_exec,
        0,
        crate::executor::frame::VARIADIC_CAP,
        true,
        Effect::EVAL,
    );
    globals.store[instance_exec_fid].set_native_variadic();

    let send_fid = globals.define_builtin_inline_funcs_with_kw(
        BASIC_OBJECT_CLASS,
        "__send__",
        &[],
        crate::builtins::send,
        inline_gen2!(crate::builtins::object_send),
        0,
        crate::executor::frame::VARIADIC_CAP,
        true,
        &[],
        true,
    );
    globals.store[send_fid].set_native_variadic();
    globals.store.record_object_send_fid(send_fid);
    globals.define_private_builtin_func_rest(
        BASIC_OBJECT_CLASS,
        "method_missing",
        bo_method_missing,
    );
    // hook methods (no-op by default, overridden by startup.rb)
    globals.define_private_builtin_func(
        BASIC_OBJECT_CLASS,
        "singleton_method_added",
        bo_noop_hook,
        1,
    );
    globals.define_private_builtin_func(
        BASIC_OBJECT_CLASS,
        "singleton_method_removed",
        bo_noop_hook,
        1,
    );
    globals.define_private_builtin_func(
        BASIC_OBJECT_CLASS,
        "singleton_method_undefined",
        bo_noop_hook,
        1,
    );
}

///
/// ### BasicObject#initialize
///
/// Bootstrap default; startup.rb replaces it with the Ruby no-op.
///
#[monoruby_builtin]
fn bo_initialize(_: &mut Executor, _: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::nil())
}

/// No-op hook for singleton_method_added/removed/undefined (overridden by startup.rb).
#[monoruby_builtin]
fn bo_noop_hook(_: &mut Executor, _: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::nil())
}

///
/// ### Object#freeze
///
/// - freeze -> self
///
/// Freezes the object, preventing further modification.
///
#[monoruby_builtin]
fn freeze(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_val = lfp.self_val();
    if !self_val.is_packed_value() {
        self_val.set_frozen();
        // Freezing an object also freezes its singleton class, if one has
        // already been created (CRuby "Frozen properties").
        if let Some(singleton) = globals.store.has_singleton(self_val) {
            singleton.as_val().set_frozen();
        }
    }
    Ok(self_val)
}

///
/// ### Object#frozen?
///
/// - frozen? -> bool
///
/// Returns true if the object is frozen.
///
#[monoruby_builtin]
fn frozen(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().is_frozen()))
}

/// Reads only the receiver Value, so it keeps firing where the class is
/// unproven: the predicate reads
/// the Value tag and the RValue header only — packed values are frozen,
/// heap Numerics are frozen, everything else tests the header FROZEN bit
/// (`emit_frozen_pred` mirrors `Value::is_frozen` exactly, chilled strings
/// included: their FROZEN bit is clear, so they report false).
pub(super) fn object_frozen(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    // The receiver Value is all this reads, so an unproven class
    // (a multi-class dispatch arm / the class-set guard) is no obstacle.
    _: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo { recv, dst, .. } = *callsite;
    state.load(ir, recv, GP::Rdi);
    ir.inline(move |r#gen, _, _, _| r#gen.emit_frozen_pred());
    state.def_rax2acc(ir, dst);
    true
}

///
/// ### BasicObject#method_missing
///
#[monoruby_builtin]
fn bo_method_missing(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    let name = args[0].expect_symbol_or_to_str(vm, globals)?;
    let recv = lfp.self_val();
    // The call style shapes the error, mirroring CRuby's default
    // method_missing: a failed "variable call" (bare identifier —
    // could as well have been a local variable) raises NameError, and
    // a failed `super` raises the super-flavored NoMethodError.
    use crate::executor::MethodMissingStyle;
    let style = vm.take_method_missing_style();
    // How the receiver is named — a class by its Ruby-level `#name` —
    // is settled when the message is read, as CRuby's
    // `name_err_mesg_to_str` does (`MonorubyErr::class_span`).
    match style {
        MethodMissingStyle::VCall => {
            // `undefined local variable or method 'name' for <recv>`,
            // carrying the receiver on `#receiver`.
            return Err(MonorubyErr::undefined_local_or_method(
                &globals.store,
                name,
                recv,
            ));
        }
        MethodMissingStyle::Super => {
            // A `super` is a function call (`VM_CALL_FCALL`).
            let err = MonorubyErr::no_method_error(
                &globals.store,
                "super: no superclass method '",
                name,
                recv,
                "' for ",
            );
            let call_args = Value::array_from_iter(args.iter().skip(1).cloned());
            return Err(no_method_error_with_args(globals, err, call_args, true));
        }
        MethodMissingStyle::Plain | MethodMissingStyle::FCall => {}
    }
    // Reaching the default `method_missing` while the method actually
    // exists means the call violated visibility (a private method called
    // with an explicit receiver, or an inaccessible protected method).
    // CRuby reports these as "private/protected method … called" rather
    // than "undefined method".
    use crate::executor::Visibility;
    let visibility = match globals.store.check_method_for_class(recv.class(), name) {
        Some(entry) if entry.func_id().is_some() => Some(entry.visibility()),
        _ => None,
    };
    match visibility {
        Some(Visibility::Private) => Err(MonorubyErr::no_method_error(
            &globals.store,
            "private method '",
            name,
            recv,
            "' called for ",
        )),
        Some(Visibility::Protected) => Err(MonorubyErr::no_method_error(
            &globals.store,
            "protected method '",
            name,
            recv,
            "' called for ",
        )),
        _ => {
            // A genuine NoMethodError carries the call arguments on
            // `#args` (`args[0]` is the method name; the rest are the
            // call arguments).
            let err = MonorubyErr::no_method_error(
                &globals.store,
                "undefined method '",
                name,
                recv,
                "' for ",
            );
            let call_args = Value::array_from_iter(args.iter().skip(1).cloned());
            // `private_call?`: the call was written as a function call
            // (CRuby's `last_call_status & MISSING_FCALL`).
            let private_call = style == MethodMissingStyle::FCall;
            Err(no_method_error_with_args(
                globals,
                err,
                call_args,
                private_call,
            ))
        }
    }
}

/// Re-materialize a NoMethodError so it also carries `#args`. The
/// `NotMethod` error's name / receiver are transferred onto a
/// pre-built exception object (via `take_ex_obj`'s standard path) and
/// the arguments array is attached as the hidden `/args` ivar, and
/// `private_call` — whether the call was written as a function call —
/// as `/private_call` (`NoMethodError#private_call?`).
fn no_method_error_with_args(
    globals: &mut Globals,
    err: MonorubyErr,
    call_args: Value,
    private_call: bool,
) -> MonorubyErr {
    let (name, receiver) = match err.kind() {
        MonorubyErrKind::NotMethod { name, receiver } => (*name, *receiver),
        _ => (None, None),
    };
    let mut exc = Value::new_exception_from(err.message().to_string(), NO_METHOD_ERROR_CLASS);
    // The pre-built object shows the name where the error did.
    exc.is_exception_mut().unwrap().name_span = err.name_span;
    exc.is_exception_mut().unwrap().class_span = err.class_span;
    if let Some(name) = name {
        let _ = globals
            .store
            .set_ivar(exc, IdentId::_NAME, Value::symbol(name));
    }
    if let Some(receiver) = receiver {
        let _ = globals
            .store
            .set_ivar(exc, IdentId::_RECEIVER, receiver);
    }
    let _ = globals
        .store
        .set_ivar(exc, IdentId::get_id("/args"), call_args);
    if private_call {
        let _ = globals.store.set_ivar(
            exc,
            IdentId::get_id("/private_call"),
            Value::bool(true),
        );
    }
    err.with_original(exc)
}

/// The NoMethodError of a call by a name no Symbol exists for
/// (`rb_make_no_method_exception` from `send`): `#name` is the String
/// the caller gave, as it is in the message.
pub(crate) fn no_method_error_unknown(
    globals: &mut Globals,
    receiver: Value,
    name: Value,
    args: &[Value],
    private_call: bool,
) -> MonorubyErr {
    let inner = name.as_rstring_inner();
    let (bytes, enc) = (inner.as_bytes().to_vec(), inner.encoding());
    let (desc, class_len) = MonorubyErr::receiver_description(&globals.store, receiver);
    let raw = [
        b"undefined method '".as_slice(),
        &bytes,
        b"' for ",
        desc.as_bytes(),
    ]
    .concat();
    let mut err = MonorubyErr::with_message(
        MonorubyErrKind::NotMethod {
            name: None,
            receiver: Some(receiver),
        },
        String::from_utf8_lossy(&raw).into_owned(),
    )
    .showing_class(class_len, 0);
    // A name that is not UTF-8 is shown as its own bytes.
    if std::str::from_utf8(&raw).is_err() {
        err.raw_message = Some((raw, enc));
    }
    let call_args = Value::array_from_iter(args.iter().cloned());
    let err = no_method_error_with_args(globals, err, call_args, private_call);
    if let Some(mut exc) = err.original {
        let _ = globals.store.set_ivar(exc, IdentId::_NAME, name);
        if let Some(inner) = exc.is_exception_mut() {
            inner.raw_message = err.raw_message.clone();
        }
    }
    err
}

///
/// ### BasicObject#!
///
/// - !obj -> bool
///
/// Returns the boolean negation of the object.
///
#[monoruby_builtin]
fn not_(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(!lfp.self_val().as_bool()))
}

fn object_not(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo { recv, dst, .. } = *callsite;
    if state.is_truthy(recv) {
        if let Some(dst) = dst {
            state.def_C(dst, Immediate::bool(false));
        }
    } else if state.is_falsy(recv) {
        if let Some(dst) = dst {
            state.def_C(dst, Immediate::bool(true));
        }
    } else {
        state.load(ir, recv, GP::Rdi);
        // Pure-LIR predicate (no arch-specific closure): Rax = !Rdi.
        ir.not_to_bool(GP::Rax, GP::Rdi);
        state.def_rax2acc(ir, dst);
    }
    true
}

#[monoruby_builtin]
fn eq(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().id() == lfp.arg(0).id()))
}

///
/// ### BasicObject#!=
///
/// - self != other -> bool
///
/// The only `!=` there is, as in CRuby: `rb_obj_not_equal`, `!(self ==
/// other)`. It is C there and calls `==` with `rb_funcall`, so a private
/// `==` is callable and a refinement of `==` is not seen
/// (`Executor::eq_values_funcall`).
#[monoruby_builtin]
fn ne(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let other = lfp.arg(0);
    // Two Strings, the pair the JIT sends here most (it inlines the
    // immediates, and a String against a constant): `String#==`'s own
    // answer for them, without the generic unpacking.
    if let Some(lhs) = self_val.is_plain_rstring_inner()
        && let Some(rhs) = other.is_rstring_inner()
        && !globals
            .store
            .basic_op_globally_redefined_for(STRING_CLASS, IdentId::_EQ)
    {
        return Ok(Value::bool(*lhs != *rhs));
    }
    // The optimized `==` dispatch (builtin fast paths first, a memoized
    // lookup otherwise) — the same policy `ne_values_vis` applies when
    // `!=` resolves to this default — with a native `==` run in this very
    // frame.
    Ok(Value::bool(vm.default_ne_in_frame(globals, lfp, pc)?))
}

///
/// JIT-inline the default `!=` where the `==` it negates is a builtin
/// with an inline form of its own, so that `a != b` is neither a call of
/// `!=` nor one of `==` and feeds the branch of an `if` directly:
///
/// - `BasicObject#==` (nil, true, false, any object whose class defines
///   no `==`) and `Symbol#==` compare the two words;
/// - `Integer#==` and `Float#==` are the numeric compare
///   (`integer_cmp_inline` / `float_cmp_inline`);
/// - `String#==` against a constant (`string_ne_gen`).
///
/// No class has a `!=` of its own, as in CRuby (#1938, #1964), so this is
/// the generator behind every `!=` the JIT inlines. The `==` asked about
/// is the one `rb_funcall` finds — no refinement (#1967). What the code
/// assumes beyond the call site's own resolution of `!=` is that
/// resolution of `==`, recorded for the class-version salvage: a `==`
/// defined on the receiver's class later recompiles the site, which then
/// declines here and calls the builtin. (The guard-free firing of an
/// `Integer` / `Float` / `String` site stands on the `(class, "!=")`
/// basic-op pair instead, which a redefinition of the class's `==` takes
/// down with it — `Store::set_bop_redefine`.)
///
fn object_ne(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    ctx: &JitContext,
    store: &Store,
    callid: CallSiteId,
    recv_class: Option<CachedClass>,
    arg_class: Option<CachedClass>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() || callsite.pos_num != 1 {
        return false;
    }
    // The receiver's class has to be proven (behind a class-set guard it is
    // one of several, left to the call).
    let Some((recv_class, eq_fid)) =
        recv_class.and_then(|class| Some((class, ctx.funcall_eq(class)?)))
    else {
        return false;
    };
    let builtin_eq =
        |class: ClassId| store.basic_op_armed_func(class, IdentId::_EQ) == Some(eq_fid);
    let fired = if store.is_identity_eq(eq_fid) {
        let CallSiteInfo {
            dst, recv, args, ..
        } = *callsite;
        if let (Some(lhs), Some(rhs)) =
            (state.is_symbol_literal(recv), state.is_symbol_literal(args))
        {
            if let Some(dst) = dst {
                state.def_C(dst, Immediate::bool(lhs != rhs));
            }
        } else if let Some(dst) = ctx.wants_cond_flags(dst) {
            let flags = state.gen_identity_cmp_flags(ir, CmpKind::Ne, dst, recv, args);
            ctx.set_cond_flags(dst, flags);
        } else {
            state.gen_identity_cmp(ir, CmpKind::Ne, dst, recv, args);
        }
        true
    } else if recv_class == CachedClass::INTEGER && builtin_eq(INTEGER_CLASS) {
        let outcome =
            numeric::integer_cmp_inline(CmpKind::Ne, state, ir, ctx, store, callid, arg_class);
        matches!(outcome, BinaryInlineOutcome::Done)
    } else if recv_class == CachedClass::FLOAT && builtin_eq(FLOAT_CLASS) {
        let outcome =
            numeric::float_cmp_inline(CmpKind::Ne, state, ir, ctx, store, callid, arg_class);
        matches!(outcome, BinaryInlineOutcome::Done)
    } else if recv_class == CachedClass::STRING && builtin_eq(STRING_CLASS) {
        string::string_ne_gen(state, ir, ctx, store, callid, Some(recv_class), arg_class)
    } else {
        false
    };
    if fired {
        ctx.assume_funcall_eq(recv_class, eq_fid);
    }
    fired
}

/// Default `Object#===` — identical objects compare true without consulting
/// `==` (CRuby's `rb_equal` fast path, observable when `==`/`equal?` are
/// overridden to lie), otherwise delegates to `==` so that subclasses which
/// override `==` get consistent `case` behaviour.
#[monoruby_builtin]
fn case_eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let other = lfp.arg(0);
    if self_val.id() == other.id() {
        return Ok(Value::bool(true));
    }
    vm.eq_values_funcall(globals, self_val, other)
}

///
/// ### BasicObject#__id__
///
/// - object_id -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/object_id.html]
#[monoruby_builtin]
pub(super) fn object_id(
    _: &mut Executor,
    _: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::integer(lfp.self_val().id() as i64))
}

/// Reads only the receiver Value, so it keeps firing where the class is
/// unproven: `Value::id()` works
/// on the raw Value bits, correct for any receiver.
pub(super) fn object_object_id(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    // The receiver Value is all this reads, so an unproven class
    // (a multi-class dispatch arm / the class-set guard) is no obstacle.
    _: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo { recv, dst: ret, .. } = *callsite;
    state.load(ir, recv, GP::Rdi);
    let using_fpr = state.get_using_fpr(ir);
    ir.fpr_save(using_fpr);
    ir.inline(move |r#gen, _, _, _| r#gen.emit_object_id());
    ir.fpr_restore(using_fpr);
    state.def_rax2acc(ir, ret);
    true
}

///
/// Object.new
///
/// - new -> Object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/s/new.html]
#[monoruby_builtin]
fn object_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let obj = Value::object(class);
    vm.invoke_method_inner(
        globals,
        IdentId::INITIALIZE,
        obj,
        &lfp.arg(0).as_array(),
        lfp.block(),
        None,
    )?;
    Ok(obj)
}

///
/// ### BasicObject#instance_exec
///
/// - instance_exec(*args) {|*args| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/BasicObject/i/instance_exec.html]
#[monoruby_builtin]
fn instance_exec(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let bh = lfp.expect_block()?;
    let data = vm.get_block_data(globals, bh)?;
    let args = lfp.variadic_args();
    vm.push_instance_eval_context(self_val);
    let res = vm.invoke_block_with_self(globals, &data, self_val, &args);
    vm.pop_class_context();
    res
}

/// ### BasicObject#instance_eval
///
/// - instance_eval(expr, filename = "(eval)", lineno = 1) -> object
/// - instance_eval {|obj| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/BasicObject/i/instance_eval.html]
#[monoruby_builtin]
fn instance_eval(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    vm.push_instance_eval_context(self_val);
    let res = instance_eval_inner(vm, globals, lfp, self_val, pc);
    vm.pop_class_context();
    res
}

fn instance_eval_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    self_val: Value,
    pc: BytecodePtr,
) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    let argc = args.len();
    if let Some(bh) = lfp.block() {
        if argc > 0 {
            return Err(MonorubyErr::wrong_number_of_arg(0, argc));
        }
        let data = vm.get_block_data(globals, bh)?;
        vm.invoke_block_with_self(globals, &data, self_val, &[self_val])
    } else if argc >= 1 && argc <= 3 {
        let expr = crate::builtins::eval_source_bytes(vm, globals, args[0])?;
        let cfp = vm.cfp();
        let caller_cfp = cfp.prev().unwrap();
        // The eval body is anchored to the Ruby scope that *wrote* the
        // call — its locals and its lexical nesting — which is not the
        // immediate caller when we were reached through a builtin
        // (`send`, a Rust helper, mspec, …). The location below still
        // comes from the immediate caller, whose pc we were handed.
        let outer_cfp = caller_cfp.nearest_ruby_frame();
        let path = if argc >= 2 {
            args[1].coerce_to_str(vm, globals)?
        } else {
            let caller_loc = globals.store.get_caller_loc(caller_cfp, Some(pc));
            format!("(eval at {})", caller_loc)
        };
        let lineno: i64 = if argc >= 3 {
            args[2].coerce_to_int_i64(vm, globals)?
        } else {
            1
        };
        let src_encoding = crate::builtins::eval_src_encoding(args[0]);
        // CRuby's instance_eval cref holds the receiver and a
        // "singleton" flag: constants are read through `CLASS_OF(self)`
        // and the singleton class is made only when something is
        // defined in the body (`CREF_CLASS_FOR_DEFINITION`) — so an
        // eval that defines nothing leaves the receiver without one,
        // and a receiver that cannot have one (an Integer, a Symbol, an
        // fstring) is evaluated all the same. The body's innermost
        // lexical class starts as the receiver's class as it is and
        // follows it from there; see `Executor::instance_eval_frame`.
        let receiver_class = self_val.class();
        let fid = globals.compile_script_eval(
            expr,
            path,
            outer_cfp,
            Some(receiver_class),
            lineno,
            src_encoding,
        )?;
        // Constant lookup inside the body checks the receiver's class
        // between the singleton class and the caller's lexical scopes
        // (CRuby); see `ISeqInfo::instance_eval_class`.
        let real_class = self_val.real_class(&globals.store).id();
        if let Some(info) = globals.store.iseq_mut(fid) {
            info.instance_eval_class = Some(real_class);
        }
        vm.flush_compile_warnings(globals);
        let proc = ProcData::new(outer_cfp.lfp(), fid);
        vm.invoke_block_with_self(globals, &proc, self_val, &[])
    } else {
        Err(MonorubyErr::wrong_number_of_arg_range(argc, 1..=3))
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn cmp_dispatch_preserves_return_value() {
        // A custom `==` / `!=` may return an arbitrary value; the binop
        // must evaluate to it untouched (CRuby semantics). Reverse
        // coercion dispatches (`1 == obj`) stay boolean-coerced, and the
        // default `!=` is the boolean negation of `==`.
        run_test_once(
            r##"
        class CustomEq; def ==(o); "EQ-RAW"; end; end
        class CustomNe; def ==(o); true; end; def !=(o); "NE-RAW"; end; end
        e = CustomEq.new
        n = CustomNe.new
        res = [e == 1, e != 1, n != 1, n == 1, 1 == e, "a" == e]
        30.times { res << (e == 1) << (n != 1) }
        res
        "##,
        );
    }

    #[test]
    fn cmp_dispatch_raw_in_bop_redefined_mode() {
        // Redefining a basic op (Integer#!=) makes the VM's `!=` handler
        // take its generic path; that path must still dispatch ==/!= raw
        // (and honor the redefined basic op itself).
        run_test_once(
            r##"
        class Integer; def !=(o); "INT-NE"; end; end
        class CustomEqB; def ==(o); "RAW-B"; end; end
        e = CustomEqB.new
        [1 != 2, e == 1, e != 1, "a" == "a", "a" != "b", nil == nil, nil != 1]
        "##,
        );
    }

    /// The default `!=` on a receiver whose `==` is the identity test (nil,
    /// booleans, an object of a class with no `==`) is a compare of the two
    /// words in JIT code; a class with a `==` of its own keeps the call.
    #[test]
    fn default_neq_jit_inline() {
        run_test(
            r##"
        class NeqPlain; end
        class NeqPlain2; end
        class NeqOwn; def ==(o) = o == 1; end
        def one(x, y) = [x != y, x.!=(y)]
        def nne(x, y) = x != y
        def tne(x, y) = x != y
        def own(x, y) = x != y
        def cls(x, y) = x != y
        def set(x, y) = x.!=(y)
        def any(x, y) = x.!=(y)
        def mixed(x, y) = x.!=(y)
        def splat(x, y) = x.!=(*y)
        def br(x, y)
          if x != y then 1 else 2 end
        end
        def nbr(x, y)
          if x != y then 1 else 2 end
        end
        a = NeqPlain.new
        b = NeqPlain.new
        c = NeqOwn.new
        d = NeqPlain2.new
        others = [a, b, c, d, nil, false, true, 0, 1, 1.5, :a, "a"]
        idents = [nil, true, :a, a, d, Object.new, NeqPlain, Comparable]
        first = nil
        unstable = 0
        30.times do
          res = []
          others.each do |y|
            idents.each { |x| res << any(x, y) << any(x, x) }
            (idents + [[], c]).each { |x| res << mixed(x, y) << mixed(x, x) }
            res << one(a, y) << br(b, y) << nne(nil, y) << nbr(nil, y) << own(c, y)
            res << tne(true, y) << tne(false, y) << splat(a, [y])
            res << cls(NeqPlain, y) << cls(Comparable, y)
            [a, d, b, d].each { |x| res << set(x, y) }
          end
          first ||= res
          unstable += 1 unless res == first
        end
        [first, unstable]
        "##,
        );
    }

    /// What the inlined default `!=` assumes is the resolution of `==` on
    /// the receiver's class: a `==` defined on it afterwards — in the class,
    /// in a module it then includes, or back out again — is seen by the
    /// compiled site, and so is a `!=` given to `BasicObject`.
    #[test]
    fn default_neq_sees_later_eq() {
        run_test_once(
            r##"
        class NeqLate; end
        class NeqMixed; end
        module NeqMod; def ==(o) = true; end
        def br(x, y)
          if x != y then 1 else 2 end
        end
        def nbr(x, y)
          if x != y then 1 else 2 end
        end
        def one(x, y) = [x != y, br(x, y)]
        def mix(x, y) = [x != y, x.!=(y)]
        def nne(x, y) = [x != y, nbr(x, y)]
        def any(x, y) = x.!=(y)
        a = NeqLate.new
        b = NeqLate.new
        m = NeqMixed.new
        idents = [nil, true, :a, a, m, Object.new, NeqLate, Comparable]
        res = []
        30.times { res << one(a, a) + one(a, b) + mix(m, m) + mix(m, 1) + nne(nil, nil) + nne(nil, 1) }
        30.times { res << idents.map { |x| [any(x, x), any(x, b)] } }
        class NeqLate; def ==(o) = true; end
        res << one(a, a) + one(a, b) << idents.map { |x| [any(x, x), any(x, b)] }
        class NeqLate; remove_method :==; end
        30.times { res << one(a, a) + one(a, b) }
        class NeqMixed; include NeqMod; end
        res << mix(m, m) + mix(m, 1)
        class NilClass; def ==(o) = true; end
        res << nne(nil, nil) + nne(nil, 1)
        class NilClass; remove_method :==; end
        30.times { res << nne(nil, nil) + nne(nil, 1) }
        class BasicObject
          alias_method :__default_neq, :!=
          def !=(o) = :basic
        end
        r = one(a, b) + nne(nil, 1) + [:a != :a]
        class BasicObject
          alias_method :!=, :__default_neq
          remove_method :__default_neq
        end
        res << r << one(a, b) + nne(nil, 1)
        res.chunk_while { |x, y| x == y }.map(&:first)
        "##,
        );
    }

    /// A megamorphic `!=` site answers two immediates by their bits in JIT
    /// code. That stands for the default `!=` only: one defined on Symbol,
    /// NilClass or TrueClass after the site was compiled is called.
    #[test]
    fn neq_custom_on_immediates_at_megamorphic_site() {
        for klass in ["Symbol", "NilClass", "TrueClass", "FalseClass"] {
            run_test_once(&format!(
                r##"
            def any_ne(a, b) = a != b
            def any_br(a, b)
              if a != b then 1 else 2 end
            end
            vals = [nil, :a, :b, true, false, 1, 2.5, "a", [1], {{}}, 1..2, Object.new, 1r]
            res = []
            10.times {{ res << vals.map {{ |y| vals.map {{ |x| [any_ne(x, y), any_br(x, y)] }} }} }}
            class {klass}; def !=(o) = :custom; end
            imm = [nil, :a, true, false, 1]
            res << imm.map {{ |x| [any_ne(x, x), any_ne(x, 2), any_br(x, x)] }}
            class {klass}; remove_method :!=; end
            10.times {{ res << imm.map {{ |x| [any_ne(x, x), any_ne(x, 2), any_br(x, x)] }} }}
            res.chunk_while {{ |x, y| x == y }}.map(&:first)
            "##
            ));
        }
    }

    /// No class has a `!=` of its own, as in CRuby (#1964): for every
    /// receiver it is `BasicObject#!=`.
    #[test]
    fn no_class_has_a_neq_of_its_own() {
        run_test_once(
            r##"
        vals = [1, 2**70, 1.5, "a", 1r, 1i, :a, nil, true, [1], {},
                Struct.new(:a).new(1), Data.define(:a).new(a: 1)]
        classes = [Integer, Float, String, Rational, Complex, Struct, Data,
                   Numeric, Comparable, Kernel, Object]
        [
          vals.map { |x| x.method(:!=).owner.to_s },
          classes.map { |c| c.instance_methods(false).include?(:!=) || c.private_instance_methods(false).include?(:!=) },
          BasicObject.instance_methods(false).include?(:!=),
        ]
        "##,
        );
    }

    /// `!=` on the classes that used to have one of their own: the same
    /// answers from `BasicObject#!=`, whose inline forms for an Integer,
    /// a Float and a String receiver are the ones those builtins had.
    #[test]
    fn neq_on_builtin_classes() {
        run_test_once(
            r##"
        st = Struct.new(:a, :b)
        dt = Data.define(:a)
        def ine(x, y) = x != y
        def bne(x, y) = x != y
        def fne(x, y) = x != y
        def sne(x, y) = x != y
        def rne(x, y) = x != y
        def cne(x, y) = x != y
        def tne(x, y) = x != y
        def dne(x, y) = x != y
        def ibr(x, y)
          if x != y then 1 else 2 end
        end
        def fbr(x, y)
          if x != y then 1 else 2 end
        end
        def sbr(x, y)
          if x != y then 1 else 2 end
        end
        def lit(x) = [x != 1, x != 1.0, x != "a", 1 != x, 1.5 != x, "a" != x, x != nil]
        rec = st.new(1, nil); rec.b = rec
        rec2 = st.new(1, nil); rec2.b = rec2
        others = [0, 1, 2**70, 1.0, 1.5, Float::NAN, "a", "b", "a".encode("UTF-16LE"), "a".b,
                  1r, 3r/2, 1i, Complex(1, 0), Complex(1.5, 0), :a, nil, true,
                  st.new(1, 2), st.new(1.0, 2), dt.new(a: 1), dt.new(a: 1.0), rec, [1], Object.new]
        first = nil
        unstable = 0
        30.times do
          r = []
          others.each do |y|
            r << ine(1, y) << ibr(1, y) << bne(2**70, y) << fne(1.5, y) << fbr(1.0, y) << fne(Float::NAN, y)
            r << sne("a", y) << sbr("a", y) << rne(1r, y) << rne(3r/2, y) << cne(Complex(1, 0), y) << cne(1i, y)
            r << tne(st.new(1, 2), y) << tne(rec2, y) << dne(dt.new(a: 1), y) << lit(y)
          end
          first ||= r
          unstable += 1 unless r == first
        end
        [first, unstable]
        "##,
        );
    }

    /// `!=` is `!(a == b)` for every class (#1964): a `==` redefined on
    /// Integer, Float, String, Rational, Complex, Struct or Data is seen
    /// by the sites compiled before it — the VM's fixnum compare and the
    /// JIT's inline forms included — and so is a `!=` given to the class,
    /// until it is removed again.
    #[test]
    fn neq_sees_redefined_eq_on_builtin_classes() {
        for (klass, prelude, a, b) in [
            ("Integer", "", "3", "4"),
            ("Float", "", "1.5", "2.5"),
            ("String", "", r#""a""#, r#""b""#),
            ("Rational", "", "1r", "2r"),
            ("Complex", "", "1i", "2i"),
            ("Struct", "st = Struct.new(:x)", "st.new(1)", "st.new(2)"),
            (
                "Data",
                "dt = Data.define(:x)",
                "dt.new(x: 1)",
                "dt.new(x: 2)",
            ),
        ] {
            run_test_once(&format!(
                r##"
            {prelude}
            def ne(x, y) = x != y
            def br(x, y)
              if x != y then 1 else 2 end
            end
            def any(x, y) = x != y
            def all(a, b) = [ne(a, a), ne(a, b), br(a, a), br(a, b), any(a, a), any(a, b), a != a, a != b]
            vals = [nil, :a, true, 1, 2.5, "a", [1], Object.new, 1r, 1i]
            a = {a}
            b = {b}
            res = []
            30.times {{ res << all(a, b); vals.each {{ |v| any(v, a); any(a, v) }} }}
            class {klass}
              alias_method :__eq, :==
              def ==(o) = true
            end
            10.times {{ res << all(a, b) }}
            class {klass}
              alias_method :==, :__eq
              remove_method :__eq
            end
            30.times {{ res << all(a, b) }}
            class {klass}; def !=(o) = :custom; end
            10.times {{ res << all(a, b) }}
            class {klass}; remove_method :!=; end
            30.times {{ res << all(a, b) }}
            res.chunk_while {{ |x, y| x == y }}.map(&:first)
            "##
            ));
        }
    }

    /// CRuby has no `!=` basic operation: defining `Integer#!=` (or a
    /// `!=` for everything) says nothing under `Warning[:performance]`,
    /// and redefining `Integer#==` — which takes the inline `!=` with
    /// it — warns about `==` only.
    #[test]
    fn neq_redefinition_does_not_warn() {
        run_test_once(
            r##"
        $neq_warnings = []
        module Warning
          def self.warn(msg, category: nil) = $neq_warnings << msg[/Redefining.*/]
        end
        Warning[:performance] = true
        class Integer
          def !=(o) = super
        end
        class Float
          alias_method :__eq, :==
          def ==(o) = __eq(o)
        end
        class BasicObject
          alias_method :__ne, :!=
          def !=(o) = __ne(o)
        end
        [$neq_warnings, 1 != 2, 1.5 != 1.5, "a" != "b"]
        "##,
        );
    }

    /// The default `!=` is C in CRuby and calls `==` with `rb_funcall`
    /// (#1967): a refinement of `==` active where the `!=` is written is
    /// not what it negates, a private or protected `==` is callable, and
    /// a class with no `==` at all raises.
    #[test]
    fn default_neq_calls_eq_without_refinements() {
        run_test_once(
            r##"
        class NeqRef; end
        module NeqRefineEq
          refine NeqRef do
            def ==(o) = true
          end
          [Symbol, Integer, Float, String, NilClass].each do |c|
            refine c do
              def ==(o) = false
            end
          end
        end
        using NeqRefineEq
        def ne_o(x, y) = [x == y, x != y]
        def ne_s(x, y) = [x == y, x != y]
        def ne_i(x, y) = [x == y, x != y]
        def ne_f(x, y) = [x == y, x != y]
        def ne_str(x, y) = [x == y, x != y]
        def ne_nil(x, y) = [x == y, x != y]
        def br_o(x, y)
          if x != y then 1 else 2 end
        end
        def br_s(x, y)
          if x != y then 1 else 2 end
        end
        def br_i(x, y)
          if x != y then 1 else 2 end
        end
        def any(x, y) = x != y
        def any_br(x, y)
          if x != y then 1 else 2 end
        end
        a = NeqRef.new
        b = NeqRef.new
        pairs = [[a, a], [a, b], [:a, :a], [:a, :b], [1, 1], [1, 2], [1.5, 1.5], [1.5, 2.5],
                 ["a", "a"], ["a", "b"], [nil, nil], [nil, 1], [2**70, 2**70], [[1], [1]]]
        res = []
        30.times do
          res << [
            ne_o(a, a) + ne_o(a, b) + ne_s(:a, :a) + ne_s(:a, :b) + ne_i(1, 1) + ne_i(1, 2),
            ne_f(1.5, 1.5) + ne_f(1.5, 2.5) + ne_str("a", "a") + ne_str("a", "b") + ne_nil(nil, nil) + ne_nil(nil, 1),
            [br_o(a, a), br_o(a, b), br_s(:a, :a), br_s(:a, :b), br_i(1, 1), br_i(1, 2)],
            pairs.map { |x, y| [any(x, y), any_br(x, y), x.send(:!=, y), x.method(:!=).call(y)] },
          ]
        end
        res.chunk_while { |x, y| x == y }.map(&:first)
        "##,
        );
        run_test_once(
            r##"
        class NeqPriv
          def ==(o) = true
          private :==
        end
        class NeqProt
          def ==(o) = :yes
          protected :==
        end
        class NeqNone
          undef_method :==
        end
        def ne(x, y) = x != y
        def nne(x, y) = x != y
        def any(x, y) = x != y
        pr = NeqPriv.new
        pt = NeqProt.new
        none = NeqNone.new
        res = []
        30.times do
          res << [
            ne(pr, 1), any(pr, 1), any(pt, 1), any(nil, 1), any(:a, 1), any(1, 1),
            ((pr == 1) rescue $!.class), ((any(none, 1)) rescue $!.class), ((none != 1) rescue $!.class),
            ((nne(none, 1)) rescue $!.class),
          ]
        end
        res.chunk_while { |x, y| x == y }.map(&:first)
        "##,
        );
    }

    /// The default `!=` runs a native `==` — Struct's, Array's, Hash's,
    /// Range's, Rational's, Complex's — in its own frame instead of
    /// calling it through a second one. Same answers, an error raised
    /// under that `==` comes out as it is, and its backtrace has the two
    /// frames CRuby's has.
    #[test]
    fn default_neq_over_a_native_eq() {
        run_test_once(
            r##"
        s = Struct.new(:a)
        d = Data.define(:a)
        class NeqElem
          def initialize(x) = @x = x
          attr_reader :x
          def ==(o)
            raise ArgumentError, "boom" if @x == 0
            @x == o.x
          end
        end
        b = NeqElem
        def nne(a, b) = a != b
        res = []
        40.times { |i| res << nne(s.new(b.new(1)), s.new(b.new(i % 3))) }
        40.times { |i| res << nne([b.new(1)], [b.new(i % 3)]) }
        40.times { |i| res << nne({ a: b.new(1) }, { a: b.new(i % 3) }) }
        40.times { |i| res << nne(d.new(b.new(1)), d.new(b.new(i % 3))) }
        40.times { |i| res << nne(1..2, 1..(i % 3)) }
        40.times { |i| res << nne(1r / 3, Rational(i % 3, 3)) }
        40.times { |i| res << nne(Complex(1, 2), Complex(i % 3, 2)) }
        res = res.chunk_while { |x, y| x == y }.map(&:first)
        mk = [->(x) { s.new(x) }, ->(x) { [x] }, ->(x) { { a: x } }, ->(x) { d.new(x) }]
        mk.each do |f|
          res << begin
            nne(f.(b.new(0)), f.(b.new(0)))
          rescue => e
            [e.class, e.message]
          end
        end
        # A site of one receiver class calls `BasicObject#!=` itself: the
        # error is traced through `==` and `!=`, and the frame is `!=`'s
        # own again afterwards.
        def nne_s(a, b) = a != b
        def nne_a(a, b) = a != b
        [[:nne_s, mk[0]], [:nne_a, mk[1]]].each do |m, f|
          40.times { |i| res << send(m, f.(b.new(1)), f.(b.new(i % 3))) }
          res << begin
            send(m, f.(b.new(0)), f.(b.new(0)))
          rescue => e
            [e.class, e.message, e.backtrace.first(4).map { |l| l[/'(.*)'/, 1] }]
          end
          40.times { |i| res << send(m, f.(b.new(1)), f.(b.new(i % 3))) }
        end
        res
        "##,
        );
    }

    /// A refinement can define `!=` (#1969): the scope that activated it
    /// calls that one — at a site of one class and at a megamorphic one —
    /// and no other scope does.
    #[test]
    fn neq_defined_by_a_refinement() {
        run_test_once(
            r##"
        class NeqRefd; end
        module NeqRefineNe
          refine NeqRefd do
            def !=(o) = :obj
          end
          refine Integer do
            def !=(o) = :int
          end
          refine Symbol do
            def !=(o) = :sym
          end
          refine String do
            def !=(o) = :str
          end
        end
        def out_o(x, y) = x != y
        def out_i(x, y) = x != y
        def out_any(x, y) = x != y
        a = NeqRefd.new
        vals = [a, 1, :a, "a", nil, 1.5, [1], true]
        res = []
        30.times { res << [[out_o(a, a), out_i(1, 1)], vals.map { |x| out_any(x, x) }] }
        using NeqRefineNe
        def in_o(x, y) = x != y
        def in_i(x, y) = x != y
        def in_s(x, y) = x != y
        def in_str(x, y) = x != y
        def in_any(x, y) = x != y
        def in_br(x, y)
          if x != y then 1 else 2 end
        end
        30.times do
          res << [
            [in_o(a, a), in_i(1, 1), in_s(:a, :a), in_str("a", "a")],
            vals.map { |x| [in_any(x, x), in_br(x, x), in_any(x, 0)] },
          ]
        end
        # Not in the loop above: a callee inlined into a scope with
        # refinements is compiled under that scope's set (#1973).
        res << [[out_o(a, a), out_i(1, 1)], vals.map { |x| out_any(x, x) }]
        res.chunk_while { |x, y| x == y }.map(&:first)
        "##,
        );
    }

    #[test]
    fn neq_custom_redefinition() {
        // Exercises the version-stamped `custom_neq` memo: `!=` on a class
        // computes `!(a == b)` while it resolves to the default, and a
        // later `!=` redefinition must be picked up.
        run_test_once(
            r##"
        res = []
        class FooNeq
          def ==(o); true; end
        end
        f = FooNeq.new
        30.times { res << (f != 1) << (f != nil) }
        class FooNeq
          def !=(o); o == 99; end
        end
        res << (f != 99) << (f != 1)
        res
        "##,
        );
    }

    #[test]
    fn cmp() {
        let v = &["true", "false", "nil", "10", ":true"];
        run_binop_tests2(v, &["==", "!="], v);
    }

    #[test]
    fn numeric_frozen() {
        run_tests(&[
            r##"[(2**70).frozen?, (1+2i).frozen?, Rational(1,3).frozen?, 0.1.frozen?, 5.frozen?]"##,
            r##"x = 2**80; x.freeze; x.frozen?"##,
        ]);
    }

    #[test]
    fn object_spaceship() {
        run_test(
            r##"
        o = Object.new
        [o <=> o, (o <=> Object.new).inspect, (o <=> 3.14).inspect]
        "##,
        );
        run_test_once(
            r##"
        m = Object.new
        def m.==(x); true; end
        m <=> Object.new
        "##,
        );
    }

    #[test]
    fn equal() {
        run_tests(&[
            r##"100.equal?(100)"##,
            r##"100.equal?(100.0)"##,
            r##""a".equal?("a")"##,
            r##"
        a = "a"
        b = a
        a.equal?(b)"##,
        ]);
    }

    #[test]
    fn basicobject_equal_is_alias_of_eq() {
        run_tests(&[
            // ruby/spec core/basicobject/equal_spec.rb: `equal?` must be the same
            // method entry as `==` on BasicObject.
            "BasicObject.instance_method(:equal?) == BasicObject.instance_method(:==)",
            "BasicObject.public_instance_methods(false).include?(:equal?)",
        ]);
    }

    #[test]
    fn case_eq() {
        run_test(
            r#"
            class Foo
              def ==(other)
                other == 42
              end
            end
            Foo.new === 42
            "#,
        );
        run_test("Object.new === Object.new");
    }

    #[test]
    fn instance_eval() {
        run_test_with_prelude(
            r#"
        some = Foo.new 'XXX'
        res = []
        res << some.instance_eval { @key} #=> "XXX"
        res << some.instance_eval { do_fuga } #=> "secret" # private メソッドも呼び出せる
        res
        "#,
            r#"
        class Foo
          def initialize data
            @key = data
          end
          private
          def do_fuga
            'secret'
          end
        end
        "#,
        );
        // constant lookup in instance_eval with string
        run_test_with_prelude(
            r#"
        some = Foo.new
        some.instance_eval("BAR")
        "#,
            r#"
        class Foo
          BAR = 42
        end
        "#,
        );
        // constant lookup in instance_eval with string (caller's lexical scope)
        run_test_with_prelude(
            r#"
        A.new.test
        "#,
            r#"
        class A
          X = 100
          def test
            instance_eval("X")
          end
        end
        "#,
        );
    }

    #[test]
    fn instance_eval_lineno() {
        // instance_eval with filename and lineno
        run_tests(&[
            r#"instance_eval("__LINE__", "test.rb", 42)"#,
            r#"instance_eval("__FILE__", "test.rb", 42)"#,
            r#"instance_eval("__LINE__\n__LINE__", "test.rb", 10)"#,
        ]);
    }

    #[test]
    fn instance_exec() {
        run_test_with_prelude(
            r#"
        some = Foo.new('hello')
        res = []
        res << some.instance_exec { @key }
        res << some.instance_exec(10) { |x| @key * x }
        res
        "#,
            r#"
        class Foo
          def initialize(data)
            @key = data
          end
        end
        "#,
        );
        run_test(
            r#"
        class Foo
          def initialize
            @x = 42
          end
        end
        Foo.new.instance_exec { @x }
        "#,
        );
        run_test(
            r#"
        class Foo
          private
          def secret; "secret"; end
        end
        Foo.new.instance_exec { secret }
        "#,
        );
        run_test(
            r#"
        1.instance_exec(2, 3) { |a, b| self + a + b }
        "#,
        );
    }

    #[test]
    fn initialize() {
        // default initialize returns the new object
        run_tests(&["Object.new.class", "Object.new.is_a?(Object)"]);
        // custom initialize sets instance variables
        run_test(
            r#"
            class Foo
              def initialize(x)
                @x = x
              end
              def x; @x; end
            end
            Foo.new(42).x
            "#,
        );
        // super in initialize
        run_test(
            r#"
            class A
              def initialize
                @a = 1
              end
            end
            class B < A
              def initialize
                super
                @b = 2
              end
              def vals; [@a, @b]; end
            end
            B.new.vals
            "#,
        );
        // initialize with block
        run_test(
            r#"
            class C
              def initialize(&blk)
                @v = blk.call
              end
              def v; @v; end
            end
            C.new { 99 }.v
            "#,
        );
    }

    #[test]
    fn method_missing_private() {
        // private method_missing should still be dispatched
        run_test(
            r#"
            class Foo
              private
              def method_missing(name, *args)
                "mm:#{name}:#{args}"
              end
            end
            Foo.new.hello
            "#,
        );
        // arguments should be forwarded in correct order
        run_test(
            r#"
            class Foo
              private
              def method_missing(name, *args)
                "mm:#{name}:#{args}"
              end
            end
            Foo.new.bar(1, 2)
            "#,
        );
        // public method_missing should also work
        run_test(
            r#"
            class Foo
              def method_missing(name, *args)
                "mm:#{name}:#{args}"
              end
            end
            Foo.new.baz(10, 20, 30)
            "#,
        );
    }

    #[test]
    fn self_index_private_visibility() {
        // An explicit-`self` receiver ignores method visibility, so a
        // private `#[]` / `#[]=` is reachable through `self[k]`,
        // `self[k] = v`, and `self[k] op= v` — matching CRuby.
        run_test(
            r#"
            class A
              def initialize(h) @a = h end
              def get(k); self[k] end
              def set(k, v); self[k] = v end
              def opset(k, v); self[k] ||= v end
              def andset(k, v); self[k] &&= v end
              def plusset(k); self[k] += 100 end
              private
              def [](k) @a[k] end
              def []=(k, v) @a[k] = v; 42 end
            end
            a = A.new(k: false, n: 1)
            [a.get(:n), a.set(:x, 9), a.opset(:k, 10), a.andset(:n, 7), a.plusset(:n)]
            "#,
        );
        // Multi-index and splat-index forms on `self` also bypass privacy.
        run_test(
            r#"
            class B
              def initialize() @h = {} end
              def set2(a, b, v); self[a, b] = v end
              def get2(a, b); self[a, b] end
              def setsplat(ks, v); self[*ks] = v end
              private
              def [](a, b = nil) @h[[a, b]] end
              def []=(*args) @h[args[0..-2]] = args[-1]; :ok end
            end
            b = B.new
            [b.set2(1, 2, :v), b.get2(1, 2), b.setsplat([3, 4], :w)]
            "#,
        );
    }

    #[test]
    fn non_self_index_private_visibility() {
        // A private `#[]` / `#[]=` reached through a *non-`self`* receiver
        // (a plain object, or a local variable that merely holds `self`)
        // must raise `NoMethodError` — the JIT index fast path must not
        // bypass the visibility check the interpreter enforces. `run_test`
        // warms the JIT (25 iterations under a JITted loop), so this also
        // covers the compiled path.
        run_test(
            r#"
            class D
              def initialize() @a = {k: 1} end
              private
              def [](k) @a[k] end
              def []=(k, v) @a[k] = v end
            end
            obj = D.new
            g = (obj[:k] rescue :get_raised)
            s = begin; obj[:k] = 9; :set_ok; rescue NoMethodError; :set_raised; end
            [g, s]
            "#,
        );
        // A local variable holding `self` is not the literal `self`
        // keyword, so it must respect visibility too.
        run_test(
            r#"
            class E
              def initialize() @a = {k: 1} end
              def viaval
                x = self
                x[:k] rescue :raised
              end
              private
              def [](k) @a[k] end
            end
            E.new.viaval
            "#,
        );
    }

    #[test]
    fn method_missing_splat() {
        // splat arguments should be expanded and forwarded
        run_test(
            r#"
            class Foo
              def method_missing(name, *args)
                [name, args]
              end
            end
            a = [1, 2, 3]
            Foo.new.bar(*a)
            "#,
        );
        // empty splat
        run_test(
            r#"
            class Foo
              def method_missing(name, *args)
                [name, args]
              end
            end
            a = []
            Foo.new.bar(*a)
            "#,
        );
        // splat mixed with normal args
        run_test(
            r#"
            class Foo
              def method_missing(name, *args)
                [name, args]
              end
            end
            a = [2, 3]
            Foo.new.bar(1, *a, 4)
            "#,
        );
    }

    #[test]
    fn method_missing_hash_splat() {
        // hash splat should be merged into keyword hash
        run_test(
            r#"
            class Foo
              def method_missing(name, *args)
                [name, args]
              end
            end
            h = {a: 1, b: 2}
            Foo.new.bar(**h)
            "#,
        );
        // keyword args combined with hash splat
        run_test(
            r#"
            class Foo
              def method_missing(name, *args)
                [name, args]
              end
            end
            h = {b: 2}
            Foo.new.bar(a: 1, **h)
            "#,
        );
    }

    #[test]
    fn method_missing_with_block() {
        // &blk parameter
        run_test_once(
            r#"
            class Foo
              def method_missing(name, *args, &blk)
                [name, args, blk ? blk.call : nil]
              end
            end
            Foo.new.bar(1, 2) { 42 }
            "#,
        );
        // block with yield
        run_test_once(
            r#"
            class Foo
              def method_missing(name, *args)
                yield(*args) if block_given?
              end
            end
            Foo.new.bar(3, 4) { |a, b| a + b }
            "#,
        );
        // no block passed
        run_test(
            r#"
            class Foo
              def method_missing(name, *args, &blk)
                [name, blk.nil?]
              end
            end
            Foo.new.baz
            "#,
        );
    }
}
