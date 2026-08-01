use super::*;
#[cfg(target_arch = "aarch64")]
use jitgen::{AbstractState, JitContext};

//
// Fiber class
//

/// Hidden ivar carrying the fiber-local storage Hash (`Fiber#storage`).
const STORAGE_IVAR: &str = "/fiber_storage";

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Fiber", FIBER_CLASS, ObjTy::FIBER);
    globals.define_builtin_class_func_with_effect(
        FIBER_CLASS,
        "new",
        fiber_new,
        0,
        1,
        Effect::CAPTURE,
    );
    globals.define_builtin_class_inline_func_rest(
        FIBER_CLASS,
        "yield",
        fiber_yield,
        inline_gen2!(fiber_yield_inline),
    );
    globals.define_builtin_class_func(FIBER_CLASS, "current", current, 0);
    // The fiber running on this executor, or nil at a thread's root context
    // (where `Fiber.current` substitutes the root Fiber object). Internal:
    // `Thread#[]`'s fiber-local tables key off it (`:root` for nil).
    globals.define_builtin_class_func(FIBER_CLASS, "__current_fiber", current_fiber, 0);
    globals.define_builtin_class_func(FIBER_CLASS, "blocking?", class_blocking_p, 0);
    globals.define_builtin_class_func(FIBER_CLASS, "blocking", class_blocking, 0);
    globals.define_builtin_class_func(FIBER_CLASS, "scheduler", scheduler_get, 0);
    globals.define_builtin_class_func(FIBER_CLASS, "current_scheduler", current_scheduler, 0);
    globals.define_builtin_class_func(FIBER_CLASS, "set_scheduler", set_scheduler, 1);
    globals.define_builtin_class_func(FIBER_CLASS, "[]", class_storage_get, 1);
    globals.define_builtin_class_func(FIBER_CLASS, "[]=", class_storage_set, 2);
    globals.define_builtin_func_rest(FIBER_CLASS, "resume", resume);
    globals.define_builtin_func_rest(FIBER_CLASS, "transfer", transfer);
    globals.define_builtin_func(FIBER_CLASS, "alive?", alive_p, 0);
    globals.define_builtin_func(FIBER_CLASS, "kill", kill, 0);
    globals.define_builtin_func_with_kw(
        FIBER_CLASS,
        "raise",
        fiber_raise,
        0,
        3,
        false,
        &["cause"],
        true,
    );
    globals.define_builtin_func(FIBER_CLASS, "blocking?", blocking_p, 0);
    globals.define_builtin_func(FIBER_CLASS, "storage", storage_get, 0);
    globals.define_builtin_func(FIBER_CLASS, "storage=", storage_set, 1);
    globals.define_builtin_funcs(FIBER_CLASS, "inspect", &["to_s"], inspect, 0);
}

fn storage_ivar_id() -> IdentId {
    IdentId::get_id(STORAGE_IVAR)
}

///
/// The `Fiber` object of the current execution context: the fiber running
/// on this executor, or — at a thread's root context — a lazily created
/// root Fiber object aliasing the root executor itself, so root identity
/// is stable (`Fiber.current == Fiber.current`) and per thread.
///
pub(crate) fn current_fiber_obj(vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
    if let Some(f) = vm.current_fiber() {
        return Ok(f);
    }
    if let Some(obj) = vm.root_fiber_obj() {
        // The cached object may have captured a pre-move executor address
        // (the embedder's executor is returned by value from
        // `Executor::init`); re-anchor it to the live one.
        let ptr = std::ptr::NonNull::new(vm as *mut Executor).unwrap();
        Fiber::new(obj).reanchor_root(ptr);
        return Ok(obj);
    }
    let owner = crate::scheduler::current_thread(vm).id();
    let ptr = std::ptr::NonNull::new(vm as *mut Executor).unwrap();
    let inner = FiberInner::root(ptr, owner);
    let obj = Value::new_fiber(inner);
    if let Some(storage) = vm.inherited_storage() {
        globals.store.set_ivar(obj, storage_ivar_id(), storage)?;
    }
    vm.set_root_fiber_obj(obj);
    // A GC triggered while an ordinary fiber runs marks only the running
    // executor; keep the root Fiber reachable through Globals regardless of
    // which executor triggers the collection.
    globals.root_fiber_objs.push(obj);
    Ok(obj)
}

/// A copy of the current fiber's storage (`Thread.new` / `Fiber.new`
/// inheritance), or `None` when it has none.
pub(crate) fn current_fiber_storage(
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<Option<Value>> {
    // Read without materializing a root Fiber object: at an untouched root
    // context the storage (if any) is still the thread-creation snapshot.
    let cur = if let Some(f) = vm.current_fiber() {
        f
    } else if let Some(obj) = vm.root_fiber_obj() {
        obj
    } else {
        return Ok(vm.inherited_storage().map(|v| v.dup()));
    };
    Ok(globals
        .store
        .get_ivar(cur, storage_ivar_id())
        .filter(|v| !v.is_nil())
        .map(|v| v.dup()))
}

/// Explicit Fiber API calls (resume/transfer/raise/kill) are only legal from
/// the thread the fiber was created on (CRuby: "fiber called across
/// threads").
fn check_same_thread(vm: &mut Executor, fiber: &FiberInner) -> Result<()> {
    let owner = fiber.owner_thread();
    if owner != 0 && owner != crate::scheduler::current_thread(vm).id() {
        return Err(MonorubyErr::fibererr(
            "fiber called across threads".to_string(),
        ));
    }
    Ok(())
}

/// Validate a storage Hash (`Fiber.new(storage:)` / `Fiber#storage=`):
/// Hash-typed, unfrozen, Symbol keys only.
fn validate_storage(globals: &Globals, hash: Value) -> Result<()> {
    let Some(h) = hash.try_hash_ty() else {
        return Err(MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into Hash",
            hash.get_real_class_name(&globals.store)
        )));
    };
    if hash.is_frozen() {
        return Err(MonorubyErr::frozenerr("can't modify frozen Hash"));
    }
    for k in h.keys() {
        if k.try_symbol().is_none() {
            return Err(MonorubyErr::typeerr(format!(
                "wrong argument type {} (expected Symbol)",
                k.get_real_class_name(&globals.store)
            )));
        }
    }
    Ok(())
}

/// Storage keys: Symbols pass through; String / `#to_str` convertibles are
/// interned (Ruby 3.4 semantics); anything else is a TypeError. `#to_sym`
/// is deliberately not consulted.
fn storage_key(vm: &mut Executor, globals: &mut Globals, key: Value) -> Result<Value> {
    if key.try_symbol().is_some() {
        return Ok(key);
    }
    if key.is_str().is_some() || globals.check_method(key, IdentId::TO_STR).is_some() {
        let id = key.coerce_to_symbol_or_string(vm, globals)?;
        return Ok(Value::symbol(id));
    }
    Err(MonorubyErr::typeerr(format!(
        "wrong argument type {} (expected Symbol)",
        key.get_real_class_name(&globals.store)
    )))
}

///
/// ### Fiber.current
///
/// - current -> Fiber
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/s/current.html]
#[monoruby_builtin]
fn current(vm: &mut Executor, globals: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    current_fiber_obj(vm, globals)
}

/// `Fiber.__current_fiber` — see `init`.
#[monoruby_builtin]
fn current_fiber(vm: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(vm.current_fiber().unwrap_or_default())
}

///
/// ### Fiber.new
///
/// - new(blocking: false, storage: true) {|obj| ... } -> Fiber
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/s/new.html]
#[monoruby_builtin]
fn fiber_new(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let Some(bh) = lfp.block() else {
        return Err(MonorubyErr::argumenterr(
            "tried to create a Fiber without a block",
        ));
    };
    // Keywords (`blocking:`, `storage:`) arrive as a trailing hash.
    let mut blocking = false;
    let mut storage_kw: Option<Value> = None;
    if let Some(arg) = lfp.try_arg(0)
        && let Some(h) = arg.try_hash_ty()
    {
        if let Some(v) = h.get(Value::symbol(IdentId::get_id("blocking")), vm, globals)? {
            blocking = v.as_bool();
        }
        if let Some(v) = h.get(Value::symbol(IdentId::get_id("storage")), vm, globals)? {
            storage_kw = Some(v);
        }
    }
    let storage = match storage_kw {
        // `storage: nil` — start empty, lazily materialized; no inheritance.
        Some(v) if v.is_nil() => None,
        Some(v) => {
            validate_storage(globals, v)?;
            Some(v.dup())
        }
        // Default: inherit a copy of the creating fiber's storage.
        None => current_fiber_storage(vm, globals)?,
    };
    let proc = vm.generate_proc(globals, bh, pc)?;
    let owner = crate::scheduler::current_thread(vm).id();
    let thread_root = Some(vm.thread_root_or_self());
    let mut inner = FiberInner::new(proc, owner, thread_root);
    inner.executor_mut().set_fiber_blocking(blocking);
    let class_id = lfp.self_val().as_class_id();
    let obj = Value::new_fiber_with_class(inner, class_id);
    if let Some(st) = storage {
        globals.store.set_ivar(obj, storage_ivar_id(), st)?;
    }
    Ok(obj)
}

///
/// ### Fiber.yield
///
/// - yield(*arg = nil) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/s/yield.html]
#[monoruby_builtin]
fn fiber_yield(
    vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    if vm.parent_fiber().is_none() {
        return Err(MonorubyErr::fibererr(
            "attempt to yield on a not resumed fiber".to_string(),
        ));
    }
    let len = lfp.arg(0).as_array().len();
    let val = if len == 0 {
        Value::nil()
    } else if len == 1 {
        lfp.arg(0).as_array()[0]
    } else {
        lfp.arg(0)
    };
    vm.yield_fiber(val)
}

/// Out-of-line error path for the inlined `Fiber.yield`: yielding with
/// no parent fiber (the main fiber, a green thread's root, or a
/// transferred fiber) raises FiberError instead of switching through a
/// null parent pointer.
pub(crate) extern "C" fn fiber_yield_no_parent(vm: &mut Executor) -> Option<Value> {
    vm.set_error(MonorubyErr::fibererr(
        "attempt to yield on a not resumed fiber".to_string(),
    ));
    None
}

fn fiber_yield_inline(
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
    let CallSiteInfo {
        args, pos_num, dst, ..
    } = *callsite;
    // `Fiber.yield` suspends this fiber: control returns to the resumer
    // and arbitrary code (including GCs) runs while this frame stays live
    // on the fiber's stack. Unlike a normal call, the inlined yield does
    // not otherwise spill register/literal-resident slots, so the
    // suspended frame would not be GC-complete and a collection in the
    // resumer could free a value the frame still holds (e.g. an
    // interpolation operand). Write the frame back (the standard GC
    // safepoint) before yielding so every live slot is materialised.
    state.exec_gc(ir, false);
    let using_fpr = state.get_using_fpr(ir);
    let error = ir.new_error(state);
    ir.fpr_save(using_fpr);
    if pos_num == 0 {
        ir.inline(move |r#gen, _, _, _| r#gen.emit_fiber_yield_value_nil());
    } else if pos_num == 1 {
        state.load(ir, args, GP::Rsi);
    } else {
        state.write_back_recv_and_callargs(ir, callsite);
        let args_off = jitgen::conv(args) as usize;
        ir.inline(move |r#gen, _, _, _| r#gen.emit_fiber_yield_value_array(args_off, pos_num));
    }
    ir.inline(move |r#gen, _, _, _| {
        let yield_fiber = r#gen.yield_fiber as *const () as u64;
        let no_parent = fiber_yield_no_parent as *const () as u64;
        r#gen.emit_fiber_yield_call(yield_fiber, no_parent)
    });
    ir.fpr_restore(using_fpr);
    ir.handle_error(error);
    state.def_rax2acc(ir, dst);
    true
}

///
/// ### Fiber#resume
///
/// - resume(*arg = nil) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/i/resume.html]
#[monoruby_builtin]
fn resume(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_val = Fiber::new(lfp.self_val());
    check_same_thread(vm, &self_val)?;
    self_val.resume(vm, globals, lfp)
}

///
/// ### Fiber#transfer
///
/// - transfer(*args) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/i/transfer.html]
#[monoruby_builtin]
fn transfer(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_val = Fiber::new(lfp.self_val());
    check_same_thread(vm, &self_val)?;
    let binding = lfp.arg(0);
    let args = binding.as_array();
    self_val.transfer(vm, globals, &args)
}

///
/// ### Fiber#alive?
///
/// - alive? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/i/alive=3f.html]
#[monoruby_builtin]
fn alive_p(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let fiber = Fiber::new(lfp.self_val());
    // A thread's root fiber lives as long as the thread does — and a root
    // Fiber object is only obtainable from code running on that thread, so
    // it reads as alive.
    let alive = fiber.is_root() || !fiber.is_terminated();
    Ok(Value::bool(alive))
}

///
/// ### Fiber#kill
///
/// Unwinds the fiber (running `ensure` clauses, skipping every `rescue`)
/// and terminates it. Killing a created fiber marks it dead without
/// running the body; killing an active ancestor is deferred until control
/// returns to it.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/i/kill.html]
#[monoruby_builtin]
fn kill(vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut fiber = Fiber::new(lfp.self_val());
    check_same_thread(vm, &fiber)?;
    if fiber.is_root() {
        return Err(MonorubyErr::fibererr(
            "attempt to kill the fiber of the main thread".to_string(),
        ));
    }
    if fiber.is_terminated() {
        return Err(MonorubyErr::fibererr(
            "attempt to resume a terminated fiber".to_string(),
        ));
    }
    if fiber.executor_ptr() == (vm as *mut Executor) {
        // Kill myself: unwind in place; the pending resume/transfer on the
        // other side swallows the kill when this fiber's body terminates.
        vm.set_killed();
        return Err(MonorubyErr::new(MonorubyErrKind::FiberKill, "killed"));
    }
    if fiber.executor().resuming_fiber().is_some() {
        // An active ancestor on the resume chain (CRuby): the kill unwinds
        // the *current* fiber immediately; the FiberKill propagates through
        // each intermediate fiber's pending `resume` up to the target,
        // whose own receiver — seeing the target's `killed` flag — swallows
        // it. Code after this call in the current fiber does not run.
        fiber.executor_mut().set_killed();
        return Err(MonorubyErr::new(MonorubyErrKind::FiberKill, "killed"));
    }
    if fiber.state() == FiberState::Created {
        // Never started: dead immediately, no body, no ensure.
        fiber.executor_mut().set_terminated();
        return Ok(Value::nil());
    }
    let resume_style = fiber.executor().is_yielding();
    fiber.executor_mut().set_killed();
    let err = MonorubyErr::new(MonorubyErrKind::FiberKill, "killed");
    fiber.inject_error(vm, err, resume_style)?;
    Ok(Value::nil())
}

///
/// ### Fiber#raise
///
/// - raise -> object
/// - raise(message, cause: $!) -> object
/// - raise(exception, message = nil, backtrace = nil, cause: $!) -> object
///
/// Delivers the exception at the fiber's suspension point; raising on the
/// current fiber (or an active ancestor) is equivalent to `Kernel#raise`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/i/raise.html]
#[monoruby_builtin]
fn fiber_raise(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // Same keyword layout as Kernel#raise / Thread#raise: `cause:` at slot
    // 3, leftover keywords (kw_rest, slot 4) act as a trailing positional
    // Hash.
    let cause_kwarg = lfp.try_arg(3);
    let kw_rest = lfp
        .try_arg(4)
        .filter(|v| v.try_hash_ty().is_some_and(|h| h.len() != 0));
    let mut args = vec![];
    if let Some(a0) = lfp.try_arg(0) {
        args.push(a0);
        if let Some(a1) = lfp.try_arg(1) {
            args.push(a1);
            if let Some(a2) = lfp.try_arg(2) {
                args.push(a2);
            }
        }
    }
    if let Some(kw) = kw_rest {
        args.push(kw);
    }
    let mut fiber = Fiber::new(lfp.self_val());
    check_same_thread(vm, &fiber)?;
    let mut err = super::thread::build_async_error(vm, globals, &args, cause_kwarg)?;
    // The cause comes from the *calling* context: pin the caller's `$!`
    // (or its absence) so delivery in the target never picks up the
    // target's own `$!`.
    if cause_kwarg.is_none() && err.explicit_cause.is_none() {
        let errinfo = vm.errinfo();
        let cause = if errinfo.is_exception().is_some()
            && err.original.map(|o| o.id()) != Some(errinfo.id())
        {
            errinfo
        } else {
            Value::nil()
        };
        err.explicit_cause = Some(cause);
    }
    if fiber.executor_ptr() == (vm as *mut Executor) {
        // Raising on the current fiber is Kernel#raise.
        return Err(err);
    }
    match fiber.state() {
        FiberState::Created => Err(MonorubyErr::fibererr(
            "cannot raise exception on unborn fiber".to_string(),
        )),
        FiberState::Terminated => Err(MonorubyErr::fibererr(
            "attempt to resume a terminated fiber".to_string(),
        )),
        FiberState::Suspended => {
            if fiber.executor().resuming_fiber().is_some() {
                // An active ancestor waiting inside `resume`: equivalent to
                // Kernel#raise here — the unwind naturally reaches it
                // through the resume chain.
                return Err(err);
            }
            let resume_style = fiber.executor().is_yielding();
            fiber.inject_error(vm, err, resume_style)
        }
    }
}

///
/// ### Fiber#blocking?
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/i/blocking=3f.html]
#[monoruby_builtin]
fn blocking_p(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let fiber = Fiber::new(lfp.self_val());
    Ok(Value::bool(fiber.executor().is_fiber_blocking()))
}

///
/// ### Fiber.blocking?
///
/// Returns `1` when the current fiber is blocking, `false` otherwise.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/s/blocking=3f.html]
#[monoruby_builtin]
fn class_blocking_p(vm: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(if vm.is_fiber_blocking() {
        Value::integer(1)
    } else {
        Value::bool(false)
    })
}

///
/// ### Fiber.blocking
///
/// Runs the block with the current fiber temporarily blocking, passing the
/// current fiber to it.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/s/blocking.html]
#[monoruby_builtin]
fn class_blocking(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let cur = current_fiber_obj(vm, globals)?;
    let old = vm.is_fiber_blocking();
    vm.set_fiber_blocking(true);
    let res = vm.invoke_block_once(globals, bh, &[cur]);
    vm.set_fiber_blocking(old);
    res
}

///
/// ### Fiber.scheduler
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/s/scheduler.html]
#[monoruby_builtin]
fn scheduler_get(_: &mut Executor, globals: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(globals.fiber_scheduler.unwrap_or_default())
}

///
/// ### Fiber.current_scheduler
///
/// The scheduler, when the current fiber is non-blocking; nil otherwise.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/s/current_scheduler.html]
#[monoruby_builtin]
fn current_scheduler(vm: &mut Executor, globals: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(if vm.is_fiber_blocking() {
        Value::nil()
    } else {
        globals.fiber_scheduler.unwrap_or_default()
    })
}

///
/// ### Fiber.set_scheduler
///
/// Validates the scheduler interface (`#block`, `#unblock`,
/// `#kernel_sleep`, `#io_wait`) and installs it; `nil` clears.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/s/set_scheduler.html]
#[monoruby_builtin]
fn set_scheduler(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let scheduler = lfp.arg(0);
    if scheduler.is_nil() {
        globals.fiber_scheduler = None;
        return Ok(Value::nil());
    }
    for method in ["block", "unblock", "kernel_sleep", "io_wait"] {
        if globals
            .check_method(scheduler, IdentId::get_id(method))
            .is_none()
        {
            return Err(MonorubyErr::argumenterr(format!(
                "Scheduler must implement #{method}"
            )));
        }
    }
    globals.fiber_scheduler = Some(scheduler);
    Ok(scheduler)
}

///
/// ### Fiber.[]
///
/// Reads `key` from the current fiber's storage.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/s/=5b=5d.html]
#[monoruby_builtin]
fn class_storage_get(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let key = storage_key(vm, globals, lfp.arg(0))?;
    let cur = current_fiber_obj(vm, globals)?;
    let Some(storage) = globals
        .store
        .get_ivar(cur, storage_ivar_id())
        .filter(|v| !v.is_nil())
    else {
        return Ok(Value::nil());
    };
    let h = storage.try_hash_ty().unwrap();
    Ok(h.get(key, vm, globals)?.unwrap_or_default())
}

///
/// ### Fiber.[]=
///
/// Writes `key` into the current fiber's storage, materializing it on
/// first write; assigning `nil` deletes the key.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/s/=5b=5d=3d.html]
#[monoruby_builtin]
fn class_storage_set(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let key = storage_key(vm, globals, lfp.arg(0))?;
    let val = lfp.arg(1);
    let cur = current_fiber_obj(vm, globals)?;
    let storage = match globals
        .store
        .get_ivar(cur, storage_ivar_id())
        .filter(|v| !v.is_nil())
    {
        Some(s) => s,
        None => {
            let s = Value::hash(RubyMap::default());
            globals.store.set_ivar(cur, storage_ivar_id(), s)?;
            s
        }
    };
    let mut h = storage.try_hash_ty().unwrap();
    if val.is_nil() {
        h.remove(key, vm, globals)?;
    } else {
        h.insert(key, val, vm, globals)?;
    }
    Ok(val)
}

///
/// ### Fiber#storage
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/i/storage.html]
#[monoruby_builtin]
fn storage_get(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let cur = current_fiber_obj(vm, globals)?;
    if cur.id() != lfp.self_val().id() {
        return Err(MonorubyErr::argumenterr(
            "Fiber storage can only be accessed from the Fiber it belongs to",
        ));
    }
    Ok(globals
        .store
        .get_ivar(lfp.self_val(), storage_ivar_id())
        .unwrap_or_default())
}

///
/// ### Fiber#storage=
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/i/storage=3d.html]
#[monoruby_builtin]
fn storage_set(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let hash = lfp.arg(0);
    if hash.is_nil() {
        globals
            .store
            .set_ivar(lfp.self_val(), storage_ivar_id(), Value::nil())?;
        return Ok(Value::nil());
    }
    validate_storage(globals, hash)?;
    globals
        .store
        .set_ivar(lfp.self_val(), storage_ivar_id(), hash)?;
    Ok(hash)
}

///
/// ### Fiber#inspect
///
/// `#<Fiber:0x.... file:line (status)>`; the root fiber has no location.
///
#[monoruby_builtin]
fn inspect(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let obj = lfp.self_val();
    let fiber = Fiber::new(obj);
    let state = if fiber.executor() as *const Executor == vm as *const Executor {
        "resumed"
    } else {
        match fiber.state() {
            FiberState::Created => "created",
            FiberState::Suspended => "suspended",
            FiberState::Terminated => "terminated",
        }
    };
    let loc = fiber
        .func_id()
        .and_then(|fid| globals.store[fid].is_iseq())
        .map(|iseq| globals.store[iseq].get_location());
    let s = match loc {
        Some(loc) => format!("#<Fiber:0x{:016x} {} ({})>", obj.id(), loc, state),
        None => format!("#<Fiber:0x{:016x} ({})>", obj.id(), state),
    };
    Ok(Value::string(s))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn fiber_svar_scope() {
        // A `Fiber.new` body owns its `$~` / `$1`, like a thread body:
        // nothing leaks in on entry, nothing leaks back on yield.
        run_test(
            r#"
            "foo" =~ /(o+)/
            r = []
            f = Fiber.new do
              r << ["initial", $1]
              "zzz" =~ /(z+)/
              r << ["after-match", $1]
              Fiber.yield
            end
            f.resume
            r << ["creator-after-resume", $1]
            r
            "#,
        );
        // Enumerator external iteration must NOT isolate: CRuby drives
        // it with a C-function fiber (root_lep = NULL), so a match
        // inside the block *is* visible to the caller after `next`.
        run_test(
            r#"
            "foo" =~ /(o+)/
            e = Enumerator.new { |y| "bb" =~ /(b+)/; y << 1; "cc" =~ /(c+)/; y << 2 }
            a = (e.next; $1)
            b = (e.next; $1)
            [a, b]
            "#,
        );
        // ... and neither does the plain block form (no fiber at all).
        run_test(
            r#"
            "foo" =~ /(o+)/
            Enumerator.new { |y| "dd" =~ /(d+)/; y << 1 }.each { }
            $1
            "#,
        );
    }

    #[test]
    fn fiber_yield_multi_arg() {
        // `Fiber.yield(a, b, ...)` with >=2 args exercises the inlined
        // `emit_fiber_yield_value_array` (builds the yielded array from the
        // arg slots). Warm the fiber body so the yield JIT-compiles; the >=2
        // arg array-build path (aarch64 addresses the arg slots through a
        // scratch register for any frame offset) must match CRuby.
        run_test(
            r#"
            def run
              out = []
              50.times do |k|
                f = Fiber.new do
                  a = Fiber.yield(k, k + 1)
                  b = Fiber.yield(k + 2, k + 3, k + 4)
                  [a, b]
                end
                out << f.resume
                out << f.resume(100)
                out << f.resume(200)
              end
              out
            end
            run
            "#,
        );
    }

    #[test]
    fn fiber_error() {
        run_test_error("Fiber.yield");
        run_test_error(
            r#"
            f = Fiber.new do
            end
            f.resume
            f.resume
        "#,
        );
    }

    #[test]
    fn fiber_resume_self_or_ancestor() {
        // Resuming the currently-running fiber, or a still-active
        // ancestor on the resume chain, must raise `FiberError` (was
        // SIGSEGV from native-stack switching into a live frame).
        // CRuby:
        //   self-resume      ⇒ "attempt to resume the current fiber"
        //   ancestor-resume  ⇒ "attempt to resume a resumed fiber (double resume)"
        run_test_error(
            r#"
            f = nil
            f = Fiber.new { f.resume }
            f.resume
        "#,
        );
        run_test_error(
            r#"
            outer = nil
            inner = nil
            outer = Fiber.new {
              inner = Fiber.new { outer.resume }
              inner.resume
            }
            outer.resume
        "#,
        );
    }

    #[test]
    fn fiber() {
        run_test(
            r##"
            answer = []
            f = Fiber.new do
                outer = 42
                answer << "invoked #{outer}"
                30.times {|i|
                    answer << "yield = #{Fiber.yield}"
                    answer << "yield = #{Fiber.yield i}"
                    answer << "yield = #{Fiber.yield i, i+1, i+2}"
                }
                "terminated #{outer}"
            end
            31.times do |i|
              answer << "resume = #{f.resume i}"
            end
            answer
        "##,
        );
    }

    #[test]
    fn fiber_closure() {
        run_test_with_prelude(
            r#"
            create_fiber.resume
        "#,
            r#"
            def create_fiber
              a = 100
              Fiber.new do
                Fiber.yield a
              end
            end
        "#,
        )
    }

    #[test]
    fn fib() {
        run_test(
            r##"
            fib = Fiber.new do
                a = b = 1
                loop do
                    Fiber.yield a
                    a, b = a + b, a
                end
            end

            30.times do fib.resume end
            fib.resume
        "##,
        );
    }

    #[test]
    fn fiber_current() {
        run_test("Fiber.current.is_a?(Fiber)");
        // Root identity is stable, and a fiber sees itself.
        run_test(
            r##"
            root = Fiber.current
            inner = nil
            f = Fiber.new { inner = Fiber.current }
            f.resume
            [root.equal?(Fiber.current), inner.equal?(f), root.equal?(f)]
            "##,
        );
    }

    #[test]
    fn fiber_transfer_basic() {
        // Termination of a transferred fiber returns to the deepest
        // resuming fiber (fiber2 here), not to the transferrer chain root.
        run_test(
            r##"
            fiber1 = Fiber.new { :fiber1 }
            fiber2 = Fiber.new { fiber1.transfer; :fiber2 }
            fiber2.resume
            "##,
        );
        // Root-transfer round trip: control comes back to the root fiber
        // with the transferred fiber's terminal value.
        run_test(
            r##"
            f1 = Fiber.new { :fiber_1 }
            f2 = Fiber.new { f1.transfer; :fiber_2 }
            a = f2.transfer
            b = f2.transfer
            [a, b]
            "##,
        );
        // Self-transfer is a no-op continuing in place.
        run_test(
            r##"
            states = []
            fiber = Fiber.new { states << :start; fiber.transfer; states << :end }
            fiber.transfer
            states
            "##,
        );
    }

    #[test]
    fn fiber_transfer_errors() {
        // Transfer to a fiber suspended in Fiber.yield.
        run_test_error(
            r##"
            fiber2 = Fiber.new { Fiber.yield }
            fiber2.resume
            fiber2.transfer
            "##,
        );
        // Yield inside a transferred fiber has no resumer.
        run_test_error(
            r##"
            f = Fiber.new { Fiber.yield }
            f.transfer
            "##,
        );
        // Dead fiber.
        run_test_error(
            r##"
            f = Fiber.new { }
            f.transfer
            f.transfer
            "##,
        );
        // A transferred fiber cannot be resumed.
        run_test_error(
            r##"
            root = Fiber.current
            f = Fiber.new { root.transfer }
            f.transfer
            f.resume
            "##,
        );
    }

    #[test]
    fn fiber_transfer_mixed_with_yield() {
        // Fiber#resume driving a fiber that first transfers elsewhere and
        // then yields (ruby/spec "can work with Fiber#transfer").
        run_test(
            r##"
            fiber1 = Fiber.new { true }
            fiber2 = Fiber.new { fiber1.transfer; Fiber.yield 10; Fiber.yield 20 }
            a = fiber2.resume
            b = fiber2.resume
            [a, b]
            "##,
        );
    }

    #[test]
    fn fiber_alive_and_kill() {
        run_test(
            r##"
            res = []
            f = Fiber.new { Fiber.yield }
            res << f.alive?
            f.resume
            res << f.alive?
            f.kill
            res << f.alive?
            g = Fiber.new { }
            g.kill
            res << g.alive?
            res
            "##,
        );
        // ensure runs on kill; rescue does not.
        run_test(
            r##"
            ensured = false
            rescued = false
            f = Fiber.new do
              begin
                while true; Fiber.yield; end
              rescue Exception
                rescued = true
              ensure
                ensured = true
              end
            end
            f.resume
            f.kill
            [ensured, rescued, f.alive?]
            "##,
        );
        // kill myself terminates the fiber without raising at the resumer.
        run_test(
            r##"
            f = nil
            f = Fiber.new { Fiber.current.kill; :unreachable }
            v = f.resume
            [v, f.alive?]
            "##,
        );
        // killing an active ancestor is deferred until control returns.
        run_test(
            r##"
            log = []
            parent = nil
            parent = Fiber.new do
              child = Fiber.new do
                parent.kill
                log << parent.alive?
              end
              child.resume
              log << :not_reached
            end
            parent.resume
            log << parent.alive?
            log
            "##,
        );
    }

    #[test]
    fn fiber_raise() {
        // Deliver at the yield point; unhandled -> propagates to the
        // raiser; the fiber dies.
        run_test(
            r##"
            f = Fiber.new { Fiber.yield :first; :second }
            f.resume
            begin
              f.raise "boom"
            rescue => e
              [e.class.to_s, e.message, f.alive?]
            end
            "##,
        );
        // Rescued inside the fiber: raise returns the next yielded value.
        run_test(
            r##"
            f = Fiber.new do
              begin
                Fiber.yield :a
              rescue => e
                Fiber.yield [:rescued, e.message]
              end
            end
            f.resume
            f.raise("caught")
            "##,
        );
        run_test_error("Fiber.new { true }.raise");
        // raise on itself is Kernel#raise.
        run_test(
            r##"
            begin
              Fiber.current.raise "self"
            rescue => e
              e.message
            end
            "##,
        );
    }

    #[test]
    fn fiber_inspect_states() {
        run_test(
            r##"
            created = Fiber.new {}
            res = [created.inspect =~ /\A#<Fiber:0x\h+ .+ \(created\)>\z/ ? true : false]
            resumed = Fiber.new { Fiber.current.inspect }
            res << (resumed.resume =~ /\(resumed\)>\z/ ? true : false)
            suspended = Fiber.new { Fiber.yield }
            suspended.resume
            res << (suspended.inspect =~ /\(suspended\)>\z/ ? true : false)
            done = Fiber.new {}
            done.resume
            res << (done.inspect =~ /\(terminated\)>\z/ ? true : false)
            res << (Fiber.current.inspect =~ /\A#<Fiber:0x\h+ \(resumed\)>\z/ ? true : false)
            res
            "##,
        );
    }

    /// Fiber-local storage: class-method sugar, `storage:` keyword,
    /// inheritance, and validation.
    #[test]
    fn fiber_storage() {
        run_test(
            r##"
            Fiber[:a] = 1
            Fiber[:b] = "x"
            [Fiber.current.storage, Fiber[:a], Fiber[:missing]]
            "##,
        );
        // String keys convert to Symbols (Ruby 3.4).
        run_test(
            r##"
            Fiber["skey"] = 7
            Fiber[:skey]
            "##,
        );
        run_test_error(r##"Fiber[Object.new]"##);
        run_test(
            r##"
            f = Fiber.new(storage: {life: 42}) { Fiber.current.storage }
            f.resume
            "##,
        );
        // Default storage inherits a copy from the creating fiber.
        run_test(
            r##"
            f = Fiber.new(storage: {life: 42}) do
              inner = Fiber.new { Fiber[:life] = 43; Fiber[:life] }.resume
              [inner, Fiber[:life]]
            end
            f.resume
            "##,
        );
        // nil value deletes the key; storage: nil starts fresh.
        run_test(
            r##"
            f = Fiber.new(storage: {life: 42}) { Fiber[:life] = nil; Fiber.current.storage }
            g = Fiber.new(storage: nil) { Fiber[:x] = 10; Fiber.current.storage }
            [f.resume, g.resume]
            "##,
        );
        run_test_error(r##"Fiber.new(storage: 42) {}"##);
        run_test_error(r##"Fiber.new(storage: {life: 43}.freeze) {}"##);
        run_test_error(r##"Fiber.new(storage: {Object.new => 44}) {}"##);
        // Cross-fiber storage read is rejected.
        run_test_error(
            r##"
            f = Fiber.new(storage: {life: 42}) { nil }
            f.storage
            "##,
        );
        run_test_error(r##"Fiber.current.storage = 1"##);
        run_test(
            r##"
            f = Fiber.current
            f.storage = {c: 3}
            r = f.storage
            f.storage = nil
            [r, f.storage]
            "##,
        );
        // Thread.new inherits the spawning fiber's storage.
        run_test(
            r##"
            fiber = Fiber.new(storage: {life: 42}) do
              Thread.new { Fiber.current.storage }.value
            end
            fiber.resume
            "##,
        );
    }

    /// Per-fiber blocking flags: `Fiber.new` defaults to non-blocking,
    /// roots are blocking, and `Fiber.blocking` temporarily flips it.
    #[test]
    fn fiber_blocking() {
        run_test_once(
            r##"
            res = []
            res << Fiber.scheduler
            res << Fiber.set_scheduler(nil)
            res << Fiber.scheduler
            res << Fiber.current_scheduler
            res << Fiber.blocking?
            res << Fiber.current.blocking?
            res << Fiber.new { Fiber.blocking? }.resume
            res << Fiber.new { Fiber.current.blocking? }.resume
            res << Fiber.new(blocking: true) { Fiber.blocking? }.resume
            res << Fiber.new(blocking: false) do
              Fiber.blocking { |f| f.blocking? ? :blocking : :non_blocking }
            end.resume
            res
            "##,
        );
    }

    #[test]
    fn fiber_scheduler_validation() {
        run_test_once(
            r##"
            required = [:block, :unblock, :kernel_sleep, :io_wait]
            res = []
            required.each do |missing|
              s = Object.new
              (required - [missing]).each { |m| s.define_singleton_method(m) {} }
              begin
                Fiber.set_scheduler(s)
                res << :accepted
              rescue ArgumentError => e
                res << e.message
              end
            end
            good = Object.new
            required.each { |m| good.define_singleton_method(m) {} }
            Fiber.set_scheduler(good)
            res << (Fiber.scheduler == good)
            Fiber.set_scheduler(nil)
            res << Fiber.scheduler
            res
            "##,
        );
    }

    /// Explicit Fiber API calls from a foreign thread are rejected, and the
    /// fiber stays usable from its owning thread afterwards.
    #[test]
    fn fiber_cross_thread_calls() {
        run_test(
            r##"
            res = []
            f1 = Fiber.new { :r }
            f2 = Fiber.new { :t }
            f3 = Fiber.new { Fiber.yield }
            f3.resume
            f4 = Fiber.new { Fiber.yield }
            f4.resume
            Thread.new do
              [-> { f1.resume }, -> { f2.transfer }, -> { f3.raise "x" }, -> { f4.kill }].each do |op|
                begin
                  op.call
                  res << :no_raise
                rescue FiberError => e
                  res << e.message
                end
              end
            end.join
            res << f1.resume
            res
            "##,
        );
    }

    /// Raise into a fiber suspended in `Fiber#transfer` (transfer-style
    /// injection), and the unborn / dead / no-op-kill edges.
    #[test]
    fn fiber_raise_kill_edges() {
        run_test(
            r##"
            root = Fiber.current
            fiber = Fiber.new { root.transfer; :not_reached }
            fiber.transfer
            begin
              fiber.raise "msg"
            rescue => e
              [e.class.to_s, e.message, fiber.alive?]
            end
            "##,
        );
        // Raising on a dead fiber.
        run_test_error(
            r##"
            f = Fiber.new { }
            f.resume
            f.raise "boom"
            "##,
        );
        // Killing a dead fiber raises (CRuby).
        run_test_error(
            r##"
            f = Fiber.new { }
            f.resume
            f.kill
            "##,
        );
        // Killing a thread's root fiber is refused (intentional divergence:
        // CRuby terminates the thread; monoruby raises FiberError).
        let v = run_test_no_result_check(
            r##"
            begin
              Fiber.current.kill
              :no_raise
            rescue FiberError
              :fiber_error
            end
            "##,
        );
        assert!(format!("{v:?}").contains("fiber_error"));
    }

    /// `Kernel#sleep` inside a non-blocking fiber delegates to the
    /// installed scheduler's `kernel_sleep` hook; `Fiber.current_scheduler`
    /// is visible from the non-blocking fiber.
    #[test]
    fn fiber_scheduler_kernel_sleep() {
        run_test_once(
            r##"
            log = []
            sched = Object.new
            [:block, :unblock, :io_wait].each { |m| sched.define_singleton_method(m) {} }
            sched.define_singleton_method(:kernel_sleep) { |*a| log << a; :slept }
            Fiber.set_scheduler(sched)
            r = Fiber.new { [Fiber.current_scheduler.equal?(sched), sleep(5)] }.resume
            Fiber.set_scheduler(nil)
            [r, log]
            "##,
        );
    }

    /// `#to_str`-convertible storage keys (Ruby 3.4) and `Thread.start`
    /// inheriting the spawning fiber's storage.
    #[test]
    fn fiber_storage_edges() {
        run_test(
            r##"
            key = Object.new
            def key.to_str = "kk"
            Fiber[key] = 5
            Fiber[:kk]
            "##,
        );
        run_test(
            r##"
            fiber = Fiber.new(storage: {life: 42}) do
              Thread.start { Fiber.current.storage }.value
            end
            fiber.resume
            "##,
        );
    }

    #[test]
    fn fiber_new_edge_cases() {
        run_test_error("Fiber.new");
        run_test_with_prelude(
            r#"
            f = MyFiber.new { :done }
            [f.class.to_s, f.resume]
        "#,
            r#"
            class MyFiber < Fiber
            end
        "#,
        );
    }
}
