use super::*;
use crate::scheduler;
use crate::value::rvalue::{InterruptTiming, PendingInterrupt, ThreadInner, ThreadState};

//
// Thread class
//
// monoruby threads are cooperative green threads multiplexed on the one
// OS thread by the scheduler (src/scheduler.rs). `Thread.new` queues the
// body; it starts running the first time any thread reaches a blocking
// point (`sleep`, `#join`, `Thread.pass`, ...). Blocking APIs park the
// calling thread on the scheduler instead of blocking the process.
//
// Ruby-level surface that is pure bookkeeping (name, thread/fiber-local
// storage, `Thread::Waiter` for `Process.detach`) stays in
// monoruby/builtins/startup.rb.

pub(super) fn init(globals: &mut Globals) {
    globals
        .store
        .define_builtin_class_under_obj("Thread", THREAD_CLASS, ObjTy::THREAD);
    globals.store[THREAD_CLASS].set_alloc_func(thread_alloc_func);

    globals.define_builtin_class_func_rest(THREAD_CLASS, "new", thread_new);
    globals.define_builtin_class_func_rest(THREAD_CLASS, "start", thread_new);
    globals.define_builtin_class_func_rest(THREAD_CLASS, "fork", thread_new);
    globals.define_builtin_class_func(THREAD_CLASS, "current", thread_current, 0);
    globals.define_builtin_class_func(THREAD_CLASS, "main", thread_main, 0);
    globals.define_builtin_class_func(THREAD_CLASS, "pass", thread_pass, 0);
    globals.define_builtin_class_func(THREAD_CLASS, "list", thread_list, 0);
    globals.define_builtin_class_func(THREAD_CLASS, "stop", thread_stop, 0);

    globals.define_builtin_func_with(THREAD_CLASS, "join", thread_join, 0, 1, false);
    globals.define_builtin_func(THREAD_CLASS, "value", thread_value, 0);
    globals.define_builtin_func(THREAD_CLASS, "status", thread_status, 0);
    globals.define_builtin_func(THREAD_CLASS, "backtrace", thread_backtrace, 0);
    globals.define_builtin_func(THREAD_CLASS, "alive?", thread_alive, 0);
    globals.define_builtin_func(THREAD_CLASS, "stop?", thread_stop_p, 0);
    globals.define_builtin_func(THREAD_CLASS, "wakeup", thread_wakeup, 0);
    globals.define_builtin_func(THREAD_CLASS, "__wakeup_permit", thread_wakeup_permit, 0);
    globals.define_builtin_func(THREAD_CLASS, "run", thread_run, 0);
    globals.define_builtin_func_rest(THREAD_CLASS, "raise", thread_raise);
    globals.define_builtin_func(THREAD_CLASS, "kill", thread_kill, 0);
    globals.define_builtin_func(THREAD_CLASS, "exit", thread_kill, 0);
    globals.define_builtin_func(THREAD_CLASS, "terminate", thread_kill, 0);
    globals.define_builtin_class_func(THREAD_CLASS, "kill", thread_class_kill, 1);
    globals.define_builtin_class_func(THREAD_CLASS, "exit", thread_class_exit, 0);
    globals.define_builtin_class_func(THREAD_CLASS, "handle_interrupt", handle_interrupt, 1);
    globals.define_builtin_class_func_with(
        THREAD_CLASS,
        "pending_interrupt?",
        class_pending_interrupt_p,
        0,
        1,
        false,
    );
    globals.define_builtin_func_with(
        THREAD_CLASS,
        "pending_interrupt?",
        pending_interrupt_p,
        0,
        1,
        false,
    );
}

/// Parse a `{Class => :immediate | :on_blocking | :never}` mask hash.
fn parse_interrupt_mask(
    globals: &mut Globals,
    hash: Value,
) -> Result<Vec<(Value, InterruptTiming)>> {
    let Some(h) = hash.try_hash_ty() else {
        return Err(MonorubyErr::argumenterr("unknown mask signature"));
    };
    let mut mask = vec![];
    let _guard = h.iter_guard();
    for (k, v) in h.iter() {
        if k.is_class_or_module().is_none() {
            return Err(MonorubyErr::typeerr(
                "class or module required for rescue clause",
            ));
        }
        let timing = match v.try_symbol().map(|id| id.get_name()) {
            Some(name) if name == "immediate" => InterruptTiming::Immediate,
            Some(name) if name == "on_blocking" => InterruptTiming::OnBlocking,
            Some(name) if name == "never" => InterruptTiming::Never,
            _ => return Err(MonorubyErr::argumenterr("unknown mask signature")),
        };
        mask.push((k, timing));
    }
    let _ = globals;
    Ok(mask)
}

///
/// ### Thread.handle_interrupt
///
/// - handle_interrupt(hash) { ... } -> object
///
/// Runs the block under an interrupt mask (`Class => :immediate /
/// :on_blocking / :never`). Pending interrupts allowed by the new mask
/// fire on entry; interrupts deferred by the mask fire at block exit
/// (under the outer mask).
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/s/handle_interrupt.html]
#[monoruby_builtin]
fn handle_interrupt(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let mask = parse_interrupt_mask(globals, lfp.arg(0))?;
    let cur = scheduler::current_thread(vm);
    scheduler::push_interrupt_mask(vm, mask);
    // Entry delivery point (non-blocking): a pending interrupt the new
    // mask allows fires before the block runs.
    let res = match scheduler::deliver_pending_now(globals, cur, false) {
        Err(err) => Err(err),
        Ok(()) => vm.invoke_block_once(globals, bh, &[]),
    };
    scheduler::pop_interrupt_mask(vm);
    // Exit delivery point, under the outer mask: interrupts the block's
    // mask deferred fire here — even when the block itself raised, and
    // the delivered interrupt takes precedence over the block's own
    // exception (CRuby).
    scheduler::deliver_pending_now(globals, cur, false)?;
    res
}

fn pending_filter(globals: &Globals, lfp: Lfp) -> Result<Option<Module>> {
    match lfp.try_arg(0) {
        None => Ok(None),
        Some(v) => match v.is_class_or_module() {
            Some(m) => Ok(Some(m)),
            None => {
                let _ = globals;
                Err(MonorubyErr::typeerr(
                    "class or module required for rescue clause",
                ))
            }
        },
    }
}

///
/// ### Thread.pending_interrupt?
///
/// Whether the current thread has queued asynchronous interrupts
/// (optionally filtered by exception class).
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/s/pending_interrupt=3f.html]
#[monoruby_builtin]
fn class_pending_interrupt_p(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let filter = pending_filter(globals, lfp)?;
    let cur = scheduler::current_thread(vm);
    Ok(Value::bool(scheduler::pending_interrupt_p(
        &globals.store,
        cur,
        filter,
    )))
}

///
/// ### Thread#pending_interrupt?
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/i/pending_interrupt=3f.html]
#[monoruby_builtin]
fn pending_interrupt_p(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let filter = pending_filter(globals, lfp)?;
    Ok(Value::bool(scheduler::pending_interrupt_p(
        &globals.store,
        lfp.self_val(),
        filter,
    )))
}

/// Build the exception for an asynchronous `Thread#raise` from its
/// arguments (a simplified `Kernel#raise`): no args -> RuntimeError;
/// exception object (+ optional message override); exception class
/// (+ optional message); a String message.
fn build_async_error(
    vm: &mut Executor,
    globals: &mut Globals,
    args: &[Value],
) -> Result<MonorubyErr> {
    let Some(a0) = args.first().copied() else {
        return Ok(MonorubyErr::runtimeerr("unhandled exception"));
    };
    if let Some(ex) = a0.is_exception() {
        let mut err = MonorubyErr::new_from_exception(ex);
        if let Some(msg) = args.get(1) {
            err.set_msg(msg.coerce_to_str(vm, globals)?);
            return Ok(err);
        }
        return Ok(err.with_original(a0));
    }
    if let Some(klass) = a0.is_class() {
        if klass.is_exception() {
            let cargs: Vec<Value> = args.get(1).copied().into_iter().collect();
            let ex = vm.invoke_method_inner(globals, IdentId::NEW, klass.as_val(), &cargs, None, None)?;
            let err = MonorubyErr::new_from_exception(ex.is_exception().unwrap());
            return Ok(err.with_original(ex));
        }
        return Err(MonorubyErr::typeerr("exception class/object expected"));
    }
    if let Some(msg) = a0.is_rstring() {
        return Ok(MonorubyErr::runtimeerr(msg.to_str()?));
    }
    Err(MonorubyErr::typeerr("exception class/object expected"))
}

///
/// ### Thread#raise
///
/// - raise -> nil
/// - raise(error_type, message = nil) -> nil
///
/// Delivers an exception into the receiver thread: immediately when it
/// targets the current thread, otherwise at the target's park point.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/i/raise.html]
#[monoruby_builtin]
fn thread_raise(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let args = lfp.arg(0).as_array().to_vec();
    let err = build_async_error(vm, globals, &args)?;
    scheduler::interrupt(vm, globals, lfp.self_val(), PendingInterrupt::Raise(err))?;
    Ok(Value::nil())
}

///
/// ### Thread#kill / #exit / #terminate
///
/// Unwinds the receiver thread (running its ensure clauses, uncatchable
/// by rescue) and terminates it as a clean death. Killing the main
/// thread terminates the process.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/i/kill.html]
#[monoruby_builtin]
fn thread_kill(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    scheduler::interrupt(vm, globals, lfp.self_val(), PendingInterrupt::Kill)?;
    Ok(lfp.self_val())
}

///
/// ### Thread.kill(thread)
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/s/kill.html]
#[monoruby_builtin]
fn thread_class_kill(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let target = lfp.arg(0);
    if !target.is_thread() {
        return Err(MonorubyErr::typeerr("wrong argument type (expected VM/thread)"));
    }
    scheduler::interrupt(vm, globals, target, PendingInterrupt::Kill)?;
    Ok(target)
}

///
/// ### Thread.exit
///
/// Kills the current thread.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/s/exit.html]
#[monoruby_builtin]
fn thread_class_exit(vm: &mut Executor, globals: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    let cur = scheduler::current_thread(vm);
    scheduler::interrupt(vm, globals, cur, PendingInterrupt::Kill)?;
    Ok(cur)
}

/// Allocator: an inert shell (never scheduled). Exists for Ruby-level
/// subclasses with their own life cycle — `Thread::Waiter.allocate` from
/// `Process.detach` — not for user `Thread.allocate` + `#run`.
pub(crate) extern "C" fn thread_alloc_func(class_id: ClassId, _: &mut Globals) -> Value {
    Value::new_thread(class_id, ThreadInner::shell())
}

///
/// ### Thread.new
///
/// - new(*args) {|*args| ... } -> Thread
///
/// Creates a green thread running the block, queues it, and immediately
/// yields one time slice so the body starts before `new` returns (up to
/// its first park). CRuby starts a new thread concurrently right away;
/// deferring the first slice to "whenever anything next blocks" leaves a
/// spawned-but-never-started thread behind whenever the spawner never
/// blocks again — the thread then fires much later against torn-down
/// state (ruby/spec's socket files: a stale accept thread from a
/// finished example stole the next example's connection through a reused
/// fd, deadlocking the file).
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/s/new.html]
#[monoruby_builtin]
fn thread_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        Some(bh) => bh,
        None => {
            return Err(MonorubyErr::threaderr(
                &globals.store,
                "must be called with a block",
            ));
        }
    };
    let proc = vm.generate_proc(globals, bh, pc)?;
    let args = lfp.arg(0).as_array().to_vec();
    let class_id = lfp.self_val().as_class_id();
    let thread = Value::new_thread(class_id, ThreadInner::new(proc, args));
    scheduler::spawn(vm, thread);
    // The eager first slice must keep `thread` (and this frame's Values)
    // rooted: `pass` is a scheduler entry (GC-safe park point), and
    // `thread` is reachable via the scheduler registry.
    scheduler::pass(vm, globals)?;
    Ok(thread)
}

///
/// ### Thread.current
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/s/current.html]
#[monoruby_builtin]
fn thread_current(vm: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(scheduler::current_thread(vm))
}

///
/// ### Thread.main
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/s/main.html]
#[monoruby_builtin]
fn thread_main(vm: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(scheduler::main_thread(vm))
}

///
/// ### Thread.pass
///
/// Gives every runnable thread a chance to run, then returns nil.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/s/pass.html]
#[monoruby_builtin]
fn thread_pass(vm: &mut Executor, globals: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    scheduler::pass(vm, globals)?;
    Ok(Value::nil())
}

///
/// ### Thread.list
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/s/list.html]
#[monoruby_builtin]
fn thread_list(vm: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::array_from_vec(scheduler::thread_list(vm)))
}

///
/// ### Thread.stop
///
/// Parks the current thread until another thread wakes it with
/// `#wakeup` / `#run`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/s/stop.html]
#[monoruby_builtin]
fn thread_stop(vm: &mut Executor, globals: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    scheduler::sleep(vm, globals, None)?;
    Ok(Value::nil())
}

fn join_timeout(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Option<std::time::Duration>> {
    match lfp.try_arg(0) {
        None => Ok(None),
        Some(v) if v.is_nil() => Ok(None),
        Some(v) => {
            let secs = v.coerce_to_f64(vm, globals)?;
            if secs.is_nan() || secs < 0.0 {
                return Err(MonorubyErr::argumenterr(
                    "time interval must not be negative or NaN",
                ));
            }
            Ok(Some(std::time::Duration::from_secs_f64(secs)))
        }
    }
}

///
/// ### Thread#join
///
/// - join -> self
/// - join(limit) -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/i/join.html]
#[monoruby_builtin]
fn thread_join(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let timeout = join_timeout(vm, globals, lfp)?;
    let dead = scheduler::join(vm, globals, self_, timeout)?;
    if !dead {
        return Ok(Value::nil());
    }
    // A thread that terminated with an exception re-raises it in the
    // joiner (CRuby).
    if let Some(err) = self_.as_thread_inner().exception.clone() {
        return Err(err);
    }
    Ok(self_)
}

///
/// ### Thread#value
///
/// Joins the thread and returns the body's value, re-raising a
/// terminating exception.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/i/value.html]
#[monoruby_builtin]
fn thread_value(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    scheduler::join(vm, globals, self_, None)?;
    let inner = self_.as_thread_inner();
    if let Some(err) = inner.exception.clone() {
        return Err(err);
    }
    Ok(inner.result.unwrap_or_default())
}

///
/// ### Thread#status
///
/// "run" | "sleep" | false (normal termination) | nil (terminated with
/// exception).
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/i/status.html]
#[monoruby_builtin]
fn thread_status(vm: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let inner = self_.as_thread_inner();
    Ok(match inner.state() {
        ThreadState::Dead => {
            if inner.exception.is_some() {
                Value::nil()
            } else {
                Value::bool(false)
            }
        }
        ThreadState::Sleeping | ThreadState::Joining | ThreadState::IoWaiting => {
            Value::string_from_str("sleep")
        }
        ThreadState::Created | ThreadState::Runnable => {
            // The current thread reports "run"; queued-but-not-yet-run
            // threads also report "run" (CRuby: runnable == "run").
            let _ = vm;
            Value::string_from_str("run")
        }
    })
}

///
/// ### Thread#backtrace
///
/// - backtrace -> [String] | nil
///
/// The target thread's current Ruby backtrace: `nil` for a dead
/// thread, `[]` for one not yet started. For the current thread the
/// live frame chain is walked (like `Kernel#caller`); for a parked
/// thread the frames come from its suspended executor, and for the
/// parked main thread from the scheduler's published main executor.
/// Native frames (e.g. the parked `TCPServer#accept`) render as
/// `<internal>:in 'label'` — mspec's TimeoutAction and specs that
/// match `/in 'accept'/` rely on the label being present.
#[monoruby_builtin]
fn thread_backtrace(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    match self_.as_thread_inner().state() {
        ThreadState::Dead => return Ok(Value::nil()),
        ThreadState::Created => return Ok(Value::array_from_vec(vec![])),
        _ => {}
    }
    let frames = if scheduler::current_thread(vm) == self_ {
        // Skip this builtin's own frame.
        super::kernel::collect_backtrace(globals, vm.cfp(), 1, None, 64, true)
    } else if let Some(exec) = self_.as_thread_inner().resume_exec {
        // SAFETY: one OS thread — a parked thread's executor is
        // quiescent until the scheduler resumes it, so its frame chain
        // cannot change under us.
        let cfp = unsafe { exec.as_ref() }.cfp();
        super::kernel::collect_backtrace(globals, cfp, 0, None, 64, true)
    } else if self_ == scheduler::main_thread(vm)
        && let Some(exec) = scheduler::main_exec_ptr()
    {
        // The parked main thread: its executor is published to the
        // scheduler while green threads run on its behalf.
        // SAFETY: as above — main is parked while a green thread runs.
        let cfp = unsafe { exec.as_ref() }.cfp();
        super::kernel::collect_backtrace(globals, cfp, 0, None, 64, true)
    } else {
        // Runnable-but-not-current (ready queue): the context is mid
        // switch — report no frames rather than guess.
        vec![]
    };
    Ok(Value::array_from_vec(frames))
}

///
/// ### Thread#alive?
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/i/alive=3f.html]
#[monoruby_builtin]
fn thread_alive(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(!lfp.self_val().as_thread_inner().is_dead()))
}

///
/// ### Thread#stop?
///
/// True when the thread is dead or parked.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/i/stop=3f.html]
#[monoruby_builtin]
fn thread_stop_p(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let state = lfp.self_val().as_thread_inner().state();
    Ok(Value::bool(matches!(
        state,
        ThreadState::Dead
            | ThreadState::Sleeping
            | ThreadState::Joining
            | ThreadState::IoWaiting
    )))
}

///
/// ### Thread#wakeup
///
/// Marks a sleeping thread eligible to run. Raises ThreadError on a dead
/// thread.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/i/wakeup.html]
#[monoruby_builtin]
fn thread_wakeup(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    if !scheduler::wakeup(self_) {
        return Err(MonorubyErr::threaderr(
            &globals.store,
            "killed thread",
        ));
    }
    Ok(self_)
}

/// Internal: wake used by the Ruby-level sync primitives in startup.rb.
/// Unlike `#wakeup`, arms the target's park permit when it is running,
/// so a wake racing the target's own park is never lost (see
/// `scheduler::wakeup_permit`).
#[monoruby_builtin]
fn thread_wakeup_permit(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    if !scheduler::wakeup_permit(self_) {
        return Err(MonorubyErr::threaderr(
            &globals.store,
            "killed thread",
        ));
    }
    Ok(self_)
}

///
/// ### Thread#run
///
/// Wakes the thread and gives it (and other runnable threads) a chance
/// to run immediately.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Thread/i/run.html]
#[monoruby_builtin]
fn thread_run(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    if !scheduler::wakeup(self_) {
        return Err(MonorubyErr::threaderr(
            &globals.store,
            "killed thread",
        ));
    }
    scheduler::pass(vm, globals)?;
    Ok(self_)
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn thread_new_join_value() {
        run_test(r#"Thread.new { 42 }.value"#);
        run_test(
            r#"
            counter = [0]
            t = Thread.new { counter[0] += 1 }
            t.value
            counter[0]
            "#,
        );
        run_test_once(
            r#"
            t = Thread.new { 3 * 4 }
            [t.is_a?(Thread), t.value, t.join == t, t.alive?, t.status]
            "#,
        );
        run_test_once(r#"Thread.new(1, 2, 3) { |a, b, c| a + b + c }.value"#);
    }

    #[test]
    fn thread_concurrency_with_queue_like_handoff() {
        // Both threads make progress and join works. The relative order
        // of `:thread` vs. a main-side append *before* the join is
        // deliberately not asserted against CRuby: monoruby's
        // `Thread.new` eagerly runs the new thread's first slice (so
        // fixture-style server threads reach their accept/park before
        // `new` returns), whereas CRuby's GVL usually lets the creator
        // continue first. Sequencing after `join` is identical on both.
        run_test_once(
            r#"
            order = []
            t = Thread.new { order << :thread }
            t.join
            order << :main
            order
            "#,
        );
        // monoruby-specific eager-start guarantee: the body has run to
        // its first blocking point (here: completion) before Thread.new
        // returns.
        let v = run_test_no_result_check(
            r#"
            order = []
            Thread.new { order << :thread }
            order << :main
            order.inspect
            "#,
        );
        assert_eq!("[:thread, :main]", v.as_str());
        // Two threads interleave at sleep points.
        run_test_once(
            r#"
            log = []
            t1 = Thread.new { 3.times { |i| log << [:a, i]; sleep 0.01 } }
            t2 = Thread.new { 3.times { |i| log << [:b, i]; sleep 0.01 } }
            t1.join
            t2.join
            log.sort
            "#,
        );
    }

    #[test]
    fn thread_status_and_pass() {
        run_test_once(
            r#"
            t = Thread.new { sleep }
            Thread.pass while t.status != "sleep"
            st = t.status
            t.wakeup
            t.join
            [st, t.status, t.stop?, t.alive?]
            "#,
        );
        run_test_once(r#"Thread.pass"#);
        run_test_once(r#"[Thread.current == Thread.main, Thread.current.alive?]"#);
        run_test_once(r#"Thread.current.status"#);
    }

    #[test]
    fn thread_backtrace_states() {
        // Live parked thread: an Array whose Ruby frames match CRuby's
        // (block in <main>); dead thread: nil; current thread: contains
        // the calling frame. Native frames render as <internal>:… in
        // monoruby, so compare only CRuby-portable properties.
        run_test_once(
            r#"
            res = []
            t = Thread.new { sleep }
            Thread.pass while t.status != "sleep"
            bt = t.backtrace
            res << bt.class.to_s
            res << bt.any? { |l| l.include?("block in <main>") }
            res << bt.any? { |l| l.include?("sleep") }
            t.kill
            t.join
            res << t.backtrace.inspect
            res << Thread.current.backtrace.any? { |l| l.include?("<main>") }
            res
            "#,
        );
        // The mspec TimeoutAction shape: a watchdog green thread dumps
        // every thread's backtrace, including the parked main thread's.
        run_test_once(
            r#"
            res = nil
            watcher = Thread.new do
              res = Thread.list.map { |th| th.backtrace.class.to_s }
            end
            sleep 0.1
            res
            "#,
        );
    }

    #[test]
    fn thread_exception_propagates_on_join() {
        run_test(
            r#"
            t = Thread.new { raise ArgumentError, "boom" }
            begin
              t.join
              :no_error
            rescue ArgumentError => e
              e.message
            end
            "#,
        );
        run_test(
            r#"
            t = Thread.new { raise "boom" }
            Thread.pass until t.status.nil?
            [t.status, t.alive?]
            "#,
        );
    }

    #[test]
    fn thread_join_timeout() {
        run_test_once(
            r#"
            t = Thread.new { sleep }
            r = t.join(0.05)
            t.wakeup
            t.join
            [r, t.status]
            "#,
        );
    }

    #[test]
    fn thread_join_self_raises() {
        run_test_error(r#"Thread.current.join"#);
        run_test_error(r#"Thread.new"#);
    }

    #[test]
    fn thread_sleep_interleaves_with_main() {
        run_test_once(
            r#"
            v = []
            t = Thread.new { v << 1; sleep 0.05; v << 3 }
            sleep 0.01   # let t run its first leg
            v << 2
            t.join
            v
            "#,
        );
    }

    #[test]
    fn thread_body_bare_return_raises_localjumperror() {
        run_test(
            r#"
            begin
              Thread.new { return }.value
              :no_error
            rescue LocalJumpError
              :local_jump
            end
            "#,
        );
    }

    #[test]
    fn thread_current_and_locals() {
        run_test_once(
            r#"
            Thread.current[:k] = 42
            Thread.current.thread_variable_set(:tv, 7)
            [Thread.current[:k], Thread.current.thread_variable_get(:tv)]
            "#,
        );
    }

    #[test]
    fn thread_blocking_io_yields_to_scheduler() {
        // The copy_stream partial-read pattern: a thread copies into a
        // pipe while the main thread drains it one byte at a time.
        run_test_once(
            r#"
            from_out, from_in = IO.pipe
            to_out, to_in = IO.pipe
            from_in.write "1234"
            from_in.close
            th = Thread.new { IO.copy_stream(from_out, to_in) }
            copied = ""
            4.times { copied += to_out.read(1) }
            th.join
            copied
            "#,
        );
        // A thread parked in gets is fed by main.
        run_test_once(
            r#"
            r, w = IO.pipe
            t = Thread.new { r.gets }
            Thread.pass while t.status != "sleep"
            w.puts "hello"
            t.value
            "#,
        );
        // Main parked in read is fed by a thread.
        run_test_once(
            r#"
            r, w = IO.pipe
            t = Thread.new { sleep 0.02; w.write "x"; w.close }
            v = r.read
            t.join
            v
            "#,
        );
        // IO.select waits without blocking the writer thread.
        run_test_once(
            r#"
            r, w = IO.pipe
            t = Thread.new { sleep 0.02; w.write "z" }
            ready = IO.select([r], nil, nil, 5)
            v = [ready[0][0] == r, r.read(1)]
            t.join
            v
            "#,
        );
        // Two threads ping-pong over a pair of pipes.
        run_test_once(
            r#"
            a_r, a_w = IO.pipe
            b_r, b_w = IO.pipe
            t = Thread.new { 3.times { v = a_r.gets.chomp; b_w.puts (v.to_i + 1).to_s } }
            res = []
            3.times { |i| a_w.puts (i*10).to_s; res << b_r.gets.chomp.to_i }
            t.join
            res
            "#,
        );
    }

    #[test]
    fn thread_blocking_io_midway_yields_to_scheduler() {
        // read(n) needing more bytes than the pipe holds: the reader
        // consumes the first chunk, parks *mid-operation*, and finishes
        // when the rest arrives (entry-only parking would block the
        // process here).
        run_test_once(
            r#"
            r, w = IO.pipe
            t = Thread.new { r.read(6) }
            w.write "abc"
            Thread.pass while t.status != "sleep"
            w.write "def"
            t.value
            "#,
        );
        // gets parking mid-line: the separator arrives after a partial
        // line was already consumed.
        run_test_once(
            r#"
            r, w = IO.pipe
            t = Thread.new { r.gets }
            w.write "hel"
            Thread.pass while t.status != "sleep"
            w.write "lo\n"
            t.value
            "#,
        );
        // Slurping read (no length) across several chunks, then EOF.
        run_test_once(
            r#"
            r, w = IO.pipe
            t = Thread.new { r.read }
            w.write "a" * 100
            Thread.pass while t.status != "sleep"
            w.write "b" * 100
            Thread.pass while t.status != "sleep"
            w.close
            t.value
            "#,
        );
        // A write larger than the pipe capacity: the writer parks
        // mid-write while main drains; nothing is lost or duplicated.
        run_test_once(
            r#"
            r, w = IO.pipe
            data = "xyz" * 70_000
            t = Thread.new { n = w.write(data); w.close; n }
            got = r.read
            [t.value, got.size, got == data]
            "#,
        );
        // readpartial parking mid-operation (needs at least one byte).
        run_test_once(
            r#"
            r, w = IO.pipe
            t = Thread.new { r.readpartial(10) }
            Thread.pass while t.status != "sleep"
            w.write "pq"
            t.value
            "#,
        );
        // An fd left non-blocking by read_nonblock: buffered reads on it
        // must still act blocking (CRuby waits instead of raising EAGAIN),
        // including with no other live threads.
        run_test_once(
            r#"
            r, w = IO.pipe
            a = r.read_nonblock(1, exception: false)
            w.write "hello"
            w.close
            [a, r.read]
            "#,
        );
        // Same, with the wait actually exercised: the reader parks on the
        // non-blocking fd until the writer thread supplies data.
        run_test_once(
            r#"
            r, w = IO.pipe
            a = r.read_nonblock(1, exception: false)
            t = Thread.new { sleep 0.02; w.write "k"; w.close }
            v = r.read
            t.join
            [a, v]
            "#,
        );
        // ... and with NO other live threads (the single-thread poll(2)
        // wait): the data is produced by a child process instead.
        run_test_once(
            r#"
            io = IO.popen("sleep 0.05; echo hi")
            a = io.read_nonblock(1, exception: false)
            v = io.gets
            io.close
            [a, v]
            "#,
        );
        // Writing to a popen'd child stdin past the pipe capacity: the
        // POLLOUT park resolves to the write-side fd (not the fd
        // `fileno` reports), while another thread stays live.
        run_test_once(
            r#"
            io = IO.popen("sleep 0.05; cat > /dev/null", "w")
            t = Thread.new { sleep 0.3 }
            n = io.write("x" * 200_000)
            io.close
            t.join
            n
            "#,
        );
        // gets with a multi-byte custom separator split across chunks
        // (the byte-at-a-time getline loop parks and re-reads pushback).
        run_test_once(
            r#"
            r, w = IO.pipe
            t = Thread.new { r.gets("!!") }
            w.write "ab!"
            Thread.pass while t.status != "sleep"
            w.write "!cd"
            v = t.value
            w.close
            [v, r.read]
            "#,
        );
        // Paragraph-mode gets parking mid-paragraph.
        run_test_once(
            r#"
            r, w = IO.pipe
            t = Thread.new { r.gets("") }
            w.write "para\n"
            Thread.pass while t.status != "sleep"
            w.write "\nnext"
            t.value
            "#,
        );
    }

    #[test]
    fn thread_exception_forwarding_to_main() {
        // SystemExit escaping a thread is re-raised in the main thread
        // (CRuby: `exit` from a thread terminates the process through
        // main). Previously main slept forever and the scheduler's
        // deadlock detector aborted with an uncatchable FatalError —
        // which killed entire mspec runs at core/kernel/exit_spec.rb.
        run_test_once(
            r#"
            r = []
            ready = false
            t = Thread.new {
              Thread.pass until ready
              begin
                exit 42
              rescue SystemExit => e
                r << :in_thread
                raise e
              end
            }
            begin
              ready = true
              sleep
            rescue SystemExit
              r << :in_main
            end
            r << (begin; t.value; rescue SystemExit; :value_raises; end)
            r
            "#,
        );
        // Thread#abort_on_exception: the terminating exception is
        // forwarded to main (net-http's spec fixture server threads set
        // this; the method was missing entirely).
        run_test_once(
            r#"
            t = Thread.new { Thread.current.abort_on_exception = true; sleep 0.02; raise "boom" }
            v = begin
              sleep
            rescue => e
              e.message
            end
            t.join rescue nil
            [v, t.abort_on_exception]
            "#,
        );
        // Thread.abort_on_exception (the global default) does the same.
        run_test_once(
            r#"
            Thread.abort_on_exception = true
            t = Thread.new { sleep 0.02; raise ArgumentError, "global" }
            v = begin
              sleep
            rescue ArgumentError => e
              e.message
            end
            Thread.abort_on_exception = false
            t.join rescue nil
            [v, Thread.abort_on_exception]
            "#,
        );
    }

    #[test]
    fn thread_fd_waiters_not_starved_by_busy_threads() {
        // A busy thread that never blocks (only `Thread.pass`es) keeps the
        // ready queue non-empty forever; fd-parked threads must still be
        // woken when their fd becomes ready (issue #950: the scheduler
        // only polled fds in its idle branch, live-locking this pattern).
        run_test_once(
            r#"
            r, w = IO.pipe
            stop = false
            busy = Thread.new { Thread.pass until stop }
            t = Thread.new { r.read(2) }
            w.write "hi"
            v = t.value
            stop = true
            busy.join
            v
            "#,
        );
        // Main spinning in `Thread.pass while …` never reaches the
        // scheduler's idle branch either; fd waiters must be promoted
        // from the pass path too.
        run_test_once(
            r#"
            r, w = IO.pipe
            t = Thread.new { r.read(2) }
            feeder = Thread.new { w.write "ok" }
            Thread.pass while t.alive?
            feeder.join
            t.value
            "#,
        );
    }

    #[test]
    fn queue_pop_timeout_validation() {
        // `timeout:` must be nil or Numeric. `false` in particular must
        // not be treated as "no timeout" — it used to fall through to the
        // unbounded park branch and block forever (deadlock FatalError in
        // ruby/spec shared/queue/deque.rb).
        run_test_once(
            r#"
            q = Thread::Queue.new
            r = []
            begin; q.pop(timeout: "1"); rescue TypeError => e; r << e.message; end
            begin; q.pop(timeout: false); rescue TypeError => e; r << e.message; end
            begin; q.pop(timeout: true); rescue TypeError => e; r << e.message; end
            begin; q.pop(true, timeout: 1); rescue ArgumentError => e; r << e.message; end
            begin; q.pop(timeout: :sym); rescue TypeError => e; r << e.message; end
            sq = Thread::SizedQueue.new(1)
            begin; sq.push(1, timeout: "1"); rescue TypeError => e; r << e.message; end
            class TimeoutConv; def to_f; 0.001; end; end
            class TimeoutBad; def to_f; "x"; end; end
            begin; q.pop(timeout: TimeoutBad.new); rescue TypeError => e; r << e.message; end
            q << 1
            r << q.pop(timeout: TimeoutConv.new)
            q << 2
            r << q.pop(timeout: 5)
            r
            "#,
        );
    }

    #[test]
    fn native_offload_flock_and_fifo() {
        // Kernel-blocking syscalls must park only the calling green
        // thread, not the process (native worker offload,
        // doc/threads.md §9). Both of these hang the whole
        // interpreter without it.
        run_test_once(
            r#"
            require 'tmpdir'
            path = File.join(Dir.tmpdir, "mrb_flock_test_#{Process.pid}")
            f1 = File.open(path, "w")
            f2 = File.open(path, "w")
            f1.flock(File::LOCK_EX)
            r = []
            t = Thread.new { f2.flock(File::LOCK_EX); r << :locked; f2.flock(File::LOCK_UN); :done }
            sleep 0.05
            r << t.alive?
            r << (r.include?(:locked) ? :early : :blocked_ok)
            f1.flock(File::LOCK_UN)
            r << t.value
            f1.close; f2.close; File.delete(path)
            r
            "#,
        );
        run_test_once(
            r#"
            require 'tmpdir'
            path = File.join(Dir.tmpdir, "mrb_fifo_test_#{Process.pid}")
            File.mkfifo(path)
            r = []
            t = Thread.new { File.open(path, "r") { |io| r << io.read } }
            sleep 0.05
            r << t.alive?
            File.open(path, "w") { |io| io.write "hi" }
            t.join
            File.delete(path)
            r
            "#,
        );
    }

    #[test]
    fn preempt_busy_main_lets_thread_run() {
        // Timeslice preemption: a busy-looping main thread must not
        // starve a runnable green thread forever. This also probes the
        // JIT captured-local protocol: `done` is captured by the thread
        // block, so the loop condition must re-read the (heap) frame
        // slot every iteration — a stale register cache would spin
        // forever. Hangs without preemption.
        run_test(
            r#"
            done = false
            i = 0
            t = Thread.new { done = true }
            until done
              i += 1
            end
            t.join
            [done, i >= 0]
            "#,
        );
        // Float (xmm-cached) loop variant of the same pattern. The
        // do-while form guarantees at least one iteration, so the
        // assertion does not depend on whether the thread got scheduled
        // before the loop started.
        run_test(
            r#"
            stop = false
            t = Thread.new { stop = true }
            sum = 0.0
            begin
              sum += 1.0
            end until stop
            t.join
            [stop, sum >= 1.0]
            "#,
        );
    }

    #[test]
    fn preempt_kill_busy_thread() {
        // `Thread#kill` must reach a thread spinning in a loop that
        // never parks: the kill is delivered at the target's next
        // timeslice boundary. Hangs without preemption (the target
        // never yields, and pre-preemption main's `sleep` was never
        // woken because the busy thread kept the scheduler loop
        // dispatching it).
        run_test_once(
            r#"
            t = Thread.new { loop {} }
            sleep 0.05
            t.kill
            t.join
            [t.alive?, t.status]
            "#,
        );
    }

    #[test]
    fn preempt_sleeper_wakes_under_busy_main() {
        // A green thread's expired `sleep` must be noticed even though
        // main never parks (the pass path promotes due sleepers).
        run_test_once(
            r#"
            r = []
            t = Thread.new { sleep 0.05; r << :woke }
            x = 0
            x += 1 while r.empty?
            t.join
            [r, x >= 0]
            "#,
        );
    }

    #[test]
    fn thread_gc_while_main_parked_inside_fiber() {
        // Main parks *inside a fiber* (`sleep` in an Enumerator body); a
        // green thread then runs and triggers GC. The scheduler used to
        // publish the parking (fiber) executor as `main_exec`, so the GC
        // marked only the fiber's frame chain — every object referenced
        // solely from the main thread's root frames (here `arr`, and the
        // Enumerator itself) was freed and later used: a use-after-free
        // that killed full ruby/spec core runs.
        // `arr` must live in a frame that is NOT on the fiber block's
        // lexical outer chain (the outer chain is marked through the
        // fiber's own frames); only the cfp chain of the main root
        // executor reaches it.
        run_test_once(
            r#"
            def make_enum
              Enumerator.new { |y| sleep 0.05; y << :done }
            end

            def hold_and_wait(e)
              arr = Array.new(64) { |i| [i, i.to_s] }
              v = e.next
              [v, arr.all? { |a| a[0] == a[1].to_i }, arr.size]
            end

            t = Thread.new { 20.times { GC.start; Thread.pass } }
            r = hold_and_wait(make_enum)
            t.join
            r
            "#,
        );
    }

    #[test]
    fn thread_block_param_of_cross_thread_frame() {
        // Reading a `&block` parameter (lazily materialized by monoruby)
        // from inside a green thread: the parameter's lexical home is a
        // heap-promoted frame owned by the *main* thread's frame chain.
        // Used to abort the whole process on `parent_fiber.unwrap()` while
        // searching the green thread's own chain for it (issue #950).
        run_test_once(
            r#"
            def go(&block)
              t = Thread.new { block.call { :x } }
              t.value
            end
            go { |&pause| pause.call }
            "#,
        );
        // The ruby/spec kernel_raise pattern that surfaced it: Thread#raise
        // into a thread parked via a block-provided pause, then join.
        run_test_once(
            r#"
            class BlockRaiser
              def self.raise_in_thread(*args, &block)
                t = Thread.new do
                  Thread.current.report_on_exception = false
                  if block_given?
                    block.call do
                      sleep
                    end
                  else
                    sleep
                  end
                end
                Thread.pass until t.stop?
                t.raise(*args)
                begin
                  t.join
                ensure
                  t.kill if t.alive?
                  Thread.pass while t.alive?
                end
              end
            end
            begin; raise "raised"; rescue => e; end
            begin
              BlockRaiser.raise_in_thread(e) { |&pause| pause.call }
            rescue => reraised
            end
            [reraised.class, reraised == e]
            "#,
        );
    }

    #[test]
    fn thread_handle_interrupt_masking() {
        // Shared driver mirroring ruby/spec's handle_interrupt fixture:
        // raise into a thread inside a handle_interrupt block, observe
        // whether it fires inside (:interrupted) or at exit (:deferred).
        const DRIVER: &str = r#"
            def run_masked(timing, blocking = true)
              klass = Class.new(RuntimeError)
              pad = []
              in_hi = Queue.new
              cont = Queue.new
              th = Thread.new do
                begin
                  Thread.handle_interrupt(klass => timing) do
                    begin
                      in_hi << true
                      if blocking
                        Thread.pass
                        cont.pop
                      else
                        begin
                          cont.pop(true)
                        rescue ThreadError
                          Thread.pass
                          retry
                        end
                      end
                    rescue klass
                      pad << :interrupted
                    end
                  end
                rescue klass
                  pad << :deferred
                end
              end
              in_hi.pop
              Thread.pass while blocking && !th.stop?
              th.raise klass, "interrupt"
              cont << true
              th.join
              pad
            end
        "#;
        run_test_once(&format!("{DRIVER}\nrun_masked(:never)"));
        run_test_once(&format!("{DRIVER}\nrun_masked(:on_blocking)"));
        // Thread.pass is NOT a blocking call: :on_blocking defers when
        // the block never really blocks.
        run_test_once(&format!("{DRIVER}\nrun_masked(:on_blocking, false)"));
        run_test_once(&format!("{DRIVER}\nrun_masked(:immediate)"));
        // Entry delivery: unmasking with pending interrupts fires them
        // before the block runs; pending_interrupt? reflects the queue.
        run_test_once(
            r#"
            Thread.handle_interrupt(RuntimeError => :never) do
              current = Thread.current
              Thread.new { current.raise "interrupt immediate" }.join
              a = Thread.pending_interrupt?
              b = begin
                Thread.handle_interrupt(RuntimeError => :immediate) { :not_reached }
              rescue RuntimeError => e
                e.message
              end
              [a, b, Thread.pending_interrupt?]
            end
            "#,
        );
        // Class-filtered pending_interrupt? and hierarchy matching.
        run_test_once(
            r#"
            Thread.handle_interrupt(StandardError => :never) do
              current = Thread.current
              Thread.new { current.raise ArgumentError, "x" }.join
              r = [Thread.pending_interrupt?, Thread.pending_interrupt?(ArgumentError),
                   Thread.pending_interrupt?(TypeError)]
              begin
                Thread.handle_interrupt(Exception => :immediate) {}
              rescue ArgumentError
                r << :fired
              end
              r
            end rescue nil
            "#,
        );
        // Invalid mask arguments.
        run_test_error(r#"Thread.handle_interrupt(RuntimeError => :sometimes) {}"#);
        run_test_error(r#"Thread.handle_interrupt(1 => :never) {}"#);
    }

    #[test]
    fn thread_io_select_edges() {
        // Timeout expiry with live threads returns nil.
        run_test_once(
            r#"
            r, w = IO.pipe
            t = Thread.new { sleep 0.2 }
            v = IO.select([r], nil, nil, 0.02)
            t.kill; t.join
            v.inspect
            "#,
        );
        // Write-ready set through the green path.
        run_test_once(
            r#"
            r, w = IO.pipe
            t = Thread.new { sleep 0.2 }
            ready = IO.select(nil, [w], nil, 5)
            t.kill; t.join
            ready[1][0] == w
            "#,
        );
        // #to_io conversion of non-IO entries.
        run_test_once(
            r#"
            r, w = IO.pipe
            w.write "x"
            box = Object.new
            box.define_singleton_method(:to_io) { r }
            t = Thread.new { sleep 0.2 }
            ready = IO.select([box], nil, nil, 5)
            t.kill; t.join
            ready[0][0] == box
            "#,
        );
        // All-empty select parks with status "sleep" until killed.
        run_test_once(
            r#"
            t = Thread.new { IO.select(nil, nil, nil, nil) }
            Thread.pass while t.status && t.status != "sleep"
            st = t.status
            t.kill
            t.join
            [st, t.status]
            "#,
        );
        // Enormous timeouts must not overflow (treated as forever).
        run_test_once(
            r#"
            t = Thread.new { IO.select(nil, nil, nil, 2**62) }
            Thread.pass while t.status && t.status != "sleep"
            t.kill
            t.join
            t.status
            "#,
        );
        run_test_once(
            r#"
            t = Thread.new { sleep(2**62) }
            Thread.pass while t.status && t.status != "sleep"
            t.kill
            t.join
            t.status
            "#,
        );
    }

    #[test]
    fn thread_kill_semantics() {
        // kill runs ensure clauses, is not rescuable, dies cleanly.
        run_test_once(
            r#"
            r = []
            t = Thread.new { begin; sleep; rescue Exception => e; r << e.class; ensure; r << :ensure; end }
            Thread.pass while t.status != "sleep"
            t.kill
            t.join
            [r, t.status, t.alive?]
            "#,
        );
        // Killing a just-created thread: whether the body ran before the
        // kill is scheduling-dependent (in CRuby too, and under
        // preemption here), so assert only the convergent facts.
        run_test_once(
            r#"
            t = Thread.new { :body }
            t.kill
            t.join
            [t.status, t.alive?]
            "#,
        );
        // Thread.exit kills the current thread mid-body.
        run_test_once(
            r#"
            r = []
            t = Thread.new { r << 1; Thread.exit; r << 2 }
            t.join
            [r, t.status]
            "#,
        );
        // killing a dead thread is a no-op returning the thread.
        run_test_once(
            r#"
            t = Thread.new { :x }
            t.join
            [t.kill == t, Thread.kill(t) == t]
            "#,
        );
    }

    #[test]
    fn thread_raise_semantics() {
        run_test_once(
            r#"
            t = Thread.new { begin; sleep; rescue ArgumentError => e; e.message; end }
            Thread.pass while t.status != "sleep"
            t.raise(ArgumentError, "boom")
            t.value
            "#,
        );
        // raise with a plain string -> RuntimeError; uncaught -> join re-raises.
        run_test(
            r#"
            t = Thread.new { sleep }
            t.report_on_exception = false
            Thread.pass while t.status != "sleep"
            t.raise("bang")
            begin
              t.join
              :no_error
            rescue RuntimeError => e
              e.message
            end
            "#,
        );
        // kill during ConditionVariable#wait re-locks then unlocks via ensure.
        run_test_once(
            r#"
            m = Mutex.new
            cv = ConditionVariable.new
            t = Thread.new { m.synchronize { cv.wait(m) } }
            Thread.pass while t.status != "sleep"
            t.kill
            t.join
            [t.status, m.locked?]
            "#,
        );
        // raise on the current thread raises in place.
        run_test(
            r#"
            begin
              Thread.current.raise(ArgumentError, "self")
            rescue ArgumentError => e
              e.message
            end
            "#,
        );
    }

    #[test]
    fn mutex_lock_unlock_contention() {
        run_test_once(
            r#"
            m = Mutex.new
            counter = 0
            ts = 4.times.map do
              Thread.new { 50.times { m.synchronize { c = counter; sleep 0.0001; counter = c + 1 } } }
            end
            ts.each(&:join)
            counter
            "#,
        );
        run_test_once(
            r#"
            m = Mutex.new
            r = [m.locked?, m.try_lock, m.locked?, m.owned?, m.try_lock]
            m.unlock
            r << m.locked?
            begin; m.unlock; rescue ThreadError; r << :not_locked; end
            m.lock
            begin; m.lock; rescue ThreadError; r << :recursive; end
            m.unlock
            r
            "#,
        );
        run_test_once(
            r#"
            m = Mutex.new
            slept = false
            t = Thread.new { m.synchronize { m.sleep(0.01); slept = m.owned? } }
            t.join
            [slept, m.locked?]
            "#,
        );
    }

    #[test]
    fn queue_producer_consumer() {
        run_test_once(
            r#"
            q = Queue.new
            prod = Thread.new { 5.times { |i| q.push i }; q.close }
            out = []
            while v = q.pop
              out << v
            end
            prod.join
            [out, q.closed?, q.pop]
            "#,
        );
        run_test_once(
            r#"
            q = Queue.new
            r = [q.pop(timeout: 0.02)]
            begin; q.pop(true); rescue ThreadError; r << :empty; end
            q.push 1
            q.close
            begin; q.push 2; rescue ClosedQueueError; r << :closed; end
            r << q.pop << q.pop
            r
            "#,
        );
        run_test_once(
            r#"
            q = Queue.new
            t = Thread.new { q.pop }
            Thread.pass until q.num_waiting == 1
            q.push :v
            [t.value, q.num_waiting]
            "#,
        );
    }

    #[test]
    fn sized_queue_backpressure() {
        run_test_once(
            r#"
            q = SizedQueue.new(2)
            pushed = 0
            t = Thread.new { 5.times { |i| q.push(i); pushed += 1 } }
            Thread.pass until q.num_waiting == 1
            first = pushed
            3.times { q.pop }
            t.join
            [q.max, first, pushed, q.size]
            "#,
        );
        run_test_once(
            r#"
            begin; SizedQueue.new(0); rescue ArgumentError; :nonpositive; end
            "#,
        );
    }

    #[test]
    fn condition_variable_wait_signal() {
        run_test_once(
            r#"
            m = Mutex.new
            cv = ConditionVariable.new
            log = []
            t = Thread.new do
              m.synchronize { log << :waiting; cv.wait(m); log << :woken; m.owned? }
            end
            Thread.pass until m.synchronize { log.include?(:waiting) }
            m.synchronize { cv.signal }
            [t.value, log]
            "#,
        );
        run_test_once(
            r#"
            m = Mutex.new
            cv = ConditionVariable.new
            woken = 0
            ts = 3.times.map { Thread.new { m.synchronize { cv.wait(m); woken += 1 } } }
            Thread.pass until ts.all? { |t| t.status == "sleep" }
            m.synchronize { cv.broadcast }
            ts.each(&:join)
            woken
            "#,
        );
        run_test_once(
            r#"
            m = Mutex.new
            cv = ConditionVariable.new
            m.synchronize { cv.wait(m, 0.02) }
            :timeout_ok
            "#,
        );
    }

    #[test]
    fn thread_fiber_yield_at_thread_root_errors() {
        // Fiber.yield at a thread root has no parent fiber: FiberError,
        // exactly like at the main fiber.
        run_test(
            r#"
            t = Thread.new { Fiber.yield rescue $!.class.to_s }
            t.value
            "#,
        );
    }

    #[test]
    fn thread_nested_fiber_inside_thread() {
        // A fiber created inside a green thread keeps its own resume
        // chain across scheduler switches.
        // NOTE: resume(arg) with a one-param fiber block hits a
        // pre-existing (master) destructure bug, so this test passes the
        // value through the yield instead.
        run_test_once(
            r#"
            t = Thread.new do
              f = Fiber.new { Fiber.yield 1; :done }
              a = f.resume
              sleep 0.01
              b = f.resume
              [a, b]
            end
            t.value
            "#,
        );
    }
}
