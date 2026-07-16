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
    globals.define_builtin_func(THREAD_CLASS, "alive?", thread_alive, 0);
    globals.define_builtin_func(THREAD_CLASS, "stop?", thread_stop_p, 0);
    globals.define_builtin_func(THREAD_CLASS, "wakeup", thread_wakeup, 0);
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
/// Creates a green thread running the block and queues it; the body gets
/// its first time slice at the next blocking point of any thread.
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
        // The spawned thread must not run at Thread.new time; it runs
        // when the main thread blocks (join), and both make progress.
        run_test_once(
            r#"
            order = []
            t = Thread.new { order << :thread }
            order << :main
            t.join
            order
            "#,
        );
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
        // killing an unstarted thread never runs the body.
        run_test_once(
            r#"
            ran = false
            t = Thread.new { ran = true }
            t.kill
            t.join
            [ran, t.status]
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
