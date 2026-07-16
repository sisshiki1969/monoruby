use super::*;
use crate::scheduler;
use crate::value::rvalue::{ThreadInner, ThreadState};

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
        ThreadState::Sleeping | ThreadState::Joining => Value::string_from_str("sleep"),
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
        ThreadState::Dead | ThreadState::Sleeping | ThreadState::Joining
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
