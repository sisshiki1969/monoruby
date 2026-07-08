use super::*;

//
// Thread class
//
// monoruby runs Ruby code on a single OS thread. The bulk of `Thread`
// lives in monoruby/builtins/startup.rb as plain Ruby — `Thread.new`
// defers its block and runs it lazily on `#value` / `#join`. Only the
// pieces that need a real syscall live here; the Ruby side reopens this
// class.

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_class_under_obj("Thread").id();
    // `Thread.pass` is reopened in startup.rb to first run a pending
    // deferred thread (cooperative scheduling); it delegates the actual
    // CPU-yield hint to this native helper.
    globals.define_builtin_class_func(klass, "__native_yield", pass, 0);
    globals.define_builtin_func(klass, "__invoke_body", invoke_body, 2);
}

///
/// ### Thread#__invoke_body(block, args)
///
/// Runs a thread body block, translating a top-level non-local `return`
/// into a `LocalJumpError`. Used by `Thread#__run` in startup.rb.
///
/// CRuby runs each thread on its own stack, so a `return` written directly
/// in a `Thread.new { ... }` block has no enclosing method frame to return
/// to and raises `LocalJumpError: unexpected return`. monoruby is
/// single-threaded and runs thread bodies synchronously on the caller's
/// stack (see `Thread#__run`), so the block's home frame is still live and
/// the `return` would otherwise perform a real non-local return, escaping
/// the thread body entirely (and, under mspec, corrupting the harness).
/// Catch that escaping `MethodReturn` here and convert it, matching
/// CRuby's observable behavior for the common (uncaught) case.
#[monoruby_builtin]
fn invoke_body(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let block = Proc::new(lfp.arg(0));
    let args = lfp.arg(1).as_array();
    match vm.invoke_proc(globals, &block, &args) {
        Ok(v) => Ok(v),
        Err(err) if matches!(err.kind(), MonorubyErrKind::MethodReturn(..)) => {
            Err(MonorubyErr::localjumperr("unexpected return"))
        }
        Err(err) => Err(err),
    }
}

///
/// ### Thread.pass
///
/// - pass -> nil
///
/// Give the OS scheduler a hint that the current thread is willing to
/// yield the CPU. monoruby has only one runnable OS thread, so there is
/// no in-process thread to switch to; this lowers to `sched_yield(2)`
/// (via `std::thread::yield_now`), which is a cheap, legitimate no-op
/// when nothing else is runnable. Always returns nil.
///
/// Previously `Thread.pass` was a pure-Ruby counter that raised
/// `ThreadError` after 1000 calls to break `Thread.pass until cond`
/// busy-waits. That guard aborted innocent subprocesses (e.g. mspec's
/// `ruby_exe`) and hung the parent on its pipe; see
/// doc/signal_handling.md (B3). Genuine single-thread blocking is
/// surfaced at the blocking call sites instead (B1).
///
/// [https://docs.ruby-lang.org/en/master/Thread.html#method-c-pass]
#[monoruby_builtin]
fn pass(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    std::thread::yield_now();
    Ok(Value::nil())
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn thread_pass_returns_nil() {
        run_test("Thread.pass");
        run_test("Thread.pass.nil?");
    }

    #[test]
    fn thread_body_bare_return_raises_localjumperror() {
        // A bare top-level `return` in a thread body is a LocalJumpError in
        // CRuby (the thread has no enclosing method frame). monoruby runs the
        // body synchronously, so without the `__invoke_body` conversion the
        // `return` would escape the block entirely. Observe it at `#value`.
        run_test(
            r#"
            begin
              Thread.new { return }.value
              :no_error
            rescue LocalJumpError => e
              e.message
            end
            "#,
        );
        run_test(
            r#"
            begin
              Thread.new { return 5 }.value
              :no_error
            rescue LocalJumpError
              :local_jump
            end
            "#,
        );
    }

    #[test]
    fn thread_body_normal_paths_unaffected() {
        // The return-conversion wrapper must not disturb ordinary thread
        // bodies: plain values, arguments, and normal exceptions.
        run_test("Thread.new { 40 + 2 }.value");
        run_test("Thread.new(3, 4) { |a, b| a * b }.value");
        run_test(
            r#"
            begin
              Thread.new { raise "boom" }.value
            rescue => e
              e.message
            end
            "#,
        );
    }

    #[test]
    fn thread_pass_drives_deferred_thread_bodies() {
        // `Thread.new` runs its block lazily, so a main-thread busy-wait on a
        // flag set by a thread body must still make progress: `Thread.pass`
        // runs the oldest pending thread. Without this the loop would hang.
        run_test_once(
            r#"
            running = false
            thr = Thread.new { running = true }
            Thread.pass until running
            thr.join
            running
            "#,
        );
        // Multiple pending threads are drained oldest-first across passes.
        run_test_once(
            r#"
            a = []
            t1 = Thread.new { a << 1 }
            t2 = Thread.new { a << 2 }
            Thread.pass until a.size == 2
            t1.join
            t2.join
            a.sort
            "#,
        );
        // A thread run directly via #value is not re-run by a later pass.
        run_test_once(
            r#"
            n = 0
            t = Thread.new { n += 1 }
            t.value
            Thread.pass
            n
            "#,
        );
    }

    #[test]
    fn thread_pass_does_not_raise_when_called_repeatedly() {
        // The old pure-Ruby guard raised ThreadError after 1000 calls.
        // A real sched_yield wrapper never raises (B3 in
        // doc/signal_handling.md).
        run_test_once(
            r#"
            2000.times { Thread.pass }
            :ok
            "#,
        );
    }
}
