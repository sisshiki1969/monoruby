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
    globals.define_builtin_class_func(klass, "pass", pass, 0);
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
