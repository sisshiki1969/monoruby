use super::*;

//
// Thread class
//
// monoruby runs Ruby code on a single OS thread and does not provide
// concurrency. Most of `Thread` lives in monoruby/builtins/startup.rb as
// plain Ruby: `Thread.new` stores its block and runs it lazily on `#value`,
// on the main thread. The kept surface is what RubyGems / Bundler touch
// plus `#value`; the hang-prone waiting/observation APIs (#join, #status,
// Thread.pass, ...) are intentionally absent so they fail fast instead of
// hanging. Only the one piece that needs native support lives here:
// `__invoke_body`, which runs a thread body with CRuby-compatible
// return/break and thread-local `$~`/`$_` semantics.

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_class_under_obj("Thread").id();
    globals.define_builtin_func(klass, "__invoke_body", invoke_body, 2);
}

///
/// ### Thread#__invoke_body(block, args)
///
/// Runs a thread body block. Used by `Thread#__run` in startup.rb. Emulates
/// two thread semantics that monoruby's synchronous, single-threaded model
/// would otherwise break:
///
/// 1. **`return` / `break` → `LocalJumpError`.** CRuby runs each thread on
///    its own stack, so a `return` (or `break`) written directly in a
///    `Thread.new { ... }` block has no enclosing frame to jump to and
///    raises `LocalJumpError`. monoruby runs the body synchronously on the
///    caller's stack, so the block's home frame is still live and the jump
///    would otherwise escape the body. Catch the escaping `MethodReturn` /
///    `BlockBreak` here and convert it.
///
/// 2. **Thread-local `$~` / `$_`.** These special variables are frame-local,
///    stored on the block's lexical home method frame — which, run
///    synchronously, is shared with the code that spawned the thread. Save
///    that frame's svar container, run the body against a fresh (empty) one,
///    and restore it, so the body's `$~` / `$_` neither leak out to nor
///    inherit from the spawning thread (matching CRuby's thread-local
///    semantics).
#[monoruby_builtin]
fn invoke_body(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let block = Proc::new(lfp.arg(0));
    let args = lfp.arg(1).as_array();

    // Isolate the block's home method frame's `$~`/`$_` container for the
    // duration of the body (item 2 above). The saved container is kept
    // reachable for the GC via the temp stack while it is off the frame.
    let home_mfp = block.outer_lfp().map(|o| o.mfp());
    let saved = home_mfp.and_then(|m| m.svar_slot_value());
    if let Some(m) = home_mfp {
        if let Some(s) = saved {
            vm.temp_push(s);
        }
        m.restore_svar_slot(None);
    }

    vm.push_break_barrier(vm.cfp());
    let result = vm.invoke_proc(globals, &block, &args);
    vm.pop_break_barrier();

    if let Some(m) = home_mfp {
        m.restore_svar_slot(saved);
        if saved.is_some() {
            vm.temp_pop();
        }
    }

    match result {
        Ok(v) => Ok(v),
        Err(err) if matches!(err.kind(), MonorubyErrKind::MethodReturn(..)) => {
            Err(MonorubyErr::localjumperr("unexpected return"))
        }
        Err(err) if matches!(err.kind(), MonorubyErrKind::BlockBreak(..)) => {
            Err(MonorubyErr::localjumperr("break from proc-closure"))
        }
        Err(err) => Err(err),
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn thread_value_runs_block_lazily() {
        // monoruby is single-threaded: Thread.new stores the block and runs
        // it lazily on #value, on the main thread.
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
            [t.is_a?(Thread), t.value]
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
    fn thread_body_bare_return_raises_localjumperror() {
        // A bare top-level `return` in a thread body is a LocalJumpError in
        // CRuby. monoruby runs the body synchronously on #value, so the
        // `__invoke_body` conversion must reproduce that.
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
        // A bare `break` in a thread body is likewise a LocalJumpError.
        run_test(
            r#"
            begin
              Thread.new { break :b }.value
              :no_error
            rescue LocalJumpError
              :local_jump
            end
            "#,
        );
    }
}
