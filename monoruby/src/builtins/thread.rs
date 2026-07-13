use super::*;

//
// Thread class
//
// monoruby runs Ruby code on a single OS thread and does not provide
// concurrency. `Thread` lives entirely in monoruby/builtins/startup.rb as
// plain Ruby: `Thread.new` stores its block but never runs it, and the
// only kept surface is what RubyGems / Bundler touch (identity,
// thread-/fiber-local storage, a name, and the Mutex / Queue /
// ConditionVariable shells). Blocking / scheduler APIs (#join, #value,
// #kill, Thread.pass, ...) are intentionally absent so they fail fast
// instead of hanging. This module only pre-declares the class so its
// ClassId exists before the Ruby prelude reopens it.

pub(super) fn init(globals: &mut Globals) {
    globals.define_class_under_obj("Thread");
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn thread_new_returns_a_thread() {
        // monoruby is single-threaded: Thread.new stores the block but never
        // runs it, and blocking / observation APIs are intentionally
        // undefined. Only identity and local storage are meaningful.
        run_test_once(
            r#"
            t = Thread.new { 42 }
            [t.is_a?(Thread), Thread.current.is_a?(Thread),
             Thread.current.equal?(Thread.current)]
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
}
