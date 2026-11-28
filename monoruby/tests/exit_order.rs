//! Process exit order when threads are still running: the `at_exit`
//! handlers, then the remaining threads' ensure clauses
//! (`scheduler::terminate_all`), then the `ObjectSpace` finalizers —
//! CRuby's `rb_ec_teardown` → `rb_thread_terminate_all` →
//! `rb_objspace_call_finalizer`. Spawns the real binary, because the
//! in-process harness never runs the exit sequence. CRuby 4.0.9 prints
//! exactly the expected string for this script.

use std::process::Command;

#[test]
fn at_exit_then_thread_ensure_then_finalizers() {
    let script = r#"
        t = Thread.new do
          sleep
        ensure
          $stdout.syswrite("thread ")
        end
        ObjectSpace.define_finalizer(Object.new, proc { $stdout.syswrite("finalizer ") })
        at_exit { $stdout.syswrite("at_exit ") }
        Thread.pass until t.status == "sleep"
    "#;
    let out = Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .args(["--disable=gems", "-e", script])
        .output()
        .expect("spawn monoruby");
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "at_exit thread finalizer "
    );
}

/// A thread's ensure clause run by `terminate_all` redefines a basic op
/// while main waits for it: the redefinition converts the compiled frames
/// of every suspended thread, main's through the root executor it
/// published (`scheduler::suspended_executors`). CRuby 4.0.9 prints
/// exactly the expected string.
#[test]
fn bop_redefined_in_an_ensure_at_exit() {
    let script = r##"
        def f(x) = x + 1
        30.times { f(1) }
        t = Thread.new do
          sleep
        ensure
          class Integer
            def +(o) = 42
          end
          $stdout.syswrite("#{f(1)} ")
        end
        at_exit { $stdout.syswrite("at_exit ") }
        Thread.pass until t.status == "sleep"
    "##;
    let out = Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .args(["--disable=gems", "-e", script])
        .output()
        .expect("spawn monoruby");
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&out.stdout), "at_exit 42 ");
}
