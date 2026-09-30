//! Process exit order when threads are still running: the `at_exit`
//! handlers, then the remaining threads' ensure clauses
//! (`scheduler::terminate_all`), then the `ObjectSpace` finalizers —
//! CRuby's `rb_ec_teardown` → `rb_thread_terminate_all` →
//! `rb_objspace_call_finalizer`. Spawns the real binary, because the
//! in-process harness never runs the exit sequence. CRuby 4.0.6 prints
//! exactly the expected string for this script.

use std::process::Command;

fn run(model: &str) -> String {
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
        .env("MONORUBY_THREAD_MODEL", model)
        .args(["--disable=gems", "-e", script])
        .output()
        .expect("spawn monoruby");
    assert!(
        out.status.success(),
        "{model}: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).into_owned()
}

#[test]
fn at_exit_then_thread_ensure_then_finalizers_green() {
    assert_eq!(run("green"), "at_exit thread finalizer ");
}

#[test]
fn at_exit_then_thread_ensure_then_finalizers_native() {
    assert_eq!(run("native"), "at_exit thread finalizer ");
}
