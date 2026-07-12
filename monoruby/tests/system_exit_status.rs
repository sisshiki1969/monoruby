//! `SystemExit` (and subclasses) propagate their exit code to the
//! process when raised — a process-boundary behavior, so these spawn the
//! real binary and inspect the exit status.

use std::process::Command;

fn exit_code(script: &str) -> Option<i32> {
    Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .env_remove("RUBYOPT")
        .args(["-e", script])
        .output()
        .expect("spawn monoruby")
        .status
        .code()
}

#[test]
fn system_exit_new_status() {
    assert_eq!(exit_code("raise SystemExit.new(7)"), Some(7));
    assert_eq!(exit_code("raise SystemExit.new(0)"), Some(0));
    assert_eq!(exit_code("raise SystemExit.new(true)"), Some(0));
    assert_eq!(exit_code("raise SystemExit.new(false)"), Some(1));
}

#[test]
fn system_exit_subclass_status() {
    assert_eq!(
        exit_code("class CustomExit < SystemExit; end; raise CustomExit.new(8)"),
        Some(8)
    );
}

#[test]
fn kernel_exit_and_plain_raise() {
    assert_eq!(exit_code("exit 3"), Some(3));
    assert_eq!(exit_code("exit"), Some(0));
    // A plain RuntimeError still exits 1.
    assert_eq!(exit_code("raise 'boom'"), Some(1));
}
