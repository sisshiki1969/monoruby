//! Integration coverage for `Signal.trap` / `Kernel#trap`
//! (doc/signal_handling.md A7).
//!
//! These exercise actual signal *delivery*: a script traps a signal,
//! `Process.kill`s its own pid, and the handler runs at the next poll
//! point. Sending a process-wide signal is unsafe under cargo's parallel
//! in-process test threads (a stray delivery could hit a thread whose
//! disposition for that signal is still the default), so — like the
//! watchdog tests — they spawn the real binary for full isolation.

use std::os::unix::process::ExitStatusExt;
use std::process::Command;

fn monoruby() -> Command {
    Command::new(env!("CARGO_BIN_EXE_monoruby"))
}

/// A trapped, self-delivered SIGUSR1 runs the Ruby handler at the next
/// poll point. The allocation loop guarantees the GC poll fires.
#[test]
fn trap_handler_runs_on_self_signal() {
    let output = monoruby()
        .args([
            "-e",
            r#"$ran = false
               Signal.trap("USR1") { $ran = true }
               Process.kill("USR1", Process.pid)
               10000.times { Object.new }
               puts $ran"#,
        ])
        .output()
        .expect("failed to spawn monoruby");

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "expected clean exit, got {:?}\nstderr: {stderr}",
        output.status
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "true");
}

/// The handler is passed the signal number (SIGUSR1 == 10 on Linux).
#[test]
fn trap_handler_receives_signo() {
    let output = monoruby()
        .args([
            "-e",
            r#"Signal.trap("USR1") { |signo| puts signo }
               Process.kill("USR1", Process.pid)
               10000.times { Object.new }"#,
        ])
        .output()
        .expect("failed to spawn monoruby");

    assert!(output.status.success(), "status: {:?}", output.status);
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).trim(),
        "10" // SIGUSR1 on x86-64 Linux
    );
}

/// "IGNORE" installs SIG_IGN: the self-delivered signal neither runs a
/// handler nor terminates the process.
#[test]
fn trap_ignore_swallows_signal() {
    let output = monoruby()
        .args([
            "-e",
            r#"trap("USR1", "IGNORE")
               Process.kill("USR1", Process.pid)
               10000.times { Object.new }
               puts "alive""#,
        ])
        .output()
        .expect("failed to spawn monoruby");

    assert!(output.status.success(), "status: {:?}", output.status);
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "alive");
}

/// Restoring "DEFAULT" on a non-default-installed signal reverts the OS
/// disposition to SIG_DFL, so a subsequent SIGUSR1 terminates the
/// process (the kernel default for SIGUSR1) instead of running anything.
#[test]
fn trap_default_restores_termination() {
    let output = monoruby()
        .args([
            "-e",
            r#"Signal.trap("USR1") { }
               Signal.trap("USR1", "DEFAULT")
               Process.kill("USR1", Process.pid)
               puts "alive""#,
        ])
        .output()
        .expect("failed to spawn monoruby");

    assert!(
        !output.status.success(),
        "expected termination by SIGUSR1, got success"
    );
    assert_eq!(
        output.status.signal(),
        Some(10), // SIGUSR1 on x86-64 Linux
        "expected death by SIGUSR1, status: {:?}",
        output.status
    );
    assert!(String::from_utf8_lossy(&output.stdout).trim().is_empty());
}

/// A handler may be any callable, not just a `Proc`: a `Method` object
/// is accepted and `#call`'d (with the signo) at delivery.
#[test]
fn trap_method_handler_runs() {
    let output = monoruby()
        .args([
            "-e",
            r#"$got = nil
               def handler(signo); $got = signo; end
               Signal.trap("USR1", method(:handler))
               Process.kill("USR1", Process.pid)
               10000.times { Object.new }
               puts $got"#,
        ])
        .output()
        .expect("failed to spawn monoruby");

    assert!(output.status.success(), "status: {:?}", output.status);
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).trim(),
        "10" // SIGUSR1 on x86-64 Linux
    );
}

/// A non-callable handler raises `NoMethodError` at the point of
/// delivery (when the runtime tries `handler.call(signo)`), matching
/// CRuby — not at trap-registration time.
#[test]
fn trap_non_callable_raises_at_delivery() {
    let output = monoruby()
        .args([
            "-e",
            r#"Signal.trap("USR1", Object.new)
               Process.kill("USR1", Process.pid)
               begin
                 10000.times { Object.new }
                 puts "no error"
               rescue NoMethodError
                 puts "nomethod"
               end"#,
        ])
        .output()
        .expect("failed to spawn monoruby");

    assert!(output.status.success(), "status: {:?}", output.status);
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "nomethod");
}

/// SIGINT delivered to a `rescue Interrupt` block lowers to a real
/// `Interrupt` (the runtime default, unchanged by this work).
#[test]
fn sigint_default_raises_interrupt() {
    let output = monoruby()
        .args([
            "-e",
            r#"begin
                 Process.kill("INT", Process.pid)
                 10000.times { Object.new }
                 puts "no interrupt"
               rescue Interrupt
                 puts "interrupt"
               end"#,
        ])
        .output()
        .expect("failed to spawn monoruby");

    assert!(output.status.success(), "status: {:?}", output.status);
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "interrupt");
}
