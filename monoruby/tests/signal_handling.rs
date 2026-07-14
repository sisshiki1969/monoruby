//! Integration coverage for POSIX signal handling (delivery → rescuable
//! SignalException, uncaught → signal-terminated process death). Everything
//! here spawns the real binary: sending signals to `Process.pid` inside the
//! in-process `run_test` harness would signal the shared cargo-test process
//! (signals — and the pending bitmap — are process-wide), letting unrelated
//! test threads steal or be killed by them. Expected values were verified
//! against CRuby 4.0.2.

use std::os::unix::process::ExitStatusExt;
use std::process::{Command, Output};

fn monoruby(script: &str) -> Output {
    Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .arg("-e")
        .arg(script)
        .output()
        .expect("failed to spawn monoruby")
}

fn stdout(out: &Output) -> String {
    String::from_utf8_lossy(&out.stdout).into_owned()
}

#[test]
fn self_signal_raises_rescuable_signal_exception() {
    // The converted SignalException is raised *inside* Process.kill
    // (synchronous drain), carrying signo/signm — `sleep` is never reached.
    let out = monoruby(
        r##"begin
             Process.kill :TERM, Process.pid
             sleep
             puts :not_reached
           rescue SignalException => e
             puts "#{e.class}|#{e.signo}|#{e.signm}"
           end"##,
    );
    assert_eq!(stdout(&out), "SignalException|15|SIGTERM\n");
    assert!(out.status.success());
}

#[test]
fn default_convert_set_matches_cruby() {
    // CRuby's default-convert set: each raises a rescuable SignalException.
    let out = monoruby(
        r##"%i[HUP USR1 USR2 ALRM QUIT].each do |s|
             begin
               Process.kill s, Process.pid
               puts :no_raise
             rescue SignalException => e
               puts e.signm
             end
           end"##,
    );
    assert_eq!(stdout(&out), "SIGHUP\nSIGUSR1\nSIGUSR2\nSIGALRM\nSIGQUIT\n");
    assert!(out.status.success());
}

#[test]
fn uncaught_signal_exception_dies_signal_terminated() {
    // Uncaught SignalException re-raises with SIG_DFL: the process dies *as
    // the signal* (parent sees termsig), silently for plain SignalException.
    let out = monoruby("Process.kill :TERM, Process.pid; sleep");
    assert_eq!(out.status.signal(), Some(libc::SIGTERM));
    assert!(
        out.stderr.is_empty(),
        "plain SignalException dies silently, got: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}

#[test]
fn uncaught_interrupt_reports_then_dies_signal_terminated() {
    // CRuby's reporting rule: an uncaught Interrupt prints the error report
    // before the SIG_DFL re-raise.
    let out = monoruby("Process.kill :INT, Process.pid; sleep");
    assert_eq!(out.status.signal(), Some(libc::SIGINT));
    let err = String::from_utf8_lossy(&out.stderr);
    assert!(err.contains("Interrupt"), "stderr: {err}");
}

#[test]
fn raised_signal_exception_self_signals() {
    // `raise SignalException, 'SIGKILL'` kills the process with SIGKILL
    // (ruby/spec: "self-signals for USR1" / "runs after at_exit").
    let out = monoruby("raise SignalException, 'SIGKILL'");
    assert_eq!(out.status.signal(), Some(libc::SIGKILL));
    let out = monoruby("raise SignalException, 'USR1'");
    assert_eq!(out.status.signal(), Some(libc::SIGUSR1));
}

#[test]
fn trap_beats_default_conversion() {
    // A user Signal.trap handler runs instead of raising SignalException.
    let out = monoruby(
        r##"Signal.trap("TERM") { |signo| puts "trapped #{signo}" }
           Process.kill :TERM, Process.pid
           puts :alive"##,
    );
    assert_eq!(stdout(&out), "trapped 15\nalive\n");
    assert!(out.status.success());
}

#[test]
fn fork_child_signal_death_is_observable() {
    // The parent's $? sees the child's signal death (both delivered and
    // raised), because the child re-raises SIG_DFL instead of exit(1).
    let out = monoruby(
        r##"pid = fork { Process.kill(:TERM, Process.pid); sleep }
           Process.wait(pid)
           a = [$?.signaled?, $?.termsig, $?.exitstatus.inspect]
           pid = fork { raise SignalException, "SIGKILL" }
           Process.wait(pid)
           puts (a + [$?.signaled?, $?.termsig]).join("|")"##,
    );
    assert_eq!(stdout(&out), "true|15|nil|true|9\n");
    assert!(out.status.success());
}

#[test]
fn sleep_is_interrupted_by_signals() {
    // A trap firing mid-sleep interrupts the nanosleep promptly; the whole
    // script must finish far sooner than the requested 30s sleep.
    let start = std::time::Instant::now();
    let out = monoruby(
        r##"Signal.trap("ALRM") { raise "wake" }
           pid = fork { sleep 0.2; Process.kill :ALRM, Process.ppid }
           begin
             sleep 30
             puts :overslept
           rescue RuntimeError => e
             puts e.message
           end
           Process.wait(pid)"##,
    );
    assert_eq!(stdout(&out), "wake\n");
    assert!(out.status.success());
    assert!(
        start.elapsed() < std::time::Duration::from_secs(10),
        "sleep was not interrupted (took {:?})",
        start.elapsed()
    );
}
