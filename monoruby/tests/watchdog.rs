//! Integration coverage for the optional hang watchdog
//! (doc/signal.md B+).
//!
//! The watchdog aborts the *process* via `_exit` from a signal handler,
//! so it can only be exercised by spawning the real binary — not through
//! the in-process `run_test` harness.

use std::process::Command;
use std::time::{Duration, Instant};

fn monoruby() -> Command {
    Command::new(env!("CARGO_BIN_EXE_monoruby"))
}

/// A non-allocating infinite loop never reaches the GC poll point, so an
/// armed watchdog must fire and abort the process — converting a hang
/// into a fast, deterministic failure rather than an opaque timeout.
#[test]
fn watchdog_aborts_non_allocating_hang() {
    let start = Instant::now();
    let output = monoruby()
        .args(["-e", "i = 0; while true; i += 1; end"])
        .env("MONORUBY_HANG_WATCHDOG_SEC", "1")
        .output()
        .expect("failed to spawn monoruby");
    let elapsed = start.elapsed();

    let stderr = String::from_utf8_lossy(&output.stderr);
    // The SIGALRM handler ends the process with `_exit(134)`.
    assert_eq!(
        output.status.code(),
        Some(134),
        "expected watchdog _exit(134), got {:?}\nstderr: {stderr}",
        output.status
    );
    assert!(
        stderr.contains("hang watchdog fired"),
        "expected watchdog message on stderr, got: {stderr}"
    );
    // Budget is 1s; allow generous slack for CI scheduling but ensure it
    // fired promptly rather than spinning indefinitely.
    assert!(
        elapsed < Duration::from_secs(10),
        "watchdog took too long to fire: {elapsed:?}"
    );
}

/// Disabled by default: with the env var unset the watchdog installs
/// nothing, so a terminating program runs to completion silently.
#[test]
fn watchdog_disabled_by_default() {
    let output = monoruby()
        .args(["-e", "x = 0; 1000.times { |k| x += k }; puts x"])
        .output()
        .expect("failed to spawn monoruby");

    assert!(
        output.status.success(),
        "expected clean exit, got {:?}",
        output.status
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.contains("watchdog"),
        "watchdog must stay silent when unset, stderr: {stderr}"
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "499500");
}

/// Armed, but the program finishes within budget: the watchdog must not
/// fire and the process exits normally.
#[test]
fn watchdog_armed_but_program_finishes_in_time() {
    let output = monoruby()
        .args(["-e", "x = 0; 1000.times { |k| x += k }; puts x"])
        .env("MONORUBY_HANG_WATCHDOG_SEC", "5")
        .output()
        .expect("failed to spawn monoruby");

    assert!(
        output.status.success(),
        "expected clean exit, got {:?}\nstderr: {}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "499500");
}
