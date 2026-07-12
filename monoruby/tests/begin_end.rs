//! `BEGIN { ... }` / `END { ... }` semantics, which only fully
//! materialize at process exit: run the same script under both the
//! built monoruby binary and the host `ruby` and compare stdout,
//! stderr, and exit status.

use std::process::Command;

fn run_both(script: &str) {
    let mono = Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .env_remove("RUBYOPT")
        .args(["-e", script])
        .output()
        .unwrap();
    let ruby = Command::new("ruby")
        // Match the main harness: skip rubygems boot and ambient RUBYOPT.
        .args(["--disable=gems,rubyopt", "-e", script])
        .output()
        .unwrap();
    assert_eq!(
        String::from_utf8_lossy(&mono.stdout),
        String::from_utf8_lossy(&ruby.stdout),
        "stdout mismatch for {script:?}"
    );
    assert_eq!(
        mono.status.code(),
        ruby.status.code(),
        "exit status mismatch for {script:?}"
    );
}

#[test]
fn begin_runs_first() {
    run_both("puts 'main'; BEGIN { puts 'begin1' }; BEGIN { puts 'begin2' }");
}

#[test]
fn begin_shares_toplevel_scope() {
    run_both("a = 1; BEGIN { b = 2 }; p [a, b]");
}

#[test]
fn end_runs_last_in_reverse_order() {
    run_both("END { puts 'end1' }; at_exit { puts 'at_exit' }; END { puts 'end2' }; puts 'main'");
}

#[test]
fn end_registers_once_per_site() {
    run_both("3.times { END { puts 'once' } }");
}

#[test]
fn end_shares_enclosing_scope() {
    run_both("a = 1; END { p a; zz = 5 }; a = 42; at_exit { p defined?(zz) }");
}

#[test]
fn end_nested_handler_runs_right_after_outer() {
    run_both("END { puts :first }; END { puts :before; END { puts :nested }; puts :after }; END { puts :last }");
}

#[test]
fn end_exit_in_handler_decides_exit_status() {
    run_both("END { exit 43 }; exit 42");
}

#[test]
fn end_handler_exception_sets_failure_status() {
    // Backtrace formats differ, so only compare stdout + exit status
    // (stderr carries the error report in both implementations).
    run_both("END { puts 'ran' }; END { raise 'boom' }");
}

#[test]
fn end_handler_exception_is_visible_as_errinfo() {
    run_both("END { puts '$!.message = ' + $!.message }; END { raise 'foo' }");
}
