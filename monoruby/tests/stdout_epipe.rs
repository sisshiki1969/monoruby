//! An `EPIPE` on a write to `STDOUT` ends the process by `SIGPIPE`
//! (CRuby's `FMODE_SIGNAL_ON_EPIPE`, https://bugs.ruby-lang.org/issues/14413,
//! issue #1853): the write still raises an ordinary, rescuable
//! `Errno::EPIPE`, but one that goes uncaught kills the process silently
//! with the signal, so `ruby -e 'loop { puts 1 }' | head -1` exits quietly.
//! Other streams keep the plain error. Each case spawns the real binary
//! with its stdout on a pipe, reads the first line and closes the read
//! end. Expected values were verified against CRuby 4.0.9.

use std::io::{BufRead, BufReader};
use std::os::unix::process::ExitStatusExt;
use std::process::{Command, Output, Stdio};

/// Run `script`, read one line of its stdout, close the pipe, and wait.
fn run_until_reader_leaves(script: &str) -> (String, Output) {
    let mut child = Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .arg("--disable=gems")
        .arg("-e")
        .arg(script)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn monoruby");
    let mut line = String::new();
    {
        let stdout = child.stdout.take().unwrap();
        BufReader::new(stdout).read_line(&mut line).unwrap();
        // Dropping the reader closes the pipe's last read end.
    }
    (line, child.wait_with_output().unwrap())
}

fn stderr(out: &Output) -> String {
    String::from_utf8_lossy(&out.stderr).into_owned()
}

#[track_caller]
fn assert_sigpipe_death(out: &Output) {
    assert_eq!(
        out.status.signal(),
        Some(libc::SIGPIPE),
        "expected a SIGPIPE death, got {:?}; stderr: {}",
        out.status,
        stderr(out)
    );
}

#[test]
fn uncaught_epipe_on_stdout_dies_by_sigpipe_silently() {
    let (line, out) = run_until_reader_leaves("loop { puts :ok }");
    assert_eq!(line, "ok\n");
    assert_sigpipe_death(&out);
    assert_eq!(stderr(&out), "");
}

#[test]
fn every_stdout_write_path_dies_by_sigpipe() {
    for script in [
        // No Ruby-level rescue clause (as in `Kernel#loop`) between the
        // write and the top level: the error reaches the exit path before
        // it was ever materialized.
        "while true; puts :ok; end",
        "loop { print \"ok\\n\" }",
        "loop { p :ok }",
        "loop { $stdout << \"ok\\n\" }",
        "loop { $stdout.write \"ok\\n\"; $stdout.flush }",
        "STDOUT.sync = false; loop { STDOUT.print \"ok\\n\" }",
        // `#dup` keeps FMODE_SIGNAL_ON_EPIPE (`rb_io_init_copy`).
        "o = STDOUT.dup; loop { o.puts :ok }",
        // A path reopen without a mode keeps the stream's FMODE bits.
        "STDOUT.reopen(\"/dev/stdout\"); loop { STDOUT.puts :ok }",
    ] {
        let (_, out) = run_until_reader_leaves(script);
        assert_sigpipe_death(&out);
        assert_eq!(stderr(&out), "", "{script}");
    }
}

#[test]
fn the_epipe_is_an_ordinary_rescuable_errno() {
    let (_, out) = run_until_reader_leaves(
        "begin; loop { puts :ok }; rescue => e; STDERR.puts e.class, e.instance_variables.inspect; end",
    );
    assert_eq!(out.status.code(), Some(0));
    assert_eq!(stderr(&out), "Errno::EPIPE\n[]\n");
}

#[test]
fn reraising_the_rescued_error_still_dies_by_sigpipe() {
    // The same object carries the signal (CRuby's hidden `signo` ivar)...
    let (_, out) = run_until_reader_leaves("begin; loop { puts :ok }; rescue => e; raise e; end");
    assert_sigpipe_death(&out);
    assert_eq!(stderr(&out), "");
    // ...and so does a copy of it.
    let (_, out) =
        run_until_reader_leaves("begin; loop { puts :ok }; rescue => e; raise e.dup; end");
    assert_sigpipe_death(&out);
    // A fresh Errno::EPIPE does not.
    let (_, out) =
        run_until_reader_leaves("begin; loop { puts :ok }; rescue => e; raise e.class, 'x'; end");
    assert_eq!(out.status.code(), Some(1));
    assert!(
        stderr(&out).contains("Broken pipe - x (Errno::EPIPE)"),
        "{}",
        stderr(&out)
    );
}

#[test]
fn at_exit_handlers_run_before_the_signal_death() {
    let (_, out) = run_until_reader_leaves("at_exit { STDERR.puts :at_exit }; loop { puts :ok }");
    assert_sigpipe_death(&out);
    assert_eq!(stderr(&out), "at_exit\n");
}

#[test]
fn a_thread_error_joined_into_main_dies_by_sigpipe() {
    // The thread's own death is reported (report_on_exception), then the
    // join hands the same error to the main thread.
    let (_, out) = run_until_reader_leaves("Thread.new { loop { puts :ok } }.join");
    assert_sigpipe_death(&out);
    assert!(
        stderr(&out).contains("terminated with exception"),
        "{}",
        stderr(&out)
    );
}

#[test]
fn a_forked_child_dies_by_sigpipe() {
    for script in [
        "pid = fork { loop { puts :ok } }; Process.wait pid; STDERR.puts $?.termsig",
        "pid = fork { while true; puts :ok; end }; Process.wait pid; STDERR.puts $?.termsig",
    ] {
        let (_, out) = run_until_reader_leaves(script);
        assert_eq!(out.status.code(), Some(0), "{script}");
        assert_eq!(stderr(&out), format!("{}\n", libc::SIGPIPE), "{script}");
    }
}

#[test]
fn other_writes_keep_the_plain_errno() {
    for script in [
        // Not STDOUT.
        "r, w = IO.pipe; r.close; loop { w.puts :ok }",
        // `reopen(io)` takes the other stream's mode, without the flag.
        "r, w = IO.pipe; STDOUT.reopen(w); r.close; loop { STDOUT.puts :ok }",
        // An explicit reopen mode replaces it.
        "STDOUT.reopen(\"/dev/stdout\", \"w\"); loop { STDOUT.puts :ok }",
        // `syswrite` is `rb_sys_fail_path`, `write_nonblock` likewise.
        "loop { STDOUT.syswrite \"ok\\n\" }",
        "loop { STDOUT.write_nonblock \"ok\\n\" }",
    ] {
        let (_, out) = run_until_reader_leaves(script);
        assert_eq!(out.status.code(), Some(1), "{script}: {:?}", out.status);
        assert!(
            stderr(&out).contains("(Errno::EPIPE)"),
            "{script}: {}",
            stderr(&out)
        );
    }
}
