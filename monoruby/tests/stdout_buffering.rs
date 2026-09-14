//! Integration coverage for `$stdout`'s buffering policy (issue #1349).
//!
//! CRuby buffers stdout unless it is a TTY or `sync` is set; `puts` is
//! ordinary buffered output (`rb_io_puts` writes and nothing more), while
//! `p` flushes when it is done (`rb_f_p`). What forces the buffer out
//! besides a full buffer, `#flush` and exit is a fork: the child would
//! otherwise inherit a copy of it.
//!
//! Each case runs with stdout on a real file and has the program report,
//! on stderr, how much of its output has reached that file — spawning the
//! binary is the only way to get a non-TTY stdout that can be measured
//! from inside the program.

use std::process::{Command, Stdio};

fn monoruby() -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_monoruby"));
    cmd.env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .env_remove("RUBYPATH");
    cmd.arg("--disable=gems");
    cmd
}

fn out_path(name: &str) -> std::path::PathBuf {
    let path = std::env::temp_dir().join(format!("monoruby_stdout_buffering_{name}.txt"));
    let _ = std::fs::remove_file(&path);
    path
}

/// Run `code` with stdout on a fresh file; return (what it printed on
/// stderr, the file's final contents).
fn run(name: &str, code: &str) -> (String, String) {
    let path = out_path(name);
    let file = std::fs::File::create(&path).expect("create stdout file");
    let out = monoruby()
        .arg("-e")
        .arg(code)
        .stdout(Stdio::from(file))
        .stderr(Stdio::piped())
        .output()
        .expect("spawn monoruby");
    assert!(
        out.status.success(),
        "exited with {:?}\nstderr: {}",
        out.status,
        String::from_utf8_lossy(&out.stderr)
    );
    let written = std::fs::read_to_string(&path).expect("read stdout file");
    let _ = std::fs::remove_file(&path);
    (String::from_utf8_lossy(&out.stderr).into_owned(), written)
}

/// Buffered or not, everything written arrives by exit.
#[test]
fn buffered_output_all_arrives_by_exit() {
    let (_, written) = run("exit", r#"puts "x"; $stdout.puts "y"; print "z\n""#);
    assert_eq!(written, "x\ny\nz\n");
}

/// `puts` leaves its bytes in the buffer on a non-TTY stdout; `#flush`
/// and `p` push them out. The program reads the size of its own
/// redirected stdout, so the buffer boundary is observed from inside.
#[test]
fn puts_does_not_reach_the_file_before_flush() {
    let path = out_path("probe");
    let file = std::fs::File::create(&path).expect("create stdout file");
    let code = format!(
        r#"
        path = {path:?}
        puts "x"
        STDERR.print "after_puts=#{{File.size(path)}} "
        $stdout.flush
        STDERR.print "after_flush=#{{File.size(path)}} "
        p 1
        STDERR.print "after_p=#{{File.size(path)}}"
        "#,
        path = path.to_str().unwrap()
    );
    let out = monoruby()
        .arg("-e")
        .arg(code)
        .stdout(Stdio::from(file))
        .stderr(Stdio::piped())
        .output()
        .expect("spawn monoruby");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        out.status.success(),
        "exited with {:?}\nstderr: {stderr}",
        out.status
    );
    // `puts` leaves the bytes in the buffer; `#flush` and `p` push them out.
    assert_eq!(stderr, "after_puts=0 after_flush=2 after_p=4");
    let _ = std::fs::remove_file(&path);
}

/// A child inheriting our stdout must not see our output land after its
/// own: the fork flushes first.
#[test]
fn a_spawned_child_writes_after_our_buffered_output() {
    let (_, written) = run(
        "system",
        r#"print "a"; system("echo", "b"); print "c\n""#,
    );
    assert_eq!(written, "ab\nc\n");
}

/// ... and must not inherit a copy of the buffer, which would print it
/// twice.
#[test]
fn a_forked_child_does_not_reprint_the_buffer() {
    let (_, written) = run(
        "fork",
        r#"print "a"; if fork.nil? then exit end; Process.wait; print "c\n""#,
    );
    assert_eq!(written, "ac\n");
}
