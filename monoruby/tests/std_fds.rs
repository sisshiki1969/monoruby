//! Integration coverage for how the interpreter normalizes its standard
//! fds at startup (`fill_closed_std_fds`, `src/executor.rs`).
//!
//! A std fd that arrived CLOSED is reopened CRuby-style — stdin on
//! `/dev/null`, stdout/stderr on the write end of a pipe whose read end
//! is closed, so a write raises `Errno::EPIPE` rather than landing in
//! whatever the interpreter later opened onto the freed slot. An fd that
//! is genuinely *open* must be left alone, whatever it points at and
//! whatever its access mode: `/dev/null` opened read-write is what a
//! shell's `1<>/dev/null` and Python's `subprocess.DEVNULL` hand over,
//! and poisoning those made `puts` raise EPIPE (issue #1342).

use std::fs::OpenOptions;
use std::process::{Command, Stdio};

fn monoruby() -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_monoruby"));
    cmd.env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .env_remove("RUBYPATH");
    cmd.arg("--disable=gems");
    cmd
}

fn devnull_rdwr() -> std::fs::File {
    OpenOptions::new()
        .read(true)
        .write(true)
        .open("/dev/null")
        .expect("open /dev/null read-write")
}

/// `puts` to a stdout that is `/dev/null` opened O_RDWR must succeed.
#[test]
fn stdout_on_a_read_write_devnull_is_left_alone() {
    let out = monoruby()
        .arg("-e")
        .arg(r#"puts "hello"; STDERR.print "ran""#)
        .stdout(Stdio::from(devnull_rdwr()))
        .stderr(Stdio::piped())
        .output()
        .expect("spawn monoruby");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        out.status.success() && stderr == "ran",
        "exited with {:?}, stderr: {stderr}",
        out.status
    );
}

/// Both streams on a read-write `/dev/null` — the shape
/// `subprocess.DEVNULL` produces. A poisoned stdout used to raise, and
/// reporting *that* on an equally poisoned stderr aborted the process.
#[test]
fn stdout_and_stderr_on_a_read_write_devnull_do_not_abort() {
    let status = monoruby()
        .arg("-e")
        .arg(r#"puts "hello"; STDERR.puts "warn""#)
        .stdout(Stdio::from(devnull_rdwr()))
        .stderr(Stdio::from(devnull_rdwr()))
        .status()
        .expect("spawn monoruby");
    assert!(status.success(), "exited with {status:?}");
}

/// The same access mode on an ordinary file: written through, not poisoned.
#[test]
fn stdout_on_a_read_write_file_is_left_alone() {
    let path = std::env::temp_dir().join("monoruby_std_fds_rdwr.txt");
    let _ = std::fs::remove_file(&path);
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(true)
        .open(&path)
        .expect("open temp file read-write");
    let status = monoruby()
        .arg("-e")
        .arg(r#"puts "hello""#)
        .stdout(Stdio::from(file))
        .status()
        .expect("spawn monoruby");
    assert!(status.success(), "exited with {status:?}");
    assert_eq!(std::fs::read_to_string(&path).unwrap(), "hello\n");
    let _ = std::fs::remove_file(&path);
}

/// A stdout that really *was* closed at exec still gets CRuby's broken
/// pipe — the repair this all exists for. Rust's runtime reopens such a
/// slot on `/dev/null` O_RDWR before `main`, so what distinguishes this
/// from the cases above is sampled by a pre-`main` constructor.
#[test]
fn a_closed_stdout_becomes_a_broken_pipe() {
    use std::os::unix::process::CommandExt;
    let mut cmd = monoruby();
    cmd.arg("-e")
        .arg(r#"STDERR.print $stdout.stat.pipe?"#)
        .stderr(Stdio::piped());
    // SAFETY: `close` on the child's own fd between fork and exec; async
    // -signal-safe, and touches nothing the parent shares.
    unsafe {
        cmd.pre_exec(|| {
            libc::close(1);
            Ok(())
        });
    }
    let out = cmd.output().expect("spawn monoruby");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        out.status.success() && stderr == "true",
        "exited with {:?}, stderr: {stderr}",
        out.status
    );
}
