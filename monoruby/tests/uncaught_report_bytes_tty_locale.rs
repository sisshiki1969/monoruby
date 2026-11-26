//! What a process started on a file writes and raises, against CRuby:
//!
//! - the report of an uncaught exception is the message's bytes as
//!   they are, whatever its encoding and whether or not they are valid
//!   there (#2246);
//! - on a terminal that report is highlighted, as CRuby asks
//!   `#detailed_message(highlight: true)` when stderr is a tty (#2250);
//! - a regexp source quoted in an error message is escaped against the
//!   locale's encoding, and the command an `Errno` raised by
//!   `IO.popen` / `system` quotes is converted into it (#2248).
//!
//! None of it is visible to an in-process `run_test`, which runs with
//! the default external encoding pinned to UTF-8 and stderr captured.

use std::io::{Read, Write};
use std::os::fd::FromRawFd;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

fn apply(cmd: &mut Command, locale: &str) {
    cmd.env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .env_remove("RUBYPATH")
        .env_remove("NO_COLOR")
        .env_remove("LANG")
        .env_remove("LC_CTYPE")
        .env("LC_ALL", locale);
}

// `--disable=gems` keeps did_you_mean and error_highlight out of both
// reports.

fn monoruby(locale: &str) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_monoruby"));
    apply(&mut cmd, locale);
    cmd.arg("--disable=gems");
    cmd
}

fn cruby(locale: &str) -> Command {
    let mut cmd = Command::new(monoruby::tests::ruby_path());
    apply(&mut cmd, locale);
    cmd.arg("--disable=gems");
    cmd
}

/// `#<Thread:0x…` with the address left out, which differs per run.
fn mask_thread_addresses(out: Vec<u8>) -> Vec<u8> {
    const HEAD: &[u8] = b"#<Thread:0x";
    let mut res = Vec::with_capacity(out.len());
    let mut i = 0;
    while i < out.len() {
        if out[i..].starts_with(HEAD) {
            res.extend_from_slice(HEAD);
            i += HEAD.len();
            while i < out.len() && out[i].is_ascii_hexdigit() {
                i += 1;
            }
        } else {
            res.push(out[i]);
            i += 1;
        }
    }
    res
}

/// Exit status, stdout and stderr of `cmd` run in `dir`, as bytes.
fn run(mut cmd: Command, dir: &Path) -> Vec<u8> {
    cmd.current_dir(dir)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    let out = cmd.output().expect("failed to spawn");
    let mut res = format!("exit: {:?}\n", out.status.code()).into_bytes();
    res.extend_from_slice(&out.stdout);
    res.extend_from_slice(b"--- stderr ---\n");
    res.extend_from_slice(&out.stderr);
    res
}

/// What `cmd` writes to its stderr when that is the slave of a pseudo
/// terminal, read off the master side.
fn run_on_tty(mut cmd: Command, dir: &Path) -> Vec<u8> {
    let mut master = -1;
    let mut slave = -1;
    // SAFETY: openpty writes two descriptors into the buffers we pass;
    // the name, termios and winsize pointers may be null.
    let rc = unsafe {
        libc::openpty(
            &mut master,
            &mut slave,
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            std::ptr::null_mut(),
        )
    };
    assert_eq!(rc, 0, "openpty failed: {}", std::io::Error::last_os_error());
    // SAFETY: both descriptors were just returned by openpty and are
    // owned here.
    let mut master = unsafe { std::fs::File::from_raw_fd(master) };
    let slave = unsafe { std::fs::File::from_raw_fd(slave) };
    cmd.current_dir(dir)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::from(slave));
    let mut child = cmd.spawn().expect("failed to spawn");
    // The `Command` holds the last copy of the slave in the parent;
    // without dropping it the master would never see the end.
    drop(cmd);
    let reader = std::thread::spawn(move || {
        let mut out = vec![];
        let mut buf = [0u8; 4096];
        loop {
            match master.read(&mut buf) {
                // EIO once the slave's last holder has gone.
                Ok(0) | Err(_) => break,
                Ok(n) => out.extend_from_slice(&buf[..n]),
            }
        }
        out
    });
    let status = child.wait().expect("wait");
    let mut res = format!("exit: {:?}\n", status.code()).into_bytes();
    res.extend_from_slice(&reader.join().unwrap());
    res
}

fn assert_same(what: &str, expected: Vec<u8>, actual: Vec<u8>) {
    let (expected, actual) = (
        mask_thread_addresses(expected),
        mask_thread_addresses(actual),
    );
    if expected != actual {
        panic!(
            "{what}:\n--- CRuby ---\n{}\n--- monoruby ---\n{}",
            String::from_utf8_lossy(&expected.escape_ascii().collect::<Vec<u8>>()),
            String::from_utf8_lossy(&actual.escape_ascii().collect::<Vec<u8>>()),
        );
    }
}

/// A private directory for one test's fixtures.
struct Fixtures(PathBuf);

impl Fixtures {
    fn new(name: &str) -> Self {
        let dir = std::env::temp_dir().join(format!("mr_uncaught_{name}_{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("create fixture dir");
        Fixtures(dir)
    }

    fn write(&self, name: &str, body: impl AsRef<[u8]>) {
        let mut f = std::fs::File::create(self.0.join(name)).expect("create fixture");
        f.write_all(body.as_ref()).expect("write fixture");
    }

    /// Run `script` on both Rubies under `locale` and require the same
    /// output, byte for byte.
    fn compare(&self, script: &str, locale: &str) {
        let expected = run(
            {
                let mut c = cruby(locale);
                c.arg(script);
                c
            },
            &self.0,
        );
        let actual = run(
            {
                let mut c = monoruby(locale);
                c.arg(script);
                c
            },
            &self.0,
        );
        assert_same(&format!("{script} (LC_ALL={locale})"), expected, actual);
    }

    /// [`compare`](Self::compare) with stderr on a terminal.
    fn compare_on_tty(&self, script: &str) {
        let expected = run_on_tty(
            {
                let mut c = cruby("C.UTF-8");
                c.arg(script);
                c
            },
            &self.0,
        );
        let actual = run_on_tty(
            {
                let mut c = monoruby("C.UTF-8");
                c.arg(script);
                c
            },
            &self.0,
        );
        assert_same(&format!("{script} on a tty"), expected, actual);
    }
}

impl Drop for Fixtures {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// #2246: the bytes of a message that are not valid in its encoding
/// reach stderr as they are — for the exception, its causes and a
/// thread's report — instead of U+FFFD or a `\xE9` escape.
#[test]
fn uncaught_report_writes_the_message_bytes() {
    let fx = Fixtures::new("bytes");
    for enc in ["UTF-8", "US-ASCII", "BINARY", "EUC-JP", "Shift_JIS"] {
        let name = format!("d_{}.rb", enc.replace('-', "_"));
        fx.write(
            &name,
            format!(
                "e = RuntimeError.new(\"caf\\xE9\\nline\\xA4\".dup.force_encoding({enc:?}))\nraise e\n"
            ),
        );
        fx.compare(&name, "C");
    }
    fx.write(
        "chain.rb",
        "Thread.new { raise \"th\\xE9\".b }.join rescue nil\n\
         begin\n\
         \x20 raise \"inner\\xE9\".b\n\
         rescue\n\
         \x20 raise ArgumentError, \"outer\\xE9\".b\n\
         end\n",
    );
    fx.compare("chain.rb", "C.UTF-8");
}

/// #2250: on a terminal the report is highlighted: bold, the class
/// underlined, every line of a multi-line message bold on its own, a
/// message-less exception underlined, and a class's own
/// `#detailed_message` asked with `highlight: true`. A `$stderr` that is
/// not the process's own is never highlighted, a terminal or not.
#[test]
fn uncaught_report_is_highlighted_on_a_tty() {
    let fx = Fixtures::new("tty");
    fx.write("plain.rb", "raise \"boom\"\n");
    fx.write("empty.rb", "raise \"\"\n");
    fx.write("multi.rb", "raise \"multi\\nline\\n\\nend\"\n");
    fx.write(
        "custom.rb",
        "class E < StandardError; def detailed_message(highlight: false, **) = \"hl=#{highlight.inspect}\"; end\n\
         begin\n\
         \x20 raise \"inner\"\n\
         rescue\n\
         \x20 raise E\n\
         end\n",
    );
    fx.write(
        "nomsg.rb",
        "class F < StandardError; def detailed_message(**) = \"\"; end\n\
         class G < RuntimeError; def detailed_message(**) = nil; end\n\
         class H < StandardError; def detailed_message(**) = raise(\"no\"); end\n\
         begin\n\
         \x20 begin\n\
         \x20   begin\n\
         \x20     raise H\n\
         \x20   rescue\n\
         \x20     raise F\n\
         \x20   end\n\
         \x20 rescue\n\
         \x20   raise G\n\
         \x20 end\n\
         rescue\n\
         \x20 raise RuntimeError.new(\"x\").tap { |e| def e.detailed_message(**) = \"\" }\n\
         end\n",
    );
    fx.write(
        "runtime.rb",
        "class RuntimeError; def detailed_message(**) = \"\"; end\nraise \"x\"\n",
    );
    fx.write(
        "thread.rb",
        "Thread.new { raise \"in thread\" }.join rescue nil\nraise \"main\"\n",
    );
    fx.write(
        "redirected.rb",
        "$stderr = STDERR.dup\nraise \"redirected\"\n",
    );
    for script in [
        "plain.rb",
        "empty.rb",
        "multi.rb",
        "custom.rb",
        "nomsg.rb",
        "runtime.rb",
        "thread.rb",
        "redirected.rb",
    ] {
        fx.compare_on_tty(script);
    }
}

/// #2248: a regexp source an error message quotes is escaped against
/// `Encoding.default_external` — a non-ASCII character by value unless
/// the whole source can be shown as it is, a bare `/` with a backslash —
/// and the command an `IO.popen` / `system` that cannot run quotes is
/// converted into the locale's encoding.
#[test]
fn messages_quote_in_the_locale_encoding() {
    let fx = Fixtures::new("locale");
    fx.write(
        "msg.rb",
        r#"def t
  yield
rescue Exception => e
  p [e.class, e.message, e.message.encoding]
end
t { eval("/あ[/") }
t { eval("/[/") }
t { Regexp.new("あ[") }
t { Regexp.new("a/(") }
t { Regexp.new("あ\n[") }
t { Regexp.new("あ/(") }
t { Regexp.new("あ\\") }
t { Regexp.new("あ\\x") }
p Regexp.new("あ"), Regexp.new("あ\n"), Regexp.new("あ/"), Regexp.new("é".encode("ISO-8859-1"))
t { IO.popen("echo ab".encode("UTF-16LE")) }
t { IO.popen("あb".encode("UTF-16LE")) }
t { IO.popen(["あb".encode("UTF-16LE")]) }
t { system("あb".encode("UTF-16LE"), exception: true) }
t { spawn("nonexistあ") }
t { spawn("nonexistあ".encode("EUC-JP")) }
"#,
    );
    fx.compare("msg.rb", "C");
    fx.compare("msg.rb", "C.UTF-8");
}
