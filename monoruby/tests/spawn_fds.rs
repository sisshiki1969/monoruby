//! `spawn` / `IO.popen` cases that name the parent's own descriptors.
//!
//! The in-process harness runs tests on parallel threads of one process,
//! so an fd number a test reads from `#fileno` can be closed or reused by
//! another test in the meantime. These cases run in a process of their
//! own instead, on monoruby and on the host CRuby, and compare stdout.

use std::process::Command;

fn run(cmd: &mut Command, script: &str) -> String {
    let out = cmd
        .env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .arg("--disable=gems")
        .arg("-e")
        .arg(script)
        .output()
        .expect("failed to spawn");
    assert!(
        out.status.success(),
        "{:?} failed: {}",
        out.status,
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8(out.stdout).unwrap()
}

#[track_caller]
fn assert_same_as_cruby(script: &str) {
    let expected = run(&mut Command::new(monoruby::tests::ruby_path()), script);
    let actual = run(&mut Command::new(env!("CARGO_BIN_EXE_monoruby")), script);
    assert_eq!(actual, expected);
}

/// A true swap (`a => b, b => a`) breaks the cycle through a spare fd,
/// `fd => fd` clears close-on-exec, `close_others:` marks an inherited fd
/// close-on-exec, a script without a shebang runs under /bin/sh, PATH
/// comes from the env hash, and popen takes its encoding options; fsync
/// errors name `rb_io_fsync`.
#[test]
fn spawn_dup2_cycles_close_others_and_noexec() {
    assert_same_as_cruby(
        r##"
    require "tmpdir"
    res = []
    Dir.mktmpdir do |d|
      r1, w1 = IO.pipe
      r2, w2 = IO.pipe
      a, b = w1.fileno, w2.fileno
      # a true swap, plus fd => fd on a close-on-exec pipe
      Process.wait Process.spawn("echo A >&#{a}; echo B >&#{b}", a => b, b => a)
      Process.wait Process.spawn("echo S >&#{a}", a => a)
      w1.close; w2.close
      res << r1.read << r2.read
      # close_others marks an inherited non-cloexec fd close-on-exec
      r, w = IO.pipe
      w.close_on_exec = false
      r3, w3 = IO.pipe
      Process.wait Process.spawn("(echo x >&#{w.fileno}) 2>/dev/null || echo closed", close_others: true, out: w3)
      Process.wait Process.spawn("echo kept >&#{w.fileno}", out: w3)
      w.close; w3.close
      res << r.read << r3.read
      # ENOEXEC: a script without a shebang runs under /bin/sh
      File.write("#{d}/s", "echo noshebang $1\n")
      File.chmod(0755, "#{d}/s")
      res << IO.popen(["#{d}/s", "arg"], &:read)
      # PATH from the env hash
      res << IO.popen({"PATH" => "/usr/bin:/bin"}, "echo path", &:read)
      res << (Process.wait2(Process.spawn({"PATH" => "#{d}"}, "s", out: File::NULL))[1].exitstatus)
      # popen's encoding options
      res << IO.popen("echo", encoding: "EUC-JP:UTF-8") { |io| [io.external_encoding&.name, io.internal_encoding&.name] }
      res << IO.popen("echo", encoding: Encoding::EUC_JP) { |io| [io.external_encoding&.name, io.internal_encoding&.name] }
      res << IO.popen("echo", external_encoding: "Shift_JIS", internal_encoding: "UTF-8") { |io| [io.external_encoding&.name, io.internal_encoding&.name] }
      # fsync on a pipe
      pr, pw = IO.pipe
      named = IO.new(pw.fileno, "w", path: "pp", autoclose: false)
      [pw, named].each do |io|
        io.fdatasync
      rescue => e
        res << [e.class, e.message]
      end
      named.close; pr.close; pw.close
    end
    p res
"##,
    );
}

/// The same child-side steps, with a `chdir:` that fails after them: the
/// child reports through the error pipe instead of exec'ing, so this is
/// also the run in which a coverage build records the steps (a
/// successful `execve` discards the child's counters).
#[test]
fn child_steps_before_a_failing_chdir() {
    assert_same_as_cruby(
        r##"
    res = []
    r1, w1 = IO.pipe
    r2, w2 = IO.pipe
    a, b = w1.fileno, w2.fileno
    w1.close_on_exec = false
    bad = "/nonexistent-dir-xyzzy"
    [
      -> { Process.spawn("true", a => b, b => a, chdir: bad) },
      -> { Process.spawn("true", a => a, chdir: bad) },
      -> { Process.spawn("true", close_others: true, chdir: bad) },
      -> { Process.spawn("true", 3 => w2, 4 => 3, 5 => 4, chdir: bad) },
      -> { Process.spawn("true", out: b, err: :out, chdir: bad) },
      -> { IO.popen(["true", a => b, b => a, chdir: bad]) },
    ].each do |l|
      l.call
      res << :ok
    rescue SystemCallError => e
      res << [e.class, e.message]
    end
    p res
"##,
    );
}
