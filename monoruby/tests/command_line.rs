//! Integration coverage for CRuby-compatible command-line option
//! processing (see `src/main.rs`). Everything here spawns the real
//! binary: option parsing is a process-boundary concern that
//! in-process `run_test` can't reach.

use std::io::Write;
use std::process::{Command, Output, Stdio};

fn monoruby() -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_monoruby"));
    // Isolate from the invoking environment.
    cmd.env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .env_remove("RUBYPATH");
    // None of the switches under test needs a gem, so skip the rubygems
    // boot — it is most of a spawn's startup cost, and this file spawns
    // the binary a few dozen times. It goes in first so a test's own
    // switches still come after it (and can still override it: the
    // `--enable`/`--disable` parsing tests append their own).
    cmd.arg("--disable=gems");
    cmd
}

fn run(cmd: &mut Command) -> Output {
    cmd.output().expect("failed to spawn monoruby")
}

fn run_with_stdin(cmd: &mut Command, input: &str) -> Output {
    let mut child = cmd
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn monoruby");
    child
        .stdin
        .take()
        .unwrap()
        .write_all(input.as_bytes())
        .unwrap();
    child.wait_with_output().expect("wait failed")
}

fn stdout_of(cmd: &mut Command) -> String {
    let out = run(cmd);
    assert!(
        out.status.success(),
        "monoruby exited with {:?}\nstderr: {}",
        out.status,
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).into_owned()
}

fn write_temp(name: &str, body: &str) -> std::path::PathBuf {
    let path = std::env::temp_dir().join(name);
    std::fs::write(&path, body).expect("write temp file");
    path
}

#[test]
fn version_switches() {
    // `-v` alone prints the version banner and exits without reading
    // stdin or a script.
    let out = stdout_of(monoruby().arg("-v"));
    assert!(out.starts_with("monoruby "), "banner: {out}");
    // `--version` prints the banner even when a program is given.
    let out = stdout_of(monoruby().args(["--version", "-e", "puts :not_run"]));
    assert!(out.starts_with("monoruby "));
    assert!(!out.contains("not_run"));
    // `-v` with a program prints the banner, then runs it.
    let out = stdout_of(monoruby().args(["-v", "-e", "puts :ran"]));
    assert!(out.starts_with("monoruby "));
    assert!(out.contains("ran"));
    let out = stdout_of(monoruby().arg("--copyright"));
    assert!(out.contains("Copyright"), "copyright: {out}");
    let out = stdout_of(monoruby().arg("-h"));
    assert!(out.contains("Usage: monoruby"));
}

#[test]
fn invalid_option_is_reported() {
    let out = run(monoruby().args(["-Z", "-e", "puts 1"]));
    assert_eq!(out.status.code(), Some(1));
    let err = String::from_utf8_lossy(&out.stderr);
    assert!(err.contains("invalid option -Z"), "stderr: {err}");
}

#[test]
fn dash_n_loop_and_autosplit() {
    let out = run_with_stdin(monoruby().args(["-n", "-e", "puts $_.upcase"]), "ab\ncd\n");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "AB\nCD\n");

    // -p prints $_ after each iteration; BEGIN runs once.
    let out = run_with_stdin(
        monoruby().args(["-p", "-e", "BEGIN { puts :begin }; $_ = $_.chomp + \"!\\n\""]),
        "x\ny\n",
    );
    assert_eq!(String::from_utf8_lossy(&out.stdout), "begin\nx!\ny!\n");

    // -a splits into $F; -F sets the pattern; clustered `-naF:`.
    let out = run_with_stdin(monoruby().args(["-naF:", "-e", "puts $F[1]"]), "a:b:c\nd:e:f\n");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "b\ne\n");

    // -l chomps each line and mirrors into $-l / $\.
    let out = run_with_stdin(
        monoruby().args(["-n", "-l", "-e", "puts [$_.end_with?(\"\\n\"), $-l, $\\ == $/].inspect"]),
        "q\n",
    );
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "[false, true, true]\n"
    );
}

#[test]
fn dash_zero_record_separator() {
    let out = run(monoruby().args(["-072", "-e", "puts $/, $-0, $/.frozen?"]));
    assert_eq!(String::from_utf8_lossy(&out.stdout), ":\n:\ntrue\n");
    // -0777 slurps (nil separator); gets reads everything. (`lines`
    // would itself split on the now-nil `$/`, so measure bytes.)
    let out = run_with_stdin(
        monoruby().args(["-0777", "-n", "-e", "puts $_.bytesize"]),
        "a\nb\nc\n",
    );
    assert_eq!(String::from_utf8_lossy(&out.stdout), "6\n");
}

#[test]
fn kernel_chomp_and_chop() {
    // chomp/chop are only defined under -n / -p.
    let out = run_with_stdin(
        monoruby().args(["-n", "-e", "chomp; print $_, \"|\"; $_ = \"xyz\"; chop; puts $_"]),
        "ab\n",
    );
    assert_eq!(String::from_utf8_lossy(&out.stdout), "ab|xy\n");
    // Without a loop switch they don't exist at all.
    let out = run(monoruby().args(["-e", "chomp"]));
    assert_eq!(out.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&out.stderr).contains("NameError"));
    // chomp with nil $_ (BEGIN runs before the first gets) is a TypeError.
    let out = run_with_stdin(monoruby().args(["-n", "-e", "BEGIN { chomp }"]), "");
    assert_eq!(out.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&out.stderr).contains("TypeError"));
}

#[test]
fn dash_s_switch_globals() {
    // `--long-name` keeps one leading dash converted: `$_long_name`
    // (matching CRuby's `--name=blah` → `$_name`).
    let script = write_temp("cli_dash_s.rb", "p [$flag, $_long_name, ARGV]\n");
    let out = stdout_of(monoruby().args([
        "-s",
        script.to_str().unwrap(),
        "-flag",
        "--long-name=v",
        "rest",
    ]));
    assert_eq!(out, "[true, \"v\", [\"rest\"]]\n");
}

#[test]
fn syntax_check() {
    let ok = write_temp("cli_ok.rb", "puts 1\n");
    let out = stdout_of(monoruby().args(["-c", ok.to_str().unwrap()]));
    assert_eq!(out, "Syntax OK\n");
    let out = stdout_of(monoruby().args(["-c", "-e", "puts 1", "-e", "hello world"]));
    assert_eq!(out, "Syntax OK\n");
    let bad = write_temp("cli_bad.rb", "def f(\n");
    let out = run(monoruby().args(["-c", bad.to_str().unwrap()]));
    assert_eq!(out.status.code(), Some(1));
}

#[test]
fn chdir_switch() {
    let out = stdout_of(monoruby().args(["-C", "/tmp", "-e", "print Dir.pwd"]));
    assert!(out.ends_with("tmp"), "pwd: {out}");
    // Attached form, and -X as an alias.
    let out = stdout_of(monoruby().args(["-C/tmp", "-e", "print 1"]));
    assert_eq!(out, "1");
    let out = run(monoruby().args(["-C", "/no/such/dir", "-e", "puts 1"]));
    assert_eq!(out.status.code(), Some(1));
}

#[test]
fn dash_x_shebang_scan() {
    let script = write_temp(
        "cli_embedded.txt",
        "leading garbage\nmore garbage\n#!ruby\nputs :embedded\n",
    );
    let out = stdout_of(monoruby().args(["-x", script.to_str().unwrap()]));
    assert_eq!(out, "embedded\n");
    // Automatic scan when the first line is a non-ruby shebang.
    let script = write_temp(
        "cli_hybrid.sh",
        "#!/bin/sh\n#!ruby\nputs :hybrid\n",
    );
    let out = stdout_of(monoruby().arg(script.to_str().unwrap()));
    assert_eq!(out, "hybrid\n");
    // No #!ruby line under -x is a LoadError.
    let script = write_temp("cli_noruby.txt", "#!/bin/sh\necho no\n");
    let out = run(monoruby().args(["-x", script.to_str().unwrap()]));
    assert_eq!(out.status.code(), Some(1));
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("no Ruby script found in input")
    );
}

#[test]
fn dash_upper_s_path_search() {
    let dir = std::env::temp_dir().join("cli_s_bin");
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("launcher_test.rb"), "puts :launched\n").unwrap();
    let out = stdout_of(
        monoruby()
            .env("RUBYPATH", dir.to_str().unwrap())
            .args(["-S", "launcher_test.rb"]),
    );
    assert_eq!(out, "launched\n");
    let out = run(monoruby().args(["-S", "no_such_launcher_xyz"]));
    assert_eq!(out.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&out.stderr).contains("LoadError"));
}

#[test]
fn rubyopt_processing() {
    let out = stdout_of(monoruby().env("RUBYOPT", "-w").args(["-e", "p $VERBOSE"]));
    assert_eq!(out, "true\n");
    // Command-line -W0 overrides RUBYOPT.
    let out = stdout_of(
        monoruby()
            .env("RUBYOPT", "-W2")
            .args(["-W0", "-e", "p $VERBOSE"]),
    );
    assert_eq!(out, "nil\n");
    // Forbidden switch in RUBYOPT.
    let out = run(monoruby().env("RUBYOPT", "-p").args(["-e", "puts 1"]));
    assert_eq!(out.status.code(), Some(1));
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("invalid switch in RUBYOPT: -p")
    );
    // --disable-rubyopt skips processing entirely.
    let out = stdout_of(
        monoruby()
            .env("RUBYOPT", "-w")
            .args(["--disable-rubyopt", "-e", "p $VERBOSE"]),
    );
    assert_eq!(out, "false\n");
}

#[test]
fn rubylib_and_dash_i_load_path() {
    let out = stdout_of(
        monoruby()
            .env("RUBYLIB", "/rubylib/dir")
            .args(["-I", "/dash/i/dir", "-e", "puts $LOAD_PATH[0..1]"]),
    );
    assert_eq!(out, "/dash/i/dir\n/rubylib/dir\n");
}

#[test]
fn warning_switches() {
    let out = stdout_of(monoruby().args(["-W:deprecated", "-e", "p Warning[:deprecated]"]));
    assert_eq!(out, "true\n");
    let out = stdout_of(monoruby().args(["-w", "-W:no-deprecated", "-e", "p Warning[:deprecated]"]));
    assert_eq!(out, "false\n");
    let out = stdout_of(monoruby().args(["-d", "-e", "p [$DEBUG, $-d, $VERBOSE]"]));
    assert_eq!(out, "[true, true, true]\n");
}

#[test]
fn encoding_switches() {
    let out = stdout_of(monoruby().args(["-E", "euc-jp", "-e", "p Encoding.default_external"]));
    assert_eq!(out, "#<Encoding:EUC-JP>\n");
    let out = stdout_of(monoruby().args(["-U", "-e", "p Encoding.default_internal"]));
    assert_eq!(out, "#<Encoding:UTF-8>\n");
    let out = stdout_of(monoruby().args(["-Ke", "-e", "p Encoding.default_external"]));
    assert_eq!(out, "#<Encoding:EUC-JP>\n");
    // -U conflicts with an explicit internal encoding on the command line.
    let out = run(monoruby().args(["-Eascii:ascii", "-U", "-e", "puts 1"]));
    assert_eq!(out.status.code(), Some(1));
    // A third encoding component is rejected.
    let out = run(monoruby().args(["--encoding", "a:b:c", "-e", "puts 1"]));
    assert_eq!(out.status.code(), Some(1));
}

#[test]
fn feature_switches() {
    let out = stdout_of(monoruby().args([
        "--enable-frozen-string-literal",
        "-e",
        "p 'x'.frozen?",
    ]));
    assert_eq!(out, "true\n");
    let out = stdout_of(monoruby().args([
        "--disable=frozen-string-literal",
        "-e",
        "p 'x'.frozen?",
    ]));
    assert_eq!(out, "false\n");
    let out = run(monoruby().args(["--enable=bogus-feature", "-e", "puts 1"]));
    assert!(out.status.success());
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("unknown argument for --enable")
    );
}

#[test]
fn backtrace_limit_truncates_report() {
    let script = write_temp(
        "cli_bt.rb",
        "def a; raise 'boom'; end\ndef b; a; end\ndef c; b; end\ndef d; c; end\nd\n",
    );
    let out = run(monoruby().args(["--backtrace-limit=1", script.to_str().unwrap()]));
    assert_eq!(out.status.code(), Some(1));
    let err = String::from_utf8_lossy(&out.stderr);
    assert!(err.contains("boom (RuntimeError)"), "stderr: {err}");
    assert!(err.contains("levels..."), "stderr: {err}");
    // Exception#full_message honours the limit too.
    let script = write_temp(
        "cli_bt_fm.rb",
        "def a; raise 'boom'; end\ndef b; a; end\ndef c; b; end\ndef d; c; end\n\
         begin; d; rescue => e; puts e.full_message; end\n",
    );
    let out = run(monoruby().args(["--backtrace-limit=1", script.to_str().unwrap()]));
    assert!(out.status.success());
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(text.contains("boom (RuntimeError)"), "stdout: {text}");
    assert!(text.contains("levels..."), "stdout: {text}");
}

#[test]
fn stdin_script_via_dash() {
    let out = run_with_stdin(monoruby().arg("-"), "puts :from_stdin\n");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "from_stdin\n");
}

#[test]
fn double_dash_ends_switches() {
    let out = run_with_stdin(
        monoruby().args(["-e", "p ARGV", "--", "-n", "x"]),
        "",
    );
    assert_eq!(String::from_utf8_lossy(&out.stdout), "[\"-n\", \"x\"]\n");
}

#[test]
fn gets_honours_record_separator() {
    // Paragraph mode: the first record is "a\nb\n\n" (5 bytes; `lines`
    // would split on the paragraph `$/`, so measure bytes).
    let out = run_with_stdin(
        monoruby().args(["-e", "$/ = \"\"; puts gets.bytesize"]),
        "a\nb\n\n\nc\n",
    );
    assert_eq!(String::from_utf8_lossy(&out.stdout), "5\n");
    // Custom multi-byte separator.
    let out = run_with_stdin(
        monoruby().args(["-e", "$/ = \"--\"; print gets"]),
        "one--two--",
    );
    assert_eq!(String::from_utf8_lossy(&out.stdout), "one--");
}

#[test]
fn inplace_edit_with_backup() {
    // `-i.bak`: each ARGV file is rewritten from the script's stdout
    // writes while the original bytes survive under the .bak name.
    let dir = std::env::temp_dir().join(format!("cli_inplace_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let f1 = dir.join("a.txt");
    let f2 = dir.join("b.txt");
    std::fs::write(&f1, "A1\nA2\n").unwrap();
    std::fs::write(&f2, "B1\nB2\n").unwrap();
    let out = run_with_stdin(
        monoruby().args([
            "-i.bak",
            "-e",
            "while line = ARGF.gets do puts line.chomp + '!' end",
            f1.to_str().unwrap(),
            f2.to_str().unwrap(),
        ]),
        "",
    );
    assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
    assert_eq!(std::fs::read_to_string(&f1).unwrap(), "A1!\nA2!\n");
    assert_eq!(std::fs::read_to_string(&f2).unwrap(), "B1!\nB2!\n");
    assert_eq!(
        std::fs::read_to_string(dir.join("a.txt.bak")).unwrap(),
        "A1\nA2\n"
    );
    assert_eq!(
        std::fs::read_to_string(dir.join("b.txt.bak")).unwrap(),
        "B1\nB2\n"
    );
    std::fs::remove_dir_all(&dir).ok();
}

#[test]
fn inplace_edit_without_backup() {
    // Bare `-i`: rewritten in place, no backup left behind. Also routes
    // the write family through the redirected $stdout.
    let dir = std::env::temp_dir().join(format!("cli_inplace_nb_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let f1 = dir.join("c.txt");
    std::fs::write(&f1, "x1\nx2\n").unwrap();
    let out = run_with_stdin(
        monoruby().args([
            "-i",
            "-e",
            "while line = ARGF.gets do ARGF.print line.upcase; end; ARGF.write ARGF.puts",
            f1.to_str().unwrap(),
        ]),
        "",
    );
    assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
    assert_eq!(std::fs::read_to_string(&f1).unwrap(), "X1\nX2\n");
    assert!(!dir.join("c.txt.bak").exists());
    std::fs::remove_dir_all(&dir).ok();
}

#[test]
fn argf_filename_global_tracks_walk() {
    // `$FILENAME` follows the file `Kernel#gets` is reading; ARGF.printf
    // exercises the un-redirected write path.
    let dir = std::env::temp_dir().join(format!("cli_filename_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let f1 = dir.join("n1.txt");
    let f2 = dir.join("n2.txt");
    std::fs::write(&f1, "1\n").unwrap();
    std::fs::write(&f2, "2\n").unwrap();
    let out = run_with_stdin(
        monoruby().args([
            "-e",
            "names = []; while gets; names << File.basename($FILENAME); end; \
             ARGF.printf(\"%s\", names.join(','))",
            f1.to_str().unwrap(),
            f2.to_str().unwrap(),
        ]),
        "",
    );
    assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
    assert_eq!(String::from_utf8_lossy(&out.stdout), "n1.txt,n2.txt");
    std::fs::remove_dir_all(&dir).ok();
}
