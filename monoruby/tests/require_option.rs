//! Integration coverage for the command-line `-r`/`--require` option.
//!
//! `-r` requires a library before the main program runs. Crucially the
//! library is required inside the *same* interpreter run as the main
//! program, so an `at_exit` handler it registers fires exactly once —
//! after the main program — rather than being drained early. This can
//! only be exercised by spawning the real binary, since it is a
//! command-line concern, not an in-process `run_test` behaviour.

use std::io::Write;
use std::process::Command;

fn monoruby() -> Command {
    Command::new(env!("CARGO_BIN_EXE_monoruby"))
}

/// Write `body` to a uniquely-named file under the system temp dir and
/// return its path. The name is caller-supplied (one per test) so
/// parallel tests don't collide.
fn write_temp(name: &str, body: &str) -> std::path::PathBuf {
    let path = std::env::temp_dir().join(name);
    let mut f = std::fs::File::create(&path).expect("create temp file");
    f.write_all(body.as_bytes()).expect("write temp file");
    path
}

fn stdout_of(cmd: &mut Command) -> String {
    let out = cmd.output().expect("failed to spawn monoruby");
    assert!(
        out.status.success(),
        "monoruby exited with {:?}\nstderr: {}",
        out.status,
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).into_owned()
}

/// `-r lib -e code`: the library is loaded first (its constants /
/// definitions are visible to the main program), and an `at_exit`
/// handler it registers runs once, after the main program — matching
/// `ruby -r lib -e code`.
#[test]
fn require_option_preloads_and_defers_at_exit() {
    let lib = write_temp(
        "monoruby_ropt_lib1.rb",
        "GREETING = \"hi\"\nat_exit { print \"|exit:#{$result}\" }\n",
    );
    let out = stdout_of(
        monoruby()
            .arg("-r")
            .arg(&lib)
            .args(["-e", "$result = GREETING; print \"main\""]),
    );
    // main body prints "main"; then the single deferred at_exit prints
    // "|exit:hi" once.
    assert_eq!(out, "main|exit:hi");
}

/// The `-rLIB` glued spelling works too, and the required library runs
/// before a main *file*.
#[test]
fn require_option_glued_with_file() {
    let lib = write_temp(
        "monoruby_ropt_lib2.rb",
        "$loaded = true\nat_exit { print \"done\" }\n",
    );
    let main = write_temp(
        "monoruby_ropt_main2.rb",
        "print($loaded ? \"yes:\" : \"no:\")\n",
    );
    let out = stdout_of(monoruby().arg(format!("-r{}", lib.display())).arg(&main));
    assert_eq!(out, "yes:done");
}

/// An uncaught exception in a `-e` program exits non-zero and reports the
/// error on stderr (exercises the `-e` error path of the run driver).
#[test]
fn dash_e_error_exits_nonzero() {
    let out = monoruby()
        .args(["-e", "raise 'boom'"])
        .output()
        .expect("failed to spawn monoruby");
    assert!(!out.status.success(), "expected failure exit");
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("boom"),
        "expected the raised message on stderr, got: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// An uncaught exception in a main *file* exits non-zero (exercises the
/// file error path of the run driver).
#[test]
fn main_file_error_exits_nonzero() {
    let main = write_temp("monoruby_ropt_err.rb", "raise 'kaboom'\n");
    let out = monoruby()
        .arg(&main)
        .output()
        .expect("failed to spawn monoruby");
    assert!(!out.status.success(), "expected failure exit");
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("kaboom"),
        "expected the raised message on stderr, got: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}
