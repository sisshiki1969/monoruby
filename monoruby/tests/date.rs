//! `date` is CRuby's ext/date built against the C API layer
//! (monoruby_capi/ext/date, doc/c_api.md §3.17): Date / DateTime behave
//! as CRuby's do. Each script under tests/date/ runs on monoruby and on
//! the host `ruby`, and the outputs must be identical. (A C extension is
//! initialized once per process, so these run the binary rather than an
//! in-process interpreter.) Today's date and the local zone are kept out
//! of the scripts.

use std::path::PathBuf;
use std::process::Command;

fn script(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(format!("tests/date/{name}.rb"))
}

fn run(cmd: &mut Command) -> String {
    let out = cmd.output().unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "{cmd:?} failed\n--- stdout\n{stdout}\n--- stderr\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    stdout
}

fn host_ruby_has_date() -> bool {
    Command::new("ruby")
        .args(["-e", "require 'date'; require 'time'; require 'yaml'; require 'json'"])
        .output()
        .is_ok_and(|o| o.status.success())
}

fn matches_cruby(name: &str) {
    if !host_ruby_has_date() {
        eprintln!("skipped: no host ruby with date");
        return;
    }
    let script = script(name);
    let expected = run(Command::new("ruby").env("TZ", "UTC").arg(&script));
    for args in [&[][..], &["--no-jit"][..]] {
        let actual = run(Command::new(env!("CARGO_BIN_EXE_monoruby")).env("TZ", "UTC").args(args).arg(&script));
        if actual != expected {
            let diff: Vec<String> = expected
                .lines()
                .zip(actual.lines())
                .enumerate()
                .filter(|(_, (e, a))| e != a)
                .map(|(i, (e, a))| format!("line {}:\n  cruby:    {e}\n  monoruby: {a}", i + 1))
                .collect();
            panic!(
                "{name}: monoruby {args:?} differs from CRuby ({} vs {} lines)\n{}",
                expected.lines().count(),
                actual.lines().count(),
                diff.join("\n")
            );
        }
    }
}

#[test]
fn date_arithmetic_and_formatting() {
    matches_cruby("date_arithmetic_and_formatting");
}

#[test]
fn date_parsing() {
    matches_cruby("date_parsing");
}

#[test]
fn datetime_and_time_conversions() {
    matches_cruby("datetime_and_time_conversions");
}

#[test]
fn date_infinity_and_comparisons() {
    matches_cruby("date_infinity_and_comparisons");
}

#[test]
fn date_keeps_the_callers_last_match() {
    matches_cruby("date_keeps_the_callers_last_match");
}

#[test]
fn date_through_yaml_and_json() {
    matches_cruby("date_through_yaml_and_json");
}
