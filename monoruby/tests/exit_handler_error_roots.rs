//! An uncaught error's `Value`s stay reachable across the exit handlers.
//!
//! `Globals::run_with_prelude` holds the script's `Err` in a Rust local —
//! which the collector does not scan — while `at_exit` blocks,
//! `ObjectSpace` finalizers and the dying threads' ensure clauses run, and
//! then dereferences the `Value`s that error carries to build the report.
//! Each case below registers an `at_exit` that allocates, so Ruby really
//! does run in that window, and then raises uncaught an error of a kind
//! that carries a `Value`: the re-raised object, an explicit `cause:`, a
//! payload, a receiver, a key, a thrown tag.
//!
//! The assertion is monoruby's own report rather than CRuby's, because the
//! point is the rooting rather than the wording — under `gc-stress`, where
//! every allocation collects, a `Value` left out of the rooting comes back
//! as a recycled cell and the report shows the wrong class, the wrong
//! receiver, or a bare address.

use std::process::Command;

/// Run `script` with an `at_exit` that allocates, and return
/// `(exit code, stderr)`.
fn run(script: &str) -> (Option<i32>, String) {
    let script = format!("at_exit {{ 200.times {{ Object.new }} }}\n{script}");
    let out = Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .env_remove("RUBYOPT")
        .args(["--disable=gems", "-e", &script])
        .output()
        .expect("spawn monoruby");
    (
        out.status.code(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

#[track_caller]
fn assert_reports(script: &str, expected: &[&str]) {
    let (code, stderr) = run(script);
    assert_eq!(code, Some(1), "script: {script}\nstderr: {stderr}");
    for want in expected {
        assert!(
            stderr.contains(want),
            "expected {want:?} in the report\nscript: {script}\nstderr: {stderr}"
        );
    }
}

/// `original`: `raise exc` re-raises that very object, so the error holds
/// it and the report reads its class, message and backtrace back out.
#[test]
fn a_reraised_object_survives_the_handlers() {
    assert_reports(
        "raise RuntimeError.new('re-raised boom')",
        &["re-raised boom", "RuntimeError"],
    );
}

/// `explicit_cause`: an explicit `cause:` is a second object the error
/// carries, and the report walks the `#cause` chain.
#[test]
fn an_explicit_cause_survives_the_handlers() {
    assert_reports(
        "raise ArgumentError, 'outer', cause: RuntimeError.new('inner cause')",
        &["outer", "ArgumentError"],
    );
}

/// A NoMethodError carries the receiver it was raised on, and the report
/// names that receiver's class.
#[test]
fn a_no_method_receiver_survives_the_handlers() {
    assert_reports(
        "Object.new.this_method_does_not_exist",
        &["this_method_does_not_exist", "NoMethodError", "Object"],
    );
}

/// A NameError carries its receiver too.
#[test]
fn a_name_error_survives_the_handlers() {
    assert_reports(
        "class Probe; def go; no_such_local_or_method; end; end; Probe.new.go",
        &["no_such_local_or_method", "NameError"],
    );
}

/// A FrozenError carries the frozen receiver.
#[test]
fn a_frozen_receiver_survives_the_handlers() {
    assert_reports(
        "s = 'frozen'.freeze; s << 'more'",
        &["FrozenError", "frozen"],
    );
}

/// A KeyError carries both the hash it was raised on and the missing key.
#[test]
fn a_key_error_pair_survives_the_handlers() {
    assert_reports("{'a' => 1}.fetch('missing key')", &["KeyError", "missing key"]);
}

/// An uncaught `throw` carries the tag and the value.
#[test]
fn an_uncaught_throw_survives_the_handlers() {
    assert_reports("throw :no_catch_for_this, 42", &["no_catch_for_this"]);
}

/// A `return` from a proc whose defining method has already returned
/// reaches the top level as a non-local return, which carries the return
/// value.
#[test]
fn a_stale_non_local_return_survives_the_handlers() {
    let (code, stderr) = run("def mk; proc { return Object.new }; end; mk.call");
    assert_eq!(code, Some(1), "stderr: {stderr}");
    assert!(
        stderr.contains("return") || stderr.contains("LocalJumpError"),
        "stderr: {stderr}"
    );
}
