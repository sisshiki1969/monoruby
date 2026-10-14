//! The CRuby-compatible C API (`monoruby_capi/`, `doc/c_api.md`): one C
//! extension source, written against CRuby's `ruby.h` with nothing
//! monoruby-specific in it, built twice — against the host CRuby's
//! headers and against `monoruby_capi/include` plus the layer's
//! `capi.c` — and driven by the same script on both interpreters. The
//! outputs must be identical.

use std::path::{Path, PathBuf};
use std::process::Command;

fn manifest_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn cc() -> String {
    std::env::var("CC").unwrap_or_else(|_| "cc".to_string())
}

fn out_dir(flavor: &str) -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR"))
        .join("capi")
        .join(flavor);
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

/// `capi_hello.so` built against monoruby's headers, in its own directory
/// (to be put on `$LOAD_PATH`). `None` when there is no C compiler. Built
/// once per test binary: the tests run in parallel.
fn build_for_monoruby() -> Option<PathBuf> {
    static DIR: std::sync::OnceLock<Option<PathBuf>> = std::sync::OnceLock::new();
    DIR.get_or_init(build_for_monoruby_once).clone()
}

fn build_for_monoruby_once() -> Option<PathBuf> {
    let root = manifest_dir().join("..");
    let dir = out_dir("monoruby");
    let status = Command::new(cc())
        .args(["-shared", "-fPIC", "-O1", "-Wall", "-Werror"])
        .arg("-I")
        .arg(root.join("monoruby_capi/include"))
        .arg("-I")
        .arg(root.join("monoruby_ext_sys/include"))
        .arg("-o")
        .arg(dir.join("capi_hello.so"))
        .arg(manifest_dir().join("tests/capi/capi_hello.c"))
        .arg(root.join("monoruby_capi/src/capi.c"))
        .status()
        .ok()?;
    assert!(status.success(), "building capi_hello.c against monoruby_capi failed");
    Some(dir)
}

fn ruby_config(key: &str) -> String {
    let out = Command::new("ruby")
        .args(["-e", &format!("print RbConfig::CONFIG[{key:?}]")])
        .output()
        .expect("a host `ruby` is needed to build the CRuby side of the comparison");
    String::from_utf8(out.stdout).unwrap()
}

/// The same source built against the host CRuby, as `extconf.rb` would.
fn build_for_cruby() -> PathBuf {
    static DIR: std::sync::OnceLock<PathBuf> = std::sync::OnceLock::new();
    DIR.get_or_init(build_for_cruby_once).clone()
}

fn build_for_cruby_once() -> PathBuf {
    let dir = out_dir("cruby");
    let dlext = ruby_config("DLEXT");
    let mut cmd = Command::new(cc());
    cmd.args(["-shared", "-fPIC", "-O1", "-w"])
        .arg("-I")
        .arg(ruby_config("rubyhdrdir"))
        .arg("-I")
        .arg(ruby_config("rubyarchhdrdir"));
    if cfg!(target_os = "macos") {
        cmd.args(["-undefined", "dynamic_lookup"]);
    }
    let status = cmd
        .arg("-o")
        .arg(dir.join(format!("capi_hello.{dlext}")))
        .arg(manifest_dir().join("tests/capi/capi_hello.c"))
        .status()
        .unwrap();
    assert!(status.success(), "building capi_hello.c against CRuby failed");
    dir
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

fn monoruby(lib: &Path) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_monoruby"));
    cmd.arg("-I").arg(lib);
    cmd
}

#[test]
fn capi_extension_matches_cruby() {
    let Some(mr_dir) = build_for_monoruby() else {
        eprintln!("skipped: no C compiler");
        return;
    };
    let cr_dir = build_for_cruby();
    let script = manifest_dir().join("tests/capi/capi_hello.rb");
    let expected = run(Command::new("ruby").arg("-I").arg(&cr_dir).arg(&script));
    for args in [&[][..], &["--no-jit"][..]] {
        let actual = run(monoruby(&mr_dir).args(args).arg(&script));
        if actual != expected {
            let diff: Vec<String> = expected
                .lines()
                .zip(actual.lines())
                .enumerate()
                .filter(|(_, (e, a))| e != a)
                .map(|(i, (e, a))| format!("line {}:\n  cruby:    {e}\n  monoruby: {a}", i + 1))
                .collect();
            panic!(
                "monoruby {args:?} differs from CRuby ({} vs {} lines)\n{}",
                expected.lines().count(),
                actual.lines().count(),
                diff.join("\n")
            );
        }
    }
}

/// A library compiled against CRuby's headers is refused with a
/// LoadError (its `rb_*` symbols do not resolve in monoruby), not loaded
/// and run with the wrong value layout.
#[test]
fn capi_cruby_build_is_refused() {
    if build_for_monoruby().is_none() {
        eprintln!("skipped: no C compiler");
        return;
    }
    let cr_dir = build_for_cruby();
    let out = run(monoruby(&cr_dir).args([
        "-e",
        r#"begin; require "capi_hello"; rescue LoadError => e; p e.message.include?("monoruby's C API headers"); end
p defined?(CapiHello)"#,
    ]));
    if cfg!(target_os = "macos") {
        // CRuby's DLEXT there is `bundle`, which `require` does not look
        // for: the plain "cannot load" path, also a LoadError.
        assert_eq!(out.lines().last(), Some("nil"), "{out}");
    } else {
        assert_eq!(out, "true\nnil\n");
    }
}

/// Loaded by path, under the name a gem's `require "x/x"` gives, and
/// required once only.
#[test]
fn capi_extension_by_path() {
    let Some(mr_dir) = build_for_monoruby() else {
        eprintln!("skipped: no C compiler");
        return;
    };
    let so = mr_dir.join("capi_hello.so");
    let out = run(Command::new(env!("CARGO_BIN_EXE_monoruby")).args([
        "-e",
        &format!(
            r#"p require({so:?}); p require({so:?}); p CapiHello.add(1, 2); p $LOADED_FEATURES.grep(/capi_hello/).size"#,
            so = so.display().to_string()
        ),
    ]));
    assert_eq!(out, "true\nfalse\n3\n1\n");
}
