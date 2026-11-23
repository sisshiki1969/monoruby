//! The runtime `MONORUBY_INSTALL_ROOT` override (see `globals::install_root`)
//! that makes distributed binaries relocatable: it must take precedence over
//! the build-time baked path, while an unset or empty variable keeps the
//! baked-path behaviour. Exercised through the built binary with a
//! per-child environment, so the test process's own env is never mutated
//! (spawned interpreters in parallel tests read it).

use std::process::Command;

/// `require` something from the vendored stdlib tree; only resolvable when
/// the install root actually points at an installed `v<version>` tree.
fn require_json(install_root: Option<&str>) -> std::process::Output {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_monoruby"));
    // `json` comes from the install root's `lib/`, not from a gem, so the
    // rubygems boot — most of a spawn's startup cost — is skipped. It also
    // keeps the bogus-root case failing on the `require` under test rather
    // than on rubygems' own load.
    cmd.args([
        "--disable=gems",
        "-e",
        r#"require "json"; puts JSON.generate({"ok" => 1})"#,
    ]);
    if let Some(root) = install_root {
        cmd.env("MONORUBY_INSTALL_ROOT", root);
    } else {
        cmd.env_remove("MONORUBY_INSTALL_ROOT");
    }
    cmd.output().unwrap()
}

fn assert_ok(out: &std::process::Output, case: &str) {
    assert!(out.status.success(), "{case}: {out:?}");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "{\"ok\":1}\n", "{case}");
}

#[test]
fn baked_path_used_when_env_unset() {
    assert_ok(&require_json(None), "unset env should use the baked path");
}

#[test]
fn empty_env_is_ignored() {
    assert_ok(&require_json(Some("")), "empty env should use the baked path");
}

#[test]
fn env_override_is_honoured() {
    // Pointing the override at the installed tree explicitly must work
    // (the env-var arm, not the baked default)…
    let baked = env!("MONORUBY_INSTALL_ROOT");
    assert_ok(
        &require_json(Some(baked)),
        "explicit valid override should resolve requires",
    );

    // …and pointing it somewhere bogus must actually be honoured, i.e. the
    // stdlib becomes unresolvable instead of silently falling back.
    let out = require_json(Some("/nonexistent/monoruby-install-root"));
    assert!(
        !out.status.success(),
        "bogus override must not fall back to the baked path: {out:?}"
    );
}

/// A fresh directory holding `root`, a symlink to the baked install root.
fn symlinked_root(name: &str) -> (std::path::PathBuf, std::path::PathBuf) {
    let dir = std::env::temp_dir().join(format!("monoruby-{name}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let link = dir.join("root");
    std::os::unix::fs::symlink(env!("MONORUBY_INSTALL_ROOT"), &link).unwrap();
    (dir, link)
}

/// Wait for `child`, killing it after a minute: a regression of #2152
/// shows as a loop that never ends (`Array#each` running on a redefined
/// `Integer#<`), which must fail this test and not hang the suite.
fn output_within_a_minute(mut child: std::process::Child, case: &str) -> (String, String) {
    use std::io::Read;
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(60);
    let status = loop {
        if let Some(status) = child.try_wait().unwrap() {
            break status;
        }
        if std::time::Instant::now() > deadline {
            let _ = child.kill();
            let _ = child.wait();
            panic!("{case}: still running after a minute");
        }
        std::thread::sleep(std::time::Duration::from_millis(20));
    };
    // The scripts here print a few lines, far less than a pipe holds, so
    // reading after the exit cannot have blocked the child.
    let (mut stdout, mut stderr) = (String::new(), String::new());
    let (mut out, mut err) = (child.stdout.take().unwrap(), child.stderr.take().unwrap());
    out.read_to_string(&mut stdout).unwrap();
    err.read_to_string(&mut stderr).unwrap();
    assert!(status.success(), "{case}: {status:?}\nstderr: {stderr}");
    (stdout, stderr)
}

fn run_with_root(root: &std::path::Path, args: &[&str], case: &str) -> (String, String) {
    let child = Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .args(args)
        .env("MONORUBY_INSTALL_ROOT", root)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .unwrap();
    output_within_a_minute(child, case)
}

/// The baked install root, and the same tree reached through a symlink.
fn real_root() -> &'static std::path::Path {
    env!("MONORUBY_INSTALL_ROOT").as_ref()
}

/// The files of `builtins/` are the interpreter's own whichever way the
/// install root is spelled (#2152). Through a symlink they used to count
/// as user code: the `__` helpers were listed among `Kernel`'s methods,
/// the loops of `Array#each` / `#map` ran on a redefined `Integer#<`, and
/// their frames showed the install path instead of `<internal:array>`.
#[test]
fn builtins_are_internal_under_a_symlinked_root() {
    // What CRuby answers too.
    const SCRIPT: &str = r#"
        p Kernel.private_instance_methods.grep(/\A__/).size
        e = ([1].each { raise "x" } rescue $!)
        p e.backtrace.none? { |l| l.include?("/builtins/") }
        class Integer; def <(o) = :OV; end
        p [10, 20, 30].map { |x| x }
        p [10, 20, 30].each { |x| x }
    "#;
    // monoruby's own rendering of a builtin's frame.
    const FRAMES: &str = r#"
        e = ([1].each { raise "x" } rescue $!)
        puts e.backtrace
    "#;
    let (dir, link) = symlinked_root("symlinked-root");
    let args = ["--disable=gems", "-e", SCRIPT];
    let (linked, _) = run_with_root(&link, &args, "symlinked root");
    let (real, _) = run_with_root(real_root(), &args, "real root");
    let ruby = Command::new(monoruby_test_support::ruby_path())
        .args(args)
        .output()
        .unwrap();
    assert!(ruby.status.success(), "CRuby: {ruby:?}");
    let ruby = String::from_utf8_lossy(&ruby.stdout);
    assert_eq!(real, ruby, "the real root should answer as CRuby does");
    assert_eq!(linked, ruby, "the linked root should answer as CRuby does");

    let args = ["--disable=gems", "-e", FRAMES];
    let (linked, _) = run_with_root(&link, &args, "symlinked root");
    let (real, _) = run_with_root(real_root(), &args, "real root");
    assert!(
        real.contains("<internal:array>:"),
        "Array#each should have an internal frame: {real}"
    );
    assert_eq!(linked, real, "frames depend on the root's spelling");
    let _ = std::fs::remove_dir_all(&dir);
}

/// The other three questions asked of a loaded file's path, each of which
/// a symlinked root used to answer the wrong way round:
/// - monoruby's own Ruby is exempt from chilled string literals
///   (`chill_literal_template`), so mutating one of its strings does not
///   warn under `Warning[:deprecated]`;
/// - a stub's top-level feature is offered to an already loaded rubygems
///   (`Gem.try_activate`, in `Executor::require`);
/// - a stand-in that is already loaded is not run a second time under the
///   name of a foreign `.so` (`Globals::require_lib_file`).
#[test]
fn the_ruby_trees_are_recognized_under_a_symlinked_root() {
    const SCRIPT: &str = r#"
        Warning[:deprecated] = true
        File.dirname("/") << "x"   # a literal of builtins/file.rb

        require "rubygems"
        $activated = []
        Gem.singleton_class.prepend(Module.new {
          def try_activate(feature) = ($activated << feature; super)
        })
        require "json"
        p $activated

        require "strscan"
        begin
          p require("strscan.so")
        rescue LoadError
          p $!.class
        end
    "#;
    let (dir, link) = symlinked_root("symlinked-trees");
    // A `strscan.so` on `$LOAD_PATH` that is no library at all: the
    // candidate `require "strscan.so"` is left with, whose stand-in
    // (`lib/strscan.rb`) is already loaded.
    let libdir = dir.join("lib");
    std::fs::create_dir_all(&libdir).unwrap();
    std::fs::write(libdir.join("strscan.so"), "not a library\n").unwrap();
    let libdir = libdir.to_string_lossy().into_owned();
    let args = ["-W", "-I", &libdir, "-e", SCRIPT];
    let (linked_out, linked_err) = run_with_root(&link, &args, "symlinked root");
    let (real_out, real_err) = run_with_root(real_root(), &args, "real root");
    assert_eq!(real_out, "[\"json\"]\nLoadError\n", "real root\n{real_err}");
    assert_eq!(linked_out, real_out, "symlinked root\n{linked_err}");
    for (case, err) in [("real root", &real_err), ("symlinked root", &linked_err)] {
        assert!(
            !err.contains("will be frozen in the future") && !err.contains("already initialized"),
            "{case}: {err}"
        );
    }
    let _ = std::fs::remove_dir_all(&dir);
}
