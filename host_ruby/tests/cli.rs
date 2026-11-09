//! The `host-ruby` binary, as `bin/test` and the other scripts call it.

use std::path::Path;
use std::process::Command;

fn fake_ruby(at: &Path, version: &str) {
    use std::os::unix::fs::PermissionsExt;
    std::fs::create_dir_all(at.parent().unwrap()).unwrap();
    std::fs::write(at, format!("#!/bin/sh\necho {version}\necho \"$0\"\n")).unwrap();
    std::fs::set_permissions(at, std::fs::Permissions::from_mode(0o755)).unwrap();
}

fn host_ruby(home: &Path) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_host-ruby"));
    // An empty home with no `ruby` on PATH: only what the test plants.
    cmd.env("HOME", home)
        .env("PATH", home)
        .env_remove("RBENV_ROOT")
        .env_remove("ASDF_DATA_DIR")
        .env_remove("MONORUBY_RUBY");
    cmd
}

#[test]
fn prints_the_executable_it_finds() {
    let tmp = tempfile::tempdir().unwrap();
    let ruby = tmp
        .path()
        .join(".rbenv/versions")
        .join(host_ruby::PINNED_RUBY_VERSION.trim())
        .join("bin/ruby");
    fake_ruby(&ruby, host_ruby::PINNED_RUBY_VERSION.trim());
    let out = host_ruby(tmp.path()).output().unwrap();
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&out.stdout).trim(),
        ruby.display().to_string()
    );
    // MONORUBY_RUBY names one explicitly.
    let other = tmp.path().join("other/ruby");
    fake_ruby(&other, "4.2.0");
    let out = host_ruby(tmp.path())
        .env("MONORUBY_RUBY", &other)
        .output()
        .unwrap();
    assert_eq!(
        String::from_utf8_lossy(&out.stdout).trim(),
        other.display().to_string()
    );
}

#[test]
fn exits_1_with_a_message_when_there_is_no_ruby() {
    let tmp = tempfile::tempdir().unwrap();
    let out = host_ruby(tmp.path()).output().unwrap();
    assert_eq!(out.status.code(), Some(1));
    assert!(out.stdout.is_empty());
    let err = String::from_utf8_lossy(&out.stderr);
    assert!(
        err.contains("no CRuby") && err.contains("MONORUBY_RUBY"),
        "{err}"
    );
}
