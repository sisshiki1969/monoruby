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
    cmd.args(["-e", r#"require "json"; puts JSON.generate({"ok" => 1})"#]);
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
