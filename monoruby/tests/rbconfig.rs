extern crate monoruby;

// #772: with no host CRuby configured, RbConfig's prefix must fall back to
// monoruby's own install root, not the vendored snapshot's baked
// build-machine prefix (`/opt/rbenv/versions/ruby-X.Y.Z-custom`, which exists
// on no other machine and made `Gem.default_dir` resolve to a bogus path so
// `Gem::Specification` enumeration silently came up empty).
//
// Forcing an empty `MONORUBY_GEM_PATH` makes monoruby take the no-host path
// regardless of whether a host Ruby happens to be on `PATH`, so this is
// reproducible in CI (where a host Ruby is installed).
#[test]
fn rbconfig_prefix_falls_back_to_install_root() {
    // One of the few spawns that deliberately keeps the rubygems boot
    // (every other integration test passes `--disable=gems`): the
    // regression is about what rubygems resolves `RbConfig`'s prefix to,
    // so booting it is the point.
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .env("MONORUBY_GEM_PATH", "")
        .arg("-e")
        .arg(r#"print RbConfig::CONFIG["rubylibprefix"]"#)
        .output()
        .unwrap();
    assert!(out.status.success());
    let prefix = String::from_utf8_lossy(&out.stdout);
    assert!(
        prefix.contains("/.monoruby/"),
        "rubylibprefix should be under monoruby's install root when no host \
         Ruby is configured, got: {prefix:?}"
    );
}

// `DLEXT` / `SOEXT` come out of the vendored x86_64-linux `rbconfig.rb`
// snapshot, so without the startup override they report `so` on every
// host. Anything that builds a native library's filename out of them —
// a gem `Fiddle.dlopen`ing its own extension, an mkmf-style probe — then
// looks on macOS for a name that only exists on Linux.
#[test]
fn rbconfig_native_suffixes_match_the_host() {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .arg("--disable=gems")
        .arg("-e")
        .arg(r#"print [RbConfig::CONFIG["DLEXT"], RbConfig::CONFIG["SOEXT"]].join(",")"#)
        .output()
        .unwrap();
    assert!(out.status.success());
    let got = String::from_utf8_lossy(&out.stdout);
    let expected = if cfg!(target_os = "macos") {
        "bundle,dylib"
    } else {
        "so,so"
    };
    assert_eq!(got, expected, "DLEXT,SOEXT for this host");
}
