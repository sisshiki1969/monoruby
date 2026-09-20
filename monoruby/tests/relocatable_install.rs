extern crate monoruby;
use monoruby::tests::{extensions_installed, install_root};
use std::path::{Path, PathBuf};
use std::process::Command;

// An installed monoruby is two things: the binary, and the tree of Ruby
// and extensions it loads. `cargo install` copies only the first, which
// is how an installed interpreter came to be unable to `require "psych"`
// — rubygems reads every gem's metadata through it. The extensions are
// now Cargo's own artifact dependencies, installed by `build.rs` beside
// the vendored stdlib, and the root is found relative to the executable
// when one ships with it, so a release archive runs from anywhere.

/// The install root, when this build is one that populated it. A cross
/// build installs no extensions (the host's root would get the target's
/// `.so` files), so there is nothing here to assemble a tree from.
fn populated_install_root() -> Option<PathBuf> {
    if !extensions_installed() {
        eprintln!("skipped: a cross build installs no extensions");
        return None;
    }
    let root = install_root();
    if !root.join("builtins").is_dir() {
        eprintln!("skipped: no install root at {}", root.display());
        return None;
    }
    Some(root)
}

fn tree(name: &str) -> Option<PathBuf> {
    let root = populated_install_root()?;
    let dir = std::env::temp_dir().join(format!("monoruby-{name}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(dir.join("bin")).unwrap();
    // The binary and the extensions are copied; the three Ruby trees are
    // symlinked, which the layout check and every read are happy with and
    // which keeps this from copying 12 MB per run.
    std::fs::copy(
        env!("CARGO_BIN_EXE_monoruby"),
        dir.join("bin").join("monoruby"),
    )
    .unwrap();
    std::fs::create_dir_all(dir.join("ext")).unwrap();
    for entry in std::fs::read_dir(root.join("ext")).unwrap() {
        let path = entry.unwrap().path();
        if path.is_file() {
            std::fs::copy(&path, dir.join("ext").join(path.file_name().unwrap())).unwrap();
        }
    }
    for name in ["builtins", "lib", "stub"] {
        std::os::unix::fs::symlink(root.join(name), dir.join(name)).unwrap();
    }
    Some(dir)
}

/// Run the tree's own binary with nothing pointing at it: no
/// `MONORUBY_INSTALL_ROOT`, no `MONORUBY_EXT_PATH`, and a `HOME` that
/// holds no monoruby install at all. Returns stdout and stderr.
fn run2(dir: &Path, code: &str) -> (String, String) {
    let home = dir.join("home");
    std::fs::create_dir_all(&home).unwrap();
    let out = Command::new(dir.join("bin").join("monoruby"))
        .args(["-e", code])
        .env_remove("MONORUBY_INSTALL_ROOT")
        .env_remove("MONORUBY_EXT_PATH")
        .env_remove("MONORUBY_GEM_PATH")
        .env_remove("MONORUBY_REPROBE")
        .env_remove("MONORUBY_RUBY")
        .env_remove("GEM_PATH")
        .env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .env("HOME", &home)
        .output()
        .expect("failed to spawn the relocated binary");
    assert!(
        out.status.success(),
        "exited with {:?}\nstderr: {}",
        out.status,
        String::from_utf8_lossy(&out.stderr)
    );
    (
        String::from_utf8_lossy(&out.stdout).trim().to_string(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

fn run(dir: &Path, code: &str) -> String {
    run2(dir, code).0
}

/// The five extensions `build.rs` installs are all loadable from the
/// install root, with nothing on `MONORUBY_EXT_PATH`.
#[test]
fn the_installed_extensions_load() {
    let Some(root) = populated_install_root() else {
        return;
    };
    let ext = root.join("ext");
    for name in [
        "psych_native",
        "zlib_native",
        "zstd_native",
        "sqlite3_native",
        "nokogiri_native",
    ] {
        let dylib = if cfg!(target_os = "macos") {
            format!("lib{name}.dylib")
        } else {
            format!("lib{name}.so")
        };
        assert!(
            ext.join(&dylib).is_file(),
            "{dylib} is missing from {}",
            ext.display()
        );
    }
}

/// A tree laid out as `bin/dist` writes it runs from anywhere, under any
/// home, with no environment set: it finds its Ruby and its extensions
/// from the executable's own path.
#[test]
fn a_relocated_tree_finds_its_own_ruby_and_extensions() {
    let Some(dir) = tree("relocated") else { return };
    // The interpreter's own `$LOAD_PATH` entries come from the tree, and
    // none from the baked root. (The rest of `$LOAD_PATH` is the host's
    // gem directories, which the runtime probe adds either way.)
    let answer = run(
        &dir,
        &format!(
            "p [$LOAD_PATH.any? {{ |d| d.start_with?({:?}) }}, \
                $LOAD_PATH.none? {{ |d| d.start_with?({:?}) }}]",
            dir.to_string_lossy(),
            install_root().to_string_lossy(),
        ),
    );
    assert_eq!(
        answer, "[true, true]",
        "$LOAD_PATH should come from the tree and not from the baked root"
    );
    // And the extensions come with it — psych is the one rubygems needs.
    assert_eq!(
        run(&dir, r#"require "psych"; p Psych.load("a: 1")"#),
        r#"{"a" => 1}"#
    );
    assert_eq!(
        run(&dir, r#"require "zlib"; p Zlib.crc32("abc")"#),
        "891568578"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// `MONORUBY_INSTALL_ROOT` still wins over the shipped tree.
#[test]
fn the_environment_still_overrides_the_shipped_tree() {
    let Some(dir) = tree("override") else { return };
    let home = dir.join("home");
    std::fs::create_dir_all(&home).unwrap();
    let out = Command::new(dir.join("bin").join("monoruby"))
        .args(["-e", "p $LOAD_PATH.any? { |d| d.start_with?(ENV['MONORUBY_INSTALL_ROOT']) }"])
        .env("MONORUBY_INSTALL_ROOT", install_root())
        .env_remove("MONORUBY_EXT_PATH")
        .env("HOME", &home)
        .output()
        .expect("failed to spawn");
    assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
    assert_eq!(String::from_utf8_lossy(&out.stdout).trim(), "true");
    let _ = std::fs::remove_dir_all(&dir);
}

/// The host-Ruby probe caches what it found under `~/.monoruby`. On a
/// machine that only ever unpacked a release archive that directory does
/// not exist — nothing created it, since `build.rs` ran on whoever built
/// the archive. The probe has to make it, or the cache write fails
/// silently and *every* start re-spawns `ruby` and warns about the
/// library path file it could not read.
#[test]
fn a_fresh_home_gets_its_probe_cache_written() {
    let Some(dir) = tree("freshhome") else { return };
    let (_, stderr) = run2(&dir, "p 1");
    let cache = dir.join("home").join(".monoruby");
    if !cache.join("gem_path").is_file() {
        // No host Ruby to probe: warning and all, that is the documented
        // behaviour, and there is nothing to cache.
        eprintln!("skipped: the probe found no host ruby");
        let _ = std::fs::remove_dir_all(&dir);
        return;
    }
    assert!(
        cache.join("library_path").is_file(),
        "the probe wrote gem_path but not library_path"
    );
    let probed_ruby = cache.join("probed_ruby");
    assert!(
        probed_ruby.is_file(),
        "the probe cached its paths but not which ruby they came from, \
         so every later start would re-probe to establish the record"
    );
    // Second start: the cache is there, so no warning.
    let (_, stderr2) = run2(&dir, "p 1");
    assert!(
        !stderr2.contains("failed to read library path file"),
        "warned again although the cache was written\nfirst: {stderr}\nsecond: {stderr2}"
    );
    // ... and the probing settles. A host whose closest Ruby is not the
    // one `ruby` resolves to gets one more probe (the second start moves
    // the cache onto it), so give it that start and then require the
    // third to leave the record alone — otherwise `preferred_ruby_changed`
    // would be spawning a Ruby on every single start.
    let settled = std::fs::metadata(&probed_ruby).unwrap().modified().unwrap();
    run2(&dir, "p 1");
    assert_eq!(
        std::fs::metadata(&probed_ruby).unwrap().modified().unwrap(),
        settled,
        "the cache was re-probed although nothing about the host moved"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
