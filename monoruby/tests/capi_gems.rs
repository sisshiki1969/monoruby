//! Real gems with a C extension, built for monoruby by `gem install`
//! through the C API layer (`doc/c_api.md` §3.11): the `.gem` the host
//! CRuby has in its cache is installed into a gem home of monoruby's own,
//! and `tests/capi/gems/<gem>.rb` is run on that and on the host CRuby
//! (with the gem it installed itself); both must print the same. The
//! script's first line names the native extensions it loaded, so the
//! monoruby side also proves the C extension won over the stand-in in
//! `gem/`.
//!
//! A gem whose `.gem` is not in the host's cache is skipped (CI installs
//! them; `gem install bcrypt msgpack` does locally).

use std::path::{Path, PathBuf};
use std::process::Command;

fn manifest_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn monoruby() -> Command {
    Command::new(env!("CARGO_BIN_EXE_monoruby"))
}

fn run(cmd: &mut Command) -> String {
    let out = cmd.output().unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    if !out.status.success() {
        let logs: Vec<String> = cmd
            .get_current_dir()
            .map(glob_logs)
            .unwrap_or_default()
            .into_iter()
            .map(|p| format!("--- {}\n{}", p.display(), std::fs::read_to_string(&p).unwrap_or_default()))
            .collect();
        panic!(
            "{cmd:?} failed\n--- stdout\n{stdout}\n--- stderr\n{}\n{}",
            String::from_utf8_lossy(&out.stderr),
            logs.join("\n")
        );
    }
    stdout
}

/// Every `mkmf.log` / `gem_make.out` under `dir` (a gem home, after a
/// failed `gem install`).
fn glob_logs(dir: &Path) -> Vec<PathBuf> {
    let mut found = Vec::new();
    let Ok(entries) = std::fs::read_dir(dir) else {
        return found;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            found.extend(glob_logs(&path));
        } else if matches!(
            path.file_name().and_then(|n| n.to_str()),
            Some("mkmf.log" | "gem_make.out")
        ) {
            found.push(path);
        }
    }
    found
}

fn has(cmd: &str, arg: &str) -> bool {
    Command::new(cmd)
        .arg(arg)
        .output()
        .is_ok_and(|o| o.status.success())
}

fn have_build_tools() -> bool {
    let cc = std::env::var("CC").unwrap_or_else(|_| "cc".to_string());
    has(&cc, "--version") && has("make", "--version")
}

fn have_cruby() -> bool {
    has("ruby", "--version")
}

/// A fresh directory under the target's tmp dir.
fn work_dir(name: &str) -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("capi_gems").join(name);
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

/// The newest `<name>-<version>.gem` in the host CRuby's gem caches,
/// with its version — the gem the host installed, so the same version
/// runs on both sides.
fn cached_gem(name: &str) -> Option<(PathBuf, String)> {
    let paths = Command::new("ruby")
        .args(["-e", "puts Gem.path"])
        .output()
        .ok()?;
    let mut found: Vec<(Vec<u64>, String, PathBuf)> = Vec::new();
    for dir in String::from_utf8_lossy(&paths.stdout).lines() {
        let Ok(entries) = std::fs::read_dir(Path::new(dir).join("cache")) else {
            continue;
        };
        for entry in entries.flatten() {
            let file = entry.file_name().to_string_lossy().into_owned();
            let Some(stem) = file.strip_suffix(".gem") else {
                continue;
            };
            let Some(version) = stem.strip_prefix(&format!("{name}-")) else {
                continue;
            };
            // `1.8.5`, not `1.8.5-x86_64-linux` (a prebuilt binary gem).
            if version.is_empty() || !version.bytes().all(|b| b.is_ascii_digit() || b == b'.') {
                continue;
            }
            let key: Vec<u64> = version.split('.').map(|p| p.parse().unwrap_or(0)).collect();
            found.push((key, version.to_string(), entry.path()));
        }
    }
    found.sort();
    found.pop().map(|(_, version, path)| (path, version))
}

/// Installs the cached `.gem` with monoruby's `gem` into a home of its
/// own, runs the gem's script there and on the host CRuby, and compares.
fn check_gem(name: &str) {
    if !have_build_tools() {
        eprintln!("skipped: no C compiler or make");
        return;
    }
    if !have_cruby() {
        eprintln!("skipped: no host ruby");
        return;
    }
    let Some((gem_file, version)) = cached_gem(name) else {
        eprintln!("skipped: {name}-*.gem is not in the host's gem cache");
        return;
    };
    let script = manifest_dir().join(format!("tests/capi/gems/{name}.rb"));
    let dir = work_dir(name);
    let home = dir.join("home");
    let root = PathBuf::from(run(monoruby().args(["-e", r#"print RbConfig::CONFIG["rubyhdrdir"]"#])));
    let mono_gem = root.parent().unwrap().join("bin/gem");
    let mut install = monoruby();
    install
        .current_dir(&dir)
        .arg(&mono_gem)
        .args(["install", "--local", "--no-document", "--ignore-dependencies"])
        .arg(&gem_file)
        .env("GEM_HOME", &home)
        .env("GEM_PATH", &home)
        .env_remove("RUBYOPT")
        .env_remove("BUNDLE_GEMFILE");
    let installed = install.output().unwrap();
    if !installed.status.success() {
        // DIAGNOSTIC (temporary): the darwin runner aborts monoruby with a
        // stack overflow inside msgpack's extconf.rb; rerun it under lldb
        // for the native backtrace.
        let ext_dir = home.join(format!("gems/{name}-{version}/ext"));
        let ext_dir = std::fs::read_dir(&ext_dir)
            .ok()
            .and_then(|d| d.flatten().map(|e| e.path()).find(|p| p.join("extconf.rb").is_file()))
            .unwrap_or(ext_dir);
        let lldb = Command::new("lldb")
            .current_dir(&ext_dir)
            // `-k`: run on a crash stop (a plain `-o` after `run` is not,
            // batch mode drops to the prompt there).
            .args([
                "--batch",
                "-o",
                "run",
                "-k",
                "thread backtrace -c 400",
                "-k",
                "register read sp fp",
                "-k",
                "image dump sections monoruby",
                "-k",
                "quit",
                "--",
            ])
            .arg(env!("CARGO_BIN_EXE_monoruby"))
            .arg("-I")
            .arg(root.parent().unwrap().join("lib"))
            .arg("extconf.rb")
            .env("GEM_HOME", &home)
            .env("GEM_PATH", &home)
            .env_remove("RUBYOPT")
            .env_remove("BUNDLE_GEMFILE")
            .output();
        let lldb = match lldb {
            Ok(o) => format!(
                "--- lldb stdout\n{}\n--- lldb stderr\n{}",
                String::from_utf8_lossy(&o.stdout),
                String::from_utf8_lossy(&o.stderr)
            ),
            Err(e) => format!("lldb: {e}"),
        };
        let logs: Vec<String> = glob_logs(&home)
            .into_iter()
            .map(|p| format!("--- {}\n{}", p.display(), std::fs::read_to_string(&p).unwrap_or_default()))
            .collect();
        panic!(
            "{install:?} failed\n--- stdout\n{}\n--- stderr\n{}\n{}\n{lldb}",
            String::from_utf8_lossy(&installed.stdout),
            String::from_utf8_lossy(&installed.stderr),
            logs.join("\n")
        );
    }
    let out = String::from_utf8_lossy(&installed.stdout).into_owned();
    assert!(out.contains("Building native extensions"), "{out}");
    assert!(out.contains("1 gem installed"), "{out}");
    let on_monoruby = run(monoruby()
        .current_dir(&dir)
        .arg(&script)
        .env("GEM_HOME", &home)
        .env("GEM_PATH", &home)
        .env("CAPI_GEM_VERSION", &version)
        .env_remove("RUBYOPT")
        .env_remove("BUNDLE_GEMFILE"));
    let on_cruby = run(Command::new("ruby")
        .current_dir(&dir)
        .arg(&script)
        .env("CAPI_GEM_VERSION", &version)
        .env_remove("RUBYOPT")
        .env_remove("BUNDLE_GEMFILE"));
    // The first line is the gem's own extension, loaded (not the stand-in
    // in `gem/`, which would leave it empty).
    assert_ne!(on_monoruby.lines().next(), Some("[]"), "{on_monoruby}");
    assert_eq!(on_monoruby, on_cruby);
}

/// bcrypt: one C file over crypt_blowfish (`rb_str_new`, `StringValuePtr`,
/// `rb_define_singleton_method`, `NUM2INT`).
#[test]
fn bcrypt() {
    check_gem("bcrypt");
}

/// msgpack: TypedData objects with their own alloc, `rb_num2ll` /
/// `rb_num2ull` and the Bignum conversions, `rb_hash_foreach`,
/// `rb_funcall` with blocks, `rb_io_write` for a packer on an IO,
/// `rb_enc_*` on strings, `rb_ext_ractor_safe`, and `-fvisibility=hidden`
/// with `RUBY_FUNC_EXPORTED` on `Init_msgpack`.
#[test]
fn msgpack() {
    check_gem("msgpack");
}
