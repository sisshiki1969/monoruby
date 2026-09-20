use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::{fs, io};

/// Minimum CRuby version whose `RUBY_PLATFORM` monoruby is willing to bake
/// at build time. Older Rubies (Debian's system 3.0, for example) lack APIs
/// monoruby relies on and report a platform string monoruby would not pair
/// with the vendored 4.x snapshot.
const MIN_RUBY_VERSION: (u32, u32) = (4, 0);

fn ruby_version_ok(ruby_cmd: &str) -> bool {
    let Ok(output) = Command::new(ruby_cmd)
        .args(["-e", "puts RUBY_VERSION"])
        .output()
    else {
        return false;
    };
    if !output.status.success() {
        return false;
    }
    let s = String::from_utf8_lossy(&output.stdout);
    let mut parts = s.trim().split('.').map(|p| p.parse::<u32>().ok());
    let major = parts.next().flatten();
    let minor = parts.next().flatten();
    match (major, minor) {
        (Some(maj), Some(min)) => (maj, min) >= MIN_RUBY_VERSION,
        _ => false,
    }
}

/// Pick a `ruby` executable that is at least `MIN_RUBY_VERSION`. Returns
/// `None` if no suitable Ruby is found. The build never *requires* this:
/// it is consulted only to recover the host `RUBY_PLATFORM` (best effort),
/// and everything else is derived from the checked-in vendored snapshot.
fn find_ruby() -> Option<String> {
    if ruby_version_ok("ruby") {
        return Some("ruby".to_string());
    }
    if let Some(home) = std::env::var_os("HOME") {
        let shim = PathBuf::from(home).join(".rbenv/shims/ruby");
        if let Some(s) = shim.to_str()
            && ruby_version_ok(s)
        {
            return Some(s.to_string());
        }
    }
    if let Some(home) = std::env::var_os("HOME") {
        let rvm = PathBuf::from(home).join(".rvm/bin/ruby");
        if let Some(s) = rvm.to_str()
            && ruby_version_ok(s)
        {
            return Some(s.to_string());
        }
    }
    None
}

fn main() {
    // The JIT is always compiled in (the `--no-jit` runtime flag disables it
    // at run time; there is no build-time switch). The front-end
    // (bytecode→TraceIR→AsmIR) and arch backends are selected purely by
    // `target_arch`: x86-64 emits via `monoasm!` (plus the builtins inline
    // generators), aarch64 uses its own AsmIR→A64 lowering
    // (`asmir/compile_stub.rs`, gated by `target_arch = "aarch64"`), where
    // inlined methods fall back to a plain call and the lowering deopts to the
    // VM on anything not yet supported (see doc/aarch64-jitgen-plan.md).
    //
    // Host-Ruby decoupling: this build script no longer shells out to a host
    // `ruby` for `$LOAD_PATH`, `Gem.paths.path`, or `RUBY_VERSION`. The
    // pure-Ruby stdlib + default gems are vendored
    // (`vendor/ruby-stdlib`, copied below), the reported `RUBY_VERSION` is
    // read from that snapshot's pin marker, and host-installed *non-default*
    // gems are discovered at run time by the probe in `ruby_probe.rs`. The
    // result is a reproducible, host-independent build: `cargo build`
    // succeeds identically whether or not a host Ruby is present.

    // Top-level state dir. Holds version-independent, host-derived state
    // shared across builds: the runtime probe cache (`library_path` /
    // `gem_path`, written at run time) and human-facing breadcrumbs.
    let lib_path = dirs::home_dir().unwrap().join(".monoruby");

    // The installed stdlib/builtins/stub trees go into a *per-version*
    // subdirectory so concurrent builds, multiple checkouts, and version
    // upgrades never clobber a single shared tree (the previous layout had
    // every build `rm -rf` and repopulate `~/.monoruby/{lib,builtins,stub}`
    // in place, which races a concurrently-running monoruby and lets a
    // stale checkout's binary read another's files). The absolute path is
    // baked into the binary as `MONORUBY_INSTALL_ROOT` and read back at run
    // time (see `globals::install_root`).
    let install_root = lib_path.join(format!("v{}", env!("CARGO_PKG_VERSION")));
    println!(
        "cargo:rustc-env=MONORUBY_INSTALL_ROOT={}",
        install_root.display()
    );

    // The install is staged in a private directory and swapped into place at
    // the end, so a concurrently-running monoruby observes either the old
    // complete tree or the new one — never a half-copied directory. The pid
    // keeps concurrent builders' staging dirs distinct.
    let staging = lib_path.join(format!(".staging-{}", std::process::id()));
    let staging_lib = staging.join("lib");
    let staging_builtins = staging.join("builtins");
    // `stub` holds *only* monoruby's own C-extension replacement stubs
    // (the `stdlib/` and `gem/` trees). The require resolver
    // (`search_lib`) pins this directory ahead of `$LOAD_PATH` so those
    // stubs win even after rubygems/bundler activates a host gem and
    // unshifts its lib dir to the front of `$LOAD_PATH`. It deliberately
    // excludes the vendored CRuby snapshot (bundler / rubygems), whose
    // code version must stay in lockstep with the activated gem spec.
    let staging_stub = staging.join("stub");
    // Order matters: the checked-in CRuby stdlib snapshot is laid down
    // first, then monoruby's own builtins/stdlib/gem stubs overwrite any
    // name clash so monoruby's host-independent implementations of
    // C-extension-backed libraries stay authoritative. No `ruby` is
    // invoked here — the snapshot is produced offline by
    // bin/vendor-ruby-stdlib, so the build works without CRuby. The
    // `stdlib/` and `gem/` trees are additionally laid down into `stub`
    // (the pinned resolution root; see `search_lib`).
    let sources = [
        (PathBuf::from("vendor/ruby-stdlib"), staging_lib.clone()),
        (PathBuf::from("builtins"), staging_builtins.clone()),
        (PathBuf::from("stdlib"), staging_lib.clone()),
        (PathBuf::from("gem"), staging_lib.clone()),
        (PathBuf::from("stdlib"), staging_stub.clone()),
        (PathBuf::from("gem"), staging_stub.clone()),
    ];

    // Re-run when the installed sources change so edits to the vendored
    // stdlib or monoruby's stubs reinstall on the next build.
    for (src, _) in &sources {
        println!("cargo:rerun-if-changed={}", src.display());
    }
    // The reported Ruby language level is baked from the vendored snapshot's
    // pin marker; re-run when it changes so MONORUBY_RUBY_VERSION stays in
    // lockstep with the stdlib that actually ships.
    let ruby_version_file = PathBuf::from("vendor/ruby-stdlib/.ruby-version");
    println!("cargo:rerun-if-changed={}", ruby_version_file.display());
    // Self-heal: track the install root's stamp file. If the user deletes
    // the install root (or all of ~/.monoruby), the stamp disappears, Cargo
    // sees a tracked input change and re-runs this script, repopulating it.
    let stamp = install_root.join(".build-stamp");
    println!("cargo:rerun-if-changed={}", stamp.display());
    // The extensions have their own stamp, tracked for the same reason:
    // `ext/` is written by `install_extensions` rather than staged with
    // the Ruby trees, so deleting it has to re-run this script too.
    println!(
        "cargo:rerun-if-changed={}",
        install_root.join("ext").join(".build-stamp").display()
    );
    // Whether this build put the extensions in the install root — it does
    // not when cross-compiling (see `install_extensions`). Emitted here,
    // before any early return, because every run of this script has to
    // print the whole of its output. `tests/relocatable_install.rs` reads
    // it to tell "the install is missing its extensions" (a regression)
    // from "this build never installed any" (a cross build, where
    // bin/test-aarch64 builds them next to the binary instead).
    println!(
        "cargo:rustc-env=MONORUBY_EXT_INSTALLED={}",
        if cross_compiling() { "0" } else { "1" }
    );

    fs::create_dir_all(&lib_path).unwrap();
    // Start from a clean staging tree (a stale one may linger if a previous
    // build with the same pid crashed mid-install).
    if staging.exists() {
        fs::remove_dir_all(&staging).unwrap();
    }
    for dir in [&staging_lib, &staging_builtins, &staging_stub] {
        fs::create_dir_all(dir).unwrap();
    }

    // Bake the reported Ruby language level from the vendored snapshot's
    // pin marker (`bin/vendor-ruby-stdlib` writes `.ruby-version`) instead
    // of from whatever host `ruby` happens to be on PATH. This keeps the
    // value deterministic and host-independent: the version monoruby
    // reports always matches the stdlib it actually ships. Differential
    // tests should run against a host CRuby of the same pin. When the
    // marker is missing the runtime falls back to its compiled-in default,
    // so the build still succeeds.
    match fs::read_to_string(&ruby_version_file) {
        Ok(s) => {
            let ver = s.trim();
            if !ver.is_empty() {
                println!("cargo:rustc-env=MONORUBY_RUBY_VERSION={ver}");
                // Human-facing breadcrumb of the pin this build reports;
                // the runtime reads the baked-in env var, not this file.
                let _ = fs::write(lib_path.join("ruby_version"), ver);
            }
        }
        Err(_) => {
            println!(
                "cargo:warning=vendored {} missing; RUBY_VERSION falls back \
                 to the compiled-in default",
                ruby_version_file.display()
            );
        }
    }

    // The commit this binary was built from, reported by `-v` /
    // `RUBY_DESCRIPTION` and as `RUBY_REVISION` (CRuby's convention).
    // Best effort: a build from a source tree without git (e.g. a crate
    // tarball) simply omits it.
    emit_git_revision();

    // Best-effort host `RUBY_PLATFORM`. This is the *only* place the build
    // still consults a host `ruby`, and it is purely additive: when no
    // suitable host Ruby is present the runtime derives the platform from a
    // `cfg!`-based default (see `globals::ruby_platform`). Its sole purpose
    // is to recover the macOS Darwin major version (e.g. `arm64-darwin23`),
    // which rubygems keys each gem's built-extension directory on
    // (`extensions/<arch>/<api>/<gem>`); the `cfg!` default omits it. On
    // Linux the `cfg!` default already equals the host string, so a missing
    // host Ruby costs nothing there.
    if let Some(ruby) = find_ruby()
        && let Ok(output) = Command::new(&ruby)
            .args(["-e", "print(RUBY_PLATFORM)"])
            .output()
        && output.status.success()
    {
        let plat = String::from_utf8_lossy(&output.stdout);
        let plat = plat.trim();
        if !plat.is_empty() {
            println!("cargo:rustc-env=MONORUBY_RUBY_PLATFORM={plat}");
            // Human-facing breadcrumb (mirrors ruby_version above); the
            // runtime reads the baked-in env var, not this file.
            let _ = fs::write(lib_path.join("ruby_platform"), plat);
        }
    }

    for (src, dst) in &sources {
        copy_dir_all(src, dst).unwrap();
    }

    // Content-address the staged tree. When the installed root already
    // holds *identical* content (the common case: rebuilding the same
    // sources under different cargo feature sets, worktrees of the same
    // revision, `cargo test` after `cargo build`, …), skip the swap
    // entirely. A running monoruby resolves the install root file-by-file
    // during startup, so an unnecessary swap is not harmless: the old
    // tree vanishes mid-`require` sequence and the process dies with a
    // spurious LoadError. The stamp stores this hash; it is only
    // rewritten on a real install, so the skip path leaves the tracked
    // stamp untouched and Cargo's fingerprint stable.
    let mut hasher = DefaultHasher::new();
    hash_tree(&staging, &mut hasher).unwrap();
    let tree_stamp = format!("{}:{:016x}", env!("CARGO_PKG_VERSION"), hasher.finish());
    if fs::read_to_string(&stamp).map(|s| s == tree_stamp).unwrap_or(false) {
        let _ = fs::remove_dir_all(&staging);
        // The Ruby trees are already current, but the extensions are
        // Cargo's artifacts rather than ours and may be newer than the
        // copies in `ext/` — install them before taking the skip.
        install_extensions(&install_root);
        return;
    }

    // Swap the fully-staged tree into the versioned install root. `rename`
    // is atomic within a filesystem, so a concurrent reader sees either the
    // prior complete tree or the new one. The prior tree is moved aside
    // with a rename (not deleted in place): the unavailability window for a
    // concurrent reader shrinks to the instant between the two renames
    // instead of a full recursive delete. If the swap races another builder
    // installing the same versioned tree, tolerate the "already present"
    // outcome rather than panicking.
    // `ext/` is not ours: `bin/install` puts the dynamically loaded
    // extensions there after `cargo install` (see `ext::search_dirs`). Carry
    // it over into the new tree, or every reinstall of the Ruby sources
    // would leave the installed binary without `psych_native.so`,
    // `zlib_native.so`, … It is moved in only after the tree was hashed, so
    // it never takes part in the stamp.
    let ext = install_root.join("ext");
    if ext.is_dir() {
        let _ = fs::rename(&ext, staging.join("ext"));
    }
    let trash = lib_path.join(format!(".trash-{}", std::process::id()));
    let _ = fs::remove_dir_all(&trash);
    if install_root.exists() {
        let _ = fs::rename(&install_root, &trash);
    }
    if fs::rename(&staging, &install_root).is_err() {
        // Either a concurrent builder won the swap, or staging and the
        // install root straddle filesystems. Fall back to a copy only if no
        // complete tree is present, then drop staging.
        if !install_root.join(".build-stamp").exists() {
            let _ = copy_dir_all(&staging, &install_root);
        } else if staging.join("ext").is_dir() && !ext.exists() {
            let _ = fs::rename(staging.join("ext"), &ext);
        }
        let _ = fs::remove_dir_all(&staging);
    }
    let _ = fs::remove_dir_all(&trash);

    // Write the stamp last so its presence means a complete install. Its
    // path is tracked above via cargo:rerun-if-changed, so deleting the
    // install root makes the next build re-run this script.
    fs::write(&stamp, tree_stamp).unwrap();

    install_extensions(&install_root);
}

/// Put the dynamically loaded extensions Cargo just built into the
/// install root's `ext/`, where the installed binary looks for them
/// (`ext::search_dirs`).
///
/// They arrive as *artifact dependencies* (`monoruby/Cargo.toml`
/// `[build-dependencies]`): Cargo builds each `cdylib` and names its
/// path in `CARGO_CDYLIB_FILE_<CRATE>`. Copying them here is what makes
/// `cargo install --path monoruby` self-sufficient — it copies only
/// `[[bin]]` targets, so before this an installed monoruby had no
/// `psych_native.so` and could not `require "psych"`, which rubygems
/// does to read any gem's metadata.
///
/// Skipped when cross-compiling: the artifacts are then for the target
/// platform and the install root belongs to the host's binary, which
/// would no longer be able to load them. A cross build's own binary
/// finds them next to itself in `target/<triple>/<profile>/` instead.
fn install_extensions(install_root: &Path) {
    if cross_compiling() {
        return;
    }
    let ext = install_root.join("ext");
    if fs::create_dir_all(&ext).is_err() {
        return;
    }
    // Cargo names each artifact twice — `CARGO_CDYLIB_FILE_<CRATE>` and
    // its `_<lib name>` suffixed twin — with the same value, so collect
    // the paths and de-duplicate on those rather than on the key's shape
    // (counting underscores in the name would quietly drop an extension
    // crate that has none).
    let mut sources: Vec<PathBuf> = std::env::vars()
        .filter(|(key, _)| key.starts_with("CARGO_CDYLIB_FILE_"))
        .map(|(_, path)| PathBuf::from(path))
        .collect();
    sources.sort();
    sources.dedup();

    let mut installed = vec![];
    for src in sources {
        let Some(name) = src.file_name() else { continue };
        let dst = ext.join(name);
        // A monoruby running from this root may have the old copy open;
        // replacing the directory entry (write beside, rename over) keeps
        // its mapping valid where writing in place would not.
        let tmp = ext.join(format!(".{}.{}", name.to_string_lossy(), std::process::id()));
        if fs::copy(&src, &tmp).is_ok() {
            if fs::rename(&tmp, &dst).is_err() {
                let _ = fs::remove_file(&tmp);
                continue;
            }
            installed.push(name.to_string_lossy().into_owned());
        }
    }
    // Written last, and tracked as an input: deleting `ext/` (or the whole
    // install root) takes the stamp with it, so Cargo re-runs this script
    // and the extensions come back.
    //
    // The profile goes in it because there is one install root per
    // *version*, shared by every build of it, and unlike the Ruby trees
    // these artifacts differ per profile: the last build wins, so a debug
    // `cargo test` leaves debug extensions where a release `bin/install`
    // had put release ones. That is correct but slower, and the stamp is
    // where to look when an installed monoruby is unexpectedly sluggish
    // inside nokogiri or sqlite3. Re-run `bin/install` to put the release
    // ones back.
    installed.sort();
    let profile = std::env::var("PROFILE").unwrap_or_else(|_| "unknown".to_string());
    let _ = fs::write(
        ext.join(".build-stamp"),
        format!("{profile}\n{}", installed.join("\n")),
    );
}

/// Is this build producing artifacts for a platform other than the one
/// it runs on? Cargo tells the build script both.
fn cross_compiling() -> bool {
    std::env::var("TARGET").ok() != std::env::var("HOST").ok()
}

fn git(args: &[&str]) -> Option<String> {
    let output = Command::new("git").args(args).output().ok()?;
    if !output.status.success() {
        return None;
    }
    let s = String::from_utf8(output.stdout).ok()?;
    let s = s.trim();
    (!s.is_empty()).then(|| s.to_string())
}

/// Bake the HEAD commit hash into `MONORUBY_GIT_REVISION`.
///
/// The script is re-run when HEAD moves: `HEAD` itself changes on a
/// checkout, the branch's loose ref (or `packed-refs`) on a commit. Paths
/// come from `git rev-parse --git-path`, so worktrees resolve HEAD to the
/// per-worktree git dir and refs to the common one. Only existing files are
/// tracked — Cargo re-runs a script on every build for a missing one.
fn emit_git_revision() {
    let Some(rev) = git(&["rev-parse", "HEAD"]) else {
        return;
    };
    println!("cargo:rustc-env=MONORUBY_GIT_REVISION={rev}");
    let mut tracked = vec!["HEAD".to_string(), "packed-refs".to_string()];
    if let Some(r) = git(&["rev-parse", "--symbolic-full-name", "HEAD"])
        && r != "HEAD"
    {
        tracked.push(r);
    }
    for name in tracked {
        if let Some(path) = git(&["rev-parse", "--git-path", &name])
            && Path::new(&path).exists()
        {
            println!("cargo:rerun-if-changed={path}");
        }
    }
}

/// Stable content hash of a directory tree: file names, lengths and bytes,
/// walked in sorted order. Not cryptographic — just a fingerprint to detect
/// "nothing changed" between installs.
fn hash_tree(root: &Path, h: &mut DefaultHasher) -> io::Result<()> {
    let mut entries: Vec<_> = fs::read_dir(root)?.collect::<io::Result<Vec<_>>>()?;
    entries.sort_by_key(|e| e.file_name());
    for e in entries {
        let p = e.path();
        h.write(e.file_name().to_string_lossy().as_bytes());
        if p.is_dir() {
            hash_tree(&p, h)?;
        } else {
            let data = fs::read(&p)?;
            h.write_u64(data.len() as u64);
            h.write(&data);
        }
    }
    Ok(())
}

fn copy_dir_all(src: &Path, dst: &Path) -> io::Result<()> {
    if !fs::exists(dst)? {
        fs::create_dir_all(dst)?;
    }
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if from.is_dir() {
            copy_dir_all(&from, &to)?;
        } else {
            fs::copy(&from, &to)?;
        }
    }
    Ok(())
}
