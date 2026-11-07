//! Test helpers that do not need the interpreter.
//!
//! An integration test is its own crate, so anything it imports from
//! `monoruby` links the whole interpreter into its binary: 122 MB against
//! the 7 MB a test that only spawns processes actually needs, once per
//! file under `monoruby/tests/`. The helpers a spawning test wants —
//! which `ruby` to compare against, and getting an extension built —
//! never touch interpreter state, so they live here instead and cost it
//! nothing.
//!
//! `monoruby::tests` re-exports these, so an in-process test keeps
//! importing everything from the one place.

use std::path::PathBuf;
use std::sync::LazyLock;

/// The oldest CRuby whose answers the suite is written against.
const MIN_RUBY_VERSION: (u32, u32) = (4, 0);

static RUBY: LazyLock<String> = LazyLock::new(find_ruby);

///
/// The reference CRuby the harness resolved (PATH, then rbenv/rvm shims),
/// as the absolute path of the executable. A test that spawns `ruby`
/// itself must use this rather than `Command::new("ruby")`, which breaks
/// in shells where only a version manager provides Ruby.
///
/// Absolute, not the shim: rbenv's shim picks the version from the
/// `.ruby-version` nearest the *current directory*, so a test that runs
/// `ruby` with its cwd outside the checkout (`../ruby-bench/benchmarks/…`)
/// got whichever Ruby `~/.rbenv/version` names — a different version with
/// a different gem set — while the same command from the checkout got the
/// pinned one. The resolution runs from the workspace root, so every test
/// gets the Ruby the checkout's `.ruby-version` selects wherever it cds.
///
pub fn ruby_path() -> &'static str {
    &RUBY
}

fn find_ruby() -> String {
    let cmd = resolve_ruby(ruby_candidates());
    real_ruby(&cmd).unwrap_or(cmd)
}

///
/// The executable behind `ruby_cmd` — `RbConfig.ruby`, asked from the
/// workspace root so a version manager's shim answers for the checkout.
///
fn real_ruby(ruby_cmd: &str) -> Option<String> {
    let output = std::process::Command::new(ruby_cmd)
        .current_dir(workspace_root())
        .args(["-e", "print RbConfig.ruby"])
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let path = String::from_utf8(output.stdout).ok()?;
    let path = path.trim();
    (!path.is_empty() && std::path::Path::new(path).is_file()).then(|| path.to_string())
}

///
/// The first candidate that answers a new enough `RUBY_VERSION`. With
/// none — no host Ruby at all — the answer is still `ruby`, so the
/// failure a test then hits names the command it could not run rather
/// than a path nobody configured.
///
fn resolve_ruby(candidates: Vec<String>) -> String {
    candidates
        .into_iter()
        .find(|cmd| ruby_version_ok(cmd))
        .unwrap_or_else(|| "ruby".to_string())
}

///
/// Where to look for the reference CRuby, in the order to try: whatever
/// `PATH` gives, then the version managers' own entry points, which are
/// what a shell without their shims on `PATH` still has.
///
fn ruby_candidates() -> Vec<String> {
    let mut candidates = vec!["ruby".to_string()];
    if let Some(home) = std::env::var_os("HOME") {
        let home = PathBuf::from(home);
        // rbenv's shim defers to the version ~/.rbenv/version selects.
        for manager in [".rbenv/shims/ruby", ".rvm/bin/ruby"] {
            if let Some(path) = home.join(manager).to_str() {
                candidates.push(path.to_string());
            }
        }
    }
    candidates
}

fn ruby_version_ok(ruby_cmd: &str) -> bool {
    let Ok(output) = std::process::Command::new(ruby_cmd)
        .current_dir(workspace_root())
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

///
/// Build the extension crate `name` (a `cdylib` workspace member, e.g.
/// `sqlite3_native`) in this test binary's profile, and answer the
/// directory its output landed in — to hand a spawned `monoruby` as
/// `MONORUBY_EXT_PATH`, or to register on the in-process search path
/// (which is what `monoruby::tests::ensure_extension` adds on top).
///
/// Built into `target/ext/` rather than the test's own target dir: the
/// outer `cargo test` holds that directory's lock while the tests run,
/// and a nested `cargo build` on it would wait forever. Once per process
/// per crate; a build failure panics with cargo's output.
///
pub fn build_extension(name: &str) -> PathBuf {
    use std::sync::Mutex;
    static BUILT: Mutex<Vec<String>> = Mutex::new(Vec::new());
    let mut built = BUILT.lock().unwrap();
    let workspace = workspace_root();
    let profile = if cfg!(debug_assertions) {
        "debug"
    } else {
        "release"
    };
    // The library must be built for the arch this test binary runs on,
    // which under `bin/test-aarch64` (a cross build run in qemu) is not
    // the build host's; naming the triple explicitly covers both.
    let triple = format!(
        "{}-{}",
        std::env::consts::ARCH,
        if cfg!(target_os = "macos") {
            "apple-darwin"
        } else {
            "unknown-linux-gnu"
        }
    );
    let target = workspace.join("target/ext");
    let dir = target.join(&triple).join(profile);
    if built.iter().any(|n| n == name) {
        return dir;
    }
    let mut cmd = std::process::Command::new(
        std::env::var("CARGO").unwrap_or_else(|_| "cargo".to_string()),
    );
    cmd.current_dir(&workspace)
        .args(["build", "-p", name, "--target", &triple, "--target-dir"])
        .arg(&target);
    if profile == "release" {
        cmd.arg("--release");
    }
    // Not part of the coverage measurement: under `cargo llvm-cov` the
    // instrumentation flags and profile path are in the environment, and
    // a second profiler runtime inside the loaded library is not wanted.
    for var in [
        "RUSTFLAGS",
        "CARGO_ENCODED_RUSTFLAGS",
        "LLVM_PROFILE_FILE",
        "CARGO_INCREMENTAL",
    ] {
        cmd.env_remove(var);
    }
    let out = cmd.output().expect("failed to run cargo");
    assert!(
        out.status.success(),
        "building extension {name} failed:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    built.push(name.to_string());
    dir
}

///
/// The workspace root, from this crate's own manifest directory.
///
/// `monoruby::tests` derived it from *its* manifest, one level down; this
/// crate sits at the same depth, so the step up is the same.
///
pub fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..")
}

#[cfg(test)]
mod tests {
    use super::*;

    ///
    /// The order matters: `PATH` first, so a shell that already has the
    /// right Ruby is never overridden by a version manager's copy.
    ///
    #[test]
    fn path_is_tried_before_the_version_managers() {
        let candidates = ruby_candidates();
        assert_eq!("ruby", candidates[0]);
        if std::env::var_os("HOME").is_some() {
            assert!(
                candidates.iter().any(|c| c.contains(".rbenv/shims/ruby")),
                "rbenv shim missing from {candidates:?}"
            );
            assert!(
                candidates.iter().any(|c| c.contains(".rvm/bin/ruby")),
                "rvm entry missing from {candidates:?}"
            );
        }
    }

    ///
    /// The three ways `resolve_ruby` walks past a candidate: it cannot
    /// be spawned, it runs and fails (an rbenv shim with no version
    /// selected exits 1), or it succeeds but answers something that is
    /// not a version.
    ///
    #[test]
    fn a_candidate_that_is_not_a_ruby_is_rejected() {
        assert!(!ruby_version_ok("monoruby-no-such-command-exists"));
        assert!(!ruby_version_ok("false"));
        // Runs, exits 0, prints its arguments rather than a version.
        assert!(!ruby_version_ok("echo"));
    }

    ///
    /// With nothing usable to resolve to, the harness still answers
    /// `ruby` — see `resolve_ruby`.
    ///
    #[test]
    fn no_usable_ruby_still_resolves_to_a_name() {
        assert_eq!(
            "ruby",
            resolve_ruby(vec!["monoruby-no-such-command-exists".to_string()])
        );
    }

    ///
    /// Whatever `find_ruby` settled on is the executable behind one of
    /// the places it looked — an absolute path, the same one whichever
    /// directory it is asked from — or, with no usable Ruby, the bare
    /// `ruby` of `resolve_ruby`.
    ///
    #[test]
    fn the_resolved_ruby_is_the_executable_behind_a_candidate() {
        let ruby = ruby_path();
        if ruby == "ruby" {
            assert!(real_ruby("ruby").is_none(), "a usable ruby was not resolved to its path");
            return;
        }
        assert!(std::path::Path::new(ruby).is_absolute(), "{ruby}");
        assert!(ruby_version_ok(ruby), "{ruby}");
        let candidate = ruby_candidates()
            .into_iter()
            .find(|c| real_ruby(c).as_deref() == Some(ruby));
        assert!(candidate.is_some(), "{ruby} is behind none of {:?}", ruby_candidates());
        // Not the shim, so the same answer from a directory outside the
        // checkout, where a version manager could pick another version.
        let elsewhere = std::process::Command::new(ruby)
            .current_dir(std::env::temp_dir())
            .args(["-e", "print RbConfig.ruby"])
            .output()
            .unwrap();
        assert_eq!(String::from_utf8_lossy(&elsewhere.stdout), ruby);
    }

    ///
    /// `build_extension` resolves `target/ext` against this, so a wrong
    /// answer would build into the directory the outer cargo has locked.
    ///
    #[test]
    fn workspace_root_is_the_workspace() {
        assert!(
            workspace_root().join("Cargo.toml").is_file(),
            "no Cargo.toml at {:?}",
            workspace_root()
        );
    }
}
