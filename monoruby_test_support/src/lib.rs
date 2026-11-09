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

static RUBY: LazyLock<String> = LazyLock::new(find_ruby);

///
/// The reference CRuby the harness compares against, as the absolute
/// path of the executable. A test that spawns `ruby` itself must use
/// this rather than `Command::new("ruby")`, which breaks in shells where
/// only a version manager provides Ruby — and, worse, may answer a
/// different Ruby from the one the interpreter under test borrows its
/// gems from.
///
/// It is `host_ruby::find` — the same choice, by the same code, as the
/// interpreter's runtime probe (`ruby_probe`), `build.rs` and `bin/test`
/// make: the installed Ruby closest to the vendored pin, `ruby` on `PATH`
/// winning a tie, `MONORUBY_RUBY` overriding. So the CRuby a test
/// compares against is the CRuby whose gems the spawned monoruby sees,
/// whatever `~/.rbenv/version` names and wherever the test `cd`s.
///
/// Absolute, not a shim: rbenv's shim picks the version from the
/// `.ruby-version` nearest the *current directory*, so a test that runs
/// `ruby` with its cwd outside the checkout (`../ruby-bench/benchmarks/…`)
/// would otherwise get whichever Ruby `~/.rbenv/version` names. The
/// resolution runs from the workspace root, and the answer is the same
/// path from everywhere.
///
pub fn ruby_path() -> &'static str {
    &RUBY
}

///
/// With no usable host Ruby at all, the answer is still `ruby`, so the
/// failure a test then hits names the command it could not run rather
/// than a path nobody configured.
///
fn find_ruby() -> String {
    host_ruby::find(host_ruby::pinned_version(), Some(&workspace_root()))
        .map(|r| r.path)
        .unwrap_or_else(|| "ruby".to_string())
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
    let mut cmd =
        std::process::Command::new(std::env::var("CARGO").unwrap_or_else(|_| "cargo".to_string()));
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
    /// The harness and the interpreter's runtime probe must name the
    /// same Ruby, or a test installs a gem for one and the monoruby it
    /// spawns looks for it in the other. Both go through
    /// `host_ruby::find` with the pin as the target; this pins the
    /// harness side of that contract — an absolute executable, the same
    /// from any directory, the one `host_ruby` answers.
    ///
    #[test]
    fn the_resolved_ruby_is_what_host_ruby_finds() {
        let ruby = ruby_path();
        let Some(found) = host_ruby::find(host_ruby::pinned_version(), None) else {
            assert_eq!(ruby, "ruby", "a usable ruby was not resolved to its path");
            return;
        };
        assert_eq!(ruby, found.path);
        assert!(std::path::Path::new(ruby).is_absolute(), "{ruby}");
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
