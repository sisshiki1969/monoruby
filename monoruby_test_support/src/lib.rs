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
/// The reference CRuby the harness resolved (PATH, then rbenv/rvm shims).
/// A test that spawns `ruby` itself must use this rather than
/// `Command::new("ruby")`, which breaks in shells where only a version
/// manager provides Ruby.
///
pub fn ruby_path() -> &'static str {
    &RUBY
}

fn find_ruby() -> String {
    // Already on PATH and recent enough?
    if ruby_version_ok("ruby") {
        return "ruby".to_string();
    }
    // rbenv shim — defers to the version selected by ~/.rbenv/version.
    if let Some(home) = std::env::var_os("HOME") {
        let shim = PathBuf::from(home).join(".rbenv/shims/ruby");
        if let Some(shim_str) = shim.to_str()
            && ruby_version_ok(shim_str)
        {
            return shim_str.to_string();
        }
    }
    // rvm
    if let Some(home) = std::env::var_os("HOME") {
        let rvm = PathBuf::from(home).join(".rvm/bin/ruby");
        if let Some(rvm_str) = rvm.to_str()
            && ruby_version_ok(rvm_str)
        {
            return rvm_str.to_string();
        }
    }
    "ruby".to_string() // last resort — will fail with a clear error message
}

fn ruby_version_ok(ruby_cmd: &str) -> bool {
    let Ok(output) = std::process::Command::new(ruby_cmd)
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
