//! Runtime probe for a host `ruby` binary, used as a fallback when
//! `build.rs` could not bake `~/.monoruby/library_path` and
//! `~/.monoruby/gem_path` (e.g. distributed binaries, containers built
//! without `ruby` in `PATH`).
//!
//! Order of precedence at startup:
//!   1. `MONORUBY_GEM_PATH` / `MONORUBY_LOAD_PATH` env vars (explicit override)
//!   2. `GEM_PATH` env var (CRuby convention)
//!   3. `~/.monoruby/{gem_path,library_path}` files (build.rs baked)
//!   4. Runtime probe — invoke `ruby` once, write the result back into
//!      the same cache files so subsequent runs skip probing
//!
//! Probe results are cached in `~/.monoruby/{gem_path,library_path}` so
//! the cost (~50ms for an interpreter spawn) is paid once per machine.
//! Set `MONORUBY_REPROBE=1` to force a fresh probe.

use std::path::PathBuf;
use std::process::Command;

/// Minimum host Ruby version accepted by the runtime probe.
/// Mirrors `build.rs::MIN_RUBY_VERSION`: older Rubies ship gems
/// whose `required_ruby_version` checks fail against monoruby's
/// reported `RUBY_VERSION = "4.0.0"`, and their default-gem set
/// drifts from the vendored stdlib snapshot.
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

/// Locate a `ruby` executable meeting `MIN_RUBY_VERSION`. Checks `PATH`
/// first, then the well-known rbenv / rvm shim locations under `$HOME`.
fn find_ruby() -> Option<String> {
    if ruby_version_ok("ruby") {
        return Some("ruby".to_string());
    }
    let home = std::env::var_os("HOME")?;
    let candidates = [
        PathBuf::from(&home).join(".rbenv/shims/ruby"),
        PathBuf::from(&home).join(".rvm/bin/ruby"),
    ];
    for c in &candidates {
        if let Some(s) = c.to_str()
            && ruby_version_ok(s)
        {
            return Some(s.to_string());
        }
    }
    None
}

/// Honour `MONORUBY_REPROBE=1` to bypass cached `~/.monoruby/gem_path`
/// and `~/.monoruby/library_path` and re-invoke `ruby`. Any other value
/// (including unset) keeps the cache.
pub fn reprobe_requested() -> bool {
    matches!(std::env::var("MONORUBY_REPROBE").as_deref(), Ok("1"))
}

/// Probe result: `(library_path, gem_path)`.
///
/// - `library_path` — newline-separated list of `$LOAD_PATH` entries
///   (stdlib + every installed gem's `lib/`).
/// - `gem_path`     — colon-separated list of `Gem.paths.path` entries
///   (the directories rubygems scans for `specifications/*.gemspec`).
pub struct ProbedPaths {
    pub library_path: String,
    pub gem_path: String,
}

/// Invoke a host `ruby` and read back its `$LOAD_PATH` and
/// `Gem.paths.path`. Returns `None` if no suitable ruby is found or
/// the invocation fails — startup then proceeds with whatever was
/// already configured (env vars / cached files / nothing).
pub fn probe() -> Option<ProbedPaths> {
    let ruby = find_ruby()?;

    // $LOAD_PATH + every installed gem's require_paths (mirrors
    // build.rs so cached files have identical shape regardless of
    // whether they were populated at build or runtime).
    let load_path_out = Command::new(&ruby).args(["-e", "puts($:)"]).output().ok()?;
    if !load_path_out.status.success() {
        return None;
    }
    let mut library_path = String::from_utf8(load_path_out.stdout).ok()?;
    if let Ok(g) = Command::new(&ruby)
        .args([
            "-e",
            "Gem::Specification.latest_specs(true).each{|s| \
             s.require_paths.each{|p| d=File.join(s.full_gem_path,p); \
             puts d if Dir.exist?(d)}}",
        ])
        .output()
        && g.status.success()
    {
        if !library_path.ends_with('\n') {
            library_path.push('\n');
        }
        library_path.push_str(&String::from_utf8_lossy(&g.stdout));
    }

    let gem_path_out = Command::new(&ruby)
        .args(["-e", "print Gem.paths.path.join(':')"])
        .output()
        .ok()?;
    if !gem_path_out.status.success() {
        return None;
    }
    let gem_path = String::from_utf8(gem_path_out.stdout).ok()?.trim().to_string();

    Some(ProbedPaths {
        library_path,
        gem_path,
    })
}
