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
//! The cache is re-probed automatically once the host's gem index moves
//! under it (`cache_is_stale`), so a `gem install` becomes visible on the
//! next start; `MONORUBY_REPROBE=1` forces a fresh probe regardless.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Minimum host Ruby version accepted by the runtime probe.
/// Mirrors `build.rs::MIN_RUBY_VERSION`: older Rubies ship gems
/// whose `required_ruby_version` checks fail against monoruby's
/// reported `RUBY_VERSION` (the vendored 4.0.x pin), and their
/// default-gem set drifts from the vendored stdlib snapshot.
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

/// Whether the cached `library_path` predates the host's installed-gem
/// index, i.e. a `gem install` (or uninstall) happened since the last
/// probe.
///
/// This matters because the cached `$LOAD_PATH` is the *only* place a
/// non-default gem's `lib/` is ever listed: monoruby boots rubygems
/// lazily (`builtins/gem_prelude.rb` autoloads `Gem`), so the CRuby
/// `Kernel#require` fallback that consults the specification index is
/// not installed at startup. A gem missing from the cache is therefore
/// invisible to `require` — permanently, since nothing else invalidates
/// the cache.
///
/// `gem_path` is the cached `Gem.paths.path` (colon-separated). Each
/// entry holds a `specifications/` directory into which rubygems writes
/// one `.gemspec` per installed gem, so that directory's mtime moves on
/// every install/uninstall. A root without one (never used by rubygems,
/// or gone with its Ruby) simply doesn't vote — which keeps a
/// host-Ruby-less machine from re-probing on every start.
pub fn cache_is_stale(library_path_file: &Path, gem_path: &str) -> bool {
    let Ok(cached_at) = std::fs::metadata(library_path_file).and_then(|m| m.modified()) else {
        // No readable cache file: only a probe can populate it.
        return true;
    };
    gem_path
        .split(':')
        .filter(|root| !root.is_empty())
        .any(|root| {
            std::fs::metadata(Path::new(root).join("specifications"))
                .and_then(|m| m.modified())
                .is_ok_and(|installed_at| installed_at > cached_at)
        })
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{Duration, SystemTime};

    /// A `library_path` cache file stamped with an explicit mtime, so the
    /// comparison under test doesn't ride on filesystem timestamp
    /// granularity or on the order the fixtures happened to be created.
    fn cache_file(dir: &Path, mtime: SystemTime) -> PathBuf {
        let path = dir.join("library_path");
        let f = std::fs::File::create(&path).unwrap();
        f.set_modified(mtime).unwrap();
        path
    }

    fn gem_root(dir: &Path, name: &str, with_specifications: bool) -> PathBuf {
        let root = dir.join(name);
        if with_specifications {
            std::fs::create_dir_all(root.join("specifications")).unwrap();
        } else {
            std::fs::create_dir_all(&root).unwrap();
        }
        root
    }

    fn hour_ago() -> SystemTime {
        SystemTime::now() - Duration::from_secs(3600)
    }

    fn hour_hence() -> SystemTime {
        SystemTime::now() + Duration::from_secs(3600)
    }

    #[test]
    fn missing_cache_file_is_stale() {
        let tmp = tempfile::tempdir().unwrap();
        assert!(cache_is_stale(&tmp.path().join("library_path"), ""));
    }

    #[test]
    fn cache_newer_than_gem_index_is_fresh() {
        let tmp = tempfile::tempdir().unwrap();
        let root = gem_root(tmp.path(), "gems", true);
        let cache = cache_file(tmp.path(), hour_hence());
        assert!(!cache_is_stale(&cache, root.to_str().unwrap()));
    }

    #[test]
    fn gem_index_newer_than_cache_is_stale() {
        let tmp = tempfile::tempdir().unwrap();
        let root = gem_root(tmp.path(), "gems", true);
        let cache = cache_file(tmp.path(), hour_ago());
        assert!(cache_is_stale(&cache, root.to_str().unwrap()));
    }

    #[test]
    fn one_moved_root_among_several_is_enough() {
        let tmp = tempfile::tempdir().unwrap();
        let quiet = gem_root(tmp.path(), "quiet", false);
        let moved = gem_root(tmp.path(), "moved", true);
        let cache = cache_file(tmp.path(), hour_ago());
        let gem_path = format!("{}:{}", quiet.display(), moved.display());
        assert!(cache_is_stale(&cache, &gem_path));
    }

    #[test]
    fn roots_without_a_specification_dir_do_not_vote() {
        // A root rubygems never wrote to, and one that vanished with its
        // Ruby: neither may force a probe, or a host with no usable ruby
        // would re-spawn the (failing) probe on every single start.
        let tmp = tempfile::tempdir().unwrap();
        let bare = gem_root(tmp.path(), "bare", false);
        let gone = tmp.path().join("gone");
        let cache = cache_file(tmp.path(), hour_ago());
        let gem_path = format!("{}:{}", bare.display(), gone.display());
        assert!(!cache_is_stale(&cache, &gem_path));
    }

    #[test]
    fn empty_gem_path_is_fresh() {
        // Nothing to compare against — an empty entry must not be read as
        // a relative "specifications" directory next to the cwd.
        let tmp = tempfile::tempdir().unwrap();
        let cache = cache_file(tmp.path(), hour_ago());
        assert!(!cache_is_stale(&cache, ""));
        assert!(!cache_is_stale(&cache, ":"));
    }
}
