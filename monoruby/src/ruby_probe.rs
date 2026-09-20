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
//!
//! Which host Ruby to probe is not "whatever `ruby` resolves to": monoruby
//! reports, and ships the stdlib of, one particular CRuby
//! ([`COMPAT_RUBY_VERSION`]), and a host Ruby further from it hands back a
//! default-gem set that disagrees with the vendored snapshot. So the probe
//! ranks every Ruby the host advertises — `ruby` on `PATH` plus the
//! versioned installs of rbenv / rvm / asdf / mise — by distance from that
//! version and takes the closest, `PATH` winning ties. The choice is
//! remembered in `~/.monoruby/probed_ruby`, and once a closer Ruby appears
//! (`preferred_ruby_changed`) the cache is re-probed against it. Set
//! `MONORUBY_RUBY=<path>` to pin one explicitly and skip the ranking.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Minimum host Ruby version accepted by the runtime probe.
/// Mirrors `build.rs::MIN_RUBY_VERSION`: older Rubies ship gems
/// whose `required_ruby_version` checks fail against monoruby's
/// reported `RUBY_VERSION` (the vendored 4.0.x pin), and their
/// default-gem set drifts from the vendored stdlib snapshot.
const MIN_RUBY_VERSION: (u32, u32) = (4, 0);

/// The CRuby release this build of monoruby conforms to: the version it
/// reports as `RUBY_VERSION` and whose stdlib it ships.
///
/// `build.rs` bakes it from the vendored snapshot's pin marker
/// (`vendor/ruby-stdlib/.ruby-version`), so it moves with the stdlib
/// rather than with whatever `ruby` happened to be on `PATH` at build
/// time. [`FALLBACK_RUBY_VERSION`] stands in when that marker was
/// missing.
pub const COMPAT_RUBY_VERSION: &str = match option_env!("MONORUBY_RUBY_VERSION") {
    Some(v) => v,
    None => FALLBACK_RUBY_VERSION,
};

/// The compiled-in stand-in for [`COMPAT_RUBY_VERSION`], used only when
/// `build.rs` could not read the vendored pin marker. Keep it in step
/// with `vendor/ruby-stdlib/.ruby-version`.
pub const FALLBACK_RUBY_VERSION: &str = "4.0.6";

/// A CRuby release as `(major, minor, teeny)`.
pub type Version = (u32, u32, u32);

/// `X.Y` or `X.Y.Z`, all components numeric — anything else (a
/// `truffleruby-24.1.1` or `jruby-9.4.5.0` directory name, a `ruby`
/// that printed something unexpected) is rejected rather than guessed
/// at.
fn parse_version(s: &str) -> Option<Version> {
    let mut parts = s.trim().split('.');
    let major = parts.next()?.parse().ok()?;
    let minor = parts.next()?.parse().ok()?;
    let teeny = match parts.next() {
        Some(t) => t.parse().ok()?,
        None => 0,
    };
    parts.next().is_none().then_some((major, minor, teeny))
}

/// [`COMPAT_RUBY_VERSION`] as a triple. A build whose baked value is
/// unparseable still gets the floor, so ranking degrades to "prefer the
/// oldest Ruby we accept" rather than panicking.
pub fn compat_version() -> Version {
    parse_version(COMPAT_RUBY_VERSION).unwrap_or((MIN_RUBY_VERSION.0, MIN_RUBY_VERSION.1, 0))
}

/// How far `v` sits from `target`, as a sort key — smaller is closer.
///
/// The components are compared most-significant first, so a Ruby of the
/// same `major.minor` always beats one a minor away however close its
/// teeny is. The trailing flag breaks an exact tie (4.0.5 and 4.0.7
/// against a 4.0.6 target) in favour of the newer release, which is the
/// one more likely to carry the default gems the vendored snapshot was
/// taken from.
fn distance(v: Version, target: Version) -> (u32, u32, u32, bool) {
    (
        v.0.abs_diff(target.0),
        v.1.abs_diff(target.1),
        v.2.abs_diff(target.2),
        v < target,
    )
}

fn ruby_version(ruby_cmd: &str) -> Option<Version> {
    let output = Command::new(ruby_cmd)
        .args(["-e", "print RUBY_VERSION"])
        .output()
        .ok()?;
    output
        .status
        .success()
        .then(|| parse_version(&String::from_utf8_lossy(&output.stdout)))
        .flatten()
}

fn usable(v: Version) -> bool {
    (v.0, v.1) >= MIN_RUBY_VERSION
}

fn ruby_version_ok(ruby_cmd: &str) -> bool {
    ruby_version(ruby_cmd).is_some_and(usable)
}

/// The directories under which a version manager keeps one subdirectory
/// per installed Ruby, each holding `bin/ruby`.
fn version_manager_roots() -> Vec<PathBuf> {
    let home = std::env::var_os("HOME").map(PathBuf::from);
    let mut roots = vec![];
    let rbenv = std::env::var_os("RBENV_ROOT")
        .map(PathBuf::from)
        .or_else(|| home.as_ref().map(|h| h.join(".rbenv")));
    if let Some(r) = rbenv {
        roots.push(r.join("versions"));
    }
    let asdf = std::env::var_os("ASDF_DATA_DIR")
        .map(PathBuf::from)
        .or_else(|| home.as_ref().map(|h| h.join(".asdf")));
    if let Some(r) = asdf {
        roots.push(r.join("installs").join("ruby"));
    }
    if let Some(h) = &home {
        roots.push(h.join(".rvm").join("rubies"));
        roots.push(h.join(".local/share/mise/installs/ruby"));
    }
    roots
}

/// Every Ruby the host's version managers advertise, as
/// `(executable, version)`.
///
/// The version is read off the directory name (`4.0.6`, or rvm's
/// `ruby-4.0.6`) rather than by running the interpreter, so this costs a
/// few `read_dir` calls and no spawn at all. That is what lets
/// [`preferred_ruby_changed`] consult it on *every* start, cache hit
/// included — the whole point of the cache is not to spawn a Ruby.
/// A name that is not a plain version is skipped, which is also how
/// JRuby and TruffleRuby installs drop out.
fn versioned_rubies() -> Vec<(PathBuf, Version)> {
    collect_versioned(&version_manager_roots())
}

fn collect_versioned(roots: &[PathBuf]) -> Vec<(PathBuf, Version)> {
    let mut found = vec![];
    for dir in roots {
        let Ok(entries) = std::fs::read_dir(dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let name = entry.file_name();
            let Some(name) = name.to_str() else { continue };
            let Some(version) = parse_version(name.strip_prefix("ruby-").unwrap_or(name)) else {
                continue;
            };
            let exe = entry.path().join("bin").join("ruby");
            if usable(version) && exe.is_file() {
                found.push((exe, version));
            }
        }
    }
    found
}

/// Of [`versioned_rubies`], the one closest to [`compat_version`].
/// `None` when the host uses no version manager, or none of its Rubies
/// clears [`MIN_RUBY_VERSION`]. Ties fall to the lexicographically first
/// path so the answer does not ride on `read_dir` order.
fn best_versioned_ruby() -> Option<(PathBuf, Version)> {
    closest_to(versioned_rubies(), compat_version())
}

fn closest_to(candidates: Vec<(PathBuf, Version)>, target: Version) -> Option<(PathBuf, Version)> {
    candidates.into_iter().min_by(|a, b| {
        distance(a.1, target)
            .cmp(&distance(b.1, target))
            .then_with(|| a.0.cmp(&b.0))
    })
}

/// Locate the host `ruby` whose version sits closest to
/// [`COMPAT_RUBY_VERSION`], among `ruby` on `PATH` and every versioned
/// install a version manager advertises. An exact `MONORUBY_RUBY`
/// overrides the search; `PATH` wins a tie, so a host whose selected
/// Ruby already matches the pin keeps using it (and its gems) rather
/// than being moved onto an equally good tree behind its back.
///
/// Only the winner is verified by actually running it — the candidates'
/// versions come from `PATH` (one spawn) and from directory names (no
/// spawn) — and a candidate that fails to run hands over to the next.
fn find_ruby() -> Option<String> {
    if let Some(pinned) = std::env::var_os("MONORUBY_RUBY")
        && let Some(pinned) = pinned.to_str()
        && ruby_version_ok(pinned)
    {
        return Some(pinned.to_string());
    }

    let target = compat_version();
    // (command, version, is `ruby` on PATH)
    let mut candidates: Vec<(String, Version, bool)> = vec![];
    if let Some(v) = ruby_version("ruby").filter(|v| usable(*v)) {
        candidates.push(("ruby".to_string(), v, true));
    }
    for (exe, v) in versioned_rubies() {
        if let Some(exe) = exe.to_str() {
            candidates.push((exe.to_string(), v, false));
        }
    }
    candidates.sort_by(|a, b| {
        distance(a.1, target)
            .cmp(&distance(b.1, target))
            // `PATH` first among equals, then by name for determinism.
            .then_with(|| b.2.cmp(&a.2))
            .then_with(|| a.0.cmp(&b.0))
    });
    for (cmd, _, from_path) in &candidates {
        // The `PATH` entry was already run to get its version.
        if *from_path || ruby_version_ok(cmd) {
            return Some(cmd.clone());
        }
    }

    // Nothing usable turned up. Fall back on the well-known shims, which
    // a `PATH` without the version manager on it would otherwise hide.
    let home = std::env::var_os("HOME")?;
    let shims = [
        PathBuf::from(&home).join(".rbenv/shims/ruby"),
        PathBuf::from(&home).join(".rvm/bin/ruby"),
    ];
    for c in &shims {
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
///
/// Separately, when *every* cached root has vanished, the Ruby
/// installation the cache was probed from is gone entirely (e.g. an
/// rbenv version upgrade deleted the old tree). The cached `$LOAD_PATH`
/// then points only at dead directories, and — since the new Ruby's gem
/// roots are not in the cache — no `gem install` there can ever move a
/// `specifications/` mtime we compare against, so the mtime rule alone
/// would keep serving the dead cache forever. Treat that as stale: a
/// fresh probe against the current Ruby rewrites the cache, and on a
/// host that lost its Ruby altogether the retried probe fails in a few
/// milliseconds without touching the cache.
pub fn cache_is_stale(library_path_file: &Path, gem_path: &str) -> bool {
    let Ok(cached_at) = std::fs::metadata(library_path_file).and_then(|m| m.modified()) else {
        // No readable cache file: only a probe can populate it.
        return true;
    };
    let roots: Vec<&str> = gem_path.split(':').filter(|root| !root.is_empty()).collect();
    if !roots.is_empty() && roots.iter().all(|root| !Path::new(root).exists()) {
        return true;
    }
    roots.iter().any(|root| {
        std::fs::metadata(Path::new(root).join("specifications"))
            .and_then(|m| m.modified())
            .is_ok_and(|installed_at| installed_at > cached_at)
    })
}

/// A `~/.monoruby/probed_ruby` record, as far as the staleness check
/// cares about it.
///
/// The file has three lines: the command that was run, the
/// `RUBY_VERSION` it reported, and the closest versioned install
/// [`best_versioned_ruby`] saw at that moment. The first two say which
/// Ruby the cached `$LOAD_PATH` and gem roots belong to — a breadcrumb
/// for whoever is wondering, and the parse that tells a record of this
/// shape from an older file — while the third is the half
/// [`preferred_ruby_changed`] compares.
pub struct ProbedRuby {
    /// `None` when the host advertised no versioned install at all,
    /// which is different from there being no record.
    pub best_versioned: Option<String>,
}

/// Read `~/.monoruby/probed_ruby` (`-` on the third line for "no
/// versioned install"). A file from an older monoruby, or one
/// half-written, simply reads as absent — which costs one probe to
/// rewrite.
fn read_probed_ruby(file: &Path) -> Option<ProbedRuby> {
    let text = std::fs::read_to_string(file).ok()?;
    let mut lines = text.lines();
    let cmd = lines.next()?.trim();
    parse_version(lines.next()?)?;
    let best = lines.next()?.trim();
    (!cmd.is_empty()).then(|| ProbedRuby {
        best_versioned: (best != "-" && !best.is_empty()).then(|| best.to_string()),
    })
}

/// The record [`read_probed_ruby`] reads back.
fn probed_ruby_record(cmd: &str, version: Version, best_versioned: Option<&Path>) -> String {
    let best = best_versioned
        .and_then(|p| p.to_str())
        .filter(|s| !s.is_empty())
        .unwrap_or("-");
    format!("{cmd}\n{}.{}.{}\n{best}\n", version.0, version.1, version.2)
}

/// Whether the set of Rubies to choose from has moved since the cache was
/// probed — an `rbenv install` of the pinned version, or the removal of
/// the tree the cache belongs to. The cached `$LOAD_PATH` would then be
/// the wrong Ruby's: its default-gem set disagrees with the vendored
/// stdlib, and gems installed for the closer one are invisible to
/// `require`, permanently, since nothing else invalidates the cache.
///
/// The comparison is deliberately an *identity* one against the closest
/// versioned install the last probe saw, not a fresh ranking. Ranking is
/// [`find_ruby`]'s job and it has already run; all that can change its
/// answer afterwards is the set of installs. That makes this exact, free
/// of any interpreter spawn (see [`versioned_rubies`] — directory names
/// only, so it can run on every start, cache hit included), and unable to
/// loop: whatever a probe now sees is what it writes down.
///
/// A missing record is either a cache written by a monoruby predating
/// this file or a host that has never probed; re-probing once to
/// establish it is worth the single spawn. A host with no version manager
/// at all has nothing to compare and is left alone, and so is one that
/// pinned its Ruby with `MONORUBY_RUBY`.
pub fn preferred_ruby_changed(probed_ruby_file: &Path) -> bool {
    if std::env::var_os("MONORUBY_RUBY").is_some() {
        // The choice is the user's, so a closer install is not a reason
        // to revisit it — and saying otherwise would re-probe on every
        // start, since the next probe would pick the pin right back.
        return false;
    }
    preferred_changed(best_versioned_ruby(), read_probed_ruby(probed_ruby_file))
}

fn preferred_changed(best: Option<(PathBuf, Version)>, probed: Option<ProbedRuby>) -> bool {
    let best = best.as_ref().and_then(|(p, _)| p.to_str());
    match probed {
        // Never probed, or a record this build cannot read: establish one,
        // unless there is nothing to watch in the first place.
        None => best.is_some(),
        Some(probed) => best != probed.best_versioned.as_deref(),
    }
}

/// Probe result: `(library_path, gem_path)`.
///
/// - `library_path` — newline-separated list of `$LOAD_PATH` entries
///   (stdlib + every installed gem's `lib/`).
/// - `gem_path`     — colon-separated list of `Gem.paths.path` entries
///   (the directories rubygems scans for `specifications/*.gemspec`).
/// - `probed_ruby`  — the `probed_ruby` record for the Ruby all of this
///   came from, so a later start can tell that a closer one appeared.
pub struct ProbedPaths {
    pub library_path: String,
    pub gem_path: String,
    pub probed_ruby: String,
}

/// Invoke a host `ruby` and read back its `$LOAD_PATH` and
/// `Gem.paths.path`. Returns `None` if no suitable ruby is found or
/// the invocation fails — startup then proceeds with whatever was
/// already configured (env vars / cached files / nothing).
pub fn probe() -> Option<ProbedPaths> {
    let ruby = find_ruby()?;
    // What the winner actually reports, not what its directory name
    // promised — the record is only useful if it names the version whose
    // `$LOAD_PATH` is about to be cached.
    let version = ruby_version(&ruby)?;
    // Read *after* the choice, so the record says what this probe
    // settled against however the choice was reached (a candidate that
    // would not run, say). That is what makes the check converge.
    let best_versioned = best_versioned_ruby();

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
        probed_ruby: probed_ruby_record(
            &ruby,
            version,
            best_versioned.as_ref().map(|(p, _)| p.as_path()),
        ),
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

    /// A version-manager root holding one `bin/ruby` per named version.
    fn manager_root(dir: &Path, names: &[&str]) -> PathBuf {
        let root = dir.join("versions");
        for name in names {
            let bin = root.join(name).join("bin");
            std::fs::create_dir_all(&bin).unwrap();
            std::fs::write(bin.join("ruby"), "").unwrap();
        }
        root
    }

    fn probed(best_versioned: Option<&str>) -> Option<ProbedRuby> {
        Some(ProbedRuby {
            best_versioned: best_versioned.map(|s| s.to_string()),
        })
    }

    fn best(path: &str, version: Version) -> Option<(PathBuf, Version)> {
        Some((PathBuf::from(path), version))
    }

    #[test]
    fn versions_parse_only_when_fully_numeric() {
        assert_eq!(parse_version("4.0.6"), Some((4, 0, 6)));
        assert_eq!(parse_version("4.0"), Some((4, 0, 0)));
        assert_eq!(parse_version(" 4.0.6\n"), Some((4, 0, 6)));
        // Everything a version manager's directory listing can also hold.
        assert_eq!(parse_version("truffleruby-24.1.1"), None);
        assert_eq!(parse_version("jruby-9.4.5.0"), None);
        assert_eq!(parse_version("system"), None);
        assert_eq!(parse_version("4"), None);
        assert_eq!(parse_version("4.0.6.1"), None);
        assert_eq!(parse_version("4.0.x"), None);
    }

    #[test]
    fn the_compat_version_is_the_one_we_report() {
        // The ranking target has to be the version monoruby claims to be,
        // or it would send the probe at a Ruby whose default gems
        // disagree with the vendored stdlib.
        assert_eq!(parse_version(COMPAT_RUBY_VERSION), Some(compat_version()));
        assert!(usable(compat_version()));
    }

    #[test]
    fn closest_beats_newest() {
        let target = (4, 0, 6);
        let cands = |names: &[&str]| -> Vec<(PathBuf, Version)> {
            names
                .iter()
                .map(|n| (PathBuf::from(*n), parse_version(n).unwrap()))
                .collect()
        };
        // An exact match wins over anything else.
        let best = closest_to(cands(&["4.0.5", "4.0.6", "4.1.0"]), target).unwrap();
        assert_eq!(best.1, (4, 0, 6));
        // Same major.minor beats a nearer teeny one minor away.
        let best = closest_to(cands(&["4.0.0", "4.1.6"]), target).unwrap();
        assert_eq!(best.1, (4, 0, 0));
        // Equidistant: the newer release.
        let best = closest_to(cands(&["4.0.5", "4.0.7"]), target).unwrap();
        assert_eq!(best.1, (4, 0, 7));
        assert!(closest_to(vec![], target).is_none());
    }

    #[test]
    fn only_runnable_versioned_installs_are_offered() {
        let tmp = tempfile::tempdir().unwrap();
        let root = manager_root(
            tmp.path(),
            &["4.0.5", "4.0.6", "truffleruby-24.1.1", "3.4.1"],
        );
        // A directory with no `bin/ruby` in it is not an install.
        std::fs::create_dir_all(root.join("4.2.0")).unwrap();
        let mut found: Vec<Version> = collect_versioned(&[root])
            .into_iter()
            .map(|(_, v)| v)
            .collect();
        found.sort();
        // 3.4.1 is below MIN_RUBY_VERSION, truffleruby is not a version,
        // and 4.2.0 has no interpreter.
        assert_eq!(found, vec![(4, 0, 5), (4, 0, 6)]);
    }

    #[test]
    fn rvm_style_names_are_understood() {
        let tmp = tempfile::tempdir().unwrap();
        let root = manager_root(tmp.path(), &["ruby-4.0.6"]);
        let found = collect_versioned(&[root]);
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].1, (4, 0, 6));
    }

    #[test]
    fn a_new_install_forces_a_reprobe() {
        // The cache was probed when rbenv held only 4.0.5; the pinned
        // version has been installed since, so `find_ruby` would now
        // answer differently and the cached $LOAD_PATH is the wrong
        // Ruby's.
        assert!(preferred_changed(
            best("/rb/4.0.6/bin/ruby", (4, 0, 6)),
            probed(Some("/rb/4.0.5/bin/ruby")),
        ));
        // Nothing moved: the probe already ranked these and settled.
        assert!(!preferred_changed(
            best("/rb/4.0.6/bin/ruby", (4, 0, 6)),
            probed(Some("/rb/4.0.6/bin/ruby")),
        ));
        // Even when the probe ended up on something *else* — a candidate
        // that would not run, a `PATH` ruby that tied — the record is
        // what that probe saw, so the answer converges after one probe
        // instead of re-probing on every start.
        assert!(!preferred_changed(
            best("/rb/4.0.6/bin/ruby", (4, 0, 6)),
            probed(Some("/rb/4.0.6/bin/ruby")),
        ));
        // The version manager's trees are gone (an rbenv uninstall): the
        // cache may well belong to a Ruby that no longer exists.
        assert!(preferred_changed(
            None,
            probed(Some("/rb/4.0.6/bin/ruby"))
        ));
    }

    #[test]
    fn a_missing_record_probes_once_but_only_with_something_to_compare() {
        // No record yet (a cache from a monoruby that predates the file):
        // probe once to establish it. Terminating, because the probe
        // writes the record.
        assert!(preferred_changed(
            best("/rb/4.0.6/bin/ruby", (4, 0, 6)),
            None
        ));
        // No version manager on this host: nothing to rank, so never
        // re-probe on this account — otherwise a machine without a host
        // Ruby would spawn on every single start.
        assert!(!preferred_changed(None, None));
        // And a host that has none stays settled once recorded.
        assert!(!preferred_changed(None, probed(None)));
    }

    #[test]
    fn the_probed_ruby_record_round_trips() {
        let tmp = tempfile::tempdir().unwrap();
        let file = tmp.path().join("probed_ruby");
        let best = PathBuf::from("/rb/4.0.6/bin/ruby");
        std::fs::write(
            &file,
            probed_ruby_record("ruby", (4, 0, 6), Some(best.as_path())),
        )
        .unwrap();
        let back = read_probed_ruby(&file).unwrap();
        assert_eq!(back.best_versioned.as_deref(), Some("/rb/4.0.6/bin/ruby"));
        // A host with no version manager records that it had nothing to
        // watch, which is different from having no record at all.
        std::fs::write(&file, probed_ruby_record("ruby", (4, 0, 6), None)).unwrap();
        assert!(read_probed_ruby(&file).unwrap().best_versioned.is_none());
        // A truncated, older-format or empty file reads as absent rather
        // than as a record claiming some default.
        for text in ["ruby\n4.0.6\n", "ruby\n", ""] {
            std::fs::write(&file, text).unwrap();
            assert!(
                read_probed_ruby(&file).is_none(),
                "{text:?} read as a record"
            );
        }
        assert!(read_probed_ruby(&tmp.path().join("nope")).is_none());
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
        // Ruby: as long as at least one cached root still exists, neither
        // may force a probe, or a host whose ruby never installs a gem
        // would re-spawn the probe on every single start.
        let tmp = tempfile::tempdir().unwrap();
        let bare = gem_root(tmp.path(), "bare", false);
        let gone = tmp.path().join("gone");
        let cache = cache_file(tmp.path(), hour_ago());
        let gem_path = format!("{}:{}", bare.display(), gone.display());
        assert!(!cache_is_stale(&cache, &gem_path));
    }

    #[test]
    fn all_roots_vanished_is_stale() {
        // Every cached root gone means the probed Ruby installation was
        // removed wholesale (e.g. an rbenv upgrade deleted the old
        // version tree): the cached $LOAD_PATH is dead and the mtime rule
        // can never fire again, so only a fresh probe can repair it.
        let tmp = tempfile::tempdir().unwrap();
        let cache = cache_file(tmp.path(), hour_hence());
        let gem_path = format!(
            "{}:{}",
            tmp.path().join("gone-a").display(),
            tmp.path().join("gone-b").display()
        );
        assert!(cache_is_stale(&cache, &gem_path));
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
