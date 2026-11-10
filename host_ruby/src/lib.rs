//! Which host CRuby monoruby pairs with.
//!
//! monoruby reports, and ships the stdlib of, one particular CRuby
//! release — the pin in `monoruby/vendor/ruby-stdlib/.ruby-version`,
//! [`PINNED_RUBY_VERSION`] here. Everything that needs a host `ruby`
//! wants *that* one: the runtime probe borrows its installed gems
//! (`monoruby/src/ruby_probe.rs`), the test harness compares against it
//! (`monoruby_test_support::ruby_path`), `build.rs` asks it for
//! `RUBY_PLATFORM`, and `bin/test` installs the gems the suite needs
//! into it. For years each of those had its own idea of "the host
//! ruby" — `ruby` on `PATH`, an rbenv shim, a version-distance ranking —
//! and whenever `~/.rbenv/version` named a Ruby other than the pin they
//! drifted apart: `bin/test` installed hexapdf into the Ruby `ruby`
//! resolved to, the probe borrowed gems from the pinned one, and the
//! gem tests failed with `LoadError` while the gem check saw nothing
//! missing.
//!
//! So the choice is made in exactly one place, [`find`]: rank `ruby` on
//! `PATH` together with every versioned install a version manager
//! advertises (rbenv / rvm / asdf / mise) by distance from the target
//! version, take the closest, and answer the **absolute path of the
//! executable** (`RbConfig.ruby`), never a shim — a shim picks its
//! version from the `.ruby-version` nearest the current directory, so
//! the same command gives different Rubies from different places.
//! `MONORUBY_RUBY=<path>` names one explicitly and skips the ranking,
//! for every caller alike.
//!
//! Dependency-free on purpose: `monoruby_test_support` links it into
//! tests that must not pull in the interpreter.

use std::path::{Path, PathBuf};
use std::process::Command;

/// The oldest CRuby anything here accepts. Older Rubies ship gems whose
/// `required_ruby_version` checks fail against monoruby's reported
/// `RUBY_VERSION`, their default-gem set drifts from the vendored
/// stdlib snapshot, and the test suite's answers are written against
/// 4.0.
pub const MIN_RUBY_VERSION: (u32, u32) = (4, 0);

/// The vendored stdlib's pin marker, as the file reads (a trailing
/// newline included): the CRuby release monoruby conforms to. Compiled
/// in from the repository, so it can never lag the stdlib. See
/// [`pinned_version`] for it parsed.
pub const PINNED_RUBY_VERSION: &str =
    include_str!("../../monoruby/vendor/ruby-stdlib/.ruby-version");

/// A CRuby release as `(major, minor, teeny)`.
pub type Version = (u32, u32, u32);

/// A host Ruby [`find`] settled on.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HostRuby {
    /// The executable, absolute (`RbConfig.ruby`), or the command as
    /// given when it could not be resolved to a file.
    pub path: String,
    /// What it reports as `RUBY_VERSION`.
    pub version: Version,
}

/// `X.Y` or `X.Y.Z`, all components numeric — anything else (a
/// `truffleruby-24.1.1` or `jruby-9.4.5.0` directory name, a `ruby`
/// that printed something unexpected) is rejected rather than guessed
/// at.
pub fn parse_version(s: &str) -> Option<Version> {
    let mut parts = s.trim().split('.');
    let major = parts.next()?.parse().ok()?;
    let minor = parts.next()?.parse().ok()?;
    let teeny = match parts.next() {
        Some(t) => t.parse().ok()?,
        None => 0,
    };
    parts.next().is_none().then_some((major, minor, teeny))
}

/// [`PINNED_RUBY_VERSION`] as a triple. The marker is written by
/// `bin/vendor-ruby-stdlib` and always parses; should it not, the
/// ranking degrades to "prefer the oldest Ruby we accept".
pub fn pinned_version() -> Version {
    parse_version(PINNED_RUBY_VERSION).unwrap_or((MIN_RUBY_VERSION.0, MIN_RUBY_VERSION.1, 0))
}

/// Whether a Ruby of version `v` clears [`MIN_RUBY_VERSION`].
pub fn usable(v: Version) -> bool {
    (v.0, v.1) >= MIN_RUBY_VERSION
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

/// Run `ruby_cmd` once and read back what it is: its `RUBY_VERSION` and
/// the executable behind it (`RbConfig.ruby`, which sees through a
/// version manager's shim). `None` when it cannot be spawned, fails,
/// prints something that is not a version, or is older than
/// [`MIN_RUBY_VERSION`].
///
/// Run from `cwd` when given — the test harness asks from the workspace
/// root so a shim answers for the checkout — and from the caller's
/// directory otherwise. Rubygems and `RUBYOPT` are left out of the
/// child: neither affects the answer and both slow the spawn.
pub fn identify(ruby_cmd: &str, cwd: Option<&Path>) -> Option<HostRuby> {
    let mut cmd = Command::new(ruby_cmd);
    if let Some(dir) = cwd {
        cmd.current_dir(dir);
    }
    let output = cmd
        // `rbconfig` is what rubygems would have loaded; without it the
        // constant is undefined.
        .args([
            "--disable=gems,rubyopt",
            "-e",
            "require 'rbconfig'; print RUBY_VERSION, \"\\n\", RbConfig.ruby",
        ])
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let text = String::from_utf8_lossy(&output.stdout);
    let mut lines = text.lines();
    let version = parse_version(lines.next()?).filter(|v| usable(*v))?;
    let exe = lines.next().map(str::trim).unwrap_or("");
    let path = if !exe.is_empty() && Path::new(exe).is_file() {
        exe.to_string()
    } else {
        ruby_cmd.to_string()
    };
    Some(HostRuby { path, version })
}

/// The directories under which a version manager keeps one subdirectory
/// per installed Ruby, each holding `bin/ruby`.
pub fn version_manager_roots() -> Vec<PathBuf> {
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
/// few `read_dir` calls and no spawn at all. That is what lets the
/// runtime probe consult it on *every* start, cache hit included — the
/// whole point of its cache is not to spawn a Ruby. A name that is not a
/// plain version is skipped, which is also how JRuby and TruffleRuby
/// installs drop out.
pub fn versioned_rubies() -> Vec<(PathBuf, Version)> {
    collect_versioned(&version_manager_roots())
}

/// [`versioned_rubies`] over the given roots.
pub fn collect_versioned(roots: &[PathBuf]) -> Vec<(PathBuf, Version)> {
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

/// Of `candidates`, the one closest to `target`. Ties fall to the
/// lexicographically first path so the answer does not ride on
/// `read_dir` order.
pub fn closest_to(
    candidates: Vec<(PathBuf, Version)>,
    target: Version,
) -> Option<(PathBuf, Version)> {
    candidates.into_iter().min_by(|a, b| {
        distance(a.1, target)
            .cmp(&distance(b.1, target))
            .then_with(|| a.0.cmp(&b.0))
    })
}

/// The well-known shims a `PATH` without the version manager on it
/// still has.
fn shims() -> Vec<PathBuf> {
    let Some(home) = std::env::var_os("HOME") else {
        return vec![];
    };
    let home = PathBuf::from(home);
    vec![home.join(".rbenv/shims/ruby"), home.join(".rvm/bin/ruby")]
}

/// Locate the host `ruby` whose version sits closest to `target`, among
/// `ruby` on `PATH` and every versioned install a version manager
/// advertises, as the absolute path of its executable.
///
/// `MONORUBY_RUBY=<path>` names one explicitly and skips the search.
/// `PATH` wins a tie, so a host whose selected Ruby already matches the
/// pin keeps using it (and its gems) rather than being moved onto an
/// equally good tree behind its back. Only the winner is verified by
/// actually running it — the `PATH` candidate costs one spawn, the
/// versioned installs none, their versions coming from directory names —
/// and a candidate that fails to run hands over to the next. With none
/// of that usable, the shims are tried last; `None` means no host Ruby
/// at all.
///
/// `cwd` is where the `PATH` `ruby` (and `MONORUBY_RUBY`) is run from,
/// see [`identify`].
pub fn find(target: Version, cwd: Option<&Path>) -> Option<HostRuby> {
    let pinned = std::env::var_os("MONORUBY_RUBY");
    find_among(
        target,
        cwd,
        pinned.as_deref().and_then(|p| p.to_str()),
        identify("ruby", cwd),
        versioned_rubies(),
        &shims(),
    )
}

/// [`find`] over an explicit set of candidates: `pinned` is
/// `MONORUBY_RUBY`, `on_path` what `ruby` on `PATH` identified as (if
/// anything), `versioned` the version managers' installs and `shims`
/// the entry points to fall back on.
pub fn find_among(
    target: Version,
    cwd: Option<&Path>,
    pinned: Option<&str>,
    on_path: Option<HostRuby>,
    versioned: Vec<(PathBuf, Version)>,
    shims: &[PathBuf],
) -> Option<HostRuby> {
    if let Some(pinned) = pinned
        && let Some(ruby) = identify(pinned, cwd)
    {
        return Some(ruby);
    }

    // (candidate, version, is `ruby` on PATH)
    let mut candidates: Vec<(Candidate, Version, bool)> = vec![];
    if let Some(ruby) = &on_path {
        candidates.push((Candidate::Identified(ruby.clone()), ruby.version, true));
    }
    for (exe, v) in versioned {
        // The install `ruby` on PATH resolves to is already in.
        if on_path.as_ref().is_some_and(|r| Path::new(&r.path) == exe) {
            continue;
        }
        if let Some(exe) = exe.to_str() {
            candidates.push((Candidate::Unverified(exe.to_string()), v, false));
        }
    }
    candidates.sort_by(|a, b| {
        distance(a.1, target)
            .cmp(&distance(b.1, target))
            // `PATH` first among equals, then by name for determinism.
            .then_with(|| b.2.cmp(&a.2))
            .then_with(|| a.0.name().cmp(b.0.name()))
    });
    for (candidate, _, _) in candidates {
        match candidate {
            Candidate::Identified(ruby) => return Some(ruby),
            Candidate::Unverified(exe) => {
                if let Some(ruby) = identify(&exe, cwd) {
                    return Some(ruby);
                }
            }
        }
    }

    shims
        .iter()
        .filter_map(|shim| shim.to_str())
        .find_map(|shim| identify(shim, cwd))
}

enum Candidate {
    /// Already run: `ruby` on `PATH`.
    Identified(HostRuby),
    /// A versioned install, known by its directory name only.
    Unverified(String),
}

impl Candidate {
    fn name(&self) -> &str {
        match self {
            Candidate::Identified(r) => &r.path,
            Candidate::Unverified(s) => s,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn the_pin_parses_and_is_usable() {
        assert_eq!(parse_version(PINNED_RUBY_VERSION), Some(pinned_version()));
        assert!(usable(pinned_version()));
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

    /// The three ways `identify` walks past a candidate: it cannot be
    /// spawned, it runs and fails (an rbenv shim with no version
    /// selected exits 1), or it succeeds but answers something that is
    /// not a version.
    #[test]
    fn a_candidate_that_is_not_a_ruby_is_rejected() {
        assert!(identify("monoruby-no-such-command-exists", None).is_none());
        assert!(identify("false", None).is_none());
        // Runs, exits 0, prints its arguments rather than a version.
        assert!(identify("echo", None).is_none());
    }

    /// A stand-in `ruby`: a script that answers `identify`'s question
    /// with `version` and, as `RbConfig.ruby`, `reports` (its own path
    /// by default — a shim names the executable behind it instead). An
    /// empty `version` makes it fail, like a shim with nothing selected.
    fn fake_ruby(at: &Path, version: &str, reports: Option<&Path>) -> PathBuf {
        use std::os::unix::fs::PermissionsExt;
        std::fs::create_dir_all(at.parent().unwrap()).unwrap();
        let body = if version.is_empty() {
            "#!/bin/sh\nexit 1\n".to_string()
        } else {
            let exe = reports
                .map(|p| p.display().to_string())
                .unwrap_or_else(|| "$0".to_string());
            format!("#!/bin/sh\necho {version}\necho \"{exe}\"\n")
        };
        std::fs::write(at, body).unwrap();
        std::fs::set_permissions(at, std::fs::Permissions::from_mode(0o755)).unwrap();
        at.to_path_buf()
    }

    fn install(root: &Path, version: &str) -> PathBuf {
        fake_ruby(&root.join(version).join("bin").join("ruby"), version, None)
    }

    #[test]
    fn identify_reads_the_version_and_the_executable_behind_the_command() {
        let tmp = tempfile::tempdir().unwrap();
        let real = install(tmp.path(), "4.0.6");
        let shim = fake_ruby(&tmp.path().join("shims").join("ruby"), "4.0.6", Some(&real));
        let found = identify(shim.to_str().unwrap(), None).unwrap();
        assert_eq!(
            found,
            HostRuby {
                path: real.display().to_string(),
                version: (4, 0, 6)
            }
        );
        // A `RbConfig.ruby` that is not a file: the command itself stands.
        let odd = fake_ruby(
            &tmp.path().join("odd").join("ruby"),
            "4.0.6",
            Some(Path::new("/no/such/ruby")),
        );
        assert_eq!(
            identify(odd.to_str().unwrap(), None).unwrap().path,
            odd.display().to_string()
        );
        // Too old, or a failing script.
        let old = install(tmp.path(), "3.4.1");
        assert!(identify(old.to_str().unwrap(), None).is_none());
        let broken = fake_ruby(&tmp.path().join("broken").join("ruby"), "", None);
        assert!(identify(broken.to_str().unwrap(), Some(tmp.path())).is_none());
    }

    #[test]
    fn the_pin_overrides_the_ranking() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().join("versions");
        let exact = install(&root, "4.0.6");
        let far = install(&root, "4.2.0");
        let found = find_among(
            (4, 0, 6),
            None,
            far.to_str(),
            None,
            collect_versioned(&[root.clone()]),
            &[],
        );
        assert_eq!(found.unwrap().path, far.display().to_string());
        // A pin that does not run falls back to the ranking.
        let broken = fake_ruby(&tmp.path().join("broken").join("ruby"), "", None);
        let found = find_among(
            (4, 0, 6),
            None,
            broken.to_str(),
            None,
            collect_versioned(&[root]),
            &[],
        );
        assert_eq!(found.unwrap().path, exact.display().to_string());
    }

    #[test]
    fn path_wins_a_tie_and_loses_to_a_closer_install() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().join("versions");
        let v5 = install(&root, "4.0.5");
        let v6 = install(&root, "4.0.6");
        let elsewhere = fake_ruby(&tmp.path().join("sys").join("ruby"), "4.0.6", None);
        let on_path = identify(elsewhere.to_str().unwrap(), None);
        // Same version as an install: the one on PATH keeps its gems.
        let found = find_among(
            (4, 0, 6),
            None,
            None,
            on_path.clone(),
            collect_versioned(&[root.clone()]),
            &[],
        );
        assert_eq!(found.unwrap().path, elsewhere.display().to_string());
        // Further from the pin than an install: the install wins.
        let found = find_among(
            (4, 0, 5),
            None,
            None,
            on_path,
            collect_versioned(&[root.clone()]),
            &[],
        );
        assert_eq!(found.unwrap().path, v5.display().to_string());
        // `ruby` on PATH *is* one of the installs: listed once, as PATH.
        let on_path = identify(v6.to_str().unwrap(), None);
        let found = find_among(
            (4, 0, 6),
            None,
            None,
            on_path,
            collect_versioned(&[root]),
            &[],
        );
        assert_eq!(found.unwrap().path, v6.display().to_string());
    }

    #[test]
    fn an_install_that_does_not_run_hands_over_then_the_shims() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().join("versions");
        // Closest by name, but its `bin/ruby` fails to run.
        fake_ruby(&root.join("4.0.6").join("bin").join("ruby"), "", None);
        let next = install(&root, "4.0.7");
        let found = find_among(
            (4, 0, 6),
            None,
            None,
            None,
            collect_versioned(&[root.clone()]),
            &[],
        );
        assert_eq!(found.unwrap().path, next.display().to_string());
        // Nothing usable among the installs: the shims, in order.
        let dead_shim = fake_ruby(&tmp.path().join("rvm").join("ruby"), "", None);
        let real = install(&tmp.path().join("hidden"), "4.0.6");
        let shim = fake_ruby(&tmp.path().join("shims").join("ruby"), "4.0.6", Some(&real));
        let empty = tmp.path().join("none");
        let found = find_among(
            (4, 0, 6),
            None,
            None,
            None,
            collect_versioned(&[empty.clone()]),
            &[dead_shim.clone(), shim],
        );
        assert_eq!(found.unwrap().path, real.display().to_string());
        // And with no shim answering either, no host Ruby.
        assert!(
            find_among(
                (4, 0, 6),
                None,
                None,
                None,
                collect_versioned(&[empty]),
                &[dead_shim]
            )
            .is_none()
        );
        assert!(
            find_among(
                (4, 0, 6),
                None,
                None,
                None,
                vec![],
                &[tmp.path().join("missing")]
            )
            .is_none()
        );
    }

    /// The process environment is shared by every test in this binary
    /// (under plain `cargo test`; nextest gives each its own process), so
    /// the tests that read or set it take this lock, and the one that
    /// sets it puts everything back.
    static ENV_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    #[test]
    fn version_manager_roots_follow_the_managers_variables() {
        let _env = ENV_LOCK.lock().unwrap();
        const VARS: [&str; 5] = [
            "HOME",
            "RBENV_ROOT",
            "ASDF_DATA_DIR",
            "PATH",
            "MONORUBY_RUBY",
        ];
        let saved: Vec<_> = VARS.iter().map(|v| (*v, std::env::var_os(v))).collect();
        struct Restore(Vec<(&'static str, Option<std::ffi::OsString>)>);
        impl Drop for Restore {
            fn drop(&mut self) {
                for (var, value) in &self.0 {
                    // SAFETY: serialized by ENV_LOCK, and nothing here spawns a
                    // thread that reads the environment concurrently.
                    unsafe {
                        match value {
                            Some(v) => std::env::set_var(var, v),
                            None => std::env::remove_var(var),
                        }
                    }
                }
            }
        }
        let _restore = Restore(saved);
        let tmp = tempfile::tempdir().unwrap();
        // SAFETY: as above — this test is the only writer, under the lock.
        unsafe {
            std::env::set_var("HOME", tmp.path());
            std::env::set_var("RBENV_ROOT", tmp.path().join("rb"));
            std::env::set_var("ASDF_DATA_DIR", tmp.path().join("as"));
        }
        let roots = version_manager_roots();
        assert_eq!(
            roots,
            vec![
                tmp.path().join("rb").join("versions"),
                tmp.path().join("as").join("installs").join("ruby"),
                tmp.path().join(".rvm").join("rubies"),
                tmp.path().join(".local/share/mise/installs/ruby"),
            ]
        );
        assert_eq!(
            shims(),
            vec![
                tmp.path().join(".rbenv/shims/ruby"),
                tmp.path().join(".rvm/bin/ruby")
            ]
        );
        // `find` itself, over that empty home with no `ruby` on PATH.
        unsafe {
            std::env::set_var("PATH", tmp.path());
            std::env::remove_var("MONORUBY_RUBY");
        }
        assert!(find((4, 0, 6), None).is_none());
        let pinned = install(&tmp.path().join("rb").join("versions"), "4.0.6");
        assert_eq!(
            find((4, 0, 6), None).unwrap().path,
            pinned.display().to_string()
        );
        unsafe {
            std::env::set_var("MONORUBY_RUBY", &pinned);
        }
        assert_eq!(
            find((4, 2, 0), None).unwrap().path,
            pinned.display().to_string()
        );
        unsafe {
            std::env::remove_var("HOME");
            std::env::remove_var("RBENV_ROOT");
            std::env::remove_var("ASDF_DATA_DIR");
        }
        assert!(version_manager_roots().is_empty());
        assert!(shims().is_empty());
    }

    /// Whatever `find` settles on is an executable, the same one from
    /// every directory (so not a shim), and nothing closer to the target
    /// is installed. Skipped on a host with no Ruby at all.
    #[test]
    fn the_found_ruby_is_the_closest_executable() {
        let _env = ENV_LOCK.lock().unwrap();
        let target = pinned_version();
        let Some(ruby) = find(target, None) else {
            eprintln!("skipped: no host ruby");
            return;
        };
        assert!(Path::new(&ruby.path).is_absolute(), "{}", ruby.path);
        let elsewhere = identify(&ruby.path, Some(&std::env::temp_dir())).unwrap();
        assert_eq!(elsewhere, ruby);
        // The ranking's view of the versioned installs: none may beat
        // what `find` answered.
        if let Some((_, best)) = closest_to(versioned_rubies(), target)
            && std::env::var_os("MONORUBY_RUBY").is_none()
        {
            assert!(
                distance(ruby.version, target) <= distance(best, target),
                "{ruby:?} is further from the pin than an installed {best:?}"
            );
        }
    }
}
