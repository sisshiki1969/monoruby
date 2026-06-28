use std::os::unix::ffi::OsStrExt;

use super::*;

impl Globals {
    ///
    /// Load external library.
    ///
    pub(crate) fn require_lib(
        &mut self,
        file_name: &std::path::Path,
        is_relative: bool,
    ) -> Result<Option<(String, std::path::PathBuf)>> {
        let path_str = file_name.to_string_lossy();

        // Absolute path: try to load directly.
        if path_str.starts_with('/') {
            // Check if already loaded (with .rb or .so extension variants).
            let mut file = PathBuf::from(file_name);
            if file.extension().is_none() {
                file.set_extension("rb");
                if self.is_feature_loaded(&file) {
                    return Ok(None);
                }
                file.set_extension("so");
                if self.is_feature_loaded(&file) {
                    return Ok(None);
                }
            } else if self.is_feature_loaded(&file) {
                return Ok(None);
            }
            // Try the file with its given extension first.
            if file_name.is_file() {
                return self.require_lib_file(file_name.into());
            }
            // Try appending .rb extension if none given.
            if file_name.extension().is_none() {
                let mut with_rb = PathBuf::from(file_name);
                with_rb.set_extension("rb");
                if with_rb.is_file() {
                    return self.require_lib_file(with_rb);
                }
                let mut with_so = PathBuf::from(file_name);
                with_so.set_extension("so");
                if with_so.is_file() {
                    return self.require_lib_file(with_so);
                }
            }
            return Err(MonorubyErr::cant_load(None, file_name));
        }

        // Relative path (starts with ./ or ../): resolve from CWD.
        if is_relative || path_str.starts_with("./") || path_str.starts_with("../") {
            let resolved = if let Ok(cwd) = std::env::current_dir() {
                cwd.join(file_name)
            } else {
                file_name.into()
            };
            if resolved.is_file() {
                return self.require_lib_file(resolved);
            }
            // Try with .rb and .so extensions.
            if resolved.extension().is_none() {
                let mut with_rb = resolved.clone();
                with_rb.set_extension("rb");
                if with_rb.is_file() {
                    return self.require_lib_file(with_rb);
                }
                let mut with_so = resolved.clone();
                with_so.set_extension("so");
                if with_so.is_file() {
                    return self.require_lib_file(with_so);
                }
            }
            return Err(MonorubyErr::cant_load(None, file_name));
        }

        // Bare path: check $LOADED_FEATURES, then search $LOAD_PATH.
        if !is_relative {
            let mut file = PathBuf::from(file_name);
            file.set_extension("rb");
            if self.is_feature_loaded(&file) {
                return Ok(None);
            }
            file.set_extension("so");
            if self.is_feature_loaded(&file) {
                return Ok(None);
            }

            if let Some(file) = self.search_lib(file_name) {
                return self.require_lib_file(file);
            }
        }
        Err(MonorubyErr::cant_load(None, file_name))
    }

    pub(crate) fn search_lib(&mut self, file_name: &std::path::Path) -> Option<PathBuf> {
        // Probe a single directory for `file_name`, applying the usual
        // extension rules: an explicit extension must match exactly;
        // otherwise try `.rb` then `.so`.
        fn probe(dir: &std::path::Path, file_name: &std::path::Path) -> Option<PathBuf> {
            let mut lib = dir.join(file_name);
            if lib.extension().is_some() {
                return lib.exists().then_some(lib);
            }
            lib.set_extension("rb");
            if lib.exists() {
                return Some(lib);
            }
            lib.set_extension("so");
            lib.exists().then_some(lib)
        }

        // Pin monoruby's own C-extension replacement stubs ahead of
        // `$LOAD_PATH`. `~/.monoruby/stub` holds exactly the files
        // monoruby ships in `stdlib/` and `gem/` (json, psych, strscan,
        // stringio, zlib, …) — pure-Ruby replacements for libraries
        // monoruby cannot load as native `.so`. They must win even after
        // rubygems/bundler `activate`s a host gem and unshifts its lib
        // dir to the front of `$LOAD_PATH`; checking them here, outside
        // the (mutable) `$LOAD_PATH` loop, makes the precedence immune to
        // that reordering. Without this, a host C-extension gem (now
        // accepted, since `Gem.extension_api_version` no longer appends
        // `-static`) would shadow the stub and then fail to load its
        // native `.so`.
        //
        // Only this stub root is pinned — NOT the vendored CRuby stdlib
        // snapshot in `~/.monoruby/lib`. That snapshot includes bundler /
        // rubygems, whose loaded *code* version must stay in lockstep
        // with the activated gem *spec* version (bundler raises
        // `CorruptBundlerInstallError` otherwise). Pinning the vendored
        // bundler ahead of the host one would force vendored code while
        // the host spec stays activated, splitting the two. So the
        // vendored snapshot keeps resolving through `$LOAD_PATH` (where it
        // is merely first), letting host activation shadow it consistently.
        let stub_root = install_root().join("stub");
        for dir in [stub_root.clone(), stub_root.join(ruby_platform())] {
            if let Some(p) = probe(&dir, file_name) {
                return Some(p);
            }
        }

        // `bundler` (and everything under `bundler/`) must resolve to the
        // *host* copy whenever one is installed, never the vendored snapshot
        // in `~/.monoruby/lib`. The vendored bundler is only a no-host
        // fallback; its version rarely matches an installed host gemspec, so
        // once it loads, `Bundler.setup`'s
        // `Gem.bin_path("bundler", "bundle", Bundler::VERSION)` raises
        // `GemNotFoundException`. Normally host gem *activation* unshifts the
        // host bundler ahead of the vendored dir, but rubygems' require
        // fast-path skips activation when there are no unresolved deps — e.g.
        // yjit-bench's `--harness=harness-warmup` does `require "benchmark"`
        // before `require "bundler"`, emptying unresolved deps — and the
        // vendored copy, first on `$LOAD_PATH`, then wins. Pin host
        // precedence here so the outcome no longer depends on activation
        // order. When the host has no bundler the normal loop below still
        // falls back to the vendored copy.
        let is_bundler = {
            let s = file_name.to_string_lossy();
            s == "bundler" || s.starts_with("bundler/")
        };
        if is_bundler {
            let vendored_lib = install_root().join("lib");
            for lib in self.load_path.as_array().iter() {
                let Some(lib) = lib.is_str() else {
                    continue;
                };
                let path = std::path::Path::new(lib);
                if path.starts_with(&vendored_lib) {
                    continue;
                }
                if let Some(p) = probe(path, file_name) {
                    return Some(p);
                }
            }
        }

        for lib in self.load_path.as_array().iter() {
            let lib = match lib.is_str() {
                Some(s) => s,
                None => continue,
            };
            if let Some(p) = probe(std::path::Path::new(lib), file_name) {
                return Some(p);
            }
        }
        None
    }

    ///
    /// Load the library if it has never been loaded before.
    ///
    /// If the library was loaded, return the code and canonical path.
    /// Otherwise, returns Ok(None).
    ///
    /// When an error occured in loading, returns Err.
    ///
    fn require_lib_file(
        &mut self,
        path: std::path::PathBuf,
    ) -> Result<Option<(String, std::path::PathBuf)>> {
        // CRuby stores the path as passed to `require`, not its
        // symlink-resolved form. `Path::canonicalize` resolves every
        // symlink (e.g. on macOS where `/tmp` is a symlink to
        // `/private/tmp`), which causes `$LOADED_FEATURES.replace($"
        // - ['/tmp/foo.rb'])` to fail to remove the entry and the
        // subsequent re-require to silently no-op. Use `path::absolute`
        // (no symlink resolution) and then collapse `.` / `..`
        // lexically via `lexically_normalize` — `path::absolute` alone
        // keeps `..` on POSIX, so different `..` spellings of the same
        // file would dedup-miss and double-load. This matches CRuby's
        // `File.expand_path` keying while leaving symlinks untouched.
        let canonicalized_path = lexically_normalize(
            &std::path::absolute(&path).unwrap_or_else(|_| path.clone()),
        );
        if self.is_feature_loaded(&canonicalized_path) {
            return Ok(None);
        }
        let (file_body, _resolved) = if let Some(b"so") = canonicalized_path.extension().map(|s| s.as_bytes()) {
            let monoruby_lib = install_root().join("lib");
            let relative = self
                .load_path
                .as_array()
                .iter()
                .filter_map(|entry| {
                    let prefix = PathBuf::from(entry.is_str()?);
                    canonicalized_path.strip_prefix(&prefix).ok()
                })
                .next()
                .unwrap_or_else(|| canonicalized_path.file_name().unwrap().as_ref());
            let mut lib = monoruby_lib.join(relative);
            lib.set_extension("rb");
            load_file(&lib)?
        } else {
            load_file(&canonicalized_path)?
        };
        self.add_loaded_feature(&canonicalized_path);
        // Return `canonicalized_path` (the `path::absolute` form we just
        // registered) rather than `load_file`'s symlink-resolved path:
        // `Executor::require` uses the returned path to
        // `remove_loaded_feature` if the require body raises, and it must
        // match the entry we added or the cleanup silently misses (on
        // macOS `/var/folders/..`→`/private/var/folders/..` symlinks the
        // two diverge, leaving a failed require un-retriable).
        Ok(Some((file_body, canonicalized_path)))
    }

    ///
    /// Find and read a file for `Kernel#load`.
    ///
    /// Unlike `require_lib`, this function:
    /// - Does NOT check or update `$LOADED_FEATURES`.
    /// - Does NOT add `.rb` / `.so` extensions automatically.
    /// - Absolute paths are loaded directly.
    /// - Paths starting with `./` or `../` are resolved relative to CWD.
    /// - Bare filenames are searched in `$LOAD_PATH`, then tried relative
    ///   to CWD as a fallback.
    ///
    pub(crate) fn find_for_load(
        &mut self,
        file_name: &std::path::Path,
    ) -> Result<(String, std::path::PathBuf)> {
        let path_str = file_name.to_string_lossy();

        // Absolute path: load directly.
        if path_str.starts_with('/') {
            return load_file(file_name);
        }

        // Relative to CWD (starts with ./ or ../): resolve against CWD first.
        if path_str.starts_with("./") || path_str.starts_with("../") {
            let resolved = if let Ok(cwd) = std::env::current_dir() {
                cwd.join(file_name)
            } else {
                file_name.into()
            };
            return load_file(&resolved);
        }

        // Bare filename: search $LOAD_PATH.
        for lib in self.load_path.as_array().iter() {
            let lib = match lib.is_str() {
                Some(s) => s,
                None => continue,
            };
            let lib = std::path::PathBuf::from(lib).join(file_name);
            if let Ok(res) = load_file(&lib) {
                return Ok(res);
            }
        }
        // Fallback: try relative to CWD.
        if let Ok(cwd) = std::env::current_dir() {
            let resolved = cwd.join(file_name);
            if resolved.is_file() {
                return load_file(&resolved);
            }
        }
        load_file(file_name)
    }
}

///
/// Lexically collapse `.` and `..` components without touching the
/// filesystem (no symlink resolution), mirroring CRuby's
/// `File.expand_path` semantics used to key `$LOADED_FEATURES`.
///
/// `std::path::absolute` makes a path absolute but, on POSIX, keeps `..`
/// to preserve symlink meaning. Two `require_relative` paths that name
/// the same file via different `..` spellings — e.g. `a/fixtures/x` and
/// `a/shared/../fixtures/x` — would then get distinct loaded-feature
/// keys and load the file twice (ruby/spec's `core/struct` fixtures hit
/// exactly this, retriggering `class Honda < Car` and raising
/// "superclass mismatch"). Collapsing `..` lexically here dedups them
/// while still leaving symlinks untouched (so the macOS
/// `/tmp`→`/private/tmp` `$LOADED_FEATURES` removal stays consistent).
///
fn lexically_normalize(path: &std::path::Path) -> std::path::PathBuf {
    use std::path::Component;
    let mut out = std::path::PathBuf::new();
    for comp in path.components() {
        match comp {
            Component::CurDir => {}
            Component::ParentDir => match out.components().next_back() {
                // Drop a preceding normal segment: `a/b/..` -> `a`.
                Some(Component::Normal(_)) => {
                    out.pop();
                }
                // `..` cannot rise above the root; absorb it.
                Some(Component::RootDir) | Some(Component::Prefix(_)) => {}
                // Leading `..` in a relative path: keep it.
                _ => out.push(comp),
            },
            _ => out.push(comp),
        }
    }
    out
}

pub fn load_file(path: &std::path::Path) -> Result<(String, std::path::PathBuf)> {
    read_source_file(path).map_err(|err| MonorubyErr::cant_load(Some(err), path))
}

///
/// Read a source file, returning the file body and the symlink-resolved
/// path. Unlike `load_file`, an I/O failure is returned as the raw
/// `std::io::Error` — callers that sit outside the VM (the program-file
/// load in `main`) must not produce a `MonorubyErr`, because no frame
/// exists to push a trace onto and a trace-less error cannot be
/// displayed properly.
///
pub fn read_source_file(
    path: &std::path::Path,
) -> std::io::Result<(String, std::path::PathBuf)> {
    // Read the file first; this gives a clear error if the file doesn't exist.
    let mut file_body = String::new();
    let mut file = std::fs::OpenOptions::new().read(true).open(path)?;
    file.read_to_string(&mut file_body)?;
    // Try to canonicalize the path for dedup tracking;
    // fall back to the original path if canonicalize fails.
    let resolved_path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    // Closure-measurement hook (Phase 1 of decoupling from a host
    // Ruby): when MONORUBY_TRACE_LOAD is set, emit every resolved
    // load path so the transitive stdlib closure can be captured
    // by running the test/spec suite and filtering paths that
    // resolve under the CRuby $LOAD_PATH vs ~/.monoruby. Gated by
    // an env var so it has zero cost in normal runs.
    if std::env::var_os("MONORUBY_TRACE_LOAD").is_some() {
        eprintln!("MONORUBY_LOADED\t{}", resolved_path.display());
    }
    Ok((file_body, resolved_path))
}
