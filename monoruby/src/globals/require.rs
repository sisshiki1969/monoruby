use std::os::unix::ffi::OsStrExt;

use super::*;

/// What `Globals::require_lib` resolved a `require` argument to.
pub(crate) enum RequireLoad {
    /// Not yet loaded: execute this body, registered in
    /// `$LOADED_FEATURES` under the given canonical path.
    Load(Vec<u8>, std::path::PathBuf),
    /// Already present in `$LOADED_FEATURES` under this path — which
    /// includes a load still *in progress* on another thread (features
    /// register before their body runs); `Executor::require` uses the
    /// path to consult the loading registry and block until the first
    /// loader finishes (CRuby's per-feature load lock).
    AlreadyLoaded(std::path::PathBuf),
}

/// The candidate feature paths `require` may load for *path*, in
/// CRuby's priority order. `require` never loads a file under its bare
/// name unless it already ends in `.rb` or `.so`: both `require "x"`
/// and `require "x.ext"` mean `x(.ext).rb` first, then the native
/// variant (which monoruby serves from its stub tree).
fn require_candidates(path: &std::path::Path) -> Vec<PathBuf> {
    match path.extension().map(|e| e.as_bytes()) {
        Some(b"rb") | Some(b"so") => vec![path.to_path_buf()],
        _ => {
            let s = path.as_os_str().to_string_lossy();
            vec![PathBuf::from(format!("{s}.rb")), PathBuf::from(format!("{s}.so"))]
        }
    }
}

/// The canonical (symlink-resolved) form of a `$LOAD_PATH` directory,
/// falling back to its lexically-normalized absolute form when the
/// directory doesn't exist.
fn canonical_dir_of(dir: &str) -> PathBuf {
    canonical_dir_of_path(std::path::Path::new(dir))
}

fn canonical_dir_of_path(dir: &std::path::Path) -> PathBuf {
    std::fs::canonicalize(dir).unwrap_or_else(|_| {
        lexically_normalize(&std::path::absolute(dir).unwrap_or_else(|_| dir.to_path_buf()))
    })
}

/// Expand a leading `~` to `$HOME` (CRuby's `require`/`load` shell
/// expansion). Paths without the tilde come back unchanged.
fn expand_tilde(path: &std::path::Path) -> PathBuf {
    let s = path.as_os_str().as_bytes();
    if s == b"~" || s.starts_with(b"~/") {
        if let Some(home) = std::env::var_os("HOME") {
            let mut out = PathBuf::from(home);
            if s.len() > 2 {
                out.push(std::ffi::OsStr::from_bytes(&s[2..]));
            }
            return out;
        }
    }
    path.to_path_buf()
}

/// Whether the request already names a loadable form (`.rb` / `.so`) —
/// such an as-given name matches `$LOADED_FEATURES` entries verbatim
/// (including manually stored relative forms like `"./x.rb"`), whereas
/// an extensionless request only falls back to its verbatim entry when
/// resolution fails.
fn has_loadable_ext(path: &std::path::Path) -> bool {
    matches!(path.extension().map(|e| e.as_bytes()), Some(b"rb") | Some(b"so"))
}

impl Globals {
    ///
    /// Load external library.
    ///
    pub(crate) fn require_lib(
        &mut self,
        vm: &mut Executor,
        file_name: &std::path::Path,
        is_relative: bool,
    ) -> Result<RequireLoad> {
        let file_name = &expand_tilde(file_name);
        let path_str = file_name.to_string_lossy();

        // Absolute path: try to load directly. Candidate-major order:
        // the `.rb` verdict (loaded or loadable) is reached before the
        // native variant is even considered, so a stored `x.so` entry
        // doesn't block loading `x.rb` for an extensionless request.
        if path_str.starts_with('/') {
            for cand in require_candidates(file_name) {
                let canon = lexically_normalize(&cand);
                if self.is_feature_loaded(&cand) || self.is_feature_loaded(&canon) {
                    return Ok(RequireLoad::AlreadyLoaded(canon));
                }
                if canon.is_file() {
                    return self.require_lib_file(canon);
                }
            }
            // The reported missing path is the lexically-normalized
            // request: `require_relative "../x"` joins its caller's dir
            // and CRuby reports (and stores in `LoadError#path`) the
            // collapsed form.
            return Err(MonorubyErr::cant_load(
                None,
                &lexically_normalize(file_name),
            ));
        }

        // Relative path (starts with ./ or ../): resolve from CWD.
        if is_relative || path_str.starts_with("./") || path_str.starts_with("../") {
            if has_loadable_ext(file_name) && self.is_feature_loaded(file_name) {
                return Ok(RequireLoad::AlreadyLoaded(file_name.into()));
            }
            // Lexically collapse `..` segments (CRuby expand_path
            // semantics): `bar/../x.rb` must resolve even when `bar`
            // doesn't exist on disk.
            let resolved = if let Ok(cwd) = std::env::current_dir() {
                lexically_normalize(&cwd.join(file_name))
            } else {
                file_name.into()
            };
            for cand in require_candidates(&resolved) {
                if cand.is_file() {
                    return self.require_lib_file(cand);
                }
            }
            // An extensionless verbatim entry converts the miss to
            // "already loaded" (require returns false).
            if self.is_feature_loaded(file_name) {
                return Ok(RequireLoad::AlreadyLoaded(file_name.into()));
            }
            return Err(MonorubyErr::cant_load(None, file_name));
        }

        // Bare path: check $LOADED_FEATURES, then search $LOAD_PATH.
        if !is_relative {
            // A verbatim stored entry under a loadable name — including
            // non-canonical spellings like `"code/../code/x.rb"`.
            if has_loadable_ext(file_name) && self.is_feature_loaded(file_name) {
                return Ok(RequireLoad::AlreadyLoaded(file_name.into()));
            }
            let entries = self.load_path_entries(vm);
            // For feature matching, `$LOAD_PATH` directories compare in
            // their canonical (symlink-resolved) form.
            let canon_dirs: Vec<PathBuf> = entries.iter().map(|d| canonical_dir_of(d)).collect();
            let bundler_priority = path_str == "bundler" || path_str.starts_with("bundler/");
            for cand in require_candidates(file_name) {
                if self.is_feature_loaded(&cand) {
                    return Ok(RequireLoad::AlreadyLoaded(cand));
                }
                // CRuby's feature index: an entry ending in
                // `/{candidate}` whose directory prefix is on the
                // (canonicalized) load path counts as loaded — the
                // `.rb` verdict is reached before the native pass, so
                // a `.rb` loaded through any load-path dir blocks
                // re-loading through another.
                if let Some(hit) = self.feature_suffix_loaded(&cand, &canon_dirs) {
                    return Ok(RequireLoad::AlreadyLoaded(hit));
                }
                if let Some(found) = self.search_candidate(&cand, &entries, bundler_priority) {
                    return self.require_lib_file(found);
                }
            }
            // Resolution failed: an extensionless verbatim entry means
            // "already loaded" (require returns false, not LoadError).
            if self.is_feature_loaded(file_name) {
                return Ok(RequireLoad::AlreadyLoaded(file_name.into()));
            }
        }
        Err(MonorubyErr::cant_load(None, file_name))
    }

    /// `$LOAD_PATH` as strings: String entries verbatim, non-String
    /// entries through the `#to_path` / `#to_str` protocol (CRuby's
    /// `rb_get_path` per entry); unconvertible entries are skipped.
    pub(crate) fn load_path_entries(&mut self, vm: &mut Executor) -> Vec<String> {
        let raw: Vec<Value> = self.load_path.as_array().iter().copied().collect();
        raw.into_iter()
            .filter_map(|v| {
                if let Some(s) = v.is_str() {
                    return Some(s.to_string());
                }
                let pathed = if let Some(fid) = self.check_method(v, IdentId::get_id("to_path")) {
                    vm.invoke_func_inner(self, fid, v, &[], None, None).ok()?
                } else {
                    v
                };
                if let Some(s) = pathed.is_str() {
                    return Some(s.to_string());
                }
                let fid = self.check_method(pathed, IdentId::get_id("to_str"))?;
                let s = vm.invoke_func_inner(self, fid, pathed, &[], None, None).ok()?;
                Some(s.is_str()?.to_string())
            })
            .collect()
    }

    /// A `$LOADED_FEATURES` entry that ends in `/{cand}` and whose
    /// directory prefix is one of the (canonicalized) load-path dirs.
    fn feature_suffix_loaded(
        &self,
        cand: &std::path::Path,
        canon_dirs: &[PathBuf],
    ) -> Option<PathBuf> {
        let suffix = format!("/{}", cand.display());
        for v in self.loaded_features.as_array().iter() {
            let Some(e) = v.is_str() else { continue };
            if e.len() > suffix.len() && e.ends_with(&suffix) {
                let prefix = &e[..e.len() - suffix.len()];
                if canon_dirs
                    .iter()
                    .any(|d| d.as_os_str().as_bytes() == prefix.as_bytes())
                {
                    return Some(PathBuf::from(e));
                }
            }
        }
        None
    }

    pub(crate) fn search_lib(
        &mut self,
        vm: &mut Executor,
        file_name: &std::path::Path,
    ) -> Option<PathBuf> {
        let entries = self.load_path_entries(vm);
        let s = file_name.to_string_lossy();
        let bundler_priority = s == "bundler" || s.starts_with("bundler/");
        for cand in require_candidates(file_name) {
            if let Some(p) = self.search_candidate(&cand, &entries, bundler_priority) {
                return Some(p);
            }
        }
        None
    }

    /// Search one concrete candidate (`x.rb` / `x.so` form) through the
    /// stub pin, the host-bundler override, and `$LOAD_PATH`. The
    /// returned path joins the *canonicalized* directory with the
    /// candidate as given: CRuby resolves symlinks in the `$LOAD_PATH`
    /// entry but never in the feature name, and that composite is what
    /// lands in `$LOADED_FEATURES`.
    fn search_candidate(
        &mut self,
        cand: &std::path::Path,
        entries: &[String],
        bundler_priority: bool,
    ) -> Option<PathBuf> {
        fn probe(dir: &std::path::Path, cand: &std::path::Path) -> Option<PathBuf> {
            if dir.join(cand).exists() {
                Some(canonical_dir_of_path(dir).join(cand))
            } else {
                None
            }
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
            if let Some(p) = probe(&dir, cand) {
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
        if bundler_priority {
            let vendored_lib = install_root().join("lib");
            for lib in entries {
                let path = std::path::Path::new(lib);
                if path.starts_with(&vendored_lib) {
                    continue;
                }
                if let Some(p) = probe(path, cand) {
                    return Some(p);
                }
            }
        }

        for lib in entries {
            if let Some(p) = probe(std::path::Path::new(lib), cand) {
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
    fn require_lib_file(&mut self, path: std::path::PathBuf) -> Result<RequireLoad> {
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
            return Ok(RequireLoad::AlreadyLoaded(canonicalized_path));
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
        Ok(RequireLoad::Load(file_body, canonicalized_path))
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
        vm: &mut Executor,
        file_name: &std::path::Path,
    ) -> Result<(Vec<u8>, std::path::PathBuf)> {
        let file_name = &expand_tilde(file_name);
        let path_str = file_name.to_string_lossy();

        // Absolute path: load directly. `__FILE__` (and
        // `Location#path`) keep the path *as given* — symlinks are NOT
        // resolved (CRuby); the canonical form is captured separately
        // at parse time for `Location#absolute_path`.
        if path_str.starts_with('/') {
            let (body, _) = load_file(file_name)?;
            return Ok((body, file_name.to_path_buf()));
        }

        // Relative to CWD (starts with ./ or ../): resolve against CWD first.
        if path_str.starts_with("./") || path_str.starts_with("../") {
            let resolved = if let Ok(cwd) = std::env::current_dir() {
                cwd.join(file_name)
            } else {
                file_name.into()
            };
            let (body, _) = load_file(&resolved)?;
            return Ok((body, resolved));
        }

        // Bare filename: search $LOAD_PATH (entries go through the
        // #to_path / #to_str protocol).
        for lib in self.load_path_entries(vm) {
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

pub fn load_file(path: &std::path::Path) -> Result<(Vec<u8>, std::path::PathBuf)> {
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
) -> std::io::Result<(Vec<u8>, std::path::PathBuf)> {
    // Read the file first; this gives a clear error if the file doesn't
    // exist. Raw bytes, NOT UTF-8-validated: a `# encoding: binary`
    // source may legitimately contain non-UTF-8 bytes in its literals,
    // and prism parses arbitrary bytes under the declared encoding
    // (invalid sequences surface as prism's own "invalid multibyte
    // char" SyntaxError, matching CRuby).
    let mut file_body = Vec::new();
    let mut file = std::fs::OpenOptions::new().read(true).open(path)?;
    file.read_to_end(&mut file_body)?;
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
