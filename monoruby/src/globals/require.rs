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
        let stub_root = dirs::home_dir().unwrap().join(".monoruby").join("stub");
        for dir in [stub_root.clone(), stub_root.join("x86_64-linux")] {
            if let Some(p) = probe(&dir, file_name) {
                return Some(p);
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
        let canonicalized_path = path.canonicalize().unwrap_or_else(|_| path.clone());
        if self.is_feature_loaded(&canonicalized_path) {
            return Ok(None);
        }
        let res = if let Some(b"so") = canonicalized_path.extension().map(|s| s.as_bytes()) {
            let monoruby_lib = dirs::home_dir().unwrap().join(".monoruby").join("lib");
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
        Ok(Some(res))
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

pub fn load_file(path: &std::path::Path) -> Result<(String, std::path::PathBuf)> {
    fn inner(path: &std::path::Path) -> std::io::Result<(String, std::path::PathBuf)> {
        // Read the file first; this gives a clear error if the file doesn't exist.
        let mut file_body = String::new();
        let mut file = std::fs::OpenOptions::new().read(true).open(path)?;
        file.read_to_string(&mut file_body)?;
        // Try to canonicalize the path for dedup tracking;
        // fall back to the original path if canonicalize fails.
        let resolved_path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        Ok((file_body, resolved_path))
    }

    match inner(path) {
        Ok(res) => {
            // Closure-measurement hook (Phase 1 of decoupling from a host
            // Ruby): when MONORUBY_TRACE_LOAD is set, emit every resolved
            // load path so the transitive stdlib closure can be captured
            // by running the test/spec suite and filtering paths that
            // resolve under the CRuby $LOAD_PATH vs ~/.monoruby. Gated by
            // an env var so it has zero cost in normal runs.
            if std::env::var_os("MONORUBY_TRACE_LOAD").is_some() {
                eprintln!("MONORUBY_LOADED\t{}", res.1.display());
            }
            Ok(res)
        }
        Err(err) => Err(MonorubyErr::cant_load(Some(err), path)),
    }
}
