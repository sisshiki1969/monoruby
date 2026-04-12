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
                if self.loaded_canonicalized_files.get(&file).is_some() {
                    return Ok(None);
                }
                file.set_extension("so");
                if self.loaded_canonicalized_files.get(&file).is_some() {
                    return Ok(None);
                }
            } else if self.loaded_canonicalized_files.get(&file).is_some() {
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

        // Bare path: check loaded_canonicalized_files, then search $LOAD_PATH.
        if !is_relative {
            let mut file = PathBuf::from(file_name);
            file.set_extension("rb");
            if self.loaded_canonicalized_files.get(&file).is_some() {
                return Ok(None);
            }
            file.set_extension("so");
            if self.loaded_canonicalized_files.get(&file).is_some() {
                return Ok(None);
            }

            if let Some(file) = self.search_lib(file_name) {
                return self.require_lib_file(file);
            }
        }
        Err(MonorubyErr::cant_load(None, file_name))
    }

    pub(crate) fn search_lib(&mut self, file_name: &std::path::Path) -> Option<PathBuf> {
        // Priority: ~/.monoruby/lib/<name>.rb overrides both $LOAD_PATH
        // and native .so extensions. The files installed there by
        // build.rs (from stdlib/ and gem/) are stubs that replace native
        // extensions monoruby cannot load. They must take precedence,
        // otherwise CRuby's stdlib wrappers (which themselves require
        // more things monoruby cannot handle) would be loaded first and
        // fail.
        if file_name.extension().is_none() {
            let mut fallback = dirs::home_dir()
                .unwrap()
                .join(".monoruby")
                .join("lib")
                .join(file_name);
            fallback.set_extension("rb");
            if fallback.exists() {
                return Some(fallback);
            }
        }
        for lib in self.load_path.as_array().iter() {
            let lib = match lib.is_str() {
                Some(s) => s,
                None => continue,
            };
            let mut lib = std::path::PathBuf::from(lib).join(file_name);
            if lib.extension().is_some() {
                if lib.exists() {
                    return Some(lib);
                } else {
                    continue;
                }
            }
            lib.set_extension("rb");
            if lib.exists() {
                return Some(lib);
            }
            lib.set_extension("so");
            if lib.exists() {
                return Some(lib);
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
        if self
            .loaded_canonicalized_files
            .get(&canonicalized_path)
            .is_some()
        {
            return Ok(None);
        }
        let res = if let Some(b"so") = canonicalized_path.extension().map(|s| s.as_bytes()) {
            let mut lib = dirs::home_dir()
                .unwrap()
                .join(".monoruby")
                .join("lib")
                .join(canonicalized_path.file_name().unwrap());
            lib.set_extension("rb");
            load_file(&lib)?
        } else {
            load_file(&canonicalized_path)?
        };
        self.loaded_canonicalized_files
            .insert(canonicalized_path.to_path_buf());
        Ok(Some(res))
    }

    ///
    /// Find and read a file for `Kernel#load`.
    ///
    /// Unlike `require_lib`, this function:
    /// - Does NOT check or update `loaded_canonicalized_files`.
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
        Ok(res) => Ok(res),
        Err(err) => Err(MonorubyErr::cant_load(Some(err), path)),
    }
}
