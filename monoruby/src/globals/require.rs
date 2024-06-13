use std::os::unix::ffi::OsStrExt;

use super::*;
use dirs;

impl Globals {
    ///
    /// Load external library.
    ///
    pub(crate) fn load_lib(
        &mut self,
        file_name: &std::path::Path,
        is_relative: bool,
    ) -> Result<Option<(String, std::path::PathBuf)>> {
        if !is_relative {
            if let Some(file) = self.search_lib(file_name) {
                return self.load_file(dbg!(file));
            }
        } else {
            if file_name.exists() {
                return self.load_file(file_name.into());
            }
        }
        Err(MonorubyErr::cant_load(None, file_name))
    }

    fn search_lib(&mut self, file_name: &std::path::Path) -> Option<PathBuf> {
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
    fn load_file(
        &mut self,
        path: std::path::PathBuf,
    ) -> Result<Option<(String, std::path::PathBuf)>> {
        let path = match path.canonicalize() {
            Ok(path) => path,
            Err(err) => return Err(MonorubyErr::cant_load(Some(err), &path)),
        };
        if self.loaded_canonicalized_files.get(&path).is_some() {
            return Ok(None);
        }
        self.load(&path)
            .map_err(|err| MonorubyErr::cant_load(Some(err), &path))
    }

    fn load(
        &mut self,
        path: &std::path::Path,
    ) -> std::result::Result<Option<(String, std::path::PathBuf)>, std::io::Error> {
        let mut file_body = String::new();
        let load_path = if let Some(b"so") = path.extension().map(|s| s.as_bytes()) {
            let mut lib = dirs::home_dir()
                .unwrap()
                .join(".monoruby")
                .join(path.file_name().unwrap());
            lib.set_extension("rb");
            lib
        } else {
            path.to_path_buf()
        };
        let mut file = std::fs::OpenOptions::new().read(true).open(&load_path)?;
        file.read_to_string(&mut file_body)?;
        self.loaded_canonicalized_files.insert(path.to_path_buf());
        Ok(Some((file_body, load_path)))
    }
}
