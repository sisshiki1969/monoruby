use std::os::unix::ffi::OsStrExt;

use super::*;

impl Globals {
    ///
    /// Load external library.
    ///
    pub(crate) fn load_lib(
        &mut self,
        file_name: &std::path::Path,
        is_relative: bool,
    ) -> Result<Option<(String, std::path::PathBuf, std::path::PathBuf)>> {
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
                return self.load_lib_file(file);
            }
        } else if file_name.exists() {
            return self.load_lib_file(file_name.into());
        }
        Err(MonorubyErr::cant_load(None, file_name))
    }

    pub(crate) fn search_lib(&mut self, file_name: &std::path::Path) -> Option<PathBuf> {
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
    fn load_lib_file(
        &mut self,
        path: std::path::PathBuf,
    ) -> Result<Option<(String, std::path::PathBuf, std::path::PathBuf)>> {
        let canonicalized_path = match path.canonicalize() {
            Ok(path) => path,
            Err(err) => return Err(MonorubyErr::cant_load(Some(err), &path)),
        };
        if self
            .loaded_canonicalized_files
            .get(&canonicalized_path)
            .is_some()
        {
            return Ok(None);
        }
        match self.load(&canonicalized_path) {
            Ok(res) => {
                Ok(res.map(|(file_body, load_path)| (file_body, load_path, canonicalized_path)))
            }
            Err(err) => Err(MonorubyErr::cant_load(Some(err), &canonicalized_path)),
        }
    }

    fn load(
        &mut self,
        canonicalized_path: &std::path::Path,
    ) -> std::result::Result<Option<(String, std::path::PathBuf)>, std::io::Error> {
        let mut file_body = String::new();
        let load_path = if let Some(b"so") = canonicalized_path.extension().map(|s| s.as_bytes()) {
            let mut lib = dirs::home_dir()
                .unwrap()
                .join(".monoruby")
                .join(canonicalized_path.file_name().unwrap());
            lib.set_extension("rb");
            lib
        } else {
            canonicalized_path.to_path_buf()
        };
        let mut file = std::fs::OpenOptions::new().read(true).open(&load_path)?;
        file.read_to_string(&mut file_body)?;
        self.loaded_canonicalized_files
            .insert(canonicalized_path.to_path_buf());
        Ok(Some((file_body, load_path)))
    }
}

pub fn load_file(path: &std::path::PathBuf) -> Result<(String, std::path::PathBuf)> {
    fn inner(path: &std::path::PathBuf) -> std::io::Result<(String, std::path::PathBuf)> {
        let canonicalized_path = path.canonicalize()?;
        let mut file_body = String::new();
        let mut file = std::fs::OpenOptions::new()
            .read(true)
            .open(&canonicalized_path)?;
        file.read_to_string(&mut file_body)?;
        Ok((file_body, canonicalized_path))
    }

    match inner(path) {
        Ok(res) => Ok(res),
        Err(err) => Err(MonorubyErr::cant_load(Some(err), path)),
    }
}
