use std::path::{Path, PathBuf};
use std::process::Command;
use std::{fs, io};

/// Minimum CRuby version whose stdlib monoruby is willing to wire up at
/// build time. Older Rubies (Debian's system 3.0, for example) lack APIs
/// monoruby relies on and have a different Hash inspect form.
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

/// Pick a `ruby` executable that is at least `MIN_RUBY_VERSION`. Returns
/// `None` if no suitable Ruby is found, in which case build.rs leaves the
/// cached library_path / ruby_version files untouched and prints a warning.
fn find_ruby() -> Option<String> {
    if ruby_version_ok("ruby") {
        return Some("ruby".to_string());
    }
    if let Some(home) = std::env::var_os("HOME") {
        let shim = PathBuf::from(home).join(".rbenv/shims/ruby");
        if let Some(s) = shim.to_str()
            && ruby_version_ok(s)
        {
            return Some(s.to_string());
        }
    }
    if let Some(home) = std::env::var_os("HOME") {
        let rvm = PathBuf::from(home).join(".rvm/bin/ruby");
        if let Some(s) = rvm.to_str()
            && ruby_version_ok(s)
        {
            return Some(s.to_string());
        }
    }
    None
}

fn main() {
    let lib_path = dirs::home_dir().unwrap().join(".monoruby");

    let lib_dir = lib_path.join("lib");
    let builtins_dir = lib_path.join("builtins");
    let sources = [
        (PathBuf::from("builtins"), builtins_dir.clone()),
        (PathBuf::from("stdlib"), lib_dir.clone()),
        (PathBuf::from("gem"), lib_dir.clone()),
    ];

    //for (src, _) in &sources {
    //    println!("cargo:rerun-if-changed={}", src.display());
    //}

    if !lib_path.exists() {
        fs::create_dir(&lib_path).unwrap();
    }
    for p in [&lib_dir, &builtins_dir] {
        if p.exists() {
            fs::remove_dir_all(p).unwrap();
        }
        fs::create_dir(p).unwrap();
    }

    match find_ruby() {
        Some(ruby) => {
            match Command::new(&ruby).args(["-e", "puts($:)"]).output() {
                Ok(output) => {
                    let dest_path = lib_path.join("library_path");
                    let load_path = std::str::from_utf8(&output.stdout).unwrap();
                    fs::write(dest_path, load_path).unwrap();
                }
                Err(_) => {
                    println!("cargo:warning=failed to read ruby library path from {ruby}");
                }
            }

            match Command::new(&ruby)
                .args(["-e", "puts(RUBY_VERSION)"])
                .output()
            {
                Ok(output) => {
                    let dest_path = lib_path.join("ruby_version");
                    let load_path = std::str::from_utf8(&output.stdout).unwrap();
                    fs::write(dest_path, load_path).unwrap();
                }
                Err(_) => {
                    println!("cargo:warning=failed to read ruby version from {ruby}");
                }
            }
        }
        None => {
            println!(
                "cargo:warning=no Ruby >= {}.{} found on PATH or in rbenv/rvm; \
                 ~/.monoruby/library_path and ruby_version were not refreshed",
                MIN_RUBY_VERSION.0, MIN_RUBY_VERSION.1
            );
        }
    }

    for (src, dst) in &sources {
        copy_dir_all(src, dst).unwrap();
    }
}

fn copy_dir_all(src: &Path, dst: &Path) -> io::Result<()> {
    if !fs::exists(dst)? {
        fs::create_dir(dst)?;
    }
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if from.is_dir() {
            copy_dir_all(&from, &to)?;
        } else {
            fs::copy(&from, &to)?;
        }
    }
    Ok(())
}
