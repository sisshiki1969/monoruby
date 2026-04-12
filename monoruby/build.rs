use std::path::{Path, PathBuf};
use std::process::Command;
use std::{fs, io};

fn main() {
    let lib_path = dirs::home_dir().unwrap().join(".monoruby");

    let lib_dir = lib_path.join("lib");
    let builtins_dir = lib_path.join("builtins");
    let sources = [
        (PathBuf::from("builtins"), builtins_dir.clone()),
        (PathBuf::from("stdlib"), lib_dir.clone()),
        (PathBuf::from("gem"), lib_dir.clone()),
    ];

    for (src, _) in &sources {
        println!("cargo:rerun-if-changed={}", src.display());
    }

    if !lib_path.exists() {
        fs::create_dir(&lib_path).unwrap();
    }
    for p in [&lib_dir, &builtins_dir] {
        if p.exists() {
            fs::remove_dir_all(p).unwrap();
        }
        fs::create_dir(p).unwrap();
    }

    match Command::new("ruby").args(["-e", "puts($:)"]).output() {
        Ok(output) => {
            let dest_path = lib_path.join("library_path");
            let load_path = std::str::from_utf8(&output.stdout).unwrap();
            fs::write(dest_path, load_path).unwrap();
        }
        Err(_) => {
            eprintln!("failed to read ruby library path");
        }
    }

    match Command::new("ruby")
        .args(["-e", "puts(RUBY_VERSION)"])
        .output()
    {
        Ok(output) => {
            let dest_path = lib_path.join("ruby_version");
            let load_path = std::str::from_utf8(&output.stdout).unwrap();
            fs::write(dest_path, load_path).unwrap();
        }
        Err(_) => {
            eprintln!("failed to read ruby version");
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
