use dirs;
use std::process::Command;
use std::{fs, io};

fn main() {
    let lib_path = dirs::home_dir().unwrap().join(".monoruby");
    match lib_path.try_exists() {
        Ok(true) => {}
        _ => fs::create_dir(&lib_path).unwrap(),
    }

    match Command::new("ruby").args(["-e", "puts($:)"]).output() {
        Ok(output) => {
            let dest_path = lib_path.clone().join("library_path");
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
            let dest_path = lib_path.clone().join("ruby_version");
            let load_path = std::str::from_utf8(&output.stdout).unwrap();
            fs::write(dest_path, load_path).unwrap();
        }
        Err(_) => {
            eprintln!("failed to read ruby version");
        }
    }

    match Command::new("which").args(["ruby"]).output() {
        Ok(output) => {
            let dest_path = lib_path.clone().join("ruby_path");
            let load_path = std::str::from_utf8(&output.stdout).unwrap();
            fs::write(dest_path, load_path).unwrap();
        }
        Err(_) => {
            eprintln!("failed to read ruby path");
        }
    }

    let mut directories = vec![(std::path::PathBuf::from("startup"), lib_path)];

    while let Some((from_dir, to_dir)) = directories.pop() {
        directories.extend(copy_dir(from_dir, to_dir).unwrap());
    }

    //println!("cargo:rerun-if-changed=startup");
}

fn copy_dir(
    from_dir: std::path::PathBuf,
    to_dir: std::path::PathBuf,
) -> io::Result<Vec<(std::path::PathBuf, std::path::PathBuf)>> {
    match to_dir.try_exists() {
        Ok(true) => {}
        _ => fs::create_dir(&to_dir)?,
    }
    let mut directories = vec![];
    for entry in fs::read_dir(&from_dir)? {
        let from = entry.unwrap().path();
        let from_name = from.file_name().unwrap();
        let to = to_dir.join(from_name);
        if from.is_dir() {
            directories.push((from, to));
            continue;
        }
        fs::copy(&from, &to)?;
    }
    Ok(directories)
}
