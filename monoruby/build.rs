use dirs;
use std::fs;
use std::process::Command;

fn main() {
    let lib_path = dirs::home_dir().unwrap().join(".monoruby");
    println!("lib_path: {:?}", lib_path);
    match lib_path.try_exists() {
        Ok(true) => {}
        _ => fs::create_dir(&lib_path).unwrap(),
    }

    match Command::new("ruby").args(["-e", "p($:)"]).output() {
        Ok(output) => {
            let dest_path = lib_path.clone().join("library_path");
            let load_path = std::str::from_utf8(&output.stdout).unwrap();
            fs::write(dest_path, load_path).unwrap();
        }
        Err(_) => {
            println!("failed to read ruby library path");
        }
    }

    match Command::new("gem").args(["environment", "paths"]).output() {
        Ok(output) => {
            let dest_path = lib_path.clone().join("gem_path");
            let load_path = std::str::from_utf8(&output.stdout)
                .unwrap()
                .split(':')
                .map(|s| format!(r#""{}""#, s))
                .collect::<Vec<_>>()
                .join(",");
            fs::write(dest_path, &format!("[{}]", load_path)).unwrap();
        }
        Err(_) => {
            println!("failed to read ruby gem path");
        }
    };

    for entry in fs::read_dir("startup").unwrap() {
        let path = entry.unwrap().path();
        let file_name = path.file_name().unwrap();
        fs::copy(&path, lib_path.join(file_name)).unwrap();
    }

    //println!("cargo:rerun-if-changed=startup");
}
