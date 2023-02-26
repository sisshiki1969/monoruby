#![feature(result_flattening)]

use std::fs::File;
use std::io::Read;

use monoruby::*;

#[derive(clap::Parser, Debug)]
#[command(author, about, long_about = None, trailing_var_arg = true)]
struct CommandLineArgs {
    /// one line of script. several -e's allowed. Omit [programfile]
    #[arg(short, num_args = 0..)]
    exec: Vec<String>,
    /// print the version number, then exit
    #[arg(short, long)]
    version: bool,
    /// switch just-in-time compilation.
    #[arg(long)]
    no_jit: bool,
    /// specify $LOAD_PATH directory (may be used more than once).
    #[arg(short = 'I')]
    directory: Vec<String>,
    /// set warning level; 0=silence, 1=medium, 2=verbose.
    #[arg(short = 'W', default_value = "1")]
    warning: u8,
    /// File name.
    #[arg(num_args = 0..)]
    file: Option<String>,
}

fn main() {
    use clap::Parser;
    let mut finish_flag = false;
    let args = CommandLineArgs::parse();
    let mut globals = Globals::new(args.warning, args.no_jit);
    let mut lib = args
        .directory
        .iter()
        .filter_map(|s| {
            std::path::Path::new(s)
                .canonicalize()
                .map(|p| p.to_string_lossy().to_string())
                .ok()
        })
        .collect();
    globals.lib_directories.append(&mut lib);

    if args.version {
        println!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
        finish_flag = true;
    }

    if !args.exec.is_empty() {
        let path = std::path::Path::new("irm");
        for code in args.exec {
            match globals.compile_and_run(&code, path) {
                Ok(_val) => {
                    #[cfg(debug_assertions)]
                    eprintln!("=> {:?}", _val)
                }
                Err(err) => {
                    err.show_error_message_and_loc();
                    std::process::exit(1);
                }
            }
        }
        return;
    }

    let mut code = String::new();
    let path = match args.file {
        Some(file_name) => {
            let path = std::path::PathBuf::from(&file_name).canonicalize().unwrap();
            let mut file = File::open(file_name).unwrap();
            file.read_to_string(&mut code).unwrap();
            path
        }
        None => {
            if finish_flag {
                return;
            }
            std::io::stdin().read_to_string(&mut code).unwrap();
            std::path::PathBuf::from("-")
        }
    };
    if let Err(err) = globals.compile_and_run(&code, &path) {
        err.show_error_message_and_loc();
        std::process::exit(1);
    };
}
