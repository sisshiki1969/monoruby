use std::fs::File;
use std::io::Read;

use monoruby::*;

#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None, trailing_var_arg = true)]
struct CommandLineArgs {
    /// one liner. several -e's allowed. Omit [programfile]
    #[arg(short, num_args = 0..)]
    exec: Vec<String>,
    /// print the version number, then turn on verbose mode
    #[arg(short)]
    verbose: bool,
    /// switch just-in-time compilation.
    #[arg(short, long)]
    no_jit: bool,
    #[arg(short = 'I')]
    import: Vec<String>,
    #[arg(short = 'W', default_value = "1")]
    warning: u8,
    /// File name.
    #[arg(num_args = 0..)]
    file: Option<String>,
}

fn main() {
    use clap::Parser;
    let args = CommandLineArgs::parse();
    let mut globals = Globals::new(args.warning, args.no_jit);
    let mut lib = args
        .import
        .iter()
        .filter_map(|s| {
            std::path::Path::new(s)
                .canonicalize()
                .map(|p| p.to_string_lossy().to_string())
                .ok()
        })
        .collect();
    globals.lib_directories.append(&mut lib);
    globals.exec_startup();

    if !args.exec.is_empty() {
        let path = std::path::Path::new("irm");
        for code in args.exec {
            match globals.compile_and_run(&code, path) {
                Ok(_val) => {
                    #[cfg(debug_assertions)]
                    eprintln!("=> {:?}", _val)
                }
                Err(err) => err.show_error_message_and_loc(),
            }
        }
        return;
    }

    let mut executor = Executor::default();
    let (code, path) = match args.file {
        Some(file_name) => {
            let path = std::path::PathBuf::from(&file_name);
            let mut file = File::open(file_name).unwrap();
            let mut code = String::new();
            file.read_to_string(&mut code).unwrap();
            (code, path)
        }
        None => {
            let path = std::path::PathBuf::from("-");
            let mut stdin = std::io::stdin();
            let mut code = String::new();
            stdin.read_to_string(&mut code).unwrap();
            (code, path)
        }
    };
    match globals.compile_script(code, path) {
        Ok(fid) => {
            match executor.eval(&mut globals, fid) {
                Ok(_val) => {}
                Err(err) => err.show_error_message_and_loc(),
            };
        }
        Err(err) => {
            err.show_error_message_and_loc();
        }
    };
}
