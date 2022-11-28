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
    let mut args = CommandLineArgs::parse();
    let mut globals = Globals::new(args.warning, args.no_jit);
    globals.lib_directories.append(&mut args.import);
    globals.exec_startup();

    if !args.exec.is_empty() {
        let path = std::path::Path::new("REPL");
        for code in args.exec {
            let res = globals.compile_and_run(&code, path);
            if let Ok(_val) = res {
                #[cfg(debug_assertions)]
                eprintln!("=> {:?}", _val)
            };
        }
        return;
    }

    let mut executor = Executor::default();
    let (code, path) = match args.file {
        Some(file_name) => {
            let path = std::path::PathBuf::from(&file_name);
            let mut file = File::open(file_name.clone()).unwrap();
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
                Err(err) => err.show_error_message_and_loc(&globals),
            };
        }
        Err(err) => {
            err.show_error_message_and_loc(&globals);
        }
    };
}
