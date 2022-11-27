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

    if !args.exec.is_empty() {
        for code in args.exec {
            exec(
                &code,
                args.no_jit,
                args.warning,
                std::path::Path::new("REPL"),
            );
        }
        return;
    }

    match args.file {
        Some(file_name) => {
            let mut file = File::open(file_name.clone()).unwrap();
            let mut code = String::new();
            file.read_to_string(&mut code).unwrap();
            exec(
                &code,
                args.no_jit,
                args.warning,
                std::path::Path::new(&file_name),
            );
        }
        None => {
            let mut globals = Globals::new(args.warning, args.no_jit);
            let mut interp = Executor::default();
            let path = std::path::Path::new("-");
            globals.exec_startup();
            let mut stdin = std::io::stdin();
            let mut code = String::new();
            stdin.read_to_string(&mut code).unwrap();
            match globals.compile_script(code, path) {
                Ok(fid) => {
                    match interp.eval(&mut globals, fid) {
                        Ok(val) => eprintln!("=> {}", val.inspect(&globals)),
                        Err(err) => err.show_error_message_and_loc(&globals),
                    };
                }
                Err(err) => {
                    err.show_error_message_and_loc(&globals);
                }
            };
        }
    }
}

fn exec(code: &str, no_jit_flag: bool, warning: u8, path: &std::path::Path) {
    let mut globals = Globals::new(warning, no_jit_flag);
    globals.exec_startup();
    let res = compile_and_run(&mut globals, code, path);
    if let Ok(_val) = res {
        #[cfg(debug_assertions)]
        eprintln!("=> {:?}", _val)
    };
}
