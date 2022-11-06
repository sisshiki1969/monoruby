use std::fs::File;
use std::io::Read;

use rustyline::error::ReadlineError;
use rustyline::Editor;

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
            let mut rl = Editor::<()>::new().unwrap();
            let mut globals = Globals::new(args.warning, args.no_jit);

            globals.exec_startup();

            let mut cont_mode = false;
            let mut buf = String::new();
            let mut script_line = 0;
            let mut context = None;
            let mut interp = Executor::new();
            loop {
                let prompt = format!(
                    "monoruby:{:03}{} ",
                    script_line,
                    if cont_mode { "*" } else { ">" }
                );
                let readline = rl.readline(&prompt);
                match readline {
                    Ok(code) => {
                        buf = if cont_mode {
                            format!("{}\n{}", buf, code)
                        } else {
                            code.clone()
                        };
                        let main_fid = match globals.compile_script_with_binding(
                            buf.clone(),
                            std::path::Path::new(&format!("REPL:{:03}", script_line)),
                            context.clone(),
                        ) {
                            Ok((fid, collector)) => {
                                context = Some(collector);
                                fid
                            }
                            Err(err) => {
                                if err.is_eof() {
                                    rl.add_history_entry(code.as_str());
                                    cont_mode = true;
                                } else {
                                    err.show_error_message_and_all_loc(&globals);
                                    cont_mode = false;
                                };
                                continue;
                            }
                        };
                        rl.add_history_entry(code.as_str());
                        cont_mode = false;
                        match interp.eval(&mut globals, main_fid) {
                            Ok(val) => eprintln!("=> {}", val.inspect(&globals)),
                            Err(err) => err.show_error_message_and_all_loc(&globals),
                        };
                        script_line += 1;
                    }
                    Err(ReadlineError::Interrupted) => {
                        // Ctrl-C
                        cont_mode = false;
                    }
                    Err(ReadlineError::Eof) => {
                        // Ctrl-D
                        break;
                    }
                    Err(err) => {
                        println!("Error: {:?}", err);
                        break;
                    }
                }
            }
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
