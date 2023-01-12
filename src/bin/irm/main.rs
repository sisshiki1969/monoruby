use rustyline::error::ReadlineError;
use rustyline::Editor;

use monoruby::*;

#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None, trailing_var_arg = true)]
struct CommandLineArgs {
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

    let mut rl = Editor::<()>::new().unwrap();
    let mut globals = Globals::new(args.warning, args.no_jit);

    globals.exec_startup();

    let mut cont_mode = false;
    let mut buf = String::new();
    let mut script_line = 0;
    let mut context = None;
    let mut interp = Executor::default();
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
                    std::path::Path::new(&format!("irm:{:03}", script_line)),
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
