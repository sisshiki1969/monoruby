use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

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

    let mut rl = DefaultEditor::new().unwrap();
    let mut globals = Globals::new(args.warning, args.no_jit);

    let mut cont_mode = false;
    let mut buf = String::new();
    let mut script_line = 0;
    let mut executor = Executor::init(&mut globals);

    let parse_result = ruruby_parse::Parser::parse_program(
        String::new(),
        std::path::Path::new(&format!("(irm):{script_line}")),
    )
    .unwrap();
    let dummy_fid = monoruby::compile_script(&mut globals, parse_result).unwrap();
    let meta = globals[dummy_fid].meta();
    let dummy_outer = Lfp::heap_frame(globals.main_object, meta);

    let binding = Binding::from_outer(dummy_outer);
    executor.temp_push(binding.as_val());
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
                    format!("{buf}\n{code}")
                } else {
                    code.clone()
                };

                let fid = match globals.compile_script_binding(
                    buf.clone(),
                    std::path::Path::new(&format!("(irm):{script_line}")),
                    binding,
                ) {
                    Ok(fid) => fid,
                    Err(err) => {
                        if err.is_unexpected_eof() {
                            rl.add_history_entry(code.as_str()).unwrap();
                            cont_mode = true;
                        } else {
                            err.show_error_message_and_all_loc(&globals);
                            cont_mode = false;
                        }
                        continue;
                    }
                };
                rl.add_history_entry(code.as_str()).unwrap();
                cont_mode = false;
                let binding_lfp = globals.new_binding_frame(fid, globals.main_object, binding);
                match executor.invoke_binding(&mut globals, binding_lfp) {
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
                println!("Error: {err:?}");
                break;
            }
        }
    }
}
