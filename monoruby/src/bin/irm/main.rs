use ruruby_parse::ParseErrKind;
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
    let mut binding = None;
    let mut binding_lfp = None;
    let mut executor = Executor::init(&mut globals);
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

                let main_fid = match ruruby_parse::Parser::parse_program_binding(
                    buf.clone(),
                    std::path::Path::new(&format!("(irm):{script_line}")).into(),
                    binding.clone(),
                    None::<&ruruby_parse::DummyContext>,
                ) {
                    Ok(res) => {
                        let collector = res.lvar_collector;
                        let fid = match monoruby::compile_script(
                            &mut globals,
                            res.node,
                            res.source_info,
                            std::mem::take(&mut binding),
                        ) {
                            Ok(id) => id,
                            Err(err) => {
                                err.show_error_message_and_all_loc(&globals);
                                cont_mode = false;
                                continue;
                            }
                        };
                        binding = Some(collector);
                        fid
                    }
                    Err(err) => {
                        if err.kind == ParseErrKind::UnexpectedEOF {
                            rl.add_history_entry(code.as_str()).unwrap();
                            cont_mode = true;
                        } else {
                            let err = MonorubyErr::parse(err);
                            err.show_error_message_and_all_loc(&globals);
                            cont_mode = false;
                        }
                        continue;
                    }
                };
                rl.add_history_entry(code.as_str()).unwrap();
                cont_mode = false;
                binding_lfp =
                    Some(globals.new_heap_frame(main_fid, globals.main_object, binding_lfp));
                match executor.eval_binding(&mut globals, binding_lfp.unwrap()) {
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
