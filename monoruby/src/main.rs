use std::io::Read;

use monoruby::*;

fn handle_error(err: MonorubyErr, globals: &Globals) -> ! {
    if let MonorubyErrKind::SystemExit(status) = err.kind {
        std::process::exit(status as i32);
    }
    if let MonorubyErrKind::MethodReturn(..) = err.kind {
        // MethodReturn that reached top-level means a return from a block
        // whose target method has already returned — this is a LocalJumpError.
        let err = MonorubyErr::localjumperr("unexpected return");
        err.show_error_message_and_all_loc(&globals.store);
        std::process::exit(1);
    }
    err.show_error_message_and_all_loc(&globals.store);
    std::process::exit(1);
}

#[derive(clap::ValueEnum, Clone, Copy, Debug, PartialEq, Eq)]
enum ParserKind {
    Ruruby,
    Prism,
}

#[derive(clap::Parser, Debug)]
#[command(author, about, long_about = None)]
struct CommandLineArgs {
    /// one line of script. several -e's allowed. Omit [programfile]
    #[arg(short, num_args = 0..)]
    exec: Vec<String>,
    /// print the version number, then exit
    #[arg(short, long)]
    version: bool,
    /// switch for just-in-time compilation.
    #[arg(long)]
    no_jit: bool,
    /// switch for loading gems.
    #[arg(long)]
    disable_gems: bool,
    /// switch for garbage collection.
    #[arg(long)]
    no_gc: bool,
    /// dump parsed AST and exit. Defaults to `ruruby`; pass `prism`
    /// to dump the equivalent ruby-prism AST.
    #[arg(long, value_enum, num_args = 0..=1, default_missing_value = "ruruby", value_name = "PARSER")]
    ast: Option<ParserKind>,
    /// specify $LOAD_PATH directory (may be used more than once).
    #[arg(short = 'I')]
    directory: Vec<String>,
    /// set warning level; 0=silence, 1=medium, 2=verbose.
    #[arg(short = 'W', default_value = "1")]
    warning: u8,
    /// File name.
    #[arg(num_args = 0.., trailing_var_arg = true)]
    file: Vec<String>,
}

fn dump_ast(code: &str, path: &std::path::Path, kind: ParserKind, globals: &Globals) {
    match kind {
        ParserKind::Ruruby => match ruruby_parse::Parser::parse_program(code.to_string(), path) {
            Ok(res) => eprintln!("{:#?}", res.node),
            Err(err) => handle_error(MonorubyErr::parse(err), globals),
        },
        ParserKind::Prism => {
            let result = ruby_prism::parse(code.as_bytes());
            for diag in result.errors() {
                eprintln!(
                    "prism error at {}..{}: {}",
                    diag.location().start_offset(),
                    diag.location().end_offset(),
                    diag.message(),
                );
            }
            eprintln!("{:#?}", result.node());
        }
    }
}

fn main() {
    use clap::Parser;
    let mut finish_flag = false;
    let args = CommandLineArgs::parse();
    let mut globals = Globals::new(args.warning, args.no_jit, args.disable_gems);
    Globals::gc_enable(!args.no_gc);
    let lib = args.directory.iter().map(|s| {
        std::path::Path::new(s)
            .canonicalize()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_else(|_| s.clone())
    });
    globals.extend_load_path(lib);

    if args.version {
        println!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
        finish_flag = true;
    }

    if !args.exec.is_empty() {
        let path = std::path::Path::new("-e");
        let argv = Value::array_from_iter(args.file.iter().cloned().map(Value::string));
        globals.set_constant_by_str(OBJECT_CLASS, "ARGV", argv);
        globals.set_gvar(monoruby::IdentId::get_id("$*"), argv);
        if let Some(kind) = args.ast {
            for code in args.exec {
                dump_ast(&code, path, kind, &globals);
            }
        } else {
            for code in args.exec {
                match globals.run(code, path) {
                    Ok(_val) => {
                        #[cfg(debug_assertions)]
                        eprintln!("=> {:?}", _val)
                    }
                    Err(err) => {
                        handle_error(err, &globals);
                    }
                }
            }
        }
        return;
    }

    let mut iter = args.file.into_iter();
    let first = iter.next();
    let argv = Value::array_from_iter(iter.map(Value::string));
    globals.set_constant_by_str(OBJECT_CLASS, "ARGV", argv);
    globals.set_gvar(monoruby::IdentId::get_id("$*"), argv);
    let (code, path) = if let Some(file_name) = first {
        match load_file(&std::path::PathBuf::from(&file_name)) {
            Ok(res) => res,
            Err(err) => {
                handle_error(err, &globals);
            }
        }
    } else {
        if finish_flag {
            return;
        }
        let mut code = String::new();
        std::io::stdin().read_to_string(&mut code).unwrap();
        (code, std::path::PathBuf::from("-"))
    };
    if let Some(kind) = args.ast {
        dump_ast(&code, &path, kind, &globals);
    } else if let Err(err) = globals.run(code, &path) {
        handle_error(err, &globals);
    }
}
