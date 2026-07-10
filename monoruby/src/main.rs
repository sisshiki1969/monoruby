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
    /// dump the parsed ruby-prism AST and exit.
    #[arg(long, value_enum, num_args = 0..=1, default_missing_value = "prism", value_name = "PARSER")]
    ast: Option<ParserKind>,
    /// require the library before executing your script.
    #[arg(short = 'r', value_name = "library")]
    require: Vec<String>,
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

fn dump_ast(code: &str, _path: &std::path::Path, kind: ParserKind, _globals: &Globals) {
    match kind {
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

/// Phase-0 measurement: print the heap-frame leak tally at process
/// exit when `MONORUBY_FRAME_STATS` is set. Drop runs on normal
/// `main` return (every exit path here returns rather than aborting).
struct FrameStatsGuard;
impl Drop for FrameStatsGuard {
    fn drop(&mut self) {
        if std::env::var_os("MONORUBY_FRAME_STATS").is_some() {
            let (n, bytes) = monoruby::frame_leak_stats();
            eprintln!("[frame-leak] promotions={n} leaked_bytes={bytes}");
        }
    }
}

fn main() {
    use clap::Parser;
    install_panic_hook();
    let _frame_stats = FrameStatsGuard;
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
            // Multiple `-e` options form a single program (joined by
            // newlines), matching CRuby, so `-r` libraries and `at_exit`
            // handlers are processed once around the combined script.
            let code = args.exec.join("\n");
            match globals.run_with_requires(&args.require, code, path) {
                Ok(_val) => {
                    #[cfg(debug_assertions)]
                    eprintln!("=> {:?}", _val)
                }
                Err(err) => {
                    handle_error(err, &globals);
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
        match read_source_file(&std::path::PathBuf::from(&file_name)) {
            // Keep the path exactly as given on the command line: CRuby
            // reports `$0` / `__FILE__` for the main script verbatim
            // (relative stays relative), not canonicalized.
            Ok((code, _resolved)) => (code, std::path::PathBuf::from(&file_name)),
            Err(err) => {
                // No VM frame exists yet, so a `MonorubyErr` here would
                // carry no trace and cannot be displayed as a normal
                // exception. Print CRuby's one-liner instead
                // (`ruby: No such file or directory -- t.rb (LoadError)`).
                // `io::Error`'s Display appends ` (os error N)`, which
                // CRuby's strerror-based message doesn't have — strip it.
                let msg = err.to_string();
                let msg = msg.split(" (os error ").next().unwrap();
                eprintln!("monoruby: {msg} -- {file_name} (LoadError)");
                std::process::exit(1);
            }
        }
    } else {
        if finish_flag {
            return;
        }
        // Read stdin as raw bytes: a script piped in may contain
        // non-UTF-8 bytes (e.g. a `# encoding:`-tagged source with
        // high bytes in a string literal). monoruby stores source as
        // UTF-8 and cannot round-trip arbitrary bytes, but it must not
        // abort — decode lossily rather than `unwrap`-ing.
        let mut bytes = Vec::new();
        std::io::stdin()
            .read_to_end(&mut bytes)
            .expect("failed to read stdin");
        let code = String::from_utf8_lossy(&bytes).into_owned();
        (code, std::path::PathBuf::from("-"))
    };
    if let Some(kind) = args.ast {
        dump_ast(&code, &path, kind, &globals);
    } else if let Err(err) = globals.run_with_requires(&args.require, code, &path) {
        handle_error(err, &globals);
    }
}
