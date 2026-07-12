use std::ffi::{OsStr, OsString};
use std::io::Read;
use std::path::{Path, PathBuf};

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

/// Raw bytes of an OS-native CLI argument (Unix: no conversion).
fn os_bytes(s: &OsStr) -> &[u8] {
    std::os::unix::ffi::OsStrExt::as_bytes(s)
}

fn os_from_bytes(b: &[u8]) -> OsString {
    <OsStr as std::os::unix::ffi::OsStrExt>::from_bytes(b).to_os_string()
}

fn dump_ast(code: &[u8]) {
    let result = ruby_prism::parse(code);
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

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
enum LoopMode {
    #[default]
    None,
    /// `-n`: wrap the script in `while gets ... end`.
    Gets,
    /// `-p`: same as `-n`, plus `print $_` at the end of each iteration.
    GetsPrint,
}

/// Parsed command-line switches. Filled once from the actual command
/// line and once more from `RUBYOPT` (which permits only a subset of
/// switches); the two are merged with command-line priority.
#[derive(Default, Debug)]
struct Opts {
    /// `-e` one-liners, joined by newlines into a single program.
    exec: Vec<OsString>,
    /// `-r` libraries.
    requires: Vec<String>,
    /// `-I` load-path directories.
    includes: Vec<String>,
    /// Explicit `-W<level>` (0/1/2). `None` when only `-w`/`-v` given.
    warning_level: Option<u8>,
    /// `-W:<category>` / `-W:no-<category>` toggles, in order.
    warn_categories: Vec<(String, bool)>,
    /// `-w` / `-v` / `--verbose`.
    verbose: bool,
    /// `-v`: print the version banner (and exit if no program).
    print_version: bool,
    /// `--version`: print the version banner and exit.
    version_only: bool,
    /// `--copyright`.
    copyright: bool,
    /// `-h` / `--help`.
    help: bool,
    /// `-d` / `--debug`.
    debug: bool,
    /// `-c`: syntax check only.
    syntax_check: bool,
    /// `-C dir` / `-X dir`, applied in order.
    chdir: Vec<String>,
    /// `-s`: parse `-switch[=value]` args after the script name into globals.
    dash_s: bool,
    loop_mode: LoopMode,
    /// `-a`.
    auto_split: bool,
    /// `-l`.
    line_ending: bool,
    /// `-0[octal]`: the raw octal digits ("" for a bare `-0`).
    record_sep: Option<String>,
    /// `-F pattern`.
    field_sep: Option<String>,
    /// `-x[dir]`.
    dash_x: bool,
    x_dir: Option<String>,
    /// `-S`: search RUBYPATH / PATH for the script.
    search_path: bool,
    /// `-E ext[:int]` / `--encoding` / `--external-encoding`.
    external_enc: Option<String>,
    /// `-E ext:int` / `-E :int` / `--internal-encoding`.
    internal_enc: Option<String>,
    /// `-U`.
    set_internal_utf8: bool,
    /// `-K<code>`.
    kcode: Option<char>,
    /// `--backtrace-limit=N`.
    backtrace_limit: Option<i64>,
    /// `--debug-frozen-string-literal` (also implied by `--debug`/`-d`).
    debug_frozen_string_literal: bool,
    /// `--enable[-=]FEATURE` / `--disable[-=]FEATURE`, in order.
    features: Vec<(String, bool)>,
    /// monoruby-specific switches.
    no_jit: bool,
    no_gc: bool,
    ast: bool,
    /// Script name + program arguments (everything after the switches).
    args: Vec<OsString>,
}

enum ParseFail {
    /// Unknown or unsupported switch.
    InvalidOption(String),
    /// A switch that requires an argument was given none.
    MissingArg(String),
    /// A switch that is not permitted in RUBYOPT.
    Forbidden(String),
    /// Pre-formatted whole message.
    Message(String),
}

impl ParseFail {
    fn switch(&self) -> &str {
        match self {
            ParseFail::InvalidOption(s)
            | ParseFail::MissingArg(s)
            | ParseFail::Forbidden(s)
            | ParseFail::Message(s) => s,
        }
    }
}

/// Take an option's value: the rest of the current token if non-empty,
/// otherwise the next token.
fn take_value(
    bytes: &[u8],
    idx: usize,
    tokens: &[OsString],
    i: &mut usize,
    switch: &str,
) -> Result<OsString, ParseFail> {
    if idx < bytes.len() {
        Ok(os_from_bytes(&bytes[idx..]))
    } else if *i < tokens.len() {
        let v = tokens[*i].clone();
        *i += 1;
        Ok(v)
    } else {
        Err(ParseFail::MissingArg(switch.to_string()))
    }
}

fn lossy(s: &OsStr) -> String {
    s.to_string_lossy().into_owned()
}

/// Short switches permitted inside RUBYOPT (CRuby's `moreswitches` set).
const RUBYOPT_SHORT: &str = "dEIKrTUvwW";

fn parse_switches(tokens: &[OsString], opts: &mut Opts, rubyopt: bool) -> Result<(), ParseFail> {
    let mut i = 0;
    while i < tokens.len() {
        let tok = tokens[i].clone();
        let bytes = os_bytes(&tok);
        if bytes == b"--" {
            if rubyopt {
                return Err(ParseFail::Forbidden("--".to_string()));
            }
            opts.args.extend(tokens[i + 1..].iter().cloned());
            return Ok(());
        }
        if bytes.starts_with(b"--") {
            i += 1;
            parse_long(&lossy(&tok), tokens, &mut i, opts, rubyopt)?;
            continue;
        }
        if bytes.len() >= 2 && bytes[0] == b'-' {
            i += 1;
            parse_short_cluster(bytes, tokens, &mut i, opts, rubyopt)?;
            continue;
        }
        // Not a switch: the script name; everything after it is ARGV.
        if rubyopt {
            return Err(ParseFail::Forbidden(lossy(&tok)));
        }
        opts.args.extend(tokens[i..].iter().cloned());
        return Ok(());
    }
    Ok(())
}

fn parse_long(
    s: &str,
    tokens: &[OsString],
    i: &mut usize,
    opts: &mut Opts,
    rubyopt: bool,
) -> Result<(), ParseFail> {
    let (name, mut value) = match s.split_once('=') {
        Some((n, v)) => (n.to_string(), Some(v.to_string())),
        None => (s.to_string(), None),
    };
    let mut next_value = |value: Option<String>| -> Result<String, ParseFail> {
        if let Some(v) = value {
            Ok(v)
        } else if *i < tokens.len() {
            let v = lossy(&tokens[*i]);
            *i += 1;
            Ok(v)
        } else {
            Err(ParseFail::MissingArg(name.clone()))
        }
    };
    // --enable-FEATURE / --disable-FEATURE (also --enable=F / --disable=F,
    // comma-separated lists allowed). This arm also covers the historic
    // monoruby spelling `--disable-gems`.
    for (prefix, on) in [("--enable", true), ("--disable", false)] {
        if name == prefix || name.starts_with(&format!("{prefix}-")) {
            let list = if name == prefix {
                next_value(value.take())?
            } else {
                name[prefix.len() + 1..].to_string()
            };
            for feat in list.split(',') {
                opts.features.push((feat.to_string(), on));
            }
            return Ok(());
        }
    }
    if rubyopt
        && !matches!(
            name.as_str(),
            "--debug"
                | "--debug-frozen-string-literal"
                | "--verbose"
                | "--encoding"
                | "--external-encoding"
                | "--internal-encoding"
        )
    {
        return Err(ParseFail::Forbidden(name));
    }
    match name.as_str() {
        "--version" => opts.version_only = true,
        "--copyright" => opts.copyright = true,
        "--help" => opts.help = true,
        "--verbose" => opts.verbose = true,
        "--debug" => opts.debug = true,
        "--debug-frozen-string-literal" => opts.debug_frozen_string_literal = true,
        "--no-jit" => opts.no_jit = true,
        "--no-gc" => opts.no_gc = true,
        "--ast" => {
            // Optional value (historically `--ast=prism`); ignore it.
            opts.ast = true;
        }
        "--encoding" => {
            let v = next_value(value)?;
            parse_encoding_spec(&v, opts)?;
        }
        "--external-encoding" => opts.external_enc = Some(next_value(value)?),
        "--internal-encoding" => opts.internal_enc = Some(next_value(value)?),
        "--backtrace-limit" => {
            let v = next_value(value)?;
            opts.backtrace_limit = v.parse::<i64>().ok();
        }
        _ => return Err(ParseFail::InvalidOption(name)),
    }
    Ok(())
}

/// `ext[:int]` for `-E` / `--encoding`. A third component is an error.
fn parse_encoding_spec(spec: &str, opts: &mut Opts) -> Result<(), ParseFail> {
    let parts: Vec<&str> = spec.split(':').collect();
    if parts.len() > 2 {
        return Err(ParseFail::Message(format!(
            "monoruby: extra argument for --encoding: {} (RuntimeError)",
            parts[2]
        )));
    }
    if let Some(ext) = parts.first()
        && !ext.is_empty()
    {
        opts.external_enc = Some(ext.to_string());
    }
    if let Some(int) = parts.get(1)
        && !int.is_empty()
    {
        opts.internal_enc = Some(int.to_string());
    }
    Ok(())
}

fn parse_short_cluster(
    bytes: &[u8],
    tokens: &[OsString],
    i: &mut usize,
    opts: &mut Opts,
    rubyopt: bool,
) -> Result<(), ParseFail> {
    let mut idx = 1;
    while idx < bytes.len() {
        let c = bytes[idx] as char;
        idx += 1;
        if rubyopt && !RUBYOPT_SHORT.contains(c) {
            return Err(ParseFail::Forbidden(format!("-{c}")));
        }
        match c {
            'e' => {
                let v = take_value(bytes, idx, tokens, i, "-e")?;
                opts.exec.push(v);
                return Ok(());
            }
            'r' => {
                let v = take_value(bytes, idx, tokens, i, "-r")?;
                opts.requires.push(lossy(&v));
                return Ok(());
            }
            'I' => {
                let v = take_value(bytes, idx, tokens, i, "-I")?;
                opts.includes.push(lossy(&v));
                return Ok(());
            }
            'C' | 'X' => {
                let v = take_value(bytes, idx, tokens, i, "-C")?;
                opts.chdir.push(lossy(&v));
                return Ok(());
            }
            'F' => {
                let v = take_value(bytes, idx, tokens, i, "-F")?;
                opts.field_sep = Some(lossy(&v));
                return Ok(());
            }
            'E' => {
                let v = take_value(bytes, idx, tokens, i, "-E")?;
                parse_encoding_spec(&lossy(&v), opts)?;
                return Ok(());
            }
            'K' => {
                if idx < bytes.len() {
                    opts.kcode = Some(bytes[idx] as char);
                    idx += 1;
                }
            }
            'W' => {
                if idx < bytes.len() && bytes[idx] == b':' {
                    let cat = String::from_utf8_lossy(&bytes[idx + 1..]).into_owned();
                    let (name, on) = match cat.strip_prefix("no-") {
                        Some(rest) => (rest.to_string(), false),
                        None => (cat, true),
                    };
                    opts.warn_categories.push((name, on));
                    return Ok(());
                }
                if idx < bytes.len() && bytes[idx].is_ascii_digit() {
                    opts.warning_level = Some((bytes[idx] - b'0').min(2));
                    idx += 1;
                } else {
                    opts.warning_level = Some(2);
                }
            }
            '0' => {
                let start = idx;
                while idx < bytes.len() && (b'0'..=b'7').contains(&bytes[idx]) {
                    idx += 1;
                }
                opts.record_sep = Some(String::from_utf8_lossy(&bytes[start..idx]).into_owned());
            }
            'x' => {
                opts.dash_x = true;
                if idx < bytes.len() {
                    opts.x_dir = Some(String::from_utf8_lossy(&bytes[idx..]).into_owned());
                }
                return Ok(());
            }
            'T' => {
                // Historic taint level: swallow the digits, ignore.
                while idx < bytes.len() && bytes[idx].is_ascii_digit() {
                    idx += 1;
                }
            }
            'v' => {
                opts.print_version = true;
                opts.verbose = true;
            }
            'w' => opts.verbose = true,
            'd' => opts.debug = true,
            'c' => opts.syntax_check = true,
            'a' => opts.auto_split = true,
            'l' => opts.line_ending = true,
            'n' => {
                if opts.loop_mode == LoopMode::None {
                    opts.loop_mode = LoopMode::Gets;
                }
            }
            'p' => opts.loop_mode = LoopMode::GetsPrint,
            's' => opts.dash_s = true,
            'S' => opts.search_path = true,
            'U' => opts.set_internal_utf8 = true,
            'h' => opts.help = true,
            _ => return Err(ParseFail::InvalidOption(format!("-{c}"))),
        }
    }
    Ok(())
}

fn fail_cli(f: ParseFail) -> ! {
    match f {
        ParseFail::Message(msg) => eprintln!("{msg}"),
        ParseFail::MissingArg(sw) => {
            eprintln!("monoruby: missing argument for {sw} (RuntimeError)")
        }
        ParseFail::InvalidOption(sw) | ParseFail::Forbidden(sw) => {
            eprintln!("monoruby: invalid option {sw}  (-h will show valid options) (RuntimeError)")
        }
    }
    std::process::exit(1);
}

fn fail_rubyopt(f: ParseFail) -> ! {
    eprintln!(
        "monoruby: invalid switch in RUBYOPT: {} (RuntimeError)",
        f.switch()
    );
    std::process::exit(1);
}

/// `File.expand_path` semantics minus `~`: absolutize against the CWD
/// and remove `.` / `..` segments lexically (never resolving symlinks).
fn expand_path(p: &str) -> String {
    let path = Path::new(p);
    let abs = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()
            .unwrap_or_else(|_| PathBuf::from("/"))
            .join(path)
    };
    let mut out = PathBuf::new();
    for comp in abs.components() {
        use std::path::Component::*;
        match comp {
            CurDir => {}
            ParentDir => {
                out.pop();
            }
            c => out.push(c.as_os_str()),
        }
    }
    out.to_string_lossy().into_owned()
}

/// `-S`: look the script up in RUBYPATH, then PATH.
fn search_script_in_path(name: &str) -> Option<PathBuf> {
    for var in ["RUBYPATH", "PATH"] {
        if let Ok(dirs) = std::env::var(var) {
            for dir in dirs.split(':') {
                if dir.is_empty() {
                    continue;
                }
                let cand = Path::new(dir).join(name);
                if cand.is_file() {
                    return Some(cand);
                }
            }
        }
    }
    None
}

/// `-x` (and the automatic variant triggered by a non-ruby `#!` first
/// line): strip everything before the first `#!`-line that mentions
/// `ruby`, replacing it with blank lines so line numbers stay stable.
/// With `forced` set, not finding such a line is an error.
fn preprocess_shebang(code: Vec<u8>, forced: bool) -> Result<Vec<u8>, String> {
    fn contains(hay: &[u8], needle: &[u8]) -> bool {
        hay.windows(needle.len()).any(|w| w == needle)
    }
    if !forced {
        let first_line = code.split(|&b| b == b'\n').next().unwrap_or(&[]);
        if !(first_line.starts_with(b"#!") && !contains(first_line, b"ruby")) {
            return Ok(code);
        }
    }
    let mut offset = 0;
    let mut lineno = 0;
    while offset < code.len() {
        let end = code[offset..]
            .iter()
            .position(|&b| b == b'\n')
            .map(|p| offset + p + 1)
            .unwrap_or(code.len());
        let line = &code[offset..end];
        if line.starts_with(b"#!") && contains(line, b"ruby") {
            let mut new_code = vec![b'\n'; lineno + 1];
            new_code.extend_from_slice(&code[end..]);
            return Ok(new_code);
        }
        offset = end;
        lineno += 1;
    }
    if forced {
        Err("no Ruby script found in input".to_string())
    } else {
        Ok(code)
    }
}

/// Emit a Ruby double-quoted string literal evaluating to `s`'s bytes.
fn ruby_str_lit(s: &str) -> String {
    let mut out = String::from("\"");
    for &b in s.as_bytes() {
        match b {
            b'"' => out.push_str("\\\""),
            b'\\' => out.push_str("\\\\"),
            b'#' => out.push_str("\\#"),
            0x20..=0x7e => out.push(b as char),
            _ => out.push_str(&format!("\\x{b:02X}")),
        }
    }
    out.push('"');
    out
}

fn print_usage() {
    println!(
        "Usage: monoruby [switches] [--] [programfile] [arguments]
  -0[octal]       specify record separator (\\0, if no argument)
  -a              autosplit mode with -n or -p (splits $_ into $F)
  -c              check syntax only
  -Cdirectory     cd to directory before executing your script
  -d, --debug     set debugging flags (set $DEBUG to true)
  -e 'command'    one line of script. Several -e's allowed. Omit [programfile]
  -Eex[:in], --encoding=ex[:in]
                  specify the default external and internal character encodings
  -Fpattern       split() pattern for autosplit (-a)
  -Idirectory     specify $LOAD_PATH directory (may be used more than once)
  -l              enable line ending processing
  -n              assume 'while gets(); ... end' loop around your script
  -p              assume loop like -n but print line also like sed
  -rlibrary       require the library before executing your script
  -s              enable some switch parsing for switches after script name
  -S              look for the script using PATH environment variable
  -U              set the internal encoding to UTF-8
  -v              print the version number, then turn on verbose mode
  -w              turn warnings on for your script
  -W[level=2|:category]
                  set warning level; 0=silence, 1=medium, 2=verbose
  -x[directory]   strip off text before #!ruby line and perhaps cd to directory
  -h              show this message, --help for more options
  --ast           dump the parsed ruby-prism AST and exit
  --no-jit        disable just-in-time compilation
  --no-gc         disable garbage collection
  --enable=feature[,...], --disable=feature[,...]
                  enable or disable features (gems, did_you_mean, rubyopt,
                  frozen-string-literal, all)"
    );
}

fn main() {
    install_panic_hook();
    let _frame_stats = FrameStatsGuard;
    let tokens: Vec<OsString> = std::env::args_os().skip(1).collect();
    let mut opts = Opts::default();
    if let Err(f) = parse_switches(&tokens, &mut opts, false) {
        fail_cli(f);
    }

    // `-E ext:int` and `-U` conflict — but only when both come from the
    // actual command line (an `-U` in RUBYOPT is overridden silently).
    if opts.set_internal_utf8
        && let Some(int) = &opts.internal_enc
    {
        eprintln!(
            "monoruby: -U is incompatible with the internal encoding already set to {int} (RuntimeError)"
        );
        std::process::exit(1);
    }

    // RUBYOPT (unless --disable-rubyopt).
    let rubyopt_enabled = opts
        .features
        .iter()
        .rev()
        .find(|(f, _)| f == "rubyopt" || f == "all")
        .map(|(_, on)| *on)
        .unwrap_or(true);
    let mut ropts = Opts::default();
    if rubyopt_enabled
        && let Some(val) = std::env::var_os("RUBYOPT")
    {
        let rtokens: Vec<OsString> = val
            .to_string_lossy()
            .split_whitespace()
            .map(OsString::from)
            .collect();
        if let Err(f) = parse_switches(&rtokens, &mut ropts, true) {
            fail_rubyopt(f);
        }
    }

    // Merge RUBYOPT with command-line priority.
    opts.requires.extend(ropts.requires);
    opts.includes.extend(ropts.includes);
    opts.print_version |= ropts.print_version;
    opts.debug |= ropts.debug;
    if opts.external_enc.is_none() {
        opts.external_enc = ropts.external_enc;
    }
    if opts.internal_enc.is_none() {
        opts.internal_enc = ropts.internal_enc;
    }
    opts.set_internal_utf8 |= ropts.set_internal_utf8;
    if opts.kcode.is_none() {
        opts.kcode = ropts.kcode;
    }
    // Explicit command-line settings win; otherwise RUBYOPT applies.
    let warning_level = opts
        .warning_level
        .or(if opts.verbose || opts.debug { Some(2) } else { None })
        .or(ropts.warning_level)
        .or(if ropts.verbose || ropts.debug {
            Some(2)
        } else {
            None
        })
        .unwrap_or(1);
    // RUBYOPT categories first so command-line ones override them.
    let mut warn_categories = ropts.warn_categories;
    warn_categories.extend(std::mem::take(&mut opts.warn_categories));
    let mut features = ropts.features;
    features.extend(std::mem::take(&mut opts.features));

    // Resolve --enable/--disable features.
    let mut gems = true;
    let mut did_you_mean = false;
    let mut frozen_string_literal: Option<bool> = None;
    for (name, on) in &features {
        match name.as_str() {
            "gems" | "gem" => gems = *on,
            "did_you_mean" => did_you_mean = *on,
            "rubyopt" => {}
            "frozen-string-literal" | "frozen_string_literal" => {
                frozen_string_literal = Some(*on)
            }
            "error_highlight" | "syntax_suggest" | "jit" | "yjit" | "zjit" | "rjit" => {}
            "all" => {
                gems = *on;
                did_you_mean = *on;
                frozen_string_literal = Some(*on);
            }
            other => eprintln!(
                "monoruby: warning: unknown argument for --{}: `{}'",
                if *on { "enable" } else { "disable" },
                other
            ),
        }
    }
    if let Some(f) = frozen_string_literal {
        parser::set_frozen_string_literal_default(f);
    }

    // -C / -X (and the -x variant with a directory).
    for dir in opts.chdir.iter().chain(opts.x_dir.iter()) {
        if let Err(err) = std::env::set_current_dir(dir) {
            let msg = err.to_string();
            let msg = msg.split(" (os error ").next().unwrap();
            eprintln!("monoruby: Can't chdir to {dir} ({msg})");
            std::process::exit(1);
        }
    }

    let mut globals = Globals::new(warning_level, opts.no_jit, !gems);
    Globals::gc_enable(!opts.no_gc);
    if let Some(limit) = opts.backtrace_limit.or(ropts.backtrace_limit) {
        Globals::set_backtrace_limit(limit);
    }
    if opts.debug
        || ropts.debug
        || opts.debug_frozen_string_literal
        || ropts.debug_frozen_string_literal
    {
        monoruby::set_debug_frozen_string_log();
    }

    if opts.help {
        print_usage();
        return;
    }
    if opts.version_only {
        println!("{}", globals.ruby_description());
        return;
    }
    if opts.copyright {
        println!("{}", globals.ruby_copyright());
        return;
    }
    if opts.print_version {
        println!("{}", globals.ruby_description());
    }

    // $LOAD_PATH front: -I dirs (expanded, symlinks untouched), then
    // RUBYOPT -I dirs (already appended above), then RUBYLIB.
    let mut front: Vec<String> = opts.includes.iter().map(|d| expand_path(d)).collect();
    if let Ok(rubylib) = std::env::var("RUBYLIB") {
        front.extend(
            rubylib
                .split(':')
                .filter(|s| !s.is_empty())
                .map(|s| s.to_string()),
        );
    }
    globals.prepend_load_path(front.into_iter());

    let mut requires = Vec::new();
    if did_you_mean {
        requires.push("did_you_mean".to_string());
    }
    requires.append(&mut opts.requires);

    // Determine the program source.
    let mut prog_args = std::mem::take(&mut opts.args);
    let (code, path): (Vec<u8>, PathBuf) = if !opts.exec.is_empty() {
        // Multiple `-e` options form a single program (joined by
        // newlines), matching CRuby, so `-r` libraries and `at_exit`
        // handlers are processed once around the combined script.
        let code: Vec<u8> = opts
            .exec
            .iter()
            .map(|s| os_bytes(s))
            .collect::<Vec<_>>()
            .join(&b'\n');
        (code, PathBuf::from("-e"))
    } else {
        let first = if prog_args.is_empty() {
            None
        } else {
            Some(prog_args.remove(0))
        };
        match first {
            Some(f) if os_bytes(&f) != b"-" => {
                let file_name = lossy(&f);
                let script_path = if opts.search_path {
                    match search_script_in_path(&file_name) {
                        Some(p) => p,
                        None => {
                            eprintln!(
                                "monoruby: No such file or directory -- {file_name} (LoadError)"
                            );
                            std::process::exit(1);
                        }
                    }
                } else {
                    PathBuf::from(&file_name)
                };
                match read_source_file(&script_path) {
                    // Keep the path exactly as given on the command line: CRuby
                    // reports `$0` / `__FILE__` for the main script verbatim
                    // (relative stays relative), not canonicalized.
                    Ok((code, _resolved)) => (code, script_path),
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
            }
            other => {
                if opts.print_version {
                    // `-v` with no program: the banner is the whole output.
                    return;
                }
                let _ = other;
                // Read stdin as raw bytes: a script piped in may contain
                // non-UTF-8 bytes (e.g. a `# encoding:`-tagged source with
                // high bytes in a string literal); the whole source pipeline
                // carries bytes, so they round-trip into literals verbatim.
                let mut bytes = Vec::new();
                std::io::stdin()
                    .read_to_end(&mut bytes)
                    .expect("failed to read stdin");
                (bytes, PathBuf::from("-"))
            }
        }
    };

    // `-x` / a non-ruby `#!` first line: strip the leading garbage.
    let code = match preprocess_shebang(code, opts.dash_x) {
        Ok(c) => c,
        Err(msg) => {
            eprintln!("monoruby: {msg} (LoadError)");
            std::process::exit(1);
        }
    };

    // `-s`: consume `-switch[=value]` program arguments into globals.
    if opts.dash_s {
        while !prog_args.is_empty() {
            let arg = lossy(&prog_args[0]);
            if !arg.starts_with('-') || arg == "-" {
                break;
            }
            prog_args.remove(0);
            if arg == "--" {
                break;
            }
            let body = &arg[1..];
            let (name, value) = match body.split_once('=') {
                Some((n, v)) => (n.to_string(), Some(v.to_string())),
                None => (body.to_string(), None),
            };
            let name = name.replace('-', "_");
            if name.is_empty()
                || !name
                    .chars()
                    .all(|c| c.is_ascii_alphanumeric() || c == '_')
            {
                eprintln!("monoruby: invalid name for global variable - {arg} (NameError)");
                std::process::exit(1);
            }
            let val = match value {
                Some(v) => Value::string(v),
                None => Value::bool(true),
            };
            globals.set_gvar(monoruby::IdentId::get_id(&format!("${name}")), val);
        }
    }

    let argv = Value::array_from_iter(prog_args.iter().map(|s| Value::string(lossy(s))));
    globals.set_constant_by_str(OBJECT_CLASS, "ARGV", argv);
    globals.set_gvar(monoruby::IdentId::get_id("$*"), argv);

    if opts.ast {
        dump_ast(&code);
        return;
    }

    if opts.syntax_check {
        match parser::parse_program(code, path.as_path()) {
            Ok(_) => {
                println!("Syntax OK");
                return;
            }
            Err(err) => {
                err.show_error_message_and_all_loc(&globals.store);
                std::process::exit(1);
            }
        }
    }

    // `-n` / `-p`: arm the implicit `while gets ... end` wrap for the
    // main script's parse (consumed by path match, so `require`d files
    // are unaffected).
    if opts.loop_mode != LoopMode::None {
        parser::set_cli_loop_wrap(
            path.clone(),
            parser::CliLoopWrap {
                print_last: opts.loop_mode == LoopMode::GetsPrint,
                auto_split: opts.auto_split,
                chomp: opts.line_ending,
            },
        );
    }

    // Ruby prelude synthesized from the remaining switches; runs inside
    // the interpreter before `-r` requires and the script itself.
    let mut prelude = String::new();
    if let Some(enc) = &opts.external_enc {
        prelude.push_str(&format!(
            "Encoding.default_external = {}\n",
            ruby_str_lit(enc)
        ));
    }
    let internal_enc = opts.internal_enc.clone().or_else(|| {
        opts.set_internal_utf8
            .then(|| "UTF-8".to_string())
    });
    if let Some(enc) = &internal_enc {
        prelude.push_str(&format!(
            "Encoding.default_internal = {}\n",
            ruby_str_lit(enc)
        ));
    }
    if let Some(k) = opts.kcode {
        let enc = match k {
            'e' | 'E' => Some("EUC-JP"),
            'u' | 'U' => Some("UTF-8"),
            's' | 'S' => Some("Windows-31J"),
            'a' | 'A' | 'n' | 'N' => Some("ASCII-8BIT"),
            _ => None, // unknown codes are ignored, like CRuby
        };
        if let Some(enc) = enc {
            prelude.push_str(&format!(
                "Encoding.default_external = {}\n",
                ruby_str_lit(enc)
            ));
            // `-K` also sets the main script's *source* encoding
            // (`__ENCODING__`); a magic comment still wins.
            parser::set_cli_source_encoding(path.clone(), enc.to_string());
        }
    }
    if let Some(digits) = &opts.record_sep {
        let lit = if digits.is_empty() {
            // Bare `-0`: NUL separator.
            Some("\"\\x00\"".to_string())
        } else {
            match u32::from_str_radix(digits, 8) {
                Ok(0) => Some("\"\"".to_string()), // -00: paragraph mode
                Ok(n) if n > 0o377 => None,        // -0777: slurp (nil)
                Ok(n) => Some(format!("\"\\x{:02X}\"", n as u8)),
                Err(_) => None,
            }
        };
        match lit {
            Some(lit) => prelude.push_str(&format!("$/ = {lit}\n")),
            None => prelude.push_str("$/ = nil\n"),
        }
    }
    if opts.line_ending {
        prelude.push_str("$\\ = $/\n");
    }
    monoruby::set_cli_flag_gvars(
        opts.auto_split,
        opts.line_ending,
        opts.loop_mode == LoopMode::GetsPrint,
    );
    if let Some(pat) = &opts.field_sep {
        prelude.push_str(&format!("$; = Regexp.new({})\n", ruby_str_lit(pat)));
    }
    if opts.debug {
        prelude.push_str("$DEBUG = true\n");
    }
    // Verbose mode turns deprecation warnings on by default; explicit
    // -W:categories (applied afterwards) can override this.
    if warning_level >= 2 {
        prelude.push_str("Warning[:deprecated] = true\n");
    }
    for (cat, on) in &warn_categories {
        match cat.as_str() {
            "deprecated" | "experimental" | "performance" | "strict_unused_block" => {
                prelude.push_str(&format!("Warning[:{cat}] = {on}\n"));
            }
            other => eprintln!("monoruby: warning: unknown warning category: `{other}'"),
        }
    }

    let from_exec = !opts.exec.is_empty();
    match globals.run_with_prelude(&requires, &prelude, code, &path) {
        Ok(_val) => {
            // Debug builds echo the result value of `-e` one-liners to
            // stderr (script files stay silent, as before, so specs
            // that capture stderr aren't polluted).
            #[cfg(debug_assertions)]
            if from_exec {
                eprintln!("=> {:?}", _val)
            }
            #[cfg(not(debug_assertions))]
            let _ = from_exec;
        }
        Err(err) => {
            handle_error(err, &globals);
        }
    }
}
