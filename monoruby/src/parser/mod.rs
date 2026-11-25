//! Parser entry-point abstraction.
//!
//! All parsing in monoruby flows through this module. The sole backend
//! is the Prism-based lowerer (`prism_backend`), which lowers ruby-prism's
//! AST into monoruby's [`crate::ast`] node tree.
//!
//! The module presents three entry points to the rest of the crate:
//! `parse_program`, `parse_program_eval`, and `parse_program_binding`.
//! All produce a [`crate::ast::ParseResult`] and report errors as
//! [`crate::globals::MonorubyErr`].

use std::path::PathBuf;
use std::sync::Mutex;
use std::sync::atomic::{AtomicI8, Ordering};

use crate::ast::{LvarCollector, ParseResult};
use crate::globals::MonorubyErr;

mod prism_backend;

/// Implicit `while gets ... end` wrap requested by the `-n` / `-p`
/// command-line switches. Applied to exactly one parse — the main
/// script, identified by its path (`-e` scripts parse as `"-e"`) — and
/// consumed on first match so `require`d files and `eval`s are never
/// wrapped.
#[derive(Debug, Clone, Copy)]
pub struct CliLoopWrap {
    /// `-p`: `print $_` at the end of each iteration.
    pub print_last: bool,
    /// `-a`: `$F = $_.split` at the top of each iteration.
    pub auto_split: bool,
    /// `-l`: `$_ = $_.chomp($/)` at the top of each iteration.
    pub chomp: bool,
}

static CLI_LOOP_WRAP: Mutex<Option<(PathBuf, CliLoopWrap)>> = Mutex::new(None);

/// Arm the `-n` / `-p` loop wrap for the main script at `path`.
pub fn set_cli_loop_wrap(path: PathBuf, wrap: CliLoopWrap) {
    *CLI_LOOP_WRAP.lock().unwrap() = Some((path, wrap));
}

/// Consume the armed loop wrap if `path` is the main script.
pub(crate) fn take_cli_loop_wrap(path: &std::path::Path) -> Option<CliLoopWrap> {
    let mut guard = CLI_LOOP_WRAP.lock().unwrap();
    if matches!(&*guard, Some((p, _)) if p == path) {
        guard.take().map(|(_, w)| w)
    } else {
        None
    }
}

/// Default *source* encoding for the main script, set by the `-K`
/// command-line switch. Like the loop wrap above, it is armed for a
/// single path (the main script) and consumed on first match; a
/// `# encoding:` magic comment in the file still wins.
static CLI_SOURCE_ENCODING: Mutex<Option<(PathBuf, String)>> = Mutex::new(None);

pub fn set_cli_source_encoding(path: PathBuf, enc: String) {
    *CLI_SOURCE_ENCODING.lock().unwrap() = Some((path, enc));
}

pub(crate) fn take_cli_source_encoding(path: &std::path::Path) -> Option<String> {
    let mut guard = CLI_SOURCE_ENCODING.lock().unwrap();
    if matches!(&*guard, Some((p, _)) if p == path) {
        guard.take().map(|(_, e)| e)
    } else {
        None
    }
}

/// Set for the main script when its source came from `-e` (prism's
/// `PM_OPTIONS_COMMAND_LINE_E`). Like the loop wrap and source encoding
/// above, it is armed for a single path (the `-e` script, which parses
/// as `"-e"`) and consumed on first match. CRuby suppresses the
/// "integer literal in flip-flop" and "regex literal in condition"
/// parse warnings for `-e` scripts — those awk/sed-style conditions are
/// idiomatic on the command line — but still emits them for files,
/// `require`s, and `eval`s, so the flag must not leak past the main
/// script.
static CLI_E_SCRIPT: Mutex<Option<PathBuf>> = Mutex::new(None);

/// Arm the `-e`-script marker for the main script at `path`.
pub fn set_cli_e_script(path: PathBuf) {
    *CLI_E_SCRIPT.lock().unwrap() = Some(path);
}

/// Consume the `-e`-script marker if `path` is the main script.
pub(crate) fn take_cli_e_script(path: &std::path::Path) -> bool {
    let mut guard = CLI_E_SCRIPT.lock().unwrap();
    if matches!(&*guard, Some(p) if p == path) {
        *guard = None;
        true
    } else {
        false
    }
}

/// Set for the main script when its ruby `#!` line ends in `\r\n`: the
/// line it is on. prism warns about it only for a main script it found
/// the `#!` line of itself (`pm_options_main_script_set`), which would
/// also have it search a script for that line as `-x` does; monoruby
/// does that search in `main.rs`, so the warning comes from there too.
/// Armed for a single path and consumed on first match, like the
/// markers above.
static CLI_SHEBANG_CR: Mutex<Option<(PathBuf, usize)>> = Mutex::new(None);

/// Arm the `#!`-line-ends-in-`\r` marker for the main script at `path`.
pub fn set_cli_shebang_cr(path: PathBuf, line: usize) {
    *CLI_SHEBANG_CR.lock().unwrap() = Some((path, line));
}

/// Consume the `#!`-line marker if `path` is the main script.
pub(crate) fn take_cli_shebang_cr(path: &std::path::Path) -> Option<usize> {
    let mut guard = CLI_SHEBANG_CR.lock().unwrap();
    if matches!(&*guard, Some((p, _)) if p == path) {
        guard.take().map(|(_, line)| line)
    } else {
        None
    }
}

/// The file a `require` / `load` is about to parse, with the encodings
/// CRuby gives its two path Strings.
pub(crate) struct LoadingFile {
    pub path: PathBuf,
    /// Of the path the file goes by: `__FILE__`, `Location#path`, the
    /// backtrace lines.
    pub path_encoding: crate::value::Encoding,
    /// Of its real path: `Location#absolute_path`, `__dir__`. The path's
    /// own, except for a file `load` found relative to the current
    /// directory (see `Executor::load`).
    pub realpath_encoding: crate::value::Encoding,
}

thread_local! {
    /// The file a `require` / `load` is running (#2090); consumed by its
    /// parse.
    static LOADING_FILE: std::cell::RefCell<Option<LoadingFile>> =
        const { std::cell::RefCell::new(None) };
    /// The encoding of the file name String an `eval` was given;
    /// consumed by its parse.
    static EVAL_NAME_ENCODING: std::cell::Cell<Option<crate::value::Encoding>> =
        const { std::cell::Cell::new(None) };
}

/// Leave the encoding of the file name String an `eval` was given for
/// the parse of its source, which is about to run: the path of the
/// eval'd code is that String, `__FILE__` included.
pub(crate) fn set_eval_name_encoding(enc: Option<crate::value::Encoding>) {
    EVAL_NAME_ENCODING.with(|c| c.set(enc));
}

thread_local! {
    /// The warnings of the last parse, when it ended in an error: there
    /// is no `ParseResult` to carry them then, and CRuby prints them
    /// before it raises the error. Replaced by every parse, emptied by
    /// [`take_error_warnings`].
    static ERROR_WARNINGS: std::cell::RefCell<Vec<(String, bool)>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

/// Leave the warnings of a parse that is about to fail (an empty list
/// when a parse starts, so that nothing outlives the parse it is of).
pub(crate) fn stash_error_warnings(warnings: Vec<(String, bool)>) {
    ERROR_WARNINGS.with(|w| *w.borrow_mut() = warnings);
}

/// The warnings of the parse that just failed on this thread, in
/// `ParseResult::warnings`' shape (formatted line, verbose-only flag).
/// `Executor::flush_compile_warnings` prints them with the rest; `-c`,
/// which has no interpreter running, prints them itself.
pub fn take_error_warnings() -> Vec<(String, bool)> {
    ERROR_WARNINGS.with(|w| std::mem::take(&mut *w.borrow_mut()))
}

/// Arm (or, with `None`, disarm) the marker for the file a `require` /
/// `load` is about to parse.
pub(crate) fn set_loading_file(file: Option<LoadingFile>) {
    LOADING_FILE.with(|r| *r.borrow_mut() = file);
}

/// The encodings of the two path Strings CRuby keeps for a source parsed
/// from *path* — the path it goes by and its real path (see
/// `SourceInfo::path_encoding`, `SourceInfo::realpath_encoding`). They
/// differ for one kind of source only, which [`LoadingFile`] describes.
///
/// For an `eval`: BINARY for the `(eval at …)` name it makes up, and the
/// encoding of the name String for one it was given
/// ([`set_eval_name_encoding`]; UTF-8 when the caller left none). For a
/// file `require` / `load` runs: what [`set_loading_file`] left for it,
/// which follows the feature String (`require "x".b` makes it the
/// filesystem encoding).
/// For the main script and `-e`: the locale encoding. A file of the
/// `builtins/` tree takes the locale encoding too: its `<internal:…>`
/// frames stand in for C frames, which CRuby shows at their caller's
/// path, and that is the locale's unless a `require` or an `eval` made
/// it.
pub(crate) fn source_path_encoding(
    path: &std::path::Path,
    is_eval: bool,
) -> (crate::value::Encoding, crate::value::Encoding) {
    let both = |enc| (enc, enc);
    let eval_name = EVAL_NAME_ENCODING.with(|c| c.take());
    if crate::globals::is_builtins_path(path) {
        return both(crate::builtins::encoding::locale_encoding());
    }
    if is_eval {
        return both(
            if path
                .as_os_str()
                .as_encoded_bytes()
                .starts_with(b"(eval at ")
            {
                crate::value::Encoding::Ascii8
            } else {
                eval_name.unwrap_or(crate::value::Encoding::UTF8)
            },
        );
    }
    LOADING_FILE.with(|r| {
        let mut r = r.borrow_mut();
        match r.take_if(|f| f.path == path) {
            Some(f) => (f.path_encoding, f.realpath_encoding),
            None => both(crate::builtins::encoding::locale_encoding()),
        }
    })
}

/// Process-wide default for the `frozen_string_literal` pragma, set by
/// the `--enable/--disable-frozen-string-literal` command-line flags.
/// A per-file magic comment still wins. -1 = unset, 0 = false, 1 = true.
static FROZEN_STRING_LITERAL_DEFAULT: AtomicI8 = AtomicI8::new(-1);

pub fn set_frozen_string_literal_default(default: bool) {
    FROZEN_STRING_LITERAL_DEFAULT.store(default as i8, Ordering::Relaxed);
}

pub(crate) fn frozen_string_literal_default() -> Option<bool> {
    match FROZEN_STRING_LITERAL_DEFAULT.load(Ordering::Relaxed) {
        0 => Some(false),
        1 => Some(true),
        _ => None,
    }
}

pub fn parse_program(
    code: impl Into<Vec<u8>>,
    path: impl Into<PathBuf>,
) -> Result<ParseResult, MonorubyErr> {
    #[cfg(feature = "prof")]
    let _phase = crate::prof::phase(crate::prof::Phase::Parse);
    prism_backend::parse_program(code.into(), path.into(), true)
}

/// `parse_program`, but with every `def` body lowered on the spot.
///
/// The normal parse hands a method body on as a
/// [`crate::ast::DeferredDef`], so it is only looked at when its bytecode
/// is generated. The modes that consume the tree itself and never compile
/// it — `-c` (which must report a body's `unsupported_node` error) and
/// `--ast` (which prints it) — ask for the whole thing here.
pub fn parse_program_eager(
    code: impl Into<Vec<u8>>,
    path: impl Into<PathBuf>,
) -> Result<ParseResult, MonorubyErr> {
    #[cfg(feature = "prof")]
    let _phase = crate::prof::phase(crate::prof::Phase::Parse);
    prism_backend::parse_program(code.into(), path.into(), false)
}

/// Lower the parameters and body of a `def` the parse deferred.
pub(crate) fn lower_deferred_def(
    deferred: &crate::ast::DeferredDef,
) -> Result<(crate::ast::BlockInfo, Vec<(String, bool)>), MonorubyErr> {
    #[cfg(feature = "prof")]
    let _phase = crate::prof::phase(crate::prof::Phase::Parse);
    prism_backend::lower_deferred_def(deferred)
}

/// `eval` / `instance_eval` / `class_eval`. The Prism backend receives
/// the surrounding scopes via the vendored ruby-prism's
/// `parse_with_options` (see `vendor/ruby-prism/src/lib.rs`).
pub(crate) fn parse_program_eval(
    code: Vec<u8>,
    path: impl Into<PathBuf>,
    extern_context: Option<&crate::globals::ExternalContext>,
    line_offset: i64,
    default_encoding: Option<String>,
) -> Result<ParseResult, MonorubyErr> {
    #[cfg(feature = "prof")]
    let _phase = crate::prof::phase(crate::prof::Phase::Parse);
    prism_backend::parse_program_eval(
        code,
        path.into(),
        extern_context,
        line_offset,
        default_encoding,
    )
}

/// `binding.eval` / `eval(code, binding)`. The binding's frame locals
/// are pushed as the innermost surrounding scope.
pub(crate) fn parse_program_binding(
    code: Vec<u8>,
    path: impl Into<PathBuf>,
    context: Option<LvarCollector>,
    extern_context: Option<&crate::globals::ExternalContext>,
    line_offset: i64,
    default_encoding: Option<String>,
    main_script: bool,
) -> Result<ParseResult, MonorubyErr> {
    #[cfg(feature = "prof")]
    let _phase = crate::prof::phase(crate::prof::Phase::Parse);
    prism_backend::parse_program_binding(
        code,
        path.into(),
        context,
        extern_context,
        line_offset,
        default_encoding,
        main_script,
    )
}
