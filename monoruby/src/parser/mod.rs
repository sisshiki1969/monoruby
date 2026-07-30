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
    prism_backend::parse_program(code.into(), path.into())
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
