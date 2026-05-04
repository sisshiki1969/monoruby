//! Parser entry-point abstraction.
//!
//! All parsing in monoruby flows through this module. The default
//! backend is the Prism-based lowerer (with automatic fallback to
//! ruruby for nodes the lowerer doesn't yet handle); setting the
//! `MONORUBY_PARSER=ruruby` environment variable forces the original
//! `ruruby-parse` backend instead.
//!
//! The module presents the same three entry points the rest of the
//! crate previously called on `ruruby_parse::Parser` directly:
//! `parse_program`, `parse_program_eval`, and `parse_program_binding`.
//! Both backends produce a [`crate::ast::ParseResult`] (which is
//! still an alias for `ruruby_parse::ParseResult` for now) and report
//! errors as [`crate::globals::MonorubyErr`].

use std::path::PathBuf;

use crate::ast::{LocalsContext, LvarCollector, ParseResult};
use crate::globals::MonorubyErr;

mod prism_backend;
mod ruruby_backend;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Backend {
    Ruruby,
    Prism,
}

/// Pick the parser backend at runtime. Defaults to `Prism` (with
/// transparent ruruby fallback for unimplemented nodes); setting
/// `MONORUBY_PARSER=ruruby` forces the legacy ruruby-parse backend.
/// `MONORUBY_PARSER=prism` is accepted for symmetry but is the
/// default.
pub fn default_backend() -> Backend {
    match std::env::var("MONORUBY_PARSER").ok().as_deref() {
        Some("ruruby") => Backend::Ruruby,
        _ => Backend::Prism,
    }
}

pub fn parse_program(
    code: String,
    path: impl Into<PathBuf>,
) -> Result<ParseResult, MonorubyErr> {
    parse_program_with(default_backend(), code, path)
}

pub fn parse_program_with(
    backend: Backend,
    code: String,
    path: impl Into<PathBuf>,
) -> Result<ParseResult, MonorubyErr> {
    let path = path.into();
    match backend {
        Backend::Ruruby => ruruby_backend::parse_program(code, path),
        Backend::Prism => prism_backend::parse_program(code, path),
    }
}

/// `eval` / `instance_eval` / `class_eval`. Dispatches to the
/// configured backend ([`default_backend`]). The Prism backend now
/// receives the surrounding scopes via the vendored ruby-prism's
/// `parse_with_options` (see `vendor/ruby-prism/src/lib.rs`); the
/// ruruby backend keeps its existing direct support.
pub fn parse_program_eval(
    code: String,
    path: impl Into<PathBuf>,
    extern_context: Option<&crate::globals::ExternalContext>,
    line_offset: i64,
) -> Result<ParseResult, MonorubyErr> {
    let path = path.into();
    match default_backend() {
        Backend::Ruruby => {
            ruruby_backend::parse_program_eval(code, path, extern_context, line_offset)
        }
        Backend::Prism => {
            prism_backend::parse_program_eval(code, path, extern_context, line_offset)
        }
    }
}

/// `binding.eval` / `eval(code, binding)`. Dispatches like
/// [`parse_program_eval`]; for the Prism backend the binding's
/// frame locals are pushed as the innermost surrounding scope.
pub fn parse_program_binding(
    code: String,
    path: impl Into<PathBuf>,
    context: Option<LvarCollector>,
    extern_context: Option<&crate::globals::ExternalContext>,
    line_offset: i64,
) -> Result<ParseResult, MonorubyErr> {
    let path = path.into();
    match default_backend() {
        Backend::Ruruby => ruruby_backend::parse_program_binding(
            code,
            path,
            context,
            extern_context,
            line_offset,
        ),
        Backend::Prism => prism_backend::parse_program_binding(
            code,
            path,
            context,
            extern_context,
            line_offset,
        ),
    }
}
