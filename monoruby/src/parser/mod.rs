//! Parser entry-point abstraction.
//!
//! All parsing in monoruby flows through this module. The default
//! backend is `ruruby-parse` (the original parser); setting the
//! `MONORUBY_PARSER=prism` environment variable selects the
//! Prism-backed lowerer instead.
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

/// Pick the parser backend at runtime. Defaults to `Ruruby`; setting
/// the env var `MONORUBY_PARSER=prism` switches to the Prism lowerer
/// (still a work-in-progress; many constructs are `unimplemented!`).
pub fn default_backend() -> Backend {
    match std::env::var("MONORUBY_PARSER").ok().as_deref() {
        Some("prism") => Backend::Prism,
        _ => Backend::Ruruby,
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

pub fn parse_program_eval<C: LocalsContext>(
    code: String,
    path: impl Into<PathBuf>,
    extern_context: Option<&C>,
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

pub fn parse_program_binding<C: LocalsContext>(
    code: String,
    path: impl Into<PathBuf>,
    context: Option<LvarCollector>,
    extern_context: Option<&C>,
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
