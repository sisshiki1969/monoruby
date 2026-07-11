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

use crate::ast::{LvarCollector, ParseResult};
use crate::globals::MonorubyErr;

mod prism_backend;

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
) -> Result<ParseResult, MonorubyErr> {
    prism_backend::parse_program_binding(
        code,
        path.into(),
        context,
        extern_context,
        line_offset,
        default_encoding,
    )
}
