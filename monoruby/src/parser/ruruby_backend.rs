//! Default backend: forwards everything to `ruruby_parse::Parser` and
//! converts the parser's error type to [`MonorubyErr`].

use std::path::PathBuf;

use crate::ast::{LocalsContext, LvarCollector, ParseResult};
use crate::globals::MonorubyErr;

pub(super) fn parse_program(
    code: String,
    path: PathBuf,
) -> Result<ParseResult, MonorubyErr> {
    ruruby_parse::Parser::parse_program(code, path).map_err(MonorubyErr::parse)
}

pub(super) fn parse_program_eval<C: LocalsContext>(
    code: String,
    path: PathBuf,
    extern_context: Option<&C>,
    line_offset: i64,
) -> Result<ParseResult, MonorubyErr> {
    ruruby_parse::Parser::parse_program_eval(code, path, extern_context, line_offset)
        .map_err(MonorubyErr::parse)
}

pub(super) fn parse_program_binding<C: LocalsContext>(
    code: String,
    path: PathBuf,
    context: Option<LvarCollector>,
    extern_context: Option<&C>,
    line_offset: i64,
) -> Result<ParseResult, MonorubyErr> {
    ruruby_parse::Parser::parse_program_binding(
        code,
        path,
        context,
        extern_context,
        line_offset,
    )
    .map_err(MonorubyErr::parse)
}
