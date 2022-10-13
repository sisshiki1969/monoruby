#![feature(box_patterns)]
#![feature(int_roundings)]
#![feature(const_option)]
#![feature(once_cell)]
mod alloc;
mod executor;
mod id_table;
mod rvalue;
#[cfg(test)]
mod tests;
mod value;

use alloc::*;
pub use executor::*;
use fxhash::FxHashMap as HashMap;
use id_table::*;
use monoasm::CodePtr;
use ruruby_parse::{
    ArgList, BinOp, BlockInfo, CmpKind, FormalParam, Loc, LvarCollector, Node, NodeKind, ParamKind,
    ParseErr, ParseErrKind, Parser, SourceInfoRef, UnOp,
};
use rvalue::*;
use value::*;

pub fn compile_and_run(
    globals: &mut Globals,
    code: &str,
    path: &std::path::Path,
) -> Result<Value, MonorubyErr> {
    let fid = match globals.compile_script(code.to_string(), path) {
        Ok(fid) => fid,
        Err(err) => {
            eprintln!("{}", err.get_error_message(&globals));
            err.show_all_loc();
            return Err(err);
        }
    };
    let res = Interp::eval_toplevel(globals, fid);
    if let Err(err) = &res {
        eprintln!("{}", err.get_error_message(&globals));
        err.show_all_loc();
    }
    res
}
