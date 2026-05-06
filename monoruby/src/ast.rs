//! Public AST surface used by monoruby's bytecode compiler and runtime.
//!
//! For the duration of the Prism migration this module simply re-exports
//! the AST types that monoruby currently consumes from `ruruby-parse`,
//! so that the rest of the crate depends on `crate::ast::*` rather than
//! reaching into the parser crate directly. Phase 2 will replace the
//! underlying types with monoruby-owned equivalents produced by a Prism
//! lowerer; consumers that already import through `crate::ast` will not
//! need their `use` statements rewritten again.
//!
//! Anything ruruby-parse-specific that we don't intend to keep across
//! the migration (for example, the `Parser` entry points themselves)
//! should NOT be re-exported here — callers of those should still go
//! through `ruruby_parse::Parser` directly so that the parser-vs-AST
//! boundary stays visible.

pub use ruruby_parse::{
    ArgList, BinOp, BlockInfo, CaseBranch, CmpKind, Loc, LocalsContext, LvarCollector, NReal,
    Node, NodeKind, ParamKind, ParseErr, ParseErrKind, ParseResult, RescueEntry, SourceInfoRef,
    UnOp,
};
