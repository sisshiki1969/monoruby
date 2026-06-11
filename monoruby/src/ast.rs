//! monoruby's own Ruby AST.
//!
//! These node types were originally defined in the `ruruby-parse` crate and
//! re-exported here. With the migration to the Prism parser complete,
//! `ruruby-parse` is gone and the AST is owned by monoruby: the Prism lowerer
//! (`crate::parser::prism_backend`) builds these nodes directly, and the
//! bytecode compiler consumes them through `crate::ast::*`.
//!
//! The node-construction helpers (`Node::new_*`, etc.) are retained as the
//! AST's API even though the lowerer mostly builds nodes via struct literals;
//! `allow(dead_code)` keeps the unused ones from warning.
#![allow(dead_code)]

mod error;
mod lvar_collector;
mod node;
mod source_info;

pub use error::*;
pub use lvar_collector::*;
pub use node::*;
pub use source_info::*;

use num::BigInt;

/// A value annotated with its source location. `Node` is `Annot<NodeKind>`
/// and `FormalParam` is `Annot<ParamKind>`.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Annot<T: PartialEq + Default> {
    pub kind: T,
    pub loc: Loc,
}

impl<T: PartialEq + Default> Annot<T> {
    pub fn new(kind: T, loc: Loc) -> Self {
        Annot { kind, loc }
    }

    pub fn loc(&self) -> Loc {
        self.loc
    }
}

/// Surrounding local-variable scope supplied to `eval` / `binding.eval`
/// parses so outer locals resolve. Implemented by the runtime's
/// `ExternalContext` (see `globals.rs`).
pub trait LocalsContext: Sized {
    fn find_lvar(&self, id: &str) -> Option<usize>;
}

/// A parsed program: the root node plus the top-level local-variable table
/// and the source map used for error reporting.
#[derive(Debug)]
pub struct ParseResult {
    pub node: Node,
    pub lvar_collector: LvarCollector,
    pub source_info: SourceInfoRef,
}

/// One `rescue` clause of a `begin`/`rescue` (or modifier `rescue`).
#[derive(Debug, Clone, PartialEq)]
pub struct RescueEntry {
    /// The exception classes for this rescue clause.
    pub exception_list: Vec<Node>,
    /// Assignment destination for the error value in the rescue clause.
    pub assign: Option<Box<Node>>,
    /// The body of this rescue clause.
    pub body: Box<Node>,
}

/// A numeric real literal as produced by the lowerer (the imaginary part of
/// a `Complex`, before it is turned into a runtime value).
#[derive(Debug, Clone, PartialEq)]
pub enum NReal {
    Integer(i64),
    Bignum(BigInt),
    Float(f64),
}
