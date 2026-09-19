//! `def` bodies the lowerer has not visited yet.
//!
//! monoruby builds its own AST from prism's tree, so a file that is
//! mostly method definitions materializes two complete trees at once and
//! keeps the second one until the last function is compiled. Method
//! bodies are nearly all of that mass — a converted-wasm source is tens
//! of thousands of lines of `def`, almost none of it ever executed.
//!
//! So a `def` is lowered where its bytecode is generated, not where the
//! file is parsed: [`Lowerer::lower_def`] emits a [`DeferredDef`] — a
//! handle on prism's own `def` node — and bytecodegen lowers it, compiles
//! it and drops it, one body at a time.
//!
//! [`Lowerer::lower_def`]: crate::parser
//!
//! Keeping the handle means keeping prism's tree alive past the parse,
//! which [`PrismTree`] does.

use std::rc::Rc;

use ruby_prism::{self as prism, DefNode};

use super::SourceInfoRef;

///
/// A parsed prism tree, owned together with the source it borrows.
///
pub struct PrismTree {
    /// Dropped first — it points into `source` and frees prism's arena.
    ///
    /// The `'static` is a lie told to the borrow checker: the real
    /// lifetime is `&'self.source`. Nothing hands a `ParseResult<'static>`
    /// out; [`PrismTree::root`] re-borrows it for no longer than the
    /// tree itself.
    result: prism::ParseResult<'static>,
    /// Boxed, so the bytes keep their address when the `PrismTree` moves.
    source: Box<[u8]>,
}

impl PrismTree {
    pub fn parse(source: Vec<u8>, options: Option<&prism::Options>) -> Self {
        let source: Box<[u8]> = source.into_boxed_slice();
        // SAFETY: `source` is boxed and this struct never exposes a way to
        // mutate or reallocate it, so the bytes stay at this address for
        // as long as the `PrismTree` lives. `result` is declared before
        // `source`, so it is dropped — freeing prism's parser and node
        // arena — while those bytes are still valid.
        let borrowed: &'static [u8] = unsafe { std::mem::transmute(&*source) };
        let result = match options {
            Some(opts) => prism::parse_with_options(borrowed, opts),
            None => prism::parse(borrowed),
        };
        Self { result, source }
    }

    pub fn source(&self) -> &[u8] {
        &self.source
    }

    pub fn result(&self) -> &prism::ParseResult<'_> {
        &self.result
    }

    pub fn root(&self) -> prism::Node<'_> {
        self.result.node()
    }
}

///
/// Everything a deferred body needs to be lowered later: the tree the
/// node lives in, plus the per-file lowering context that produced it.
///
/// One per parsed file, shared by every [`DeferredDef`] in it.
///
pub struct DeferCtx {
    pub tree: PrismTree,
    /// The path, for `__FILE__`.
    pub path: String,
    pub source_info: SourceInfoRef,
    pub line_offset: i64,
    pub eval_parse: bool,
}

///
/// A `def` whose parameters and body prism has parsed and the lowerer has
/// not looked at.
///
/// Bytecodegen turns it into a [`crate::ast::BlockInfo`] the moment it
/// reaches the definition — see `BytecodeGen::lower_deferred`.
///
pub struct DeferredDef {
    /// Keeps the prism arena `node` points into alive.
    ctx: Rc<DeferCtx>,
    /// The `def` node, with its lifetime erased.
    ///
    /// A prism node handle is a parser pointer plus a node pointer; the
    /// lifetime is only a marker. The real lifetime is `&ctx.tree`,
    /// restored by [`DeferredDef::node`].
    node: DefNode<'static>,
    /// `Lowerer::prism_scope_level` at the definition site. A method body
    /// resolves no local outside itself, so this is all the enclosing
    /// scope state the deferred lowering needs.
    scope_level: u32,
    /// `Lowerer::pm_temp` at the definition site, so the hidden locals the
    /// pattern-match desugar synthesizes cannot collide with an enclosing
    /// scope's.
    pm_temp: usize,
}

impl DeferredDef {
    ///
    /// # Safety
    ///
    /// `node` must belong to `ctx.tree`.
    ///
    pub unsafe fn new(
        ctx: Rc<DeferCtx>,
        node: &DefNode<'_>,
        scope_level: u32,
        pm_temp: usize,
    ) -> Self {
        // SAFETY: the caller guarantees `node` points into `ctx.tree`,
        // which this struct holds, so the erased lifetime is bounded by
        // `ctx` — see the field's doc comment. `DefNode` is a parser
        // pointer, a node pointer and a `PhantomData`, so reading it out
        // duplicates a handle rather than an owner, and the transmute
        // only rewrites the marker.
        let node = unsafe { std::ptr::read(node) };
        let node = unsafe { std::mem::transmute::<DefNode<'_>, DefNode<'static>>(node) };
        Self {
            ctx,
            node,
            scope_level,
            pm_temp,
        }
    }

    pub fn ctx(&self) -> &Rc<DeferCtx> {
        &self.ctx
    }

    /// The `def` node, re-borrowed for no longer than this handle.
    pub fn node(&self) -> &DefNode<'_> {
        // SAFETY: shrinking `'static` back to `&self`, which `self.ctx`
        // outlives by construction.
        unsafe { std::mem::transmute::<&DefNode<'static>, &DefNode<'_>>(&self.node) }
    }

    pub fn scope_level(&self) -> u32 {
        self.scope_level
    }

    pub fn pm_temp(&self) -> usize {
        self.pm_temp
    }
}

impl Clone for DeferredDef {
    fn clone(&self) -> Self {
        Self {
            ctx: self.ctx.clone(),
            // SAFETY: `DefNode` is a parser pointer, a node pointer and a
            // `PhantomData` — no ownership, so a bitwise copy is another
            // handle on the same node, valid as long as the cloned `ctx`.
            node: unsafe { std::ptr::read(&self.node) },
            scope_level: self.scope_level,
            pm_temp: self.pm_temp,
        }
    }
}

impl PartialEq for DeferredDef {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.ctx, &other.ctx)
            && self.node().location().start_offset() == other.node().location().start_offset()
    }
}

impl std::fmt::Debug for DeferredDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "DeferredDef(@{})",
            self.node().location().start_offset()
        )
    }
}
