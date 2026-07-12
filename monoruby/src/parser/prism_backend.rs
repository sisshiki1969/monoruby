//! Prism-backed parser entry points.
//!
//! Calls `ruby_prism::parse(...)` and lowers the borrowed Prism
//! `Node<'pr>` tree into monoruby's owned [`crate::ast::Node`] shape that
//! the rest of monoruby consumes.
//!
//! Prism failures fall into two cases:
//!
//! * **Parse error** — Prism rejected the source as invalid Ruby.
//!   Surfaced to the caller as a normal [`MonorubyErr`] so the
//!   interpreter can report it the same way as a CRuby
//!   `SyntaxError`.
//! * **Unsupported node** — Prism parsed successfully but the
//!   lowerer has no handler for one of the produced node types.
//!   Surfaced as a Ruby-level [`MonorubyErrKind::Fatal`] error
//!   carrying the offending node name and the file path. The host
//!   process keeps running and the script gets the same trace it
//!   would for any other unhandled error; add the missing arm to
//!   [`Lowerer::lower_node`] (or the relevant sub-helper) when the
//!   error fires.

use std::path::PathBuf;

use ruby_prism::{
    self as prism, ArrayNode, BeginNode, BlockNode, ClassNode, ConstantId, ConstantList,
    ConstantPathNode, ConstantReadNode, ConstantWriteNode, DefNode, FloatNode, HashNode, IfNode,
    IntegerNode, InterpolatedRegularExpressionNode, InterpolatedStringNode, LambdaNode,
    LocalVariableReadNode, LocalVariableWriteNode, Location, ModuleNode, MultiWriteNode,
    ParametersNode, ProgramNode, RangeNode, RegularExpressionNode, RescueNode, ReturnNode,
    StatementsNode, StringNode, SymbolNode, UnlessNode, UntilNode, WhileNode,
};

use crate::ast::{
    ArgList, BinOp, BlockInfo, CmpKind, DestructEntry, Loc, LvarCollector, NReal, Node, NodeKind,
    ParamKind, ParseResult, SourceInfoRef, UnOp,
};
use crate::globals::{ExternalContext, MonorubyErr};
use crate::id_table::IdentId;

/// Reserved, unspellable local names bound to anonymous `*` / `**`
/// parameters so their forwarding forms (`foo(*)` / `foo(**)`) can
/// reference them. Neither is a valid Ruby identifier, so they can't
/// collide with a user variable (mirrors the empty name used for `&`).
const ANON_REST_NAME: &str = "*";
const ANON_KWREST_NAME: &str = "**";

/// Reserved, unspellable local bound to the hidden single loop variable
/// synthesized for a `for <complex-target> in ...` loop (one whose index
/// has a splat / post element / nested or non-local target). The body is
/// prefixed with `<target> = <this>` so the ordinary multiple-assignment
/// machinery destructures each element while the targets still leak to the
/// enclosing scope. Not a valid Ruby identifier, so it can't collide.
const FOR_INDEX_NAME: &str = "(for)";

pub(super) fn parse_program(code: Vec<u8>, path: PathBuf) -> Result<ParseResult, MonorubyErr> {
    try_prism_inner(&code, path, None, None, 0, None)
}

pub(super) fn parse_program_eval(
    code: Vec<u8>,
    path: PathBuf,
    extern_context: Option<&ExternalContext>,
    line_offset: i64,
    default_encoding: Option<String>,
) -> Result<ParseResult, MonorubyErr> {
    let options = build_prism_options(extern_context, None, line_offset);
    try_prism_inner(&code, path, Some(options), None, line_offset, default_encoding)
}

pub(super) fn parse_program_binding(
    code: Vec<u8>,
    path: PathBuf,
    context: Option<LvarCollector>,
    extern_context: Option<&ExternalContext>,
    line_offset: i64,
    default_encoding: Option<String>,
) -> Result<ParseResult, MonorubyErr> {
    let options = build_prism_options(extern_context, context.as_ref(), line_offset);
    try_prism_inner(&code, path, Some(options), context, line_offset, default_encoding)
}

/// Build a `prism::Options` for an eval/binding parse. Prism's
/// scope list is **outermost-first**; monoruby's `ExternalContext`
/// stores them innermost-first (`scope[0]` = caller of eval), so we
/// iterate in reverse. If `binding_locals` is supplied we append it
/// as the very innermost scope (the binding's own frame).
fn build_prism_options(
    extern_context: Option<&ExternalContext>,
    binding_locals: Option<&LvarCollector>,
    line_offset: i64,
) -> prism::Options {
    // Prism's `line` is 1-indexed. monoruby tracks an offset
    // (`lineno - 1`) at the call sites in `globals.rs`, so the
    // wire-format we want is `line_offset + 1`.
    let line = line_offset.saturating_add(1).clamp(1, i32::MAX as i64) as i32;

    let mut scopes: Vec<prism::Scope> = Vec::new();

    if let Some(ctx) = extern_context {
        // Walk outermost -> innermost.
        for scope_idx in (0..ctx.len()).rev() {
            let (locals, block_param) = &ctx[scope_idx];
            let mut names: Vec<String> = locals.keys().map(|id: &IdentId| id.get_name()).collect();
            if let Some(blk_id) = block_param {
                names.push(blk_id.get_name());
            }
            scopes.push(prism::Scope::new(names));
        }
    }

    // Prism's `pm_parser_init` makes the *last* options scope the
    // parser's current scope, which `parse_program` then reuses as
    // the program's own scope. So whichever scope we push last is
    // the "eval body" scope.
    //
    // - For plain `eval(code)` (no binding), the body needs its own
    //   fresh, empty scope on top of the surrounding-scope stack;
    //   any reference to e.g. `str` inside `eval("str")` shows up
    //   with `depth=1` so bytecodegen walks the outer chain. This
    //   matches Ruby's `Prism.parse(source, scopes:)` API, which
    //   internally appends an empty scope.
    // - For `binding.eval(code)` the body *shares* the binding's
    //   own frame — that frame's locals are visible at depth 0 and
    //   any new locals introduced by the eval append onto it. So
    //   the binding's locals go in as the last (eval-body-itself)
    //   scope, and we don't append a separate empty placeholder.
    if let Some(coll) = binding_locals {
        let names: Vec<String> = coll.table().iter().cloned().collect();
        scopes.push(prism::Scope::new(names));
    } else {
        scopes.push(prism::Scope::new(Vec::<&[u8]>::new()));
    }

    prism::Options::new().line(line).scopes(scopes)
}

/// Result of lowering the `block` slot on a Prism call/super node.
enum CallBlock {
    /// A block literal (`{ ... }` / `do ... end`) or a `&expr`
    /// argument — placed into `ArgList::block` as-is.
    Block(Node),
    /// `foo(&)` — anonymous forward of the enclosing method's block.
    /// Caller flips `ArgList::delegate_block`.
    Delegate,
}

/// Extract the source-encoding name declared by a `# coding:` /
/// `# encoding:` magic comment, following CRuby's placement rules:
///
/// * The directive must be on the first line, or the second line when
///   the first line is a shebang (`#!...`).
/// * That line must be a comment from its first token — only ASCII
///   whitespace may precede the `#` (so `1 + 1 # encoding: x` does not
///   count).
/// * Within the comment the name is whatever follows `coding` (as a
///   substring, so both `coding` and `encoding` match — and `fileencoding`
///   in a vim modeline) immediately followed by `:` or `=`. The match is
///   case-insensitive. Emacs `-*- coding: X -*-` and vim
///   `fileencoding=X` fall out of the same scan.
///
/// Returns the raw declared name (e.g. `"big5"`); the caller maps it to an
/// [`crate::value::Encoding`] via `Encoding::try_from_str`.
fn detect_source_encoding(code: &[u8]) -> Option<String> {
    let mut lines = code.split(|&b| b == b'\n');
    let first = lines.next().unwrap_or(&[]);
    let candidate: &[u8] = if first.starts_with(b"#!") {
        lines.next().unwrap_or(&[])
    } else {
        first
    };
    // Only ASCII whitespace may precede the `#`.
    let mut i = 0;
    while i < candidate.len()
        && matches!(candidate[i], b' ' | b'\t' | b'\r' | 0x0b | 0x0c)
    {
        i += 1;
    }
    let comment = candidate.get(i..)?.strip_prefix(b"#")?;
    let comment = std::str::from_utf8(comment).ok()?;
    parse_coding_directive(comment)
}

/// Scan a comment body for `coding` immediately followed (after optional
/// blanks) by `:` or `=` and an encoding name. See `detect_source_encoding`.
fn parse_coding_directive(comment: &str) -> Option<String> {
    let lower = comment.to_ascii_lowercase();
    let lb = lower.as_bytes();
    let cb = comment.as_bytes();
    let mut from = 0;
    while let Some(off) = lower[from..].find("coding") {
        let mut j = from + off + "coding".len();
        while j < lb.len() && matches!(lb[j], b' ' | b'\t') {
            j += 1;
        }
        if j < lb.len() && matches!(lb[j], b':' | b'=') {
            j += 1;
            while j < lb.len() && matches!(lb[j], b' ' | b'\t') {
                j += 1;
            }
            let start = j;
            while j < cb.len()
                && (cb[j].is_ascii_alphanumeric() || cb[j] == b'_' || cb[j] == b'-')
            {
                j += 1;
            }
            if j > start {
                // Preserve the declared case; `try_from_str` normalizes.
                return Some(comment[start..j].to_string());
            }
        }
        from += off + 1;
    }
    None
}

/// Detect a `# frozen_string_literal: true|false` magic comment.
///
/// Unlike the encoding directive (which CRuby honours only on the very
/// first line), the `frozen_string_literal` pragma may appear on any of the
/// leading comment lines — CRuby scans the contiguous comment block at the
/// top of the file (after an optional shebang) and stops at the first line
/// that is not a comment. Returns `Some(true)`/`Some(false)` for the last
/// matching directive, or `None` when the pragma is absent.
fn detect_frozen_string_literal(code: &[u8]) -> Option<bool> {
    let mut result = None;
    for (idx, line) in code.split(|&b| b == b'\n').enumerate() {
        // Only ASCII whitespace may precede the `#`.
        let mut i = 0;
        while i < line.len() && matches!(line[i], b' ' | b'\t' | b'\r' | 0x0b | 0x0c) {
            i += 1;
        }
        let rest = &line[i..];
        // A shebang is allowed only on the first line and does not end the
        // magic-comment block.
        if idx == 0 && rest.starts_with(b"#!") {
            continue;
        }
        match rest.strip_prefix(b"#") {
            Some(comment) => {
                if let Ok(s) = std::str::from_utf8(comment) {
                    if let Some(v) = parse_frozen_directive(s) {
                        result = Some(v);
                    }
                }
            }
            // First non-comment line (code or blank) ends the block.
            None => break,
        }
    }
    result
}

/// Scan a comment body for `frozen_string_literal` immediately followed
/// (after optional blanks) by `:` or `=` and a `true`/`false` value.
fn parse_frozen_directive(comment: &str) -> Option<bool> {
    // CRuby treats `-` and `_` in magic-comment names as equivalent
    // (`# frozen-string-literal: false` appears in the stock
    // `rbconfig.rb`), so normalize before matching.
    let lower = comment.to_ascii_lowercase().replace('-', "_");
    let lb = lower.as_bytes();
    let key = "frozen_string_literal";
    let mut from = 0;
    while let Some(off) = lower[from..].find(key) {
        let mut j = from + off + key.len();
        while j < lb.len() && matches!(lb[j], b' ' | b'\t') {
            j += 1;
        }
        if j < lb.len() && matches!(lb[j], b':' | b'=') {
            j += 1;
            while j < lb.len() && matches!(lb[j], b' ' | b'\t') {
                j += 1;
            }
            let start = j;
            while j < lb.len() && (lb[j].is_ascii_alphanumeric() || lb[j] == b'_') {
                j += 1;
            }
            match &lower[start..j] {
                "true" => return Some(true),
                "false" => return Some(false),
                _ => {}
            }
        }
        from += off + 1;
    }
    None
}

fn try_prism_inner(
    code: &[u8],
    path: PathBuf,
    options: Option<prism::Options>,
    seed_lvars: Option<LvarCollector>,
    line_offset: i64,
    default_encoding: Option<String>,
) -> Result<ParseResult, MonorubyErr> {
    let path_display = path.display().to_string();
    // `-n` / `-p`: the main script (and only the main script) gets its
    // top-level statements wrapped in an implicit `while gets ... end`.
    let cli_loop_wrap = super::take_cli_loop_wrap(&path);
    // `-K`: default source encoding for the main script.
    let cli_source_encoding = super::take_cli_source_encoding(&path);

    let result = match options.as_ref() {
        Some(opts) => prism::parse_with_options(code, opts),
        None => prism::parse(code),
    };

    // Detect the source encoding from the `# coding:` / `# encoding:`
    // magic comment ourselves rather than via prism's `magic_comments()`
    // list: prism reports every `key: value` comment regardless of
    // position, whereas CRuby only honours an encoding directive on the
    // first line (or the second line when the first is a shebang) and
    // only when the line is a comment from its first token. See
    // `detect_source_encoding`.
    // A magic comment wins; otherwise fall back to the caller-supplied
    // default (the eval'd string's own encoding — CRuby uses it as the
    // eval source encoding when no `# encoding:` comment is present).
    let source_encoding: Option<String> = detect_source_encoding(code)
        .or(cli_source_encoding)
        .or(default_encoding);
    // Per-file magic comment wins; otherwise fall back to the
    // `--enable/--disable-frozen-string-literal` process default.
    let frozen_string_literal: Option<bool> = detect_frozen_string_literal(code)
        .or_else(super::frozen_string_literal_default);

    // SourceInfo's copy of the source is display-only (error carets,
    // `get_line`), so a binary source with invalid UTF-8 bytes is
    // stored lossily; the parse and the lowerer above work on the raw
    // bytes. For valid-UTF-8 sources (the overwhelming case) the lossy
    // conversion is the identity.
    let source_info: SourceInfoRef = std::rc::Rc::new(
        crate::ast::SourceInfo::new_eval(
            path,
            String::from_utf8_lossy(code).into_owned(),
            line_offset,
        )
        .with_source_encoding(source_encoding)
        .with_frozen_string_literal(frozen_string_literal),
    );

    if let Some(diag) = result.errors().next() {
        let loc = location_to_loc(&diag.location());
        return Err(MonorubyErr::parse(crate::ast::ParseErr {
            kind: crate::ast::ParseErrKind::SyntaxError(format!("prism: {}", diag.message())),
            loc,
            source_info,
        }));
    }

    // Forward a vetted subset of prism's parse warnings, formatted the
    // way CRuby prints them. Only messages whose wording *and* CRuby
    // warning level we have verified are forwarded — the Rust prism
    // wrapper does not expose the diagnostic level, so an allowlist
    // (message → verbose-only flag) stands in for it. Everything else
    // is dropped rather than risking warnings CRuby would not print
    // (e.g. prism flags an eval's final literal as "useless use of a
    // literal in void context"; CRuby does not, since that literal is
    // the eval's value).
    let warnings: Vec<(String, bool)> = result
        .warnings()
        .filter_map(|w| {
            let msg = w.message();
            let verbose_only = if msg == "END in method; use at_exit"
                || msg == "integer literal in flip-flop"
                || msg == "regex literal in condition"
            {
                false
            } else if msg == "possibly useless use of defined? in void context"
                || (msg.starts_with("'when' clause on line ")
                    && msg.ends_with(" and is ignored"))
            {
                true
            } else {
                return None;
            };
            let loc = location_to_loc(&w.location());
            let line = source_info.get_line(&loc);
            Some((
                format!("{}:{}: warning: {}", path_display, line, msg),
                verbose_only,
            ))
        })
        .collect();

    let mut lowerer = Lowerer::new(code, path_display, source_info.clone());
    lowerer.line_offset = line_offset;
    lowerer.eval_parse = options.is_some();
    lowerer.cli_loop_wrap = cli_loop_wrap;
    if let Some(seed) = seed_lvars {
        // For `binding.eval`, monoruby preloads a `LvarCollector`
        // with the binding's existing locals so any *new* names the
        // eval body introduces append onto it. Match that on the
        // Prism side too.
        lowerer.lvars = seed;
    }
    // The lowerer raises a recoverable `SyntaxError` (via
    // `unsupported_node`) when it hits a node it doesn't handle, so
    // we just propagate the error here. Prism parsed the file fine,
    // but the construct is not accepted by this implementation, so
    // `SyntaxError` is the correct Ruby class and — being `rescue`-
    // able — a single unsupported construct only fails its own
    // example instead of aborting the whole process.
    let body = lowerer.lower_top(&result.node())?;
    let mut warnings = warnings;
    warnings.append(&mut lowerer.warnings);
    let lvar_collector = lowerer.into_lvars();

    Ok(ParseResult {
        node: body,
        lvar_collector,
        source_info,
        warnings,
    })
}

struct Lowerer<'pr> {
    #[allow(dead_code)]
    source: &'pr [u8],
    /// The path of the file we're lowering. Inlined for `__FILE__`.
    path: String,
    /// Shared source-info handle used to seed the location frame on
    /// any `MonorubyErr` we raise from the lowerer (so users get a
    /// `<path>:<line>` prefix and a code-pointing arrow). The
    /// caller in `try_prism_inner` builds this once and stores it
    /// here as well as on the `ParseResult`.
    source_info: SourceInfoRef,
    /// 0 for ordinary file parses; for `eval(_, _, _, lineno)` and
    /// `binding.eval` this is `lineno - 1`. Added to every
    /// `__LINE__` literal so the inlined value matches the host
    /// frame instead of starting back at 1 inside the eval body.
    line_offset: i64,
    lvars: LvarCollector,
    /// Number of prism scopes (program → def/class/block/lambda
    /// nesting) currently entered. Maintained by
    /// `enter_prism_scope` / `exit_prism_scope` and consumed by
    /// `adjust_lvar_depth`.
    prism_scope_level: u32,
    /// Prism scope levels at which the lowerer synthesized a closure
    /// scope prism doesn't know about (currently only the hidden
    /// `at_exit` block wrapping an `END { ... }` body). A local
    /// variable reference whose target scope lies at or outside such
    /// a level must hop one extra scope per crossed wrap; the crossed
    /// names are collected so the wrap site can anchor them in their
    /// home scope.
    scope_wraps: Vec<ScopeWrap>,
    /// Parse warnings raised by the lowerer itself (in addition to
    /// prism's own diagnostics), merged into `ParseResult::warnings`
    /// by `try_prism_inner`. Same shape: (formatted message,
    /// verbose-only flag).
    warnings: Vec<(String, bool)>,
    /// `true` for `eval` / `binding.eval` parses. Suppresses warnings
    /// that only apply to a script file's top level (e.g. "argument of
    /// top-level return is ignored" — a return in an eval belongs to
    /// the surrounding frame, not the script's top level).
    eval_parse: bool,
    /// Monotonic counter for hidden locals synthesized by the pattern
    /// matching desugar (`%pm<n>` — `%` is not spellable, so they never
    /// surface in `local_variables`).
    pm_temp: usize,
    /// `-n` / `-p` command-line switches: wrap the program body in an
    /// implicit `while gets ... end` (see `lower_program`).
    cli_loop_wrap: Option<super::CliLoopWrap>,
}

/// See [`Lowerer::scope_wraps`].
struct ScopeWrap {
    /// Prism scope level the wrap was inserted at.
    level: u32,
    /// Names of locals referenced across this wrap.
    escaped: Vec<String>,
}

impl<'pr> Lowerer<'pr> {
    fn new(source: &'pr [u8], path: String, source_info: SourceInfoRef) -> Self {
        Self {
            source,
            path,
            source_info,
            line_offset: 0,
            lvars: LvarCollector::new(),
            prism_scope_level: 0,
            scope_wraps: Vec::new(),
            warnings: Vec::new(),
            eval_parse: false,
            pm_temp: 0,
            cli_loop_wrap: None,
        }
    }

    /// A fresh hidden local name for the pattern-matching desugar.
    fn pm_temp_name(&mut self) -> String {
        self.pm_temp += 1;
        format!("%pm{}", self.pm_temp)
    }

    /// Enter a prism scope (def/class/block/lambda body): bumps the
    /// scope level and hands out a fresh `LvarCollector`, returning
    /// the outer one for `exit_prism_scope` to restore.
    fn enter_prism_scope(&mut self) -> LvarCollector {
        self.prism_scope_level += 1;
        std::mem::take(&mut self.lvars)
    }

    /// Leave a prism scope: restores the outer `LvarCollector` and
    /// returns the scope's own collector (used by block/def lowering
    /// to attach it to the produced `BlockInfo`).
    fn exit_prism_scope(&mut self, saved: LvarCollector) -> LvarCollector {
        self.prism_scope_level -= 1;
        std::mem::replace(&mut self.lvars, saved)
    }

    /// Translate a prism-reported local variable depth into a
    /// monoruby-AST depth, adding one hop for every synthesized
    /// closure scope (`scope_wraps`) between the reference and its
    /// target scope. Every crossing is also recorded on the crossed
    /// wraps so `lower_post_execution` can anchor the variable in its
    /// home scope (bytecodegen materializes local slots lazily on
    /// first depth-0 reference, which a variable only ever referenced
    /// from inside the synthesized block would otherwise never get).
    fn adjust_lvar_depth(&mut self, depth: usize, name: &str) -> usize {
        if self.scope_wraps.is_empty() {
            return depth;
        }
        let target = self.prism_scope_level - depth as u32;
        let mut hops = 0;
        for wrap in self.scope_wraps.iter_mut() {
            if target <= wrap.level {
                hops += 1;
                if !wrap.escaped.iter().any(|n| n == name) {
                    wrap.escaped.push(name.to_string());
                }
            }
        }
        depth + hops
    }

    fn into_lvars(self) -> LvarCollector {
        self.lvars
    }

    /// Build the canonical "prism lowerer hit an unsupported node"
    /// error for a given node-kind tag and source location. Emitted
    /// as a recoverable `SyntaxError` (Prism parsed the file fine,
    /// but the construct is not accepted by this implementation), so
    /// one unsupported construct only fails its own example instead
    /// of aborting the whole process. Carries the lowerer's
    /// `SourceInfoRef` so the host's error formatter can print the
    /// usual `<path>:<line>` prefix and an arrow under the span.
    fn unsupported_node(&self, kind: &str, loc: Loc) -> MonorubyErr {
        MonorubyErr::syntax_with_loc(
            format!(
                "prism lowerer hit an unsupported node `{kind}` while parsing {} \
                 — add a handler in monoruby/src/parser/prism_backend.rs",
                self.source_info.path.display(),
            ),
            loc,
            self.source_info.clone(),
        )
    }

    /// Convenience wrapper around `unsupported_node` that takes a
    /// raw Prism node and pulls its `node_kind_name` + location for
    /// you. Use this from the catch-all arms of every
    /// dispatch-style match.
    fn unsupported(&self, context: &'static str, node: &prism::Node<'_>) -> MonorubyErr {
        let kind = node_kind_name(node);
        let location = location_to_loc(&node.location());
        self.unsupported_node(&format!("{}:{}", context, kind), location)
    }

    /// Top-level entry: lowers a `ProgramNode` to a `CompStmt`.
    fn lower_top(&mut self, node: &prism::Node<'pr>) -> Result<Node, MonorubyErr> {
        match node {
            prism::Node::ProgramNode { .. } => {
                let program: ProgramNode<'_> = node.as_program_node().unwrap();
                self.lower_program(&program)
            }
            other => Err(self.unsupported("top", other)),
        }
    }

    fn lower_program(&mut self, node: &ProgramNode<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        // Pre-populate the local variable table from Prism's per-scope
        // locals list so the resulting AST matches what ruruby-parse
        // would have produced (bytecodegen reads `LvarCollector` to
        // assign register slots before walking the tree).
        self.collect_locals(&node.locals())?;
        // `BEGIN { ... }` blocks run before the rest of the code
        // unit, in source order: CRuby hoists them to the top of the
        // program, and their bodies share the toplevel scope, so
        // simply lowering the hoisted statements first is enough.
        let body = node.statements();
        let mut stmts = Vec::new();
        for n in body.body().iter() {
            if n.as_pre_execution_node().is_some() {
                stmts.push(self.lower_node(&n)?);
            }
        }
        let mut main_stmts = Vec::new();
        for n in body.body().iter() {
            if n.as_pre_execution_node().is_none() {
                main_stmts.push(self.lower_node(&n)?);
            }
        }
        if let Some(wrap) = self.cli_loop_wrap {
            // `-n` / `-p`: run the (non-BEGIN) program body inside an
            // implicit `while gets ... end` — CRuby wraps at the AST
            // level too, which is why BEGIN blocks still parse and run
            // once, before the loop.
            stmts.push(Self::build_cli_loop(wrap, main_stmts, loc));
        } else {
            stmts.append(&mut main_stmts);
        }
        Ok(Node {
            kind: NodeKind::CompStmt(stmts),
            loc,
        })
    }

    /// Build the `-n` / `-p` implicit loop:
    ///
    /// ```text
    /// while gets
    ///   $_ = $_.chomp($/)   # -l
    ///   $F = $_.split       # -a
    ///   <program body>
    ///   print $_            # -p
    /// end
    /// ```
    fn build_cli_loop(wrap: super::CliLoopWrap, body_stmts: Vec<Node>, loc: Loc) -> Node {
        let gvar = |name: &str| Node {
            kind: NodeKind::GlobalVar(name.to_string()),
            loc,
        };
        let assign = |target: Node, value: Node| Node {
            kind: NodeKind::MulAssign(vec![target], vec![value]),
            loc,
        };
        let mut stmts = Vec::new();
        if wrap.chomp {
            let chomp = Node::new_mcall(
                gvar("$_"),
                "chomp".to_string(),
                ArgList::from_args(vec![gvar("$/")]),
                false,
                loc,
            );
            stmts.push(assign(gvar("$_"), chomp));
        }
        if wrap.auto_split {
            let split = Node::new_mcall_noarg(gvar("$_"), "split".to_string(), false, loc);
            stmts.push(assign(gvar("$F"), split));
        }
        stmts.extend(body_stmts);
        if wrap.print_last {
            stmts.push(Node::new_fcall(
                "print".to_string(),
                ArgList::from_args(vec![gvar("$_")]),
                false,
                loc,
            ));
        }
        let cond = Node::new_fcall_noarg("gets".to_string(), false, loc);
        let body = Node {
            kind: NodeKind::CompStmt(stmts),
            loc,
        };
        Node::new_while(cond, body, true, loc)
    }

    fn collect_locals(&mut self, locals: &ConstantList<'pr>) -> Result<(), MonorubyErr> {
        for id in locals.iter() {
            let name = constant_name(&id)?;
            self.lvars.insert(&name);
        }
        Ok(())
    }

    /// Register block-local shadow variables (`{ |x; a, b| ... }`).
    /// They are fresh locals scoped to the block body, so adding them
    /// to the block's (already fresh) lvar table gives the correct
    /// shadowing behaviour.
    fn collect_block_local_shadows(
        &mut self,
        locals: &prism::NodeList<'pr>,
    ) -> Result<(), MonorubyErr> {
        for n in locals.iter() {
            if let Some(blv) = n.as_block_local_variable_node() {
                let name = constant_name(&blv.name())?;
                self.lvars.insert(&name);
            }
        }
        Ok(())
    }

    fn lower_statements_into_vec(
        &mut self,
        node: &StatementsNode<'pr>,
    ) -> Result<Vec<Node>, MonorubyErr> {
        node.body().iter().map(|n| self.lower_node(&n)).collect()
    }

    fn lower_node(&mut self, node: &prism::Node<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        Ok(match node {
            prism::Node::NilNode { .. } => Node {
                kind: NodeKind::Nil,
                loc,
            },
            prism::Node::TrueNode { .. } => Node {
                kind: NodeKind::Bool(true),
                loc,
            },
            prism::Node::FalseNode { .. } => Node {
                kind: NodeKind::Bool(false),
                loc,
            },
            prism::Node::SelfNode { .. } => Node {
                kind: NodeKind::SelfValue,
                loc,
            },
            prism::Node::IntegerNode { .. } => self.lower_integer(&node.as_integer_node().unwrap()),
            prism::Node::FloatNode { .. } => self.lower_float(&node.as_float_node().unwrap()),
            prism::Node::StringNode { .. } => self.lower_string(&node.as_string_node().unwrap()),
            prism::Node::SymbolNode { .. } => self.lower_symbol(&node.as_symbol_node().unwrap()),
            prism::Node::StatementsNode { .. } => {
                let inner: StatementsNode<'_> = node.as_statements_node().unwrap();
                Node {
                    kind: NodeKind::CompStmt(self.lower_statements_into_vec(&inner)?),
                    loc,
                }
            }
            prism::Node::CallNode { .. } => self.lower_call(&node.as_call_node().unwrap(), loc)?,
            prism::Node::LocalVariableReadNode { .. } => {
                self.lower_local_var_read(&node.as_local_variable_read_node().unwrap())?
            }
            prism::Node::ItLocalVariableReadNode { .. } => {
                // Ruby 3.4 `it` reference. Bound by the enclosing
                // block's synthesized `Param("it")` (see the
                // `ItParametersNode` arm), always at depth 0.
                let n = node.as_it_local_variable_read_node().unwrap();
                Node {
                    kind: NodeKind::LocalVar(0, "it".to_string()),
                    loc: location_to_loc(&n.location()),
                }
            }
            prism::Node::LocalVariableWriteNode { .. } => {
                self.lower_local_var_write(&node.as_local_variable_write_node().unwrap())?
            }
            prism::Node::IfNode { .. } => self.lower_if(&node.as_if_node().unwrap())?,
            prism::Node::UnlessNode { .. } => self.lower_unless(&node.as_unless_node().unwrap())?,
            prism::Node::WhileNode { .. } => self.lower_while(&node.as_while_node().unwrap())?,
            prism::Node::UntilNode { .. } => self.lower_until(&node.as_until_node().unwrap())?,
            prism::Node::ArrayNode { .. } => self.lower_array(&node.as_array_node().unwrap())?,
            prism::Node::HashNode { .. } => self.lower_hash(&node.as_hash_node().unwrap())?,
            prism::Node::RangeNode { .. } => self.lower_range(&node.as_range_node().unwrap())?,
            prism::Node::FlipFlopNode { .. } => {
                self.lower_flip_flop(&node.as_flip_flop_node().unwrap())?
            }
            prism::Node::ParenthesesNode { .. } => {
                let inner = node.as_parentheses_node().unwrap();
                match inner.body() {
                    Some(body) => self.lower_node(&body)?,
                    None => Node {
                        kind: NodeKind::Nil,
                        loc,
                    },
                }
            }
            prism::Node::SplatNode { .. } => {
                let inner = node.as_splat_node().unwrap();
                let expr = match inner.expression() {
                    Some(e) => self.lower_node(&e)?,
                    // Anonymous `*` (forwarding `foo(*)`): splat the reserved
                    // anonymous-rest local bound by the enclosing `def m(*)`.
                    None => Node {
                        kind: NodeKind::LocalVar(0, ANON_REST_NAME.to_owned()),
                        loc,
                    },
                };
                Node {
                    kind: NodeKind::Splat(Box::new(expr)),
                    loc,
                }
            }
            prism::Node::DefNode { .. } => self.lower_def(&node.as_def_node().unwrap())?,
            prism::Node::ClassNode { .. } => self.lower_class(&node.as_class_node().unwrap())?,
            prism::Node::ModuleNode { .. } => self.lower_module(&node.as_module_node().unwrap())?,
            prism::Node::SingletonClassNode { .. } => {
                let n = node.as_singleton_class_node().unwrap();
                let singleton = self.lower_node(&n.expression())?;
                let saved = self.enter_prism_scope();
                if let Err(e) = self.collect_locals(&n.locals()) {
                    self.exit_prism_scope(saved);
                    return Err(e);
                }
                let info_res = self.lower_class_body(n.body(), loc);
                self.exit_prism_scope(saved);
                let info = info_res?;
                Node {
                    kind: NodeKind::SingletonClassDef {
                        singleton: Box::new(singleton),
                        info: Box::new(info),
                    },
                    loc,
                }
            }
            prism::Node::ConstantReadNode { .. } => {
                self.lower_constant_read(&node.as_constant_read_node().unwrap())?
            }
            prism::Node::ConstantPathNode { .. } => {
                self.lower_constant_path(&node.as_constant_path_node().unwrap())?
            }
            prism::Node::ConstantWriteNode { .. } => {
                self.lower_constant_write(&node.as_constant_write_node().unwrap())?
            }
            prism::Node::ReturnNode { .. } => self.lower_return(&node.as_return_node().unwrap())?,
            prism::Node::InterpolatedStringNode { .. } => {
                self.lower_interpolated_string(&node.as_interpolated_string_node().unwrap())?
            }
            prism::Node::XStringNode { .. } => {
                // `` `cmd` `` — ruruby wraps the static string in
                // `Command(...)`. The wrapped node is whichever
                // string shape ruruby would have produced for the
                // body, here a plain `NodeKind::String`.
                let n = node.as_x_string_node().unwrap();
                let body_loc = location_to_loc(&n.content_loc());
                let bytes = n.unescaped();
                let body_kind = match std::str::from_utf8(bytes) {
                    Ok(s) => NodeKind::String(s.to_owned()),
                    Err(_) => NodeKind::Bytes(bytes.to_vec()),
                };
                let body = Node {
                    kind: body_kind,
                    loc: body_loc,
                };
                Node {
                    kind: NodeKind::Command(Box::new(body)),
                    loc,
                }
            }
            prism::Node::InterpolatedXStringNode { .. } => {
                // `` `echo #{x}` `` — same `Command(...)` wrapper as
                // the static form, but the inner node is an
                // `InterporatedString` (note ruruby's misspelling).
                let n = node.as_interpolated_x_string_node().unwrap();
                let parts = self.lower_interp_parts(n.parts())?;
                let inner = Node {
                    kind: NodeKind::InterporatedString(parts),
                    loc,
                };
                Node {
                    kind: NodeKind::Command(Box::new(inner)),
                    loc,
                }
            }
            prism::Node::RationalNode { .. } => {
                let n = node.as_rational_node().unwrap();
                Node {
                    kind: NodeKind::Rational(
                        prism_integer_to_bigint(&n.numerator()),
                        prism_integer_to_bigint(&n.denominator()),
                    ),
                    loc,
                }
            }
            prism::Node::ImaginaryNode { .. } => {
                let n = node.as_imaginary_node().unwrap();
                self.lower_imaginary(&n.numeric(), loc)?
            }
            prism::Node::InterpolatedSymbolNode { .. } => {
                // ruruby has no dedicated InterpolatedSymbol variant —
                // it desugars `:"foo#{x}"` to `(InterporatedString
                // ["foo", x]).to_sym`. Match that.
                let n = node.as_interpolated_symbol_node().unwrap();
                let parts = self.lower_interp_parts(n.parts())?;
                let inner = Node {
                    kind: NodeKind::InterporatedString(parts),
                    loc,
                };
                Node {
                    kind: NodeKind::MethodCall {
                        receiver: Box::new(inner),
                        method: "to_sym".to_owned(),
                        arglist: Box::new(crate::ast::ArgList::default()),
                        safe_nav: false,
                    },
                    loc,
                }
            }
            prism::Node::RegularExpressionNode { .. } => {
                self.lower_regex(&node.as_regular_expression_node().unwrap())?
            }
            prism::Node::InterpolatedRegularExpressionNode { .. } => self
                .lower_interpolated_regex(
                    &node.as_interpolated_regular_expression_node().unwrap(),
                )?,
            prism::Node::MatchLastLineNode { .. } => {
                // `if /pat/` — a regexp literal in a conditional matches
                // the last read line implicitly. Desugar to
                // `/pat/ =~ $_` (nil / position result gives the
                // conditional its truthiness, and `$~` is set).
                let n = node.as_match_last_line_node().unwrap();
                let part = Node {
                    kind: regex_body_to_string(n.unescaped(), location_to_loc(&n.location()))?,
                    loc: location_to_loc(&n.location()),
                };
                let flags = regex_flags_from_closing(&n.closing_loc());
                let regex = Node {
                    kind: NodeKind::RegExp(vec![part], flags, true),
                    loc,
                };
                match_last_line(regex, loc)
            }
            prism::Node::InterpolatedMatchLastLineNode { .. } => {
                let n = node.as_interpolated_match_last_line_node().unwrap();
                let parts = self.lower_interp_parts(n.parts())?;
                let flags = regex_flags_from_closing(&n.closing_loc());
                let regex = Node {
                    kind: NodeKind::RegExp(parts, flags, false),
                    loc,
                };
                match_last_line(regex, loc)
            }
            prism::Node::MatchWriteNode { .. } => {
                // `/(?<name>...)/ =~ value` — Prism wraps the `=~`
                // CallNode with a list of `LocalVariableTargetNode`
                // targets that should receive each named capture.
                // Prism only produces a `MatchWriteNode` for the exact
                // syntactic form of a *regexp literal* with named
                // captures on the *left* of `=~`; every other shape
                // (`str =~ /re/`, a regexp *variable*, an explicit
                // `.=~` / `.send` call) is a plain `CallNode` and binds
                // nothing. Prism has also already declared each target
                // on its owning scope's `locals` list (at the right
                // depth), so `local_variables` reports them and outer
                // scopes are reused rather than shadowed.
                //
                // Desugar to:
                //   %match = (/re/ =~ value)                # run the match, set `$~`
                //   name1  = Regexp.last_match(:name1)       # capture or nil
                //   name2  = Regexp.last_match(:name2)
                //   %match                                   # value == `=~` result
                //
                // The trailing `%match` read preserves the expression's
                // value (the match position, or `nil`) so uses like
                // `if /re/ =~ s` keep the correct truthiness even when a
                // capture is `nil`. `%match` starts with `%`, which is
                // not a spellable local-variable name, so it never
                // surfaces in `local_variables`. `Regexp.last_match`
                // returns `nil` when the match failed, giving every
                // capture the `nil` no-match value for free.
                let n = node.as_match_write_node().unwrap();
                let call = n.call();
                let call_node = self.lower_call(&call, location_to_loc(&call.location()))?;
                let temp_name = "%match".to_string();
                let mut stmts = vec![Node::new_mul_assign(
                    vec![Node {
                        kind: NodeKind::LocalVar(0, temp_name.clone()),
                        loc,
                    }],
                    vec![call_node],
                )];
                for target in n.targets().iter() {
                    let tgt = self.lower_assign_target(&target)?;
                    let cap_name = match &tgt.kind {
                        NodeKind::LocalVar(_, name) => name.clone(),
                        _ => unreachable!("MatchWriteNode target is not a local variable"),
                    };
                    let regexp = Node::new_const("Regexp".to_string(), false, None, vec![], loc);
                    let arglist =
                        crate::ast::ArgList::from_args(vec![Node::new_symbol(cap_name, loc)]);
                    let last_match =
                        Node::new_mcall(regexp, "last_match".to_string(), arglist, false, loc);
                    stmts.push(Node::new_mul_assign(vec![tgt], vec![last_match]));
                }
                stmts.push(Node {
                    kind: NodeKind::LocalVar(0, temp_name),
                    loc,
                });
                Node {
                    kind: NodeKind::CompStmt(stmts),
                    loc,
                }
            }
            prism::Node::BeginNode { .. } => self.lower_begin(&node.as_begin_node().unwrap())?,
            // `BEGIN { ... }`: hoisted to the top of the code unit by
            // `lower_program`, so by the time this arm runs the node is
            // already in first position and its body (which shares the
            // toplevel scope) can be lowered inline. `END { ... }`
            // registers its body as a once-only at_exit handler (see
            // `lower_post_execution`).
            prism::Node::PreExecutionNode { .. } => {
                let n = node.as_pre_execution_node().unwrap();
                match n.statements() {
                    Some(stmts) => self.lower_statements_compact(&stmts, loc)?,
                    None => Node {
                        kind: NodeKind::Nil,
                        loc,
                    },
                }
            }
            prism::Node::PostExecutionNode { .. } => {
                let n = node.as_post_execution_node().unwrap();
                self.lower_post_execution(&n, loc)?
            }
            prism::Node::MultiWriteNode { .. } => {
                self.lower_multi_write(&node.as_multi_write_node().unwrap())?
            }
            prism::Node::LocalVariableOperatorWriteNode { .. } => {
                let n = node.as_local_variable_operator_write_node().unwrap();
                let target = Node {
                    kind: {
                        let name = constant_name(&n.name())?;
                        let depth = self.adjust_lvar_depth(n.depth() as usize, &name);
                        NodeKind::LocalVar(depth, name)
                    },
                    loc: location_to_loc(&n.name_loc()),
                };
                self.build_op_assign(target, &n.binary_operator(), &n.value(), loc)?
            }
            prism::Node::LocalVariableOrWriteNode { .. } => {
                let n = node.as_local_variable_or_write_node().unwrap();
                let target = Node {
                    kind: {
                        let name = constant_name(&n.name())?;
                        let depth = self.adjust_lvar_depth(n.depth() as usize, &name);
                        NodeKind::LocalVar(depth, name)
                    },
                    loc: location_to_loc(&n.name_loc()),
                };
                self.build_short_circuit_assign(BinOp::LOr, target, &n.value(), loc)?
            }
            prism::Node::LocalVariableAndWriteNode { .. } => {
                let n = node.as_local_variable_and_write_node().unwrap();
                let target = Node {
                    kind: {
                        let name = constant_name(&n.name())?;
                        let depth = self.adjust_lvar_depth(n.depth() as usize, &name);
                        NodeKind::LocalVar(depth, name)
                    },
                    loc: location_to_loc(&n.name_loc()),
                };
                self.build_short_circuit_assign(BinOp::LAnd, target, &n.value(), loc)?
            }
            prism::Node::InstanceVariableOperatorWriteNode { .. } => {
                let n = node.as_instance_variable_operator_write_node().unwrap();
                let target = Node {
                    kind: NodeKind::InstanceVar(constant_name(&n.name())?),
                    loc: location_to_loc(&n.name_loc()),
                };
                self.build_op_assign(target, &n.binary_operator(), &n.value(), loc)?
            }
            prism::Node::InstanceVariableOrWriteNode { .. } => {
                let n = node.as_instance_variable_or_write_node().unwrap();
                let target = Node {
                    kind: NodeKind::InstanceVar(constant_name(&n.name())?),
                    loc: location_to_loc(&n.name_loc()),
                };
                self.build_short_circuit_assign(BinOp::LOr, target, &n.value(), loc)?
            }
            prism::Node::InstanceVariableAndWriteNode { .. } => {
                let n = node.as_instance_variable_and_write_node().unwrap();
                let target = Node {
                    kind: NodeKind::InstanceVar(constant_name(&n.name())?),
                    loc: location_to_loc(&n.name_loc()),
                };
                self.build_short_circuit_assign(BinOp::LAnd, target, &n.value(), loc)?
            }
            prism::Node::GlobalVariableOperatorWriteNode { .. } => {
                let n = node.as_global_variable_operator_write_node().unwrap();
                let target = Node {
                    kind: NodeKind::GlobalVar(constant_name(&n.name())?),
                    loc: location_to_loc(&n.name_loc()),
                };
                self.build_op_assign(target, &n.binary_operator(), &n.value(), loc)?
            }
            prism::Node::GlobalVariableOrWriteNode { .. } => {
                let n = node.as_global_variable_or_write_node().unwrap();
                let target = Node {
                    kind: NodeKind::GlobalVar(constant_name(&n.name())?),
                    loc: location_to_loc(&n.name_loc()),
                };
                // `$x ||= v` must not fire the verbose-mode
                // "uninitialized global" warning for its read (CRuby
                // treats lazy initialization as fine), so guard the
                // short-circuit read behind a definedness check:
                // if defined?($x) then $x ||= v else $x = v end
                let assign = Node::new_mul_assign(
                    vec![target.clone()],
                    vec![self.lower_node(&n.value())?],
                );
                let or_assign =
                    self.build_short_circuit_assign(BinOp::LOr, target.clone(), &n.value(), loc)?;
                Node::new_if(
                    Node::new(NodeKind::Defined(Box::new(target)), loc),
                    or_assign,
                    assign,
                    loc,
                )
            }
            prism::Node::GlobalVariableAndWriteNode { .. } => {
                let n = node.as_global_variable_and_write_node().unwrap();
                let target = Node {
                    kind: NodeKind::GlobalVar(constant_name(&n.name())?),
                    loc: location_to_loc(&n.name_loc()),
                };
                self.build_short_circuit_assign(BinOp::LAnd, target, &n.value(), loc)?
            }
            prism::Node::ClassVariableOperatorWriteNode { .. } => {
                let n = node.as_class_variable_operator_write_node().unwrap();
                let target = Node {
                    kind: NodeKind::ClassVar(constant_name(&n.name())?),
                    loc: location_to_loc(&n.name_loc()),
                };
                self.build_op_assign(target, &n.binary_operator(), &n.value(), loc)?
            }
            prism::Node::ClassVariableOrWriteNode { .. } => {
                let n = node.as_class_variable_or_write_node().unwrap();
                let target = Node {
                    kind: NodeKind::ClassVar(constant_name(&n.name())?),
                    loc: location_to_loc(&n.name_loc()),
                };
                self.build_short_circuit_assign(BinOp::LOr, target, &n.value(), loc)?
            }
            prism::Node::ClassVariableAndWriteNode { .. } => {
                let n = node.as_class_variable_and_write_node().unwrap();
                let target = Node {
                    kind: NodeKind::ClassVar(constant_name(&n.name())?),
                    loc: location_to_loc(&n.name_loc()),
                };
                self.build_short_circuit_assign(BinOp::LAnd, target, &n.value(), loc)?
            }
            prism::Node::ConstantOperatorWriteNode { .. } => {
                let n = node.as_constant_operator_write_node().unwrap();
                let target = Node {
                    kind: NodeKind::Const {
                        toplevel: false,
                        parent: None,
                        prefix: vec![],
                        name: constant_name(&n.name())?,
                    },
                    loc: location_to_loc(&n.name_loc()),
                };
                self.build_op_assign(target, &n.binary_operator(), &n.value(), loc)?
            }
            prism::Node::ConstantOrWriteNode { .. } => {
                let n = node.as_constant_or_write_node().unwrap();
                let target = Node {
                    kind: NodeKind::Const {
                        toplevel: false,
                        parent: None,
                        prefix: vec![],
                        name: constant_name(&n.name())?,
                    },
                    loc: location_to_loc(&n.name_loc()),
                };
                self.build_short_circuit_assign(BinOp::LOr, target, &n.value(), loc)?
            }
            prism::Node::LambdaNode { .. } => self.lower_lambda(&node.as_lambda_node().unwrap())?,
            prism::Node::ConstantAndWriteNode { .. } => {
                let n = node.as_constant_and_write_node().unwrap();
                let target = Node {
                    kind: NodeKind::Const {
                        toplevel: false,
                        parent: None,
                        prefix: vec![],
                        name: constant_name(&n.name())?,
                    },
                    loc: location_to_loc(&n.name_loc()),
                };
                self.build_short_circuit_assign(BinOp::LAnd, target, &n.value(), loc)?
            }
            prism::Node::IndexOperatorWriteNode { .. } => {
                let n = node.as_index_operator_write_node().unwrap();
                if n.block().is_some() {
                    return Err(self.unsupported_node("indexed op-assign with block argument", loc));
                }
                let target = self.build_index_target(
                    n.receiver().as_ref(),
                    n.arguments().as_ref(),
                    location_to_loc(&n.location()),
                )?;
                self.build_op_assign(target, &n.binary_operator(), &n.value(), loc)?
            }
            prism::Node::IndexOrWriteNode { .. } => {
                let n = node.as_index_or_write_node().unwrap();
                if n.block().is_some() {
                    return Err(self.unsupported_node("indexed ||= with block argument", loc));
                }
                let target = self.build_index_target(
                    n.receiver().as_ref(),
                    n.arguments().as_ref(),
                    location_to_loc(&n.location()),
                )?;
                self.build_short_circuit_assign(BinOp::LOr, target, &n.value(), loc)?
            }
            prism::Node::IndexAndWriteNode { .. } => {
                let n = node.as_index_and_write_node().unwrap();
                if n.block().is_some() {
                    return Err(self.unsupported_node("indexed &&= with block argument", loc));
                }
                let target = self.build_index_target(
                    n.receiver().as_ref(),
                    n.arguments().as_ref(),
                    location_to_loc(&n.location()),
                )?;
                self.build_short_circuit_assign(BinOp::LAnd, target, &n.value(), loc)?
            }
            prism::Node::CallOperatorWriteNode { .. } => {
                let n = node.as_call_operator_write_node().unwrap();
                let target = self.build_attr_target(
                    n.receiver().as_ref(),
                    &n.read_name(),
                    n.is_safe_navigation(),
                    location_to_loc(&n.location()),
                )?;
                self.build_op_assign(target, &n.binary_operator(), &n.value(), loc)?
            }
            prism::Node::CallOrWriteNode { .. } => {
                let n = node.as_call_or_write_node().unwrap();
                let target = self.build_attr_target(
                    n.receiver().as_ref(),
                    &n.read_name(),
                    n.is_safe_navigation(),
                    location_to_loc(&n.location()),
                )?;
                self.build_short_circuit_assign(BinOp::LOr, target, &n.value(), loc)?
            }
            prism::Node::CallAndWriteNode { .. } => {
                let n = node.as_call_and_write_node().unwrap();
                let target = self.build_attr_target(
                    n.receiver().as_ref(),
                    &n.read_name(),
                    n.is_safe_navigation(),
                    location_to_loc(&n.location()),
                )?;
                self.build_short_circuit_assign(BinOp::LAnd, target, &n.value(), loc)?
            }
            prism::Node::ConstantPathWriteNode { .. } => {
                let n = node.as_constant_path_write_node().unwrap();
                let target_path = n.target();
                let target = self.lower_const_chain(&target_path.as_node())?;
                let value = self.lower_node(&n.value())?;
                Node {
                    kind: NodeKind::MulAssign(vec![target], vec![value]),
                    loc,
                }
            }
            prism::Node::ConstantPathOperatorWriteNode { .. } => {
                let n = node.as_constant_path_operator_write_node().unwrap();
                let target = self.lower_const_chain(&n.target().as_node())?;
                self.build_op_assign(target, &n.binary_operator(), &n.value(), loc)?
            }
            prism::Node::ConstantPathOrWriteNode { .. } => {
                let n = node.as_constant_path_or_write_node().unwrap();
                let target = self.lower_const_chain(&n.target().as_node())?;
                self.build_short_circuit_assign(BinOp::LOr, target, &n.value(), loc)?
            }
            prism::Node::ConstantPathAndWriteNode { .. } => {
                let n = node.as_constant_path_and_write_node().unwrap();
                let target = self.lower_const_chain(&n.target().as_node())?;
                self.build_short_circuit_assign(BinOp::LAnd, target, &n.value(), loc)?
            }
            prism::Node::SuperNode { .. } => {
                let n = node.as_super_node().unwrap();
                let mut args: Vec<Node> = vec![];
                let mut kw_args: Vec<(String, Node)> = vec![];
                let mut hash_splat: Vec<Node> = vec![];
                let mut forwarding = false;
                if let Some(prism_args) = n.arguments() {
                    for arg in prism_args.arguments().iter() {
                        self.collect_call_arg(
                            &arg,
                            &mut args,
                            &mut kw_args,
                            &mut hash_splat,
                            &mut forwarding,
                        )?;
                    }
                }
                let has_splat = args.iter().any(|a| matches!(a.kind, NodeKind::Splat(_)));
                let mut arglist = crate::ast::ArgList::default();
                arglist.args = args;
                arglist.kw_args = kw_args;
                arglist.hash_splat = hash_splat;
                arglist.forwarding = forwarding;
                arglist.splat = has_splat;
                if let Some(block_node) = n.block() {
                    match self.lower_call_block(&block_node)? {
                        CallBlock::Block(b) => arglist.block = Some(Box::new(b)),
                        CallBlock::Delegate => arglist.delegate_block = true,
                    }
                }
                Node {
                    kind: NodeKind::Super(Some(Box::new(arglist))),
                    loc,
                }
            }
            prism::Node::ForwardingSuperNode { .. } => {
                // `super` (no parens) — forwards the enclosing
                // method's arguments. ruruby uses `Super(None)` for
                // exactly this case. A literal block can still be
                // attached (`super { ... }`); ruruby models that as
                // `Super(Some(ArgList { args: [], block: Some(...) }))`
                // — empty positional args + a block — and bytecodegen
                // treats empty-args as the forwarding case
                // regardless of the block slot.
                let n = node.as_forwarding_super_node().unwrap();
                let kind = match n.block() {
                    None => NodeKind::Super(None),
                    Some(block_node) => {
                        // ForwardingSuperNode.block() is the
                        // typed `BlockNode` (literal `{ ... }` /
                        // `do ... end`); a `BlockArgumentNode`
                        // (`super(&blk)`) goes through the regular
                        // SuperNode path. So we can hand it
                        // straight to `lower_block`.
                        let body = self.lower_block(&block_node)?;
                        let mut arglist = crate::ast::ArgList::default();
                        arglist.block = Some(Box::new(body));
                        NodeKind::Super(Some(Box::new(arglist)))
                    }
                };
                Node { kind, loc }
            }
            prism::Node::YieldNode { .. } => {
                let n = node.as_yield_node().unwrap();
                let mut arglist = crate::ast::ArgList::default();
                let mut args: Vec<Node> = vec![];
                let mut kw_args: Vec<(String, Node)> = vec![];
                let mut hash_splat: Vec<Node> = vec![];
                let mut forwarding = false;
                if let Some(prism_args) = n.arguments() {
                    for arg in prism_args.arguments().iter() {
                        self.collect_call_arg(
                            &arg,
                            &mut args,
                            &mut kw_args,
                            &mut hash_splat,
                            &mut forwarding,
                        )?;
                    }
                }
                let has_splat = args.iter().any(|a| matches!(a.kind, NodeKind::Splat(_)));
                arglist.args = args;
                arglist.kw_args = kw_args;
                arglist.hash_splat = hash_splat;
                arglist.forwarding = forwarding;
                arglist.splat = has_splat;
                Node {
                    kind: NodeKind::Yield(Box::new(arglist)),
                    loc,
                }
            }
            prism::Node::ForNode { .. } => {
                let n = node.as_for_node().unwrap();
                let index = n.index();
                // `prepend` is a destructuring assignment to run at the top
                // of the body when the index can't be expressed as a flat
                // list of leaked loop variables (splat / post / nested /
                // non-local targets).
                let (param, prepend): (Vec<(usize, String)>, Option<Node>) = match index {
                    prism::Node::LocalVariableTargetNode { .. } => {
                        let inner = index.as_local_variable_target_node().unwrap();
                        (
                            {
                                let name = constant_name(&inner.name())?;
                                let depth =
                                    self.adjust_lvar_depth(inner.depth() as usize, &name);
                                vec![(depth, name)]
                            },
                            None,
                        )
                    }
                    // `for a, b in ...` / `for (a, b) in ...` lands
                    // as MultiTargetNode. ruruby flattens this into
                    // a multi-element `param: Vec<(depth, name)>`.
                    prism::Node::MultiTargetNode { .. } => {
                        let mt = index.as_multi_target_node().unwrap();
                        let mt_loc = location_to_loc(&index.location());
                        // Fast path: a flat list of plain locals maps directly
                        // to leaked loop variables.
                        let flat = mt.rest().is_none()
                            && mt.rights().iter().next().is_none()
                            && mt.lefts().iter().all(|t| {
                                matches!(t, prism::Node::LocalVariableTargetNode { .. })
                            });
                        if flat {
                            let mut out: Vec<(usize, String)> = Vec::new();
                            for tgt in mt.lefts().iter() {
                                let inner = tgt.as_local_variable_target_node().unwrap();
                                out.push({
                                    let name = constant_name(&inner.name())?;
                                    let depth = self
                                        .adjust_lvar_depth(inner.depth() as usize, &name);
                                    (depth, name)
                                });
                            }
                            (out, None)
                        } else {
                            // Desugar to a single hidden loop variable plus a
                            // destructuring assignment; the ordinary MulAssign
                            // machinery handles rest / post / nested targets.
                            let lefts: Vec<_> = mt.lefts().iter().collect();
                            let rights: Vec<_> = mt.rights().iter().collect();
                            let mlhs = self.lower_target_list(&lefts, mt.rest(), &rights)?;
                            let read = Node {
                                kind: NodeKind::LocalVar(0, FOR_INDEX_NAME.to_owned()),
                                loc: mt_loc,
                            };
                            let assign = Node {
                                kind: NodeKind::MulAssign(mlhs, vec![read]),
                                loc: mt_loc,
                            };
                            (vec![(0, FOR_INDEX_NAME.to_owned())], Some(assign))
                        }
                    }
                    // A single non-local target (`for @x in ...`,
                    // `for CONST in ...`, `for a[i] in ...`): desugar to the
                    // hidden loop variable plus a plain assignment.
                    other => {
                        let tgt = self.lower_assign_target(&other)?;
                        let t_loc = tgt.loc;
                        let read = Node {
                            kind: NodeKind::LocalVar(0, FOR_INDEX_NAME.to_owned()),
                            loc: t_loc,
                        };
                        let assign = Node {
                            kind: NodeKind::MulAssign(vec![tgt], vec![read]),
                            loc: t_loc,
                        };
                        (vec![(0, FOR_INDEX_NAME.to_owned())], Some(assign))
                    }
                };
                let iter = self.lower_node(&n.collection())?;
                let body_loc = match n.statements() {
                    Some(s) => location_to_loc(&s.location()),
                    None => loc,
                };
                let body = match n.statements() {
                    Some(s) => self.lower_statements_compact(&s, body_loc)?,
                    None => Node {
                        kind: NodeKind::Nil,
                        loc: body_loc,
                    },
                };
                let body = match prepend {
                    Some(assign) => Node {
                        kind: NodeKind::CompStmt(vec![assign, body]),
                        loc: body_loc,
                    },
                    None => body,
                };
                // ruruby's `for` body lives in a `BlockInfo` whose
                // own LvarCollector is empty — the loop variable is
                // visible to the surrounding scope, not introduced
                // here. Build a fresh empty collector to match.
                Node {
                    kind: NodeKind::For {
                        param,
                        iter: Box::new(iter),
                        body: Box::new(BlockInfo {
                            params: vec![],
                            body: Box::new(body),
                            lvar: LvarCollector::new(),
                            loc: body_loc,
                            is_lambda: false,
                        }),
                    },
                    loc,
                }
            }
            prism::Node::BreakNode { .. } => {
                let n = node.as_break_node().unwrap();
                Node {
                    kind: NodeKind::Break(Box::new(self.lower_jump_value(n.arguments(), loc)?)),
                    loc,
                }
            }
            prism::Node::NextNode { .. } => {
                let n = node.as_next_node().unwrap();
                Node {
                    kind: NodeKind::Next(Box::new(self.lower_jump_value(n.arguments(), loc)?)),
                    loc,
                }
            }
            prism::Node::RedoNode { .. } => Node {
                kind: NodeKind::Redo,
                loc,
            },
            prism::Node::RetryNode { .. } => Node {
                kind: NodeKind::Retry,
                loc,
            },
            prism::Node::DefinedNode { .. } => {
                let n = node.as_defined_node().unwrap();
                let value = n.value();
                // `defined?($x ||= v)` must classify as "assignment", so
                // lower the or-write directly (without the definedness
                // guard used for evaluation, which would make it an If
                // node and classify as "expression"). defined? never
                // evaluates assignments, so the uninitialized-global
                // warning the guard suppresses cannot fire here.
                let inner = if let Some(gw) = value.as_global_variable_or_write_node() {
                    let target = Node {
                        kind: NodeKind::GlobalVar(constant_name(&gw.name())?),
                        loc: location_to_loc(&gw.name_loc()),
                    };
                    let inner_loc = location_to_loc(&value.location());
                    self.build_short_circuit_assign(BinOp::LOr, target, &gw.value(), inner_loc)?
                } else {
                    self.lower_node(&value)?
                };
                Node {
                    kind: NodeKind::Defined(Box::new(inner)),
                    loc,
                }
            }
            prism::Node::AliasMethodNode { .. } => {
                let n = node.as_alias_method_node().unwrap();
                let new_name = self.lower_node(&n.new_name())?;
                let old_name = self.lower_node(&n.old_name())?;
                Node {
                    kind: NodeKind::AliasMethod(Box::new(new_name), Box::new(old_name)),
                    loc,
                }
            }
            prism::Node::AliasGlobalVariableNode { .. } => {
                // `alias $foo $bar` — ruruby reuses `AliasMethod` and
                // expects its operands to be `Symbol("$foo") /
                // Symbol("$bar")`; bytecodegen pattern-matches on the
                // `$` prefix to emit `AliasGvar` instead of
                // `AliasMethod`. Convert the Prism
                // `GlobalVariableReadNode`s accordingly.
                let n = node.as_alias_global_variable_node().unwrap();
                let new_name = global_var_alias_target(&n.new_name())?;
                let old_name = global_var_alias_target(&n.old_name())?;
                Node {
                    kind: NodeKind::AliasMethod(Box::new(new_name), Box::new(old_name)),
                    loc,
                }
            }
            prism::Node::UndefNode { .. } => {
                let n = node.as_undef_node().unwrap();
                // ruruby's `UndefMethod(Box<Node>)` only carries a
                // single name; Prism's UndefNode can list multiple
                // (`undef foo, bar`). Wrap each in its own UndefMethod
                // and emit them as a CompStmt so bytecodegen sees
                // them in source order.
                let mut nodes: Vec<Node> = Vec::new();
                for name in n.names().iter() {
                    let lowered = self.lower_node(&name)?;
                    let inner_loc = lowered.loc;
                    nodes.push(Node {
                        kind: NodeKind::UndefMethod(Box::new(lowered)),
                        loc: inner_loc,
                    });
                }
                match nodes.len() {
                    0 => Node {
                        kind: NodeKind::Nil,
                        loc,
                    },
                    1 => nodes.pop().unwrap(),
                    _ => Node {
                        kind: NodeKind::CompStmt(nodes),
                        loc,
                    },
                }
            }
            prism::Node::CaseNode { .. } => {
                let n = node.as_case_node().unwrap();
                let cond = match n.predicate() {
                    Some(p) => Some(Box::new(self.lower_node(&p)?)),
                    None => None,
                };
                let mut when_branches: Vec<crate::ast::CaseBranch> = Vec::new();
                for w in n.conditions().iter() {
                    match w {
                        prism::Node::WhenNode { .. } => {
                            let wn = w.as_when_node().unwrap();
                            let mut conds: Vec<Node> = Vec::new();
                            for c in wn.conditions().iter() {
                                conds.push(self.lower_node(&c)?);
                            }
                            let body_loc = match wn.statements() {
                                Some(s) => location_to_loc(&s.location()),
                                None => location_to_loc(&wn.location()),
                            };
                            let body = match wn.statements() {
                                Some(s) => self.lower_statements_compact(&s, body_loc)?,
                                None => Node {
                                    kind: NodeKind::Nil,
                                    loc: body_loc,
                                },
                            };
                            when_branches.push(crate::ast::CaseBranch {
                                when: conds,
                                body: Box::new(body),
                            });
                        }
                        // `case ... in pattern` arms (CaseMatchNode)
                        // produce `InNode`s instead; not supported here.
                        other => return Err(self.unsupported("case branch", &other)),
                    }
                }
                let else_branch = match n.else_clause() {
                    Some(e) => {
                        let stmts = e.statements();
                        Box::new(self.lower_optional_statements(
                            stmts.as_ref(),
                            location_to_loc(&e.location()),
                        )?)
                    }
                    None => Box::new(Node {
                        kind: NodeKind::Nil,
                        loc,
                    }),
                };
                Node {
                    kind: NodeKind::Case {
                        cond,
                        when_: when_branches,
                        else_: else_branch,
                    },
                    loc,
                }
            }
            prism::Node::CaseMatchNode { .. } => {
                let n = node.as_case_match_node().unwrap();
                self.lower_case_match(&n, loc)?
            }
            prism::Node::MatchPredicateNode { .. } => {
                // `expr in pattern` — strict boolean result.
                let n = node.as_match_predicate_node().unwrap();
                let subj_name = self.pm_temp_name();
                let subj_assign = pm_assign(
                    pm_lvar(&subj_name, loc),
                    self.lower_node(&n.value())?,
                );
                let subject = pm_lvar(&subj_name, loc);
                let matched = self.lower_pattern(&n.pattern(), &subject, None)?;
                Node::new_comp_stmt(
                    vec![
                        subj_assign,
                        Node::new_if(
                            matched,
                            Node::new_bool(true, loc),
                            Node::new_bool(false, loc),
                            loc,
                        ),
                    ],
                    loc,
                )
            }
            prism::Node::MatchRequiredNode { .. } => {
                // `expr => pattern` — raises NoMatchingPatternError on
                // mismatch, evaluates to nil.
                let n = node.as_match_required_node().unwrap();
                let subj_name = self.pm_temp_name();
                let subj_assign = pm_assign(
                    pm_lvar(&subj_name, loc),
                    self.lower_node(&n.value())?,
                );
                let subject = pm_lvar(&subj_name, loc);
                let matched = self.lower_pattern(&n.pattern(), &subject, None)?;
                Node::new_comp_stmt(
                    vec![
                        subj_assign,
                        Node::new_if(
                            matched,
                            Node::new_nil(loc),
                            pm_raise_no_matching_pattern(&subject, loc),
                            loc,
                        ),
                        Node::new_nil(loc),
                    ],
                    loc,
                )
            }
            prism::Node::InstanceVariableReadNode { .. } => {
                let n = node.as_instance_variable_read_node().unwrap();
                self.lower_named_var_read(NodeKind::InstanceVar, &n.name(), &n.location())?
            }
            prism::Node::InstanceVariableWriteNode { .. } => {
                let n = node.as_instance_variable_write_node().unwrap();
                self.lower_named_var_write(
                    NodeKind::InstanceVar,
                    &n.name(),
                    &n.name_loc(),
                    &n.value(),
                    &n.location(),
                )?
            }
            prism::Node::GlobalVariableReadNode { .. } => {
                let n = node.as_global_variable_read_node().unwrap();
                self.lower_named_var_read(NodeKind::GlobalVar, &n.name(), &n.location())?
            }
            prism::Node::GlobalVariableWriteNode { .. } => {
                let n = node.as_global_variable_write_node().unwrap();
                self.lower_named_var_write(
                    NodeKind::GlobalVar,
                    &n.name(),
                    &n.name_loc(),
                    &n.value(),
                    &n.location(),
                )?
            }
            prism::Node::ClassVariableReadNode { .. } => {
                let n = node.as_class_variable_read_node().unwrap();
                Node {
                    kind: NodeKind::ClassVar(constant_name(&n.name())?),
                    loc,
                }
            }
            prism::Node::ClassVariableWriteNode { .. } => {
                let n = node.as_class_variable_write_node().unwrap();
                let target = Node {
                    kind: NodeKind::ClassVar(constant_name(&n.name())?),
                    loc: location_to_loc(&n.name_loc()),
                };
                let value = self.lower_node(&n.value())?;
                Node {
                    kind: NodeKind::MulAssign(vec![target], vec![value]),
                    loc,
                }
            }
            prism::Node::NumberedReferenceReadNode { .. } => {
                // `$1` / `$2` etc. ruruby treats these as ordinary
                // global variables ($1 lookups eventually go through
                // the regex match data); match that.
                let n = node.as_numbered_reference_read_node().unwrap();
                Node {
                    kind: NodeKind::GlobalVar(format!("${}", n.number())),
                    loc,
                }
            }
            prism::Node::BackReferenceReadNode { .. } => {
                // `$~`, `$&`, `$'`, `` $` ``, `$+` — ruruby exposes
                // these as plain global variables with the sigil
                // included.
                let n = node.as_back_reference_read_node().unwrap();
                Node {
                    kind: NodeKind::GlobalVar(constant_name(&n.name())?),
                    loc,
                }
            }
            prism::Node::SourceFileNode { .. } => {
                // `__FILE__` — ruruby's parser inlines the source
                // path as a `String` literal at parse time, so do the
                // same here.
                Node {
                    kind: NodeKind::String(self.path.clone()),
                    loc,
                }
            }
            prism::Node::SourceLineNode { .. } => {
                // `__LINE__` — likewise inlined as the (1-based) line
                // number computed by counting newlines up to the
                // start offset, then adding the eval line offset
                // (0 for ordinary file parses, `lineno - 1` for
                // `eval(_, _, _, lineno)`).
                let off = node.location().start_offset();
                let line = source_line_for_offset(self.source, off) as i64 + self.line_offset;
                Node {
                    kind: NodeKind::Integer(line),
                    loc,
                }
            }
            prism::Node::SourceEncodingNode { .. } => {
                // ruruby parses `__ENCODING__` as a bare identifier
                // (`NodeKind::Ident("__ENCODING__")`) which bytecodegen
                // turns into a method call resolved at runtime.
                Node {
                    kind: NodeKind::Ident("__ENCODING__".to_string()),
                    loc,
                }
            }
            prism::Node::RescueModifierNode { .. } => {
                // `expr rescue value` — desugar to a `Begin` with a
                // single bare-rescue clause that catches the default
                // `StandardError` family. ruruby uses the same
                // shape (empty exception_list, no assign).
                let n = node.as_rescue_modifier_node().unwrap();
                let body = self.lower_node(&n.expression())?;
                let rescue_body = self.lower_node(&n.rescue_expression())?;
                let entry = crate::ast::RescueEntry {
                    exception_list: vec![],
                    assign: None,
                    body: Box::new(rescue_body),
                };
                Node {
                    kind: NodeKind::Begin {
                        body: Box::new(body),
                        rescue: vec![entry],
                        else_: None,
                        ensure: None,
                    },
                    loc,
                }
            }
            prism::Node::AndNode { .. } => {
                let inner = node.as_and_node().unwrap();
                let lhs = self.lower_node(&inner.left())?;
                let rhs = self.lower_node(&inner.right())?;
                Node {
                    kind: NodeKind::BinOp(BinOp::LAnd, Box::new(lhs), Box::new(rhs)),
                    loc,
                }
            }
            prism::Node::OrNode { .. } => {
                let inner = node.as_or_node().unwrap();
                let lhs = self.lower_node(&inner.left())?;
                let rhs = self.lower_node(&inner.right())?;
                Node {
                    kind: NodeKind::BinOp(BinOp::LOr, Box::new(lhs), Box::new(rhs)),
                    loc,
                }
            }
            prism::Node::ImplicitNode { .. } => {
                let implicit = node.as_implicit_node().unwrap();
                return self.lower_node(&implicit.value());
            }
            prism::Node::ShareableConstantNode { .. } => {
                let sc = node.as_shareable_constant_node().unwrap();
                return self.lower_node(&sc.write());
            }
            other => return Err(self.unsupported("expression", other)),
        })
    }

    fn lower_integer(&self, node: &IntegerNode<'pr>) -> Node {
        let value = node.value();
        let (negative, digits) = value.to_u32_digits();
        let loc = location_to_loc(&node.location());
        if digits.is_empty() {
            return Node {
                kind: NodeKind::Integer(0),
                loc,
            };
        }
        if digits.len() == 1 {
            let v = digits[0] as i64;
            let signed = if negative { -v } else { v };
            return Node {
                kind: NodeKind::Integer(signed),
                loc,
            };
        }
        if digits.len() == 2 {
            let lo = digits[0] as u64;
            let hi = digits[1] as u64;
            let combined = (hi << 32) | lo;
            if combined <= i64::MAX as u64 {
                let signed = if negative {
                    -(combined as i64)
                } else {
                    combined as i64
                };
                return Node {
                    kind: NodeKind::Integer(signed),
                    loc,
                };
            }
            if negative && combined == (1u64 << 63) {
                return Node {
                    kind: NodeKind::Integer(i64::MIN),
                    loc,
                };
            }
        }
        let mut bytes: Vec<u8> = Vec::with_capacity(digits.len() * 4);
        for &d in digits {
            bytes.extend_from_slice(&d.to_le_bytes());
        }
        let mag = num::BigInt::from_bytes_le(num::bigint::Sign::Plus, &bytes);
        let bi = if negative { -mag } else { mag };
        Node {
            kind: NodeKind::Bignum(bi),
            loc,
        }
    }

    fn lower_float(&self, node: &FloatNode<'pr>) -> Node {
        Node {
            kind: NodeKind::Float(node.value()),
            loc: location_to_loc(&node.location()),
        }
    }

    fn lower_string(&self, node: &StringNode<'pr>) -> Node {
        let bytes: &[u8] = node.unescaped();
        let loc = location_to_loc(&node.location());
        // CRuby upgrades a non-UTF-8 source's literal to UTF-8 the
        // moment it contains a `\u` escape (the bytes the escape
        // produces are valid UTF-8, so they no longer fit the source
        // encoding). Prism flags this via
        // `PM_STRING_FLAGS_FORCED_UTF8_ENCODING`; surface it as the
        // dedicated `EncodedString` variant so bytecodegen tags the
        // literal UTF-8 regardless of the file's `# encoding:`.
        // Likewise `forced_binary` (rare) overrides to ASCII-8BIT.
        if node.is_forced_utf8_encoding() {
            return Node {
                kind: NodeKind::EncodedString(bytes.to_vec(), "UTF-8"),
                loc,
            };
        }
        if node.is_forced_binary_encoding() {
            return Node {
                kind: NodeKind::EncodedString(bytes.to_vec(), "ASCII-8BIT"),
                loc,
            };
        }
        match std::str::from_utf8(bytes) {
            Ok(s) => Node {
                kind: NodeKind::String(s.to_owned()),
                loc,
            },
            Err(_) => Node {
                kind: NodeKind::Bytes(bytes.to_vec()),
                loc,
            },
        }
    }

    fn lower_symbol(&self, node: &SymbolNode<'pr>) -> Node {
        let bytes: &[u8] = node.unescaped();
        let s = std::str::from_utf8(bytes)
            .map(str::to_owned)
            .unwrap_or_else(|_| String::from_utf8_lossy(bytes).into_owned());
        Node {
            kind: NodeKind::Symbol(s),
            loc: location_to_loc(&node.location()),
        }
    }

    fn lower_local_var_read(&mut self, node: &LocalVariableReadNode<'pr>) -> Result<Node, MonorubyErr> {
        let name = constant_name(&node.name())?;
        Ok(Node {
            kind: NodeKind::LocalVar(self.adjust_lvar_depth(node.depth() as usize, &name), name),
            loc: location_to_loc(&node.location()),
        })
    }

    fn lower_local_var_write(
        &mut self,
        node: &LocalVariableWriteNode<'pr>,
    ) -> Result<Node, MonorubyErr> {
        let name = constant_name(&node.name())?;
        let depth = self.adjust_lvar_depth(node.depth() as usize, &name);
        let target = Node {
            kind: NodeKind::LocalVar(depth, name),
            loc: location_to_loc(&node.name_loc()),
        };
        let value = self.lower_node(&node.value())?;
        Ok(Node {
            kind: NodeKind::MulAssign(vec![target], vec![value]),
            loc: location_to_loc(&node.location()),
        })
    }

    fn lower_optional_statements(
        &mut self,
        stmts: Option<&StatementsNode<'pr>>,
        loc: Loc,
    ) -> Result<Node, MonorubyErr> {
        match stmts {
            Some(s) => Ok(Node {
                kind: NodeKind::CompStmt(self.lower_statements_into_vec(s)?),
                loc,
            }),
            None => Ok(Node {
                kind: NodeKind::Nil,
                loc,
            }),
        }
    }

    fn lower_if(&mut self, node: &IfNode<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        let cond = self.lower_node(&node.predicate())?;
        let stmts = node.statements();
        let then_ = self.lower_optional_statements(stmts.as_ref(), loc)?;
        let else_ = match node.subsequent() {
            Some(sub) => match sub {
                prism::Node::IfNode { .. } => self.lower_if(&sub.as_if_node().unwrap())?,
                prism::Node::ElseNode { .. } => {
                    let else_node = sub.as_else_node().unwrap();
                    let inner = else_node.statements();
                    self.lower_optional_statements(
                        inner.as_ref(),
                        location_to_loc(&else_node.location()),
                    )?
                }
                other => return Err(self.unsupported("if subsequent", &other)),
            },
            None => Node {
                kind: NodeKind::Nil,
                loc,
            },
        };
        Ok(Node {
            kind: NodeKind::If {
                cond: Box::new(cond),
                then_: Box::new(then_),
                else_: Box::new(else_),
            },
            loc,
        })
    }

    fn lower_unless(&mut self, node: &UnlessNode<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        let cond = self.lower_node(&node.predicate())?;
        let stmts = node.statements();
        let then_for_unless = self.lower_optional_statements(stmts.as_ref(), loc)?;
        let else_for_unless = match node.else_clause() {
            Some(e) => {
                let inner = e.statements();
                self.lower_optional_statements(inner.as_ref(), location_to_loc(&e.location()))?
            }
            None => Node {
                kind: NodeKind::Nil,
                loc,
            },
        };
        // ruruby has no separate `Unless` variant; it represents
        // `unless cond ; A ; else ; B ; end` as
        // `If { cond, then_: B, else_: A }` with the branches swapped.
        Ok(Node {
            kind: NodeKind::If {
                cond: Box::new(cond),
                then_: Box::new(else_for_unless),
                else_: Box::new(then_for_unless),
            },
            loc,
        })
    }

    fn lower_while(&mut self, node: &WhileNode<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        let cond = self.lower_node(&node.predicate())?;
        let stmts = node.statements();
        let body = self.lower_optional_statements(stmts.as_ref(), loc)?;
        // bytecodegen's `gen_while_begin_postfix` (do-while semantics —
        // run body once before testing the condition) is only selected
        // when the body is `NodeKind::Begin { .. }`. Prism's
        // `is_begin_modifier()` already tells us we're in
        // `begin...end while cond` form, so wrap the body to match
        // ruruby's shape and unblock that path.
        let body = if node.is_begin_modifier() {
            Node {
                kind: NodeKind::Begin {
                    body: Box::new(body),
                    rescue: vec![],
                    else_: None,
                    ensure: None,
                },
                loc,
            }
        } else {
            body
        };
        Ok(Node {
            kind: NodeKind::While {
                cond: Box::new(cond),
                body: Box::new(body),
                cond_op: true,
                postfix: node.is_begin_modifier(),
            },
            loc,
        })
    }

    fn lower_until(&mut self, node: &UntilNode<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        let cond = self.lower_node(&node.predicate())?;
        let stmts = node.statements();
        let body = self.lower_optional_statements(stmts.as_ref(), loc)?;
        let body = if node.is_begin_modifier() {
            Node {
                kind: NodeKind::Begin {
                    body: Box::new(body),
                    rescue: vec![],
                    else_: None,
                    ensure: None,
                },
                loc,
            }
        } else {
            body
        };
        Ok(Node {
            kind: NodeKind::While {
                cond: Box::new(cond),
                body: Box::new(body),
                cond_op: false,
                postfix: node.is_begin_modifier(),
            },
            loc,
        })
    }

    fn lower_array(&mut self, node: &ArrayNode<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        let mut elements: Vec<Node> = Vec::new();
        let mut all_const = true;
        for n in node.elements().iter() {
            // `[1, foo: 2, bar: 3]` — Prism gathers the trailing bare
            // keyword pairs into a `KeywordHashNode`; in an array
            // context they collapse into a single trailing Hash
            // element (`[1, {foo: 2, bar: 3}]`).
            let lowered = if let prism::Node::KeywordHashNode { .. } = n {
                let kh = n.as_keyword_hash_node().unwrap();
                let kh_loc = location_to_loc(&kh.location());
                let mut pairs: Vec<(Node, Node)> = Vec::new();
                let mut splat: Vec<(usize, Node)> = Vec::new();
                for elem in kh.elements().iter() {
                    match elem {
                        prism::Node::AssocNode { .. } => {
                            let assoc = elem.as_assoc_node().unwrap();
                            let key = self.lower_node(&assoc.key())?;
                            let value = self.lower_node(&assoc.value())?;
                            pairs.push((key, value));
                        }
                        prism::Node::AssocSplatNode { .. } => {
                            let s = elem.as_assoc_splat_node().unwrap();
                            let inner = match s.value() {
                                Some(v) => self.lower_node(&v)?,
                                None => Node {
                                    kind: NodeKind::Nil,
                                    loc: location_to_loc(&s.location()),
                                },
                            };
                            splat.push((pairs.len(), inner));
                        }
                        other => return Err(self.unsupported("array kwarg element", &other)),
                    }
                }
                Node {
                    kind: NodeKind::Hash(pairs, splat),
                    loc: kh_loc,
                }
            } else {
                self.lower_node(&n)?
            };
            if !is_constant_literal(&lowered.kind) {
                all_const = false;
            }
            elements.push(lowered);
        }
        Ok(Node {
            kind: NodeKind::Array(elements, all_const),
            loc,
        })
    }

    fn lower_hash(&mut self, node: &HashNode<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        let mut pairs: Vec<(Node, Node)> = Vec::new();
        // Each `**` operand records how many ordinary `k: v` pairs precede
        // it, so `gen_hash` can replay the elements strictly left-to-right
        // (later entries overwrite earlier ones — `{a: 1, **h, c: 4}`).
        let mut splat: Vec<(usize, Node)> = Vec::new();
        for elem in node.elements().iter() {
            match elem {
                prism::Node::AssocNode { .. } => {
                    let assoc = elem.as_assoc_node().unwrap();
                    let key = self.lower_node(&assoc.key())?;
                    let value = self.lower_node(&assoc.value())?;
                    pairs.push((key, value));
                }
                prism::Node::AssocSplatNode { .. } => {
                    let s = elem.as_assoc_splat_node().unwrap();
                    let inner = match s.value() {
                        Some(v) => self.lower_node(&v)?,
                        None => Node {
                            kind: NodeKind::Nil,
                            loc: location_to_loc(&s.location()),
                        },
                    };
                    splat.push((pairs.len(), inner));
                }
                other => return Err(self.unsupported("hash element", &other)),
            }
        }
        Ok(Node {
            kind: NodeKind::Hash(pairs, splat),
            loc,
        })
    }

    fn lower_range(&mut self, node: &RangeNode<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        let start = match node.left() {
            Some(n) => Some(self.lower_node(&n)?),
            None => None,
        };
        let end = match node.right() {
            Some(n) => Some(self.lower_node(&n)?),
            None => None,
        };
        // Match ruruby's narrower constness check (`is_integer &&
        // is_integer`, where `is_integer` covers Integer + Bignum).
        // Pre-folding heterogeneous-type ranges like `9155.."s"` would
        // silently swallow the runtime ArgumentError CRuby raises.
        let is_const = matches!(
            (
                start.as_ref().map(|n| &n.kind),
                end.as_ref().map(|n| &n.kind)
            ),
            (
                Some(NodeKind::Integer(_) | NodeKind::Bignum(_)),
                Some(NodeKind::Integer(_) | NodeKind::Bignum(_)),
            ),
        );
        Ok(Node {
            kind: NodeKind::Range {
                start: Box::new(start),
                end: Box::new(end),
                exclude_end: node.is_exclude_end(),
                is_const,
            },
            loc,
        })
    }

    /// Lower a flip-flop (`(a)..(b)` in a conditional) by desugaring
    /// it into an `if` chain over a hidden global variable that
    /// carries the per-site on/off state:
    ///
    /// ```text
    /// if $__flip_flop_N          # currently on
    ///   $__flip_flop_N = false if (b)
    ///   true
    /// elsif (a)                  # currently off, begin-condition hit
    ///   $__flip_flop_N = <`..`: !(b), `...`: true>
    ///   true
    /// else
    ///   false
    /// end
    /// ```
    ///
    /// Each lowered flip-flop draws a fresh id from a process-global
    /// counter, so distinct sites — including re-`eval`s of the same
    /// source — get independent state, while every evaluation of one
    /// parsed site shares it (an unset global reads as nil = off).
    /// CRuby instead keeps the state in the frame's special storage;
    /// the observable difference (thread isolation) doesn't matter in
    /// monoruby's single-threaded runtime.
    fn lower_flip_flop(
        &mut self,
        node: &prism::FlipFlopNode<'pr>,
    ) -> Result<Node, MonorubyErr> {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static NEXT_FLIP_FLOP_ID: AtomicUsize = AtomicUsize::new(0);
        let loc = location_to_loc(&node.location());
        let state = format!(
            "$__flip_flop_{}",
            NEXT_FLIP_FLOP_ID.fetch_add(1, Ordering::Relaxed)
        );
        let left = self.lower_flip_flop_operand(node.left(), loc)?;
        let right = self.lower_flip_flop_operand(node.right(), loc)?;
        let state_var = || Node::new_global_var(state.clone(), loc);
        let set_state = |value: Node| Node::new_mul_assign(vec![state_var()], vec![value]);
        // On: evaluate the end condition; a hit turns the flip-flop
        // off after this (still truthy) evaluation.
        let on_arm = Node::new_comp_stmt(
            vec![
                Node::new_if(
                    right.clone(),
                    set_state(Node::new_bool(false, loc)),
                    Node::new_nil(loc),
                    loc,
                ),
                Node::new_bool(true, loc),
            ],
            loc,
        );
        // Off + begin-condition hit: `..` also evaluates the end
        // condition immediately (a simultaneous hit keeps it off),
        // `...` defers it to the next evaluation.
        let turn_on = if node.is_exclude_end() {
            Node::new_bool(true, loc)
        } else {
            Node::new_unop(UnOp::Not, right, loc)
        };
        let off_arm = Node::new_if(
            left,
            Node::new_comp_stmt(vec![set_state(turn_on), Node::new_bool(true, loc)], loc),
            Node::new_bool(false, loc),
            loc,
        );
        Ok(Node::new_if(state_var(), on_arm, off_arm, loc))
    }

    /// A bare Integer literal as a flip-flop operand compares against
    /// `$.` (the last input line number) in CRuby, not its own
    /// truthiness. A missing operand can never hit (nil).
    fn lower_flip_flop_operand(
        &mut self,
        node: Option<prism::Node<'pr>>,
        loc: Loc,
    ) -> Result<Node, MonorubyErr> {
        let Some(node) = node else {
            return Ok(Node::new_nil(loc));
        };
        let lowered = self.lower_node(&node)?;
        Ok(
            if matches!(lowered.kind, NodeKind::Integer(_) | NodeKind::Bignum(_)) {
                Node::new_binop(
                    BinOp::Cmp(CmpKind::Eq),
                    lowered,
                    Node::new_global_var("$.".to_string(), loc),
                )
            } else {
                lowered
            },
        )
    }

    /// Lower `END { body }`: register the body to run at process
    /// exit, once per site even if the END statement is executed
    /// repeatedly:
    ///
    /// ```text
    /// unless $__end_block_N   # once-only gate, fresh id per site
    ///   $__end_block_N = true
    ///   at_exit { <body> }
    /// end
    /// ```
    ///
    /// CRuby compiles the body as a block sharing the enclosing
    /// scope; the hidden `at_exit` block does the same, with the
    /// `scope_wraps` bookkeeping re-aiming local variable references
    /// across the synthesized closure scope (prism reports depths
    /// that don't know about it).
    fn lower_post_execution(
        &mut self,
        node: &prism::PostExecutionNode<'pr>,
        loc: Loc,
    ) -> Result<Node, MonorubyErr> {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static NEXT_END_BLOCK_ID: AtomicUsize = AtomicUsize::new(0);
        let gate = format!(
            "$__end_block_{}",
            NEXT_END_BLOCK_ID.fetch_add(1, Ordering::Relaxed)
        );
        self.scope_wraps.push(ScopeWrap {
            level: self.prism_scope_level,
            escaped: Vec::new(),
        });
        let body = match node.statements() {
            Some(stmts) => self.lower_statements_compact(&stmts, loc),
            None => Ok(Node::new_nil(loc)),
        };
        let escaped = self.scope_wraps.pop().unwrap().escaped;
        let body = body?;
        let block = Node::new(
            NodeKind::Lambda(Box::new(BlockInfo::new(
                Vec::new(),
                body,
                LvarCollector::new(),
                loc,
            ))),
            loc,
        );
        let arglist = crate::ast::ArgList {
            block: Some(Box::new(block)),
            ..Default::default()
        };
        let register = Node::new_comp_stmt(
            vec![
                Node::new_mul_assign(
                    vec![Node::new_global_var(gate.clone(), loc)],
                    vec![Node::new_bool(true, loc)],
                ),
                Node::new_fcall("at_exit".to_string(), arglist, false, loc),
                Node::new_nil(loc),
            ],
            loc,
        );
        let gated = Node::new_if(
            Node::new_global_var(gate, loc),
            Node::new_nil(loc),
            register,
            loc,
        );
        if escaped.is_empty() {
            return Ok(gated);
        }
        // Anchor every local the body referenced across the wrap with
        // a bare read at the END statement's position: bytecodegen
        // materializes local slots lazily on first same-scope
        // reference, and these variables' home scope is out here, not
        // inside the hidden block. The reads evaluate to nil and are
        // discarded. Re-adjusting the depth (0 = home scope in
        // prism's model) also re-records the names on any still-open
        // outer wrap, propagating anchors for nested END bodies.
        let mut stmts: Vec<Node> = escaped
            .into_iter()
            .map(|name| {
                let depth = self.adjust_lvar_depth(0, &name);
                Node::new_lvar(name, depth, loc)
            })
            .collect();
        stmts.push(gated);
        Ok(Node::new_comp_stmt(stmts, loc))
    }

    /// Lower a `StatementsNode`-shaped body to ruruby-parse's compact
    /// shape:
    ///
    ///   - empty body  -> `CompStmt([])`
    ///   - 1 statement -> bare expression (NO `CompStmt` wrapper)
    ///   - N>=2        -> `CompStmt([s1, s2, ...])`
    ///
    /// Plain `lower_node(StatementsNode)` always wraps in `CompStmt`,
    /// which (a) throws off class-body register arithmetic in
    /// bytecodegen and (b) hides the inner kind from
    /// `bytecodegen::hint()`, suppressing trivial-method
    /// `ConstReturn` annotation. Both class-body and method-body
    /// lowering route through here so the shape stays uniform with
    /// ruruby-parse.
    fn lower_compact_body(
        &mut self,
        body: Option<prism::Node<'pr>>,
        fallback_loc: Loc,
    ) -> Result<Node, MonorubyErr> {
        match body {
            Some(b) => match b {
                prism::Node::StatementsNode { .. } => {
                    let stmts_node = b.as_statements_node().unwrap();
                    let stmts_loc = location_to_loc(&stmts_node.location());
                    let mut stmts = self.lower_statements_into_vec(&stmts_node)?;
                    Ok(match stmts.len() {
                        0 => Node {
                            kind: NodeKind::CompStmt(vec![]),
                            loc: stmts_loc,
                        },
                        1 => stmts.pop().unwrap(),
                        _ => Node {
                            kind: NodeKind::CompStmt(stmts),
                            loc: stmts_loc,
                        },
                    })
                }
                _ => self.lower_node(&b),
            },
            None => Ok(Node {
                kind: NodeKind::CompStmt(vec![]),
                loc: fallback_loc,
            }),
        }
    }

    /// Lower the body of a class / module / class-shovel definition,
    /// returning a fully-built `BlockInfo` with the body wrapped in
    /// the implicit `Begin { rescue: [], else_: None, ensure: None }`
    /// envelope ruruby-parse produces.
    fn lower_class_body(
        &mut self,
        body: Option<prism::Node<'pr>>,
        loc: Loc,
    ) -> Result<BlockInfo, MonorubyErr> {
        let saved = self.enter_prism_scope();
        let body_res = self.lower_compact_body(body, loc);
        match body_res {
            Ok(body_inner) => {
                let body = Node {
                    kind: NodeKind::Begin {
                        body: Box::new(body_inner),
                        rescue: vec![],
                        else_: None,
                        ensure: None,
                    },
                    loc,
                };
                let class_lvars = self.exit_prism_scope(saved);
                Ok(BlockInfo {
                    params: vec![],
                    body: Box::new(body),
                    lvar: class_lvars,
                    loc,
                    is_lambda: false,
                })
            }
            Err(e) => {
                self.exit_prism_scope(saved);
                Err(e)
            }
        }
    }

    fn lower_class(&mut self, node: &ClassNode<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        let (base, name) = self.split_class_path(&node.constant_path())?;
        let superclass = match node.superclass() {
            Some(s) => Some(Box::new(self.lower_node(&s)?)),
            None => None,
        };
        // The class body's `locals` list lives on the ClassNode itself
        // (Prism scopes it to the class definition). Seed the lowerer
        // with those before lowering the body.
        let saved = self.enter_prism_scope();
        if let Err(e) = self.collect_locals(&node.locals()) {
            self.exit_prism_scope(saved);
            return Err(e);
        }
        let info_res = self.lower_class_body(node.body(), loc);
        // `lower_class_body` already swapped lvars back — restore the
        // outer scope from `saved` here regardless of outcome.
        self.exit_prism_scope(saved);
        let info = info_res?;
        Ok(Node {
            kind: NodeKind::ClassDef {
                base,
                name,
                superclass,
                info: Box::new(info),
                is_module: false,
            },
            loc,
        })
    }

    fn lower_module(&mut self, node: &ModuleNode<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        let (base, name) = self.split_class_path(&node.constant_path())?;
        let saved = self.enter_prism_scope();
        if let Err(e) = self.collect_locals(&node.locals()) {
            self.exit_prism_scope(saved);
            return Err(e);
        }
        let info_res = self.lower_class_body(node.body(), loc);
        self.exit_prism_scope(saved);
        let info = info_res?;
        Ok(Node {
            kind: NodeKind::ClassDef {
                base,
                name,
                superclass: None,
                info: Box::new(info),
                is_module: true,
            },
            loc,
        })
    }

    /// Splits the class/module's `constant_path` into ruruby's
    /// `(base: Option<Box<Node>>, name: String)` form.
    /// `class A; end` => `(None, "A")`. `class M::N::A; end` =>
    /// `(Some(Const{prefix:["M"], name:"N"}), "A")`.
    fn split_class_path(
        &mut self,
        path: &prism::Node<'pr>,
    ) -> Result<(Option<Box<Node>>, String), MonorubyErr> {
        match path {
            prism::Node::ConstantReadNode { .. } => {
                let n = path.as_constant_read_node().unwrap();
                Ok((None, constant_name(&n.name())?))
            }
            prism::Node::ConstantPathNode { .. } => {
                let n = path.as_constant_path_node().unwrap();
                let name = match n.name() {
                    Some(id) => constant_name(&id)?,
                    None => {
                        return Err(self.unsupported_node(
                            "class path missing name",
                            location_to_loc(&path.location()),
                        ));
                    }
                };
                let base = match n.parent() {
                    Some(parent) => {
                        // `class A::B::C` — pure constant chain, hand
                        // off to the const-chain flattener. For
                        // dynamic prefixes (`class expr::C`,
                        // including `class parent::C` where `parent`
                        // is a local variable, or `class self::C`),
                        // fall back to a regular value lowering;
                        // bytecodegen accepts any expression as the
                        // class's base scope.
                        let parent_loc = location_to_loc(&parent.location());
                        match self.collect_const_chain(&parent)? {
                            Some(chain) => Some(Box::new(Node {
                                kind: NodeKind::Const {
                                    toplevel: chain.toplevel,
                                    parent: chain.parent,
                                    prefix: chain.prefix,
                                    name: chain.name,
                                },
                                loc: parent_loc,
                            })),
                            None => Some(Box::new(self.lower_node(&parent)?)),
                        }
                    }
                    // `class ::Foo; end`: the leading `::` binds `Foo` at
                    // the top level (on Object) regardless of the enclosing
                    // lexical scope, so give it an explicit `Object` base.
                    // Without this, `module M; class ::Foo; end; end` would
                    // wrongly create `M::Foo`.
                    None => {
                        let path_loc = location_to_loc(&path.location());
                        Some(Box::new(Node {
                            kind: NodeKind::Const {
                                toplevel: true,
                                parent: None,
                                prefix: vec![],
                                name: "Object".to_string(),
                            },
                            loc: path_loc,
                        }))
                    }
                };
                Ok((base, name))
            }
            other => Err(self.unsupported("class path", other)),
        }
    }

    fn lower_constant_read(&self, node: &ConstantReadNode<'pr>) -> Result<Node, MonorubyErr> {
        Ok(Node {
            kind: NodeKind::Const {
                toplevel: false,
                parent: None,
                prefix: vec![],
                name: constant_name(&node.name())?,
            },
            loc: location_to_loc(&node.location()),
        })
    }

    fn lower_constant_path(&mut self, node: &ConstantPathNode<'pr>) -> Result<Node, MonorubyErr> {
        self.lower_const_chain(&node.as_node())
    }

    /// Flatten a `ConstantPathNode` chain (or the leaf `ConstantReadNode`
    /// it terminates on) into ruruby's `Const { toplevel, parent,
    /// prefix, name }` representation. Pure constant-only chains like
    /// `A::B::C` flatten into `prefix: ["A", "B"], name: "C"`. Chains
    /// rooted at `::` flatten with `toplevel: true`. Chains rooted at
    /// a non-constant expression (e.g. `expr::C`) become
    /// `parent: Some(<lowered expr>)`.
    /// Lower a `ConstantReadNode` / `ConstantPathNode` into the
    /// `Const { toplevel, parent, prefix, name }` shape ruruby uses
    /// for constant references. The Prism grammar guarantees every
    /// caller that types this signature receives a constant path
    /// node, so a non-constant root would be a parser bug — we
    /// surface that as a `SyntaxError` rather than silently returning
    /// a non-Const node.
    fn lower_const_chain(&mut self, node: &prism::Node<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        let chain = self
            .collect_const_chain(node)?
            .ok_or_else(|| self.unsupported_node("non-constant constant path prefix", loc))?;
        Ok(Node {
            kind: NodeKind::Const {
                toplevel: chain.toplevel,
                parent: chain.parent,
                prefix: chain.prefix,
                name: chain.name,
            },
            loc,
        })
    }

    /// `Ok(Some(_))` — chain successfully assembled (purely
    /// constant, or rooted in a value expression that we lowered
    /// into `parent`). `Ok(None)` — the input itself isn't a
    /// constant node, so a caller in a place that *also* accepts
    /// arbitrary expressions (e.g. `class expr::C` base, or `m::Foo`
    /// in a write target) should fall back to a regular value
    /// lowering.
    fn collect_const_chain(
        &mut self,
        node: &prism::Node<'pr>,
    ) -> Result<Option<ConstChain>, MonorubyErr> {
        match node {
            prism::Node::ConstantReadNode { .. } => {
                let n = node.as_constant_read_node().unwrap();
                Ok(Some(ConstChain {
                    toplevel: false,
                    parent: None,
                    prefix: vec![],
                    name: constant_name(&n.name())?,
                }))
            }
            prism::Node::ConstantPathNode { .. } => {
                let n = node.as_constant_path_node().unwrap();
                let name = match n.name() {
                    Some(id) => constant_name(&id)?,
                    None => {
                        return Err(self.unsupported_node(
                            "constant path missing name",
                            location_to_loc(&node.location()),
                        ));
                    }
                };
                match n.parent() {
                    None => Ok(Some(ConstChain {
                        toplevel: true,
                        parent: None,
                        prefix: vec![],
                        name,
                    })),
                    Some(p) => match self.collect_const_chain(&p)? {
                        Some(mut inner) => {
                            // Slide the inner leaf into `prefix`,
                            // replace with `name`. Same mutation in
                            // both the pure-constant case and the
                            // already-non-const-rooted case.
                            inner.prefix.push(std::mem::take(&mut inner.name));
                            inner.name = name;
                            Ok(Some(inner))
                        }
                        // Non-constant parent: lower it as a value
                        // expression and root the chain on it.
                        None => {
                            let parent_node = self.lower_node(&p)?;
                            Ok(Some(ConstChain {
                                toplevel: false,
                                parent: Some(Box::new(parent_node)),
                                prefix: vec![],
                                name,
                            }))
                        }
                    },
                }
            }
            // Anything else: signal "not a constant chain" so the
            // caller falls back to a value lowering.
            _ => Ok(None),
        }
    }

    /// `return` (no value) -> `Return(Nil)`.
    /// `return x` -> `Return(<x>)`.
    /// `return x, y` -> `Return(Array([x, y], is_const))` (matching the
    /// shape ruruby-parse produces — it always wraps the multi-value
    /// case in an Array).
    /// Generic helper for `InstanceVariableReadNode` / `GlobalVariableReadNode`,
    /// both of which carry an interned `name` (already including the
    /// `@`/`$` sigil) plus a location. The same `Const` / `Param` /
    /// `Splat` shape is built by passing the appropriate `NodeKind`
    /// variant constructor.
    fn lower_named_var_read(
        &self,
        kind: fn(String) -> NodeKind,
        name: &ConstantId<'pr>,
        loc: &Location<'pr>,
    ) -> Result<Node, MonorubyErr> {
        Ok(Node {
            kind: kind(constant_name(name)?),
            loc: location_to_loc(loc),
        })
    }

    fn lower_named_var_write(
        &mut self,
        kind: fn(String) -> NodeKind,
        name: &ConstantId<'pr>,
        name_loc: &Location<'pr>,
        value: &prism::Node<'pr>,
        full_loc: &Location<'pr>,
    ) -> Result<Node, MonorubyErr> {
        let target = Node {
            kind: kind(constant_name(name)?),
            loc: location_to_loc(name_loc),
        };
        let value = self.lower_node(value)?;
        Ok(Node {
            kind: NodeKind::MulAssign(vec![target], vec![value]),
            loc: location_to_loc(full_loc),
        })
    }

    /// `"foo #{expr} bar #@x"` -> `InterporatedString([String("foo "),
    /// <expr>, String(" bar "), InstanceVar("@x")])`. ruruby drops
    /// embed wrappers entirely: a single-statement `#{...}` inlines
    /// the expression directly, multi-statement `#{a; b}` collapses
    /// into a `CompStmt`. `#@x` / `#$x` / `#@@x` short-form embeds
    /// arrive as `EmbeddedVariableNode`s that we forward to
    /// `lower_node` (which already knows how to lower the variable).
    /// Lowers `begin ... rescue ... else ... ensure ... end` to
    /// ruruby's `Begin { body, rescue, else_, ensure }`. Prism stores
    /// the `rescue` chain as a singly-linked list via
    /// `RescueNode::subsequent`; we walk it and collect each clause
    /// into a `RescueEntry`.
    /// Lowers the `block` slot of a `CallNode` / `SuperNode` — Prism
    /// stores either a literal `BlockNode` (`{ ... }`) or a
    /// `BlockArgumentNode` (`&proc`) there, and ruruby plugs both
    /// into `arglist.block` differentiated by NodeKind. The
    /// anonymous-& form (`foo(&)`) is reported separately via
    /// [`CallBlock::Delegate`] so the caller can flip
    /// `arglist.delegate_block`.
    fn lower_call_block(
        &mut self,
        block_node: &prism::Node<'pr>,
    ) -> Result<CallBlock, MonorubyErr> {
        match block_node {
            prism::Node::BlockNode { .. } => {
                let bn = block_node.as_block_node().unwrap();
                Ok(CallBlock::Block(self.lower_block(&bn)?))
            }
            prism::Node::BlockArgumentNode { .. } => {
                let ba = block_node.as_block_argument_node().unwrap();
                match ba.expression() {
                    Some(e) => Ok(CallBlock::Block(self.lower_node(&e)?)),
                    // `foo(&)` — anonymous forward of the outer
                    // method's block.
                    None => Ok(CallBlock::Delegate),
                }
            }
            other => Err(self.unsupported("call block", other)),
        }
    }

    /// Build the `MethodCall { receiver, method, arglist: empty }`
    /// Lower one call-site argument. Routes regular expressions into
    /// `args`, `KeywordHashNode` elements into `kw_args` /
    /// `hash_splat`, and `ForwardingArgumentsNode` (`g(...)`) into the
    /// `forwarding` flag — mirroring the way ruruby's parser flattens
    /// these onto an `ArgList`.
    fn collect_call_arg(
        &mut self,
        n: &prism::Node<'pr>,
        args: &mut Vec<Node>,
        kw_args: &mut Vec<(String, Node)>,
        hash_splat: &mut Vec<Node>,
        forwarding: &mut bool,
    ) -> Result<(), MonorubyErr> {
        match n {
            // `g(...)` — forward the enclosing method's args.
            prism::Node::ForwardingArgumentsNode { .. } => {
                *forwarding = true;
            }
            // `g(a: 1, **opts)` — Prism gathers trailing keyword
            // pairs into a `KeywordHashNode`. ruruby instead splits
            // them onto `arglist.kw_args` (static-keyed) and
            // `arglist.hash_splat` (`**` splats).
            prism::Node::KeywordHashNode { .. } => {
                let kh = n.as_keyword_hash_node().unwrap();
                let kh_loc = location_to_loc(&kh.location());
                // ruruby splits trailing keyword pairs into
                // `arglist.kw_args` (static-symbol keys) and
                // `arglist.hash_splat` (`**` splats); a key that
                // isn't a static symbol literal — string keys
                // (`f("a" => 1)`), interpolated keys, dynamic keys,
                // etc. — doesn't fit that shape, so we fall back
                // to the same shape ruruby produces in that case:
                // build a single `Hash(...)` value and pass it as a
                // regular positional arg.
                let mut all_static_symbol = true;
                for elem in kh.elements().iter() {
                    if let prism::Node::AssocNode { .. } = elem {
                        let assoc = elem.as_assoc_node().unwrap();
                        if !matches!(assoc.key(), prism::Node::SymbolNode { .. }) {
                            all_static_symbol = false;
                            break;
                        }
                    }
                }
                if !all_static_symbol {
                    // Mixed-key trailing hash (`a: 1, 100 => 100`).
                    // ruruby pushes a single `Hash(pairs, splats)`
                    // node onto `arglist.hash_splat` (NOT `args`,
                    // and NOT `kw_args` — the non-symbol key
                    // doesn't fit kwargs's `(String, Node)` shape).
                    // bytecodegen treats a `hash_splat` entry as a
                    // `**hash` argument; CRuby semantics for
                    // `foo(a: 1, 100 => 100)` are equivalent to
                    // `foo(**{a: 1, 100 => 100})` so the runtime
                    // passes the whole hash to the method's
                    // `**kw` slot (or, if there is none, falls
                    // back to a trailing positional hash).
                    let mut pairs: Vec<(Node, Node)> = Vec::new();
                    let mut inner_splat: Vec<(usize, Node)> = Vec::new();
                    for elem in kh.elements().iter() {
                        match elem {
                            prism::Node::AssocNode { .. } => {
                                let assoc = elem.as_assoc_node().unwrap();
                                let k = self.lower_node(&assoc.key())?;
                                let v = self.lower_node(&assoc.value())?;
                                pairs.push((k, v));
                            }
                            prism::Node::AssocSplatNode { .. } => {
                                let s = elem.as_assoc_splat_node().unwrap();
                                let inner = match s.value() {
                                    Some(v) => self.lower_node(&v)?,
                                    None => Node {
                                        kind: NodeKind::Nil,
                                        loc: location_to_loc(&s.location()),
                                    },
                                };
                                inner_splat.push((pairs.len(), inner));
                            }
                            other => return Err(self.unsupported("kwarg element", &other)),
                        }
                    }
                    hash_splat.push(Node {
                        kind: NodeKind::Hash(pairs, inner_splat),
                        loc: kh_loc,
                    });
                    return Ok(());
                }
                for elem in kh.elements().iter() {
                    match elem {
                        prism::Node::AssocNode { .. } => {
                            let assoc = elem.as_assoc_node().unwrap();
                            let key = assoc.key();
                            let s = key.as_symbol_node().unwrap();
                            let bytes = s.unescaped();
                            let key_loc = location_to_loc(&key.location());
                            if bytes.is_empty() {
                                return Err(
                                    self.unsupported_node("empty keyword arg name", key_loc)
                                );
                            }
                            let name =
                                std::str::from_utf8(bytes).map(str::to_owned).map_err(|_| {
                                    self.unsupported_node("non-utf8 keyword arg name", key_loc)
                                })?;
                            let value = self.lower_node(&assoc.value())?;
                            kw_args.push((name, value));
                        }
                        prism::Node::AssocSplatNode { .. } => {
                            let s = elem.as_assoc_splat_node().unwrap();
                            let inner = match s.value() {
                                Some(v) => self.lower_node(&v)?,
                                // Anonymous `**` (forwarding `foo(**)`): splat
                                // the reserved anonymous-kwrest local bound by
                                // the enclosing `def m(**)`.
                                None => Node {
                                    kind: NodeKind::LocalVar(0, ANON_KWREST_NAME.to_owned()),
                                    loc: location_to_loc(&s.location()),
                                },
                            };
                            hash_splat.push(inner);
                        }
                        other => return Err(self.unsupported("kwarg element", &other)),
                    }
                }
            }
            // Splat (`*expr`) is already handled in `lower_node` and
            // ends up as `Splat(...)` in args; the `arglist.splat`
            // flag is set later by detecting it.
            _ => args.push(self.lower_node(n)?),
        }
        Ok(())
    }

    /// shape used as the target of `obj.attr += ...` / `obj.attr ||=
    /// ...`. This mirrors what ruruby's parser does for the LHS of an
    /// attribute op-assign — bytecodegen later rewrites the call into
    /// the corresponding setter (`attr=`) when it emits the
    /// assignment.
    fn build_attr_target(
        &mut self,
        receiver: Option<&prism::Node<'pr>>,
        read_name: &ConstantId<'pr>,
        safe_nav: bool,
        loc: Loc,
    ) -> Result<Node, MonorubyErr> {
        let recv = receiver.ok_or(self.unsupported_node("attr op-assign without receiver", loc))?;
        let receiver_node = self.lower_node(recv)?;
        let method = constant_name(read_name)?;
        Ok(Node {
            kind: NodeKind::MethodCall {
                receiver: Box::new(receiver_node),
                method,
                arglist: Box::new(crate::ast::ArgList::default()),
                safe_nav,
            },
            loc,
        })
    }

    /// Build the `Index { base, index }` shape used as the target of
    /// `a[i] += ...` / `a[i] ||= ...` / `a[i, j] = ...`. Shared
    /// between the operator-write nodes, the multi-assign LHS path
    /// (via `IndexTargetNode`), and the `[]` / `[]=` short-form
    /// detected inside `lower_call`.
    fn build_index_target(
        &mut self,
        receiver: Option<&prism::Node<'pr>>,
        args: Option<&prism::ArgumentsNode<'pr>>,
        loc: Loc,
    ) -> Result<Node, MonorubyErr> {
        let recv = receiver.ok_or(self.unsupported_node("index target without receiver", loc))?;
        let base = self.lower_node(recv)?;
        let mut index: Vec<Node> = Vec::new();
        if let Some(a) = args {
            for arg in a.arguments().iter() {
                index.push(self.lower_node(&arg)?);
            }
        }
        Ok(Node {
            kind: NodeKind::Index {
                base: Box::new(base),
                index,
            },
            loc,
        })
    }

    /// `target OP= value` -> `AssignOp(BinOp, target, value)`.
    /// Used for the `*OperatorWriteNode` family (the `+=`, `-=`, ...
    /// shapes). The operator name comes back from Prism as the bare
    /// op (`"+"`, `"-"`, ...); we route it through the existing
    /// `binop_from_name` table.
    fn build_op_assign(
        &mut self,
        target: Node,
        op_id: &ConstantId<'pr>,
        value: &prism::Node<'pr>,
        loc: Loc,
    ) -> Result<Node, MonorubyErr> {
        let op_name = constant_name(op_id)?;
        let op = binop_from_name(&op_name)
            .ok_or(self.unsupported_node("unknown op-assign operator", loc))?;
        let value = self.lower_node(value)?;
        Ok(Node {
            kind: NodeKind::AssignOp(op, Box::new(target), Box::new(value)),
            loc,
        })
    }

    /// `target ||= value` -> `AssignOp(LOr, target, value)` and likewise
    /// for `&&=` -> `LAnd`. Prism reports these via `*OrWriteNode` /
    /// `*AndWriteNode`. Routing them through `AssignOp` (rather than a
    /// `BinOp(LOr, target, MulAssign([target], [value]))` wrapper) lets
    /// bytecodegen evaluate an attribute/index receiver exactly once for
    /// both the read and the conditional write — the wrapper form
    /// re-evaluated the receiver, so `a[side_effect] ||= v` ran the index
    /// twice. Bytecodegen still short-circuits: the setter fires only when
    /// the current value is falsy (`||=`) / truthy (`&&=`).
    fn build_short_circuit_assign(
        &mut self,
        op: BinOp,
        target: Node,
        value: &prism::Node<'pr>,
        loc: Loc,
    ) -> Result<Node, MonorubyErr> {
        let value = self.lower_node(value)?;
        Ok(Node {
            kind: NodeKind::AssignOp(op, Box::new(target), Box::new(value)),
            loc,
        })
    }

    /// `a, b = 1, 2` and friends. Prism's MultiWriteNode keeps three
    /// piece-wise lists (`lefts`, optional `rest`, `rights`); we
    /// concatenate them in source order, wrapping the rest target in
    /// `Splat(...)` to match ruruby's flat shape. The RHS shape is
    /// determined by the ArrayNode wrapper Prism inserts:
    /// `a, b = 1, 2` -> implicit (no `[`) -> unpack into the
    /// multi-RHS form `[1, 2]`. `a, b = [1, 2]` -> explicit -> single-
    /// element RHS `[Array([1,2])]`. Any non-array RHS (`a, b = c`)
    /// goes in unwrapped as a single-element RHS.
    /// Builds a flat `Vec<Node>` of assign targets from a
    /// `lefts` / optional `rest` / `rights` triple, the shared shape of
    /// both `MultiWriteNode` (top-level `a, b = ...`) and a nested
    /// `MultiTargetNode` (`(b, c)` inside a larger LHS). The rest is
    /// lowered to `Splat(target)` (`*a` / `*` → `Splat(DiscardLhs)`) or
    /// a bare `DiscardLhs` for a trailing-comma `ImplicitRestNode`.
    fn lower_target_list(
        &mut self,
        lefts: &[prism::Node<'pr>],
        rest: Option<prism::Node<'pr>>,
        rights: &[prism::Node<'pr>],
    ) -> Result<Vec<Node>, MonorubyErr> {
        let mut lhs: Vec<Node> = Vec::new();
        for n in lefts {
            lhs.push(self.lower_assign_target(n)?);
        }
        if let Some(rest) = rest {
            match rest {
                prism::Node::SplatNode { .. } => {
                    let inner = rest.as_splat_node().unwrap();
                    let rest_loc = location_to_loc(&inner.location());
                    let target = match inner.expression() {
                        Some(e) => self.lower_assign_target(&e)?,
                        // Anonymous splat target `*` swallows the rest
                        // without binding; ruruby uses `DiscardLhs` for
                        // this slot.
                        None => Node {
                            kind: NodeKind::DiscardLhs,
                            loc: rest_loc,
                        },
                    };
                    lhs.push(Node {
                        kind: NodeKind::Splat(Box::new(target)),
                        loc: rest_loc,
                    });
                }
                prism::Node::ImplicitRestNode { .. } => {
                    // `a, b, = arr` — ruruby produces a bare
                    // `DiscardLhs` for the trailing slot, NOT
                    // `Splat(DiscardLhs)`. Wrapping it in `Splat`
                    // turns the slot into a rest-position with no
                    // matching register, which corrupts
                    // `ExpandArray`'s output (e.g. `len=1, rest_pos=1`
                    // for `os, = arch`).
                    let rest_loc = location_to_loc(&rest.location());
                    lhs.push(Node {
                        kind: NodeKind::DiscardLhs,
                        loc: rest_loc,
                    });
                }
                other => return Err(self.unsupported("multi-write rest", &other)),
            }
        }
        for n in rights {
            lhs.push(self.lower_assign_target(n)?);
        }
        Ok(lhs)
    }

    fn lower_multi_write(&mut self, node: &MultiWriteNode<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        let lefts: Vec<_> = node.lefts().iter().collect();
        let rights: Vec<_> = node.rights().iter().collect();
        let lhs = self.lower_target_list(&lefts, node.rest(), &rights)?;

        let value = node.value();
        let rhs: Vec<Node> = match value {
            prism::Node::ArrayNode { .. } => {
                let arr = value.as_array_node().unwrap();
                let elements = arr.elements();
                let has_splat = elements
                    .iter()
                    .any(|e| matches!(e, prism::Node::SplatNode { .. }));
                if arr.opening_loc().is_none() && !has_splat {
                    // Implicit array (no `[...]`) with no splat: the
                    // source wrote `a, b = 1, 2` and Prism wrapped the
                    // RHS into an ArrayNode for the assignment. Unpack
                    // so each element becomes its own RHS slot,
                    // matching ruruby's `MulAssign(lhs, [v1, v2, ...])`
                    // form.
                    let mut out = Vec::new();
                    for elem in elements.iter() {
                        out.push(self.lower_node(&elem)?);
                    }
                    out
                } else {
                    // Explicit `a, b = [1, 2]` *or* implicit-with-splat
                    // (`a, b = *arg`, `a, b = 1, *rest`): keep the
                    // array as a single RHS so it gets splatted at
                    // runtime. ruruby's parser uses the same single-
                    // `Array` shape for both, with the splat element
                    // preserved inside.
                    let mut elems = Vec::new();
                    for elem in elements.iter() {
                        elems.push(self.lower_node(&elem)?);
                    }
                    let arr_loc = location_to_loc(&arr.location());
                    let is_const = elems.iter().all(|n| is_constant_literal(&n.kind));
                    vec![Node {
                        kind: NodeKind::Array(elems, is_const),
                        loc: arr_loc,
                    }]
                }
            }
            _ => vec![self.lower_node(&value)?],
        };

        Ok(Node {
            kind: NodeKind::MulAssign(lhs, rhs),
            loc,
        })
    }

    /// Lowers a `*TargetNode` (the assignment-LHS variants Prism uses
    /// inside `MultiWriteNode`/`MultiTargetNode`/etc.) onto the same
    /// `Local/Instance/Global/ClassVar/Const` shapes ruruby's parser
    /// would have produced for the LHS. Used by both `MultiWriteNode`
    /// and the rescue-target lowerer.
    // ----------------------------------------------------------------
    // Pattern matching (`case/in`, `expr => pat`, `expr in pat`)
    //
    // Desugared entirely into the core AST: `===` dispatch for value
    // patterns, `deconstruct` / `deconstruct_keys` calls for array and
    // hash patterns, local assignments for bindings, and
    // `raise NoMatchingPatternError` for exhaustion. Each pattern
    // compiles to a boolean expression (side-effecting bindings on the
    // way), so the whole construct becomes an if/elsif chain and JITs
    // like ordinary code — no VM support needed.
    // ----------------------------------------------------------------

    fn lower_case_match(
        &mut self,
        n: &prism::CaseMatchNode<'pr>,
        loc: Loc,
    ) -> Result<Node, MonorubyErr> {
        let subj_name = self.pm_temp_name();
        let cache_name = self.pm_temp_name();
        let subject = pm_lvar(&subj_name, loc);
        // `case/in` always has a predicate (a case-less `case` cannot
        // have `in` branches).
        let predicate = n.predicate().unwrap();
        let subj_assign = pm_assign(pm_lvar(&subj_name, loc), self.lower_node(&predicate)?);
        // Reset the per-case `deconstruct` cache: the hidden local
        // survives across executions of an enclosing loop.
        let cache_reset = pm_assign(pm_lvar(&cache_name, loc), Node::new_nil(loc));
        let mut chain = match n.else_clause() {
            Some(e) => {
                let stmts = e.statements();
                self.lower_optional_statements(stmts.as_ref(), location_to_loc(&e.location()))?
            }
            None => pm_raise_no_matching_pattern(&subject, loc),
        };
        let conditions: Vec<_> = n.conditions().iter().collect();
        for c in conditions.into_iter().rev() {
            let in_node = match &c {
                prism::Node::InNode { .. } => c.as_in_node().unwrap(),
                other => return Err(self.unsupported("case/in branch", other)),
            };
            let branch_loc = location_to_loc(&c.location());
            let pattern = in_node.pattern();
            let mut cond = self.lower_in_pattern(&pattern, &subject, Some(&cache_name))?;
            if let Some((guard, positive)) = split_pattern_guard(&pattern) {
                let mut g = self.lower_node(&guard)?;
                if !positive {
                    g = Node::new_unop(UnOp::Not, g, location_to_loc(&guard.location()));
                }
                cond = pm_and(cond, g);
            }
            let body = match in_node.statements() {
                Some(s) => {
                    let body_loc = location_to_loc(&s.location());
                    self.lower_statements_compact(&s, body_loc)?
                }
                None => Node::new_nil(branch_loc),
            };
            chain = Node::new_if(cond, body, chain, branch_loc);
        }
        Ok(Node::new_comp_stmt(
            vec![subj_assign, cache_reset, chain],
            loc,
        ))
    }

    /// Lower an `in` clause's pattern, skipping over a guard wrapper
    /// (handled separately by the caller).
    fn lower_in_pattern(
        &mut self,
        pattern: &prism::Node<'pr>,
        subject: &Node,
        cache: Option<&str>,
    ) -> Result<Node, MonorubyErr> {
        match pattern_guard_body(pattern) {
            Some(inner) => self.lower_pattern(&inner, subject, cache),
            None => self.lower_pattern(pattern, subject, cache),
        }
    }

    /// Compile one pattern against `subject` (a re-readable expression:
    /// a hidden local or an element access) into a boolean expression.
    /// `cache` names the per-case hidden local memoizing the top-level
    /// subject's `deconstruct` result across branches (CRuby calls
    /// `#deconstruct` once per case subject but `#deconstruct_keys`
    /// per pattern).
    fn lower_pattern(
        &mut self,
        pat: &prism::Node<'pr>,
        subject: &Node,
        cache: Option<&str>,
    ) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&pat.location());
        Ok(match pat {
            prism::Node::ArrayPatternNode { .. } => {
                let n = pat.as_array_pattern_node().unwrap();
                self.lower_array_pattern(&n, subject, cache, loc)?
            }
            prism::Node::FindPatternNode { .. } => {
                let n = pat.as_find_pattern_node().unwrap();
                self.lower_find_pattern(&n, subject, cache, loc)?
            }
            prism::Node::HashPatternNode { .. } => {
                let n = pat.as_hash_pattern_node().unwrap();
                self.lower_hash_pattern(&n, subject, loc)?
            }
            prism::Node::CapturePatternNode { .. } => {
                // `pat => name` — match, then bind.
                let n = pat.as_capture_pattern_node().unwrap();
                let matched = self.lower_pattern(&n.value(), subject, cache)?;
                let target = self.lower_assign_target(&n.target().as_node())?;
                pm_and(matched, pm_bind(target, subject.clone(), loc))
            }
            prism::Node::AlternationPatternNode { .. } => {
                let n = pat.as_alternation_pattern_node().unwrap();
                let l = self.lower_pattern(&n.left(), subject, cache)?;
                let r = self.lower_pattern(&n.right(), subject, cache)?;
                pm_or(l, r)
            }
            prism::Node::PinnedVariableNode { .. } => {
                let n = pat.as_pinned_variable_node().unwrap();
                pm_teq(self.lower_node(&n.variable())?, subject.clone())
            }
            prism::Node::PinnedExpressionNode { .. } => {
                let n = pat.as_pinned_expression_node().unwrap();
                pm_teq(self.lower_node(&n.expression())?, subject.clone())
            }
            prism::Node::LocalVariableTargetNode { .. } => {
                // `in name` — binds unconditionally.
                let target = self.lower_assign_target(pat)?;
                pm_bind(target, subject.clone(), loc)
            }
            prism::Node::ImplicitNode { .. } => {
                let n = pat.as_implicit_node().unwrap();
                self.lower_pattern(&n.value(), subject, cache)?
            }
            // Value pattern: any literal/const/range/regexp/pin —
            // `pattern === subject`.
            _ => pm_teq(self.lower_node(pat)?, subject.clone()),
        })
    }

    /// `subject.deconstruct` with the TypeError contract, optionally
    /// memoized in the per-case cache local. Produces a check node
    /// that assigns the hidden local `d_name` and evaluates truthy
    /// (or raises TypeError).
    fn pm_deconstructed(
        &mut self,
        subject: &Node,
        d_name: &str,
        cache: Option<&str>,
        loc: Loc,
    ) -> Node {
        let decon = Node::new_mcall_noarg(subject.clone(), "deconstruct".to_string(), false, loc);
        let type_check = |d: Node| {
            Node::new_if(
                pm_teq(pm_const("Array", loc), d),
                Node::new_bool(true, loc),
                pm_raise_type_error("deconstruct must return Array", loc),
                loc,
            )
        };
        match cache {
            Some(c) => {
                // if %cache.nil?
                //   %d = subject.deconstruct
                //   (TypeError check)
                //   %cache = %d
                // else
                //   %d = %cache
                // end
                // true
                let fill = Node::new_comp_stmt(
                    vec![
                        pm_assign(pm_lvar(d_name, loc), decon),
                        type_check(pm_lvar(d_name, loc)),
                        pm_assign(pm_lvar(c, loc), pm_lvar(d_name, loc)),
                    ],
                    loc,
                );
                let reuse = pm_assign(pm_lvar(d_name, loc), pm_lvar(c, loc));
                Node::new_comp_stmt(
                    vec![
                        Node::new_if(
                            Node::new_mcall_noarg(pm_lvar(c, loc), "nil?".to_string(), false, loc),
                            fill,
                            reuse,
                            loc,
                        ),
                        Node::new_bool(true, loc),
                    ],
                    loc,
                )
            }
            None => Node::new_comp_stmt(
                vec![
                    pm_assign(pm_lvar(d_name, loc), decon),
                    type_check(pm_lvar(d_name, loc)),
                ],
                loc,
            ),
        }
    }

    fn lower_array_pattern(
        &mut self,
        n: &prism::ArrayPatternNode<'pr>,
        subject: &Node,
        cache: Option<&str>,
        loc: Loc,
    ) -> Result<Node, MonorubyErr> {
        let mut checks: Vec<Node> = vec![];
        if let Some(c) = n.constant() {
            checks.push(pm_teq(self.lower_node(&c)?, subject.clone()));
        }
        checks.push(pm_respond_to(subject, "deconstruct", loc));
        let d_name = self.pm_temp_name();
        checks.push(self.pm_deconstructed(subject, &d_name, cache, loc));
        let d = |loc| pm_lvar(&d_name, loc);
        let req_len = n.requireds().iter().count();
        let posts_len = n.posts().iter().count();
        let fixed = (req_len + posts_len) as i64;
        let has_rest = n.rest().is_some();
        let size = Node::new_mcall_noarg(d(loc), "size".to_string(), false, loc);
        checks.push(Node::new_binop(
            BinOp::Cmp(if has_rest { CmpKind::Ge } else { CmpKind::Eq }),
            size,
            Node::new_integer(fixed, loc),
        ));
        for (i, p) in n.requireds().iter().enumerate() {
            let elem = Node::new_array_member(d(loc), vec![Node::new_integer(i as i64, loc)], loc);
            checks.push(self.lower_pattern(&p, &elem, None)?);
        }
        if let Some(rest) = n.rest() {
            // `*name` binds the middle slice; a bare `*` and the
            // trailing-comma `ImplicitRestNode` just allow extra
            // elements.
            if let prism::Node::SplatNode { .. } = &rest {
                let sp = rest.as_splat_node().unwrap();
                if let Some(expr) = sp.expression() {
                    let target = self.lower_assign_target(&expr)?;
                    let size =
                        Node::new_mcall_noarg(d(loc), "size".to_string(), false, loc);
                    let slice_len = Node::new_binop(
                        BinOp::Sub,
                        size,
                        Node::new_integer(fixed, loc),
                    );
                    let slice = Node::new_array_member(
                        d(loc),
                        vec![Node::new_integer(req_len as i64, loc), slice_len],
                        loc,
                    );
                    checks.push(pm_bind(target, slice, loc));
                }
            }
        }
        for (j, p) in n.posts().iter().enumerate() {
            let size = Node::new_mcall_noarg(d(loc), "size".to_string(), false, loc);
            let idx = Node::new_binop(
                BinOp::Sub,
                size,
                Node::new_integer((posts_len - j) as i64, loc),
            );
            let elem = Node::new_array_member(d(loc), vec![idx], loc);
            checks.push(self.lower_pattern(&p, &elem, None)?);
        }
        Ok(pm_and_all(checks, loc))
    }

    fn lower_find_pattern(
        &mut self,
        n: &prism::FindPatternNode<'pr>,
        subject: &Node,
        cache: Option<&str>,
        loc: Loc,
    ) -> Result<Node, MonorubyErr> {
        let mut checks: Vec<Node> = vec![];
        if let Some(c) = n.constant() {
            checks.push(pm_teq(self.lower_node(&c)?, subject.clone()));
        }
        checks.push(pm_respond_to(subject, "deconstruct", loc));
        let d_name = self.pm_temp_name();
        checks.push(self.pm_deconstructed(subject, &d_name, cache, loc));
        let d = |loc| pm_lvar(&d_name, loc);
        let k = n.requireds().iter().count() as i64;
        // %i = 0
        // %found = false
        // while %i <= %d.size - K
        //   if (required matches at offset %i)
        //     pre = %d[0, %i]; post = %d[%i + K, %d.size]
        //     %found = true
        //     break
        //   end
        //   %i = %i + 1
        // end
        // %found
        let i_name = self.pm_temp_name();
        let found_name = self.pm_temp_name();
        let i = |loc| pm_lvar(&i_name, loc);
        let found = |loc| pm_lvar(&found_name, loc);
        let mut arm: Vec<Node> = vec![];
        let mut elem_checks: Vec<Node> = vec![];
        for (j, p) in n.requireds().iter().enumerate() {
            let idx = if j == 0 {
                i(loc)
            } else {
                Node::new_binop(BinOp::Add, i(loc), Node::new_integer(j as i64, loc))
            };
            let elem = Node::new_array_member(d(loc), vec![idx], loc);
            elem_checks.push(self.lower_pattern(&p, &elem, None)?);
        }
        if let Some(expr) = n.left().expression() {
            let target = self.lower_assign_target(&expr)?;
            let slice =
                Node::new_array_member(d(loc), vec![Node::new_integer(0, loc), i(loc)], loc);
            arm.push(pm_assign(target, slice));
        }
        if let prism::Node::SplatNode { .. } = &n.right() {
            let sp = n.right().as_splat_node().unwrap();
            if let Some(expr) = sp.expression() {
                let target = self.lower_assign_target(&expr)?;
                let from = Node::new_binop(BinOp::Add, i(loc), Node::new_integer(k, loc));
                let size = Node::new_mcall_noarg(d(loc), "size".to_string(), false, loc);
                let slice = Node::new_array_member(d(loc), vec![from, size], loc);
                arm.push(pm_assign(target, slice));
            }
        }
        arm.push(pm_assign(found(loc), Node::new_bool(true, loc)));
        arm.push(Node {
            kind: NodeKind::Break(Box::new(Node::new_nil(loc))),
            loc,
        });
        let size = Node::new_mcall_noarg(d(loc), "size".to_string(), false, loc);
        let while_cond = Node::new_binop(
            BinOp::Cmp(CmpKind::Le),
            i(loc),
            Node::new_binop(BinOp::Sub, size, Node::new_integer(k, loc)),
        );
        let body = Node::new_comp_stmt(
            vec![
                Node::new_if(
                    pm_and_all(elem_checks, loc),
                    Node::new_comp_stmt(arm, loc),
                    Node::new_nil(loc),
                    loc,
                ),
                pm_assign(
                    i(loc),
                    Node::new_binop(BinOp::Add, i(loc), Node::new_integer(1, loc)),
                ),
            ],
            loc,
        );
        checks.push(Node::new_comp_stmt(
            vec![
                pm_assign(i(loc), Node::new_integer(0, loc)),
                pm_assign(found(loc), Node::new_bool(false, loc)),
                Node::new_while(while_cond, body, true, loc),
                found(loc),
            ],
            loc,
        ));
        Ok(pm_and_all(checks, loc))
    }

    fn lower_hash_pattern(
        &mut self,
        n: &prism::HashPatternNode<'pr>,
        subject: &Node,
        loc: Loc,
    ) -> Result<Node, MonorubyErr> {
        // Collect (symbol-name, value-pattern) pairs first — the keys
        // double as the `deconstruct_keys` argument.
        let mut pairs: Vec<(String, Option<prism::Node<'pr>>)> = vec![];
        for e in n.elements().iter() {
            let assoc = match &e {
                prism::Node::AssocNode { .. } => e.as_assoc_node().unwrap(),
                other => return Err(self.unsupported("hash pattern element", other)),
            };
            let key = assoc.key();
            let name = match &key {
                prism::Node::SymbolNode { .. } => {
                    let sym = key.as_symbol_node().unwrap();
                    String::from_utf8_lossy(sym.unescaped()).into_owned()
                }
                other => return Err(self.unsupported("hash pattern key", other)),
            };
            // `a:` alone wraps its implicit binding in ImplicitNode.
            pairs.push((name, Some(assoc.value())));
        }
        // rest: None | AssocSplatNode (`**` / `**rest`) |
        // NoKeywordsParameterNode (`**nil`).
        let mut no_extra_keys = false;
        let mut rest_target: Option<prism::Node<'pr>> = None;
        let mut pass_nil = pairs.is_empty();
        if let Some(rest) = n.rest() {
            match &rest {
                prism::Node::AssocSplatNode { .. } => {
                    let sp = rest.as_assoc_splat_node().unwrap();
                    if let Some(v) = sp.value() {
                        // `**rest` — deconstruct_keys receives nil.
                        rest_target = Some(v);
                        pass_nil = true;
                    }
                    // bare `**` — keys are still passed (CRuby).
                }
                prism::Node::NoKeywordsParameterNode { .. } => {
                    no_extra_keys = true;
                    pass_nil = true;
                }
                other => return Err(self.unsupported("hash pattern rest", other)),
            }
        } else if pairs.is_empty() {
            // `in {}` matches only an empty hash.
            no_extra_keys = true;
        }
        let mut checks: Vec<Node> = vec![];
        if let Some(c) = n.constant() {
            checks.push(pm_teq(self.lower_node(&c)?, subject.clone()));
        }
        checks.push(pm_respond_to(subject, "deconstruct_keys", loc));
        let h_name = self.pm_temp_name();
        let h = |loc| pm_lvar(&h_name, loc);
        let keys_arg = if pass_nil {
            Node::new_nil(loc)
        } else {
            Node::new_array(
                pairs
                    .iter()
                    .map(|(name, _)| Node::new_symbol(name.clone(), loc))
                    .collect(),
                loc,
            )
        };
        let decon = Node::new_mcall(
            subject.clone(),
            "deconstruct_keys".to_string(),
            crate::ast::ArgList::from_args(vec![keys_arg]),
            false,
            loc,
        );
        checks.push(Node::new_comp_stmt(
            vec![
                pm_assign(h(loc), decon),
                Node::new_if(
                    pm_teq(pm_const("Hash", loc), h(loc)),
                    Node::new_bool(true, loc),
                    pm_raise_type_error("deconstruct_keys must return Hash", loc),
                    loc,
                ),
            ],
            loc,
        ));
        for (name, value) in &pairs {
            checks.push(Node::new_mcall(
                h(loc),
                "key?".to_string(),
                crate::ast::ArgList::from_args(vec![Node::new_symbol(name.clone(), loc)]),
                false,
                loc,
            ));
            if let Some(v) = value {
                let elem = Node::new_array_member(
                    h(loc),
                    vec![Node::new_symbol(name.clone(), loc)],
                    loc,
                );
                checks.push(self.lower_pattern(v, &elem, None)?);
            }
        }
        if no_extra_keys {
            checks.push(Node::new_binop(
                BinOp::Cmp(CmpKind::Eq),
                Node::new_mcall_noarg(h(loc), "size".to_string(), false, loc),
                Node::new_integer(pairs.len() as i64, loc),
            ));
        }
        if let Some(target) = rest_target {
            // rest = %h.dup minus the matched keys.
            let target = self.lower_assign_target(&target)?;
            let rest_read = target.clone();
            let mut stmts = vec![pm_assign(
                target,
                Node::new_mcall_noarg(h(loc), "dup".to_string(), false, loc),
            )];
            for (name, _) in &pairs {
                stmts.push(Node::new_mcall(
                    rest_read.clone(),
                    "delete".to_string(),
                    crate::ast::ArgList::from_args(vec![Node::new_symbol(name.clone(), loc)]),
                    false,
                    loc,
                ));
            }
            stmts.push(Node::new_bool(true, loc));
            checks.push(Node::new_comp_stmt(stmts, loc));
        }
        Ok(pm_and_all(checks, loc))
    }

    fn lower_assign_target(&mut self, node: &prism::Node<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        Ok(match node {
            prism::Node::LocalVariableTargetNode { .. } => {
                let n = node.as_local_variable_target_node().unwrap();
                Node {
                    kind: {
                        let name = constant_name(&n.name())?;
                        let depth = self.adjust_lvar_depth(n.depth() as usize, &name);
                        NodeKind::LocalVar(depth, name)
                    },
                    loc,
                }
            }
            prism::Node::InstanceVariableTargetNode { .. } => {
                let n = node.as_instance_variable_target_node().unwrap();
                Node {
                    kind: NodeKind::InstanceVar(constant_name(&n.name())?),
                    loc,
                }
            }
            prism::Node::GlobalVariableTargetNode { .. } => {
                let n = node.as_global_variable_target_node().unwrap();
                Node {
                    kind: NodeKind::GlobalVar(constant_name(&n.name())?),
                    loc,
                }
            }
            prism::Node::ClassVariableTargetNode { .. } => {
                let n = node.as_class_variable_target_node().unwrap();
                Node {
                    kind: NodeKind::ClassVar(constant_name(&n.name())?),
                    loc,
                }
            }
            prism::Node::ConstantTargetNode { .. } => {
                let n = node.as_constant_target_node().unwrap();
                Node {
                    kind: NodeKind::Const {
                        toplevel: false,
                        parent: None,
                        prefix: vec![],
                        name: constant_name(&n.name())?,
                    },
                    loc,
                }
            }
            prism::Node::IndexTargetNode { .. } => {
                let n = node.as_index_target_node().unwrap();
                if n.block().is_some() {
                    return Err(self.unsupported_node("index target with block argument", loc));
                }
                let recv = n.receiver();
                self.build_index_target(Some(&recv), n.arguments().as_ref(), loc)?
            }
            prism::Node::ConstantPathTargetNode { .. } => {
                // Same logical shape as ConstantPathNode used in a
                // read context — flatten through `collect_const_chain`
                // by reading the parent chain plus this target's name.
                let n = node.as_constant_path_target_node().unwrap();
                let name = match n.name() {
                    Some(id) => constant_name(&id)?,
                    None => {
                        return Err(self.unsupported_node("constant path target missing name", loc));
                    }
                };
                let chain = match n.parent() {
                    None => ConstChain {
                        toplevel: true,
                        parent: None,
                        prefix: vec![],
                        name,
                    },
                    Some(p) => match self.collect_const_chain(&p)? {
                        // A constant chain, possibly already rooted on a
                        // non-constant base (`(expr)::A::B`): slide the inner
                        // leaf into `prefix` and take this target's name,
                        // exactly as the read-side `collect_const_chain` does.
                        Some(mut inner) => {
                            inner.prefix.push(std::mem::take(&mut inner.name));
                            inner.name = name;
                            inner
                        }
                        // `(expr)::A = value` — a non-constant prefix. Lower
                        // it as an ordinary expression and use it as the
                        // constant's base scope (bytecodegen evaluates it),
                        // mirroring the read-side `ConstantPathNode` path.
                        None => ConstChain {
                            toplevel: false,
                            parent: Some(Box::new(self.lower_node(&p)?)),
                            prefix: vec![],
                            name,
                        },
                    },
                };
                Node {
                    kind: NodeKind::Const {
                        toplevel: chain.toplevel,
                        parent: chain.parent,
                        prefix: chain.prefix,
                        name: chain.name,
                    },
                    loc,
                }
            }
            prism::Node::CallTargetNode { .. } => {
                // `recv.attr = value` form embedded in a multi-write
                // (`obj.x, obj.y = 1, 2`). Prism normalises this to
                // a `CallTargetNode { receiver, name }` whose `name`
                // is the *setter* identifier (`x=`); ruruby instead
                // produces a getter-shaped `MethodCall { receiver,
                // method, arglist: empty, safe_nav }` carrying just
                // the reader (`x`). Bytecodegen pattern-matches that
                // exact shape in `eval_lvalue` and emits the
                // corresponding `attr=` setter call when the
                // assignment runs — so we strip the trailing `=`
                // here.
                let n = node.as_call_target_node().unwrap();
                let receiver = self.lower_node(&n.receiver())?;
                let raw = constant_name(&n.name())?;
                let method = raw.strip_suffix('=').unwrap_or(&raw).to_owned();
                Node {
                    kind: NodeKind::MethodCall {
                        receiver: Box::new(receiver),
                        method,
                        arglist: Box::new(crate::ast::ArgList::default()),
                        safe_nav: n.is_safe_navigation(),
                    },
                    loc,
                }
            }
            // Nested destructure (`(a, (b, c)) = ...`): Prism nests a
            // `MultiTargetNode` inside the LHS. Lower its own
            // lefts/rest/rights recursively and wrap in
            // `MulAssignNested`, which bytecodegen expands with a
            // chained `ExpandArray`.
            prism::Node::MultiTargetNode { .. } => {
                let mt = node.as_multi_target_node().unwrap();
                let lefts: Vec<_> = mt.lefts().iter().collect();
                let rights: Vec<_> = mt.rights().iter().collect();
                let targets = self.lower_target_list(&lefts, mt.rest(), &rights)?;
                Node {
                    kind: NodeKind::MulAssignNested(targets),
                    loc,
                }
            }
            other => return Err(self.unsupported("assign target", &other)),
        })
    }

    fn lower_begin(&mut self, node: &BeginNode<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        let body_loc = match node.statements() {
            Some(s) => location_to_loc(&s.location()),
            None => loc,
        };
        let body = match node.statements() {
            Some(s) => self.lower_statements_compact(&s, body_loc)?,
            None => Node {
                kind: NodeKind::Nil,
                loc: body_loc,
            },
        };

        let mut rescue_entries: Vec<crate::ast::RescueEntry> = Vec::new();
        if let Some(first) = node.rescue_clause() {
            let mut current = first;
            loop {
                rescue_entries.push(self.lower_rescue_entry(&current)?);
                match current.subsequent() {
                    Some(next) => current = next,
                    None => break,
                }
            }
        }

        let else_ = match node.else_clause() {
            Some(e) => {
                let inner = e.statements();
                Some(Box::new(self.lower_optional_statements(
                    inner.as_ref(),
                    location_to_loc(&e.location()),
                )?))
            }
            None => None,
        };
        let ensure = match node.ensure_clause() {
            Some(e) => {
                let inner = e.statements();
                Some(Box::new(self.lower_optional_statements(
                    inner.as_ref(),
                    location_to_loc(&e.location()),
                )?))
            }
            None => None,
        };

        Ok(Node {
            kind: NodeKind::Begin {
                body: Box::new(body),
                rescue: rescue_entries,
                else_,
                ensure,
            },
            loc,
        })
    }

    fn lower_rescue_entry(
        &mut self,
        node: &RescueNode<'pr>,
    ) -> Result<crate::ast::RescueEntry, MonorubyErr> {
        let mut exception_list: Vec<Node> = Vec::new();
        for ex in node.exceptions().iter() {
            exception_list.push(self.lower_node(&ex)?);
        }
        let assign = match node.reference() {
            Some(ref_node) => Some(Box::new(self.lower_assign_target(&ref_node)?)),
            None => None,
        };
        let body_loc = match node.statements() {
            Some(s) => location_to_loc(&s.location()),
            None => location_to_loc(&node.location()),
        };
        let body = match node.statements() {
            Some(s) => self.lower_statements_compact(&s, body_loc)?,
            None => Node {
                kind: NodeKind::Nil,
                loc: body_loc,
            },
        };
        Ok(crate::ast::RescueEntry {
            exception_list,
            assign,
            body: Box::new(body),
        })
    }

    /// Lower a `StatementsNode` body so that single-statement bodies
    /// inline as the bare expression (matching ruruby's convention)
    /// and multi-statement bodies become a `CompStmt`.
    fn lower_statements_compact(
        &mut self,
        node: &StatementsNode<'pr>,
        loc: Loc,
    ) -> Result<Node, MonorubyErr> {
        let mut stmts = self.lower_statements_into_vec(node)?;
        Ok(match stmts.len() {
            0 => Node {
                kind: NodeKind::Nil,
                loc,
            },
            1 => stmts.pop().unwrap(),
            _ => Node {
                kind: NodeKind::CompStmt(stmts),
                loc,
            },
        })
    }

    fn lower_interpolated_string(
        &mut self,
        node: &InterpolatedStringNode<'pr>,
    ) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        let parts = self.lower_interp_parts(node.parts())?;
        Ok(Node {
            kind: NodeKind::InterporatedString(parts),
            loc,
        })
    }

    /// Shared lowering for the `parts` lists of
    /// `InterpolatedStringNode`, `InterpolatedRegularExpressionNode`,
    /// and `InterpolatedMatchLastLineNode`. Each part is one of:
    /// - `StringNode` — a static chunk; pass through `lower_string`
    /// - `EmbeddedStatementsNode` (`#{...}`) — unwrap to bare expr,
    ///   `Nil`, or `CompStmt` depending on statement count
    /// - `EmbeddedVariableNode` (`#@x`, `#$x`) — forward the inner
    ///   variable read directly
    fn lower_interp_parts(
        &mut self,
        parts: prism::NodeList<'pr>,
    ) -> Result<Vec<Node>, MonorubyErr> {
        let mut out: Vec<Node> = Vec::new();
        for part in parts.iter() {
            match part {
                prism::Node::StringNode { .. } => {
                    out.push(self.lower_string(&part.as_string_node().unwrap()));
                }
                prism::Node::EmbeddedStatementsNode { .. } => {
                    let inner = part.as_embedded_statements_node().unwrap();
                    let part_loc = location_to_loc(&inner.location());
                    let lowered = match inner.statements() {
                        None => Node {
                            kind: NodeKind::Nil,
                            loc: part_loc,
                        },
                        Some(s) => {
                            let mut stmts = self.lower_statements_into_vec(&s)?;
                            match stmts.len() {
                                0 => Node {
                                    kind: NodeKind::Nil,
                                    loc: part_loc,
                                },
                                1 => stmts.pop().unwrap(),
                                _ => Node {
                                    kind: NodeKind::CompStmt(stmts),
                                    loc: part_loc,
                                },
                            }
                        }
                    };
                    out.push(lowered);
                }
                prism::Node::EmbeddedVariableNode { .. } => {
                    let inner = part.as_embedded_variable_node().unwrap();
                    out.push(self.lower_node(&inner.variable())?);
                }
                // Heredoc-and-friends: Prism may emit a nested
                // InterpolatedStringNode as one part — common when a
                // squiggly heredoc body or adjacent-string concat is
                // parsed inside another interpolated string. Lower it
                // as a regular value-producing string and let it sit
                // alongside the static chunks.
                prism::Node::InterpolatedStringNode { .. } => {
                    out.push(self.lower_node(&part)?);
                }
                other => return Err(self.unsupported("interpolation part", &other)),
            }
        }
        Ok(out)
    }

    /// `42i` / `1.5i` / `1ri` — Prism nests an `IntegerNode` /
    /// `FloatNode` / `RationalNode` inside `ImaginaryNode::numeric`.
    /// ruruby uses `Imaginary(NReal)` for the integer / float cases
    /// and a separate `RImaginary(BigInt, BigInt)` when the inner
    /// part is a rational.
    fn lower_imaginary(&self, numeric: &prism::Node<'pr>, loc: Loc) -> Result<Node, MonorubyErr> {
        match numeric {
            prism::Node::IntegerNode { .. } => {
                let inner = numeric.as_integer_node().unwrap();
                let value = inner.value();
                let (negative, digits) = value.to_u32_digits();
                if digits.len() <= 1 {
                    let n = digits.first().copied().unwrap_or(0) as i64;
                    let signed = if negative { -n } else { n };
                    Ok(Node {
                        kind: NodeKind::Imaginary(NReal::Integer(signed)),
                        loc,
                    })
                } else {
                    let bi = prism_integer_to_bigint(&value);
                    Ok(Node {
                        kind: NodeKind::Imaginary(NReal::Bignum(bi)),
                        loc,
                    })
                }
            }
            prism::Node::FloatNode { .. } => {
                let inner = numeric.as_float_node().unwrap();
                Ok(Node {
                    kind: NodeKind::Imaginary(NReal::Float(inner.value())),
                    loc,
                })
            }
            prism::Node::RationalNode { .. } => {
                let inner = numeric.as_rational_node().unwrap();
                Ok(Node {
                    kind: NodeKind::RImaginary(
                        prism_integer_to_bigint(&inner.numerator()),
                        prism_integer_to_bigint(&inner.denominator()),
                    ),
                    loc,
                })
            }
            other => Err(self.unsupported("imaginary numeric", other)),
        }
    }

    fn lower_regex(&self, node: &RegularExpressionNode<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        let part = Node {
            kind: regex_body_to_string(node.unescaped(), loc)?,
            loc,
        };
        let flags = regex_flags_from_closing(&node.closing_loc());
        Ok(Node {
            kind: NodeKind::RegExp(vec![part], flags, true),
            loc,
        })
    }

    fn lower_interpolated_regex(
        &mut self,
        node: &InterpolatedRegularExpressionNode<'pr>,
    ) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        let parts = self.lower_interp_parts(node.parts())?;
        let flags = regex_flags_from_closing(&node.closing_loc());
        Ok(Node {
            kind: NodeKind::RegExp(parts, flags, false),
            loc,
        })
    }

    /// Build the value carried by a `return` / `break` / `next`.
    /// ruruby's shape is:
    ///   - 0 args         -> `Nil`
    ///   - 1 non-splat    -> bare expr
    ///   - 1 splat        -> `Array([Splat(...)])` (wrapped, NOT bare)
    ///   - n>=2           -> `Array([...])`
    /// Wrapping the lone-splat case keeps bytecodegen from feeding a
    /// raw `Splat(...)` into the value-producing path (which it
    /// rejects as `unsupported lhs Splat(...)`); the array shape is
    /// flattened at runtime. Ruby semantics: `return *x` returns
    /// `[*x]`, which matches.
    fn lower_jump_value(
        &mut self,
        args: Option<prism::ArgumentsNode<'pr>>,
        loc: Loc,
    ) -> Result<Node, MonorubyErr> {
        let mut values: Vec<Node> = Vec::new();
        if let Some(arglist) = args {
            for arg in arglist.arguments().iter() {
                values.push(self.lower_node(&arg)?);
            }
        }
        Ok(jump_value_node(values, loc))
    }

    fn lower_return(&mut self, node: &ReturnNode<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        let has_args = node.arguments().is_some();
        let mut values: Vec<Node> = Vec::new();
        if let Some(arglist) = node.arguments() {
            for arg in arglist.arguments().iter() {
                values.push(self.lower_node(&arg)?);
            }
        }
        let inner = jump_value_node(values, loc);
        let ret = Node {
            kind: NodeKind::Return(Box::new(inner)),
            loc,
        };
        // `return <arg>` directly at a script file's top level (not
        // inside a def/block/class; if/begin are fine): CRuby warns —
        // the value is discarded — at *runtime*, only when the return
        // actually executes, with a path-only (no line) prefix, at
        // the default warning level (silenced by -W0). Desugar to
        //   (__warn_toplevel_return("<path>"); return <arg>)
        // so the enclosing control flow gates the warning naturally.
        if self.prism_scope_level == 0 && !self.eval_parse && has_args {
            let warn_call = Node::new_fcall(
                "__warn_toplevel_return".to_string(),
                crate::ast::ArgList {
                    args: vec![Node {
                        kind: NodeKind::String(self.path.clone()),
                        loc,
                    }],
                    ..Default::default()
                },
                false,
                loc,
            );
            return Ok(Node::new_comp_stmt(vec![warn_call, ret], loc));
        }
        Ok(ret)
    }

    fn lower_constant_write(&mut self, node: &ConstantWriteNode<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        let name = constant_name(&node.name())?;
        let target = Node {
            kind: NodeKind::Const {
                toplevel: false,
                parent: None,
                prefix: vec![],
                name,
            },
            loc: location_to_loc(&node.name_loc()),
        };
        let value = self.lower_node(&node.value())?;
        Ok(Node {
            kind: NodeKind::MulAssign(vec![target], vec![value]),
            loc,
        })
    }

    /// Lowers a top-level `def name(params) ... end` into ruruby's
    /// `MethodDef(name, BlockInfo)` shape. The method body is wrapped
    /// in an implicit `Begin { rescue: [], else_: None, ensure: None }`
    /// to mirror what ruruby-parse produces — bytecodegen relies on
    /// every method body being a `Begin` so `return` / `rescue` /
    /// `ensure` see a uniform structure.
    fn lower_def(&mut self, node: &DefNode<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        let name = constant_name(&node.name())?;

        // `def self.foo`, `def obj.foo` — Prism puts the receiver in
        // `node.receiver()`. The body / params / locals story is the
        // same as for a regular method, so we share the rest of the
        // lowering and just emit the singleton variant once we know
        // which receiver to attach.
        let singleton_receiver = match node.receiver() {
            Some(recv) => Some(self.lower_node(&recv)?),
            None => None,
        };

        let saved = self.enter_prism_scope();
        let result =
            (|this: &mut Self| -> Result<(Vec<crate::ast::FormalParam>, Node), MonorubyErr> {
                // Parameters first so the LvarCollector's special
                // fields (kw / kwrest / block / forwarding_param) get
                // populated through the proper insert helpers; then
                // sweep the rest of `node.locals()` to add body-only
                // locals. `insert` is idempotent, so previously-seen
                // names from the param pass are no-ops.
                let params = match node.parameters() {
                    Some(p) => this.lower_parameters(&p)?,
                    None => Vec::new(),
                };
                this.collect_locals(&node.locals())?;
                // Match ruruby-parse's method-body shape (also assumed by
                // bytecodegen's `hint()`): a single-statement body is the
                // bare expression, not a `CompStmt([expr])`. Plain
                // `lower_node(StatementsNode)` always wraps in CompStmt,
                // which makes `def foo; false; end` fall through `hint()`
                // and miss the `ConstReturn(false)` annotation; without
                // it the JIT can't fold trivial helpers like the default
                // `Object#respond_to_missing?`.
                let body_inner = this.lower_compact_body(node.body(), loc)?;
                let body = Node {
                    kind: NodeKind::Begin {
                        body: Box::new(body_inner),
                        rescue: vec![],
                        else_: None,
                        ensure: None,
                    },
                    loc,
                };
                Ok((params, body))
            })(self);

        match result {
            Ok((params, body)) => {
                let method_lvars = self.exit_prism_scope(saved);
                let info = Box::new(BlockInfo {
                    params,
                    body: Box::new(body),
                    lvar: method_lvars,
                    loc,
                    is_lambda: false,
                });
                let kind = match singleton_receiver {
                    Some(recv) => NodeKind::SingletonMethodDef(Box::new(recv), name, info),
                    None => NodeKind::MethodDef(name, info),
                };
                Ok(Node { kind, loc })
            }
            Err(e) => {
                self.exit_prism_scope(saved);
                Err(e)
            }
        }
    }

    /// Lowers `BlockNode` (the literal `{ ... }` / `do ... end` attached
    /// to a method call) into the `Lambda(BlockInfo)` shape ruruby uses
    /// for `arglist.block`.
    /// `-> (params) { body }` standalone lambda literal. Has the same
    /// scope shape as a `BlockNode`, just produced without an outer
    /// CallNode. Lowers to `Lambda(BlockInfo)` — bytecodegen treats
    /// the same Lambda node as either a method-call block or a free
    /// proc depending on how it's used.
    fn lower_lambda(&mut self, node: &LambdaNode<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        let saved = self.enter_prism_scope();
        let result =
            (|this: &mut Self| -> Result<(Vec<crate::ast::FormalParam>, Node), MonorubyErr> {
                let params = match node.parameters() {
                    None => Vec::new(),
                    Some(p) => match p {
                        prism::Node::BlockParametersNode { .. } => {
                            let bp = p.as_block_parameters_node().unwrap();
                            let params = match bp.parameters() {
                                Some(pn) => this.lower_parameters(&pn)?,
                                None => Vec::new(),
                            };
                            this.collect_block_local_shadows(&bp.locals())?;
                            params
                        }
                        // `->(a, b) { ... }` with full ParametersNode
                        // (no BlockParametersNode wrapper) is allowed
                        // in Prism for `->(...)` form; handle it the
                        // same way.
                        prism::Node::ParametersNode { .. } => {
                            let pn = p.as_parameters_node().unwrap();
                            this.lower_parameters(&pn)?
                        }
                        prism::Node::ItParametersNode { .. } => {
                            // Ruby 3.4 `it` in a `-> { it }` lambda —
                            // same single-anonymous-param shape as the
                            // block-form `ItParametersNode` arm.
                            let ip = p.as_it_parameters_node().unwrap();
                            let ip_loc = location_to_loc(&ip.location());
                            this.lvars.insert("it");
                            vec![crate::ast::FormalParam {
                                kind: ParamKind::ItParam,
                                loc: ip_loc,
                            }]
                        }
                        prism::Node::NumberedParametersNode { .. } => {
                            // `-> { _1 + _2 }` — numbered params in a
                            // lambda behave exactly like in a block; mirror
                            // the block-form arm.
                            let np = p.as_numbered_parameters_node().unwrap();
                            let max = np.maximum() as usize;
                            let np_loc = location_to_loc(&np.location());
                            let mut out = Vec::with_capacity(max);
                            for i in 1..=max {
                                let name = format!("_{i}");
                                this.lvars.insert(&name);
                                out.push(crate::ast::FormalParam {
                                    kind: ParamKind::Param(name),
                                    loc: np_loc,
                                });
                            }
                            if max > 0 {
                                this.lvars.numbered_param = Some(np_loc);
                                this.lvars.numbered_param_max = max as u8;
                            }
                            out
                        }
                        other => return Err(this.unsupported("lambda parameters", &other)),
                    },
                };
                this.collect_locals(&node.locals())?;
                let body = match node.body() {
                    Some(b) => this.lower_node(&b)?,
                    None => Node {
                        kind: NodeKind::Nil,
                        loc,
                    },
                };
                Ok((params, body))
            })(self);

        match result {
            Ok((params, body)) => {
                let lambda_lvars = self.exit_prism_scope(saved);
                Ok(Node {
                    kind: NodeKind::Lambda(Box::new(BlockInfo {
                        params,
                        body: Box::new(body),
                        lvar: lambda_lvars,
                        loc,
                        is_lambda: true,
                    })),
                    loc,
                })
            }
            Err(e) => {
                self.exit_prism_scope(saved);
                Err(e)
            }
        }
    }

    fn lower_block(&mut self, node: &BlockNode<'pr>) -> Result<Node, MonorubyErr> {
        let loc = location_to_loc(&node.location());
        // Block scope owns its own LvarCollector. Save the outer one
        // and install a fresh table for the block body. The body
        // lowering is wrapped so an error inside it doesn't leak the
        // partially-built block scope into the outer lowerer.
        let saved = self.enter_prism_scope();
        let result =
            (|this: &mut Self| -> Result<(Vec<crate::ast::FormalParam>, Node), MonorubyErr> {
                // Parameters first (see comment on the def-node lowerer for
                // why), then top up the rest from `node.locals()`.
                let params = match node.parameters() {
                    None => Vec::new(),
                    Some(p) => match p {
                        prism::Node::BlockParametersNode { .. } => {
                            let bp = p.as_block_parameters_node().unwrap();
                            let params = match bp.parameters() {
                                Some(pn) => this.lower_parameters(&pn)?,
                                None => Vec::new(),
                            };
                            this.collect_block_local_shadows(&bp.locals())?;
                            params
                        }
                        prism::Node::NumberedParametersNode { .. } => {
                            // `_1`, `_2`, … — Prism reports the max
                            // index actually referenced (`.maximum()`)
                            // and we synthesize one `Param("_N")` per
                            // index. ruruby tracks the same shape plus
                            // a `numbered_param: Some(loc)` flag on
                            // `LvarCollector`; we mirror that flag too
                            // so bytecodegen's `_1`-vs-explicit-block-arg
                            // checks behave identically.
                            let np = p.as_numbered_parameters_node().unwrap();
                            let max = np.maximum() as usize;
                            let np_loc = location_to_loc(&np.location());
                            let mut out = Vec::with_capacity(max);
                            for i in 1..=max {
                                let name = format!("_{i}");
                                this.lvars.insert(&name);
                                out.push(crate::ast::FormalParam {
                                    kind: ParamKind::Param(name),
                                    loc: np_loc,
                                });
                            }
                            if max > 0 {
                                this.lvars.numbered_param = Some(np_loc);
                                this.lvars.numbered_param_max = max as u8;
                            }
                            out
                        }
                        prism::Node::ItParametersNode { .. } => {
                            // Ruby 3.4 `it`: an anonymous single block
                            // parameter. Prism emits `ItParametersNode`
                            // on the block and every reference as an
                            // `ItLocalVariableReadNode`. It behaves
                            // exactly like an explicit `|it|` (one
                            // required param, no auto-splat), so we
                            // synthesize a single `Param("it")` and
                            // bind the local; the read side lowers to
                            // `LocalVar(0, "it")`.
                            let ip = p.as_it_parameters_node().unwrap();
                            let ip_loc = location_to_loc(&ip.location());
                            this.lvars.insert("it");
                            vec![crate::ast::FormalParam {
                                kind: ParamKind::ItParam,
                                loc: ip_loc,
                            }]
                        }
                        other => return Err(this.unsupported("block parameters", &other)),
                    },
                };
                this.collect_locals(&node.locals())?;
                let body = match node.body() {
                    Some(b) => this.lower_node(&b)?,
                    None => Node {
                        kind: NodeKind::Nil,
                        loc,
                    },
                };
                Ok((params, body))
            })(self);

        match result {
            Ok((params, body)) => {
                let block_lvars = self.exit_prism_scope(saved);
                Ok(Node {
                    kind: NodeKind::Lambda(Box::new(BlockInfo {
                        params,
                        body: Box::new(body),
                        lvar: block_lvars,
                        loc,
                        is_lambda: false,
                    })),
                    loc,
                })
            }
            Err(e) => {
                self.exit_prism_scope(saved);
                Err(e)
            }
        }
    }

    /// Recursively lowers a destructuring block/lambda parameter
    /// (`MultiTargetNode`) into a `Vec<DestructEntry>`. Leaves are
    /// `RequiredParameterNode` (`Param`), the optional `rest` is a
    /// `SplatNode` (`Rest`, named or anonymous), and nested groups are
    /// `MultiTargetNode` (`Nested`). Entries are emitted in source
    /// order: `lefts`, then `rest`, then `rights`.
    fn lower_destruct_el(
        &mut self,
        entries: &mut Vec<DestructEntry>,
        el: &prism::Node<'pr>,
    ) -> Result<(), MonorubyErr> {
        match el {
            prism::Node::RequiredParameterNode { .. } => {
                let inner = el.as_required_parameter_node().unwrap();
                let name = constant_name(&inner.name())?;
                self.lvars.insert(&name);
                entries.push(DestructEntry::Param(name, location_to_loc(&inner.location())));
            }
            prism::Node::MultiTargetNode { .. } => {
                let nested = el.as_multi_target_node().unwrap();
                let nested_loc = location_to_loc(&el.location());
                let sub = self.lower_destruct(&nested)?;
                entries.push(DestructEntry::Nested(sub, nested_loc));
            }
            other => {
                return Err(self.unsupported("destructure param leaf", other));
            }
        }
        Ok(())
    }

    fn lower_destruct(
        &mut self,
        mt: &prism::MultiTargetNode<'pr>,
    ) -> Result<Vec<DestructEntry>, MonorubyErr> {
        let mut entries: Vec<DestructEntry> = Vec::new();
        for el in mt.lefts().iter() {
            self.lower_destruct_el(&mut entries, &el)?;
        }
        if let Some(rest) = mt.rest() {
            match rest {
                prism::Node::SplatNode { .. } => {
                    let inner = rest.as_splat_node().unwrap();
                    let rest_loc = location_to_loc(&inner.location());
                    let name = match inner.expression() {
                        Some(e) => match e {
                            prism::Node::RequiredParameterNode { .. } => {
                                let rp = e.as_required_parameter_node().unwrap();
                                let name = constant_name(&rp.name())?;
                                self.lvars.insert(&name);
                                Some(name)
                            }
                            other => {
                                return Err(self.unsupported("destructure splat target", &other));
                            }
                        },
                        None => None,
                    };
                    entries.push(DestructEntry::Rest(name, rest_loc));
                }
                other => return Err(self.unsupported("destructure rest", &other)),
            }
        }
        for el in mt.rights().iter() {
            self.lower_destruct_el(&mut entries, &el)?;
        }
        Ok(entries)
    }

    /// Lowers a `ParametersNode` (the typed parameter cluster shared by
    /// method definitions and block parameters) into ruruby's flat
    /// `Vec<FormalParam>` AND threads each parameter into the current
    /// `LvarCollector` using the same special-field rules ruruby's
    /// parser applies (`kw.push` for keyword params,
    /// `insert_kwrest_param` for `**kw`, `insert_block_param` for
    /// `&blk`, `insert_delegate_param` for `...`). Callers must
    /// invoke this BEFORE seeding the rest of `node.locals()` so that
    /// the table order matches ruruby's.
    fn lower_parameters(
        &mut self,
        params: &ParametersNode<'pr>,
    ) -> Result<Vec<crate::ast::FormalParam>, MonorubyErr> {
        let mut out: Vec<crate::ast::FormalParam> = Vec::new();

        // Required positional: `def f(a, b)` -> Param("a"), Param("b")
        for n in params.requireds().iter() {
            match n {
                prism::Node::RequiredParameterNode { .. } => {
                    let inner = n.as_required_parameter_node().unwrap();
                    let name = constant_name(&inner.name())?;
                    self.lvars.insert(&name);
                    out.push(crate::ast::FormalParam {
                        kind: ParamKind::Param(name),
                        loc: location_to_loc(&inner.location()),
                    });
                }
                // `|(a, *b, (c, d))|` block-destructure: Prism wraps it
                // in a `MultiTargetNode` whose `lefts`/`rest`/`rights`
                // lists hold the individual leaves (`RequiredParameterNode`),
                // an optional splat (`SplatNode`), and nested destructures
                // (`MultiTargetNode`). We lower this recursively into a
                // `Vec<DestructEntry>` supporting splat + nesting.
                prism::Node::MultiTargetNode { .. } => {
                    let mt = n.as_multi_target_node().unwrap();
                    let mt_loc = location_to_loc(&n.location());
                    let entries = self.lower_destruct(&mt)?;
                    if entries.is_empty() {
                        return Err(self.unsupported_node("empty destructure param", mt_loc));
                    }
                    out.push(crate::ast::FormalParam {
                        kind: ParamKind::Destruct(entries),
                        loc: mt_loc,
                    });
                }
                other => return Err(self.unsupported("required param", &other)),
            }
        }

        // Optional: `b = expr` -> Optional("b", default)
        for n in params.optionals().iter() {
            match n {
                prism::Node::OptionalParameterNode { .. } => {
                    let inner = n.as_optional_parameter_node().unwrap();
                    let name = constant_name(&inner.name())?;
                    self.lvars.insert(&name);
                    let default = self.lower_node(&inner.value())?;
                    out.push(crate::ast::FormalParam {
                        kind: ParamKind::Optional(name, Box::new(default)),
                        loc: location_to_loc(&inner.location()),
                    });
                }
                other => return Err(self.unsupported("optional param", &other)),
            }
        }

        // Rest: `*rest` or `*` (anonymous)
        if let Some(rest) = params.rest() {
            match rest {
                prism::Node::RestParameterNode { .. } => {
                    let inner = rest.as_rest_parameter_node().unwrap();
                    let name = match inner.name() {
                        Some(id) => Some(constant_name(&id)?),
                        // Anonymous `*`: bind a reserved, unspellable local
                        // so `foo(*)` forwarding can splat it.
                        None => Some(ANON_REST_NAME.to_owned()),
                    };
                    if let Some(n) = &name {
                        self.lvars.insert(n);
                    }
                    out.push(crate::ast::FormalParam {
                        kind: ParamKind::Rest(name),
                        loc: location_to_loc(&inner.location()),
                    });
                }
                prism::Node::ImplicitRestNode { .. } => {
                    // `|a,|` — trailing comma in block params. Acts
                    // like an anonymous rest *only* under block
                    // dispatch (extras absorbed, single Array
                    // auto-splat fires); under method-style dispatch
                    // (`define_method`) it's invisible — `arity` is
                    // unchanged and extras raise `ArgumentError`.
                    // `ParamKind::ImplicitRest` carries that two-faced
                    // semantic; it lowers to a `rest`-slot in
                    // `ParamsInfo` with `rest_is_implicit = true`.
                    out.push(crate::ast::FormalParam {
                        kind: ParamKind::ImplicitRest,
                        loc: location_to_loc(&rest.location()),
                    });
                }
                other => return Err(self.unsupported("rest param", &other)),
            }
        }

        // Post (after `*rest`): `def f(*rest, c, d)` -> Post("c"), Post("d")
        for n in params.posts().iter() {
            match n {
                prism::Node::RequiredParameterNode { .. } => {
                    let inner = n.as_required_parameter_node().unwrap();
                    let name = constant_name(&inner.name())?;
                    self.lvars.insert(&name);
                    out.push(crate::ast::FormalParam {
                        kind: ParamKind::Post(Some(name)),
                        loc: location_to_loc(&inner.location()),
                    });
                }
                // `|*c, (*d, e)|` — a destructure in post position.
                prism::Node::MultiTargetNode { .. } => {
                    let mt = n.as_multi_target_node().unwrap();
                    let mt_loc = location_to_loc(&n.location());
                    let entries = self.lower_destruct(&mt)?;
                    if entries.is_empty() {
                        return Err(self.unsupported_node("empty destructure param", mt_loc));
                    }
                    out.push(crate::ast::FormalParam {
                        kind: ParamKind::PostDestruct(entries),
                        loc: mt_loc,
                    });
                }
                other => return Err(self.unsupported("post param", &other)),
            }
        }

        // Keywords (required and optional, intermingled in source order)
        for n in params.keywords().iter() {
            match n {
                prism::Node::RequiredKeywordParameterNode { .. } => {
                    let inner = n.as_required_keyword_parameter_node().unwrap();
                    let name = constant_name(&inner.name())?;
                    let lid = self.lvars.insert(&name);
                    self.lvars.kw.push(lid);
                    out.push(crate::ast::FormalParam {
                        kind: ParamKind::Keyword(name, None),
                        loc: location_to_loc(&inner.location()),
                    });
                }
                prism::Node::OptionalKeywordParameterNode { .. } => {
                    let inner = n.as_optional_keyword_parameter_node().unwrap();
                    let name = constant_name(&inner.name())?;
                    let lid = self.lvars.insert(&name);
                    self.lvars.kw.push(lid);
                    let default = self.lower_node(&inner.value())?;
                    out.push(crate::ast::FormalParam {
                        kind: ParamKind::Keyword(name, Some(Box::new(default))),
                        loc: location_to_loc(&inner.location()),
                    });
                }
                other => return Err(self.unsupported("keyword param", &other)),
            }
        }

        // Keyword rest (`**kw` / `**` / `...` forwarding lands here too)
        if let Some(kr) = params.keyword_rest() {
            match kr {
                prism::Node::KeywordRestParameterNode { .. } => {
                    let inner = kr.as_keyword_rest_parameter_node().unwrap();
                    let loc = location_to_loc(&inner.location());
                    let name_opt = match inner.name() {
                        Some(id) => Some(constant_name(&id)?),
                        // Anonymous `**`: bind a reserved, unspellable local
                        // so `foo(**)` forwarding can splat it.
                        None => Some(ANON_KWREST_NAME.to_owned()),
                    };
                    if let Some(n) = &name_opt {
                        self.lvars.insert_kwrest_param(n.clone());
                    }
                    out.push(crate::ast::FormalParam {
                        kind: ParamKind::KWRest(name_opt),
                        loc,
                    });
                }
                prism::Node::ForwardingParameterNode { .. } => {
                    let loc = location_to_loc(&kr.location());
                    self.lvars.insert_delegate_param();
                    out.push(crate::ast::FormalParam {
                        kind: ParamKind::Forwarding,
                        loc,
                    });
                }
                prism::Node::NoKeywordsParameterNode { .. } => {
                    // `**nil` — explicitly declines kwargs. ruruby
                    // models it as a kwrest param literally named
                    // "nil" (`KWRest(Some("nil"))` + an `lvars`
                    // entry); bytecodegen reads that shape and
                    // emits the "kwargs forbidden" runtime check.
                    // No safer name is available without ruruby
                    // AST extensions, so we replicate that exact
                    // shape.
                    let loc = location_to_loc(&kr.location());
                    self.lvars.insert_kwrest_param("nil".to_owned());
                    out.push(crate::ast::FormalParam {
                        kind: ParamKind::KWRest(Some("nil".to_owned())),
                        loc,
                    });
                }
                other => return Err(self.unsupported("keyword rest param", &other)),
            }
        }

        // Block param (`&blk` / `&`)
        if let Some(bp) = params.block() {
            let loc = location_to_loc(&bp.location());
            let name = match bp.name() {
                Some(id) => Some(constant_name(&id)?),
                None => None,
            };
            if let Some(n) = &name {
                self.lvars.insert_block_param(n.clone());
            }
            out.push(crate::ast::FormalParam {
                kind: ParamKind::Block(name),
                loc,
            });
        }

        Ok(out)
    }

    fn lower_call(&mut self, node: &prism::CallNode<'pr>, loc: Loc) -> Result<Node, MonorubyErr> {
        let receiver_opt = node.receiver();
        let name_bytes = node.name().as_slice();
        let method = std::str::from_utf8(name_bytes)
            .map(str::to_owned)
            .unwrap_or_else(|_| String::from_utf8_lossy(name_bytes).into_owned());

        let mut args: Vec<Node> = vec![];
        let mut kw_args: Vec<(String, Node)> = vec![];
        let mut hash_splat: Vec<Node> = vec![];
        let mut forwarding = false;
        if let Some(arglist) = node.arguments() {
            for n in arglist.arguments().iter() {
                self.collect_call_arg(
                    &n,
                    &mut args,
                    &mut kw_args,
                    &mut hash_splat,
                    &mut forwarding,
                )?;
            }
        }
        // Detect splat presence in the freshly-lowered args list and
        // mirror ruruby's `arglist.splat` flag so the calling
        // convention agrees with what bytecodegen expects.
        let has_splat = args.iter().any(|a| matches!(a.kind, NodeKind::Splat(_)));
        // ruruby stores both block-literal (`{ ... }`) and
        // block-argument (`&proc`) forms in the same `arglist.block`
        // slot — bytecodegen disambiguates by NodeKind.
        // `foo(&)` (anonymous & forward) instead sets the
        // `delegate_block` flag with no block node.
        let mut delegate_block = false;
        let block = match node.block() {
            None => None,
            Some(block_node) => match self.lower_call_block(&block_node)? {
                CallBlock::Block(b) => Some(Box::new(b)),
                CallBlock::Delegate => {
                    delegate_block = true;
                    None
                }
            },
        };

        // `a[i]` / `a[i, j]` come through as `CallNode` with method
        // `[]`. ruruby treats indexing as a first-class `Index` node
        // rather than a method call, so collapse it here.
        if block.is_none()
            && let Some(recv) = receiver_opt.as_ref()
            && method == "[]"
        {
            let base = self.lower_node(recv)?;
            // `recv[k => v]` / `recv[**h]`: the `NodeKind::Index`
            // fast path is positional-only and would drop the
            // keyword/`**` portion. Keep it as a real `[]` method
            // call so the *callee* decides what to do with the
            // keywords (e.g. `Hash.[]` folds them into a trailing
            // Hash; `Proc#[]` forwards them to the block).
            if kw_args.is_empty() && hash_splat.is_empty() {
                return Ok(Node {
                    kind: NodeKind::Index {
                        base: Box::new(base),
                        index: args,
                    },
                    loc,
                });
            }
            let mut arglist = crate::ast::ArgList::default();
            arglist.args = args;
            arglist.kw_args = kw_args;
            arglist.hash_splat = hash_splat;
            arglist.splat = has_splat;
            return Ok(Node {
                kind: NodeKind::MethodCall {
                    receiver: Box::new(base),
                    method: "[]".to_string(),
                    arglist: Box::new(arglist),
                    safe_nav: false,
                },
                loc,
            });
        }

        // Setter calls (`a[i] = v`, `c.x = v`) come back as a
        // `CallNode` whose method name ends in `=` and whose
        // `is_attribute_write` flag is set. The last arg is always
        // the value being written; the rest (if any) are index
        // arguments. ruruby's parser desugars all of these to
        // `MulAssign([target], [value])` where the target is built
        // from the read form (method name without the trailing `=`)
        // — bytecodegen later rewrites the call into the setter when
        // it emits the assignment.
        if block.is_none()
            && let Some(recv) = receiver_opt.as_ref()
            && node.is_attribute_write()
            && !args.is_empty()
        {
            let value = args.pop().unwrap();
            let base = self.lower_node(recv)?;
            let read_name = method.strip_suffix('=').unwrap_or(&method).to_string();
            let target = if read_name == "[]" {
                Node {
                    kind: NodeKind::Index {
                        base: Box::new(base),
                        index: args,
                    },
                    loc,
                }
            } else {
                let mut arglist = crate::ast::ArgList::default();
                arglist.args = args;
                Node {
                    kind: NodeKind::MethodCall {
                        receiver: Box::new(base),
                        method: read_name,
                        arglist: Box::new(arglist),
                        safe_nav: node.is_safe_navigation(),
                    },
                    loc,
                }
            };
            return Ok(Node {
                kind: NodeKind::MulAssign(vec![target], vec![value]),
                loc,
            });
        }

        // 0-arg "method" calls of the form `-x`, `+x`, `~x`, `!x` are
        // really unary operators in our IR. Detect them before the
        // binop fast path so they don't get mistaken for a 1-arg
        // arithmetic call. (UnOp / BinOp shapes never carry a block.)
        if block.is_none()
            && let Some(recv) = receiver_opt.as_ref()
            && args.is_empty()
            && let Some(op) = unop_from_name(&method)
        {
            let inner = self.lower_node(recv)?;
            return Ok(Node {
                kind: NodeKind::UnOp(op, Box::new(inner)),
                loc,
            });
        }

        // Binary operators come back from Prism as a `CallNode` with a
        // one-element arg list and the operator as the method name.
        if block.is_none()
            && let Some(recv) = receiver_opt.as_ref()
            && args.len() == 1
            && let Some(op) = binop_from_name(&method)
        {
            let lhs = self.lower_node(recv)?;
            let rhs = args.into_iter().next().unwrap();
            return Ok(Node {
                kind: NodeKind::BinOp(op, Box::new(lhs), Box::new(rhs)),
                loc,
            });
        }

        // A "variable call" — a bare identifier with no receiver,
        // arguments, or parentheses (`foo` as opposed to `foo()` /
        // `self.foo`). Lowered to its own node kind because a failed
        // lookup must raise NameError ("undefined local variable or
        // method ...") instead of NoMethodError.
        if node.is_variable_call() {
            return Ok(Node::new_identifier(method, loc));
        }

        let mut arglist = crate::ast::ArgList::default();
        arglist.args = args;
        arglist.kw_args = kw_args;
        arglist.hash_splat = hash_splat;
        arglist.block = block;
        arglist.forwarding = forwarding;
        arglist.splat = has_splat;
        arglist.delegate_block = delegate_block;

        Ok(match receiver_opt {
            Some(recv) => {
                let receiver_node = self.lower_node(&recv)?;
                Node {
                    kind: NodeKind::MethodCall {
                        receiver: Box::new(receiver_node),
                        method,
                        arglist: Box::new(arglist),
                        safe_nav: node.is_safe_navigation(),
                    },
                    loc,
                }
            }
            None => Node {
                kind: NodeKind::FuncCall {
                    method,
                    arglist: Box::new(arglist),
                    safe_nav: false,
                },
                loc,
            },
        })
    }
}

/// Intermediate, owned representation of a constant-path chain used
/// while flattening Prism's recursive `ConstantPathNode` into the
/// flat `prefix` / `name` shape ruruby expects.
struct ConstChain {
    toplevel: bool,
    parent: Option<Box<Node>>,
    prefix: Vec<String>,
    name: String,
}

/// Conservative test for "this expression has no side effects and
/// produces a value known at compile time", used to set the
/// `is_constant_expr` flag on `Array` / `Range` literals so that
/// bytecodegen can fold them into the constant pool.
/// Build the value-producing inner node for a `return` / `break` /
/// `next` from its lowered argument list. See `lower_jump_value` for
/// the wrapping rules.
fn jump_value_node(values: Vec<Node>, loc: Loc) -> Node {
    match values.len() {
        0 => Node {
            kind: NodeKind::Nil,
            loc,
        },
        1 if matches!(values[0].kind, NodeKind::Splat(_)) => {
            let all_const = values.iter().all(|n| is_constant_literal(&n.kind));
            Node {
                kind: NodeKind::Array(values, all_const),
                loc,
            }
        }
        1 => values.into_iter().next().unwrap(),
        _ => {
            let all_const = values.iter().all(|n| is_constant_literal(&n.kind));
            Node {
                kind: NodeKind::Array(values, all_const),
                loc,
            }
        }
    }
}

/// Convert a Prism global-variable reference (the kind that appears
/// on either side of `alias $foo $bar`) into the
/// `Symbol("$foo")`-shape ruruby uses for `NodeKind::AliasMethod`
/// operands, including the `$`/`$1`/`$~` etc. prefixes that
/// bytecodegen later pattern-matches to pick `AliasGvar`.
fn global_var_alias_target(node: &prism::Node<'_>) -> Result<Node, MonorubyErr> {
    let loc = location_to_loc(&node.location());
    let bytes = node.location().as_slice();
    match std::str::from_utf8(bytes) {
        Ok(s) => Ok(Node {
            kind: NodeKind::Symbol(s.to_owned()),
            loc,
        }),
        Err(_) => Err(MonorubyErr::fatal(
            "prism lowerer: non-utf8 global variable name",
        )),
    }
}

fn is_constant_literal(kind: &NodeKind) -> bool {
    matches!(
        kind,
        NodeKind::Nil
            | NodeKind::Bool(_)
            | NodeKind::Integer(_)
            | NodeKind::Bignum(_)
            | NodeKind::Float(_)
            | NodeKind::Imaginary(_)
            | NodeKind::Rational(_, _)
            | NodeKind::RImaginary(_, _)
            | NodeKind::String(_)
            | NodeKind::Bytes(_)
            | NodeKind::EncodedString(..)
            | NodeKind::Symbol(_)
    )
}

fn constant_name(id: &ConstantId<'_>) -> Result<String, MonorubyErr> {
    let bytes = id.as_slice();
    std::str::from_utf8(bytes)
        .map(str::to_owned)
        .map_err(|_| MonorubyErr::fatal("prism lowerer: non-utf8 identifier"))
}

/// 1-based line number of `offset` in `source`, used to inline
/// `__LINE__`.
fn source_line_for_offset(source: &[u8], offset: usize) -> usize {
    let upper = offset.min(source.len());
    1 + source[..upper].iter().filter(|&&b| b == b'\n').count()
}

/// Convert Prism's `Integer<'_>` (numeric handle exposing
/// `to_u32_digits`) into a `num::BigInt`.
fn prism_integer_to_bigint(value: &prism::Integer<'_>) -> num::BigInt {
    let (negative, digits) = value.to_u32_digits();
    let mut bytes: Vec<u8> = Vec::with_capacity(digits.len() * 4);
    for &d in digits {
        bytes.extend_from_slice(&d.to_le_bytes());
    }
    let mag = num::BigInt::from_bytes_le(num::bigint::Sign::Plus, &bytes);
    if negative { -mag } else { mag }
}

/// Convert the byte body of a regex literal (Prism's `unescaped()`)
/// into the `NodeKind::String(...)` part used inside ruruby's
/// `RegExp` parts list.
fn regex_body_to_string(bytes: &[u8], _loc: Loc) -> Result<NodeKind, MonorubyErr> {
    match std::str::from_utf8(bytes) {
        Ok(s) => Ok(NodeKind::String(s.to_owned())),
        Err(_) => Err(MonorubyErr::fatal("prism lowerer: non-utf8 regex literal")),
    }
}

/// Pull the option flags out of a regex closing-delimiter slice.
/// Prism reports the closing as the literal closing characters from
/// the source (`b"/"`, `b"/i"`, `b"]i"`, `b"}"`, `b")im"`, …) — the
/// first byte is the closing delimiter character (which varies by
/// `%r{...}` / `%r[...]` / `%r(...)` / `/.../`) and any trailing
/// bytes are option flags.
///
/// Of those, only `i`/`m`/`x`/`n` survive into the regex options
/// string; ruruby drops `o` (compile-once, runtime-only),
/// `u`/`s`/`e` (legacy encoding selectors), and we follow that
/// convention so the resulting flag string can be passed straight
/// into Onigmo (which only knows the four). Without this filter,
/// e.g. `%r[\A...\z]o` would inject `o` into `(?o-mix:...)` and
/// Onigmo errors with `undefined group option`.
fn regex_flags_from_closing(closing: &Location<'_>) -> String {
    let bytes = closing.as_slice();
    let tail = if bytes.is_empty() { bytes } else { &bytes[1..] };
    tail.iter()
        .copied()
        // Onigmo flags (`i` / `m` / `x`) plus the encoding-pin
        // letters (`n` / `u` / `e` / `s`) — `const_regexp` /
        // `gen_regexp` decode the latter into the
        // `RegexpInner::KCODE_*` bits the encoding resolver
        // reads.
        .filter(|b| matches!(b, b'i' | b'm' | b'x' | b'n' | b'u' | b'e' | b's'))
        .map(|b| b as char)
        .collect()
}

/// Wrap a regexp-literal node as `<regex> =~ $_` — the implicit
/// last-read-line match a bare regexp literal performs in a conditional
/// (prism's `MatchLastLineNode` / `InterpolatedMatchLastLineNode`).
fn match_last_line(regex: Node, loc: crate::ast::Loc) -> Node {
    let lastline = Node {
        kind: NodeKind::GlobalVar("$_".to_string()),
        loc,
    };
    Node {
        kind: NodeKind::MethodCall {
            receiver: Box::new(regex),
            method: "=~".to_owned(),
            arglist: Box::new(crate::ast::ArgList {
                args: vec![lastline],
                ..Default::default()
            }),
            safe_nav: false,
        },
        loc,
    }
}

// ---- pattern-matching desugar helpers ----

fn pm_lvar(name: &str, loc: Loc) -> Node {
    Node::new_lvar(name.to_string(), 0, loc)
}

fn pm_assign(target: Node, value: Node) -> Node {
    Node::new_mul_assign(vec![target], vec![value])
}

/// An unconditional binding as a truthy check: `(target = value; true)`.
fn pm_bind(target: Node, value: Node, loc: Loc) -> Node {
    Node::new_comp_stmt(vec![pm_assign(target, value), Node::new_bool(true, loc)], loc)
}

fn pm_and(l: Node, r: Node) -> Node {
    Node::new_binop(BinOp::LAnd, l, r)
}

fn pm_or(l: Node, r: Node) -> Node {
    Node::new_binop(BinOp::LOr, l, r)
}

fn pm_and_all(checks: Vec<Node>, loc: Loc) -> Node {
    let mut it = checks.into_iter();
    match it.next() {
        Some(first) => it.fold(first, pm_and),
        None => Node::new_bool(true, loc),
    }
}

/// `pattern === subject`.
fn pm_teq(pattern: Node, subject: Node) -> Node {
    Node::new_binop(BinOp::Cmp(CmpKind::TEq), pattern, subject)
}

fn pm_const(name: &str, loc: Loc) -> Node {
    Node::new_const(name.to_string(), false, None, vec![], loc)
}

fn pm_respond_to(subject: &Node, method: &str, loc: Loc) -> Node {
    Node::new_mcall(
        subject.clone(),
        "respond_to?".to_string(),
        crate::ast::ArgList::from_args(vec![Node::new_symbol(method.to_string(), loc)]),
        false,
        loc,
    )
}

fn pm_raise_no_matching_pattern(subject: &Node, loc: Loc) -> Node {
    let msg = Node::new_mcall_noarg(subject.clone(), "inspect".to_string(), false, loc);
    Node::new_fcall(
        "raise".to_string(),
        crate::ast::ArgList::from_args(vec![pm_const("NoMatchingPatternError", loc), msg]),
        false,
        loc,
    )
}

fn pm_raise_type_error(msg: &str, loc: Loc) -> Node {
    Node::new_fcall(
        "raise".to_string(),
        crate::ast::ArgList::from_args(vec![
            pm_const("TypeError", loc),
            Node {
                kind: NodeKind::String(msg.to_string()),
                loc,
            },
        ]),
        false,
        loc,
    )
}

/// A guard on an `in` clause reaches us as an `IfNode` / `UnlessNode`
/// wrapping the pattern (patterns proper cannot contain conditionals,
/// so the wrapper is unambiguous). Returns the guard predicate and its
/// polarity, if present.
fn split_pattern_guard<'pr>(pattern: &prism::Node<'pr>) -> Option<(prism::Node<'pr>, bool)> {
    match pattern {
        prism::Node::IfNode { .. } => {
            let n = pattern.as_if_node().unwrap();
            Some((n.predicate(), true))
        }
        prism::Node::UnlessNode { .. } => {
            let n = pattern.as_unless_node().unwrap();
            Some((n.predicate(), false))
        }
        _ => None,
    }
}

/// The actual pattern inside a guard wrapper (see
/// [`split_pattern_guard`]); `None` when the node is not a guard.
fn pattern_guard_body<'pr>(pattern: &prism::Node<'pr>) -> Option<prism::Node<'pr>> {
    let statements = match pattern {
        prism::Node::IfNode { .. } => pattern.as_if_node().unwrap().statements(),
        prism::Node::UnlessNode { .. } => pattern.as_unless_node().unwrap().statements(),
        _ => return None,
    };
    statements.and_then(|s| s.body().iter().next())
}

fn binop_from_name(name: &str) -> Option<BinOp> {
    Some(match name {
        "+" => BinOp::Add,
        "-" => BinOp::Sub,
        "*" => BinOp::Mul,
        "/" => BinOp::Div,
        "%" => BinOp::Rem,
        "**" => BinOp::Exp,
        "<<" => BinOp::Shl,
        ">>" => BinOp::Shr,
        "&" => BinOp::BitAnd,
        "|" => BinOp::BitOr,
        "^" => BinOp::BitXor,
        "<=>" => BinOp::Compare,
        "==" => BinOp::Cmp(CmpKind::Eq),
        "!=" => BinOp::Cmp(CmpKind::Ne),
        "<" => BinOp::Cmp(CmpKind::Lt),
        "<=" => BinOp::Cmp(CmpKind::Le),
        ">" => BinOp::Cmp(CmpKind::Gt),
        ">=" => BinOp::Cmp(CmpKind::Ge),
        "===" => BinOp::Cmp(CmpKind::TEq),
        "=~" => BinOp::Match,
        "!~" => BinOp::Unmatch,
        _ => return None,
    })
}

fn unop_from_name(name: &str) -> Option<UnOp> {
    Some(match name {
        "-@" | "-" => UnOp::Neg,
        "+@" | "+" => UnOp::Pos,
        "~" => UnOp::BitNot,
        "!" => UnOp::Not,
        _ => return None,
    })
}

fn location_to_loc(loc: &Location<'_>) -> Loc {
    let start = loc.start_offset();
    let end = loc.end_offset();
    Loc(start, end.saturating_sub(1).max(start))
}

fn node_kind_name(node: &prism::Node<'_>) -> &'static str {
    use prism::Node::*;
    match node {
        ProgramNode { .. } => "ProgramNode",
        StatementsNode { .. } => "StatementsNode",
        NilNode { .. } => "NilNode",
        TrueNode { .. } => "TrueNode",
        FalseNode { .. } => "FalseNode",
        SelfNode { .. } => "SelfNode",
        IntegerNode { .. } => "IntegerNode",
        FloatNode { .. } => "FloatNode",
        StringNode { .. } => "StringNode",
        SymbolNode { .. } => "SymbolNode",
        CallNode { .. } => "CallNode",
        DefNode { .. } => "DefNode",
        ClassNode { .. } => "ClassNode",
        ModuleNode { .. } => "ModuleNode",
        IfNode { .. } => "IfNode",
        UnlessNode { .. } => "UnlessNode",
        WhileNode { .. } => "WhileNode",
        UntilNode { .. } => "UntilNode",
        ArrayNode { .. } => "ArrayNode",
        HashNode { .. } => "HashNode",
        LocalVariableReadNode { .. } => "LocalVariableReadNode",
        LocalVariableWriteNode { .. } => "LocalVariableWriteNode",
        ConstantReadNode { .. } => "ConstantReadNode",
        ConstantWriteNode { .. } => "ConstantWriteNode",
        InstanceVariableReadNode { .. } => "InstanceVariableReadNode",
        InstanceVariableWriteNode { .. } => "InstanceVariableWriteNode",
        BeginNode { .. } => "BeginNode",
        InterpolatedStringNode { .. } => "InterpolatedStringNode",
        RangeNode { .. } => "RangeNode",
        ReturnNode { .. } => "ReturnNode",
        LocalVariableTargetNode { .. } => "LocalVariableTargetNode",
        MultiWriteNode { .. } => "MultiWriteNode",
        BlockNode { .. } => "BlockNode",
        BlockParametersNode { .. } => "BlockParametersNode",
        BlockArgumentNode { .. } => "BlockArgumentNode",
        LambdaNode { .. } => "LambdaNode",
        SingletonClassNode { .. } => "SingletonClassNode",
        ClassVariableReadNode { .. } => "ClassVariableReadNode",
        ClassVariableWriteNode { .. } => "ClassVariableWriteNode",
        BackReferenceReadNode { .. } => "BackReferenceReadNode",
        NumberedReferenceReadNode { .. } => "NumberedReferenceReadNode",
        ItLocalVariableReadNode { .. } => "ItLocalVariableReadNode",
        ItParametersNode { .. } => "ItParametersNode",
        NumberedParametersNode { .. } => "NumberedParametersNode",
        ImplicitNode { .. } => "ImplicitNode",
        ImplicitRestNode { .. } => "ImplicitRestNode",
        ShareableConstantNode { .. } => "ShareableConstantNode",
        FlipFlopNode { .. } => "FlipFlopNode",
        SourceFileNode { .. } => "SourceFileNode",
        SourceLineNode { .. } => "SourceLineNode",
        SourceEncodingNode { .. } => "SourceEncodingNode",
        ForwardingArgumentsNode { .. } => "ForwardingArgumentsNode",
        ForwardingSuperNode { .. } => "ForwardingSuperNode",
        ForwardingParameterNode { .. } => "ForwardingParameterNode",
        SuperNode { .. } => "SuperNode",
        YieldNode { .. } => "YieldNode",
        DefinedNode { .. } => "DefinedNode",
        AliasMethodNode { .. } => "AliasMethodNode",
        AliasGlobalVariableNode { .. } => "AliasGlobalVariableNode",
        UndefNode { .. } => "UndefNode",
        BreakNode { .. } => "BreakNode",
        NextNode { .. } => "NextNode",
        RedoNode { .. } => "RedoNode",
        RetryNode { .. } => "RetryNode",
        ForNode { .. } => "ForNode",
        CaseNode { .. } => "CaseNode",
        CaseMatchNode { .. } => "CaseMatchNode",
        WhenNode { .. } => "WhenNode",
        InNode { .. } => "InNode",
        ArrayPatternNode { .. } => "ArrayPatternNode",
        HashPatternNode { .. } => "HashPatternNode",
        FindPatternNode { .. } => "FindPatternNode",
        CapturePatternNode { .. } => "CapturePatternNode",
        PinnedExpressionNode { .. } => "PinnedExpressionNode",
        PinnedVariableNode { .. } => "PinnedVariableNode",
        SplatNode { .. } => "SplatNode",
        AssocNode { .. } => "AssocNode",
        AssocSplatNode { .. } => "AssocSplatNode",
        ParenthesesNode { .. } => "ParenthesesNode",
        RegularExpressionNode { .. } => "RegularExpressionNode",
        InterpolatedRegularExpressionNode { .. } => "InterpolatedRegularExpressionNode",
        MatchLastLineNode { .. } => "MatchLastLineNode",
        InterpolatedMatchLastLineNode { .. } => "InterpolatedMatchLastLineNode",
        InterpolatedSymbolNode { .. } => "InterpolatedSymbolNode",
        RationalNode { .. } => "RationalNode",
        ImaginaryNode { .. } => "ImaginaryNode",
        ConstantPathNode { .. } => "ConstantPathNode",
        ConstantPathWriteNode { .. } => "ConstantPathWriteNode",
        ConstantPathOperatorWriteNode { .. } => "ConstantPathOperatorWriteNode",
        ConstantPathOrWriteNode { .. } => "ConstantPathOrWriteNode",
        ConstantPathAndWriteNode { .. } => "ConstantPathAndWriteNode",
        ConstantPathTargetNode { .. } => "ConstantPathTargetNode",
        IndexOperatorWriteNode { .. } => "IndexOperatorWriteNode",
        IndexOrWriteNode { .. } => "IndexOrWriteNode",
        IndexAndWriteNode { .. } => "IndexAndWriteNode",
        IndexTargetNode { .. } => "IndexTargetNode",
        CallOperatorWriteNode { .. } => "CallOperatorWriteNode",
        CallOrWriteNode { .. } => "CallOrWriteNode",
        CallAndWriteNode { .. } => "CallAndWriteNode",
        CallTargetNode { .. } => "CallTargetNode",
        LocalVariableOperatorWriteNode { .. } => "LocalVariableOperatorWriteNode",
        LocalVariableOrWriteNode { .. } => "LocalVariableOrWriteNode",
        LocalVariableAndWriteNode { .. } => "LocalVariableAndWriteNode",
        InstanceVariableOperatorWriteNode { .. } => "InstanceVariableOperatorWriteNode",
        InstanceVariableOrWriteNode { .. } => "InstanceVariableOrWriteNode",
        InstanceVariableAndWriteNode { .. } => "InstanceVariableAndWriteNode",
        InstanceVariableTargetNode { .. } => "InstanceVariableTargetNode",
        GlobalVariableReadNode { .. } => "GlobalVariableReadNode",
        GlobalVariableWriteNode { .. } => "GlobalVariableWriteNode",
        GlobalVariableOperatorWriteNode { .. } => "GlobalVariableOperatorWriteNode",
        GlobalVariableOrWriteNode { .. } => "GlobalVariableOrWriteNode",
        GlobalVariableAndWriteNode { .. } => "GlobalVariableAndWriteNode",
        GlobalVariableTargetNode { .. } => "GlobalVariableTargetNode",
        ClassVariableOperatorWriteNode { .. } => "ClassVariableOperatorWriteNode",
        ClassVariableOrWriteNode { .. } => "ClassVariableOrWriteNode",
        ClassVariableAndWriteNode { .. } => "ClassVariableAndWriteNode",
        ClassVariableTargetNode { .. } => "ClassVariableTargetNode",
        ConstantOperatorWriteNode { .. } => "ConstantOperatorWriteNode",
        ConstantOrWriteNode { .. } => "ConstantOrWriteNode",
        ConstantAndWriteNode { .. } => "ConstantAndWriteNode",
        ConstantTargetNode { .. } => "ConstantTargetNode",
        MatchPredicateNode { .. } => "MatchPredicateNode",
        MatchRequiredNode { .. } => "MatchRequiredNode",
        MatchWriteNode { .. } => "MatchWriteNode",
        PreExecutionNode { .. } => "PreExecutionNode",
        PostExecutionNode { .. } => "PostExecutionNode",
        EmbeddedStatementsNode { .. } => "EmbeddedStatementsNode",
        EmbeddedVariableNode { .. } => "EmbeddedVariableNode",
        ElseNode { .. } => "ElseNode",
        EnsureNode { .. } => "EnsureNode",
        RescueNode { .. } => "RescueNode",
        RescueModifierNode { .. } => "RescueModifierNode",
        ParametersNode { .. } => "ParametersNode",
        OptionalParameterNode { .. } => "OptionalParameterNode",
        OptionalKeywordParameterNode { .. } => "OptionalKeywordParameterNode",
        RequiredKeywordParameterNode { .. } => "RequiredKeywordParameterNode",
        RestParameterNode { .. } => "RestParameterNode",
        KeywordRestParameterNode { .. } => "KeywordRestParameterNode",
        BlockParameterNode { .. } => "BlockParameterNode",
        RequiredParameterNode { .. } => "RequiredParameterNode",
        MultiTargetNode { .. } => "MultiTargetNode",
        XStringNode { .. } => "XStringNode",
        InterpolatedXStringNode { .. } => "InterpolatedXStringNode",
        ArgumentsNode { .. } => "ArgumentsNode",
        _ => "<other>",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use prism::Visit;
    use std::collections::HashSet;

    /// `Visit`or that records every `node_kind_name` it sees, plus
    /// the `Debug`-formatted prefix of any node that landed in the
    /// catch-all `<other>` arm so the failure message names the
    /// missing variant.
    ///
    /// Most variants flow through the generic `visit_branch_node_enter`
    /// / `visit_leaf_node_enter` hooks. A handful (e.g.
    /// `ParametersNode`, `ArgumentsNode`) are dispatched via typed
    /// callbacks from their parents and bypass the enum hook, so we
    /// override those typed hooks individually.
    #[derive(Default)]
    struct Collector {
        names: HashSet<&'static str>,
        unknown: Vec<String>,
    }
    impl Collector {
        fn record(&mut self, node: &prism::Node<'_>) {
            let name = node_kind_name(node);
            if name == "<other>" {
                self.unknown
                    .push(format!("{:?}", node).chars().take(80).collect());
            }
            self.names.insert(name);
        }
    }
    impl<'pr> Visit<'pr> for Collector {
        fn visit_branch_node_enter(&mut self, node: prism::Node<'pr>) {
            self.record(&node);
        }
        fn visit_leaf_node_enter(&mut self, node: prism::Node<'pr>) {
            self.record(&node);
        }
        fn visit_parameters_node(&mut self, node: &prism::ParametersNode<'pr>) {
            self.record(&node.as_node());
            prism::visit_parameters_node(self, node);
        }
        fn visit_arguments_node(&mut self, node: &prism::ArgumentsNode<'pr>) {
            self.record(&node.as_node());
            prism::visit_arguments_node(self, node);
        }
        fn visit_block_parameter_node(&mut self, node: &prism::BlockParameterNode<'pr>) {
            self.record(&node.as_node());
            prism::visit_block_parameter_node(self, node);
        }
        fn visit_else_node(&mut self, node: &prism::ElseNode<'pr>) {
            self.record(&node.as_node());
            prism::visit_else_node(self, node);
        }
        fn visit_ensure_node(&mut self, node: &prism::EnsureNode<'pr>) {
            self.record(&node.as_node());
            prism::visit_ensure_node(self, node);
        }
        fn visit_rescue_node(&mut self, node: &prism::RescueNode<'pr>) {
            self.record(&node.as_node());
            prism::visit_rescue_node(self, node);
        }
    }

    fn collect_names(source: &str) -> HashSet<&'static str> {
        collect(source).0
    }

    fn collect(source: &str) -> (HashSet<&'static str>, Vec<String>) {
        let result = prism::parse(source.as_bytes());
        let node = result.node();
        let mut visitor = Collector::default();
        visitor.visit(&node);
        (visitor.names, visitor.unknown)
    }

    fn assert_contains(names: &HashSet<&'static str>, expected: &[&'static str]) {
        for name in expected {
            assert!(
                names.contains(name),
                "expected node_kind_name to yield `{name}`; got: {:?}",
                names
            );
        }
    }

    /// PR #439: prism wraps the body of a file with a
    /// `# shareable_constant_value:` magic comment in a
    /// `ShareableConstantNode`. The lowerer must unwrap it; without
    /// the handler, parsing errors with "unsupported node".
    #[test]
    fn shareable_constant_value_literal_parses() {
        let source = "# shareable_constant_value: literal\nFOO = [1, 2, 3]\n".to_owned();
        let result = parse_program(source.into(), PathBuf::from("test.rb"));
        assert!(
            result.is_ok(),
            "parse_program should succeed; got {:?}",
            result.err().map(|e| format!("{:?}", e.kind))
        );
    }

    /// And confirm the AST really contains a `ShareableConstantNode`,
    /// so this test would notice if prism stopped emitting one.
    #[test]
    fn shareable_constant_value_emits_shareable_constant_node() {
        let names = collect_names("# shareable_constant_value: literal\nFOO = [1, 2, 3]\n");
        assert_contains(&names, &["ShareableConstantNode", "ConstantWriteNode"]);
    }

    /// Drive a wide swath of Ruby syntax through prism and check that
    /// `node_kind_name` returns a real name (not `<other>`) for every
    /// variant the parser produces. This pins the match arms so a
    /// future prism bump that introduces a new node type forces us
    /// to extend the function.
    #[test]
    fn node_kind_name_no_other_for_broad_syntax() {
        // Multiple snippets so each magic comment / construct lives
        // at the position prism expects.
        let snippets: &[&str] = &[
            // Literals, control flow, classes, methods, parameters.
            r#"
module M
  CONST = 42
  @ivar = 1
  @@cvar = 2
  $gvar = 3
  class C < Object
    def self.foo(x, y = 1, *r, k:, kk: 2, **kw, &b)
      return nil if x.nil?
      unless y
        while true; break; end
        until false; next; end
        loop { redo }
      end
      begin
        raise "oops"
      rescue StandardError => e
        retry
      ensure
      end
      yield 1, 2
    end
  end
end
puts "string", :sym, :"sy#{1}m", 1.5, 1r, 1i, /re/i, %r{x}, `cmd`, "i#{1}p", `e#{1}c`
[1, 2, *a]
{k: 1, **h, "x" => 1}
x..y; x...y
x = 1; x ||= 1; x &&= 1; x += 1
@i ||= 1; @i &&= 1; @i += 1
@@c ||= 1; @@c &&= 1; @@c += 1
$g ||= 1; $g &&= 1; $g += 1
C ||= 1; C &&= 1; C += 1
A::B = 1; A::B ||= 1; A::B &&= 1; A::B += 1
arr[0] = 1; arr[0] ||= 1; arr[0] &&= 1; arr[0] += 1
o.x = 1; o.x ||= 1; o.x &&= 1; o.x += 1
a, b = 1, 2; a, *b, c = [1, 2, 3, 4]
__FILE__; __LINE__; __ENCODING__
@ivar; @@cvar; $gvar; FOO
super; super(1, &b)
-> (a) { a }
class << obj; end
defined?(x)
alias :a :b
alias_method :a, :b
undef :a
$1; $`; $&
nil; true; false; self
case x
when 1, 2 then 0
else 1
end
case x
in [a, b]
in {k: v}
in Integer => n
end
for i in 1..3; end
$0 =~ /pat/
$1
"#,
            // BEGIN/END (pre/post execution) need to be at top level.
            "BEGIN { 1 }\nEND { 2 }\n",
            // Magic-comment-driven nodes need to be at top of file.
            "# shareable_constant_value: literal\nFOO = [1, 2, 3]\n",
            // Numbered / `it` block parameters.
            "[1].each { _1 + _2 }\n[1].each { it + 1 }\n",
            // Forwarding parameters.
            "def f(...); g(...); end\n",
            // Pinned vars / find pattern in a pattern match.
            "x = 1; case [1,2,3]; in [*, ^x, *]; end\n",
            // Flip-flop. Prism emits this only inside a conditional.
            "if (x..y); end\n",
            // `ImplicitRestNode` is the trailing-comma rest in
            // multi-assignment (`a, = [1]`).
            "a, = [1, 2]\n",
        ];

        let mut all = HashSet::new();
        let mut unknown = Vec::new();
        for s in snippets {
            let (names, unk) = collect(s);
            all.extend(names);
            unknown.extend(unk);
        }

        assert!(
            unknown.is_empty(),
            "node_kind_name returned `<other>` for these prism nodes — extend the match arm:\n{:#?}",
            unknown
        );

        // Spot-check a representative cross-section of variants.
        assert_contains(
            &all,
            &[
                "ProgramNode",
                "StatementsNode",
                "ModuleNode",
                "ClassNode",
                "DefNode",
                "ParametersNode",
                "RequiredParameterNode",
                "OptionalParameterNode",
                "RestParameterNode",
                "RequiredKeywordParameterNode",
                "OptionalKeywordParameterNode",
                "KeywordRestParameterNode",
                "BlockParameterNode",
                "ConstantWriteNode",
                "InstanceVariableWriteNode",
                "ClassVariableWriteNode",
                "GlobalVariableReadNode",
                "GlobalVariableWriteNode",
                "LocalVariableWriteNode",
                "CapturePatternNode",
                "ConstantPathWriteNode",
                "IndexOperatorWriteNode",
                "CallOperatorWriteNode",
                "LocalVariableOperatorWriteNode",
                "InstanceVariableOperatorWriteNode",
                "ClassVariableOperatorWriteNode",
                "GlobalVariableOperatorWriteNode",
                "ConstantOperatorWriteNode",
                "IfNode",
                "UnlessNode",
                "WhileNode",
                "UntilNode",
                "BreakNode",
                "NextNode",
                "RedoNode",
                "RetryNode",
                "ReturnNode",
                "YieldNode",
                "BeginNode",
                "RescueNode",
                "EnsureNode",
                "ArrayNode",
                "HashNode",
                "AssocNode",
                "SplatNode",
                "RangeNode",
                "StringNode",
                "InterpolatedStringNode",
                "SymbolNode",
                "InterpolatedSymbolNode",
                "RegularExpressionNode",
                "XStringNode",
                "RationalNode",
                "ImaginaryNode",
                "FloatNode",
                "IntegerNode",
                "NilNode",
                "TrueNode",
                "FalseNode",
                "SelfNode",
                "LambdaNode",
                "BlockNode",
                "BlockParametersNode",
                "SingletonClassNode",
                "DefinedNode",
                "AliasMethodNode",
                "UndefNode",
                "SuperNode",
                "CaseNode",
                "CaseMatchNode",
                "WhenNode",
                "InNode",
                "ArrayPatternNode",
                "HashPatternNode",
                "ForNode",
                "PreExecutionNode",
                "PostExecutionNode",
                "SourceFileNode",
                "SourceLineNode",
                "SourceEncodingNode",
                "ForwardingArgumentsNode",
                "ForwardingParameterNode",
                "ImplicitRestNode",
                "ShareableConstantNode",
                "NumberedParametersNode",
                "ItParametersNode",
                "MultiWriteNode",
                "ArgumentsNode",
                "BackReferenceReadNode",
                "NumberedReferenceReadNode",
                "ConstantReadNode",
                "InstanceVariableReadNode",
                "ClassVariableReadNode",
                "LocalVariableReadNode",
                "CallNode",
            ],
        );
    }

    // ----- # encoding: / # coding: magic-comment handling -----------------
    //
    // Source encoding flows: prism's `magic_comments()` -> SourceInfo's
    // `source_encoding` field -> bytecodegen's `emit_string` / `emit_bytes`
    // -> the resulting `String` literal's `encoding` tag at runtime.
    //
    // These tests pin the end-to-end behaviour by running short scripts
    // through the full parse → bytecode → eval pipeline and reading back
    // `String#encoding.to_s`. Going through the live runtime keeps the
    // test honest: it would notice if any layer (parser, bytecodegen,
    // value constructors) lost the encoding on the way through.
    //
    // These go through the full parse → bytecode → eval pipeline; prism
    // is monoruby's only parser, so `crate::parser::parse_program` is the
    // backend under test.

    /// Drive a short Ruby script through the parser explicitly,
    /// returning the final `Value` and the `Globals` it ran against.
    /// Callers inspect the value with the returned `Globals` (needed
    /// for `Value::inspect`).
    fn run_prism_source(source: &str) -> (crate::Globals, crate::Value) {
        let path = std::path::Path::new("(test)");
        let mut globals = crate::Globals::new_test();
        let parsed = crate::parser::parse_program(source.to_owned(), path)
            .expect("parse_program");
        let fid = crate::bytecodegen::bytecode_compile_script(&mut globals, parsed)
            .expect("bytecode_compile_script");
        let mut executor = crate::executor::Executor::init(&mut globals, "(test)")
            .expect("Executor::init");
        executor.init_stack_limit(&mut globals);
        let val = executor
            .eval_toplevel(&mut globals, fid)
            .expect("eval_toplevel");
        (globals, val)
    }

    fn run_encoding_query(source: &str) -> String {
        let (globals, val) = run_prism_source(source);
        val.inspect(&globals.store)
            .trim_matches('"')
            .to_owned()
    }

    /// Under `# frozen_string_literal: true`, every string literal is a
    /// shared frozen object, so two literals with the same content are
    /// `equal?` (interned) and each is frozen.
    #[test]
    fn frozen_string_literal_interns_and_freezes() {
        let (_g, val) = run_prism_source(
            "# frozen_string_literal: true\n\"abc\".frozen? && \"abc\".equal?(\"abc\")\n",
        );
        assert_eq!(val, crate::Value::bool(true));
    }

    /// Without the pragma, literals stay mutable and each evaluation is a
    /// fresh object.
    #[test]
    fn without_frozen_string_literal_literals_are_mutable() {
        let (_g, val) =
            run_prism_source("!\"abc\".frozen? && !\"abc\".equal?(\"abc\")\n");
        assert_eq!(val, crate::Value::bool(true));
    }

    /// `# encoding: binary` makes every string literal in the file
    /// ASCII-8BIT. Without honouring the comment, `pack`-style spec
    /// files (which all open with `# encoding: binary`) compare a
    /// pack output (ASCII-8BIT) against a literal we'd tagged UTF-8
    /// and the encoding-aware `String#==` returns false.
    #[test]
    fn magic_comment_encoding_binary_tags_literal_ascii8bit() {
        let enc = run_encoding_query("# encoding: binary\n\"hello\".encoding.to_s\n");
        assert_eq!(enc, "ASCII-8BIT");
    }

    /// `# coding: NAME` is the legacy form prism still emits.
    #[test]
    fn magic_comment_coding_alias_resolves() {
        let enc = run_encoding_query("# coding: us-ascii\n\"abc\".encoding.to_s\n");
        assert_eq!(enc, "US-ASCII");
    }

    /// Emacs-style `# -*- coding: NAME -*-` — prism normalises the
    /// `-*-` decoration away and surfaces the same `coding` key.
    #[test]
    fn magic_comment_emacs_style_resolves() {
        let enc = run_encoding_query("# -*- coding: shift_jis -*-\n\"abc\".encoding.to_s\n");
        assert_eq!(enc, "Shift_JIS");
    }

    /// No magic comment -> UTF-8 (Ruby 2.0+ default source encoding).
    #[test]
    fn magic_comment_default_is_utf8() {
        let enc = run_encoding_query("\"abc\".encoding.to_s\n");
        assert_eq!(enc, "UTF-8");
    }

    /// ASCII-compatible national byte encodings (Big5, GBK, …) are now
    /// name-preserved via `Encoding::NamedByte` rather than folded onto
    /// ASCII-8BIT, so `# encoding: big5` reports "Big5".
    #[test]
    fn magic_comment_named_byte_encoding_preserves_name() {
        assert_eq!(
            run_encoding_query("# encoding: big5\n\"abc\".encoding.to_s\n"),
            "Big5"
        );
        assert_eq!(
            run_encoding_query("# encoding: GBK\n\"abc\".encoding.to_s\n"),
            "GBK"
        );
        // ASCII-only content stays SevenBit and inspects normally
        // (not \xHH-escaped) — Big5 is ASCII-compatible.
        assert_eq!(run_encoding_query("# encoding: big5\n\"abc\"\n"), "abc");
    }

    /// `detect_source_encoding` follows CRuby's placement rules.
    #[test]
    fn magic_comment_placement_rules() {
        // Case-insensitive key, mixed-case value.
        assert_eq!(parse_coding_directive(" CoDiNg: bIg5"), Some("bIg5".into()));
        // `=` separator and a junk prefix are allowed.
        assert_eq!(
            parse_coding_directive(" foo encoding = big5"),
            Some("big5".into())
        );
        // `coding` must be immediately followed by `:`/`=`; `encodings:`
        // does not match.
        assert_eq!(parse_coding_directive(" encodings: big5"), None);
        // `fileencoding=` inside a vim modeline is caught by the same scan.
        assert_eq!(
            parse_coding_directive(" vim: filetype=ruby, fileencoding=big5, tabsize=3"),
            Some("big5".into())
        );

        // Only the first line (or the second after a shebang) counts, and
        // only when the line is a comment from its first token.
        assert_eq!(
            detect_source_encoding(b"# encoding: big5\nx = 1\n"),
            Some("big5".into())
        );
        assert_eq!(
            detect_source_encoding(b"#!/usr/bin/ruby\n# encoding: big5\n"),
            Some("big5".into())
        );
        assert_eq!(detect_source_encoding(b"\n# encoding: big5\n"), None);
        assert_eq!(detect_source_encoding(b"1 + 1 # encoding: big5\n"), None);
        assert_eq!(detect_source_encoding(b"x = 1\n"), None);
    }

    /// `detect_frozen_string_literal` follows CRuby's magic-comment rules
    /// for the `frozen_string_literal` pragma.
    #[test]
    fn frozen_string_literal_placement_rules() {
        // Case-insensitive key, `:` or `=` separator, true/false values.
        assert_eq!(parse_frozen_directive(" frozen_string_literal: true"), Some(true));
        assert_eq!(parse_frozen_directive(" Frozen_String_Literal = FALSE"), Some(false));
        // Missing or non-boolean value does not match.
        assert_eq!(parse_frozen_directive(" frozen_string_literal:"), None);
        assert_eq!(parse_frozen_directive(" frozen_string_literal: yes"), None);
        // Key must be followed by `:`/`=`.
        assert_eq!(parse_frozen_directive(" frozen_string_literal true"), None);

        // First line, or after a shebang.
        assert_eq!(
            detect_frozen_string_literal(b"# frozen_string_literal: true\nx = 1\n"),
            Some(true)
        );
        assert_eq!(
            detect_frozen_string_literal(b"#!/usr/bin/ruby\n# frozen_string_literal: true\n"),
            Some(true)
        );
        // May appear on a later comment line in the leading block (e.g.
        // below an `# encoding:` directive).
        assert_eq!(
            detect_frozen_string_literal(
                b"# encoding: euc-jp\n# frozen_string_literal: true\n\nx = 1\n"
            ),
            Some(true)
        );
        // Absent, or after the leading comment block, yields None.
        assert_eq!(detect_frozen_string_literal(b"x = 1\n"), None);
        assert_eq!(
            detect_frozen_string_literal(b"\n# frozen_string_literal: true\n"),
            None
        );
        assert_eq!(
            detect_frozen_string_literal(b"x = 1\n# frozen_string_literal: true\n"),
            None
        );
    }

    /// `\xNN` byte escapes in a `# encoding: binary` source come out
    /// of the lowerer as `NodeKind::Bytes` (their bytes don't UTF-8
    /// decode). The bytecodegen must still tag those as ASCII-8BIT,
    /// not silently UTF-8.
    #[test]
    fn magic_comment_binary_byte_literal_is_ascii8bit() {
        let enc = run_encoding_query("# encoding: binary\n\"\\x80\".encoding.to_s\n");
        assert_eq!(enc, "ASCII-8BIT");
    }

    /// The proximate fix for PR #462: pack output (ASCII-8BIT) and a
    /// `# encoding: binary` literal with the same bytes must compare
    /// equal under the encoding-aware `String#==`. This is the exact
    /// shape of every `core/array/pack/*_spec.rb` assertion.
    #[test]
    fn magic_comment_pack_output_eq_binary_literal() {
        let (_globals, val) =
            run_prism_source("# encoding: binary\n[\"1\"].pack(\"B\") == \"\\x80\"\n");
        assert!(val.as_bool(), "pack output should equal binary literal");
    }

    /// Sanity check on the parser layer in isolation: prism reports
    /// the magic comment, and `parse_program` stashes its value on
    /// the `SourceInfo` for downstream consumers.
    #[test]
    fn parse_program_extracts_source_encoding_from_magic_comment() {
        let result = parse_program(
            "# encoding: binary\nx = 1\n".to_owned().into(),
            PathBuf::from("test.rb"),
        )
        .expect("parse_program");
        assert_eq!(
            result.source_info.source_encoding.as_deref(),
            Some("binary")
        );

        let result =
            parse_program("x = 1\n".to_owned().into(), PathBuf::from("test.rb")).expect("parse_program");
        assert_eq!(result.source_info.source_encoding, None);
    }

    /// CRuby promotes a string literal to UTF-8 the moment it contains
    /// a `\u` escape, even when the source file declares a non-UTF-8
    /// encoding. Prism flags this case via
    /// `PM_STRING_FLAGS_FORCED_UTF8_ENCODING`; the lowerer must lower
    /// it as `EncodedString("UTF-8")` so bytecodegen tags the result
    /// UTF-8 instead of inheriting the file's `# encoding: binary`.
    /// Tested cases: `\u` in binary source, `\u` in shift_jis source,
    /// and the negative — a plain ASCII literal in binary source
    /// stays ASCII-8BIT (no spurious upgrade).
    #[test]
    fn magic_comment_unicode_escape_forces_utf8_in_binary_source() {
        let enc = run_encoding_query("# encoding: binary\n\"\\u{9819}\".encoding.to_s\n");
        assert_eq!(enc, "UTF-8");
    }

    #[test]
    fn magic_comment_unicode_escape_forces_utf8_in_shift_jis_source() {
        let enc = run_encoding_query("# encoding: shift_jis\n\"\\u{1234}\".encoding.to_s\n");
        assert_eq!(enc, "UTF-8");
    }

    #[test]
    fn magic_comment_no_unicode_escape_keeps_source_encoding() {
        let enc = run_encoding_query("# encoding: binary\n\"abc\".encoding.to_s\n");
        assert_eq!(enc, "ASCII-8BIT");
    }
}
