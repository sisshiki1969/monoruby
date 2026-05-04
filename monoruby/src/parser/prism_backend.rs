//! Prism-backed parser entry points.
//!
//! Calls `ruby_prism::parse(...)` and lowers the borrowed Prism
//! `Node<'pr>` tree into the owned `ruruby_parse` AST shape that the
//! rest of monoruby consumes via [`crate::ast::Node`].
//!
//! The lowerer is incremental: only a small subset of node kinds is
//! handled today. When the lowerer encounters something it does not
//! yet recognise it returns [`LowerError::Unsupported`]; the wrapper
//! catches that and silently falls back to the ruruby backend so the
//! interpreter as a whole keeps running while we expand coverage.
//! Set `MONORUBY_PARSER_VERBOSE=1` to log every fallback to stderr.

use std::path::PathBuf;

use ruby_prism::{
    self as prism, ArrayNode, BeginNode, BlockNode, ClassNode, ConstantId, ConstantList,
    ConstantPathNode, ConstantReadNode, ConstantWriteNode, DefNode, FloatNode,
    GlobalVariableReadNode, GlobalVariableWriteNode, HashNode, IfNode, InstanceVariableReadNode,
    InstanceVariableWriteNode, IntegerNode, InterpolatedRegularExpressionNode,
    InterpolatedStringNode, LambdaNode, LocalVariableReadNode, LocalVariableWriteNode, Location,
    ModuleNode, MultiWriteNode, ParametersNode, ProgramNode, RangeNode, RegularExpressionNode,
    RescueNode, ReturnNode, StatementsNode, StringNode, SymbolNode, UnlessNode, UntilNode,
    WhileNode,
};

use crate::ast::{
    BinOp, BlockInfo, CmpKind, Loc, LocalsContext, LvarCollector, NReal, Node, NodeKind, ParamKind,
    ParseResult, SourceInfoRef, UnOp,
};
use crate::globals::MonorubyErr;

use super::ruruby_backend;

pub(super) fn parse_program(code: String, path: PathBuf) -> Result<ParseResult, MonorubyErr> {
    try_prism_then_ruruby(code, path, None::<&DummyContext>, 0, |code, path, ctx, line| {
        ruruby_backend::parse_program_eval(code, path, ctx, line)
    })
}

pub(super) fn parse_program_eval<C: LocalsContext>(
    code: String,
    path: PathBuf,
    extern_context: Option<&C>,
    line_offset: i64,
) -> Result<ParseResult, MonorubyErr> {
    try_prism_then_ruruby(
        code,
        path,
        extern_context,
        line_offset,
        |code, path, ctx, line| ruruby_backend::parse_program_eval(code, path, ctx, line),
    )
}

pub(super) fn parse_program_binding<C: LocalsContext>(
    code: String,
    path: PathBuf,
    context: Option<LvarCollector>,
    extern_context: Option<&C>,
    line_offset: i64,
) -> Result<ParseResult, MonorubyErr> {
    let context_for_fallback = context.clone();
    try_prism_then_ruruby(
        code,
        path,
        extern_context,
        line_offset,
        |code, path, ctx, line| {
            ruruby_backend::parse_program_binding(code, path, context_for_fallback.clone(), ctx, line)
        },
    )
}

fn try_prism_then_ruruby<C, F>(
    code: String,
    path: PathBuf,
    extern_context: Option<&C>,
    line_offset: i64,
    fallback: F,
) -> Result<ParseResult, MonorubyErr>
where
    C: LocalsContext,
    F: FnOnce(String, PathBuf, Option<&C>, i64) -> Result<ParseResult, MonorubyErr>,
{
    match try_prism(&code, path.clone()) {
        Ok(result) => Ok(result),
        Err(reason) => {
            // While the lowerer is incomplete and Prism's stricter
            // grammar diverges from ruruby on some legacy forms (e.g.
            // `expr rescue return X`), every Prism failure — both real
            // parse errors and "lowerer doesn't know this node yet" —
            // falls back to ruruby. Once Prism is the source of truth
            // we'll switch ParseError back to a hard failure.
            if std::env::var("MONORUBY_PARSER_VERBOSE").ok().as_deref() == Some("1") {
                eprintln!(
                    "[prism] falling back to ruruby for {}: {reason:?}",
                    path.display()
                );
            }
            fallback(code, path, extern_context, line_offset)
        }
    }
}

#[derive(Debug)]
enum LowerError {
    /// A genuine parse error reported by Prism (also includes our own
    /// "post-parse rejected" cases). Surfaced to the caller as-is.
    ParseError(MonorubyErr),
    /// Prism parsed the source successfully but the lowerer hit a node
    /// it cannot yet translate. The caller should retry with the ruruby
    /// backend.
    Unsupported(&'static str),
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

fn try_prism(code: &str, path: PathBuf) -> Result<ParseResult, LowerError> {
    let path_display = path.display().to_string();
    let source_info: SourceInfoRef =
        std::rc::Rc::new(ruruby_parse::SourceInfo::new(path, code.to_owned()));

    let result = prism::parse(code.as_bytes());
    if let Some(diag) = result.errors().next() {
        let loc = location_to_loc(&diag.location());
        return Err(LowerError::ParseError(MonorubyErr::parse(
            ruruby_parse::ParseErr {
                kind: ruruby_parse::ParseErrKind::SyntaxError(format!(
                    "prism: {}",
                    diag.message()
                )),
                loc,
                source_info,
            },
        )));
    }

    let mut lowerer = Lowerer::new(code.as_bytes(), path_display);
    let body = lowerer.lower_top(&result.node())?;
    let lvar_collector = lowerer.into_lvars();

    Ok(ParseResult {
        node: body,
        lvar_collector,
        source_info,
    })
}

/// Placeholder `LocalsContext` used when callers don't supply one.
struct DummyContext;

impl LocalsContext for DummyContext {
    fn find_lvar(&self, _name: &str) -> Option<usize> {
        None
    }
}

struct Lowerer<'pr> {
    #[allow(dead_code)]
    source: &'pr [u8],
    /// The path of the file we're lowering. Inlined for `__FILE__`.
    path: String,
    lvars: LvarCollector,
}

impl<'pr> Lowerer<'pr> {
    fn new(source: &'pr [u8], path: String) -> Self {
        Self {
            source,
            path,
            lvars: LvarCollector::new(),
        }
    }

    fn into_lvars(self) -> LvarCollector {
        self.lvars
    }

    /// Top-level entry: lowers a `ProgramNode` to a `CompStmt`.
    fn lower_top(&mut self, node: &prism::Node<'pr>) -> Result<Node, LowerError> {
        match node {
            prism::Node::ProgramNode { .. } => {
                let program: ProgramNode<'_> = node.as_program_node().unwrap();
                self.lower_program(&program)
            }
            other => Err(unsupported("top", other)),
        }
    }

    fn lower_program(&mut self, node: &ProgramNode<'pr>) -> Result<Node, LowerError> {
        let loc = location_to_loc(&node.location());
        // Pre-populate the local variable table from Prism's per-scope
        // locals list so the resulting AST matches what ruruby-parse
        // would have produced (bytecodegen reads `LvarCollector` to
        // assign register slots before walking the tree).
        self.collect_locals(&node.locals())?;
        let stmts = self.lower_statements_into_vec(&node.statements())?;
        Ok(Node {
            kind: NodeKind::CompStmt(stmts),
            loc,
        })
    }

    fn collect_locals(&mut self, locals: &ConstantList<'pr>) -> Result<(), LowerError> {
        for id in locals.iter() {
            let name = constant_name(&id)?;
            self.lvars.insert(&name);
        }
        Ok(())
    }

    fn lower_statements_into_vec(
        &mut self,
        node: &StatementsNode<'pr>,
    ) -> Result<Vec<Node>, LowerError> {
        node.body().iter().map(|n| self.lower_node(&n)).collect()
    }

    fn lower_node(&mut self, node: &prism::Node<'pr>) -> Result<Node, LowerError> {
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
            prism::Node::LocalVariableWriteNode { .. } => {
                self.lower_local_var_write(&node.as_local_variable_write_node().unwrap())?
            }
            prism::Node::IfNode { .. } => self.lower_if(&node.as_if_node().unwrap())?,
            prism::Node::UnlessNode { .. } => {
                self.lower_unless(&node.as_unless_node().unwrap())?
            }
            prism::Node::WhileNode { .. } => self.lower_while(&node.as_while_node().unwrap())?,
            prism::Node::UntilNode { .. } => self.lower_until(&node.as_until_node().unwrap())?,
            prism::Node::ArrayNode { .. } => {
                self.lower_array(&node.as_array_node().unwrap())?
            }
            prism::Node::HashNode { .. } => self.lower_hash(&node.as_hash_node().unwrap())?,
            prism::Node::RangeNode { .. } => {
                self.lower_range(&node.as_range_node().unwrap())?
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
                    None => Node {
                        kind: NodeKind::Nil,
                        loc,
                    },
                };
                Node {
                    kind: NodeKind::Splat(Box::new(expr)),
                    loc,
                }
            }
            prism::Node::DefNode { .. } => self.lower_def(&node.as_def_node().unwrap())?,
            prism::Node::ClassNode { .. } => {
                self.lower_class(&node.as_class_node().unwrap())?
            }
            prism::Node::ModuleNode { .. } => {
                self.lower_module(&node.as_module_node().unwrap())?
            }
            prism::Node::SingletonClassNode { .. } => {
                let n = node.as_singleton_class_node().unwrap();
                let singleton = self.lower_node(&n.expression())?;
                let saved = std::mem::take(&mut self.lvars);
                if let Err(e) = self.collect_locals(&n.locals()) {
                    self.lvars = saved;
                    return Err(e);
                }
                let info_res = self.lower_class_body(n.body(), loc);
                self.lvars = saved;
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
            prism::Node::ReturnNode { .. } => {
                self.lower_return(&node.as_return_node().unwrap())?
            }
            prism::Node::InterpolatedStringNode { .. } => {
                self.lower_interpolated_string(&node.as_interpolated_string_node().unwrap())?
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
                        arglist: Box::new(ruruby_parse::ArgList::default()),
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
                // `if /pat/` (implicit match against `$_`) — ruruby
                // doesn't have a dedicated node for this either; the
                // regex literal is parsed the same way and the
                // implicit match is recognised at bytecodegen.
                let n = node.as_match_last_line_node().unwrap();
                let part = Node {
                    kind: regex_body_to_string(n.unescaped(), location_to_loc(&n.location()))?,
                    loc: location_to_loc(&n.location()),
                };
                let flags = regex_flags_from_closing(&n.closing_loc());
                Node {
                    kind: NodeKind::RegExp(vec![part], flags, true),
                    loc,
                }
            }
            prism::Node::InterpolatedMatchLastLineNode { .. } => {
                let n = node.as_interpolated_match_last_line_node().unwrap();
                let parts = self.lower_interp_parts(n.parts())?;
                let flags = regex_flags_from_closing(&n.closing_loc());
                Node {
                    kind: NodeKind::RegExp(parts, flags, false),
                    loc,
                }
            }
            prism::Node::BeginNode { .. } => {
                self.lower_begin(&node.as_begin_node().unwrap())?
            }
            prism::Node::MultiWriteNode { .. } => {
                self.lower_multi_write(&node.as_multi_write_node().unwrap())?
            }
            prism::Node::LocalVariableOperatorWriteNode { .. } => {
                let n = node.as_local_variable_operator_write_node().unwrap();
                let target = Node {
                    kind: NodeKind::LocalVar(n.depth() as usize, constant_name(&n.name())?),
                    loc: location_to_loc(&n.name_loc()),
                };
                self.build_op_assign(target, &n.binary_operator(), &n.value(), loc)?
            }
            prism::Node::LocalVariableOrWriteNode { .. } => {
                let n = node.as_local_variable_or_write_node().unwrap();
                let target = Node {
                    kind: NodeKind::LocalVar(n.depth() as usize, constant_name(&n.name())?),
                    loc: location_to_loc(&n.name_loc()),
                };
                self.build_short_circuit_assign(BinOp::LOr, target, &n.value(), loc)?
            }
            prism::Node::LocalVariableAndWriteNode { .. } => {
                let n = node.as_local_variable_and_write_node().unwrap();
                let target = Node {
                    kind: NodeKind::LocalVar(n.depth() as usize, constant_name(&n.name())?),
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
                self.build_short_circuit_assign(BinOp::LOr, target, &n.value(), loc)?
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
            prism::Node::LambdaNode { .. } => {
                self.lower_lambda(&node.as_lambda_node().unwrap())?
            }
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
                    return Err(LowerError::Unsupported(
                        "indexed op-assign with block argument",
                    ));
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
                    return Err(LowerError::Unsupported(
                        "indexed ||= with block argument",
                    ));
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
                    return Err(LowerError::Unsupported(
                        "indexed &&= with block argument",
                    ));
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
                let mut arglist = ruruby_parse::ArgList::default();
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
                // attached (`super { ... }`); we don't model that
                // yet, so fall back if one is present.
                let n = node.as_forwarding_super_node().unwrap();
                if n.block().is_some() {
                    return Err(LowerError::Unsupported(
                        "forwarding super with literal block",
                    ));
                }
                Node {
                    kind: NodeKind::Super(None),
                    loc,
                }
            }
            prism::Node::YieldNode { .. } => {
                let n = node.as_yield_node().unwrap();
                let mut arglist = ruruby_parse::ArgList::default();
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
                let param: Vec<(usize, String)> = match index {
                    prism::Node::LocalVariableTargetNode { .. } => {
                        let inner = index.as_local_variable_target_node().unwrap();
                        vec![(inner.depth() as usize, constant_name(&inner.name())?)]
                    }
                    // `for a, b in ...` lands as MultiTargetNode here;
                    // ruruby's `param: Vec<(usize, String)>` could
                    // accommodate that but bytecodegen's lowering of
                    // For only handles a flat list and we'd need to
                    // expand each target. Defer.
                    other => return Err(unsupported("for index", &other)),
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
                let inner = self.lower_node(&n.value())?;
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
                let mut when_branches: Vec<ruruby_parse::CaseBranch> = Vec::new();
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
                            when_branches.push(ruruby_parse::CaseBranch {
                                when: conds,
                                body: Box::new(body),
                            });
                        }
                        // `case ... in pattern` arms (CaseMatchNode)
                        // produce `InNode`s instead; not supported here.
                        other => return Err(unsupported("case branch", &other)),
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
                // start offset.
                let off = node.location().start_offset();
                let line = source_line_for_offset(self.source, off) as i64;
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
                let entry = ruruby_parse::RescueEntry {
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
            other => return Err(unsupported("expression", other)),
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

    fn lower_local_var_read(
        &self,
        node: &LocalVariableReadNode<'pr>,
    ) -> Result<Node, LowerError> {
        let name = constant_name(&node.name())?;
        Ok(Node {
            kind: NodeKind::LocalVar(node.depth() as usize, name),
            loc: location_to_loc(&node.location()),
        })
    }

    fn lower_local_var_write(
        &mut self,
        node: &LocalVariableWriteNode<'pr>,
    ) -> Result<Node, LowerError> {
        let name = constant_name(&node.name())?;
        let depth = node.depth() as usize;
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
    ) -> Result<Node, LowerError> {
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

    fn lower_if(&mut self, node: &IfNode<'pr>) -> Result<Node, LowerError> {
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
                other => return Err(unsupported("if subsequent", &other)),
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

    fn lower_unless(&mut self, node: &UnlessNode<'pr>) -> Result<Node, LowerError> {
        let loc = location_to_loc(&node.location());
        let cond = self.lower_node(&node.predicate())?;
        let stmts = node.statements();
        let then_for_unless = self.lower_optional_statements(stmts.as_ref(), loc)?;
        let else_for_unless = match node.else_clause() {
            Some(e) => {
                let inner = e.statements();
                self.lower_optional_statements(
                    inner.as_ref(),
                    location_to_loc(&e.location()),
                )?
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

    fn lower_while(&mut self, node: &WhileNode<'pr>) -> Result<Node, LowerError> {
        let loc = location_to_loc(&node.location());
        let cond = self.lower_node(&node.predicate())?;
        let stmts = node.statements();
        let body = self.lower_optional_statements(stmts.as_ref(), loc)?;
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

    fn lower_until(&mut self, node: &UntilNode<'pr>) -> Result<Node, LowerError> {
        let loc = location_to_loc(&node.location());
        let cond = self.lower_node(&node.predicate())?;
        let stmts = node.statements();
        let body = self.lower_optional_statements(stmts.as_ref(), loc)?;
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

    fn lower_array(&mut self, node: &ArrayNode<'pr>) -> Result<Node, LowerError> {
        let loc = location_to_loc(&node.location());
        let mut elements: Vec<Node> = Vec::new();
        let mut all_const = true;
        for n in node.elements().iter() {
            let lowered = self.lower_node(&n)?;
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

    fn lower_hash(&mut self, node: &HashNode<'pr>) -> Result<Node, LowerError> {
        let loc = location_to_loc(&node.location());
        let mut pairs: Vec<(Node, Node)> = Vec::new();
        let mut splat: Vec<Node> = Vec::new();
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
                    splat.push(inner);
                }
                other => return Err(unsupported("hash element", &other)),
            }
        }
        Ok(Node {
            kind: NodeKind::Hash(pairs, splat),
            loc,
        })
    }

    fn lower_range(&mut self, node: &RangeNode<'pr>) -> Result<Node, LowerError> {
        let loc = location_to_loc(&node.location());
        let start = match node.left() {
            Some(n) => Some(self.lower_node(&n)?),
            None => None,
        };
        let end = match node.right() {
            Some(n) => Some(self.lower_node(&n)?),
            None => None,
        };
        let is_const = match (&start, &end) {
            (Some(s), Some(e)) => {
                is_constant_literal(&s.kind) && is_constant_literal(&e.kind)
            }
            _ => false,
        };
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

    /// Lower the body of a class / module / class-shovel definition,
    /// returning a fully-built `BlockInfo` with the body wrapped in
    /// the implicit `Begin { rescue: [], else_: None, ensure: None }`
    /// envelope ruruby-parse produces.
    fn lower_class_body(
        &mut self,
        body: Option<prism::Node<'pr>>,
        loc: Loc,
    ) -> Result<BlockInfo, LowerError> {
        let saved = std::mem::take(&mut self.lvars);
        // ruruby's class / module body has a specific `Begin { body }`
        // shape that bytecodegen depends on:
        //   - empty body  -> `Begin { body: CompStmt([]) }`
        //   - 1 statement -> `Begin { body: <bare expr> }` (NO CompStmt)
        //   - N>=2        -> `Begin { body: CompStmt([s1, s2, ...]) }`
        // Lowering through plain `lower_node(StatementsNode)` always
        // wraps in `CompStmt`, even for single statements; that
        // throws off bytecodegen's class-definition register
        // arithmetic and panics later in `expand_array`. Use the
        // statement-compact lowering instead so the shape matches.
        let body_res = match body {
            Some(b) => match b {
                prism::Node::StatementsNode { .. } => {
                    let stmts_node = b.as_statements_node().unwrap();
                    let stmts_loc = location_to_loc(&stmts_node.location());
                    match self.lower_statements_into_vec(&stmts_node) {
                        Ok(mut stmts) => match stmts.len() {
                            0 => Ok(Node {
                                kind: NodeKind::CompStmt(vec![]),
                                loc: stmts_loc,
                            }),
                            1 => Ok(stmts.pop().unwrap()),
                            _ => Ok(Node {
                                kind: NodeKind::CompStmt(stmts),
                                loc: stmts_loc,
                            }),
                        },
                        Err(e) => Err(e),
                    }
                }
                _ => self.lower_node(&b),
            },
            None => Ok(Node {
                kind: NodeKind::CompStmt(vec![]),
                loc,
            }),
        };
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
                let class_lvars = std::mem::replace(&mut self.lvars, saved);
                Ok(BlockInfo {
                    params: vec![],
                    body: Box::new(body),
                    lvar: class_lvars,
                    loc,
                })
            }
            Err(e) => {
                self.lvars = saved;
                Err(e)
            }
        }
    }

    fn lower_class(&mut self, node: &ClassNode<'pr>) -> Result<Node, LowerError> {
        let loc = location_to_loc(&node.location());
        let (base, name) = self.split_class_path(&node.constant_path())?;
        let superclass = match node.superclass() {
            Some(s) => Some(Box::new(self.lower_node(&s)?)),
            None => None,
        };
        // The class body's `locals` list lives on the ClassNode itself
        // (Prism scopes it to the class definition). Seed the lowerer
        // with those before lowering the body.
        let saved = std::mem::take(&mut self.lvars);
        if let Err(e) = self.collect_locals(&node.locals()) {
            self.lvars = saved;
            return Err(e);
        }
        let info_res = self.lower_class_body(node.body(), loc);
        // `lower_class_body` already swapped lvars back — restore the
        // outer scope from `saved` here regardless of outcome.
        self.lvars = saved;
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

    fn lower_module(&mut self, node: &ModuleNode<'pr>) -> Result<Node, LowerError> {
        let loc = location_to_loc(&node.location());
        let (base, name) = self.split_class_path(&node.constant_path())?;
        let saved = std::mem::take(&mut self.lvars);
        if let Err(e) = self.collect_locals(&node.locals()) {
            self.lvars = saved;
            return Err(e);
        }
        let info_res = self.lower_class_body(node.body(), loc);
        self.lvars = saved;
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
    ) -> Result<(Option<Box<Node>>, String), LowerError> {
        match path {
            prism::Node::ConstantReadNode { .. } => {
                let n = path.as_constant_read_node().unwrap();
                Ok((None, constant_name(&n.name())?))
            }
            prism::Node::ConstantPathNode { .. } => {
                let n = path.as_constant_path_node().unwrap();
                let name = match n.name() {
                    Some(id) => constant_name(&id)?,
                    None => return Err(LowerError::Unsupported("class path missing name")),
                };
                let base = match n.parent() {
                    Some(parent) => Some(Box::new(self.lower_const_chain(&parent)?)),
                    // `class ::Foo; end`: ruruby would set toplevel:true on
                    // the resulting Const but ClassDef has no such field;
                    // mark unsupported until we wire the toplevel base in.
                    None => return Err(LowerError::Unsupported("toplevel class path")),
                };
                Ok((base, name))
            }
            other => Err(unsupported("class path", other)),
        }
    }

    fn lower_constant_read(&self, node: &ConstantReadNode<'pr>) -> Result<Node, LowerError> {
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

    fn lower_constant_path(&mut self, node: &ConstantPathNode<'pr>) -> Result<Node, LowerError> {
        self.lower_const_chain(&node.as_node())
    }

    /// Flatten a `ConstantPathNode` chain (or the leaf `ConstantReadNode`
    /// it terminates on) into ruruby's `Const { toplevel, parent,
    /// prefix, name }` representation. Pure constant-only chains like
    /// `A::B::C` flatten into `prefix: ["A", "B"], name: "C"`. Chains
    /// rooted at `::` flatten with `toplevel: true`. Chains rooted at
    /// a non-constant expression (e.g. `expr::C`) become
    /// `parent: Some(<lowered expr>)`.
    fn lower_const_chain(&mut self, node: &prism::Node<'pr>) -> Result<Node, LowerError> {
        let loc = location_to_loc(&node.location());
        let chain = self.collect_const_chain(node)?;
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

    fn collect_const_chain(
        &mut self,
        node: &prism::Node<'pr>,
    ) -> Result<ConstChain, LowerError> {
        match node {
            prism::Node::ConstantReadNode { .. } => {
                let n = node.as_constant_read_node().unwrap();
                Ok(ConstChain {
                    toplevel: false,
                    parent: None,
                    prefix: vec![],
                    name: constant_name(&n.name())?,
                })
            }
            prism::Node::ConstantPathNode { .. } => {
                let n = node.as_constant_path_node().unwrap();
                let name = match n.name() {
                    Some(id) => constant_name(&id)?,
                    None => return Err(LowerError::Unsupported("constant path missing name")),
                };
                match n.parent() {
                    None => Ok(ConstChain {
                        toplevel: true,
                        parent: None,
                        prefix: vec![],
                        name,
                    }),
                    Some(p) => match self.collect_const_chain(&p) {
                        Ok(mut inner) => {
                            if inner.parent.is_some() {
                                // Already rooted in a non-constant
                                // expression — push our leaf into the
                                // prefix, keep the same parent.
                                inner.prefix.push(std::mem::take(&mut inner.name));
                                inner.name = name;
                                Ok(inner)
                            } else {
                                // Pure constant chain: slide the inner
                                // leaf into the prefix and replace it
                                // with `name`.
                                inner.prefix.push(std::mem::take(&mut inner.name));
                                inner.name = name;
                                Ok(inner)
                            }
                        }
                        // Non-constant parent: lower the parent as a
                        // value expression and root the chain on it.
                        Err(LowerError::Unsupported("non-constant constant path prefix")) => {
                            let parent_node = self.lower_node(&p)?;
                            Ok(ConstChain {
                                toplevel: false,
                                parent: Some(Box::new(parent_node)),
                                prefix: vec![],
                                name,
                            })
                        }
                        Err(e) => Err(e),
                    },
                }
            }
            // Treat anything else as a non-constant prefix; the caller
            // (the `ConstantPathNode` arm above) recovers and lowers it
            // as a value expression.
            _ => Err(LowerError::Unsupported("non-constant constant path prefix")),
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
    ) -> Result<Node, LowerError> {
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
    ) -> Result<Node, LowerError> {
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
    ) -> Result<CallBlock, LowerError> {
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
            other => Err(unsupported("call block", other)),
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
    ) -> Result<(), LowerError> {
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
                for elem in kh.elements().iter() {
                    match elem {
                        prism::Node::AssocNode { .. } => {
                            let assoc = elem.as_assoc_node().unwrap();
                            let key = assoc.key();
                            // Only static-symbol keys (`a:` form) fit
                            // ruruby's `(String, Node)` shape;
                            // anything else (string interpolation,
                            // dynamic key, …) goes through `lower_hash`
                            // by lowering the whole `KeywordHashNode`
                            // as a regular hash arg instead.
                            let name = match &key {
                                prism::Node::SymbolNode { .. } => {
                                    let s = key.as_symbol_node().unwrap();
                                    match s.unescaped() {
                                        bytes if !bytes.is_empty() => {
                                            std::str::from_utf8(bytes)
                                                .map(str::to_owned)
                                                .map_err(|_| LowerError::Unsupported(
                                                    "non-utf8 keyword arg name",
                                                ))?
                                        }
                                        _ => return Err(LowerError::Unsupported(
                                            "empty keyword arg name",
                                        )),
                                    }
                                }
                                _ => return Err(LowerError::Unsupported(
                                    "non-symbol keyword arg key",
                                )),
                            };
                            let value = self.lower_node(&assoc.value())?;
                            kw_args.push((name, value));
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
                            hash_splat.push(inner);
                        }
                        other => return Err(unsupported("kwarg element", &other)),
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
    ) -> Result<Node, LowerError> {
        let recv = receiver
            .ok_or(LowerError::Unsupported("attr op-assign without receiver"))?;
        let receiver_node = self.lower_node(recv)?;
        let method = constant_name(read_name)?;
        Ok(Node {
            kind: NodeKind::MethodCall {
                receiver: Box::new(receiver_node),
                method,
                arglist: Box::new(ruruby_parse::ArgList::default()),
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
    ) -> Result<Node, LowerError> {
        let recv = receiver
            .ok_or(LowerError::Unsupported("index target without receiver"))?;
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
    ) -> Result<Node, LowerError> {
        let op_name = constant_name(op_id)?;
        let op = binop_from_name(&op_name)
            .ok_or(LowerError::Unsupported("unknown op-assign operator"))?;
        let value = self.lower_node(value)?;
        Ok(Node {
            kind: NodeKind::AssignOp(op, Box::new(target), Box::new(value)),
            loc,
        })
    }

    /// `target ||= value` -> `BinOp(LOr, target, MulAssign([target],
    /// [value]))` and likewise for `&&=` -> `LAnd`. Prism reports
    /// these via `*OrWriteNode` / `*AndWriteNode`; we collapse them
    /// into the same shape ruruby's parser produces (a short-circuit
    /// binop wrapped around an inline assignment).
    fn build_short_circuit_assign(
        &mut self,
        op: BinOp,
        target: Node,
        value: &prism::Node<'pr>,
        loc: Loc,
    ) -> Result<Node, LowerError> {
        let value = self.lower_node(value)?;
        let assign = Node {
            kind: NodeKind::MulAssign(vec![target.clone()], vec![value]),
            loc,
        };
        Ok(Node {
            kind: NodeKind::BinOp(op, Box::new(target), Box::new(assign)),
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
    fn lower_multi_write(&mut self, node: &MultiWriteNode<'pr>) -> Result<Node, LowerError> {
        let loc = location_to_loc(&node.location());
        let mut lhs: Vec<Node> = Vec::new();
        for n in node.lefts().iter() {
            lhs.push(self.lower_assign_target(&n)?);
        }
        if let Some(rest) = node.rest() {
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
                other => return Err(unsupported("multi-write rest", &other)),
            }
        }
        for n in node.rights().iter() {
            lhs.push(self.lower_assign_target(&n)?);
        }

        let value = node.value();
        let rhs: Vec<Node> = match value {
            prism::Node::ArrayNode { .. } => {
                let arr = value.as_array_node().unwrap();
                if arr.opening_loc().is_none() {
                    // Implicit array (no `[...]`): the source wrote
                    // `a, b = 1, 2` and Prism wrapped the RHS into an
                    // ArrayNode for the assignment. Unpack so each
                    // element becomes its own RHS slot, matching
                    // ruruby's `MulAssign(lhs, [v1, v2, ...])` form.
                    let mut out = Vec::new();
                    for elem in arr.elements().iter() {
                        out.push(self.lower_node(&elem)?);
                    }
                    out
                } else {
                    // Explicit `a, b = [1, 2]` keeps the array as a
                    // single RHS so it gets splatted at runtime.
                    vec![self.lower_node(&value)?]
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
    fn lower_assign_target(
        &mut self,
        node: &prism::Node<'pr>,
    ) -> Result<Node, LowerError> {
        let loc = location_to_loc(&node.location());
        Ok(match node {
            prism::Node::LocalVariableTargetNode { .. } => {
                let n = node.as_local_variable_target_node().unwrap();
                Node {
                    kind: NodeKind::LocalVar(n.depth() as usize, constant_name(&n.name())?),
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
                    return Err(LowerError::Unsupported(
                        "index target with block argument",
                    ));
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
                    None => return Err(LowerError::Unsupported(
                        "constant path target missing name",
                    )),
                };
                let chain = match n.parent() {
                    None => ConstChain {
                        toplevel: true,
                        parent: None,
                        prefix: vec![],
                        name,
                    },
                    Some(p) => {
                        let mut inner = self.collect_const_chain(&p)?;
                        if inner.parent.is_some() {
                            return Err(LowerError::Unsupported(
                                "constant path target nested under non-constant parent",
                            ));
                        }
                        inner.prefix.push(std::mem::take(&mut inner.name));
                        inner.name = name;
                        inner
                    }
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
            // Nested destructure (`((a, b), c) = ...`) and call /
            // attribute targets (`a.foo = x`) need their own per-
            // shape lowering. Defer.
            other => return Err(unsupported("assign target", &other)),
        })
    }

    fn lower_begin(&mut self, node: &BeginNode<'pr>) -> Result<Node, LowerError> {
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

        let mut rescue_entries: Vec<ruruby_parse::RescueEntry> = Vec::new();
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
    ) -> Result<ruruby_parse::RescueEntry, LowerError> {
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
        Ok(ruruby_parse::RescueEntry {
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
    ) -> Result<Node, LowerError> {
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
    ) -> Result<Node, LowerError> {
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
    ) -> Result<Vec<Node>, LowerError> {
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
                other => return Err(unsupported("interpolation part", &other)),
            }
        }
        Ok(out)
    }

    /// `42i` / `1.5i` / `1ri` — Prism nests an `IntegerNode` /
    /// `FloatNode` / `RationalNode` inside `ImaginaryNode::numeric`.
    /// ruruby uses `Imaginary(NReal)` for the integer / float cases
    /// and a separate `RImaginary(BigInt, BigInt)` when the inner
    /// part is a rational.
    fn lower_imaginary(
        &self,
        numeric: &prism::Node<'pr>,
        loc: Loc,
    ) -> Result<Node, LowerError> {
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
            other => Err(unsupported("imaginary numeric", other)),
        }
    }

    fn lower_regex(&self, node: &RegularExpressionNode<'pr>) -> Result<Node, LowerError> {
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
    ) -> Result<Node, LowerError> {
        let loc = location_to_loc(&node.location());
        let parts = self.lower_interp_parts(node.parts())?;
        let flags = regex_flags_from_closing(&node.closing_loc());
        Ok(Node {
            kind: NodeKind::RegExp(parts, flags, false),
            loc,
        })
    }

    /// Build the value carried by a `break` / `next`. Mirrors the
    /// `return` shape: 0 args -> Nil, 1 arg -> bare, more -> Array.
    fn lower_jump_value(
        &mut self,
        args: Option<prism::ArgumentsNode<'pr>>,
        loc: Loc,
    ) -> Result<Node, LowerError> {
        let mut values: Vec<Node> = Vec::new();
        if let Some(arglist) = args {
            for arg in arglist.arguments().iter() {
                values.push(self.lower_node(&arg)?);
            }
        }
        Ok(match values.len() {
            0 => Node {
                kind: NodeKind::Nil,
                loc,
            },
            1 => values.into_iter().next().unwrap(),
            _ => {
                let all_const = values.iter().all(|n| is_constant_literal(&n.kind));
                Node {
                    kind: NodeKind::Array(values, all_const),
                    loc,
                }
            }
        })
    }

    fn lower_return(&mut self, node: &ReturnNode<'pr>) -> Result<Node, LowerError> {
        let loc = location_to_loc(&node.location());
        let mut values: Vec<Node> = Vec::new();
        if let Some(arglist) = node.arguments() {
            for arg in arglist.arguments().iter() {
                values.push(self.lower_node(&arg)?);
            }
        }
        let inner = match values.len() {
            0 => Node {
                kind: NodeKind::Nil,
                loc,
            },
            1 => values.into_iter().next().unwrap(),
            _ => {
                let all_const = values.iter().all(|n| is_constant_literal(&n.kind));
                Node {
                    kind: NodeKind::Array(values, all_const),
                    loc,
                }
            }
        };
        Ok(Node {
            kind: NodeKind::Return(Box::new(inner)),
            loc,
        })
    }

    fn lower_constant_write(&mut self, node: &ConstantWriteNode<'pr>) -> Result<Node, LowerError> {
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
    fn lower_def(&mut self, node: &DefNode<'pr>) -> Result<Node, LowerError> {
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

        let saved = std::mem::take(&mut self.lvars);
        let result =
            (|this: &mut Self| -> Result<(Vec<ruruby_parse::FormalParam>, Node), LowerError> {
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
                let body_inner = match node.body() {
                    Some(b) => this.lower_node(&b)?,
                    None => Node {
                        kind: NodeKind::Nil,
                        loc,
                    },
                };
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
                let method_lvars = std::mem::replace(&mut self.lvars, saved);
                let info = Box::new(BlockInfo {
                    params,
                    body: Box::new(body),
                    lvar: method_lvars,
                    loc,
                });
                let kind = match singleton_receiver {
                    Some(recv) => {
                        NodeKind::SingletonMethodDef(Box::new(recv), name, info)
                    }
                    None => NodeKind::MethodDef(name, info),
                };
                Ok(Node { kind, loc })
            }
            Err(e) => {
                self.lvars = saved;
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
    fn lower_lambda(&mut self, node: &LambdaNode<'pr>) -> Result<Node, LowerError> {
        let loc = location_to_loc(&node.location());
        let saved = std::mem::take(&mut self.lvars);
        let result =
            (|this: &mut Self| -> Result<(Vec<ruruby_parse::FormalParam>, Node), LowerError> {
                let params = match node.parameters() {
                    None => Vec::new(),
                    Some(p) => match p {
                        prism::Node::BlockParametersNode { .. } => {
                            let bp = p.as_block_parameters_node().unwrap();
                            if bp.locals().iter().next().is_some() {
                                return Err(LowerError::Unsupported("lambda shadow locals"));
                            }
                            match bp.parameters() {
                                Some(pn) => this.lower_parameters(&pn)?,
                                None => Vec::new(),
                            }
                        }
                        // `->(a, b) { ... }` with full ParametersNode
                        // (no BlockParametersNode wrapper) is allowed
                        // in Prism for `->(...)` form; handle it the
                        // same way.
                        prism::Node::ParametersNode { .. } => {
                            let pn = p.as_parameters_node().unwrap();
                            this.lower_parameters(&pn)?
                        }
                        other => return Err(unsupported("lambda parameters", &other)),
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
                let lambda_lvars = std::mem::replace(&mut self.lvars, saved);
                Ok(Node {
                    kind: NodeKind::Lambda(Box::new(BlockInfo {
                        params,
                        body: Box::new(body),
                        lvar: lambda_lvars,
                        loc,
                    })),
                    loc,
                })
            }
            Err(e) => {
                self.lvars = saved;
                Err(e)
            }
        }
    }

    fn lower_block(&mut self, node: &BlockNode<'pr>) -> Result<Node, LowerError> {
        let loc = location_to_loc(&node.location());
        // Block scope owns its own LvarCollector. Save the outer one
        // and install a fresh table for the block body. The body
        // lowering is wrapped so an error inside it doesn't leak the
        // partially-built block scope into the outer lowerer.
        let saved = std::mem::take(&mut self.lvars);
        let result = (|this: &mut Self| -> Result<(Vec<ruruby_parse::FormalParam>, Node), LowerError> {
            // Parameters first (see comment on the def-node lowerer for
            // why), then top up the rest from `node.locals()`.
            let params = match node.parameters() {
                None => Vec::new(),
                Some(p) => match p {
                    prism::Node::BlockParametersNode { .. } => {
                        let bp = p.as_block_parameters_node().unwrap();
                        if bp.locals().iter().next().is_some() {
                            return Err(LowerError::Unsupported("block shadow locals"));
                        }
                        match bp.parameters() {
                            Some(pn) => this.lower_parameters(&pn)?,
                            None => Vec::new(),
                        }
                    }
                    other => return Err(unsupported("block parameters", &other)),
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
                let block_lvars = std::mem::replace(&mut self.lvars, saved);
                Ok(Node {
                    kind: NodeKind::Lambda(Box::new(BlockInfo {
                        params,
                        body: Box::new(body),
                        lvar: block_lvars,
                        loc,
                    })),
                    loc,
                })
            }
            Err(e) => {
                self.lvars = saved;
                Err(e)
            }
        }
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
    ) -> Result<Vec<ruruby_parse::FormalParam>, LowerError> {
        let mut out: Vec<ruruby_parse::FormalParam> = Vec::new();

        // Required positional: `def f(a, b)` -> Param("a"), Param("b")
        for n in params.requireds().iter() {
            match n {
                prism::Node::RequiredParameterNode { .. } => {
                    let inner = n.as_required_parameter_node().unwrap();
                    let name = constant_name(&inner.name())?;
                    self.lvars.insert(&name);
                    out.push(ruruby_parse::FormalParam {
                        kind: ParamKind::Param(name),
                        loc: location_to_loc(&inner.location()),
                    });
                }
                // `|(a, b)|` block-destructure: Prism wraps it in a
                // `MultiTargetNode` whose `lefts` list holds the
                // individual `RequiredParameterNode` leaves. ruruby's
                // shape is `ParamKind::Destruct(Vec<(name, loc)>)` —
                // flat, no splat / post entries — so we reject nested
                // destructure (`|((a, b), c)|`) and `|(*rest)|` for
                // now.
                prism::Node::MultiTargetNode { .. } => {
                    let mt = n.as_multi_target_node().unwrap();
                    if mt.rest().is_some() {
                        return Err(LowerError::Unsupported(
                            "destructure param with splat",
                        ));
                    }
                    if mt.rights().iter().next().is_some() {
                        return Err(LowerError::Unsupported(
                            "destructure param with post element",
                        ));
                    }
                    let mut destruct: Vec<(String, Loc)> = Vec::new();
                    for el in mt.lefts().iter() {
                        match el {
                            prism::Node::RequiredParameterNode { .. } => {
                                let inner = el.as_required_parameter_node().unwrap();
                                let name = constant_name(&inner.name())?;
                                self.lvars.insert(&name);
                                destruct.push((name, location_to_loc(&inner.location())));
                            }
                            other => {
                                return Err(unsupported("destructure param leaf", &other));
                            }
                        }
                    }
                    if destruct.is_empty() {
                        return Err(LowerError::Unsupported("empty destructure param"));
                    }
                    // Match ruruby's loc-merging convention so
                    // bytecodegen reports matching source spans.
                    let merged =
                        destruct.iter().map(|(_, l)| *l).reduce(|a, b| a.merge(b)).unwrap();
                    out.push(ruruby_parse::FormalParam {
                        kind: ParamKind::Destruct(destruct),
                        loc: merged,
                    });
                }
                other => return Err(unsupported("required param", &other)),
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
                    out.push(ruruby_parse::FormalParam {
                        kind: ParamKind::Optional(name, Box::new(default)),
                        loc: location_to_loc(&inner.location()),
                    });
                }
                other => return Err(unsupported("optional param", &other)),
            }
        }

        // Rest: `*rest` or `*` (anonymous)
        if let Some(rest) = params.rest() {
            match rest {
                prism::Node::RestParameterNode { .. } => {
                    let inner = rest.as_rest_parameter_node().unwrap();
                    let name = match inner.name() {
                        Some(id) => Some(constant_name(&id)?),
                        None => None,
                    };
                    if let Some(n) = &name {
                        self.lvars.insert(n);
                    }
                    out.push(ruruby_parse::FormalParam {
                        kind: ParamKind::Rest(name),
                        loc: location_to_loc(&inner.location()),
                    });
                }
                // `ImplicitRestNode` (trailing `,` in block params)
                // and other rest shapes need their own handling.
                other => return Err(unsupported("rest param", &other)),
            }
        }

        // Post (after `*rest`): `def f(*rest, c, d)` -> Post("c"), Post("d")
        for n in params.posts().iter() {
            match n {
                prism::Node::RequiredParameterNode { .. } => {
                    let inner = n.as_required_parameter_node().unwrap();
                    let name = constant_name(&inner.name())?;
                    self.lvars.insert(&name);
                    out.push(ruruby_parse::FormalParam {
                        kind: ParamKind::Post(Some(name)),
                        loc: location_to_loc(&inner.location()),
                    });
                }
                other => return Err(unsupported("post param", &other)),
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
                    out.push(ruruby_parse::FormalParam {
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
                    out.push(ruruby_parse::FormalParam {
                        kind: ParamKind::Keyword(name, Some(Box::new(default))),
                        loc: location_to_loc(&inner.location()),
                    });
                }
                other => return Err(unsupported("keyword param", &other)),
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
                        None => None,
                    };
                    if let Some(n) = &name_opt {
                        self.lvars.insert_kwrest_param(n.clone());
                    }
                    out.push(ruruby_parse::FormalParam {
                        kind: ParamKind::KWRest(name_opt),
                        loc,
                    });
                }
                prism::Node::ForwardingParameterNode { .. } => {
                    let loc = location_to_loc(&kr.location());
                    self.lvars.insert_delegate_param();
                    out.push(ruruby_parse::FormalParam {
                        kind: ParamKind::Forwarding,
                        loc,
                    });
                }
                other => return Err(unsupported("keyword rest param", &other)),
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
            out.push(ruruby_parse::FormalParam {
                kind: ParamKind::Block(name),
                loc,
            });
        }

        Ok(out)
    }

    fn lower_call(
        &mut self,
        node: &prism::CallNode<'pr>,
        loc: Loc,
    ) -> Result<Node, LowerError> {
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
            return Ok(Node {
                kind: NodeKind::Index {
                    base: Box::new(base),
                    index: args,
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
                let mut arglist = ruruby_parse::ArgList::default();
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

        let mut arglist = ruruby_parse::ArgList::default();
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
                        safe_nav: false,
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
            | NodeKind::Symbol(_)
    )
}

fn constant_name(id: &ConstantId<'_>) -> Result<String, LowerError> {
    let bytes = id.as_slice();
    std::str::from_utf8(bytes)
        .map(str::to_owned)
        .map_err(|_| LowerError::Unsupported("non-utf8 identifier"))
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
fn regex_body_to_string(bytes: &[u8], _loc: Loc) -> Result<NodeKind, LowerError> {
    match std::str::from_utf8(bytes) {
        Ok(s) => Ok(NodeKind::String(s.to_owned())),
        Err(_) => Err(LowerError::Unsupported("non-utf8 regex literal")),
    }
}

/// Pull the option flags out of a regex closing-delimiter slice. The
/// slice looks like `b"/"`, `b"/i"`, `b"/im"`, etc.; we drop the
/// leading `/` and return the rest as an owned UTF-8 string.
fn regex_flags_from_closing(closing: &Location<'_>) -> String {
    let bytes = closing.as_slice();
    let tail = bytes.strip_prefix(b"/").unwrap_or(bytes);
    std::str::from_utf8(tail)
        .map(str::to_owned)
        .unwrap_or_default()
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

fn unsupported(context: &'static str, node: &prism::Node<'_>) -> LowerError {
    let kind = node_kind_name(node);
    if std::env::var("MONORUBY_PARSER_VERBOSE").ok().as_deref() == Some("1") {
        if kind == "<other>" {
            // Surface the precise Prism type name when we hit the
            // node_kind_name catch-all so it's easy to extend.
            eprintln!("[prism] unsupported {context} node: <other> // {node:?}");
        } else {
            eprintln!("[prism] unsupported {context} node: {kind}");
        }
    }
    LowerError::Unsupported(kind)
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
        ImplicitRestNode { .. } => "ImplicitRestNode",
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
