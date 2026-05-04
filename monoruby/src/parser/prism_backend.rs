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
    self as prism, FloatNode, IntegerNode, Location, ProgramNode, StatementsNode, StringNode,
    SymbolNode,
};

use crate::ast::{
    BinOp, Loc, LocalsContext, LvarCollector, Node, NodeKind, ParseResult, SourceInfoRef,
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

fn try_prism(code: &str, path: PathBuf) -> Result<ParseResult, LowerError> {
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

    let mut lowerer = Lowerer::new(code.as_bytes());
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
    lvars: LvarCollector,
}

impl<'pr> Lowerer<'pr> {
    fn new(source: &'pr [u8]) -> Self {
        Self {
            source,
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
        let stmts = self.lower_statements_into_vec(&node.statements())?;
        Ok(Node {
            kind: NodeKind::CompStmt(stmts),
            loc,
        })
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
        if let Some(arglist) = node.arguments() {
            for n in arglist.arguments().iter() {
                args.push(self.lower_node(&n)?);
            }
        }
        if node.block().is_some() {
            return Err(LowerError::Unsupported("CallNode with block"));
        }

        // Common binary operators come back from Prism as a `CallNode`
        // with a one-element arg list and the operator as the name.
        if let Some(recv) = receiver_opt.as_ref()
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
        eprintln!("[prism] unsupported {context} node: {kind}");
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
        _ => "<other>",
    }
}
