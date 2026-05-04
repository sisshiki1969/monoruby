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
    self as prism, ArrayNode, ConstantId, ConstantList, FloatNode, HashNode, IfNode, IntegerNode,
    LocalVariableReadNode, LocalVariableWriteNode, Location, ProgramNode, RangeNode,
    StatementsNode, StringNode, SymbolNode, UnlessNode, UntilNode, WhileNode,
};

use crate::ast::{
    BinOp, CmpKind, Loc, LocalsContext, LvarCollector, Node, NodeKind, ParseResult, SourceInfoRef,
    UnOp,
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

        // 0-arg "method" calls of the form `-x`, `+x`, `~x`, `!x` are
        // really unary operators in our IR. Detect them before the
        // binop fast path so they don't get mistaken for a 1-arg
        // arithmetic call.
        if let Some(recv) = receiver_opt.as_ref()
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
        LocalVariableTargetNode { .. } => "LocalVariableTargetNode",
        MultiWriteNode { .. } => "MultiWriteNode",
        _ => "<other>",
    }
}
