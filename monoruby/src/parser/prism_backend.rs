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
    InstanceVariableWriteNode, IntegerNode, InterpolatedStringNode, LocalVariableReadNode,
    LocalVariableWriteNode, Location, ModuleNode, ParametersNode, ProgramNode, RangeNode,
    RescueNode, ReturnNode, StatementsNode, StringNode, SymbolNode, UnlessNode, UntilNode,
    WhileNode,
};

use crate::ast::{
    BinOp, BlockInfo, CmpKind, Loc, LocalsContext, LvarCollector, Node, NodeKind, ParamKind,
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
            prism::Node::DefNode { .. } => self.lower_def(&node.as_def_node().unwrap())?,
            prism::Node::ClassNode { .. } => {
                self.lower_class(&node.as_class_node().unwrap())?
            }
            prism::Node::ModuleNode { .. } => {
                self.lower_module(&node.as_module_node().unwrap())?
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
            prism::Node::BeginNode { .. } => {
                self.lower_begin(&node.as_begin_node().unwrap())?
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
        let body_res = match body {
            Some(b) => self.lower_node(&b),
            None => Ok(Node {
                kind: NodeKind::Nil,
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
        &self,
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

    fn lower_constant_path(&self, node: &ConstantPathNode<'pr>) -> Result<Node, LowerError> {
        self.lower_const_chain(&node.as_node())
    }

    /// Flatten a `ConstantPathNode` chain (or the leaf `ConstantReadNode`
    /// it terminates on) into ruruby's `Const { toplevel, parent,
    /// prefix, name }` representation. Pure constant-only chains like
    /// `A::B::C` flatten into `prefix: ["A", "B"], name: "C"`. Chains
    /// rooted at `::` flatten with `toplevel: true`. Chains rooted at
    /// an arbitrary expression (e.g. `expr::C`) are not yet supported
    /// here — they fall back to ruruby.
    fn lower_const_chain(&self, node: &prism::Node<'pr>) -> Result<Node, LowerError> {
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

    fn collect_const_chain(&self, node: &prism::Node<'pr>) -> Result<ConstChain, LowerError> {
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
                    Some(p) => {
                        let mut inner = self.collect_const_chain(&p)?;
                        if inner.parent.is_some() {
                            return Err(LowerError::Unsupported(
                                "constant path nested under non-constant parent",
                            ));
                        }
                        // Slide the inner chain's leaf name into the
                        // prefix and put `name` in its place. After
                        // the swap, `inner` describes the same chain
                        // one level deeper.
                        inner.prefix.push(std::mem::take(&mut inner.name));
                        inner.name = name;
                        Ok(inner)
                    }
                }
            }
            // Non-constant prefix expressions are handled by ruruby.
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
            Some(ref_node) => Some(Box::new(self.lower_rescue_target(&ref_node)?)),
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

    /// `rescue ... => target`. The target is one of: a local var (the
    /// usual `=> e`), an instance / class / global var, or a constant.
    /// Prism uses `*TargetNode` variants for the bare-name cases that
    /// only make sense as assignment LHS.
    fn lower_rescue_target(
        &self,
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
            other => return Err(unsupported("rescue target", &other)),
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
        let mut parts: Vec<Node> = Vec::new();
        for part in node.parts().iter() {
            match part {
                prism::Node::StringNode { .. } => {
                    parts.push(self.lower_string(&part.as_string_node().unwrap()));
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
                    parts.push(lowered);
                }
                prism::Node::EmbeddedVariableNode { .. } => {
                    let inner = part.as_embedded_variable_node().unwrap();
                    parts.push(self.lower_node(&inner.variable())?);
                }
                other => return Err(unsupported("interpolation part", &other)),
            }
        }
        Ok(Node {
            kind: NodeKind::InterporatedString(parts),
            loc,
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

        if node.receiver().is_some() {
            // `def self.foo` / `def obj.foo` — needs SingletonMethodDef
            // and is not yet handled.
            return Err(LowerError::Unsupported("singleton method def"));
        }

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
                Ok(Node {
                    kind: NodeKind::MethodDef(
                        name,
                        Box::new(BlockInfo {
                            params,
                            body: Box::new(body),
                            lvar: method_lvars,
                            loc,
                        }),
                    ),
                    loc,
                })
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
                // `|(a, b)|` destructure on block params lands here as
                // `MultiTargetNode`; not yet supported.
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
        if let Some(arglist) = node.arguments() {
            for n in arglist.arguments().iter() {
                args.push(self.lower_node(&n)?);
            }
        }
        let block = match node.block() {
            None => None,
            Some(block_node) => match block_node {
                prism::Node::BlockNode { .. } => {
                    let bn = block_node.as_block_node().unwrap();
                    Some(Box::new(self.lower_block(&bn)?))
                }
                // BlockArgumentNode (`&proc`) and other forms are not
                // supported yet — fall back so monoruby still runs.
                other => return Err(unsupported("call block", &other)),
            },
        };

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
        arglist.block = block;

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
