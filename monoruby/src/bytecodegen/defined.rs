use super::*;

impl BytecodeGen {
    pub(super) fn gen_defined(&mut self, node: Node) -> Result<()> {
        let res = defined_str(&node);
        let reg = self.push().into();
        self.emit_string(reg, res.to_string());
        let exit_label = self.new_label();
        let nil_label = self.new_label();
        self.check_defined(node, nil_label, reg, true)?;
        self.emit_br(exit_label);
        self.apply_label(nil_label);
        self.emit_nil(reg);
        self.apply_label(exit_label);
        Ok(())
    }

    fn check_defined(&mut self, node: Node, nil_label: Label, ret: BcReg, top: bool) -> Result<()> {
        match node.kind {
            NodeKind::MulAssign(lhs, rhs) => {
                if lhs.len() == 1 && rhs.len() == 1 {
                    let lhs = lhs[0].clone();
                    if let NodeKind::Index { base: box b, .. } = lhs.kind {
                        let name = IdentId::_INDEX_ASSIGN;
                        if top {
                            let body_start = self.new_label();
                            let body_end = self.new_label();
                            self.apply_label(body_start);
                            let recv = self.gen_temp_expr(b)?;
                            self.apply_label(body_end);
                            self.emit(BcIr::DefinedMethod { ret, recv, name }, node.loc);
                            self.exception_table.push(ExceptionEntry {
                                range: body_start..body_end,
                                rescue: Some(nil_label),
                                ensure: None,
                                err_reg: None,
                            });
                        } else {
                            self.check_defined(b, nil_label, ret, false)?;
                        }
                        for n in rhs {
                            self.check_defined(n, nil_label, ret, false)?
                        }
                    }
                }
            }
            NodeKind::Array(v, ..) | NodeKind::CompStmt(v) => {
                for n in v {
                    self.check_defined(n, nil_label, ret, false)?
                }
            }
            NodeKind::Splat(box n)
            | NodeKind::Begin { body: box n, .. }
            | NodeKind::UnOp(_, box n) => self.check_defined(n, nil_label, ret, false)?,
            NodeKind::BinOp(op, box l, box r) => {
                if top {
                    let body_start = self.new_label();
                    let body_end = self.new_label();
                    self.apply_label(body_start);
                    let recv = self.gen_temp_expr(l)?;
                    self.apply_label(body_end);
                    let name = match op {
                        BinOp::Add => IdentId::_ADD,
                        BinOp::Sub => IdentId::_SUB,
                        BinOp::Mul => IdentId::_MUL,
                        BinOp::Div => IdentId::_DIV,
                        BinOp::Rem => IdentId::_REM,
                        BinOp::Shl => IdentId::_SHL,
                        BinOp::Shr => IdentId::_SHR,
                        BinOp::BitAnd => IdentId::_BAND,
                        BinOp::BitOr => IdentId::_BOR,
                        BinOp::BitXor => IdentId::_BXOR,
                        BinOp::Exp => IdentId::_POW,
                        BinOp::Cmp(cmp) => match cmp {
                            CmpKind::Eq => IdentId::_EQ,
                            CmpKind::Ne => IdentId::_NEQ,
                            CmpKind::Lt => IdentId::_LT,
                            CmpKind::Gt => IdentId::_GT,
                            CmpKind::Le => IdentId::_LE,
                            CmpKind::Ge => IdentId::_GE,
                            CmpKind::TEq => IdentId::_TEQ,
                            CmpKind::Cmp => IdentId::_CMP,
                        },
                        _ => unimplemented!(),
                    };
                    self.emit(BcIr::DefinedMethod { ret, recv, name }, node.loc);
                    self.exception_table.push(ExceptionEntry {
                        range: body_start..body_end,
                        rescue: Some(nil_label),
                        ensure: None,
                        err_reg: None,
                    });
                } else {
                    self.check_defined(l, nil_label, ret, false)?;
                }
                self.check_defined(r, nil_label, ret, false)?;
            }
            NodeKind::Ident(name) => {
                let name = IdentId::get_id_from_string(name);
                self.emit(
                    BcIr::DefinedMethod {
                        ret,
                        recv: BcReg::Self_,
                        name,
                    },
                    node.loc,
                );
            }
            NodeKind::FuncCall {
                method, arglist, ..
            } => {
                let name = IdentId::get_id_from_string(method);
                self.emit(
                    BcIr::DefinedMethod {
                        ret,
                        recv: BcReg::Self_,
                        name,
                    },
                    node.loc,
                );
                for n in arglist.args {
                    self.check_defined(n, nil_label, ret, false)?
                }
            }
            NodeKind::MethodCall {
                receiver: box r,
                method,
                arglist,
                ..
            } => {
                let name = IdentId::get_id_from_string(method);
                if top {
                    let body_start = self.new_label();
                    let body_end = self.new_label();
                    self.apply_label(body_start);
                    let recv = self.gen_temp_expr(r)?;
                    self.apply_label(body_end);
                    self.emit(BcIr::DefinedMethod { ret, recv, name }, node.loc);
                    self.exception_table.push(ExceptionEntry {
                        range: body_start..body_end,
                        rescue: Some(nil_label),
                        ensure: None,
                        err_reg: None,
                    });
                } else {
                    self.check_defined(r, nil_label, ret, false)?;
                }
                for n in arglist.args {
                    self.check_defined(n, nil_label, ret, false)?;
                }
            }
            NodeKind::Index {
                base: box b,
                index: v,
            } => {
                if top {
                    let body_start = self.new_label();
                    let body_end = self.new_label();
                    self.apply_label(body_start);
                    let recv = self.gen_temp_expr(b)?;
                    self.apply_label(body_end);
                    self.emit(
                        BcIr::DefinedMethod {
                            ret,
                            recv,
                            name: IdentId::_INDEX,
                        },
                        node.loc,
                    );
                    self.exception_table.push(ExceptionEntry {
                        range: body_start..body_end,
                        rescue: Some(nil_label),
                        ensure: None,
                        err_reg: None,
                    });
                } else {
                    self.check_defined(b, nil_label, ret, false)?;
                }
                for n in v {
                    self.check_defined(n, nil_label, ret, false)?;
                }
            }
            NodeKind::Yield(_) => {
                self.emit(BcIr::DefinedYield { ret }, node.loc);
            }
            NodeKind::Const {
                toplevel,
                parent: _,
                prefix,
                name,
            } => {
                let name = IdentId::get_id_from_string(name);
                let prefix = prefix
                    .into_iter()
                    .map(IdentId::get_id_from_string)
                    .collect();
                self.emit(
                    BcIr::DefinedConst {
                        ret,
                        toplevel,
                        prefix,
                        name,
                    },
                    node.loc,
                );
            }
            NodeKind::LocalVar(..) => {}
            NodeKind::InstanceVar(name) => {
                let name = IdentId::get_id_from_string(name);
                self.emit(BcIr::DefinedIvar { ret, name }, node.loc);
            }
            NodeKind::GlobalVar(name) => {
                let name = IdentId::get_id_from_string(name);
                self.emit(BcIr::DefinedGvar { ret, name }, node.loc);
            }
            NodeKind::SpecialVar(..) => {}
            NodeKind::ClassVar(..) | NodeKind::Super(_) | NodeKind::Lambda(_) => {
                return Err(MonorubyErr::unsupported_node(
                    node.clone(),
                    self.sourceinfo.clone(),
                ))
            }
            _ => {}
        };
        Ok(())
    }
}

fn defined_str(node: &Node) -> &'static str {
    match &node.kind {
        NodeKind::Integer(_)
        | NodeKind::Bignum(_)
        | NodeKind::Float(_)
        | NodeKind::Imaginary(_)
        | NodeKind::Symbol(_)
        | NodeKind::String(_)
        | NodeKind::InterporatedString(_)
        | NodeKind::RegExp(..)
        | NodeKind::Range { .. }
        | NodeKind::Hash { .. }
        | NodeKind::Command(_)
        | NodeKind::Array(..)
        | NodeKind::AliasMethod(..)
        | NodeKind::Defined(..)
        | NodeKind::If { .. }
        | NodeKind::While { .. }
        | NodeKind::For { .. }
        | NodeKind::Case { .. }
        | NodeKind::Break(_)
        | NodeKind::Next(_)
        | NodeKind::Return(_)
        | NodeKind::Redo
        | NodeKind::ClassDef { .. }
        | NodeKind::SingletonClassDef { .. }
        | NodeKind::MethodDef(..)
        | NodeKind::SingletonMethodDef(..)
        | NodeKind::Splat(_) => "expression",
        NodeKind::Bool(true) => "true",
        NodeKind::Bool(false) => "false",
        NodeKind::Nil => "nil",
        NodeKind::SelfValue => "self",
        NodeKind::Yield(_) => "yield",
        NodeKind::MulAssign(lhs, rhs) if lhs.len() == 1 && rhs.len() == 1 => match lhs[0].kind {
            NodeKind::Index { .. } | NodeKind::MethodCall { .. } => "method",
            _ => "assignment",
        },
        NodeKind::MulAssign(..) | NodeKind::AssignOp(..) => "assignment",
        NodeKind::LocalVar(..) => "local-variable",
        NodeKind::InstanceVar(..) => "instance-variable",
        NodeKind::GlobalVar(..) | NodeKind::SpecialVar(..) => "global-variable",
        NodeKind::ClassVar(..) => "class-variable",
        NodeKind::Const { .. } => "constant",
        NodeKind::BinOp(..)
        | NodeKind::FuncCall { .. }
        | NodeKind::MethodCall { .. }
        | NodeKind::Ident(_)
        | NodeKind::Index { .. }
        | NodeKind::UnOp(..) => "method",
        NodeKind::Super(_) => "super",
        NodeKind::Begin { box body, .. } => defined_str(body),
        NodeKind::CompStmt(v) => match v.last() {
            Some(node) => defined_str(node),
            None => "nil",
        },
        NodeKind::Lambda(_) => unreachable!(),
    }
}
