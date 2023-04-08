use super::*;

impl IrContext {
    pub(super) fn gen_defined(&mut self, node: Node) -> Result<()> {
        let res = defined_str(&node);
        let reg = self.next_reg().into();
        self.emit_string(None, res.to_string());
        let exit_label = self.new_label();
        let nil_label = self.new_label();
        self.check_defined(node, nil_label, reg)?;
        self.emit_br(exit_label);
        self.apply_label(nil_label);
        self.emit_nil(Some(reg));
        self.apply_label(exit_label);
        Ok(())
    }

    fn check_defined(&mut self, node: Node, nil_label: usize, ret: BcReg) -> Result<()> {
        match node.kind {
            NodeKind::Array(v, ..) | NodeKind::CompStmt(v) => {
                for n in v {
                    self.check_defined(n, nil_label, ret)?
                }
            }
            NodeKind::Splat(box n)
            | NodeKind::Begin { body: box n, .. }
            | NodeKind::UnOp(_, box n) => self.check_defined(n, nil_label, ret)?,
            NodeKind::BinOp(_, box l, box r) => {
                self.check_defined(l, nil_label, ret)?;
                self.check_defined(r, nil_label, ret)?;
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
                for n in arglist.args {
                    self.check_defined(n, nil_label, ret)?
                }
                self.emit(
                    BcIr::DefinedMethod {
                        ret,
                        recv: BcReg::Self_,
                        name,
                    },
                    node.loc,
                );
            }
            NodeKind::MethodCall {
                receiver: box r,
                method,
                arglist,
                ..
            } => {
                let name = IdentId::get_id_from_string(method);
                self.check_defined(r.clone(), nil_label, ret)?;
                for n in arglist.args {
                    self.check_defined(n, nil_label, ret)?;
                }
                let body_start = self.new_label();
                let body_end = self.new_label();
                self.apply_label(body_start);
                let recv = self.gen_temp_expr(r)?;
                self.apply_label(body_end);
                self.emit(BcIr::DefinedMethod { ret, recv, name }, node.loc);
                self.exception_table
                    .push((body_start..body_end, nil_label, None, None));
            }
            NodeKind::Index {
                base: box b,
                index: v,
            } => {
                self.check_defined(b, nil_label, ret)?;
                for n in v {
                    self.check_defined(n, nil_label, ret)?;
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
                    .map(|s| IdentId::get_id_from_string(s))
                    .collect();
                let ret = ret;
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
        | NodeKind::Return(_)
        | NodeKind::Next(_)
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
