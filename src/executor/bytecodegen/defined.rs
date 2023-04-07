use super::*;

impl IrContext {
    pub(super) fn gen_defined(&mut self, node: Node) -> Result<()> {
        let res = defined_str(&node);
        let reg = self.next_reg().into();
        self.emit_string(None, res.to_string());
        let exit_label = self.new_label();
        self.check_defined(node, exit_label, reg)?;
        self.apply_label(exit_label);
        Ok(())
    }

    fn check_defined(&mut self, node: Node, exit_label: usize, reg: BcReg) -> Result<()> {
        match node.kind {
            NodeKind::Array(v, ..) | NodeKind::CompStmt(v) => {
                for n in v {
                    self.check_defined(n, exit_label, reg)?
                }
            }
            NodeKind::Splat(box n)
            | NodeKind::Begin { body: box n, .. }
            | NodeKind::UnOp(_, box n) => self.check_defined(n, exit_label, reg)?,
            NodeKind::BinOp(_, box l, box r) => {
                self.check_defined(l, exit_label, reg)?;
                self.check_defined(r, exit_label, reg)?;
            }
            NodeKind::Ident(name) => {
                let name = IdentId::get_id_from_string(name);
                self.emit(
                    BcIr::DefinedMethod {
                        ret: reg,
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
                    self.check_defined(n, exit_label, reg)?
                }
                self.emit(
                    BcIr::DefinedMethod {
                        ret: reg,
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
                self.check_defined(r.clone(), exit_label, reg)?;
                for n in arglist.args {
                    self.check_defined(n, exit_label, reg)?;
                }
                let recv = self.gen_temp_expr(r)?;
                self.emit(
                    BcIr::DefinedMethod {
                        ret: reg,
                        recv,
                        name,
                    },
                    node.loc,
                );
            }
            NodeKind::Index {
                base: box b,
                index: v,
            } => {
                self.check_defined(b, exit_label, reg)?;
                for n in v {
                    self.check_defined(n, exit_label, reg)?;
                }
            }
            NodeKind::Yield(_) => {
                self.emit(BcIr::DefinedYield { ret: reg }, node.loc);
            }
            NodeKind::Const {
                toplevel,
                parent: _,
                prefix,
                name,
            } => {
                let name = IdentId::get_id_from_string(name);
                let prefix = prefix.iter().map(|s| IdentId::get_id(s)).collect();
                let ret = reg;
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
            NodeKind::InstanceVar(..) => {}
            NodeKind::GlobalVar(..) => {}
            NodeKind::SpecialVar(..) => {}
            NodeKind::ClassVar(..) => {}
            NodeKind::Super(_) | NodeKind::Lambda(_) => {
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
