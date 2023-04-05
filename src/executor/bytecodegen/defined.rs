use super::*;

impl IrContext {
    pub(super) fn gen_defined(&mut self, node: Node, loc: Loc) -> Result<()> {
        match node.kind {
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
            | NodeKind::Splat(_)
            | NodeKind::Bool(_)
            | NodeKind::Nil
            | NodeKind::SelfValue
            | NodeKind::MulAssign(..)
            | NodeKind::AssignOp(..) => {
                let s = Self::defined_str(&node).unwrap();
                self.emit_string(None, s.to_string());
                return Ok(());
            }
            NodeKind::Yield(_) => {}
            _ => return Err(MonorubyErr::unsupported_node(node, self.sourceinfo.clone())),
        }
        let ret = Some(self.push().into());
        self.emit(BcIr::Defined { ret, ty: 0 }, loc);
        Ok(())
    }

    fn defined_str(node: &Node) -> Option<&'static str> {
        let s = match &node.kind {
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
            NodeKind::Begin { box body, .. } => return Self::defined_str(body),
            NodeKind::CompStmt(v) => match v.last() {
                Some(node) => return Self::defined_str(node),
                None => "nil",
            },
            NodeKind::Lambda(_) => unreachable!(),
        };
        Some(s)
    }
}
