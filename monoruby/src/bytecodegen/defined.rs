use super::*;

impl<'a> BytecodeGen<'a> {
    pub(super) fn gen_defined(&mut self, node: Node) -> Result<()> {
        let dst = self.push().into();
        match &node.kind {
            NodeKind::Super(..) => {
                self.emit(BytecodeInst::DefinedSuper { dst }, node.loc);
            }
            NodeKind::Yield(..) => {
                self.emit(BytecodeInst::DefinedYield { dst }, node.loc);
            }
            NodeKind::GlobalVar(name) => {
                self.emit(
                    BytecodeInst::DefinedGvar {
                        dst,
                        name: IdentId::get_id(name),
                    },
                    node.loc,
                );
            }
            _ => {
                let res = defined_str(&node);
                // CRuby returns a frozen String from `defined?`.
                self.emit_frozen_string(dst, res);
                let exit_label = self.new_label();
                let nil_label = self.new_label();
                self.check_defined(node, nil_label, dst, true)?;
                self.emit_br(exit_label);
                self.apply_label(nil_label);
                self.emit_nil(dst);
                self.apply_label(exit_label);
            }
        }
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
                            self.emit(BytecodeInst::DefinedMethod { ret, recv, name }, node.loc);
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
            NodeKind::Hash(v, splat) => {
                for (k, v) in v {
                    self.check_defined(k, nil_label, ret, false)?;
                    self.check_defined(v, nil_label, ret, false)?;
                }
                for (_, s) in splat {
                    self.check_defined(s, nil_label, ret, false)?;
                }
            }
            NodeKind::Splat(box n) | NodeKind::Begin { body: box n, .. } => {
                self.check_defined(n, nil_label, ret, false)?
            }
            NodeKind::UnOp(_, box n) => {
                // `defined?(not X)` / `defined?(!X)` / `defined?(-X)` parse
                // the operator as a method call `X.op`. When the operand `X`
                // is itself a method call, CRuby *evaluates* it (as the
                // receiver of the operator) — executing its side effects and
                // reporting "method", or swallowing any exception it raises
                // to `nil`. A bare variable/const operand keeps the ordinary
                // definedness recursion (an undefined operand ⇒ `nil`).
                let evaluate_operand = top
                    && matches!(
                        n.kind,
                        NodeKind::FuncCall { .. }
                            | NodeKind::Ident(_)
                            | NodeKind::MethodCall { .. }
                            | NodeKind::Index { .. }
                            | NodeKind::BinOp(..)
                            | NodeKind::UnOp(..)
                    );
                if evaluate_operand {
                    let body_start = self.new_label();
                    let body_end = self.new_label();
                    self.apply_label(body_start);
                    let _ = self.gen_temp_expr(n)?;
                    self.apply_label(body_end);
                    self.exception_table.push(ExceptionEntry {
                        range: body_start..body_end,
                        rescue: Some(nil_label),
                        ensure: None,
                        err_reg: None,
                    });
                    // `ret` already holds "method"; the operator method
                    // (`!`, `-@`, …) is defined on every object, so there is
                    // nothing more to check.
                } else {
                    self.check_defined(n, nil_label, ret, false)?;
                }
            }
            NodeKind::BinOp(op, box l, box r) => {
                // `&&` / `||` (and `and`/`or`) are not method calls:
                // CRuby `defined?(a && b)` is always "expression" and
                // the operands are *not* checked. Keep the "expression"
                // string set by `gen_defined` and never jump to nil.
                if matches!(op, BinOp::LAnd | BinOp::LOr) {
                    return Ok(());
                }
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
                        BinOp::Compare => IdentId::_CMP,
                        BinOp::Cmp(cmp) => match cmp {
                            CmpKind::Eq => IdentId::_EQ,
                            CmpKind::Ne => IdentId::_NEQ,
                            CmpKind::Lt => IdentId::_LT,
                            CmpKind::Gt => IdentId::_GT,
                            CmpKind::Le => IdentId::_LE,
                            CmpKind::Ge => IdentId::_GE,
                            CmpKind::TEq => IdentId::_TEQ,
                        },
                        BinOp::Match => IdentId::_MATCH,
                        BinOp::Unmatch => IdentId::_UNMATCH,
                        // LAnd/LOr handled above; all BinOp variants are
                        // now covered.
                        BinOp::LAnd | BinOp::LOr => unreachable!(),
                    };
                    self.emit(BytecodeInst::DefinedMethod { ret, recv, name }, node.loc);
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
                // `__ENCODING__` is a pseudo-variable, always defined.
                if name == "__ENCODING__" {
                    return Ok(());
                }
                let name = IdentId::get_id_from_string(name);
                self.emit(
                    BytecodeInst::DefinedMethod {
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
                    BytecodeInst::DefinedMethod {
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
                    self.emit(BytecodeInst::DefinedMethod { ret, recv, name }, node.loc);
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
            NodeKind::Super { .. } => {
                self.emit(BytecodeInst::DefinedSuper { dst: ret }, node.loc);
            }
            NodeKind::Yield(_) => {
                self.emit(BytecodeInst::DefinedYield { dst: ret }, node.loc);
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
                        BytecodeInst::DefinedMethod {
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
            NodeKind::Const {
                toplevel,
                parent,
                prefix,
                name,
            } => {
                let name = IdentId::get_id_from_string(name);
                let prefix = prefix
                    .into_iter()
                    .map(IdentId::get_id_from_string)
                    .collect();
                let base = if let Some(box parent) = parent {
                    // Evaluate the parent expression; if it raises, defined? yields nil.
                    let body_start = self.new_label();
                    let body_end = self.new_label();
                    self.apply_label(body_start);
                    let base = self.gen_temp_expr(parent)?;
                    self.apply_label(body_end);
                    self.exception_table.push(ExceptionEntry {
                        range: body_start..body_end,
                        rescue: Some(nil_label),
                        ensure: None,
                        err_reg: None,
                    });
                    Some(base)
                } else {
                    None
                };
                self.emit(
                    BytecodeInst::DefinedConst {
                        ret,
                        base,
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
                self.emit(BytecodeInst::DefinedIvar { ret, name }, node.loc);
            }
            NodeKind::GlobalVar(name) => {
                let name = IdentId::get_id_from_string(name);
                self.emit(BytecodeInst::DefinedGvar { dst: ret, name }, node.loc);
            }
            NodeKind::ClassVar(name) => {
                let name = IdentId::get_id_from_string(name);
                self.emit(BytecodeInst::DefinedCvar { dst: ret, name }, node.loc);
            }
            NodeKind::Lambda(_) => {
                return Err(self.unsupported_node(&node));
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
        | NodeKind::Rational(..)
        | NodeKind::RImaginary(..)
        | NodeKind::Symbol(_)
        | NodeKind::String(_)
        | NodeKind::Bytes(_)
        | NodeKind::EncodedString(..)
        | NodeKind::InterporatedString(_)
        | NodeKind::RegExp(..)
        | NodeKind::Range { .. }
        | NodeKind::Hash { .. }
        | NodeKind::Command(_)
        | NodeKind::Array(..)
        | NodeKind::Lambda(_)
        | NodeKind::AliasMethod(..)
        | NodeKind::UndefMethod(..)
        | NodeKind::If { .. }
        | NodeKind::While { .. }
        | NodeKind::For { .. }
        | NodeKind::Case { .. }
        | NodeKind::Break(_)
        | NodeKind::Next(_)
        | NodeKind::Return(_)
        | NodeKind::Redo
        | NodeKind::Retry
        | NodeKind::Defined(_)
        | NodeKind::ClassDef { .. }
        | NodeKind::SingletonClassDef { .. }
        | NodeKind::MethodDef(..)
        | NodeKind::SingletonMethodDef(..)
        | NodeKind::Splat(_) => "expression",
        NodeKind::Super(..) => "super",
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
        NodeKind::GlobalVar(..) => "global-variable",
        // CRuby uses a space (not a hyphen) for class variables.
        NodeKind::ClassVar(..) => "class variable",
        NodeKind::Const { .. } => "constant",
        // `target ||= value` / `&&=` desugar to
        // `BinOp(LOr/LAnd, target, MulAssign([target], [value]))`;
        // `defined?` reports these as "assignment". A plain `a || b`
        // (RHS is not an inline assignment) stays "expression".
        NodeKind::BinOp(BinOp::LAnd | BinOp::LOr, _, r) => {
            if matches!(r.kind, NodeKind::MulAssign(..)) {
                "assignment"
            } else {
                "expression"
            }
        }
        // `__ENCODING__` is a pseudo-variable (parsed as a bare Ident),
        // not a method call.
        NodeKind::Ident(name) if name == "__ENCODING__" => "expression",
        NodeKind::BinOp(..)
        | NodeKind::FuncCall { .. }
        | NodeKind::MethodCall { .. }
        | NodeKind::Ident(_)
        | NodeKind::Index { .. }
        | NodeKind::UnOp(..) => "method",
        NodeKind::Begin { box body, .. } => defined_str(body),
        NodeKind::CompStmt(v) => match v.last() {
            Some(node) => defined_str(node),
            None => "nil",
        },
        NodeKind::DiscardLhs | NodeKind::MulAssignNested(..) => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn defined_logical_and_match_ops() {
        // `&&` / `||` / `and` / `or` are expressions (operands not
        // checked); `=~` / `!~` are method calls. Previously these
        // panicked (`unimplemented!`) and aborted the process.
        run_test(r#"a = 1; b = 2; defined?(a && b)"#);
        run_test(r#"defined?(1 || 2)"#);
        run_test(r#"a = 1; defined?(a and 2)"#);
        run_test(r#"defined?(1 or 2)"#);
        run_test(r#"defined?(undef_x && 1)"#);
        run_test(r#"defined?(undef_x || undef_y)"#);
        run_test(r#"defined?("x" =~ /x/)"#);
        run_test(r#"defined?(1 =~ 2).inspect"#);
        run_test(r#"defined?(1 !~ 2)"#);
        run_test(r#"defined?(undef_x =~ /a/).inspect"#);
    }

    #[test]
    fn defined_void_context_does_not_evaluate() {
        // `defined?` in a void (statement) context must not evaluate its
        // operand — the receiver's side effects never fire — while a value
        // context still probes it.
        run_test(
            r#"
            $log = []
            def se; $log << :ran; 1; end
            defined?(se / 2)          # void: must not call `se`
            a = $log.dup
            x = defined?(se / 2)      # value: does call `se`
            [a, x, $log]
            "#,
        );
        // Void `defined?` on an undefined name is likewise a no-op.
        run_test(
            r#"
            $c = 0
            def bump; $c += 1; self; end
            defined?(bump.no_such_method)
            $c
            "#,
        );
    }

    #[test]
    fn defined_frozen_and_assignment() {
        // `defined?` returns a frozen String.
        run_tests(&[
            r#"defined?(self)"#,
            r#"defined?(self).frozen?"#,
            r#"defined?(nil).frozen?"#,
            r#"defined?(true)"#,
            r#"defined?(false)"#,
            r#"defined?([Object, Array])"#,
            r#"defined?([Object, Array]).frozen?"#,
            r#"$g = 5; [defined?($g), defined?($g).frozen?]"#,
            r#"$n = nil; defined?($n)"#,
            r#"defined?(__ENCODING__)"#,
            // `||=` / `&&=` are "assignment" for every target kind.
            r#"x = 1; defined?(x ||= 2)"#,
            r#"x = 1; defined?(x &&= 2)"#,
            r#"@i = 1; defined?(@i ||= 2)"#,
            r#"$g2 = 1; defined?($g2 ||= 2)"#,
            r#"a = [0]; defined?(a[0] ||= 2)"#,
            // A plain `a || b` stays "expression".
            r#"a = 1; b = 2; defined?(a || b)"#,
        ]);
        // Class variables report "class variable" (a space, not a hyphen),
        // frozen.
        run_test(
            r#"
            class DefCV
              @@v = 1
              def t; [defined?(@@v), defined?(@@v).frozen?]; end
            end
            DefCV.new.t
            "#,
        );
    }

    #[test]
    fn defined_method_respond_to_missing_and_protected() {
        // `defined?(recv.meth)` consults `respond_to_missing?`, honours
        // protected-method visibility (checked against the *defining*
        // class), and a qualified constant does not fall through to a
        // top-level constant.
        run_test(
            r#"
            module DefM1; end
            class DefR
              def respond_to_missing?(name, inc); name == :foo; end
            end
            class DefPB
              def m; end
              protected :m
              def chk(o); defined?(o.m); end
            end
            class DefPS < DefPB; end
            [
              defined?(DefR.new.foo),      # respond_to_missing? => "method"
              defined?(DefR.new.bar),      # => nil
              DefPB.new.chk(DefPS.new),    # protected, subclass recv => "method"
              DefPB.new.chk(DefPB.new),    # protected, base recv => "method"
              defined?(DefM1::String),     # no top-level fallback => nil
              defined?(Integer::String),   # no top-level fallback => nil
            ]
            "#,
        );
    }

    #[test]
    fn defined_scoped_constant_resolution() {
        // Qualified constants: direct, nested-qualifier walk, top-level
        // qualified, and the negative paths (undefined leaf / undefined
        // qualifier — the latter must not fire `const_missing`).
        run_test(
            r#"
            module CovM
              CONST = 1
              module Inner; DEEP = 2; end
            end
            [
              defined?(CovM::CONST),        # "constant"
              defined?(CovM::Inner),        # "constant"
              defined?(CovM::Inner::DEEP),  # "constant" (prefix walk)
              defined?(CovM::Inner::Nope),  # nil
              defined?(CovM::Missing::X),   # nil (undefined qualifier)
              defined?(::CovM::CONST),      # "constant" (top-level qualified)
            ]
            "#,
        );
    }

    #[test]
    fn defined_method_negative_visibility_and_raising_respond_to_missing() {
        // Private methods are not defined via an explicit receiver, an
        // `undef`ined method is not defined, and an exception raised by
        // `respond_to_missing?` is swallowed to nil (never propagated).
        run_test(
            r#"
            class CovP; def pm; end; private :pm; end
            class CovU; def m; end; undef_method :m; end
            class CovR; def respond_to_missing?(n, i); raise "boom"; end; end
            [
              defined?(CovP.new.pm),   # private via explicit recv => nil
              defined?(CovU.new.m),    # undef'd => nil
              defined?(CovR.new.x),    # respond_to_missing? raises => nil
            ]
            "#,
        );
    }

    #[test]
    fn defined_last_paren_match_and_not_operator() {
        // `$+` is the last non-empty captured group.
        run_test(r#""mis-matched" =~ /s(-)m(.)/; [defined?($+), $+]"#);
        // A match with no capture groups leaves `$+` undefined.
        run_test(r#""abc" =~ /b/; [defined?($+), $+]"#);
        // `defined?(not X)` / `defined?(!X)` evaluate a method operand as
        // the operator's receiver — executing its side effects and
        // reporting "method".
        run_test(
            r#"
            $dlog = []
            def dse; $dlog << :se; 1; end
            [[defined?(not dse), defined?(!dse)], $dlog]
            "#,
        );
        // When the evaluated operand raises, the exception is swallowed to
        // nil, but its side effects still run.
        run_test(
            r#"
            $clog = []
            def craise; $clog << :r; raise "x"; end
            [defined?(not craise), $clog]
            "#,
        );
    }
}
