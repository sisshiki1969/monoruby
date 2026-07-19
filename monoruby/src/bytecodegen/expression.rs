use super::*;

/// Decode the encoding-selector modifier letters (`n`/`u`/`e`/`s`) of a
/// regexp literal into the corresponding `NOENCODING` / `KCODE_*` bit.
///
/// When several appear, CRuby's "last specifier wins" rule applies
/// (`/foo/ensuens` selects `s`), so the scan runs right-to-left and stops
/// at the first (i.e. source-order last) selector. Returns 0 when none is
/// present.
fn regexp_encoding_bits(option: &str) -> u32 {
    option
        .chars()
        .rev()
        .find_map(|c| match c {
            'n' => Some(RegexpInner::NOENCODING),
            'u' => Some(RegexpInner::KCODE_UTF8),
            'e' => Some(RegexpInner::KCODE_EUCJP),
            's' => Some(RegexpInner::KCODE_SJIS),
            _ => None,
        })
        .unwrap_or(0)
}

///
/// Maximum number of elements (Array) / entries (Hash) of a collection
/// literal that are evaluated into consecutive temporary registers at once.
/// Longer literals are built in chunks of this size, re-using the same
/// registers for every chunk (see `gen_array` / `gen_hash`), so that the
/// frame's register count — frames live on the native machine stack — stays
/// bounded no matter how large the literal is (issue #706).
///
const LITERAL_CHUNK_LEN: usize = 256;

///
/// A single target of a multiple assignment: either a resolved leaf
/// lvalue, or a deferred nested destructure group (`(b, c)` in
/// `(a, (b, c)) = ...`) whose targets are expanded once its parent array
/// element is available.
///
enum MulLhs {
    Leaf(LvalueKind),
    /// A nested destructure group (`(b, c)` in `(a, (b, c)) = ...`). Its
    /// members' lvalues are already evaluated (receivers / indices captured
    /// left-to-right, before the rhs); `rest_pos` records the splat slot for
    /// the group's own `ExpandArray`.
    Nested {
        targets: Vec<MulLhs>,
        rest_pos: Option<u16>,
    },
}

impl<'a> BytecodeGen<'a> {
    ///
    /// Evaluate *expr*, push the result, and return the register.
    ///
    /// `temp` is moved next.
    ///
    pub(super) fn push_expr(&mut self, expr: Node) -> Result<BcTemp> {
        let ret = self.sp();
        self.gen_expr(expr, UseMode2::Push)?;
        Ok(ret)
    }

    pub(super) fn push_check_expr(&mut self, expr: Node) -> Result<BcTemp> {
        let ret = self.sp();
        match expr.kind {
            NodeKind::Const {
                toplevel,
                name,
                parent,
                prefix,
            } => {
                let old = self.temp;
                let dst = self.push().into();
                //self.gen_store_expr(dst, expr)?;
                let loc = expr.loc;
                let base: Option<BcReg> = if let Some(box parent) = parent {
                    let base = self.gen_temp_expr(parent)?;
                    Some(base)
                } else {
                    None
                };
                self.emit_check_const(Some(dst), base, toplevel, name, prefix, loc);
                assert_eq!(old + 1, self.temp);
            }
            NodeKind::ClassVar(name) => {
                let old = self.temp;
                let dst = self.push().into();
                let loc = expr.loc;
                let name = IdentId::get_id_from_string(name);
                self.emit_check_cvar(Some(dst), name, loc);
                assert_eq!(old + 1, self.temp);
            }
            _ => {
                self.gen_expr(expr, UseMode2::Push)?;
            }
        }
        Ok(ret)
    }

    ///
    /// Evaluate *expr* and
    ///
    /// * If *use_mode* is `UseMode2::Store(r)`, the result is stored in r.
    /// * If *use_mode* is `UseMode2::NotUse`, the result will be discarded.
    /// * If *use_mode* is `UseMode2::Push`, push the result to the stack and return the register.
    /// * If *use_mode* is `UseMode2::Ret`, emit BcIr::Ret with the result.
    ///
    pub(super) fn gen_expr(&mut self, expr: Node, use_mode: UseMode2) -> Result<()> {
        let old = self.temp;
        //let loc = expr.loc;
        self.gen_expr_inner(expr, use_mode)?;
        match use_mode {
            UseMode2::Push => {
                assert_eq!(old + 1, self.temp);
            }
            _ => {
                assert_eq!(old, self.temp);
            }
        };
        Ok(())
    }

    ///
    /// Evaluate *expr* and store the result to *dst*.
    ///
    /// ### arguments
    /// - `dst`: destination register.
    /// - `expr`: expression.
    ///
    /// ### note
    /// `temp` is not moved.
    ///
    pub(super) fn gen_store_expr(&mut self, dst: BcReg, rhs: Node) -> Result<()> {
        let loc = rhs.loc;
        match rhs.kind {
            NodeKind::Nil => self.emit_nil(dst),
            NodeKind::Bool(b) => self.emit_literal(dst, Value::bool(b)),
            NodeKind::SelfValue => self.emit_mov(dst, BcReg::Self_),
            NodeKind::Integer(i) => self.emit_integer(dst, i),
            NodeKind::Symbol(sym) => {
                let val = Value::symbol_from_source_str(&sym, self.source_encoding());
                self.emit_symbol(dst, val.as_symbol())
            }
            NodeKind::Bignum(bigint) => self.emit_bigint(dst, bigint),
            NodeKind::Float(f) => self.emit_float(dst, f),
            NodeKind::Imaginary(r) => self.emit_imaginary(dst, r.into()),
            NodeKind::Rational(n, d) => self.emit_rational(dst, &n, &d),
            NodeKind::RImaginary(n, d) => self.emit_rimaginary(dst, &n, &d),
            NodeKind::String(s) => self.emit_string(dst, s, loc),
            NodeKind::Bytes(b) => self.emit_bytes(dst, b, loc),
            NodeKind::EncodedString(b, enc) => self.emit_encoded_string(dst, b, enc, loc),
            NodeKind::Array(nodes, false) => self.gen_array(dst, nodes, loc)?,
            NodeKind::Hash(nodes, splat) => self.gen_hash(dst, nodes, splat, loc)?,
            NodeKind::RegExp(nodes, op, false) => self.gen_regexp(dst, nodes, op, loc)?,
            NodeKind::Array(_, true) | NodeKind::Range { is_const: true, .. } => {
                let val = Value::from_const_ast(&rhs, self.source_encoding());
                self.emit_literal(dst, val);
            }
            NodeKind::RegExp(nodes, op, true) => {
                let val = self.const_regexp(nodes, op, loc)?;
                self.emit_literal(dst, val);
            }
            NodeKind::Range {
                box start,
                box end,
                exclude_end,
                is_const: false,
            } => self.gen_range(dst, start, end, exclude_end, loc)?,
            NodeKind::Lambda(info) => self.gen_lambda(dst, info, loc)?,
            NodeKind::Index {
                box base,
                mut index,
            } => {
                // `self[i]` uses the fast `Index` opcode like any other
                // single-index read; `gen_index` keeps a literal `self` base as
                // slot 0, so the VM passes `is_func_call` to `get_index` and a
                // private `#[]` stays reachable (matching CRuby) with no slow
                // call-site detour for the common public case.
                if index.len() == 1 && !index[0].is_splat() {
                    self.gen_index(Some(dst), base, index.remove(0), loc)?;
                } else {
                    let arglist = ArgList::from_args(index);
                    self.gen_method_call(
                        IdentId::_INDEX,
                        Some(base),
                        arglist,
                        false,
                        false,
                        UseMode2::Store(dst),
                        loc,
                    )?;
                };
            }
            NodeKind::UnOp(op, box rhs) => match op {
                UnOp::Neg => {
                    match rhs.kind {
                        NodeKind::Integer(i) => self.emit_integer(dst, -i),
                        NodeKind::Imaginary(r) => self.emit_imaginary(dst, -Real::from(r)),
                        NodeKind::Float(f) => self.emit_float(dst, -f),
                        NodeKind::Rational(n, d) => self.emit_rational(dst, &-n, &d),
                        _ => self.emit_unary_op(UnOpK::Neg, dst, rhs, loc)?,
                    };
                }
                UnOp::Pos => {
                    match rhs.kind {
                        NodeKind::Integer(i) => self.emit_integer(dst, i),
                        NodeKind::Imaginary(r) => self.emit_imaginary(dst, r.into()),
                        NodeKind::Float(f) => self.emit_float(dst, f),
                        NodeKind::Rational(n, d) => self.emit_rational(dst, &n, &d),
                        _ => self.emit_unary_op(UnOpK::Pos, dst, rhs, loc)?,
                    };
                }
                UnOp::Not => self.emit_unary_op(UnOpK::Not, dst, rhs, loc)?,
                UnOp::BitNot => self.emit_unary_op(UnOpK::BitNot, dst, rhs, loc)?,
            },
            NodeKind::BinOp(op, box lhs, box rhs) => {
                self.gen_binop(op, lhs, rhs, UseMode2::Store(dst), loc)?;
            }
            NodeKind::MulAssign(mut mlhs, mut mrhs) => {
                if mlhs.len() == 1 && mrhs.len() == 1 {
                    let (lhs, rhs) = (mlhs.remove(0), mrhs.remove(0));
                    if let Some(src) = self.is_assign_local(&lhs) {
                        self.gen_store_expr(src.into(), rhs)?;
                        self.emit_mov(dst, src.into());
                    } else if matches!(&lhs.kind,
                        NodeKind::MethodCall { arglist, safe_nav: true, .. }
                            if arglist.args.is_empty()
                                && arglist.block.is_none()
                                && arglist.kw_args.is_empty())
                    {
                        // `recv&.attr = rhs` in value context (see the
                        // `gen_expr` twin for the short-circuit rationale).
                        let NodeKind::MethodCall {
                            box receiver,
                            method,
                            ..
                        } = lhs.kind
                        else {
                            unreachable!()
                        };
                        let setter = IdentId::get_id_from_string(format!("{method}="));
                        self.gen_safe_nav_attr_assign(
                            receiver,
                            setter,
                            rhs,
                            UseMode2::Store(dst),
                            loc,
                        )?;
                    } else {
                        let temp = self.temp;
                        let (lhs, rest) = self.eval_lvalue(&lhs)?;
                        if rest {
                            // `*a = rhs` used as a value (`x = (*a = ...)`):
                            // the expression evaluates to `rhs` itself, while
                            // `a` receives the splatted form (an Array
                            // distributes, a scalar/`nil` is wrapped). Put
                            // `rhs` in `dst`, then splat a copy into `a`.
                            self.gen_store_expr(dst, rhs)?;
                            let tmp = self.push().into();
                            self.emit_mov(tmp, dst);
                            self.emit(BytecodeInst::ExpandArray(tmp, tmp, 1, Some(0)), loc);
                            self.emit_assign(tmp, lhs, Some(temp), loc);
                        } else {
                            self.gen_store_expr(dst, rhs)?;
                            self.emit_assign(dst, lhs, Some(temp), loc);
                        }
                    }
                } else {
                    self.gen_mul_assign(mlhs, mrhs, UseMode2::Push)?;
                    let temp = self.pop().into();
                    self.emit_mov(dst, temp);
                }
            }
            NodeKind::LocalVar(0, ident) => {
                if let Some(local2) = self.refer_local(&ident) {
                    self.emit_mov(dst, local2);
                } else {
                    self.emit(BytecodeInst::BlockArg(dst, 0), loc);
                }
            }
            NodeKind::LocalVar(outer, ident) => {
                let name = IdentId::get_id_from_string(ident);
                if let Some(src) = self.refer_dynamic_local(outer, name) {
                    let src = src.into();
                    self.emit(BytecodeInst::LoadDynVar { dst, src, outer }, loc);
                } else {
                    assert_eq!(Some(name), self.block_param);
                    self.emit(BytecodeInst::BlockArg(dst, outer), loc);
                }
            }
            NodeKind::Const {
                toplevel,
                name,
                parent,
                prefix,
            } => {
                let base: Option<BcReg> = if let Some(box parent) = parent {
                    let base = self.gen_temp_expr(parent)?;
                    Some(base)
                } else {
                    None
                };
                self.emit_load_const(dst.into(), base, toplevel, name, prefix, loc);
            }
            NodeKind::InstanceVar(name) => {
                let name = IdentId::get_id_from_string(name);
                self.emit_load_ivar(dst.into(), name, loc);
            }
            NodeKind::GlobalVar(name) => {
                let name = IdentId::get_id_from_string(name);
                self.emit_load_gvar(dst.into(), name, loc);
            }
            NodeKind::ClassVar(name) => {
                let name = IdentId::get_id_from_string(name);
                self.emit_load_cvar(dst.into(), name, loc);
            }
            NodeKind::MethodCall {
                box receiver,
                method,
                box arglist,
                safe_nav,
            } => {
                let method = IdentId::get_id_from_string(method);
                self.gen_method_call(
                    method,
                    Some(receiver),
                    arglist,
                    safe_nav,
                    false,
                    UseMode2::Store(dst),
                    loc,
                )?;
            }
            NodeKind::FuncCall {
                method,
                box arglist,
                safe_nav,
            } => {
                let method = IdentId::get_id_from_string(method);
                self.gen_method_call(method, None, arglist, safe_nav, false, UseMode2::Store(dst), loc)?;
            }
            NodeKind::Return(box val) => {
                if self.return_escapes() {
                    return self.gen_method_return(val, UseMode2::Store(dst));
                } else {
                    return self.gen_return(val, UseMode2::Store(dst));
                }
            }
            NodeKind::CompStmt(nodes) => {
                self.gen_comp_stmts(nodes, Some(dst), UseMode2::NotUse)?;
            }
            NodeKind::ClassDef {
                base,
                name,
                superclass,
                box info,
                is_module,
            } => {
                let name = IdentId::get_id_from_string(name);
                self.gen_class_def(
                    name,
                    base,
                    superclass,
                    info,
                    UseMode2::Store(dst),
                    is_module,
                    loc,
                )?;
            }
            _ => {
                let ret = self.push_expr(rhs)?.into();
                self.emit_mov(dst, ret);
                self.pop();
            }
        };
        Ok(())
    }

    fn gen_comp_stmts(
        &mut self,
        mut nodes: Vec<Node>,
        dst: Option<BcReg>,
        use_mode: UseMode2,
    ) -> Result<()> {
        let last = match nodes.pop() {
            Some(node) => node,
            None => Node::new_nil(Loc(0, 0)),
        };
        for node in nodes.into_iter() {
            self.gen_expr(node, UseMode2::NotUse)?;
        }
        match dst {
            Some(ret) => {
                self.gen_store_expr(ret, last)?;
                self.handle_mode(use_mode, ret)?;
            }
            None => {
                self.gen_expr(last, use_mode)?;
            }
        }
        Ok(())
    }

    ///
    /// Evaluate *expr* and
    ///
    /// * If *use_mode* is `UseMode::Use`, push the result to the stack and return the register.
    /// * If *use_mode* is `UseMode::Ret`, emit BcIr::Ret with the result.
    /// * If *use_mode* is `UseMode::NotUse`, the result is discarded.
    ///
    fn gen_expr_inner(&mut self, expr: Node, use_mode: UseMode2) -> Result<()> {
        if !use_mode.use_val() {
            match &expr.kind {
                NodeKind::Nil
                | NodeKind::Bool(_)
                | NodeKind::SelfValue
                | NodeKind::Integer(_)
                | NodeKind::Symbol(_)
                | NodeKind::Bignum(_)
                | NodeKind::Float(_)
                | NodeKind::Imaginary(_)
                | NodeKind::Rational(..)
                | NodeKind::RImaginary(..)
                | NodeKind::String(_)
                | NodeKind::EncodedString(..) => return Ok(()),
                // `__ENCODING__` is a pure pseudo-variable; in void
                // context it has no effect.
                NodeKind::Ident(name) if name == "__ENCODING__" => return Ok(()),
                _ => {}
            }
        }
        if let UseMode2::Store(r) = use_mode {
            return self.gen_store_expr(r, expr);
        }
        let loc = expr.loc;
        match expr.kind {
            NodeKind::Nil
            | NodeKind::Bool(_)
            | NodeKind::Integer(_)
            | NodeKind::Symbol(_)
            | NodeKind::Bignum(_)
            | NodeKind::Float(_)
            | NodeKind::Imaginary(_)
            | NodeKind::Rational(..)
            | NodeKind::RImaginary(..)
            | NodeKind::String(_)
            | NodeKind::Bytes(_)
            | NodeKind::EncodedString(..)
            | NodeKind::Array(..)
            | NodeKind::Hash(..)
            | NodeKind::Range { .. }
            | NodeKind::RegExp(_, _, _)
            | NodeKind::Lambda(_)
            | NodeKind::UnOp(..)
            | NodeKind::Const { .. }
            | NodeKind::InstanceVar(_)
            | NodeKind::ClassVar(_)
            | NodeKind::GlobalVar(_) => {
                let ret = self.push().into();
                self.gen_store_expr(ret, expr)?;
            }
            // `__ENCODING__` — parsed as a bare `Ident`; evaluate to the
            // source-encoding `Encoding` object. Load it as the constant
            // `::Encoding::<NAME>` (rather than a baked literal) so it keeps
            // object identity with the registered `Encoding` singleton.
            NodeKind::Ident(method) if method == "__ENCODING__" => {
                let ret = self.push().into();
                let const_name =
                    crate::builtins::encoding::encoding_constant_name(self.source_encoding());
                self.emit_load_const(
                    Some(ret),
                    None,
                    true,
                    const_name.to_string(),
                    vec!["Encoding".to_string()],
                    loc,
                );
            }
            NodeKind::BinOp(op, box lhs, box rhs) => {
                self.gen_binop(op, lhs, rhs, use_mode, loc)?;
                return Ok(());
            }
            NodeKind::Index {
                box base,
                mut index,
            } => {
                // `self[i]` uses the fast `Index` opcode; a literal `self` base
                // stays slot 0 so the VM passes `is_func_call` to `get_index`
                // and a private `#[]` stays reachable. See the read arm above.
                if index.len() == 1 && !index[0].is_splat() {
                    self.gen_index(None, base, index.remove(0), loc)?;
                } else {
                    let arglist = ArgList::from_args(index);
                    self.gen_method_call(
                        IdentId::_INDEX,
                        Some(base),
                        arglist,
                        false,
                        false,
                        use_mode,
                        loc,
                    )?;
                    return Ok(());
                };
            }
            NodeKind::AssignOp(op, box lhs, box rhs) => {
                if let Some(local) = self.is_assign_local(&lhs) {
                    self.gen_binop(op, lhs, rhs, UseMode2::Store(local.into()), loc)?;
                    self.handle_mode(use_mode, local.into())?;
                    return Ok(());
                }
                // Safe-navigation op-assign `recv&.attr op= rhs`: nil recv
                // short-circuits to nil. Reuse the ordinary op-assign codegen
                // (which handles `+=`, `||=`, `&&=`, ...) on the safe_nav-
                // stripped lhs, wrapped in a nil guard on the receiver.
                if matches!(&lhs.kind,
                    NodeKind::MethodCall { arglist, safe_nav: true, .. }
                        if arglist.args.is_empty()
                            && arglist.block.is_none()
                            && arglist.kw_args.is_empty())
                {
                    let receiver = match &lhs.kind {
                        NodeKind::MethodCall { receiver, .. } => (**receiver).clone(),
                        _ => unreachable!(),
                    };
                    let mut lhs = lhs;
                    if let NodeKind::MethodCall { safe_nav, .. } = &mut lhs.kind {
                        *safe_nav = false;
                    }
                    return self.gen_safe_nav_op_assign(receiver, op, lhs, rhs, use_mode, loc);
                }
                // Attribute (`recv.attr op= rhs`) and index (`base[idx] op=
                // rhs`) targets have a receiver / index expression that may
                // carry side effects, so it must be evaluated exactly once and
                // reused for both the getter and the setter. Every other target
                // (const / ivar / cvar / gvar / dynamic var) is side-effect-free
                // to re-read, so the simpler re-evaluating path stays correct.
                if matches!(&lhs.kind,
                    NodeKind::MethodCall { .. } | NodeKind::Index { .. })
                {
                    return self.gen_op_assign_with_receiver(op, lhs, rhs, use_mode, loc);
                }
                // A scoped constant `Scope::CONST op= rhs` has a scope
                // (module part) expression that may carry side effects
                // (`(x += 1; M)::C ||= v`), so it must be evaluated exactly
                // once and reused for both the read and the store. A bare
                // constant (`CONST op= rhs`, no parent) is side-effect-free
                // to re-read, so it stays on the simpler path below.
                if matches!(&lhs.kind, NodeKind::Const { parent: Some(_), .. }) {
                    return self.gen_scoped_const_op_assign(op, lhs, rhs, use_mode, loc);
                }
                match op {
                    BinOp::LOr | BinOp::LAnd => {
                        // `target ||= rhs` / `target &&= rhs`: short-circuit so
                        // the store only fires when needed. Re-reading a
                        // side-effect-free target is fine, so lower to the
                        // ordinary short-circuit binop over an inline assign.
                        let assign = Node::new(
                            NodeKind::MulAssign(vec![lhs.clone()], vec![rhs]),
                            loc,
                        );
                        return self.gen_binop(op, lhs, assign, use_mode, loc);
                    }
                    _ => {}
                }
                let lhs_loc = lhs.loc;
                let temp = self.temp;
                // First, evaluate lvalue.
                let (lhs_kind, rest) = self.eval_lvalue(&lhs)?;
                assert!(!rest);
                // Evaluate rvalue.
                let src = self.sp().into();
                self.gen_binop(op, lhs, rhs, UseMode2::Push, loc)?;
                // Assign rvalue to lvalue.
                return self.assign_with_mode(use_mode, src, lhs_kind, temp, lhs_loc);
            }
            NodeKind::MulAssign(mut mlhs, mut mrhs) => {
                if mlhs.len() == 1 && mrhs.len() == 1 {
                    let (lhs, rhs) = (mlhs.remove(0), mrhs.remove(0));
                    if let Some(local) = self.is_assign_local(&lhs) {
                        self.gen_store_expr(local.into(), rhs)?;
                        self.handle_mode(use_mode, local.into())?;
                        return Ok(());
                    }
                    // Safe-navigation attribute assignment `recv&.attr = rhs`:
                    // when `recv` is nil the whole thing is nil and the rhs is
                    // NOT evaluated. This short-circuit doesn't fit the generic
                    // eval_lvalue path (which always evaluates the rhs), so
                    // handle it directly.
                    if matches!(&lhs.kind,
                        NodeKind::MethodCall { arglist, safe_nav: true, .. }
                            if arglist.args.is_empty()
                                && arglist.block.is_none()
                                && arglist.kw_args.is_empty())
                    {
                        let NodeKind::MethodCall {
                            box receiver,
                            method,
                            ..
                        } = lhs.kind
                        else {
                            unreachable!()
                        };
                        let setter = IdentId::get_id_from_string(format!("{method}="));
                        return self.gen_safe_nav_attr_assign(receiver, setter, rhs, use_mode, loc);
                    }
                    let temp = self.temp;
                    let (lhs, rest) = self.eval_lvalue(&lhs)?;
                    if rest {
                        // `*a = rhs`: `a` receives the splatted `rhs` — an
                        // Array distributes its elements (`*a = [1, 2]` ⇒
                        // `[1, 2]`), a non-Array is wrapped (`*a = 1` ⇒ `[1]`,
                        // `*a = nil` ⇒ `[nil]`) — via `ExpandArray` with a
                        // lone rest slot. As a *value*, though, the whole
                        // expression is `rhs` itself (`x = (*a = 1)` ⇒ `1`),
                        // so when the result is used we keep the original and
                        // splat a copy into `a`.
                        if matches!(use_mode, UseMode2::NotUse) {
                            let src = self.gen_expr_reg(rhs)?;
                            self.emit(BytecodeInst::ExpandArray(src, src, 1, Some(0)), loc);
                            self.emit_assign(src, lhs, Some(temp), loc);
                            return Ok(());
                        }
                        let rhs_reg = self.push_expr(rhs)?.into();
                        let tmp = self.push().into();
                        self.emit_mov(tmp, rhs_reg);
                        self.emit(BytecodeInst::ExpandArray(tmp, tmp, 1, Some(0)), loc);
                        self.emit_assign(tmp, lhs, None, loc);
                        self.temp = temp;
                        self.handle_mode(use_mode, rhs_reg)?;
                        return Ok(());
                    } else {
                        let src = self.gen_expr_reg(rhs)?;
                        return self.assign_with_mode(use_mode, src, lhs, temp, loc);
                    }
                } else {
                    return self.gen_mul_assign(mlhs, mrhs, use_mode);
                }
            }
            NodeKind::SelfValue => {
                self.handle_mode(use_mode, BcReg::Self_)?;
                return Ok(());
            }
            NodeKind::LocalVar(0, ident) => {
                if let Some(local) = self.refer_local(&ident) {
                    self.handle_mode(use_mode, local)?;
                    return Ok(());
                } else {
                    let ret = self.push().into();
                    self.emit(BytecodeInst::BlockArg(ret, 0), loc);
                }
            }
            NodeKind::LocalVar(outer, ident) => {
                let ret = self.push().into();
                let lvar = IdentId::get_id_from_string(ident);
                if let Some(src) = self.refer_dynamic_local(outer, lvar) {
                    let src = src.into();
                    self.emit(
                        BytecodeInst::LoadDynVar {
                            dst: ret,
                            src,
                            outer,
                        },
                        loc,
                    );
                } else if Some(lvar) == self.outer_block_param_name(outer) {
                    self.emit(BytecodeInst::BlockArg(ret, outer), loc);
                } else {
                    return Err(MonorubyErr::runtimeerr(format!(
                        "can't access local variable '{}' in outer block",
                        lvar.get_name()
                    )));
                }
            }

            NodeKind::MethodCall {
                box receiver,
                method,
                box arglist,
                safe_nav,
            } => {
                let method = IdentId::get_id_from_string(method);
                return self.gen_method_call(
                    method,
                    Some(receiver),
                    arglist,
                    safe_nav,
                    false,
                    use_mode,
                    loc,
                );
            }
            NodeKind::FuncCall {
                method,
                box arglist,
                safe_nav,
            } => {
                let method = IdentId::get_id_from_string(method);
                return self.gen_method_call(method, None, arglist, safe_nav, false, use_mode, loc);
            }
            NodeKind::Super(arglist) => {
                return self.gen_super(arglist.map(|arglist| *arglist), use_mode, loc);
            }
            NodeKind::Command(box expr) => {
                let method = IdentId::get_id("`");
                // A non-interpolated command literal (`` `cmd` `` / `%x{…}`)
                // passes its command string to the `` ` `` method FROZEN,
                // matching CRuby (which freezes it regardless of any
                // `frozen_string_literal` pragma). Build the call directly so
                // the argument is a frozen literal; an interpolated form
                // (`` `cmd #{x}` ``) builds a fresh, unfrozen string and takes
                // the ordinary method-call path below.
                if let NodeKind::String(s) = expr.kind {
                    let (dst, push_flag) = match use_mode {
                        UseMode2::NotUse => (None, false),
                        UseMode2::Push | UseMode2::Ret => (Some(self.sp().into()), true),
                        UseMode2::Store(dst) => (Some(dst), false),
                    };
                    let old_temp = self.temp;
                    let arg = self.push().into();
                    self.emit_frozen_string(arg, &s);
                    self.temp = old_temp;
                    if push_flag {
                        self.push();
                    }
                    let callsite = CallSite::simple(method, 1, arg, BcReg::Self_, dst);
                    self.emit_call(callsite, loc);
                    if use_mode.is_ret() {
                        self.emit_ret(None)?;
                    }
                    return Ok(());
                }
                let arglist = ArgList::from_args(vec![expr]);
                return self.gen_method_call(method, None, arglist, false, false, use_mode, loc);
            }
            NodeKind::Yield(arglist) => {
                return self.gen_yield(*arglist, use_mode, loc);
            }
            NodeKind::Ident(method) => {
                let arglist = ArgList::default();
                let method = IdentId::get_id_from_string(method);
                return self.gen_method_call(method, None, arglist, false, true, use_mode, loc);
            }
            NodeKind::If {
                box cond,
                box then_,
                box else_,
            } => {
                let else_pos = self.new_label();
                self.gen_opt_condbr(false, cond, else_pos)?;
                let old = self.temp;
                self.gen_expr(then_, use_mode)?;
                if else_.is_empty() && use_mode == UseMode2::NotUse {
                    self.apply_label(else_pos);
                } else {
                    let succ_pos = self.new_label();
                    match use_mode {
                        UseMode2::NotUse | UseMode2::Push => {
                            self.emit_br(succ_pos);
                        }
                        UseMode2::Ret => {}
                        _ => unreachable!(),
                    }
                    self.temp = old;
                    self.apply_label(else_pos);
                    self.gen_expr(else_, use_mode)?;
                    self.apply_label(succ_pos);
                }
                return Ok(());
            }
            NodeKind::While {
                box cond,
                box body,
                cond_op,
                postfix,
            } => {
                if postfix && matches!(body.kind, NodeKind::Begin { .. }) {
                    self.gen_while_begin_postfix(cond_op, cond, body, use_mode.use_val())?;
                } else {
                    self.gen_while(cond_op, cond, body, use_mode.use_val())?;
                }
                if use_mode.is_ret() {
                    self.emit_ret(None)?;
                }
                return Ok(());
            }
            NodeKind::For {
                param,
                box iter,
                box body,
            } => {
                self.gen_for(param, iter, body, use_mode.use_val())?;
                if use_mode.is_ret() {
                    self.emit_ret(None)?;
                }
                return Ok(());
            }
            NodeKind::Case {
                cond,
                when_,
                box else_,
            } => {
                return self.gen_case(cond, when_, else_, use_mode);
            }
            NodeKind::Break(box val) => {
                let LoopInfo {
                    break_dest,
                    ret,
                    ensure_depth,
                    ..
                } = match self.loops.last() {
                    Some(data) => data.clone(),
                    None => {
                        if self.is_escaping_block() {
                            self.gen_block_break(val, use_mode)?;
                            return Ok(());
                        } else if self.outer.is_some() {
                            // `break` in a lambda exits the lambda with the
                            // value, like `return` (not a non-local break).
                            self.gen_return(val, use_mode)?;
                            return Ok(());
                        } else {
                            return Err(self.escape_from_eval("break", loc));
                        }
                    }
                };
                if let Some(reg) = ret {
                    self.gen_store_expr(reg, val)?;
                } else {
                    self.gen_expr(val, UseMode2::NotUse)?;
                }
                // Run `ensure` blocks nested inside the loop before exiting.
                self.gen_loop_pending_ensures(ensure_depth)?;
                self.emit(BytecodeInst::Br(break_dest), loc);
                if use_mode == UseMode2::Push {
                    self.push();
                }
                return Ok(());
            }
            NodeKind::Next(box val) => {
                let LoopInfo {
                    next_dest,
                    ensure_depth,
                    ..
                } = match self.loops.last() {
                    Some(data) => data.clone(),
                    None => {
                        if self.is_block() {
                            self.gen_return(val, use_mode)?;
                            return Ok(());
                        } else {
                            return Err(self.escape_from_eval("next", loc));
                        }
                    }
                };
                self.gen_expr(val, UseMode2::NotUse)?;
                // Run `ensure` blocks nested inside the loop before
                // continuing to the next iteration.
                self.gen_loop_pending_ensures(ensure_depth)?;
                self.emit(BytecodeInst::Br(next_dest), loc);
                if use_mode == UseMode2::Push {
                    self.push();
                }
                return Ok(());
            }
            NodeKind::Redo => {
                if use_mode == UseMode2::Push {
                    self.push();
                }
                let LoopInfo {
                    redo_dest,
                    ensure_depth,
                    ..
                } = match self.loops.last() {
                    Some(data) => data.clone(),
                    None => {
                        if self.is_block() {
                            // Restarting the block body exits every open
                            // begin/ensure region — run their ensure
                            // bodies first (same unwinding protocol as a
                            // block-local return).
                            self.gen_all_pending_ensures()?;
                            self.emit(BytecodeInst::Redo(self.redo_label), loc);
                            return Ok(());
                        } else {
                            return Err(self.escape_from_eval("redo", loc));
                        }
                    }
                };
                // Run `ensure` blocks nested inside the loop before
                // re-executing the body.
                self.gen_loop_pending_ensures(ensure_depth)?;
                // Use the `Redo` op (a `goto` via the error path) rather than a
                // plain `Br`: its target can sit in the middle of the loop body
                // (skipping the condition) without the JIT seeing a second
                // back-edge into the loop, which its loop analysis can't merge.
                self.emit(BytecodeInst::Redo(redo_dest), loc);
                return Ok(());
            }
            NodeKind::Retry => {
                if use_mode == UseMode2::Push {
                    self.push();
                }
                match self.retry_labels.last() {
                    Some(&label) => {
                        self.emit(BytecodeInst::Retry(label), loc);
                    }
                    None => {
                        return Err(self.syntax_error("Invalid retry.", loc));
                    }
                };
                return Ok(());
            }
            NodeKind::Return(box val) => {
                if self.return_escapes() {
                    self.gen_method_return(val, use_mode)?;
                } else {
                    self.gen_return(val, use_mode)?;
                }

                return Ok(());
            }
            NodeKind::CompStmt(nodes) => return self.gen_comp_stmts(nodes, None, use_mode),
            NodeKind::Begin {
                box body,
                rescue,
                else_,
                ensure,
            } => {
                return self.gen_begin(body, rescue, else_, ensure, use_mode);
            }
            NodeKind::MethodDef(name, box block) => {
                let name = IdentId::get_id_from_string(name);
                self.gen_method_def(name, block, use_mode, loc)?;
                return Ok(());
            }
            NodeKind::SingletonMethodDef(box obj, name, box block) => {
                self.gen_expr(obj, UseMode2::Push)?;
                let name = IdentId::get_id_from_string(name);
                self.gen_singleton_method_def(name, block, use_mode, loc)?;
                return Ok(());
            }
            NodeKind::ClassDef {
                base,
                name,
                superclass,
                box info,
                is_module,
            } => {
                let name = IdentId::get_id_from_string(name);
                self.gen_class_def(name, base, superclass, info, use_mode, is_module, loc)?;
                return Ok(());
            }
            NodeKind::SingletonClassDef {
                box singleton,
                box info,
            } => {
                self.gen_singleton_class_def(singleton, info, use_mode, loc)?;
                return Ok(());
            }
            NodeKind::InterporatedString(nodes) => {
                // The interpolation result takes the SOURCE encoding as its
                // starting point (CRuby: a dstr begins with its first
                // literal segment). When the first part is an embedded
                // expression (`"#{a}"`), seed with an empty literal so the
                // runtime concat negotiates from the source encoding.
                let seed = !matches!(
                    nodes.first().map(|n| &n.kind),
                    Some(NodeKind::String(_))
                        | Some(NodeKind::Bytes(_))
                        | Some(NodeKind::EncodedString(..))
                );
                let len = nodes.len() + seed as usize;
                let arg = self.sp();
                if seed {
                    let r = self.push().into();
                    // Plain (never chilled/frozen) seed: the concat
                    // result is a fresh mutable string in CRuby, even
                    // under a frozen_string_literal pragma.
                    let enc = self.source_encoding();
                    self.emit_literal(r, Value::string_from_source_str("", enc));
                }
                for expr in nodes {
                    self.push_expr(expr)?;
                }
                self.temp -= len as u16;
                let ret = if use_mode.use_val() {
                    Some(self.push().into())
                } else {
                    None
                };
                self.emit(BytecodeInst::ConcatStr(ret, arg, len), Loc::default());
                if use_mode.is_ret() {
                    self.emit_ret(None)?;
                }
                return Ok(());
            }
            NodeKind::AliasMethod(box new, box old) => {
                if let (NodeKind::Symbol(new_name), NodeKind::Symbol(old_name)) =
                    (&new.kind, &old.kind)
                {
                    if new_name.starts_with('$') && old_name.starts_with('$') {
                        // Global variable alias: resolve names at compile
                        // time and emit a dedicated AliasGvar instruction.
                        let new_id = IdentId::get_id_from_string(new_name.clone());
                        let old_id = IdentId::get_id_from_string(old_name.clone());
                        self.emit(
                            BytecodeInst::AliasGvar {
                                new: new_id,
                                old: old_id,
                            },
                            loc,
                        );
                        self.push_nil();
                        // fall through to use_mode handling at end of match
                    } else {
                        let new = self.push_expr(new)?;
                        let old = self.push_expr(old)?;
                        self.temp -= 2;
                        self.emit(
                            BytecodeInst::AliasMethod {
                                new: new.into(),
                                old: old.into(),
                            },
                            loc,
                        );
                        self.push_nil();
                    }
                } else {
                    let new = self.push_expr(new)?;
                    let old = self.push_expr(old)?;
                    self.temp -= 2;
                    self.emit(
                        BytecodeInst::AliasMethod {
                            new: new.into(),
                            old: old.into(),
                        },
                        loc,
                    );
                    self.push_nil();
                }
                //match use_mode {
                //    UseMode2::Ret => {
                //        self.push_nil();
                //        self.emit_ret(None)?;
                //    }
                //    UseMode2::NotUse => {}
                //    UseMode2::Push => {
                //        self.push_nil();
                //    }
                //    _ => unreachable!(),
                //}
                //return Ok(());
            }
            NodeKind::UndefMethod(box undef) => {
                if let NodeKind::Symbol(name) = &undef.kind {
                    let temp = self.temp;
                    let undef = IdentId::get_id(name);
                    self.temp = temp;
                    self.emit(BytecodeInst::UndefMethod { undef }, loc);
                } else {
                    // Dynamic name, e.g. `undef :"#{expr}"`. The static
                    // `UndefMethod` instruction bakes in an `IdentId`, so
                    // lower this to an implicit-self `undef_method(<expr>)`
                    // call (Module#undef_method coerces String/Symbol),
                    // matching CRuby. `undef` evaluates to nil, so discard
                    // the call result and push nil like the static path.
                    let arglist = ArgList::from_args(vec![undef]);
                    self.gen_method_call(
                        IdentId::get_id("undef_method"),
                        None,
                        arglist,
                        false,
                        false,
                        UseMode2::NotUse,
                        loc,
                    )?;
                }
                self.push_nil();
            }
            NodeKind::Defined(box node) => {
                // `defined?` in a void (statement) context evaluates nothing:
                // CRuby elides the whole expression, so the operand's
                // receiver side effects never fire. In a value context the
                // operand is still probed by `gen_defined`.
                if use_mode == UseMode2::NotUse {
                    return Ok(());
                }
                self.gen_defined(node)?;
            }
            NodeKind::Splat(..) => {
                return Err(self.unsupported_lhs(&expr));
            }
            // Only ever appears as an LHS element of a `MulAssign`, where
            // `gen_mul_assign` consumes it directly.
            NodeKind::DiscardLhs | NodeKind::MulAssignNested(..) => unreachable!(),
        }
        match use_mode {
            UseMode2::Ret => {
                self.emit_ret(None)?;
            }
            UseMode2::NotUse => {
                self.pop();
            }
            UseMode2::Push => {}
            _ => unreachable!(),
        }
        Ok(())
    }

    fn assign_with_mode(
        &mut self,
        use_mode: UseMode2,
        src: BcReg,
        lhs_kind: LvalueKind,
        temp: u16,
        lhs_loc: Loc,
    ) -> Result<()> {
        match use_mode {
            UseMode2::Ret => {
                self.emit_assign(src, lhs_kind, None, lhs_loc);
                self.temp = temp;
                self.emit_ret(Some(src))?;
            }
            UseMode2::Push => {
                self.emit_assign(src, lhs_kind, None, lhs_loc);
                self.temp = temp;
                let dst = self.push();
                self.emit_mov(dst.into(), src);
            }
            UseMode2::NotUse => {
                self.emit_assign(src, lhs_kind, Some(temp), lhs_loc);
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    ///
    /// Generate an op-assign whose target has a side-effecting receiver or
    /// index (`recv.attr op= rhs`, `base[idx] op= rhs`).
    ///
    /// `eval_lvalue` evaluates the receiver / index exactly once (into temp
    /// registers); the current value is then read through the matching getter,
    /// combined with `rhs`, and written back through the setter — reusing that
    /// single evaluation. For `||=` / `&&=` the setter fires only when the
    /// current value is falsy / truthy respectively.
    ///
    fn gen_op_assign_with_receiver(
        &mut self,
        op: BinOp,
        lhs: Node,
        rhs: Node,
        use_mode: UseMode2,
        loc: Loc,
    ) -> Result<()> {
        let temp = self.temp;
        // The getter name for an attribute target (`recv.attr`); `eval_lvalue`
        // turns the `Send` lvalue's method into the setter (`attr=`), so keep
        // the original method name for the read.
        let getter = if let NodeKind::MethodCall { method, .. } = &lhs.kind {
            Some(IdentId::get_id(method))
        } else {
            None
        };
        let (lhs_kind, rest) = self.eval_lvalue(&lhs)?;
        assert!(!rest);
        // Read the current value.
        let cur = self.push().into();
        self.emit_op_assign_getter(&lhs_kind, getter, cur, loc);
        match op {
            BinOp::LOr | BinOp::LAnd => {
                let exit = self.new_label();
                // `||=`: skip the store when the current value is truthy.
                // `&&=`: skip the store when the current value is falsy.
                let jmp_if_true = matches!(op, BinOp::LOr);
                self.emit_condbr(cur, exit, jmp_if_true, false);
                self.gen_store_expr(cur, rhs)?;
                self.emit_assign(cur, lhs_kind, None, loc);
                self.apply_label(exit);
            }
            _ => {
                let binopk = Self::binop_to_binopk(op);
                let old = self.temp;
                let rhs_reg = self.gen_expr_reg(rhs)?;
                self.temp = old;
                self.emit(BytecodeInst::BinOp(binopk, Some(cur), (cur, rhs_reg)), loc);
                self.emit_assign(cur, lhs_kind, None, loc);
            }
        }
        self.temp = temp;
        self.handle_mode(use_mode, cur)?;
        Ok(())
    }

    ///
    /// Op-assign to a scoped constant `Scope::CONST op= rhs`.
    ///
    /// The scope (module part) is evaluated exactly once — `eval_lvalue`
    /// reserves it as `base` — and reused for both the read and the store,
    /// so a side-effecting scope (`(x += 1; M)::C ||= v`) fires only once.
    ///
    fn gen_scoped_const_op_assign(
        &mut self,
        op: BinOp,
        lhs: Node,
        rhs: Node,
        use_mode: UseMode2,
        loc: Loc,
    ) -> Result<()> {
        let temp = self.temp;
        // Evaluates the scope once and keeps it reserved as `base`.
        let (lhs_kind, rest) = self.eval_lvalue(&lhs)?;
        assert!(!rest);
        let (toplevel, base, prefix, name) = match &lhs_kind {
            LvalueKind::Const {
                toplevel,
                parent,
                prefix,
                name,
            } => (*toplevel, *parent, prefix.clone(), *name),
            _ => unreachable!(),
        };
        let cur = self.push().into();
        match op {
            BinOp::LOr | BinOp::LAnd => {
                let exit = self.new_label();
                // `Scope::C ||= rhs` reads the constant without raising when
                // it is undefined (nil ⇒ store fires); `&&=` uses the plain
                // load (undefined ⇒ NameError), matching CRuby.
                if matches!(op, BinOp::LOr) {
                    self.emit(
                        BytecodeInst::CheckConst {
                            dst: cur,
                            base,
                            toplevel,
                            prefix,
                            name,
                        },
                        loc,
                    );
                } else {
                    self.emit(
                        BytecodeInst::LoadConst {
                            dst: cur,
                            base,
                            toplevel,
                            prefix,
                            name,
                        },
                        loc,
                    );
                }
                let jmp_if_true = matches!(op, BinOp::LOr);
                self.emit_condbr(cur, exit, jmp_if_true, false);
                self.gen_store_expr(cur, rhs)?;
                self.emit_assign(cur, lhs_kind, None, loc);
                self.apply_label(exit);
            }
            _ => {
                self.emit(
                    BytecodeInst::LoadConst {
                        dst: cur,
                        base,
                        toplevel,
                        prefix,
                        name,
                    },
                    loc,
                );
                let binopk = Self::binop_to_binopk(op);
                let old = self.temp;
                let rhs_reg = self.gen_expr_reg(rhs)?;
                self.temp = old;
                self.emit(BytecodeInst::BinOp(binopk, Some(cur), (cur, rhs_reg)), loc);
                self.emit_assign(cur, lhs_kind, None, loc);
            }
        }
        self.temp = temp;
        self.handle_mode(use_mode, cur)?;
        Ok(())
    }

    ///
    /// Emit the getter for an already-evaluated attribute / index lvalue,
    /// storing the read value into `dst`.
    ///
    fn emit_op_assign_getter(
        &mut self,
        lhs_kind: &LvalueKind,
        getter: Option<IdentId>,
        dst: BcReg,
        loc: Loc,
    ) {
        let callsite = match lhs_kind {
            LvalueKind::Send { recv, .. } => CallSite::unary(getter.unwrap(), *recv, Some(dst)),
            LvalueKind::Index { base, index } => {
                // The receiver and index are already evaluated exactly once
                // into temps by `eval_lvalue`, so read through the
                // specialized `Index` bytecode — the same opcode an ordinary
                // `base[idx]` read uses. Its inline cache feeds the JIT's
                // array/hash/integer index fast path; lowering this getter to
                // a generic `#[]` method call instead compiles to a full
                // (non-inlined) call in JIT code and costs ~2x on
                // index-op-assign-heavy loops (matmul, sudoku).
                self.emit(BytecodeInst::Index(dst, *base, (*index).into()), loc);
                return;
            }
            LvalueKind::Index2 { base, index1, num } => {
                CallSite::simple(IdentId::_INDEX, *num, (*index1).into(), *base, Some(dst))
            }
            LvalueKind::IndexSplat {
                base,
                index1,
                num,
                splat_pos,
            } => CallSite::new(
                IdentId::_INDEX,
                *num,
                None,
                splat_pos.clone(),
                None,
                None,
                (*index1).into(),
                *base,
                Some(dst),
                false,
            ),
            _ => unreachable!(),
        };
        self.emit_call(callsite, loc);
    }

    ///
    /// Map an arithmetic / bitwise op-assign operator to its `BinOpK`.
    /// `||=` / `&&=` are handled separately (short-circuit) and never reach
    /// here.
    ///
    fn binop_to_binopk(op: BinOp) -> BinOpK {
        match op {
            BinOp::Add => BinOpK::Add,
            BinOp::Sub => BinOpK::Sub,
            BinOp::Mul => BinOpK::Mul,
            BinOp::Div => BinOpK::Div,
            BinOp::Rem => BinOpK::Rem,
            BinOp::Exp => BinOpK::Exp,
            BinOp::BitOr => BinOpK::BitOr,
            BinOp::BitAnd => BinOpK::BitAnd,
            BinOp::BitXor => BinOpK::BitXor,
            BinOp::Shl => BinOpK::Shl,
            BinOp::Shr => BinOpK::Shr,
            _ => unreachable!("non-arithmetic op-assign operator: {:?}", op),
        }
    }

    fn handle_mode(&mut self, use_mode: UseMode2, src: BcReg) -> Result<()> {
        match use_mode {
            UseMode2::Ret => {
                self.emit_ret(Some(src))?;
            }
            UseMode2::Push => {
                let dst = self.push();
                self.emit_mov(dst.into(), src);
            }
            UseMode2::NotUse => {}
            UseMode2::Store(r) => {
                self.emit_mov(r, src);
            }
        }
        Ok(())
    }
}

impl<'a> BytecodeGen<'a> {
    ///
    /// Generate multiple assignment.
    ///
    /// This func always use a new temporary register for rhs even if the number of rhs is 1.
    ///
    ///
    /// Generate `recv&.attr = rhs` (safe-navigation attribute assignment).
    ///
    /// When `recv` is nil, the result is nil and `rhs` is left unevaluated;
    /// otherwise `recv.attr=(rhs)` runs and the assignment evaluates to `rhs`.
    ///
    fn gen_safe_nav_attr_assign(
        &mut self,
        receiver: Node,
        setter: IdentId,
        rhs: Node,
        use_mode: UseMode2,
        loc: Loc,
    ) -> Result<()> {
        let old_temp = self.temp;
        // recv and the result slot must survive across the nil branch.
        let recv: BcReg = self.push_expr(receiver)?.into();
        let res: BcReg = self.push().into();
        let merge_sp = self.temp;

        let nil_exit = self.new_label();
        let exit = self.new_label();

        // If recv is nil, skip evaluating rhs and the setter call.
        self.emit_nilbr(recv, nil_exit, merge_sp);

        // Non-nil path: evaluate rhs, call the setter, result is rhs.
        let rhs_reg: BcReg = self.push_expr(rhs)?.into();
        let callsite = CallSite::simple(setter, 1, rhs_reg, recv, None);
        self.emit_method_assign(callsite, loc);
        self.emit_mov(res, rhs_reg);
        self.temp = merge_sp;
        self.emit_br(exit);

        // Nil path: the whole expression is nil.
        self.apply_label(nil_exit);
        self.emit_nil(res);

        self.apply_label(exit);
        self.temp = old_temp;
        self.handle_mode(use_mode, res)?;
        Ok(())
    }

    ///
    /// Generate `recv&.attr op= rhs` (safe-navigation op-assignment).
    ///
    /// `lhs` is the `recv.attr` getter with `safe_nav` already cleared. When
    /// `recv` is nil the result is nil and nothing else runs; otherwise the
    /// ordinary op-assign codegen handles the read/modify/write.
    ///
    fn gen_safe_nav_op_assign(
        &mut self,
        receiver: Node,
        op: BinOp,
        lhs: Node,
        rhs: Node,
        use_mode: UseMode2,
        loc: Loc,
    ) -> Result<()> {
        let old_temp = self.temp;
        let recv: BcReg = self.push_expr(receiver)?.into();
        let res: BcReg = self.push().into();
        let merge_sp = self.temp;

        let nil_exit = self.new_label();
        let exit = self.new_label();

        self.emit_nilbr(recv, nil_exit, merge_sp);

        // Non-nil path: run the ordinary `lhs op= rhs` and keep its value.
        let assign = Node::new(NodeKind::AssignOp(op, Box::new(lhs), Box::new(rhs)), loc);
        self.gen_store_expr(res, assign)?;
        self.temp = merge_sp;
        self.emit_br(exit);

        self.apply_label(nil_exit);
        self.emit_nil(res);

        self.apply_label(exit);
        self.temp = old_temp;
        self.handle_mode(use_mode, res)?;
        Ok(())
    }

    fn gen_mul_assign(
        &mut self,
        mlhs: Vec<Node>,
        mut mrhs: Vec<Node>,
        use_mode: UseMode2,
    ) -> Result<()> {
        let mlhs_len = mlhs.len();
        let mrhs_len = mrhs.len();
        let loc = mlhs[0].loc().merge(mrhs.last().unwrap().loc());

        let temp = self.temp;

        // At first, we evaluate lvalues and save their info(LhsKind).
        // The receiver / index subexpressions of every target — including
        // those inside a nested destructure group (`(b, c)` in
        // `(a, (b, c)) = ...`) — are evaluated here, left-to-right and
        // BEFORE the rhs, matching CRuby's evaluation order. Only the
        // `ExpandArray` + store of a nested group is deferred (it needs the
        // parent array element), carrying the pre-evaluated lvalues.
        let mut lhs_kind: Vec<MulLhs> = vec![];
        let mut rest_pos = None;
        for (i, lhs) in mlhs.into_iter().enumerate() {
            match lhs.kind {
                NodeKind::MulAssignNested(targets) => {
                    lhs_kind.push(self.eval_nested_lvalues(targets)?);
                }
                _ => {
                    let (kind, rest) = self.eval_lvalue(&lhs)?;
                    if rest {
                        rest_pos = Some(i as u16)
                    }
                    lhs_kind.push(MulLhs::Leaf(kind));
                }
            }
        }

        // Next, we evaluate rvalues and save them in temporary registers which start from temp_reg.
        //
        // When the mlhs has a rest target (`a, *b = ...`) or the arities
        // differ, CRuby collects the whole rhs into one array and
        // destructures it (distributing to the rest, truncating extras, or
        // nil-filling shortfalls). Only a rest-free, equal-arity mlhs uses a
        // true parallel assignment (which keeps `a, b = b, a` swap
        // semantics). The whole assignment expression evaluates to the rhs
        // array in either case.
        let use_expand = rest_pos.is_some() || mlhs_len != mrhs_len;
        let (rhs_reg, ret_val) = if use_expand {
            // A single source value to expand: the lone rhs itself when
            // there is one, otherwise the collected `[rhs0, rhs1, ..]` array.
            let src: BcReg = if mrhs_len == 1 {
                self.push_expr(std::mem::take(&mut mrhs[0]))?.into()
            } else {
                let arr: BcReg = self.push().into();
                self.gen_array(arr, mrhs, loc)?;
                arr
            };
            let rhs_reg = self.sp();
            // One expanded slot per mlhs target (including discards), so the
            // per-target indices below line up with the `ExpandArray` output.
            let expand_len = lhs_kind.len();
            for _ in 0..expand_len {
                self.push();
            }
            self.emit(
                BytecodeInst::ExpandArray(src, rhs_reg.into(), expand_len as u16, rest_pos),
                Loc::default(),
            );
            // lhs0, lhs1, .. = src
            //
            //   temp               src   rhs_reg              sp
            //   v                  v     v                    v
            // -+-----------------+-----+--------------------+--
            //  | lhs working reg | src | lhs * expand_len   |
            // -+-----------------+-----+--------------------+--
            //
            (rhs_reg, Some(src))
        } else {
            let rhs_reg = self.sp();
            for rhs in mrhs {
                self.push_expr(rhs)?;
            }
            // lhs0, lhs1, .. = rhs0, rhs1, ..
            //
            //   temp               rhs_reg         sp
            //   v                  v               v
            // -+-----------------+----------------+--
            //  | lhs working reg | rhs * mrhs_len |
            // -+-----------------+----------------+--
            //
            (rhs_reg, None)
        };

        // Finally, assign rvalues to lvalue.
        for (i, lhs) in lhs_kind.into_iter().enumerate() {
            let src = (rhs_reg + i).into();
            match lhs {
                MulLhs::Leaf(kind) => self.emit_assign(src, kind, None, loc),
                MulLhs::Nested { targets, rest_pos } => {
                    self.assign_prepared(src, targets, rest_pos, loc)?
                }
            }
        }

        self.temp = temp;

        // Generate return value if needed.
        match use_mode {
            UseMode2::Push => {
                let dst = self.push().into();
                if let Some(src) = ret_val {
                    self.emit_mov(dst, src);
                } else {
                    self.emit_array(dst, rhs_reg.into(), mrhs_len, vec![], loc);
                }
            }
            UseMode2::Ret => {
                if let Some(src) = ret_val {
                    self.emit_ret(Some(src))?;
                } else {
                    let dst = self.push().into();
                    self.emit_array(dst, rhs_reg.into(), mrhs_len, vec![], loc);
                    self.emit_ret(None)?;
                }
            }
            UseMode2::Store(dst) => {
                if let Some(src) = ret_val {
                    self.emit_mov(dst, src);
                } else {
                    self.emit_array(dst, rhs_reg.into(), mrhs_len, vec![], loc);
                }
            }
            UseMode2::NotUse => {}
        }

        Ok(())
    }

    ///
    /// Evaluate the lvalues of a nested destructure group's `targets`,
    /// recursing into further groups. Receivers / indices are evaluated
    /// here (left-to-right, before the rhs); the actual `ExpandArray` +
    /// store is deferred to [`Self::assign_prepared`]. `rest_pos` records
    /// the splat slot (`(a, *b)`) for the group's own `ExpandArray`.
    ///
    fn eval_nested_lvalues(&mut self, targets: Vec<Node>) -> Result<MulLhs> {
        let rest_pos = targets.iter().position(|t| t.is_splat()).map(|p| p as u16);
        let mut kinds = vec![];
        for t in targets.into_iter() {
            match t.kind {
                NodeKind::MulAssignNested(sub) => {
                    kinds.push(self.eval_nested_lvalues(sub)?);
                }
                _ => {
                    // Plain leaf or `Splat(target)` rest slot; `eval_lvalue`
                    // unwraps the splat and returns the inner lvalue kind.
                    let (kind, _rest) = self.eval_lvalue(&t)?;
                    kinds.push(MulLhs::Leaf(kind));
                }
            }
        }
        Ok(MulLhs::Nested {
            targets: kinds,
            rest_pos,
        })
    }

    ///
    /// Destructure the array in register `src` into the pre-evaluated
    /// `targets`, recursing into nested groups with a chained
    /// `ExpandArray`. Backs nested multiple-assignment targets
    /// (`(a, (b, c)) = ...`). The targets' receivers / indices were already
    /// captured by [`Self::eval_nested_lvalues`]; here we only expand and
    /// store. New registers are pushed above the caller's stack and left in
    /// place — the top-level caller restores `temp` once the whole
    /// assignment is emitted.
    ///
    fn assign_prepared(
        &mut self,
        src: BcReg,
        targets: Vec<MulLhs>,
        rest_pos: Option<u16>,
        loc: Loc,
    ) -> Result<()> {
        let len = targets.len();
        let base = self.sp();
        for _ in 0..len {
            self.push();
        }
        self.emit(
            BytecodeInst::ExpandArray(src, base.into(), len as u16, rest_pos),
            loc,
        );
        for (i, t) in targets.into_iter().enumerate() {
            let elem = (base + i).into();
            match t {
                MulLhs::Leaf(kind) => self.emit_assign(elem, kind, None, loc),
                MulLhs::Nested { targets, rest_pos } => {
                    self.assign_prepared(elem, targets, rest_pos, loc)?;
                }
            }
        }
        Ok(())
    }

    fn gen_index(&mut self, ret: Option<BcReg>, base: Node, index: Node, loc: Loc) -> Result<()> {
        let old = self.temp;
        // Keep a literal `self` base as `BcReg::Self_` (slot 0, no temp) so the
        // emitted `Index` opcode records the receiver as self: the VM reads
        // base-slot == 0 and passes `is_func_call = true` to `get_index`, which
        // lets `self[i]` reach a private `#[]` (matching CRuby) while a plain
        // `obj[i]` — or `x = self; x[i]` — still enforces visibility.
        let base = if base.kind == NodeKind::SelfValue {
            BcReg::Self_
        } else {
            self.gen_expr_reg(base)?
        };
        let index = self.gen_expr_reg(index)?;
        self.temp = old;
        let ret = match ret {
            None => self.push().into(),
            Some(local) => local,
        };
        self.emit(BytecodeInst::Index(ret, base, index), loc);
        Ok(())
    }
}

//
// Literals
//
impl<'a> BytecodeGen<'a> {
    fn gen_symbol(&mut self, sym: IdentId, use_mode: UseMode2) -> Result<()> {
        match use_mode {
            UseMode2::NotUse => {}
            UseMode2::Push => {
                self.push_symbol(sym);
            }
            UseMode2::Store(r) => {
                self.emit_symbol(r, sym);
            }
            UseMode2::Ret => {
                self.push_symbol(sym);
                self.emit_ret(None)?;
            }
        }
        Ok(())
    }

    pub(super) fn gen_array(&mut self, ret: BcReg, nodes: Vec<Node>, loc: Loc) -> Result<()> {
        if nodes.len() <= LITERAL_CHUNK_LEN {
            let (src, len, splat) = self.ordinary_args(nodes)?;
            self.popn(len);
            self.emit_array(ret, src, len, splat, loc);
            return Ok(());
        }
        // Build a large Array literal in bounded chunks so the number of
        // temporary registers does not grow with the literal's length: emit
        // the first chunk with an ordinary Array instruction, then build each
        // following chunk into a scratch register (re-using the same element
        // registers) and concatenate it. Construct into a temporary so `ret`
        // (which may be a local variable) never holds a half-built array.
        let old_reg = self.temp;
        let tmp: BcReg = self.push().into();
        let base = self.temp;
        let mut first = true;
        let mut iter = nodes.into_iter().peekable();
        while iter.peek().is_some() {
            let chunk: Vec<Node> = iter.by_ref().take(LITERAL_CHUNK_LEN).collect();
            if first {
                let (src, len, splat) = self.ordinary_args(chunk)?;
                self.popn(len);
                self.emit_array(tmp, src, len, splat, loc);
                first = false;
            } else {
                let chunk_dst: BcReg = self.push().into();
                let (src, len, splat) = self.ordinary_args(chunk)?;
                self.popn(len);
                self.emit_array(chunk_dst, src, len, splat, loc);
                self.pop();
                self.emit(
                    BytecodeInst::ArrayConcat {
                        dst: tmp,
                        src: chunk_dst,
                    },
                    loc,
                );
            }
            debug_assert_eq!(base, self.temp);
        }
        self.temp = old_reg;
        self.emit_mov(ret, tmp);
        Ok(())
    }

    fn gen_hash(
        &mut self,
        ret: BcReg,
        nodes: Vec<(Node, Node)>,
        splat: Vec<(usize, Node)>,
        loc: Loc,
    ) -> Result<()> {
        self.warn_duplicated_hash_keys(&nodes, &splat);
        if splat.is_empty() {
            if nodes.len() <= LITERAL_CHUNK_LEN {
                let len = nodes.len();
                let old_reg = self.temp;
                let args = self.sp();
                for (k, v) in nodes {
                    self.push_expr(k)?;
                    self.push_expr(v)?;
                }
                self.temp = old_reg;
                self.emit_hash(ret, args.into(), len, loc);
            } else {
                // Chunked construction for large Hash literals (see
                // `gen_array`): keeps register usage bounded regardless of
                // the literal's size (issue #706).
                let old_reg = self.temp;
                let hash: BcReg = self.push().into();
                let base = self.temp;
                let count = nodes.len();
                let mut iter = nodes.into_iter();
                self.emit_hash_pairs(hash, &mut iter, count, true, base, loc)?;
                self.temp = old_reg;
                self.emit_mov(ret, hash);
            }
            return Ok(());
        }
        // At least one `**` splat: replay the elements strictly
        // left-to-right so later entries overwrite earlier ones
        // (`{a: 1, **h, c: 4}` ⇒ `{a: 1, b: 2, c: 4}` when `h == {b: 2,
        // c: 3}`). `splat[j].0` records how many ordinary pairs precede
        // splat `j`, so the source order is
        //   pairs[0..p0], splat0, pairs[p0..p1], splat1, …, pairs[p(n-1)..]
        let total_pairs = nodes.len();
        let positions: Vec<usize> = splat.iter().map(|(p, _)| *p).collect();
        let merge_id = IdentId::get_id("merge!");
        let old_reg = self.temp;
        let hash: BcReg = self.push().into();
        let base = self.temp;
        let mut iter = nodes.into_iter();
        // Leading run of pairs before the first splat (may be empty, in
        // which case an empty hash is created to merge into).
        self.emit_hash_pairs(hash, &mut iter, positions[0], true, base, loc)?;
        for (j, (pos, snode)) in splat.into_iter().enumerate() {
            // Merge the `**` operand. A nil operand contributes nothing
            // (`{a: 1, **nil}` == `{a: 1}`), so guard `merge!` with a nil
            // check; a non-nil, non-Hash operand still raises via
            // `merge!` / `#to_hash`.
            let old_reg2 = self.temp;
            let splat_arg = self.sp();
            self.push_expr(snode)?;
            let skip = self.new_label();
            self.emit_nilbr(splat_arg.into(), skip, old_reg2);
            self.temp = old_reg2;
            let callsite = CallSite::simple(merge_id, 1, splat_arg.into(), hash, Some(hash));
            self.emit_call(callsite, loc);
            self.apply_label(skip);
            // Pairs between this splat and the next one (or the tail).
            let end = positions.get(j + 1).copied().unwrap_or(total_pairs);
            self.emit_hash_pairs(hash, &mut iter, end - pos, false, base, loc)?;
        }
        self.temp = old_reg;
        self.emit_mov(ret, hash);
        Ok(())
    }

    ///
    /// Build (`create == true`, first run) or merge-insert (`create ==
    /// false`) a run of `count` key/value pairs drawn from `iter` into the
    /// hash register `hash`, chunked at `LITERAL_CHUNK_LEN` to keep register
    /// pressure bounded. `base` is the temp watermark restored after each
    /// chunk (the hash register itself must already be reserved below it).
    ///
    fn emit_hash_pairs(
        &mut self,
        hash: BcReg,
        iter: &mut impl Iterator<Item = (Node, Node)>,
        count: usize,
        create: bool,
        base: u16,
        loc: Loc,
    ) -> Result<()> {
        if count == 0 {
            if create {
                // Empty leading run (`{**h}`): start from an empty hash.
                let args = self.sp();
                self.emit_hash(hash, args.into(), 0, loc);
            }
            return Ok(());
        }
        let mut remaining = count;
        let mut create = create;
        while remaining > 0 {
            let take = remaining.min(LITERAL_CHUNK_LEN);
            let args = self.sp();
            for _ in 0..take {
                let (k, v) = iter.next().unwrap();
                self.push_expr(k)?;
                self.push_expr(v)?;
            }
            self.temp = base;
            if create {
                self.emit_hash(hash, args.into(), take, loc);
                create = false;
            } else {
                self.emit(
                    BytecodeInst::HashInsert {
                        hash,
                        args: args.into(),
                        len: take as u16,
                    },
                    loc,
                );
            }
            remaining -= take;
        }
        Ok(())
    }

    ///
    /// Buffer CRuby's "key ... is duplicated and overwritten" warning for a
    /// hash literal that repeats a *literal* key (`{a: 1, a: 2}`). Only
    /// constant keys (symbol / string / integer / bignum / float / true /
    /// false / nil) are checked — runtime-computed keys (`{k => 1, k => 2}`)
    /// are not — matching CRuby's compile-time check. Silenced at warning
    /// level 0 (`-W0`). The message is buffered on `Store` and flushed
    /// through `$stderr` by `Executor::flush_compile_warnings` once
    /// compilation finishes.
    ///
    fn warn_duplicated_hash_keys(&mut self, nodes: &[(Node, Node)], splat: &[(usize, Node)]) {
        use std::sync::atomic::Ordering;
        if crate::globals::WARNING.load(Ordering::Relaxed) == 0 {
            return;
        }
        // Flatten the literal's statically-known keys into source order,
        // descending into `**{...}` splats whose operand is itself a
        // literal hash, then warn on every re-occurrence.
        let mut keys: Vec<(String, String, Loc)> = vec![];
        Self::collect_hash_literal_keys(nodes, splat, &mut keys);
        let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
        for (eq, inspect, loc) in keys {
            if !seen.insert(eq) {
                let line = self.sourceinfo.get_line(&loc);
                let msg = format!(
                    "{}:{}: warning: key {} is duplicated and overwritten on line {}",
                    self.sourceinfo.path.to_string_lossy(),
                    line,
                    inspect,
                    line,
                );
                self.store.compile_warnings.push((msg, false));
            }
        }
    }

    ///
    /// Collect a hash literal's *statically known* keys into `out` in
    /// source order. Descends into `**{...}` splats whose operand is
    /// itself a literal hash (recursively), so a duplicate spanning a
    /// literal splat (`{a: 1, **{a: 2}}`) is detected — matching CRuby's
    /// compile-time check. Runtime splats (`**h`) and runtime-computed
    /// keys (`{k => 1}`) are opaque and contribute nothing.
    ///
    /// `splat[j].0` is the number of ordinary pairs that precede splat
    /// `j`, so the source order is
    ///   pairs[0..p0], splat0, pairs[p0..p1], splat1, …, trailing pairs
    /// — the same interleaving `gen_hash` replays.
    ///
    fn collect_hash_literal_keys(
        nodes: &[(Node, Node)],
        splat: &[(usize, Node)],
        out: &mut Vec<(String, String, Loc)>,
    ) {
        // (equality key, inspect string)
        fn literal_key(kind: &NodeKind) -> Option<(String, String)> {
            match kind {
                NodeKind::Symbol(s) => Some((format!("sym:{s}"), format!(":{s}"))),
                NodeKind::String(s) => Some((format!("str:{s}"), format!("{s:?}"))),
                NodeKind::Integer(i) => Some((format!("int:{i}"), format!("{i}"))),
                NodeKind::Bignum(n) => Some((format!("int:{n}"), format!("{n}"))),
                NodeKind::Float(f) => Some((format!("flt:{f}"), crate::value::ruby_float_to_s(*f))),
                NodeKind::Bool(b) => Some((format!("bool:{b}"), format!("{b}"))),
                NodeKind::Nil => Some(("nil".to_string(), "nil".to_string())),
                _ => None,
            }
        }
        let mut pi = 0usize;
        for (pos, snode) in splat {
            while pi < *pos && pi < nodes.len() {
                let k = &nodes[pi].0;
                if let Some((eq, inspect)) = literal_key(&k.kind) {
                    out.push((eq, inspect, k.loc));
                }
                pi += 1;
            }
            if let NodeKind::Hash(inner, inner_splat) = &snode.kind {
                Self::collect_hash_literal_keys(inner, inner_splat, out);
            }
        }
        while pi < nodes.len() {
            let k = &nodes[pi].0;
            if let Some((eq, inspect)) = literal_key(&k.kind) {
                out.push((eq, inspect, k.loc));
            }
            pi += 1;
        }
    }

    fn gen_regexp(&mut self, ret: BcReg, nodes: Vec<Node>, option: String, loc: Loc) -> Result<()> {
        let mut len = nodes.len();
        let arg = self.sp();
        // The interpolated-regexp source is assembled at run time by
        // `runtime::concatenate_regexp`. Decode the literal-syntax modifier
        // letters into the same option word `const_regexp` builds, and pass
        // it as a leading Fixnum operand rather than as an inline `(?imx)`
        // source prefix. Two reasons:
        //   - `n`/`u`/`e`/`s` are encoding selectors, *not* valid Onigmo
        //     group options — `(?n)` makes Onigmo raise "undefined group
        //     option". They must set the regexp's `KCODE_*` / `NOENCODING`
        //     bits instead.
        //   - Even the valid `i`/`m`/`x` must not sit in the source: CRuby's
        //     `Regexp#source` for `/a#{x}/im` is `"a…"`, not `"(?im-x:a…)"`.
        //     Passing them as real Onigmo option bits keeps `#source` clean.
        // `concatenate_regexp` (the sole consumer of `ConcatRegexp`) peels
        // this operand off and hands the bits to the regexp builder.
        let mut opt: i64 = 0;
        if option.contains('i') {
            opt |= onigmo_regex::ONIG_OPTION_IGNORECASE as i64;
        }
        if option.contains('x') {
            opt |= onigmo_regex::ONIG_OPTION_EXTEND as i64;
        }
        if option.contains('m') {
            opt |= onigmo_regex::ONIG_OPTION_MULTILINE as i64;
        }
        // Encoding selector honours "last wins" (`regexp_encoding_bits`).
        opt |= regexp_encoding_bits(&option) as i64;
        // Always the first operand (even when 0) so the run-time layout is
        // uniform: operand 0 is the option word, the rest are source
        // fragments.
        let r = self.push().into();
        self.emit_literal(r, Value::integer(opt));
        len += 1;
        for expr in nodes {
            self.push_expr(expr)?;
        }
        self.temp -= len as u16;
        self.emit(BytecodeInst::ConcatRegexp(Some(ret), arg, len), loc);
        Ok(())
    }

    fn gen_range(
        &mut self,
        ret: BcReg,
        start: Option<Node>,
        end: Option<Node>,
        exclude_end: bool,
        loc: Loc,
    ) -> Result<()> {
        let old = self.temp;
        let start = if let Some(start) = start {
            self.gen_expr_reg(start)?
        } else {
            self.push_nil()
        };
        let end = if let Some(end) = end {
            self.gen_expr_reg(end)?
        } else {
            self.push_nil()
        };
        self.temp = old;
        self.emit(
            BytecodeInst::Range {
                ret,
                start,
                end,
                exclude_end,
            },
            loc,
        );
        Ok(())
    }

    fn gen_lambda(&mut self, dst: BcReg, info: Box<BlockInfo>, loc: Loc) -> Result<()> {
        let func = self.handle_lambda(*info)?;
        self.emit(BytecodeInst::Lambda { dst, func_id: func }, loc);
        Ok(())
    }

    fn const_regexp(&self, nodes: Vec<Node>, option: String, loc: Loc) -> Result<Value> {
        let mut string = String::new();
        let enc_bits = regexp_encoding_bits(&option);
        let encoding = if enc_bits & RegexpInner::NOENCODING != 0 {
            onigmo_regex::OnigmoEncoding::ASCII
        } else {
            onigmo_regex::OnigmoEncoding::UTF8
        };
        // Decode the literal-syntax modifier letters into the
        // standard Onigmo bits *plus* monoruby's internal
        // `KCODE_*` / `NOENCODING` bits used by
        // `with_option_kcode` to derive the declared (CRuby-
        // visible) encoding for `Regexp#encoding` /
        // `Regexp#fixed_encoding?`. The encoding selector honours
        // "last wins" (`regexp_encoding_bits`); `i`/`m`/`x` are order-
        // independent flags.
        let mut opt = onigmo_regex::ONIG_OPTION_NONE;
        if option.contains('i') {
            opt |= onigmo_regex::ONIG_OPTION_IGNORECASE;
        }
        if option.contains('x') {
            opt |= onigmo_regex::ONIG_OPTION_EXTEND;
        }
        if option.contains('m') {
            opt |= onigmo_regex::ONIG_OPTION_MULTILINE;
        }
        opt |= enc_bits;
        let kcode = if enc_bits & RegexpInner::KCODE_MASK != 0 {
            Some(enc_bits & RegexpInner::KCODE_MASK)
        } else {
            None
        };
        for node in nodes {
            match &node.kind {
                NodeKind::String(s) => string += s,
                _ => unreachable!(),
            }
        }
        let re = match RegexpInner::with_option_kcode(string, opt, encoding, kcode, None) {
            Ok(re) => re,
            Err(err) => return Err(self.syntax_error(err.message(), loc)),
        };
        // Regexp literals are frozen in CRuby; the spec battery
        // checks that `//.frozen?` is true and that
        // `//.send(:initialize, "")` raises `FrozenError`.
        let mut val = Value::regexp(re);
        val.set_frozen();
        Ok(val)
    }
}

//
// Definitions
//
impl<'a> BytecodeGen<'a> {
    fn gen_method_def(
        &mut self,
        name: IdentId,
        block: BlockInfo,
        use_mode: UseMode2,
        loc: Loc,
    ) -> Result<()> {
        let compile_info = Store::handle_args(block, vec![])?;
        let func_id = self.add_method(Some(name), compile_info, loc)?;
        self.emit(BytecodeInst::MethodDef { name, func_id }, loc);
        self.gen_symbol(name, use_mode)?;
        Ok(())
    }

    fn gen_singleton_method_def(
        &mut self,
        name: IdentId,
        block: BlockInfo,
        use_mode: UseMode2,
        loc: Loc,
    ) -> Result<()> {
        let compile_info = Store::handle_args(block, vec![])?;
        let func_id = self.add_method(Some(name), compile_info, loc)?;
        let obj = self.pop().into();
        self.emit(BytecodeInst::SingletonMethodDef { obj, name, func_id }, loc);
        self.gen_symbol(name, use_mode)?;
        Ok(())
    }

    ///
    /// Generate class definition.
    ///
    /// ### arguments
    /// - `name` - class name
    /// - `base` - base class
    /// - `superclass` - superclass
    /// - `info` - block info
    /// - `use_mode` - use mode
    /// - `is_module` - whether this is module definition or not
    /// - `loc` - location
    ///
    fn gen_class_def(
        &mut self,
        name: IdentId,
        base: Option<Box<Node>>,
        superclass: Option<Box<Node>>,
        info: BlockInfo,
        use_mode: UseMode2,
        is_module: bool,
        loc: Loc,
    ) -> Result<()> {
        let old = self.temp;
        let base = if let Some(box base) = base {
            Some(self.push_expr(base)?.into())
        } else {
            None
        };
        let compile_info = Store::handle_args(info, vec![])?;
        let func_id = self.add_classdef(Some(name), compile_info, loc, false)?;
        let superclass = match superclass {
            Some(box superclass) => Some(self.push_expr(superclass)?.into()),
            None => None,
        };
        self.temp = old;
        let ret = match use_mode {
            UseMode2::NotUse => None,
            UseMode2::Push | UseMode2::Ret => Some(self.push().into()),
            UseMode2::Store(r) => Some(r),
        };
        //let base = None;
        self.emit(
            if is_module {
                BytecodeInst::ModuleDef {
                    ret,
                    base,
                    name,
                    func_id,
                }
            } else {
                BytecodeInst::ClassDef {
                    ret,
                    base,
                    superclass,
                    name,
                    func_id,
                }
            },
            loc,
        );
        if use_mode == UseMode2::Ret {
            self.emit_ret(None)?;
        }
        Ok(())
    }

    fn gen_singleton_class_def(
        &mut self,
        base: Node,
        info: BlockInfo,
        use_mode: UseMode2,
        loc: Loc,
    ) -> Result<()> {
        let compile_info = Store::handle_args(info, vec![])?;
        let func_id = self.add_classdef(None, compile_info, loc, true)?;
        let old = self.temp;
        let base = self.gen_expr_reg(base)?;
        self.temp = old;
        let ret = match use_mode {
            UseMode2::NotUse => None,
            UseMode2::Push | UseMode2::Ret => Some(self.push().into()),
            UseMode2::Store(r) => Some(r),
        };
        self.emit(BytecodeInst::SingletonClassDef { ret, base, func_id }, loc);
        if use_mode == UseMode2::Ret {
            self.emit_ret(None)?;
        }
        Ok(())
    }
}

//
// Flow control
//
impl<'a> BytecodeGen<'a> {
    fn gen_return(&mut self, val: Node, use_mode: UseMode2) -> Result<()> {
        self.gen_expr(val, UseMode2::Ret)?;
        if use_mode == UseMode2::Push {
            self.push();
        }
        Ok(())
    }

    fn gen_method_return(&mut self, val: Node, use_mode: UseMode2) -> Result<()> {
        if let Some(local) = self.is_refer_local(&val) {
            self.emit(BytecodeInst::MethodRet(local), Loc::default());
        } else {
            self.gen_expr(val, UseMode2::Push)?;
            let ret = self.pop().into();
            self.emit(BytecodeInst::MethodRet(ret), Loc::default());
        }
        if use_mode == UseMode2::Push {
            self.push();
        }
        Ok(())
    }

    fn gen_block_break(&mut self, val: Node, use_mode: UseMode2) -> Result<()> {
        if let Some(local) = self.is_refer_local(&val) {
            self.emit(BytecodeInst::BlockBreak(local), Loc::default());
        } else {
            self.gen_expr(val, UseMode2::Push)?;
            let ret = self.pop().into();
            self.emit(BytecodeInst::BlockBreak(ret), Loc::default());
        }
        if use_mode == UseMode2::Push {
            self.push();
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn next_and_break_run_loop_ensures() {
        // `next` / `break` inside `begin/ensure` nested in a `while`/`until`
        // loop must run the enclosing `ensure` blocks (innermost first).
        run_test(
            r#"
            log = []; x = true
            while x
              begin
                log << :outer_begin; x = false
                begin
                  log << :inner_begin; next
                ensure
                  log << :inner_ensure
                end
              ensure
                log << :outer_ensure
              end
            end
            log
            "#,
        );
        run_test(
            r#"
            log = []; x = 1
            until false
              begin
                log << :b
                break if x > 1
                x += 1; next
              ensure
                log << :e
              end
            end
            log
            "#,
        );
    }

    #[test]
    fn splat_target_assignment_distributes_array() {
        // `*a = rhs` splats an Array `rhs` into `a` and wraps a non-Array
        // (previously an Array `rhs` was double-wrapped into `[[...]]`).
        run_test(
            r#"
            def r; *a = yield; a; end
            [r{[1,2]}, r{1}, r{nil}, r{[]}, r{[*[3,4]]}]
            "#,
        );
        run_tests(&[
            r#"*a = [1, 2]; a"#,
            r#"*a = 1; a"#,
            r#"*a = nil; a"#,
            r#"a, *b = [1, 2, 3]; [a, b]"#,
        ]);
    }

    #[test]
    fn undef_dynamic_name() {
        // `undef :"#{expr}"` (interpolated symbol) is lowered to an
        // implicit-self `undef_method(<expr>)` call. Previously the
        // non-`Symbol` branch hit `unimplemented!()` in the
        // `NodeKind::UndefMethod` codegen and aborted.
        run_test(
            r##"
            c = Class.new do
              def m; 1; end
              undef :"#{'m'}"
            end
            c.new.respond_to?(:m)
        "##,
        );
        run_test(
            r##"
            c = Class.new do
              def foo; 'x'; end
              undef :"#{'fo' + 'o'}"
            end
            c.new.respond_to?(:foo)
        "##,
        );
        // Non-literal interpolation (call .to_s on a Symbol).
        run_test(
            r##"
            c = Class.new do
              def bar; end
              undef :"#{:bar.to_s}"
            end
            c.new.respond_to?(:bar)
        "##,
        );
    }

    #[test]
    fn source_encoding_pseudo_variable() {
        // `__ENCODING__` evaluates to the source `Encoding` object (UTF-8
        // for this harness), usable in every expression position.
        run_test(r#"__ENCODING__.name"#);
        run_test(r#"__ENCODING__.is_a?(Encoding)"#);
        run_test(r#"x = __ENCODING__; x.name"#);
        run_test(r#"[__ENCODING__].first.name"#);
        run_test(r#"def f(e); e.name; end; f(__ENCODING__)"#);
        // Identity: same object as the corresponding Encoding constant.
        run_test(r#"__ENCODING__.equal?(Encoding::UTF_8)"#);
        // Void context is a no-op.
        run_test(r#"__ENCODING__; 42"#);
    }
}
