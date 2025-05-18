use super::*;

impl BytecodeGen {
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
                let sym = IdentId::get_id_from_string(sym);
                self.emit_symbol(dst, sym)
            }
            NodeKind::Bignum(bigint) => self.emit_bigint(dst, bigint),
            NodeKind::Float(f) => self.emit_float(dst, f),
            NodeKind::Imaginary(r) => self.emit_imaginary(dst, r.into()),
            NodeKind::String(s) => self.emit_string(dst, s),
            NodeKind::Bytes(b) => self.emit_bytes(dst, b),
            NodeKind::Array(nodes, false) => self.gen_array(dst, nodes, loc)?,
            NodeKind::Hash(nodes, false) => self.gen_hash(dst, nodes, loc)?,
            NodeKind::RegExp(nodes, op, false) => self.gen_regexp(dst, nodes, op, loc)?,
            NodeKind::Array(_, true)
            | NodeKind::Hash(_, true)
            | NodeKind::Range { is_const: true, .. } => {
                let val = Value::from_ast2(&rhs);
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
                if index.len() == 1 {
                    self.gen_index(Some(dst), base, index.remove(0), loc)?;
                } else {
                    let arglist = ArgList::from_args(index);
                    self.gen_method_call(
                        IdentId::_INDEX,
                        Some(base),
                        arglist,
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
                        _ => self.emit_neg(dst, rhs, loc)?,
                    };
                }
                UnOp::Pos => {
                    match rhs.kind {
                        NodeKind::Integer(i) => self.emit_integer(dst, i),
                        NodeKind::Imaginary(r) => self.emit_imaginary(dst, r.into()),
                        NodeKind::Float(f) => self.emit_float(dst, f),
                        _ => self.emit_pos(dst, rhs, loc)?,
                    };
                }
                UnOp::Not => self.emit_not(dst, rhs, loc)?,
                UnOp::BitNot => self.emit_bitnot(dst, rhs, loc)?,
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
                    } else {
                        let temp = self.temp;
                        let (lhs, rest) = self.eval_lvalue(&lhs)?;
                        assert!(!rest);
                        self.gen_store_expr(dst, rhs)?;
                        self.emit_assign(dst, lhs, Some(temp), loc);
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
            NodeKind::SpecialVar(id) => {
                self.emit_load_svar(dst.into(), id, loc);
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
                self.gen_method_call(method, None, arglist, safe_nav, UseMode2::Store(dst), loc)?;
            }
            NodeKind::Return(box val) => {
                if self.is_block() {
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
                | NodeKind::String(_) => return Ok(()),
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
            | NodeKind::String(_)
            | NodeKind::Bytes(_)
            | NodeKind::Array(..)
            | NodeKind::Hash(..)
            | NodeKind::Range { .. }
            | NodeKind::RegExp(_, _, _)
            | NodeKind::Lambda(_)
            | NodeKind::UnOp(..)
            | NodeKind::Const { .. }
            | NodeKind::InstanceVar(_)
            | NodeKind::ClassVar(_)
            | NodeKind::GlobalVar(_)
            | NodeKind::SpecialVar(_) => {
                let ret = self.push().into();
                self.gen_store_expr(ret, expr)?;
            }
            NodeKind::BinOp(op, box lhs, box rhs) => {
                self.gen_binop(op, lhs, rhs, use_mode, loc)?;
                return Ok(());
            }
            NodeKind::Index {
                box base,
                mut index,
            } => {
                if index.len() == 1 {
                    self.gen_index(None, base, index.remove(0), loc)?;
                } else {
                    let arglist = ArgList::from_args(index);
                    self.gen_method_call(
                        IdentId::_INDEX,
                        Some(base),
                        arglist,
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
                    let temp = self.temp;
                    let (lhs, rest) = self.eval_lvalue(&lhs)?;
                    if rest {
                        let rhs_loc = rhs.loc;
                        let src = self.gen_expr_reg(rhs)?;
                        self.emit_array(src, src, 1, vec![], rhs_loc);
                        return self.assign_with_mode(use_mode, src, lhs, temp, loc);
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
                } else {
                    assert_eq!(Some(lvar), self.outer_block_param_name(outer));
                    self.emit(BytecodeInst::BlockArg(ret, outer), loc);
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
                return self.gen_method_call(method, None, arglist, safe_nav, use_mode, loc);
            }
            NodeKind::Super(arglist) => {
                return self.gen_super(arglist.map(|arglist| *arglist), use_mode, loc);
            }
            NodeKind::Command(box expr) => {
                let arglist = ArgList::from_args(vec![expr]);
                let method = IdentId::get_id("`");
                return self.gen_method_call(method, None, arglist, false, use_mode, loc);
            }
            NodeKind::Yield(arglist) => {
                return self.gen_yield(*arglist, use_mode, loc);
            }
            NodeKind::Ident(method) => {
                let arglist = ArgList::default();
                let method = IdentId::get_id_from_string(method);
                return self.gen_method_call(method, None, arglist, false, use_mode, loc);
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
                    break_dest, ret, ..
                } = match self.loops.last() {
                    Some(data) => data.clone(),
                    None => {
                        if self.is_block() {
                            self.gen_block_break(val, use_mode)?;
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
                self.emit(BytecodeInst::Br(break_dest), loc);
                if use_mode == UseMode2::Push {
                    self.push();
                }
                return Ok(());
            }
            NodeKind::Next(box val) => {
                let LoopInfo { next_dest, .. } = match self.loops.last() {
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
                let LoopInfo { redo_dest, .. } = match self.loops.last() {
                    Some(data) => data,
                    None => {
                        if self.is_block() {
                            self.emit(BytecodeInst::Br(self.redo_label), loc);
                            return Ok(());
                        } else {
                            return Err(self.escape_from_eval("redo", loc));
                        }
                    }
                };
                self.emit(BytecodeInst::Br(*redo_dest), loc);
                return Ok(());
            }
            NodeKind::Return(box val) => {
                if self.is_block() {
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
                let len = nodes.len();
                let arg = self.sp();
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
                match (new.kind, old.kind) {
                    (NodeKind::Symbol(new), NodeKind::Symbol(old)) => {
                        let temp = self.temp;
                        let new = IdentId::get_id_from_string(new);
                        let old = IdentId::get_id_from_string(old);
                        self.temp = temp;
                        self.emit(BytecodeInst::AliasMethod { new, old }, loc);
                    }
                    _ => unimplemented!(),
                };
                match use_mode {
                    UseMode2::Ret => {
                        self.push_nil();
                        self.emit_ret(None)?;
                    }
                    UseMode2::NotUse => {}
                    UseMode2::Push => {
                        self.push_nil();
                    }
                    _ => unreachable!(),
                }
                return Ok(());
            }
            NodeKind::Defined(box node) => {
                self.gen_defined(node)?;
            }
            NodeKind::Splat(..) => {
                return Err(self.unsupported_lhs(&expr));
            }
            NodeKind::DiscardLhs => unreachable!(),
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

impl BytecodeGen {
    ///
    /// Generate multiple assignment.
    ///
    /// This func always use a new temporary register for rhs even if the number of rhs is 1.
    ///
    fn gen_mul_assign(
        &mut self,
        mlhs: Vec<Node>,
        mut mrhs: Vec<Node>,
        use_mode: UseMode2,
    ) -> Result<()> {
        let mlhs_len = mlhs.len();
        let mut mrhs_len = mrhs.len();
        let loc = mlhs[0].loc().merge(mrhs.last().unwrap().loc());
        if mlhs_len != mrhs_len && mrhs_len != 1 {
            return Err(self.unsupported_feature("mlhs_len != mrhs_len", loc));
        };

        let temp = self.temp;

        // At first, we evaluate lvalues and save their info(LhsKind).
        let mut lhs_kind = vec![];
        let mut rest_pos = None;
        for (i, lhs) in mlhs.iter().enumerate() {
            let (lhs, rest) = self.eval_lvalue(&lhs)?;
            if rest {
                rest_pos = Some(i as u16)
            }
            lhs_kind.push(lhs);
        }

        // Next, we evaluate rvalues and save them in temporary registers which start from temp_reg.
        let (rhs_reg, ret_val) = if mlhs_len != 1 && mrhs_len == 1 {
            let rhs = self.push_expr(std::mem::take(&mut mrhs[0]))?.into();
            mrhs_len = mlhs_len;
            let rhs_reg = self.sp();
            let lhs_len = lhs_kind
                .iter()
                .filter(|kind| kind != &&LvalueKind::Discard)
                .count();
            for _ in 0..lhs_len {
                self.push();
            }
            self.emit(
                BytecodeInst::ExpandArray(rhs, rhs_reg.into(), lhs_len as u16, rest_pos),
                Loc::default(),
            );
            // lhs0, lhs1, .. = rhs
            //
            //   temp               rhs   rhs_reg         sp
            //   v                  v     v               v
            // -+-----------------+-----+---------------+--
            //  | lhs working reg | rhs | lhs * lhs_len |
            // -+-----------------+-----+---------------+--
            //
            (rhs_reg, Some(rhs))
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
            self.emit_assign(src, lhs, None, loc);
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

    fn gen_index(&mut self, ret: Option<BcReg>, base: Node, index: Node, loc: Loc) -> Result<()> {
        let old = self.temp;
        let base = self.gen_expr_reg(base)?;
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
impl BytecodeGen {
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

    fn gen_array(&mut self, ret: BcReg, nodes: Vec<Node>, loc: Loc) -> Result<()> {
        let (src, len, splat) = self.ordinary_args(nodes)?;
        self.popn(len);
        self.emit_array(ret, src, len, splat, loc);
        Ok(())
    }

    fn gen_hash(&mut self, ret: BcReg, nodes: Vec<(Node, Node)>, loc: Loc) -> Result<()> {
        let len = nodes.len();
        let old_reg = self.temp;
        let args = self.sp();
        for (k, v) in nodes {
            self.push_expr(k)?;
            self.push_expr(v)?;
        }
        self.temp = old_reg;
        self.emit_hash(ret, args.into(), len, loc);
        Ok(())
    }

    fn gen_regexp(&mut self, ret: BcReg, nodes: Vec<Node>, option: String, loc: Loc) -> Result<()> {
        let mut len = nodes.len();
        let arg = self.sp();
        if !option.is_empty() {
            let r = self.push().into();
            self.emit_literal(r, Value::string(format!("(?{option})")));
            len += 1;
        }
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
        start: Node,
        end: Node,
        exclude_end: bool,
        loc: Loc,
    ) -> Result<()> {
        let old = self.temp;
        let start = self.gen_expr_reg(start)?;
        let end = self.gen_expr_reg(end)?;
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
        let func = self.handle_lambda(*info);
        self.emit(
            BytecodeInst::Lambda {
                dst,
                func: Box::new(func),
            },
            loc,
        );
        Ok(())
    }

    fn const_regexp(&self, nodes: Vec<Node>, option: String, loc: Loc) -> Result<Value> {
        let mut string = String::new();
        let option = onigmo_regex::ONIG_OPTION_NONE
            | if option.contains('i') {
                onigmo_regex::ONIG_OPTION_IGNORECASE
            } else {
                0
            }
            | if option.contains('x') {
                onigmo_regex::ONIG_OPTION_EXTEND
            } else {
                0
            }
            | if option.contains('m') {
                onigmo_regex::ONIG_OPTION_MULTILINE
            } else {
                0
            };
        for node in nodes {
            match &node.kind {
                NodeKind::String(s) => string += s,
                _ => unreachable!(),
            }
        }
        let re = match RegexpInner::with_option(string, option) {
            Ok(re) => re,
            Err(err) => return Err(self.syntax_error(err.msg(), loc)),
        };
        Ok(Value::regexp(re))
    }
}

//
// Definitions
//
impl BytecodeGen {
    fn gen_method_def(
        &mut self,
        name: IdentId,
        block: BlockInfo,
        use_mode: UseMode2,
        loc: Loc,
    ) -> Result<()> {
        let func = self.add_method(Some(name), block);
        self.emit(
            BytecodeInst::MethodDef {
                name,
                func: Box::new(func),
            },
            loc,
        );
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
        let func = self.add_method(Some(name), block);
        let obj = self.pop().into();
        self.emit(
            BytecodeInst::SingletonMethodDef {
                obj,
                name,
                func: Box::new(func),
            },
            loc,
        );
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
        let func = self.add_classdef(Some(name), info);
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
                    func: Box::new(func),
                }
            } else {
                BytecodeInst::ClassDef {
                    ret,
                    base,
                    superclass,
                    name,
                    func: Box::new(func),
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
        let func = self.add_classdef(None, info);
        let old = self.temp;
        let base = self.gen_expr_reg(base)?;
        self.temp = old;
        let ret = match use_mode {
            UseMode2::NotUse => None,
            UseMode2::Push | UseMode2::Ret => Some(self.push().into()),
            UseMode2::Store(r) => Some(r),
        };
        self.emit(
            BytecodeInst::SingletonClassDef {
                ret,
                base,
                func: Box::new(func),
            },
            loc,
        );
        if use_mode == UseMode2::Ret {
            self.emit_ret(None)?;
        }
        Ok(())
    }
}

//
// Flow control
//
impl BytecodeGen {
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
