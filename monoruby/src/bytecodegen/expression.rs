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

    ///
    /// Evaluate *expr* and
    ///
    /// * If *use_mode* is `UseMode2::Store(r)`, the result is stored in r.
    /// * If *use_mode* is `UseMode2::NotUse`, the result will be discarded.
    /// * If *use_mode* is `UseMode2::Push`, push the result to the stack and return the register.
    /// * If *use_mode* is `UseMode2::Ret`, emit BcIr::Ret with the result.
    ///
    pub(super) fn gen_expr(&mut self, expr: Node, use_mode: UseMode2) -> Result<()> {
        let use_mode = match use_mode {
            UseMode2::Store(r) => return self.gen_store_expr(r, expr),
            UseMode2::NotUse => UseMode::NotUse,
            UseMode2::Push => UseMode::Push,
            UseMode2::Ret => UseMode::Ret,
        };
        let old = self.temp;
        //let loc = expr.loc;
        self.gen_expr_inner(expr, use_mode)?;
        match use_mode {
            UseMode::Push => {
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
            NodeKind::String(s) => self.emit_string(dst, s),
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
            NodeKind::Index {
                box base,
                mut index,
            } => {
                if index.len() == 1 {
                    self.gen_index(Some(dst), base, index.remove(0), loc)?;
                } else if index.len() == 2 {
                    let arglist = ArgList::from_args(index);
                    self.gen_method_call(
                        IdentId::_INDEX,
                        Some(base),
                        arglist,
                        Some(dst),
                        UseMode::Push,
                        loc,
                    )?;
                } else {
                    return Err(MonorubyErr::unsupported_feature(
                        &format!("unsupported index. {}", index.len()),
                        loc,
                        self.sourceinfo.clone(),
                    ));
                };
            }
            NodeKind::UnOp(op, box rhs) => match op {
                UnOp::Neg => {
                    match rhs.kind {
                        NodeKind::Integer(i) => self.emit_integer(dst, -i),
                        NodeKind::Float(f) => self.emit_float(dst, -f),
                        _ => self.emit_neg(dst, rhs, loc)?,
                    };
                }
                UnOp::Pos => {
                    match rhs.kind {
                        NodeKind::Integer(i) => self.emit_integer(dst, i),
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
                        let lhs = self.eval_lvalue(&lhs)?;
                        self.gen_store_expr(dst, rhs)?;
                        self.emit_assign(dst, lhs, Some(temp), loc);
                    }
                } else {
                    self.gen_mul_assign(mlhs, mrhs, UseMode::Push)?;
                    let temp = self.pop().into();
                    self.emit_mov(dst, temp);
                }
            }
            NodeKind::LocalVar(0, ident) => {
                if let Some(local2) = self.refer_local(&ident) {
                    self.emit_mov(dst, local2.into());
                } else {
                    self.emit(BcIr::BlockArg(dst, 0), loc);
                }
            }
            NodeKind::LocalVar(outer, ident) => {
                let ret = dst.into();
                let name = IdentId::get_id_from_string(ident);
                if let Some(src) = self.refer_dynamic_local(outer, name) {
                    let src = src.into();
                    self.emit(BcIr::LoadDynVar { ret, src, outer }, loc);
                } else {
                    assert_eq!(Some(name), self.block_param);
                    self.emit(BcIr::BlockArg(dst, outer), loc);
                }
            }
            NodeKind::Const {
                toplevel,
                name,
                parent,
                prefix,
            } => {
                let base: Option<BcReg> = if let Some(box parent) = parent {
                    let base = self.sp().into();
                    self.gen_temp_expr(parent)?;
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
                self.emit_load_svar(dst.into(), id as u32, loc);
            }
            NodeKind::MethodCall {
                box receiver,
                method,
                arglist,
                safe_nav: false,
            } => {
                let method = IdentId::get_id_from_string(method);
                self.gen_method_call(
                    method,
                    Some(receiver),
                    arglist,
                    Some(dst),
                    UseMode::Push,
                    loc,
                )?;
            }
            NodeKind::FuncCall {
                method,
                arglist,
                safe_nav: false,
            } => {
                let method = IdentId::get_id_from_string(method);
                self.gen_method_call(method, None, arglist, Some(dst), UseMode::Push, loc)?;
            }
            NodeKind::Return(box val) => {
                if self.is_block() {
                    return self.gen_method_return(val, UseMode2::Store(dst));
                } else {
                    return self.gen_return(val, UseMode2::Store(dst));
                }
            }
            NodeKind::CompStmt(nodes) => {
                self.gen_comp_stmts(nodes, Some(dst), UseMode::NotUse)?;
            }
            NodeKind::ClassDef {
                base,
                name,
                superclass,
                info,
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
        ret: Option<BcReg>,
        use_mode: UseMode,
    ) -> Result<()> {
        let last = match nodes.pop() {
            Some(node) => node,
            None => Node::new_nil(Loc(0, 0)),
        };
        for node in nodes.into_iter() {
            self.gen_expr(node, UseMode2::NotUse)?;
        }
        match ret {
            Some(ret) => {
                self.gen_store_expr(ret, last)?;
                self.handle_mode(use_mode, ret)?;
            }
            None => {
                self.gen_expr(last, use_mode.into())?;
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
    fn gen_expr_inner(&mut self, expr: Node, use_mode: UseMode) -> Result<()> {
        if !use_mode.use_val() {
            match &expr.kind {
                NodeKind::Nil
                | NodeKind::Bool(_)
                | NodeKind::SelfValue
                | NodeKind::Integer(_)
                | NodeKind::Symbol(_)
                | NodeKind::Bignum(_)
                | NodeKind::Float(_)
                | NodeKind::String(_) => return Ok(()),
                _ => {}
            }
        }
        let loc = expr.loc;
        match expr.kind {
            NodeKind::Nil
            | NodeKind::Bool(_)
            | NodeKind::SelfValue
            | NodeKind::Integer(_)
            | NodeKind::Symbol(_)
            | NodeKind::Bignum(_)
            | NodeKind::Float(_)
            | NodeKind::String(_)
            | NodeKind::Array(..)
            | NodeKind::Hash(..)
            | NodeKind::Range { .. }
            | NodeKind::RegExp(_, _, _)
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
                self.gen_binop(op, lhs, rhs, use_mode.into(), loc)?;
                return Ok(());
            }
            NodeKind::Index {
                box base,
                mut index,
            } => {
                if index.len() == 1 {
                    self.gen_index(None, base, index.remove(0), loc)?;
                } else if index.len() == 2 {
                    let arglist = ArgList::from_args(index);
                    self.gen_method_call(
                        IdentId::_INDEX,
                        Some(base),
                        arglist,
                        None,
                        use_mode,
                        loc,
                    )?;
                    return Ok(());
                } else {
                    return Err(MonorubyErr::unsupported_feature(
                        &format!("unsupported index. {}", index.len()),
                        loc,
                        self.sourceinfo.clone(),
                    ));
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
                let lhs_kind = self.eval_lvalue(&lhs)?;
                // Evaluate rvalue.
                let src = self.sp().into();
                self.gen_binop(op, lhs, rhs, UseMode2::Push, loc)?;
                // Assign rvalue to lvalue.
                match use_mode {
                    UseMode::Ret => {
                        self.emit_assign(src, lhs_kind, None, lhs_loc);
                        self.temp = temp;
                        self.emit_ret(Some(src))?;
                    }
                    UseMode::Push => {
                        self.emit_assign(src, lhs_kind, None, lhs_loc);
                        self.temp = temp;
                        let dst = self.push();
                        self.emit_mov(dst.into(), src);
                    }
                    UseMode::NotUse => {
                        self.emit_assign(src, lhs_kind, Some(temp), lhs_loc);
                    }
                }
                return Ok(());
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
                    let lhs = self.eval_lvalue(&lhs)?;
                    let src = self.gen_expr_reg(rhs)?;

                    match use_mode {
                        UseMode::Ret => {
                            self.emit_assign(src, lhs, None, loc);
                            self.temp = temp;
                            self.emit_ret(Some(src))?;
                        }
                        UseMode::Push => {
                            self.emit_assign(src, lhs, None, loc);
                            self.temp = temp;
                            let dst = self.push();
                            self.emit_mov(dst.into(), src);
                        }
                        UseMode::NotUse => {
                            self.emit_assign(src, lhs, Some(temp), loc);
                        }
                    }
                    return Ok(());
                } else {
                    return self.gen_mul_assign(mlhs, mrhs, use_mode);
                }
            }
            NodeKind::LocalVar(0, ident) => {
                if let Some(local) = self.refer_local(&ident) {
                    self.handle_mode(use_mode, local.into())?;
                    return Ok(());
                } else {
                    let ret = self.push().into();
                    self.emit(BcIr::BlockArg(ret, 0), loc);
                }
            }
            NodeKind::LocalVar(outer, ident) => {
                let ret = self.push().into();
                let lvar = IdentId::get_id_from_string(ident);
                if let Some(src) = self.refer_dynamic_local(outer, lvar) {
                    let src = src.into();
                    self.emit(BcIr::LoadDynVar { ret, src, outer }, loc);
                } else {
                    assert_eq!(Some(lvar), self.outer_block_param_name(outer));
                    self.emit(BcIr::BlockArg(ret, outer), loc);
                }
            }

            NodeKind::MethodCall {
                box receiver,
                method,
                arglist,
                safe_nav: false,
            } => {
                let method = IdentId::get_id_from_string(method);
                return self.gen_method_call(method, Some(receiver), arglist, None, use_mode, loc);
            }
            NodeKind::FuncCall {
                method,
                arglist,
                safe_nav: false,
            } => {
                let method = IdentId::get_id_from_string(method);
                return self.gen_method_call(method, None, arglist, None, use_mode, loc);
            }
            NodeKind::Super(arglist) => {
                return self.gen_super(arglist, None, use_mode, loc);
            }
            NodeKind::Command(box expr) => {
                let arglist = ArgList::from_args(vec![expr]);
                let method = IdentId::get_id("`");
                return self.gen_method_call(method, None, arglist, None, use_mode, loc);
            }
            NodeKind::Yield(arglist) => {
                return self.gen_yield(arglist, use_mode, loc);
            }
            NodeKind::Ident(method) => {
                let arglist = ArgList::default();
                let method = IdentId::get_id_from_string(method);
                return self.gen_method_call(method, None, arglist, None, use_mode, loc);
            }
            NodeKind::If {
                box cond,
                box then_,
                box else_,
            } => {
                let else_pos = self.new_label();
                self.gen_opt_condbr(false, cond, else_pos)?;
                let old = self.temp;
                self.gen_expr(then_, use_mode.into())?;
                if else_.is_empty() && use_mode == UseMode::NotUse {
                    self.apply_label(else_pos);
                } else {
                    let succ_pos = self.new_label();
                    match use_mode {
                        UseMode::NotUse | UseMode::Push => {
                            self.emit_br(succ_pos);
                        }
                        UseMode::Ret => {}
                    }
                    self.temp = old;
                    self.apply_label(else_pos);
                    self.gen_expr(else_, use_mode.into())?;
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
                body,
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
                } = match self.loops.last().cloned() {
                    Some(data) => data,
                    None => {
                        if self.is_block() {
                            self.gen_break(val, use_mode.into())?;
                            return Ok(());
                        } else {
                            return Err(MonorubyErr::escape_from_eval(
                                loc,
                                self.sourceinfo.clone(),
                            ));
                        }
                    }
                };
                if let Some(reg) = ret {
                    self.gen_store_expr(reg, val)?;
                } else {
                    self.gen_expr(val, UseMode2::NotUse)?;
                }
                self.emit(BcIr::Br(break_dest), loc);
                if use_mode == UseMode::Push {
                    self.push();
                }
                return Ok(());
            }
            NodeKind::Next(box val) => {
                let LoopInfo { next_dest, .. } = match self.loops.last().cloned() {
                    Some(data) => data,
                    None => {
                        if self.is_block() {
                            self.gen_return(val, use_mode.into())?;
                            return Ok(());
                        } else {
                            return Err(MonorubyErr::escape_from_eval(
                                loc,
                                self.sourceinfo.clone(),
                            ));
                        }
                    }
                };
                self.gen_expr(val, UseMode2::NotUse)?;
                self.emit(BcIr::Br(next_dest), loc);
                if use_mode == UseMode::Push {
                    self.push();
                }
                return Ok(());
            }
            NodeKind::Return(box val) => {
                if self.is_block() {
                    self.gen_method_return(val, use_mode.into())?;
                } else {
                    self.gen_return(val, use_mode.into())?;
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
            NodeKind::MethodDef(name, block) => {
                let name = IdentId::get_id_from_string(name);
                self.gen_method_def(name, block, use_mode.into(), loc)?;
                return Ok(());
            }
            NodeKind::SingletonMethodDef(box obj, name, block) => {
                self.gen_expr(obj, UseMode2::Push)?;
                let name = IdentId::get_id_from_string(name);
                self.gen_singleton_method_def(name, block, use_mode.into(), loc)?;
                return Ok(());
            }
            NodeKind::ClassDef {
                base,
                name,
                superclass,
                info,
                is_module,
            } => {
                let name = IdentId::get_id_from_string(name);
                let use_mode = use_mode.into();
                self.gen_class_def(name, base, superclass, info, use_mode, is_module, loc)?;
                return Ok(());
            }
            NodeKind::SingletonClassDef {
                box singleton,
                info,
            } => {
                let use_mode = use_mode.into();
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
                self.emit(BcIr::ConcatStr(ret, arg, len), Loc::default());
                if use_mode.is_ret() {
                    self.emit_ret(None)?;
                }
                return Ok(());
            }
            NodeKind::AliasMethod(box new, box old) => {
                match (new.kind, old.kind) {
                    (NodeKind::Symbol(new), NodeKind::Symbol(old)) => {
                        let temp = self.temp;
                        let new = self.push_symbol(IdentId::get_id_from_string(new));
                        let old = self.push_symbol(IdentId::get_id_from_string(old));
                        self.temp = temp;
                        self.emit(BcIr::AliasMethod { new, old }, loc);
                    }
                    _ => unimplemented!(),
                };
                match use_mode {
                    UseMode::Ret => {
                        self.push_nil();
                        self.emit_ret(None)?;
                    }
                    UseMode::NotUse => {}
                    UseMode::Push => {
                        self.push_nil();
                    }
                }
                return Ok(());
            }
            NodeKind::Defined(box node) => {
                self.gen_defined(node)?;
            }
            _ => return Err(MonorubyErr::unsupported_node(expr, self.sourceinfo.clone())),
        }
        match use_mode {
            UseMode::Ret => {
                self.emit_ret(None)?;
            }
            UseMode::NotUse => {
                self.pop();
            }
            UseMode::Push => {}
        }
        Ok(())
    }

    fn handle_mode(&mut self, use_mode: UseMode, src: BcReg) -> Result<()> {
        match use_mode {
            UseMode::Ret => {
                self.emit_ret(Some(src))?;
            }
            UseMode::Push => {
                let dst = self.push();
                self.emit_mov(dst.into(), src);
            }
            UseMode::NotUse => {}
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
        use_mode: UseMode,
    ) -> Result<()> {
        let mlhs_len = mlhs.len();
        let mut mrhs_len = mrhs.len();
        let loc = mlhs[0].loc().merge(mrhs.last().unwrap().loc());
        if mlhs_len != mrhs_len && mrhs_len != 1 {
            return Err(MonorubyErr::unsupported_feature(
                "mlhs_len != mrhs_len",
                loc,
                self.sourceinfo.clone(),
            ));
        };

        let temp = self.temp;

        // At first, we evaluate lvalues and save their info(LhsKind).
        let mut lhs_kind: Vec<LvalueKind> = vec![];
        for lhs in &mlhs {
            lhs_kind.push(self.eval_lvalue(lhs)?);
        }

        // Next, we evaluate rvalues and save them in temporary registers which start from temp_reg.
        let (rhs_reg, ret_val) = if mlhs_len != 1 && mrhs_len == 1 {
            let rhs = self.push_expr(std::mem::take(&mut mrhs[0]))?.into();
            mrhs_len = mlhs_len;
            let rhs_reg = self.sp();
            for _ in 0..mlhs_len {
                self.push();
            }
            self.emit(
                BcIr::ExpandArray(rhs, rhs_reg.into(), mlhs_len as u16),
                Loc::default(),
            );
            (rhs_reg, Some(rhs))
        } else {
            let rhs_reg = self.sp();
            for rhs in mrhs {
                self.push_expr(rhs)?;
            }
            (rhs_reg, None)
        };
        let mut temp_reg = rhs_reg;

        // Finally, assign rvalues to lvalue.
        for (lhs, kind) in mlhs.into_iter().zip(lhs_kind) {
            if let Some(local) = self.is_assign_local(&lhs) {
                assert!(matches!(kind, LvalueKind::LocalVar { .. }));
                self.emit_mov(local.into(), temp_reg.into());
            } else {
                let src = temp_reg.into();
                self.emit_assign(src, kind, None, loc);
            }
            temp_reg += 1;
        }

        self.temp = temp;

        // Generate return value if needed.
        if use_mode.use_val() {
            let ret = self.push().into();
            if ret_val.is_none() {
                self.emit_array(ret, rhs_reg.into(), mrhs_len, vec![], loc);
            }
        }

        if use_mode.is_ret() {
            self.emit_ret(None)?;
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
        self.emit(BcIr::Index(ret, base, index), loc);
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

    fn gen_regexp(&mut self, ret: BcReg, nodes: Vec<Node>, op: String, loc: Loc) -> Result<()> {
        let len = nodes.len() + 1;
        let arg = self.push();
        self.emit_literal(arg.into(), Value::string(format!("(?{op})")));
        for expr in nodes {
            self.push_expr(expr)?;
        }
        self.temp -= len as u16;
        self.emit(BcIr::ConcatRegexp(Some(ret), arg, len), loc);
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
            BcIr::Range {
                ret,
                start,
                end,
                exclude_end,
            },
            loc,
        );
        Ok(())
    }

    fn const_regexp(&self, nodes: Vec<Node>, option: String, loc: Loc) -> Result<Value> {
        let mut string = String::new();
        for node in nodes {
            match &node.kind {
                NodeKind::String(s) => string += s,
                _ => unreachable!(),
            }
        }
        let string = format!("(?{}){}", option, string);
        let re = match RegexpInner::new(string) {
            Ok(re) => re,
            Err(err) => return Err(MonorubyErr::syntax(err, loc, self.sourceinfo.clone())),
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
            BcIr::MethodDef {
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
            BcIr::SingletonMethodDef {
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
            /*return Err(MonorubyErr::unsupported_feature(
                &format!("base in class def. {:?}", base.kind),
                loc,
                self.sourceinfo.clone(),
            ));*/
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
                BcIr::ModuleDef {
                    ret,
                    base,
                    name,
                    func: Box::new(func),
                }
            } else {
                BcIr::ClassDef {
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
            BcIr::SingletonClassDef {
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
            self.emit(BcIr::MethodRet(local.into()), Loc::default());
        } else {
            self.gen_expr(val, UseMode2::Push)?;
            let ret = self.pop().into();
            self.emit(BcIr::MethodRet(ret), Loc::default());
        }
        if use_mode == UseMode2::Push {
            self.push();
        }
        Ok(())
    }

    fn gen_break(&mut self, val: Node, use_mode: UseMode2) -> Result<()> {
        if let Some(local) = self.is_refer_local(&val) {
            self.emit(BcIr::Break(local.into()), Loc::default());
        } else {
            self.gen_expr(val, UseMode2::Push)?;
            let ret = self.pop().into();
            self.emit(BcIr::Break(ret), Loc::default());
        }
        if use_mode == UseMode2::Push {
            self.push();
        }
        Ok(())
    }
}
