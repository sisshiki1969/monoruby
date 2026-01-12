use super::*;

mod arguments;

impl<'a> BytecodeGen<'a> {
    pub(super) fn emit_call(&mut self, callsite: CallSite, loc: Loc) {
        if callsite.block_fid.is_some() {
            self.emit(
                BytecodeInst::MethodCallBlock(Box::new(callsite.clone())),
                loc,
            )
        } else {
            self.emit(BytecodeInst::MethodCall(Box::new(callsite.clone())), loc);
        };
        self.emit(BytecodeInst::InlineCache(Box::new(callsite)), loc);
    }

    pub(super) fn emit_yield(&mut self, callsite: CallSite, loc: Loc) {
        self.emit(BytecodeInst::Yield(Box::new(callsite.clone())), loc);
        self.emit(BytecodeInst::InlineCache(Box::new(callsite)), loc);
    }

    pub(super) fn emit_binary_op(
        &mut self,
        method: IdentId,
        lhs: BcReg,
        rhs: BcReg,
        ret: Option<BcReg>,
        loc: Loc,
    ) {
        let callsite = CallSite::simple(method, 1, rhs, lhs, ret);
        self.emit_call(callsite, loc);
    }

    pub(super) fn emit_method_assign(&mut self, callsite: CallSite, loc: Loc) {
        self.emit_call(callsite, loc);
    }

    pub(super) fn gen_method_call(
        &mut self,
        method: IdentId,
        receiver: Option<Node>,
        arglist: ArgList,
        safe_nav: bool,
        use_mode: UseMode2,
        loc: Loc,
    ) -> Result<()> {
        let (dst, push_flag) = match use_mode {
            UseMode2::NotUse => (None, false),
            UseMode2::Push | UseMode2::Ret => (Some(self.sp().into()), true),
            UseMode2::Store(dst) => (Some(dst), false),
        };
        let old_temp = self.temp;
        let recv = match receiver {
            Some(receiver) => {
                if receiver.kind == NodeKind::SelfValue {
                    BcReg::Self_
                } else if self.is_refer_block_arg(&receiver) {
                    self.push_expr(receiver)?.into()
                } else if let Some(local) = self.is_refer_local(&receiver) {
                    local
                } else {
                    self.push_expr(receiver)?.into()
                }
            }
            None => BcReg::Self_,
        };

        let nil_exit = if safe_nav {
            let nil_exit = self.new_label();
            let org_temp = self.temp;
            self.temp = old_temp;
            if push_flag {
                self.push();
            }
            self.emit_nilbr(recv, nil_exit);
            self.temp = org_temp;
            Some(nil_exit)
        } else {
            None
        };

        let callid = self.handle_arguments(arglist, method, recv, dst, loc)?;

        self.temp = old_temp;
        if push_flag {
            self.push();
        }
        self.emit_call(callid, loc);
        if let Some(nil_exit) = nil_exit {
            if let Some(dst) = dst
                && dst != recv
            {
                let exit = self.new_label();
                self.emit_br(exit);
                self.apply_label(nil_exit);
                self.emit_nil(dst);
                self.apply_label(exit);
            } else {
                self.apply_label(nil_exit);
            }
        }
        if use_mode.is_ret() {
            self.emit_ret(None)?;
        }
        Ok(())
    }

    pub(super) fn gen_super(
        &mut self,
        arglist: Option<ArgList>,
        use_mode: UseMode2,
        loc: Loc,
    ) -> Result<()> {
        let (dst, ret_pop_flag) = match use_mode {
            UseMode2::NotUse => (None, false),
            UseMode2::Push | UseMode2::Ret => (Some(self.sp().into()), true),
            UseMode2::Store(dst) => (Some(dst), false),
        };
        let old = self.temp;
        let callid = self.handle_super_arguments(arglist, dst, loc)?;
        self.temp = old;
        if ret_pop_flag {
            self.push();
        };
        self.emit_call(callid, loc);
        if use_mode.is_ret() {
            self.emit_ret(None)?;
        }
        Ok(())
    }

    pub(super) fn gen_each(
        &mut self,
        param: Vec<(usize, String)>,
        iter: Node,
        mut block: BlockInfo,
        use_mode: UseMode2,
        loc: Loc,
    ) -> Result<()> {
        // collect assignments for local variables.
        let mut optional_params = vec![];
        for (outer, name) in param {
            let name = IdentId::get_id_from_string(name);
            let r = if outer == 0 {
                self.assign_local(name)
            } else {
                self.refer_dynamic_local(outer, name).unwrap()
            };
            optional_params.push((outer + 1, r, name));
        }
        self.level_down(&mut block.body, 0);
        let recv_kind = if iter.kind == NodeKind::SelfValue {
            RecvKind::SelfValue
        } else if let Some(local) = self.is_refer_local(&iter) {
            RecvKind::Local(local)
        } else {
            self.push_expr(iter)?;
            RecvKind::Temp
        };

        let old_temp = self.temp;
        let arg = self.sp();
        let block_fid = self.handle_block(optional_params, block)?;
        self.push_nil();
        self.temp = old_temp;

        let recv = match recv_kind {
            RecvKind::SelfValue => BcReg::Self_,
            RecvKind::Local(reg) => reg,
            RecvKind::Temp => self.pop().into(),
        };
        let dst = match use_mode {
            UseMode2::NotUse => None,
            UseMode2::Push | UseMode2::Ret => Some(self.push().into()),
            UseMode2::Store(dst) => Some(dst),
        };
        let callsite = CallSite::new(
            IdentId::EACH,
            0,
            None,
            vec![],
            Some(block_fid),
            None,
            arg.into(),
            recv,
            dst,
            false,
        );
        self.emit_call(callsite, loc);
        if use_mode.is_ret() {
            self.emit_ret(None)?;
        }
        Ok(())
    }

    pub(super) fn gen_yield(
        &mut self,
        arglist: ArgList,
        use_mode: UseMode2,
        loc: Loc,
    ) -> Result<()> {
        let dst = match use_mode {
            UseMode2::NotUse => None,
            UseMode2::Push | UseMode2::Ret => Some(self.push().into()),
            UseMode2::Store(dst) => Some(dst),
        };
        let old = self.temp;
        // TODO: We must check this in parser
        if arglist.forwarding {
            return Err(self.syntax_error("Delegate argument should not be given", loc));
        }
        // yield does not accept block.
        if arglist.block.is_some() {
            return Err(self.syntax_error("Block argument should not be given", loc));
        }

        let callid =
            self.handle_no_forward(arglist, IdentId::get_id("<block>"), BcReg::Self_, dst, loc)?;
        self.emit_yield(callid, loc);

        self.temp = old;

        if use_mode.is_ret() {
            self.emit_ret(None)?;
        }
        Ok(())
    }
}

impl<'a> BytecodeGen<'a> {
    fn level_down(&mut self, node: &mut Node, level: usize) {
        match &mut node.kind {
            NodeKind::LocalVar(l, _) => {
                if *l >= level {
                    *l += 1;
                }
            }
            NodeKind::MulAssign(n1, n2) => {
                n1.iter_mut().for_each(|n| {
                    if level == 0 {
                        if let NodeKind::LocalVar(0, name) = &n.kind {
                            let name = IdentId::get_id(name);
                            self.assign_local(name);
                        } else if let NodeKind::Splat(n) = &n.kind {
                            if let NodeKind::LocalVar(0, name) = &n.kind {
                                let name = IdentId::get_id(name);
                                self.assign_local(name);
                            }
                        }
                    }
                    self.level_down(n, level);
                });
                n2.iter_mut().for_each(|n| self.level_down(n, level));
            }
            NodeKind::Lambda(box BlockInfo { params, body, .. }) => {
                self.level_down(body, level + 1);
                for p in params {
                    match &mut p.kind {
                        ruruby_parse::ParamKind::Optional(_, n) => {
                            self.level_down(n, level);
                        }
                        ruruby_parse::ParamKind::Keyword(_, Some(n)) => {
                            self.level_down(n, level);
                        }
                        _ => {}
                    }
                }
            }
            NodeKind::SelfValue
            | NodeKind::Nil
            | NodeKind::Integer(_)
            | NodeKind::Bignum(_)
            | NodeKind::Float(_)
            | NodeKind::Imaginary(_)
            | NodeKind::Bool(_)
            | NodeKind::String(_)
            | NodeKind::Bytes(_)
            | NodeKind::Symbol(_)
            | NodeKind::Ident(_)
            | NodeKind::InstanceVar(_)
            | NodeKind::GlobalVar(_)
            | NodeKind::SpecialVar(_)
            | NodeKind::ClassVar(_)
            | NodeKind::MethodDef(..)
            | NodeKind::SingletonMethodDef(..)
            | NodeKind::ClassDef { .. }
            | NodeKind::SingletonClassDef { .. }
            | NodeKind::Redo
            | NodeKind::DiscardLhs => {}
            NodeKind::CompStmt(nodes)
            | NodeKind::InterporatedString(nodes)
            | NodeKind::Array(nodes, ..)
            | NodeKind::RegExp(nodes, ..) => {
                nodes.iter_mut().for_each(|n| self.level_down(n, level));
            }
            NodeKind::Command(n)
            | NodeKind::UnOp(_, n)
            | NodeKind::Splat(n)
            | NodeKind::Break(n)
            | NodeKind::Next(n)
            | NodeKind::Return(n)
            | NodeKind::Defined(n) => {
                self.level_down(n, level);
            }
            NodeKind::Const { parent, .. } => {
                if let Some(n) = parent {
                    self.level_down(n, level);
                }
            }
            NodeKind::BinOp(_, box n1, box n2)
            | NodeKind::AssignOp(_, box n1, box n2)
            | NodeKind::While {
                cond: box n1,
                body: box n2,
                ..
            }
            | NodeKind::AliasMethod(box n1, box n2) => {
                self.level_down(n1, level);
                self.level_down(n2, level);
            }
            NodeKind::Range {
                start: box n1,
                end: box n2,
                ..
            } => {
                if let Some(n1) = n1 {
                    self.level_down(n1, level);
                }
                if let Some(n2) = n2 {
                    self.level_down(n2, level);
                }
            }
            NodeKind::UndefMethod(box n1) => {
                self.level_down(n1, level);
            }
            NodeKind::If {
                cond: n1,
                then_: n2,
                else_: n3,
            } => {
                self.level_down(n1, level);
                self.level_down(n2, level);
                self.level_down(n3, level);
            }
            NodeKind::Hash(pairs) => pairs.iter_mut().for_each(|(n1, n2)| {
                self.level_down(n1, level);
                self.level_down(n2, level);
            }),
            NodeKind::FuncCall { arglist, .. } | NodeKind::Yield(arglist) => {
                self.level_down_arglist(arglist, level);
            }
            NodeKind::MethodCall {
                receiver, arglist, ..
            } => {
                self.level_down(receiver, level);
                self.level_down_arglist(arglist, level);
            }
            NodeKind::Index { base, index } => {
                self.level_down(base, level);
                index.iter_mut().for_each(|n| self.level_down(n, level));
            }
            NodeKind::For {
                param,
                iter,
                box body,
            } => {
                for (outer, name) in param {
                    if level == *outer {
                        let name = IdentId::get_id(name);
                        self.assign_local(name);
                    }
                    if *outer >= level {
                        *outer += 1;
                    }
                }
                self.level_down(iter, level);
                let BlockInfo { params, body, .. } = body;
                self.level_down(body, level);
                for p in params {
                    match &mut p.kind {
                        ruruby_parse::ParamKind::Optional(_, n) => {
                            self.level_down(n, level);
                        }
                        ruruby_parse::ParamKind::Keyword(_, Some(n)) => {
                            self.level_down(n, level);
                        }
                        _ => {}
                    }
                }
            }
            NodeKind::Case { cond, when_, else_ } => {
                if let Some(n) = cond {
                    self.level_down(n, level);
                }
                self.level_down(else_, level);
                for CaseBranch { when, body } in when_ {
                    when.iter_mut().for_each(|n| self.level_down(n, level));
                    self.level_down(body, level);
                }
            }
            NodeKind::Super(args) => {
                if let Some(arglist) = args {
                    self.level_down_arglist(arglist, level);
                }
            }
            NodeKind::Begin {
                body,
                rescue,
                else_,
                ensure,
            } => {
                self.level_down(body, level);
                for ruruby_parse::RescueEntry {
                    exception_list,
                    assign,
                    body,
                } in rescue
                {
                    exception_list
                        .iter_mut()
                        .for_each(|n| self.level_down(n, level));
                    if let Some(n) = assign {
                        self.level_down(n, level);
                    }
                    self.level_down(body, level);
                }
                if let Some(n) = else_ {
                    self.level_down(n, level);
                }
                if let Some(n) = ensure {
                    self.level_down(n, level);
                }
            }
        }
    }

    fn level_down_arglist(&mut self, arglist: &mut ArgList, level: usize) {
        let ArgList {
            args,
            kw_args,
            hash_splat,
            block,
            ..
        } = arglist;
        args.iter_mut().for_each(|n| self.level_down(n, level));
        kw_args
            .iter_mut()
            .for_each(|(_, n)| self.level_down(n, level));
        hash_splat
            .iter_mut()
            .for_each(|n| self.level_down(n, level));
        if let Some(n) = block {
            self.level_down(n, level);
        }
    }
}
