use super::*;

impl BytecodeGen {
    pub(super) fn emit_call(&mut self, callsite: CallSite, loc: Loc) {
        if callsite.block_fid.is_some() {
            self.emit(BcIr::MethodCallBlock(Box::new(callsite.clone())), loc)
        } else {
            self.emit(BcIr::MethodCall(Box::new(callsite.clone())), loc);
        };
        self.emit(BcIr::InlineCache(Box::new(callsite)), loc);
    }

    pub(super) fn emit_yield(&mut self, callsite: CallSite, loc: Loc) {
        self.emit(BcIr::Yield(Box::new(callsite.clone())), loc);
        self.emit(BcIr::InlineCache(Box::new(callsite)), loc);
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
                    local.into()
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

        if arglist.delegate {
            return Err(MonorubyErr::unsupported_feature(
                "argument delegation is not supported.",
                loc,
                self.sourceinfo.clone(),
            ));
        }

        let callid = self.handle_arguments(arglist, method, recv, dst, loc)?;

        self.temp = old_temp;
        if push_flag {
            self.push();
        }
        self.emit_call(callid, loc);
        if let Some(nil_exit) = nil_exit {
            let exit = self.new_label();
            self.emit_br(exit);
            self.apply_label(nil_exit);
            if let Some(dst) = dst {
                self.emit_nil(dst);
            }
            self.apply_label(exit);
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
        let callid = if let Some(arglist) = arglist {
            assert!(!arglist.delegate);
            self.handle_arguments(arglist, None, BcReg::Self_, dst, loc)?
        } else {
            let (_, mother_args, outer) = self.mother.clone();
            let pos_len = mother_args.pos_num();
            let pos_start = if outer == 0 {
                BcLocal(0).into()
            } else {
                let args = self.sp().into();
                for i in 0..pos_len {
                    let dst = self.push().into();
                    let src = BcLocal(i as _).into();
                    self.emit(BcIr::LoadDynVar { dst, src, outer }, loc);
                }
                args
            };
            let kw_list = &mother_args.kw_names;
            let kw = if kw_list.is_empty() {
                None
            } else {
                let mut kw_args = IndexMap::default();
                let kw_start = if outer == 0 {
                    BcLocal(mother_args.pos_num() as u16).into()
                } else {
                    let dst = self.push().into();
                    let src = BcLocal(mother_args.pos_num() as u16).into();
                    self.emit(BcIr::LoadDynVar { dst, src, outer }, loc);
                    dst
                };
                for (id, name) in kw_list.iter().enumerate() {
                    kw_args.insert(*name, id);
                }
                Some(KeywordArgs {
                    kw_start,
                    kw_args,
                    hash_splat_pos: vec![],
                })
            };
            CallSite::new(
                None,
                pos_len,
                kw,
                vec![],
                None,
                None,
                pos_start,
                BcReg::Self_,
                dst,
            )
        };
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
            RecvKind::Local(local.into())
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
        if arglist.delegate {
            return Err(MonorubyErr::syntax(
                "Delegate argument should not be given".to_string(),
                loc,
                self.sourceinfo.clone(),
            ));
        }
        // yield does not accept block.
        if arglist.block.is_some() {
            return Err(MonorubyErr::syntax(
                "Block argument should not be given".to_string(),
                loc,
                self.sourceinfo.clone(),
            ));
        }

        let callid =
            self.handle_arguments(arglist, IdentId::get_id("<block>"), BcReg::Self_, dst, loc)?;
        self.emit_yield(callid, loc);

        self.temp = old;

        if use_mode.is_ret() {
            self.emit_ret(None)?;
        }
        Ok(())
    }

    fn handle_arguments(
        &mut self,
        mut arglist: ArgList,
        method: impl Into<Option<IdentId>>,
        recv: BcReg,
        dst: Option<BcReg>,
        loc: Loc,
    ) -> Result<CallSite> {
        let (args, pos_num, splat_pos) = self.positional_args(&mut arglist)?;

        let kw = self.keyword_arg(&mut arglist)?;

        let block_arg = self.sp().into();
        let block_fid = if let Some(box block) = std::mem::take(&mut arglist.block) {
            self.block_arg(block, loc)?
        } else {
            None
        };
        let block_arg = if block_arg == self.sp().into() {
            None
        } else {
            Some(block_arg)
        };

        let callsite = CallSite::new(
            method, pos_num, kw, splat_pos, block_fid, block_arg, args, recv, dst,
        );
        Ok(callsite)
    }

    ///
    /// Handle ordinary arguments.
    ///
    fn positional_args(&mut self, arglist: &mut ArgList) -> Result<(BcReg, usize, Vec<usize>)> {
        if arglist.args.len() == 1
            //&& arglist.block.is_none()
            && arglist.kw_args.is_empty()
            && arglist.hash_splat.is_empty()
            && !arglist.delegate
        {
            if let NodeKind::LocalVar(0, ident) = &arglist.args[0].kind {
                // in the case of "f(a)"
                let local = self.refer_local(ident).unwrap().into();
                return Ok((local, 1, vec![]));
            } else if let NodeKind::Splat(box node) = &arglist.args[0].kind {
                // in the case of "f(*a)"
                if let NodeKind::LocalVar(0, ident) = &node.kind {
                    let local = self.refer_local(ident).unwrap().into();
                    return Ok((local, 1, vec![0]));
                }
            }
        };

        let (args, arg_len, splat_pos) = self.ordinary_args(std::mem::take(&mut arglist.args))?;
        Ok((args, arg_len, splat_pos))
    }

    fn keyword_arg(&mut self, arglist: &mut ArgList) -> Result<Option<KeywordArgs>> {
        let kw_args_list = std::mem::take(&mut arglist.kw_args);
        let hash_splat = std::mem::take(&mut arglist.hash_splat);
        if kw_args_list.is_empty() && hash_splat.is_empty() {
            Ok(None)
        } else {
            let mut kw_args = IndexMap::default();
            let kw_start = self.sp().into();
            let mut hash_splat_pos = vec![];
            for (id, (name, node)) in kw_args_list.into_iter().enumerate() {
                self.push_expr(node)?;
                kw_args.insert(IdentId::get_id_from_string(name), id);
            }
            for node in hash_splat {
                hash_splat_pos.push(self.push_expr(node)?.into());
            }
            Ok(Some(KeywordArgs {
                kw_start,
                kw_args,
                hash_splat_pos,
            }))
        }
    }

    fn block_arg(&mut self, block: Node, loc: Loc) -> Result<Option<Functions>> {
        match block.kind {
            NodeKind::Lambda(block) => return Ok(Some(self.handle_block(vec![], block)?)),
            NodeKind::LocalVar(0, proc_local) => {
                let dst = self.push().into();
                if let Some(local) = self.refer_local(&proc_local) {
                    self.emit_mov(dst, local.into());
                } else {
                    self.emit(BcIr::BlockArgProxy(dst, 0), loc);
                }
            }
            NodeKind::LocalVar(outer, proc_local) => {
                let proc_local = IdentId::get_id_from_string(proc_local);
                let dst = self.push().into();
                if let Some(src) = self.refer_dynamic_local(outer, proc_local) {
                    let src = src.into();
                    self.emit(BcIr::LoadDynVar { dst, src, outer }, loc);
                } else {
                    assert_eq!(Some(proc_local), self.outer_block_param_name(outer));
                    self.emit(BcIr::BlockArgProxy(dst, outer), loc);
                }
            }
            _ => {
                self.push_expr(block)?;
            }
        }
        Ok(None)
    }

    fn handle_block(
        &mut self,
        optional_params: Vec<(usize, BcLocal, IdentId)>,
        block: BlockInfo,
    ) -> Result<Functions> {
        let outer_locals = self.get_locals();
        let (mother, _, outer) = self.mother;
        let func_id = self.add_block(
            (mother, outer + 1),
            (self.id, outer_locals),
            optional_params,
            block,
        );
        Ok(func_id)
    }
}

impl BytecodeGen {
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
                        }
                    }
                    self.level_down(n, level);
                });
                n2.iter_mut().for_each(|n| self.level_down(n, level));
            }
            NodeKind::Lambda(BlockInfo { params, body, .. }) => {
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
            | NodeKind::Redo => {}
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
            | NodeKind::Range {
                start: box n1,
                end: box n2,
                ..
            }
            | NodeKind::While {
                cond: box n1,
                body: box n2,
                ..
            }
            | NodeKind::AliasMethod(box n1, box n2) => {
                self.level_down(n1, level);
                self.level_down(n2, level);
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
            NodeKind::Hash(pairs, ..) => pairs.iter_mut().for_each(|(n1, n2)| {
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
            NodeKind::For { param, iter, body } => {
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
