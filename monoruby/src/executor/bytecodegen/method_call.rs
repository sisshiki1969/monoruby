use super::*;

impl BytecodeGen {
    pub(super) fn gen_method_call(
        &mut self,
        method: IdentId,
        receiver: Option<Node>,
        arglist: ArgList,
        ret: Option<BcReg>,
        use_mode: UseMode,
        loc: Loc,
    ) -> Result<()> {
        let recv_kind = match receiver {
            Some(receiver) => {
                if receiver.kind == NodeKind::SelfValue {
                    RecvKind::SelfValue
                } else if self.is_refer_block_arg(&receiver) {
                    self.push_expr(receiver)?;
                    RecvKind::Temp
                } else if let Some(local) = self.is_refer_local(&receiver) {
                    RecvKind::Local(local.into())
                } else {
                    self.push_expr(receiver)?;
                    RecvKind::Temp
                }
            }
            None => RecvKind::SelfValue,
        };

        if arglist.delegate {
            return Err(MonorubyErr::unsupported_feature(
                "argument delegation is not supported.",
                loc,
                self.sourceinfo.clone(),
            ));
        }
        let has_splat = arglist.splat;
        let with_block = arglist.block.is_some();

        let (callid, args, len) = self.handle_arguments(arglist, method, loc)?;

        let recv = match recv_kind {
            RecvKind::SelfValue => BcReg::Self_,
            RecvKind::Local(reg) => reg,
            RecvKind::Temp => self.pop().into(),
        };
        let ret = if ret.is_some() {
            ret
        } else if use_mode.use_val() {
            Some(self.push().into())
        } else {
            None
        };
        self.emit_call(recv, callid, ret, args, len, with_block, has_splat, loc);
        if use_mode.is_ret() {
            self.emit_ret(None);
        }
        Ok(())
    }

    pub(super) fn gen_super(
        &mut self,
        arglist: Option<ArgList>,
        ret: Option<BcReg>,
        use_mode: UseMode,
        loc: Loc,
    ) -> Result<()> {
        let (callid, args, len) = if let Some(arglist) = arglist {
            assert!(!arglist.delegate);
            self.handle_arguments(arglist, None, loc)?
        } else {
            let (mother_id, mother_args) = self.mother.as_ref().unwrap();
            assert_eq!(self.id, *mother_id);
            let arg_num = mother_args.pos_num;
            let args = BcLocal(0).into();
            let kw_list = &mother_args.keyword_names;
            let kw = if kw_list.len() == 0 {
                None
            } else {
                let mut kw_args = HashMap::default();
                let kw_pos = BcLocal(mother_args.pos_num as u16).into();
                for (id, name) in kw_list.iter().enumerate() {
                    kw_args.insert(*name, id);
                }
                Some(KeywordArgs {
                    kw_pos,
                    kw_args,
                    hash_splat_pos: vec![],
                })
            };
            let callid = self.add_callsite(None, arg_num, kw, vec![]);
            (callid, args, arg_num)
        };
        let ret = if ret.is_some() {
            ret
        } else if use_mode.use_val() {
            Some(self.push().into())
        } else {
            None
        };
        self.emit_super(callid, ret, args, len, loc);
        if use_mode.is_ret() {
            self.emit_ret(None);
        }
        Ok(())
    }

    pub(super) fn gen_each(
        &mut self,
        param: Vec<(usize, String)>,
        iter: Node,
        mut block: BlockInfo,
        ret: Option<BcReg>,
        use_mode: UseMode,
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
        let arg = self.next_reg();
        self.handle_block(optional_params, block)?;
        self.push_nil();
        self.temp = old_temp;

        let recv = match recv_kind {
            RecvKind::SelfValue => BcReg::Self_,
            RecvKind::Local(reg) => reg,
            RecvKind::Temp => self.pop().into(),
        };
        let ret = if ret.is_some() {
            ret
        } else if use_mode.use_val() {
            Some(self.push().into())
        } else {
            None
        };
        let callid = self.add_callsite(IdentId::EACH, 0, None, vec![]);
        self.emit_call(recv, callid, ret, arg.into(), 0, true, false, loc);
        if use_mode.is_ret() {
            self.emit_ret(None);
        }
        Ok(())
    }

    pub(super) fn gen_yield(
        &mut self,
        arglist: ArgList,
        ret: Option<BcReg>,
        is_ret: bool,
        loc: Loc,
    ) -> Result<()> {
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

        let (callid, args, len) =
            self.handle_arguments(arglist, IdentId::get_id("<block>"), loc)?;

        self.emit(
            BcIr::Yield {
                ret,
                args,
                len,
                callid,
            },
            loc,
        );

        if is_ret {
            self.emit_ret(None);
        }
        Ok(())
    }

    pub(super) fn gen_method_assign(
        &mut self,
        callid: CallSiteId,
        receiver: BcReg,
        val: BcReg,
        loc: Loc,
    ) {
        self.emit_call(receiver, callid, None, val, 1, false, false, loc);
    }

    fn handle_arguments(
        &mut self,
        mut arglist: ArgList,
        method: impl Into<Option<IdentId>>,
        loc: Loc,
    ) -> Result<(CallSiteId, BcReg, usize)> {
        let old_temp = self.temp;
        let (args, arg_len, splat_pos) = self.handle_positional_arguments(&mut arglist, loc)?;
        let kw_args_list = std::mem::take(&mut arglist.kw_args);
        let hash_splat = std::mem::take(&mut arglist.hash_splat);

        let kw = if kw_args_list.len() == 0 && hash_splat.is_empty() {
            None
        } else {
            let mut kw_args = HashMap::default();
            let kw_pos = self.next_reg().into();
            let mut hash_splat_pos = vec![];
            for (id, (name, node)) in kw_args_list.into_iter().enumerate() {
                self.push_expr(node)?;
                kw_args.insert(IdentId::get_id_from_string(name), id);
            }
            for node in hash_splat {
                hash_splat_pos.push(self.push_expr(node)?.into());
            }
            Some(KeywordArgs {
                kw_pos,
                kw_args,
                hash_splat_pos,
            })
        };

        self.temp = old_temp;
        let callid = self.add_callsite(method, arg_len, kw, splat_pos);
        Ok((callid, args, arg_len))
    }

    fn handle_positional_arguments(
        &mut self,
        arglist: &mut ArgList,
        loc: Loc,
    ) -> Result<(BcReg, usize, Vec<usize>)> {
        let with_block = arglist.block.is_some();
        let args = self.next_reg().into();
        if with_block {
            let block = std::mem::take(&mut arglist.block);
            self.handle_block_param(block, loc)?;
        } else if arglist.args.len() == 1
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
        }

        let (_, arg_len, splat_pos) = self.gen_args(std::mem::take(&mut arglist.args))?;
        Ok((args, arg_len, splat_pos))
    }

    fn handle_block_param(&mut self, block: Option<Box<Node>>, loc: Loc) -> Result<()> {
        if let Some(box block) = block {
            match block.kind {
                NodeKind::Lambda(block) => {
                    self.handle_block(vec![], block)?;
                }
                NodeKind::LocalVar(0, proc_local) => {
                    if let Some(local) = self.refer_local(&proc_local) {
                        self.emit_temp_mov(local.into());
                    } else {
                        let proc_temp = self.push().into();
                        self.emit(BcIr::BlockArgProxy(proc_temp, 0), loc);
                    }
                }
                NodeKind::LocalVar(outer, proc_local) => {
                    let proc_local = IdentId::get_id_from_string(proc_local);
                    let ret = self.push().into();
                    if let Some(src) = self.refer_dynamic_local(outer, proc_local) {
                        let src = src.into();
                        self.emit(BcIr::LoadDynVar { ret, src, outer }, loc);
                    } else {
                        assert_eq!(Some(proc_local), self.outer_block_param_name(outer));
                        self.emit(BcIr::BlockArgProxy(ret, outer), loc);
                    }
                }
                _ => {
                    return Err(MonorubyErr::unsupported_block_param(
                        &block,
                        self.sourceinfo.clone(),
                    ))
                }
            }
        } else {
            self.push_nil();
        }
        Ok(())
    }

    fn emit_call(
        &mut self,
        recv: BcReg,
        callid: CallSiteId,
        ret: Option<BcReg>,
        arg: BcReg,
        len: usize,
        with_block: bool,
        has_splat: bool,
        loc: Loc,
    ) {
        if with_block {
            self.emit(BcIr::MethodCallBlock(ret, callid, has_splat), loc)
        } else {
            self.emit(BcIr::MethodCall(ret, callid, has_splat), loc)
        };
        self.emit(BcIr::MethodArgs(recv, arg, len), loc);
    }

    fn emit_super(
        &mut self,
        callid: CallSiteId,
        ret: Option<BcReg>,
        args: BcReg,
        len: usize,
        loc: Loc,
    ) {
        self.emit(BcIr::Super(ret, callid), loc);
        self.emit(BcIr::MethodArgs(BcReg::Self_, args, len), loc);
    }

    fn handle_block(
        &mut self,
        optional_params: Vec<(usize, BcLocal, IdentId)>,
        block: BlockInfo,
    ) -> Result<()> {
        let outer_locals = self.get_locals();
        let mother = self.mother.as_ref().unwrap().0;
        let func_id = self.add_block(mother, (self.id, outer_locals), optional_params, block);
        let block_handler = ((u32::from(func_id) as i64) << 16) + 1;
        let dst = self.push().into();
        self.emit_literal(dst, Value::integer(block_handler));
        Ok(())
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
                nodes.into_iter().for_each(|n| self.level_down(n, level));
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
                    when.into_iter().for_each(|n| self.level_down(n, level));
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
                        .into_iter()
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
