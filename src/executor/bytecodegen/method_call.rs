use super::*;

impl IrContext {
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
                } else if let Some(local) = self.is_refer_local(&receiver) {
                    RecvKind::Local(local.into())
                } else {
                    self.push_expr(receiver)?;
                    RecvKind::Temp
                }
            }
            None => RecvKind::SelfValue,
        };

        assert!(arglist.hash_splat.is_empty());
        assert!(!arglist.delegate);
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
        if let Some(arglist) = arglist {
            assert!(arglist.hash_splat.is_empty());
            assert!(!arglist.delegate);
            //let has_splat = arglist.splat;
            //let with_block = arglist.block.is_some();
            let (callid, args, len) = self.handle_arguments(arglist, None, loc)?;

            let ret = if ret.is_some() {
                ret
            } else if use_mode.use_val() {
                Some(self.push().into())
            } else {
                None
            };
            self.emit_super(callid, ret, args, len, loc);
        } else {
            unimplemented!()
        };
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
        assert_eq!(1, param.len());
        // collect assignments for local variables.
        let mut optional_params = vec![];
        for (outer, name) in param {
            let r = if outer == 0 {
                self.assign_local(&name)
            } else {
                self.refer_dynamic_local(outer, &name)
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
        self.emit_nil(None);
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
        assert!(arglist.hash_splat.is_empty());
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
            self.handle_arguments(arglist, IdentId::get_ident_id("<block>"), loc)?;

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

    fn handle_arguments(
        &mut self,
        mut arglist: ArgList,
        method: impl Into<Option<IdentId>>,
        loc: Loc,
    ) -> Result<(CallSiteId, BcReg, usize)> {
        let old_temp = self.temp;
        let kw_args_list = std::mem::take(&mut arglist.kw_args);
        let (args, arg_len, splat_pos) = self.handle_positional_arguments(arglist, loc)?;

        let kw = if kw_args_list.len() == 0 {
            None
        } else {
            let mut kw_args = HashMap::default();
            let kw_pos = self.next_reg().into();
            for (id, (name, node)) in kw_args_list.into_iter().enumerate() {
                self.push_expr(node)?;
                kw_args.insert(IdentId::get_ident_id_from_string(name), id);
            }
            Some(KeywordArgs { kw_pos, kw_args })
        };

        self.temp = old_temp;
        let callid = self.add_callsite(method, arg_len, kw, splat_pos);
        Ok((callid, args, arg_len))
    }

    fn handle_positional_arguments(
        &mut self,
        arglist: ArgList,
        loc: Loc,
    ) -> Result<(BcReg, usize, Vec<usize>)> {
        let with_block = arglist.block.is_some();
        let args = self.next_reg().into();
        if with_block {
            self.handle_block_param(arglist.block, loc)?;
        } else if arglist.args.len() == 1 {
            if let NodeKind::LocalVar(0, ident) = &arglist.args[0].kind {
                // in the case of "f(a)"
                let local = self.refer_local(ident).into();
                return Ok((local, 1, vec![]));
            } else if let NodeKind::Splat(box node) = &arglist.args[0].kind {
                // in the case of "f(*a)"
                if let NodeKind::LocalVar(0, ident) = &node.kind {
                    let local = self.refer_local(ident).into();
                    return Ok((local, 1, vec![0]));
                }
            }
        }

        let (_, arg_len, splat_pos) = self.gen_args(arglist.args)?;
        Ok((args, arg_len, splat_pos))
    }

    fn handle_block_param(&mut self, block: Option<Box<Node>>, loc: Loc) -> Result<()> {
        if let Some(box block) = block {
            match block.kind {
                NodeKind::Lambda(block) => {
                    self.handle_block(vec![], block)?;
                }
                NodeKind::LocalVar(0, proc_local) => {
                    if self.block_param.is_some() {
                        let proc_temp = self.push().into();
                        self.emit(BcIr::BlockArgProxy(proc_temp, 0), loc);
                    } else {
                        let local = self.refer_local(&proc_local).into();
                        self.emit_temp_mov(local);
                    }
                }
                NodeKind::LocalVar(outer, proc_local) => {
                    if Some(&proc_local) == self.outer_block_param_name(outer) {
                        let proc_temp = self.push().into();
                        self.emit(BcIr::BlockArgProxy(proc_temp, outer), loc);
                    } else {
                        let src = self.refer_dynamic_local(outer, &proc_local).into();
                        let ret = self.push().into();
                        self.emit(BcIr::LoadDynVar { ret, src, outer }, loc);
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
            self.emit_nil(None);
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
        optional_params: Vec<(usize, BcLocal, String)>,
        block: BlockInfo,
    ) -> Result<()> {
        let outer_locals = self.get_locals();
        let func_id = self.add_block(
            self.mother.unwrap(),
            (self.id, outer_locals),
            optional_params,
            block,
        );
        let block_handler = ((u32::from(func_id) as i64) << 16) + 1;
        self.emit_literal(None, Value::new_integer(block_handler));
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
}
