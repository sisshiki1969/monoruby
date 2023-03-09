use super::*;

impl IrContext {
    pub(super) fn gen_method_call(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        method: String,
        receiver: Option<Node>,
        arglist: ArgList,
        ret: Option<BcReg>,
        use_mode: UseMode,
        loc: Loc,
    ) -> Result<()> {
        let method = IdentId::get_ident_id_from_string(method);
        let recv_kind = match receiver {
            Some(receiver) => {
                if receiver.kind == NodeKind::SelfValue {
                    RecvKind::SelfValue
                } else if let Some(local) = info.is_refer_local(&receiver) {
                    RecvKind::Local(local.into())
                } else {
                    self.push_expr(ctx, info, receiver)?;
                    RecvKind::Temp
                }
            }
            None => RecvKind::SelfValue,
        };

        assert!(arglist.hash_splat.is_empty());
        assert!(!arglist.delegate);
        let has_splat = arglist.splat;
        let with_block = arglist.block.is_some();
        let arg = info.next_reg().into();
        let len = arglist.args.len();

        let callid = self.handle_arguments(ctx, info, arglist, method, loc)?;

        let recv = match recv_kind {
            RecvKind::SelfValue => BcReg::Self_,
            RecvKind::Local(reg) => reg,
            RecvKind::Temp => info.pop().into(),
        };
        let ret = if ret.is_some() {
            ret
        } else if use_mode.use_val() {
            Some(info.push().into())
        } else {
            None
        };
        self.gen_call(recv, callid, ret, arg, len, with_block, has_splat, loc);
        if use_mode.is_ret() {
            self.gen_ret(info, None);
        }
        Ok(())
    }

    pub(super) fn gen_each(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
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
                info.assign_local(&name)
            } else {
                info.refer_dynamic_local(outer, &name)
            };
            optional_params.push((outer + 1, r, name));
        }
        info.level_down(&mut block.body, 0);
        let recv_kind = if iter.kind == NodeKind::SelfValue {
            RecvKind::SelfValue
        } else if let Some(local) = info.is_refer_local(&iter) {
            RecvKind::Local(local.into())
        } else {
            self.push_expr(ctx, info, iter)?;
            RecvKind::Temp
        };

        let old_temp = info.temp;
        let arg = info.next_reg();
        self.handle_block(ctx, info, optional_params, block)?;
        self.gen_nil(info, None);
        info.temp = old_temp;

        let recv = match recv_kind {
            RecvKind::SelfValue => BcReg::Self_,
            RecvKind::Local(reg) => reg,
            RecvKind::Temp => info.pop().into(),
        };
        let ret = if ret.is_some() {
            ret
        } else if use_mode.use_val() {
            Some(info.push().into())
        } else {
            None
        };
        let callid = ctx.add_callsite(IdentId::EACH, 0, HashMap::default(), 0, vec![]);
        self.gen_call(recv, callid, ret, arg.into(), 0, true, false, loc);
        if use_mode.is_ret() {
            self.gen_ret(info, None);
        }
        Ok(())
    }

    pub(super) fn gen_yield(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        arglist: ArgList,
        ret: Option<BcReg>,
        is_ret: bool,
        loc: Loc,
    ) -> Result<()> {
        assert!(arglist.hash_splat.is_empty());
        // TODO: We must check this in parser
        if arglist.delegate {
            return Err(MonorubyErr::syntax(
                format!("Delegate argument should not be given"),
                loc,
                info.sourceinfo.clone(),
            ));
        }
        // yield does not accept block.
        if arglist.block.is_some() {
            return Err(MonorubyErr::syntax(
                format!("Block argument should not be given"),
                loc,
                info.sourceinfo.clone(),
            ));
        }

        let arg = info.next_reg();
        let len = arglist.args.len();

        let callid =
            self.handle_arguments(ctx, info, arglist, IdentId::get_ident_id("<block>"), loc)?;

        self.push(
            BcIr::Yield {
                ret,
                args: arg.into(),
                len,
                callid,
            },
            loc,
        );

        if is_ret {
            self.gen_ret(info, None);
        }
        Ok(())
    }

    fn handle_arguments(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        arglist: ArgList,
        method: IdentId,
        loc: Loc,
    ) -> Result<CallSiteId> {
        let with_block = arglist.block.is_some();
        let old_temp = info.temp;
        if with_block {
            self.handle_block_param(ctx, info, arglist.block, loc)?;
        }
        let args = arglist.args;
        let arg_num = args.len();
        let (_, splat_pos) = self.gen_args(ctx, info, args)?;

        let mut kw_args = HashMap::default();
        let kw_pos = info.next_reg().0;
        for (id, (name, node)) in arglist.kw_args.iter().enumerate() {
            self.push_expr(ctx, info, node.clone())?;
            kw_args.insert(IdentId::get_ident_id(name), id);
        }

        info.temp = old_temp;
        let callid = ctx.add_callsite(method, arg_num, kw_args, kw_pos, splat_pos);
        Ok(callid)
    }

    fn handle_block_param(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        block: Option<Box<Node>>,
        loc: Loc,
    ) -> Result<()> {
        if let Some(box block) = block {
            match block.kind {
                NodeKind::Lambda(block) => {
                    self.handle_block(ctx, info, vec![], block)?;
                }
                NodeKind::LocalVar(0, proc_local) => {
                    if Some(&proc_local) == info.block_param_name() {
                        let proc_temp = info.push().into();
                        self.push(BcIr::BlockArgProxy(proc_temp, 0), loc);
                    } else {
                        let local = info.refer_local(&proc_local).into();
                        self.gen_temp_mov(info, local);
                    }
                }
                NodeKind::LocalVar(outer, proc_local) => {
                    if Some(&proc_local) == info.outer_block_param_name(outer) {
                        let proc_temp = info.push().into();
                        self.push(BcIr::BlockArgProxy(proc_temp, outer), loc);
                    } else {
                        let src = info.refer_dynamic_local(outer, &proc_local).into();
                        let ret = info.push().into();
                        self.push(BcIr::LoadDynVar { ret, src, outer }, loc);
                    }
                }
                _ => {
                    return Err(MonorubyErr::unsupported_block_param(
                        &block,
                        info.sourceinfo.clone(),
                    ))
                }
            }
        } else {
            self.gen_nil(info, None);
        }
        Ok(())
    }

    fn gen_call(
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
            self.push(BcIr::MethodCallBlock(ret, callid, has_splat), loc)
        } else {
            self.push(BcIr::MethodCall(ret, callid, has_splat), loc)
        };
        self.push(BcIr::MethodArgs(recv, arg, len), loc);
    }

    fn handle_block(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        optional_params: Vec<(usize, BcLocal, String)>,
        block: BlockInfo,
    ) -> Result<()> {
        let outer_locals = info.get_locals();
        let func_id = ctx.add_block(
            (info.id(), outer_locals),
            optional_params,
            block,
            info.sourceinfo.clone(),
        )?;
        let block_handler = ((u32::from(func_id) as i64) << 16) + 1;
        self.gen_literal(info, None, Value::new_integer(block_handler));
        Ok(())
    }

    pub(super) fn gen_method_assign(
        &mut self,
        callid: CallSiteId,
        receiver: BcReg,
        val: BcReg,
        loc: Loc,
    ) {
        self.gen_call(receiver, callid, None, val, 1, false, false, loc);
    }
}
