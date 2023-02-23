use ruruby_parse::{ParamKind, RescueEntry};

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
        let mut has_block_or_kw = false;
        let old_temp = info.temp;
        let arg = info.next_reg();
        if arglist.block.is_some() || !arglist.kw_args.is_empty() {
            has_block_or_kw = true;
            if let Some(box block) = arglist.block {
                match block.kind {
                    NodeKind::Lambda(block) => {
                        self.handle_block(ctx, info, vec![], block)?;
                    }
                    NodeKind::LocalVar(0, proc_local) => {
                        if Some(&proc_local) == info.block_param_name() {
                            let proc_temp = info.push().into();
                            self.push(BcIr::BlockArgProxy(proc_temp), loc);
                        } else {
                            let local = info.refer_local(&proc_local).into();
                            self.gen_temp_mov(info, local);
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

            if arglist.kw_args.is_empty() {
                self.gen_nil(info, None);
            } else {
                let len = arglist.kw_args.len();
                let old_reg = info.temp;
                let args = info.next_reg();
                for (name, node) in arglist.kw_args {
                    self.gen_symbol(info, None, name);
                    self.push_expr(ctx, info, node)?;
                }
                info.temp = old_reg;
                self.emit_hash(args.into(), args.into(), len, loc);
                info.push();
            }
        }
        let args = arglist.args;
        let len = args.len();
        self.gen_args(ctx, info, args)?;
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
        self.gen_call(
            recv,
            method,
            ret,
            arg.into(),
            len,
            has_block_or_kw,
            has_splat,
            loc,
        );
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
        let mut names = vec![];
        level_down(&mut names, &mut block.body, 0);
        let mut optional_params = vec![];
        for (outer, name) in param {
            let r = if outer == 0 {
                info.assign_local(&name)
            } else {
                info.refer_dynamic_local(outer, &name)
            };
            optional_params.push((outer + 1, r, name));
        }
        for name in names {
            info.assign_local(&name);
        }
        let method = IdentId::EACH;
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
        self.gen_call(recv, method, ret, arg.into(), 0, true, false, loc);
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
        assert!(arglist.kw_args.is_empty());
        assert!(arglist.hash_splat.is_empty());
        assert!(!arglist.delegate);
        assert!(arglist.block.is_none());
        let old_temp = info.temp;
        let arg = info.next_reg();
        let args = arglist.args;
        let len = args.len();
        self.gen_args(ctx, info, args)?;
        info.temp = old_temp;

        self.push(
            BcIr::Yield {
                ret,
                args: arg.into(),
                len,
            },
            loc,
        );

        if is_ret {
            self.gen_ret(info, None);
        }
        Ok(())
    }

    fn gen_call(
        &mut self,
        recv: BcReg,
        method: IdentId,
        ret: Option<BcReg>,
        arg: BcReg,
        len: usize,
        has_block_or_kw: bool,
        has_splat: bool,
        loc: Loc,
    ) {
        if has_block_or_kw {
            self.push(BcIr::MethodCallBlock(ret, method, has_splat), loc)
        } else {
            self.push(BcIr::MethodCall(ret, method, has_splat), loc)
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
        method: IdentId,
        receiver: BcReg,
        val: BcReg,
        loc: Loc,
    ) {
        self.gen_call(receiver, method, None, val, 1, false, false, loc);
    }
}

fn level_down(names: &mut Vec<String>, node: &mut Node, level: usize) {
    match &mut node.kind {
        NodeKind::LocalVar(l, _) => {
            if *l >= level {
                *l += 1;
            }
        }
        NodeKind::MulAssign(n1, n2) => {
            n1.into_iter().for_each(|n| {
                if level == 0 {
                    if let NodeKind::LocalVar(0, name) = &n.kind {
                        names.push(name.clone())
                    }
                }
                level_down(names, n, level);
            });
            n2.into_iter().for_each(|n| level_down(names, n, level));
        }
        NodeKind::Lambda(BlockInfo { params, body, .. }) => {
            level_down(names, body, level + 1);
            for p in params {
                match &mut p.kind {
                    ParamKind::Optional(_, n) => {
                        level_down(names, n, level);
                    }
                    ParamKind::Keyword(_, Some(n)) => {
                        level_down(names, n, level);
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
        | NodeKind::SingletonClassDef { .. } => {}
        NodeKind::CompStmt(nodes)
        | NodeKind::InterporatedString(nodes)
        | NodeKind::Array(nodes, ..)
        | NodeKind::RegExp(nodes, ..) => {
            nodes.into_iter().for_each(|n| level_down(names, n, level));
        }
        NodeKind::Command(n)
        | NodeKind::UnOp(_, n)
        | NodeKind::Splat(n)
        | NodeKind::Break(n)
        | NodeKind::Next(n)
        | NodeKind::Return(n)
        | NodeKind::Defined(n) => {
            level_down(names, n, level);
        }
        NodeKind::Const { parent, .. } => {
            if let Some(n) = parent {
                level_down(names, n, level);
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
            level_down(names, n1, level);
            level_down(names, n2, level);
        }
        NodeKind::If {
            cond: n1,
            then_: n2,
            else_: n3,
        } => {
            level_down(names, n1, level);
            level_down(names, n2, level);
            level_down(names, n3, level);
        }
        NodeKind::Hash(pairs, ..) => pairs.into_iter().for_each(|(n1, n2)| {
            level_down(names, n1, level);
            level_down(names, n2, level);
        }),
        NodeKind::FuncCall { arglist, .. } | NodeKind::Yield(arglist) => {
            level_down_arglist(names, arglist, level);
        }
        NodeKind::MethodCall {
            receiver, arglist, ..
        } => {
            level_down(names, receiver, level);
            level_down_arglist(names, arglist, level);
        }
        NodeKind::Index { base, index } => {
            level_down(names, base, level);
            index.into_iter().for_each(|n| level_down(names, n, level));
        }
        NodeKind::For { param, iter, body } => {
            for (outer, name) in param {
                if level == *outer {
                    names.push(name.clone());
                }
                if *outer >= level {
                    *outer += 1;
                }
            }
            level_down(names, iter, level);
            let BlockInfo { params, body, .. } = body;
            level_down(names, body, level);
            for p in params {
                match &mut p.kind {
                    ParamKind::Optional(_, n) => {
                        level_down(names, n, level);
                    }
                    ParamKind::Keyword(_, Some(n)) => {
                        level_down(names, n, level);
                    }
                    _ => {}
                }
            }
        }
        NodeKind::Case { cond, when_, else_ } => {
            if let Some(n) = cond {
                level_down(names, n, level);
            }
            level_down(names, else_, level);
            for CaseBranch { when, body } in when_ {
                when.into_iter().for_each(|n| level_down(names, n, level));
                level_down(names, body, level);
            }
        }
        NodeKind::Super(args) => {
            if let Some(arglist) = args {
                level_down_arglist(names, arglist, level);
            }
        }
        NodeKind::Begin {
            body,
            rescue,
            else_,
            ensure,
        } => {
            level_down(names, body, level);
            for RescueEntry {
                exception_list,
                assign,
                body,
            } in rescue
            {
                exception_list
                    .into_iter()
                    .for_each(|n| level_down(names, n, level));
                if let Some(n) = assign {
                    level_down(names, n, level);
                }
                level_down(names, body, level);
            }
            if let Some(n) = else_ {
                level_down(names, n, level);
            }
            if let Some(n) = ensure {
                level_down(names, n, level);
            }
        }
    }
}

fn level_down_arglist(names: &mut Vec<String>, arglist: &mut ArgList, level: usize) {
    let ArgList {
        args,
        kw_args,
        hash_splat,
        block,
        ..
    } = arglist;
    args.into_iter().for_each(|n| level_down(names, n, level));
    kw_args
        .into_iter()
        .for_each(|(_, n)| level_down(names, n, level));
    hash_splat
        .into_iter()
        .for_each(|n| level_down(names, n, level));
    if let Some(n) = block {
        level_down(names, n, level);
    }
}
