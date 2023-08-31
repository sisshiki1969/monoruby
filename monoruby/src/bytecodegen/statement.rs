use super::*;

impl BytecodeGen {
    pub(super) fn gen_for(
        &mut self,
        param: Vec<(usize, String)>,
        iter: Node,
        body: BlockInfo,
        use_value: bool,
    ) -> Result<()> {
        let loc = iter.loc;
        if let NodeKind::Range {
            box start,
            box end,
            exclude_end,
            ..
        } = iter.kind
        {
            assert_eq!(1, param.len());
            let name = IdentId::get_id(&param[0].1);
            let counter = self.assign_local(name);
            let break_dest = self.new_label();
            let next_dest = self.new_label();
            let ret = match use_value {
                true => Some(self.sp().into()),
                false => None,
            };
            self.loop_push(break_dest, next_dest, ret);
            // +------+
            // | iter | (when use_value)
            // +------+
            // | end  |
            // +------+
            // | dst  |
            // +------+
            let loop_entry = self.new_label();
            let loop_exit = self.new_label();
            self.gen_store_expr(counter.into(), start)?;
            let end = if use_value {
                let iter = self.push();
                let end = self.push_expr(end)?.into();
                self.emit(
                    BcIr::Range {
                        ret: iter.into(),
                        start: counter.into(),
                        end,
                        exclude_end,
                    },
                    loc,
                );
                end
            } else {
                self.push_expr(end)?.into()
            };
            self.apply_label(loop_entry);
            self.emit(BcIr::LoopStart, loc);
            let dst = self.push().into();
            self.emit(
                BcIr::Cmp(
                    if exclude_end {
                        CmpKind::Ge
                    } else {
                        CmpKind::Gt
                    },
                    dst,
                    BinopMode::RR(counter.into(), end),
                    true,
                ),
                loc,
            );
            self.pop(); // pop *dst*
            self.emit_condbr(dst, loop_exit, true, true);

            self.gen_expr(*body.body, UseMode::NotUse)?;
            self.apply_label(next_dest);

            self.emit(
                BcIr::BinOp(
                    BinOpK::Add,
                    counter.into(),
                    BinopMode::RI(counter.into(), 1),
                ),
                loc,
            );
            self.emit_br(loop_entry);

            self.apply_label(loop_exit);
            self.pop(); // pop *end*

            self.loop_pop();
            self.apply_label(break_dest);
            self.emit(BcIr::LoopEnd, loc);
        } else {
            let use_mode = if use_value {
                UseMode::Push
            } else {
                UseMode::NotUse
            };
            self.gen_each(param, iter, body, None, use_mode, loc)?;
        }
        Ok(())
    }

    pub(super) fn gen_while(
        &mut self,
        cond_op: bool,
        cond: Node,
        body: Node,
        use_value: bool,
    ) -> Result<()> {
        let loop_start = self.new_label();
        let succ_pos = self.new_label();
        let loop_exit = self.new_label();
        let ret = match use_value {
            true => Some(self.push_nil()),
            false => None,
        };
        self.loop_push(loop_exit, loop_start, ret);
        let loc = body.loc;
        self.apply_label(loop_start);
        self.emit(BcIr::LoopStart, loc);
        self.gen_opt_condbr(!cond_op, cond, succ_pos)?;
        self.gen_expr(body, UseMode::NotUse)?;
        self.emit_br(loop_start);
        self.apply_label(succ_pos);

        self.loop_pop();
        self.apply_label(loop_exit);
        self.emit(BcIr::LoopEnd, loc);

        Ok(())
    }

    // in postfix while, we must evaluate the body expr once at least.
    pub(super) fn gen_while_begin_postfix(
        &mut self,
        cond_op: bool,
        cond: Node,
        body: Node,
        use_value: bool,
    ) -> Result<()> {
        let loop_pos = self.new_label();
        let break_dest = self.new_label();
        let next_dest = self.new_label();
        let ret = match use_value {
            true => Some(self.sp().into()),
            false => None,
        };
        self.loop_push(break_dest, next_dest, ret);
        let loc = body.loc;
        self.apply_label(loop_pos);
        self.emit(BcIr::LoopStart, loc);
        self.gen_expr(body, UseMode::NotUse)?;
        self.apply_label(next_dest);
        self.gen_opt_condbr(cond_op, cond, loop_pos)?;

        if use_value {
            self.push_nil();
        }
        self.loop_pop();
        self.apply_label(break_dest);
        self.emit(BcIr::LoopEnd, loc);

        Ok(())
    }

    pub(super) fn gen_case(
        &mut self,
        cond: Option<Box<Node>>,
        when_: Vec<CaseBranch>,
        else_: Node,
        use_mode: UseMode,
    ) -> Result<()> {
        let exit_pos = self.new_label();
        if let Some(box cond) = cond {
            let ret = self.push().into(); // +1
            let rhs = self.push_expr(cond)?.into(); // +2
            for branch in when_ {
                let CaseBranch { box body, mut when } = branch;
                let succ_pos = self.new_label();
                if when.len() == 1 {
                    let when = when.remove(0);
                    self.gen_teq_condbr(when, rhs, succ_pos, false)?; // +2
                } else {
                    let then_pos = self.new_label();
                    for when in when {
                        self.gen_teq_condbr(when, rhs, then_pos, true)?; // +2
                    }
                    self.pop();
                    self.emit_br(succ_pos); // +1
                    self.apply_label(then_pos); // +1
                    self.push();
                }
                self.gen_store_expr(ret, body)?; // +2

                if use_mode.is_ret() {
                    self.emit(BcIr::Ret(ret), Loc::default());
                } else {
                    self.pop();
                    self.emit_br(exit_pos); // +1
                    self.push();
                }
                self.apply_label(succ_pos); // +1
            }
            self.pop(); // +1
            self.gen_store_expr(ret, else_)?; // +1
            match use_mode {
                UseMode::Ret => {
                    self.emit_ret(None); // +1
                }
                UseMode::NotUse => {
                    self.pop(); // 0
                }
                UseMode::Push => {} // +1
            }
        } else {
            let temp = self.temp;
            for branch in when_ {
                self.temp = temp; // 0
                let CaseBranch { box body, mut when } = branch;
                let succ_pos = self.new_label();
                if when.len() == 1 {
                    let when = when.remove(0);
                    self.gen_opt_condbr(false, when, succ_pos)?;
                } else {
                    let then_pos = self.new_label();
                    for when in when {
                        self.gen_opt_condbr(true, when, then_pos)?;
                    }
                    self.emit_br(succ_pos);
                    self.apply_label(then_pos);
                }
                self.gen_expr(body, use_mode)?;
                if !use_mode.is_ret() {
                    self.emit_br(exit_pos);
                }
                self.apply_label(succ_pos);
            }
            self.temp = temp;
            self.gen_expr(else_, use_mode)?;
        }

        self.apply_label(exit_pos);
        Ok(())
    }

    pub(super) fn gen_begin(
        &mut self,
        body: Node,
        rescue: Vec<RescueEntry>,
        else_: Option<Box<Node>>,
        ensure: Option<Box<Node>>,
        use_mode: UseMode,
    ) -> Result<()> {
        let base = self.temp;
        let ret_reg = self.sp().into();
        let ensure_label = self.new_label();
        let body_use = if else_.is_some() {
            // if else_ exists, rescue must also exists.
            UseMode::NotUse
        } else if ensure.is_some() && use_mode.is_ret() {
            UseMode::Push
        } else {
            use_mode
        };
        let rescue_use = if ensure.is_some() && use_mode.is_ret() {
            UseMode::Push
        } else {
            use_mode
        };
        let body_start = self.new_label();
        let body_end = self.new_label();
        let range = body_start..body_end;
        self.apply_label(body_start);
        self.gen_expr(body, body_use)?;
        self.apply_label(body_end);
        let else_label = self.new_label();
        if !rescue.is_empty() {
            if !body_use.is_ret() {
                self.emit_br(else_label);
            }
            let err_reg = self.push().into();
            let rescue_pos = self.new_label();
            self.apply_label(rescue_pos);
            //assert_eq!(1, rescue.len());
            let old = self.temp;
            for RescueEntry {
                exception_list,
                assign,
                box body,
            } in rescue
            {
                if let Some(box assign) = assign {
                    let lhs = self.eval_lvalue(&assign)?;
                    let loc = assign.loc;
                    self.emit_assign(err_reg, lhs, loc);
                };
                let cont_pos = self.new_label();
                let next_pos = self.new_label();
                if !exception_list.is_empty() {
                    assert_eq!(1, exception_list.len());
                    for ex in exception_list {
                        self.gen_teq_condbr(ex, err_reg, cont_pos, true)?;
                    }
                    self.emit_br(next_pos);
                };
                self.apply_label(cont_pos);
                match rescue_use {
                    UseMode::Push => self.gen_store_expr(ret_reg, body)?,
                    _ => self.gen_expr(body, rescue_use)?,
                }
                if !rescue_use.is_ret() {
                    self.emit_br(ensure_label);
                }
                self.apply_label(next_pos);
                self.temp = old;
            }
            // no rescue branch was matched.
            if let Some(box ensure) = &ensure {
                self.gen_expr(ensure.clone(), UseMode::NotUse)?;
            }
            self.emit(BcIr::Raise(err_reg), Loc::default());
            self.pop();

            self.exception_table.push(ExceptionEntry {
                range,
                rescue: Some(rescue_pos),
                ensure: if ensure.is_some() {
                    Some(ensure_label)
                } else {
                    None
                },
                err_reg: Some(err_reg),
            });
        } else {
            if let Some(else_) = else_ {
                return Err(MonorubyErr::syntax(
                    "else without rescue is useless. (SyntaxError)".to_string(),
                    else_.loc,
                    self.sourceinfo.clone(),
                ));
            }
            // no rescue branch.
            if let Some(box ensure) = &ensure {
                self.emit_br(else_label);
                let err_reg = self.push().into();
                let rescue_pos = self.new_label();
                self.apply_label(rescue_pos);
                self.gen_expr(ensure.clone(), UseMode::NotUse)?;
                self.emit(BcIr::Raise(err_reg), Loc::default());
                self.pop();

                self.exception_table.push(ExceptionEntry {
                    range,
                    rescue: Some(rescue_pos),
                    ensure: Some(ensure_label),
                    err_reg: Some(err_reg),
                });
            } else {
                self.exception_table.push(ExceptionEntry {
                    range,
                    rescue: None,
                    ensure: None,
                    err_reg: None,
                });
            }
        }
        self.apply_label(else_label);
        if let Some(box else_) = else_ {
            self.gen_expr(else_, rescue_use)?;
        }
        self.apply_label(ensure_label);
        if let Some(box ensure) = ensure {
            self.gen_expr(ensure, UseMode::NotUse)?;
            self.emit(BcIr::EnsureEnd, Loc::default());
            if use_mode.is_ret() {
                self.emit_ret(None);
            }
        }
        match use_mode {
            UseMode::Push => assert_eq!(self.temp, base + 1),
            _ => assert_eq!(self.temp, base),
        }

        Ok(())
    }
}
