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
                true => Some(self.next_reg().into()),
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
                let end = self.push_expr(end)?;
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
                self.push_expr(end)?
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
            self.emit_condbr(dst, loop_exit, true, true);
            self.pop(); // pop *dst*

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
                UseMode::Use
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
            true => Some(self.next_reg().into()),
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

        if use_value {
            self.push_nil();
        }
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
            true => Some(self.next_reg().into()),
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
            let ret = self.push().into();
            let rhs = self.next_reg().into();
            self.gen_expr(cond, UseMode::Use)?;
            for branch in when_ {
                let CaseBranch { box body, mut when } = branch;
                let succ_pos = self.new_label();
                if when.len() == 1 {
                    let when = when.remove(0);
                    self.gen_teq_condbr(when, rhs, succ_pos, false)?;
                } else {
                    let then_pos = self.new_label();
                    for when in when {
                        self.gen_teq_condbr(when, rhs, then_pos, true)?;
                    }
                    self.emit_br(succ_pos);
                    self.apply_label(then_pos);
                }
                self.gen_store_expr(ret, body)?;

                if use_mode.is_ret() {
                    self.emit(BcIr::Ret(ret), Loc::default());
                } else {
                    self.emit_br(exit_pos);
                }

                self.apply_label(succ_pos);
            }
            self.pop();
            self.gen_store_expr(ret, else_)?;
            match use_mode {
                UseMode::Ret => {
                    self.emit_ret(None);
                }
                UseMode::NotUse => {
                    self.pop();
                }
                UseMode::Use => {}
            }
        } else {
            let temp = self.temp;
            for branch in when_ {
                self.temp = temp;
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
        let ensure_label = self.new_label();

        let body_use = if else_.is_some() {
            UseMode::NotUse
        } else if ensure.is_some() && use_mode.is_ret() {
            UseMode::Use
        } else {
            use_mode
        };
        let rescue_use = if ensure.is_some() && use_mode.is_ret() {
            UseMode::Use
        } else {
            use_mode
        };
        let body_start = self.new_label();
        let body_end = self.new_label();
        self.apply_label(body_start);
        self.gen_expr(body, body_use)?;
        self.apply_label(body_end);
        let else_label = self.new_label();
        if !rescue.is_empty() {
            if !body_use.is_ret() {
                self.emit_br(else_label);
            }
            let rescue_start = self.new_label();
            self.apply_label(rescue_start);
            //assert_eq!(1, rescue.len());
            let err_reg = self.push().into();
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
                    self.gen_assign(err_reg, lhs, loc);
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
                self.gen_expr(body, rescue_use)?;
                if !rescue_use.is_ret() {
                    self.emit_br(ensure_label);
                }
                self.apply_label(next_pos);
                self.temp = old;
            }
            // no rescue branch was matched.
            // TODO:we must raise.
            if rescue_use.is_ret() {
                self.emit_nil(err_reg);
                self.emit_ret(Some(err_reg));
            } else {
                self.emit_br(ensure_label);
            }
            self.pop();
            self.exception_table.push(ExceptionEntry {
                range: body_start..body_end,
                dest: rescue_start,
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
        }
        self.apply_label(else_label);
        if let Some(box else_) = else_ {
            self.gen_expr(else_, rescue_use)?;
        }
        self.apply_label(ensure_label);
        if let Some(box ensure) = ensure {
            self.gen_expr(ensure, UseMode::NotUse)?;
            if use_mode.is_ret() {
                self.emit_ret(None);
            }
        }
        Ok(())
    }
}
