use super::*;

#[derive(Clone, Copy)]
enum Counter {
    Local(BcReg),
    DynLocal(usize, BcLocal),
}

impl<'a> BytecodeGen<'a> {
    pub(super) fn gen_for(
        &mut self,
        param: Vec<(usize, String)>,
        iter: Node,
        body: BlockInfo,
        use_value: bool,
    ) -> Result<()> {
        let loc = iter.loc;
        if let NodeKind::Range {
            start: box Some(start),
            end: box Some(end),
            exclude_end,
            ..
        } = &iter.kind
            && let NodeKind::Integer(_) = start.kind
        {
            assert_eq!(1, param.len());
            let c_name = IdentId::get_id(&param[0].1);
            let c_outer = param[0].0;
            let counter = if c_outer == 0 {
                Counter::Local(self.assign_local(c_name).into())
            } else {
                Counter::DynLocal(
                    c_outer,
                    self.refer_dynamic_local(c_outer, c_name).unwrap().into(),
                )
            };

            let break_dest = self.new_label();
            let next_dest = self.new_label();
            let ret = if use_value {
                Some(self.sp().into())
            } else {
                None
            };
            let loop_start = self.new_label();
            let loop_exit = self.new_label();
            self.loop_push(break_dest, next_dest, loop_start, ret);

            // +------+
            // | iter | (when use_value)
            // +------+
            // | tmp  |
            // +------+
            // | end  |
            // +------+
            // | dst  |
            // +------+
            let iter = if use_value {
                Some(self.push().into())
            } else {
                None
            };
            let tmp = self.push_expr(start.clone())?.into();
            let end = self.push_expr(end.clone())?.into();
            if let Some(iter) = iter {
                self.emit(
                    BytecodeInst::Range {
                        ret: iter,
                        start: tmp,
                        end,
                        exclude_end: *exclude_end,
                    },
                    loc,
                );
            }

            self.apply_label(loop_start);
            self.emit(BytecodeInst::LoopStart, loc);
            let dst = self.push().into();
            self.emit(
                BytecodeInst::Cmp(
                    if *exclude_end {
                        CmpKind::Ge
                    } else {
                        CmpKind::Gt
                    },
                    Some(dst),
                    (tmp, end),
                    true,
                ),
                loc,
            );
            self.pop(); // pop *dst*
            self.emit_condbr(dst, loop_exit, true, true);

            self.store_counter(counter, tmp, loc);
            self.gen_expr(*body.body, UseMode2::NotUse)?;
            self.apply_label(next_dest);

            self.inc_reg(tmp, loc);
            self.emit_br(loop_start);

            self.apply_label(loop_exit);
            self.pop(); // pop *end*
            self.pop(); // pop *tmp*

            self.loop_pop();
            self.apply_label(break_dest);
            self.emit(BytecodeInst::LoopEnd, loc);
        } else {
            let use_mode = if use_value {
                UseMode2::Push
            } else {
                UseMode2::NotUse
            };
            self.gen_each(param, iter, body, use_mode, loc)?;
        }
        Ok(())
    }

    fn inc_reg(&mut self, tmp: BcReg, loc: Loc) {
        let inc = self.push().into();
        self.emit_integer(inc, 1);
        self.emit(BytecodeInst::BinOp(BinOpK::Add, Some(tmp), (tmp, inc)), loc);
        self.pop();
    }

    fn store_counter(&mut self, counter: Counter, tmp: BcReg, loc: Loc) {
        match counter {
            Counter::Local(counter) => {
                self.emit_mov(counter, tmp);
            }
            Counter::DynLocal(outer, dyn_local) => {
                self.emit(
                    BytecodeInst::StoreDynVar {
                        dst: dyn_local.into(),
                        outer,
                        src: tmp,
                    },
                    loc,
                );
            }
        }
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
        self.loop_push(loop_exit, loop_start, loop_start, ret);
        let loc = body.loc;
        self.apply_label(loop_start);
        self.emit(BytecodeInst::LoopStart, loc);
        self.gen_opt_condbr(!cond_op, cond, succ_pos)?;
        self.gen_expr(body, UseMode2::NotUse)?;
        self.emit_br(loop_start);
        self.apply_label(succ_pos);

        self.loop_pop();
        self.apply_label(loop_exit);
        self.emit(BytecodeInst::LoopEnd, loc);

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
        let loop_start = self.new_label();
        let break_dest = self.new_label();
        let next_dest = self.new_label();
        let ret = match use_value {
            true => Some(self.sp().into()),
            false => None,
        };
        self.loop_push(break_dest, next_dest, loop_start, ret);
        let loc = body.loc;
        self.apply_label(loop_start);
        self.emit(BytecodeInst::LoopStart, loc);
        self.gen_expr(body, UseMode2::NotUse)?;
        self.apply_label(next_dest);
        self.gen_opt_condbr(cond_op, cond, loop_start)?;

        if use_value {
            self.push_nil();
        }
        self.loop_pop();
        self.apply_label(break_dest);
        self.emit(BytecodeInst::LoopEnd, loc);

        Ok(())
    }

    fn gen_case_teq_condbr(
        &mut self,
        when: Node,
        reg: BcReg,
        cont_pos: Label,
        jmp_if_true: bool,
    ) -> Result<()> {
        if let NodeKind::Splat(box when) = when.kind {
            let loc = when.loc;
            let old = self.temp;
            let lhs = self.push_expr(when)?.into();
            self.emit(BytecodeInst::ArrayTEq { lhs, rhs: reg }, loc);
            self.temp = old;
            self.emit_condbr(lhs, cont_pos, jmp_if_true, false);
            Ok(())
        } else {
            self.gen_teq_condbr(when, reg, cont_pos, jmp_if_true)
        }
    }

    pub(super) fn gen_case(
        &mut self,
        cond: Option<Box<Node>>,
        when_: Vec<CaseBranch>,
        else_: Node,
        use_mode: UseMode2,
    ) -> Result<()> {
        fn check_opt(when_: &[CaseBranch]) -> Option<(u16, u16)> {
            let branch_len = when_.iter().fold(0, |acc, branch| acc + branch.when.len());
            if branch_len < 8 {
                return None;
            }
            let mut min = 2048;
            let mut max = 0;
            let b = when_.iter().all(|cb| {
                cb.when.iter().all(|node| {
                    if let NodeKind::Integer(i) = node.kind {
                        if (0..2048).contains(&i) {
                            min = min.min(i);
                            max = max.max(i);
                            return true;
                        }
                    }
                    false
                })
            });
            if b {
                Some((min as u16, max as u16))
            } else {
                None
            }
        }

        let else_pos = self.new_label();
        let exit_pos = self.new_label();
        let base = self.temp;
        if let Some(box cond) = cond {
            let loc = cond.loc;
            let reg = self.push_expr(cond)?.into();
            if let Some((min, max)) = check_opt(&when_) {
                let mut table = vec![];
                let mut bodies = vec![];
                let mut labels = vec![else_pos];
                for branch in when_ {
                    let CaseBranch { box body, when } = branch;
                    let then_pos = self.new_label();
                    labels.push(then_pos);
                    for when in when {
                        let idx = if let NodeKind::Integer(i) = when.kind {
                            i as u16 - min
                        } else {
                            unreachable!()
                        };
                        table.push((idx, then_pos));
                    }
                    bodies.push((then_pos, body));
                }
                self.emit(
                    BytecodeInst::OptCase {
                        reg,
                        min,
                        max,
                        table,
                        labels,
                    },
                    loc,
                );
                for (then_pos, body) in bodies {
                    self.temp = base;
                    self.apply_label(then_pos);
                    self.gen_expr(body, use_mode)?;
                    if !use_mode.is_ret() {
                        self.emit_br(exit_pos);
                    }
                }
            } else {
                for branch in when_ {
                    let CaseBranch { box body, mut when } = branch;
                    let succ_pos = self.new_label();
                    if when.len() == 1 {
                        let when = when.remove(0);
                        self.gen_case_teq_condbr(when, reg, succ_pos, false)?;
                    } else {
                        let then_pos = self.new_label();
                        for when in when {
                            self.gen_case_teq_condbr(when, reg, then_pos, true)?;
                        }
                        self.emit_br(succ_pos);
                        self.apply_label(then_pos);
                    }

                    self.temp = base;
                    self.gen_expr(body, use_mode)?;

                    if !use_mode.is_ret() {
                        self.emit_br(exit_pos);
                    }
                    self.temp = base + 1;
                    self.apply_label(succ_pos);
                }
            }
        } else {
            for branch in when_ {
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
                self.temp = base;
                self.apply_label(succ_pos);
            }
        }

        self.temp = base;
        self.apply_label(else_pos);
        self.gen_expr(else_, use_mode)?;

        self.apply_label(exit_pos);
        match use_mode {
            UseMode2::Push => assert_eq!(self.temp, base + 1),
            _ => assert_eq!(self.temp, base),
        }
        Ok(())
    }

    pub(super) fn gen_begin(
        &mut self,
        body: Node,
        rescue: Vec<RescueEntry>,
        else_: Option<Box<Node>>,
        ensure: Option<Box<Node>>,
        use_mode: UseMode2,
    ) -> Result<()> {
        self.ensure.push(ensure.as_deref().cloned());
        let base = self.temp;
        let ensure_label = self.new_label();
        let body_use = if else_.is_some() {
            // if else_ exists, rescue must also exists.
            UseMode2::NotUse
        } else if ensure.is_some() && use_mode.is_ret() {
            UseMode2::Push
        } else {
            use_mode
        };
        let rescue_use = if ensure.is_some() && use_mode.is_ret() {
            UseMode2::Push
        } else {
            use_mode
        };
        let body_start = self.new_label();
        let body_end = self.new_label();
        let range = body_start..body_end;
        self.apply_label(body_start);
        self.gen_expr(body, body_use)?;
        let finish = self.temp;
        self.apply_label(body_end);
        let else_label = self.new_label();
        if !rescue.is_empty() {
            if !body_use.is_ret() {
                self.emit_br(else_label);
            }
            self.temp = base;
            let err_reg = self.push().into();
            let rescue_pos = self.new_label();
            self.apply_label(rescue_pos);

            for RescueEntry {
                exception_list,
                assign,
                box body,
            } in rescue
            {
                if let Some(box assign) = assign {
                    let (lhs, rest) = self.eval_lvalue(&assign)?;
                    assert!(!rest);
                    let loc = assign.loc;
                    self.emit_assign(err_reg, lhs, None, loc);
                };
                let cont_pos = self.new_label();
                let next_pos = self.new_label();
                if !exception_list.is_empty() {
                    //assert_eq!(1, exception_list.len());
                    for ex in exception_list {
                        self.gen_teq_condbr(ex, err_reg, cont_pos, true)?;
                    }
                    self.emit_br(next_pos);
                };
                self.apply_label(cont_pos);
                self.temp = base;
                self.gen_expr(body, rescue_use)?;
                if !rescue_use.is_ret() {
                    self.emit_br(ensure_label);
                }
                self.temp = base + 1;
                self.apply_label(next_pos);
            }
            self.temp = finish;
            // no rescue branch was matched.
            if let Some(box ensure) = &ensure {
                self.gen_expr(ensure.clone(), UseMode2::NotUse)?;
            }
            self.emit(BytecodeInst::Raise(err_reg), Loc::default());

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
                return Err(
                    self.syntax_error("else without rescue is useless. (SyntaxError)", else_.loc)
                );
            }
            // no rescue branch.
            if let Some(box ensure) = &ensure {
                self.emit_br(else_label);
                let err_reg = self.push().into();
                let rescue_pos = self.new_label();
                self.apply_label(rescue_pos);
                self.gen_expr(ensure.clone(), UseMode2::NotUse)?;
                self.emit(BytecodeInst::Raise(err_reg), Loc::default());
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
        self.ensure.pop().unwrap();
        if let Some(box ensure) = ensure {
            self.gen_expr(ensure, UseMode2::NotUse)?;
            self.emit(BytecodeInst::EnsureEnd, Loc::default());
            if use_mode.is_ret() {
                self.emit_ret(None)?;
            }
        }
        match use_mode {
            UseMode2::Push => assert_eq!(self.temp, base + 1),
            _ => assert_eq!(self.temp, base),
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::tests::*;
    #[test]
    fn for_range_integer1() {
        run_test("a = []; for i in 0..3; a << i; end; a << i; a");
    }

    #[test]
    fn for_range_integer2() {
        run_test("a = []; for i in 0...3; a << i; end; a << i; a");
    }

    #[test]
    fn for_range_each() {
        run_test("a = []; r = 0..3; for i in r; a << i; end; a << i; a");
    }

    #[test]
    fn for_range_each2() {
        run_test("a = []; r = 0...3; for i in r; a << i; end; a << i; a");
    }

    #[test]
    fn for_range_integer_dyn() {
        run_test("a = []; i = 42; 1.times { for i in 0..3; a << i; end }; a << i; a");
    }

    #[test]
    fn for_range_modify_loop_var() {
        run_test("a = []; for i in 0..2; a << i; i = i + 10; end; [a, i]");
    }

    #[test]
    fn for_range_modify_loop_var_dyn() {
        run_test("a = []; i = 42; 1.times { for i in 0..2; a << i; i = i + 10; end }; [a, i]");
    }
}
