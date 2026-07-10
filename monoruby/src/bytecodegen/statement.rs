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
        let body_start = self.new_label();
        let succ_pos = self.new_label();
        let loop_exit = self.new_label();
        let ret = match use_value {
            true => Some(self.push_nil()),
            false => None,
        };
        // `next` re-checks the condition (→ `loop_start`); `redo` restarts the
        // body without re-evaluating it (→ `body_start`, after the condition).
        // `redo` is emitted as a `Redo` op — a `goto` routed through the error
        // path that the JIT treats as a bail — not a plain back-edge branch, so
        // the JIT never sees a second back-edge into the loop body.
        self.loop_push(loop_exit, loop_start, body_start, ret);
        let loc = body.loc;
        self.apply_label(loop_start);
        self.emit(BytecodeInst::LoopStart, loc);
        self.gen_opt_condbr(!cond_op, cond, succ_pos)?;
        self.apply_label(body_start);
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
            self.emit(
                BytecodeInst::ArrayTEq {
                    lhs,
                    rhs: reg,
                    rescue_clause: false,
                },
                loc,
            );
            self.temp = old;
            self.emit_condbr(lhs, cont_pos, jmp_if_true, false);
            Ok(())
        } else {
            self.gen_teq_condbr(when, reg, cont_pos, jmp_if_true)
        }
    }

    /// Emit the condition test for one `when` value of a *subjectless*
    /// `case` (`case; when cond; ...`), where each `when` value is tested
    /// for plain truthiness rather than `===`.
    fn gen_case_cond_condbr(
        &mut self,
        when: Node,
        cont_pos: Label,
        jmp_if_true: bool,
    ) -> Result<()> {
        if when.is_splat() {
            // `when *arr` with no subject matches when any element of `arr`
            // is truthy. `when` is the `Splat` node itself, so wrapping it in
            // an array literal expands the splat (mirroring CRuby's
            // `splatarray`); `ArrayAny` then tests truthiness of any element
            // (CRuby's `checkmatch` with `VM_CHECKMATCH_TYPE_WHEN`), without
            // any user-visible method call.
            let loc = when.loc;
            let old = self.temp;
            let arr: BcReg = self.push().into();
            self.gen_array(arr, vec![when], loc)?;
            self.emit(BytecodeInst::ArrayAny { reg: arr }, loc);
            self.temp = old;
            self.emit_condbr(arr, cont_pos, jmp_if_true, false);
            Ok(())
        } else {
            self.gen_opt_condbr(jmp_if_true, when, cont_pos)
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
                    self.gen_case_cond_condbr(when, succ_pos, false)?;
                } else {
                    let then_pos = self.new_label();
                    for when in when {
                        self.gen_case_cond_condbr(when, then_pos, true)?;
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
        // When this begin has rescue clauses, capture the `$!` active on
        // entry into a hidden local. CRuby restores `$!` to its previous
        // value when a rescue clause completes normally — for a nested
        // rescue that is the *outer* exception, not nil — so a matched
        // clause restores from this slot instead of clearing to nil. An
        // anonymous local (not a temp) survives the whole begin/rescue/
        // ensure region without being reused as scratch.
        let errinfo_save: Option<BcReg> = if !rescue.is_empty() {
            let reg: BcReg = self.add_local(None).into();
            self.emit(
                BytecodeInst::LoadGvar {
                    dst: reg,
                    name: IdentId::get_id("$!"),
                },
                Loc::default(),
            );
            Some(reg)
        } else {
            None
        };
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
            // The balanced stack level while matching a rescue clause: only
            // `err_reg` is live. A clause's `=> target` capture may leave
            // scratch temps above this (e.g. `rescue => self[:error]` reserves
            // base/index/src), so the match code below resets to it.
            let match_temp = self.temp;
            let rescue_pos = self.new_label();
            self.apply_label(rescue_pos);
            self.retry_labels.push(body_start);

            for RescueEntry {
                exception_list,
                assign,
                box body,
            } in rescue
            {
                if let Some(box assign) = assign {
                    let loc = assign.loc;
                    // `rescue => recv&.attr`: assign the caught exception via a
                    // safe-navigation setter — skip the store when `recv` is nil.
                    if matches!(&assign.kind,
                        NodeKind::MethodCall { arglist, safe_nav: true, .. }
                            if arglist.args.is_empty()
                                && arglist.block.is_none()
                                && arglist.kw_args.is_empty())
                    {
                        let NodeKind::MethodCall {
                            box receiver,
                            method,
                            ..
                        } = assign.kind
                        else {
                            unreachable!()
                        };
                        let setter = IdentId::get_id_from_string(format!("{method}="));
                        let old = self.temp;
                        let recv = self.push_expr(receiver)?.into();
                        let skip = self.new_label();
                        self.emit_nilbr(recv, skip, old);
                        let arg = self.push().into();
                        self.emit_mov(arg, err_reg);
                        let callsite = CallSite::simple(setter, 1, arg, recv, None);
                        self.emit_method_assign(callsite, loc);
                        self.temp = old;
                        self.apply_label(skip);
                    } else {
                        let (lhs, rest) = self.eval_lvalue(&assign)?;
                        assert!(!rest);
                        self.emit_assign(err_reg, lhs, None, loc);
                    }
                };
                // Reclaim any scratch temps the capture target reserved so the
                // exception-match branches below all record the same stack sp.
                self.temp = match_temp;
                let cont_pos = self.new_label();
                let next_pos = self.new_label();
                if !exception_list.is_empty() {
                    for ex in exception_list {
                        if ex.is_splat() {
                            // `rescue *list` / `rescue Foo, *list`: expand the
                            // operand into an Array (splatarray semantics —
                            // `#to_a` on a non-Array), then match `err` against
                            // any element via `ArrayTEq` (the same primitive
                            // `case ... when *arr` uses). Wrapping the splat
                            // node in a one-element Array literal performs the
                            // expansion.
                            let loc = ex.loc;
                            let old = self.temp;
                            let ary = self.push_expr(Node::new_array(vec![ex], loc))?.into();
                            self.emit(
                                BytecodeInst::ArrayTEq {
                                    lhs: ary,
                                    rhs: err_reg,
                                    rescue_clause: true,
                                },
                                loc,
                            );
                            self.temp = old;
                            self.emit_condbr(ary, cont_pos, true, false);
                        } else {
                            self.gen_rescue_teq_condbr(ex, err_reg, cont_pos, true)?;
                        }
                    }
                    self.emit_br(next_pos);
                } else {
                    // A bare `rescue` (no exception list, with or without a
                    // `=> e` capture) is `rescue StandardError`: only
                    // StandardError and its subclasses are caught, so a
                    // non-StandardError (Exception, SignalException,
                    // SystemExit, …) propagates instead of being swallowed.
                    let std_err = Node::new_const(
                        "StandardError".to_string(),
                        false,
                        None,
                        vec![],
                        Loc::default(),
                    );
                    self.gen_teq_condbr(std_err, err_reg, cont_pos, true)?;
                    self.emit_br(next_pos);
                };
                self.apply_label(cont_pos);
                self.temp = base;
                self.rescue_depth += 1;
                self.gen_expr(body, rescue_use)?;
                self.rescue_depth -= 1;
                if !rescue_use.is_ret() {
                    // Restore `$!` to the value active before this begin when
                    // the rescue clause completes normally. CRuby restores the
                    // previous exception (nil at the top level, the outer
                    // exception when nested), rather than always clearing to nil.
                    self.emit(
                        BytecodeInst::StoreGvar {
                            val: errinfo_save.expect("rescue clause without errinfo save slot"),
                            name: IdentId::get_id("$!"),
                        },
                        Loc::default(),
                    );
                    self.emit_br(ensure_label);
                }
                self.temp = base + 1;
                self.apply_label(next_pos);
            }
            self.retry_labels.pop();
            self.temp = finish;
            // `no_match_pos` is reached both by fall-through (no rescue clause
            // matched) and — when there is an `ensure` — as the exception
            // handler for the rescue-matching / rescue-body region below: on a
            // raise there the runtime stores the new exception into `err_reg`,
            // so running the `ensure` and re-raising `err_reg` propagates it
            // (the `ensure` body's own `raise`/`return`/`throw` still overrides
            // via `defer_unwind` / `EnsureEnd`).
            let no_match_pos = self.new_label();
            self.apply_label(no_match_pos);
            // no rescue branch was matched.
            if let Some(box ensure) = &ensure {
                // Keep `err_reg` reserved while emitting the `ensure` body: the
                // re-raise below reads the in-flight exception back out of
                // `err_reg`, so the body's scratch temps must not reuse that
                // slot. `match_temp` sits one slot above `err_reg` (only
                // `err_reg` is live here), so generating the body from there
                // leaves the exception intact. (Without this the body would
                // start at `finish`, which can alias `err_reg` and cause a
                // non-exception value — e.g. a `puts` string — to be re-raised.)
                let saved = self.temp;
                self.temp = match_temp;
                self.gen_expr(ensure.clone(), UseMode2::NotUse)?;
                self.temp = saved;
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
            // An exception (or `return` / `throw`) raised while matching a
            // rescue clause or running a matched clause's body must still run
            // this frame's `ensure`. The begin-body exception entry above only
            // covers `range` (the body), so cover the rescue region too — but
            // only when there is an `ensure` to run (otherwise such an
            // exception simply propagates, which is already correct).
            if ensure.is_some() {
                self.exception_table.push(ExceptionEntry {
                    range: rescue_pos..no_match_pos,
                    rescue: Some(no_match_pos),
                    ensure: Some(ensure_label),
                    err_reg: Some(err_reg),
                });
            }
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
    fn for_range() {
        run_tests(&[
            "a = []; for i in 0..3; a << i; end; a << i; a",
            "a = []; for i in 0...3; a << i; end; a << i; a",
            "a = []; r = 0..3; for i in r; a << i; end; a << i; a",
            "a = []; r = 0...3; for i in r; a << i; end; a << i; a",
            "a = []; i = 42; 1.times { for i in 0..3; a << i; end }; a << i; a",
            "a = []; for i in 0..2; a << i; i = i + 10; end; [a, i]",
            "a = []; i = 42; 1.times { for i in 0..2; a << i; i = i + 10; end }; [a, i]",
        ]);
    }

    #[test]
    fn ensure_runs_when_rescue_clause_exits() {
        // An `ensure` must run — and its own `return` / `raise` must override
        // — when a rescue clause re-raises, raises a new exception, returns,
        // or throws. The rescue region (not just the begin body) is covered
        // by the ensure handler.
        run_test(
            r#"
            def m
              raise "orig"
            rescue
              raise "from rescue"
            ensure
              return "from ensure"
            end
            m
            "#,
        );
        run_test(
            r#"
            def m
              raise "orig"
            rescue
              raise "from rescue"
            ensure
              raise "from ensure"
            end
            begin; m; rescue => e; e.message; end
            "#,
        );
        // The ensure runs (side effect) even when the rescue body raises, and
        // a normally-completing ensure lets the rescue's exception propagate.
        run_test(
            r#"
            $log = []
            def m
              raise "orig"
            rescue
              raise "reraised"
            ensure
              $log << :ensure_ran
            end
            msg = (begin; m; rescue => e; e.message; end)
            [$log, msg]
            "#,
        );
        // return / throw / retry through rescue + ensure still behave.
        run_test(
            r#"
            $log = []
            def m
              raise "x"
            rescue
              return :from_rescue
            ensure
              $log << :ens
            end
            [m, $log]
            "#,
        );
        run_test(
            r#"
            $log = []
            r = catch(:t) do
              begin
                raise "x"
              rescue
                throw :t, :thrown
              ensure
                $log << :ens
              end
            end
            [r, $log]
            "#,
        );
        run_test(
            r#"
            $log = []
            def m
              n = 0
              begin
                n += 1
                raise "x" if n < 3
                n
              rescue
                retry if n < 3
              ensure
                $log << n
              end
            end
            [m, $log]
            "#,
        );
    }

    #[test]
    fn reraise_through_ensure_preserves_exception() {
        // Regression: `err_reg` (the slot the exception dispatcher stashes the
        // in-flight exception in, re-raised after the `ensure`) must not be
        // reused as a scratch temp by the `ensure` body. When it was, an
        // `ensure` that built a string (e.g. via `puts`/interpolation) left a
        // String in `err_reg`, so the re-raise handed a non-exception value to
        // the runtime and aborted the process. Here the outer `rescue` must
        // receive the original exception object, not the ensure's string.
        run_test(
            r#"
            o = StandardError.new "outer"
            i = StandardError.new "inner"
            begin
              raise o
            rescue
              begin
                begin
                  raise i
                rescue
                  raise i
                ensure
                  s = "ensure: #{i.message}"
                end
              rescue => e
                [e.class.name, e.message, e.equal?(i)]
              end
            end
            "#,
        );
        // Same shape without an outer pending exception, re-raising the bare
        // `$!` through an `ensure` whose body allocates scratch temps.
        run_test(
            r#"
            begin
              begin
                raise ArgumentError, "boom"
              rescue
                raise
              ensure
                _ = "x" * 4
              end
            rescue => e
              [e.class.name, e.message]
            end
            "#,
        );
    }

    #[test]
    fn errinfo_restored_after_rescue() {
        // CRuby restores `$!` when a rescue clause completes normally: to nil
        // at the top level, but to the *outer* exception inside a nested
        // rescue. Previously monoruby always cleared `$!` to nil.
        run_test(
            r#"
            log = []
            begin
              raise "outer"
            rescue
              begin
                raise "inner"
              rescue
                log << $!.message   # inner
              end
              log << $!.message     # outer (restored)
            end
            log << $!.inspect       # nil (restored at top level)
            log
            "#,
        );
        // Three levels deep, plus an `ensure` on the innermost.
        run_test(
            r#"
            log = []
            begin
              raise "L1"
            rescue
              begin
                raise "L2"
              rescue
                begin
                  raise "L3"
                rescue
                  log << $!.message
                ensure
                  log << $!.message
                end
                log << $!.message
              end
              log << $!.message
            end
            log << $!.inspect
            log
            "#,
        );
        // A re-raise that propagates out and is caught by the outer rescue
        // still leaves `$!` restored to nil at the top afterwards.
        run_test(
            r#"
            r = begin
              begin
                raise "a"
              rescue
                raise "b"
              end
            rescue => e
              e.message
            end
            [r, $!.inspect]
            "#,
        );
    }

    #[test]
    fn rescue_splat_exception_list() {
        // `rescue *list` matches the raised exception against every element
        // of the splatted list; `rescue Foo, *list` combines a literal entry
        // with the splat, and the operand is coerced with `#to_a`.
        run_test(
            r#"
            class E1 < StandardError; end
            class E2 < StandardError; end
            list = [E1, E2]
            r1 = begin; raise E2, "x"; rescue *list; :a; end
            r2 = begin; raise E1, "y"; rescue ArgumentError, *list; :b; end
            r3 = begin
                   begin; raise TypeError, "z"; rescue *list; :wrong; end
                 rescue TypeError
                   :propagated
                 end
            [r1, r2, r3]
            "#,
        );
        // `#to_a` coercion of a non-Array splat operand, and a single-class
        // (non-Array) operand handled as itself.
        run_test(
            r#"
            class E1 < StandardError; end
            class Custom; def to_a; [E1]; end; end
            r1 = begin; raise E1, "x"; rescue *Custom.new; :toa; end
            r2 = begin; raise E1, "y"; rescue *E1; :single; end
            [r1, r2]
            "#,
        );
    }

    #[test]
    fn redo_loop() {
        // `redo` restarts the loop body without re-evaluating the condition
        // (so a side-effecting condition like `(i += 1)` does not re-run).
        // The `run_tests` harness wraps each snippet in a hot `for` loop, so
        // these also exercise a `while`-redo nested inside a loop-JIT'd loop.
        run_tests(&[
            "a=[]; i=0; j=0; while (i+=1)<3; a<<i; j+=1; redo if j<3; end; a",
            "a=[]; i=0; j=0; until (i+=1)>=3; a<<i; j+=1; redo if j<3; end; a",
            // `next` still re-checks the condition; `break` still exits.
            "i=0; r=[]; while (i+=1)<6; next if i==2; break if i==5; r<<i; end; r",
            // nested while, `redo` on the inner loop only
            "o=[]; x=0; while (x+=1)<3; y=0; k=0; while (y+=1)<3; k+=1; o<<[x,y]; redo if k<3 && x==1; end; end; o",
            // postfix / do-while form
            "a=[]; i=0; j=0; begin; a<<i; i+=1; j+=1; redo if j<3 && i==1; end while i<3; a",
            // a method-JIT'd body: a `while`-redo nested inside a `for` loop
            // (the redo must not corrupt the enclosing loop's counter).
            "def rr; o=[]; for k in 0..3; a=[]; i=0; j=0; while (i+=1)<3; a<<i; j+=1; redo if j<3; end; o<<a; end; o; end; rr",
        ]);
    }
}
