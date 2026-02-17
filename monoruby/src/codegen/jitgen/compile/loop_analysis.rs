use super::*;

impl<'a> JitContext<'a> {
    pub(in crate::codegen::jitgen) fn analyse_backedge_fixpoint(
        &mut self,
        state: AbstractState,
        loop_start: BasicBlockId,
        loop_end: BasicBlockId,
    ) -> JitResult<()> {
        for x in 0..10 {
            #[cfg(feature = "jit-debug")]
            eprintln!("########## analyse iteration[{x}]");
            let (liveness, backedge) = self.analyse_loop(loop_start, loop_end, state.clone())?;
            if let Some(backedge) = backedge {
                if let Some(be) = self.loop_backedge(loop_start)
                    && be.equiv(&backedge)
                {
                    #[cfg(feature = "jit-debug")]
                    eprintln!(
                        "fixed: {x} {:?}=={:?}",
                        (be.slot_state()),
                        backedge.slot_state()
                    );
                    break;
                } else {
                    #[cfg(feature = "jit-debug")]
                    eprintln!(
                        "analyse_loop[{x}] backedge: {loop_end:?}->{loop_start:?} {:?}",
                        backedge.slot_state()
                    );
                    self.add_loop_info(loop_start, liveness, Some(backedge));
                }
            } else {
                self.add_loop_info(loop_start, liveness, None);
                break;
            }
            if x == 9 {
                panic!("not fixed")
            }
        }
        Ok(())
    }

    fn analyse_loop(
        &self,
        loop_start: BasicBlockId,
        loop_end: BasicBlockId,
        mut state: AbstractState,
    ) -> JitResult<(Liveness, Option<AbstractState>)> {
        let pc = self.iseq().get_bb_pc(loop_start);
        let mut ctx = JitContext::loop_analysis(self, pc);
        let mut liveness = Liveness::new(ctx.total_reg_num());

        if let Some(backedge) = self.loop_backedge(loop_start) {
            state.join(backedge);
        };
        ctx.branch_continue(loop_start, state);

        for bbid in loop_start..=loop_end {
            ctx.analyse_basic_block(&mut liveness, bbid, bbid == loop_start, bbid == loop_end)?;
        }

        let mut backedge: Option<AbstractState> = None;
        if let Some(branches) = ctx.remove_branch(loop_start) {
            for BranchEntry { src_bb, state, .. } in branches {
                liveness.join(&state);
                assert!(src_bb.unwrap() >= loop_start);
                // backegde
                if let Some(backedge) = &mut backedge {
                    backedge.join(&state);
                } else {
                    backedge = Some(state);
                }
            }
        }
        if let Some(backedge) = &mut backedge {
            for i in backedge.all_regs() {
                if let LinkMode::G(_) = backedge.mode(i) {
                    let g = backedge.guarded(i);
                    backedge.set_S_with_guard(i, g);
                }
            }
        }
        #[cfg(feature = "jit-debug")]
        eprintln!(
            "analyse_end: {loop_start:?}->{loop_end:?} {}",
            backedge
                .as_ref()
                .map_or("no backedge".to_string(), |b| format!(
                    "{:?}",
                    b.slot_state()
                ))
        );

        Ok((liveness, backedge))
    }

    fn analyse_basic_block(
        &mut self,
        liveness: &mut Liveness,
        bbid: BasicBlockId,
        is_start: bool,
        is_last: bool,
    ) -> JitResult<()> {
        let mut ir = AsmIr::new(self);
        let mut state = match self.incoming_context(bbid, is_start)? {
            Some(bb) => bb,
            None => return Ok(()),
        };

        let BasicBlockInfoEntry { begin, end, .. } = self.iseq().bb_info[bbid];
        for bc_pos in begin..=end {
            state.set_next_sp(self.iseq().get_sp(bc_pos));

            match self.compile_instruction(&mut ir, &mut state, bc_pos)? {
                CompileResult::Continue => {}
                CompileResult::Branch(dest_bb) => {
                    self.new_branch(bc_pos, dest_bb, state);
                    return Ok(());
                }
                CompileResult::Cease => return Ok(()),
                CompileResult::Raise
                | CompileResult::Return(_)
                | CompileResult::Break(_)
                | CompileResult::MethodReturn(_)
                | CompileResult::Recompile(_)
                | CompileResult::ExitLoop => {
                    liveness.join(&state);
                    return Ok(());
                }
                CompileResult::Abort => {
                    #[cfg(feature = "emit-bc")]
                    self.dump_iseq();
                    unreachable!()
                }
            }
            state.clear_above_next_sp();
        }

        if !is_last {
            self.prepare_next(state, end);
        }

        Ok(())
    }
}
