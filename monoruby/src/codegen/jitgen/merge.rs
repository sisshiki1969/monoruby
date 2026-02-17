use super::*;

impl<'a> JitContext<'a> {
    ///
    /// Generate bridge AsmIr for backedge branches.
    ///
    pub(super) fn backedge_branches(&mut self) {
        let branch_map = self.detach_branch_map();
        for (bbid, entries) in branch_map.into_iter() {
            let target = self.remove_backedge(bbid).unwrap();
            let pc = self.iseq().get_bb_pc(bbid);
            #[cfg(feature = "jit-debug")]
            eprintln!("  backedge_bridge to:{bbid:?} target:{target:?}");
            for BranchEntry {
                src_bb,
                state,
                mode,
                ..
            } in entries
            {
                #[cfg(feature = "jit-debug")]
                eprintln!("    {mode:?} src:{src_bb:?}");

                let mut ir = AsmIr::new(self);
                state.gen_bridge(&mut ir, &target, pc);
                match mode {
                    BranchMode::Side { dest } => {
                        self.add_outline_bridge(ir, dest, bbid);
                    }
                    BranchMode::Branch => {
                        self.add_inline_bridge(src_bb, ir, Some(bbid));
                    }
                    BranchMode::Continue => unreachable!(),
                }
            }
            #[cfg(feature = "jit-debug")]
            eprintln!("  backedge_bridge end");
        }
    }

    ///
    /// Merge incoming contexts for *bbid*.
    ///
    /// ```text
    ///                    
    ///      entries       
    ///                    
    ///     \   |   /              
    ///      \  |  /  /======== backedge             
    ///       v v v  /              
    ///  +------------+      
    ///  |   target   |      
    ///  +------------+      
    ///         |
    ///         v
    ///  +------------+
    ///  |    bbid    |
    ///  +------------+
    /// ```
    pub(super) fn incoming_context(
        &mut self,
        bbid: BasicBlockId,
        no_calc_backedge: bool,
    ) -> JitResult<Option<AbstractState>> {
        let entries = if let Some(entries) = self.remove_branch(bbid) {
            entries
        } else {
            return Ok(None);
        };
        let iseq = self.iseq();
        let pc = iseq.get_bb_pc(bbid);

        let res = if let Some((loop_start, loop_end)) = iseq.bb_info.is_loop_begin(bbid) {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge loop: {bbid:?}");

            let incoming = AbstractState::join_entries(&entries);
            if !no_calc_backedge {
                self.analyse_backedge_fixpoint(incoming.clone(), loop_start, loop_end)?;
            }

            let mut target = incoming;
            if let Some((liveness, backedge)) = self.loop_info(bbid) {
                if let Some(backedge) = backedge {
                    target.join(&backedge);
                }

                target.liveness_analysis(liveness);
            }
            #[cfg(feature = "jit-debug")]
            eprintln!("  target:  {:?}\n", target.slot_state());

            self.gen_bridges_for_branches(&target, entries, bbid, pc + 1);
            self.new_backedge(target.slot_state().clone(), bbid);

            Some(target)
        } else {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge {bbid:?}");

            let target = AbstractState::join_entries(&entries);
            self.gen_bridges_for_branches(&target, entries, bbid, pc);

            Some(target)
        };

        #[cfg(feature = "jit-debug")]
        eprintln!("===merge_end");
        Ok(res)
    }

    ///
    /// Generate bridge AsmIr for branches(*entries*) flowing into the basic block(*bbid*).
    ///
    fn gen_bridges_for_branches(
        &mut self,
        target: &SlotState,
        entries: Vec<BranchEntry>,
        bbid: BasicBlockId,
        pc: BytecodePtr,
    ) {
        let target = target.clone();
        #[cfg(feature = "jit-debug")]
        eprintln!("  bridge to:{bbid:?} target:{target:?}");
        for BranchEntry {
            src_bb,
            state,
            mode,
            ..
        } in entries
        {
            #[cfg(feature = "jit-debug")]
            eprintln!("    {mode:?} src:{src_bb:?}");

            let mut ir = AsmIr::new(self);
            state.gen_bridge(&mut ir, &target, pc);
            match mode {
                BranchMode::Side { dest } => {
                    self.add_outline_bridge(ir, dest, bbid);
                }
                BranchMode::Branch => {
                    self.add_inline_bridge(src_bb, ir, Some(bbid));
                }
                BranchMode::Continue => {
                    self.add_inline_bridge(src_bb, ir, None);
                }
            }
        }
        #[cfg(feature = "jit-debug")]
        eprintln!("  bridge end");
    }
}
