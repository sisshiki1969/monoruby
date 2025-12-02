use super::*;

impl JitContext {
    ///
    /// Generate bridge AsmIr for backedge branches.
    ///
    pub(super) fn backedge_branches(&mut self, iseq: &ISeqInfo) {
        let branch_map = self.detach_branch_map();
        for (bbid, entries) in branch_map.into_iter() {
            let target = self.remove_backedge(bbid).unwrap();
            let pc = iseq.get_bb_pc(bbid);
            #[cfg(feature = "jit-debug")]
            eprintln!("  backedge_bridge to: {bbid:?} {target:?}");
            for BranchEntry {
                src_bb,
                bbctx,
                mode,
                ..
            } in entries
            {
                #[cfg(feature = "jit-debug")]
                eprintln!("    {mode:?}");
                let mut ir = AsmIr::new();
                bbctx.gen_bridge(&mut ir, src_bb, &target, pc);
                match mode {
                    BranchMode::Side { dest } => {
                        self.outline_bridges.push((ir, dest, bbid));
                    }
                    BranchMode::Branch => {
                        self.inline_bridges.insert(src_bb, (ir, Some(bbid)));
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
        store: &Store,
        iseq: &ISeqInfo,
        bbid: BasicBlockId,
        no_calc_backedge: bool,
    ) -> Option<BBContext> {
        let entries = self.remove_branch(bbid)?;
        let pc = iseq.get_bb_pc(bbid);

        let res = if let Some((loop_start, loop_end)) = iseq.bb_info.is_loop_begin(bbid) {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge loop: {bbid:?}");

            let incoming = BBContext::join_entries(&entries);
            if !no_calc_backedge {
                self.analyse_backedge_fixpoint(store, incoming.clone(), loop_start, loop_end);
            }

            let mut target = incoming;
            if let Some((liveness, backedge)) = self.loop_info(bbid) {
                if let Some(backedge) = backedge {
                    #[cfg(feature = "jit-debug")]
                    eprintln!("  backedge : {:?}", backedge.slot_state);
                    target.join(&backedge);
                }

                target.liveness_analysis(liveness);
            }
            #[cfg(feature = "jit-debug")]
            eprintln!("  target:  {:?}", target.slot_state);

            self.gen_bridges_for_branches(&target, entries, bbid, pc + 1);
            self.new_backedge(target.slot_state.clone(), bbid);

            Some(target)
        } else {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge {bbid:?}");

            let target = BBContext::join_entries(&entries);
            self.gen_bridges_for_branches(&target, entries, bbid, pc);

            Some(target)
        };

        #[cfg(feature = "jit-debug")]
        eprintln!("===merge_end");
        res
    }

    ///
    /// Generate bridge AsmIr for branches(*entries*) flowing into the basic block(*bbid*).
    ///
    fn gen_bridges_for_branches(
        &mut self,
        target: &SlotContext,
        entries: Vec<BranchEntry>,
        bbid: BasicBlockId,
        pc: BytecodePtr,
    ) {
        let target = target.clone();
        #[cfg(feature = "jit-debug")]
        eprintln!("  bridge to: {bbid:?} {target:?}");
        for BranchEntry {
            src_bb,
            bbctx,
            mode,
            ..
        } in entries
        {
            #[cfg(feature = "jit-debug")]
            eprintln!("    {mode:?}");
            let mut ir = AsmIr::new();
            bbctx.gen_bridge(&mut ir, src_bb, &target, pc);
            match mode {
                BranchMode::Side { dest } => {
                    self.outline_bridges.push((ir, dest, bbid));
                }
                BranchMode::Branch => {
                    self.inline_bridges.insert(src_bb, (ir, Some(bbid)));
                }
                BranchMode::Continue => {
                    self.inline_bridges.insert(src_bb, (ir, None));
                }
            }
        }
        #[cfg(feature = "jit-debug")]
        eprintln!("  bridge end");
    }
}
