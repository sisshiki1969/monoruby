use super::*;

impl JitContext {
    ///
    /// Generate bridge AsmIr for backedge branches.
    ///
    pub(super) fn backedge_branches(&mut self, iseq: &ISeqInfo) {
        let branch_map = std::mem::take(&mut self.branch_map);
        for (bbid, entries) in branch_map.into_iter() {
            let target = self.backedge_map.remove(&bbid).unwrap();
            let unused = self.loop_info(bbid).1;
            let pc = iseq.get_bb_pc(bbid);
            for BranchEntry {
                src_bb,
                bbctx,
                mode,
                ..
            } in entries
            {
                let mut ir = AsmIr::new();
                #[cfg(feature = "jit-debug")]
                eprintln!("  backedge_write_back {src_bb:?}->{bbid:?}");
                bbctx.gen_bridge(&mut ir, &target, pc, &unused);
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
        iseq: &ISeqInfo,
        bbid: BasicBlockId,
    ) -> Option<BBContext> {
        let entries = self.branch_map.remove(&bbid)?;
        let pc = iseq.get_bb_pc(bbid);

        let is_loop = iseq.get_bb_pc(bbid).is_loop_start();
        let res = if is_loop {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge bb(loop): {:?}", bbid);

            let (use_set, unused, backedge) = self.loop_info(bbid);

            #[cfg(feature = "jit-debug")]
            {
                eprintln!("  use set:  {:?}", use_set);
                eprintln!("  not used: {:?}", unused);
                eprintln!("  backedge: {:?}", backedge);
            }

            let target = BBContext::join_entries(&entries, backedge).use_float(&use_set);
            #[cfg(feature = "jit-debug")]
            eprintln!("  target:[{:?}]   {:?}", target.sp, target.slot_state);

            self.gen_bridges_for_branches(&target, entries, bbid, pc + 1, &unused);
            self.new_backedge(target.slot_state.clone(), bbid);

            Some(target)
        } else {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge bb: {:?}", bbid);

            let target = BBContext::join_entries(&entries, None);
            self.gen_bridges_for_branches(&target, entries, bbid, pc, &[]);

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
        unused: &[SlotId],
    ) {
        let target = target.clone();
        for BranchEntry {
            src_bb,
            bbctx,
            mode,
            ..
        } in entries
        {
            let mut ir = AsmIr::new();
            #[cfg(feature = "jit-debug")]
            eprintln!("  bridge {mode:?} {src_bb:?}->{bbid:?}");
            bbctx.gen_bridge(&mut ir, &target, pc, unused);
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
    }
}
