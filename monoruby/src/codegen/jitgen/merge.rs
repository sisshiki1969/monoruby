use super::*;

impl JitContext {
    ///
    /// Generate bridge AsmIr for backedge branches.
    ///
    pub(super) fn backedge_branches(&mut self, iseq: &ISeqInfo) {
        let branch_map = std::mem::take(&mut self.branch_map);
        for (bbid, entries) in branch_map.into_iter() {
            let mut target_ctx = self.backedge_map.remove(&bbid).unwrap();
            let unused = self.loop_info(bbid).1;
            let pc = iseq.get_bb_pc(bbid);
            target_ctx.remove_unused(&unused);
            for BranchEntry {
                src_bb,
                mut bbctx,
                mode: cont,
                ..
            } in entries
            {
                let mut ir = AsmIr::new();
                bbctx.remove_unused(&unused);
                #[cfg(feature = "jit-debug")]
                {
                    eprintln!("  backedge_write_back {src_bb:?}->{bbid:?}");
                    eprintln!("    src:    {:?}", bbctx.slot_state);
                    eprintln!("    target: {:?}", target_ctx);
                }
                bbctx.gen_bridge_for_target(&mut ir, &target_ctx, pc);
                match cont {
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
    pub(super) fn incoming_context(
        &mut self,
        iseq: &ISeqInfo,
        bbid: BasicBlockId,
    ) -> Option<BBContext> {
        let is_loop = iseq.get_bb_pc(bbid).is_loop_start();
        let res = if is_loop {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge bb(loop): {:?}", bbid);
            self.incoming_context_loop(iseq, bbid)
        } else {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge bb: {:?}", bbid);
            self.incoming_context_method(iseq, bbid)
        };

        #[cfg(feature = "jit-debug")]
        eprintln!("===merge_end");
        res
    }

    ///
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
    ///
    fn incoming_context_loop(&mut self, iseq: &ISeqInfo, bbid: BasicBlockId) -> Option<BBContext> {
        let entries = self.branch_map.remove(&bbid)?;

        let (use_set, unused, backedge) = self.loop_info(bbid);

        #[cfg(feature = "jit-debug")]
        {
            eprintln!("  use set:  {:?}", use_set);
            eprintln!("  not used: {:?}", unused);
        }

        let mut target = BBContext::join_entries(&entries);
        if let Some(backedge) = backedge {
            target.join(&backedge);
        }

        let bbctx = BBContext::from_target(&target, &use_set);
        let target = bbctx.slot_state.clone();
        #[cfg(feature = "jit-debug")]
        eprintln!("  target_ctx:[{:?}]   {:?}", bbctx.sp, bbctx.slot_state);

        let pc = iseq.get_bb_pc(bbid);
        self.gen_bridges_for_branches(&target, entries, bbid, pc + 1, &unused);

        self.new_backedge(target, bbid);

        Some(bbctx)
    }

    fn incoming_context_method(
        &mut self,
        iseq: &ISeqInfo,
        bbid: BasicBlockId,
    ) -> Option<BBContext> {
        let entries = self.branch_map.remove(&bbid)?;

        let target_ctx = BBContext::join_entries(&entries);

        let pc = iseq.get_bb_pc(bbid);
        self.gen_bridges_for_branches(&target_ctx, entries, bbid, pc, &[]);

        Some(target_ctx)
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
        let mut target_ctx = target.clone();
        target_ctx.remove_unused(unused);
        for BranchEntry {
            src_bb,
            mut bbctx,
            mode,
            ..
        } in entries
        {
            #[cfg(feature = "jit-debug")]
            eprintln!("  bridge {mode:?} {src_bb:?}->{bbid:?}");
            let mut ir = AsmIr::new();
            bbctx.remove_unused(unused);
            #[cfg(feature = "jit-debug")]
            {
                eprintln!("    src:    {:?}", bbctx.slot_state);
                eprintln!("    target: {:?}", target_ctx);
            }
            bbctx.gen_bridge_for_target(&mut ir, &target_ctx, pc);
            match mode {
                BranchMode::Side { dest } => self.outline_bridges.push((ir, dest, bbid)),
                BranchMode::Branch => {
                    self.inline_bridges.insert(src_bb, (ir, Some(bbid)));
                }
                BranchMode::Continue => {
                    self.inline_bridges.insert(src_bb, (ir, None));
                }
            }
            #[cfg(feature = "jit-debug")]
            eprintln!("  bridge end");
        }
    }
}

impl BBContext {
    ///
    /// Generate bridge AsmIr to merge current state(*bbctx*) with target state(*target*)
    ///
    fn gen_bridge_for_target(mut self, ir: &mut AsmIr, target: &SlotContext, pc: BytecodePtr) {
        let len = self.sp.0 as usize;

        for i in 0..len {
            let slot = SlotId(i as u16);
            self.gen_bridge(ir, target, slot, pc);
        }
    }
}
