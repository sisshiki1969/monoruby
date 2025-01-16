use super::*;

impl JitContext {
    ///
    /// Generate bridge AsmIr for backedge branches.
    ///
    pub(super) fn backedge_branches(&mut self, func: &ISeqInfo) {
        let branch_map = std::mem::take(&mut self.branch_map);
        for (bbid, entries) in branch_map.into_iter() {
            let BackedgeInfo {
                mut target_ctx,
                unused,
            } = self.backedge_map.remove(&bbid).unwrap();
            let pc = func.get_bb_pc(bbid);
            target_ctx.remove_unused(&unused);
            for BranchEntry {
                src_idx: _src_idx,
                mut bbctx,
                branch_dest,
                ..
            } in entries
            {
                #[cfg(feature = "jit-debug")]
                eprintln!("  backedge_write_back {_src_idx}->{:?}", bbid);
                let mut ir = AsmIr::new();
                bbctx.remove_unused(&unused);
                bbctx.gen_bridge_for_target(&mut ir, &target_ctx, pc);
                self.bridges.push((ir, branch_dest, bbid));
            }
        }
    }

    ///
    /// Merge incoming contexts for *bbid*.
    ///
    pub(super) fn incoming_context(
        &mut self,
        func: &ISeqInfo,
        bbid: BasicBlockId,
    ) -> Option<BBContext> {
        let is_loop = func.get_bb_pc(bbid).is_loop_start();
        let res = if is_loop {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge bb(loop): {:?}", bbid);
            self.incoming_context_loop(func, bbid)
        } else {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge bb: {:?}", bbid);
            self.incoming_context_method(func, bbid)
        };

        #[cfg(feature = "jit-debug")]
        eprintln!("===merge_end");
        res
    }

    pub(super) fn gen_continuation(&mut self, ir: &mut AsmIr) {
        if let Some((data, entry)) = std::mem::take(&mut self.continuation_bridge) {
            ir.push(AsmInst::Label(entry));
            if let Some(ContinuationInfo { from, to, pc }) = data {
                from.gen_bridge_for_target(ir, &to, pc);
            }
        }
    }

    ///
    ///
    /// ```text
    ///                    
    ///      entries       
    ///                    
    ///     \   |   /              
    ///      \  |  /               
    ///       v v v                
    ///  +------------+      +------------+
    ///  |   target   |      |   bbctx    |
    ///  +------------+      +-----+------+
    ///          \                 |
    ///           \--------------  |
    ///                          \ |
    ///                           v+
    ///                            |
    ///                           bbid
    /// ```
    ///
    fn incoming_context_loop(&mut self, func: &ISeqInfo, bbid: BasicBlockId) -> Option<BBContext> {
        let entries = self.branch_map.remove(&bbid)?;

        let (use_set, unused) = self.loop_info(bbid);

        #[cfg(feature = "jit-debug")]
        {
            eprintln!("  use set:  {:?}", use_set);
            eprintln!("  not used: {:?}", unused);
        }

        let target = BBContext::union(&entries);

        let mut bbctx = BBContext::new(self);
        for (slot, coerced) in use_set {
            match target.mode(slot) {
                LinkMode::Stack => {}
                LinkMode::ConcreteValue(v) => {
                    if v.is_float() {
                        bbctx.def_new_xmm(slot);
                    }
                }
                LinkMode::Xmm(r) if !coerced => {
                    bbctx.def_xmm(slot, r);
                }
                LinkMode::Both(r) | LinkMode::Xmm(r) => {
                    bbctx.def_both(slot, r, Guarded::Value);
                }
                LinkMode::Accumulator => unreachable!(),
            };
        }
        #[cfg(feature = "jit-debug")]
        eprintln!("  target_ctx:[{:?}]   {:?}", bbctx.sp, bbctx.slot_state);

        let pc = func.get_bb_pc(bbid);
        self.gen_bridges_for_branches(&MergeContext::new(&bbctx), entries, bbid, pc + 1, &unused);

        self.new_backedge(func, &mut bbctx, bbid, unused);

        Some(bbctx)
    }

    fn incoming_context_method(
        &mut self,
        func: &ISeqInfo,
        bbid: BasicBlockId,
    ) -> Option<BBContext> {
        let mut entries = self.branch_map.remove(&bbid)?;

        if entries.len() == 1 {
            let entry = entries.remove(0);
            assert!(self.continuation_bridge.is_none());
            self.continuation_bridge = Some((None, entry.branch_dest));
            return Some(entry.bbctx);
        }

        let target_ctx = BBContext::union(&entries);

        let pc = func.get_bb_pc(bbid);
        self.gen_bridges_for_branches(&target_ctx, entries, bbid, pc, &[]);

        Some(target_ctx.get())
    }

    ///
    /// Generate bridge AsmIr for branches(*entries*) flowing into the basic block(*bbid*).
    ///
    fn gen_bridges_for_branches(
        &mut self,
        target_bb: &MergeContext,
        entries: Vec<BranchEntry>,
        bbid: BasicBlockId,
        pc: BytecodePtr,
        unused: &[SlotId],
    ) {
        let mut target_ctx = target_bb.clone();
        target_ctx.remove_unused(unused);
        for BranchEntry {
            src_idx: _src_idx,
            mut bbctx,
            branch_dest,
            cont,
        } in entries
        {
            #[cfg(feature = "jit-debug")]
            eprintln!("  ***write_back {_src_idx}->{:?}", bbid);
            if cont {
                //  the destination is adjacent to the source basic block on the bytecode.
                assert!(self.continuation_bridge.is_none());
                self.continuation_bridge = Some((
                    Some(ContinuationInfo::new(bbctx, target_ctx.clone(), pc)),
                    branch_dest,
                ));
            } else {
                let mut ir = AsmIr::new();
                bbctx.remove_unused(unused);
                bbctx.gen_bridge_for_target(&mut ir, &target_ctx, pc);
                self.bridges.push((ir, branch_dest, bbid));
            }
            #[cfg(feature = "jit-debug")]
            eprintln!("  ***write_back end");
        }
    }
}

impl BBContext {
    ///
    /// Clear slots that are not to be used.
    ///
    fn remove_unused(&mut self, unused: &[SlotId]) {
        for r in unused {
            self.discard(*r);
        }
    }

    ///
    /// Generate bridge AsmIr to merge current state(*bbctx*) with target state(*target*)
    ///
    fn gen_bridge_for_target(mut self, ir: &mut AsmIr, target: &MergeContext, pc: BytecodePtr) {
        #[cfg(feature = "jit-debug")]
        {
            eprintln!("    src:    {:?}", self.slot_state);
            eprintln!("    target: {:?}", target);
        }
        let len = self.sp.0 as usize;

        for i in 0..len {
            let slot = SlotId(i as u16);
            if target.mode(slot) == LinkMode::Stack {
                self.write_back_slot(ir, slot);
            };
        }

        for i in 0..len {
            let slot = SlotId(i as u16);
            self.gen_bridge(ir, target, slot, pc);
        }
    }
}
