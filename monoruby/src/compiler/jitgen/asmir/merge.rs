use super::*;

impl JitContext {
    ///
    /// Generate bridge AsmIr for backedge branches.
    ///
    pub(in crate::compiler::jitgen) fn backedge_branches(&mut self, func: &ISeqInfo) {
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
                ir.remove_unused(&mut bbctx, &unused);
                ir.gen_bridge_for_target(bbctx, &target_ctx, pc);
                self.bridges.push((ir, branch_dest, bbid));
            }
        }
    }

    ///
    /// Merge incoming contexts for *bbid*.
    ///
    pub(in crate::compiler::jitgen) fn incoming_context(
        &mut self,
        ir: &mut AsmIr,
        func: &ISeqInfo,
        bbid: BasicBlockId,
    ) -> Option<BBContext> {
        let is_loop = func.get_bb_pc(bbid).is_loop_start();
        let res = if is_loop {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge bb(loop): {:?}", bbid);
            self.incoming_context_loop(ir, func, bbid)
        } else {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge bb: {:?}", bbid);
            self.incoming_context_method(func, bbid)
        };

        #[cfg(feature = "jit-debug")]
        eprintln!("===merge_end");
        res
    }

    pub(in crate::compiler::jitgen) fn gen_continuation(&mut self, ir: &mut AsmIr) {
        if let Some((data, entry)) = std::mem::take(&mut self.continuation_bridge) {
            ir.inst.push(AsmInst::Label(entry));
            if let Some(ContinuationInfo(from, to, pc)) = data {
                ir.gen_bridge_for_target(from, &to, pc);
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
    fn incoming_context_loop(
        &mut self,
        ir: &mut AsmIr,
        func: &ISeqInfo,
        bbid: BasicBlockId,
    ) -> Option<BBContext> {
        let entries = self.branch_map.remove(&bbid)?;

        let (use_set, unused) = self.analyse(func, bbid);

        #[cfg(feature = "jit-debug")]
        {
            eprintln!("  use set:  {:?}", use_set);
            eprintln!("  not used: {:?}", unused);
        }

        let target = BBContext::union(&entries);

        let mut bbctx = BBContext::new(self);
        for (slot, coerced) in use_set {
            match target.slot(slot) {
                LinkMode::Stack => {}
                LinkMode::Literal(v) => {
                    if v.is_float() {
                        ir.store_new_xmm(&mut bbctx, slot);
                    }
                }
                LinkMode::Xmm(r) if !coerced => {
                    ir.store_xmm(&mut bbctx, slot, r);
                }
                LinkMode::Both(r) | LinkMode::Xmm(r) => {
                    ir.store_both(&mut bbctx, slot, r, Guarded::Value);
                }
                LinkMode::R15 | LinkMode::Alias(_) => unreachable!(),
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
                ir.remove_unused(&mut bbctx, unused);
                ir.gen_bridge_for_target(bbctx, &target_ctx, pc);
                self.bridges.push((ir, branch_dest, bbid));
            }
            #[cfg(feature = "jit-debug")]
            eprintln!("  ***write_back end");
        }
    }
}

impl AsmIr {
    ///
    /// Clear slots that are not to be used.
    ///
    fn remove_unused(&mut self, bb: &mut BBContext, unused: &[SlotId]) {
        for r in unused {
            self.unlink(bb, *r);
        }
    }

    ///
    /// Generate bridge AsmIr to merge current state(*bbctx*) with target state(*target*)
    ///
    fn gen_bridge_for_target(
        &mut self,
        mut bbctx: BBContext,
        target: &MergeContext,
        pc: BytecodePtr,
    ) {
        #[cfg(feature = "jit-debug")]
        {
            eprintln!("    src:    {:?}", bbctx.slot_state);
            eprintln!("    target: {:?}", target);
        }
        let len = bbctx.sp.0 as usize;

        for i in 0..len {
            let slot = SlotId(i as u16);
            let guarded = target.guarded(slot);
            if target.slot(slot) == LinkMode::Stack {
                self.write_back_with_guarded(&mut bbctx, slot, guarded);
            };
        }

        for i in 0..len {
            let slot = SlotId(i as u16);
            let guarded = target.guarded(slot);
            match (bbctx.slot(slot), target.slot(slot)) {
                (LinkMode::Xmm(l), LinkMode::Xmm(r)) => {
                    if l == r {
                    } else if bbctx.is_xmm_vacant(r) {
                        self.store_xmm(&mut bbctx, slot, r);
                        self.xmm_move(l, r);
                    } else {
                        self.xmm_swap(&mut bbctx, l, r);
                    }
                }
                (LinkMode::Both(l), LinkMode::Xmm(r)) => {
                    let deopt = self.new_deopt(&bbctx, pc + 1);
                    self.stack2reg(slot, GP::Rax);
                    self.guard_float(GP::Rax, deopt);
                    if l == r {
                        // Both(l) -> Xmm(l)
                        bbctx.set_xmm(slot, l);
                    } else if bbctx.is_xmm_vacant(r) {
                        // Xmm(l) -> Xmm(r)
                        self.store_xmm(&mut bbctx, slot, r);
                        self.xmm_move(l, r);
                    } else {
                        self.xmm_swap(&mut bbctx, l, r);
                    }
                }
                (LinkMode::Stack, LinkMode::Stack) => {}
                (LinkMode::Xmm(l), LinkMode::Both(r)) => {
                    self.xmm2stack(l, vec![slot]);
                    if l == r {
                        bbctx.set_both_float(slot, l);
                    } else if bbctx.is_xmm_vacant(r) {
                        self.store_both(&mut bbctx, slot, r, guarded);
                        self.xmm_move(l, r);
                    } else {
                        self.xmm_swap(&mut bbctx, l, r);
                    }
                }
                (LinkMode::Both(l), LinkMode::Both(r)) => {
                    if l == r {
                    } else if bbctx.is_xmm_vacant(r) {
                        self.store_both(&mut bbctx, slot, r, guarded);
                        self.xmm_move(l, r);
                    } else {
                        self.xmm_swap(&mut bbctx, l, r);
                    }
                }
                (LinkMode::Stack, LinkMode::Both(r)) => {
                    let deopt = self.new_deopt(&bbctx, pc + 1);
                    self.stack2reg(slot, GP::Rax);
                    self.inst.push(AsmInst::NumToXmm(GP::Rax, r, deopt));
                    self.store_both(&mut bbctx, slot, r, guarded);
                    //conv_list.push((slot, r));
                }
                (LinkMode::Literal(l), LinkMode::Literal(r)) if l == r => {}
                (LinkMode::Literal(l), LinkMode::Xmm(r)) => {
                    if let Some(f) = l.try_float() {
                        self.store_xmm(&mut bbctx, slot, r);
                        self.f64toxmm(f, r);
                    } else {
                        unreachable!()
                    }
                }
                (l, r) => unreachable!("src:{:?} target:{:?}", l, r),
            }
        }
    }
}
