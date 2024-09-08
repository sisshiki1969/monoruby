use super::*;

impl JitContext {
    pub(in crate::compiler::jitgen) fn backedge_branches(&mut self, func: &ISeqInfo) {
        let branch_map = std::mem::take(&mut self.branch_map);
        for (bb_pos, entries) in branch_map.into_iter() {
            let BackedgeInfo {
                mut target_ctx,
                unused,
            } = self.backedge_map.remove(&bb_pos).unwrap();
            let pc = func.get_bb_pc(bb_pos);
            target_ctx.remove_unused(&unused);
            for BranchEntry {
                src_idx: _src_idx,
                mut bb,
                label,
                ..
            } in entries
            {
                #[cfg(feature = "jit-debug")]
                eprintln!("  backedge_write_back {_src_idx}->{:?}", bb_pos);
                let mut ir = AsmIr::new();
                ir.remove_unused(&mut bb, &unused);
                ir.write_back_for_target(bb, &target_ctx, pc);
                self.bridges.push((ir, label, bb_pos));
            }
        }
    }

    ///
    /// Merge incoming contexts for *bb_pos*.
    ///
    pub(in crate::compiler::jitgen) fn incoming_context(
        &mut self,
        ir: &mut AsmIr,
        func: &ISeqInfo,
        bb_pos: BasicBlockId,
    ) -> Option<BBContext> {
        let is_loop = func.get_bb_pc(bb_pos).is_loop_start();
        let res = if is_loop {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge bb(loop): {:?}", bb_pos);
            self.incoming_context_loop(ir, func, bb_pos)
        } else {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge bb: {:?}", bb_pos);
            self.incoming_context_method(func, bb_pos)
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
    ///      \  |  /               
    ///       v v v                
    ///       union                
    ///  +------------+      +------------+
    ///  |   target   |      |   new bb   |
    ///  +------------+      +-----+------+
    ///          \                 |
    ///           \--------------  |
    ///                          \ |
    ///                           v+
    ///                            |
    /// ```
    ///
    fn incoming_context_loop(
        &mut self,
        ir: &mut AsmIr,
        func: &ISeqInfo,
        bb_pos: BasicBlockId,
    ) -> Option<BBContext> {
        let entries = self.branch_map.remove(&bb_pos)?;
        let pc = func.get_bb_pc(bb_pos);

        let (use_set, unused) = self.analyse(func, bb_pos);

        #[cfg(feature = "jit-debug")]
        {
            eprintln!("  use set:  {:?}", use_set);
            eprintln!("  not used: {:?}", unused);
        }

        let target = BBContext::union(&entries);

        let mut bb = BBContext::new(self);
        for (slot, coerced) in use_set {
            match target.slot(slot) {
                LinkMode::Stack => {}
                LinkMode::Literal(v) => {
                    if v.is_float() {
                        ir.store_new_xmm(&mut bb, slot);
                    }
                }
                LinkMode::Xmm(r) if !coerced => {
                    ir.store_xmm(&mut bb, slot, r);
                }
                LinkMode::Both(r) | LinkMode::Xmm(r) => {
                    ir.store_both(&mut bb, slot, r, Guarded::Value);
                }
                LinkMode::R15 | LinkMode::Alias(_) => unreachable!(),
            };
        }
        #[cfg(feature = "jit-debug")]
        eprintln!("  target_ctx:[{:?}]   {:?}", bb.sp, bb.slot_state);

        self.write_back_branches(&MergeContext::new(&bb), entries, bb_pos, pc + 1, &unused);

        self.new_backedge(func, &mut bb, bb_pos, unused);

        Some(bb)
    }

    fn incoming_context_method(
        &mut self,
        func: &ISeqInfo,
        bb_pos: BasicBlockId,
    ) -> Option<BBContext> {
        let mut entries = self.branch_map.remove(&bb_pos)?;
        let pc = func.get_bb_pc(bb_pos);

        if entries.len() == 1 {
            let entry = entries.remove(0);
            assert!(self.continuation_bridge.is_none());
            self.continuation_bridge = Some((None, entry.label));
            return Some(entry.bb);
        }

        let target_ctx = BBContext::union(&entries);

        self.write_back_branches(&target_ctx, entries, bb_pos, pc, &[]);

        Some(target_ctx.get())
    }

    fn write_back_branches(
        &mut self,
        target_bb: &MergeContext,
        entries: Vec<BranchEntry>,
        bb_pos: BasicBlockId,
        pc: BytecodePtr,
        unused: &[SlotId],
    ) {
        let mut target_ctx = target_bb.clone();
        target_ctx.remove_unused(unused);
        for BranchEntry {
            src_idx: _src_idx,
            mut bb,
            label,
            cont,
        } in entries
        {
            #[cfg(feature = "jit-debug")]
            eprintln!("  ***write_back {_src_idx}->{:?}", bb_pos);
            if cont {
                assert!(self.continuation_bridge.is_none());
                self.continuation_bridge = Some((
                    Some(ContinuationInfo::new(bb, target_ctx.clone(), pc)),
                    label,
                ));
            } else {
                let mut ir = AsmIr::new();
                ir.remove_unused(&mut bb, unused);
                ir.write_back_for_target(bb, &target_ctx, pc);
                self.bridges.push((ir, label, bb_pos));
            }
            #[cfg(feature = "jit-debug")]
            eprintln!("  ***write_back end");
        }
    }
}

impl AsmIr {
    fn remove_unused(&mut self, bb: &mut BBContext, unused: &[SlotId]) {
        for r in unused {
            self.unlink(bb, *r);
        }
    }

    pub(in crate::compiler::jitgen) fn write_back_for_target(
        &mut self,
        mut bb: BBContext,
        target: &MergeContext,
        pc: BytecodePtr,
    ) {
        #[cfg(feature = "jit-debug")]
        {
            eprintln!("    src:    {:?}", bb.slot_state);
            eprintln!("    target: {:?}", target);
        }
        let len = bb.sp.0 as usize;

        for i in 0..len {
            let slot = SlotId(i as u16);
            let guarded = target.guarded(slot);
            if target.slot(slot) == LinkMode::Stack {
                self.write_back_with_guarded(&mut bb, slot, guarded);
            };
        }

        for i in 0..len {
            let slot = SlotId(i as u16);
            let guarded = target.guarded(slot);
            match (bb.slot(slot), target.slot(slot)) {
                (LinkMode::Xmm(l), LinkMode::Xmm(r)) => {
                    if l == r {
                    } else if bb.is_xmm_vacant(r) {
                        self.store_xmm(&mut bb, slot, r);
                        self.xmm_move(l, r);
                    } else {
                        self.xmm_swap(&mut bb, l, r);
                    }
                }
                (LinkMode::Both(l), LinkMode::Xmm(r)) => {
                    let deopt = self.new_deopt(&bb, pc + 1);
                    self.stack2reg(slot, GP::Rax);
                    self.guard_float(GP::Rax, deopt);
                    if l == r {
                        // Both(l) -> Xmm(l)
                        bb.set_xmm(slot, l);
                    } else if bb.is_xmm_vacant(r) {
                        // Xmm(l) -> Xmm(r)
                        self.store_xmm(&mut bb, slot, r);
                        self.xmm_move(l, r);
                    } else {
                        self.xmm_swap(&mut bb, l, r);
                    }
                }
                (LinkMode::Stack, LinkMode::Stack) => {}
                (LinkMode::Xmm(l), LinkMode::Both(r)) => {
                    self.xmm2stack(l, vec![slot]);
                    if l == r {
                        bb.set_both_float(slot, l);
                    } else if bb.is_xmm_vacant(r) {
                        self.store_both(&mut bb, slot, r, guarded);
                        self.xmm_move(l, r);
                    } else {
                        self.xmm_swap(&mut bb, l, r);
                    }
                }
                (LinkMode::Both(l), LinkMode::Both(r)) => {
                    if l == r {
                    } else if bb.is_xmm_vacant(r) {
                        self.store_both(&mut bb, slot, r, guarded);
                        self.xmm_move(l, r);
                    } else {
                        self.xmm_swap(&mut bb, l, r);
                    }
                }
                (LinkMode::Stack, LinkMode::Both(r)) => {
                    let deopt = self.new_deopt(&bb, pc + 1);
                    self.stack2reg(slot, GP::Rax);
                    self.inst.push(AsmInst::NumToXmm(GP::Rax, r, deopt));
                    self.store_both(&mut bb, slot, r, guarded);
                    //conv_list.push((slot, r));
                }
                (LinkMode::Literal(l), LinkMode::Literal(r)) if l == r => {}
                (LinkMode::Literal(l), LinkMode::Xmm(r)) => {
                    if let Some(f) = l.try_float() {
                        self.store_xmm(&mut bb, slot, r);
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
