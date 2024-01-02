use super::*;

impl JitContext {
    pub(in crate::compiler::jitgen) fn backedge_branches(&mut self, func: &ISeqInfo) {
        let branch_map = std::mem::take(&mut self.branch_map);
        for (bb_pos, entries) in branch_map.into_iter() {
            let (target_label, mut target_ctx, unused) = self.backedge_map.remove(&bb_pos).unwrap();
            let pc = func.get_pc(bb_pos);
            target_ctx.remove_unused(&unused);
            for BranchEntry {
                src_idx: _src_idx,
                mut bb,
                label,
                ..
            } in entries
            {
                #[cfg(feature = "jit-debug")]
                eprintln!("  backedge_write_back {_src_idx}->{bb_pos}");
                let mut ir = AsmIr::new();
                ir.remove_unused(&mut bb, &unused);
                ir.write_back_for_target(bb, &target_ctx, pc);
                self.bridges.push((ir, label, target_label));
            }
        }
    }

    pub(in crate::compiler::jitgen) fn incoming_context(
        &mut self,
        func: &ISeqInfo,
        bb_pos: BcIndex,
    ) -> Option<BBContext> {
        let is_loop = func.get_pc(bb_pos).is_loop_start();
        let res = if is_loop {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge bb(loop): {bb_pos}");
            self.incoming_context_loop(func, bb_pos)?
        } else {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge bb: {bb_pos}");
            self.incoming_context_method(func, bb_pos)?
        };

        #[cfg(feature = "jit-debug")]
        eprintln!("===merge_end");
        Some(res)
    }

    fn incoming_context_loop(&mut self, func: &ISeqInfo, bb_pos: BcIndex) -> Option<BBContext> {
        let entries = self.branch_map.remove(&bb_pos)?;
        let pc = func.get_pc(bb_pos);

        let (use_set, unused) = self.analyse(func, bb_pos);

        let cur_label = self.inst_labels[&bb_pos];

        #[cfg(feature = "jit-debug")]
        {
            eprintln!("  use set:  {:?}", use_set);
            eprintln!("  not used: {:?}", unused);
        }

        let template = BBContext::merge_entries(&entries);

        let mut target_ctx = BBContext::new(&self);
        let mut const_vec = vec![];
        for (reg, coerced) in use_set {
            match template[reg] {
                LinkMode::Stack | LinkMode::R15 => {}
                LinkMode::Literal(v) => {
                    if v.class() == FLOAT_CLASS {
                        const_vec.push(reg);
                    }
                }
                LinkMode::Xmm(r) if !coerced => {
                    self.ir.link_xmm(&mut target_ctx, reg, r);
                }
                LinkMode::Both(r) | LinkMode::Xmm(r) => {
                    self.ir.link_both(&mut target_ctx, reg, r);
                }
            };
        }
        for r in const_vec {
            self.ir.link_new_xmm(&mut target_ctx, r);
        }
        #[cfg(feature = "jit-debug")]
        eprintln!(
            "  target_ctx:[{:?}]   {:?}",
            target_ctx.sp, target_ctx.slot_state
        );

        self.write_back_branches(
            &MergeContext::new(&target_ctx),
            entries,
            cur_label,
            pc + 1,
            bb_pos,
            &unused,
        );

        self.new_backedge(func, &mut target_ctx, bb_pos, cur_label, unused);

        Some(target_ctx)
    }

    fn incoming_context_method(&mut self, func: &ISeqInfo, bb_pos: BcIndex) -> Option<BBContext> {
        let mut entries = self.branch_map.remove(&bb_pos)?;
        let pc = func.get_pc(bb_pos);

        if entries.len() == 1 {
            let entry = entries.remove(0);
            assert!(self.continuation_bridge.is_none());
            self.continuation_bridge = Some((None, entry.label));
            return Some(entry.bb);
        }

        let target_ctx = BBContext::merge_entries(&entries);
        let cur_label = self.inst_labels[&bb_pos];

        self.write_back_branches(&target_ctx, entries, cur_label, pc, bb_pos, &[]);

        Some(target_ctx.get())
    }

    fn write_back_branches(
        &mut self,
        target_bb: &super::slot::MergeContext,
        entries: Vec<BranchEntry>,
        cur_label: DestLabel,
        pc: BcPc,
        _bb_pos: BcIndex,
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
            eprintln!("  ***write_back {_src_idx}->{_bb_pos}");
            if cont {
                assert!(self.continuation_bridge.is_none());
                self.continuation_bridge = Some((Some((bb, target_ctx.clone(), pc)), label));
            } else {
                let mut ir = AsmIr::new();
                ir.remove_unused(&mut bb, unused);
                ir.write_back_for_target(bb, &target_ctx, pc);
                self.bridges.push((ir, label, cur_label));
            }
            #[cfg(feature = "jit-debug")]
            eprintln!("  ***write_back end");
        }
    }
}

impl AsmIr {
    fn remove_unused(&mut self, bb: &mut BBContext, unused: &[SlotId]) {
        for r in unused {
            self.link_stack(bb, *r);
        }
    }

    pub(in crate::compiler::jitgen) fn write_back_for_target(
        &mut self,
        mut bb: BBContext,
        target: &super::slot::MergeContext,
        pc: BcPc,
    ) {
        #[cfg(feature = "jit-debug")]
        {
            eprintln!("    src:    {:?}", bb.slot_state);
            eprintln!("    target: {:?}", target);
        }
        let len = bb.reg_num();

        self.writeback_acc(&mut bb);

        for i in 0..len {
            let reg = SlotId(i as u16);
            if target[reg] == LinkMode::Stack {
                match bb[reg] {
                    LinkMode::Xmm(freg) => {
                        bb.xmm_to_both(freg);
                        self.xmm2both(freg, bb[freg].clone());
                    }
                    LinkMode::Literal(v) => {
                        self.lit2stack(v, reg);
                    }
                    LinkMode::Both(_) | LinkMode::Stack => {}
                    LinkMode::R15 => unreachable!(),
                }
                self.link_stack(&mut bb, reg);
            };
        }

        let mut conv_list = vec![];
        let mut guard_list = vec![];
        for i in 0..len {
            let reg = SlotId(i as u16);
            match (bb[reg], target[reg]) {
                (LinkMode::Xmm(l), LinkMode::Xmm(r)) => {
                    if l == r {
                    } else if bb.is_xmm_vacant(r) {
                        self.link_xmm(&mut bb, reg, r);
                        self.xmm_move(l, r);
                    } else {
                        bb.xmm_swap(l, r);
                        self.xmm_swap(l, r);
                    }
                }
                (LinkMode::Both(l), LinkMode::Xmm(r)) => {
                    if l == r {
                        bb[reg] = LinkMode::Xmm(l);
                    } else if bb.is_xmm_vacant(r) {
                        self.link_xmm(&mut bb, reg, r);
                        self.xmm_move(l, r);
                    } else {
                        bb.xmm_swap(l, r);
                        self.xmm_swap(l, r);
                    }
                    guard_list.push(reg);
                }
                (LinkMode::Stack, LinkMode::Stack) => {}
                (LinkMode::Xmm(l), LinkMode::Both(r)) => {
                    self.xmm2both(l, vec![reg]);
                    if l == r {
                        bb[reg] = LinkMode::Both(l);
                    } else if bb.is_xmm_vacant(r) {
                        self.link_both(&mut bb, reg, r);
                        self.xmm_move(l, r);
                    } else {
                        bb.xmm_swap(l, r);
                        self.xmm_swap(l, r);
                    }
                }
                (LinkMode::Both(l), LinkMode::Both(r)) => {
                    if l == r {
                    } else if bb.is_xmm_vacant(r) {
                        self.link_both(&mut bb, reg, r);
                        self.xmm_move(l, r);
                    } else {
                        bb.xmm_swap(l, r);
                        self.xmm_swap(l, r);
                    }
                }
                (LinkMode::Stack, LinkMode::Both(r)) => {
                    self.link_both(&mut bb, reg, r);
                    conv_list.push((reg, r));
                }
                (LinkMode::Literal(l), LinkMode::Literal(r)) if l == r => {}
                (LinkMode::Literal(l), LinkMode::Xmm(r)) => {
                    if let Some(f) = l.try_float() {
                        self.link_xmm(&mut bb, reg, r);
                        self.f64toxmm(f, r);
                    } else {
                        unreachable!()
                    }
                }
                (l, r) => unreachable!("src:{:?} target:{:?}", l, r),
            }
        }

        let deopt = self.new_deopt(&bb, pc + 1);

        for (r, x) in conv_list {
            self.stack2reg(r, GP::Rax);
            self.inst.push(AsmInst::NumToXmm(GP::Rax, x, deopt));
        }

        for r in guard_list {
            self.stack2reg(r, GP::Rax);
            self.guard_float(GP::Rax, deopt);
        }
    }
}
