use super::*;

impl JitContext {
    pub(super) fn backedge_branches(&mut self, func: &ISeqInfo) {
        let branch_map = std::mem::take(&mut self.branch_map);
        for (bb_pos, entries) in branch_map.into_iter() {
            let (target_label, mut target_ctx, unused) = self.backedge_map.remove(&bb_pos).unwrap();
            let pc = func.get_pc(bb_pos);
            target_ctx.remove_unused(&unused);
            for BranchEntry {
                src_idx: _src_idx,
                mut bbctx,
                label,
                ..
            } in entries
            {
                #[cfg(feature = "jit-debug")]
                eprintln!("  backedge_write_back {_src_idx}->{bb_pos}");
                bbctx.remove_unused(&unused);
                let ir = bbctx.write_back_for_target(&target_ctx, pc);
                self.asmir.push((ir, label, Some(target_label)));
            }
        }
    }

    pub(super) fn incoming_context(
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

        let cur_label = self.labels[&bb_pos];

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
                    target_ctx.link_xmm(reg, r);
                }
                LinkMode::Both(r) | LinkMode::Xmm(r) => {
                    target_ctx.link_both(reg, r);
                }
            };
        }
        for r in const_vec {
            target_ctx.link_new_xmm(r);
        }
        #[cfg(feature = "jit-debug")]
        eprintln!(
            "  target_ctx:[{:?}]   {:?}",
            target_ctx.sp, target_ctx.slot_state
        );

        let v = target_ctx.write_back_branches(entries, cur_label, pc + 1, bb_pos, &unused);
        self.asmir.extend(v);

        self.new_backedge(func, &mut target_ctx, bb_pos, cur_label, unused);

        Some(target_ctx)
    }

    fn incoming_context_method(&mut self, func: &ISeqInfo, bb_pos: BcIndex) -> Option<BBContext> {
        let mut entries = self.branch_map.remove(&bb_pos)?;
        let pc = func.get_pc(bb_pos);

        if entries.len() == 1 {
            let entry = entries.remove(0);
            self.asmir.push((AsmIr::new(), entry.label, None));
            //self.jit.bind_label(entry.entry);
            return Some(entry.bbctx);
        }

        let target_ctx = BBContext::merge_entries(&entries);
        let cur_label = self.labels[&bb_pos];

        let v = target_ctx.write_back_branches(entries, cur_label, pc, bb_pos, &[]);
        self.asmir.extend(v);

        Some(target_ctx)
    }
}

impl BBContext {
    fn write_back_for_target(mut self, target: &BBContext, pc: BcPc) -> AsmIr {
        #[cfg(feature = "jit-debug")]
        {
            eprintln!("    src:    {:?}", self.slot_state);
            eprintln!("    target: {:?}", target.slot_state);
        }
        let mut ir = AsmIr::new();
        let len = self.reg_num();

        if let Some(slot) = self.clear_r15() {
            ir.push(AsmInst::AccToStack(slot));
        }

        for i in 0..len {
            let reg = SlotId(i as u16);
            if target[reg] == LinkMode::Stack {
                match self[reg] {
                    LinkMode::Xmm(freg) => {
                        self.xmm_to_both(freg);
                        ir.push(AsmInst::XmmToBoth(freg, self.xmm_slots(freg).to_vec()));
                    }
                    LinkMode::Literal(v) => {
                        ir.push(AsmInst::LitToStack(v, reg));
                    }
                    LinkMode::Both(_) | LinkMode::Stack => {}
                    LinkMode::R15 => unreachable!(),
                }
                self.release(reg);
            };
        }

        let mut conv_list = vec![];
        let mut guard_list = vec![];
        for i in 0..len {
            let reg = SlotId(i as u16);
            match (self[reg], target[reg]) {
                (LinkMode::Xmm(l), LinkMode::Xmm(r)) => {
                    if l == r {
                    } else if self.is_xmm_vacant(r) {
                        self.link_xmm(reg, r);
                        ir.push(AsmInst::XmmMove(l, r));
                    } else {
                        self.xmm_swap(l, r);
                        ir.push(AsmInst::XmmSwap(l, r));
                    }
                }
                (LinkMode::Both(l), LinkMode::Xmm(r)) => {
                    if l == r {
                        self[reg] = LinkMode::Xmm(l);
                    } else if self.is_xmm_vacant(r) {
                        self.link_xmm(reg, r);
                        ir.push(AsmInst::XmmMove(l, r));
                    } else {
                        self.xmm_swap(l, r);
                        ir.push(AsmInst::XmmSwap(l, r));
                    }
                    guard_list.push(reg);
                }
                (LinkMode::Stack, LinkMode::Stack) => {}
                (LinkMode::Xmm(l), LinkMode::Both(r)) => {
                    ir.push(AsmInst::XmmToBoth(l, vec![reg]));
                    if l == r {
                        self[reg] = LinkMode::Both(l);
                    } else if self.is_xmm_vacant(r) {
                        self.link_both(reg, r);
                        ir.push(AsmInst::XmmMove(l, r));
                    } else {
                        self.xmm_swap(l, r);
                        ir.push(AsmInst::XmmSwap(l, r));
                    }
                }
                (LinkMode::Both(l), LinkMode::Both(r)) => {
                    if l == r {
                    } else if self.is_xmm_vacant(r) {
                        self.link_both(reg, r);
                        ir.push(AsmInst::XmmMove(l, r));
                    } else {
                        self.xmm_swap(l, r);
                        ir.push(AsmInst::XmmSwap(l, r));
                    }
                }
                (LinkMode::Stack, LinkMode::Both(r)) => {
                    self.link_both(reg, r);
                    conv_list.push((reg, r));
                }
                (LinkMode::Literal(l), LinkMode::Literal(r)) if l == r => {}
                (LinkMode::Literal(l), LinkMode::Xmm(r)) => {
                    if let Some(f) = l.try_float() {
                        self.link_xmm(reg, r);
                        ir.push(AsmInst::F64ToXmm(f, r));
                    } else {
                        unreachable!()
                    }
                }
                (l, r) => unreachable!("src:{:?} target:{:?}", l, r),
            }
        }

        let side_exit = ir.new_deopt(pc + 1, self.get_write_back());

        for (r, x) in conv_list {
            ir.push(AsmInst::NumToXmm(r, x, side_exit));
        }

        for r in guard_list {
            ir.push(AsmInst::GuardFloat(r, side_exit));
        }

        ir
    }

    fn write_back_branches(
        &self,
        entries: Vec<BranchEntry>,
        cur_label: DestLabel,
        pc: BcPc,
        _bb_pos: BcIndex,
        unused: &[SlotId],
    ) -> Vec<(AsmIr, DestLabel, Option<DestLabel>)> {
        let mut target_ctx = self.clone();
        target_ctx.remove_unused(unused);
        let mut v = vec![];
        for BranchEntry {
            src_idx: _src_idx,
            mut bbctx,
            label,
            cont,
        } in entries
        {
            bbctx.remove_unused(unused);
            #[cfg(feature = "jit-debug")]
            eprintln!("  ***write_back {_src_idx}->{_bb_pos}");
            let ir = bbctx.write_back_for_target(&target_ctx, pc);
            v.push((ir, label, if cont { None } else { Some(cur_label) }));
            #[cfg(feature = "jit-debug")]
            eprintln!("  ***write_back end");
        }
        v
    }
}
