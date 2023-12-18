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
                mut bb,
                label,
                ..
            } in entries
            {
                #[cfg(feature = "jit-debug")]
                eprintln!("  backedge_write_back {_src_idx}->{bb_pos}");
                bb.remove_unused(&unused);
                let mut ir = AsmIr::new();
                bb.write_back_for_target(&target_ctx, &mut ir, pc);
                self.bridges.push((ir, label, target_label));
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

        self.write_back_branches(&target_ctx, entries, cur_label, pc + 1, bb_pos, &unused);

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

        Some(target_ctx)
    }

    fn write_back_branches(
        &mut self,
        target_ctx: &BBContext,
        entries: Vec<BranchEntry>,
        cur_label: DestLabel,
        pc: BcPc,
        _bb_pos: BcIndex,
        unused: &[SlotId],
    ) {
        let mut target_ctx = target_ctx.clone();
        target_ctx.remove_unused(unused);
        for BranchEntry {
            src_idx: _src_idx,
            mut bb,
            label,
            cont,
        } in entries
        {
            bb.remove_unused(unused);
            #[cfg(feature = "jit-debug")]
            eprintln!("  ***write_back {_src_idx}->{_bb_pos}");
            if cont {
                assert!(self.continuation_bridge.is_none());
                self.continuation_bridge = Some((Some((bb, target_ctx.clone(), pc)), label));
            } else {
                let mut ir = AsmIr::new();
                bb.write_back_for_target(&target_ctx, &mut ir, pc);
                self.bridges.push((ir, label, cur_label));
            }
            #[cfg(feature = "jit-debug")]
            eprintln!("  ***write_back end");
        }
    }
}

impl BBContext {
    pub(super) fn write_back_for_target(mut self, target: &BBContext, ir: &mut AsmIr, pc: BcPc) {
        #[cfg(feature = "jit-debug")]
        {
            eprintln!("    src:    {:?}", self.slot_state);
            eprintln!("    target: {:?}", target.slot_state);
        }
        let len = self.reg_num();

        ir.writeback_acc(&mut self);

        for i in 0..len {
            let reg = SlotId(i as u16);
            if target[reg] == LinkMode::Stack {
                match self[reg] {
                    LinkMode::Xmm(freg) => {
                        self.xmm_to_both(freg);
                        ir.xmm2both(freg, self.xmm_slots(freg).to_vec());
                    }
                    LinkMode::Literal(v) => {
                        ir.lit2stack(v, reg);
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
                        ir.xmm_move(l, r);
                    } else {
                        self.xmm_swap(l, r);
                        ir.xmm_swap(l, r);
                    }
                }
                (LinkMode::Both(l), LinkMode::Xmm(r)) => {
                    if l == r {
                        self[reg] = LinkMode::Xmm(l);
                    } else if self.is_xmm_vacant(r) {
                        self.link_xmm(reg, r);
                        ir.xmm_move(l, r);
                    } else {
                        self.xmm_swap(l, r);
                        ir.xmm_swap(l, r);
                    }
                    guard_list.push(reg);
                }
                (LinkMode::Stack, LinkMode::Stack) => {}
                (LinkMode::Xmm(l), LinkMode::Both(r)) => {
                    ir.xmm2both(l, vec![reg]);
                    if l == r {
                        self[reg] = LinkMode::Both(l);
                    } else if self.is_xmm_vacant(r) {
                        self.link_both(reg, r);
                        ir.xmm_move(l, r);
                    } else {
                        self.xmm_swap(l, r);
                        ir.xmm_swap(l, r);
                    }
                }
                (LinkMode::Both(l), LinkMode::Both(r)) => {
                    if l == r {
                    } else if self.is_xmm_vacant(r) {
                        self.link_both(reg, r);
                        ir.xmm_move(l, r);
                    } else {
                        self.xmm_swap(l, r);
                        ir.xmm_swap(l, r);
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
                        ir.f64toxmm(f, r);
                    } else {
                        unreachable!()
                    }
                }
                (l, r) => unreachable!("src:{:?} target:{:?}", l, r),
            }
        }

        let deopt = ir.new_deopt(pc + 1, self.get_write_back());

        for (r, x) in conv_list {
            ir.stack2reg(r, GP::Rax);
            ir.inst.push(AsmInst::NumToXmm(GP::Rax, x, deopt));
        }

        for r in guard_list {
            ir.stack2reg(r, GP::Rax);
            ir.guard_float(GP::Rax, deopt);
        }
    }
}
