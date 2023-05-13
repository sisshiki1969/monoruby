use super::*;

impl Codegen {
    pub(super) fn gen_backedge_branches(&mut self, cc: &mut JitContext, func: &ISeqInfo) {
        let branch_map = std::mem::take(&mut cc.branch_map);
        for (bb_pos, entries) in branch_map.into_iter() {
            let (target_label, merge_info, unused) = cc.backedge_map.remove(&bb_pos).unwrap();
            let target_ctx = BBContext::from_merge_info(&merge_info, cc.self_value);
            let pc = func.get_pc(bb_pos);
            for BranchEntry {
                src_idx: _src_idx,
                mut bbctx,
                dest_label,
            } in entries
            {
                #[cfg(feature = "emit-tir")]
                eprintln!("  backedge_write_back {_src_idx}->{bb_pos}");
                bbctx.remove_unused(&unused);
                self.gen_write_back_for_target(bbctx, &target_ctx, dest_label, target_label, pc);
            }
        }
    }

    pub(super) fn gen_merging_branches_loop(
        &mut self,
        func: &ISeqInfo,
        fnstore: &Store,
        cc: &mut JitContext,
        bb_pos: BcIndex,
    ) -> BBContext {
        if let Some(entries) = cc.branch_map.remove(&bb_pos) {
            let pc = func.get_pc(bb_pos);
            #[cfg(feature = "emit-tir")]
            eprintln!("gen_merge bb(loop): {bb_pos}");
            let (use_set, unused) = analysis::LoopAnalysis::analyse(func, fnstore, cc.bb_pos);
            let cur_label = cc.labels[&bb_pos];

            #[cfg(feature = "emit-tir")]
            {
                eprintln!("  use set:  {:?}", use_set);
                eprintln!("  not used: {:?}", unused);
            }

            let target_slot_info = MergeInfo::merge_entries(&entries).stack_slot;
            let mut ctx = BBContext::new(func.total_reg_num(), func.local_num(), cc.self_value);
            for (reg, class) in use_set {
                match target_slot_info[reg] {
                    LinkMode::Stack => {}
                    LinkMode::Xmm(_) if class => {
                        let freg = ctx.alloc_xmm();
                        ctx.link_xmm(reg, freg);
                    }
                    LinkMode::Both(_) | LinkMode::Xmm(_) => {
                        let freg = ctx.alloc_xmm();
                        ctx.link_both(reg, freg);
                    }
                };
            }
            #[cfg(feature = "emit-tir")]
            eprintln!("  merged target:   {:?}", ctx.stack_slot);

            for BranchEntry {
                src_idx: _src_idx,
                mut bbctx,
                dest_label,
            } in entries
            {
                bbctx.remove_unused(&unused);
                #[cfg(feature = "emit-tir")]
                eprintln!("  write_back {_src_idx}->{bb_pos} {:?}", bbctx.stack_slot);
                self.gen_write_back_for_target(bbctx, &ctx, dest_label, cur_label, pc + 1);
            }

            cc.new_backedge(&ctx, cc.bb_pos, cur_label, unused);
            #[cfg(feature = "emit-tir")]
            eprintln!("merge_end");
            ctx
        } else {
            unreachable!()
        }
    }

    pub(super) fn gen_merging_branches(
        &mut self,
        func: &ISeqInfo,
        cc: &mut JitContext,
        bb_pos: BcIndex,
    ) -> BBContext {
        if let Some(mut entries) = cc.branch_map.remove(&bb_pos) {
            let pc = func.get_pc(bb_pos);

            if entries.len() == 1 {
                let entry = entries.remove(0);
                #[cfg(feature = "emit-tir")]
                eprintln!("gen_merge bb: {bb_pos}<-{}", entry.src_idx);
                self.jit.bind_label(entry.dest_label);
                return entry.bbctx;
            }

            #[cfg(feature = "emit-tir")]
            eprintln!("gen_merge bb: {bb_pos}");

            let merge_info = MergeInfo::merge_entries(&entries);
            #[cfg(feature = "emit-tir")]
            eprintln!("  target: {:?}", merge_info);

            let cur_label = cc.labels[&bb_pos];
            let target_ctx = BBContext::from_merge_info(&merge_info, cc.self_value);
            for BranchEntry {
                src_idx: _src_idx,
                bbctx,
                dest_label,
            } in entries
            {
                #[cfg(feature = "emit-tir")]
                eprintln!("  write_back {_src_idx}->{bb_pos}",);
                self.gen_write_back_for_target(bbctx, &target_ctx, dest_label, cur_label, pc);
            }

            #[cfg(feature = "emit-tir")]
            eprintln!("merge_end");

            target_ctx
        } else {
            unreachable!()
        }
    }

    fn gen_write_back_for_target(
        &mut self,
        mut src_ctx: BBContext,
        target_ctx: &BBContext,
        entry: DestLabel,
        exit: DestLabel,
        pc: BcPc,
    ) {
        #[cfg(feature = "emit-tir")]
        {
            eprintln!("      src:    {:?}", src_ctx.stack_slot);
            eprintln!("      target: {:?}", target_ctx.stack_slot);
        }
        let len = src_ctx.stack_slot.0.len();

        self.jit.select_page(1);
        self.jit.bind_label(entry);
        for i in 0..len {
            let reg = SlotId(i as u16);
            if target_ctx.stack_slot[reg] == LinkMode::Stack {
                match src_ctx.stack_slot[reg] {
                    LinkMode::Xmm(freg) => {
                        let v = src_ctx.xmm[freg].clone();
                        for i in &v {
                            src_ctx.stack_slot[*i] = LinkMode::Both(freg);
                        }
                        src_ctx.dealloc_xmm(reg);
                        self.gen_write_back_single(freg, v);
                    }
                    LinkMode::Both(_) => {
                        src_ctx.dealloc_xmm(reg);
                    }
                    LinkMode::Stack => {}
                }
            };
        }

        let mut conv_list = vec![];
        let mut guard_list = vec![];
        for i in 0..len {
            let reg = SlotId(i as u16);
            match (src_ctx.stack_slot[reg], target_ctx.stack_slot[reg]) {
                (LinkMode::Xmm(l), LinkMode::Xmm(r)) => {
                    if l == r {
                        src_ctx.stack_slot[reg] = LinkMode::Xmm(l);
                    } else if src_ctx.xmm[r].is_empty() {
                        monoasm!( &mut self.jit,
                            movq  xmm(r.enc()), xmm(l.enc());
                        );
                        src_ctx.dealloc_xmm(reg);
                        src_ctx.link_xmm(reg, r);
                    } else {
                        src_ctx.xmm_swap(l, r);
                        monoasm!( &mut self.jit,
                            movq  xmm0, xmm(l.enc());
                            movq  xmm(l.enc()), xmm(r.enc());
                            movq  xmm(r.enc()), xmm0;
                        );
                    }
                }
                (LinkMode::Both(l), LinkMode::Xmm(r)) => {
                    if l == r {
                        src_ctx.stack_slot[reg] = LinkMode::Xmm(l);
                    } else if src_ctx.xmm[r].is_empty() {
                        monoasm!( &mut self.jit,
                            movq  xmm(r.enc()), xmm(l.enc());
                        );
                        src_ctx.dealloc_xmm(reg);
                        src_ctx.link_xmm(reg, r);
                    } else {
                        src_ctx.xmm_swap(l, r);
                        monoasm!( &mut self.jit,
                            movq  xmm0, xmm(l.enc());
                            movq  xmm(l.enc()), xmm(r.enc());
                            movq  xmm(r.enc()), xmm0;
                        );
                    }
                    guard_list.push(reg);
                }
                (_, LinkMode::Stack) => {}
                (LinkMode::Xmm(l), LinkMode::Both(r)) => {
                    self.gen_write_back_single(l, vec![reg]);
                    if l == r {
                        src_ctx.stack_slot[reg] = LinkMode::Both(l);
                    } else if src_ctx.xmm[r].is_empty() {
                        monoasm!( &mut self.jit,
                            movq  xmm(r.enc()), xmm(l.enc());
                        );
                        src_ctx.dealloc_xmm(reg);
                        src_ctx.link_both(reg, r);
                    } else {
                        src_ctx.xmm_swap(l, r);
                        monoasm!( &mut self.jit,
                            movq  xmm0, xmm(l.enc());
                            movq  xmm(l.enc()), xmm(r.enc());
                            movq  xmm(r.enc()), xmm0;
                        );
                    }
                }
                (LinkMode::Both(l), LinkMode::Both(r)) => {
                    if l == r {
                        src_ctx.stack_slot[reg] = LinkMode::Both(l);
                    } else if src_ctx.xmm[r].is_empty() {
                        monoasm!( &mut self.jit,
                            movq  xmm(r.enc()), xmm(l.enc());
                        );
                        src_ctx.dealloc_xmm(reg);
                        src_ctx.link_both(reg, r);
                    } else {
                        src_ctx.xmm_swap(l, r);
                        monoasm!( &mut self.jit,
                            movq  xmm0, xmm(l.enc());
                            movq  xmm(l.enc()), xmm(r.enc());
                            movq  xmm(r.enc()), xmm0;
                        );
                    }
                }
                (LinkMode::Stack, LinkMode::Both(r)) => {
                    src_ctx.link_both(reg, r);
                    conv_list.push((reg, r));
                }
                _ => unreachable!(),
            }
        }
        #[cfg(feature = "emit-tir")]
        eprintln!("      src_end:   {:?}", src_ctx.stack_slot);

        let side_exit = self.jit.label();
        for (reg, freg) in conv_list {
            self.load_rdi(reg);
            self.unbox_integer_float_to_f64(freg.enc(), side_exit);
            #[cfg(feature = "emit-tir")]
            eprintln!("      conv: {:?}->{:?}", reg, freg);
        }
        for reg in guard_list {
            self.slot_guard_float(reg, side_exit);
        }
        monoasm!( &mut self.jit,
            jmp exit;
        );
        self.jit.select_page(0);
        self.gen_side_deopt_with_label(pc + 1, Some(&src_ctx), side_exit);
    }
}
