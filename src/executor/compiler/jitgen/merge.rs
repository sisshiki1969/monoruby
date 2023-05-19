use super::*;

impl Codegen {
    pub(super) fn gen_backedge_branches(&mut self, cc: &mut JitContext, func: &ISeqInfo) {
        let branch_map = std::mem::take(&mut cc.branch_map);
        for (bb_pos, entries) in branch_map.into_iter() {
            let (target_label, target_ctx, unused) = cc.backedge_map.remove(&bb_pos).unwrap();
            let pc = func.get_pc(bb_pos);
            for BranchEntry {
                src_idx: _src_idx,
                mut bbctx,
                entry: dest_label,
                ..
            } in entries
            {
                #[cfg(feature = "emit-tir")]
                eprintln!("  backedge_write_back {_src_idx}->{bb_pos}");
                bbctx.remove_unused(&unused);
                self.gen_write_back_for_target(
                    bbctx,
                    &target_ctx,
                    dest_label,
                    target_label,
                    pc,
                    false,
                );
            }
        }
    }

    pub(super) fn gen_merging_branches(
        &mut self,
        func: &ISeqInfo,
        cc: &mut JitContext,
    ) -> BBContext {
        let bb_pos = cc.bb_pos;
        let is_loop = func.get_pc(bb_pos).is_loop();
        let res = if is_loop {
            #[cfg(feature = "emit-tir")]
            eprintln!("\n===gen_merge bb(loop): {bb_pos}");
            self.gen_merging_branches_loop(func, cc)
        } else {
            #[cfg(feature = "emit-tir")]
            eprintln!("\n===gen_merge bb: {bb_pos}");
            self.gen_merging_branches_non_loop(func, cc)
        };
        #[cfg(feature = "emit-tir")]
        eprintln!("===merge_end\n");
        res
    }

    fn gen_merging_branches_loop(&mut self, func: &ISeqInfo, cc: &mut JitContext) -> BBContext {
        let bb_pos = cc.bb_pos;
        if let Some(entries) = cc.branch_map.remove(&bb_pos) {
            let pc = func.get_pc(bb_pos);
            let bb_pos = cc.bb_pos;

            let (use_set, unused) = analysis::LoopAnalysis::analyse(cc, func, bb_pos);

            let cur_label = cc.labels[&bb_pos];

            #[cfg(feature = "emit-tir")]
            {
                eprintln!("  use set:  {:?}", use_set);
                eprintln!("  not used: {:?}", unused);
            }

            let template = BBContext::merge_entries(&entries);

            let mut target_ctx = BBContext::new(&cc);
            let mut const_vec = vec![];
            for (reg, coerced) in use_set {
                match template.slot_state[reg] {
                    LinkMode::Stack => {}
                    LinkMode::Const(v) => {
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
                let freg = target_ctx.alloc_xmm();
                target_ctx.link_xmm(r, freg);
            }
            #[cfg(feature = "emit-tir")]
            eprintln!("  target_ctx:   {:?}", target_ctx.slot_state);

            self.write_back_branches(entries, &target_ctx, cur_label, pc + 1, bb_pos, &unused);

            cc.new_backedge(&target_ctx, bb_pos, cur_label, unused);

            target_ctx
        } else {
            unreachable!()
        }
    }

    fn gen_merging_branches_non_loop(&mut self, func: &ISeqInfo, cc: &mut JitContext) -> BBContext {
        let bb_pos = cc.bb_pos;
        if let Some(mut entries) = cc.branch_map.remove(&bb_pos) {
            let pc = func.get_pc(bb_pos);

            if entries.len() == 1 {
                let entry = entries.remove(0);
                self.jit.bind_label(entry.entry);
                return entry.bbctx;
            }

            let target_ctx = BBContext::merge_entries(&entries);
            let cur_label = cc.labels[&bb_pos];

            self.write_back_branches(entries, &target_ctx, cur_label, pc, bb_pos, &[]);

            target_ctx
        } else {
            unreachable!()
        }
    }

    fn write_back_branches(
        &mut self,
        entries: Vec<BranchEntry>,
        target_ctx: &BBContext,
        cur_label: DestLabel,
        pc: BcPc,
        _bb_pos: BcIndex,
        unused: &[SlotId],
    ) {
        for BranchEntry {
            src_idx: _src_idx,
            mut bbctx,
            entry,
            cont,
        } in entries
        {
            bbctx.remove_unused(&unused);
            #[cfg(feature = "emit-tir")]
            eprintln!("  write_back {_src_idx}->{_bb_pos} {:?}", bbctx.slot_state);
            self.gen_write_back_for_target(bbctx, target_ctx, entry, cur_label, pc, cont);
            #[cfg(feature = "emit-tir")]
            eprintln!("  write_back end");
        }
    }

    fn gen_write_back_for_target(
        &mut self,
        mut src_ctx: BBContext,
        target_ctx: &BBContext,
        entry: DestLabel,
        exit: DestLabel,
        pc: BcPc,
        cont: bool,
    ) {
        #[cfg(feature = "emit-tir")]
        {
            eprintln!("      src:    {:?}", src_ctx.slot_state);
            eprintln!("      target: {:?}", target_ctx.slot_state);
        }
        let len = src_ctx.slot_state.0.len();

        if !cont {
            self.jit.select_page(1);
            self.jit.bind_label(entry);
        }
        for i in 0..len {
            let reg = SlotId(i as u16);
            if target_ctx.slot_state[reg] == LinkMode::Stack {
                match src_ctx.slot_state[reg] {
                    LinkMode::Xmm(freg) => {
                        let v = src_ctx.xmm[freg].clone();
                        for i in &v {
                            src_ctx.slot_state[*i] = LinkMode::Both(freg);
                        }
                        src_ctx.dealloc_xmm(reg);
                        self.gen_write_back_single(freg, v);
                    }
                    LinkMode::Both(_) => {
                        src_ctx.dealloc_xmm(reg);
                    }
                    LinkMode::Const(v) => {
                        self.write_back_val(reg, v);
                        src_ctx.slot_state[reg] = LinkMode::Stack;
                    }
                    LinkMode::Stack => {}
                }
            };
        }

        let mut conv_list = vec![];
        let mut guard_list = vec![];
        for i in 0..len {
            let reg = SlotId(i as u16);
            match (src_ctx.slot_state[reg], target_ctx.slot_state[reg]) {
                (LinkMode::Xmm(l), LinkMode::Xmm(r)) => {
                    if l == r {
                        src_ctx.slot_state[reg] = LinkMode::Xmm(l);
                    } else if src_ctx.xmm[r].is_empty() {
                        monoasm!( &mut self.jit,
                            movq  xmm(r.enc()), xmm(l.enc());
                        );
                        src_ctx.link_xmm(reg, r);
                    } else {
                        self.xmm_swap(&mut src_ctx, l, r);
                    }
                }
                (LinkMode::Both(l), LinkMode::Xmm(r)) => {
                    if l == r {
                        src_ctx.slot_state[reg] = LinkMode::Xmm(l);
                    } else if src_ctx.xmm[r].is_empty() {
                        monoasm!( &mut self.jit,
                            movq  xmm(r.enc()), xmm(l.enc());
                        );
                        src_ctx.link_xmm(reg, r);
                    } else {
                        self.xmm_swap(&mut src_ctx, l, r);
                    }
                    guard_list.push(reg);
                }
                (_, LinkMode::Stack) => {}
                (LinkMode::Xmm(l), LinkMode::Both(r)) => {
                    self.gen_write_back_single(l, vec![reg]);
                    if l == r {
                        src_ctx.slot_state[reg] = LinkMode::Both(l);
                    } else if src_ctx.xmm[r].is_empty() {
                        monoasm!( &mut self.jit,
                            movq  xmm(r.enc()), xmm(l.enc());
                        );
                        src_ctx.link_both(reg, r);
                    } else {
                        self.xmm_swap(&mut src_ctx, l, r);
                    }
                }
                (LinkMode::Both(l), LinkMode::Both(r)) => {
                    if l == r {
                        src_ctx.slot_state[reg] = LinkMode::Both(l);
                    } else if src_ctx.xmm[r].is_empty() {
                        monoasm!( &mut self.jit,
                            movq  xmm(r.enc()), xmm(l.enc());
                        );
                        src_ctx.link_both(reg, r);
                    } else {
                        self.xmm_swap(&mut src_ctx, l, r);
                    }
                }
                (LinkMode::Stack, LinkMode::Both(r)) => {
                    src_ctx.link_both(reg, r);
                    conv_list.push((reg, r));
                }
                (LinkMode::Const(l), LinkMode::Const(r)) if l == r => {}
                (LinkMode::Const(l), LinkMode::Xmm(r)) => {
                    if let Some(f) = l.try_float() {
                        src_ctx.link_xmm(reg, r);
                        let f = self.jit.const_f64(f);
                        monoasm!( &mut self.jit,
                            movq  xmm(r.enc()), [rip + f];
                        );
                    } else {
                        unreachable!()
                    }
                }
                (l, r) => unreachable!("src:{:?} target:{:?}", l, r),
            }
        }
        #[cfg(feature = "emit-tir")]
        eprintln!("      src_end:{:?}", src_ctx.slot_state);

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
        if !cont {
            monoasm!( &mut self.jit,
                jmp exit;
            );
            self.jit.select_page(0);
        }
        self.gen_side_deopt_with_label(pc + 1, Some(&src_ctx), side_exit);
    }

    fn xmm_swap(&mut self, ctx: &mut BBContext, l: Xmm, r: Xmm) {
        ctx.xmm_swap(l, r);
        monoasm!( &mut self.jit,
            movq  xmm0, xmm(l.enc());
            movq  xmm(l.enc()), xmm(r.enc());
            movq  xmm(r.enc()), xmm0;
        );
    }
}
