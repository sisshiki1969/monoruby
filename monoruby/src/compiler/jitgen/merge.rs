use super::*;

impl Codegen {
    fn gen_asmir(&mut self, labels: &[DestLabel], inst: &AsmInst) {
        match inst {
            AsmInst::AccToStack(r) => {
                self.store_r15(*r);
            }
            AsmInst::XmmMove(l, r) => self.xmm_mov(*l, *r),
            AsmInst::XmmSwap(l, r) => self.xmm_swap(*l, *r),
            AsmInst::F64ToXmm(f, x) => {
                let f = self.jit.const_f64(*f);
                monoasm!( &mut self.jit,
                    movq  xmm(x.enc()), [rip + f];
                );
            }
            AsmInst::XmmToBoth(x, slots) => self.xmm_to_both(*x, slots),
            AsmInst::LitToStack(v, slot) => self.literal_to_stack(*slot, *v),
            AsmInst::IntToF64(r, x, side_exit) => {
                self.load_rdi(*r);
                self.unbox_integer_float_to_f64(x.enc(), labels[*side_exit]);
                #[cfg(feature = "jit-debug")]
                eprintln!("      conv: {:?}->{:?}", r, x);
            }
            AsmInst::GuardFloat(r, side_exit) => self.slot_guard_float(*r, labels[*side_exit]),
        }
    }
}

impl Codegen {
    pub(super) fn gen_merging_branches(
        &mut self,
        func: &ISeqInfo,
        cc: &mut JitContext,
        bb_pos: BcIndex,
    ) -> Option<BBContext> {
        //let bb_pos = cc.cur_pos;
        let is_loop = func.get_pc(bb_pos).is_loop_start();
        let res = if is_loop {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge bb(loop): {bb_pos}");
            Self::merging_branches_loop(func, cc, bb_pos)?
        } else {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge bb: {bb_pos}");
            self.merging_branches_non_loop(func, cc, bb_pos)?
        };
        cc.gen_asm(self);
        #[cfg(feature = "jit-debug")]
        eprintln!("===merge_end");
        Some(res)
    }

    fn merging_branches_loop(
        func: &ISeqInfo,
        cc: &mut JitContext,
        bb_pos: BcIndex,
    ) -> Option<BBContext> {
        //let bb_pos = cc.cur_pos;
        if let Some(entries) = cc.branch_map.remove(&bb_pos) {
            let pc = func.get_pc(bb_pos);

            let (use_set, unused) = cc.analyse(func, bb_pos);

            let cur_label = cc.labels[&bb_pos];

            #[cfg(feature = "jit-debug")]
            {
                eprintln!("  use set:  {:?}", use_set);
                eprintln!("  not used: {:?}", unused);
            }

            let template = BBContext::merge_entries(&entries);

            let mut target_ctx = BBContext::new(&cc);
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
            cc.asmir.extend(v);

            cc.new_backedge(func, &mut target_ctx, bb_pos, cur_label, unused);

            Some(target_ctx)
        } else {
            None
        }
    }

    fn merging_branches_non_loop(
        &mut self,
        func: &ISeqInfo,
        cc: &mut JitContext,
        bb_pos: BcIndex,
    ) -> Option<BBContext> {
        if let Some(mut entries) = cc.branch_map.remove(&bb_pos) {
            let pc = func.get_pc(bb_pos);

            if entries.len() == 1 {
                let entry = entries.remove(0);
                self.jit.bind_label(entry.entry);
                return Some(entry.bbctx);
            }

            let target_ctx = BBContext::merge_entries(&entries);
            let cur_label = cc.labels[&bb_pos];

            let v = target_ctx.write_back_branches(entries, cur_label, pc, bb_pos, &[]);
            cc.asmir.extend(v);

            Some(target_ctx)
        } else {
            None
        }
    }

    pub(super) fn gen_code(&mut self, ir: AsmIr, cont: bool, entry: DestLabel, exit: DestLabel) {
        let mut labels = vec![];
        for _ in 0..ir.label {
            labels.push(self.jit.label());
        }

        for (pc, wb, label) in ir.deopt {
            self.gen_side_deopt_with_label(pc, &wb, labels[label])
        }
        if !cont {
            self.jit.select_page(1);
            self.jit.bind_label(entry);
        }
        for inst in ir.inst {
            self.gen_asmir(&labels, &inst);
        }
        if !cont {
            monoasm!( &mut self.jit,
                jmp exit;
            );
            self.jit.select_page(0);
        }
    }
}

impl BBContext {
    pub(super) fn write_back_for_target(mut self, target: &BBContext, pc: BcPc) -> AsmIr {
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

        let side_exit = ir.new_label();
        ir.new_deopt(pc + 1, self.get_write_back(), side_exit);

        for (r, x) in conv_list {
            ir.push(AsmInst::IntToF64(r, x, side_exit));
        }

        for r in guard_list {
            ir.push(AsmInst::GuardFloat(r, side_exit));
        }

        ir
    }
}
