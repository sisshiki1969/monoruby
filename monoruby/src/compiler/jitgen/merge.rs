use super::*;

struct AsmIr {
    inst: Vec<AsmInst>,
    deopt: Vec<(BcPc, WriteBack, DestLabel)>,
}

impl std::ops::Deref for AsmIr {
    type Target = Vec<AsmInst>;
    fn deref(&self) -> &Self::Target {
        &self.inst
    }
}

impl std::ops::DerefMut for AsmIr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inst
    }
}

impl AsmIr {
    fn new() -> Self {
        Self {
            inst: vec![],
            deopt: vec![],
        }
    }

    fn new_deopt(&mut self, pc: BcPc, wb: WriteBack, label: DestLabel) {
        self.deopt.push((pc, wb, label));
    }
}

enum AsmInst {
    AccToStack(SlotId),
    XmmMove(Xmm, Xmm),
    XmmSwap(Xmm, Xmm),
    F64ToXmm(f64, Xmm),
    XmmToBoth(Xmm, Vec<SlotId>),
    LitToStack(Value, SlotId),
    IntToF64(SlotId, Xmm, DestLabel),
    GuardFloat(SlotId, DestLabel),
}

impl Codegen {
    fn gen_asmir(&mut self, ir: &AsmInst) {
        match ir {
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
                self.unbox_integer_float_to_f64(x.enc(), *side_exit);
                #[cfg(feature = "jit-debug")]
                eprintln!("      conv: {:?}->{:?}", r, x);
            }
            AsmInst::GuardFloat(r, side_exit) => self.slot_guard_float(*r, *side_exit),
        }
    }
}

impl Codegen {
    pub(super) fn gen_backedge_branches(&mut self, cc: &mut JitContext, func: &ISeqInfo) {
        let branch_map = std::mem::take(&mut cc.branch_map);
        for (bb_pos, entries) in branch_map.into_iter() {
            let (target_label, mut target_ctx, unused) = cc.backedge_map.remove(&bb_pos).unwrap();
            let pc = func.get_pc(bb_pos);
            target_ctx.remove_unused(&unused);
            for BranchEntry {
                src_idx: _src_idx,
                mut bbctx,
                entry: dest_label,
                ..
            } in entries
            {
                #[cfg(feature = "jit-debug")]
                eprintln!("  backedge_write_back {_src_idx}->{bb_pos}");
                bbctx.remove_unused(&unused);
                let ir = self.gen_write_back_for_target(bbctx, &target_ctx, pc);
                self.gen_code(ir, false, dest_label, target_label);
            }
        }
    }

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
            self.gen_merging_branches_loop(func, cc, bb_pos)?
        } else {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge bb: {bb_pos}");
            self.gen_merging_branches_non_loop(func, cc, bb_pos)?
        };
        #[cfg(feature = "jit-debug")]
        eprintln!("===merge_end");
        Some(res)
    }

    fn gen_merging_branches_loop(
        &mut self,
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

            self.write_back_branches(entries, &target_ctx, cur_label, pc + 1, bb_pos, &unused);

            cc.new_backedge(func, &mut target_ctx, bb_pos, cur_label, unused);

            Some(target_ctx)
        } else {
            None
        }
    }

    fn gen_merging_branches_non_loop(
        &mut self,
        func: &ISeqInfo,
        cc: &mut JitContext,
        bb_pos: BcIndex,
    ) -> Option<BBContext> {
        //let bb_pos = cc.cur_pos;
        if let Some(mut entries) = cc.branch_map.remove(&bb_pos) {
            let pc = func.get_pc(bb_pos);

            if entries.len() == 1 {
                let entry = entries.remove(0);
                self.jit.bind_label(entry.entry);
                return Some(entry.bbctx);
            }

            let target_ctx = BBContext::merge_entries(&entries);
            let cur_label = cc.labels[&bb_pos];

            self.write_back_branches(entries, &target_ctx, cur_label, pc, bb_pos, &[]);

            Some(target_ctx)
        } else {
            None
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
        let mut target_ctx = target_ctx.clone();
        target_ctx.remove_unused(unused);
        for BranchEntry {
            src_idx: _src_idx,
            mut bbctx,
            entry,
            cont,
        } in entries
        {
            bbctx.remove_unused(unused);
            #[cfg(feature = "jit-debug")]
            eprintln!("  ***write_back {_src_idx}->{_bb_pos}");
            let ir = self.gen_write_back_for_target(bbctx, &target_ctx, pc);
            self.gen_code(ir, cont, entry, cur_label);
            #[cfg(feature = "jit-debug")]
            eprintln!("  ***write_back end");
        }
    }

    fn gen_write_back_for_target(
        &mut self,
        mut src_ctx: BBContext,
        target_ctx: &BBContext,
        pc: BcPc,
    ) -> AsmIr {
        #[cfg(feature = "jit-debug")]
        {
            eprintln!("    src:    {:?}", src_ctx.slot_state);
            eprintln!("    target: {:?}", target_ctx.slot_state);
        }
        let mut ir = AsmIr::new();
        let len = src_ctx.reg_num();

        //self.writeback_acc(&mut src_ctx);
        if let Some(slot) = src_ctx.clear_r15() {
            ir.push(AsmInst::AccToStack(slot));
        }

        for i in 0..len {
            let reg = SlotId(i as u16);
            if target_ctx[reg] == LinkMode::Stack {
                match src_ctx[reg] {
                    LinkMode::Xmm(freg) => {
                        src_ctx.xmm_to_both(freg);
                        ir.push(AsmInst::XmmToBoth(freg, src_ctx.xmm_slots(freg).to_vec()));
                    }
                    LinkMode::Literal(v) => {
                        ir.push(AsmInst::LitToStack(v, reg));
                    }
                    LinkMode::Both(_) | LinkMode::Stack => {}
                    LinkMode::R15 => unreachable!(),
                }
                src_ctx.release(reg);
            };
        }

        let mut conv_list = vec![];
        let mut guard_list = vec![];
        for i in 0..len {
            let reg = SlotId(i as u16);
            match (src_ctx[reg], target_ctx[reg]) {
                (LinkMode::Xmm(l), LinkMode::Xmm(r)) => {
                    if l == r {
                    } else if src_ctx.is_xmm_vacant(r) {
                        src_ctx.link_xmm(reg, r);
                        ir.push(AsmInst::XmmMove(l, r));
                    } else {
                        src_ctx.xmm_swap(l, r);
                        ir.push(AsmInst::XmmSwap(l, r));
                    }
                }
                (LinkMode::Both(l), LinkMode::Xmm(r)) => {
                    if l == r {
                        src_ctx[reg] = LinkMode::Xmm(l);
                    } else if src_ctx.is_xmm_vacant(r) {
                        src_ctx.link_xmm(reg, r);
                        ir.push(AsmInst::XmmMove(l, r));
                    } else {
                        src_ctx.xmm_swap(l, r);
                        ir.push(AsmInst::XmmSwap(l, r));
                    }
                    guard_list.push(reg);
                }
                (LinkMode::Stack, LinkMode::Stack) => {}
                (LinkMode::Xmm(l), LinkMode::Both(r)) => {
                    ir.push(AsmInst::XmmToBoth(l, vec![reg]));
                    if l == r {
                        src_ctx[reg] = LinkMode::Both(l);
                    } else if src_ctx.is_xmm_vacant(r) {
                        src_ctx.link_both(reg, r);
                        ir.push(AsmInst::XmmMove(l, r));
                    } else {
                        src_ctx.xmm_swap(l, r);
                        ir.push(AsmInst::XmmSwap(l, r));
                    }
                }
                (LinkMode::Both(l), LinkMode::Both(r)) => {
                    if l == r {
                    } else if src_ctx.is_xmm_vacant(r) {
                        src_ctx.link_both(reg, r);
                        ir.push(AsmInst::XmmMove(l, r));
                    } else {
                        src_ctx.xmm_swap(l, r);
                        ir.push(AsmInst::XmmSwap(l, r));
                    }
                }
                (LinkMode::Stack, LinkMode::Both(r)) => {
                    src_ctx.link_both(reg, r);
                    conv_list.push((reg, r));
                }
                (LinkMode::Literal(l), LinkMode::Literal(r)) if l == r => {}
                (LinkMode::Literal(l), LinkMode::Xmm(r)) => {
                    if let Some(f) = l.try_float() {
                        src_ctx.link_xmm(reg, r);
                        ir.push(AsmInst::F64ToXmm(f, r));
                    } else {
                        unreachable!()
                    }
                }
                (l, r) => unreachable!("src:{:?} target:{:?}", l, r),
            }
        }

        let side_exit = self.jit.label();
        ir.new_deopt(pc + 1, src_ctx.get_write_back(), side_exit);

        for (r, x) in conv_list {
            ir.push(AsmInst::IntToF64(r, x, side_exit));
        }

        for r in guard_list {
            ir.push(AsmInst::GuardFloat(r, side_exit));
        }

        ir
        //self.gen_code(ir, cont, entry, exit);
    }

    fn gen_code(&mut self, ir: AsmIr, cont: bool, entry: DestLabel, exit: DestLabel) {
        for (pc, wb, label) in ir.deopt {
            self.gen_side_deopt_with_label(pc, &wb, label)
        }
        if !cont {
            self.jit.select_page(1);
            self.jit.bind_label(entry);
        }
        for ir in ir.inst {
            self.gen_asmir(&ir);
        }
        if !cont {
            monoasm!( &mut self.jit,
                jmp exit;
            );
            self.jit.select_page(0);
        }
    }
}
