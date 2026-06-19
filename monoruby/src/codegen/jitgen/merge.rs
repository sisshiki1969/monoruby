use super::*;

impl<'a> JitContext<'a> {
    ///
    /// Generate bridge AsmIr for backedge branches.
    ///
    pub(super) fn backedge_branches(&mut self) {
        let branch_map = self.detach_branch_map();
        for (bbid, entries) in branch_map.into_iter() {
            let target = self.remove_backedge(bbid).unwrap();
            let pc = self.iseq().get_bb_pc(bbid);
            #[cfg(feature = "jit-debug")]
            eprintln!("  backedge_bridge to:{bbid:?} target:{target:?}");
            for BranchEntry {
                src_bb,
                state,
                mode,
                ..
            } in entries
            {
                #[cfg(feature = "jit-debug")]
                eprintln!("    {mode:?} src:{src_bb:?}");

                let mut ir = AsmIr::new(self);
                state.gen_bridge(&mut ir, &target, pc);
                match mode {
                    BranchMode::Side { dest } => {
                        self.add_outline_bridge(ir, dest, bbid);
                    }
                    BranchMode::Branch => {
                        self.add_inline_bridge(src_bb, ir, Some(bbid));
                    }
                    BranchMode::Continue => unreachable!(),
                }
            }
            #[cfg(feature = "jit-debug")]
            eprintln!("  backedge_bridge end");
        }
    }

    ///
    /// Merge incoming contexts for *bbid*.
    ///
    /// ```text
    ///                    
    ///      entries       
    ///                    
    ///     \   |   /              
    ///      \  |  /  /======== backedge             
    ///       v v v  /              
    ///  +------------+      
    ///  |   target   |      
    ///  +------------+      
    ///         |
    ///         v
    ///  +------------+
    ///  |    bbid    |
    ///  +------------+
    /// ```
    pub(super) fn incoming_context(
        &mut self,
        bbid: BasicBlockId,
        no_calc_backedge: bool,
    ) -> JitResult<Option<AbstractState>> {
        let entries = if let Some(entries) = self.remove_branch(bbid) {
            entries
        } else {
            return Ok(None);
        };
        let iseq = self.iseq();
        let pc = iseq.get_bb_pc(bbid);

        let res = if let Some((loop_start, loop_end)) = iseq.bb_info.is_loop_begin(bbid) {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge loop: {bbid:?}");

            let incoming = AbstractState::join_entries(&entries);
            if !no_calc_backedge {
                self.analyse_backedge_fixpoint(incoming.clone(), loop_start, loop_end)?;
            }

            let mut target = incoming;
            if let Some((liveness, backedge)) = self.loop_info(bbid) {
                let backedge_for_floats = backedge.as_ref().map(|b| b.slot_state().clone());
                if let Some(backedge) = backedge {
                    target.join(backedge);
                }

                target.liveness_analysis(liveness);

                // §15.5 loop-entry float specialization: a loop JIT enters a
                // loop-carried float from the VM as a conservative boxed
                // `S(Value)` even though the back-edge fixpoint proved it is a
                // `Float (F)`; `join(S(Value), F)` keeps `S`, so the body would
                // decode+rebox it every iteration. Re-adopt the back-edge's `F`
                // (the forward entry is unboxed once at the pre-header by the
                // `S -> F` bridge, whose `float_to_fpr` carries the runtime float
                // guard). Promote a slot only when every predecessor entry has a
                // valid `_ -> F` bridge (`F`/`S`/`Sf`/float-`C`); a non-float-`C`
                // path is genuinely not a float, so it is left boxed.
                if let Some(be) = &backedge_for_floats {
                    let float_bridgeable = |m: LinkMode| {
                        matches!(m, LinkMode::F(_) | LinkMode::S(_) | LinkMode::Sf(_, _))
                            || matches!(m, LinkMode::C(v) if v.is_float())
                    };
                    // Adoption policy (Layer-② representation decision, §16): which
                    // loop-carried slots re-adopt the back-edge fixpoint's `F`.
                    //
                    // Default reads the fixpoint's *placement* (`mode == F`) — the
                    // remaining dependence on the analysis-pass allocation. L2-1
                    // (feature `layer2-float-by-type`) replaces it with the
                    // allocation-free *type + liveness* signal: the back-edge type
                    // is `Float` and the slot is used as float in the loop
                    // (`Liveness::loop_used_as_float`). `try_set_new_F` (inside the
                    // mechanism) still self-limits to a free physical fpr, so a
                    // promotion the fixpoint could not place under pressure simply
                    // does not fire.
                    let promotable =
                        |i| entries.iter().all(|e| float_bridgeable(e.state.mode(i)));
                    #[cfg(not(feature = "layer2-float-by-type"))]
                    {
                        let adopt = |i| matches!(be.mode(i), LinkMode::F(_));
                        target.keep_backedge_floats(adopt, promotable);
                    }
                    #[cfg(feature = "layer2-float-by-type")]
                    {
                        let loop_float: std::collections::HashSet<SlotId> =
                            liveness.loop_used_as_float().map(|(s, _)| s).collect();
                        // Adopt the *type+liveness* signal (the L2-1 decoupling
                        // goal), but never adopt a *narrower* set than the
                        // fixpoint's placement: a loop-carried float the fixpoint
                        // already kept `F` must stay `F`, else the back-edge boxes
                        // it every iteration (the mandelbrot regression — the
                        // type signal misses copy-aliased carried floats). So
                        // adopt the union, keeping layer2 ⊇ the default greedy.
                        let adopt = |i| {
                            (be.is_float_typed(i) && loop_float.contains(&i))
                                || matches!(be.mode(i), LinkMode::F(_))
                        };
                        target.keep_backedge_floats(adopt, promotable);
                    }
                }
            }
            #[cfg(feature = "jit-debug")]
            eprintln!("  target:  {:?}\n", target.slot_state());

            // `bridge` adds `+1` to the deopt resume PC so it lands on
            // the first body instruction (skipping LoopStart, which
            // would re-enter the JIT and infinite-loop). Pass `pc` (=
            // LoopStart's PC) directly; an extra `+1` here would push
            // the deopt resume past the fused BinCmp into the bare
            // CondBr, which then reads a stale `%dst` — see #480.
            self.gen_bridges_for_branches(&target, entries, bbid, pc);
            self.new_backedge(target.slot_state().clone(), bbid);

            Some(target)
        } else {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge {bbid:?}");

            let target = AbstractState::join_entries(&entries);
            self.gen_bridges_for_branches(&target, entries, bbid, pc);

            Some(target)
        };

        #[cfg(feature = "jit-debug")]
        eprintln!("===merge_end");
        Ok(res)
    }

    ///
    /// Generate bridge AsmIr for branches(*entries*) flowing into the basic block(*bbid*).
    ///
    fn gen_bridges_for_branches(
        &mut self,
        target: &SlotState,
        entries: Vec<BranchEntry>,
        bbid: BasicBlockId,
        pc: BytecodePtr,
    ) {
        let target = target.clone();
        #[cfg(feature = "jit-debug")]
        eprintln!("  bridge to:{bbid:?} target:{target:?}");
        for BranchEntry {
            src_bb,
            state,
            mode,
            ..
        } in entries
        {
            #[cfg(feature = "jit-debug")]
            eprintln!("    {mode:?} src:{src_bb:?}");

            let mut ir = AsmIr::new(self);
            state.gen_bridge(&mut ir, &target, pc);
            match mode {
                BranchMode::Side { dest } => {
                    self.add_outline_bridge(ir, dest, bbid);
                }
                BranchMode::Branch => {
                    self.add_inline_bridge(src_bb, ir, Some(bbid));
                }
                BranchMode::Continue => {
                    self.add_inline_bridge(src_bb, ir, None);
                }
            }
        }
        #[cfg(feature = "jit-debug")]
        eprintln!("  bridge end");
    }
}
