use super::*;
use crate::codegen::jitgen::context::SpecializedId;
use state::{Guarded, SfGuarded};

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
                state.gen_bridge_all(&mut ir, &target, pc, &self.chain_surrender_table());
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

            // §9d-2d: loop-carried GP retention. The loop-entry merge keeps an
            // agreed `G` binding (the fixpoint no longer demotes it at the
            // back-edge, and `use_float` tolerates a `G` loop-carried slot), so a
            // value the loop carries stays in its pool register across the
            // back-edge instead of round-tripping its stack home each iteration.
            let incoming = AbstractState::join_entries(&entries);
            let target_incoming = incoming.clone();
            if !no_calc_backedge {
                self.analyse_backedge_fixpoint(incoming.clone(), loop_start, loop_end)?;
            }

            // Everything the loop-entry merge does spans the chain — the
            // fixpoint's back edge is a whole `AbstractState`, `join` and
            // `equiv` walk every frame, and `gen_bridge_all` bridges every
            // frame. The three calls below are the exception, and they stay
            // innermost-only for two different reasons.
            //
            // `liveness_analysis`'s `kill_unused` is innermost *by nature*.
            // It discards the slots this loop does not touch, which is
            // sound at a merge inside the frame that owns the loop and
            // nowhere else: an outer frame's local that the loop never
            // reads is still live in that frame's own continuation.
            //
            // The float half — `loop_used_as_float` feeding `use_float`,
            // and `keep_backedge_floats` — is innermost only until an fpr
            // can be allocated across frames. `FprAllocator` is per
            // `SlotState` and its ids are positional (`FPReg(id)` is
            // `xmm{id+2}` below `PHYS_FPR_POOL`), so two frames each
            // promoting a slot would both take `FPReg(0)` and both write
            // `xmm2`. Promoting an outer frame's slot needs one id space
            // for the whole chain; that is the piece still missing, and it
            // is the same one that keeps `bridge_at` asserting no outer
            // `F`/`Sf`.
            let mut target = incoming;
            let mut hoist_version_guard = false;
            let mut hoisted_guards = vec![];
            if let Some((liveness, backedge)) = self.loop_info(bbid) {
                let backedge_for_floats = backedge.as_ref().map(|b| b.slot_state().clone());
                if let Some(backedge) = backedge {
                    target.join(backedge);
                    // Loop-entry hoisting of the class-version guard. The
                    // fixpoint analysed the body from a head without the
                    // guard, so a back edge that still has it checked the
                    // version after the body's last call: it arrives with
                    // the guard whatever the head had. Checking it once on
                    // each forward entry then lets the head keep it, and
                    // the body's typed ivar loads (`crate::ivar_ty`) need no
                    // guard per iteration. Only inline entries are bridged
                    // this way (the guard is emitted on the inline page).
                    if backedge.class_version_guard()
                        && !target.class_version_guard()
                        && entries
                            .iter()
                            .all(|e| !matches!(e.mode, BranchMode::Side { .. }))
                    {
                        target.set_loop_head_version_guard(backedge.ivar_ty_guard());
                        hoist_version_guard = true;
                    }
                }

                target.liveness_analysis(liveness);

                // Loop-invariant guard hoisting: a slot the forward entries
                // bring in untyped but the back edge carries typed was
                // guarded (or typed) on every path through the body, and
                // the head's join throws that away each iteration. Guard
                // it once on the forward entries instead, so the head
                // keeps the type and the body's guards fold away.
                if !no_calc_backedge && let Some(be) = backedge {
                    hoisted_guards = self.loop_hoist_guards(&target, &entries, liveness, be);
                    for &(slot, g) in &hoisted_guards {
                        target.refine_S_guarded(slot, g);
                    }
                }

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
                    // Use the allocation-free *type + liveness* signal: the
                    // back-edge type is `Float` and the slot is used as a float in
                    // the loop (`Liveness::loop_used_as_float`). `try_set_new_F`
                    // (inside the mechanism) still self-limits to a free physical
                    // fpr, so a promotion the fixpoint could not place under
                    // pressure simply does not fire.
                    let promotable =
                        |i| entries.iter().all(|e| float_bridgeable(e.state.mode(i)));
                    {
                        let loop_float: std::collections::HashSet<SlotId> =
                            liveness.loop_used_as_float().map(|(s, _)| s).collect();
                        // Adopt the type+liveness signal, but never adopt a
                        // *narrower* set than the fixpoint's placement: a
                        // loop-carried float the fixpoint already kept `F` must
                        // stay `F`, else the back-edge boxes it every iteration
                        // (the mandelbrot regression — the type signal misses
                        // copy-aliased carried floats). So adopt the union,
                        // keeping it ⊇ the placement-based greedy set.
                        //
                        // The `Sf(Float)` placement fallback (`adopt_sf`) is
                        // the subtree-crossing use signal: the fixpoint walk
                        // *descends into inlined blocks*, and a block that
                        // stores a float into an owner slot every iteration
                        // leaves that verdict behind as the outer promotion's
                        // `Sf(Float)` at the back edge (`try_promote_outer_sf`)
                        // — likewise an owner-side unbox the loop never
                        // invalidates. The owner-side liveness cannot see a
                        // block-only use — the owner's own body never touches
                        // the slot — so without it a float carried only
                        // through the block collapses to `S` here and
                        // re-promotes every iteration, and the block's home
                        // reads never see a parked `Sf`. Such slots re-adopt
                        // `Sf`, not `F`: the back-edge `Sf` says the slot is
                        // current on every path around the loop, and an `F`
                        // adoption's "slot stale" fiction would make every
                        // block-passing call site in the body re-box it each
                        // iteration (see `keep_backedge_floats`).
                        let adopt = |i| {
                            (be.is_float_typed(i) && loop_float.contains(&i))
                                || matches!(be.mode(i), LinkMode::F(_))
                        };
                        // Stage-A use propagation: an inlined callee's
                        // raw-f64 read of an owner slot is float-use
                        // evidence the owner-side liveness cannot see (the
                        // owner's own body never touches the slot). A
                        // read-only outer float — a loop-invariant scale
                        // factor the block consumes each iteration — leaves
                        // no store behind, so no `Sf(Float)` placement
                        // survives to the back edge and the placement arm
                        // above has nothing to adopt; the slot would stay
                        // `S` and the block would guard+unbox it every
                        // iteration. Adopt such slots (subtree-read, still
                        // `S` at the back edge) as `Sf` too: the entry
                        // bridge is the same guard+unbox the *first*
                        // iteration paid anyway, hoisted out of the loop,
                        // and the block's reads become home reads. The
                        // signal rides outside `IsUsed` and this is its
                        // only consumer, so the long-tuned `use_float` /
                        // `F`-adoption policies are untouched (folding it
                        // into the type lattice measurably regressed
                        // so_mandelbrot, whose `for` bodies are blocks).
                        let subtree_read: std::collections::HashSet<SlotId> =
                            liveness.subtree_float_reads().collect();
                        let adopt_sf = |i| {
                            matches!(be.mode(i), LinkMode::Sf(_, SfGuarded::Float))
                                || (subtree_read.contains(&i)
                                    && matches!(be.mode(i), LinkMode::S(_)))
                        };
                        // Stage 1'': adopt a back-edge *home* view as
                        // `F(home)`, so a deferral the loop's subtree
                        // established holds across the whole loop. See
                        // `keep_backedge_floats`.
                        //
                        // The id must be one the home ledger issued —
                        // `h >= PHYS_FPR_POOL` says only "spill-resident",
                        // and the ordinary allocator spills into the same
                        // file whenever the pool runs out. Adopting one of
                        // *those* force-binds a loop-carried float to a
                        // spill id at the head, which is exactly the
                        // speculative promotion the arms below refuse
                        // under pressure (`try_set_new_F`); with
                        // `stress-spill-pool` (a pool of 2) it mislabeled
                        // ordinary floats and miscompiled nested float
                        // loops — so_mandelbrot's own, among others.
                        // Innermost frame: `keep_backedge_floats` is
                        // innermost-only (see above).
                        let homes = self
                            .outer_pos(0)
                            .map(|pos| self.spill_home_ids_at(pos))
                            .unwrap_or_default();
                        let adopt_deferred = move |i| match be.mode(i) {
                            LinkMode::Sf(h, SfGuarded::Float) | LinkMode::F(h)
                                if h.0 >= crate::codegen::PHYS_FPR_POOL
                                    && homes.contains(&h.0) =>
                            {
                                Some(h)
                            }
                            _ => None,
                        };
                        target.keep_backedge_floats(adopt, adopt_sf, adopt_deferred, promotable);
                    }
                }
            }
            #[cfg(feature = "jit-debug")]
            eprintln!("  target:  {:?}\n", target.slot_state());

            // Stage-C loop adoption: the outer-frame slots the loop's
            // inlined subtree reads as raw f64s (marks read off the back
            // edge's state, vetoes from the fixpoint above) adopt a
            // spill-homed `Sf(Float)` on the live loop-entry state — the
            // same claim a stage-2 dominating store creates, established
            // instead by an init on every entry edge below (chain load,
            // Float guard, unbox, store to the home). The claim's scope
            // is handled by the joins themselves: a path bypassing the
            // head meets it back to `S`.
            let outer_inits = if let Some(be_state) = self.loop_backedge(bbid).cloned() {
                self.adopt_outer_loop_views(bbid, &mut target, &entries, &be_state, &target_incoming)
            } else {
                vec![]
            };

            // `bridge` adds `+1` to the deopt resume PC so it lands on
            // the first body instruction (skipping LoopStart, which
            // would re-enter the JIT and infinite-loop). Pass `pc` (=
            // LoopStart's PC) directly; an extra `+1` here would push
            // the deopt resume past the fused BinCmp into the bare
            // CondBr, which then reads a stale `%dst` — see #480.
            self.gen_bridges_for_branches(
                &target,
                entries,
                bbid,
                pc,
                &outer_inits,
                hoist_version_guard,
                &hoisted_guards,
            );
            self.new_backedge(target.frames_cloned(), bbid);

            Some(target)
        } else {
            #[cfg(feature = "jit-debug")]
            eprintln!("\n===gen_merge {bbid:?}");

            let target = AbstractState::join_entries(&entries);
            self.gen_bridges_for_branches(&target, entries, bbid, pc, &[], false, &[]);

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
        target: &AbstractState,
        entries: Vec<BranchEntry>,
        bbid: BasicBlockId,
        pc: BytecodePtr,
        outer_inits: &[(Vec<SpecializedId>, usize, SlotId, OuterFprHome)],
        hoist_version_guard: bool,
        hoisted_guards: &[(SlotId, Guarded)],
    ) {
        let target = target.frames_cloned();
        #[cfg(feature = "jit-debug")]
        eprintln!("  bridge to:{bbid:?} target:{target:?}");
        for BranchEntry {
            src_bb,
            mut state,
            mode,
            ..
        } in entries
        {
            #[cfg(feature = "jit-debug")]
            eprintln!("    {mode:?} src:{src_bb:?}");

            let mut ir = AsmIr::new(self);
            // Loop-invariant guard hoisting (see `incoming_context`): a
            // miss resumes at the loop head's first body instruction with
            // this entry's write-back, and after a few misses recompiles
            // the unit without hoisting in this iseq.
            for &(slot, g) in hoisted_guards {
                if state.mode(slot) != LinkMode::S(Guarded::Value) {
                    continue;
                }
                let deopt = ir.new_recompile_deopt_with_pc(
                    &state,
                    pc + 1,
                    RecompileReason::LoopHoistGuardFailed,
                    self.loop_hoist_recompile_target(),
                );
                state.load(&mut ir, slot, GP::Rdi);
                state.guard_class(&mut ir, slot, GP::Rdi, hoist_cached_class(g), deopt);
            }
            // Stage-C loop adoption: establish each adopted outer view on
            // this entry edge — load the owner's boxed slot through the
            // chain, guard it a Float (a miss deopts to the loop head's
            // first body instruction with this entry's write-back, like
            // any entry-bridge guard), and unbox into the spill home.
            for (ids, extra, slot, home) in outer_inits {
                ir.push(AsmInst::LoadDynVarSpecialized {
                    offset: DynVarOffset::Hint {
                        ids: ids.clone(),
                        extra: *extra,
                    },
                    reg: *slot,
                });
                ir.reg_move(GP::Rax, GP::Rdi);
                let deopt = ir.new_deopt_with_pc(&state, pc + 1);
                ir.push(AsmInst::GuardFloatToOuterHomeF {
                    home: home.clone(),
                    deopt,
                });
            }
            if hoist_version_guard && !state.class_version_guard() {
                // See `incoming_context`: a miss resumes at the loop head's
                // first body instruction, like the other entry guards.
                let deopt = ir.new_deopt_with_pc(&state, pc + 1);
                ir.push(match self.jit_type() {
                    JitType::Specialized { idx, .. } => {
                        AsmInst::GuardClassVersionSpecialized { idx: *idx, deopt }
                    }
                    _ => AsmInst::GuardClassVersion {
                        position: self.position(),
                        with_recovery: true,
                        deopt,
                    },
                });
            }
            state.gen_bridge_all(&mut ir, &target, pc, &self.chain_surrender_table());
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

fn hoist_cached_class(g: Guarded) -> CachedClass {
    match g {
        Guarded::Fixnum => CachedClass::INTEGER,
        Guarded::Class(class) => CachedClass::from_class(class),
        Guarded::Float | Guarded::Value | Guarded::NilOr(_) => unreachable!(),
    }
}

impl<'a> JitContext<'a> {
    ///
    /// The slots of this frame worth guarding on the forward entries of a
    /// loop head instead of in the body (loop-invariant guard hoisting):
    ///
    /// - the head's join leaves the slot `S(Value)`, but the back edge
    ///   carries it as a `Fixnum` or a non-nil, non-boolean class — every
    ///   path around the loop guarded it to that type, or wrote one;
    /// - the loop reads it (a guard on a slot it only overwrites buys
    ///   nothing and could only miss);
    /// - every forward entry has it either untyped (`S(Value)`, guarded on
    ///   that entry) or already of that type.
    ///
    /// Only inline entries are bridged this way, like the class-version
    /// guard hoisting. Floats are left to the loop-entry float adoption.
    ///
    fn loop_hoist_guards(
        &self,
        target: &AbstractState,
        entries: &[BranchEntry],
        liveness: &Liveness,
        backedge: &AbstractState,
    ) -> Vec<(SlotId, Guarded)> {
        let block_param = self.iseq().block_param_slot();
        if self.loop_hoist_disabled.contains(&self.iseq_id())
            || entries
                .iter()
                .any(|e| matches!(e.mode, BranchMode::Side { .. }))
        {
            return vec![];
        }
        (1..=self.local_num())
            .map(|i| SlotId(i as u16))
            .filter(|slot| Some(*slot) != block_param)
            .filter(|slot| target.mode(*slot) == LinkMode::S(Guarded::Value))
            .filter(|slot| liveness.is_used(*slot))
            .filter_map(|slot| {
                let g = match backedge.mode(slot) {
                    LinkMode::S(g @ Guarded::Fixnum) => g,
                    LinkMode::S(g @ Guarded::Class(c))
                        if c != NIL_CLASS
                            && c != TRUE_CLASS
                            && c != FALSE_CLASS
                            && c != BOOL_CLASS =>
                    {
                        g
                    }
                    _ => return None,
                };
                let mut needs_guard = false;
                for e in entries {
                    match e.state.mode(slot) {
                        LinkMode::S(Guarded::Value) => needs_guard = true,
                        LinkMode::S(eg) if eg == g => {}
                        LinkMode::C(v) if Guarded::from_concrete_value(v) == g => {}
                        _ => return None,
                    }
                }
                needs_guard.then_some((slot, g))
            })
            .collect()
    }

    fn loop_hoist_recompile_target(&self) -> RecompileTarget {
        match self.jit_type() {
            JitType::Specialized { idx, .. } => RecompileTarget::Specialized(*idx),
            _ => RecompileTarget::Whole(self.position()),
        }
    }
}
