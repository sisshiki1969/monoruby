use super::*;

///
/// What one analysis walk over a loop's blocks learned, per loop head in
/// the walked range: the head of the walked loop itself and every loop
/// nested inside it.
///
struct LoopWalk {
    /// Per head: the liveness of its loop (joined at the loop's back
    /// edges and at every leaving path inside it).
    liveness: indexmap::IndexMap<BasicBlockId, Liveness>,
    /// Per head: the join of the states arriving on its back edges, if
    /// any back edge was reached.
    backedge: indexmap::IndexMap<BasicBlockId, Option<AbstractState>>,
    /// Stage-C loop adoption vetoes, for the walked loop's own head.
    vetoes: (bool, Vec<(usize, SlotId)>),
}

impl<'a> JitContext<'a> {
    ///
    /// Compute the back-edge fixpoint of the loop `loop_start..=loop_end`.
    ///
    /// One walk covers the whole nest: an inner loop head met during the
    /// walk does not run a fixpoint of its own (`analyse_basic_block`
    /// merges it with the back edge recorded by the previous walk, if
    /// any), and the back edges of every head in the range are collected
    /// at the end of the walk and recorded in this frame's `loop_info`.
    /// The walk repeats until no head's back edge moved. A nest of depth
    /// d thus costs a handful of linear walks instead of the ~3^d walks
    /// that recursing into each inner head's fixpoint (and then walking
    /// its body again as part of the enclosing body) used to cost — a
    /// 12-deep nest in dewasm-generated code took seconds per compile.
    ///
    /// The recorded inner-head information is what the inner heads' own
    /// merges (`incoming_context`, when the real compile reaches them)
    /// start their fixpoint from, so those converge on their first walk.
    ///
    pub(in crate::codegen::jitgen) fn analyse_backedge_fixpoint(
        &mut self,
        state: AbstractState,
        loop_start: BasicBlockId,
        loop_end: BasicBlockId,
    ) -> JitResult<()> {
        let heads: Vec<BasicBlockId> = self
            .iseq()
            .bb_info
            .loops_within(loop_start, loop_end)
            .into_iter()
            .map(|(begin, _)| begin)
            .collect();
        debug_assert_eq!(heads[0], loop_start);
        // Each walk can carry a back edge one nesting level further out,
        // so the bound grows with the number of heads.
        let limit = 10 * heads.len();
        for x in 0..limit {
            #[cfg(feature = "jit-debug")]
            eprintln!("########## analyse iteration[{x}]");
            let walk = self.analyse_loop(loop_start, loop_end, &heads, state.clone())?;
            // Stage-C loop adoption: park the walk's vetoes for the
            // loop-entry merge (overwritten per iteration). The float-read
            // marks themselves ride the analysed states and are read off
            // the back edge there. Inner heads get theirs from their own
            // fixpoint when the real compile reaches them.
            self.record_loop_adoption_vetoes(loop_start, walk.vetoes);
            let mut changed = false;
            for (head, backedge) in walk.backedge {
                let liveness = walk.liveness[&head].clone();
                if let Some(backedge) = backedge {
                    if let Some(be) = self.loop_backedge(head)
                        && be.equiv(&backedge)
                    {
                        #[cfg(feature = "jit-debug")]
                        eprintln!(
                            "fixed: {x} {head:?} {:?}=={:?}",
                            (be.slot_state()),
                            backedge.slot_state()
                        );
                    } else {
                        #[cfg(feature = "jit-debug")]
                        eprintln!(
                            "analyse_loop[{x}] backedge: ->{head:?} {:?}",
                            backedge.slot_state()
                        );
                        self.add_loop_info(head, liveness, Some(backedge));
                        changed = true;
                    }
                } else {
                    // This walk found no back edge for this head. If an
                    // earlier walk already recorded one, keep it: the
                    // back-edge fixpoint must be *monotone* (a back edge,
                    // once present, cannot disappear). A later walk's
                    // refined entry state can statically prune the
                    // back-edge branch (it decides the loop always exits),
                    // but the real forward compilation still emits that
                    // back edge, whose state then bridges to the loop-head
                    // merge target. Overwriting with `None` would drop the
                    // back-edge join from that target, leaving loop-carried
                    // slots at their stale pre-header concrete types (e.g.
                    // an uninitialized local's `C(nil)`) that the real back
                    // edge (e.g. `S(Fixnum)`) contradicts — tripping the
                    // `bridge` `S -> C` unreachable. Refresh liveness but
                    // retain the recorded back edge.
                    //
                    // A head seen for the first time is a change even so:
                    // the next walk applies its liveness at the merge, as
                    // the walk that recorded it would have.
                    let first = self.loop_info(head).is_none();
                    let be = self.loop_backedge(head).cloned();
                    self.add_loop_info(head, liveness, be);
                    changed |= first;
                }
            }
            if !changed {
                break;
            }
            if x == limit - 1 {
                panic!("not fixed")
            }
        }
        Ok(())
    }

    fn analyse_loop(
        &self,
        loop_start: BasicBlockId,
        loop_end: BasicBlockId,
        heads: &[BasicBlockId],
        mut state: AbstractState,
    ) -> JitResult<LoopWalk> {
        let pc = self.iseq().get_bb_pc(loop_start);
        let mut ctx = JitContext::loop_analysis(self, pc, loop_start);
        let mut liveness: indexmap::IndexMap<BasicBlockId, Liveness> = heads
            .iter()
            .map(|head| (*head, Liveness::new(ctx.total_reg_num())))
            .collect();

        if let Some(backedge) = self.loop_backedge(loop_start) {
            state.join(backedge);
        };
        ctx.branch_continue(loop_start, state);

        for bbid in loop_start..=loop_end {
            ctx.analyse_basic_block(&mut liveness, bbid, bbid == loop_end)?;
        }

        let mut backedge: indexmap::IndexMap<BasicBlockId, Option<AbstractState>> =
            heads.iter().map(|head| (*head, None)).collect();
        for head in heads {
            // Entries left for a head after its block was walked all come
            // from blocks behind it: its back edges.
            if let Some(branches) = ctx.remove_branch(*head) {
                let liveness = liveness.get_mut(head).unwrap();
                let backedge = backedge.get_mut(head).unwrap();
                for BranchEntry { src_bb, state, .. } in branches {
                    liveness.join(&state);
                    assert!(src_bb.unwrap() >= *head);
                    if let Some(backedge) = backedge {
                        backedge.join(&state);
                    } else {
                        *backedge = Some(state);
                    }
                }
            }
        }
        #[cfg(feature = "jit-debug")]
        eprintln!(
            "analyse_end: {loop_start:?}->{loop_end:?} {}",
            backedge[&loop_start]
                .as_ref()
                .map_or("no backedge".to_string(), |b| format!(
                    "{:?}",
                    b.slot_state()
                ))
        );

        // Stage-C loop adoption: what the walk learned that would veto
        // adopting an outer view (the marks themselves ride the states).
        let vetoes = ctx.export_loop_adoption_vetoes(self);

        Ok(LoopWalk {
            liveness,
            backedge,
            vetoes,
        })
    }

    ///
    /// One analysis walk over the block *bbid*. A loop head — the walked
    /// loop's own or an inner one — is merged with the back edge recorded
    /// for it, never with a nested fixpoint of its own (see
    /// `analyse_backedge_fixpoint`). A leaving path joins into the
    /// liveness of every loop that contains it.
    ///
    fn analyse_basic_block(
        &mut self,
        liveness: &mut indexmap::IndexMap<BasicBlockId, Liveness>,
        bbid: BasicBlockId,
        is_last: bool,
    ) -> JitResult<()> {
        let mut ir = AsmIr::new(self);
        let mut state = match self.incoming_context(bbid, true)? {
            Some(bb) => bb,
            None => return Ok(()),
        };

        let BasicBlockInfoEntry { begin, end, .. } = self.iseq().bb_info[bbid];
        for bc_pos in begin..=end {
            state.set_next_sp(self.iseq().get_sp(bc_pos));

            match self.compile_instruction(&mut ir, &mut state, bc_pos)? {
                CompileResult::Continue => {}
                CompileResult::Branch(dest_bb) => {
                    self.new_branch(bc_pos, dest_bb, state);
                    return Ok(());
                }
                CompileResult::Cease => return Ok(()),
                CompileResult::Raise
                | CompileResult::Return(_)
                | CompileResult::Break(_)
                | CompileResult::MethodReturn(_)
                | CompileResult::Recompile(_)
                | CompileResult::Deopt
                | CompileResult::ExitLoop => {
                    for (head, liveness) in liveness.iter_mut() {
                        let (_, loop_end) = self.iseq().bb_info.is_loop_begin(*head).unwrap();
                        if *head <= bbid && bbid <= loop_end {
                            liveness.join(&state);
                        }
                    }
                    return Ok(());
                }
                CompileResult::Abort => {
                    #[cfg(feature = "emit-bc")]
                    self.dump_iseq();
                    unreachable!()
                }
            }
            state.clear_above_next_sp();
        }

        if !is_last {
            self.prepare_next(state, end);
        }

        Ok(())
    }
}
