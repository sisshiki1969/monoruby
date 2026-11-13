use super::*;

///
/// What one analysis walk over a loop's blocks learned, per loop head in
/// the walked range: the head of the walked loop itself and every loop
/// nested inside it.
///
#[derive(Clone, PartialEq)]
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

///
/// The `jit-log` statistics of one back-edge fixpoint
/// (`crate::codegen::jit_stats`'s `FX_*` counters;
/// `doc/fixpoint_engine_plan.md` step B). Nothing here feeds back into
/// the compile.
///
#[cfg(feature = "jit-log")]
#[derive(Default)]
struct FixpointStats {
    walks: usize,
}

#[cfg(feature = "jit-log")]
impl FixpointStats {
    fn start() -> Self {
        crate::codegen::jit_stats::bump(&crate::codegen::jit_stats::FX_FIXPOINTS);
        Self::default()
    }

    /// Time one walk; only a walk no other walk encloses adds to the
    /// total, so nested fixpoints are not counted twice.
    fn time_walk<T>(&mut self, walk: impl FnOnce() -> T) -> T {
        use crate::codegen::jit_stats::*;
        use std::sync::atomic::Ordering::Relaxed;
        self.walks += 1;
        bump(&FX_WALKS);
        let depth = FX_DEPTH.fetch_add(1, Relaxed);
        let start = std::time::Instant::now();
        let res = walk();
        let elapsed = start.elapsed().as_nanos() as usize;
        FX_DEPTH.fetch_sub(1, Relaxed);
        if depth == 0 {
            FX_WALK_TOP_NS.fetch_add(elapsed, Relaxed);
        }
        res
    }

    /// A walk was not run: it could only have repeated the previous one.
    fn skipped(&self) {
        crate::codegen::jit_stats::bump(&crate::codegen::jit_stats::FX_ENTRY_REPEATS);
    }

    /// A head's recorded back edge `old` (if any) is being replaced by `new`.
    fn note_record(old: Option<&AbstractState>, new: &AbstractState) {
        use crate::codegen::jit_stats::*;
        bump(match old.map(|old| new.climb_direction(old)) {
            None => &FX_BE_FIRST,
            Some(ClimbDirection::Rise) => &FX_BE_RISE,
            Some(ClimbDirection::Descent) => &FX_BE_DESCENT,
            Some(ClimbDirection::Same | ClimbDirection::Incomparable) => &FX_BE_INCOMPARABLE,
        });
    }

    fn converged(&self) {
        use crate::codegen::jit_stats::*;
        use std::sync::atomic::Ordering::Relaxed;
        bump(&FX_CONVERGED_IN[self.walks.clamp(1, 5) - 1]);
        FX_MAX_WALKS.fetch_max(self.walks, Relaxed);
    }

    fn gave_up(&self) {
        crate::codegen::jit_stats::bump(&crate::codegen::jit_stats::FX_GAVE_UP);
    }
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
    /// A walk reads its entry state (the incoming state joined with the
    /// back edge recorded for *loop_start*), the inner heads' recorded
    /// loop info and the outer frames; nothing else it reads changes from
    /// one walk to the next. So when a walk moved neither an inner head's
    /// info nor an outer frame, and the next entry equals its entry, the
    /// next walk could only repeat it — record nothing new — and is not
    /// run: the fixpoint has converged (`doc/fixpoint_engine_plan.md`
    /// step D). Debug builds run it anyway and check that it does repeat.
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
        // The bound is the length of the chain the recorded back edges can
        // climb: per head, every slot of the chain a few steps
        // (`AbstractState::backedge_chain_len`). It is not a small
        // constant per head — a body that copies along a chain of locals
        // (`a0 = a1; a1 = a2; ...`, a delay line) moves a type one local
        // further per walk, so it needs as many walks as it has such
        // locals, and a bound of ten walks used to abort the process on
        // a ten-tap one (`doc/fixpoint_engine_plan.md` §2.3).
        let limit = heads.len() * state.backedge_chain_len();
        #[cfg(feature = "jit-log")]
        let mut stats = FixpointStats::start();
        // The previous walk's entry, kept only when that walk moved
        // nothing else a walk reads (and, in debug builds, what it found).
        let mut repeatable: Option<(AbstractState, Option<LoopWalk>)> = None;
        for x in 0..limit {
            let _ = x;
            let (ctx, entry) = self.loop_walk_entry(loop_start, state.clone());
            if let Some((prev_entry, _prev_walk)) = &repeatable
                && *prev_entry == entry
            {
                #[cfg(debug_assertions)]
                {
                    let walk = self.analyse_loop(ctx, loop_start, loop_end, &heads, entry);
                    assert!(
                        walk.ok().as_ref() == _prev_walk.as_ref(),
                        "a walk from a repeated entry did not repeat: {loop_start:?}..={loop_end:?}"
                    );
                }
                #[cfg(feature = "jit-log")]
                {
                    stats.skipped();
                    stats.converged();
                }
                return Ok(());
            }
            #[cfg(feature = "jit-debug")]
            eprintln!("########## analyse iteration[{x}]");
            #[cfg(not(feature = "jit-log"))]
            let walk = self.analyse_loop(ctx, loop_start, loop_end, &heads, entry.clone())?;
            #[cfg(feature = "jit-log")]
            let walk = stats.time_walk(|| {
                self.analyse_loop(ctx, loop_start, loop_end, &heads, entry.clone())
            })?;
            let prev_walk = if cfg!(debug_assertions) {
                Some(walk.clone())
            } else {
                None
            };
            // Whether this walk moved anything the next walk reads besides
            // its entry: an outer frame's widening, or an inner head's info.
            let mut moved = !walk.vetoes.1.is_empty();
            // The walk ran on a throwaway clone, so every widen it
            // reported reached the *clone's* parked outer frames: a
            // block handed out of the unit inside the loop body
            // (`unbox_to_S_for_outgoing_block`) widens a suspended
            // caller's slot in the live chain and in the parked copy the
            // caller resumes from. The chain's widening survives in the
            // back-edge join the real compile starts from, so when the
            // real compile reaches the same hand-out the slot is already
            // `S` and nothing is reported again — and the caller would
            // resume from an un-widened parked copy, believing its
            // register still held a value the block overwrote. Replay the
            // walk's log on our own parked frames (the clone starts with
            // an empty log, so this is exactly the walk's delta).
            for (pos, slot) in walk.vetoes.1.clone() {
                self.widen_outer_at_pos(pos, slot);
            }
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
                    #[cfg(feature = "fixpoint-join")]
                    let backedge = self.accumulate_backedge(loop_start, head, backedge);
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
                        #[cfg(feature = "jit-log")]
                        FixpointStats::note_record(self.loop_backedge(head), &backedge);
                        moved |= head != loop_start;
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
                    // The walk reads an inner head's liveness too.
                    moved |= head != loop_start
                        && self.loop_info(head).is_none_or(|(l, _)| *l != liveness);
                    let be = self.loop_backedge(head).cloned();
                    self.add_loop_info(head, liveness, be);
                    changed |= first;
                }
            }
            if !changed {
                #[cfg(feature = "jit-log")]
                stats.converged();
                return Ok(());
            }
            repeatable = (!moved).then_some((entry, prev_walk));
        }
        #[cfg(feature = "jit-log")]
        stats.gave_up();
        // Past the chain's length the walks are not climbing it: give this
        // compile up rather than the process. The unit is abandoned like
        // any other failed compile and its code keeps running in the
        // interpreter.
        #[cfg(any(feature = "jit-log", feature = "jit-debug"))]
        eprintln!(
            "<== back-edge fixpoint of {loop_start:?}..={loop_end:?} not reached in {limit} walks"
        );
        Err(CompileError)
    }

    ///
    /// The back edge to record for *head* when a walk found *new*: its
    /// join with the back edge recorded before, if any, so that the
    /// recorded back edges only ever climb (`doc/fixpoint_engine_plan.md`
    /// step A'). A walk can find a narrower back edge than the previous
    /// one — a loop head's float adoption narrows below `incoming ⊔
    /// backedge` (§2.4) — and recording it as it is made the sequence
    /// rise and fall.
    ///
    /// The two sides must carry the same elided objects for the join: one
    /// not on both is materialized, as `analyse_loop` does for the walk's
    /// entry. The join keeps *new*'s placement where the two disagree.
    ///
    #[cfg(feature = "fixpoint-join")]
    fn accumulate_backedge(
        &self,
        loop_start: BasicBlockId,
        head: BasicBlockId,
        mut new: AbstractState,
    ) -> AbstractState {
        let Some(old) = self.loop_backedge(head) else {
            return new;
        };
        let mut old = old.clone();
        if new.virtuals() != old.virtuals() {
            let common: Vec<(SlotId, VirtualSrc)> = new
                .virtuals()
                .iter()
                .filter(|v| old.virtuals().contains(v))
                .copied()
                .collect();
            let pc = self.iseq().get_bb_pc(loop_start);
            let mut ctx = JitContext::loop_analysis(self, pc, loop_start);
            let mut scratch = AsmIr::new(&ctx);
            ctx.materialize_except(&mut new, &mut scratch, &common);
            ctx.materialize_except(&mut old, &mut scratch, &common);
        }
        new.join(&old);
        new
    }

    ///
    /// The throwaway context for one walk over the loop at *loop_start*,
    /// and the state the walk starts the loop's head from: *state* (the
    /// state arriving from before the loop) joined with the back edge
    /// recorded so far.
    ///
    fn loop_walk_entry(
        &self,
        loop_start: BasicBlockId,
        mut state: AbstractState,
    ) -> (JitContext<'a>, AbstractState) {
        let pc = self.iseq().get_bb_pc(loop_start);
        let mut ctx = JitContext::loop_analysis(self, pc, loop_start);
        if let Some(backedge) = self.loop_backedge(loop_start) {
            // An elided object the entry carries but the recorded back
            // edge does not is one the body materializes: analyse the
            // body from a head with it materialized, as the real compile's
            // entries will leave it (`incoming_context`).
            let mut scratch = AsmIr::new(&ctx);
            ctx.materialize_except(&mut state, &mut scratch, backedge.virtuals());
            state.join(backedge);
        };
        // The liveness is the loop's own: what was read or overwritten
        // before the loop (a method JIT's `i = 0`) must not stand in for
        // what the loop does with the slot.
        state.reset_liveness();
        (ctx, state)
    }

    fn analyse_loop(
        &self,
        mut ctx: JitContext<'a>,
        loop_start: BasicBlockId,
        loop_end: BasicBlockId,
        heads: &[BasicBlockId],
        entry: AbstractState,
    ) -> JitResult<LoopWalk> {
        let mut liveness: indexmap::IndexMap<BasicBlockId, Liveness> = heads
            .iter()
            .map(|head| (*head, Liveness::new(ctx.total_reg_num())))
            .collect();
        ctx.branch_continue(loop_start, entry);

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
                let mut states: Vec<AbstractState> = branches
                    .into_iter()
                    .map(|BranchEntry { src_bb, state, .. }| {
                        assert!(src_bb.unwrap() >= *head);
                        state
                    })
                    .collect();
                // The back edges agree on the elided objects they carry
                // (`doc/escape_analysis.md` §3): one not on every back
                // edge is materialized on the others.
                let common: Vec<(SlotId, VirtualSrc)> = states[0]
                    .virtuals()
                    .iter()
                    .filter(|v| states.iter().all(|s| s.virtuals().contains(v)))
                    .copied()
                    .collect();
                let mut scratch = AsmIr::new(&ctx);
                for state in &mut states {
                    ctx.materialize_except(state, &mut scratch, &common);
                }
                for state in states {
                    liveness.join(&state);
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

///
/// `doc/fixpoint_engine_plan.md` §5.4, the recording stage: the float
/// slots a loop head would unbox if the choice were made *before* the
/// back-edge fixpoint, from the bytecode, the inline caches and the
/// incoming state alone, to be compared with the set the loop-entry merge
/// adopts from the fixpoint's back edge (`keep_backedge_floats`). Nothing
/// here feeds back into the compile.
///
#[cfg(feature = "jit-log")]
use crate::bytecodegen::{BinOpK, UnOpK};

///
/// What one definition puts in a slot of the frame a loop is in.
///
#[cfg(feature = "jit-log")]
#[derive(Clone, Copy, PartialEq, Debug)]
enum FloatDef {
    Float,
    Other,
    /// Whatever that slot of the frame holds: a float when the slot is
    /// float-valued around the loop.
    CopyOf(SlotId),
}

///
/// The slots *ir* reads as float operands, by its cache (a `BinOp`,
/// `BinCmp` or `UnOp` whose cache says `Float` for the operand), and the
/// slot it defines as a float: float arithmetic, or a float literal.
///
#[cfg(feature = "jit-log")]
fn float_ops(ir: &TraceIr) -> (Vec<SlotId>, Option<SlotId>) {
    let is_num = |c: CachedClass| c == CachedClass::FLOAT || c == CachedClass::INTEGER;
    let mut reads = vec![];
    let mut def = None;
    match ir {
        TraceIr::BinOp {
            kind,
            dst,
            lhs,
            rhs,
            ic: Some((l, r)),
            ..
        } => {
            if *l == CachedClass::FLOAT {
                reads.push(*lhs);
            }
            if *r == CachedClass::FLOAT {
                reads.push(*rhs);
            }
            let arith = matches!(
                kind,
                BinOpK::Add | BinOpK::Sub | BinOpK::Mul | BinOpK::Div | BinOpK::Rem | BinOpK::Exp
            );
            if arith
                && is_num(*l)
                && is_num(*r)
                && (*l == CachedClass::FLOAT || *r == CachedClass::FLOAT)
            {
                def = *dst;
            }
        }
        TraceIr::BinCmp {
            lhs,
            rhs,
            ic: Some((l, r)),
            ..
        } => {
            if *l == CachedClass::FLOAT {
                reads.push(*lhs);
            }
            if *r == CachedClass::FLOAT {
                reads.push(*rhs);
            }
        }
        TraceIr::UnOp {
            kind,
            _dst,
            src,
            ic: Some(c),
            ..
        } if *c == CachedClass::FLOAT => {
            reads.push(*src);
            if matches!(kind, UnOpK::Neg | UnOpK::Pos) {
                def = Some(*_dst);
            }
        }
        TraceIr::Literal(dst, v) | TraceIr::FrozenLiteral(dst, v) if v.is_float() => {
            def = Some(*dst)
        }
        _ => {}
    }
    (reads, def)
}

///
/// Every slot of its own frame *ir* writes: `TraceIr::defined_slot`, and
/// the destinations that are not plain.
///
#[cfg(feature = "jit-log")]
fn written_slots(ir: &TraceIr, store: &Store) -> Vec<SlotId> {
    let mut written: Vec<SlotId> = ir.defined_slot(store).into_iter().collect();
    match ir {
        TraceIr::ToA { dst, .. }
        | TraceIr::DefinedYield { dst, .. }
        | TraceIr::DefinedConst { dst, .. }
        | TraceIr::DefinedMethod { dst, .. }
        | TraceIr::DefinedSuper { dst, .. }
        | TraceIr::DefinedGvar { dst, .. }
        | TraceIr::DefinedIvar { dst, .. }
        | TraceIr::DefinedCvar { dst, .. } => written.push(*dst),
        TraceIr::ClassDef { dst, .. }
        | TraceIr::ModuleDef { dst, .. }
        | TraceIr::SingletonClassDef { dst, .. }
        | TraceIr::ConcatRegexp(dst, ..) => written.extend(*dst),
        TraceIr::Yield { callid } => written.extend(store[*callid].dst),
        TraceIr::ExpandArray {
            dst: (start, len, rest),
            ..
        } => {
            let len = *len as usize + rest.is_some() as usize;
            written.extend((0..len).map(|i| SlotId(start.0 + i as u16)));
        }
        _ => {}
    }
    written
}

///
/// The iseq of the block literal *ir* passes, for a method call with one.
///
#[cfg(feature = "jit-log")]
fn block_literal(ir: &TraceIr, store: &Store) -> Option<ISeqId> {
    let TraceIr::MethodCall { callid, .. } = ir else {
        return None;
    };
    store[store[*callid].block_fid?].is_iseq()
}

///
/// What a block literal — with the blocks nested in it — may do, each
/// time it runs, to the slots of a frame outside it: the summary a scan
/// of that frame's own instructions uses in place of the block's body,
/// which is another iseq (doc/fixpoint_engine_plan.md §5.4). Indexed by
/// the slots of that frame.
///
/// The block may run any number of times, or not at all, so each entry is
/// a "may": a definition here does not end the life of the value the slot
/// had.
///
#[cfg(feature = "jit-log")]
struct BlockSummary {
    /// Read as a float operand.
    float_read: Vec<bool>,
    /// Read in some basic block of the block before that basic block
    /// writes it.
    exposed: Vec<bool>,
    /// What each write puts there.
    defs: Vec<Vec<FloatDef>>,
}

#[cfg(feature = "jit-log")]
impl BlockSummary {
    /// Add what *other* may do as well.
    fn merge(&mut self, other: &BlockSummary) {
        for i in 0..self.defs.len().min(other.defs.len()) {
            self.float_read[i] |= other.float_read[i];
            self.exposed[i] |= other.exposed[i];
            self.defs[i].extend(&other.defs[i]);
        }
    }

    /// Whether anything here reads or writes the slot.
    fn touches(&self, i: usize) -> bool {
        self.float_read[i] || self.exposed[i] || !self.defs[i].is_empty()
    }
}

#[cfg(feature = "jit-log")]
impl<'a> JitContext<'a> {
    ///
    /// Whether the block literal the call *ir* passes is expected to be
    /// compiled into this unit, as far as the call site alone says: the
    /// call is resolved to a Ruby method that does not take its block as
    /// a `&blk` parameter (the static part of the specialization gate in
    /// `compile/method_call.rs`). A builtin callee runs the block as a
    /// unit of its own.
    ///
    fn block_call_inlines(&self, ir: &TraceIr) -> bool {
        matches!(ir, TraceIr::MethodCall { cache: MethodCache::Cached(e), .. }
            if self.store[e.func_id]
                .is_iseq()
                .is_some_and(|iseq| !self.store[iseq].has_block_arg()))
    }

    ///
    /// The summary of the block literal *block* the call *ir* passes
    /// (`block_summary`). When the call is not expected to compile the
    /// block into this unit, the block runs as a unit of its own, reading
    /// and writing the slots in the frame: its float reads say nothing to
    /// the loop, and what it writes is a boxed value of no known class.
    ///
    fn block_summary_at(
        &self,
        ir: &TraceIr,
        block: ISeqId,
        depth: usize,
        n: usize,
    ) -> BlockSummary {
        let mut sum = self.block_summary(block, depth, n);
        if !self.block_call_inlines(ir) {
            sum.float_read.fill(false);
            for d in sum.defs.iter_mut().flatten() {
                *d = FloatDef::Other;
            }
        }
        sum
    }

    ///
    /// The summary of the block *block*, for the *n* slots of the frame
    /// *depth* levels out from it (1 for a block literal of that frame).
    ///
    /// A basic block of the block is scanned in order, tracing which of
    /// its own slots hold a float, a copy of an outer slot, or something
    /// else; what crosses basic blocks in the block's own slots is not
    /// followed and counts as something else. The inline caches decide
    /// what a float is, so the summary belongs to the moment it is made.
    ///
    fn block_summary(&self, block: ISeqId, depth: usize, n: usize) -> BlockSummary {
        self.region_summary(block, None, depth, n)
    }

    ///
    /// `block_summary` for the basic blocks *region* (inclusive) of
    /// *iseq* alone, or all of them: what a loop in a block does to the
    /// frame *depth* levels out, through the outer chain and through the
    /// block literals it passes.
    ///
    fn region_summary(
        &self,
        block: ISeqId,
        region: Option<(BasicBlockId, BasicBlockId)>,
        depth: usize,
        n: usize,
    ) -> BlockSummary {
        use std::collections::{HashMap, HashSet};
        /// What a slot of the block's own frame holds.
        #[derive(Clone, Copy)]
        enum Tmp {
            Float,
            Other,
            /// Whatever that outer slot holds.
            Outer(SlotId),
        }
        let store = self.store;
        let iseq = &store[block];
        let mut sum = BlockSummary {
            float_read: vec![false; n],
            exposed: vec![false; n],
            defs: vec![vec![]; n],
        };
        let (first, last) = match region {
            Some((first, last)) => (first.index(), last.index()),
            None => (0, iseq.bb_info.len().saturating_sub(1)),
        };
        for bb in first..=last {
            let mut tmp: HashMap<SlotId, Tmp> = Default::default();
            // The outer slot an own slot was loaded from, and how many
            // times this basic block had written that outer slot by then.
            let mut src: HashMap<SlotId, (SlotId, usize)> = Default::default();
            // What this basic block (certainly) left in an outer slot.
            let mut outer_now: HashMap<SlotId, FloatDef> = Default::default();
            // The outer slots this basic block has certainly written.
            let mut certain: HashSet<SlotId> = Default::default();
            // How many times this basic block has (maybe) written an
            // outer slot.
            let mut generation: HashMap<SlotId, usize> = Default::default();
            // An outer slot this basic block wrote other than as a float:
            // the index of that definition, until a float read guards it.
            let mut pending: HashMap<SlotId, usize> = Default::default();
            let BasicBlockInfoEntry { begin, end, .. } = iseq.bb_info[BasicBlockId(bb as u32)];
            for pos in begin..=end {
                let pc = iseq.get_pc(pos);
                if TraceIr::is_handler_only(pc) {
                    continue;
                }
                let ir = TraceIr::from_pc(pc, store);
                let (float_reads, float_def) = float_ops(&ir);
                for r in float_reads {
                    if let Some(&(x, at)) = src.get(&r) {
                        sum.float_read[x.0 as usize] = true;
                        // The read guards the value: a definition this
                        // basic block made of the slot, still there, is a
                        // float on the paths that go on.
                        if generation.get(&x).copied().unwrap_or(0) == at
                            && let Some(idx) = pending.remove(&x)
                        {
                            sum.defs[x.0 as usize][idx] = FloatDef::Float;
                            outer_now.insert(x, FloatDef::Float);
                        }
                    }
                    if !matches!(tmp.get(&r), Some(Tmp::Outer(_))) {
                        tmp.insert(r, Tmp::Float);
                    }
                }
                for w in written_slots(&ir, store) {
                    let from = match ir {
                        TraceIr::Mov(dst, from) if dst == w => Some(from),
                        _ => None,
                    };
                    let (t, s) = if float_def == Some(w) {
                        (Some(Tmp::Float), None)
                    } else if let Some(from) = from {
                        (tmp.get(&from).copied(), src.get(&from).copied())
                    } else {
                        (Some(Tmp::Other), None)
                    };
                    match t {
                        Some(t) => tmp.insert(w, t),
                        None => tmp.remove(&w),
                    };
                    match s {
                        Some(s) => src.insert(w, s),
                        None => src.remove(&w),
                    };
                }
                match &ir {
                    TraceIr::LoadDynVar(dst, DynVar { reg, outer })
                        if *outer == depth && (reg.0 as usize) < n =>
                    {
                        let x = *reg;
                        if !certain.contains(&x) {
                            sum.exposed[x.0 as usize] = true;
                        }
                        tmp.insert(
                            *dst,
                            match outer_now.get(&x) {
                                Some(FloatDef::Float) => Tmp::Float,
                                Some(FloatDef::Other) => Tmp::Other,
                                Some(FloatDef::CopyOf(y)) => Tmp::Outer(*y),
                                None => Tmp::Outer(x),
                            },
                        );
                        src.insert(*dst, (x, generation.get(&x).copied().unwrap_or(0)));
                    }
                    TraceIr::StoreDynVar(DynVar { reg, outer }, from)
                        if *outer == depth && (reg.0 as usize) < n =>
                    {
                        let x = *reg;
                        let def = match tmp.get(from) {
                            Some(Tmp::Float) => FloatDef::Float,
                            Some(Tmp::Outer(y)) => FloatDef::CopyOf(*y),
                            Some(Tmp::Other) | None => FloatDef::Other,
                        };
                        if def == FloatDef::Other {
                            pending.insert(x, sum.defs[x.0 as usize].len());
                        } else {
                            pending.remove(&x);
                        }
                        sum.defs[x.0 as usize].push(def);
                        outer_now.insert(x, def);
                        certain.insert(x);
                        *generation.entry(x).or_default() += 1;
                    }
                    _ => {}
                }
                // A block nested here may run, any number of times.
                if let Some(inner) = block_literal(&ir, store) {
                    let inner = self.block_summary_at(&ir, inner, depth + 1, n);
                    for i in 0..n {
                        let x = SlotId(i as u16);
                        sum.float_read[i] |= inner.float_read[i];
                        if inner.exposed[i] && !certain.contains(&x) {
                            sum.exposed[i] = true;
                        }
                        if !inner.defs[i].is_empty() {
                            sum.defs[i].extend(&inner.defs[i]);
                            outer_now.remove(&x);
                            *generation.entry(x).or_default() += 1;
                        }
                    }
                }
            }
        }
        sum
    }
}

#[cfg(feature = "jit-log")]
impl<'a> JitContext<'a> {
    ///
    /// Per slot, whether it is a candidate of the loop
    /// `loop_start..=loop_end` in the innermost frame, given the state
    /// *incoming* from before the loop, or why not; and per slot whether
    /// it is live at the head (below). A candidate is a slot
    /// that
    /// some instruction of the loop reads as a float operand (a `BinOp`,
    /// `BinCmp` or `UnOp` whose cache says `Float` for it), every entry
    /// can bridge to a float (*promotable*), the loop may read before it
    /// writes it (live at the head by the tables, and read before written
    /// in some basic block of the loop), and that either
    ///
    /// - the loop defines only by float arithmetic (a numeric `BinOp` or
    ///   `UnOp` with a `Float` operand in its cache), float literals, and
    ///   copies of slots that are float by this same rule, or
    /// - the loop does not define, and *incoming* has as a `Float` or as a
    ///   value of no known class (the float reads guard it).
    ///
    /// A block literal passed in the loop counts through its summary
    /// (`block_summary_at`): its reads and writes of the frame's slots
    /// are reads and writes the loop may make at the call. A slot the
    /// frame's blocks touch is no candidate when one of them is opaque to
    /// this compilation (`table_blocks_opaque`), where no summary holds.
    ///
    pub(in crate::codegen::jitgen) fn loop_float_candidates(
        &self,
        loop_start: BasicBlockId,
        loop_end: BasicBlockId,
        incoming: &AbstractState,
        promotable: impl Fn(SlotId) -> bool,
    ) -> (Vec<std::result::Result<(), &'static str>>, Vec<bool>) {
        use FloatDef as Def;
        use std::collections::{HashMap, HashSet};
        let iseq = self.iseq();
        let store = self.store;
        let n = self.total_reg_num();
        let mut defs: Vec<Vec<Def>> = vec![vec![]; n];
        let mut float_use = vec![false; n];
        // Read in some basic block of the loop before that block writes it.
        let mut exposed = vec![false; n];
        let table = self.slot_analysis(self.iseq_id());
        for bb in loop_start..=loop_end {
            // The definitions made so far in this basic block.
            let mut local: HashMap<SlotId, Def> = Default::default();
            // The slots this basic block has certainly written.
            let mut certain: HashSet<SlotId> = Default::default();
            // The slot whose value a slot copied in this basic block holds.
            let mut copy_src: HashMap<SlotId, SlotId> = Default::default();
            // A slot this basic block defined other than as a float, not
            // read since: the indices of those definitions in `defs`
            // (more than one when a block may have written it).
            let mut pending: HashMap<SlotId, Vec<usize>> = Default::default();
            let BasicBlockInfoEntry { begin, end, .. } = iseq.bb_info[bb];
            for pos in begin..=end {
                let pc = iseq.get_pc(pos);
                if TraceIr::is_handler_only(pc) {
                    continue;
                }
                for r in table.reads(pos) {
                    if (r.0 as usize) < n && !certain.contains(r) {
                        exposed[r.0 as usize] = true;
                    }
                }
                let ir = TraceIr::from_pc(pc, store);
                let (float_reads, float_def) = float_ops(&ir);
                // The reads come before the writes. A float read of a copy
                // is a float read of what it copies; a float read guards
                // the value a slot was given in this block (an `Index`
                // read, a call's result), so that definition is a float
                // on the paths that reach the loop's back edge.
                for r in float_reads {
                    for r in std::iter::once(r).chain(copy_src.get(&r).copied()) {
                        if let Some(f) = float_use.get_mut(r.0 as usize) {
                            *f = true;
                        }
                    }
                    if let Some(idxs) = pending.remove(&r) {
                        for idx in idxs {
                            defs[r.0 as usize][idx] = Def::Float;
                        }
                        local.insert(r, Def::Float);
                    }
                }
                // A block literal passed here may run, any number of
                // times, before the call's own result is written.
                if let Some(block) = block_literal(&ir, store) {
                    let sum = self.block_summary_at(&ir, block, 1, n);
                    for i in 0..n {
                        let x = SlotId(i as u16);
                        if sum.float_read[i] {
                            float_use[i] = true;
                            if let Some(y) = copy_src.get(&x) {
                                float_use[y.0 as usize] = true;
                            }
                        }
                        if sum.exposed[i] && !certain.contains(&x) {
                            exposed[i] = true;
                        }
                        if sum.defs[i].is_empty() {
                            continue;
                        }
                        for d in &sum.defs[i] {
                            if *d == Def::Other {
                                pending.entry(x).or_default().push(defs[i].len());
                            }
                            defs[i].push(*d);
                        }
                        // Written or not: whatever the slot holds.
                        local.insert(x, Def::CopyOf(x));
                        copy_src.remove(&x);
                    }
                }
                for w in written_slots(&ir, store) {
                    if w.0 as usize >= n {
                        continue;
                    }
                    copy_src.remove(&w);
                    pending.remove(&w);
                    certain.insert(w);
                    let def = if float_def == Some(w) {
                        Def::Float
                    } else if let TraceIr::Mov(dst, src) = ir
                        && dst == w
                    {
                        copy_src.insert(dst, copy_src.get(&src).copied().unwrap_or(src));
                        local.get(&src).copied().unwrap_or(Def::CopyOf(src))
                    } else {
                        pending.insert(w, vec![defs[w.0 as usize].len()]);
                        Def::Other
                    };
                    local.insert(w, def);
                    defs[w.0 as usize].push(def);
                }
            }
        }
        // The greatest set closed under "every definition is a float".
        let float_valued = |ok: &[bool], s: SlotId| {
            let i = s.0 as usize;
            i < n
                && if defs[i].is_empty() {
                    // Loop-invariant: a Float on entry, or one the float
                    // reads would guard on every iteration (a guard the
                    // merge hoists to the entries; *promotable* checks
                    // that they can take it).
                    incoming.guarded(s).class().is_none_or(|c| c == FLOAT_CLASS)
                } else {
                    ok[i]
                }
        };
        let mut ok: Vec<bool> = defs.iter().map(|d| !d.contains(&Def::Other)).collect();
        loop {
            let mut changed = false;
            for i in 0..n {
                if ok[i]
                    && defs[i]
                        .iter()
                        .any(|d| matches!(d, Def::CopyOf(s) if !float_valued(&ok, *s)))
                {
                    ok[i] = false;
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
        let head_pos = iseq.bb_info[loop_start].begin;
        let blocks_opaque = self.table_blocks_opaque();
        let live: Vec<bool> = (0..n)
            .map(|i| exposed[i] && self.table_live_after(head_pos, SlotId(i as u16)))
            .collect();
        let verdicts = (0..n)
            .map(|i| {
                let s = SlotId(i as u16);
                if i == 0 {
                    Err("self")
                } else if self.table_touched_by_blocks(s) && blocks_opaque {
                    // A block has a handler or a lambda, or a call that
                    // may capture the frame: its summary does not hold.
                    Err("touched by an opaque block")
                } else if !float_use[i] {
                    Err("no float read")
                } else if !float_valued(&ok, s) {
                    Err(if defs[i].is_empty() {
                        "not a Float on entry, by class"
                    } else if defs[i].contains(&Def::Other) {
                        "a non-float def"
                    } else {
                        "a copy of a non-float"
                    })
                } else if !live[i] {
                    Err("dead at the head")
                } else if !promotable(s) {
                    Err("an entry is not float")
                } else {
                    Ok(())
                }
            })
            .collect();
        (verdicts, live)
    }
}

#[cfg(feature = "jit-log")]
impl<'a> JitContext<'a> {
    ///
    /// Compare the floats the loop-entry merge of *head* adopted (`F`, or
    /// `Sf` guarded `Float`, in the innermost frame of *target*) with the
    /// candidates *pre* chosen before the fixpoint
    /// (`loop_float_candidates`), into the `FX54_*` counters. With
    /// `MONORUBY_FX54_LOG` set, each head where the two differ is printed.
    ///
    pub(in crate::codegen::jitgen) fn record_pre_fixpoint_floats(
        &self,
        head: BasicBlockId,
        target: &AbstractState,
        backedge: &state::SlotState,
        (verdicts, live): &(Vec<std::result::Result<(), &'static str>>, Vec<bool>),
    ) {
        let live = |s: SlotId| live[s.0 as usize];
        let pre: Vec<SlotId> = (0..verdicts.len())
            .filter(|&i| verdicts[i].is_ok())
            .map(|i| SlotId(i as u16))
            .collect();
        use crate::codegen::jit_stats::*;
        use std::sync::atomic::Ordering::Relaxed;
        let adopted: Vec<SlotId> = (1..self.total_reg_num())
            .map(|i| SlotId(i as u16))
            .filter(|&s| {
                matches!(
                    target.mode(s),
                    LinkMode::F(_) | LinkMode::Sf(_, state::SfGuarded::Float)
                )
            })
            .collect();
        let both = adopted.iter().filter(|s| pre.contains(s)).count();
        let fix_only: Vec<SlotId> = adopted
            .iter()
            .copied()
            .filter(|s| !pre.contains(s))
            .collect();
        let fix_only_block = fix_only
            .iter()
            .filter(|&&s| self.table_touched_by_blocks(s))
            .count();
        // Not read before written in the loop: an adoption the loop never
        // reads from the head.
        let fix_only_dead = fix_only.iter().filter(|&&s| !live(s)).count();
        let pre_only: Vec<SlotId> = pre
            .iter()
            .copied()
            .filter(|s| !adopted.contains(s))
            .collect();
        bump(&FX54_HEADS);
        if fix_only.is_empty() && pre_only.is_empty() {
            bump(&FX54_SAME);
        }
        FX54_BOTH.fetch_add(both, Relaxed);
        FX54_FIX_ONLY.fetch_add(fix_only.len(), Relaxed);
        FX54_FIX_ONLY_BLOCK.fetch_add(fix_only_block, Relaxed);
        FX54_FIX_ONLY_DEAD.fetch_add(fix_only_dead, Relaxed);
        FX54_PRE_ONLY.fetch_add(pre_only.len(), Relaxed);
        if (!fix_only.is_empty() || !pre_only.is_empty())
            && std::env::var_os("MONORUBY_FX54_LOG").is_some()
        {
            let iseq = self.iseq();
            eprintln!(
                "[fx54] {:?} {} {head:?}: both {both}, fixpoint only {:?}, pre only {:?}",
                self.iseq_id(),
                iseq.name(),
                fix_only
                    .iter()
                    .map(|&s| format!(
                        "{s:?} {:?}{}: {}",
                        target.mode(s),
                        if live(s) { "" } else { " (dead at head)" },
                        verdicts[s.0 as usize].err().unwrap_or("?")
                    ))
                    .collect::<Vec<_>>(),
                pre_only
                    .iter()
                    .map(|&s| format!(
                        "{s:?} {:?} (back edge {:?}){}",
                        target.mode(s),
                        backedge.mode(s),
                        if self.table_touched_by_blocks(s) {
                            ", touched by a block"
                        } else {
                            ""
                        }
                    ))
                    .collect::<Vec<_>>(),
            );
        }
    }
}

#[cfg(feature = "jit-log")]
impl<'a> JitContext<'a> {
    ///
    /// The yield side of `record_pre_fixpoint_floats`: at the loop-entry
    /// merge of *head*, compare the floats the merge holds in the frames
    /// **outside** the loop's own (`F`, or `Sf` guarded `Float`, in
    /// *target*) with a choice made before the fixpoint, for the slots the
    /// loop reaches. A loop reaches an outer frame's slots
    ///
    /// - through the outer chain, when its frame is a block (its own
    ///   `LoadDynVar` / `StoreDynVar`, and the block literals it passes),
    ///   and
    /// - through a `yield` to the block literal its method was given,
    ///   whose summary (`block_summary`) says what that block does to the
    ///   frame that wrote it and to that frame's lexical ancestors.
    ///
    /// The choice is the one `loop_float_candidates` makes for the loop's
    /// own frame, read off the summaries: a slot the loop reads as a
    /// float, may read before it writes, whose every definition in the
    /// loop is a float (or, with none, that comes in as a `Float` or with
    /// no known class), and that every entry can bridge to a float. Into
    /// the `FX54O_*` counters; with `MONORUBY_FX54_LOG` set, each head
    /// where the two differ is printed.
    ///
    pub(in crate::codegen::jitgen) fn record_pre_fixpoint_outer_floats(
        &self,
        head: BasicBlockId,
        loop_start: BasicBlockId,
        loop_end: BasicBlockId,
        target: &AbstractState,
        entries: &[BranchEntry],
        incoming: &AbstractState,
    ) {
        use crate::codegen::jit_stats::*;
        use FloatDef as Def;
        use std::sync::atomic::Ordering::Relaxed;
        let store = self.store;
        let innermost = self.stack_frame_len() - 1;
        if innermost == 0 || target.depth() != innermost + 1 {
            return;
        }
        // What the loop may do to each outer frame, by stack position.
        let mut effects: std::collections::BTreeMap<usize, BlockSummary> = Default::default();
        let mut add = |pos: usize, sum: BlockSummary| match effects.get_mut(&pos) {
            Some(e) => e.merge(&sum),
            None => {
                effects.insert(pos, sum);
            }
        };
        let slots_at = |pos: usize| store[self.frame_iseq_at(pos)].total_reg_num();
        // Through the outer chain.
        let mut depth = 1;
        while let Some(pos) = self.outer_pos(depth) {
            add(
                pos,
                self.region_summary(
                    self.iseq_id(),
                    Some((loop_start, loop_end)),
                    depth,
                    slots_at(pos),
                ),
            );
            depth += 1;
        }
        // Through a `yield` in the loop.
        let iseq = self.iseq();
        let yields = (loop_start..=loop_end).any(|bb| {
            let BasicBlockInfoEntry { begin, end, .. } = iseq.bb_info[bb];
            (begin..=end).any(|pos| {
                let pc = iseq.get_pc(pos);
                !TraceIr::is_handler_only(pc)
                    && matches!(TraceIr::from_pc(pc, store), TraceIr::Yield { .. })
            })
        });
        if yields
            && let Some(info) = self.current_method_given_block()
            && let Some(block) = store[info.block_fid].is_iseq()
        {
            let mut pos = innermost.checked_sub(info.outer);
            let mut depth = 1;
            while let Some(p) = pos {
                add(p, self.block_summary(block, depth, slots_at(p)));
                pos = self.lexical_outer_at(p);
                depth += 1;
            }
        }
        let float_bridgeable = |m: LinkMode| {
            matches!(m, LinkMode::F(_) | LinkMode::S(_) | LinkMode::Sf(_, _))
                || matches!(m, LinkMode::C(v) if v.is_float())
        };
        let (mut both, mut fix_only, mut pre_only) = (vec![], vec![], vec![]);
        for (&pos, sum) in &effects {
            let n = sum.defs.len();
            let opaque = self.unit_liveness(self.frame_iseq_at(pos)).blocks_opaque();
            // The greatest set closed under "every definition is a float".
            let float_valued = |ok: &[bool], i: usize| {
                if sum.defs[i].is_empty() {
                    incoming[pos]
                        .guarded(SlotId(i as u16))
                        .class()
                        .is_none_or(|c| c == FLOAT_CLASS)
                } else {
                    ok[i]
                }
            };
            let mut ok: Vec<bool> = sum.defs.iter().map(|d| !d.contains(&Def::Other)).collect();
            loop {
                let mut changed = false;
                for i in 0..n {
                    if ok[i]
                        && sum.defs[i].iter().any(|d| {
                            matches!(d, Def::CopyOf(s)
                                if (s.0 as usize) >= n || !float_valued(&ok, s.0 as usize))
                        })
                    {
                        ok[i] = false;
                        changed = true;
                    }
                }
                if !changed {
                    break;
                }
            }
            for i in 1..n {
                if !sum.touches(i) {
                    continue;
                }
                let slot = SlotId(i as u16);
                let verdict = if opaque {
                    Err("an opaque block")
                } else if !sum.float_read[i] {
                    Err("no float read")
                } else if !float_valued(&ok, i) {
                    Err(if sum.defs[i].is_empty() {
                        "not a Float on entry, by class"
                    } else {
                        "a non-float def"
                    })
                } else if !sum.exposed[i] {
                    Err("written before read")
                } else if !entries
                    .iter()
                    .all(|e| e.state.depth() > pos && float_bridgeable(e.state[pos].mode(slot)))
                {
                    Err("an entry is not float")
                } else {
                    Ok(())
                };
                let mode = target[pos].mode(slot);
                // A float constant at the head needs no register: no
                // choice to compare.
                if matches!(mode, LinkMode::C(v) if v.is_float()) {
                    continue;
                }
                let adopted = matches!(
                    mode,
                    LinkMode::F(_) | LinkMode::Sf(_, state::SfGuarded::Float)
                );
                match (adopted, verdict) {
                    (true, Ok(())) => both.push((pos, slot)),
                    (true, Err(why)) => fix_only.push(format!("{pos}:{slot:?} {mode:?}: {why}")),
                    (false, Ok(())) => {
                        let written = !sum.defs[i].is_empty();
                        if written {
                            bump(&FX54O_PRE_ONLY_WRITTEN);
                        }
                        pre_only.push(format!(
                            "{pos}:{slot:?} {mode:?} ({}, entry {:?})",
                            if written { "written" } else { "read only" },
                            incoming[pos].mode(slot)
                        ))
                    }
                    (false, Err(_)) => {}
                }
            }
        }
        if effects
            .values()
            .all(|sum| (1..sum.defs.len()).all(|i| !sum.touches(i)))
        {
            return;
        }
        bump(&FX54O_HEADS);
        if fix_only.is_empty() && pre_only.is_empty() {
            bump(&FX54O_SAME);
        }
        FX54O_BOTH.fetch_add(both.len(), Relaxed);
        FX54O_FIX_ONLY.fetch_add(fix_only.len(), Relaxed);
        FX54O_PRE_ONLY.fetch_add(pre_only.len(), Relaxed);
        if (!fix_only.is_empty() || !pre_only.is_empty())
            && std::env::var_os("MONORUBY_FX54_LOG").is_some()
        {
            eprintln!(
                "[fx54-outer] {:?} {} {head:?}: both {}, fixpoint only {fix_only:?}, pre only {pre_only:?}",
                self.iseq_id(),
                iseq.name(),
                both.len(),
            );
        }
    }
}
