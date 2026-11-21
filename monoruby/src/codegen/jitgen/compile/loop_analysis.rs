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
    /// The temporaries above the head's stack top are dead at the head,
    /// and the join with a back edge discards them there. The incoming
    /// state can still carry something in them (a loop JIT's entry has
    /// every slot a boxed value), which made the second walk's entry
    /// differ from the first's in those slots alone and so kept the
    /// second walk from being skipped. They are discarded before the
    /// first walk (`doc/fixpoint_engine_plan.md` §5.5).
    ///
    pub(in crate::codegen::jitgen) fn analyse_backedge_fixpoint(
        &mut self,
        mut state: AbstractState,
        loop_start: BasicBlockId,
        loop_end: BasicBlockId,
    ) -> JitResult<()> {
        state.clear_above(self.iseq().get_sp(self.iseq().bb_info[loop_start].begin));
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
/// Block summaries: what a block literal does to the slots of an outer
/// frame, read off the bytecode and the inline caches alone
/// (`doc/fixpoint_engine_plan.md` §5.4). `block_float_accumulators`
/// reads them at a call that hands a block to an inlined callee.
///
use crate::bytecodegen::{BinOpK, UnOpK};

///
/// What one definition puts in a slot of an outer frame.
///
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
struct BlockSummary {
    /// Read as a float operand.
    float_read: Vec<bool>,
    /// What each write puts there.
    defs: Vec<Vec<FloatDef>>,
}

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
    /// the caller, and what it writes is a boxed value of no known class.
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
        use std::collections::HashMap;
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
            defs: vec![vec![]; n],
        };
        for bb in 0..iseq.bb_info.len() {
            let mut tmp: HashMap<SlotId, Tmp> = Default::default();
            // The outer slot an own slot was loaded from, and how many
            // times this basic block had written that outer slot by then.
            let mut src: HashMap<SlotId, (SlotId, usize)> = Default::default();
            // What this basic block (certainly) left in an outer slot.
            let mut outer_now: HashMap<SlotId, FloatDef> = Default::default();
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

impl<'a> JitContext<'a> {
    ///
    /// The locals of the current frame that the block literal *block*
    /// accumulates floats into: slots the block reads as a float and
    /// whose every write in the block (nested blocks included) is a
    /// float. Used at the call that hands *block* to an inlined callee,
    /// to give a float literal in such a slot a float home before the
    /// call (see `method_call.rs`). Only a profitability hint: whatever
    /// the block really does to the slot, the claim the call site makes
    /// is the one a computed float already makes there.
    ///
    pub(in crate::codegen::jitgen) fn block_float_accumulators(
        &self,
        block: ISeqId,
    ) -> Vec<SlotId> {
        let n = self.iseq().total_reg_num();
        let sum = self.block_summary(block, 1, n);
        (1..n)
            .filter(|&i| {
                sum.float_read[i]
                    && !sum.defs[i].is_empty()
                    && sum.defs[i].iter().all(|d| *d == FloatDef::Float)
            })
            .map(|i| SlotId(i as u16))
            .collect()
    }
}
