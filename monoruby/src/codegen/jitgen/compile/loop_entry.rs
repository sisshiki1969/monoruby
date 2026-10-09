//!
//! Loop-entry type seeding.
//!
//! A loop JIT is entered from the interpreter in the middle of a method, so
//! it knows nothing about the frame it lands in: every slot starts as
//! `S(Value)`, and the loop head's merge (`Value ⊔ T = Value`) keeps even a
//! loop-invariant local untyped, which then costs a type check per
//! iteration.
//!
//! The method JIT never has that problem: it reaches the same loop head
//! from the method entry, and the forward entries carry whatever the code
//! before the loop established (`i = 0`, `a = []`, a guarded call result).
//! So the loop JIT asks the same question the method JIT would: it walks
//! the method from its entry in analysis mode, joins the states that arrive
//! at each loop head along its forward entries, and takes the slot types
//! found there as *predictions* ([`JitContext::loop_entry_seeds`]; one walk
//! per method and `self` class, cached by `Codegen`). Each predicted slot
//! the loop does not overwrite before reading it is guarded once at the
//! loop entry; a miss takes a counter-gated recompile exit
//! (`RecompileReason::LoopEntryGuardFailed`) that resumes the interpreter
//! after the `LoopStart`, and the recompile drops the slots that missed
//! ([`LoopSeedRecord::note_entry_miss`]).
//!
//! The prediction is never trusted on its own: the guards are what make
//! the seeded types facts, so the analysis may speculate as freely as the
//! method JIT does (inline caches, typed ivar loads, constant folds) — a
//! wrong guess costs one guard miss, not a wrong answer.
//!
use super::*;
use crate::codegen::jitgen::state::Guarded;
use std::rc::Rc;

/// Walking more bytecode than this is not worth it: a loop head further in
/// is usually the tail of a long straight-line script (a top-level loop),
/// which the method JIT never compiles either.
const MAX_PREFIX_BYTECODES: isize = 4000;

///
/// What the compile of one loop JIT knows about its entry from outside the
/// analysis.
///
#[derive(Debug, Default, Clone)]
pub(in crate::codegen) struct LoopSeedInput {
    /// The type of each slot of the frame entering the loop right now
    /// (`None` for a recompile not triggered from the loop's own frame).
    /// A prediction the current value contradicts would miss on this very
    /// entry, so it is dropped.
    live: Option<LiveTypes>,
    /// Slots whose entry guard missed in an earlier compile of this loop.
    veto: Vec<SlotId>,
    /// Seeding is off for this loop altogether.
    disabled: bool,
    /// The predictions of an earlier walk of this iseq, if any.
    predictions: Option<Rc<LoopPredictions>>,
}

///
/// The type of each slot of a frame (`None`: the slot is empty).
///
#[derive(Debug, Clone)]
pub(in crate::codegen) struct LiveTypes(Vec<Option<Guarded>>);

impl LiveTypes {
    ///
    /// The types of the first `slots` registers of *lfp* (`self` and the
    /// locals), read while the loop is being entered.
    ///
    pub(in crate::codegen) fn new(lfp: Lfp, slots: usize) -> Self {
        Self(
            (0..slots)
                .map(|i| {
                    lfp.register(SlotId(i as u16))
                        .map(Guarded::from_concrete_value)
                })
                .collect(),
        )
    }

    fn get(&self, slot: SlotId) -> Option<Guarded> {
        self.0.get(slot.0 as usize).copied().flatten()
    }
}

///
/// What a loop compile hands back: the slots it guarded at its entry (and
/// the types guarded), and the predictions of the walk it made, if it
/// made one.
///
#[derive(Debug, Default, Clone)]
pub(in crate::codegen) struct LoopSeeded {
    pub(super) seeds: Vec<(SlotId, Guarded)>,
    pub(super) predictions: Option<Rc<LoopPredictions>>,
}

impl LoopSeeded {
    pub(in crate::codegen) fn take_predictions(&mut self) -> Option<Rc<LoopPredictions>> {
        self.predictions.take()
    }
}

///
/// The typed slots at each loop head of an iseq, as the method JIT would
/// carry them in along the forward entries ([`JitContext::loop_entry_seeds`]).
/// Computed by one walk of the iseq and cached per (iseq, `self` class), so
/// the several loops of one method share it.
///
#[derive(Debug, Default)]
pub(in crate::codegen) struct LoopPredictions(HashMap<BasicBlockId, Vec<(SlotId, Guarded)>>);

///
/// The entry seeding of one loop (per iseq and loop head) across its
/// compiles.
///
#[derive(Debug, Default)]
pub(in crate::codegen) struct LoopSeedRecord {
    /// What the current compile guards at the entry.
    seeded: LoopSeeded,
    veto: Vec<SlotId>,
    disabled: bool,
}

impl LoopSeedRecord {
    ///
    /// An entry guard of the current compile missed often enough to
    /// recompile. Veto the seeded slots the frame now contradicts; if
    /// none does (or the frame is not at hand), the miss cannot be pinned
    /// on a slot and seeding is switched off for this loop.
    ///
    pub(in crate::codegen) fn note_entry_miss(&mut self, live: Option<&LiveTypes>) {
        let mut found = false;
        if let Some(live) = live {
            for &(slot, g) in &self.seeded.seeds {
                if live.get(slot) != Some(g) {
                    self.veto.push(slot);
                    found = true;
                }
            }
        }
        if !found {
            self.disabled = true;
        }
        self.seeded.seeds.clear();
    }

    pub(in crate::codegen) fn input(
        &self,
        live: Option<LiveTypes>,
        predictions: Option<Rc<LoopPredictions>>,
    ) -> LoopSeedInput {
        LoopSeedInput {
            live,
            veto: self.veto.clone(),
            disabled: self.disabled,
            predictions,
        }
    }

    pub(in crate::codegen) fn set_seeded(&mut self, seeded: LoopSeeded) {
        self.seeded = seeded;
    }
}

impl<'a> JitContext<'a> {
    ///
    /// Predict the types of the slots entering the loop headed at
    /// *loop_start* (this frame's loop JIT) from the method's own code
    /// before it, and keep those worth an entry guard: typed (`Fixnum`,
    /// `Float`, or a non-nil class), not overwritten by the loop before a
    /// read, not vetoed, and agreeing with the frame as it is entering now.
    ///
    /// Also returns the predictions of a fresh walk, if one was needed,
    /// for the caller to cache.
    ///
    pub(in crate::codegen::jitgen) fn loop_entry_seeds(
        &self,
        loop_start: BasicBlockId,
    ) -> (Vec<(SlotId, Guarded)>, Option<Rc<LoopPredictions>>) {
        let Some(input) = self.loop_seed_input.as_ref() else {
            return (vec![], None);
        };
        if input.disabled {
            return (vec![], None);
        }
        let iseq = self.iseq();
        if iseq.bb_info[loop_start].begin - BcIndex::default() > MAX_PREFIX_BYTECODES {
            return (vec![], None);
        }
        let mut fresh = None;
        let cached = input
            .predictions
            .as_ref()
            .and_then(|p| p.0.get(&loop_start).cloned());
        let predicted = match cached {
            Some(predicted) => predicted,
            None => {
                let p = Rc::new(self.predict_loop_entries());
                #[cfg(feature = "jit-log")]
                eprintln!(
                    "    loop entry predictions: {:?}",
                    p.0.keys().collect::<Vec<_>>()
                );
                let predicted = p.0.get(&loop_start).cloned();
                fresh = Some(p);
                match predicted {
                    Some(predicted) => predicted,
                    None => return (vec![], fresh),
                }
            }
        };

        let seeds: Vec<_> = predicted
            .into_iter()
            .filter(|(slot, g)| {
                !input.veto.contains(slot)
                    && input
                        .live
                        .as_ref()
                        .is_none_or(|live| live.get(*slot) == Some(*g))
            })
            .collect();
        (seeds, fresh)
    }

    ///
    /// One analysis walk of this frame's iseq *as a method*, from its entry
    /// on, recording at each loop head it reaches the typed slots of the
    /// join of the states arriving there along the forward entries — what
    /// the method JIT would carry into that loop. Loop heads met on the way
    /// run their own back-edge fixpoint, as in the method JIT, so the
    /// entries of a nested loop come from its enclosing loop's fixpoint.
    ///
    /// The walk stops at the first block it cannot analyse, and past
    /// [`MAX_PREFIX_BYTECODES`].
    ///
    fn predict_loop_entries(&self) -> LoopPredictions {
        let mut predictions = LoopPredictions::default();
        let mut ctx = self.method_prefix_analysis();
        let state = AbstractState::new(&ctx);
        let entry = BasicBlockId::new(0);
        ctx.branch_continue(entry, state);
        let block_param = self.iseq().block_param_slot();
        let last = BasicBlockId::new(self.iseq().bb_info.len() - 1);
        for bbid in entry..=last {
            if self.iseq().bb_info[bbid].begin - BcIndex::default() > MAX_PREFIX_BYTECODES {
                break;
            }
            if self.iseq().bb_info.is_loop_begin(bbid).is_some()
                && let Some(entries) = ctx.branch_entries(bbid)
                && !entries.is_empty()
            {
                let joined = AbstractState::join_entries(entries);
                let typed = (1..=self.local_num())
                    .map(|i| SlotId(i as u16))
                    .filter(|slot| Some(*slot) != block_param)
                    .filter_map(|slot| {
                        let g = match joined.mode(slot) {
                            LinkMode::S(g) => g,
                            LinkMode::F(_) => Guarded::Float,
                            LinkMode::Sf(_, sf) => sf.into(),
                            LinkMode::C(v) => Guarded::from_concrete_value(v),
                            LinkMode::V | LinkMode::None | LinkMode::MaybeNone => return None,
                        };
                        // `NilOr` has no single class to guard, and `Value`
                        // says nothing. A local still `nil` at the head is
                        // usually one the loop assigns: once an enclosing
                        // loop runs in the interpreter, a later entry finds
                        // it set and misses.
                        matches!(g, Guarded::Fixnum | Guarded::Float | Guarded::Class(_))
                            .then_some((slot, g))
                            .filter(|_| g != Guarded::Class(NIL_CLASS))
                    })
                    .collect();
                predictions.0.insert(bbid, typed);
            }
            if ctx.prefix_basic_block(bbid).is_err() {
                break;
            }
        }
        // A slot a loop overwrites before any read is discarded at its head
        // anyway (`liveness_analysis`): its guard would buy nothing and
        // could only miss. The liveness is the one the walk's own merge at
        // that head computed.
        for (head, typed) in predictions.0.iter_mut() {
            match ctx.loop_info(*head) {
                Some((liveness, _)) => typed.retain(|(slot, _)| !liveness.is_killed(*slot)),
                None => typed.clear(),
            }
        }
        predictions
    }

    ///
    /// One analysis walk over the block *bbid* of the method prefix:
    /// [`Self::analyse_basic_block`] without the liveness bookkeeping.
    /// Loop heads met on the way run their own back-edge fixpoint, as in
    /// the method JIT.
    ///
    fn prefix_basic_block(&mut self, bbid: BasicBlockId) -> JitResult<()> {
        let mut ir = AsmIr::new(self);
        let mut state = match self.incoming_context(bbid, false)? {
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
                CompileResult::Cease
                | CompileResult::Raise
                | CompileResult::Return(_)
                | CompileResult::Break(_)
                | CompileResult::MethodReturn(_)
                | CompileResult::Recompile(_)
                | CompileResult::Deopt
                | CompileResult::ExitLoop => return Ok(()),
                CompileResult::Abort => return Err(CompileError),
            }
            state.clear_above_next_sp();
        }

        self.prepare_next(state, end);
        Ok(())
    }
}
