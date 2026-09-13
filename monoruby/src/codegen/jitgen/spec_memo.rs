use super::*;
use context::{JitArgumentInfo, JitType, SpecializedId};

///
/// Memo of the specialized calls compiled during analysis walks, shared
/// by every walk of one compilation
/// ([`JitContext::analysis_clone`] hands the same table to each
/// throwaway context).
///
/// A back-edge fixpoint walks its loop repeatedly, re-inlining every
/// callee on each walk, and each inlined callee's own loops run a
/// fixpoint of their own: the nest costs one full callee compile per
/// walk per level, so the work grows as the product of the per-level
/// walk counts. The memo answers a repeat of a (call site, tower) pair
/// it has already compiled with that compile's recorded effect, which
/// cuts the product back to a sum.
///
/// A specialized call is a function from the abstract-frame tower it
/// enters with to the tower it returns, so that is what the table
/// stores: [`Tower`] as both key and value. What does not fit in the
/// tower is the handful of appends the compile makes to the
/// context-wide logs, kept alongside as deltas.
///
/// Sound only with `codegen_mode` off. A replay reproduces the abstract
/// effect and no `AsmIr`, so it stands in for the compile exactly while
/// the compile's emitted code is discarded, which is what an analysis
/// walk does with it.
///
#[derive(Default)]
pub(super) struct SpecMemo {
    table: HashMap<SpecCallSite, Vec<Entry>>,
}

///
/// One recorded compile. `hash` is [`JitContext::tower_hash`] of
/// `entered`, kept so a lookup can reject an entry without building the
/// tower it would be compared against.
///
struct Entry {
    hash: u64,
    entered: Tower,
    returned: Tower,
    effect: Effect,
}

///
/// How many towers one call site keeps. A site whose tower keeps
/// changing is not repeating work the memo can answer, and an unbounded
/// table would hold a whole compilation's abstract states alive.
///
const MAX_TOWERS_PER_SITE: usize = 8;

///
/// Which compile this is: the callee, the arguments it is entered with,
/// and the inlining path it sits on. Everything here is identity, so it
/// is the hash-map key; what varies between two arrivals is the
/// [`Tower`].
///
#[derive(PartialEq, Eq, Hash)]
pub(super) struct SpecCallSite {
    pub(super) iseq_id: ISeqId,
    pub(super) self_class: ClassId,
    pub(super) outer: Option<usize>,
    pub(super) callid: CallSiteId,
    pub(super) bmethod: bool,
    pub(super) args_info: JitArgumentInfo,
    ///
    /// The inlining path this call site is being compiled on. The
    /// recorded tower is stated per chain position, so it answers an
    /// arrival on the same path and no other; keying the table by the
    /// path keeps the per-site entries comparable.
    ///
    pub(super) chain: Vec<ChainStep>,
}

///
/// One frame of the inlining path, by identity alone.
///
#[derive(PartialEq, Eq, Hash)]
pub(super) struct ChainStep {
    pub(super) iseq_id: ISeqId,
    pub(super) self_class: ClassId,
    pub(super) callid: Option<CallSiteId>,
    pub(super) outer: Option<usize>,
    pub(super) specialize_level: usize,
    pub(super) specialized_id: SpecializedId,
}

///
/// The abstract-frame tower at a specialized call site: the live chain,
/// each suspended frame's own view of itself, and the per-path flags
/// that travel with them.
///
/// The `AsmIr` side of each frame is deliberately absent: labels, the
/// instruction stream and the `specialized_methods` list steer code
/// emission only, and a tower is replayed only where no code is
/// emitted.
///
/// Field order is comparison order: the flags reject a mismatched entry
/// before the frames are walked.
///
#[derive(PartialEq)]
pub(super) struct Tower {
    pub(super) flags: CtxFlags,
    pub(super) frames: Vec<FrameState>,
    pub(super) state: MemoChain,
}

///
/// The live chain, compared and hashed at the granularity the compiler
/// consumes (see [`SlotState::memo_eq`]) rather than field by field.
/// A newtype rather than a hand-written `PartialEq` on [`Tower`] and
/// [`FrameState`], so those keep deriving it and a field added to
/// either is compared without anyone remembering to.
///
pub(super) struct MemoChain(pub(super) AbstractState);

/// One suspended frame's parked view, at the same granularity.
pub(super) struct MemoFrame(pub(super) Option<AbstractFrame>);

impl PartialEq for MemoChain {
    fn eq(&self, other: &Self) -> bool {
        self.0.depth() == other.0.depth()
            && (0..self.0.depth()).all(|i| self.0[i].memo_eq(&other.0[i]))
    }
}

impl PartialEq for MemoFrame {
    fn eq(&self, other: &Self) -> bool {
        match (&self.0, &other.0) {
            (None, None) => true,
            (Some(a), Some(b)) => a.memo_eq(b),
            _ => false,
        }
    }
}

///
/// Hash a live chain the way [`MemoChain`] compares it. Free functions
/// rather than methods so [`JitContext::tower_hash`] can digest the
/// live data in place, without the copy building a [`Tower`] would
/// cost on a lookup that is about to miss.
///
pub(super) fn hash_chain<H: std::hash::Hasher>(chain: &AbstractState, h: &mut H) {
    for i in 0..chain.depth() {
        chain[i].memo_hash(h);
    }
}

/// Hash a parked frame view the way [`MemoFrame`] compares it.
pub(super) fn hash_parked<H: std::hash::Hasher>(frame: &Option<AbstractFrame>, h: &mut H) {
    use std::hash::Hash;
    frame.is_some().hash(h);
    if let Some(f) = frame {
        f.memo_hash(h);
    }
}

///
/// One suspended frame's floor of the tower.
///
#[derive(PartialEq)]
pub(super) struct FrameState {
    pub(super) is_not_block: bool,
    pub(super) stack_offset: usize,
    pub(super) base_stack_offset: usize,
    pub(super) spill_home_watermark: usize,
    pub(super) speculation_poisoned: bool,
    pub(super) had_deopt: bool,
    pub(super) generic_yield: bool,
    pub(super) ivar_heap_accessed: bool,
    pub(super) speculated_using_fpr: UsingFpr,
    pub(super) speculated_floats: Vec<(SlotId, FPReg)>,
    pub(super) spill_home_ids: std::collections::HashSet<usize>,
    pub(super) return_context: HashMap<usize, ReturnState>,
    pub(super) jit_type: JitType,
    /// The frame's parked view of itself, which a nested compile widens
    /// (`widen_outer_at_pos`) and drops the capture invariant on
    /// (`unset_outer_no_capture_guard`).
    pub(super) abstract_state: MemoFrame,
}

///
/// The context-wide flags a specialized call both reads and leaves
/// changed. The callee clears the unfrozen-slot proofs on its way out
/// (`traceir_to_asmir`), so these are part of the tower, not just of
/// the key.
///
#[derive(Clone, PartialEq)]
pub(super) struct CtxFlags {
    pub(super) fused_skip: Option<BcIndex>,
    pub(super) in_dispatch_arm: bool,
    pub(super) in_set_guarded_arm: bool,
    pub(super) unfrozen_slots: Vec<SlotId>,
    pub(super) instr_unfrozen: Vec<SlotId>,
}

///
/// What a compile leaves behind that the tower does not hold: appends
/// to the two context-wide logs, the claim barrier it raised, and the
/// value its call site consumes.
///
pub(super) struct Effect {
    /// Appended to `widened_outer_log` by the compile.
    pub(super) widened: Vec<(usize, SlotId)>,
    /// What `kept_outer_views` holds past the site's mark once the
    /// compile (and its own `drain_kept_outer_views`) is done.
    pub(super) kept: Vec<(usize, SlotId)>,
    /// The compile raised the stage-C claim barrier.
    pub(super) claim_barrier: bool,
    pub(super) capture_events: usize,
    pub(super) result: SpecializedCompileResultMemo,
}

///
/// Where the context-wide logs stood, and whether the claim barrier was
/// already raised, when a call site began compiling. What the compile
/// appended past these marks becomes its [`Effect`].
///
pub(super) struct CallMarks {
    pub(super) widened: usize,
    pub(super) kept: usize,
    pub(super) capture_events: usize,
    pub(super) claim_barrier: bool,
}

///
/// [`SpecializedCompileResult`] minus its `entry` label, which a replay
/// takes fresh: the label is emission bookkeeping, and the AsmIr that
/// would resolve it is never built on a replay.
///
#[derive(Clone)]
pub(super) struct SpecializedCompileResultMemo {
    pub(super) return_state: Option<ReturnState>,
    pub(super) deferred_rest: bool,
    pub(super) needs_rest_array: bool,
    pub(super) had_deopt: bool,
    pub(super) generic_yield: bool,
    pub(super) spec_id: SpecializedId,
    pub(super) using_fpr: UsingFpr,
}

/// How often the memo answered a call site and how often it could not,
/// reported by `jit_stats::dump` under the `jit-log` feature.
pub(super) fn count_hit() {
    #[cfg(feature = "jit-log")]
    crate::codegen::jit_stats::bump(&crate::codegen::jit_stats::SPEC_MEMO_HIT);
}

/// See [`count_hit`].
pub(super) fn count_miss() {
    #[cfg(feature = "jit-log")]
    crate::codegen::jit_stats::bump(&crate::codegen::jit_stats::SPEC_MEMO_MISS);
}

impl SpecMemo {
    fn entries(&self, site: &SpecCallSite) -> Option<&Vec<Entry>> {
        self.table.get(site)
    }

    ///
    /// Whether this site has spent its entries, so a compile about to
    /// run there need not snapshot the tower it enters with.
    ///
    pub(super) fn is_full(&self, site: &SpecCallSite) -> bool {
        self.entries(site)
            .is_some_and(|v| v.len() >= MAX_TOWERS_PER_SITE)
    }

    ///
    /// The tower and effect recorded for an arrival at *entered*, whose
    /// digest is *hash*. The digest is a filter only: what decides the
    /// answer is the full comparison of the towers.
    ///
    pub(super) fn get(
        &self,
        site: &SpecCallSite,
        hash: u64,
        entered: &Tower,
    ) -> Option<(&Tower, &Effect)> {
        self.entries(site)?
            .iter()
            .find(|e| e.hash == hash && &e.entered == entered)
            .map(|e| (&e.returned, &e.effect))
    }

    ///
    /// Whether any entry of this site carries *hash*, the cheap test
    /// that decides whether snapshotting the tower for a full
    /// comparison is worth it.
    ///
    pub(super) fn may_have(&self, site: &SpecCallSite, hash: u64) -> bool {
        self.entries(site)
            .is_some_and(|v| v.iter().any(|e| e.hash == hash))
    }

    pub(super) fn insert(
        &mut self,
        site: SpecCallSite,
        hash: u64,
        entered: Tower,
        returned: Tower,
        effect: Effect,
    ) {
        let entries = self.table.entry(site).or_default();
        if entries.len() < MAX_TOWERS_PER_SITE {
            entries.push(Entry {
                hash,
                entered,
                returned,
                effect,
            });
        }
    }
}
