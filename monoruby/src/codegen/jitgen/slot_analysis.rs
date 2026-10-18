//!
//! Def-use and liveness tables for the slots of one iseq, in two layers
//! (`doc/escape_analysis.md` §11).
//!
//! **Layer A — [`SlotAnalysis`]**: what the bytecode alone decides. The
//! slots each instruction reads and the one it defines, the basic blocks'
//! use and def sets, and what the block literals of the frame can do to
//! its locals. Built once per iseq, the first time the JIT needs it, and
//! kept in `ISeqInfo::slot_analysis`: the bytecode is never rewritten
//! after it is installed (the inline caches patch operand words, never
//! the shape of an instruction), so the table is never invalidated. It
//! is kept small, since it lives as long as the iseq: a summary is 16
//! bytes, the reads of all instructions share one vector, and the
//! per-block sets share one bit matrix.
//!
//! **Layer B — [`UnitLiveness`]**: what the inline caches decide, which
//! may differ from one compilation to the next: whether a call is a
//! capture point ([`JitContext::capture_point`], a `MethodCache` verdict)
//! and whether the calls inside the frame's blocks are all resolved. Built
//! per compilation unit per iseq from Layer A's summaries, with one
//! backward liveness dataflow over the basic blocks that counts a capture
//! point as a read of every local it may reach.
//!
//! `local_live_after`, `touched_by_blocks` and `alias_consumed_by_call`
//! in `compile/escape.rs` are lookups in these tables.
//!

use super::compile::escape::Capture;
use super::*;
use crate::basic_block::BasicBlockInfo;

///
/// A set of this frame's slots, as a bitset over `0..total_reg_num`.
///
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct SlotSet {
    words: Box<[u64]>,
}

impl SlotSet {
    pub(crate) fn empty(total_reg_num: usize) -> Self {
        Self {
            words: vec![0; total_reg_num.div_ceil(64)].into_boxed_slice(),
        }
    }

    pub(crate) fn full(total_reg_num: usize) -> Self {
        let mut set = Self::empty(total_reg_num);
        for i in 0..total_reg_num {
            set.insert(SlotId(i as u16));
        }
        set
    }

    pub(crate) fn contains(&self, slot: SlotId) -> bool {
        word_contains(&self.words, slot)
    }

    /// Insert *slot*; a slot beyond the frame is ignored.
    pub(crate) fn insert(&mut self, slot: SlotId) {
        word_insert(&mut self.words, slot);
    }

    /// `self |= other`; whether anything changed.
    pub(crate) fn union_with(&mut self, other: &SlotSet) -> bool {
        word_union(&mut self.words, &other.words)
    }

    /// `self -= other`.
    pub(crate) fn difference_with(&mut self, other: &SlotSet) {
        word_difference(&mut self.words, &other.words);
    }

    #[cfg(test)]
    pub(crate) fn iter(&self) -> impl Iterator<Item = SlotId> + '_ {
        self.words.iter().enumerate().flat_map(|(wi, w)| {
            (0..64)
                .filter(move |b| w & (1u64 << b) != 0)
                .map(move |b| SlotId((wi * 64 + b) as u16))
        })
    }
}

fn word_contains(words: &[u64], slot: SlotId) -> bool {
    let i = slot.0 as usize;
    words
        .get(i / 64)
        .is_some_and(|w| w & (1u64 << (i % 64)) != 0)
}

fn word_insert(words: &mut [u64], slot: SlotId) {
    let i = slot.0 as usize;
    if let Some(w) = words.get_mut(i / 64) {
        *w |= 1u64 << (i % 64);
    }
}

fn word_union(words: &mut [u64], other: &[u64]) -> bool {
    let mut changed = false;
    for (a, b) in words.iter_mut().zip(other) {
        let n = *a | *b;
        changed |= n != *a;
        *a = n;
    }
    changed
}

fn word_difference(words: &mut [u64], other: &[u64]) {
    for (a, b) in words.iter_mut().zip(other) {
        *a &= !*b;
    }
}

///
/// One slot set per basic block, in one allocation.
///
#[derive(Clone, Debug)]
pub(crate) struct SlotSets {
    per_set: usize,
    words: Box<[u64]>,
}

impl SlotSets {
    fn empty(sets: usize, total_reg_num: usize) -> Self {
        let per_set = total_reg_num.div_ceil(64);
        Self {
            per_set,
            words: vec![0; sets * per_set].into_boxed_slice(),
        }
    }

    fn len(&self) -> usize {
        if self.per_set == 0 {
            0
        } else {
            self.words.len() / self.per_set
        }
    }

    fn row(&self, b: usize) -> &[u64] {
        &self.words[b * self.per_set..(b + 1) * self.per_set]
    }

    fn row_mut(&mut self, b: usize) -> &mut [u64] {
        &mut self.words[b * self.per_set..(b + 1) * self.per_set]
    }

    pub(crate) fn contains(&self, b: usize, slot: SlotId) -> bool {
        word_contains(self.row(b), slot)
    }

    fn insert(&mut self, b: usize, slot: SlotId) {
        word_insert(self.row_mut(b), slot);
    }
}

///
/// What one instruction does to this frame's slots. 16 bytes: its reads
/// are a window of [`SlotAnalysis::reads`].
///
#[derive(Clone, Copy, Debug)]
struct InstSummary {
    reads_start: u32,
    reads_len: u16,
    /// The slot it defines without reading (`TraceIr::defined_slot`), or
    /// `NO_SLOT` for an instruction whose destination is not plain, which
    /// is neither a read nor a kill.
    def: u16,
    /// The call site, for a method call.
    call: Option<CallSiteId>,
}

const NO_SLOT: u16 = u16::MAX;

impl InstSummary {
    const EMPTY: Self = Self {
        reads_start: 0,
        reads_len: 0,
        def: NO_SLOT,
        call: None,
    };

    fn def(&self) -> Option<SlotId> {
        (self.def != NO_SLOT).then_some(SlotId(self.def))
    }
}

///
/// Layer A: the def-use and liveness facts of one iseq that its bytecode
/// alone decides. See the module documentation.
///
#[derive(Clone, Debug)]
pub(crate) struct SlotAnalysis {
    total_reg_num: usize,
    /// Every local may be read at any point: the iseq rescues, and a
    /// handler block could read anything. The tables are still built
    /// (the handler-only instructions as empty summaries) for the
    /// questions that stay local to a basic block.
    all_live: bool,
    /// Per instruction, by `BcIndex`.
    insts: Box<[InstSummary]>,
    /// The slots every instruction reads (`TraceIr::read_slots`), with
    /// repetitions: a call reading its receiver and the same slot as an
    /// argument lists it twice.
    reads: Box<[SlotId]>,
    /// Per basic block: the slots read before being written in it.
    bb_use: SlotSets,
    /// Per basic block: the slots it writes (`InstSummary::def`).
    bb_def: SlotSets,
    /// The locals a block literal of this frame (or a block nested in one)
    /// reads or writes through the outer chain (`LoadDynVar` /
    /// `StoreDynVar` at the block's depth).
    block_touched: SlotSet,
    /// A block of this frame is opaque: it has a handler (its instructions
    /// are not decodable and could read anything) or a lambda literal
    /// (whose body is not reachable from here). Every local then counts
    /// as touched.
    block_opaque: bool,
    /// The method calls inside the frame's blocks, by the block's iseq and
    /// position. Whether each is resolved to a callee that cannot capture
    /// its caller is an inline-cache verdict, left to Layer B.
    block_calls: Box<[(ISeqId, BcIndex)]>,
}

impl SlotAnalysis {
    ///
    /// Build the table of *iseq*. Decodes every instruction of the iseq
    /// once, and every instruction of the blocks it passes (transitively).
    ///
    pub(crate) fn new(store: &Store, iseq_id: ISeqId) -> Self {
        let iseq = &store[iseq_id];
        let total_reg_num = iseq.total_reg_num();
        let len = iseq.bytecode().len();
        let mut insts = Vec::with_capacity(len);
        let mut reads: Vec<SlotId> = vec![];
        for i in 0..len {
            let pc = iseq.get_pc(BcIndex::from(i));
            if TraceIr::is_handler_only(pc) {
                // Not decodable, and in a handler block the JIT never
                // reaches: neither a read nor a kill of anything.
                insts.push(InstSummary::EMPTY);
                continue;
            }
            let ir = TraceIr::from_pc(pc, store);
            let call = match &ir {
                TraceIr::MethodCall { callid, .. } => Some(*callid),
                _ => None,
            };
            let reads_start = reads.len() as u32;
            reads.extend(ir.read_slots(store));
            insts.push(InstSummary {
                reads_start,
                reads_len: (reads.len() - reads_start as usize) as u16,
                def: ir.defined_slot(store).map_or(NO_SLOT, |s| s.0),
                call,
            });
        }

        // Per-block use / def.
        let bb_info = &iseq.bb_info;
        let nbb = bb_info.len();
        let mut bb_use = SlotSets::empty(nbb, total_reg_num);
        let mut bb_def = SlotSets::empty(nbb, total_reg_num);
        for b in 0..nbb {
            let entry = &bb_info[BasicBlockId(b as u32)];
            for i in entry.begin.to_usize()..=entry.end.to_usize() {
                let inst = &insts[i];
                for r in &reads[inst.reads_start as usize..][..inst.reads_len as usize] {
                    if !bb_def.contains(b, *r) {
                        bb_use.insert(b, *r);
                    }
                }
                if let Some(d) = inst.def() {
                    bb_def.insert(b, d);
                }
            }
        }
        // The blocks of this frame: what they touch through the outer
        // chain, and the calls in them.
        let mut block_touched = SlotSet::empty(total_reg_num);
        let mut block_opaque = false;
        let mut block_calls = vec![];
        let mut stack: Vec<(ISeqId, usize)> = vec![(iseq_id, 0)];
        'walk: while let Some((bid, depth)) = stack.pop() {
            let block = &store[bid];
            if depth > 0 && block.has_exception_handler() {
                block_opaque = true;
                break 'walk;
            }
            for i in 0..block.bytecode().len() {
                let pc = block.get_pc(BcIndex::from(i));
                if TraceIr::is_handler_only(pc) {
                    continue;
                }
                match TraceIr::from_pc(pc, store) {
                    TraceIr::LoadDynVar(_, DynVar { reg, outer })
                    | TraceIr::StoreDynVar(DynVar { reg, outer }, _)
                        if depth > 0 && outer == depth =>
                    {
                        block_touched.insert(reg);
                    }
                    TraceIr::Lambda if depth > 0 => {
                        block_opaque = true;
                        break 'walk;
                    }
                    TraceIr::MethodCall { callid, .. } => {
                        if depth > 0 {
                            block_calls.push((bid, BcIndex::from(i)));
                        }
                        if let Some(fid) = store[callid].block_fid
                            && let Some(inner) = store[fid].is_iseq()
                        {
                            stack.push((inner, depth + 1));
                        }
                    }
                    _ => {}
                }
            }
        }
        if block_opaque {
            block_calls.clear();
        }

        Self {
            total_reg_num,
            all_live: iseq.has_exception_handler(),
            insts: insts.into_boxed_slice(),
            reads: reads.into_boxed_slice(),
            bb_use,
            bb_def,
            block_touched,
            block_opaque,
            block_calls: block_calls.into_boxed_slice(),
        }
    }

    ///
    /// The backward dataflow
    ///   live_in(b)  = use(b) ∪ (live_out(b) − def(b))
    ///   live_out(b) = ∪ live_in(s) over the successors s of b,
    /// to a fixpoint. Blocks are in source order, so sweeping them
    /// backwards converges in a few passes.
    ///
    fn dataflow(bb_info: &BasicBlockInfo, bb_use: &SlotSets, bb_def: &SlotSets) -> SlotSets {
        let nbb = bb_use.len();
        let per_set = bb_use.per_set;
        let mut live_in = bb_use.clone();
        live_in.words.fill(0);
        let mut live_out = live_in.clone();
        let mut out = vec![0u64; per_set];
        let mut inp = vec![0u64; per_set];
        loop {
            let mut changed = false;
            for b in (0..nbb).rev() {
                out.fill(0);
                for s in bb_info.succ(BasicBlockId(b as u32)) {
                    word_union(&mut out, live_in.row(s.index()));
                }
                inp.copy_from_slice(&out);
                word_difference(&mut inp, bb_def.row(b));
                word_union(&mut inp, bb_use.row(b));
                changed |= word_union(live_out.row_mut(b), &out);
                changed |= word_union(live_in.row_mut(b), &inp);
            }
            if !changed {
                break;
            }
        }
        live_out
    }

    pub(crate) fn total_reg_num(&self) -> usize {
        self.total_reg_num
    }

    pub(crate) fn all_live(&self) -> bool {
        self.all_live
    }

    /// The slots the instruction at *pos* reads, with repetitions.
    pub(crate) fn reads(&self, pos: BcIndex) -> &[SlotId] {
        let inst = &self.insts[pos.to_usize()];
        &self.reads[inst.reads_start as usize..][..inst.reads_len as usize]
    }

    /// The slot the instruction at *pos* defines without reading.
    pub(crate) fn def(&self, pos: BcIndex) -> Option<SlotId> {
        self.insts[pos.to_usize()].def()
    }

    /// The call site of the instruction at *pos*, for a method call.
    pub(crate) fn call(&self, pos: BcIndex) -> Option<CallSiteId> {
        self.insts[pos.to_usize()].call
    }

    /// Whether a basic block in `start..=end` writes *slot* (a plain
    /// definition, `InstSummary::def`).
    pub(crate) fn defined_in_blocks(
        &self,
        start: BasicBlockId,
        end: BasicBlockId,
        slot: SlotId,
    ) -> bool {
        (start..=end).any(|b| self.bb_def.contains(b.index(), slot))
    }

    ///
    /// The first instruction after *pos* in its basic block that is a
    /// method call, reads *dst*, or does not define a plain slot other
    /// than *dst*: the only instruction a temporary's view of an elided
    /// object may be consumed by (`JitContext::alias_consumed_by_call`),
    /// when it is a call. `None` when the block ends first, or the
    /// stopping instruction is not a call.
    ///
    pub(crate) fn alias_stop(
        &self,
        bb_info: &BasicBlockInfo,
        pos: BcIndex,
        dst: SlotId,
    ) -> Option<BcIndex> {
        let end = bb_info[bb_info.get_bb_id(pos)].end;
        for i in pos.to_usize() + 1..=end.to_usize() {
            let i = BcIndex::from(i);
            if self.call(i).is_some() {
                return Some(i);
            }
            if self.reads(i).contains(&dst) || !self.def(i).is_some_and(|d| d != dst) {
                return None;
            }
        }
        None
    }
}

///
/// Layer B: Layer A's liveness with the capture points of one compilation
/// counted as reads. See the module documentation.
///
pub(crate) struct UnitLiveness {
    /// Per instruction: the capture verdict of the call there, if any.
    captures: Vec<Option<Capture>>,
    /// The locals a block literal of the frame may read or write: Layer
    /// A's `block_touched`, or every slot when a block is opaque or holds a
    /// call this compilation cannot resolve to a non-capturing callee.
    touched: SlotSet,
    /// Per basic block: live at its exit, the capture reads counted.
    live_out: SlotSets,
}

impl UnitLiveness {
    /// Whether a capture point in `begin..=end` may read the local
    /// *slot*.
    pub(crate) fn captured_in(&self, begin: BcIndex, end: BcIndex, slot: SlotId) -> bool {
        (begin.to_usize()..=end.to_usize()).any(|i| match self.captures[i] {
            Some(Capture::Frame) => true,
            Some(Capture::Block) => self.touched.contains(slot),
            None => false,
        })
    }
}

impl<'a> JitContext<'a> {
    ///
    /// Layer A of *iseq_id*, built on first use.
    ///
    pub(crate) fn slot_analysis(&self, iseq_id: ISeqId) -> &'a SlotAnalysis {
        self.store[iseq_id]
            .slot_analysis
            .get_or_init(|| Box::new(SlotAnalysis::new(self.store, iseq_id)))
    }

    ///
    /// Layer B of *iseq_id* for this compilation, built on first use.
    ///
    pub(crate) fn unit_liveness(&self, iseq_id: ISeqId) -> std::rc::Rc<UnitLiveness> {
        if let Some(u) = self.unit_liveness.borrow().get(&iseq_id) {
            return u.clone();
        }
        let u = std::rc::Rc::new(self.build_unit_liveness(iseq_id));
        self.unit_liveness.borrow_mut().insert(iseq_id, u.clone());
        u
    }

    fn build_unit_liveness(&self, iseq_id: ISeqId) -> UnitLiveness {
        let a = self.slot_analysis(iseq_id);
        let iseq = &self.store[iseq_id];
        let n = a.total_reg_num();
        // What the blocks may touch, for this compilation.
        let unresolved = a.block_calls.iter().any(|(bid, pos)| {
            let ir = TraceIr::from_pc(self.store[*bid].get_pc(*pos), self.store);
            !self.call_resolved_non_capturing(&ir)
        });
        let touched = if a.block_opaque || unresolved {
            SlotSet::full(n)
        } else {
            a.block_touched.clone()
        };
        // The capture verdict of every call, and the per-block use sets
        // with the capture reads added (a capture point reads its slots
        // before anything written after it, so the def set is unchanged).
        let bb_info = &iseq.bb_info;
        let mut captures = vec![None; a.insts.len()];
        let mut bb_use = a.bb_use.clone();
        let full = SlotSet::full(n);
        let mut defined = SlotSet::empty(n);
        let mut extra = SlotSet::empty(n);
        for b in 0..bb_use.len() {
            let entry = &bb_info[BasicBlockId(b as u32)];
            // Slots written so far in the block: a capture read of one of
            // them is not a use of the block's live-in value.
            defined.words.fill(0);
            for i in entry.begin.to_usize()..=entry.end.to_usize() {
                let pos = BcIndex::from(i);
                if a.call(pos).is_some() {
                    let ir = TraceIr::from_pc(iseq.get_pc(pos), self.store);
                    let capture = self.capture_point(&ir);
                    captures[i] = capture;
                    let read = match capture {
                        Some(Capture::Frame) => &full,
                        Some(Capture::Block) => &touched,
                        None => continue,
                    };
                    extra.words.copy_from_slice(&read.words);
                    extra.difference_with(&defined);
                    word_union(bb_use.row_mut(b), &extra.words);
                }
                if let Some(d) = a.def(pos) {
                    defined.insert(d);
                }
            }
        }
        let live_out = SlotAnalysis::dataflow(bb_info, &bb_use, &a.bb_def);
        UnitLiveness {
            captures,
            touched,
            live_out,
        }
    }

    ///
    /// Whether *ir* is a call resolved, by its inline cache, to a callee
    /// that cannot capture its caller's frame without a block.
    ///
    pub(in crate::codegen::jitgen) fn call_resolved_non_capturing(&self, ir: &TraceIr) -> bool {
        matches!(ir, TraceIr::MethodCall { cache: MethodCache::Cached(e), .. }
            if !self.store[e.func_id].possibly_capture_without_block())
    }

    ///
    /// Whether the slot *slot* of the current frame may be read after the
    /// instruction at *bc_pos*, by the tables: the rest of its basic
    /// block, then the block's live-out, with every capture point of
    /// this compilation counted as a read of the locals it may reach.
    ///
    pub(in crate::codegen::jitgen) fn table_live_after(
        &self,
        bc_pos: BcIndex,
        slot: SlotId,
    ) -> bool {
        let iseq_id = self.iseq_id();
        let a = self.slot_analysis(iseq_id);
        if a.all_live() {
            return true;
        }
        let u = self.unit_liveness(iseq_id);
        let bb_info = &self.store[iseq_id].bb_info;
        let bb = bb_info.get_bb_id(bc_pos);
        let end = bb_info[bb].end;
        for i in bc_pos.to_usize() + 1..=end.to_usize() {
            let pos = BcIndex::from(i);
            let captured = match u.captures[i] {
                Some(Capture::Frame) => true,
                Some(Capture::Block) => u.touched.contains(slot),
                None => false,
            };
            if captured || a.reads(pos).contains(&slot) {
                return true;
            }
            if a.def(pos) == Some(slot) {
                return false;
            }
        }
        u.live_out.contains(bb.index(), slot)
    }

    ///
    /// Whether a block literal of the current frame may read or write the
    /// local *slot*, by the tables.
    ///
    pub(in crate::codegen::jitgen) fn table_touched_by_blocks(&self, slot: SlotId) -> bool {
        self.unit_liveness(self.iseq_id()).touched.contains(slot)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn slot_set_ops() {
        let mut a = SlotSet::empty(70);
        a.insert(SlotId(3));
        a.insert(SlotId(69));
        a.insert(SlotId(200)); // beyond the frame: ignored
        assert!(a.contains(SlotId(3)) && a.contains(SlotId(69)));
        assert!(!a.contains(SlotId(4)) && !a.contains(SlotId(200)));
        assert_eq!(a.iter().collect::<Vec<_>>(), vec![SlotId(3), SlotId(69)]);
        let mut b = SlotSet::empty(70);
        b.insert(SlotId(4));
        assert!(b.union_with(&a));
        assert!(!b.union_with(&a));
        assert_eq!(b.iter().count(), 3);
        b.difference_with(&a);
        assert_eq!(b.iter().collect::<Vec<_>>(), vec![SlotId(4)]);
        let full = SlotSet::full(70);
        assert_eq!(full.iter().count(), 70);
        assert!(!full.contains(SlotId(70)));
        assert_eq!(size_of::<InstSummary>(), 16);
    }
}
