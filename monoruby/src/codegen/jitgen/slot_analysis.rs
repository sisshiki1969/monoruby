//!
//! Def-use and liveness tables for the slots of one iseq, in two layers
//! (`doc/escape_analysis.md` §11).
//!
//! **Layer A — [`SlotAnalysis`]**: what the bytecode alone decides. The
//! slots each instruction reads and the one it defines, the basic blocks'
//! live-in / live-out sets with no capture counted, and what the block
//! literals of the frame can do to its locals. Built once per iseq, the
//! first time the JIT needs it, and kept in `ISeqInfo::slot_analysis`:
//! the bytecode is never rewritten after it is installed (the inline
//! caches patch operand words, never the shape of an instruction), so the
//! table is never invalidated.
//!
//! **Layer B — [`UnitLiveness`]**: what the inline caches decide, which
//! may differ from one compilation to the next: whether a call is a
//! capture point ([`JitContext::capture_point`], a `MethodCache` verdict)
//! and whether the calls inside the frame's blocks are all resolved. Built
//! per compilation unit per iseq from Layer A's summaries, with one
//! dataflow pass that counts a capture point as a read of every local it
//! may reach.
//!
//! Both layers answer the same questions the forward scans of
//! `compile/escape.rs` answer by re-decoding the bytecode on every call
//! (`local_live_after`, `dead_after`, `touched_by_blocks`,
//! `alias_consumed_by_call`); for now the scans stay the live answer and
//! the tables are checked against them (`debug_assert`).
//!

use super::compile::escape::Capture;
use super::*;
use crate::basic_block::BasicBlockInfo;

///
/// A set of this frame's slots, as a bitset over `0..total_reg_num`.
///
#[derive(Clone, PartialEq, Eq)]
pub(crate) struct SlotSet {
    words: Box<[u64]>,
}

impl std::fmt::Debug for SlotSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
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
        let i = slot.0 as usize;
        self.words
            .get(i / 64)
            .is_some_and(|w| w & (1u64 << (i % 64)) != 0)
    }

    /// Insert *slot*; a slot beyond the frame is ignored.
    pub(crate) fn insert(&mut self, slot: SlotId) {
        let i = slot.0 as usize;
        if let Some(w) = self.words.get_mut(i / 64) {
            *w |= 1u64 << (i % 64);
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.words.iter().all(|w| *w == 0)
    }

    /// `self |= other`; whether anything changed.
    pub(crate) fn union_with(&mut self, other: &SlotSet) -> bool {
        let mut changed = false;
        for (a, b) in self.words.iter_mut().zip(other.words.iter()) {
            let n = *a | *b;
            changed |= n != *a;
            *a = n;
        }
        changed
    }

    /// `self -= other`.
    pub(crate) fn difference_with(&mut self, other: &SlotSet) {
        for (a, b) in self.words.iter_mut().zip(other.words.iter()) {
            *a &= !*b;
        }
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = SlotId> + '_ {
        self.words.iter().enumerate().flat_map(|(wi, w)| {
            (0..64)
                .filter(move |b| w & (1u64 << b) != 0)
                .map(move |b| SlotId((wi * 64 + b) as u16))
        })
    }

    pub(crate) fn heap_size(&self) -> usize {
        self.words.len() * size_of::<u64>()
    }
}

///
/// What one instruction does to this frame's slots.
///
#[derive(Clone, Debug, Default)]
pub(crate) struct InstSummary {
    /// The slots it reads (`TraceIr::read_slots`), with repetitions: a
    /// call reading its receiver and the same slot as an argument lists
    /// it twice.
    pub(crate) reads: smallvec::SmallVec<[SlotId; 6]>,
    /// The slot it defines without reading (`TraceIr::defined_slot`):
    /// `None` for an instruction whose destination is not plain, which is
    /// neither a read nor a kill.
    pub(crate) def: Option<SlotId>,
    /// The call site, for a method call.
    pub(crate) call: Option<CallSiteId>,
}

impl InstSummary {
    pub(crate) fn reads(&self, slot: SlotId) -> bool {
        self.reads.contains(&slot)
    }

    pub(crate) fn read_count(&self, slot: SlotId) -> usize {
        self.reads.iter().filter(|s| **s == slot).count()
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
    /// handler block (whose instructions the JIT cannot decode) could
    /// read anything. The other tables are empty then.
    all_live: bool,
    /// Per instruction, by `BcIndex`.
    insts: Vec<InstSummary>,
    /// Per basic block: the slots read before being written in it.
    bb_use: Vec<SlotSet>,
    /// Per basic block: the slots it writes (`InstSummary::def`).
    bb_def: Vec<SlotSet>,
    /// Per basic block: live at its exit, counting no capture.
    live_out: Vec<SlotSet>,
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
    block_calls: Vec<(ISeqId, BcIndex)>,
}

impl SlotAnalysis {
    ///
    /// Build the table of *iseq*. Decodes every instruction of the iseq
    /// once, and every instruction of the blocks it passes (transitively).
    ///
    pub(crate) fn new(store: &Store, iseq_id: ISeqId) -> Self {
        let iseq = &store[iseq_id];
        let total_reg_num = iseq.total_reg_num();
        if iseq.has_exception_handler() {
            return Self {
                total_reg_num,
                all_live: true,
                insts: vec![],
                bb_use: vec![],
                bb_def: vec![],
                live_out: vec![],
                block_touched: SlotSet::empty(total_reg_num),
                block_opaque: false,
                block_calls: vec![],
            };
        }
        let len = iseq.bytecode().len();
        let mut insts = Vec::with_capacity(len);
        for i in 0..len {
            let ir = TraceIr::from_pc(iseq.get_pc(BcIndex::from(i)), store);
            let call = match &ir {
                TraceIr::MethodCall { callid, .. } => Some(*callid),
                _ => None,
            };
            insts.push(InstSummary {
                reads: ir.read_slots(store).into_iter().collect(),
                def: ir.defined_slot(store),
                call,
            });
        }

        // Per-block use / def, then the backward dataflow:
        //   live_in(b)  = use(b) ∪ (live_out(b) − def(b))
        //   live_out(b) = ∪ live_in(s) over the successors s of b.
        // Blocks are in source order, so sweeping them backwards
        // converges in a few passes.
        let bb_info = &iseq.bb_info;
        let nbb = bb_info.len();
        let mut bb_use = vec![SlotSet::empty(total_reg_num); nbb];
        let mut bb_def = vec![SlotSet::empty(total_reg_num); nbb];
        for b in 0..nbb {
            let entry = &bb_info[BasicBlockId(b as u32)];
            for i in entry.begin.to_usize()..=entry.end.to_usize() {
                let inst = &insts[i];
                for r in inst.reads.iter() {
                    if !bb_def[b].contains(*r) {
                        bb_use[b].insert(*r);
                    }
                }
                if let Some(d) = inst.def {
                    bb_def[b].insert(d);
                }
            }
        }
        let live_out = Self::dataflow(bb_info, &bb_use, &bb_def, total_reg_num);

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
                match TraceIr::from_pc(block.get_pc(BcIndex::from(i)), store) {
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
            all_live: false,
            insts,
            bb_use,
            bb_def,
            live_out,
            block_touched,
            block_opaque,
            block_calls,
        }
    }

    fn dataflow(
        bb_info: &BasicBlockInfo,
        bb_use: &[SlotSet],
        bb_def: &[SlotSet],
        total_reg_num: usize,
    ) -> Vec<SlotSet> {
        let nbb = bb_info.len();
        let mut live_in = vec![SlotSet::empty(total_reg_num); nbb];
        let mut live_out = vec![SlotSet::empty(total_reg_num); nbb];
        loop {
            let mut changed = false;
            for b in (0..nbb).rev() {
                let mut out = SlotSet::empty(total_reg_num);
                for s in bb_info.succ(BasicBlockId(b as u32)) {
                    out.union_with(&live_in[s.index()]);
                }
                let mut inp = out.clone();
                inp.difference_with(&bb_def[b]);
                inp.union_with(&bb_use[b]);
                changed |= live_out[b].union_with(&out);
                changed |= live_in[b].union_with(&inp);
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

    pub(crate) fn inst(&self, pos: BcIndex) -> &InstSummary {
        &self.insts[pos.to_usize()]
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
            let inst = &self.insts[i];
            if inst.call.is_some() {
                return Some(BcIndex::from(i));
            }
            if inst.reads(dst) || !inst.def.is_some_and(|d| d != dst) {
                return None;
            }
        }
        None
    }

    /// Bytes this owns outside itself.
    pub(crate) fn heap_size(&self) -> usize {
        self.insts.capacity() * size_of::<InstSummary>()
            + self
                .insts
                .iter()
                .filter(|i| i.reads.spilled())
                .map(|i| i.reads.capacity() * size_of::<SlotId>())
                .sum::<usize>()
            + (self.bb_use.iter().chain(&self.bb_def).chain(&self.live_out))
                .map(|s| s.heap_size())
                .sum::<usize>()
            + self.block_touched.heap_size()
            + self.block_calls.capacity() * size_of::<(ISeqId, BcIndex)>()
    }
}

///
/// Layer B: Layer A's liveness with the capture points of one compilation
/// counted as reads. See the module documentation.
///
#[derive(Debug)]
pub(crate) struct UnitLiveness {
    iseq_id: ISeqId,
    /// Per instruction: the capture verdict of the call there, if any.
    captures: Vec<Option<Capture>>,
    /// The locals a block literal of the frame may read or write: Layer
    /// A's `block_touched`, or every slot when a block is opaque or holds a
    /// call this compilation cannot resolve to a non-capturing callee.
    touched: SlotSet,
    /// Per basic block: live at its exit, the capture reads counted.
    live_out: Vec<SlotSet>,
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
        if a.all_live() {
            return UnitLiveness {
                iseq_id,
                captures: vec![],
                touched: SlotSet::full(n),
                live_out: vec![],
            };
        }
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
        for b in 0..bb_info.len() {
            let entry = &bb_info[BasicBlockId(b as u32)];
            // Slots written so far in the block: a capture read of one of
            // them is not a use of the block's live-in value.
            let mut defined = SlotSet::empty(n);
            for i in entry.begin.to_usize()..=entry.end.to_usize() {
                let inst = &a.insts[i];
                if inst.call.is_some() {
                    let ir = TraceIr::from_pc(iseq.get_pc(BcIndex::from(i)), self.store);
                    let capture = self.capture_point(&ir);
                    captures[i] = capture;
                    let read: Option<&SlotSet> = match capture {
                        Some(Capture::Frame) => None,
                        Some(Capture::Block) => Some(&touched),
                        None => continue,
                    };
                    let mut extra = match read {
                        Some(set) => set.clone(),
                        None => SlotSet::full(n),
                    };
                    extra.difference_with(&defined);
                    bb_use[b].union_with(&extra);
                }
                if let Some(d) = inst.def {
                    defined.insert(d);
                }
            }
        }
        let live_out = SlotAnalysis::dataflow(bb_info, &bb_use, &a.bb_def, n);
        UnitLiveness {
            iseq_id,
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
            let inst = a.inst(BcIndex::from(i));
            let captured = match u.captures[i] {
                Some(Capture::Frame) => true,
                Some(Capture::Block) => u.touched.contains(slot),
                None => false,
            };
            if captured || inst.reads(slot) {
                return true;
            }
            if inst.def == Some(slot) {
                return false;
            }
        }
        u.live_out[bb.index()].contains(slot)
    }

    ///
    /// Whether a block literal of the current frame may read or write the
    /// local *slot*, by the tables.
    ///
    pub(in crate::codegen::jitgen) fn table_touched_by_blocks(&self, slot: SlotId) -> bool {
        self.unit_liveness(self.iseq_id()).touched.contains(slot)
    }
}

impl UnitLiveness {
    pub(crate) fn iseq_id(&self) -> ISeqId {
        self.iseq_id
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn slot_set_ops() {
        let mut a = SlotSet::empty(70);
        assert!(a.is_empty());
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
    }
}
