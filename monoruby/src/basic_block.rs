//! Basic-block structure over a function's bytecode.
//!
//! Although this feeds the JIT, it is **bytecode-level** infrastructure, not
//! JIT-tier code: `bytecodegen` builds a `BasicBlockInfo` for every method
//! (see `bytecodegen::encode`) and stores it in `ISeqInfo.bb_info`, where the
//! interpreter/runtime reads it. It therefore lives outside `codegen::jitgen`
//! so the VM/runtime tier can use it without depending on the JIT front-end.

use std::iter::Step;

use crate::bytecode::BcIndex;
use crate::bytecodegen::inst::BytecodeIr;

///
/// Index of a basic block within one iseq.
///
/// `u32`, not `usize`: it is stored once per *instruction* in
/// `BasicBlockInfo::bb_map` and once per CFG edge, so the width is
/// multiplied by the size of the program. The largest body in a
/// definition-heavy file has tens of thousands of blocks.
///
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub(crate) struct BasicBlockId(pub u32);

impl BasicBlockId {
    pub(crate) fn new(id: usize) -> Self {
        Self(id as u32)
    }

    pub(crate) fn index(self) -> usize {
        self.0 as usize
    }
}

impl std::fmt::Debug for BasicBlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "BB{}", self.0)
    }
}

impl std::ops::Add<usize> for BasicBlockId {
    type Output = Self;
    fn add(self, rhs: usize) -> Self {
        Self(self.0 + rhs as u32)
    }
}

impl std::ops::Sub<usize> for BasicBlockId {
    type Output = Self;
    fn sub(self, rhs: usize) -> Self {
        Self(self.0 - rhs as u32)
    }
}

impl std::ops::AddAssign<usize> for BasicBlockId {
    fn add_assign(&mut self, rhs: usize) {
        *self = Self(self.0 + rhs as u32)
    }
}

impl Step for BasicBlockId {
    fn steps_between(start: &Self, end: &Self) -> (usize, Option<usize>) {
        let d = (end.0 - start.0) as usize;
        (d, Some(d))
    }

    fn forward_checked(start: Self, count: usize) -> Option<Self> {
        Some(start + count)
    }

    fn backward_checked(start: Self, count: usize) -> Option<Self> {
        Some(start - count)
    }

    fn forward_overflowing(start: Self, count: usize) -> (Self, bool) {
        (start + count, false)
    }

    fn backward_overflowing(start: Self, count: usize) -> (Self, bool) {
        match Self::backward_checked(start, count) {
            Some(next) => (next, false),
            None => (start, true),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct BasicBlockInfo {
    ///
    /// Basic block information.
    ///
    info: Vec<BasicBlockInfoEntry>,
    ///
    /// Whether an each instruction (*BcIndex*) is a beginning of a basic block.
    ///
    bb_head: bitvec::vec::BitVec,
    ///
    /// A map for *BcIndex* -> *BasicBlockId*.
    ///
    bb_map: Vec<BasicBlockId>,
    ///
    /// Every block's successors, concatenated; each block's slice is
    /// named by its [`BasicBlockInfoEntry::succ`] range.
    ///
    succ: Box<[BasicBlockId]>,
    ///
    /// Loop information. An array of (start:*BasicBlockId*, end:*BasicBlockId*).
    ///
    loops: Vec<(BasicBlockId, BasicBlockId)>,
}

impl std::ops::Index<BasicBlockId> for BasicBlockInfo {
    type Output = BasicBlockInfoEntry;
    fn index(&self, index: BasicBlockId) -> &Self::Output {
        &self.info[index.index()]
    }
}

impl std::ops::IndexMut<BasicBlockId> for BasicBlockInfo {
    fn index_mut(&mut self, index: BasicBlockId) -> &mut Self::Output {
        &mut self.info[index.index()]
    }
}

impl std::ops::Index<std::ops::RangeInclusive<BasicBlockId>> for BasicBlockInfo {
    type Output = [BasicBlockInfoEntry];
    fn index(&self, index: std::ops::RangeInclusive<BasicBlockId>) -> &Self::Output {
        &self.info[index.start().index()..=index.end().index()]
    }
}

impl std::ops::Index<BcIndex> for BasicBlockInfo {
    type Output = BasicBlockInfoEntry;
    fn index(&self, index: BcIndex) -> &Self::Output {
        let id = self.bb_map[index.to_usize()];
        &self[id]
    }
}

impl std::ops::IndexMut<BcIndex> for BasicBlockInfo {
    fn index_mut(&mut self, index: BcIndex) -> &mut Self::Output {
        let id = self.bb_map[index.to_usize()];
        &mut self[id]
    }
}

///
/// One iseq's basic-block tables, broken down for `Store::memory_report`.
///
#[derive(Default, Clone, Copy)]
pub(crate) struct BbHeapSize {
    pub blocks: usize,
    pub edges: usize,
    /// `info`, the `BasicBlockInfoEntry` array itself.
    pub entries: usize,
    /// The per-entry `pred` / `succ` allocations.
    pub edge_vecs: usize,
    pub bb_head: usize,
    pub bb_map: usize,
    pub loops: usize,
}

impl std::ops::AddAssign for BbHeapSize {
    fn add_assign(&mut self, o: Self) {
        self.blocks += o.blocks;
        self.edges += o.edges;
        self.entries += o.entries;
        self.edge_vecs += o.edge_vecs;
        self.bb_head += o.bb_head;
        self.bb_map += o.bb_map;
        self.loops += o.loops;
    }
}

impl BasicBlockInfo {
    /// Bytes this owns outside itself, for `Store::memory_report`.
    pub(crate) fn heap_size(&self) -> BbHeapSize {
        BbHeapSize {
            blocks: self.info.len(),
            edges: self.succ.len(),
            entries: self.info.capacity() * size_of::<BasicBlockInfoEntry>(),
            edge_vecs: self.succ.len() * size_of::<BasicBlockId>(),
            bb_head: self.bb_head.capacity() / 8,
            bb_map: self.bb_map.capacity() * size_of::<BasicBlockId>(),
            loops: self.loops.capacity() * size_of::<(BasicBlockId, BasicBlockId)>(),
        }
    }

    pub(crate) fn new(incoming: Vec<Vec<BcIndex>>, ir: &BytecodeIr) -> Self {
        // generate bb_head.
        let bb_head: bitvec::vec::BitVec<_> = incoming
            .iter()
            .enumerate()
            .map(|(i, v)| i == 0 || !v.is_empty() || ir.is_terminal(i - 1))
            .collect();

        // generate bb_map.
        let mut bb_id = -1i32;
        let mut bb_map = vec![];
        for b in bb_head.iter() {
            if *b {
                bb_id += 1;
            }
            bb_map.push(BasicBlockId(bb_id as u32));
        }
        bb_id += 1;

        // generate bb_info.
        let mut bb_info = BasicBlockInfo {
            info: vec![BasicBlockInfoEntry::default(); bb_id as usize],
            bb_head,
            bb_map,
            succ: Default::default(),
            loops: Default::default(),
        };

        // Collect the successors per block, then concatenate them: the
        // edges are discovered out of order (an edge is found at its
        // *destination*), so they are gathered before being flattened.
        let mut succ: Vec<Vec<BasicBlockId>> = vec![vec![]; bb_id as usize];
        let mut loop_stack = vec![];
        for (i, incoming) in incoming.into_iter().enumerate() {
            let idx = BcIndex::from(i);
            if ir.is_loop_start(i) {
                loop_stack.push(idx);
            } else if ir.is_loop_end(i) {
                let start = loop_stack.pop().unwrap();
                bb_info
                    .loops
                    .push((bb_info.get_bb_id(start), bb_info.get_bb_id(idx)));
            }

            bb_info[idx].end = idx;
            let dst = bb_info.get_bb_id(idx);
            for src_idx in incoming {
                let src = bb_info.get_bb_id(src_idx);
                bb_info[idx].begin = idx;
                succ[src.index()].push(dst);
            }
        }
        assert!(loop_stack.is_empty());

        let mut flat = Vec::with_capacity(succ.iter().map(|s| s.len()).sum());
        for (id, s) in succ.into_iter().enumerate() {
            bb_info.info[id].succ = EdgeRange {
                start: flat.len() as u32,
                len: s.len() as u32,
            };
            flat.extend(s);
        }
        bb_info.succ = flat.into_boxed_slice();

        bb_info
    }

    /// The blocks control can reach directly from `bb_id`.
    pub(crate) fn succ(&self, bb_id: BasicBlockId) -> &[BasicBlockId] {
        &self.succ[self[bb_id].succ.range()]
    }

    pub(crate) fn len(&self) -> usize {
        self.info.len()
    }

    pub(crate) fn is_bb_head(&self, i: BcIndex) -> Option<BasicBlockId> {
        if *self.bb_head.get(i.to_usize())? {
            Some(self.get_bb_id(i))
        } else {
            None
        }
    }

    pub(crate) fn get_bb_id(&self, i: BcIndex) -> BasicBlockId {
        self.bb_map[i.to_usize()]
    }

    ///
    /// Whether this body has any loop, i.e. whether compiling it runs a
    /// back-edge fixpoint.
    ///
    pub(crate) fn has_loop(&self) -> bool {
        !self.loops.is_empty()
    }

    pub(crate) fn is_loop_begin(
        &self,
        bb_id: BasicBlockId,
    ) -> Option<(BasicBlockId, BasicBlockId)> {
        self.loops
            .iter()
            .find(|(begin, _)| *begin == bb_id)
            .cloned()
    }

    ///
    /// Every loop whose blocks lie within `start..=end` — the loop
    /// headed at `start` itself first, then the loops nested inside it in
    /// order of their heads.
    ///
    pub(crate) fn loops_within(
        &self,
        start: BasicBlockId,
        end: BasicBlockId,
    ) -> Vec<(BasicBlockId, BasicBlockId)> {
        let mut loops: Vec<_> = self
            .loops
            .iter()
            .filter(|(begin, last)| start <= *begin && *last <= end)
            .cloned()
            .collect();
        loops.sort_by_key(|(begin, _)| *begin);
        loops
    }
}

#[derive(Clone, Default)]
pub(crate) struct BasicBlockInfoEntry {
    pub begin: BcIndex,
    pub end: BcIndex,
    /// Where this block's successors sit in [`BasicBlockInfo::succ`] —
    /// a range, not a `Vec`, so a block costs 16 bytes and one iseq's
    /// edges cost one allocation instead of one per block.
    succ: EdgeRange,
}

impl std::fmt::Debug for BasicBlockInfoEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{:?}..={:?}]", self.begin, self.end)
    }
}

///
/// A block's slice of [`BasicBlockInfo::succ`].
///
#[derive(Clone, Copy, Default, Debug)]
struct EdgeRange {
    start: u32,
    len: u32,
}

impl EdgeRange {
    fn range(self) -> std::ops::Range<usize> {
        self.start as usize..(self.start + self.len) as usize
    }
}
