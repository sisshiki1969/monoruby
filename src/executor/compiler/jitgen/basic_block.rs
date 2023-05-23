use super::*;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub(crate) struct BasicBlockId(pub usize);

impl std::ops::Add<usize> for BasicBlockId {
    type Output = Self;
    fn add(self, rhs: usize) -> Self {
        Self(self.0 + rhs)
    }
}

impl std::ops::AddAssign<usize> for BasicBlockId {
    fn add_assign(&mut self, rhs: usize) {
        *self = Self(self.0 + rhs)
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct BasicBlockInfo {
    ///
    /// Basic block information.
    ///
    info: Vec<BasciBlockInfoEntry>,
    ///
    /// Whether an each instruction (*BcIndex*) is a beginning of a basic block.
    ///
    bb_head: bitvec::vec::BitVec,
    ///
    /// A map for *BcIndex* -> *BasicBlockId*.
    ///
    bb_map: Vec<BasicBlockId>,
    ///
    /// Loop information. An array of (start:*BasicBlockId*, end:*BasicBlockId*).
    ///
    pub loops: Vec<(BasicBlockId, BasicBlockId)>,
}

impl std::ops::Index<BasicBlockId> for BasicBlockInfo {
    type Output = BasciBlockInfoEntry;
    fn index(&self, index: BasicBlockId) -> &Self::Output {
        &self.info[index.0]
    }
}

impl std::ops::IndexMut<BasicBlockId> for BasicBlockInfo {
    fn index_mut(&mut self, index: BasicBlockId) -> &mut Self::Output {
        &mut self.info[index.0]
    }
}

impl std::ops::Index<BcIndex> for BasicBlockInfo {
    type Output = BasciBlockInfoEntry;
    fn index(&self, index: BcIndex) -> &Self::Output {
        let id = self.bb_map[index.0 as usize];
        &self[id]
    }
}

impl std::ops::IndexMut<BcIndex> for BasicBlockInfo {
    fn index_mut(&mut self, index: BcIndex) -> &mut Self::Output {
        let id = self.bb_map[index.0 as usize];
        &mut self[id]
    }
}

impl BasicBlockInfo {
    pub(crate) fn new(info: &ISeqInfo) -> Self {
        let incoming = info.get_incoming();

        // generate bb_head.
        let bb_head = incoming
            .iter()
            .enumerate()
            .map(|(i, v)| {
                i == 0 || !v.is_empty() || {
                    let pc = BcPc::from(&info.bytecode()[i - 1]);
                    TraceIr::is_terminal(pc)
                }
            })
            .collect();

        // generate bb_bap.
        let mut bb_id = BasicBlockId(1);
        let mut bb_map = vec![];
        for i in &incoming {
            if !i.is_empty() {
                bb_id += 1;
            }
            bb_map.push(bb_id);
        }
        bb_id += 1;

        // generate bb_info.
        let mut bb_info = BasicBlockInfo {
            info: vec![BasciBlockInfoEntry::default(); bb_id.0],
            bb_head,
            bb_map,
            loops: Default::default(),
        };

        // generate predecessor and successor.
        let mut loop_stack = vec![];
        for (i, incoming) in incoming.into_iter().enumerate() {
            let idx = BcIndex::from(i);
            let pc = info.get_pc(idx);
            if TraceIr::is_loop_start(pc) {
                loop_stack.push(idx);
            } else if TraceIr::is_loop_end(pc) {
                let start = loop_stack.pop().unwrap();
                bb_info
                    .loops
                    .push((bb_info.get_bb_id(start), bb_info.get_bb_id(idx)));
            }

            bb_info[idx].end = idx;
            for i in incoming {
                let incoming = bb_info.get_bb_id(i);
                bb_info[idx].begin = idx;
                bb_info[idx].pred.push(incoming);
                let id = bb_info.get_bb_id(idx);
                bb_info[incoming].succ.push(id);
            }
        }
        assert!(loop_stack.is_empty());

        bb_info
    }

    pub(crate) fn len(&self) -> usize {
        self.info.len()
    }

    pub(super) fn init_bb_scan(&self, func: &ISeqInfo) -> Vec<(ExitType, SlotInfo)> {
        let mut bb_scan = vec![];
        for entry in &self.info {
            bb_scan.push(JitContext::scan_bb(func, entry));
        }
        bb_scan
    }

    pub(crate) fn is_bb_head(&self, i: BcIndex) -> bool {
        self.bb_head[i.0 as usize]
    }

    ///
    /// Get position of start instructions (BcIndex) of basic blocks.
    ///
    pub(super) fn get_bb_pos(&self, start_pos: BcIndex) -> Vec<BcIndex> {
        self.bb_head
            .iter()
            .enumerate()
            .filter_map(|(idx, v)| {
                if *v {
                    let idx = BcIndex::from(idx);
                    if idx >= start_pos {
                        Some(idx)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect()
    }

    pub(crate) fn get_bb_id(&self, i: BcIndex) -> BasicBlockId {
        self.bb_map[i.0 as usize]
    }

    pub(super) fn get_loop(&self, bb_id: BasicBlockId) -> Option<(BasicBlockId, BasicBlockId)> {
        self.loops
            .iter()
            .find(|(begin, end)| (*begin..=*end).contains(&bb_id))
            .cloned()
    }
}

#[derive(Clone, Default)]
pub(crate) struct BasciBlockInfoEntry {
    pub begin: BcIndex,
    pub end: BcIndex,
    pub pred: Vec<BasicBlockId>,
    pub succ: Vec<BasicBlockId>,
}

impl std::fmt::Debug for BasciBlockInfoEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{:?}..={:?} in:{:?} out:{:?}]",
            self.begin, self.end, self.pred, self.succ
        )
    }
}
