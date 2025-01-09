//
// Just-in-time compiler module.
//

use super::*;

#[derive(Clone)]
pub(super) enum JitType {
    /// JIT for method.
    Method,
    /// JIT for loop.
    Loop(BytecodePtr),
    /// inlined JIT method,
    Inlined,
}

///
/// Context for JIT compilation.
///
pub(super) struct JitContext {
    ///
    /// IseqId of the method.
    ///
    iseq_id: ISeqId,
    ///
    /// The block given to the method and its `self` class.
    ///
    block_info: Option<(FuncId, ClassId)>,
    ///
    /// The start bytecode position of the loop to be compiled.
    ///
    /// None for method compilation.
    ///
    jit_type: JitType,
    ///
    /// Number of slots.
    ///
    total_reg_num: usize,
    ///
    /// Number of local variables.
    ///
    local_num: usize,
    ///
    /// *self* for this loop/method.
    ///
    self_class: ClassId,
    ///
    /// Object type of *self*.
    ///
    self_ty: Option<ObjTy>,
    ///
    /// Class version at compile time.
    ///
    class_version: u32,
    ///
    /// Level of inlining.
    ///
    inlining_level: usize,

    ///
    /// Destination labels for each BasicBlock.
    ///
    basic_block_labels: HashMap<BasicBlockId, JitLabel>,
    ///
    /// Loop information.
    ///
    /// ### key
    /// the entry basic block of the loop.
    ///
    /// ### value
    /// (the last basic block, liveness info)
    ///
    pub loop_info: HashMap<BasicBlockId, Liveness>,
    ///
    /// Nested loop count.
    ///
    pub loop_count: usize,
    ///
    /// Map for bytecode position and branches.
    ///
    pub branch_map: HashMap<BasicBlockId, Vec<BranchEntry>>,
    ///
    /// Target `BBContext` for an each instruction.
    ///
    pub target_ctx: HashMap<BasicBlockId, BBContext>,
    ///
    /// Map for backward branches.
    ///
    pub backedge_map: HashMap<BasicBlockId, BackedgeInfo>,
    ///
    /// Information for bridges.
    ///
    pub bridges: Vec<(AsmIr, JitLabel, BasicBlockId)>,
    ///
    /// Information for continuation bridge.
    ///
    pub continuation_bridge: Option<(Option<ContinuationInfo>, JitLabel)>,
    ///
    /// Information for `JitLabel`s`.
    ///
    labels: Vec<Option<DestLabel>>,
    ///
    /// Generated AsmIr.
    ///
    pub ir: Vec<AsmIr>,
    ///
    /// Flag whether ivar on the heap is accessed in this context.
    ///
    pub ivar_heap_accessed: bool,
    ///
    /// Information for inlined methods.
    ///
    pub inlined_methods: Vec<(JitLabel, JitContext)>,
    ///
    /// Source map for bytecode index and machine code position.
    ///
    #[cfg(feature = "emit-asm")]
    pub sourcemap: Vec<(BcIndex, usize)>,
    ///
    /// Start offset of a machine code corresponding to the current basic block.
    ///
    #[cfg(feature = "emit-asm")]
    pub start_codepos: usize,
}

impl JitContext {
    ///
    /// Create new JitContext.
    ///
    pub(super) fn new(
        store: &Store,
        iseq_id: ISeqId,
        jit_type: JitType,
        class_version: u32,
        self_class: ClassId,
        inlining_level: usize,
        block_info: Option<(FuncId, ClassId)>,
    ) -> Self {
        let func = &store[iseq_id];
        let self_ty = store[self_class].instance_ty();
        let mut basic_block_labels = HashMap::default();
        let mut labels = vec![];
        for i in 0..func.bb_info.len() {
            let idx = BasicBlockId(i);
            basic_block_labels.insert(idx, JitLabel(labels.len()));
            labels.push(None);
        }

        let total_reg_num = func.total_reg_num();
        let local_num = func.local_num();
        Self {
            iseq_id,
            block_info,
            jit_type,
            basic_block_labels,
            loop_info: HashMap::default(),
            loop_count: 0,
            branch_map: HashMap::default(),
            target_ctx: HashMap::default(),
            backedge_map: HashMap::default(),
            total_reg_num,
            local_num,
            self_class,
            self_ty,
            bridges: vec![],
            continuation_bridge: None,
            labels,
            class_version,
            ir: vec![],
            ivar_heap_accessed: false,
            inlining_level,
            inlined_methods: vec![],
            #[cfg(feature = "emit-asm")]
            sourcemap: vec![],
            #[cfg(feature = "emit-asm")]
            start_codepos: 0,
        }
    }

    pub(super) fn from_ctx(&self) -> Self {
        let total_reg_num = self.total_reg_num;
        let local_num = self.local_num;
        Self {
            iseq_id: self.iseq_id,
            block_info: self.block_info,
            jit_type: self.jit_type.clone(),
            basic_block_labels: HashMap::default(),
            loop_info: HashMap::default(),
            loop_count: 0,
            branch_map: HashMap::default(),
            target_ctx: HashMap::default(),
            backedge_map: HashMap::default(),
            total_reg_num,
            local_num,
            self_class: NIL_CLASS,
            self_ty: None,
            bridges: vec![],
            continuation_bridge: None,
            labels: vec![],
            class_version: 0,
            ir: vec![],
            ivar_heap_accessed: false,
            inlining_level: 0,
            inlined_methods: vec![],
            #[cfg(feature = "emit-asm")]
            sourcemap: vec![],
            #[cfg(feature = "emit-asm")]
            start_codepos: 0,
        }
    }

    pub(super) fn iseq_id(&self) -> ISeqId {
        self.iseq_id
    }

    pub(super) fn jit_type(&self) -> &JitType {
        &self.jit_type
    }

    pub(super) fn block_info(&self) -> &Option<(FuncId, ClassId)> {
        &self.block_info
    }

    pub(super) fn local_num(&self) -> usize {
        self.local_num
    }

    pub(super) fn total_reg_num(&self) -> usize {
        self.total_reg_num
    }

    pub(super) fn self_class(&self) -> ClassId {
        self.self_class
    }

    pub(super) fn self_ty(&self) -> Option<ObjTy> {
        self.self_ty
    }

    pub(super) fn class_version(&self) -> u32 {
        self.class_version
    }

    pub(super) fn inlining_level(&self) -> usize {
        self.inlining_level
    }

    pub(super) fn position(&self) -> Option<BytecodePtr> {
        match &self.jit_type {
            JitType::Loop(pos) => Some(*pos),
            _ => None,
        }
    }
    pub(super) fn is_loop(&self) -> bool {
        matches!(self.jit_type, JitType::Loop(_))
    }

    ///
    /// Resolve *JitLabel* and return *DestLabel*.
    ///
    pub(super) fn resolve_label(&mut self, jit: &mut JitMemory, label: JitLabel) -> DestLabel {
        match self.labels[label.0] {
            Some(l) => l,
            None => {
                let l = jit.label();
                self.labels[label.0] = Some(l);
                l
            }
        }
    }

    pub(super) fn get_bb_label(&self, bb: BasicBlockId) -> JitLabel {
        self.basic_block_labels.get(&bb).copied().unwrap()
    }

    ///
    /// Create a new *JitLabel*.
    ///
    pub(super) fn label(&mut self) -> JitLabel {
        let id = self.labels.len();
        self.labels.push(None);
        JitLabel(id)
    }

    pub(super) fn loop_info(&self, entry_bb: BasicBlockId) -> (Vec<(SlotId, bool)>, Vec<SlotId>) {
        match self.loop_info.get(&entry_bb) {
            Some(liveness) => (liveness.get_loop_used_as_float(), liveness.get_unused()),
            None => (vec![], vec![]),
        }
    }

    ///
    /// Add new branch from *src_idx* to *dest* with the context *bbctx*.
    ///
    pub(super) fn new_branch(
        &mut self,
        func: &ISeqInfo,
        src_idx: BcIndex,
        dest: BasicBlockId,
        mut bbctx: BBContext,
        branch_dest: JitLabel,
    ) {
        bbctx.sp = func.get_sp(src_idx);
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_branch: [{:?}]{src_idx}->{:?}", bbctx.sp, dest);
        self.branch_map.entry(dest).or_default().push(BranchEntry {
            src_idx,
            bbctx,
            branch_dest,
            cont: false,
        });
    }

    ///
    /// Add new continuation branch from *src_idx* to *dest* with the context *bbctx*.
    ///
    pub(super) fn new_continue(
        &mut self,
        func: &ISeqInfo,
        src_idx: BcIndex,
        dest: BasicBlockId,
        mut bbctx: BBContext,
        branch_dest: JitLabel,
    ) {
        bbctx.sp = func.get_sp(src_idx);
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_continue:[{:?}] {src_idx}->{:?}", bbctx.sp, dest);
        self.branch_map.entry(dest).or_default().push(BranchEntry {
            src_idx,
            bbctx,
            branch_dest,
            cont: true,
        })
    }

    ///
    /// Add new backward branch from *src_idx* to *dest* with the context *bbctx*.
    ///
    pub(super) fn new_backedge(
        &mut self,
        func: &ISeqInfo,
        bb: &mut BBContext,
        bb_pos: BasicBlockId,
        unused: Vec<SlotId>,
    ) {
        bb.sp = func.get_sp(func.bb_info[bb_pos].begin);
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_backedge:[{:?}] {:?}", bb.sp, bb_pos);
        self.backedge_map.insert(
            bb_pos,
            BackedgeInfo {
                target_ctx: MergeContext::new(bb),
                unused,
            },
        );
    }
}
