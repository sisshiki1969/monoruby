//
// Just-in-time compiler module.
//

use super::*;

#[derive(Debug, Clone)]
pub(super) enum JitType {
    /// JIT for method / block.
    Method,
    /// JIT for loop.
    Loop(BytecodePtr),
    /// specialized JIT method.
    Specialized(usize),
}

pub(super) struct SpecializeInfo {
    pub(super) entry: JitLabel,
    pub(super) ctx: JitContext,
    pub(super) patch_point: Option<JitLabel>,
}

///
/// Context for JIT compilation.
///
pub struct JitContext {
    ///
    /// IseqId of the method / block.
    ///
    iseq_id: ISeqId,
    ///
    /// The block given to the method and its `self` class.
    ///
    block_info: Option<method_call::JitBlockInfo>,
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
    /// Number of non-temp registers. (includes arguments and local variables, not `self`)
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
    specialize_level: usize,

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
    /// liveness and backedge info in the loop head.
    ///
    pub(super) loop_info: HashMap<BasicBlockId, (Liveness, Option<BBContext>)>,
    ///
    /// Nested loop count.
    ///
    pub(super) loop_count: usize,
    ///
    /// Map for bytecode position and branches.
    ///
    pub(super) branch_map: HashMap<BasicBlockId, Vec<BranchEntry>>,
    ///
    /// Map for backward branches.
    ///
    pub(super) backedge_map: HashMap<BasicBlockId, BBContext>,
    ///
    /// Information for bridges.
    ///
    pub(super) outline_bridges: Vec<(AsmIr, JitLabel, BasicBlockId)>,
    ///
    /// Information for continue bridges.
    ///
    pub(super) inline_bridges: HashMap<BasicBlockId, (AsmIr, Option<BasicBlockId>)>,
    ///
    /// Information for `JitLabel`s`.
    ///
    labels: Vec<Option<DestLabel>>,
    ///
    /// Generated AsmIr.
    ///
    pub(super) ir: Vec<(Option<BasicBlockId>, AsmIr)>,
    ///
    /// Flag whether ivar on the heap is accessed in this context.
    ///
    pub ivar_heap_accessed: bool,
    ///
    /// Information for specialized method / block.
    ///
    pub(super) specialized_methods: Vec<SpecializeInfo>,
    ///
    /// Source map for bytecode index and machine code position.
    ///
    #[cfg(feature = "emit-asm")]
    pub(crate) sourcemap: Vec<(BcIndex, usize)>,
    ///
    /// Start offset of a machine code corresponding to the current basic block.
    ///
    #[cfg(feature = "emit-asm")]
    pub(crate) start_codepos: usize,
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
        specialize_level: usize,
        block_info: Option<method_call::JitBlockInfo>,
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
            backedge_map: HashMap::default(),
            total_reg_num,
            local_num,
            self_class,
            self_ty,
            outline_bridges: vec![],
            inline_bridges: HashMap::default(),
            labels,
            class_version,
            ir: vec![],
            ivar_heap_accessed: false,
            specialize_level,
            specialized_methods: vec![],
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
            block_info: self.block_info.clone(),
            jit_type: self.jit_type.clone(),
            basic_block_labels: HashMap::default(),
            loop_info: HashMap::default(),
            loop_count: 0,
            branch_map: HashMap::default(),
            backedge_map: HashMap::default(),
            total_reg_num,
            local_num,
            self_class: NIL_CLASS,
            self_ty: None,
            outline_bridges: vec![],
            inline_bridges: HashMap::default(),
            labels: vec![],
            class_version: 0,
            ir: vec![],
            ivar_heap_accessed: false,
            specialize_level: 0,
            specialized_methods: vec![],
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

    pub(super) fn is_specialized(&self) -> bool {
        matches!(self.jit_type, JitType::Specialized(_))
    }

    pub(super) fn block_info(&self) -> &Option<method_call::JitBlockInfo> {
        &self.block_info
    }

    pub fn has_block_info(&self) -> bool {
        self.block_info.is_some()
    }

    ///
    /// Get a number of non-temp registers. (includes arguments and local variables, not self)
    ///
    pub(super) fn local_num(&self) -> usize {
        self.local_num
    }

    ///
    /// Get a number of slots. (including `self`, arguments, local variables, and temp registers)
    ///
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

    pub(super) fn specialize_level(&self) -> usize {
        self.specialize_level
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

    pub(super) fn loop_info(
        &self,
        entry_bb: BasicBlockId,
    ) -> (Vec<(SlotId, bool)>, Vec<SlotId>, Option<BBContext>) {
        match self.loop_info.get(&entry_bb) {
            Some((liveness, merger)) => (
                liveness.get_loop_used_as_float(),
                liveness.get_unused(),
                merger.clone(),
            ),
            None => (vec![], vec![], None),
        }
    }

    ///
    /// Add new branch from *src_idx* to *dest* with the context *bbctx*.
    ///
    pub(super) fn new_side_branch(
        &mut self,
        iseq: &ISeqInfo,
        src_idx: BcIndex,
        dest_bb: BasicBlockId,
        mut bbctx: BBContext,
        dest: JitLabel,
    ) {
        bbctx.clear_above_next_sp();
        bbctx.sp = iseq.get_sp(src_idx);
        let src_bb = iseq.bb_info.get_bb_id(src_idx);
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_branch: [{:?}]{src_idx}->{:?}", bbctx.sp, dest_bb);
        self.branch_map
            .entry(dest_bb)
            .or_default()
            .push(BranchEntry {
                src_bb,
                bbctx,
                mode: BranchMode::Side { dest },
            });
    }

    ///
    /// Add new branch from *src_idx* to *dest* with the context *bbctx*.
    ///
    pub(super) fn new_branch(
        &mut self,
        iseq: &ISeqInfo,
        src_idx: BcIndex,
        dest_bb: BasicBlockId,
        mut bbctx: BBContext,
    ) {
        bbctx.clear_above_next_sp();
        bbctx.sp = iseq.get_sp(src_idx);
        let src_bb = iseq.bb_info.get_bb_id(src_idx);
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_branch: [{:?}]{src_idx}->{:?}", bbctx.sp, dest_bb);
        self.branch_map
            .entry(dest_bb)
            .or_default()
            .push(BranchEntry {
                src_bb,
                bbctx,
                mode: BranchMode::Branch,
            });
    }

    ///
    /// Add new continuation branch from *src_idx* to *dest* with the context *bbctx*.
    ///
    pub(super) fn new_continue(
        &mut self,
        iseq: &ISeqInfo,
        src_idx: BcIndex,
        dest_bb: BasicBlockId,
        mut bbctx: BBContext,
    ) {
        bbctx.clear_above_next_sp();
        bbctx.sp = iseq.get_sp(src_idx);
        let src_bb = iseq.bb_info.get_bb_id(src_idx);
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_continue:[{:?}] {src_idx}->{:?}", bbctx.sp, dest_bb);
        self.branch_map
            .entry(dest_bb)
            .or_default()
            .push(BranchEntry {
                src_bb,
                bbctx,
                mode: BranchMode::Continue,
            })
    }

    ///
    /// Add new backward branch from *src_idx* to *dest* with the context *bbctx*.
    ///
    pub(super) fn new_backedge(
        &mut self,
        iseq: &ISeqInfo,
        mut target: BBContext,
        bb_pos: BasicBlockId,
    ) {
        target.sp = iseq.get_sp(iseq.bb_info[bb_pos].begin);
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_backedge:[{:?}] {:?}", target.sp, bb_pos);
        self.backedge_map.insert(bb_pos, target);
    }

    ///
    /// Check whether a method or `super` of class *class_id* exists in compile time.
    ///
    pub(super) fn jit_check_call(
        &mut self,
        store: &Store,
        recv_class: ClassId,
        name: Option<IdentId>,
    ) -> Option<FuncId> {
        if let Some(name) = name {
            // for method call
            self.jit_check_method(store, recv_class, name)
        } else {
            // for super
            self.jit_check_super(store, recv_class)
        }
    }

    ///
    /// Check whether a method *name* of class *class_id* exists in compile time.
    ///
    pub(super) fn jit_check_method(
        &self,
        store: &Store,
        class_id: ClassId,
        name: IdentId,
    ) -> Option<FuncId> {
        let class_version = self.class_version;
        store
            .check_method_for_class(class_id, name, class_version)?
            .func_id()
    }

    ///
    /// Check whether `super` of class *class_id* exists in compile time.
    ///
    fn jit_check_super(&mut self, store: &Store, recv_class: ClassId) -> Option<FuncId> {
        // for super
        let iseq_id = self.iseq_id;
        let mother = store[iseq_id].mother().0;
        let class_context = store[mother].owner_class().unwrap();
        let func_name = store[mother].name().unwrap();
        store.check_super(recv_class, class_context, func_name)
    }
}
