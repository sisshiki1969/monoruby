//
// Just-in-time compiler module.
//

use super::*;

#[derive(Debug, Clone)]
pub(super) enum JitType {
    /// JIT for method / block.
    Generic,
    /// specialized JIT for method / block.
    Specialized {
        idx: usize,
        args_info: JitArgumentInfo,
    },
    /// JIT for loop.
    Loop(BytecodePtr),
}

pub(super) struct SpecializeInfo {
    pub(super) entry: JitLabel,
    pub(super) ctx: JitContext,
    pub(super) patch_point: Option<JitLabel>,
}

///
/// The information of the given block for the frame.
///
#[derive(Clone)]
pub struct JitBlockInfo {
    ///
    /// `FuncId` of the block.
    ///
    pub block_fid: FuncId,
    ///
    /// `ClassId` of the *self*.
    ///
    pub self_class: ClassId,
    ///
    /// Offset of the outer frame. this must be > 0.
    ///
    pub outer: usize,
}

impl std::fmt::Debug for JitBlockInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Block {{ {:?}, self:{:?}, outer:{} }}",
            self.block_fid, self.self_class, self.outer
        )
    }
}

impl JitBlockInfo {
    pub(super) fn new(block_fid: FuncId, self_class: ClassId) -> Self {
        Self {
            block_fid,
            self_class,
            outer: 1,
        }
    }

    pub(super) fn add(&self, outer: usize) -> Self {
        Self {
            block_fid: self.block_fid,
            self_class: self.self_class,
            outer: self.outer + outer,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub(super) struct JitArgumentInfo(pub Option<Vec<slot::LinkMode>>);

impl JitArgumentInfo {
    pub(super) fn new(slot: Vec<slot::LinkMode>) -> Self {
        Self(Some(slot))
    }
}

///
/// Virtual Stack frame for specialized compilation.
///
#[derive(Debug, Clone)]
pub(crate) struct JitStackFrame {
    ///
    /// `ISeqId` of the frame.
    ///
    iseq_id: ISeqId,
    ///
    /// Outer frame. (None for methods)
    ///
    outer: Option<usize>,
    ///
    /// Given block for the frame.
    ///
    given_block: Option<JitBlockInfo>,
    ///
    /// `ClassId`` of *self*.
    ///
    self_class: ClassId,
    ///
    /// Object type of *self*.
    ///
    self_ty: Option<ObjTy>,
    ///
    /// Whether this function is a method, a class definition, or a top-level.
    ///
    is_not_block: bool,
}

impl JitStackFrame {
    pub fn new(
        iseq_id: ISeqId,
        outer: Option<usize>,
        given_block: Option<JitBlockInfo>,
        self_class: ClassId,
        self_ty: Option<ObjTy>,
        is_not_block: bool,
    ) -> Self {
        Self {
            iseq_id,
            outer,
            given_block,
            self_class,
            self_ty,
            is_not_block,
        }
    }
    pub fn given_block(&self) -> Option<&JitBlockInfo> {
        self.given_block.as_ref()
    }
}

///
/// Context for JIT compilation.
///
pub struct JitContext {
    ///
    /// Type of compilation for this frame.
    ///
    jit_type: JitType,
    ///
    /// Class version at compile time.
    ///
    class_version: u32,
    class_version_label: DestLabel,
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
    pub(super) loop_info: indexmap::IndexMap<BasicBlockId, (Liveness, Option<BBContext>)>,
    ///
    /// Nested loop count.
    ///
    pub(super) loop_count: usize,
    ///
    /// Map for bytecode position and branches.
    ///
    pub(super) branch_map: HashMap<BasicBlockId, Vec<BranchEntry>>,
    ///
    /// Map for target contexts of backward branches.
    ///
    pub(super) backedge_map: HashMap<BasicBlockId, SlotContext>,
    ///
    /// Information for outlined bridges.
    ///
    pub(super) outline_bridges: Vec<(AsmIr, JitLabel, BasicBlockId)>,
    ///
    /// Information for inlined bridges.
    ///
    pub(super) inline_bridges: HashMap<Option<BasicBlockId>, (AsmIr, Option<BasicBlockId>)>,
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
    /// Inline cache for method calls.
    ///
    pub(crate) inline_method_cache: HashMap<BytecodePtr, MethodCacheEntry>,
    ///
    /// Stack frame for specialized compilation. (iseq, outer_scope, block_iseq)
    ///
    pub(crate) stack_frame: Vec<JitStackFrame>,
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
    pub(super) fn new(
        store: &Store,
        iseq_id: ISeqId,
        jit_type: JitType,
        class_version: u32,
        class_version_label: DestLabel,
        self_class: ClassId,
        specialize_level: usize,
    ) -> Self {
        let self_ty = store[self_class].instance_ty();
        let is_not_block = store[store[iseq_id].func_id()].is_not_block();
        Self::new_with_stack_frame(
            store,
            iseq_id,
            jit_type,
            class_version,
            class_version_label,
            specialize_level,
            vec![JitStackFrame {
                iseq_id,
                outer: None,
                given_block: None,
                self_class,
                self_ty,
                is_not_block,
            }],
        )
    }

    ///
    /// Create new JitContext.
    ///
    pub(super) fn new_with_stack_frame(
        store: &Store,
        iseq_id: ISeqId,
        jit_type: JitType,
        class_version: u32,
        class_version_label: DestLabel,
        specialize_level: usize,
        stack_frame: Vec<JitStackFrame>,
    ) -> Self {
        let iseq = &store[iseq_id];
        let mut basic_block_labels = HashMap::default();
        let mut labels = vec![];
        for i in 0..iseq.bb_info.len() {
            let idx = BasicBlockId(i);
            basic_block_labels.insert(idx, JitLabel(labels.len()));
            labels.push(None);
        }

        Self {
            jit_type,
            basic_block_labels,
            loop_info: indexmap::IndexMap::default(),
            loop_count: 0,
            branch_map: HashMap::default(),
            backedge_map: HashMap::default(),
            outline_bridges: vec![],
            inline_bridges: HashMap::default(),
            labels,
            class_version,
            class_version_label,
            ir: vec![],
            ivar_heap_accessed: false,
            specialize_level,
            specialized_methods: vec![],
            inline_method_cache: HashMap::default(),
            stack_frame,
            #[cfg(feature = "emit-asm")]
            sourcemap: vec![],
            #[cfg(feature = "emit-asm")]
            start_codepos: 0,
        }
    }

    pub(super) fn loop_analysis(&self, pc: BytecodePtr) -> Self {
        Self {
            jit_type: JitType::Loop(pc),
            basic_block_labels: HashMap::default(),
            loop_info: indexmap::IndexMap::default(),
            loop_count: 0,
            branch_map: HashMap::default(),
            backedge_map: HashMap::default(),
            outline_bridges: vec![],
            inline_bridges: HashMap::default(),
            labels: vec![],
            class_version: 0,
            class_version_label: self.class_version_label(),
            ir: vec![],
            ivar_heap_accessed: false,
            specialize_level: 0,
            specialized_methods: vec![],
            inline_method_cache: HashMap::default(),
            stack_frame: self.stack_frame.clone(),
            #[cfg(feature = "emit-asm")]
            sourcemap: vec![],
            #[cfg(feature = "emit-asm")]
            start_codepos: 0,
        }
    }

    pub(super) fn iseq_id(&self) -> ISeqId {
        self.current_frame().iseq_id
    }

    pub(super) fn self_class(&self) -> ClassId {
        self.current_frame().self_class
    }

    pub(super) fn self_ty(&self) -> Option<ObjTy> {
        self.current_frame().self_ty
    }

    /// Whether this function is a method, a class definition, or a top-level.
    pub(super) fn is_not_block(&self) -> bool {
        self.current_frame().is_not_block
    }

    pub(crate) fn current_frame(&self) -> &JitStackFrame {
        self.stack_frame.last().unwrap()
    }

    pub(crate) fn current_method_given_block(&self) -> Option<JitBlockInfo> {
        let mut i = self.stack_frame.len() - 1;
        loop {
            let frame = &self.stack_frame[i];
            if let Some(outer) = frame.outer {
                i -= outer;
            } else {
                return if frame.is_not_block {
                    let block = frame.given_block()?.clone();
                    Some(block.add(self.stack_frame.len() - 1 - i))
                } else {
                    None
                };
            }
        }
    }

    pub(crate) fn current_method_block_given(&self) -> Option<bool> {
        let mut i = self.stack_frame.len() - 1;
        loop {
            let frame = &self.stack_frame[i];
            if let Some(outer) = frame.outer {
                i -= outer;
            } else {
                return if frame.is_not_block {
                    Some(frame.given_block.is_some())
                } else {
                    None
                };
            }
        }
    }

    pub(super) fn get_pc(&self, store: &Store, i: BcIndex) -> BytecodePtr {
        store[self.iseq_id()].get_pc(i)
    }

    pub(super) fn jit_type(&self) -> &JitType {
        &self.jit_type
    }

    pub(super) fn is_specialized(&self) -> bool {
        matches!(self.jit_type, JitType::Specialized { .. })
    }

    ///
    /// Get a number of non-temp registers. (includes arguments and local variables, not self)
    ///
    pub(super) fn local_num(&self, store: &Store) -> usize {
        store[self.iseq_id()].local_num()
    }

    ///
    /// Get a number of slots. (including `self`, arguments, local variables, and temp registers)
    ///
    pub(super) fn total_reg_num(&self, store: &Store) -> usize {
        store[self.iseq_id()].total_reg_num()
    }

    pub(crate) fn class_version(&self) -> u32 {
        self.class_version
    }

    pub(crate) fn class_version_label(&self) -> DestLabel {
        self.class_version_label.clone()
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
        match &self.labels[label.0] {
            Some(l) => l.clone(),
            None => {
                let l = jit.label();
                self.labels[label.0] = Some(l.clone());
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
    ) -> Option<&(Liveness, Option<BBContext>)> {
        self.loop_info.get(&entry_bb)
    }

    pub(super) fn loop_backedge(&self, entry_bb: BasicBlockId) -> Option<&BBContext> {
        self.loop_info
            .get(&entry_bb)
            .and_then(|(_, be)| be.as_ref())
    }

    fn branch(
        &mut self,
        src_bb: BasicBlockId,
        dest_bb: BasicBlockId,
        bbctx: BBContext,
        mode: BranchMode,
    ) {
        self.branch_map
            .entry(dest_bb)
            .or_default()
            .push(BranchEntry {
                src_bb: Some(src_bb),
                bbctx,
                mode,
            });
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
        let src_bb = iseq.bb_info.get_bb_id(src_idx);
        #[cfg(feature = "jit-debug")]
        eprintln!(
            "   new_side branch: {src_idx}->{dest_bb:?} {:?}",
            bbctx.slot_state
        );
        self.branch(src_bb, dest_bb, bbctx, BranchMode::Side { dest });
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
        let src_bb = iseq.bb_info.get_bb_id(src_idx);
        #[cfg(feature = "jit-debug")]
        eprintln!(
            "   new_branch: {src_idx}->{dest_bb:?} {:?}",
            bbctx.slot_state
        );
        self.branch(src_bb, dest_bb, bbctx, BranchMode::Branch);
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
        let src_bb = iseq.bb_info.get_bb_id(src_idx);
        #[cfg(feature = "jit-debug")]
        eprintln!(
            "   new_continue: {src_idx}->{dest_bb:?} {:?}",
            bbctx.slot_state
        );
        self.branch(src_bb, dest_bb, bbctx, BranchMode::Continue);
    }

    ///
    /// Add new backward branch from *src_idx* to *dest* with the context *bbctx*.
    ///
    pub(super) fn new_backedge(&mut self, target: SlotContext, bb_pos: BasicBlockId) {
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_backedge:{bb_pos:?} {target:?}");
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
            .check_method_for_class_with_version(class_id, name, class_version)?
            .func_id()
    }

    ///
    /// Check whether `super` of class *class_id* exists in compile time.
    ///
    fn jit_check_super(&mut self, store: &Store, recv_class: ClassId) -> Option<FuncId> {
        // for super
        let iseq_id = self.iseq_id();
        let mother = store[iseq_id].mother().0;
        let owner = store[mother].owner_class().unwrap();
        let func_name = store[mother].name().unwrap();
        store.check_super(recv_class, owner, func_name)
    }
}
