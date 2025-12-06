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

#[derive(Debug)]
pub(super) struct SpecializeInfo {
    pub(super) entry: JitLabel,
    pub(super) frame: JitStackFrame,
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
pub(crate) struct JitStackFrame {
    ///
    /// Type of compilation for this frame.
    ///
    jit_type: JitType,
    ///
    /// Level of inlining.
    ///
    specialize_level: usize,
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
    /// Callsite Id.
    ///
    callid: Option<CallSiteId>,
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

    ///
    /// Information for `JitLabel`s`.
    ///
    labels: Vec<Option<DestLabel>>,
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
    loop_info: indexmap::IndexMap<BasicBlockId, (Liveness, Option<BBContext>)>,
    ///
    /// Nested loop count.
    ///
    loop_count: usize,

    ///
    /// Map for forward branches.
    ///
    branch_map: HashMap<BasicBlockId, Vec<BranchEntry>>,
    ///
    /// Map for target contexts of backward branches.
    ///
    backedge_map: HashMap<BasicBlockId, SlotContext>,
    ///
    /// Contexts for returning to this frame.
    ///
    pub(super) return_context: Vec<ResultState>,

    ///
    /// Generated AsmIr.
    ///
    ir: Vec<(Option<BasicBlockId>, AsmIr)>,
    ///
    /// Information for outlined bridges.
    ///
    outline_bridges: Vec<(AsmIr, JitLabel, BasicBlockId)>,
    ///
    /// Information for inlined bridges.
    ///
    inline_bridges: HashMap<Option<BasicBlockId>, (AsmIr, Option<BasicBlockId>)>,

    ///
    /// Information for specialized method / block.
    ///
    pub(super) specialized_methods: Vec<SpecializeInfo>,

    ///
    /// Flag whether ivar on the heap is accessed in this context.
    ///
    ivar_heap_accessed: bool,

    ///
    /// Source map for bytecode index and machine code position.
    ///
    #[cfg(feature = "emit-asm")]
    pub(super) sourcemap: Vec<(BcIndex, usize)>,
    ///
    /// Start offset of a machine code corresponding to the current basic block.
    ///
    #[cfg(feature = "emit-asm")]
    pub(super) start_codepos: usize,
}

impl std::fmt::Debug for JitStackFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JitStackFrame")
            .field("jit_type", &self.jit_type)
            .field("outer", &self.outer)
            .field("given_block", &self.given_block)
            .finish()
    }
}

impl Clone for JitStackFrame {
    fn clone(&self) -> Self {
        self.dup()
    }
}

impl JitStackFrame {
    pub(super) fn new(
        store: &Store,
        jit_type: JitType,
        specialize_level: usize,
        iseq_id: ISeqId,
        outer: Option<usize>,
        given_block: Option<JitBlockInfo>,
        callid: Option<CallSiteId>,
        self_class: ClassId,
    ) -> Self {
        let self_ty = store[self_class].instance_ty();
        let is_not_block = store[store[iseq_id].func_id()].is_not_block();
        let mut basic_block_labels = HashMap::default();
        let mut labels = vec![];
        for i in 0..store[iseq_id].bb_info.len() {
            let idx = BasicBlockId(i);
            basic_block_labels.insert(idx, JitLabel(labels.len()));
            labels.push(None);
        }
        Self {
            jit_type,
            specialize_level,
            iseq_id,
            outer,
            given_block,
            callid,
            self_class,
            self_ty,
            is_not_block,
            labels,
            basic_block_labels,
            loop_info: indexmap::IndexMap::default(),
            loop_count: 0,
            branch_map: HashMap::default(),
            backedge_map: HashMap::default(),
            return_context: vec![],
            ir: vec![],
            outline_bridges: vec![],
            inline_bridges: HashMap::default(),
            specialized_methods: vec![],
            ivar_heap_accessed: false,
            #[cfg(feature = "emit-asm")]
            sourcemap: vec![],
            #[cfg(feature = "emit-asm")]
            start_codepos: 0,
        }
    }

    fn dup(&self) -> Self {
        Self {
            jit_type: self.jit_type.clone(),
            specialize_level: self.specialize_level,
            iseq_id: self.iseq_id,
            outer: self.outer,
            given_block: self.given_block.clone(),
            callid: self.callid,
            self_class: self.self_class,
            self_ty: self.self_ty,
            is_not_block: self.is_not_block,
            labels: self.labels.clone(),
            basic_block_labels: self.basic_block_labels.clone(),
            loop_info: indexmap::IndexMap::default(),
            loop_count: 0,
            branch_map: HashMap::default(),
            backedge_map: HashMap::default(),
            return_context: vec![],
            ir: vec![],
            outline_bridges: vec![],
            inline_bridges: HashMap::default(),
            specialized_methods: vec![],
            ivar_heap_accessed: false,
            #[cfg(feature = "emit-asm")]
            sourcemap: vec![],
            #[cfg(feature = "emit-asm")]
            start_codepos: 0,
        }
    }

    // accessors

    pub(super) fn self_class(&self) -> ClassId {
        self.self_class
    }

    pub(super) fn self_ty(&self) -> Option<ObjTy> {
        self.self_ty
    }

    pub(super) fn iseq_id(&self) -> ISeqId {
        self.iseq_id
    }

    pub(super) fn is_specialized(&self) -> bool {
        matches!(self.jit_type, JitType::Specialized { .. })
    }

    #[cfg(any(feature = "emit-asm", feature = "jit-log"))]
    pub(super) fn specialize_level(&self) -> usize {
        self.specialize_level
    }

    pub(super) fn ivar_heap_accessed(&mut self) -> bool {
        self.ivar_heap_accessed
    }

    // bridge operations

    pub(super) fn inline_bridge_exists(&self, src_bb: BasicBlockId) -> bool {
        self.inline_bridges.contains_key(&Some(src_bb))
    }

    pub(super) fn remove_inline_bridge(
        &mut self,
        src_bb: Option<BasicBlockId>,
    ) -> Option<(AsmIr, Option<BasicBlockId>)> {
        self.inline_bridges.remove(&src_bb)
    }

    pub(super) fn detach_outline_bridges(&mut self) -> Vec<(AsmIr, JitLabel, BasicBlockId)> {
        std::mem::take(&mut self.outline_bridges)
    }

    pub(super) fn detach_ir(&mut self) -> Vec<(Option<BasicBlockId>, AsmIr)> {
        std::mem::take(&mut self.ir)
    }

    // handling labels

    ///
    /// Create a new *JitLabel*.
    ///
    pub(super) fn label(&mut self) -> JitLabel {
        let id = self.labels.len();
        self.labels.push(None);
        JitLabel(id)
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

    pub(super) fn resolve_bb_label(&mut self, jit: &mut JitMemory, bb: BasicBlockId) -> DestLabel {
        let label = self.basic_block_labels.get(&bb).copied().unwrap();
        self.resolve_label(jit, label)
    }
}

///
/// Context for JIT compilation.
///
pub struct JitContext {
    codegen_mode: bool,

    ///
    /// Class version at compile time.
    ///
    class_version: u32,
    class_version_label: DestLabel,

    ///
    /// Inline cache for method calls.
    ///
    pub(crate) inline_method_cache: HashMap<BytecodePtr, MethodCacheEntry>,
    ///
    /// Stack frame for specialized compilation. (iseq, outer_scope, block_iseq)
    ///
    pub(crate) stack_frame: Vec<JitStackFrame>,
}

impl JitContext {
    fn new(
        codegen_mode: bool,
        class_version: u32,
        class_version_label: DestLabel,
        stack_frame: Vec<JitStackFrame>,
    ) -> Self {
        Self {
            codegen_mode,
            class_version,
            class_version_label,
            inline_method_cache: HashMap::default(),
            stack_frame,
        }
    }

    pub(super) fn create(
        store: &Store,
        iseq_id: ISeqId,
        jit_type: JitType,
        class_version: u32,
        class_version_label: DestLabel,
        self_class: ClassId,
        specialize_level: usize,
        callid: Option<CallSiteId>,
    ) -> Self {
        let stack_frame = vec![JitStackFrame::new(
            store,
            jit_type,
            specialize_level,
            iseq_id,
            None,
            None,
            callid,
            self_class,
        )];

        Self::new(true, class_version, class_version_label, stack_frame)
    }

    pub(super) fn loop_analysis(&self, pc: BytecodePtr) -> Self {
        let mut stack_frame = self.stack_frame.clone();
        stack_frame.last_mut().unwrap().jit_type = JitType::Loop(pc);
        Self {
            codegen_mode: false,
            class_version: self.class_version,
            class_version_label: self.class_version_label(),
            inline_method_cache: HashMap::default(),
            stack_frame,
        }
    }

    pub(super) fn codegen_mode(&self) -> bool {
        self.codegen_mode
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

    pub(super) fn specialized_methods_len(&self) -> usize {
        self.current_frame().specialized_methods.len()
    }

    pub(super) fn specialized_methods_push(&mut self, info: SpecializeInfo) {
        self.current_frame_mut().specialized_methods.push(info);
    }

    pub(super) fn detach_return_context(&mut self) -> Vec<ResultState> {
        std::mem::take(&mut self.current_frame_mut().return_context)
    }

    fn current_frame(&self) -> &JitStackFrame {
        self.stack_frame.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut JitStackFrame {
        self.stack_frame.last_mut().unwrap()
    }

    fn caller_frame_mut(&mut self) -> Option<&mut JitStackFrame> {
        let len = self.stack_frame.len();
        if len <= 1 {
            return None;
        }
        Some(&mut self.stack_frame[len - 2])
    }

    pub(super) fn detach_current_frame(&mut self) -> JitStackFrame {
        self.stack_frame.pop().unwrap()
    }

    fn current_method_frame(&self) -> Option<(&JitStackFrame, usize)> {
        let mut i = self.stack_frame.len() - 1;
        loop {
            let frame = &self.stack_frame[i];
            if let Some(outer) = frame.outer {
                i -= outer;
            } else {
                return if frame.is_not_block {
                    Some((frame, self.stack_frame.len() - 1 - i))
                } else {
                    None
                };
            }
        }
    }

    pub(crate) fn current_method_given_block(&self) -> Option<JitBlockInfo> {
        let (frame, i) = self.current_method_frame()?;
        Some(frame.given_block.as_ref()?.add(i))
    }

    pub(crate) fn current_method_callsite(&self) -> Option<CallSiteId> {
        self.current_method_frame()?.0.callid
    }

    pub(super) fn set_ivar_heap_accessed(&mut self) {
        self.current_frame_mut().ivar_heap_accessed = true;
    }

    pub(super) fn get_pc(&self, store: &Store, i: BcIndex) -> BytecodePtr {
        store[self.iseq_id()].get_pc(i)
    }

    pub(super) fn jit_type(&self) -> &JitType {
        &self.current_frame().jit_type
    }

    pub(super) fn is_specialized(&self) -> bool {
        matches!(self.jit_type(), JitType::Specialized { .. })
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
        self.current_frame().specialize_level
    }

    pub(super) fn position(&self) -> Option<BytecodePtr> {
        match &self.jit_type() {
            JitType::Loop(pos) => Some(*pos),
            _ => None,
        }
    }
    pub(super) fn is_loop(&self) -> bool {
        matches!(self.jit_type(), JitType::Loop(_))
    }

    pub(super) fn get_bb_label(&self, bb: BasicBlockId) -> JitLabel {
        self.current_frame().get_bb_label(bb)
    }

    ///
    /// Create a new *JitLabel*.
    ///
    pub(super) fn label(&mut self) -> JitLabel {
        self.current_frame_mut().label()
    }

    pub(super) fn loop_info(
        &self,
        entry_bb: BasicBlockId,
    ) -> Option<&(Liveness, Option<BBContext>)> {
        self.current_frame().loop_info.get(&entry_bb)
    }

    pub(super) fn loop_backedge(&self, entry_bb: BasicBlockId) -> Option<&BBContext> {
        self.current_frame()
            .loop_info
            .get(&entry_bb)
            .and_then(|(_, be)| be.as_ref())
    }

    pub(super) fn add_loop_info(
        &mut self,
        entry_bb: BasicBlockId,
        liveness: Liveness,
        backedge: Option<BBContext>,
    ) {
        self.current_frame_mut()
            .loop_info
            .insert(entry_bb, (liveness, backedge));
    }

    pub(super) fn loop_count(&self) -> usize {
        self.current_frame().loop_count
    }

    pub(super) fn inc_loop_count(&mut self) {
        self.current_frame_mut().loop_count += 1;
    }

    pub(super) fn dec_loop_count(&mut self) {
        self.current_frame_mut().loop_count -= 1;
    }

    pub(super) fn branch_continue(&mut self, bb_begin: BasicBlockId, bbctx: BBContext) {
        self.current_frame_mut().branch_map.insert(
            bb_begin,
            vec![BranchEntry {
                src_bb: None,
                bbctx,
                mode: BranchMode::Continue,
            }],
        );
    }

    pub(super) fn remove_branch(&mut self, bb: BasicBlockId) -> Option<Vec<BranchEntry>> {
        self.current_frame_mut().branch_map.remove(&bb)
    }

    pub(super) fn remove_backedge(&mut self, bb: BasicBlockId) -> Option<SlotContext> {
        self.current_frame_mut().backedge_map.remove(&bb)
    }

    pub(super) fn detach_branch_map(&mut self) -> HashMap<BasicBlockId, Vec<BranchEntry>> {
        std::mem::take(&mut self.current_frame_mut().branch_map)
    }

    fn branch(
        &mut self,
        src_bb: BasicBlockId,
        dest_bb: BasicBlockId,
        bbctx: BBContext,
        mode: BranchMode,
    ) {
        self.current_frame_mut()
            .branch_map
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
        bc_pos: BcIndex,
        dest_bb: BasicBlockId,
        mut bbctx: BBContext,
    ) {
        bbctx.clear_above_next_sp();
        let src_bb = iseq.bb_info.get_bb_id(bc_pos);
        #[cfg(feature = "jit-debug")]
        eprintln!(
            "   new_branch: {bc_pos}->{dest_bb:?} {:?}",
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
        self.current_frame_mut().backedge_map.insert(bb_pos, target);
    }

    ///
    /// Add new return branch with the context *bbctx*.
    ///
    pub(super) fn new_return(&mut self, ret: ResultState) {
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_return:{:?}", ret);
        if let Some(caller_frame) = &mut self.caller_frame_mut() {
            caller_frame.return_context.push(ret);
        }
    }

    pub(super) fn push_ir(&mut self, bb: Option<BasicBlockId>, ir: AsmIr) {
        self.current_frame_mut().ir.push((bb, ir));
    }

    pub(super) fn add_inline_bridge(
        &mut self,
        src_bb: Option<BasicBlockId>,
        ir: AsmIr,
        dest_bb: Option<BasicBlockId>,
    ) {
        self.current_frame_mut()
            .inline_bridges
            .insert(src_bb, (ir, dest_bb));
    }

    pub(super) fn add_outline_bridge(&mut self, ir: AsmIr, dest: JitLabel, bbid: BasicBlockId) {
        self.current_frame_mut()
            .outline_bridges
            .push((ir, dest, bbid));
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
