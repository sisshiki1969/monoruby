//
// Just-in-time compiler module.
//

use super::*;

#[derive(Debug, Clone)]
pub(super) enum JitType {
    /// JIT for method / block.
    Entry,
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
    pub(super) info: AsmInfo,
    pub(super) patch_point: Option<JitLabel>,
}

///
/// The information of the given block for the frame.
///
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

impl JitBlockInfo {
    pub(super) fn new(block_fid: FuncId, self_class: ClassId, outer: usize) -> Self {
        Self {
            block_fid,
            self_class,
            outer,
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
pub(super) struct JitArgumentInfo(pub Option<Vec<LinkMode>>);

impl JitArgumentInfo {
    pub(super) fn new(slot: Vec<LinkMode>) -> Self {
        Self(Some(slot))
    }
}

#[derive(Debug)]
pub(super) struct AsmInfo {
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
    pub iseq_id: ISeqId,
    ///
    /// `ClassId`` of *self*.
    ///
    pub self_class: ClassId,
    ///
    /// Object type of *self*.
    ///
    pub self_ty: Option<ObjTy>,

    ///
    /// Information for `JitLabel`s`.
    ///
    labels: Vec<Option<DestLabel>>,
    ///
    /// Destination labels for each BasicBlock.
    ///
    basic_block_labels: HashMap<BasicBlockId, JitLabel>,

    ///
    /// Generated AsmIr.
    ///
    ir: Vec<(Option<BasicBlockId>, AsmIr)>,
    ///
    /// Information for inlined bridges.
    ///
    inline_bridges: HashMap<Option<BasicBlockId>, (AsmIr, Option<BasicBlockId>)>,
    ///
    /// Information for outlined bridges.
    ///
    outline_bridges: Vec<(AsmIr, JitLabel, BasicBlockId)>,

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
    pub(super) sourcemap: Vec<(BcIndex, usize)>,
    ///
    /// Start offset of a machine code corresponding to the current basic block.
    ///
    #[cfg(feature = "emit-asm")]
    pub(super) start_codepos: usize,
}

impl AsmInfo {
    fn dup(&self) -> Self {
        Self {
            jit_type: self.jit_type.clone(),
            specialize_level: self.specialize_level,
            iseq_id: self.iseq_id,
            self_class: self.self_class,
            self_ty: self.self_ty,
            labels: self.labels.clone(),
            basic_block_labels: self.basic_block_labels.clone(),
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

    pub(super) fn is_specialized(&self) -> bool {
        matches!(self.jit_type, JitType::Specialized { .. })
    }

    #[cfg(any(feature = "emit-asm", feature = "jit-log"))]
    pub(super) fn specialize_level(&self) -> usize {
        self.specialize_level
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

    pub(super) fn resolve_bb_label(&mut self, jit: &mut JitMemory, bb: BasicBlockId) -> DestLabel {
        let label = self.basic_block_labels.get(&bb).copied().unwrap();
        self.resolve_label(jit, label)
    }

    pub(super) fn detach_ir(&mut self) -> Vec<(Option<BasicBlockId>, AsmIr)> {
        std::mem::take(&mut self.ir)
    }

    // bridge operations

    pub(super) fn detach_outline_bridges(&mut self) -> Vec<(AsmIr, JitLabel, BasicBlockId)> {
        std::mem::take(&mut self.outline_bridges)
    }

    pub(super) fn inline_bridge_exists(&self, src_bb: BasicBlockId) -> bool {
        self.inline_bridges.contains_key(&Some(src_bb))
    }

    pub(super) fn remove_inline_bridge(
        &mut self,
        src_bb: Option<BasicBlockId>,
    ) -> Option<(AsmIr, Option<BasicBlockId>)> {
        self.inline_bridges.remove(&src_bb)
    }
}

///
/// Virtual Stack frame for specialized compilation.
///
pub(super) struct JitStackFrame {
    pub asm_info: AsmInfo,

    ///
    /// Outer frame. (None for methods)
    ///
    outer: Option<usize>,
    ///
    /// Callsite Id.
    ///
    callid: Option<CallSiteId>,
    ///
    /// Snapshot of `AbstractScopeState`` when the child method is called.
    ///
    abstract_state: Option<AbstractFrame>,

    ///
    /// Whether this function is a method, a class definition, or a top-level.
    ///
    is_not_block: bool,

    ///
    /// Loop information.
    ///
    /// ### key
    /// the entry basic block of the loop.
    ///
    /// ### value
    /// liveness and backedge info in the loop head.
    ///
    loop_info: indexmap::IndexMap<BasicBlockId, (Liveness, Option<AbstractState>)>,
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
    backedge_map: HashMap<BasicBlockId, SlotState>,
    ///
    /// Contexts for returning from this frame.
    ///
    return_context: HashMap<usize, ResultState>,

    ///
    /// Machine stack offset for this frame.
    ///
    stack_offset: usize,
}

impl std::fmt::Debug for JitStackFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JitStackFrame")
            .field("jit_type", &self.jit_type)
            .field("outer", &self.outer)
            .field("stack_offset", &self.stack_offset)
            .finish()
    }
}

impl std::ops::Deref for JitStackFrame {
    type Target = AsmInfo;
    fn deref(&self) -> &AsmInfo {
        &self.asm_info
    }
}

impl std::ops::DerefMut for JitStackFrame {
    fn deref_mut(&mut self) -> &mut AsmInfo {
        &mut self.asm_info
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
        self_class: ClassId,
        abstract_state: Option<AbstractFrame>,
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
        let stack_offset = store[iseq_id].stack_offset();
        Self {
            asm_info: AsmInfo {
                jit_type,
                specialize_level,
                iseq_id,
                self_class,
                self_ty,
                labels,
                basic_block_labels,
                ir: vec![],
                outline_bridges: vec![],
                inline_bridges: HashMap::default(),
                ivar_heap_accessed: false,
                specialized_methods: vec![],
                #[cfg(feature = "emit-asm")]
                sourcemap: vec![],
                #[cfg(feature = "emit-asm")]
                start_codepos: 0,
            },
            outer,
            callid: None,
            abstract_state,
            is_not_block,
            loop_info: indexmap::IndexMap::default(),
            loop_count: 0,
            branch_map: HashMap::default(),
            backedge_map: HashMap::default(),
            return_context: HashMap::default(),
            stack_offset,
        }
    }

    fn dup(&self) -> Self {
        Self {
            asm_info: self.asm_info.dup(),
            outer: self.outer,
            callid: self.callid,
            abstract_state: self.abstract_state.clone(),
            is_not_block: self.is_not_block,
            loop_info: indexmap::IndexMap::default(),
            loop_count: 0,
            branch_map: HashMap::default(),
            backedge_map: HashMap::default(),
            return_context: HashMap::default(),
            stack_offset: self.stack_offset,
        }
    }

    // accessors

    pub(super) fn iseq_id(&self) -> ISeqId {
        self.iseq_id
    }

    // bridge operations

    pub(super) fn detach_return_context(&mut self) -> HashMap<usize, ResultState> {
        std::mem::take(&mut self.return_context)
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

    pub(super) fn get_bb_label(&self, bb: BasicBlockId) -> JitLabel {
        self.basic_block_labels.get(&bb).copied().unwrap()
    }
}

///
/// Context for JIT compilation.
///
pub(crate) struct JitContext<'a> {
    pub store: &'a Store,
    codegen_mode: bool,

    ///
    /// Class version at compile time.
    ///
    class_version: u32,
    class_version_label: DestLabel,

    ///
    /// Inline cache for method calls.
    ///
    pub(crate) inline_method_cache: Vec<(ClassId, Option<IdentId>, FuncId)>,
    ///
    /// Stack frame for specialized compilation. (iseq, outer_scope, block_iseq)
    ///
    stack_frame: Vec<JitStackFrame>,
}

impl<'a> JitContext<'a> {
    pub(super) fn new(
        store: &'a Store,
        codegen_mode: bool,
        class_version: u32,
        class_version_label: DestLabel,
        stack_frame: Vec<JitStackFrame>,
    ) -> Self {
        Self {
            store,
            codegen_mode,
            class_version,
            class_version_label,
            inline_method_cache: vec![],
            stack_frame,
        }
    }

    pub(super) fn loop_analysis(&self, pc: BytecodePtr) -> Self {
        let mut stack_frame = self.stack_frame.clone();
        stack_frame.last_mut().unwrap().jit_type = JitType::Loop(pc);
        Self {
            store: self.store,
            codegen_mode: false,
            class_version: self.class_version,
            class_version_label: self.class_version_label(),
            inline_method_cache: vec![],
            stack_frame,
        }
    }

    pub(super) fn codegen_mode(&self) -> bool {
        self.codegen_mode
    }

    pub(super) fn iseq_id(&self) -> ISeqId {
        self.current_frame().iseq_id
    }

    pub(super) fn iseq(&self) -> &ISeqInfo {
        &self.store[self.current_frame().iseq_id]
    }

    #[cfg(feature = "emit-bc")]
    pub(super) fn dump_iseq(&self) {
        self.store.dump_iseq(self.iseq_id());
    }

    pub(super) fn func_id(&self) -> FuncId {
        self.iseq().func_id()
    }

    pub(super) fn self_class(&self) -> ClassId {
        self.current_frame().self_class
    }

    pub(super) fn self_ty(&self) -> Option<ObjTy> {
        self.current_frame().self_ty
    }

    pub(super) fn is_block(&self) -> bool {
        !self.store[self.func_id()].is_not_block()
    }

    pub(super) fn specialized_methods_len(&self) -> usize {
        self.current_frame().specialized_methods.len()
    }

    pub(super) fn specialized_methods_push(&mut self, info: SpecializeInfo) {
        self.current_frame_mut().specialized_methods.push(info);
    }

    pub(super) fn push_return_context(&mut self, pos: usize, ret: ResultState) {
        if let Some(frame) = self.current_frame_mut().return_context.get_mut(&pos) {
            frame.join(&ret);
        } else {
            self.current_frame_mut().return_context.insert(pos, ret);
        }
    }

    pub(super) fn unset_return_context_side_effect_guard(&mut self) {
        let pos = if let Some(pos) = self.caller_pos() {
            pos
        } else {
            return;
        };
        if let Some(frame) = self.current_frame_mut().return_context.get_mut(&pos) {
            frame.unset_side_effect_guard();
        } else {
            self.current_frame_mut()
                .return_context
                .insert(pos, ResultState::may_side_effect());
        }
    }

    pub(super) fn merge_return_context(&mut self, context: HashMap<usize, ResultState>) {
        for (pos, res) in context {
            if let Some(frame) = self.current_frame_mut().return_context.get_mut(&pos) {
                frame.join(&res);
            } else {
                self.current_frame_mut().return_context.insert(pos, res);
            }
        }
    }

    // handling frame
    fn current_frame(&self) -> &JitStackFrame {
        self.stack_frame.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut JitStackFrame {
        self.stack_frame.last_mut().unwrap()
    }

    pub(super) fn current_frame_pos(&self) -> usize {
        self.stack_frame.len() - 1
    }

    pub(super) fn push_frame(&mut self, frame: JitStackFrame) {
        self.stack_frame.push(frame);
    }

    pub(super) fn pop_frame(&mut self) -> JitStackFrame {
        self.stack_frame.pop().unwrap()
    }

    pub(super) fn specialized_compile(
        &mut self,
        state: &mut AbstractFrame,
        callid: CallSiteId,
        frame: JitStackFrame,
    ) -> JitStackFrame {
        let stack_offset = state.get_using_xmm().offset();
        let current = self.current_frame_mut();
        current.stack_offset += stack_offset;
        current.callid = Some(callid);
        let scope = std::mem::take(state);
        assert!(std::mem::replace(&mut current.abstract_state, Some(scope)).is_none());

        let frame = self.traceir_to_asmir(frame);

        let current = self.current_frame_mut();
        *state = current.abstract_state.take().unwrap();
        current.callid = None;
        current.stack_offset -= stack_offset;
        frame
    }

    pub(crate) fn current_method_given_block(&self) -> Option<JitBlockInfo> {
        let caller = self.method_caller_pos()?;
        let callid = self.stack_frame[caller].callid?;
        let block_fid = self.store[callid].block_fid?;
        let self_class = self.stack_frame[caller].self_class;
        let outer = self.stack_frame.len() - 1 - caller;
        Some(JitBlockInfo::new(block_fid, self_class, outer))
    }

    pub(crate) fn method_caller_callsite(&self) -> Option<CallSiteId> {
        let caller = self.method_caller_pos()?;
        self.stack_frame[caller].callid
    }

    pub(super) fn set_ivar_heap_accessed(&mut self) {
        self.current_frame_mut().ivar_heap_accessed = true;
    }

    fn caller_pos(&self) -> Option<usize> {
        let len = self.stack_frame.len();
        if len < 2 {
            return None;
        }
        Some(len - 2)
    }

    fn method_caller_pos(&self) -> Option<usize> {
        let offset = self.current_method_frame()?.1 + 1;
        let len = self.stack_frame.len();
        if len - 1 < offset {
            return None;
        }
        Some(len - 1 - offset)
    }

    fn iter_caller_pos(&self) -> Option<usize> {
        let len = self.stack_frame.len();
        if len < 3 {
            return None;
        }
        Some(len - 3)
    }

    fn outer_pos(&self, outer: usize) -> Option<usize> {
        let mut i = self.stack_frame.len() - 1;
        for _ in 0..outer {
            i -= self.stack_frame[i].outer?;
        }
        Some(i)
    }

    pub(super) fn outer_contexts(&self) -> Vec<AbstractFrame> {
        let mut i = self.stack_frame.len() - 1;
        let mut v = vec![];
        while let Some(outer) = self.stack_frame[i].outer {
            i -= outer;
            let scope = self.stack_frame[i].abstract_state.clone().unwrap();
            v.push(scope);
        }
        v
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

    ///
    /// Unset frame capture guard in the outer `JitFrame`s.
    ///
    pub(super) fn unset_outer_no_capture_guard(&mut self) {
        let mut i = self.stack_frame.len() - 1;
        loop {
            let frame = &mut self.stack_frame[i];
            if let Some(outer) = frame.outer {
                i -= outer;
                self.stack_frame[i]
                    .abstract_state
                    .as_mut()
                    .unwrap()
                    .unset_no_capture_guard();
            } else {
                return;
            }
        }
    }

    fn calc_stack_offset(&self, begin: usize, end: usize) -> usize {
        self.stack_frame[begin..end]
            .iter()
            .fold(0, |acc, f| acc + f.stack_offset)
    }

    fn check_exception_handler(&self, begin: usize, end: usize) -> bool {
        self.stack_frame[begin..end].iter().any(|f| {
            let iseq_id = f.iseq_id();
            let callsite = f.callid.unwrap();
            let pc = self.store[callsite].bc_pos;
            self.store[iseq_id].get_exception_dest(pc).is_some()
        })
    }

    pub(super) fn outer_stack_offset(
        &self,
        state: &AbstractState,
        outer: usize,
    ) -> Option<(usize, bool)> {
        let not_captured = state.outer_no_capture_guard(outer)?;
        let outer = self.outer_pos(outer)?;
        let offset = self.calc_stack_offset(outer, self.stack_frame.len() - 1);
        Some((offset, not_captured))
    }

    pub(super) fn method_caller_stack_offset(&self) -> Option<usize> {
        let caller = self.method_caller_pos()?;
        let begin = caller + 1;
        let end = self.stack_frame.len() - 1;
        if self.check_exception_handler(begin, end) {
            return None;
        }
        Some(self.calc_stack_offset(begin, end))
    }

    pub(super) fn iter_caller_stack_offset(&self) -> Option<usize> {
        let caller = self.iter_caller_pos()?;
        let begin = caller + 1;
        let end = self.stack_frame.len() - 1;
        if self.check_exception_handler(begin, end) {
            return None;
        }
        Some(self.calc_stack_offset(begin, end))
    }

    pub(super) fn get_pc(&self, i: BcIndex) -> BytecodePtr {
        self.iseq().get_pc(i)
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
    pub(super) fn local_num(&self) -> usize {
        self.iseq().local_num()
    }

    pub(super) fn locals(&self) -> std::ops::Range<SlotId> {
        SlotId(1)..SlotId(self.local_num() as u16 + 1)
    }

    pub(super) fn args(&self) -> std::ops::Range<SlotId> {
        SlotId(1)..SlotId(self.store[self.func_id()].params().total_args() as u16 + 1)
    }

    ///
    /// Get a number of slots. (including `self`, arguments, local variables, and temp registers)
    ///
    pub(super) fn total_reg_num(&self) -> usize {
        self.iseq().total_reg_num()
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
    ) -> Option<&(Liveness, Option<AbstractState>)> {
        self.current_frame().loop_info.get(&entry_bb)
    }

    pub(super) fn loop_backedge(&self, entry_bb: BasicBlockId) -> Option<&AbstractState> {
        self.current_frame()
            .loop_info
            .get(&entry_bb)
            .and_then(|(_, be)| be.as_ref())
    }

    pub(super) fn add_loop_info(
        &mut self,
        entry_bb: BasicBlockId,
        liveness: Liveness,
        backedge: Option<AbstractState>,
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

    pub(super) fn branch_continue(&mut self, bb_begin: BasicBlockId, state: AbstractState) {
        self.current_frame_mut().branch_map.insert(
            bb_begin,
            vec![BranchEntry {
                src_bb: None,
                state,
                mode: BranchMode::Continue,
            }],
        );
    }

    pub(super) fn remove_branch(&mut self, bb: BasicBlockId) -> Option<Vec<BranchEntry>> {
        self.current_frame_mut().branch_map.remove(&bb)
    }

    pub(super) fn remove_backedge(&mut self, bb: BasicBlockId) -> Option<SlotState> {
        self.current_frame_mut().backedge_map.remove(&bb)
    }

    pub(super) fn detach_branch_map(&mut self) -> HashMap<BasicBlockId, Vec<BranchEntry>> {
        std::mem::take(&mut self.current_frame_mut().branch_map)
    }

    fn branch(
        &mut self,
        src_bb: BasicBlockId,
        dest_bb: BasicBlockId,
        state: AbstractState,
        mode: BranchMode,
    ) {
        self.current_frame_mut()
            .branch_map
            .entry(dest_bb)
            .or_default()
            .push(BranchEntry {
                src_bb: Some(src_bb),
                state,
                mode,
            });
    }

    ///
    /// Add new branch from *src_idx* to *dest* with `state`.
    ///
    pub(super) fn new_side_branch(
        &mut self,
        src_idx: BcIndex,
        dest_bb: BasicBlockId,
        mut state: AbstractState,
        dest: JitLabel,
    ) {
        state.clear_above_next_sp();
        let src_bb = self.iseq().bb_info.get_bb_id(src_idx);
        #[cfg(feature = "jit-debug")]
        eprintln!(
            "   new_side branch: {src_idx}->{dest_bb:?} {:?}",
            state.slot_state()
        );
        self.branch(src_bb, dest_bb, state, BranchMode::Side { dest });
    }

    ///
    /// Add new branch from *src_idx* to *dest* with `state`.
    ///
    pub(super) fn new_branch(
        &mut self,
        bc_pos: BcIndex,
        dest_bb: BasicBlockId,
        mut state: AbstractState,
    ) {
        state.clear_above_next_sp();
        let src_bb = self.iseq().bb_info.get_bb_id(bc_pos);
        #[cfg(feature = "jit-debug")]
        eprintln!(
            "   new_branch: {bc_pos}->{dest_bb:?} {:?}",
            state.slot_state()
        );
        self.branch(src_bb, dest_bb, state, BranchMode::Branch);
    }

    ///
    /// Add new continuation branch from *src_idx* to *dest* with `state`.
    ///
    pub(super) fn new_continue(
        &mut self,
        src_idx: BcIndex,
        dest_bb: BasicBlockId,
        mut state: AbstractState,
    ) {
        state.clear_above_next_sp();
        let src_bb = self.iseq().bb_info.get_bb_id(src_idx);
        #[cfg(feature = "jit-debug")]
        eprintln!(
            "   new_continue: {src_idx}->{dest_bb:?} {:?}",
            state.slot_state()
        );
        self.branch(src_bb, dest_bb, state, BranchMode::Continue);
    }

    ///
    /// Add new backward branch from *src_idx* to *dest* with `state`.
    ///
    pub(super) fn new_backedge(&mut self, target: SlotState, bb_pos: BasicBlockId) {
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_backedge:{bb_pos:?} {target:?}");
        self.current_frame_mut().backedge_map.insert(bb_pos, target);
    }

    ///
    /// Add new return branch with `state`.
    ///
    pub(super) fn new_return(&mut self, ret: ResultState) {
        if let Some(pos) = self.caller_pos() {
            #[cfg(feature = "jit-debug")]
            eprintln!("   new_return:{:?}", ret);
            self.push_return_context(pos, ret);
        }
    }

    ///
    /// Add new return branch with `state`.
    ///
    pub(super) fn new_method_return(&mut self, ret: ResultState) {
        if let Some(pos) = self.method_caller_pos() {
            #[cfg(feature = "jit-debug")]
            eprintln!("   new_method_return:{:?}", ret);
            self.push_return_context(pos, ret);
        }
    }

    ///
    /// Add new return branch with `state`.
    ///
    pub(super) fn new_break(&mut self, ret: ResultState) {
        if let Some(pos) = self.iter_caller_pos() {
            #[cfg(feature = "jit-debug")]
            eprintln!("   new_break:{:?}", ret);
            self.push_return_context(pos, ret);
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
}
