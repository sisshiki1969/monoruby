//
// Just-in-time compiler module.
//

use super::*;

///
/// Unique identifier for a specialized method / block frame within a
/// single JIT compilation. Used by [`AsmInst::LoadDynVarSpecialized`] /
/// [`AsmInst::StoreDynVarSpecialized`] hints to defer concrete stack
/// offset computation until after every frame's size is finalized.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct SpecializedId(pub(super) usize);

///
/// Per-frame stack-size pair recorded in
/// [`JitContext::specialized_frame_sizes`]. `total` is the dynamic
/// `stack_offset` at pop time (`base` plus any JIT-managed spill);
/// `base` is the immutable value snapshotted at frame creation. The
/// difference is what the Loop-JIT-side rsp bump consumes.
///
///
/// D1/K1 forwarding-deferral record for a specialized pure trampoline
/// frame (`def f(...) = g(...)`): the caller's positional window that
/// backs `f`'s un-materialized `...` rest `Array`, plus — when the
/// caller passes literal keywords — the caller's kw window that backs
/// `f`'s un-materialized `**kwrest` Hash (K1).
///
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct DeferredForward {
    /// `f`'s synthetic rest local slot.
    pub rest_local: SlotId,
    /// Caller-frame base of the positional source window.
    pub src: SlotId,
    /// Positional source count.
    pub len: u16,
    /// K1: `(f's **kwrest local, caller kw base, names in slot order)` —
    /// `names[i]`'s value lives at caller slot `kw_pos + i`.
    pub kw: Option<(SlotId, SlotId, Box<[IdentId]>)>,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct FrameSizes {
    pub(super) total: usize,
    pub(super) base: usize,
}

///
/// Walk every `AsmIr` reachable from `asm_info` (main inst stream,
/// inline / outline bridges, and recursively the
/// `specialized_methods`) and return the maximum `VirtFPReg.0` seen
/// in any `fpr` operand. `None` if no `VirtFPReg` is referenced.
///
pub(super) fn max_virt_fpreg_id(asm_info: &AsmInfo) -> Option<usize> {
    let mut max: Option<usize> = None;
    let mut bump = |id: usize| {
        max = Some(match max {
            Some(m) if m >= id => m,
            _ => id,
        });
    };
    for (_, ir) in &asm_info.ir {
        for inst in ir.inst_iter() {
            for v in inst.fpr_operands() {
                bump(v.0);
            }
        }
    }
    for (ir, _, _) in &asm_info.outline_bridges {
        for inst in ir.inst_iter() {
            for v in inst.fpr_operands() {
                bump(v.0);
            }
        }
    }
    for (ir, _) in asm_info.inline_bridges.values() {
        for inst in ir.inst_iter() {
            for v in inst.fpr_operands() {
                bump(v.0);
            }
        }
    }
    for SpecializeInfo { info, .. } in &asm_info.specialized_methods {
        if let Some(id) = max_virt_fpreg_id(info) {
            bump(id);
        }
    }
    max
}

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
    /// The subtree was compiled while an enclosing frame's unboxed-Float
    /// speculation was armed, so its body addresses that frame's FP
    /// save/spill slots and must never be recompiled standalone (#1140).
    pub(super) speculated: bool,
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
    /// Jump-threading aliases: a `JitLabel` mapped here resolves to its
    /// target instead of itself. Populated for the entry labels of
    /// *empty* outline bridges (a `Side` edge whose write-back glue is a
    /// no-op): rather than emit a one-instruction `b dest_bb` forwarder,
    /// the branch that targeted the bridge resolves straight to the
    /// destination block. `resolve_label` chases the chain.
    ///
    label_alias: HashMap<JitLabel, JitLabel>,

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
    /// For `JitType::Loop`, the bytes the JIT itself adds to `rsp`
    /// at loop entry (matching the `addq rsp, _` emitted at every
    /// side-exit handler). Populated by the pre-codegen resolve
    /// pass; `0` for non-Loop frames or Loop frames with no
    /// JIT-managed spill space.
    ///
    pub(super) loop_jit_spill_bytes: usize,

    ///
    /// Identifier of the [`JitStackFrame`] that owns this `AsmInfo`.
    /// Set by [`JitContext::push_frame`]. Used by `expand_spills` to
    /// look up the frame's `base_stack_offset` from
    /// [`JitContext::specialized_frame_sizes`] when resolving spill
    /// slot offsets.
    ///
    pub(super) specialized_id: SpecializedId,

    ///
    /// Snapshot of the frame's immutable `base_stack_offset` taken
    /// at `pop_frame`. Lets codegen-side spill-aware lowerings (e.g.
    /// `FprBinOp`) compute the rbp-relative offset of a spilled
    /// `VirtFPReg` directly without re-querying
    /// [`JitContext::specialized_frame_sizes`].
    ///
    pub(super) base_stack_offset: usize,

    ///
    /// Source map for bytecode index and machine code position.
    ///
    pub(super) sourcemap: Vec<(BcIndex, usize)>,
    ///
    /// Start position of the machine code in `JitMemory`.
    ///
    pub(super) start_codepos: usize,
    /// aarch64: side-exit handler `LInst`s accumulated by `gen_asm` and
    /// emitted in one outlined island — at the frame's end, or mid-stream
    /// when the hot run since the last island approaches the `TBZ`/`TBNZ`
    /// imm14 range (see `a64_drain_side_exits`). Unused on x86-64, which
    /// outlines its handlers to the cold page instead.
    pub(in crate::codegen) pending_side_exits: Vec<crate::codegen::jitgen::lir::LInst>,
    /// aarch64: code position of the last side-exit island (or the frame
    /// start), the base the hot-run length is measured from.
    pub(in crate::codegen) side_exit_watermark: usize,
    /// aarch64: indices into `pending_side_exits` whose labels have been
    /// referenced since their last thunk — the only ones a thunk island has
    /// to bind (binding all of them made the island itself outgrow the
    /// `TBZ` reach on handler-heavy frames).
    pub(in crate::codegen) touched_side_exits: std::collections::HashSet<usize>,
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
            label_alias: HashMap::default(),
            ir: vec![],
            outline_bridges: vec![],
            inline_bridges: HashMap::default(),
            specialized_methods: vec![],
            loop_jit_spill_bytes: 0,
            specialized_id: SpecializedId(usize::MAX),
            base_stack_offset: 0,
            ivar_heap_accessed: false,
            sourcemap: vec![],
            start_codepos: 0,
            pending_side_exits: Vec::new(),
            side_exit_watermark: 0,
            touched_side_exits: std::collections::HashSet::default(),
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
        // Follow jump-threading aliases first (bounded: aliases only ever
        // point at a real BB label, which is never itself aliased, so the
        // chain is at most one hop — the loop is just defensive).
        let mut label = label;
        let mut guard = self.label_alias.len() + 1;
        while let Some(&next) = self.label_alias.get(&label) {
            label = next;
            guard -= 1;
            if guard == 0 {
                break;
            }
        }
        match &self.labels[label.0] {
            Some(l) => l.clone(),
            None => {
                let l = jit.label();
                self.labels[label.0] = Some(l.clone());
                l
            }
        }
    }

    ///
    /// Partition the outline bridges into *empty* (no write-back glue) and
    /// the rest. For each empty one, install a jump-threading alias from the
    /// bridge's entry label to its destination block, so the branch that
    /// targeted the bridge resolves straight through (the empty `b dest_bb`
    /// forwarder is never emitted). Returns the non-empty bridges, which the
    /// caller still emits.
    ///
    pub(super) fn thread_empty_outline_bridges(&mut self) -> Vec<(AsmIr, JitLabel, BasicBlockId)> {
        let bridges = std::mem::take(&mut self.outline_bridges);
        let mut keep = Vec::with_capacity(bridges.len());
        for (ir, dest, bbid) in bridges {
            if ir.is_empty() {
                let target = self.basic_block_labels.get(&bbid).copied().unwrap();
                self.label_alias.insert(dest, target);
            } else {
                keep.push((ir, dest, bbid));
            }
        }
        keep
    }

    pub(super) fn resolve_bb_label(&mut self, jit: &mut JitMemory, bb: BasicBlockId) -> DestLabel {
        let label = self.basic_block_labels.get(&bb).copied().unwrap();
        self.resolve_label(jit, label)
    }

    pub(super) fn detach_ir(&mut self) -> Vec<(Option<BasicBlockId>, AsmIr)> {
        std::mem::take(&mut self.ir)
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

    // pre-codegen rewrite helpers

    pub(super) fn iter_ir_mut(&mut self) -> std::slice::IterMut<'_, (Option<BasicBlockId>, AsmIr)> {
        self.ir.iter_mut()
    }

    pub(super) fn iter_outline_bridges_mut(
        &mut self,
    ) -> std::slice::IterMut<'_, (AsmIr, JitLabel, BasicBlockId)> {
        self.outline_bridges.iter_mut()
    }

    pub(super) fn iter_inline_bridges_mut(
        &mut self,
    ) -> std::collections::hash_map::ValuesMut<
        '_,
        Option<BasicBlockId>,
        (AsmIr, Option<BasicBlockId>),
    > {
        self.inline_bridges.values_mut()
    }

    pub(super) fn iter_specialized_methods_mut(
        &mut self,
    ) -> std::slice::IterMut<'_, SpecializeInfo> {
        self.specialized_methods.iter_mut()
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
    /// Stage-C loop adoption: per loop head, the outer-frame slots the
    /// loop's inlined subtree read as raw f64s — `(stack position,
    /// slot)`, exported by the back-edge fixpoint's analysis walk
    /// (whose own context clone would otherwise take the marks to its
    /// grave). Consumed once by the loop-entry merge.
    ///
    loop_outer_reads: HashMap<BasicBlockId, Vec<(usize, SlotId)>>,
    ///
    /// Stage-C loop adoption: the outer views adopted for a loop
    /// currently being compiled, keyed by the loop's *last* basic block
    /// — `(loop_end, stack position, slot)`. Reverted (re-widened on
    /// the parked frame) as soon as the compile leaves the loop: the
    /// entry bridges establish the home on every path through the loop
    /// head, which dominates the body but nothing after it.
    ///
    adopted_outer_views: Vec<(BasicBlockId, usize, SlotId)>,
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
    /// Loop-entry target state, one `SlotState` per frame of this
    /// compilation, innermost last — the shape
    /// [`AbstractState::gen_bridge_all`] bridges every back edge to.
    /// Outer frames are carried because a block handed out of the unit can
    /// write their locals, so their modes have to be merged (and written
    /// back) at the loop head like the innermost frame's.
    backedge_map: HashMap<BasicBlockId, Vec<SlotState>>,
    ///
    /// Contexts for returning from this frame.
    ///
    return_context: HashMap<usize, ReturnState>,

    ///
    /// Machine stack offset for this frame. May be temporarily bumped
    /// by [`JitContext::specialized_compile`] for the duration of a
    /// nested specialized call (`+= using_fpr.offset()` then `-=`),
    /// so this is the *dynamic* value at any moment. The base value
    /// is preserved in [`Self::base_stack_offset`].
    ///
    stack_offset: usize,

    ///
    /// Initial (immutable) value of [`Self::stack_offset`] at frame
    /// creation. The dynamic field gets `+=`/`-=` adjustments inside
    /// `specialized_compile`; this snapshot lets emit-time helpers
    /// recover the bump (`stack_offset - base_stack_offset`) that
    /// [`JitContext::specialized_frame_sizes`] alone does not capture.
    ///
    base_stack_offset: usize,

    ///
    /// Unique identifier assigned by [`JitContext::push_frame`]. After
    /// the frame is popped via [`JitContext::pop_frame`], the
    /// finalised `stack_offset` is recorded in
    /// [`JitContext::specialized_frame_sizes`] under this id and used
    /// by the pre-codegen pass to resolve `DynVarOffset::Hint`
    /// hints into concrete byte offsets.
    ///
    specialized_id: SpecializedId,

    ///
    /// True iff any deopt-able side exit was emitted during this
    /// frame's compilation, including from inlined sub-iseqs whose
    /// compile-time deopt fact has been propagated up. Used by
    /// `compile_specialized_func` to taint a `ReturnValue::Const`
    /// before it leaks to the caller — see
    /// `ReturnState::taint_for_unmodeled_rescue` for the dual case.
    ///
    /// Aggregated from `AsmIr::had_deopt()` at every
    /// `push_ir`/`add_inline_bridge`/`add_outline_bridge`, and
    /// propagated from inlined sub-iseqs in
    /// `compile_specialized_func`.
    ///
    pub(super) had_deopt: bool,

    ///
    /// Some `yield` in this frame was lowered as a generic call rather
    /// than inlined ([`JitContext::compile_yield_specialized`]), so the
    /// block it invokes runs as a compilation unit of its own and any
    /// store it makes into an outer frame is invisible here.
    ///
    pub(super) generic_yield: bool,
    /// D1: set when the trampoline forwarding consumer routed `g(...)`
    /// straight from the caller source (elided `f`'s rest Array).
    /// Aggregated from `AsmIr::deferred_rest()` like `had_deopt`,
    /// surfaced via `compile_specialized_func`.
    pub(super) deferred_rest: bool,
    /// D1 veto (see `AsmIr::needs_rest_array`): some forwarding consume
    /// of the deferred rest needs the real `Array`. Aggregated like
    /// `deferred_rest`; producer skips `create_array` only when
    /// `deferred_rest && !needs_rest_array`.
    pub(super) needs_rest_array: bool,

    ///
    /// Unboxed-locals speculation (`doc/chain_deopt.md` §5 steps 4–5):
    /// non-empty exactly while this frame's qualifying block-passing
    /// call site compiles its specialized subtree. Each entry is a
    /// pure-`F` local kept unboxed across the call; a specialized block
    /// in the subtree reads and writes it in this frame's FP save/spill
    /// area (`Load/StoreDynVarSpeculatedF`) instead of the LFP slot.
    ///
    pub(super) speculated_floats: Vec<(SlotId, crate::codegen::FPReg)>,
    ///
    /// The site's `UsingFpr` snapshot, taken when the speculation set was
    /// pinned. Pool-resident speculated floats live in the call's
    /// cont-mode save area; a pool register's save-slot index is its
    /// bit's rank in this snapshot (ascending bit order — the layout
    /// `emit_fpr_save` writes and `ChainReplay` reads).
    ///
    pub(super) speculated_using_fpr: UsingFpr,
    ///
    /// Set when the speculated subtree compiled something that could
    /// capture this frame or route a speculated slot onto the boxed
    /// path (generic block-passing call, generic yield, block-handler
    /// materialization, a no-capture invalidation reaching this frame).
    /// The site then discards the subtree and recompiles it without
    /// speculation.
    ///
    pub(super) speculation_poisoned: bool,
    ///
    /// `ensure` regions of this frame with JIT-spliced non-local exits
    /// (issue #1185), keyed by the region's `EnsureEnd` bc index. The
    /// value records which exit kinds spliced into it, so the `EnsureEnd`
    /// arm knows which specialized-teardown dispatch arms to emit.
    /// `(break, method_return)`.
    ///
    pub(super) spliced_ensures: HashMap<BcIndex, (bool, bool)>,
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
    ///
    /// Mark this frame as a define_method proc-method body: `return`
    /// targets the frame itself (lambda-style) and `current_method_frame`
    /// must stop here, exactly as for a real method frame.
    ///
    pub(super) fn set_bmethod_home(&mut self) {
        self.is_not_block = true;
    }

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
        let stack_offset = store[iseq_id].stack_offset() + CONTINUATION_FRAME_SIZE; // size of continuation frame
        Self {
            asm_info: AsmInfo {
                jit_type,
                specialize_level,
                iseq_id,
                self_class,
                self_ty,
                labels,
                basic_block_labels,
                label_alias: HashMap::default(),
                ir: vec![],
                outline_bridges: vec![],
                inline_bridges: HashMap::default(),
                ivar_heap_accessed: false,
                specialized_methods: vec![],
                loop_jit_spill_bytes: 0,
                specialized_id: SpecializedId(usize::MAX),
                base_stack_offset: 0,
                sourcemap: vec![],
                start_codepos: 0,
                pending_side_exits: Vec::new(),
                side_exit_watermark: 0,
                touched_side_exits: std::collections::HashSet::default(),
            },
            outer,
            callid: None,
            abstract_state,
            is_not_block,
            loop_info: indexmap::IndexMap::default(),
            loop_outer_reads: HashMap::default(),
            adopted_outer_views: vec![],
            loop_count: 0,
            branch_map: HashMap::default(),
            backedge_map: HashMap::default(),
            return_context: HashMap::default(),
            stack_offset,
            base_stack_offset: stack_offset,
            // Sentinel — overwritten by [`JitContext::push_frame`].
            specialized_id: SpecializedId(usize::MAX),
            had_deopt: false,
            generic_yield: false,
            deferred_rest: false,
            needs_rest_array: false,
            speculated_floats: vec![],
            speculated_using_fpr: UsingFpr::default(),
            speculation_poisoned: false,
            spliced_ensures: HashMap::default(),
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
            loop_outer_reads: HashMap::default(),
            adopted_outer_views: vec![],
            loop_count: 0,
            branch_map: HashMap::default(),
            backedge_map: HashMap::default(),
            return_context: HashMap::default(),
            stack_offset: self.stack_offset,
            base_stack_offset: self.base_stack_offset,
            // Preserve the source frame's id — the dup is used by
            // [`JitContext::loop_analysis`] which performs read-only
            // walks over a snapshot of the stack and never reaches
            // the codegen resolve pass, so id reuse is safe.
            specialized_id: self.specialized_id,
            had_deopt: self.had_deopt,
            generic_yield: self.generic_yield,
            deferred_rest: self.deferred_rest,
            needs_rest_array: self.needs_rest_array,
            speculated_floats: self.speculated_floats.clone(),
            speculated_using_fpr: self.speculated_using_fpr,
            speculation_poisoned: self.speculation_poisoned,
            spliced_ensures: self.spliced_ensures.clone(),
        }
    }

    // accessors

    pub(super) fn iseq_id(&self) -> ISeqId {
        self.iseq_id
    }

    // bridge operations

    pub(super) fn detach_return_context(&mut self) -> HashMap<usize, ReturnState> {
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
    /// Set by an instruction-fusing arm (`try_fuse_array_minmax`): the
    /// bytecode position whose work was already emitted by its
    /// predecessor, to be skipped by `compile_instruction`.
    pub(super) fused_skip: Option<BcIndex>,

    /// "the object in this slot is proven not frozen on the current path"
    /// (④-b): `SlotId::self_()` after a `StoreIvar` guard, an attr_writer
    /// receiver after its guard. Consumed by the next self-ivar store /
    /// attr_writer on the same object slot to skip the redundant re-check.
    /// Only sound while nothing in between can freeze the object or
    /// redefine the slot, so `compile_instruction` takes the set into
    /// [`Self::instr_unfrozen`] on every instruction and only arms that
    /// compiled a provably-transparent lowering — no Ruby code, no
    /// safepoint (a green-thread preemption could let another thread call
    /// `freeze`) — restore it (minus any slot they redefine).
    /// `compile_basic_block` clears it at BB entry (merges and loop heads
    /// make no path promise, and the loop-head safepoint is a preemption
    /// point), and `traceir_to_asmir` clears it when a (possibly nested,
    /// specialized) compile finishes, so callee-frame proofs never leak
    /// into the caller.
    pub(super) unfrozen_slots: Vec<SlotId>,
    /// The proof set taken at the head of the instruction currently being
    /// compiled (see [`Self::unfrozen_slots`]). Read via
    /// `instr_unfrozen_contains`, published back via `restore_unfrozen`.
    pub(super) instr_unfrozen: Vec<SlotId>,

    ///
    /// Monotone count of capture-relevant compile events — see
    /// [`Self::capture_events`].
    ///
    capture_events: usize,

    ///
    /// Set while an intra-block dispatch arm is being emitted
    /// (`compile/pic.rs`), which suppresses callee specialization.
    ///
    /// An arm is emitted after its predecessors are already in the
    /// instruction stream, so it has no way to back out: a
    /// `compile_specialized_func` that answers `CompileError` would abandon
    /// the whole compile, and one that answers `Cease` would leave the arm
    /// with no path to the merge. Specialization is also the wrong shape
    /// here — a dispatch arm exists to keep the chain short, and inlining a
    /// callee body into each of up to four arms is the opposite.
    ///
    in_dispatch_arm: bool,
    ///
    /// Inside a dispatch arm whose receiver class is *not* proven: the arm
    /// covers several classes, all resolving to the arm's target, and the
    /// membership test that admitted them has already been emitted.
    ///
    /// `compile_method_call` reads this to skip its own receiver guard (the
    /// arm's test is the guard) and to restrict itself to class-independent
    /// inline generators, which is the same treatment the class-set guard
    /// gets, for the same reason.
    ///
    in_set_guarded_arm: bool,

    ///
    /// Class version at compile time.
    ///
    class_version: u32,

    ///
    /// Const version at compile time.
    ///
    const_version: u64,

    ///
    /// The refinement set the body being compiled resolves under.
    ///
    /// `EMPTY` for every compilation in a program that never refines
    /// anything, which is what keeps the emitted code identical to what
    /// it was before refinements existed. Recorded alongside each
    /// compile-time resolution in [`Self::inline_method_cache`] so the
    /// class-version repair re-asks the same question
    /// (`doc/refinements.md` §6.6).
    ///
    refinements: RefinementSetId,

    ///
    /// Inline cache for method calls.
    ///
    pub(crate) inline_method_cache: Vec<InlineCacheEntry>,

    ///
    /// Every constant this compilation folded (root body and inlined
    /// specialized children alike), recorded for const-version salvage —
    /// see [`crate::globals::ConstSalvageMap`].
    ///
    pub(crate) const_fold_cache: Vec<ConstFoldSite>,

    ///
    /// The `(class, operator)` basic-op invariants this body inlined *without*
    /// a runtime guard — integer/float arithmetic, comparisons, and constant
    /// folds. The emitted code is correct only while each of them is still the
    /// builtin, so `set_bop_redefine` uses the recorded set to decide which
    /// compiled bodies a redefinition actually invalidates (rather than
    /// throwing away every compiled body in the process).
    ///
    pub(crate) bop_deps: Vec<(ClassId, IdentId)>,
    ///
    /// Stack frame for specialized compilation. (iseq, outer_scope, block_iseq)
    ///
    stack_frame: Vec<JitStackFrame>,

    ///
    /// Counter for handing out fresh [`SpecializedId`]s to frames as
    /// they are pushed. Monotonic across the whole compilation.
    ///
    next_specialized_id: usize,

    ///
    /// Map from each frame's [`SpecializedId`] to its finalised
    /// stack-size pair (`total` = base + spill, `base` = invariant
    /// from frame creation). Populated when [`Self::pop_frame`] hands
    /// the frame back to the caller. Read by
    /// [`Self::resolve_dyn_var_offsets`] before code generation:
    /// `total` resolves DynVar / chain hints and method-JIT
    /// prologues, `total - base` resolves Loop-JIT rsp bumps.
    ///
    specialized_frame_sizes: HashMap<SpecializedId, FrameSizes>,
    ///
    /// The final `UsingFpr` each specialized call site saved (keyed by the
    /// *callee instance*), recorded when the call is emitted. The resolve
    /// pass reads it to place a write-through refresh
    /// (`AsmInst::StoreOuterFprHomeF`) into the owner's save area.
    call_site_fpr_saves: HashMap<SpecializedId, UsingFpr>,
    ///
    /// Outer-frame `Sf` views kept alive by write-through stores
    /// (stage 1'), as `(stack position, slot)`. A specialized call whose
    /// subtree had a deopt-able exit or a generic yield drains its marks
    /// and re-widens them — the same bet-confirmation the kept constants
    /// go through (#1140), for the same reason: a salvage re-entry runs
    /// compiled continuations without a chain conversion.
    kept_outer_views: Vec<(usize, SlotId)>,
    ///
    /// Stage-C loop adoption: set when this context compiled anything
    /// that can rewrite an outer frame's slot *invisibly* — a call that
    /// hands a block out of the unit (`all_frames_unbox_to_S`), a call
    /// site forwarding an explicit `&blk`, or a capture event. A loop
    /// analysed while this fires must not adopt an outer view: the
    /// compile-time widen hooks do not cover such stores, so the adopted
    /// home could go stale between iterations.
    ///
    outer_claim_barrier: bool,
    ///
    /// Stage-C loop adoption: every `(stack position, slot)` an outer
    /// widen reached during this context's walk
    /// ([`Self::widen_outer_slot`]). A loop analysis excludes these pairs
    /// from adoption — the body contains a store the write-through keep
    /// could not hold onto, so an adopted home would be stale on the next
    /// iteration.
    ///
    widened_outer_log: Vec<(usize, SlotId)>,
}

impl<'a> JitContext<'a> {
    pub(super) fn new(
        store: &'a Store,
        codegen_mode: bool,
        class_version: u32,
        const_version: u64,
        refinements: RefinementSetId,
        stack_frame: Vec<JitStackFrame>,
    ) -> Self {
        Self {
            store,
            codegen_mode,
            fused_skip: None,
            unfrozen_slots: Vec::new(),
            instr_unfrozen: Vec::new(),
            capture_events: 0,
            in_dispatch_arm: false,
            in_set_guarded_arm: false,
            class_version,
            const_version,
            refinements,
            inline_method_cache: vec![],
            const_fold_cache: vec![],
            bop_deps: vec![],
            stack_frame,
            next_specialized_id: 0,
            specialized_frame_sizes: HashMap::default(),
            call_site_fpr_saves: HashMap::default(),
            kept_outer_views: vec![],
            outer_claim_barrier: false,
            widened_outer_log: vec![],
        }
    }

    pub(super) fn loop_analysis(&self, pc: BytecodePtr) -> Self {
        let mut ctx = self.analysis_clone();
        ctx.stack_frame.last_mut().unwrap().jit_type = JitType::Loop(pc);
        ctx
    }

    ///
    /// A throwaway copy of this context for an analysis pass: same frames,
    /// `codegen_mode` off, so the `AsmIr` it produces is never emitted.
    ///
    pub(super) fn analysis_clone(&self) -> Self {
        let stack_frame = self.stack_frame.clone();
        Self {
            store: self.store,
            codegen_mode: false,
            fused_skip: None,
            call_site_fpr_saves: HashMap::default(),
            kept_outer_views: vec![],
            outer_claim_barrier: false,
            widened_outer_log: vec![],
            unfrozen_slots: Vec::new(),
            instr_unfrozen: Vec::new(),
            capture_events: 0,
            in_dispatch_arm: false,
            in_set_guarded_arm: false,
            class_version: self.class_version,
            const_version: self.const_version,
            refinements: self.refinements,
            inline_method_cache: vec![],
            const_fold_cache: vec![],
            bop_deps: vec![],
            stack_frame,
            // The cloned context emits AsmIr only for analysis (it is
            // never codegen'd), so the id counter / size map can stay
            // empty — the resolve pass never runs against this ir.
            next_specialized_id: 0,
            specialized_frame_sizes: HashMap::default(),
        }
    }

    pub(super) fn codegen_mode(&self) -> bool {
        self.codegen_mode
    }

    ///
    /// Whether every interpreter-resuming side exit emitted for the frame
    /// currently being compiled must **escalate to chain deopt** — i.e. run
    /// the chain-deopt walk (converting every suspended JIT frame in the
    /// caller chain into an interpreter frame) before falling back to the
    /// interpreter (`doc/chain_deopt.md` §5 step 4 / §6).
    ///
    /// This is the "mechanical guarantee" §6 asks for: the flag is consulted
    /// in exactly one place — [`AsmIr::new`], which stamps it onto the IR so
    /// every side-exit constructor (`new_deopt` / `new_recompile_deopt` /
    /// `new_error` / `deopt_from_point`) picks it up — rather than relying on
    /// each emitter to remember.
    ///
    /// Escalation used to be unconditional. `doc/chain_deopt.md` §6 warned
    /// against exactly that ("blanket escalation would make today's cheap
    /// per-frame deopts pay a chain walk ... so gate it per site"), and the
    /// activerecord measurement put the cost at 1.2M conversions per three
    /// iterations, 94.7% of them with nothing at all to replay.
    ///
    /// Both things escalation buys are confined to a single compilation
    /// unit, so a side exit in the unit's root frame needs none of it:
    ///
    /// * §6's return-state narrowing. A narrowed `ReturnState` is applied
    ///   only by `def_rax2acc_return`, which only the specialized-compile
    ///   path reaches; an ordinary send types its result `Guarded::Value`.
    ///   Type information never crosses a unit boundary, so no caller
    ///   outside this unit can be holding a tag this frame's deopt would
    ///   invalidate.
    /// * §5 step 5's unboxed-locals speculation. §7 scopes it to
    ///   specialized `iseq_block` frames, so an unboxed local is only ever
    ///   read across a frame boundary *inside* the unit.
    ///
    /// A frame at depth 0 is the unit's root: everything above it was
    /// entered through a call this compiler did not compile, holding no
    /// narrowed tag and no unboxed local of ours. Frames deeper than that
    /// still escalate — the callers they have to convert are the
    /// specialized frames this same compilation built above them.
    ///
    /// Basic-op redefinition is unaffected: it evicts through
    /// `Codegen::check_bop_redefine`, a separate entry point, and it does
    /// have to convert frames across unit boundaries.
    ///
    pub(super) fn escalate_side_exits(&self) -> bool {
        self.current_frame_pos() > 0
    }

    pub(super) fn in_dispatch_arm(&self) -> bool {
        self.in_dispatch_arm
    }

    /// ④-b: is *slot* in the unfrozen-proof set taken at the head of the
    /// current instruction?
    pub(super) fn instr_unfrozen_contains(&self, slot: SlotId) -> bool {
        self.instr_unfrozen.contains(&slot)
    }

    /// ④-b: the current instruction compiled to a provably-transparent
    /// lowering (no Ruby code, no safepoint) — publish the instruction-head
    /// proof set back, minus the slot the instruction redefined (its value
    /// changed, so any proof about the old value is void).
    ///
    /// Inside a dispatch / class-set-guarded arm the set stays empty: the
    /// arms merge afterwards, and a proof holding on one arm's path says
    /// nothing about its siblings.
    pub(super) fn restore_unfrozen(&mut self, redefined: Option<SlotId>) {
        if self.in_dispatch_arm || self.in_set_guarded_arm {
            self.instr_unfrozen.clear();
            return;
        }
        let mut set = std::mem::take(&mut self.instr_unfrozen);
        if let Some(d) = redefined {
            set.retain(|&s| s != d);
        }
        self.unfrozen_slots = set;
    }

    /// ④-b: record that the current path just guarded (or re-proved) the
    /// frozen bit of the object in *slot*. No-op inside dispatch /
    /// set-guarded arms (see `restore_unfrozen`).
    pub(super) fn prove_unfrozen(&mut self, slot: SlotId) {
        if self.in_dispatch_arm || self.in_set_guarded_arm {
            return;
        }
        if !self.unfrozen_slots.contains(&slot) {
            self.unfrozen_slots.push(slot);
        }
    }

    pub(super) fn in_set_guarded_arm(&self) -> bool {
        self.in_set_guarded_arm
    }

    ///
    /// Run *f* with the dispatch-arm flag set, restoring it afterwards.
    ///
    ///
    /// Run *f* inside a dispatch arm, recording whether the arm's receiver
    /// class is proven (`set_guarded == false`) or merely known to be one of
    /// the arm's set.
    ///
    pub(super) fn with_arm<R>(&mut self, set_guarded: bool, f: impl FnOnce(&mut Self) -> R) -> R {
        let saved = std::mem::replace(&mut self.in_dispatch_arm, true);
        let saved_set = std::mem::replace(&mut self.in_set_guarded_arm, set_guarded);
        let r = f(self);
        self.in_dispatch_arm = saved;
        self.in_set_guarded_arm = saved_set;
        r
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

    //pub(super) fn is_block(&self) -> bool {
    //    !self.store[self.func_id()].is_not_block()
    //}

    pub(super) fn specialized_methods_len(&self) -> usize {
        self.current_frame().specialized_methods.len()
    }

    pub(super) fn specialized_methods_push(&mut self, info: SpecializeInfo) {
        self.current_frame_mut().specialized_methods.push(info);
    }

    pub(super) fn push_return_context(&mut self, pos: usize, ret: ReturnState) {
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
                .insert(pos, ReturnState::may_side_effect());
        }
    }

    pub(super) fn merge_return_context(&mut self, context: HashMap<usize, ReturnState>) {
        for (pos, res) in context {
            if let Some(frame) = self.current_frame_mut().return_context.get_mut(&pos) {
                frame.join(&res);
            } else {
                self.current_frame_mut().return_context.insert(pos, res);
            }
        }
    }

    // handling frame
    pub(super) fn current_frame(&self) -> &JitStackFrame {
        self.stack_frame.last().unwrap()
    }

    pub(super) fn current_frame_mut(&mut self) -> &mut JitStackFrame {
        self.stack_frame.last_mut().unwrap()
    }

    pub(super) fn current_frame_pos(&self) -> usize {
        self.stack_frame.len() - 1
    }

    pub(super) fn push_frame(&mut self, mut frame: JitStackFrame) {
        // Hand out a fresh id; codegen-mode contexts use this to wire
        // every frame into [`Self::specialized_frame_sizes`] when the
        // frame is popped. Loop-analysis contexts never codegen, so
        // they reuse the cloned id from `JitStackFrame::dup`.
        if frame.specialized_id.0 == usize::MAX {
            frame.specialized_id = SpecializedId(self.next_specialized_id);
            self.next_specialized_id += 1;
        }
        // Mirror onto AsmInfo so `expand_spills` can look up the
        // frame's base_stack_offset from `specialized_frame_sizes`
        // by reading the AsmInfo alone.
        frame.asm_info.specialized_id = frame.specialized_id;
        self.stack_frame.push(frame);
    }

    pub(super) fn pop_frame(&mut self) -> JitStackFrame {
        let mut frame = self.stack_frame.pop().unwrap();
        // Grow `stack_offset` by the JIT-owned spill region — every
        // `VirtFPReg(N)` with `N >= PHYS_FPR_POOL` claims 8 bytes
        // at the top of the frame's local area. Walk the
        // freshly-finalised AsmIr to find the max id used; this
        // works regardless of which `AbstractFrame` branch produced
        // the alloc (the per-state allocator counter is local to
        // each branch, but the resulting AsmInst is committed once
        // and we can find the same value by scanning).
        let spill_count = max_virt_fpreg_id(&frame.asm_info)
            .map(|m| (m + 1).saturating_sub(PHYS_FPR_POOL))
            .unwrap_or(0);
        // Each spill slot is 8 bytes; round the spill region up to a
        // 16-byte multiple so that any external `call` (e.g. into
        // Rust runtime helpers that emit movapd) keeps a 16-byte
        // aligned rsp under the SysV x86-64 ABI.
        let spill_bytes = (spill_count * 8 + 15) & !15;
        frame.stack_offset += spill_bytes;
        // Record the finalised frame sizes for the resolve pass.
        // After this point, `frame.stack_offset` will not be modified
        // (the `+=`/`-=` adjustments inside `specialized_compile`
        // are always paired around the recursive `traceir_to_asmir`
        // call), so this snapshot is the canonical pair for the id.
        self.specialized_frame_sizes.insert(
            frame.specialized_id,
            FrameSizes {
                total: frame.stack_offset,
                base: frame.base_stack_offset,
            },
        );
        // Stamp the JIT-owned spill bytes onto the AsmInfo so that
        // `side_exit_with_label` can emit the matching `addq rsp, _`
        // without re-walking the AsmIr. Method / specialized frames
        // restore rsp implicitly via `leave; ret`, so we leave it
        // at `0` for them.
        if matches!(frame.asm_info.jit_type, JitType::Loop(_)) {
            frame.asm_info.loop_jit_spill_bytes = frame.stack_offset - frame.base_stack_offset;
        }
        // Mirror `base_stack_offset` onto the AsmInfo so that
        // codegen-side spill-aware lowerings can compute spill slot
        // offsets directly.
        frame.asm_info.base_stack_offset = frame.base_stack_offset;
        frame
    }

    pub(super) fn current_frame_id(&self) -> SpecializedId {
        self.current_frame().specialized_id
    }

    ///
    /// Compile specialized method / block.
    ///
    pub(super) fn specialized_compile(
        &mut self,
        state: &mut AbstractState,
        callid: CallSiteId,
        frame: JitStackFrame,
    ) -> JitResult<JitStackFrame> {
        // Stage-B home-aliased reads: the callee can store through the
        // frame chain, so no alias survives a specialized call either.
        state.clear_dynvar_aliases();
        let stack_offset = state.using_fpr_offset().offset();
        let caller = self.current_frame_mut();
        caller.stack_offset += stack_offset;
        caller.callid = Some(callid);
        let scope = std::mem::take(&mut **state);
        assert!(std::mem::replace(&mut caller.abstract_state, Some(scope)).is_none());

        let frame = self.traceir_to_asmir(frame)?;

        let current = self.current_frame_mut();
        let innermost = current.abstract_state.take().unwrap();
        current.callid = None;
        current.stack_offset -= stack_offset;
        // Take the chain back. The nested compile could only record what
        // it did to our outer frames on the context's copies, so re-read
        // those rather than keep the clones we handed over.
        //
        // Resuming from the *join of the callee's return-path chains*
        // instead (the natural next unification step) is measurably
        // unsound today: a return snapshot preserves the caller's kept-`C`
        // claims path-sensitively, while the kept-constant bet machinery
        // (`forget_constants` emitting the deferred literal writes, the
        // `specialized_iseq` confirmation) is built around the parked
        // copy's path-insensitive widens — a resumed `C` whose slot only
        // some paths wrote reads stale (Integer#downto resumed `C(0)`
        // where the parked copy held `S(Value)`; Array#permutation then
        // read nil). Making that resume sound means making the kept-`C`
        // discipline per return path first.
        self.adopt_outer(state, innermost);
        Ok(frame)
    }

    ///
    /// Rebuild *state*'s frame chain from the context's copies, keeping
    /// *innermost* (this compile's own frame, which the context does not
    /// track while the compile is running).
    ///
    /// The reverse — publishing this compile's view of the outer frames
    /// *into* the context on the way down — is not available and must not
    /// be added back. `abstract_state` is not only what nested compiles
    /// clone; it is also where a frame's own compile parks its state while
    /// it waits for one. Writing a nested compile's view of a frame there
    /// replaces the suspended state that frame will resume from with a
    /// view taken at a different program point, in a different frame's
    /// terms — three levels of nested blocks segfaulted in generated code.
    ///
    /// Nothing needs it today: what a nested compile has to learn about an
    /// outer frame is only that a slot was widened, and that arrives
    /// through `widen_outer_slot`.
    ///
    fn adopt_outer(&self, state: &mut AbstractState, innermost: AbstractFrame) {
        let mut frames = self.trace_contexts();
        frames.push(innermost);
        state.set_frames(frames);
    }

    pub(crate) fn current_method_given_block(&self) -> Option<JitBlockInfo> {
        self.resolve_given_block().flatten()
    }

    ///
    /// Resolve the block effectively given to the current method,
    /// following block-forwarding caller call sites.
    ///
    /// The immediate method caller's call site may not pass a literal
    /// block yet still provably determine one: a `(...)` forwarding call
    /// (`def f(...) = g(...)` — e.g. `Class#new` driving
    /// `__builtin_initialize__`) passes its own method's incoming block
    /// onward, so the effective block is whatever *that* method was
    /// given, one specialization level up. Walking the chain lets a
    /// `yield` inside e.g. `Array#initialize` specialize against the
    /// literal block written at the user's `Array.new { .. }` site, and
    /// lets `block_given?` / the block-param proxy constant-fold when no
    /// block exists anywhere up the chain.
    ///
    /// The returned `JitBlockInfo::outer` is the specialization-stack
    /// distance from the current frame to the frame that owns the block
    /// literal — the same distance `setup_yield_frame` hops on the
    /// runtime CFP chain (specialized calls push real frames, so the two
    /// stacks coincide), and the lexical-home distance for `break` /
    /// non-local `return` bookkeeping.
    ///
    /// - `Some(Some(info))`: a literal block, `info.outer` frames up.
    /// - `Some(None)`: provably no block.
    /// - `None`: statically unknown (unspecialized root frame, or an
    ///   explicit `&blk` argument somewhere in the chain).
    ///
    pub(crate) fn resolve_given_block(&self) -> Option<Option<JitBlockInfo>> {
        // The frame whose incoming block `yield` / `block_given?` / the
        // block-param proxy refer to: the current *method* frame.
        let offset = self.current_method_frame()?.1;
        let mut method_pos = self.stack_frame.len().checked_sub(1 + offset)?;
        loop {
            let caller = method_pos.checked_sub(1)?;
            let callid = self.stack_frame[caller].callid?;
            let callsite = &self.store[callid];
            if let Some(block_fid) = callsite.block_fid {
                let self_class = self.stack_frame[caller].self_class;
                let outer = self.stack_frame.len() - 1 - caller;
                return Some(Some(JitBlockInfo::new(block_fid, self_class, outer)));
            }
            if callsite.block_arg.is_none() {
                return Some(None);
            }
            // A dynamic block argument. Only the `(...)` forwarding
            // shape provably passes the enclosing method's own block
            // onward; an explicit `&blk` stays unknown.
            if !callsite.forwarding {
                return None;
            }
            // The forwarded block belongs to the method lexically
            // enclosing the forwarding call site: hop the caller
            // frame's outer links up to its method frame and resolve
            // that method's own block.
            let mut pos = caller;
            while let Some(o) = self.stack_frame[pos].outer {
                pos = pos.checked_sub(o)?;
            }
            method_pos = pos;
        }
    }

    ///
    /// `Some(given?)` when [`Self::resolve_given_block`] statically
    /// determines whether a block was given; `None` when unknown.
    ///
    pub(crate) fn resolve_block_given(&self) -> Option<bool> {
        self.resolve_given_block().map(|b| b.is_some())
    }

    pub(crate) fn method_caller_callsite(&self) -> Option<CallSiteId> {
        let caller = self.method_caller_pos()?;
        self.stack_frame[caller].callid
    }

    pub(super) fn set_ivar_heap_accessed(&mut self) {
        self.current_frame_mut().ivar_heap_accessed = true;
    }

    fn caller_pos(&self) -> Option<usize> {
        self.stack_frame.len().checked_sub(2)
    }

    fn method_caller_pos(&self) -> Option<usize> {
        let offset = self.current_method_frame()?.1 + 1;
        let len = self.stack_frame.len();
        len.checked_sub(1)?.checked_sub(offset)
    }

    fn iter_caller_pos(&self) -> Option<usize> {
        // `break` exits the method the current block was *passed to* (the
        // "iter method"); the iter caller is that method's caller, which is
        // exactly the block's immediate lexical-outer frame. Derive it from
        // the frame's `outer` link (the stack-frame distance to the lexical
        // parent) instead of assuming a single intervening frame.
        //
        // For a block yielded directly by the method it was passed to
        // (e.g. `Array#each { break }`) the parent sits at `len - 3`, which
        // the old hard-coded `len - 3` happened to match. But blocks reached
        // through nested iterator frames (e.g. `Enumerable#find` ->
        // `__gather_each` -> `Array#each` -> block) have their lexical parent
        // much deeper, so `len - 3` pointed at an unrelated intermediate
        // frame — corrupting both the specialized break rbp offset and the
        // break return context, which dropped the break value (returned
        // `nil`).
        let len = self.stack_frame.len();
        let outer = self.current_frame().outer?;
        len.checked_sub(1)?.checked_sub(outer)
    }

    fn outer_pos(&self, outer: usize) -> Option<usize> {
        let mut i = self.stack_frame.len() - 1;
        for _ in 0..outer {
            i -= self.stack_frame[i].outer?;
        }
        Some(i)
    }

    ///
    /// Record that a `StoreDynVar` has written an unknown value into
    /// *slot* of the frame *outer* levels out, so nothing downstream keeps
    /// believing a mode this store just invalidated.
    ///
    /// A frame's locals cross a call boundary with their `Guarded` intact
    /// (`all_frames_unbox_to_S`), which is only sound if a callee that
    /// writes one of them says so: an `S(Guarded::Float)` a callee stores
    /// a String into would otherwise still be read as a Float once the
    /// call returns.
    ///
    /// Conservative on purpose: the slot drops to `S(Guarded::Value)`
    /// rather than taking the stored value's own mode.
    ///
    /// This is the context half. The live chain in `AbstractState` is the
    /// one this compile reasons about and has its own
    /// [`AbstractState::widen_outer_slot`]; both are written, because the
    /// frame that owns the slot reads the context copy back when the
    /// nested compile it is waiting on returns.
    ///
    /// Record that a `yield` in the current frame was not inlined.
    pub(super) fn set_generic_yield(&mut self) {
        self.current_frame_mut().generic_yield = true;
    }

    ///
    /// The stack-frame indices of the lexical chain a frame *outer* levels
    /// out from the next one to be pushed, immediate outer first.
    ///
    /// Takes the distance rather than reading it off the top frame,
    /// because the caller is about to push a block whose chain is not the
    /// current top's: a specialized method has `outer: None`, so walking
    /// from `Integer#times` finds nothing even though the block it is
    /// about to yield to is lexically inside `kill_int`.
    ///
    fn outer_chain_from(&self, outer: usize) -> Vec<usize> {
        let mut v = vec![];
        let Some(mut i) = self.stack_frame.len().checked_sub(outer) else {
            return v;
        };
        v.push(i);
        while let Some(o) = self.stack_frame[i].outer {
            let Some(next) = i.checked_sub(o) else { return v };
            i = next;
            v.push(i);
        }
        v
    }

    ///
    /// How many constants that chain still claims.
    ///
    pub(super) fn outer_const_count(&self, outer: usize) -> usize {
        self.outer_chain_from(outer)
            .into_iter()
            .map(|i| {
                self.stack_frame[i]
                    .abstract_state
                    .as_ref()
                    .map_or(0, |f| f.held_constants().len())
            })
            .sum()
    }

    ///
    /// Give up, on that chain, every constant *probe* gave up on its own
    /// copy of it. Returns whether anything changed.
    ///
    pub(super) fn adopt_outer_widenings(&mut self, probe: &Self, outer: usize) -> bool {
        let mut changed = false;
        for i in self.outer_chain_from(outer) {
            let Some(probed) = probe.stack_frame[i].abstract_state.as_ref() else {
                continue;
            };
            let lost: Vec<_> = probed.lost_constants_of(
                self.stack_frame[i].abstract_state.as_ref().unwrap(),
            );
            if !lost.is_empty() {
                changed = true;
                let mine = self.stack_frame[i].abstract_state.as_mut().unwrap();
                for slot in lost {
                    mine.invalidate_slot(slot);
                }
            }
        }
        changed
    }

    ///
    /// Stage-A use propagation: an inlined callee consumed the value it
    /// read from the slot *outer* levels out as a raw f64. Land the mark
    /// on the owner's parked frame — the same channel `widen_outer_slot`
    /// uses — so it survives `adopt_outer` and reaches the owner's
    /// back-edge `Liveness` harvest, where the loop-entry `Sf` adoption
    /// (the `adopt_sf` arm's subtree-read disjunct) can act on it.
    ///
    pub(super) fn mark_outer_float_read(&mut self, outer: usize, slot: SlotId) {
        let Some(pos) = self.outer_pos(outer) else {
            return;
        };
        if let Some(frame) = self.stack_frame[pos].abstract_state.as_mut() {
            frame.mark_subtree_float_read(slot);
        }
    }

    ///
    /// Stage-C loop adoption: everything an adoption decision needs to
    /// know about the loop body it just analysed — see the field docs.
    ///
    pub(super) fn set_outer_claim_barrier(&mut self) {
        self.outer_claim_barrier = true;
    }

    pub(super) fn outer_claim_barrier(&self) -> bool {
        self.outer_claim_barrier
    }

    pub(super) fn widened_outer_log(&self) -> &[(usize, SlotId)] {
        &self.widened_outer_log
    }

    ///
    /// Stage-C loop adoption: the id chain + live bump for addressing the
    /// frame at stack position *pos* from the current frame — the
    /// pos-addressed sibling of [`Self::outer_specialized_ids`], for the
    /// loop-entry init whose owner is reachable through a *block's*
    /// lexical chain but not the (method) frame the loop head sits in.
    ///
    fn specialized_ids_at_pos(&self, pos: usize) -> (Vec<SpecializedId>, usize) {
        let end = self.stack_frame.len() - 1;
        let chain = &self.stack_frame[pos..end];
        let ids = chain.iter().map(|f| f.specialized_id).collect();
        let extra = chain
            .iter()
            .map(|f| f.stack_offset - f.base_stack_offset)
            .sum();
        (ids, extra)
    }

    ///
    /// Stage-C loop adoption: the [`OuterFprHome`] hint for the frame at
    /// stack position *pos* — the pos-addressed sibling of
    /// [`Self::outer_fpr_home_hint`].
    ///
    fn outer_fpr_home_hint_at_pos(
        &self,
        pos: usize,
        fpr: crate::codegen::FPReg,
    ) -> Option<OuterFprHome> {
        let owner = self.stack_frame[pos].specialized_id;
        let callee = self.stack_frame.get(pos + 1)?.specialized_id;
        let (ids, extra) = self.specialized_ids_at_pos(pos);
        Some(OuterFprHome::Hint {
            ids,
            extra,
            owner,
            callee,
            fpr,
        })
    }

    ///
    /// Stage-C loop adoption, the analysis-side export: *self* is the
    /// throwaway analysis clone after one `analyse_loop` walk, *base* the
    /// context it was cloned from. Returns the `(stack position, slot)`
    /// pairs the walk newly marked as subtree float reads on the parked
    /// outer frames — the marks would otherwise die with the clone —
    /// minus everything the walk disqualified: nothing at all if the
    /// body raised the claim barrier or lowered a generic yield, and no
    /// pair an outer widen reached.
    ///
    pub(super) fn export_subtree_outer_reads(&self, base: &JitContext) -> Vec<(usize, SlotId)> {
        if self.outer_claim_barrier {
            return vec![];
        }
        let gy = |c: &JitContext| c.stack_frame.last().is_some_and(|f| f.generic_yield);
        if gy(self) && !gy(base) {
            return vec![];
        }
        let mut v = vec![];
        for pos in 0..self.stack_frame.len().saturating_sub(1) {
            let Some(now) = self.stack_frame[pos].abstract_state.as_ref() else {
                continue;
            };
            let before = base
                .stack_frame
                .get(pos)
                .and_then(|f| f.abstract_state.as_ref());
            for slot in now.subtree_float_read_slots() {
                if before.is_some_and(|b| b.subtree_float_read(slot)) {
                    continue;
                }
                if self.widened_outer_log.contains(&(pos, slot)) {
                    continue;
                }
                v.push((pos, slot));
            }
        }
        v
    }

    pub(super) fn record_loop_outer_reads(
        &mut self,
        entry_bb: BasicBlockId,
        reads: Vec<(usize, SlotId)>,
    ) {
        self.current_frame_mut()
            .loop_outer_reads
            .insert(entry_bb, reads);
    }

    ///
    /// Stage-C loop adoption, the decision itself (called from the
    /// loop-entry merge, codegen pass only): for every outer-frame slot
    /// the loop's subtree read as a raw f64 and that is still a plain
    /// `S` on its (parked) owner, allocate a spill-resident raw-f64 home
    /// in the owner's file and bind the slot `Sf(Float)` there — the
    /// same promotion a stage-2 dominating store performs, driven by the
    /// loop-entry bridges instead: the caller emits, on **every** entry
    /// edge of the loop head, a chain load of the boxed slot, a Float
    /// guard, and an unbox into the home, so the claim holds on every
    /// path through the head — which dominates the whole loop body,
    /// where all its consumers (the blocks' home reads, stage 3a/B)
    /// live. The adoption is *scoped to the loop*: the claim is reverted
    /// when the compile leaves the loop ([`Self::revert_adopted_outer_views`]),
    /// so no path that bypassed the head can observe it.
    ///
    /// Returns, per adopted slot, what the entry-edge init needs:
    /// the chain offset of the boxed slot and the home.
    ///
    pub(super) fn adopt_outer_loop_views(
        &mut self,
        entry_bb: BasicBlockId,
        loop_end: BasicBlockId,
    ) -> Vec<(Vec<SpecializedId>, usize, SlotId, OuterFprHome)> {
        let mut inits = vec![];
        if !self.codegen_mode() {
            return inits;
        }
        let Some(reads) = self
            .current_frame_mut()
            .loop_outer_reads
            .remove(&entry_bb)
        else {
            return inits;
        };
        for (pos, slot) in reads {
            if pos + 1 >= self.stack_frame.len() {
                continue;
            }
            let Some(parked) = self.stack_frame[pos].abstract_state.as_ref() else {
                continue;
            };
            // Only a plain boxed slot adopts, and only while the static
            // chain addressing is valid for the owner.
            if !matches!(parked.mode(slot), LinkMode::S(_)) || !parked.no_capture_guard() {
                continue;
            }
            let fpr = self.stack_frame[pos]
                .abstract_state
                .as_mut()
                .unwrap()
                .alloc_spill_home(slot);
            let Some(home) = self.outer_fpr_home_hint_at_pos(pos, fpr) else {
                // No hint — undo the claim (the spill id stays allocated,
                // which only costs the owner 8 bytes of frame).
                self.stack_frame[pos]
                    .abstract_state
                    .as_mut()
                    .unwrap()
                    .invalidate_slot(slot);
                continue;
            };
            let (ids, extra) = self.specialized_ids_at_pos(pos);
            self.current_frame_mut()
                .adopted_outer_views
                .push((loop_end, pos, slot));
            inits.push((ids, extra, slot, home));
        }
        inits
    }

    ///
    /// Stage-C loop adoption: the compile has finished the loop whose
    /// last basic block is *bbid* — re-widen every view it adopted, so
    /// nothing after the loop (where the entry bridges no longer
    /// dominate) consumes the claim.
    ///
    pub(super) fn revert_adopted_outer_views(&mut self, bbid: BasicBlockId) {
        let views = &mut self.current_frame_mut().adopted_outer_views;
        if views.is_empty() {
            return;
        }
        let (done, rest): (Vec<_>, Vec<_>) = std::mem::take(views)
            .into_iter()
            .partition(|(end, _, _)| *end == bbid);
        self.current_frame_mut().adopted_outer_views = rest;
        for (_, pos, slot) in done {
            if let Some(frame) = self
                .stack_frame
                .get_mut(pos)
                .and_then(|f| f.abstract_state.as_mut())
            {
                frame.invalidate_slot(slot);
            }
        }
    }

    ///
    /// Position-addressed twin of [`Self::widen_outer_slot`], for callers
    /// that walk the trace chain (whose positions align with
    /// `stack_frame`) rather than a lexical distance.
    ///
    pub(super) fn widen_outer_at_pos(&mut self, pos: usize, slot: SlotId) {
        if pos + 1 >= self.stack_frame.len() {
            return;
        }
        if let Some(frame) = self
            .stack_frame
            .get_mut(pos)
            .and_then(|f| f.abstract_state.as_mut())
        {
            frame.invalidate_slot(slot);
        }
        self.widened_outer_log.push((pos, slot));
    }

    pub(super) fn widen_outer_slot(&mut self, outer: usize, slot: SlotId) {
        let Some(pos) = self.outer_pos(outer) else {
            // The chain leaves this compilation: the frame is not one of
            // ours, so there is no abstract state of ours to invalidate.
            return;
        };
        if let Some(frame) = self.stack_frame[pos].abstract_state.as_mut() {
            frame.invalidate_slot(slot);
        }
        // Stage-C loop adoption: a loop analysis excludes pairs a widen
        // reached — see `widened_outer_log`.
        self.widened_outer_log.push((pos, slot));
    }

    ///
    /// The abstract frames of this compilation's lexical chain, **outermost
    /// first** — the order [`AbstractState`] keeps them in, so that
    /// `frames[len - 1 - outer]` is the frame `outer` levels out, agreeing
    /// with [`Self::outer_pos`].
    ///
    /// The walk itself goes inward-out, so it is reversed before returning.
    /// Without that, `AbstractState::outer_no_capture_guard(1)` answered for
    /// the *outermost* frame rather than the immediate one as soon as the
    /// chain was three deep — and that answer gates whether a dynvar access
    /// may use the static frame-chain offset.
    ///
    ///
    /// The abstract frames of the **whole trace** (every suspended frame of
    /// this compilation, outermost first, 1:1 with `stack_frame`), each
    /// annotated with its lexical link — join unification, step B2: the
    /// state chain mirrors the specialization stack, so suspended method
    /// callers and lexical outer scopes are the same kind of entry and the
    /// merge machinery (join / equiv / bridge, which already walk every
    /// frame) covers both. Lexical (dynvar) addressing walks the per-frame
    /// links, exactly as [`Self::outer_pos`] walks `stack_frame`.
    ///
    pub(super) fn trace_contexts(&self) -> Vec<AbstractFrame> {
        let end = self.stack_frame.len() - 1;
        (0..end)
            .map(|pos| {
                let mut f = self.stack_frame[pos].abstract_state.clone().unwrap();
                f.set_lexical_outer(self.stack_frame[pos].outer);
                f
            })
            .collect()
    }

    pub(super) fn current_frame_lexical_outer(&self) -> Option<usize> {
        self.stack_frame.last().unwrap().outer
    }

    #[allow(dead_code)]
    pub(super) fn outer_contexts(&self) -> Vec<AbstractFrame> {
        let mut i = self.stack_frame.len() - 1;
        let mut v = vec![];
        while let Some(outer) = self.stack_frame[i].outer {
            i -= outer;
            let scope = self.stack_frame[i].abstract_state.clone().unwrap();
            v.push(scope);
        }
        v.reverse();
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
        self.capture_events += 1;
        // Stage-C loop adoption: deliberately *not* a claim barrier. This
        // fires conservatively at every block-literal call site (the
        // callee could heapify), but every such site also emits a runtime
        // capture guard whose miss deopts — and a deopt abandons the
        // compiled loop continuation (chain escalation) before the next
        // iteration's home read could consume a stale home. An actual
        // out-of-unit block run is what invisibly stores without a guard,
        // and that is the `all_frames_unbox` barrier's job.
        let mut i = self.stack_frame.len() - 1;
        loop {
            let frame = &mut self.stack_frame[i];
            if let Some(outer) = frame.outer {
                i -= outer;
                let frame = &mut self.stack_frame[i];
                frame
                    .abstract_state
                    .as_mut()
                    .unwrap()
                    .unset_no_capture_guard();
                // A possible capture reaching a frame with unboxed
                // speculated locals kills the speculation: a heapified
                // copy of the frame would carry stale LFP slots for
                // them (doc/chain_deopt.md §7).
                if !frame.speculated_floats.is_empty() {
                    frame.speculation_poisoned = true;
                }
            } else {
                return;
            }
        }
    }

    // ===== Unboxed-locals speculation (doc/chain_deopt.md §5 steps 4–5) =====





    ///
    /// Whether any frame on the compile stack currently has an armed
    /// unboxed-Float speculation. Sampled when a specialized subtree is
    /// recorded (`compile_specialized_func`): a subtree compiled under an
    /// armed speculation reads the arming frame's FP save/spill area and
    /// must not be recompiled standalone (#1140).
    ///
    pub(super) fn under_armed_speculation(&self) -> bool {
        self.stack_frame
            .iter()
            .any(|f| !f.speculated_floats.is_empty())
    }


    fn check_exception_handler(&self, begin: usize, end: usize) -> bool {
        self.stack_frame[begin..end].iter().any(|f| {
            let iseq_id = f.iseq_id();
            let callsite = f.callid.unwrap();
            let pc = self.store[callsite].bc_pos;
            self.store[iseq_id].get_exception_dest(pc).is_some()
        })
    }

    ///
    /// Whether the *current* frame's instruction at `bc_pos` is covered by
    /// an entry of its iseq's exception table.
    ///
    /// The companion to [`Self::check_exception_handler`], which covers only
    /// the chain's *suspended* frames (each at its in-progress call site) —
    /// the frame currently being compiled is not in that range, so a
    /// non-local exit (`MethodRet` / `BlockBreak`) written inside a
    /// `begin`..`ensure` region asks this before choosing the specialized
    /// teardown: that teardown is a pure machine-level frame pop, and only
    /// the generic path's `handle_error` unwind runs the `ensure` bodies
    /// (and the `$!` restore on leaving a rescue clause). Conservative like
    /// its companion: any table entry forces the generic path (#1179).
    ///
    pub(super) fn in_protected_region(&self, bc_pos: BcIndex) -> bool {
        self.iseq().get_exception_dest(bc_pos).is_some()
    }

    ///
    /// Try to arrange a JIT-spliced non-local exit (issue #1185): a
    /// `break` / non-local `return` written inside its own frame's
    /// `begin`..`ensure` region can *defer* its unwind and jump straight
    /// into the shared `ensure` body — which is ordinary, already-compiled
    /// code of this frame — whose `EnsureEnd` then delivers it through the
    /// specialized teardown. That models the unwind edge in the CFG (the
    /// `ensure`'s writes become visible to the abstract interpreter) and
    /// skips the whole generic unwind / chain-deopt / VM stint.
    ///
    /// Returns the `ensure` body's entry block on success, after recording
    /// the region's `EnsureEnd` in the frame's [`JitStackFrame::spliced_ensures`]
    /// registry. `None` refuses (stage 1 is deliberately narrow) and the
    /// caller falls back to the generic lowering, which handles every case.
    ///
    pub(super) fn try_splice_exit(
        &mut self,
        bc_pos: BcIndex,
        kind: SplicedExitKind,
    ) -> Option<BasicBlockId> {
        // A dispatch arm compiles straight-line per-class code at one call
        // site; it must not spawn branch entries into the frame's CFG.
        if self.in_dispatch_arm() {
            return None;
        }
        // A loop-rooted frame compiles only the loop's basic blocks, and
        // the shared `ensure` body may lie outside that range. (The common
        // case — an exit in a specialized inlined frame — compiles the
        // whole iseq.)
        if matches!(self.jit_type(), JitType::Loop(_)) {
            return None;
        }
        // The specialized teardown must exist: an in-unit chain with no
        // handler in any *suspended* frame. (The current frame's own
        // handler is the very thing being spliced.)
        match kind {
            SplicedExitKind::Break => self.iter_caller_specialized_ids()?,
            SplicedExitKind::MethodReturn => self.method_caller_specialized_ids()?,
        };
        let iseq = self.iseq();
        // Exactly one covering region, and it has an `ensure`.
        let ensure_pc = iseq.single_covering_ensure(bc_pos)?;
        // The shared `ensure` copy practically always heads a basic block
        // (rescue clauses branch to it; without rescue it coincides with
        // the `else` join) — but refuse rather than assume.
        let dest_bb = iseq.bb_info.is_bb_head(ensure_pc)?;
        // Pair the body with its `EnsureEnd` by forward scan.
        let mut end = None;
        // Bound the scan by the sp table (one entry per instruction).
        let limit = iseq.sp_table_len().min(ensure_pc.to_usize() + 1024);
        let mut i = ensure_pc;
        while i.to_usize() < limit {
            match TraceIr::from_pc(iseq.get_pc(i), self.store) {
                TraceIr::EnsureEnd => {
                    end = Some(i);
                    break;
                }
                // The spliced deferral is consumed only by the region's
                // `EnsureEnd` (or discarded by `handle_error` on a raise).
                // An exit that leaves the frame *without* raising — a `next`
                // (`Ret`), another `break` / non-local `return`, `retry` /
                // `redo` — would tear the frame down with the deferral still
                // parked, leaking a stale per-lfp entry. The VM path runs
                // these under `handle_error`, which discards; the compiled
                // teardowns do not. Refuse the splice for such a body.
                TraceIr::Ret(..)
                | TraceIr::MethodRet(..)
                | TraceIr::BlockBreak(..)
                | TraceIr::Retry
                | TraceIr::Redo => return None,
                _ => {}
            }
            i = i + 1;
        }
        let end = end?;
        // A nested handler inside the body would put its own `EnsureEnd`
        // between the two and break the pairing; refuse.
        if iseq.any_handler_intersects(ensure_pc..end) {
            return None;
        }
        let entry = self
            .current_frame_mut()
            .spliced_ensures
            .entry(end)
            .or_insert((false, false));
        match kind {
            SplicedExitKind::Break => entry.0 = true,
            SplicedExitKind::MethodReturn => entry.1 = true,
        }
        Some(dest_bb)
    }

    ///
    /// Hint variant of stack-offset computation for `LoadDynVar` /
    /// `StoreDynVar`. Returns the chain of [`SpecializedId`]s for
    /// frames `[outer..current)` together with `extra` — the sum
    /// of the dynamic `stack_offset - base_stack_offset` per frame
    /// at *this* AsmIr emission time. The pre-codegen resolve pass
    /// computes `sum(map[id]) + extra`. The boolean mirrors the
    /// `not_captured` flag from `state.outer_no_capture_guard`.
    ///
    pub(super) fn outer_specialized_ids(
        &self,
        state: &AbstractState,
        outer: usize,
    ) -> Option<(Vec<SpecializedId>, usize, bool)> {
        let not_captured = state.outer_no_capture_guard(outer)?;
        let outer = self.outer_pos(outer)?;
        let end = self.stack_frame.len() - 1;
        let chain = &self.stack_frame[outer..end];
        let ids = chain.iter().map(|f| f.specialized_id).collect();
        let extra = chain
            .iter()
            .map(|f| f.stack_offset - f.base_stack_offset)
            .sum();
        Some((ids, extra, not_captured))
    }

    ///
    /// Record the `UsingFpr` a specialized call site's `fpr_save_cont`
    /// will save, keyed by the callee instance (stage 1' write-through).
    ///
    pub(super) fn record_call_site_fpr_save(&mut self, callee: SpecializedId, using: UsingFpr) {
        self.call_site_fpr_saves.insert(callee, using);
    }

    ///
    /// The *parked* mode an outer frame holds for `slot`: `Some(fpr)` iff
    /// it is a Float-guarded `Sf` view — the only shape the write-through
    /// keep applies to.
    ///
    pub(super) fn outer_parked_sf_float(&self, outer: usize, slot: SlotId) -> Option<crate::codegen::FPReg> {
        let pos = self.outer_pos(outer)?;
        let st = self.stack_frame[pos].abstract_state.as_ref()?;
        match st.mode(slot) {
            LinkMode::Sf(fpr, crate::codegen::jitgen::state::SfGuarded::Float) => Some(fpr),
            _ => None,
        }
    }

    ///
    /// Build the [`OuterFprHome`] hint for a write-through refresh of the
    /// frame `outer` levels out, and record the kept view for the
    /// bet-confirmation drain.
    ///
    pub(super) fn keep_outer_sf_view(
        &mut self,
        ids: Vec<SpecializedId>,
        extra: usize,
        outer: usize,
        slot: SlotId,
        fpr: crate::codegen::FPReg,
    ) -> Option<OuterFprHome> {
        let pos = self.outer_pos(outer)?;
        self.kept_outer_views.push((pos, slot));
        self.outer_fpr_home_hint(ids, extra, outer, fpr)
    }

    ///
    /// Build the [`OuterFprHome`] hint for the frame `outer` levels out —
    /// without recording a kept view. A home *read* (stage 3a) records
    /// nothing: its validity is a fact about the paths reaching it (the
    /// `Sf(Float)` binding at the read point), not a claim the owner's
    /// continuation consumes.
    ///
    pub(super) fn outer_fpr_home_hint(
        &self,
        ids: Vec<SpecializedId>,
        extra: usize,
        outer: usize,
        fpr: crate::codegen::FPReg,
    ) -> Option<OuterFprHome> {
        let pos = self.outer_pos(outer)?;
        let owner = self.stack_frame[pos].specialized_id;
        let callee = self.stack_frame.get(pos + 1)?.specialized_id;
        Some(OuterFprHome::Hint {
            ids,
            extra,
            owner,
            callee,
            fpr,
        })
    }

    ///
    /// Stage 2: try to *promote* an outer frame's plain-`S` slot to
    /// `Sf(Float)` at a float store from the current (inlined) frame,
    /// allocating a fresh spill-resident home in the owner's file. The
    /// physical store into that home rides the same
    /// [`AsmInst::StoreOuterFprHomeF`] as the stage-1' keep, resolved
    /// through the hint's spill branch (no save-set involvement).
    ///
    /// Soundness needs the store to dominate every *normal* exit of the
    /// whole call subtree — the owner's continuation consumes the claim
    /// path-insensitively, and a path on which the store did not run
    /// leaves the home garbage. The check is the recursive
    /// dominating-prefix condition:
    ///
    /// - the store sits in the current frame's **entry basic block**
    ///   (everything there executes on every entry of the frame — a
    ///   conditional or loop-guarded position lands in a later block);
    /// - every intermediate frame of the chain is suspended (at its
    ///   in-progress call) inside **its own** entry block, so reaching
    ///   that frame implies reaching the store.
    ///
    /// This correctly refuses zero-trip shapes: `[].each { x = 1.5 }`
    /// never runs the block, and `Array#each`'s `yield` sits inside its
    /// `while`, i.e. not in the entry block. Direct-yield chains
    /// (`def call_block; yield; end`, `tap`) pass.
    ///
    /// Deopt/raise paths need no dominance: the owner's compiled
    /// continuation never runs on them (the stage-1' escalation
    /// argument), and the boxed slot store keeps slot-level truth on
    /// every path either way.
    ///
    pub(super) fn try_promote_outer_sf(
        &mut self,
        state: &mut AbstractState,
        ids: &[SpecializedId],
        extra: usize,
        outer: usize,
        slot: SlotId,
        bc_pos: BcIndex,
    ) -> Option<(crate::codegen::FPReg, OuterFprHome)> {
        if !Self::in_entry_block(self.iseq(), bc_pos) {
            return None;
        }
        let pos = self.outer_pos(outer)?;
        for f in &self.stack_frame[pos + 1..self.stack_frame.len() - 1] {
            let callid = f.callid?;
            let fiseq = &self.store[f.iseq_id()];
            if !Self::in_entry_block(fiseq, self.store[callid].bc_pos) {
                return None;
            }
        }
        {
            let parked = self.stack_frame[pos].abstract_state.as_ref()?;
            let m = parked.mode(slot);
            if !matches!(parked.mode(slot), LinkMode::S(_)) {
                return None;
            }
        }
        let fpr = self.stack_frame[pos]
            .abstract_state
            .as_mut()
            .unwrap()
            .alloc_spill_home(slot);
        state.promote_outer_sf(outer, slot, fpr);
        let home = self.keep_outer_sf_view(ids.to_vec(), extra, outer, slot, fpr)?;
        Some((fpr, home))
    }

    /// Whether *pos* lies in the entry basic block of *iseq* — the block
    /// every activation of the frame executes from its first instruction.
    fn in_entry_block(iseq: &ISeqInfo, pos: BcIndex) -> bool {
        iseq.bb_info.get_bb_id(pos) == iseq.bb_info.get_bb_id(BcIndex::from(0))
    }

    /// Mark for [`Self::drain_kept_outer_views`].
    pub(super) fn kept_outer_views_mark(&self) -> usize {
        self.kept_outer_views.len()
    }

    ///
    /// Bet-confirmation for the write-through keeps a call's subtree made
    /// (stage 1'): the subtree had a deopt-able exit or a generic yield,
    /// so a salvage re-entry can run its compiled remainder — or the VM
    /// can run it outright — writing the kept slots with nothing
    /// refreshing the raw homes. Re-widen every view the subtree kept:
    /// the boxed slot stores were emitted either way, so the slots are
    /// current and `S` is sound; the emitted refreshes go dead (their
    /// resolve keeps the store — it targets the owner's save area or
    /// spill slot, both dead once nothing reads the view).
    ///
    // Dormant by construction today, verified by probe: a compiled
    // generic `yield` cannot currently appear *inside* a specialized
    // subtree — a call site passing `&blk` refuses specialization, and
    // the given-block resolver only fails at a unit root, which is not a
    // subtree. The belt exists for the day the specializer widens.
    #[coverage(off)]
    pub(super) fn drain_kept_outer_views(&mut self, mark: usize, state: &mut AbstractState) {
        let current = self.stack_frame.len() - 1;
        for (pos, slot) in self.kept_outer_views.split_off(mark) {
            if pos > current {
                // The target frame was popped with its subtree; its state
                // died with it.
                continue;
            }
            if pos == current {
                state.invalidate_innermost(slot);
            } else {
                if let Some(frame) = self.stack_frame[pos].abstract_state.as_mut() {
                    frame.invalidate_slot(slot);
                }
                state.widen_outer_slot(current - pos, slot);
            }
        }
    }

    ///
    /// Resolve a `DynVarOffset::Hint` chain: the distance from the current
    /// frame's `rbp`/`x29` out to the target frame's, summed frame by
    /// frame. Used by [`Self::resolve_dyn_var_offsets`].
    ///
    /// Each frame in the chain contributes its local area
    /// (`total - PROLOGUE_OVERHEAD`) plus what an inlined callee consumes to
    /// establish a frame on top of it, and the caller adds the `extra` (the
    /// `FprSave` areas live at the call). Every frame kind reaches that one
    /// local area: it is what the `Init` prologue reserves, and what the
    /// loop-JIT entry pins `sp`/`rsp` to — see `emit_loop_jit_rsp_bump`,
    /// whose two producers would otherwise disagree by this unit's spill
    /// region.
    ///
    pub(super) fn resolve_specialized_id_chain(&self, ids: &[SpecializedId]) -> usize {
        ids.iter()
            .map(|id| self.frame_sizes_or_panic(*id).total)
            .sum()
    }

    fn frame_sizes_or_panic(&self, id: SpecializedId) -> FrameSizes {
        *self.specialized_frame_sizes.get(&id).unwrap_or_else(|| {
            panic!(
                "frame-size hint references {:?} but no frame sizes were recorded",
                id
            )
        })
    }

    ///
    /// Walk every `AsmIr` reachable from `asm_info` (the main inst
    /// stream, inline / outline bridges, and recursively the
    /// `specialized_methods`) and rewrite each
    /// `DynVarOffset::Hint(...)` into `DynVarOffset::Concrete(...)`
    /// using [`Self::specialized_frame_sizes`]. Must be called after
    /// every frame has been popped (so the size map is fully
    /// populated) and before `gen_machine_code` runs — code
    /// generation [`unwrap_concrete`s](DynVarOffset::unwrap_concrete)
    /// each hint and would panic otherwise.
    ///
    pub(super) fn resolve_dyn_var_offsets(&self, asm_info: &mut AsmInfo) {
        for (_, ir) in asm_info.iter_ir_mut() {
            for inst in ir.inst_iter_mut() {
                self.resolve_dyn_var_offset_in(inst);
            }
        }
        for (ir, _, _) in asm_info.iter_outline_bridges_mut() {
            for inst in ir.inst_iter_mut() {
                self.resolve_dyn_var_offset_in(inst);
            }
        }
        for (ir, _) in asm_info.iter_inline_bridges_mut() {
            for inst in ir.inst_iter_mut() {
                self.resolve_dyn_var_offset_in(inst);
            }
        }
        for SpecializeInfo { info, .. } in asm_info.iter_specialized_methods_mut() {
            self.resolve_dyn_var_offsets(info);
        }
    }

    fn resolve_dyn_var_offset_in(&self, inst: &mut AsmInst) {
        match inst {
            AsmInst::LoadDynVarSpecialized { offset, .. }
            | AsmInst::StoreDynVarSpecialized { offset, .. }
            | AsmInst::MethodRetSpecialized {
                rbp_offset: offset, ..
            }
            | AsmInst::BlockBreakSpecialized {
                rbp_offset: offset, ..
            } => {
                if let DynVarOffset::Hint { ids, extra } = offset {
                    let resolved = self.resolve_specialized_id_chain(ids) + *extra;
                    *offset = DynVarOffset::Concrete(resolved);
                }
            }
            AsmInst::Init {
                prologue_offset, ..
            } => {
                if let PrologueOffset::Hint(id) = prologue_offset {
                    let sizes = self.frame_sizes_or_panic(*id);
                    // Reserve exactly what the VM's `init_method` reserves
                    // for the same iseq, plus this compile's spill region.
                    //
                    // The VM reserves the bytecode operand,
                    // `FnInitInfo::stack_offset * 16`. Note that this is
                    // *not* `ISeqInfo::stack_offset()`, which is the same
                    // expression plus a further 16 — so the VM's reservation
                    // is `base_stack_offset - PROLOGUE_OVERHEAD`, and adding
                    // the spill region (the rest of `total`) gives
                    // `total - PROLOGUE_OVERHEAD`.
                    let prologue_bytes = sizes.total - PROLOGUE_OVERHEAD;
                    *prologue_offset = PrologueOffset::Concrete(prologue_bytes);
                }
            }
            AsmInst::EnsureEnd {
                spliced_break,
                spliced_ret,
                ..
            } => {
                for off in [spliced_break, spliced_ret].into_iter().flatten() {
                    if let DynVarOffset::Hint { ids, extra } = off {
                        let resolved = self.resolve_specialized_id_chain(ids) + *extra;
                        *off = DynVarOffset::Concrete(resolved);
                    }
                }
            }
            AsmInst::StoreOuterFprHomeF { home, .. }
            | AsmInst::LoadOuterFprHomeF { home, .. }
            | AsmInst::GuardFloatToOuterHomeF { home, .. } => {
                if let OuterFprHome::Hint {
                    ids,
                    extra,
                    owner,
                    callee,
                    fpr,
                } = home
                {
                    let sigma = (self.resolve_specialized_id_chain(ids) + *extra) as i64;
                    let disp = if fpr.0 < crate::codegen::PHYS_FPR_POOL {
                        // Pool-resident: the home is the owner's call-site
                        // save slot — present only if the emitted save still
                        // covers the fpr (a later widen may have shrunk the
                        // set; the refresh is then dead and elided).
                        match self.call_site_fpr_saves.get(callee) {
                            Some(using) if using[fpr.0] => {
                                let rank = using[..fpr.0].count_ones() as i64;
                                let sizes = self.frame_sizes_or_panic(*owner);
                                Some(
                                    sigma
                                        - (sizes.total as i64 - PROLOGUE_OVERHEAD as i64)
                                        - using.offset() as i64
                                        + 8 * rank,
                                )
                            }
                            _ => None,
                        }
                    } else {
                        // Spilled: the home is the owner's own spill slot,
                        // valid regardless of the save set.
                        let sizes = self.frame_sizes_or_panic(*owner);
                        Some(
                            sigma
                                - (sizes.base as i64 - 24
                                    + 8 * (fpr.0 - crate::codegen::PHYS_FPR_POOL) as i64),
                        )
                    };
                    *home = OuterFprHome::Concrete(disp);
                }
            }
            AsmInst::LoopJitRspBump { offset } => {
                if let LoopRspOffset::Hint(id) = offset {
                    let sizes = self.frame_sizes_or_panic(*id);
                    // The depth to pin `rsp`/`sp` to, not a delta: the
                    // frame this body runs in may have been built by
                    // either producer, and only one of them reserved
                    // this unit's spill region (see
                    // `Codegen::emit_loop_jit_rsp_bump`). Both reach the
                    // same local area, which is the one rule
                    // `resolve_specialized_id_chain` relies on.
                    debug_assert!(sizes.total >= PROLOGUE_OVERHEAD);
                    *offset = LoopRspOffset::Concrete(sizes.total - PROLOGUE_OVERHEAD);
                }
            }
            _ => {}
        }
    }

    ///
    /// Hint-form chain of [`SpecializedId`]s between the method
    /// caller and the current frame. Returns `(ids, extra)` where
    /// `extra` is the live `stack_offset - base_stack_offset` sum
    /// captured at AsmIr emission time — see
    /// [`Self::outer_specialized_ids`] for the rationale.
    ///
    pub(super) fn method_caller_specialized_ids(&self) -> Option<(Vec<SpecializedId>, usize)> {
        // Method specialization is lowered on both arches now, so the caller
        // chain is encoded for `MethodRetSpecialized` on aarch64 too — as is
        // block inlining, see `iter_caller_specialized_ids`.
        //
        // The chain starts at the HOME method frame itself, so the home may
        // sit anywhere in the unit — including the compile root. The
        // teardown (`method_return_specialized`: `lea rbp += Σ; leave; ret`)
        // only needs the frames between the current frame and the home; the
        // final `ret` returns from the home to its *dynamic* caller, whose
        // post-call frame pop is rbp-derived and therefore correct no matter
        // how many inlined frames were flown over. Requiring the home's
        // caller to be in-unit (the previous form) sent every root-homed
        // non-local return down the runtime `err_method_return` unwind —
        // which, with side-exit escalation unconditional, ran a chain-deopt
        // walk per `return` (the `throw` benchmark: one walk per call).
        let home = {
            let (_, dist) = self.current_method_frame()?;
            self.stack_frame.len() - 1 - dist
        };
        let begin = home;
        let end = self.stack_frame.len() - 1;
        if self.check_exception_handler(begin, end) {
            return None;
        }
        let chain = &self.stack_frame[begin..end];
        let ids = chain.iter().map(|f| f.specialized_id).collect();
        let extra = chain
            .iter()
            .map(|f| f.stack_offset - f.base_stack_offset)
            .sum();
        Some((ids, extra))
    }

    ///
    /// Hint-form chain of [`SpecializedId`]s between the iter caller
    /// and the current frame. Returns `(ids, extra)` — see
    /// [`Self::method_caller_specialized_ids`].
    ///
    pub(super) fn iter_caller_specialized_ids(&self) -> Option<(Vec<SpecializedId>, usize)> {
        // Block inlining is lowered on aarch64 now, so `break` out of an inlined
        // block can encode its caller chain for `BlockBreakSpecialized` too.
        let caller = self.iter_caller_pos()?;
        let begin = caller + 1;
        let end = self.stack_frame.len() - 1;
        if self.check_exception_handler(begin, end) {
            return None;
        }
        let chain = &self.stack_frame[begin..end];
        let ids = chain.iter().map(|f| f.specialized_id).collect();
        let extra = chain
            .iter()
            .map(|f| f.stack_offset - f.base_stack_offset)
            .sum();
        Some((ids, extra))
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

    /// The refinement set the body being compiled resolves under.
    pub(crate) fn refinements(&self) -> RefinementSetId {
        self.refinements
    }

    pub(crate) fn class_version(&self) -> u32 {
        self.class_version
    }

    pub(crate) fn const_version(&self) -> u64 {
        self.const_version
    }

    pub(super) fn specialize_level(&self) -> usize {
        self.current_frame().specialize_level
    }

    ///
    /// D1 forwarding-rest deferral decision for the *current* frame.
    ///
    /// `Some((rest_local, src, len))` iff the current frame is a
    /// specialized pure forwarding trampoline `def f(...) = g(...)`.
    /// The specialization depth does not matter: every frame (root or
    /// specialized) runs the standard prologue (`pushq rbp; movq rbp,
    /// rsp`), so `f`'s *direct physical caller* frame is always the
    /// value `f` saved at `[rbp]` — and for a method trampoline
    /// `method_caller_callsite` is exactly that direct caller's call
    /// site (`current_method_frame` resolves `f` itself at offset 0).
    /// The caller spilled that site's args to its frame slots
    /// (`write_back_recv_and_callargs`) before the call, and a
    /// trampoline-into-trampoline chain cannot defer through a deferred
    /// source: the parent's own forwarding call site has a splat, which
    /// fails the `is_simple_call` gate below. `src` is the caller call
    /// site's positional args base (caller register numbering, read via
    /// that saved caller `rbp`), `len` its positional count,
    /// `rest_local` `f`'s synthetic rest local slot.
    ///
    pub(super) fn forward_rest_deferral(&self) -> Option<DeferredForward> {
        // D1 deferred-rest is lowered on both backends now: the side-exit
        // `forward_rest` materialize (`gen_forward_rest_materialize` /
        // `a64_gen_forward_rest_materialize`) and the `SetArgumentsForwarded`
        // inline source-routing (`jit_set_arguments_forwarded` /
        // `a64_set_arguments_forwarded_deferred`). The caller-slot addressing is
        // arch-neutral (`[fp - rbp_local(slot)]`, since
        // `RBP_LOCAL_FRAME == (BP_CFP + CFP_LFP) + 8` on both arches), so the
        // whole deferral is enabled uniformly here.
        if !self.is_specialized() {
            return None;
        }
        let fid = self.func_id();
        let rest_local = self.store.forwarding_trampoline_rest(fid)?;
        let cid = self.method_caller_callsite()?;
        if !self.store.is_simple_call(fid, cid) {
            return None;
        }
        let cs = &self.store[cid];
        // K1: literal keywords at the caller defer alongside the rest —
        // recorded here as (f's `**kwrest` local, the caller's kw base,
        // the names in slot order) and either source-routed into the
        // forwarded callee's declared kw params by the consume or vetoed
        // as a whole (`needs_rest_array`). A `**hash` splat stays on the
        // generic path: its keys are dynamic.
        if !cs.hash_splat_pos.is_empty() || cs.block_arg.is_some() {
            return None;
        }
        let kw = if cs.kw_args.is_empty() {
            None
        } else {
            // `def f(...)` always declares `**kwrest`.
            let kwrest_local = self.store[fid].kw_rest()?;
            // kw_args maps name -> offset from kw_pos; record names in
            // offset order so `names[i]` lives at `kw_pos + i`.
            let mut names = vec![IdentId::get_id(""); cs.kw_args.len()];
            for (name, i) in &cs.kw_args {
                names[*i] = *name;
            }
            Some((kwrest_local, cs.kw_pos, names.into_boxed_slice()))
        };
        // `pos_num == 0` (e.g. `NoArg.new` through the Ruby `Class#new`)
        // defers to an *empty* source range: the consume copies nothing
        // and the side-exit materialization (`create_array` with len 0)
        // rebuilds `[]` without touching the source pointer.
        Some(DeferredForward {
            rest_local,
            src: cs.args,
            len: cs.pos_num as u16,
            kw,
        })
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

    pub(super) fn remove_backedge(&mut self, bb: BasicBlockId) -> Option<Vec<SlotState>> {
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
    pub(super) fn new_backedge(&mut self, target: Vec<SlotState>, bb_pos: BasicBlockId) {
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_backedge:{bb_pos:?} {target:?}");
        self.current_frame_mut().backedge_map.insert(bb_pos, target);
    }

    ///
    /// Add new return branch with `state`.
    ///
    pub(super) fn new_return(&mut self, ret: ReturnState) {
        if let Some(pos) = self.caller_pos() {
            #[cfg(feature = "jit-debug")]
            eprintln!("   new_return:{:?}", ret);
            self.push_return_context(pos, ret);
        }
    }

    ///
    /// Add new return branch with `state`.
    ///
    pub(super) fn new_method_return(&mut self, ret: ReturnState) {
        if let Some(pos) = self.method_caller_pos() {
            #[cfg(feature = "jit-debug")]
            eprintln!("   new_method_return:{:?}", ret);
            self.push_return_context(pos, ret);
        }
    }

    ///
    /// Add new return branch with `state`.
    ///
    pub(super) fn new_break(&mut self, ret: ReturnState) {
        if let Some(pos) = self.iter_caller_pos() {
            #[cfg(feature = "jit-debug")]
            eprintln!("   new_break:{:?}", ret);
            self.push_return_context(pos, ret);
        }
    }

    pub(super) fn push_ir(&mut self, bb: Option<BasicBlockId>, ir: AsmIr) {
        let frame = self.current_frame_mut();
        frame.had_deopt |= ir.had_deopt();
        frame.deferred_rest |= ir.deferred_rest();
        frame.needs_rest_array |= ir.needs_rest_array();
        frame.ir.push((bb, ir));
    }

    pub(super) fn add_inline_bridge(
        &mut self,
        src_bb: Option<BasicBlockId>,
        ir: AsmIr,
        dest_bb: Option<BasicBlockId>,
    ) {
        let frame = self.current_frame_mut();
        frame.had_deopt |= ir.had_deopt();
        frame.deferred_rest |= ir.deferred_rest();
        frame.needs_rest_array |= ir.needs_rest_array();
        frame.inline_bridges.insert(src_bb, (ir, dest_bb));
    }

    pub(super) fn add_outline_bridge(&mut self, ir: AsmIr, dest: JitLabel, bbid: BasicBlockId) {
        let frame = self.current_frame_mut();
        frame.had_deopt |= ir.had_deopt();
        frame.deferred_rest |= ir.deferred_rest();
        frame.needs_rest_array |= ir.needs_rest_array();
        frame.outline_bridges.push((ir, dest, bbid));
    }
}
