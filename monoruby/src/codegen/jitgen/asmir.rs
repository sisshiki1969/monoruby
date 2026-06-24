use crate::bytecodegen::BinOpK;
// Only the x86-gated `gen_asm` driver uses these directly; the aarch64 driver
// lives in `arch/aarch64/compile.rs` with its own imports.
#[cfg(target_arch = "x86_64")]
use crate::codegen::jitgen::lir::{LInst, LSideExitKind, Lir};

use super::*;

// AsmIR→machine-code lowering. The per-arch backend (`compile`) provides the
// emission primitives + `compile_asmir_arch`; the arch-neutral dispatcher
// (`compile_shared`) owns the shared instruction arms and forwards the rest.
// x86 emits via `monoasm!`, aarch64 via `monoasm_arm64!`; both lower every
// `AsmInst`, so neither bails (see doc/aarch64-jitgen-plan.md).
#[cfg(target_arch = "x86_64")]
#[path = "../arch/x86_64/compile/mod.rs"]
mod compile;
#[cfg(target_arch = "aarch64")]
#[path = "../arch/aarch64/compile.rs"]
mod compile;
mod compile_shared;

pub(super) struct InlineProcedure {
    proc: Box<dyn FnOnce(&mut Codegen, &Store, &SideExitLabels, usize)>,
}

impl InlineProcedure {
    /// Wrap a context-carrying generator as an `LInst::Inline` payload. Lets the
    /// `compile_asmir` lowering route the not-yet-typed families (specialized
    /// inlined-frame ops, `gen_array_index*`) through the same `LInst::Inline`
    /// escape hatch as the inline-builtin generators, so *every* `AsmInst`
    /// lowers to an `LInst` (the §9a whole-region-buffering prerequisite).
    pub(in crate::codegen::jitgen) fn new(
        f: impl FnOnce(&mut Codegen, &Store, &SideExitLabels, usize) + 'static,
    ) -> Self {
        InlineProcedure { proc: Box::new(f) }
    }
}

impl std::fmt::Debug for InlineProcedure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InlineProcedure")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct AsmDeopt(usize);

#[derive(Debug, Clone, Copy)]
pub(crate) struct AsmError(usize);

/// §20 (B): how an array index reached the `ArrayIndex`/`ArrayIndexAssign`
/// records — a `u16` compile-time immediate, or a runtime `Fixnum` already loaded
/// into the index register (Rsi / x3). The data the old `ir.inline` closures
/// captured, now a plain `Clone` value.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum ArrayIndexKind {
    U16(u16),
    Fixnum,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct AsmEvict(usize);

pub(crate) struct SideExitLabels(Vec<DestLabel>);

impl SideExitLabels {
    fn new() -> Self {
        Self(vec![])
    }

    fn push(&mut self, label: DestLabel) {
        self.0.push(label);
    }
}

impl std::ops::Index<AsmDeopt> for SideExitLabels {
    type Output = DestLabel;

    fn index(&self, index: AsmDeopt) -> &Self::Output {
        &self.0[index.0]
    }
}

impl std::ops::Index<AsmError> for SideExitLabels {
    type Output = DestLabel;

    fn index(&self, index: AsmError) -> &Self::Output {
        &self.0[index.0]
    }
}

impl std::ops::Index<AsmEvict> for SideExitLabels {
    type Output = DestLabel;

    fn index(&self, index: AsmEvict) -> &Self::Output {
        &self.0[index.0]
    }
}

#[derive(Debug)]
pub(crate) struct AsmIr {
    codegen_mode: bool,
    inst: Vec<AsmInst>,
    side_exit: Vec<SideExit>,
    /// Set when this IR has emitted at least one deopt-able side exit
    /// (via [`Self::new_deopt`] / [`Self::new_deopt_with_pc`]). Read by
    /// the enclosing frame to taint any `ReturnValue::Const` it would
    /// otherwise propagate to its caller — see
    /// `JitStackFrame::had_deopt`.
    had_deopt: bool,
    /// D1: set by the forwarding consumer when it routed the
    /// trampoline's `g(...)` straight from the caller source.
    deferred_rest: bool,
    /// D1 veto: a forwarding consume of the deferred rest that is NOT
    /// source-routed (needs the real `Array`: 989-array-path / helper /
    /// generic / native callee). Producer skips `create_array` only
    /// when `deferred_rest && !needs_rest_array`.
    needs_rest_array: bool,
    /// Item ② step 2 (record collection): the typed-IR transfer stream, grown
    /// in lock-step with `inst` whenever a transfer/eviction primitive funnels
    /// through [`Self::transfer`] (codegen mode only). Not yet consumed — this
    /// is the faithful record stream that record-driven lowering will replay.
    /// Truncated alongside `inst` by `save`/`restore`.
    transfers: Vec<TransferIR>,
}

impl std::ops::Index<AsmEvict> for AsmIr {
    type Output = SideExit;

    fn index(&self, index: AsmEvict) -> &Self::Output {
        &self.side_exit[index.0]
    }
}

impl std::ops::IndexMut<AsmEvict> for AsmIr {
    fn index_mut(&mut self, index: AsmEvict) -> &mut Self::Output {
        &mut self.side_exit[index.0]
    }
}

/// Element-wise `Debug`-string equality, the `transfer` shadow check's
/// comparator: neither `AsmInst` nor `SideExit` is `PartialEq` (they carry
/// non-comparable payloads such as boxed closures), but both are `Debug`.
#[cfg(debug_assertions)]
fn dbg_slice_eq<T: std::fmt::Debug>(a: &[T], b: &[T]) -> bool {
    a.len() == b.len() && a.iter().zip(b).all(|(x, y)| format!("{x:?}") == format!("{y:?}"))
}

// private interface
impl AsmIr {
    fn new_label(&mut self, side_exit: SideExit) -> usize {
        let label = self.side_exit.len();
        self.side_exit.push(side_exit);
        label
    }
}

// public interface
impl AsmIr {
    pub(super) fn new(ctx: &JitContext) -> Self {
        Self {
            codegen_mode: ctx.codegen_mode(),
            inst: vec![],
            side_exit: vec![],
            had_deopt: false,
            deferred_rest: false,
            needs_rest_array: false,
            transfers: vec![],
        }
    }

    ///
    /// Funnel a transfer/eviction primitive's typed-IR record (item ② step 2):
    /// collect it into the `transfers` stream (codegen mode only, so it stays in
    /// lock-step with the gated `inst`) and emit it. The emission is identical to
    /// the prior direct `record.emit(ir, …)`; collection is purely additive.
    ///
    /// **Analysis mode** (`codegen_mode == false`, the loop pre-pass) returns
    /// immediately: the abstract-state mutation already happened in the wrapper's
    /// `*_state` half, and `emit` only writes to `ir` — which the analysis pass
    /// discards (`analyse_basic_block` drops its local `AsmIr`). This is doc §9
    /// step 2's "the analysis pass calls the state halves and **skips
    /// emission**", and it stops the dead `side_exit` growth a guarded record's
    /// `deopt_from_point` would otherwise cause (the waste doc §10 flagged). It is
    /// provably safe: `emit`'s signature (`fn emit(self, ir: &mut AsmIr)`) cannot
    /// touch frame/abstract state, as the shadow check below independently relies
    /// on.
    ///
    /// In debug builds a **shadow check** replays the record alone into a scratch
    /// buffer and asserts it reproduces exactly the `AsmInst`s *and* `SideExit`s
    /// this call appended. That proves `TransferIR::emit` is a pure function of
    /// the record and the current `side_exit` cursor — its only codegen input,
    /// since a guarded record materializes its deopt as `AsmDeopt(side_exit.len())`
    /// (the scratch is pre-padded to the same length so the index matches). This
    /// is the self-containment record-driven lowering relies on, and a standing
    /// guard against a future transfer that reads other frame/codegen state.
    ///
    pub(super) fn transfer(&mut self, t: TransferIR) {
        if !self.codegen_mode {
            return;
        }
        self.transfers.push(t.clone());
        #[cfg(not(debug_assertions))]
        t.emit(self);
        #[cfg(debug_assertions)]
        {
            let (inst_start, se_start) = (self.inst.len(), self.side_exit.len());
            t.clone().emit(self);
            // Replay into a scratch whose deopt cursor (`side_exit.len()`) matches,
            // so a materialized `AsmDeopt` index is identical.
            let mut scratch = AsmIr::shadow_scratch();
            scratch
                .side_exit
                .resize_with(se_start, || SideExit::Evict(None));
            t.clone().emit(&mut scratch);
            debug_assert!(
                dbg_slice_eq(&self.inst[inst_start..], &scratch.inst)
                    && dbg_slice_eq(&self.side_exit[se_start..], &scratch.side_exit[se_start..]),
                "TransferIR replay mismatch: {t:?}\n  real inst:    {:?}\n  replay inst:  {:?}\n  real exit:    {:?}\n  replay exit:  {:?}",
                &self.inst[inst_start..],
                &scratch.inst,
                &self.side_exit[se_start..],
                &scratch.side_exit[se_start..],
            );
        }
    }

    /// An empty codegen-mode `AsmIr` for the [`Self::transfer`] shadow check —
    /// the replay target. Needs no `JitContext`.
    #[cfg(debug_assertions)]
    fn shadow_scratch() -> Self {
        Self {
            codegen_mode: true,
            inst: vec![],
            side_exit: vec![],
            had_deopt: false,
            deferred_rest: false,
            needs_rest_array: false,
            transfers: vec![],
        }
    }

    pub(super) fn had_deopt(&self) -> bool {
        self.had_deopt
    }

    pub(super) fn deferred_rest(&self) -> bool {
        self.deferred_rest
    }

    pub(super) fn set_deferred_rest(&mut self) {
        self.deferred_rest = true;
    }

    pub(super) fn needs_rest_array(&self) -> bool {
        self.needs_rest_array
    }

    pub(super) fn set_needs_rest_array(&mut self) {
        self.needs_rest_array = true;
    }

    pub(super) fn push(&mut self, inst: AsmInst) {
        if self.codegen_mode {
            self.inst.push(inst);
        }
    }

    pub(super) fn inst_iter_mut(&mut self) -> std::slice::IterMut<'_, AsmInst> {
        self.inst.iter_mut()
    }

    pub(super) fn inst_iter(&self) -> std::slice::Iter<'_, AsmInst> {
        self.inst.iter()
    }

    ///
    /// §21 — the record-stream optimization seam (Path 2).
    ///
    /// `inst` *is* the ordered, replayable instruction stream: handlers build it
    /// during the analysis/codegen walk (`traceir_to_asmir`) and
    /// `gen_machine_code` replays it afterwards to emit machine code
    /// (`compile_asmir` per `AsmInst`). This method is the arch-neutral layer
    /// boundary where optimization passes over that typed stream belong — run
    /// once per block (and per bridge) between AsmIR construction and emission.
    /// `inst` is private to this module, so passes live here.
    ///
    /// Dropping instructions is sound: branch targets are *labels* (`JitLabel`)
    /// and deopt targets index the `side_exit` vec (`AsmEvict`/`AsmDeopt`),
    /// never `inst` positions, so a removal cannot perturb control-flow or
    /// deopt resolution.
    ///
    /// **Pass 1 — self-move elimination.** A `RegMove`/`FprMove` whose source
    /// and destination coincide is a no-op (see [`AsmInst::is_self_move`]).
    /// Returns the number of instructions removed (instrumentation for the
    /// `jit-log` feature, which confirms the seam fires).
    ///
    pub(super) fn optimize_peephole(&mut self) -> usize {
        let before = self.inst.len();
        self.inst.retain(|inst| !inst.is_self_move());
        before - self.inst.len()
    }

    pub(super) fn is_empty(&self) -> bool {
        self.inst.is_empty()
    }

    ///
    /// Does this block's instruction stream end with an unconditional
    /// terminator (so the code physically following it is not reached by
    /// fall-through)? See [`AsmInst::is_unconditional_terminator`]; the
    /// answer is conservative (an empty stream, or one ending in any
    /// non-terminator, is treated as fall-through).
    ///
    pub(super) fn ends_unconditionally(&self) -> bool {
        self.inst
            .last()
            .is_some_and(AsmInst::is_unconditional_terminator)
    }

    ///
    /// If this block's whole body is a label, optional source-position
    /// markers, and a single trailing `Deopt` (e.g. a loop's natural exit
    /// block, which only deopts to the interpreter), return the block label
    /// and the deopt. Such a block is a pure indirection to its deopt handler,
    /// so the aarch64 backend emits the deopt inline at the block label rather
    /// than laying a cold handler and branching to it — letting predecessors
    /// land straight on the deopt code. `BcIndex` markers emit no machine code
    /// (they only feed the perf/emit-asm source map) and are ignored here.
    ///
    /// aarch64-only: x86 lays deopt handlers on the cold page and has no
    /// inline-handler indirection to collapse.
    ///
    #[cfg(target_arch = "aarch64")]
    pub(super) fn as_pure_deopt(&self) -> Option<(JitLabel, AsmDeopt)> {
        let (first, rest) = self.inst.split_first()?;
        let AsmInst::Label(bb) = first else {
            return None;
        };
        let (last, middle) = rest.split_last()?;
        let AsmInst::Deopt(deopt) = last else {
            return None;
        };
        middle
            .iter()
            .all(|i| matches!(i, AsmInst::BcIndex(_)))
            .then_some((*bb, *deopt))
    }

    ///
    /// The single `SideExit` of a pure-deopt block (see [`Self::as_pure_deopt`])
    /// when it is a plain `Deoptimize(pc, write_back)` and the block's only side
    /// exit, returned as `(pc, write_back)`. `None` otherwise (e.g. an evict /
    /// recompile-deopt, or extra side exits), so callers fall back to the
    /// ordinary cold-handler path. aarch64-only (see [`Self::as_pure_deopt`]).
    ///
    #[cfg(target_arch = "aarch64")]
    pub(super) fn pure_deopt_target(&self, deopt: AsmDeopt) -> Option<(BytecodePtr, &WriteBack)> {
        if self.side_exit.len() == 1
            && let SideExit::Deoptimize(pc, wb) = &self.side_exit[deopt.0]
        {
            Some((*pc, wb))
        } else {
            None
        }
    }

    pub(super) fn save(&mut self) -> (usize, usize, usize, bool, bool, bool, bool) {
        (
            self.inst.len(),
            self.transfers.len(),
            self.side_exit.len(),
            self.codegen_mode,
            self.had_deopt,
            self.deferred_rest,
            self.needs_rest_array,
        )
    }

    pub(super) fn restore(
        &mut self,
        (inst, transfers, side_exit, codegen_mode, had_deopt, deferred_rest, needs_rest_array): (
            usize,
            usize,
            usize,
            bool,
            bool,
            bool,
            bool,
        ),
    ) {
        self.inst.truncate(inst);
        self.transfers.truncate(transfers);
        self.side_exit.truncate(side_exit);
        self.codegen_mode = codegen_mode;
        self.had_deopt = had_deopt;
        self.deferred_rest = deferred_rest;
        self.needs_rest_array = needs_rest_array;
    }

    pub(crate) fn new_evict(&mut self) -> AsmEvict {
        let i = self.new_label(SideExit::Evict(None));
        AsmEvict(i)
    }

    pub(crate) fn new_deopt_with_pc(&mut self, state: &AbstractFrame, pc: BytecodePtr) -> AsmDeopt {
        let i = self.new_label(SideExit::Deoptimize(pc, state.get_write_back()));
        self.had_deopt = true;
        AsmDeopt(i)
    }

    pub(crate) fn new_deopt(&mut self, state: &AbstractFrame) -> AsmDeopt {
        let pc = state.pc();
        self.new_deopt_with_pc(state, pc)
    }

    ///
    /// Materialize a guarded transfer record's deopt **program point** (a
    /// [`DeoptPoint`], recorded by the analysis half — doc §9) into a `side_exit`
    /// label, returning its `AsmDeopt`. The emit/codegen half calls this so that
    /// `side_exit` construction lives entirely on the codegen side and the
    /// `TransferIR` stream stays codegen-independent. Equivalent to `new_deopt`,
    /// but driven by the recorded `(pc, write_back)` rather than reading the live
    /// `AbstractFrame`.
    ///
    pub(super) fn deopt_from_point(&mut self, point: &DeoptPoint) -> AsmDeopt {
        let i = self.new_label(SideExit::Deoptimize(point.pc(), point.write_back().clone()));
        self.had_deopt = true;
        AsmDeopt(i)
    }

    ///
    /// Like `new_deopt`, but the side exit recompiles the method/loop
    /// after a few misses (reason *reason*) before continuing to fall
    /// back to the interpreter. Used for the receiver-class guard of
    /// monomorphic-compiled BinCmp sites (Part B).
    ///
    /// *position* is the JIT entry position (`None` for a full
    /// method/block JIT, `Some(loop-header pc)` for a loop JIT) — the
    /// recompile target, NOT the deopt site `pc`.
    ///
    pub(crate) fn new_recompile_deopt(
        &mut self,
        state: &AbstractFrame,
        reason: RecompileReason,
        position: Option<BytecodePtr>,
    ) -> AsmDeopt {
        let pc = state.pc();
        let i = self.new_label(SideExit::RecompileDeoptimize(
            pc,
            state.get_write_back(),
            reason,
            position,
        ));
        self.had_deopt = true;
        AsmDeopt(i)
    }

    pub(crate) fn new_error_with_pc(&mut self, state: &AbstractFrame, pc: BytecodePtr) -> AsmError {
        let i = self.new_label(SideExit::Error(pc, state.get_write_back()));
        AsmError(i)
    }

    pub(crate) fn new_error(&mut self, state: &AbstractFrame) -> AsmError {
        let pc = state.pc();
        self.new_error_with_pc(state, pc)
    }
}

impl AsmIr {
    ///
    /// Save floating point registers in use.
    ///
    /// ### stack pointer adjustment
    /// - -`using_fpr`.offset()
    ///
    pub(crate) fn fpr_save(&mut self, using_fpr: UsingFpr) {
        self.push(AsmInst::FprSave(using_fpr, false));
    }

    pub(crate) fn fpr_save_cont(&mut self, using_fpr: UsingFpr) {
        self.push(AsmInst::FprSave(using_fpr, true));
    }

    ///
    /// Restore floating point registers in use.
    ///
    pub(crate) fn fpr_restore(&mut self, using_fpr: UsingFpr) {
        self.push(AsmInst::FprRestore(using_fpr, false));
    }

    pub(crate) fn fpr_restore_cont(&mut self, using_fpr: UsingFpr) {
        self.push(AsmInst::FprRestore(using_fpr, true));
    }

    ///
    /// Execute GC.
    ///
    /// ### in
    /// - rbx: &mut Executor
    /// - r12: &mut Globals
    ///
    /// ### destroy
    /// - rax, rcx
    /// - stack
    ///
    pub(super) fn exec_gc(&mut self, write_back: WriteBack, error: AsmError, check_stack: bool) {
        if check_stack {
            self.push(AsmInst::CheckStack {
                write_back: write_back.clone(),
                error,
            });
        }
        self.push(AsmInst::ExecGc { write_back, error });
    }

    pub(super) fn reg_move(&mut self, src: GP, dst: GP) {
        if src != dst {
            self.push(AsmInst::RegMove(src, dst));
        }
    }

    pub(super) fn reg_add(&mut self, r: GP, i: i32) {
        self.push(AsmInst::RegAdd(r, i));
    }

    pub(super) fn reg_sub(&mut self, r: GP, i: i32) {
        self.push(AsmInst::RegSub(r, i));
    }

    pub(super) fn loop_jit_rsp_bump(&mut self, offset: LoopRspOffset) {
        self.push(AsmInst::LoopJitRspBump { offset });
    }

    /// movq [rsp + (ofs)], R(r);
    pub(super) fn reg2rsp_offset(&mut self, r: GP, ofs: i32) {
        self.push(AsmInst::RegToRSPOffset(r, ofs));
    }

    /// movq [rsp + (ofs)], 0;
    pub(super) fn zero2rsp_offset(&mut self, ofs: i32) {
        self.push(AsmInst::ZeroToRSPOffset(ofs));
    }

    /// movq [rsp + (ofs)], (i);
    pub(super) fn u64torsp_offset(&mut self, i: u64, ofs: i32) {
        self.push(AsmInst::U64ToRSPOffset(i, ofs));
    }

    pub(crate) fn reg2stack(&mut self, src: GP, dst: impl Into<Option<SlotId>>) {
        if let Some(dst) = dst.into() {
            self.push(AsmInst::RegToStack(src, dst));
        }
    }

    /// Like `reg2stack`, but addresses the slot via the LFP (r14) so the store
    /// follows the frame across a `move_frame_to_heap`. Used for the result of
    /// a possibly-capturing call (see `AsmInst::RegToLfpStack`).
    pub(crate) fn reg2lfp_stack(&mut self, src: GP, dst: impl Into<Option<SlotId>>) {
        if let Some(dst) = dst.into() {
            self.push(AsmInst::RegToLfpStack(src, dst));
        }
    }

    pub(crate) fn stack2reg(&mut self, src: SlotId, dst: GP) {
        self.push(AsmInst::StackToReg(src, dst));
    }

    pub(crate) fn self2reg(&mut self, dst: GP) {
        self.push(AsmInst::StackToReg(SlotId::self_(), dst));
    }

    pub(super) fn fpr_move(&mut self, src: FPReg, dst: FPReg) {
        self.push(AsmInst::FprMove(src, dst));
    }

    ///
    /// Generate convert code from fpr to stack slots.
    ///
    /// ### out
    /// - rax: Value
    ///
    /// ### destroy
    /// - rcx
    ///
    pub fn fpr2stack(&mut self, fpr: FPReg, reg: SlotId) {
        self.push(AsmInst::FprToStack(fpr, reg));
    }

    pub fn lit2stack(&mut self, v: Value, reg: SlotId) {
        self.push(AsmInst::LitToStack(v, reg));
    }

    pub fn lit2reg(&mut self, v: Value, reg: GP) {
        self.push(AsmInst::LitToReg(v, reg));
    }

    ///
    /// Convert Fixnum to f64.
    ///
    /// ### in
    /// - R(*reg*): Value
    ///
    /// ### out
    /// - fpr(*fpr*)
    ///
    /// ### destroy
    /// - R(*reg*)
    ///
    pub fn fixnum2fpr(&mut self, reg: GP, x: FPReg) {
        self.push(AsmInst::FixnumToFpr(reg, x));
    }

    ///
    /// Float guard and unboxing.
    ///
    /// Unbox a Float Value and return f64.
    ///
    /// If the input Value was not Float, go to *deopt*.
    ///
    /// ### in
    ///
    /// - R(*reg*): Value
    ///
    /// ### out
    ///
    /// - fpr(*fpr*)
    ///
    /// ### destroy
    ///
    /// - rax, rdi
    ///
    pub fn float_to_fpr(&mut self, reg: GP, x: FPReg, deopt: AsmDeopt) {
        self.push(AsmInst::FloatToFpr(reg, x, deopt));
    }

    ///
    /// Move *f*(f64) to VirtFPReg(*x*).
    ///
    pub fn f64_to_fpr(&mut self, f: f64, x: FPReg) {
        self.push(AsmInst::F64ToFpr(f, x));
    }

    ///
    /// Move *i*(i63) to the stack *slot* and VirtFPReg(*x*).
    ///
    pub fn i64_to_stack_and_fpr(&mut self, i: i64, slot: SlotId, x: FPReg) {
        self.push(AsmInst::I64ToBoth(i, slot, x));
    }

    ///
    /// Deep copy *v* and store it to `rax`.
    ///
    /// ### out
    /// - rax: Value
    ///
    /// ### destroy
    /// - caller save registers
    ///
    pub(super) fn deep_copy_lit(&mut self, using_fpr: UsingFpr, val: Value) {
        self.push(AsmInst::DeepCopyLit(val, using_fpr));
    }

    pub(super) fn guard_array_ty(&mut self, r: GP, deopt: AsmDeopt) {
        self.push(AsmInst::GuardArrayTy(r, deopt));
    }

    pub(super) fn guard_capture(&mut self, deopt: AsmDeopt) {
        self.push(AsmInst::GuardCapture(deopt));
    }

    pub(super) fn deopt(&mut self, state: &AbstractFrame) {
        let exit = self.new_deopt(state);
        self.push(AsmInst::Deopt(exit));
    }

    pub(super) fn check_bop(&mut self, state: &AbstractFrame) {
        let deopt = self.new_deopt(state);
        self.push(AsmInst::CheckBOP { deopt });
    }

    pub(super) fn block_arg(
        &mut self,
        state: &AbstractFrame,
        ret: SlotId,
        outer: usize,
        call_site_bc_ptr: BytecodePtr,
    ) {
        let using_fpr = state.using_fpr_offset();
        let error = self.new_error(state);
        self.push(AsmInst::BlockArg {
            ret,
            _outer: outer,
            using_fpr,
            error,
            call_site_bc_ptr,
        });
    }

    pub(super) fn to_a(&mut self, state: &AbstractFrame, src: SlotId) {
        let using_fpr = state.using_fpr_offset();
        self.push(AsmInst::ToA { src, using_fpr });
    }

    pub(super) fn concat_str(&mut self, state: &AbstractFrame, arg: SlotId, len: u16) {
        let using_fpr = state.using_fpr_offset();
        self.push(AsmInst::ConcatStr {
            arg,
            len,
            using_fpr,
        });
    }

    pub(super) fn concat_regexp(&mut self, state: &AbstractFrame, arg: SlotId, len: u16) {
        let using_fpr = state.using_fpr_offset();
        self.push(AsmInst::ConcatRegexp {
            arg,
            len,
            using_fpr,
        });
    }

    pub(super) fn expand_array(
        &mut self,
        state: &AbstractFrame,
        dst: SlotId,
        len: u16,
        rest_pos: Option<u16>,
    ) {
        let using_fpr = state.using_fpr_offset();
        let len = len as _;
        let rest_pos = rest_pos.map(|v| v as _);
        self.push(AsmInst::ExpandArray {
            dst,
            len,
            rest_pos,
            using_fpr,
        });
    }

    pub(super) fn create_array(&mut self, src: SlotId, len: usize) {
        self.push(AsmInst::CreateArray { src, len });
    }

    pub(super) fn kw_rest(&mut self, rest_kw: Vec<(SlotId, IdentId)>) {
        self.push(AsmInst::RestKw { rest_kw });
    }

    ///
    /// Compare `lhs and `rhs` with "===" and return the result in rax.
    ///
    /// If `lhs` is Array, compare `rhs` and each element of `lhs`.
    ///
    pub(super) fn array_teq(&mut self, state: &AbstractFrame, lhs: SlotId, rhs: SlotId) {
        let using_fpr = state.using_fpr_offset();
        self.push(AsmInst::ArrayTEq {
            lhs,
            rhs,
            using_fpr,
        });
    }

    pub(crate) fn generic_binop(
        &mut self,
        state: &AbstractFrame,
        lhs: SlotId,
        rhs: SlotId,
        func: crate::executor::BinaryOpFn,
    ) {
        let using_fpr = state.using_fpr_offset();
        self.push(AsmInst::GenericBinOp {
            lhs,
            rhs,
            func,
            using_fpr,
        });
    }

    ///
    /// `==` / `!=` with a YJIT-`opt_eq`-style inline fast path: when
    /// both operands are non-heap, non-flonum immediates (Fixnum /
    /// nil / true / false / Symbol) the result is exactly bit
    /// equality; anything else (Float, heap, mixed numeric, custom
    /// `==`) falls back to the generic `cmp_*_values` C-call. No
    /// receiver-class guard (Part C, layered on Part 3-B).
    ///
    pub(super) fn opt_eq_cmp(
        &mut self,
        state: &AbstractFrame,
        lhs: SlotId,
        rhs: SlotId,
        kind: CmpKind,
        func: crate::executor::BinaryOpFn,
    ) {
        let using_fpr = state.using_fpr_offset();
        self.push(AsmInst::OptEqCmp {
            lhs,
            rhs,
            kind,
            func,
            using_fpr,
        });
    }

    pub(super) fn undef_method(&mut self, state: &AbstractFrame, undef: IdentId) {
        let using_fpr = state.using_fpr_offset();
        let error = self.new_error(state);
        self.push(AsmInst::UndefMethod { undef, using_fpr });
        self.handle_error(error);
    }

    pub(super) fn alias_method(&mut self, state: &AbstractFrame, new: SlotId, old: SlotId) {
        let using_fpr = state.using_fpr_offset();
        let error = self.new_error(state);
        self.push(AsmInst::AliasMethod {
            new,
            old,
            using_fpr,
        });
        self.handle_error(error);
    }

    pub(super) fn opt_case(
        &mut self,
        max: u16,
        min: u16,
        else_label: JitLabel,
        branch_labels: Box<[JitLabel]>,
    ) {
        self.push(AsmInst::OptCase {
            max,
            min,
            else_label,
            branch_labels,
        });
    }

    ///
    /// Handle error.
    ///
    /// Check *rax*, and if it is 0, go to 'error'.
    ///
    pub(crate) fn handle_error(&mut self, error: AsmError) {
        self.push(AsmInst::HandleError(error));
    }

    ///
    /// Guard that the object in *rdi* is not frozen.
    ///
    pub(crate) fn guard_frozen(&mut self, deopt: AsmDeopt) {
        self.push(AsmInst::GuardFrozen { deopt });
    }
}

//
// binary operations
//
impl AsmIr {
    ///
    /// Integer binary operation.
    ///
    /// ### in
    /// - rdi  lhs
    /// - rsi  rhs
    ///
    /// ### out
    /// - rdi  dst
    ///
    /// ### destroy
    /// - caller save registers
    /// - stack
    ///
    pub(super) fn integer_binop(
        &mut self,
        kind: BinOpK,
        lhs: GP,
        rhs: GP,
        mode: OpMode,
        deopt: AsmDeopt,
    ) {
        self.push(AsmInst::IntegerBinOp {
            kind,
            lhs,
            rhs,
            mode,
            deopt,
        });
    }

    /// §slot-IR: slot-based fixnum binop. Operands and destination are stack
    /// slots; the LIR lowering loads them into scratch registers, fixnum-guards,
    /// computes (overflow -> `deopt`), and stores the result back to `dst`. No
    /// physical register appears at the AsmIR level.
    pub(super) fn integer_binop_slot(
        &mut self,
        kind: BinOpK,
        dst: Option<SlotId>,
        mode: OpMode,
        deopt: AsmDeopt,
    ) {
        self.push(AsmInst::IntegerBinOpSlot {
            kind,
            dst,
            mode,
            deopt,
        });
    }

    ///
    /// Float binary operation
    ///
    /// ### in
    /// - depends on *mode*
    ///
    /// ### out
    /// - fpr(*dst*): dst
    ///
    /// ### destroy
    /// - caller save registers
    /// - stack
    ///
    pub(super) fn fpr_binop(&mut self, kind: BinOpK, lhs: FPReg, rhs: FPReg, dst: FPReg) {
        self.push(AsmInst::FloatBinOp {
            kind,
            binary_fpr: (lhs, rhs),
            dst,
        });
    }

    ///
    /// Integer comparison
    ///
    /// §slot-IR: slot-based fixnum comparison (see [`AsmInst::IntegerCmpSlot`]).
    pub(super) fn integer_cmp_slot(
        &mut self,
        mode: OpMode,
        kind: CmpKind,
        dst: Option<SlotId>,
        deopt: AsmDeopt,
    ) {
        self.push(AsmInst::IntegerCmpSlot {
            kind,
            mode,
            dst,
            deopt,
        });
    }

    /// §slot-IR: slot-based fused fixnum compare + branch (see
    /// [`AsmInst::IntegerCmpBrSlot`]).
    pub(super) fn integer_cmp_br_slot(
        &mut self,
        mode: OpMode,
        kind: CmpKind,
        brkind: BrKind,
        branch_dest: JitLabel,
        deopt: AsmDeopt,
    ) {
        self.push(AsmInst::IntegerCmpBrSlot {
            mode,
            kind,
            brkind,
            branch_dest,
            deopt,
        });
    }

    pub(super) fn float_cmp_br(
        &mut self,
        binary_fpr: (FPReg, FPReg),
        kind: CmpKind,
        brkind: BrKind,
        branch_dest: JitLabel,
    ) {
        self.push(AsmInst::FloatCmpBr {
            lhs: binary_fpr.0,
            rhs: binary_fpr.1,
            kind,
            brkind,
            branch_dest,
        });
    }
}

///
/// index operations
///

impl AsmIr {
    pub(super) fn new_array(&mut self, using_fpr: UsingFpr, callid: CallSiteId) {
        self.push(AsmInst::NewArray { callid, using_fpr });
    }

    pub(super) fn new_hash(&mut self, using_fpr: UsingFpr, args: SlotId, len: usize) {
        self.push(AsmInst::NewHash(args, len, using_fpr));
    }

    pub(super) fn hash_insert(&mut self, using_fpr: UsingFpr, hash: SlotId, args: SlotId, len: usize) {
        self.push(AsmInst::HashInsert {
            hash,
            args,
            len,
            using_fpr,
        });
    }

    pub(super) fn array_concat(&mut self, using_fpr: UsingFpr, dst: SlotId, src: SlotId) {
        self.push(AsmInst::ArrayConcat {
            dst,
            src,
            using_fpr,
        });
    }

    pub(super) fn new_range(
        &mut self,
        start: SlotId,
        end: SlotId,
        exclude_end: bool,
        using_fpr: UsingFpr,
        error: AsmError,
    ) {
        self.push(AsmInst::NewRange {
            start,
            end,
            exclude_end,
            using_fpr,
        });
        self.handle_error(error);
    }

    pub(super) fn block_arg_proxy(&mut self, ret: SlotId, outer: usize) {
        self.push(AsmInst::BlockArgProxy { ret, outer });
    }

    pub(crate) fn inline(
        &mut self,
        f: impl FnOnce(&mut Codegen, &Store, &SideExitLabels, usize) + 'static,
    ) {
        self.inst
            .push(AsmInst::Inline(InlineProcedure { proc: Box::new(f) }));
    }

    /// Emit `dst <- [base + disp]` (load a heap-object field into a GP register).
    /// A typed alternative to `inline` for trivial field-reader inline builtins,
    /// so their codegen flows through LIR (`LInst::Load { Field }`) instead of
    /// hand-written per-arch asm.
    pub(crate) fn load_field_to_reg(&mut self, dst: GP, base: GP, disp: i32) {
        self.inst.push(AsmInst::LoadFieldToReg { dst, base, disp });
    }

    /// Emit `dst <- bool([base + disp])`: load a 32-bit raw-bool field and
    /// convert it to a Ruby `true`/`false` `Value`. Typed alternative to
    /// `inline` for the `exclude_end?` field-reader inline builtins.
    pub(crate) fn bool_field_to_reg(&mut self, dst: GP, base: GP, disp: i32) {
        self.inst.push(AsmInst::BoolFieldToReg { dst, base, disp });
    }

    /// Emit `dst <- fixnum(Array#size)`: the fixnum-tagged length of the array
    /// receiver in `base`. Typed alternative to `inline` for `Array#size`.
    pub(crate) fn array_len_fixnum(&mut self, dst: GP, base: GP) {
        self.inst.push(AsmInst::ArrayLenFixnum { dst, base });
    }

    /// Emit `dst <- fixnum(String#bytesize)`: the fixnum-tagged byte length of
    /// the string receiver in `base`. Typed alternative to `inline`.
    pub(crate) fn string_len_fixnum(&mut self, dst: GP, base: GP) {
        self.inst.push(AsmInst::StringLenFixnum { dst, base });
    }

    /// Emit `dst <- (src == nil)` as a Ruby bool `Value` (`Object#nil?`). Typed
    /// alternative to `inline`; destroys the `GP::Rsi` scratch.
    pub(crate) fn is_nil_to_bool(&mut self, dst: GP, src: GP) {
        self.inst.push(AsmInst::IsNilToBool { dst, src });
    }

    /// Emit `dst <- !src` as a Ruby bool `Value` (`BasicObject#!`). Typed
    /// alternative to `inline`; destroys `src` and the `GP::Rsi` scratch.
    pub(crate) fn not_to_bool(&mut self, dst: GP, src: GP) {
        self.inst.push(AsmInst::NotToBool { dst, src });
    }

    /// Emit `Math.sqrt`: `fret <- sqrt(fsrc)`, deopting on a negative argument.
    /// Typed alternative to `inline`.
    pub(crate) fn math_sqrt(&mut self, fsrc: FPReg, fret: Option<FPReg>, deopt: AsmDeopt) {
        self.inst.push(AsmInst::MathSqrt { fsrc, fret, deopt });
    }

    /// Emit `Integer#succ`: `reg <- reg + 1` (tagged), deopting on overflow.
    /// Typed alternative to `inline`.
    pub(crate) fn integer_succ(&mut self, reg: GP, deopt: AsmDeopt) {
        self.inst.push(AsmInst::IntegerSucc { reg, deopt });
    }

    /// Emit `Kernel#block_given?`: `dst <- (a block was passed)`. Typed
    /// alternative to `inline`; destroys `GP::Rdi`.
    pub(crate) fn block_given(&mut self, dst: GP) {
        self.inst.push(AsmInst::BlockGiven { dst });
    }

    pub(crate) fn bc_index(&mut self, index: BcIndex) {
        self.push(AsmInst::BcIndex(index));
    }
}

///
/// Chain-based stack offset hint reused by every AsmInst that needs
/// "sum of frame sizes between frame A and frame B" — DynVar
/// access ([`AsmInst::LoadDynVarSpecialized`] /
/// [`AsmInst::StoreDynVarSpecialized`]) and specialized return /
/// break ([`AsmInst::MethodRetSpecialized`] /
/// [`AsmInst::BlockBreakSpecialized`]).
///
/// Pass 1 emits `Hint { ids, extra }`:
///
/// * `ids` — the chain of [`SpecializedId`]s. Resolved at the
///   pre-codegen pass into `sum(map[id])` — the frame *base* sizes.
/// * `extra` — bytes captured at AsmIr emission time that the
///   per-frame map cannot reproduce after pop. Currently this is
///   the cumulative `using_fpr.offset()` bump applied by every
///   active specialized call up the chain (each call temporarily
///   `+=`s its `using_fpr.offset()` to the caller frame's
///   `stack_offset`; we capture `current - base` for every frame in
///   the chain and sum the differences). Once
///   [`JitContext::specialized_compile`] stops doing the dynamic
///   `+=`/`-=` (e.g. when VirtFPReg-aware spill replaces
///   `using_fpr`), `extra` will be 0.
///
/// The pre-codegen resolve pass rewrites each `Hint` into
/// `Concrete(sum(map[id]) + extra)`. Code generation asserts
/// `Concrete(_)` and panics on a stray `Hint`.
///
#[derive(Debug, Clone)]
pub(crate) enum DynVarOffset {
    Hint {
        ids: Vec<super::context::SpecializedId>,
        extra: usize,
    },
    Concrete(usize),
}

impl DynVarOffset {
    pub(crate) fn unwrap_concrete(&self) -> usize {
        match self {
            DynVarOffset::Concrete(o) => *o,
            DynVarOffset::Hint { ids, extra } => panic!(
                "DynVarOffset::Hint(ids={:?}, extra={}) reached code generation — resolve pass did not run",
                ids, extra
            ),
        }
    }
}

///
/// Bytes the JIT prologue should subtract from `rsp` for a frame.
///
/// Pass 1 emits `Hint(id)` — the current frame's [`SpecializedId`].
/// The pre-codegen resolve pass derives the byte count from the
/// frame's recorded `stack_offset` once it is finalised (so that
/// future spill slots growing the frame size feed through to the
/// prologue subq automatically). Code generation asserts
/// `Concrete(_)`.
///
#[derive(Debug, Clone)]
pub(crate) enum PrologueOffset {
    Hint(super::context::SpecializedId),
    Concrete(usize),
}

impl PrologueOffset {
    pub(crate) fn unwrap_concrete(&self) -> usize {
        match self {
            PrologueOffset::Concrete(o) => *o,
            PrologueOffset::Hint(id) => panic!(
                "PrologueOffset::Hint({:?}) reached code generation — resolve pass did not run",
                id
            ),
        }
    }
}

///
/// Bytes the Loop JIT entry should subtract from `rsp` (and the
/// matching side-exit handler should add back). Only the bytes that
/// the JIT itself owns — the invoker / interpreter prologue is left
/// untouched, so this captures the JIT-managed spill slots that sit
/// on top of the surrounding interpreter frame.
///
/// Pass 1 emits `Hint(id)` — the current frame's [`SpecializedId`].
/// The pre-codegen resolve pass derives the byte count as
/// `stack_offset - base_stack_offset` of that frame, so any future
/// VirtFPReg spill space added to the frame's recorded size flows
/// through to the JIT-emitted bump automatically.
///
#[derive(Debug, Clone)]
pub(crate) enum LoopRspOffset {
    Hint(super::context::SpecializedId),
    Concrete(usize),
}

impl LoopRspOffset {
    pub(crate) fn unwrap_concrete(&self) -> usize {
        match self {
            LoopRspOffset::Concrete(o) => *o,
            LoopRspOffset::Hint(id) => panic!(
                "LoopRspOffset::Hint({:?}) reached code generation — resolve pass did not run",
                id
            ),
        }
    }
}

#[derive(Debug)]
pub(super) enum AsmInst {
    BcIndex(BcIndex),
    Label(JitLabel),
    Unreachable,
    /// move reg to stack
    RegToStack(GP, SlotId),
    /// move reg to the slot's home addressed via the **LFP (r14)**, not the
    /// native frame pointer (rbp). On x86 ordinary `RegToStack` writes
    /// `[rbp - rbp_local(slot)]`; that is the *stack* frame, which becomes
    /// stale after a callee moves this frame to the heap (`move_frame_to_heap`,
    /// e.g. a block captured into a Proc). The result of such a capturing call
    /// must instead land on whichever frame is live afterwards — the heap copy
    /// when captured, the stack frame otherwise — which is exactly what the LFP
    /// (reloaded from `cfp.lfp` after the call) points at. aarch64 already
    /// addresses every slot via the LFP, so there this is identical to
    /// `RegToStack`. See the capture-deopt analysis in `doc/arch_difference.md`.
    RegToLfpStack(GP, SlotId),

    /// move reg to stack
    StackToReg(SlotId, GP),
    LitToReg(Value, GP),

    /// move reg to reg
    RegMove(GP, GP),
    RegAdd(GP, i32),
    RegSub(GP, i32),
    /// movq [rsp + (ofs)], R(r);
    RegToRSPOffset(GP, i32),
    /// movq [rsp + (ofs)], 0;
    ZeroToRSPOffset(i32),
    /// movq [rsp + (ofs)], (i);
    U64ToRSPOffset(u64, i32),

    FprMove(FPReg, FPReg),
    FprSwap(FPReg, FPReg),
    FloatBinOp {
        kind: BinOpK,
        binary_fpr: (FPReg, FPReg),
        dst: FPReg,
    },
    FloatUnOp {
        kind: UnOpK,
        dst: FPReg,
    },

    ///
    /// Move f64 to fpr.
    ///
    F64ToFpr(f64, FPReg),
    ///
    /// Move *i*(i63) to the stack slot *reg* and VirtFPReg(*x*).
    ///
    I64ToBoth(i64, SlotId, FPReg),
    ///
    /// Generate convert code from VirtFPReg to Both.
    ///
    /// ### out
    /// - rax: Value
    ///
    /// ### destroy
    /// - rcx
    ///
    FprToStack(FPReg, SlotId),
    ///
    /// Move Value *v* to stack slot *reg*.
    ///
    /// ### destroy
    /// - rax
    ///
    LitToStack(Value, SlotId),
    ///
    /// Deep copy *v* and store it to `rax`.
    ///
    /// ### out
    /// - rax: Value
    ///
    /// ### destroy
    /// - caller save registers
    ///
    DeepCopyLit(Value, UsingFpr),
    /*///
    /// Convert Value to f64.
    ///
    /// go to *deopt* if *reg* was neither Float nor Fixnum(i63).
    ///
    /// ### in
    ///
    /// - R(*reg*): Value
    ///
    /// ### out
    ///
    /// - fpr(*fpr*)
    ///
    /// ### destroy
    ///
    /// - rax, rdi, R(*reg*)
    ///
    NumToFpr(GP, VirtFPReg, AsmDeopt),*/
    ///
    /// Convert Fixnum to f64.
    ///
    /// ### in
    /// - R(*reg*): Value
    ///
    /// ### out
    /// - fpr(*fpr*)
    ///
    /// ### destroy
    /// - R(*reg*)
    ///
    FixnumToFpr(GP, FPReg),
    ///
    /// Float guard and unboxing.
    ///
    /// Unbox a Float Value and return f64.
    ///
    /// If the input Value was not Float, go to *deopt*.
    ///
    /// ### in
    ///
    /// - R(*reg*): Value
    ///
    /// ### out
    ///
    /// - fpr(*fpr*)
    ///
    /// ### destroy
    ///
    /// - rax, rdi
    ///
    FloatToFpr(GP, FPReg, AsmDeopt),

    ///
    /// Class version guard for JIT.
    ///
    /// Check the cached class version.
    /// If different, jump to `deopt`.
    ///
    /// ### destroy
    /// - rax
    ///
    GuardClassVersion {
        position: Option<BytecodePtr>,
        with_recovery: bool,
        deopt: AsmDeopt,
    },
    GuardClassVersionSpecialized {
        idx: usize,
        deopt: AsmDeopt,
    },
    ///
    /// Type guard.
    ///
    /// Generate type guard for *class_id*.
    /// If the type was not matched, go to *deopt*.
    ///
    /// ### in
    /// - R(*reg*): Value
    ///
    GuardClass(GP, ClassId, AsmDeopt),
    GuardArrayTy(GP, AsmDeopt),
    GuardCapture(AsmDeopt),

    Ret,
    BlockBreak(BytecodePtr),
    MethodRet(BytecodePtr),
    BlockBreakSpecialized {
        /// Bytes between the current rbp and the iter caller's rbp.
        /// Emitted as `DynVarOffset::Hint(...)` and resolved before
        /// code generation — see [`DynVarOffset`].
        rbp_offset: DynVarOffset,
    },
    MethodRetSpecialized {
        /// Bytes between the current rbp and the method caller's
        /// rbp. Emitted as `DynVarOffset::Hint(...)` and resolved
        /// before code generation — see [`DynVarOffset`].
        rbp_offset: DynVarOffset,
    },
    Raise,
    Retry(BytecodePtr),
    Redo(BytecodePtr),
    EnsureEnd,
    ///
    /// Conditional branch
    ///
    /// When *kind* is BrKind::BrIf, jump to *dst* if the Value in `rax` is true.
    ///
    /// ### in
    /// - rax: Value
    ///
    CondBr(BrKind, JitLabel),
    NilBr(JitLabel),
    CheckLocal(JitLabel),
    CheckKwRest(SlotId),
    OptCase {
        max: u16,
        min: u16,
        else_label: JitLabel,
        branch_labels: Box<[JitLabel]>,
    },

    Preparation,
    ///
    /// Loop JIT entry rsp bump. Emits `subq rsp, X` where `X` is the
    /// JIT-managed spill region for this Loop frame (the existing
    /// invoker / interpreter prologue is left untouched). The
    /// matching `addq rsp, X` is emitted by `side_exit_with_label`
    /// at every deopt / loop-natural-exit handler, using the same
    /// resolved value (carried through `AsmInfo::loop_jit_spill_bytes`).
    ///
    LoopJitRspBump {
        offset: LoopRspOffset,
    },

    ///
    /// Initialize function frame.
    ///
    /// ### stack pointer adjustment
    /// - `prologue_offset` bytes (resolved before code generation)
    ///
    Init {
        info: FnInitInfo,
        ///
        /// Byte count for the prologue `subq rsp, _`. Emitted as
        /// `PrologueOffset::Hint(current_frame_id)` and rewritten
        /// to `PrologueOffset::Concrete(_)` by the resolve pass once
        /// the frame's final `stack_offset` is known. `info.stack_offset`
        /// is retained for non-JIT callers; the JIT codegen uses this
        /// field instead.
        ///
        prologue_offset: PrologueOffset,
    },
    ///
    /// Deoptimize and fallback to interpreter.
    ///
    Deopt(AsmDeopt),
    ///
    /// Deoptimize and after several times, recompile whole method or loop..
    ///
    RecompileDeopt {
        position: Option<BytecodePtr>,
        deopt: AsmDeopt,
        /// Error side-exit reached when the recompile itself raises (a Rust
        /// `panic!` during recompilation, caught at the `extern "C"` boundary
        /// and turned into a Ruby `FatalError`). `None` on x86, which
        /// recompiles in place and never returns an error here.
        error: Option<AsmError>,
        reason: RecompileReason,
    },
    RecompileDeoptSpecialized {
        idx: usize,
        deopt: AsmDeopt,
        reason: RecompileReason,
    },
    ///
    /// Handle error.
    ///
    /// Check *rax*, and if it is 0, go to 'error'.
    ///
    HandleError(AsmError),
    ///
    /// Save floating point registers in use.
    ///
    /// ### stack pointer adjustment
    /// - -`using_fpr`.offset()
    ///
    FprSave(UsingFpr, bool),
    ///
    /// Restore floating point registers in use.
    ///
    FprRestore(UsingFpr, bool),
    ///
    /// Execute GC.
    ///
    /// ### in
    /// - rbx: &mut Executor
    /// - r12: &mut Globals
    ///
    /// ### destroy
    /// - rax, rcx
    /// - stack
    ///
    ExecGc {
        write_back: WriteBack,
        error: AsmError,
    },
    ///
    /// Check stack overflow.
    ///
    /// ### in
    /// - rbx: &mut Executor
    ///
    CheckStack {
        write_back: WriteBack,
        error: AsmError,
    },
    ///
    /// Set self, req, opt and rest arguments on the callee frame.
    ///
    /// ### out
    /// - rax: None for error.
    ///
    /// ### destroy
    /// - caller save registers
    ///
    SetArguments {
        callid: CallSiteId,
        callee_fid: FuncId,
    },

    ///
    /// Specialized argument setup for a forwarding call `g(x.., ...)`.
    ///
    /// `f`'s `...` rest is already an `Array` at `args + lead_num`, with
    /// `lead_num` ordinary leading args at `args .. args+lead_num`. When
    /// the runtime array length equals `g`'s required arity minus
    /// `lead_num` (a compile-time constant), copy the leading args and
    /// then the array elements straight into the callee frame with no
    /// `Array` re-parse. Any guard miss (`args+lead_num` not an `Array`,
    /// length mismatch, or a non-nil forwarded kw-rest) is a clean
    /// side-exit to the generic `jit_generic_set_arguments` path *before*
    /// any callee-frame write, so no rollback is needed.
    ///
    /// ### out
    /// - rax: None for error.
    ///
    /// ### destroy
    /// - caller save registers
    ///
    SetArgumentsForwarded {
        callid: CallSiteId,
        callee_fid: FuncId,
        recv: SlotId,
        args: SlotId,
        lead_num: usize,
        kwrest_guard: Option<SlotId>,
        /// D1: when `Some((src, len))` the trampoline's `...` rest array
        /// was deferred — copy the `len` forwarded positionals straight
        /// from the caller source slots `src..src+len` (read via the
        /// caller `rbp` saved at `[rbp]`) instead of loading/length-
        /// guarding `f`'s rest `Array`. No fallback (the structural gate
        /// guarantees exact arity and a nil forwarded `**kwrest`).
        deferred_src: Option<(SlotId, u16)>,
    },

    ///
    /// Argument setup for a forwarding call `g(x.., ...)` whose callee
    /// is a `no_keyword` iseq with opt/post/rest (rest-array allocation
    /// is unavoidable, so handled by the specialized runtime helper
    /// `jit_forwarded_set_arguments` rather than inline asm). Same asm
    /// shape as `SetArguments`, different call target.
    ///
    /// ### out
    /// - rax: None for error.
    ///
    /// ### destroy
    /// - caller save registers
    ///
    SetArgumentsForwardedHelper {
        callid: CallSiteId,
        callee_fid: FuncId,
    },

    ///
    /// Set up a callee method frame for send.
    ///
    /// ### destroy
    /// - rax
    ///
    SetupMethodFrame {
        meta: Meta,
        callid: CallSiteId,
        outer_lfp: Option<Lfp>,
    },
    ///
    /// Set up a callee block frame for yield.
    ///
    /// ### destroy
    /// - rax, rdi
    ///
    SetupYieldFrame {
        meta: Meta,
        outer: usize,
    },

    ///
    /// Call method
    ///
    /// ### in
    /// - r13: receiver: Value.
    ///
    /// ### destroy
    /// - caller save registers
    ///
    Call {
        callee_fid: FuncId,
        recv_class: ClassId,
        evict: AsmEvict,
        pc: BytecodePtr,
    },
    ///
    /// Call specialized method
    ///
    /// ### in
    /// - rdi: receiver: Value
    ///
    /// ### destroy
    /// - caller save registers
    ///
    SpecializedCall {
        entry: JitLabel,
        patch_point: Option<JitLabel>,
        evict: AsmEvict,
    },
    Yield {
        callid: CallSiteId,
        error: AsmError,
        evict: AsmEvict,
    },
    SpecializedYield {
        entry: JitLabel,
        evict: AsmEvict,
    },
    Inline(InlineProcedure),
    /// §20 (B): array integer-index **read** (`ary[idx]`), a typed data record
    /// replacing the `ir.inline(|gen| …)` closure escape hatch — so `AsmInst`
    /// carries no `FnOnce` and becomes `Clone` (the prerequisite for the unified,
    /// shadow-checkable record stream). The per-arch index-register setup +
    /// `array_index` call is dispatched in `gen_array_index`.
    ArrayIndex {
        kind: ArrayIndexKind,
    },
    /// §20 (B): array integer-index **assign** (`ary[idx] = src`). Typed twin of
    /// `ArrayIndex`; carries the `using_fpr` save-set and the generic-path error
    /// side-exit. Dispatched in `gen_array_index_assign`.
    ArrayIndexAssign {
        kind: ArrayIndexKind,
        using_fpr: UsingFpr,
        error: AsmError,
    },
    /// `dst <- [base + disp]`: load a field of a heap object into a GP register.
    /// A typed replacement for the `ir.inline(|gen| gen.emit_*())` escape hatch
    /// used by trivial field-reader inline builtins (e.g. `Range#begin/end`),
    /// so their codegen is expressed once in arch-neutral LIR rather than as
    /// per-arch hand-written asm. Lowers to `LInst::Load { mem: Field }`.
    LoadFieldToReg {
        dst: GP,
        base: GP,
        disp: i32,
    },
    /// `dst <- bool([base + disp])`: load a 32-bit raw-bool field of a heap
    /// object and convert it to a Ruby `true`/`false` `Value` (`(b << 3) |
    /// FALSE_VALUE`). A typed replacement for the `emit_*_exclude_end` closures
    /// (`Range#exclude_end?`, `ArithmeticSequence#exclude_end?`), which were
    /// byte-identical bar the offset. Lowers to `LInst::BoolFieldToReg`.
    BoolFieldToReg {
        dst: GP,
        base: GP,
        disp: i32,
    },
    /// `dst <- fixnum(Array#size)`: the fixnum-tagged length of the array
    /// receiver in `base` (inline `capa`, or the heap length when `capa` exceeds
    /// `ARRAY_INLINE_CAPA`), `(n << 1) | 1`. Typed replacement for the
    /// `emit_array_size` closure. Lowers to `LInst::ArrayLenFixnum`.
    ArrayLenFixnum {
        dst: GP,
        base: GP,
    },
    /// `dst <- fixnum(String#bytesize)`: as `ArrayLenFixnum` but for a string
    /// receiver (inline threshold `STRING_INLINE_CAP`). Typed replacement for the
    /// `emit_string_bytesize` closure. Lowers to `LInst::StringLenFixnum`.
    StringLenFixnum {
        dst: GP,
        base: GP,
    },
    /// `dst <- (src == nil) ? true : false` as a Ruby bool `Value` (`Object#nil?`).
    /// Typed replacement for the `emit_kernel_nil` closure. Uses `GP::Rsi` as a
    /// scratch and destroys it; `dst`/`src` must not be `Rsi`. Lowers to
    /// `LInst::IsNilToBool`.
    IsNilToBool {
        dst: GP,
        src: GP,
    },
    /// `dst <- (!src) ? true : false` as a Ruby bool `Value` (`BasicObject#!`):
    /// `true` when `src` is nil/false, else `false`. Typed replacement for the
    /// `emit_object_not` closure. Destroys `src` and the `GP::Rsi` scratch.
    /// Lowers to `LInst::NotToBool`.
    NotToBool {
        dst: GP,
        src: GP,
    },
    /// `Math.sqrt`: `fret <- sqrt(fsrc)`, guarding the domain. NaN passes through
    /// (`sqrt(NaN) = NaN`); a negative argument branches to `deopt` (the
    /// interpreter re-runs and raises `Math::DomainError`); `-0.0` yields `-0.0`.
    /// Typed replacement for the `emit_math_sqrt` closure. Carries the deopt as an
    /// `AsmDeopt` (resolved to a label by the dispatcher, like `GuardClass`).
    MathSqrt {
        fsrc: FPReg,
        fret: Option<FPReg>,
        deopt: AsmDeopt,
    },
    /// `Integer#succ`: `reg <- reg + 1` on the tagged fixnum in `reg` (`+2` in
    /// tagged form), branching to `deopt` on signed overflow (the interpreter
    /// re-runs and promotes to Bignum). Typed replacement for `emit_integer_succ`.
    IntegerSucc {
        reg: GP,
        deopt: AsmDeopt,
    },
    /// `Kernel#block_given?`: `dst <- (block slot is set and non-nil)` as a Ruby
    /// bool `Value`. Reads `[LFP - LFP_BLOCK]`; destroys `GP::Rdi`. Typed
    /// replacement for the `emit_block_given` closure. Lowers to `LInst::BlockGiven`.
    BlockGiven {
        dst: GP,
    },
    #[allow(non_camel_case_types)]
    CFunc_F_F {
        f: unsafe extern "C" fn(f64) -> f64,
        src: FPReg,
        dst: FPReg,
        using_fpr: UsingFpr,
    },
    #[allow(non_camel_case_types)]
    CFunc_FF_F {
        f: extern "C" fn(f64, f64) -> f64,
        lhs: FPReg,
        rhs: FPReg,
        dst: FPReg,
        using_fpr: UsingFpr,
    },
    ///
    /// Imnmediate eviction.
    ///
    /// When BOPs are re-defined, this palce will be overwritten by the code causes deoptimization.
    ///
    ImmediateEvict {
        evict: AsmEvict,
    },
    CheckBOP {
        deopt: AsmDeopt,
    },

    FixnumNeg {
        reg: GP,
        deopt: AsmDeopt,
    },
    FixnumBitNot {
        reg: GP,
    },

    ///
    /// Integer binary operation.
    ///
    /// ### in
    /// - rdi  lhs
    /// - rsi  rhs
    ///
    /// ### out
    /// - rdi  dst
    ///
    /// ### destroy
    /// - caller save registers
    /// - stack
    ///
    IntegerBinOp {
        kind: BinOpK,
        lhs: GP,
        rhs: GP,
        mode: OpMode,
        deopt: AsmDeopt,
    },
    ///
    /// §slot-IR: slot-based fixnum binop (`dst = lhs <kind> rhs`, all stack
    /// slots). The LIR lowering materializes the physical registers — load each
    /// operand slot into a scratch reg, fixnum-guard it, compute in place
    /// (overflow -> `deopt`), and store the result to `dst`. The AsmIR layer
    /// carries no GP register for this op (the slot-IR migration).
    ///
    IntegerBinOpSlot {
        kind: BinOpK,
        dst: Option<SlotId>,
        mode: OpMode,
        deopt: AsmDeopt,
    },
    ///
    ///
    /// §slot-IR: slot-based fixnum comparison (`dst = lhs <kind> rhs` as a bool
    /// `Value`, all stack slots). The LIR lowering loads each operand slot into a
    /// scratch reg, fixnum-guards it (`deopt`), compares, and stores the boolean
    /// result to `dst` (when present). No GP register at the AsmIR layer.
    ///
    IntegerCmpSlot {
        mode: OpMode,
        kind: CmpKind,
        dst: Option<SlotId>,
        deopt: AsmDeopt,
    },
    ///
    /// §slot-IR: slot-based fused fixnum compare + conditional branch. The LIR
    /// lowering loads each operand slot into a scratch reg, fixnum-guards it
    /// (`deopt`), compares, and branches to `branch_dest` per `kind`/`brkind`.
    /// No GP register at the AsmIR layer.
    ///
    IntegerCmpBrSlot {
        mode: OpMode,
        kind: CmpKind,
        brkind: BrKind,
        branch_dest: JitLabel,
        deopt: AsmDeopt,
    },
    FloatCmp {
        kind: CmpKind,
        lhs: FPReg,
        rhs: FPReg,
    },
    FloatCmpBr {
        kind: CmpKind,
        lhs: FPReg,
        rhs: FPReg,
        brkind: BrKind,
        branch_dest: JitLabel,
    },
    ///
    /// Generic binary operation through a `BinaryOpFn` C helper.
    ///
    /// Calls `func(vm, globals, lhs, rhs) -> Option<Value>`; the
    /// result (or 0 = error) is left in rax. Emits **no**
    /// receiver-class guard, so the site never deopts on receiver
    /// class variance — used for polymorphic BinCmp/BinOp.
    ///
    /// ### out
    /// - rax: result `Option<Value>`
    ///
    /// ### destroy
    /// - caller save registers
    ///
    GenericBinOp {
        lhs: SlotId,
        rhs: SlotId,
        func: crate::executor::BinaryOpFn,
        using_fpr: UsingFpr,
    },

    ///
    /// `==` / `!=` with an inline immediate fast path (YJIT
    /// `opt_eq` style), generic `cmp_*_values` C-call fallback.
    /// *kind* is `CmpKind::Eq` or `CmpKind::Ne`. Result `Value`
    /// (fast path) or `Option<Value>` (slow path) in rax.
    ///
    OptEqCmp {
        lhs: SlotId,
        rhs: SlotId,
        kind: CmpKind,
        func: crate::executor::BinaryOpFn,
        using_fpr: UsingFpr,
    },

    ///
    /// Compare `lhs and `rhs` with "===" and return the result in rax.
    ///
    /// If `lhs` is Array, compare `rhs` and each element of `lhs`.
    ///
    ArrayTEq {
        lhs: SlotId,
        rhs: SlotId,
        using_fpr: UsingFpr,
    },

    ///
    /// Guard for the base class of the constant.
    ///
    /// ### in
    /// - rax: Class
    ///
    GuardConstBaseClass {
        base_class: Value,
        deopt: AsmDeopt,
    },
    ///
    /// Guard for constant version.
    ///
    /// ### destroy
    /// - rax
    ///
    GuardConstVersion {
        const_version: usize,
        deopt: AsmDeopt,
    },
    StoreConstant {
        id: ConstSiteId,
        using_fpr: UsingFpr,
        error: AsmError,
    },

    ///
    /// Generate new Array object according to `callid`.
    ///
    /// ### out
    ///
    /// - rax: result Option<Value>
    ///
    /// ### destroy
    ///
    /// - caller save registers
    ///
    NewArray {
        callid: CallSiteId,
        using_fpr: UsingFpr,
    },
    ///
    /// Create a new Hash object and store it to *rax*
    ///
    NewHash(SlotId, usize, UsingFpr),
    ///
    /// Insert `len` key/value pairs at `args` into the Hash in `hash`
    /// (chunked Hash literal); the hash is returned in *rax*.
    ///
    HashInsert {
        hash: SlotId,
        args: SlotId,
        len: usize,
        using_fpr: UsingFpr,
    },
    ///
    /// Concatenate the Array in `src` onto the Array in `dst` (chunked
    /// Array literal); dst is returned in *rax*.
    ///
    ArrayConcat {
        dst: SlotId,
        src: SlotId,
        using_fpr: UsingFpr,
    },
    ///
    /// Create a new Range object and store it to *rax*
    ///
    NewRange {
        start: SlotId,
        end: SlotId,
        exclude_end: bool,
        using_fpr: UsingFpr,
    },
    ToA {
        src: SlotId,
        using_fpr: UsingFpr,
    },
    ConcatStr {
        arg: SlotId,
        len: u16,
        using_fpr: UsingFpr,
    },
    ConcatRegexp {
        arg: SlotId,
        len: u16,
        using_fpr: UsingFpr,
    },

    BlockArgProxy {
        ret: SlotId,
        outer: usize,
    },
    BlockArg {
        ret: SlotId,
        _outer: usize,
        using_fpr: UsingFpr,
        error: AsmError,
        call_site_bc_ptr: BytecodePtr,
    },

    /// Load instance var *ivarid* of the object *rdi* into register *rax*.
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    /// #### out
    /// - r15: Value
    ///
    /// #### destroy
    /// - rdi, rsi
    ///
    LoadIVarHeap {
        ivarid: IvarId,
        is_object_ty: bool,
        self_: bool,
    },
    ///
    /// Load ivar embedded to RValue. (only for object type)
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    /// #### out
    /// - r15: Value
    ///
    /// #### destroy
    /// - rdi
    ///
    LoadIVarInline {
        ivarid: IvarId,
    },
    ///
    /// Store *src* in an instance var *ivarid* of the object *rdi*.
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    /// #### destroy
    /// - caller-save registers
    ///
    StoreIVarHeap {
        src: GP,
        ivarid: IvarId,
        is_object_ty: bool,
        using_fpr: UsingFpr,
    },
    ///
    /// Store *src* in an instance var *ivarid* of the object *rdi*.
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    /// #### destroy
    /// - rdx
    ///
    StoreSelfIVarHeap {
        src: GP,
        ivarid: IvarId,
        is_object_ty: bool,
    },
    ///
    /// Store *src* in ivar embedded to RValue `rdi`. (only for object type)
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    StoreIVarInline {
        src: GP,
        ivarid: IvarId,
    },
    ///
    /// Load slot `slot_index` of a `Struct` subclass instance whose
    /// slot vector is INLINE in the RValue's `kind` union (i.e. the
    /// class has at most `STRUCT_INLINE_SLOTS` members). Single mov.
    ///
    /// #### in
    /// - rdi: receiver (a Value pointing at an `ObjTy::STRUCT` RValue)
    ///
    /// #### out
    /// - r15: Value at slot `slot_index`
    ///
    LoadStructSlotInline {
        slot_index: u16,
    },
    ///
    /// Load slot `slot_index` of a `Struct` subclass instance whose
    /// slot vector is on the HEAP (`> STRUCT_INLINE_SLOTS` members).
    /// Two movs: heap-pointer deref + slot index.
    ///
    /// #### in
    /// - rdi: receiver
    /// #### out
    /// - r15: Value
    /// #### destroy
    /// - rdi
    ///
    LoadStructSlotHeap {
        slot_index: u16,
    },
    ///
    /// Store *src* into the inline slot `slot_index` of the `Struct`
    /// subclass instance `rdi`. Caller must have emitted `GuardFrozen`.
    ///
    /// #### in
    /// - rdi: receiver
    /// - src: Value to store
    /// #### out
    /// - rax: src (return value of the writer)
    ///
    StoreStructSlotInline {
        src: GP,
        slot_index: u16,
    },
    ///
    /// Store *src* into the heap-allocated slot `slot_index` of the
    /// `Struct` subclass instance `rdi`. Caller must have emitted
    /// `GuardFrozen`.
    ///
    /// #### in
    /// - rdi: receiver
    /// - src: Value to store
    /// #### out
    /// - rax: src
    /// #### destroy
    /// - rdi
    ///
    StoreStructSlotHeap {
        src: GP,
        slot_index: u16,
    },
    ///
    /// Guard that the object in *rdi* is not frozen.
    /// If frozen, deoptimize to interpreter (which will raise FrozenError).
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    GuardFrozen {
        deopt: AsmDeopt,
    },

    /// rax = DynVar(src)
    LoadDynVar {
        src: DynVar,
    },
    /// rax = DynVar(src)
    LoadDynVarSpecialized {
        /// Machine stack offset in bytes. Emitted as a
        /// `DynVarOffset::Hint(...)` chain by Pass 1; the pre-codegen
        /// resolve pass replaces it with `DynVarOffset::Concrete(_)`
        /// once every frame's final size is known.
        offset: DynVarOffset,
        reg: SlotId,
    },
    /// DynVar(dst) = src
    StoreDynVar {
        dst: DynVar,
        src: GP,
    },
    /// DynVar(dst) = src
    StoreDynVarSpecialized {
        /// Machine stack offset in bytes — see
        /// [`AsmInst::LoadDynVarSpecialized`].
        offset: DynVarOffset,
        dst: SlotId,
        src: GP,
    },
    LoadCVar {
        name: IdentId,
        using_fpr: UsingFpr,
    },
    CheckCVar {
        name: IdentId,
        using_fpr: UsingFpr,
    },
    StoreCVar {
        name: IdentId,
        src: SlotId,
        using_fpr: UsingFpr,
    },
    LoadGVar {
        name: IdentId,
        using_fpr: UsingFpr,
    },
    StoreGVar {
        name: IdentId,
        src: SlotId,
        using_fpr: UsingFpr,
    },

    ClassDef {
        base: Option<SlotId>,
        superclass: Option<SlotId>,
        dst: Option<SlotId>,
        name: IdentId,
        func_id: FuncId,
        is_module: bool,
        using_fpr: UsingFpr,
        error: AsmError,
    },
    SingletonClassDef {
        base: SlotId,
        dst: Option<SlotId>,
        func_id: FuncId,
        using_fpr: UsingFpr,
        error: AsmError,
    },
    MethodDef {
        name: IdentId,
        func_id: FuncId,
        using_fpr: UsingFpr,
        error: AsmError,
    },
    SingletonMethodDef {
        obj: SlotId,
        name: IdentId,
        func_id: FuncId,
        using_fpr: UsingFpr,
        error: AsmError,
    },

    ExpandArray {
        dst: SlotId,
        len: usize,
        rest_pos: Option<usize>,
        using_fpr: UsingFpr,
    },
    CreateArray {
        src: SlotId,
        len: usize,
    },
    RestKw {
        rest_kw: Vec<(SlotId, IdentId)>,
    },

    UndefMethod {
        undef: IdentId,
        using_fpr: UsingFpr,
    },
    AliasMethod {
        new: SlotId,
        old: SlotId,
        using_fpr: UsingFpr,
    },
    AliasGvar {
        new: IdentId,
        old: IdentId,
        using_fpr: UsingFpr,
    },
    ///
    /// Check if `yield` is callable.
    ///
    /// Set `dst` to "yield" if callable, `nil` if not.
    ///
    DefinedYield {
        dst: SlotId,
        using_fpr: UsingFpr,
    },
    DefinedConst {
        dst: SlotId,
        siteid: ConstSiteId,
        using_fpr: UsingFpr,
    },
    DefinedMethod {
        dst: SlotId,
        recv: SlotId,
        name: IdentId,
        using_fpr: UsingFpr,
    },
    ///
    /// Check if `super` is callable.
    ///
    /// Set `dst` to "super" if callable, `nil` if not.
    ///
    DefinedSuper {
        dst: SlotId,
        using_fpr: UsingFpr,
    },
    ///
    /// Check if global var `name` exists.
    ///
    /// Set `dst`` to "global-variable" if exists, `nil` if not.
    ///
    DefinedGvar {
        dst: SlotId,
        name: IdentId,
        using_fpr: UsingFpr,
    },
    DefinedIvar {
        dst: SlotId,
        name: IdentId,
        using_fpr: UsingFpr,
    },
    DefinedCvar {
        dst: SlotId,
        name: IdentId,
        using_fpr: UsingFpr,
    },
}

impl AsmInst {
    ///
    /// Does this instruction unconditionally transfer control away, so that
    /// the instruction physically following it is *not* reached by
    /// fall-through?
    ///
    /// Conservative: returns `true` only for variants that are definitely
    /// unconditional terminators. Anything uncertain returns `false` (the
    /// caller then assumes the next block *is* fall-through reachable). A
    /// false negative only keeps a harmless `b skip` over the aarch64
    /// inline side-exit handlers; a false positive would drop a *needed*
    /// `b skip` and let control fall into the cold handlers, so the bias
    /// must stay on the safe side.
    ///
    pub(super) fn is_unconditional_terminator(&self) -> bool {
        matches!(
            self,
            Self::Ret
                | Self::BlockBreak(_)
                | Self::MethodRet(_)
                | Self::BlockBreakSpecialized { .. }
                | Self::MethodRetSpecialized { .. }
                | Self::Raise
                | Self::Retry(_)
                | Self::Redo(_)
                | Self::Deopt(_)
                | Self::RecompileDeopt { .. }
                | Self::RecompileDeoptSpecialized { .. }
        )
    }

    ///
    /// Is this a register-to-itself move (`RegMove(r, r)` / `FprMove(r, r)`,
    /// i.e. `dst == src`)? Such a move emits a no-op (`mov r, r` does not even
    /// touch the flags; `movapd x, x` is likewise inert), so the §21 peephole
    /// drops it. Both variants store their operands as `(src, dst)` — see the
    /// `Debug` formatters that render them as `dst = src`.
    ///
    fn is_self_move(&self) -> bool {
        match self {
            Self::RegMove(src, dst) => src == dst,
            Self::FprMove(src, dst) => src == dst,
            _ => false,
        }
    }

    ///
    /// Enumerate every `VirtFPReg` operand referenced by this
    /// instruction (in any role — read, write, or read-write).
    /// Used by `pop_frame` to compute the frame's max
    /// `VirtFPReg` id (and thus the stack-spill region size) and
    /// — once Phase 2's codegen-side spill expansion lands — to
    /// detect operands that need swap-load-swap-store.
    ///
    pub(super) fn fpr_operands(&self) -> Vec<FPReg> {
        match self {
            Self::FprMove(a, b) | Self::FprSwap(a, b) => vec![*a, *b],
            Self::FloatBinOp {
                binary_fpr: (l, r),
                dst,
                ..
            } => vec![*l, *r, *dst],
            Self::FloatUnOp { dst, .. } => vec![*dst],
            Self::F64ToFpr(_, x) => vec![*x],
            Self::I64ToBoth(_, _, x) => vec![*x],
            Self::FprToStack(x, _) => vec![*x],
            Self::FixnumToFpr(_, x) => vec![*x],
            Self::FloatToFpr(_, x, _) => vec![*x],
            Self::CFunc_F_F { src, dst, .. } => vec![*src, *dst],
            Self::CFunc_FF_F { lhs, rhs, dst, .. } => vec![*lhs, *rhs, *dst],
            Self::FloatCmp { lhs, rhs, .. } => vec![*lhs, *rhs],
            Self::FloatCmpBr { lhs, rhs, .. } => vec![*lhs, *rhs],
            _ => vec![],
        }
    }

    #[allow(dead_code)]
    #[cfg(feature = "emit-asm")]
    #[allow(dead_code)]
    pub fn dump(&self, store: &Store) -> String {
        match self {
            Self::RegToStack(gpr, slot) => format!("{:?} = {:?}", slot, gpr),
            Self::StackToReg(slot, gpr) => format!("{:?} = {:?}", gpr, slot),
            Self::LitToReg(val, gpr) => format!("{:?} = {}", gpr, val.debug(store)),
            Self::RegMove(src, dst) => format!("{:?} = {:?}", dst, src),
            Self::RegAdd(gpr, i) => format!("{:?} += {i}", gpr),
            Self::RegSub(gpr, i) => format!("{:?} -= {i}", gpr,),
            Self::RegToRSPOffset(gpr, offset) => format!("RSP[{offset}] = {:?}", gpr),
            Self::FprMove(src, dst) => format!("{:?} = {:?}", dst, src),
            Self::FprSwap(fp1, fp2) => format!("{:?} <-> {:?}", fp1, fp2),
            Self::FloatBinOp {
                kind,
                binary_fpr,
                dst,
            } => format!(
                "{:?} = {:?} {:?} {:?}",
                dst, binary_fpr.0, kind, binary_fpr.1
            ),
            Self::FloatUnOp { kind, dst } => format!("{:?} = {:?} {:?}", dst, kind, dst),

            Self::F64ToFpr(f, dst) => format!("{:?} = {}", dst, f),
            Self::I64ToBoth(i, slot, fpr) => format!("{:?}:{:?} = {i}", slot, fpr),
            Self::FprToStack(fpr, slots) => format!("{:?} = {:?}", slots, fpr),
            Self::LitToStack(val, slot) => format!("{:?} = {}", slot, val.debug(store)),
            Self::DeepCopyLit(val, _using_fpr) => format!("DeepCopyLiteral {}", val.debug(store)),
            Self::FloatToFpr(gpr, fpr, _deopt) => format!("{:?} = {:?} Float to f64", fpr, gpr),
            Self::GuardClassVersion {
                position: _,
                with_recovery: _,
                deopt: _,
            } => {
                format!("GuardClassVersion")
            }
            Self::GuardClass(gpr, class, _deopt) => format!("GuardClass {:?} {:?}", class, gpr),
            Self::GuardCapture(_deopt) => format!("Guard Capture"),

            Self::CondBr(kind, label) => format!("condbr {:?} {:?}", kind, label),
            Self::NilBr(label) => format!("nil_br {:?}", label),
            Self::CheckLocal(label) => format!("check_local {:?}", label),
            Self::OptCase {
                max,
                min,
                else_label,
                branch_labels,
            } => format!(
                "opt_case {:?}..{:?} {:?} else {:?}",
                min, max, branch_labels, else_label
            ),
            Self::Deopt(deopt) => format!("deopt {:?}", deopt),
            Self::RecompileDeopt {
                position,
                deopt: _,
                error: _,
                reason,
            } => {
                format!("recompile_deopt {:?} {:?}", position, reason)
            }
            Self::HandleError(error) => format!("handle_error {:?}", error),
            Self::FprSave(using_fpr, cont) => format!("fpr_save {:?} {cont}", using_fpr),
            Self::FprRestore(using_fpr, cont) => format!("fpr_restore {:?} {cont}", using_fpr),
            Self::ExecGc {
                write_back,
                error: _,
            } => format!("exec_gc {:?}", write_back),
            _ => format!("{:?}", self),
        }
    }
}

#[derive(Debug)]
pub enum SideExit {
    Evict(Option<(BytecodePtr, WriteBack)>),
    Deoptimize(BytecodePtr, WriteBack),
    ///
    /// A deopt that, after a small number of misses, recompiles the
    /// whole method/loop with the given reason instead of falling
    /// back to the interpreter forever. Used as the
    /// receiver-class-guard miss target for monomorphic-compiled
    /// BinCmp sites so they flip to the non-deopting polymorphic
    /// path once the VM has observed class variance (Part B).
    ///
    RecompileDeoptimize(BytecodePtr, WriteBack, RecompileReason, Option<BytecodePtr>),
    Error(BytecodePtr, WriteBack),
}

#[cfg(target_arch = "x86_64")]
impl Codegen {
    ///
    /// Generate machine code for *ir*.
    ///
    pub(super) fn gen_asm(
        &mut self,
        ir: AsmIr,
        store: &Store,
        frame: &mut AsmInfo,
        entry: Option<DestLabel>,
        exit: Option<BasicBlockId>,
        class_version: DestLabel,
        // Whether this block is fall-through reachable. Only the aarch64 backend
        // acts on it (to drop a dead `b skip` over inline side-exit handlers);
        // x86 lays its handlers on the cold page via `select_page`, so there is
        // no skip branch to elide and the flag is unused here.
        _fallthrough_in: bool,
    ) {
        let mut side_exits = SideExitLabels::new();
        let mut deopt_table: HashMap<(BytecodePtr, WriteBack), DestLabel> = HashMap::default();
        let loop_jit_spill_bytes = frame.loop_jit_spill_bytes;
        let base = frame.base_stack_offset;
        // §9 (9a, first brick): reify the isolated side-exit handler block into a
        // whole-region `Lir` buffer, then drain it through `encode_linst` below
        // instead of emitting each handler inline. Byte-identical (immediate
        // drain, no allocation pass yet); the labels are still created eagerly so
        // the main body can reference them. This is the seam the future
        // physical-allocation pass slots into (between buffering and the drain).
        let mut lir = Lir::new();
        for side_exit in ir.side_exit {
            let label = match side_exit {
                SideExit::Evict(Some((pc, wb))) => {
                    let label = self.jit.label();
                    lir.push(LInst::SideExit {
                        kind: LSideExitKind::Evict,
                        pc,
                        wb,
                        entry: label.clone(),
                        loop_jit_spill_bytes,
                        base,
                    });
                    label
                }
                SideExit::Deoptimize(pc, wb) => {
                    let t = (pc, wb);
                    if let Some(label) = deopt_table.get(&t) {
                        label.clone()
                    } else {
                        let label = self.jit.label();
                        lir.push(LInst::SideExit {
                            kind: LSideExitKind::Deopt,
                            pc: t.0,
                            wb: t.1.clone(),
                            entry: label.clone(),
                            loop_jit_spill_bytes,
                            base,
                        });
                        deopt_table.insert(t, label.clone());
                        label
                    }
                }
                SideExit::RecompileDeoptimize(pc, wb, reason, position) => {
                    let label = self.jit.label();
                    lir.push(LInst::SideExit {
                        kind: LSideExitKind::RecompileDeopt { reason, position },
                        pc,
                        wb,
                        entry: label.clone(),
                        loop_jit_spill_bytes,
                        base,
                    });
                    label
                }
                SideExit::Error(pc, wb) => {
                    let label = self.jit.label();
                    lir.push(LInst::SideExit {
                        kind: LSideExitKind::Error,
                        pc,
                        wb,
                        entry: label.clone(),
                        loop_jit_spill_bytes,
                        base,
                    });
                    label
                }
                _ => unreachable!("unexpected {side_exit:?}"),
            };
            side_exits.push(label);
        }
        for inst in lir.into_insts() {
            self.encode_linst(inst);
        }

        if entry.is_some() && exit.is_some() {
            self.encode_linst(LInst::SelectPage(1));
        }

        if let Some(entry) = &entry {
            self.encode_linst(LInst::BindLabel(entry.clone()));
        }

        // (§9a-ii) Lower the whole body into one ordered `Vec<LInst>` (the
        // `encode_linst*` + per-arch-fallthrough buffer-pass guards collect
        // instead of emitting), then drain it. The buffer is the seam the future
        // GP physical-allocation pass slots between these two loops; today it
        // drains immediately, so the output is byte-identical.
        self.lir_buf = Some(Vec::new());
        for inst in ir.inst {
            #[cfg(feature = "emit-asm")]
            {
                //eprintln!("  ; {}", inst.dump(store));
            }
            self.compile_asmir(store, frame, &side_exits, inst, class_version.clone());
        }
        let body = self.lir_buf.take().unwrap();
        // (§9d) GP physical-allocation pass seam — identity today (byte-identical).
        let body = self.allocate_gp(body);
        for inst in body {
            match inst {
                LInst::Inline(_) => {
                    self.encode_linst_inline(inst, store, &side_exits, frame.base_stack_offset)
                }
                LInst::SourcePos { .. } => self.encode_linst_frame(inst, frame),
                LInst::DeferredArch {
                    inst,
                    class_version,
                } => {
                    self.compile_asmir_arch(store, frame, &side_exits, inst, class_version);
                }
                _ => self.encode_linst(inst),
            }
        }

        if let Some(exit) = exit {
            let exit = frame.resolve_bb_label(&mut self.jit, exit);
            monoasm! { &mut self.jit,
                jmp exit;
            }
        }
        if entry.is_some() && exit.is_some() {
            self.encode_linst(LInst::SelectPage(0));
        }
    }

    ///
    /// Handle error.
    ///
    /// Check *rax*, and if it is 0, go to 'error'.
    ///
    pub(crate) fn handle_error(&mut self, error: &DestLabel) {
        monoasm! { &mut self.jit,
            testq rax, rax;
            jeq   error;
        }
    }
}
