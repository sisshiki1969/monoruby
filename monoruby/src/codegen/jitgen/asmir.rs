use crate::bytecodegen::BinOpK;

use super::*;

mod compile;

pub(super) struct InlineProcedure {
    proc: Box<dyn FnOnce(&mut Codegen, &Store, &SideExitLabels)>,
}

impl std::fmt::Debug for InlineProcedure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InlineProcedure")
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct AsmDeopt(usize);

#[derive(Debug, Clone, Copy)]
pub(crate) struct AsmError(usize);

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
        }
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

    pub(super) fn take_inst(&mut self) -> Vec<AsmInst> {
        std::mem::take(&mut self.inst)
    }

    pub(super) fn replace_inst(&mut self, inst: Vec<AsmInst>) {
        self.inst = inst;
    }

    pub(super) fn is_empty(&self) -> bool {
        self.inst.is_empty()
    }

    pub(super) fn save(&mut self) -> (usize, usize, bool) {
        (self.inst.len(), self.side_exit.len(), self.codegen_mode)
    }

    pub(super) fn restore(&mut self, (inst, side_exit, codegen_mode): (usize, usize, bool)) {
        self.inst.truncate(inst);
        self.side_exit.truncate(side_exit);
        self.codegen_mode = codegen_mode;
    }

    pub(crate) fn new_evict(&mut self) -> AsmEvict {
        let i = self.new_label(SideExit::Evict(None));
        AsmEvict(i)
    }

    pub(crate) fn new_deopt_with_pc(&mut self, state: &AbstractFrame, pc: BytecodePtr) -> AsmDeopt {
        let i = self.new_label(SideExit::Deoptimize(pc, state.get_write_back()));
        AsmDeopt(i)
    }

    pub(crate) fn new_deopt(&mut self, state: &AbstractFrame) -> AsmDeopt {
        let pc = state.pc();
        self.new_deopt_with_pc(state, pc)
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
    /// - -`using_xmm`.offset()
    ///
    pub(crate) fn xmm_save(&mut self, using_xmm: UsingXmm) {
        self.push(AsmInst::XmmSave(using_xmm, false));
    }

    pub(crate) fn xmm_save_cont(&mut self, using_xmm: UsingXmm) {
        self.push(AsmInst::XmmSave(using_xmm, true));
    }

    ///
    /// Restore floating point registers in use.
    ///
    pub(crate) fn xmm_restore(&mut self, using_xmm: UsingXmm) {
        self.push(AsmInst::XmmRestore(using_xmm, false));
    }

    pub(crate) fn xmm_restore_cont(&mut self, using_xmm: UsingXmm) {
        self.push(AsmInst::XmmRestore(using_xmm, true));
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

    pub(crate) fn stack2reg(&mut self, src: SlotId, dst: GP) {
        self.push(AsmInst::StackToReg(src, dst));
    }

    pub(crate) fn self2reg(&mut self, dst: GP) {
        self.push(AsmInst::StackToReg(SlotId::self_(), dst));
    }

    pub(super) fn xmm_move(&mut self, src: VirtFPReg, dst: VirtFPReg) {
        self.push(AsmInst::XmmMove(src, dst));
    }

    ///
    /// Generate convert code from xmm to stack slots.
    ///
    /// ### out
    /// - rax: Value
    ///
    /// ### destroy
    /// - rcx
    ///
    pub fn xmm2stack(&mut self, xmm: VirtFPReg, reg: SlotId) {
        self.push(AsmInst::XmmToStack(xmm, reg));
    }

    pub fn lit2stack(&mut self, v: Value, reg: SlotId) {
        self.push(AsmInst::LitToStack(v, reg));
    }

    pub fn lit2reg(&mut self, v: Value, reg: GP) {
        self.push(AsmInst::LitToReg(v, reg));
    }

    pub fn acc2stack(&mut self, reg: SlotId) {
        self.push(AsmInst::AccToStack(reg));
    }

    ///
    /// Convert Fixnum to f64.
    ///
    /// ### in
    /// - R(*reg*): Value
    ///
    /// ### out
    /// - xmm(*xmm*)
    ///
    /// ### destroy
    /// - R(*reg*)
    ///
    pub fn fixnum2xmm(&mut self, reg: GP, x: VirtFPReg) {
        self.push(AsmInst::FixnumToXmm(reg, x));
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
    /// - xmm(*xmm*)
    ///
    /// ### destroy
    ///
    /// - rax, rdi
    ///
    pub fn float_to_xmm(&mut self, reg: GP, x: VirtFPReg, deopt: AsmDeopt) {
        self.push(AsmInst::FloatToXmm(reg, x, deopt));
    }

    ///
    /// Move *f*(f64) to VirtFPReg(*x*).
    ///
    pub fn f64_to_xmm(&mut self, f: f64, x: VirtFPReg) {
        self.push(AsmInst::F64ToXmm(f, x));
    }

    ///
    /// Move *i*(i63) to the stack *slot* and VirtFPReg(*x*).
    ///
    pub fn i64_to_stack_and_xmm(&mut self, i: i64, slot: SlotId, x: VirtFPReg) {
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
    pub(super) fn deep_copy_lit(&mut self, using_xmm: UsingXmm, val: Value) {
        self.push(AsmInst::DeepCopyLit(val, using_xmm));
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
        let using_xmm = state.get_using_xmm();
        let error = self.new_error(state);
        self.push(AsmInst::BlockArg {
            ret,
            _outer: outer,
            using_xmm,
            error,
            call_site_bc_ptr,
        });
    }

    pub(super) fn to_a(&mut self, state: &AbstractFrame, src: SlotId) {
        let using_xmm = state.get_using_xmm();
        self.push(AsmInst::ToA { src, using_xmm });
    }

    pub(super) fn concat_str(&mut self, state: &AbstractFrame, arg: SlotId, len: u16) {
        let using_xmm = state.get_using_xmm();
        self.push(AsmInst::ConcatStr {
            arg,
            len,
            using_xmm,
        });
    }

    pub(super) fn concat_regexp(&mut self, state: &AbstractFrame, arg: SlotId, len: u16) {
        let using_xmm = state.get_using_xmm();
        self.push(AsmInst::ConcatRegexp {
            arg,
            len,
            using_xmm,
        });
    }

    pub(super) fn expand_array(
        &mut self,
        state: &AbstractFrame,
        dst: SlotId,
        len: u16,
        rest_pos: Option<u16>,
    ) {
        let using_xmm = state.get_using_xmm();
        let len = len as _;
        let rest_pos = rest_pos.map(|v| v as _);
        self.push(AsmInst::ExpandArray {
            dst,
            len,
            rest_pos,
            using_xmm,
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
        let using_xmm = state.get_using_xmm();
        self.push(AsmInst::ArrayTEq {
            lhs,
            rhs,
            using_xmm,
        });
    }

    pub(super) fn undef_method(&mut self, state: &AbstractFrame, undef: IdentId) {
        let using_xmm = state.get_using_xmm();
        let error = self.new_error(state);
        self.push(AsmInst::UndefMethod { undef, using_xmm });
        self.handle_error(error);
    }

    pub(super) fn alias_method(&mut self, state: &AbstractFrame, new: SlotId, old: SlotId) {
        let using_xmm = state.get_using_xmm();
        let error = self.new_error(state);
        self.push(AsmInst::AliasMethod {
            new,
            old,
            using_xmm,
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

    ///
    /// Float binary operation
    ///
    /// ### in
    /// - depends on *mode*
    ///
    /// ### out
    /// - xmm(*dst*): dst
    ///
    /// ### destroy
    /// - caller save registers
    /// - stack
    ///
    pub(super) fn xmm_binop(&mut self, kind: BinOpK, lhs: VirtFPReg, rhs: VirtFPReg, dst: VirtFPReg) {
        self.push(AsmInst::XmmBinOp {
            kind,
            binary_xmm: (lhs, rhs),
            dst,
        });
    }

    ///
    /// Integer comparison
    ///
    /// Compare two Values in *lhs* and *rhs* with *kind*, and return the result in `rax` as Value.
    ///
    /// If error occurs in comparison operation, raise error.
    ///
    pub(super) fn integer_cmp(&mut self, mode: OpMode, kind: CmpKind, lhs: GP, rhs: GP) {
        self.push(AsmInst::IntegerCmp {
            kind,
            mode,
            lhs,
            rhs,
        });
    }

    pub(super) fn integer_cmp_br(
        &mut self,
        mode: OpMode,
        kind: CmpKind,
        lhs: GP,
        rhs: GP,
        brkind: BrKind,
        branch_dest: JitLabel,
    ) {
        self.push(AsmInst::IntegerCmpBr {
            mode,
            kind,
            lhs,
            rhs,
            brkind,
            branch_dest,
        });
    }

    pub(super) fn float_cmp_br(
        &mut self,
        binary_xmm: (VirtFPReg, VirtFPReg),
        kind: CmpKind,
        brkind: BrKind,
        branch_dest: JitLabel,
    ) {
        self.push(AsmInst::FloatCmpBr {
            lhs: binary_xmm.0,
            rhs: binary_xmm.1,
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
    pub(super) fn new_array(&mut self, using_xmm: UsingXmm, callid: CallSiteId) {
        self.push(AsmInst::NewArray { callid, using_xmm });
    }

    pub(super) fn new_hash(&mut self, using_xmm: UsingXmm, args: SlotId, len: usize) {
        self.push(AsmInst::NewHash(args, len, using_xmm));
    }

    pub(super) fn new_range(
        &mut self,
        start: SlotId,
        end: SlotId,
        exclude_end: bool,
        using_xmm: UsingXmm,
        error: AsmError,
    ) {
        self.push(AsmInst::NewRange {
            start,
            end,
            exclude_end,
            using_xmm,
        });
        self.handle_error(error);
    }

    pub(super) fn block_arg_proxy(&mut self, ret: SlotId, outer: usize) {
        self.push(AsmInst::BlockArgProxy { ret, outer });
    }

    pub(crate) fn inline(
        &mut self,
        f: impl FnOnce(&mut Codegen, &Store, &SideExitLabels) + 'static,
    ) {
        self.inst
            .push(AsmInst::Inline(InlineProcedure { proc: Box::new(f) }));
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
///   the cumulative `using_xmm.offset()` bump applied by every
///   active specialized call up the chain (each call temporarily
///   `+=`s its `using_xmm.offset()` to the caller frame's
///   `stack_offset`; we capture `current - base` for every frame in
///   the chain and sum the differences). Once
///   [`JitContext::specialized_compile`] stops doing the dynamic
///   `+=`/`-=` (e.g. when VirtFPReg-aware spill replaces
///   `using_xmm`), `extra` will be 0.
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
    /// move acc to stack
    AccToStack(SlotId),

    /// move reg to acc
    RegToAcc(GP),

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

    XmmMove(VirtFPReg, VirtFPReg),
    XmmSwap(VirtFPReg, VirtFPReg),
    XmmBinOp {
        kind: BinOpK,
        binary_xmm: (VirtFPReg, VirtFPReg),
        dst: VirtFPReg,
    },
    XmmUnOp {
        kind: UnOpK,
        dst: VirtFPReg,
    },

    ///
    /// Move f64 to xmm.
    ///
    F64ToXmm(f64, VirtFPReg),
    ///
    /// Move *i*(i63) to the stack slot *reg* and VirtFPReg(*x*).
    ///
    I64ToBoth(i64, SlotId, VirtFPReg),
    ///
    /// Generate convert code from VirtFPReg to Both.
    ///
    /// ### out
    /// - rax: Value
    ///
    /// ### destroy
    /// - rcx
    ///
    XmmToStack(VirtFPReg, SlotId),
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
    DeepCopyLit(Value, UsingXmm),
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
    /// - xmm(*xmm*)
    ///
    /// ### destroy
    ///
    /// - rax, rdi, R(*reg*)
    ///
    NumToXmm(GP, VirtFPReg, AsmDeopt),*/
    ///
    /// Convert Fixnum to f64.
    ///
    /// ### in
    /// - R(*reg*): Value
    ///
    /// ### out
    /// - xmm(*xmm*)
    ///
    /// ### destroy
    /// - R(*reg*)
    ///
    FixnumToXmm(GP, VirtFPReg),
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
    /// - xmm(*xmm*)
    ///
    /// ### destroy
    ///
    /// - rax, rdi
    ///
    FloatToXmm(GP, VirtFPReg, AsmDeopt),

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
    /// Load a spilled `VirtFPReg`'s 8-byte value from its stack slot
    /// into a scratch xmm. Emitted by the pre-codegen `expand_spills`
    /// pass before any AsmInst that reads the spilled operand.
    ///
    /// `scratch` is `VirtFPReg::SCRATCH_XMM_0` or
    /// `VirtFPReg::SCRATCH_XMM_1` (the two reserved scratch xmms,
    /// which are not allocated by the pool). `rbp_offset` is the
    /// positive byte distance from `rbp` to the spill slot;
    /// codegen emits `movq xmm(scratch.enc()), [rbp - rbp_offset]`.
    ///
    LoadSpill {
        scratch: VirtFPReg,
        rbp_offset: i32,
    },
    ///
    /// Store a scratch xmm back into a spilled `VirtFPReg`'s stack
    /// slot. Emitted by `expand_spills` after any AsmInst that
    /// writes to the spilled operand. `scratch` and `rbp_offset`
    /// follow the same convention as `LoadSpill`; codegen emits
    /// `movq [rbp - rbp_offset], xmm(scratch.enc())`.
    ///
    StoreSpill {
        scratch: VirtFPReg,
        rbp_offset: i32,
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
    /// - -`using_xmm`.offset()
    ///
    XmmSave(UsingXmm, bool),
    ///
    /// Restore floating point registers in use.
    ///
    XmmRestore(UsingXmm, bool),
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
    #[allow(non_camel_case_types)]
    CFunc_F_F {
        f: unsafe extern "C" fn(f64) -> f64,
        src: VirtFPReg,
        dst: VirtFPReg,
        using_xmm: UsingXmm,
    },
    #[allow(non_camel_case_types)]
    CFunc_FF_F {
        f: extern "C" fn(f64, f64) -> f64,
        lhs: VirtFPReg,
        rhs: VirtFPReg,
        dst: VirtFPReg,
        using_xmm: UsingXmm,
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
    /// Integer comparison
    ///
    /// Compare two Values in *lhs* and *rhs* with *kind*, and return the result in `rax` as Value.
    ///
    /// If error occurs in comparison operation, raise error.
    ///
    IntegerCmp {
        mode: OpMode,
        kind: CmpKind,
        lhs: GP,
        rhs: GP,
    },
    ///
    /// Integer comparison and conditional branch
    ///
    /// Compare two values with `mode``, jump to `branch_dest`` if the condition specified by `kind``
    /// and `brkind`` is met.
    ///
    IntegerCmpBr {
        mode: OpMode,
        kind: CmpKind,
        lhs: GP,
        rhs: GP,
        brkind: BrKind,
        branch_dest: JitLabel,
    },
    FloatCmp {
        kind: CmpKind,
        lhs: VirtFPReg,
        rhs: VirtFPReg,
    },
    FloatCmpBr {
        kind: CmpKind,
        lhs: VirtFPReg,
        rhs: VirtFPReg,
        brkind: BrKind,
        branch_dest: JitLabel,
    },
    ///
    /// Compare `lhs and `rhs` with "===" and return the result in rax.
    ///
    /// If `lhs` is Array, compare `rhs` and each element of `lhs`.
    ///
    ArrayTEq {
        lhs: SlotId,
        rhs: SlotId,
        using_xmm: UsingXmm,
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
        using_xmm: UsingXmm,
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
        using_xmm: UsingXmm,
    },
    ///
    /// Create a new Hash object and store it to *rax*
    ///
    NewHash(SlotId, usize, UsingXmm),
    ///
    /// Create a new Range object and store it to *rax*
    ///
    NewRange {
        start: SlotId,
        end: SlotId,
        exclude_end: bool,
        using_xmm: UsingXmm,
    },
    ToA {
        src: SlotId,
        using_xmm: UsingXmm,
    },
    ConcatStr {
        arg: SlotId,
        len: u16,
        using_xmm: UsingXmm,
    },
    ConcatRegexp {
        arg: SlotId,
        len: u16,
        using_xmm: UsingXmm,
    },

    BlockArgProxy {
        ret: SlotId,
        outer: usize,
    },
    BlockArg {
        ret: SlotId,
        _outer: usize,
        using_xmm: UsingXmm,
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
        using_xmm: UsingXmm,
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
        using_xmm: UsingXmm,
    },
    CheckCVar {
        name: IdentId,
        using_xmm: UsingXmm,
    },
    StoreCVar {
        name: IdentId,
        src: SlotId,
        using_xmm: UsingXmm,
    },
    LoadGVar {
        name: IdentId,
        using_xmm: UsingXmm,
    },
    StoreGVar {
        name: IdentId,
        src: SlotId,
        using_xmm: UsingXmm,
    },

    ClassDef {
        base: Option<SlotId>,
        superclass: Option<SlotId>,
        dst: Option<SlotId>,
        name: IdentId,
        func_id: FuncId,
        is_module: bool,
        using_xmm: UsingXmm,
        error: AsmError,
    },
    SingletonClassDef {
        base: SlotId,
        dst: Option<SlotId>,
        func_id: FuncId,
        using_xmm: UsingXmm,
        error: AsmError,
    },
    MethodDef {
        name: IdentId,
        func_id: FuncId,
        using_xmm: UsingXmm,
        error: AsmError,
    },
    SingletonMethodDef {
        obj: SlotId,
        name: IdentId,
        func_id: FuncId,
        using_xmm: UsingXmm,
        error: AsmError,
    },

    ExpandArray {
        dst: SlotId,
        len: usize,
        rest_pos: Option<usize>,
        using_xmm: UsingXmm,
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
        using_xmm: UsingXmm,
    },
    AliasMethod {
        new: SlotId,
        old: SlotId,
        using_xmm: UsingXmm,
    },
    AliasGvar {
        new: IdentId,
        old: IdentId,
        using_xmm: UsingXmm,
    },
    ///
    /// Check if `yield` is callable.
    ///
    /// Set `dst` to "yield" if callable, `nil` if not.
    ///
    DefinedYield {
        dst: SlotId,
        using_xmm: UsingXmm,
    },
    DefinedConst {
        dst: SlotId,
        siteid: ConstSiteId,
        using_xmm: UsingXmm,
    },
    DefinedMethod {
        dst: SlotId,
        recv: SlotId,
        name: IdentId,
        using_xmm: UsingXmm,
    },
    ///
    /// Check if `super` is callable.
    ///
    /// Set `dst` to "super" if callable, `nil` if not.
    ///
    DefinedSuper {
        dst: SlotId,
        using_xmm: UsingXmm,
    },
    ///
    /// Check if global var `name` exists.
    ///
    /// Set `dst`` to "global-variable" if exists, `nil` if not.
    ///
    DefinedGvar {
        dst: SlotId,
        name: IdentId,
        using_xmm: UsingXmm,
    },
    DefinedIvar {
        dst: SlotId,
        name: IdentId,
        using_xmm: UsingXmm,
    },
    DefinedCvar {
        dst: SlotId,
        name: IdentId,
        using_xmm: UsingXmm,
    },
}

impl AsmInst {
    ///
    /// Enumerate every `VirtFPReg` operand referenced by this
    /// instruction (in any role — read, write, or read-write).
    /// Used by `pop_frame` to compute the frame's max
    /// `VirtFPReg` id (and thus the stack-spill region size) and
    /// — once Phase 2's codegen-side spill expansion lands — to
    /// detect operands that need swap-load-swap-store.
    ///
    pub(super) fn xmm_operands(&self) -> Vec<VirtFPReg> {
        match self {
            Self::XmmMove(a, b) | Self::XmmSwap(a, b) => vec![*a, *b],
            Self::XmmBinOp {
                binary_xmm: (l, r),
                dst,
                ..
            } => vec![*l, *r, *dst],
            Self::XmmUnOp { dst, .. } => vec![*dst],
            Self::F64ToXmm(_, x) => vec![*x],
            Self::I64ToBoth(_, _, x) => vec![*x],
            Self::XmmToStack(x, _) => vec![*x],
            Self::FixnumToXmm(_, x) => vec![*x],
            Self::FloatToXmm(_, x, _) => vec![*x],
            Self::CFunc_F_F { src, dst, .. } => vec![*src, *dst],
            Self::CFunc_FF_F { lhs, rhs, dst, .. } => vec![*lhs, *rhs, *dst],
            Self::FloatCmp { lhs, rhs, .. } => vec![*lhs, *rhs],
            Self::FloatCmpBr { lhs, rhs, .. } => vec![*lhs, *rhs],
            _ => vec![],
        }
    }

    ///
    /// Mutable view of every `VirtFPReg` operand in this instruction.
    /// Used by `expand_spills` to rewrite spilled operands into
    /// scratch xmm markers in place.
    ///
    pub(super) fn xmm_operands_mut(&mut self) -> Vec<&mut VirtFPReg> {
        match self {
            // The variants below resolve spilled operands themselves
            // in their codegen lowerings (using x86 memory operands
            // and dedicated scratch paths), so their operands stay
            // raw through `expand_spills`.
            Self::XmmMove(_, _) | Self::XmmSwap(_, _) | Self::XmmBinOp { .. } => vec![],
            Self::XmmUnOp { dst, .. } => vec![dst],
            Self::F64ToXmm(_, x) => vec![x],
            Self::I64ToBoth(_, _, x) => vec![x],
            Self::XmmToStack(x, _) => vec![x],
            Self::FixnumToXmm(_, x) => vec![x],
            Self::FloatToXmm(_, x, _) => vec![x],
            Self::CFunc_F_F { src, dst, .. } => vec![src, dst],
            Self::CFunc_FF_F { lhs, rhs, dst, .. } => vec![lhs, rhs, dst],
            Self::FloatCmp { lhs, rhs, .. } => vec![lhs, rhs],
            Self::FloatCmpBr { lhs, rhs, .. } => vec![lhs, rhs],
            _ => vec![],
        }
    }

    #[allow(dead_code)]
    #[cfg(feature = "emit-asm")]
    #[allow(dead_code)]
    pub fn dump(&self, store: &Store) -> String {
        match self {
            Self::AccToStack(slot) => format!("{:?} = R15", slot),
            Self::RegToAcc(gpr) => format!("R15 = {:?}", gpr),
            Self::RegToStack(gpr, slot) => format!("{:?} = {:?}", slot, gpr),
            Self::StackToReg(slot, gpr) => format!("{:?} = {:?}", gpr, slot),
            Self::LitToReg(val, gpr) => format!("{:?} = {}", gpr, val.debug(store)),
            Self::RegMove(src, dst) => format!("{:?} = {:?}", dst, src),
            Self::RegAdd(gpr, i) => format!("{:?} += {i}", gpr),
            Self::RegSub(gpr, i) => format!("{:?} -= {i}", gpr,),
            Self::RegToRSPOffset(gpr, offset) => format!("RSP[{offset}] = {:?}", gpr),
            Self::XmmMove(src, dst) => format!("{:?} = {:?}", dst, src),
            Self::XmmSwap(fp1, fp2) => format!("{:?} <-> {:?}", fp1, fp2),
            Self::XmmBinOp {
                kind,
                binary_xmm,
                dst,
            } => format!(
                "{:?} = {:?} {:?} {:?}",
                dst, binary_xmm.0, kind, binary_xmm.1
            ),
            Self::XmmUnOp { kind, dst } => format!("{:?} = {:?} {:?}", dst, kind, dst),

            Self::F64ToXmm(f, dst) => format!("{:?} = {}", dst, f),
            Self::I64ToBoth(i, slot, xmm) => format!("{:?}:{:?} = {i}", slot, xmm),
            Self::XmmToStack(fpr, slots) => format!("{:?} = {:?}", slots, fpr),
            Self::LitToStack(val, slot) => format!("{:?} = {}", slot, val.debug(store)),
            Self::DeepCopyLit(val, _using_xmm) => format!("DeepCopyLiteral {}", val.debug(store)),
            Self::FloatToXmm(gpr, fpr, _deopt) => format!("{:?} = {:?} Float to f64", fpr, gpr),
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
                reason,
            } => {
                format!("recompile_deopt {:?} {:?}", position, reason)
            }
            Self::HandleError(error) => format!("handle_error {:?}", error),
            Self::XmmSave(using_xmm, cont) => format!("xmm_save {:?} {cont}", using_xmm),
            Self::XmmRestore(using_xmm, cont) => format!("xmm_restore {:?} {cont}", using_xmm),
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
    Error(BytecodePtr, WriteBack),
}

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
    ) {
        let mut side_exits = SideExitLabels::new();
        let mut deopt_table: HashMap<(BytecodePtr, WriteBack), DestLabel> = HashMap::default();
        let loop_jit_spill_bytes = frame.loop_jit_spill_bytes;
        for side_exit in ir.side_exit {
            let label = match side_exit {
                SideExit::Evict(Some((pc, wb))) => {
                    let label = self.jit.label();
                    self.gen_evict_with_label(pc, &wb, label.clone(), loop_jit_spill_bytes);
                    label
                }
                SideExit::Deoptimize(pc, wb) => {
                    let t = (pc, wb);
                    if let Some(label) = deopt_table.get(&t) {
                        label.clone()
                    } else {
                        let label = self.jit.label();
                        self.gen_deopt_with_label(pc, &t.1, label.clone(), loop_jit_spill_bytes);
                        deopt_table.insert(t, label.clone());
                        label
                    }
                }
                SideExit::Error(pc, wb) => {
                    let label = self.jit.label();
                    self.gen_handle_error(pc, wb, label.clone());
                    label
                }
                _ => unreachable!("unexpected {side_exit:?}"),
            };
            side_exits.push(label);
        }

        if entry.is_some() && exit.is_some() {
            self.jit.select_page(1);
        }

        if let Some(entry) = &entry {
            self.jit.bind_label(entry.clone());
        }

        for inst in ir.inst {
            #[cfg(feature = "emit-asm")]
            {
                //eprintln!("  ; {}", inst.dump(store));
            }
            self.compile_asmir(store, frame, &side_exits, inst, class_version.clone());
        }

        if let Some(exit) = exit {
            let exit = frame.resolve_bb_label(&mut self.jit, exit);
            monoasm! { &mut self.jit,
                jmp exit;
            }
        }
        if entry.is_some() && exit.is_some() {
            self.jit.select_page(0);
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
