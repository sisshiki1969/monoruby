use super::*;

mod join;
mod liveness;
mod read_slot;
mod slot;

use liveness::IsUsed;
pub(super) use liveness::Liveness;
use slot::SfGuarded;
pub(super) use slot::{Guarded, LinkMode, SlotState};

#[derive(Debug, Clone)]
pub(crate) struct AbstractState {
    frames: Vec<AbstractFrame>,
}

impl std::ops::Deref for AbstractState {
    type Target = AbstractFrame;
    fn deref(&self) -> &Self::Target {
        &self.frames.last().unwrap()
    }
}

impl std::ops::DerefMut for AbstractState {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.frames.last_mut().unwrap()
    }
}

impl std::ops::Index<usize> for AbstractState {
    type Output = AbstractFrame;
    fn index(&self, index: usize) -> &Self::Output {
        &self.frames[index]
    }
}

impl std::ops::IndexMut<usize> for AbstractState {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.frames[index]
    }
}

impl AbstractState {
    pub(super) fn new(jitctx: &JitContext) -> Self {
        let mut frames = jitctx.outer_contexts();
        frames.push(AbstractFrame::new(jitctx));
        AbstractState { frames }
    }

    pub(super) fn equiv(&self, other: &Self) -> bool {
        self.frames
            .iter()
            .zip(other.frames.iter())
            .all(|(lhs, rhs)| lhs.equiv(rhs))
    }

    pub(super) fn unset_no_capture_guard(&mut self, jitctx: &mut JitContext) {
        jitctx.unset_outer_no_capture_guard();
        self.unset_all_no_capture_guard();
    }

    pub(super) fn unset_all_no_capture_guard(&mut self) {
        for frame in &mut self.frames {
            frame.unset_no_capture_guard();
        }
    }

    pub(super) fn outer_no_capture_guard(&self, outer: usize) -> Option<bool> {
        if outer >= self.frames.len() {
            return None;
        }
        Some(self.frames[self.frames.len() - 1 - outer].no_capture_guard())
    }

    pub(super) fn join_entries(entries: &[BranchEntry]) -> Self {
        let mut merge_ctx = entries.last().unwrap().state.clone();
        for BranchEntry { state, .. } in entries.iter() {
            merge_ctx.join(state);
        }
        merge_ctx
    }

    ///
    /// Generate bridge AsmIr to merge current state with target state.
    ///
    pub(super) fn gen_bridge(mut self, ir: &mut AsmIr, target: &SlotState, pc: BytecodePtr) {
        #[cfg(feature = "jit-debug")]
        eprintln!("      from:{:?}", &self);
        for slot in self.all_regs() {
            self.bridge(ir, target, slot, pc);
        }
    }
}

///
/// Context of an each basic block.
///
#[derive(Debug, Clone, Default)]
pub(crate) struct AbstractFrame {
    /// state stack slots.
    slot_state: SlotState,
    /// stack top register.
    next_sp: SlotId,
    /// assumptions
    invariants: Invariants,
}

impl std::ops::Deref for AbstractFrame {
    type Target = SlotState;
    fn deref(&self) -> &Self::Target {
        &self.slot_state
    }
}

impl std::ops::DerefMut for AbstractFrame {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.slot_state
    }
}

impl AbstractFrame {
    fn new(cc: &JitContext) -> Self {
        let next_sp = SlotId(cc.local_num() as u16 + 1);
        match cc.jit_type() {
            JitType::Entry => AbstractFrame {
                slot_state: SlotState::new_method(cc),
                next_sp,
                invariants: Invariants::new_entry(cc),
            },
            JitType::Loop(_) => AbstractFrame {
                slot_state: SlotState::new_loop(cc),
                next_sp,
                invariants: Invariants::new_loop(),
            },
            JitType::Specialized { .. } => AbstractFrame {
                slot_state: SlotState::new_method(cc),
                next_sp,
                invariants: Invariants::new_specialized(cc),
            },
        }
    }

    fn equiv(&self, other: &Self) -> bool {
        self.slot_state.equiv(&other.slot_state) && self.invariants == other.invariants
    }

    pub(in crate::codegen::jitgen) fn slot_state(&self) -> &SlotState {
        &self.slot_state
    }

    pub(in crate::codegen::jitgen) fn set_next_sp(&mut self, slot: SlotId) {
        self.next_sp = slot;
    }

    pub(super) fn no_capture_guard(&self) -> bool {
        self.invariants.no_capture_guard
    }

    pub(super) fn unset_no_capture_guard(&mut self) {
        self.invariants.no_capture_guard = false;
    }

    pub(super) fn class_version_guard(&self) -> bool {
        self.invariants.class_version_guard
    }

    pub(super) fn set_class_version_guard(&mut self) {
        self.invariants.class_version_guard = true;
    }

    pub(super) fn unset_class_version_guard(&mut self) {
        self.invariants.class_version_guard = false;
    }

    pub(super) fn unset_side_effect_guard(&mut self) {
        self.invariants.side_effect_guard = false;
    }

    pub(super) fn as_return(&self, slot: SlotId) -> ReturnState {
        let ret = self.mode(slot).as_return();
        ReturnState {
            ret,
            invariants: self.invariants.clone(),
        }
    }

    pub(crate) fn def_rax2acc(&mut self, ir: &mut AsmIr, dst: impl Into<Option<SlotId>>) {
        self.def_reg2acc(ir, GP::Rax, dst);
    }

    pub(crate) fn def_reg2acc(&mut self, ir: &mut AsmIr, src: GP, dst: impl Into<Option<SlotId>>) {
        self.def_reg2acc_guarded(ir, src, dst, slot::Guarded::Value)
    }

    pub(crate) fn def_reg2acc_fixnum(
        &mut self,
        ir: &mut AsmIr,
        src: GP,
        dst: impl Into<Option<SlotId>>,
    ) {
        self.def_reg2acc_guarded(ir, src, dst, slot::Guarded::Fixnum)
    }

    pub(crate) fn def_reg2acc_class(
        &mut self,
        ir: &mut AsmIr,
        src: GP,
        dst: impl Into<Option<SlotId>>,
        class: ClassId,
    ) {
        self.def_reg2acc_guarded(ir, src, dst, slot::Guarded::from_class(class))
    }

    pub(crate) fn def_reg2acc_concrete_value(
        &mut self,
        ir: &mut AsmIr,
        src: GP,
        dst: impl Into<Option<SlotId>>,
        v: Value,
    ) {
        self.def_reg2acc_guarded(ir, src, dst, Guarded::from_concrete_value(v))
    }

    fn def_reg2acc_guarded(
        &mut self,
        ir: &mut AsmIr,
        src: GP,
        dst: impl Into<Option<SlotId>>,
        guarded: slot::Guarded,
    ) {
        if let Some(dst) = dst.into() {
            self.def_G(ir, dst, guarded);
            ir.push(AsmInst::RegToAcc(src));
        }
    }

    pub(in crate::codegen::jitgen) fn def_rax2acc_return(
        &mut self,
        ir: &mut AsmIr,
        dst: impl Into<Option<SlotId>>,
        return_state: Option<ReturnState>,
    ) -> CompileResult {
        if let Some(return_state) = return_state {
            self.invariants.join(&return_state.invariants);
            match return_state.ret {
                ReturnValue::UD => {
                    ir.push(AsmInst::Unreachable);
                    return CompileResult::Cease;
                }
                ReturnValue::Const(v) => {
                    self.def_C(dst, v);
                }
                ReturnValue::Class(class) => {
                    self.def_reg2acc_class(ir, GP::Rax, dst, class);
                }
                ReturnValue::Value => {
                    self.def_rax2acc(ir, dst);
                }
            }
            CompileResult::Continue
        } else {
            ir.push(AsmInst::Unreachable);
            CompileResult::Cease
        }
    }

    ///
    /// Guard for the base class object of the constant in *slot*.
    ///
    /// ### destroy
    /// - rax
    ///
    pub(in crate::codegen::jitgen) fn guard_const_base_class(
        &mut self,
        ir: &mut AsmIr,
        slot: SlotId,
        base_class: Value,
        pc: BytecodePtr,
    ) {
        self.load(ir, slot, GP::Rax);
        let deopt = ir.new_deopt(self, pc);
        ir.push(AsmInst::GuardConstBaseClass { base_class, deopt });
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
    pub fn exec_gc(&self, ir: &mut AsmIr, check_stack: bool, pc: BytecodePtr) {
        let wb = self.get_gc_write_back();
        let error = ir.new_error(self, pc);
        ir.exec_gc(wb, error, check_stack);
    }

    pub fn load_constant(
        &mut self,
        ir: &mut AsmIr,
        dst: SlotId,
        cache: &ConstCache,
        pc: BytecodePtr,
    ) {
        let ConstCache { version, value, .. } = cache;
        let deopt = ir.new_deopt(self, pc);
        ir.push(AsmInst::GuardConstVersion {
            const_version: *version,
            deopt,
        });
        ir.lit2reg(*value, GP::Rax);
        if let Some(f) = value.try_float() {
            let fdst = self.def_Sf_float(dst);
            ir.f64_to_xmm(f, fdst);
            ir.reg2stack(GP::Rax, dst);
        } else {
            self.def_reg2acc(ir, GP::Rax, dst);
        }
    }
}

impl AbstractFrame {
    ///
    /// Discard slots above *next_sp*.
    ///
    pub(in crate::codegen::jitgen) fn clear_above_next_sp(&mut self) {
        let sp = self.next_sp;
        for i in sp..SlotId(self.slots_len() as u16) {
            self.discard(i);
        }
    }
}

// write back operations
impl AbstractFrame {
    pub(crate) fn write_back_slots(&mut self, ir: &mut AsmIr, slot: &[SlotId]) {
        slot.iter().for_each(|r| self.write_back_slot(ir, *r));
    }

    ///
    /// Fetch from *args* to *args* + *len* - 1 and store in corresponding stack slots.
    ///
    pub(super) fn write_back_range(&mut self, ir: &mut AsmIr, args: SlotId, len: u16) {
        for reg in args.0..args.0 + len {
            self.write_back_slot(ir, SlotId::new(reg))
        }
    }

    pub(crate) fn write_back_recv_and_callargs(&mut self, ir: &mut AsmIr, callsite: &CallSiteInfo) {
        self.write_back_slot(ir, callsite.recv);
        self.write_back_args(ir, callsite);
    }

    pub(super) fn write_back_args(&mut self, ir: &mut AsmIr, callsite: &CallSiteInfo) {
        let CallSiteInfo {
            args,
            pos_num,
            kw_pos,
            block_arg,
            ..
        } = callsite;
        self.write_back_range(ir, *args, *pos_num as u16);
        self.write_back_range(ir, *kw_pos, callsite.kw_len() as u16);
        if let Some(block_arg) = block_arg {
            self.write_back_slot(ir, *block_arg);
        }
    }

    #[allow(non_snake_case)]
    pub(super) fn locals_to_S(&mut self, ir: &mut AsmIr) {
        for i in self.locals() {
            self.to_S(ir, i);
        }
    }

    pub(super) fn write_back_locals_if_captured(&mut self, ir: &mut AsmIr) {
        if !self.no_capture_guard() {
            let wb = self.get_locals_write_back();
            ir.push(AsmInst::WriteBackIfCaptured(wb));
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct ReturnState {
    ret: ReturnValue,
    invariants: Invariants,
}

#[derive(Debug, Clone, Copy)]
enum ReturnValue {
    UD,
    Const(Value),
    Class(ClassId),
    Value,
}

impl ReturnState {
    pub(super) fn default() -> Self {
        ReturnState {
            ret: ReturnValue::Value,
            invariants: Invariants::default(),
        }
    }

    pub(super) fn may_side_effect() -> Self {
        ReturnState {
            ret: ReturnValue::UD,
            invariants: Invariants {
                class_version_guard: true,
                side_effect_guard: false,
                no_capture_guard: true,
            },
        }
    }

    pub(super) fn unset_side_effect_guard(&mut self) {
        self.invariants.side_effect_guard = false;
    }

    fn return_class(&self) -> Option<ClassId> {
        match self.ret {
            ReturnValue::Const(v) => Some(v.class()),
            ReturnValue::Class(class) => Some(class),
            _ => None,
        }
    }

    pub(in crate::codegen::jitgen) fn const_folded(&self) -> Option<Value> {
        if !self.invariants.side_effect_guard {
            return None;
        }
        if let ReturnValue::Const(v) = self.ret {
            return Some(v);
        }
        None
    }

    pub(in crate::codegen::jitgen) fn join(&mut self, other: &Self) {
        self.invariants.join(&other.invariants);
        match (&self.ret, &other.ret) {
            (ReturnValue::UD, _) => {
                self.ret = other.ret;
                return;
            }
            (_, ReturnValue::UD) => return,
            (ReturnValue::Const(l), ReturnValue::Const(r)) if l.id() == r.id() => {
                return;
            }
            _ => {}
        }

        if let Some(class) = self.return_class()
            && other.return_class() == Some(class)
        {
            self.ret = ReturnValue::Class(class);
            return;
        }
        self.ret = ReturnValue::Value;
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct Invariants {
    /// guard for class version. true if guaranteed the class version is not changed.
    class_version_guard: bool,
    /// guard for frame capture. true if guaranteed the frame is not captured.
    no_capture_guard: bool,
    /// guard for side effect. true if guaranteed no side effects are not occurred.
    ///
    /// all of the following conditions must be satisfied:
    /// 1) no exceptions
    /// 2) no modifications to global variables, class variables, instance variables, and constans.
    /// 3) no method calls that may have side effects.
    /// 4) no ensure clauses
    side_effect_guard: bool,
}

impl Invariants {
    fn new_entry(cc: &JitContext) -> Self {
        let side_effect_guard = cc.iseq().no_ensure();
        Self {
            class_version_guard: false,
            no_capture_guard: true,
            side_effect_guard,
        }
    }

    fn new_loop() -> Self {
        Self {
            class_version_guard: false,
            // loop compilation is started only if the current local frame is not captured.
            no_capture_guard: true,
            side_effect_guard: false,
        }
    }

    fn new_specialized(cc: &JitContext) -> Self {
        let side_effect_guard = cc.iseq().no_ensure();
        Self {
            class_version_guard: false,
            // specialized methods and blocks are always guarded frame capture.
            no_capture_guard: true,
            side_effect_guard,
        }
    }

    fn join(&mut self, other: &Self) {
        self.class_version_guard &= other.class_version_guard;
        self.no_capture_guard &= other.no_capture_guard;
        self.side_effect_guard &= other.side_effect_guard;
    }
}
