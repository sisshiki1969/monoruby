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

pub(crate) struct AsmIr {
    pub(super) inst: Vec<AsmInst>,
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
    pub(super) fn new() -> Self {
        Self {
            inst: vec![],
            side_exit: vec![],
        }
    }

    pub(super) fn push(&mut self, inst: AsmInst) {
        self.inst.push(inst);
    }

    pub(super) fn save(&mut self) -> (usize, usize) {
        (self.inst.len(), self.side_exit.len())
    }

    pub(super) fn restore(&mut self, (inst, side_exit): (usize, usize)) {
        self.inst.truncate(inst);
        self.side_exit.truncate(side_exit);
    }

    pub(crate) fn new_evict(&mut self) -> AsmEvict {
        let i = self.new_label(SideExit::Evict(None));
        AsmEvict(i)
    }

    pub(crate) fn new_deopt(&mut self, bb: &BBContext) -> AsmDeopt {
        self.new_deopt_with_pc(bb, bb.pc())
    }

    pub(crate) fn new_deopt_with_pc(&mut self, bb: &BBContext, pc: BytecodePtr) -> AsmDeopt {
        let i = self.new_label(SideExit::Deoptimize(pc, bb.get_write_back()));
        AsmDeopt(i)
    }

    pub(crate) fn new_error(&mut self, bb: &BBContext) -> AsmError {
        let i = self.new_label(SideExit::Error(bb.pc(), bb.get_write_back()));
        AsmError(i)
    }
}

impl AsmIr {
    ///
    /// Save floating point registers in use.
    ///
    pub(crate) fn xmm_save(&mut self, using_xmm: UsingXmm) {
        self.push(AsmInst::XmmSave(using_xmm));
    }

    ///
    /// Restore floating point registers in use.
    ///
    pub(crate) fn xmm_restore(&mut self, using_xmm: UsingXmm) {
        self.push(AsmInst::XmmRestore(using_xmm));
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

    pub(super) fn reg_and(&mut self, r: GP, i: u64) {
        self.push(AsmInst::RegAnd(r, i));
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

    pub(super) fn xmm_move(&mut self, src: Xmm, dst: Xmm) {
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
    pub fn xmm2stack(&mut self, xmm: Xmm, reg: SlotId) {
        self.push(AsmInst::XmmToStack(xmm, reg));
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
    pub fn fixnum2xmm(&mut self, reg: GP, x: Xmm) {
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
    pub fn float_to_xmm(&mut self, reg: GP, x: Xmm, deopt: AsmDeopt) {
        self.push(AsmInst::FloatToXmm(reg, x, deopt));
    }

    ///
    /// Move *f*(f64) to Xmm(*x*).
    ///
    pub fn f64_to_xmm(&mut self, f: f64, x: Xmm) {
        self.push(AsmInst::F64ToXmm(f, x));
    }

    ///
    /// Move *i*(i63) to the stack *slot* and Xmm(*x*).
    ///
    pub fn i64_to_stack_and_xmm(&mut self, i: i64, slot: SlotId, x: Xmm) {
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

    pub(super) fn deopt(&mut self, bb: &BBContext) {
        let exit = self.new_deopt(bb);
        self.push(AsmInst::Deopt(exit));
    }

    pub(super) fn check_bop(&mut self, bb: &BBContext) {
        let deopt = self.new_deopt(bb);
        self.push(AsmInst::CheckBOP { deopt });
    }

    pub(super) fn block_arg(&mut self, bb: &BBContext, ret: SlotId, outer: usize) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb);
        self.push(AsmInst::BlockArg {
            ret,
            outer,
            using_xmm,
            error,
        });
    }

    pub(super) fn load_svar(&mut self, bb: &BBContext, id: u32) {
        let using_xmm = bb.get_using_xmm();
        self.push(AsmInst::LoadSVar { id, using_xmm });
    }

    pub(super) fn to_a(&mut self, bb: &BBContext, src: SlotId) {
        let using_xmm = bb.get_using_xmm();
        self.push(AsmInst::ToA { src, using_xmm });
    }

    pub(super) fn concat_str(&mut self, bb: &BBContext, arg: SlotId, len: u16) {
        let using_xmm = bb.get_using_xmm();
        self.push(AsmInst::ConcatStr {
            arg,
            len,
            using_xmm,
        });
    }

    pub(super) fn concat_regexp(&mut self, bb: &BBContext, arg: SlotId, len: u16) {
        let using_xmm = bb.get_using_xmm();
        self.push(AsmInst::ConcatRegexp {
            arg,
            len,
            using_xmm,
        });
    }

    pub(super) fn expand_array(
        &mut self,
        bb: &BBContext,
        dst: SlotId,
        len: u16,
        rest_pos: Option<u16>,
    ) {
        let using_xmm = bb.get_using_xmm();
        let len = len as _;
        let rest_pos = rest_pos.map(|v| v as _);
        self.push(AsmInst::ExpandArray {
            dst,
            len,
            rest_pos,
            using_xmm,
        });
    }

    ///
    /// Compare `lhs and `rhs` with "===" and return the result in rax.
    ///
    /// If `lhs` is Array, compare `rhs` and each element of `lhs`.
    ///
    pub(super) fn array_teq(&mut self, bb: &BBContext, lhs: SlotId, rhs: SlotId) {
        let using_xmm = bb.get_using_xmm();
        self.push(AsmInst::ArrayTEq {
            lhs,
            rhs,
            using_xmm,
        });
    }

    pub(super) fn undef_method(&mut self, bb: &BBContext, undef: IdentId) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb);
        self.push(AsmInst::UndefMethod { undef, using_xmm });
        self.handle_error(error);
    }

    pub(super) fn alias_method(&mut self, bb: &BBContext, new: IdentId, old: IdentId) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb);
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

    pub(super) fn handle_hash_splat_kwrest(
        &mut self,
        store: &Store,
        callid: CallSiteId,
        callee_fid: FuncId,
        error: AsmError,
    ) {
        let caller = &store[callid];
        let callee = &store[callee_fid];
        if callee.kw_rest().is_some() || !caller.hash_splat_pos.is_empty() {
            let meta = callee.meta();
            let offset = callee.get_offset();
            self.push(AsmInst::SetupHashSplatKwRest {
                callid,
                meta,
                offset,
                error,
            });
        }
    }

    ///
    /// Handle error.
    ///
    /// Check *rax*, and if it is 0, go to 'error'.
    ///
    pub(crate) fn handle_error(&mut self, error: AsmError) {
        self.push(AsmInst::HandleError(error));
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
    pub(super) fn xmm_binop(&mut self, kind: BinOpK, mode: FMode, dst: Xmm, using_xmm: UsingXmm) {
        self.push(AsmInst::XmmBinOp {
            kind,
            mode,
            dst,
            using_xmm,
        });
    }

    pub(super) fn integer_exp(&mut self, bb: &BBContext) {
        let using_xmm = bb.get_using_xmm();
        self.push(AsmInst::IntegerExp { using_xmm });
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
        mode: FMode,
        kind: CmpKind,
        brkind: BrKind,
        branch_dest: JitLabel,
    ) {
        self.push(AsmInst::FloatCmpBr {
            mode,
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
    ///
    /// Array index operation with u16 index `idx``.
    ///
    /// Execute *rdi*[[`idx`]] and store the result to *rax*.
    ///
    /// ### in
    /// - rdi: base Array
    ///
    /// ### out
    /// - rax: result Value
    ///
    pub(crate) fn array_u16_index(&mut self, idx: u16) {
        self.push(AsmInst::ArrayU16Index { idx });
    }

    ///
    /// Array index operation.
    ///
    /// ### in
    /// - rdi: base Array
    /// - rsi: index Fixnum
    ///
    /// ### out
    /// - rax: result Value
    ///
    pub(crate) fn array_index(&mut self) {
        self.push(AsmInst::ArrayIndex);
    }

    pub(super) fn generic_index_assign(
        &mut self,
        bb: &BBContext,
        base: SlotId,
        idx: SlotId,
        src: SlotId,
    ) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb);
        let pc = bb.pc();
        self.push(AsmInst::GenericIndexAssign {
            src,
            base,
            idx,
            pc,
            using_xmm,
        });
        self.handle_error(error);
    }

    ///
    /// Array index assign operation with u16 index `idx`.
    ///
    /// ### in
    /// - rdi: base: Array
    /// - rdx: result Value
    ///
    /// ### destroy
    /// - caller save registers
    ///
    pub(super) fn array_u16_index_assign(&mut self, bb: &BBContext, idx: u16) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb);
        self.push(AsmInst::ArrayU16IndexAssign {
            idx,
            using_xmm,
            error,
        });
    }

    ///
    /// Aray index assign operation.
    ///
    /// ### in
    /// - rdi: base Array
    /// - rsi: index Fixnum
    /// - rdx: result Value
    ///    
    /// ### destroy
    /// - caller save registers
    ///
    pub(super) fn array_index_assign(&mut self, bb: &BBContext) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb);
        self.inst
            .push(AsmInst::ArrayIndexAssign { using_xmm, error });
    }
}

impl AsmIr {
    pub(super) fn new_array(&mut self, using_xmm: UsingXmm, callid: CallSiteId) {
        self.push(AsmInst::NewArray { callid, using_xmm });
    }

    pub(super) fn new_lambda(&mut self, using_xmm: UsingXmm, func_id: FuncId) {
        self.push(AsmInst::NewLambda(func_id, using_xmm));
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

    #[cfg(feature = "emit-asm")]
    pub(crate) fn bc_index(&mut self, index: BcIndex) {
        self.push(AsmInst::BcIndex(index));
    }
}

#[derive(Debug)]
pub(super) enum AsmInst {
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
    RegAnd(GP, u64),
    /// movq [rsp + (ofs)], R(r);
    RegToRSPOffset(GP, i32),
    /// movq [rsp + (ofs)], 0;
    ZeroToRSPOffset(i32),
    /// movq [rsp + (ofs)], (i);
    U64ToRSPOffset(u64, i32),
    RSPOffsetToArray(i32),

    XmmMove(Xmm, Xmm),
    XmmSwap(Xmm, Xmm),
    XmmBinOp {
        kind: BinOpK,
        mode: FMode,
        dst: Xmm,
        using_xmm: UsingXmm,
    },
    XmmUnOp {
        kind: UnOpK,
        dst: Xmm,
    },

    ///
    /// Move f64 to xmm.
    ///
    F64ToXmm(f64, Xmm),
    ///
    /// Move *i*(i63) to the stack slot *reg* and Xmm(*x*).
    ///
    I64ToBoth(i64, SlotId, Xmm),
    ///
    /// Generate convert code from Xmm to Both.
    ///
    /// ### out
    /// - rax: Value
    ///
    /// ### destroy
    /// - rcx
    ///
    XmmToStack(Xmm, SlotId),
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
    NumToXmm(GP, Xmm, AsmDeopt),*/
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
    FixnumToXmm(GP, Xmm),
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
    FloatToXmm(GP, Xmm, AsmDeopt),

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

    Ret,
    BlockBreak,
    Raise,
    MethodRet(BytecodePtr),
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
    Init {
        info: FnInitInfo,
        is_method: bool,
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
    WriteBackIfCaptured(WriteBack),
    ///
    /// Handle error.
    ///
    /// Check *rax*, and if it is 0, go to 'error'.
    ///
    HandleError(AsmError),
    ///
    /// Save floating point registers in use.
    ///
    XmmSave(UsingXmm),
    ///
    /// Restore floating point registers in use.
    ///
    XmmRestore(UsingXmm),
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
    /// Set arguments.
    ///
    /// ### out
    /// - rdi: the number of arguments
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
    /// ### in
    /// - r13: receiver
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
    /// Set up a callee frame for binop method call.
    ///
    /// ### in
    /// - r13: receiver
    ///
    /// ### destroy
    /// - rax
    ///
    SetupBinopFrame {
        meta: Meta,
    },
    SetupHashSplatKwRest {
        callid: CallSiteId,
        meta: Meta,
        offset: usize,
        error: AsmError,
    },
    CopyKeywordArgs {
        callid: CallSiteId,
        callee_fid: FuncId,
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
    CFunc {
        f: extern "C" fn(f64) -> f64,
        src: Xmm,
        dst: Xmm,
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

    Not,
    FixnumNeg {
        reg: GP,
        deopt: AsmDeopt,
    },
    FixnumBitNot {
        reg: GP,
    },
    GenericUnOp {
        func: UnaryOpFn,
        using_xmm: UsingXmm,
    },

    /*///
    /// Generic integer operation.
    ///
    /// ### in
    /// - rdi: lhs
    /// - rsi: rhs
    ///
    /// ### out
    /// - rax: dst
    ///
    /// ### destroy
    /// - caller save registers
    /// - stack
    ///
    GenericBinOp {
        kind: BinOpK,
        using_xmm: UsingXmm,
    },*/
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
    IntegerExp {
        using_xmm: UsingXmm,
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
        mode: FMode,
    },
    FloatCmpBr {
        kind: CmpKind,
        mode: FMode,
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
    },

    /*///
    /// Generic index operation.
    ///
    /// Execute `base`[[`idx`]] and store the result to *rax*.
    ///
    /// ### out
    /// - rax: result Option<Value>
    ///
    /// ### destroy
    ///
    /// - caller save registers
    ///
    GenericIndex {
        base: SlotId,
        idx: SlotId,
        pc: BytecodePtr,
        using_xmm: UsingXmm,
    },*/
    ///
    /// Array index operation with u16 index `idx``.
    ///
    /// Execute *rdi*[[`idx`]] and store the result to *rax*.
    ///
    /// ### in
    /// - rdi: base Array
    ///
    /// ### out
    /// - rax: result Value
    ///
    ArrayU16Index {
        idx: u16,
    },
    ///
    /// Array index operation.
    ///
    /// ### in
    /// - rdi: base Array
    /// - rsi: index Fixnum
    ///
    /// ### out
    /// - rax: result Value
    ///
    ArrayIndex,

    ///
    /// Generic index assign operation.
    ///
    /// Execute `base`[[`idx`]] = `src`.
    ///
    /// ### out
    /// - rax: result Option<Value>
    ///    
    /// ### destroy
    /// - caller save registers
    ///
    GenericIndexAssign {
        src: SlotId,
        base: SlotId,
        idx: SlotId,
        pc: BytecodePtr,
        using_xmm: UsingXmm,
    },
    ///
    /// Array index assign operation with u16 index `idx`.
    ///
    /// ### in
    /// - rdi: base: Array
    /// - rdx: result Value
    ///
    /// ### destroy
    /// - caller save registers
    ///
    ArrayU16IndexAssign {
        idx: u16,
        using_xmm: UsingXmm,
        error: AsmError,
    },
    ///
    /// Aray index assign operation.
    ///
    /// ### in
    /// - rdi: base Array
    /// - rsi: index Fixnum
    /// - rdx: Value
    ///    
    /// ### destroy
    /// - caller save registers
    ///
    ArrayIndexAssign {
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
    /// Create a new Array object and store it to *rax*
    ///
    NewLambda(FuncId, UsingXmm),
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
        outer: usize,
        using_xmm: UsingXmm,
        error: AsmError,
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

    /// rax = DynVar(src)
    LoadDynVar {
        src: DynVar,
    },
    /// DynVar(dst) = src
    StoreDynVar {
        dst: DynVar,
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
    LoadSVar {
        id: u32,
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
    },
    SingletonMethodDef {
        obj: SlotId,
        name: IdentId,
        func_id: FuncId,
        using_xmm: UsingXmm,
    },

    ExpandArray {
        dst: SlotId,
        len: usize,
        rest_pos: Option<usize>,
        using_xmm: UsingXmm,
    },

    UndefMethod {
        undef: IdentId,
        using_xmm: UsingXmm,
    },
    AliasMethod {
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
    #[cfg(feature = "emit-asm")]
    BcIndex(BcIndex),
    Label(JitLabel),
}

impl AsmInst {
    /*pub fn dump(&self, store: &Store) -> String {
        match self {
            Self::AccToStack(slot) => format!("{:?} = R15", slot),
            Self::RegToAcc(gpr) => format!("R15 = {:?}", gpr),
            Self::RegToStack(gpr, slot) => format!("{:?} = {:?}", slot, gpr),
            Self::StackToReg(slot, gpr) => format!("{:?} = {:?}", gpr, slot),
            Self::LitToReg(val, gpr) => format!("{:?} = {}", gpr, val.debug(store)),
            Self::I32ToReg(i, gpr) => format!("{:?} = {i}", gpr),
            Self::RegMove(src, dst) => format!("{:?} = {:?}", dst, src),
            Self::RegAdd(gpr, i) => format!("{:?} += {i}", gpr),
            Self::RegSub(gpr, i) => format!("{:?} -= {i}", gpr,),
            Self::RegToRSPOffset(gpr, offset) => format!("RSP[{offset}] = {:?}", gpr),
            Self::I32ToRSPOffset(i, offset) => format!("RSP[{offset}] = {i}"),
            Self::XmmMove(src, dst) => format!("{:?} = {:?}", dst, src),
            Self::XmmSwap(fp1, fp2) => format!("{:?} <-> {:?}", fp1, fp2),
            Self::XmmBinOp {
                kind,
                mode,
                dst,
                using_xmm,
            } => format!("{:?} = {:?} {:?}  {:?}", dst, kind, mode, using_xmm),
            Self::XmmUnOp { kind, dst } => format!("{:?} = {:?} {:?}", dst, kind, dst),

            Self::F64ToXmm(f, dst) => format!("{:?} = {}", dst, f),
            Self::I64ToBoth(i, slot, xmm) => format!("{:?}:{:?} = {i}", slot, xmm),
            Self::XmmToStack(fpr, slots) => format!("{:?} = {:?}", slots, fpr),
            Self::LitToStack(val, slot) => format!("{:?} = {}", slot, val.debug(store)),
            Self::DeepCopyLit(val, _using_xmm) => format!("DeepCopyLiteral {}", val.debug(store)),
            Self::NumToXmm(gpr, fpr, _deopt) => format!("{:?} = {:?} Numeric to f64", fpr, gpr),
            Self::IntToXmm(gpr, fpr, _deopt) => format!("{:?} = {:?} Integer to f64", fpr, gpr),
            Self::FloatToXmm(gpr, fpr, _deopt) => format!("{:?} = {:?} Float to f64", fpr, gpr),
            Self::GuardFloat(gpr, _deopt) => format!("Guard Float {:?}", gpr),
            Self::GuardFixnum(gpr, _deopt) => format!("Guard Fixnum {:?}", gpr),
            Self::GuardArrayTy(gpr, _deopt) => format!("Guard ArrayTy {:?}", gpr),

            Self::GuardClassVersion(fid, version, _callid, _using_xmm, _deopt, _error) => {
                format!("GuardVersion fid={:?} version={version}", fid)
            }
            Self::GuardClass(gpr, class, _deopt) => format!("GuardClass {:?} {:?}", class, gpr),
            Self::Ret => "ret".to_string(),
            Self::BlockBreak => "break".to_string(),
            Self::Raise => "raise".to_string(),
            Self::MethodRet(_pc) => format!("method_return"),
            Self::EnsureEnd => "ensure_end".to_string(),

            Self::Br(label) => format!("br {:?}", label),
            Self::CondBr(kind, label) => format!("condbr {:?} {:?}", kind, label),
            Self::NilBr(label) => format!("nil_br {:?}", label),
            Self::CheckLocal(label) => format!("check_local {:?}", label),
            Self::GenericCondBr {
                brkind,
                branch_dest,
            } => format!("condbr {:?} {:?}", brkind, branch_dest),
            Self::OptCase {
                max,
                min,
                branch_table,
                else_dest,
            } => format!(
                "opt_case {:?}..{:?} {:?} else {:?}",
                min, max, branch_table, else_dest
            ),
            Self::Deopt(deopt) => format!("deopt {:?}", deopt),
            Self::RecompileDeopt { position, deopt } => {
                format!("recompile_deopt {:?} {:?}", position, deopt)
            }
            Self::WriteBack(wb) => format!("write_back {:?}", wb),
            Self::HandleError(error) => format!("handle_error {:?}", error),
            Self::XmmSave(using_xmm) => format!("xmm_save {:?}", using_xmm),
            Self::XmmRestore(using_xmm) => format!("xmm_restore {:?}", using_xmm),
            Self::ExecGc(wb) => format!("exec_gc {:?}", wb),
            Self::LoadGenericConstant {
                cached_val,
                cached_version,
                deopt: _,
            } => format!(
                "load_generic_constant {} {}",
                cached_val.debug(store),
                cached_version
            ),
            _ => format!("{:?}", self),
        }
    }*/
}

#[derive(Clone, Debug)]
pub(super) enum FMode {
    RR(Xmm, Xmm),
    RI(Xmm, i16),
    IR(i16, Xmm),
}

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
        ctx: &mut JitContext,
        entry: Option<DestLabel>,
        exit: Option<BasicBlockId>,
    ) {
        let mut side_exits = SideExitLabels::new();
        let mut deopt_table: HashMap<(BytecodePtr, WriteBack), DestLabel> = HashMap::default();
        for side_exit in ir.side_exit {
            let label = match side_exit {
                SideExit::Evict(Some((pc, wb))) => {
                    let label = self.jit.label();
                    self.gen_evict_with_label(pc, &wb, label.clone());
                    label
                }
                SideExit::Deoptimize(pc, wb) => {
                    let t = (pc, wb);
                    if let Some(label) = deopt_table.get(&t) {
                        label.clone()
                    } else {
                        let label = self.jit.label();
                        self.gen_deopt_with_label(pc, &t.1, label.clone());
                        deopt_table.insert(t, label.clone());
                        label
                    }
                }
                SideExit::Error(pc, wb) => {
                    let label = self.jit.label();
                    self.gen_handle_error(pc, wb, label.clone());
                    label
                }
                _ => unreachable!(),
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
            if let AsmInst::BcIndex(i) = &inst {
                ctx.sourcemap
                    .push((*i, self.jit.get_current() - ctx.start_codepos));
            }
            self.compile_asmir(store, ctx, &side_exits, inst);
        }

        if let Some(exit) = exit {
            let exit = ctx.get_bb_label(exit);
            let exit = ctx.resolve_label(&mut self.jit, exit);
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
