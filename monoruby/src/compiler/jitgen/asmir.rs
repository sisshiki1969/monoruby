use crate::bytecodegen::BinOpK;

use super::*;

mod binary_op;
mod compile;
mod definition;
mod index;
mod merge;
mod method_call;
mod read_slot;
pub mod slot;
mod variables;

// ~~~text
// MethodCall
//  0   2   4   6    8  10  12  14
// +---+---+---+---++---+---+---+---+
// |callid |ret| op||  fid  |   -   |
// +---+---+---+---++---+---+---+---+
// InlineCache
// 16  18  20  22   24  26  28  30
// +---+---+---+---++---+---+---+---+
// |pos|arg|rcv| op|| class |version|
// +---+---+---+---++---+---+---+---+
// ~~~

const BC_OFFSET_CACHED_CLASS: usize = 24;
const BC_OFFSET_CACHED_VERSION: usize = 28;
const BC_OFFSET_CACHED_FUNCID: usize = 8;

pub(super) struct InlineProcedure {
    proc: Box<dyn FnOnce(&mut Codegen, &SideExitLabels)>,
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

// public interface
impl AsmIr {
    pub(crate) fn new_deopt(&mut self, bb: &BBContext, pc: BytecodePtr) -> AsmDeopt {
        let i = self.new_label(SideExit::Deoptimize(pc, bb.get_write_back()));
        AsmDeopt(i)
    }

    pub(crate) fn new_error(&mut self, bb: &BBContext, pc: BytecodePtr) -> AsmError {
        let i = self.new_label(SideExit::Error(pc, bb.get_write_back()));
        AsmError(i)
    }

    pub(crate) fn new_evict(&mut self) -> AsmEvict {
        let i = self.new_label(SideExit::Evict(None));
        AsmEvict(i)
    }

    pub(crate) fn new_deopt_error(
        &mut self,
        bb: &BBContext,
        pc: BytecodePtr,
    ) -> (AsmDeopt, AsmError) {
        let wb = bb.get_write_back();
        let deopt = self.new_label(SideExit::Deoptimize(pc, wb.clone()));
        let error = self.new_label(SideExit::Error(pc, wb));
        (AsmDeopt(deopt), AsmError(error))
    }

    pub(crate) fn rax2acc(&mut self, bb: &mut BBContext, dst: impl Into<Option<SlotId>>) {
        self.reg2acc(bb, GP::Rax, dst);
    }

    pub(crate) fn reg2acc(&mut self, bb: &mut BBContext, src: GP, dst: impl Into<Option<SlotId>>) {
        self.reg2acc_guarded(bb, src, dst, slot::Guarded::Value)
    }

    pub(crate) fn reg2acc_fixnum(
        &mut self,
        bb: &mut BBContext,
        src: GP,
        dst: impl Into<Option<SlotId>>,
    ) {
        self.reg2acc_guarded(bb, src, dst, slot::Guarded::Fixnum)
    }

    pub(crate) fn reg2acc_guarded(
        &mut self,
        bb: &mut BBContext,
        src: GP,
        dst: impl Into<Option<SlotId>>,
        guarded: slot::Guarded,
    ) {
        if let Some(dst) = dst.into() {
            self.clear(bb);
            if let Some(acc) = self.clear_r15(bb)
                && acc < bb.sp
                && acc != dst
            {
                self.acc2stack(acc);
            }
            self.store_r15(bb, dst, guarded);
            self.inst.push(AsmInst::RegToAcc(src));
        }
    }
}

impl AsmIr {
    pub(super) fn new() -> Self {
        Self {
            inst: vec![],
            side_exit: vec![],
        }
    }

    fn new_label(&mut self, side_exit: SideExit) -> usize {
        let label = self.side_exit.len();
        self.side_exit.push(side_exit);
        label
    }

    pub(super) fn deopt(&mut self, bb: &BBContext, pc: BytecodePtr) {
        let exit = self.new_deopt(bb, pc);
        self.inst.push(AsmInst::Deopt(exit));
    }

    pub(super) fn check_bop(&mut self, bb: &BBContext, pc: BytecodePtr) {
        let deopt = self.new_deopt(bb, pc);
        self.inst.push(AsmInst::CheckBOP { deopt });
    }

    pub(super) fn recompile_and_deopt(
        &mut self,
        bb: &BBContext,
        pc: BytecodePtr,
        position: Option<BytecodePtr>,
    ) {
        let deopt = self.new_deopt(bb, pc);
        self.inst.push(AsmInst::RecompileDeopt { position, deopt });
    }

    pub(super) fn xmm_save(&mut self, using_xmm: UsingXmm) {
        self.inst.push(AsmInst::XmmSave(using_xmm));
    }
    pub(super) fn exec_gc(&mut self, wb: WriteBack) {
        self.inst.push(AsmInst::ExecGc(wb));
    }

    fn reg_move(&mut self, src: GP, dst: GP) {
        if src != dst {
            self.inst.push(AsmInst::RegMove(src, dst));
        }
    }

    fn reg_add(&mut self, r: GP, i: i32) {
        self.inst.push(AsmInst::RegAdd(r, i));
    }

    fn reg_sub(&mut self, r: GP, i: i32) {
        self.inst.push(AsmInst::RegSub(r, i));
    }

    fn reg2rsp_offset(&mut self, r: GP, i: i32) {
        self.inst.push(AsmInst::RegToRSPOffset(r, i));
    }

    pub(crate) fn reg2stack(&mut self, src: GP, dst: impl Into<Option<SlotId>>) {
        if let Some(dst) = dst.into() {
            self.inst.push(AsmInst::RegToStack(src, dst));
        }
    }

    pub(crate) fn stack2reg(&mut self, src: SlotId, dst: GP) {
        self.inst.push(AsmInst::StackToReg(src, dst));
    }

    pub(super) fn xmm_move(&mut self, src: Xmm, dst: Xmm) {
        self.inst.push(AsmInst::XmmMove(src, dst));
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
        self.inst.push(AsmInst::XmmBinOp {
            kind,
            mode,
            dst,
            using_xmm,
        });
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
    fn xmm2stack(&mut self, xmm: Xmm, reg: Vec<SlotId>) {
        self.inst.push(AsmInst::XmmToStack(xmm, reg));
    }

    fn lit2reg(&mut self, v: Value, reg: GP) {
        self.inst.push(AsmInst::LitToReg(v, reg));
    }

    fn acc2stack(&mut self, reg: SlotId) {
        self.inst.push(AsmInst::AccToStack(reg));
    }

    fn int2xmm(&mut self, reg: GP, x: Xmm, deopt: AsmDeopt) {
        self.inst.push(AsmInst::IntToXmm(reg, x, deopt));
    }

    fn float2xmm(&mut self, reg: GP, x: Xmm, deopt: AsmDeopt) {
        self.inst.push(AsmInst::FloatToXmm(reg, x, deopt));
    }

    fn f64toxmm(&mut self, f: f64, x: Xmm) {
        self.inst.push(AsmInst::F64ToXmm(f, x));
    }

    fn i64toboth(&mut self, i: i64, reg: SlotId, x: Xmm) {
        self.inst.push(AsmInst::I64ToBoth(i, reg, x));
    }

    /// rax = val
    pub(super) fn deep_copy_lit(&mut self, bb: &BBContext, val: Value) {
        let using_xmm = bb.get_using_xmm();
        self.inst.push(AsmInst::DeepCopyLit(val, using_xmm));
    }

    fn guard_fixnum(&mut self, r: GP, deopt: AsmDeopt) {
        self.inst.push(AsmInst::GuardFixnum(r, deopt));
    }

    fn guard_float(&mut self, r: GP, deopt: AsmDeopt) {
        self.inst.push(AsmInst::GuardFloat(r, deopt));
    }

    fn guard_array_ty(&mut self, r: GP, deopt: AsmDeopt) {
        self.inst.push(AsmInst::GuardArrayTy(r, deopt));
    }

    ///
    /// Class version guard for JIT.
    ///
    /// Check the cached class version, and if the version is changed, call `find_method` and
    /// compare obtained FuncId and cached FuncId.
    /// If different, jump to `deopt`.
    /// If identical, update the cached version and go on.
    ///
    /// ### in
    /// - rdi: receiver: Value
    ///
    /// ### out
    /// - rdi: receiver: Value
    ///
    /// ### destroy
    /// - caller save registers
    /// - stack
    ///
    pub(super) fn guard_version(
        &mut self,
        cached_fid: FuncId,
        cached_version: u32,
        callid: CallSiteId,
        using_xmm: UsingXmm,
        deopt: AsmDeopt,
        error: AsmError,
    ) {
        self.inst.push(AsmInst::GuardClassVersion(
            cached_fid,
            cached_version,
            callid,
            using_xmm,
            deopt,
            error,
        ));
    }

    ///
    /// Type guard.
    ///
    /// Generate type guard for *class_id*.
    /// If the type was not matched, go to *deopt*.
    ///
    /// ### in
    /// - R(*reg*): Value
    ///
    pub(crate) fn guard_class(
        &mut self,
        bb: &mut BBContext,
        slot: SlotId,
        r: GP,
        class: ClassId,
        deopt: AsmDeopt,
    ) {
        match class {
            INTEGER_CLASS => {
                if bb.is_fixnum(slot) {
                    return;
                }
                bb.set_guard_fixnum(slot);
            }
            FLOAT_CLASS => {
                if bb.is_float(slot) {
                    return;
                }
                bb.set_guard_float(slot);
            }
            class => {
                if bb.is_class(slot, class) {
                    return;
                }
                bb.set_guard_class(slot, class);
            }
        }
        self.inst.push(AsmInst::GuardClass(r, class, deopt));
    }

    pub(super) fn opt_case(
        &mut self,
        max: u16,
        min: u16,
        else_dest: BasicBlockId,
        branch_table: Box<[BasicBlockId]>,
    ) {
        self.inst.push(AsmInst::OptCase {
            max,
            min,
            else_dest,
            branch_table,
        });
    }

    pub(super) fn handle_error(&mut self, error: AsmError) {
        self.inst.push(AsmInst::HandleError(error));
    }
}

// write back operations
impl AsmIr {
    pub(super) fn write_back_slots(&mut self, bb: &mut BBContext, slot: &[SlotId]) {
        slot.iter().for_each(|r| self.write_back_slot(bb, *r));
    }

    ///
    /// Fetch from *args* to *args* + *len* - 1 and store in corresponding stack slots.
    ///
    pub(super) fn write_back_range(&mut self, bb: &mut BBContext, args: SlotId, len: u16) {
        for reg in args.0..args.0 + len {
            self.write_back_slot(bb, SlotId::new(reg))
        }
    }

    pub(crate) fn write_back_callargs_and_dst(
        &mut self,
        bb: &mut BBContext,
        callsite: &CallSiteInfo,
    ) {
        let CallSiteInfo { recv, dst, .. } = callsite;
        self.write_back_slot(bb, *recv);
        self.write_back_args(bb, callsite);
        self.unlink(bb, *dst);
    }

    fn write_back_args(&mut self, bb: &mut BBContext, callsite: &CallSiteInfo) {
        let CallSiteInfo {
            args,
            pos_num,
            kw_pos,
            block_arg,
            ..
        } = callsite;
        self.write_back_range(bb, *args, *pos_num as u16);
        self.write_back_range(bb, *kw_pos, callsite.kw_len() as u16);
        if let Some(block_arg) = block_arg {
            self.write_back_slot(bb, *block_arg);
        }
    }
}

impl AsmIr {
    ///
    /// Set positional arguments for callee.
    ///
    fn set_arguments(
        &mut self,
        store: &Store,
        bb: &mut BBContext,
        callid: CallSiteId,
        callee_fid: FuncId,
        pc: BytecodePtr,
    ) {
        let caller = &store[callid];
        let callee = &store[callee_fid];
        let args = caller.args;
        let pos_num = caller.pos_num;
        let kw_pos = caller.kw_pos;
        let kw_num = caller.kw_len();
        let single_arg_expand = pos_num == 1 && callee.single_arg_expand();
        let ex_positional = callee.no_keyword() && caller.kw_may_exists();
        if !caller.has_splat()
            && !caller.has_hash_splat()
            && !ex_positional
            && !single_arg_expand
            && !callee.is_rest()
            && pos_num <= callee.max_positional_args()
            && callee.req_num() <= pos_num
        {
            // write back keyword arguments.
            for arg in kw_pos..kw_pos + kw_num {
                self.write_back_slot(bb, arg);
            }
            // write back block argument.
            if let Some(block_arg) = caller.block_arg {
                self.write_back_slot(bb, block_arg);
            }
            let ofs = if (args..args + pos_num).any(|reg| matches!(bb.slot(reg), LinkMode::Xmm(_)))
            {
                (RSP_LOCAL_FRAME + LFP_ARG0 + (8 * pos_num) as i32 + 8) & !0xf
            } else {
                0
            };

            self.reg_sub(GP::Rsp, ofs);
            for i in 0..pos_num {
                let reg = args + i;
                let offset = ofs - (RSP_LOCAL_FRAME + LFP_ARG0 + (8 * i) as i32);
                self.fetch_to_rsp_offset(bb, reg, offset);
            }
            if pos_num != callee.max_positional_args() {
                self.inst.push(AsmInst::I32ToReg(0, GP::Rax));
                for i in pos_num..callee.max_positional_args() {
                    let offset = ofs - (RSP_LOCAL_FRAME + LFP_ARG0 as i32 + (8 * i) as i32);
                    self.reg2rsp_offset(GP::Rax, offset);
                }
            }
            self.reg_add(GP::Rsp, ofs);
        } else {
            self.write_back_args(bb, caller);

            let error = self.new_error(bb, pc);
            self.inst.push(AsmInst::SetArguments { callid, callee_fid });
            self.handle_error(error);
        }
    }

    pub(super) fn generic_unop(&mut self, bb: &BBContext, pc: BytecodePtr, func: UnaryOpFn) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::GenericUnOp { func, using_xmm });
        self.handle_error(error);
    }

    ///
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
    pub(super) fn generic_binop(&mut self, bb: &BBContext, pc: BytecodePtr, kind: BinOpK) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::GenericBinOp { kind, using_xmm });
        self.handle_error(error);
    }

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
        bb: &BBContext,
        pc: BytecodePtr,
        kind: BinOpK,
        mode: OpMode,
    ) {
        let using_xmm = bb.get_using_xmm();
        let (deopt, error) = self.new_deopt_error(bb, pc);
        self.inst.push(AsmInst::IntegerBinOp {
            kind,
            mode,
            using_xmm,
            deopt,
            error,
        });
    }

    pub(super) fn generic_cmp(&mut self, bb: &BBContext, pc: BytecodePtr, kind: CmpKind) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::GenericCmp { kind, using_xmm });
        self.handle_error(error);
    }

    pub(super) fn integer_cmp_br(
        &mut self,
        mode: OpMode,
        kind: CmpKind,
        brkind: BrKind,
        branch_dest: DestLabel,
    ) {
        self.inst.push(AsmInst::IntegerCmpBr {
            mode,
            kind,
            brkind,
            branch_dest,
        });
    }

    pub(super) fn float_cmp_br(
        &mut self,
        mode: FMode,
        kind: CmpKind,
        brkind: BrKind,
        branch_dest: DestLabel,
    ) {
        self.inst.push(AsmInst::FloatCmpBr {
            mode,
            kind,
            brkind,
            branch_dest,
        });
    }

    pub(super) fn generic_index(
        &mut self,
        bb: &BBContext,
        base: SlotId,
        idx: SlotId,
        pc: BytecodePtr,
    ) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::GenericIndex {
            base,
            idx,
            pc,
            using_xmm,
        });
        self.handle_error(error);
    }

    pub(super) fn array_u16_index(&mut self, idx: u16) {
        self.inst.push(AsmInst::ArrayU16Index { idx });
    }

    pub(super) fn array_index(&mut self) {
        self.inst.push(AsmInst::ArrayIndex);
    }

    pub(super) fn generic_index_assign(
        &mut self,
        bb: &BBContext,
        pc: BytecodePtr,
        base: SlotId,
        idx: SlotId,
        src: SlotId,
    ) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::GenericIndexAssign {
            src,
            base,
            idx,
            pc,
            using_xmm,
        });
        self.handle_error(error);
    }

    pub(super) fn array_u16_index_assign(&mut self, bb: &BBContext, idx: u16, pc: BytecodePtr) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::ArrayU16IndexAssign {
            idx,
            using_xmm,
            error,
        });
    }

    pub(super) fn array_index_assign(&mut self, bb: &BBContext, pc: BytecodePtr) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst
            .push(AsmInst::ArrayIndexAssign { using_xmm, error });
    }

    pub(super) fn new_array(&mut self, bb: &BBContext, callid: CallSiteId) {
        let using_xmm = bb.get_using_xmm();
        self.inst.push(AsmInst::NewArray { callid, using_xmm });
    }

    pub(super) fn new_lambda(&mut self, bb: &BBContext, func_id: FuncId) {
        let using_xmm = bb.get_using_xmm();
        self.inst.push(AsmInst::NewLambda(func_id, using_xmm));
    }

    pub(super) fn new_hash(&mut self, bb: &BBContext, args: SlotId, len: usize) {
        let using_xmm = bb.get_using_xmm();
        self.inst.push(AsmInst::NewHash(args, len, using_xmm));
    }

    pub(super) fn new_range(
        &mut self,
        bb: &BBContext,
        pc: BytecodePtr,
        start: SlotId,
        end: SlotId,
        exclude_end: bool,
    ) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::NewRange {
            start,
            end,
            exclude_end,
            using_xmm,
        });
        self.handle_error(error);
    }

    pub(super) fn block_arg_proxy(&mut self, ret: SlotId, outer: usize) {
        self.inst.push(AsmInst::BlockArgProxy { ret, outer });
    }

    pub(super) fn block_arg(&mut self, bb: &BBContext, pc: BytecodePtr, ret: SlotId, outer: usize) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::BlockArg {
            ret,
            outer,
            using_xmm,
            error,
        });
    }

    pub(super) fn load_gvar(&mut self, bb: &BBContext, name: IdentId) {
        let using_xmm = bb.get_using_xmm();
        self.inst.push(AsmInst::LoadGVar { name, using_xmm });
    }

    pub(super) fn store_gvar(&mut self, bb: &BBContext, name: IdentId, src: SlotId) {
        let using_xmm = bb.get_using_xmm();
        self.inst.push(AsmInst::StoreGVar {
            name,
            src,
            using_xmm,
        });
    }

    pub(super) fn load_cvar(&mut self, bb: &BBContext, pc: BytecodePtr, name: IdentId) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::LoadCVar { name, using_xmm });
        self.handle_error(error);
    }

    pub(super) fn check_cvar(&mut self, bb: &BBContext, name: IdentId) {
        let using_xmm = bb.get_using_xmm();
        self.inst.push(AsmInst::CheckCVar { name, using_xmm });
    }

    pub(super) fn store_cvar(
        &mut self,
        bb: &BBContext,
        pc: BytecodePtr,
        name: IdentId,
        src: SlotId,
    ) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::StoreCVar {
            name,
            src,
            using_xmm,
        });
        self.handle_error(error);
    }

    pub(super) fn load_svar(&mut self, bb: &BBContext, id: u32) {
        let using_xmm = bb.get_using_xmm();
        self.inst.push(AsmInst::LoadSVar { id, using_xmm });
    }

    pub(super) fn concat_str(&mut self, bb: &BBContext, arg: SlotId, len: u16) {
        let using_xmm = bb.get_using_xmm();
        self.inst.push(AsmInst::ConcatStr {
            arg,
            len,
            using_xmm,
        });
    }

    pub(super) fn concat_regexp(&mut self, bb: &BBContext, arg: SlotId, len: u16) {
        let using_xmm = bb.get_using_xmm();
        self.inst.push(AsmInst::ConcatRegexp {
            arg,
            len,
            using_xmm,
        });
    }

    pub(super) fn expand_array(&mut self, bb: &BBContext, dst: SlotId, len: u16) {
        let using_xmm = bb.get_using_xmm();
        let len = len as _;
        self.inst.push(AsmInst::ExpandArray {
            dst,
            len,
            using_xmm,
        });
    }

    pub(super) fn alias_method(
        &mut self,
        bb: &BBContext,
        pc: BytecodePtr,
        new: IdentId,
        old: IdentId,
    ) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::AliasMethod {
            new,
            old,
            using_xmm,
        });
        self.handle_error(error);
    }

    pub(crate) fn inline(&mut self, f: impl FnOnce(&mut Codegen, &SideExitLabels) + 'static) {
        self.inst
            .push(AsmInst::Inline(InlineProcedure { proc: Box::new(f) }));
    }

    pub(crate) fn bc_index(&mut self, index: BcIndex) {
        self.inst.push(AsmInst::BcIndex(index));
    }

    pub(super) fn jit_load_gvar(&mut self, bb: &mut BBContext, name: IdentId, dst: SlotId) {
        self.unlink(bb, dst);
        self.load_gvar(bb, name);
        self.rax2acc(bb, dst);
    }

    pub(super) fn jit_store_gvar(&mut self, bb: &mut BBContext, name: IdentId, src: SlotId) {
        self.write_back_slots(bb, &[src]);
        self.store_gvar(bb, name, src);
    }

    pub(super) fn jit_load_cvar(
        &mut self,
        bb: &mut BBContext,
        pc: BytecodePtr,
        name: IdentId,
        dst: SlotId,
    ) {
        self.unlink(bb, dst);
        self.load_cvar(bb, pc, name);
        self.rax2acc(bb, dst);
    }

    pub(super) fn jit_check_cvar(&mut self, bb: &mut BBContext, name: IdentId, dst: SlotId) {
        self.unlink(bb, dst);
        self.check_cvar(bb, name);
        self.rax2acc(bb, dst);
    }

    pub(super) fn jit_store_cvar(
        &mut self,
        bb: &mut BBContext,
        pc: BytecodePtr,
        name: IdentId,
        src: SlotId,
    ) {
        self.write_back_slots(bb, &[src]);
        self.store_cvar(bb, pc, name, src);
    }
}

impl AsmIr {
    pub(super) fn fetch_binary(&mut self, bb: &mut BBContext, mode: OpMode) {
        match mode {
            OpMode::RR(lhs, rhs) => {
                self.fetch_to_reg(bb, lhs, GP::Rdi);
                self.fetch_to_reg(bb, rhs, GP::Rsi);
            }
            OpMode::RI(lhs, rhs) => {
                self.fetch_to_reg(bb, lhs, GP::Rdi);
                self.lit2reg(Value::i32(rhs as i32), GP::Rsi);
            }
            OpMode::IR(lhs, rhs) => {
                self.lit2reg(Value::i32(lhs as i32), GP::Rdi);
                self.fetch_to_reg(bb, rhs, GP::Rsi);
            }
        }
    }

    pub(super) fn fmode(
        &mut self,
        mode: &OpMode,
        bb: &mut BBContext,
        lhs_class: ClassId,
        rhs_class: ClassId,
        deopt: AsmDeopt,
    ) -> FMode {
        match mode {
            OpMode::RR(l, r) => {
                let (flhs, frhs) = self.fetch_float_binary(bb, *l, *r, lhs_class, rhs_class, deopt);
                FMode::RR(flhs, frhs)
            }
            OpMode::RI(l, r) => {
                let l = self.fetch_float_assume_float(bb, *l, deopt);
                FMode::RI(l, *r)
            }
            OpMode::IR(l, r) => {
                let r = self.fetch_float_assume_float(bb, *r, deopt);
                FMode::IR(*l, r)
            }
        }
    }

    pub(super) fn write_back_locals(&mut self, bb: &mut BBContext) {
        let wb = bb.get_locals_write_back();
        self.inst.push(AsmInst::WriteBack(wb));
        self.release_locals(bb);
    }
}

#[derive(Debug)]
pub(super) enum AsmInst {
    /// move acc to stack
    AccToStack(SlotId),
    /// move reg to acc
    RegToAcc(GP),
    /// move reg to stack
    RegToStack(GP, SlotId),
    /// move reg to stack
    StackToReg(SlotId, GP),
    LitToReg(Value, GP),
    I32ToReg(i32, GP),
    /// move reg to reg
    RegMove(GP, GP),
    RegAdd(GP, i32),
    RegSub(GP, i32),
    RegToRSPOffset(GP, i32),

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

    /// move f64 to xmm
    F64ToXmm(f64, Xmm),
    /// move i64 to both of xmm and a stack slot
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
    XmmToStack(Xmm, Vec<SlotId>),
    ///
    /// ### destroy
    /// - rax
    ///
    LitToStack(Value, SlotId),
    DeepCopyLit(Value, UsingXmm),
    NumToXmm(GP, Xmm, AsmDeopt),
    IntToXmm(GP, Xmm, AsmDeopt),
    /// move a Flonum Value in a reg to xmm reg, and deoptimize if it is not a Flonum.
    FloatToXmm(GP, Xmm, AsmDeopt),

    /// check whether a Value in a stack slot is a Flonum, and if not, deoptimize.
    GuardFloat(GP, AsmDeopt),
    GuardFixnum(GP, AsmDeopt),
    GuardArrayTy(GP, AsmDeopt),
    ///
    /// Class version guard fro JIT.
    ///
    /// Check the cached class version, and if the version is changed, call `find_method` and
    /// compare obtained FuncId and cached FuncId.
    /// If different, jump to `deopt`.
    /// If identical, update the cached version and go on.
    ///    
    /// ### in
    /// - rdi: receiver: Value
    ///
    /// ### out
    /// - rdi: receiver: Value
    ///
    /// ### destroy
    /// - caller save registers
    /// - stack
    ///
    GuardClassVersion(FuncId, u32, CallSiteId, UsingXmm, AsmDeopt, AsmError),
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

    Ret,
    BlockBreak,
    Raise,
    MethodRet(BytecodePtr),
    EnsureEnd,
    Br(DestLabel),
    CondBr(BrKind, DestLabel),
    NilBr(DestLabel),
    CheckLocal(DestLabel),
    ///
    /// Conditional branch
    ///
    /// When *kind* is BrKind::BrIf, jump to *dst* if the Value in `rax` is true.
    ///
    /// ### in
    /// - rax: Value
    ///
    GenericCondBr {
        brkind: BrKind,
        branch_dest: DestLabel,
    },
    OptCase {
        max: u16,
        min: u16,
        else_dest: BasicBlockId,
        branch_table: Box<[BasicBlockId]>,
    },

    /// deoptimize
    Deopt(AsmDeopt),
    /// recompile and deoptimize
    RecompileDeopt {
        position: Option<BytecodePtr>,
        deopt: AsmDeopt,
    },
    WriteBack(WriteBack),
    HandleError(AsmError),
    XmmSave(UsingXmm),
    ExecGc(WriteBack),
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
    /// Attribute writer
    ///
    /// ### in
    /// - rdi: receiver: Value
    /// - rdx: value: Value
    ///
    AttrWriter {
        ivar_id: IvarId,
        using_xmm: UsingXmm,
        error: AsmError,
    },
    ///
    /// Attribute reader
    ///
    /// ### in
    /// - rdi: receiver: Value
    ///
    AttrReader {
        ivar_id: IvarId,
    },
    ///
    /// Send cached method
    ///
    /// ### in
    /// - rdi: receiver: Value
    ///
    /// ### destroy
    /// - caller save registers
    /// - r15
    ///
    SendCached {
        callid: CallSiteId,
        recv_class: ClassId,
        callee_fid: FuncId,
        using_xmm: UsingXmm,
        error: AsmError,
        evict: AsmEvict,
    },
    ///
    /// Send non-cached method
    ///
    /// ### destroy
    /// - caller save registers
    /// - r15
    ///
    SendNotCached {
        callid: CallSiteId,
        self_class: ClassId,
        pc: BytecodePtr,
        using_xmm: UsingXmm,
        error: AsmError,
        evict: AsmEvict,
    },
    Inline(InlineProcedure),
    Yield {
        callid: CallSiteId,
        using_xmm: UsingXmm,
        error: AsmError,
        evict: AsmEvict,
    },
    ImmediateEvict {
        evict: AsmEvict,
    },
    CheckBOP {
        deopt: AsmDeopt,
    },

    Not,
    GenericUnOp {
        func: UnaryOpFn,
        using_xmm: UsingXmm,
    },

    ///
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
        mode: OpMode,
        using_xmm: UsingXmm,
        deopt: AsmDeopt,
        error: AsmError,
    },

    ///
    /// Generic comparison
    ///
    /// Compare two Values in `rdi` and `rsi` with *kind*, and return the result in `rax` as Value.
    ///
    /// If error occurs in comparison operation, raise error.
    ///
    ///
    GenericCmp {
        kind: CmpKind,
        using_xmm: UsingXmm,
    },
    ///
    /// Integer comparison
    ///
    /// Compare two Values in `rdi` and `rsi` with *kind*, and return the result in `rax` as Value.
    ///
    /// If error occurs in comparison operation, raise error.
    ///
    IntegerCmp {
        mode: OpMode,
        kind: CmpKind,
    },
    ///
    /// Integer comparison and conditional branch
    ///
    /// Compare two values with *mode*, jump to *branch_dest* if the condition specified by *kind*
    /// and *brkind* is met.
    ///
    IntegerCmpBr {
        mode: OpMode,
        kind: CmpKind,
        brkind: BrKind,
        branch_dest: DestLabel,
    },
    FloatCmp {
        kind: CmpKind,
        mode: FMode,
    },
    FloatCmpBr {
        kind: CmpKind,
        mode: FMode,
        brkind: BrKind,
        branch_dest: DestLabel,
    },

    GuardBaseClass {
        base_class: Value,
        deopt: AsmDeopt,
    },
    LoadFloatConstant {
        fdst: Xmm,
        f: f64,
        cached_version: usize,
        deopt: AsmDeopt,
    },
    LoadGenericConstant {
        cached_val: Value,
        cached_version: usize,
        deopt: AsmDeopt,
    },
    StoreConstant {
        id: ConstSiteId,
        using_xmm: UsingXmm,
    },

    GenericIndex {
        base: SlotId,
        idx: SlotId,
        pc: BytecodePtr,
        using_xmm: UsingXmm,
    },
    ArrayU16Index {
        idx: u16,
    },
    ArrayIndex,
    GenericIndexAssign {
        src: SlotId,
        base: SlotId,
        idx: SlotId,
        pc: BytecodePtr,
        using_xmm: UsingXmm,
    },
    ArrayU16IndexAssign {
        idx: u16,
        using_xmm: UsingXmm,
        error: AsmError,
    },
    ArrayIndexAssign {
        using_xmm: UsingXmm,
        error: AsmError,
    },

    /// create a new Array object and store it to rax
    NewArray {
        callid: CallSiteId,
        using_xmm: UsingXmm,
    },
    /// create a new Array object and store it to rax
    NewLambda(FuncId, UsingXmm),
    /// create a new Hash object and store it to rax
    NewHash(SlotId, usize, UsingXmm),
    /// create a new Range object and store it to rax
    NewRange {
        start: SlotId,
        end: SlotId,
        exclude_end: bool,
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

    LoadIVar {
        name: IdentId,
        cached_ivarid: IvarId,
        is_object_ty: bool,
        is_self_cached: bool,
        using_xmm: UsingXmm,
    },
    StoreIVar {
        name: IdentId,
        cached_ivarid: IvarId,
        is_object_ty: bool,
        is_self_cached: bool,
        using_xmm: UsingXmm,
        error: AsmError,
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
        using_xmm: UsingXmm,
    },

    AliasMethod {
        new: IdentId,
        old: IdentId,
        using_xmm: UsingXmm,
    },
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

    #[allow(dead_code)]
    BcIndex(BcIndex),
    Label(DestLabel),
}

impl AsmInst {
    #[cfg(feature = "emit-asm")]
    pub(crate) fn dump(&self, store: &Store) -> String {
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
    }
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
        #[cfg(feature = "emit-asm")]
        for ir in &ir.inst {
            eprintln!("    {}", ir.dump(store));
        }
        let mut side_exits = SideExitLabels::new();
        for side_exit in ir.side_exit {
            let label = self.jit.label();
            side_exits.push(label);
            match side_exit {
                SideExit::Evict(Some((pc, wb))) => {
                    self.gen_evict_with_label(pc, &wb, label);
                }
                SideExit::Deoptimize(pc, wb) => {
                    self.gen_deopt_with_label(pc, &wb, label);
                }
                SideExit::Error(pc, wb) => {
                    self.gen_handle_error(pc, wb, label);
                }
                _ => {}
            }
        }

        if exit.is_some() {
            self.jit.select_page(1);
        }
        if let Some(entry) = entry {
            self.jit.bind_label(entry);
        }

        #[cfg(feature = "emit-asm")]
        let mut _sourcemap = vec![];

        for inst in ir.inst {
            #[cfg(feature = "emit-asm")]
            if let AsmInst::BcIndex(i) = &inst {
                _sourcemap.push((*i, self.jit.get_current()));
            }
            self.gen_asmir(store, ctx, &side_exits, inst);
        }

        if let Some(exit) = exit {
            let exit = *ctx.basic_block_labels.get(&exit).unwrap();
            monoasm! { &mut self.jit,
                jmp exit;
            }
            self.jit.select_page(0);
        }

        #[cfg(feature = "emit-asm")]
        if entry.is_none() {
            let map = _sourcemap
                .into_iter()
                .map(|(pc, pos)| (pc, pos - ctx.start_codepos));
            ctx.sourcemap.extend(map);
        }
    }

    ///
    /// Handle error.
    ///
    /// Check *rax*, and if it is 0, go to 'error'.
    ///
    pub(crate) fn handle_error(&mut self, error: DestLabel) {
        monoasm! { &mut self.jit,
            testq rax, rax;
            jeq   error;
        }
    }
}
