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

const BC_OFFSET_CALLSITE_ID: usize = 0;
const BC_OFFSET_CACHED_CLASS: usize = 24;
const BC_OFFSET_CACHED_VERSION: usize = 28;
const BC_OFFSET_CACHED_FUNCID: usize = 8;

#[derive(Debug, Clone, Copy)]
pub(crate) struct AsmDeopt(usize);

#[derive(Debug, Clone, Copy)]
pub(crate) struct AsmError(usize);

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

pub(crate) struct AsmIr {
    pub(super) inst: Vec<AsmInst>,
    pub(super) side_exit: Vec<SideExit>,
}

// public interface
impl AsmIr {
    pub(crate) fn new_deopt(&mut self, bb: &BBContext, pc: BcPc) -> AsmDeopt {
        let i = self.new_label(SideExit::Deoptimize(pc, bb.get_write_back()));
        AsmDeopt(i)
    }

    pub(crate) fn new_error(&mut self, bb: &BBContext, pc: BcPc) -> AsmError {
        let i = self.new_label(SideExit::Error(pc, bb.get_write_back()));
        AsmError(i)
    }

    pub(crate) fn new_deopt_error(&mut self, bb: &BBContext, pc: BcPc) -> (AsmDeopt, AsmError) {
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

    pub(super) fn deopt(&mut self, bb: &BBContext, pc: BcPc) {
        let exit = self.new_deopt(bb, pc);
        self.inst.push(AsmInst::Deopt(exit));
    }

    pub(super) fn recompile_and_deopt(&mut self, bb: &BBContext, pc: BcPc, position: Option<BcPc>) {
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
    pub(super) fn guard_class_version(
        &mut self,
        pc: BcPc,
        using_xmm: UsingXmm,
        deopt: AsmDeopt,
        error: AsmError,
    ) {
        self.inst
            .push(AsmInst::GuardClassVersion(pc, using_xmm, deopt, error));
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
        if class == INTEGER_CLASS {
            if bb.is_fixnum(slot) {
                return;
            }
            bb.set_guard_fixnum(slot);
        } else if class == FLOAT_CLASS {
            if bb.is_float(slot) {
                return;
            }
            bb.set_guard_float(slot);
        }
        self.inst.push(AsmInst::GuardClass(r, class, deopt));
    }

    pub(super) fn opt_case(&mut self, max: u16, min: u16, opt_case_id: usize, else_dest: AsmLabel) {
        self.inst.push(AsmInst::OptCase {
            max,
            min,
            opt_case_id,
            else_dest,
        });
    }

    ///
    /// Attribute writer
    ///
    /// ### in
    /// - rdi: receiver: Value
    /// - rdx: value: Value
    ///
    pub(super) fn attr_writer(&mut self, bb: &BBContext, pc: BcPc, ivar_id: IvarId) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::AttrWriter {
            using_xmm,
            error,
            ivar_id,
        });
    }

    ///
    /// ### in
    /// - rdi: receiver: Value
    ///
    pub(super) fn attr_reader(&mut self, ivar_id: IvarId) {
        self.inst.push(AsmInst::AttrReader { ivar_id });
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

    pub(crate) fn write_back_callargs(&mut self, bb: &mut BBContext, callsite: &CallSiteInfo) {
        let CallSiteInfo {
            recv, args, len, ..
        } = callsite;
        self.write_back_slot(bb, *recv);
        self.write_back_range(bb, *args, *len as u16);
    }

    fn write_back_args(&mut self, bb: &mut BBContext, callsite: &CallSiteInfo) {
        let CallSiteInfo { args, len, .. } = callsite;
        self.write_back_range(bb, *args, *len as u16);
    }
}

impl AsmIr {
    ///
    /// ### in
    /// rdi: receiver: Value
    ///
    pub(super) fn send_cached(
        &mut self,
        store: &Store,
        bb: &mut BBContext,
        pc: BcPc,
        callid: CallSiteId,
        callee_fid: FuncId,
        recv_class: ClassId,
        native: bool,
    ) {
        self.reg_move(GP::Rdi, GP::R13);
        self.exec_gc(bb.get_register());
        let using_xmm = bb.get_using_xmm();
        self.xmm_save(using_xmm);
        let caller = &store[callid];
        let callee = &store[callee_fid];
        self.set_arguments(bb, caller, callid, callee, pc);
        self.unlink(bb, caller.dst);
        self.clear(bb);
        let error = self.new_error(bb, pc);
        self.writeback_acc(bb);
        let offset = (16 + (LBP_ARG0 as usize) + 8 * callee.total_args() + 8) / 16 * 16;
        self.inst.push(AsmInst::SendCached {
            callid,
            callee_fid,
            recv_class,
            native,
            offset,
            using_xmm,
            error,
        });
    }

    ///
    /// ### out
    /// - rdi: arg_num
    ///
    fn set_arguments(
        &mut self,
        bb: &mut BBContext,
        caller: &CallSiteInfo,
        callid: CallSiteId,
        callee: &FuncInfo,
        pc: BcPc,
    ) {
        let args = caller.args;
        let pos_num = caller.pos_num;
        let single_arg_expand = pos_num == 1 && callee.single_arg_expand();
        let kw_expansion = callee.no_keyword() && caller.kw_exists();
        if !caller.has_splat()
            && !kw_expansion
            && !single_arg_expand
            && !callee.is_rest()
            && pos_num <= callee.max_positional_args()
            && callee.req_num() <= pos_num
        {
            // write back keyword arguments.
            for i in pos_num as u16..caller.len as u16 {
                self.write_back_slot(bb, args + i);
            }
            let ofs = if (args..args + pos_num).any(|reg| matches!(bb.slot(reg), LinkMode::Xmm(_)))
            {
                (16 + LBP_ARG0 as i32 + (8 * pos_num) as i32 + 8) / 16 * 16
            } else {
                0
            };

            self.reg_sub(GP::Rsp, ofs);
            for i in 0..pos_num {
                let reg = args + i;
                let offset = ofs - (16 + LBP_ARG0 as i32 + (8 * i) as i32);
                self.fetch_to_rsp_offset(bb, reg, offset);
            }
            if pos_num != callee.max_positional_args() {
                self.inst.push(AsmInst::I32ToReg(0, GP::Rax));
                for i in pos_num..callee.max_positional_args() {
                    let offset = ofs - (16 + LBP_ARG0 as i32 + (8 * i) as i32);
                    self.reg2rsp_offset(GP::Rax, offset);
                }
            }
            self.reg_add(GP::Rsp, ofs);
            self.inst
                .push(AsmInst::I32ToReg(callee.pos_num() as _, GP::Rdi));
        } else {
            self.write_back_args(bb, caller);
            let meta = callee.meta();
            let offset = (16 + (LBP_ARG0 as usize) + 8 * callee.max_positional_args() + 8) & !0xf;
            let error = self.new_error(bb, pc);
            self.inst.push(AsmInst::SetArguments {
                callid,
                args,
                meta,
                offset,
                error,
            });
        }
    }

    pub(super) fn send_not_cached(&mut self, bb: &BBContext, pc: BcPc, callid: CallSiteId) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        let self_class = bb.self_value.class();
        self.inst.push(AsmInst::SendNotCached {
            self_class,
            callid,
            pc,
            using_xmm,
            error,
        });
    }

    pub(super) fn generic_unop(&mut self, bb: &BBContext, pc: BcPc, func: UnaryOpFn) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::GenericUnOp {
            func,
            using_xmm,
            error,
        });
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
    pub(super) fn generic_binop(&mut self, bb: &BBContext, pc: BcPc, kind: BinOpK) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::GenericBinOp {
            kind,
            using_xmm,
            error,
        });
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
    pub(super) fn integer_binop(&mut self, bb: &BBContext, pc: BcPc, kind: BinOpK, mode: OpMode) {
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

    pub(super) fn generic_cmp(&mut self, bb: &BBContext, pc: BcPc, kind: CmpKind) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::GenericCmp {
            kind,
            using_xmm,
            error,
        });
    }

    pub(super) fn integer_cmp_br(
        &mut self,
        mode: OpMode,
        kind: CmpKind,
        brkind: BrKind,
        branch_dest: AsmLabel,
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
        branch_dest: AsmLabel,
    ) {
        self.inst.push(AsmInst::FloatCmpBr {
            mode,
            kind,
            brkind,
            branch_dest,
        });
    }

    pub(super) fn generic_index(&mut self, bb: &BBContext, base: SlotId, idx: SlotId, pc: BcPc) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::GenericIndex {
            base,
            idx,
            pc,
            using_xmm,
            error,
        });
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
        pc: BcPc,
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
            error,
        });
    }

    pub(super) fn array_u16_index_assign(&mut self, bb: &BBContext, idx: u16, pc: BcPc) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::ArrayU16IndexAssign {
            idx,
            using_xmm,
            error,
        });
    }

    pub(super) fn array_index_assign(&mut self, bb: &BBContext, pc: BcPc) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst
            .push(AsmInst::ArrayIndexAssign { using_xmm, error });
    }

    pub(super) fn new_array(&mut self, bb: &BBContext, callid: CallSiteId) {
        let using_xmm = bb.get_using_xmm();
        self.inst.push(AsmInst::NewArray(callid, using_xmm));
    }

    pub(super) fn new_hash(&mut self, bb: &BBContext, args: SlotId, len: usize) {
        let using_xmm = bb.get_using_xmm();
        self.inst.push(AsmInst::NewHash(args, len, using_xmm));
    }

    pub(super) fn new_range(
        &mut self,
        bb: &BBContext,
        pc: BcPc,
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
            error,
        });
    }

    pub(super) fn block_arg_proxy(&mut self, ret: SlotId, outer: usize) {
        self.inst.push(AsmInst::BlockArgProxy { ret, outer });
    }

    pub(super) fn block_arg(&mut self, bb: &BBContext, pc: BcPc, ret: SlotId, outer: usize) {
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

    pub(super) fn load_cvar(&mut self, bb: &BBContext, pc: BcPc, name: IdentId) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::LoadCVar {
            name,
            using_xmm,
            error,
        });
    }

    pub(super) fn store_cvar(&mut self, bb: &BBContext, pc: BcPc, name: IdentId, src: SlotId) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::StoreCVar {
            name,
            src,
            using_xmm,
            error,
        });
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

    pub(super) fn concat_regexp(&mut self, bb: &BBContext, pc: BcPc, arg: SlotId, len: u16) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        let len = len as _;
        self.inst.push(AsmInst::ConcatRegexp {
            arg,
            len,
            using_xmm,
            error,
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

    pub(super) fn alias_method(&mut self, bb: &BBContext, pc: BcPc, new: SlotId, old: SlotId) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        self.inst.push(AsmInst::AliasMethod {
            new,
            old,
            using_xmm,
            error,
        });
    }

    pub(crate) fn inline(&mut self, f: impl FnOnce(&mut Codegen, &SideExitLabels) + 'static) {
        self.inst.push(AsmInst::Inline { proc: Box::new(f) });
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
        pc: BcPc,
        name: IdentId,
        dst: SlotId,
    ) {
        self.unlink(bb, dst);
        self.load_cvar(bb, pc, name);
        self.rax2acc(bb, dst);
    }

    pub(super) fn jit_store_cvar(
        &mut self,
        bb: &mut BBContext,
        pc: BcPc,
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
        pc: BcPc,
        deopt: AsmDeopt,
    ) -> FMode {
        match mode {
            OpMode::RR(l, r) => {
                let (flhs, frhs) = self.fetch_float_binary(bb, *l, *r, pc, deopt);
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
    GuardClassVersion(BcPc, UsingXmm, AsmDeopt, AsmError),
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
    Break,
    Raise,
    MethodRet(BcPc),
    EnsureEnd,
    Br(AsmLabel),
    CondBr(BrKind, AsmLabel),
    CheckLocal(AsmLabel),
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
        branch_dest: AsmLabel,
    },
    OptCase {
        max: u16,
        min: u16,
        opt_case_id: usize,
        else_dest: AsmLabel,
    },

    /// deoptimize
    Deopt(AsmDeopt),
    /// recompile and deoptimize
    RecompileDeopt {
        position: Option<BcPc>,
        deopt: AsmDeopt,
    },
    WriteBack(WriteBack),
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
        args: SlotId,
        meta: Meta,
        offset: usize,
        error: AsmError,
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
    /// ### in
    /// - rdi: receiver: Value
    ///
    SendCached {
        callid: CallSiteId,
        recv_class: ClassId,
        callee_fid: FuncId,
        native: bool,
        offset: usize,
        using_xmm: UsingXmm,
        error: AsmError,
    },
    SendNotCached {
        callid: CallSiteId,
        self_class: ClassId,
        pc: BcPc,
        using_xmm: UsingXmm,
        error: AsmError,
    },
    Inline {
        proc: Box<dyn FnOnce(&mut Codegen, &SideExitLabels)>,
    },
    Yield {
        callid: CallSiteId,
        using_xmm: UsingXmm,
        error: AsmError,
    },

    Not,
    GenericUnOp {
        func: UnaryOpFn,
        using_xmm: UsingXmm,
        error: AsmError,
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
        error: AsmError,
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
        error: AsmError,
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
        branch_dest: AsmLabel,
    },
    FloatCmp {
        kind: CmpKind,
        mode: FMode,
    },
    FloatCmpBr {
        kind: CmpKind,
        mode: FMode,
        brkind: BrKind,
        branch_dest: AsmLabel,
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
        name: IdentId,
        using_xmm: UsingXmm,
    },

    GenericIndex {
        base: SlotId,
        idx: SlotId,
        pc: BcPc,
        using_xmm: UsingXmm,
        error: AsmError,
    },
    ArrayU16Index {
        idx: u16,
    },
    ArrayIndex,
    GenericIndexAssign {
        src: SlotId,
        base: SlotId,
        idx: SlotId,
        pc: BcPc,
        using_xmm: UsingXmm,
        error: AsmError,
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
    NewArray(CallSiteId, UsingXmm),
    /// create a new Hash object and store it to rax
    NewHash(SlotId, usize, UsingXmm),
    /// create a new Range object and store it to rax
    NewRange {
        start: SlotId,
        end: SlotId,
        exclude_end: bool,
        using_xmm: UsingXmm,
        error: AsmError,
    },
    ConcatStr {
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
        error: AsmError,
    },
    StoreCVar {
        name: IdentId,
        src: SlotId,
        using_xmm: UsingXmm,
        error: AsmError,
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
    ConcatRegexp {
        arg: SlotId,
        len: usize,
        using_xmm: UsingXmm,
        error: AsmError,
    },
    AliasMethod {
        new: SlotId,
        old: SlotId,
        using_xmm: UsingXmm,
        error: AsmError,
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
    Label(AsmLabel),
    DestLabel(DestLabel),
}

#[derive(Clone, Debug)]
pub(super) enum FMode {
    RR(Xmm, Xmm),
    RI(Xmm, i16),
    IR(i16, Xmm),
}

pub(super) enum SideExit {
    Deoptimize(BcPc, WriteBack),
    Error(BcPc, WriteBack),
}

impl Codegen {
    pub(super) fn gen_code(&mut self, store: &Store, ctx: &mut JitContext) {
        // generate machine code for a main math
        self.gen_asm(store, ctx, None, None);

        // generate machinwe code for bridges
        for (ir, entry, exit) in std::mem::take(&mut ctx.bridges) {
            ctx.ir = ir;
            self.gen_asm(store, ctx, Some(entry), Some(exit));
        }
        assert!(ctx.continuation_bridge.is_none());
    }

    ///
    /// Generate machine code for *ctx.ir*.
    ///
    fn gen_asm(
        &mut self,
        store: &Store,
        ctx: &mut JitContext,
        entry: Option<AsmLabel>,
        exit: Option<DestLabel>,
    ) {
        let mut side_exits = SideExitLabels::new();
        for side_exit in std::mem::take(&mut ctx.ir.side_exit) {
            let label = self.jit.label();
            side_exits.push(label);
            match side_exit {
                SideExit::Deoptimize(pc, wb) => self.gen_deopt_with_label(pc, &wb, label),
                SideExit::Error(pc, wb) => self.gen_handle_error(pc, wb, label),
            }
        }

        for label in ctx.asm_labels.iter_mut().filter(|i| i.is_none()) {
            *label = Some(self.jit.label());
        }

        if exit.is_some() {
            self.jit.select_page(1);
        }
        if let Some(entry) = entry {
            self.jit.bind_label(ctx[entry]);
        }

        #[cfg(feature = "emit-asm")]
        let mut _sourcemap = vec![];

        for inst in std::mem::take(&mut ctx.ir.inst) {
            #[cfg(feature = "emit-asm")]
            if let AsmInst::BcIndex(i) = &inst {
                _sourcemap.push((*i, self.jit.get_current()));
            }
            self.gen_asmir(store, ctx, &side_exits, inst);
        }

        if let Some(exit) = exit {
            monoasm!( &mut self.jit,
                jmp exit;
            );
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

    pub(crate) fn handle_error(&mut self, error: DestLabel) {
        monoasm! { &mut self.jit,
            testq rax, rax;
            jeq   error;
        }
    }
}
