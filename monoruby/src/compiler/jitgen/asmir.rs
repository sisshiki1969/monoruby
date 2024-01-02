use crate::bytecodegen::BinOpK;

use super::*;

mod binary_op;
mod constants;
mod defined;
mod definition;
mod index;
mod merge;
mod method_call;
mod read_slot;
pub mod slot;
mod variables;

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

impl AsmIr {
    pub fn new() -> Self {
        Self {
            inst: vec![],
            side_exit: vec![],
        }
    }

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

    pub(crate) fn rax2acc(&mut self, bb: &mut BBContext, dst: impl Into<Option<SlotId>>) {
        self.reg2acc(bb, GP::Rax, dst);
    }

    pub(crate) fn reg2acc(&mut self, bb: &mut BBContext, src: GP, dst: impl Into<Option<SlotId>>) {
        if let Some(dst) = dst.into() {
            self.clear(bb);
            if let Some(acc) = self.clear_r15(bb)
                && acc < bb.sp
                && acc != dst
            {
                self.inst.push(AsmInst::AccToStack(acc));
            }
            self.link_r15(bb, dst);
            self.inst.push(AsmInst::RegToAcc(src));
        }
    }

    pub(super) fn reg_move(&mut self, src: GP, dst: GP) {
        if src != dst {
            self.inst.push(AsmInst::RegMove(src, dst));
        }
    }

    pub(super) fn reg_add(&mut self, r: GP, i: i32) {
        self.inst.push(AsmInst::RegAdd(r, i));
    }

    pub(super) fn reg_sub(&mut self, r: GP, i: i32) {
        self.inst.push(AsmInst::RegSub(r, i));
    }

    pub(super) fn reg2rsp_offset(&mut self, r: GP, i: i32) {
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

    pub(super) fn xmm_swap(&mut self, x1: Xmm, x2: Xmm) {
        self.inst.push(AsmInst::XmmSwap(x1, x2));
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
    /// Generate convert code from Xmm to Both.
    ///
    /// ### out
    /// - rax: Value
    ///
    /// ### destroy
    /// - rcx
    ///
    pub(super) fn xmm2both(&mut self, freg: Xmm, reg: Vec<SlotId>) {
        self.inst.push(AsmInst::XmmToBoth(freg, reg));
    }

    ///
    /// ### destroy
    /// - rax
    ///
    pub(super) fn lit2stack(&mut self, v: Value, reg: SlotId) {
        self.inst.push(AsmInst::LitToStack(v, reg));
    }

    pub(super) fn lit2reg(&mut self, v: Value, reg: GP) {
        self.inst.push(AsmInst::LitToReg(v, reg));
    }

    pub(super) fn acc2stack(&mut self, reg: SlotId) {
        self.inst.push(AsmInst::AccToStack(reg));
    }

    pub(super) fn int2xmm(&mut self, reg: GP, x: Xmm, deopt: AsmDeopt) {
        self.inst.push(AsmInst::IntToXmm(reg, x, deopt));
    }

    pub(super) fn float2xmm(&mut self, reg: GP, x: Xmm, deopt: AsmDeopt) {
        self.inst.push(AsmInst::FloatToXmm(reg, x, deopt));
    }

    pub(super) fn f64toxmm(&mut self, f: f64, x: Xmm) {
        self.inst.push(AsmInst::F64ToXmm(f, x));
    }

    pub(super) fn i64toboth(&mut self, i: i64, reg: SlotId, x: Xmm) {
        self.inst.push(AsmInst::I64ToBoth(i, reg, x));
    }

    /// rax = val
    pub(super) fn deep_copy_lit(&mut self, bb: &BBContext, val: Value) {
        let using_xmm = bb.get_using_xmm();
        self.inst.push(AsmInst::DeepCopyLit(val, using_xmm));
    }

    pub(super) fn guard_fixnum(&mut self, r: GP, deopt: AsmDeopt) {
        self.inst.push(AsmInst::GuardFixnum(r, deopt));
    }

    pub(super) fn guard_float(&mut self, r: GP, deopt: AsmDeopt) {
        self.inst.push(AsmInst::GuardFloat(r, deopt));
    }

    pub(super) fn guard_array_ty(&mut self, r: GP, deopt: AsmDeopt) {
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
    pub(crate) fn guard_class(&mut self, r: GP, class: ClassId, deopt: AsmDeopt) {
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
        let callsite = &store[callid];
        self.set_arguments(bb, callsite);
        self.link_stack(bb, callsite.dst);
        self.clear(bb);
        let error = self.new_error(bb, pc);
        self.writeback_acc(bb);
        self.inst.push(AsmInst::SendCached {
            callid,
            callee_fid,
            recv_class,
            native,
            using_xmm,
            error,
        });
    }

    fn set_arguments(&mut self, bb: &mut BBContext, callsite: &CallSiteInfo) {
        let args = callsite.args;
        let pos_num = callsite.pos_num as u16;
        if callsite.has_splat() {
            self.write_back_args(bb, callsite);
            let splat_pos = callsite.splat_pos.clone();
            self.inst.push(AsmInst::SetArgumentsWithSplat {
                splat_pos,
                args,
                pos_num,
            });
        } else {
            for i in pos_num..callsite.len as u16 {
                self.write_back_slot(bb, args + i);
            }
            let ofs = if (args..args + pos_num).any(|reg| matches!(bb[reg], LinkMode::Xmm(_))) {
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
            self.reg_add(GP::Rsp, ofs);
            self.inst.push(AsmInst::I32ToReg(pos_num as _, GP::Rdi));
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
        self.link_stack(bb, dst);
        self.load_gvar(bb, name);
        self.rax2acc(bb, dst);
    }

    pub(super) fn jit_store_gvar(&mut self, bb: &mut BBContext, name: IdentId, src: SlotId) {
        self.write_back_slots(bb, &[src]);
        self.store_gvar(bb, name, src);
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
    XmmToBoth(Xmm, Vec<SlotId>),
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
    SetArgumentsWithSplat {
        splat_pos: Vec<usize>,
        args: SlotId,
        pos_num: u16,
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
        superclass: SlotId,
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum GP {
    Rax = 0,
    Rdx = 2,
    Rsp = 4,
    Rsi = 6,
    Rdi = 7,
    R13 = 13,
    R15 = 15,
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

    ///
    /// Generate machine code for *inst*.
    ///
    fn gen_asmir(
        &mut self,
        store: &Store,
        ctx: &JitContext,
        labels: &SideExitLabels,
        inst: AsmInst,
    ) {
        match inst {
            AsmInst::BcIndex(_) => {}
            AsmInst::Label(label) => {
                self.jit.bind_label(ctx[label]);
            }
            AsmInst::DestLabel(label) => {
                self.jit.bind_label(label);
            }
            AsmInst::AccToStack(r) => {
                self.store_r15(r);
            }
            AsmInst::RegToAcc(r) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movq r15, R(r);
                );
            }
            AsmInst::RegToStack(r, slot) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movq [r14 - (conv(slot))], R(r);
                );
            }
            AsmInst::StackToReg(slot, r) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movq R(r), [r14 - (conv(slot))];
                );
            }
            AsmInst::LitToReg(v, r) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movq R(r), (v.id());
                );
            }
            AsmInst::I32ToReg(i, r) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movl R(r), (i);
                );
            }
            AsmInst::RegMove(src, dst) => {
                let src = src as u64;
                let dst = dst as u64;
                monoasm!( &mut self.jit,
                    movq R(dst), R(src);
                );
            }
            AsmInst::RegAdd(r, i) => {
                if i != 0 {
                    let r = r as u64;
                    monoasm! { &mut self.jit,
                        addq R(r), (i);
                    }
                }
            }
            AsmInst::RegSub(r, i) => {
                if i != 0 {
                    let r = r as u64;
                    monoasm! { &mut self.jit,
                        subq R(r), (i);
                    }
                }
            }
            AsmInst::RegToRSPOffset(r, ofs) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movq [rsp + (ofs)], R(r);
                );
            }

            AsmInst::XmmMove(l, r) => self.xmm_mov(l, r),
            AsmInst::XmmSwap(l, r) => self.xmm_swap(l, r),
            AsmInst::XmmBinOp {
                kind,
                mode,
                dst,
                using_xmm,
            } => self.float_binop(kind, using_xmm, dst, mode),
            AsmInst::XmmUnOp { kind, dst } => match kind {
                UnOpK::Neg => {
                    let imm = self.jit.const_i64(0x8000_0000_0000_0000u64 as i64);
                    monoasm!( &mut self.jit,
                        xorps xmm(dst.enc()), [rip + imm];
                    );
                }
                UnOpK::Pos => {}
            },

            AsmInst::NumToXmm(reg, x, side_exit) => {
                self.numeric_val_to_f64(reg, x.enc(), labels[side_exit]);
            }
            AsmInst::F64ToXmm(f, x) => {
                let f = self.jit.const_f64(f);
                monoasm!( &mut self.jit,
                    movq  xmm(x.enc()), [rip + f];
                );
            }
            AsmInst::IntToXmm(r, x, side_exit) => {
                self.integer_val_to_f64(r, x.enc(), labels[side_exit]);
            }
            AsmInst::FloatToXmm(reg, x, side_exit) => {
                self.float_to_f64(reg, x.enc(), labels[side_exit]);
            }
            AsmInst::I64ToBoth(i, r, x) => {
                let f = self.jit.const_f64(i as f64);
                monoasm! {&mut self.jit,
                    movq [r14 - (conv(r))], (Value::integer(i).id());
                    movq xmm(x.enc()), [rip + f];
                }
            }
            AsmInst::XmmToBoth(x, slots) => self.xmm_to_both(x, &slots),
            AsmInst::LitToStack(v, slot) => self.literal_to_stack(slot, v),
            AsmInst::DeepCopyLit(v, using_xmm) => {
                self.xmm_save(using_xmm);
                monoasm!( &mut self.jit,
                  movq rdi, (v.id());
                  movq rax, (Value::value_deep_copy);
                  call rax;
                );
                self.xmm_restore(using_xmm);
            }

            AsmInst::GuardFloat(r, deopt) => {
                let deopt = labels[deopt];
                self.guard_float(r, deopt);
            }
            AsmInst::GuardFixnum(r, deopt) => {
                let deopt = labels[deopt];
                self.guard_fixnum(r, deopt)
            }
            AsmInst::GuardArrayTy(r, deopt) => {
                let deopt = labels[deopt];
                self.guard_array_ty(r, deopt)
            }
            AsmInst::GuardClassVersion(pc, using_xmm, deopt, error) => {
                let deopt = labels[deopt];
                let error = labels[error];
                self.guard_class_version(pc, using_xmm, deopt, error);
            }
            AsmInst::GuardClass(r, class, deopt) => {
                let deopt = labels[deopt];
                self.guard_class(r, class, deopt);
            }
            AsmInst::Deopt(deopt) => {
                let deopt = labels[deopt];
                monoasm!( &mut self.jit,
                    jmp deopt;
                );
            }
            AsmInst::RecompileDeopt { position, deopt } => {
                let deopt = labels[deopt];
                self.recompile_and_deopt(position, deopt)
            }
            AsmInst::WriteBack(wb) => self.gen_write_back(&wb),
            AsmInst::XmmSave(using_xmm) => self.xmm_save(using_xmm),
            AsmInst::ExecGc(wb) => self.execute_gc(Some(&wb)),
            AsmInst::SetArgumentsWithSplat {
                splat_pos,
                args,
                pos_num,
            } => {
                self.jit_set_arguments_splat(&splat_pos, args, pos_num);
            }

            AsmInst::Ret => {
                self.epilogue();
            }
            AsmInst::MethodRet(pc) => {
                monoasm! { &mut self.jit,
                    movq r13, ((pc + 1).u64());
                };
                self.method_return();
            }
            AsmInst::Break => {
                self.block_break();
                self.epilogue();
            }
            AsmInst::Raise => {
                let raise = self.entry_raise;
                monoasm! { &mut self.jit,
                    movq rdi, rbx;
                    movq rsi, rax;
                    movq rax, (runtime::raise_err);
                    call rax;
                    jmp  raise;
                };
            }
            AsmInst::EnsureEnd => {
                let raise = self.entry_raise;
                monoasm! { &mut self.jit,
                    movq rdi, rbx;
                    movq rax, (runtime::check_err);
                    call rax;
                    testq rax, rax;
                    jne  raise;
                };
            }
            AsmInst::Br(dest) => {
                let dest = ctx[dest];
                monoasm!( &mut self.jit,
                    jmp dest;
                );
            }
            AsmInst::CondBr(brkind, dest) => {
                let dest = ctx[dest];
                monoasm!( &mut self.jit,
                    orq rax, 0x10;
                    cmpq rax, (FALSE_VALUE);
                );
                match brkind {
                    BrKind::BrIf => monoasm!( &mut self.jit, jne dest;),
                    BrKind::BrIfNot => monoasm!( &mut self.jit, jeq dest;),
                }
            }
            AsmInst::CheckLocal(branch_dest) => {
                let branch_dest = ctx[branch_dest];
                monoasm!( &mut self.jit,
                    testq rax, rax;
                    jnz  branch_dest;
                );
            }
            AsmInst::OptCase {
                max,
                min,
                opt_case_id,
                else_dest,
            } => {
                let OptCaseAsmInfo {
                    id,
                    bb_pos,
                    label_map,
                } = &ctx.opt_case[opt_case_id];

                // generate a jump table.
                let jump_table = self.jit.const_align8();
                for ofs in store[*id].branch_table.iter() {
                    let idx = *bb_pos + 1 + (*ofs as i32);
                    let dest_label = ctx[label_map.get(&idx).cloned().unwrap()];
                    self.jit.abs_address(dest_label);
                }

                let else_dest = ctx[else_dest];
                monoasm! {&mut self.jit,
                    sarq rdi, 1;
                    cmpq rdi, (max);
                    jgt  else_dest;
                    subq rdi, (min);
                    jlt  else_dest;
                    lea  rax, [rip + jump_table];
                    jmp  [rax + rdi * 8];
                };
            }

            AsmInst::AttrWriter {
                ivar_id,
                using_xmm,
                error,
            } => {
                self.attr_writer(using_xmm, labels[error], ivar_id);
            }
            AsmInst::AttrReader { ivar_id } => {
                self.attr_reader(ivar_id);
            }
            AsmInst::SendCached {
                callid,
                callee_fid,
                recv_class,
                native,
                using_xmm,
                error,
            } => {
                let error = labels[error];
                self.send_cached(
                    store, callid, callee_fid, recv_class, native, using_xmm, error,
                );
            }
            AsmInst::SendNotCached {
                self_class,
                callid: callsite,
                pc,
                using_xmm,
                error,
            } => {
                let error = labels[error];
                self.send_not_cached(self_class, &store[callsite], pc, using_xmm, error);
            }
            AsmInst::Yield {
                callid,
                using_xmm,
                error,
            } => {
                let error = labels[error];
                self.gen_yield(store, callid, using_xmm, error);
            }

            AsmInst::Not => {
                self.not_rdi_to_rax();
            }
            AsmInst::GenericUnOp {
                func,
                using_xmm,
                error,
            } => {
                self.xmm_save(using_xmm);
                self.call_unop(func);
                self.xmm_restore(using_xmm);
                self.handle_error(labels[error]);
            }

            AsmInst::GenericBinOp {
                kind,
                using_xmm,
                error,
            } => {
                let error = labels[error];
                self.generic_binop(kind, using_xmm, error);
            }
            AsmInst::IntegerBinOp {
                kind,
                mode,
                using_xmm,
                deopt,
                error,
            } => {
                let deopt = labels[deopt];
                let error = labels[error];
                self.integer_binop(&mode, kind, deopt, error, using_xmm);
            }

            AsmInst::GenericCmp {
                kind,
                using_xmm,
                error,
            } => {
                self.generic_cmp(&kind, using_xmm);
                self.handle_error(labels[error]);
            }
            AsmInst::IntegerCmp { kind, mode } => self.integer_cmp(kind, mode),
            AsmInst::IntegerCmpBr {
                mode,
                kind,
                brkind,
                branch_dest,
            } => {
                let branch_dest = ctx[branch_dest];
                self.cmp_integer(&mode);
                self.condbr_int(kind, branch_dest, brkind);
            }
            AsmInst::FloatCmp { kind, mode } => {
                monoasm! { &mut self.jit,
                    xorq rax, rax;
                };
                self.cmp_float(&mode);
                self.setflag_float(kind);
            }
            AsmInst::FloatCmpBr {
                kind,
                mode,
                brkind,
                branch_dest,
            } => {
                let branch_dest = ctx[branch_dest];
                self.cmp_float(&mode);
                self.condbr_float(kind, branch_dest, brkind);
            }

            AsmInst::GuardBaseClass { base_class, deopt } => {
                let deopt = labels[deopt];
                let cached_base_class = self.jit.const_i64(base_class.id() as _);
                monoasm! { &mut self.jit,
                    cmpq rax, [rip + cached_base_class];  // rax: base_class
                    jne  deopt;
                }
            }
            AsmInst::LoadFloatConstant {
                fdst,
                f,
                cached_version,
                deopt,
            } => {
                let deopt = labels[deopt];
                self.load_float_constant(fdst, deopt, f, cached_version);
            }
            AsmInst::LoadGenericConstant {
                cached_val,
                cached_version,
                deopt,
            } => {
                let deopt = labels[deopt];
                self.load_generic_constant(deopt, cached_val, cached_version);
            }
            AsmInst::StoreConstant { name, using_xmm } => {
                self.store_constant(name, using_xmm);
            }

            AsmInst::GenericIndex {
                base,
                idx,
                pc,
                using_xmm,
                error,
            } => {
                self.generic_index(using_xmm, base, idx, pc);
                self.handle_error(labels[error]);
            }
            AsmInst::ArrayU16Index { idx } => {
                self.gen_array_u16_index(idx);
            }
            AsmInst::ArrayIndex => {
                self.gen_array_index();
            }
            AsmInst::GenericIndexAssign {
                src,
                base,
                idx,
                pc,
                using_xmm,
                error,
            } => {
                self.generic_index_assign(using_xmm, base, idx, src, pc);
                self.handle_error(labels[error]);
            }
            AsmInst::ArrayU16IndexAssign {
                idx,
                using_xmm,
                error,
            } => {
                self.gen_array_u16_index_assign(using_xmm, idx);
                self.handle_error(labels[error]);
            }
            AsmInst::ArrayIndexAssign { using_xmm, error } => {
                self.gen_array_index_assign(using_xmm);
                self.handle_error(labels[error]);
            }

            AsmInst::NewArray(callid, using_xmm) => {
                self.new_array(callid, using_xmm);
            }
            AsmInst::NewHash(args, len, using_xmm) => {
                self.new_hash(args, len, using_xmm);
            }
            AsmInst::NewRange {
                start,
                end,
                exclude_end,
                using_xmm,
                error,
            } => {
                self.load_rdi(start);
                self.load_rsi(end);
                self.new_range(exclude_end, using_xmm);
                self.handle_error(labels[error]);
            }

            AsmInst::BlockArgProxy { ret, outer } => {
                self.get_method_lfp(outer);
                self.block_arg_proxy();
                self.store_rax(ret);
            }
            AsmInst::BlockArg {
                ret,
                outer,
                using_xmm,
                error,
            } => {
                self.get_method_lfp(outer);
                self.block_arg(using_xmm);
                self.handle_error(labels[error]);
                self.store_rax(ret);
            }

            AsmInst::LoadDynVar { src } => self.load_dyn_var(src),
            AsmInst::StoreDynVar { dst, src } => self.store_dyn_var(dst, src),

            AsmInst::LoadIVar {
                name,
                cached_ivarid,
                is_object_ty,
                is_self_cached,
                using_xmm,
            } => self.load_ivar(name, cached_ivarid, is_object_ty, is_self_cached, using_xmm),
            AsmInst::StoreIVar {
                name,
                cached_ivarid,
                is_object_ty,
                is_self_cached,
                using_xmm,
                error,
            } => self.store_ivar(
                name,
                cached_ivarid,
                is_object_ty,
                is_self_cached,
                using_xmm,
                labels[error],
            ),

            AsmInst::LoadGVar { name, using_xmm } => self.load_gvar(name, using_xmm),
            AsmInst::StoreGVar {
                name,
                src,
                using_xmm,
            } => self.store_gvar(name, src, using_xmm),
            AsmInst::LoadSVar { id, using_xmm } => self.load_svar(id, using_xmm),

            AsmInst::ClassDef {
                superclass,
                dst,
                name,
                func_id,
                is_module,
                using_xmm,
                error,
            } => {
                self.class_def(
                    superclass,
                    dst,
                    name,
                    func_id,
                    is_module,
                    using_xmm,
                    labels[error],
                );
            }
            AsmInst::SingletonClassDef {
                base,
                dst,
                func_id,
                using_xmm,
                error,
            } => {
                self.singleton_class_def(base, dst, func_id, using_xmm, labels[error]);
            }
            AsmInst::MethodDef {
                name,
                func_id,
                using_xmm,
            } => {
                self.method_def(name, func_id, using_xmm);
            }
            AsmInst::SingletonMethodDef {
                obj,
                name,
                func_id,
                using_xmm,
            } => {
                self.singleton_method_def(obj, name, func_id, using_xmm);
            }

            AsmInst::ExpandArray {
                dst,
                len,
                using_xmm,
            } => {
                self.xmm_save(using_xmm);
                monoasm!( &mut self.jit,
                    lea rsi, [r14 - (conv(dst))];
                    movq rdx, (len);
                    movq rax, (runtime::expand_array);
                    call rax;
                );
                self.xmm_restore(using_xmm);
            }
            AsmInst::ConcatStr {
                arg,
                len,
                using_xmm,
            } => self.concat_string(arg, len, using_xmm),
            AsmInst::ConcatRegexp {
                arg,
                len,
                using_xmm,
                error,
            } => {
                self.xmm_save(using_xmm);
                monoasm!( &mut self.jit,
                    movq rdi, rbx;
                    movq rsi, r12;
                    lea rdx, [r14 - (conv(arg))];
                    movq rcx, (len);
                    movq rax, (runtime::concatenate_regexp);
                    call rax;
                );
                self.xmm_restore(using_xmm);
                self.handle_error(labels[error]);
            }
            AsmInst::AliasMethod {
                new,
                old,
                using_xmm,
                error,
            } => {
                self.xmm_save(using_xmm);
                monoasm!( &mut self.jit,
                    movq rdi, rbx;
                    movq rsi, r12;
                    movq rdx, [r14 - (LBP_SELF)];
                    movq rcx, [r14 - (conv(new))];
                    movq r8, [r14 - (conv(old))];
                    movq r9, [r14 - (LBP_META)];
                    movq rax, (runtime::alias_method);
                    call rax;
                );
                self.xmm_restore(using_xmm);
                self.handle_error(labels[error]);
            }
            AsmInst::DefinedYield { dst, using_xmm } => self.defined_yield(dst, using_xmm),
            AsmInst::DefinedConst {
                dst,
                siteid,
                using_xmm,
            } => self.defined_const(dst, siteid, using_xmm),
            AsmInst::DefinedMethod {
                dst,
                recv,
                name,
                using_xmm,
            } => self.defined_method(dst, recv, name, using_xmm),
            AsmInst::DefinedGvar {
                dst,
                name,
                using_xmm,
            } => self.defined_gvar(dst, name, using_xmm),

            AsmInst::DefinedIvar {
                dst,
                name,
                using_xmm,
            } => self.defined_ivar(dst, name, using_xmm),

            AsmInst::GenericCondBr {
                brkind,
                branch_dest,
            } => {
                let branch_dest = ctx[branch_dest];
                self.cond_br(branch_dest, brkind);
            }

            AsmInst::Inline { proc } => proc(self, labels),
        }
    }

    pub(crate) fn handle_error(&mut self, error: DestLabel) {
        monoasm! { &mut self.jit,
            testq rax, rax;
            jeq   error;
        }
    }

    ///
    /// Get method lfp.
    ///
    /// ### in
    /// - r14: lfp
    ///
    /// ### out
    /// - rax: method lfp
    ///
    fn get_method_lfp(&mut self, outer: usize) {
        if outer == 0 {
            monoasm! { &mut self.jit,
                movq rax, r14;
            };
        } else {
            monoasm!( &mut self.jit,
                movq rax, [r14 - (LBP_OUTER)];
            );
            for _ in 0..outer - 1 {
                monoasm!( &mut self.jit,
                    movq rax, [rax];
                );
            }
            monoasm!( &mut self.jit,
                lea rax, [rax + (LBP_OUTER)];
            );
        }
    }

    fn get_outer(&mut self, outer: usize) {
        monoasm!( &mut self.jit,
            movq rax, [r14 - (LBP_OUTER)];
        );
        for _ in 0..outer - 1 {
            monoasm!( &mut self.jit,
                movq rax, [rax];
            );
        }
    }
}

impl Codegen {
    fn new_array(&mut self, callid: CallSiteId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movl rdx, (callid.get());
            lea  rcx, [r14 - (LBP_SELF)];
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::gen_array);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    fn new_hash(&mut self, args: SlotId, len: usize, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            lea  rdi, [r14 - (conv(args))];
            movq rsi, (len);
            movq rax, (runtime::gen_hash);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    fn new_range(&mut self, exclude_end: bool, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdx, rbx; // &mut Executor
            movq rcx, r12; // &mut Globals
            movl r8, (exclude_end as u32);
            movq rax, (runtime::gen_range);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }

    fn concat_string(&mut self, arg: SlotId, len: u16, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdi, r12;
            lea rsi, [r14 - (conv(arg))];
            movq rdx, (len);
            movq rax, (runtime::concatenate_string);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    ///
    /// Get block handler of a current method frame.
    ///
    /// ### in
    /// - rax: method lfp
    ///
    /// ### out
    /// - rax: block handler
    ///
    fn block_arg_proxy(&mut self) {
        monoasm! { &mut self.jit,
            movq rax, [rax - (LBP_BLOCK)];
            xorq rdi, rdi;
            movq rsi, 0b10;
            testq rax, 0b1;
            cmovneq rdi, rsi;
            addq rax, rdi;
        };
    }

    ///
    /// Get a block argument of current frame.
    ///
    fn block_arg(&mut self, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdx, [rax - (LBP_BLOCK)];
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::block_arg);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }
}
