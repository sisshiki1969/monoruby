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

// public interface
impl AsmIr {
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

    pub(crate) fn new_deopt(&mut self, bb: &BBContext) -> AsmDeopt {
        let pc = bb.pc();
        self.new_deopt_with_pc(bb, pc)
    }

    pub(crate) fn new_deopt_with_pc(&mut self, bb: &BBContext, pc: BytecodePtr) -> AsmDeopt {
        let i = self.new_label(SideExit::Deoptimize(pc, bb.get_write_back()));
        AsmDeopt(i)
    }

    pub(crate) fn new_error(&mut self, bb: &BBContext) -> AsmError {
        let pc = bb.pc();
        let i = self.new_label(SideExit::Error(pc, bb.get_write_back()));
        AsmError(i)
    }

    pub(crate) fn new_evict(&mut self) -> AsmEvict {
        let i = self.new_label(SideExit::Evict(None));
        AsmEvict(i)
    }

    pub(crate) fn new_deopt_error(&mut self, bb: &BBContext) -> (AsmDeopt, AsmError) {
        let pc = bb.pc();
        let wb = bb.get_write_back();
        let deopt = self.new_label(SideExit::Deoptimize(pc, wb.clone()));
        let error = self.new_label(SideExit::Error(pc, wb));
        (AsmDeopt(deopt), AsmError(error))
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

    pub(super) fn deopt(&mut self, bb: &BBContext) {
        let exit = self.new_deopt(bb);
        self.push(AsmInst::Deopt(exit));
    }

    pub(super) fn check_bop(&mut self, bb: &BBContext) {
        let deopt = self.new_deopt(bb);
        self.push(AsmInst::CheckBOP { deopt });
    }

    pub(super) fn recompile_and_deopt(&mut self, bb: &BBContext, position: Option<BytecodePtr>) {
        let deopt = self.new_deopt(bb);
        self.push(AsmInst::RecompileDeopt { position, deopt });
    }

    pub(crate) fn xmm_save(&mut self, using_xmm: UsingXmm) {
        self.push(AsmInst::XmmSave(using_xmm));
    }

    pub(crate) fn xmm_restore(&mut self, using_xmm: UsingXmm) {
        self.push(AsmInst::XmmRestore(using_xmm));
    }

    pub(super) fn exec_gc(&mut self, wb: WriteBack) {
        self.push(AsmInst::ExecGc(wb));
    }

    pub fn reg_move(&mut self, src: GP, dst: GP) {
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

    pub fn reg2rsp_offset(&mut self, r: GP, i: i32) {
        self.push(AsmInst::RegToRSPOffset(r, i));
    }

    pub fn i32torsp_offset(&mut self, val: i32, i: i32) {
        self.push(AsmInst::I32ToRSPOffset(val, i));
    }

    pub(crate) fn reg2stack(&mut self, src: GP, dst: impl Into<Option<SlotId>>) {
        if let Some(dst) = dst.into() {
            self.push(AsmInst::RegToStack(src, dst));
        }
    }

    pub(crate) fn stack2reg(&mut self, src: SlotId, dst: GP) {
        self.push(AsmInst::StackToReg(src, dst));
    }

    pub(super) fn xmm_move(&mut self, src: Xmm, dst: Xmm) {
        self.push(AsmInst::XmmMove(src, dst));
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

    pub fn int2xmm(&mut self, reg: GP, x: Xmm, deopt: AsmDeopt) {
        self.push(AsmInst::IntToXmm(reg, x, deopt));
    }

    pub fn float2xmm(&mut self, reg: GP, x: Xmm, deopt: AsmDeopt) {
        self.push(AsmInst::FloatToXmm(reg, x, deopt));
    }

    pub fn f64toxmm(&mut self, f: f64, x: Xmm) {
        self.push(AsmInst::F64ToXmm(f, x));
    }

    pub fn i64toboth(&mut self, i: i64, reg: SlotId, x: Xmm) {
        self.push(AsmInst::I64ToBoth(i, reg, x));
    }

    /// rax = val
    pub(super) fn deep_copy_lit(&mut self, bb: &BBContext, val: Value) {
        let using_xmm = bb.get_using_xmm();
        self.push(AsmInst::DeepCopyLit(val, using_xmm));
    }

    pub fn guard_fixnum(&mut self, r: GP, deopt: AsmDeopt) {
        self.push(AsmInst::GuardFixnum(r, deopt));
    }

    pub fn guard_float(&mut self, r: GP, deopt: AsmDeopt) {
        self.push(AsmInst::GuardFloat(r, deopt));
    }

    pub fn guard_array_ty(&mut self, r: GP, deopt: AsmDeopt) {
        self.push(AsmInst::GuardArrayTy(r, deopt));
    }

    /*///
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
        self.push(AsmInst::GuardClassVersionWithRecovery(
            cached_fid,
            cached_version,
            callid,
            using_xmm,
            deopt,
            error,
        ));
    }*/

    ///
    /// Class version guard for JIT.
    ///
    /// Check the cached class version.
    /// If different, jump to `deopt`.
    ///
    /// ### destroy
    /// - rax
    ///
    pub(super) fn guard_class_version(&mut self, cached_version: u32, deopt: AsmDeopt) {
        self.push(AsmInst::GuardClassVersion(cached_version, deopt));
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
        self.push(AsmInst::GuardClass(r, class, deopt));
    }

    pub(crate) fn guard_lhs_class_for_mode(
        &mut self,
        bb: &mut BBContext,
        mode: OpMode,
        lhs_class: ClassId,
        deopt: AsmDeopt,
    ) {
        match mode {
            OpMode::RR(lhs, _) | OpMode::RI(lhs, _) => {
                self.guard_class(bb, lhs, GP::Rdi, lhs_class, deopt);
            }
            OpMode::IR(_, _) => {
                if lhs_class != INTEGER_CLASS {
                    self.push(AsmInst::Deopt(deopt));
                }
            }
        }
    }

    pub(super) fn opt_case(
        &mut self,
        max: u16,
        min: u16,
        else_dest: BasicBlockId,
        branch_table: Box<[BasicBlockId]>,
    ) {
        self.push(AsmInst::OptCase {
            max,
            min,
            else_dest,
            branch_table,
        });
    }

    pub fn guard_base_class(&mut self, bbctx: &mut BBContext, slot: SlotId, base_class: Value) {
        bbctx.fetch_for_gpr(self, slot, GP::Rax);
        let deopt = self.new_deopt(bbctx);
        self.inst
            .push(AsmInst::GuardBaseClass { base_class, deopt });
    }

    pub fn load_constant(&mut self, bbctx: &mut BBContext, dst: SlotId, cache: &ConstCache) {
        let ConstCache { version, value, .. } = cache;
        let deopt = self.new_deopt(bbctx);
        if let Some(f) = value.try_float() {
            self.load_float_constant(bbctx, f, dst, *version, deopt);
        } else {
            self.load_generic_constant(bbctx, *value, dst, *version, deopt);
        }
    }

    fn load_float_constant(
        &mut self,
        bbctx: &mut BBContext,
        f: f64,
        dst: SlotId,
        cached_version: usize,
        deopt: AsmDeopt,
    ) {
        let fdst = bbctx.def_new_both_float(dst);
        self.push(AsmInst::LoadFloatConstant {
            fdst,
            f,
            cached_version,
            deopt,
        });
        self.reg2stack(GP::Rax, dst);
    }

    fn load_generic_constant(
        &mut self,
        bbctx: &mut BBContext,
        cached_val: Value,
        dst: SlotId,
        cached_version: usize,
        deopt: AsmDeopt,
    ) {
        self.push(AsmInst::LoadGenericConstant {
            cached_val,
            cached_version,
            deopt,
        });
        bbctx.rax2acc(self, dst);
    }

    pub(crate) fn handle_error(&mut self, error: AsmError) {
        self.push(AsmInst::HandleError(error));
    }
}

impl AsmIr {
    ///
    /// Set positional arguments for callee.
    ///
    pub(super) fn set_arguments(
        &mut self,
        store: &Store,
        bb: &mut BBContext,
        callsite: &CallSiteInfo,
        callee_fid: FuncId,
    ) {
        let callee = &store[callee_fid];
        let args = callsite.args;
        let pos_num = callsite.pos_num;
        let kw_pos = callsite.kw_pos;
        let kw_num = callsite.kw_len();
        let single_arg_expand = pos_num == 1 && callee.single_arg_expand();
        let ex_positional = callee.no_keyword() && callsite.kw_may_exists();
        if !callsite.has_splat()
            && !callsite.has_hash_splat()
            && !ex_positional
            && !single_arg_expand
            && !callee.is_rest()
            && (callee.is_block_style() || (pos_num <= callee.max_positional_args()))
            && callee.req_num() <= pos_num
        {
            // write back keyword arguments.
            for arg in kw_pos..kw_pos + kw_num {
                bb.write_back_slot(self, arg);
            }
            // write back block argument.
            if let Some(block_arg) = callsite.block_arg {
                bb.write_back_slot(self, block_arg);
            }
            let ofs = if (args..args + pos_num).any(|reg| matches!(bb.mode(reg), LinkMode::Xmm(_)))
            {
                (RSP_LOCAL_FRAME + LFP_ARG0 + (8 * pos_num) as i32 + 8) & !0xf
            } else {
                0
            };

            self.reg_sub(GP::Rsp, ofs);
            for i in 0..pos_num {
                let reg = args + i;
                let offset = ofs - (RSP_LOCAL_FRAME + LFP_ARG0 + (8 * i) as i32);
                bb.fetch_for_callee(self, reg, offset);
            }
            if pos_num != callee.max_positional_args() {
                self.push(AsmInst::I32ToReg(0, GP::Rax));
                for i in pos_num..callee.max_positional_args() {
                    let offset = ofs - (RSP_LOCAL_FRAME + LFP_ARG0 as i32 + (8 * i) as i32);
                    self.reg2rsp_offset(GP::Rax, offset);
                }
            }
            self.reg_add(GP::Rsp, ofs);
        } else {
            bb.write_back_args(self, callsite);

            let error = self.new_error(bb);
            self.push(AsmInst::SetArguments {
                callid: callsite.id,
                callee_fid,
            });
            self.handle_error(error);
        }
    }

    pub(super) fn set_binop_arguments(
        &mut self,
        store: &Store,
        bb: &mut BBContext,
        callee_fid: FuncId,
        mode: OpMode,
    ) {
        let callee = &store[callee_fid];
        // callee.req_num() <= 1 at this point.
        // callee.is_rest() || callee.max_positional_args() >= 1 at this point.
        let xmm_flag = match mode {
            OpMode::RR(_, rhs) | OpMode::IR(_, rhs) => {
                matches!(bb.mode(rhs), LinkMode::Xmm(_))
            }
            OpMode::RI(_, _) => false,
        };
        let ofs = if xmm_flag || callee.is_rest() {
            (RSP_LOCAL_FRAME + LFP_ARG0 + 16 as i32) & !0xf
        } else {
            0
        };

        self.reg_sub(GP::Rsp, ofs);
        let offset = ofs - (RSP_LOCAL_FRAME + LFP_ARG0);
        bb.fetch_rhs_for_callee(self, mode, offset);
        if 1 < callee.max_positional_args() {
            self.push(AsmInst::I32ToReg(0, GP::Rax));
            for i in 1..callee.max_positional_args() {
                let offset = ofs - (RSP_LOCAL_FRAME + LFP_ARG0 as i32 + (8 * i) as i32);
                self.reg2rsp_offset(GP::Rax, offset);
            }
        }
        if callee.is_rest() {
            self.push(AsmInst::RSPOffsetToArray(offset));
        }
        self.reg_add(GP::Rsp, ofs);
    }

    pub(super) fn generic_unop(&mut self, bb: &BBContext, func: UnaryOpFn) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb);
        self.push(AsmInst::GenericUnOp { func, using_xmm });
        self.handle_error(error);
    }

    pub(super) fn integer_cmp_br(
        &mut self,
        mode: OpMode,
        kind: CmpKind,
        brkind: BrKind,
        branch_dest: JitLabel,
    ) {
        self.push(AsmInst::IntegerCmpBr {
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
        branch_dest: JitLabel,
    ) {
        self.push(AsmInst::FloatCmpBr {
            mode,
            kind,
            brkind,
            branch_dest,
        });
    }

    /*pub(crate) fn generic_index(&mut self, bb: &BBContext, base: SlotId, idx: SlotId) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb);
        let pc = bb.pc();
        self.push(AsmInst::GenericIndex {
            base,
            idx,
            pc,
            using_xmm,
        });
        self.handle_error(error);
    }*/

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

    pub(super) fn new_array(&mut self, bb: &BBContext, callid: CallSiteId) {
        let using_xmm = bb.get_using_xmm();
        self.push(AsmInst::NewArray { callid, using_xmm });
    }

    pub(super) fn new_lambda(&mut self, bb: &BBContext, func_id: FuncId) {
        let using_xmm = bb.get_using_xmm();
        self.push(AsmInst::NewLambda(func_id, using_xmm));
    }

    pub(super) fn new_hash(&mut self, bb: &BBContext, args: SlotId, len: usize) {
        let using_xmm = bb.get_using_xmm();
        self.push(AsmInst::NewHash(args, len, using_xmm));
    }

    pub(super) fn new_range(
        &mut self,
        bb: &BBContext,
        start: SlotId,
        end: SlotId,
        exclude_end: bool,
    ) {
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb);
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

    pub(super) fn expand_array(&mut self, bb: &BBContext, dst: SlotId, len: u16) {
        let using_xmm = bb.get_using_xmm();
        let len = len as _;
        self.push(AsmInst::ExpandArray {
            dst,
            len,
            using_xmm,
        });
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
    I32ToRSPOffset(i32, i32),
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
    XmmToStack(Xmm, SlotId),
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
    /*///
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
    GuardClassVersionWithRecovery(FuncId, u32, CallSiteId, UsingXmm, AsmDeopt, AsmError),*/
    ///
    /// Class version guard for JIT.
    ///
    /// Check the cached class version.
    /// If different, jump to `deopt`.
    ///
    /// ### destroy
    /// - rax
    ///
    GuardClassVersion(u32, AsmDeopt),
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
    Br(JitLabel),
    CondBr(BrKind, JitLabel),
    NilBr(JitLabel),
    CheckLocal(JitLabel),
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
        branch_dest: JitLabel,
    },
    OptCase {
        max: u16,
        min: u16,
        else_dest: BasicBlockId,
        branch_table: Box<[BasicBlockId]>,
    },

    Preparation,
    Init(FnInitInfo),
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
    XmmRestore(UsingXmm),
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
    /// Send method
    ///
    /// ### in
    /// - rdi: receiver: Value
    ///
    /// ### destroy
    /// - caller save registers
    /// - r15
    ///
    Send {
        callid: CallSiteId,
        recv_class: ClassId,
        callee_fid: FuncId,
        error: AsmError,
        evict: AsmEvict,
    },
    ///
    /// Send inlined method
    ///
    /// ### in
    /// - rdi: receiver: Value
    ///
    /// ### destroy
    /// - caller save registers
    /// - r15
    ///
    SendInlined {
        callid: CallSiteId,
        callee_fid: FuncId,
        inlined_entry: JitLabel,
        error: AsmError,
        evict: AsmEvict,
    },
    BinopCached {
        recv_class: ClassId,
        callee_fid: FuncId,
        evict: AsmEvict,
    },
    Inline(InlineProcedure),
    Yield {
        callid: CallSiteId,
        using_xmm: UsingXmm,
        error: AsmError,
        evict: AsmEvict,
    },
    YieldInlined {
        callid: CallSiteId,
        block_iseq: ISeqId,
        block_entry: JitLabel,
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
    /// Compare two values with `mode``, jump to `branch_dest`` if the condition specified by `kind``
    /// and `brkind`` is met.
    ///
    IntegerCmpBr {
        mode: OpMode,
        kind: CmpKind,
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
    /// - rax: Value
    ///
    /// #### destroy
    /// - rdi, rsi
    ///
    LoadIVarHeap {
        ivarid: IvarId,
        is_object_ty: bool,
        self_: bool,
    },
    LoadIVarInline {
        ivarid: IvarId,
    },
    ///
    /// Store the object *rax* in an instance var *ivarid* of the object *rdi*.
    ///
    /// #### in
    /// - rax: Value
    /// - rdi: &RValue
    ///
    /// #### destroy
    /// - caller-save registers
    ///
    StoreIVarHeap {
        ivarid: IvarId,
        is_object_ty: bool,
        self_: bool,
        using_xmm: UsingXmm,
    },
    StoreIVarInline {
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
        entry_exit: Option<(DestLabel, BasicBlockId)>,
    ) {
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

        if entry_exit.is_some() {
            self.jit.select_page(1);
        }
        if let Some((entry, _)) = entry_exit {
            self.jit.bind_label(entry);
        }

        for inst in ir.inst {
            #[cfg(feature = "emit-asm")]
            if let AsmInst::BcIndex(i) = &inst {
                ctx.sourcemap
                    .push((*i, self.jit.get_current() - ctx.start_codepos));
            }
            self.compile_asmir(store, ctx, &side_exits, inst);
        }

        if let Some((_, exit)) = entry_exit {
            let exit = ctx.get_bb_label(exit);
            let exit = ctx.resolve_label(&mut self.jit, exit);
            monoasm! { &mut self.jit,
                jmp exit;
            }
            self.jit.select_page(0);
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
