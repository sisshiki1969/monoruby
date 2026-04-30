use num::Zero;

use crate::bytecodegen::BinOpK;

use super::*;

impl<'a> JitContext<'a> {
    pub(super) fn binary_op(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        kind: BinOpK,
        dst: Option<SlotId>,
        lhs: SlotId,
        rhs: SlotId,
        ic: Option<(ClassId, ClassId)>,
        bc_pos: BcIndex,
    ) -> JitResult<CompileResult> {
        match kind {
            // These ops are always compiled as method calls.
            // The inline function registered on Integer#<< / Integer#>> /
            // Integer#| / Integer#& / Integer#^ / Integer#** / Integer#% /
            // Float#** / Float#% handles code generation using both-side class
            // info from the BinOp inline cache.
            BinOpK::Shl
            | BinOpK::Shr
            | BinOpK::BitOr
            | BinOpK::BitAnd
            | BinOpK::BitXor
            | BinOpK::Exp
            | BinOpK::Rem => {
                let (lhs_class, rhs_class) = state.binary_class(lhs, rhs, ic);
                match lhs_class {
                    None => Ok(CompileResult::Recompile(RecompileReason::NotCached)),
                    Some(lhs_class) => {
                        self.call_binary_method(
                            state, ir, lhs, rhs, lhs_class, rhs_class, kind, bc_pos,
                        )
                    }
                }
            }
            _ => match state.binop_type(lhs, rhs, ic) {
                BinaryOpType::Integer(mode) => {
                    state.binop_integer(ir, kind, dst, mode);
                    Ok(CompileResult::Continue)
                }
                BinaryOpType::Float(info) => {
                    state.binop_float(ir, kind, dst, info);
                    Ok(CompileResult::Continue)
                }
                BinaryOpType::Other(None, _) => {
                    Ok(CompileResult::Recompile(RecompileReason::NotCached))
                }
                BinaryOpType::Other(Some(lhs_class), rhs_class) => {
                    self.call_binary_method(
                        state, ir, lhs, rhs, lhs_class, rhs_class, kind, bc_pos,
                    )
                }
            },
        }
    }

    pub(super) fn binary_cmp(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        kind: CmpKind,
        dst: Option<SlotId>,
        lhs: SlotId,
        rhs: SlotId,
        ic: Option<(ClassId, ClassId)>,
        bc_pos: BcIndex,
    ) -> JitResult<CompileResult> {
        match state.binop_type(lhs, rhs, ic) {
            BinaryOpType::Integer(mode) => {
                state.gen_cmp_integer(ir, kind, dst, mode);
                Ok(CompileResult::Continue)
            }
            BinaryOpType::Float(info) => {
                state.gen_cmp_float(ir, dst, info, kind);
                Ok(CompileResult::Continue)
            }
            BinaryOpType::Other(None, _) => {
                Ok(CompileResult::Recompile(RecompileReason::NotCached))
            }
            BinaryOpType::Other(Some(lhs_class), rhs_class) => {
                self.call_binary_method(state, ir, lhs, rhs, lhs_class, rhs_class, kind, bc_pos)
            }
        }
    }

    pub(super) fn binary_cmp_br(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        kind: CmpKind,
        lhs: SlotId,
        rhs: SlotId,
        dest_bb: BasicBlockId,
        brkind: BrKind,
        ic: Option<(ClassId, ClassId)>,
        bc_pos: BcIndex,
    ) -> JitResult<CompileResult> {
        match state.binop_type(lhs, rhs, ic) {
            BinaryOpType::Integer(mode) => {
                if let Some(result) = state.check_concrete_i64_cmpbr(mode, kind, brkind, dest_bb) {
                    return Ok(result);
                }
                let src_idx = bc_pos + 1;
                let dest = self.label();
                state.gen_cmpbr_integer(ir, kind, mode, brkind, dest);
                self.new_side_branch(src_idx, dest_bb, state.clone(), dest);
                Ok(CompileResult::Continue)
            }
            BinaryOpType::Float(info) => {
                if let Some(result) =
                    state.check_concrete_f64_cmpbr(lhs, rhs, kind, brkind, dest_bb)
                {
                    return Ok(result);
                }
                let src_idx = bc_pos + 1;
                let dest = self.label();
                let mode = state.load_binary_xmm(ir, info);
                ir.float_cmp_br(mode, kind, brkind, dest);
                self.new_side_branch(src_idx, dest_bb, state.clone(), dest);
                Ok(CompileResult::Continue)
            }
            BinaryOpType::Other(None, _) => {
                Ok(CompileResult::Recompile(RecompileReason::NotCached))
            }
            BinaryOpType::Other(Some(lhs_class), rhs_class) => {
                let res = self
                    .call_binary_method(state, ir, lhs, rhs, lhs_class, rhs_class, kind, bc_pos)?;
                if let CompileResult::Continue = res {
                    let src_idx = bc_pos + 1;
                    state.unset_class_version_guard();
                    self.gen_cond_br(state, ir, src_idx, dest_bb, brkind);
                }
                Ok(res)
            }
        }
    }
}

fn cmp<T>(kind: CmpKind, lhs: T, rhs: T) -> bool
where
    T: PartialEq + PartialOrd,
{
    match kind {
        CmpKind::Eq | CmpKind::TEq => lhs == rhs,
        CmpKind::Ne => lhs != rhs,
        CmpKind::Lt => lhs < rhs,
        CmpKind::Le => lhs <= rhs,
        CmpKind::Gt => lhs > rhs,
        CmpKind::Ge => lhs >= rhs,
    }
}

impl AbstractFrame {
    fn fold_constant_cmp<T>(&mut self, kind: CmpKind, lhs: T, rhs: T, dst: Option<SlotId>)
    where
        T: PartialEq + PartialOrd,
    {
        let b = cmp(kind, lhs, rhs);
        self.def_C(dst, Immediate::bool(b));
    }

    fn check_concrete_i64(&self, mode: OpMode) -> Option<(i64, i64)> {
        match mode {
            OpMode::RR(lhs, rhs) => {
                let lhs = self.is_fixnum_literal(lhs)?.get();
                let rhs = self.is_fixnum_literal(rhs)?.get();
                Some((lhs, rhs))
            }
            OpMode::RI(lhs, rhs) => {
                let lhs = self.is_fixnum_literal(lhs)?.get();
                Some((lhs, rhs as i64))
            }
            OpMode::IR(lhs, rhs) => {
                let rhs = self.is_fixnum_literal(rhs)?.get();
                Some((lhs as i64, rhs))
            }
        }
    }

    #[allow(non_snake_case)]
    pub(super) fn check_binary_C_f64(&self, lhs: SlotId, rhs: SlotId) -> Option<(f64, f64)> {
        let lhs = self.coerce_C_f64(lhs)?;
        let rhs = self.coerce_C_f64(rhs)?;
        Some((lhs, rhs))
    }

    pub(super) fn check_concrete_i64_cmpbr(
        &mut self,
        mode: OpMode,
        kind: CmpKind,
        brkind: BrKind,
        dest_bb: BasicBlockId,
    ) -> Option<CompileResult> {
        if let Some((lhs, rhs)) = self.check_concrete_i64(mode) {
            let b = cmp(kind, lhs, rhs) ^ (brkind == BrKind::BrIfNot);
            return Some(if b {
                CompileResult::Branch(dest_bb)
            } else {
                CompileResult::Continue
            });
        }
        None
    }

    pub(super) fn check_concrete_f64_cmpbr(
        &mut self,
        lhs: SlotId,
        rhs: SlotId,
        kind: CmpKind,
        brkind: BrKind,
        dest_bb: BasicBlockId,
    ) -> Option<CompileResult> {
        if let Some((lhs, rhs)) = self.check_binary_C_f64(lhs, rhs) {
            let b = cmp(kind, lhs, rhs) ^ (brkind == BrKind::BrIfNot);
            return Some(if b {
                CompileResult::Branch(dest_bb)
            } else {
                CompileResult::Continue
            });
        }
        None
    }

    fn binop_integer_folded(&mut self, kind: BinOpK, lhs: i64, rhs: i64) -> Option<Immediate> {
        match kind {
            BinOpK::Add => {
                if let Some(result) = lhs.checked_add(rhs) {
                    return Immediate::check_fixnum(result);
                }
            }
            BinOpK::Sub => {
                if let Some(result) = lhs.checked_sub(rhs) {
                    return Immediate::check_fixnum(result);
                }
            }
            BinOpK::Mul => {
                if let Some(result) = lhs.checked_mul(rhs) {
                    return Immediate::check_fixnum(result);
                }
            }
            BinOpK::Div => {
                if rhs.is_zero() {
                    return None;
                }
                return Immediate::check_fixnum(lhs.ruby_div(&rhs));
            }
            BinOpK::Rem
            | BinOpK::Exp
            | BinOpK::BitOr
            | BinOpK::BitAnd
            | BinOpK::BitXor
            | BinOpK::Shl
            | BinOpK::Shr => unreachable!(),
        }
        None
    }

    ///
    /// Integer binary operations
    ///
    /// ### in
    /// - rdi: lhs
    /// - rsi: rhs
    ///
    /// ### out
    /// - r15: dst
    ///
    fn binop_integer(&mut self, ir: &mut AsmIr, kind: BinOpK, dst: Option<SlotId>, mode: OpMode) {
        if let Some((lhs, rhs)) = self.check_concrete_i64(mode)
            && let Some(result) = self.binop_integer_folded(kind, lhs, rhs)
        {
            self.def_C(dst, result);
            return;
        };

        match kind {
            BinOpK::Add | BinOpK::Mul => {
                let lhs = GP::Rdi;
                let rhs = GP::Rsi;
                self.fetch_fixnum_comm(ir, lhs, rhs, mode);
                let deopt = ir.new_deopt(self);
                ir.integer_binop(kind, lhs, rhs, mode, deopt);
                self.def_reg2acc_fixnum(ir, lhs, dst);
            }
            BinOpK::Sub => {
                let lhs = GP::Rdi;
                let rhs = GP::Rsi;
                self.fetch_fixnum_mode(ir, lhs, rhs, mode);
                let deopt = ir.new_deopt(self);
                ir.integer_binop(kind, lhs, rhs, mode, deopt);
                self.def_reg2acc_fixnum(ir, lhs, dst);
            }
            BinOpK::Div => {
                let lhs = GP::Rdi;
                let rhs = GP::Rsi;
                self.fetch_fixnum_binary(ir, lhs, rhs, mode);
                let deopt = ir.new_deopt(self);
                ir.integer_binop(kind, lhs, rhs, mode, deopt);
                self.def_reg2acc_fixnum(ir, GP::Rax, dst);
            }
            BinOpK::Rem
            | BinOpK::Exp
            | BinOpK::BitOr
            | BinOpK::BitAnd
            | BinOpK::BitXor
            | BinOpK::Shl
            | BinOpK::Shr => unreachable!(),
        }
    }

    fn binop_float_folded(&self, kind: BinOpK, lhs: f64, rhs: f64) -> Option<f64> {
        Some(match kind {
            BinOpK::Add => lhs + rhs,
            BinOpK::Sub => lhs - rhs,
            BinOpK::Mul => lhs * rhs,
            BinOpK::Div => lhs.ruby_div(&rhs),
            _ => return None,
        })
    }

    fn binop_float(&mut self, ir: &mut AsmIr, kind: BinOpK, dst: Option<SlotId>, info: FBinOpInfo) {
        if let Some((lhs, rhs)) = self.check_binary_C_f64(info.lhs, info.rhs)
            && let Some(result) = self.binop_float_folded(kind, lhs, rhs)
            && self.def_C_float(dst, result)
        {
            return;
        };

        let (lhs, rhs, dst) = self.load_binary_ret_xmm(ir, dst, info);
        if let Some(dst) = dst {
            ir.xmm_binop(kind, lhs, rhs, dst);
        }
    }

    fn cmp_regs(&self, mode: OpMode) -> (GP, GP) {
        match mode {
            OpMode::RR(lhs, rhs) => (self.on_reg_or(lhs, GP::Rdi), self.on_reg_or(rhs, GP::Rsi)),
            OpMode::RI(lhs, _) => (self.on_reg_or(lhs, GP::Rdi), GP::Rsi),
            OpMode::IR(_, rhs) => (GP::Rdi, self.on_reg_or(rhs, GP::Rsi)),
        }
    }

    pub(super) fn gen_cmp_integer(
        &mut self,
        ir: &mut AsmIr,
        kind: CmpKind,
        dst: Option<SlotId>,
        mode: OpMode,
    ) {
        if let Some((lhs, rhs)) = self.check_concrete_i64(mode) {
            self.fold_constant_cmp(kind, lhs, rhs, dst);
            return;
        };
        let (lhs, rhs) = self.fetch_fixnum_mode_nodeopt(ir, mode);
        ir.integer_cmp(mode, kind, lhs, rhs);
        self.def_rax2acc(ir, dst);
    }

    pub(super) fn gen_cmp_float(
        &mut self,
        ir: &mut AsmIr,
        dst: Option<SlotId>,
        info: FBinOpInfo,
        kind: CmpKind,
    ) {
        if let Some((lhs, rhs)) = self.check_binary_C_f64(info.lhs, info.rhs) {
            self.fold_constant_cmp(kind, lhs, rhs, dst);
            return;
        };
        let binary_xmm = self.load_binary_xmm(ir, info);
        ir.push(AsmInst::FloatCmp {
            kind,
            lhs: binary_xmm.0,
            rhs: binary_xmm.1,
        });
        self.def_rax2acc(ir, dst);
    }

    pub(super) fn gen_cmpbr_integer(
        &mut self,
        ir: &mut AsmIr,
        kind: CmpKind,
        mode: OpMode,
        brkind: BrKind,
        branch_dest: JitLabel,
    ) {
        let (lhs, rhs) = self.fetch_fixnum_mode_nodeopt(ir, mode);
        ir.integer_cmp_br(mode, kind, lhs, rhs, brkind, branch_dest);
    }

    fn fetch_fixnum_comm(&mut self, ir: &mut AsmIr, lhs: GP, rhs: GP, mode: OpMode) {
        match mode {
            OpMode::RR(l, r) => self.fetch_fixnum_rr(ir, l, r, lhs, rhs),
            OpMode::RI(slot, _) | OpMode::IR(_, slot) => self.fetch_fixnum_r(ir, slot, lhs),
        }
    }

    fn fetch_fixnum_mode(&mut self, ir: &mut AsmIr, lhs: GP, rhs: GP, mode: OpMode) {
        match mode {
            OpMode::RR(l, r) => self.fetch_fixnum_rr(ir, l, r, lhs, rhs),
            OpMode::RI(l, _) => self.fetch_fixnum_r(ir, l, lhs),
            OpMode::IR(_, r) => self.fetch_fixnum_r(ir, r, rhs),
        }
    }

    fn fetch_fixnum_mode_nodeopt(&mut self, ir: &mut AsmIr, mode: OpMode) -> (GP, GP) {
        let (lhs, rhs) = self.cmp_regs(mode);
        match mode {
            OpMode::RR(l, r) => self.fetch_fixnum_rr_nodeopt(ir, l, r, lhs, rhs),
            OpMode::RI(l, _) => self.fetch_fixnum_r_nodeopt(ir, l, lhs),
            OpMode::IR(_, r) => self.fetch_fixnum_r_nodeopt(ir, r, rhs),
        }
        (lhs, rhs)
    }

    fn fetch_fixnum_binary(&mut self, ir: &mut AsmIr, lhs: GP, rhs: GP, mode: OpMode) {
        match mode {
            OpMode::RR(l, r) => self.fetch_fixnum_rr(ir, l, r, lhs, rhs),
            OpMode::RI(l, r) => {
                ir.lit2reg(Value::i32(r as i32), rhs);
                self.fetch_fixnum_r(ir, l, lhs);
            }
            OpMode::IR(l, r) => {
                ir.lit2reg(Value::i32(l as i32), lhs);
                self.fetch_fixnum_r(ir, r, rhs);
            }
        }
    }

    fn fetch_fixnum_rr(&mut self, ir: &mut AsmIr, l: SlotId, r: SlotId, lhs: GP, rhs: GP) {
        self.load(ir, l, lhs);
        self.load(ir, r, rhs);
        self.guard_fixnum(ir, l, lhs);
        self.guard_fixnum(ir, r, rhs);
    }

    fn fetch_fixnum_rr_nodeopt(&mut self, ir: &mut AsmIr, l: SlotId, r: SlotId, lhs: GP, rhs: GP) {
        self.load(ir, l, lhs);
        self.load(ir, r, rhs);
        if self.is_fixnum_literal(l).is_some() && self.is_fixnum_literal(r).is_some() {
            return;
        }
        self.guard_fixnum(ir, l, lhs);
        self.guard_fixnum(ir, r, rhs);
    }

    fn fetch_fixnum_r(&mut self, ir: &mut AsmIr, slot: SlotId, r: GP) {
        self.load(ir, slot, r);
        if self.is_fixnum_literal(slot).is_some() {
        } else {
            self.guard_fixnum(ir, slot, r);
        }
    }

    fn fetch_fixnum_r_nodeopt(&mut self, ir: &mut AsmIr, slot: SlotId, r: GP) {
        self.load(ir, slot, r);
        if self.is_fixnum_literal(slot).is_some() {
        } else {
            self.guard_fixnum(ir, slot, r);
        }
    }

    pub(super) fn load_binary_xmm(&mut self, ir: &mut AsmIr, info: FBinOpInfo) -> (VirtFPReg, VirtFPReg) {
        let FBinOpInfo {
            lhs,
            rhs,
            lhs_class,
            rhs_class,
            ..
        } = info;
        if lhs != rhs {
            // Loading lhs may set an `Sf` mode on its xmm. Without pinning,
            // the next allocator call (when loading rhs) can demote that
            // same xmm via Phase-1 of `try_alloc_xmm_demote` and hand it
            // back as the rhs xmm — so the consumer would compare /
            // arithmetic the value with itself. Pin lhs across the rhs
            // load to force the allocator to pick a different physical
            // register.
            let lhs_xmm = self.fetch_float_assume(ir, lhs, lhs_class);
            self.pin_xmm(lhs_xmm);
            let rhs_xmm = self.fetch_float_assume(ir, rhs, rhs_class);
            self.unpin_xmm(lhs_xmm);
            (lhs_xmm, rhs_xmm)
        } else {
            let lhs = self.fetch_float_assume(ir, lhs, lhs_class);
            (lhs, lhs)
        }
    }

    pub(super) fn load_binary_ret_xmm(
        &mut self,
        ir: &mut AsmIr,
        dst: Option<SlotId>,
        info: FBinOpInfo,
    ) -> (VirtFPReg, VirtFPReg, Option<VirtFPReg>) {
        let (lhs, rhs) = self.load_binary_xmm(ir, info);
        // Pin both operands while allocating the destination — `def_F` calls
        // `alloc_xmm`, which can otherwise pick `lhs` or `rhs` as the spill
        // victim and alias dst onto an operand the consumer still needs.
        self.pin_xmm(lhs);
        self.pin_xmm(rhs);
        let dst = dst.map(|dst| {
            if dst == info.lhs {
                self.def_F_with_xmm(dst, lhs);
                lhs
            } else {
                self.def_F(ir, dst)
            }
        });
        self.unpin_xmm(rhs);
        self.unpin_xmm(lhs);
        (lhs, rhs, dst)
    }

    fn fetch_float_assume(&mut self, ir: &mut AsmIr, rhs: SlotId, class: FOpClass) -> VirtFPReg {
        match class {
            FOpClass::Integer => self.load_xmm_fixnum(ir, rhs),
            FOpClass::Float => self.load_xmm(ir, rhs),
        }
    }
}
