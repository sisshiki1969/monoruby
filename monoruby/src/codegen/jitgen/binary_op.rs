use crate::bytecodegen::BinOpK;

use super::*;

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

impl BBContext {
    fn fold_constant_cmp<T>(&mut self, kind: CmpKind, lhs: T, rhs: T, dst: Option<SlotId>)
    where
        T: PartialEq + PartialOrd,
    {
        let b = cmp(kind, lhs, rhs);
        self.def_C(dst, Value::bool(b));
    }
}

impl BBContext {
    fn check_concrete_i64(&self, mode: OpMode) -> Option<(i64, i64)> {
        match mode {
            OpMode::RR(lhs, rhs) => {
                let lhs = self.is_fixnum_literal(lhs)?;
                let rhs = self.is_fixnum_literal(rhs)?;
                Some((lhs, rhs))
            }
            OpMode::RI(lhs, rhs) => {
                let lhs = self.is_fixnum_literal(lhs)?;
                Some((lhs, rhs as i64))
            }
            OpMode::IR(lhs, rhs) => {
                let rhs = self.is_fixnum_literal(rhs)?;
                Some((lhs as i64, rhs))
            }
        }
    }

    fn check_concrete_f64(&self, lhs: SlotId, rhs: SlotId) -> Option<(f64, f64)> {
        let lhs = self.is_float_literal(lhs)?;
        let rhs = self.is_float_literal(rhs)?;
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
        if let Some((lhs, rhs)) = self.check_concrete_f64(lhs, rhs) {
            let b = cmp(kind, lhs, rhs) ^ (brkind == BrKind::BrIfNot);
            return Some(if b {
                CompileResult::Branch(dest_bb)
            } else {
                CompileResult::Continue
            });
        }
        None
    }

    fn binop_integer_folded(&mut self, kind: BinOpK, lhs: i64, rhs: i64) -> Option<i64> {
        match kind {
            BinOpK::Add => {
                if let Some(result) = lhs.checked_add(rhs)
                    && Value::is_i63(result)
                {
                    return Some(result);
                }
            }
            BinOpK::Sub => {
                if let Some(result) = lhs.checked_sub(rhs)
                    && Value::is_i63(result)
                {
                    return Some(result);
                }
            }
            BinOpK::Mul => {
                if let Some(result) = lhs.checked_mul(rhs)
                    && Value::is_i63(result)
                {
                    return Some(result);
                }
            }
            BinOpK::Div => {
                if let Some(result) = lhs.checked_div(rhs)
                    && Value::is_i63(result)
                {
                    return Some(result);
                }
            }
            BinOpK::Rem => {
                if let Some(result) = lhs.checked_rem(rhs)
                    && Value::is_i63(result)
                {
                    return Some(result);
                }
            }
            BinOpK::Exp => {
                if let Ok(rhs) = u32::try_from(rhs)
                    && let Some(result) = lhs.checked_pow(rhs)
                    && Value::is_i63(result)
                {
                    return Some(result);
                }
            }
            BinOpK::BitOr => {
                return Some(lhs | rhs);
            }
            BinOpK::BitAnd => {
                return Some(lhs & rhs);
            }
            BinOpK::BitXor => {
                return Some(lhs ^ rhs);
            }
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
    pub(super) fn gen_binop_integer(
        &mut self,
        ir: &mut AsmIr,
        kind: BinOpK,
        dst: Option<SlotId>,
        mode: OpMode,
        pc: BytecodePtr,
    ) {
        if let Some((lhs, rhs)) = self.check_concrete_i64(mode)
            && let Some(result) = self.binop_integer_folded(kind, lhs, rhs)
        {
            self.def_C_fixnum(dst, result);
            return;
        };

        match kind {
            BinOpK::Add | BinOpK::Mul | BinOpK::BitOr | BinOpK::BitAnd | BinOpK::BitXor => {
                let lhs = GP::Rdi;
                let rhs = GP::Rsi;
                self.fetch_fixnum_comm(ir, lhs, rhs, mode, pc);
                let deopt = ir.new_deopt(self, pc);
                ir.integer_binop(kind, lhs, rhs, mode, deopt);
                self.def_reg2acc_fixnum(ir, lhs, dst);
            }
            BinOpK::Sub => {
                let lhs = GP::Rdi;
                let rhs = GP::Rsi;
                self.fetch_fixnum_mode(ir, lhs, rhs, mode, pc);
                let deopt = ir.new_deopt(self, pc);
                ir.integer_binop(kind, lhs, rhs, mode, deopt);
                self.def_reg2acc_fixnum(ir, lhs, dst);
            }
            BinOpK::Exp => {
                self.fetch_fixnum_binary(ir, GP::Rdi, GP::Rsi, mode, pc);
                ir.integer_exp(self);
                self.def_reg2acc(ir, GP::Rax, dst);
            }
            BinOpK::Div => {
                let lhs = GP::Rdi;
                let rhs = GP::Rsi;
                self.fetch_fixnum_binary(ir, lhs, rhs, mode, pc);
                let deopt = ir.new_deopt(self, pc);
                ir.integer_binop(kind, lhs, rhs, mode, deopt);
                self.def_reg2acc_fixnum(ir, GP::Rax, dst);
            }
            BinOpK::Rem => match mode {
                OpMode::RI(lhs, rhs) if rhs > 0 && (rhs as u64).is_power_of_two() => {
                    self.fetch_fixnum_r_nodeopt(ir, lhs, GP::Rax, pc);
                    ir.reg_and(GP::Rax, (rhs * 2 - 1) as i64 as u64);
                    self.def_reg2acc_fixnum(ir, GP::Rax, dst);
                }
                _ => {
                    let lhs = GP::Rdi;
                    let rhs = GP::Rsi;
                    self.fetch_fixnum_binary(ir, lhs, rhs, mode, pc);
                    let deopt = ir.new_deopt(self, pc);
                    ir.integer_binop(kind, lhs, rhs, mode, deopt);
                    self.def_reg2acc_fixnum(ir, GP::Rax, dst);
                }
            },
        }
    }

    fn binop_float_folded(&self, kind: BinOpK, lhs: f64, rhs: f64) -> Option<f64> {
        Some(match kind {
            BinOpK::Add => lhs + rhs,
            BinOpK::Sub => lhs - rhs,
            BinOpK::Mul => lhs * rhs,
            BinOpK::Div => lhs / rhs,
            BinOpK::Exp => lhs.powf(rhs),
            BinOpK::Rem => lhs.rem_euclid(rhs),
            _ => return None,
        })
    }

    pub(super) fn gen_binop_float(
        &mut self,
        ir: &mut AsmIr,
        kind: BinOpK,
        dst: Option<SlotId>,
        info: FBinOpInfo,
        pc: BytecodePtr,
    ) {
        if let Some((lhs, rhs)) = self.check_concrete_f64(info.lhs, info.rhs)
            && let Some(result) = self.binop_float_folded(kind, lhs, rhs)
        {
            self.def_C_float(dst, result);
            return;
        };

        let binary_xmm = self.load_binary_ret_xmm(ir, dst, info, pc);
        if let Some(dst) = binary_xmm.2 {
            let using_xmm = self.get_using_xmm();
            ir.xmm_binop(kind, binary_xmm.0, binary_xmm.1, dst, using_xmm);
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
        pc: BytecodePtr,
    ) {
        if let Some((lhs, rhs)) = self.check_concrete_i64(mode) {
            self.fold_constant_cmp(kind, lhs, rhs, dst);
            return;
        };
        let (lhs, rhs) = self.fetch_fixnum_mode_nodeopt(ir, mode, pc);
        ir.integer_cmp(mode, kind, lhs, rhs);
        self.def_rax2acc(ir, dst);
    }

    pub(super) fn gen_cmp_float(
        &mut self,
        ir: &mut AsmIr,
        dst: Option<SlotId>,
        info: FBinOpInfo,
        kind: CmpKind,
        pc: BytecodePtr,
    ) {
        if let Some((lhs, rhs)) = self.check_concrete_f64(info.lhs, info.rhs) {
            self.fold_constant_cmp(kind, lhs, rhs, dst);
            return;
        };
        let binary_xmm = self.load_binary_xmm(ir, info, pc);
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
        pc: BytecodePtr,
    ) {
        let (lhs, rhs) = self.fetch_fixnum_mode_nodeopt(ir, mode, pc);
        ir.integer_cmp_br(mode, kind, lhs, rhs, brkind, branch_dest);
    }

    fn fetch_fixnum_comm(
        &mut self,
        ir: &mut AsmIr,
        lhs: GP,
        rhs: GP,
        mode: OpMode,
        pc: BytecodePtr,
    ) {
        match mode {
            OpMode::RR(l, r) => self.fetch_fixnum_rr(ir, l, r, lhs, rhs, pc),
            OpMode::RI(slot, _) | OpMode::IR(_, slot) => self.fetch_fixnum_r(ir, slot, lhs, pc),
        }
    }

    fn fetch_fixnum_mode(
        &mut self,
        ir: &mut AsmIr,
        lhs: GP,
        rhs: GP,
        mode: OpMode,
        pc: BytecodePtr,
    ) {
        match mode {
            OpMode::RR(l, r) => self.fetch_fixnum_rr(ir, l, r, lhs, rhs, pc),
            OpMode::RI(l, _) => self.fetch_fixnum_r(ir, l, lhs, pc),
            OpMode::IR(_, r) => self.fetch_fixnum_r(ir, r, rhs, pc),
        }
    }

    fn fetch_fixnum_mode_nodeopt(
        &mut self,
        ir: &mut AsmIr,
        mode: OpMode,
        pc: BytecodePtr,
    ) -> (GP, GP) {
        let (lhs, rhs) = self.cmp_regs(mode);
        match mode {
            OpMode::RR(l, r) => self.fetch_fixnum_rr_nodeopt(ir, l, r, lhs, rhs, pc),
            OpMode::RI(l, _) => self.fetch_fixnum_r_nodeopt(ir, l, lhs, pc),
            OpMode::IR(_, r) => self.fetch_fixnum_r_nodeopt(ir, r, rhs, pc),
        }
        (lhs, rhs)
    }

    fn fetch_fixnum_binary(
        &mut self,
        ir: &mut AsmIr,
        lhs: GP,
        rhs: GP,
        mode: OpMode,
        pc: BytecodePtr,
    ) {
        match mode {
            OpMode::RR(l, r) => self.fetch_fixnum_rr(ir, l, r, lhs, rhs, pc),
            OpMode::RI(l, r) => {
                ir.lit2reg(Value::i32(r as i32), rhs);
                self.fetch_fixnum_r(ir, l, lhs, pc);
            }
            OpMode::IR(l, r) => {
                ir.lit2reg(Value::i32(l as i32), lhs);
                self.fetch_fixnum_r(ir, r, rhs, pc);
            }
        }
    }

    fn fetch_fixnum_rr(
        &mut self,
        ir: &mut AsmIr,
        l: SlotId,
        r: SlotId,
        lhs: GP,
        rhs: GP,
        pc: BytecodePtr,
    ) {
        self.load(ir, l, lhs);
        self.load(ir, r, rhs);
        self.guard_fixnum(ir, l, lhs, pc);
        self.guard_fixnum(ir, r, rhs, pc);
    }

    fn fetch_fixnum_rr_nodeopt(
        &mut self,
        ir: &mut AsmIr,
        l: SlotId,
        r: SlotId,
        lhs: GP,
        rhs: GP,
        pc: BytecodePtr,
    ) {
        self.load(ir, l, lhs);
        self.load(ir, r, rhs);
        if self.is_fixnum_literal(l).is_some() && self.is_fixnum_literal(r).is_some() {
            return;
        }
        self.guard_fixnum(ir, l, lhs, pc);
        self.guard_fixnum(ir, r, rhs, pc);
    }

    fn fetch_fixnum_r(&mut self, ir: &mut AsmIr, slot: SlotId, r: GP, pc: BytecodePtr) {
        self.load(ir, slot, r);
        if self.is_fixnum_literal(slot).is_some() {
        } else {
            self.guard_fixnum(ir, slot, r, pc);
        }
    }

    fn fetch_fixnum_r_nodeopt(&mut self, ir: &mut AsmIr, slot: SlotId, r: GP, pc: BytecodePtr) {
        self.load(ir, slot, r);
        if self.is_fixnum_literal(slot).is_some() {
        } else {
            self.guard_fixnum(ir, slot, r, pc);
        }
    }

    pub(super) fn load_binary_xmm(
        &mut self,
        ir: &mut AsmIr,
        info: FBinOpInfo,
        pc: BytecodePtr,
    ) -> (Xmm, Xmm) {
        let FBinOpInfo {
            lhs,
            rhs,
            lhs_class,
            rhs_class,
            ..
        } = info;
        if lhs != rhs {
            (
                self.fetch_float_assume(ir, lhs, lhs_class, pc),
                self.fetch_float_assume(ir, rhs, rhs_class, pc),
            )
        } else {
            let lhs = self.fetch_float_assume(ir, lhs, lhs_class, pc);
            (lhs, lhs)
        }
    }

    pub(super) fn load_binary_ret_xmm(
        &mut self,
        ir: &mut AsmIr,
        dst: Option<SlotId>,
        info: FBinOpInfo,
        pc: BytecodePtr,
    ) -> (Xmm, Xmm, Option<Xmm>) {
        let (lhs, rhs) = self.load_binary_xmm(ir, info, pc);
        let dst = dst.map(|dst| {
            if dst == info.lhs {
                self.def_F_with_xmm(dst, lhs);
                lhs
            } else {
                self.def_F(dst)
            }
        });
        (lhs, rhs, dst)
    }

    fn fetch_float_assume(
        &mut self,
        ir: &mut AsmIr,
        rhs: SlotId,
        class: FOpClass,
        pc: BytecodePtr,
    ) -> Xmm {
        match class {
            FOpClass::Integer => self.load_xmm_fixnum(ir, rhs, pc),
            FOpClass::Float => self.load_xmm(ir, rhs, pc),
        }
    }
}
