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

    fn check_concrete_f64(&self, mode: OpMode) -> Option<(f64, f64)> {
        match mode {
            OpMode::RR(lhs, rhs) => {
                let lhs = self.is_float_literal(lhs)?;
                let rhs = self.is_float_literal(rhs)?;
                Some((lhs, rhs))
            }
            OpMode::RI(lhs, rhs) => {
                let lhs = self.is_float_literal(lhs)?;
                Some((lhs, rhs as f64))
            }
            OpMode::IR(lhs, rhs) => {
                let rhs = self.is_float_literal(rhs)?;
                Some((lhs as f64, rhs))
            }
        }
    }

    pub(super) fn check_concrete_i64_cmpbr(
        &mut self,
        mode: OpMode,
        kind: CmpKind,
        brkind: BrKind,
    ) -> Option<CompileResult> {
        if let Some((lhs, rhs)) = self.check_concrete_i64(mode) {
            let b = cmp(kind, lhs, rhs) ^ (brkind == BrKind::BrIfNot);
            return Some(if b {
                CompileResult::Branch
            } else {
                CompileResult::Continue
            });
        }
        None
    }

    pub(super) fn check_concrete_f64_cmpbr(
        &mut self,
        mode: OpMode,
        kind: CmpKind,
        brkind: BrKind,
    ) -> Option<CompileResult> {
        if let Some((lhs, rhs)) = self.check_concrete_f64(mode) {
            let b = cmp(kind, lhs, rhs) ^ (brkind == BrKind::BrIfNot);
            return Some(if b {
                CompileResult::Branch
            } else {
                CompileResult::Continue
            });
        }
        None
    }

    fn binop_fixnum_folded(
        &mut self,
        kind: BinOpK,
        lhs: i64,
        rhs: i64,
        dst: Option<SlotId>,
    ) -> bool {
        match kind {
            BinOpK::Add => {
                if let Some(result) = lhs.checked_add(rhs)
                    && Value::is_i63(result)
                {
                    self.def_C_fixnum(dst, result);
                    return true;
                }
            }
            BinOpK::Sub => {
                if let Some(result) = lhs.checked_sub(rhs)
                    && Value::is_i63(result)
                {
                    self.def_C_fixnum(dst, result);
                    return true;
                }
            }
            BinOpK::Mul => {
                if let Some(result) = lhs.checked_mul(rhs)
                    && Value::is_i63(result)
                {
                    self.def_C_fixnum(dst, result);
                    return true;
                }
            }
            BinOpK::Div => {
                if let Some(result) = lhs.checked_div(rhs)
                    && Value::is_i63(result)
                {
                    self.def_C_fixnum(dst, result);
                    return true;
                }
            }
            BinOpK::Rem => {
                if let Some(result) = lhs.checked_rem(rhs)
                    && Value::is_i63(result)
                {
                    self.def_C_fixnum(dst, result);
                    return true;
                }
            }
            BinOpK::Exp => {}
            BinOpK::BitOr => {
                self.def_C_fixnum(dst, lhs | rhs);
                return true;
            }
            BinOpK::BitAnd => {
                self.def_C_fixnum(dst, lhs & rhs);
                return true;
            }
            BinOpK::BitXor => {
                self.def_C_fixnum(dst, lhs ^ rhs);
                return true;
            }
        }
        false
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
    pub(super) fn gen_binop_fixnum(
        &mut self,
        ir: &mut AsmIr,
        kind: BinOpK,
        dst: Option<SlotId>,
        mode: OpMode,
    ) {
        if let Some((lhs, rhs)) = self.check_concrete_i64(mode)
            && self.binop_fixnum_folded(kind, lhs, rhs, dst)
        {
            return;
        };

        match kind {
            BinOpK::Add | BinOpK::Mul | BinOpK::BitOr | BinOpK::BitAnd | BinOpK::BitXor => {
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
            BinOpK::Exp => {
                self.fetch_fixnum_binary(ir, GP::Rdi, GP::Rsi, mode);
                ir.integer_exp(self);
                self.def_reg2acc(ir, GP::Rax, dst);
            }
            BinOpK::Div => {
                let lhs = GP::Rdi;
                let rhs = GP::Rsi;
                self.fetch_fixnum_binary(ir, lhs, rhs, mode);
                let deopt = ir.new_deopt(self);
                ir.integer_binop(kind, lhs, rhs, mode, deopt);
                self.def_reg2acc_fixnum(ir, GP::Rax, dst);
            }
            BinOpK::Rem => match mode {
                OpMode::RI(lhs, rhs) if rhs > 0 && (rhs as u64).is_power_of_two() => {
                    self.fetch_fixnum_r_nodeopt(ir, lhs, GP::Rax);
                    ir.reg_and(GP::Rax, (rhs * 2 - 1) as i64 as u64);
                    self.def_reg2acc_fixnum(ir, GP::Rax, dst);
                }
                _ => {
                    let lhs = GP::Rdi;
                    let rhs = GP::Rsi;
                    self.fetch_fixnum_binary(ir, lhs, rhs, mode);
                    let deopt = ir.new_deopt(self);
                    ir.integer_binop(kind, lhs, rhs, mode, deopt);
                    self.def_reg2acc_fixnum(ir, GP::Rax, dst);
                }
            },
        }
    }

    pub(super) fn gen_binop_float(&mut self, ir: &mut AsmIr, kind: BinOpK, info: FBinOpInfo) {
        if let Some((lhs, rhs)) = self.check_concrete_f64(info.mode) {
            match kind {
                BinOpK::Add => {
                    self.def_C_float(info.dst, lhs + rhs);
                    return;
                }
                BinOpK::Sub => {
                    self.def_C_float(info.dst, lhs - rhs);
                    return;
                }
                BinOpK::Mul => {
                    self.def_C_float(info.dst, lhs * rhs);
                    return;
                }
                BinOpK::Div => {
                    self.def_C_float(info.dst, lhs / rhs);
                    return;
                }
                _ => {}
            }
        };

        let fmode = self.fmode(ir, info);
        if let Some(dst) = info.dst {
            let dst = self.def_F(dst);
            let using_xmm = self.get_using_xmm();
            ir.xmm_binop(kind, fmode, dst, using_xmm);
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

    pub(super) fn gen_cmp_float(&mut self, ir: &mut AsmIr, info: FBinOpInfo, kind: CmpKind) {
        if let Some((lhs, rhs)) = self.check_concrete_f64(info.mode) {
            self.fold_constant_cmp(kind, lhs, rhs, info.dst);
            return;
        };
        let mode = self.fmode(ir, info);
        self.clear_above_next_sp();
        ir.push(AsmInst::FloatCmp { kind, mode });
        self.def_rax2acc(ir, info.dst);
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

    ///
    /// Fetch lhs operands for binary operation according to *mode*.
    ///
    /// #### in
    /// - rdi: lhs
    ///
    pub(super) fn load_lhs(&mut self, ir: &mut AsmIr, mode: OpMode, r: GP) {
        match mode {
            OpMode::RR(lhs, _) | OpMode::RI(lhs, _) => {
                self.load(ir, lhs, r);
            }
            OpMode::IR(lhs, _) => {
                ir.lit2reg(Value::i32(lhs as i32), r);
            }
        }
    }

    pub(super) fn fmode(&mut self, ir: &mut AsmIr, info: FBinOpInfo) -> FMode {
        let FBinOpInfo {
            mode,
            lhs_class,
            rhs_class,
            ..
        } = info;
        match mode {
            OpMode::RR(l, r) => {
                let (flhs, frhs) = if l != r {
                    (
                        self.fetch_float_assume(ir, l, lhs_class),
                        self.fetch_float_assume(ir, r, rhs_class),
                    )
                } else {
                    let lhs = self.fetch_float_assume(ir, l, lhs_class);
                    (lhs, lhs)
                };
                FMode::RR(flhs, frhs)
            }
            OpMode::RI(l, r) => {
                let l = self.load_xmm(ir, l);
                FMode::RI(l, r)
            }
            OpMode::IR(l, r) => {
                let r = self.load_xmm(ir, r);
                FMode::IR(l, r)
            }
        }
    }

    fn fetch_float_assume(&mut self, ir: &mut AsmIr, rhs: SlotId, class: FOpClass) -> Xmm {
        match class {
            FOpClass::Integer => self.load_xmm_fixnum(ir, rhs),
            FOpClass::Float => self.load_xmm(ir, rhs),
        }
    }
}
