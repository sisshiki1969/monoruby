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
        self.def_concrete_value(dst, Value::bool(b));
    }
}

impl BBContext {
    fn check_concrete_integer(&self, mode: OpMode) -> Option<(i64, i64)> {
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

    fn check_concrete_float(&self, mode: OpMode) -> Option<(f64, f64)> {
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

    pub(super) fn check_concrete_integer_cmpbr(
        &mut self,
        mode: OpMode,
        kind: CmpKind,
        brkind: BrKind,
    ) -> Option<CompileResult> {
        if let Some((lhs, rhs)) = self.check_concrete_integer(mode) {
            let b = cmp(kind, lhs, rhs) ^ (brkind == BrKind::BrIfNot);
            return Some(if b {
                CompileResult::Branch
            } else {
                CompileResult::Continue
            });
        }
        None
    }

    pub(super) fn check_concrete_float_cmpbr(
        &mut self,
        mode: OpMode,
        kind: CmpKind,
        brkind: BrKind,
    ) -> Option<CompileResult> {
        if let Some((lhs, rhs)) = self.check_concrete_float(mode) {
            let b = cmp(kind, lhs, rhs) ^ (brkind == BrKind::BrIfNot);
            return Some(if b {
                CompileResult::Branch
            } else {
                CompileResult::Continue
            });
        }
        None
    }

    fn binop_integer_folded(
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
                    self.def_fixnum_value(dst, result);
                    return true;
                }
            }
            BinOpK::Sub => {
                if let Some(result) = lhs.checked_sub(rhs)
                    && Value::is_i63(result)
                {
                    self.def_fixnum_value(dst, result);
                    return true;
                }
            }
            BinOpK::Mul => {
                if let Some(result) = lhs.checked_mul(rhs)
                    && Value::is_i63(result)
                {
                    self.def_fixnum_value(dst, result);
                    return true;
                }
            }
            BinOpK::Div => {
                if let Some(result) = lhs.checked_div(rhs)
                    && Value::is_i63(result)
                {
                    self.def_fixnum_value(dst, result);
                    return true;
                }
            }
            BinOpK::Rem => {
                if let Some(result) = lhs.checked_rem(rhs)
                    && Value::is_i63(result)
                {
                    self.def_fixnum_value(dst, result);
                    return true;
                }
            }
            BinOpK::Exp => {}
            BinOpK::BitOr => {
                self.def_fixnum_value(dst, lhs | rhs);
                return true;
            }
            BinOpK::BitAnd => {
                self.def_fixnum_value(dst, lhs & rhs);
                return true;
            }
            BinOpK::BitXor => {
                self.def_fixnum_value(dst, lhs ^ rhs);
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
    pub(super) fn gen_binop_integer(
        &mut self,
        ir: &mut AsmIr,
        kind: BinOpK,
        dst: Option<SlotId>,
        mode: OpMode,
    ) {
        if let Some((lhs, rhs)) = self.check_concrete_integer(mode)
            && self.binop_integer_folded(kind, lhs, rhs, dst)
        {
            return;
        };

        match kind {
            BinOpK::Add | BinOpK::Mul | BinOpK::BitOr | BinOpK::BitAnd | BinOpK::BitXor => {
                let lhs = GP::Rdi;
                let rhs = GP::Rsi;
                let deopt = self.fetch_fixnum_comm(ir, lhs, rhs, mode);
                ir.integer_binop(kind, lhs, rhs, mode, deopt);
                self.reg2acc_fixnum(ir, lhs, dst);
            }
            BinOpK::Sub => {
                let lhs = GP::Rdi;
                let rhs = GP::Rsi;
                let deopt = self.fetch_fixnum_mode(ir, lhs, rhs, mode);
                ir.integer_binop(kind, lhs, rhs, mode, deopt);
                self.reg2acc_fixnum(ir, lhs, dst);
            }
            BinOpK::Exp => {
                self.fetch_fixnum_binary(ir, GP::Rdi, GP::Rsi, mode);
                ir.integer_exp(self);
                self.reg2acc(ir, GP::Rax, dst);
            }
            BinOpK::Div => {
                let lhs = GP::Rdi;
                let rhs = GP::Rsi;
                let deopt = self.fetch_fixnum_binary(ir, lhs, rhs, mode);
                ir.integer_binop(kind, lhs, rhs, mode, deopt);
                self.reg2acc_fixnum(ir, GP::Rax, dst);
            }
            BinOpK::Rem => match mode {
                OpMode::RI(lhs, rhs) if rhs > 0 && (rhs as u64).is_power_of_two() => {
                    self.fetch_fixnum_r_nodeopt(ir, lhs, GP::R15);
                    ir.reg_and(GP::R15, (rhs * 2 - 1) as i64 as u64);
                    self.reg2acc_fixnum(ir, GP::R15, dst);
                }
                _ => {
                    let lhs = GP::Rdi;
                    let rhs = GP::Rsi;
                    let deopt = self.fetch_fixnum_binary(ir, lhs, rhs, mode);
                    ir.integer_binop(kind, lhs, rhs, mode, deopt);
                    self.reg2acc_fixnum(ir, GP::Rax, dst);
                }
            },
        }
    }

    pub(super) fn gen_binop_float(&mut self, ir: &mut AsmIr, kind: BinOpK, info: BinOpInfo) {
        if let Some((lhs, rhs)) = self.check_concrete_float(info.mode) {
            match kind {
                BinOpK::Add => {
                    self.def_float_value(info.dst, lhs + rhs);
                    return;
                }
                BinOpK::Sub => {
                    self.def_float_value(info.dst, lhs - rhs);
                    return;
                }
                BinOpK::Mul => {
                    self.def_float_value(info.dst, lhs * rhs);
                    return;
                }
                BinOpK::Div => {
                    self.def_float_value(info.dst, lhs / rhs);
                    return;
                }
                _ => {}
            }
        };

        let fmode = self.fmode(ir, info);
        if let Some(dst) = info.dst {
            let dst = self.xmm_write(dst);
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
        if let Some((lhs, rhs)) = self.check_concrete_integer(mode) {
            self.fold_constant_cmp(kind, lhs, rhs, dst);
            return;
        };
        let (lhs, rhs) = self.fetch_fixnum_mode_nodeopt(ir, mode);
        ir.integer_cmp(mode, kind, lhs, rhs);
        self.rax2acc(ir, dst);
    }

    pub(super) fn gen_cmp_float(&mut self, ir: &mut AsmIr, info: BinOpInfo, kind: CmpKind) {
        if let Some((lhs, rhs)) = self.check_concrete_float(info.mode) {
            self.fold_constant_cmp(kind, lhs, rhs, info.dst);
            return;
        };
        let mode = self.fmode(ir, info);
        self.discard(info.dst);
        self.clear_above_next_sp();
        ir.push(AsmInst::FloatCmp { kind, mode });
        self.rax2acc(ir, info.dst);
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

    fn fetch_fixnum_comm(&mut self, ir: &mut AsmIr, lhs: GP, rhs: GP, mode: OpMode) -> AsmDeopt {
        match mode {
            OpMode::RR(l, r) => self.fetch_fixnum_rr(ir, l, r, lhs, rhs),
            OpMode::RI(slot, _) | OpMode::IR(_, slot) => self.fetch_fixnum_r(ir, slot, lhs),
        }
    }

    fn fetch_fixnum_mode(&mut self, ir: &mut AsmIr, lhs: GP, rhs: GP, mode: OpMode) -> AsmDeopt {
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

    fn fetch_fixnum_binary(&mut self, ir: &mut AsmIr, lhs: GP, rhs: GP, mode: OpMode) -> AsmDeopt {
        match mode {
            OpMode::RR(l, r) => self.fetch_fixnum_rr(ir, l, r, lhs, rhs),
            OpMode::RI(l, r) => {
                ir.lit2reg(Value::i32(r as i32), rhs);
                self.fetch_fixnum_r(ir, l, lhs)
            }
            OpMode::IR(l, r) => {
                ir.lit2reg(Value::i32(l as i32), lhs);
                self.fetch_fixnum_r(ir, r, rhs)
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
    ) -> AsmDeopt {
        self.fetch(ir, l, lhs);
        self.fetch(ir, r, rhs);
        let deopt = ir.new_deopt(self);
        self.guard_fixnum(ir, l, lhs, deopt);
        self.guard_fixnum(ir, r, rhs, deopt);
        deopt
    }

    fn fetch_fixnum_rr_nodeopt(&mut self, ir: &mut AsmIr, l: SlotId, r: SlotId, lhs: GP, rhs: GP) {
        self.fetch(ir, l, lhs);
        self.fetch(ir, r, rhs);
        if self.is_fixnum_literal(l).is_some() && self.is_fixnum_literal(r).is_some() {
            return;
        }
        let deopt = ir.new_deopt(self);
        self.guard_fixnum(ir, l, lhs, deopt);
        self.guard_fixnum(ir, r, rhs, deopt);
    }

    fn fetch_fixnum_r(&mut self, ir: &mut AsmIr, slot: SlotId, r: GP) -> AsmDeopt {
        if self.is_fixnum_literal(slot).is_some() {
            self.fetch(ir, slot, r);
            ir.new_deopt(self)
        } else {
            self.fetch(ir, slot, r);
            let deopt = ir.new_deopt(self);
            self.guard_fixnum(ir, slot, r, deopt);
            deopt
        }
    }

    fn fetch_fixnum_r_nodeopt(&mut self, ir: &mut AsmIr, slot: SlotId, r: GP) {
        if self.is_fixnum_literal(slot).is_some() {
            self.fetch(ir, slot, r);
        } else {
            self.fetch(ir, slot, r);
            let deopt = ir.new_deopt(self);
            self.guard_fixnum(ir, slot, r, deopt);
        }
    }
}

impl BBContext {
    /*///
    /// Fetch operands for binary operation according to *mode*.
    ///
    /// #### in
    /// - rdi: lhs
    /// - rsi: rhs
    ///
    pub(super) fn fetch_binary(&mut self, ir: &mut AsmIr, mode: OpMode) {
        match mode {
            OpMode::RR(lhs, rhs) => {
                self.fetch(ir, lhs, GP::Rdi);
                self.fetch(ir, rhs, GP::Rsi);
            }
            OpMode::RI(lhs, rhs) => {
                self.fetch(ir, lhs, GP::Rdi);
                ir.lit2reg(Value::i32(rhs as i32), GP::Rsi);
            }
            OpMode::IR(lhs, rhs) => {
                ir.lit2reg(Value::i32(lhs as i32), GP::Rdi);
                self.fetch(ir, rhs, GP::Rsi);
            }
        }
    }*/

    ///
    /// Fetch lhs operands for binary operation according to *mode*.
    ///
    /// #### in
    /// - rdi: lhs
    ///
    pub(super) fn fetch_lhs(&mut self, ir: &mut AsmIr, mode: OpMode) {
        match mode {
            OpMode::RR(lhs, _) | OpMode::RI(lhs, _) => {
                self.fetch(ir, lhs, GP::Rdi);
            }
            OpMode::IR(lhs, _) => {
                ir.lit2reg(Value::i32(lhs as i32), GP::Rdi);
            }
        }
    }

    fn fetch_float_assume(
        &mut self,
        ir: &mut AsmIr,
        rhs: SlotId,
        class: ClassId,
        deopt: AsmDeopt,
    ) -> Xmm {
        match class {
            INTEGER_CLASS => self.fetch_integer_for_xmm(ir, rhs, deopt),
            FLOAT_CLASS => self.fetch_float_for_xmm(ir, rhs, deopt),
            _ => unreachable!(),
        }
    }

    pub(super) fn fmode(&mut self, ir: &mut AsmIr, info: BinOpInfo) -> FMode {
        let deopt = ir.new_deopt(self);
        let BinOpInfo {
            mode,
            lhs_class,
            rhs_class,
            ..
        } = info;
        match mode {
            OpMode::RR(l, r) => {
                let (flhs, frhs) = if l != r {
                    (
                        self.fetch_float_assume(ir, l, lhs_class, deopt),
                        self.fetch_float_assume(ir, r, rhs_class, deopt),
                    )
                } else {
                    let lhs = self.fetch_float_assume(ir, l, lhs_class, deopt);
                    (lhs, lhs)
                };
                FMode::RR(flhs, frhs)
            }
            OpMode::RI(l, r) => {
                let l = self.fetch_float_for_xmm(ir, l, deopt);
                FMode::RI(l, r)
            }
            OpMode::IR(l, r) => {
                let r = self.fetch_float_for_xmm(ir, r, deopt);
                FMode::IR(l, r)
            }
        }
    }
}
