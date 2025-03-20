use crate::bytecodegen::BinOpK;

use super::*;

impl BBContext {
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
        if let Some((lhs, rhs)) = match mode {
            OpMode::RR(lhs, rhs) => {
                if let Some(lhs) = self.is_fixnum_literal(lhs)
                    && let Some(rhs) = self.is_fixnum_literal(rhs)
                {
                    Some((lhs, rhs))
                } else {
                    None
                }
            }
            OpMode::RI(lhs, rhs) => {
                if let Some(lhs) = self.is_fixnum_literal(lhs) {
                    Some((lhs, rhs as i64))
                } else {
                    None
                }
            }
            OpMode::IR(lhs, rhs) => {
                if let Some(rhs) = self.is_fixnum_literal(rhs) {
                    Some((lhs as i64, rhs))
                } else {
                    None
                }
            }
        } {
            match kind {
                BinOpK::Add => {
                    if let Some(result) = lhs.checked_add(rhs) {
                        if Value::is_i63(result) {
                            self.def_fixnum_value(dst, result);
                            return;
                        }
                    }
                }
                BinOpK::Sub => {
                    if let Some(result) = lhs.checked_sub(rhs) {
                        if Value::is_i63(result) {
                            self.def_fixnum_value(dst, result);
                            return;
                        }
                    }
                }
                BinOpK::Mul => {
                    if let Some(result) = lhs.checked_mul(rhs) {
                        if Value::is_i63(result) {
                            self.def_fixnum_value(dst, result);
                            return;
                        }
                    }
                }
                BinOpK::Div => {
                    if let Some(result) = lhs.checked_div(rhs) {
                        if Value::is_i63(result) {
                            self.def_fixnum_value(dst, result);
                            return;
                        }
                    }
                }
                BinOpK::Rem => {
                    if let Some(result) = lhs.checked_rem(rhs) {
                        if Value::is_i63(result) {
                            self.def_fixnum_value(dst, result);
                            return;
                        }
                    }
                }
                BinOpK::Exp => {}
                BinOpK::BitOr => {
                    self.def_fixnum_value(dst, lhs | rhs);
                    return;
                }
                BinOpK::BitAnd => {
                    self.def_fixnum_value(dst, lhs & rhs);
                    return;
                }
                BinOpK::BitXor => {
                    self.def_fixnum_value(dst, lhs ^ rhs);
                    return;
                }
            }
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
        if let Some((lhs, rhs)) = match info.mode {
            OpMode::RR(l, r) => {
                if let Some(lhs) = self.is_float_literal(l)
                    && let Some(rhs) = self.is_float_literal(r)
                {
                    Some((lhs, rhs))
                } else {
                    None
                }
            }
            OpMode::RI(l, r) => {
                if let Some(lhs) = self.is_float_literal(l) {
                    Some((lhs, r as f64))
                } else {
                    None
                }
            }
            OpMode::IR(l, r) => {
                if let Some(rhs) = self.is_float_literal(r) {
                    Some((l as f64, rhs))
                } else {
                    None
                }
            }
        } {
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
        let (lhs, rhs) = self.fetch_fixnum_mode_nodeopt(ir, mode);
        ir.integer_cmp(mode, kind, lhs, rhs);
        self.rax2acc(ir, dst);
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

    /*pub(super) fn gen_cmp_generic(&mut self, ir: &mut AsmIr, kind: CmpKind, info: BinOpInfo) {
        self.fetch_binary(ir, info.mode);
        self.generic_cmp(ir, kind);
        self.rax2acc(ir, info.dst);
    }*/
}

impl BBContext {
    ///
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
    }

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
