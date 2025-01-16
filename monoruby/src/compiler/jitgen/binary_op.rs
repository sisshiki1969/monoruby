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
        match kind {
            BinOpK::Add | BinOpK::BitOr | BinOpK::BitAnd | BinOpK::BitXor => {
                let lhs = GP::R15;
                let rhs = GP::Rsi;
                let deopt = self.fetch_fixnum_comm(ir, lhs, rhs, mode);
                self.integer_binop(ir, kind, lhs, rhs, mode, deopt);
                self.reg2acc_fixnum(ir, lhs, dst);
            }
            BinOpK::Mul => {
                let lhs = GP::R15;
                let rhs = GP::Rsi;
                let deopt = self.fetch_fixnum_comm(ir, lhs, rhs, mode);
                self.integer_binop(ir, kind, lhs, rhs, mode, deopt);
                self.reg2acc_fixnum(ir, lhs, dst);
            }
            BinOpK::Sub => {
                let lhs = GP::R15;
                let rhs = GP::Rsi;
                let deopt = self.fetch_fixnum_mode(ir, lhs, rhs, mode);
                self.integer_binop(ir, kind, lhs, rhs, mode, deopt);
                self.reg2acc_fixnum(ir, lhs, dst);
            }
            BinOpK::Exp => {
                self.fetch_fixnum_binary(ir, GP::Rdi, GP::Rsi, mode);
                self.integer_exp(ir);
                self.reg2acc(ir, GP::Rax, dst);
            }
            BinOpK::Div => {
                let lhs = GP::Rdi;
                let rhs = GP::Rsi;
                let deopt = self.fetch_fixnum_binary(ir, lhs, rhs, mode);
                self.integer_binop(ir, kind, lhs, rhs, mode, deopt);
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
                    self.integer_binop(ir, kind, lhs, rhs, mode, deopt);
                    self.reg2acc_fixnum(ir, GP::Rax, dst);
                }
            },
        }
    }

    pub(super) fn gen_cmp_integer(
        &mut self,
        ir: &mut AsmIr,
        kind: CmpKind,
        dst: Option<SlotId>,
        mode: OpMode,
    ) {
        let lhs = GP::Rdi;
        let rhs = GP::Rsi;
        self.fetch_fixnum_mode_nodeopt(ir, lhs, rhs, mode);
        ir.push(AsmInst::IntegerCmp {
            kind,
            mode,
            lhs,
            rhs,
        });
        self.rax2acc(ir, dst);
    }

    pub(super) fn gen_cmpbr_integer(
        &mut self,
        ir: &mut AsmIr,
        kind: CmpKind,
        mode: OpMode,
        dst: Option<SlotId>,
        brkind: BrKind,
        branch_dest: JitLabel,
    ) {
        let lhs = GP::Rdi;
        let rhs = GP::Rsi;
        self.fetch_fixnum_mode_nodeopt(ir, lhs, rhs, mode);
        self.discard(dst);
        self.clear_above_next_sp();
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

    fn fetch_fixnum_mode_nodeopt(&mut self, ir: &mut AsmIr, lhs: GP, rhs: GP, mode: OpMode) {
        match mode {
            OpMode::RR(l, r) => self.fetch_fixnum_rr_nodeopt(ir, l, r, lhs, rhs),
            OpMode::RI(l, _) => self.fetch_fixnum_r_nodeopt(ir, l, lhs),
            OpMode::IR(_, r) => self.fetch_fixnum_r_nodeopt(ir, r, rhs),
        }
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
        if self.is_r15(r) {
            self.fetch(ir, r, rhs);
            self.fetch(ir, l, lhs);
        } else {
            self.fetch(ir, l, lhs);
            self.fetch(ir, r, rhs);
        }
        let deopt = self.new_deopt(ir);
        self.guard_fixnum(ir, l, lhs, deopt);
        self.guard_fixnum(ir, r, rhs, deopt);
        deopt
    }

    fn fetch_fixnum_rr_nodeopt(&mut self, ir: &mut AsmIr, l: SlotId, r: SlotId, lhs: GP, rhs: GP) {
        if self.is_r15(r) {
            self.fetch(ir, r, rhs);
            self.fetch(ir, l, lhs);
        } else {
            self.fetch(ir, l, lhs);
            self.fetch(ir, r, rhs);
        }
        if self.is_fixnum_literal(l).is_some() && self.is_fixnum_literal(r).is_some() {
            return;
        }
        let deopt = self.new_deopt(ir);
        self.guard_fixnum(ir, l, lhs, deopt);
        self.guard_fixnum(ir, r, rhs, deopt);
    }

    fn fetch_fixnum_r(&mut self, ir: &mut AsmIr, slot: SlotId, r: GP) -> AsmDeopt {
        if self.is_fixnum_literal(slot).is_some() {
            self.fetch(ir, slot, r);
            self.new_deopt(ir)
        } else {
            self.fetch(ir, slot, r);
            let deopt = self.new_deopt(ir);
            self.guard_fixnum(ir, slot, r, deopt);
            deopt
        }
    }

    fn fetch_fixnum_r_nodeopt(&mut self, ir: &mut AsmIr, slot: SlotId, r: GP) {
        if self.is_fixnum_literal(slot).is_some() {
            self.fetch(ir, slot, r);
        } else {
            self.fetch(ir, slot, r);
            let deopt = self.new_deopt(ir);
            self.guard_fixnum(ir, slot, r, deopt);
        }
    }

    pub(super) fn gen_cmp_generic(&mut self, ir: &mut AsmIr, kind: CmpKind, info: BinOpInfo) {
        self.fetch_binary(ir, info.mode);
        self.generic_cmp(ir, kind);
        self.rax2acc(ir, info.dst);
    }

    pub(super) fn gen_cmpbr_generic(
        &mut self,
        ir: &mut AsmIr,
        kind: CmpKind,
        mode: OpMode,
        dst: Option<SlotId>,
        brkind: BrKind,
        branch_dest: JitLabel,
    ) {
        self.fetch_binary(ir, mode);
        self.discard(dst);
        self.clear_above_next_sp();
        self.generic_cmp(ir, kind);
        ir.push(AsmInst::GenericCondBr {
            brkind,
            branch_dest,
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
    fn integer_binop(
        &self,
        ir: &mut AsmIr,
        kind: BinOpK,
        lhs: GP,
        rhs: GP,
        mode: OpMode,
        deopt: AsmDeopt,
    ) {
        ir.push(AsmInst::IntegerBinOp {
            kind,
            lhs,
            rhs,
            mode,
            deopt,
        });
    }

    fn integer_exp(&self, ir: &mut AsmIr) {
        let using_xmm = self.get_using_xmm();
        ir.push(AsmInst::IntegerExp { using_xmm });
    }

    ///
    /// Generic integer binary operation.
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
    pub(super) fn generic_binop(&self, ir: &mut AsmIr, kind: BinOpK) {
        let using_xmm = self.get_using_xmm();
        let error = self.new_error(ir);
        ir.push(AsmInst::GenericBinOp { kind, using_xmm });
        ir.handle_error(error);
    }

    pub(super) fn generic_cmp(&self, ir: &mut AsmIr, kind: CmpKind) {
        let using_xmm = self.get_using_xmm();
        let error = self.new_error(ir);
        ir.push(AsmInst::GenericCmp { kind, using_xmm });
        ir.handle_error(error);
    }
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
        let deopt = self.new_deopt(ir);
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
