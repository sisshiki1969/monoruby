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
            BinOpK::Add | BinOpK::Mul | BinOpK::BitOr | BinOpK::BitAnd | BinOpK::BitXor => {
                let deopt = self.new_deopt(ir);
                match mode {
                    OpMode::RR(lhs, rhs) => {
                        self.fetch_fixnum(ir, lhs, GP::Rdi, deopt);
                        self.fetch_fixnum(ir, rhs, GP::Rsi, deopt);
                    }
                    OpMode::RI(slot, _) | OpMode::IR(_, slot) => {
                        self.fetch_fixnum(ir, slot, GP::Rdi, deopt);
                    }
                }
                self.integer_binop(ir, kind, mode);
                self.reg2acc_fixnum(ir, GP::Rdi, dst);
            }
            BinOpK::Sub => {
                self.fetch_fixnum_binary(ir, &mode);
                self.integer_binop(ir, kind, mode);
                self.reg2acc_fixnum(ir, GP::Rdi, dst);
            }
            BinOpK::Exp => {
                self.fetch_fixnum_mode(ir, &mode);
                self.discard(dst);
                self.integer_binop(ir, kind, mode);
                self.reg2acc(ir, GP::Rax, dst);
            }
            BinOpK::Div => {
                self.fetch_fixnum_mode(ir, &mode);
                self.discard(dst);
                self.integer_binop(ir, kind, mode);
                self.reg2acc_fixnum(ir, GP::Rax, dst);
            }
            BinOpK::Rem => match mode {
                OpMode::RI(lhs, rhs) if rhs > 0 && (rhs as u64).is_power_of_two() => {
                    let deopt = self.new_deopt(ir);
                    self.fetch_fixnum(ir, lhs, GP::Rdi, deopt);
                    self.discard(dst);
                    self.integer_binop(ir, kind, mode);
                    self.reg2acc_fixnum(ir, GP::Rdi, dst);
                }
                _ => {
                    self.fetch_fixnum_mode(ir, &mode);
                    self.discard(dst);
                    self.integer_binop(ir, kind, mode);
                    self.reg2acc_fixnum(ir, GP::Rax, dst);
                }
            },
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

    pub(super) fn fetch_fixnum_binary(&mut self, ir: &mut AsmIr, mode: &OpMode) {
        let deopt = self.new_deopt(ir);
        match mode {
            OpMode::RR(l, r) => {
                self.fetch_fixnum(ir, *l, GP::Rdi, deopt);
                self.fetch_fixnum(ir, *r, GP::Rsi, deopt);
            }
            OpMode::RI(l, _) => {
                self.fetch_fixnum(ir, *l, GP::Rdi, deopt);
            }
            OpMode::IR(_, r) => {
                self.fetch_fixnum(ir, *r, GP::Rsi, deopt);
            }
        }
    }

    fn fetch_fixnum_mode(&mut self, ir: &mut AsmIr, mode: &OpMode) {
        let deopt = self.new_deopt(ir);
        match mode {
            OpMode::RR(lhs, rhs) => {
                self.fetch_fixnum(ir, *lhs, GP::Rdi, deopt);
                self.fetch_fixnum(ir, *rhs, GP::Rsi, deopt);
            }
            OpMode::RI(lhs, rhs) => {
                self.fetch_fixnum(ir, *lhs, GP::Rdi, deopt);
                ir.lit2reg(Value::i32(*rhs as i32), GP::Rsi);
            }
            OpMode::IR(lhs, rhs) => {
                ir.lit2reg(Value::i32(*lhs as i32), GP::Rdi);
                self.fetch_fixnum(ir, *rhs, GP::Rsi, deopt);
            }
        }
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
    fn integer_binop(&self, ir: &mut AsmIr, kind: BinOpK, mode: OpMode) {
        let using_xmm = self.get_using_xmm();
        let (deopt, error) = self.new_deopt_error(ir);
        ir.push(AsmInst::IntegerBinOp {
            kind,
            mode,
            using_xmm,
            deopt,
            error,
        });
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
                self.fetch_for_gpr(ir, lhs, GP::Rdi);
                self.fetch_for_gpr(ir, rhs, GP::Rsi);
            }
            OpMode::RI(lhs, rhs) => {
                self.fetch_for_gpr(ir, lhs, GP::Rdi);
                ir.lit2reg(Value::i32(rhs as i32), GP::Rsi);
            }
            OpMode::IR(lhs, rhs) => {
                ir.lit2reg(Value::i32(lhs as i32), GP::Rdi);
                self.fetch_for_gpr(ir, rhs, GP::Rsi);
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
                self.fetch_for_gpr(ir, lhs, GP::Rdi);
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
