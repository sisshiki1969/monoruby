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
        pc: BytecodePtr,
        kind: BinOpK,
        dst: Option<SlotId>,
        mode: OpMode,
    ) {
        match kind {
            BinOpK::Add | BinOpK::Mul | BinOpK::BitOr | BinOpK::BitAnd | BinOpK::BitXor => {
                let deopt = ir.new_deopt(self, pc);
                match mode {
                    OpMode::RR(lhs, rhs) => {
                        self.fetch_guard_fixnum(ir, lhs, GP::Rdi, deopt);
                        self.fetch_guard_fixnum(ir, rhs, GP::Rsi, deopt);
                    }
                    OpMode::RI(slot, _) | OpMode::IR(_, slot) => {
                        self.fetch_guard_fixnum(ir, slot, GP::Rdi, deopt);
                    }
                }
                self.integer_binop(ir, pc, kind, mode);
                self.reg2acc_fixnum(ir, GP::Rdi, dst);
            }
            BinOpK::Sub => {
                self.fetch_fixnum_binary(ir, pc, &mode);
                self.integer_binop(ir, pc, kind, mode);
                self.reg2acc_fixnum(ir, GP::Rdi, dst);
            }
            BinOpK::Exp => {
                self.fetch_fixnum_mode(ir, &mode, pc);
                self.unlink(ir, dst);
                self.integer_binop(ir, pc, kind, mode);
                self.reg2acc(ir, GP::Rax, dst);
            }
            BinOpK::Div => {
                self.fetch_fixnum_mode(ir, &mode, pc);
                self.unlink(ir, dst);
                self.integer_binop(ir, pc, kind, mode);
                self.reg2acc_fixnum(ir, GP::Rax, dst);
            }
            BinOpK::Rem => match mode {
                OpMode::RI(lhs, rhs) if rhs > 0 && (rhs as u64).is_power_of_two() => {
                    let deopt = ir.new_deopt(self, pc);
                    self.fetch_guard_fixnum(ir, lhs, GP::Rdi, deopt);
                    self.unlink(ir, dst);
                    self.integer_binop(ir, pc, kind, mode);
                    self.reg2acc_fixnum(ir, GP::Rdi, dst);
                }
                _ => {
                    self.fetch_fixnum_mode(ir, &mode, pc);
                    self.unlink(ir, dst);
                    self.integer_binop(ir, pc, kind, mode);
                    self.reg2acc_fixnum(ir, GP::Rax, dst);
                }
            },
        }
    }

    pub(super) fn gen_binop_generic(
        &mut self,
        ir: &mut AsmIr,
        pc: BytecodePtr,
        kind: BinOpK,
        mode: OpMode,
        dst: Option<SlotId>,
    ) {
        self.fetch_binary(ir, mode);
        self.generic_binop(ir, pc, kind);
        self.rax2acc(ir, dst);
    }

    pub(super) fn gen_cmp_integer(
        &mut self,
        ir: &mut AsmIr,
        pc: BytecodePtr,
        kind: CmpKind,
        mode: OpMode,
        dst: Option<SlotId>,
    ) {
        self.fetch_fixnum_binary(ir, pc, &mode);
        ir.inst.push(AsmInst::IntegerCmp { kind, mode });
        self.rax2acc(ir, dst);
    }

    pub(super) fn gen_cmpbr_integer(
        &mut self,
        ir: &mut AsmIr,
        pc: BytecodePtr,
        kind: CmpKind,
        mode: OpMode,
        dst: Option<SlotId>,
        brkind: BrKind,
        branch_dest: JitLabel,
    ) {
        self.fetch_fixnum_binary(ir, pc, &mode);
        self.unlink(ir, dst);
        self.clear(ir);
        ir.integer_cmp_br(mode, kind, brkind, branch_dest);
    }

    pub(super) fn gen_cmp_generic(
        &mut self,
        ir: &mut AsmIr,
        pc: BytecodePtr,
        kind: CmpKind,
        mode: OpMode,
        dst: Option<SlotId>,
    ) {
        self.fetch_binary(ir, mode);
        self.generic_cmp(ir, pc, kind);
        self.rax2acc(ir, dst);
    }

    pub(super) fn gen_cmpbr_generic(
        &mut self,
        ir: &mut AsmIr,
        pc: BytecodePtr,
        kind: CmpKind,
        mode: OpMode,
        dst: Option<SlotId>,
        brkind: BrKind,
        branch_dest: JitLabel,
    ) {
        self.fetch_binary(ir, mode);
        self.unlink(ir, dst);
        self.clear(ir);
        self.generic_cmp(ir, pc, kind);
        ir.inst.push(AsmInst::GenericCondBr {
            brkind,
            branch_dest,
        });
    }

    fn fetch_fixnum_binary(&mut self, ir: &mut AsmIr, pc: BytecodePtr, mode: &OpMode) {
        let deopt = ir.new_deopt(self, pc);
        match mode {
            OpMode::RR(l, r) => {
                self.fetch_guard_fixnum(ir, *l, GP::Rdi, deopt);
                self.fetch_guard_fixnum(ir, *r, GP::Rsi, deopt);
            }
            OpMode::RI(l, _) => {
                self.fetch_guard_fixnum(ir, *l, GP::Rdi, deopt);
            }
            OpMode::IR(_, r) => {
                self.fetch_guard_fixnum(ir, *r, GP::Rsi, deopt);
            }
        }
    }

    fn fetch_fixnum_mode(&mut self, ir: &mut AsmIr, mode: &OpMode, pc: BytecodePtr) {
        let deopt = ir.new_deopt(self, pc);
        match mode {
            OpMode::RR(lhs, rhs) => {
                self.fetch_guard_fixnum(ir, *lhs, GP::Rdi, deopt);
                self.fetch_guard_fixnum(ir, *rhs, GP::Rsi, deopt);
            }
            OpMode::RI(lhs, rhs) => {
                self.fetch_guard_fixnum(ir, *lhs, GP::Rdi, deopt);
                ir.lit2reg(Value::i32(*rhs as i32), GP::Rsi);
            }
            OpMode::IR(lhs, rhs) => {
                ir.lit2reg(Value::i32(*lhs as i32), GP::Rdi);
                self.fetch_guard_fixnum(ir, *rhs, GP::Rsi, deopt);
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
    fn integer_binop(&self, ir: &mut AsmIr, pc: BytecodePtr, kind: BinOpK, mode: OpMode) {
        let using_xmm = self.get_using_xmm();
        let (deopt, error) = ir.new_deopt_error(self, pc);
        ir.inst.push(AsmInst::IntegerBinOp {
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
    fn generic_binop(&self, ir: &mut AsmIr, pc: BytecodePtr, kind: BinOpK) {
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(self, pc);
        ir.inst.push(AsmInst::GenericBinOp { kind, using_xmm });
        ir.handle_error(error);
    }

    fn generic_cmp(&self, ir: &mut AsmIr, pc: BytecodePtr, kind: CmpKind) {
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(self, pc);
        ir.inst.push(AsmInst::GenericCmp { kind, using_xmm });
        ir.handle_error(error);
    }
}

impl BBContext {
    pub(super) fn gen_binop_float(
        &mut self,
        ir: &mut AsmIr,
        pc: BytecodePtr,
        kind: BinOpK,
        mode: OpMode,
        lhs_class: ClassId,
        rhs_class: ClassId,
        dst: Option<SlotId>,
    ) {
        let fmode = self.fmode(&mode, ir, lhs_class, rhs_class, pc);
        if let Some(dst) = dst {
            let dst = self.xmm_write(ir, dst);
            let using_xmm = self.get_using_xmm();
            ir.xmm_binop(kind, fmode, dst, using_xmm);
        }
    }

    pub(super) fn gen_cmp_float(
        &mut self,
        ir: &mut AsmIr,
        pc: BytecodePtr,
        kind: CmpKind,
        mode: OpMode,
        lhs_class: ClassId,
        rhs_class: ClassId,
        dst: Option<SlotId>,
    ) {
        if kind != CmpKind::Cmp {
            let mode = self.fmode(&mode, ir, lhs_class, rhs_class, pc);
            self.unlink(ir, dst);
            self.clear(ir);
            ir.inst.push(AsmInst::FloatCmp { kind, mode });
        } else {
            self.fetch_binary(ir, mode);
            self.unlink(ir, dst);
            self.generic_cmp(ir, pc, kind);
        }
        self.rax2acc(ir, dst);
    }

    pub(super) fn gen_cmpbr_float(
        &mut self,
        ir: &mut AsmIr,
        pc: BytecodePtr,
        kind: CmpKind,
        mode: OpMode,
        lhs_class: ClassId,
        rhs_class: ClassId,
        dst: Option<SlotId>,
        brkind: BrKind,
        branch_dest: JitLabel,
    ) {
        let mode = self.fmode(&mode, ir, lhs_class, rhs_class, pc);
        self.unlink(ir, dst);
        self.clear(ir);
        ir.float_cmp_br(mode, kind, brkind, branch_dest);
    }

    fn fetch_binary(&mut self, ir: &mut AsmIr, mode: OpMode) {
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

    fn fmode(
        &mut self,
        mode: &OpMode,
        ir: &mut AsmIr,
        lhs_class: ClassId,
        rhs_class: ClassId,
        pc: BytecodePtr,
    ) -> FMode {
        let deopt = ir.new_deopt(self, pc);
        match mode {
            OpMode::RR(l, r) => {
                let (flhs, frhs) = if l != r {
                    (
                        self.fetch_float_assume(ir, *l, lhs_class, deopt),
                        self.fetch_float_assume(ir, *r, rhs_class, deopt),
                    )
                } else {
                    let lhs = self.fetch_float_assume(ir, *l, lhs_class, deopt);
                    (lhs, lhs)
                };
                FMode::RR(flhs, frhs)
            }
            OpMode::RI(l, r) => {
                let l = self.fetch_float_for_xmm(ir, *l, deopt);
                FMode::RI(l, *r)
            }
            OpMode::IR(l, r) => {
                let r = self.fetch_float_for_xmm(ir, *r, deopt);
                FMode::IR(*l, r)
            }
        }
    }
}
