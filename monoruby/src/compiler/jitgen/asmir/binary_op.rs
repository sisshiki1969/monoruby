use crate::bytecodegen::BinOpK;

use super::*;

impl AsmIr {
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
    pub(in crate::compiler::jitgen) fn gen_binop_integer(
        &mut self,
        bb: &mut BBContext,
        pc: BytecodePtr,
        kind: BinOpK,
        dst: Option<SlotId>,
        mode: OpMode,
    ) {
        match kind {
            BinOpK::Add | BinOpK::Mul | BinOpK::BitOr | BinOpK::BitAnd | BinOpK::BitXor => {
                let deopt = self.new_deopt(bb, pc);
                match mode {
                    OpMode::RR(lhs, rhs) => {
                        self.fetch_guard_fixnum(bb, lhs, GP::Rdi, deopt);
                        self.fetch_guard_fixnum(bb, rhs, GP::Rsi, deopt);
                    }
                    OpMode::RI(slot, _) | OpMode::IR(_, slot) => {
                        self.fetch_guard_fixnum(bb, slot, GP::Rdi, deopt);
                    }
                }
                self.integer_binop(bb, pc, kind, mode);
                self.reg2acc_fixnum(bb, GP::Rdi, dst);
            }
            BinOpK::Sub => {
                self.fetch_fixnum_binary(bb, pc, &mode);
                self.integer_binop(bb, pc, kind, mode);
                self.reg2acc_fixnum(bb, GP::Rdi, dst);
            }
            BinOpK::Exp => {
                self.fetch_fixnum_mode(bb, &mode, pc);
                self.unlink(bb, dst);
                self.integer_binop(bb, pc, kind, mode);
                self.reg2acc(bb, GP::Rax, dst);
            }
            BinOpK::Div => {
                self.fetch_fixnum_mode(bb, &mode, pc);
                self.unlink(bb, dst);
                self.integer_binop(bb, pc, kind, mode);
                self.reg2acc_fixnum(bb, GP::Rax, dst);
            }
            BinOpK::Rem => match mode {
                OpMode::RI(lhs, rhs) if rhs > 0 && (rhs as u64).is_power_of_two() => {
                    let deopt = self.new_deopt(bb, pc);
                    self.fetch_guard_fixnum(bb, lhs, GP::Rdi, deopt);
                    self.unlink(bb, dst);
                    self.integer_binop(bb, pc, kind, mode);
                    self.reg2acc_fixnum(bb, GP::Rdi, dst);
                }
                _ => {
                    self.fetch_fixnum_mode(bb, &mode, pc);
                    self.unlink(bb, dst);
                    self.integer_binop(bb, pc, kind, mode);
                    self.reg2acc_fixnum(bb, GP::Rax, dst);
                }
            },
        }
    }

    pub(in crate::compiler::jitgen) fn fetch_fixnum_binary(
        &mut self,
        bb: &mut BBContext,
        pc: BytecodePtr,
        mode: &OpMode,
    ) {
        let deopt = self.new_deopt(bb, pc);
        match mode {
            OpMode::RR(l, r) => {
                self.fetch_guard_fixnum(bb, *l, GP::Rdi, deopt);
                self.fetch_guard_fixnum(bb, *r, GP::Rsi, deopt);
            }
            OpMode::RI(l, _) => {
                self.fetch_guard_fixnum(bb, *l, GP::Rdi, deopt);
            }
            OpMode::IR(_, r) => {
                self.fetch_guard_fixnum(bb, *r, GP::Rsi, deopt);
            }
        }
    }

    fn fetch_fixnum_mode(&mut self, bb: &mut BBContext, mode: &OpMode, pc: BytecodePtr) {
        let deopt = self.new_deopt(bb, pc);
        match mode {
            OpMode::RR(lhs, rhs) => {
                self.fetch_guard_fixnum(bb, *lhs, GP::Rdi, deopt);
                self.fetch_guard_fixnum(bb, *rhs, GP::Rsi, deopt);
            }
            OpMode::RI(lhs, rhs) => {
                self.fetch_guard_fixnum(bb, *lhs, GP::Rdi, deopt);
                self.lit2reg(Value::i32(*rhs as i32), GP::Rsi);
            }
            OpMode::IR(lhs, rhs) => {
                self.lit2reg(Value::i32(*lhs as i32), GP::Rdi);
                self.fetch_guard_fixnum(bb, *rhs, GP::Rsi, deopt);
            }
        }
    }
}
