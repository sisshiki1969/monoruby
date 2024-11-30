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
                        bb.fetch_guard_fixnum(self, lhs, GP::Rdi, deopt);
                        bb.fetch_guard_fixnum(self, rhs, GP::Rsi, deopt);
                    }
                    OpMode::RI(slot, _) | OpMode::IR(_, slot) => {
                        bb.fetch_guard_fixnum(self, slot, GP::Rdi, deopt);
                    }
                }
                self.integer_binop(bb, pc, kind, mode);
                bb.reg2acc_fixnum(self, GP::Rdi, dst);
            }
            BinOpK::Sub => {
                self.fetch_fixnum_binary(bb, pc, &mode);
                self.integer_binop(bb, pc, kind, mode);
                bb.reg2acc_fixnum(self, GP::Rdi, dst);
            }
            BinOpK::Exp => {
                self.fetch_fixnum_mode(bb, &mode, pc);
                bb.unlink(self, dst);
                self.integer_binop(bb, pc, kind, mode);
                bb.reg2acc(self, GP::Rax, dst);
            }
            BinOpK::Div => {
                self.fetch_fixnum_mode(bb, &mode, pc);
                bb.unlink(self, dst);
                self.integer_binop(bb, pc, kind, mode);
                bb.reg2acc_fixnum(self, GP::Rax, dst);
            }
            BinOpK::Rem => match mode {
                OpMode::RI(lhs, rhs) if rhs > 0 && (rhs as u64).is_power_of_two() => {
                    let deopt = self.new_deopt(bb, pc);
                    bb.fetch_guard_fixnum(self, lhs, GP::Rdi, deopt);
                    bb.unlink(self, dst);
                    self.integer_binop(bb, pc, kind, mode);
                    bb.reg2acc_fixnum(self, GP::Rdi, dst);
                }
                _ => {
                    self.fetch_fixnum_mode(bb, &mode, pc);
                    bb.unlink(self, dst);
                    self.integer_binop(bb, pc, kind, mode);
                    bb.reg2acc_fixnum(self, GP::Rax, dst);
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
                bb.fetch_guard_fixnum(self, *l, GP::Rdi, deopt);
                bb.fetch_guard_fixnum(self, *r, GP::Rsi, deopt);
            }
            OpMode::RI(l, _) => {
                bb.fetch_guard_fixnum(self, *l, GP::Rdi, deopt);
            }
            OpMode::IR(_, r) => {
                bb.fetch_guard_fixnum(self, *r, GP::Rsi, deopt);
            }
        }
    }

    fn fetch_fixnum_mode(&mut self, bb: &mut BBContext, mode: &OpMode, pc: BytecodePtr) {
        let deopt = self.new_deopt(bb, pc);
        match mode {
            OpMode::RR(lhs, rhs) => {
                bb.fetch_guard_fixnum(self, *lhs, GP::Rdi, deopt);
                bb.fetch_guard_fixnum(self, *rhs, GP::Rsi, deopt);
            }
            OpMode::RI(lhs, rhs) => {
                bb.fetch_guard_fixnum(self, *lhs, GP::Rdi, deopt);
                self.lit2reg(Value::i32(*rhs as i32), GP::Rsi);
            }
            OpMode::IR(lhs, rhs) => {
                self.lit2reg(Value::i32(*lhs as i32), GP::Rdi);
                bb.fetch_guard_fixnum(self, *rhs, GP::Rsi, deopt);
            }
        }
    }
}
