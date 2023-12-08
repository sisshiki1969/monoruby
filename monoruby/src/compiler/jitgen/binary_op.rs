use crate::bytecodegen::BinOpK;
use ruruby_parse::CmpKind;

use super::*;

mod float_binop;

macro_rules! cmp_main {
    ($op:ident) => {
        paste! {
            pub(in crate::compiler) fn [<icmp_ $op>](&mut self) {
                monoasm! { &mut self.jit,
                    xorq rax, rax;
                    cmpq rdi, rsi;
                    [<set $op>] rax;
                    shlq rax, 3;
                    orq rax, (FALSE_VALUE);
                };
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        cmp_main!($op1);
        cmp_main!($($op2),+);
    };
}

macro_rules! cmp_opt_main {
    (($op:ident, $rev_op:ident, $sop:ident, $rev_sop:ident)) => {
        paste! {
            fn [<condbr_int_ $sop>](&mut self, branch_dest: DestLabel, brkind: BrKind) {
                match brkind {
                    BrKind::BrIf => monoasm! { &mut self.jit,
                        [<j $sop>] branch_dest;
                    },
                    BrKind::BrIfNot => monoasm! { &mut self.jit,
                        [<j $rev_sop>] branch_dest;
                    },
                }
            }

            fn [<condbr_float_ $sop>](&mut self, branch_dest: DestLabel, brkind: BrKind) {
                match brkind {
                    BrKind::BrIf => monoasm! { &mut self.jit,
                        [<j $op>] branch_dest;
                    },
                    BrKind::BrIfNot => monoasm! { &mut self.jit,
                        [<j $rev_op>] branch_dest;
                    },
                }
            }
        }
    };
    (($op1:ident, $rev_op1:ident, $sop1:ident, $rev_sop1:ident), $(($op2:ident, $rev_op2:ident, $sop2:ident, $rev_sop2:ident)),+) => {
        cmp_opt_main!(($op1, $rev_op1, $sop1, $rev_sop1));
        cmp_opt_main!($(($op2, $rev_op2, $sop2, $rev_sop2)),+);
    };
}

impl Codegen {
    pub(super) fn setflag_float(&mut self, kind: CmpKind) {
        match kind {
            CmpKind::Eq | CmpKind::TEq => monoasm! { &mut self.jit, seteq rax; },
            CmpKind::Ne => monoasm! { &mut self.jit, setne rax; },
            CmpKind::Ge => monoasm! { &mut self.jit, setae rax; },
            CmpKind::Gt => monoasm! { &mut self.jit, seta rax; },
            CmpKind::Le => monoasm! { &mut self.jit, setbe rax; },
            CmpKind::Lt => monoasm! { &mut self.jit, setb rax; },
            _ => unimplemented!(),
        }
        monoasm! { &mut self.jit,
            shlq rax, 3;
            orq rax, (FALSE_VALUE);
        };
    }

    pub(super) fn generic_cmp(&mut self, kind: &CmpKind, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        match kind {
            CmpKind::Eq => self.call_binop(cmp_eq_values),
            CmpKind::Ne => self.call_binop(cmp_ne_values),
            CmpKind::Ge => self.call_binop(cmp_ge_values),
            CmpKind::Gt => self.call_binop(cmp_gt_values),
            CmpKind::Le => self.call_binop(cmp_le_values),
            CmpKind::Lt => self.call_binop(cmp_lt_values),
            CmpKind::TEq => self.call_binop(cmp_teq_values),
            CmpKind::Cmp => self.call_binop(cmp_cmp_values),
        }
        self.xmm_restore(using_xmm);
    }

    cmp_main!(eq, ne, lt, le, gt, ge);

    pub(super) fn integer_cmp(&mut self, kind: CmpKind) {
        match kind {
            CmpKind::Eq => self.icmp_eq(),
            CmpKind::Ne => self.icmp_ne(),
            CmpKind::Ge => self.icmp_ge(),
            CmpKind::Gt => self.icmp_gt(),
            CmpKind::Le => self.icmp_le(),
            CmpKind::Lt => self.icmp_lt(),
            CmpKind::TEq => self.icmp_eq(),
            CmpKind::Cmp => self.icmp_cmp(),
        }
    }

    pub(super) fn cmp_opt_generic(
        &mut self,
        ctx: &BBContext,
        kind: CmpKind,
        branch_dest: DestLabel,
        brkind: BrKind,
        pc: BcPc,
    ) {
        self.generic_cmp(&kind, ctx.get_using_xmm());
        self.jit_handle_error(ctx, pc);
        monoasm!( &mut self.jit,
            orq  rax, 0x10;
            cmpq rax, (FALSE_VALUE);
            // if true, Z=0(not set).
        );
        match brkind {
            BrKind::BrIf => monoasm! { &mut self.jit,
                jnz branch_dest;
            },
            BrKind::BrIfNot => monoasm! { &mut self.jit,
                jz  branch_dest;
            },
        }
    }

    pub(super) fn condbr_int(&mut self, kind: CmpKind, branch_dest: DestLabel, brkind: BrKind) {
        match kind {
            CmpKind::Eq => self.condbr_int_eq(branch_dest, brkind),
            CmpKind::Ne => self.condbr_int_ne(branch_dest, brkind),
            CmpKind::Ge => self.condbr_int_ge(branch_dest, brkind),
            CmpKind::Gt => self.condbr_int_gt(branch_dest, brkind),
            CmpKind::Le => self.condbr_int_le(branch_dest, brkind),
            CmpKind::Lt => self.condbr_int_lt(branch_dest, brkind),
            CmpKind::TEq => self.condbr_int_eq(branch_dest, brkind),
            _ => unreachable!(),
        }
    }

    cmp_opt_main!(
        (eq, ne, eq, ne),
        (ne, eq, ne, eq),
        (a, be, gt, le),
        (b, ae, lt, ge),
        (ae, b, ge, lt),
        (be, a, le, gt)
    );

    pub(super) fn condbr_float(&mut self, kind: CmpKind, branch_dest: DestLabel, brkind: BrKind) {
        match kind {
            CmpKind::Eq => self.condbr_float_eq(branch_dest, brkind),
            CmpKind::Ne => self.condbr_float_ne(branch_dest, brkind),
            CmpKind::Ge => self.condbr_float_ge(branch_dest, brkind),
            CmpKind::Gt => self.condbr_float_gt(branch_dest, brkind),
            CmpKind::Le => self.condbr_float_le(branch_dest, brkind),
            CmpKind::Lt => self.condbr_float_lt(branch_dest, brkind),
            CmpKind::TEq => self.condbr_float_eq(branch_dest, brkind),
            _ => unreachable!(),
        }
    }

    pub(super) fn cmp_opt_float(
        &mut self,
        ctx: &mut BBContext,
        mode: OpMode,
        ret: Option<SlotId>,
        pc: BcPc,
    ) {
        match mode {
            OpMode::RR(lhs, rhs) => {
                let (flhs, frhs) = self.fetch_float_binary(ctx, lhs, rhs, pc);
                monoasm! { &mut self.jit,
                    ucomisd xmm(flhs.enc()), xmm(frhs.enc());
                };
            }
            OpMode::RI(lhs, rhs) => {
                let rhs_label = self.jit.const_f64(rhs as f64);
                let flhs = self.fetch_float_assume_float(ctx, lhs, pc);
                monoasm! { &mut self.jit,
                    ucomisd xmm(flhs.enc()), [rip + rhs_label];
                };
            }
            _ => unreachable!(),
        }
        ctx.release(ret);
    }

    pub(super) fn cmp_opt_integer(&mut self, ctx: &mut BBContext, mode: OpMode, pc: BcPc) {
        let deopt = self.gen_deopt(pc, ctx);
        match mode {
            OpMode::RR(lhs, rhs) => {
                self.load_guard_binary_fixnum(lhs, rhs, deopt);
                monoasm!( &mut self.jit,
                    cmpq rdi, rsi;
                );
            }
            OpMode::RI(lhs, rhs) => {
                self.load_guard_rdi_fixnum(lhs, deopt);
                monoasm!( &mut self.jit,
                    cmpq rdi, (Value::i32(rhs as i32).id());
                );
            }
            OpMode::IR(lhs, rhs) => {
                self.load_guard_rsi_fixnum(rhs, deopt);
                monoasm!( &mut self.jit,
                    movq rdi, (Value::i32(lhs as i32).id());
                    cmpq rdi, rsi;
                );
            }
        }
    }
}

impl Codegen {
    pub(crate) fn load_guard_rdi_fixnum(&mut self, reg: SlotId, deopt: DestLabel) {
        self.load_rdi(reg);
        self.guard_rdi_fixnum(deopt);
    }

    fn load_guard_rsi_fixnum(&mut self, reg: SlotId, deopt: DestLabel) {
        self.load_rsi(reg);
        self.guard_rsi_fixnum(deopt);
    }

    pub(crate) fn load_guard_binary_fixnum(&mut self, lhs: SlotId, rhs: SlotId, deopt: DestLabel) {
        self.load_binary_args(lhs, rhs);
        self.guard_rdi_fixnum(deopt);
        self.guard_rsi_fixnum(deopt);
    }

    pub(super) fn load_binary_args_with_mode(&mut self, mode: &OpMode) {
        match *mode {
            OpMode::RR(lhs, rhs) => self.load_binary_args(lhs, rhs),
            OpMode::RI(lhs, rhs) => {
                self.load_rdi(lhs);
                monoasm!( &mut self.jit,
                    movq rsi, (Value::i32(rhs as i32).id());
                );
            }
            OpMode::IR(lhs, rhs) => {
                monoasm!( &mut self.jit,
                    movq rdi, (Value::i32(lhs as i32).id());
                );
                self.load_rsi(rhs);
            }
        }
    }

    fn shift_under(&mut self, under: DestLabel, after: DestLabel) {
        self.jit.select_page(1);
        let zero = self.jit.label();
        monoasm!( &mut self.jit,
        under:
            testq rdi, rdi;
            jns zero;
            xorq rdi, rdi;
            subq rdi, 1;
            jmp after;
        zero:
            xorq rdi, rdi;
            jmp after;
        );
        self.jit.select_page(0);
    }

    ///
    /// gen code for shift-right of integer.
    ///
    /// ### in
    /// - rdi: lhs:Value
    /// - rsi: rhs:Value
    ///
    /// ### out
    /// - rdi: result:Value
    ///
    /// ### destroy
    /// - rax
    /// - rcx
    ///
    pub(crate) fn gen_shr(&mut self, deopt: DestLabel) {
        let shl = self.jit.label();
        let after = self.jit.label();
        let under = self.jit.label();
        monoasm!( &mut self.jit,
            movq rcx, rsi;
            sarq rcx, 1;
            js shl;
            cmpq rcx, 64;
            jge under;
            sarq rdi, rcx;
        after:
            orq rdi, 1;
        );
        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        shl:
            negq rcx;
            lzcntq rax, rdi;
            cmpq rcx, rax;
            jgt deopt;
            subq rdi, 1;
            salq rdi, rcx;
            jmp after;
        );
        self.jit.select_page(0);
        self.shift_under(under, after);
    }

    pub(crate) fn gen_shr_imm(&mut self, imm: u8) {
        let after = self.jit.label();
        let under = self.jit.label();
        monoasm!( &mut self.jit,
            movq rcx, (imm);
            cmpq rcx, 64;
            jge under;
            sarq rdi, rcx;
        after:
            orq rdi, 1;
        );
        self.shift_under(under, after);
    }

    ///
    /// gen code for shift-left of integer.
    ///
    /// ### in
    /// - rdi: lhs:Value
    /// - rsi: rhs:Value
    ///
    /// ### out
    /// - rdi: result:Value
    ///
    /// ### destroy
    /// - rax
    /// - rcx
    ///
    pub(crate) fn gen_shl(&mut self, deopt: DestLabel) {
        let shr = self.jit.label();
        let after = self.jit.label();
        let under = self.jit.label();
        monoasm!( &mut self.jit,
            movq rcx, rsi;
            sarq rcx, 1;
            js shr;
            lzcntq rax, rdi;
            cmpq rcx, rax;
            jgt deopt;
            subq rdi, 1;
            salq rdi, rcx;
        after:
            orq rdi, 1;
        );
        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        shr:
            negq rcx;
            cmpq rcx, 64;
            jge under;
            sarq rdi, rcx;
            jmp after;
        );
        self.jit.select_page(0);
        self.shift_under(under, after);
    }

    ///
    /// gen code for shift-left of integer (rhs is u8).
    ///
    /// ### in
    /// - rdi: lhs:Value
    ///
    /// ### out
    /// - rdi: result:Value
    ///
    /// ### destroy
    /// - rax
    /// - rcx
    ///
    pub(crate) fn gen_shl_imm(&mut self, imm: u8, deopt: DestLabel) {
        monoasm!( &mut self.jit,
            movl rcx, (imm);
            lzcntq rax, rdi;
            cmpq rax, rcx;
            jle deopt;
            subq rdi, 1;
            shlq rdi, rcx;
            orq rdi, 1;
        );
    }
}

impl BBContext {
    pub(super) fn gen_binop_integer(
        &mut self,
        ir: &mut AsmIr,
        pc: BcPc,
        kind: BinOpK,
        dst: Option<SlotId>,
        mode: OpMode,
    ) {
        match kind {
            BinOpK::Add => {
                match mode {
                    OpMode::RR(lhs, rhs) => {
                        self.fetch_fixnum_rr(ir, lhs, rhs, pc);
                    }
                    OpMode::RI(slot, _) | OpMode::IR(_, slot) => {
                        self.fetch_fixnum_rdi(ir, slot, pc);
                    }
                }
                self.release(dst);
                ir.integer_binop(self, pc, kind, mode);
                ir.reg2stack(GP::Rdi, dst);
            }
            BinOpK::Sub => {
                match mode {
                    OpMode::RR(lhs, rhs) => {
                        self.fetch_fixnum_rr(ir, lhs, rhs, pc);
                    }
                    OpMode::RI(lhs, _) => {
                        self.fetch_fixnum_rdi(ir, lhs, pc);
                    }
                    OpMode::IR(_, rhs) => {
                        self.fetch_fixnum_rsi(ir, rhs, pc);
                    }
                }
                self.release(dst);
                ir.integer_binop(self, pc, kind, mode);
                ir.reg2stack(GP::Rdi, dst);
            }
            BinOpK::Exp => {
                self.fetch_fixnum_mode(ir, &mode, pc);
                self.release(dst);
                ir.integer_binop(self, pc, kind, mode);
                ir.reg2stack(GP::Rax, dst);
            }
            BinOpK::Mul | BinOpK::Div => {
                self.fetch_fixnum_mode(ir, &mode, pc);
                self.release(dst);
                ir.integer_binop(self, pc, kind, mode);
                ir.reg2stack(GP::Rax, dst);
            }
            BinOpK::Rem => match mode {
                OpMode::RI(lhs, rhs) if rhs > 0 && (rhs as u64).is_power_of_two() => {
                    self.fetch_fixnum_rdi(ir, lhs, pc);
                    self.release(dst);
                    ir.integer_binop(self, pc, kind, mode);
                    ir.reg2stack(GP::Rdi, dst);
                }
                _ => {
                    self.fetch_fixnum_mode(ir, &mode, pc);
                    self.release(dst);
                    ir.integer_binop(self, pc, kind, mode);
                    ir.reg2stack(GP::Rax, dst);
                }
            },
            BinOpK::BitOr => {
                match mode {
                    OpMode::RR(lhs, rhs) => {
                        self.fetch_fixnum_rr(ir, lhs, rhs, pc);
                    }
                    OpMode::RI(slot, _) | OpMode::IR(_, slot) => {
                        self.fetch_fixnum_rdi(ir, slot, pc);
                    }
                }
                self.release(dst);
                ir.integer_binop(self, pc, kind, mode);
                ir.reg2stack(GP::Rdi, dst);
            }
            BinOpK::BitAnd => {
                match mode {
                    OpMode::RR(lhs, rhs) => {
                        self.fetch_fixnum_rr(ir, lhs, rhs, pc);
                    }
                    OpMode::RI(slot, _) | OpMode::IR(_, slot) => {
                        self.fetch_fixnum_rdi(ir, slot, pc);
                    }
                }
                self.release(dst);
                ir.integer_binop(self, pc, kind, mode);
                ir.reg2stack(GP::Rdi, dst);
            }
            BinOpK::BitXor => {
                match mode {
                    OpMode::RR(lhs, rhs) => {
                        self.fetch_fixnum_rr(ir, lhs, rhs, pc);
                    }
                    OpMode::RI(slot, _) | OpMode::IR(_, slot) => {
                        self.fetch_fixnum_rdi(ir, slot, pc);
                    }
                }
                self.release(dst);
                ir.integer_binop(self, pc, kind, mode);
                ir.reg2stack(GP::Rdi, dst);
            }
        }
    }

    fn fetch_fixnum_rr(&mut self, ir: &mut AsmIr, lhs: SlotId, rhs: SlotId, pc: BcPc) -> usize {
        let is_lhs_smi = self.is_i16_literal(lhs).is_some();
        let is_rhs_smi = self.is_i16_literal(rhs).is_some();
        self.fetch_to_reg(ir, lhs, GP::Rdi);
        self.fetch_to_reg(ir, rhs, GP::Rsi);
        let deopt = ir.new_deopt(pc, self.get_write_back());

        if !is_lhs_smi {
            ir.guard_reg_fixnum(GP::Rdi, deopt);
        }
        if !is_rhs_smi {
            ir.guard_reg_fixnum(GP::Rsi, deopt);
        }
        deopt
    }

    fn fetch_fixnum_rdi(&mut self, ir: &mut AsmIr, slot: SlotId, pc: BcPc) -> usize {
        let is_smi = self.is_i16_literal(slot).is_some();
        self.fetch_to_reg(ir, slot, GP::Rdi);
        let deopt = ir.new_deopt(pc, self.get_write_back());

        if !is_smi {
            ir.guard_reg_fixnum(GP::Rdi, deopt);
        }
        deopt
    }

    fn fetch_fixnum_rsi(&mut self, ir: &mut AsmIr, slot: SlotId, pc: BcPc) -> usize {
        let is_smi = self.is_i16_literal(slot).is_some();
        self.fetch_to_reg(ir, slot, GP::Rsi);
        let deopt = ir.new_deopt(pc, self.get_write_back());

        if !is_smi {
            ir.guard_reg_fixnum(GP::Rsi, deopt);
        }
        deopt
    }

    fn fetch_fixnum_mode(&mut self, ir: &mut AsmIr, mode: &OpMode, pc: BcPc) -> usize {
        match mode {
            OpMode::RR(lhs, rhs) => {
                let deopt = self.fetch_fixnum_rr(ir, *lhs, *rhs, pc);
                deopt
            }
            OpMode::RI(lhs, rhs) => {
                let deopt = self.fetch_fixnum_rdi(ir, *lhs, pc);
                ir.lit2reg(Value::i32(*rhs as i32), GP::Rsi);
                deopt
            }
            OpMode::IR(lhs, rhs) => {
                let deopt = self.fetch_fixnum_rsi(ir, *rhs, pc);
                ir.lit2reg(Value::i32(*lhs as i32), GP::Rdi);
                deopt
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn rem() {
        run_test("a = 3456; a % 64");
        run_test("a = 3456; a % 32");
        run_test("a = 3456; a % 16");
        run_test("a = 3456; a % 8");
        run_test("a = 3456; a % 4");
        run_test("a = 3456; a % 2");
    }
}
