use crate::bytecodegen::BinOpK;
use ruruby_parse::CmpKind;

use super::*;

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

            pub(in crate::compiler) fn [<set_ $op>](&mut self) {
                monoasm! { &mut self.jit,
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
    pub(super) fn cmp_float(&mut self, mode: &FMode) {
        match mode {
            FMode::RR(l, r) => {
                monoasm! { &mut self.jit,
                    ucomisd xmm(l.enc()), xmm(r.enc());
                };
            }
            FMode::RI(l, r) => {
                let r = self.jit.const_f64(*r as f64);
                monoasm! { &mut self.jit,
                    ucomisd xmm(l.enc()), [rip + r];
                };
            }
            _ => unreachable!(),
        }
    }

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

    pub(super) fn flag_to_bool(&mut self, kind: CmpKind) {
        match kind {
            CmpKind::Eq => self.set_eq(),
            CmpKind::Ne => self.set_ne(),
            CmpKind::Ge => self.set_ge(),
            CmpKind::Gt => self.set_gt(),
            CmpKind::Le => self.set_le(),
            CmpKind::Lt => self.set_lt(),
            CmpKind::TEq => self.set_eq(),
            CmpKind::Cmp => unreachable!(),
        }
    }

    pub(super) fn cond_br(&mut self, branch_dest: DestLabel, brkind: BrKind) {
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

    ///
    /// Compare two values with *mode*, and set flags.
    ///
    pub(super) fn cmp_integer(&mut self, mode: &OpMode) {
        match mode {
            OpMode::RR(..) => {
                monoasm!( &mut self.jit,
                    cmpq rdi, rsi;
                );
            }
            OpMode::RI(_, r) => {
                monoasm!( &mut self.jit,
                    cmpq rdi, (Value::i32(*r as i32).id());
                );
            }
            OpMode::IR(l, _) => {
                monoasm!( &mut self.jit,
                    movq rdi, (Value::i32(*l as i32).id());
                    cmpq rdi, rsi;
                );
            }
        }
    }
}

impl Codegen {
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

impl AsmIr {
    pub(in crate::compiler::jitgen) fn gen_binop_integer(
        &mut self,
        ctx: &mut BBContext,
        pc: BcPc,
        kind: BinOpK,
        dst: Option<SlotId>,
        mode: OpMode,
    ) {
        match kind {
            BinOpK::Add | BinOpK::BitOr | BinOpK::BitAnd | BinOpK::BitXor => {
                match mode {
                    OpMode::RR(lhs, rhs) => {
                        self.fetch_fixnum_rr(ctx, lhs, rhs, pc);
                    }
                    OpMode::RI(slot, _) | OpMode::IR(_, slot) => {
                        self.fetch_fixnum_rdi(ctx, slot, pc);
                    }
                }
                ctx.release(dst);
                self.integer_binop(ctx, pc, kind, mode);
                self.reg2stack(GP::Rdi, dst);
            }
            BinOpK::Sub => {
                self.fetch_fixnum_binary(ctx, pc, &mode);
                ctx.release(dst);
                self.integer_binop(ctx, pc, kind, mode);
                self.reg2stack(GP::Rdi, dst);
            }
            BinOpK::Exp | BinOpK::Mul | BinOpK::Div => {
                self.fetch_fixnum_mode(ctx, &mode, pc);
                ctx.release(dst);
                self.integer_binop(ctx, pc, kind, mode);
                self.reg2stack(GP::Rax, dst);
            }
            BinOpK::Rem => match mode {
                OpMode::RI(lhs, rhs) if rhs > 0 && (rhs as u64).is_power_of_two() => {
                    self.fetch_fixnum_rdi(ctx, lhs, pc);
                    ctx.release(dst);
                    self.integer_binop(ctx, pc, kind, mode);
                    self.reg2stack(GP::Rdi, dst);
                }
                _ => {
                    self.fetch_fixnum_mode(ctx, &mode, pc);
                    ctx.release(dst);
                    self.integer_binop(ctx, pc, kind, mode);
                    self.reg2stack(GP::Rax, dst);
                }
            },
        }
    }

    pub(in crate::compiler::jitgen) fn fetch_fixnum_binary(
        &mut self,
        ctx: &mut BBContext,
        pc: BcPc,
        mode: &OpMode,
    ) {
        match mode {
            OpMode::RR(l, r) => {
                self.fetch_fixnum_rr(ctx, *l, *r, pc);
            }
            OpMode::RI(l, _) => {
                self.fetch_fixnum_rdi(ctx, *l, pc);
            }
            OpMode::IR(_, r) => {
                self.fetch_fixnum_rsi(ctx, *r, pc);
            }
        }
    }

    fn fetch_fixnum_rr(
        &mut self,
        ctx: &mut BBContext,
        lhs: SlotId,
        rhs: SlotId,
        pc: BcPc,
    ) -> AsmDeopt {
        let is_lhs_smi = ctx.is_i16_literal(lhs).is_some();
        let is_rhs_smi = ctx.is_i16_literal(rhs).is_some();
        self.fetch_to_reg(ctx, lhs, GP::Rdi);
        self.fetch_to_reg(ctx, rhs, GP::Rsi);
        let deopt = self.new_deopt(pc, ctx.get_write_back());

        if !is_lhs_smi {
            self.guard_fixnum(GP::Rdi, deopt);
        }
        if !is_rhs_smi {
            self.guard_fixnum(GP::Rsi, deopt);
        }
        deopt
    }

    fn fetch_fixnum_rdi(&mut self, ctx: &mut BBContext, slot: SlotId, pc: BcPc) -> AsmDeopt {
        self.fetch_fixnum(ctx, slot, GP::Rdi, pc)
    }

    fn fetch_fixnum_rsi(&mut self, ctx: &mut BBContext, slot: SlotId, pc: BcPc) -> AsmDeopt {
        self.fetch_fixnum(ctx, slot, GP::Rsi, pc)
    }

    fn fetch_fixnum(&mut self, ctx: &mut BBContext, slot: SlotId, reg: GP, pc: BcPc) -> AsmDeopt {
        let is_smi = ctx.is_i16_literal(slot).is_some();
        self.fetch_to_reg(ctx, slot, reg);
        let deopt = self.new_deopt(pc, ctx.get_write_back());

        if !is_smi {
            self.guard_fixnum(reg, deopt);
        }
        deopt
    }

    fn fetch_fixnum_mode(&mut self, ctx: &mut BBContext, mode: &OpMode, pc: BcPc) -> AsmDeopt {
        match mode {
            OpMode::RR(lhs, rhs) => {
                let deopt = self.fetch_fixnum_rr(ctx, *lhs, *rhs, pc);
                deopt
            }
            OpMode::RI(lhs, rhs) => {
                let deopt = self.fetch_fixnum_rdi(ctx, *lhs, pc);
                self.lit2reg(Value::i32(*rhs as i32), GP::Rsi);
                deopt
            }
            OpMode::IR(lhs, rhs) => {
                let deopt = self.fetch_fixnum_rsi(ctx, *rhs, pc);
                self.lit2reg(Value::i32(*lhs as i32), GP::Rdi);
                deopt
            }
        }
    }
}

impl Codegen {
    pub(super) fn integer_binop(
        &mut self,
        mode: &OpMode,
        kind: BinOpK,
        deopt: DestLabel,
        error: DestLabel,
        using_xmm: UsingXmm,
    ) {
        match kind {
            BinOpK::Add => match mode {
                OpMode::RR(_, _) => {
                    monoasm!( &mut self.jit,
                        subq rdi, 1;
                        addq rdi, rsi;
                        jo deopt;
                    );
                }
                OpMode::RI(_, i) | OpMode::IR(i, _) => {
                    monoasm!( &mut self.jit,
                        addq rdi, (Value::i32(*i as i32).id() - 1);
                        jo deopt;
                    );
                }
            },
            BinOpK::Sub => match mode {
                OpMode::RR(_, _) => {
                    monoasm!( &mut self.jit,
                        subq rdi, rsi;
                        jo deopt;
                        addq rdi, 1;
                    );
                }
                OpMode::RI(_, rhs) => {
                    monoasm!( &mut self.jit,
                        subq rdi, (Value::i32(*rhs as i32).id() - 1);
                        jo deopt;
                    );
                }
                OpMode::IR(lhs, _) => {
                    monoasm!( &mut self.jit,
                        movq rdi, (Value::i32(*lhs as i32).id());
                        subq rdi, rsi;
                        jo deopt;
                        addq rdi, 1;
                    );
                }
            },
            BinOpK::Exp => {
                self.xmm_save(using_xmm);
                monoasm!( &mut self.jit,
                    sarq rdi, 1;
                    sarq rsi, 1;
                    movq rax, (pow_ii as u64);
                    call rax;
                );
                self.xmm_restore(using_xmm);
            }
            BinOpK::Mul | BinOpK::Div => {
                self.generic_binop(kind, using_xmm, error);
            }
            BinOpK::Rem => match mode {
                OpMode::RI(_, rhs) if *rhs > 0 && (*rhs as u64).is_power_of_two() => {
                    monoasm!( &mut self.jit,
                        andq rdi, (*rhs * 2 - 1);
                    );
                }
                _ => {
                    self.generic_binop(kind, using_xmm, error);
                }
            },
            BinOpK::BitOr => match mode {
                OpMode::RR(_, _) => {
                    monoasm!( &mut self.jit,
                        orq rdi, rsi;
                    );
                }
                OpMode::RI(_, i) | OpMode::IR(i, _) => {
                    monoasm!( &mut self.jit,
                        orq rdi, (Value::i32(*i as i32).id());
                    );
                }
            },
            BinOpK::BitAnd => match mode {
                OpMode::RR(_, _) => {
                    monoasm!( &mut self.jit,
                        andq rdi, rsi;
                    );
                }
                OpMode::RI(_, i) | OpMode::IR(i, _) => {
                    monoasm!( &mut self.jit,
                        andq rdi, (Value::i32(*i as i32).id());
                    );
                }
            },
            BinOpK::BitXor => match mode {
                OpMode::RR(_, _) => {
                    monoasm!( &mut self.jit,
                        xorq rdi, rsi;
                        addq rdi, 1;
                    );
                }
                OpMode::RI(_, i) | OpMode::IR(i, _) => {
                    monoasm!( &mut self.jit,
                        xorq rdi, (Value::i32(*i as i32).id() - 1);
                    );
                }
            },
        }
    }

    pub(super) fn generic_binop(&mut self, kind: BinOpK, using_xmm: UsingXmm, error: DestLabel) {
        let func = kind.generic_func();
        self.xmm_save(using_xmm);
        self.call_binop(func);
        self.xmm_restore(using_xmm);
        self.handle_error(error);
    }
}

impl Codegen {
    pub(super) fn float_binop(&mut self, kind: BinOpK, using_xmm: UsingXmm, dst: Xmm, mode: FMode) {
        match mode {
            FMode::RR(l, r) => self.binop_float_rr(kind, using_xmm, dst, l, r),
            FMode::RI(l, r) => self.binop_float_ri(kind, using_xmm, dst, l, r),
            FMode::IR(l, r) => self.binop_float_ir(kind, using_xmm, dst, l, r),
        }
    }

    fn binop_float_rr(&mut self, kind: BinOpK, using_xmm: UsingXmm, dst: Xmm, l: Xmm, r: Xmm) {
        let lhs = l.enc();
        let rhs = r.enc();
        let ret = dst.enc();
        match kind {
            BinOpK::Add => {
                if ret == rhs {
                    monoasm!( &mut self.jit,
                        addsd xmm(ret), xmm(lhs);
                    );
                } else {
                    self.xmm_mov(l, dst);
                    monoasm!( &mut self.jit,
                        addsd xmm(ret), xmm(rhs);
                    );
                }
            }
            BinOpK::Sub => {
                if ret == rhs {
                    monoasm!( &mut self.jit,
                        movq  xmm0, xmm(lhs);
                        subsd xmm0, xmm(ret);
                        movq  xmm(ret), xmm0;
                    );
                } else {
                    self.xmm_mov(l, dst);
                    monoasm!( &mut self.jit,
                        subsd xmm(ret), xmm(rhs);
                    );
                }
            }
            BinOpK::Mul => {
                if ret == rhs {
                    monoasm!( &mut self.jit,
                        mulsd xmm(ret), xmm(lhs);
                    );
                } else {
                    self.xmm_mov(l, dst);
                    monoasm!( &mut self.jit,
                        mulsd xmm(ret), xmm(rhs);
                    );
                }
            }
            BinOpK::Div => {
                if ret == rhs {
                    monoasm!( &mut self.jit,
                        movq  xmm0, xmm(lhs);
                        divsd xmm0, xmm(ret);
                        movq  xmm(ret), xmm0;
                    );
                } else {
                    self.xmm_mov(l, dst);
                    monoasm!( &mut self.jit,
                        divsd xmm(ret), xmm(rhs);
                    );
                }
            }
            BinOpK::Exp => {
                self.xmm_save(using_xmm);
                monoasm!( &mut self.jit,
                    movq xmm0, xmm(lhs);
                    movq xmm1, xmm(rhs);
                    movq rax, (pow_ff_f as u64);
                    call rax;
                );
                self.xmm_restore(using_xmm);
                monoasm!( &mut self.jit,
                    movq xmm(ret), xmm0;
                );
            }
            _ => unimplemented!(),
        }
    }

    fn binop_float_ri(&mut self, kind: BinOpK, using_xmm: UsingXmm, dst: Xmm, l: Xmm, r: i16) {
        let rhs_label = self.jit.const_f64(r as f64);
        let ret = dst.enc();
        match kind {
            BinOpK::Add => {
                self.xmm_mov(l, dst);
                monoasm!( &mut self.jit,
                    addsd xmm(ret), [rip + rhs_label];
                );
            }
            BinOpK::Sub => {
                self.xmm_mov(l, dst);
                monoasm!( &mut self.jit,
                    subsd xmm(ret), [rip + rhs_label];
                );
            }
            BinOpK::Mul => {
                self.xmm_mov(l, dst);
                monoasm!( &mut self.jit,
                    mulsd xmm(ret), [rip + rhs_label];
                );
            }
            BinOpK::Div => {
                self.xmm_mov(l, dst);
                monoasm!( &mut self.jit,
                    divsd xmm(ret), [rip + rhs_label];
                )
            }
            BinOpK::Exp => {
                let lhs = l.enc();
                self.xmm_save(using_xmm);
                monoasm!( &mut self.jit,
                    movq xmm0, xmm(lhs);
                    movq xmm1, [rip + rhs_label];
                    movq rax, (pow_ff_f as u64);
                    call rax;
                );
                self.xmm_restore(using_xmm);
                monoasm!( &mut self.jit,
                    movq xmm(ret), xmm0;
                );
            }
            _ => unimplemented!(),
        }
    }

    fn binop_float_ir(&mut self, kind: BinOpK, using_xmm: UsingXmm, dst: Xmm, l: i16, r: Xmm) {
        let lhs = self.jit.const_f64(l as f64);
        let rhs = r.enc();
        let ret = dst.enc();
        match kind {
            BinOpK::Add => {
                if ret != rhs {
                    monoasm!( &mut self.jit,
                        movq  xmm(ret), [rip + lhs];
                        addsd xmm(ret), xmm(rhs);
                    );
                } else {
                    monoasm!( &mut self.jit,
                        addsd xmm(ret), [rip + lhs];
                    );
                }
            }
            BinOpK::Sub => {
                if ret != rhs {
                    monoasm!( &mut self.jit,
                        movq  xmm(ret), [rip + lhs];
                        subsd xmm(ret), xmm(rhs);
                    );
                } else {
                    monoasm!( &mut self.jit,
                        movq  xmm0, xmm(rhs);
                        movq  xmm(ret), [rip + lhs];
                        subsd xmm(ret), xmm0;
                    );
                }
            }
            BinOpK::Mul => {
                if ret != rhs {
                    monoasm!( &mut self.jit,
                        movq  xmm(ret), [rip + lhs];
                        mulsd xmm(ret), xmm(rhs);
                    );
                } else {
                    monoasm!( &mut self.jit,
                        mulsd xmm(ret), [rip + lhs];
                    );
                }
            }
            BinOpK::Div => {
                if ret != rhs {
                    monoasm!( &mut self.jit,
                        movq  xmm(ret), [rip + lhs];
                        divsd xmm(ret), xmm(rhs);
                    );
                } else {
                    monoasm!( &mut self.jit,
                        movq  xmm0, xmm(ret);
                        movq  xmm(ret), [rip + lhs];
                        divsd xmm(ret), xmm0;
                    );
                }
            }
            BinOpK::Exp => {
                self.xmm_save(using_xmm);
                monoasm!( &mut self.jit,
                    movq xmm0, [rip + lhs];
                    movq xmm1, xmm(rhs);
                    movq rax, (pow_ff_f as u64);
                    call rax;
                );
                self.xmm_restore(using_xmm);
                monoasm!( &mut self.jit,
                    movq xmm(ret), xmm0;
                );
            }
            _ => unimplemented!(),
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
