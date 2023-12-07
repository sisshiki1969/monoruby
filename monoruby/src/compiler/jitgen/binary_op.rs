use crate::bytecodegen::BinOpK;
use ruruby_parse::CmpKind;

use super::*;

impl Codegen {
    pub(super) fn gen_float_binop(
        &mut self,
        kind: BinOpK,
        using_xmm: UsingXmm,
        fret: Xmm,
        mode: FMode,
    ) {
        match mode {
            FMode::RR(l, r) => self.binop_float_rr(kind, using_xmm, fret, l, r),
            FMode::RI(l, r) => self.binop_float_ri(kind, using_xmm, fret, l, r),
            FMode::IR(l, r) => self.binop_float_ir(kind, using_xmm, fret, l, r),
        }
    }

    fn binop_float_rr(
        &mut self,
        kind: BinOpK,
        using_xmm: UsingXmm,
        fret: Xmm,
        flhs: Xmm,
        frhs: Xmm,
    ) {
        let lhs = flhs.enc();
        let rhs = frhs.enc();
        let ret = fret.enc();
        match kind {
            BinOpK::Add => {
                if ret == rhs {
                    monoasm!( &mut self.jit,
                        addsd xmm(ret), xmm(lhs);
                    );
                } else {
                    self.xmm_mov(flhs, fret);
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
                    self.xmm_mov(flhs, fret);
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
                    self.xmm_mov(flhs, fret);
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
                    self.xmm_mov(flhs, fret);
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

    fn binop_float_ri(
        &mut self,
        kind: BinOpK,
        using_xmm: UsingXmm,
        fret: Xmm,
        flhs: Xmm,
        rhs: i16,
    ) {
        let rhs_label = self.jit.const_f64(rhs as f64);
        let ret = fret.enc();
        match kind {
            BinOpK::Add => {
                self.xmm_mov(flhs, fret);
                monoasm!( &mut self.jit,
                    addsd xmm(ret), [rip + rhs_label];
                );
            }
            BinOpK::Sub => {
                self.xmm_mov(flhs, fret);
                monoasm!( &mut self.jit,
                    subsd xmm(ret), [rip + rhs_label];
                );
            }
            BinOpK::Mul => {
                self.xmm_mov(flhs, fret);
                monoasm!( &mut self.jit,
                    mulsd xmm(ret), [rip + rhs_label];
                );
            }
            BinOpK::Div => {
                self.xmm_mov(flhs, fret);
                monoasm!( &mut self.jit,
                    divsd xmm(ret), [rip + rhs_label];
                )
            }
            BinOpK::Exp => {
                let lhs = flhs.enc();
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

    fn binop_float_ir(
        &mut self,
        kind: BinOpK,
        using_xmm: UsingXmm,
        fret: Xmm,
        lhs: i16,
        frhs: Xmm,
    ) {
        let lhs = self.jit.const_f64(lhs as f64);
        let rhs = frhs.enc();
        let ret = fret.enc();
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

    pub(super) fn gen_cmp_opt(
        &mut self,
        ctx: &mut BBContext,
        cc: &mut JitContext,
        func: &ISeqInfo,
        mode: OpMode,
        kind: CmpKind,
        ret: Option<SlotId>,
        pc: BcPc,
        index: BcIndex,
    ) {
        match (pc + 1).trace_ir() {
            TraceIr::CondBr(_, disp, true, brkind) => {
                let dest_idx = index + disp + 1;
                let branch_dest = self.jit.label();
                if mode.is_float_op(&pc) {
                    match mode {
                        OpMode::RR(lhs, rhs) => {
                            let (flhs, frhs) = self.fetch_float_binary(ctx, lhs, rhs, pc);
                            ctx.release(ret);
                            monoasm! { &mut self.jit,
                                ucomisd xmm(flhs.enc()), xmm(frhs.enc());
                            };
                        }
                        OpMode::RI(lhs, rhs) => {
                            let rhs_label = self.jit.const_f64(rhs as f64);
                            let flhs = self.fetch_float_assume_float(ctx, lhs, pc);
                            ctx.release(ret);
                            monoasm! { &mut self.jit,
                                ucomisd xmm(flhs.enc()), [rip + rhs_label];
                            };
                        }
                        _ => unreachable!(),
                    }
                    self.condbr_float(kind, branch_dest, brkind);
                } else {
                    self.fetch_binary(ctx, &mode);
                    ctx.release(ret);
                    if mode.is_integer_op(&pc) {
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
                        self.condbr_int(kind, branch_dest, brkind);
                    } else {
                        self.load_binary_args_with_mode(&mode);
                        self.cmp_opt_generic(ctx, kind, branch_dest, brkind, pc);
                    }
                }
                cc.new_branch(func, index, dest_idx, ctx.clone(), branch_dest);
            }
            _ => unreachable!(),
        }
    }

    pub(super) fn generic_cmp(&mut self, kind: CmpKind, ctx: &BBContext) {
        let using_xmm = ctx.get_using_xmm();
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
        self.generic_cmp(kind, ctx);
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

    pub(crate) fn load_and_guard_binary_fixnum_with_mode(
        &mut self,
        deopt: DestLabel,
        mode: &OpMode,
    ) {
        match *mode {
            OpMode::RR(lhs, rhs) => {
                self.load_guard_binary_fixnum(lhs, rhs, deopt);
            }
            OpMode::RI(lhs, rhs) => {
                self.load_guard_rdi_fixnum(lhs, deopt);
                monoasm!( &mut self.jit,
                    movq rsi, (Value::i32(rhs as i32).id());
                );
            }
            OpMode::IR(lhs, rhs) => {
                self.load_guard_rsi_fixnum(rhs, deopt);
                monoasm!( &mut self.jit,
                    movq rdi, (Value::i32(lhs as i32).id());
                );
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

    pub(super) fn generic_binop(
        &mut self,
        ctx: &mut BBContext,
        dst: Option<SlotId>,
        kind: BinOpK,
        pc: BcPc,
    ) {
        let func = kind.generic_func();
        let using_xmm = ctx.get_using_xmm();
        self.xmm_save(using_xmm);
        self.call_binop(func);
        self.xmm_restore(using_xmm);
        self.jit_handle_error(ctx, pc);
        self.save_rax_to_acc(ctx, dst);
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
