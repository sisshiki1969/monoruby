use super::*;
use ruruby_parse::CmpKind;

impl Codegen {
    pub(super) fn gen_binop_integer(
        &mut self,
        pc: BcPc,
        kind: BinOpK,
        ret: SlotId,
        mode: OpMode,
        ctx: &BBContext,
    ) {
        let deopt = self.gen_side_deopt(pc, ctx);
        match kind {
            BinOpK::Add => {
                match mode {
                    OpMode::RR(lhs, rhs) => {
                        self.load_guard_binary_fixnum(lhs, rhs, deopt);
                        monoasm!( &mut self.jit,
                            // fastpath
                            subq rdi, 1;
                            addq rdi, rsi;
                            jo deopt;
                        );
                        self.store_rdi(ret);
                    }
                    OpMode::RI(lhs, rhs) => {
                        self.load_guard_rdi_fixnum(lhs, deopt);
                        monoasm!( &mut self.jit,
                            // fastpath
                            addq rdi, (Value::i32(rhs as i32).id() - 1);
                            jo deopt;
                        );
                        self.store_rdi(ret);
                    }
                    OpMode::IR(lhs, rhs) => {
                        self.load_guard_rsi_fixnum(rhs, deopt);
                        monoasm!( &mut self.jit,
                            // fastpath
                            addq rsi, (Value::i32(lhs as i32).id() - 1);
                            jo deopt;
                        );
                        self.store_rsi(ret);
                    }
                }
            }
            BinOpK::Sub => {
                match mode {
                    OpMode::RR(lhs, rhs) => {
                        self.load_guard_binary_fixnum(lhs, rhs, deopt);
                        monoasm!( &mut self.jit,
                            // fastpath
                            subq rdi, rsi;
                            jo deopt;
                            addq rdi, 1;
                        );
                        self.store_rdi(ret);
                    }
                    OpMode::RI(lhs, rhs) => {
                        self.load_guard_rdi_fixnum(lhs, deopt);
                        monoasm!( &mut self.jit,
                            // fastpath
                            subq rdi, (Value::i32(rhs as i32).id() - 1);
                            jo deopt;
                        );
                        self.store_rdi(ret);
                    }
                    OpMode::IR(lhs, rhs) => {
                        self.load_guard_rsi_fixnum(rhs, deopt);
                        monoasm!( &mut self.jit,
                            // fastpath
                            movq rdi, (Value::i32(lhs as i32).id());
                            subq rdi, rsi;
                            jo deopt;
                            addq rdi, 1;
                        );
                        self.store_rdi(ret);
                    }
                }
            }
            BinOpK::Exp => {
                let xmm_using = ctx.get_xmm_using();
                self.xmm_save(&xmm_using);
                self.load_and_guard_binary_fixnum_with_mode(deopt, &mode);
                monoasm!( &mut self.jit,
                    sarq rdi, 1;
                    sarq rsi, 1;
                    movq rax, (pow_ii as u64);
                    call rax;
                );
                self.xmm_restore(&xmm_using);
                self.store_rax(ret);
            }
            BinOpK::Mul | BinOpK::Div | BinOpK::Rem => {
                self.load_binary_args_with_mode(&mode);
                self.generic_binop(ctx, ret, kind, pc);
            }
            _ => {
                match kind {
                    BinOpK::BitOr => {
                        self.load_and_guard_binary_fixnum_with_mode(deopt, &mode);
                        monoasm!( &mut self.jit,
                            orq rdi, rsi;
                        );
                    }
                    BinOpK::BitAnd => {
                        self.load_and_guard_binary_fixnum_with_mode(deopt, &mode);
                        monoasm!( &mut self.jit,
                            andq rdi, rsi;
                        );
                    }
                    BinOpK::BitXor => {
                        self.load_and_guard_binary_fixnum_with_mode(deopt, &mode);
                        monoasm!( &mut self.jit,
                            xorq rdi, rsi;
                            addq rdi, 1;
                        );
                    }
                    BinOpK::Shr => {
                        self.load_and_guard_binary_fixnum_with_mode(deopt, &mode);
                        self.gen_shr(deopt);
                    }
                    BinOpK::Shl => match mode {
                        OpMode::RI(lhs, imm) if imm >= 0 && i8::try_from(imm).is_ok() => {
                            let imm = i8::try_from(imm).unwrap() as u8;
                            self.load_guard_rdi_fixnum(lhs, deopt);
                            self.gen_shl_imm(imm, deopt);
                        }
                        _ => {
                            self.load_and_guard_binary_fixnum_with_mode(deopt, &mode);
                            self.gen_shl(deopt);
                        }
                    },
                    _ => unimplemented!(),
                }
                self.store_rdi(ret);
            }
        }
    }

    pub(super) fn gen_binop_float_rr(
        &mut self,
        kind: BinOpK,
        ctx: &BBContext,
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
                let xmm_using = ctx.get_xmm_using();
                self.xmm_save(&xmm_using);
                monoasm!( &mut self.jit,
                    movq xmm0, xmm(lhs);
                    movq xmm1, xmm(rhs);
                    movq rax, (pow_ff_f as u64);
                    call rax;
                );
                self.xmm_restore(&xmm_using);
                monoasm!( &mut self.jit,
                    movq xmm(ret), xmm0;
                );
            }
            _ => unimplemented!(),
        }
    }

    pub(super) fn gen_binop_float_ri(
        &mut self,
        kind: BinOpK,
        ctx: &BBContext,
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
                let xmm_using = ctx.get_xmm_using();
                self.xmm_save(&xmm_using);
                monoasm!( &mut self.jit,
                    movq xmm0, xmm(lhs);
                    movq xmm1, [rip + rhs_label];
                    movq rax, (pow_ff_f as u64);
                    call rax;
                );
                self.xmm_restore(&xmm_using);
                monoasm!( &mut self.jit,
                    movq xmm(ret), xmm0;
                );
            }
            _ => unimplemented!(),
        }
    }

    pub(super) fn gen_binop_float_ir(
        &mut self,
        kind: BinOpK,
        ctx: &BBContext,
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
                let xmm_using = ctx.get_xmm_using();
                self.xmm_save(&xmm_using);
                monoasm!( &mut self.jit,
                    movq xmm0, [rip + lhs];
                    movq xmm1, xmm(rhs);
                    movq rax, (pow_ff_f as u64);
                    call rax;
                );
                self.xmm_restore(&xmm_using);
                monoasm!( &mut self.jit,
                    movq xmm(ret), xmm0;
                );
            }
            _ => unimplemented!(),
        }
    }

    pub(super) fn gen_generic_binop(
        &mut self,
        ctx: &BBContext,
        pc: BcPc,
        kind: BinOpK,
        ret: SlotId,
    ) {
        self.generic_binop(ctx, ret, kind, pc);
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
        mode: OpMode,
        kind: CmpKind,
        ret: SlotId,
        pc: BcPc,
        index: BcIndex,
    ) {
        match (pc + 1).get_ir() {
            TraceIr::CondBr(_, disp, true, brkind) => {
                let dest_idx = index + disp + 1;
                let branch_dest = self.jit.label();
                if mode.is_float_op(&pc) {
                    match mode {
                        OpMode::RR(lhs, rhs) => {
                            let (flhs, frhs) = self.fetch_float_binary(ctx, lhs, rhs, pc);
                            ctx.dealloc_xmm(ret);
                            monoasm! { &mut self.jit,
                                ucomisd xmm(flhs.enc()), xmm(frhs.enc());
                            };
                        }
                        OpMode::RI(lhs, rhs) => {
                            let rhs_label = self.jit.const_f64(rhs as f64);
                            let flhs = self.fetch_float_assume_float(ctx, lhs, pc);
                            ctx.dealloc_xmm(ret);
                            monoasm! { &mut self.jit,
                                ucomisd xmm(flhs.enc()), [rip + rhs_label];
                            };
                        }
                        _ => unreachable!(),
                    }
                    self.condbr_float(kind, branch_dest, brkind);
                } else {
                    self.writeback_binary(ctx, &mode);
                    ctx.dealloc_xmm(ret);
                    if mode.is_integer_op(&pc) {
                        let deopt = self.gen_side_deopt(pc, ctx);
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
                cc.new_branch(index, dest_idx, ctx.clone(), branch_dest);
            }
            _ => unreachable!(),
        }
    }

    pub(super) fn generic_cmp(&mut self, kind: CmpKind, ctx: &BBContext) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
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
        self.xmm_restore(&xmm_using);
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
    pub(super) fn writeback_binary(&mut self, ctx: &mut BBContext, mode: &OpMode) {
        match mode {
            OpMode::RR(lhs, rhs) => {
                self.fetch_slots(ctx, &[*lhs, *rhs]);
            }
            OpMode::RI(r, _) | OpMode::IR(_, r) => {
                self.fetch_slot(ctx, *r);
            }
        }
    }

    fn load_guard_rdi_fixnum(&mut self, reg: SlotId, deopt: DestLabel) {
        self.load_rdi(reg);
        self.guard_rdi_fixnum(deopt);
    }

    fn load_guard_rsi_fixnum(&mut self, reg: SlotId, deopt: DestLabel) {
        self.load_rsi(reg);
        self.guard_rsi_fixnum(deopt);
    }

    fn load_guard_binary_fixnum(&mut self, lhs: SlotId, rhs: SlotId, deopt: DestLabel) {
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

    pub(super) fn load_and_guard_binary_fixnum_with_mode(
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

    fn gen_shr(&mut self, deopt: DestLabel) {
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

    fn gen_shl(&mut self, deopt: DestLabel) {
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
    /// lhs << imm(>=0)
    ///
    fn gen_shl_imm(&mut self, imm: u8, deopt: DestLabel) {
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

    fn generic_binop(&mut self, ctx: &BBContext, ret: SlotId, kind: BinOpK, pc: BcPc) {
        let func = kind.generic_func();
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        self.call_binop(func);
        self.xmm_restore(&xmm_using);
        self.jit_handle_error(ctx, pc);
        self.store_rax(ret);
    }
}
