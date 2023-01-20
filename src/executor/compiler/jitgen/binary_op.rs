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
                        monoasm!(self.jit,
                            // fastpath
                            subq rdi, 1;
                            addq rdi, rsi;
                            jo deopt;
                        );
                        self.store_rdi(ret);
                    }
                    OpMode::RI(lhs, rhs) => {
                        self.load_guard_rdi_fixnum(lhs, deopt);
                        monoasm!(self.jit,
                            // fastpath
                            addq rdi, (Value::int32(rhs as i32).get() - 1);
                            jo deopt;
                        );
                        self.store_rdi(ret);
                    }
                    OpMode::IR(lhs, rhs) => {
                        self.load_guard_rsi_fixnum(rhs, deopt);
                        monoasm!(self.jit,
                            // fastpath
                            addq rsi, (Value::int32(lhs as i32).get() - 1);
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
                        monoasm!(self.jit,
                            // fastpath
                            subq rdi, rsi;
                            jo deopt;
                            addq rdi, 1;
                        );
                        self.store_rdi(ret);
                    }
                    OpMode::RI(lhs, rhs) => {
                        self.load_guard_rdi_fixnum(lhs, deopt);
                        monoasm!(self.jit,
                            // fastpath
                            subq rdi, (Value::int32(rhs as i32).get() - 1);
                            jo deopt;
                        );
                        self.store_rdi(ret);
                    }
                    OpMode::IR(lhs, rhs) => {
                        self.load_guard_rsi_fixnum(rhs, deopt);
                        monoasm!(self.jit,
                            // fastpath
                            movq rdi, (Value::int32(lhs as i32).get());
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
                monoasm!(self.jit,
                    sarq rdi, 1;
                    sarq rsi, 1;
                    movq rax, (pow_ii as u64);
                    call rax;
                );
                self.xmm_restore(&xmm_using);
                self.store_rax(ret);
            }
            BinOpK::Mul | BinOpK::Div | BinOpK::Rem => {
                let xmm_using = ctx.get_xmm_using();
                self.load_binary_args_with_mode(&mode);
                self.generic_binop(ret, kind.generic_func() as _, xmm_using, pc);
            }
            _ => {
                self.load_and_guard_binary_fixnum_with_mode(deopt, &mode);
                match kind {
                    BinOpK::BitOr => {
                        monoasm!(self.jit,
                            orq rdi, rsi;
                        );
                    }
                    BinOpK::BitAnd => {
                        monoasm!(self.jit,
                            andq rdi, rsi;
                        );
                    }
                    BinOpK::BitXor => {
                        monoasm!(self.jit,
                            xorq rdi, rsi;
                            addq rdi, 1;
                        );
                    }
                    BinOpK::Shr => self.gen_shr(deopt),
                    BinOpK::Shl => self.gen_shl(deopt),
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
                    monoasm!(self.jit,
                        addsd xmm(ret), xmm(lhs);
                    );
                } else {
                    self.xmm_mov(flhs, fret);
                    monoasm!(self.jit,
                        addsd xmm(ret), xmm(rhs);
                    );
                }
            }
            BinOpK::Sub => {
                if ret == rhs {
                    monoasm!(self.jit,
                        movq  xmm0, xmm(lhs);
                        subsd xmm0, xmm(ret);
                        movq  xmm(ret), xmm0;
                    );
                } else {
                    self.xmm_mov(flhs, fret);
                    monoasm!(self.jit,
                        subsd xmm(ret), xmm(rhs);
                    );
                }
            }
            BinOpK::Mul => {
                if ret == rhs {
                    monoasm!(self.jit,
                        mulsd xmm(ret), xmm(lhs);
                    );
                } else {
                    self.xmm_mov(flhs, fret);
                    monoasm!(self.jit,
                        mulsd xmm(ret), xmm(rhs);
                    );
                }
            }
            BinOpK::Div => {
                let div_by_zero = self.div_by_zero;
                if ret == rhs {
                    monoasm!(self.jit,
                        movq  rax, xmm(ret);
                        testq  rax, rax;
                        jeq   div_by_zero;
                        movq  xmm0, xmm(lhs);
                        divsd xmm0, xmm(ret);
                        movq  xmm(ret), xmm0;
                    );
                } else {
                    self.xmm_mov(flhs, fret);
                    monoasm!(self.jit,
                        movq  rax, xmm(rhs);
                        testq rax, rax;
                        jz    div_by_zero;
                        divsd xmm(ret), xmm(rhs);
                    );
                }
            }
            BinOpK::Exp => {
                let xmm_using = ctx.get_xmm_using();
                self.xmm_save(&xmm_using);
                monoasm!(self.jit,
                    movq xmm0, xmm(lhs);
                    movq xmm1, xmm(rhs);
                    movq rax, (pow_ff_f as u64);
                    call rax;
                );
                self.xmm_restore(&xmm_using);
                monoasm!(self.jit,
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
                monoasm!(self.jit,
                    addsd xmm(ret), [rip + rhs_label];
                );
            }
            BinOpK::Sub => {
                self.xmm_mov(flhs, fret);
                monoasm!(self.jit,
                    subsd xmm(ret), [rip + rhs_label];
                );
            }
            BinOpK::Mul => {
                self.xmm_mov(flhs, fret);
                monoasm!(self.jit,
                    mulsd xmm(ret), [rip + rhs_label];
                );
            }
            BinOpK::Div => {
                if rhs == 0 {
                    let div_by_zero = self.div_by_zero;
                    monoasm!(self.jit,
                        jmp   div_by_zero;
                    )
                } else {
                    self.xmm_mov(flhs, fret);
                    monoasm!(self.jit,
                        divsd xmm(ret), [rip + rhs_label];
                    )
                }
            }
            BinOpK::Exp => {
                let lhs = flhs.enc();
                let xmm_using = ctx.get_xmm_using();
                self.xmm_save(&xmm_using);
                monoasm!(self.jit,
                    movq xmm0, xmm(lhs);
                    movq xmm1, [rip + rhs_label];
                    movq rax, (pow_ff_f as u64);
                    call rax;
                );
                self.xmm_restore(&xmm_using);
                monoasm!(self.jit,
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
                    monoasm!(self.jit,
                        movq  xmm(ret), [rip + lhs];
                        addsd xmm(ret), xmm(rhs);
                    );
                } else {
                    monoasm!(self.jit,
                        addsd xmm(ret), [rip + lhs];
                    );
                }
            }
            BinOpK::Sub => {
                if ret != rhs {
                    monoasm!(self.jit,
                        movq  xmm(ret), [rip + lhs];
                        subsd xmm(ret), xmm(rhs);
                    );
                } else {
                    monoasm!(self.jit,
                        movq  xmm0, xmm(rhs);
                        movq  xmm(ret), [rip + lhs];
                        subsd xmm(ret), xmm0;
                    );
                }
            }
            BinOpK::Mul => {
                if ret != rhs {
                    monoasm!(self.jit,
                        movq  xmm(ret), [rip + lhs];
                        mulsd xmm(ret), xmm(rhs);
                    );
                } else {
                    monoasm!(self.jit,
                        mulsd xmm(ret), [rip + lhs];
                    );
                }
            }
            BinOpK::Div => {
                let div_by_zero = self.div_by_zero;
                if ret != rhs {
                    monoasm!(self.jit,
                        movq  xmm(ret), [rip + lhs];
                        movq  rax, xmm(rhs);
                        testq rax, rax;
                        jeq   div_by_zero;
                        divsd xmm(ret), xmm(rhs);
                    );
                } else {
                    monoasm!(self.jit,
                        movq  rax, xmm(ret);
                        testq rax, rax;
                        jeq   div_by_zero;
                        movq  xmm(ret), [rip + lhs];
                        movq  xmm0, rax;
                        divsd xmm(ret), xmm0;
                    );
                }
            }
            BinOpK::Exp => {
                let xmm_using = ctx.get_xmm_using();
                self.xmm_save(&xmm_using);
                monoasm!(self.jit,
                    movq xmm0, [rip + lhs];
                    movq xmm1, xmm(rhs);
                    movq rax, (pow_ff_f as u64);
                    call rax;
                );
                self.xmm_restore(&xmm_using);
                monoasm!(self.jit,
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
        let xmm_using = ctx.get_xmm_using();
        self.generic_binop(ret, kind.generic_func() as _, xmm_using, pc);
    }

    pub(super) fn setflag_float(&mut self, kind: CmpKind) {
        match kind {
            CmpKind::Eq | CmpKind::TEq => monoasm! { self.jit, seteq rax; },
            CmpKind::Ne => monoasm! { self.jit, setne rax; },
            CmpKind::Ge => monoasm! { self.jit, setae rax; },
            CmpKind::Gt => monoasm! { self.jit, seta rax; },
            CmpKind::Le => monoasm! { self.jit, setbe rax; },
            CmpKind::Lt => monoasm! { self.jit, setb rax; },
            _ => unimplemented!(),
        }
        monoasm! { self.jit,
            shlq rax, 3;
            orq rax, (FALSE_VALUE);
        };
    }

    pub(super) fn gen_cmp_opt(
        &mut self,
        ctx: &mut BBContext,
        cc: &mut JitContext,
        fnstore: &FnStore,
        mode: OpMode,
        kind: CmpKind,
        ret: SlotId,
        pc: BcPc,
        index: usize,
    ) {
        match (pc + 1).get_ir(fnstore) {
            TraceIr::CondBr(_, disp, true, brkind) => {
                let dest_idx = (index as i32 + disp + 1) as usize;
                let branch_dest = self.jit.label();
                if mode.is_float_op(&*pc) {
                    match mode {
                        OpMode::RR(lhs, rhs) => {
                            let (flhs, frhs) = self.xmm_read_binary(ctx, lhs, rhs, pc);
                            ctx.dealloc_xmm(ret);
                            monoasm! { self.jit,
                                ucomisd xmm(flhs.enc()), xmm(frhs.enc());
                            };
                        }
                        OpMode::RI(lhs, rhs) => {
                            let rhs_label = self.jit.const_f64(rhs as f64);
                            let flhs = self.xmm_read_assume_float(ctx, lhs, pc);
                            ctx.dealloc_xmm(ret);
                            monoasm! { self.jit,
                                ucomisd xmm(flhs.enc()), [rip + rhs_label];
                            };
                        }
                        _ => unreachable!(),
                    }
                    self.cmp_opt_float(kind, branch_dest, brkind);
                } else {
                    self.writeback_binary(ctx, &mode);
                    ctx.dealloc_xmm(ret);
                    if mode.is_integer_op(&*pc) {
                        let deopt = self.gen_side_deopt(pc, ctx);
                        match mode {
                            OpMode::RR(lhs, rhs) => {
                                self.load_guard_binary_fixnum(lhs, rhs, deopt);
                                monoasm!(self.jit,
                                    cmpq rdi, rsi;
                                );
                            }
                            OpMode::RI(lhs, rhs) => {
                                self.load_guard_rdi_fixnum(lhs, deopt);
                                monoasm!(self.jit,
                                    cmpq rdi, (Value::int32(rhs as i32).get());
                                );
                            }
                            OpMode::IR(lhs, rhs) => {
                                self.load_guard_rsi_fixnum(rhs, deopt);
                                monoasm!(self.jit,
                                    movq rdi, (Value::int32(lhs as i32).get());
                                    cmpq rdi, rsi;
                                );
                            }
                        }
                        self.cmp_opt_int(kind, branch_dest, brkind);
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
            CmpKind::Eq => self.call_binop(cmp_eq_values as _),
            CmpKind::Ne => self.call_binop(cmp_ne_values as _),
            CmpKind::Ge => self.call_binop(cmp_ge_values as _),
            CmpKind::Gt => self.call_binop(cmp_gt_values as _),
            CmpKind::Le => self.call_binop(cmp_le_values as _),
            CmpKind::Lt => self.call_binop(cmp_lt_values as _),
            CmpKind::TEq => self.call_binop(cmp_teq_values as _),
            _ => unimplemented!(),
        }
        self.xmm_restore(&xmm_using);
    }

    pub(super) fn integer_cmp(&mut self, kind: CmpKind) {
        match kind {
            CmpKind::Eq => self.integer_cmp_eq(),
            CmpKind::Ne => self.integer_cmp_ne(),
            CmpKind::Ge => self.integer_cmp_ge(),
            CmpKind::Gt => self.integer_cmp_gt(),
            CmpKind::Le => self.integer_cmp_le(),
            CmpKind::Lt => self.integer_cmp_lt(),
            CmpKind::TEq => self.integer_cmp_eq(),
            _ => unimplemented!(),
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
        self.handle_error(pc);
        monoasm!(self.jit,
            orq  rax, 0x10;
            cmpq rax, (FALSE_VALUE);
            // if true, Z=0(not set).
        );
        match brkind {
            BrKind::BrIf => monoasm! { self.jit,
                jnz branch_dest;
            },
            BrKind::BrIfNot => monoasm! { self.jit,
                jz  branch_dest;
            },
        }
    }

    pub(super) fn cmp_opt_int(&mut self, kind: CmpKind, branch_dest: DestLabel, brkind: BrKind) {
        match kind {
            CmpKind::Eq => self.cmp_opt_int_eq(branch_dest, brkind),
            CmpKind::Ne => self.cmp_opt_int_ne(branch_dest, brkind),
            CmpKind::Ge => self.cmp_opt_int_ge(branch_dest, brkind),
            CmpKind::Gt => self.cmp_opt_int_gt(branch_dest, brkind),
            CmpKind::Le => self.cmp_opt_int_le(branch_dest, brkind),
            CmpKind::Lt => self.cmp_opt_int_lt(branch_dest, brkind),
            CmpKind::TEq => self.cmp_opt_int_eq(branch_dest, brkind),
            _ => unimplemented!(),
        }
    }

    pub(super) fn cmp_opt_float(&mut self, kind: CmpKind, branch_dest: DestLabel, brkind: BrKind) {
        match kind {
            CmpKind::Eq => self.cmp_opt_float_eq(branch_dest, brkind),
            CmpKind::Ne => self.cmp_opt_float_ne(branch_dest, brkind),
            CmpKind::Ge => self.cmp_opt_float_ge(branch_dest, brkind),
            CmpKind::Gt => self.cmp_opt_float_gt(branch_dest, brkind),
            CmpKind::Le => self.cmp_opt_float_le(branch_dest, brkind),
            CmpKind::Lt => self.cmp_opt_float_lt(branch_dest, brkind),
            CmpKind::TEq => self.cmp_opt_float_eq(branch_dest, brkind),
            _ => unimplemented!(),
        }
    }
}

impl Codegen {
    pub(super) fn writeback_binary(&mut self, ctx: &mut BBContext, mode: &OpMode) {
        match mode {
            OpMode::RR(lhs, rhs) => {
                self.write_back_slot(ctx, *lhs);
                self.write_back_slot(ctx, *rhs);
            }
            OpMode::RI(lhs, _) => {
                self.write_back_slot(ctx, *lhs);
            }
            OpMode::IR(_, rhs) => {
                self.write_back_slot(ctx, *rhs);
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
                monoasm!(self.jit,
                    movq rsi, (Value::int32(rhs as i32).get());
                );
            }
            OpMode::IR(lhs, rhs) => {
                monoasm!(self.jit,
                    movq rdi, (Value::int32(lhs as i32).get());
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
                monoasm!(self.jit,
                    movq rsi, (Value::int32(rhs as i32).get());
                );
            }
            OpMode::IR(lhs, rhs) => {
                self.load_guard_rsi_fixnum(rhs, deopt);
                monoasm!(self.jit,
                    movq rdi, (Value::int32(lhs as i32).get());
                );
            }
        }
    }

    fn shift_under(&mut self, under: DestLabel, after: DestLabel) {
        self.jit.select_page(1);
        let zero = self.jit.label();
        monoasm!(self.jit,
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
        monoasm!(self.jit,
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
        monoasm!(self.jit,
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
        monoasm!(self.jit,
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
        monoasm!(self.jit,
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

    fn generic_binop(&mut self, ret: SlotId, func: usize, xmm_using: UsingXmm, pc: BcPc) {
        self.xmm_save(&xmm_using);
        self.call_binop(func);
        self.xmm_restore(&xmm_using);
        self.handle_error(pc);
        self.store_rax(ret);
    }
}