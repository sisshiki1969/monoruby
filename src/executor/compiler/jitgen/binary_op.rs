use super::*;
use ruruby_parse::CmpKind;

macro_rules! cmp_main {
    ($op:ident) => {
        paste! {
            fn [<cmp_ $op>](&mut self, generic:DestLabel, xmm_using: Vec<Xmm>) {
                let exit = self.jit.label();
                self.[<integer_cmp_ $op>]();
                self.jit.bind_label(exit);
                self.jit.select_page(1);
                self.jit.bind_label(generic);
                self.xmm_save(&xmm_using);
                self.call_binop([<cmp_ $op _values>] as _);
                self.xmm_restore(&xmm_using);
                monoasm!(self.jit,
                    jmp  exit;
                );
                self.jit.select_page(0);
            }

            pub(in crate::executor::compiler) fn [<integer_cmp_ $op>](&mut self) {
                monoasm! { self.jit,
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
            fn [<cmp_opt_int_ $sop>](&mut self, branch_dest: DestLabel, generic:DestLabel, brkind: BrKind, xmm_using: Vec<Xmm>) {
                let cont = self.jit.label();
                match brkind {
                    BrKind::BrIf => monoasm! { self.jit,
                        [<j $sop>] branch_dest;
                    },
                    BrKind::BrIfNot => monoasm! { self.jit,
                        [<j $rev_sop>] branch_dest;
                    },
                }
                self.jit.bind_label(cont);
                self.jit.select_page(1);
                self.jit.bind_label(generic);
                self.xmm_save(&xmm_using);
                self.call_binop([<cmp_ $sop _values>] as _);
                self.xmm_restore(&xmm_using);
                monoasm!(self.jit,
                    orq  rax, 0x10;
                    cmpq rax, (FALSE_VALUE);
                    // if true, Z=0(not set).
                );
                match brkind {
                    BrKind::BrIf => monoasm! { self.jit,
                        jz  cont;
                        jmp branch_dest;
                    },
                    BrKind::BrIfNot => monoasm! { self.jit,
                        jnz  cont;
                        jmp branch_dest;
                    },
                }
                self.jit.select_page(0);
            }

            fn [<cmp_opt_float_ $sop>](&mut self, branch_dest: DestLabel, brkind: BrKind) {
                let cont = self.jit.label();
                match brkind {
                    BrKind::BrIf => monoasm! { self.jit,
                        [<j $op>] branch_dest;
                    },
                    BrKind::BrIfNot => monoasm! { self.jit,
                        [<j $rev_op>] branch_dest;
                    },
                }
                self.jit.bind_label(cont);
            }
        }
    };
    (($op1:ident, $rev_op1:ident, $sop1:ident, $rev_sop1:ident), $(($op2:ident, $rev_op2:ident, $sop2:ident, $rev_sop2:ident)),+) => {
        cmp_opt_main!(($op1, $rev_op1, $sop1, $rev_sop1));
        cmp_opt_main!($(($op2, $rev_op2, $sop2, $rev_sop2)),+);
    };
}

impl Codegen {
    cmp_opt_main!(
        (eq, ne, eq, ne),
        (ne, eq, ne, eq),
        (a, be, gt, le),
        (b, ae, lt, ge),
        (ae, b, ge, lt),
        (be, a, le, gt)
    );
    cmp_main!(eq, ne, lt, le, gt, ge);

    pub(super) fn gen_binop_integer(
        &mut self,
        pc: BcPc,
        kind: BinOpK,
        ret: SlotId,
        mode: BinOpMode,
        ctx: &BBContext,
    ) {
        let xmm_using = ctx.get_xmm_using();
        let deopt = self.gen_side_deopt(pc, ctx);
        match kind {
            BinOpK::Add => {
                match mode {
                    BinOpMode::RR(lhs, rhs) => {
                        self.load_guard_binary_fixnum(lhs, rhs, deopt);
                        monoasm!(self.jit,
                            // fastpath
                            subq rdi, 1;
                            addq rdi, rsi;
                            jo deopt;
                        );
                        self.store_rdi(ret);
                    }
                    BinOpMode::RI(lhs, rhs) => {
                        self.load_guard_rdi_fixnum(lhs, deopt);
                        monoasm!(self.jit,
                            // fastpath
                            addq rdi, (Value::int32(rhs as i32).get() - 1);
                            jo deopt;
                        );
                        self.store_rdi(ret);
                    }
                    BinOpMode::IR(lhs, rhs) => {
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
                    BinOpMode::RR(lhs, rhs) => {
                        self.load_guard_binary_fixnum(lhs, rhs, deopt);
                        monoasm!(self.jit,
                            // fastpath
                            subq rdi, rsi;
                            jo deopt;
                            addq rdi, 1;
                        );
                        self.store_rdi(ret);
                    }
                    BinOpMode::RI(lhs, rhs) => {
                        self.load_guard_rdi_fixnum(lhs, deopt);
                        monoasm!(self.jit,
                            // fastpath
                            subq rdi, (Value::int32(rhs as i32).get() - 1);
                            jo deopt;
                        );
                        self.store_rdi(ret);
                    }
                    BinOpMode::IR(lhs, rhs) => {
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
            BinOpK::Mul => {
                self.load_binary_args_with_mode(&mode);
                self.generic_binop(ret, mul_values as _, xmm_using, pc);
            }
            BinOpK::Div => {
                self.load_binary_args_with_mode(&mode);
                self.generic_binop(ret, div_values as _, xmm_using, pc);
            }
            BinOpK::Rem => {
                self.load_binary_args_with_mode(&mode);
                self.generic_binop(ret, rem_values as _, xmm_using, pc);
            }
            BinOpK::Exp => {
                self.xmm_save(&xmm_using);
                match mode {
                    BinOpMode::RR(lhs, rhs) => {
                        self.load_guard_binary_fixnum(lhs, rhs, deopt);
                        monoasm!(self.jit,
                            sarq rdi, 1;
                            sarq rsi, 1;
                        );
                    }
                    BinOpMode::RI(lhs, rhs) => {
                        self.load_guard_rdi_fixnum(lhs, deopt);
                        monoasm!(self.jit,
                            sarq rdi, 1;
                            movq rsi, (rhs);
                        );
                    }
                    BinOpMode::IR(lhs, rhs) => {
                        self.load_guard_rsi_fixnum(rhs, deopt);
                        monoasm!(self.jit,
                            movq rdi, (lhs);
                            sarq rsi, 1;
                        );
                    }
                }
                monoasm!(self.jit,
                    movq rax, (pow_ii as u64);
                    call rax;
                );
                self.xmm_restore(&xmm_using);
                self.store_rax(ret);
            }
            _ => {
                let generic = self.jit.label();
                self.load_binary_args_with_mode(&mode);
                self.guard_binary_fixnum_with_mode(generic, mode);
                match kind {
                    BinOpK::BitOr => self.gen_bit_or(generic, ret, xmm_using, pc),
                    BinOpK::BitAnd => self.gen_bit_and(generic, ret, xmm_using, pc),
                    BinOpK::BitXor => self.gen_bit_xor(generic, ret, xmm_using, pc),
                    BinOpK::Shr => self.gen_shr(generic, ret, xmm_using, pc),
                    BinOpK::Shl => self.gen_shl(generic, ret, xmm_using, pc),
                    _ => unimplemented!(),
                }
            }
        }
    }

    pub(super) fn gen_binop_float(
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
            CmpKind::Eq => monoasm! { self.jit, seteq rax; },
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

    pub(super) fn gen_cmp_prep(&mut self, lhs: SlotId, rhs: SlotId, generic: DestLabel) {
        self.load_binary_args(lhs, rhs);
        self.guard_rdi_rsi_fixnum(generic);
    }

    pub(super) fn gen_cmpri_prep(&mut self, lhs: SlotId, rhs: i16, generic: DestLabel) {
        self.load_rdi(lhs);
        monoasm!(self.jit,
            movq rsi, (Value::new_integer(rhs as i64).get());
        );
        self.guard_rdi_fixnum(generic);
    }

    pub(super) fn gen_cmp_kind(
        &mut self,
        kind: CmpKind,
        generic: DestLabel,
        ret: SlotId,
        ctx: &BBContext,
        pc: BcPc,
    ) {
        let xmm_using = ctx.get_xmm_using();
        match kind {
            CmpKind::Eq => self.cmp_eq(generic, xmm_using),
            CmpKind::Ne => self.cmp_ne(generic, xmm_using),
            CmpKind::Ge => self.cmp_ge(generic, xmm_using),
            CmpKind::Gt => self.cmp_gt(generic, xmm_using),
            CmpKind::Le => self.cmp_le(generic, xmm_using),
            CmpKind::Lt => self.cmp_lt(generic, xmm_using),
            _ => unimplemented!(),
        }
        self.handle_error(pc);
        self.store_rax(ret);
    }

    pub(super) fn gen_integer_cmp_kind(&mut self, kind: CmpKind, ret: SlotId, pc: BcPc) {
        match kind {
            CmpKind::Eq => self.integer_cmp_eq(),
            CmpKind::Ne => self.integer_cmp_ne(),
            CmpKind::Ge => self.integer_cmp_ge(),
            CmpKind::Gt => self.integer_cmp_gt(),
            CmpKind::Le => self.integer_cmp_le(),
            CmpKind::Lt => self.integer_cmp_lt(),
            _ => unimplemented!(),
        }
        self.handle_error(pc);
        self.store_rax(ret);
    }

    pub(super) fn gen_cmp_int_opt(
        &mut self,
        kind: CmpKind,
        branch_dest: DestLabel,
        generic: DestLabel,
        brkind: BrKind,
        xmm_using: Vec<Xmm>,
    ) {
        match kind {
            CmpKind::Eq => self.cmp_opt_int_eq(branch_dest, generic, brkind, xmm_using),
            CmpKind::Ne => self.cmp_opt_int_ne(branch_dest, generic, brkind, xmm_using),
            CmpKind::Ge => self.cmp_opt_int_ge(branch_dest, generic, brkind, xmm_using),
            CmpKind::Gt => self.cmp_opt_int_gt(branch_dest, generic, brkind, xmm_using),
            CmpKind::Le => self.cmp_opt_int_le(branch_dest, generic, brkind, xmm_using),
            CmpKind::Lt => self.cmp_opt_int_lt(branch_dest, generic, brkind, xmm_using),
            _ => unimplemented!(),
        }
    }

    pub(super) fn gen_cmp_float_opt(
        &mut self,
        kind: CmpKind,
        branch_dest: DestLabel,
        brkind: BrKind,
    ) {
        match kind {
            CmpKind::Eq => self.cmp_opt_float_eq(branch_dest, brkind),
            CmpKind::Ne => self.cmp_opt_float_ne(branch_dest, brkind),
            CmpKind::Ge => self.cmp_opt_float_ge(branch_dest, brkind),
            CmpKind::Gt => self.cmp_opt_float_gt(branch_dest, brkind),
            CmpKind::Le => self.cmp_opt_float_le(branch_dest, brkind),
            CmpKind::Lt => self.cmp_opt_float_lt(branch_dest, brkind),
            _ => unimplemented!(),
        }
    }
}

impl Codegen {
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

    fn gen_bit_or(&mut self, generic: DestLabel, ret: SlotId, xmm_using: UsingXmm, pc: BcPc) {
        monoasm!(self.jit,
            // fastpath
            orq rdi, rsi;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, bitor_values as _, xmm_using, pc);
    }

    fn gen_bit_and(&mut self, generic: DestLabel, ret: SlotId, xmm_using: UsingXmm, pc: BcPc) {
        monoasm!(self.jit,
            // fastpath
            andq rdi, rsi;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, bitand_values as _, xmm_using, pc);
    }

    fn gen_bit_xor(&mut self, generic: DestLabel, ret: SlotId, xmm_using: UsingXmm, pc: BcPc) {
        monoasm!(self.jit,
            // fastpath
            xorq rdi, rsi;
            addq rdi, 1;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, bitxor_values as _, xmm_using, pc);
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

    fn gen_shr(&mut self, generic: DestLabel, ret: SlotId, xmm_using: UsingXmm, pc: BcPc) {
        let shl = self.jit.label();
        let after = self.jit.label();
        let under = self.jit.label();
        monoasm!(self.jit,
            // fastpath
            movq rcx, rsi;
            sarq rcx, 1;
            js shl;
            cmpq rcx, 64;
            jge under;
            sarq rdi, rcx;
        after:
            orq rdi, 1;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, shr_values as _, xmm_using, pc);
        self.jit.select_page(1);
        monoasm!(self.jit,
        shl:
            negq rcx;
            lzcntq rax, rdi;
            cmpq rcx, rax;
            jgt generic;
            subq rdi, 1;
            salq rdi, rcx;
            jmp after;
        );
        self.jit.select_page(0);
        self.shift_under(under, after);
    }

    fn gen_shl(&mut self, generic: DestLabel, ret: SlotId, xmm_using: UsingXmm, pc: BcPc) {
        let shr = self.jit.label();
        let after = self.jit.label();
        let under = self.jit.label();
        monoasm!(self.jit,
            // fastpath
            movq rcx, rsi;
            sarq rcx, 1;
            js shr;
            lzcntq rax, rdi;
            cmpq rcx, rax;
            jgt generic;
            subq rdi, 1;
            salq rdi, rcx;
        after:
            orq rdi, 1;
        );
        self.store_rdi(ret);

        self.side_generic_op(generic, ret, shl_values as _, xmm_using, pc);
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

    fn side_generic_op(
        &mut self,
        generic: DestLabel,
        ret: SlotId,
        func: usize,
        xmm_using: UsingXmm,
        pc: BcPc,
    ) {
        let exit = self.jit.label();
        self.jit.bind_label(exit);
        self.jit.select_page(1);
        self.jit.bind_label(generic);
        self.generic_binop(ret, func, xmm_using, pc);
        monoasm!(self.jit,
            jmp  exit;
        );
        self.jit.select_page(0);
    }

    fn generic_binop(&mut self, ret: SlotId, func: usize, xmm_using: UsingXmm, pc: BcPc) {
        self.xmm_save(&xmm_using);
        self.call_binop(func);
        self.xmm_restore(&xmm_using);
        self.handle_error(pc);
        self.store_rax(ret);
    }

    fn guard_binary_fixnum_with_mode(&mut self, generic: DestLabel, mode: BinOpMode) {
        match mode {
            BinOpMode::RR(..) => self.guard_rdi_rsi_fixnum(generic),
            BinOpMode::RI(..) => self.guard_rdi_fixnum(generic),
            BinOpMode::IR(..) => self.guard_rsi_fixnum(generic),
        }
    }

    fn load_binary_args_with_mode(&mut self, mode: &BinOpMode) {
        match *mode {
            BinOpMode::RR(lhs, rhs) => self.load_binary_args(lhs, rhs),
            BinOpMode::RI(lhs, rhs) => {
                self.load_rdi(lhs);
                monoasm!(self.jit,
                    movq rsi, (Value::int32(rhs as i32).get());
                );
            }
            BinOpMode::IR(lhs, rhs) => {
                monoasm!(self.jit,
                    movq rdi, (Value::int32(lhs as i32).get());
                );
                self.load_rsi(rhs);
            }
        }
    }
}
