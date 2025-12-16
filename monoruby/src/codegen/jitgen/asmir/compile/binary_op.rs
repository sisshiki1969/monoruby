use super::*;

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
impl Codegen {
    pub(super) fn integer_binop(
        &mut self,
        lhs: GP,
        rhs: GP,
        mode: &OpMode,
        kind: BinOpK,
        deopt: &DestLabel,
    ) {
        let lhs_r = lhs as u64;
        let rhs_r = rhs as u64;
        assert_eq!(0, self.jit.get_page());
        match kind {
            BinOpK::Add => {
                let overflow = self.jit.label();
                match mode {
                    OpMode::RR(_, _) => {
                        monoasm!( &mut self.jit,
                            subq R(lhs_r), 1;
                            addq R(lhs_r), R(rhs_r);
                            jo overflow;
                        );
                    }
                    OpMode::RI(_, i) | OpMode::IR(i, _) => {
                        monoasm!( &mut self.jit,
                            addq  R(lhs_r), (Value::i32(*i as i32).id() - 1);
                            jo overflow;
                        );
                    }
                }
                self.jit.select_page(1);
                monoasm!( &mut self.jit,
                overflow:
                    movq rdi, (Value::symbol_from_str("_arith_overflow").id());
                    jmp deopt;
                );
                self.jit.select_page(0);
            }
            BinOpK::Mul => {
                let overflow = self.jit.label();
                match mode {
                    OpMode::RR(_, _) => {
                        monoasm!( &mut self.jit,
                            sarq R(rhs_r), 1;
                        );
                    }
                    OpMode::RI(_, i) | OpMode::IR(i, _) => {
                        monoasm!( &mut self.jit,
                            movq R(rhs_r), (*i as i64);
                        );
                    }
                }
                monoasm!( &mut self.jit,
                    subq R(lhs_r), 1;
                    imul R(lhs_r), R(rhs_r);
                    jo overflow;
                    orq  R(lhs_r), 1;
                );
                self.jit.select_page(1);
                monoasm!( &mut self.jit,
                overflow:
                    movq rdi, (Value::symbol_from_str("_arith_overflow").id());
                    jmp deopt;
                );
                self.jit.select_page(0);
            }
            BinOpK::Sub => {
                let overflow = self.jit.label();
                match mode {
                    OpMode::RR(_, _) => {
                        monoasm!( &mut self.jit,
                            subq R(lhs_r), R(rhs_r);
                            jo overflow;
                            addq R(lhs_r), 1;
                        );
                    }
                    OpMode::RI(_, rhs) => {
                        monoasm!( &mut self.jit,
                            subq R(lhs_r), (Value::i32(*rhs as i32).id() - 1);
                            jo overflow;
                        );
                    }
                    OpMode::IR(lhs, _) => {
                        monoasm!( &mut self.jit,
                            movq R(lhs_r), (Value::i32(*lhs as i32).id());
                            subq R(lhs_r), R(rhs_r);
                            jo overflow;
                            addq R(lhs_r), 1;
                        );
                    }
                }
                self.jit.select_page(1);
                monoasm!( &mut self.jit,
                overflow:
                    movq rdi, (Value::symbol_from_str("_arith_overflow").id());
                    jmp deopt;
                );
                self.jit.select_page(0);
            }
            BinOpK::Div => {
                let zero_div = self.jit.label();
                monoasm!( &mut self.jit,
                    sarq R(rhs_r), 1;
                    testq R(rhs_r), R(rhs_r);
                    jeq  zero_div;
                    movq rax, R(lhs_r);
                    sarq rax, 1;
                    cqo;
                    idiv R(rhs_r);
                    salq rax, 1;
                    orq  rax, 1;
                );
                self.jit.select_page(1);
                monoasm!( &mut self.jit,
                zero_div:
                    movq rdi, (Value::symbol_from_str("_divide_by_zero").id());
                    jmp deopt;
                );
                self.jit.select_page(0);
            }
            BinOpK::Rem => {
                let zero_div = self.jit.label();
                monoasm!( &mut self.jit,
                    sarq R(rhs_r), 1;
                    testq R(rhs_r), R(rhs_r);
                    jeq  zero_div;
                    movq rax, R(lhs_r);
                    sarq rax, 1;
                    cqo;
                    idiv R(rhs_r);
                    movq rax, rdx;
                    salq rax, 1;
                    orq  rax, 1;
                );
                self.jit.select_page(1);
                monoasm!( &mut self.jit,
                zero_div:
                    movq rdi, (Value::symbol_from_str("_divide_by_zero").id());
                    jmp deopt;
                );
                self.jit.select_page(0);
            }
            BinOpK::BitOr => match mode {
                OpMode::RR(_, _) => {
                    monoasm!( &mut self.jit,
                        orq R(lhs as u64), R(rhs as u64);
                    );
                }
                OpMode::RI(_, i) | OpMode::IR(i, _) => {
                    monoasm!( &mut self.jit,
                        orq R(lhs as u64), (Value::i32(*i as i32).id());
                    );
                }
            },
            BinOpK::BitAnd => match mode {
                OpMode::RR(_, _) => {
                    monoasm!( &mut self.jit,
                        andq R(lhs as u64), R(rhs as u64);
                    );
                }
                OpMode::RI(_, i) | OpMode::IR(i, _) => {
                    monoasm!( &mut self.jit,
                        andq R(lhs as u64), (Value::i32(*i as i32).id());
                    );
                }
            },
            BinOpK::BitXor => match mode {
                OpMode::RR(_, _) => {
                    monoasm!( &mut self.jit,
                        xorq R(lhs as u64), R(rhs as u64);
                        addq R(lhs as u64), 1;
                    );
                }
                OpMode::RI(_, i) | OpMode::IR(i, _) => {
                    monoasm!( &mut self.jit,
                        xorq R(lhs as u64), (Value::i32(*i as i32).id() - 1);
                    );
                }
            },
            _ => unreachable!(),
        }
    }

    pub(super) fn integer_exp(&mut self, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            sarq rdi, 1;
            sarq rsi, 1;
            movq rax, (pow_ii as u64);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }
}

impl Codegen {
    ///
    /// Float binary operation with registers.
    ///
    /// ### in
    /// - xmm(*l*): lhs
    /// - xmm(*r*): rhs
    ///
    /// ### out
    /// - xmm(*dst*): dst
    ///
    /// ### destroy
    /// - caller save registers
    /// - stack
    ///
    pub(super) fn float_binop(
        &mut self,
        kind: BinOpK,
        using_xmm: UsingXmm,
        dst: Xmm,
        mode: (Xmm, Xmm),
    ) {
        let (l, r) = mode;
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
            BinOpK::Rem => {
                self.xmm_save(using_xmm);
                monoasm!( &mut self.jit,
                    movq xmm0, xmm(lhs);
                    movq xmm1, xmm(rhs);
                    movq rax, (rem_ff_f as u64);
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

extern "C" fn pow_ff_f(lhs: f64, rhs: f64) -> f64 {
    lhs.powf(rhs)
}

extern "C" fn rem_ff_f(lhs: f64, rhs: f64) -> f64 {
    lhs.rem_euclid(rhs)
}

macro_rules! cmp_main {
    ($op:ident) => {
        paste! {
            pub(in crate::codegen) fn [<icmp_ $op>](&mut self) {
                monoasm! { &mut self.jit,
                    xorq rax, rax;
                    cmpq rdi, rsi;
                    [<set $op>] rax;
                    shlq rax, 3;
                    orq rax, (FALSE_VALUE);
                };
            }

            pub(in crate::codegen) fn [<set_ $op>](&mut self) {
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

macro_rules! jit_cmp_opt_main {
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
        jit_cmp_opt_main!(($op1, $rev_op1, $sop1, $rev_sop1));
        jit_cmp_opt_main!($(($op2, $rev_op2, $sop2, $rev_sop2)),+);
    };
}

impl Codegen {
    pub(super) fn cmp_float(&mut self, mode: (Xmm, Xmm)) {
        let (l, r) = mode;
        monoasm! { &mut self.jit,
            ucomisd xmm(l.enc()), xmm(r.enc());
        };
    }

    pub(super) fn setflag_float(&mut self, kind: CmpKind) {
        match kind {
            CmpKind::Eq | CmpKind::TEq => monoasm! { &mut self.jit, seteq rax; },
            CmpKind::Ne => monoasm! { &mut self.jit, setne rax; },
            CmpKind::Ge => monoasm! { &mut self.jit, setae rax; },
            CmpKind::Gt => monoasm! { &mut self.jit, seta rax; },
            CmpKind::Le => monoasm! { &mut self.jit, setbe rax; },
            CmpKind::Lt => monoasm! { &mut self.jit, setb rax; },
        }
        monoasm! { &mut self.jit,
            shlq rax, 3;
            orq rax, (FALSE_VALUE);
        };
    }

    cmp_main!(eq, ne, lt, le, gt, ge);

    pub(super) fn integer_cmp(&mut self, kind: CmpKind, mode: OpMode, lhs: GP, rhs: GP) {
        monoasm! { &mut self.jit,
            xorq rax, rax;
        };
        self.cmp_integer(&mode, lhs, rhs);
        self.flag_to_bool(kind);
    }

    fn flag_to_bool(&mut self, kind: CmpKind) {
        match kind {
            CmpKind::Eq => self.set_eq(),
            CmpKind::Ne => self.set_ne(),
            CmpKind::Ge => self.set_ge(),
            CmpKind::Gt => self.set_gt(),
            CmpKind::Le => self.set_le(),
            CmpKind::Lt => self.set_lt(),
            CmpKind::TEq => self.set_eq(),
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
        }
    }

    jit_cmp_opt_main!(
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
        }
    }

    ///
    /// Compare two values with *mode*, and set flags.
    ///
    /// ### in
    ///
    /// ~~~text
    /// +-----+--------------------+
    /// |     |        mode        |
    /// |     +--------------------+
    /// |     |  RR     RI     IR  |
    /// +--------------------------+
    /// | rdi | lhs    lhs    ---  |
    /// +--------------------------|
    /// | rsi | rhs    ---    rhs  |
    /// +--------------------------+
    /// ~~~
    ///
    /// ### destroy
    ///
    /// - rdi
    ///
    pub(super) fn cmp_integer(&mut self, mode: &OpMode, lhs: GP, rhs: GP) {
        match mode {
            OpMode::RR(..) => {
                monoasm!( &mut self.jit,
                    cmpq R(lhs as u64), R(rhs as u64);
                );
            }
            OpMode::RI(_, r) => {
                monoasm!( &mut self.jit,
                    cmpq R(lhs as u64), (Value::i32(*r as i32).id());
                );
            }
            OpMode::IR(l, _) => {
                monoasm!( &mut self.jit,
                    movq R(lhs as u64), (Value::i32(*l as i32).id());
                    cmpq R(lhs as u64), R(rhs as u64);
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
    /// - rcx: rhs:Value
    ///
    /// ### out
    /// - rdi: result:Value
    ///
    /// ### destroy
    /// - rax
    /// - rcx
    ///
    pub(crate) fn gen_shr(&mut self, deopt: &DestLabel) {
        let shl = self.jit.label();
        let after = self.jit.label();
        let under = self.jit.label();
        monoasm!( &mut self.jit,
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
        if imm >= 64 {
            let exit = self.jit.label();
            let zero = self.jit.label();
            monoasm! { &mut self.jit,
                testq rdi, rdi;
                jns zero;
                movq rdi, (Value::i32(-1).id());
                jmp exit;
            zero:
                movq rdi, (Value::i32(0).id());
            exit:
            }
        } else {
            monoasm! { &mut self.jit,
                sarq rdi, (imm);
                orq rdi, 1;
            }
        }
    }

    ///
    /// gen code for shift-left of integer.
    ///
    /// ### in
    /// - rdi: lhs:Value
    /// - rcx: rhs:Value
    ///
    /// ### out
    /// - rdi: result:Value
    ///
    /// ### destroy
    /// - rax
    /// - rcx
    ///
    pub(crate) fn gen_shl(&mut self, deopt: &DestLabel) {
        let shr = self.jit.label();
        let after = self.jit.label();
        let under = self.jit.label();
        monoasm!( &mut self.jit,
            sarq rcx, 1;
            js shr;
            lzcntq rax, rdi;
            cmpq rax, rcx;
            jle deopt;
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
    /// gen code for shift-left of integer (lhs must be a Fixnum).
    ///
    /// ### in
    /// - rdi: lhs:Fixnum
    /// - rcx: rhs:Value
    ///
    /// ### out
    /// - rdi: result:Value
    ///
    /// ### destroy
    /// - rax
    /// - rcx
    ///
    pub(crate) fn gen_shl_lhs_imm(&mut self, lhs: i64, deopt: &DestLabel) {
        let shr = self.jit.label();
        let after = self.jit.label();
        let under = self.jit.label();
        let lhs = Value::fixnum(lhs);
        let lzcnt = lhs.id().leading_zeros();
        monoasm!( &mut self.jit,
            sarq rcx, 1;
            js shr;
            cmpq rcx, (lzcnt);
            jgt deopt;
            subq rdi, 1;
            salq rdi, rcx;
        after:
            orq rdi, 1;
        );
        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        shr:
            movq rdi, (lhs.id());
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
    pub(crate) fn gen_shl_rhs_imm(&mut self, rhs: u8, deopt: &DestLabel) {
        monoasm!( &mut self.jit,
            lzcntq rax, rdi;
            cmpq rax, (rhs);
            jle deopt;
            subq rdi, 1;
            shlq rdi, (rhs);
            orq rdi, 1;
        );
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn rem() {
        run_test("a = 3456; a % 64");
        run_test("a = 3456; a % 32");
        run_test("a = 3456; a % 16");
        run_test("a = 3456; a % 8");
        run_test("a = 3456; a % 4");
        run_test("a = 3456; a % 2");
    }

    #[test]
    fn constant_folding() {
        run_test("584+1+5-(3+91)*56");
        run_test("if 584+(1+5)-(3+91)*56%(1+5) == 0 then 1 else 0 end");
        run_test("if 584+(1+5)-(3+91)*56%(1+5) != 0 then 1 else 0 end");
        run_test("if 584+(1+5)-(3+91)*56%(1+5) < 0 then 1 else 0 end");
        run_test("if 584+(1+5)-(3+91)*56%(1+5) <= 0 then 1 else 0 end");
        run_test("if 584+(1+5)-(3+91)*56%(1+5) > 0 then 1 else 0 end");
        run_test("if 584+(1+5)-(3+91)*56%(1+5) >= 0 then 1 else 0 end");

        run_test("if 58.4+(1.7+5)-(3+91.7)*56/(0.1+5) == 0 then 1 else 0 end");
        run_test("if 58.4+(1.7+5)-(3+91.7)*56/(0.1+5) != 0 then 1 else 0 end");
        run_test("if 58.4+(1.7+5)-(3+91.7)*56/(0.1+5) < 0 then 1 else 0 end");
        run_test("if 58.4+(1.7+5)-(3+91.7)*56/(0.1+5) <= 0 then 1 else 0 end");
        run_test("if 58.4+(1.7+5)-(3+91.7)*56/(0.1+5) > 0 then 1 else 0 end");
        run_test("if 58.4+(1.7+5)-(3+91.7)*56/(0.1+5) >= 0 then 1 else 0 end");
    }
}
