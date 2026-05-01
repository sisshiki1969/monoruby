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
                let exit = self.jit.label();
                let negative_divisor = self.jit.label();
                let dec = self.jit.label();
                monoasm!( &mut self.jit,
                    sarq R(rhs_r), 1;
                    testq R(rhs_r), R(rhs_r);
                    jeq  zero_div;
                    movq rax, R(lhs_r);
                    sarq rax, 1;
                    cqo;
                    idiv R(rhs_r);
                    // rdx: remainder
                    // rax: quotient
                    // rhs: divisor
                    testq R(rhs_r), R(rhs_r);
                    js negative_divisor;
                    testq rdx, rdx;
                    js dec;
                    jmp exit;
                negative_divisor:
                    testq rdx, rdx;
                    jle exit;
                dec:
                    subq rax, 1;
                exit:
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
            BinOpK::Rem
            | BinOpK::Exp
            | BinOpK::BitOr
            | BinOpK::BitAnd
            | BinOpK::BitXor
            | BinOpK::Shl
            | BinOpK::Shr => unreachable!(),
        }
    }

    ///
    /// gen code for Integer#% (rem) of two fixnums.
    ///
    /// ### in
    /// - rdi: lhs:Fixnum (tagged)
    /// - rsi: rhs:Fixnum (tagged)
    ///
    /// ### out
    /// - rax: result:Fixnum (tagged)
    ///
    /// On zero divisor, jumps to `deopt`.
    ///
    /// ### destroy
    /// - rax, rdx, rsi
    ///
    pub(crate) fn gen_int_rem(&mut self, deopt: &DestLabel) {
        let zero_div = self.jit.label();
        let exit = self.jit.label();
        let negative_divisor = self.jit.label();
        let dec = self.jit.label();
        monoasm!( &mut self.jit,
            sarq rsi, 1;
            testq rsi, rsi;
            jeq  zero_div;
            movq rax, rdi;
            sarq rax, 1;
            cqo;
            idiv rsi;
            // rdx: remainder, rax: quotient, rsi: divisor
            testq rsi, rsi;
            js negative_divisor;
            testq rdx, rdx;
            js dec;
            jmp exit;
        negative_divisor:
            testq rdx, rdx;
            jle exit;
        dec:
            addq rdx, rsi;
        exit:
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

    ///
    /// gen code for Integer#** (exp) of two fixnums.
    ///
    /// ### in
    /// - rdi: lhs:Fixnum (tagged)
    /// - rsi: rhs:Fixnum (tagged)
    ///
    /// ### out
    /// - rax: result:Value (or 0 on error)
    ///
    /// On error (raised by `pow_ii`), jumps to `error`.
    ///
    /// ### destroy
    /// - caller-save registers
    ///
    pub(crate) fn gen_int_pow(&mut self, using_xmm: UsingXmm, error: &DestLabel) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            sarq rdi, 1;
            sarq rsi, 1;
            movq rdx, rbx;
            movq rax, (pow_ii as u64);
            call rax;
        );
        self.xmm_restore(using_xmm);
        monoasm!( &mut self.jit,
            testq rax, rax;
            jeq error;
        );
    }

    ///
    /// gen code for `Integer#%` with a Float rhs.
    ///
    /// Calls `rem_ff(lhs, rhs)` and stores the f64 result in `dst_xmm`.
    ///
    /// ### in
    /// - xmm(*lhs_xmm*): lhs as f64 (Integer converted to f64)
    /// - xmm(*rhs_xmm*): rhs as f64
    ///
    /// ### out
    /// - xmm(*dst_xmm*): result f64
    ///
    pub(crate) fn gen_int_rem_if(
        &mut self,
        lhs_xmm: VirtFPReg,
        rhs_xmm: VirtFPReg,
        dst_xmm: VirtFPReg,
        using_xmm: UsingXmm,
        base: usize,
    ) {
        self.xmm_save(using_xmm);
        self.load_xmm_into_xmm0(lhs_xmm, base);
        self.load_xmm_into_xmm1(rhs_xmm, base);
        monoasm!( &mut self.jit,
            movq rax, (rem_ff as u64);
            call rax;
        );
        self.xmm_restore(using_xmm);
        self.store_xmm0_into_xmm(dst_xmm, base);
    }

    ///
    /// gen code for `Integer#**` with a Float rhs.
    ///
    /// Calls `pow_ff(lhs, rhs)` and returns a `Value` (Float or Complex)
    /// in `rax`. The result may be Complex when lhs is negative and rhs
    /// is non-integer, so we cannot store it as a raw f64.
    ///
    /// ### in
    /// - xmm(*lhs_xmm*): lhs as f64
    /// - xmm(*rhs_xmm*): rhs as f64
    ///
    /// ### out
    /// - rax: result Value
    ///
    /// ### destroy
    /// - caller-save registers
    ///
    pub(crate) fn gen_int_pow_if(
        &mut self,
        lhs_xmm: VirtFPReg,
        rhs_xmm: VirtFPReg,
        using_xmm: UsingXmm,
        base: usize,
    ) {
        self.xmm_save(using_xmm);
        self.load_xmm_into_xmm0(lhs_xmm, base);
        self.load_xmm_into_xmm1(rhs_xmm, base);
        monoasm!( &mut self.jit,
            movq rax, (pow_ff as u64);
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
    ///
    /// Lower `dst <- lhs op rhs` where each operand may be a
    /// pool-allocated phys xmm or a stack-spilled `VirtFPReg`. The
    /// strategy mirrors the pattern documented in the spill design:
    ///
    /// 1. Pick `work` xmm = phys `dst` or scratch `xmm0` (when `dst`
    ///    is spilled).
    /// 2. Get `rhs` into a phys reg = its own phys (when not
    ///    spilled, and not aliasing `work` for non-commutative ops)
    ///    or scratch `xmm1`.
    /// 3. Load `lhs` into `work` (memory operand if spilled, plain
    ///    `movq` if not, no-op if `lhs` already lives in `work`).
    /// 4. Run the SSE op `work, rhs_reg` register-to-register so
    ///    every kind (commutative or not) follows the same shape.
    /// 5. Store-back `work` to the spill slot when `dst` was spilled.
    ///
    pub(super) fn float_binop(
        &mut self,
        kind: BinOpK,
        dst: VirtFPReg,
        binary_xmm: (VirtFPReg, VirtFPReg),
        base_stack_offset: usize,
    ) {
        let (l, r) = binary_xmm;
        let lhs_loc = xmm_loc(l, base_stack_offset);
        let rhs_loc = xmm_loc(r, base_stack_offset);
        let dst_loc = xmm_loc(dst, base_stack_offset);

        // Step 1: pick `work` for `dst`.
        let (work, dst_spill_off) = match dst_loc {
            XmmLoc::Phys(p) => (p, None),
            XmmLoc::Spill(off) => (0u64, Some(off)),
        };
        let commutative = matches!(kind, BinOpK::Add | BinOpK::Mul);

        // Step 2: route `rhs` into a phys reg distinct from `work`
        // for non-commutative kinds (otherwise the `op` would lose
        // `lhs` after the mov in step 3). Scratch xmm1 is the
        // fallback when `rhs` is spilled or the alias would clobber.
        let rhs_phys = match rhs_loc {
            XmmLoc::Phys(p) if p == work && !commutative => {
                // Save rhs to xmm1 before we overwrite `work` with
                // `lhs`.
                monoasm!( &mut self.jit,
                    movq xmm1, xmm(p);
                );
                1
            }
            XmmLoc::Phys(p) => p,
            XmmLoc::Spill(off) => {
                monoasm!( &mut self.jit,
                    movq xmm1, [rbp - (off)];
                );
                1
            }
        };

        // Step 3: load `lhs` into `work` (skip when already there).
        let lhs_in_work = matches!(lhs_loc, XmmLoc::Phys(p) if p == work);
        if !lhs_in_work {
            match lhs_loc {
                XmmLoc::Phys(p) => monoasm!( &mut self.jit,
                    movq xmm(work), xmm(p);
                ),
                XmmLoc::Spill(off) => monoasm!( &mut self.jit,
                    movq xmm(work), [rbp - (off)];
                ),
            }
        }

        // Step 4: run the op register-to-register.
        match kind {
            BinOpK::Add => monoasm!( &mut self.jit, addsd xmm(work), xmm(rhs_phys); ),
            BinOpK::Sub => monoasm!( &mut self.jit, subsd xmm(work), xmm(rhs_phys); ),
            BinOpK::Mul => monoasm!( &mut self.jit, mulsd xmm(work), xmm(rhs_phys); ),
            BinOpK::Div => monoasm!( &mut self.jit, divsd xmm(work), xmm(rhs_phys); ),
            _ => unreachable!(),
        }

        // Step 5: store-back if `dst` was spilled.
        if let Some(off) = dst_spill_off {
            monoasm!( &mut self.jit,
                movq [rbp - (off)], xmm(work);
            );
        }
    }
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
        }
    };
    (($op1:ident, $rev_op1:ident, $sop1:ident, $rev_sop1:ident), $(($op2:ident, $rev_op2:ident, $sop2:ident, $rev_sop2:ident)),+) => {
        jit_cmp_opt_main!(($op1, $rev_op1, $sop1, $rev_sop1));
        jit_cmp_opt_main!($(($op2, $rev_op2, $sop2, $rev_sop2)),+);
    };
}

impl Codegen {
    pub(super) fn cmp_float(
        &mut self,
        binary_xmm: (VirtFPReg, VirtFPReg),
        base_stack_offset: usize,
    ) {
        let (l, r) = binary_xmm;
        let l_loc = xmm_loc(l, base_stack_offset);
        let r_loc = xmm_loc(r, base_stack_offset);
        // ucomisd's first operand must be an xmm register; the second
        // can be xmm or memory. Pick the cheapest combination.
        match (l_loc, r_loc) {
            (XmmLoc::Phys(lp), XmmLoc::Phys(rp)) => monoasm! { &mut self.jit,
                ucomisd xmm(lp), xmm(rp);
            },
            (XmmLoc::Phys(lp), XmmLoc::Spill(r_off)) => monoasm! { &mut self.jit,
                ucomisd xmm(lp), [rbp - (r_off)];
            },
            (XmmLoc::Spill(l_off), XmmLoc::Phys(rp)) => monoasm! { &mut self.jit,
                movq xmm0, [rbp - (l_off)];
                ucomisd xmm0, xmm(rp);
            },
            (XmmLoc::Spill(l_off), XmmLoc::Spill(r_off)) => monoasm! { &mut self.jit,
                movq xmm0, [rbp - (l_off)];
                ucomisd xmm0, [rbp - (r_off)];
            },
        }
    }

    pub(super) fn setflag_float(&mut self, kind: CmpKind) {
        // ucomisd sets ZF=1, PF=1, CF=1 for NaN (unordered).
        // - Gt (seta: CF=0 && ZF=0) and Ge (setae: CF=0) are correct (NaN → false).
        // - Eq (seteq: ZF=1) is wrong for NaN → need seteq AND setnp.
        // - Ne (setne: ZF=0) is wrong for NaN → need setne OR setp.
        // - Lt (setb: CF=1) is wrong for NaN → need setb AND setnp.
        // - Le (setbe: CF=1||ZF=1) is wrong for NaN → need setbe AND setnp.
        match kind {
            CmpKind::Eq | CmpKind::TEq => monoasm! { &mut self.jit,
                seteq rax;
                setnp rcx;
                andq rax, rcx;
            },
            CmpKind::Ne => monoasm! { &mut self.jit,
                // !(equal AND not_unordered) = not_equal OR unordered
                seteq rax;
                setnp rcx;
                andq rax, rcx;
                xorq rax, 1;
            },
            CmpKind::Ge => monoasm! { &mut self.jit, setae rax; },
            CmpKind::Gt => monoasm! { &mut self.jit, seta rax; },
            CmpKind::Le => monoasm! { &mut self.jit,
                setbe rax;
                setnp rcx;
                andq rax, rcx;
            },
            CmpKind::Lt => monoasm! { &mut self.jit,
                setb rax;
                setnp rcx;
                andq rax, rcx;
            },
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

    /// Float conditional branch with proper NaN (unordered) handling.
    ///
    /// ucomisd sets ZF=1, PF=1, CF=1 for NaN. We must check PF for Eq/Ne/Lt/Le
    /// because their default jcc instructions don't exclude unordered results.
    /// Gt (ja: CF=0&&ZF=0) and Ge (jae: CF=0) naturally exclude NaN.
    pub(super) fn condbr_float(&mut self, kind: CmpKind, branch_dest: DestLabel, brkind: BrKind) {
        match (kind, brkind) {
            // Gt and Ge are correct as-is (ja/jae exclude NaN)
            // NOTE: ucomisd sets CF/ZF/PF, so we must use unsigned conditions
            // (ja/jbe/jae/jb), NOT signed conditions (jg/jle/jge/jl).
            (CmpKind::Gt, _) => match brkind {
                BrKind::BrIf => monoasm! { &mut self.jit,
                    ja branch_dest;
                },
                BrKind::BrIfNot => monoasm! { &mut self.jit,
                    jbe branch_dest;
                },
            },
            (CmpKind::Ge, _) => match brkind {
                BrKind::BrIf => monoasm! { &mut self.jit,
                    jae branch_dest;
                },
                BrKind::BrIfNot => monoasm! { &mut self.jit,
                    jb branch_dest;
                },
            },

            // Eq BrIf: branch if equal AND not unordered
            (CmpKind::Eq | CmpKind::TEq, BrKind::BrIf) => {
                let skip = self.jit.label();
                monoasm! { &mut self.jit,
                    jp skip;
                    jeq branch_dest;
                skip:
                };
            }
            // Eq BrIfNot: branch if not-equal OR unordered
            (CmpKind::Eq | CmpKind::TEq, BrKind::BrIfNot) => {
                monoasm! { &mut self.jit,
                    jp branch_dest;
                    jne branch_dest;
                };
            }

            // Ne BrIf: branch if not-equal OR unordered
            (CmpKind::Ne, BrKind::BrIf) => {
                monoasm! { &mut self.jit,
                    jp branch_dest;
                    jne branch_dest;
                };
            }
            // Ne BrIfNot: branch if equal AND not unordered
            (CmpKind::Ne, BrKind::BrIfNot) => {
                let skip = self.jit.label();
                monoasm! { &mut self.jit,
                    jp skip;
                    jeq branch_dest;
                skip:
                };
            }

            // Lt BrIf: branch if below AND not unordered
            (CmpKind::Lt, BrKind::BrIf) => {
                let skip = self.jit.label();
                monoasm! { &mut self.jit,
                    jp skip;
                    jb branch_dest;
                skip:
                };
            }
            // Lt BrIfNot: branch if above-or-equal OR unordered
            (CmpKind::Lt, BrKind::BrIfNot) => {
                monoasm! { &mut self.jit,
                    jp branch_dest;
                    jae branch_dest;
                };
            }

            // Le BrIf: branch if below-or-equal AND not unordered
            (CmpKind::Le, BrKind::BrIf) => {
                let skip = self.jit.label();
                monoasm! { &mut self.jit,
                    jp skip;
                    jbe branch_dest;
                skip:
                };
            }
            // Le BrIfNot: branch if above OR unordered
            (CmpKind::Le, BrKind::BrIfNot) => {
                monoasm! { &mut self.jit,
                    jp branch_dest;
                    ja branch_dest;
                };
            }
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
        let lhs = Value::check_fixnum(lhs).unwrap();
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

    /// Loop JIT spill stress. The right-associated multiplication chain
    /// forces every left operand to remain live until the innermost
    /// `s * t` is evaluated and the result bubbles back out — so all
    /// 14 input variables (plus the running result) must occupy xmm
    /// slots simultaneously. With PHYS_XMM_POOL = 14, that overflow
    /// guarantees at least one `VirtFPReg(>=14)` allocation per
    /// iteration, exercising LoadSpill/StoreSpill and the spill-aware
    /// XmmBinOp memory-operand lowering. The hot `for` loop hits the
    /// loop-JIT threshold at iteration 16 (test mode).
    #[test]
    fn loop_jit_spill_stress() {
        run_test(
            r##"
            r = 0.0
            for i in 0..50
              f = i * 1.1
              g = i * 0.9
              h = i * 1.3
              j = i * 0.7
              k = i * 1.5
              l = i * 0.5
              m = i * 1.7
              n = i * 0.3
              o = i * 1.9
              p = i * 0.1
              q = i * 2.1
              s = i * 0.2
              t = i * 2.3
              u = i * 0.4
              r += f * (g * (h * (j * (k * (l * (m * (n * (o * (p * (q * (s * (t * u))))))))))))
            end
            r
            "##,
        );
    }

    /// Method JIT spill stress. Same nested-multiplication shape as
    /// `loop_jit_spill_stress`, but wrapped in a method body so the
    /// spill region lives inside the method's frame (not the loop
    /// frame). 25× call repetition inside `run_test` exceeds the
    /// method-JIT threshold (5 in test mode).
    #[test]
    fn method_jit_spill_stress() {
        run_test(
            r##"
            def calc(i)
              f = i * 1.1
              g = i * 0.9
              h = i * 1.3
              j = i * 0.7
              k = i * 1.5
              l = i * 0.5
              m = i * 1.7
              n = i * 0.3
              o = i * 1.9
              p = i * 0.1
              q = i * 2.1
              s = i * 0.2
              t = i * 2.3
              u = i * 0.4
              f * (g * (h * (j * (k * (l * (m * (n * (o * (p * (q * (s * (t * u))))))))))))
            end
            calc(7)
            "##,
        );
    }
}
