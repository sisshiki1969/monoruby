use super::*;

impl Codegen {
    ///
    /// Fetch *reg*s and store in corresponding stack slots.
    ///
    pub(crate) fn fetch_slots(&mut self, ctx: &mut BBContext, reg: &[SlotId]) {
        let mut ir = AsmIr::new();
        ctx.fetch_slots(&mut ir, reg);
        self.gen_code(ir);
    }

    pub(super) fn fetch_binary(&mut self, ctx: &mut BBContext, mode: &OpMode) {
        let mut ir = AsmIr::new();
        match mode {
            OpMode::RR(lhs, rhs) => {
                ctx.fetch_slots(&mut ir, &[*lhs, *rhs]);
            }
            OpMode::RI(r, _) | OpMode::IR(_, r) => {
                ctx.fetch_slots(&mut ir, &[*r]);
            }
        }
        self.gen_code(ir);
    }

    ///
    /// Fetch *arg* and store in *rax*.
    ///
    pub(super) fn fetch_to_rax(&mut self, ctx: &mut BBContext, reg: SlotId) {
        self.fetch_to_reg(ctx, reg, 0)
    }

    ///
    /// Fetch *arg* and store in *rdi*.
    ///
    pub(super) fn fetch_to_rdi(&mut self, ctx: &mut BBContext, reg: SlotId) {
        self.fetch_to_reg(ctx, reg, 7)
    }

    ///
    /// Fetch *arg* and store in *rsi*.
    ///
    pub(super) fn fetch_to_rsi(&mut self, ctx: &mut BBContext, reg: SlotId) {
        self.fetch_to_reg(ctx, reg, 6)
    }

    ///
    /// Fetch *arg* and store in *r15*.
    ///
    pub(super) fn fetch_to_r15(&mut self, ctx: &mut BBContext, reg: SlotId) {
        self.fetch_to_reg(ctx, reg, 15)
    }

    ///
    /// Fetch *arg* and store in *reg*.
    ///
    /// 0 : rax
    /// 6 : rsi
    /// 7 : rdi
    /// 15: r15
    ///
    fn fetch_to_reg(&mut self, ctx: &mut BBContext, reg: SlotId, dst: u64) {
        if reg >= ctx.sp {
            eprintln!("warning: {:?} >= {:?} in fetch_to_rax()", reg, ctx.sp);
            panic!();
        };
        match ctx[reg] {
            LinkMode::Xmm(x) => {
                self.writeback_acc(ctx);
                let f64_to_val = self.f64_to_val;
                monoasm! { &mut self.jit,
                        movq xmm0, xmm(x.enc());
                        call f64_to_val;
                }
                if dst != 0 {
                    monoasm! { &mut self.jit,
                        movq R(dst), rax;
                    }
                }
                monoasm! { &mut self.jit,
                    movq [r14 - (conv(reg))], rax;
                }
                ctx[reg] = LinkMode::Both(x);
            }
            LinkMode::Literal(v) => {
                self.writeback_acc(ctx);
                monoasm!(&mut self.jit,
                    movq R(dst), (v.id());
                );
            }
            LinkMode::Both(_) | LinkMode::Stack => {
                self.writeback_acc(ctx);
                monoasm!( &mut self.jit,
                    movq R(dst), [r14 - (conv(reg))];
                );
            }
            LinkMode::R15 => {
                if dst != 15 {
                    monoasm!(&mut self.jit,
                        movq R(dst), r15;
                    );
                }
            }
        }
    }

    ///
    /// Fetch *lhs* and *rhs* as f64, and store in xmm registers.
    ///
    /// ### destroy
    /// - rdi, rax
    ///
    pub(super) fn fetch_float_binary(
        &mut self,
        ctx: &mut BBContext,
        lhs: SlotId,
        rhs: SlotId,
        pc: BcPc,
    ) -> (Xmm, Xmm) {
        if lhs != rhs {
            (
                self.fetch_float_assume(ctx, lhs, pc.classid1(), pc),
                self.fetch_float_assume(ctx, rhs, pc.classid2(), pc),
            )
        } else {
            let lhs = self.fetch_float_assume(ctx, lhs, pc.classid1(), pc);
            (lhs, lhs)
        }
    }

    pub(super) fn fetch_float_assume_float(
        &mut self,
        ctx: &mut BBContext,
        reg: SlotId,
        pc: BcPc,
    ) -> Xmm {
        let mut ir = AsmIr::new();
        let x = ctx.fetch_float_assume_float(&mut ir, reg, pc);
        self.gen_code(ir);
        x
    }

    pub(crate) fn fetch_float_assume_float_enc(
        &mut self,
        ctx: &mut BBContext,
        reg: SlotId,
        pc: BcPc,
    ) -> u64 {
        let mut ir = AsmIr::new();
        let enc = ctx.fetch_float_assume_float(&mut ir, reg, pc).enc();
        self.gen_code(ir);
        enc
    }

    ///
    /// Read from a slot *reg* as f64, and store in xmm register.
    ///
    /// ### destroy
    /// - rdi, rax
    ///
    fn fetch_float_assume(
        &mut self,
        ctx: &mut BBContext,
        rhs: SlotId,
        class: ClassId,
        pc: BcPc,
    ) -> Xmm {
        let mut ir = AsmIr::new();
        let x = match class {
            INTEGER_CLASS => ctx.fetch_float_assume_integer(&mut ir, rhs, pc),
            FLOAT_CLASS => ctx.fetch_float_assume_float(&mut ir, rhs, pc),
            _ => unreachable!(),
        };
        self.gen_code(ir);
        x
    }

    ///
    /// Assume the Value is Integer, and convert to f64.
    ///
    /// side-exit if not Integer.
    ///
    /// ### in
    /// - rdi: Value
    ///
    /// ### out
    /// - xmm(*xmm*)
    ///
    /// ### destroy
    /// - none
    pub(super) fn integer_val_to_f64(&mut self, xmm: u64, side_exit: DestLabel) {
        monoasm!(&mut self.jit,
            testq rdi, 0b01;
            jz side_exit;
            sarq rdi, 1;
            cvtsi2sdq xmm(xmm), rdi;
        );
    }
}

impl BBContext {
    ///
    /// Fetch *reg* and store in a corresponding stack slot.
    ///
    fn fetch_slot(&mut self, ir: &mut AsmIr, reg: SlotId) {
        if reg >= self.sp {
            eprintln!("warning: {:?} >= {:?} in fetch_slot()", reg, self.sp);
            panic!();
        };
        match self[reg] {
            LinkMode::Xmm(freg) => {
                self[reg] = LinkMode::Both(freg);
                ir.push(AsmInst::XmmToBoth(freg, vec![reg]));
            }
            LinkMode::Literal(v) => {
                self[reg] = LinkMode::Stack;
                ir.push(AsmInst::LitToStack(v, reg));
            }
            LinkMode::R15 => {
                self.release(reg);
                ir.push(AsmInst::AccToStack(reg));
            }
            LinkMode::Both(_) | LinkMode::Stack => {}
        }
    }

    pub(crate) fn fetch_slots(&mut self, ir: &mut AsmIr, reg: &[SlotId]) {
        reg.iter().for_each(|r| self.fetch_slot(ir, *r));
    }

    ///
    /// Fetch from *args* to *args* + *len* - 1 and store in corresponding stack slots.
    ///
    pub(crate) fn fetch_range(&mut self, ir: &mut AsmIr, args: SlotId, len: u16) {
        for reg in args.0..args.0 + len {
            self.fetch_slot(ir, SlotId::new(reg))
        }
    }

    pub(crate) fn fetch_callargs(&mut self, ir: &mut AsmIr, callsite: &CallSiteInfo) {
        let CallSiteInfo {
            recv, args, len, ..
        } = callsite;
        self.fetch_slot(ir, *recv);
        self.fetch_range(ir, *args, *len as u16);
    }

    ///
    /// Read from a slot *reg* as f64, and store in xmm register.
    ///
    /// ### destroy
    /// - rdi
    ///
    fn fetch_float_assume_integer(&mut self, ir: &mut AsmIr, reg: SlotId, pc: BcPc) -> Xmm {
        match self[reg] {
            LinkMode::Both(x) | LinkMode::Xmm(x) => x,
            LinkMode::Stack => {
                let x = self.link_new_both(reg);
                let label = ir.new_deopt(pc, self.get_write_back());
                ir.push(AsmInst::IntToXmm(Some(reg), x, label));
                x
            }
            LinkMode::R15 => {
                let x = self.link_new_both(reg);
                let label = ir.new_deopt(pc, self.get_write_back());
                ir.push(AsmInst::IntToXmm(None, x, label));
                x
            }
            LinkMode::Literal(v) => {
                if let Some(f) = v.try_float() {
                    let x = self.link_new_xmm(reg);
                    ir.push(AsmInst::F64ToXmm(f, x));
                    x
                } else if let Some(i) = v.try_fixnum() {
                    let x = self.link_new_both(reg);
                    ir.push(AsmInst::I64ToBoth(i, reg, x));
                    x
                } else {
                    unreachable!()
                }
            }
        }
    }

    ///
    /// Fetch *reg* as f64, and store in xmm register.
    ///
    /// ### destroy
    /// - rdi, rax
    ///
    ///
    fn fetch_float_assume_float(&mut self, ir: &mut AsmIr, reg: SlotId, pc: BcPc) -> Xmm {
        match self[reg] {
            LinkMode::Both(x) | LinkMode::Xmm(x) => x,
            LinkMode::Stack => {
                let x = self.link_new_both(reg);
                let label = ir.new_deopt(pc, self.get_write_back());
                ir.push(AsmInst::FloatToXmm(Some(reg), x, label));
                x
            }
            LinkMode::R15 => {
                let x = self.link_new_both(reg);
                let label = ir.new_deopt(pc, self.get_write_back());
                ir.push(AsmInst::FloatToXmm(None, x, label));
                x
            }
            LinkMode::Literal(v) => {
                if let Some(f) = v.try_float() {
                    let x = self.link_new_xmm(reg);
                    ir.push(AsmInst::F64ToXmm(f, x));
                    x
                } else if let Some(i) = v.try_fixnum() {
                    let x = self.link_new_both(reg);
                    ir.push(AsmInst::I64ToBoth(i, reg, x));
                    x
                } else {
                    unreachable!()
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn float_test() {
        let gen = Codegen::new(false, Value::nil());

        let from_f64_entry = gen.jit.get_label_address(gen.f64_to_val);
        let from_f64: fn(f64) -> Value = unsafe { std::mem::transmute(from_f64_entry.as_ptr()) };

        for lhs in [
            0.0,
            4.2,
            35354354354.2135365,
            -3535354345111.5696876565435432,
            f64::MAX,
            f64::MAX / 10.0,
            f64::MIN * 10.0,
            f64::NAN,
        ] {
            let v = from_f64(lhs);
            let rhs = match v.unpack() {
                RV::Float(f) => f,
                _ => panic!(),
            };
            if lhs.is_nan() {
                assert!(rhs.is_nan());
            } else {
                assert_eq!(lhs, rhs);
            }
        }
    }

    #[test]
    fn float_test2() {
        let mut gen = Codegen::new(false, Value::nil());

        let panic = gen.entry_panic;
        let assume_int_to_f64 = gen.jit.label();
        monoasm!(&mut gen.jit,
        assume_int_to_f64:
            pushq rbp;
        );
        gen.integer_val_to_f64(0, panic);
        monoasm!(&mut gen.jit,
            popq rbp;
            ret;
        );
        gen.jit.finalize();
        let int_to_f64_entry = gen.jit.get_label_address(assume_int_to_f64);

        let int_to_f64: fn(Value) -> f64 =
            unsafe { std::mem::transmute(int_to_f64_entry.as_ptr()) };
        assert_eq!(143.0, int_to_f64(Value::integer(143)));
        assert_eq!(14354813558.0, int_to_f64(Value::integer(14354813558)));
        assert_eq!(-143.0, int_to_f64(Value::integer(-143)));
    }
}
