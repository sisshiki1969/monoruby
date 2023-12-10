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

    ///
    /// Fetch *arg* and store in *rax*.
    ///
    pub(super) fn fetch_to_rax(&mut self, ctx: &mut BBContext, reg: SlotId) {
        self.fetch_to_reg(ctx, reg, GP::Rax)
    }

    ///
    /// Fetch *arg* and store in *rdi*.
    ///
    pub(super) fn fetch_to_rdi(&mut self, ctx: &mut BBContext, reg: SlotId) {
        self.fetch_to_reg(ctx, reg, GP::Rdi)
    }

    ///
    /// Fetch *arg* and store in *reg*.
    ///
    /// 0 : rax
    /// 6 : rsi
    /// 7 : rdi
    /// 15: r15
    ///
    fn fetch_to_reg(&mut self, ctx: &mut BBContext, reg: SlotId, dst: GP) {
        let mut ir = AsmIr::new();
        ctx.fetch_to_reg(&mut ir, reg, dst);
        self.gen_code(ir);
    }

    pub(crate) fn fetch_float_assume_float(
        &mut self,
        ctx: &mut BBContext,
        reg: SlotId,
        pc: BcPc,
    ) -> Xmm {
        let mut ir = AsmIr::new();
        let deopt = ir.new_deopt(pc, ctx.get_write_back());
        let x = ctx.fetch_float_assume_float(&mut ir, reg, deopt);
        self.gen_code(ir);
        x
    }

    ///
    /// Assume the Value is Integer, and convert to f64.
    ///
    /// side-exit if not Integer.
    ///
    /// ### in
    /// - R(*reg*): Value
    ///
    /// ### out
    /// - xmm(*xmm*)
    ///
    /// ### destroy
    /// - none
    pub(super) fn integer_val_to_f64(&mut self, reg: GP, xmm: u64, side_exit: DestLabel) {
        monoasm!(&mut self.jit,
            testq R(reg as _), 0b01;
            jz side_exit;
            sarq R(reg as _), 1;
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
                ir.xmm2both(freg, vec![reg]);
            }
            LinkMode::Literal(v) => {
                self[reg] = LinkMode::Stack;
                ir.lit2stack(v, reg);
            }
            LinkMode::R15 => {
                self.release(reg);
                ir.acc2stack(reg);
            }
            LinkMode::Both(_) | LinkMode::Stack => {}
        }
    }

    pub(crate) fn fetch_slots(&mut self, ir: &mut AsmIr, reg: &[SlotId]) {
        reg.iter().for_each(|r| self.fetch_slot(ir, *r));
    }

    pub(crate) fn fetch_binary(&mut self, ir: &mut AsmIr, mode: &OpMode) {
        match mode {
            OpMode::RR(lhs, rhs) => {
                self.fetch_slots(ir, &[*lhs, *rhs]);
            }
            OpMode::RI(r, _) | OpMode::IR(_, r) => {
                self.fetch_slots(ir, &[*r]);
            }
        }
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

    pub(super) fn fetch_to_reg(&mut self, ir: &mut AsmIr, reg: SlotId, dst: GP) {
        if reg >= self.sp {
            eprintln!("warning: {:?} >= {:?} in fetch_to_reg()", reg, self.sp);
            panic!();
        };
        match self[reg] {
            LinkMode::Xmm(x) => {
                if dst == GP::R15 {
                    self.writeback_acc(ir);
                }
                ir.xmm2both(x, vec![reg]);
                ir.reg_move(GP::Rax, dst);
                self[reg] = LinkMode::Both(x);
            }
            LinkMode::Literal(v) => {
                if dst == GP::R15 {
                    self.writeback_acc(ir);
                }
                ir.inst.push(AsmInst::LitToReg(v, dst));
            }
            LinkMode::Both(_) | LinkMode::Stack => {
                if dst == GP::R15 {
                    self.writeback_acc(ir);
                }
                ir.stack2reg(reg, dst);
            }
            LinkMode::R15 => {
                ir.reg_move(GP::R15, dst);
            }
        }
    }

    pub(super) fn writeback_acc(&mut self, ir: &mut AsmIr) {
        if let Some(slot) = self.clear_r15()
            && slot < self.sp
        {
            ir.acc2stack(slot);
        }
    }

    ///
    /// Read from a slot *reg* as f64, and store in xmm register.
    ///
    /// ### destroy
    /// - rdi
    ///
    fn fetch_float_assume_integer(&mut self, ir: &mut AsmIr, reg: SlotId, deopt: usize) -> Xmm {
        match self[reg] {
            LinkMode::Both(x) | LinkMode::Xmm(x) => x,
            LinkMode::Stack => {
                let x = self.alloc_xmm();
                ir.stack2reg(reg, GP::Rdi);
                ir.int2xmm(GP::Rdi, x, deopt);
                self.link_both(reg, x);
                x
            }
            LinkMode::R15 => {
                let x = self.alloc_xmm();
                ir.int2xmm(GP::R15, x, deopt);
                self.link_both(reg, x);
                x
            }
            LinkMode::Literal(v) => {
                let x = self.alloc_xmm();
                if let Some(f) = v.try_float() {
                    ir.f64toxmm(f, x);
                    self.link_xmm(reg, x);
                } else if let Some(i) = v.try_fixnum() {
                    ir.i64toboth(i, reg, x);
                    self.link_both(reg, x);
                } else {
                    unreachable!()
                }
                x
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
    pub(super) fn fetch_float_assume_float(
        &mut self,
        ir: &mut AsmIr,
        reg: SlotId,
        deopt: usize,
    ) -> Xmm {
        match self[reg] {
            LinkMode::Both(x) | LinkMode::Xmm(x) => x,
            LinkMode::Stack => {
                let x = self.alloc_xmm();
                ir.stack2reg(reg, GP::Rdi);
                ir.float2xmm(GP::Rdi, x, deopt);
                self.link_both(reg, x);
                x
            }
            LinkMode::R15 => {
                let x = self.alloc_xmm();
                ir.float2xmm(GP::R15, x, deopt);
                self.link_both(reg, x);
                x
            }
            LinkMode::Literal(v) => {
                let x = self.alloc_xmm();
                if let Some(f) = v.try_float() {
                    ir.f64toxmm(f, x);
                    self.link_xmm(reg, x);
                } else if let Some(i) = v.try_fixnum() {
                    ir.i64toboth(i, reg, x);
                    self.link_both(reg, x);
                } else {
                    unreachable!()
                }
                x
            }
        }
    }

    ///
    /// Read from a slot *reg* as f64, and store in xmm register.
    ///
    /// ### destroy
    /// - rdi, rax
    ///
    fn fetch_float_assume(
        &mut self,
        ir: &mut AsmIr,
        rhs: SlotId,
        class: ClassId,
        deopt: usize,
    ) -> Xmm {
        match class {
            INTEGER_CLASS => self.fetch_float_assume_integer(ir, rhs, deopt),
            FLOAT_CLASS => self.fetch_float_assume_float(ir, rhs, deopt),
            _ => unreachable!(),
        }
    }

    pub(super) fn fetch_float_binary(
        &mut self,
        ir: &mut AsmIr,
        lhs: SlotId,
        rhs: SlotId,
        pc: BcPc,
        deopt: usize,
    ) -> (Xmm, Xmm) {
        if lhs != rhs {
            (
                self.fetch_float_assume(ir, lhs, pc.classid1(), deopt),
                self.fetch_float_assume(ir, rhs, pc.classid2(), deopt),
            )
        } else {
            let lhs = self.fetch_float_assume(ir, lhs, pc.classid1(), deopt);
            (lhs, lhs)
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
        gen.integer_val_to_f64(GP::Rdi, 0, panic);
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
