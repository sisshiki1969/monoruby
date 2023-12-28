use super::*;

impl Codegen {
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

impl AsmIr {
    ///
    /// Fetch *reg* and store in a corresponding stack slot.
    ///
    /// ### destroy
    /// - rax, rcx
    fn fetch_slot(&mut self, bb: &mut BBContext, reg: SlotId) {
        if reg >= bb.sp {
            eprintln!("warning: {:?} >= {:?} in fetch_slot()", reg, bb.sp);
            panic!();
        };
        match bb[reg] {
            LinkMode::Xmm(freg) => {
                bb[reg] = LinkMode::Both(freg);
                self.xmm2both(freg, vec![reg]);
            }
            LinkMode::Literal(v) => {
                bb[reg] = LinkMode::Stack;
                self.lit2stack(v, reg);
            }
            LinkMode::R15 => {
                bb.link_stack(reg);
                self.acc2stack(reg);
            }
            LinkMode::Both(_) | LinkMode::Stack => {}
        }
    }

    pub(crate) fn fetch_slots(&mut self, bb: &mut BBContext, reg: &[SlotId]) {
        reg.iter().for_each(|r| self.fetch_slot(bb, *r));
    }

    ///
    /// Fetch from *args* to *args* + *len* - 1 and store in corresponding stack slots.
    ///
    pub(crate) fn fetch_range(&mut self, bb: &mut BBContext, args: SlotId, len: u16) {
        for reg in args.0..args.0 + len {
            self.fetch_slot(bb, SlotId::new(reg))
        }
    }

    pub(crate) fn fetch_callargs(&mut self, bb: &mut BBContext, callsite: &CallSiteInfo) {
        let CallSiteInfo {
            recv, args, len, ..
        } = callsite;
        self.fetch_slot(bb, *recv);
        self.fetch_range(bb, *args, *len as u16);
    }

    pub(crate) fn fetch_to_reg(&mut self, bb: &mut BBContext, reg: SlotId, dst: GP) {
        if reg >= bb.sp {
            eprintln!("warning: {:?} >= {:?} in fetch_to_reg()", reg, bb.sp);
            panic!();
        };
        match bb[reg] {
            LinkMode::Xmm(x) => {
                if dst == GP::R15 {
                    self.writeback_acc(bb);
                }
                self.xmm2both(x, vec![reg]);
                self.reg_move(GP::Rax, dst);
                bb[reg] = LinkMode::Both(x);
            }
            LinkMode::Literal(v) => {
                if dst == GP::R15 {
                    self.writeback_acc(bb);
                }
                self.inst.push(AsmInst::LitToReg(v, dst));
            }
            LinkMode::Both(_) | LinkMode::Stack => {
                if dst == GP::R15 {
                    self.writeback_acc(bb);
                }
                self.stack2reg(reg, dst);
            }
            LinkMode::R15 => {
                self.reg_move(GP::R15, dst);
            }
        }
    }

    pub(in crate::compiler::jitgen) fn writeback_acc(&mut self, bb: &mut BBContext) {
        if let Some(slot) = bb.clear_r15()
            && slot < bb.sp
        {
            self.acc2stack(slot);
        }
    }

    ///
    /// Read from a slot *reg* as f64, and store in xmm register.
    ///
    /// ### destroy
    /// - rdi
    ///
    fn fetch_float_assume_integer(
        &mut self,
        bb: &mut BBContext,
        reg: SlotId,
        deopt: AsmDeopt,
    ) -> Xmm {
        match bb[reg] {
            LinkMode::Both(x) | LinkMode::Xmm(x) => x,
            LinkMode::Stack => {
                let x = bb.alloc_xmm();
                self.stack2reg(reg, GP::Rdi);
                self.int2xmm(GP::Rdi, x, deopt);
                bb.link_both(reg, x);
                x
            }
            LinkMode::R15 => {
                let x = bb.alloc_xmm();
                self.int2xmm(GP::R15, x, deopt);
                bb.link_both(reg, x);
                x
            }
            LinkMode::Literal(v) => {
                let x = bb.alloc_xmm();
                if let Some(f) = v.try_float() {
                    self.f64toxmm(f, x);
                    bb.link_xmm(reg, x);
                } else if let Some(i) = v.try_fixnum() {
                    self.i64toboth(i, reg, x);
                    bb.link_both(reg, x);
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
    pub(crate) fn fetch_float_assume_float(
        &mut self,
        bb: &mut BBContext,
        reg: SlotId,
        deopt: AsmDeopt,
    ) -> Xmm {
        match bb[reg] {
            LinkMode::Both(x) | LinkMode::Xmm(x) => x,
            LinkMode::Stack => {
                let x = bb.alloc_xmm();
                self.stack2reg(reg, GP::Rdi);
                self.float2xmm(GP::Rdi, x, deopt);
                bb.link_both(reg, x);
                x
            }
            LinkMode::R15 => {
                let x = bb.alloc_xmm();
                self.float2xmm(GP::R15, x, deopt);
                bb.link_both(reg, x);
                x
            }
            LinkMode::Literal(v) => {
                let x = bb.alloc_xmm();
                if let Some(f) = v.try_float() {
                    self.f64toxmm(f, x);
                    bb.link_xmm(reg, x);
                } else if let Some(i) = v.try_fixnum() {
                    self.i64toboth(i, reg, x);
                    bb.link_both(reg, x);
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
        bb: &mut BBContext,
        rhs: SlotId,
        class: Option<ClassId>,
        deopt: AsmDeopt,
    ) -> Xmm {
        match class {
            Some(INTEGER_CLASS) => self.fetch_float_assume_integer(bb, rhs, deopt),
            Some(FLOAT_CLASS) => self.fetch_float_assume_float(bb, rhs, deopt),
            _ => unreachable!(),
        }
    }

    pub(super) fn fetch_float_binary(
        &mut self,
        bb: &mut BBContext,
        lhs: SlotId,
        rhs: SlotId,
        pc: BcPc,
        deopt: AsmDeopt,
    ) -> (Xmm, Xmm) {
        if lhs != rhs {
            (
                self.fetch_float_assume(bb, lhs, pc.classid1(), deopt),
                self.fetch_float_assume(bb, rhs, pc.classid2(), deopt),
            )
        } else {
            let lhs = self.fetch_float_assume(bb, lhs, pc.classid1(), deopt);
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
