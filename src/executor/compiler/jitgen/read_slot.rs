use super::*;

impl Codegen {
    ///
    /// Read from slots *lhs* and *rhs* as f64, and store in xmm registers.
    ///
    /// ### destroy
    /// - rdi, rax
    ///
    pub(super) fn xmm_read_binary(
        &mut self,
        ctx: &mut BBContext,
        lhs: SlotId,
        rhs: SlotId,
        pc: BcPc,
    ) -> (Xmm, Xmm) {
        if lhs != rhs {
            (
                self.xmm_read_assume(ctx, lhs, pc.classid1(), pc),
                self.xmm_read_assume(ctx, rhs, pc.classid2(), pc),
            )
        } else {
            let lhs = self.xmm_read_assume(ctx, lhs, pc.classid1(), pc);
            (lhs, lhs)
        }
    }

    ///
    /// Read from a slot *reg* as f64, and store in xmm register.
    ///
    /// ### destroy
    /// - rdi, rax
    ///
    ///
    pub(super) fn xmm_read_assume_float(
        &mut self,
        ctx: &mut BBContext,
        reg: SlotId,
        pc: BcPc,
    ) -> Xmm {
        match ctx.slot_state[reg] {
            LinkMode::Both(freg) | LinkMode::Xmm(freg) => freg,
            LinkMode::Stack => {
                let freg = ctx.alloc_xmm();
                ctx.link_both(reg, freg);
                let side_exit = self.gen_side_deopt(pc, ctx);
                self.load_rdi(reg);
                self.unbox_float(freg.enc(), side_exit);
                freg
            }
            LinkMode::Fixnum(i) => {
                let freg = ctx.alloc_xmm();
                ctx.link_both(reg, freg);
                let f = self.jit.const_f64(i as f64);
                monoasm! {&mut self.jit,
                    movq xmm(freg.enc()), [rip + f];
                }
                freg
            }
        }
    }
}

impl Codegen {
    ///
    /// Read from a slot *reg* as f64, and store in xmm register.
    ///
    /// ### destroy
    /// - rdi, rax
    ///
    fn xmm_read_assume(
        &mut self,
        ctx: &mut BBContext,
        rhs: SlotId,
        class: ClassId,
        pc: BcPc,
    ) -> Xmm {
        match class {
            INTEGER_CLASS => self.xmm_read_assume_integer(ctx, rhs, pc),
            FLOAT_CLASS => self.xmm_read_assume_float(ctx, rhs, pc),
            _ => unreachable!(),
        }
    }

    ///
    /// Read from a slot *reg* as f64, and store in xmm register.
    ///
    /// ### destroy
    /// - rdi
    ///
    fn xmm_read_assume_integer(&mut self, ctx: &mut BBContext, reg: SlotId, pc: BcPc) -> Xmm {
        match ctx.slot_state[reg] {
            LinkMode::Both(freg) | LinkMode::Xmm(freg) => freg,
            LinkMode::Stack => {
                let freg = ctx.alloc_xmm();
                ctx.link_both(reg, freg);
                let side_exit = self.gen_side_deopt(pc, ctx);
                self.load_rdi(reg);
                self.integer_to_f64(freg.enc(), side_exit);
                freg
            }
            LinkMode::Fixnum(i) => {
                let freg = ctx.alloc_xmm();
                ctx.link_both(reg, freg);
                let f = self.jit.const_f64(i as f64);
                monoasm! {&mut self.jit,
                    movq xmm(freg.enc()), [rip + f];
                }
                freg
            }
        }
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
    fn integer_to_f64(&mut self, xmm: u64, side_exit: DestLabel) {
        monoasm!(&mut self.jit,
            testq rdi, 0b01;
            jz side_exit;
            sarq rdi, 1;
            cvtsi2sdq xmm(xmm), rdi;
        );
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
        gen.integer_to_f64(0, panic);
        monoasm!(&mut gen.jit,
            popq rbp;
            ret;
        );
        gen.jit.finalize();
        let int_to_f64_entry = gen.jit.get_label_address(assume_int_to_f64);

        let int_to_f64: fn(Value) -> f64 =
            unsafe { std::mem::transmute(int_to_f64_entry.as_ptr()) };
        assert_eq!(143.0, int_to_f64(Value::new_integer(143)));
        assert_eq!(14354813558.0, int_to_f64(Value::new_integer(14354813558)));
        assert_eq!(-143.0, int_to_f64(Value::new_integer(-143)));
    }
}
