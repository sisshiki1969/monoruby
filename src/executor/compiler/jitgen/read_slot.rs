use super::*;

impl Codegen {
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

    pub(super) fn xmm_read_assume_float(
        &mut self,
        ctx: &mut BBContext,
        reg: SlotId,
        pc: BcPc,
    ) -> Xmm {
        match ctx.stack_slot[reg] {
            LinkMode::XmmR(freg) | LinkMode::XmmRW(freg) => freg,
            _ => {
                let freg = ctx.alloc_xmm();
                ctx.link_r_xmm(reg, freg);
                let side_exit = self.gen_side_deopt(pc, ctx);
                self.load_rdi(reg);
                self.unbox_float(freg.enc(), side_exit);
                freg
            }
        }
    }
}

impl Codegen {
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

    fn xmm_read_assume_integer(&mut self, ctx: &mut BBContext, reg: SlotId, pc: BcPc) -> Xmm {
        match ctx.stack_slot[reg] {
            LinkMode::XmmR(freg) | LinkMode::XmmRW(freg) => freg,
            _ => {
                let freg = ctx.alloc_xmm();
                ctx.link_r_xmm(reg, freg);
                let side_exit = self.gen_side_deopt(pc, ctx);
                self.load_rdi(reg);
                self.gen_val_to_f64_assume_integer(freg.enc(), side_exit);
                freg
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
        gen.gen_val_to_f64_assume_integer(0, panic);
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
