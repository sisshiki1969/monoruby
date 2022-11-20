use super::*;

impl Codegen {
    pub(super) fn xmm_read_binary(
        &mut self,
        ctx: &mut BBContext,
        lhs: SlotId,
        rhs: SlotId,
        pc: BcPc,
    ) -> (u16, u16) {
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
    ) -> u16 {
        match ctx.stack_slot[reg] {
            LinkMode::XmmR(freg) | LinkMode::XmmRW(freg) => freg,
            _ => {
                let freg = ctx.alloc_xmm_read(reg);
                let side_exit = self.gen_side_writeback_deopt(pc, ctx);
                monoasm!(self.jit,
                    movq rdi, [rbp - (conv(reg))];
                );
                self.gen_val_to_f64_assume_float(freg as u64 + 2, side_exit);
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
    ) -> u16 {
        match class {
            INTEGER_CLASS => self.xmm_read_assume_integer(ctx, rhs, pc),
            FLOAT_CLASS => self.xmm_read_assume_float(ctx, rhs, pc),
            _ => unreachable!(),
        }
    }

    fn xmm_read_assume_integer(&mut self, ctx: &mut BBContext, reg: SlotId, pc: BcPc) -> u16 {
        match ctx.stack_slot[reg] {
            LinkMode::XmmR(freg) | LinkMode::XmmRW(freg) => freg,
            _ => {
                let freg = ctx.alloc_xmm_read(reg);
                let side_exit = self.gen_side_writeback_deopt(pc, ctx);
                monoasm!(self.jit,
                    movq rdi, [rbp - (conv(reg))];
                );
                self.gen_val_to_f64_assume_integer(freg as u64 + 2, side_exit);
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
        let mut gen = Codegen::new(false, Value::nil());

        let panic = gen.entry_panic;
        let from_f64_entry = gen.jit.get_label_address(gen.f64_to_val);
        let assume_float_to_f64 = gen.jit.label();
        monoasm!(&mut gen.jit,
        assume_float_to_f64:
            pushq rbp;
        );
        gen.gen_val_to_f64_assume_float(0, panic);
        monoasm!(&mut gen.jit,
            popq rbp;
            ret;
        );
        gen.jit.finalize();
        let to_f64_entry = gen.jit.get_label_address(assume_float_to_f64);

        let from_f64: fn(f64) -> Value = unsafe { std::mem::transmute(from_f64_entry.as_ptr()) };
        let to_f64: fn(Value) -> f64 = unsafe { std::mem::transmute(to_f64_entry.as_ptr()) };

        for n in [
            0.0,
            4.2,
            35354354354.2135365,
            -3535354345111.5696876565435432,
            f64::MAX,
            f64::MAX / 10.0,
            f64::MIN * 10.0,
            f64::NAN,
        ] {
            let v = from_f64(n);
            let (lhs, rhs) = (n, to_f64(v));
            if lhs.is_nan() {
                assert!(rhs.is_nan());
            } else {
                assert_eq!(n, to_f64(v));
            }
        }
    }

    #[test]
    fn float_test2() {
        let mut gen = Codegen::new(false, Value::nil());

        let panic = gen.entry_panic;
        let assume_float_to_f64 = gen.jit.label();
        monoasm!(&mut gen.jit,
        assume_float_to_f64:
            pushq rbp;
        );
        gen.gen_val_to_f64_assume_float(0, panic);
        monoasm!(&mut gen.jit,
            popq rbp;
            ret;
        );
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
        let float_to_f64_entry = gen.jit.get_label_address(assume_float_to_f64);
        let int_to_f64_entry = gen.jit.get_label_address(assume_int_to_f64);

        let float_to_f64: fn(Value) -> f64 =
            unsafe { std::mem::transmute(float_to_f64_entry.as_ptr()) };
        let int_to_f64: fn(Value) -> f64 =
            unsafe { std::mem::transmute(int_to_f64_entry.as_ptr()) };
        assert_eq!(3.574, float_to_f64(Value::new_float(3.574)));
        assert_eq!(0.0, float_to_f64(Value::new_float(0.0)));
        assert_eq!(143.0, int_to_f64(Value::new_integer(143)));
        assert_eq!(14354813558.0, int_to_f64(Value::new_integer(14354813558)));
        assert_eq!(-143.0, int_to_f64(Value::new_integer(-143)));
    }

    #[test]
    fn float_test3() {
        let mut gen = Codegen::new(false, Value::nil());

        let panic = gen.entry_panic;
        let to_f64 = gen.jit.label();
        monoasm!(&mut gen.jit,
        to_f64:
            pushq rbp;
        );
        gen.gen_val_to_f64(0, panic);
        monoasm!(&mut gen.jit,
            popq rbp;
            ret;
        );
        gen.jit.finalize();
        let to_f64_entry = gen.jit.get_label_address(to_f64);

        let to_f64: fn(Value) -> f64 = unsafe { std::mem::transmute(to_f64_entry.as_ptr()) };
        assert_eq!(3.574, to_f64(Value::new_float(3.574)));
        assert_eq!(0.0, to_f64(Value::new_float(0.0)));
        assert_eq!(f64::MAX, to_f64(Value::new_float(f64::MAX)));
        assert_eq!(f64::MIN, to_f64(Value::new_float(f64::MIN)));
        assert!(to_f64(Value::new_float(f64::NAN)).is_nan());
        assert!(to_f64(Value::new_float(f64::INFINITY)).is_infinite());
        assert_eq!(143.0, to_f64(Value::new_integer(143)));
        assert_eq!(14354813558.0, to_f64(Value::new_integer(14354813558)));
        assert_eq!(-143.0, to_f64(Value::new_integer(-143)));
    }
}
